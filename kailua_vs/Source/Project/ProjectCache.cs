using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace Kailua
{
    internal class ProjectObserver : IDisposable, IVsSolutionEvents
    {
        private static EnvDTE.Project getProjectFromHierarchy(IVsHierarchy hierarchy)
        {
            object projObj;
            Trace.Assert(hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out projObj) == VSConstants.S_OK);
            EnvDTE.Project project = projObj as EnvDTE.Project;
            Trace.Assert(project != null);
            return project;
        }

        internal class HierarchyObserver : IDisposable, IVsHierarchyEvents
        {
            public ProjectObserver observer;
            public WeakReference<IVsHierarchy> hierarchy;
            public EnvDTE.Project project;
            public uint eventCookie;

            public Dictionary<uint, string> fileItemIds;
            public Dictionary<string, uint> fileNames;

            public HierarchyObserver(ProjectObserver observer, IVsHierarchy hierarchy)
            {
                this.observer = observer;
                this.hierarchy = new WeakReference<IVsHierarchy>(hierarchy);
                this.project = getProjectFromHierarchy(hierarchy);

                this.fileItemIds = new Dictionary<uint, string>();
                this.fileNames = new Dictionary<string, uint>();

                // trigger initial events for already existing items
                foreach (var itemId in this.enumerateHierarchyFiles(hierarchy, VSConstants.VSITEMID_ROOT))
                {
                    this.onFileAdded(itemId);
                }

                Trace.Assert(hierarchy.AdviseHierarchyEvents(this, out this.eventCookie) == VSConstants.S_OK);
            }

            private IEnumerable<uint> enumerateHierarchyFiles(IVsHierarchy hierarchy, uint itemId)
            {
                yield return itemId;

                // note that this should traverse all children, not only visible ones
                // (it seems that VS puts the first file added to the "other files" pseudo-project as invisible, somehow)
                object child = null;
                int ret = hierarchy.GetProperty(itemId, (int)__VSHPROPID.VSHPROPID_FirstChild, out child);
                while (ret == VSConstants.S_OK && child != null)
                {
                    if (child is int && (uint)(int)child == VSConstants.VSITEMID_NIL)
                    {
                        break; // no more sibling nodes
                    }

                    uint childItemId = Convert.ToUInt32(child);
                    foreach (var subItemId in this.enumerateHierarchyFiles(hierarchy, childItemId))
                    {
                        yield return subItemId;
                    }

                    child = null;
                    ret = hierarchy.GetProperty(childItemId, (int)__VSHPROPID.VSHPROPID_NextSibling, out child);
                }
            }

            private bool getFileNameFromItemId(uint itemId, out string fileName)
            {
                IVsHierarchy hierarchy;
                if (this.hierarchy.TryGetTarget(out hierarchy))
                {
                    // check if the item refers to a regular file
                    Guid typeGuid;
                    int ret = hierarchy.GetGuidProperty(itemId, (int)__VSHPROPID.VSHPROPID_TypeGuid, out typeGuid);
                    if (ret == VSConstants.S_OK && typeGuid == VSConstants.ItemTypeGuid.PhysicalFile_guid)
                    {
                        return hierarchy.GetCanonicalName(itemId, out fileName) == VSConstants.S_OK;
                    }
                }

                fileName = null;
                return false;
            }

            private void onFileAdded(uint itemId)
            {
                // only keep the regular files
                string fileName;
                if (!this.getFileNameFromItemId(itemId, out fileName))
                {
                    return;
                }

                this.fileItemIds.Add(itemId, fileName);
                this.fileNames.Add(fileName, itemId);

                if (this.observer.FileAdded != null)
                {
                    this.observer.FileAdded(this.project, fileName);
                }
            }

            private void onFileRemoved(uint itemId)
            {
                string fileName;
                if (this.fileItemIds.TryGetValue(itemId, out fileName))
                {
                    this.fileItemIds.Remove(itemId);
                    this.fileNames.Remove(fileName);
                }

                if (this.observer.FileRemoved != null)
                {
                    this.observer.FileRemoved(this.project, fileName);
                }
            }

            int IVsHierarchyEvents.OnItemAdded(uint itemidParent, uint itemidSiblingPrev, uint itemidAdded)
            {
                this.onFileAdded(itemidAdded);
                return VSConstants.S_OK;
            }

            int IVsHierarchyEvents.OnItemDeleted(uint itemid)
            {
                this.onFileRemoved(itemid);
                return VSConstants.S_OK;
            }

            #region placeholder IVsHierarchyEvents methods

            int IVsHierarchyEvents.OnInvalidateIcon(IntPtr hicon)
            {
                return VSConstants.S_OK;
            }

            int IVsHierarchyEvents.OnInvalidateItems(uint itemidParent)
            {
                return VSConstants.S_OK;
            }

            int IVsHierarchyEvents.OnItemsAppended(uint itemidParent)
            {
                return VSConstants.S_OK;
            }

            int IVsHierarchyEvents.OnPropertyChanged(uint itemid, int propid, uint flags)
            {
                return VSConstants.S_OK;
            }

            #endregion

            public void Dispose()
            {
                IVsHierarchy hierarchy;
                if (this.hierarchy.TryGetTarget(out hierarchy))
                {
                    Trace.Assert(hierarchy.UnadviseHierarchyEvents(this.eventCookie) == VSConstants.S_OK);
                }
            }
        }

        private bool initialized = false;
        internal IVsSolution solution;
        private uint solutionEventcookie;
        internal ConditionalWeakTable<IVsHierarchy, HierarchyObserver> hierarchyObservers;

        public ProjectObserver()
        {
            this.solution = Package.GetGlobalService(typeof(SVsSolution)) as IVsSolution;
            Trace.Assert(this.solution != null);

            this.hierarchyObservers = new ConditionalWeakTable<IVsHierarchy, HierarchyObserver>();

            Trace.Assert(this.solution.AdviseSolutionEvents(this, out this.solutionEventcookie) == VSConstants.S_OK);
        }

        private IEnumerable<IVsHierarchy> ProjectHierarchies
        {
            get
            {
                IEnumHierarchies projectEnum = null;
                Guid guid = Guid.Empty;
                this.solution.GetProjectEnum((uint)__VSENUMPROJFLAGS.EPF_LOADEDINSOLUTION, ref guid, out projectEnum);

                var hierarchy = new IVsHierarchy[1];
                uint fetched = 0;
                projectEnum.Reset();
                while (projectEnum.Next(1, hierarchy, out fetched) == VSConstants.S_OK && fetched == 1)
                {
                    yield return hierarchy[0];
                }
            }
        }

        public void Init()
        {
            if (this.initialized)
            {
                return;
            }

            // populate projects
            foreach (var hierarchy in this.ProjectHierarchies)
            {
                this.hierarchyObservers.Add(hierarchy, new HierarchyObserver(this, hierarchy));
            }

            this.initialized = true;
        }

        public delegate void FileAddedEventHandler(EnvDTE.Project project, string fileName);
        public delegate void FileRemovedEventHandler(EnvDTE.Project project, string fileName);

        // these are called only after the initial population finishes
        public event FileAddedEventHandler FileAdded;
        public event FileRemovedEventHandler FileRemoved;

        int IVsSolutionEvents.OnAfterOpenProject(IVsHierarchy pHierarchy, int fAdded)
        {
            var hierarchyObserver = new HierarchyObserver(this, pHierarchy);
            this.hierarchyObservers.Add(pHierarchy, hierarchyObserver);
            return VSConstants.S_OK;
        }

        int IVsSolutionEvents.OnBeforeCloseProject(IVsHierarchy pHierarchy, int fRemoved)
        {
            // the project may not have been loaded if no document in that project has ever been opened
            this.hierarchyObservers.Remove(pHierarchy);
            return VSConstants.S_OK;
        }

        #region placeholder IVsSolutionEvents methods

        int IVsSolutionEvents.OnAfterCloseSolution(object pUnkReserved)
        {
            return VSConstants.S_OK;
        }

        int IVsSolutionEvents.OnAfterLoadProject(IVsHierarchy pStubHierarchy, IVsHierarchy pRealHierarchy)
        {
            return VSConstants.S_OK;
        }

        int IVsSolutionEvents.OnAfterOpenSolution(object pUnkReserved, int fNewSolution)
        {
            return VSConstants.S_OK;
        }

        int IVsSolutionEvents.OnBeforeCloseSolution(object pUnkReserved)
        {
            return VSConstants.S_OK;
        }

        int IVsSolutionEvents.OnBeforeUnloadProject(IVsHierarchy pRealHierarchy, IVsHierarchy pStubHierarchy)
        {
            return VSConstants.S_OK;
        }

        int IVsSolutionEvents.OnQueryCloseProject(IVsHierarchy pHierarchy, int fRemoving, ref int pfCancel)
        {
            return VSConstants.S_OK;
        }

        int IVsSolutionEvents.OnQueryCloseSolution(object pUnkReserved, ref int pfCancel)
        {
            return VSConstants.S_OK;
        }

        int IVsSolutionEvents.OnQueryUnloadProject(IVsHierarchy pRealHierarchy, ref int pfCancel)
        {
            return VSConstants.S_OK;
        }

        #endregion

        public IEnumerable<EnvDTE.Project> GetProjects(String fileName)
        {
            this.Init();

            // this is O(n) over the projects, but we ain't gonna have tons of projects anyway
            var projects = new List<Project>();
            foreach (var hierarchy in this.ProjectHierarchies)
            {
                HierarchyObserver hierarchyObserver;
                if (this.hierarchyObservers.TryGetValue(hierarchy, out hierarchyObserver))
                {
                    if (hierarchyObserver.fileNames.ContainsKey(fileName))
                    {
                        yield return hierarchyObserver.project;
                    }
                }
            }
            yield break;
        }

        public void Dispose()
        {
            Trace.Assert(this.solution.UnadviseSolutionEvents(this.solutionEventcookie) == VSConstants.S_OK);
        }
    }

    public class ProjectCache
    {
        private static readonly ConditionalWeakTable<EnvDTE.Project, Project> table = new ConditionalWeakTable<EnvDTE.Project, Project>();
        private static readonly ProjectObserver observer;

        static ProjectCache()
        {
            observer = new ProjectObserver();

            // translate observer events to project events
            observer.FileAdded += delegate(EnvDTE.Project project, string fileName)
            {
                Get(project).OnFileAdded(fileName);
            };
            observer.FileRemoved += delegate(EnvDTE.Project project, string fileName)
            {
                Get(project).OnFileRemoved(fileName);
            };

            observer.Init();
        }

        public static IEnumerable<Project> GetProjects(ITextBuffer buffer)
        {
            ITextDocument document;
            if (!buffer.Properties.TryGetProperty(typeof(ITextDocument), out document))
            {
                yield break;
            }

            foreach (var project in observer.GetProjects(document.FilePath))
            {
                yield return Get(project);
            }
        }

        public static Project GetAnyProject(ITextBuffer buffer, out string path)
        {
            ITextDocument document;
            if (!buffer.Properties.TryGetProperty(typeof(ITextDocument), out document))
            {
                path = null;
                return null;
            }

            path = document.FilePath;
            foreach (var project in observer.GetProjects(path))
            {
                // XXX do not try multiple times when the file is shared by multiple projects
                // ideally we should have one ProjectFile per unique file path, but that's annoying and rare anyway
                // while inefficient this should be correct at least
                return Get(project);
            }
            return null;
        }

        public static Project Get(EnvDTE.Project project)
        {
            if (project == null)
            {
                throw new ArgumentNullException("project");
            }

            // this is thread-safe, though it may create Project multiple times
            return table.GetValue(project, p => new Project(p));
        }
    }
}
