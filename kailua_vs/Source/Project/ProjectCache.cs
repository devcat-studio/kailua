using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace Kailua
{
    // observes all documents with edit lock and corresponding projects in the current solution
    internal class ProjectObserver : IDisposable, IVsSolutionEvents, IVsRunningDocTableEvents
    {
        internal IVsRunningDocumentTable rdt;
        internal IVsSolution solution;
        private uint rdtEventCookie;
        private uint solutionEventcookie;
        internal HashSet<string> openFileNames;
        internal ConditionalWeakTable<IVsHierarchy, HierarchyObserver> hierarchyObservers;

        internal class HierarchyObserver : IDisposable, IVsHierarchyEvents
        {
            public ProjectObserver observer;
            public WeakReference<IVsHierarchy> hierarchy;
            public uint eventCookie;
            public Dictionary<string, uint> openFileItemIds;

            public HierarchyObserver(ProjectObserver observer, IVsHierarchy hierarchy)
            {
                this.observer = observer;
                this.hierarchy = new WeakReference<IVsHierarchy>(hierarchy);
                this.openFileItemIds = new Dictionary<string, uint>();

                Debug.Assert(hierarchy.AdviseHierarchyEvents(this, out this.eventCookie) == VSConstants.S_OK);
            }

            private bool getOpenFileNameFromItemId(uint itemId, out string fileName)
            {
                IVsHierarchy hierarchy;
                if (!this.hierarchy.TryGetTarget(out hierarchy))
                {
                    fileName = null;
                    return false;
                }

                if (hierarchy.GetCanonicalName(itemId, out fileName) == VSConstants.S_OK)
                {
                    return this.observer.openFileNames.Contains(fileName);
                }
                else
                {
                    return false;
                }
            }

            public int OnItemAdded(uint itemidParent, uint itemidSiblingPrev, uint itemidAdded)
            {
                string fileName;
                if (this.getOpenFileNameFromItemId(itemidAdded, out fileName))
                {
                    this.openFileItemIds.Add(fileName, itemidAdded);
                }
                return VSConstants.S_OK;
            }

            public int OnItemDeleted(uint itemid)
            {
                string fileName;
                if (this.getOpenFileNameFromItemId(itemid, out fileName))
                {
                    this.openFileItemIds.Remove(fileName);
                }
                return VSConstants.S_OK;
            }

            #region placeholder IVsHierarchyEvents methods

            public int OnInvalidateIcon(IntPtr hicon)
            {
                return VSConstants.S_OK;
            }

            public int OnInvalidateItems(uint itemidParent)
            {
                return VSConstants.S_OK;
            }

            public int OnItemsAppended(uint itemidParent)
            {
                return VSConstants.S_OK;
            }

            public int OnPropertyChanged(uint itemid, int propid, uint flags)
            {
                return VSConstants.S_OK;
            }

            #endregion

            public void Dispose()
            {
                IVsHierarchy hierarchy;
                if (this.hierarchy.TryGetTarget(out hierarchy))
                {
                    Debug.Assert(hierarchy.UnadviseHierarchyEvents(this.eventCookie) == VSConstants.S_OK);
                }
            }
        }

        public ProjectObserver()
        {
            this.rdt = Package.GetGlobalService(typeof(IVsRunningDocumentTable)) as IVsRunningDocumentTable;
            Debug.Assert(this.rdt != null);

            this.solution = Package.GetGlobalService(typeof(SVsSolution)) as IVsSolution;
            Debug.Assert(this.solution != null);

            this.openFileNames = new HashSet<string>();
            this.hierarchyObservers = new ConditionalWeakTable<IVsHierarchy, HierarchyObserver>();

            Debug.Assert(this.rdt.AdviseRunningDocTableEvents(this, out this.rdtEventCookie) == VSConstants.S_OK);
            Debug.Assert(this.solution.AdviseSolutionEvents(this, out this.solutionEventcookie) == VSConstants.S_OK);

            this.populateProjects();
        }

        public IEnumerable<IVsHierarchy> ProjectHierarchies
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

        // returns false if this document should be excluded from the observer's view
        private bool getDocumentFromCookie(uint docCookie, out string fileName, out IVsHierarchy hierarchy, out uint itemId, out bool hasEditLock)
        {
            uint flags;
            uint readLockCount;
            uint editLockCount;
            IntPtr docData;

            Debug.Assert(this.rdt.GetDocumentInfo(
                docCookie, out flags, out readLockCount, out editLockCount,
                out fileName, out hierarchy, out itemId, out docData) == VSConstants.S_OK);
            if (docData != IntPtr.Zero)
            {
                Marshal.Release(docData);
            }

            // exclude virtual documents and those without edit lock
            if ((flags & (int)_VSRDTFLAGS.RDT_VirtualDocument) != 0)
            {
                hierarchy = null;
                itemId = 0;
                hasEditLock = false;
                return false;
            }

            hasEditLock = (editLockCount > 0);
            return true;
        }

        private void populateProjects()
        {
            IEnumRunningDocuments rdtEnum;
            Debug.Assert(this.rdt.GetRunningDocumentsEnum(out rdtEnum) == VSConstants.S_OK);
            rdtEnum.Reset();

            uint[] cookie = new uint[1];
            uint fetched;
            while (rdtEnum.Next(1, cookie, out fetched) == VSConstants.S_OK)
            {
                string fileName;
                IVsHierarchy hierarchy;
                uint itemId;
                bool hasEditLock;
                if (this.getDocumentFromCookie(cookie[0], out fileName, out hierarchy, out itemId, out hasEditLock) && hasEditLock)
                {
                    // initialize the hierarchy observer as needed
                    var observer = this.hierarchyObservers.GetValue(hierarchy, delegate(IVsHierarchy h) { return new HierarchyObserver(this, h); });
                    observer.openFileItemIds.Add(fileName, itemId);
                }
            }
        }

        public int OnAfterOpenProject(IVsHierarchy pHierarchy, int fAdded)
        {
            var hierarchyObserver = new HierarchyObserver(this, pHierarchy);
            this.hierarchyObservers.Add(pHierarchy, hierarchyObserver);
            return VSConstants.S_OK;
        }

        public int OnBeforeCloseProject(IVsHierarchy pHierarchy, int fRemoved)
        {
            Debug.Assert(this.hierarchyObservers.Remove(pHierarchy));
            return VSConstants.S_OK;
        }

        #region placeholder IVsSolutionEvents methods

        public int OnAfterCloseSolution(object pUnkReserved)
        {
            return VSConstants.S_OK;
        }

        public int OnAfterLoadProject(IVsHierarchy pStubHierarchy, IVsHierarchy pRealHierarchy)
        {
            return VSConstants.S_OK;
        }

        public int OnAfterOpenSolution(object pUnkReserved, int fNewSolution)
        {
            return VSConstants.S_OK;
        }

        public int OnBeforeCloseSolution(object pUnkReserved)
        {
            return VSConstants.S_OK;
        }

        public int OnBeforeUnloadProject(IVsHierarchy pRealHierarchy, IVsHierarchy pStubHierarchy)
        {
            return VSConstants.S_OK;
        }

        public int OnQueryCloseProject(IVsHierarchy pHierarchy, int fRemoving, ref int pfCancel)
        {
            return VSConstants.S_OK;
        }

        public int OnQueryCloseSolution(object pUnkReserved, ref int pfCancel)
        {
            return VSConstants.S_OK;
        }

        public int OnQueryUnloadProject(IVsHierarchy pRealHierarchy, ref int pfCancel)
        {
            return VSConstants.S_OK;
        }

        #endregion

        public int OnAfterFirstDocumentLock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining)
        {
            if ((dwRDTLockType & (int)_VSRDTFLAGS.RDT_EditLock) != 0)
            {
                string fileName;
                IVsHierarchy hierarchy;
                uint itemId;
                bool hasEditLock; // ignored, we already know it's in the observer's view
                if (this.getDocumentFromCookie(docCookie, out fileName, out hierarchy, out itemId, out hasEditLock))
                {
                    HierarchyObserver hierarchyObserver;
                    if (this.hierarchyObservers.TryGetValue(hierarchy, out hierarchyObserver))
                    {
                        this.openFileNames.Add(fileName);
                        hierarchyObserver.openFileItemIds.Add(fileName, itemId);
                    }
                }
            }
            return VSConstants.S_OK;
        }

        public int OnBeforeLastDocumentUnlock(uint docCookie, uint dwRDTLockType, uint dwReadLocksRemaining, uint dwEditLocksRemaining)
        {
            if ((dwRDTLockType & (int)_VSRDTFLAGS.RDT_EditLock) != 0)
            {
                string fileName;
                IVsHierarchy hierarchy;
                uint itemId;
                bool hasEditLock; // ignored, we already know it's in the observer's view
                if (this.getDocumentFromCookie(docCookie, out fileName, out hierarchy, out itemId, out hasEditLock))
                {
                    HierarchyObserver hierarchyObserver;
                    if (this.hierarchyObservers.TryGetValue(hierarchy, out hierarchyObserver))
                    {
                        this.openFileNames.Remove(fileName);
                        hierarchyObserver.openFileItemIds.Remove(fileName);
                    }
                }
            }
            return VSConstants.S_OK;
        }

        #region placeholder IVsRunningDocTableEvents methods

        public int OnAfterAttributeChange(uint docCookie, uint grfAttribs)
        {
            return VSConstants.S_OK;
        }

        public int OnAfterDocumentWindowHide(uint docCookie, IVsWindowFrame pFrame)
        {
            return VSConstants.S_OK;
        }

        public int OnAfterSave(uint docCookie)
        {
            return VSConstants.S_OK;
        }

        public int OnBeforeDocumentWindowShow(uint docCookie, int fFirstShow, IVsWindowFrame pFrame)
        {
            return VSConstants.S_OK;
        }

        #endregion

        public void Dispose()
        {
            Debug.Assert(this.rdt.UnadviseRunningDocTableEvents(this.rdtEventCookie) == VSConstants.S_OK);
            Debug.Assert(this.solution.UnadviseSolutionEvents(this.solutionEventcookie) == VSConstants.S_OK);
        }
    }

    public class ProjectCache
    {
        private static readonly ConditionalWeakTable<EnvDTE.Project, Project> table = new ConditionalWeakTable<EnvDTE.Project, Project>();
        private static readonly ProjectObserver observer = new ProjectObserver();

        private static EnvDTE.Project getProjectFromHierarchy(IVsHierarchy hierarchy)
        {
            object projObj;
            Debug.Assert(hierarchy.GetProperty(VSConstants.VSITEMID_ROOT, (int)__VSHPROPID.VSHPROPID_ExtObject, out projObj) == VSConstants.S_OK);
            EnvDTE.Project project = projObj as EnvDTE.Project;
            Debug.Assert(project != null);
            return project;
        }

        public static IEnumerable<Project> GetProjects(ITextBuffer buffer)
        {
            ITextDocument document;
            if (!buffer.Properties.TryGetProperty(typeof(ITextDocument), out document))
            {
                yield break;
            }

            var projects = new List<Project>();
            foreach (var hierarchy in observer.ProjectHierarchies)
            {
                ProjectObserver.HierarchyObserver hierarchyObserver;
                if (observer.hierarchyObservers.TryGetValue(hierarchy, out hierarchyObserver))
                {
                    if (hierarchyObserver.openFileItemIds.ContainsKey(document.FilePath))
                    {
                        yield return Get(getProjectFromHierarchy(hierarchy));
                    }
                }
            }
        }

        public static Project Get(EnvDTE.Project project)
        {
            // this is thread-safe, though it may create Project multiple times
            return table.GetValue(project, p => new Project(p));
        }
    }
}
