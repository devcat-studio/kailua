using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using Microsoft.VisualStudio.Text;
using Kailua.Util.Extensions;

namespace Kailua
{
    public static class ProjectCache
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

        public static IEnumerable<Project> GetProjects(string filePath)
        {
            if (filePath == null)
            {
                throw new ArgumentNullException("filePath");
            }

            foreach (var project in observer.GetProjects(filePath))
            {
                yield return Get(project);
            }
        }

        public static IEnumerable<Project> GetProjects(ITextBuffer buffer)
        {
            var path = buffer.GetFilePath();
            if (path != null)
            {
                foreach (var project in GetProjects(path))
                {
                    yield return project;
                }
            }
        }

        public static Project GetAnyProject(String filePath)
        {
            if (filePath == null)
            {
                throw new ArgumentNullException("filePath");
            }

            foreach (var project in observer.GetProjects(filePath))
            {
                // XXX do not try multiple times when the file is shared by multiple projects
                // ideally we should have one ProjectFile per unique file path, but that's annoying and rare anyway
                // while inefficient this should be correct at least
                return Get(project);
            }
            return null;
        }

        public static Project GetAnyProject(ITextBuffer buffer)
        {
            var path = buffer.GetFilePath();
            if (path != null)
            {
                return GetAnyProject(path);
            }
            else
            {
                return null;
            }
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
