using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Collections.Concurrent;

namespace Kailua
{
    [DebuggerDisplay("{{Kailua.Project name={name}}}")]
    public class Project : IDisposable
    {
        internal string name;
        internal ConcurrentDictionary<string, ProjectFile> files;

        private Native.Source source;
        private readonly object reportMonitor = new object();

        public Project(EnvDTE.Project project)
        {
            this.name = project.FullName;
            this.files = new ConcurrentDictionary<string, ProjectFile>();

            this.source = new Native.Source();
        }

        public Native.Source Source
        {
            get { return this.source; }
        }

        public ProjectFile this[string fileName]
        {
            get
            {
                ProjectFile projectFile;
                if (!this.files.TryGetValue(fileName, out projectFile))
                {
                    throw new KeyNotFoundException();
                }
                return projectFile;
            }
        }

        internal void OnFileAdded(string fileName)
        {
            ProjectFile projectFile = new ProjectFile(this, fileName);
            Trace.Assert(this.files.TryAdd(fileName, projectFile));
            Log.Write("added: {0} at {1}", fileName, name);
        }

        internal void OnFileRemoved(string fileName)
        {
            ProjectFile projectFile;
            Trace.Assert(this.files.TryRemove(fileName, out projectFile));
            Log.Write("removed: {0} at {1}", fileName, name);
        }

        public void Dispose()
        {
            this.source.Dispose();
        }
    }
}
