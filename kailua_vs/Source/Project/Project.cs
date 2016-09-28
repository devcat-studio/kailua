using System;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Linq;

namespace Kailua
{
    [DebuggerDisplay("{{Kailua.Project name={name}}}")]
    public class Project : IDisposable
    {
        internal string name;
        internal ConcurrentDictionary<string, ProjectFile> files;
        internal ProjectUIThread uiThread;

        private Native.Source source;

        internal CancellationTokenSource cts;
        internal Native.Report report;
        internal List<Native.ReportData> reportData;
        private Task checkTask;
        private readonly object syncLock = new object();

        public Project(EnvDTE.Project project)
        {
            this.name = project.FullName;
            this.files = new ConcurrentDictionary<string, ProjectFile>();

            this.source = new Native.Source();

            this.cts = null;
            this.report = null;
            this.reportData = new List<Native.ReportData>();
            this.checkTask = null;

            this.uiThread = new ProjectUIThread(this);
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
            projectFile.BeforeReset += delegate() { this.OnBeforeReset(fileName); };
            Trace.Assert(this.files.TryAdd(fileName, projectFile));

            Log.Write("added: {0} at {1}", fileName, name);

            lock (this.syncLock)
            {
                this.resetUnlocked();
            }
        }

        internal void OnFileRemoved(string fileName)
        {
            ProjectFile projectFile;
            Trace.Assert(this.files.TryRemove(fileName, out projectFile));

            Log.Write("removed: {0} at {1}", fileName, name);

            lock (this.syncLock)
            {
                this.resetUnlocked();
            }
        }

        internal void OnBeforeReset(string fileName)
        {
            Log.Write("cancel requested by {0}", fileName);

            lock (this.syncLock)
            {
                this.resetUnlocked();
            }
        }

        public Task CheckTask
        {
            get
            {
                lock (this.syncLock)
                {
                    this.ensureCheckTaskUnlocked();
                    return this.checkTask;
                }
            }
        }

        private void resetUnlocked()
        {
            if (this.cts != null)
            {
                this.cts.Cancel();
                this.cts.Dispose();
                this.cts = null;
            }

            if (this.report != null)
            {
                this.report.Dispose();
                this.report = null;
            }
            this.reportData.Clear();

            this.checkTask = null;
        }

        private void ensureCheckTaskUnlocked()
        {
            if (this.checkTask != null)
            {
                return;
            }

            // if there are no parent tasks we just make it dummy
            if (this.files.IsEmpty)
            {
                this.checkTask = Task.FromResult(false);
                return;
            }

            Debug.Assert(this.cts == null);
            this.cts = new CancellationTokenSource();
            
            // activate all lexing and parsing jobs, as much asynchronously as possible
            var parents = new List<Task<Native.ParseTree>>();
            var fileNames = new List<string>();
            foreach (var file in this.files)
            {
                // this is a core bit, ParseTreeTask will activate the parsing job and any prerequisites
                // asynchonrously, as opposed to TokenStreamTask which tends to be synchronous.
                // it *will* block if other thread is running TokenStreamTask synchronously, but only momentarily.
                parents.Add(file.Value.ParseTreeTask);
                fileNames.Add(file.Key);
            }

            this.checkTask = Task.Factory.ContinueWhenAll(parents.ToArray(), tasks =>
            {
                // this may fault, which is fine---we cannot check without parsed trees
                var trees = (from task in tasks select task.Result).ToArray();

                // wait for a short amount of time, and if the cancel is requested restart the timer
                Task.Delay(500, this.cts.Token).Wait();
                return trees;
            }, this.cts.Token, TaskContinuationOptions.None, TaskScheduler.Default).ContinueWith((Task<Native.ParseTree[]> treesTask) =>
            {
                var trees = treesTask.Result;
                Log.Write("starting the checking job");

                string entryFileName = null;
                foreach (var e in fileNames.Zip(trees, Tuple.Create))
                {
                    var fileName = e.Item1;
                    var tree = e.Item2;
                    if (tree.HasPrimitiveOpen)
                    {
                        if (entryFileName == null)
                        {
                            entryFileName = fileName;
                        }
                        else
                        {
                            Log.Write("multiple entry points detected, stopping");
                            var reportData = new Native.ReportData(
                                Native.ReportKind.Fatal,
                                Native.Span.Dummy,
                                Properties.Strings.MultipleEntryPoints);
                            this.reportData.Add(reportData);
                            throw new Exception();
                        }
                    }
                }

                if (entryFileName == null)
                {
                    Log.Write("no entry points detected, stopping");
                    var reportData = new Native.ReportData(
                        Native.ReportKind.Fatal,
                        Native.Span.Dummy,
                        Properties.Strings.NoEntryPoint);
                    this.reportData.Add(reportData);
                    throw new Exception();
                }

                // TODO continue parsing here
                Log.Write("found an entry point at {0}", entryFileName);
            }, this.cts.Token, TaskContinuationOptions.None, TaskScheduler.Default);
        }

        public void Dispose()
        {
            this.source.Dispose();
        }
    }
}
