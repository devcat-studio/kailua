﻿using System;
using System.Diagnostics;
using System.Threading;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Linq;
using Microsoft.VisualStudio.Text;

namespace Kailua
{
    [DebuggerDisplay("{{Kailua.Project name={name}}}")]
    public class Project : IDisposable
    {
        internal string name;
        internal ConcurrentDictionary<string, ProjectFile> files;
        internal ConcurrentDictionary<Native.Unit, ProjectFile> units;
        internal ProjectUIThread uiThread;

        private Native.Source source;

        internal CancellationTokenSource cts;
        internal Native.Report report;
        internal List<Native.ReportData> reportData;
        internal Dictionary<string, Native.Unit> tempUnits;
        internal Native.Checker checker;
        private Task checkTask;
        private readonly object syncLock = new object();

        public Project(EnvDTE.Project project)
        {
            this.name = project.FullName;
            this.files = new ConcurrentDictionary<string, ProjectFile>();
            this.units = new ConcurrentDictionary<Native.Unit, ProjectFile>();

            this.source = new Native.Source();

            this.cts = null;
            this.report = null;
            this.reportData = null;
            this.tempUnits = null;
            this.checker = null;
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
            projectFile.Dispose();

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
            // reallocation required, different threads may hold a handle to the previous list
            this.reportData = new List<Native.ReportData>();

            this.checkTask = null;
        }

        private void notifyReportDataChanged()
        {
            if (this.ReportDataChanged != null)
            {
                this.ReportDataChanged(this.reportData);
            }
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
            var pairs = new List<Tuple<string, ProjectFile>>();
            foreach (var file in this.files)
            {
                var fileNameNoCase = file.Key.ToLowerInvariant();
                if (!(fileNameNoCase.EndsWith(".lua") || fileNameNoCase.EndsWith(".kailua")))
                {
                    continue;
                }

                // this is a core bit, ParseTreeTaskAsync will activate the parsing job and any prerequisites
                // asynchonrously, as opposed to TokenStreamTask which tends to be synchronous.
                // it *will* block if other thread is running TokenStreamTask synchronously, but only momentarily.
                parents.Add(file.Value.ParseTreeTaskAsync);
                pairs.Add(Tuple.Create(file.Key, file.Value));
            }

            this.checkTask = Task.Factory.ContinueWhenAll(parents.ToArray(), tasks =>
            {
                // this may fault, which is fine---we cannot check without parsed trees
                var trees = new List<Native.ParseTree>();
                foreach (var e in pairs.Zip(tasks, Tuple.Create))
                {
                    var fileName = e.Item1.Item1;
                    var task = e.Item2;
                    try
                    {
                        trees.Add(task.Result);
                    }
                    catch (AggregateException)
                    {
                        Log.Write("the checking cannot proceed due to the parsing error at {0}", fileName);
                        this.notifyReportDataChanged();
                        throw;
                    }
                }

                // wait for a short amount of time, and if the cancel is requested restart the timer
                Task.Delay(500, this.cts.Token).Wait();
                return trees;
            }, this.cts.Token, TaskContinuationOptions.None, TaskScheduler.Default).ContinueWith(treesTask =>
            {
                var trees = treesTask.Result;
                Log.Write("starting the checking job");

                string entryFileName = null;
                var fileNameToTree = new Dictionary<string, Native.ParseTree>();
                // if the task is cancelled we are sure that this mapping would not be used
                var unitToSnapshot = new Dictionary<Native.Unit, ITextSnapshot>();
                foreach (var e in pairs.Zip(trees, Tuple.Create))
                {
                    var fileName = e.Item1.Item1;
                    var projectFile = e.Item1.Item2;
                    var tree = e.Item2;

                    var sourceSpan = projectFile.SourceSpanTask.Result;
                    fileNameToTree.Add(fileName, tree);
                    unitToSnapshot.Add(sourceSpan.Unit, projectFile.SourceSnapshot);
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
                            this.notifyReportDataChanged();

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
                    this.notifyReportDataChanged();

                    throw new Exception();
                }

                return Tuple.Create(entryFileName, fileNameToTree, unitToSnapshot);
            }, this.cts.Token, TaskContinuationOptions.None, TaskScheduler.Default).ContinueWith(task =>
            {
                var e = task.Result;
                var entryFileName = e.Item1;
                var fileNameToTrees = e.Item2;
                var unitToSnapshot = e.Item3;
                Log.Write("found an entry point at {0}", entryFileName);

                // we need to keep the temporary list of spans that requested but not in the project
                // so that we deallocate them after the checking.
                var fileNameToTempTrees = new Dictionary<string, Native.ParseTree>();
                var report = new Native.Report();
                try
                {
                    var checker = new Native.Checker(path =>
                    {
                        // try to return the existing tree
                        Native.ParseTree tree;
                        if (fileNameToTrees.TryGetValue(path, out tree) || fileNameToTempTrees.TryGetValue(path, out tree))
                        {
                            return tree;
                        }

                        // otherwise parse on demand
                        var span = this.source.AddFile(path);
                        var stream = new Native.TokenStream(this.source, span, report);
                        return new Native.ParseTree(stream, report);
                    }, report);

                    if (checker.Execute(entryFileName))
                    {
                        Log.Write("checking success");
                    }
                    else
                    {
                        Log.Write("checking failed");
                    }
                }
                catch (Exception ee)
                {
                    Log.Write("failed to check {0}: {1}", this.name, ee.Message);
                    throw;
                }
                finally
                {
                    // attach the saved snapshot to the span
                    foreach (var data_ in report)
                    {
                        var data = data_;
                        ITextSnapshot snapshot;
                        if (unitToSnapshot.TryGetValue(data.Span.Unit, out snapshot))
                        {
                            data.Snapshot = snapshot;
                        }
                        this.reportData.Add(data);
                    }

                    this.notifyReportDataChanged();
                }
            }, this.cts.Token, TaskContinuationOptions.None, TaskScheduler.Default);
        }

        public delegate void ReportDataChangedHandler(IList<Native.ReportData> reportData);

        public event ReportDataChangedHandler ReportDataChanged;

        public void Dispose()
        {
            // the final signal to reset the UI thread
            if (this.ReportDataChanged != null)
            {
                this.ReportDataChanged(new List<Native.ReportData>());
            }

            this.source.Dispose();
            this.uiThread.Dispose();
        }
    }
}
