using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text;
using Kailua.Util.Extensions;

namespace Kailua
{
    public class ProjectFile : IDisposable
    {
        internal Project project;
        internal Native.Unit unit = Native.Unit.Dummy;
        internal string path;

        internal CancellationTokenSource cts;
        internal Native.Report report;
        internal List<ReportData> reportData;
        private ITextSnapshot sourceSnapshot;
        private string sourceText;
        private Task<Native.Span> sourceSpanTask;
        private Task<Native.TokenStream> tokenStreamTask;
        private Task<TokenList> tokenListTask;
        private Task<Native.ParseTree> parseTreeTask;
        private Native.ParseTree lastValidParseTree;
        private SortedSet<string> lastValidGlobalScope;

        private readonly object syncLock = new object();

        internal ProjectFile(Project project, string path)
        {
            this.project = project;
            this.path = path;

            this.cts = null;
            this.report = null;
            this.reportData = new List<ReportData>();
            this.sourceSnapshot = null;
            this.sourceText = null;
            this.sourceSpanTask = null;
            this.tokenStreamTask = null;
            this.tokenListTask = null;
            this.parseTreeTask = null;
            this.lastValidParseTree = null;
            this.lastValidGlobalScope = null;
        }

        public string Path
        {
            get { return this.path; }
        }

        public Native.Unit Unit
        {
            get { return this.unit; }
        }

        public IEnumerable<ReportData> ReportData
        {
            get
            {
                // they should be read atomically
                List<ReportData> reportData;
                ITextSnapshot sourceSnapshot;
                lock (this.syncLock)
                {
                    reportData = this.reportData;
                    sourceSnapshot = this.sourceSnapshot;
                }

                foreach (var data in reportData)
                {
                    yield return data;
                }
            }
        }

        // this can be set to null to make it read directly from the filesystem
        public ITextSnapshot SourceSnapshot
        {
            get
            {
                return this.sourceSnapshot;
            }

            set
            {
                if (this.BeforeReset != null)
                {
                    this.BeforeReset();
                }

                lock (this.syncLock)
                {
                    this.resetUnlocked();
                    if (value != null)
                    {
                        this.sourceSnapshot = value;
                        this.sourceText = value.GetText();
                    }
                }
            }
        }

        // this can be set to null to make it read directly from the filesystem
        public SnapshotSpan SourceSnapshotSpan
        {
            set
            {
                if (this.BeforeReset != null)
                {
                    this.BeforeReset();
                }

                lock (this.syncLock)
                {
                    this.resetUnlocked();
                    if (value != null)
                    {
                        this.sourceSnapshot = value.Snapshot;
                        this.sourceText = value.GetText();
                    }
                }
            }
        }

        // this can be set to null to make it read directly from the filesystem
        public string SourceText
        {
            get { return this.sourceText; }

            set
            {
                if (this.BeforeReset != null)
                {
                    this.BeforeReset();
                }

                lock (this.syncLock)
                {
                    this.resetUnlocked();
                    this.sourceText = value;
                }
            }
        }

        public Task<Native.Span> SourceSpanTask
        {
            get
            {
                lock (this.syncLock)
                {
                    this.ensureSourceSpanTaskUnlocked(sync: true);
                    return this.sourceSpanTask;
                }
            }
        }

        public Task<TokenList> TokenListTask
        {
            get
            {
                lock (this.syncLock)
                {
                    this.ensureTokenListTaskUnlocked(sync: true);
                    return this.tokenListTask;
                }
            }
        }

        public TokenList TokenList
        {
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException("TokenList");
                }

                if (this.BeforeReset != null)
                {
                    this.BeforeReset();
                }

                lock (this.syncLock)
                {
                    this.resetUnlocked();
                    this.tokenListTask = Task.FromResult(value);
                }
            }
        }

        public Task<Native.ParseTree> ParseTreeTask
        {
            get
            {
                lock (this.syncLock)
                {
                    // XXX originally it was async, but it seemed to cause a hard-to-understand sync issue in WPF (!).
                    // more specifically, the editor caret somehow goes to a SnapshotSpan with an invalid ITextSnapshot.
                    // the span does not seem to originate from Kailua, so we believe it's a delicate sync issue
                    // arising from the use of P/Invoke in other threads and task yields from the UI thread.
                    // since the issue does not reproduce when P/Invoke is used in the UI thread, we use this workaround.
                    this.ensureParseTreeTaskUnlocked(sync: true);
                    return this.parseTreeTask;
                }
            }
        }

        public Task<Native.ParseTree> ParseTreeTaskAsync
        {
            get
            {
                lock (this.syncLock)
                {
                    this.ensureParseTreeTaskUnlocked(sync: false);
                    return this.parseTreeTask;
                }
            }
        }

        public Native.ParseTree ParseTree
        {
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException("ParseTree");
                }

                if (this.BeforeReset != null)
                {
                    this.BeforeReset();
                }

                lock (this.syncLock)
                {
                    this.resetUnlocked();
                    this.parseTreeTask = Task.FromResult(value);
                }
            }
        }

        public Native.ParseTree LastValidParseTree
        {
            get
            {
                try
                {
                    return this.ParseTreeTask.Result;
                }
                catch (Exception)
                {
                    return this.lastValidParseTree;
                }
            }
        }

        public SortedSet<string> LastValidGlobalScopeIfAny
        {
            get
            {
                // unlike LastValidParseTree, this can be called for every file in the project
                // so we don't try to trigger the parsing (which will be eventually triggered from
                // the project-wide checking process).
                return this.lastValidGlobalScope;
            }
        }

        public event ResetHandler BeforeReset;

        public delegate void ResetHandler();

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
            this.reportData = new List<ReportData>();

            this.sourceSnapshot = null;
            this.sourceText = null;
            this.sourceSpanTask = null;
            this.tokenStreamTask = null;
            this.tokenListTask = null;
            this.parseTreeTask = null;
        }

        private void addReportsUnlocked(List<ReportData> reportData, Native.Report report)
        {
            foreach (var nativeData in report)
            {
                var data = new ReportData(project.Source, nativeData, this.sourceSnapshot);
                reportData.Add(data);
            }
        }

        private void ensureReportUnlocked()
        {
            if (this.report != null)
            {
                return;
            }

            this.report = new Native.Report();

            Debug.Assert(this.cts == null);
            this.cts = new CancellationTokenSource();
        }

        private void ensureSourceSpanTaskUnlocked(bool sync)
        {
            if (this.sourceSpanTask != null)
            {
                return;
            }

            this.ensureReportUnlocked();
            
            var sourceText = this.sourceText;
            Func<Native.Span> job = () =>
            {
                var source = this.project.Source;
                Native.Span span;
                if (this.unit.IsValid)
                {
                    if (sourceText == null)
                    {
                        span = source.ReplaceByFile(this.unit, path);
                    }
                    else
                    {
                        span = source.ReplaceByString(this.unit, this.path, sourceText);
                    }
                }
                else
                {
                    if (sourceText == null)
                    {
                        span = source.AddFile(this.path);
                    }
                    else
                    {
                        span = source.AddString(this.path, sourceText);
                    }

                    this.unit = span.Unit; // only used in this task, so no synchronization required
                    Debug.Assert(this.unit.IsValid);
                    Trace.Assert(this.project.units.TryAdd(this.unit, this));
                }

                return span;
            };

            if (sync)
            {
                this.sourceSpanTask = job.CreateSyncTask();
            }
            else
            {
                this.sourceSpanTask = Task.Factory.StartNew(job, this.cts.Token, TaskCreationOptions.None, TaskScheduler.Default);
            }
        }

        private void ensureTokenStreamTaskUnlocked(bool sync)
        {
            if (this.tokenStreamTask != null)
            {
                return;
            }

            this.ensureSourceSpanTaskUnlocked(sync);

            var report = this.report;
            var reportData = this.reportData;
            Func<Task<Native.Span>, Native.TokenStream> job = task =>
            {
                try
                {
                    var sourceSpan = task.Result;
                    try
                    {
                        return new Native.TokenStream(this.project.Source, sourceSpan, report);
                    }
                    catch (Exception e)
                    {
#if DEBUG
                        Log.Write("failed to tokenize {0}: {1}", this.path, e.Message);
#endif
                        throw e;
                    }
                }
                finally
                {
                    // may continue to return reports even on error
                    this.addReportsUnlocked(reportData, report);
                }
            };

            if (sync)
            {
                this.tokenStreamTask = job.CreateSyncTask(this.sourceSpanTask);
            }
            else
            {
                this.tokenStreamTask = this.sourceSpanTask.ContinueWith(job, this.cts.Token, TaskContinuationOptions.None, TaskScheduler.Default);
            }
        }

        private void ensureTokenListTaskUnlocked(bool sync)
        {
            if (this.tokenListTask != null)
            {
                return;
            }

            this.ensureTokenStreamTaskUnlocked(sync);

            var snapshot = this.sourceSnapshot;
            Func<Task<Native.TokenStream>, TokenList> job = task => new TokenList(snapshot, task.Result);

            if (sync)
            {
                this.tokenListTask = job.CreateSyncTask(this.tokenStreamTask);
            }
            else
            {
                this.tokenListTask = this.tokenStreamTask.ContinueWith(job, this.cts.Token, TaskContinuationOptions.None, TaskScheduler.Default);
            }
        }

        private void ensureParseTreeTaskUnlocked(bool sync)
        {
            if (this.parseTreeTask != null)
            {
                return;
            }

            this.ensureTokenStreamTaskUnlocked(sync);

            var report = this.report;
            var reportData = this.reportData;
            Func<Task<Native.TokenStream>, Native.ParseTree> job = task =>
            {
                try
                {
                    var stream = task.Result;
                    try
                    {
                        var tree = new Native.ParseTree(stream, report);
                        this.lastValidParseTree = tree;
                        this.lastValidGlobalScope = new SortedSet<string>(from e in tree.GlobalNames select e.Name);
                        return tree;
                    }
                    catch (Exception e)
                    {
#if DEBUG
                        Log.Write("failed to parse {0}: {1}", this.path, e.Message);
#endif
                        throw e;
                    }
                }
                finally
                {
                    // may continue to return reports even on error
                    this.addReportsUnlocked(reportData, report);
                }
            };

            if (sync)
            {
                this.parseTreeTask = job.CreateSyncTask(this.tokenStreamTask);
            }
            else
            {
                this.parseTreeTask = this.tokenStreamTask.ContinueWith(job, this.cts.Token, TaskContinuationOptions.None, TaskScheduler.Default);
            }
        }

        public void Dispose()
        {
            lock (this.syncLock)
            {
                this.resetUnlocked();
            }

            if (this.unit.IsValid)
            {
                ProjectFile projectFile;
                Trace.Assert(this.project.units.TryRemove(this.unit, out projectFile));
                Debug.Assert(Object.ReferenceEquals(projectFile, this)); 
            }
        }
    }
}
