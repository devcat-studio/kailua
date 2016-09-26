using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace Kailua
{
    public class ProjectFile : IDisposable
    {
        internal Project project;
        internal Native.Unit unit = Native.Unit.Dummy;
        internal string path;

        internal CancellationTokenSource cts;
        internal Native.Report report;
        internal List<Native.ReportData> reportData;
        private string sourceText;
        private Task<Native.Span> sourceSpanTask;
        private Task<Native.TokenStream> tokenStreamTask;
        private Task<Native.ParseTree> parseTreeTask;

        private readonly object syncLock = new object();

        internal ProjectFile(Project project, string path)
        {
            this.project = project;
            this.path = path;

            this.cts = null;
            this.report = null;
            this.reportData = new List<Native.ReportData>();
            this.sourceText = null;
            this.sourceSpanTask = null;
            this.tokenStreamTask = null;
            this.parseTreeTask = null;
        }

        private Task<T> taskFromResult<T>(T result)
        {
            var taskSource = new TaskCompletionSource<T>();
            taskSource.SetResult(result);
            return taskSource.Task;
        }

        public List<Native.ReportData> ReportData
        {
            get { return this.reportData; }
        }

        // this can be set to null to make it read directly from the filesystem
        public string SourceText
        {
            get { return this.sourceText; }

            set
            {
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
                    this.ensureSourceSpanTaskUnlocked(true);
                    return this.sourceSpanTask;
                }
            }
        }

        public Task<Native.TokenStream> TokenStreamTask
        {
            get
            {
                lock (this.syncLock)
                {
                    this.ensureTokenStreamTaskUnlocked(true);
                    return this.tokenStreamTask;
                }
            }
        }

        public Native.TokenStream TokenStream
        {
            set
            {
                if (value == null)
                {
                    throw new ArgumentNullException("TokenStream");
                }

                lock (this.syncLock)
                {
                    this.resetUnlocked();
                    this.tokenStreamTask = taskFromResult(value);
                }
            }
        }

        public Task<Native.ParseTree> ParseTreeTask
        {
            get
            {
                lock (this.syncLock)
                {
                    this.ensureParseTreeTaskUnlocked(false);
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

                lock (this.syncLock)
                {
                    this.resetUnlocked();
                    this.parseTreeTask = taskFromResult(value);
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

            this.sourceText = null;
            this.sourceSpanTask = null;
            this.tokenStreamTask = null;
            this.parseTreeTask = null;
        }

        private void readReportData(Native.Report report, List<Native.ReportData> reportData)
        {
            while (true)
            {
                var data = report.GetNext();
                if (data == null)
                {
                    break;
                }
                reportData.Add(data.Value);
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
                }

                return span;
            };

            if (sync)
            {
                this.sourceSpanTask = this.taskFromResult(job());
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
                    return new Native.TokenStream(this.project.Source, sourceSpan, report);
                }
                finally
                {
                    // may continue to return reports even on error
                    this.readReportData(report, reportData);
                }
            };

            if (sync)
            {
                this.tokenStreamTask = this.taskFromResult(job(this.sourceSpanTask));
            }
            else
            {
                this.tokenStreamTask = this.sourceSpanTask.ContinueWith(job, this.cts.Token, TaskContinuationOptions.None, TaskScheduler.Default);
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
                    return new Native.ParseTree(stream, report);
                }
                finally
                {
                    // may continue to return reports even on error
                    this.readReportData(report, reportData);
                }
            };

            if (sync)
            {
                this.parseTreeTask = this.taskFromResult(job(this.tokenStreamTask));
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
        }
    }
}
