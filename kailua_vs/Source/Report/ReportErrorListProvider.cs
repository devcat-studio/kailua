using System;
using System.Diagnostics;
using Microsoft.VisualStudio.Shell;

namespace Kailua
{
    internal class ReportErrorListProvider : ErrorListProvider
    {
        public ReportErrorListProvider() : base(getDTEServiceProvider())
        {
            this.ProviderName = "Kailua";
            this.ProviderGuid = new Guid("4eb53e1e-8da6-4e30-916d-561b7d2eb6c4");
        }

        internal static ServiceProvider getDTEServiceProvider()
        {
            // a valid IServiceProvider can only be retrieved via DTE interface
            var dte2 = Package.GetGlobalService(typeof(EnvDTE.DTE)) as EnvDTE80.DTE2;
            var sp = (Microsoft.VisualStudio.OLE.Interop.IServiceProvider)dte2;
            Trace.Assert(sp != null, "failed to retrieve VS shell service provider");
            return new ServiceProvider(sp);
        }

        internal TaskErrorCategory reportKindToTaskErrorCategory(Native.ReportKind kind)
        {
            switch (kind)
            {
                case Native.ReportKind.Warning:
                    return TaskErrorCategory.Warning;
                case Native.ReportKind.Error:
                    return TaskErrorCategory.Error;
                case Native.ReportKind.Fatal:
                    return TaskErrorCategory.Error;
                default:
                    return TaskErrorCategory.Message;
            }
        }

        // TODO sourcePath should really be reconstructed from ReportData, we still don't have stable Source interface
        public void AddReport(Native.Source source, Native.ReportData data, string sourcePath)
        {
            var task = new ErrorTask();
            task.Category = TaskCategory.User;
            task.ErrorCategory = reportKindToTaskErrorCategory(data.Kind);
            task.Text = data.Message;
            if (data.Span.IsValid)
            {
                Native.Span lineSpan;
                int lineNo = source.LineFromPos(data.Span.Begin, out lineSpan);
                if (lineNo > 0)
                {
                    task.Line = lineNo - 1; // 0-based
                    task.Column = data.Span.Begin.Offset - lineSpan.Begin.Offset;
                    task.Document = sourcePath;
                    // TODO set task.HierarchyItem if possible
                    task.Navigate += (object sender, EventArgs e) =>
                    {
                        var errorTask = sender as ErrorTask;
                        errorTask.Line += 1; // Navigate expects 1-based line number
                        this.Navigate(errorTask, new Guid(EnvDTE.Constants.vsViewKindCode));
                        errorTask.Line -= 1;
                    };
                }
            }

            this.Tasks.Add(task);
        }
    }
}
