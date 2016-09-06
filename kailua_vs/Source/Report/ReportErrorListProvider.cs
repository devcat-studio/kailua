using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Text.Adornments;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Utilities;

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
            Debug.Assert(sp != null, "failed to retrieve VS shell service provider");
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
        public void AddReport(ITextSnapshot snapshot, Native.ReportData data, String sourcePath)
        {
            var span = data.Span.AttachSnapshot(snapshot);

            var task = new ErrorTask();
            task.Category = TaskCategory.User;
            task.ErrorCategory = reportKindToTaskErrorCategory(data.Kind);
            task.Text = data.Message;
            if (data.Span.IsValid)
            {
                var spanLine = span.Start.GetContainingLine();
                task.Line = spanLine.LineNumber;
                task.Column = span.Start - spanLine.Start;
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

            this.Tasks.Add(task);
        }
    }
}
