using System;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace Kailua
{
    internal class ReportErrorListProvider : ErrorListProvider
    {
        private IVsHierarchy hierarchy;

        public ReportErrorListProvider(EnvDTE.Project project) : base(getDTEServiceProvider())
        {
            this.hierarchy = getHierarchy(project);

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

        internal static IVsHierarchy getHierarchy(EnvDTE.Project project)
        {
            var solution = Package.GetGlobalService(typeof(SVsSolution)) as IVsSolution;
            IVsHierarchy hierarchy;
            if (solution.GetProjectOfUniqueName(project.UniqueName, out hierarchy) == VSConstants.S_OK)
            {
                return hierarchy;
            }
            else
            {
                return null;
            }
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
        public void AddReport(ReportData data)
        {
            var task = new ErrorTask();
            task.Category = TaskCategory.User;
            task.ErrorCategory = reportKindToTaskErrorCategory(data.Kind);
            task.Text = data.Message;
            task.Line = (data.Line > 0 ? data.Line - 1 : -1); // 0-based
            task.Column = (data.Column > 0 ? data.Column - 1 : -1); // 0-based
            task.Document = data.FileName;
            task.HierarchyItem = this.hierarchy;
            task.Navigate += (object sender, EventArgs e) =>
            {
                var errorTask = sender as ErrorTask;
                errorTask.Line += 1; // Navigate expects 1-based line number
                this.Navigate(errorTask, new Guid(EnvDTE.Constants.vsViewKindCode));
                errorTask.Line -= 1;
            };
            this.Tasks.Add(task);
        }
    }
}
