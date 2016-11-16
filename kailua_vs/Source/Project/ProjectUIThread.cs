using System;
using System.Collections.Generic;
using Microsoft.VisualStudio.Shell;
using Kailua.Util.Extensions;
using Task = System.Threading.Tasks.Task;

namespace Kailua
{
    public class ProjectUIThread : IDisposable
    {
        internal WeakReference<Project> project;
        internal ReportErrorListProvider errorListProvider;
        private bool disposed = false;
        private object syncLock = new object();

        public ProjectUIThread(Project project, EnvDTE.Project dteProject)
        {
            project.ReportDataChanged += this.OnReportDataChanged;
            this.project = new WeakReference<Project>(project);
            this.errorListProvider = new ReportErrorListProvider(dteProject);
        }

        internal void OnReportDataChanged(IList<ReportData> reportData)
        {
            ThreadHelper.JoinableTaskFactory.RunAsync(async delegate ()
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                Project project;
                if (this.disposed || !this.project.TryGetTarget(out project))
                {
                    // the project is dead, no need to continue
                    return;
                }

                lock (this.syncLock)
                {
                    this.updateErrorListUnlocked(project, reportData);
                }
            });
        }

        private void updateErrorListUnlocked(Project project, IList<ReportData> reportData)
        {
            this.errorListProvider.Tasks.Clear();
            foreach (var data in reportData)
            {
                this.errorListProvider.AddReport(data);
            }
            this.errorListProvider.Refresh();
        }

        public void Dispose()
        {
            lock (this.syncLock)
            {
                this.disposed = true;
                this.errorListProvider.Dispose();
            }
        }
    }
}
