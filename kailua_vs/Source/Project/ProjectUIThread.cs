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
        private IList<ReportData> lastReportData;
        private bool disposed = false;
        private object syncLock = new object();

        public ProjectUIThread(Project project, EnvDTE.Project dteProject)
        {
            project.ReportDataChanged += this.OnReportDataChanged;
            this.project = new WeakReference<Project>(project);
            this.errorListProvider = new ReportErrorListProvider(dteProject);
            this.lastReportData = null;
            this.Launch();
        }

        internal void OnReportDataChanged(IList<ReportData> reportData)
        {
            lock (this.syncLock)
            {
                this.lastReportData = reportData;
            }
        }

        private void updateErrorListUnlocked(Project project, IList<ReportData> reportData)
        {
            this.errorListProvider.Tasks.Clear();
            foreach (var data in reportData)
            {
                this.errorListProvider.AddReport(data);
            }
        }

        internal void Launch()
        {
            ThreadHelper.JoinableTaskFactory.RunAsync(async delegate()
            {
                await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();

                // try to relaunch the parsing job every second
                while (true)
                {
                    Project project;
                    if (this.disposed || !this.project.TryGetTarget(out project))
                    {
                        // the project is dead, no need to continue
                        break;
                    }

                    // note that we do NOT await for this task, we just launch them and leave it as is
                    var _ = project.CheckTask;

                    lock (this.syncLock)
                    {
                        if (this.lastReportData != null)
                        {
                            this.updateErrorListUnlocked(project, this.lastReportData);
                            this.lastReportData = null;
                        }
                    }

                    await Task.Delay(1000);
                }
            });
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
