using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Shell;
using Task = System.Threading.Tasks.Task;

namespace Kailua
{
    public class ProjectUIThread
    {
        internal WeakReference<Project> project;

        public ProjectUIThread(Project project)
        {
            this.project = new WeakReference<Project>(project);
            this.Launch();
        }

        internal void Launch()
        {
            ThreadHelper.JoinableTaskFactory.RunAsync(async delegate()
            {
                // try to relaunch the parsing job every second
                while (true)
                {
                    Project project;
                    if (!this.project.TryGetTarget(out project))
                    {
                        // the project is dead, no need to continue
                        break;
                    }

                    // note that we do NOT await for this task, we just launch them and leave it as is
                    var _ = project.CheckTask;

                    await Task.Delay(1000);
                }
            });
        }
    }
}
