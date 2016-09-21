using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Shell;

namespace Kailua
{
    public class Project : IDisposable
    {
        string name;

        public Project(EnvDTE.Project project)
        {
            this.name = project.FullName;
        }

        public void Dispose()
        {
        }
    }
}
