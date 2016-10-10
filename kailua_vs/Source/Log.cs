using System;
using System.Diagnostics;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;

namespace Kailua
{
    public class Log
    {
        public static void Write(string format, params object[] args)
        {
            var window = Package.GetGlobalService(typeof(SVsOutputWindow)) as IVsOutputWindow;
            Guid paneGuid = VSConstants.GUID_OutWindowDebugPane;
            IVsOutputWindowPane pane;
            window.GetPane(ref paneGuid, out pane);
            Trace.Assert(pane != null);
            pane.OutputString(String.Format(format, args) + "\n");
            pane.Activate();
        }
    }
}
