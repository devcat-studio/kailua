using System;
using System.Diagnostics;
using System.Runtime.InteropServices;

namespace Kailua.Native
{
    internal class CheckerHandle : SafeHandle
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern void kailua_checker_free(IntPtr checker);

        private CheckerHandle() : base(IntPtr.Zero, true) { }

        public override bool IsInvalid
        {
            get
            {
                return this.handle.Equals(IntPtr.Zero);
            }
        }

        protected override bool ReleaseHandle()
        {
            lock (this)
            {
                kailua_checker_free(this.handle);
            }
 	        return true;
        }
    }

    public class Checker : IDisposable
    {
        private delegate int SourceCallback(
            IntPtr path,
            int pathlen,
            IntPtr userdata,
            [Out] out IntPtr tree);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern CheckerHandle kailua_checker_new(
            SourceCallback callback,
            IntPtr callback_data,
            ReportHandle report);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_checker_exec(
            CheckerHandle checker,
            [MarshalAs(UnmanagedType.LPWStr)] String path,
            int pathlen,
            [Out] out CheckerOutputHandle output);

        internal CheckerHandle native;
        private SourceCallback nativeCallback; // avoid GC
        internal Func<String, ParseTree> callback;
        internal Exception lastException;

        internal int InternalCallback(IntPtr path, int pathlen, IntPtr userdata, out IntPtr tree)
        {
            tree = IntPtr.Zero;
            this.lastException = null;

            try
            {
                var pathstr = Marshal.PtrToStringUni(path, pathlen);
                if (pathstr == null) return -1;

                var treeobj = this.callback(pathstr);
                if (treeobj != null)
                {
                    tree = treeobj.native.DangerousGetHandle();
                }
                return 0; // tree can be null
            }
            catch (Exception e)
            {
                this.lastException = e;
                return -1;
            }
        }

        public Checker(Func<String, ParseTree> callback, Report report)
        {
            this.callback = callback;
            this.nativeCallback = this.InternalCallback;
            this.native = kailua_checker_new(this.nativeCallback, IntPtr.Zero, report.native);
            if (this.native.IsInvalid)
            {
                throw new NativeException("internal error while initializing a checker");
            }
        }

        public bool Execute(String mainpath, out CheckerOutput output)
        {
            int ret;
            Exception lastException;
            CheckerOutputHandle nativeOutput;
            lock (this.native)
            {
                this.lastException = null;
                try
                {
                    ret = kailua_checker_exec(this.native, mainpath, mainpath.Length, out nativeOutput);
                }
                finally
                {
                    lastException = this.lastException;
                    this.lastException = null;
                }
            }
            if (ret < 0)
            {
                throw new NativeException("internal error while checking a source code");
            }
            output = (nativeOutput.IsInvalid ? null : new Native.CheckerOutput(nativeOutput));
            if (lastException != null)
            {
                throw lastException;
            }
            return (ret == 0);
        }

        public void Dispose()
        {
            this.native.Dispose();
        }
    }
}