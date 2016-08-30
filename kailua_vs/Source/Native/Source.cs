using System;
using System.Runtime.InteropServices;

namespace Kailua.Native
{
    internal class SourceHandle : SafeHandle
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern void kailua_source_free(IntPtr src);

        public SourceHandle() : base(IntPtr.Zero, true) {}

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
                kailua_source_free(this.handle);
            }
 	        return true;
        }
    }

    public class Source : IDisposable
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern SourceHandle kailua_source_new();

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_source_add_file(
            SourceHandle src,
            [MarshalAs(UnmanagedType.LPWStr)] String path,
            int pathlen,
            [Out] out Span span);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_source_add_string(
            SourceHandle src,
            [MarshalAs(UnmanagedType.LPWStr)] String path,
            int pathlen,
            [MarshalAs(UnmanagedType.LPWStr)] String data,
            int datalen,
            [Out] out Span span);

        internal SourceHandle native;

        public Source()
        {
            this.native = kailua_source_new();
        }

        public Span AddFile(String path)
        {
            Span span;
            int ret;
            lock (this.native)
            {
                ret = kailua_source_add_file(this.native, path, path.Length, out span);
            }
            if (ret != 0)
            {
                throw new NativeException("internal error while reading a source code");
            }
            return span;
        }

        public Span AddString(String path, String data)
        {
            Span span;
            int ret;
            lock (this.native)
            {
                ret = kailua_source_add_string(this.native, path, path.Length, data, data.Length, out span);
            }
            if (ret != 0)
            {
                throw new NativeException("internal error while adding a source code");
            }
            return span;
        }

        public void Dispose()
        {
            this.native.Dispose();
        }
    }
}