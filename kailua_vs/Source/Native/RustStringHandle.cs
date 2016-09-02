using System;
using System.Runtime.InteropServices;

namespace Kailua.Native
{
    internal class RustStringHandle : SafeHandle
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern void kailua_strbuf_free(IntPtr buf);

        public RustStringHandle() : base(IntPtr.Zero, false) { }

        public override bool IsInvalid
        {
            get
            {
                return this.handle.Equals(IntPtr.Zero);
            }
        }

        public String String
        {
            get
            {
                return Marshal.PtrToStringUni(this.handle);
            }
        }

        protected override bool ReleaseHandle()
        {
            lock (this)
            {
                kailua_strbuf_free(this.handle);
            }
            return true;
        }
    }

}
