using System;
using System.Runtime.InteropServices;

namespace Kailua.Native
{
    internal class ParseTreeHandle : SafeHandle
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern void kailua_parse_tree_free(IntPtr tree);

        private ParseTreeHandle() : base(IntPtr.Zero, true) { }

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
                kailua_parse_tree_free(this.handle);
            }
 	        return true;
        }
    }

    public class ParseTree : IDisposable
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern ParseTreeHandle kailua_parse_tree_new(
            TokenStreamHandle stream,
            ReportHandle report);

        internal ParseTreeHandle native;

        public ParseTree(TokenStream stream, Report report)
        {
            this.native = kailua_parse_tree_new(stream.native, report.native);
            stream.native.Leak(); // further use of this stream is invalid
            if (this.native.IsInvalid)
            {
                throw new NativeException("internal error while parsing a token stream");
            }
        }

        public void Dispose()
        {
            this.native.Dispose();
        }
    }
}