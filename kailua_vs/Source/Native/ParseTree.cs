using System;
using System.Text;
using System.Collections.Generic;
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

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_parse_tree_has_prim_open(
            ParseTreeHandle tree);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_parse_tree_names_at_pos(
            ParseTreeHandle tree,
            ref Pos pos,
            [Out] out NameEntriesHandle entries);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_parse_tree_global_names(
            ParseTreeHandle tree,
            [Out] out NameEntriesHandle entries);

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

        public bool HasPrimitiveOpen
        {
            get
            {
                int ret;
                lock (this.native)
                {
                    ret = kailua_parse_tree_has_prim_open(this.native);
                }
                if (ret < 0)
                {
                    throw new NativeException("internal error while checking if the parse tree has a primitive `open`");
                }
                return ret != 0;
            }
        }

        public IEnumerable<NameEntry> NamesAt(Pos pos)
        {
            int nentries;
            NameEntriesHandle entries;
            lock (this.native)
            {
                nentries = kailua_parse_tree_names_at_pos(this.native, ref pos, out entries);
            }
            if (nentries < 0)
            {
                throw new NativeException("internal error while retrieving names visible at given position");
            }
            return new NameEntries(this.native, entries, nentries);
        }

        public IEnumerable<NameEntry> GlobalNames
        {
            get
            {
                int nentries;
                NameEntriesHandle entries;
                lock (this.native)
                {
                    nentries = kailua_parse_tree_global_names(this.native, out entries);
                }
                if (nentries < 0)
                {
                    throw new NativeException("internal error while retrieving global names");
                }
                return new NameEntries(this.native, entries, nentries);
            }
        }

        public void Dispose()
        {
            this.native.Dispose();
        }
    }
}