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

    public struct NameEntry
    {
        public string Name;
        public uint Scope;
    }

    internal class NameEntriesHandle : SafeHandle
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern void kailua_parse_tree_free_names(IntPtr entries, int nentries);

        private NameEntriesHandle() : base(IntPtr.Zero, true) { }

        internal int Size = -1;

        public override bool IsInvalid
        {
            get
            {
                return this.handle.Equals(IntPtr.Zero);
            }
        }

        protected override bool ReleaseHandle()
        {
            if (this.Size < 0)
            {
                throw new NativeException("NameEntriesHandle does not have its Size set");
            }

            lock (this)
            {
                kailua_parse_tree_free_names(this.handle, this.Size);
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

        [StructLayout(LayoutKind.Sequential)]
        private struct NativeNameEntry
        {
            private IntPtr namePtr;
            private UIntPtr nameLen;
            private uint scope;

            public string Name
            {
                get
                {
                    byte[] buffer = new byte[(int)this.nameLen];
                    Marshal.Copy(this.namePtr, buffer, 0, buffer.Length);
                    return Encoding.UTF8.GetString(buffer);
                }
            }

            public uint Scope
            {
                get { return this.scope; }
            }

            public NameEntry ToManaged()
            {
                NameEntry entry;
                entry.Name = this.Name;
                entry.Scope = this.Scope;
                return entry;
            }
        }

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
            var tree = this.native; // avoid GC while iterating
            
            int nentries;
            NameEntriesHandle entriesHandle;
            lock (tree)
            {
                nentries = kailua_parse_tree_names_at_pos(tree, ref pos, out entriesHandle);
            }
            if (nentries < 0)
            {
                throw new NativeException("internal error while retrieving names visible at given position");
            }
            entriesHandle.Size = nentries; // required for deallocation

            // `entriesHandle` refers to the current parse tree
            long entriesPtr = entriesHandle.DangerousGetHandle().ToInt64();
            for (int i = 0; i < nentries; ++i)
            {
                var nameEntry = (NativeNameEntry)Marshal.PtrToStructure(new IntPtr(entriesPtr), typeof(NativeNameEntry));
                yield return nameEntry.ToManaged();
                entriesPtr += Marshal.SizeOf(typeof(NativeNameEntry)); // not sizeof()
            }
        }

        public void Dispose()
        {
            this.native.Dispose();
        }
    }
}