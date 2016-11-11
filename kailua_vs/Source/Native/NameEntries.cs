using System;
using System.Text;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Collections;

namespace Kailua.Native
{
    public struct NameEntry
    {
        public string Name;
        public int Scope;
    }

    internal class NameEntriesHandle : SafeHandle
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern void kailua_free_names(IntPtr entries, int nentries);

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
                kailua_free_names(this.handle, this.Size);
            }
            return true;
        }
    }

    internal class NameEntries : IEnumerable<NameEntry>, IDisposable
    {
        [StructLayout(LayoutKind.Sequential)]
        private struct NativeNameEntry
        {
            private IntPtr namePtr;
            private UIntPtr nameLen;
            private int scope;

            public string Name
            {
                get
                {
                    byte[] buffer = new byte[(int)this.nameLen];
                    Marshal.Copy(this.namePtr, buffer, 0, buffer.Length);
                    return Encoding.UTF8.GetString(buffer);
                }
            }

            public int Scope
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

        // parent object is required for avoiding GC
        internal object parent;
        internal NameEntriesHandle native;

        internal NameEntries(object parent, NameEntriesHandle entries, int nentries)
        {
            if (entries == null)
            {
                throw new ArgumentNullException("entries");
            }

            this.parent = parent;
            this.native = entries;
            this.native.Size = nentries;
        }

        public IEnumerator<NameEntry> GetEnumerator()
        {
            var tree = this.parent; // avoid GC while iterating

            // `entriesHandle` refers to the current parse tree
            long entriesPtr = this.native.DangerousGetHandle().ToInt64();
            for (int i = 0; i < this.native.Size; ++i)
            {
                var nameEntry = (NativeNameEntry)Marshal.PtrToStructure(new IntPtr(entriesPtr), typeof(NativeNameEntry));
                yield return nameEntry.ToManaged();
                entriesPtr += Marshal.SizeOf(typeof(NativeNameEntry)); // not sizeof()
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }

        public void Dispose()
        {
            this.native.Dispose();
        }
    }
}