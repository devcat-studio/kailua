using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace Kailua.Native
{
    internal class CheckerOutputHandle : SafeHandle
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern void kailua_checker_output_free(IntPtr output);

        private CheckerOutputHandle() : base(IntPtr.Zero, true) { }

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
                kailua_checker_output_free(this.handle);
            }
 	        return true;
        }
    }

    public class CheckerOutput : IDisposable
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_checker_output_global_names(
            CheckerOutputHandle output,
            [Out] out NameEntriesHandle entries);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_checker_output_fields_after_pos(
            CheckerOutputHandle output,
            ref Pos pos,
            [Out] out NameEntriesHandle entries);

        internal CheckerOutputHandle native;

        internal CheckerOutput(CheckerOutputHandle output)
        {
            if (output.IsInvalid)
            {
                throw new ArgumentException("invalid handle", "output");
            }
            this.native = output;
        }

        public IEnumerable<NameEntry> GlobalNames
        {
            get
            {
                int nentries;
                NameEntriesHandle entries;
                lock (this.native)
                {
                    nentries = kailua_checker_output_global_names(this.native, out entries);
                }
                if (nentries < 0)
                {
                    throw new NativeException("internal error while retrieving global names");
                }
                return new NameEntries(this.native, entries, nentries);
            }
        }

        public IEnumerable<NameEntry> FieldsAfter(Pos pos)
        {
            int nentries;
            NameEntriesHandle entries;
            lock (this.native)
            {
                nentries = kailua_checker_output_fields_after_pos(this.native, ref pos, out entries);
            }
            if (nentries < 0)
            {
                throw new NativeException("internal error while retrieving field names available at given position");
            }
            return new NameEntries(this.native, entries, nentries);
        }

        public void Dispose()
        {
            this.native.Dispose();
        }
    }
}