using System;
using System.Runtime.InteropServices;

namespace Kailua.Native
{
    internal class SourceHandle : SafeHandle
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern void kailua_source_free(IntPtr src);

        private SourceHandle() : base(IntPtr.Zero, true) { }

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
            Unit unit,
            [MarshalAs(UnmanagedType.LPWStr)] String path,
            int pathlen,
            [Out] out Span span);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_source_add_string(
            SourceHandle src,
            Unit unit,
            [MarshalAs(UnmanagedType.LPWStr)] String path,
            int pathlen,
            [MarshalAs(UnmanagedType.LPWStr)] String data,
            int datalen,
            [Out] out Span span);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_source_remove(
            SourceHandle src,
            Unit unit);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_source_line_from_pos(
            SourceHandle src,
            ref Pos pos,
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
                ret = kailua_source_add_file(this.native, Unit.Dummy, path, path.Length, out span);
            }
            if (ret != 0)
            {
                throw new NativeException("internal error while reading and adding a source code");
            }
            return span;
        }

        public Span AddString(String path, String data)
        {
            Span span;
            int ret;
            lock (this.native)
            {
                ret = kailua_source_add_string(this.native, Unit.Dummy, path, path.Length, data, data.Length, out span);
            }
            if (ret != 0)
            {
                throw new NativeException("internal error while adding a source code");
            }
            return span;
        }

        public Span ReplaceByFile(Unit unit, String path)
        {
            if (!unit.IsValid)
            {
                throw new ArgumentException("dummy Unit", "unit");
            }

            Span span;
            int ret;
            lock (this.native)
            {
                ret = kailua_source_add_file(this.native, unit, path, path.Length, out span);
            }
            if (ret != 0)
            {
                throw new NativeException("internal error while reading and updating a source code");
            }
            return span;
        }

        public Span ReplaceByString(Unit unit, String path, String data)
        {
            if (!unit.IsValid)
            {
                throw new ArgumentException("dummy Unit", "unit");
            }

            Span span;
            int ret;
            lock (this.native)
            {
                ret = kailua_source_add_string(this.native, unit, path, path.Length, data, data.Length, out span);
            }
            if (ret != 0)
            {
                throw new NativeException("internal error while updating a source code");
            }
            return span;
        }

        public void Remove(Unit unit)
        {
            if (!unit.IsValid)
            {
                throw new ArgumentException("dummy Unit", "unit");
            }

            int ret;
            lock (this.native)
            {
                ret = kailua_source_remove(this.native, unit);
            }
            if (ret != 0)
            {
                throw new NativeException("internal error while removing a source code");
            }
        }

        // 0 indicates that the position is invalid or dummy
        public int LineFromPos(Pos pos, out Span lineSpan)
        {
            int ret;
            lineSpan = Span.Dummy;
            lock (this.native)
            {
                ret = kailua_source_line_from_pos(this.native, ref pos, out lineSpan);
            }
            if (ret < 0)
            {
                throw new NativeException("internal error while removing a source code");
            }
            return ret;
        }

        public void Dispose()
        {
            this.native.Dispose();
        }
    }
}