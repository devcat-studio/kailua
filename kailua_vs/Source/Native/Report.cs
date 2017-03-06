using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Globalization;

namespace Kailua.Native
{
    internal class ReportHandle : SafeHandle
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern void kailua_report_free(IntPtr report);

        private ReportHandle() : base(IntPtr.Zero, true) { }

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
                kailua_report_free(this.handle);
            }
            return true;
        }
    }

    public enum ReportKind : byte
    {
        Note = 0,
        Info = 1,
        Cause = 5,
        Warning = 2,
        Error = 3,
        Fatal = 4,
    }

    public struct ReportData
    {
        public ReportKind Kind { get; private set; }
        public Span Span { get; private set; }
        public String Message { get; private set; }

        internal ReportData(ReportKind kind, Span span, String msg)
            : this()
        {
            this.Kind = kind;
            this.Span = span;
            this.Message = msg;
        }
    }

    public class Report : IDisposable, IEnumerable<ReportData>
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern ReportHandle kailua_report_new(
            [MarshalAs(UnmanagedType.LPWStr)] String lang);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern int kailua_report_get_next(
            ReportHandle report,
            out ReportKind kind,
            out Span span,
            out RustStringHandle msg);

        internal ReportHandle native;

        public Report()
        {
            var cultureName = CultureInfo.CurrentUICulture.Name;
            var lang = cultureName.Substring(0, 2); // first two letters are ISO 639-1 alpha 2

            this.native = kailua_report_new(lang);
            if (this.native.IsInvalid)
            {
                throw new NativeException("internal error while creating a report sink");
            }
        }

        public ReportData? GetNext()
        {
            ReportKind kind;
            Span span;
            RustStringHandle msg;
            int ret;
            lock (this.native)
            {
                ret = kailua_report_get_next(this.native, out kind, out span, out msg);
            }
            if (ret < 0)
            {
                throw new NativeException("internal error while getting a report");
            }
            else if (ret == 0)
            {
                return null;
            }
            else
            {
                return new ReportData(kind, span, msg.String);
            }
        }

        public IEnumerator<ReportData> GetEnumerator()
        {
            while (true)
            {
                var data = this.GetNext();
                if (data == null)
                {
                    yield break;
                }
                yield return data.Value;
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
