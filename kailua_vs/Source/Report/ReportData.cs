using System;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Globalization;
using Microsoft.VisualStudio.Text;
using Kailua.Util.Extensions;

namespace Kailua
{
    // unlike Native.ReportData, this contains all necessary information to display the report.
    // this is necessary to resolve the errors even _after_ the originating span or buffer
    // becomes unavailable.
    public class ReportData
    {
        public Native.ReportKind Kind { get; private set; }
        public Native.Span NativeSpan { get; private set; }
        public String Message { get; private set; }

        // these fields are populated at the time of initialization if possible
        public String FileName { get; private set; }
        public int Line { get; private set; } // 0 for "unknown"
        public int Column { get; private set; } // 0 for "unknown"

        private void populateFileName(ITextSnapshot snapshot)
        {
            if (snapshot == null)
            {
                throw new ArgumentNullException("snapshot");
            }

            this.FileName = snapshot.TextBuffer.GetFilePath();
        }

        private void populateLineColumn(Native.Source source)
        {
            if (source == null)
            {
                throw new ArgumentNullException("source");
            }

            this.Line = 0;
            this.Column = 0;
            if (this.NativeSpan.IsValid)
            {
                Native.Span lineSpan;
                var line = source.LineFromPos(this.NativeSpan.Begin, out lineSpan);
                if (line > 0)
                {
                    this.Line = line;
                    this.Column = this.NativeSpan.Begin.Offset - lineSpan.Begin.Offset + 1;
                }
            }
        }

        // optional, used to convert Native.Span to SnapshotSpan
        // this should be a weak reference, as ReportData can be alive for very long time
        private WeakReference<ITextSnapshot> snapshot;

        public ITextSnapshot Snapshot
        {
            get
            {
                ITextSnapshot snapshot;
                if (this.snapshot != null && this.snapshot.TryGetTarget(out snapshot))
                {
                    return snapshot;
                }
                else
                {
                    return null;
                }
            }

            private set
            {
                if (value != null)
                {
                    this.snapshot = new WeakReference<ITextSnapshot>(value);
                }
                else
                {
                    this.snapshot = null;
                }
            }
        }

        public SnapshotSpan? SnapshotSpan
        {
            get
            {
                var snapshot = this.Snapshot;
                if (snapshot != null && this.NativeSpan.IsValid)
                {
                    return this.NativeSpan.AttachSnapshot(snapshot);
                }
                else
                {
                    return null;
                }
            }
        }

        public SnapshotSpan? SnapshotSpanNonEmpty
        {
            get
            {
                var snapshot = this.Snapshot;
                if (snapshot != null && this.NativeSpan.IsValid)
                {
                    return this.NativeSpan.AttachSnapshotNonEmpty(snapshot);
                }
                else
                {
                    return null;
                }
            }
        }

        public ReportData(Native.ReportKind kind, String msg)
        {
            this.Kind = kind;
            this.NativeSpan = Native.Span.Dummy;
            this.Message = msg;
        }

        public ReportData(Native.ReportKind kind, Native.Span span, String msg)
        {
            this.Kind = kind;
            this.NativeSpan = span;
            this.Message = msg;
        }

        public ReportData(Native.ReportKind kind, String fileName, String msg)
            : this(kind, msg)
        {
            this.FileName = fileName;
        }

        public ReportData(Native.ReportKind kind, String fileName, Native.Span span, String msg)
            : this(kind, span, msg)
        {
            this.FileName = fileName;
        }

        public ReportData(Native.ReportKind kind, String fileName, int line, int column, String msg)
            : this(kind, msg)
        {
            this.FileName = fileName;
            this.Line = line;
            this.Column = column;
        }

        public ReportData(Native.ReportKind kind, Native.Source source, String fileName, Native.Span span, String msg)
            : this(kind, span, msg)
        {
            this.FileName = fileName;
            this.populateLineColumn(source);
        }

        public ReportData(Native.ReportKind kind, Native.Span span, String msg, ITextSnapshot snapshot)
            : this(kind, span, msg)
        {
            this.Snapshot = snapshot;
            if (snapshot != null)
            {
                this.populateFileName(snapshot);
            }
        }

        public ReportData(Native.ReportKind kind, Native.Source source, Native.Span span, String msg, ITextSnapshot snapshot)
            : this(kind, span, msg)
        {
            this.Snapshot = snapshot;
            if (snapshot != null)
            {
                this.populateFileName(snapshot);
            }
            this.populateLineColumn(source);
        }

        public ReportData(Native.ReportData reportData)
            : this(reportData.Kind, reportData.Span, reportData.Message)
        {
        }

        public ReportData(Native.ReportData reportData, ITextSnapshot snapshot)
            : this(reportData.Kind, reportData.Span, reportData.Message, snapshot)
        {
        }

        public ReportData(Native.Source source, String fileName, Native.ReportData reportData)
            : this(reportData.Kind, source, fileName, reportData.Span, reportData.Message)
        {
        }

        public ReportData(Native.Source source, Native.ReportData reportData, ITextSnapshot snapshot)
            : this(reportData.Kind, source, reportData.Span, reportData.Message, snapshot)
        {
        }
    }
}
