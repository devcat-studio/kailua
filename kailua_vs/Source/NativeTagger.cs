using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Text.Adornments;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Utilities;

namespace Kailua
{
    [Export(typeof(ITaggerProvider))]
    [ContentType("kailua")]
    [TagType(typeof(TokenTag))]
    [TagType(typeof(ReportTag))]
    internal sealed class TokenTagProvider : ITaggerProvider
    {
        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            // one TokenTagger instance is unique to given ITextBuffer instance
            // (otherwise TagAggregator is unable to receive an event from it)
            return buffer.Properties.GetOrCreateSingletonProperty(delegate() { return new NativeTagger(buffer); }) as ITagger<T>;
        }
    }

    public class TokenTag : ITag 
    {
        public Native.TokenType Type { get; private set; }

        public TokenTag(Native.TokenType type)
        {
            this.Type = type;
        }
    }

    public class ReportTag : ITag
    {
        public Native.ReportData Data { get; private set; }
        public String Path { get; private set; }

        public ReportTag(Native.ReportData data, String path)
        {
            this.Data = data;
            this.Path = path;
        }
    }

    internal sealed class NativeTagger : ITagger<TokenTag>, ITagger<ReportTag>
    {
        ITextBuffer buffer;
        ITextSnapshot lastSnapshot;
        List<TagSpan<TokenTag>> lastTokens;
        List<TagSpan<ReportTag>> lastReports;
        object updateLock = new object();

        internal NativeTagger(ITextBuffer buffer)
        {
            this.buffer = buffer;
            this.lastSnapshot = null;
            this.lastTokens = null;
            this.lastReports = null;
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        internal bool calculateTags(ITextSnapshot snapshot, out List<TagSpan<TokenTag>> tokens, out List<TagSpan<ReportTag>> reports)
        {
            lock (this.updateLock)
            {
                if (this.lastSnapshot != null && this.lastSnapshot == snapshot)
                {
                    tokens = this.lastTokens;
                    reports = this.lastReports;
                    return false;
                }

                // do not update the existing last tokens & reports, it may be used elsewhere
                tokens = new List<TagSpan<TokenTag>>();
                reports = new List<TagSpan<ReportTag>>();

                var sourceText = snapshot.GetText();

                var sourcePath = "<unsaved>";
                ITextDocument document;
                if (this.buffer.Properties.TryGetProperty(typeof(ITextDocument), out document))
                {
                    sourcePath = document.FilePath;
                }

                var source = new Native.Source();
                var report = new Native.Report();
                var nativeSpan = source.AddString(sourcePath, sourceText);
                if (nativeSpan.IsValid)
                {
                    var stream = new Native.TokenStream(source, nativeSpan, report);
                    foreach (Native.TokenTypeAndSpan token in stream)
                    {
                        var span = token.Span.AttachSnapshot(snapshot);
                        tokens.Add(new TagSpan<TokenTag>(span, new TokenTag(token.Type)));
                    }

                    try
                    {
                        var tree = new Native.ParseTree(stream, report);
                        // for now we discard tree immediately, probably we can use it later
                    }
                    catch (Native.NativeException _)
                    {
                        // ignore any error here
                    }
                }

                // grab all reported errors
                while (true)
                {
                    var data = report.GetNext();
                    if (data == null)
                    {
                        break;
                    }

                    // for now, ignore unspanned reports (TODO)
                    if (!data.Value.Span.IsValid)
                    {
                        break;
                    }

                    var span = data.Value.Span.AttachSnapshotNonEmpty(snapshot);
                    var path = data.Value.Span.IsValid ? sourcePath : null;
                    reports.Add(new TagSpan<ReportTag>(span, new ReportTag(data.Value, path)));
                }

                // invalidate the entire buffer
                invalidate(new SnapshotSpan(snapshot, 0, snapshot.Length));

                this.lastSnapshot = snapshot;
                this.lastTokens = tokens;
                this.lastReports = reports;
                return true;
            }
        }

        internal void invalidate(SnapshotSpan span)
        {
            if (this.TagsChanged != null)
            {
                this.TagsChanged(this, new SnapshotSpanEventArgs(span));
            }
        }

        IEnumerable<ITagSpan<TokenTag>> ITagger<TokenTag>.GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (spans.Count <= 0)
            {
                yield break;
            }

            var snapshot = this.buffer.CurrentSnapshot;
            Debug.Assert(snapshot == spans[0].Snapshot);

            List<TagSpan<TokenTag>> tokens;
            List<TagSpan<ReportTag>> reports;
            var tagsUpdated = calculateTags(snapshot, out tokens, out reports);

            // TODO think about optimizing O(n) traversal
            foreach (var tag in tokens)
            {
                if (spans.IntersectsWith(tag.Span))
                {
                    yield return tag;
                }
            }
        }

        IEnumerable<ITagSpan<ReportTag>> ITagger<ReportTag>.GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (spans.Count <= 0)
            {
                yield break;
            }

            var snapshot = this.buffer.CurrentSnapshot;
            // XXX not sure if snapshot == spans[i].Snapshot?

            List<TagSpan<TokenTag>> tokens;
            List<TagSpan<ReportTag>> reports;
            var tagsUpdated = calculateTags(snapshot, out tokens, out reports);

            // TODO think about optimizing O(n) traversal
            foreach (var tag in reports)
            {
                if (spans.IntersectsWith(tag.Span))
                {
                    yield return tag;
                }
            }
        }
    }
}
