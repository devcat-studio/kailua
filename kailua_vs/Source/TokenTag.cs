namespace Kailua
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.Composition;
    using Microsoft.VisualStudio.Text;
    using Microsoft.VisualStudio.Text.Tagging;
    using Microsoft.VisualStudio.Text.Editor;
    using Microsoft.VisualStudio.Text.Adornments;
    using Microsoft.VisualStudio.Utilities;

    [Export(typeof(ITaggerProvider))]
    [ContentType("kailua")]
    [TagType(typeof(TokenTag))]
    internal sealed class TokenTagProvider : ITaggerProvider
    {
        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            // one TokenTagger instance is unique to given ITextBuffer instance
            // (otherwise TagAggregator is unable to receive an event from it)
            return buffer.Properties.GetOrCreateSingletonProperty(delegate() { return new TokenTagger(buffer); }) as ITagger<T>;
        }
    }

    public class TokenTag : ITag 
    {
        public Native.TokenType type { get; private set; }

        public TokenTag(Native.TokenType type)
        {
            this.type = type;
        }
    }

    internal sealed class TokenTagger : ITagger<TokenTag>, ITagger<ErrorTag>
    {
        ITextBuffer buffer;
        ITextSnapshot lastSnapshot;
        List<TagSpan<TokenTag>> lastTags;
        List<TagSpan<ErrorTag>> lastErrors;
        object updateLock = new object();

        internal TokenTagger(ITextBuffer buffer)
        {
            this.buffer = buffer;
            this.lastSnapshot = null;
            this.lastTags = null;
            this.lastErrors = null;
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        internal String reportKindToErrorType(Native.ReportKind kind)
        {
            switch (kind)
            {
                case Native.ReportKind.Warning:
                    return PredefinedErrorTypeNames.Warning;
                case Native.ReportKind.Error:
                    return PredefinedErrorTypeNames.SyntaxError; // TODO
                case Native.ReportKind.Fatal:
                    return PredefinedErrorTypeNames.SyntaxError; // TODO
                default:
                    return null;
            }
        }

        internal bool calculateTags(ITextSnapshot snapshot, out List<TagSpan<TokenTag>> tags, out List<TagSpan<ErrorTag>> errors)
        {
            lock (this.updateLock)
            {
                if (this.lastSnapshot != null && this.lastSnapshot == snapshot)
                {
                    tags = this.lastTags;
                    errors = this.lastErrors;
                    return false;
                }

                // do not update the existing this.lastTags, it may be used elsewhere
                tags = new List<TagSpan<TokenTag>>();
                errors = new List<TagSpan<ErrorTag>>();

                var sourceText = snapshot.GetText();

                var source = new Native.Source();
                var report = new Native.Report();
                var nativeSpan = source.AddString("<editor>", sourceText);
                if (nativeSpan.IsValid)
                {
                    var stream = new Native.TokenStream(source, nativeSpan, report);
                    foreach (Native.TokenTypeAndSpan token in stream)
                    {
                        var span = token.Span.AttachSnapshot(snapshot);
                        tags.Add(new TagSpan<TokenTag>(span, new TokenTag(token.Type)));
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

                    var errorType = reportKindToErrorType(data.Value.Kind);
                    if (errorType == null)
                    {
                        // skip notes
                        continue;
                    }
                    var span = data.Value.Span.AttachSnapshot(snapshot);
                    if (span.IsEmpty)
                    {
                        // a point span should be converted to something visible in VS
                        var start = span.Start.Position;
                        if (start == snapshot.Length && start > 0)
                        {
                            --start; // do not go past EOF
                        }
                        span = new SnapshotSpan(snapshot, start, 1);
                    }
                    errors.Add(new TagSpan<ErrorTag>(span, new ErrorTag(errorType, data.Value.Message)));
                }

                // for now, invalidate the entire buffer
                invalidate(new SnapshotSpan(snapshot, 0, snapshot.Length));

                this.lastSnapshot = snapshot;
                this.lastTags = tags;
                this.lastErrors = errors;
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
            // XXX not sure if snapshot == spans[i].Snapshot?

            List<TagSpan<TokenTag>> tags;
            List<TagSpan<ErrorTag>> errors;
            var tagsUpdated = calculateTags(snapshot, out tags, out errors);

            // TODO think about optimizing O(n) traversal
            foreach (var tag in tags)
            {
                if (spans.OverlapsWith(tag.Span))
                {
                    yield return tag;
                }
            }
        }

        IEnumerable<ITagSpan<ErrorTag>> ITagger<ErrorTag>.GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (spans.Count <= 0)
            {
                yield break;
            }

            var snapshot = this.buffer.CurrentSnapshot;
            // XXX not sure if snapshot == spans[i].Snapshot?

            List<TagSpan<TokenTag>> tags;
            List<TagSpan<ErrorTag>> errors;
            var tagsUpdated = calculateTags(snapshot, out tags, out errors);

            // TODO think about optimizing O(n) traversal
            foreach (var tag in errors)
            {
                if (spans.OverlapsWith(tag.Span))
                {
                    yield return tag;
                }
            }
        }
    }
}
