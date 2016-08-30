namespace Kailua
{
    using System;
    using System.Collections.Generic;
    using System.ComponentModel.Composition;
    using Microsoft.VisualStudio.Text;
    using Microsoft.VisualStudio.Text.Tagging;
    using Microsoft.VisualStudio.Text.Editor;
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

    internal sealed class TokenTagger : ITagger<TokenTag>
    {
        ITextBuffer buffer;
        ITextSnapshot lastSnapshot;
        List<TagSpan<TokenTag>> lastTags;
        object updateLock = new object();

        internal TokenTagger(ITextBuffer buffer)
        {
            this.buffer = buffer;
            this.lastSnapshot = null;
            this.lastTags = null;
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        internal bool calculateTags(ITextSnapshot snapshot, out List<TagSpan<TokenTag>> tags)
        {
            lock (this.updateLock)
            {
                if (this.lastSnapshot != null && this.lastSnapshot == snapshot)
                {
                    tags = this.lastTags;
                    return false;
                }

                // do not update the existing this.lastTags, it may be used elsewhere
                tags = new List<TagSpan<TokenTag>>();

                var sourceText = snapshot.GetText();

                var source = new Native.Source();
                var nativeSpan = source.AddString("<editor>", sourceText);
                if (nativeSpan.IsValid)
                {
                    var stream = new Native.TokenStream(source, nativeSpan);
                    foreach (Native.TokenTypeAndSpan token in stream)
                    {
                        var begin = token.Span.Begin.Offset;
                        var end = token.Span.End.Offset;
                        var span = new SnapshotSpan(snapshot, begin, end - begin);
                        tags.Add(new TagSpan<TokenTag>(span, new TokenTag(token.Type)));
                    }
                }

                // for now, invalidate the entire buffer
                invalidate(new SnapshotSpan(snapshot, 0, snapshot.Length));

                this.lastSnapshot = snapshot;
                this.lastTags = tags;
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

        public IEnumerable<ITagSpan<TokenTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            if (spans.Count <= 0)
            {
                yield break;
            }

            var snapshot = this.buffer.CurrentSnapshot;
            // XXX not sure if snapshot == spans[i].Snapshot?
            List<TagSpan<TokenTag>> tags;
            var tagsUpdated = calculateTags(snapshot, out tags);

            // TODO think about optimizing O(n) traversal
            foreach (var tag in tags)
            {
                if (spans.OverlapsWith(tag.Span))
                {
                    yield return tag;
                }
            }
        }
    }
}
