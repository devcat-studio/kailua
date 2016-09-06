using System;
using System.Collections.Generic;
using System.Linq;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;

namespace Kailua
{
    [Export(typeof(IQuickInfoSourceProvider))]
    [ContentType("kailua")]
    [Name("kailuaQuickInfo")]
    internal sealed class ErrorQuickInfoSourceProvider : IQuickInfoSourceProvider
    {
        [Import]
        internal IBufferTagAggregatorFactoryService aggregatorFactory = null;

        public IQuickInfoSource TryCreateQuickInfoSource(ITextBuffer buffer)
        {
            var tagAggregator = aggregatorFactory.CreateTagAggregator<ReportTag>(buffer);
            return new ErrorQuickInfoSource(buffer, tagAggregator);
        }
    }

    public class ErrorQuickInfoSource : IQuickInfoSource
    {
        private ITagAggregator<ReportTag> aggregator;
        private ITextBuffer buffer;
        private bool disposed = false;

        public ErrorQuickInfoSource(ITextBuffer buffer, ITagAggregator<ReportTag> aggregator)
        {
            this.buffer = buffer;
            this.aggregator = aggregator;
        }

        public void AugmentQuickInfoSession(IQuickInfoSession session, IList<object> quickInfoContent, out ITrackingSpan applicableToSpan)
        {
            applicableToSpan = null;
            quickInfoContent.Clear();

            if (this.disposed)
            {
                throw new ObjectDisposedException("ErrorQuickInfoSource");
            }

            var triggerPoint = (SnapshotPoint)session.GetTriggerPoint(this.buffer.CurrentSnapshot);
            if (triggerPoint == null)
            {
                return;
            }

            var veryEnd = triggerPoint.Position == triggerPoint.Snapshot.Length;
            var querySpan = new SnapshotSpan(triggerPoint, veryEnd ? 0 : 1);

            var tags = aggregator.GetTags(querySpan);
            foreach (var tag in tags)
            {
                // filter if the tag just shares the end points with the query
                var tagSpan = tag.Span.GetSpans(this.buffer).First();
                if (!querySpan.OverlapsWith(tagSpan))
                {
                    continue;
                }

                // only use the first error tag detected
                applicableToSpan = this.buffer.CurrentSnapshot.CreateTrackingSpan(tagSpan, SpanTrackingMode.EdgeExclusive);
                quickInfoContent.Add(tag.Tag.Data.Message);
                break;
            }
        }

        public void Dispose()
        {
            this.disposed = true;
        }
    }
}
