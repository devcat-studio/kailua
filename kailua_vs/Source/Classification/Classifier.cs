using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using Kailua.Native.Extensions;

namespace Kailua
{
    [Export(typeof(ITaggerProvider))]
    [TagType(typeof(ClassificationTag))]
    [ContentType("kailua")]
    internal sealed class ClassifierProvider : ITaggerProvider
    {
        [Export]
        [Name("kailua")]
        [BaseDefinition("code")]
        internal static ContentTypeDefinition contentType = null;

        [Export]
        [FileExtension(".lua")]
        [ContentType("kailua")]
        internal static FileExtensionToContentTypeDefinition luaFileType = null;

        [Export]
        [FileExtension(".kailua")]
        [ContentType("kailua")]
        internal static FileExtensionToContentTypeDefinition kailuaFileType = null;

        [Import]
        internal IClassificationTypeRegistryService classificationTypeRegistry = null;

        [Import]
        internal IBufferTagAggregatorFactoryService aggregatorFactory = null;

        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            ITagAggregator<TokenTag> tagAggregator = aggregatorFactory.CreateTagAggregator<TokenTag>(buffer);
            return new Classifier(buffer, tagAggregator, classificationTypeRegistry) as ITagger<T>;
        }
    }

    internal sealed class Classifier : ITagger<ClassificationTag>
    {
        internal ITextBuffer buffer;
        internal ITagAggregator<TokenTag> aggregator;

        internal IClassificationType commentClassification;
        internal IClassificationType keywordClassification;
        internal IClassificationType punctClassification;
        internal IClassificationType metaClassification;
        internal IClassificationType symbolClassification;
        internal IClassificationType numberClassification;
        internal IClassificationType stringClassification;
        internal IClassificationType localClassification;
        internal IClassificationType globalClassification;

        internal Classifier(ITextBuffer buffer, 
                            ITagAggregator<TokenTag> tagAggregator, 
                            IClassificationTypeRegistryService typeService)
        {
            this.buffer = buffer;
            this.aggregator = tagAggregator;

            this.commentClassification = typeService.GetClassificationType(ClassificationTypeName.Comment);
            this.keywordClassification = typeService.GetClassificationType(ClassificationTypeName.Keyword);
            this.punctClassification = typeService.GetClassificationType(ClassificationTypeName.Punct);
            this.metaClassification = typeService.GetClassificationType(ClassificationTypeName.Meta);
            this.symbolClassification = typeService.GetClassificationType(ClassificationTypeName.Symbol);
            this.numberClassification = typeService.GetClassificationType(ClassificationTypeName.Number);
            this.stringClassification = typeService.GetClassificationType(ClassificationTypeName.String);
            this.localClassification = typeService.GetClassificationType(ClassificationTypeName.Local);
            this.globalClassification = typeService.GetClassificationType(ClassificationTypeName.Global);

            this.aggregator.TagsChanged += tokenTagsChanged;
        }

        internal void tokenTagsChanged(object sender, TagsChangedEventArgs args)
        {
            if (this.TagsChanged != null)
            {
                var spans = args.Span.GetSpans(this.buffer);
                this.TagsChanged(sender, new SnapshotSpanEventArgs(spans[0]));
            }
        }

        internal IClassificationType tokenTypeToClassification(Native.TokenType tokenType)
        {
            switch (tokenType)
            {
                case Native.TokenType.Comment:
                    return this.commentClassification;

                case Native.TokenType.Num:
                    return this.numberClassification;

                case Native.TokenType.Str:
                    return this.stringClassification;

                case Native.TokenType.Name:
                    return this.localClassification; // XXX for now

                case Native.TokenType.True:
                case Native.TokenType.False:
                case Native.TokenType.Nil:
                    return this.symbolClassification;

                case Native.TokenType.DashDashHash:
                case Native.TokenType.DashDashV:
                case Native.TokenType.DashDashColon:
                case Native.TokenType.DashDashGt:
                    return this.metaClassification;

                default:
                    if (tokenType.IsPunct())
                    {
                        return this.punctClassification;
                    }
                    else if (tokenType.IsKeyword())
                    {
                        return this.keywordClassification;
                    }
                    else
                    {
                        return null;
                    }
            }
        }

        public event EventHandler<SnapshotSpanEventArgs> TagsChanged;

        internal void invalidate(SnapshotSpan span)
        {
            if (this.TagsChanged != null)
            {
                this.TagsChanged(this, new SnapshotSpanEventArgs(span));
            }
        }

        public IEnumerable<ITagSpan<ClassificationTag>> GetTags(NormalizedSnapshotSpanCollection spans)
        {
            var snapshot = spans[0].Snapshot;

            foreach (var tagSpan in this.aggregator.GetTags(spans))
            {
                var span = tagSpan.Span.GetSpans(snapshot)[0];
                var classification = tokenTypeToClassification(tagSpan.Tag.Type);
                if (classification != null)
                {
                    yield return new TagSpan<ClassificationTag>(span, new ClassificationTag(classification));
                }
            }
        }
    }
}
