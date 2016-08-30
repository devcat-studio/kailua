using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace Kailua
{
    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ClassificationTypeName.Comment)]
    [Name(ClassificationTypeName.Comment)]
    [UserVisible(true)]
    [Order(Before = Priority.Default)]
    internal sealed class KailuaCommentFormat : ClassificationFormatDefinition
    {
        public KailuaCommentFormat()
        {
            this.DisplayName = "Kailua Comment";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ClassificationTypeName.Keyword)]
    [Name(ClassificationTypeName.Keyword)]
    [UserVisible(true)]
    [Order(Before = Priority.Default)]
    internal sealed class KailuaKeywordFormat : ClassificationFormatDefinition
    {
        public KailuaKeywordFormat()
        {
            this.DisplayName = "Kailua Keyword";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ClassificationTypeName.Punct)]
    [Name(ClassificationTypeName.Punct)]
    [UserVisible(true)]
    [Order(Before = Priority.Default)]
    internal sealed class KailuaPunctFormat : ClassificationFormatDefinition
    {
        public KailuaPunctFormat()
        {
            this.DisplayName = "Kailua Punctuations";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ClassificationTypeName.Meta)]
    [Name(ClassificationTypeName.Meta)]
    [UserVisible(true)]
    [Order(Before = Priority.Default)]
    internal sealed class KailuaMetaFormat : ClassificationFormatDefinition
    {
        public KailuaMetaFormat()
        {
            this.DisplayName = "Kailua Meta Declarations";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ClassificationTypeName.Symbol)]
    [Name(ClassificationTypeName.Symbol)]
    [UserVisible(true)]
    [Order(Before = Priority.Default)]
    internal sealed class KailuaSymbolFormat : ClassificationFormatDefinition
    {
        public KailuaSymbolFormat()
        {
            this.DisplayName = "Kailua Symbolic Literals";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ClassificationTypeName.Number)]
    [Name(ClassificationTypeName.Number)]
    [UserVisible(true)]
    [Order(Before = Priority.Default)]
    internal sealed class KailuaNumberFormat : ClassificationFormatDefinition
    {
        public KailuaNumberFormat()
        {
            this.DisplayName = "Kailua Numeric Literals";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ClassificationTypeName.String)]
    [Name(ClassificationTypeName.String)]
    [UserVisible(true)]
    [Order(Before = Priority.Default)]
    internal sealed class KailuaStringFormat : ClassificationFormatDefinition
    {
        public KailuaStringFormat()
        {
            this.DisplayName = "Kailua String Literals";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ClassificationTypeName.Local)]
    [Name(ClassificationTypeName.Local)]
    [UserVisible(true)]
    [Order(Before = Priority.Default)]
    internal sealed class KailuaLocalFormat : ClassificationFormatDefinition
    {
        public KailuaLocalFormat()
        {
            this.DisplayName = "Kailua Local Names";
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = ClassificationTypeName.Global)]
    [Name(ClassificationTypeName.Global)]
    [UserVisible(true)]
    [Order(Before = Priority.Default)]
    internal sealed class KailuaGlobalFormat : ClassificationFormatDefinition
    {
        public KailuaGlobalFormat()
        {
            this.DisplayName = "Kailua Global Names";
        }
    }
}
