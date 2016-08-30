using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using Microsoft.VisualStudio.Language.StandardClassification;

namespace Kailua
{
    internal static class OrdinaryClassificationDefinition
    {
        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ClassificationTypeName.Comment)]
        [BaseDefinition(PredefinedClassificationTypeNames.Comment)]
        internal static ClassificationTypeDefinition commentDef = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ClassificationTypeName.Keyword)]
        [BaseDefinition(PredefinedClassificationTypeNames.Keyword)]
        internal static ClassificationTypeDefinition keywordDef = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ClassificationTypeName.Punct)]
        [BaseDefinition(PredefinedClassificationTypeNames.Operator)]
        internal static ClassificationTypeDefinition punctDef = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ClassificationTypeName.Meta)]
        [BaseDefinition(PredefinedClassificationTypeNames.PreprocessorKeyword)]
        internal static ClassificationTypeDefinition metaDef = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ClassificationTypeName.Symbol)]
        [BaseDefinition(PredefinedClassificationTypeNames.Literal)]
        internal static ClassificationTypeDefinition symbolDef = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ClassificationTypeName.Number)]
        [BaseDefinition(PredefinedClassificationTypeNames.Number)]
        internal static ClassificationTypeDefinition numberDef = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ClassificationTypeName.String)]
        [BaseDefinition(PredefinedClassificationTypeNames.String)]
        internal static ClassificationTypeDefinition stringDef = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ClassificationTypeName.Local)]
        [BaseDefinition(PredefinedClassificationTypeNames.Identifier)]
        internal static ClassificationTypeDefinition localNameDef = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name(ClassificationTypeName.Global)]
        [BaseDefinition(PredefinedClassificationTypeNames.Identifier)]
        internal static ClassificationTypeDefinition globalNameDef = null;
    }
}
