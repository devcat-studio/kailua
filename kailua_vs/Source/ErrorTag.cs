using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;

namespace Kailua
{
    [Export(typeof(ITaggerProvider))]
    [ContentType("kailua")]
    [TagType(typeof(ErrorTag))]
    internal sealed class ErrorTagProvider : ITaggerProvider
    {
        public ITagger<T> CreateTagger<T>(ITextBuffer buffer) where T : ITag
        {
            // one TokenTagger instance is unique to given ITextBuffer instance
            // (otherwise TagAggregator is unable to receive an event from it)
            return buffer.Properties.GetOrCreateSingletonProperty(delegate() { return new TokenTagger(buffer); }) as ITagger<T>;
        }
    }
}
