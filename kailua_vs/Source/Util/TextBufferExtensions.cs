using System;
using Microsoft.VisualStudio.Text;

namespace Kailua.Util.Extensions
{
    public static class TextBufferExtensions
    {
        public static String GetFilePath(this ITextBuffer buffer)
        {
            ITextDocument document;
            if (buffer.Properties.TryGetProperty(typeof(ITextDocument), out document))
            {
                return document.FilePath;
            }
            else
            {
                return null;
            }
        }
    }
}
