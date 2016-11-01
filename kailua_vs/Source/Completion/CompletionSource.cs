using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;
using Kailua.Util.Extensions;
using Kailua.Native.Extensions;

namespace Kailua
{
    [Export(typeof(ICompletionSourceProvider))]
    [ContentType("kailua")]
    [Name("kailuaCompletion")]
    class CompletionSourceProvider : ICompletionSourceProvider
    {
        public ICompletionSource TryCreateCompletionSource(ITextBuffer textBuffer)
        {
            return new CompletionSource(textBuffer);
        }
    }

    class CompletionSource : ICompletionSource
    {
        private ITextBuffer buffer;
        private bool disposed = false;

        public CompletionSource(ITextBuffer buffer)
        {
            this.buffer = buffer;
        }

        private class NameComparer : IComparer<string>
        {
            public int Compare(string x, string y)
            {
                return x.ToUpperInvariant().CompareTo(y.ToUpperInvariant());
            }
        }

        private class NameEntryComparer : IComparer<Native.NameEntry>
        {
            public int Compare(Native.NameEntry x, Native.NameEntry y)
            {
                return x.Name.ToUpperInvariant().CompareTo(y.Name.ToUpperInvariant());
            }
        }

        public void AugmentCompletionSession(ICompletionSession session, IList<CompletionSet> completionSets)
        {
            if (this.disposed)
            {
                throw new ObjectDisposedException("CompletionSource");
            }

            ITextSnapshot snapshot = this.buffer.CurrentSnapshot;
            var triggerPoint = (SnapshotPoint)session.GetTriggerPoint(snapshot);
            if (triggerPoint == null)
            {
                return;
            }

            string sourcePath = this.buffer.GetFilePath();
            if (sourcePath == null)
            {
                return;
            }
            var project = ProjectCache.GetAnyProject(sourcePath);
            if (project == null)
            {
                return;
            }

            var file = project[sourcePath];

            // try to filter inappropriate cases to complete the word
            SnapshotSpan targetSpan;
            try
            {
                var tokenList = file.TokenListTask.Result;

                // locate the token which contains the trigger point or ends at that point
                // if there is no such token, the trigger point is after one or more whitespace and should be dismissed
                int index;
                switch (tokenList.IntersectionWith(triggerPoint, out index))
                {
                    case IntersectionMode.End:
                    case IntersectionMode.Inside:
                    case IntersectionMode.Both:
                        break;
                    case IntersectionMode.None:
                    case IntersectionMode.Begin:
                        return;
                }

                // that token should be a keyword or a name
                var token = tokenList.WithSnapshot[index];
                if (!(token.Type.IsKeyword() || token.Type == Native.TokenType.Name))
                {
                    return;
                }

                // and should not be preceded by `:` or `.` (for now)
                if (index > 0)
                {
                    var prevToken = tokenList.WithSnapshot[index - 1];
                    if (prevToken.Type == Native.TokenType.Colon || prevToken.Type == Native.TokenType.Dot)
                    {
                        return;
                    }
                }

                targetSpan = token.Span;
            }
            catch (AggregateException)
            {
                return;
            }

            // grab the last valid parse tree, which may not be current but can be useful for completion
            var tree = file.LastValidParseTree;
            if (tree == null)
            {
                return;
            }

            // the trigger point might not be in the correct span
            var translatedTriggerPoint = triggerPoint.TranslateTo(file.SourceSnapshot, PointTrackingMode.Positive);
            var pos = new Native.Pos(file.Unit, (uint)translatedTriggerPoint.Position);
            var completions = new List<Completion>();
            var names = new SortedSet<Native.NameEntry>(tree.NamesAt(pos), new NameEntryComparer());
            foreach (var entry in names)
            {
                var name = entry.Name;
                completions.Add(new Completion(name + Properties.Strings.LocalNameSuffix, name, null, null, null));
            }
            var globalNames = new SortedSet<string>(project.GlobalScope, new NameComparer());
            globalNames.ExceptWith(from entry in names select entry.Name);
            foreach (var name in project.GlobalScope)
            {
                completions.Add(new Completion(name + Properties.Strings.GlobalNameSuffix, name, null, null, null));
            }

            var applicableTo = snapshot.CreateTrackingSpan(targetSpan, SpanTrackingMode.EdgeInclusive);
            completionSets.Add(new CompletionSet("All", "All", applicableTo, completions, Enumerable.Empty<Completion>()));
        }

        public void Dispose()
        {
            this.disposed = true;
        }
    }
}
