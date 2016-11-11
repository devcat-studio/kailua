using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Linq;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Utilities;
using Kailua.Extensions;
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

        private void completeVariable(
            Project project,
            ProjectFile file,
            SnapshotPoint triggerPoint,
            SnapshotSpan targetSpan,
            IList<CompletionSet> completionSets)
        {
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

            var applicableTo = triggerPoint.Snapshot.CreateTrackingSpan(targetSpan, SpanTrackingMode.EdgeInclusive);
            completionSets.Add(new CompletionSet("All", "All", applicableTo, completions, Enumerable.Empty<Completion>()));
        }

        private void completeField(
            Project project,
            ProjectFile file,
            SnapshotPoint triggerPoint,
            TokenList tokenList,
            int sepTokenIndex,
            SnapshotSpan targetSpan,
            IList<CompletionSet> completionSets)
        {
            // TODO we should really distinguish desugared subexpression from normal prefix expression,
            // but for now we use the most common criterion to distinguish them.
            // one of known problems in this approach is that `x"":foo()` etc wouldn't work.
            SnapshotPoint? prefixExprEnd = null;
            for (int i = sepTokenIndex - 1; i >= 0; --i)
            {
                var prevToken = tokenList.WithSnapshot[i];
                if (prevToken.Type == Native.TokenType.RParen || prevToken.Type == Native.TokenType.Name)
                {
                    prefixExprEnd = prevToken.Span.End;
                    break;
                }
                else if (prevToken.Type != Native.TokenType.Comment)
                {
                    // skip comments to be sure
                    break;
                }
            }

            if (!prefixExprEnd.HasValue)
            {
                return;
            }

            // grab the last valid checker output, which may not be current but can be useful for completion
            var output = project.LastValidCheckerOutput;
            if (output == null)
            {
                return;
            }

            // the trigger point might not be in the correct span
            var translatedPrefixExprEnd = prefixExprEnd.Value.TranslateTo(file.SourceSnapshot, PointTrackingMode.Positive);
            var pos = new Native.Pos(file.Unit, (uint)translatedPrefixExprEnd.Position);
            var completions = new List<Completion>();
            var names = new SortedSet<Native.NameEntry>(output.FieldsAfter(pos), new NameEntryComparer());
            foreach (var entry in names)
            {
                var name = entry.Name;
                completions.Add(new Completion(name + Properties.Strings.FieldNameSuffix, name, null, null, null));
            }

            var applicableTo = triggerPoint.Snapshot.CreateTrackingSpan(targetSpan, SpanTrackingMode.EdgeInclusive);
            completionSets.Add(new CompletionSet("All", "All", applicableTo, completions, Enumerable.Empty<Completion>()));
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

            TokenList tokenList;
            try
            {
                tokenList = file.TokenListTask.Result;
            }
            catch (AggregateException)
            {
                return;
            }

            // we have two modes of autocompletion right now:
            // 1. variable completion, enabled when the identifier is being typed
            // 2. field completion, enabled when `.` or `:` is typed after a prefix expression
            //
            // it should be noted that the completion source sees the source code after the keystroke,
            // so a newly typed letter is already in the code.

            int index;
            var mode = tokenList.IntersectionWith(triggerPoint, out index);
            var token = tokenList.WithSnapshot[index];

            if (mode.HasEnd() && (token.Type == Native.TokenType.Dot || token.Type == Native.TokenType.Colon))
            {
                // ... `.` | ...
                // ... `:` | ...
                // possibly mode 2

                var targetSpan = new SnapshotSpan(triggerPoint, triggerPoint);
                this.completeField(project, file, triggerPoint, tokenList, index, targetSpan, completionSets);
            }
            else if (mode.IsAfter() && (token.Type.IsKeyword() || token.Type == Native.TokenType.Name))
            {
                // ... NAM|E ...
                // ... NAME | ...
                // mode 2 if preceded by `.` or `:`, mode 1 otherwise

                var dotOrColon = false;
                if (index > 0)
                {
                    var prevToken = tokenList.WithSnapshot[index - 1];
                    if (prevToken.Type == Native.TokenType.Colon || prevToken.Type == Native.TokenType.Dot)
                    {
                        dotOrColon = true;
                    }
                }

                if (dotOrColon)
                {
                    this.completeField(project, file, triggerPoint, tokenList, index, token.Span, completionSets);
                }
                else
                {
                    this.completeVariable(project, file, triggerPoint, token.Span, completionSets);
                }
            }
        }

        public void Dispose()
        {
            this.disposed = true;
        }
    }
}
