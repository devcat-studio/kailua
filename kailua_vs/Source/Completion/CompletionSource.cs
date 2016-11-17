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
        // see http://glyphlist.azurewebsites.net/standardglyphgroup/ for the list of icons
        [Import]
        IGlyphService GlyphService { get; set; }

        public ICompletionSource TryCreateCompletionSource(ITextBuffer textBuffer)
        {
            return new CompletionSource(textBuffer, GlyphService);
        }
    }

    class CompletionSource : ICompletionSource
    {
        private ITextBuffer buffer;
        private IGlyphService glyphService;
        private bool disposed = false;

        public CompletionSource(ITextBuffer buffer, IGlyphService glyphService)
        {
            this.buffer = buffer;
            this.glyphService = glyphService;
        }

        private IEnumerable<Completion> completeVariableAndKeyword(
            Project project,
            ProjectFile file,
            SnapshotPoint triggerPoint)
        {
            // grab the last valid parse tree, which may not be current but can be useful for completion
            var tree = file.LastValidParseTree;
            if (tree == null)
            {
                yield break;
            }

            // the trigger point might not be in the correct span
            var translatedTriggerPoint = triggerPoint.TranslateTo(file.SourceSnapshot, PointTrackingMode.Positive);
            var pos = new Native.Pos(file.Unit, (uint)translatedTriggerPoint.Position);

            var localNameIcon = this.glyphService.GetGlyph(StandardGlyphGroup.GlyphGroupVariable, StandardGlyphItem.GlyphItemPrivate);
            var names = new HashSet<Native.NameEntry>(tree.NamesAt(pos));
            foreach (var entry in names)
            {
                var name = entry.Name;
                yield return new Completion(name, name, null, localNameIcon, null);
            }

            var globalNameIcon = this.glyphService.GetGlyph(StandardGlyphGroup.GlyphGroupVariable, StandardGlyphItem.GlyphItemPublic);
            var globalNames = new HashSet<string>(project.GlobalScope);
            globalNames.ExceptWith(from entry in names select entry.Name);
            foreach (var name in project.GlobalScope)
            {
                yield return new Completion(name, name, null, globalNameIcon, null);
            }

            // TODO contextual keywords
            var keywords = new string[]
            {
                "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if", "in",
                "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"
            };
            var keywordIcon = this.glyphService.GetGlyph(StandardGlyphGroup.GlyphKeyword, StandardGlyphItem.GlyphItemPublic);
            foreach (var keyword in keywords)
            {
                yield return new Completion(keyword, keyword, null, keywordIcon, null);
            }
        }

        private IEnumerable<Completion> completeField(
            Project project,
            ProjectFile file,
            SnapshotPoint triggerPoint,
            TokenList tokenList,
            int sepTokenIndex)
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
                yield break;
            }

            // grab the last valid checker output, which may not be current but can be useful for completion
            var output = project.LastValidCheckerOutput;
            if (output == null)
            {
                yield break;
            }

            // the trigger point might not be in the correct span
            var translatedPrefixExprEnd = prefixExprEnd.Value.TranslateTo(file.SourceSnapshot, PointTrackingMode.Positive);
            var pos = new Native.Pos(file.Unit, (uint)translatedPrefixExprEnd.Position);

            var fieldNameIcon = this.glyphService.GetGlyph(StandardGlyphGroup.GlyphGroupProperty, StandardGlyphItem.GlyphItemPublic);
            foreach (var entry in output.FieldsAfter(pos))
            {
                var name = entry.Name;
                yield return new Completion(name, name, null, fieldNameIcon, null);
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

            SnapshotSpan targetSpan;
            IEnumerable<Completion> completions;
            if (mode.HasEnd() && (token.Type == Native.TokenType.Dot || token.Type == Native.TokenType.Colon))
            {
                // ... `.` | ...
                // ... `:` | ...
                // possibly mode 2

                targetSpan = new SnapshotSpan(triggerPoint, triggerPoint);
                completions = this.completeField(project, file, triggerPoint, tokenList, index);
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

                targetSpan = token.Span;
                if (dotOrColon)
                {
                    completions = this.completeField(project, file, triggerPoint, tokenList, index);
                }
                else
                {
                    completions = this.completeVariableAndKeyword(project, file, triggerPoint);
                }
            }
            else
            {
                return;
            }

            var applicableTo = triggerPoint.Snapshot.CreateTrackingSpan(targetSpan, SpanTrackingMode.EdgeInclusive);
            var sortedCompletions = completions.OrderBy(c => c.InsertionText).ToList();
            completionSets.Add(new CompletionSet("All", "All", applicableTo, sortedCompletions, Enumerable.Empty<Completion>()));
        }

        public void Dispose()
        {
            this.disposed = true;
        }
    }
}
