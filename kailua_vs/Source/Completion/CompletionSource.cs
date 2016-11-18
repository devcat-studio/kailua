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

        private string[] keywordsForNestingCategory(Native.TokenNestingCategory nestingCategory)
        {
            switch (nestingCategory)
            {
                case Native.TokenNestingCategory.Expr:
                default:
                    return new string[]
                    {
                        "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if",
                        "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
                    };

                case Native.TokenNestingCategory.Meta:
                    return new string[]
                    {
                        "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "goto", "if",
                        "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while",
                        "assume", "const", "global", "map", "module", "once", "open", "type", "var", "vector",
                    };
            }
        }

        // check if the caret is located in regions where the autocompletion should be disabled:
        //
        // 1. `local NAME ... | ... [= ...]`
        // 2. `for NAME ... | ... = ... do ... end`
        // 3. `function NAME ... | ( ... )`
        // 4. `function [NAME ...] ( ... | ... )`
        //
        // the check for 1 and 2 is handled by looking backward for the first token
        // that is not a comment, a name or a comma and is in the same nesting as the caret.
        // if the token exists and it's `local` or `for`, autocompletion is disabled.
        //
        // the check for 3 is handled by looking backward for the first token
        // that is not a comment, a name, a dot or a colon and is in the same nesting as the caret.
        // if the token exists and it's `function`, autocompletion is disabled.
        //
        // the check for 4 is handled similarly to the check for 1 and 2,
        // but once seen a `(` token, it will switch to the check for 3 at the parent nesting.
        //
        // for the interactivity, the lookbehind is limited to a reasonable number.
        private bool isNameCompletionDisabled(TokenList tokenList, int nameTokenIndex)
        {
            const int LOOKBEHIND_LIMIT = 4096;

            var tokenListWithSnapshot = tokenList.WithSnapshot;
            var nameToken = tokenListWithSnapshot[nameTokenIndex];
            var initNesting = nameToken.Nesting;
#if TRACE_COMPLETION
            Log.Write("starting at token {0} {1} - target nesting {2} {3}",
                nameToken.Type, nameToken.Span, initNesting.Depth, initNesting.Serial);
#endif

            var isNameDeclPossible = true; // case 1, 2 and 4a
            var isFuncSigPossible = true; // case 3 and 4b
            var count = 0;
            for (int i = nameTokenIndex - 1;
                 i >= 0 && count < LOOKBEHIND_LIMIT && (isNameDeclPossible || isFuncSigPossible);
                 --i, ++count)
            {
                var token = tokenListWithSnapshot[i];
#if TRACE_COMPLETION
                Log.Write("looking at token {0} {1} (namedecl {2}, sig {3}) - current nesting {4} {5}",
                    token.Type, token.Span, isNameDeclPossible, isFuncSigPossible, token.Nesting.Depth, token.Nesting.Serial);
#endif
                if (token.Nesting.Depth <= initNesting.Depth && token.Nesting.Serial != initNesting.Serial)
                {
                    // escaped the current nesting, stop the search
                    return false;
                }
                else if (token.Nesting.Depth > initNesting.Depth)
                {
                    // ignore more nested tokens (but count them towards the threshold)
                    continue;
                }

                // isNameDeclPossible can continue to isFuncSigPossible in place, so this should be first
                if (isFuncSigPossible)
                {
                    switch (token.Type)
                    {
                        case Native.TokenType.Comment:
                        case Native.TokenType.Name:
                        case Native.TokenType.Dot:
                        case Native.TokenType.Colon:
                            break;

                        case Native.TokenType.Function:
                            return true;

                        default:
                            isFuncSigPossible = false;
                            break;
                    }
                }

                if (isNameDeclPossible)
                {
                    switch (token.Type)
                    {
                        case Native.TokenType.Comment:
                        case Native.TokenType.Name:
                        case Native.TokenType.Comma:
                        case Native.TokenType.Newline: // to account for meta comments (other tokens are nested)
                            break;

                        case Native.TokenType.LParen:
                            // `function ... ( ... | ... )` is possible
                            // update the initial nesting to point to a token before `(` and proceed
                            if (i == 0)
                            {
                                return false;
                            }
                            initNesting = tokenListWithSnapshot[i - 1].Nesting;
                            isNameDeclPossible = false;
                            isFuncSigPossible = true;
                            break;

                        case Native.TokenType.Local:
                        case Native.TokenType.For:
                            return true;

                        default:
                            isNameDeclPossible = false;
                            break;
                    }
                }
            }

            return false;
        }
        
        private IEnumerable<Completion> completeVariableAndKeyword(
            Project project,
            ProjectFile file,
            SnapshotPoint triggerPoint,
            TokenList tokenList,
            int nameTokenIndex,
            Native.TokenNestingCategory nestingCategory)
        {
            // check if the caret is at the name definition and autocompletion should be disabled
            if (nestingCategory == Native.TokenNestingCategory.Expr && this.isNameCompletionDisabled(tokenList, nameTokenIndex))
            {
                yield break;
            }

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

            var keywordIcon = this.glyphService.GetGlyph(StandardGlyphGroup.GlyphKeyword, StandardGlyphItem.GlyphItemPublic);
            foreach (var keyword in this.keywordsForNestingCategory(nestingCategory))
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
            // note that this approach of using the closest non-comment token's end is prone to syntax error;
            // while completing `f"":` should work, `"":` (invalid, should have been `(""):`) will also work.
            // as such a mistake can be readily reported, however, we don't try to perfect the approach.
            SnapshotPoint? prefixExprEnd = null;
            for (int i = sepTokenIndex - 1; i >= 0; --i)
            {
                var prevToken = tokenList.WithSnapshot[i];
                if (prevToken.Type != Native.TokenType.Comment)
                {
                    prefixExprEnd = prevToken.Span.End;
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
                    // due to mode.IsAfter() condition, the token used for the nesting category is correct
                    completions = this.completeVariableAndKeyword(project, file, triggerPoint, tokenList, index, token.Nesting.Category);
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
