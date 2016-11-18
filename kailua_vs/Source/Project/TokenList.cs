using System;
using System.Runtime.InteropServices;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using Microsoft.VisualStudio.Text;

namespace Kailua
{
    public struct TokenTypeAndSnapshotSpan
    {
        public Native.TokenType Type;
        public Native.TokenNesting Nesting;
        public SnapshotSpan Span;

        public TokenTypeAndSnapshotSpan(Native.TokenType type, SnapshotSpan span, Native.TokenNesting nesting)
        {
            this.Type = type;
            this.Span = span;
            this.Nesting = nesting;
        }

        public TokenTypeAndSnapshotSpan(Native.TokenTypeAndSpan token, ITextSnapshot snapshot)
        {
            this.Type = token.Type;
            this.Span = token.Span.AttachSnapshot(snapshot);
            this.Nesting = token.Nesting;
        }
    }

    public enum IntersectionMode
    {
        None,
        Begin,
        End,
        Both,
        Inside,
    }

    namespace Extensions
    {
        public static class IntersectionModeExtension
        {
            public static bool HasBegin(this IntersectionMode mode)
            {
                return (mode == IntersectionMode.Begin || mode == IntersectionMode.Both);
            }

            public static bool HasEnd(this IntersectionMode mode)
            {
                return (mode == IntersectionMode.End || mode == IntersectionMode.Both);
            }

            public static bool IsBefore(this IntersectionMode mode)
            {
                return (mode == IntersectionMode.Begin || mode == IntersectionMode.Both || mode == IntersectionMode.Inside);
            }

            public static bool IsAfter(this IntersectionMode mode)
            {
                return (mode == IntersectionMode.End || mode == IntersectionMode.Both || mode == IntersectionMode.Inside);
            }
        }
    }

    public class TokenList : IList<Native.TokenTypeAndSpan>
    {
        private ITextSnapshot snapshot;
        private List<Native.TokenTypeAndSpan> tokens;

        public TokenList(ITextSnapshot snapshot, Native.TokenStream stream)
        {
            this.snapshot = snapshot;
            this.tokens = stream.ToList();
        }

        public ITextSnapshot Snapshot
        {
            get { return this.snapshot; }
        }

        #region IList<Native.TokenTypeAndSpan> implementations

        public int IndexOf(Native.TokenTypeAndSpan item)
        {
            return this.tokens.IndexOf(item);
        }

        public void Insert(int index, Native.TokenTypeAndSpan item)
        {
            this.tokens.Insert(index, item);
        }

        public void RemoveAt(int index)
        {
            this.tokens.RemoveAt(index);
        }

        public Native.TokenTypeAndSpan this[int index]
        {
            get { return this.tokens[index]; }
            set { this.tokens[index] = value; }
        }

        public void Add(Native.TokenTypeAndSpan item)
        {
            this.tokens.Add(item);
        }

        public void Clear()
        {
            this.tokens.Clear();
        }

        public bool Contains(Native.TokenTypeAndSpan item)
        {
            return this.tokens.Contains(item);
        }

        public void CopyTo(Native.TokenTypeAndSpan[] array, int arrayIndex)
        {
            this.tokens.CopyTo(array, arrayIndex);
        }

        public int Count
        {
            get { return this.tokens.Count; }
        }

        public bool IsReadOnly
        {
            get { return ((ICollection<Native.TokenTypeAndSpan>)this.tokens).IsReadOnly; }
        }

        public bool Remove(Native.TokenTypeAndSpan item)
        {
            return this.tokens.Remove(item);
        }

        public IEnumerator<Native.TokenTypeAndSpan> GetEnumerator()
        {
            return this.tokens.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.tokens.GetEnumerator();
        }

        #endregion

        public class ListWithSnapshot : IEnumerable<TokenTypeAndSnapshotSpan>
        {
            private TokenList parent;

            internal ListWithSnapshot(TokenList parent)
            {
                this.parent = parent;
            }

            public TokenTypeAndSnapshotSpan this[int index]
            {
                get
                {
                    return new TokenTypeAndSnapshotSpan(this.parent.tokens[index], this.parent.snapshot);
                }
            }

            public IEnumerator<TokenTypeAndSnapshotSpan> GetEnumerator()
            {
                foreach (var token in this.parent.tokens)
                {
                    yield return new TokenTypeAndSnapshotSpan(token, this.parent.snapshot);
                }
            }

            IEnumerator IEnumerable.GetEnumerator()
            {
                return this.GetEnumerator();
            }
        }

        public ListWithSnapshot WithSnapshot
        {
            get { return new ListWithSnapshot(this); }
        }

        private class SearchComparer : IComparer<Native.TokenTypeAndSpan>
        {
            public int Compare(Native.TokenTypeAndSpan x, Native.TokenTypeAndSpan y)
            {
                return x.Span.Begin.Offset.CompareTo(y.Span.Begin.Offset);
            }
        }

        // IntersectionWith methods will search a particular position through the sorted,
        // non-overlapping (but possibly neighboring) list of spanned tokens.
        //
        // the index indicates that the position is "inside" that token, is the "begin"ning of that token,
        // is the "end" of that token, is "both" the beginning of that token and the end of the next token,
        // or is overlapping with "none" but the position is located between two tokens where the index
        // points to the later token, respectively.

        private IntersectionMode doIntersectionWith(Native.Pos pos, out int index)
        {
            var needle = new Native.TokenTypeAndSpan(Native.TokenType.Error, new Native.Span(pos), new Native.TokenNesting());
            var i = this.tokens.BinarySearch(needle, new SearchComparer());
            if (i < 0)
            {
                index = ~i;
                // now tokens[index-1].begin < pos < tokens[index].begin
                // and        tokens[index-1].end <= tokens[index].begin
                if (index == 0)
                {
                    return IntersectionMode.None;
                }
                var prevEnd = this.tokens[index - 1].Span.End;
                if (prevEnd.Offset < pos.Offset)
                {
                    // tokens[index-1].end < pos < tokens[index].begin
                    return IntersectionMode.None;
                }
                else if (prevEnd.Offset == pos.Offset)
                {
                    // tokens[index-1].end = pos < tokens[index].begin
                    --index;
                    return IntersectionMode.End;
                }
                else
                {
                    // tokens[index-1].begin < pos < tokens[index-1].end < tokens[index].begin
                    --index;
                    return IntersectionMode.Inside;
                }
            }
            else
            {
                index = i;
                // now tokens[index-1].end <= pos = tokens[index].begin
                if (index == 0)
                {
                    return IntersectionMode.Begin;
                }
                var prevEnd = this.tokens[index - 1].Span.End;
                if (prevEnd.Offset < pos.Offset)
                {
                    // tokens[index-1].end < pos = tokens[index].begin
                    return IntersectionMode.Begin;
                }
                else
                {
                    // tokens[index-1].end = pos = tokens[index].begin
                    --index;
                    return IntersectionMode.Both;
                }
            }
        }

        public IntersectionMode IntersectionWith(Native.Pos pos, out int index)
        {
            if (this.tokens.Count == 0)
            {
                index = 0;
                return IntersectionMode.None;
            }

            if (!this.tokens[0].Span.Unit.Equals(pos.Unit))
            {
                throw new ArgumentException("Unit mismatch in TokenList.Search", "pos");
            }

            return this.doIntersectionWith(pos, out index);
        }

        public IntersectionMode IntersectionWith(SnapshotPoint point, out int index)
        {
            if (this.tokens.Count == 0)
            {
                index = 0;
                return IntersectionMode.None;
            }

            // since the request snapshot is normally equal to or after the token list's snapshot,
            // and since the completion takes place at the end of the word being typed,
            // any revert to the snapshot should push the point towards the end of document.
            point = point.TranslateTo(this.snapshot, PointTrackingMode.Positive);

            var pos = new Native.Pos(this.tokens[0].Span.Unit, (UInt32)point.Position);
            return this.doIntersectionWith(pos, out index);
        }
    }
}
