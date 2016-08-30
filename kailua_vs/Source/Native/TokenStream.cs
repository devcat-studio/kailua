using System;
using System.Runtime.InteropServices;
using System.Collections;
using System.Collections.Generic;

namespace Kailua.Native
{
    internal class TokenStreamHandle : SafeHandle
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern void kailua_token_stream_free(IntPtr stream);

        public TokenStreamHandle() : base(IntPtr.Zero, true) { }

        public override bool IsInvalid
        {
            get
            {
                return this.handle.Equals(IntPtr.Zero);
            }
        }

        protected override bool ReleaseHandle()
        {
            lock (this)
            {
                kailua_token_stream_free(this.handle);
            }
            return true;
        }
    }

    public struct TokenTypeAndSpan
    {
        public TokenType Type;
        public Span Span;

        public TokenTypeAndSpan(TokenType type, Span span)
        {
            this.Type = type;
            this.Span = span;
        }
    }

    public class TokenStream : IDisposable, IEnumerable<TokenTypeAndSpan>, IEnumerator<TokenTypeAndSpan>
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern TokenStreamHandle kailua_token_stream_new(
            SourceHandle source,
            ref Span span);

        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern TokenType kailua_token_stream_next(
            TokenStreamHandle stream,
            out Span span);

        internal TokenStreamHandle native;
        internal TokenTypeAndSpan last;

        public TokenStream(Source source, Span span)
        {
            this.native = kailua_token_stream_new(source.native, ref span);
            if (this.native.IsInvalid)
            {
                throw new NativeException("internal error while finding a source code to tokenize");
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this;
        }

        IEnumerator<TokenTypeAndSpan> IEnumerable<TokenTypeAndSpan>.GetEnumerator()
        {
            return this;
        }

        bool IEnumerator.MoveNext()
        {
            Span span;
            TokenType ret;
            lock (this.native)
            {
                ret = kailua_token_stream_next(this.native, out span);
            }
            if (ret == TokenType.Dead)
            {
                throw new NativeException("internal error while tokenizing a source code");
            }
            this.last = new TokenTypeAndSpan(ret, span);
            return ret != TokenType.EOF;
        }

        void IEnumerator.Reset()
        {
            throw new NotImplementedException();
        }

        object IEnumerator.Current
        {
            get { return this.last; }
        }

        TokenTypeAndSpan IEnumerator<TokenTypeAndSpan>.Current
        {
            get { return this.last; }
        }

        public void Dispose()
        {
            this.native.Dispose();
        }
    }
}