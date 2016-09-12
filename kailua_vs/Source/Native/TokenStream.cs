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

        private TokenStreamHandle() : base(IntPtr.Zero, true) { }

        public override bool IsInvalid
        {
            get
            {
                return this.handle.Equals(IntPtr.Zero);
            }
        }

        public void Leak()
        {
            lock (this)
            {
                this.SetHandle(IntPtr.Zero);
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

    internal class TokenStreamEnumerator : IDisposable, IEnumerator<TokenTypeAndSpan>
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern TokenType kailua_token_stream_next(
            TokenStreamHandle stream,
            out Span span);

        internal TokenStream stream;
        internal TokenTypeAndSpan last;

        internal TokenStreamEnumerator(TokenStream stream)
        {
            this.stream = stream;
        }

        public bool MoveNext()
        {
            Span span;
            TokenType ret;
            lock (this.stream.native)
            {
                ret = kailua_token_stream_next(this.stream.native, out span);
            }
            if (ret == TokenType.Dead)
            {
                throw new NativeException("internal error while tokenizing a source code");
            }
            this.last = new TokenTypeAndSpan(ret, span);
            return ret != TokenType.EOF;
        }

        public void Reset()
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
        }
    }

    public class TokenStream : IDisposable, IEnumerable<TokenTypeAndSpan>
    {
        [DllImport("KailuaVSNative.dll", CharSet = CharSet.Unicode, CallingConvention = CallingConvention.Cdecl)]
        private static extern TokenStreamHandle kailua_token_stream_new(
            SourceHandle source,
            ref Span span,
            ReportHandle report);

        internal TokenStreamHandle native;

        public TokenStream(Source source, Span span, Report report)
        {
            this.native = kailua_token_stream_new(source.native, ref span, report.native);
            if (this.native.IsInvalid)
            {
                throw new NativeException("internal error while finding a source code to tokenize");
            }
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return new TokenStreamEnumerator(this);
        }

        IEnumerator<TokenTypeAndSpan> IEnumerable<TokenTypeAndSpan>.GetEnumerator()
        {
            return new TokenStreamEnumerator(this);
        }

        public void Dispose()
        {
            this.native.Dispose();
        }
    }
}