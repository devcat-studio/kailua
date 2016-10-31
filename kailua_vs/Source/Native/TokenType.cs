namespace Kailua.Native
{
    public enum TokenType : byte
    {
        Dead = 0xff,

        Error = 0x00,
        Comment = 0x01,
        EOF = 0x07,

        Num = 0x04,
        Name = 0x05,
        Str = 0x06,

        Plus = 0x40,
        Dash = 0x41,
        Star = 0x42,
        Slash = 0x43,
        Percent = 0x44,
        Caret = 0x45,
        Hash = 0x46,
        EqEq = 0x47,
        TildeEq = 0x48,
        LtEq = 0x49,
        GtEq = 0x4a,
        Lt = 0x4b,
        Gt = 0x4c,
        Eq = 0x4d,
        Amp = 0x4e,
        Tilde = 0x4f,
        Pipe = 0x50,
        LtLt = 0x51,
        GtGt = 0x52,
        SlashSlash = 0x53,
        LParen = 0x54,
        RParen = 0x55,
        LBrace = 0x56,
        RBrace = 0x57,
        LBracket = 0x58,
        RBracket = 0x59,
        Semicolon = 0x5a,
        Colon = 0x5b,
        ColonColon = 0x5c,
        Comma = 0x5d,
        Dot = 0x5e,
        DotDot = 0x5f,
        DotDotDot = 0x60,
        DashDashHash = 0x61,
        DashDashV = 0x62,
        DashDashColon = 0x63,
        DashDashGt = 0x64,
        Ques = 0x65,
        Bang = 0x66,
        Newline = 0x3f,

        And = 0x80,
        Break = 0x81,
        Do = 0x82,
        Else = 0x83,
        Elseif = 0x84,
        End = 0x85,
        False = 0x86,
        For = 0x87,
        Function = 0x88,
        Goto = 0x89,
        If = 0x8a,
        In = 0x8b,
        Local = 0x8c,
        Nil = 0x8d,
        Not = 0x8e,
        Or = 0x8f,
        Repeat = 0x90,
        Return = 0x91,
        Then = 0x92,
        True = 0x93,
        Until = 0x94,
        While = 0x95,
        Assume = 0x96,
        Const = 0x97,
        Global = 0x98,
        Map = 0x99,
        Module = 0x9a,
        Once = 0x9b,
        Open = 0x9c,
        Type = 0x9d,
        Var = 0x9e,
        Vector = 0x9f,
    }

    namespace Extensions
    {
        public static class TokenTypeExtension
        {
            public static bool IsPunct(this TokenType tok)
            {
                return ((byte)tok & 0xc0) == 0x40;
            }

            public static bool IsKeyword(this TokenType tok)
            {
                return ((byte)tok & 0xc0) == 0x80;
            }
        }
    }
}
