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
        LParen = 0x4e,
        RParen = 0x4f,
        LBrace = 0x50,
        RBrace = 0x51,
        LBracket = 0x52,
        RBracket = 0x53,
        Semicolon = 0x54,
        Colon = 0x55,
        Comma = 0x56,
        Dot = 0x57,
        DotDot = 0x58,
        DotDotDot = 0x59,
        DashDashHash = 0x5a,
        DashDashV = 0x5b,
        DashDashColon = 0x5c,
        DashDashGt = 0x5d,
        Ques = 0x5e,
        Pipe = 0x5f,
        Amp = 0x60,
        DashGt = 0x61,
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
        If = 0x89,
        In = 0x8a,
        Local = 0x8b,
        Nil = 0x8c,
        Not = 0x8d,
        Or = 0x8e,
        Repeat = 0x8f,
        Return = 0x90,
        Then = 0x91,
        True = 0x92,
        Until = 0x93,
        While = 0x94,
        Assume = 0x95,
        Const = 0x96,
        Global = 0x97,
        Module = 0x98,
        Once = 0x99,
        Open = 0x9a,
        Type = 0x9b,
        Var = 0x9c,
    }

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
