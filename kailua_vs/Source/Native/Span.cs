using System;

namespace Kailua.Native
{
    public struct Span : IEquatable<Span>
    {
        private UInt32 unit;
        private UInt32 begin;
        private UInt32 end;

        static public Span Dummy
        {
            get { return new Span(0, 0, 0); }
        }

        internal Span(UInt32 unit, UInt32 begin, UInt32 end)
        {
            this.unit = unit;
            this.begin = begin;
            this.end = end;
        }
        
        public bool IsValid
        {
            get { return this.unit != 0; }
        }

        public Unit Unit
        {
            get { return new Unit(this.unit); }
        }

        public Pos Begin
        {
            get { return new Pos(this.unit, this.begin); }
        }

        public Pos End
        {
            get { return new Pos(this.unit, this.end); }
        }

        public bool Equals(Span other)
        {
            return this.unit == other.unit && this.begin == other.begin && this.end == other.end;
        }
    }
}
