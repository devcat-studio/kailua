using System;
using System.Runtime.InteropServices;

namespace Kailua.Native
{
    [StructLayout(LayoutKind.Sequential)]
    public struct Pos : IEquatable<Pos>
    {
        private UInt32 unit;
        private UInt32 pos;

        static public Pos Dummy
        {
            get { return new Pos(0, 0); }
        }

        internal Pos(UInt32 unit, UInt32 pos)
        {
            this.unit = unit;
            this.pos = pos;
        }

        internal Pos(Unit unit, UInt32 pos)
        {
            this.unit = (uint)unit.Index;
            this.pos = pos;
        }

        public bool IsValid
        {
            get { return this.unit != 0; }
        }

        public Unit Unit
        {
            get { return new Unit(this.unit); }
        }

        public int Offset
        {
            get { return (int)this.pos; }
        }

        public bool Equals(Pos other)
        {
            return this.unit == other.unit && this.pos == other.pos;
        }
    }
}
