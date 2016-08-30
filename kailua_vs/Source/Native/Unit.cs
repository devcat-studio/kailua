using System;

namespace Kailua.Native
{
    public struct Unit : IEquatable<Unit>
    {
        private UInt32 unit;

        static public Unit Dummy
        {
            get { return new Unit(0); }
        }

        internal Unit(UInt32 unit)
        {
            this.unit = unit;
        }

        public bool IsValid
        {
            get { return this.unit != 0; }
        }

        public int Index
        {
            get { return (int)this.unit; }
        }

        public bool Equals(Unit other)
        {
            return this.unit == other.unit;
        }
    }
}
