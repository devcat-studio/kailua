using System;

namespace Kailua.Native
{
    public class NativeException : Exception
    {
        public String Description { get; internal set; }
        
        public NativeException(String description)
        {
            this.Description = description;
        }
    }
}
