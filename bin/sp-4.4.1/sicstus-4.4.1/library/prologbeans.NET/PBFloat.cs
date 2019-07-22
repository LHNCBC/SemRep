
using System;

namespace se.sics.prologbeans
{
	
	/// <summary> <code>PBFloat</code> is the representation of Prolog integers.</summary>
	
	class PBFloat:PBAtomic
	{
		override sealed internal int Type {
			get {
				return FLOAT;
			}
			
		}

		override sealed public bool Float {
			get {
				return true;
			}
			
		}

		override sealed public bool Number {
			get {
				return true;
			}
		}

		internal double doubleValue;

		/// <summary> Creates a new <code>PBFloat</code> instance with the specified value.</summary>
		internal PBFloat (double value) : base (value.ToString ())
		{
			doubleValue = value;
		}

		internal PBFloat (double value, string name) : base (name)
		{
			doubleValue = value;
		}

		public override double floatValue ()
		{
			//      return Double.parseDouble(name);
			return doubleValue;
		}

		internal override string toPrologString ()
		{
			return ToString ();
		}

		public override string ToString ()
		{
			return doubleValue.ToString ();
		}
	}
}