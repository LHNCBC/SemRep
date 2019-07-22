using System;
using System.Numerics;

namespace se.sics.prologbeans
{
	
	/// <summary> <code>PBInteger</code> is the representation of Prolog integers.</summary>
	
	class PBInteger:PBAtomic
	{
		override internal int Type {
			get {
				return INTEGER;
			}
			
		}

		override public bool Integer {
			get {
				return true;
			}
		}

		override sealed public bool Number {
			get {
				return true;
			}
		}

		internal long longValue;

		/// <summary> Creates a new <code>PBInteger</code> instance with the specified value.</summary>
		internal PBInteger (long value) : base (Convert.ToString (value))
		{
			longValue = value;
		}

		internal PBInteger (long value, string name) : base (name)
		{
			longValue = value;
		}

		public override long intValue ()
		{
			return longValue;
		}

		public override BigInteger bigIntegerValue ()
		{
			return new BigInteger(intValue());
		}

		internal override string toPrologString ()
		{
			return ToString ();
		}

		public override string ToString ()
		{
			return Convert.ToString (longValue);
		}
	}
}