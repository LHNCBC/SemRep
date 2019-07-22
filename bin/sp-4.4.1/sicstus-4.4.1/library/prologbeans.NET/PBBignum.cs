namespace se.sics.prologbeans
{
	using System;
	using System.Numerics;

	/// <summary> <code>PBBignum</code> is the representation of large Prolog integers.</summary>
	class PBBignum:PBAtomic
	{
		override sealed internal int Type {
			get {
				return INTEGER;
			}
		}

		override sealed public bool Bignum {
			get {
				return true;
			}
		}

		override sealed public bool Number {
			get {
				return true;
			}
		}

		override sealed public bool Integer {
			get {
				return true;
			}
		}

		// [PM] 4.3.2 BigInteger is new in .NET Framework 4.0, let's use it.
		internal BigInteger bigIntValue;

		internal PBBignum (BigInteger value) : base (value.ToString ())
		{
			bigIntValue = value;
		}

		internal PBBignum (BigInteger value, string name) : base (name)
		{
			bigIntValue = value;
		}

		public override BigInteger bigIntegerValue ()
		{
			return bigIntValue;
		}

		internal override string toPrologString ()
		{
			return ToString ();
		}

		public override string ToString ()
		{
			return bigIntValue.ToString ();
		}
	}
}