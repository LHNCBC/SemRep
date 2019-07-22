// Copyright (c) 2004 SICS AB. All rights reserved.
// 
namespace se.sics.prologbeans
{
	/// <summary> <code>PBAtomic</code> is the representation
	/// of Prolog constants.
	/// </summary>
	
	abstract class PBAtomic:PBTerm
	{
		internal abstract int Type{ get; }

		override sealed public bool Atomic {
			get {
				return true;
			}
		}

		internal const int ATOM = 1;
		internal const int INTEGER = 2;
		internal const int FLOAT = 3;
		internal const int VARIABLE = 4;

		/// <summary> Creates a new <code>PBAtomic</code> instance with the specified name.</summary>
		internal PBAtomic (string name) : base (name)
		{
		}

		internal abstract override string toPrologString ();

		public abstract override string ToString ();

		internal override PBTerm[] fastWritePrefix (FastWriter writer)
		{
			writer.writeAtomic (this);
			return PBTerm.NO_TERMS;
		}
	}
}
