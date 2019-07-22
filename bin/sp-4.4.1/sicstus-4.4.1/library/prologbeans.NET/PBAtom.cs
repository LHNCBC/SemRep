namespace se.sics.prologbeans
{
	using System;

	/// <summary> <code>PBAtom</code> is the representation of Prolog atoms.</summary>
	
	class PBAtom:PBAtomic
	{
		override sealed internal int Type {
			get {
				return ATOM;
			}
		}

		override sealed public bool Atom {
			get {
				return true;
			}
		}

		/// <summary> Creates a new <code>PBAtom</code> instance with the specified name.</summary>
		internal PBAtom (string name) : base (name)
		{
		}

		internal override string toPrologString ()
		{
			return stuffAtom (Name);
		}

		public override string ToString ()
		{
			return Name;
		}
	}
}