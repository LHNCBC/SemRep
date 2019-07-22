// Copyright (c) 2004 SICS AB. All rights reserved.
// 
namespace se.sics.prologbeans
{
	using System;

	/// <summary> <code>PBCompound</code> is the
	/// representation of Prolog compound terms.
	/// </summary>
	class PBCompound:PBTerm
	{
		override public sealed bool Compound {
			get {
				return true;
			}

		}

		override public int Arity {
			get {
				return arguments.Length;
			}
		}

		protected internal readonly PBTerm[] arguments;

		// Must never be used to create a (.)/2 term (i.e. a list cell).
		internal PBCompound (string name, PBTerm[] args) : base (name)
		{
			// assert (args != null && args.Length > 0 && !(".".equals(name) && args.Length == 2))
			arguments = args;
		}

		public override PBTerm getArgument (int index)
		{
			if (index < 1 || index > arguments.Length) {
				throw new ArgumentOutOfRangeException ("index");
			}
			return arguments [index - 1];
		}

		internal override string toPrologString ()
		{
			// [PM] 4.3.2 FIXME: stack overflow issue.
			System.Text.StringBuilder sb = new System.Text.StringBuilder ().Append (stuffAtom (Name));

			sb.Append ('(');
			for (int i = 0, n = arguments.Length; i < n; i++) {
				if (i > 0) {
					sb.Append (',');
				}
				sb.Append (arguments [i].toPrologString ());
			}
			sb.Append (')');

			return sb.ToString ();
		}

		public override string ToString ()
		{
			// [PM] 4.3.2 FIXME: stack overflow issue.
			System.Text.StringBuilder sb = new System.Text.StringBuilder ().Append (Name);
		
			sb.Append ('(');
			for (int i = 0, n = arguments.Length; i < n; i++) {
				if (i > 0) {
					sb.Append (',');
				}
				sb.Append ((arguments [i] != null ? arguments [i].ToString () : "<<NULL>>"));
			}
			sb.Append (')');

			return sb.ToString ();
		}

		internal override PBTerm[] fastWritePrefix (FastWriter writer)
		{
			writer.writeCompound (Name, arguments.Length);
			return arguments;
		}
	}
}
