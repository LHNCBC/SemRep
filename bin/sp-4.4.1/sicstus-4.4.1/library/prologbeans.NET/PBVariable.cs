
namespace se.sics.prologbeans
{
	
	/// <summary> <code>PBVariable</code> is the
	/// representation of Prolog variables.
	/// </summary>
	
	class PBVariable:PBTerm
	{
		virtual internal int Type {
			get {
				return PBAtomic.VARIABLE;
			}			
		}

		override public sealed bool Variable {
			get {
				return true;
			}			
		}

		/// <summary> Creates a new <code>PBVariable</code> instance with the specified name.</summary>
		internal PBVariable (string name) : base (name)
		{
		}

		internal override string toPrologString ()
		{
			return ToString ();
		}

		public override string ToString ()
		{
			return Name;
		}

		internal override PBTerm[] fastWritePrefix (FastWriter writer)
		{
			writer.writeVariable (this);
			return PBTerm.NO_TERMS;
		}
	}
}