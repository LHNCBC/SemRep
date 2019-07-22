
namespace se.sics.prologbeans
{
	
	/// <summary> <code>PBNil</code> is the representation of the empty list, [].</summary>
	
	class PBNil:PBAtom
	{
		override public bool EmptyList {
			get {
				return true;
			}
			
		}

		override public bool ProperList {
			get {
				return true;
			}
			
		}

		/// <summary> Creates a new <code>PBNil</code> instance with the specified name.</summary>
		internal PBNil () : base ("[]")
		{
		}

		public override int length ()
		{
			return 0;
		}

		public override string getString ()
		{
			return "";
		}
	}
}