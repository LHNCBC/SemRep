namespace se.sics.prologbeans
{
	using System;

	// [PM] 4.3.2 BigInteger is new in .NET Framework 4.0, let's use it.
	using System.Numerics;

	/// <summary> <code>PBTerm</code> is the representations
	/// of Prolog terms.
	/// </summary>
	public abstract class PBTerm
	{
		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is
		/// an atom and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is an atom
		/// </returns>
		virtual public bool Atom {
			get {
				return false;
			}
			
		}

		/// <summary> See the Atom property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isAtom ()
		{
			return Atom;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
		/// constant (e.g. integer, floating-point number, or atom) and
		/// <code>false</code> if this {@link se.sics.prologbeans.PBTerm} is a
		/// compound term or variable.
		/// </summary>
		/// <returns> whether the argument is an atomic term
		/// </returns>
		virtual public bool Atomic {
			get {
				return false;
			}
			
		}

		/// <summary> See the Atomic property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isAtomic ()
		{
			return Atomic;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
		/// number and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is a number
		/// </returns>
		virtual public bool Number {
			get {
				return false;
			}
			
		}

		/// <summary> See the Number property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isNumber ()
		{
			return Number;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is
		/// an integer and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is an integer
		/// </returns>
		virtual public bool Integer {
			get {
				return false;
			}			
		}

		/// <summary> See the Integer property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isInteger ()
		{
			return Integer;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is
		/// an bignum integer and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is a bignum integer
		/// </returns>
		virtual public bool Bignum {
			get {
				return false;
			}			
		}

		/// <summary> See the Bignum property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isBignum ()
		{
			return Bignum;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
		/// floating-point number and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is a floating-point number
		/// </returns>
		virtual public bool Float {
			get {
				return false;
			}			
		}

		/// <summary> See the Float property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isFloat ()
		{
			return Float;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
		/// compund term and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is a compound term
		/// </returns>
		virtual public bool Compound {
			get {
				return false;
			}
		}

		/// <summary> See the Compound property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isCompound ()
		{
			return Compound;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
		/// list cell, i.e. a compound term with the functor ./2, and
		/// <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is a list cell
		/// </returns>
		virtual public bool ListCell {
			get {
				return false;
			}
			
		}

		/// <summary> See the ListCell property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isListCell ()
		{
			return ListCell;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
		/// proper list and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is a proper list
		/// </returns>
		virtual public bool ProperList {
			get {
				return false;
			}
			
		}

		/// <summary>Getter for ProperList property</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isProperList ()
		{
			return ProperList;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is
		/// the empty list and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is an empty list
		/// </returns>
		virtual public bool EmptyList {
			get {
				return false;
			}
		}

		/// <summary> See the EmptyList property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isEmptyList ()
		{
			return EmptyList;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
		/// variable and <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is a variable
		/// </returns>
		virtual public bool Variable {
			get {
				return false;
			}
			
		}

		/// <summary>Getter for Variable property</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public bool isVariable ()
		{
			return Variable;
		}


		/// <summary> Returns the name of this constant or compound term.</summary>
		/// <returns> the name of the term
		/// </returns>
		virtual public string Name {
			get {
				return _name;
			}
			
		}

		/// <summary> See the Name property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public string getName ()
		{
			return Name;
		}

		/// <summary> Returns the number of arguments of this compound term or 0 if this
		/// {@link se.sics.prologbeans.PBTerm} is not a compound term.
		/// </summary>
		/// <returns> the arity of the term
		/// </returns>
		virtual public int Arity {
			get {
				return 0;
			}
			
		}

		/// <summary> See the Arity property.</summary>
		// [PM] 4.3.2 Backwards compatibility with the J# version.
		public int getArity ()
		{
			return Arity;
		}

		internal static readonly PBTerm[] NO_TERMS = new PBTerm[0];

		/// <summary> The {@link se.sics.prologbeans.PBTerm} <code>NIL</code>, with the
		/// printname "[]", represents the empty list.
		/// </summary>
		public static readonly PBTerm NIL = new PBNil ();
		
		readonly string _name;

		/// <summary> Creates a new {@link se.sics.prologbeans.PBTerm} instance with the
		/// specified name.
		/// </summary>
		internal PBTerm (string name)
		{
			if (name == null) {
				throw new ArgumentNullException ("name");
			}
			_name = name;
		}

		/// <summary> Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
		/// proper list and all of its elements are character codes or one character
		/// atoms. Returns <code>false</code> otherwise.
		/// </summary>
		/// <returns> whether the argument is a string
		/// </returns>
		public virtual bool isString ()
		{
			if (PrologSession.debugging ()) {
				Console.Error.WriteLine ("Entering PBTerm.isString()");
			}
			return false;
		}

		/// <summary> Returns the head of this {@link se.sics.prologbeans.PBTerm} if it is a
		/// list cell, i.e. a compound term with the functor ./2.
		/// 
		/// </summary>
		/// <returns> the head of the list
		/// </returns>
		public virtual PBTerm head ()
		{
			throw new InvalidOperationException ("not a list cell: " + ToString ());
		}

		/// <summary> Returns the tail of this {@link se.sics.prologbeans.PBTerm} if it is a
		/// list cell, i.e. a compound term with the functor ./2.
		/// 
		/// </summary>
		/// <returns> the tail of the list
		/// </returns>
		public virtual PBTerm tail ()
		{
			throw new InvalidOperationException ("not a list cell: " + ToString ());
		}

		/// <summary>The ONE BASED argument if this is a compound term</summary>
		public virtual PBTerm getArgument (int index)
		{
			throw new InvalidOperationException ("not a compound term: " + ToString ());
		}

		/// <summary> If this {@link se.sics.prologbeans.PBTerm} is a proper list, returns its
		/// length.
		/// </summary>
		/// <returns> the length of the list
		/// </returns>
		public virtual int length ()
		{
			throw new InvalidOperationException ("not a proper list: " + ToString ());
		}

		/// <summary> Returns the integer value, as a long, if this is an integer term</summary>
		/// <returns> the integer value of the term
		/// </returns>
		public virtual long intValue ()
		{
			throw new InvalidOperationException ("not an integer: " + ToString ());
		}

		/// <summary> Returns the integer value, as a BigInteger, if this is an integer term</summary>
		/// <returns> the integer value of the term
		/// </returns>
		public virtual BigInteger bigIntegerValue ()
		{
			throw new InvalidOperationException ("not an integer: " + ToString ());
		}

		/// <summary> Returns the floating-point valuem, if this is a floating point term.</summary>
		/// <returns> the floating point value of the term
		/// </returns>
		public virtual double floatValue ()
		{
			throw new InvalidOperationException ("not a float: " + ToString ());
		}

		/// <summary> If this is a proper list and all its
		/// elements are Unicode character codes, either as integers or as one-character atoms,
		/// returns a  string with the list elements as the character codes of the string.
		/// 
		/// </summary>
		/// <returns> the {@link string} corresponding to this list
		/// </returns>
		public virtual string getString ()
		{
			throw new InvalidOperationException ("not a proper list: " + ToString ());
		}

		/// <summary> For internal use by PrologBeans.
		/// 
		/// Returns a string representation of this term in a format that can be
		/// parsed by a Prolog parser.
		/// </summary>
		
		// [PM] FIXME: Should use fastrw-format to send data to Prolog as
		// well. It will be a nightmare trying to produce properly quoted
		// terms in a way that can be read correctly by Prolog. If that is
		// not hard enough try making it work with non-8-bit characters and
		// then start flipping the prolog-flags 'language' (ISO have
		// different quoting rules than SICStus), 'double_quotes' and
		// 'character_escapes'. (Did I mention that I think relying on the
		// prolog reader is a bad idea for the release version :-).
		// [JE] Fixed using writeFast() (toPrologString() not used anymore)
		
		internal abstract string toPrologString ();

		/// <summary> Returns a string description of this term.</summary>
		abstract public override string ToString ();
		
		// -------------------------------------------------------------------
		// Static methods to create PBTerm instances
		// -------------------------------------------------------------------
		
		/// <summary> Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
		/// float value.
		/// </summary>
		/// <param name="value">should be finite
		/// </param>
		/// <returns> a term representing the argument
		/// </returns>
		static public PBTerm makeTerm (float value)
		{
			return new PBFloat (value);
		}

		/// <summary> Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
		/// double value.
		/// </summary>
		/// <param name="value">should be finite
		/// </param>
		/// <returns> a term representing the argument
		/// </returns>
		static public PBTerm makeTerm (double value)
		{
			return new PBFloat (value);
		}

		/// <summary> Creates a new {@link se.sics.prologbeans.PBTerm} instance representing an
		/// int value.
		/// </summary>
		/// <param name="value">the value to represent
		/// </param>
		/// <returns> a term representing the argument
		/// </returns>
		static public PBTerm makeTerm (int value)
		{
			return new PBInteger (value);
		}

		/// <summary> Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
		/// long value.
		/// </summary>
		/// <param name="value">the value to represent
		/// </param>
		/// <returns> a term representing the argument
		/// </returns>
		static public PBTerm makeTerm (long value)
		{
			return new PBInteger (value);
		}

		/// <summary> Creates a PBTerm instance representing a BigInteger value.
		/// </summary>
		/// <param name="value">the non-null value to represent
		/// </param>
		/// <returns> a term representing the argument
		/// </returns>
		static public PBTerm makeTerm (BigInteger value)
		{
			return new PBBignum (value);
		}

		/// <summary> Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
		/// list with the characters, as integer values, in the string argument as
		/// its elements.
		/// </summary>
		/// <param name="value">the non-null value to represent
		/// </param>
		/// <returns> a term representing the argument
		/// </returns>
		static public PBTerm makeTerm (string value)
		{
			if (value == null) {
				throw new ArgumentNullException ("value");
			}

			// [PM] 4.3.2
			{
				PBTerm tail = PBTerm.NIL;
				for (int i = value.Length - 1; i >= 0; i--) {
					tail = PBTerm.makeTerm (PBTerm.makeTerm (value [i]), tail);
				}
				return tail;
			}
		}

		/// <summary> Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
		/// compound term.
		/// </summary>
		/// <param name="name">the name of the term
		/// </param>
		/// <param name="arguments">the, non-empty, arguments of the term
		/// </param>
		/// <returns> a term representing the argument
		/// </returns>
		static public PBTerm makeTerm (string name, PBTerm[] arguments)
		{
			if (name == null) {
				throw new ArgumentNullException ("name");
			}
			if (arguments == null) {
				throw new ArgumentNullException ("arguments");
			}
			if (arguments.Length == 0) {
				throw new ArgumentException ("arguments must be non-empty");
			}


			// If the compound term being created is a list cell, create an
			// instance of PBListCell instead, making sure that its methods will
			// be called when the instance is used.
			
			// [PM] 4.1.3 Now arguments may not yet be filled in so must pass
			// arguments[] to constructor.
			if (name.Equals (".") && arguments.Length == 2) {
				return new PBListCell (arguments);
			}

			return new PBCompound (name, arguments);
		}

		/// <summary> Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
		/// list cell.
		/// </summary>
		/// <param name="head">the first argument of the list cell
		/// </param>
		/// <param name="tail">the second argument of the list cell
		/// </param>
		/// <returns> a term representing the argument
		/// </returns>
		static public PBTerm makeTerm (PBTerm head, PBTerm tail)
		{
			return new PBListCell (head, tail);
		}

		/// <summary> Creates a new {@link se.sics.prologbeans.PBTerm} instance representing an
		/// atom.
		/// </summary>
		/// <param name="value">the non-null name of the atom
		/// </param>
		/// <returns> a term representing the argument
		/// </returns>
		static public PBTerm makeAtom (string value)
		{
			if (value.Equals ("[]")) {
				return NIL;
			} else {
				return new PBAtom (value);
			}
		}
		
		// -------------------------------------------------------------------
		// Internal utilities
		// -------------------------------------------------------------------

		static internal string stuffAtom (string atom)
		{
			// [PM] 4.3.2 Do not quote it (only used for ToString, i.e. debugging.
			return atom;
		}

		/// <summary> Write fastrw prefix. Return array of arguments that should be written
		/// after the prefix. The returned array may be empty but never null. Note
		/// that the returned array may have no direct relationship to the arguments
		/// of the term, e.g. when writing a list in compact string notation.
		/// 
		/// </summary>
		/// <throws>  IOException </throws>
		// [PM] 4.1.3
		internal abstract PBTerm[] fastWritePrefix (FastWriter writer);
	}
}