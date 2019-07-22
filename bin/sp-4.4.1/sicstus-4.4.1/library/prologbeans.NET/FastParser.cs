// Copyright (c) 2004 SICS AB. All rights reserved.
// 
namespace se.sics.prologbeans
{
	using System;

	// [PM] 4.3.2 BigInteger is new in .NET Framework 4.0, let's use it.
	using System.Numerics;

	/// <summary> <c>FastParser</c>
	/// Documentation of the fast_write/read "protocol"
	/// Each "string" looks like:
	/// [PM] 3.10.2 Updated specification
	/// D TYPE [type-specific data]
	/// D -> start of fastrw Term (D is also the version number)
	/// TYPE, one byte for type:
	/// I - integer - argument is a arbitrary long, nonempty string of
	/// ASCII decimal digits, prefixed by a minus sign if negative
	/// There should be no plus sign prefixed. There is no size restriction.
	/// F - float - argument is an ASCII string of digits with a decimal
	/// point preceeded by at least one digit, optionally using
	/// E-notation (capital E) with exponent optionally prefixed by
	/// minus sign. The floating point number is prefixed by minus if
	/// negative
	/// A - atom - argument is an ATOMNAME (*)
	/// Make UTF-8 explicit (QP loses here). (Luckily this should be exactly
	/// the Java String.getbytes bytes.)
	/// _ - variable (followed by DATA which is a string of digits). The
	/// variables are numbered 0..n in depth first traversal
	/// increasing argument order. This is the change in version 'D'
	/// compared to version 'C'. The variable numbering is strictly defined.
	/// S ATOMNAME n - compound with n terms [followed by n terms] - n is
	/// an (unsigned) byte
	/// " - a list prefix consisting of only integers in [1..255] (*)
	/// the sequence is terminated by zero byte followed by the tail
	/// term. Note that this is just a compact representation of
	/// nested compound terms with functor ./2.
	/// Example "ab", i.e., .(97, .(98, [])) becomes (using C
	/// char-constant notation) '"' 'a' 'b' '\0' ']'; [x,97,98|y]
	/// becomes '[' 'A' 'x' '\0' '"' 'a' 'b' '\0' 'A' 'y' '\0'
	/// > Clarified that this encoding is used for any "list"-prefix with
	/// > suitable elements. In particular it is not always followed by ']'.
	/// > Also note that the elements of this kind of list should *not* be
	/// > treated by the reader as individual bytes of an UTF-8 encoded
	/// > string. If a list is to be treated as a string then each element
	/// > should be treated as a single UNICODE character, this holds for
	/// > lists transmitted using any of the three ('"', '[' or 'S' '.' '\0'
	/// > '\2') possible serialization-formats.
	/// >
	/// [ - a list cell, i.e, a shortcut for 'S' '.' '\0' '\2' (*)
	/// ] - empty list, i.e, a shortcut for 'A' '[' ']' '\0' (*)
	/// DATA - a byte sequence terminated by '\0'
	/// NOTE: As of verson 'D' the numbering must be sequential from
	/// _0.._n, the prolog side fast_read will most likely
	/// crash if * this invariant is not maintained when
	/// sending data to * prolog.
	/// ATOMNAME is DATA where the bytes make up the UTF-8 name of the
	/// atom.
	/// (*) These could be optional in the writer since there are longer
	/// equivalent forms from which the reader could produce the same term.
	/// </summary>
	
    class FastParser
	{
		
		internal const byte VERSION = (byte)'D';
		internal const byte INTEGER = (byte)'I';
		internal const byte FLOAT = (byte)'F';
		internal const byte ATOM = (byte)'A';
		internal const byte COMPOUND = (byte)'S';
		internal const byte VARIABLE = (byte)'_';
		internal const byte STRING = (byte)'"';
		internal const byte LIST = (byte)'[';
		internal const byte NIL = (byte)']';
		
		System.Collections.Hashtable variableTable = null;


		public virtual PBTerm parseProlog (System.IO.Stream stream)
		{
			
			try {
				int c = stream.ReadByte ();
				if (c == VERSION) {
					return parseTerm (stream);
				} else {
					// [PD] 4.0.5 Stopgap solution until we implement a well designed exception strategy in Prologbeans.
					throw new System.IO.IOException ("Not a fast prolog expression, first byte: " + (int)c);
				}
			} finally {
				if (variableTable != null) {
					variableTable.Clear ();
				}
			}
		}

		static readonly bool logging = false;

		class Work
		{
			// Next read term should go into args[i]
			int i;
			// these are patched up after construction.
			internal readonly PBTerm[] args;

			readonly Work next;

			public Work (PBTerm[] args, Work next)
			{
				// assert args != null;
				// assert args.length > 0;
				this.args = args;
				i = 0;
				this.next = next;
			}

			/// <summary> Update top of stack. Return the resulting stack, i.e. either 'this'
			/// or this.next.
			/// </summary>
			internal Work bump (PBTerm term)
			{
				// assert i < args.length;
				// assert term != null;
				args [i] = term;
				i++;
				if (i < args.Length) {
					if (FastParser.logging) {
						Console.Out.WriteLine ("bump() returning this: " + this);
					}
					return this;
				}
				if (FastParser.logging) {
					Console.Out.WriteLine ("bump() returning next: " + next);
				}
				return next;
			}
			
			/*
			* bump the term onto the stack and then push terms on the resulting
			* stack. Never null.
			*/
			public Work bump (PBTerm term, PBTerm[] terms)
			{
				// Note that bump(term) may return null. This is why we do not use
				// something like bump(term).push(terms).
				Work tmp = new Work (terms, bump (term));
				if (FastParser.logging) {
					Console.Out.WriteLine ("bump() returning new: " + tmp);
				}
				return tmp;
			}
			
			// @Override
			public override string ToString ()
			{
				return "[" + base.ToString () + " at " + i + "/" + args.Length + "]";
			}
		}
		
		/*
		* [PM] 4.1.3 Manage an explicit stack to avoid running out of Java stack
		* (SPRM 11909)
		*/
		PBTerm parseTerm (System.IO.Stream stream)
		{
			Work outer = new Work (new PBTerm[1], null);
			Work stack = outer;
			do {
				int chr = stream.ReadByte ();
				
				PBTerm term;
				
				if (FastParser.logging) {
					Console.Out.WriteLine ("parseTerm() switch on " + chr);
				}

				switch (chr) {
					
				case INTEGER:
					{
						string val = getString (stream);
						// return new PBInteger(val);
						try {
							term = new PBInteger (Convert.ToInt64 (val, 10), val);
						} catch (FormatException /* nfe */) {
							// FIXME: Perhaps not catch FormatException? If malformed then it is no bignum either.
							term = new PBBignum (BigInteger.Parse (val), val);
						} catch (OverflowException) {
							term = new PBBignum (BigInteger.Parse (val), val);
						}
						if (logging) {
							Console.Out.WriteLine ("bump() INTEGER " + val);
						}
						stack = stack.bump (term);
					}
					break;
					
				case FLOAT:
					{
						string val = getString (stream);
						term = new PBFloat (Double.Parse (val), val);
						if (logging) {
							Console.Out.WriteLine ("bump() FLOAT " + val);
						}
						stack = stack.bump (term);
					}
					break;
					
				case ATOM:
					{
						string val = getString (stream);
						term = PBTerm.makeAtom (val);
						if (logging) {
							Console.Out.WriteLine ("bump() ATOM " + val);
						}
						stack = stack.bump (term);
					}
					break;
					
				case VARIABLE:
					{
						string val = '_' + getString (stream);
						if (variableTable == null) {
							variableTable = System.Collections.Hashtable.Synchronized (new System.Collections.Hashtable ());
						}
						PBVariable var = (PBVariable)variableTable [val];
						if (var == null) {
							var = new PBVariable (val);
							variableTable [val] = var;
						}
						term = var;
						if (logging) {
							Console.Out.WriteLine ("bump() VARIABLE " + val);
						}
						stack = stack.bump (term);
					}
					break;
					
				case STRING:
					{
						byte[] val = getBytes (stream);
						PBTerm t = parseTerm (stream);
						// Note that this builds the list from the end.
						for (int i = val.Length - 1; i >= 0; i--) {
							t = PBTerm.makeTerm (new PBInteger (val [i]), t);
						}
						term = t;
						if (logging) {
							Console.Out.WriteLine ("bump() STRING " + val);
						}
						stack = stack.bump (term);
					}
					break;
					
				case LIST:
					{
						const int noTerms = 2;
						PBTerm[] terms = new PBTerm[noTerms];
						term = new PBListCell (terms);
						if (logging) {
							Console.Out.WriteLine ("bump() LIST ./2");
						}
						stack = stack.bump (term, terms);
					}
					break;
					
				case NIL:
					{
						term = PBTerm.NIL;
						if (logging) {
							Console.Out.WriteLine ("bump() NIL");
						}
						stack = stack.bump (term);
					}
					break;
					
				case COMPOUND:
					{
						string val = getString (stream);
						int noTerms = stream.ReadByte ();
						PBTerm[] terms = new PBTerm[noTerms];

						term = PBTerm.makeTerm (val, terms);
							
						if (logging) {
							Console.Out.WriteLine ("bump() COMPOUND " + val + "/" + noTerms);
						}
						stack = stack.bump (term, terms);
					}
					break;
					
				default: 
					throw new System.IO.IOException ("Parse error: illegal character " + (char)chr);
					
				}
			} while (stack != null);
			// assert outer != null;
			// assert outer.args != null && outer.args.length == 1;
			// assert validTerm(outer.args[0]);
			if (logging) {
				Console.Out.WriteLine ("parseTermWithStack returning " + outer.args [0]);
			}
			return outer.args [0];
		}

		/// <summary> [PM] 4.1.3 Traverse the term and verify that no null arguments remain
		/// (for assertions).
		/// </summary>
		static bool validTerm (PBTerm pbTerm)
		{
			if (pbTerm == null) {
				return false;
			}
			// FIXME: Traverse all args[] arrays (without running out of Java
			// stack!)
			return true;
		}
		
		// [PM] 4.3.2 FIXME: Will do the wrong thing for supplementary code points
		static string getString (System.IO.Stream stream)
		{
			int c;
			System.IO.MemoryStream byteBuff = new System.IO.MemoryStream ();
			while ((c = stream.ReadByte ()) != '\x0000') {
				byteBuff.WriteByte ((Byte)c);
			}
			return FastWriter.UTF8encoder /* System.Text.Encoding.UTF8 */ .GetString (byteBuff.ToArray ());
		}

		static byte[] getBytes (System.IO.Stream stream)
		{
			int c;
			System.IO.MemoryStream byteBuff = new System.IO.MemoryStream ();
			while ((c = stream.ReadByte ()) != '\x0000') {
				byteBuff.WriteByte ((Byte)c);
			}
			return byteBuff.ToArray ();
		}
	}
}
