using System;

namespace se.sics.prologbeans
{
	
	/// <summary> <code>PBListCell</code> is the 
	/// representation of Prolog list cells, i.e. compound terms with the functor
	/// ./2.
	/// </summary>
	class PBListCell:PBCompound
	{
		override public sealed bool ListCell {
			get {
				return true;
			}
		}

		override public bool ProperList {
			get {
				if (cachedIsProperList == CACHED_BOOLEAN_UNKNOWN) {
					PBTerm tail = this;
					do {
						tail = tail.getArgument (2);
					} while (tail.ListCell);

					bool isProperList = tail.EmptyList;

					cachedIsProperList = (isProperList ? CACHED_BOOLEAN_TRUE : CACHED_BOOLEAN_FALSE);
				}
				return cachedIsProperList == CACHED_BOOLEAN_TRUE;
			}	
		}

		/// <summary> Returns <code>true</code> if this <code>PBTerm</code> is a proper list
		/// and all of its elements are character codes. Returns <code>false</code>
		/// otherwise.
		/// </summary>
		bool TrueString {
			get {
				// [PM] 4.3.2 Changed to use three byte values {unknown, false, true}.
				if (cachedIsTrueString == CACHED_BOOLEAN_UNKNOWN) {
					bool trueString = true;
					PBListCell pblc;
					PBTerm h, t = this;
					while ((t != PBTerm.NIL) && trueString) {
						pblc = (PBListCell)t;
						h = pblc.arguments [0]; // head()
						if (h.Integer && isUnicode ((int)h.intValue ())) {
							t = pblc.arguments [1]; // tail()
							trueString = (t.ListCell || t == PBTerm.NIL);
						} else {
							trueString = false;
						}
					}
					cachedIsTrueString = (trueString ? CACHED_BOOLEAN_TRUE : CACHED_BOOLEAN_FALSE);
				}
				return cachedIsTrueString == CACHED_BOOLEAN_TRUE;
			}
		}

		// [PM] FIXME: synchronization story
		string cachedString = null;
		// [PM] FIXME: synchronization story

		const byte CACHED_BOOLEAN_UNKNOWN = 0;
		const byte CACHED_BOOLEAN_FALSE = 1;
		const byte CACHED_BOOLEAN_TRUE = 2;
		byte cachedIsTrueString = CACHED_BOOLEAN_UNKNOWN;
		byte cachedIsProperList = CACHED_BOOLEAN_UNKNOWN;

		internal PBListCell (PBTerm head, PBTerm tail) : this (new []{ head, tail })
		{
		}

		internal PBListCell (PBTerm[] terms) : base (".", terms)
		{
			// assert terms != null && terms.length == 2;
		}
		
		// There is no Character.isDefined(int) in .NET
		static bool isUnicode (int c)
		{
			return ((c < 0xD800 || c > 0xDFFF) && c <= 0x10FFFD);
		}
		
		// There is no StringBuilder.appendCodePoint(int) in .NET
		static void appendCodePoint (System.Text.StringBuilder sb, int c)
		{
			if (c < 0xFFFF) {
				sb.Append ((char)c);
			} else {
				int c1 = c - 0x10000;
				int c1h = c1 >> 10; // higher 10 bits of c1
				int c1l = (c1 << 10) >> 10; // lower 10 bits of c1
				int u1 = 0xD800;
				int u2 = 0xDC00;
				u1 = u1 | c1h;
				u2 = u2 | c1l;
				sb.Append ((char)u1);
				sb.Append ((char)u2);
			}
		}

		public override bool isString ()
		{
			if (cachedString != null) {
				return true;
			} else {
				try {
					cachedString = getString ();
					return true;
				} catch (SystemException /* ise */) {
					cachedString = null; // *** FIXME: is this necessary?
					return false;
				}
			}
		}

		public override string getString ()
		{
			if (cachedString == null) {
				System.Text.StringBuilder sbuf = new System.Text.StringBuilder ();
				cachedString = getString (sbuf).ToString ();
			}
			return cachedString;
		}
		
		// [PD] 4.0.0 Non-recursive version
		System.Text.StringBuilder getString (System.Text.StringBuilder sbuf)
		{
			PBListCell pbt = this;
			PBTerm h;
			do {
				h = pbt.arguments [0];
				if (h.Atom && h.Name.Length == 1) {
					// Is head a one char
					// atom?
					sbuf.Append (h.Name);
				} else if (h.Integer) {
					// Is head an integer?
					// *** FIXME: Perhaps check if this cast truncates.
					int c = (int)h.intValue ();
					if (!isUnicode (c)) {
						throw new InvalidOperationException ("not a list of characters");
					}
					appendCodePoint (sbuf, c);
				} else {
					throw new InvalidOperationException ("not a list of characters");
				}
				PBTerm t = pbt.arguments [1]; // tail()
				if (t == PBTerm.NIL) {
					return sbuf;
				} else {
					if (t.ListCell) {
						pbt = (PBListCell)t;
					} else {
						throw new InvalidOperationException ("not a list of characters");
					}
				}
			} while (true);
		}

		/// <summary> Returns the head of this list cell.</summary>
		public override PBTerm head ()
		{
			return arguments [0];
		}

		/// <summary> Returns the tail of this list cell.</summary>
		public override PBTerm tail ()
		{
			return arguments [1];
		}

		public override int length ()
		{
			// [PM] 4.1.3 non-recursive version
			int len = 0;
			PBTerm tmp;
			for (tmp = this; tmp.ListCell; tmp = tmp.tail ()) {
				len++;
			}
			// [PM] 4.1.3 for verifying properness. Expect tmp to be a PBNil
			// here, i.e. tmp.length()==0.
			len += tmp.length ();
			return len;
		}

		internal override string toPrologString ()
		{
			System.Text.StringBuilder sb = new System.Text.StringBuilder ().Append ('[');
			sb.Append (arguments [0].toPrologString ());
			PBTerm t = arguments [1];
			while (!t.EmptyList) {
				sb.Append (',');
				sb.Append (t.head ().toPrologString ());
				t = t.tail ();
			}
			sb.Append (']');
			return sb.ToString ();
		}

		public override string ToString ()
		{
			System.Text.StringBuilder sb = new System.Text.StringBuilder ().Append ('[');
			sb.Append (arguments [0].ToString ());
			PBTerm t = arguments [1];
			while (!t.EmptyList) {
				sb.Append (',');
				sb.Append (t.head ().ToString ());
				t = t.tail ();
			}
			sb.Append (']');
			return sb.ToString ();
		}

		internal override PBTerm[] fastWritePrefix (FastWriter writer)
		{
			// [PM] 4.3.2 FIXME: it would be better to always write a string prefix (writeCharList) if possible, and then write the remainder regardless of whether the remainder is NIL or not.
			if (TrueString) {
				writer.writeString (getString ());
				return PBTerm.NO_TERMS;
			} else {
				return base.fastWritePrefix (writer);
			}
		}
	}
}