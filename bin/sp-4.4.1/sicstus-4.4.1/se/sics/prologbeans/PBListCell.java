/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

import java.io.IOException;

/**
 * <code>PBListCell</code> is the 
 * representation of Prolog list cells, i.e. compound terms with the functor
 * ./2.
 */
class PBListCell extends PBCompound {
	// [PM] FIXME: synchronization story
	private String cachedString = null;
	// [PM] FIXME: synchronization story
	private Boolean cachedIsTrueString = null;

	PBListCell(PBTerm head, PBTerm tail) {
		this(new PBTerm[] { head, tail });
	}

	PBListCell(PBTerm[] terms) {
		super(".", terms);
		// assert terms != null && terms.length == 2;
	}

	public boolean isListCell() {
		return true;
	}

	// There is no Character.isDefined(int) in .NET
	private boolean isUnicode(int c) {
		return ((c < 0xD800 || c > 0xDFFF) && c <= 0x10FFFD);
	}

	// There is no StringBuilder.appendCodePoint(int) in .NET
	private void appendCodePoint(StringBuffer sb, int c) {
		if (c < 0xFFFF) {
			sb.append((char) c);
		} else {
			int c1 = c - 0x10000;
			int c1h = c1 >> 10; // higher 10 bits of c1
			int c1l = (c1 << 10) >> 10; // lower 10 bits of c1
			int u1 = 0xD800;
			int u2 = 0xDC00;
			u1 = u1 | c1h;
			u2 = u2 | c1l;
			sb.append((char) u1);
			sb.append((char) u2);
		}
	}

	public boolean isString() {
		if (cachedString != null) {
			return true;
		} else {
			try {
				cachedString = getString();
				return true;
			} catch (IllegalStateException ise) {
				cachedString = null; // *** FIXME: is this necessary?
				return false;
			}
		}
	}

	/**
	 * Returns <code>true</code> if this <code>PBTerm</code> is a proper list
	 * and all of its elements are character codes. Returns <code>false</code>
	 * otherwise.
	 */
	private boolean isTrueString() {
		if (cachedIsTrueString == null) {
			boolean trueString = true;
			PBListCell pblc;
			PBTerm h, t = this;
			while ((t != PBTerm.NIL) && trueString) {
				pblc = (PBListCell) t;
				h = pblc.arguments[0]; // head()
				if (h.isInteger() && isUnicode((int) h.intValue())) {
					t = pblc.arguments[1]; // tail()
					trueString = (t.isListCell() || t == PBTerm.NIL);
				} else {
					trueString = false;
				}
			}
			cachedIsTrueString = new Boolean(trueString);
		}
		return cachedIsTrueString.booleanValue();
	}

	public String getString() // throws IllegalStateException
	{
		if (cachedString == null) {
			StringBuffer sbuf = new StringBuffer();
			cachedString = getString(sbuf).toString();
		}
		return cachedString;
	}

	// [PD] 4.0.0 Non-recursive version
	private StringBuffer getString(StringBuffer sbuf) // throws
														// IllegalStateException
	{
		PBListCell pbt = this;
		PBTerm h;
		do {
			h = pbt.arguments[0];
			if (h.isAtom() && h.name.length() == 1) { // Is head a one char
														// atom?
				sbuf.append(((PBAtom) h).getName());
			} else if (h.isInteger()) { // Is head an integer?
				// *** FIXME: Perhaps check if this cast truncates.
				int c = (int) h.intValue();
				if (!isUnicode(c)) {
					throw new IllegalStateException("not a list of characters");
				}
				appendCodePoint(sbuf, c);
			} else {
				throw new IllegalStateException("not a list of characters");
			}
			PBTerm t = pbt.arguments[1]; // tail()
			if (t == PBTerm.NIL) {
				return sbuf;
			} else {
				if (t.isListCell()) {
					pbt = (PBListCell) t;
				} else {
					throw new IllegalStateException("not a list of characters");
				}
			}
		} while (true);
	}

	/**
	 * Returns the head of this list cell.
	 */
	public PBTerm head() {
		// return getArgument(1);
		return arguments[0];
	}

	/**
	 * Returns the tail of this list cell.
	 */
	public PBTerm tail() {
		// return getArgument(2);
		return arguments[1];
	}

	/**
	 * Returns the length of this <code>PBListCell</code>.
	 * <p>
	 */
	public int length() {
		// [PM] 4.1.3 non-recursive version
		int len = 0;
		PBTerm tmp;
		for (tmp = this; tmp.isListCell(); tmp = tmp.tail()) {
			len++;
		}
		// [PM] 4.1.3 for verifying properness. Expect tmp to be a PBNil
		// here, i.e. tmp.length()==0.
		len += tmp.length();
		// return 1 + tail().length();
		return len;
	}

	String toPrologString() {
		StringBuffer sb = new StringBuffer().append('[');
		sb.append(arguments[0].toPrologString());
		PBTerm t = arguments[1];
		while (!t.isEmptyList()) {
			sb.append(',');
			sb.append(t.head().toPrologString());
			t = t.tail();
		}
		sb.append(']');
		return sb.toString();
	}

	public String toString() {
		StringBuffer sb = new StringBuffer().append('[');
		sb.append(arguments[0].toString());
		PBTerm t = arguments[1];
		while (!t.isEmptyList()) {
			sb.append(',');
			sb.append(t.head().toString());
			t = t.tail();
		}
		sb.append(']');
		return sb.toString();
	}

	void fastWrite(FastWriter writer) throws IOException {
		if (isTrueString()) {
			writer.writeString(getString());
		} else {
			super.fastWrite(writer);
		}
	}

	// @Override
	PBTerm[] fastWritePrefix(FastWriter writer) throws IOException {
		if (isTrueString()) {
			writer.writeString(getString());
			return PBTerm.NO_TERMS;
		} else {
			return super.fastWritePrefix(writer);
		}
	}

}
