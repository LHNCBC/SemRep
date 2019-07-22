/**
 * Copyright (c) 2005 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

import java.io.IOException;
import java.math.BigInteger;

/**
 * <code>PBTerm</code> is the representations
 * of Prolog terms.
 */
public abstract class PBTerm {
	static final PBTerm[] NO_TERMS = new PBTerm[0];
	/**
	 * The {@link se.sics.prologbeans.PBTerm} <code>NIL</code>, with the
	 * printname "[]", represents the empty list.
	 */
	public final static PBTerm NIL = new PBNil();

	protected final String name;

	/**
	 * Creates a new {@link se.sics.prologbeans.PBTerm} instance with the
	 * specified name.
	 * 
	 * @throws {@link NullPointerException} if the argument is null.
	 */
	PBTerm(String name) {
		if (name == null) {
			throw new NullPointerException("PBTerm cannot have a null name");
		}
		this.name = name;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is
	 * an atom and <code>false</code> otherwise.
	 * @return whether the argument is an atom
	 */
	public boolean isAtom() {
		return false;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
	 * constant (e.g. integer, floating-point number, or atom) and
	 * <code>false</code> if this {@link se.sics.prologbeans.PBTerm} is a
	 * compound term or variable.
	 * @return whether the argument is an atomic term
	 */
	public boolean isAtomic() {
		return false;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
	 * number and <code>false</code> otherwise.
	 * @return whether the argument is a number
	 */
	public boolean isNumber() {
		return false;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is
	 * an integer and <code>false</code> otherwise.
	 * @return whether the argument is an integer
	 */
	public boolean isInteger() {
		return false;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is
	 * an bignum integer and <code>false</code> otherwise.
	 * @return whether the argument is a bignum integer
	 */
	public boolean isBignum() {
		return false;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
	 * floating-point number and <code>false</code> otherwise.
	 * @return whether the argument is a floating-point number
	 */
	public boolean isFloat() {
		return false;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
	 * compund term and <code>false</code> otherwise.
	 * @return whether the argument is a compound term
	 */
	public boolean isCompound() {
		return getArity() > 0;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
	 * list cell, i.e. a compound term with the functor ./2, and
	 * <code>false</code> otherwise.
	 * @return whether the argument is a list cell
	 */
	public boolean isListCell() {
		return false;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
	 * proper list and <code>false</code> otherwise.
	 * @return whether the argument is a proper list
	 */
	public boolean isProperList() {
		return false;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is
	 * the empty list and <code>false</code> otherwise.
	 * @return whether the argument is an empty list
	 */
	public boolean isEmptyList() {
		return false;
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
	 * proper list and all of its elements are character codes or one character
	 * atoms. Returns <code>false</code> otherwise.
	 * @return whether the argument is a string
	 */
	public boolean isString() {
		if (PrologSession.debugging()) {
			System.err.println("Entering PBTerm.isString()");
		}
		return false;
	}

	/**
	 * Returns the head of this {@link se.sics.prologbeans.PBTerm} if it is a
	 * list cell, i.e. a compound term with the functor ./2.
	 * 
	 * @return the head of the list
	 * @throws IllegalStateException if this
	 *         {@link se.sics.prologbeans.PBTerm} is not a list cell.
	 */
	public PBTerm head() {
		throw new IllegalStateException("not a list cell: " + toString());
	}

	/**
	 * Returns the tail of this {@link se.sics.prologbeans.PBTerm} if it is a
	 * list cell, i.e. a compound term with the functor ./2.
	 * 
	 * @return the tail of the list
	 * @throws IllegalStateException if this
	 *         {@link se.sics.prologbeans.PBTerm} is not a list cell.
	 */
	public PBTerm tail() {
		throw new IllegalStateException("not a list cell: " + toString());
	}

	/**
	 * Returns <code>true</code> if this {@link se.sics.prologbeans.PBTerm} is a
	 * variable and <code>false</code> otherwise.
	 * @return whether the argument is a variable
	 */
	public boolean isVariable() {
		return false;
	}

	/**
	 * Returns the name of this constant or compound term.
	 * @return the name of the term
	 */
	public String getName() {
		return name;
	}

	/**
	 * Returns the argument at the specified index. Only compound terms have
	 * arguments.<br>
	 * <strong>Note:</strong> the arguments are indexed from 1 to arity.
	 * 
	 * @param index
	 *            the (one based) index of the argument
	 * @return the argument as a {@link se.sics.prologbeans.PBTerm}
	 * @throws IllegalStateException if this term is not
	 *         compound
	 */
	public PBTerm getArgument(int index) {
		throw new IllegalStateException("not a compound term: " + toString());
	}

	/**
	 * If this {@link se.sics.prologbeans.PBTerm} is a proper list, returns its
	 * length.
	 * 
	 * @throws IllegalStateException if this
	 *         {@link se.sics.prologbeans.PBTerm} is not a proper list.
	 * @return the length of the list
	 */
	public int length() {
		throw new IllegalStateException("not a proper list: " + toString());
	}

	/**
	 * Returns the number of arguments of this compound term or 0 if this
	 * {@link se.sics.prologbeans.PBTerm} is not a compound term.
	 * @return the arity of the term
	 */
	public int getArity() {
		return 0;
	}

	/**
	 * Returns the integer value of this {@link se.sics.prologbeans.PBTerm}.
	 * 
	 * @return the integer value of the term
	 * @throws IllegalStateException if this
	 *         {@link se.sics.prologbeans.PBTerm} is not an integer.
	 */
	public long intValue() {
		throw new IllegalStateException("not an integer: " + toString());
	}

	/**
	 * Returns the {@link java.math.BigInteger BigInteger} value of this
	 * {@link se.sics.prologbeans.PBTerm}.
	 * 
	 * @return the big integer value of the term
	 * @throws IllegalStateException if this
	 *         {@link se.sics.prologbeans.PBTerm} is not a bignum integer.
	 * @see BigInteger
	 */
	public BigInteger bigIntegerValue() {
		throw new IllegalStateException("not an integer: " + toString());
	}

	/**
	 * Returns the floating-point value of this PBTerm.
	 * 
	 * @return the floating point value of the term
	 * @throws IllegalStateException if this
	 *         {@link se.sics.prologbeans.PBTerm} is not a number.
	 */
	public double floatValue() {
		throw new IllegalStateException("not a number: " + toString());
	}

	/**
	 * If this {@link se.sics.prologbeans.PBTerm} is a proper list and all its
	 * elements are Unicode character codes, either as integers or as one-character atoms,
	 * returns a
	 * {@link String} with the list elements as the character
	 * codes of the {@link String}.
	 * 
	 * @return the {@link String} corresponding to this list
	 * @throws IllegalStateException if this
	 *         {@link se.sics.prologbeans.PBTerm} is not a proper list.
	 */
	public String getString() {
		throw new IllegalStateException("not a proper list: " + toString());
	}

	/**
	 * For internal use by PrologBeans.
	 * 
	 * Returns a string representation of this term in a format that can be
	 * parsed by a Prolog parser.
	 */

	abstract String toPrologString();

	abstract void fastWrite(FastWriter writer) throws IOException;

	/**
	 * Returns a string description of this term.
	 */
	public abstract String toString();

	// -------------------------------------------------------------------
	// Static methods to create PBTerm instances
	// -------------------------------------------------------------------

	/**
	 * Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
	 * float value.
	 * @param value should be finite
	 * @return a term representing the argument
	 */
	static public PBTerm makeTerm(float value) {
		return new PBFloat(value);
	}

	/**
	 * Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
	 * double value.
	 * @param value should be finite
	 * @return a term representing the argument
	 */
	static public PBTerm makeTerm(double value) {
		return new PBFloat(value);
	}

	/**
	 * Creates a new {@link se.sics.prologbeans.PBTerm} instance representing an
	 * int value.
	 * @param value the value to represent
	 * @return a term representing the argument
	 */
	static public PBTerm makeTerm(int value) {
		return new PBInteger(value);
	}

	/**
	 * Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
	 * long value.
	 * @param value the value to represent
	 * @return a term representing the argument
	 */
	static public PBTerm makeTerm(long value) {
		return new PBInteger(value);
	}

	/**
	 * Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
	 * BigInteger value.
	 * @param value the non-null value to represent
	 * @return a term representing the argument
	 */
	static public PBTerm makeTerm(BigInteger value) {
		return new PBBignum(value);
	}

	/**
	 * Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
	 * list with the characters, as integer values, in the string argument as
	 * its elements.
	 * @param value the non-null value to represent
	 * @return a term representing the argument
	 */
	static public PBTerm makeTerm(String value) {
		// *** FIXME: This should create a list. Below code is stopgap.
		throw new UnsupportedOperationException("Not yet implemented");
		// return new PBAtom(value);
	}

	/**
	 * Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
	 * compound term.
	 * @param name the name of the term
	 * @param arguments the, non-empty, arguments of the term
	 * @return a term representing the argument
	 */
	static public PBTerm makeTerm(String name, PBTerm[] arguments) {
		// If the compound term being created is a list cell, create an
		// instance of PBListCell instead, making sure that its methods will
		// be called when the instance is used.

		// [PM] 4.1.3 Now arguments may not yet be filled in so must pass
		// arguments[] to constructor.
		if (name.equals(".") && arguments != null && arguments.length == 2) {
			return new PBListCell(arguments);
		}
		/*- if (name.equals(".") && arguments != null && arguments.length == 2) {
		return new PBListCell(arguments[0], arguments[1]);
		} */
		return new PBCompound(name, arguments);

	}

	/**
	 * Creates a new {@link se.sics.prologbeans.PBTerm} instance representing a
	 * list cell.
	 * @param head the first argument of the list cell
	 * @param tail the second argument of the list cell
	 * @return a term representing the argument
	 */
	static public PBTerm makeTerm(PBTerm head, PBTerm tail) {
		return new PBListCell(head, tail);
	}

	/**
	 * Creates a new {@link se.sics.prologbeans.PBTerm} instance representing an
	 * atom.
	 * @param value the non-null name of the atom
	 * @return a term representing the argument
	 */
	static public PBTerm makeAtom(String value) {
		if (value.equals("[]")) {
			return PBAtom.NIL;
		} else {
			return new PBAtom(value);
		}
	}

	// -------------------------------------------------------------------
	// Internal utilities
	// -------------------------------------------------------------------

	private int getFirstStuffing(String atom) {
		int len = atom.length();
		if (len == 0) {
			return 0;
		}
		char c = atom.charAt(0);
		if (c < 'a' || c > 'z') {
			return 0;
		}

		for (int i = 1; i < len; i++) {
			c = atom.charAt(i);
			if (!((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
					|| (c >= '0' && c <= '9') || (c == '_'))) {
				return i;
			}
		}
		return -1;
	}

	protected String stuffAtom(String atom) {
		int start = getFirstStuffing(atom);
		if (start < 0) {
			// No stuffing needed
			return atom;
		}

		int len = atom.length();
		char[] buf = new char[start + (len - start) * 2 + 2];
		int index = 1;
		buf[0] = '\'';
		if (start > 0) {
			atom.getChars(0, start, buf, 1);
			index += start;
		}
		for (int i = start; i < len; i++) {
			char c = atom.charAt(i);
			if (c == '\'') {
				buf[index++] = '\\';
			}
			buf[index++] = c;
		}
		buf[index++] = '\'';
		return new String(buf, 0, index);
	}

	/**
	 * Write fastrw prefix. Return array of arguments that should be written
	 * after the prefix. The returned array may be empty but never null. Note
	 * that the returned array may have no direct relationship to the arguments
	 * of the term, e.g. when writing a list in compact string notation.
	 * 
	 * @throws IOException
	 */
	// [PM] 4.1.3
	abstract PBTerm[] fastWritePrefix(FastWriter writer) throws IOException;

}
