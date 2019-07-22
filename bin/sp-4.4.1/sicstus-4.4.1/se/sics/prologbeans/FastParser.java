/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */

package se.sics.prologbeans;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.Hashtable;

/*-
 * <code>FastParser</code>
 *
 * Documentation of the fast_write/read "protocol"
 *
 * Each "string" looks like:
 *
 * [PM] 3.10.2 Updated specification
 *
 * D TYPE [type-specific data]
 *
 * D -> start of fastrw Term (D is also the version number)
 *
 * TYPE, one byte for type:
 *  I - integer - argument is a arbitrary long, nonempty string of
 *     ASCII decimal digits, prefixed by a minus sign if negative
 *     There should be no plus sign prefixed. There is no size restriction.
 *  F - float - argument is an ASCII string of digits with a decimal
 *     point preceeded by at least one digit, optionally using
 *     E-notation (capital E) with exponent optionally prefixed by
 *     minus sign. The floating point number is prefixed by minus if
 *     negative
 *  A - atom - argument is an ATOMNAME (*)
 *     Make UTF-8 explicit (QP loses here). (Luckily this should be exactly
 *     the Java String.getbytes bytes.)
 *  _ - variable (followed by DATA which is a string of digits). The
 *      variables are numbered 0..n in depth first traversal
 *      increasing argument order. This is the change in version 'D'
 *      compared to version 'C'. The variable numbering is strictly defined.
 *
 * S ATOMNAME n - compound with n terms [followed by n terms] - n is
 *      an (unsigned) byte
 * " - a list prefix consisting of only integers in [1..255] (*)
 *     the sequence is terminated by zero byte followed by the tail
 *     term. Note that this is just a compact representation of
 *     nested compound terms with functor ./2.
 *     Example "ab", i.e., .(97, .(98, [])) becomes (using C
 *     char-constant notation) '"' 'a' 'b' '\0' ']'; [x,97,98|y]
 *     becomes '[' 'A' 'x' '\0' '"' 'a' 'b' '\0' 'A' 'y' '\0'
 > Clarified that this encoding is used for any "list"-prefix with
 > suitable elements. In particular it is not always followed by ']'.
 > Also note that the elements of this kind of list should *not* be
 > treated by the reader as individual bytes of an UTF-8 encoded
 > string. If a list is to be treated as a string then each element
 > should be treated as a single UNICODE character, this holds for
 > lists transmitted using any of the three ('"', '[' or 'S' '.' '\0'
 > '\2') possible serialization-formats.
 >
 * [ - a list cell, i.e, a shortcut for 'S' '.' '\0' '\2' (*)
 * ] - empty list, i.e, a shortcut for 'A' '[' ']' '\0' (*)
 *
 * DATA - a byte sequence terminated by '\0'
 *     NOTE: As of verson 'D' the numbering must be sequential from
 *           _0.._n, the prolog side fast_read will most likely
 *           crash if * this invariant is not maintained when
 *           sending data to * prolog.
 * ATOMNAME is DATA where the bytes make up the UTF-8 name of the
 *     atom.
 *
 *   (*) These could be optional in the writer since there are longer
 *       equivalent forms from which the reader could produce the same term.
 */

class FastParser {

	static final byte VERSION = (byte) 'D';
	static final byte INTEGER = (byte) 'I';
	static final byte FLOAT = (byte) 'F';
	static final byte ATOM = (byte) 'A';
	static final byte COMPOUND = (byte) 'S';
	static final byte VARIABLE = (byte) '_';
	static final byte STRING = (byte) '"';
	static final byte LIST = (byte) '[';
	static final byte NIL = (byte) ']';

	private Hashtable variableTable = null;

	public FastParser() {
	}

	public PBTerm parseProlog(InputStream stream) throws IOException {
		int c;
		try {
			if ((c = stream.read()) == VERSION) {
				return parseTerm(-1, stream);
			} else {
				// [PD] 4.0.5 We can't print to standard output here.
				// System.out.println("Not a fast prolog expression" + c);
				// [PD] 4.0.5 Stopgap solution until we implement a well
				// designed
				// exception strategy in Prologbeans.
				throw new IOException("Not a fast prolog expression" + c);
			}
		} finally {
			if (variableTable != null) {
				variableTable.clear();
			}
		}
		// return null;
	}

	private PBTerm parseTerm(int pch, InputStream stream) throws IOException,
			NumberFormatException {
		// [PM] 4.1.3 FIXME: remove pch argument
		// assert pch == -1;
		if (pch != -1) {
			throw new IOException("Parse error: illegal lookahead character "
					+ pch);
		}
		return parseTermWithStack(stream);
	}

	private static final boolean logging = false;

	static private class Work {
		// Next read term should go into args[i]
		int i;
		final PBTerm[] args;
		final Work next;

		public Work(PBTerm[] args, Work next) {
                    // assert args != null;
                    // assert args.length > 0;
			this.args = args;
			this.i = 0;
			this.next = next;
		}

		/**
		 * Update top of stack. Return the resulting stack, i.e. either 'this'
		 * or this.next.
		 */
		Work bump(PBTerm term) {
			// assert i < args.length;
			// assert term != null;
			args[i] = term;
			i++;
			if (i < args.length) {
				if (logging) {
					System.out.println("bump() returning this: " + this);
				}
				return this;
			}
			if (logging) {
				System.out.println("bump() returning next: " + next);
			}
			return next;
		}

		/*
		 * bump the term onto the stack and then push terms on the resulting
		 * stack. Never null.
		 */
		public Work bump(PBTerm term, PBTerm[] terms) {
			// Note that bump(term) may return null. This is why we do not use
			// something like bump(term).push(terms).
			Work tmp = new Work(terms, bump(term));
			if (logging) {
				System.out.println("bump() returning new: " + tmp);
			}
			return tmp;
		}

            // @Override
		public String toString() {
			return "[" + super.toString() + " at " + i + "/" + args.length
					+ "]";
		}
	}

	/*
	 * [PM] 4.1.3 Manage an explicit stack to avoid running out of Java stack
	 * (SPRM 11909)
	 */
	private PBTerm parseTermWithStack(InputStream stream) throws IOException,
			NumberFormatException {
		final Work outer = new Work(new PBTerm[1], null);
		Work stack = outer;
		do {
			int chr = stream.read();

			PBTerm term;

			switch (chr) {
			case INTEGER: {
				String val = getString(stream);
				// return new PBInteger(val);
				try {
					term = new PBInteger(Long.parseLong(val, 10), val);
				} catch (NumberFormatException nfe) {
					term = new PBBignum(new BigInteger(val, 10), val);
				}
				if (logging) {
					System.out.println("bump() INTEGER " + val);
				}
				stack = stack.bump(term);
			}
				break;
			case FLOAT: {
				String val = getString(stream);
				term = new PBFloat(Double.parseDouble(val), val);
				if (logging) {
					System.out.println("bump() FLOAT " + val);
				}
				stack = stack.bump(term);
			}
				break;
			case ATOM: {
				String val = getString(stream);
				term = new PBAtom(val);
				if (logging) {
					System.out.println("bump() ATOM " + val);
				}
				stack = stack.bump(term);
			}
				break;
			case VARIABLE: {
				String val = "" + '_' + getString(stream);
				if (variableTable == null) {
					variableTable = new Hashtable();
				}
				PBVariable var = (PBVariable) variableTable.get(val);
				if (var == null) {
					var = new PBVariable(val);
					variableTable.put(val, var);
				}
				term = var;
				if (logging) {
					System.out.println("bump() VARIABLE " + val);
				}
				stack = stack.bump(term);
			}
				break;
			case STRING: {
				byte[] val = getBytes(stream);
				PBTerm t = parseTerm(-1, stream);
				// Note that this builds the list from the end.
				for (int i = val.length - 1; i >= 0; i--) {
					// [PM] 4.3 anding to convert from signed byte to unsigned 0..255 integer
					t = new PBListCell(new PBInteger(val[i] & 0xFF), t);
				}
				term = t;
				if (logging) {
					System.out.println("bump() STRING " + val);
				}
				stack = stack.bump(term);
			}
				break;
			case LIST: {
				// term = parseList(stream);
				int noTerms = 2;
				PBTerm[] terms = new PBTerm[noTerms];
				term = new PBListCell(terms);
				if (logging) {
					System.out.println("bump() LIST ./2");
				}
				stack = stack.bump(term, terms);
			}
				break;
			case NIL: {
				term = PBTerm.NIL;
				if (logging) {
					System.out.println("bump() NIL");
				}
				stack = stack.bump(term);
			}
				break;
			case COMPOUND: {
				String val = getString(stream);
				int noTerms = stream.read();
				PBTerm[] terms = new PBTerm[noTerms];
				// [PM] 4.1.3 ensure we get a PBListCell even if writer sent it
				// as plain compound
				term = PBTerm.makeTerm(val, terms);
				// term = new PBCompound(val, terms);

				if (logging) {
					System.out
							.println("bump() COMPOUND " + val + "/" + noTerms);
				}
				stack = stack.bump(term, terms);
			}
				break;
			default:
				throw new IOException("Parse error: illegal character "
						+ (char) chr);
			}
		} while (stack != null);
		// assert outer != null;
		// assert outer.args != null && outer.args.length == 1;
		// assert validTerm(outer.args[0]);
		if (logging) {
			System.out.println("parseTermWithStack returning " + outer.args[0]);
		}
		return outer.args[0];
	}

	/**
	 * [PM] 4.1.3 Traverse the term and verify that no null arguments remain
	 * (for assertions).
	 */
	private boolean validTerm(PBTerm pbTerm) {
		if (pbTerm == null) {
			return false;
		}
		// FIXME: Traverse all args[] arrays (without running out of Java
		// stack!)
		return true;
	}

	// [PD] 3.12.1 Correct version, which handles UTF-8.
	// FIXME: For Java 5 (a.k.a. 1.5) we have to revise this.
	private String getString(InputStream stream) throws IOException {
		int c;
		ByteArrayOutputStream byteBuff = new ByteArrayOutputStream();
		while ((c = stream.read()) != '\0') {
			byteBuff.write(c);
		}
		return byteBuff.toString("UTF8");
	}

	private byte[] getBytes(InputStream stream) throws IOException {
		int c;
		ByteArrayOutputStream byteBuff = new ByteArrayOutputStream();
		while ((c = stream.read()) != '\0') {
			byteBuff.write(c);
		}
		// [PM] 4.3 SPRM 13863 J# is broken and uses
		// Windows-1252 rather than ISO-8859-1 when passed the encoding name "ISO-8859-1".
        // As a workaround we could use the deprecated hibyte API
		// (which is actually closer to our intent, anayway), i.e. byteBuff.toString(0).
		// But, since it does not make any sense to return a String here, anyway, we instead return what we mean, i.e. a byte array.
        
		// SPRM 13863 uses Windows-1252, so does not treats all byte values as themselves.
		// return byteBuff.toString("ISO-8859-1");
		return byteBuff.toByteArray();
	}
}
