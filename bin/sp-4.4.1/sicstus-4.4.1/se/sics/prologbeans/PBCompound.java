/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

import java.io.IOException;

/**
 * <code>PBCompound</code> is the
 * representation of Prolog compound terms.
 */
class PBCompound extends PBTerm {
	protected final PBTerm[] arguments;

	PBCompound(String name, PBTerm[] args) {
		super(name);
		this.arguments = args;
	}

	public boolean isAtom() {
		return getArity() == 0;
	}

	public boolean isAtomic() {
		return getArity() == 0;
	}

	public boolean isListCell() {
		return getArity() == 2 && ".".equals(getName());
	}

	public boolean isProperList() {
		// return (isListCell() && getArgument(2).isProperList())
		// || isEmptyList();
		return (isListCell() && getArgument(2).isProperList());
	}

	public PBTerm getArgument(int index) {
		if (arguments == null) {
			throw new IllegalStateException("not a compound term: "
					+ toString());
		}
		if (index < 1 || index > arguments.length) {
			throw new IndexOutOfBoundsException("Index: " + index
					+ ", needs to be between 1 and " + getArity());
		}
		return arguments[index - 1];
	}

	public int getArity() {
		return arguments == null ? 0 : arguments.length;
	}

	String toPrologString() {
		StringBuffer sb = new StringBuffer().append(stuffAtom(name));
		if (arguments != null) {
			sb.append('(');
			for (int i = 0, n = arguments.length; i < n; i++) {
				if (i > 0) {
					sb.append(',');
				}
				sb.append(arguments[i].toPrologString());
			}
			sb.append(')');
		}
		return sb.toString();
	}

	public String toString() {
		StringBuffer sb = new StringBuffer().append(name);
		if (arguments != null) {
			sb.append('(');
			for (int i = 0, n = arguments.length; i < n; i++) {
				if (i > 0) {
					sb.append(',');
				}
				sb.append((arguments[i] != null ? arguments[i].toString()
						: "<<NULL>>"));
			}
			sb.append(')');
		}
		return sb.toString();
	}

	void fastWrite(FastWriter writer) throws IOException {
		if (arguments != null) {
			writer.writeCompound(name, arguments.length);
			for (int i = 0, n = arguments.length; i < n; i++) {
				arguments[i].fastWrite(writer);
			}
		} else {
			throw new IllegalStateException("not a compound term: "
					+ toString());
		}
	}

	// @Override
	PBTerm[] fastWritePrefix(FastWriter writer) throws IOException {
		if (arguments != null) {
			writer.writeCompound(name, arguments.length);
			return arguments;
		} else {
			throw new IllegalStateException("not a compound term: "
					+ toString());
		}
	}

}
