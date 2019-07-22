/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

import java.io.IOException;

/**
 * <code>PBAtomic</code> is the representation
 * of Prolog constants.
 */

abstract class PBAtomic extends PBTerm {

	final static int ATOM = 1;
	final static int INTEGER = 2;
	final static int FLOAT = 3;
	final static int VARIABLE = 4;

	// private final int type;

	/**
	 * Creates a new <code>PBAtomic</code> instance with the specified name.
	 */
	PBAtomic(String name) {
		super(name);
	}

	abstract int getType();

	public boolean isAtom() {
		return false;
	}

	public boolean isInteger() {
		return false;
	}

	public boolean isFloat() {
		return false;
	}

	public boolean isAtomic() {
		return true;
	}

	abstract String toPrologString();

	abstract public String toString();

	void fastWrite(FastWriter writer) throws IOException {
		writer.writeAtomic(this);
	}

	// @Override
	PBTerm[] fastWritePrefix(FastWriter writer) throws IOException {
		writer.writeAtomic(this);
		return PBTerm.NO_TERMS;
	}

}
