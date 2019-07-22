/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

import java.io.IOException;

/**
 * <code>PBVariable</code> is the
 * representation of Prolog variables.
 */

class PBVariable extends PBTerm {
	/**
	 * Creates a new <code>PBVariable</code> instance with the specified name.
	 */
	PBVariable(String name) {
		super(name);
	}

	int getType() {
		return PBAtomic.VARIABLE;
	}

	public boolean isVariable() {
		return true;
	}

	String toPrologString() {
		return toString();
	}

	public String toString() {
		return name;
	}

	void fastWrite(FastWriter writer) throws IOException {
		writer.writeVariable(this);
	}

	// @Override
	PBTerm[] fastWritePrefix(FastWriter writer) throws IOException {
		writer.writeVariable(this);
		return PBTerm.NO_TERMS;
	}
}
