/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Hashtable;

/**
 * {@link se.sics.prologbeans.Bindings} handles the variable bindings in the
 * communication with the prolog server. Using variable bindings ensures that
 * the values are properly quoted when sent to the prolog server.
 */
public class Bindings {

	private Hashtable bindings;

	/**
	 * Creates a new {@link se.sics.prologbeans.Bindings} instance with no
	 * variable bindings.
	 */
	public Bindings() {
		// [PD] 4.0.0 SPRM 9937 make sure bindings is not null.
		bindings = new Hashtable();
	}

	/**
	 * Creates a new {@link se.sics.prologbeans.Bindings} instance and copies
	 * all existing variable bindings from the specified bindings.
	 * 
	 * @param binds
	 *            the variable bindings to copy
	 */
	public Bindings(Bindings binds) {
		if (binds != null && binds.bindings != null) {
			bindings = (Hashtable) binds.bindings.clone();
		}
	}

	/**
	 * Adds the specified variable binding. The variable name must start with an
	 * upper case letter or '_'.
	 * 
	 * @param name
	 *            a prolog variable name
	 * @param intvalue
	 *            the value to bind to the variable
	 * @return a reference to this {@link se.sics.prologbeans.Bindings} object
	 * @throws IllegalArgumentException if the name is not
	 *         a valid prolog variable name
	 */
	public Bindings bind(String name, int intvalue) {
		checkVar(name);
		bindings.put(name, new PBInteger(intvalue));
		return this;
	}

	/**
	 * Adds the specified variable binding. The variable name must start with an
	 * upper case letter or '_'.
	 * 
	 * @param name
	 *            a prolog variable name
	 * @param longvalue
	 *            the value to bind to the variable
	 * @return a reference to this {@link se.sics.prologbeans.Bindings} object
	 * @throws IllegalArgumentException if the name is not
	 *         a valid prolog variable name
	 */
	public Bindings bind(String name, long longvalue) {
		checkVar(name);
		bindings.put(name, new PBInteger(longvalue));
		return this;
	}

	/**
	 * Adds the specified variable binding. The variable name must start with an
	 * upper case letter or '_'.
	 * 
	 * @param name
	 *            a prolog variable name
	 * @param floatvalue
	 *            the value to bind to the variable
	 * @return a reference to this {@link se.sics.prologbeans.Bindings} object
	 * @throws IllegalArgumentException if the name is not
	 *         a valid prolog variable name
	 */
	public Bindings bind(String name, float floatvalue) {
		checkVar(name);
		bindings.put(name, new PBFloat(floatvalue));
		return this;
	}

	/**
	 * Adds the specified variable binding. The variable name must start with an
	 * upper case letter or '_'.
	 * 
	 * @param name
	 *            a prolog variable name
	 * @param doublevalue
	 *            the value to bind to the variable
	 * @return a reference to this {@link se.sics.prologbeans.Bindings} object
	 * @throws IllegalArgumentException if the name is not
	 *         a valid prolog variable name
	 */
	public Bindings bind(String name, double doublevalue) {
		checkVar(name);
		bindings.put(name, new PBFloat(doublevalue));
		return this;
	}

	/**
	 * Adds the specified variable binding. The variable name must start with an
	 * upper case letter or '_'.
	 * 
	 * @param name
	 *            a prolog variable name
	 * @param stringvalue
	 *            the value to bind to the variable
	 * @return a reference to this {@link se.sics.prologbeans.Bindings} object
	 * @throws IllegalArgumentException if the name is not
	 *         a valid prolog variable name
	 */
	public Bindings bind(String name, String stringvalue) {
		checkVar(name);
		PBTerm val = createListFromString(stringvalue);
		bindings.put(name, val);
		return this;
	}

	// [PD] 4.0.0 Non-recursive version.
	private PBTerm createListFromString(String str) {
		if (str.length() == 0) {
			return PBTerm.NIL;
		} else {
			PBListCell pblc1 = new PBListCell(null, PBTerm.NIL);
			PBTerm pblc2 = pblc1;
			PBTerm pblct = pblc2;
			for (int i = 0; i < str.length(); i++) {
				pblct = pblc2;
				((PBListCell) pblc2).arguments[0] = new PBInteger(str.charAt(i));
				PBListCell pblc3 = new PBListCell(null, PBTerm.NIL);
				((PBListCell) pblc2).arguments[1] = pblc3;
				pblc2 = pblc3;
			}
			((PBListCell) pblct).arguments[1] = PBTerm.NIL;
			return pblc1;
		}
	}

	/**
	 * Adds the specified variable binding. The variable name must start with an
	 * upper case letter or '_'.
	 * 
	 * @param name
	 *            a prolog variable name
	 * @param termvalue
	 *            the value to bind to the variable
	 * @return a reference to this {@link se.sics.prologbeans.Bindings} object
	 * @throws IllegalArgumentException if the name is not
	 *         a valid prolog variable name
	 */
	public Bindings bind(String name, PBTerm termvalue) {
		checkVar(name);
		bindings.put(name, termvalue);
		return this;
	}

	/**
	 * Adds the specified variable binding. The variable name must start with an
	 * upper case letter or '_'. The value will be bound as an atom.
	 * 
	 * @param name
	 *            a prolog variable name
	 * @param atomvalue
	 *            the value to bind to the variable as an atom
	 * @return a reference to this {@link se.sics.prologbeans.Bindings} object
	 * @throws IllegalArgumentException if the name is not
	 *         a valid prolog variable name
	 */
	public Bindings bindAtom(String name, String atomvalue) {
		checkVar(name);
                // [PM] 4.2.1 guard against "[]"
                bindings.put(name, PBTerm.makeAtom(atomvalue));
                // bindings.put(name, new PBAtom(atomvalue));
		return this;
	}

	private void checkVar(String name) {
		char c = name.charAt(0);
		if (!Character.isUpperCase(c) && c != '_' || "_".equals(name)) {
			throw new IllegalArgumentException(
					"Variable names must start with uppercase letter or '_' : "
							+ name);
		}
		if (bindings == null) {
			bindings = new Hashtable();
		}
	}

	/**
	 * Returns the value for the specified variable or <code>null</code> if the
	 * variable is not bound.
	 * 
	 * @param name
	 *            the name of the variable
	 * @return the value of the variable as a {@link se.sics.prologbeans.PBTerm
	 *         PBTerm} or <code>null</code> if the variable is not bound
	 */
	public PBTerm getValue(String name) {
		if (bindings != null) {
			return (PBTerm) bindings.get(name);
		}
		return null;
	}

	public String toString() {
		Enumeration keys = bindings.keys();
		StringBuffer buffer = new StringBuffer();
		buffer.append('[');
		String key = null;
		while (keys.hasMoreElements()) {
			if (key != null) {
				buffer.append(',');
			}
			key = (String) keys.nextElement();
			PBTerm value = (PBTerm) bindings.get(key);
			buffer.append(key).append('=').append(value.toPrologString());
		}
		buffer.append(']');
		return buffer.toString();
	}

	void fastWrite(FastWriter writer) throws IOException {
		Enumeration keys = bindings.keys();
		String key = null;
		while (keys.hasMoreElements()) {
			key = (String) keys.nextElement();
			PBTerm value = (PBTerm) bindings.get(key);
			writer.writeList();
			writer.writeCompound("=", 2);
			// Arg 1
			writer.writeAtom(key);
			// Arg 2
			// [PM] 4.1.3 Use non-recursive write
			writer.writeTerm(value);
			// value.fastWrite(writer);
		}
		writer.writeNIL();
	}

}
