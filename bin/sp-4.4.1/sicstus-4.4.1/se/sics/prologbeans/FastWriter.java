/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */

package se.sics.prologbeans;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Hashtable;

/**
 * <code>FastWriter</code>
 */
class FastWriter {

	// [PD] 3.12.1 UTF-8 is required to make SICStus happy.
	private final static String ENCODING = "UTF8";

	private OutputStream output;
	private boolean isWritingTerm = false;
	private Hashtable variableTable = null;

	public FastWriter(OutputStream output) {
		this.output = output;
	} // FastWriter constructor

	private void initOutput() throws IOException {
		if (!isWritingTerm) {
			isWritingTerm = true;
			output.write(FastParser.VERSION);
		}
	}

	public void writeCompound(String name, int arguments) throws IOException {
		initOutput();
		output.write(FastParser.COMPOUND);
		// [PD] 3.12.1 Correct encoding is required to make SICStus happy.
		output.write(name.getBytes(ENCODING));
		output.write(0);
		output.write(arguments);
	}

	public void writeList() throws IOException {
		initOutput();
		output.write(FastParser.LIST);
	}

	public void writeNIL() throws IOException {
		initOutput();
		output.write(FastParser.NIL);
	}

	// [PD] 3.12.3 Correct version which handles character codes > 255
	public void writeString(String value) throws IOException {
		initOutput();
		// writeCharList(value, false); // recursive version
		// output.write(0);
		writeCharList(value);
		output.write(FastParser.NIL);
	}

	// [PD] 4.0.0 Non-recursive version.
	private void writeCharList(String value) throws IOException {
		boolean in_compact_list = false;
		for (int i = 0; i < value.length(); i++) {
			char c = value.charAt(i);
			if (0 < c && c < 256) {
				if (!in_compact_list) {
					output.write(FastParser.STRING);
				}
				output.write((int) c);
				in_compact_list = true;
			} else {
				if (in_compact_list) {
					output.write(0);
				}
				output.write(FastParser.LIST);
				output.write(FastParser.INTEGER);
				output.write(Integer.toString(c).getBytes());
				output.write(0);
				in_compact_list = false;
			}
		}
		if (in_compact_list) {
			output.write(0);
		}
	}

	public void writeAtom(String value) throws IOException {
		initOutput();
		output.write(FastParser.ATOM);
		// [PD] 3.12.1 Correct encoding is required to make SICStus happy.
		output.write(value.getBytes(ENCODING));
		output.write(0);
	}

	public void writeAtomic(PBAtomic atomic) throws IOException,
			IllegalStateException {
		initOutput();

		int type = atomic.getType();
		switch (type) {
		case PBAtomic.ATOM:
			output.write(FastParser.ATOM);
			break;
		case PBAtomic.FLOAT:
			output.write(FastParser.FLOAT);
			break;
		case PBAtomic.INTEGER:
			output.write(FastParser.INTEGER);
			break;
		default:
			throw new IllegalStateException("illegal type: " + type);
		}
		// [PD] 3.12.1 Correct encoding is required to make SICStus happy.
		output.write(atomic.getName().getBytes(ENCODING));
		output.write(0);
	}

	public void writeVariable(PBVariable var) throws IOException {
		// [PD] 4.0.2+
		// frw_read_term() in fastrw.c expects only numbers after the variable
		// prefix.
		// All variables in variableTable are of the form _<number> so we cannot
		// have
		// the variable prefix in the name in variableTable as previously.
		output.write(FastParser.VARIABLE);
		if (variableTable == null) {
			variableTable = new Hashtable();
		}
		String variableName = (String) variableTable.get(var);
		if (variableName == null) {
			variableName = Integer.toString(variableTable.size());
			variableTable.put(var, variableName);
		}
		// [PD] 3.12.1 Correct encoding is required to make SICStus happy.
		output.write(variableName.getBytes(ENCODING));
		output.write(0);
	}

	public void commit() throws IOException {
		output.flush();
		isWritingTerm = false;
		if (variableTable != null) {
			variableTable.clear();
		}
	}

	public void close() throws IOException {
		commit();
		this.output.close();
	}

	static private class WriteWork {
		private int i;
		private final PBTerm[] args;
		private final WriteWork next;

		public WriteWork(PBTerm[] args, WriteWork next) {
                    // assert args != null;
                    // assert args.length > 0;
			this.i = 0;
			this.args = args;
			this.next = next;
		}

		/**
		 * Get next term to process at this level or null. If null is returned
		 * then use getNext() to pop this level
		 */
		PBTerm bump() {
			if (i < args.length) {
				return args[i++];
			}
			return null;
		}

		public WriteWork getNext() {
			return next;
		}

		public WriteWork push(PBTerm[] args) {
                    // assert args != null && args.length > 0;
			return new WriteWork(args, this.prune());
		}

		/**
		 * Pop exhausted stack entries and return the resulting stack (which may
		 * be null).
		 */
		private WriteWork prune() {
			WriteWork tmp = this;
			while (tmp != null && tmp.i == tmp.args.length) {
				tmp = tmp.getNext();
			}
			return tmp;
		}
	}

	void writeTerm(PBTerm term) throws IOException {
		WriteWork stack = new WriteWork(new PBTerm[] { term }, null);
		do {
			PBTerm t = stack.bump();
			if (t != null) {
				PBTerm[] args = t.fastWritePrefix(this);
				if (args.length > 0) {
					stack = stack.push(args);
				}
			} else {
				// top-most entry exhausted, pop it.
				stack = stack.prune();
			}
		} while (stack != null);
	}

} // FastWriter
