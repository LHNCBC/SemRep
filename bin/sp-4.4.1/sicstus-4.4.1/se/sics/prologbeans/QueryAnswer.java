/**
 * Copyright (c) 2003 SICS AB. All rights reserved.
 */
package se.sics.prologbeans;

/**
 * <code>QueryAnswer</code> is the
 * representation of an answer from the Prolog server. The
 * <code>QueryAnswer</code> is returned by
 * {@link se.sics.prologbeans.PrologSession} in response to a query and contains
 * variable bindings, errors, and success/failure information. It also contains
 * the variable bindings specified in the query.
 */
public class QueryAnswer extends Bindings {

	// This term is on one of these forms:
	// - A list of the form "['='(VariableNameAsAtom,Value), ...]" (variable
	// bindings)
	// - The atom "no" (the prolog responded with 'no')
	// - The functor "error(ErrorReason)" (an error occurred)
	// [PM] 4.1.3 FIXME: synchronization story?
	private PBTerm answer;
	private boolean hasValues = false;
	private boolean bound = false;

	/**
	 * Creates a new <code>QueryAnswer</code> instance with the specified
	 * information.
	 * 
	 * @param answer
	 *            a {@link se.sics.prologbeans.PBTerm} value representing the
	 *            Prolog response
	 * @param bindings
	 *            the variable bindings for the query to which this is an answer
	 */
	public QueryAnswer(PBTerm answer, Bindings bindings) {
		super(bindings);
		this.answer = answer;
		// *** FIXME: this test is a bit contrived.
		// [PM] 4.2.1 FIXME: No need to check instanceof.
		hasValues = answer.isListCell() && answer instanceof PBListCell;
	} // QueryAnswer constructor

	/**
	 * Returns the value of the specified variable or <code>null</code> if the
	 * variable is not bound.
	 * 
	 * @param variable
	 *            the name of the variable
	 * @return the value of the variable as a {@link se.sics.prologbeans.PBTerm}
	 *         or <code>null</code> if the variable is not bound
	 */
	public PBTerm getValue(String variable) {
		if (!bound) {
			if (hasValues) {
				// [PM] 4.1.3 No longer rely on answer being a PBListCell

				// copy all the new bindings into Bindings
				PBTerm list;
				// [PM] 4.1.3 for type checking only. Barfs if not a proper
				// list. (FIXME: redundant)
				answer.length();
				for (list = answer; list.isListCell(); list = list
						.getArgument(2)) {
					PBTerm bindTerm = list.getArgument(1);
					if (bindTerm.getName().equals("=")) {
						bind(bindTerm.getArgument(1).getName(),
								bindTerm.getArgument(2));
					}
				}
				// assert list.isEmptyList();
			}

			bound = true;
		}
		return super.getValue(variable);
	}

	/**
	 * Returns <code>true</code> if the query failed (i.e. the Prolog responded
	 * with 'no') and <code>false</code> otherwise.
	 * @return whether the query failed
	 */
	public boolean queryFailed() {
		return !hasValues && answer.getName().equals("no");
	}

	/**
	 * Returns <code>true</code> if an error occurred while processing the query
	 * and <code>false</code> otherwise.
	 * @return whether the query threw an exception
	 */
	public boolean isError() {
		return !hasValues && answer.getName().equals("error");
	}

	/**
	 * Returns the error reason or <code>null</code> if an error has not
	 * occurred or if no error reason is known.
	 * @return the error, or null, if none
	 */
	public String getError() {
		if (answer.getName().equals("error")) {
			return answer.getArgument(1).toString();
		}
		return null;
	}

	/**
	 * Returns a string description of this answer.
	 */
	public String toString() {
		return answer.toString();
	}

} // QueryAnswer
