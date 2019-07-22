/*
 * Copyright (c) 2002 SICS AB
 */

package se.sics.jasper;

import java.util.Map;

/**
 * The class {@link se.sics.jasper.SICStus} and an inner class in
 * {@link se.sics.jasper.Jasper} inherit from this interface. These methods
 * should be used to call SICStus runtime in both multi threaded and single
 * threaded mode. When using the Prolog-Java interface in single thread mode,
 * the methods of this interface are preferred over the old methods of the class
 * {@link se.sics.jasper.SICStus}.
 */
public interface Prolog {
	/**
	 * For internal use by Jasper.
	 * 
	 * @return the server
	 */
	Server getServer();

	/**
	 * Opens a query, specified as a string, for obtaining multiple solutions.
	 *
	 * The method itself does not find any solutions; use the method
	 * {@link se.sics.jasper.Query#nextSolution nextSolution} to do that. When
	 * no more solutions are needed, the query must be closed using methods
	 * {@link se.sics.jasper.Query#close close} or
	 * {@link se.sics.jasper.Query#cut cut} on the query-object.
	 * 
	 * <p>
	 * 
	 * Multiple queries can be open at the same time. See
	 * {@link se.sics.jasper.Query#nextSolution nextSolution} for details and
	 * restrictions.
	 * 
	 * @param string
	 *            The goal to use for the query, with terminating period.
	 * @param varMap
	 *            The arguments to the predicate as a map from variable names to
	 *            Term objects. The map will get entries added for all variables
	 *            (with names not starting with underscore) in the goal that
	 *            were not already present as input. May be null.
	 *
	 * @return The opened {@link se.sics.jasper.Query query} object.
	 * @throws NoSuchMethodException
	 *             Undocumented
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 * @see se.sics.jasper.Query#nextSolution
	 * @see se.sics.jasper.Query#close
	 * @see se.sics.jasper.Query#cut
	 *
	 */
	Query openPrologQuery(String string, Map varMap)
			throws NoSuchMethodException, InterruptedException, Exception;

	/**
	 * Finds the first solution to a query specified as a string. If you need
	 * more than one solution, use {@link se.sics.jasper.Prolog#openPrologQuery
	 * openPrologQuery}.
	 * 
	 * @param string
	 *            The goal to use for the query, with terminating period.
	 * @param varMap
	 *            The arguments to the predicate as a map from variable names to
	 *            Term objects. On success varMap will get entries added for all
	 *            variables (with names not starting with underscore) in the
	 *            goal that were not already present as input. May be null.
	 *
	 * @return True/false corresponding to success/failure of the query.
	 * @throws NoSuchMethodException
	 *             Undocumented
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 * @see se.sics.jasper.Prolog#openPrologQuery
	 * @see se.sics.jasper.Prolog#queryCutFail
	 * @see se.sics.jasper.Term
	 */
	boolean query(String string, Map varMap) throws NoSuchMethodException,
			InterruptedException, Exception;

	/**
	 * Finds the first solution to a query specified as a string, then cuts away
	 * any choicepoints and fails, i.e. ignores everything but the side-effects
	 * during the first solution.
	 *
	 * @param string
	 *            The goal to use for the query, with terminating period.
	 * @param varMap
	 *            The arguments to the predicate as a map from variable names to
	 *            Term objects. May be null. No bindings are added.
	 *
	 * @return True/false corresponding to success/failure of the query.
	 * @throws NoSuchMethodException
	 *             Undocumented
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 * @see se.sics.jasper.Prolog#query
	 *
	 */
	boolean queryCutFail(String string, Map varMap)
			throws NoSuchMethodException, InterruptedException, Exception;

	// Term creating methods:
	/**
	 * Creates a null-term (i.e. the empty list, '[]').
	 * 
	 * @return an empty list term
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term newTerm() throws InterruptedException, Exception;

	/**
	 * Creates a term initialized to an existing term. Modifications to the
	 * created Term does not affect the input Term.
	 * 
	 * @param t
	 *            The Term object whose term to use
	 * @return a new term
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term newTerm(Term t) throws InterruptedException, Exception; // spterm

	/**
	 * Creates a term initialized to a Prolog integer from a Java
	 * <code>int</code>.
	 * 
	 * @param i
	 *            Initial value of the integer.
	 * @return a new term representing the argument
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term newTerm(int i) throws InterruptedException, Exception; // integer

	/**
	 * Creates a term initialized to a Prolog integer from a Java
	 * <code>long</code>.
	 * 
	 * @param j
	 *            Initial value of the integer (long).
	 * @return a new term representing the argument
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term newTerm(long j) throws InterruptedException, Exception; // integer

	/**
	 * Creates a term initialized to a Prolog float.
	 * 
	 * @param d
	 *            Initial value of the float.
	 * @return a new term representing the argument
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term newTerm(double d) throws InterruptedException, Exception; // float

	/**
	 * Creates a term initialized to a Prolog float.
	 * 
	 * @param f
	 *            Initial value of the float.
	 * @return a new term representing the argument
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term newTerm(float f) throws InterruptedException, Exception; // float

	/**
	 * Creates a term initialized to a Prolog atom.
	 * 
	 * @param a
	 *            String describing the initial value of the atom.
	 * @return a new term representing the argument
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term newTerm(String a) throws InterruptedException, Exception; // atom

	/**
	 * Creates a term initialized to a Prolog compound term.
	 * 
	 * @param functor
	 *            The functor of the compound term as a string
	 * @param args
	 *            The arguments as an array of Terms
	 * @return a new term representing the argument
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term newTerm(String functor, Term args[]) throws InterruptedException,
			Exception; // functor

	/**
	 * Creates a term by reading from a string.
	 * 
	 * @param string
	 *            The printed representation of the term, with terminating
	 *            period.
	 * @param varMap
	 *            Bindings for any variables occurring in the term, as a map
	 *            from variable names to Term objects. The map will get entries
	 *            added for all variables (with names not starting with
	 *            underscore) in the term that were not already present as
	 *            input. May be null.
	 * @return a new term representing the arguments
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term prologReadFromString(String string, java.util.Map varMap)
			throws InterruptedException, Exception;

	/**
	 * Creates a term initialized to an unbound variable.
	 * 
	 * @return a new term representing the argument
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term newVariable() throws InterruptedException, Exception; // Var

	/**
	 * Creates a term initialized to a compound term.
	 * 
	 * @param functor
	 *            The term's name as a Java <code>String</code>
	 * @param args
	 *            The arguments of the term
	 * @return a new callable term representing the arguments
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term consFunctor(String functor, Term[] args) throws InterruptedException,
			Exception;

	/**
	 * Creates a term initialized to a list with given head and tail.
	 * 
	 * @param head
	 *            The head of the list
	 * @param tail
	 *            The tail of the list
	 * @return a new list cell
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             some other problem happened
	 */
	Term consList(Term head, Term tail) throws InterruptedException, Exception;

	/**
	 * Creates a term initialized to a *global* reference to a Java object.
	 * 
	 * @param obj
	 *            A reference to a Java object.
	 * @return a new global reference term
	 * @throws IllegalTermException
	 *             an invalid term was detected
	 * @throws ConversionFailedException
	 *             the argument could not be converted to a term
	 * @throws Exception
	 *             some other problem happened
	 */
	// [PD] 3.9.2 SPRM 3141
	Term newObjectTerm(Object obj) throws IllegalTermException,
			ConversionFailedException, Exception;

	/**
	 * Creates a term initialized to a Prolog number from a string.
	 *
	 * @param str
	 *            The string containing the printed representation of the
	 *            number.
	 * @return a term representing the argument
	 * @throws ConversionFailedException
	 *             the argument could not be converted to a term
	 * @throws IllegalTermException
	 *             an invalid term was detected
	 * @throws Exception
	 *             some other problem happened
	 */
	// [PD] 3.9.2 SPRM 3141
	Term numberFromString(String str) throws ConversionFailedException,
			IllegalTermException, Exception;

	/**
	 * Creates a term initialized to a list of character codes from a Java
	 * <code>String</code>.
	 *
	 * @param str
	 *            The string of characters.
	 * @return a term representing the argument
	 * @throws ConversionFailedException
	 *             the argument could not be converted to a term
	 * @throws IllegalTermException
	 *             an invalid term was detected
	 * @throws Exception
	 *             some other problem happened
	 */
	// [PD] 3.9.2 SPRM 3141
	Term listFromString(String str) throws ConversionFailedException,
			IllegalTermException, Exception;

}
