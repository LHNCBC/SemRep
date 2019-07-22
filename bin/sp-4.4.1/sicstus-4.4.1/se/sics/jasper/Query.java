/*
 * Copyright (c) 2002 SICS AB
 */

package se.sics.jasper;

/**
 * Objects implementing this interface are used for holding a query-reference
 * which is used to find multiple solutions to a query.
 * 
 * @see se.sics.jasper.Prolog#openPrologQuery
 *
 */
public interface Query {
	/**
	 * Closes a query. All remaining solutions to the query are discarded, and
	 * the state of the Prolog engine will be restored to the state it was in
	 * when {@link se.sics.jasper.Prolog#openPrologQuery openPrologQuery} was
	 * called. If the query is not the most recently opened, all still open
	 * queries opened after this query will be closed as well.
	 * <p>
	 * A {@link se.sics.jasper.SPQuery#close closed} or
	 * {@link se.sics.jasper.SPQuery#cut cut} query is invalidated and most
	 * operations on it will throw an exception. In particular, you should not
	 * call <code>close</code>, <code>cut</code> or <code>nextSolution</code> on
	 * an invalidated query.
	 * <p>
	 * Invalidates all Term objects created since this query was created. If you
	 * need to keep data created by the query (i.e. data referred to by
	 * {@link se.sics.jasper.Term Term} objects), you need to convert it to Java
	 * datatypes before calling this function.
	 * 
	 * @throws NoSuchMethodException
	 *             Undocumented
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             something went wrong
	 **/
	void close() throws NoSuchMethodException, InterruptedException, Exception;

	/**
	 * Discards choices made since this query object was created, like the goal
	 * <code>!</code>.
	 * <p>
	 * If the query is not the most recently opened, all still open queries
	 * opened after this query will be cut as well.
	 * <p>
	 * A {@link se.sics.jasper.Query#close closed} or
	 * {@link se.sics.jasper.Query#cut cut} query is invalidated and most
	 * operations on it will throw an exception. In particular, you should not
	 * call <code>close</code>, <code>cut</code> or <code>nextSolution</code> on
	 * an invalidated query.
	 * <p>
	 * Invalidates all Term objects created since this query was created. If you
	 * need to keep data created by the query (i.e. data referred to by
	 * {@link se.sics.jasper.Term Term} objects), you need to convert it to Java
	 * datatypes before calling this function.
	 * 
	 * @throws NoSuchMethodException
	 *             Undocumented
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             something went wrong
	 **/
	void cut() throws NoSuchMethodException, InterruptedException, Exception;

	/**
	 * Gets the next solution for the query.
	 * 
	 * Returns <code>false</code> when there are no more solutions. When no more
	 * solutions are needed, the query must be closed using the method
	 * <code>close()</code>.
	 * <p>
	 * Multiple queries can be open at the same time, but the calls to
	 * nextSolution must be nested, i.e. refer to the most recently opened
	 * query. Invalidates all <code>Term</code> objects created since this query
	 * was created
	 * <p>
	 * Invalidates all SPTerm objects created since this query was created. If
	 * you need to keep data created by the previous call to
	 * <code>nextSolution</code> to this query (i.e. data referred to by
	 * {@link se.sics.jasper.Term Term} objects), you need to convert it to Java
	 * datatypes before calling this function.
	 * 
	 *
	 * @see se.sics.jasper.Prolog#openPrologQuery
	 * @see se.sics.jasper.Query#close
	 * @see se.sics.jasper.Query#cut
	 * 
	 * @return False if there are no more solutions, True otherwise.
	 * @throws NoSuchMethodException
	 *             Undocumented
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             something went wrong
	 */
	boolean nextSolution() throws NoSuchMethodException, InterruptedException,
			Exception;
}
