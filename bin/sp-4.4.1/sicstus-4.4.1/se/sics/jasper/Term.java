/*
 * Copyright (c) 2002 SICS AB
 */

package se.sics.jasper;

//import se.sics.jasper.PrologException;

/**
 * Terms are Java representations of Prolog terms. See
 * {@link se.sics.jasper.SPTerm} for more information on the implementation.
 */
public interface Term {
	// *NOTE*
	// The TYPE codes must be synched with the corresponding codes
	// in SPTerm.java and in sicstus.h.
	/**
	 * The term is a variable.
	 * 
	 * @see Term#type
	 */
	int TYPE_VARIABLE = 1;

	/**
	 * The term is an integer.
	 * 
	 * @see Term#type
	 */
	int TYPE_INTEGER = 2;

	/**
	 * The term is an atom.
	 * 
	 * @see Term#type
	 */
	int TYPE_ATOM = 3;

	/**
	 * The term is a float.
	 * 
	 * @see Term#type
	 */
	int TYPE_FLOAT = 4;

	/**
	 * The term is a compound term.
	 * 
	 * @see Term#type
	 */
	int TYPE_COMPOUND = 5;

	/**
	 * Compares two terms according to standard order.
	 * 
	 * @param with
	 *            The term to compare with.
	 * @return -1 if <code>x &lt; y</code>, 0 if <code>x == y</code>, and 1 if
	 *         <code>x &gt; y</code>.
	 * @throws IllegalTermException
	 *             an illegal term was detected
	 * @throws Exception
	 *             something went wrong
	 */
	int compare(Term with) throws IllegalTermException, Exception;

	/*
	 * consFunctor and consList moved to Prolog.java Term consFunctor(String
	 * functor, Term[] args) throws PrologException, InterruptedException; Term
	 * consList(Term head, Term tail) throws PrologException,
	 * InterruptedException;
	 */

	/**
	 * Invalidates the Term object and make the SP_term_ref available for
	 * re-use. Accessing an invalid Term object throws IllegalTermException.
	 * 
	 * @throws Exception
	 *             something went wrong
	 **/
	void delete() throws Exception;

	/**
	 * Gets an argument from a compound term. Similar to calling
	 * <code>arg/3</code> with the third argument unbound.
	 * 
	 * @param i
	 *            The number of the argument.
	 * @param arg
	 *            The term to which the <code>i</code><i>th</i> argument will be
	 *            assigned.
	 * @return A reference to itself.
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             something went wrong
	 */
	Term getArg(int i, Term arg) throws InterruptedException, Exception;

	/**
	 * Obtains the value of the Prolog float.
	 *
	 * @return The value of the Prolog float as a Java <code>double</code>.
	 * @throws ConversionFailedException
	 *             The term could not be converted to a double.
	 * @throws IllegalTermException
	 *             an illegal term was detected
	 * @throws Exception
	 *             something went wrong
	 */
	double getDouble() throws ConversionFailedException, IllegalTermException,
			Exception;

	/**
	 * Obtains the arity of a functor.
	 *
	 * @return The arity of a functor as a Java <code>int</code>.
	 * @throws ConversionFailedException
	 *             The term could not be converted to the arity of a functor.
	 * @throws IllegalTermException
	 *             an illegal term was detected
	 * @throws Exception
	 *             something went wrong
	 */
	int getFunctorArity() throws ConversionFailedException,
			IllegalTermException, Exception;

	/**
	 * Obtains the name of a functor.
	 *
	 * @return The functor as a Java <code>String</code>.
	 * @throws ConversionFailedException
	 *             The term could not be converted to the name of a functor.
	 * @throws IllegalTermException
	 *             an illegal term was detected
	 * @throws Exception
	 *             something went wrong
	 */
	String getFunctorName() throws ConversionFailedException,
			IllegalTermException, Exception;

	/**
	 * Obtains the integer value of the Prolog term.
	 *
	 * @return The value of the Prolog term as a Java <code>long</code>.
	 * @throws Exception
	 *             something went wrong
	 */
	long getInteger() throws Exception;

	/**
	 * Gets the head and tail of a Prolog list.
	 * 
	 * @param head
	 *            The term which will be assigned the head
	 * @param tail
	 *            The term which will be assigned the tail
	 *
	 * @return A reference to itself.
	 * @throws InterruptedException
	 *             the thread was interrupted
	 * @throws Exception
	 *             something went wrong
	 */
	Term getList(Term head, Term tail) throws InterruptedException, Exception;

	/**
	 * Obtains the value of a list of characters as a string.
	 *
	 * @return The value of the list as a Java <code>String</code>.
	 * @throws Exception
	 *             something went wrong
	 */
	String getListChars() throws Exception;

	/**
	 * Obtains the value of a Prolog number as a string.
	 *
	 * @return The value of the number as a Java <code>String</code>.
	 * @throws Exception
	 *             something went wrong
	 */
	String getNumberChars() throws Exception;

	/**
	 * Returns the object encapsulated in the Prolog term. In multi-threaded
	 * Jasper, this method should only be called on terms created with the
	 * method {@link se.sics.jasper.Prolog#newObjectTerm}.
	 * <p>
	 * See {@link se.sics.jasper.SPTerm#getObject} for documentation on the
	 * single threaded implementation.
	 *
	 * @return A Java object
	 * @throws Exception
	 *             something went wrong
	 */
	Object getObject() throws Exception;

	/**
	 * Obtains the value of the Prolog atom as a string.
	 *
	 * @return The value of the Prolog atom as a Java <code>String</code>.
	 * @throws Exception
	 *             something went wrong
	 */
	String getString() throws Exception;

	/**
	 * Tests if the term is an atom.
	 * 
	 * @return <code>true</code> if the term is an atom; <code>false</code>
	 *         otherwise
	 * @throws Exception
	 *             something went wrong
	 */
	boolean isAtom() throws Exception;

	/**
	 * Tests if the term is atomic. A term is atomic if it is instantiated to an
	 * atom or a number.
	 * 
	 * @return <code>true</code> if the term is atomic; <code>false</code>
	 *         otherwise
	 * @throws Exception
	 *             something went wrong
	 */
	boolean isAtomic() throws Exception;

	/**
	 * Tests if the term is a compound term.
	 * 
	 * @return <code>true</code> if the term is a compound term;
	 *         <code>false</code> otherwise
	 * @throws Exception
	 *             something went wrong
	 */
	boolean isCompound() throws Exception;

	/**
	 * Tests if the term is the empty list.
	 * 
	 * @return <code>true</code> if the term is the atom <code>[]</code>;
	 *         <code>false</code> otherwise
	 * @throws Exception
	 *             something went wrong
	 */
	boolean isEmptyList() throws Exception;

	/**
	 * Tests if the term is a float.
	 * 
	 * @return <code>true</code> if the term is a float; <code>false</code>
	 *         otherwise
	 * @throws Exception
	 *             something went wrong
	 */
	boolean isFloat() throws Exception;

	/**
	 * Tests if the term is an integer.
	 * 
	 * @return <code>true</code> if the term is an integer; <code>false</code>
	 *         otherwise
	 * @throws Exception
	 *             something went wrong
	 */
	boolean isInteger() throws Exception;

	/**
	 * Tests if the term is a list cell. Equivalent to the Prolog code
	 * <code>isList(T) :- nonvar(T), T=[_|_].</code>
	 * 
	 * @return <code>true</code> if the term is a list cell; <code>false</code>
	 *         otherwise
	 * @throws Exception
	 *             something went wrong
	 */
	boolean isList() throws Exception;

	/**
	 * Tests if the term is a number. A number is a term instantiated to an
	 * integer or a float.
	 * 
	 * @return <code>true</code> if the term is a number; <code>false</code>
	 *         otherwise
	 * @throws Exception
	 *             something went wrong
	 */
	boolean isNumber() throws Exception;

	/**
	 * Tests if the Prolog term referenced is still accessible through this
	 * object. Term object becomes invalid as the result of of explicit
	 * {@link #delete delete}, closing, cutting or asking for the nextSolution
	 * of an enclosing SPQuery.
	 * 
	 * @return <code>true</code> if still valid; <code>false</code> otherwise.
	 * @throws Exception
	 *             something went wrong
	 */
	boolean isValid() throws Exception;

	/**
	 * Tests if the term is a variable.
	 * 
	 * @return <code>true</code> if the term is a variable; <code>false</code>
	 *         otherwise
	 * @throws Exception
	 *             something went wrong
	 */
	boolean isVariable() throws Exception;

	/*
	 * We don't want the put methods. Use unify instead. Term putEmptyList();
	 * Term putFloat(double value); Term putFloat(float value); Term
	 * putFunctor(String functor, int arity); Term putInteger(int value); Term
	 * putList(); Term putListChars(String string); Term putNumberChars(String
	 * string); Term putObject(Object obj); // *** We might want this one [PD]
	 * Term putString(String value); Term putTerm(Term new_term); Term
	 * putVariable();
	 */

	/**
	 * Returns a string-respresentation of a term.
	 */
	// String toString() throws Exception;
	String toString();

	/**
	 * Returns a string-representation of a term. This is an interface to
	 * write_term/3, which see.
	 * 
	 * @param options the options that are passet do <tt>write_term/3</tt>
	 * @return the written representation of the term
	 * @throws SPException
	 *             A Prolog exception was thrown.
	 * @throws Exception
	 *             something went wrong
	 */
	String toString(Term options) throws SPException, Exception;

	/**
	 * Converts a list to an array of Terms. Useful when examining lists
	 * returned from Prolog queries.
	 *
	 * This is of course only possible on a list.
	 * 
	 * @return An array of the terms.
	 * @throws Exception
	 *             something went wrong
	 */
	Term[] toPrologTermArray() throws Exception;

	/**
	 * Returns the type of this term. The return value is as defined by the
	 * constants <code>TYPE_INTEGER</code>, <code>TYPE_FLOAT</code>,
	 * <code>TYPE_ATOM</code>, <code>TYPE_VARIABLE</code>, and
	 * <code>TYPE_COMPOUND</code>
	 * 
	 * @return the term type of the receiver
	 * @throws Exception
	 *             something went wrong
	 *
	 * @see Term#TYPE_INTEGER
	 * @see Term#TYPE_ATOM
	 * @see Term#TYPE_FLOAT
	 * @see Term#TYPE_VARIABLE
	 * @see Term#TYPE_COMPOUND
	 * @see Term#isVariable
	 * @see Term#isInteger
	 * @see Term#isAtom
	 * @see Term#isFloat
	 * @see Term#isCompound
	 * @see Term#isList
	 * @see Term#isAtomic
	 * @see Term#isNumber
	 */
	int type() throws Exception;

	/**
	 * Unifies the term with another term.
	 * 
	 * @param with
	 *            The term with which to unify.
	 * @return True/False depending on whether the unification succeeded.
	 * @throws Exception
	 *             something went wrong
	 */
	boolean unify(Term with) throws Exception;
}
