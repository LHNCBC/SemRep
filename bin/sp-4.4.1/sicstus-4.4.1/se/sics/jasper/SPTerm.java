/* 
 * Copyright (c) 1998 SICS 
 */

package se.sics.jasper;

import java.util.HashMap;

/**
 * SPTerms are Java representations of Prolog terms. SPTerms do <i>not</i>
 * contain Prolog terms themselves; Prolog terms are <i>always</i> stored on the
 * Prolog heap. Instead, an SPTerm object contains an <i>SP_term_ref</i> which
 * is an index into a stack maintained by Prolog. This stack in turn contains
 * references into the Prolog heap. For more information, consult the section on
 * "Mixing Java and Prolog" in the SICStus Prolog User's Manual.
 * 
 * <p>
 * In other words, SPTerm objects are only handles to SP_term_refs. The actual
 * life of an SP_term_ref (and of the Prolog term) are completely independent of
 * the life of an SPTerm object. So, if a SPTerm object is garbage collected,
 * the SP_term_ref and the Prolog term may very well be alive and well inside
 * the Prolog stack. On the other hand, just because an SPTerm object is not
 * GC:d, that does not mean that the Prolog term is still alive. The life-span
 * of SP_term_refs and Prolog terms are explained in detail in the User's Manual
 * Section "SPTerm and Memory".
 *
 * <p>
 * The methods in this class are encapsulations of the C-functions in the
 * C-Prolog interface which accesses SP_term_refs and their underlying Prolog
 * terms.
 *
 * <p>
 * You should not inherit from <code>SPTerm</code>.
 */

public class SPTerm implements Term {
	// Object dummy [] = new Object[100];; // [PM] DUMMY REMOVE THIS BEFORE
	// RELEASE

	SICStus sp;

	// Do not access this directly (from glue code for example) use
	// GetNativeTermRef() instead (that performs CheckValid())
	long nativeTermRef = SP_ILLEGAL_TERM_REF; // contains an SP_term_ref

	// [PM] 3.8.5 The next in the stack of term references (top of
	// stack is sp.term_stack)
	//
	SPTerm next;

	// value of sp.query_stack at creation time. Used to determine
	// this.age() Also indentifies the SPQuery.freeList to put our
	// nativeTermRef when this.delete() is called.
	//

	SPQuery context;

	// See termAge() how we approximate this
	// int termAgeTick; // Age of term. Should never be younger (larger integer)
	// than this.age()

	// DBG
	// need to use getName() when printing Thread objects since
	// Thread.toString() has a Thread-unsafe race condition!
	Thread creating_thread = null;
	// String creating_thread_name = null;

	boolean fromGlue;

	/**
	 * The term is a variable.
	 * 
	 * @see SPTerm#type
	 */
	public static final int SP_TYPE_VARIABLE = 1;

	/**
	 * The term is an integer.
	 * 
	 * @see SPTerm#type
	 */
	public static final int SP_TYPE_INTEGER = 2;

	/**
	 * The term is an atom.
	 * 
	 * @see SPTerm#type
	 */
	public static final int SP_TYPE_ATOM = 3;

	/**
	 * The term is a float.
	 * 
	 * @see SPTerm#type
	 */
	public static final int SP_TYPE_FLOAT = 4;

	/**
	 * The term is a compound term.
	 * 
	 * @see SPTerm#type
	 */
	public static final int SP_TYPE_COMPOUND = 5;

	static final long SP_ILLEGAL_TERM_REF = 0; // [PM] 3.8.5 signifies Prolog
												// deallocated this SP_term_ref

	// [PM] 3.9 moved to SICSTus
	// private native String spGetListChars( SICStus sp, long term ) throws
	// NativeCodeException;
	// private native String spGetNumberChars( SICStus sp, long term ) throws
	// NativeCodeException;
	// private native String spGetString( SICStus sp, long term ) throws
	// NativeCodeException;
	// private native String spGetStringFromAtom( SICStus sp, long canonical );
	// private native boolean spIsAtomic( SICStus sp, long term );
	// private native boolean spIsList( SICStus sp, long term );
	// private native boolean spIsNumber( SICStus sp, long term );
	// private native boolean spUnify( SICStus sp, long term1, long term2 );
	// private native double spGetFloat( SICStus sp, long termref ) throws
	// NativeCodeException;
	// private native Object spGetObject( SICStus sp, long termref ) throws
	// NativeCodeException;
	// private native int spCompare( SICStus sp, long term1, long term2 );
	// private native void spConsFunctor( SICStus sp, long term, long atm, long
	// args[]) throws NativeCodeException;
	// private native void spConsList( SICStus sp, long termref, long head, long
	// tail ) throws NativeCodeException;
	// private native int spGetFunctorArity( SICStus sp, long term ) throws
	// NativeCodeException;
	// private native void spPutFloat( SICStus sp, long termref, double d )
	// throws NativeCodeException;
	// private native void spPutFunctor( SICStus sp, long termref, long atm, int
	// arity) throws NativeCodeException;
	// private native void spPutInteger( SICStus sp, long termref, long i )
	// throws NativeCodeException;
	// private native void spPutList( SICStus sp, long termref ) throws
	// NativeCodeException;
	// private native void spPutListChars( SICStus sp, long termref, String
	// string ) throws NativeCodeException;
	// private native void spPutNumberChars( SICStus sp, long termref, String
	// string ) throws NativeCodeException;
	// private native void spPutString( SICStus sp, long termref, String value )
	// throws NativeCodeException;
	// private native void spPutVariable( SICStus sp, long termref ) throws
	// NativeCodeException;
	// private native int spTermType( SICStus sp, long term);
	// private native long spCreateGlobalRef( SICStus sp, Object obj );
	// private native void spGetArg( SICStus sp, long i, long term, long arg )
	// throws NativeCodeException;
	// private native long spGetAtom( SICStus sp, long term ) throws
	// NativeCodeException;
	// private native long spGetAtomFromString( SICStus sp, String string );
	// private native long spGetFunctorCanonical( SICStus sp, long term ) throws
	// NativeCodeException;
	// private native long spGetInteger( SICStus sp, long termref ) throws
	// NativeCodeException;
	//
	// // moved to SICStus private native long spMakeTermRef( SICStus sp );
	//
	// // private native void spDeleteGlobalRef( SICStus sp, long globref );
	// // private native void spDeleteLocalRef( SICStus sp, long ref );
	//
	// private native void spGetList( SICStus sp, long term, long head, long
	// tail ) throws NativeCodeException;
	// private native void spPutAtom( SICStus sp, long termref, long canonical )
	// throws NativeCodeException;
	// private native void spPutTerm( SICStus sp, long term1, long term2 )
	// throws NativeCodeException;
	// // private native void spRegisterAtom( SICStus sp, long canonical );
	// // private native void spUnRegisterAtom( SICStus sp, long canonical );
	//
	// private native String spPrintVariable( SICStus sp, long termref);

	private boolean debugging(int minLevel) {
		return (minLevel > 0) && (sp != null) && sp.debugging(minLevel);
	}

	private boolean debugging() {
		return debugging(1);
	}

	private boolean shouldCheckAge() {
		return sp.shouldCheckAge();
	}

	// [PM] 3.9.1b4 The SPTerm constructors in 3.9.0 was not declared to throw
	// IllegalCallerException (or even SPException).
	// To maintain source compatibility we fake out and throw a
	// NullPointerException instead.
	private void legalCallerOrThrowNullPointerException(SICStus sp) {
		if (!sp.isLegalCaller()) {
			if (debugging()) {
				// [PD] 3.9.2
				// SICStus.dbgPrintln("*** ERROR: creating an SPTerm is the wrong thread");
				SICStus.dbgPrintln("*** ERROR: creating or accessing an SPTerm in the wrong thread");
			}
			// [PD] 3.9.2
			// throw new
			// NullPointerException("Creating SPTerm in wrong thread. (IllegalCallerException in disguise.)");
			throw new NullPointerException(
					"Creating or accessing SPTerm in wrong thread. (IllegalCallerException in disguise.)");
		}
	}

	// [PM] 3.9.1 Will do legalCallerOrThrowNullPointerException
	private void initSPTerm(SICStus sp, long nativeTermRef) {
		legalCallerOrThrowNullPointerException(sp);
		initSPTerm(sp, nativeTermRef, false/* !fromGlue */);
	}

	// [PM] 3.9.1 Caller should have called
	// legalCallerOrThrowNullPointerException
	private void initSPTerm(SICStus sp, long nativeTermRef, boolean fromGlue) {
		if (debugging(2)) {
			SICStus.dbgPrintln("Enter: " + superToString() + ".initSPTerm("
					+ sp + ", " + nativeTermRef + ", " + fromGlue + ")");
		}

		synchronized (sp) {

			this.nativeTermRef = nativeTermRef;
			this.sp = sp;
			if (debugging(2)) {
				this.creating_thread = Thread.currentThread();
				// this.creating_thread_name = this.creating_thread.toString();
				SICStus.dbgPrintln("Created " + (fromGlue ? "glue " : "")
						+ "SPTerm " + superToString() + " nativeTermRef=="
						+ nativeTermRef + " in Thread "
						+ this.creating_thread.getName());
			}
			this.fromGlue = fromGlue;

			this.next = sp.term_stack;
			sp.term_stack = this;

			this.context = sp.query_stack; // the age of this SPTerm object,
											// also the freelist to use
			if (this.context != null
					&& this.context.term_stack_mark != null
					&& !(this.context.term_stack_mark.nativeTermRef < nativeTermRef)) {
				String msg = "** ERROR: Internal Jasper Error: "
						+ "! (this.context.term_stack_mark.nativeTermRef ("
						+ this.context.term_stack_mark.nativeTermRef
						+ ") < nativeTermRef (" + nativeTermRef + ")";
				if (debugging()) {
					SICStus.dbgPrintln(msg);
					dumpTermStack();
					sp.query_stack.dumpQueryStack();
				}

				throw new NullPointerException(msg); // Hack to avoid declaring
														// that we can throw
														// exceptions
			}
			if (debugging(2)) {
				SICStus.dbgPrintln("Exit: " + superToString() + ".initSPTerm("
						+ sp + ", " + nativeTermRef + ", " + fromGlue + ")");
			}

		}
	}

	String superToString() {
		return super.toString();
	}

	void dumpTermStack() {
		SICStus.dbgPrintln("Term stack dump:");
		SPTerm x = this;
		for (x = this; x != null; x = x.next) {
			SICStus.dbgPrintln("Term " + x.superToString() + " nativeTermRef="
					+ x.nativeTermRef + " context=" + x.context);
		}
	}

	// [PM] Will do legalCallerOrThrowNullPointerException
	private void initSPTerm(SICStus sp) {
		initSPTerm(sp, false/* !fromGlue */);
	}

	private void initSPTerm(SICStus sp, boolean fromGlue) {

		legalCallerOrThrowNullPointerException(sp);
		synchronized (sp) {
			// long termRef = spMakeTermRef(sp);
			long termRef = sp.makeTermRef();
			initSPTerm(sp, termRef, fromGlue);
		}
	}

	final int age() {
		// 3.8.6 context == null for the InitialSPTerm()
		// the age of the empty term stack is zero
		return ((context == null) ? 0 : context.age());
	}

	final int termAge() {
		// Ideally we should keep track of the age of the referred term
		// separately.
		// The way to do this is:
		// 1. in putTerm(t): set this.termAgeTick to t.termAge()
		// 2. in putAtom, putFunctor (arity zero): set this.termAgeTick to zero
		// (very old)
		// 3. in all other putXXX set termAgeTick to this.age();
		// (In this case, if check-age is turned off, then sp.age()
		// may be younger than this.age() so another reasonable
		// alternative is to use sp.age(). )
		// For now approximate this.termAge() with this.age(). This
		// can make termAge() too old and cause CheckPut to barf
		// unnecessarily.
		// Note: If we always perform the age check except at putTerm
		// then it seems unlikely that we should barf for any safe
		// use. (It is possible though)

		// the age of the term nativeTermRef refers to
		return age();
	}

	// Is it OK to create a heap term and put it into 'this'? Throw if not
	final void CheckPut() throws IllegalTermException {
		// barf if 'this' is older than the current choice point
		if (age() < sp.age()) {
			if (debugging()) {
				SICStus.dbgPrintln("** Warning " + "putting term (age "
						+ sp.age() + ") into SPTerm (age " + age() + ")");
			}
			if (shouldCheckAge()) {
				throw new IllegalTermException(sp);
			}
		}
	}

	// Is it OK to create a heap term and put it into 'this'? Throw if not
	final void CheckPut(SPTerm t) throws IllegalTermException {
		// barf if 'this' is older than 't'
		if (age() < t.termAge()) {
			if (debugging()) {
				SICStus.dbgPrintln("** Warning " + "putting SPTerm (age="
						+ t.age() + ", termAge=" + t.termAge()
						+ ") into SPTerm (age " + age() + ")");
			}
			if (shouldCheckAge()) {
				throw new IllegalTermException(sp);
			}
		}
	}

	// must be called while synchronized on sp
	void unlink() throws SPException {
		sp.term_stack_unlink(this,
				(context == null ? sp.query_stack : context.predecessor()));
		this.next = null;
	}

	/*
	 * Comment for when it was off by default <p> Experimental feature: If the
	 * system property <code>se.sics.jasper.SICStus.reuseTermRefs</code> is
	 * <code>true</code> then the Prolog side SP_term_ref is made available for
	 * reuse. A future release will make reuse the default.
	 * 
	 * @see se.sics.jasper.SICStus#setReuseTermRefs
	 */
	/**
	 * Invalidates the SPTerm object and makes the SP_term_ref available for
	 * re-use. Accessing an invalid SPTerm object throws IllegalTermException.
	 * 
	 * @see se.sics.jasper.SICStus#setReuseTermRefs
	 **/
	public void delete() throws SPException {
		delete(sp.reuseTermRefs());
	}

	void delete(boolean reuse) throws SPException {
		synchronized (sp) {
			// multiple delete and delete an invalidated is currently OK
			if (!isValid())
				return;
			// [PD] 3.10.1 Check legal caller too
			legalCallerOrThrowNullPointerException(sp);

			if (fromGlue) {
				throw new SPException(sp, 0, "Attempt to delete glue SPTerm");
			}

			long termRef = nativeTermRef;

			invalidate(); // zaps nativeTermRef

			if (!reuse) {
				return;
			}

			// remove this from sp.term_stack (and from SPQuery.term_stack_mark)
			unlink();

			// make Prolog term available for Prolog GC and make it [] for when
			// it is reused.
			sp.spPutString(termRef, "[]");

			// query stack is now populated when the SICStus object is
			// created so context can never be null
			// if (context == null) {
			// // what to do. Should perhaps enforce a dummy context
			// // to represent the bottom of the choice point (query)
			// // stack.
			// }

			context.reuseTermRef(termRef);
		}
	}

	/**
	 * Creates a null-term (i.e. the empty list, '[]').
	 * 
	 * @param sp
	 *            Pointer to the SICStus object.
	 */
	public SPTerm(SICStus sp) {
		// [PM] 3.8.5 sp.checkLegalCaller();
		initSPTerm(sp);
	}

	/**
	 * Creates a term initialized to an existing term. Modifications to the
	 * created SPTerm does not affect the input SPTerm.
	 * 
	 * @param sp
	 *            Pointer to the SICStus object.
	 * @param value
	 *            The SPTerm object whose term to use
     * @throws IllegalTermException an illegal term was detected
	 * @throws ConversionFailedException the term could not be created, for some reason
	 * 
	 * @see se.sics.jasper.SPTerm#putTerm
	 */
	public SPTerm(SICStus sp, SPTerm value) throws IllegalTermException,
			ConversionFailedException {
		synchronized (sp) {
			initSPTerm(sp);
			putTerm(value);
		}
	}

	/**
	 * Creates a term initialized to a Prolog integer.
	 * 
	 * @param sp
	 *            Pointer to the SICStus object.
	 * @param value
	 *            Initial value of the integer.
	 * 
	 * @see se.sics.jasper.SPTerm#putInteger
	 */
	public SPTerm(SICStus sp, int value) {
		// [PM] 3.8.5 sp.checkLegalCaller();
		synchronized (sp) {
			initSPTerm(sp);

			try {
				putInteger(value);
			} catch (ConversionFailedException cfe) {
			} // [PM] Cannot happen (int is 32 bit)
			catch (IllegalTermException e) {
			} // [PM] Cannot happen
		}
	}

	/**
	 * Creates a term initialized to a Prolog float.
	 * 
	 * @param sp
	 *            Pointer to the SICStus object.
	 * @param value
	 *            Initial value of the float.
	 * 
	 * @see se.sics.jasper.SPTerm#putFloat
	 */
	public SPTerm(SICStus sp, double value) {
		// [PM] 3.8.5 sp.checkLegalCaller();
		synchronized (sp) {
			initSPTerm(sp);
			try {
				putFloat(value);
			} catch (ConversionFailedException cfe) {
			} // [PM] Cannot happen
			catch (IllegalTermException e) {
			} // [PM] Cannot happen

		}
	}

	/**
	 * Creates a term initialized to a Prolog float.
	 * 
	 * @param sp
	 *            Pointer to the SICStus object.
	 * @param value
	 *            Initial value of the float.
	 * 
	 * @see se.sics.jasper.SPTerm#putFloat
	 */
	public SPTerm(SICStus sp, float value) {
		// [PM] 3.8.5 sp.checkLegalCaller();
		synchronized (sp) {
			initSPTerm(sp);
			try {
				putFloat(value);
			} catch (ConversionFailedException cfe) {
			} catch (IllegalTermException e) {
			} // [PM] Cannot happen
		}
	}

	/**
	 * Creates a term initialized to a Prolog atom.
	 * 
	 * @param sp
	 *            Pointer to the SICStus object.
	 * @param value
	 *            String describing the initial value of the atom.
	 * @throws ConversionFailedException the term could not be created, for some reason
	 * 
	 * @see se.sics.jasper.SPTerm#putString
	 */
	public SPTerm(SICStus sp, String value) throws ConversionFailedException {
		// [PM] 3.8.5 sp.checkLegalCaller();
		synchronized (sp) {
			initSPTerm(sp);

			try {
				putString(value);
			}
			// [PM] 3.8.5 this can happen so should be allowed to propagate
			// catch (ConversionFailedException cfe)
			// { }
			catch (IllegalTermException e) {
			} // [PM] Cannot happen

		}
	}

	/**
	 * Creates a term initialized to a Prolog compound term.
	 * 
	 * @param sp
	 *            Pointer to the SICStus object.
	 * @param functor
	 *            The functor of the compound term as a string
	 * @param args
	 *            The arguments as an array of SPTerm:s
	 * @throws ConversionFailedException the term could not be created, for some reason
     * @throws IllegalTermException an illegal term was detected
	 * 
	 * @see se.sics.jasper.SPTerm#consFunctor
	 */
	public SPTerm(SICStus sp, String functor, SPTerm args[])
			throws ConversionFailedException, IllegalTermException {
		// [PM] 3.8.5 sp.checkLegalCaller();
		synchronized (sp) {
			initSPTerm(sp);

			// try
			{
				consFunctor(functor, args);
			}
			// [PM] 3.8.5 This can happen so should be allowed to propagate
			// catch (ConversionFailedException cfe)
			// {}
			// [PM] 3.9 This *can* happen if args[] contains illegal terms!
			// catch (IllegalTermException e)
			// {} // [PM] Cannot happen
		}
	}

	/**
	 * Creates a term initialized to a Prolog compound term.
	 * 
	 * @param sp
	 *            Pointer to the SICStus object.
	 * @param functor
	 *            The functor of the compound term as a string
	 * @param arity
	 *            The arity of the compound term (ignored)
	 * @param args
	 *            The arguments as an SPTerm array.
	 * @throws ConversionFailedException the term could not be created, for some reason
     * @throws IllegalTermException an illegal term was detected
	 * @deprecated Do not pass the arity argument.
	 * 
	 * @see se.sics.jasper.SPTerm#consFunctor
	 */
	public SPTerm(SICStus sp, String functor, int arity, SPTerm args[])
			throws ConversionFailedException, IllegalTermException {
		// [PM] 3.8.5 sp.checkLegalCaller();
		synchronized (sp) {
			initSPTerm(sp);

			// try
			{
				consFunctor(functor, args);
			}
			// [PM] 3.8.5 This can happen so should be allowed to propagate
			// catch (ConversionFailedException cfe)
			// {}
			// [PM] 3.9 This *can* happen if args[] contains illegal terms!
			// catch (IllegalTermException e)
			// {} // [PM] Cannot happen
		}
	}

	// multi-SP
	// /**
	// * Creates a term given only the native term ref. It
	// * should really have been private, but for internal technical
	// * reasons it must be public. (No longer public as of SP 3.8.5) <p>
	// *
	// * <i>The use of this constructor is strongly discouraged!</i>
	// */
	// /* [PM] 3.8.5 no longer public */ SPTerm(long nativeTermRef) // Used by
	// glue code
	// {
	// SICStus sp = SICStus.spGlobal; // [PM] should be an argument
	// // DBG
	// // SICStus.dbgPrintln("Calling new SPTerm(long) (nativeTermRef=" +
	// nativeTermRef + ")");
	// initSPTerm(sp, nativeTermRef, true/*fromGlue*/);
	// // DBG
	// // SICStus.dbgPrintln("Term value: `" + this.toString() + "'");
	// }

	/**
	 * Creates a term given the native term ref.
	 * <p>
	 * Used by glue code
	 * 
	 * @since 3.9
	 * 
	 */
	SPTerm(SICStus sp, long nativeTermRef) // Used by glue code
	{
		// DBG
		// [PD] 3.9.2 Check if debugging before calling debug code ...
		if (debugging())
			SICStus.dbgPrintln("Calling new SPTerm(sp, long) (nativeTermRef="
					+ nativeTermRef + ")");
		boolean fromGlue = true;
		if (nativeTermRef == 0) { // hack, used from SICStus.newGlueTerm()
			initSPTerm(sp, fromGlue);
		} else {
			legalCallerOrThrowNullPointerException(sp);
			initSPTerm(sp, nativeTermRef, fromGlue);
		}
	}

	// multi-SP
	// /**
	// * Creates a null-term, but doesn't require a pointer to the
	// * SICStus object. Should really have been private, but for internal
	// * technical reasons it must be public. (No longer public as of SP 3.8.5)
	// <p>
	// *
	// * <i>The use of this constructor is strongly discouraged!</i>
	// */
	// /* [PM] 3.8.5 no longer public */ SPTerm() // [PM] Called by glue for
	// -term,-atom
	// {
	// // [PM] 3.8.4 use initSPTerm instead (was not even sp synchronized)
	// // [PM] 3.8.4 this.sp = SICStus.spGlobal;
	// // [PM] 3.8.5 SICStus.spGlobal.checkLegalCaller();
	// // [PM] 3.8.4 this.nativeTermRef = spMakeTermRef(sp);
	// initSPTerm(SICStus.spGlobal, true/*fromGlue*/);
	// }

	/**
	 * 3.8.6 SICStus constructor calls this to create a faked SPTerm that
	 * terminates the term_stack. (SPRM 2063)
	 * <p>
	 * 
	 */
	static SPTerm InitialSPTerm(SICStus sp) {
		SPTerm t = new SPTerm(sp);
		long old_ref = t.nativeTermRef;

		// Need something that is older than any term ref that could
		// reasonably be encountered. Term ref 1 is actually a valid
		// term ref but used for special purposes in the Prolog
		// run-time so is unlikely to reach Java.
		t.nativeTermRef = 1;
		// Use fromGlue to ensure it can never be deleted
		t.fromGlue = true;
		return t;
	}

	/**
	 * Tests if the Prolog term referenced is still accessible through this
	 * object. SPTerm object becomes invalid as the result of of explicit
	 * {@link #delete delete}, closing, cutting or asking for the nextSolution
	 * of an enclosing SPQuery.
	 * 
	 * @return <code>true</code> if still valid; <code>false</code> otherwise.
	 *         The result is only guaranteed to remain valid while inside a
	 *         block synchronized on the SICStus object.
	 */
	public boolean isValid() {
		return (nativeTermRef != SP_ILLEGAL_TERM_REF);
	}

	/**
	 * As isValid(). In addition verifies that this SPTerm belongs to the
	 * SICStus object argument.
	 * @param sp the Prolog that the term should be validated against
	 * @return true if the receiver is a valid term in the specified Prolog
	 */
	public boolean isValid(SICStus sp) {
		return isValid() && (sp == this.sp);
	}

	// [PM] 3.8.5 Used by glue code
	/* public */long GetNativeTermRef() throws IllegalTermException {
		// SICStus.dbgPrintln("GetNativeTermRef (" + nativeTermRef +
		// ") isValid: " + isValid());
		synchronized (sp) {
			CheckValid();
			return nativeTermRef;
		}
	}

	/**
	 * Throws an exception if the term is not valid.
	 * 
     * @throws IllegalTermException an illegal term was detected
	 * @see SPTerm#isValid
	 */
	public void CheckValid() throws IllegalTermException {
		if (!isValid()) {
			if (debugging())
				SICStus.dbgPrintln("!" + superToString()
						+ ".isValid, this.sp==" + this.sp + ", nativeTermRef=="
						+ this.nativeTermRef);

			throw new IllegalTermException(sp);
		}
	}

	/**
	 * @param sp the Prolog instance to check
	 * @throws IllegalTermException if the argument was invalid
	 */
	public void CheckValid(SICStus sp) throws IllegalTermException {
		if (!isValid(sp)) {
			if (debugging())
				SICStus.dbgPrintln("!" + superToString() + ".isValid(" + sp
						+ "), this.sp==" + this.sp + ", nativeTermRef=="
						+ this.nativeTermRef);
			throw new IllegalTermException(this.sp);
		}
	}

	/**
	 * Calls isValid() (which see) and legalCallerOrThrowNullPointerException
	 * (which also see). All public methods should start with calling this
	 * method.
	 */
	private void checkValidAndLegalCaller() throws IllegalTermException {
		if (!isValid()) {
			if (debugging())
				SICStus.dbgPrintln("!" + superToString()
						+ ".isValid, this.sp==" + this.sp + ", nativeTermRef=="
						+ this.nativeTermRef);
			throw new IllegalTermException(sp);
		}
		legalCallerOrThrowNullPointerException(sp);
	}

	// [PM] 3.8.5 Called from SPQuery (and from glue?)
	// Called when synchronized on sp but before unlinked from
	// termRefStack (so must not touch next field)
	void invalidate() {
		if (debugging(2)) {
			SICStus.dbgPrintln("Invalidate "
					+ (fromGlue ? "glue" : "")
					+ "SPTerm "
					+ super.toString()
					+ " nativeTermRef=="
					+ nativeTermRef
					+ " in "
					+ Thread.currentThread()
					+ ((this.creating_thread == Thread.currentThread()) ? ""
							: " Warning: creating thread was "
									+ (this.creating_thread != null ? this.creating_thread
											.getName() : "null!?(ERROR)")));
			if (this.creating_thread != Thread.currentThread()) {
				Thread.currentThread().dumpStack();
			}
			Thread.dumpStack();
		}
		nativeTermRef = SP_ILLEGAL_TERM_REF; // crucial

		// Do not zap the next field. delete() should be able to
		// invalidate SPTerms still on the term-stack
		// next = null; // not really needed but perhaps safer

		// Wrong (sp needed to raise exception and for synchronizing):
		// sp = null; // not needed at all
	}

	/**
	 * Returns the type of this term. The return value is as defined by the
	 * constants <code>SP_TYPE_INTEGER</code>, <code>SP_TYPE_FLOAT</code>,
	 * <code>SP_TYPE_ATOM</code>, <code>SP_TYPE_VARIABLE</code>, and
	 * <code>SP_TYPE_COMPOUND</code>
	 *
	 * @see SPTerm#SP_TYPE_INTEGER
	 * @see SPTerm#SP_TYPE_ATOM
	 * @see SPTerm#SP_TYPE_FLOAT
	 * @see SPTerm#SP_TYPE_VARIABLE
	 * @see SPTerm#SP_TYPE_COMPOUND
	 * @see SPTerm#isVariable
	 * @see SPTerm#isInteger
	 * @see SPTerm#isAtom
	 * @see SPTerm#isFloat
	 * @see SPTerm#isCompound
	 * @see SPTerm#isList
	 * @see SPTerm#isAtomic
	 * @see SPTerm#isNumber
	 */
	public int type() throws IllegalTermException {
		synchronized (sp) {
			// [PD] 3.10.1 This switch to checkValidAndLegalCaller() should
			// have been done in 3.9.2 !!!
			// CheckValid();
			checkValidAndLegalCaller();
			return sp.spTermType(nativeTermRef);
		}
	}

	/**
	 * Tests if the term is a variable.
	 * 
	 * @return <code>true</code> if the term is a variable; <code>false</code>
	 *         otherwise
	 */
	public boolean isVariable() throws IllegalTermException

	{
		return type() == SP_TYPE_VARIABLE;
	}

	/**
	 * Tests if the term is an integer.
	 * 
	 * @return <code>true</code> if the term is an integer; <code>false</code>
	 *         otherwise
	 */
	public boolean isInteger() throws IllegalTermException {
		return type() == SP_TYPE_INTEGER;
	}

	/**
	 * Tests if the term is an atom.
	 * 
	 * @return <code>true</code> if the term is an atom; <code>false</code>
	 *         otherwise
	 */
	public boolean isAtom() throws IllegalTermException {
		return type() == SP_TYPE_ATOM;
	}

	/**
	 * Tests if the term is a float.
	 * 
	 * @return <code>true</code> if the term is a float; <code>false</code>
	 *         otherwise
	 */
	public boolean isFloat() throws IllegalTermException {
		return type() == SP_TYPE_FLOAT;
	}

	/**
	 * Tests if the term is a compound term.
	 * 
	 * @return <code>true</code> if the term is a compound term;
	 *         <code>false</code> otherwise
	 */
	public boolean isCompound() throws IllegalTermException {
		return type() == SP_TYPE_COMPOUND;
	}

	/**
	 * Tests if the term is a list cell. Equivalent to the Prolog code
	 * <code>isList(T) :- nonvar(T), T=[_|_].</code>
	 * 
	 * @return <code>true</code> if the term is a list cell; <code>false</code>
	 *         otherwise
	 */
	public boolean isList() throws IllegalTermException {
		synchronized (sp) {
			// [PD] 3.10.1 This switch to checkValidAndLegalCaller() should
			// have been done in 3.9.2 !!!
			// CheckValid();
			checkValidAndLegalCaller();
			return sp.spIsList(this.nativeTermRef);
		}
	}

	/**
	 * Tests if the term is the empty list.
	 * 
	 * @return <code>true</code> if the term is the atom <code>[]</code>;
	 *         <code>false</code> otherwise
	 */
	public boolean isEmptyList() throws IllegalTermException {
		synchronized (sp) {
			if (isAtom()) { // Does CheckValid
				return (sp.isEmptyList(this.nativeTermRef));
			}
			return false;
		}
	}

	// 3.8
	// public boolean isEmptyList()
	// throws IllegalTermException
	// {
	// synchronized (sp) {
	// if (isAtom()) { // Does CheckValid
	// try {
	// return (sp.spGetAtom(nativeTermRef) == SPCanonicalAtom.atomNil);
	// }
	// catch (SPException e) {
	// if (debugging())
	// SICStus.dbgPrintln("** Warning Exception in isEmptyList");
	// return false;
	// }
	// }
	// return false;
	// }
	// }

	/**
	 * Tests if the term is atomic. A term is atomic if it is instantiated to an
	 * atom or a number.
	 * 
	 * @return <code>true</code> if the term is atomic; <code>false</code>
	 *         otherwise
	 */
	public boolean isAtomic() throws IllegalTermException {
		synchronized (sp) {
			// [PD] 3.10.1 This switch to checkValidAndLegalCaller() should
			// have been done in 3.9.2 !!!
			// CheckValid();
			checkValidAndLegalCaller();
			return sp.spIsAtomic(this.nativeTermRef);
		}
	}

	/**
	 * Tests if the term is a number. A number is a term instantiated to an
	 * integer or a float.
	 * 
	 * @return <code>true</code> if the term is a number; <code>false</code>
	 *         otherwise
	 */
	public boolean isNumber() throws IllegalTermException {
		synchronized (sp) {
			// [PD] 3.10.1 This switch to checkValidAndLegalCaller() should
			// have been done in 3.9.2 !!!
			// CheckValid();
			checkValidAndLegalCaller();
			return sp.spIsNumber(this.nativeTermRef);
		}
	}

	/*
	 * --------------------------------------------------------------------------
	 * ---- Methods for manipulating SPTerms. Corresponds more or less directly
	 * to the SP_put_xxx() functions in the C-Prolog interface.
	 * ------------------
	 * ------------------------------------------------------------
	 */

	/**
	 * Assigns the term to an unbound variable.
	 *
	 * @return Returns a reference to itself
	 * @throws ConversionFailedException
	 *                The term could not be converted to a variable.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putVariable() throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			CheckPut();

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutVariable(nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to variable");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a Prolog integer from a Java <code>int</code>.
	 *
	 * @param value
	 *            The value of the integer
	 * @return Returns a reference to itself
	 * @throws ConversionFailedException
	 *                The term could not be converted to a integer.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putInteger(int value) throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			CheckPut(); // overkill for fixnums

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutInteger(nativeTermRef, value);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to integer");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a Prolog integer from a Java <code>long</code>.
	 *
	 * @param value
	 *            The value of the integer
	 * @return Returns a reference to itself
	 * @throws ConversionFailedException
	 *                The term could not be converted to a integer.
     * @throws IllegalTermException an illegal term was detected
	 * @since 3.9.1 FIXME: Should be made public when Java longs (64bit) can be
	 *        put also on 32bit machines. /FIXED [PD] 3.9.2 Added in order to
	 *        support 64bit platforms (Tru64)
	 */
	public SPTerm putInteger(long value) throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			CheckPut(); // overkill for fixnums

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutInteger(nativeTermRef, value);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to integer");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a Prolog float from a Java <code>double</code>.
	 *
	 * @param value
	 *            The value of the float
	 * @return Returns a reference to itself
	 * @throws ConversionFailedException
	 *                The term could not be converted to a float.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putFloat(double value) throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			CheckPut();

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutFloat(nativeTermRef, value);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to float");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a Prolog float from a Java <code>float</code>.
	 *
	 * @param value
	 *            The value of the float
	 * @return Returns a reference to itself
	 * @throws ConversionFailedException
	 *                The term could not be converted to a float.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putFloat(float value) throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			CheckPut();

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutFloat(nativeTermRef, value);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to float");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a Prolog atom from a Java <code>String</code>.
	 *
	 * @param value
	 *            The value of the atom as a String
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to an atom.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putString(String value) throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			// No CheckPut needed since atoms are not on heap

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutString(nativeTermRef, value);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to atom (string)");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to an empty list <code>[]</code>.
	 * 
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException the term could not be created, for some reason
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putEmptyList() throws ConversionFailedException,
			IllegalTermException {
		return putString("[]");
	}

	/**
	 * Assigns the term to a Prolog atom. The argument is the canonical
	 * representation of a Prolog atom represented as a Java <code>long</code>.
	 *
	 * @param canonical_value
	 *            The canonical representation of an atom.
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to an atom.
     * @throws IllegalTermException an illegal term was detected
	 * @deprecated Use putCanonicalAtom instead.
	 */
	public SPTerm putAtom(long canonical_value)
			throws ConversionFailedException, IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			// no CheckPut needed since atom are not on heap

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutAtom(nativeTermRef, canonical_value);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to atom (canonical)");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a Prolog atom, given the canonical representation of
	 * the atom as an SPCanonicalAtom object.
	 *
	 * @param cAtom
	 *            The canonical representation of an atom.
	 * @return Returns a reference to itself.
	 * @see se.sics.jasper.SPCanonicalAtom
	 * @throws ConversionFailedException
	 *                The term could not be converted to an atom.
     * @throws IllegalTermException an illegal term was detected
	 */

	public SPTerm putCanonicalAtom(SPCanonicalAtom cAtom)
			throws ConversionFailedException, IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			// no CheckPut needed for atoms
			cAtom.CheckValid(sp);

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutString(nativeTermRef, cAtom.getString());
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to atom (canonical)");
			}
			return this;
		}
	}

	// 3.8
	// public SPTerm putCanonicalAtom(SPCanonicalAtom cAtom)
	// throws ConversionFailedException, IllegalTermException
	// {
	// synchronized (sp) {
	// CheckValid();
	// // no CheckPut needed for atoms
	// cAtom.CheckValid(sp);
	//
	// // [PM] 3.8.5 sp.checkLegalCaller();
	// try { sp.spPutAtom(nativeTermRef, cAtom.getAtomNumber()); }
	// catch ( NativeCodeException nce )
	// { throw new ConversionFailedException(sp, "to atom (canonical)"); }
	// return this;
	// }
	// }

	/**
	 * Assigns the term to a *global* reference to a Java object.
	 * 
	 * @param obj
	 *            A reference to a Java object.
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to a Java object
	 *                reference.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putObject(Object obj) throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {

			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			CheckPut();

			SPTerm array[];
			long globRef;

			// [PM] 3.8.5 sp.checkLegalCaller();

			try {
				globRef = sp.spCreateGlobalRef(obj);
				// xref jasper.pl

				// [PM] 3.9.1 use SPTerm.putInteger(long) instead of
				// SPTerm(SICStus,int) so the full 64bit value is
				// preserved, at least on 64bit machines (for Tru64)
				consFunctor("$java_object",
						new SPTerm[] { new SPTerm(sp).putInteger(globRef) });
			} catch (ConversionFailedException cfe) {
				throw new ConversionFailedException(sp, "to object-reference");
			}
			return this;
		}
	}

	/**
	 * Returns the object encapsulated in the Prolog term. This method should
	 * only be called on terms to which a Java object has been assigned using
	 * the method {@link se.sics.jasper.SPTerm#putObject}.
	 * 
	 * @return A Java object
	 */
	public Object getObject() throws NativeCodeException, IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			// [PM] 3.8.5 sp.checkLegalCaller();
			return sp.spGetObject(this.nativeTermRef);
		}
	}

	/**
	 * Assigns the term to another Prolog term.
	 * 
	 * @param new_term
	 *            The term's new value
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to another term.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putTerm(SPTerm new_term) throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			new_term.CheckValid(sp);
			CheckPut(new_term);

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutTerm(this.nativeTermRef, new_term.nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to term");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a list of character codes from a Java
	 * <code>String</code>.
	 * 
	 * @param string
	 *            The String object
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to list of characters.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putListChars(String string) throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			CheckPut();

			// [PM] 3.8.5 sp.checkLegalCaller();

			try {
				sp.spPutListChars(this.nativeTermRef, string);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to list of characters");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a Prolog number from a string.
	 * 
	 * @param string
	 *            The string containing the printed representation of the
	 *            number.
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to number.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putNumberChars(String string)
			throws ConversionFailedException, IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			CheckPut();

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutNumberChars(this.nativeTermRef, string);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to number");
			}

			return this;
		}
	}

	/**
	 * Assigns the term to a compound term with all arguments initialized to
	 * unbound variables.
	 * 
	 * @param functor
	 *            The term's functor as a Java <code>String</code>
	 * @param arity
	 *            The term's arity
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to compound term.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putFunctor(String functor, int arity)
			throws ConversionFailedException, IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			if (arity > 0) {
				CheckPut();
			}

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutFunctor(nativeTermRef, sp.spGetAtomFromString(functor),
						arity);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to functor");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a compount term.
	 * 
	 * @param functor
	 *            The term's functor as a Java <code>String</code>
	 * @param args
	 *            The arguments of the term
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to compound term.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm consFunctor(String functor, SPTerm args[])
			throws ConversionFailedException, IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			if (args.length > 0)
				CheckPut();

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spConsFunctor(nativeTermRef,
						sp.spGetAtomFromString(functor),
						sp.termArrayToLongArray(args));
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to compound term");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a compount term.
	 * 
	 * @param functor
	 *            The term's functor as a Java <code>String</code>
	 * @param arity
	 *            The term's arity (ignored)
	 * @param args
	 *            The arguments of the term
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to compound term.
     * @throws IllegalTermException an illegal term was detected
	 * @deprecated Use consFunctor without arity argument instead.
	 */
	// NOTE: drop the arity argument (implicit in args)
	public SPTerm consFunctor(String functor, int arity, SPTerm args[])
			throws ConversionFailedException, IllegalTermException {
		return consFunctor(functor, args);
	}

	/**
	 * Assigns the term to a list whose head and tail are both unbound
	 * variables.
	 * 
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to an empty list.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm putList() throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			CheckPut();

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spPutList(nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to empty list");
			}
			return this;
		}
	}

	/**
	 * Assigns the term to a list with given head and tail.
	 * 
	 * @param head
	 *            The head of the list
	 * @param tail
	 *            The tail of the list
	 * @return Returns a reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to list.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm consList(SPTerm head, SPTerm tail)
			throws ConversionFailedException, IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			CheckPut();

			head.CheckValid(sp);
			tail.CheckValid(sp);

			// [PM] 3.8.5 sp.checkLegalCaller();

			try {
				sp.spConsList(nativeTermRef, head.nativeTermRef,
						tail.nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to list");
			}
			return this;
		}
	}

	/*
	 * Methods for accessing SPTerms. Corresponds more or less directly to the
	 * SP_get_xxx() functions in the C-Prolog interface.
	 */

	/**
	 * Obtains the integer value of the Prolog term.
	 *
	 * @return The value of the Prolog term as a Java <code>long</code>.
	 * @throws ConversionFailedException
	 *                The term could not be converted to an integer.
	 */
	public long getInteger() throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				return sp.spGetInteger(nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "term to integer");
			}
		}
	}

	/**
	 * Obtains the value of the Prolog float.
	 *
	 * @return The value of the Prolog float as a Java <code>double</code>.
	 * @throws ConversionFailedException
	 *                The term could not be converted to a double.
	 */
	public double getDouble() throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				return sp.spGetFloat(nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "term to double");
			}
		}
	}

	/**
	 * Obtains the canonical representation of a Prolog atom.
	 *
	 * @return The canonical representation of a Prolog atom as a Java
	 *         <code>long</code>.
	 * @throws ConversionFailedException
	 *                The term could not be converted to the canonical
	 *                representation of an atom.
	 * @deprecated Use getCanonicalAtom instead.
	 */
	/* public *//* Deprecated in 3.8.5, no longer piblic in 3.9 */
	long getAtom() // [PM] 3.8.5 also used by glue, must have the same name as
					// in SPCanonicalAtom (but need not be public)
			throws ConversionFailedException, IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				return sp.spGetAtom(nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp,
						"term to atom (canonical)");
			}
		}
	}

	/**
	 * Obtains the canonical representation of a Prolog atom.
	 *
	 * @return The canonical representation of a Prolog atom as an
	 *         SPCanonicalAtom object.
	 * @throws ConversionFailedException
	 *                The term could not be converted to the canonical
	 *                representation of an atom.
	 * @throws AtomRegisterFailure the atom could not be registered for some reason
     * @throws IllegalTermException an illegal term was detected
	 * @see se.sics.jasper.SPCanonicalAtom
	 */
	public SPCanonicalAtom getCanonicalAtom() throws ConversionFailedException,
			AtomRegisterFailure, IllegalTermException {
		// [PM] 3.8.5 sp.checkLegalCaller();
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			try {
				return new SPCanonicalAtom(sp, sp.spGetString(nativeTermRef));
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp,
						"term to atom (canonical)");
			}
		}
	}

	// 3.8
	// public SPCanonicalAtom getCanonicalAtom()
	// throws ConversionFailedException, AtomRegisterFailure,
	// IllegalTermException
	// {
	// // [PM] 3.8.5 sp.checkLegalCaller();
	// synchronized (sp) {
	// CheckValid();
	// try {
	// return new SPCanonicalAtom(sp,sp.spGetAtom(nativeTermRef));
	// } catch ( NativeCodeException nce )
	// { throw new ConversionFailedException(sp, "term to atom (canonical)"); }
	// }
	// }

	/**
	 * Obtains the value of the Prolog atom as a string.
	 *
	 * @return The value of the Prolog atom as a Java <code>String</code>.
	 * @throws ConversionFailedException
	 *                The term could not be converted to a string.
	 */
	public String getString() throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			// [PM] 3.8.5 sp.checkLegalCaller();

			try {
				return sp.spGetString(nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "to atom (string)");
			}
		}
	}

	/**
	 * Obtains the value of a list of characters as a string.
	 *
	 * @return The value of the list as a Java <code>String</code>.
	 * @throws ConversionFailedException
	 *                The term could not be converted to a list of characters.
	 */
	public String getListChars() throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			// [PM] 3.8.5 sp.checkLegalCaller();

			try {
				return sp.spGetListChars(/* this, */nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp,
						"list of character to string");
			}
		}
	}

	/**
	 * Obtains the value of a Prolog number as a string.
	 *
	 * @return The value of the number as a Java <code>String</code>.
	 * @throws ConversionFailedException
	 *                The term could not be converted to a number.
	 */
	public String getNumberChars() throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				return sp.spGetNumberChars(/* this, */nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "number to string");
			}
		}
	}

	/**
	 * Obtains the name of a functor.
	 *
	 * @return The functor as a Java <code>String</code>.
	 * @throws ConversionFailedException
	 *                The term could not be converted to the name of a functor.
	 */
	public String getFunctorName() throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				return sp.spGetStringFromAtom(sp
						.spGetFunctorCanonical(this.nativeTermRef));
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp,
						"functor to canonical atom");
			}
		}
	}

	/**
	 * Obtains the arity of a functor.
	 *
	 * @return The arity of a functor as a Java <code>int</code>.
	 * @throws ConversionFailedException
	 *                The term could not be converted to the arity of a functor.
	 */
	public int getFunctorArity() throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				return sp.spGetFunctorArity(nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "functor to arity");
			}
		}
	}

	/**
	 * Gets the head and tail of a Prolog list.
	 * 
	 * @param head
	 *            The term which will be assigned the head
	 * @param tail
	 *            The term which will be assigned the tail
	 *
	 * @return A reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to a list.
	 */
	// public SPTerm getList(SPTerm head, SPTerm tail) // [PD] 3.9 Threadsafe
	public Term getList(Term head, Term tail) // [PD] 3.9 Threadsafe
			throws ConversionFailedException, IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			((SPTerm) head).CheckValid(sp);
			((SPTerm) tail).CheckValid(sp);

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spGetList(this.nativeTermRef, ((SPTerm) head).nativeTermRef,
						((SPTerm) tail).nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp, "list to head/tail");
			}
			return this;
		}
	}

	/**
	 * @param i
	 *            The number of the argument.
	 * @param arg
	 *            The term to which the <code>i</code><i>th</i> argument will be
	 *            assigned.
	 * @return A reference to itself.
	 * @throws ConversionFailedException
	 *                The term could not be converted to the argument of a
	 *                compound term.
	 * @throws IllegalTermException an invalid term was detected
	 */
	public Term getArg(Integer i, Term arg) throws ConversionFailedException,
			IllegalTermException {
		return getArg(i.intValue(), arg);
	}

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
	 * @throws ConversionFailedException
	 *                The term could not be converted to the argument of a
	 *                compound term.
	 */
	// public SPTerm getArg(int i, SPTerm arg) // [PD] 3.9 Threadsafe
	public Term getArg(int i, Term arg) // [PD] 3.9 Threadsafe
			throws ConversionFailedException, IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2
			((SPTerm) arg).CheckValid(sp);

			// [PM] 3.8.5 sp.checkLegalCaller();
			try {
				sp.spGetArg(i, this.nativeTermRef, ((SPTerm) arg).nativeTermRef);
			} catch (NativeCodeException nce) {
				throw new ConversionFailedException(sp,
						"compound term to argument");
			}
			return this;
		}
	}

	/**
	 * Unifies the term with another term.
	 * 
	 * @param with
	 *            The term with which to unify.
	 * @return True/False depending on whether or not the unification succeeded.
	 */
	// public boolean unify(SPTerm with) // [PD] 3.9 Threadsafe
	public boolean unify(Term with) // [PD] 3.9 Threadsafe
			throws IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			((SPTerm) with).CheckValid(sp);
			// [PM] 3.8.5 sp.checkLegalCaller();
			return sp
					.spUnify(this.nativeTermRef, ((SPTerm) with).nativeTermRef);
		}
	}

	/**
	 * Compares two terms according to standard order.
	 * 
	 * @param with
	 *            The term to compare with.
	 * @return -1 if <code>x &lt; y</code>, 0 if <code>x == y</code>, and 1 if
	 *         <code>x &gt; y</code>.
	 */
	// public int compare(SPTerm with) // [PD] 3.9 Threadsafe
	public int compare(Term with) // [PD] 3.9 Threadsafe
			throws IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			((SPTerm) with).CheckValid(sp);
			// [PM] 3.8.5 sp.checkLegalCaller();
			return sp.spCompare(this.nativeTermRef,
					((SPTerm) with).nativeTermRef);
		}
	}

	/**
	 * Returns a string-respresentation of a term. This is not very
	 * sophisticated, for example, it does not take operators or lists into
	 * account.
	 */
	public String toString() {
		return toStringRec(0);
	}

	String toStringRec(int level) {
		// SICStus.dbgPrintln("in toString(): termref = " + this.nativeTermRef);
		// Thread.dumpStack();
		synchronized (sp) {
			// { // DBG
			//
			// if (level > 20) {
			// SICStus.dbgPrintln("Warning: SPTerm::toString() excessive recursion ("
			// + level + ")");
			// return "...";
			// }
			// }

			try {
				// [PM] 3.8.5 sp.checkLegalCaller();
				// SICStus.dbgPrintln("Caller is legal");

				if (!isValid()) {
					return "<error:invalid term>";
				}

				// [PD] 3.9.2
				if (!sp.isLegalCaller()) {
					return "<error: illegal caller>";
				}

				if (debugging())
					sp.dbgPrintln("in SPTerm.toString(): caller is legal");
				switch (type()) // Does CheckValid()
				{
				case SPTerm.SP_TYPE_INTEGER:
					return Long.toString(getInteger());

				case SPTerm.SP_TYPE_FLOAT:
					return Double.toString(getDouble());

				case SPTerm.SP_TYPE_ATOM:
					if (debugging())
						sp.dbgPrintln("    SP_TYPE_ATOM");
					return getString();

				case SPTerm.SP_TYPE_VARIABLE:
					return sp.spPrintVariable(this.nativeTermRef);

				case SPTerm.SP_TYPE_COMPOUND: {
					SPTerm arg = new SPTerm(sp);
					int arity;
					String name;
					StringBuffer printrep = new StringBuffer();

					try {
						arity = getFunctorArity();
						name = getFunctorName();
					} catch (ConversionFailedException e) {
						return "<cfexcp>";
					}

					if (name != null)
						printrep.append(name);
					else
						printrep.append("<null>");

					if (arity >= 1) {
						int i;
						printrep.append("(");

						// Make a recursive call on each argument
						for (i = 1; i <= arity; i++) {
							try {
								this.getArg(i, arg);
								if (arg != null)
									printrep.append(arg.toStringRec(level + 1));
								else
									printrep.append("<null>");
							} catch (ConversionFailedException e) {
								printrep.append("<cfexcp>");
							}
							if (i < arity)
								printrep.append(",");
						}

						printrep.append(")");
					}
					return printrep.toString();
				}

				default:
					return "<unknown>";
				}
			} catch (SPException e) {
				return "<spexcp>";
			}
		}
	}

	public String toString(Term options) throws SPException {
		long jstreamCode;
		StringBuffer stringBuf;
		Term streamCode;
		HashMap varMap;

		if (debugging())
			SICStus.dbgPrintln("Entering SPTerm.toString(Term options)");
		checkValidAndLegalCaller();
		synchronized (sp) {
			stringBuf = new StringBuffer();
			jstreamCode = sp.openStringBufferStream(stringBuf);
			try {
				if (debugging())
					SICStus.dbgPrintln("SPTerm.toString(Term options): jstreamCode=="
							+ jstreamCode);
				streamCode = sp.newTerm(jstreamCode);
				varMap = new HashMap();
				varMap.put("StreamCode", streamCode);
				varMap.put("Term", this);
				varMap.put("Options",
						(options == null ? sp.newTerm() : options));
				// if (debugging()) SICStus.dbgPrintln("toString(): varMap==" +
				// varMap);
				if (!sp.query(
						"prolog:stream_code(Stream, StreamCode), prolog:write_term(Stream, Term, Options).",
						varMap)) {
					throw new SPException(sp, "toString(Term options) failed");
				}
			} finally {
				sp.closeStringStream(jstreamCode);
			}
			return stringBuf.toString();
		}
	}

	// Alternative code without readFromString.
	//
	// public String toString(Term options) throws SPException
	// {
	// long jstreamCode;
	// StringBuffer stringBuf;
	// SPTerm streamCode;

	// if (debugging()) SICStus.dbgPrintln("Enter: " + superToString() +
	// ".toString(" + options + ")");
	// synchronized (sp) {
	// stringBuf = new StringBuffer();
	// jstreamCode = sp.openStringBufferStream(stringBuf);
	// if (debugging()) SICStus.dbgPrintln("toString(): jstreamCode==" +
	// jstreamCode);
	// try {
	// streamCode = new SPTerm(sp).putInteger(jstreamCode);
	// SPTerm stream = new SPTerm(sp).putVariable();
	// if (!sp.query("prolog", "stream_code",
	// new SPTerm[] {stream, streamCode})) {
	// throw new SPException(sp, "stream_code/2 failed");
	// }
	// if (!sp.query("prolog", "write_term",
	// new SPTerm[] {stream, this, (SPTerm)options})) {
	// throw new SPException(sp, "write_term/3 failed");
	// }
	// } finally {
	// sp.closeStringStream(jstreamCode);
	// }
	// return stringBuf.toString();
	// }
	// }

	/**
	 * Converts a list to an array of SPTerm. Useful when examining lists
	 * returned from Prolog queries.
	 *
	 * This is of course only possible on a list.
	 * 
	 * @throws ConversionFailedException
	 *                If the term could not be converted to a list.
	 * @return An array of the terms.
     * @throws IllegalTermException an illegal term was detected
	 */
	public SPTerm[] toTermArray() throws ConversionFailedException,
			IllegalTermException {
		synchronized (sp) {
			// [PD] 3.9.2
			// CheckValid();
			checkValidAndLegalCaller(); // [PD] 3.9.2

			// [PM] 3.8.5 sp.checkLegalCaller();
			// SICStus.dbgPrintln("Caller is legal");

			// SICStus.dbgPrintln("SPTerm is valid");
			SPTerm head, tail, elem;
			SPTerm elems[];
			int len = 1, i;

			head = new SPTerm(sp);
			tail = new SPTerm(sp);

			// [PM] 3.8.5 Always documented as throwing
			// ConversionFailedException so why catch it?
			// try
			{
				// SICStus.dbgPrintln("SPTerm isList=="+isList());

				this.getList(head, tail);

				while (tail.isList()) {
					len++;
					tail.getList(head, tail);
				}
				// SICStus.dbgPrintln("len=="+len);
				elems = new SPTerm[len];
				tail.putTerm(this);
				for (i = 0; i < len; i++) {
					// SICStus.dbgPrintln("i=="+i);
					elem = new SPTerm(sp);
					tail.getList(elem, tail);
					elems[i] = elem;
				}
			}
			// catch (ConversionFailedException cfe)
			// { return null; }

			return elems;
		}
	}

	// [PD] 3.9 Implementing Interface Term
	public Term[] toPrologTermArray() throws ConversionFailedException,
			IllegalTermException {
		return (Term[]) toTermArray();
	}

}

/**
 * [PM] Keep the original indentation style Local variables: indent-tabs-mode:
 * nil c-basic-offset: 4 end:
 **/

