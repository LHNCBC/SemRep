/* $Id$
 * Copyright (c) 1998 SICS 
 */

package se.sics.jasper;

/** The SPCanonicalAtom class encapsulates the canonical representation of
 * a Prolog atom, which is usually a 32 or 64-bit integer. In previous
 * versions of Jasper, atoms were passed around as Java long
 * types. However, this prevented interaction with the atom garbage
 * collector. By using this class instead, atoms only referenced
 * canonically from Java are ensured not to be garbage collected.
 * 
 * <p> You should not inherit from <code>SPCanonicalAtom</code>.
 *
 * @see se.sics.jasper.SPTerm#getCanonicalAtom */
public class SPCanonicalAtom
{
    SICStus sp;
    // [PM] 3.9 Use strings to minimize calls to SP run-time
    // long cAtom;
    String string;

    // [PM] 3.9 Moved to SICStus
    // private native int spRegisterAtom(long cAtom);
    // private native int spUnRegisterAtom(long cAtom);
    // private native long spAtomFromString(SICStus sp, String string); // [PM] 3.8.5 sp always this.sp
    // private native String spStringFromAtom(long cAtom);

    // static long atomNil = 0xa0000000L; // [PM] Akk. Will not work on 64bit SP.

    // The sp arguments below are not used for now, but may be useful in
    // the future if multiple atom databases (i.e. multiple Prolog engines)
    // become possible.

    boolean isValid()
    {
        return true;
    }

    boolean isValid(SICStus sp)
    {
        return isValid() && (sp == this.sp);
    }

    /**
     * Throws an exception of the atom is not valid.
     * @see SPCanonicalAtom#isValid
     */
    void CheckValid()
        throws IllegalTermException
    {
      if (!isValid()) {
        throw new IllegalTermException(sp); // NOTE: need a better exception
      }
    }

    void CheckValid(SICStus sp)
        throws IllegalTermException
    {
      if (!isValid(sp)) {
        throw new IllegalTermException(this.sp); // NOTE: need a better exception
      }
    }

    //    long getAtomNumber() { // [PM] 3.8.5 Caller should ensure isValid()
    //	return cAtom;
    //    }

    //    long getAtom()            // [PM] 3.8.5 Used by glue, must have the same name as in SPTerm
    //        throws IllegalTermException
    //    {
    //        CheckValid();
    //        return getAtomNumber();
    //    }

    //  long getAtom()            // [PM] 3.8.5 Used by glue, must have the same name as in SPTerm
    //      throws IllegalTermException
    //  {
    //      CheckValid();
    //      return getAtomNumber();
    //  }

    /** Creates a canonical atom from an integer */
    // [PM] 3.9 was:
    //  SPCanonicalAtom(SICStus sp, long cAtom)
    //      throws AtomRegisterFailure
    //  {
    //      this.cAtom = cAtom;
    //      this.sp = sp;
    //      synchronized (sp) {
    //          if (sp.spRegisterAtom(cAtom) == 0)
    //              throw new AtomRegisterFailure(sp, "could not register atom");
    //      }
    //  }

    //  /* 3.9 Does not register, used by SICStus.newGlueAtom() */
    //  SPCanonicalAtom(long cAtom, SICStus sp)
    //  {
    //      // this.cAtom = cAtom;
    //      this.sp = sp;
    //  }

    //  /** Creates a canonical atom from a string.  */
    //  public SPCanonicalAtom(SICStus sp, String string)
    //      throws AtomRegisterFailure
    //  {
    //      this.sp = sp;
    //      synchronized (sp) {
    //          this.cAtom = (long)sp.spAtomFromString(/*this, */ string); // throws NCE
    //          if (sp.spRegisterAtom(cAtom) == 0)
    //              throw new AtomRegisterFailure(sp, "could not register atom");
    //      }
    //  }

    /** Creates a canonical atom from a string.  
     * @param sp the Prolog where the atom should be created
     * @param string the name of the atom
     * @throws AtomRegisterFailure if the atom could not be registered, for any reason
     */
    public SPCanonicalAtom(SICStus sp, String string)
	throws AtomRegisterFailure
    {
    	// [PM] 4.3.1 FIXME: This looks strange
        if (string == null) throw new AtomRegisterFailure(sp, "could not register atom \"" + (string == null ? "?null?" : string) + "\"");
	this.sp = sp;
        this.string = string;
    }

// [PM] 3.9 multi-SP
//     /**
//      * Creates a canonical atom given only the canonical atom
//      * representation.
//      * 
//      * <i>The use of this constructor is strongly discouraged!</i> */
//
//     /* [PM] 3.8.5 no longer public */ SPCanonicalAtom(long cAtom) // Used by glue code
// 	throws AtomRegisterFailure
//     {
// 	this.sp = SICStus.spGlobal;
// 	this.cAtom = cAtom;
// 	synchronized (sp) {
// 	    if (spRegisterAtom(cAtom) == 0)
// 		throw new AtomRegisterFailure(sp, "could not register atom");
// 	}
//     }

    /** Returns this canonical atom as a string */
    public String toString() {
        return string;
    //      synchronized (sp) { 
    //          return sp.spStringFromAtom(cAtom); 
    //      }
    }

    /* [PM] 3.9 The Prolog side string, currently the same as toString()*/
    /** Returns this canonical atom as a string */
    String getString() {
        return string;
    }


    /** Returns true iff the atoms have the same canonical representation. 
     * @param cAtom the atom to check against
     * @return whether the argument represents the same atom as the receiver.
     * @throws IllegalTermException an illegal term was detected
     *  */
    public boolean isSameAtom(SPCanonicalAtom cAtom)
        throws IllegalTermException
    {
        CheckValid();
        cAtom.CheckValid(sp);
        return this.string.equals(cAtom.string);
    //      synchronized (sp) { 
    //          return this.getAtomNumber() == cAtom.getAtomNumber(); 
    //      }
    }

    // [PM] 3.8.5 Do not want synchronized methods during GC that can
    // cause hard to track down deadlocks.
    // NOTE: lacks unregister facility, should allow explicit
    // unregister and should queue up unregister from finalizer for
    // the next sp.XXX callback.


//     /** Un-registers this atom so it may be GC:d */
//     public void finalize() {
// 	synchronized (sp) {
// 	    sp.spUnRegisterAtom(cAtom);
// 	}
//     }
}


/** [PM] Keep the original indentation style
 *  Local variables:
 *      indent-tabs-mode: nil
 *      c-basic-offset: 4
 *  end:
 **/

