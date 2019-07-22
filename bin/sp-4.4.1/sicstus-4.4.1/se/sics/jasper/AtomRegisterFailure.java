/* $Id$
 * Copyright (c) 1999 SICS 
 */

package se.sics.jasper;

/**
 * Thrown if SPCanonicalAtom could not register or un-register a
 * canonical atom representation. You should not create or
 * inherit from <code>AtomRegisterFailure</code>.
 * <p>
 * You should not catch an AtomRegisterFailure. Catch SPException instead,
 * since AtomRegisterFailure is not guaranteed to exist in future versions
 * of Jasper.
 */
public class AtomRegisterFailure extends SPException
{
    AtomRegisterFailure(SICStus sp, String msg) {
	super(sp, msg);
    }

//     /* obsolete */
//     /* [PM] 3.8.5 no longer public */ AtomRegisterFailure(String msg) {
// 	super(msg);
//     }
    
}
