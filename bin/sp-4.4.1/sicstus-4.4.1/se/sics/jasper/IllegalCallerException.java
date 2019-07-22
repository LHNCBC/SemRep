/* $Id$
 * Copyright (c) 1999 SICS 
 */

package se.sics.jasper;

import java.lang.*;

// [PD] 3.10.1 This can be seen by user code, if the user is doing the
// wrong thing. Remove the following line from the Javadoc text.
// * Reinstated in 3.9. This should never be seen by user code.
/**
 * Thrown if SICStus runtime is called from a thread which is not the
 * creator of the SICStus runtime.
 * <p> See the class {@link se.sics.jasper.Jasper} for a way to call
 * SICStus runtime from multiple threads.
 * <p> You should not create or inherit from
 * <code>IllegalCallerException</code>. 
 * <p>
 * You should not catch an IllegalCallerException. Catch SPException instead,
 * since IllegalCallerException is not guaranteed to exist in future versions
 * of Jasper.
 */
public class IllegalCallerException extends SPException
{
    /*public*/ IllegalCallerException(SICStus sp, Thread expected, Thread found) {
        super(sp, "illegal caller: expected thread " + 
              expected + ", found " + found);
    }

    // [PM] 3.8.5 obsolete
    //     public IllegalCallerException(Thread expected, Thread found) {
    //      super("illegal caller: expected thread " + 
    //            expected + ", found " + found);
    //     }
}




/** [PM] Keep the original indentation style
 *  Local variables:
 *      indent-tabs-mode: nil
 *      c-basic-offset: 4
 *  end:
 **/

