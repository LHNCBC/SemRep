/* $Id$
 * Copyright (c) 1998 SICS 
 */

package se.sics.jasper;

import java.lang.*;
import java.util.HashMap;

/**
 * Represents a exception thrown by Jasper. You should not create or
 * inherit from <code>SPException</code> or its sub-classes.
 */
public class SPException extends Exception implements PrologException
{
    SICStus sp;
    private int errno;
  
    // [PD] 3.10.2 Use assertIndex instead to get the exception term.
    //protected SPTerm term;

    private String desc;

    // [PM] 3.8.5 
    protected String toStringString;

    // [PM] 3.9 moved to SICStus
    // private native String spErrorMessage(int errno);
    // private native int spGetErrno();

    // [PD] 3.10.2 Index which was used when the exception term was asserted.
    private int assertIndex;

    // [PD] 3.10.2 Instead of saving the exception term we assert the exception
    // term (this is done in spnative). Save an index so we later can retreive
    // the exception term.
    void initSPException(SICStus sp, int err_no, String spec, int termIndex, String termString)
    {
        if (sp.debugging(2)) {
            SICStus.dbgPrintln("SPException.initSPException():");
            SICStus.dbgPrintln("    sp==" + sp);
            SICStus.dbgPrintln("    err_no==" + err_no);
            SICStus.dbgPrintln("    spec==" + spec);
            SICStus.dbgPrintln("    termIndex==" + termIndex);
            SICStus.dbgPrintln("    termString==" + termString);
        }            
        this.sp = sp;
        this.errno = err_no;
        desc = spec;
        //        this.term = term;
        assertIndex = termIndex;
        toStringString = null;

        // [PM] 3.8.5 Use this code later perhaps and make
        // this.toString just return toStringString
        // [PD] 3.10.2 OK, let's do it!
        if (termString != null) {
            toStringString = termString;
        } else {
            String errdesc = null;
            if (err_no != 0) {
                synchronized (sp) {
                    errdesc = sp.spErrorMessage(err_no);
                }
                if (errdesc == "") {
                    errdesc = "Error #" + err_no;
                }
            }
       
            if (spec != null) {
                if (errdesc != null) {
                    toStringString = desc + ": >" + errdesc + "<";
                } else {
                    toStringString = desc + ": <unknown>";
                }
            } else {
                toStringString = errdesc;
            }
        }
    }


    SPException( SICStus sp, int err_no, String spec )
    {
// [PD] 3.10.2
//        initSPException(sp, err_no, spec, null);
        initSPException(sp, err_no, spec, 0, null);
    }

    // [PM] 3.8.5 Added SICStus arg
    /**
     * Creates an exception with the given specification.
     * @param spec Description of the error.
     */
    /* [PM] 3.8.5 no longer public */ SPException( SICStus sp, String spec )
    {
        // [PM] 3.8.5 was
        // //Thread.dumpStack();
        // desc = spec;
        // errno = sp.spGetErrno();
        // term = null;

        int err_no;
        if (sp!=null
            && sp.isLegalCaller() // [PM] 3.9.2b3 Avoid spGetErrno() from wrong caller when creating IllegalCallerException...
            && sp.initialized ) {
            synchronized (sp) {
                err_no = sp.spGetErrno();
            }
        } else {
            err_no = 0;
        }
// [PD] 3.10.2
//        initSPException(sp, err_no, spec, null);
        initSPException(sp, err_no, spec, 0, null);
    }

    /** 
     * Creates an exception containing a Prolog term. Prolog
     * exceptions thrown by <code>se.sics.jasper.SICStus#query</code> will be of
     * this type.  
     */
// [PD] 3.10.2 Removed in favor of SPException(SICStus sp, int termIndex)
//     /* [PM] 3.8.5 no longer public */ SPException( SICStus sp, SPTerm term )
//     {
//         int err_no = 0;
//         if (term == null
//             && sp.isLegalCaller() // [PM] 3.9.2b3 Avoid spGetErrno() from wrong caller
//             && sp.initialized) {
//             synchronized (sp) {
//                 err_no = sp.spGetErrno();
//             }
//         }
//         initSPException(sp, err_no, null, term);
// //         //Thread.dumpStack();
// //         if (term != null)
// //             this.term = term;
// //         else
// //             {
// //                 desc = null;
// //                 synchronized (term.sp) {
// //                     errno = sp.spGetErrno();
// //                 }
// //             }
//     }

    // [PD] 3.10.2 Called from SICStus.handleQueryResult() instead of the above constructor
    SPException( SICStus sp, String termString, int termIndex)
    {
        int err_no = 0;
        if (termIndex == 0
            && sp.isLegalCaller() // [PM] 3.9.2b3 Avoid spGetErrno() from wrong caller
            && sp.initialized) {
            synchronized (sp) {
                err_no = sp.spGetErrno();
            }
        }
        initSPException(sp, err_no, null, termIndex, termString);
    }


// 3.8.5 no longer used. Caller should always pass at least a descriptive string
//      /**
//       * Creates an undescribed exception.
//       */
//      /* [PM] 3.8.5 no longer public */ SPException(SICStus sp) // [PM] 3.8.5 added SICStus arg
//      {
//          Who uses this? what should err_no be?
//          initSPException(sp, err_no, null, null);
//  //         //Thread.dumpStack();
//  //         desc = null;
//  //         synchronized (sp) {
//  //             errno = sp.spGetErrno();
//  //         }
//      }



// [PD] 3.10.2 Since 'toString(Term option)' has been removed, this must be
// removed too.
//     /** 
//      * Returns a description of the exception. Equivalent to
//      * {@link #toString(Term) toString(sp.readFromString("[max_depth(10)]."))}
//      */
//     public String toString()
//     {
//         SPTerm option;
//         try {
//             option = sp.readFromString("[max_depth(10)].");
//         } catch (SPException spe) {
//             option = new SPTerm(sp);
//         }
//         return toString(option);
//     }

    /** 
     * Returns a description of the exception. If an error-description
     * was given, it is returned together with the error-string returned
     * from Prolog, if any.  
     */
    public String toString()

/* [PD] 3.9
   Normally the toString method may not call SICStus runtime. Return a string
   consisting of the name of the actual class of the exception (i.e.
   AtomRegisterFailure, IllegalCallerException, ConversionFailedException or
   IllegalCallerException, and the message string used in the constructor (if
   one exists).
   If sp.debugLevelValue >= 1 we use the old dangerous code which will call
   SICStus runtime.
   The above is no longer strictly true. We now test for legal caller.
*/
    {
        // [PM] 3.8.5 later: replace all this with return toStringString;
        // [PD] 3.10.2 Do exactly what he says above.
        if (sp.debugging(2)) SICStus.dbgPrintln("in SPException.toString()");
        return toStringString;
/*
        // DBG
        // System.out.println("SPException::toString");
        // System.out.println("SPException::toString sp==" + sp);
        // System.out.println("SPException::toString desc==" + desc);
        // System.out.println("SPException::toString term==" + ((term != null && term.isValid(sp)) ? term : null ));
       
        String errdesc = ( desc != null ? desc : "Unknown SPException" );

        // We want to synch on the SICStus object but sp is null if SICStus not
        // initialized.
        // Also make sure that calls into SICStus runtime is made only by the
        // legal caller.
        if (sp.debugging(2))
            SICStus.dbgPrintln("SPException.toString(): sp.isLegalCaller()==" +
                               sp.isLegalCaller());
        if (sp != null && sp.isLegalCaller() && sp.debugging()) { // [PD] 3.9
            synchronized (sp) {
                // [PD] 3.10.2 We don't have to test for 'sp != null' again
                //if (sp != null && sp.initialized) {
                if (sp.initialized) {
                    if (errno != 0) {
                        errdesc = sp.spErrorMessage(errno);
                    }
                } else {
                    // DBG
                    // Thread.dumpStack();
                    errdesc += " (SICStus not initialized)";
                }

                if (sp.debugging(2))
                    SICStus.dbgPrintln("SPException.toString(): term.isValid()==" + term.isValid());
                if (sp.initialized && term != null && term.isValid()) {
                    return term.toString();
                } else if (desc != null) {
                    return desc + ": >" + errdesc + "<";
                } else {
                    return errdesc;
                }
            }
        } else { // [PD] 3.9 if not debugging don't call SICStus runtime
            return this.getClass().getName() + ": >" + errdesc + "<";
        }
*/
    }


// [PD] 3.10.2 More or less bogus.
//
// // [PD] 3.10.2 There is no toStringString anymore, the exception term is not
// //             saved (since it becomes invalid to quickly, but we can get the
// //             exception term since it has been asserted.
// //             See initSPException().
//     public String toString(Term option)
//     {
//         String errdesc = (desc != null ? desc : "Unknown SPException");
//
//         if (sp != null && sp.isLegalCaller()) {
//             synchronized (sp) {
//                 if (sp.initialized) {
//                     if (assertIndex != 0) {
//                         try {
//                             HashMap varMap = new HashMap();
//                             sp.query("prolog:'$SPException'("
//                                      + assertIndex + ",Term).",
//                                      varMap);
//                             SPTerm term = (SPTerm)(varMap.get("Term"));
//                             if (term !=null) errdesc = term.toString(option);
//                         } finally { return errdesc; }
//                     }
//                     if (errno != 0) {
//                         String errmsg = sp.spErrorMessage(errno);
//                         errdesc += ": >" + errmsg + "<";
//                     }
//                 } else {
//                     errdesc += " (SICStus not initialized)";
//                 }
//             }
//         } else {
//             errdesc = this.getClass().getName() + ": >" + errdesc + "<";
//         }
//         return errdesc;
//     }

    /**
     * Returns the exception term. Returns null if there is no exception term.
     *
     */
    public Term getTerm() throws SPException
    {
        if (assertIndex == 0) return null;
        sp.checkLegalCaller();
        HashMap varMap = new HashMap();
        sp.query("prolog:'$SPException'(" + assertIndex + ",Term).", varMap);
        return (SPTerm)(varMap.get("Term"));
    }

    /* ***NOTE*** this method may *NOT* call SICStus runtime. */
    protected void finalize()
    {
        if (sp.debugging()) {
            SICStus.dbgPrintln("SPException.finalize() called (this is expected)");
        }
        sp.markAssertion(assertIndex);
    }

}



/** [PM] Keep the original indentation style
 *  Local variables:
 *      indent-tabs-mode: nil
 *      c-basic-offset: 4
 *  end:
 **/

