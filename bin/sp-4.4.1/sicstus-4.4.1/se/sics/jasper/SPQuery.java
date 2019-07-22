/*
 * Copyright (c) 1998 SICS 
 */

package se.sics.jasper;

/** This class is used for holding a query-reference which is used to
 * find multiple solutions to a query. This class is never
 * instantiated explicitly by the user.
 * 
 * <p> You should not inherit from <code>SPQuery</code>.
 *
 * @see se.sics.jasper.SICStus#openQuery
 *
 */

public class SPQuery implements Query
{

    // [PM] 3.8.5 now reset to null to mark as (and cause exception if) closed 
    SICStus sp;
    long nativeQueryRef;        // may be SP_ILLEGAL_QID
                                // (see SICStus.(Meta)Glue_Push/PopContext)

    // 3.8.5 Surrounding context.
    // closed (cut) as last action of this.close() (this.cut())
    // Used in order to deallocate SP_term_refs that got created as part
    // of setting up this query. See SICStus.openQueryFromString
    SPQuery context=null;       // protected by synchronized (sp)

    // A free list of nativeTermRefs;
    java.util.TreeSet freeList = null;


    // [PM] 3.8.5 sp.term_stack as it was when this SPQuery was created.
    // sp.term_stack should be reset to term_stack_mark whenever
    // Prolog would reset its SP_term_ref stack. This should be done in
    // cut/close/next and when returning to Prolog from Java (see SICStus.(Meta)Glue_Push/PopContext)
    SPTerm term_stack_mark;

    long term_stack_index_mark = 0; // [PM] 3.8.7 SPRM 2495 SP_reset_term_refs(mark) when the query is closed/cut by prolog runtime
    
    SPQuery next;               // [PM] 3.8.5 the next older query. See SICStus query_stack.

    // used in order to find the term_stack_mark containing a given SPTerm.
    SPQuery predecessor()
    {
        SPQuery prev;
        SPQuery x;

        // We assume that the query-stack is fairly short. If  this
        // becomes a problem we should use some less naive search.
        for (prev = null, x = sp.query_stack; x != this; x = x.next) {
            prev = x;
        }
        return prev;
    }

    // The age of this choice point (higher is younger)
    int ageTick;                // == next.age()+1 if a real choice point else next.age()
    int age()
    {
        return ageTick;
    }
  
    // [PM] 3.9 moved to SICStus
    // // [PM] 3.8.5 the sp arg should always be this.sp
    // private native int spNextSolution(SICStus sp, long ref);
    // private native void spCutQuery(SICStus sp, long ref);
    // private native void spCloseQuery(SICStus sp, long ref);

    static final int SP_ILLEGAL_QID = 0;


    void dumpQueryStack()
    {
        SICStus.dbgPrintln("Query stack dump:");
        SPQuery x = this;
        for (x=this; x != null; x=x.next) {
            SICStus.dbgPrintln("Query " + x.superToString()
                               + " term_stack_mark=" + ( (x.term_stack_mark == null) ? "NULL" : x.term_stack_mark.superToString())
                               + " freeList=" + x.freeList);
        }
    }

    String superToString()
    {
        return super.toString();
    }

    void setContext(SPQuery c)
        throws SPException
    {
        synchronized (sp) {
            if (c == this.next) {
                context = c;
            } else {
                throw new SPException(sp, "setContext: context != this.next");
            }
        }
    }
    
    void popToTermStackMark()
    {
        sp.popToTermStackMark(term_stack_mark);
        /*
          {
              synchronized (sp) {
                  // Do not attempt SPTerm toString here (e.g., for debug)
                  // System.out.println("term_stack_mark==" + term_stack_mark.nativeTermRef);
                  while (sp.term_stack != term_stack_mark) {
                      // System.out.println("sp.term_stack==" + sp.term_stack.nativeTermRef);
                      // System.out.println("sp.term_stack.next==" + sp.term_stack.nativeTermRef);
                      SPTerm tmp = sp.term_stack;
                      sp.term_stack = tmp.next;
                      tmp.invalidate();
                  }
              }
          }
        */
    }
  
    void popToQuery(SPQuery query)
    {
        SICStus spTmp = sp;     // this.sp will become zapped when query == this
        synchronized (spTmp) {
            while (spTmp.query_stack != query) {
                SPQuery tmp = spTmp.query_stack;
                spTmp.query_stack = tmp.next;
                tmp.invalidate();
            }
        }
    }

    void popToQuery()
    {
        popToQuery(this);
    }


    void invalidate()
    {
        sp = null;
    }

    // must be called while synchronized on the sp object
    void reuseTermRef(long termRef)
        throws SPException
    {
        if (sp.debugging()) {
            SICStus.dbgPrintln("reuseTermRef: termRef=" + termRef + ", freeList = " + freeList);
            if (sp.debugging(2)) SICStus.dbgPrintln("reuseTermRef: this=" + this.toString());
            if (sp.debugging(2)) SICStus.dbgPrintln("reuseTermRef: term_stack_mark=" + term_stack_mark);
        }

        if (term_stack_mark != null) {
            // if (sp.debugging(2)) SICStus.dbgPrintln("reuseTermRef: (term_stack_mark != null)");

            // extra error check.
            if (!term_stack_mark.isValid()) { // invariant violation if this should be false
                String msg = "** Internal Jasper Error: ERROR: !term_stack_mark.isValid() in reuseTermRef";
                if (sp.debugging()) SICStus.dbgPrintln(msg);
                sp.term_stack.dumpTermStack();
                throw new SPException(sp, 0, msg);
            }
            long tmp = term_stack_mark.GetNativeTermRef();

            // if (sp.debugging(2)) SICStus.dbgPrintln("reuseTermRef: tmp=" + tmp + ", termRef=" + termRef);
            
            // extra error check.
            if (! (termRef > tmp)) { // tmp belongs to the next choice point so termRef should be strictly newer 
                String msg =
                    "** ERROR: Internal Jasper Error: Trying to reuse a too old SP_term_ref "
                    + "(reuse " + termRef + ", "
                    + "mark=" + tmp + ")";
                if (sp.debugging()) {
                    SICStus.dbgPrintln(msg);
                    sp.term_stack.dumpTermStack();
                }
                throw new SPException(sp, 0, msg);
            }

            if (freeList == null) {
                // if (sp.debugging(2)) SICStus.dbgPrintln("reuseTermRef: creating freeList");
                freeList = new java.util.TreeSet();
                // if (sp.debugging(2)) SICStus.dbgPrintln("reuseTermRef: created freeList=" + freeList);
            }
            if (!freeList.add(new Long(termRef))) {
                // invariant violation, was already present!
                String msg = "** ERROR: Internal Jasper Error: termRef(" + termRef + ") already in freeList = " + freeList;
                if (sp.debugging()) SICStus.dbgPrintln(msg);
                throw new SPException(sp, 0, msg);
            }
            // if (sp.debugging(2)) SICStus.dbgPrintln("reuseTermRef: Added to freeList=" + freeList);
        }
    }

    // must be called when synchronized on the sp object
    long getTermRef()
    {
        if (sp.debugging()) SICStus.dbgPrintln("getTermRef: freeList = " + freeList);
        if ( freeList == null
             || freeList.isEmpty() ) {
            return SPTerm.SP_ILLEGAL_TERM_REF;
        }
        Long x = (Long)freeList.first();
        freeList.remove(x);
        return x.longValue();
    }

    void InitSPQuery(SICStus sp, long qid, long mark)
    {
        this.sp = sp;
        nativeQueryRef = qid;
        term_stack_mark = sp.term_stack;
        term_stack_index_mark = mark;

        next = sp.query_stack;
        sp.query_stack = this;
        // the age of the empty term stack is zero
        int nextTick = ( next == null ? 0 : next.age() ); // age of the next older choice point
        if (qid == SP_ILLEGAL_QID) {
            // we are not a real choice point so we inherit the age.
            ageTick = nextTick;
        } else {
            // We are a real choice point so is strictly younger
            ageTick = nextTick+1;
        }
    }

    // [PM] Called with qid == SP_ILLEGAL_QID from
    // SICStus.(Meta)Glue_PushContext to mark a term_stack level
    // Also from initSICStus to ensure the query stack is never empty
    // Must be called within a synchronized (sp) {} block
    SPQuery(SICStus sp, long qid, long mark)
    {

        InitSPQuery(sp, qid, mark);

//         this.sp = sp;
//         nativeQueryRef = qid;
//         term_stack_mark = sp.term_stack;
//         term_stack_index_mark = mark
//
//         next = sp.query_stack;
//         sp.query_stack = this;
//         // the age of the empty term stack is zero
//         int nextTick = ( next == null ? 0 : next.age() ); // age of the next older choice point
//         if (qid == SP_ILLEGAL_QID) {
//             // we are not a real choice point so we inherit the age.
//             ageTick = nextTick;
//         } else {
//             // We are a real choice point so is strictly younger
//             ageTick = nextTick+1;
//         }
    }
  
    SPQuery(SICStus sp, long qid)
    {
        long illegalMark = 0;
        InitSPQuery(sp, qid, illegalMark);
    }

    /** Gets the next solution for the query.

     * Returns <code>false</code> when there are no more
     * solutions. When no more solutions are needed, the query must be
     * closed using the method <code>close()</code>.
     <p>
     * Multiple queries can be open at the same time, but the calls to
     * nextSolution must be nested, i.e. refer to the most recently
     * opened query.  Invalidates all <code>SPTerm</code> objects
     * created since this query was created
     <p>
     * Invalidates all SPTerm objects created since this query was
     * created If you need to keep data created by the previous call
     * to <code>nextSolution</code> to this query (i.e. data referred
     * to by {@link se.sics.jasper.SPTerm SPTerm} objects), you need
     * to convert it to Java datatypes before calling this function.

     *
     * @see se.sics.jasper.SICStus#openQuery
     * @see se.sics.jasper.SPQuery#close
     * @see se.sics.jasper.SPQuery#cut
     * 
     * @throws SPException If a Prolog exception was thrown during
     * the query.
     * @return False if there are no more solutions, True
     * otherwise.  */
    public synchronized boolean nextSolution()
	throws SPException
    {
        // [PM] 3.8.5 sp.checkLegalCaller();
        // sp is null if invalidatet so this is intended to give null
        // pointer exception
        synchronized (sp) {  // This restriction is really unfortunate but (almost) unavoidable
            try {
                popToTermStackMark();
                return sp.handleQueryResult(sp.spNextSolution(nativeQueryRef));
            }
            finally {

                // Incorrectly calling popToTermStackMark here will
                // leave illegal SPTerm live over the call *and* will
                // invalidate the SPException term on exception.
                // WRONG: popToTermStackMark();
                popToQuery();
            }
        }
    }
  
    /** Closes a query. All remaining solutions to the query are
     * discarded, and the state of the Prolog engine will be restored
     * to the state it was in when {@link
     * se.sics.jasper.SICStus#openQuery openQuery} was called. If the
     * query is not the most recently opened, all still open queries
     * opened after this query will be closed as well.
     <p>
     * A {@link se.sics.jasper.SPQuery#close closed} or {@link
     * se.sics.jasper.SPQuery#cut cut} query is invalidated and most
     * operations on it will throw an exception. In particular you
     * should not call <code>close</code>, <code>cut</code> or
     * <code>nextSolution</code> on an invalidated query.
     <p>
     * Invalidates all SPTerm objects created since this
     * query was created
     * If you need to keep data created by the query (i.e. data
     * referred to by {@link se.sics.jasper.SPTerm SPTerm} objects),
     * you need to convert it to Java datatypes before calling this
     * function.
     **/
    public synchronized void close()
    {
//         if (sp.debugging(2))
//             SICStus.dbgPrintln("ENTER SPQuery.close()");
        // [PM] 3.8.5 sp.checkLegalCaller();
        if (sp != null) {       // make it legal to close multiple times
            SICStus mySP = sp;

            synchronized (sp) {
                SPQuery c = context;
                long qid = nativeQueryRef;
                long mark = term_stack_index_mark;

//                 if (mySP.debugging(2))
//                     SICStus.dbgPrintln("SPQuery.close(): popToTermStackMark");
                popToTermStackMark();
//                 if (mySP.debugging(2))
//                     SICStus.dbgPrintln("SPQuery.close(): popToQuery");
                popToQuery(this.next); // will call this.invalidate()

                // System.out.println("SYNCH sp.close()");
                try {
                    if (qid != SP_ILLEGAL_QID) {
//                         if (mySP.debugging(2))
//                             SICStus.dbgPrintln("SPQuery.close(): spCloseQuery");
                        mySP.spCloseQuery(qid); // ! beware: could call cleanup goals
                    }
                }
                finally {
                    if (mark != 0) {
                        mySP.resetTermRefs(mark);
                    }
                    // SPQuery c = context;
                    // popToTermStackMark();
                     // popToQuery(this.next); // will call this.invalidate()
                    if (c != null) {
                        // invariant: this.next==c
                        c.close();
                    }
//                     if (mySP.debugging(2))
//                         SICStus.dbgPrintln("LEAVE finally-block in SPQuery.close(), this==" + this);
                }
            }
        }
//         if (sp.debugging(2))
//             SICStus.dbgPrintln("LEAVE SPQuery.close(), this==" + this);
    }

    /** Discards choices made since this query object was created, like
     * the goal <code>!</code>.
     <p>
     * If the query is not the most recently opened, all still open
     * queries opened after this query will be cut as well.
     <p>
     * A {@link se.sics.jasper.SPQuery#close closed} or {@link
     * se.sics.jasper.SPQuery#cut cut} query is invalidated and most
     * operations on it will throw an exception. In particular you
     * should not call <code>close</code>, <code>cut</code> or
     * <code>nextSolution</code> on an invalidated query.
     <p>
     * Invalidates all SPTerm objects created since this
     * query was created
     * If you need to keep data created by the query (i.e. data
     * referred to by {@link se.sics.jasper.SPTerm SPTerm} objects),
     * you need to convert it to Java datatypes before calling this
     * function.
     **/
    public synchronized void cut()
	throws SPException
    {
        // [PM] 3.8.5 sp.checkLegalCaller();
        // sp is null if invalidated so this is intended to give null
        // pointer exception
        SICStus mySP = sp;

        synchronized (sp) {
            SPQuery c = context;
            long qid = nativeQueryRef;
            long mark = term_stack_index_mark;

            popToTermStackMark();
            popToQuery(this.next); // invalidates this

            try {
                if (qid != SP_ILLEGAL_QID) {
                    mySP.spCutQuery(qid); // ! beware: could call cleanup goals
                }
            }
            finally {
                if (mark != 0) {
                    mySP.resetTermRefs(mark);
                }

                // SPQuery c = context;
                // popToTermStackMark();
                // popToQuery(this.next);
                if (c != null) {
                    // invariant: this.next==c (apart from the fact
                    // the this just got invalidated)
                    c.cut();
                }
            }
        }
    }
}



/** [PM] Keep the original indentation style
 *  Local variables:
 *      indent-tabs-mode: nil
 *      c-basic-offset: 4
 *  end:
 **/

