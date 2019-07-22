/* Copyright (c) 1998 SICS */

package se.sics.jasper;

/**
 * <strong>Deprecated</strong>
 * Instead use module and predicate name as strings directly
 * or specify the goal as a string.
 *
 * <p> You should not inherit from <code>SPPredicate</code>.
 *
 * @see se.sics.jasper.SICStus#query
 * @see se.sics.jasper.SICStus#queryCutFail
 * @see se.sics.jasper.SICStus#openQuery
 */
public class SPPredicate
{
    SICStus sp;
    String module;
    String name;
    int arity;

    long nativePredRef;
    // native long spMakePredRef( SICStus sp, String name, int arity, String module);
    // native long spMakePredRefCanonical( SICStus sp, long name, int arity, long module);

    private void initSPPredicate ( SICStus sp, String name, int arity, String module )
        throws SPException
    {
        this.sp = sp;
        if (! (0 <= arity && arity <= 255) ) throw new SPException(sp, "Predicate arity not in [0..255]");
        this.arity = arity;
        if (name == null) throw new NullPointerException("Predicate name is null");
        this.name = name;

        if (module == null || module.equals("")) { // use type-in module
            synchronized (sp) {
                SPQuery context = sp.openContext();
                try {
                    
                    java.util.HashMap map = new java.util.HashMap();
                    sp.query("prolog_flag(typein_module, X).", map);
                    module = ((SPTerm)map.get("X")).getString();
                }
                // catch (SPException e) {
                //     throw e;
                // }
                finally {
                    context.close();
                }
            }
        }
        this.module = module;
    }

    /**
     * 
     * <strong>Deprecated</strong> Creates a predicate reference.
     * Instead use module and predicate name as strings directly
     * or specify the goal as a string.
     * 
     * @param sp A reference to the SICStus object to which this
     * predicate belongs to.
     * @param name The predicate's name.
     * @param arity The arity of the predicate (in [0..255])
     * @param module The module to use whe calling the predicate.
     * When calling the predicate the module (M) specified
     * is used as if the call was call(M:NAME(ARGS)). This means that
     * the module specified will affect calls to meta preciates and
     * goal expansion.
     * For backward compatibility module can be null or "" to signify
     * the type-in module but this use is deprecated (and slow).
     * 
     * @throws SPException The predicate reference could not be
     * created. The usual cause for this is that the predicate is not
     * defined (although detecting undefined predicates in this manner
     * is <strong>not</strong> guaranteed and will not happen by
     * default as of 3.8.5).
     */
    public SPPredicate(SICStus sp, String name, int arity, String module)
        throws SPException
    {
        initSPPredicate(sp, name, arity, module);

        // [PM] 3.8.5 this was done up to 3.8.5beta1
        //  this.sp = sp;
        //  synchronized (sp) {
        //      this.nativePredRef = 
        //          spMakePredRef(sp, name, arity, (module==null)?"":module);
        //  }
        //
        //  if (nativePredRef == 0) {
        //      throw new SPException(sp, sp.getExceptionTerm());
        //  }
    }

    /**
     * <strong>Deprecated</strong> Creates a predicate reference.
     * Instead use module and predicate name as strings directly
     * or specify the goal as a string.
     *
     * @param sp A reference to the SICStus object to which this
     * predicate belongs to.
     * @param name The predicate's name.
     * @param arity The arity of the predicate (in [0..255])
     * @param module The module to use whe calling the predicate.
     * When calling the predicate the module (M) specified
     * is used as if the call was call(M:NAME(ARGS)). This means that
     * the module specified will affect calls to meta preciates and
     * goal expansion.
	 * @throws SPException if an Prolog exception was thrown
     */
    public SPPredicate( SICStus sp, SPCanonicalAtom name, int arity, SPCanonicalAtom module )
        throws SPException
    { 
        initSPPredicate(sp, name.toString(), arity, ( (module == null) ? null : module.toString()));
        // [PM] 3.8.5 this was done up to 3.8.5beta1
        //  this.sp = sp;
        //  synchronized (sp) {
        //      name.CheckValid(sp);
        //      if (module != null) module.CheckValid(sp);
        //      this.nativePredRef = 
        //          spMakePredRefCanonical(sp, name.getAtomNumber(), arity,
        //                                 (module != null)?module.getAtomNumber():SPCanonicalAtom.atomNil);
        //  }
        //  if (nativePredRef == 0) {
        //      throw new SPException(sp, sp.getExceptionTerm());
        //  }
        //  this.arity = arity;
    }
}


/** [PM] Keep the original indentation style
 *  Local variables:
 *      indent-tabs-mode: nil
 *      c-basic-offset: 4
 *  end:
 **/

