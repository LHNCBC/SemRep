/* Copyright(C) 2002, Swedish Institute of Computer Science */

/* Domain variable abstraction */
/*******************************/

#define DV_PRUNED_DOM 0x1	/* some domain element was pruned */
#define DV_PRUNED_MIN 0x2	/* min was adjusted */
#define DV_PRUNED_MAX 0x4	/* max was adjusted */
#define DV_PRUNED_VAL 0x8	/* domain is/became a singleton */
#define DV_SET_OK     0x10	/* means set \subseteq min..max */
#define DV_INTERVAL   0x20	/* domain is an interval */
#define DV_EXPORTED   0x40	/* dv_export() called, so set is unsafe */

struct dvar {
  TAGGED min;			/* kept up to date */
  TAGGED max;			/* kept up to date */
  TAGGED set;
  TAGGED cookie;
  SP_globref attr_ref;
  SP_globref var_ref;
  unsigned int flags;
};

typedef struct dvar *Dvar;

typedef struct {
  TAGGED min, max, fdset;
} DVITER;


#define dvar_refresh(dvar) {				\
  TAGGED m_temp = RefGlob((dvar)->attr_ref);		\
  TAGGED m_cookie, m_set;				\
							\
  DerefAttribute(m_temp,m_temp);			\
  (dvar)->min = DomainMin(m_temp);			\
  (dvar)->max = DomainMax(m_temp);			\
  (dvar)->set = m_set = DomainSet(m_temp);		\
  (dvar)->cookie = m_cookie = DomainSize(m_temp);	\
  (dvar)->flags = DV_SET_OK |				\
    (m_cookie==TaggedOne ? DV_PRUNED_VAL : 0) |		\
    (CTagToCdr(m_set)==EmptySet ? DV_INTERVAL : 0);	\
}							\

/* Purpose: 
     (Re)Initialize dvar when entering filtering alg.
   Synopsis:
     void dvar_init(dvar, attr_ref, var_ref);
   Arguments:
     Dvar dvar            - the dvar
     SP_globref attr_ref - SP_globref holding Prolog FD attribute
     SP_globref var_ref  - SP_globref holding Prolog FD varable
*/
extern void
dvar_init(Dvar dvar,
	  SP_globref attr_ref,
	  SP_globref var_ref);

extern void
dvar_init_temp(Dvar dvar,
	       TAGGED min,
	       TAGGED max);

extern void
dvar_init_ix(Dvar dvar,
	     TAGGED attr,
	     TAGGED var);

/* Purpose: 
     Get dereferenced domain variable.
   Synopsis:
     TAGGED dvar_get(dvar);
   Arguments:
     Dvar dvar            - the dvar
*/
extern TAGGED
dvar_get(Dvar dvar);

/* Purpose: 
     Clone a dvar, e.g. for constructive disjunction.
   Synopsis:
     void dvar_assign(dest, source);
   Arguments:
     Dvar dest            - destination dvar
     Dvar source          - source dvar
*/

#define dvar_assign(DEST,SOURCE) \
  dvar_assign_(wam, DEST,SOURCE)

extern void
dvar_assign_(Wam wam,
	     Dvar dest,
	     Dvar source);

/* Purpose:
     Access various dvar attributes.
   Synopsis:
     SP_BOOL   dvar_is_alias(dvar,dvar);
     SP_BOOL   dvar_is_integer(dvar);
     SP_BOOL   dvar_is_interval(dvar);
     SP_integer   dvar_min_l(dvar);
     SP_integer   dvar_max_l(dvar);
     TAGGED dvar_min_t(dvar);
     TAGGED dvar_max_t(dvar);
     TAGGED dvar_set(dvar);
     SP_integer   dvar_interval_count(dvar);
     SP_integer   dvar_value_count(dvar);
   Arguments:
     Dvar dvar          - the dvar
*/
#define dvar_is_pruned(DVAR)  ((DVAR)->flags & (DV_PRUNED_DOM|DV_PRUNED_MIN|DV_PRUNED_MAX))
#define dvar_is_integer(DVAR)  ((DVAR)->flags & DV_PRUNED_VAL)
#define dvar_is_integer_first(DVAR)  			\
((DVAR)->flags==(DV_PRUNED_VAL|DV_SET_OK|DV_INTERVAL))
#define dvar_is_interval(DVAR) ((DVAR)->flags & DV_INTERVAL)
#define dvar_min_t(DVAR) ((DVAR)->min)
#define dvar_min_l(DVAR) GetSmall0((DVAR)->min)
#define dvar_min_int(DVAR) GetSmall_int0((DVAR)->min)
#define dvar_max_t(DVAR) ((DVAR)->max)
#define dvar_max_l(DVAR) GetSmall0((DVAR)->max)
#define dvar_max_int(DVAR) GetSmall_int0((DVAR)->max)

extern SP_BOOL
dvar_is_alias(Dvar dv1,Dvar dv2);

#define dvar_set(DVAR)				\
  dvar_set_(wam, DVAR)

extern TAGGED
dvar_set_(Wam wam, Dvar dvar);


extern SP_integer
dvar_interval_count(Dvar dvar);


extern SP_integer
dvar_value_count(Dvar dvar);


/* Purpose:
     Compare a dvar with a set/interval/value.
   Synopsis:
     int    dvar_compare_set(dvar, set);
     int    dvar_compare_interval_l(dvar, min, max);
     int    dvar_compare_interval_t(dvar, tmin, tmax);
     SP_BOOL   dvar_contains_value_l(dvar, value);
     SP_BOOL   dvar_contains_value_t(dvar, tvalue);
   Arguments:
     Dvar dvar          - the dvar
     TAGGED set         - the set to compare with
     SP_integer min           - min. of the interval to compare with
     SP_integer max           - max. of the interval to compare with
     SP_integer value         - the value to test for membership
     TAGGED tmin        - min. of the interval to compare with
     TAGGED tmax        - max. of the interval to compare with
     TAGGED tvalue      - the value to test for membership
   int  values:
     FDI_EQUAL          - domain is equal to set/interval
     FDI_SUBSET         - domain is a struct subset of set/interval
     FDI_SUPERSET       - domain is a strict superset of set/interval
     FDI_DISJOINT       - domain does not intersect set/interval,
                          which is non-empty
     FDI_INTERSECT      - domain is neither subset nor superset,
                          but intersects set/interval
   SP_BOOL values:
     TRUE               - the value is in the domain
     FALSE              - the value is not in the domain
*/

#define dvar_compare_set(DVAR,S) \
  dvar_compare_set_(wam, DVAR,S)

extern int
dvar_compare_set_(Wam wam, Dvar dvar, TAGGED set);

#define dvar_compare_interval_l(DVAR,MIN,MAX) \
        dvar_compare_interval_t_(wam, DVAR,MakeSmall0(MIN),MakeSmall0(MAX))

#define dvar_compare_interval_t(DVAR,MIN,MAX) \
  dvar_compare_interval_t_(wam, DVAR,MIN,MAX)

extern int
dvar_compare_interval_t_(Wam wam, Dvar dvar, TAGGED tmin, TAGGED tmax);

#define dvar_contains_value_l(DVAR,VALUE) \
        dvar_contains_value_t(DVAR,MakeSmall0(VALUE))

extern SP_BOOL
dvar_contains_value_t(Dvar dvar, TAGGED tvalue);

/* Purpose:
     Get successor domain element.
   Synopsis:
     SP_integer   dvar_successor_l(dvar, val);
     TAGGED dvar_successor_t(dvar, tval);
   Arguments:
     Dvar dvar          - the dvar
     SP_integer val           - the predecessor element
                          -CLPFD_MAXINT means no predecessor
     TAGGED tval        - the predecessor element
                          Inf means no predecessor
   SP_integer   values:
     CLPFD_MAXINT       - no successor exists
     anything else      - successor
   TAGGED values:
     Sup                - no successor exists
     anything else      - successor
*/

extern SP_integer
dvar_successor_l(Dvar dvar,SP_integer val);

extern TAGGED
dvar_successor_t(Dvar dvar,TAGGED tval);

/* Purpose:
     Get predecessor domain element.
   Synopsis:
     SP_integer   dvar_predecessor_l(dvar, val);
     TAGGED dvar_predecessor_t(dvar, tval);
   Arguments:
     Dvar dvar          - the dvar
     SP_integer val           - the successor element
                          CLPFD_MAXINT means no successor
     TAGGED tval        - the successor element
                          Sup means no successor
   SP_integer   values:
     -CLPFD_MAXINT      - no predecessor exists
     anything else      - predecessor
   TAGGED values:
     Inf                - no predecessor exists
     anything else      - predecessor
*/

extern SP_integer
dvar_predecessor_l(Dvar dvar,SP_integer val);

extern TAGGED
dvar_predecessor_t(Dvar dvar,TAGGED tval);

/* Purpose:
     Iterate over domain intervals or values.
   Synopsis:
     void dviter_init(&it, dvar);
     SP_BOOL dviter_empty(&it);
     void dviter_next_interval_l(&it, &min, &max);
     void dviter_next_interval_t(&it, &tmin, &tmax);
     SP_integer dviter_next_value_l(&it);
     TAGGED dviter_next_value_t(&it);
     void dviter_skip_l(&it, value);
     void dviter_skip_t(&it, tvalue);
   Arguments:
     Dvar dvar          - the dvar
     DVITER it          - the iterator
     SP_integer   min         - min. of the next interval
     SP_integer   max         - max. of the next interval
     SP_integer   value       - the next value
     TAGGED tmin        - min. of the next interval
     TAGGED tmax        - max. of the next interval
     TAGGED tvalue      - the next value
     SP_BOOL values:
     TRUE               - no more intervals or values
     FALSE              - there are more intervals and values
*/

extern void 
dviter_init(DVITER *it, Dvar dvar);

#define dviter_empty(it) ((it)->fdset==EmptySet)

extern void
dviter_next_interval_l(DVITER *it, SP_integer *min, SP_integer *max);

extern void
dviter_next_interval_t(DVITER *it, TAGGED *tmin, TAGGED *tmax);

#define dviter_next_value_l(it) \
        GetSmall0(dviter_next_value_t(it))

extern TAGGED
dviter_next_value_t(DVITER *it);

#define dviter_skip_l(it,l) dviter_skip_t(it,MakeSmall0(l))

extern void
dviter_skip_t(DVITER *it, TAGGED t);

/* Purpose:
     Remove a set/interval/value from a dvar.
   Synopsis:
     int    dvar_prune_set(dvar, set);
     int    dvar_prune_interval_l(dvar, min, max);
     int    dvar_prune_interval_t(dvar, tmin, tmax);
     int    dvar_prune_value_l(dvar, value);
     int    dvar_prune_value_t(dvar, tvalue);
   Arguments:
     Dvar dvar          - the dvar to prune
     TAGGED set         - the set to remove
     SP_integer min           - min. of the interval to remove
     SP_integer max           - max. of the interval to remove
     SP_integer value         - the value to remove
     TAGGED tmin        - min. of the interval to remove
     TAGGED tmax        - max. of the interval to remove
     TAGGED tvalue      - the value to remove
   int  values:
     -1                 - failure (empty domain)
     >=0                - success, value is sum of DV_PRUNED_{DOM,MIN,MAX,VAL}
*/

#define dvar_prune_value_l(DVAR,L) dvar_prune_value_t(DVAR,MakeSmall0(L))

#define dvar_prune_value_t(DVAR,T) dvar_prune_interval_t_(wam, DVAR,T,T)

#define dvar_prune_interval_l(DVAR,L,U) \
        dvar_prune_interval_t_(wam, DVAR,MakeSmall0(L),MakeSmall0(U))

#define dvar_prune_interval_t(DVAR,L,U) \
  dvar_prune_interval_t_(wam, DVAR,L,U)

#define dvar_prune_set(DVAR,S) \
  dvar_prune_set_(wam, DVAR,S)

extern int 
dvar_prune_interval_t_(Wam wam, Dvar dvar,TAGGED lbt,TAGGED ubt);

int 
dvar_prune_set_(Wam wam, Dvar dvar,TAGGED set);

/* Purpose:
     Fix a dvar to a set/interval/value.
   Synopsis:
     int    dvar_fix_set(dvar, set);
     int    dvar_fix_interval_l(dvar, min, max);
     int    dvar_fix_interval_t(dvar, tmin, tmax);
     int    dvar_fix_min_l(dvar, min);
     int    dvar_fix_min_t(dvar, tmin);
     int    dvar_fix_max_l(dvar, max);
     int    dvar_fix_max_t(dvar, tmax);
     int    dvar_fix_value_l(dvar, value);
     int    dvar_fix_value_t(dvar, tvalue);
   Arguments:
     Dvar dvar          - the dvar to prune
     TAGGED set         - the set to impose
     SP_integer min           - min. of the interval to impose
     SP_integer max           - max. of the interval to impose
     SP_integer value         - the value to impose
     TAGGED tmin        - min. of the interval to impose
     TAGGED tmax        - max. of the interval to impose
     TAGGED tvalue      - the value to impose
   int  values:
     -1                 - failure (empty domain)
     >=0                - success, value is sum of DV_PRUNED_{DOM,MIN,MAX,VAL}
*/

#define dvar_fix_value_l(DVAR,L) \
        dvar_fix_value_t(DVAR,MakeSmall0(L))

#define dvar_fix_value_t(DVAR,T) \
        dvar_fix_interval_t(DVAR,T,T)

#define dvar_fix_interval_l(DVAR,L,U) \
        dvar_fix_interval_t(DVAR,MakeSmall0(L),MakeSmall0(U))

#define dvar_fix_set(DVAR,S) \
  dvar_fix_set_(wam,DVAR,S)

#define dvar_fix_min_l(DVAR,L) \
  dvar_fix_min_t(DVAR,MakeSmall0(L))

#define dvar_fix_min_t(DVAR,TL) \
        dvar_fix_interval_t(DVAR,TL,Sup)

#define dvar_fix_max_l(DVAR,U) \
        dvar_fix_interval_t(DVAR,Inf,MakeSmall0(U))

#define dvar_fix_max_t(DVAR,UL) \
        dvar_fix_interval_t(DVAR,Inf,UL)

extern int 
dvar_fix_interval_t(Dvar dvar,TAGGED lbt,TAGGED ubt);

extern int 
dvar_fix_set_(Wam wam, Dvar dvar,TAGGED set);

/* Purpose: 
     Export dvars to Prolog environment.
     Signal success/failure/entailment.
     1. Enter propagator: dvar_export_start(wam);
     2. For each dvar: dvar_pruning_done(dvar)
     3. For each dvar: dvar_export(dvar);
     4. dvar_export_done(wam,actions, ent);
   Synopsis:
     void dvar_pruning_done(dvar);
     void dvar_export_start(wam);
     void dvar_export(dvar);
     void dvar_export_done(wam,actions, ent);
   Arguments:
     Dvar dvar            - the dvar
     int  ent             - the dvar
     SP_term_ref actions  - SP_term_ref holding actions list
*/

#define dvar_pruning_done(DVAR) \
  dvar_pruning_done_(wam, DVAR)

extern void
dvar_pruning_done_(Wam wam, Dvar dvar);

#define dvar_export_start(wam) 			\
  (X(EVAL_ARITY-1) = atom_nil, w->numstack_end = NULL)

#define dvar_export(dv) {					\
if ((dv)->flags & (DV_PRUNED_DOM|DV_PRUNED_MIN|DV_PRUNED_MAX))	\
  dvar_export_do(wam,dv);					\
(dv)->set = 0xfddeadfd;						\
}								\

extern void
dvar_export_do(Wam wam, Dvar dvar);

extern void
dvar_export_ix(Wam wam, Dvar dvar, SP_BOOL fdbg);

extern void
dvar_export_equal(Wam wam, Dvar dv1, Dvar dv2);

extern void
dvar_export_leqc(Wam wam, Dvar dv1, Dvar dv2, int c);

extern void
dvar_export_alldiff(Wam wam, Dvar *dv, int n);

extern void
dvar_export_done(Wam wam, SP_term_ref Actions, int ent);


/* attaching a daemon */
/* Term = daemon(Global,AttrRef,StatusM,Ent,RawHandle) */
extern void 
dvar_attach_daemon(Wam wam,
		   Dvar dv,
		   void *handle,
		   TAGGED global,
		   TAGGED list_functor);

/* accellerator for dvar_fix_value + dvar_export in one go.  Can GC. */
extern SP_BOOL
bool_export_value(Wam wam, SP_globref attr_ref, SP_globref var_ref, int value);


#if DBG
extern void dvar_dump(Dvar dv);
#endif /* DBG */

#if DBG
extern void dvar_validate(Wam wam);
#endif


/* TODO:
   - handle inf/sup consistently for SP_integer typed ops
   - handle unification
*/
