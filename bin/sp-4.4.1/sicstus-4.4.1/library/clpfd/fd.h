/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "datadefs.h"
#include "support.h"
#include "predtyp.h"
#include "clpfd_glue.h"         /* [PM] 3.9b4 splfr-generated   */

/* [PM] 4.2.2 We use internal things, like BindHVA, that uses
   choice_overflow which is no longer exported. Use the external name
   sp_choice_overflow instead. */
#define choice_overflow sp_choice_overflow

#if !MULTI_SP_AWARE
/* [PM] 4.0.5 As per Mats' request we now use MULTI_SP-style dispatch
   parameter also when compiled for static resource. */
#error "[PM] 4.0.5 clpfd now requires configure --enable-clpfd-multi-sp=always"
#endif  /* !MULTI_SP_AWARE */

#ifdef sp_make_integer          /* [PM] 3.10.1 */
#ifdef make_integer
#error "Did not expect make_integer to be #defined here/PM"
#endif /* make_integer */
/* make_integer (as used by, e.g., MakeInteger) is
   no longer exported from the support.h API. Instead use the new
   sp_make_integer with identical functionality. */
#define make_integer sp_make_integer
#endif /* sp_make_integer */

/* [PM] 4.3 For functions that do not have a MULTI_SP argument you can
   use MakeSmall0() instead of MakeSmall(), and similar for
   GetSmall0(), et al.

   This is needed in DBG where these call error checking versions in
   the SICStus API.

   The long-term solution is to ensure that all clpfd functions take
   the MULTI_SP argument, e.g. by ripping out the support for
   non-multi-sp compilation.

*/
#define MakeSmall0 MakeSmall_
#define GetSmall0 GetSmall_
#define GetSmall_int0 GetSmall_int_
#define GetSmall_uint0 GetSmall_uint_

/* [PM] 3.9.2b1 SPRM 3610
   FD_CONSTRAINT predicates need to be destructed using free_fd_info.

   Prior to 3.9.2b1 the pointer to the destructed was kept with the
   FD_CONSTRAINT predicate. However, the pointer was invalid when used
   by prolog_restore to undefine all predicates since the clpfd
   resource had by then been unloaded. It "used to work" because the
   clpfd resource (shared object) apparently always ended up at the
   same address when re-loaded.

   The bug surfaced when the SPEnv stored in the clpfd resource
   descriptor got munged so that free_fd_info got a segfault trying to
   use the SP API dispatch table.

   The fix is to tell SP about the destructor separately (using
   SP_install_fd_hooks). This will also save two words for each
   indexical_info struct.
 */

typedef SPEnv *Wam;
#define wam spenv_arg
#define w ((struct worker *)spenv_arg->fli_data)
#define fd (*(struct fd_state*)*(SP_foreign_stash()))

/* destructors use this to retrieve the spenv, e.g. FD_SETUP_SPENV(pdata->spenv); */
#define FD_SETUP_SPENV(SPENV) Wam wam=(SPENV);
/* use this to initialize the field in the memory block, e.g. FD_STORE_SPENV(pdata->spenv); */
#define FD_STORE_SPENV(SPENV) (SPENV)=wam

struct size_mem {
  size_t size;
  void *mem;
};

/* new persistent storage allocation */
/* Handle is a term [Flag | '$free'(Ptr)] */
#define Palloc(What,Extra,Handle) ((What *)fd_perm_alloc(wam,sizeof(What)+(Extra),Handle))

#define Pdata(What,Handle) ((What *)fd_perm_data(Handle))

#define Pfree fd_perm_free(wam)

#define Pcommitted(Handle) (TagToLST(Handle) >= w->global_uncond)

extern void *fd_perm_alloc(Wam wam,size_t,TAGGED);
extern void *fd_perm_data(TAGGED);
extern void fd_perm_free(Wam wam);
extern void fd_common_done(Wam wam, int argno);

extern TAGGED
fd_unify_output_state(Wam wam, TAGGED *phandle, SP_integer *pstamp, SP_BOOL *pcommitted);

extern TAGGED
fd_static_output_state(Wam wam, TAGGED *phandle, SP_BOOL *pcommitted);

extern int
fd_list_length(TAGGED tvec);

extern void
fd_get_var_and_attr(TAGGED term, SP_globref ref);

extern TAGGED
fd_deref(Wam wam, TAGGED term);

/* [PM] 4.0 Like SP_malloc but does MEMORY_FAULT instead of returning NULL on out-of-memory */
extern void *
fd_malloc(Wam wam, size_t size);

extern void *
fd_realloc(Wam wam, void* ptr, size_t size);

extern void *
fd_temp_alloc(Wam wam, size_t nchars);

extern void *
fd_perm_alloc2(Wam wam, size_t nchars, struct size_mem *size_mem);

extern void
fd_perm_free2(Wam wam, void *ptr, struct size_mem *size_mem);

extern struct fd_state *
fd_state(Wam wam);

extern void
fd_not_fixpoint(Wam wam);

/* temporary storage allocation */
#define Malloc(N,What) ((What *)fd_malloc(wam, (N)*sizeof(What)))
#define Realloc(Ptr,NewSize,What) ((What *)fd_realloc(wam, (void *)(Ptr),(NewSize)*sizeof(What)))
#define Free(Ptr) SP_free(Ptr)

/* numstack allocation -- BEWARE: temp domains are on numstack 

   Protocol for allocating & returning memory:

   TAGGED *top;
   NumstackAlloc(0,top);

   [ ... code that conses on numstack but DOES NOT modify any fdset
     of any dvar ... use TempAlloc here ... ]

     numstack_trim(w,top);
*/
#define TempAlloc(N,What) ((What *)fd_temp_alloc(wam,(N)*sizeof(What)))

/* Only for global constraint state allocation */
#define PermAlloc(N,What,SizeMem) ((What *)fd_perm_alloc2(wam, (N)*sizeof(What),SizeMem))
#define PermFree(Ptr,SizeMem)     fd_perm_free2(wam, Ptr,SizeMem)

#define EVAL_ARITY 3

#define EmptySet atom_nil

#define Inf atom_inf

#define Sup atom_sup

/* something smaller than all smallints */
#define InfAsINT (TaggedLow-1)

/* something greater than all smallints */
#define SupAsINT (TaggedHigh+1)

#define CLPFD_MAXINT  ((SP_integer)(((TAGGED)(-1)) >> 1))
#define CLPFD_MAXINT2 ((SP_integer)(((TAGGED)(-1)) >> 2))
#define CLPFD_MAXINT4 ((SP_integer)(((TAGGED)(-1)) >> 4))

/* support for x*x=y */
#define SquareIntIsSmall(I) ((I) < 1L << ((WORDSIZE-4)/2))

#define FDMIN(A,B) ((A)<=(B) ? (A) : (B))
#define FDMAX(A,B) ((A)>=(B) ? (A) : (B))

#define ABS(A) ((A) < 0 ? -(A) : (A))

/* an FD Set is [First|Rest] */

/* a range is [Min|Max] */
#define RangeMin(X) CTagToCar(X)
#define RangeMax(X) CTagToCdr(X)

/* a domain is dom(Set,Min,Max,Size) */
#define DomainSet(D) CTagToArg(D,1)
#define DomainMin(D) CTagToArg(D,2)
#define DomainMax(D) CTagToArg(D,3)
#define DomainSize(D) CTagToArg(D,4)
#define DomainSizeAsInt(D) (DomainSize(D)==Sup ? CLPFD_MAXINT2 : GetSmall0(DomainSize(D)))

#define FDdecr(T) ((T)!=TaggedLow  ? (T)-IStep(1) : Inf)
#define FDincr(T) ((T)!=TaggedHigh ? (T)+IStep(1) : Sup)

#define FDgt(A,B) (AreSmall(A,B) ? Tgt(A,B) : (A)==(B) ? FALSE : ((A)==Sup || (B)==Inf))
#define FDlt(A,B) FDgt(B,A)
#define FDle(A,B) (!FDgt(A,B))
#define FDge(A,B) (!FDlt(A,B))

#define InInterval(X,A,B) (!FDgt(A,X) && !FDgt(X,B))

#define   CMP(U,V) ((U)<(V) ? -1 : (U)>(V) ? 1 : 0)
#define  TCMP(U,V) (Tlt(U,V) ? -1 : Tgt(U,V) ? 1 : 0)
#define FDCMP(U,V) (FDlt(U,V) ? -1 : FDgt(U,V) ? 1 : 0)

#define DerefNonvar(X) \
{while (IsVar(X)) (X) = CTagToPointer(X);}

#define CMP_BEFORE -1
#define CMP_INSIDE 0
#define CMP_AFTER  1


/*	 v(1,0,'$mutable'(dom([[inf|sup]],inf,sup,sup),0),
	       '$mutable'(lists(0,0,[],[],[],[],[]),0))
*/
#define FD_ATTR_DOM_OFFSET 5
#define FD_ATTR_SUSPS_OFFSET 8
#define FD_ATTR_MIN_OFFSET 13
#define FD_ATTR_MAX_OFFSET 14
#define FD_ATTR_SIZE_OFFSET 15
#define FD_ATTR_V_ARITY 4

/* support unification of dvars */
#define RefMutable(Mut) CTagToArg((Mut),1)

#define AttrToSuspM(Attr,To)				\
{							\
  TAGGED m_prev = (Attr);				\
  (To)=RefMutable(CTagToArg(m_prev,3));			\
  while (TagToHeadfunctor(To)==functor_v4) {		\
    m_prev=(To); (To)=RefMutable(CTagToArg(m_prev,3));	\
  }							\
  (To)=CTagToArg(m_prev,4);				\
}

#define AttrToDomM(Attr,To)				\
{							\
  TAGGED m_prev = (Attr);				\
  (To)=RefMutable(CTagToArg(m_prev,3));			\
  while (TagToHeadfunctor(To)==functor_v4) {		\
    m_prev=(To); (To)=RefMutable(CTagToArg(m_prev,3));	\
  }							\
  (To)=CTagToArg(m_prev,3);				\
}

#define DerefAttribute(To,Attr)			\
{						\
  (To)=RefMutable(CTagToArg((Attr),3));		\
  while (TagToHeadfunctor(To)==functor_v4) {	\
    (To)=RefMutable(CTagToArg((To),3));		\
  }						\
}

/* suspension mask bits */
#define MASK_DOM 0x1
#define MASK_MIN 0x2
#define MASK_MAX 0x4
#define MASK_MINMAX 0x8
#define MASK_VAL 0x10
#define MASK_SINGLETON 0x20

/* What is the relation between two FD sets. */
/* N.B. idiom <= FDI_SUPERSET captures "equal or superset"! */
#define FDI_EQUAL 1
#define FDI_SUPERSET 2
#define FDI_SUBSET 3
#define FDI_DISJOINT 4
#define FDI_INTERSECT 5

/* The temporal relations between two intervals. */
#define FD_BEFORE 0
#define FD_MEETS 1
#define FD_OVERLAPS 2
#define FD_FINISHED_BY 3
#define FD_CONTAINS 4
#define FD_STARTS 5
#define FD_EQUALS 6
#define FD_STARTED_BY 7
#define FD_DURING 8
#define FD_FINISHES 9
#define FD_OVERLAPPED_BY 10
#define FD_MET_BY 11
#define FD_AFTER 12

#define FD_INLINE 1

#if FD_INLINE

#define switch_fd_interval_cmp(B1,E1,B2,E2,				\
			       CASE_BEFORE,				\
			       CASE_MEETS,				\
			       CASE_OVERLAPS,				\
			       CASE_FINISHED_BY,			\
			       CASE_CONTAINS,				\
			       CASE_STARTS,				\
			       CASE_EQUALS,				\
			       CASE_STARTED_BY,				\
			       CASE_DURING,				\
			       CASE_FINISHES,				\
			       CASE_OVERLAPPED_BY,			\
			       CASE_MET_BY,				\
			       CASE_AFTER)				\
{									\
  TAGGED m_b1 = (B1);							\
  TAGGED m_e1 = (E1);							\
  TAGGED m_b2 = (B2);							\
  TAGGED m_e2 = (E2);							\
  if (m_b1==Inf)							\
    m_b1 = InfAsINT;							\
  if (m_e1==Sup || Tge(m_e1,SupAsINT-IStep(1))) /* SPRM 13738 */	\
    m_e1 = SupAsINT;							\
  else									\
    m_e1 += IStep(1);		/* want [m_b1,m_e1), [m_b2,m_e2) */	\
  if (m_b2==Inf)							\
    m_b2 = InfAsINT;							\
  if (m_e2==Sup || Tge(m_e2,SupAsINT-IStep(1)))	/* SPRM 13738 */	\
    m_e2 = SupAsINT;							\
  else									\
    m_e2 += IStep(1);		/* want [m_b1,m_e1), [m_b2,m_e2) */	\
  if (Tlt(m_b1,m_b2)) {							\
    if (Tle(m_e1,m_b2)) {						\
      if (Teq(m_e1,m_b2)) {						\
	CASE_MEETS;							\
      } else {								\
	CASE_BEFORE;							\
      }									\
    } else if (Tlt(m_e1,m_e2)) {					\
      CASE_OVERLAPS;							\
    } else if (Teq(m_e1,m_e2)) {					\
      CASE_FINISHED_BY;							\
    } else {								\
      CASE_CONTAINS;							\
    }									\
  } else if (Teq(m_b1,m_b2)) {						\
    if (Tlt(m_e1,m_e2)) {						\
      CASE_STARTS;							\
    } else if (Teq(m_e1,m_e2)) {					\
      CASE_EQUALS;							\
    } else {								\
      CASE_STARTED_BY;							\
    }									\
  } else {								\
    if (Tlt(m_e1,m_e2)) {						\
      CASE_DURING;							\
    } else if (Teq(m_e1,m_e2)) {					\
      CASE_FINISHES;							\
    } else if (Tlt(m_b1,m_e2)) {					\
      CASE_OVERLAPPED_BY;						\
    } else {								\
      if (Teq(m_b1,m_e2)) {						\
	CASE_MET_BY;							\
      } else {								\
	CASE_AFTER;							\
      }									\
    }									\
  }									\
}

#else /* FD_INLINE */

#define switch_fd_interval_cmp(B1,E1,B2,E2,		\
			       CASE_BEFORE,		\
			       CASE_MEETS,		\
			       CASE_OVERLAPS,		\
			       CASE_FINISHED_BY,	\
			       CASE_CONTAINS,		\
			       CASE_STARTS,		\
			       CASE_EQUALS,		\
			       CASE_STARTED_BY,		\
			       CASE_DURING,		\
			       CASE_FINISHES,		\
			       CASE_OVERLAPPED_BY,	\
			       CASE_MET_BY,		\
			       CASE_AFTER)		\
     switch (fd_interval_cmp(B1,E1,B2,E2)) {		\
     case FD_BEFORE: CASE_BEFORE; break;		\
     case FD_MEETS: CASE_MEETS; break;			\
     case FD_OVERLAPS: CASE_OVERLAPS; break;		\
     case FD_FINISHED_BY: CASE_FINISHED_BY; break;	\
     case FD_CONTAINS: CASE_CONTAINS; break;		\
     case FD_STARTS: CASE_STARTS; break;		\
     case FD_EQUALS: CASE_EQUALS; break;		\
     case FD_STARTED_BY: CASE_STARTED_BY; break;	\
     case FD_DURING: CASE_DURING; break;		\
     case FD_FINISHES: CASE_FINISHES; break;		\
     case FD_OVERLAPPED_BY: CASE_OVERLAPPED_BY; break;	\
     case FD_MET_BY: CASE_MET_BY; break;		\
     case FD_AFTER: CASE_AFTER; break;			\
     }							\


#endif


#define FLOORDIV(Over,Under) \
((Under)==1 ? (Over) : \
 ((Over)>=0) ? (Over)/(Under) : -((-(Over)-1)/(Under)+1))
#define  CEILDIV(Over,Under) \
((Under)==1 ? (Over) : \
 ((Over)<=0) ? (Over)/(Under) : ((Over)-1)/(Under)+1)

#define fd_point_vs_range(p, r) fd_point_vs_interval(p, RangeMin(r), RangeMax(r))
#define fd_val_vs_range(p, r) fd_val_vs_interval(p, RangeMin(r), RangeMax(r))

typedef SP_uinteger ix_byte;

typedef struct profile *PROFILE;

#define FD_LIST_DOM_IX        3
#define FD_LIST_DOM_DAEMON    4
#define FD_LIST_DOM_GLOBAL    5
#define FD_LIST_MIN_IX        6
#define FD_LIST_MIN_DAEMON    7
#define FD_LIST_MIN_GLOBAL    8
#define FD_LIST_MAX_IX        9
#define FD_LIST_MAX_DAEMON    10
#define FD_LIST_MAX_GLOBAL    11
#define FD_LIST_MINMAX_IX     12
#define FD_LIST_MINMAX_DAEMON 13
#define FD_LIST_MINMAX_GLOBAL 14
#define FD_LIST_VAL_IMP0      15
#define FD_LIST_VAL_IMP1      16
#define FD_LIST_VAL_DISEQ     17
#define FD_LIST_VAL_WATCHER   18
#define FD_LIST_VAL_IFF       19
#define FD_LIST_VAL_IX        20
#define FD_LIST_VAL_DAEMON    21
#define FD_LIST_VAL_GLOBAL    22
#define FD_LISTS_SIZE         23

#define FD_CHECK_ARGUMENT_NEED (FD_ATTRIBUTE_SIZE + ARITYLIMIT + 4)
#define FD_LINK_NEED_SAFE (FD_LISTS_SIZE+2)
#define FD_LINK_NEED (FD_LINK_NEED_SAFE+FD_CHECK_ARGUMENT_NEED)

#define FD_QUEUE_IMP     0
#define FD_QUEUE_DISEQ   1
#define FD_QUEUE_WATCHER 2
#define FD_QUEUE_IFF     3
#define FD_QUEUE_IX      4
#define FD_QUEUE_DAEMON  5
#define FD_QUEUE_VAL     6
#define FD_QUEUE_MINMAX  7
#define FD_QUEUE_DOM     8
#define FD_QUEUE_WAKE    9
#define FD_NB_QUEUES     10

struct prop_queue {
  SP_globref bottom;
  SP_globref top;
  SP_globref head;
  SP_globref tail;
};

struct propagator {
  struct propagator *next;
  struct prop_queue queue[FD_NB_QUEUES];
  unsigned int hint;	       /* first queue that can be non-empty */
};

struct fd_state {
#if DBG                         /* [PM] 3.9.2b1 */
  int generation;               /* a unique ID for this instance of the clpfd resource (roughly: incremented for each load_foreign_resource(clpfd)) */
#endif /* DBG */

  void *gdata;			/* of various types */
  SP_uinteger batching;		/* 0=off, 1=on */
  SP_uinteger hiding;		/* 0=off, 1=on */
  SP_uinteger debugging;	/* 0=off, 1=on */
  SP_uinteger overflowing;	/* 0=fail, 1=error */
  SP_uinteger resumptions;
  SP_uinteger entailments;
  SP_uinteger prunings;
  SP_uinteger failures;
  SP_uinteger constraints;
  PROFILE profile;
  struct profile *profile_pool;
  struct mod_def *fd_module;
  SP_pred_ref overflow_action2;
  int fd_overflow;		/* 1 = underflow, 2 = overflow, 0 = neither */
  TAGGED functor_fdlists22;
  TAGGED functor_in_set2;
  TAGGED functor_min;
  TAGGED functor_max;
  TAGGED functor_minmax;
  TAGGED functor_val;
  TAGGED functor_none;
  TAGGED functor_call;
  TAGGED functor_eq;
  TAGGED functor_t_eq_u;
  TAGGED functor_leqc;
  TAGGED functor_alldiff;
  TAGGED functor_colored;
  TAGGED token_a;
  TAGGED token_d;
  TAGGED token_h;
  TAGGED token_l;
  TAGGED token_t;
  TAGGED linkage_keys[8];
  TAGGED var_options[7];
  struct propagator *current_propagator;
  struct propagator *free_propagators;
  struct sw_on_key *dispatch;
};


/* The literals array below consists of items tagged:
   CONST - integers, inf or sup
   STRUCT - FD sets with a bignum header, in its own mem
   LIST - hash table, in its own mem
*/
struct indexical_info {
#if DBG                         /* [PM] 3.9.2b1 */
  int identifier;               /* the clpfd_generation that was used to create this struct */
#endif

  struct definition *pred;
  ix_byte *linkage;		/* each is (var<<8)+index */
  unsigned int length_of_linkage;
  unsigned int length_of_bytecode;
  unsigned int length_of_literals;
  unsigned int pruned:8;
  unsigned int checking:1;	/* checking as opp. to pruning */
  unsigned int truth_value:1;	/* positive as opp. to negative */
  ix_byte *code;
  TAGGED *literals;
  struct indexical_info *next;
};

#define TADD(X,Y) ((X) + ((Y)-TaggedZero))
#define TSUB(X,Y) ((X) - ((Y)-TaggedZero))

#define HALFSHIFT (4<<LogSizeOfWord)

#ifdef __MSVC_RUNTIME_CHECKS
/* [PM] 3.9.1 .NET "/RTCc Reports when a value is assigned to a
        smaller data type that results in a data loss." */
#define LOWER_UHALF(X) ((UHALF)((X) & ((UHALF)~0)))
#else
#define LOWER_UHALF(X) ((UHALF)(X))
#endif

#if SICSTUS_MAJOR_VERSION < 4
# define TADDCHK(X,Y) {				\
  (X) += ((Y)-TaggedZero);			\
  if (!TagIsSmall(X))				\
    (X) = Tltz(X) ? Inf : Sup;			\
}
# define TSUBCHK(X,Y) {				\
  (X) -= ((Y)-TaggedZero);			\
  if (!TagIsSmall(X))				\
    (X) = Tltz(X) ? Inf : Sup;			\
}
#else
/* Converting between LIST_TAG and INT_TAG */
# define LIST_INT(X) ((X)+1)
# define INT_LIST(X) ((X)-1)
# define TADDCHK(X,Y) {					\
  TAGGED m_z = (X)+((Y)-TaggedZero);			\
  if ((SP_integer)((X)^(Y))>=0 && (SP_integer)((X)^m_z)<0)		\
    m_z = Tltz(X) ? Inf : Sup;				\
  (X) = m_z;						\
}
# define TSUBCHK(X,Y) {					\
  TAGGED m_z = (X)-((Y)-TaggedZero);			\
  if ((SP_integer)((X)^(Y))<0 && (SP_integer)((X)^m_z)<0)		\
    m_z = Tltz(X) ? Inf : Sup;				\
  (X) = m_z;						\
}
#endif

#define FD_ATTRIBUTE_SIZE 41
#define INT_ATTRIBUTE_SIZE 15
extern TAGGED fd_attribute[];

extern TAGGED fd_safe_mul (TAGGED t1,TAGGED t2);
extern TAGGED fd_safe_divu (TAGGED t1,TAGGED t2);
extern TAGGED fd_safe_divd (TAGGED t1,TAGGED t2);
extern TAGGED fd_safe_plus (TAGGED t1,TAGGED t2);
extern TAGGED fd_safe_minus (TAGGED t1,TAGGED t2);
extern TAGGED fd_min(TAGGED set);
extern TAGGED fd_max(TAGGED set);
extern SP_BOOL fd_countable(TAGGED set);
extern void   fd_first_and_rest(Wam wam, TAGGED d1, TAGGED *firstp, TAGGED *restp);
extern TAGGED fd_localize(Wam wam, TAGGED old);
extern SP_integer   fd_size(TAGGED set);
extern SP_BOOL fd_singleton(TAGGED set);
extern int    fd_point_vs_interval(TAGGED p, TAGGED min, TAGGED max);
extern int    fd_val_vs_interval(TAGGED p, TAGGED min, TAGGED max);
extern unsigned int fd_interval_cmp(TAGGED b1,TAGGED e1,TAGGED b2,TAGGED e2);
extern TAGGED fd_interval(Wam wam, TAGGED min, TAGGED max);
extern TAGGED fd_compl_interval(Wam wam, TAGGED min, TAGGED max);
extern TAGGED fd_pair(Wam wam, TAGGED t1, TAGGED t2);
extern TAGGED fd_lsh(Wam wam, TAGGED d1, TAGGED toffset);
extern TAGGED *fd_neg_internal(Wam wam, TAGGED d1, TAGGED toffset, TAGGED *valuep);
extern TAGGED fd_neg_offset(Wam wam, TAGGED d1, TAGGED t2);
extern TAGGED fd_delete(Wam wam, TAGGED d1, TAGGED t2);
extern TAGGED fd_subtract(Wam wam, TAGGED d1, TAGGED t2);
extern TAGGED fd_interval_subtract(Wam wam, TAGGED b, TAGGED e, TAGGED d2);
extern TAGGED fd_subtract_interval(Wam wam, TAGGED d, TAGGED b, TAGGED e);
extern TAGGED fd_complement(Wam wam, TAGGED d);
extern TAGGED fd_intersection(Wam wam, TAGGED d1, TAGGED d2);
extern SP_integer   fd_intersection_size(TAGGED d1, TAGGED d2);
extern TAGGED fd_intersection_interval(Wam wam, TAGGED d, TAGGED b, TAGGED e);
extern void   fd_intersection_interval_min_max_l(Wam wam, TAGGED d, TAGGED b, TAGGED e, SP_integer *min, SP_integer* max);
extern TAGGED fd_intersection_min(TAGGED d1, TAGGED d2);
extern TAGGED fd_intersection_max(TAGGED d1, TAGGED d2);
extern TAGGED fd_intersection_min2(TAGGED d1, TAGGED d2, TAGGED *elt);
extern TAGGED fd_union(Wam wam, TAGGED d1, TAGGED d2);
extern TAGGED fd_union_interval(Wam wam, TAGGED d1, TAGGED b, TAGGED e);
extern TAGGED fd_union_dest(TAGGED a, TAGGED b);
extern TAGGED fd_union_sort(TAGGED list);
extern TAGGED fd_check_argument(Wam wam, TAGGED argument, TAGGED min, TAGGED max, TAGGED size);
extern int fd_compare(TAGGED d1, TAGGED d2);
extern int fd_compare_interval(TAGGED d1, TAGGED min, TAGGED max);
extern int fd_compare_intervals(TAGGED b1, TAGGED e1, TAGGED mb2, TAGGED e2);
extern TAGGED fd_merge_into(Wam wam, TAGGED d1, TAGGED d2);
extern TAGGED fd_insert_into(Wam wam, TAGGED t, TAGGED d);
extern SP_BOOL fd_member(TAGGED x, TAGGED set);
extern SP_BOOL fd_member_else(TAGGED x, TAGGED set, TAGGED *tail);
extern SP_BOOL fd_intersects_else(TAGGED x, TAGGED y, TAGGED set, TAGGED *tail);
extern SP_BOOL fd_check_overflow(Wam wam, TAGGED goal);
extern TAGGED fd_globalize(Wam wam, TAGGED old, SP_integer req, int ar);
extern TAGGED fd_globalize_unsafe(Wam wam, TAGGED old, SP_integer req, int ar);
extern void fd_update_mutable(Wam wam, TAGGED new_value, TAGGED mutable);
extern void fd_sync(Wam wam);
extern void fd_end(Wam wam);
extern void fd_dealloc(Wam wam);
extern TAGGED fd_daemon_copy_state(Wam wam, TAGGED *global, SP_BOOL *buried);
extern int  fd_dequeue(Wam wam, TAGGED *item);
extern void fd_enqueue_all(Wam wam, int bits, TAGGED lists_loc, TAGGED value);
extern void fd_expand_prop_queue(Wam wam, struct prop_queue *q);
extern TAGGED fd_multiples(Wam wam, int count, SP_integer value);
extern TAGGED fd_plus(Wam wam, TAGGED d1, TAGGED d2, TAGGED lb, TAGGED ub);
extern TAGGED fd_minus(Wam wam, TAGGED d1, TAGGED d2, TAGGED lb, TAGGED ub);
extern int fd_tell_value(Wam wam, TAGGED value);
extern int fd_tell_interval(Wam wam, TAGGED min, TAGGED max, int why);
extern int fd_tell(Wam wam, TAGGED new);
extern int fd_tell_unsafe(Wam wam, TAGGED new);
extern void fd_told(Wam wam, int why, TAGGED value);
extern void fd_link(Wam wam, TAGGED var, TAGGED key, TAGGED item, TAGGED trigger);

enum daemon_rc_enum {
  DAEMON_NOFIX=1,
  DAEMON_FIX,
  DAEMON_ENTAIL,
  DAEMON_FAIL
};

typedef enum daemon_rc_enum DAEMON_RC;

#define fd_enqueue_prepend(wam,item,ix)					\
{									\
  TAGGED mutable = CTagToArg(item,3);					\
  struct prop_queue *q = &cur->queue[ix];				\
  SP_globref s = q->head;						\
  if (s == q->bottom) s = q->top;					\
  (--s)->term[0] = item;						\
  q->head = s;								\
  if (s == q->tail)							\
    fd_expand_prop_queue(wam,q);					\
  FD_UPDATE_MUTABLE(RefMutable(mutable)|IStep(1),mutable); /* STATUS: enqueued */ \
  if (cur->hint > ix)							\
    cur->hint = ix;							\
}									\

#define fd_enqueue_prepend_nomut(wam,item,ix)					\
{									\
  struct prop_queue *q = &cur->queue[ix];				\
  SP_globref s = q->head;						\
  if (s == q->bottom) s = q->top;					\
  (--s)->term[0] = item;						\
  q->head = s;								\
  if (s == q->tail)							\
    fd_expand_prop_queue(wam,q);					\
  if (cur->hint > ix)							\
    cur->hint = ix;							\
}									\

#define fd_enqueue_append(wam,item,ix)					\
{									\
  TAGGED mutable = CTagToArg(item,3);					\
  struct prop_queue *q = &cur->queue[ix];				\
  SP_globref s = q->tail;						\
  (s++)->term[0] = item;						\
  if (s == q->top) s = q->bottom;					\
  q->tail = s;								\
  if (q->head == s)							\
    fd_expand_prop_queue(wam,q);					\
  FD_UPDATE_MUTABLE(RefMutable(mutable)|IStep(1),mutable); /* STATUS: enqueued */ \
  if (cur->hint > ix)							\
    cur->hint = ix;							\
}									\

#define fd_enqueue_append_nomut(wam,item,ix)					\
{									\
  struct prop_queue *q = &cur->queue[ix];				\
  SP_globref s = q->tail;						\
  (s++)->term[0] = item;						\
  if (s == q->top) s = q->bottom;					\
  q->tail = s;								\
  if (q->head == s)							\
    fd_expand_prop_queue(wam,q);					\
  if (cur->hint > ix)							\
    cur->hint = ix;							\
}									\

#define fd_enqueue_imp(wam,list)			\
{							\
  struct prop_queue *q = &cur->queue[FD_QUEUE_IMP];	\
  SP_globref s = q->head;				\
  if (s == q->bottom) s = q->top;			\
  (--s)->term[0] = list;				\
  q->head = s;						\
  if (s == q->tail)					\
    fd_expand_prop_queue(wam,q);			\
  cur->hint = FD_QUEUE_IMP;				\
}							\

#define fd_enqueue_diseq(wam,list)			\
{							\
  struct prop_queue *q = &cur->queue[FD_QUEUE_DISEQ];	\
  SP_globref s = q->head;				\
  if (s == q->bottom) s = q->top;			\
  (--s)->term[0] = list;				\
  q->head = s;						\
  if (s == q->tail)					\
    fd_expand_prop_queue(wam,q);			\
  if (cur->hint > FD_QUEUE_DISEQ)			\
    cur->hint = FD_QUEUE_DISEQ;				\
}							\

#define fd_enqueue_watcher(wam,value,list)		\
{							\
  struct prop_queue *q = &cur->queue[FD_QUEUE_WATCHER];	\
  SP_globref s = q->head;				\
  if (s == q->bottom) s = q->top;			\
  s -= 2;						\
  (s+0)->term[0] = value;				\
  (s+1)->term[0] = list;				\
  q->head = s;						\
  if (s == q->tail)					\
    fd_expand_prop_queue(wam,q);			\
  if (cur->hint > FD_QUEUE_WATCHER)			\
    cur->hint = FD_QUEUE_WATCHER;			\
}							\

#define fd_enqueue_iff(wam,value,list)		\
{							\
  struct prop_queue *q = &cur->queue[FD_QUEUE_IFF];	\
  SP_globref s = q->head;				\
  if (s == q->bottom) s = q->top;			\
  s -= 2;						\
  (s+0)->term[0] = value;				\
  (s+1)->term[0] = list;				\
  q->head = s;						\
  if (s == q->tail)					\
    fd_expand_prop_queue(wam,q);			\
  if (cur->hint > FD_QUEUE_IFF)				\
    cur->hint = FD_QUEUE_IFF;				\
}							\

#define fd_enqueue_wake(wam,item)			\
{							\
  struct prop_queue *q = &cur->queue[FD_QUEUE_WAKE];	\
  SP_globref s = q->tail;				\
  (s++)->term[0] = item;				\
  if (s == q->top) s = q->bottom;			\
  q->tail = s;						\
  if (q->head == s)					\
    fd_expand_prop_queue(wam,q);			\
  if (cur->hint > FD_QUEUE_WAKE)			\
    cur->hint = FD_QUEUE_WAKE;				\
}							\

struct profile {
  SP_integer begin;
  SP_integer end;
  SP_integer erg;
  struct profile *next;
};

#define profile_nonzero(p,l,u) (fd_profile_max(p,l,u)>0)

extern void fd_init_profile(Wam wam);
extern PROFILE fd_empty_profile(void);
extern PROFILE fd_profile_cons(Wam wam, SP_integer begin, SP_integer end, SP_integer erg, PROFILE next);
extern void fd_profile_dispose(Wam wam, PROFILE cons);
extern PROFILE fd_profile_update(Wam wam, PROFILE p, SP_integer b2, SP_integer e2, SP_integer y2);
extern PROFILE fd_profile_copy(Wam wam, PROFILE p);
extern SP_BOOL fd_profile_zero_at(PROFILE p, SP_integer b2, SP_integer e2, SP_integer *wit);
extern SP_integer    fd_profile_max(PROFILE p, SP_integer b2, SP_integer e2);
extern SP_integer    fd_profile_get(PROFILE p, SP_integer ix);
extern SP_BOOL fd_profile_at_most(struct profile *p, SP_integer limit) ;
extern SP_BOOL fd_profile_next(PROFILE prof, SP_integer *bp, SP_integer *ep, SP_integer *hp, PROFILE *nextp);

/* support for longest hole method */

struct lhs {
  SP_integer l; SP_integer h; SP_integer s;
};

extern void
fd_lh_init(Wam wam,
	   SP_integer sigma0,
	   int row,
	   int nrows,
	   SP_integer bblength,
	   SP_integer bbheight,
	   int ntask,
	   struct lhs *lhs,
	   SP_integer *lh_table,
	   SP_integer *lch_table,
	   int maxbacks,
	   int maxfails);

/* support for save & restore */

#ifndef SP_MANGLE
# define SP_MANGLE(FNAME) FNAME
#endif

extern void SPCDECL free_fd_info_hook (struct indexical_info **infop);

extern void SPCDECL fd_manager_hook (Wam wam, struct worker *,int msg,TAGGED *ptr);
/* [PM] 3.9b4 Proper prototype for frd and fwr */
extern void SPCDECL fd_restore_hook(Wam wam, struct saverest_record *record, fread_longs_fun *frd, struct saverest_info *sr_info);
extern void SPCDECL fd_save_hook (Wam wam, struct saverest_record *record, struct definition *pred, fwrite_fun *fwr, struct saverest_info *sr_info);

/* [PM] 3.9.2b1 */
extern void SPCDECL fd_destructor_hook(Wam wam, struct indexical_info **infop);

/* Iterators */

typedef struct {
  TAGGED cur, max, tail;
} FDITER;

#define fditer_empty(it) ((it)->cur==SupAsINT)

extern TAGGED fd_successor(TAGGED set,TAGGED tval);
extern TAGGED fd_predecessor(TAGGED set,TAGGED tval);
extern void fditer_init(FDITER *it, TAGGED d);
extern TAGGED fditer_next(FDITER *it);
extern void fditer_next_interval(FDITER *it, TAGGED *tmin, TAGGED *tmax);
extern void fditer_skip(FDITER *it, TAGGED d);


/* Constructors */

typedef struct {
  TAGGED head, cur;
  int size;
} FDCONS;

#define fdcons_size(cons) ((cons)->size)
#define fdcons_set(cons) ((cons)->head)

extern void fdcons_init(FDCONS *cons);
extern void fdcons_add(Wam wam, FDCONS *cons, TAGGED t);
extern void fdcons_add_interval(Wam wam, FDCONS *cons, TAGGED t, TAGGED u);

/* mutables */

#define DECL_UPDATE_MUTABLE					\
TAGGED safe_time_stamp = TrailToInt(w->node->trail_top) 	\

#define FD_UPDATE_MUTABLE(VAL,MUT)			\
{							\
  TAGGED m_val = (VAL);					\
  if (Tge(CTagToArg((MUT),2),safe_time_stamp)) {	\
    CTagToArg((MUT),1) = m_val;				\
  } else {						\
    fd_update_mutable(wam,m_val,(MUT));			\
  }							\
}							\

/* accelerator for dvar_init, if we only want the bounds */

#define REF_GET_BOUNDS(ATTR_REF,MIN,MAX)		\
{							\
  TAGGED m_tmp = RefGlob(ATTR_REF);			\
  DerefAttribute(m_tmp,m_tmp); /* get dom/4 term */	\
  (MIN) = DomainMin(m_tmp);				\
  (MAX) = DomainMax(m_tmp);				\
}							\

/* radix (bucket?) sort */

#define KEYSORT(Inarray, Incount, Type, Keyarray, Keycount, Key, Outarray)	\
{										\
  int _i;									\
  Type _x;									\
  for (_i=(Keycount)-1; _i>=0; _i--)						\
    (Keyarray)[_i] = 0;								\
  for (_i=(Incount)-1; _i>=0; _i--)						\
    (Keyarray)[Key((Inarray)[_i])]++;						\
  for (_i=1; _i<(Keycount); _i++)						\
    (Keyarray)[_i] += (Keyarray)[_i-1];						\
  for (_i=(Incount)-1; _i>=0; _i--)						\
    _x = (Inarray)[_i],								\
    (Outarray)[--(Keyarray)[Key(_x)]] = _x;					\
}

extern void
fd_qsort_asc_long(Wam wam, SP_integer *l1, int n);

extern void
fd_qsort_asc_tagged(Wam wam, TAGGED *l1, int n);

/* internal API to guts of cumulative */

struct task;

typedef int (prop_method)           (Wam wam, struct task *t);
typedef SP_BOOL (is_interval_method)(Wam wam, struct task *t);
typedef void (refresh_method)       (Wam wam, struct task *t);
typedef TAGGED (set_method)         (Wam wam, struct task *t);
typedef int (prune_interval_method) (Wam wam, struct task *t, SP_integer min, SP_integer max);
typedef int (prune_set_method)      (Wam wam, struct task *t, TAGGED set);
typedef int (dp_hash_method)        (Wam wam, int msg, SP_integer *support, int n, SP_integer lslack, SP_integer rslack, SP_integer sslack);
typedef SP_integer (lh_method)            (Wam wam, SP_integer sigma, SP_integer epsilon, int closed);

struct diff_constraint {
  SP_integer si;
  SP_integer sj;
};

struct task {
  SP_integer est;
  SP_integer lct;
  SP_integer lctmax;
  SP_integer dur;
  SP_integer res;
  int  est_rank;
  int  lct_rank;
  int  enable;			/* 0=off, 1=optional, 2=on */
  int  diffid;			/* ordinal number among all tasks */
  int  id;			/* e.g. object ID */
  int  part;			/* e.g. shape ID */
  int  support;			/* for TOUCH_METHOD */
  prop_method *propagate;
  refresh_method *refresh;	/* analogous to dvar_set() to ensure valid set */
  set_method *set;		/* analogous to dvar_set() */
  prune_interval_method *prune_interval; /* analogous to dvar_prune_interval_*() */
  prune_set_method *prune_set; /* analogous to dvar_prune_set() */
  is_interval_method *is_interval; /* analogous to dvar_is_interval() */
};

struct dvar;

extern int
fd_discrete_filtering(Wam wam,
			  struct task *tasks,
			  int nbtasks,
			  int nbtotal,
			  int flags,
			  struct dvar *diffvar,
			  struct dvar *limitvar,
			  struct dvar *lowervar,
			  struct dvar *uppervar,
			  struct diff_constraint *dc,
			  int nbdiffs,
			  lh_method *lh_table,
			  dp_hash_method *hasher,
			  struct size_mem *size_mem);

extern SP_BOOL
fd_linear_filter(Wam wam, SP_term_ref State);

extern SP_BOOL
fd_linear_filter_fast(Wam wam, SP_term_ref State);

extern SP_BOOL
fd_linear_filter_pbool(Wam wam, SP_term_ref State);

extern SP_BOOL
fd_linear_filter_ubool(Wam wam, SP_term_ref State);

extern SP_BOOL 
propagate_watchers(Wam wam, TAGGED watcher, TAGGED value);

extern SP_BOOL
propagate_implications(Wam wam, TAGGED list);

extern SP_BOOL
propagate_disequations(Wam wam, TAGGED list);

