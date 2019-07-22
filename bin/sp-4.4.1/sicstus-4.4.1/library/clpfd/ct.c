/* -*- Mode:C; coding:utf-8 -*- */
/* Copyright(C) 1999, Swedish Institute of Computer Science */
/*** 
Jordan Demeulenaere, Renaud Hartert, Christophe Lecoutre, Guillaume Perez, Laurent Perron, Jean-Charles Régin, Pierre Schaus:
Compact-Table: Efficiently Filtering Table Constraints with Reversible Sparse Bit-Sets. CP 2016: 207-223

Extended to "interval literals", as well as sets of such in elements of the extension, by Mats Carlsson.

Literal = lit(VaIndex,Min,Max) (0-based VaIndex)
***/

#include "fd.h"
#include "dvars.h"

// Both of the following options are BROKEN in the presence of multi-literal extension elements!

// DELTA=1 enables the use of the delta in update_table(). 
#define DELTA 0
// PROJECT=1 enables special casing of exactly one supported tuple left
// PROJECT=2 enables computing supported literals as a function of supported tuples in filter_domain()
#define PROJECT 0

#define VAR(V) (pdata->dvar+(V))

#define EOL 0xfffffffU
#define BPW (8<<LogSizeOfWord)

struct compact_table_common {
  int refcount;
  int postcount;
  int nwords;
  int nextension;
  int nliterals;
  SP_uinteger *support;		/* [nliterals*nwords] */
  TAGGED *min;			/* [nliterals] */
  TAGGED *max;			/* [nliterals] */
  int *first_lit;		/* [nvars] */
  int *interval;		/* [nvars] */
#if PROJECT
  int *lits;			/* [nextension*nvars] */
#endif
};

struct compact_table_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  struct compact_table_common *tset;
  SP_globref refbase;		/* static */
  SP_integer stamp;
  SP_uinteger *trail;
  SP_uinteger *ttop;
  SP_uinteger *tend;
  int nvars;
  int skipvar;
  int mod;
  Dvar dvar;			/* [nvars] */
  SP_uinteger *currTable;	/* [nwords] */
#if DELTA
  TAGGED *oldmin;		/* [nvars] */
  TAGGED *oldmax;		/* [nvars] */
#endif
  int *timestamps;		/* [nwords] */
#if DELTA
                     		/* [nvars] */
                   		/* [nvars] */
#endif
  SP_uinteger *mask;		/* [nwords], or unused if nwords==1 */
  int *index;			/* [nwords], or unused if nwords==1 */
  int *residue;			/* [nliterals], or unused if nwords==1 */
  int limit;			/* unused if nwords==1 */
};

static void SPCDECL compact_table_destructor(void *pdata_v)
{
  struct compact_table_data *pdata = (struct compact_table_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  if (pdata->refbase)
    SP_free_globrefs(pdata->refbase,2*pdata->nvars);
  if (pdata->trail)
    SP_free(pdata->trail);
  if (--pdata->tset->refcount==0)
    SP_free(pdata->tset);
  SP_free(pdata);
}

static void
trail_expand(Wam wam,
	     struct compact_table_data *pdata)
{
  SP_integer inuse   = pdata->ttop - pdata->trail;
  SP_integer oldsize = pdata->tend - pdata->trail;
  
  pdata->trail = SP_realloc(pdata->trail, 2*oldsize*sizeof(SP_uinteger));
  pdata->ttop = pdata->trail + inuse;
  pdata->tend = pdata->trail + 2*oldsize;
}

static void
first_support(struct compact_table_data *pdata, int var, DVITER *it, int *lit)
{
  struct compact_table_common *tset = pdata->tset;
  int f = tset->first_lit[var];
  
  if (tset->interval[var]) {
    f += GetSmall_int_(it->min - tset->max[f]);
  } else {
    if (FDlt(tset->max[f],it->min)) {
      int maxf = var+1==pdata->nvars ? tset->nliterals : tset->first_lit[var+1];
      while (f<maxf) {
	int mid = (f+maxf)>>1;
	if (FDlt(tset->max[mid],it->min))
	  f = mid+1;
	else
	  maxf = mid;
      }
    }
  }
  *lit = f;
}

static SP_BOOL
next_support(struct compact_table_data *pdata, int var, DVITER *it, int *lit)
{
  struct compact_table_common *tset = pdata->tset;
  int f = *lit;
  
  dviter_skip_t(it, tset->max[f]);
  if (dviter_empty(it))
    return FALSE;
  if (tset->interval[var]) {
    f += GetSmall_int_(it->min - tset->max[f]);
  } else {
    f++;
    if (FDlt(tset->max[f],it->min)) {
      int maxf = var+1==pdata->nvars ? tset->nliterals : tset->first_lit[var+1];
      while (f<maxf) {
	int mid = (f+maxf)>>1;
	if (FDlt(tset->max[mid],it->min))
	  f = mid+1;
	else
	  maxf = mid;
      }
    }
  }
  *lit = f;
  return TRUE;
}

static void
assign_and_trail(Wam wam, struct compact_table_data *pdata, SP_uinteger *loc, SP_uinteger modified) {
  SP_integer offset = loc - pdata->currTable;
	
  if (pdata->timestamps[offset] < pdata->stamp) {
    if (pdata->ttop+3 > pdata->tend)
      trail_expand(wam,pdata);
    pdata->ttop[0] = offset;
    pdata->ttop[1] = pdata->timestamps[offset];
    pdata->ttop[2] = *loc;
    pdata->ttop += 3;
    pdata->timestamps[offset] = (int)pdata->stamp;
  }
  *loc = modified;
}

static SP_uinteger *
get_support_singleton(struct compact_table_data *pdata, int var, Dvar dv)
{
  struct compact_table_common *tset = pdata->tset;
  int f = tset->first_lit[var];
  TAGGED val = dvar_min_t(dv);
  
  if (tset->interval[var]) {
    f += GetSmall_int_(val - tset->max[f]);
  } else {
    if (FDlt(tset->max[f],val)) {
      int maxf = var+1==pdata->nvars ? tset->nliterals : tset->first_lit[var+1];
      while (f<maxf) {
	int mid = (f+maxf)>>1;
	if (FDlt(tset->max[mid],val))
	  f = mid+1;
	else
	  maxf = mid;
      }
    }
  }
  return tset->support + f*tset->nwords;
}

static SP_uinteger
get_support_singleton_1w(struct compact_table_data *pdata, int var, Dvar dv)
{
  struct compact_table_common *tset = pdata->tset;
  int f = tset->first_lit[var];
  TAGGED val = dvar_min_t(dv);
  
  if (tset->interval[var]) {
    f += GetSmall_int_(val - tset->max[f]);
  } else {
    if (FDlt(tset->max[f],val)) {
      int maxf = var+1==pdata->nvars ? tset->nliterals : tset->first_lit[var+1];
      while (f<maxf) {
	int mid = (f+maxf)>>1;
	if (FDlt(tset->max[mid],val))
	  f = mid+1;
	else
	  maxf = mid;
      }
    }
  }
  return tset->support[f];
}

#if DELTA

static SP_uinteger *
get_support_incremental(Wam wam, struct compact_table_data *pdata, int var, Dvar dv)
{
  struct compact_table_common *tset = pdata->tset;
  SP_uinteger *lmask;
  DVITER it;
  int j, lit;

  lmask = pdata->mask;
  it.min = pdata->oldmin[var];
  it.max = pdata->oldmax[var];
  it.fdset = fd_interval_subtract(wam, it.min, it.max, dvar_set(dv));
  if (it.fdset==EmptySet)
    return NULL;
  it.min = fd_min(it.fdset);
  it.max = fd_max(it.fdset);
  for (j=0; j<pdata->limit; j++)
    lmask[pdata->index[j]] = -((SP_uinteger)1);
  first_support(pdata, var, &it, &lit);
  do {
    if (dvar_compare_interval_t(dv, tset->min[lit], tset->max[lit])==FDI_DISJOINT) {
      SP_uinteger *sw = tset->support + lit*tset->nwords;
      for (j=0; j<pdata->limit; j++)
	lmask[pdata->index[j]] &= ~sw[pdata->index[j]];
    }
  } while (next_support(pdata, var, &it, &lit));
  if (pdata->oldmin[var] != dvar_min_t(dv))
    assign_and_trail(wam, pdata, &pdata->oldmin[var], dvar_min_t(dv));
  if (pdata->oldmax[var] != dvar_max_t(dv))
    assign_and_trail(wam, pdata, &pdata->oldmax[var], dvar_max_t(dv));
  return lmask;
}

static SP_uinteger
get_support_incremental_1w(Wam wam, struct compact_table_data *pdata, int var, Dvar dv)
{
  struct compact_table_common *tset = pdata->tset;
  SP_uinteger lmask = -((SP_uinteger)1);
  DVITER it;
  int lit;

  it.min = pdata->oldmin[var];
  it.max = pdata->oldmax[var];
  it.fdset = fd_interval_subtract(wam, it.min, it.max, dvar_set(dv));
  if (it.fdset==EmptySet)
    return 0;
  it.min = fd_min(it.fdset);
  it.max = fd_max(it.fdset);
  first_support(pdata, var, &it, &lit);
  do {
    if (dvar_compare_interval_t(dv, tset->min[lit], tset->max[lit])==FDI_DISJOINT) {
      lmask &= tset->support[lit];
    }
  } while (next_support(pdata, var, &it, &lit));
  if (pdata->oldmin[var] != dvar_min_t(dv))
    assign_and_trail(wam, pdata, &pdata->oldmin[var], dvar_min_t(dv));
  if (pdata->oldmax[var] != dvar_max_t(dv))
    assign_and_trail(wam, pdata, &pdata->oldmax[var], dvar_max_t(dv));
  return lmask;
}

#else

static SP_uinteger *
get_support_basic(struct compact_table_data *pdata, int var, Dvar dv)
{
  struct compact_table_common *tset = pdata->tset;
  SP_uinteger *lmask;
  DVITER it;
  int j, lit;

  dviter_init(&it, dv);
  first_support(pdata, var, &it, &lit);
    lmask = pdata->mask;
    for (j=0; j<pdata->limit; j++)
      lmask[pdata->index[j]] = ((SP_uinteger)0);
    do {
      SP_uinteger *sw = tset->support + lit*tset->nwords;
      for (j=0; j<pdata->limit; j++)
	lmask[pdata->index[j]] |= sw[pdata->index[j]];
    } while (next_support(pdata, var, &it, &lit));
  return lmask;
}

static SP_uinteger
get_support_basic_1w(struct compact_table_data *pdata, int var, Dvar dv)
{
  struct compact_table_common *tset = pdata->tset;
  SP_uinteger lmask = 0;
  DVITER it;
  int lit;

  dviter_init(&it, dv);
  first_support(pdata, var, &it, &lit);
    do {
      lmask |= tset->support[lit];
    } while (next_support(pdata, var, &it, &lit));
  return lmask;
}

#endif

static SP_BOOL
update_table_nw(Wam wam, struct compact_table_data *pdata, int var, Dvar dv)
{
  SP_uinteger *lmask;
  int j;

  if (dvar_is_integer(dv)) {
    lmask = get_support_singleton(pdata, var, dv);
#if DELTA
  } else {
    lmask = get_support_incremental(wam, pdata, var, dv);
    if (!lmask)
      return TRUE;
#else
  } else {
    lmask = get_support_basic(pdata, var, dv);
#endif
  }
  for (j=pdata->limit; j--; ) {	/* MUST be in decreasing order */
    int offset = pdata->index[j];
    SP_uinteger modified = (pdata->currTable[offset] & lmask[offset]);
    if (modified!=pdata->currTable[offset]) {
      pdata->mod = TRUE;
      assign_and_trail(wam, pdata, &pdata->currTable[offset], modified);
      if (!modified) {
	pdata->index[j] = pdata->index[pdata->limit-1];
	pdata->index[--pdata->limit] = offset;
      }
    }
  }
  return (pdata->limit>0);
}

static SP_BOOL
update_table_1w(Wam wam, struct compact_table_data *pdata, int var, Dvar dv)
{
  SP_uinteger lmask;

  if (dvar_is_integer(dv)) {
    lmask = get_support_singleton_1w(pdata, var, dv);
#if DELTA
  } else {
    lmask = get_support_incremental_1w(wam, pdata, var, dv);
    if (!lmask)
      return TRUE;
#else
  } else {
    lmask = get_support_basic_1w(pdata, var, dv);
#endif
  }
  lmask &= pdata->currTable[0];
  if (lmask!=pdata->currTable[0]) {
    pdata->mod = TRUE;
    assign_and_trail(wam, pdata, &pdata->currTable[0], lmask);
  }
  return (lmask!=0);
}

static SP_BOOL
update_table(Wam wam, struct compact_table_data *pdata, int var, Dvar dv) {
  struct compact_table_common *tset = pdata->tset;
  if (tset->nwords>1)
    return update_table_nw(wam, pdata, var, dv);
  else
    return update_table_1w(wam, pdata, var, dv);
}

#if PROJECT

static int
msb(SP_uinteger x)
{
  int i=0;

#if (LogSizeOfWord>=3)
  if (x >= 0x100000000) {
    x >>= 32; i += 32;
  }
#endif
  if (x >= 0x10000) {
    x >>= 16; i += 16;
  }
  if (x >= 0x100) {
    x >>= 8; i += 8;
  }
  if (x >= 0x10) {
    x >>= 4; i += 4;
  }
  if (x >= 0x4) {
    x >>= 2; i += 2;
  }
  if (x >= 0x2) {
    x >>= 1; i++;
  }
  if (x) {
    i++;
  }
  return i-1;
}

#endif

#if PROJECT==2

static void
filter_domain_nw(Wam wam, struct compact_table_data *pdata, int var, Dvar dv)
{
  struct compact_table_common *tset = pdata->tset;
  int nwords = tset->nwords;
  int nvars = pdata->nvars;
  SP_uinteger *ct = pdata->currTable;
  SP_uinteger *lmask = pdata->mask;
  TAGGED resurrected = EmptySet;
  int j, nzword = -1;

  for (j=pdata->limit; j--; ) {
    int wordno = pdata->index[j];
    lmask[wordno] = ct[wordno];
    if (lmask[wordno])
      nzword = wordno;
  }
  while (nzword>=0) {
    int lit = tset->lits[nvars*(nzword*BPW + msb(lmask[nzword])) + var];
    
    resurrected = fd_union_interval(wam, resurrected, tset->min[lit], tset->max[lit]);
    nzword = -1;
    for (j=pdata->limit; j--; ) {
      int wordno = pdata->index[j];
      lmask[wordno] &= ~tset->support[nwords*lit + wordno];
      if (lmask[wordno])
	nzword = wordno;
    }
  }
  dvar_fix_set(dv, resurrected);
}

static void
filter_domain_1w(Wam wam, struct compact_table_data *pdata, int var, Dvar dv)
{
  struct compact_table_common *tset = pdata->tset;
  SP_uinteger ct = pdata->currTable[0];
  int nvars = pdata->nvars;
  TAGGED resurrected = EmptySet;

  while (ct>0) {
    int lit = tset->lits[nvars*msb(ct) + var];
    resurrected = fd_union_interval(wam, resurrected, tset->min[lit], tset->max[lit]);
    ct &= ~tset->support[lit];
  }
  dvar_fix_set(dv, resurrected);
}

#else

static void
filter_domain_nw(Wam wam, struct compact_table_data *pdata, int var, Dvar dv)
{
  struct compact_table_common *tset = pdata->tset;
  int nwords = tset->nwords;
  SP_uinteger *ct = pdata->currTable;
  DVITER it;
  FDCONS cons;
  int j, lit;
      
  dviter_init(&it, dv);
  fdcons_init(&cons);
  first_support(pdata, var, &it, &lit);
  do {
    SP_uinteger *sw = tset->support + lit*nwords;
    int windex = pdata->residue[lit];
    if (!(ct[windex] & sw[windex])) {
      for (j=0; j<pdata->limit; j++)
	if (ct[pdata->index[j]] & sw[pdata->index[j]])
	  break;
      if (j<pdata->limit)
	pdata->residue[lit] = pdata->index[j];
      else
	fdcons_add_interval(wam, &cons, tset->min[lit], tset->max[lit]);
    }
  } while (next_support(pdata, var, &it, &lit));
  dvar_prune_set(dv, fdcons_set(&cons));
}

static void
filter_domain_1w(Wam wam, struct compact_table_data *pdata, int var, Dvar dv)
{
  struct compact_table_common *tset = pdata->tset;
  SP_uinteger ct = pdata->currTable[0];
  DVITER it;
  FDCONS cons;
  int lit;
      
  dviter_init(&it, dv);
  fdcons_init(&cons);
  first_support(pdata, var, &it, &lit);
  do {
    if (!(ct & tset->support[lit]))
      fdcons_add_interval(wam, &cons, tset->min[lit], tset->max[lit]);
  } while (next_support(pdata, var, &it, &lit));
  dvar_prune_set(dv, fdcons_set(&cons));
}

#endif

static void
filter_domain(Wam wam, struct compact_table_data *pdata, int var, Dvar dv) {
  struct compact_table_common *tset = pdata->tset;
  if (tset->nwords>1)
    filter_domain_nw(wam, pdata, var, dv);
  else
    filter_domain_1w(wam, pdata, var, dv);
}

#define ARG_TTOP 3

static DAEMON_RC SPCDECL 
compact_table_daemon(Wam wam, void *vdata, SP_globref attr_ref, TAGGED *global)
{
  struct compact_table_data *pdata = (struct compact_table_data *)vdata;
  TAGGED tstate = RefMutable(CTagToArg(*global,1));
  int nvars = pdata->nvars;
  int tuple_var = (int)(attr_ref - pdata->refbase) >> 1;
  int var = tuple_var % nvars;
  Dvar dv = VAR(var);
  SP_integer state_stamp;
  SP_BOOL buried;
  int ar;
  DAEMON_RC rc = DAEMON_FIX;
  
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  if (pdata->stamp!=state_stamp) { /* non-incremental */
    SP_uinteger *btr = pdata->trail + GetSmall(CTagToArg(tstate,ARG_TTOP));
    
    pdata->skipvar = EOL;
    pdata->stamp = state_stamp;
    while (pdata->ttop != btr) {
      int offset, stamp;
      SP_uinteger word;
      pdata->ttop -= 3;
      offset = (int)pdata->ttop[0];
      stamp = (int)pdata->ttop[1];
      word = pdata->ttop[2];
      if (!pdata->currTable[offset])
	pdata->limit++;
      pdata->currTable[offset] = word;
      pdata->timestamps[offset] = stamp;
    }
  }
  (void)fd_daemon_copy_state(wam, global,&buried);
  if (!buried)
    CTagToArg(tstate,ar) -= IStep(1);
  else
    pdata->stamp++;	/* increase iff old and new states are separated by a choicepoint */
  if (pdata->skipvar == EOL) {
    pdata->skipvar = var;
  } else {
    pdata->skipvar = nvars;
  }
  dvar_refresh(dv);
  pdata->mod = FALSE;
  if (!update_table(wam, pdata, var, dv))
    rc = DAEMON_FAIL;
  else if (pdata->mod || dvar_is_integer(dv)) /* tuple wipe-out OR potential entailment */
    rc = DAEMON_NOFIX;
  return rc;
}

#if PROJECT==1

static SP_BOOL
one_support(struct compact_table_data *pdata, int *litbase) {
  struct compact_table_common *tset = pdata->tset;
  int nvars = pdata->nvars;
  
  if (tset->nwords>1) {
    SP_uinteger *ct = pdata->currTable;
    SP_uinteger firstw = ct[pdata->index[0]];
    if (pdata->limit==1 && !(firstw&(firstw-1))) {
      int bitno = msb(firstw);
      *litbase = nvars*(pdata->index[0]*BPW + bitno);
      return TRUE;
    }
  } else {
    SP_uinteger firstw = pdata->currTable[0];
    if (!(firstw&(firstw-1))) {
      int bitno = msb(firstw);
      *litbase = nvars*bitno;
      return TRUE;
    }
  }
  return FALSE;
}

#endif


/* '$fd_compact_table_common'(+Extension, +AllLits, -state([_ | '$free'(Ptr)], 0)) */
/* Extension is a list of list of list of literal. */
void SPCDECL
prolog_fd_compact_table_common(Wam wam,
			       SP_term_ref extension_ref,
			       SP_term_ref literals_ref,
			       SP_integer  postcount,
			       SP_term_ref state_ref)
{
  TAGGED extension, literals;
  struct compact_table_common *tset;
  struct compact_table_data *pdata;
    
  DEREF(extension, RefTerm(extension_ref));
  DEREF(literals, RefTerm(literals_ref));
  {
    int nextension = fd_list_length(extension);
    int nliterals = fd_list_length(literals);
    int nwords = ((nextension-1) / BPW) + 1;
    SP_integer extra_size;
    char *ptr;
    int nvars, i, j, k;
    TAGGED cur, *h;

    DerefCar(cur,extension);
    nvars = fd_list_length(cur);
    extra_size = 
      nliterals * nwords * sizeof(SP_integer) + /* support */
      nliterals * sizeof(TAGGED) +	  /* min */
      nliterals * sizeof(TAGGED) +	  /* max */
      nvars * sizeof(int) +		  /* first_lit */
      nvars * sizeof(int);		  /* interval */
#if PROJECT
    extra_size +=
      nextension * nvars * sizeof(int);	  /* lits */
#endif
    tset = fd_malloc(wam, sizeof(struct compact_table_common) + extra_size);
    ptr = (char *)(tset+1);
    tset->support = (SP_uinteger *)ptr;
    ptr += nliterals * nwords * sizeof(SP_integer);
    tset->min = (TAGGED *)ptr;
    ptr += nliterals * sizeof(TAGGED);
    tset->max = (TAGGED *)ptr;
    ptr += nliterals * sizeof(TAGGED);
    tset->first_lit = (int *)ptr;
    ptr += nvars * sizeof(int);
    tset->interval = (int *)ptr;
    ptr += nvars * sizeof(int);
#if PROJECT
    tset->lits = (int *)ptr;
    ptr += nextension * nvars * sizeof(int);
#endif
    SP_ASSERT(ptr==(char *)(tset+1)+extra_size);
    tset->refcount = 1;
    tset->postcount = (int)postcount;
    tset->nwords = nwords;
    tset->nextension = nextension;
    tset->nliterals = nliterals;
    for (i=0, k=-1; i<nliterals; i++) {
      TAGGED x;
      SP_uinteger *st = tset->support + i*nwords;
      memset(st, 0, nwords*sizeof(*st));						    
      DerefCar(cur,literals);
      DerefCdr(literals,literals);
      DerefArg(x,cur,1);
      if (GetSmall(x) > k)
	tset->first_lit[++k] = i;
      DerefArg(x,cur,2);
      tset->min[i] = x;
      DerefArg(x,cur,3);
      tset->max[i] = x;
    }
    for (i=0; i<nvars; i++) {
      int minf = tset->first_lit[i];
      int maxf = i+1==nvars ? nliterals-1 : tset->first_lit[i+1]-1;
      tset->interval[i] = (AreSmall(tset->max[maxf],tset->min[minf]) &&
			    tset->max[maxf] - tset->min[minf] == MakeSmall(maxf-minf)-TaggedZero);
    }
    for (i=0; i<nextension; i++) {
      int wordno = i / BPW;
      int bitno = i % BPW;
      TAGGED tuple;
      
      DerefCar(tuple,extension);
      DerefCdr(extension,extension);
      for (j=0; j<nvars; j++) {
	TAGGED litset;
	DerefCar(litset,tuple);
	DerefCdr(tuple,tuple);
	while (TagIsLST(litset)) {
	  TAGGED lit;
	  DerefCar(lit,litset);
	  DerefCdr(litset,litset);
	  tset->support[nwords*GetSmall(lit) + wordno] |= (((SP_uinteger)1) << bitno);
#if PROJECT
	  tset->lits[i*nvars + j] = GetSmall_int(lit);
#endif
	}
      }
    }
    h = w->global_top;
    RefTerm(state_ref) = MakeStructure(h);
    h[0] = functor_minus;
    h[1] = TagREF(h+1);
    h[2] = TaggedZero;
    w->global_top = h+3;
    pdata = Palloc(struct compact_table_data, 0, h[1]);
    pdata->destructor = compact_table_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = NULL;
    pdata->trail = NULL;
    pdata->tset = tset;
  }
}

/*
  '$fd_compact_table'(+State0, +State, -Actions).
  State = state(VATuple,TupleSet,TrailTop,Handle,Stamp)
  TupleSet = ts(Extension,Literals,TSHandle)
*/
void SPCDECL
prolog_fd_compact_table(Wam wam,
			SP_term_ref State0,
			SP_term_ref State,
			SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  int nvars, nwords;
  int litbase=0, nassigned = 0;
  TAGGED handle, cur;
  struct compact_table_data *pdata;
  struct compact_table_common *tset;
  SP_BOOL committed, posted=TRUE;
  char *ptr;
  int i, k;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_static_output_state(wam,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct compact_table_data,handle);
    tset = pdata->tset;
    nvars = pdata->nvars;
    nwords = tset->nwords;
    posted = FALSE;
  } else {			/* build persistent state */
    SP_uinteger nonlast_word = ~((SP_uinteger)0);
    SP_uinteger last_word;
    SP_integer extra_size;
    int nextension;

    DerefArg(cur,X(0),1);
    nvars = fd_list_length(cur);
    DerefArg(cur,X(0),2);	/* get state(...) */
    DerefArg(cur,cur,1);	/* get [_ | '$free'(_)] */
    tset = Pdata(struct compact_table_data,cur)->tset;
    tset->refcount++;
    if (--tset->postcount==0)
      fd_common_done(wam,2);
    nwords = tset->nwords;
    nextension = tset->nextension;
    last_word = BPW*nwords == nextension ? nonlast_word : (((SP_uinteger)1)<<(nextension % BPW))-1;
    extra_size = nvars*sizeof(struct dvar) +
      nwords * sizeof(SP_integer) + /* currTable */
#if DELTA
      nvars * sizeof(TAGGED) +  /* oldmin */
      nvars * sizeof(TAGGED) +  /* oldmax */
      nvars * sizeof(int) + /* their timestamps */
#endif
      nwords * sizeof(int) +	  /* timestamps */
      nwords * sizeof(int) + /* index */
      nwords * sizeof(SP_integer) +		 /* mask */
      tset->nliterals * sizeof(int);	 /* residue */
    pdata = Palloc(struct compact_table_data, extra_size, handle);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->currTable = (SP_uinteger *)ptr;
    ptr += nwords * sizeof(SP_integer);
#if DELTA
    pdata->oldmin = (TAGGED *)ptr;
    ptr += nvars * sizeof(TAGGED);
    pdata->oldmax = (TAGGED *)ptr;
    ptr += nvars * sizeof(TAGGED);
#endif
    pdata->mask = (SP_uinteger *)ptr;
    ptr += nwords * sizeof(SP_integer);
    pdata->index = (int *)ptr;
    ptr += nwords * sizeof(int);
    pdata->residue = (int *)ptr;
    memset(ptr, 0, tset->nliterals * sizeof(int));
    ptr += tset->nliterals * sizeof(int);
    pdata->timestamps = (int *)ptr;
    ptr += nwords * sizeof(int);
#if DELTA
    ptr += 2 * nvars * sizeof(int);
#endif
    SP_ASSERT(ptr==(char *)(pdata+1)+extra_size);
    pdata->destructor = compact_table_destructor;
    pdata->daemon = compact_table_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->tset = tset;
    pdata->stamp = 0;
    pdata->nvars = nvars;
    pdata->refbase = SP_alloc_globrefs(2*nvars);
    pdata->trail = SP_malloc(16*sizeof(SP_uinteger));
    pdata->ttop = pdata->trail;
    pdata->tend = pdata->trail+16;
    
				/* internalize all variables */
    DerefArg(cur,X(0),1);
    for (i=0, k=0; i<nvars; i++, k+=2) {
      TAGGED x;
      DerefCar(x,cur);
      DerefCdr(cur,cur);
      fd_get_var_and_attr(x,pdata->refbase + k);
    }
				/* init algorithmic data */
    for (i=0; i<nwords-1; i++)
      pdata->currTable[i] = nonlast_word;
    pdata->currTable[i] = last_word;
    for (i=0; i<nwords; i++)
      pdata->index[i] = i;
    pdata->limit = nwords;
    i = nwords;
#if DELTA
    i += 2 * nvars;
#endif
    while (i--)
      pdata->timestamps[i] = 0;      
    for (i=0, k=0; i<nvars; i++, k+=2) {
      SP_globref refoffset = pdata->refbase + k;
      Dvar dv = VAR(i);
      dvar_init(dv, refoffset,   refoffset+1);
      dvar_attach_daemon(wam, dv, pdata, X(1), functor_dom1);
#if DELTA
      pdata->oldmin[i] = tset->min[tset->first_lit[i]];
      pdata->oldmax[i] = tset->max[i+1==nvars ? tset->nliterals-1 : tset->first_lit[i+1]-1];
#endif
    }
    pdata->skipvar = EOL;
  }

  /* RESUME HERE */
  if (posted)
    for (i=0; i<nvars; i++) {
      Dvar dv = VAR(i);
      dvar_refresh(dv);
      if (!update_table(wam, pdata, i, dv))
	goto ret;
    }

#if PROJECT==1
  if (one_support(pdata, &litbase)) { /* exactly one support for this tuple */
    nassigned = nvars;
    for (i=0; i<nvars; i++) {
      Dvar dv = VAR(i);
      dvar_refresh(dv);
      if (i!=pdata->skipvar && !dvar_is_integer(dv)) {      
	int lit = tset->lits[litbase+i];
	dvar_fix_interval_t(dv, tset->min[lit], tset->max[lit]);
      }
    }
  } else
#endif
    {			/* more than one support -- scan domain */
      (void)litbase;
      for (i=0; i<nvars; i++) {
	Dvar dv = VAR(i);
	dvar_refresh(dv);
	if (i!=pdata->skipvar && !dvar_is_integer(dv))
	  filter_domain(wam, pdata, i, dv);
	if (dvar_is_integer(dv))
	  nassigned++;
      }
    }
  pdata->skipvar = EOL;
  for (i=0; i<nvars; i++)
    dvar_pruning_done(VAR(i));
  for (i=0; i<nvars; i++)
    dvar_export(VAR(i));
  
  CTagToArg(X(0),ARG_TTOP) = MakeSmall(pdata->ttop - pdata->trail);
  ent = (nassigned >= nvars-1);
  if (ent==1)
    Pfree;
 ret:
  dvar_export_done(wam,Actions, ent);
}

