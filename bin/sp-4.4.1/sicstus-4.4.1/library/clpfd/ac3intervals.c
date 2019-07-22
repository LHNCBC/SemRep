/* Copyright(C) 1999, Swedish Institute of Computer Science */
/*** 

The AC3intervals filtering algorithm for binary relations.

Based on AC3rm, but repacing linked list of supporting tuples by a small array:

Lecoutre, C., Hemery, F., et al (2007). A study of residual supports
in arc consistency. In IJCAI, vol 7, pp 125-130.

Reasoning on intervals as opposed to single values.  X values are
partitioned into intervals.  Ditto for Y values.

For some i in [0..first[0]):
  let j = lit[0].first[i],
      k = lit[0].past[i],
      a = lit[0].a[i],
      b = lit[0].b[i]

Then Y in (support[0].a[m] .. support[0].b[m]), m in [j,k), support X in a..b.

Similarly for Y (index 1 instead).

Optimization 1 (trailing):

  when we found something, update backtrackably (trail) first[*], lit[*].first

Optimization 2 (dicho search)

  the arrays are sorted, so don't use linear search, use dichotomic search

Bad case for trailing: "indomain": value is not used upon trailing.
I.e. exploring all branches of the search.

Bad case for dicho: "step", when trailing would be maximally effective.
I.e. along one branch of the search.
But the cost of dicho is slight.

Conclusion: they are orthogonal, but dicho has better worst-case behavior (indomain).
Also, trailing costs space.

***/

#include "fd.h"
#include "dvars.h"

#define XVAR (pdata->dvar+0)
#define YVAR (pdata->dvar+1)
#define THISVAR (pdata->dvar+this)
#define THATVAR (pdata->dvar+that)

#define EOL 0xfffffffU

struct ac3intervals_common {
  int refcount;
  int postcount;
  SP_integer size[2];
  SP_BOOL interval[2];
  struct {
    TAGGED *a;
    TAGGED *b;
    SP_integer *first;
    SP_integer *past;
  } lit[2];			/* for each a in dom(X/Y) */
  struct {
    TAGGED *a;
    TAGGED *b;
  } support[2];			/* for each reltuple */
};

struct ac3intervals_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  struct ac3intervals_common *pset;
  SP_globref refbase;		/* static */
  SP_integer stamp;
  int xy;
  Dvar dvar;
  /* space for the above arrays */
};

static void SPCDECL ac3intervals_destructor(void *pdata_v)
{
  struct ac3intervals_data *pdata = (struct ac3intervals_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  if (pdata->refbase)
    SP_free_globrefs(pdata->refbase,4);
  if (--pdata->pset->refcount==0)
    SP_free(pdata->pset);
  SP_free(pdata);
}

static DAEMON_RC SPCDECL 
ac3intervals_daemon(Wam wam, void *vdata, SP_globref attr_ref, TAGGED *global)
{
  struct ac3intervals_data *pdata = (struct ac3intervals_data *)vdata;
  TAGGED tstate = RefMutable(CTagToArg(*global,1));
  SP_integer state_stamp;
  int ar;
  unsigned int xyflag;
  SP_BOOL buried;
  DAEMON_RC rc = DAEMON_FIX;
  
  ar = Arity(TagToHeadfunctor(tstate));
  xyflag = ((attr_ref - pdata->refbase) & 0x3) ? 2 : 1;
  state_stamp = GetSmall(CTagToArg(tstate,ar));
  if (pdata->stamp!=state_stamp) { /* non-incremental */
    pdata->xy = 0;
    pdata->stamp = state_stamp;
  }
  if (!pdata->xy) {
    (void)fd_daemon_copy_state(wam, global,&buried);
    pdata->stamp++;
    rc = DAEMON_NOFIX;
  }
  pdata->xy |= xyflag;
  return rc;
}

/* dichotomic search for first mid | FDle(key,elements[mid]) */
static INLINE SP_integer dicho_le(TAGGED key, TAGGED *elements, SP_integer nelements)
{
  SP_integer inf = 0;
  while (inf<nelements) {
    SP_integer mid = (inf+nelements)>>1;
    if (FDgt(key,elements[mid]))
      inf = mid+1;
    else
      nelements = mid;
  }
  return inf;
}

/* dichotomic search for first mid | FDlt(key,elements[mid]) */
static INLINE SP_integer dicho_lt(TAGGED key, TAGGED *elements, SP_integer nelements)
{
  SP_integer inf = 0;
  while (inf<nelements) {
    SP_integer mid = (inf+nelements)>>1;
    if (FDge(key,elements[mid]))
      inf = mid+1;
    else
      nelements = mid;
  }
  return inf;
}

/* if THIS interval l has no support in dom(THAT), then add it to cons */
static void seeksupport(Wam wam,
			struct ac3intervals_data *pdata,
			SP_integer l,
			FDCONS *cons,
			int this)
{
  struct ac3intervals_common *pset = pdata->pset;
  int that = (this^1);
  SP_integer first = pset->lit[this].first[l];
  SP_integer  past = pset->lit[this].past[l];
  DVITER it;
  TAGGED da, db;
  
  dviter_init(&it, THATVAR);
  dviter_next_interval_t(&it, &da, &db);
  first += dicho_le(da, pset->support[this].b+first, past-first);
  while (first<past) {
    if (FDlt(db, pset->support[this].a[first])) {
      if (dviter_empty(&it))
	break;
      dviter_next_interval_t(&it, &da, &db);
    } else if (FDgt(da, pset->support[this].b[first])) {
      first++;
    } else {
      return;
    }
  }
  fdcons_add_interval(wam,cons,pset->lit[this].a[l], pset->lit[this].b[l]);
}

/* 1 - entailment, 2 - fail */
static unsigned int filter(Wam wam,
			   struct ac3intervals_data *pdata,
			   int this)
{
  struct ac3intervals_common *pset = pdata->pset;
  DVITER it;
  FDCONS cons;
  SP_integer l, first, past;
  TAGGED da, db;
  int rc;
  int that = (this^1);
  
  /* first, skip THAT support arrays that precede THAT */

  fdcons_init(&cons);
  da = dvar_min_t(THATVAR);
  db = dvar_max_t(THATVAR);      
  if (pset->interval[that]) {
    if (da!=db)
      goto hit;
    l = GetSmall_int_(da - pset->lit[that].a[0]);
  } else {
    l = dicho_le(da, pset->lit[that].b, pset->size[that]); /* TODO: direct access? */
    if (FDlt(da,pset->lit[that].a[l]) ||
	FDgt(db,pset->lit[that].b[l]))
      goto hit;
  }

  /* THAT is contained in a single support interval. */

  first = pset->lit[that].first[l];
  past = pset->lit[that].past[l];
  while (first<past) {
    TAGGED a = pset->support[that].a[first];
    TAGGED b = pset->support[that].b[first++];
    fdcons_add_interval(wam,&cons,a,b);
  }
  rc = dvar_fix_set(THISVAR, fdcons_set(&cons));
  return (rc >= 0 ? 1 : 2);

  /* THAT is not contained in a single support interval */

 hit:

  /** For every THIS interval that intersects THISVAR: seek its support **/

  dviter_init(&it, THISVAR);
  if (pset->interval[this]) {
    TAGGED thisbase = pset->lit[this].a[0];
    while (!dviter_empty(&it)) {
      da = dviter_next_value_t(&it);
      l = GetSmall_int_(da - thisbase);
      seeksupport(wam, pdata, l, &cons, this);
    }
  } else {
    past = pset->size[this];
    dviter_next_interval_t(&it, &da, &db);
    l = dicho_le(da, pset->lit[this].b, past);
    while (l<past) {
      if (FDlt(db,pset->lit[this].a[l])) {
	if (dviter_empty(&it))
	  break;
	dviter_next_interval_t(&it, &da, &db);
      } else if (FDgt(da,pset->lit[this].b[l])) {
	l++;
      } else {
	seeksupport(wam, pdata, l, &cons, this);
	l++;
      }
    }
  }
  rc = dvar_prune_set(THISVAR, fdcons_set(&cons));
  return (rc >= 0 ? 0 : 2);
}


/* '$fd_ac3intervals_common'(+ExtensionXY, +ExtensionYX, +Literals, +PostCount, -state([_ | '$free'(Ptr)], 0)) */
void SPCDECL
prolog_fd_ac3intervals_common(Wam wam,
			      SP_term_ref extensionxy_ref,
			      SP_term_ref extensionyx_ref,
			      SP_term_ref literals_ref,
			      SP_integer  postcount,
			      SP_term_ref state_ref)
{
  struct ac3intervals_common *pset;
  struct ac3intervals_data *pdata;
    
  {
    TAGGED reltuples[2], literals, *h;
    SP_integer nextension, nliterals, extra_size, nxlits = -1;
    char *ptr;
    int i, j, k;

    DEREF(reltuples[0], RefTerm(extensionxy_ref));
    DEREF(reltuples[1], RefTerm(extensionyx_ref));
    DEREF(literals, RefTerm(literals_ref));
    nextension = fd_list_length(reltuples[0]);
    nliterals = fd_list_length(literals);
    extra_size = (4*nextension + 4*nliterals)*sizeof(TAGGED);
    pset = fd_malloc(wam, sizeof(struct ac3intervals_common) + extra_size);
    ptr = (char *)(pset+1);
    pset->lit[0].a = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nliterals;
    pset->lit[0].b = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nliterals;
    pset->lit[0].first = (SP_integer *)ptr;
    ptr += sizeof(SP_integer)*nliterals;
    pset->lit[0].past = (SP_integer *)ptr;
    ptr += sizeof(SP_integer *)*nliterals;
    pset->support[0].a = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nextension;
    pset->support[0].b = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nextension;
    pset->support[1].a = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nextension;
    pset->support[1].b = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nextension;
    SP_ASSERT(ptr==(char *)(pset+1)+extra_size);
    pset->refcount = 1;
    pset->postcount = (int)postcount;
    for (i=0; i<nliterals; i++) {
      TAGGED car, no, a, b;
      DerefCar(car,literals); /* lit(no, a, b) */
      DerefCdr(literals,literals);
      DerefArg(no,car,1);
      DerefArg(a,car,2);
      DerefArg(b,car,3);
      if (no==MakeSmall(1) && nxlits==-1)
	nxlits = i;
      pset->lit[0].a[i] = a;
      pset->lit[0].b[i] = b;
    }
    pset->size[0] = nxlits;
    pset->size[1] = nliterals - nxlits;
    pset->lit[1].a = pset->lit[0].a + nxlits;
    pset->lit[1].b = pset->lit[0].b + nxlits;
    pset->lit[1].first = pset->lit[0].first + nxlits;
    pset->lit[1].past = pset->lit[0].past + nxlits;    
    for (k=0; k<2; k++) {
      TAGGED lit = ERRORTAG;
      for (j=0; j<nextension; j++) {
	TAGGED car, lit1, lit2;
	DerefCar(car,reltuples[k]); 
	DerefCdr(reltuples[k],reltuples[k]);
	DerefCar(lit1,car);
	DerefCdr(car,car);
	DerefCar(lit2,car);
	if (lit != lit1) {
	  lit = lit1;
	  pset->lit[0].first[GetSmall_int(lit1)] = j;
	}
	pset->support[k].a[j] = pset->lit[0].a[GetSmall_int(lit2)];
	pset->support[k].b[j] = pset->lit[0].b[GetSmall_int(lit2)];
      }
      for (j=0; j<pset->size[k]-1; j++)
	pset->lit[k].past[j] = pset->lit[k].first[j+1];
      pset->lit[k].past[j] = nextension;
      pset->interval[k] = (AreSmall(pset->lit[k].b[pset->size[k]-1],pset->lit[k].a[0]) &&
			    pset->lit[k].b[pset->size[k]-1] - pset->lit[k].a[0] == MakeSmall(pset->size[k]-1)-TaggedZero);
    }

    h = w->global_top;
    RefTerm(state_ref) = MakeStructure(h);
    h[0] = functor_minus;
    h[1] = TagREF(h+1);
    h[2] = TaggedZero;
    w->global_top = h+3;
    pdata = Palloc(struct ac3intervals_data, 0, h[1]);
    pdata->destructor = ac3intervals_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = NULL;
    pdata->pset = pset;
  }
}

/*
  '$fd_ac3intervals'(+State0, +State, -Actions).
  State = state(VarTuple,Common,Handle,Stamp)
  ExtensionXY :: ExtensionYX :: list of [lit,lit]
  Literals :: list of lit(0/1, Min, Max)
*/
void SPCDECL
prolog_fd_ac3intervals(Wam wam,
		       SP_term_ref State0,
		       SP_term_ref State,
		       SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle;
  struct ac3intervals_data *pdata;
  struct ac3intervals_common *pset;
  SP_BOOL committed;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_static_output_state(wam,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct ac3intervals_data,handle);
  } else {			/* build persistent state */
    TAGGED cur;
    char *ptr;
    SP_integer extra_size = 2*sizeof(struct dvar);
    
    pdata = Palloc(struct ac3intervals_data, extra_size, handle);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += 2*sizeof(struct dvar);
    SP_ASSERT(ptr==(char *)(pdata+1)+extra_size);
    pdata->destructor = ac3intervals_destructor;
    pdata->daemon = ac3intervals_daemon;
    FD_STORE_SPENV(pdata->spenv);
    DerefArg(cur,X(0),2);	/* get state(...) */
    DerefArg(cur,cur,1);	/* get [_ | '$free'(_)] */
    pset = Pdata(struct ac3intervals_data,cur)->pset;
    pset->refcount++;
    if (--pset->postcount==0)
      fd_common_done(wam,2);
    pdata->pset = pset;
    pdata->stamp = 0;
    pdata->refbase = SP_alloc_globrefs(4);
    DerefArg(cur,X(0),1);		/* get var tuple */
    {
      TAGGED xvar, yvar;
      DerefCar(xvar,cur);
      DerefCdr(cur,cur);
      DerefCar(yvar,cur);
      fd_get_var_and_attr(xvar,pdata->refbase);
      fd_get_var_and_attr(yvar,pdata->refbase + 2);
    }
    dvar_init(XVAR, pdata->refbase,   pdata->refbase+1);
    dvar_init(YVAR, pdata->refbase+2, pdata->refbase+3);
    dvar_attach_daemon(wam, XVAR, pdata, X(1), functor_dom1);
    dvar_attach_daemon(wam, YVAR, pdata, X(1), functor_dom1);
    pdata->xy = 3;
  }

  /* RESUME HERE */
  {
    unsigned int rc=0;

    dvar_refresh(XVAR);
    dvar_refresh(YVAR);
    if (pdata->xy & 1)
      rc |= filter(wam,pdata,1);
    if (pdata->xy & 2)
      rc |= filter(wam,pdata,0);
    if (rc & 2) {
      goto fail;
    } else {
      ent = rc;
    }
    dvar_pruning_done(XVAR);
    dvar_pruning_done(YVAR);
    dvar_export(XVAR);
    dvar_export(YVAR);
    pdata->xy = 0;
  }
    
  if (ent==1)
    Pfree;
 fail:
  dvar_export_done(wam,Actions, ent);
}

/*** special case: element/3 (over multiple tuples) ***/

struct ac3element_common {
  int refcount;
  int postcount;
  int nbyvals;
  TAGGED *y;			/* [nbxvals] */
  struct {
    TAGGED *a;
    TAGGED *b;
    SP_integer *first;
    SP_integer *past;
  } lit;			/* [nbyvals] */
  struct {
    TAGGED *a;
    TAGGED *b;
  } support;			/* [nbyvals] */
};

struct ac3element_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  struct ac3element_common *pset;
  SP_globref refbase;		/* static */
  SP_integer stamp;
  int xy;
  Dvar dvar;
  /* space for the above arrays */
};

/* 1 - entailment, 2 - fail */
static unsigned int filterx(Wam wam,
			    struct ac3element_data *pdata)
{
  struct ac3element_common *pset = pdata->pset;
  DVITER it;
  FDCONS cons;
  SP_integer l, first, past;
  TAGGED da, db;
  int rc;
  
  fdcons_init(&cons);

  da = dvar_min_t(YVAR);
  db = dvar_max_t(YVAR);
  l = dicho_le(da, pset->lit.b, pset->nbyvals);
  if (Tlt(da,pset->lit.a[l]) ||
      Tgt(db,pset->lit.b[l]))
    goto hit;

  /* Y is contained in a single support interval. */

  first = pset->lit.first[l];
  past = pset->lit.past[l];
  while (first<past) {
    TAGGED a = pset->support.a[first];
    TAGGED b = pset->support.b[first++];
    fdcons_add_interval(wam,&cons,a,b);
  }
  rc = dvar_fix_set(XVAR, fdcons_set(&cons));
  return (rc >= 0 ? 1 : 2);

  /* Y is not contained in a single support interval */

 hit:

  /** For every X value: seek its support **/

  dviter_init(&it, XVAR);
  while (!dviter_empty(&it)) {
    da = dviter_next_value_t(&it);
    if (!dvar_contains_value_t(YVAR, pset->y[GetSmall_int(da)-1]))
      fdcons_add_interval(wam,&cons,da,da);
  }
  rc = dvar_prune_set(XVAR, fdcons_set(&cons));
  return (rc >= 0 ? 0 : 2);
}


/* Y interval l has support in dom(XVAR)? */
static SP_BOOL seeksupporty(struct ac3element_data *pdata,
			    SP_integer l)
{
  struct ac3element_common *pset = pdata->pset;
  SP_integer first = pset->lit.first[l];
  SP_integer  past = pset->lit.past[l];
  DVITER it;
  TAGGED da, db;
  
  dviter_init(&it, XVAR);
  dviter_next_interval_t(&it, &da, &db);
  first += dicho_le(da, pset->support.b+first, past-first);
  while (first<past) {
    if (Tlt(db, pset->support.a[first])) {
      if (dviter_empty(&it))
	break;
      dviter_next_interval_t(&it, &da, &db);
    } else if (Tgt(da, pset->support.b[first])) {
      first++;
    } else {
      return TRUE;
    }
  }
  return FALSE;
}

/* 1 - entailment, 2 - fail */
static unsigned int filtery(Wam wam,
			    struct ac3element_data *pdata)
{
  struct ac3element_common *pset = pdata->pset;
  SP_integer l;
  TAGGED tmin, tmax;
  int rc;
  
  if (dvar_is_integer(XVAR)) {
    rc = dvar_fix_value_t(YVAR, pset->y[dvar_min_l(XVAR)-1]);
    return (rc >= 0 ? 1 : 2);
  }

  /** For every Y interval that intersects YVAR: seek its support **/

  tmin = Sup;
  tmax = Inf;
  l = dicho_le(dvar_min_t(YVAR), pset->lit.b, pset->nbyvals);
  while (tmin==Sup && l<pset->nbyvals) {
    if (dvar_compare_interval_t(YVAR, pset->lit.a[l], pset->lit.b[l])!=FDI_DISJOINT &&
	seeksupporty(pdata, l))
      tmin = pset->lit.a[l];
    l++;
  }
  if (tmin==Sup)
    return 2;
  l = dicho_lt(dvar_max_t(YVAR), pset->lit.a, pset->nbyvals)-1;
  while (tmax==Inf && l>=0) {
    if (dvar_compare_interval_t(YVAR, pset->lit.a[l], pset->lit.b[l])!=FDI_DISJOINT &&
	(tmin==pset->lit.a[l] || seeksupporty(pdata, l)))
      tmax = pset->lit.b[l];
    --l;
  }
  rc = dvar_fix_interval_t(YVAR, tmin, tmax);
  return (rc >= 0 ? 0 : 2);
}


/* '$fd_ac3element_common'(+YVals, +YXRows, -state([_ | '$free'(Ptr)], 0)) */
void SPCDECL
prolog_fd_ac3element_common(Wam wam,
			    SP_term_ref yvals_ref,
			    SP_term_ref yxrows_ref,
			    SP_integer  postcount,
			    SP_term_ref state_ref)
{
  struct ac3element_common *pset;
  struct ac3element_data *pdata;
  {
    TAGGED yvals, yxrows, cur, prev, *h;
    SP_integer nbxvals, nbyxrows, nbyvals=0, extra_size;
    char *ptr;
    int i, j;

    DEREF(yvals, RefTerm(yvals_ref));
    DEREF(yxrows, RefTerm(yxrows_ref));
    nbxvals = fd_list_length(yvals);
    nbyxrows = fd_list_length(yxrows);
    cur = yxrows;
    prev = 0;
    for (i=0; i<nbyxrows; i++) {
      TAGGED car;
      DerefCar(car,cur); /* car = (A..B)-(C..D) */
      DerefCdr(cur,cur);
      DerefArg(car,car,1);
      DerefArg(car,car,1);
      nbyvals += (car != prev);
      prev = car;
    }
    extra_size = (nbxvals + 2*nbyxrows + 4*nbyvals)*sizeof(TAGGED);
    pset = fd_malloc(wam, sizeof(struct ac3element_common) + extra_size);
    ptr = (char *)(pset+1);
    pset->y = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nbxvals;
    pset->lit.a = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nbyvals;
    pset->lit.b = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nbyvals;
    pset->lit.first = (SP_integer *)ptr;
    ptr += sizeof(SP_integer)*nbyvals;
    pset->lit.past = (SP_integer *)ptr;
    ptr += sizeof(SP_integer *)*nbyvals;
    pset->support.a = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nbyxrows;
    pset->support.b = (TAGGED *)ptr;
    ptr += sizeof(TAGGED)*nbyxrows;
    SP_ASSERT(ptr==(char *)(pset+1)+extra_size);
    pset->refcount = 1;
    pset->postcount = (int)postcount;
    pset->nbyvals = (int)nbyvals;
    for (i=0; i<nbxvals; i++) {
      TAGGED a;
      DerefCar(a,yvals);
      DerefCdr(yvals,yvals);
      pset->y[i] = a;
    }
    for (i=0, j=0, prev=0; j<nbyxrows; j++) {
      TAGGED car, a, b, c, d;
      DerefCar(car,yxrows); /* car = (A..B)-(C..D) */
      DerefCdr(yxrows,yxrows);
      DerefArg(b,car,1);
      DerefArg(d,car,2);
      DerefArg(a,b,1);
      DerefArg(b,b,2);
      DerefArg(c,d,1);
      DerefArg(d,d,2);
      if (a!=prev) {
	prev = a;
	pset->lit.a[i] = a;
	pset->lit.b[i] = b;
	pset->lit.first[i++] = j;
      }
      pset->support.a[j] = c;
      pset->support.b[j] = d;
    }
    for (i=0; i<nbyvals-1; i++) {
      pset->lit.past[i] = pset->lit.first[i+1];
    }
    pset->lit.past[nbyvals-1] = nbyxrows;

    h = w->global_top;
    RefTerm(state_ref) = MakeStructure(h);
    h[0] = functor_minus;
    h[1] = TagREF(h+1);
    h[2] = TaggedZero;
    w->global_top = h+3;
    pdata = Palloc(struct ac3element_data, 0, h[1]);
    pdata->destructor = ac3intervals_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = NULL;
    pdata->pset = pset;
  }
}

/*
  '$fd_ac3element'(+State0, +State, -Actions).
  State = state(VarTuple,Common,Handle,Stamp)
*/
void SPCDECL
prolog_fd_ac3element(Wam wam,
		     SP_term_ref State0,
		     SP_term_ref State,
		     SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle;
  struct ac3element_data *pdata;
  struct ac3element_common *pset;
  SP_BOOL committed;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_static_output_state(wam,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct ac3element_data,handle);
  } else {			/* build persistent state */
    TAGGED cur;
    char *ptr;
    SP_integer extra_size = 2*sizeof(struct dvar);
    
    pdata = Palloc(struct ac3element_data, extra_size, handle);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += 2*sizeof(struct dvar);
    SP_ASSERT(ptr==(char *)(pdata+1)+extra_size);
    pdata->destructor = ac3intervals_destructor;
    pdata->daemon = ac3intervals_daemon;
    FD_STORE_SPENV(pdata->spenv);
    DerefArg(cur,X(0),2);	/* get state(...) */
    DerefArg(cur,cur,1);	/* get [_ | '$free'(_)] */
    pset = Pdata(struct ac3element_data,cur)->pset;
    pset->refcount++;
    if (--pset->postcount==0)
      fd_common_done(wam,2);
    pdata->pset = pset;
    pdata->stamp = 0;
    pdata->refbase = SP_alloc_globrefs(4);
    DerefArg(cur,X(0),1);		/* get var tuple */
    {
      TAGGED xvar, yvar;
      DerefCar(xvar,cur);
      DerefCdr(cur,cur);
      DerefCar(yvar,cur);
      fd_get_var_and_attr(xvar,pdata->refbase);
      fd_get_var_and_attr(yvar,pdata->refbase + 2);
    }    
    dvar_init(XVAR, pdata->refbase,   pdata->refbase+1);
    dvar_init(YVAR, pdata->refbase+2, pdata->refbase+3);
    dvar_attach_daemon(wam, XVAR, pdata, X(1), functor_dom1);
    dvar_attach_daemon(wam, YVAR, pdata, X(1), functor_dom1);
    pdata->xy = 3;
  }

  /* RESUME */

  {
    unsigned int rc=0;
  
    dvar_refresh(XVAR);
    dvar_refresh(YVAR);
    if (TRUE) /*(xy & 1) remember, if Y was pruned, bounds may not have support */
      rc |= filtery(wam,pdata);
    if (pdata->xy & 2)
      rc |= filterx(wam,pdata);
    if (rc & 2) {
      goto fail;
    } else {
      ent = rc;
    }
    dvar_pruning_done(XVAR);
    dvar_pruning_done(YVAR);
    dvar_export(XVAR);
    dvar_export(YVAR);
    pdata->xy = 0;
  }

  if (ent==1)
    Pfree;
 fail:
  dvar_export_done(wam,Actions, ent);
}
