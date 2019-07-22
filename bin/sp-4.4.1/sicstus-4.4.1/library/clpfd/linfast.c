/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

typedef int TERM;

struct trail_item {
  TERM elt;
  int depth;
};

#define TRAIL(E,D)				\
{						\
  if (pdata->ttop >= pdata->tend)		\
    trail_expand(wam, pdata);			\
  pdata->ttop->elt = (E);			\
  pdata->ttop->depth = DEPTH(E);		\
  pdata->ttop++;				\
  DEPTH(E) = (D);				\
}

struct linear_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;

  SP_globref refbase;
  int op;			/* 1(#=<), 2(#>=), 3(#=), 4(#\=) */
  int depth;			/* choicept depth */
  int nonground;		/* maintained incrementally */
  int nvars;			/* #terms */
  SP_BOOL gcd_due;		/* whether gcd check is due */
  SP_integer bige;
  SP_integer bigf;
  TERM *heap;
  int *vheap;
  TERM *target;
  int *tloc;
  Dvar dvar;
  struct {
    SP_integer *cmin;		/* min(ai*xi) */
    SP_integer *cmax;		/* max(ai*xi) */
    SP_integer *interval;	/* cmax-cmin */
    SP_integer *coeff;		/* ai */
    SP_integer *abscoeff;	/* |ai| */
    int *depth;			/* depth at which I was modified */
  } term;
  struct trail_item *trail;
  struct trail_item *ttop;
  struct trail_item *tend;
};

  /* Maintain:
     bigf = rhs - sum{min(a_i x_i)}
     bige = sum{max(a_i x_i)} - rhs

     I_i = max(a_i x_i) - min(a_i x_i)
  */

#define SV(I)   (pdata->target[I])
#define VS(I)   (pdata->tloc[I])
#define COEFF(t) (pdata->term.coeff[t])
#define ABSCOEFF(t) (pdata->term.abscoeff[t])
#define DEPTH(t) (pdata->term.depth[t])
#define DVAR(t) (pdata->dvar+(t))
#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define CMIN(t) (pdata->term.cmin[t])
#define CMAX(t) (pdata->term.cmax[t])
#define INTERVAL(t) (pdata->term.interval[t])

static void SPCDECL linear_destructor(void *pdata_v)
{
  struct linear_data *pdata = (struct linear_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata->trail);
  SP_free(pdata);
}

#define SWAP(I,J)				\
{						\
  TERM vi = heap[I];				\
  TERM vj = heap[J];				\
  heap[I] = vj;					\
  heap[J] = vi;					\
  vheap[vi] = (J);				\
  vheap[vj] = (I);				\
}

static void 
heap_demote(struct linear_data *pdata, int i)
{
  TERM *heap = pdata->heap;
  int *vheap = pdata->vheap;
  SP_integer *key = pdata->term.interval;
  
  for (;;) {
    int l = (i<<1)+1;
    int topmost = i;
    if (l<pdata->nvars && key[heap[l]] > key[heap[topmost]])
      topmost = l;
    if (l+1<pdata->nvars && key[heap[l+1]] > key[heap[topmost]])
      topmost = l+1;
    if (topmost==i)
      break;
    SWAP(i,topmost);
    i = topmost;
  }
}

static void
heap_promote(struct linear_data *pdata, int i)
{
  TERM *heap = pdata->heap;
  int *vheap = pdata->vheap;
  SP_integer *key = pdata->term.interval;
  SP_integer key0 = key[heap[i]];
  int dest = i;

  while (dest>0 && key[heap[(dest-1)>>1]] < key0) {
    dest = (dest-1)>>1;
  }
  while (i>dest) {
    SWAP(i,(i-1)>>1);
    i = (i-1)>>1;
  }
}

static void trail_expand(Wam wam, struct linear_data *pdata)
{
  SP_integer oldsize = pdata->tend - pdata->trail;
  pdata->trail = SP_realloc(pdata->trail, 2*oldsize*sizeof(struct trail_item));
  pdata->ttop = pdata->trail + oldsize;
  pdata->tend = pdata->trail + 2*oldsize;
}

static SP_BOOL
not_fixpoint(struct linear_data *pdata)
{
  TERM elt;
  if (pdata->nonground==0) {
    return TRUE;
  } else {
    switch (pdata->op) {
    case 1:
      elt = pdata->heap[0];
      return (pdata->bigf < INTERVAL(elt) || pdata->bige <= 0);
    case 2:
      elt = pdata->heap[0];
      return (pdata->bige < INTERVAL(elt) || pdata->bigf <= 0);
    case 3:
      elt = pdata->heap[0];
      return (pdata->gcd_due ||
	      pdata->bigf < INTERVAL(elt) ||
	      pdata->bige < INTERVAL(elt));
    case 4:
    default:
      return (pdata->nonground <= 1);
    }
  }
}

static DAEMON_RC SPCDECL 
linear_daemon(Wam wam,
	      void *vdata,
	      SP_globref attr_ref,
	      TAGGED *global)
{
  struct linear_data *pdata = (struct linear_data *)vdata;
  TERM elt = (int)((attr_ref - pdata->refbase)>>1);
  SP_BOOL buried;
  SP_integer cminj, cmaxj;
  TAGGED tstate;
  struct trail_item *ttop;
  TAGGED tmin, tmax;
  TERM *target = pdata->target;
  int *tloc = pdata->tloc;
  DAEMON_RC rc = DAEMON_FIX;
  
  tstate = fd_daemon_copy_state(wam, global,&buried);
  pdata->depth = GetSmall_int(CTagToArg(tstate,3)) + buried;
  TRAIL(elt,DEPTH(elt));
  ttop = pdata->trail + GetSmall(CTagToArg(tstate,4));
  while (pdata->ttop > ttop) {
    struct trail_item item = *--pdata->ttop;
    SP_globref ref;
    SP_integer c = COEFF(item.elt);
    SP_integer key = INTERVAL(item.elt);
    DEPTH(item.elt) = item.depth;
    ref = pdata->refbase + 2*item.elt;
    REF_GET_BOUNDS(ref, tmin, tmax);
    if (c>0) {
      cminj = c*GetSmall(tmin);
      cmaxj = c*GetSmall(tmax);
    } else {
      cmaxj = c*GetSmall(tmin);
      cminj = c*GetSmall(tmax);
    }
    /* refresh state for item.elt, unless it was unchanged */
    if (cminj==CMIN(item.elt) && cmaxj==CMAX(item.elt))
      continue;
    pdata->bigf += CMIN(item.elt);
    pdata->bige -= CMAX(item.elt);
    CMIN(item.elt) = cminj;
    CMAX(item.elt) = cmaxj;
    INTERVAL(item.elt) = cmaxj - cminj;
    pdata->bigf -= cminj;
    pdata->bige += cmaxj;
    if (key==0 && cminj<cmaxj) {
      int loc = tloc[item.elt];
      TERM swap = target[pdata->nonground];
      target[loc] = swap;
      tloc[swap] = loc;
      target[pdata->nonground] = item.elt;
      tloc[item.elt] = pdata->nonground++;
    } else if (key>0 && cminj==cmaxj) {
      int loc = tloc[item.elt];
      TERM swap = target[--pdata->nonground];
      target[loc] = swap;
      tloc[swap] = loc;
      target[pdata->nonground] = item.elt;
      tloc[item.elt] = pdata->nonground;
    }
    if (pdata->op < 4) {
      if (key < INTERVAL(item.elt)) {
	heap_promote(pdata,pdata->vheap[item.elt]);
      } else if (key > INTERVAL(item.elt)) {
	heap_demote(pdata,pdata->vheap[item.elt]);
      }
    }
  }
  if (pdata->depth > DEPTH(elt)) {
    TRAIL(elt,pdata->depth);
  }
  CTagToArg(tstate,3) = MakeSmall(pdata->depth);
  CTagToArg(tstate,4) = MakeSmall(pdata->ttop - pdata->trail);
  if (not_fixpoint(pdata))
    rc = DAEMON_NOFIX;
  return rc;
}

static SP_BOOL
scalar_product_le(Wam wam, struct linear_data *pdata, TERM elt)
{
  SP_integer c = COEFF(elt);
  Dvar dv = DVAR(elt);
  SP_integer decr;
  SP_integer cmax0 = CMAX(elt);
  SP_integer cmax;

  /*
    Phase 2:

    For <=
    ******
    bigf>=0 is a necessary condition.
    bige<=0 is a sufficient condition.

    enforce:
    bigf >= I_i for all i

    rules:
    x_i <=  floor(F / a_i) + min(x_i)  if a_i>0
    x_i >= -floor(F /-a_i) + max(x_i)  if a_i<0

  */

  if (pdata->bigf<0)
    return FALSE;
  if (c>0) {
    SP_integer ub = pdata->bigf/c + dvar_min_l(dv);
    if (dvar_fix_max_l(dv,ub)<0)
      return FALSE;
    cmax = c*dvar_max_l(dv);
  } else {
    SP_integer lb = -(pdata->bigf/(-c)) + dvar_max_l(dv);
    if (dvar_fix_min_l(dv,lb)<0)
      return FALSE;
    cmax = c*dvar_min_l(dv);
  }
  if (pdata->depth > DEPTH(elt)) {
    TRAIL(elt,pdata->depth);
  }
  CMAX(elt) = cmax;
  decr = cmax0-cmax;
  INTERVAL(elt) -= decr;
  heap_demote(pdata,pdata->vheap[elt]);
  pdata->bige -= decr;
  if (dvar_is_integer(dv)) {
    int loc = VS(elt);
    TERM swap = SV(--pdata->nonground);
    SV(loc) = swap;
    VS(swap) = loc;
    SV(pdata->nonground) = elt;
    VS(elt) = pdata->nonground;
    pdata->gcd_due = TRUE;
  }
  return TRUE;
}

static SP_BOOL
scalar_product_ge(Wam wam, struct linear_data *pdata, TERM elt)
{
  SP_integer c = COEFF(elt);
  Dvar dv = DVAR(elt);
  SP_integer decr;
  SP_integer cmin0 = CMIN(elt);
  SP_integer cmin;

  /*
    Phase 2:

    For >=
    ******
    bige>=0 is a necessary condition.
    bigf<=0 is a sufficient condition.

    enforce:
    bige >= I_i for all i

    rules:
    x_i >= -floor(E / a_i) + max(x_i)  if a_i>0
    x_i <=  floor(E /-a_i) + min(x_i)  if a_i<0
  */

  if (pdata->bige<0)
    return FALSE;
  if (c>0) {
    SP_integer lb = -(pdata->bige/c) + dvar_max_l(dv);
    if (dvar_fix_min_l(dv,lb)<0)
      return FALSE;
    cmin = c*dvar_min_l(dv);
  } else {
    SP_integer ub = pdata->bige/(-c) + dvar_min_l(dv);
    if (dvar_fix_max_l(dv,ub)<0)
      return FALSE;
    cmin = c*dvar_max_l(dv);
  }
  if (pdata->depth > DEPTH(elt)) {
    TRAIL(elt,pdata->depth);
  }
  CMIN(elt) = cmin;
  decr = cmin-cmin0;
  INTERVAL(elt) -= decr;
  heap_demote(pdata,pdata->vheap[elt]);
  pdata->bigf -= decr;
  if (dvar_is_integer(dv)) {
    int loc = VS(elt);
    TERM swap = SV(--pdata->nonground);
    SV(loc) = swap;
    VS(swap) = loc;
    SV(pdata->nonground) = elt;
    VS(elt) = pdata->nonground;
    pdata->gcd_due = TRUE;
  }
  return TRUE;
}

static SP_BOOL
scalar_product_setmin(struct linear_data *pdata)
{
  int i;
  for (i=0; i<pdata->nonground; i++) {
    TERM elt = SV(i);
    Dvar dv = DVAR(elt);

    if (dvar_fix_value_l(dv, CMIN(elt)/COEFF(elt))<0)
      return FALSE;
  }
  return TRUE;
}

static SP_BOOL
scalar_product_setmax(struct linear_data *pdata)
{
  int i;
  for (i=0; i<pdata->nonground; i++) {
    TERM elt = SV(i);
    Dvar dv = DVAR(elt);

    if (dvar_fix_value_l(dv, CMAX(elt)/COEFF(elt))<0)
      return FALSE;
  }
  return TRUE;
}

/* assert: c1>1 && c2>1 */
static INLINE SP_integer gcd(SP_integer c1,SP_integer c2)
{
  while (TRUE) {
    if (c1==c2) {
      return c1;
    } else if (c1<c2) {
      if (c1==0)
	return c2;
      else
	c2 %= c1;
    } else {
      if (c2==0)
	return c1;
      else
	c1 %= c2;
    }
  }
}

static SP_BOOL
gcd_check(struct linear_data *pdata)
{
  SP_integer g = ABSCOEFF(SV(0));
  int i;
  
  for (i=1; i<pdata->nonground && g!=1; i++) {
    g = gcd(g,ABSCOEFF(SV(i)));
  }
  pdata->gcd_due = FALSE;
  return (g==1 || pdata->bigf % g == 0);
}

SP_BOOL
fd_linear_filter_fast(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle, t1;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  SP_integer state_stamp;
  int nonground0, nvars, i, j, k, ent = -1;
  SP_integer total_size;
  char *ptr;
  struct linear_data *pdata;

/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct linear_data,handle);
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get CX0 */
    nvars = fd_list_length(tvec);	/* count terms */
    total_size = 
      nvars*sizeof(struct dvar) +
      5*nvars*sizeof(SP_integer) +
      2*nvars*sizeof(TERM) +
      3*nvars*sizeof(int);
  
    pdata = Palloc(struct linear_data, total_size, handle);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->term.cmin = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.cmax = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.interval = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.coeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.abscoeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->target = (TERM *)ptr;
    ptr += nvars*sizeof(TERM);
    pdata->heap = (TERM *)ptr;
    ptr += nvars*sizeof(TERM);
    pdata->vheap = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->tloc = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->term.depth = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->trail = SP_malloc(nvars*sizeof(struct trail_item));
    pdata->ttop = pdata->trail;
    pdata->tend = pdata->trail+nvars;
    pdata->depth = 0;
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    DerefArg(telt,X(0),2);
    pdata->op = GetSmall(telt) & 0x7;
    pdata->destructor = linear_destructor;
    pdata->daemon = linear_daemon;
    pdata->nvars = nvars;
    DerefArg(tvec,X(0),1);	/* get CX0 */
    DerefArg(telt,X(0),3);	/* get RHS */
    pdata->bigf = GetSmall(telt);
    pdata->bige = -GetSmall(telt);
				/* xfer all info to the struct linear_terms */
    for (i=0; i<nvars; i++) {
      TERM elt = i;
      SP_integer c;
      
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);
      COEFF(elt) = c = GetSmall(t1);
      ABSCOEFF(i) = (c>=0 ? c : -c);
      DEPTH(elt) = 0;
      fd_get_var_and_attr(telt+WD(1),RefAttr(elt));
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    for (i=0; i<nvars; i++) {
      TERM elt = i;
      Dvar dv = DVAR(elt);
      TAGGED functor;
      dvar_init(dv, RefAttr(elt), RefVar(elt));
      switch (pdata->op) {
      case 4:
	functor = fd.functor_val;
	break;
      default:
	functor = fd.functor_minmax;
	break;
      }
      dvar_attach_daemon(wam, dv, pdata, X(1), functor);
    }
    /*pasted*/
    for (i=0, j=0, k=nvars; i<nvars; i++) {
      TERM elt = i;
      SP_WORD c = COEFF(elt);
      Dvar dv = DVAR(elt);

      if (c>0) {
	CMIN(elt) = c*dvar_min_l(dv);
	CMAX(elt) = c*dvar_max_l(dv);
      } else {
	CMAX(elt) = c*dvar_min_l(dv);
	CMIN(elt) = c*dvar_max_l(dv);
      }
      INTERVAL(elt) = CMAX(elt) - CMIN(elt);
      if (pdata->op<4) {
	pdata->heap[i] = elt;
	pdata->vheap[elt] = i;
      }
      pdata->bigf -= CMIN(elt);
      pdata->bige += CMAX(elt);
      if (dvar_is_integer(dv)) {
	VS(elt) = --k;
	SV(k) = elt;
      } else {
	VS(elt) = j;
	SV(j++) = elt;
      }
    }
    pdata->nonground = j;
    if (pdata->op<4)
      for (i=(nvars-2)>>1; i>=0; i--)
	heap_demote(pdata,i);
    /*pasted*/
  }
  
				/* RESUME HERE */
  nonground0 = pdata->nonground;
  for (i=0; i<nonground0; i++) {
    TERM elt = SV(i);
    dvar_refresh(DVAR(elt));
  }
  /* fast special cases */
  switch (pdata->op) {
  case 1:
    if (pdata->bigf < 0) {
      goto ret;
    } else if (pdata->bigf==0) {
      ent = (scalar_product_setmin(pdata) ? 1 : -1);
      goto export;
    }
    break;
  case 2:
    if (pdata->bige < 0) {
      goto ret;
    } else if (pdata->bige==0) {
      ent = (scalar_product_setmax(pdata) ? 1 : -1);
      goto export;
    }
    break;
  case 3:
    if (pdata->bigf < 0) {
      goto ret;
    } else if (pdata->bige < 0) {
      goto ret;
    } else if (pdata->bigf==0) {
      ent = (scalar_product_setmin(pdata) ? 1 : -1);
      goto export;
    } else if (pdata->bige==0) {
      ent = (scalar_product_setmax(pdata) ? 1 : -1);
      goto export;
    }
    break;
  }
  pdata->gcd_due = TRUE;
  switch (pdata->op) {
  case 1:			/* #=< */
    if (pdata->nonground>0) {
      TERM elt = pdata->heap[0];
      if (pdata->bigf < INTERVAL(elt)) {
	if (!scalar_product_le(wam, pdata,elt))
	  goto ret;
      }
    }
    break;
  case 2:			/* #>= */
    if (pdata->nonground>0) {
      TERM elt = pdata->heap[0];
      if (pdata->bige < INTERVAL(elt)) {
	if (!scalar_product_ge(wam, pdata,elt))
	  goto ret;
      }
    }
    break;
  case 3:			/* #= */
    if (pdata->nonground>0) {
      TERM elt = pdata->heap[0];
      if (pdata->gcd_due && !gcd_check(pdata))
	goto ret;
      if (pdata->bigf < INTERVAL(elt)) {
	if (!scalar_product_le(wam, pdata,elt))
	  goto ret;
      }
      if (pdata->bige < INTERVAL(elt)) {
	if (!scalar_product_ge(wam, pdata,elt))
	  goto ret;
      }
    }
    break;
  case 4:			/* #\= */
    if (pdata->nonground==0 && pdata->bigf==0) {
      goto ret;
    } else if (pdata->nonground==1) {
      TERM elt = SV(0);
      Dvar dv = DVAR(elt);
      
      if (pdata->bigf % COEFF(elt)==0) { /* RHS a multiple of coefficient */
	dvar_prune_value_l(dv,(pdata->bigf+CMIN(elt))/COEFF(elt));
      }
    }
    break;
  }
  switch (pdata->op) {
  case 1:
    ent = (pdata->bigf < 0 ? -1 : pdata->bige <= 0);
    break;
  case 2:
    ent = (pdata->bige < 0 ? -1 : pdata->bigf <= 0);
    break;
  case 3:
    ent = (pdata->bigf < 0 ? -1 :
	   pdata->bige < 0 ? -1 : 
	   pdata->bigf > 0 ? 0 :
	   pdata->bige > 0 ? 0 : 1);
    break;
  case 4:
    ent = (pdata->nonground<=1);
    break;
  }

  if (!ent && not_fixpoint(pdata))
    fd_not_fixpoint(wam);
 export:
  for (i=0; i<nonground0; i++) {
    TERM elt = SV(i);
    dvar_export(DVAR(elt));
  }
  CTagToArg(X(0),3) = MakeSmall(pdata->depth);
  CTagToArg(X(0),4) = MakeSmall(pdata->ttop - pdata->trail);
 ret:
  return ent;
}

struct gcd_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;

  SP_integer stamp;		/* increases up to backtracking */
  SP_globref refbase;
  SP_integer gcdall;		/* valid during GCD rules */
  SP_integer rhs;
  int nedges;			/* maintained incrementally */
  int nvars;			/* #terms */
  int ntargets;			/* #terms that may be targets */
  TERM *target;
  int *tloc;
  Dvar dvar;
  struct {
    SP_integer *coeff;		/* ai */
    SP_integer *abscoeff;	/* |ai| */
    SP_integer *gcd;		/* valid during GCD rules */
    TERM *mate;
  } term;
};

#define GCD(t) (pdata->term.gcd[t])
#define MATE(t) (pdata->term.mate[t])

static void SPCDECL gcd_destructor(void *pdata_v)
{
  struct gcd_data *pdata = (struct gcd_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata);
}

/* TRUE iff gcdall>1 or GCD(j)>1 (* and ABSCOEFF(j)>1 *) for some j, */
/* i.e. pruning is possible */
/* DONALD puzzle can prune even if ABSCOEFF(j)==1 */
/* FALSE means that pdata->gcdall and pdata->GCD(_) are undefined */
static SP_BOOL
refresh_gcd_aux(struct gcd_data *pdata)
{
  int ntargets = pdata->ntargets;

  if (ntargets==0 || pdata->nedges>1) {
    return FALSE;
  } else {	/* for each i, compute GCD of all coefficients except i */
    SP_integer g;
    int i;
    int first1;			/* GCD(SV(first1...)) are all 1 */
    int last1;			/* GCD(SV(...last1)) are all 1 */
    SP_BOOL rc = FALSE;
    
    g = ABSCOEFF(SV(0));
    for (i=1; i<ntargets && g!=1; i++) {
      TERM elt = SV(i);
      
      GCD(elt) = g;
      g = gcd(g,ABSCOEFF(elt));
    }
    first1 = i;
    pdata->gcdall = g;
				/* now, GCD(SV(i)) = gcd(a[0] ... a[i-1]) */
    rc |= (first1==ntargets);
    if (!rc)
      GCD(SV(ntargets-1)) = 1;
    g = ABSCOEFF(SV(ntargets-1));
    for (i=ntargets-2; i>0 && g!=1; i--) {
      TERM elt = SV(i);
      
      GCD(elt) = i>=first1 ? 1 : gcd(g,GCD(elt));
      rc |= (GCD(elt)>1 /* && ABSCOEFF(elt)>1 */);
      g = gcd(g,ABSCOEFF(elt));
    }
    last1 = i;
    GCD(SV(0)) = g;
    rc |= (g>1 /* && ABSCOEFF(SV(0))>1 */);
    if (rc) {
      for (i=1; i<=last1; i++)
	GCD(SV(i)) = 1;
    }
				/* now, GCD(SV(i)) = gcd({a[j] | i!=j}) */
    return rc;
  }
}

static void
gcd_handle_ground(struct gcd_data *pdata,TERM varno,SP_integer cvalue)
{
  int loc = VS(varno);
  TERM swap = SV(--pdata->ntargets);
  TERM mate = MATE(varno);
  
  SV(loc) = swap;
  VS(swap) = loc;
  SV(pdata->ntargets) = varno;
  VS(varno) = pdata->ntargets;
  pdata->rhs -= cvalue;
  if (mate==varno ||		/* self-loop */
      (mate > -1 && VS(mate) < pdata->ntargets)) { /* proper edge, mate among targets */
    --pdata->nedges;
  }
}

static DAEMON_RC SPCDECL 
gcd_daemon(Wam wam,
	   void *vdata,
	   SP_globref attr_ref,
	   TAGGED *global)
{
  struct gcd_data *pdata = (struct gcd_data *)vdata;
  SP_BOOL buried = FALSE;
  TAGGED tstate = fd_daemon_copy_state(wam, global,&buried); 
  int varno = (int)((attr_ref - pdata->refbase)>>1);
  int enable;
  TAGGED tmin, tmax;
  DAEMON_RC rc = DAEMON_FIX;
  
  pdata->rhs = GetSmall(CTagToArg(tstate,2)); /* Sum */
  pdata->ntargets = pdata->nvars - GetSmall_int(CTagToArg(tstate,3)); /* NGround */
  pdata->nedges = GetSmall_int(CTagToArg(tstate,4)); /* NEdges */
  enable = (pdata->nedges<=1 && pdata->gcdall>1);
  REF_GET_BOUNDS(attr_ref, tmin, tmax);
  if (tmin==tmax && VS(varno)<pdata->ntargets) { /* if varno not among targets then self-invocation */
    gcd_handle_ground(pdata,varno,COEFF(varno)*GetSmall(tmin));
    CTagToArg(tstate,2) = MakeSmall(pdata->rhs); /* update Sum */
    CTagToArg(tstate,3) = MakeSmall(pdata->nvars-pdata->ntargets); /* update NGround */
    CTagToArg(tstate,4) = MakeSmall(pdata->nedges); /* update NEdges */
  }
  if (pdata->nedges<=1)
    enable = refresh_gcd_aux(pdata);
  if (pdata->ntargets==0 || enable)
    rc = DAEMON_NOFIX;
  return rc;
}

/* Preconditions:
   0<coeff<mod, 0=<rhs<mod, 0<mod

   Solve min X such that coeff*X = rhs (modulo mod)
*/
static SP_integer solve_gcd(SP_integer coeff, SP_integer rhs, SP_integer mod)
{
  if (rhs==0)
    return 0;
  else {
    SP_integer rhs1 = rhs%coeff;
    if (rhs1==0)
      return rhs/coeff;
    else
      return (rhs + mod*solve_gcd(mod%coeff, coeff-rhs1, coeff)) / coeff;
  }
}

/* Preconditions: 
   0<mod, gcdall = gcd(coeff,mod)

   Adjusts minx up and maxx down s.t.
   Returns smallest s.t. coeff*minx = coeff*maxx = rhs (modulo mod)
*/
static void 
adjust_bounds_gcd(SP_integer coeff, SP_integer rhs,
		  SP_integer mod, SP_integer gcdall,
		  SP_integer *minx, SP_integer *maxx)
{
  SP_integer minx0 = *minx, maxx0 = *maxx;
  SP_integer q = mod/gcdall;
  SP_integer r, x, s;
  SP_integer rhslocal;

  if (coeff>0) {
    rhslocal = rhs;
  } else {
    rhslocal = -rhs;
    coeff = -coeff;
  }
  coeff %= mod;
  rhslocal %= mod;
  if (rhslocal<0)		/* ensure mod, not rem */
    rhslocal += mod;
  s = solve_gcd(coeff, rhslocal, mod);
  r = minx0 % q;
  if (r<0)
    r += q;
  x = minx0 - r + s;
  if (x<minx0)
    x += q;
  *minx = x;
  r = maxx0 % q;
  if (r<0)
    r += q;
  x = maxx0 - r + s;
  if (x>maxx0)
    x -= q;
  *maxx = x;
}

/*
  '$fd_gcd_aux'(+State0, -State, -Actions).
  State = state(Vec,Sum,NGround,NEdges,Handle,Stamp) where CX are all non-ground
*/
void SPCDECL
prolog_fd_gcd_aux(Wam wam,
		  SP_term_ref State0,
		  SP_term_ref State,
		  SP_term_ref Actions)
{
  TAGGED tvec, telt, handle, t1;
  SP_BOOL posted, committed;
  SP_integer state_stamp;
  int nvars, i, j, k, ent = -1, ntargets0;
  SP_integer total_size;
  char *ptr;
  struct gcd_data *pdata;
  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    posted = FALSE;
    pdata = Pdata(struct gcd_data,handle);
  } else {			/* build persistent state */
    posted = TRUE;
    DerefArg(tvec,X(0),1);	  /* get Vec */
    nvars = fd_list_length(tvec); /* count terms */
    total_size = 
      nvars*sizeof(struct dvar) +
      3*nvars*sizeof(SP_integer) +
      2*nvars*sizeof(TERM) +
      nvars*sizeof(int);
  
    pdata = Palloc(struct gcd_data, total_size, handle);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->term.coeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.abscoeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.gcd = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.mate = (TERM *)ptr;
    ptr += nvars*sizeof(TERM);
    pdata->target = (TERM *)ptr;
    ptr += nvars*sizeof(TERM);
    pdata->tloc = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->destructor = gcd_destructor;
    pdata->daemon = gcd_daemon;
    pdata->nvars = nvars;
    DerefArg(tvec,X(0),1);	/* get Vec */
    DerefArg(telt,X(0),2);	/* get Sum */
    pdata->rhs = GetSmall(telt);
    pdata->nedges = 0;
    for (i=0, j=0, k=nvars; i<nvars; i++) {
      SP_integer c;
      Dvar dv = DVAR(i);
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);
      COEFF(i) = c = GetSmall(t1);
      ABSCOEFF(i) = (c>=0 ? c : -c);
      MATE(i) = -1;
      fd_get_var_and_attr(telt+WD(1),RefAttr(i));
      dvar_init(dv, RefAttr(i), RefVar(i));
      if (dvar_is_integer(dv)) {
	pdata->rhs -= c*dvar_min_l(dv);
	SV(--k) = i;
	VS(i) = k;
      } else {
	VS(i) = j;
	SV(j++) = i;
      }
    }
    pdata->ntargets = k;
    for (i=0; i<k; i++) {
      Dvar dv = DVAR(SV(i));
      SP_integer aci = ABSCOEFF(i);
      
      dvar_attach_daemon(wam, dv, pdata, X(1), fd.functor_minmax);
      if (aci==1) {
	MATE(i) = i;
	pdata->nedges++;
      } else if (MATE(i) > -1) {
      } else {
	for (j=i+1; j<k; j++) {
	  SP_integer acj = ABSCOEFF(j);
	  if (acj>1 && gcd(aci,acj)==1 && MATE(j)==-1) {
	    MATE(i) = j;
	    MATE(j) = i;
	    pdata->nedges++;
	    break;
	  }
	}
      }	
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free Vec for GC */
    if (!(pdata->nedges<=1 && refresh_gcd_aux(pdata)))
      goto nofilter;
  }
  (void)posted; /* [PM] 4.3 avoid warning about unused assignment */
  
				/* RESUME HERE with pdata->* refreshed by daemon */
  pdata->stamp = state_stamp+1;
  if (pdata->ntargets==0)
    goto nofilter;
  ntargets0 = pdata->ntargets;	/* the DVARs that we touch in the propagator */
  for (i=0; i<ntargets0; i++) {
    int j = SV(i);
    Dvar dv = DVAR(j);
    dvar_refresh(dv);
  }
  if (pdata->rhs % pdata->gcdall != 0)
    goto ret;
  for (i=0; i<pdata->ntargets; i++) {
    TERM elt = SV(i);
    Dvar dv = DVAR(elt);
    SP_integer imin = dvar_min_l(dv);
    SP_integer imax = dvar_max_l(dv);
      
    if (imin<imax && GCD(elt)>1) {
      /* Ensure that:

	 Ai * min(Xi) = Ai * max(Xi) = RHS (modulo G)

	 where G is the gcd of all coefficients excepts Ai.
      */
      SP_integer c = COEFF(elt);
      int rc;
      adjust_bounds_gcd(c,
			pdata->rhs,
			GCD(elt),
			pdata->gcdall,
			&imin, &imax);
      rc = dvar_fix_interval_l(dv, imin, imax);
      if (rc<0) {
	goto ret;
      } else if (rc>0) {
	if (dvar_is_integer(dv)) {
	  gcd_handle_ground(pdata,elt,c*dvar_min_l(dv));
	  if (refresh_gcd_aux(pdata)) {
	    fd_not_fixpoint(wam);
	    break;
	  }
	} else if (dvar_min_l(dv) > imin || dvar_max_l(dv) < imax) { /* hit a hole */
	  fd_not_fixpoint(wam);
	  break;
	}
      }
    }
  }

  for (i=0; i<ntargets0; i++)
    dvar_pruning_done( DVAR(SV(i)));
  for (i=0; i<ntargets0; i++)
    dvar_export(DVAR(SV(i)));
 nofilter:
  CTagToArg(X(0),2) = MakeSmall(pdata->rhs); /* update Sum */
  CTagToArg(X(0),3) = MakeSmall(pdata->nvars-pdata->ntargets); /* update NGround */
  CTagToArg(X(0),4) = MakeSmall(pdata->nedges); /* update NEdges */
  ent = (pdata->ntargets==0);
  if (ent==1)
    Pfree;
 ret:
  dvar_export_done(wam, Actions, ent);
}
