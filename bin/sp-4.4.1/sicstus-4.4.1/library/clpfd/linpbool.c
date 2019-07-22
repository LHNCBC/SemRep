/* Copyright(C) 1999, Swedish Institute of Computer Science */
/* Linear arithmetic over 0-1 variables, aka. pseudo-boolean constraints */

#include "fd.h"
#include "dvars.h"

typedef int TERM;

struct dlist {
  TERM term;
  struct dlist *forward;
  struct dlist *backward;
};

#define TRAIL(E)				\
{						\
  *pdata->ttop++ = pdata->dlist_item+(E);	\
}

struct linear_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam, void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;

  SP_globref refbase;
  int op;			/* 1(#=<), 2(#>=), 3(#=), 4(#\=) */
  int nonground;		/* maintained incrementally */
  int nvars;			/* #terms */
  SP_integer bige;
  SP_integer bigf;
  TERM *target;
  int *tloc;
  Dvar dvar;
  struct {
    SP_integer *cmin;		/* min(ai*xi) */
    SP_integer *cmax;		/* max(ai*xi) */
    SP_integer *interval;	/* cmax-cmin */
    SP_integer *coeff;		/* ai */
  } term;
  struct dlist root;
  struct dlist *dlist_item;	/* [nvars] */
};

  /* Maintain:
     bigf = rhs - sum{min(a_i x_i)}
     bige = sum{max(a_i x_i)} - rhs

     I_i = max(a_i x_i) - min(a_i x_i)
  */

#define SV(I)   (pdata->target[I])
#define VS(I)   (pdata->tloc[I])
#define COEFF(t) (pdata->term.coeff[t])
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
  SP_free(pdata);
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
  TAGGED tstate = fd_daemon_copy_state(wam, global,&buried);
  int nonground = pdata->nvars - GetSmall_int(CTagToArg(tstate,4));
  DAEMON_RC rc = DAEMON_FIX;

  while (pdata->nonground < nonground) {	/* restore state wrt. trail */
    TERM ielt = SV(pdata->nonground++);
    struct dlist *item = pdata->dlist_item+ielt;
    SP_integer c = COEFF(ielt);
    
    if (c>0) {
      cminj = 0;
      cmaxj = c;
    } else {
      cmaxj = 0;
      cminj = c;
    }
    pdata->bigf += CMIN(ielt)-cminj;
    pdata->bige -= CMAX(ielt)-cmaxj;
    CMIN(ielt) = cminj;
    CMAX(ielt) = cmaxj;
    INTERVAL(ielt) = cmaxj - cminj;
				/* dlist_insert(item); */
    item->backward->forward = item;
    item->forward->backward = item;
  }
  {				/* update state wrt. elt */
    struct dlist *item = pdata->dlist_item+elt;
    SP_globref ref = pdata->refbase + 2*elt;
    SP_integer c = COEFF(elt);
    TAGGED tmin, tmax;
    
    REF_GET_BOUNDS(ref, tmin, tmax);
    (void) tmax; /* [PM] 4.3 avoid warning about unused assignment */
    cminj = (Tgtz(tmin) ? c : 0);
    pdata->bigf += CMIN(elt)-cminj;
    pdata->bige -= CMAX(elt)-cminj;
    CMIN(elt) = cminj;
    CMAX(elt) = cminj;
    INTERVAL(elt) = 0;
    {
      int loc = VS(elt);
      TERM swap = SV(--pdata->nonground);
      SV(loc) = swap;
      VS(swap) = loc;
      SV(pdata->nonground) = elt;
      VS(elt) = pdata->nonground;
    }
				/* dlist_delete(item); */
    item->backward->forward = item->forward;
    item->forward->backward = item->backward;
  }
  if (pdata->nonground==0) {
    rc = DAEMON_NOFIX;
  } else {
    switch (pdata->op) {
    case 1:
      elt = pdata->root.forward->term;
      if (pdata->bigf < INTERVAL(elt) || pdata->bige <= 0)
	rc = DAEMON_NOFIX;
      break;
    case 2:
      elt = pdata->root.forward->term;
      if (pdata->bige < INTERVAL(elt) || pdata->bigf <= 0)
	rc = DAEMON_NOFIX;
      break;
    case 3:
      elt = pdata->root.forward->term;
      if (pdata->bigf < INTERVAL(elt) ||
	  pdata->bige < INTERVAL(elt))
	rc = DAEMON_NOFIX;
      break;
    case 4:
      if (pdata->nonground <= 1)
	rc = DAEMON_NOFIX;
      break;
    }
  }
  CTagToArg(tstate,4) = MakeSmall(pdata->nvars-pdata->nonground); /* update NGround */
  return rc;
}

static SP_BOOL
scalar_product_le(Wam wam, struct linear_data *pdata, TERM elt)
{
  SP_integer c = COEFF(elt);
  struct dlist *item = pdata->dlist_item+elt;

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
    if (!bool_export_value(wam, RefAttr(elt), RefVar(elt), 0))
      return FALSE;
    CMAX(elt) = 0;
    pdata->bige -= c;
  } else {
    if (!bool_export_value(wam, RefAttr(elt), RefVar(elt), 1))
      return FALSE;
    CMAX(elt) = c;
    pdata->bige += c;
  }
  INTERVAL(elt) = 0;
  {
    int loc = VS(elt);
    TERM swap = SV(--pdata->nonground);
    SV(loc) = swap;
    VS(swap) = loc;
    SV(pdata->nonground) = elt;
    VS(elt) = pdata->nonground;
  }
				/* dlist_delete(elt); */
  item->backward->forward = item->forward;
  item->forward->backward = item->backward;
  return TRUE;
}

static SP_BOOL
scalar_product_ge(Wam wam, struct linear_data *pdata, TERM elt)
{
  SP_integer c = COEFF(elt);
  struct dlist *item = pdata->dlist_item+elt;

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
    if (!bool_export_value(wam, RefAttr(elt), RefVar(elt), 1))
      return FALSE;
    CMIN(elt) = c;
    pdata->bigf -= c;
  } else {
    if (!bool_export_value(wam, RefAttr(elt), RefVar(elt), 0))
      return FALSE;
    CMIN(elt) = 0;
    pdata->bigf += c;
  }
  INTERVAL(elt) = 0;
  {
    int loc = VS(elt);
    TERM swap = SV(--pdata->nonground);
    SV(loc) = swap;
    VS(swap) = loc;
    SV(pdata->nonground) = elt;
    VS(elt) = pdata->nonground;
  }
				/* dlist_delete(elt); */
  item->backward->forward = item->forward;
  item->forward->backward = item->backward;
  return TRUE;
}

static SP_BOOL
scalar_product_setmin(Wam wam, struct linear_data *pdata)
{
  int i;
  for (i=0; i<pdata->nonground; i++) {
    TERM elt = SV(i);
    if (!bool_export_value(wam, RefAttr(elt), RefVar(elt), COEFF(elt)<0))
      return FALSE;
  }
  return TRUE;
}

static SP_BOOL
scalar_product_setmax(Wam wam, struct linear_data *pdata)
{
  int i;
  for (i=0; i<pdata->nonground; i++) {
    TERM elt = SV(i);
    if (!bool_export_value(wam, RefAttr(elt), RefVar(elt), COEFF(elt)>0))
      return FALSE;
  }
  return TRUE;
}

/* sort by descending interval */
static int 
cmp_dec_interval(Wam wam, TERM *v1, TERM *v2)
{
  struct linear_data *pdata = fd.gdata; 
  SP_integer d1 = INTERVAL(*v1);
  SP_integer d2 = INTERVAL(*v2);

  return (d1>d2) ? -1 : (d1<d2) ? 1 : 0;
}

#define QType TERM
#define QCmp  cmp_dec_interval
#define QSort qsort_dec_interval
#include "qsort.ic"

SP_BOOL
fd_linear_filter_pbool(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle, t1;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  SP_integer state_stamp;
  int nvars, i, j, ent = -1;
  SP_integer total_size;
  char *ptr;
  struct linear_data *pdata;

/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct linear_data,handle);
  } else {			/* build persistent state */
    struct dlist *curitem;
    
    DerefArg(tvec,X(0),1);	  /* get CX0 */
    nvars = fd_list_length(tvec); /* count terms */
    total_size = 
      nvars*sizeof(struct dvar) +
      nvars*sizeof(struct dlist) +
      4*nvars*sizeof(SP_integer) +
      nvars*sizeof(TERM) +
      nvars*sizeof(int);
  
    pdata = Palloc(struct linear_data, total_size, handle);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->dlist_item = (struct dlist *)ptr;
    ptr += nvars*sizeof(struct dlist);
    pdata->term.cmin = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.cmax = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.interval = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.coeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->target = (TERM *)ptr;
    ptr += nvars*sizeof(TERM);
    pdata->tloc = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
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
      
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);
      COEFF(elt) = GetSmall(t1);
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
    for (i=0, j=0; i<nvars; i++) {
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
      pdata->bigf -= CMIN(elt);
      pdata->bige += CMAX(elt);
      if (!dvar_is_integer(dv)) {
	VS(elt) = j;
	SV(j++) = elt;
      }
    }
    pdata->nonground = j;
    /* build the dlist by decreasing interval */
    fd.gdata = pdata;
    qsort_dec_interval(wam, pdata->target,pdata->nonground);
    curitem = &pdata->root;
    for (i=0; i<pdata->nonground; i++) {
      TERM elt = SV(i);
      struct dlist *item = pdata->dlist_item+elt;

      VS(elt) = i;
      item->term = elt;
      curitem->forward = item;
      item->backward = curitem;
      curitem = item;
    }
    curitem->forward = &pdata->root;
    pdata->root.backward = curitem;
  }
  
				/* RESUME HERE */

  /* fast special cases */
  switch (pdata->op) {
  case 1:
    if (pdata->bigf < 0) {
      goto ret;
    } else if (pdata->bigf==0) {
      ent = (scalar_product_setmin(wam, pdata) ? 1 : -1);
      goto export;
    }
    break;
  case 2:
    if (pdata->bige < 0) {
      goto ret;
    } else if (pdata->bige==0) {
      ent = (scalar_product_setmax(wam, pdata) ? 1 : -1);
      goto export;
    }
    break;
  case 3:
    if (pdata->bigf < 0) {
      goto ret;
    } else if (pdata->bige < 0) {
      goto ret;
    } else if (pdata->bigf==0) {
      ent = (scalar_product_setmin(wam, pdata) ? 1 : -1);
      goto export;
    } else if (pdata->bige==0) {
      ent = (scalar_product_setmax(wam, pdata) ? 1 : -1);
      goto export;
    }
    break;
  }
 loop:
  switch (pdata->op) {
  case 1:			/* #=< */
    if (pdata->nonground>0) {
      TERM elt = pdata->root.forward->term;
      if (pdata->bigf < INTERVAL(elt)) {
	if (!scalar_product_le(wam, pdata,elt)) {
	  goto ret;
	} else {
	  goto loop;
	}
      }
    }
    break;
  case 2:			/* #>= */
    if (pdata->nonground>0) {
      TERM elt = pdata->root.forward->term;
      if (pdata->bige < INTERVAL(elt)) {
	if (!scalar_product_ge(wam, pdata,elt)) {
	  goto ret;
	} else {
	  goto loop;
	}
      }
    }
    break;
  case 3:			/* #= */
    if (pdata->nonground>0) {
      TERM elt = pdata->root.forward->term; 
      if (pdata->bigf < INTERVAL(elt)) {
	if (!scalar_product_le(wam, pdata,elt))
	  goto ret;
	goto loop;
      }
      if (pdata->bige < INTERVAL(elt)) {
	if (!scalar_product_ge(wam, pdata,elt))
	  goto ret;
	goto loop;
      }
    }
    break;
  case 4:			/* #\= */
    if (pdata->nonground==0 && pdata->bigf==0) {
      goto ret;
    } else if (pdata->nonground==1) {
      TERM elt = pdata->root.forward->term;
      
      if (pdata->bigf % COEFF(elt)==0) { /* RHS a multiple of coefficient */
	SP_integer nono = (pdata->bigf+CMIN(elt))/COEFF(elt);
	if (nono >= 0 && nono <= 1) {
	  if (!bool_export_value(wam, RefAttr(elt), RefVar(elt), 1-(int)nono))
	    return -1;
	}
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

 export:
  CTagToArg(X(0),4) = MakeSmall(pdata->nvars-pdata->nonground); /* update NGround */
 ret:
  return ent;
}
