/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

struct minmax_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  SP_globref refbase;
  int nrefs;
  int *target;
  Dvar dvar;
};

#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define TARGET(T) (pdata->target[T])
#define DVAR(T) (pdata->dvar+(T))

static void SPCDECL minmax_destructor(void *pdata_v)
{
  struct minmax_data *pdata = (struct minmax_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

/* Daemon for minimum/2: effectively disable Xi that is for sure > Y */
/* Daemon for maximum/2: effectively disable Xi that is for sure < Y */
static DAEMON_RC SPCDECL 
minmax_daemon(Wam wam,
	      void *vdata,
	      SP_globref attr_ref,
	      TAGGED *global)
{
  struct minmax_data *pdata = (struct minmax_data *)vdata;
  TAGGED tstate;
  int varno, i, ntargets;

  (void)wam;
  varno = (int)((attr_ref - pdata->refbase)>>1);
  tstate = RefMutable(CTagToArg(*global,1));
  DerefArg(tstate,tstate,3);	/* get NTargets */
  ntargets = GetSmall_int(tstate);
  for (i=0; i<ntargets; i++)
    if (TARGET(i)==varno)
      return DAEMON_NOFIX;
  return DAEMON_FIX;
}

/*
  '$fd_minmax'(+State0, +State, -Actions).
  State = state(Y,Xs,NTargets,IsMax,Handle,Stamp)
  IsMax is 0 -> minimum/2
  IsMax is 1 -> maximum/2
*/
void SPCDECL
prolog_fd_minmax(Wam wam,
			       SP_term_ref State0,
			       SP_term_ref State,
			       SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, tvec, tismax;
  TAGGED ymin, ymax;
  SP_integer state_stamp, total_size;
  int i, ntargets, count, pivot=0;
  struct minmax_data *pdata;
  SP_BOOL committed;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  DerefArg(tvec,X(0),3);	/* get NTargets */
  DerefArg(tismax,X(0),4);	/* get IsMax */
  ntargets = GetSmall_int(tvec);
  if (ntargets==0) {
    goto ret;
  } else if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct minmax_data,handle);
  } else {			/* build persistent state */
    total_size = (ntargets+1)*(sizeof(int)+sizeof(struct dvar));
    pdata = Palloc(struct minmax_data, total_size, handle);
    pdata->destructor = minmax_destructor;
    pdata->daemon = minmax_daemon;
    pdata->nrefs = (ntargets+1)<<1;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs((ntargets+1)<<1);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += (ntargets+1)*sizeof(struct dvar);
    pdata->target = (int *)ptr;
    ptr += (ntargets+1)*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    DerefArg(tvec,X(0),1);		/* get Y */
    fd_get_var_and_attr(tvec,RefAttr(0));
    dvar_init(DVAR(0), RefAttr(0), RefVar(0));
    DerefArg(tvec,X(0),2);		/* get Xs */
    for (i=0; i<ntargets; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      TARGET(i) = i+1;
      fd_get_var_and_attr(telt,RefAttr(i+1));
    }
    for (i=0; i<ntargets; i++) {
      dvar_init(DVAR(i+1), RefAttr(i+1), RefVar(i+1));
      dvar_attach_daemon(wam, DVAR(i+1), pdata, X(1), fd.functor_minmax); /* [MC] 4.2.3: can GC */
    }
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  dvar_refresh(DVAR(0));
  for (i=0; i<ntargets; i++) {
    int elt = TARGET(i);
    dvar_refresh(DVAR(elt));
  }
 loop:
  count = 0;
  ymin = dvar_min_t(DVAR(0));
  ymax = dvar_max_t(DVAR(0));
  if (tismax==TaggedZero) {
    TAGGED minofmax = Sup;
    TAGGED minofmin = Sup;
    int rc;
    for (i=0; i<ntargets; i++) {
      int elt = TARGET(i);
      Dvar dv = DVAR(elt);
      if (dvar_fix_min_t(dv, ymin)<0) {
	goto ret;
      } else if (FDle(dvar_min_t(dv),ymax)) {
	count++;
	pivot = i;
	if (FDgt(minofmax,dvar_max_t(dv)))
	  minofmax = dvar_max_t(dv);
	if (FDgt(minofmin,dvar_min_t(dv)))
	  minofmin = dvar_min_t(dv);
      }
    }
    rc = dvar_fix_interval_t(DVAR(0), minofmin, minofmax);
    if (rc<0)
      goto ret;
    else if (rc>0)
      goto loop;
  } else {
    TAGGED maxofmax = Inf;
    TAGGED maxofmin = Inf;
    int rc;
    for (i=0; i<ntargets; i++) {
      int elt = TARGET(i);
      Dvar dv = DVAR(elt);
      if (dvar_fix_max_t(dv, ymax)<0) {
	goto ret;
      } else if (FDle(ymin,dvar_max_t(dv))) {
	count++;
	pivot = i;
	if (FDlt(maxofmax,dvar_max_t(dv)))
	  maxofmax = dvar_max_t(dv);
	if (FDlt(maxofmin,dvar_min_t(dv)))
	  maxofmin = dvar_min_t(dv);
      }
    }
    rc = dvar_fix_interval_t(DVAR(0), maxofmin, maxofmax);
    if (rc<0)
      goto ret;
    else if (rc>0)
      goto loop;
  }
  if (count==0) {
    goto ret;
  } else if (count==1) {
    Dvar dv = DVAR(TARGET(pivot));
    if (dvar_fix_set(DVAR(0),dvar_set(dv))<0 ||
	dvar_fix_set(dv,dvar_set(DVAR(0)))<0)
      goto ret;
    ymin = dvar_min_t(DVAR(0));
    ymax = dvar_max_t(DVAR(0));
  }
  dvar_pruning_done( DVAR(0));
  for (i=0; i<ntargets; i++) {
    int elt = TARGET(i);
    dvar_pruning_done( DVAR(elt));
  }
  dvar_export( DVAR(0));
  for (i=0; i<ntargets; i++) {
    int elt = TARGET(i);
    dvar_export( DVAR(elt));
  }
  /* Maintain targets, the Xs that can potentially equal Y */
  /* Entailment condition: integer(Y) and at least one X equals Y */
  {
    int inf = 0, hit = 0;
    int sup = ntargets-1;
    int held = TARGET(sup);		/* sup is the hole */
    int current = TARGET(inf);
    
    while (inf<=sup) {
      TAGGED xmin = dvar_min_t(DVAR(current));
      TAGGED xmax = dvar_max_t(DVAR(current));
      if (tismax==TaggedZero ? FDge(ymax,xmin) : FDle(ymin,xmax)) {
	if (ymin==ymax && xmin==ymin && xmax==ymax)
	  hit = 1;
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : TARGET(sup));
      }
    }
    CTagToArg(X(0),3) = MakeSmall(inf);
    if (inf>0) ent = hit;
  }
  if (ent==1)
    Pfree;
 ret:
  dvar_export_done(wam,Actions, ent);
}

struct bool_channel_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  SP_globref refbase;
  int ntargets;
  int nrefs;
  SP_integer stamp;
  int top;
  int offset;
  int code;
  Dvar dvar;
  int *stack;
  char *sense;
};

static void SPCDECL bool_channel_destructor(void *pdata_v)
{
  struct bool_channel_data *pdata = (struct bool_channel_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

static DAEMON_RC SPCDECL 
bool_channel_daemon(Wam wam,
		    void *vdata,
		    SP_globref attr_ref,
		    TAGGED *global)
{
  struct bool_channel_data *pdata = (struct bool_channel_data *)vdata;
  int ar, varno, state_stamp;
  TAGGED tstate;

  if (pdata->code & 2) {	/* #=, #\= */
    varno = (int)((attr_ref - pdata->refbase)>>1);
    tstate = RefMutable(CTagToArg(*global,1));
    ar = Arity(TagToHeadfunctor(tstate));
    state_stamp = GetSmall_int(CTagToArg(tstate,ar));
    if (pdata->stamp!=state_stamp) { /* non-incremental */
      pdata->top = 0;
    }
    if (pdata->top==0) {
      SP_BOOL buried;
      (void)fd_daemon_copy_state(wam, global,&buried);
      pdata->stamp = state_stamp+1;
    }
    SP_ASSERT(pdata->top < pdata->ntargets);
    if (varno>0)
      pdata->stack[pdata->top++] = varno;
  }
  return DAEMON_NOFIX;
}


static int bool_channel_eq(Wam wam, struct bool_channel_data *pdata,int sense)
{
  int pivot = 0, j, ent = -1;
  TAGGED tvec, tmin, tmax, curdom;
  FDITER it;
  
  dvar_refresh(DVAR(0));
  for (j=0; j<pdata->top; j++) {
    int varno = pdata->stack[j];
    REF_GET_BOUNDS(RefAttr(varno), tmin, tmax);
    if (tmin==tmax) { /* can be false when posting */
      if (Tgtz(tmin)^pdata->sense[varno]^sense) {
	if (dvar_fix_value_l(DVAR(0),varno+pdata->offset-1)<0)
	  goto ret;
      } else {
	if (dvar_prune_value_l(DVAR(0),varno+pdata->offset-1)<0)
	  goto ret;
      }
    }
  }
  if (dvar_is_integer(DVAR(0))) {
    pivot = dvar_min_int(DVAR(0))-pdata->offset+1;
    if (pivot<1 || pivot>=pdata->ntargets)
      pivot = 0;
  }
  curdom = fd_localize(wam,dvar_set(DVAR(0)));
  tvec = fd_subtract(wam, CTagToArg(X(0),6),curdom); /* prev domain \ cur domain */
  tvec = fd_localize(wam,tvec);				       /* protect from GC */
  dvar_pruning_done( DVAR(0));
  dvar_export( DVAR(0));
  fditer_init(&it, tvec);
  while (!fditer_empty(&it)) {
    int varno = GetSmall_int(fditer_next(&it))-pdata->offset+1;
    if (!bool_export_value(wam, RefAttr(varno), RefVar(varno), pdata->sense[varno]^sense))
      goto ret;
  }  
  if (pivot) {
    if (!bool_export_value(wam, RefAttr(pivot), RefVar(pivot), pdata->sense[pivot]^sense^1))
      goto ret;
  }
  ent = !!dvar_is_integer(DVAR(0));
  if (!ent) {			/* update "prev domain" - slight overkill */
    TAGGED newdom = fd_intersection_interval(wam, curdom,
					     MakeSmall(pdata->offset),
					     MakeSmall(pdata->offset+pdata->ntargets-2));
    newdom = fd_globalize(wam,fd_localize(wam,newdom),0,3); /* beware of GC */
    CTagToArg(X(0),6) = newdom;
  }
 ret:
  pdata->top = 0;
  return ent;
}

/* sense=0 for #=<, 1 for #>= */
static int bool_channel_le(Wam wam,
			   struct bool_channel_data *pdata,
			   int sense, int yoff) 
{
  int i, ent = -1;
  int start01, start1, new01, new1; /* N.B. 1-based */
  SP_integer yminl, ymaxl;
  TAGGED ymint, ymaxt, tmin, tmax;

  dvar_refresh(DVAR(0));
  new01 = start01 = GetSmall_int(CTagToArg(X(0),6));
  new1 = start1 = GetSmall_int(CTagToArg(X(0),7));

  /* prune from Xs to Y */
  for (i=start01; i<start1; i++) {
    REF_GET_BOUNDS(RefAttr(i), tmin, tmax);
    if (tmin==tmax && Tgtz(tmin)==(pdata->sense[i]^sense))
      new01 = i+1;
  }
  for (i=start1-1; i>=start01; i--) {
    REF_GET_BOUNDS(RefAttr(i), tmin, tmax);
    if (tmin==tmax && Tgtz(tmin)!=(pdata->sense[i]^sense))
      new1 = i;
  }
  if (start01<new01 && start1>new1) { /* found '0' and '1' */
    if (dvar_fix_interval_l(DVAR(0),
			    new01+pdata->offset-yoff-1,
			    new1+pdata->offset-yoff-1)<0)
      goto ret;
  } else if (start01<new01) { /* found '0' */
    if (dvar_fix_min_l(DVAR(0), new01+pdata->offset-yoff-1)<0)
      goto ret;
  } else if (start1>new1) { /* found '1' */
    if (dvar_fix_max_l(DVAR(0), new1+pdata->offset-yoff-1)<0)
      goto ret;
  }
  dvar_pruning_done( DVAR(0));
  dvar_export( DVAR(0));
  /* prune from Y to Xs */
  ymint = dvar_min_t(DVAR(0));
  ymaxt = dvar_max_t(DVAR(0));
  yminl = dvar_min_l(DVAR(0));
  ymaxl = dvar_max_l(DVAR(0));
  if (AreSmall(ymint,ymaxt) &&
      yminl >= pdata->offset-yoff &&
      ymaxl <= pdata->offset-yoff+pdata->ntargets-2) {
    new01 = (int)yminl-pdata->offset+yoff+1;
    new1 = (int)ymaxl-pdata->offset+yoff+1;
    for (i=start01; i<new01; i++)
      if (!bool_export_value(wam, RefAttr(i), RefVar(i), pdata->sense[i]^sense))
	goto ret;
    for (i=new1; i<start1; i++)
      if (!bool_export_value(wam, RefAttr(i), RefVar(i), pdata->sense[i]^sense^1))
	goto ret;
  } else if (IsSmall(ymint) && yminl >= pdata->offset-yoff) {
    new01 = new1 < yminl-pdata->offset+yoff+1 ? new1 : (int)yminl-pdata->offset+yoff+1;
    for (i=start01; i<new01; i++)
      if (!bool_export_value(wam, RefAttr(i), RefVar(i), pdata->sense[i]^sense))
	goto ret;
  } else if (IsSmall(ymaxt) && ymaxl <= pdata->offset-yoff+pdata->ntargets-2) {
    new1 = new01 > ymaxl-pdata->offset+yoff+1 ? new01 : (int)ymaxl-pdata->offset+yoff+1;
    for (i=new1; i<start1; i++)
      if (!bool_export_value(wam, RefAttr(i), RefVar(i), pdata->sense[i]^sense^1))
	goto ret;
  }
  CTagToArg(X(0),6) = MakeSmall(new01);
  CTagToArg(X(0),7) = MakeSmall(new1);
  ent = (new01==new1);
 ret:
  return ent;
}


/*
  FlatZinc accelerator:
  '$fd_bool_channel'(+State0, +State, -Actions).
  State = state(XAs,XSs,Y-YA,Offet,YSet,Handle,Stamp)
*/
void SPCDECL
prolog_fd_bool_channel(Wam wam,
		       SP_term_ref State0,
		       SP_term_ref State,
		       SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, tvec, svec, susp;
  SP_integer state_stamp, total_size;
  int i, j, ntargets;
  struct bool_channel_data *pdata;
  SP_BOOL committed;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


  /*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct bool_channel_data,handle);
    ntargets = pdata->ntargets;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Xs */
    ntargets = fd_list_length(tvec)+1;
    total_size = ntargets*(sizeof(struct dvar)+sizeof(int)+1);
    pdata = Palloc(struct bool_channel_data, total_size, handle);
    pdata->ntargets = ntargets;
    pdata->destructor = bool_channel_destructor;
    pdata->daemon = bool_channel_daemon;
    pdata->nrefs = ntargets<<1;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(ntargets<<1);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += ntargets*sizeof(struct dvar);
    pdata->stack = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->sense = ptr;
    ptr += ntargets;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    pdata->stamp = 0;
    DerefArg(tvec,X(0),1);		/* get Xs */
    DerefArg(svec,X(0),2);		/* get Ss */
    for (i=1; i<ntargets; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      DerefCar(telt,svec);
      DerefCdr(svec,svec);
      pdata->sense[i] = (char)GetSmall(telt);
    }
    DerefArg(tvec,X(0),3);		/* get Y */
    fd_get_var_and_attr(tvec,RefAttr(0));
    DerefArg(tvec,X(0),4);		/* get Code */
    pdata->code = GetSmall_int(tvec);
    DerefArg(tvec,X(0),5);		/* get Offset */
    pdata->offset = GetSmall_int(tvec);
    susp = pdata->code & 2 ? functor_dom1 : fd.functor_minmax;
    for (i=0, j=0; i<ntargets; i++) {
      dvar_init(DVAR(i), RefAttr(i), RefVar(i));
      dvar_attach_daemon(wam, DVAR(i), pdata, X(1), susp); /* [MC] 4.2.3: can GC */
      if (i>0 && pdata->code & 2)
	pdata->stack[j++] = i;
    }
    pdata->top = j;
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  pdata->stamp = state_stamp+1;
  switch (pdata->code) {
  case 0:
    ent = bool_channel_le(wam, pdata,0,0); /* #=< */
    break;
  case 1:
    ent = bool_channel_le(wam, pdata,1,1); /* #>= */
    break;
  case 2:
    ent = bool_channel_eq(wam, pdata,0); /* #= */
    break;
  case 3:
    ent = bool_channel_eq(wam, pdata,1); /* #\= */
    break;
  case 4:
    ent = bool_channel_le(wam, pdata,0,1); /* #< */
    break;
  case 5:
    ent = bool_channel_le(wam, pdata,1,0); /* #> */
    break;
  }
  if (ent==1) {
    Pfree;
  }
  dvar_export_done(wam, Actions, ent);
}

struct bool_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  SP_globref refbase;
  int ntargets;
  int nonground;
  int ac;
  int xone;
  int yzero;
  int nrefs;
  Dvar dvar;
  int *target;
  int *tloc;
  char *sense;
};

static void SPCDECL bool_destructor(void *pdata_v)
{
  struct bool_data *pdata = (struct bool_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

static DAEMON_RC SPCDECL 
bool_or_daemon(Wam wam,
	       void *vdata,
	       SP_globref attr_ref,
	       TAGGED *global)
{
  struct bool_data *pdata = (struct bool_data *)vdata;
  int varno = (int)((attr_ref - pdata->refbase)>>1);
  int loc, swap;
  TAGGED tmin, tmax;
  SP_BOOL buried;
  TAGGED tstate = fd_daemon_copy_state(wam, global,&buried);
  int nonground = pdata->ntargets - GetSmall_int(CTagToArg(tstate,3));
  DAEMON_RC rc = DAEMON_FIX;
    
  if (pdata->nonground < nonground) { /* we have backtracked */
    pdata->xone = pdata->yzero = FALSE;
  }
  pdata->nonground = nonground;
  loc = pdata->tloc[varno];
  swap = pdata->target[--pdata->nonground];
  pdata->target[loc] = swap;
  pdata->tloc[swap] = loc;
  pdata->target[pdata->nonground] = varno;
  pdata->tloc[varno] = pdata->nonground;

  REF_GET_BOUNDS(attr_ref, tmin, tmax);
  (void)tmax;
  if (varno==0 && (Teqz(tmin) ^ pdata->sense[varno])) {
    pdata->yzero = TRUE;
  } else if (varno>0 && (Tgtz(tmin) ^ pdata->sense[varno])) {
    pdata->xone = TRUE;
  }
  CTagToArg(tstate,3) = MakeSmall(pdata->ntargets-pdata->nonground); /* update NGround */
  if (pdata->nonground<=1 || pdata->yzero || pdata->xone)
    rc = DAEMON_NOFIX;
  return rc;
}


/*
  FlatZinc accelerator:
  '$fd_bool_or'(+State0, +State, -Actions).
  State = state(Xs,Ss,NGround,Handle,Stamp)
*/
void SPCDECL
prolog_fd_bool_or(Wam wam,
		  SP_term_ref State0,
		  SP_term_ref State,
		  SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, tvec, svec;
  SP_integer total_size, state_stamp;
  int i, j, k, ntargets;
  struct bool_data *pdata;
  SP_BOOL committed;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


  /*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct bool_data,handle);
    ntargets = pdata->ntargets;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Xs */
    ntargets = fd_list_length(tvec);
    total_size = ntargets*(sizeof(struct dvar)+1) + 2*ntargets*sizeof(int);
    pdata = Palloc(struct bool_data, total_size, handle);
    pdata->ntargets = ntargets;
    pdata->destructor = bool_destructor;
    pdata->daemon = bool_or_daemon;
    pdata->nrefs = ntargets<<1;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(ntargets<<1);
    ptr = (char *)(pdata+1);
    pdata->target = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->tloc = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->dvar = (Dvar)ptr;
    ptr += ntargets*sizeof(struct dvar);
    pdata->sense = ptr;
    ptr += ntargets;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    DerefArg(tvec,X(0),1);		/* get Xs */
    DerefArg(svec,X(0),2);		/* get Ss */
    for (i=0; i<ntargets; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      DerefCar(telt,svec);
      DerefCdr(svec,svec);
      pdata->sense[i] = (char)GetSmall(telt);
    }
    pdata->yzero = pdata->xone = FALSE;
    for (i=0, j=0, k=ntargets; i<ntargets; i++) {
      dvar_init(DVAR(i), RefAttr(i), RefVar(i));
      dvar_attach_daemon(wam, DVAR(i), pdata, X(1), fd.functor_minmax); /* [MC] 4.2.3: can GC */
      if (dvar_is_integer(DVAR(i))) {
	pdata->tloc[i] = --k;
	pdata->target[k] = i;
	if (i==0 && ((dvar_min_l(DVAR(i))==0) ^ pdata->sense[i])) {
	  pdata->yzero = TRUE;
	} else if (i>0 && ((dvar_min_l(DVAR(i))>0) ^ pdata->sense[i])) {
	  pdata->xone = TRUE;
	}
      } else {
	pdata->tloc[i] = j;
	pdata->target[j++] = i;
      }
    }
    pdata->nonground = j;
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  if (pdata->xone) {
    if (pdata->yzero) {
      goto ret;
    } else {
      if (!bool_export_value(wam, RefAttr(0), RefVar(0), pdata->sense[0]^1)) /* may be 1 already */
	goto ret;
      ent = 1;
    }
  } else if (pdata->yzero) {
    for (i=0; i<pdata->nonground; i++) {
      int elt = pdata->target[i];
      if (!bool_export_value(wam, RefAttr(elt), RefVar(elt), pdata->sense[elt]))
	goto ret;
    }
    ent = 1;
  } else if (pdata->nonground==0) {
    TAGGED tmin, tmax;
    REF_GET_BOUNDS(RefAttr(0), tmin, tmax);
    (void)tmax;
    if (Tgtz(tmin) ^ pdata->sense[0])
      goto ret;
    ent = 1;
  } else if (pdata->nonground==1) {
    int elt = pdata->target[0];
    if (!bool_export_value(wam, RefAttr(elt), RefVar(elt), (elt>0)^pdata->sense[elt]))
      goto ret;
    ent = 1;
  } else {
    ent = 0;
  }
 ret:
  pdata->xone = pdata->yzero = FALSE;
  CTagToArg(X(0),3) = MakeSmall(pdata->ntargets-pdata->nonground); /* update NGround */
  if (ent==1) {
    Pfree;
  }
  dvar_export_done(wam, Actions, ent);
}

static DAEMON_RC SPCDECL 
bool_xor_daemon(Wam wam,
		void *vdata,
		SP_globref attr_ref,
		TAGGED *global)
{
  struct bool_data *pdata = (struct bool_data *)vdata;
  int varno = (int)((attr_ref - pdata->refbase)>>1);
  int loc, swap;
  TAGGED tmin, tmax;
  SP_BOOL buried;
  TAGGED tstate = fd_daemon_copy_state(wam, global,&buried);
  DAEMON_RC rc = DAEMON_FIX;
  
  pdata->nonground = pdata->ntargets - GetSmall_int(CTagToArg(tstate,3));
  pdata->ac = GetSmall_int(CTagToArg(tstate,4));
  loc = pdata->tloc[varno];
  swap = pdata->target[--pdata->nonground];
  pdata->target[loc] = swap;
  pdata->tloc[swap] = loc;
  pdata->target[pdata->nonground] = varno;
  pdata->tloc[varno] = pdata->nonground;

  REF_GET_BOUNDS(attr_ref, tmin, tmax);
  (void)tmax;
  pdata->ac += (Tgtz(tmin) ^ pdata->sense[varno]);
  CTagToArg(tstate,3) = MakeSmall(pdata->ntargets-pdata->nonground); /* update NGround */
  CTagToArg(tstate,4) = MakeSmall(pdata->ac); /* update NGround */
  if (pdata->nonground<=1)
    rc = DAEMON_NOFIX;
  return rc;
}


/*
  FlatZinc accelerator:
  '$fd_bool_xor'(+State0, +State, -Actions).
  State = state(Xs,Ss,NGround,Ac,Handle,Stamp)
*/
void SPCDECL
prolog_fd_bool_xor(Wam wam,
		   SP_term_ref State0,
		   SP_term_ref State,
		   SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, tvec, svec;
  SP_integer total_size, state_stamp;
  int i, j, k, ntargets;
  struct bool_data *pdata;
  SP_BOOL committed;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


  /*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct bool_data,handle);
    ntargets = pdata->ntargets;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Xs */
    ntargets = fd_list_length(tvec);
    total_size = ntargets*(sizeof(struct dvar)+1) + 2*ntargets*sizeof(int);
    pdata = Palloc(struct bool_data, total_size, handle);
    pdata->ntargets = ntargets;
    pdata->destructor = bool_destructor;
    pdata->daemon = bool_xor_daemon;
    pdata->nrefs = ntargets<<1;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(ntargets<<1);
    ptr = (char *)(pdata+1);
    pdata->target = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->tloc = (int *)ptr;
    ptr += ntargets*sizeof(int);
    pdata->dvar = (Dvar)ptr;
    ptr += ntargets*sizeof(struct dvar);
    pdata->sense = ptr;
    ptr += ntargets;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    DerefArg(tvec,X(0),1);		/* get Xs */
    DerefArg(svec,X(0),2);		/* get Ss */
    for (i=0; i<ntargets; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      DerefCar(telt,svec);
      DerefCdr(svec,svec);
      pdata->sense[i] = (char)GetSmall(telt);
    }
    pdata->ac = 0;
    for (i=0, j=0, k=ntargets; i<ntargets; i++) {
      dvar_init(DVAR(i), RefAttr(i), RefVar(i));
      dvar_attach_daemon(wam, DVAR(i), pdata, X(1), fd.functor_minmax); /* [MC] 4.2.3: can GC */
      if (dvar_is_integer(DVAR(i))) {
	pdata->tloc[i] = --k;
	pdata->target[k] = i;
	pdata->ac += (dvar_min_int(DVAR(i)) ^ pdata->sense[i]);
      } else {
	pdata->tloc[i] = j;
	pdata->target[j++] = i;
      }
    }
    pdata->nonground = j;
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  if (pdata->nonground==0) {
    if (pdata->ac & 1)
      goto ret;
    ent = 1;
  } else if (pdata->nonground==1) {
    int elt = pdata->target[0];
    if (!bool_export_value(wam, RefAttr(elt), RefVar(elt), (pdata->ac&1)^pdata->sense[elt]))
      goto ret;
    ent = 1;
  } else {
    ent = 0;
  }
  CTagToArg(X(0),3) = MakeSmall(pdata->ntargets-pdata->nonground); /* update NGround */
  CTagToArg(X(0),4) = MakeSmall(pdata->ac); /* update Ac */
  if (ent==1) {
    Pfree;
  }
 ret:
  dvar_export_done(wam, Actions, ent);
}

static SP_BOOL propagate_binary_clause(Wam wam, 
				       TAGGED tval,
				       TAGGED otherpair,
				       TAGGED entflag) 
{
  TAGGED attr = CTagToArg(otherpair,2); /* Attribute */

  DerefAttribute(attr,attr);
  if (DomainSize(attr)==TaggedOne) { /* already fixed */
    if (DomainMin(attr)==tval)
      goto entailed;
    else
      return FALSE;
  } else {
    int why;
    X(EVAL_ARITY) = CTagToArg(otherpair,2);   /* for fd_tell_value */
    X(EVAL_ARITY+1) = CTagToArg(otherpair,1); /* for fd_tell_value */
    why = fd_tell_value(wam,tval); /* can GC */
    fd_told(wam, why, tval);
  }

 entailed:
  BindHVA(entflag,TaggedOne);
  fd.resumptions++;
  fd.entailments++;
  return TRUE;  
}

static SP_BOOL propagate_nary_clause(Wam wam, 
				     TAGGED watching,
				     TAGGED entflag,
				     TAGGED clause)
{
  TAGGED watch1 = CTagToArg(clause,1);
  TAGGED watch2 = CTagToArg(clause,2);
  TAGGED poslits = CTagToArg(clause,5);
  TAGGED neglits = CTagToArg(clause,6);
  TAGGED var=0, tval, attr;
  int no=0, free=0, why, wix, other;

  if (watching==watch1) {
    wix = 1;
    other = GetSmall_int0(watch2);
  } else if (watching==watch2) {
    wix = 2;
    other = GetSmall_int0(watch1);
  } else {
    return TRUE;
  }
  tval = other>0 ? TaggedOne : TaggedZero;
  while (TagIsLST(poslits) && !free) {
    TAGGED vapair = CTagToCar(poslits);
    poslits = CTagToCdr(poslits);
    no++;
    attr = CTagToArg(vapair,2); /* Attribute */
    DerefAttribute(attr,attr);
    if (Teqz(DomainMin(attr))) {
      if (Tnez(DomainMax(attr))) { /* still free */
	if (no==other)
	  var = vapair;
	else
	  free = no;
      }
    } else {			/* fixed 1 */
      goto entailed;
    }
  }
  no = 0;
  while (TagIsLST(neglits) && !free) {
    TAGGED vapair = CTagToCar(neglits);
    neglits = CTagToCdr(neglits);
    no--;
    attr = CTagToArg(vapair,2); /* Attribute */
    DerefAttribute(attr,attr);
    if (Teqz(DomainMin(attr))) {
      if (Tnez(DomainMax(attr))) { /* still free */
	if (no==other)
	  var = vapair;
	else
	  free = no;
      } else {		/* fixed 0 */
	goto entailed;
      }
    }
  }
  if (free) {
    CTagToArg(clause,wix) = MakeSmall(free); /* update watch */
  } else if (var!=0) {
    X(EVAL_ARITY) = CTagToArg(var,2);   /* for fd_tell_value */
    X(EVAL_ARITY+1) = CTagToArg(var,1); /* for fd_tell_value */
    why = fd_tell_value(wam,tval); /* can GC */
    fd_told(wam, why, tval);
    goto entailed;
  } else {
    return FALSE;
  }
  return TRUE;

 entailed:
  BindHVA(entflag,TaggedOne);
  fd.resumptions++;
  fd.entailments++;
  return TRUE;  
}

SP_BOOL propagate_disequations(Wam wam, TAGGED list)
{
  DECL_UPDATE_MUTABLE;
  X(0) = list;			/* X(0) is preserved over any GC here */

  while (TagIsLST(X(0))) {
    TAGGED disequation = CTagToCar(X(0));
    TAGGED countm = /* CTagToArg(disequation,1) */ disequation+WD(5);
    TAGGED count = RefMutable(countm);

    FD_UPDATE_MUTABLE(count-IStep(1),countm);
    if (count == MakeSmall(2)) {
      TAGGED coeffs = CTagToArg(disequation,2);
      TAGGED vapairs = CTagToArg(disequation,3);
      TAGGED rhs = GetSmall(CTagToArg(disequation,4));
      TAGGED c=0, var=0;

      while (TagIsLST(coeffs)) {
	TAGGED coeff, vapair, attr;
	coeff = CTagToCar(coeffs);
	coeffs = CTagToCdr(coeffs);
	vapair = CTagToCar(vapairs);
	vapairs = CTagToCdr(vapairs);
	attr = CTagToArg(vapair,2); /* Attribute */
	DerefAttribute(attr,attr);
	if (DomainSize(attr)==MakeSmall(2)) { /* the single free var */
	  var = vapair;
	  c = coeff;
	} else if (Tgtz(DomainMin(attr))) {
	  rhs -= GetSmall(coeff);
	}
      }
  
      if (var!=0) {
	TAGGED tval = (rhs==0 ? TaggedOne : rhs==GetSmall(c) ? TaggedZero : 0);
	if (tval) {
	  int why;
	  X(EVAL_ARITY) = CTagToArg(var,2);   /* for fd_tell_value */
	  X(EVAL_ARITY+1) = CTagToArg(var,1); /* for fd_tell_value */
	  why = fd_tell_value(wam,tval); /* can GC */
	  fd_told(wam, why, tval);
	}
      } else if (rhs==0) {
	return FALSE;
      }
      fd.resumptions++;
      fd.entailments++;
    }
    X(0) = CTagToCdr(X(0));
  }
  return TRUE;
}

SP_BOOL propagate_watchers(Wam wam, TAGGED list, TAGGED value)
{
  X(0) = list;			/* X(0) is preserved over any GC here */

  while (TagIsLST(X(0))) {
    TAGGED watcher = CTagToCar(X(0));
    TAGGED entflag = CTagToArg(watcher,4); /* assert: dereffed */
    TAGGED watching, otherpair, tag, clause;
    
    X(0) = CTagToCdr(X(0));
    if (!IsVar(entflag) || !IsVar(CTagToREF(entflag)))
      continue;
    watching = CTagToArg(watcher,1);
    if (Teqz(value) == Tltz(watching)) {
      fd.entailments++;
      BindHVA(entflag,TaggedOne);
      continue;
    }
    otherpair = CTagToArg(watcher,2);
    tag = CTagToArg(watcher,3); /* 0: nary clause, 1,2: binary clause */
    clause = CTagToArg(watcher,5);
    if (Tgtz(tag)) {
      if (!propagate_binary_clause(wam, tag & TaggedOne, otherpair, entflag))
	return FALSE;
    } else {
      if (!propagate_nary_clause(wam, watching, entflag, clause))
	return FALSE;
    }
  }
  return TRUE;
}

SP_BOOL propagate_implications(Wam wam, TAGGED list)
{
  X(0) = list;			/* X(0) is preserved over any GC here */

  while (TagIsLST(X(0))) {
    TAGGED item = CTagToCar(X(0));
    TAGGED pair = CTagToArg(item,1);
    TAGGED right = CTagToArg(item,2);
    TAGGED var = CTagToArg(pair,1);
    TAGGED attr = CTagToArg(pair,2);
    int why;
    
    X(0) = CTagToCdr(X(0));
    DerefSwitch(var, goto unbound;);
    if (var==right)
      continue;
    else
      return FALSE;
  unbound:
    DerefAttribute(attr,attr);
    if (DomainSize(attr)==TaggedOne) {
      var = DomainMin(attr);
      if (var==right)
	continue;
      else
	return FALSE;
    }
    X(EVAL_ARITY) = CTagToArg(pair,2); /* for fd_tell_value */
    X(EVAL_ARITY+1) = var;	       /* for fd_tell_value */
    why = fd_tell_value(wam,right);    /* can GC */
    fd_told(wam, why, right);
  }
  return TRUE;
}

struct int_element_data {
  void (SPCDECL *destructor)(void *);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  int n;
  int dc;
  struct dvar dv_index;
  struct dvar dv_value;
  TAGGED *v;
};

static void SPCDECL int_element_destructor(void *pdata_v)
{
  struct int_element_data *pdata = (struct int_element_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,4);
  SP_free(pdata);
}

static int
fd_int_element(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  int i, n, ent = -1;
  SP_integer total_size;
  char *ptr;
  struct int_element_data *pdata;
  SP_globref refbase;

  RefTerm(State) = fd_static_output_state(wam, &handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct int_element_data,handle);
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),3);	/* get Array */
    n = fd_list_length(tvec);	/* count terms */
    total_size = 
      (n+1)*sizeof(TAGGED);
  
    fd.gdata = pdata = Palloc(struct int_element_data, total_size, handle);
    ptr = (char *)(pdata+1);
    pdata->v = (TAGGED *)ptr;
    ptr += (n+1)*sizeof(TAGGED);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->n = n;
    pdata->refbase = refbase = SP_alloc_globrefs(4);
    pdata->destructor = int_element_destructor;
    DerefArg(telt,X(0),4);	/* get Flags */
    pdata->dc = (telt & IStep(2));
    DerefArg(telt,X(0),1);	/* get Index */
    fd_get_var_and_attr(telt,refbase);
    dvar_init(&pdata->dv_index, refbase, refbase+1);    
    DerefArg(telt,X(0),2);	/* get Value */
    fd_get_var_and_attr(telt,refbase+2);
    dvar_init(&pdata->dv_value, refbase+2, refbase+3);    
    for (i=1; i<=n; i++) {
      DerefCar(pdata->v[i],tvec);
      DerefCdr(tvec,tvec);
    }
  }

  /* RESUME */
  dvar_refresh(&pdata->dv_index);
  dvar_refresh(&pdata->dv_value);

  if (dvar_is_integer(&pdata->dv_index)) {
    if (dvar_fix_value_t(&pdata->dv_value, pdata->v[dvar_min_l(&pdata->dv_index)])<0)
      return -1;
    ent = 1;
  } else if (dvar_is_integer(&pdata->dv_value)) {
    TAGGED key = dvar_min_t(&pdata->dv_value);
    SP_integer imin, imax;
    FDCONS icons;
    DVITER it;
    
    fdcons_init(&icons);
    dviter_init(&it, &pdata->dv_index);
    while (!dviter_empty(&it)) {
      dviter_next_interval_l(&it, &imin, &imax);
      for (i=(int)imin; i<=(int)imax; i++)
	if (pdata->v[i] == key)
	  fdcons_add(wam, &icons, MakeSmall(i));
    }
    if (dvar_fix_set(&pdata->dv_index, fdcons_set(&icons))<0)
      return -1;
    ent = 1;
  } else if (pdata->dc) {			/* general DC case */
    DVITER it;
    FDCONS icons, vcons;
    TAGGED *tarray;
    SP_integer imin, imax;
    SP_BOOL sorted = TRUE;
    int nt = 0;
    int nv = (int)dvar_value_count(&pdata->dv_index);
    
    NumstackAlloc(nv,tarray);
    fdcons_init(&icons);
    dviter_init(&it, &pdata->dv_index);
    while (!dviter_empty(&it)) {
      dviter_next_interval_l(&it, &imin, &imax);
      for (i=(int)imin; i<=(int)imax; i++) {
	TAGGED v = pdata->v[i];
	if (!dvar_contains_value_t(&pdata->dv_value,v)) {
	  fdcons_add(wam, &icons, MakeSmall(i));
	} else if (nt==0) {
	  tarray[nt++] = v;
	} else if (v<tarray[nt-1]) {
	  sorted = FALSE;
	  tarray[nt++] = v;
	} else if (v>tarray[nt-1]) {
	  tarray[nt++] = v;
	}
      }
    }
    
    if (!sorted)
      fd_qsort_asc_tagged(wam,tarray,nt);
    fdcons_init(&vcons);
    for (i=0; i<nt; i++)
      if (i==0 || tarray[i-1] != tarray[i])
	fdcons_add(wam, &vcons, tarray[i]);
    if (nt==0 ||
	dvar_prune_set(&pdata->dv_index, fdcons_set(&icons))<0 ||
	dvar_fix_set(&pdata->dv_value, fdcons_set(&vcons))<0)
      return -1;
    else
      ent = (dvar_is_integer(&pdata->dv_value) ? 1 : 0);
  } else {			/* general BC case */
    DVITER it;
    FDCONS icons;
    TAGGED vmin = dvar_max_t(&pdata->dv_value);
    TAGGED vmax = dvar_min_t(&pdata->dv_value);
    SP_integer imin, imax;
    
    fdcons_init(&icons);
    dviter_init(&it, &pdata->dv_index);
    while (!dviter_empty(&it)) {
      dviter_next_interval_l(&it, &imin, &imax);
      for (i=(int)imin; i<=(int)imax; i++) {
	TAGGED v = pdata->v[i];
	if (!dvar_contains_value_t(&pdata->dv_value,v)) {
	  fdcons_add(wam, &icons, MakeSmall(i));
	} else {
	  vmin = Tlt(vmin,v) ? vmin : v;
	  vmax = Tgt(vmax,v) ? vmax : v;
	}
      }
    }
    
    if (Tgt(vmin,vmax) ||
	dvar_prune_set(&pdata->dv_index, fdcons_set(&icons))<0 ||
	dvar_fix_interval_t(&pdata->dv_value, vmin, vmax)<0)
      return -1;
    else
      ent = (dvar_is_integer(&pdata->dv_value) ? 1 : 0);
  }
  
  dvar_pruning_done(&pdata->dv_index);
  dvar_pruning_done(&pdata->dv_value);
  dvar_export(&pdata->dv_index);
  dvar_export(&pdata->dv_value);
    
  return ent;
}

struct var_element_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;
  int n;
  struct dvar dv_index;
  struct dvar dv_value;
  struct dvar *dvar;
};

static void SPCDECL var_element_destructor(void *pdata_v)
{
  struct var_element_data *pdata = (struct var_element_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,2*pdata->n+4);
  SP_free(pdata);
}

/* Daemon for element/3: effectively disable Xi if i is not in the index domain */
static DAEMON_RC SPCDECL 
var_element_daemon(Wam wam,
		   void *vdata,
		   SP_globref attr_ref,
		   TAGGED *global)
{
  struct var_element_data *pdata = (struct var_element_data *)vdata;
  int eltno = (int)(((attr_ref - pdata->refbase)>>1)-1);
  DAEMON_RC rc = DAEMON_FIX;
  
  (void)wam;
  (void)global;
  dvar_refresh(&pdata->dv_index);
  if (dvar_contains_value_l(&pdata->dv_index,eltno))
    rc = DAEMON_NOFIX;
  return rc;
}

static int
var_element_eq(Wam wam, Dvar x, Dvar y)
{
  int rc1 = dvar_fix_set(x, dvar_set(y));
  int rc2 = dvar_fix_set(y, dvar_set(x));
  
  return (rc1>=0 && rc2>=0);
}


static int
fd_var_element(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  int i, n, ent = -1;
  SP_integer total_size;
  char *ptr;
  struct var_element_data *pdata;
  SP_globref refbase, r;
  Dvar pivot = 0;

  RefTerm(State) = fd_static_output_state(wam, &handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct var_element_data,handle);
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),3);	/* get Array */
    n = fd_list_length(tvec);	/* count terms */
    total_size = 
      (n+1)*sizeof(struct dvar);
  
    fd.gdata = pdata = Palloc(struct var_element_data, total_size, handle);
    ptr = (char *)(pdata+1);
    pdata->dvar = (struct dvar *)ptr;
    ptr += (n+1)*sizeof(struct dvar);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->n = n;
    pdata->refbase = refbase = SP_alloc_globrefs(2*n+4);
    pdata->destructor = var_element_destructor;
    pdata->daemon = var_element_daemon;
    DerefArg(telt,X(0),1);	/* get Index */
    fd_get_var_and_attr(telt,refbase);
    dvar_init(&pdata->dv_index, refbase, refbase+1);    
    DerefArg(telt,X(0),2);	/* get Value */
    fd_get_var_and_attr(telt,refbase+2);
    dvar_init(&pdata->dv_value, refbase+2, refbase+3);
    r = refbase+4;
    for (i=1; i<=n; i++, r+=2) {
      struct dvar *dvar = pdata->dvar + i;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,r);
      dvar_init(dvar, r, r+1);
    }
    for (i=1; i<=n; i++, r+=2)
      dvar_attach_daemon(wam, pdata->dvar+i, pdata, X(1), functor_dom1); /* [MC] 4.1: wakeup on dom */
  }

  /* RESUME */
  dvar_refresh(&pdata->dv_index);
  dvar_refresh(&pdata->dv_value);
  
  if (dvar_is_integer(&pdata->dv_index)) {
    pivot = pdata->dvar + dvar_min_l(&pdata->dv_index);
    dvar_refresh(pivot);
    if (!var_element_eq(wam, pivot, &pdata->dv_value))
      return -1;
    ent = dvar_is_integer(pivot) ? 1 : 0;
  } else {			/* general case */
    DVITER it;
    FDCONS icons;
    SP_integer imin, imax;
    TAGGED vset = dvar_set(&pdata->dv_value);
    TAGGED tmin = dvar_max_t(&pdata->dv_value);
    TAGGED tmax = dvar_min_t(&pdata->dv_value);
    
    fdcons_init(&icons);
    dviter_init(&it, &pdata->dv_index);
    while (!dviter_empty(&it)) {
      dviter_next_interval_l(&it, &imin, &imax);
      for (i=(int)imin; i<=(int)imax; i++) {
	Dvar dv = pdata->dvar+i;
	dvar_refresh(dv);
	if (dvar_compare_set(dv,vset)==FDI_DISJOINT) {
	  fdcons_add(wam, &icons, MakeSmall(i));
	} else {
	  TAGGED dvmin = dvar_min_t(dv);
	  TAGGED dvmax = dvar_max_t(dv);
	  tmin = FDlt(tmin,dvmin) ? tmin : dvmin;
	  tmax = FDgt(tmax,dvmax) ? tmax : dvmax;
	}
      }
    }
    if (FDgt(tmin,tmax) ||
	dvar_prune_set(&pdata->dv_index, fdcons_set(&icons))<0 ||
	dvar_fix_interval_t(&pdata->dv_value, tmin, tmax)<0) {
      return -1;
    } else if (dvar_is_integer(&pdata->dv_index)) {
      pivot = pdata->dvar + dvar_min_l(&pdata->dv_index);
      if (!var_element_eq(wam, pivot, &pdata->dv_value))
	return -1;
      ent = dvar_is_integer(pivot) ? 1 : 0;
    } else {
      ent = 0;
    }
  }

  dvar_pruning_done(&pdata->dv_index);
  dvar_pruning_done(&pdata->dv_value);
  if (pivot)
    dvar_pruning_done(pivot);
  dvar_export(&pdata->dv_index);
  dvar_export(&pdata->dv_value);
  if (pivot)
    dvar_export(pivot);
    
  return ent;
}

/*
  '$fd_element'(+State0, +State, -Actions).
  State = state(Index,Value,Array,Flags,Handle,Stamp)
  !(Flags&1) -> Array is a list of Var-Mutable
  (Flags&1) -> Array is a list of integers
  !(Flags&2) -> don't maintain DC
  (Flags&2) -> maintain DC
*/
void SPCDECL
prolog_fd_element(Wam wam,
		  SP_term_ref State0,
		  SP_term_ref State,
		  SP_term_ref Actions)
{
  int ent;
  TAGGED telt;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  DerefArg(telt,X(0),4);

/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  if (Teqz(telt))
    ent = fd_var_element(wam, State);
  else
    ent = fd_int_element(wam, State);
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}
