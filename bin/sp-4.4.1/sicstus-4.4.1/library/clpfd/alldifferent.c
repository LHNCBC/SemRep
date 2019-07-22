/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

struct alldiff_data {
  void (SPCDECL *destructor)(void *);
  SPEnv *spenv;
  SP_globref refbase;
  SP_integer stamp;			/* increases up to backtracking */
  int nvars;			/* _original_ #terms */
  int *target;
  TAGGED *val;			/* volatile */
  Dvar dvar;
  /* for circuit */
  int *head;
  int *tail;
  int *head_loc;
  int *tail_loc;
  int ntargets;
};

#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define SV(T) (pdata->target[T])
#define VAL(T) (pdata->val[T])
#define DVAR(T) (pdata->dvar+(T))

static void SPCDECL alldiff_destructor(void *pdata_v)
{
  struct alldiff_data *pdata = (struct alldiff_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata);
}

/*
  '$fd_all_different'(+State0, +State, -Actions).
  State = state(Vec,NDone,Handle,Stamp)
*/
void SPCDECL
prolog_fd_all_different(Wam wam,
			SP_term_ref State0,
			SP_term_ref State,
			SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec, min=0;
  TAGGED infeasible=0;
  SP_integer state_stamp, total_size;
  int i, ntargets, nint, nvars=0;
  struct alldiff_data *pdata;
  SP_BOOL committed;
  int elt;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct alldiff_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Vec */
    nvars = fd_list_length(tvec);	/* count terms, moving ground terms to RHS */
    if (nvars==0) {
      ent = 1;
      goto ret1;
    }
    total_size = nvars*(sizeof(int)+sizeof(struct dvar));
    pdata = Palloc(struct alldiff_data, total_size, handle);
    pdata->destructor = (alldiff_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->nvars = nvars;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->target = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);

    DerefArg(tvec,X(0),1);		/* get Vec */
    for (elt=0; elt<nvars; elt++) {
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(elt));
      dvar_init(DVAR(elt), RefAttr(elt), RefVar(elt));
      SV(elt) = elt;
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  pdata->val = Malloc(nvars,TAGGED);
  DerefArg(telt,X(0),2);
  ntargets = nvars-GetSmall_int(telt);
  pdata->stamp = state_stamp+1;
  for (i=0; i<ntargets; i++) {
    elt = SV(i);
    dvar_refresh(DVAR(elt));
  }
  {
    int inf = 0;
    int sup = ntargets-1;
    int held = SV(sup);		/* sup is the hole */
    Dvar dv;
    
    elt = SV(inf);
    while (inf<=sup) {
      dv = DVAR(elt);
      if (!dvar_is_integer(dv))
	goto nonground;
      VAL(sup) = dvar_min_t(dv);
      SV(sup) = elt;
      sup--;
      elt = (inf>=sup ? held : SV(sup));
      continue;
    nonground:
      SV(inf) = elt;
      inf++;
      elt = (inf>=sup ? held : SV(inf));
    }
    nint = ntargets-inf;
    if (nint==1) {
      min = VAL(inf);
    } else if (nint>1) {
      FDCONS cons;
      
      fd_qsort_asc_tagged(wam, &VAL(inf),nint);
      fdcons_init(&cons);
      min = VAL(inf);
      fdcons_add(wam, &cons,min);
      for (i=inf+1; i<ntargets; i++) {
	if (min==VAL(i))
	  goto ret;
	min = VAL(i);
	fdcons_add(wam, &cons,min);
      }
      infeasible = fdcons_set(&cons);
    }
    if (nint>0) {
      CTagToArg(X(0),2) = MakeSmall(nvars-inf);
      for (i=0; i<inf; i++) {
	Dvar dv = DVAR(SV(i));
	if ((nint==1 ? dvar_prune_interval_t(dv, min,min) : dvar_prune_set(dv, infeasible)) < 0)
	  goto ret;
	dvar_pruning_done(dv);
      }
      for (i=0; i<inf; i++) {
	Dvar dv = DVAR(SV(i));
	dvar_export(dv);
      }
    }
    ent = !inf;
  }
 ret:
  SP_free(pdata->val);
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(wam,Actions, ent);
}

/*
  '$fd_pairing'(+State0, +State, -Actions).
  State = state(XVec,YVec,NDone,Handle,Stamp)
*/
void SPCDECL
prolog_fd_pairing(Wam wam,
			 SP_term_ref State0,
			 SP_term_ref State,
			 SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec, infeasible, min;
  SP_integer state_stamp, total_size;
  int i, k, ntargets, nxs, nvars=0; /* NB. nvars is _total_ #vars */
  struct alldiff_data *pdata;
  SP_BOOL committed;
  int elt, ok;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct alldiff_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Vec */
    nvars = fd_list_length(tvec)<<1; /* count terms, moving ground terms to RHS */
    if (nvars==0) {
      ent = 1;
      goto ret1;
    }
    total_size = nvars*(sizeof(int)+sizeof(struct dvar));
    pdata = Palloc(struct alldiff_data, total_size, handle);
    pdata->destructor = (alldiff_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->nvars = nvars;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->target = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    elt = 0;
    for (i=0; i<2; i++) {
      DerefArg(tvec,X(0),i+1);		/* get XVec */
      while (TagIsLST(tvec)) {
	DerefCar(telt,tvec);
	DerefCdr(tvec,tvec);
	fd_get_var_and_attr(telt,RefAttr(elt));
	dvar_init(DVAR(elt), RefAttr(elt), RefVar(elt));
	SV(elt) = elt;
	elt++;
      }
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  nxs = nvars>>1;
  DerefArg(telt,X(0),3);
  ntargets = nvars-GetSmall_int(telt);
  pdata->val = Malloc(nvars,TAGGED);
  pdata->stamp = state_stamp+1;
  for (i=0; i<ntargets; i++) {
    elt = SV(i);
    dvar_refresh(DVAR(elt));
  }
  {
    int inf = 0;
    int sup = ntargets-1;
    int held = SV(sup);		/* sup is the hole */
    Dvar dv;
    TAGGED val;
    int top[2];

    top[0] = 0;
    top[1] = nxs;
    ok = TRUE;
    elt = SV(inf);
    while (inf<=sup) {
      dv = DVAR(elt);
      if (!ok || !dvar_is_integer_first(dv))
	goto nonground;
      val = dvar_min_t(dv);
      if (elt<nxs) {
	VAL(top[0]++) = val;
	if (dvar_fix_value_l(DVAR(GetSmall(val)+nxs-1),elt+1) <0)
	  ok = FALSE;
      } else {
	VAL(top[1]++) = val;
	if (dvar_fix_value_l(DVAR(GetSmall(val)-1),elt-nxs+1) <0)
	  ok = FALSE;
      }
      SV(sup) = elt;
      sup--;
      elt = (inf>=sup ? held : SV(sup));
      continue;
    nonground:
      SV(inf) = elt;
      inf++;
      elt = (inf>=sup ? held : SV(inf));
    }
    if (!ok)
      goto ret;
    for (k=0; k<2; k++) {
      int bos = k*nxs;
      int nvals = top[k]-bos;
      if (nvals>0) {	/* typically, nvals is a very small integer */
	if (nvals==1) {
	  min = VAL(bos);
	  infeasible = fd_interval(wam,min,min);
	} else {
	  FDCONS cons;
	
	  fd_qsort_asc_tagged(wam, &VAL(bos),nvals);
	  fdcons_init(&cons);
	  min = VAL(bos);
	  fdcons_add(wam, &cons,min);
	  for (i=1; i<nvals; i++) {
	    if (min==VAL(bos+i))
	      goto ret;
	    min = VAL(bos+i);
	    fdcons_add(wam, &cons,min);
	  }
	  infeasible = fdcons_set(&cons);
	}
	for (i=0; i<inf; i++) {
	  elt = SV(i);
	  if ((elt<nxs)^(k==1))
	    if (dvar_prune_set(DVAR(elt),infeasible)<0)
	      goto ret;
	}
      }
    }
    CTagToArg(X(0),3) = MakeSmall(nvars-inf);
    for (i=0; i<inf; i++) {
      Dvar dv = DVAR(SV(i));
      dvar_pruning_done(dv);
    }
    for (i=0; i<inf; i++) {
      Dvar dv = DVAR(SV(i));
      dvar_export(dv);
    }
    ent = (inf==0);
  }
 ret:
  SP_free(pdata->val);
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(wam,Actions, ent);
}

/*
  '$fd_circuit'(+State0, +State, -Actions).
  State = state(XVec,NDone,Handle,Stamp)
*/
void SPCDECL
prolog_fd_circuit(Wam wam,
			 SP_term_ref State0,
			 SP_term_ref State,
			 SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec;
  SP_integer state_stamp;
  int i, ntargets, nvars=0; /* NB. nvars is _total_ #vars */
  size_t total_size;
  struct alldiff_data *pdata;
  SP_BOOL committed;
  int elt;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct alldiff_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Vec */
    nvars = fd_list_length(tvec);	/* count terms, moving ground terms to RHS */
    if (nvars==0) {
      ent = 1;
      goto ret1;
    }
    total_size = 
      nvars*(5*sizeof(int)+
	     sizeof(SP_integer)+
	     sizeof(struct dvar));
    pdata = Palloc(struct alldiff_data,total_size,handle);
    pdata->destructor = (alldiff_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->nvars = nvars;
    pdata->ntargets = nvars;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->val = (TAGGED *)ptr;
    ptr += nvars*sizeof(TAGGED);
    pdata->target = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->head = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->tail = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->head_loc = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->tail_loc = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    elt = 0;
    DerefArg(tvec,X(0),1);		/* get XVec */
    while (TagIsLST(tvec)) {
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(elt));
      dvar_init(DVAR(elt), RefAttr(elt), RefVar(elt));
      SV(elt) = elt;
      elt++;
    }
    for (i=nvars-1; i>=0; i--) {
      pdata->head[i] = i;
      pdata->tail[i] = i;
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  DerefArg(telt,X(0),2);
  ntargets = nvars-GetSmall_int(telt);
  for (i=pdata->ntargets; i<ntargets; i++) {
    /* untrail head/tail updates, oldest last */
    elt = SV(i);
    pdata->head[pdata->head_loc[elt]] = GetSmall_int(VAL(elt))-1;
    pdata->tail[pdata->tail_loc[elt]] = elt;
  }
  pdata->stamp = state_stamp+1;
  for (i=0; i<ntargets; i++) {
    elt = SV(i);
    dvar_refresh(DVAR(elt));
  }
  {
    int inf = 0;
    int sup = ntargets-1;
    int held = SV(sup);		/* sup is the hole */
    int val, h, t;
    SP_BOOL ok = TRUE;
    
    elt = SV(inf);
    while (inf<=sup) {
      Dvar dv = DVAR(elt);
      if (!ok || !dvar_is_integer_first(dv))
	goto nonground;
      if (sup>0) {
	val = dvar_min_int(dv)-1;
	h = pdata->head[elt];
	t = pdata->tail[val];
	pdata->tail[h] = t;
	pdata->head[t] = h;
	pdata->head_loc[elt] = t;	/* trailing */
	pdata->tail_loc[elt] = h;	/* trailing */
	VAL(elt) = dvar_min_t(dv); /* trailing */
	if ((sup>1 ? dvar_prune_value_l(DVAR(t),h+1) :
	             dvar_fix_value_l(DVAR(t),h+1))<0)
	  ok = FALSE;
      }
      SV(sup) = elt;
      sup--;
      elt = (inf>=sup ? held : SV(sup));
      continue;
    nonground:
      SV(inf) = elt;
      inf++;
      elt = (inf>=sup ? held : SV(inf));
    }
    pdata->ntargets = (sup<1 ? 1 : sup+1);
    if (!ok)
      goto ret;
    CTagToArg(X(0),2) = MakeSmall(nvars-inf);
    for (i=0; i<inf; i++) {
      Dvar dv = DVAR(SV(i));
      dvar_pruning_done(dv);
    }
    for (i=0; i<inf; i++) {
      Dvar dv = DVAR(SV(i));
      dvar_export(dv);
    }
    ent = (inf==0);
  }
 ret:
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(wam,Actions, ent);
}

