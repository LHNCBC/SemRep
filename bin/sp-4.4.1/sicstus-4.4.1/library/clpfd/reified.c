/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

static int tell_eq(Wam wam,
		   Dvar dvx, Dvar dvy,
		   int intersecting)
{
  int rc;
  int ix = !!dvar_is_integer(dvx);
  int iy = !!dvar_is_integer(dvy);
  
  switch (intersecting) {
  case FDI_DISJOINT:
    return -1;
  case FDI_EQUAL:
    return ix;
  case FDI_SUBSET:
    if (ix) {
      dvar_fix_value_t(dvy, dvar_min_t(dvx));
      return 1;
    } else {
      dvar_fix_set(dvy, dvar_set(dvx));
      return 0;
    }
  case FDI_SUPERSET:
    if (iy) {
      dvar_fix_value_t(dvx, dvar_min_t(dvy));
      return 1;
    } else {
      dvar_fix_set(dvx, dvar_set(dvy));
      return 0;
    }
  default:
    rc = dvar_fix_set(dvx, dvar_set(dvy))| dvar_fix_set(dvy, dvar_set(dvx));
    return ((rc&DV_PRUNED_VAL) ? 1 : 0);
  }
}

struct arith_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  SP_globref refbase;
  Dvar dvar;
  int nrefs;
};

static void SPCDECL arith_destructor(void *pdata_v)
{
  struct arith_data *pdata = (struct arith_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

/* '$fd_in_set_iff'(+State, -NewState, -Actions) :- X in_set Set iff B.
   State is f(X,XMut,Set,B,BMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_in_set_iff(Wam wam,
		     SP_term_ref State,
		     SP_term_ref NewState,
		     SP_term_ref Actions)
{
  TAGGED set;
  int intersecting;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  Dvar dvx, dvb;
  int ent = 0;		/* neither entailed nor dis- */

  (void)State;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(NewState) = fd_static_output_state(wam,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct arith_data,handle);
  } else {			/* build persistent state */
    pdata = Palloc(struct arith_data, 2*sizeof(struct dvar), handle);
    pdata->destructor = arith_destructor;
    pdata->nrefs = 5;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(5);
    pdata->dvar = (Dvar)(pdata+1);
    fd_get_var_and_attr(X(0),pdata->refbase); /* get X */
    fd_get_var_and_attr(X(0)+WD(3),pdata->refbase+2); /* get B */
    dvar_init(pdata->dvar  , pdata->refbase,   pdata->refbase+1);
    dvar_init(pdata->dvar+1, pdata->refbase+2, pdata->refbase+3);
    DerefArg(set,X(0),3);	/* get Set */
    RefGlob(pdata->refbase+4) = set;
  }

  /* RESUME HERE */
  dvx = pdata->dvar;
  dvb = dvx+1;
  dvar_refresh(dvx);
  dvar_refresh(dvb);
  set = RefGlob(pdata->refbase+4);
  if (set==EmptySet)
    intersecting = FDI_DISJOINT;
  else
    intersecting = dvar_compare_set(dvx,set);
  if (!dvar_is_integer(dvb)) {
    switch (intersecting) {
    case FDI_SUBSET:		/* [B=1,exit] */
    case FDI_EQUAL:		/* [B=1,exit] */
      ent = 1;
      dvar_fix_value_t(dvb, TaggedOne);
      break;
    case FDI_DISJOINT:		/* [B=0,exit] */
      ent = 1;
      dvar_fix_value_t(dvb, TaggedZero);
      break;
    }
  } else if (Teqz(dvar_min_t(dvb))) {
    if (!dvar_is_integer(dvx)) { /* [X in_set \set,exit] */
      switch (intersecting) {
      case FDI_SUBSET:
      case FDI_EQUAL:
	ent = -1; break;
      case FDI_DISJOINT:
	ent = 1; break;
      default:
	ent = 1;
	dvar_prune_set(dvx, set);
      }
    } else if (fd_member(dvar_min_t(dvx),set)) {
      ent = -1;			/* [fail] */
    } else {
      ent = 1;			/* [exit] */
    }
  } else {
    if (!dvar_is_integer(dvx)) { /* [X in_set set,exit] */
      switch (intersecting) {
      case FDI_SUBSET:
      case FDI_EQUAL:
	ent = 1; break;
      case FDI_DISJOINT:
	ent = -1; break;
      default:
	ent = 1;
	dvar_fix_set(dvx, set);
      }
    } else if (fd_member(dvar_min_t(dvx),set)) {
      ent = 1; /* [exit] */
    } else {
      ent = -1; /* [fail] */
    }
  }
  dvar_pruning_done( dvx);
  dvar_pruning_done( dvb);
  dvar_export(dvx);
  dvar_export(dvb);
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}

static DAEMON_RC SPCDECL 
eq_iff_daemon(Wam wam,
	      void *vdata,
	      SP_globref attr_ref,
	      TAGGED *global)
{
  struct arith_data *pdata = (struct arith_data *)vdata;
  Dvar dvx = pdata->dvar;

  (void)attr_ref;
  dvar_refresh(dvx+0);
  dvar_refresh(dvx+1);
  dvar_refresh(dvx+2);
  
  if (!dvar_is_integer(dvx+2)) { /* var B */
    if (dvar_is_integer(dvx+0) && dvar_is_integer(dvx+1)) { /* int X && int Y */
      return DAEMON_NOFIX;
    } else if (dvar_compare_set(dvx+0,dvar_set(dvx+1))==FDI_DISJOINT) /* X, Y disj */
      return DAEMON_NOFIX;
  } else if (dvar_min_l(dvx+2)==0) { /* B = 0 */
    if (dvar_is_integer(dvx+0) || dvar_is_integer(dvx+1)) { /* int X || int Y */
      return DAEMON_NOFIX;
    }
  } else {			/*  B = 1 */
    return DAEMON_NOFIX;
  }
  return DAEMON_FIX;
}

/* '$fd_eq_iff'(+State, -Actions) :- X#=Y iff B.
   State is f(X,XMut,Y,YMut,B,BMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_eq_iff(Wam wam,
		 SP_term_ref State,
		 SP_term_ref NewState,
		 SP_term_ref Actions)
{
  TAGGED handle;
  SP_BOOL committed, post;
  struct arith_data *pdata;
  Dvar dvx, dvy, dvb;
  int ent = 0;		/* neither entailed nor dis- */

  (void)State;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(NewState) = fd_static_output_state(wam,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    post = FALSE;
    pdata = Pdata(struct arith_data,handle);
  } else {			/* build persistent state */
    post = TRUE;
    pdata = Palloc(struct arith_data, 3*sizeof(struct dvar), handle);
    pdata->destructor = arith_destructor;
    pdata->nrefs = 6;
    pdata->daemon = eq_iff_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(6);
    pdata->dvar = (Dvar)(pdata+1);
    fd_get_var_and_attr(X(0),pdata->refbase);
    fd_get_var_and_attr(X(0)+WD(2),pdata->refbase+2);
    fd_get_var_and_attr(X(0)+WD(4),pdata->refbase+4);
    dvar_init(pdata->dvar  , pdata->refbase,   pdata->refbase+1);
    dvar_init(pdata->dvar+1, pdata->refbase+2, pdata->refbase+3);
    dvar_init(pdata->dvar+2, pdata->refbase+4, pdata->refbase+5);
  }

  /* RESUME HERE */
  dvx = pdata->dvar;
  dvy = dvx+1;
  dvb = dvx+2;
  dvar_refresh(dvx);
  dvar_refresh(dvy);
  dvar_refresh(dvb);

  if (!dvar_is_integer(dvb)) {
    if (dvar_is_integer(dvx) && dvar_is_integer(dvy)) {
      dvar_fix_value_l(dvb, (dvar_min_t(dvx)==dvar_min_t(dvy)));
      ent = 1;
    } else if (fd_compare(dvar_set(dvx),dvar_set(dvy))==FDI_DISJOINT) {
      dvar_fix_value_t(dvb, TaggedZero);
      ent = 1;
    }
  } else if (Teqz(dvar_min_t(dvb))) {
    if (dvar_is_integer(dvy)) {
      ent = (dvar_prune_value_t(dvx, dvar_min_t(dvy))>=0 ? 1 : -1);
    } else if (dvar_is_integer(dvx)) {
      ent = (dvar_prune_value_t(dvy, dvar_min_t(dvx))>=0 ? 1 : -1);
    } else if (fd_compare(dvar_set(dvx),dvar_set(dvy))==FDI_DISJOINT)
      ent = 1;
  } else {
    if (dvar_is_integer(dvy)) {
      ent = (dvar_fix_value_t(dvx, dvar_min_t(dvy))>=0 ? 1 : -1);
    } else if (dvar_is_integer(dvx)) {
      ent = (dvar_fix_value_t(dvy, dvar_min_t(dvx))>=0 ? 1 : -1);
    } else {
      ent = tell_eq(wam, dvx,dvy,fd_compare(dvar_set(dvx),dvar_set(dvy)));
    }
  }
  dvar_pruning_done( dvx);
  dvar_pruning_done( dvy);
  dvar_pruning_done( dvb);
  dvar_export(dvx);
  dvar_export(dvy);
  dvar_export(dvb);
  if (ent==0 && post) {
    if (dvar_is_integer(dvb)) {
      dvar_attach_daemon(wam, dvx, pdata, X(1), fd.functor_val);
      dvar_attach_daemon(wam, dvy, pdata, X(1), fd.functor_val);
    } else {			      
      dvar_attach_daemon(wam, dvx, pdata, X(1), functor_dom1);
      dvar_attach_daemon(wam, dvy, pdata, X(1), functor_dom1);
      dvar_attach_daemon(wam, dvb, pdata, X(1), fd.functor_val);
    }
  }
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}


/* '$fd_le_iff'(+State, -Actions) :- X #=< Y iff B.
   State is f(X,XMut,Y,YMut,B,BMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_le_iff(Wam wam,
		 SP_term_ref State,
		 SP_term_ref NewState,
		 SP_term_ref Actions)
{
  TAGGED xmin, xmax, ymin, ymax;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  Dvar dvx, dvy, dvb;
  int ent = 0;		/* neither entailed nor dis- */

  (void)State;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(NewState) = fd_static_output_state(wam,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct arith_data,handle);
  } else {			/* build persistent state */
    pdata = Palloc(struct arith_data, 3*sizeof(struct dvar), handle);
    pdata->destructor = arith_destructor;
    pdata->nrefs = 6;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(6);
    pdata->dvar = (Dvar)(pdata+1);
    fd_get_var_and_attr(X(0),pdata->refbase);
    fd_get_var_and_attr(X(0)+WD(2),pdata->refbase+2);
    fd_get_var_and_attr(X(0)+WD(4),pdata->refbase+4);
    dvar_init(pdata->dvar  , pdata->refbase,   pdata->refbase+1);
    dvar_init(pdata->dvar+1, pdata->refbase+2, pdata->refbase+3);
    dvar_init(pdata->dvar+2, pdata->refbase+4, pdata->refbase+5);
  }

  /* RESUME HERE */
  dvx = pdata->dvar;
  dvy = dvx+1;
  dvb = dvx+2;
  dvar_refresh(dvx);
  dvar_refresh(dvy);
  dvar_refresh(dvb);
  xmin = dvar_min_t(dvx);
  xmax = dvar_max_t(dvx);
  ymin = dvar_min_t(dvy);
  ymax = dvar_max_t(dvy);
  if (!dvar_is_integer(dvb)) {
    if (FDlt(ymax,xmin)) {	/* [B=0,exit] */
      ent = 1;
      dvar_fix_value_t(dvb, TaggedZero);
    } else if (!FDgt(xmax,ymin)) { /* [B=1,exit] */
      ent = 1;
      dvar_fix_value_t(dvb, TaggedOne);
    }
  } else if (Teqz(dvar_min_t(dvb))) {		/* enforce X #> Y */
    if (FDlt(ymax,xmin))
      ent = 1;
    else if (!FDgt(xmax,ymin))
      ent = -1;
    else {
      TAGGED ymin1 = FDincr(ymin);
      TAGGED xmax1 = FDdecr(xmax);
      
      if (TagIsSmall(ymin) && FDlt(xmin,ymin1))
	dvar_fix_min_t(dvx, ymin1);
      if (TagIsSmall(xmax) && FDgt(ymax,xmax1))
	dvar_fix_max_t(dvy, xmax1);
      ent = dvar_is_integer(dvx) || dvar_is_integer(dvy) || xmax==ymin1;
    }
  } else {			/* enforce X #=< Y */
    if (FDlt(ymax,xmin))
      ent = -1;
    else if (!FDgt(xmax,ymin))
      ent = 1;
    else {
      if (TagIsSmall(ymax) && FDgt(xmax,ymax))
	dvar_fix_max_t(dvx, ymax);
      if (TagIsSmall(xmin) && FDlt(ymin,xmin))
	dvar_fix_min_t(dvy, xmin);
      ent = dvar_is_integer(dvx) || dvar_is_integer(dvy) || ymax==xmin;
    }
  }
  dvar_pruning_done( dvx);
  dvar_pruning_done( dvy);
  dvar_pruning_done( dvb);
  dvar_export(dvx);
  dvar_export(dvy);
  dvar_export(dvb);
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}



/* '$fd_oneof'(+State, -Actions) :- X#=Z #\/ Y#=Z.
   State is f(X,XMut,Y,YMut,Z,ZMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_oneof(Wam wam,
		SP_term_ref State,
		SP_term_ref NewState,
		SP_term_ref Actions)
{
  int xcapz, ycapz;
  TAGGED xdom, ydom, zdom;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  Dvar dvx, dvy, dvz;
  int ent = 0;		/* neither entailed nor dis- */

  (void)State;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(NewState) = fd_static_output_state(wam,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct arith_data,handle);
  } else {			/* build persistent state */
    pdata = Palloc(struct arith_data, 3*sizeof(struct dvar), handle);
    pdata->destructor = arith_destructor;
    pdata->nrefs = 6;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(6);
    pdata->dvar = (Dvar)(pdata+1);
    fd_get_var_and_attr(X(0),pdata->refbase);
    fd_get_var_and_attr(X(0)+WD(2),pdata->refbase+2);
    fd_get_var_and_attr(X(0)+WD(4),pdata->refbase+4);
    dvar_init(pdata->dvar  , pdata->refbase,   pdata->refbase+1);
    dvar_init(pdata->dvar+1, pdata->refbase+2, pdata->refbase+3);
    dvar_init(pdata->dvar+2, pdata->refbase+4, pdata->refbase+5);
  }

  /* RESUME HERE */
  dvx = pdata->dvar;
  dvy = dvx+1;
  dvz = dvx+2;
  dvar_refresh(dvx);
  dvar_refresh(dvy);
  dvar_refresh(dvz);

  xdom = dvar_set(dvx);
  ydom = dvar_set(dvy);
  zdom = dvar_set(dvz);
  xcapz = fd_compare(xdom,zdom);
  ycapz = fd_compare(ydom,zdom);
  if (xcapz==FDI_DISJOINT)
    ent = tell_eq(wam, dvy,dvz,ycapz);
  else if (ycapz==FDI_DISJOINT)
    ent = tell_eq(wam, dvx,dvz,xcapz);
  else {
    TAGGED xcupy = fd_union(wam, xdom,ydom);

    switch (fd_compare(zdom,xcupy)) {
    case FDI_INTERSECT:
    case FDI_SUPERSET:
      dvar_fix_set(dvz, xcupy);
    case FDI_EQUAL:
      /* entailed if at most one variable left */
      ent = (!dvar_is_integer(dvx) + !dvar_is_integer(dvy) + (!dvar_is_integer(dvz)) <= 1);
      break;
    case FDI_DISJOINT:
      ent = -1;
      break;
    }
  }
  dvar_pruning_done( dvx);
  dvar_pruning_done( dvy);
  dvar_pruning_done( dvz);
  dvar_export(dvx);
  dvar_export(dvy);
  dvar_export(dvz);
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}


/* computes -dom(d2) */
/* ripped off from indexical.c */
static TAGGED negdom(Wam wam, TAGGED d2)
{
  int j;
  TAGGED t2, r2, tail, *h, *array;

  for (j=0, t2=d2; t2!=EmptySet; j++)
    t2 = CTagToCdr(t2);

				/* create j intervals, then merge */

  NumstackAlloc(4*j,array);
  h = array + 4*j;
  tail = EmptySet;
  for (t2=d2; t2!=EmptySet;) {
    h -= 4;
    r2 = CTagToCar(t2); t2 = CTagToCdr(t2);
    h[0] = MakeList(h+2);
    h[1] = tail;
    h[2] = fd_safe_minus(TaggedZero,RangeMax(r2));
    h[3] = fd_safe_minus(TaggedZero,RangeMin(r2));
    tail = MakeList(h);
  }
  return tail;
}


/* '$fd_abs'(+State, -Actions) :- abs(X) #= Y.
   State is f(X,XMut,Y,YMut),
   Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_abs(Wam wam,
	      SP_term_ref State,
	      SP_term_ref NewState,
	      SP_term_ref Actions)
{
  TAGGED xdom, ydom, xmin;
  TAGGED handle;
  SP_BOOL committed;
  struct arith_data *pdata;
  Dvar dvx, dvy;
  int ent = -1;

  (void)State;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(NewState) = fd_static_output_state(wam,&handle,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct arith_data,handle);
  } else {			/* build persistent state */
    pdata = Palloc(struct arith_data, 2*sizeof(struct dvar), handle);
    pdata->destructor = arith_destructor;
    pdata->nrefs = 4;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(4);
    pdata->dvar = (Dvar)(pdata+1);
    fd_get_var_and_attr(X(0),pdata->refbase);
    fd_get_var_and_attr(X(0)+WD(2),pdata->refbase+2);
    dvar_init(pdata->dvar  , pdata->refbase,   pdata->refbase+1);
    dvar_init(pdata->dvar+1, pdata->refbase+2, pdata->refbase+3);
  }

  /* RESUME HERE */
  dvx = pdata->dvar;
  dvy = dvx+1;
  dvar_refresh(dvx);
  dvar_refresh(dvy);

  xdom = dvar_set(dvx);
  ydom = dvar_set(dvy);
  xmin = fd_min(xdom);
  if (!FDlt(xmin,TaggedZero)) {
    ent = tell_eq(wam, dvx,dvy,fd_compare(xdom,ydom));
  } else {
    TAGGED ydom0 = ydom;
    TAGGED xndom, yndom, xdom1, ydom1;
    TAGGED ymin = fd_min(ydom);
    
    if (FDlt(ymin,TaggedZero))
      ydom0 = fd_intersection_interval(wam, ydom0,TaggedZero,Sup);
    xndom = negdom(wam, xdom);
    yndom = negdom(wam, ydom0);
    xdom1 = fd_union(wam, ydom0,yndom);
    ydom1 = fd_union(wam, xdom,xndom);
    if (FDlt(ymin,TaggedZero))
      ydom1 = fd_intersection_interval(wam, ydom1,TaggedZero,Sup);
    switch (fd_compare(xdom,xdom1)) {
    case FDI_SUBSET:
    case FDI_EQUAL:
      break;
    case FDI_DISJOINT:
      goto ret;
    case FDI_INTERSECT:
      xdom1 = fd_intersection(wam, xdom,xdom1);
    case FDI_SUPERSET:
      dvar_fix_set(dvx, xdom1);
    }
    switch (fd_compare(ydom,ydom1)) {
    case FDI_SUBSET:
    case FDI_EQUAL:
      break;
    case FDI_DISJOINT:
      goto ret;
    case FDI_INTERSECT:
      ydom1 = fd_intersection(wam, ydom,ydom1);
    case FDI_SUPERSET:
      dvar_fix_set(dvy, ydom1);
    }
    ent = !!dvar_is_integer(dvy);
  }
  dvar_pruning_done( dvx);
  dvar_pruning_done( dvy);
  dvar_export(dvx);
  dvar_export(dvy);
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}

