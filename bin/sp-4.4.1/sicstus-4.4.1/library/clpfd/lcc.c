/* Copyright(C) 2001, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

typedef int VERTEX;

struct lcc_graph
{
  void (SPCDECL *destructor)(void*); /* [PM] 3.9b4 changed name to destructor for consistency */
  SPEnv *spenv;

  SP_globref refbase;
  SP_integer stamp;
  int nvars;
  int nvals;
  int nnodes;
  int nvartargets;
  int nvaltargets;
  int nactive;
  Dvar dvar;			/* nvals+nvars+2 */
  VERTEX *valtarget;            /* 2*nvals */
  VERTEX *vartarget;            /* 2*nvars */
  VERTEX *edge;                 /* 2*nvars*nvals */
  VERTEX *stack;                /* nvals+nvars+2, volatile */
  VERTEX *activeset;		/* union of {s,t,valtarget,vartarget}, ascending */
  struct {                      /* source, vals, vars, target */
    SP_integer *val;			/* the FD value for value vertices */
    SP_integer *component;	        /* auxiliary variable for some algorithms */
    SP_integer *nfixed;	        /* # fixed vars for a given value */
    SP_integer *out_degree;           /* # outbound edges (forward edges) */
    SP_integer *in_degree;            /* #  inbound edges (backward edges) */
    VERTEX **neighs;            /* array of pointers to the neighbours */
  } vertex;
};

#define DVAR(V) (pdata->dvar+(V))
#define TVAR(V) (pdata->vartarget[V])
#define ACTIVE(V) (pdata->activeset[V])
#define VAL(V) (pdata->vertex.val[V])
#define COMPONENT(V) (pdata->vertex.component[V])
#define NFIXED(V) (pdata->vertex.nfixed[V])
#define NEIGHS(V) (pdata->vertex.neighs[V])
#define NEIGH(V,I) (pdata->vertex.neighs[V][I])
#define OUT_DEGREE(V) (pdata->vertex.out_degree[V])
#define IN_DEGREE(V) (pdata->vertex.in_degree[V])
#define RefAttr(V) (pdata->refbase + 2*(V))
#define RefVar(V) (pdata->refbase + 2*(V) + 1)

#define TVAL(I) (pdata->valtarget[I])
/* these are only valid if V is a value vertex */
#define LIMIT(V) dvar_min_l(DVAR(V))
#define CAPACITY(V) dvar_max_l(DVAR(V))
#define VALKEY(I) ((I)-1)
#define VARKEY(I) ((I)-nvals-1)

#define VALOF(x) ((x) % nvals)


/*
  These are values that are NOT fd integers.  We set the value fields of
  the start, variable and target vertices to these values.  They are not
  essential, actually, we never test if a gien vertex has one of these
  values.
*/
static SP_integer const START_VERTEX  = CLPFD_MAXINT-2;
static SP_integer const VAR_VERTEX    = CLPFD_MAXINT-1;
static SP_integer const TARGET_VERTEX = CLPFD_MAXINT-0;

static void SPCDECL 
lcc_destructor(void *pdata_v)
{
  struct lcc_graph *pdata = (struct lcc_graph *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase, 2*pdata->nnodes);
  SP_free(pdata);
}


/* At present, called only from one place. Could be INLINEd. */
static struct lcc_graph *
lcc_alloc(Wam wam, 
	  int novars,
	  int novals,
	  TAGGED handle)
{
  char *ptr;
  int const nodes = novars+novals+2;
  SP_integer const total_size =
    nodes*sizeof(struct dvar) +
    5*nodes*sizeof(SP_integer) +
    nodes*sizeof(VERTEX *) +
    (novals+novars+nodes)*sizeof(VERTEX) +
    (novals+novals*novars+novars+1)*2*sizeof(VERTEX);

  struct lcc_graph *pdata =
    Palloc(struct lcc_graph, total_size, handle);
  pdata->destructor = lcc_destructor;
  FD_STORE_SPENV(pdata->spenv);
  pdata->nnodes = nodes;
  pdata->refbase = SP_alloc_globrefs(2*pdata->nnodes);
  pdata->nvartargets = pdata->nvars = novars;
  pdata->nvaltargets = pdata->nvals = novals;
  ptr = (char *)(pdata+1);
  pdata->dvar = (Dvar)ptr;
  ptr += nodes*sizeof(struct dvar);
  pdata->vertex.val = (SP_integer *)ptr;
  ptr += nodes*sizeof(SP_integer);
  pdata->vertex.neighs = (VERTEX **)ptr;
  ptr += nodes*sizeof(VERTEX *);
  pdata->vertex.component = (SP_integer *)ptr;
  ptr += nodes*sizeof(SP_integer);
  pdata->vertex.nfixed = (SP_integer *)ptr;
  ptr += nodes*sizeof(SP_integer);
  pdata->vertex.out_degree = (SP_integer *)ptr;
  ptr += nodes*sizeof(SP_integer);
  pdata->vertex.in_degree  = (SP_integer *)ptr;
  ptr += nodes*sizeof(SP_integer);
  pdata->valtarget = (VERTEX *)ptr;
  ptr += novals*sizeof(VERTEX);
  pdata->vartarget = (VERTEX *)ptr;
  ptr += novars*sizeof(VERTEX);
  pdata->activeset = (VERTEX *)ptr;
  ptr += nodes*sizeof(VERTEX);
  pdata->edge = (VERTEX *)ptr;
  ptr += (novals+novals*novars+novars+1)*2*sizeof(VERTEX);
  SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
  return pdata;
}

static void 
lcc_init(Wam wam,
	 struct lcc_graph *pdata,
	 TAGGED vals)
{
  int i, vx=0, nx=0, nvals=pdata->nvals, nvars=pdata->nvars;
  int n = pdata->nnodes;

  (void)wam;
  /* source vertex */
  NEIGHS(0) = pdata->edge;
  IN_DEGREE(0) = 1;
  VAL(0) = START_VERTEX;
  OUT_DEGREE(0) = nvals;
  for (i=nvals-1; i>=0; i--)
    NEIGH(0, i) = i+1;
  NEIGH(0, i) = n-1;
  ++vx;
  nx = nvals+1;

  /* value vertices + capacity + limit */
  for (i = 0; i < nvals; i++) {
    TAGGED telt, t1;
    TVAL(i) = vx;
    NEIGHS(vx) = pdata->edge + nx;
    NEIGH(vx, 0) = 0;
    IN_DEGREE(vx) = 1;
    OUT_DEGREE(vx) = 0;
    DerefCar(telt, vals);
    DerefCdr(vals, vals);
    DerefArg(t1, telt, 1);	/* get value to count */
    VAL(vx) = GetSmall(t1);
    nx += nvars+1;
    DerefArg(telt, telt, 2);	/* get count pair */
    fd_get_var_and_attr(telt,RefAttr(vx));
    dvar_init(DVAR(vx), RefAttr(vx), RefVar(vx));
    ++vx;
  }

  /* var vertices */
  for (i = 0; i < nvars; i++) {
    TVAR(i) = vx;
    NEIGHS(vx) = pdata->edge + nx;
    NEIGH(vx, 0) = n-1;
    IN_DEGREE(vx) = 0;
    OUT_DEGREE(vx) = 1;
    VAL(vx) = VAR_VERTEX;
    ++vx;
    nx += nvals+1;
  }

  /* target vertex */
  NEIGHS(vx) = pdata->edge + nx;
  OUT_DEGREE(vx) = 1;
  VAL(vx) = TARGET_VERTEX;
  NEIGH(vx,0) = 0;
  IN_DEGREE(vx) = nvars;
  for (i = 1; i <= nvars; i++)
    NEIGH(vx, i) = nvals+i;
}


static int 
lcc_apply_delta(struct lcc_graph *pdata,
		int *delta,
		int dmax,
		SP_BOOL valp,
		SP_BOOL incremental)
{
  int d=0, j=0, k=0, idelta, igraph;
  int nvals = pdata->nvals;
  VERTEX eov = pdata->nnodes;
  VERTEX v = valp ? delta[0]%nvals + 1 : delta[0]/nvals + nvals + 1;
  VERTEX *neighs = valp ? NEIGHS(v) : NEIGHS(v)+1;
  int degree = (int)(valp ? OUT_DEGREE(v) : IN_DEGREE(v));
  VERTEX *target = incremental ? neighs : pdata->stack;
  
  idelta = !valp ? delta[d++]%nvals + 1 : delta[d++]/nvals + nvals + 1;
  if (d<dmax && (valp ? delta[d]%nvals + 1 : delta[d]/nvals + nvals + 1)!=v)
    dmax = d;
  igraph = j>=degree ? eov : neighs[j++];
  while (idelta!=eov || igraph!=eov)
    if (idelta<igraph) {	/* domain edge, no graph edge (only noninc) */
      target[k++] = idelta;
      idelta = d==dmax ? eov :
	       !valp ? delta[d++]%nvals + 1 : delta[d++]/nvals + nvals + 1;
      if (d<dmax && (valp ? delta[d]%nvals + 1 : delta[d]/nvals + nvals + 1)!=v)
	dmax = d;
    } else if (idelta>igraph) { /* graph edge, no domain edge */
      target[k++] = igraph;
      igraph = j>=degree ? eov : neighs[j++];
    } else {
      idelta = d==dmax ? eov :
	       !valp ? delta[d++]%nvals + 1 : delta[d++]/nvals + nvals + 1;
      if (d<dmax && (valp ? delta[d]%nvals + 1 : delta[d]/nvals + nvals + 1)!=v)
	dmax = d;
      igraph = j>=degree ? eov : neighs[j++];
    }
  if (!incremental)
    for (j=k-1; j>=0; j--)
      neighs[j] = target[j];
  if (valp) {
    neighs[k] = 0;		/* inbound edge from source node */
    OUT_DEGREE(v) = k;
  } else {
    IN_DEGREE(v) = k;
  }
  return d;
}

/*
  Recompute the variable part of the graph repr.  UNIFIED VERSION.
*/
static void 
lcc_refresh(Wam wam,
		   struct lcc_graph *pdata,
		   SP_BOOL incremental)
{
  int i, d, dmax, *delta, *delta2;
  int nvars = pdata->nvars;
  int nvals = pdata->nvals;
  int nedges = nvars*nvals;
  VERTEX v;

  delta = Malloc(nedges*2,int);
  delta2 = delta+nedges;

  /* I: Compute Delta = (domain edges) XOR (graph edges), by ascending VAR vertex. */
  d = 0;
  for (i=pdata->nvaltargets+1; i<pdata->nactive-1; i++) {
    v = ACTIVE(i);
    if (!incremental || dvar_value_count(DVAR(v)) < IN_DEGREE(v)) {
      int degree = (int)IN_DEGREE(v);
      int j=0;
      int vbase = (v-nvals-1)*nvals;
      DVITER it;
      TAGGED tdom, tgraph;
      VERTEX cur=1;
      VERTEX *neighs = NEIGHS(v)+1;

      dviter_init(&it, DVAR(v));
      tdom = dviter_next_value_t(&it);
      tgraph = j>=degree ? SupAsINT : MakeSmall(VAL(neighs[j]));
      while (tdom!=SupAsINT || tgraph!=SupAsINT)
	if (Tlt(tdom,tgraph)) {	/* domain edge, no graph edge (only noninc) */
	  while (MakeSmall(VAL(cur)) != tdom)
	    cur++;
	  delta[d++] = vbase + cur - 1;
	  tdom = dviter_empty(&it) ? SupAsINT : dviter_next_value_t(&it);
	} else if (Tgt(tdom,tgraph)) { /* graph edge, no domain edge */
	  cur = neighs[j];
	  delta[d++] = vbase + cur - 1;
	  j++;
	  tgraph = j>=degree ? SupAsINT : MakeSmall(VAL(neighs[j]));
	} else {
	  cur = neighs[j];
	  j++;
	  tdom = dviter_empty(&it) ? SupAsINT : dviter_next_value_t(&it);
	  tgraph = j>=degree ? SupAsINT : MakeSmall(VAL(neighs[j]));
	}
    }
  }
  dmax = d;
  
  /* II. XOR Delta into VAR adjacency lists. */
  d = 0;
  while (d<dmax)
    d += lcc_apply_delta(pdata, delta+d, dmax-d, FALSE, incremental);

  /* III. Keysort Delta by ascending VAL vertex. */
  KEYSORT(delta, dmax, int, (int *)pdata->stack, nvals, VALOF, delta2);

  /* IV. XOR Delta into VAL adjacency lists. */
  d = 0;
  while (d<dmax)
    d += lcc_apply_delta(pdata, delta2+d, dmax-d, TRUE, incremental);

  SP_free(delta);
}

/* prevent there from being too few of too many vars
   taking value y
*/
static SP_BOOL
propagate_values(struct lcc_graph *pdata,
		 VERTEX y,
		 SP_BOOL *change)
{
  int lb = (int)NFIXED(y);
  int ub = (int)OUT_DEGREE(y);
  int j;

  if (COMPONENT(y) != y) {
    if (lb<ub && LIMIT(y)==ub) {
      *change |= TRUE;
      COMPONENT(y) = y;
      for (j=0; j<ub; j++) {
	VERTEX x = NEIGH(y,j);
	if (COMPONENT(x) != -1 && COMPONENT(x) != y)
	  return FALSE;
	COMPONENT(x) = y;
      }
    } else if (lb<ub && CAPACITY(y)==lb) {
      *change |= TRUE;
      COMPONENT(y) = y;
      for (j=0; j<ub; j++) {
	VERTEX x = NEIGH(y,j);
	if (IN_DEGREE(x)==1)
	  COMPONENT(x) = y;
      }
    }
  }
  return TRUE;
}

/* adjust bounds of counts */
static SP_BOOL
adjust_lcc_bounds(struct lcc_graph *pdata, SP_BOOL *change)
{
  int i, j, lb, ub;
  int nvaltargets = pdata->nvaltargets;

  /* Phase 0: reset component fields */
  
  for (i=1; i<pdata->nactive-1; i++)
    COMPONENT(ACTIVE(i)) = -1;

  /* Phase 1: infer changes from the value network */
  
  for (i=0; i < nvaltargets; i++) {
    VERTEX y = TVAL(i);
    ub = (int)OUT_DEGREE(y);
    /* adjust lower bound */
    lb = 0;
    for (j=0; j<ub; j++)
      if (IN_DEGREE(NEIGH(y,j))==1)
	lb++;
    NFIXED(y) = lb;
    if (LIMIT(y)<lb || CAPACITY(y)>ub) {
      if (dvar_fix_interval_l(DVAR(y), lb, ub)<0)
	return FALSE;
      *change |= (LIMIT(y)>lb || CAPACITY(y)<ub);
    }
    if (!propagate_values(pdata,y,change))
      return FALSE;
  }
  
  return TRUE;
}

static void 
compress_edges(Wam wam,
		      struct lcc_graph *pdata)
{
  int i, d, dmax, *delta, *delta2;
  int nvars = pdata->nvars;
  int nvals = pdata->nvals;
  int nedges = nvars*nvals;

  delta = Malloc(nedges*2,int);
  delta2 = delta+nedges;

  /* I: Compute Delta = (deleted edges) */
  d = 0;
  for (i=pdata->nvaltargets+1; i<pdata->nactive-1; i++) {
    VERTEX u = ACTIVE(i);
    int compu = (int)COMPONENT(u);
    int degree = (int)IN_DEGREE(u);
    int j=0;
    int ubase = (u-nvals-1)*nvals;
    VERTEX *neighs = NEIGHS(u)+1;
    
    for (j=0; j<degree; j++) {
      VERTEX v = neighs[j];
      if (COMPONENT(v)!=compu)
	delta[d++] = ubase + v - 1;
    }
  }
  dmax = d;
  
  /* II. XOR Delta into VAR adjacency lists. */
  d = 0;
  while (d<dmax)
    d += lcc_apply_delta(pdata, delta+d, dmax-d, FALSE, TRUE);

  /* III. Keysort Delta by ascending VAL vertex. */
  KEYSORT(delta, dmax, int, (int *)pdata->stack, nvals, VALOF, delta2);

  /* IV. XOR Delta into VAL adjacency lists. */
  d = 0;
  while (d<dmax)
    d += lcc_apply_delta(pdata, delta2+d, dmax-d, TRUE, TRUE);

  SP_free(delta);
}

/* precond: n>=1 */
static TAGGED 
lcc_new_domain(Wam wam, struct lcc_graph *pdata, VERTEX vertex)
{
  int i = 1;
  int last = (int)IN_DEGREE(vertex)+1;
  FDCONS cons;

  fdcons_init(&cons);
  while (i<last)
    fdcons_add(wam, &cons,MakeSmall(VAL(NEIGHS(vertex)[i++])));
  return fdcons_set(&cons);
}


static void 
contract_vars(struct lcc_graph *pdata)
{
  VERTEX *target = pdata->vartarget;
  int inf = 0;
  int sup = pdata->nvartargets-1;
  VERTEX held = target[sup]; /* sup is the hole */
  VERTEX current = target[inf];
    
  while (inf<=sup) {
    VERTEX y = NEIGH(current,1);
    if (IN_DEGREE(current)>1 || NFIXED(y)<OUT_DEGREE(y)) {
      target[inf] = current;
      inf++;
      current = (inf>=sup ? held : target[inf]);
    } else {
      target[sup] = current;
      sup--;
      current = (inf>=sup ? held : target[sup]);
    }
  }
  pdata->nvartargets = inf;
}


static void 
contract_vals(struct lcc_graph *pdata)
{
  VERTEX *target = pdata->valtarget;
  int inf = 0;
  int sup = pdata->nvaltargets-1;
  VERTEX held = target[sup]; /* sup is the hole */
  VERTEX current = target[inf];
    
  while (inf<=sup) {
    if (NFIXED(current)<OUT_DEGREE(current)) {
      target[inf] = current;
      inf++;
      current = (inf>=sup ? held : target[inf]);
    } else {
      target[sup] = current;
      sup--;
      current = (inf>=sup ? held : target[sup]);
    }
  }
  pdata->nvaltargets = inf;
}

/*
  '$fd_lcc'(+State0, -State, -Actions) :-
  State0 is f(NVars,NVals,Vars,Vals,Handle,Stamp).
  Vals is a keysorted list of Val-CountVar terms.  
  It is assumed, that the domains of Vars
  have been restricted to the available values.

  State similarly,
  Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_lcc(Wam wam,
	      SP_term_ref State0,
	      SP_term_ref State,
	      SP_term_ref Actions)
{
  int i, ent = -1;
  SP_integer state_stamp;
  TAGGED handle;
  struct lcc_graph *pdata;
  SP_BOOL committed, incremental;
  char *ptr;
  int nnodes, nvals, nvars, nvartargets, nvaltargets;

/*    X(0) = RefTerm(State0); */
  (void)State0;
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    fd.gdata = pdata = Pdata(struct lcc_graph, handle);
  } else {			/* build persistent state */
				/* compute novars, novals, all */
    TAGGED tlvec, telt;
    int novals, novars;
    VERTEX vx;

    DerefArg(tlvec, X(0), 1);	/* get #vars */
    novars = GetSmall_int(tlvec);
    DerefArg(tlvec, X(0), 2);	/* get #vals */
    novals = GetSmall_int(tlvec);

    /* ensure heap space for integer attributes */

    RequireHeap1((novars+novals)*INT_ATTRIBUTE_SIZE,handle,EVAL_ARITY);    
    fd.gdata = pdata = lcc_alloc(wam, novars, novals, handle);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */

    /* initialise the persistent parts */
    DerefArg(tlvec, X(0), 3);	/* get variables */
    for (i=0, vx=novals+1; i<novars; i++, vx++) {
      DerefCar(telt, tlvec);
      DerefCdr(tlvec, tlvec);
      RefGlob(RefVar(vx)) = telt;	/* protect FD variable */
      telt = fd_check_argument(wam, telt,Inf,Sup,Sup);
      RefGlob(RefAttr(vx)) = telt;	/* protect attribute */
      dvar_init(DVAR(vx), RefAttr(vx), RefVar(vx));
    }

    DerefArg(tlvec, X(0), 4);	/* get values */
    lcc_init(wam, pdata, tlvec);
    CTagToArg(X(0),3) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),4) = atom_nil; /* [MC] 3.12: free for GC */
  }

                                /* RESUME HERE */
  nnodes = pdata->nnodes;
  nvals = pdata->nvals;
  nvars = pdata->nvars;
  ptr = (char *)Malloc(nnodes,VERTEX);
  pdata->stack = (VERTEX *)ptr; /* volatile */
  incremental = (state_stamp == pdata->stamp);
  if (!incremental) {
    TAGGED telt;
    DerefArg(telt,X(0),1);
    pdata->nvartargets = GetSmall_int(telt);
    DerefArg(telt,X(0),2);
    pdata->nvaltargets = GetSmall_int(telt);
  }
  nvartargets = pdata->nvartargets;
  nvaltargets = pdata->nvaltargets;
  pdata->nactive = nvartargets + nvaltargets + 2;
  pdata->activeset[0] = 0;	/* SOURCE node */
  pdata->activeset[pdata->nactive-1] = nnodes-1; /* TARGET node */
  for (i = 0; i < nvaltargets; i++) {
    VERTEX vertex = TVAL(i);
    dvar_refresh(DVAR(vertex));
  }
  KEYSORT(pdata->valtarget, nvaltargets, VERTEX, pdata->stack,
	  nvals, VALKEY, pdata->activeset+1);
  for (i = 0; i < nvartargets; i++) {
    VERTEX vertex = TVAR(i);
    dvar_refresh(DVAR(vertex));
  }
  KEYSORT(pdata->vartarget, nvartargets, VERTEX, pdata->stack,
	  nvars, VARKEY, pdata->activeset+nvaltargets+1);
  lcc_refresh(wam, pdata,incremental);
  pdata->stamp = state_stamp+1;

  {
    SP_BOOL change;
    
    do {
      change = FALSE;
      if (!adjust_lcc_bounds(pdata,&change))
	goto ret;
      if (change)
	compress_edges(wam, pdata);
    } while (change);
  }

				/* Compute prunings. */
  ent = 1;			/* Entailed while ground */
  for (i = 0; i < pdata->nvartargets; i++) {
    VERTEX varvertex = TVAR(i);
    Dvar dv = DVAR(varvertex);
    int live = (int)IN_DEGREE(varvertex);

    if (live > 1)
      ent = 0;
    if (live != dvar_value_count(dv))
      dvar_fix_set(dv, lcc_new_domain(wam, pdata, varvertex));
    dvar_pruning_done( dv);
  }
  for (i = 0; i < pdata->nvartargets; i++) {
    VERTEX varvertex = TVAR(i);
    Dvar dv = DVAR(varvertex);

    dvar_export(dv);
  }
  for (i = 0; i < pdata->nvaltargets; i++) {
    VERTEX valvertex = TVAL(i);
    Dvar dv = DVAR(valvertex);

    dvar_export(dv);
  }
  contract_vals(pdata);		/* must precede contract_vars */
  contract_vars(pdata);

  CTagToArg(X(0),1) = MakeSmall(pdata->nvartargets);
  CTagToArg(X(0),2) = MakeSmall(pdata->nvaltargets);

ret:
  SP_free(pdata->stack);
  if (ent==1) {
    Pfree;
  }
  dvar_export_done(wam, Actions, ent);
}
