/* Copyright(C) 2001, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

/***
  Support for global_cardinality/2.
  Algorithms, see J-C. Regin: "Generalized Arc Consistency for
                               Global Cardinality Constraint".
  ***/

typedef int VERTEX;
static int const VEOL = 0xffffff; /* should be a big value see scc_visit */
static int const CEOL = -1;     /* should be negative */

struct gcc_graph
{
  void (SPCDECL *destructor)(void*); /* [PM] 3.9b4 changed name to destructor for consistency */
  SPEnv *spenv;

  SP_globref refbase;
  SP_integer stamp;
  int nvars;
  int nvals;
  int nnodes;			/* for convenience */
  int nvartargets;
  int nvaltargets;
  int nactive;
  int scc_component;		/* SCC number */
  int scc_visited;		/* Vertex number during SCC search */
  int scc_index;		/* Stack ptr during SCC search */
  Dvar dvar;			/* nvals+nvars+2 */
  VERTEX *valtarget;            /* 2*nvals */
  VERTEX *vartarget;            /* 2*nvars */
  VERTEX *edge;                 /* 2*nvars*nvals */
  VERTEX *stack;                /* nvals+nvars+2, volatile */
  VERTEX *activeset;		/* union of {s,t,valtarget,vartarget}, ascending */
#if EXPLICIT_FLOW
  int **flow;			/* a matrix */
#endif

  struct {                      /* source, vals, vars, target */
    SP_integer *val;			/* the FD value for value vertices */
    int *component;	        /* auxiliary variable for some algorithms */
    int *visited;	        /* auxiliary variable for some algorithms */
    VERTEX *dad;                /* aux. var.: previous vertex in a path, volatile */
#if !EXPLICIT_FLOW
    VERTEX *mate[2];		/* VAR: a VAL or -1; VAL: flow; SOURCE: flow; */
#endif
    int *out_degree;           /* # outbound edges (forward edges) */
    int *in_degree;            /* #  inbound edges (backward edges) */
    int *generation;
    VERTEX **neighs;            /* array of pointers to the neighbours */
    SP_integer *dual[2];		/* for cost-based pruning */
  } vertex;

  /* cost fields, persistent */
  SP_integer costmin[2];		/* min (-max) element of cost matrix */
  SP_integer *cost;			/* cost matrix, parameter */
  SP_integer *costbase;		/* cost minus a fixed offset */
  SP_integer *regret[2];		/* regret matrix, parameter */
  SP_integer *regretbase[2];		/* regret minus a fixed offset */
  
  /* cost fields, volatile */
  SP_integer optcost[2];
  SP_integer threshold[2];
  SP_integer min_regret[2];
  SP_integer max_regret[2];
  SP_integer *dist;
  SP_integer *distrow;		/* points at k'th row of dist */
  VERTEX *heap;
  int *vheap;
  int heapsize;
  int heapmax;
  int generation;
};

#define DVAR(V) (pdata->dvar+(V))
#define TVAR(V) (pdata->vartarget[V])
#define ACTIVE(V) (pdata->activeset[V])
#define VAL(V) (pdata->vertex.val[V])
#define NEIGHS(V) (pdata->vertex.neighs[V])
#define NEIGH(V,I) (pdata->vertex.neighs[V][I])
#define OUT_DEGREE(V) (pdata->vertex.out_degree[V])
#define IN_DEGREE(V) (pdata->vertex.in_degree[V])
#define COMPONENT(V) (pdata->vertex.component[V])
#define VISITED(V) (pdata->vertex.visited[V])
#define DAD(V) (pdata->vertex.dad[V])
#define DUAL(V,B) (pdata->vertex.dual[B][V])
#define MATE(V,B) (pdata->vertex.mate[B][V])
#define RefAttr(V) (pdata->refbase + 2*(V))
#define RefVar(V) (pdata->refbase + 2*(V) + 1)

#define TVAL(I) (pdata->valtarget[I])
/* these are only valid if V is a value vertex */
#define LIMIT(V) dvar_min_l(DVAR(V))
#define CAPACITY(V) dvar_max_l(DVAR(V))
#define VALKEY(I) ((I)-1)
#define VARKEY(I) ((I)-nvals-1)

#if EXPLICIT_FLOW

#define FLOW(Val,Var,B) pdata->flow[Val][Var]

#define SFLOW(Val,B) FLOW(0,Val,B)

#define INCFLOW(Val,Var,B) {			\
  pdata->flow[Val][Var]++;			\
  pdata->flow[Var][Val]--;			\
}

#else

#define FLOW(Val,Var,B) getflow(pdata,Val,Var,B)

#define SFLOW(Val,B) MATE(Val,B)

#define INCFLOW(Val,Var,B) incflow(pdata,Val,Var,B)

static int
getflow(struct gcc_graph *pdata,
	VERTEX From, VERTEX To, int b)
{
  int nvals = pdata->nvals;
  int nvars = pdata->nvars;
  int n = nvals + nvars + 2;

  if (From==0) {
    if (To==n-1)
      return -MATE(0,b);		/* SOURCE->TARGET */
    else
      return MATE(To,b);		/* SOURCE->VAL */
  } else if (From<=nvals) {
    if (To==0)
      return -MATE(To,b);		/* VAL->SOURCE */
    else
      return (MATE(To,b)==From);	/* VAL->VAR */
  } else if (From<n-1) {
    if (To<=nvals)
      return (MATE(From,b)==To ? -1 : 0); /* VAR->VAL */
    else
      return (MATE(From,b) > -1);	/* VAR->TARGET */
  } else {
    if (To==0)
      return MATE(0,b);		/* TARGET->SOURCE */
    else
      return (MATE(From,b) > -1 ? -1 : 0); /* TARGET->VAR */
  }
}

static void
incflow(struct gcc_graph *pdata,
	VERTEX From, VERTEX To, int b)
{
  int nvals = pdata->nvals;
  int nvars = pdata->nvars;
  int n = nvals + nvars + 2;

  if (From==0) {
    if (To==n-1)
      --MATE(0,b);		/* SOURCE->TARGET */
    else
      ++MATE(To,b);		/* SOURCE->VAL */
  } else if (From<=nvals) {
    if (To==0)
      --MATE(From,b);		/* VAL->SOURCE */
    else
      MATE(To,b) = From;		/* VAL->VAR */
  } else if (From<n-1) {
    if (To<=nvals)
      MATE(From,b) = -1;		/* VAR->VAL */
  } else {
    if (To==0)
      ++MATE(0,b);		/* TARGET->SOURCE */
  }
}

#endif

/* Invariant: VISITED(V)==VEOL and COMPONENT(V)==CEOL iff V is not a target.
   There is one small exception: on nonincremental resumption,
   the resurrected vertices will have EOLs in these fields.
*/

/*
  calculate the residual capacity of the edge u->v
  PRE: u and v are neighbours
*/
static int
res(struct gcc_graph *pdata,
    VERTEX u,
    VERTEX v,
    int b)
{
#if EXPLICIT_FLOW
  if (u == 0)                   /* from start to target or to vals */
    return (int)(v > pdata->nvals ? FLOW(v,u,b) : CAPACITY(v) - FLOW(u,v,b));
  if (v == 0)                   /* to start from target or from vals */
    return (int)(u > pdata->nvals ? pdata->nvars - FLOW(u,v,b) : FLOW(v,u,b) - LIMIT(u));
  /* other forward or backward edges */
  return (int)(u < v ? 1 - FLOW(u,v,b) : FLOW(v,u,b));
#else
  int n = pdata->nnodes;
  
  if (u == 0)                   /* from start to target or to vals */
    return (int)(v > pdata->nvals ? MATE(0,b) : CAPACITY(v) - MATE(v,b));
  if (v == 0)                   /* to start from target or from vals */
    return (int)(u > pdata->nvals ? pdata->nvars - MATE(0,b) : MATE(u,b) - LIMIT(u));
  /* other forward or backward edges */
  if (u == n-1)
    return (int)(v==0 ?  -MATE(0,b) : MATE(v,b) != -1);
  if (v == n-1)
    return (int)(u==0 ? 1+MATE(0,b) : MATE(u,b) == -1);
  return (int)(u < v ? MATE(v,b)!=u : MATE(u,b)==v);
#endif
}


#if DBG
extern void 
print_vertex(struct gcc_graph *pdata, VERTEX v);
void 
print_vertex(struct gcc_graph *pdata, VERTEX v)
{
  int i, degree;
  printf("%d(%d): ", v, COMPONENT(v));
  for (i = 0, degree = OUT_DEGREE(v); i < degree; ++i) {
    VERTEX u = NEIGH(v, i);
    printf("o(%d,%d,%d,%d,%d) ", u,
	      FLOW(v,u,0), res(pdata,v,u,0),
	      FLOW(v,u,1), res(pdata,v,u,1));
  }
  for (i = degree, degree += IN_DEGREE(v); i < degree; ++i) {
    VERTEX u = NEIGH(v, i);
    printf("i(%d,%d,%d,%d,%d) ", u,
	      FLOW(v,u,0), res(pdata,v,u,0),
	      FLOW(v,u,1), res(pdata,v,u,1));
  }
  printf("\n");
}


extern void 
print_graph(struct gcc_graph *pdata);
void 
print_graph(struct gcc_graph *pdata)
{
  int i;

  printf("vals:");
  for (i = 0; i < pdata->nvaltargets; ++i)
    printf(" %d", TVAL(i));
  printf(" #vars: %d\n", pdata->nvartargets);
  print_vertex(pdata, 0);
  for (i = 0; i < pdata->nvaltargets; ++i)
    print_vertex(pdata, TVAL(i));
  for (i = 0; i < pdata->nvartargets; ++i)
    print_vertex(pdata, TVAR(i));
  print_vertex(pdata, pdata->nvals+pdata->nvars+1);
  printf("\n");
}

#endif

/* 
   look for a directed path from s to t (in the residual graph) that
   doesn't include the edge s->t (because this function is only called
   from feasible_flow, we know that s->t is not part of the residual graph
   (it is infeasible))

   PRE: s != t
*/
static SP_BOOL 
augmenting_path(struct gcc_graph *pdata,
		VERTEX s,
		VERTEX t,
		int b)
{
  VERTEX *vqueue = pdata->stack;
  int top = 0, bottom = 0;
  int i;

  /* if (s == t) return FALSE; */

  /* Now we do a breadth first search to find t. */

  for (i=pdata->nactive-1; i>=0; i--)
    VISITED(ACTIVE(i)) = 0;

  DAD(s) = -1;                  /* s has no dad :( */
  VISITED(s) = 1;
  for (;;) {
    int degree = OUT_DEGREE(s) + IN_DEGREE(s);
    for (i = 0; i < degree; ++i) {
      VERTEX v = NEIGH(s,i);
      if (!VISITED(v) && 0 < res(pdata, s, v, b)) {
        DAD(v) = s;
        if (v == t)
	  return TRUE; /* found a path */
        VISITED(v) = 1;
        vqueue[top++] = v;
      }
    }
    if (top == bottom)
      return FALSE; /* t is not reachable from s */
    s = vqueue[bottom++];
  }
}


/*
  calulate the minimum residual capacity along the path (given by the dad
  pointers) and adjust the flow accordingly
*/
static void 
adjust_flow(struct gcc_graph *pdata,
	    VERTEX s, VERTEX t, int b)
{
  VERTEX d;

  INCFLOW(s,t,b);
  for (d = DAD(s); d >= 0; s = d, d = DAD(s))
    INCFLOW(d,s,b);
}


/*
  try to create a feasible flow if possible and return true iff succeeded
*/
static SP_BOOL 
feasible_flow(struct gcc_graph *pdata,
	      int b,
	      SP_BOOL *change)
{
  int i, ilim = pdata->nvaltargets;
  
  for (i = 0; i < ilim; ++i) {
    VERTEX v = TVAL(i);
    int limit = (int)LIMIT(v);
    while (SFLOW(v,b) < limit) {
      *change = TRUE;
      if (!augmenting_path(pdata, v, 0, b))
	return FALSE;
      adjust_flow(pdata, 0, v, b);
    }
  }
  return TRUE;
}


/*
  Partial breadth first search (in the residual graph) from the start:
  only vertices reachable from start are visited.  When the function is
  finised, the component field gives the depth from the start of the given
  vertex plus 1.  If this field is 0, then it isn't reachable from the start.
*/
static void bfs(struct gcc_graph *pdata,
		int b)
{
  int top = 0, bottom = 0;
  int i, n = pdata->nnodes;
  VERTEX *vqueue = pdata->stack, x = 0;

  for (i=pdata->nvaltargets-1; i>=0; i--)
    COMPONENT(TVAL(i)) = 0;
  for (i=pdata->nvartargets-1; i>=0; i--)
    COMPONENT(TVAR(i)) = 0;
  COMPONENT(n-1) = 0;
  COMPONENT(0) = 1;
  for (;;) {
    int degree = OUT_DEGREE(x) + IN_DEGREE(x);
    for (i = 0; i < degree; ++i) {
      VERTEX v = NEIGH(x, i);
      if (!COMPONENT(v) && 0 < res(pdata, x, v, b)) {
        COMPONENT(v) = COMPONENT(x)+1;
        vqueue[top++] = v;
      }
    }
    if (top == bottom)
      return;
    x = vqueue[bottom++];
  }
}


/*
  A flow is blocking iff on each directed path from the start to the sink
  there is a saturated edge.

  the component fields give the breadth-first search depth of the
  vertices starting from the start vertex in the residual graph.

  The edges x->v where v->component == x->component+1 form a DAG.  This
  function augments the current flow until it blocks this DAG.
*/
static void 
blocking_flow(struct gcc_graph *pdata,
	      int b)
{
  VERTEX const n = pdata->nnodes;

  int i;

  for (i=pdata->nvaltargets-1; i>=0; i--)
    VISITED(TVAL(i)) = 0;
  for (i=pdata->nvartargets-1; i>=0; i--)
    VISITED(TVAR(i)) = 0;
  VISITED(n-1) = 0;

  DAD(0) = -1;                  /* the source vertex has no dad */
  for (;;) {
    VERTEX x = 0;   /* start a depth first search from 0 to n-1 */
    int *stack = pdata->stack;
    i = 0;
    while (x != n-1) {
      VERTEX v=0;
      int degree = OUT_DEGREE(x) + IN_DEGREE(x);
      for (; i < degree; ++i) {
        v = NEIGHS(x)[i];
        if (COMPONENT(v) == COMPONENT(x)+1 && !VISITED(v) && 0<res(pdata,x,v,b))
          break;
      }
      if (i >= degree) {         /* there's no way out from x */
        if (x == 0)
	  return;     /* x is the source */
        VISITED(x) = 1;
        x = DAD(x);             /* take one step back */
        i = *--stack+1;         /* restore the old neighbour index */
        continue;
      }
      DAD(v) = x;
      x = v;                    /* take one step forward */
      *stack++ = i;             /* save the old neighbour index */
      i = 0;                    /*   and start the new one */
    }
    adjust_flow(pdata, n-1, 0, b);
  }
}


/*
  Find the maximum flow (using Dinic's algorithm) and return its value.
*/
static int 
maximum_flow(struct gcc_graph *pdata,
	     int b,
	     SP_BOOL *change)
{
  VERTEX target = pdata->nnodes-1;
  
  IN_DEGREE(0) = 0;             /* pretend that there's no t->s edge */
  for (;;) {
    bfs(pdata,b);
    if (!COMPONENT(target)) {   /* target is not reachable */
      IN_DEGREE(0) = 1;         /* there's actually a t->s edge */  
      return FLOW(target,0,b);	/* flow of the edge t->s */ /* HERE MATE(0) */
    }
    *change = TRUE;
    blocking_flow(pdata,b);
  }
}

/*
 A recursive function that finds strongly connected components [Tarjan'72].
 Code taken roughly from [Sedgewick, Algorithms in C, page 482] via
 alldistinct.c.
*/
static int 
sccs_visit(Wam wam,
		  VERTEX x)
{
  struct gcc_graph *pdata = fd.gdata;
  VERTEX v = -1;                /* different from all valid vertices */
  int i, degree = OUT_DEGREE(x) + IN_DEGREE(x);
  int min = ++pdata->scc_visited;

  VISITED(x) = pdata->scc_visited;
  pdata->stack[pdata->scc_index++] = x;
  for (i = 0; i < degree; ++i) {
    v = NEIGH(x, i);
    if (0 < res(pdata, x, v, 0)) {
      int m = VISITED(v) ? VISITED(v) : sccs_visit(wam, v);
      if (m < min)
	min = m;
    }
  }
  if (min == VISITED(x)) {
    ++pdata->scc_component;
    while (v != x) {
      v = pdata->stack[--pdata->scc_index];
      COMPONENT(v) = pdata->scc_component;
      VISITED(v) = VEOL; /* High value, so that
                                        this vertex will be ignored
                                        in the future search. */
    }
  }
  return min;
}


static int
sccs(Wam wam,
	    struct gcc_graph *pdata)
{
  int i;
  /* int t = pdata->nvals+pdata->nvars+1; */

  pdata->scc_component = 0;
  pdata->scc_visited = 0;
  pdata->scc_index = 0;

  for (i=pdata->nvaltargets-1; i>=0; i--)
    VISITED(TVAL(i)) = 0;
  for (i=pdata->nvartargets-1; i>=0; i--)
    VISITED(TVAR(i)) = 0;
/*    VISITED(t) = 0; */

  IN_DEGREE(0) = 0;             /* pretend that there's no t->s edge */
  sccs_visit(wam, 0);
  for (i = 0; i < pdata->nvaltargets; ++i) {
    VERTEX v = TVAL(i);
    if (!VISITED(v))
      sccs_visit(wam, v);
  }
  for (i = 0; i < pdata->nvartargets; ++i) {
    VERTEX v = TVAR(i);
    if (!VISITED(v))
      sccs_visit(wam, v);
  }
/*    if (!VISITED(t)) sccs_visit(wam, t); */
  IN_DEGREE(0) = 1;             /* there's a t->s edge */
  return pdata->scc_component;
}

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
gcc_destructor(void *pdata_v)
{
  struct gcc_graph *pdata = (struct gcc_graph *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase, 2*pdata->nnodes);
  SP_free(pdata);
}


/* At present, called only from one place. Could be INLINEd. */
static struct gcc_graph *
gcc_alloc(Wam wam,
	  int novars,
	  int novals,
	  int flags,
	  TAGGED handle)
{
  char *ptr;
  int b;
  int const nodes = novars+novals+2;
  SP_integer const total_size =
    nodes*sizeof(struct dvar) +
    nodes*sizeof(SP_integer) +
    4*nodes*sizeof(int) +
    nodes*sizeof(VERTEX *) +
    (novals+novars+nodes)*sizeof(VERTEX) +
    (novals+novals*novars+novars+1)*2*sizeof(VERTEX) +
#if EXPLICIT_FLOW
    nodes*sizeof(int *) +
    nodes*nodes*sizeof(int) +
#else
    2*nodes*sizeof(VERTEX) +
#endif
    (!flags ? 0 : 3*novars*novals*sizeof(SP_integer)) +
    (!flags ? 0 : 2*nodes*sizeof(SP_integer)) +
    (!flags ? 0 : nodes*sizeof(int));

  struct gcc_graph *pdata =
    Palloc(struct gcc_graph, total_size, handle);
  pdata->destructor = gcc_destructor;
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
#if EXPLICIT_FLOW
  pdata->flow = (int **)ptr;
  ptr += nodes*sizeof(int *);
#endif
  if (!flags) {
    pdata->cost = NULL;
  } else {
    pdata->cost = (SP_integer *)ptr;
    pdata->costbase = pdata->cost - (novals*novals + novals + 1);
    ptr += novars*novals*sizeof(SP_integer);
    for (b=0; b<2; b++) {
      pdata->regret[b] = (SP_integer *)ptr;
      pdata->regretbase[b] = pdata->regret[b] - (novals*novals + novals + 1);
      ptr += novars*novals*sizeof(SP_integer);
      pdata->vertex.dual[b] = (SP_integer *)ptr;
      ptr += nodes*sizeof(SP_integer);
    }
    pdata->vertex.generation = (int *)ptr;
    ptr += nodes*sizeof(int);
  }
  pdata->vertex.component = (int *)ptr;
  ptr += nodes*sizeof(int);
  pdata->vertex.visited = (int *)ptr;
  ptr += nodes*sizeof(int);
  pdata->vertex.out_degree = (int *)ptr;
  ptr += nodes*sizeof(int);
  pdata->vertex.in_degree  = (int *)ptr;
  ptr += nodes*sizeof(int);
  pdata->valtarget = (VERTEX *)ptr;
  ptr += novals*sizeof(VERTEX);
  pdata->vartarget = (VERTEX *)ptr;
  ptr += novars*sizeof(VERTEX);
  pdata->activeset = (VERTEX *)ptr;
  ptr += nodes*sizeof(VERTEX);
  pdata->edge = (VERTEX *)ptr;
  ptr += (novals+novals*novars+novars+1)*2*sizeof(VERTEX);
#if EXPLICIT_FLOW
  for (i = 0; i < nodes; ++i) {
    pdata->flow[i] = (int *)ptr;
    ptr += nodes*sizeof(int);
  }
  memset((char *)pdata->flow[0], 0, nodes*nodes*sizeof(int));
#else
  for (b=0; b<2; b++) {
    pdata->vertex.mate[b] = (VERTEX *)ptr;
    ptr += nodes*sizeof(VERTEX);
    memset((char *)&MATE(0,b), 0, (novals+1)*sizeof(VERTEX));
    memset((char *)&MATE(novals+1,b), -1, (novars+1)*sizeof(VERTEX));
  }
#endif
  SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
  return pdata;
}


/* At present, called only from one place. Could be INLINEd. */
static void 
gcc_init(Wam wam, struct gcc_graph *pdata,
		TAGGED vals,
		TAGGED tmat)
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
  for (i = 0; i < nvals; ++i) {
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
  for (i = 0; i < nvars; ++i) {
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
  for (i = 1; i <= nvars; ++i)
    NEIGH(vx, i) = nvals+i;

  /* cost matrix */
  if (!IsAtomic(tmat)) {
    TAGGED trow, telt;
    int k = 0;
    SP_integer cmin = CLPFD_MAXINT;
    SP_integer cmax = -cmin;
    SP_integer c;
    VERTEX v;
    
    while (tmat!=atom_nil) {
      DerefCar(trow,tmat);
      DerefCdr(tmat,tmat);
      while (trow!=atom_nil) {
	DerefCar(telt,trow);
	DerefCdr(trow,trow);
	c = GetSmall(telt);
	if (cmin>c)
	  cmin = c;
	if (cmax<c)
	  cmax = c;
	pdata->cost[k++] = c;
      }
    }
    pdata->costmin[0] = cmin;
    pdata->costmin[1] = -cmax;
    pdata->generation = 0;
    /* reset dual values and generation "cookies" */
    for (v=0; v<n; v++) {
      DUAL(v,0) = v<=nvals ? 0 : -cmin;
      DUAL(v,1) = v<=nvals ? 0 : cmax;
      pdata->vertex.generation[v] = 0;
    }
  }
}


static int 
gcc_apply_delta(struct gcc_graph *pdata,
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
  int degree = valp ? OUT_DEGREE(v) : IN_DEGREE(v);
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
  Should be called for an already non-existent edge val->var.  It will
  zero the flow on this edge and on the edge start->val.  The original
  flow is returned.
*/
static int 
reset_flow(struct gcc_graph *pdata,
	 VERTEX val,
	 VERTEX var,
	 int b)
{
  int f = FLOW(val,var,b);

  if (f) {
#if EXPLICIT_FLOW
    INCFLOW(n-1,var,b);
    INCFLOW(var,val,b);
    INCFLOW(val,0,b);
    INCFLOW(0,n-1,b);
#else
    MATE(var,b) = -1;
    MATE(val,b)--;
    MATE(0,b)--;
#endif
  }
  return f;
}


#define VALOF(x) ((x) % nvals)

/*
  Recompute the variable part of the graph repr.  UNIFIED VERSION.
*/
static void 
gcc_refresh(Wam wam,
		   struct gcc_graph *pdata,
		   int flags,
		   SP_BOOL incremental)
{
  int i, b, d, dmax, *delta, *delta2;
  int bmax = flags ? 2 : 1;
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
      int degree = IN_DEGREE(v);
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
	  for (b=0; b<bmax; b++)
	    reset_flow(pdata, cur, v, b);
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
    d += gcc_apply_delta(pdata, delta+d, dmax-d, FALSE, incremental);

  /* III. Keysort Delta by ascending VAL vertex. */
  KEYSORT(delta, dmax, int, (int *)pdata->stack, nvals, VALOF, delta2);

  /* IV. XOR Delta into VAL adjacency lists. */
  d = 0;
  while (d<dmax)
    d += gcc_apply_delta(pdata, delta2+d, dmax-d, TRUE, incremental);

  SP_free(delta);
}


static void 
compress_edges(Wam wam,
		      struct gcc_graph *pdata)
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
    int compu = COMPONENT(u);
    int degree = IN_DEGREE(u);
    int j=0;
    int ubase = (u-nvals-1)*nvals;
    VERTEX *neighs = NEIGHS(u)+1;
    
    for (j=0; j<degree; j++) {
      VERTEX v = neighs[j];
      if (COMPONENT(v)!=compu && FLOW(v,u,0)==0)
	delta[d++] = ubase + v - 1;
    }
  }
  dmax = d;
  
  /* II. XOR Delta into VAR adjacency lists. */
  d = 0;
  while (d<dmax)
    d += gcc_apply_delta(pdata, delta+d, dmax-d, FALSE, TRUE);

  /* III. Keysort Delta by ascending VAL vertex. */
  KEYSORT(delta, dmax, int, (int *)pdata->stack, nvals, VALOF, delta2);

  /* IV. XOR Delta into VAL adjacency lists. */
  d = 0;
  while (d<dmax)
    d += gcc_apply_delta(pdata, delta2+d, dmax-d, TRUE, TRUE);

  SP_free(delta);
}

/* maintain the variant res(S,Yi)>=0 for all values Yi */
static void
maintain_capacities(struct gcc_graph *pdata,
		    int b)
{
  int i, j;
  int ilim = pdata->nvaltargets;
  
  for (i=0; i < ilim; i++) {
    VERTEX y = TVAL(i);
    int f = -res(pdata,0,y,b);

    for (j=0; f>0; j++)	/* consider each neighbor */
      f -= reset_flow(pdata,y,NEIGH(y,j),b);
  }
}

/* adjust bounds of counts */
static SP_BOOL
adjust_count_bounds(struct gcc_graph *pdata,
		    int flags,
		    SP_BOOL *change)
{
  int rc = 0;
  int i, j, lb, jlim, f, b;
  int nvaltargets = pdata->nvaltargets;
  int bmax = flags ? 2 : 1;

  /* Phase 1: infer changes from the value network */
  
  for (i=0; i < nvaltargets; i++) {
    VERTEX y = TVAL(i);
    jlim = OUT_DEGREE(y);
    /* adjust lower bound */
    lb = 0;
    for (j=0; j<jlim; j++)
      if (IN_DEGREE(NEIGH(y,j))==1)
	lb++;
    if (LIMIT(y)<lb || CAPACITY(y)>jlim) {
      if (dvar_fix_interval_l(DVAR(y), lb, jlim)<0)
	return FALSE;
      rc |= (LIMIT(y)>lb || CAPACITY(y)<jlim /* 3.9 */);
      for (b=0; !rc && b<bmax; b++) {
	f = SFLOW(y,b);
	rc |= (LIMIT(y)>f || CAPACITY(y)<f);
      }
    }
  }
  
  *change |= rc;
  return TRUE;
}

/* precond: n>=1 */
static TAGGED 
gcc_new_domain(Wam wam,
	       struct gcc_graph *pdata,
	       int i,
	       int n,
	       VERTEX vertex)
{
  int last = i+n;
  FDCONS cons;

  fdcons_init(&cons);
  while (i<last)
    fdcons_add(wam, &cons,MakeSmall(VAL(NEIGHS(vertex)[i++])));
  return fdcons_set(&cons);
}


static void 
contract_vars(struct gcc_graph *pdata)
{
  VERTEX *target = pdata->vartarget;
  int inf = 0;
  int sup = pdata->nvartargets-1;
  VERTEX held = target[sup]; /* sup is the hole */
  VERTEX current = target[inf];
    
  while (inf<=sup) {
    if (IN_DEGREE(current)>1 /*new*/ || VISITED(NEIGH(current,1))!=VEOL) {
      target[inf] = current;
      inf++;
      current = (inf>=sup ? held : target[inf]);
    } else {
      VISITED(current) = VEOL;	/* maintain invariant! */
      COMPONENT(current) = CEOL; /* maintain invariant! */
      target[sup] = current;
      sup--;
      current = (inf>=sup ? held : target[sup]);
    }
  }
  pdata->nvartargets = inf;
}


static void 
contract_vals(struct gcc_graph *pdata)
{
  VERTEX *target = pdata->valtarget;
  int inf = 0;
  int sup = pdata->nvaltargets-1;
  VERTEX held = target[sup]; /* sup is the hole */
  VERTEX current = target[inf];
    
  while (inf<=sup) {
    int degree = OUT_DEGREE(current);
    int i;
    SP_BOOL forget = (degree == SFLOW(current,0));

    for (i=0; i<degree && forget; i++) {
      VERTEX n = NEIGH(current,i);
      if (IN_DEGREE(n)>1)
	forget = FALSE;
    }

    if (!forget) {
      VISITED(current) = 0;	/* maintain invariant! */
      target[inf] = current;
      inf++;
      current = (inf>=sup ? held : target[inf]);
    } else {
      VISITED(current) = VEOL;	/* maintain invariant! */
      COMPONENT(current) = CEOL; /* maintain invariant! */
      target[sup] = current;
      sup--;
      current = (inf>=sup ? held : target[sup]);
    }
  }
  pdata->nvaltargets = inf;
}

/* PRE: value I and var J are neighbors. */
#define COST(VAL,VAR,B)  			\
(_cost = pdata->costbase[nvals*(VAR) + (VAL)],	\
 ((B) ? -_cost : _cost))

#define REGRET(VAL,VAR,B) pdata->regretbase[B][nvals*(VAR) + (VAL)] 

#define INVALIDATE_DIST pdata->generation++

#define VALIDATE_DIST_ROW(V,B) 				\
if (pdata->vertex.generation[V]!=pdata->generation) {	\
  costgcc_reset_dist_row(pdata,V,B);			\
}


static SP_integer
reduced_cost(struct gcc_graph *pdata, VERTEX u, VERTEX v, int b)
{
  int nvals = pdata->nvals;
  SP_integer differential = DUAL(v,b) - DUAL(u,b);
  SP_integer _cost;
  
  if (u==0 || v==0) {			/* (S,VAL) | (VAL,S) */
    return   differential;
  } else if (u<=nvals) {	/* (VAL,VAR) */
      return differential + COST(u,v,b);
  } else {			/* (VAR,VAL) */
    return   differential - COST(v,u,b);
  }
}




/* reset distances */
static void
costgcc_reset_dist_row(struct gcc_graph *pdata,
		       VERTEX u,
		       int b)
{
  int top = pdata->nactive-1;	/* exclude the target node */
  int varbase = top-pdata->nvartargets;
  int j;
  int n = pdata->nnodes;
  int nvals = pdata->nvals;
  SP_integer *distrow = pdata->dist + n*u;

  pdata->vertex.generation[u] = pdata->generation;
  if (u==0) {
    for (j=1; j<varbase; j++) { /* [S,VAL] entries */
      VERTEX v = ACTIVE(j);
      SP_integer redcost = reduced_cost(pdata,0,v,b);
      distrow[v] = SFLOW(v,b) >= CAPACITY(v) ? CLPFD_MAXINT :
	redcost<0 ? 0 : redcost;
    }
    for (; j<top; j++) { /* [S,VAR] entries */
      distrow[ACTIVE(j)] = CLPFD_MAXINT;
    }
  } else if (u<=nvals) {	/* [VAL,S] entries */
    int deg = OUT_DEGREE(u);
    int k=0;
    SP_integer redcost = reduced_cost(pdata,u,0,b);
      
    distrow[0] = SFLOW(u,b) <= LIMIT(u) ? CLPFD_MAXINT :
      redcost<0 ? 0 : redcost;
    for (j=1; j<varbase; j++) { /* [VAL,VAL] entries */
      distrow[ACTIVE(j)] = CLPFD_MAXINT;
    }
    for (; j<top; j++) { /* [VAL,VAR] entries */
      /* we must take domain info into account */
      VERTEX v = ACTIVE(j);
      SP_integer redcost = reduced_cost(pdata,u,v,b);
      int k0 = k;
	
      if (k<deg && NEIGH(u,k)==v)
	k++;	
      distrow[v] = k==k0 ? CLPFD_MAXINT :
	FLOW(u,v,b) ? CLPFD_MAXINT : redcost<0 ? 0 : redcost;
    }
  } else {
    distrow[0] = CLPFD_MAXINT;	/* [VAR,S] */
    for (j=1; j<varbase; j++) { /* [VAR,VAL] entries */
      VERTEX v = ACTIVE(j);
      SP_integer redcost = reduced_cost(pdata,u,v,b);
      distrow[v] = FLOW(v,u,b)==0 ? CLPFD_MAXINT : redcost<0 ? 0 : redcost;
    }
    for (; j<top; j++) { /* [VAR,VAR] entries */
      distrow[ACTIVE(j)] = CLPFD_MAXINT;
    }
  }
  distrow[u] = 0;		/* zero the main diagonal! */
}


#define SWAP(I,J)				\
{						\
  VERTEX vi = pdata->heap[I];			\
  VERTEX vj = pdata->heap[J];			\
  pdata->heap[I] = vj;				\
  pdata->heap[J] = vi;				\
  pdata->vheap[vi] = (J);			\
  pdata->vheap[vj] = (I);			\
}

static void 
decrease_key(struct gcc_graph *pdata,
	     VERTEX v,
	     SP_integer value)
{
  int i = pdata->vheap[v];
  int p = (i-1)>>1;
  pdata->distrow[v] = value;
  while (i>0 && value<pdata->distrow[pdata->heap[p]]) {
    SWAP(i,p);
    i = p;
    p = (i-1)>>1;
  }
}

static void 
spheapify(struct gcc_graph *pdata,
	  int i)
{
  int *heap = pdata->heap;
  SP_integer *distrow = pdata->distrow;
  
  for (;;) {
    int l = (i<<1)+1;
    int smallest = i;
    if (l<pdata->heapsize && distrow[heap[l]]<distrow[heap[smallest]])
      smallest = l;
    if (l+1<pdata->heapsize && distrow[heap[l+1]]<distrow[heap[smallest]])
      smallest = l+1;
    if (smallest==i)
      break;
    SWAP(i,smallest);
    i = smallest;
  }
}

static void 
spheap_init(struct gcc_graph *pdata,
	    VERTEX vertex)
{
  int i;
  VERTEX v;
  int top = pdata->nactive-1;	/* exclude TARGET */
  
  for (i=top-1; i>=0; i--) {
    v = ACTIVE(i);
    pdata->heap[i] = v;
    pdata->vheap[v] = i;
  }
  /* warm start: exclude vertex (SOURCE or VALUE) from queue */
  top--;
  i = pdata->vheap[vertex];
  v = pdata->heap[top];
  pdata->heap[i] = v;
  pdata->vheap[v] = i;
  pdata->heapsize = top;
  for (i=(top-2)>>1; i>=0; i--)
    spheapify(pdata,i);
}

static VERTEX 
spheap_extract_min(struct gcc_graph *pdata)
{
  VERTEX u = pdata->heap[0];
  VERTEX v = pdata->heap[--pdata->heapsize];

  pdata->vheap[u] = -1;		/* mark not in heap */
  if (pdata->heapsize>0) {
    pdata->heap[0] = v;
    pdata->vheap[v] = 0;
    spheapify(pdata,0);
  }
  return u;
}

/* Solve single-source shortest-paths for vertex.
 * Stop when distances for all nodes in stack computed
 *   OR when distances >= duth.
 */
static void 
shortest_paths(struct gcc_graph *pdata,
	       VERTEX vertex,
	       VERTEX *stack,
	       int top,
	       SP_integer duth,
	       int b)
{
  int n = pdata->nnodes;
  int nvals = pdata->nvals;
  int nvaltargets = pdata->nvaltargets;
  int j;

  VALIDATE_DIST_ROW(vertex,b);
  pdata->distrow = pdata->dist + n*vertex;
  spheap_init(pdata, vertex);
  while (top>0) {
    VERTEX u = spheap_extract_min(pdata);
    SP_integer du = pdata->distrow[u];
    
    if (du>=duth)
      break;
    else if (u==0) {		/* u is the SOURCE */
      VALIDATE_DIST_ROW(0,b);
      for (j=0; j<nvaltargets; j++) {
	VERTEX v = TVAL(j);
	SP_integer duv = pdata->dist[v];
	if (duv<CLPFD_MAXINT && pdata->distrow[v]>du+duv)
	  decrease_key(pdata,v,du+duv);
      }
    } else {	/* u is a VAL or VAR */
      int deg = IN_DEGREE(u)+OUT_DEGREE(u);
      VALIDATE_DIST_ROW(u,b);
      for (j=(u>nvals); j<deg; j++) { /* skip TARGET node! */
	VERTEX v = NEIGH(u,j);
	SP_integer duv;

	if (v>n)
	  v -= n;	/* could have been marked already */
	duv = pdata->dist[n*u + v];
	if (duv<CLPFD_MAXINT && pdata->distrow[v]>du+duv)
	  decrease_key(pdata,v,du+duv);
      }
    }
    if (stack[top-1] == u) {
      do
	top--;
      while (top>0 && pdata->vheap[stack[top-1]] == -1);
    }
  }
}

/* Same as above, but returns first VAR node found, or -1.
 * Also, updates DAD pointers at every decrease_key call.
 * term=n => returns first VAR node with zero outflow
 * term<n => returns node term
 */
static VERTEX
opt_shortest_paths(struct gcc_graph *pdata,
		   VERTEX vertex,
		   VERTEX target,
		   int b)
{
  int n = pdata->nnodes;
  int nvals = pdata->nvals;
  int nvaltargets = pdata->nvaltargets;
  int j;

  VALIDATE_DIST_ROW(vertex,b);
  pdata->distrow = pdata->dist + n*vertex;
  spheap_init(pdata, vertex);
  for (j=pdata->heapsize-1; j>=0; j--) {
    VERTEX v = pdata->heap[j];
    DAD(v) = pdata->distrow[v]<CLPFD_MAXINT ? vertex : -1;
  }
  DAD(vertex) = -1;
  pdata->heapmax = pdata->heapsize;
  while (pdata->heapsize>0) {
    VERTEX u = spheap_extract_min(pdata);
    SP_integer du = pdata->distrow[u];

    pdata->heap[pdata->heapsize] = u; /* for updating dual(u) */
    if (du==CLPFD_MAXINT)
      break;
    else if (target==u)		/* explicit vertex popped */
      return u;
    else if (target==n && u>nvals && !FLOW(u,n-1,b)) /* unassigned VAR vertex popped */
      return u;			/* unassigned VAR vertex popped */
    else if (u==0) {		/* u is the SOURCE */
      VALIDATE_DIST_ROW(0,b);
      for (j=0; j<nvaltargets; j++) {
	VERTEX v = TVAL(j);
	SP_integer duv = pdata->dist[v];
	if (duv<CLPFD_MAXINT && pdata->distrow[v]>du+duv) {
	  DAD(v) = u;
	  decrease_key(pdata,v,du+duv);
	}
      }
    } else {			/* u is a VAL or assigned VAR */
      int deg = IN_DEGREE(u)+OUT_DEGREE(u);
      VALIDATE_DIST_ROW(u,b);
      for (j=(u>nvals); j<deg; j++) { /* skip TARGET node! */
	VERTEX v = NEIGH(u,j);
	SP_integer duv = pdata->dist[n*u + v];
	if (duv<CLPFD_MAXINT && pdata->distrow[v]>du+duv) {
	  DAD(v) = u;
	  decrease_key(pdata,v,du+duv);
	}
      }
    }
  }
  return -1;
}


static SP_BOOL
costgcc_mark(Wam wam,
		    struct gcc_graph *pdata,
		    int b)
{
  int nvaltargets = pdata->nvaltargets;  
  int nvals = pdata->nvals;
  SP_integer _cost, max_regret, min_regret;
  int i, j;
  
  (void)wam;
  if (b==0) {
    TAGGED ubt = dvar_max_t(DVAR(0)); 
    pdata->threshold[0] =
      TagIsSmall(ubt) ? GetSmall(ubt) - pdata->optcost[0] :
      CLPFD_MAXINT-1;
  } else {
    TAGGED lbt = dvar_min_t(DVAR(0)); 
    pdata->threshold[1] =
      TagIsSmall(lbt) ? - pdata->optcost[1] - GetSmall(lbt) :
      CLPFD_MAXINT-1;
  }
  if (pdata->threshold[b]<0)	/* minimum is too large,
				   or maximum is too small */

    return FALSE;

  max_regret = 0;
  min_regret = CLPFD_MAXINT;
  INVALIDATE_DIST;
  for (i=0; i<nvaltargets; i++) {
    VERTEX /*value b*/val1 = TVAL(i);
    if (SFLOW(val1,b)) {
      int deg = OUT_DEGREE(val1);
      int top = 0;

      for (j=nvaltargets-1; j>=0; j--)
	DAD(TVAL(j)) = 0;	/* mark not in stack */
      for (j=0; j<deg; j++) {
	VERTEX /*var y*/var1 = NEIGH(val1,j);
      
	if (FLOW(val1,var1,b)) {
	  int deg2 = IN_DEGREE(var1);
	  int j2;

	  if (deg2>1) { /* otherwise there will be no val2 */
	    for (j2=1; j2<=deg2; j2++) {
	      VERTEX /*value a*/val2 = NEIGH(var1,j2);
	      if (val2!=val1) {
		if (!DAD(val2)) {
		  pdata->stack[top++] = val2;
		  DAD(val2) = var1;	/* mark in stack */
		}
	      }
	    }
	  }
	}
      }

      shortest_paths(pdata, val1, pdata->stack, top, pdata->threshold[b]+1, b);
      for (j=0; j<deg; j++) {
	VERTEX /*var y*/var1 = NEIGH(val1,j);
      
	if (FLOW(val1,var1,b)) {
	  int deg2 = IN_DEGREE(var1);
	  int j2;
	  
	  REGRET(val1,var1,b) = 0;
	  if (deg2>1) { /* otherwise there will be no val2 */
	    SP_integer relcost1 = COST(val1,var1,b) - DUAL(val1,b);
	    for (j2=1; j2<=deg2; j2++) {
	      VERTEX /*value a*/val2 = NEIGH(var1,j2);

	      if (val2!=val1) {
		SP_integer dist = pdata->distrow[val2];

		if (dist==CLPFD_MAXINT) { /* separate case to avoid oflo */
		  max_regret = dist;
		  REGRET(val2,var1,b) = dist;
		} else {
		  SP_integer relcost2 = COST(val2,var1,b) - DUAL(val2,b);
		  SP_integer regret = dist-relcost1+relcost2;
		  if (max_regret<regret)
		    max_regret = regret;
		  if (min_regret>regret)
		    min_regret = regret;
		  REGRET(val2,var1,b) = regret;
		}
	      }
	    }
	  }
	}
      }
    }
  }
  pdata->max_regret[b] = max_regret;
  pdata->min_regret[b] = min_regret;
  return TRUE;
}

static SP_BOOL
costgcc_prune(Wam wam,
	      struct gcc_graph *pdata,
	      SP_BOOL *change)
{
  SP_BOOL cost_has_hole = !dvar_is_interval(DVAR(0));
  SP_integer optcost;

  if (pdata->max_regret[0] > pdata->threshold[0] ||
      pdata->max_regret[1] > pdata->threshold[1] ||
      cost_has_hole) {
    SP_integer *threshold = pdata->threshold;
    SP_integer regret[2];
    int i, j, d, dmax, *delta, *delta2;
    int nvars = pdata->nvars;
    int nvals = pdata->nvals;
    int nedges = nvars*nvals;
    SP_BOOL ok = TRUE;
    
    delta = Malloc(nedges*2,int);
    delta2 = delta+nedges;

    /* I: Compute Delta = edges to delete by ascending VAR vertex. */
    d = 0;
    for (i=pdata->nvaltargets+1; i<pdata->nactive-1; i++) {
      VERTEX u = ACTIVE(i);
      int degree = IN_DEGREE(u);
      
      if (degree>1) {
	int d0 = d;
	int vbase = (u-nvals-1)*nvals;
	VERTEX *neighs = NEIGHS(u)+1;

	for (j=0; j<degree; j++) {
	  VERTEX v = neighs[j];
	  regret[0] = REGRET(v,u,0);
	  regret[1] = REGRET(v,u,1);
	  if (regret[0] > threshold[0] ||
	      regret[1] > threshold[1] ||
	      (cost_has_hole && regret[0]+regret[1]>0 &&
	       dvar_compare_interval_l(DVAR(0),
				       pdata->optcost[0] + regret[0],
				       - pdata->optcost[1] - regret[1]) ==
	       FDI_DISJOINT)) {
	    reset_flow(pdata, v, u, 0);
	    reset_flow(pdata, v, u, 1);
	    delta[d++] = vbase + v - 1;
	  }
	}
	if (d-d0==degree)	/* empty domain */
	  ok = FALSE;
      }
    }
    dmax = d;

    if (dmax>0) {
      *change = TRUE;
  
      /* II. XOR Delta into VAR adjacency lists. */
      d = 0;
      while (d<dmax)
	d += gcc_apply_delta(pdata, delta+d, dmax-d, FALSE, TRUE);
  
      /* III. Keysort Delta by ascending VAL vertex. */
      KEYSORT(delta, dmax, int, (int *)pdata->stack, nvals, VALOF, delta2);
  
      /* IV. XOR Delta into VAL adjacency lists. */
      d = 0;
      while (d<dmax)
	d += gcc_apply_delta(pdata, delta2+d, dmax-d, TRUE, TRUE);
    }

    SP_free(delta);
    if (!ok)
      return FALSE;
  }
  
  /* PRUNE COST VAR */
  optcost = pdata->optcost[0];
  if (!dvar_contains_value_l(DVAR(0),optcost)) {
    if (pdata->min_regret[0]==CLPFD_MAXINT)
      return FALSE;
    optcost += pdata->min_regret[0];
  }
  switch (dvar_fix_min_l(DVAR(0),optcost)) {
  case -1:
    return FALSE;
  case 0:
    break;
  default:
    *change = TRUE;
  }

  optcost = -pdata->optcost[1];
  if (!dvar_contains_value_l(DVAR(0),optcost)) {
    if (pdata->min_regret[1]==CLPFD_MAXINT)
      return FALSE;
    optcost -= pdata->min_regret[1];
  }
  switch (dvar_fix_max_l(DVAR(0),optcost)) {
  case -1:
    return FALSE;
  case 0:
    break;
  default:
    *change = TRUE;
  }

  return TRUE;
}


/* Out-of-kilter algo.  [Ahuja et al., p.. 326-331]
 * Arcs can have nonzero lower bounds.
 */
static SP_BOOL
costgcc_in_kilter(Wam wam,
			 struct gcc_graph *pdata,
			 SP_integer accum,
			 int b)
{
  int nvars = pdata->nvars;
  int nvals = pdata->nvals;
  int n = pdata->nnodes;
  int nvaltargets = pdata->nvaltargets;
  int nvartargets = pdata->nvartargets;
  SP_integer _cost;
  int *arcs = Malloc(nvals*(nvars+2), int);
  int ai = 0;
  int aimax;
  int i, j;
  
  /* I. Set duals of all var nodes, making all (VAR,VAL) arcs in-kilter.
     TENTATIVE. */

  for (i=0; i<nvartargets; i++) {
    VERTEX v1 = TVAR(i);
    VERTEX v2 = MATE(v1,b);
    SP_integer cost = COST(v2,v1,b);
    
    DUAL(v1,b) = DUAL(v2,b) - cost;
    accum += cost;
  }

  /* II. Collect all out-of-kilter arcs; compute total cost. */
  
  for (i=0; i<nvaltargets; i++) {
    int f;
    SP_integer rescost;
    VERTEX v1 = TVAL(i);
    int deg = OUT_DEGREE(v1);
    for (j=0; j<deg; j++) {
      VERTEX v2 = NEIGH(v1,j);
      rescost = reduced_cost(pdata,v1,v2,b);
      if (MATE(v2,b)!=v1 && rescost<0) /* residual arc (v1,v2) out of kilter */
	arcs[ai++] = v1*n + v2;
      /* see I. above *
      else if (MATE(v2,b)==v1 && rescost>0) * residual arc (v2,v1) out of kilter *
        arcs[ai++] = v2*n + v1;
      */
    }
    f = SFLOW(v1,b);
    rescost = reduced_cost(pdata,0,v1,b);
    if (f<CAPACITY(v1) && rescost<0) /* residual arc (0,v1) out of kilter */
      arcs[ai++] = v1;
    if (f>LIMIT(v1) && rescost>0) /* residual arc (v1,0) out of kilter */
      arcs[ai++] = v1*n;
  }
  aimax = ai;

  /* III. Make all arcs in-kilter. */

  for (ai=0; ai<aimax; ai++) {
    VERTEX v1 = arcs[ai]/n;
    VERTEX v2 = arcs[ai]%n;
    SP_integer v1dist;

    while (res(pdata,v1,v2,b)>0 && reduced_cost(pdata,v1,v2,b)<0) {
      /* find shortest distances from v2 to all nodes,
	 pretending that (v2,v1) does not exist. */
      INVALIDATE_DIST;
      /* error on Ahuja et al?  No need to zap arc (v2,v1)?
      VALIDATE_DIST_ROW(v2,b);
      pdata->distrow = pdata->dist + n*v2;
      pdata->distrow[v1] = CLPFD_MAXINT;
      */
      if (opt_shortest_paths(pdata, v2, v1, b) == -1)
				/* TENTATIVE PATCH */
	break;			/* (v1,v2) is infeasible---ignore it */
      /* Update duals. Note: dual(v1) remains unchanged. */
      v1dist = pdata->distrow[v1];
      DUAL(v2,b) += v1dist;
      for (i=pdata->heapsize+1; i<pdata->heapmax; i++) {
	VERTEX v = pdata->heap[i];
	DUAL(v,b) -= pdata->distrow[v] - v1dist;
      }
      if (reduced_cost(pdata,v1,v2,b) < 0) {
	VERTEX u1=v1, u2=v2;
	if (u1>0 && u1<=nvals) {
	  /* ensure that we start with a flow decrement,
	     so as to avoid inbound overflow to VAR node */
	  DAD(u2) = u1;
	  u2 = u1;
	  u1 = DAD(u1);
	}	  
	/* augment 1 unit of flow along path */
	for (; u1 >= 0; u2 = u1, u1 = DAD(u2)) {
	  DAD(u2) = -1;
	  INCFLOW(u1,u2,b);
	  if (u2>nvals)		/* add cost */
	    accum += COST(u1,u2,b);
	  else if (u1>nvals)	/* subtract cost */
	    accum -= COST(u2,u1,b);
	}
      }
    }
  }
  pdata->optcost[b] = accum;
  SP_free(arcs);
  return TRUE;
}

static SP_BOOL 
costgcc_filter(Wam wam,
		      struct gcc_graph *pdata,
		      SP_integer accum,
		      SP_BOOL incremental,
		      int b,
		      SP_BOOL *change)
{
  maintain_capacities(pdata,b);
  return (feasible_flow(pdata,b,change) &&
	  maximum_flow(pdata,b,change) == pdata->nvars &&
	  ((incremental && !*change) || costgcc_in_kilter(wam, pdata,accum,b)) &&
	  costgcc_mark(wam, pdata,b));
}


static SP_BOOL
gcc_filter(Wam wam,
		  struct gcc_graph *pdata,
		  SP_BOOL *change)
{
  maintain_capacities(pdata,0);
  if (!feasible_flow(pdata,0,change) ||
      maximum_flow(pdata,0,change) != pdata->nvars)
    return FALSE;
  if (sccs(wam, pdata)>1)
    compress_edges(wam, pdata);

  return TRUE;
}

#define GCC_ARITY 9

/*
  '$fd_gcc'(+State0, -State, -Actions) :-
  State0 is f(NVars,NVals,Vars,Vals,Flag,CostParm,AccumCost,Handle,Stamp).
  Vals is a keysorted list of Val-CountVar terms.  
  It is assumed, that the domains of Vars
  have been restricted to the available values.

  State similarly,
  Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_gcc(Wam wam,
	      SP_term_ref State0,
	      SP_term_ref State,
	      SP_term_ref Actions)
{
  int i, ent = -1;
  SP_integer state_stamp;
  TAGGED handle, tlvec;
  struct gcc_graph *pdata;
  SP_BOOL committed, incremental, ok;
  int flags;
  SP_integer accum;
  char *ptr;
  int nnodes, nvals, nvars, nvartargets, nvaltargets;

  (void)State0;
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);
  DerefArg(tlvec, X(0), 5);	/* get flags */
  flags = GetSmall_int(tlvec);
  accum = GetSmall_int(CTagToArg(X(0),7));

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    fd.gdata = pdata = Pdata(struct gcc_graph, handle);
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
    fd.gdata = pdata = gcc_alloc(wam, novars, novals, flags, handle);
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
    DerefArg(telt, X(0), 6);
    if (!IsAtomic(telt)) {
      fd_get_var_and_attr(telt,RefAttr(0));
      dvar_init(DVAR(0), RefAttr(0), RefVar(0));
      DerefArg(telt,telt,3);	/* cost matrix */
    }
    gcc_init(wam, pdata, tlvec, telt);
    CTagToArg(X(0),3) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),4) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),6) = atom_nil; /* [MC] 3.12: free for GC */
  }

                                /* RESUME HERE */
  nnodes = pdata->nnodes;
  nvals = pdata->nvals;
  nvars = pdata->nvars;
  ptr = (char *)Malloc(2*nnodes,VERTEX);
  pdata->stack = (VERTEX *)ptr; /* volatile */
  ptr += nnodes*sizeof(VERTEX);
  pdata->vertex.dad = (VERTEX *)ptr; /* volatile */
  ptr += nnodes*sizeof(VERTEX);
  (void)ptr; /* suppress clang warning about value stored is never read. */
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
  for (i = 0; i < nvaltargets; ++i) {
    VERTEX vertex = TVAL(i);
    VISITED(vertex) = 0;	/* used in gcc_refresh? */
    dvar_refresh(DVAR(vertex));
  }
  KEYSORT(pdata->valtarget, nvaltargets, VERTEX, pdata->stack,
	  nvals, VALKEY, pdata->activeset+1);
  for (i = 0; i < nvartargets; ++i) {
    VERTEX vertex = TVAR(i);
    VISITED(vertex) = 0;	/* used in gcc_refresh? */
    dvar_refresh(DVAR(vertex));
  }
  KEYSORT(pdata->vartarget, nvartargets, VERTEX, pdata->stack,
	  nvars, VARKEY, pdata->activeset+nvaltargets+1);
  gcc_refresh(wam, pdata,flags,incremental);
  pdata->stamp = state_stamp+1;

  ok = TRUE;
  if (flags) {			/* cost-based algo */
    SP_BOOL change;
    
    dvar_refresh(DVAR(0));
    pdata->dist = Malloc(nnodes*nnodes,SP_integer);
    pdata->heap = Malloc(nnodes,VERTEX);
    pdata->vheap = Malloc(nnodes,int);
    do {
      change = FALSE;
      if (!costgcc_filter(wam, pdata,accum,incremental,0,&change) ||
	  !costgcc_filter(wam, pdata,-accum,incremental,1,&change) ||
	  !costgcc_prune(wam, pdata,&change) ||
	  !adjust_count_bounds(pdata,flags,&change)
	  )
	ok = FALSE;
    } while (ok && change);
    SP_free(pdata->dist);
    SP_free(pdata->heap);
    SP_free(pdata->vheap);
  } else {			/* vanilla algo */
    SP_BOOL change;
    
    do {
      change = FALSE;
      if (!gcc_filter(wam, pdata,&change) ||
	  !adjust_count_bounds(pdata,flags,&change))
	ok = FALSE;
    } while (ok && change);
  }
  if (!ok)
    goto ret;

				/* Compute prunings. */
  ent = 1;			/* Entailed while ground */
  for (i = 0; i < pdata->nvartargets; ++i) {
    VERTEX varvertex = TVAR(i);
    Dvar dv = DVAR(varvertex);
    int live = IN_DEGREE(varvertex);

    if (live > 1)
      ent = 0;
    if (live != dvar_value_count(dv))
      dvar_fix_set(dv, gcc_new_domain(wam, pdata, 1, live, varvertex));
    dvar_pruning_done( dv);
  }
  for (i = 0; i < pdata->nvartargets; ++i) {
    VERTEX varvertex = TVAR(i);
    Dvar dv = DVAR(varvertex);

    dvar_export(dv);
  }
  for (i = 0; i < pdata->nvaltargets; ++i) {
    VERTEX valvertex = TVAL(i);
    Dvar dv = DVAR(valvertex);

    dvar_export(dv);
  }
  if (flags) {
    Dvar dv = DVAR(0);

    dvar_export(dv);
  }

  contract_vals(pdata);		/* must precede contract_vars */
  contract_vars(pdata);

  if (pdata->cost) {
    int nvars_back = GetSmall_int(CTagToArg(X(0),1));
    if (pdata->nvartargets < nvars_back) {
      int nvals = pdata->nvals;
      SP_integer _cost;
    
      for (i=pdata->nvartargets; i<nvars_back; i++) {
	VERTEX vertex = TVAR(i);
	accum += COST(NEIGH(vertex,1),vertex,0);
      }
      CTagToArg(X(0),7) = MakeSmall(accum);
    }
  }

  CTagToArg(X(0),1) = MakeSmall(pdata->nvartargets);
  CTagToArg(X(0),2) = MakeSmall(pdata->nvaltargets);

ret:
  SP_free(pdata->stack);
  if (ent==1) {
    Pfree;
    CTagToArg(X(0),GCC_ARITY-1) = TaggedZero; /* hide state from helper */
  }
  dvar_export_done(wam, Actions, ent);
}

void SPCDECL 
prolog_fd_gcc_helper(Wam wam,
		     SP_term_ref State0,
		     SP_term_ref State,
		     SP_term_ref Actions)
{
  int nvals, ix, ival=0, i;
  TAGGED tstamp, thandle, tval;
  SP_integer lreg, ureg;
  int ent = 1;
  struct gcc_graph *pdata;
  Dvar dv;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  DerefNonvar(X(0));
  RefTerm(State) = X(0);	/* f(Index,Var,StateMutable) */
  tval = fd_deref(wam, CTagToArg(X(0),2));
  if (IsVar(tval)) {
    ent = 0; goto ret;
  }
  ix = GetSmall_int(CTagToArg(X(0),1));
  thandle = RefMutable(CTagToArg(X(0),3));
  DerefArg(tstamp,thandle,GCC_ARITY);
  DerefArg(thandle,thandle,GCC_ARITY-1);
  if (thandle==TaggedZero)
    goto ret;
  pdata = Pdata(struct gcc_graph,thandle);
  if (GetSmall(tstamp)!=pdata->stamp)
    goto ret;
  nvals = pdata->nvals;
  ix += nvals+1;	/* now VAR node */
  if (IN_DEGREE(ix)==1)		/* nothing to do */
    goto ret;
  for (i=0; i<pdata->nvaltargets; i++) {
    ival = TVAL(i);
    if (VAL(ival)==GetSmall(tval))
      break;
  }
  lreg = REGRET(ival,ix,0);
  ureg = REGRET(ival,ix,1);
  if (lreg==CLPFD_MAXINT || ureg==CLPFD_MAXINT) {
    ent = -1; goto ret;
  }
  dv = DVAR(0);
  dvar_refresh(dv);
  if (dvar_fix_interval_l(dv, pdata->optcost[0]+lreg,
			     -pdata->optcost[1]-ureg) == -1) {
    ent = -1; goto ret;
  }
  dvar_pruning_done( dv);
  dvar_export(dv);
 ret:
  dvar_export_done(wam, Actions, ent);
}
