/* Copyright(C) 1999, Swedish Institute of Computer Science */
/***
  Support for all_distinct/[1,2], assignment/[2,3], circuit/[1,2].
  Algorithms:
  Regin '94, Costa '94, Caseau & Laburthe '97, Mehlhorn & Thiel '00, Francis & Stuckey '14.
  ***/

#include "fd.h"
#include "dvars.h"

#define PROPAGATE 0

/* VERTEX should really be int, but ensuring alignment becomes so awkward */
typedef SP_integer VERTEX;

#define EOL ((SP_WORD)-1)

struct all_dist_data {
  void (SPCDECL *destructor)(void*);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  SP_globref refbase;
  SP_integer stamp;			/* increases up to backtracking */
  SP_BOOL density;			/* 2 if vars 1..n, vals 1..n, 1 if |vals|=|vars|, 0 otherwise */
  SP_BOOL reset_density;
  SP_BOOL incremental;
  int pruned;
  int flags;
  int enqueue_code;
  int nrefs;
  int nvars;			/* #kernels + #shells + #ground */
  int nvartargets;		/* #kernels + #shells */
  int nvarkernels;		/* #kernels */
  int nvals;			/* #kernels + #shells + #ground */
  int nvaltargets;		/* #kernels + #shells */
  int nvalkernels;		/* #kernels */
  int scc_component;		/* SCC number */
  int scc_visited;		/* Vertex number during SCC search */
  int scc_index;		/* Stack ptr during SCC search */
  VERTEX back_source;
  TAGGED back_target;
  Dvar dvar;
  VERTEX *vartarget;		/* var vertices: nvars*2 */
  VERTEX *valtarget;		/* val vertices: nvals*2 */
  VERTEX *dist_stack;		/* : 3*nvars+nvals, volatile */
  VERTEX *head;			/* for single_circuit: nvars, volatile */
  VERTEX *tail;			/* for single_circuit: nvars, volatile */
  struct {
    TAGGED *label;		/* The value of the vertex.  1-based: nvars+nvals  */
    VERTEX *mate;		/* Mate of this vertex */
    SP_integer *component;		/* Component number */
    SP_integer *lowlink;		/* Lowlink for Stuckey et al. */
    SP_integer *size;			/* _Saved_ domain size */
    SP_integer *visited;		/* VAL nodes: Nth visited in SCC search */
				/* VAR nodes: visited in augment_path */
  } vertex;			/* : nvars+nvals */
  struct {
    int size;
    SP_integer *item;
    VERTEX *vertex;
    TAGGED *fdset;
    TAGGED *key;
  } heap;
  /* additions for cost_based pruning */
  SP_integer *cost;			/* matrix if cost-based, else NULL */
  struct {
    /* the following fields are valid while descending a branch */
    SP_integer costbase;		/* cost of all ground items */
    int ntargets;		/* rc indexes range in [0,ntargets-1] */
    int *index_var;		/* rc index to vertex no. */
    int *var_index;		/* vertex no. to rc index */
    SP_integer *lbdual[2];		/* min. cost dual soln. */
    SP_integer *ubdual[2];		/* max. cost dual soln. */
    int  *lbprimal[2];		/* min. cost primal soln. */
    int  *ubprimal[2];		/* max. cost primal soln. */
    Dvar costvar;
    SP_integer *rcost[2];		/* 0=min, 1=max */
    SP_integer optcost[2];		/* 0=min, 1=max */
    /* the following fields are volatile */
    int *label[2];		/* for finding aug. path */
    SP_integer *cost;
    SP_integer *dist;
    SP_integer *distrow;		/* points at k'th row of dist */
    int *queue;
    int *index;
    int *heap;
    int *vheap;
    int heapsize;
    SP_integer min_regret[2];		/* 0=min, 1=max */
    SP_integer max_regret[2];		/* 0=min, 1=max */
    SP_integer threshold[2];		/* 0=min, 1=max */
  } rc;
};

#define DVAR(V) (pdata->dvar+(V))
#define LABEL(V) (pdata->vertex.label[V])
#define SIZE(V) (pdata->vertex.size[V])
#define MATE(V) (pdata->vertex.mate[V])
#define LIVE(V) dvar_value_count(DVAR(V))
#define GROUND(V) dvar_is_integer(DVAR(V))
#define COMPONENT(V) (pdata->vertex.component[V])
#define LOWLINK(V) (pdata->vertex.lowlink[V])
#define VISITED(V) (pdata->vertex.visited[V])
#define RefAttr(V) (pdata->refbase + 2*(V))
#define RefVar(V) (pdata->refbase + 2*(V) + 1)
#define TVAR(V) (pdata->vartarget[V])
#define TVAL(V) (pdata->valtarget[V])

/* Invariant: VISITED(V)>EOL iff V is a target (not yet ground and checked) */

static SP_BOOL cost_filter(Wam wam,
			   struct all_dist_data *pdata,
			   SP_integer costbase,
			   int flags);

static void SPCDECL all_distinct_destructor(void *pdata_v)
{
  struct all_dist_data *pdata = (struct all_dist_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

static DAEMON_RC SPCDECL 
all_distinct_daemon(Wam wam,
		    void *vdata,
		    SP_globref attr_ref,
		    TAGGED *global)
{
  struct all_dist_data *pdata = (struct all_dist_data *)vdata;
  TAGGED tstate, tsize, tdone;
  (void)wam;

  if (pdata->flags)
    return DAEMON_NOFIX;
  tstate = RefMutable(CTagToArg(*global,1));
  DerefArg(tdone,tstate,2);
  tsize = RefGlob(attr_ref);
  DerefAttribute(tsize,tsize); /* get dom/4 term */
  tsize = DomainSize(tsize);
  if (IsSmall(tsize) &&
      GetSmall(tsize) < pdata->nvars-GetSmall(tdone)) /* CLAIM: otherwise, can't prune */
    return DAEMON_NOFIX;
  return DAEMON_FIX;
}

static int cmp_asc_label(Wam wam,
			 VERTEX *t1, VERTEX *t2)
{
  struct all_dist_data *pdata = fd.gdata;
  TAGGED val1 = LABEL(*t1);
  TAGGED val2 = LABEL(*t2);

  return TCMP(val1,val2);
}

#define QType VERTEX
#define QCmp  cmp_asc_label
#define QSort qsort_asc_label
#include "qsort.ic"

#if PROPAGATE
static INLINE void clear_mate(struct all_dist_data *pdata, VERTEX vertex)
{
  VERTEX mate = MATE(vertex);

  if (mate>EOL)
    MATE(mate) = MATE(vertex) = EOL;
}
#endif	/* PROPAGATE */

#define set_mates(n1,n2) MATE(n1) = (n2), MATE(n2) = (n1)

#define exposed(vertex) (MATE(vertex) == EOL)

#if PROPAGATE
static int remove_neighbor(Wam wam,
			   struct all_dist_data *pdata,
			   VERTEX u, VERTEX v)
{
  TAGGED tv = LABEL(v);
  Dvar udv = DVAR(u);

  if (dvar_prune_value_t(udv,tv)) {
    pdata->pruned++;
  }

  return LIVE(u);
}
#endif

static VERTEX neighbor_of(Wam wam,
			  struct all_dist_data *pdata,				 
			  TAGGED tval,
			  VERTEX *ptarget,
			  VERTEX u)
{
  (void)wam;
  if (u>=pdata->nvars) {
    return GetSmall(tval)-1;
  } else if (pdata->density>1) {
    int j = GetSmall_int(tval)-1;
    *ptarget = j;
    return pdata->nvars+j;
  } else {
    /* dichotomic search for j such that LABEL(TVAL(j))==tval */
    int mid, sup;
    int j = (int)(*ptarget+1);
    
    sup = pdata->nvalkernels;
    while (j<sup) {
      mid = (j+sup)>>1;
      if (Tlt(LABEL(TVAL(mid)),tval)) /* ORDER */
	j = mid+1;
      else
	sup = mid;
    }
    *ptarget = j;
    return TVAL(j);
  }
}

#if PROPAGATE
static SP_BOOL propagate_value(Wam wam,
			       struct all_dist_data *pdata,
			       int nvars, int nvals)
{/* Performs all possible pruning by propagating singletons.
    Also, build matching for such singletons. */
  int i;
  int top=0;
  VERTEX *dist_stack = pdata->dist_stack;
  DVITER it;

  for (i=nvars-1; i>=0; i--) {
    VERTEX v = TVAR(i);
     
    if (GROUND(v))
      dist_stack[top++] = v;
  }
  for (i=nvals-1; i>=0; i--) {
    VERTEX v = TVAL(i);
       
    if (GROUND(v))
      dist_stack[top++] = v;
  }

  while (top>0) {
    VERTEX vertex = dist_stack[--top];
    TAGGED tv = LABEL(vertex);
    VERTEX valtarget = -1;
    VERTEX valvertex = neighbor_of(wam, pdata,dvar_min_t(DVAR(vertex)), &valtarget, vertex);
    Dvar rdv = DVAR(valvertex);

    if (MATE(vertex) != valvertex) {
      clear_mate(pdata,vertex);
      clear_mate(pdata,valvertex);
      set_mates(vertex,valvertex); /* one matching done */
    }
    if (!GROUND(valvertex)) {
      valtarget = -1;
      dviter_init(&it, rdv);
      while (!dviter_empty(&it)) {
	VERTEX lvertex = neighbor_of(wam, pdata,dviter_next_value_t(&it), &valtarget, valvertex);
	
	if (lvertex!=vertex)
	  switch (remove_neighbor(wam, pdata,lvertex,valvertex)) {
	  case 0: return FALSE;
	  case 1: dist_stack[top++] = lvertex;
	  }
      }
      dvar_fix_value_t(rdv, tv);
      pdata->pruned++;
    }
  }

  return TRUE;
}
#endif

static SP_BOOL augment_path(Wam wam,
				   struct all_dist_data *pdata,
				   VERTEX vertex0,
				   int now)
   /* BFS algorithm for finding an augmenting path. 
      If successful, updates the graph to reflect the new matching.
      Return TRUE or FALSE, depending on whether an augmenting path is
      found.
      */
{
  int p=0, q=0;
  VERTEX *vstack = pdata->dist_stack;

  vstack[q++] = vertex0;
  while (p<q) {
    DVITER it;
    VERTEX newvertex, neighbor=0;
    VERTEX vertex = vstack[p++];
    VERTEX valtarget = -1;
    dviter_init(&it, DVAR(vertex));
    while (!dviter_empty(&it)) {
      neighbor = neighbor_of(wam, pdata,dviter_next_value_t(&it), &valtarget, vertex);
      newvertex = MATE(neighbor);
      if (newvertex==EOL) {
	for (;;) {
	  newvertex = MATE(vertex);
	  set_mates(vertex,neighbor);
	  if (newvertex==EOL)
	    return TRUE;
	  neighbor = newvertex;
	  vertex = vstack[newvertex]; /* FATHER(newvertex) */
	}
      } else if (VISITED(newvertex)!=now) {
	VISITED(newvertex) = now;
	vstack[neighbor] = vertex; /* FATHER(neighbor) */
	vstack[q++] = newvertex;
      }
    }
  }
  return FALSE;
}



static SP_BOOL augment(Wam wam,
			      struct all_dist_data *pdata,
			      int nvars)
   /* This function returns TRUE and a new graph with a maximum matching
      if there is such a matching. Returns FALSE otherwise. The maximum
      matching is found by repeatedly finding augmenting paths in the
      graph until all variable vertices are matched.  */
{
  int i, first_exp=0;
  VERTEX vertex;
  VERTEX rvertex;
  TAGGED seed=0;		/* avoid false alarm */
				/* greedy phase */
  first_exp = nvars;
  for (i=0; i<nvars; i++) {
    VERTEX valtarget = -1;
    vertex = TVAR(i);
    if (exposed(vertex)) {
      if (first_exp>i) {
	first_exp = i;
	seed = dvar_min_t(DVAR(vertex));
	rvertex = neighbor_of(wam, pdata,seed,&valtarget,vertex);
	if (exposed(rvertex))
	  set_mates(vertex,rvertex);
      } else {
	TAGGED seed2 = dvar_successor_t(DVAR(vertex),seed);
	if (TagIsSmall(seed2)) {
	  seed = seed2;
	  rvertex = neighbor_of(wam, pdata,seed,&valtarget,vertex);
	  if (exposed(rvertex))
	    set_mates(vertex,rvertex);
	}
      }
    }
  }
				/* augmenting phase */
  for (i=first_exp; i<nvars; i++) {
    vertex = TVAR(i);
    if (exposed(vertex)) {
      if (!augment_path(wam, pdata,vertex,i+1))
	return FALSE;
    }
  }
  return TRUE;
}


/* Returns number of SCCs found. */
static int
fast_find_sccs(Wam wam, struct all_dist_data *pdata, int sccid)
{
  int sccid0 = sccid;
  int j, top1, top2;
  int numberx = pdata->nvars;
  int numbery = pdata->nvalkernels;
  VERTEX aftery = numberx + pdata->nvals;
  struct {
    VERTEX *node;
    VERTEX *root;
    VERTEX *rightmost;
    VERTEX *maxx;
  } stk;

  /* init */
  stk.node      = pdata->dist_stack;
  stk.root      = stk.node+numberx;
  stk.rightmost = stk.root+numberx;
  stk.maxx      = stk.rightmost+numberx;
  j=0; top1=0; top2=0;
  while (j-top1<numbery) {
    VERTEX y = j<numbery ? TVAL(j) : aftery;
    VERTEX x = j<numbery ? MATE(y) : numberx;
    VERTEX valtarget1 = -1;
    VERTEX valtarget2 = -1;
    if (j<numbery && (x==EOL || VISITED(y))) /* node to be ignored */
      j++;
    else if (top2==0) {	/* start 1st component */
      stk.node[0] = y;
      stk.root[0] = y;
      stk.rightmost[0] = y;
      stk.maxx[0] = neighbor_of(wam, pdata,dvar_max_t(DVAR(x)), &valtarget2, x); /* SHAVE? */
      top1 = top2 = 1;
      j++;
    } else if (stk.maxx[top2-1]>=y) { /* start/extend component */
      VERTEX min1 = neighbor_of(wam, pdata,dvar_min_t(DVAR(x)), &valtarget1, x);
      VERTEX max1 = neighbor_of(wam, pdata,dvar_max_t(DVAR(x)), &valtarget2, x);
      
      stk.node[top1++] = y;
      stk.root[top2] = y;
      while (top2>0 && min1<=stk.rightmost[top2-1]) { /* merge components */
	top2--;
	if (max1<stk.maxx[top2])
	  max1 = stk.maxx[top2];
      }
      stk.rightmost[top2] = y;
      stk.maxx[top2++] = max1; /* SHAVE? */
      j++;
    } else {			/* component done */
      VERTEX root1 = stk.root[--top2];
      VERTEX x1, y1;

      sccid++;
      do {
	y1 = stk.node[--top1];
	x1 = MATE(y1);
	COMPONENT(y1) = sccid;
	COMPONENT(x1) = sccid;
      } while (y1>root1);
    }
  }
  return sccid - sccid0;
}

static int visit(Wam wam,
			struct all_dist_data *pdata,
			VERTEX vertex, int *ix1, int *ix2)
   /* A recursive function that finds strongly connected components
      [Tarjan'72]. Code taken roughly from [Sedgewick, Algorithms in C,
      page 482].
      One big difference is that we only step on value vertices, i.e. we
      jump directly from variable vertices using the mate ptr.
      */
{
  VERTEX newvertex=EOL, mate;
   int m, min, scc;
   VERTEX *dist_stack = pdata->dist_stack;
   DVITER it;
   VERTEX valtarget = -1;

   VISITED(vertex) = ++pdata->scc_visited;		/* Mark vertex as the id'th visited */
   min = pdata->scc_visited;
   dist_stack[pdata->scc_index++] = vertex;
   mate = MATE(vertex);
   dviter_init(&it, DVAR(mate));
   while (!dviter_empty(&it)) {
     newvertex = neighbor_of(wam, pdata,dviter_next_value_t(&it),&valtarget, mate);
     if (newvertex != vertex) {
       m = (VISITED(newvertex) == 0) ? 
	 visit(wam, pdata,newvertex,ix1,ix2) : (int)VISITED(newvertex);
       if (m < min)
	 min = m;
     }
   }
   if (min == VISITED(vertex)) {
     scc = ++pdata->scc_component;
     do {
       VERTEX mate;
       /* Each descendant on the dist_stack is part of this vertex's SCC. */
       newvertex = dist_stack[--pdata->scc_index];
       mate = MATE(newvertex);
       COMPONENT(newvertex) = scc;
       COMPONENT(mate) = scc;
       VISITED(newvertex)=0xffffff; /* High value, so that
				       this vertex will be ignored
				       in the future search. */
     } while (newvertex != vertex);
   }
   return min;
}


static SP_BOOL single_scc(Wam wam,
			  struct all_dist_data *pdata,
			  TAGGED unvisited)
{
  TAGGED y, preds, fdset;
  VERTEX yvertex;
  int cmp = FDI_DISJOINT;
  VERTEX valtarget;

  y = fd_min(unvisited);
  preds = fd_interval(wam,y,y);
  valtarget = -1;
  yvertex = neighbor_of(wam, pdata,y,&valtarget, 0);
  unvisited = fd_delete(wam,unvisited,y);
  fdset = dvar_set(DVAR(MATE(yvertex)));
  while (unvisited!=EmptySet) {
    y = fd_intersection_min(unvisited,fdset);
    if (y==ERRORTAG)
      return FALSE;
    unvisited = fd_delete(wam,unvisited,y);
    valtarget = -1;
    yvertex = neighbor_of(wam, pdata,y,&valtarget, 0);
    fdset = dvar_set(DVAR(MATE(yvertex)));
    cmp = fd_compare(fdset,preds);
    if (cmp!=FDI_DISJOINT)
      preds = fd_union_interval(wam,preds,y,y);
  }
  return (cmp != FDI_DISJOINT);
}

/* Returns #non-singleton components. */
static int find_sccs(Wam wam,
		     struct all_dist_data *pdata,
		     int sccs0)
   /* Marks all edges belonging to any strongly connected component in
      the graph. The strongly connected components are calculated using
      Tarjan's depth-first search based algorithm from 1972. */
{
   int i, ix1, ix2;
   FDCONS cons;
   VERTEX vertex, mate;
   Dvar dv;
   int nvars = pdata->nvarkernels;
   int nvals = pdata->nvalkernels;
   int sccs = sccs0;
   SP_integer mindeg = CLPFD_MAXINT;
   SP_integer degree;
   int nnonground = 0;
     
   fdcons_init(&cons);
   for (i=0; i<nvals; i++) {
     vertex = TVAL(i);
     mate = MATE(vertex);
     degree = LIVE(mate);
     if (degree==1) {
       VISITED(vertex) = 0xffffff; /* ignore in SCC search */
       COMPONENT(mate) = ++sccs; /* singleton SCC */
       COMPONENT(vertex) = sccs;
     } else {
       fdcons_add(wam, &cons,LABEL(vertex)); /* ORDER */
       nnonground++;
       if (mindeg > degree)
	 mindeg = degree;
     }
   }
   if (nnonground==0)		/* all vars fixed */
     return sccs - sccs0;
   if (mindeg==nnonground)
     goto single;
   for (i=0; i<nvars; i++) {
     vertex = TVAR(i);
     mate = MATE(vertex);
     dv = DVAR(vertex);
     if (!VISITED(mate)) {
       TAGGED holes =
	 fd_interval_subtract(wam,dvar_min_t(dv), dvar_max_t(dv), dvar_set(dv));

       if (holes != EmptySet &&
	   fd_compare(holes,fdcons_set(&cons))!=FDI_DISJOINT)
	 goto not_convex;
     }
   }
   if (1) {
     sccs += fast_find_sccs(wam, pdata,sccs/* latest used */);
   } else
 not_convex:
   if (!single_scc(wam, pdata,fdcons_set(&cons))) {
     pdata->scc_component = sccs;
     pdata->scc_visited = 0;
     pdata->scc_index = 0;
     ix1 = ix2 = 0;
     for (i=0; i<nvars; i++) {
       vertex=MATE(TVAR(i));
       if (VISITED(vertex) == 0)
	 visit(wam, pdata,vertex,&ix1,&ix2);
     }
     sccs = pdata->scc_component;
   } else {
   single:
     sccs++;
     for (i=nvars-1; i>=0; i--) {
       vertex = TVAR(i);
       if (COMPONENT(vertex)==0) {
	 COMPONENT(vertex) = sccs;
	 COMPONENT(MATE(vertex)) = sccs;
       }
     }
   }
   return sccs - sccs0;
}


static void compress_edges(Wam wam,
			   struct all_dist_data *pdata,
			   VERTEX *vals,
			   int nvals,
			   int nscc)
{
  int i=0, j=0;
  FDCONS cons;
  VERTEX *sortarr = pdata->dist_stack;
  SP_integer *keyarr = sortarr + nvals;
  
  KEYSORT(vals, (int)nvals, VERTEX, keyarr, nscc, COMPONENT, sortarr);

  while (j<nvals) {
    int comp = (int)COMPONENT(sortarr[j]);
    fdcons_init(&cons);
    for (; j<nvals; j++) {
      VERTEX y = sortarr[j];
      int newcomp = (int)COMPONENT(y);
      if (comp!=newcomp)
	break;
      fdcons_add(wam, &cons,LABEL(y));
    }
    for (; i<j; i++) {
      VERTEX x = MATE(sortarr[i]);
      if (x>EOL && dvar_fix_set(DVAR(x), fdcons_set(&cons)))
	pdata->pruned++;
    }
  }
}

/* slight overkill, the vars do not need to be ordered */
static void 
all_dist_partition_vars(struct all_dist_data *pdata)
{
  int imax = pdata->nvarkernels;
  int n = pdata->nvars;
  VERTEX *target = pdata->vartarget;
  int i, j, k;
    
  /* separate into kernel (0..) and nonkernel (n..) */
  for (i=0, j=0, k=n; i<imax; i++) {
    VERTEX current = target[i];
    if (!VISITED(current))
      target[j++] = current;
    else
      target[k++] = current;
  }
  if (i==j)
    return;
  
  pdata->nvarkernels = j;

  while (n<k)
    target[j++] = target[n++];
}

/* the targets need to be ordered (see ORDER comments) */
static void 
all_dist_partition_vals(struct all_dist_data *pdata)
{
  int imax = pdata->nvalkernels;
  int n = pdata->nvals;
  VERTEX *target = pdata->valtarget;
  int i, j, k;
    
  /* separate into kernel (0..) and nonkernel (n..) */
  for (i=0, j=0, k=n; i<imax; i++) {
    VERTEX current = target[i];
    VERTEX mate = MATE(current);
    if (mate>EOL && !VISITED(mate))
      target[j++] = current;
    else
      target[k++] = current;
  }
  if (i==j)
    return;
  
  pdata->nvalkernels = j;

  while (n<k)
    target[j++] = target[n++];
}


/* Partition pdata->vartarget into "kernel" and "shell" variables.
 * Initialize the "kernel" value nodes.
 * There are two rules for eliminating kernel variables.
 * (a) \exists x : #dom(x) > #Kernels ==> eliminate x.
 * (b) \exists Y,Z : Y \cup Z = #Kernels,
 *                   Y \cap Z = \emptyset,
 *                   #(dom(Y) \ dom(Z)) > #Y ==> eliminate all in Y.
 * Must compute pdata->nvarkernels, pdata->nvalkernels;
 */

static SP_BOOL all_dist_kernel(Wam wam,
			       struct all_dist_data *pdata)
{
  FDITER it;
  SP_BOOL change = TRUE;
  TAGGED vals, *h;
  int i, j=0;

  pdata->nvarkernels = pdata->nvartargets;
  while (change) {
    change = FALSE;
    for (i=pdata->nvarkernels-1; i>=0; i--) {
      VERTEX v = TVAR(i);
      Dvar dv = DVAR(v);
      if (!fd_countable(dvar_set(dv)) || LIVE(v) > pdata->nvarkernels)
	VISITED(v) = change = 1;
      else
	VISITED(v) = 0;
    }
    all_dist_partition_vars(pdata);
  }
  if (pdata->nvarkernels>0) {
    for (i=pdata->nvarkernels-1; i>=0; i--) {
      VERTEX v = TVAR(i);
      j += (int)dvar_interval_count(DVAR(v));
    }
    NumstackAlloc(4*j,h);
    for (i=pdata->nvarkernels-1; i>=0; i--) {
      VERTEX v = TVAR(i);
      TAGGED d = dvar_set(DVAR(v));
      while (d!=EmptySet) {
	TAGGED r = CTagToCar(d);
	d = CTagToCdr(d);
	h[0] = MakeList(h+2);
	h[1] = MakeList(h+4);
	h[2] = RangeMin(r);
	h[3] = RangeMax(r);
	h += 4;
      }
    }
    h[-3] = atom_nil;
    vals = fd_union_sort(MakeList(h - 4*j));
#if 0
    /* Proposition (det. experimentally):
       #dom(x) > (#Kernels^2 - #values)/(#Kernels-1) ==> elimitate x */
    {
      int maxdeg = 0;
      int nvals = fd_size(vals);
      BOOL cond;
      for (i=pdata->nvarkernels-1; i>=0; i--)
	if (maxdeg < LIVE(TVAR(i)))
	  maxdeg = LIVE(TVAR(i));
      cond = (pdata->nvarkernels>1 && maxdeg > (pdata->nvarkernels*pdata->nvarkernels-nvals)/(pdata->nvarkernels-1));
      printf("# maxdeg=%" SPRIdINTEGER " #kernels=%d #values=%" SPRIdINTEGER " can eliminate=%s\n", (SP_integer)maxdeg, (int)pdata->nvarkernels, (SP_integer)nvals, cond ? "TRUE" : "FALSE");
    }
#endif
    fditer_init(&it, vals);
    i = 0;
    j = pdata->nvars;
    while (!fditer_empty(&it)) {
      TAGGED t = fditer_next(&it);
      LABEL(j) = t;
      MATE(j) = EOL;
      TVAL(i++) = j++;
    }
  } else {
    i = 0;
  }
  pdata->nvalkernels = i;
  pdata->nvaltargets = i;
  if (i==pdata->nvartargets && i==pdata->nvarkernels)
    pdata->density = 1;
  /* validate kernel matching */
  for (i=pdata->nvarkernels-1; i>=0; i--) {
    VERTEX v = TVAR(i);
    VERTEX mate = MATE(v);
    if (mate>EOL &&
	mate<j &&
	MATE(mate)==EOL &&
	dvar_contains_value_t(DVAR(v),LABEL(mate)))
      MATE(mate) = v;
    else
      MATE(v) = EOL;
  }
  return TRUE;
}


static SP_BOOL all_dist_filter(Wam wam,
			       struct all_dist_data *pdata)
{
  int i, j, ncands;
  int slack = pdata->nvalkernels-pdata->nvarkernels;
  int nunreach = pdata->nvarkernels;
  TAGGED scc_values = EmptySet;
  VERTEX vertex;
  
  if (slack<0)
    return FALSE;

  for (i=pdata->nvarkernels-1; i>=0; i--) {
    vertex = TVAR(i);
    VISITED(vertex) = 0;
    COMPONENT(vertex) = 0;
  }
  for (i=pdata->nvalkernels-1; i>=0; i--) {
    vertex = TVAL(i);
    VISITED(vertex) = 0;
    COMPONENT(vertex) = 0;
  }
#if PROPAGATE
  {
    SP_integer rcall;
    do {
      int nint = 0;
      int rc;
      Dvar dv;
      VERTEX rvertex;

      rcall = 0;    
      for (i=0; i<pdata->nvarkernels; i++) {
	VERTEX valtarget = -1;
	vertex = TVAR(i);
	if (GROUND(vertex)) {
	  rvertex = neighbor_of(wam, pdata,dvar_min_t(DVAR(vertex)),&valtarget, vertex);
	  VISITED(vertex) = 1;
	  if (VISITED(rvertex)++ != 0)
	    return FALSE;
	  if (MATE(vertex) != rvertex) {
	    clear_mate(pdata,vertex);
	    clear_mate(pdata,rvertex);
	    set_mates(vertex,rvertex); /* one matching done */
	  }
	  nint++;
	}
      }
      if (nint>0) {
	FDCONS cons;
	fdcons_init(&cons);
	for (i=0; i<pdata->nvalkernels; i++) {
	  vertex = TVAL(i);
	  if (VISITED(vertex)!=0) {
	    VISITED(vertex) = 0;
	    fdcons_add(wam, &cons,LABEL(vertex));
	  }
	}
	for (i=0; i<pdata->nvarkernels; i++) {
	  vertex = TVAR(i);
	  dv = DVAR(vertex);
	  if (VISITED(vertex)!=0) {
	    VISITED(vertex) = 0;
	  } else {
	    rc = dvar_prune_set(dv, fdcons_set(&cons));
	    rcall |= rc;
	    if (rc<0)
	      return FALSE;
	    pdata->pruned++;
	  }
	}
	scc_values = fd_union_dest(scc_values,fdcons_set(&cons));
      }
    } while (rcall & DV_PRUNED_VAL);
  }
#endif
  /* Find maximal matching. */
  if (!augment(wam, pdata,pdata->nvarkernels))
    return FALSE;
  
  for (i=pdata->nvarkernels-1; i>=0; i--) {
    vertex = TVAR(i);
    VISITED(vertex) = 0;
  }
  /* Look for all even length paths starting at an exposed value vertex. */
  if (slack>0) {
    FDCONS reach0;
    TAGGED reachable;
    VERTEX mate;
    VERTEX *cands = pdata->dist_stack;
    VERTEX *src = pdata->valtarget;

    fdcons_init(&reach0);
    for (i=j=0; i<pdata->nvalkernels; i++) {
      vertex = src[i];
      mate = MATE(vertex);
      COMPONENT(vertex) = 0;
      if (exposed(vertex)) {
	COMPONENT(vertex) = 1;
	fdcons_add(wam, &reach0,LABEL(vertex)); /* ORDER */
      } else if (!GROUND(mate)) {
	cands[j++] = vertex;
      }
    }
    ncands = j;
    reachable = EmptySet;
    while (fdcons_set(&reach0) != EmptySet) {
      reachable = fd_union_dest(reachable,fdcons_set(&reach0));
      fdcons_init(&reach0);
      for (i=j=0; i<ncands; i++) {
	vertex = cands[i];
	mate = MATE(vertex);
	if (dvar_compare_set(DVAR(mate),reachable)!=FDI_DISJOINT) {
	  COMPONENT(vertex) = 1;
	  COMPONENT(mate) = 1;
	  VISITED(mate) = 1;
	  fdcons_add(wam, &reach0,LABEL(vertex)); /* ORDER */
	  nunreach--;
	} else
	  cands[j++] = vertex;
      }
      ncands = j;
    }
    if (nunreach>0) {
      all_dist_partition_vars(pdata);
      all_dist_partition_vals(pdata);
    }
  }
  
  /* Look for SCCs */
  /* In the tight case, can't prune if there is only 1 SCC. */
  /* In the loose case, can't prune if there is only 0 SCC. */
  if (nunreach>0) {
    int nscc = find_sccs(wam, pdata,1);
    
    if (nscc>1)
      compress_edges(wam, pdata,pdata->valtarget,pdata->nvalkernels,nscc+2);
    if (pdata->nvarkernels < pdata->nvartargets) {
      FDCONS cons;
      
      fdcons_init(&cons);
      for (i=0; i<pdata->nvalkernels; i++) {
	VERTEX vertex = TVAL(i);
	if (COMPONENT(vertex)>=2)
	  fdcons_add(wam, &cons,LABEL(vertex)); /* ORDER */
      }
      scc_values = fd_union_dest(scc_values,fdcons_set(&cons));
      for (i=pdata->nvarkernels; i<pdata->nvartargets; i++) {
	VERTEX vertex = TVAR(i);
	Dvar dv = DVAR(vertex);
	if (dvar_prune_set(dv,scc_values)!=0) {
	  pdata->pruned++;
	}
      }
    }
  }
  return TRUE;
}

static void ass_init(Wam wam,
		     struct all_dist_data *pdata,
		     TAGGED tmat)
{
  VERTEX vertex = 0;
  int k;
  int n = pdata->nvars;
  TAGGED trow, telt;

  (void)wam;
  pdata->density = 2;

  for (k=0; k<n; k++) {
    LABEL(vertex) = MakeSmall(k+1);
    MATE(vertex) = EOL;
    TVAR(k) = vertex;
    vertex++;
  }
  for (k=0; k<n; k++) {
    LABEL(vertex) = MakeSmall(k+1);
    MATE(vertex) = EOL;
    TVAL(k) = vertex;
    vertex++;
  }
  if (!IsAtomic(tmat)) {
    k = 0;
    while (tmat!=atom_nil) {
      DerefCar(trow,tmat);
      DerefCdr(tmat,tmat);
      while (trow!=atom_nil) {
	DerefCar(telt,trow);
	DerefCdr(trow,trow);
	pdata->cost[k++] = GetSmall(telt);
      }
    }	  
  }			      
}

#if 0

#define EVKEY(I) (event.key[I])

static TAGGED cons_batch(Wam wam, int *buf2, int n)
{
  TAGGED t1, t2;
  int n2;

  switch (n) {
  case 1:
    t1 = MakeSmall(buf2[0]+1);
    return fd_interval(wam,t1,t1);
  case 2:
    t1 = MakeSmall(buf2[0]+1);
    t2 = MakeSmall(buf2[1]+1);
    return fd_pair(wam,t1,t2);
  default:
    n2 = n>>1;
    return fd_union_dest(cons_batch(wam,buf2,n2),cons_batch(wam,buf2+n2,n-n2));
  }
}

/*
  Pruning rules:
  [P1.] Xi in {j | i in dom(Yj)}
  [P2.] Yj in {i | j in dom(Xi)}

  A sweep algorithm:

  Events for rule P1:
  add(i,j) where i=min(I), for each interval I of Yj.
  del(i,j) where i=max(I), for each interval I of Yj.
  query(i), for each i.

  Sort events by (1) i, (2) add<query<del.

  Data structures: FD set.

  Handle add(_,j): add j to FD set.
  Handle del(_,j): delete j from FD set.
  Handle query(i): Infer Xi in FD set.
*/
static SP_BOOL pairing_filter(Wam wam,
				  struct all_dist_data *pdata)
{
  /* volatile storage: */
  int *buf1, *buf2, *buf3, ap, dp, minv[2], maxv[2];
  int nxs = pdata->nvars;
  int nkernels = pdata->nvarkernels;
  int nevents, iter, ev, type, i, k, elt, rvertex;
  Dvar dv;
  TAGGED feasible;
  struct {
    int *key;
    int *type;
    int *elt;
    int *queue;
  } event;

  /* At most nkernels QUERY events,
     at most nkernels*ceiling(nxs/2) ADD events,
     at most nkernels*ceiling(nxs/2) DEL events.
     Hence, at most nkernels*(nxs+2) events.
  */
  nevents = nkernels*(nxs+2);
  buf1 = Malloc(nxs,int);
  buf2 = Malloc(nevents,int);
  buf3 = Malloc(nevents,int);
  event.queue = Malloc(nevents,int);
  event.key = Malloc(nevents,int);
  event.type = Malloc(nevents,int);
  event.elt = Malloc(nevents,int);
  minv[0] = minv[1] = nxs;
  maxv[0] = maxv[1] = -1;
  for (iter=0; iter<2; iter++)
    for (i=0; i<nkernels; i++) {
      elt = iter ? (int)(TVAL(i)-nxs) : (int)TVAR(i);
      if (minv[iter]>elt)
	minv[iter] = elt;
      if (maxv[iter]<elt)
	maxv[iter] = elt;
    }
  for (iter=0; iter<2; iter++) {
    feasible = EmptySet;
    /* Use the low part of buf2 for buffered ADD events. */
    /* Use the high part of buf2 for buffered DEL events. */
    ap = 0;
    ev = 0;
    for (type=0; type<3; type++)
      for (k=0; k<2; k++)
	for (i=0; i<nkernels; i++) {
	  elt = k ? (int)TVAL(i) : (int)TVAR(i);
	  dv = DVAR(elt);
	  if (!dvar_is_integer_first(dv) &&
	      /* If the variable is ground _on entry_, we
		 rely on its being taken care of by pairing/2.
		 We know for a fact that pairing/2 is not
		 in the propagation queue. */
	      (iter ^ k ^ (type==1))) {
	    if (type==1) {
	      event.key[ev] = (!k ? elt : elt-nxs);
	      event.type[ev] = 1;
	      event.elt[ev] = elt;
	      event.queue[ev] = ev;
	      ev++;
	    } else if (pdata->incremental && LIVE(elt)==SIZE(elt)) {
	      if (type==0)
		buf2[ap++] = iter ? elt : elt-nxs;
	    } else {
	      DVITER it;
	      SP_integer min, max;
	      dviter_init(&it, dv);
	      while (!dviter_empty(&it)) {
		dviter_next_interval_l(&it, &min, &max);
		/* ADD event useful if interval overlaps min..max */
		/* DEL event useful if end point inside min..max */
		if (max-1>=minv[iter]) {
		  if (type==0 && min-1<=minv[iter])
		    buf2[ap++] = iter ? elt : elt-nxs;
		  else if (type==0 ? (min-1<=maxv[iter])
			   : (max-1< maxv[iter])) {
		    event.key[ev] = (int)(type==0 ? min-1 : max-1);
		    event.type[ev] = type;
		    event.elt[ev] = elt;
		    event.queue[ev] = ev;
		    ev++;
		  }
		}
	      }
	    }
	  }
	}
    dp = nevents = ev;
    KEYSORT(event.queue, nevents, int,
	    buf1, nxs, EVKEY, buf3);
    for (i=0; i<nevents; i++) {
      event.queue[i] = ev = buf3[i];
    }
    for (i=0; i<nevents; i++) {
      ev = event.queue[i];
      elt = event.elt[ev];
      type = event.type[ev];
      switch (type) {
      case 0:			/* add */
	if (dp<nevents) {
	  feasible = fd_subtract(wam, feasible,cons_batch(wam, buf2+dp, nevents-dp));
	  dp = nevents;
	}
	buf2[ap++] = iter ? elt : elt-nxs;
	break;
      case 2:			/* del */
	if (ap>0) {
	  feasible = fd_union_dest(feasible,cons_batch(wam, buf2, ap));
	  ap = 0;
	}
	buf2[--dp] = iter ? elt : elt-nxs;
	break;
      default:			/* query */
	if (ap>0) {
	  feasible = fd_union_dest(feasible,cons_batch(wam, buf2, ap));
	  ap = 0;
	}
	if (dp<nevents) {
	  feasible = fd_subtract(wam, feasible,cons_batch(wam, buf2+dp, nevents-dp));
	  dp = nevents;
	}
	if (feasible==EmptySet)
	  goto ret;
	dv = DVAR(elt);
	switch (dvar_fix_set(dv,feasible)) {
	case -1:
	  goto ret;
	case 0:
	  break;
	default:
	  pdata->pruned++;
	}
      }
    }
  }
  if (!pdata->cost) {		/* validate mates */
    for (i=0; i<nkernels; i++) {
      elt = (int)TVAR(i);
      if (!pdata->incremental || LIVE(elt)!=SIZE(elt)) {
	rvertex = (int)MATE(elt);
	if (rvertex>EOL && !dvar_contains_value_t(DVAR(elt),LABEL(rvertex)))
	  MATE(elt) = MATE(rvertex) = EOL;
      }
    }
    for (i=0; i<nkernels; i++) {
      elt = (int)TVAL(i);
      if (!pdata->incremental || LIVE(elt)!=SIZE(elt)) {
	rvertex = (int)MATE(elt);
	if (rvertex>EOL && !dvar_contains_value_t(DVAR(elt),LABEL(rvertex)))
	  MATE(elt) = MATE(rvertex) = EOL;
      }
    }
  }
 ret:
  SP_free(buf1);
  SP_free(buf2);
  SP_free(buf3);
  SP_free(event.queue);
  SP_free(event.key);
  SP_free(event.type);
  SP_free(event.elt);
  return (iter==2);
}

#else
/*
  Ensure X->Y edge iff Y->X edge.
*/
static SP_BOOL pairing_filter(Wam wam,
			      struct all_dist_data *pdata)
{
  int nvars = pdata->nvars;
  int nkernels = pdata->nvarkernels;
  int i, valp;

  for (valp=0; valp<2; valp++) {
     for (i=0; i<nkernels; i++) {
       DVITER it;
       FDCONS cons;		/* constructor for pruned values */
       VERTEX vertex = valp ? TVAL(i) : TVAR(i);
       int v2mate = (1-valp)*nvars-1;

       fdcons_init(&cons);
       dviter_init(&it, DVAR(vertex));
       while (!dviter_empty(&it)) {
	 TAGGED tv = dviter_next_value_t(&it);
	 VERTEX mate = GetSmall(tv) + v2mate;
	 if (!dvar_contains_value_t(DVAR(mate), LABEL(vertex)))
	   fdcons_add(wam, &cons, tv);
       }
       if (fdcons_size(&cons)>0) {
	 pdata->pruned++;
	 if (dvar_prune_set(DVAR(vertex),fdcons_set(&cons))<0)
	   return FALSE;
       }
     }
  }
  for (i=0; i<nkernels; i++) {
    VERTEX elt = TVAR(i);
    if (!pdata->incremental || LIVE(elt)!=SIZE(elt)) {
      VERTEX rvertex = MATE(elt);
      if (rvertex>EOL && !dvar_contains_value_t(DVAR(elt),LABEL(rvertex)))
	MATE(elt) = MATE(rvertex) = EOL;
    }
  }
  for (i=0; i<nkernels; i++) {
    VERTEX elt = TVAL(i);
    if (!pdata->incremental || LIVE(elt)!=SIZE(elt)) {
      VERTEX rvertex = MATE(elt);
      if (rvertex>EOL && !dvar_contains_value_t(DVAR(elt),LABEL(rvertex)))
	MATE(elt) = MATE(rvertex) = EOL;
    }
  }
  return TRUE;
}

#endif

static SP_BOOL ass_filter(Wam wam, struct all_dist_data *pdata) 
{
  int i, nscc;
  VERTEX vertex;
  int nvarkernels = pdata->nvarkernels;
  int nvalkernels = pdata->nvalkernels;
  
  for (i=nvarkernels-1; i>=0; i--) {
    vertex = TVAR(i);
    COMPONENT(vertex) = 0;
    VISITED(vertex) = 0;
  }
  for (i=nvalkernels-1; i>=0; i--) {
    vertex = TVAL(i);
    COMPONENT(vertex) = 0;
    VISITED(vertex) = 0;
  }
#if PROPAGATE
  if (!propagate_value(wam, pdata,nvarkernels,nvalkernels))
    return FALSE;
#endif
  /* Find maximal matching. */
  if (!augment(wam, pdata,nvarkernels))
    return FALSE;
  
  /* Look for SCCs */
  /* In the tight case, can't prune if there is only 1 SCC. */
  /* In the loose case, can't prune if there is only 0 SCC. */
  nscc = find_sccs(wam, pdata,1);
  if (nscc > 1) {  
    /* Delete edges */
    compress_edges(wam, pdata,pdata->vartarget,nvarkernels,nscc+2);
    compress_edges(wam, pdata,pdata->valtarget,nvalkernels,nscc+2);
  }
  return TRUE;
}

/* Extra pruning + feasibility check: SCC method from
   K.G. Francis, P.J. Stuckey, "Explaining Circuit Propagation", 
   Constraints (2014) 19:1-29, DOI 10.1007/s10601-013-9148-0
   */
static SP_BOOL scc_explore(Wam wam, struct all_dist_data *pdata, VERTEX v, int start_prev, int end_prev)
{
  DVITER it;
  VERTEX valtarget = -1;
  int nvars = pdata->nvars;
  int nchild = 0;

  VISITED(v) = LOWLINK(v) = pdata->scc_index++;
  dviter_init(&it, DVAR(v));
  while (!dviter_empty(&it)) {
    TAGGED tv = dviter_next_value_t(&it);
    VERTEX child = pdata->tail[neighbor_of(wam, pdata,tv,&valtarget, v)-nvars /*LABEL(neighs[j])-1*/];
    SP_integer visited = VISITED(child);
    if (visited < 0) {	/* haven't explored this yet */
      if (!scc_explore(wam, pdata, child, start_prev, end_prev))
	return FALSE;
      visited = LOWLINK(child);
      nchild++;
      if (nchild==1 && visited >= VISITED(v)) { /* "prune within" rule */
	pdata->pruned++;
	if (dvar_prune_value_t(DVAR(v), tv) < 0)
	  return FALSE;
      }
    } else if (VISITED(child) >= start_prev && VISITED(child) <= end_prev) {
      if (pdata->back_source<0) {
	pdata->back_source = v;	/* =1 back edge */
	pdata->back_target = tv;
      } else {
	pdata->back_source = pdata->nvars; /* >1 back edges */
      }
    } else if (VISITED(child) < start_prev) {
	pdata->pruned++;
	if (dvar_prune_value_t(DVAR(v), tv) < 0)
	  return FALSE;
    }
    LOWLINK(v) = visited < LOWLINK(v) ? visited : LOWLINK(v);
  }
  return (LOWLINK(v) != VISITED(v));
}


static SP_BOOL single_circuit(Wam wam,
			      struct all_dist_data *pdata,
			      VERTEX root)
{
  int nvars = pdata->nvars;
  int start_subtree = 0;
  int end_subtree = 0;
  VERTEX valtarget = -1;
  VERTEX child;
  TAGGED tv;
  DVITER it;
  
  pdata->scc_index = 1;
  VISITED(root) = 0;
  LOWLINK(root) = 0;
  dviter_init(&it, DVAR(root));
  while (!dviter_empty(&it)) {
    tv = dviter_next_value_t(&it);
    child = pdata->tail[neighbor_of(wam, pdata,tv,&valtarget, root)-nvars /*LABEL(neighs[j])-1*/];
    if (VISITED(child) < 0) {	/* haven't explored this yet */
      pdata->back_source = -1;
      if (!scc_explore(wam, pdata, child, start_subtree, end_subtree) || pdata->back_source<0) {
	return FALSE;
      } else if (pdata->back_source<pdata->nvars) {
	if (dvar_fix_value_t(DVAR(pdata->back_source), pdata->back_target))
	  pdata->pruned++;
      }
      start_subtree = end_subtree+1;
      end_subtree = pdata->scc_index-1;
    }
  }
  if (pdata->scc_index != pdata->nvartargets)
    return FALSE;
  if (start_subtree > 1) {
    dviter_init(&it, DVAR(root));
    while (!dviter_empty(&it)) {
      tv = dviter_next_value_t(&it);
      child = pdata->tail[neighbor_of(wam, pdata,tv,&valtarget, root)-nvars /*LABEL(neighs[j])-1*/];
      if (VISITED(child) < start_subtree) {
	dvar_prune_value_t(DVAR(root), tv);
	pdata->pruned++;
      }
    }
  }
  return TRUE;
}

static SP_BOOL
single_circuit_check(Wam wam, struct all_dist_data *pdata)
{
  int nvars = pdata->nvars;
  int maxdegree = 0;
  int mindegree = pdata->nvartargets+1;
  int i;
  VERTEX bestv = EOL;
  VERTEX *head = pdata->head;
  VERTEX *tail = pdata->tail;
  
  /* [Dirac, 1952]: If the graph has N>=3 nodes and the minimum degree
     is >= N/2, then the graph is Hamiltonian. */

  for (i=pdata->nvartargets-1; i>=0; i--) {
    int degree;
    VERTEX v1 = TVAR(i);
    VISITED(v1) = -1;
    degree = (int)LIVE(v1);
    if (degree < mindegree)
      mindegree = degree;
    if (degree > maxdegree) {
      bestv = v1;
      maxdegree = degree;
    }
  }
  /* if (mindegree >= (pdata->nvartargets+1)>>1) */
  /*   return TRUE;		/\* Dirac *\/ */
  for (i=nvars-1; i>=0; i--)
    head[i] = tail[i] = i;
  for (i=pdata->nvartargets; i<nvars; i++) {
    VERTEX v1 = TVAR(i);
    VERTEX v2 = dvar_min_l(DVAR(v1))-1;
    v1 = head[v1];
    v2 = tail[v2];
    tail[v1] = v2;
    head[v2] = v1;
  }
  return single_circuit(wam, pdata,bestv);
}


static struct all_dist_data *
all_dist_alloc(Wam wam,
	       int nvars,
	       int nvals,
	       int nrefs, 
	       TAGGED handle,
	       int flags) /* 0x1 = circuit, 0x2 = cost matrix */
{
  SP_integer total_size = 
    nrefs*sizeof(struct dvar) +
    15*nvars*sizeof(TAGGED) +
    9*nvals*sizeof(TAGGED) +
    (!(flags & 0x1) ? 0 :
     2*nvars*sizeof(VERTEX)) +
    (!(flags & 0x2) ? 0 :
     3*(nvars*nvars)*sizeof(SP_integer) +
     4*nvars*sizeof(SP_integer) +
     8*nvars*sizeof(int));
  struct all_dist_data *pdata =
    Palloc(struct all_dist_data, total_size, handle);
  
  char *ptr;
  int i;
    
  pdata->flags = flags;
    pdata->destructor = all_distinct_destructor;
    pdata->daemon = all_distinct_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(2*nrefs);
    pdata->nrefs = 2*nrefs;
    pdata->nvars = nvars;
    pdata->nvartargets = nvars;
    pdata->nvals = nvals;
    pdata->nvaltargets = nvals;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nrefs*sizeof(struct dvar);
    pdata->dist_stack = (VERTEX *)ptr; /* volatile */
    ptr += (3*nvars+nvals)*sizeof(VERTEX);
    pdata->vartarget = (VERTEX *)ptr; /* array + aux */
    ptr += 2*nvars*sizeof(VERTEX);
    pdata->valtarget = (VERTEX *)ptr; /* array + aux */
    ptr += 2*nvals*sizeof(VERTEX);
    pdata->vertex.label = (TAGGED *)ptr;
    ptr += (nvars+nvals)*sizeof(TAGGED);
    pdata->vertex.mate = (VERTEX *)ptr;
    ptr += (nvars+nvals)*sizeof(VERTEX);
    pdata->vertex.component = (SP_integer *)ptr;
    ptr += (nvars+nvals)*sizeof(SP_integer);
    pdata->vertex.lowlink = (SP_integer *)ptr;
    ptr += (nvars+nvals)*sizeof(SP_integer);
    pdata->vertex.visited = (SP_integer *)ptr;
    ptr += (nvars+nvals)*sizeof(SP_integer);
    pdata->vertex.size = (SP_integer *)ptr;
    ptr += (nvars+nvals)*sizeof(SP_integer);
    pdata->heap.item = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->heap.vertex = (VERTEX *)ptr;
    ptr += nvars*sizeof(VERTEX);
    pdata->heap.fdset = (TAGGED *)ptr;
    ptr += nvars*sizeof(TAGGED);
    pdata->heap.key = (TAGGED *)ptr;
    ptr += nvars*sizeof(TAGGED);
    if (flags & 0x1) {
      pdata->head = (VERTEX *)ptr;
      ptr += nvars*sizeof(VERTEX);
      pdata->tail = (VERTEX *)ptr;
      ptr += nvars*sizeof(VERTEX);
    }
    if (!(flags & 0x2)) {
      pdata->cost = NULL;
    } else {
      pdata->cost = (SP_integer *)ptr;
      ptr += nvars*nvars*sizeof(SP_integer);
      for (i=0; i<2; i++) {
	pdata->rc.rcost[i] = (SP_integer *)ptr;
	ptr += nvars*nvars*sizeof(SP_integer);
	pdata->rc.lbdual[i] = (SP_integer *)ptr;
	ptr += nvars*sizeof(SP_integer);
	pdata->rc.ubdual[i] = (SP_integer *)ptr;
	ptr += nvars*sizeof(SP_integer);
      }
      pdata->rc.index_var = (int *)ptr;
      ptr += 2*nvars*sizeof(int);
      pdata->rc.var_index = (int *)ptr;
      ptr += 2*nvars*sizeof(int);
      for (i=0; i<2; i++) {
	pdata->rc.lbprimal[i] = (int *)ptr;
	ptr += nvars*sizeof(int);
	pdata->rc.ubprimal[i] = (int *)ptr;
	ptr += nvars*sizeof(int);
      }
    }
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    return pdata;
}


/* the targets need to be ordered (see ORDER comments) */
static void contract_vars(struct all_dist_data *pdata)
{
  int imax = pdata->nvartargets;
  int n = pdata->nvars;
  VERTEX *target = pdata->vartarget;
  int i, j, k;
    
  /* separate into nonground (0..) and ground (n..) */
  for (i=0, j=0, k=n; i<imax; i++) {
    VERTEX current = target[i];
    
    if (!GROUND(current)) {
      target[j++] = current;
    } else {
      VISITED(current) = EOL;	/* maintain invariant! */
      target[k++] = current;
    }
  }
  if (i==j)
    return;
  
  pdata->nvartargets = j;

  while (n<k)
    target[j++] = target[n++];
}


/* the targets need to be ordered (see ORDER comments) */
static void alldiff_contract_vals(struct all_dist_data *pdata)
{
  int imax = pdata->nvaltargets;
  int n = pdata->nvals;
  VERTEX *target = pdata->valtarget;
  int i, j, k;
    
  /* separate into nonground (0..) and ground (n..) */
  for (i=0, j=0, k=n; i<imax; i++) {
    VERTEX current = target[i];
    
    if (!GROUND(MATE(current)))
      target[j++] = current;
    else {
      VISITED(current) = EOL;	/* maintain invariant! */
      target[k++] = current;
    }
  }
  if (i==j)
    return;
  
  pdata->nvaltargets = j;

  while (n<k)
    target[j++] = target[n++];
}

/* the targets need to be ordered (see ORDER comments) */
static void assignment_contract_vals(struct all_dist_data *pdata)
{
  int imax = pdata->nvaltargets;
  int n = pdata->nvals;
  VERTEX *target = pdata->valtarget;
  int i, j, k;
    
  /* separate into nonground (0..) and ground (n..) */
  for (i=0, j=0, k=n; i<imax; i++) {
    VERTEX current = target[i];
    
    if (!GROUND(current))
      target[j++] = current;
    else {
      VISITED(current) = EOL;	/* maintain invariant! */
      target[k++] = current;
    }
  }
  if (i==j)
    return;
  
  pdata->nvaltargets = j;

  while (n<k)
    target[j++] = target[n++];
}

#define ASSIGNMENT_ARITY 8

/* '$fd_all_distinct'(+State0, -State, -Actions) :-
   State0 is f(LVec,NVarsGone,NValsGone,Flag,SuspF,Handle,Stamp) 
   where LVec is a list of Var-Attribute
   where Flag is (0x1 -> circuit),
   State  similarly,
   Actions is a list of prunings etc.
   */
void SPCDECL 
prolog_fd_all_distinct(Wam wam,
		       SP_term_ref State0,
		       SP_term_ref State,
		       SP_term_ref Actions)
{
  int i, flags, all_dist_pruned, ent = -1; /* disentailed unless otherwise */
  TAGGED tlvec, telt, t1, f;
  TAGGED handle;
  SP_integer state_stamp;
  struct all_dist_data *pdata;
  SP_BOOL committed;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  DerefArg(telt,X(0),4);
  flags = GetSmall_int(telt);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    fd.gdata = pdata = Pdata(struct all_dist_data,handle);
  } else {			/* build persistent state */
    TAGGED all = EmptySet;
    int nvals = 0;
    int nvars = 0;
    FDITER it;
    
    DerefArg(tlvec,X(0),1);
    while (TagIsLST(tlvec)) {
      DerefCar(telt,tlvec);
      DerefCdr(tlvec,tlvec);
      DerefArg(t1,telt,2);	/* Attribute */
      DerefAttribute(t1,t1);
      all = fd_merge_into(wam, DomainSet(t1),all);
      nvars++;
    }
    if (nvars==0) {
      ent = 1;
      goto ret1;
    }
    nvals = nvars*nvars;
    if (fd_countable(all) && fd_size(all)<nvals) {
      nvals = (int)fd_size(all);
    }
    fd.gdata = pdata = all_dist_alloc(wam, nvars,nvals,nvars,handle,flags);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    if (fd_compare_interval(all, TaggedOne, MakeSmall(nvars))==FDI_EQUAL)
      pdata->reset_density = 2;
    else if (nvals==nvars)
      pdata->reset_density = 1;
    else
      pdata->reset_density = 0;
    DerefArg(f,X(0),5);
    f = SetArity(f,1);
    pdata->enqueue_code =
      f==fd.functor_val ? FD_QUEUE_VAL :
      f==fd.functor_minmax ? FD_QUEUE_MINMAX :
      f==functor_dom1 ? FD_QUEUE_DOM : 0;
    DerefArg(tlvec,X(0),1);
    for (i=0; i<nvars; i++) {
      DerefCar(telt,tlvec);
      DerefCdr(tlvec,tlvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      dvar_init(DVAR(i), RefAttr(i), RefVar(i));
    }
    if (pdata->enqueue_code)
      for (i=0; i<nvars; i++) {
	dvar_attach_daemon(wam, DVAR(i), pdata, X(1), f);
      }
    for (i=pdata->nvars-1; i>=0; i--) {
      LABEL(i) = MakeSmall(i+1);
      MATE(i) = EOL;
      TVAR(i) = i;
    }

    if (pdata->reset_density > 0) {
      fditer_init(&it, all);
      for (i=0; i<pdata->nvals; i++) {
	VERTEX valvertex = i + pdata->nvars;
	t1 = fditer_next(&it);
	LABEL(valvertex) = t1;
	MATE(valvertex) = EOL;
	TVAL(i) = valvertex;
      }
    }

    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }

				/* RESUME HERE */
  pdata->pruned = 0;
  pdata->incremental = (state_stamp==pdata->stamp);
  if (!pdata->incremental) {
    int nvals = pdata->nvaltargets;
    pdata->density = pdata->reset_density;
    DerefArg(telt,X(0),2);
    pdata->nvartargets = pdata->nvars-GetSmall_int(telt);
    DerefArg(telt,X(0),3);
    pdata->nvaltargets = pdata->nvals-GetSmall_int(telt);
    if (pdata->density>0 && pdata->nvaltargets>nvals)
      qsort_asc_label(wam, pdata->valtarget,pdata->nvaltargets);
  }

  for (i=pdata->nvartargets-1; i>=0; i--) {
    VERTEX vertex = TVAR(i);
    
    dvar_refresh(DVAR(vertex));
  }
  pdata->stamp = state_stamp+1;

  do {
    switch (pdata->density) {
    case 0:
      if (!all_dist_kernel(wam, pdata))
	goto ret;
      break;
    default:
      pdata->nvarkernels = pdata->nvartargets;
      pdata->nvalkernels = pdata->nvaltargets;
      for (i=pdata->nvartargets-1; i>=0; i--) {
	VERTEX vertex = TVAR(i);
	VERTEX mate = MATE(vertex);
    
	if (mate>EOL && !dvar_contains_value_t(DVAR(vertex),LABEL(mate)))
	  MATE(vertex) = MATE(mate) = EOL;
      }
      break;
    }
    if (!all_dist_filter(wam, pdata))
      goto ret;
    all_dist_pruned = pdata->pruned;
    if (flags&1 && pdata->nvartargets>=5)	/* determined experimentally */
      if (!single_circuit_check(wam, pdata))
	goto ret;
  } while (pdata->pruned > all_dist_pruned);

  if (pdata->pruned>0) {
    for (i=pdata->nvartargets-1; i>=0; i--) {
      VERTEX vertex = TVAR(i);
    
      dvar_pruning_done( DVAR(vertex));
    }
    for (i=pdata->nvartargets-1; i>=0; i--) {
      VERTEX vertex = TVAR(i);
    
      dvar_export(DVAR(vertex));
    }
  }
  contract_vars(pdata);
  if (pdata->density>0)
    alldiff_contract_vals(pdata);
  CTagToArg(X(0),2) = MakeSmall(pdata->nvars-pdata->nvartargets);
  CTagToArg(X(0),3) = MakeSmall(pdata->nvals-pdata->nvaltargets);
  ent = (pdata->nvartargets<=1);
 ret:
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(wam, Actions, ent);
}



/* '$fd_assignment'(+State0, -State, -Actions) :-
   State0 is f(LVec,RVec,IntSoFar,Flag,Cost,CostSoFar,Handle,Stamp) 
   where LVec,RVec are lists of f(Var,DomainMut),
   where Flag is (0x1 -> circuit ; 0x2 -> cost),
   State  similarly,
   Actions is a list of prunings etc.
   */
void SPCDECL 
prolog_fd_assignment(Wam wam,
		     SP_term_ref State0,
		     SP_term_ref State,
		     SP_term_ref Actions)
{
  int nvars, nrefs, nvals, i, j, flags;
  TAGGED tlvec, telt, trvec;
  TAGGED handle;
  SP_integer state_stamp;
  int ent = -1;			/* disentailed unless otherwise */
  struct all_dist_data *pdata;
  SP_BOOL committed;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

  
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  DerefArg(telt,X(0),4);
  flags = GetSmall_int(telt);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    fd.gdata = pdata = Pdata(struct all_dist_data,handle);
    nvars = pdata->nvars;
    nvals = pdata->nvals;
  } else {			/* build persistent state */
				/* compute nvars, nvals */
    DerefArg(tlvec,X(0),1);
    nvars = fd_list_length(tlvec);
    if (nvars==0) {
      ent = 1;
      goto ret1;
    }
    nvals = nvars;
    DerefArg(telt,X(0),5);
    nrefs = IsAtomic(telt) ? 2*nvars : 2*nvars+1;
    fd.gdata = pdata = all_dist_alloc(wam, nvars,nvals,nrefs,handle,flags);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    DerefArg(tlvec,X(0),1);
    DerefArg(trvec,X(0),2);
    for (i=0; i<nvars; i++) {
      DerefCar(telt,tlvec);
      DerefCdr(tlvec,tlvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      dvar_init(DVAR(i), RefAttr(i), RefVar(i));
      DerefCar(telt,trvec);
      DerefCdr(trvec,trvec);
      fd_get_var_and_attr(telt,RefAttr(nvars+i));
      dvar_init(DVAR(nvars+i), RefAttr(nvars+i), RefVar(nvars+i));
    }
    DerefArg(telt,X(0),5);
    if (!IsAtomic(telt)) {
      fd_get_var_and_attr(telt,RefAttr(nrefs-1));
      pdata->rc.costvar = pdata->dvar+(nrefs-1);
      dvar_init(pdata->rc.costvar, RefAttr(nrefs-1), RefVar(nrefs-1));
      DerefArg(telt,telt,3);	/* cost matrix */
    }
    ass_init(wam, pdata,telt);
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),5) = atom_nil; /* [MC] 3.12: free for GC */
  }

				/* RESUME HERE */
  pdata->pruned = 0;
  pdata->incremental = (state_stamp==pdata->stamp);
  if (!pdata->incremental) {
    int nvars1, nvals1;
    DerefArg(telt,X(0),3);
    nvars1 = nvars-GetSmall_int(telt);
    nvals1 = nvals-GetSmall_int(telt);
    if (pdata->nvartargets < nvars1) {
      pdata->nvartargets = nvars1;
      qsort_asc_label(wam, pdata->vartarget,nvars1);
    }
    if (pdata->nvaltargets < nvals1) {
      pdata->nvaltargets = nvals1;
      qsort_asc_label(wam, pdata->valtarget,nvals1);
    }
  }
  
  /* Always refresh dom fields */
  for (i=pdata->nvartargets-1; i>=0; i--) {
    VERTEX vertex = TVAR(i);

    dvar_refresh(DVAR(vertex));
    VISITED(vertex) = 0;
  }
  for (i=pdata->nvaltargets-1; i>=0; i--) {
    VERTEX vertex = TVAL(i);

    dvar_refresh(DVAR(vertex));
    VISITED(vertex) = 0;
  }
  pdata->stamp = state_stamp+1;
  pdata->nvarkernels = pdata->nvartargets;
  pdata->nvalkernels = pdata->nvaltargets;
  if (!pairing_filter(wam, pdata))
    goto ret;
  if (!pdata->cost && !ass_filter(wam, pdata))
    goto ret;
  if (pdata->cost && !cost_filter(wam, pdata,GetSmall(CTagToArg(X(0),6)),flags))
    goto ret;
  /* If <6 targets and more than one SCC, then one
     will be of size 2 and will be caught by pairing/2. */
  if ((flags&1) && !pdata->cost && pdata->nvartargets>=6)
    if (!single_circuit_check(wam, pdata))
      goto ret;
  for (i=pdata->nvartargets-1; i>=0; i--) { /* must precede exporting! */
    VERTEX v;
    v = TVAR(i);
    SIZE(v) = LIVE(v);
    v = TVAL(i);
    SIZE(v) = LIVE(v);
  }
  if (pdata->pruned>0) {
    for (i=pdata->nvartargets-1; i>=0; i--)
      for (j=0; j<2; j++) {
	VERTEX vertex = j==0 ? TVAR(i) : TVAL(i);

	dvar_pruning_done( DVAR(vertex));
      }
    if (pdata->cost)
      dvar_pruning_done( pdata->rc.costvar);
    for (i=pdata->nvartargets-1; i>=0; i--)
      for (j=0; j<2; j++) {
	VERTEX vertex = j==0 ? TVAR(i) : TVAL(i);

	dvar_export(DVAR(vertex));
      }
    if (pdata->cost)
      dvar_export(pdata->rc.costvar);
  }
  if (pdata->cost) {
    SP_integer costbase = GetSmall(CTagToArg(X(0),6));
    
    for (i=pdata->nvartargets-1; i>=0; i--) {
      VERTEX vertex = TVAR(i);
      if (GROUND(vertex))
	costbase += pdata->cost[nvars*vertex + dvar_min_l(DVAR(vertex))-1];
    }
    CTagToArg(X(0),6) = MakeSmall(costbase);
  }
  contract_vars(pdata);
  assignment_contract_vals(pdata);
  CTagToArg(X(0),3) = MakeSmall(nvars-pdata->nvartargets);
  ent = (pdata->nvartargets<=1);
 ret:
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(wam, Actions, ent);
  if (ent==1)
    CTagToArg(X(0),ASSIGNMENT_ARITY-1) = TaggedZero; /* hide state from helper */
}



/*** COST-BASED PRUNING ***/

/* iterating over a "row" (in terms of column indices) */

#define rowiter DVITER

#define rowiter_init(IT,R) dviter_init(IT,DVAR(pdata->rc.index_var[R]))

#define rowiter_empty(IT) dviter_empty(IT)

#define rowiter_next(IT) (pdata->rc.var_index[dviter_next_value_l(IT)+nvars-1]-n)

/* iterating over a "column" (in terms of row indices) */

#define coliter DVITER

#define coliter_init(IT,R) dviter_init(IT,DVAR(pdata->rc.index_var[(R)+n]))

#define coliter_empty(IT) dviter_empty(IT)

#define coliter_next(IT) (pdata->rc.var_index[dviter_next_value_l(IT)-1])

#if 0 /* MAY COME IN HANDY */
static int cmp_asc_cost(Wam wam,
			       int *t1, int *t2)
{
  struct all_dist_data *pdata = fd.gdata;
  SP_integer c1, c2;
  
  c1 = -pdata->rc.cost[*t1];
  c2 = -pdata->rc.cost[*t2];
  if (c1==c2)
    return 0;
  else
    return (c1<c2 ? -1 : 1);
}

#define QType int
#define QCmp  cmp_asc_cost
#define QSort qsort_asc_cost
#include "qsort.ic"
#endif

/* BINARY HEAPS */

#define SWAP(I,J)				\
{						\
  int vi = pdata->rc.heap[I];			\
  int vj = pdata->rc.heap[J];			\
  pdata->rc.heap[I] = vj;			\
  pdata->rc.heap[J] = vi;			\
  pdata->rc.vheap[vi] = (J);			\
  pdata->rc.vheap[vj] = (I);			\
}

static void decrease_key(struct all_dist_data *pdata,int v, SP_integer value)
{
  int i = pdata->rc.vheap[v];
  int p = (i-1)>>1;
  pdata->rc.distrow[v] = value;
  while (i>0 && value<pdata->rc.distrow[pdata->rc.heap[p]]) {
    SWAP(i,p);
    i = p;
    p = (i-1)>>1;
  }
}

static void spheapify(struct all_dist_data *pdata,int i)
{
  int *heap = pdata->rc.heap;
  SP_integer *distrow = pdata->rc.distrow;
  
  for (;;) {
    int l = (i<<1)+1;
    int smallest = i;
    if (l<pdata->rc.heapsize && distrow[heap[l]]<distrow[heap[smallest]])
      smallest = l;
    if (l+1<pdata->rc.heapsize && distrow[heap[l+1]]<distrow[heap[smallest]])
      smallest = l+1;
    if (smallest==i)
      break;
    SWAP(i,smallest);
    i = smallest;
  }
}

static void spheap_init(struct all_dist_data *pdata,int k)
{
  int n = pdata->rc.ntargets;
  int i, j;
  for (i=0; i<n-1; i++)
    pdata->rc.heap[i] = pdata->rc.vheap[i] = i;
  pdata->rc.heap[k] = i;
  pdata->rc.vheap[i] = k;
  pdata->rc.heapsize = i;
  for (j=(i-2)>>1; j>=0; j--)
    spheapify(pdata,j);
}

static int spheap_extract_min(struct all_dist_data *pdata)
{
  int u = pdata->rc.heap[0];
  int v = pdata->rc.heap[--pdata->rc.heapsize];

  if (pdata->rc.heapsize>0) {
    pdata->rc.heap[0] = v;
    pdata->rc.vheap[v] = 0;
    spheapify(pdata,0);
  }
  return u;
}

static void all_pairs_shortest_paths(struct all_dist_data *pdata,
				     int **primal)
{
  int n = pdata->rc.ntargets;
  int k;
  for (k=0; k<n; k++) {
    /* solve single-source shortest-paths for vertex k */
    /* warm start: exclude k from queue */
    pdata->rc.distrow = pdata->rc.dist + n*k;
    spheap_init(pdata,k);
    while (pdata->rc.heapsize>0) {
      int u, v;
      SP_integer du, duv;
      coliter it;
      u = spheap_extract_min(pdata);
      du = pdata->rc.distrow[u];
      if (du<CLPFD_MAXINT2) {
	coliter_init(&it,primal[0][u]);
	while (!coliter_empty(&it)) {
	  v = coliter_next(&it);
	  duv = pdata->rc.dist[n*u + v];
	  if (u!=v && pdata->rc.distrow[v]>du+duv)
	    decrease_key(pdata,v,du+duv);
	}
      }
    }
  }
}

#define safeadd(S,T) 				\
((S)==CLPFD_MAXINT2 ? CLPFD_MAXINT2 		\
: (T)==CLPFD_MAXINT2 ? CLPFD_MAXINT2 		\
: (S)+(T)					\
)

#define reduced_cost(I,J) 			\
(cost[n*(I)+(J)]-(dual[0][I]+dual[1][J]))

#define admissible(I,J) 			\
(reduced_cost(I,J)==0)

static int hungarian_init(struct all_dist_data *pdata,
			  SP_integer **dual,
			  int **primal)
{
  int i, j;
  rowiter it;
  int card = 0;
  int n = pdata->rc.ntargets;
  int nvars = pdata->nvars;
  SP_integer *cost = pdata->rc.cost;
  SP_integer lu, lv;
  
  /* identify a dual feasible solution */
  for (i=0; i<n; i++) {
    lu = CLPFD_MAXINT2;
    rowiter_init(&it,i);
    while (!rowiter_empty(&it)) {
      j = rowiter_next(&it);
      lv = cost[n*i + j];
      if (lu>lv)
	lu = lv;
    }
    dual[0][i] = lu;
  }
  for (j=0; j<n; j++) {
    lu = CLPFD_MAXINT2;
    coliter_init(&it,j);
    while (!coliter_empty(&it)) {
      i = coliter_next(&it);
      lv = cost[n*i + j] - dual[0][i];
      if (lu>lv)
	lu = lv;
    }
    dual[1][j] = lu;
  }
  /* find an initial allocation */
  for (j=0; j<n; j++)
    primal[1][j] = -1;		/* mark all columns unallocated */
  for (i=0; i<n; i++) {
    primal[0][i] = -1;		/* mark row i unallocated */
    rowiter_init(&it,i);
    while (!rowiter_empty(&it)) {
      j = rowiter_next(&it);
      if (primal[1][j] == -1 && admissible(i,j)) {
	primal[0][i] = j;	/* mark row i allocated */
	primal[1][j] = i;	/* mark col j allocated */
	card++;
	break;
      }
    }
  }
  return card;
}



static SP_BOOL
hungarian_body(struct all_dist_data *pdata,
	       SP_integer **dual,
	       int **primal,
	       int card,
	       SP_integer *opt)
{
  int n = pdata->rc.ntargets;
  int nvars = pdata->nvars;
  int i, j, head, tail;
  SP_integer lu, lv;
  rowiter it;
  SP_integer *cost = pdata->rc.cost;

  /* labeling for checking max flow */
  while (card<n) {
    for (j=n-1; j>=0; j--)
      pdata->rc.label[1][j] = -1;
    for (i=n-1; i>=0; i--)
      pdata->rc.label[0][i] =  (primal[0][i] == -1) ? n : -1;
    /* for each col j, find index of labeled row in which min occurs */
    for (j=0; j<n; j++) {
      int ix = -1;
      lu = CLPFD_MAXINT2;
      coliter_init(&it,j);
      while (!coliter_empty(&it)) {
	i = coliter_next(&it);
	if (pdata->rc.label[0][i] != -1) {
	  lv = reduced_cost(i,j);
	  if (lu>=lv) {
	    lu = lv;
	    ix = i;
	  }
	}
      }
      pdata->rc.index[j] = ix;
    }
  mklists:
    head = tail = 0;
    for (i=0; i<n; i++)
      if (pdata->rc.label[0][i] != -1)
	pdata->rc.queue[tail++] = i; /* enqueue row i */
    while (head!=tail) {
      i = pdata->rc.queue[head++];
      if (i<n) {		/* scan labeled row i */
	rowiter_init(&it,i);
	while (!rowiter_empty(&it)) {
	  j = rowiter_next(&it);
	  if (pdata->rc.label[1][j] == -1 && admissible(i,j)) { /* (i,j) admissible, j unlabeled */
	    pdata->rc.label[1][j] = i;
	    pdata->rc.queue[tail++] = j+n;	/* enqueue col j */
	  }
	}
      } else {
	j = i-n;		/* scan labeled col j */
	i = primal[1][j];
	if (i == -1) {		/* BREAKTHRU: found an augmenting path */
	  int row, col;

	  col = j;
	  for (;;) {
	    row = pdata->rc.label[1][col];
	    col = pdata->rc.label[0][row];
	    if (col==n)
	      break;
	    primal[0][row] = primal[1][col] = -1; /* unalloc (row,col) */
	  }
	  col = j;
	  while (col<n) {
	    row = pdata->rc.label[1][col];
	    primal[0][row] = col; /* alloc (row,col) */
	    primal[1][col] = row;
	    col = pdata->rc.label[0][row];
	  }
	  card++;
	  goto augment;
	}
	if (pdata->rc.label[0][i] == -1) { /* (i,j) is alloced, i unlabeled */
	  pdata->rc.label[0][i] = j;
	  pdata->rc.queue[tail++] = i;	/* enqueue row i */
	  /* update indices for unlabeled cols. */
	  rowiter_init(&it,i);
	  while (!rowiter_empty(&it)) {
	    j = rowiter_next(&it);
	    if (pdata->rc.label[1][j] == -1) {
	      int ix = pdata->rc.index[j];
	      lv = reduced_cost(i,j);
	      if (lv==0) {
		pdata->rc.label[1][j] = i;
		pdata->rc.queue[tail++] = j+n; /* enqueue col j */
	      } else if (ix == -1 || reduced_cost(ix,j)>lv)
		pdata->rc.index[j] = i;
	    }
	  }
	}
      }
    }
    /* NONBREAKTHROUGH */
    /* find min. reduced cost in (LABELED,UNLABELED) */
    lu = CLPFD_MAXINT2;
    for (j=0; j<n; j++)
      if (pdata->rc.label[1][j] == -1) {
	int ix = pdata->rc.index[j];
	if (ix != -1) {
	  lv = reduced_cost(ix,j);
	  if (lu>lv)
	    lu = lv;
	}
      }
    if (lu==CLPFD_MAXINT2)
      return FALSE;			/* no feasible assignment */
    /* update dual soln */
    for (i=n-1; i>=0; i--)
      if (pdata->rc.label[0][i] != -1)
	dual[0][i] += lu;
    for (j=n-1; j>=0; j--)
      if (pdata->rc.label[1][j] != -1)
	dual[1][j] -= lu;
    goto mklists;
  augment:
    ;
  }
  /* have max. cardinality matching */
  lu = 0;
  for (i=n-1; i>=0; i--)
    lu += dual[0][i];
  for (j=n-1; j>=0; j--)
    lu += dual[1][j];
  *opt = lu;
  return TRUE;
}

static SP_BOOL
cost_based_filter(Wam wam,
		  struct all_dist_data *pdata,
		  SP_BOOL b /* 0=min, 1=max */)
{
  int n = pdata->rc.ntargets;
  int nvars = pdata->nvars;
  int i, j, nextj, u, v, card;
  SP_integer max_regret, min_regret;
  SP_integer **dual = (b==0 ? pdata->rc.lbdual : pdata->rc.ubdual);
  int  **primal = (b==0 ? pdata->rc.lbprimal : pdata->rc.ubprimal);
  SP_integer *cost = pdata->rc.cost;
  SP_integer *rcost = pdata->rc.rcost[b];
  
  (void)wam;
  for (i=0; i<n; i++) {
    rowiter it;			/* iterator for one variable */
    u = pdata->rc.index_var[i];
    rowiter_init(&it,i);
    nextj = rowiter_next(&it);
    for (j=0; j<n; j++) {
      v = pdata->rc.index_var[j+n]-nvars;
      if (j<nextj)
	pdata->rc.cost[n*i + j] = CLPFD_MAXINT2;
      else if (b==0) {
	pdata->rc.cost[n*i + j] =  pdata->cost[nvars*u + v];
	nextj = rowiter_empty(&it) ? n : rowiter_next(&it);
      } else {
	pdata->rc.cost[n*i + j] = -pdata->cost[nvars*u + v];
	nextj = rowiter_empty(&it) ? n : rowiter_next(&it);
      }
    }
  }
  if (pdata->incremental) {
    card = n;
    for (i=0; i<n; i++) {
      j = primal[0][i];
      if (!dvar_contains_value_t(DVAR(pdata->rc.index_var[i]),
				 LABEL(pdata->rc.index_var[j+n]))) {
	primal[0][i] = -1;
	primal[1][j] = -1;
	card--;
      }
    }
  } else {
    card = hungarian_init(pdata,dual,primal);
  }
  if (!hungarian_body(pdata,dual,primal,card,&pdata->rc.optcost[b]))
    return FALSE;
  /* We have obtained:
     pdata->rc.optcost, the (smallest) cost of an (optimal) assignment.

     PROPOSITION: all solutions containing only (xi=vi) with regret=0
     have cost = cost of optimal assignment.  If true, the following
     rule is valid:

     If optcost is NOT in the domain of costvar, then infer:
         min(costvar) >= optcost + smallest regret>0.

     Open question: how to infer a sharp bound for costvar.
  */
  if (b==0) {
    TAGGED ubt = dvar_max_t(pdata->rc.costvar);
    
    pdata->rc.optcost[0] += pdata->rc.costbase;
    pdata->rc.threshold[0] =
      TagIsSmall(ubt) ? GetSmall(ubt) - pdata->rc.optcost[0] :
      CLPFD_MAXINT2-1;
  } else {
    TAGGED lbt = dvar_min_t(pdata->rc.costvar);
    
    pdata->rc.optcost[1] = pdata->rc.costbase - pdata->rc.optcost[1];
    pdata->rc.threshold[1] =
      TagIsSmall(lbt) ? pdata->rc.optcost[1] - GetSmall(lbt) :
      CLPFD_MAXINT2-1;
  }
  if (pdata->rc.threshold[b]<0)	/* minimum is too large,
				   or maximum is too small */

    return FALSE;

  for (i=0; i<n; i++) {
    rowiter it;		/* iterator for one variable */
    rowiter_init(&it,i);
    nextj = rowiter_next(&it);
    for (j=0; j<n; j++) {
      if (j<nextj)
	rcost[n*i + j] = CLPFD_MAXINT2;
      else {
	rcost[n*i + j] = reduced_cost(i,j);
	nextj = rowiter_empty(&it) ? n : rowiter_next(&it);
      }
    }
  }
  for (i=0; i<n; i++) {
    int mate = primal[0][i];
    for (j=0; j<n; j++)
      pdata->rc.dist[n*i + j] = rcost[n*j + mate];
  }
  all_pairs_shortest_paths(pdata,primal);
  max_regret = 0;
  min_regret = CLPFD_MAXINT2;
  for (i=0; i<n; i++)
    for (j=0; j<n; j++) {
      SP_integer regret =
	safeadd(rcost[n*i + j],
		pdata->rc.dist[n*i + primal[1][j]]);
      rcost[n*i + j] = regret;
      if (max_regret<regret)
	max_regret = regret;
      if (regret>0 && min_regret>regret)
	min_regret = regret;
    }
  pdata->rc.min_regret[b] = min_regret;
  pdata->rc.max_regret[b] = max_regret;
  return TRUE;
}

/* Same algo. as single_circuit, but adapted for cost_based_pruning. */
static SP_BOOL 
cost_cycle_feasible(struct all_dist_data *pdata,
		    SP_integer max_regret,
		    int *map,
		    int b)
{
  int id=0, top=0, min, m, vertid, i, vertex=0, child;
  SP_integer *lstack = (SP_integer *)pdata->dist_stack;
  SP_integer *regret = pdata->rc.rcost[b];
  int n = pdata->rc.ntargets;
  int *visited = pdata->rc.index;
  
  for (i=n-1; i>=0; i--)
    visited[i] = 0;
start:
  visited[vertex] = vertid = min = ++id;
  for (i=0; i<n; i++) {
    if (regret[n*vertex + i]>max_regret)
      continue;
    child = map[i];
    m = visited[child];
    if (m==0) {
      lstack[top] = vertex;
      lstack[top+1] = i;
      lstack[top+2] = min;
      top += 3;
      vertex = child;
      goto start;
    }
  cont:
    if (min > m)
      min = m;
  }
  if (min==vertid)
    return (min==1 && id==n);
  m = min;
  top -= 3;
  vertex = (int)lstack[top];
  i = (int)lstack[top+1];
  min = (int)lstack[top+2];
  vertid = visited[vertex];
				/* here, m is the minimal ID reachable from
				   some just visited child of vertex */
  goto cont;
}

#if 0 /* MAY COME IN HANDY */
/* Ripped off from visit(wam, ) */
static int
cost_visit(wam, struct all_dist_data *pdata,
	   int vertex,
	   SP_integer max_regret,
	   int *map,
	   int b)
{
  int newvertex=EOL;
  int i, m, min, scc;
  int *dist_stack = (int *)pdata->dist_stack;
  int n = pdata->rc.ntargets;
  int *visited = pdata->rc.index;
  SP_integer *regret = pdata->rc.rcost[b];
   
  visited[vertex] = ++pdata->scc_visited;
  min = pdata->scc_visited;
  dist_stack[pdata->scc_index++] = vertex;
  for (i=0; i<n; i++) {
    if (regret[n*vertex + i]>max_regret)
      continue;
    newvertex = map[i];
    if (newvertex != vertex) {
      m = (visited[newvertex] == 0) ? 
	cost_visit(wam, pdata,newvertex,max_regret,map,b) :
	visited[newvertex];
      if (m < min)
	min = m;
    }
   }
   if (min == visited[vertex]) {
     scc = ++pdata->scc_component;
     do {
       /* Each descendant on the dist_stack is part of this vertex's SCC. */
       newvertex = dist_stack[--pdata->scc_index];
       COMPONENT(newvertex) = scc;
       visited[newvertex]=0xffffff; /* High value, so that
				       this vertex will be ignored
				       in the future search. */
     } while (newvertex != vertex);
   }
   return min;
}
#endif

static SP_BOOL
cost_based_pruning(Wam wam,
		   struct all_dist_data *pdata,
		   int flags,
		   SP_BOOL *change)
{
  SP_BOOL cost_has_hole = !dvar_is_interval(pdata->rc.costvar);
  SP_BOOL pruned = FALSE;
  SP_integer bound[2];
  int n = pdata->rc.ntargets;
  int nvars = pdata->nvars;
  int i, j, b;

  for (b=0; b<2; b++)
    bound[b] = pdata->rc.optcost[b];
  if (pdata->rc.max_regret[0] > pdata->rc.threshold[0] ||
      pdata->rc.max_regret[1] > pdata->rc.threshold[1] ||
      cost_has_hole) {
    SP_integer *threshold = pdata->rc.threshold;
    SP_integer **rcost = pdata->rc.rcost;
    SP_integer regret[2];
    
    /* PRUNE EACH VAR */
    for (i=0; i<n; i++) {
      VERTEX v = pdata->rc.index_var[i];

      if (!GROUND(v)) {
	rowiter it;		/* iterator over variable v */
	FDCONS cons;		/* constructor for pruned values */
	
	rowiter_init(&it,i);
	fdcons_init(&cons);
	while (!rowiter_empty(&it)) {
	  j = rowiter_next(&it);
	  regret[0] = rcost[0][n*i + j];
	  regret[1] = rcost[1][n*i + j];
	  if (regret[0] > threshold[0] ||
	      regret[1] > threshold[1] ||
	      (cost_has_hole && regret[0]+regret[1]>0 &&
	       dvar_compare_interval_l(pdata->rc.costvar,
				       pdata->rc.optcost[0] + regret[0],
				       pdata->rc.optcost[1] - regret[1])==
	       FDI_DISJOINT))
	    fdcons_add(wam, &cons,LABEL(pdata->rc.index_var[j+n]));
	}
	if (fdcons_size(&cons)>0) {
	  if (dvar_prune_set(DVAR(v),fdcons_set(&cons))== -1)
	    return FALSE;
	  pruned = TRUE;
	}
      }
    }
    
    /* PRUNE EACH VAL */
    for (j=0; j<n; j++) {
      VERTEX v = pdata->rc.index_var[j+n];
      
      if (!GROUND(v)) {
	coliter it;		/* iterator over value v */
	FDCONS cons;		/* constructor for pruned values */

	coliter_init(&it,j);
	fdcons_init(&cons);
	while (!coliter_empty(&it)) {
	  i = coliter_next(&it);
	  regret[0] = rcost[0][n*i + j];
	  regret[1] = rcost[1][n*i + j];
	  if (regret[0] > threshold[0] ||
	      regret[1] > threshold[1] ||
	      (cost_has_hole && regret[0]+regret[1]>0 &&
	       dvar_compare_interval_l(pdata->rc.costvar,
				       pdata->rc.optcost[0] + regret[0],
				       pdata->rc.optcost[1] - regret[1])==
	       FDI_DISJOINT))
	    fdcons_add(wam, &cons,LABEL(pdata->rc.index_var[i]));
	}
	if (fdcons_size(&cons)>0) {
	  if (dvar_prune_set(DVAR(v),fdcons_set(&cons))== -1)
	    return FALSE;
	  pruned = TRUE;
	}
      }
    }
  }
  

#if 0 /* MAY COME IN HANDY */
  /* CHECK PROPOSITION: all zero-regret solutions have cost=LB */
  {
    int val[10];
    int row = 0;
    int col = 0;

    val[0] = -1;
    while (row>=0) {
      col = val[row]+1;
      while (col<n && pdata->rc.rcost[0][n*row + col]>0)
	col++;
      if (col==n)
	row--;
      else {
	for (j=0; j<row && val[j]!=col; j++)
	  ;
	val[row] = col;
	if (j==row) {
	  if (row==n-1) {
	    SP_integer total = pdata->rc.costbase;
	    for (j=0; j<n; j++)
	      total -= pdata->rc.cost[n*j + val[j]]; /* rc.cost is negated */
	    if (total > pdata->rc.optcost[0]) {
	      printf("! PROPOSITION FALSE\n");
	      return FALSE;
	    }					       
	  } else
	    val[++row] = -1;
	}
      }
    }
  }
#endif

  if (flags&1)
    /* CHECK HAMILTONIAN FEASIBILITY */
    for (b=0; b<2; b++) {
      int nn = n*n;
      SP_integer *regarr = Malloc(nn,SP_integer);
      SP_integer *regret = pdata->rc.rcost[b];
      SP_integer threshold = pdata->rc.threshold[b];
      int nregrets = 0;
      int inf = -1, mid, sup;
      SP_integer optreg = 0;
      VERTEX v1, v2, *head, *tail;
      int *map = pdata->rc.heap;	/* val index -> var index */
      int *invmap = pdata->rc.vheap;	/* var index -> val index */
      int *index_var = pdata->rc.index_var;
      int *var_index = pdata->rc.var_index;
      
      head = pdata->dist_stack;
      tail = head + nvars;
      for (i=nvars-1; i>=0; i--)
	head[i] = tail[i] = i;

      /* merge circuits for the "non rc.targets" */
      for (i=n; i<nvars; i++) {
	v1 = TVAR(i);
	v2 = dvar_min_l(DVAR(v1))-1;
	v1 = head[v1];
	v2 = tail[v2];
	tail[v1] = v2;
	head[v2] = v1;
      }
      for (i=n-1; i>=0; i--) {
	map[i] = var_index[tail[index_var[i+n]-nvars]];
	invmap[map[i]] = i;
      }

#if 0 /* MAY COME IN HANDY */
      SP_integer regret0, regret1=0;
      int *comp_type = (int *)pdata->dist_stack;
      int *comp_minin = comp_type+n;
      int *comp_minout = comp_minin+n;
      int *visited = pdata->rc.index;
      int ncomp;
      /* INFER REGRETS FROM SCC ON SUBGRAPH BASED ON REGRET THRESHOLD */
      do {
	regret0 = regret1;

	pdata->scc_component = 0;
	pdata->scc_visited = 0;
	pdata->scc_index = 0;
	for (i=n-1; i>=0; i--)
	  visited[i] = 0;
	for (i=n-1; i>=0; i--)
	  if (visited[i] == 0)
	    cost_visit(wam, pdata,i,regret0,map,b);
	ncomp = pdata->scc_component;
	if (ncomp==1)
	  break;
	for (i=ncomp-1; i>=0; i--) {
	  comp_type[i] = 3;
	  comp_minin[i] = CLPFD_MAXINT2;
	  comp_minout[i] = CLPFD_MAXINT2;
	}
	for (i=0; i<nn; i++) {
	  SP_integer reg = regret[i];
	  int ifrom, ito, cfrom, cto;
      
	  if (reg<=threshold) {
	    ifrom = i/n;
	    ito = map[i%n];
	    cfrom = COMPONENT(ifrom)-1;
	    cto = COMPONENT(ito)-1;
	    if (cfrom!=cto) {
	      if (reg<=regret0) {
		comp_type[cfrom] &= -3; /* mark not SINK */
		comp_type[cto] &= -2; /* mark not SOURCE */
	      } else {
		if (comp_minout[cfrom]>reg)
		  comp_minout[cfrom] = reg;
		if (comp_minin[cto]>reg)
		  comp_minin[cto] = reg;
	      }
	    }
	  }
	}
	for (i=0; i<ncomp; i++) {
	  if ((comp_type[i]&1) && regret1<comp_minin[i]) /* SOURCE component */
	    regret1 = comp_minin[i];
	  if ((comp_type[i]&2) && regret1<comp_minout[i]) /* SINK component */
	    regret1 = comp_minout[i];
	}
      } while (regret0 < regret1);
#endif
      for (i=0, j=0; i<nn; i++)
	if (regret[i]<=threshold)
	  regarr[j++] = regret[i];
      nregrets = j;
      fd_qsort_asc_long(wam, regarr, nregrets);
      for (i=0, j=0; i<nregrets; i++)
	if (i==0 || regarr[i]>regarr[i-1])
	  regarr[j++] = regarr[i];
      nregrets = j;

      inf = 0;
      sup = nregrets;
      while (inf<sup) {
	mid = (inf+sup)>>1;
	if (!cost_cycle_feasible(pdata,regarr[mid],map,b))
	  inf = mid+1;
	else
	  sup = mid;
      }
      optreg = inf<nregrets ? regarr[inf] : CLPFD_MAXINT2;
#if 0 /* MAY COME IN HANDY */
      /* Compute a min-cost MST with Kruskal's algorithm. */
      if (b==0 && n>2 && inf<nregrets) {
	SP_integer mstcost = pdata->rc.costbase;
	int *set = (int *)head;
	int *indexarr = (int *)regarr;
	int ncomp = n;
	int untaken = -1;
	int nedges;
	
	for (i=n-1; i>=0; i--)
	  set[i] = i;		/* poor man's Union-Find */
	for (i=0, j=0; i<nn; i++) {
	  if (regret[i]<=optreg) {
	    SP_integer cost, altcost;
	    int u, v, altu, altv;

	    u = i/n;
	    v = i%n;
	    altu = map[v];
	    altv = invmap[u];
	    cost =  -pdata->rc.cost[i];
	    altcost =  -pdata->rc.cost[n*altu + altv];
	    if (cost<altcost || (cost==altcost && u<v))
	      indexarr[j++] = i;
	  }
	}
	nedges = j;
	qsort_asc_cost(wam, indexarr, nedges);
	for (i=0; i<nedges && ncomp>1; i++) {
	  int u, v;
	  SP_integer cost;
	  
	  u = v = indexarr[i];
	  cost = -pdata->rc.cost[u]; /* rc.cost is negated */
	  u /= n;
	  v = map[v%n];
	  while (set[u]!=u)
	    u = set[u];
	  while (set[v]!=v)
	    v = set[v];
	  if (u==v) {
	    if (untaken == -1)
	      untaken = indexarr[i];
	  } else {
	    ncomp--;
	    mstcost += cost;
	    if (u<v)
	      set[v] = u;
	    else
	      set[u] = v;
	  }
	}
	/* add cost of one more arc */
	if (untaken == -1 && i<nedges)
	  untaken = indexarr[i];
	if (untaken != -1)
	  mstcost -= pdata->rc.cost[untaken];/* rc.cost is negated */
      }
#endif
      Free(regarr);
      if (inf==nregrets)
	return FALSE;
      if (optreg!=0) {
	if (b==1)
	  optreg = -optreg;
	bound[b] += optreg;
	pdata->rc.min_regret[b] = 0;
      }
    }

  /* PRUNE COST VAR */
  if (!dvar_contains_value_l(pdata->rc.costvar,bound[0])) {
    if (pdata->rc.min_regret[0]==CLPFD_MAXINT2)
      return FALSE;
    bound[0] += pdata->rc.min_regret[0];
  }
  switch (dvar_fix_min_l(pdata->rc.costvar,bound[0])) {
  case -1:
    return FALSE;
  case 0:
    break;
  default:
    pruned = TRUE;
  }

  if (!dvar_contains_value_l(pdata->rc.costvar,bound[1])) {
    if (pdata->rc.min_regret[1]==CLPFD_MAXINT2)
      return FALSE;
    bound[1] -= pdata->rc.min_regret[1];
  }
  switch (dvar_fix_max_l(pdata->rc.costvar,bound[1])) {
  case -1:
    return FALSE;
  case 0:
    break;
  default:
    pruned = TRUE;
  }

  pdata->pruned += pruned;
  *change = pruned;
  return TRUE;
}


static SP_BOOL cost_filter(Wam wam,
			   struct all_dist_data *pdata, 
			   SP_integer costbase,
			   int flags)
{
  int nvartargets = pdata->nvartargets;
  int nvaltargets = pdata->nvaltargets;
  SP_BOOL ok, change;
  int i, n;
  
  dvar_refresh(pdata->rc.costvar);

  if (pdata->incremental)
    n = pdata->rc.ntargets;
  else {
    pdata->rc.costbase = costbase;
    for (i=0, n=0; i<nvartargets; i++) {
      VERTEX vertex = TVAR(i);
      pdata->rc.var_index[vertex] = (int)n;
      pdata->rc.index_var[n++] = (int)vertex;
    }
    for (i=0; i<nvaltargets; i++) {
      VERTEX vertex = TVAL(i);
      pdata->rc.var_index[vertex] = (int)n;
      pdata->rc.index_var[n++] = (int)vertex;
    }
    n >>= 1;
    pdata->rc.ntargets = (int)n;
  }
  pdata->rc.cost = Malloc(n*n,SP_integer); /* pos. or neg. submatrix */
  pdata->rc.dist = Malloc(n*n,SP_integer); /* distance matrix */
  pdata->rc.label[0] = Malloc(n,int);
  pdata->rc.label[1] = Malloc(n,int);
  pdata->rc.queue = Malloc(2*n,int);
  pdata->rc.index = Malloc(n,int);
  pdata->rc.heap = Malloc(n,int);
  pdata->rc.vheap = Malloc(n,int);
  ok = TRUE;
  do {
    change = FALSE;
    if (!cost_based_filter(wam, pdata,0) ||
	!cost_based_filter(wam, pdata,1) ||
	!cost_based_pruning(wam, pdata,flags,&change))
      ok = FALSE;
  } while (ok && change);
  SP_free(pdata->rc.cost);
  SP_free(pdata->rc.dist);
  SP_free(pdata->rc.label[0]);
  SP_free(pdata->rc.label[1]);
  SP_free(pdata->rc.queue);
  SP_free(pdata->rc.index);
  SP_free(pdata->rc.heap);
  SP_free(pdata->rc.vheap);
  return ok;
}

void SPCDECL 
prolog_fd_assignment_helper(Wam wam,
			    SP_term_ref State0,
			    SP_term_ref State,
			    SP_term_ref Actions)
{
  int nvars, n, ix, ival;
  TAGGED tstamp, thandle, tval;
  SP_integer lreg, ureg;
  int ent = 1;
  struct all_dist_data *pdata;
  Dvar dv;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
  dvar_export_start(wam);

/*    X(0) = RefTerm(State0); */
  DerefNonvar(X(0));
  RefTerm(State) = X(0);	/* f(Index,Var,StateMutable) */
  tval = fd_deref(wam, CTagToArg(X(0),2));
  if (IsVar(tval)) {
    ent = 0; goto ret;
  }
  ix = GetSmall_int(CTagToArg(X(0),1));
  ival = GetSmall_int(tval);
  thandle = RefMutable(CTagToArg(X(0),3));
  DerefArg(tstamp,thandle,ASSIGNMENT_ARITY);
  DerefArg(thandle,thandle,ASSIGNMENT_ARITY-1);
  if (thandle==TaggedZero)
    goto ret;
  pdata = Pdata(struct all_dist_data,thandle);
  if (GetSmall(tstamp)!=pdata->stamp)
    goto ret;
  n = pdata->rc.ntargets;
  nvars = pdata->nvars;
  if (VISITED(ix)==EOL)
    goto ret;			/* nothing to do */
  ix = pdata->rc.var_index[ix];
  ival = pdata->rc.var_index[ival+nvars-1]-n;
  lreg = pdata->rc.rcost[0][n*ix + ival];
  ureg = pdata->rc.rcost[1][n*ix + ival];
  if (lreg==CLPFD_MAXINT2 || ureg==CLPFD_MAXINT2) {
    ent = -1; goto ret;
  }
  dv = pdata->rc.costvar;
  dvar_refresh(dv);
  if (dvar_fix_interval_l(dv, pdata->rc.optcost[0]+lreg,
			      pdata->rc.optcost[1]-ureg) == -1) {
    ent = -1; goto ret;
  }
  dvar_pruning_done( dv);
  dvar_export(dv);
 ret:
  dvar_export_done(wam, Actions, ent);
}
