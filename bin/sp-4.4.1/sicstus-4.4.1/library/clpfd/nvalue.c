/* Copyright(C) 2001, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

typedef SP_integer VERTEX;

struct nvalue_data
{
  void (SPCDECL *destructor)(void*); /* [PM] 3.9b4 changed name to destructor for consistency */
  SPEnv *spenv;

  SP_globref refbase;
  SP_integer stamp;
  int nvars;
  int nvals;
  int nnodes;
  int nrefs;
  int nvartargets;
  int nvaltargets;
  int scc_component;
  int scc_index;
  int scc_visited;
  Dvar dvar;			/* nvals+nvars+2 */
  VERTEX *edge;                 /* 2*nvars*nvals */
  VERTEX *iset;			/* nvars */
  VERTEX *vartarget;		/* nvars */
  VERTEX *valtarget;		/* nvals */
  VERTEX *stack;                /* 3*nnodes, volatile */
  char *igraph;			/* intersection graph */
  struct {                      /* source, vals, vars, target */
    TAGGED *val;		/* the FD value for value vertices */
    SP_integer *visited;	        /* auxiliary variable for some algorithms */
    SP_integer *component;	        /* auxiliary variable for some algorithms */
    SP_integer *mate;			/* mate of this vertex in matching ([PM] 4.2.1+ should this be VERTEX?) */
    SP_integer *out_degree;           /* # outbound edges (forward edges) */
    SP_integer *in_degree;            /* #  inbound edges (backward edges) */
    VERTEX **neighs;            /* array of pointers to the neighbours */
  } vertex;
};

#define RefAttr(V) (pdata->refbase + 2*(V))
#define RefVar(V) (pdata->refbase + 2*(V) + 1)
#define DVAR(V) (pdata->dvar+(V))

#define VAL(V) (pdata->vertex.val[V])
#define VISITED(V) (pdata->vertex.visited[V])
#define COMPONENT(V) (pdata->vertex.component[V])
#define MATE(V) (pdata->vertex.mate[V])
#define NEIGHS(V) (pdata->vertex.neighs[V])
#define NEIGH(V,I) (pdata->vertex.neighs[V][I])
#define OUT_DEGREE(V) (pdata->vertex.out_degree[V])
#define IN_DEGREE(V) (pdata->vertex.in_degree[V])

#define VALKEY(I) ((I)-1)
#define VARKEY(I) ((I)-nvals-1)
#define VAROF(x) ((x) / nvals)
#define VALOF(x) ((x) % nvals)
#define EOL ((VERTEX)-1)

#define set_mates(n1,n2) MATE(n1) = (n2), MATE(n2) = (n1)

#define exposed(vertex) (MATE(vertex) == EOL)

/*
  These are values that are NOT fd integers.  We set the value fields of
  the start, variable and target vertices to these values.  They are not
  essential, actually, we never test if a gien vertex has one of these
  values.
*/
static TAGGED const START_VERTEX  = SupAsINT-IStep(2);
static TAGGED const VAR_VERTEX    = SupAsINT-IStep(1);
static TAGGED const TARGET_VERTEX = SupAsINT-IStep(0);

static void SPCDECL 
nvalue_destructor(void *pdata_v)
{
  struct nvalue_data *pdata = (struct nvalue_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase, pdata->nrefs);
  SP_free(pdata);
}


/* At present, called only from one place. Could be INLINEd. */
static struct nvalue_data *
nvalue_alloc(Wam wam, 
	     int nvars,
	     int nvals,
	     TAGGED handle)
{
  char *ptr;
  int const nnodes = nvars+nvals+2;
  int const ndvars = nvars+1;
  SP_integer const total_size =
    ndvars*sizeof(struct dvar) +
    6*nnodes*sizeof(SP_integer) +
    nnodes*sizeof(VERTEX *) +
    nvars*sizeof(VERTEX) +
    nvars*sizeof(VERTEX) +
    nvals*sizeof(VERTEX) +
    (nvals+nvals*nvars+nvars+1)*2*sizeof(VERTEX) +
    nvars*nvars;

  struct nvalue_data *pdata =
    Palloc(struct nvalue_data, total_size, handle);
  pdata->destructor = nvalue_destructor;
  FD_STORE_SPENV(pdata->spenv);
  pdata->nvals = nvals;
  pdata->nvars = nvars;
  pdata->nnodes = nnodes;
  pdata->nrefs = 2*ndvars;
  pdata->nvartargets = nvars;
  pdata->nvaltargets = nvals;
  pdata->refbase = SP_alloc_globrefs(2*ndvars);
  ptr = (char *)(pdata+1);
  pdata->dvar = (Dvar)ptr;
  ptr += ndvars*sizeof(struct dvar);
  pdata->vertex.val = (TAGGED *)ptr;
  ptr += nnodes*sizeof(TAGGED);
  pdata->vertex.neighs = (VERTEX **)ptr;
  ptr += nnodes*sizeof(VERTEX *);
  pdata->vertex.visited = (SP_integer *)ptr;
  ptr += nnodes*sizeof(SP_integer);
  pdata->vertex.component = (SP_integer *)ptr;
  ptr += nnodes*sizeof(SP_integer);
  pdata->vertex.mate = (SP_integer *)ptr;
  ptr += nnodes*sizeof(SP_integer);
  pdata->vertex.out_degree = (SP_integer *)ptr;
  ptr += nnodes*sizeof(SP_integer);
  pdata->vertex.in_degree  = (SP_integer *)ptr;
  ptr += nnodes*sizeof(SP_integer);
  pdata->iset  = (VERTEX *)ptr;
  ptr += nvars*sizeof(VERTEX);
  pdata->vartarget  = (VERTEX *)ptr;
  ptr += nvars*sizeof(VERTEX);
  pdata->valtarget  = (VERTEX *)ptr;
  ptr += nvals*sizeof(VERTEX);
  pdata->edge = (VERTEX *)ptr;
  ptr += (nvals+nvals*nvars+nvars+1)*2*sizeof(VERTEX);
  pdata->igraph = ptr;
  ptr += nvars*nvars;
  SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
  return pdata;
}

static void 
nvalue_init(struct nvalue_data *pdata,
	    TAGGED vals)
{
  int i, j, vx, nx=0, nvals=pdata->nvals, nvars=pdata->nvars;
  int nnodes = pdata->nnodes;
  FDITER it;

  /* source vertex */
  NEIGHS(0) = pdata->edge;
  OUT_DEGREE(0) = nvals;
  IN_DEGREE(0) = 1;
  VAL(0) = START_VERTEX;
  for (i=0; i<nvals; i++)
    NEIGH(0, i) = i+1;
  NEIGH(0, i) = nnodes-1;
  nx = nvals+1;

  /* value vertices */
  fditer_init(&it, vals);
  vx = 1;
  j = 0;
  while (!fditer_empty(&it)) {
    NEIGHS(vx) = pdata->edge + nx;
    NEIGH(vx, 0) = 0;
    OUT_DEGREE(vx) = 0;
    IN_DEGREE(vx) = 1;
    VAL(vx) = fditer_next(&it);
    MATE(vx) = EOL;
    pdata->valtarget[j++] = vx++;
    nx += nvars+1;
  }

  /* var vertices */
  for (j=0, vx=nvals+1; vx<nnodes-1; j++, vx++) {
    NEIGHS(vx) = pdata->edge + nx;
    NEIGH(vx, 0) = nnodes-1;
    OUT_DEGREE(vx) = 1;
    IN_DEGREE(vx) = 0;
    VAL(vx) = VAR_VERTEX;
    MATE(vx) = EOL;
    pdata->vartarget[j] = vx;
    nx += nvals+1;
  }

  /* target vertex */
  NEIGHS(vx) = pdata->edge + nx;
  OUT_DEGREE(vx) = 1;
  IN_DEGREE(vx) = nvars;
  VAL(vx) = TARGET_VERTEX;
  NEIGH(vx,0) = 0;
  for (i = 1; i <= nvars; i++)
    NEIGH(vx, i) = nvals+i;
}


static int 
nvalue_apply_delta(struct nvalue_data *pdata,
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
  igraph = (int)(j>=degree ? eov : neighs[j++]);
  while (idelta!=eov || igraph!=eov)
    if (idelta<igraph) {	/* domain edge, no graph edge (only noninc) */
      target[k++] = idelta;
      idelta = d==dmax ? (int)eov :
	       !valp ? delta[d++]%nvals + 1 : delta[d++]/nvals + nvals + 1;
      if (d<dmax && (valp ? delta[d]%nvals + 1 : delta[d]/nvals + nvals + 1)!=v)
	dmax = d;
    } else if (idelta>igraph) { /* graph edge, no domain edge */
      target[k++] = igraph;
      igraph = (int)(j>=degree ? eov : neighs[j++]);
    } else {
      idelta = d==dmax ? (int)eov :
	       !valp ? delta[d++]%nvals + 1 : delta[d++]/nvals + nvals + 1;
      if (d<dmax && (valp ? delta[d]%nvals + 1 : delta[d]/nvals + nvals + 1)!=v)
	dmax = d;
      igraph = (int)(j>=degree ? eov : neighs[j++]);
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
  Recompute the variable part of the graph repr.
*/
static void 
nvalue_refresh(Wam wam,
		      struct nvalue_data *pdata,
		      SP_BOOL incremental)
{
  int i, d, dmax, *delta, *delta2;
  int nvars = pdata->nvars;
  int nvals = pdata->nvals;
  int nedges = nvars*nvals;

  delta = Malloc(nedges*2,int);
  delta2 = delta+nedges;

  /* I: Compute Delta = (domain edges) XOR (graph edges), by ascending VAR vertex. */
  d = 0;
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    if (!incremental || dvar_value_count(DVAR(var-nvals)) < IN_DEGREE(var)) {
      int degree = (int)IN_DEGREE(var);
      int j=0;
      int vbase = (int)((var-nvals-1)*nvals);
      DVITER it;
      TAGGED tdom, tgraph;
      VERTEX cur=1;
      VERTEX *neighs = NEIGHS(var)+1;

      dviter_init(&it, DVAR(var-nvals));
      tdom = dviter_next_value_t(&it);
      tgraph = j>=degree ? SupAsINT : VAL(neighs[j]);
      while (tdom!=SupAsINT || tgraph!=SupAsINT)
	if (Tlt(tdom,tgraph)) {	/* domain edge, no graph edge (only noninc) */
	  while (VAL(cur) != tdom)
	    cur++;
	  delta[d++] = (int)(vbase + cur - 1);
	  tdom = dviter_empty(&it) ? SupAsINT : dviter_next_value_t(&it);
	} else if (Tgt(tdom,tgraph)) { /* graph edge, no domain edge */
	  cur = neighs[j];
	  delta[d++] = (int)(vbase + cur - 1);
	  j++;
	  tgraph = j>=degree ? SupAsINT : VAL(neighs[j]);
	} else {
	  cur = neighs[j];
	  j++;
	  tdom = dviter_empty(&it) ? SupAsINT : dviter_next_value_t(&it);
	  tgraph = j>=degree ? SupAsINT : VAL(neighs[j]);
	}
    }
  }
  dmax = d;
  
  /* IIa. Keysort Delta by ascending VAR vertex. */
  KEYSORT(delta, dmax, int, (int *)pdata->stack, nvars, VAROF, delta2);

  /* IIb. XOR Delta into VAR adjacency lists. */
  d = 0;
  while (d<dmax)
    d += nvalue_apply_delta(pdata, delta2+d, dmax-d, FALSE, incremental);

  /* IIIa. Keysort Delta by ascending VAL vertex. */
  KEYSORT(delta2, dmax, int, (int *)pdata->stack, nvals, VALOF, delta);

  /* IIIb. XOR Delta into VAL adjacency lists. */
  d = 0;
  while (d<dmax)
    d += nvalue_apply_delta(pdata, delta+d, dmax-d, TRUE, incremental);

  SP_free(delta);

  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    VERTEX mate = MATE(var);
    
    if (mate>EOL && !dvar_contains_value_t(DVAR(var-nvals),VAL(mate)))
      MATE(var) = MATE(mate) = EOL;
  }
}

/* Delete all edges linking different components number do not match. */
static SP_BOOL
compress_edges(Wam wam,
		      struct nvalue_data *pdata)
{
  int i, d, dmax, *delta, *delta2;
  int nvars = pdata->nvars;
  int nvals = pdata->nvals;
  int nedges = nvars*nvals;

  delta = Malloc(nedges*2,int);
  delta2 = delta+nedges;

  /* I: Compute Delta = (deleted edges) */
  d = 0;
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    int compu = (int)COMPONENT(var);
    int degree = (int)IN_DEGREE(var);
    int j=0;
    int ubase = (int)((var-nvals-1)*nvals);
    VERTEX *neighs = NEIGHS(var)+1;

    for (j=0; j<degree; j++) {
      VERTEX v = neighs[j];
      if (COMPONENT(v)!=compu)
	delta[d++] = (int)(ubase + v - 1);
    }
  }
  dmax = d;
  
  /* IIa. Keysort Delta by ascending VAR vertex. */
  KEYSORT(delta, dmax, int, (int *)pdata->stack, nvars, VAROF, delta2);

  /* IIb. XOR Delta into VAR adjacency lists. */
  d = 0;
  while (d<dmax)
    d += nvalue_apply_delta(pdata, delta2+d, dmax-d, FALSE, TRUE);

  /* IIIa. Keysort Delta by ascending VAL vertex. */
  KEYSORT(delta2, dmax, int, (int *)pdata->stack, nvals, VALOF, delta);

  /* IIIb. XOR Delta into VAL adjacency lists. */
  d = 0;
  while (d<dmax)
    d += nvalue_apply_delta(pdata, delta+d, dmax-d, TRUE, TRUE);
  
  SP_free(delta);
  return (dmax>0);
}


/* precond: n>=1 */
static TAGGED 
nvalue_new_domain(Wam wam,
		  struct nvalue_data *pdata,
		  VERTEX var)
{
  int i = 1;
  int last = (int)IN_DEGREE(var)+1;
  FDCONS cons;

  fdcons_init(&cons);
  while (i<last)
    fdcons_add(wam, &cons,VAL(NEIGHS(var)[i++]));
  return fdcons_set(&cons);
}


static SP_BOOL augment_path(struct nvalue_data *pdata,
			    VERTEX var)
   /* BFS algorithm for finding an augmenting path. 
      If successful, updates the graph to reflect the new matching.
      Return TRUE or FALSE, depending on whether an augmenting path is
      found.
      */
{
  int p=0, q=0, j;
  int nvars = pdata->nvars;
  VERTEX *vstack = pdata->stack;
  VERTEX now = var;		

  vstack[q++] = var;
  while (p<q) {
    var = vstack[p++];
    {
      int outdeg = (int)OUT_DEGREE(var);
      int indeg = (int)IN_DEGREE(var);

      for (j=0; j<indeg; j++) {
        VERTEX neighbor = NEIGH(var,outdeg+j);
        VERTEX newvertex = MATE(neighbor);
        if (newvertex==EOL) {
          for (;;) {
            newvertex = MATE(var);
            set_mates(var,neighbor);
            if (newvertex==EOL)
              return TRUE;
            neighbor = newvertex;
            var = vstack[newvertex+nvars]; /* FATHER(newvertex) */
          }
        } else if (VISITED(newvertex)!=now) {
          VISITED(newvertex) = now;
          vstack[neighbor+nvars] = var; /* FATHER(neighbor) */
          vstack[q++] = newvertex;
        }
      }
    }
  }
  return FALSE;
}


/* Find maximum matching by repeatedly finding augmenting paths in the graph.  */
/* Return size of matching. */
static int max_matching(struct nvalue_data *pdata)
{
  int i, j, first_exp, size;
  int nvals = pdata->nvals;
  int nkernel = pdata->nvartargets;
  VERTEX val;
  TAGGED seed=0;
				/* find kernel vars */
  do {
    int inf = 0;
    int sup = nkernel-1;
    VERTEX held = pdata->vartarget[sup]; /* sup is the hole */
    VERTEX current = pdata->vartarget[inf];
    
    size = pdata->nvartargets - nkernel;
    while (inf<=sup) {
      if (IN_DEGREE(current)<=nkernel) {
	pdata->vartarget[inf] = current;
	inf++;
	current = (inf>=sup ? held : pdata->vartarget[inf]);
      } else {
	pdata->vartarget[sup] = current;
	sup--;
	current = (inf>=sup ? held : pdata->vartarget[sup]);
      }
    }
    nkernel = inf;
  } while (size < pdata->nvartargets - nkernel);
				/* ensure non-kernel vars are not matched */
  for (i=nkernel; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    VERTEX mate = MATE(var);
    if (mate!=EOL)
      MATE(mate)=EOL, MATE(var)=EOL;
  }

				/* greedy phase */
  first_exp = nkernel;
  for (i=0; i<nkernel; i++) {
    VERTEX var = pdata->vartarget[i];
    int outdeg = (int)OUT_DEGREE(var);
    VISITED(var) = 0;	/* reset VISITED for all variable nodes */
    if (exposed(var)) {
      if (first_exp>i) {
	first_exp = i;
	seed = dvar_min_t(DVAR(var-nvals));
	val = NEIGH(var,outdeg);
	if (exposed(val)) {
	  size++;
	  set_mates(var,val);
	}
      } else {
	TAGGED seed2 = dvar_successor_t(DVAR(var-nvals),seed);
	if (TagIsSmall(seed2)) {
	  seed = seed2;
	  val = 0;
	  for (j=0; VAL(val)!=seed; j++)
	    val = NEIGH(var,outdeg+j);
	  if (exposed(val)) {
	    size++;
	    set_mates(var,val);
	  }
	}
      }
    } else {
      size++;
    }
  }
				/* augmenting phase */
  for (i=first_exp; i<nkernel; i++) {
    VERTEX var = pdata->vartarget[i];
    if (exposed(var))
      size += augment_path(pdata,var);
  }
				/* match non-kernel vars */
  for (i=nkernel; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    int indeg = (int)IN_DEGREE(var);
    for (j=0; j<indeg; j++) {
      VERTEX val = NEIGH(var,j+1);
      if (exposed(val)) {
	set_mates(var,val);
	break;
      }
    }
    if (exposed(var))
      printf("! non-kernel match failed\n");
  }
  return size;
}

static int visit(Wam wam,
			struct nvalue_data *pdata,
			VERTEX val,
			int *ix1,
			int *ix2)
   /* A recursive function that finds strongly connected components
      [Tarjan'72]. Code taken roughly from [Sedgewick, Algorithms in C,
      page 482].
      One big difference is that we only step on value vertices, i.e. we
      jump directly from variable vertices using the mate ptr.
      */
{
  VERTEX newvertex=EOL;		/* value */
  VERTEX mate;			/* variable */
  int m, min, scc, indeg, outdeg, j;
  VERTEX *dist_stack = pdata->stack;
   
  VISITED(val) = ++pdata->scc_visited;		/* Mark vertex as the id'th visited */
  min = pdata->scc_visited;
  dist_stack[pdata->scc_index++] = val;
  mate = MATE(val);
  outdeg = (int)OUT_DEGREE(mate);
  indeg = (int)IN_DEGREE(mate);
  for (j=0; j<indeg; j++) {
    newvertex = NEIGH(mate,outdeg+j);
    if (newvertex != val) {
      m = (VISITED(newvertex) == 0) ? 
	visit(wam, pdata,newvertex,ix1,ix2) : (int)VISITED(newvertex);
      if (m < min)
	min = m;
    }
  }
  if (min == VISITED(val)) {
    scc = ++pdata->scc_component;
    do {
      /* Each descendant on the dist_stack is part of this vertex's SCC. */
      newvertex = dist_stack[--pdata->scc_index];
      mate = MATE(newvertex);
      COMPONENT(newvertex) = scc;
      COMPONENT(mate) = scc;
      VISITED(newvertex)=0xffffff; /* High value, so that
				      this vertex will be ignored
				      in the future search. */
    } while (newvertex != val);
  }
  return min;
}

/* Returns #scc. */
static int find_sccs(Wam wam,
			    struct nvalue_data *pdata,
			    int sccs0)
   /* Marks all edges belonging to any strongly connected component in
      the graph. The strongly connected components are calculated using
      Tarjan's depth-first search based algorithm from 1972. */
{
  int i, ix1, ix2;
  int sccs = sccs0;
  SP_integer mindeg = CLPFD_MAXINT;
  SP_integer degree;
  int nnonground = 0;
     
  for (i=0; i<pdata->nvaltargets; i++) {
    VERTEX val = pdata->valtarget[i];
    VERTEX mate = MATE(val);
    VISITED(val) = 0xffffff; /* disable visiting this value node */
    if (COMPONENT(val)==0) {
      degree = IN_DEGREE(mate);
      if (degree==1) {
	COMPONENT(mate) = ++sccs; /* singleton SCC */
	COMPONENT(val) = sccs;
	mindeg = 1;
      } else {
	VISITED(val) = 0;	/* enable visiting this value node */
	nnonground++;
	if (mindeg > degree)
	  mindeg = degree;
      }
    }
  }
  if (nnonground==0)		/* all vars fixed */
    return sccs - sccs0;
  if (mindeg!=nnonground || TRUE) {
    pdata->scc_component = sccs;
    pdata->scc_visited = 0;
    pdata->scc_index = 0;
    ix1 = ix2 = 0;
    for (i=0; i<pdata->nvartargets; i++) {
      VERTEX var = pdata->vartarget[i];
      VERTEX mate = MATE(var);
      if (mate!=EOL && !VISITED(mate))
	visit(wam, pdata,mate,&ix1,&ix2);
    }
    sccs = pdata->scc_component;
  } else {
    sccs++;			/* this opt. is not valid in the presence of exposed var vertices */
    for (i=0; i<pdata->nvartargets; i++) {
      VERTEX var = pdata->vartarget[i];
      VERTEX mate = MATE(var);
      if (mate!=EOL && COMPONENT(var)==0) {
	COMPONENT(var) = sccs;
	COMPONENT(mate) = sccs;
      }
    }
  }
  return sccs - sccs0;
}

/* Component 1 = vertices reachable from an exposed var vertex. */
/* Component 2 = vertices reachable from an exposed val vertex. */
/* Component 3 and up = an scc in the residue. */
static void matching_prune(Wam wam,
				  struct nvalue_data *pdata,
				  int mu) /* size of matching */
{
  int nvals = pdata->nvals;
  VERTEX *cands = pdata->stack;
  int i, p=0, q=0;

  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    if (exposed(var)) {
      COMPONENT(var) = 1;
      VISITED(var) = 1;
      cands[q++] = var;
    } else {
      COMPONENT(var) = 0;	/* 0 means available in find_sccs */
      VISITED(var) = 0;
    }
  }
  for (i=0; i<pdata->nvaltargets; i++) {
    VERTEX val = pdata->valtarget[i];
    if (exposed(val)) {
      COMPONENT(val) = 2;
      VISITED(val) = 1;
      cands[q++] = val;
    } else {
      COMPONENT(val) = 0;	/* 0 means available in find_sccs */
      VISITED(val) = 0;
    }
  }
  while (p<q) {
    VERTEX vertex = cands[p++];
    int valp = (vertex<nvals+1);
    int minj = (valp ? 0 : (int)OUT_DEGREE(vertex));
    int maxj = (int)((valp ? OUT_DEGREE(vertex) : minj+IN_DEGREE(vertex)));
    int j;
    for (j=minj; j<maxj; j++) {
      VERTEX neighbor = NEIGH(vertex,j);
      VERTEX mate = MATE(neighbor);
      if (mate!=EOL && VISITED(mate)==0) {
	COMPONENT(neighbor) = valp+1;
	COMPONENT(mate) = valp+1;
	VISITED(mate) = 1;
	cands[q++] = mate;
	mu--;
      }
    }
  }
  if (mu>0)
    find_sccs(wam, pdata,2);
}


/* sort by ascending min */
static int 
cmp_asc_min(Wam wam,
		   VERTEX *v1, VERTEX *v2)
{
  struct nvalue_data *pdata = fd.gdata;

  TAGGED d1 = NEIGH(*v1,1);
  TAGGED d2 = NEIGH(*v2,1);

  return (d1<d2) ? -1 : (d1>d2) ? 1 : 0;
}

#define QType VERTEX
#define QCmp  cmp_asc_min
#define QSort qsort_asc_min
#include "qsort.ic"

/* Compute lb of hitting set based on ordered intervals. */
static int ordered_intervals(Wam wam,
				    struct nvalue_data *pdata,
				    int nkvars)
{
  int ndistinct = 0;
  int i;
  VERTEX low = pdata->nnodes;
  VERTEX up  = 0;

  qsort_asc_min(wam, pdata->vartarget,nkvars);

  for (i=0; i<nkvars; i++) {
    VERTEX ui = pdata->vartarget[i];
    VERTEX minui = NEIGH(ui,1);
    VERTEX maxui = NEIGH(ui,IN_DEGREE(ui));

    low = low > minui ? low : minui;
    up = up   < maxui ? up  : maxui;
    if (low>up) {
      low = minui;
      up  = maxui;
      ndistinct++;
    }
  }
  return ndistinct;
}

static void ordered_intervals_prune(struct nvalue_data *pdata,
				    int nkvars,
				    int nkvals)
{
  int i, j, ix;
  VERTEX *kinf = pdata->stack;
  VERTEX *ksup = pdata->stack + pdata->nnodes;
  VERTEX low = pdata->nnodes;
  VERTEX up  = 0;

  for (i=0, j=0, ix=0; i<nkvars; i++) {
    VERTEX ui = pdata->vartarget[i];
    VERTEX minui = NEIGH(ui,1);
    VERTEX maxui = NEIGH(ui,IN_DEGREE(ui));

    low = low > minui ? low : minui;
    up = up   < maxui ? up  : maxui;
    if (low>up) {
      low = minui;
      up  = maxui;
      if (j<i) {
	kinf[ix] = 0;
	ksup[ix] = pdata->nnodes;
	for (; j<i; j++) {
	  VERTEX uj = pdata->vartarget[j];
	  VERTEX minuj = NEIGH(uj,1);
	  VERTEX maxuj = NEIGH(uj,IN_DEGREE(uj));

	  if (maxuj<minui)
	    kinf[ix] = minuj > kinf[ix] ? minuj : kinf[ix];
	  ksup[ix] = maxuj < ksup[ix] ? maxuj : ksup[ix];
	}
	ix++;
      }
    }
  }
  kinf[ix] = 0;
  ksup[ix] = pdata->nnodes;
  for (; j<i; j++) {
    VERTEX uj = pdata->vartarget[j];
    VERTEX minuj = NEIGH(uj,1);
    VERTEX maxuj = NEIGH(uj,IN_DEGREE(uj));

    kinf[ix] = minuj > kinf[ix] ? minuj : kinf[ix];
    ksup[ix] = maxuj < ksup[ix] ? maxuj : ksup[ix];
  }
  ix++;

  /* Keep (a) the "ground" values, (b) the values in the kernel intervals */
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    COMPONENT(var) = 1;
  }
  for (i=0; i<pdata->nvaltargets; i++) {
    VERTEX val = pdata->valtarget[i];
    COMPONENT(val) = i<nkvals ? 2 : 1; /* COMPONENT=1 means keep */
  }
  for (i=0; i<ix; i++)
    for (low=kinf[i]; low<=ksup[i]; low++)
      COMPONENT(low) = 1;
}

static int neighbors_intersect(struct nvalue_data *pdata,
			       VERTEX v1,
			       VERTEX v2)
{
  int i1 = 1;
  int i2 = 1;
  int deg1 = (int)IN_DEGREE(v1)+1;
  int deg2 = (int)IN_DEGREE(v2)+1;

  while (i1<deg1 && i2<deg2) {
    int n1 = (int)NEIGH(v1,i1);
    int n2 = (int)NEIGH(v2,i2);
    if (n1<n2)
      i1++;
    else if (n1>n2)
      i2++;
    else
      return TRUE;
  }
  return FALSE;
}


/* Find a large independent set in the intersection graph induced
   by the value graph. */
static int large_independent_set(struct nvalue_data *pdata,
				 int nkvars)
{
  int i, j, k=0;
  int nvars = pdata->nvars;
  int nvals = pdata->nvals;
  int size = 0;
  int arroffset = (0-nvals-1);	/* vertex + arroffset = 0-based index */
  int matoffset = (nvars+1)*arroffset; /* nvars*v1 + v2 + matoffset = 0-based index */
  SP_integer *degree = pdata->stack;
  SP_integer *vertex = pdata->stack+nvars;

  for (i=0; i<nkvars; i++) {
    VERTEX v1 = pdata->vartarget[i];
    vertex[i] = v1;
    degree[v1 + arroffset] = 0;
  }

  /* Refresh intersection graph. */
  for (i=0; i<nkvars-1; i++) {
    VERTEX v1 = pdata->vartarget[i];
    for (j=i+1; j<nkvars; j++) {
      VERTEX v2 = pdata->vartarget[j];
      int value = neighbors_intersect(pdata,v1,v2);
      pdata->igraph[nvars*v1 + v2 + matoffset] = (char)value;
      pdata->igraph[nvars*v2 + v1 + matoffset] = (char)value;
      degree[v1 + arroffset] += value;
      degree[v2 + arroffset] += value;
    }
  }

  /* Greedy phase. */
  while (nkvars>0) {
    VERTEX best = vertex[0];
    VERTEX held, current;
    int inf, sup;
    
    size++;
    for (i=1; i<nkvars; i++) {
      VERTEX v = vertex[i];
      if (degree[best + arroffset] > degree[v + arroffset])
	best = v;
    }
    
    /* remove best and its neighbors */
    inf = 0;
    sup = nkvars-1;
    held = vertex[sup]; /* sup is the hole */
    current = vertex[inf];
    
    while (inf<=sup) {
      if (current!=best && !pdata->igraph[nvars*current + best + matoffset]) {
	vertex[inf] = current;
	inf++;
	current = (inf>=sup ? held : vertex[inf]);
      } else {
	for (i=0; i<nkvars; i++) {
	  VERTEX var = pdata->vartarget[i];
	  if (var != current)
	    degree[var + arroffset] -= pdata->igraph[nvars*current + var + matoffset];
	}
	pdata->iset[k++] = current;
	COMPONENT(current) = size;
	VISITED(current) = (current==best);
	vertex[sup] = current;
	sup--;
	current = (inf>=sup ? held : vertex[sup]);
      }
    }
    nkvars = inf;
  }
  return size;
}

static SP_BOOL can_swap_in_iset(struct nvalue_data *pdata,
				VERTEX v,
				int nkvars,
				int comp)
{
  int nvars = pdata->nvars;
  int nvals = pdata->nvals;
  int arroffset = (0-nvals-1);	/* vertex + arroffset = 0-based index */
  int matoffset = (nvars+1)*arroffset; /* nvars*v1 + v2 + matoffset = 0-based index */
  int i;

  for (i=0; i<nkvars; i++) {
    VERTEX var = pdata->vartarget[i];
    if (v!=var && pdata->igraph[nvars*v + var + matoffset] && VISITED(var) && COMPONENT(var)!=comp)
      return FALSE;
  }
  return TRUE;
}

/* VISITED=1 iff var is in independent set */
static void independent_set_prune(struct nvalue_data *pdata,
				  int nkvars,
				  int nkvals)
{
  int i, k, k2;

  for (i=0; i<pdata->nvaltargets; i++) {
    VERTEX val = pdata->valtarget[i];
    COMPONENT(val) = i<nkvals ? 2 : 1; /* COMPONENT=1 means keep */
  }
  for (k=0; k<nkvars; k=k2) {
    int comp = (int)COMPONENT(pdata->iset[k]);
    int visits = 0;

    for (k2=k; k2<nkvars && COMPONENT(pdata->iset[k2])==comp; k2++)
      ;
    for (i=0; i<nkvals; i++) {
      VERTEX val = pdata->valtarget[i];
      VISITED(val) = 0;
    }
    for (i=k; i<k2; i++) {
      VERTEX v = pdata->iset[i];
      if (VISITED(v) || can_swap_in_iset(pdata,v,nkvars,comp)) {
	int j;
	visits++;
	for (j=1; j<=IN_DEGREE(v); j++)
	  VISITED(NEIGH(v,j))++;
      }
    }
    for (i=0; i<nkvals; i++) {
      VERTEX val = pdata->valtarget[i];
      if (VISITED(val)>=visits)
	COMPONENT(val) = 1;
    }
  }
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    COMPONENT(var) = 1;
  }
}

static SP_BOOL
val_is_ground(struct nvalue_data *pdata,
	      VERTEX val)
{
  int degree = (int)OUT_DEGREE(val);
  int j;

  for (j=0; j<degree; j++)
    if (IN_DEGREE(NEIGH(val,j))!=1)
      return FALSE;
  return TRUE;
}

static void 
contract_vals(struct nvalue_data *pdata)
{
  VERTEX *target = pdata->valtarget;
  int inf = 0;
  int sup = pdata->nvaltargets-1;
  VERTEX held = target[sup]; /* sup is the hole */
  VERTEX current = target[inf];
    
  while (inf<=sup) {
    if (!val_is_ground(pdata,current)) {
      VISITED(current) = 1;
      target[inf] = current;
      inf++;
      current = (inf>=sup ? held : target[inf]);
    } else {
      VISITED(current) = 0;
      target[sup] = current;
      sup--;
      current = (inf>=sup ? held : target[sup]);
    }
  }
  pdata->nvaltargets = inf;
}

static void 
contract_vars(struct nvalue_data *pdata)
{
  VERTEX *target = pdata->vartarget;
  int inf = 0;
  int sup = pdata->nvartargets-1;
  VERTEX held = target[sup]; /* sup is the hole */
  VERTEX current = target[inf];
    
  while (inf<=sup) {
    VERTEX y = NEIGH(current,1);
    if (IN_DEGREE(current)>1 || VISITED(y)) {
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

/*
  '$fd_nvalue'(+State0, -State, -Actions) :-
  State0 is f(N,Vars,Handle,Stamp).
  State similarly,
  Actions is a list of prunings etc.
*/
void SPCDECL
prolog_fd_nvalue(Wam wam,
		 SP_term_ref State0,
		 SP_term_ref State,
		 SP_term_ref Actions)
{
  int ground, ent = -1;
  SP_integer state_stamp;
  TAGGED handle;
  struct nvalue_data *pdata;
  SP_BOOL committed, incremental, change;
  int i, j, nvals=0, nvars=0;

/*    X(0) = RefTerm(State0); */
  (void)State0;
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    TAGGED telt;
    pdata = Pdata(struct nvalue_data, handle);
    DerefArg(telt,X(0),3);
    pdata->nvartargets = GetSmall_int(telt);
    DerefArg(telt,X(0),4);
    pdata->nvaltargets = GetSmall_int(telt);
  } else {			/* build persistent state */
				/* compute nvars, nvals, all */
    TAGGED tlvec, telt;
    TAGGED all = EmptySet;

    DerefArg(tlvec,X(0),2);
    while (TagIsLST(tlvec)) {
      TAGGED t1;
      DerefCar(telt,tlvec);
      DerefCdr(tlvec,tlvec);
      DerefArg(t1,telt,2);	/* Attribute */
      DerefAttribute(t1,t1);
      all = fd_merge_into(wam, DomainSet(t1),all);
      nvars++;
    }
    nvals = (int)fd_size(all);
    pdata = nvalue_alloc(wam, nvars, nvals, handle);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    DerefArg(telt,X(0),1);
    fd_get_var_and_attr(telt,RefAttr(0));
    dvar_init(DVAR(0), RefAttr(0), RefVar(0));
    DerefArg(tlvec,X(0),2);
    nvars = 0;
    while (TagIsLST(tlvec)) {
      DerefCar(telt,tlvec);
      DerefCdr(tlvec,tlvec);
      fd_get_var_and_attr(telt,RefAttr(nvars+1));
      dvar_init(DVAR(nvars+1), RefAttr(nvars+1), RefVar(nvars+1));
      nvars++;
    }
    nvalue_init(pdata, all);
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
  }

                                /* RESUME HERE */
  fd.gdata = pdata;
  nvals = pdata->nvals;
  pdata->stack = (VERTEX *)Malloc(3*pdata->nnodes,VERTEX);
  incremental = (state_stamp == pdata->stamp);
  dvar_refresh(DVAR(0));
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    dvar_refresh(DVAR(var-nvals));
  }
  nvalue_refresh(wam, pdata,incremental);
  pdata->stamp = state_stamp+1;

  {
    int lboi, lbmd, ub;
    int all_ground_vals=0;	/* #values that occur ground among non-targets */
    int nkvars;	 /* #vars that cannot take a currently ground value */
    int nkvals;	 /* #vals that do not occur ground among targets */

    ub = max_matching(pdata);	/* must precede the rest, for it shuffles vartargets */
    for (i=pdata->nvaltargets; i<nvals; i++) {
      VERTEX val = pdata->valtarget[i];
      if (OUT_DEGREE(val)>0)
	all_ground_vals++;
    }
    for (i=0; i<pdata->nvaltargets; i++) {
      VERTEX val = pdata->valtarget[i];
      VISITED(val) = 0;
    }
    for (i=0; i<pdata->nvartargets; i++) {
      VERTEX var = pdata->vartarget[i];
      VISITED(var) = 0;
      if (IN_DEGREE(var)==1) {
	VERTEX val = NEIGH(var,1);
	VISITED(val) = 1;
      }
    }
    for (i=0; i<pdata->nvaltargets; i++) {
      VERTEX val = pdata->valtarget[i];
      if (VISITED(val)) {
	int degree = (int)OUT_DEGREE(val);
	for (j=0; j<degree; j++) {
	  VERTEX var = NEIGH(val,j);
	  VISITED(var) = 1;
	}
      }
    }

    {
      int inf = 0;
      int sup = pdata->nvartargets-1;
      VERTEX held = pdata->vartarget[sup]; /* sup is the hole */
      VERTEX current = pdata->vartarget[inf];
      while (inf<=sup) {
	if (!VISITED(current)) {
	  pdata->vartarget[inf] = current;
	  inf++;
	  current = (inf>=sup ? held : pdata->vartarget[inf]);
	} else {
	  pdata->vartarget[sup] = current;
	  sup--;
	  current = (inf>=sup ? held : pdata->vartarget[sup]);
	}
      }
      nkvars = inf;
    }
    {
      int inf = 0;
      int sup = pdata->nvaltargets-1;
      VERTEX held = pdata->valtarget[sup]; /* sup is the hole */
      VERTEX current = pdata->valtarget[inf];
      while (inf<=sup) {
	if (!VISITED(current)) {
	  pdata->valtarget[inf] = current;
	  inf++;
	  current = (inf>=sup ? held : pdata->valtarget[inf]);
	} else {
	  pdata->valtarget[sup] = current;
	  sup--;
	  current = (inf>=sup ? held : pdata->valtarget[sup]);
	}
      }
      nkvals = inf;
    }

    change = TRUE;
    while (change) {
      change = FALSE;
      lboi = ordered_intervals(wam, pdata,nkvars) + pdata->nvaltargets - nkvals;
      lbmd = large_independent_set(pdata,nkvars) + pdata->nvaltargets - nkvals;
      if (dvar_fix_interval_l(DVAR(0),lboi+all_ground_vals,ub+all_ground_vals)<0 ||
	  dvar_fix_interval_l(DVAR(0),lbmd+all_ground_vals,ub+all_ground_vals)<0)
	goto ret;
      if (lboi<ub && lbmd<ub && dvar_is_integer(DVAR(0))) {
	int value = dvar_max_int(DVAR(0));
	if (value==ub+all_ground_vals) {
	  matching_prune(wam, pdata,ub);
	  change |= compress_edges(wam, pdata);
	} 
	if (!change && value==lbmd+all_ground_vals) {
	  independent_set_prune(pdata,nkvars,nkvals);
	  change |= compress_edges(wam, pdata);
	}
	if (!change && value==lboi+all_ground_vals) {
	  ordered_intervals_prune(pdata,nkvars,nkvals);
	  change |= compress_edges(wam, pdata);
	}
      }
    };
  }

				/* Compute prunings. */
  ground = 1;
  dvar_pruning_done( DVAR(0));
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    Dvar dv = DVAR(var-nvals);
    int live = (int)IN_DEGREE(var);

    if (live > 1)
      ground = 0;
    if (live != dvar_value_count(dv)) {
      if (dvar_fix_set(dv, nvalue_new_domain(wam, pdata, var))<0)
	goto ret;
    }
    dvar_pruning_done( dv);
  }
  ent = ground;
  dvar_export(DVAR(0));
  for (i=0; i<pdata->nvartargets; i++) {
    VERTEX var = pdata->vartarget[i];
    Dvar dv = DVAR(var-nvals);

    dvar_export(dv);
  }
  contract_vals(pdata);		/* must precede contract_vars */
  contract_vars(pdata);
  CTagToArg(X(0),3) = MakeSmall(pdata->nvartargets);
  CTagToArg(X(0),4) = MakeSmall(pdata->nvaltargets);
ret:
  SP_free(pdata->stack);
  if (ent==1) {
    Pfree;
  }
  dvar_export_done(wam, Actions, ent);
}
