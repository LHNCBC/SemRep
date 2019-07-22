/* Copyright(C) 1999, Swedish Institute of Computer Science */
/***
Graeme Gange, Peter J. Stuckey, Radoslaw Szymanek:
MDD propagators with explanation. Constraints 16(4): 407-429 (2011)

Extended with linear extra constraints on given DAG edges, when such edges are committed.
***/


#include "fd.h"
#include "dvars.h"

#define DVAR(C) (pdata->dvar+(C))
#define ATTRIBUTE_LOC(C) (pdata->refbase + 2*(C))

struct mddi_common {
  int refcount;
  int postcount;
  int nvars;			/* arg 1 of state */
  int nnodes;			/* arg 2 of state */
  int nedges;			/* |arg 3 of state| */
  int nvarvals;			/* |arg 4 of state| */
  int nlinles;			/* |arg 5 of state| */
  struct {
    SP_integer *var;		/* [nnodes+1, unused?] */
    SP_integer *in;		/* [nnodes+1] */
    SP_integer *out;		/* [nnodes+1] */
  } cnode;
  struct {
    SP_integer *source;		/* [nedges], source node */
    SP_integer *dest;		/* [nedges], destination node */
    SP_integer *varval;		/* [nedges], unique (var,val) combination, increasing */
    SP_integer *in;		/* [nedges] */
    SP_integer *out;		/* [nedges] */
    SP_integer *chunk;		/* [nvars] -- index of first edge for var */
  } cedge;
  struct {
    SP_integer *var;		/* [nvarvals] */
    TAGGED *min;		/* [nvarvals], arg 4 of state */
    TAGGED *max;		/* [nvarvals], arg 4 of state */
    SP_integer *edges;		/* [nvarvals] */
    SP_integer *chunk;		/* [nvars] -- index of first varval for var */
  } cvarval;
  struct {
    SP_integer *lhs;		/* [nlinles*nvars] coefficient[linle,i] */
    SP_integer *lhsc;		/* [nlinles*nvars] coefficient[linle,lhsi[linle,i]], nonzero */
    SP_integer *lhsi;		/* [nlinles*nvars] */
    SP_integer *lhsn;		/* [nlinles] #nonzero coefficients */
    SP_integer *rhs;		/* [nlinles] */
    SP_integer *edge;		/* [nlinles] */
    SP_integer *chunk;		/* [nedges] */
  } clinle;
};
  
struct mddi_data {
  void (SPCDECL *destructor)(void*);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  struct mddi_common *common;
  SP_globref refbase;
  SP_integer stamp;		/* increases up to backtracking */
  int trail_top;
  Dvar dvar;
  unsigned char **trail;	 /* [(nedges+1+nvars+2*nlinles)*1] */
  SP_integer *killed_from_above; /* [nedges] */
  SP_integer *killed_from_below; /* [nedges] */
  SP_integer *killed;		 /* [nvarvals] */
  int nkfa, nkfb, nk;
  SP_BOOL active;		/* FALSE means for sure no propagating linle */
  struct {
    SP_integer *watch_in;	/* [nnodes+1], watched edge */
    SP_integer *watch_out;	/* [nnodes+1], watched edge */
  } node;
  struct {
    unsigned char *status;	/* [nedges] */
  } edge;
  struct {
    SP_integer *support;	/* [nvarvals], watched edge */
  } varval;
  struct {
    int watch;			/* entailed[watch] is watched */
    unsigned char *entailed;	/* [nlinles] */
    unsigned char *active;	/* [nlinles] */
  } linle;
  struct {
    SP_integer *watch1;		/* [nvars], watched edge */
    SP_integer *watch2;		/* [nvars], watched edge */
  } isthmus;
  struct {
    unsigned char *status;	 /* [nvars] */
    SP_integer watch1;		/* watched var */
    SP_integer watch2;		/* watched var */
  } nonground;
};

#define EDGE_ALIVE 0x3
#define EDGE_KILLED_ABOVE 0x2
#define EDGE_KILLED_BELOW 0x1
#define EDGE_KILLED_VALUE 0x0
#define EDGE_WATCHED_ABOVE 0x4	/* aka. begin, support for node above */
#define EDGE_WATCHED_BELOW 0x8	/* aka. end, support for node below */
#define EDGE_WATCHED_VALUE 0x10	/* aka. val, support for varval */
#define EDGE_WATCHED_ISTHMUS 0x20 /* support for level */

#define TRAIL_DEC(VAR,DECR)			\
  SP_ASSERT((VAR) & EDGE_ALIVE);		\
  (VAR) = (unsigned char)((VAR) - (DECR));	\
  pdata->trail[pdata->trail_top++] = &(VAR);	\

#define TRAIL_DEC_EDGE(VAR,DECR,EDGE)		\
  TRAIL_DEC(VAR,DECR)				\

static void SPCDECL mddi_destructor(void *pdata_v)
{
  struct mddi_data *pdata = (struct mddi_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  if (pdata->refbase)
    SP_free_globrefs(pdata->refbase,2*pdata->common->nvars);
  if (--pdata->common->refcount==0)
    SP_free(pdata->common);
  SP_free(pdata);
}

/* watched edge was killed---do we have a committed isthmus? */
static void maintain_isthmus(Wam wam,
			     struct mddi_data *pdata,
			     SP_integer edge)
{
  struct mddi_common *common = pdata->common;
  SP_integer var = common->cvarval.var[common->cedge.varval[edge]];
  SP_integer first = common->cedge.chunk[var];
  SP_integer next = (var+1 < common->nvars ? common->cedge.chunk[var+1] : common->nedges);
  SP_integer i;
  (void)wam;

  for (i=first; i<next; i++) {
    if ((pdata->edge.status[i] & (EDGE_ALIVE|EDGE_WATCHED_ISTHMUS)) == EDGE_ALIVE) {
      // pdata->edge.status[edge] -= (unsigned char)EDGE_WATCHED_ISTHMUS;
      pdata->edge.status[edge] = (unsigned char)
	(pdata->edge.status[edge] - EDGE_WATCHED_ISTHMUS);
      pdata->edge.status[i] = (unsigned char)
	(pdata->edge.status[i] + EDGE_WATCHED_ISTHMUS);
      if (pdata->isthmus.watch1[var]==edge)
	pdata->isthmus.watch1[var] = i;
      if (pdata->isthmus.watch2[var]==edge)
	pdata->isthmus.watch2[var] = i;
      return;
    }
  }
  /* no hit - commit remaining edge, if not done already */
  edge = pdata->isthmus.watch1[var] + pdata->isthmus.watch2[var] - edge;
  first = common->clinle.chunk[edge];
  next = (edge+1 < common->nedges ? common->clinle.chunk[edge+1] : common->nlinles);
  for (i=first; i<next; i++) {
    if (pdata->linle.active[i] & EDGE_ALIVE) {
      TRAIL_DEC(pdata->linle.active[i],EDGE_ALIVE);
      pdata->linle.active[i] |= EDGE_WATCHED_ISTHMUS; /* [MC] 4.3.0 */
      pdata->active = TRUE;
    }
  }
}

/* the given variable of the given tuple is ground */
static void maintain_entailment(Wam wam,
				struct mddi_data *pdata,
				SP_integer var)
{
  struct mddi_common *common = pdata->common;
  int i;
  (void)wam;
  
  if (var >= 0) {
    if (pdata->nonground.status[var] & EDGE_ALIVE) {
      TRAIL_DEC(pdata->nonground.status[var],EDGE_ALIVE);
    }
    if (!(pdata->nonground.status[var] & EDGE_WATCHED_VALUE))
      return;
    for (i=0; i<common->nvars; i++) {
      if ((pdata->nonground.status[i] & (EDGE_ALIVE|EDGE_WATCHED_VALUE)) == EDGE_ALIVE) {
	pdata->nonground.status[var] = (unsigned char)
	  (pdata->nonground.status[var] - EDGE_WATCHED_VALUE);
	pdata->nonground.status[i] = (unsigned char)
	  (pdata->nonground.status[i] + EDGE_WATCHED_VALUE);
	if (pdata->nonground.watch1==var)
	  pdata->nonground.watch1 = i;
	if (pdata->nonground.watch2==var)
	  pdata->nonground.watch2 = i;
	return;
      }
    }
  }
}

static void entail_linle(Wam wam,
			 struct mddi_data *pdata,
			 SP_integer linle,
			 int ent)
{
  struct mddi_common *common = pdata->common;
  TRAIL_DEC(pdata->linle.entailed[linle],EDGE_ALIVE-ent);
  if (pdata->linle.entailed[linle] & EDGE_WATCHED_VALUE) {
    int j;
    for (j=0; j<common->nlinles; j++) {
      if ((pdata->linle.entailed[j] & 0x3) == EDGE_ALIVE) {
	pdata->linle.entailed[linle] = (unsigned char)
	  (pdata->linle.entailed[linle] - EDGE_WATCHED_VALUE);
	pdata->linle.entailed[j] = (unsigned char)
	  (pdata->linle.entailed[j] + EDGE_WATCHED_VALUE);
	pdata->linle.watch = j;
	break;
      }
    }
    if (j==common->nlinles) /* it was the last linle to get (dis)entailed */
      maintain_entailment(wam, pdata,-1);
  }
}

static void disentail_linles(Wam wam,
			     struct mddi_data *pdata,
			     SP_integer edge)
{
  struct mddi_common *common = pdata->common;
  SP_integer first = common->clinle.chunk[edge];
  SP_integer next = (edge+1 < common->nedges ? common->clinle.chunk[edge+1] : common->nlinles);
  SP_integer i;
  for (i=first; i<next; i++) {
    if ((pdata->linle.entailed[i] & 0x3)==EDGE_ALIVE)
      entail_linle(wam, pdata,i,0);
  }    
}

static void downward_pass(Wam wam,
			  struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int nkfa = pdata->nkfa;
  int nk = pdata->nk;
  
 restart:
  while (nkfa > 0) {	/* search for a new support */
    SP_integer node = pdata->killed_from_above[--nkfa];
    SP_integer edge = common->cnode.in[node];
    SP_integer watch_in = pdata->node.watch_in[node];
    while (edge > -1) {
      if ((pdata->edge.status[edge] & 0x3) == EDGE_ALIVE) {
	/* support found; update watches */
	pdata->edge.status[watch_in] = (unsigned char)
	  (pdata->edge.status[watch_in] & ~EDGE_WATCHED_BELOW);
	pdata->edge.status[edge] |= EDGE_WATCHED_BELOW;
	pdata->node.watch_in[node] = edge;
	goto restart;
      }
      edge = common->cedge.in[edge];
    }
    /* the node is still dead; kill outgoing edges */
    edge = common->cnode.out[node];
    while (edge > -1) {
      unsigned char status = pdata->edge.status[edge];
      if ((status & 0x3) == EDGE_ALIVE) {
	TRAIL_DEC_EDGE(pdata->edge.status[edge],EDGE_ALIVE - EDGE_KILLED_ABOVE,edge);
	if (status & EDGE_WATCHED_BELOW)
	  pdata->killed_from_above[nkfa++] = common->cedge.dest[edge];
	if (status & EDGE_WATCHED_VALUE)
	  pdata->killed[nk++] = common->cedge.varval[edge];
	if (common->nlinles>0) {
	  if (status & EDGE_WATCHED_ISTHMUS)
	    maintain_isthmus(wam, pdata,edge);
	  disentail_linles(wam, pdata,edge);
	}
      }
      edge = common->cedge.out[edge];
    }
  }
  pdata->nkfa = 0;
  pdata->nk = nk;
}

static void upward_pass(Wam wam,
			struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int nkfb = pdata->nkfb;
  int nk = pdata->nk;
  
 restart:
  while (nkfb > 0) {	/* search for a new support */
    SP_integer node = pdata->killed_from_below[--nkfb];
    SP_integer edge = common->cnode.out[node];
    SP_integer watch_out = pdata->node.watch_out[node];
    while (edge > -1) {
      if ((pdata->edge.status[edge] & 0x3) == EDGE_ALIVE) {
	/* support found; update watches */
	pdata->edge.status[watch_out] = (unsigned char)
	  (pdata->edge.status[watch_out] & ~EDGE_WATCHED_ABOVE);
	pdata->edge.status[edge] |= EDGE_WATCHED_ABOVE;
	pdata->node.watch_out[node] = edge;
	goto restart;
      }
      edge = common->cedge.out[edge];
    }
    /* the node is still dead; kill ingoing edges */
    edge = common->cnode.in[node];
    while (edge > -1) {
      unsigned char status = pdata->edge.status[edge];
      if ((status & 0x3) == EDGE_ALIVE) {
	TRAIL_DEC_EDGE(pdata->edge.status[edge],EDGE_ALIVE - EDGE_KILLED_BELOW,edge);
	if (status & EDGE_WATCHED_ABOVE)
	  pdata->killed_from_below[nkfb++] = common->cedge.source[edge];
	if (status & EDGE_WATCHED_VALUE)
	  pdata->killed[nk++] = common->cedge.varval[edge];
	if (common->nlinles>0) {
	  if (status & EDGE_WATCHED_ISTHMUS)
	    maintain_isthmus(wam, pdata,edge);
	  disentail_linles(wam, pdata,edge);
	}
      }
      edge = common->cedge.in[edge];
    }
  }
  pdata->nkfb = 0;
  pdata->nk = nk;
}

static void collect(struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int nk0=0, nk=0;

  while (nk0 < pdata->nk) {
    SP_integer varval = pdata->killed[nk0++];
    SP_integer edge = pdata->varval.support[varval];
    SP_integer e = common->cvarval.edges[varval];
    while (e<common->nedges && common->cedge.varval[e]==varval) {
      if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) {
	pdata->edge.status[edge] = (unsigned char)
	  (pdata->edge.status[edge] & ~EDGE_WATCHED_VALUE);
	pdata->edge.status[e] |= EDGE_WATCHED_VALUE;
	pdata->varval.support[varval] = edge = e;
	break;
      }
      e++;
    }
    if ((pdata->edge.status[edge] & 0x3) != EDGE_ALIVE) {
      pdata->killed[nk++] = varval;
    }
  }
  pdata->nk = nk;  
}

#if SP_ASSERTIONS
static SP_BOOL mddi_assertion_above(struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int i;
  
  for (i=0; i<common->nnodes; i++) { /* exclude bottom node */
    int j = 0;
    SP_integer e = common->cnode.out[i];
    if (e > -1) {
      while (e > -1) {
	if (pdata->edge.status[e] & EDGE_WATCHED_ABOVE) j++;
	e = common->cedge.out[e];
      }
      if (j!=1)
	return FALSE;
    }
  }
  return TRUE;
}

static SP_BOOL mddi_assertion_below(struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int i;
  
  for (i=1; i<=common->nnodes; i++) { /* exclude top node */
    int j = 0;
    SP_integer e = common->cnode.in[i];
    if (e > -1) {
      while (e > -1) {
	if (pdata->edge.status[e] & EDGE_WATCHED_BELOW) j++;
	e = common->cedge.in[e];
      }
      if (j!=1)
	return FALSE;
    }
  }
  return TRUE;
}

static SP_BOOL mddi_assertion_varval(struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int i;
  
  for (i=0; i<common->nvarvals; i++) {
    int j = 0;
    SP_integer e = pdata->varval.support[i];
    while (e<common->nedges && common->cedge.varval[e]==i) {
      if (pdata->edge.status[e] & EDGE_WATCHED_VALUE) j++;
      e++;
    }
    if (j!=1)
      return FALSE;
  }
  return TRUE;
}
#endif /* SP_ASSERTIONS */

static void kill_edge(Wam wam,
		      struct mddi_data *pdata,
		      SP_integer edge,
		      SP_BOOL from_linle)
{
  struct mddi_common *common = pdata->common;
  unsigned char status = pdata->edge.status[edge];
  
  TRAIL_DEC_EDGE(pdata->edge.status[edge],EDGE_ALIVE - EDGE_KILLED_VALUE,edge);
  if (status & EDGE_WATCHED_ABOVE)
    pdata->killed_from_below[pdata->nkfb++] = common->cedge.source[edge];
  if (status & EDGE_WATCHED_BELOW)
    pdata->killed_from_above[pdata->nkfa++] = common->cedge.dest[edge];
  if (common->nlinles>0) {
    if (from_linle) {
      if (status & EDGE_WATCHED_VALUE)
	pdata->killed[pdata->nk++] = common->cedge.varval[edge];
    }
    if (status & EDGE_WATCHED_ISTHMUS)
      maintain_isthmus(wam, pdata,edge);
    disentail_linles(wam, pdata,edge);
  }
}


static void kill_all_support_integer(Wam wam,
				     struct mddi_data *pdata,
				     SP_integer var,
				     TAGGED value)
{
  struct mddi_common *common = pdata->common;
  SP_integer e = common->cedge.chunk[var];
  SP_integer last = (var+1 < common->nvars ? common->cedge.chunk[var+1] : common->nedges);
  SP_integer vvfalse = -1, vvtrue = -1;

  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) { /* killed due to external pruning */
      SP_integer varval = common->cedge.varval[e];
      if (varval==vvtrue) {
      } else if (varval==vvfalse) {
	kill_edge(wam, pdata,e,FALSE);
      } else if (Tgt(common->cvarval.min[varval],value) && IsSmall(common->cvarval.min[varval])) {
	break;			/* condition now monotone */
      } else if (Tlt(common->cvarval.max[varval],value) && IsSmall(common->cvarval.max[varval])) {
	vvfalse = varval;
	kill_edge(wam, pdata,e,FALSE);
      } else {
	vvtrue = varval;
      }
    }
  }
  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) {
      kill_edge(wam, pdata,e,FALSE);
    }
  }
}

static void kill_all_support_interval(Wam wam,
				      struct mddi_data *pdata,
				      SP_integer var,
				      TAGGED min,
				      TAGGED max)
{
  struct mddi_common *common = pdata->common; 
  SP_integer e = common->cedge.chunk[var];
  SP_integer last = (var+1 < common->nvars ? common->cedge.chunk[var+1] : common->nedges);
  SP_integer vvfalse = -1, vvtrue = -1;

  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) { /* killed due to external pruning */
      SP_integer varval = common->cedge.varval[e];
      if (varval==vvtrue) {
      } else if (varval==vvfalse) {
	kill_edge(wam, pdata,e,FALSE);
      } else if (FDgt(common->cvarval.min[varval],max)) {
	break;			/* condition now monotone */
      } else if (FDlt(common->cvarval.max[varval],min)) {
	vvfalse = varval;
	kill_edge(wam, pdata,e,FALSE);
      } else {
	vvtrue = varval;
      }
    }
  }
  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) {
      kill_edge(wam, pdata,e,FALSE);
    }
  }
}

static void kill_all_support_general(Wam wam,
				     struct mddi_data *pdata,
				     SP_integer var,
				     Dvar dv) 
{
  struct mddi_common *common = pdata->common; 
  SP_integer e = common->cedge.chunk[var];
  SP_integer last = (var+1 < common->nvars ? common->cedge.chunk[var+1] : common->nedges);
  SP_integer vvfalse = -1, vvtrue = -1;
  TAGGED min = dv->min;
  TAGGED max = dv->max;
  TAGGED set = dvar_set(dv);

  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) { /* killed due to external pruning */
      SP_integer varval = common->cedge.varval[e];
      TAGGED vmin = common->cvarval.min[varval];
      TAGGED vmax = common->cvarval.max[varval];
      if (varval==vvtrue) {
      } else if (varval==vvfalse) {
	kill_edge(wam, pdata,e,FALSE);
      } else if (FDgt(vmin,max)) {
	break;			/* remaining edges must all be dead */
      } else if (FDlt(vmax,min)) {
	vvfalse = varval;
	kill_edge(wam, pdata,e,FALSE);
      } else if (!fd_intersects_else(vmin,vmax,set,&set)) {
	if (set==EmptySet)
	  break;			/* remaining edges must all be dead */
	min = RangeMin(CTagToCar(set)); /* fd_min(set) */
	vvfalse = varval;
	kill_edge(wam, pdata,e,FALSE);
      } else {
	min = RangeMin(CTagToCar(set)); /* fd_min(set) */
	vvtrue = varval;
      }
    }
  }
  for (; e<last; e++) {
    if ((pdata->edge.status[e] & 0x3) == EDGE_ALIVE) {
      kill_edge(wam, pdata,e,FALSE);
    }
  }
}

static void kill_all_support(Wam wam,
			     struct mddi_data *pdata,
			     SP_integer var) 
{
  Dvar dv = DVAR(var);

  if (dvar_is_integer(dv)) {	/* ground case */
    kill_all_support_integer(wam, pdata,var,dv->min);
    maintain_entailment(wam, pdata,var);
  } else if (dvar_is_interval(dv)) { /* interval case */
    kill_all_support_interval(wam, pdata,var,dv->min,dv->max);
  } else {			/* general case */
    kill_all_support_general(wam, pdata,var,dv);
  }
}


/* Propagation of a linear le constraint
   Fixpoints are dealt with in the main loop
   TRUE for success, FALSE for failure
*/
static SP_BOOL linle_propagate(struct mddi_data *pdata,
			       SP_integer linle)
{
  struct mddi_common *common = pdata->common;
  SP_integer lbase = common->nvars * linle;
  SP_integer ix, ixn = common->clinle.lhsn[linle];
  SP_integer bigf = common->clinle.rhs[linle]; /* rhs - \sum \min a_i x_i */

  for (ix=0; ix<ixn; ix++) {
    SP_integer i = common->clinle.lhsi[lbase+ix];
    SP_integer c = common->clinle.lhsc[lbase+ix];
    Dvar dv = DVAR(i);
    if (c > 0) {
      bigf -= c*dvar_min_l(dv);
    } else {
      bigf -= c*dvar_max_l(dv);
    }
  }
  if (bigf<0)
    return FALSE;
  for (ix=0; ix<ixn; ix++) {
    SP_integer i = common->clinle.lhsi[lbase+ix];
    SP_integer c = common->clinle.lhsc[lbase+ix];
    Dvar dv = DVAR(i);
    SP_integer min, max;
    if (c > 0) {
      min = c*dvar_min_l(dv);
      max = c*dvar_max_l(dv);
      if (max-min > bigf) {
	SP_integer ub = bigf/c + dvar_min_l(dv);
	if (dvar_fix_max_l(dv,ub)<0)
	  return FALSE;
      }
    } else {
      min = c*dvar_max_l(dv);
      max = c*dvar_min_l(dv);
      if (max-min > bigf) {
	SP_integer lb = -(bigf/(-c)) + dvar_max_l(dv);
	if (dvar_fix_min_l(dv,lb)<0)
	  return FALSE;
      }
    }
  }
  return TRUE;
}

/* (dis)entailment test of a linear le constraint
   Note: under the assumption that the edge is committed
   -1 for disentailed
    1 for entailed
    2 for can prune
    0 for neither
*/
static int linle_entailment(struct mddi_data *pdata,
			    SP_integer linle)
{
  struct mddi_common *common = pdata->common;
  SP_integer edge = common->clinle.edge[linle];
  SP_integer edgevar =
    (edge == -1 ? -1 : /* no edge - don't narrow */
     !(pdata->linle.active[linle] & EDGE_ALIVE) ? -1 : /* isthmus - don't narrow */
     common->cvarval.var[common->cedge.varval[edge]]);
  SP_integer lbase = common->nvars * linle;
  SP_integer min = - common->clinle.rhs[linle];
  SP_integer max = min;
  SP_integer maxinterval = 0;
  SP_integer ix, ixn = common->clinle.lhsn[linle];

  for (ix=0; ix<ixn; ix++) {
    SP_integer i = common->clinle.lhsi[lbase+ix];
    SP_integer c = common->clinle.lhsc[lbase+ix];
    Dvar dv = DVAR(i);
    SP_integer cmin, cmax;
    if (i==edgevar) {		/* simulate domain in current edge */
      TAGGED tedgemin = common->cvarval.min[common->cedge.varval[edge]];
      TAGGED tedgemax = common->cvarval.max[common->cedge.varval[edge]];
      SP_integer edgemin = (tedgemin!=Inf && Tge(tedgemin,dvar_min_t(dv)) ?
			    GetSmall0(tedgemin) : dvar_min_l(dv));
      SP_integer edgemax = (tedgemax!=Sup && Tle(tedgemax,dvar_max_t(dv)) ?
			    GetSmall0(tedgemax) : dvar_max_l(dv));
      if (c > 0) {
	cmin = c*edgemin;
	cmax = c*edgemax;
      } else {
	cmin = c*edgemax;
	cmax = c*edgemin;
      }
    } else {
      if (c > 0) {
	cmin = c*dvar_min_l(dv);
	cmax = c*dvar_max_l(dv);
      } else {
	cmin = c*dvar_max_l(dv);
	cmax = c*dvar_min_l(dv);
      }
    }
    min += cmin;
    max += cmax;
    maxinterval = cmax-cmin > maxinterval ? cmax-cmin : maxinterval;
  }
  return (max <= 0 ? 1 : min > 0 ? -1 : maxinterval > -min ? 2 : 0);
}

static void check_all_linles(Wam wam,
			     struct mddi_data *pdata,
			     int filter)
{
  struct mddi_common *common = pdata->common;
  int i;
  
  for (i=0; i<common->nlinles; i++) {
    SP_integer edge = common->clinle.edge[i];
    if ((pdata->linle.entailed[i] & 0x3)==EDGE_ALIVE &&
	(filter<0 || common->clinle.lhs[common->nvars * i + filter]!=0)) {
      SP_ASSERT((edge == -1 || (pdata->edge.status[edge] & 0x3)==EDGE_ALIVE));
      switch (linle_entailment(pdata,i)) {
      case -1:			/* disentailed */
	if (edge >= 0) {
	  kill_edge(wam, pdata,edge,TRUE);
	} else {
	  pdata->linle.active[i] |= EDGE_WATCHED_ISTHMUS; /* will fail in propagator */
	  pdata->active = TRUE;
	}
	break;
      case 1:		/* entailed */
	entail_linle(wam, pdata,i,1);
	break;
      case 2:
	if (!(pdata->linle.active[i] & EDGE_ALIVE)) {
	  pdata->linle.active[i] |= EDGE_WATCHED_ISTHMUS;
	  pdata->active = TRUE;
	}
      }
    }
  }
}

static int
mddi_propagate(Wam wam,
	       struct mddi_data *pdata,
	       SP_BOOL norefresh)
{
  struct mddi_common *common = pdata->common;
  int i, j;
  int nvars = common->nvars;
  int nnodes = common->nnodes;
  SP_BOOL idempotent = (!(RefMutable(CTagToArg(X(1),3))&IStep(4))); /* STATUS: idempotent */
  
  {
    SP_integer edge;

    downward_pass(wam, pdata);
    edge = pdata->node.watch_in[nnodes];
    if ((pdata->edge.status[edge] & 0x3) != EDGE_ALIVE)
      return -1;
    upward_pass(wam, pdata);
    collect(pdata); /* pdata->killed contains a list of varvals */
    SP_ASSERT(pdata->nk <= common->nvarvals);
    SP_ASSERT(mddi_assertion_above(pdata));
    SP_ASSERT(mddi_assertion_below(pdata));
    SP_ASSERT(mddi_assertion_varval(pdata));
    /* sort and form sets for batch pruning? */
    if (pdata->nk + pdata->active > 0) {
      if (!norefresh) {
	for (i=0; i<nvars; i++) {	/* i: var index */
	  Dvar dv = DVAR(i);
	  dvar_refresh(dv);
	}
      }
      for (j=0; j<pdata->nk; j++) {
	SP_integer varval = pdata->killed[j];
	SP_integer var = common->cvarval.var[varval];
	Dvar dv = DVAR(var);
	TAGGED min = common->cvarval.min[varval];
	TAGGED max = common->cvarval.max[varval];
	if (dvar_prune_interval_t(dv,min,max)< 0) /* can happen if there are corefs? */
	  return -1;
	if (dvar_is_integer(dv) && idempotent)
	  maintain_entailment(wam, pdata,var);
      }
      pdata->nk = 0;
      if (pdata->active) {
	for (i=0; i<common->nlinles; i++) {
	  if ((pdata->linle.entailed[i] & 0x3)==EDGE_ALIVE &&
	      (pdata->linle.active[i] & (EDGE_ALIVE|EDGE_WATCHED_ISTHMUS))==EDGE_WATCHED_ISTHMUS) {
	    if (!linle_propagate(pdata,i))
	      return -1;
	    pdata->linle.active[i] = (unsigned char)
	      (pdata->linle.active[i] - EDGE_WATCHED_ISTHMUS);
	  }
	}
      }
      pdata->active = FALSE;
      for (i=0; i < nvars; i++) {
	Dvar dv = DVAR(i);
	dvar_pruning_done( dv);
      }
      for (i=0; i < nvars; i++) {
	Dvar dv = DVAR(i);
	dvar_export(dv);
      }
    }
  }
  CTagToArg(X(0),2) = MakeSmall(pdata->trail_top);
  if ((pdata->nonground.status[pdata->nonground.watch1] & EDGE_ALIVE) &&
      (pdata->nonground.status[pdata->nonground.watch2] & EDGE_ALIVE)) {
    return 0;
  /* here, at most one var is nonground */
  } else if (common->nlinles>0 &&
	     (pdata->linle.entailed[pdata->linle.watch] & EDGE_ALIVE) == EDGE_ALIVE) {
    return 0;
  /* here, all linles are (dis)entailed */
  } else {
    return 1;
  }
}

static DAEMON_RC SPCDECL 
mddi_daemon(Wam wam,
	    void *vdata,
	    SP_globref attr_ref,
	    TAGGED *global)
{
  struct mddi_data *pdata = (struct mddi_data *)vdata;
  struct mddi_common *common = pdata->common;
  TAGGED tstate;
  int ar, state_stamp;
  DAEMON_RC rc = DAEMON_FIX;

  tstate = RefMutable(CTagToArg(*global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall_int(CTagToArg(tstate,ar));
  if (pdata->stamp!=state_stamp) { /* non-incremental */
    int trail_top = GetSmall_int(CTagToArg(tstate,2));
    int f = pdata->trail_top;
    
    while (f>trail_top) {
      *(pdata->trail[--f]) |= EDGE_ALIVE;
    }
    pdata->trail_top = f;
    pdata->nkfa = 0;
    pdata->nkfb = 0;
    pdata->nk = 0;
  }
  {
    int col = (int)((attr_ref - pdata->refbase))>>1;
    if (pdata->nonground.status[col] & EDGE_ALIVE) { /* could have been processed already */
      Dvar dv = DVAR(col);
      SP_BOOL buried;
    
      tstate = fd_daemon_copy_state(wam, global,&buried);
      pdata->stamp = GetSmall_int(CTagToArg(tstate,ar));
      dvar_refresh(dv);
      kill_all_support(wam, pdata,col);
      if (common->nlinles>0) {
	int i;
	for (i=0; i<common->nvars; i++) {
	  Dvar dv = DVAR(i);
	  dvar_refresh(dv);
	}
	check_all_linles(wam, pdata,col);
      }
      CTagToArg(tstate,2) = MakeSmall(pdata->trail_top);
      if (pdata->nkfa + pdata->nkfb + pdata->nk + pdata->active > 0 ||
	  !(pdata->nonground.status[pdata->nonground.watch1] & EDGE_ALIVE) ||
	  !(pdata->nonground.status[pdata->nonground.watch2] & EDGE_ALIVE) ||
	  !(common->nlinles>0 &&
	    (pdata->linle.entailed[pdata->linle.watch] & EDGE_ALIVE) == EDGE_ALIVE)) {
	rc = DAEMON_NOFIX;
      }
    }
  }
  return rc;
}

static struct mddi_data *
mddi_alloc_state(Wam wam, struct mddi_common *common, TAGGED handle)
{
  char *ptr;
  struct mddi_data *pdata;
  int nvars = common->nvars;
  int nnodes = common->nnodes;
  int nedges = common->nedges;
  int nvarvals = common->nvarvals;
  int nlinles = common->nlinles;
  SP_integer total_size = nvars*sizeof(struct dvar) + /* dvar */
    nedges*sizeof(SP_integer) + /* killed_from_above */
    nedges*sizeof(SP_integer) + /* killed_from_below */
    nvarvals*sizeof(SP_integer) + /* killed */
    (nedges+1+nvars+2*nlinles)*sizeof(unsigned char *) + /* trail */
    nvarvals*sizeof(SP_integer) +  /* varval */
    2*(nnodes+1)*sizeof(SP_integer) + /* node */
    2*nvars*sizeof(SP_integer) + /* isthmus */
    nvars +			    /* nonground */
    nedges +			    /* edge */
    2*nlinles;		    /* linle */
  pdata = Palloc(struct mddi_data, total_size, handle);
  ptr = (char *)(pdata+1);
  pdata->destructor = mddi_destructor;
  pdata->daemon = mddi_daemon;
  FD_STORE_SPENV(pdata->spenv);
  pdata->common = common;
  pdata->stamp = 0;
  pdata->refbase = SP_alloc_globrefs(2*nvars);
  pdata->dvar = (Dvar)ptr; ptr += nvars*sizeof(struct dvar);
  pdata->trail = (unsigned char **)ptr; ptr += (nedges+1+nvars+2*nlinles)*sizeof(unsigned char *);
  pdata->node.watch_in = (SP_integer *)ptr; ptr += (nnodes+1)*sizeof(SP_integer);
  pdata->node.watch_out = (SP_integer *)ptr; ptr += (nnodes+1)*sizeof(SP_integer);
  pdata->isthmus.watch1 = (SP_integer *)ptr; ptr += (nvars)*sizeof(SP_integer);
  pdata->isthmus.watch2 = (SP_integer *)ptr; ptr += (nvars)*sizeof(SP_integer);
  pdata->varval.support = (SP_integer *)ptr; ptr += nvarvals*sizeof(SP_integer);
  pdata->killed_from_above = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  pdata->killed_from_below = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  pdata->killed = (SP_integer *)ptr; ptr += nvarvals*sizeof(SP_integer);
  pdata->edge.status = (unsigned char *)ptr; ptr += nedges;
  pdata->nonground.status = (unsigned char *)ptr; ptr += nvars;
  pdata->linle.entailed = (unsigned char *)ptr; ptr += nlinles;
  pdata->linle.active = (unsigned char *)ptr; ptr += nlinles;
  SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
  pdata->nkfa = 0;
  pdata->nkfb = 0;
  pdata->nk = 0;
  pdata->active = TRUE;
  return pdata;
}

static void
mddi_init_state(Wam wam, struct mddi_data *pdata)
{
  struct mddi_common *common = pdata->common;
  int nvars = common->nvars;
  int nnodes = common->nnodes;
  int nedges = common->nedges;
  int nvarvals = common->nvarvals;
  int nlinles = common->nlinles;
  int j;
  {
				/* tuple is entailed when < 2 nonground */
    for (j=0; j<nvars; j++) {
      pdata->nonground.status[j] = EDGE_ALIVE;
    }
    pdata->nonground.status[0] |= EDGE_WATCHED_VALUE;
    pdata->nonground.watch1 = 0;
    pdata->nonground.watch2 = 0;
    if (nvars>1) {
      pdata->nonground.status[1] |= EDGE_WATCHED_VALUE;
      pdata->nonground.watch2 = 1;
    }
    for (j=0; j<nedges; j++) {
      pdata->edge.status[j] = EDGE_ALIVE;
    }
    for (j=0; j<nlinles; j++) {
      SP_integer edge = common->clinle.edge[j];
      pdata->linle.entailed[j] = EDGE_ALIVE;
      pdata->linle.active[j] = (edge == -1 ? EDGE_WATCHED_ISTHMUS : EDGE_ALIVE);
      if (edge >= 0) {
	SP_integer var = common->cvarval.var[common->cedge.varval[edge]];
	edge = common->cedge.chunk[var];
	pdata->edge.status[edge] |= EDGE_WATCHED_ISTHMUS;
	pdata->isthmus.watch1[var] = edge;
	pdata->isthmus.watch2[var] = edge;
	if (edge+1<nedges && common->cvarval.var[common->cedge.varval[edge+1]]==var) {
	  pdata->edge.status[edge+1] |= EDGE_WATCHED_ISTHMUS;
	  pdata->isthmus.watch2[var] = edge+1;
	} else {		/* committed edge */
	  pdata->linle.active[j] = EDGE_WATCHED_ISTHMUS;
	}
      }
    }
    if (nlinles>0) {
      pdata->linle.entailed[0] |= EDGE_WATCHED_VALUE;
      pdata->linle.watch = 0;
    }
    for (j=0; j<(nnodes+1); j++) {
      SP_integer edge = common->cnode.in[j];
      if (edge > -1) {	/* not for top node */
	pdata->node.watch_in[j] = edge;
	pdata->edge.status[edge] |= EDGE_WATCHED_BELOW;
      }
      edge = common->cnode.out[j];
      if (edge > -1) {	/* not for bot node */
	pdata->node.watch_out[j] = edge;
	pdata->edge.status[edge] |= EDGE_WATCHED_ABOVE;
      }
    }
    for (j=0; j<nvarvals; j++) {
      SP_integer edge = common->cvarval.edges[j];
      pdata->varval.support[j] = edge;
      pdata->edge.status[edge] |= EDGE_WATCHED_VALUE;
    }
    for (j=0; j<nvars; j++) {
      SP_globref ref = ATTRIBUTE_LOC(j);
      Dvar dv = DVAR(j);
	
      dvar_init(dv, ref, ref+1);
      dvar_attach_daemon(wam, dv, pdata, X(1), functor_dom1);
    }
  }
  pdata->trail_top = 0;
}

/*
'$fd_mddi_common'(NVars, Nodes, Edges, VarVals, Linles, Common), % Common = state([_ | '$free'(Ptr)], 0)
*/
void SPCDECL
prolog_fd_mddi_common(Wam wam,
		      SP_integer nvars,
		      SP_term_ref nodes_ref,
		      SP_term_ref edges_ref,
		      SP_term_ref varvals_ref,
		      SP_term_ref linles_ref,
		      SP_integer  postcount,
		      SP_term_ref common_ref)
{
  TAGGED nodes, edges, varvals, linles, *h;
  int nnodes, nedges, nvarvals, nlinles, i, j;
  struct mddi_common *common;
  struct mddi_data *pdata;
  SP_integer total_size;
  char *ptr;
    
  DEREF(nodes, RefTerm(nodes_ref));
  DEREF(edges, RefTerm(edges_ref));
  DEREF(varvals, RefTerm(varvals_ref));
  DEREF(linles, RefTerm(linles_ref));
  nnodes = fd_list_length(nodes);
  nedges = fd_list_length(edges);
  nvarvals = fd_list_length(varvals);
  nlinles = fd_list_length(linles);

  total_size = 
    3*(nnodes+1)*sizeof(SP_integer) +	     /* cnode */
    5*nedges*sizeof(SP_integer) +	     /* cedge */
    nvars*sizeof(SP_integer) +		     /* cedge */
    4*nvarvals*sizeof(SP_integer) +	     /* cvarval */
    nvars*sizeof(SP_integer) +		     /* cvarval */
    nlinles*(3*nvars+3)*sizeof(SP_integer) + /* clinle */
    nedges*sizeof(SP_integer);		     /* clinle */
  common = fd_malloc(wam, sizeof(struct mddi_common) + total_size);
  ptr = (char *)(common+1);
  common->refcount = 1;
  common->postcount = (int)postcount;
  common->nvars = (int)nvars;
  common->nnodes = nnodes;
  common->nedges = nedges;
  common->nvarvals = nvarvals;
  common->nlinles = nlinles;
  common->cnode.var = (SP_integer *)ptr; ptr += (nnodes+1)*sizeof(SP_integer);
  common->cnode.in  = (SP_integer *)ptr; ptr += (nnodes+1)*sizeof(SP_integer);
  common->cnode.out = (SP_integer *)ptr; ptr += (nnodes+1)*sizeof(SP_integer);
  common->cedge.source = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  common->cedge.dest = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  common->cedge.varval = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  common->cedge.in = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  common->cedge.out = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  common->cedge.chunk = (SP_integer *)ptr; ptr += nvars*sizeof(SP_integer);
  common->cvarval.var = (SP_integer *)ptr; ptr += nvarvals*sizeof(SP_integer);
  common->cvarval.min = (TAGGED *)ptr; ptr += nvarvals*sizeof(TAGGED);
  common->cvarval.max = (TAGGED *)ptr; ptr += nvarvals*sizeof(TAGGED);
  common->cvarval.edges = (SP_integer *)ptr; ptr += nvarvals*sizeof(SP_integer);
  common->cvarval.chunk = (SP_integer *)ptr; ptr += nvars*sizeof(SP_integer);
  common->clinle.lhs = (SP_integer *)ptr; ptr += nlinles*nvars*sizeof(SP_integer);
  common->clinle.lhsc = (SP_integer *)ptr; ptr += nlinles*nvars*sizeof(SP_integer);
  common->clinle.lhsi = (SP_integer *)ptr; ptr += nlinles*nvars*sizeof(SP_integer);
  common->clinle.lhsn = (SP_integer *)ptr; ptr += nlinles*sizeof(SP_integer);
  common->clinle.rhs = (SP_integer *)ptr; ptr += nlinles*sizeof(SP_integer);
  common->clinle.edge = (SP_integer *)ptr; ptr += nlinles*sizeof(SP_integer);
  common->clinle.chunk = (SP_integer *)ptr; ptr += nedges*sizeof(SP_integer);
  for (i=0; i<nlinles; i++) {
    int k = (int)nvars*i;
    int ix = 0;
    TAGGED tvars, item;
    
    DerefCar(tvars,linles);
    DerefCdr(linles,linles);
    DerefArg(item,tvars,3);
    common->clinle.rhs[i] = GetSmall(item);
    DerefArg(item,tvars,1);
    common->clinle.edge[i] = GetSmall(item);
    DerefArg(tvars,tvars,2);
    for (j=0; j<nvars; j++) {
      SP_integer c;
      DerefCar(item,tvars);
      DerefCdr(tvars,tvars);
      c = GetSmall(item);
      common->clinle.lhs[k+j] = c;
      if (c) {
	common->clinle.lhsc[k+ix] = c;
	common->clinle.lhsi[k+ix++] = j;
      }
    }
    common->clinle.lhsn[i] = ix;
  }
  for (i=0; i<nnodes; i++) {
    TAGGED item;
    
    DerefCar(item,nodes);
    DerefCdr(nodes,nodes);
    common->cnode.var[i] = GetSmall(item);
    common->cnode.in[i] = -1;
    common->cnode.out[i] = -1;
  }
  common->cnode.in[nnodes] = -1;
  common->cnode.out[nnodes] = -1;
  
  for (i=0; i<nedges; i++) {
    TAGGED item, tmp;
    
    DerefCar(item,edges);
    DerefCdr(edges,edges);
    DerefArg(tmp,item,1);	/* source */
    common->cedge.source[i] = GetSmall(tmp);
    DerefArg(tmp,item,2);	/* dest */
    common->cedge.dest[i] = GetSmall(tmp);
    DerefArg(tmp,item,3);	/* varval */
    common->cedge.varval[i] = GetSmall(tmp);
    common->cedge.in[i] = -1;
    common->cedge.out[i] = -1;
  }
    
  for (i=0; i<nvarvals; i++) {
    TAGGED item, tmp;
    
    DerefCar(item,varvals);
    DerefCdr(varvals,varvals);
    DerefArg(tmp,item,1);	/* var */
    common->cvarval.var[i] = GetSmall(tmp);
    DerefArg(tmp,item,2);	/* min */
    common->cvarval.min[i] = tmp;
    DerefArg(tmp,item,3);	/* max */
    common->cvarval.max[i] = tmp;
    common->cvarval.edges[i] = -1;
  }
				/* build linked lists */
  for (i=nvarvals-1; i>=0; --i) {
    common->cvarval.chunk[common->cvarval.var[i]] = i;
  }
  for (i=nedges-1; i>=0; --i) {
    SP_integer node = common->cedge.dest[i];
    SP_integer next = common->cnode.in[node];
    SP_integer varval = common->cedge.varval[i];
    common->cedge.in[i] = next;
    common->cnode.in[node] = i;
    node = common->cedge.source[i];
    next = common->cnode.out[node];
    common->cedge.out[i] = next;
    common->cnode.out[node] = i;
    common->cvarval.edges[varval] = i;
    common->cedge.chunk[common->cvarval.var[varval]] = i;
    common->clinle.chunk[i] = nlinles;
  }
  for (i=nlinles-1; i>=0; --i) {
    SP_integer edge = common->clinle.edge[i];
    if (edge >= 0)
      for (j=0; j<=edge; j++)
	common->clinle.chunk[j] = i;
  }
  h = w->global_top;
  RefTerm(common_ref) = MakeStructure(h);
  h[0] = functor_minus;
  h[1] = TagREF(h+1);
  h[2] = TaggedZero;
  w->global_top = h+3;
  pdata = Palloc(struct mddi_data, 0, h[1]);
  pdata->destructor = mddi_destructor;
  FD_STORE_SPENV(pdata->spenv);
  pdata->refbase = NULL;
  pdata->common = common;
}

/*
state(Tuple, Common, TrailTop , _Handle,0)
*/
void SPCDECL
prolog_fd_mddi(Wam wam,
	       SP_term_ref State0,
	       SP_term_ref State,
	       SP_term_ref Actions)
{
  struct mddi_data *pdata;
  struct mddi_common *common;
  int i, j, nnodes, nvars;
  int ent = -1;
  TAGGED cur, handle;
  SP_BOOL committed, posted;
  
/*    X(0) = RefTerm(State0); */
  (void)State0;
  dvar_export_start(wam);
  RefTerm(State) = fd_static_output_state(wam,&handle,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    posted = FALSE;
    pdata = Pdata(struct mddi_data,handle);
    common = pdata->common;
    nvars = common->nvars;
    nnodes = common->nnodes;
  } else {			/* build persistent state */
    TAGGED item, tvars;
    
    posted = TRUE;
    DerefArg(cur,X(0),2);	/* get state(...) */
    DerefArg(cur,cur,1);	/* get [_ | '$free'(_)] */
    common = Pdata(struct mddi_data,cur)->common;
    common->refcount++;
    if (--common->postcount==0)
      fd_common_done(wam,2);
    nvars = common->nvars;
    nnodes = common->nnodes;
    pdata = mddi_alloc_state(wam, common,handle);
    DerefArg(tvars,X(0),1);	/* refresh */
    {
      for (j=0; j<nvars; j++) {
	SP_globref ref = ATTRIBUTE_LOC(j);

	DerefCar(item,tvars);
	DerefCdr(tvars,tvars);
	fd_get_var_and_attr(item,ref);
      }
    }    
    mddi_init_state(wam, pdata);	/* can GC */
  }
  
  /* RESUME */
  if (posted) {
    {
      for (i=0; i<nvars; i++) {
	Dvar dv = DVAR(i);
	
	dvar_refresh(dv);
	kill_all_support(wam, pdata,i);
      }
      if (nvars<2)
	maintain_entailment(wam, pdata,-1);
      check_all_linles(wam, pdata,-1);
				/* clean up orphants */
      for (j=1; j<nnodes; j++) {
	if (common->cnode.in[j] == -1) {
	  pdata->node.watch_in[j] = -1;
	  pdata->killed_from_above[pdata->nkfa++] = j;
	}
	if (common->cnode.out[j] == -1) {
	  pdata->node.watch_out[j] = -1;
	  pdata->killed_from_below[pdata->nkfb++] = j;
	}
      }
    }
  }
  ent = mddi_propagate(wam, pdata, FALSE);
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}

