/* Copyright(C) 2002, Swedish Institute of Computer Science */

/*  Constraint analog of keysort/2.
    Clone of bound-consistent keysorting/3 constraint.  See:
    K. Mehlhorn and Sven Thiel.
    Faster algorithms for bound-consistency of the sortedness 
    and the alldifferent constraint.
    In Sixth Int. Conf. on Principles and Practice of Constraint Programming 
    (CP2000), Lecture Notes in Computer Science 1894, Springer, 2000. 
*/

#include "fd.h"
#include "dvars.h"

struct keysorting_data {
  void (SPCDECL *destructor)(void *);
  SPEnv *spenv;
  SP_globref refbase;
  SP_integer stamp;			/* increases up to backtracking */
  int nkeys;			/* size of key part */
  int nvalues;			/* size of value part */
  int nkov;			/* size of tuple + 1 */
  int ntuples;			/* #tuples (total) */
  int ntargets;			/* #tuples (active) */
  int yoffset;			/* #leading shaved Y tuples */
  int nscc;
  int prunings;
  int *xtarget;			/* [ntuples], the active xvars */
  int *xmate;			/* [ntuples], x's matched val, volatile */
  int *ymate;			/* [ntuples], y's matched var, volatile */
  int *sortmin;			/* [ntuples], vars by ascending min, volatile */
  int *sortmax;			/* [ntuples], vars by ascending max, volatile */
  int *sorty;			/* [ntuples], ys ascending, volatile */
  int *chunk;			/* [ntuples], chunk in which var was inserted, volatile */
  int *class;			/* [ntuples], class map in off-line-minimum, volatile */
  int *scc;			/* [ntuples], scc ID of val, volatile */
  TAGGED *lb;			/* [2*nkov*ntuples], lower bounds, volatile */
  TAGGED *ub;			/* [2*nkov*ntuples], upper bounds, volatile */
  TAGGED *tuple;	        /* [3*nkov],   scratch pad, volatile */
  struct {
    int *node;			/* [ntuples], volatile */
    int *root;			/* [ntuples], volatile */
    int *rightmost;		/* [ntuples], volatile */
    int *maxx;			/* [ntuples], volatile */
  } stk;
  Dvar dvar;
};

#define CURTUPLE (pdata->tuple)
#define XV(I)   (pdata->xtarget[I])
#define XMATE(I) (pdata->xmate[I])
#define XSCC(I) scc[XMATE(I)]
#define XLB(I,J) (pdata->lb+nkov*(I)+(J))
#define XUB(I,J) (pdata->ub+nkov*(I)+(J))
#define XVAR(I,J) (pdata->dvar+nkov*(I)+(J))
#define XRefAttr(I,J) (pdata->refbase + 2*nkov*(I) + 2*(J))
#define XRefVar(I,J)  (pdata->refbase + 2*nkov*(I) + 2*(J) + 1)
#define YV(I)   ((I)+pdata->yoffset)
#define YMATE(I) (pdata->ymate[I])
#define YSCC(I) scc[I]
#define YLB(I,J) (pdata->lb+nkov*(I)+nkov*ntuples+(J))
#define YUB(I,J) (pdata->ub+nkov*(I)+nkov*ntuples+(J))
#define YVAR(I,J) (pdata->dvar+nkov*(I)+nkov*ntuples+(J))
#define YRefAttr(I,J) (pdata->refbase + 2*nkov*(I) + 2*(J) + 2*nkov*ntuples)
#define YRefVar(I,J)  (pdata->refbase + 2*nkov*(I) + 2*(J) + 2*nkov*ntuples + 1)

#define CHECK(RC)				\
if ((RC)<0)					\
  return FALSE;					\
else if ((RC)>0)				\
  pdata->prunings++;

static void SPCDECL keysorting_destructor(void *pdata_v)
{
  struct keysorting_data *pdata = (struct keysorting_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

    SP_free_globrefs(pdata->refbase,4*pdata->ntuples*pdata->nkov);
  SP_free(pdata);
}

#define lex_set(P,Q,N)					\
  { int m_i;						\
    for (m_i=0; m_i<(N); m_i++) (P)[m_i] = (Q)[m_i];	\
  }							\

static SP_BOOL
lex_lt(TAGGED *t1, TAGGED *t2, int n)
{
  int i;

  for (i=0; i<n; i++) {
    if (t1[i] != t2[i])
      return (FDlt(t1[i], t2[i]));
  }
  return FALSE;
}

static SP_BOOL
lex_fix_min(TAGGED *l, TAGGED *u, TAGGED *t, int n)
{
  int i, cmp=0x3;		/* 0x2 while t lex_lt l is possible */
  				/* 0x1 while u lex_lt t is possible */

  for (i=0; i<n; i++) {
    if (u[i] != t[i]) {
      if ((cmp & 0x1) && FDlt(u[i], t[i]))
	return FALSE;
      else
	cmp &= 0x2;
    }
    if (l[i] != t[i]) {
      if ((cmp & 0x2) && FDgt(l[i], t[i]))
	return TRUE;
      else
	cmp &= 0x1;
      l[i] = t[i];
    }
  }
  return TRUE;
}

static SP_BOOL
lex_fix_max(TAGGED *l, TAGGED *u, TAGGED *t, int n)
{
  int i, cmp=0x3;		/* 0x1 while t lex_lt l is possible */
  				/* 0x2 while u lex_lt t is possible */

  for (i=0; i<n; i++) {
    if (l[i] != t[i]) {
      if ((cmp & 0x1) && FDgt(l[i], t[i]))
	return FALSE;
      else
	cmp &= 0x2;
    }
    if (u[i] != t[i]) {
      if ((cmp & 0x2) && FDlt(u[i], t[i]))
	return TRUE;
      else
	cmp &= 0x1;
      u[i] = t[i];
    }
  }
  return TRUE;
}

static int
dvar_lex_fix_min(Dvar dv, TAGGED *t, int n)
{
  int i=0, rc=0, q=0;

  for (; i<n; i++) {
    q = i;
    if (FDgt(dvar_min_t(dv+i),t[i])) {
      return rc;
    } else if (FDlt(dvar_max_t(dv+i),t[i])) {
      return -1;
    } else if (dvar_max_t(dv+i) != t[i]) {
      rc |= dvar_fix_min_t(dv+q,t[q]);
      break;
    } else {
      rc |= dvar_fix_value_t(dv+i,t[i]);
    }
  }
  for (i++; i<n; i++) {
    if (FDlt(dvar_max_t(dv+i),t[i])) {
      rc |= dvar_fix_min_t(dv+q,FDincr(t[q]));
      break;
    } else if (dvar_max_t(dv+i) != t[i]) {
      break;
    }
  }
  return rc;
}

static int
dvar_lex_fix_max(Dvar dv, TAGGED *t, int n)
{
  int i=0, rc=0, q=0;

  for (; i<n; i++) {
    q = i;
    if (FDlt(dvar_max_t(dv+i),t[i])) {
      return rc;
    } else if (FDgt(dvar_min_t(dv+i),t[i])) {
      return -1;
    } else if (dvar_min_t(dv+i) != t[i]) {
      rc |= dvar_fix_max_t(dv+q,t[q]);
      break;
    } else {
      rc |= dvar_fix_value_t(dv+i,t[i]);
    }
  }
  for (i++; i<n; i++) {
    if (FDgt(dvar_min_t(dv+i),t[i])) {
      rc |= dvar_fix_max_t(dv+q,FDdecr(t[q]));
      break;
    } else if (dvar_min_t(dv+i) != t[i]) {
      break;
    }
  }
  return rc;
}

static int
lex_cmp(TAGGED *t1, TAGGED *t2, int n)
{
  int i;

  for (i=0; i<n; i++) {
    if (t1[i] != t2[i])
      return (FDlt(t1[i], t2[i]) ? -1 : 1);
  }
  return 0;
}

static SP_BOOL
dvar_lex_is_fixed(Dvar dv, int n)
{
  int i;
  
  for (i=0; i<n; i++) {
    if (!dvar_is_integer(dv+i))
      return FALSE;
  }
  return TRUE;
}

static SP_BOOL
dvar_lex_lt(Dvar dv1, Dvar dv2, int n)
{
  int i;
  
  for (i=0; i<n; i++) {
    if (dvar_max_t(dv1+i) != dvar_min_t(dv2+i)) {
      return (FDlt(dvar_max_t(dv1+i), dvar_min_t(dv2+i)));
    }
  }
  return FALSE;
}

/* sort by ascending XMIN */
static int 
cmp_asc_min(Wam wam, int *x1, int *x2)
{
  struct keysorting_data *pdata = fd.gdata;
  int nkov = pdata->nkov;
  return lex_cmp(XLB(*x1,0), XLB(*x2,0), pdata->nkeys+1);
}

#define QType int
#define QCmp  cmp_asc_min
#define QSort qsort_asc_min
#include "qsort.ic"

/* sort by ascending XMAX */
static int 
cmp_asc_max(Wam wam, int *x1, int *x2)
{
  struct keysorting_data *pdata = fd.gdata;
  int nkov = pdata->nkov;
  return lex_cmp(XUB(*x1,0), XUB(*x2,0), pdata->nkeys+1);
}

#define QType int
#define QCmp  cmp_asc_max
#define QSort qsort_asc_max
#include "qsort.ic"

static SP_BOOL
normalize(Wam wam,
	  struct keysorting_data *pdata)
{
  int i, j;
  int ntuples = pdata->ntuples;
  int nkov = pdata->nkov;
  int nkeys = pdata->nkeys;
  int ntargets = pdata->ntargets;
  
  /* normalize min(Y) */
  lex_set(CURTUPLE, YLB(YV(0),0), nkeys+1);
  for (i=1; i<ntargets; i++) {
    int y = YV(i);
    CURTUPLE[nkeys] += IStep(1); /* YLB strictly increasing */
    if (!lex_fix_min(YLB(y,0), YUB(y,0), CURTUPLE, nkeys+1))
      return FALSE;		/* reachable */
    lex_set(CURTUPLE, YLB(y,0), nkeys+1);
  }
  /* normalize max(Y) */
  lex_set(CURTUPLE, YUB(YV(ntargets-1),0), nkeys+1);
  for (i=ntargets-2; i>=0; i--) {
    int y = YV(i);
    CURTUPLE[nkeys] -= IStep(1); /* YUB strictly increasing */
    if (!lex_fix_max(YLB(y,0), YUB(y,0), CURTUPLE, nkeys+1))
      return FALSE;
    lex_set(CURTUPLE, YUB(y,0), nkeys+1);
  }
  /* sort X by ascending XMIN */
  qsort_asc_min(wam, pdata->sortmin, ntargets);
  /* scan Xs */
  for (i=j=0; i<ntargets; i++) {
    int x = pdata->sortmin[i];
    int y = YV(j);

    while (j<ntargets && lex_lt(YUB(y,0), XLB(x,0), nkeys+1))
      y = YV(++j);
    if (j==ntargets)
      return FALSE;		/* reachable */
    if (lex_lt(XLB(x,0), YLB(y,0), nkeys+1)) {
      if (!lex_fix_min(XLB(x,0), XUB(x,0), YLB(y,0), nkeys+1))
	return FALSE;		/* reachable */
    }
  }
  /* sort X by ascending XMAX */
  qsort_asc_max(wam, pdata->sortmax, ntargets);
  /* scan Xs */
  for (i=j=ntargets-1; i>=0; i--) {
    int x = pdata->sortmax[i];
    int y = YV(j);

    while (j>=0 && lex_lt(XUB(x,0), YLB(y,0), nkeys+1))
      y = YV(--j);
    if (j<0)
      return FALSE;
    if (lex_lt(YUB(y,0), XUB(x,0), nkeys+1)) {
      if (!lex_fix_max(XLB(x,0), XUB(x,0), YUB(y,0), nkeys+1))
	return FALSE;
    }
  }
  return TRUE;
}

static SP_BOOL
match_up(struct keysorting_data *pdata)
{
  int i, j, e, x, y;
  int ntuples = pdata->ntuples;
  int ntargets = pdata->ntargets;
  int nkov = pdata->nkov;
  int nkeys = pdata->nkeys;
  int *chunk = pdata->chunk;
  int *class = pdata->class;

  /* simulate a priority queue and an iteration from 0 to ntuples s.t.
     in iteration j:
     1. add all i to P with YMAX(j-1)<XMIN(i)<=YMAX(j)
     2. if P empty, fail
     3. otherwise, extract i with smallest XMAX(i),
        YMATE(j)=i,
	if XMAX(i)<YMIN(j), then there is no matching

     No actual priority queue is used.
     We solve the off-line-minimum problem instead.
     TODO: add a disjoint-set data structure instead.
  */

  /* compute chunks nodes */
  e = 0;			/* extract count */
  i = 0;			/* insert count */
  x = pdata->sortmin[i];
  for (j=0; j<ntargets; j++) {
    y = YV(j);
    while (i<ntargets && !lex_lt(YUB(y,0), XLB(x,0), nkeys+1)) {
      chunk[x] = y;
      i++;
      if (i<ntargets)
	x = pdata->sortmin[i];
    }
    if (i==e)
      return FALSE;
    else {
      class[y] = y;		/* matched */
      e++;
    }
  }
  if (i!=e)
    return FALSE;

  /* poor man's off-line-minimum algorithm */
  for (e=0; e<ntargets; e++) {
    x = pdata->sortmax[e];
    y = chunk[x];
    while (y<class[y])		/* dereference class */
      y = class[y];		/* TODO: path compression */
    if (lex_lt(XUB(x,0), YLB(y,0), nkeys+1))
      return FALSE;
    YMATE(y) = x;
    XMATE(x) = y;
    class[y] = y+1;		/* merge with next class, TODO: decomposition */
  }
  for (i=0; i<ntargets; i++) {
    y = YV(i);
    x = YMATE(y);
    if (!lex_fix_max(YLB(y,0), YUB(y,0), XUB(x,0), nkeys+1))
      return FALSE;
  }
  return TRUE;
}

static SP_BOOL
match_down(struct keysorting_data *pdata)
{
  int i, j, e, x, y;
  int ntuples = pdata->ntuples;
  int ntargets = pdata->ntargets;
  int nkov = pdata->nkov;
  int nkeys = pdata->nkeys;
  int *chunk = pdata->chunk;
  int *class = pdata->class;

  /* simulate a priority queue and an iteration from ntuples to 0 s.t.
     in iteration j:
     1. add all i to P with YMIN(j+1)>XMAX(i)>=YMIN(j)
     2. if P empty, fail
     3. otherwise, extract i with greatest XMIN(i),
        YMATE(j)=i,
	if XMIN(i)>YMAX(j), then there is no matching

     No actual priority queue is used.
     We solve the off-line-minimum problem instead.
     TODO: add a disjoint-set data structure instead.
  */

  /* compute chunks nodes */
  e = 0;			/* extract count */
  i = 0;			/* insert count */
  x = pdata->sortmax[ntargets-1];
  for (j=ntargets-1; j>=0; j--) {
    y = YV(j);
    while (i<ntargets && !lex_lt(XUB(x,0), YLB(y,0), nkeys+1)) {
      chunk[x] = y;
      i++;
      if (i<ntargets)
	x = pdata->sortmax[ntargets-i-1];
    }
    if (i==e)
      return FALSE;
    else {
      class[y] = y;		/* matched */
      e++;
    }
  }
  if (i!=e)
    return FALSE;

  /* poor man's off-line-minimum algorithm */
  for (e=ntargets-1; e>=0; e--) {
    x = pdata->sortmin[e];
    y = chunk[x];
    while (y>class[y])		/* dereference class */
      y = class[y];		/* TODO: path compression */
    if (lex_lt(YUB(y,0), XLB(x,0), nkeys+1))
      return FALSE;
    YMATE(y) = x;
    XMATE(x) = y;
    class[y] = y-1;		/* merge with next class, TODO: decomposition */
  }
  for (i=0; i<ntargets; i++) {
    y = YV(i);
    x = YMATE(y);
    if (!lex_fix_min(YLB(y,0), YUB(y,0), XLB(x,0), nkeys+1))
      return FALSE;
  }
  return TRUE;
}

static SP_BOOL
findscc(struct keysorting_data *pdata)
{
  int j, sccid, top1, top2;
  int ntuples = pdata->ntuples;
  int nkov = pdata->nkov;
  int nkeys = pdata->nkeys;
  int ntargets = pdata->ntargets;

  /* init */
  j=0; sccid=0; top1=0; top2=0;
  while (j-top1<ntargets) {
    int y = j<ntargets ? YV(j) : 0; /* only used if j<ntargets */
    if (top2==0) {	/* start 1st component */
      int x = YMATE(y);
      pdata->stk.node[0] = y;
      pdata->stk.root[0] = y;
      pdata->stk.rightmost[0] = y;
      pdata->stk.maxx[0] = x;
      top1 = top2 = 1;
      j++;
    } else if (j<ntargets &&
	       !lex_lt(XUB(pdata->stk.maxx[top2-1],0), YLB(y,0), nkeys+1)) { /* start/extend component */
      int x = YMATE(y);
      int max1 = x;
      
      pdata->stk.node[top1++] = y;
      pdata->stk.root[top2] = y;
      while (top2>0 &&
	     !lex_lt(YUB(pdata->stk.rightmost[top2-1],0), XLB(x,0), nkeys+1)) { /* merge components */
	top2--;
	if (lex_lt(XUB(max1,0), XUB(pdata->stk.maxx[top2],0), nkeys+1))
	  max1 = pdata->stk.maxx[top2];
      }
      pdata->stk.rightmost[top2] = y;
      pdata->stk.maxx[top2++] = max1;
      j++;
    } else {			/* component done */
      int root1 = pdata->stk.root[--top2];
      int y;

      do {
	y = pdata->stk.node[--top1];
	pdata->scc[y] = sccid;
      } while (y>root1);
      sccid++;
    }
  }
  pdata->nscc = sccid;
  return TRUE;
}

static SP_BOOL
narrow(struct keysorting_data *pdata)
{
  int i, j, n;
  int ntuples = pdata->ntuples;
  int nkov = pdata->nkov;
  int nkeys = pdata->nkeys;
  int ntargets = pdata->ntargets;
  int *chunk = pdata->chunk;
  int *class = pdata->class;
  int *scc = pdata->scc;

  if (pdata->nscc==1)		/* no pruning possible */
    return TRUE;

  /* Compute three arrays */
  /* sorty   : matched y's by (a) SCC (b) increasing y */
  /* sortmin : x's by (a) SCC (b) increasing XMIN */
  /* sortmax : x's by (a) SCC (b) increasing XMAX */
  for (i=ntargets-1; i>=0; i--)
    class[i] = YV(i);
  KEYSORT(class, ntargets, int,
	  chunk, pdata->nscc, YSCC,
	  pdata->sorty);
  for (i=ntargets-1; i>=0; i--)
    class[i] = pdata->sortmin[i];
  KEYSORT(class, ntargets, int,
	  chunk, pdata->nscc, XSCC,
	  pdata->sortmin);
  for (i=ntargets-1; i>=0; i--)
    class[i] = pdata->sortmax[i];
  KEYSORT(class, ntargets, int,
	  chunk, pdata->nscc, XSCC,
	  pdata->sortmax);

  /* Adjust all lower bounds. */
  j = 0;
  for (i=0; i<ntargets; i=n) {
    int y = pdata->sorty[i];
    int sccid = scc[y];

    n = i+1;
    while (n<ntargets && scc[pdata->sorty[n]]==sccid)
      n++;
    while (j<n) {
      int x = pdata->sortmin[j];
      if (!lex_lt(YUB(y,0), XLB(x,0), nkeys+1)) {
	if (!lex_fix_min(XLB(x,0), XUB(x,0), YLB(y,0), nkeys+1))
	  return FALSE;
	j++;
      } else
	if (++i<ntargets) {
	  y = pdata->sorty[i];
	}
    }
  }

  /* Adjust all upper bounds. */
  j = ntargets-1;
  for (i=ntargets-1; i>=0; i=n) {
    int y = pdata->sorty[i];
    int sccid = scc[y];

    n = i-1;
    while (n>=0 && scc[pdata->sorty[n]]==sccid)
      n--;
    while (j>n) {
      int x = pdata->sortmax[j];
      if (!lex_lt(XUB(x,0), YLB(y,0), nkeys+1)) {
	if (!lex_fix_max(XLB(x,0), XUB(x,0), YUB(y,0), nkeys+1))
	  return FALSE;
	j--;
      } else
	if (--i>=0) {
	  y = pdata->sorty[i];
	}
    }
  }
	
  return TRUE;
}

static void
init_bounds(struct keysorting_data *pdata)
{
  int nkov = pdata->nkov;
  int ntargets = pdata->ntargets;
  int ntuples = pdata->ntuples;
  int i, j;
  
  for (i=0; i<ntargets; i++) {
    int xelt = XV(i);
    int yelt = YV(i);
    for (j=0; j<nkov; j++) {
      *XLB(xelt,j) = dvar_min_t(XVAR(xelt,j));
      *XUB(xelt,j) = dvar_max_t(XVAR(xelt,j));
      *YLB(yelt,j) = dvar_min_t(YVAR(yelt,j));
      *YUB(yelt,j) = dvar_max_t(YVAR(yelt,j));
    }
  }
}

static SP_BOOL
prune_bounds(struct keysorting_data *pdata)
{
  int nkov = pdata->nkov;
  int nkeys = pdata->nkeys;
  int ntargets = pdata->ntargets;
  int ntuples = pdata->ntuples;
  int i, j, k, rc;
  
  for (i=0; i<ntargets; i++) {
    int xelt = XV(i);
    int yelt = YV(i);
    rc = dvar_lex_fix_min(XVAR(xelt,0), XLB(xelt,0), nkeys+1);
    CHECK(rc);
    rc = dvar_lex_fix_max(XVAR(xelt,0), XUB(xelt,0), nkeys+1);
    CHECK(rc);
    rc = dvar_lex_fix_min(YVAR(yelt,0), YLB(yelt,0), nkeys+1);
    CHECK(rc);
    rc = dvar_lex_fix_max(YVAR(yelt,0), YUB(yelt,0), nkeys+1);
    CHECK(rc);
  }

  /* Make all tuple fields bounds-consistent. */
  /* The following example doesn't work if we skip the most significant field (from k=1 instead of 0):

     | ?- D in 2..4, K in{2}\/{4}, E in 2..4, M in{2}\/{4}, 
          keysorting([[2,2,2],[2,D,3],[E,2,2]],[[2,2,2],[2,K,3],[M,2,2]],[keys(2)]), 
          K = 4, E = 3.

  */
  if (pdata->nscc==1) {
    for (i=0; i<ntargets; i++)
      pdata->sorty[i] = YV(i);
  }
  for (k=0; k<nkov; k++) {
    for (i=j=0; i<ntargets; ) {
      int y0 = pdata->sorty[i];
      int sccid = pdata->scc[y0];
      TAGGED xlb = Sup;
      TAGGED xub = Inf;
      TAGGED ylb = Sup;
      TAGGED yub = Inf;

      while (i<ntargets && pdata->scc[pdata->sorty[i]]==sccid) {
	int y = pdata->sorty[i++];
	int x = YMATE(y);
	if (FDgt(ylb,dvar_min_t(YVAR(y,k)))) ylb = dvar_min_t(YVAR(y,k));
	if (FDgt(xlb,dvar_min_t(XVAR(x,k)))) xlb = dvar_min_t(XVAR(x,k));
	if (FDlt(yub,dvar_max_t(YVAR(y,k)))) yub = dvar_max_t(YVAR(y,k));
	if (FDlt(xub,dvar_max_t(XVAR(x,k)))) xub = dvar_max_t(XVAR(x,k));
      }
      while (j<i) {
	int y = pdata->sorty[j++];
	int x = YMATE(y);
	rc = dvar_fix_interval_t(XVAR(x,k), ylb, yub);
	CHECK(rc);
	rc = dvar_fix_interval_t(YVAR(y,k), xlb, xub);
	CHECK(rc);
      }
    }
  }
  /* now set pdata->scc[y] to the size of y's SCC */
  for (i=j=0; i<ntargets; ) {
    int y0 = pdata->sorty[i];
    int sccid = pdata->scc[y0];
    while (i<ntargets && pdata->scc[pdata->sorty[i]]==sccid)
      i++;
    sccid = i-j;
    while (j<i) {
      int y = pdata->sorty[j++];
      pdata->scc[y] = sccid;
    }
  }
  return TRUE;
}

/*
  '$fd_keysorting'(+State0, +State, -Actions).
  State = state(XVec,YVec,NTuples,NKey,NVal,NShaved,YOffset,Handle,Stamp)

  where XVec is [ X(i,1)...X(i,NKey),   i,X(i,NKey+1)...X(i,NKey+NVal) | i in 1..NTuples ]
  where YVec is [ Y(i,1)...Y(i,NKey),P(i),Y(i,NKey+1)...Y(i,NKey+NVal) | i in 1..NTuples ]
*/
void SPCDECL
prolog_fd_keysorting(Wam wam,
		     SP_term_ref State0,
		     SP_term_ref State,
		     SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec;
  SP_integer state_stamp;
  SP_integer total_size;
  int i, j, ntuples, nkeys, nvalues, nkov, ref=0;
  int ntargets;
  struct keysorting_data *pdata;
  SP_BOOL committed;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct keysorting_data,handle);
    ntuples = pdata->ntuples;
    nkeys = pdata->nkeys;
    nvalues = pdata->nvalues;
    nkov = pdata->nkov;
  } else {			/* build persistent state */
    DerefArg(telt,X(0),3);	/* get NTuples */
    ntuples = GetSmall_int(telt);
    if (ntuples==0) {
      ent = 1;
      goto ret1;
    }
    DerefArg(telt,X(0),4);	/* get NKeys */
    nkeys = GetSmall_int(telt);
    DerefArg(telt,X(0),5);	/* get NValues */
    nvalues = GetSmall_int(telt);
    nkov = nkeys + nvalues + 1;
    total_size = ntuples*(sizeof(int) + 2*nkov*sizeof(struct dvar));
    pdata = Palloc(struct keysorting_data, total_size, handle);
    pdata->destructor = (keysorting_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(4*ntuples*nkov);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->ntuples = ntuples;
    pdata->nkeys = nkeys;
    pdata->nvalues = nvalues;
    pdata->nkov = nkov;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr = (char *)(pdata->dvar+2*ntuples*nkov);
    pdata->xtarget = (int *)ptr;
    ptr = (char *)(pdata->xtarget+ntuples);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    for (i=ntuples-1; i>=0; i--)
      XV(i) = i;
    for (i=1; i<3; i++) {
      DerefArg(tvec,X(0),i);		/* get ?Vec */
      for (j=0; j<ntuples*nkov; j++) {
	DerefCar(telt,tvec);
	DerefCdr(tvec,tvec);
	fd_get_var_and_attr(telt,XRefAttr(0,ref));
	dvar_init(XVAR(0,ref), XRefAttr(0,ref), XRefVar(0,ref));
	ref++;
      }
    }
    CTagToArg(X(0),1) = atom_nil; /* free for GC */
    CTagToArg(X(0),2) = atom_nil; /* free for GC */
  }

  /* RESUME HERE */
  ptr = fd_malloc(wam, 12*ntuples*sizeof(int) + nkov*sizeof(TAGGED) + 4*ntuples*nkov*sizeof(TAGGED));
  pdata->lb = (TAGGED *)ptr;
  ptr = (char *)(pdata->lb+2*ntuples*nkov);
  pdata->ub = (TAGGED *)ptr;
  ptr = (char *)(pdata->ub+2*ntuples*nkov);
  pdata->tuple = (TAGGED *)ptr;
  ptr = (char *)(pdata->tuple+nkov);
  pdata->xmate = (int *)ptr;
  ptr = (char *)(pdata->xmate+ntuples);
  pdata->ymate = (int *)ptr;
  ptr = (char *)(pdata->ymate+ntuples);
  pdata->sortmin = (int *)ptr;
  ptr = (char *)(pdata->sortmin+ntuples);
  pdata->sortmax = (int *)ptr;
  ptr = (char *)(pdata->sortmax+ntuples);
  pdata->sorty = (int *)ptr;
  ptr = (char *)(pdata->sorty+ntuples);
  pdata->chunk = (int *)ptr;
  ptr = (char *)(pdata->chunk+ntuples);
  pdata->class = (int *)ptr;
  ptr = (char *)(pdata->class+ntuples);
  pdata->scc = (int *)ptr;
  ptr = (char *)(pdata->scc+ntuples);
  pdata->stk.node = (int *)ptr;
  ptr = (char *)(pdata->stk.node+ntuples);
  pdata->stk.root = (int *)ptr;
  ptr = (char *)(pdata->stk.root+ntuples);
  pdata->stk.rightmost = (int *)ptr;
  ptr = (char *)(pdata->stk.rightmost+ntuples);
  pdata->stk.maxx = (int *)ptr;
  ptr = (char *)(pdata->stk.maxx+ntuples);
  (void)ptr; /* suppress clang warning about value stored is never read. */
  fd.gdata = pdata;
  pdata->stamp = state_stamp+1;
  DerefArg(telt,X(0),6);
  ntargets = ntuples-GetSmall_int(telt);
  pdata->ntargets = ntargets;
  DerefArg(telt,X(0),7);
  pdata->yoffset = GetSmall_int(telt);
  for (i=0; i<ntargets; i++) {
    int xelt = XV(i);
    int yelt = YV(i);
    for (j=0; j<nkov; j++) {
      dvar_refresh(XVAR(xelt,j));
      dvar_refresh(YVAR(yelt,j));
    }
  }
  for (i=ntargets-1; i>=0; i--)
    pdata->sortmin[i] = pdata->sortmax[i] = XV(i);
  do {
    pdata->prunings = 0;
    init_bounds(pdata);
    if (!normalize(wam, pdata) ||
        !match_up(pdata) ||
	!match_down(pdata) ||
	!findscc(pdata) ||
	!narrow(pdata) ||
	!prune_bounds(pdata))
      goto ret;
  } while (pdata->prunings>0);
  for (i=0; i<ntargets; i++) {
    for (j=0; j<nkov; j++) {
      dvar_export(XVAR(XV(i),j));
      dvar_export(YVAR(YV(i),j));
    }
  }
  if (ntargets>0) {
    /* "shave" the Y targets and corresponding X targets */
    int k1 = ntargets;
    int kn = -1;
    int held, current;
    for (i=0; i<ntargets; i++) {
      int y = YV(i);
      
      if (pdata->scc[y]>1 ||
	  !dvar_lex_is_fixed(YVAR(y,0), nkov) ||
	  /* wrong !dvar_lex_is_fixed(XVAR(YMATE(y),0), nkov) || */
	  !(i==0 || dvar_lex_lt(YVAR(YV(i-1),0), YVAR(y,0), nkeys+1)) ||
	  !(i+1==ntargets || dvar_lex_lt(YVAR(y,0), YVAR(YV(i+1),0), nkeys+1))) {
	if (k1>i) k1 = i;
	if (kn<i) kn = i;
      }
    }
    /* k1/kn is the first/last to keep */
    for (i=0; i<k1; i++)
      XMATE(YMATE(YV(i))) = -1;
    for (i=kn+1; i<ntargets; i++)
      XMATE(YMATE(YV(i))) = -1;
    /* partition the active targets, moving shaved ones to the end */
    i = 0;
    j = ntargets-1;
    held = XV(j); /* j is the hole */
    current = XV(i);
    while (i<=j) {
      if (XMATE(current) > -1) {
	XV(i) = current;
	i++;
	current = (i>=j ? held : XV(i));
      } else {
	XV(j) = current;
	j--;
	current = (i>=j ? held : XV(j));
      }
    }
    ntargets = i;
    CTagToArg(X(0),6) = MakeSmall(ntuples-ntargets);
    CTagToArg(X(0),7) = MakeSmall(pdata->yoffset+k1);
  }
  ent = (ntargets==0);
 ret:
  SP_free(pdata->lb);
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(wam, Actions, ent);
}
