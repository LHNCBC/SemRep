/* Copyright(C) 2002, Swedish Institute of Computer Science */

/*  Bound-consistent sorting/3 constraint.  See:
    K. Mehlhorn and Sven Thiel.
    Faster algorithms for bound-consistency of the sortedness 
    and the alldifferent constraint.
    In Sixth Int. Conf. on Principles and Practice of Constraint Programming 
    (CP2000), Lecture Notes in Computer Science 1894, Springer, 2000. 
*/

#include "fd.h"
#include "dvars.h"

struct sorting_data {
  void (SPCDECL *destructor)(void *);
  SPEnv *spenv;
  SP_globref refbase;
  SP_integer stamp;			/* increases up to backtracking */
  int nvars;			/* #variables (total) */
  int ntargets;			/* #variables (active) */
  int yoffset;			/* #leading shaved Y variables */
  int nscc;
  int prunings;
  int *xtarget;			/* [nvars], the active xvars */
  int *xmate;			/* [nvars], x's matched val, volatile */
  int *ymate;			/* [nvars], y's matched var, volatile */
  int *sortmin;			/* [nvars], vars by ascending min, volatile */
  int *sortmax;			/* [nvars], vars by ascending max, volatile */
  int *sorty;			/* [nvars], ys ascending, volatile */
  int *chunk;			/* [nvars], chunk in which var was inserted, volatile */
  int *class;			/* [nvars], class map in off-line-minimum, volatile */
  int *scc;			/* [nvars], scc ID of val, volatile */
  struct {
    int *node;			/* [nvars], volatile */
    int *root;			/* [nvars], volatile */
    int *rightmost;		/* [nvars], volatile */
    TAGGED *maxx;		/* [nvars], volatile */
    SP_integer   *maxp;		/* alias maxx */
  } stk;
  Dvar dvar;
};

#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define XV(I)   (pdata->xtarget[I])
#define XVAR(I) (pdata->dvar+(I))
#define XMIN(I) (dvar_min_t(XVAR(I)))
#define XMAX(I) (dvar_max_t(XVAR(I)))
#define XMATE(I) (pdata->xmate[I])
#define XSCC(I) scc[XMATE(I)]
#define PVAR(I) XVAR((I)+nvars)
#define PMIN(I) (dvar_min_l(XVAR((I)+nvars)))
#define PMIN0(I) (PMIN(I)-yoffset-1)
#define PMAX(I) (dvar_max_l(XVAR((I)+nvars)))
#define PMAX0(I) (PMAX(I)-yoffset-1)
#define YV(I)   ((I)+pdata->yoffset)
#define YVAR(I) XVAR((I)+2*nvars)
#define YMIN(I) XMIN((I)+2*nvars)
#define YMAX(I) XMAX((I)+2*nvars)
#define YMATE(I) (pdata->ymate[I])
#define YSCC(I) scc[I]

#define CHECK(COND)							\
{ int rc = (COND); if (rc<0) return FALSE; else if (rc>0) pdata->prunings++; }

static void SPCDECL sorting_destructor(void *pdata_v)
{
  struct sorting_data *pdata = (struct sorting_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,6*pdata->nvars);
  SP_free(pdata);
}

/* sort by ascending XMIN */
static int 
cmp_asc_min(Wam wam,
		   int *x1, int *x2)
{
  struct sorting_data *pdata = fd.gdata;
  TAGGED xmin1 = XMIN(*x1);
  TAGGED xmin2 = XMIN(*x2);
  return FDCMP(xmin1,xmin2);
}

#define QType int
#define QCmp  cmp_asc_min
#define QSort qsort_asc_min
#include "qsort.ic"

/* sort by ascending XMAX */
static int 
cmp_asc_max(Wam wam,
		   int *x1, int *x2)
{
  struct sorting_data *pdata = fd.gdata;
  TAGGED xmax1 = XMAX(*x1);
  TAGGED xmax2 = XMAX(*x2);
  return FDCMP(xmax1,xmax2);
}

#define QType int
#define QCmp  cmp_asc_max
#define QSort qsort_asc_max
#include "qsort.ic"

static SP_BOOL
normalize(Wam wam,
		 struct sorting_data *pdata)
{
  int i, j;
  int nvars = pdata->nvars;
  int ntargets = pdata->ntargets;
  TAGGED cur;
  SP_BOOL donormal;
  
  donormal = TRUE;
  while (donormal) {
    /* normalize min(Y) */
    cur = YMIN(YV(0));
    for (i=1; i<ntargets; i++) {
      int y = YV(i);
      if (FDlt(YMIN(y),cur)) {
	CHECK(dvar_fix_min_t(YVAR(y),cur));
      }
      cur = YMIN(y);
    }
    /* normalize max(Y) */
    cur = YMAX(YV(ntargets-1));
    for (i=ntargets-1; i>=0; i--) {
      int y = YV(i);
      if (FDgt(YMAX(y),cur)) {
	CHECK(dvar_fix_max_t(YVAR(y),cur));
      }
      cur = YMAX(y);
    }
    donormal = FALSE;
    /* sort X by ascending XMIN */
    qsort_asc_min(wam, pdata->sortmin, ntargets);
    /* scan Xs and Ps in sync */
    for (i=j=0; i<ntargets; i++) {
      int x = pdata->sortmin[i];
      int y = YV(j);
      TAGGED xmin = XMIN(x);
      TAGGED xlb;
      
      while (j<ntargets && FDgt(xmin,YMAX(y)))
	y = YV(++j);
      if (j==ntargets)
	return FALSE;
      if (PMIN(x)<y+1) {
	CHECK(dvar_fix_min_l(PVAR(x),y+1));
	if (PMIN(x)>y+1)
	  donormal = TRUE;
      }
      xlb = YMIN(PMIN(x)-1);
      if (FDlt(xmin,xlb)) {
	CHECK(dvar_fix_min_t(XVAR(x),xlb));
	xmin = XMIN(x);
	if (FDgt(xmin,xlb))
	  donormal = TRUE;
      }
      if (i>0 && FDgt(cur,xmin))
	donormal = TRUE;
      cur = xmin;
    }
    /* sort X by ascending PMAX */
    qsort_asc_max(wam, pdata->sortmax, ntargets);
    /* scan Xs and Ps in sync */
    for (i=j=ntargets-1; i>=0; i--) {
      int x = pdata->sortmax[i];
      int y = YV(j);
      TAGGED xmax = XMAX(x);
      TAGGED xub;
      
      while (j>=0 && FDlt(xmax,YMIN(y)))
	y = YV(--j);
      if (j<0)
	return FALSE;
      if (PMAX(x)>y+1) {
	CHECK(dvar_fix_max_l(PVAR(x),y+1));
	if (PMAX(x)<y+1)
	  donormal = TRUE;
      }
      xub = YMAX(PMAX(x)-1);
      if (FDgt(xmax,xub)) {
	CHECK(dvar_fix_max_t(XVAR(x),xub));
	xmax = XMAX(x);
	if (FDgt(xmax,xub))
	  donormal = TRUE;
      }
      if (dvar_is_integer(PVAR(x))) {
	y = (int)PMIN(x)-1;
	if (FDlt(YMIN(y),XMIN(x)) || FDgt(YMAX(y),xmax)) {
	  CHECK(dvar_fix_interval_t(YVAR(y),XMIN(x),xmax));
	  donormal = TRUE;
	}
      }
      if (i<ntargets-1 && FDlt(cur,xmax))
	donormal = TRUE;
      cur = xmax;
    }
  }
  return TRUE;
}

static SP_BOOL
match_up(struct sorting_data *pdata)
{
  int i, j, e, x, y;
  int nvars = pdata->nvars;
  int ntargets = pdata->ntargets;
  int *chunk = pdata->chunk;
  int *class = pdata->class;

  /* simulate a priority queue and an iteration from 0 to nvars s.t.
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
    while (i<ntargets && !FDgt(XMIN(x),YMAX(y))) {
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
    if (FDlt(XMAX(x),YMIN(y)))
      return FALSE;
    YMATE(y) = x;
    XMATE(x) = y;
    class[y] = y+1;		/* merge with next class, TODO: decomposition */
  }
  for (i=0; i<ntargets; i++) {
    y = YV(i);
    x = YMATE(y);
    CHECK(dvar_fix_max_t(YVAR(y),XMAX(x)));
  }
  return TRUE;
}

static SP_BOOL
match_down(struct sorting_data *pdata)
{
  int i, j, e, x, y;
  int nvars = pdata->nvars;
  int ntargets = pdata->ntargets;
  int *chunk = pdata->chunk;
  int *class = pdata->class;

  /* simulate a priority queue and an iteration from nvars to 0 s.t.
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
    while (i<ntargets && !FDlt(XMAX(x),YMIN(y))) {
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
    if (FDgt(XMIN(x),YMAX(y)))
      return FALSE;
    YMATE(y) = x;
    XMATE(x) = y;
    class[y] = y-1;		/* merge with next class, TODO: decomposition */
  }
  for (i=0; i<ntargets; i++) {
    y = YV(i);
    x = YMATE(y);
    CHECK(dvar_fix_min_t(YVAR(y),XMIN(x)));
  }
  return TRUE;
}

static SP_BOOL
match_p(struct sorting_data *pdata)
{
  int i, j, e, x, y;
  int nvars = pdata->nvars;
  int ntargets = pdata->ntargets;
  int yoffset = pdata->yoffset;
  int *chunk = pdata->chunk;
  int *class = pdata->class;

  KEYSORT(pdata->xtarget, ntargets, int,
	  chunk, ntargets, PMIN0,
	  pdata->sortmin);
#if !CLPFD_INLINE_KEYSORT_TEST
  KEYSORT(pdata->xtarget, ntargets, int,
	  chunk, ntargets, PMAX0,
	  pdata->sortmax);
#else  /* CLPFD_INLINE_KEYSORT_TEST */
  /* #define KEYSORT(Inarray, Incount, Type, Keyarray, Keycount, Key, Outarray) */
{										
  int _i;									
  int /* Type */ _x;									
  for (_i=(ntargets /* Keycount */)-1; _i>=0; _i--)						
    (chunk /* Keyarray */)[_i] = 0;								
  for (_i=(ntargets /* Incount */)-1; _i>=0; _i--)						
    (chunk /* Keyarray */)[PMAX0 /* Key */((pdata->xtarget /* Inarray */)[_i])]++;						
  for (_i=1; _i<(ntargets /* Keycount */); _i++)						
    (chunk /* Keyarray */)[_i] += (chunk /* Keyarray */)[_i-1];						
  for (_i=(ntargets /* Incount */)-1; _i>=0; _i--)						
    _x = (pdata->xtarget /* Inarray */)[_i],								
      (pdata->sortmax /* Outarray */)[--(chunk /* Keyarray */)[PMAX0 /* Key */(_x)]] = _x;					
}
#endif /* CLPFD_INLINE_KEYSORT_TEST */
  /* simulate a priority queue and an iteration from 0 to nvars s.t.
     in iteration j:
     1. add all i to P with j-1<PMIN(i)<=j
     2. if P empty, fail
     3. otherwise, extract i with smallest XMAX(i),
        YMATE(j)=i,
	if XMAX(i)<j, then there is no matching

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
    while (i<ntargets && PMIN(x)<=y+1) {
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
    if (PMAX(x)<y+1)
      return FALSE;
    YMATE(y) = x;
    XMATE(x) = y;
    class[y] = y+1;		/* merge with next class, TODO: decomposition */
  }

  return TRUE;
}

static SP_BOOL
findscc_xy(struct sorting_data *pdata)
{
  int j, sccid, top1, top2;
  int nvars = pdata->nvars;
  int ntargets = pdata->ntargets;

  /* init */
  j=0; sccid=0; top1=0; top2=0;
  while (j-top1<ntargets) {
    int y = j<ntargets ? YV(j) : 0;
    if (top2==0) {	/* start 1st component */
      int x = YMATE(y);
      pdata->stk.node[0] = y;
      pdata->stk.root[0] = y;
      pdata->stk.rightmost[0] = y;
      pdata->stk.maxx[0] = XMAX(x);
      top1 = top2 = 1;
      j++;
    } else if (j<ntargets &&
	       !FDlt(pdata->stk.maxx[top2-1],YMIN(y))) { /* start/extend component */
      int x = YMATE(y);
      TAGGED max1 = XMAX(x);
      
      pdata->stk.node[top1++] = y;
      pdata->stk.root[top2] = y;
      while (top2>0 &&
	     !FDgt(XMIN(x),YMAX(pdata->stk.rightmost[top2-1]))) { /* merge components */
	top2--;
	if (FDlt(max1,pdata->stk.maxx[top2]))
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
findscc_p(struct sorting_data *pdata)
{
  int j, sccid, top1, top2;
  int nvars = pdata->nvars;
  int ntargets = pdata->ntargets;

  /* init */
  j=0; sccid=0; top1=0; top2=0;
  while (j-top1<ntargets) {
    int y = j<ntargets ? YV(j) : 0;
    if (top2==0) {	/* start 1st component */
      int x = YMATE(y);
      pdata->stk.node[0] = y;
      pdata->stk.root[0] = y;
      pdata->stk.rightmost[0] = y;
      pdata->stk.maxp[0] = PMAX(x);
      top1 = top2 = 1;
      j++;
    } else if (j<ntargets &&
	       pdata->stk.maxp[top2-1]>=y+1) { /* start/extend component */
      int x = YMATE(y);
      int max1 = (int)PMAX(x);
      
      pdata->stk.node[top1++] = y;
      pdata->stk.root[top2] = y;
      while (top2>0 &&
	     PMIN(x)<=pdata->stk.rightmost[top2-1]+1) { /* merge components */
	top2--;
	if (max1<pdata->stk.maxp[top2])
	  max1 = (int)pdata->stk.maxp[top2];
      }
      pdata->stk.rightmost[top2] = y;
      pdata->stk.maxp[top2++] = max1;
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
narrow_x(struct sorting_data *pdata)
{
  int i, j, n;
  int nvars = pdata->nvars;
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
    TAGGED ymax = YMAX(y);
    int sccid = scc[y];
    n = i+1;
    while (n<ntargets && scc[pdata->sorty[n]]==sccid)
      n++;
    while (j<n) {
      int x = pdata->sortmin[j];
      TAGGED xmin = XMIN(x);
      if (!FDgt(xmin,ymax)) {
	TAGGED ymin = YMIN(y);
	CHECK(dvar_fix_min_t(XVAR(x),ymin));
	j++;
      } else
	if (++i<ntargets) {
	  y = pdata->sorty[i];
	  ymax = YMAX(y);
	}
    }
  }

  /* Adjust all upper bounds. */
  j = ntargets-1;
  for (i=ntargets-1; i>=0; i=n) {
    int y = pdata->sorty[i];
    TAGGED ymin = YMIN(y);
    int sccid = scc[y];
    n = i-1;
    while (n>=0 && scc[pdata->sorty[n]]==sccid)
      n--;
    while (j>n) {
      int x = pdata->sortmax[j];
      TAGGED xmax = XMAX(x);
      if (!FDlt(xmax,ymin)) {
	TAGGED ymax = YMAX(y);
	CHECK(dvar_fix_max_t(XVAR(x),ymax));
	j--;
      } else
	if (--i>=0) {
	  y = pdata->sorty[i];
	  ymin = YMIN(y);
	}
    }
  }
	
  return TRUE;
}

static SP_BOOL
narrow_p(struct sorting_data *pdata)
{
  int i, j, n;
  int nvars = pdata->nvars;
  int ntargets = pdata->ntargets;
  int *chunk = pdata->chunk;
  int *class = pdata->class;
  int *scc = pdata->scc;

  if (pdata->nscc==1)		/* no pruning possible */
    return TRUE;

  /* Compute three arrays */
  /* sorty   : matched y's by (a) SCC (b) increasing y */
  /* sortmin : x's by (a) SCC (b) increasing PMIN */
  /* sortmax : x's by (a) SCC (b) increasing PMAX */
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
      int pmin = (int)PMIN(x);
      if (pmin<=y+1) {
	CHECK(dvar_fix_min_l(PVAR(x),y+1));
	j++;
      } else
	if (++i<ntargets)
	  y = pdata->sorty[i];
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
      SP_integer pmax = PMAX(x);
      if (pmax>=y+1) {
	CHECK(dvar_fix_max_l(PVAR(x),y+1));
	j--;
      } else
	if (--i>=0)
	  y = pdata->sorty[i];
    }
  }
	
  return TRUE;
}

/*
  '$fd_sorting'(+State0, +State, -Actions).
  State = state(XVec,PVec,YVec,NVars,NShaved,YOffset,Handle,Stamp)

  NOTE: The P variables are 1-based.
*/
void SPCDECL
prolog_fd_sorting(Wam wam,
			 SP_term_ref State0,
			 SP_term_ref State,
			 SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec;
  SP_integer state_stamp;
  SP_integer total_size;
  int i, j, nvars;
  int ntargets;
  struct sorting_data *pdata;
  SP_BOOL committed, ground;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct sorting_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(telt,X(0),4);	/* get NVars */
    nvars = GetSmall_int(telt);
    if (nvars==0) {
      ent = 1;
      goto ret1;
    }
    total_size = nvars*(sizeof(int) + 3*sizeof(struct dvar));
    pdata = Palloc(struct sorting_data, total_size, handle);
    pdata->destructor = (sorting_destructor);
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(6*nvars);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->nvars = nvars;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr = (char *)(pdata->dvar+3*nvars);
    pdata->xtarget = (int *)ptr;
    ptr = (char *)(pdata->xtarget+nvars);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    for (i=nvars-1; i>=0; i--)
      XV(i) = i;
    for (i=1; i<4; i++) {
      int base = (i-1)*nvars;
      DerefArg(tvec,X(0),i);		/* get ?Vec */
      for (j=0; j<nvars; j++) {
	DerefCar(telt,tvec);
	DerefCdr(tvec,tvec);
	fd_get_var_and_attr(telt,RefAttr(j+base));
	dvar_init(XVAR(j+base), RefAttr(j+base), RefVar(j+base));
      }
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),2) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),3) = atom_nil; /* [MC] 3.12: free for GC */
  }

  /* RESUME HERE */
  ptr = fd_malloc(wam, nvars*(sizeof(TAGGED) + 11*sizeof(int)));
  pdata->stk.maxx = (TAGGED *)ptr;
  pdata->stk.maxp = (SP_integer *)ptr;
  ptr = (char *)(pdata->stk.maxx+nvars);
  pdata->xmate = (int *)ptr;
  ptr = (char *)(pdata->xmate+nvars);
  pdata->ymate = (int *)ptr;
  ptr = (char *)(pdata->ymate+nvars);
  pdata->sortmin = (int *)ptr;
  ptr = (char *)(pdata->sortmin+nvars);
  pdata->sortmax = (int *)ptr;
  ptr = (char *)(pdata->sortmax+nvars);
  pdata->sorty = (int *)ptr;
  ptr = (char *)(pdata->sorty+nvars);
  pdata->chunk = (int *)ptr;
  ptr = (char *)(pdata->chunk+nvars);
  pdata->class = (int *)ptr;
  ptr = (char *)(pdata->class+nvars);
  pdata->scc = (int *)ptr;
  ptr = (char *)(pdata->scc+nvars);
  pdata->stk.node = (int *)ptr;
  ptr = (char *)(pdata->stk.node+nvars);
  pdata->stk.root = (int *)ptr;
  ptr = (char *)(pdata->stk.root+nvars);
  pdata->stk.rightmost = (int *)ptr;
  ptr = (char *)(pdata->stk.rightmost+nvars);
  (void)ptr; /* suppress clang warning about value stored is never read. */
  fd.gdata = pdata;
  pdata->stamp = state_stamp+1;
  DerefArg(telt,X(0),5);
  ntargets = nvars-GetSmall_int(telt);
  pdata->ntargets = ntargets;
  DerefArg(telt,X(0),6);
  pdata->yoffset = GetSmall_int(telt);
  for (i=0; i<ntargets; i++) {
    int elt = XV(i);
    dvar_refresh(XVAR(elt));
    dvar_refresh(PVAR(elt));
    elt = YV(i);
    dvar_refresh(YVAR(elt));
  }
  for (i=ntargets-1; i>=0; i--)
    pdata->sortmin[i] = pdata->sortmax[i] = XV(i);
  do {
    pdata->prunings = 0;
    if (!normalize(wam, pdata))
      goto ret;
    if (!match_up(pdata) ||
	!match_down(pdata) ||
	!findscc_xy(pdata) ||
	!narrow_x(pdata) ||
	!match_p(pdata) ||
	!findscc_p(pdata) ||
	!narrow_p(pdata))
      goto ret;
  } while (pdata->prunings>0);
  for (i=0; i<ntargets; i++) {
    dvar_export(XVAR(XV(i)));
    dvar_export(PVAR(XV(i)));
    dvar_export(YVAR(YV(i)));
  }
  ground = TRUE;
  for (i=0; ground && i<ntargets; i++) {
    if (!dvar_is_integer(XVAR(XV(i))) ||
	!dvar_is_integer(PVAR(XV(i))))
      ground = FALSE;
  }
  if (ntargets>0) {
    /* "shave" the Y targets and corresponding X targets */
    int k1 = ntargets;
    int kn = -1;
    int held, current;
    for (i=0; i<ntargets; i++) {
      int y = YV(i);
      TAGGED yval = YMIN(y);
      if (!dvar_is_integer(YVAR(y)) ||
	  !dvar_is_integer(PVAR(YMATE(y))) ||
	  !(i==0 || FDlt(YMAX(YV(i-1)),yval)) ||
	  !(i+1==ntargets || FDlt(yval,YMIN(YV(i+1))))) {
	if (k1>i)
	  k1 = i;
	if (kn<i)
	  kn = i;
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
    CTagToArg(X(0),5) = MakeSmall(nvars-ntargets);
    CTagToArg(X(0),6) = MakeSmall(pdata->yoffset+k1);
  }
  ent = ground;
 ret:
  SP_free(pdata->stk.maxx);
  if (ent==1)
    Pfree;
 ret1:
  dvar_export_done(wam, Actions, ent);
}
