/* Copyright(C) 2013, Swedish Institute of Computer Science */

/*  Bounds-consistent all_different/1 constraint.  See:
    A López-Ortiz, CG Quimper, J Tromp, P van Beek.
    A fast and simple algorithm for bounds consistency of the alldifferent constraint.
    IJCAI 2003: 245-250.
*/

#include "fd.h"
#include "dvars.h"

struct alldiff_data {
  void (SPCDECL *destructor)(void *);
  SPEnv *spenv;
  SP_globref refbase;
  int nvars;			/* _original_ ntargets */
  int ntargets;
  int *target;			/* [nvars], "active" var */
  int *mintarget;		/* [nvars], "active" var */
  int *maxtarget;		/* [nvars], "active" var */
  struct {
    SP_integer *bounds;		/* [2*nvars + 2] */
    SP_integer *d;		/* [2*nvars + 2] */
    int *t;			/* [2*nvars + 2] */
    int *h;			/* [2*nvars + 2] */
  } ptrs;
  struct {
    TAGGED *min;		/* [nvars] */
    TAGGED *max;		/* [nvars] */
    int *minrank;		/* [nvars] */
    int *maxrank;		/* [nvars] */
  } sorted;
  Dvar dvar;			/* [nvars] */
  /** begin bc_alldiff_lia **/
  SP_integer mincost;
  SP_integer maxcost;
  int nblocks;
  int heapsize;
  int nrows;
  int opt;
  struct {
    int *low;
    int *up;
    SP_integer *first;
    SP_integer *last;
    int *ind;
    int *heap;
    int *type;			/* [nrows+1] */
    int *lhs;			/* [nrows+1] */
    int *row;			/* [total row elements] */
    int *target;
    TAGGED forbidden;
  } lia;
  /** end   bc_alldiff_lia **/
};

#define NRefs (2*pdata->nvars + 3*pdata->nrows + 1)
#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define SV(T) (pdata->target[T])
#define DVAR(T) (pdata->dvar+(T))
#define ENT_FLAG(R) RefGlob(pdata->refbase+(2*pdata->nvars + 2*pdata->nrows)+(R))
#define PRUNED_VALS RefGlob(pdata->refbase+NRefs-1)
#define XMIN(I) dvar_min_l(DVAR(I))
#define XMAX(I) dvar_max_l(DVAR(I))

static void SPCDECL bc_alldiff_destructor(void *pdata_v)
{
  struct alldiff_data *pdata = (struct alldiff_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,NRefs);
  SP_free(pdata);
}


/* sort by ascending min */
static int 
cmp_asc_dvar_min(Wam wam, int *i1, int *i2)
{
  struct alldiff_data *pdata = fd.gdata;
  TAGGED tmin1 = pdata->sorted.min[*i1];
  TAGGED tmin2 = pdata->sorted.min[*i2];

  return (tmin1==tmin2 ? 0 :
	  tmin1==Inf ? -1 :
	  tmin2==Inf ? 1 :
	  Tlt(tmin1,tmin2) ? -1 : 1);
}

#define QType int
#define QCmp  cmp_asc_dvar_min
#define QSort qsort_asc_dvar_min
#include "qsort.ic"

/* sort by ascending max */
static int 
cmp_asc_dvar_max(Wam wam, int *i1, int *i2)
{
  struct alldiff_data *pdata = fd.gdata;
  TAGGED tmax1 = pdata->sorted.max[*i1];
  TAGGED tmax2 = pdata->sorted.max[*i2];

  return (tmax1==tmax2 ? 0 :
	  tmax1==Sup ? 1 :
	  tmax2==Sup ? -1 :
	  Tlt(tmax1,tmax2) ? -1 : 1);
}

#define QType int
#define QCmp  cmp_asc_dvar_max
#define QSort qsort_asc_dvar_max
#include "qsort.ic"

/* sort by descending min */
static int 
cmp_desc_dvar_min(Wam wam, int *i1, int *i2)
{
  struct alldiff_data *pdata = fd.gdata;
  TAGGED tmin1 = pdata->sorted.min[*i1];
  TAGGED tmin2 = pdata->sorted.min[*i2];

  return (tmin1==tmin2 ? 0 :
	  tmin1==Inf ? 1 :
	  tmin2==Inf ? -1 :
	  Tlt(tmin1,tmin2) ? 1 : -1);
}

#define QType int
#define QCmp  cmp_desc_dvar_min
#define QSort qsort_desc_dvar_min
#include "qsort.ic"

/* sort by descending max */
static int 
cmp_desc_dvar_max(Wam wam, int *i1, int *i2)
{
  struct alldiff_data *pdata = fd.gdata;
  TAGGED tmax1 = pdata->sorted.max[*i1];
  TAGGED tmax2 = pdata->sorted.max[*i2];

  return (tmax1==tmax2 ? 0 :
	  tmax1==Inf ? 1 :
	  tmax2==Inf ? -1 :
	  Tlt(tmax1,tmax2) ? 1 : -1);
}

#define QType int
#define QCmp  cmp_desc_dvar_max
#define QSort qsort_desc_dvar_max
#include "qsort.ic"

#define setpathmax(A,X,D)			\
(D) = (X);					\
while ((D)<(A)[D]) (D) = (A)[D];		\

#define pathset(A,X,Y,Z)				\
if ((X)!=(Y)) { int mi=(X), mv;				\
  do {mv=(A)[mi]; (A)[mi]=(Z); mi=mv;} while (mv!=(Y));	\
}							\

/* Input: 
   pdata->ptrs.bounds: array of all (min) and (max+1) among the variables,
   pdata->maxsorted.minrank[i]: index in pdata->ptrs.bounds of dvar_min_l(DVAR(i))
   pdata->maxsorted.maxrank[i]: index in pdata->ptrs.bounds of dvar_max_l(DVAR(i))+1
   inf has rank 0
   sup has rank nb+1

   Output:
   pdata->maxsorted.min[] updated if minp==TRUE
   pdata->maxsorted.max[] updated if minp==FALSE
*/
static SP_BOOL
bc_alldiff_quimper(Wam wam, struct alldiff_data *pdata,int nb,int niv,int minp)
{
  int i;
  int *t = pdata->ptrs.t;
  int *h = pdata->ptrs.h;
  int *target = (minp ? pdata->maxtarget : pdata->mintarget);
  SP_integer *d = pdata->ptrs.d;
  SP_integer *bounds = pdata->ptrs.bounds;
  
  (void)wam;
  t[0] = h[0] = 0;		/* [MC] for rank 0 (inf) to work */
  for (i=1; i<nb+2; i++) {
    t[i] = h[i] = i-1;
    d[i] = bounds[i] - bounds[i-1];
  }
  for (i=0; i<niv; i++) {
    int x, y, z, j, vv, ti=target[i];
    x = pdata->sorted.minrank[ti];
    y = pdata->sorted.maxrank[ti];
    setpathmax(t, x+1, z);
    j = t[z];
    if (--d[z]==0) {
      t[z] = z+1;
      setpathmax(t, t[z], z);
      t[z] = j;
    }
    pathset(t, x+1, z, z);
    if (d[z] < bounds[z] - bounds[y])
      return FALSE;
    if (h[x] > x) {
      setpathmax(h, h[x], vv);
      if (minp)
	pdata->sorted.min[ti] = MakeSmall(bounds[vv]);
      else
	pdata->sorted.max[ti] = MakeSmall(-bounds[vv]);
      pathset(h, x, vv, vv);
    }
    if (d[z] == bounds[z] - bounds[y]) {
      pathset(h, h[y], j-1, y);
      h[y] = j-1;
    }
  }
  return TRUE;
}

/* partition targets into kernel/nonkernel */
/* returns #kernel */
static int
bc_alldiff_split(struct alldiff_data *pdata)
{
  int niv = pdata->ntargets;
  int niv0 = niv;

  do {
    /* partition into kernel/nonkernel */
    int inf = 0;
    int sup = niv-1;
    int held = SV(sup); /* sup is the hole */
    int current = SV(0);

    niv0 = niv;
    while (inf<=sup) {
      Dvar dv = DVAR(current);
      if (AreSmall(dvar_min_t(dv),dvar_max_t(dv)) &&
	  dvar_max_l(dv)-dvar_min_l(dv) < niv) {
	SV(inf) = current;
	inf++;
	current = (inf>=sup ? held : SV(inf));
      } else {
	SV(sup) = current;
	sup--;
	current = (inf>=sup ? held : SV(sup));
      }
    }
    niv = inf;
  } while (niv0 != niv);
  return niv;
}

static int
bc_alldiff_filter(Wam wam, struct alldiff_data *pdata)
{
  int i, j, nb=0, rc=0;
				/* split into kernel/nonkernel */
  int niv = bc_alldiff_split(pdata);
  int nov = pdata->ntargets - niv;
  if (niv==0)
    return 0;
				/* populate mintarget[], maxtarget[], sorted.min[], sorted.max[] */
  for (j=0; j<niv+nov; j++) {
    int elt = pdata->target[j];
    Dvar dv = DVAR(elt);
    pdata->mintarget[j] = pdata->maxtarget[j] = elt;
    pdata->sorted.min[elt] = dvar_min_t(dv);
    pdata->sorted.max[elt] = dvar_max_t(dv);
  }
				/* sort maxtarget[] ascending, mintarget[] descending */
  qsort_asc_dvar_max(wam, pdata->maxtarget, niv);
  qsort_desc_dvar_min(wam, pdata->mintarget, niv);
  if (nov>0) {
    qsort_asc_dvar_max(wam, pdata->maxtarget+niv, nov);
    qsort_desc_dvar_min(wam, pdata->mintarget+niv, nov);
  }
				/* populate bounds[], minrank[], maxrank[] */
  i = niv;
  j = 0;
  pdata->ptrs.bounds[nb++] = -CLPFD_MAXINT2;
  while (i>0 && j<niv) {
    int mi = pdata->mintarget[i-1];
    int mj = pdata->maxtarget[j];
    TAGGED ti = pdata->sorted.min[mi];
    TAGGED tj = pdata->sorted.max[mj];
    SP_integer b = pdata->ptrs.bounds[nb-1];

    if (ti==Inf) {
      pdata->sorted.minrank[mi] = 0;
      --i;
    } else if (GetSmall(ti)==b) {
      pdata->sorted.minrank[mi] = nb-1;
      --i;
    } else if (tj==Sup || Tle(ti,tj)) { /* extra care for == case! */
      pdata->ptrs.bounds[nb++] = GetSmall(ti);
      pdata->sorted.minrank[mi] = nb-1;
      --i;
    } else if (GetSmall(tj)+1==b) {
      pdata->sorted.maxrank[mj] = nb-1;
      j++;
    } else /* if (Tgt(ti,tj)) */ {
      pdata->ptrs.bounds[nb++] = GetSmall(tj)+1;
      pdata->sorted.maxrank[mj] = nb-1;
      j++;      
    }
  }
  for (; j<niv; j++) {
    int mj = pdata->maxtarget[j];
    TAGGED tj = pdata->sorted.max[mj];
    SP_integer b = pdata->ptrs.bounds[nb-1];

    if (tj==Sup) {
      pdata->sorted.maxrank[mj] = nb;
    } else if (GetSmall(tj)+1==b) {
      pdata->sorted.maxrank[mj] = nb-1;
    } else {
      pdata->ptrs.bounds[nb++] = GetSmall(tj)+1;
      pdata->sorted.maxrank[mj] = nb-1;
    }
  }
  pdata->ptrs.bounds[nb++] = CLPFD_MAXINT2+1;
				/* run Quimper algorithm updating lower bounds */
  if (!bc_alldiff_quimper(wam, pdata,nb-2,niv,TRUE))
    return -1;
				/* update lower bounds for nonkernel, if any */
  if (nov>0) {
    int *h = pdata->ptrs.h;
    SP_integer *bounds = pdata->ptrs.bounds;
    
    for (i=niv+nov, j=0; i>niv; --i) {
      int ti = pdata->mintarget[i-1];
      TAGGED sm = pdata->sorted.min[ti];
      
      if (sm!=Inf) {
	while (GetSmall(sm)>=bounds[j+1])
	  j++;
	if (h[j] > j) {
	  int j1;
	  setpathmax(h, h[j], j1);
	  pdata->sorted.min[ti] = MakeSmall(bounds[j1]);
	}
      }
    }
  }
				/* reverse pdata->ptrs.bounds and replace each element x by 1-x; */
  for (i=0; i<(nb+1)>>1; i++) {
    int k = nb-1-i;
    SP_integer left = 1-pdata->ptrs.bounds[i];
    SP_integer right = 1-pdata->ptrs.bounds[k];
    pdata->ptrs.bounds[i] = right;
    pdata->ptrs.bounds[k] = left;
  }
				/* swap minrank and maxrank, replacing each element x by nb-1-x; */
  for (i=0; i<niv; i++) {
    int ti = SV(i);
    int newmin = nb-1-pdata->sorted.maxrank[ti];
    int newmax = nb-1-pdata->sorted.minrank[ti];
    pdata->sorted.minrank[ti] = newmin;
    pdata->sorted.maxrank[ti] = newmax;
  }
				/* run Quimper algorithm updating upper bounds */
  if (!bc_alldiff_quimper(wam, pdata,nb-2,niv,FALSE))
    return -1;
				/* update lower bounds for nonkernel, if any */
  if (nov>0) {
    int *h = pdata->ptrs.h;
    SP_integer *bounds = pdata->ptrs.bounds;
    
    for (i=niv+nov, j=0; i>niv; --i) {
      int ti = pdata->maxtarget[i-1];
      TAGGED sm = pdata->sorted.max[ti];
      
      if (sm!=Sup) {
	while (-GetSmall(sm)>=bounds[j+1])
	  j++;
	if (h[j] > j) {
	  int j1;
	  setpathmax(h, h[j], j1);
	  pdata->sorted.max[ti] = MakeSmall(-bounds[j1]);
	}
      }
    }
  }
  
				/* prune each dvar by pdata->sorted.min .. max; */
				/* fail if any pruning fails; */
				/* rc |= 1 if we hit a hole; */
  for (i=0; i<niv+nov; i++) {
    int ti = SV(i);
    Dvar dv = DVAR(ti);
    int sc = dvar_fix_interval_t(dv, pdata->sorted.min[ti], pdata->sorted.max[ti]);
    if (sc<0) {
      return -1;
    } else if (((sc&DV_PRUNED_MIN) && Tgt(dvar_min_t(dv),pdata->sorted.min[ti])) ||
	       ((sc&DV_PRUNED_MAX) && Tlt(dvar_max_t(dv),pdata->sorted.max[ti]))) {
      rc |= 1;			/* hit a hole - must rerun */
    }
  }
  return rc;
}
		
static struct alldiff_data *
bc_alldiff_alloc(Wam wam, int nvars, int nrows, int total_rows, TAGGED handle)
{
  struct alldiff_data *pdata;
  char *ptr;
  SP_integer total_size = 
    (4*nvars+4)*sizeof(SP_integer) +
    (4*nvars+4)*sizeof(int) +
    2*nvars*sizeof(TAGGED) +
    5*nvars*sizeof(int) +
    (nvars+nrows)*sizeof(struct dvar) +
    (2*nrows+2)*sizeof(int) +
    total_rows*sizeof(int) +
    2*nvars*sizeof(SP_integer) +
    4*nvars*sizeof(int);
  
  pdata = Palloc(struct alldiff_data, total_size, handle);
  pdata->destructor = bc_alldiff_destructor;
  FD_STORE_SPENV(pdata->spenv);
  pdata->nvars = nvars;
  pdata->nrows = nrows;
  pdata->refbase = SP_alloc_globrefs(NRefs);
  ptr = (char *)(pdata+1);
  pdata->dvar = (Dvar)ptr;
  ptr = (char *)(pdata->dvar+nvars+nrows);
  pdata->ptrs.bounds = (SP_integer *)ptr;
  ptr = (char *)(pdata->ptrs.bounds+2*nvars+2);
  pdata->ptrs.d = (SP_integer *)ptr;
  ptr = (char *)(pdata->ptrs.d+2*nvars+2);
  pdata->sorted.min = (TAGGED *)ptr;
  ptr = (char *)(pdata->sorted.min+nvars);
  pdata->sorted.max = (TAGGED *)ptr;
  ptr = (char *)(pdata->sorted.max+nvars);
  pdata->lia.first = (SP_integer *)ptr;
  ptr = (char *)(pdata->lia.first+nvars);
  pdata->lia.last = (SP_integer *)ptr;
  ptr = (char *)(pdata->lia.last+nvars);
  pdata->ptrs.t = (int *)ptr;
  ptr = (char *)(pdata->ptrs.t+2*nvars+2);
  pdata->ptrs.h = (int *)ptr;
  ptr = (char *)(pdata->ptrs.h+2*nvars+2);
  pdata->sorted.minrank = (int *)ptr;
  ptr = (char *)(pdata->sorted.minrank+nvars);
  pdata->sorted.maxrank = (int *)ptr;
  ptr = (char *)(pdata->sorted.maxrank+nvars);
  pdata->target = (int *)ptr;
  ptr = (char *)(pdata->target+nvars);
  pdata->mintarget = (int *)ptr;
  ptr = (char *)(pdata->mintarget+nvars);
  pdata->maxtarget = (int *)ptr;
  ptr = (char *)(pdata->maxtarget+nvars);
  pdata->lia.type = (int *)ptr;
  ptr = (char *)(pdata->lia.type+nrows+1);
  pdata->lia.lhs = (int *)ptr;
  ptr = (char *)(pdata->lia.lhs+nrows+1);
  pdata->lia.row = (int *)ptr;
  ptr = (char *)(pdata->lia.row+total_rows);
  pdata->lia.low = (int *)ptr;
  ptr = (char *)(pdata->lia.low+nvars);
  pdata->lia.up = (int *)ptr;
  ptr = (char *)(pdata->lia.up+nvars);
  pdata->lia.ind = (int *)ptr;
  ptr = (char *)(pdata->lia.ind+nvars);
  pdata->lia.heap = (int *)ptr;
  ptr = (char *)(pdata->lia.heap+nvars);
  SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
  return pdata;
}

/*
  '$fd_bc_alldiff'(+State0, +State, -Actions).
  State = state(Vec,NDone,Handle,Stamp)
*/
void SPCDECL
prolog_fd_bc_alldiff(Wam wam,
		     SP_term_ref State0,
		     SP_term_ref State,
		     SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  TAGGED handle, telt, tvec, ymin, ymax;
  SP_integer state_stamp;
  int i, nvars=0, rerun;
  struct alldiff_data *pdata;
  SP_BOOL committed;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct alldiff_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get Vec */
    while (TagIsLST(tvec)) {	/* count terms, moving ground terms to RHS */
      DerefCdr(tvec,tvec);
      nvars++;
    }
    pdata = bc_alldiff_alloc(wam, nvars, 0, 0, handle);
    DerefArg(tvec,X(0),1);		/* get Vec */
    for (i=0; i<nvars; i++) {
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      dvar_init(DVAR(i), RefAttr(i), RefVar(i));
      SV(i) = i;
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
  }

				/* RESUME HERE */
  fd.gdata = pdata;
  DerefArg(telt,X(0),2);
  pdata->ntargets = nvars-GetSmall_int(telt);
  for (i=pdata->ntargets-1; i>=0; i--) {
    int elt = SV(i);
    Dvar dv = DVAR(elt);
    dvar_refresh(dv);
  }
  do {
    rerun = bc_alldiff_filter(wam, pdata);
  } while (FALSE /*rerun==1*/);
  if (rerun<0)
    goto ret;
  /* determine min/max domain value of any nonground var */
  ymin = Sup;
  ymax = Inf;
  for (i=0; i<pdata->ntargets; i++) {
    Dvar dv = DVAR(SV(i));
    TAGGED tmin = dvar_min_t(dv);
    TAGGED tmax = dvar_max_t(dv);
    dvar_export(dv);
    if (!dvar_is_integer(dv)) {
      if (FDgt(ymin,tmin)) {
	ymin = tmin;
      }
      if (FDlt(ymax,tmax)) {
	ymax = tmax;
      }
    }
  }
  /* partition into target/source */
  if (rerun) {
    ent = 0;
  } else {
    int inf = 0;
    int sup = pdata->ntargets-1;
    int held = SV(sup); /* sup is the hole */
    int current = SV(0);
    
    while (inf<=sup) {
      Dvar dv = DVAR(current);
      TAGGED dvmin = dvar_min_t(dv);
      if (!dvar_is_integer(dv) || InInterval(dvmin,ymin,ymax)) {
	SV(inf) = current;
	inf++;
	current = (inf>=sup ? held : SV(inf));
      } else {
	SV(sup) = current;
	sup--;
	current = (inf>=sup ? held : SV(sup));
      }
    }
    CTagToArg(X(0),2) = MakeSmall(nvars-inf);
    ent = (inf<=1);
  }
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}

#define SWAP(I,J)				\
{						\
  int vi = pdata->lia.heap[I];			\
  int vj = pdata->lia.heap[J];			\
  pdata->lia.heap[I] = vj;			\
  pdata->lia.heap[J] = vi;			\
}

static void heapify_mincost(struct alldiff_data *pdata,
			    int i)
{
  int *heap = pdata->lia.heap;
  
  for (;;) {
    int l = (i<<1)+1;
    int topmost = i;
    if (l<pdata->heapsize && XMAX(heap[l]) < XMAX(heap[topmost]))
      topmost = l;
    if (l+1<pdata->heapsize && XMAX(heap[l+1]) < XMAX(heap[topmost]))
      topmost = l+1;
    if (topmost==i)
      break;
    SWAP(i,topmost);
    i = topmost;
  }
}

static void heap_insert_mincost(struct alldiff_data *pdata,
				int elt)
{
  int *heap = pdata->lia.heap;
  int i = pdata->heapsize++;
  SP_integer key = XMAX(elt);

  while (i>0 && XMAX(heap[(i-1)>>1]) > key) {
    heap[i] = heap[(i-1)>>1];
    i = (i-1)>>1;
  }  
  heap[i] = elt;
}

static int lia_split(struct alldiff_data *pdata,
		     int row)
{
  int sup = pdata->lia.lhs[row+1] - pdata->lia.lhs[row] - 1;
  int ntargets = 0;
  int *target = pdata->lia.row+pdata->lia.lhs[row];
  int current = target[0];
  int held = target[sup];	/* sup is the hole */
  int type = pdata->lia.type[row];
  
  switch (type) {
  case 1:
  case 2:
    pdata->mincost = 0;
    break;
  case 3:
    pdata->mincost = 1;
    break;
  }
  while (ntargets<=sup) {
    Dvar dv = DVAR(current);
    SP_integer dvmin = dvar_min_l(dv);
    if (!dvar_is_integer(dv) || !pdata->opt) {
      target[ntargets] = current;
      ntargets++;
      current = (ntargets>=sup ? held : target[ntargets]);
    } else {
      switch (type) {
      case 1:
	pdata->mincost += dvmin;
	break;
      case 2:
	pdata->mincost += dvmin*dvmin;
	break;
      case 3:
	pdata->mincost *= dvmin;
	break;
      }
      target[sup] = current;
      sup--;
      current = (ntargets>=sup ? held : target[sup]);
    }
  }
  pdata->maxcost = pdata->mincost;
  pdata->lia.target = target;
  return ntargets;
}

static SP_BOOL 
lia_mincost(Wam wam, struct alldiff_data *pdata,int row,int ntargets)
{
  int i=0, m=0, j, varj;
  SP_integer minval = -1;
  int type = pdata->lia.type[row];
  
  for (j=0; j<ntargets; j++) {
    int elt = pdata->lia.target[j];
    Dvar dv = DVAR(elt);
    pdata->sorted.min[elt] = dvar_min_t(dv);
  }
  qsort_asc_dvar_min(wam, pdata->lia.target, ntargets);
  pdata->heapsize = 0;
  for (j=0; j<ntargets; j++) {
    int oldheapsize = pdata->heapsize;
    SP_integer nextval = XMIN(pdata->lia.target[j]);

    do {
      minval++;
    } while (pdata->opt && fd_member(MakeSmall(minval),pdata->lia.forbidden));
    if (minval < nextval)
      minval = nextval;
    while (i<ntargets && XMIN(pdata->lia.target[i]) <= minval) {
      heap_insert_mincost(pdata,pdata->lia.target[i++]);
    }
    pdata->lia.ind[j] = varj = pdata->lia.heap[0];
    pdata->lia.heap[0] = pdata->lia.heap[--pdata->heapsize];
    if (pdata->heapsize>1)
      heapify_mincost(pdata,0);
    switch (type) {
    case 1:
      pdata->mincost += minval;
      break;
    case 2:
      pdata->mincost += (minval)*(minval);
      break;
    case 3:
      pdata->mincost *= minval;
      break;
    }
    if (minval > XMAX(varj))
      return FALSE;
    if (oldheapsize==0) {
      pdata->lia.low[m] = j;
      pdata->lia.first[m] = minval;
    }
    if (pdata->heapsize==0) {
      pdata->lia.up[m] = j;
      pdata->lia.last[m] = minval;
      m++;
    }
  }
  pdata->nblocks = m;
  if (pdata->mincost > dvar_max_l(DVAR(pdata->nvars+row)))
    return FALSE;
  return TRUE;
}

static int lia_filter_mincost(Wam wam,
				     struct alldiff_data *pdata,
				     int row,
				     int *prc)
{
  int b, rerun=0, i=pdata->nblocks-1, rc = *prc;
  SP_integer tbase = dvar_max_l(DVAR(pdata->nvars+row)) - pdata->mincost; 
  int type = pdata->lia.type[row];
  
  (void)wam;
  for (b=i; b>=0; --b) {
    SP_integer max, t = 0;
    int j, found = 0;
    TAGGED tmax;
    
    switch (type) {
    case 1:
      t = tbase + pdata->lia.last[b];
      break;
    case 2:
      {
	double square = (double)(tbase + (pdata->lia.last[b])*(pdata->lia.last[b]));
	
	if (square<0) continue;	/* [MC] 4.3 */
	/* precision loss due to casting argument to sqrt() should be OK.  */
	t = (SP_integer) sqrt(square);
      }
      break;
    case 3:
      t = dvar_max_l(DVAR(pdata->nvars+row)) * (pdata->lia.last[b]) / pdata->mincost;
      break;
    }
    while (pdata->opt && fd_member(MakeSmall(t),pdata->lia.forbidden))
      --t;
    while (!found) {
      if ((i==pdata->nblocks-1 && t>pdata->lia.last[i]) ||
	  (i>0 && pdata->lia.last[i-1]<t && t<pdata->lia.first[i])) {
	found = 1;
	max = t;
      } else if (i>b && pdata->lia.last[i-1]<pdata->lia.first[i]-1 && t>=pdata->lia.first[i]) {
	found = 1;
	max = pdata->lia.first[i]-1;
      } else if (i==b) {
	found = 1;
	max = pdata->lia.last[i];
      } else {
	--i;
      }
    }
    tmax = MakeSmall(max);
    for (j=pdata->lia.low[b]; j<=pdata->lia.up[b]; j++) {
      int x = pdata->lia.ind[j];
      rc |= dvar_fix_max_t(DVAR(x), tmax);
      if (rc<0)
	return -1;
      else if ((rc&DV_PRUNED_MAX) && Tlt(dvar_max_t(DVAR(x)),tmax))
	rerun = 1;		/* new bound hit a hole */
    }
  }
  *prc = rc;
  return rerun;
}

static void heapify_maxcost(struct alldiff_data *pdata,
			    int i)
{
  int *heap = pdata->lia.heap;
  
  for (;;) {
    int l = (i<<1)+1;
    int topmost = i;
    if (l<pdata->heapsize && XMIN(heap[l]) > XMIN(heap[topmost]))
      topmost = l;
    if (l+1<pdata->heapsize && XMIN(heap[l+1]) > XMIN(heap[topmost]))
      topmost = l+1;
    if (topmost==i)
      break;
    SWAP(i,topmost);
    i = topmost;
  }
}

static void heap_insert_maxcost(struct alldiff_data *pdata,
				int elt)
{
  int *heap = pdata->lia.heap;
  int i = pdata->heapsize++;
  SP_integer key = XMIN(elt);

  while (i>0 && XMIN(heap[(i-1)>>1]) < key) {
    heap[i] = heap[(i-1)>>1];
    i = (i-1)>>1;
  }  
  heap[i] = elt;
}

static SP_BOOL 
lia_maxcost(Wam wam, struct alldiff_data *pdata,int row,int ntargets)
{
  int i=0, m=0, j, varj;
  SP_integer maxval;
  int type = pdata->lia.type[row];
  
  for (j=0; j<ntargets; j++) {
    int elt = pdata->lia.target[j];
    Dvar dv = DVAR(elt);
    pdata->sorted.max[elt] = dvar_max_t(dv);
  }
  qsort_desc_dvar_max(wam, pdata->lia.target, ntargets);
  maxval = XMAX(pdata->lia.target[0])+1;
  pdata->heapsize = 0;
  for (j=0; j<ntargets; j++) {
    int oldheapsize = pdata->heapsize;
    SP_integer nextval = XMAX(pdata->lia.target[j]);

    do {
      maxval--;
    } while (pdata->opt && fd_member(MakeSmall(maxval),pdata->lia.forbidden));
    if (maxval > nextval)
      maxval = nextval;
    while (i<ntargets && XMAX(pdata->lia.target[i]) >= maxval) {
      heap_insert_maxcost(pdata,pdata->lia.target[i++]);
    }
    pdata->lia.ind[j] = varj = pdata->lia.heap[0];
    pdata->lia.heap[0] = pdata->lia.heap[--pdata->heapsize];
    if (pdata->heapsize>1)
      heapify_maxcost(pdata,0);
    switch (type) {
    case 1:
      pdata->maxcost += maxval;
      break;
    case 2:
      pdata->maxcost += (maxval)*(maxval);
      break;
    case 3:
      pdata->maxcost *= maxval;
      break;
    }
    if (maxval < XMIN(varj))
      return FALSE;
    if (oldheapsize==0) {
      pdata->lia.low[m] = j;
      pdata->lia.first[m] = maxval;
    }
    if (pdata->heapsize==0) {
      pdata->lia.up[m] = j;
      pdata->lia.last[m] = maxval;
      m++;
    }
  }
  pdata->nblocks = m;
  if (pdata->maxcost < dvar_min_l(DVAR(pdata->nvars+row)))
    return FALSE;
  return TRUE;
}

static int lia_filter_maxcost(Wam wam,
				     struct alldiff_data *pdata,
				     int row,
				     int *prc)
{
  int b, rerun=0, i=pdata->nblocks-1, rc = *prc;
  SP_integer tbase = dvar_min_l(DVAR(pdata->nvars+row)) - pdata->maxcost;
  int type = pdata->lia.type[row];
  
  (void)wam;
  for (b=i; b>=0; --b) {
    SP_integer min, t = 0;
    int j, found = 0;
    TAGGED tmin;
    
    switch (type) {
    case 1:
      t = tbase + pdata->lia.last[b];
      break;
    case 2:
      {
	double square = (double)(tbase + (pdata->lia.last[b])*(pdata->lia.last[b]));
	
	if (square<0) continue;	/* [MC] 4.3 */
	/* precision loss due to casting argument to sqrt() should be OK. */
	t = (SP_integer) ceil(sqrt(square));
      }
      break;
    case 3:
      t = (dvar_min_l(DVAR(pdata->nvars+row)) * (pdata->lia.last[b]) - 1)
	/ pdata->maxcost
	+ 1;
      break;
    }
    while (pdata->opt && fd_member(MakeSmall(t),pdata->lia.forbidden))
      t++;
    while (!found) {
      if ((i==pdata->nblocks-1 && t<pdata->lia.last[i]) ||
	  (i>0 && pdata->lia.last[i-1]>t && t>pdata->lia.first[i])) {
	found = 1;
	min = t;
      } else if (i>b && pdata->lia.last[i-1]>pdata->lia.first[i]+1 && t<=pdata->lia.first[i]) {
	found = 1;
	min = pdata->lia.first[i]+1;
      } else if (i==b) {
	found = 1;
	min = pdata->lia.last[i];
      } else {
	--i;
      }
    }
    tmin = MakeSmall(min);
    for (j=pdata->lia.low[b]; j<=pdata->lia.up[b]; j++) {
      int x = pdata->lia.ind[j];
      rc |= dvar_fix_min_t(DVAR(x), tmin);
      if (rc<0)
	return -1;
      else if ((rc&DV_PRUNED_MIN) && Tgt(dvar_min_t(DVAR(x)),tmin))
	rerun = 1;		/* new bound hit a hole */
    }
  }
  *prc = rc;
  return rerun;
}

/*
  '$fd_bc_alldiff_lia'(+State0, +State, -Actions).
  State = state(Vec,NDone,Handle,Stamp)
*/
void SPCDECL
prolog_fd_bc_alldiff_lia(Wam wam,
			 SP_term_ref State0,
			 SP_term_ref State,
			 SP_term_ref Actions)
{
  int ent = -1;
  TAGGED handle, telt, tvec, ymin = TaggedHigh, ymax = TaggedLow;
  SP_integer state_stamp;
  int i, j, nvars=0, rerun, rc;
  int nrows, total_rows;
  struct alldiff_data *pdata;
  SP_BOOL committed, posted;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */


/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    posted = FALSE;
    pdata = Pdata(struct alldiff_data,handle);
    nvars = pdata->nvars;
    nrows = pdata->nrows;
  } else {			/* build persistent state */
    TAGGED *h;
    posted = TRUE;
    DerefArg(tvec,X(0),1);	/* get Vec */
    while (TagIsLST(tvec)) {	/* count terms */
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(telt,telt,2);	/* get attribute */
      DerefAttribute(telt,telt); /* dom/4 term */
      if (Tgt(ymin,DomainMin(telt)))
	ymin = DomainMin(telt);
      if (Tlt(ymax,DomainMax(telt)))
	ymax = DomainMax(telt);
      nvars++;
    }
    if (nvars==0) {
      ent = 1;
      dvar_export_done(wam, Actions, ent);
      return;
    }
    DerefArg(tvec,X(0),4);	/* get Eqs */
    nrows = fd_list_length(tvec);
    total_rows = 0;
    while (TagIsLST(tvec)) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(telt,telt,2);	/* get row indices */
      total_rows += fd_list_length(telt);
    }
    pdata = bc_alldiff_alloc(wam, nvars, nrows, total_rows, handle);
    DerefArg(tvec,X(0),1);		/* get Vec */
    for (i=0; i<nvars; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      dvar_init(DVAR(i), RefAttr(i), RefVar(i));
      SV(i) = i;
    }
    DerefArg(tvec,X(0),4);	/* get Eqs, list of eqn(Type,Ixs,Pair) */
    nrows = fd_list_length(tvec);
    i = 0;
    j = 0;
    while (TagIsLST(tvec)) {
      TAGGED pair, type, tixs;
      int i0 = i;
      DerefCar(tixs,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(pair,tixs,3);	/* get row rhs */
      DerefArg(type,tixs,1);	/* get row type */
      DerefArg(tixs,tixs,2);	/* get row indices */
      while (TagIsLST(tixs)) {
	TAGGED tix;
	DerefCar(tix,tixs);
	DerefCdr(tixs,tixs);
	pdata->lia.row[i++] = (int)GetSmall(tix);
      }
      fd_get_var_and_attr(pair,RefAttr(nvars+j));
      dvar_init(DVAR(nvars+j), RefAttr(nvars+j), RefVar(nvars+j));
      pdata->lia.type[j] = (int)GetSmall(type);
      pdata->lia.lhs[j++] = i0;
    }
    pdata->lia.lhs[j++] = i;
    RequireHeap(nrows,EVAL_ARITY);
    h = w->global_top;
    for (i=nrows-1; i>=0; i--)
      ENT_FLAG(i) = h[i] = TagREF(h+i);
    w->global_top = h+nrows; 
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    DerefArg(tvec,X(0),3);	/* get Hack */
    pdata->opt = (int)GetSmall(tvec);
  }

				/* RESUME HERE */
  fd.gdata = pdata;
  DerefArg(tvec,X(0),2);
  pdata->ntargets = nvars-GetSmall_int(tvec);
  if (posted && pdata->opt) {
    TAGGED omni = EmptySet;
    for (i=0; i<nvars; i++) {
      dvar_refresh(DVAR(i));
      omni = fd_merge_into(wam, dvar_set(DVAR(i)),omni);
    }
    omni = fd_interval_subtract(wam, ymin,ymax,omni);
    PRUNED_VALS = fd_globalize(wam, fd_localize(wam, omni),0,EVAL_ARITY); /* beware of GC */
  }
  for (i=pdata->ntargets-1; i>=0; i--) {
    int elt = SV(i);
    Dvar dv = DVAR(elt);
    dvar_refresh(dv);
  }
  for (i=0; i<nrows; i++) {
    dvar_refresh(DVAR(nvars+i));
  }
  do {
    int n, row;
    
    rerun = bc_alldiff_filter(wam, pdata);
    if (rerun<0) goto ret;
    rc = 0;
    for (row=0; row<nrows; row++) {      
      TAGGED t = ENT_FLAG(row);
      DerefSwitch(t,goto cont;);
      continue;
    cont:
      if (pdata->opt) {
	pdata->lia.forbidden = PRUNED_VALS;
	for (i=pdata->ntargets-1; i>=0; i--) {
	  Dvar dv = DVAR(SV(i));
	  TAGGED tvmin = dvar_min_t(dv);
	  if (dvar_is_integer(dv))
	    pdata->lia.forbidden = fd_union_interval(wam, pdata->lia.forbidden,tvmin,tvmin);
	}
      }
      n = lia_split(pdata,row);
      if (!lia_mincost(wam, pdata,row,n)) goto ret;
      rerun |= lia_filter_mincost(wam, pdata,row,&rc);
      if (rerun<0) goto ret;
      if (!lia_maxcost(wam, pdata,row,n)) goto ret;
      rerun |= lia_filter_maxcost(wam, pdata,row,&rc);
      if (rerun<0) goto ret;
      if (pdata->nblocks==0) {
	BindHVA(t,atom_nil);
      }
      j = dvar_fix_interval_l(DVAR(nvars+row), pdata->mincost, pdata->maxcost);
      if (j<0) goto ret;
      else if (((j&DV_PRUNED_MIN) && dvar_min_l(DVAR(nvars+row))>pdata->mincost) ||
	       ((j&DV_PRUNED_MAX) && dvar_max_l(DVAR(nvars+row))<pdata->maxcost))
	rerun |= 1;		/* new bound hit a hole */
      rc |= j;
    }    
  } while (FALSE /*rerun>0 || rc>0*/);
  {
    /* determine min/max domain value of any nonground var */
    TAGGED ymin = TaggedHigh;
    TAGGED ymax = TaggedLow;
    int ground = 1;
    int inf = 0;
    int sup = pdata->ntargets-1;
    int held = SV(sup); /* sup is the hole */
    int current = SV(0);
    
    for (i=0; i<nrows; i++) {
      if (!dvar_is_integer(DVAR(nvars+i)))
	ground = 0;
      dvar_export(DVAR(nvars+i));
    }
    for (i=0; i<pdata->ntargets; i++) {
      Dvar dv = DVAR(SV(i));
      dvar_export(dv);
      if (!dvar_is_integer(dv)) {
	if (Tgt(ymin,dvar_min_t(dv)))
	  ymin = dvar_min_t(dv);
	if (Tlt(ymax,dvar_max_t(dv)))
	  ymax = dvar_max_t(dv);
      }
    }
    if (rerun>0 || rc>0) {
      ent = 0;
    } else {    /* partition into target/source */
      while (inf<=sup) {
	Dvar dv = DVAR(current);
	TAGGED dvmin = dvar_min_t(dv);
	if (!dvar_is_integer(dv) || InInterval(dvmin,ymin,ymax)) {
	  SV(inf) = current;
	  inf++;
	  current = (inf>=sup ? held : SV(inf));
	} else {
	  SV(sup) = current;
	  sup--;
	  current = (inf>=sup ? held : SV(sup));
	}
      }
      CTagToArg(X(0),2) = MakeSmall(nvars-inf);
      ent = (inf<=1 && ground);
    }
  }
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}

/*** for IJCAI 2013 paper ***/

struct sigma {
  SP_WORD value;
  SP_WORD index;
};

struct atleast_le_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;
  SP_globref refbase;
  SP_WORD stamp;			/* increases up to backtracking */
  int nvars;
  int nrefs;
  int b;
  SP_WORD c;
  SP_WORD *a;
  SP_WORD *precious;
  SP_WORD *intermin;
  SP_WORD *intermax;
  struct sigma *sigma;
  int *target;			/* [nvars], "active" var */
  int *p;
  char *flag;
  Dvar dvar;
};

#define FLAG_IN_PLUS   0x1
#define FLAG_IN_MINUS  0x2
#define FLAG_OUT_PLUS  0x4
#define FLAG_OUT_MINUS 0x8

static void SPCDECL atleast_le_destructor(void *pdata_v)
{
  struct atleast_le_data *pdata = (struct atleast_le_data*)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

static DAEMON_RC SPCDECL 
atleast_le_daemon(Wam wam,
		  void *vdata,
		  SP_globref attr_ref,
		  TAGGED *global)
{
  struct atleast_le_data *pdata = (struct atleast_le_data *)vdata;
  TAGGED tstate = RefMutable(CTagToArg(*global,1));
  int j = (int)(attr_ref - pdata->refbase)>>1;
  int ar = Arity(TagToHeadfunctor(tstate));
  TAGGED state_stamp = GetSmall(CTagToArg(tstate,ar));
  TAGGED vset;
  Dvar dv = DVAR(j);
  DAEMON_RC rc = DAEMON_FIX;

  DerefArg(vset,tstate,2);
  dvar_refresh(dv);
  if (dvar_is_integer(dv)) {	/* ensure entailment is detected */
    rc = DAEMON_NOFIX;
  } else if (pdata->stamp!=state_stamp /* non-incremental */ ||
      ((pdata->flag[j] & (FLAG_IN_PLUS|FLAG_IN_MINUS)) &&
       dvar_compare_set(dv,vset)==FDI_DISJOINT) ||
      !dvar_contains_value_l(dv,pdata->precious[j])) {
    rc = DAEMON_NOFIX;
  }
  return rc;
}

static int 
cmp_asc_sigma(Wam wam, struct sigma *l1, struct sigma *l2)
{
  int cmp = CMP(l1->value,l2->value);
  (void)wam;
  if (cmp==0)
    cmp = CMP(l1->index,l2->index);
  return cmp;
}


#define QType struct sigma
#define QCmp  cmp_asc_sigma
#define QSort qsort_asc_sigma
#include "qsort.ic"

static void pruneg(Wam wam, 
		   Dvar dv,
		   TAGGED v,
		   SP_WORD limit,
		   SP_WORD a,
		   SP_WORD b,
		   SP_WORD delta)
{
  if (a > 0) {
    SP_WORD u = FLOORDIV(limit-b,a);
    SP_WORD u2 = FLOORDIV(limit-(b-delta),a);
    v = fd_union_interval(wam, v,Inf,MakeSmall(u));
    dvar_fix_set(dv,v);
    dvar_fix_max_l(dv,u2);
  } else if (a < 0) {
    SP_WORD u = CEILDIV(b-limit,-a);
    SP_WORD u2 = CEILDIV((b-delta)-limit,-a);
    v = fd_union_interval(wam, v,MakeSmall(u),Sup);
    dvar_fix_set(dv,v);
    dvar_fix_min_l(dv,u2);
  } else {
    SP_ASSERT(b-delta <= limit);
    if (b > limit) {
      dvar_fix_set(dv,v);
    }
  }
}

/*
  '$fd_atleast_le'(+State0, +State, -Actions).
  State = state(B,Vset,XVec,As,C,NGround,Handle,Stamp), where B, As, C are ground
*/
void SPCDECL
prolog_fd_atleast_le(Wam wam,
		     SP_term_ref State0,
		     SP_term_ref State,
		     SP_term_ref Actions)
{
  TAGGED handle, tvec, vset;
  SP_WORD *a, c, state_stamp;
  int b, i, n, nvars, ent=-1;
  SP_integer total_size;
  struct atleast_le_data *pdata;
  SP_BOOL committed;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct atleast_le_data,handle);
    nvars = pdata->nvars;
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),3);
    nvars = fd_list_length(tvec);
    total_size =
      nvars*sizeof(struct dvar) +
      nvars*sizeof(struct sigma) +
      4*nvars*sizeof(SP_WORD) +
      2*nvars*sizeof(int) +
      nvars;
    pdata = Palloc(struct atleast_le_data, total_size, handle);
    pdata->destructor = atleast_le_destructor;
    pdata->daemon = atleast_le_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->stamp = state_stamp;	/* trust initially */
    pdata->nvars = nvars;
    pdata->nrefs = 2*nvars;
    pdata->refbase = SP_alloc_globrefs(2*nvars);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr = (char *)(pdata->dvar+nvars);
    pdata->a = (SP_WORD *)ptr;
    ptr = (char *)(pdata->a+nvars);
    pdata->precious = (SP_WORD *)ptr;
    ptr = (char *)(pdata->precious+nvars);
    pdata->intermin = (SP_WORD *)ptr;
    ptr = (char *)(pdata->intermin+nvars);
    pdata->intermax = (SP_WORD *)ptr;
    ptr = (char *)(pdata->intermax+nvars);
    pdata->sigma = (struct sigma *)ptr;
    ptr = (char *)(pdata->sigma+nvars);
    pdata->p = (int *)ptr;
    ptr = (char *)(pdata->p+nvars);
    pdata->target = (int *)ptr;
    ptr = (char *)(pdata->target+nvars);
    pdata->flag = (char *)ptr;
    ptr += nvars;
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    for (i=0; i<nvars; i++) {
      TAGGED telt;
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,RefAttr(i));
      SV(i) = i;
    }
    DerefArg(tvec,X(0),1);
    pdata->b = GetSmall_int(tvec);
    DerefArg(tvec,X(0),5);
    pdata->c = GetSmall(tvec);
    DerefArg(tvec,X(0),4);
    for (i=0; i<nvars; i++) {
      TAGGED telt;
      Dvar dv = DVAR(i);
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      pdata->a[i] = GetSmall(telt);
      dvar_init(dv, RefAttr(i), RefVar(i));
      dvar_attach_daemon(wam, dv, pdata, X(1), functor_dom1);
    }
  }

  /* RESUME HERE */
  pdata->stamp = state_stamp+1;
  DerefArg(tvec,X(0),6);
  n = nvars - GetSmall_int(tvec);
  a = pdata->a;
  b = pdata->b;
  c = pdata->c;
  DerefArg(vset,X(0),2);
  for (i=0; i<n; i++) {
    int j = SV(i);
    Dvar dv = DVAR(j);
    dvar_refresh(dv);
  }
  for (i=n; i<nvars; i++) {
    int j = SV(i);
    Dvar dv = DVAR(j);
    TAGGED u = dvar_min_t(dv);
    if (fd_member(u,vset)) --b;
    c -= a[j]*GetSmall(u);
  }
  /* GAC version, no fixpoint loop */
  {
    int in = 0;
    SP_WORD l = 0;
    SP_WORD sigma_b, sigma_b1;
    for (i=0; i<n; i++) {
      int j = SV(i);
      Dvar dv = DVAR(j);
      
      if (dvar_compare_set(dv,vset)==FDI_DISJOINT) {
	pdata->flag[j] = (a[j]>=0 ? FLAG_OUT_PLUS : FLAG_OUT_MINUS);
      } else {
	pdata->flag[j] = (a[j]>=0 ? FLAG_IN_PLUS : FLAG_IN_MINUS);
	pdata->intermin[j] = GetSmall(fd_intersection_min(dvar_set(dv),vset));
	pdata->intermax[j] = GetSmall(fd_intersection_max(dvar_set(dv),vset));
	pdata->sigma[in].value = 
	  a[j]>=0 ? a[j]*(pdata->intermin[j] - dvar_min_l(dv)) :
	            a[j]*(pdata->intermax[j] - dvar_max_l(dv));
	pdata->sigma[in++].index = i;
      }
      if (pdata->flag[j] & (FLAG_IN_PLUS|FLAG_OUT_PLUS)) {
	l += a[j]*dvar_min_l(dv);
      } else {
	l += a[j]*dvar_max_l(dv);
      }
    }
    if (in < b)
      goto ret;
    qsort_asc_sigma(wam, pdata->sigma,in);
    for (i=0; i<b; i++)
      l += pdata->sigma[i].value;
    if (l > c)
      goto ret;
    for (i=0; i<in; i++)
      pdata->p[pdata->sigma[i].index] = i;
    sigma_b = b<1 ? 0 : pdata->sigma[b-1].value;
    sigma_b1 = b==in ? 0 : pdata->sigma[b].value;
    for (i=0; i<n; i++) {
      int j = SV(i);
      Dvar dv = DVAR(j);
      if (!dvar_is_integer(dv)) {
	char flag = pdata->flag[j];
	if (flag & FLAG_OUT_PLUS) {
	  if (a[j] > 0)
	    dvar_fix_max_l(dv,FLOORDIV(c+a[j]*dvar_min_l(dv)-l,a[j]));
	} else if ((flag & FLAG_IN_PLUS) && pdata->p[i]>=b) {
	  pruneg(wam, dv,vset,c,a[j],-a[j]*dvar_min_l(dv)+l, sigma_b);
	} else if ((flag & FLAG_IN_PLUS) && in>b) {
	  pruneg(wam, dv,vset,c,a[j],sigma_b1-a[j]*pdata->intermin[j]+l, sigma_b1);
	} else if (flag & FLAG_IN_PLUS) {
	  dvar_fix_set(dv,vset);
	  if (a[j] > 0)
	    dvar_fix_max_l(dv,FLOORDIV(c+a[j]*pdata->intermin[j]-l,a[j]));
	} else if (flag & FLAG_OUT_MINUS) {
	  dvar_fix_min_l(dv,CEILDIV(-c-a[j]*dvar_max_l(dv)+l,-a[j]));
	} else if ((flag & FLAG_IN_MINUS) && pdata->p[i]>=b) {
	  pruneg(wam, dv,vset,c,a[j],-a[j]*dvar_max_l(dv)+l, sigma_b);
	} else if ((flag & FLAG_IN_MINUS) && in>b) {
	  pruneg(wam, dv,vset,c,a[j],sigma_b1-a[j]*pdata->intermax[j]+l, sigma_b1);
	} else if (flag & FLAG_IN_MINUS) {
	  dvar_fix_set(dv,vset);
	  dvar_fix_min_l(dv,CEILDIV(-c-a[j]*pdata->intermax[j]+l,-a[j]));
	}
	if (pdata->flag[j] & (FLAG_IN_PLUS|FLAG_OUT_PLUS))
	  pdata->precious[j] = dvar_min_l(dv);
	else
	  pdata->precious[j] = dvar_max_l(dv);
      }
    }
  }
  for (i=0; i<n; i++) {
    int j = SV(i);
    Dvar dv = DVAR(j);
    dvar_pruning_done( dv);
  }
  /* partition into target/source */
  {
    int inf = 0;
    int sup = n-1;
    int held = SV(sup); /* sup is the hole */
    int current = SV(0);
    while (inf<=sup) {
      Dvar dv = DVAR(current);
      dvar_export(dv);
      if (!dvar_is_integer(dv)) {
	SV(inf) = current;
	inf++;
	current = (inf>=sup ? held : SV(inf));
      } else {
	SV(sup) = current;
	sup--;
	current = (inf>=sup ? held : SV(sup));
      }
    }
    CTagToArg(X(0),6) = MakeSmall(nvars-inf);
    ent = (inf<=1);
  }
 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}
