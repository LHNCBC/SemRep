/* Copyright(C) 1999, Swedish Institute of Computer Science */
/* Support for the "longest hole" method. */

#include "fd.h"

#define LH_USE_PROFILE 1

#if LH_USE_PROFILE
# define clh_get_p(IX) fd_profile_get(clh->P,(IX))
#else
# define clh_get_p(IX) (clh->P[IX])
#endif

/* Sort by decreasing <height,length>. */

static int 
cmp_lhs_decr(Wam wam, struct lhs *t1, struct lhs *t2)
{
  SP_integer h1 = t1->h;
  SP_integer h2 = t2->h;

  (void)wam;
  if (h1<h2)
    return 1;
  if (h1>h2)
    return -1;

  h1 = t1->l;
  h2 = t2->l;

  if (h1<h2)
    return 1;
  if (h1>h2)
    return -1;
  else
    return 0;
}

#define QType struct lhs
#define QCmp  cmp_lhs_decr
#define QSort qsort_lhs_decr
#include "qsort.ic"

/* Sort by increasing length. */

static int 
cmp_lhs_incr(Wam wam, struct lhs *t1, struct lhs *t2)
{
  SP_integer h1 = t1->l;
  SP_integer h2 = t2->l;

  (void)wam;
  return CMP(h1,h2);
}

#define QType struct lhs
#define QCmp  cmp_lhs_incr
#define QSort qsort_lhs_incr
#include "qsort.ic"

static void
lh_eval(Wam wam,
	SP_integer eps,
	SP_integer sigma,
	int s,
	int ntask,
	struct lhs *lhs,
	SP_integer *lh_table,
	SP_integer *lh);

static void
lh_preprocessing(Wam wam,
		 SP_integer eps,
		 SP_integer sigma,
		 int s,
		 int ntask,
		 struct lhs *lhs,
		 struct lhs *lhs_eps,
		 SP_integer *ntask_eps,
		 SP_integer *lh_table,
		 SP_integer *lh);

static int
lh_is_easy(Wam wam,
	   SP_integer eps,
	   SP_integer sigma,
	   int closed,
	   int ntask_eps,
	   struct lhs *lhs_eps,
	   SP_integer *sumh,
	   int *nsingle,
	   struct lhs **lhs_single,
	   int *ndisj,
	   struct lhs **lhs_disj,
	   int *nsmall,
	   struct lhs **lhs_small);

static SP_integer
lh_easy_case(SP_integer eps,
	     SP_integer sigma,
	     int closed,
	     SP_integer sumh,
	     int nsingle,
	     struct lhs *lhs_single,
	     int ndisj,
	     struct lhs *lhs_disj,
	     int nsmall,
	     struct lhs *lhs_small);

static SP_integer
lh_upperbound1(SP_integer eps,
	       SP_integer sigma,
	       int ntask_eps,
	       struct lhs *lhs_eps,
	       int closed);

static SP_integer
lh_upperbound2(SP_integer eps,
	       SP_integer sigma,
	       int ntask_eps,
	       struct lhs *lhs_eps);

static SP_integer
lh_upperbound3(SP_integer eps,
	       SP_integer sigma,
	       int ntask_eps,
	       struct lhs *lhs_eps);

static SP_integer
lh_upperbound4(SP_integer eps,
	       SP_integer sigma,
	       int ntask_eps,
	       struct lhs *lhs_eps,
	       SP_integer *lh_table);

static SP_integer
lh_upperbound5(SP_integer eps,
	       SP_integer sigma,
	       int ntask_eps,
	       struct lhs *lhs_eps);


static SP_integer
clh1(Wam wam,
     SP_integer eps,
     SP_integer sigma,
     int *nback,
     int ntask,
     struct lhs *lhs,
     SP_integer *lh_table,
     SP_integer *lch_table,
     SP_integer row,
     SP_integer bbheight,
     int closed,
     int enable);


void
fd_lh_init(Wam wam,
	   SP_integer sigma0,
	   int row,
	   int nrows,
	   SP_integer bblength,
	   SP_integer bbheight,
	   int ntask,
	   struct lhs *lhs,
	   SP_integer *lh_table,
	   SP_integer *lch_table,
	   int maxbacks,
	   int maxfails)
{
  SP_integer eps; 
  SP_integer offset = row*bbheight;
  SP_integer sigma = sigma0+row;
  int lch_maxfails = maxfails;
  int lh_maxfails = maxfails;
  
  (void)nrows;
  qsort_lhs_decr(wam, lhs,ntask);
  for (eps=1; eps<=bbheight; eps++) {
    SP_integer lh[2];
    lh_eval(wam, eps,sigma,-1,ntask,lhs,lh_table+offset,lh);
    lh_table[offset+eps-1] = lh[0] < bblength ? lh[0] : bblength;
    lch_table[offset+eps-1] = lh[1] < bblength ? lh[1] : bblength;
    if (maxbacks>=0) {
      int nbacks = maxbacks;
/*       SP_integer ub = lch_table[offset+eps-1];    */

      if (lch_maxfails>0) {
	SP_integer exact = clh1(wam, eps, sigma, &nbacks, ntask, lhs, lh_table, lch_table, row, bbheight, TRUE, 0x3);
	lch_table[offset+eps-1] = exact;
	if (nbacks<0)
	  lch_maxfails--;
	else
	  lch_maxfails = maxfails;
      }
    }
    if (maxbacks>=0) {
      int nbacks = maxbacks;
/*       SP_integer ub = lh_table[offset+eps-1];    */

      if (lh_maxfails>0) {
	SP_integer exact = clh1(wam, eps, sigma, &nbacks, ntask, lhs, lh_table, lch_table, row, bbheight, FALSE, 0x3);
	lh_table[offset+eps-1] = exact;
	if (nbacks<0)
	  lh_maxfails--;
	else
	  lh_maxfails = maxfails;
      }
    }
  }
}

static void
lh_eval(Wam wam,
	SP_integer eps,
	SP_integer sigma,
	int s,
	int ntask,
	struct lhs *lhs,
	SP_integer *lh_table,
	SP_integer *lh)
{
  struct lhs *lhs_eps = Malloc(ntask, struct lhs);
  struct lhs *lhs_single, *lhs_disj, *lhs_small;
  SP_integer ntask_eps, sumh, b[5];
  int nsingle, ndisj, nsmall, easy, i;

  lh_preprocessing(wam, eps,sigma,s,ntask,lhs,lhs_eps,&ntask_eps,lh_table,lh);
  if (ntask_eps==0) {
    lh[0] += sigma/eps;
    lh[1] += sigma/eps;
  } else {
    int k;
    for (k=FALSE; k<=TRUE; k++) {
      easy = lh_is_easy(wam, eps,sigma,k,(int)ntask_eps,lhs_eps,&sumh,&nsingle,&lhs_single,&ndisj,&lhs_disj,&nsmall,&lhs_small);
      switch (easy) {
      case 1:
	lh[k] += lh_easy_case(eps,sigma,k,sumh,nsingle,lhs_single,ndisj,lhs_disj,nsmall,lhs_small);
	Free(lhs_single);
	Free(lhs_disj);
	Free(lhs_small);
	break;
      case 2:
	b[0] = lh_upperbound1(eps,sigma,(int)ntask_eps,lhs_eps,k);
	b[1] = lh_upperbound2(eps,sigma,(int)ntask_eps,lhs_eps);
	b[2] = lh_upperbound3(eps,sigma,(int)ntask_eps,lhs_eps);
	b[3] = lh_upperbound4(eps,sigma,(int)ntask_eps,lhs_eps,lh_table);
	b[4] = lh_upperbound5(eps,sigma,(int)ntask_eps,lhs_eps);
	for (i=1; i<5; i++)
	  if (b[0] > b[i])
	    b[0] = b[i];
	lh[k] += b[0];
      }
    }
  }
  Free(lhs_eps);
}

static void
lh_preprocessing(Wam wam,
		 SP_integer eps,
		 SP_integer sigma,
		 int s,
		 int ntask,
		 struct lhs *lhs,
		 struct lhs *lhs_eps,
		 SP_integer *ntask_eps,
		 SP_integer *lh_table,
		 SP_integer *lh)
{
  int shrink_length = 0;
  int i, j;

  lh[0] = 0;
  *ntask_eps = 0;
  
  i = 0;
  while (i<ntask && (i==s || lhs[i].h>eps))
    i++;
  while (i<ntask && (i==s || lhs[i].h==eps)) {
    lh[0] += (i!=s)*lhs[i].l;
    i++;
  }
  if (i>=ntask)
    goto ret;
  for (j=i; j<ntask; j++) {
    if (j!=s) {
      SP_integer lh1[2];
      lh1[0] = lh_table[eps-lhs[j].h-1];
      if (lh1[0]>0) {
	if (s==-1 && eps==2*lhs[j].h)
	  lh_eval(wam, eps-lhs[j].h,sigma,j,ntask,lhs,lh_table,lh1);
	if (lh1[0]>0) {
	  int shrink;
	  lhs_eps[*ntask_eps].l = lh1[0] < lhs[j].l ? lh1[0] : lhs[j].l;
	  lhs_eps[*ntask_eps].h = lhs[j].h;
	  shrink = lhs_eps[*ntask_eps].l!=lhs[j].l;
	  lhs_eps[*ntask_eps].s = shrink;
	  shrink_length |= shrink;
	  *ntask_eps += 1;
	}
      }
    }
  }
  if (shrink_length)
    qsort_lhs_decr(wam, lhs_eps,*ntask_eps);
 ret:
  lh[1] = lh[0];
}

static int
lh_is_easy(Wam wam,
	   SP_integer eps,
	   SP_integer sigma,
	   int closed,
	   int ntask_eps,
	   struct lhs *lhs_eps,
	   SP_integer *sumh,
	   int *nsingle,
	   struct lhs **lhs_single,
	   int *ndisj,
	   struct lhs **lhs_disj,
	   int *nsmall,
	   struct lhs **lhs_small)
{
  int i, ns, nd, nm;
  SP_integer hdisj=0, hsingle=0;
  SP_integer sum = (sigma+eps-1)/eps;

  for (i=0; !closed && i<ntask_eps-1 && lhs_eps[i].h+lhs_eps[ntask_eps-1].h>eps; i++)
    if (hsingle==0)		/* 20080912 */
      hsingle = lhs_eps[i].h;
  ns = i;
  for (; i<ntask_eps && (i==ns || lhs_eps[i].h+lhs_eps[i-1].h>eps); i++) {
    if (hdisj==0)
      hdisj = lhs_eps[i].h;
  }
  nd = i-ns;
  *sumh = 0;
  while (i<ntask_eps && hdisj + *sumh <= eps)
    *sumh += lhs_eps[i++].h;
  if (hdisj + *sumh + sigma < eps && hsingle + sigma < eps) { /* 20080912 */
    return 0;
  } else if (hdisj + *sumh > eps) {
    return 2;
  } else {
    nm = ntask_eps-ns-nd;
    *nsingle = ns;
    *ndisj = nd+1;
    *nsmall = nm+1;
    *lhs_single = Malloc(ns, struct lhs);
    *lhs_disj = Malloc(nd+1, struct lhs);
    *lhs_small = Malloc(nm+1, struct lhs);
    for (i=0; i<ns; i++)
      (*lhs_single)[i] = lhs_eps[i];
    for (i=0; i<nd; i++)
      (*lhs_disj)[i] = lhs_eps[i+ns];
    for (i=0; i<nm; i++)
      (*lhs_small)[i] = lhs_eps[i+ns+nd];
    for (i=0; i<ntask_eps; i++)
      sum += lhs_eps[i].l;
    (*lhs_disj)[nd].l = sum;
    (*lhs_disj)[nd].h = 0;
    (*lhs_small)[nm].l = sum;
    (*lhs_small)[nm].h = 0;
    if (nm>1)
      (*lhs_small)[nm].l += (*lhs_small)[nm-1].l;
    qsort_lhs_incr(wam, (*lhs_small),nm);
    return 1;
  }
}

static SP_integer
lh_easy_case(SP_integer eps,
	     SP_integer sigma,
	     int closed,
	     SP_integer sumh,
	     int nsingle,
	     struct lhs *lhs_single,
	     int ndisj,
	     struct lhs *lhs_disj,
	     int nsmall,
	     struct lhs *lhs_small)
{
  int i=0, j=0, k=0, prevr=1;
  SP_integer prevdl=0, prevdh=0, prevsl=0, prevsh=0, sumr=0, last=0, suml=0;

  (void)closed;
  (void)ndisj;
  (void)nsmall;
  for (;;) {
    int get_next_i = (sumr+prevdl <= prevsl);
    int get_next_j = (sumr+prevdl >= prevsl);
    SP_integer gap, l, surf;
    if (prevr && get_next_i) {
      sumr += prevdl;
      prevdl = lhs_disj[i].l;
      prevdh = lhs_disj[i++].h;
    }
    if (prevr && get_next_j) {
      sumh -= prevsh;
      prevsl = lhs_small[j].l;
      prevsh = lhs_small[j++].h;
    }
    l = sumr+prevdl < prevsl ? sumr+prevdl : prevsl;
    gap = eps-sumh-prevdh;
    surf = (l-last)*gap;
    prevr = (k>=nsingle || gap<=eps-lhs_single[k].h);
    if (!prevr) {
      gap = eps-lhs_single[k].h;
      surf = gap*lhs_single[k].l;
    }
    if (surf>sigma) {
      return suml + last + sigma/gap;
    }
    sigma -= surf;
    if (prevr) {
      last = l;
    } else {
      suml += lhs_single[k++].l;
    }
  }
}

static SP_integer
lh_upperbound1(SP_integer eps,
	       SP_integer sigma,
	       int ntask_eps,
	       struct lhs *lhs_eps,
	       int closed)
{
  SP_integer surf_rest = 0;
  SP_integer surf_max1 = 0;
  SP_integer surf_max2 = 0;
  int i=0;

  for (i=0; i<ntask_eps && (i==0 || lhs_eps[i].h+lhs_eps[i-1].h>eps); i++) {
    SP_integer surf = lhs_eps[i].l*lhs_eps[i].h;
    if (lhs_eps[i].s) {
      if (surf >= surf_max1)
	surf_max2 = surf_max1, surf_max1 = surf;
      else if (surf > surf_max2)
	surf_max2 = surf;
    } else {
      surf_rest += surf;
    }
  }
  for (; i<ntask_eps; i++)
    surf_rest += lhs_eps[i].l*lhs_eps[i].h;
  return (surf_rest + surf_max1 + (!closed)*surf_max2 + sigma)/eps;
}

static SP_integer
lh_upperbound2(SP_integer eps,
	       SP_integer sigma,
	       int ntask_eps,
	       struct lhs *lhs_eps)
{
  SP_integer l1=0, l2=0, l12=0, small=sigma, len=0;
  int removed=0;
  int i, ndisj, count=0;
  for (i=0; i<ntask_eps && (i==0 || lhs_eps[i].h+lhs_eps[i-1].h>eps); i++) {
    if (lhs_eps[i].s) {
      if (l1==0) {
	l1 = lhs_eps[i].l;
      } else if (l2==0) {
	l2 = lhs_eps[i].l;
	l12 = l1;
	if (l12>l2)
	  l12 = l2;
      } else if (l12<lhs_eps[i].l) {
	return CLPFD_MAXINT;
      } else {
	removed = 1;
      }
    }
  }
  if (!removed)
    return CLPFD_MAXINT;
  ndisj = i;
  for (; i<ntask_eps; i++)
    small += lhs_eps[i].l*lhs_eps[i].h;
  for (i=0; i<ndisj; i++) {
    int inc = (int)lhs_eps[i].s;
    count += inc;
    if (!inc || count<=2) {
      if (lhs_eps[i].l*(eps-lhs_eps[i].h)>=small)
	return len + small/(eps-lhs_eps[i].h);
      small -= lhs_eps[i].l*(eps-lhs_eps[i].h);
      len += lhs_eps[i].l;
    }
  }
  return len + small/eps;  
}

static SP_integer
lh_upperbound3(SP_integer eps,
	       SP_integer sigma,
	       int ntask_eps,
	       struct lhs *lhs_eps)
{
  SP_integer lmax = CLPFD_MAXINT;
  SP_integer maxh = 0;
  SP_integer delta, len, bound;
  int i;

  for (i=0; i<ntask_eps; i++) {
    if (maxh < lhs_eps[i].h)
      maxh = lhs_eps[i].h;
  }
  for (delta=2; delta<=maxh; delta++) {
    SP_integer p = eps/delta;
    SP_integer eps_p = eps % delta;
    SP_integer n_delta=0, n_1=0;
    for (i=0; i<ntask_eps; i++) {
      n_delta += lhs_eps[i].h/delta*lhs_eps[i].l;
      n_1     += lhs_eps[i].h%delta*lhs_eps[i].l;
    }
    len = n_delta/p;
    if (eps_p==0) {
      bound = len + (n_delta%p*delta + n_1 + sigma)/eps;
    } else if (n_1+sigma<=eps_p*len) {
      bound = (n_1+sigma)/eps_p;
    } else {
      bound = len + (n_delta%p*delta + n_1 + sigma - eps_p*len)/eps;
    }
    if (lmax > bound)
      lmax = bound;
  }
  return lmax;
}

static SP_integer
lh_upperbound4(SP_integer eps,
	       SP_integer sigma,
	       int ntask_eps,
	       struct lhs *lhs_eps,
	       SP_integer *lh_table)
{
  int i, j, ndisj;
  SP_integer small=sigma, ldisj=0, sdisj=0, tdisj=0, li;

  for (ndisj=1; ndisj<ntask_eps; ndisj++) {
    SP_integer h = lhs_eps[ndisj-1].h + lhs_eps[ndisj].h;
    if (h<=eps)
      break;
  }
  /* now carefully examine some more tasks wrt. lh_table */
  for (; ndisj<ntask_eps; ndisj++) {
    for (j=0; j<ndisj; j++) {
      SP_integer h = lhs_eps[j].h + lhs_eps[ndisj].h;
      if (h==eps || (h<eps && lh_table[eps-h-1]>0))
	goto nodisj;
    }
  }
nodisj:
  j = ntask_eps-1;
  for (i=0; i<ndisj; i++) {
    for (; j>=ndisj && lhs_eps[j].h<=eps-lhs_eps[i].h; j--) {
      small += lhs_eps[j].l*lhs_eps[j].h;
    }
    if (sdisj + lhs_eps[i].l*lhs_eps[i].h + small < (ldisj + lhs_eps[i].l)*eps)
      li = (small-tdisj)/(eps-lhs_eps[i].h);
    else
      li = lhs_eps[i].l;
    sdisj += li*lhs_eps[i].h;
    ldisj += li;
    tdisj = eps*ldisj-sdisj;
  }
  for (; j>=ndisj; j--)
    small += lhs_eps[j].l*lhs_eps[j].h;
  tdisj = eps*ldisj-sdisj;
  return ldisj + (small-tdisj)/eps; 
}

static SP_integer
lh_upperbound5(SP_integer eps,
	       SP_integer sigma,
	       int ntask_eps,
	       struct lhs *lhs_eps)
{
  SP_integer delta=eps/3, n1=0, len3d=0, len2d=0, len1d=0, eps_p, len;
  int t;
  if (delta <= 1 || 4*delta<=eps)
    return CLPFD_MAXINT;
  eps_p = eps%delta;
  for (t=0; t<ntask_eps; t++) {
    SP_integer l = lhs_eps[t].l;
    int s = (int)(lhs_eps[t].h/delta);
    if (s==3)
      len3d += l;
    else if (s==2)
      len2d += l;
    else if (s==1)
      len1d += l;
    n1 += lhs_eps[t].h % delta * l;
  }
  if (len1d <= len2d)
    len = len3d + len1d;
  else
    len = len3d + len2d + (len1d-len2d)/3;
  if (eps_p==0) {
    if (len1d <= len2d) {
      if (n1+sigma <= (len2d-len1d)*delta) {
	return len+(n1+sigma)/delta;
      } else {
	return len+(n1+sigma+(len2d-len1d)*2*delta)/eps;
      }
    } else {
      return len+(n1+sigma+(len1d-len2d)%3*delta)/eps;
    }
  } else {
    if (n1+sigma <= eps_p*len) {
      return (n1+sigma)/eps_p;
    } else {
      if (len1d <= len2d) {
	if (n1+sigma-len*eps_p <= (len2d-len1d)*(delta+eps_p)) {
	  return len+(n1+sigma-len*eps_p)/(delta+eps_p);
	} else {
	  return len+(n1+sigma-len*eps_p+(len2d-len1d)*2*delta)/eps;
	}
      } else {
	return len+(n1+sigma-len*eps_p+(len1d-len2d)%3*delta)/eps;
      }
    }
  }
}

struct clh {
  int m;			/* number of tasks for which the weight is < eps */
  int top;
  SP_integer eps;
  SP_integer est;			/* innitial earliest start */
  SP_integer umax;			/* upper bound */
  SP_integer sum_len_eps;		/* total length of tasks of height = eps if closed, 0 if open */
  SP_integer sum_surf;		/* total area   of tasks of height < eps if closed, =< eps if open */
  int nchoice;
  SP_integer *L;			/* length of tasks for which height is < eps */
  SP_integer *H;			/* height of tasks for which height is < eps */
  SP_integer *U;			/* unscheduled flag */
  SP_integer *E;			/* U[t] ? earliest start : effective start */
#if LH_USE_PROFILE
  struct profile *P;
  struct profile *PP;
#else
  SP_integer *P;			/* cumulated resource consumption profile */
  SP_integer *PP;			/* temp. copy of clh->P */
#endif
  SP_integer *S;			/* tasks for which a choicepoint was created */
  SP_integer *B;			/* branch on which we are */
  SP_integer *F;			/* saved earliest start */
};

static SP_integer
clh_init(Wam wam,
	       SP_integer eps,
	       SP_integer sigma,
	       int ntask,
	       struct lhs *lhs,
	       SP_integer *lh_table,
	       SP_integer *lch_table,
	       SP_integer row,
	       SP_integer bbheight,
	       int closed,
	       struct clh *clh)
{
  int i, m=0;
  SP_integer offset=row*bbheight;
  SP_integer cmax, holemax;

  clh->est = 0;
  clh->L = Malloc(4*ntask,SP_integer);
  clh->H = clh->L + ntask;
  clh->U = clh->H + ntask;
  clh->E = clh->U + ntask;
  clh->sum_len_eps = 0;
  clh->sum_surf = 0;
  for (i=0; i<ntask; i++) {
    if (closed && lhs[i].h==eps) {
      clh->sum_len_eps += lhs[i].l;
    } else if (lhs[i].h==eps || (lhs[i].h<=eps && lh_table[offset+eps-lhs[i].h-1]>0)) {
      clh->L[m] = lhs[i].l;
      clh->H[m] = lhs[i].h;
      clh->U[m] = TRUE;
      clh->E[m] = closed ? 0 : 1-clh->L[m];
      clh->est = clh->est < clh->E[m] ? clh->est : clh->E[m];
      clh->sum_surf += clh->L[m]*clh->H[m];
      m++;
    }
  }
  clh->m = m;
  clh->top = 0;
  clh->eps = eps;
  cmax = sigma/eps;
  if (closed) {
    clh->umax = lch_table[offset+eps-1] - clh->sum_len_eps;
    if (row>0)
      cmax = cmax > lch_table[offset-bbheight+eps-1] - clh->sum_len_eps ? cmax : lch_table[offset-bbheight+eps-1] - clh->sum_len_eps;
  } else {
    clh->umax = lh_table[offset+eps-1] - clh->sum_len_eps;
    cmax = cmax > lch_table[offset+eps-1] - clh->sum_len_eps ? cmax : lch_table[offset+eps-1] - clh->sum_len_eps;
    if (row>0)
      cmax = cmax > lh_table[offset-bbheight+eps-1] - clh->sum_len_eps ? cmax : lh_table[offset-bbheight+eps-1] - clh->sum_len_eps;
  }
  if (cmax>=clh->umax) {	/* cmax > clh->umax is possible since table values are clipped to bblength */
    clh->P = NULL;
    clh->S = NULL;
    return clh->umax;		/* table value is known to be sharp */
  }
  SP_ASSERT(clh->umax <= (sigma+clh->sum_surf)/eps);
  holemax = clh->umax-clh->est;
#if LH_USE_PROFILE
  clh->P = fd_empty_profile();
#else
 {
   SP_integer *hole = Malloc(2*holemax,SP_integer);
   clh->P = hole - clh->est;
   clh->PP = clh->P + holemax;
   for (i=clh->est; i<clh->umax; i++)
     clh->P[i] = 0;
 }
#endif
  clh->nchoice = (int)(m * holemax);
  clh->S = Malloc(3*clh->nchoice,SP_integer);
  clh->B = clh->S + clh->nchoice;
  clh->F = clh->B + clh->nchoice;
  return cmax < clh->umax ? cmax : clh->umax;
}

static SP_integer
clh_get_est_unscheduled(struct clh *clh)
{
  SP_integer delta = clh->umax;
  int t;

  for (t=0; t<clh->m; t++) {
    if (clh->U[t] && delta > clh->E[t])
      delta = clh->E[t];
  }
  return (delta < clh->umax ? delta : clh->umax);
}

static SP_BOOL
clh_open_dominated(struct clh *clh)
{
  if (clh_get_p(-1)==clh->eps) {
    return TRUE;		/* open case: non-essential: increase all starts by 1 and you get a longer hole */
#if 0 /* clashes with swap symmetry */
  } else if (!clh->U[0] && 2*clh->E[0]+clh->L[0]>clh->umax) {
    return TRUE;		/* open case: non-essential: task 0 is too delayed */
#endif
  } else {
    return FALSE;
  }
}

static SP_BOOL
clh_dominated(Wam wam,
		    SP_integer delta,
		    struct clh *clh)
{
  (void)wam;
  if (clh->est<0 && clh_open_dominated(clh)) {
    return TRUE;
  } else if (delta > 0) {
    int last_unscheduled = -1;
    int t;

    for (t=clh->m-1; t>=0; t--) {
      if (clh->U[t]) {
	if (last_unscheduled==-1 ||
	    clh->L[t] < clh->L[last_unscheduled] ||
	    clh->H[t] < clh->H[last_unscheduled] ) {
	  SP_integer height = clh->H[t];
	  SP_integer length = clh->L[t];
	  SP_integer rest_length = length;
	  int i;
	  
	  if (last_unscheduled==-1)
	    last_unscheduled = t;
	  for (i=(int)delta-1; i-rest_length >= -1; i--) {
	    SP_ASSERT(delta >= 1 && 0 <= i && i <= delta-1 && i < clh->umax);
	    if (clh_get_p(i)+height <= clh->eps) {
	      rest_length--;
	      if (rest_length==0)
		return TRUE;
	      else
		rest_length = length;
	    }
	  }
	}
      }
    }
  }
  return FALSE;
}

/***
Nicolas's idea for a better heuristic:
(1) If possible, select a task of height h = the gap on top of delta.
(2) Otherwise, select a task of weight h that maximizes lmax[sigma_delta,gap-h]
***/
static int
clh_select_task(Wam wam,
		      SP_integer delta,
		      struct clh *clh)
{
  int t;
  (void)wam;
  
  for (t=0; t<clh->m; t++) {
    SP_ASSERT(clh->E[t]==delta || (clh->U[t] ^ (clh->E[t]<delta)));
    if (clh->U[t] && clh->E[t]==delta) {
      return t;
    }
  }  
  SP_ASSERT(FALSE);
  return -1;
}

static SP_BOOL
clh_swap_dominated(SP_integer startt,
		   int t,
		   struct clh *clh)
{
  SP_integer ht = clh->H[t];
  SP_integer lt = clh->L[t];
  int u;
  
  for (u=t+1; u<clh->m; u++) {
    SP_integer startu = clh->E[u];
    SP_integer hu = clh->H[u];
    SP_integer lu = clh->L[u];
    if (!clh->U[u] && startu+lt<=startt && lu<=lt) {
      /* intervals where the profile decreases after swap are already checked */

#if LH_USE_PROFILE
      if (fd_profile_max(clh->P,startu,startu+lu)-hu+ht>clh->eps)
	goto nextu;
      if (fd_profile_max(clh->P,startu+lu,startu+lt)+ht>clh->eps)
	goto nextu;
#else
      {
	SP_integer cur;
	/* check an interval intersecting only $u$ before swap and only $t$ after swap */
	for (cur=startu; cur<startu+lu; cur++)
	  if (clh_get_p(cur)-hu+ht>clh->eps)
	    goto nextu;
	/* check an interval intersecting neither before swap and only $t$ after swap */
	for (; cur<startu+lt; cur++)
	  if (clh_get_p(cur)+ht>clh->eps)
	    goto nextu;
      }
#endif

      return TRUE;
    }
  nextu: ;
  }
  return FALSE;
}

static SP_BOOL
clh_overflow(SP_integer delta,
	     int t,
	     struct clh *clh)
{
  if (clh_get_p(delta)+clh->H[t] > clh->eps) {
    return TRUE;
  } else if (delta>0 && clh_get_p(delta-1)+clh->H[t]<=clh->eps) {
    return TRUE;
  } else if (delta<0 && clh_get_p(-1)+clh->H[t] == clh->eps) {
    return TRUE;		/* open case: non-essential: prevent full P[-1] */
  } else if (clh_swap_dominated(delta,t,clh)) {
    return TRUE;
  } else {
    return FALSE;
  }
}

static void
clh_delay_task(Wam wam,
		     int t,
		     struct clh *clh)
{
  int top = clh->top;
  SP_integer start  = clh->E[t];
  SP_integer height = clh->H[t];
  int i;
  (void)wam;

  SP_ASSERT(clh->U[t] && top<clh->nchoice);
    
  clh->S[top] = t;
  clh->B[top] = 2;
  clh->F[top] = start;
  clh->top++;
  for (i=(int)start+1; i<clh->umax; i++) {
    if (clh_get_p(i)+height <= clh->eps) /* TODO: profile op */
      break;
  }
  if (clh->est<0) {
    if (i<0 && clh_get_p(-1)+height == clh->eps)
      i = 0;			/* open case: non-essential: prevent full P[-1] */
#if 0 /* clashes with swap symmetry */
    else if (t==0 && 2*i+clh->L[0]>clh->umax)
      i = clh->umax;		/* open case: non-essential: prevent task 0 from being too delayed */
#endif
  }
  clh->E[t] = i;
}

static void
clh_push(Wam wam, int t, struct clh *clh)
{
  int top = clh->top;
  SP_integer delta = clh->E[t];
  SP_integer height = clh->H[t];
  SP_integer length = clh->L[t];
  SP_integer last = delta+length < clh->umax ? delta+length : clh->umax;
  
#if LH_USE_PROFILE
  clh->P = fd_profile_update(wam, clh->P,delta,last,height);
#else
  {
    int i;
    for (i=delta; i<last; i++)
      clh_get_p(i) += height;
  }
#endif
  SP_ASSERT(clh->U[t] && top<clh->nchoice);
  clh->U[t] = 0;
  clh->S[top] = t;
  clh->B[top] = 1;
  clh->F[top] = clh->E[t];
  clh->top++;
}

static void
clh_slack_overflow(SP_integer delta,
		   SP_integer sigma,
		   struct clh *clh,
		   SP_integer *sigma_delta,
		   SP_integer *sumh_delta)
{
  SP_integer loss=0;
  int i;
  
  if (delta < clh->umax) {
    for (i=0; i<delta && loss<=sigma; i++) {
/*       SP_ASSERT(clh_get_p(i)<=clh->eps); */
      loss += clh->eps-clh_get_p(i); /* TODO: profile op */
    }
    if (delta < 0) {
      *sigma_delta = sigma;
      *sumh_delta = clh->eps;
    } else if (loss > sigma) {
      *sigma_delta = -1;
      *sumh_delta = 0;
    } else {
      SP_integer sd = 0;
      int t;
      SP_integer limit = clh->eps-clh_get_p(delta);

      for (t=0; t<clh->m; t++)
	if (clh->U[t] && clh->E[t]==delta && clh->H[t]<=limit)
	  sd += clh->H[t];
    
      *sigma_delta = sigma-loss;
      *sumh_delta = sd;
    }
  }
}

static SP_integer
clh_delta_slack(SP_integer delta,
		SP_integer sigma_delta,
		struct clh *clh)
{
  SP_integer loss = 0;
  int i = delta > 0 ? (int)delta : 0;
  int j = 0;

  if (clh->est==0) {
    for (; i<clh->umax; i++) {
      loss += clh->eps-clh_get_p(i); /* TODO: profile op */
      if (loss>sigma_delta)
	break;
    }
    return i;
  } else {			/* open case: non-essential: count columns before 0 too */
    while (loss<=sigma_delta && (i<clh->umax || j>clh->est)) {
      SP_integer rloss = i<clh->umax ? clh->eps-clh_get_p(i) : CLPFD_MAXINT2; /* TODO: profile op */
      SP_integer lloss = j>clh->est ? clh->eps-clh_get_p(j-1) : CLPFD_MAXINT2; /* TODO: profile op */
      if (rloss<lloss) {
	loss += rloss;
	i++;
      } else {
	loss += lloss;
	j--;
      }
    }
    return i-j-(loss>sigma_delta);
  }
}

static int
clh_poly_case_root(struct clh *clh)
{
  SP_integer sumh_non_disj = 0;
  int last_disj = 0;
  int t;
  
  for (t=1; t<clh->m; t++) {
    int disj_task = TRUE;
    if (clh->H[t-1]+clh->H[t] <= clh->eps) {
      disj_task = FALSE;
      sumh_non_disj += clh->H[t];
    }
    if (clh->H[0]+sumh_non_disj > clh->eps)
      return -2;		/* failure */
    if (disj_task)
      last_disj = t;
  }
  return last_disj;	      	/* success */
}

static int
clh_poly_case(SP_integer delta,
	      struct clh *clh)
{
  int first_task = TRUE;
  int disj_task = TRUE;
  int last_disj = -1;
  SP_integer hmax_disj = 0;
  SP_integer sumh_non_disj = 0;
  SP_integer suml_disj = 0;
  SP_integer h_delta = clh_get_p(delta);
  SP_integer hprev = 0;
  int t;
  
  if (delta<0)
    return -2;
  
  for (t=0; t<clh->m; t++) {
    if (clh->U[t]) {
      SP_integer p = 0;
      if (delta+suml_disj+clh->L[t]-1 < clh->umax)
	p = clh_get_p(delta+suml_disj+clh->L[t]-1);
      if (first_task) {
	first_task = FALSE;
	hmax_disj = clh->H[t];
      } else if (!disj_task || p+hprev+clh->H[t] <= clh->eps) {
	disj_task = FALSE;
	sumh_non_disj += clh->H[t];
      }
      if (hmax_disj+sumh_non_disj+h_delta > clh->eps)
	return -2;		/* failure */
      if (disj_task) {
	suml_disj += clh->L[t];
	last_disj = t;
      }
      hprev = clh->H[t];
    }
  }
  return last_disj;	      	/* success */
}

static SP_integer
clh_poly_bound(Wam wam,
	       int last_disj,
	       SP_integer delta,
	       SP_integer sigma_delta,
	       struct clh *clh)
{
  int t, i, j=(int)delta;

#if LH_USE_PROFILE
  clh->PP = fd_profile_copy(wam, clh->P);
#else
  for (t=delta; t<clh->umax; t++)
    clh->PP[t] = clh_get_p(t);
#endif
  for (t=0; t<clh->m; t++) {
    if (clh->U[t]) {
      if (t<=last_disj) {
	SP_integer lent = clh->L[t] < clh->umax-j ? clh->L[t] : clh->umax-j ;
#if LH_USE_PROFILE
	clh->P = fd_profile_update(wam, clh->P,j,j+lent,clh->H[t]);
#else
	for (i=j; i<j+lent; i++) {
	  clh_get_p(i) += clh->H[t];
/* 	  SP_ASSERT(clh_get_p(i) <= clh->eps); */
	}
#endif
	j += (int)lent;
      } else {
	SP_integer lent = clh->L[t] < clh->umax-delta ? clh->L[t] : clh->umax-delta ;
#if LH_USE_PROFILE
	clh->P = fd_profile_update(wam, clh->P,delta,delta+lent,clh->H[t]);
#else
	for (i=delta; i<delta+lent; i++) {
	  clh_get_p(i) += clh->H[t];
/* 	  SP_ASSERT(clh_get_p(i) <= clh->eps); */
	}
#endif
      }
    }
  }
  i = (int)clh_delta_slack(delta, sigma_delta, clh);
#if LH_USE_PROFILE
  fd_profile_dispose(wam, clh->P);
  clh->P = clh->PP;
#else
  for (t=delta; t<clh->umax; t++)
    clh_get_p(t) = clh->PP[t];
#endif
  return i;
}

static SP_BOOL
clh_not_too_big_loss1(SP_integer delta,
		      SP_integer sigma_delta,
		      SP_integer cmax,
		      struct clh *clh)
{
  SP_integer emin = CLPFD_MAXINT2;
  int t;

  for (t=0; t<clh->m; t++)
    if (clh->U[t] && clh->E[t]>delta+1 && clh->E[t]<emin)
      emin = clh->E[t];
  if (emin<=cmax) {
    SP_integer maxl = emin-delta;
    SP_integer maxh = clh->eps-clh_get_p(emin-1);
    SP_integer available = clh->eps*maxl;
    SP_integer sum_surf = sigma_delta;
    int i;

    for (i=(int)delta; i<emin; i++) {
      sum_surf += clh_get_p(i); /* TODO: profile op */
      if (sum_surf >= available)
	return TRUE;
    }
    for (t=0; t<clh->m; t++) {
      if (clh->U[t] && clh->E[t]<emin && clh->H[t]<=maxh) {
	SP_integer lent = clh->L[t] < maxl ? clh->L[t] : maxl;
	sum_surf += lent*clh->H[t];
	if (sum_surf >= available)
	  return TRUE;
      }
    }
    return FALSE;    
  } else {
    return TRUE;
  }
}

static SP_BOOL
clh_not_too_big_loss(SP_integer delta,
		     SP_integer sigma_delta,
		     SP_integer cmax,
		     SP_integer *lh_table,
		     SP_integer *lch_table,
		     SP_integer row,
		     SP_integer bbheight,
		     struct clh *clh)
{
  SP_integer h_delta = clh_get_p(delta);
  
  if (delta>=0 && h_delta<clh->eps) {
    SP_integer l, size_delta;
    SP_integer gap_delta = clh->eps-h_delta;
    int i, ix;
    
    SP_BOOL closed_delta = ((clh->est==0 && delta==0) || (clh->est<delta && clh_get_p(delta-1)==clh->eps));

    for (i=(int)delta+1; i<clh->umax && clh_get_p(i)==h_delta; i++) /* TODO: profile op */
      ;
    size_delta = i-delta;
    ix = (int)gap_delta-1;
    if (row>0)
      ix += (int)(sigma_delta*bbheight);
    l = (closed_delta ? lch_table[ix] : lh_table[ix]);
    if (l<size_delta && delta+l<=cmax)
      return FALSE;
    else
      return clh_not_too_big_loss1(delta,sigma_delta,cmax,clh);
  } else {
    return TRUE;
  }
}

static void clh_pop(Wam wam, struct clh *clh)
{
  int top = clh->top;

  while (top>0) {
    int t = (int)clh->S[--top];

    SP_ASSERT(0 <= top && top < clh->nchoice);
    if (clh->B[top]==1) {
      SP_integer height = clh->H[t];
      SP_integer length = clh->L[t];
      SP_integer start  = clh->E[t];
      SP_integer last = start+length < clh->umax ? start+length : clh->umax;
      int i;
    
      SP_ASSERT(!clh->U[t]);
#if LH_USE_PROFILE
      clh->P = fd_profile_update(wam, clh->P,start,last,-height);
#else
      for (i=start; i<last; i++) {
	clh_get_p(i) -= height;
/* 	SP_ASSERT(clh_get_p(i)>=0); */
      }
#endif
      clh->U[t] = TRUE;
      for (i=(int)start+1; i<clh->umax; i++) {
	if (clh_get_p(i)+height <= clh->eps) /* TODO: profile op */
	  break;
      }
      if (clh->est<0) {
	if (i<0 && clh_get_p(-1)+height == clh->eps)
	  i = 0;		/* open case: non-essential: prevent full P[-1] */
#if 0 /* clashes with swap symmetry */
	else if (t==0 && 2*i+clh->L[0]>clh->umax)
	  i = clh->umax; /* open case: non-essential: prevent task 0 from being too delayed */
#endif
      }
      clh->E[t] = i;
      clh->B[top++] = 2;
      break;
    } else {
      SP_ASSERT(clh->U[t]);
      clh->E[t] = clh->F[top];
    }
  }
  clh->top = top;
}

#if 0
static void
clh_dump(SP_integer cmax,
	 struct clh *clh)
{
  int i;
  printf("INCUMBENT length=%" SPRIdINTEGER "\n\n", (SP_integer)cmax);

  for (i=0; i<clh->m; i++)
    if (!clh->U[i])
      printf("length=%" SPRIdINTEGER " height=%" SPRIdINTEGER " origin=%" SPRIdINTEGER "\n",(SP_integer)clh->L[i],(SP_integer)clh->H[i],(SP_integer)clh->E[i]);

  printf("\n\n");
}
#endif

static SP_integer
clh1(Wam wam,
     SP_integer eps,
     SP_integer sigma,
     int *nback,
     int ntask,
     struct lhs *lhs,
     SP_integer *lh_table,
     SP_integer *lch_table,
     SP_integer row,
     SP_integer bbheight,
     int closed,
     int enable)
{
  struct clh struct_clh;
  struct clh *clh = &struct_clh;
  SP_integer cmax = clh_init(wam, eps, sigma, ntask, lhs, lh_table, lch_table, row, bbheight, closed, clh);

  if (cmax<clh->umax) {
    SP_integer last_disj = clh_poly_case_root(clh);
    if (last_disj>=-1) {
      cmax = clh_poly_bound(wam, (int)last_disj, 0, sigma, clh);
    } else {
      while (cmax<clh->umax) {
	SP_integer delta = clh_get_est_unscheduled(clh);
	SP_integer sigma_delta=0, sumh_delta=0, last_disj1;

	clh_slack_overflow(delta,sigma,clh,&sigma_delta,&sumh_delta);	
	if ((delta<0 || clh->est==0 || clh_get_p(-1)>0) &&
	    delta<clh->umax &&
	    sigma_delta>=0 &&
	    eps-clh_get_p(delta)-sumh_delta<=sigma_delta) {
	  int t = clh_select_task(wam, delta,clh); /* select first unscheduled task for which earliest start = delta */
	  if (clh_overflow(delta,t,clh)) {
	    clh_delay_task(wam, t,clh);
	    continue;
	  } else if (!clh_dominated(wam, delta,clh)) {
	    clh_push(wam, t, clh);
	    last_disj1 = !(enable & 0x1) ? -2 : clh_poly_case(delta,clh);
	    if (last_disj1>=-1) {
	      SP_integer cmax1 = clh_poly_bound(wam, (int)last_disj1, delta, sigma_delta, clh);
	      if (cmax<cmax1) {
		cmax = cmax1;	/* new incumbent solution, then backtrack */
		/* clh_dump(cmax1,clh); */
	      }
	    } else if (!(enable & 0x2) || clh_not_too_big_loss(delta, sigma_delta, cmax, lh_table, lch_table, row, bbheight, clh)) {
	      SP_integer cmax1 = clh_delta_slack(delta,sigma_delta,clh);
	      if (cmax<cmax1) {
		cmax = cmax1;	/* new incumbent solution, then proceed down */
		/* clh_dump(cmax1,clh); */
	      }
	      continue;
	    }
	  }
	}
	if (cmax<clh->umax) {
	  if (--(*nback)<0) {
	    cmax = clh->umax;	/* exhausted available backtracks: return upper bound */
	    break;
	  } else {
	    clh_pop(wam, clh);
	    if (clh->top==0)	/* exhausted search space: exit */
	      break;
	  }
	}
      }
    }
  }
  Free(clh->L);
#if LH_USE_PROFILE
  fd_profile_dispose(wam, clh->P);
#else
  if (clh->P)
    Free(clh->P + clh->est);
#endif
  if (clh->S)
    Free(clh->S);
  return (cmax < clh->umax ? cmax : clh->umax) + clh->sum_len_eps;
}

