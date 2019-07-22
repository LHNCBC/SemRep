/* Copyright(C) 1999, Swedish Institute of Computer Science */
/*** 
@inproceedings{shaw2004constraint,
  title={A constraint for bin packing},
  author={Shaw, Paul},
  booktitle={CP 2004},
  editor    = {Mark Wallace},
  series    = {Lecture Notes in Computer Science},
  volume    = {3258},
  pages={648--662},
  year={2004},
  publisher={Springer}
}
***/

#include "fd.h"
#include "dvars.h"

#define BVAR(V) (pdata->dvar+(V))
#define LVAR(V) (pdata->dvar+nitems+(V))
#define SIZETAB(J,I) sizetab[ncols*(J)+(I)]

struct bin_packing_data {
  void (SPCDECL *destructor)(void *);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */
  SP_globref refbase;		/* static */
  int nitems;
  int nbins;
  int nfixed;
  int *prev;			/* [nitems+1] */
  int *next;			/* [nitems+1] */
  int *trail;			/* [nitems] */
  int *refreshed;		/* [nitems] */
  SP_integer *size;		/* [nitems] */
  SP_integer *possible;		/* [nbins] */
  SP_integer *required;		/* [nbins] */
  TAGGED *bin2label;		/* [nbins] */
  Dvar dvar;			/* [nitems+nbins] */
};

static void SPCDECL bin_packing_destructor(void *pdata_v)
{
  struct bin_packing_data *pdata = (struct bin_packing_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  if (pdata->refbase)
    SP_free_globrefs(pdata->refbase,pdata->nitems+pdata->nbins);
  SP_free(pdata);
}


/* for qsorting by descending SP_integer */
static int cmp_desc_long(Wam wam, SP_integer *l1, SP_integer *l2)
{
  SP_integer val1 = *l1;
  SP_integer val2 = *l2;

  (void)wam;
  return -CMP(val1,val2);
}

#define QType SP_integer
#define QCmp  cmp_desc_long
#define QSort fd_qsort_desc_long
#include "qsort.ic"

#define GETARR(IX) arr[(IX)<butindex ? (IX) : (IX)+1]

static void
bp_nosum(SP_integer *arr,
	 int butindex,
	 SP_integer a,
	 SP_integer b,
	 SP_integer *ap,
	 SP_integer *bp)
{
  int n=0, k=0, kp=0;
  SP_integer sum=0, sa=0, sb=0, sc=0;

  *ap = 1;
  *bp = 0;
  while (GETARR(n)>0) {
    sum += GETARR(n);
    n++;
  }
  n--;

  if (a <= 0 || b >= sum)
    return;
  while (sc + GETARR(n-kp) < a) {
    sc += GETARR(n-kp);
    kp++;
  }
  sb = GETARR(n-kp);
  while (sa < a && sb <= b) {
    sa += GETARR(k);
    k++;
    if (sa < a) {
      kp--;
      sb += GETARR(n-kp);
      sc -= GETARR(n-kp);
      while (sa + sc >= a) {
	kp--;
	sc -= GETARR(n-kp);
	sb += GETARR(n-kp) - GETARR(n-kp-k-1);
      }
    }
  }
  if (sa < a) {
    *ap = sa + sc;
    *bp = sb;
  }
}

static void
fix_item(struct bin_packing_data *pdata, int i, int j)
{
  int p = pdata->prev[i];
  int n = pdata->next[i];

  pdata->next[p] = n;
  pdata->prev[n] = p;
  pdata->trail[pdata->nfixed++] = i;
  pdata->required[j] += pdata->size[i];
}

static void
unfix_item(struct bin_packing_data *pdata, int i, int j)
{
  int p = pdata->prev[i];
  int n = pdata->next[i];

  pdata->next[p] = i;
  pdata->prev[n] = i;
  pdata->required[j] -= pdata->size[i];
}

static int
label2bin(struct bin_packing_data *pdata, TAGGED key)
{
  int inf = 0;
  int sup = pdata->nbins;
  while (inf<sup) {
    int mid = (inf+sup)>>1;
    if (Tlt(pdata->bin2label[mid],key))
      inf = mid+1;
    else
      sup = mid;
  }
  return inf;
}

/*
  '$fd_bin_packing'(+State0, +State, -Actions).
  state(Bi vars,Si ints,Jj ints,Lj vars,Handle,Stamp)
*/
void SPCDECL
prolog_fd_bin_packing(Wam wam,
		      SP_term_ref State0,
		      SP_term_ref State,
		      SP_term_ref Actions)
{
  int ent = -1;			/* initially disentailed */
  int nitems, nbins, ncols, nrefreshed=0;
  TAGGED handle, t1, t2, inset, outset;
  struct bin_packing_data *pdata;
  SP_integer *sizetab = NULL;
  int *butindex = NULL;
  SP_BOOL committed, ground = TRUE;
  SP_integer a=0, b=0, c=0, f2=0, f3=0, q=0, interval, stamp;
  int i, j, k, rc, ss=0, n1=0, n2=0, n3=0;

  DVITER it;
  FDCONS incons;
  FDCONS outcons;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam,&handle,&stamp,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct bin_packing_data,handle);
    nitems = pdata->nitems;
    nbins = pdata->nbins;
  } else {			/* build persistent state */
    SP_integer extra_size;
    char *ptr;

    DerefArg(t1,X(0),1);
    nitems = fd_list_length(t1);
    DerefArg(t1,X(0),3);	/* get state(...) */
    nbins = fd_list_length(t1);
    extra_size = nitems*sizeof(SP_integer) + /* size */
      nbins*sizeof(SP_integer) + /* possible */
      nbins*sizeof(SP_integer) + /* required */
      nbins*sizeof(TAGGED) +	 /* bin2label */
      (nitems+1)*sizeof(int) +	 /* prev */
      (nitems+1)*sizeof(int) +	 /* next */
      (nitems)*sizeof(int) +	 /* trail */
      (nitems)*sizeof(int) +	 /* refreshed */
      (nitems+nbins)*sizeof(struct dvar); /* dvar */
    pdata = Palloc(struct bin_packing_data, extra_size, handle);
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += (nitems+nbins)*sizeof(struct dvar);
    pdata->size = (SP_integer *)ptr;
    ptr += nitems * sizeof(SP_integer);
    pdata->possible = (SP_integer *)ptr;
    ptr += nbins * sizeof(SP_integer);
    pdata->required = (SP_integer *)ptr;
    ptr += nbins * sizeof(SP_integer);
    pdata->bin2label = (TAGGED *)ptr;
    ptr += nbins * sizeof(TAGGED);
    pdata->prev = (int *)ptr;
    ptr += (nitems+1)*sizeof(int);
    pdata->next = (int *)ptr;
    ptr += (nitems+1)*sizeof(int);
    pdata->trail = (int *)ptr;
    ptr += (nitems)*sizeof(int);
    pdata->refreshed = (int *)ptr;
    ptr += (nitems)*sizeof(int);
    SP_ASSERT(ptr==(char *)(pdata+1)+extra_size);
    pdata->destructor = bin_packing_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->nitems = nitems;
    pdata->nbins = nbins;
    pdata->nfixed = 0;
    pdata->refbase = SP_alloc_globrefs(2*(nitems+nbins));
				/* internalize all variables */
    DerefArg(t1,X(0),1);	/* bin variables */
    DerefArg(t2,X(0),2);	/* weights */
    for (i=0, k=0; i<nitems; i++, k+=2) {
      TAGGED x, y;
      SP_globref refoffset = pdata->refbase + k;
      DerefCar(x,t1);
      DerefCdr(t1,t1);
      DerefCar(y,t2);
      DerefCdr(t2,t2);
      fd_get_var_and_attr(x,refoffset);
      pdata->size[i] = GetSmall(y);
      dvar_init(BVAR(i), refoffset, refoffset+1);
    }
    DerefArg(t1,X(0),3);	/* bin labels */
    DerefArg(t2,X(0),4);	/* load variables */
    for (j=0; j<nbins; j++, k+=2) {
      TAGGED x, y;
      SP_globref refoffset = pdata->refbase + k;
      DerefCar(x,t1);
      DerefCdr(t1,t1);
      DerefCar(y,t2);
      DerefCdr(t2,t2);
      pdata->bin2label[j] = x;
      fd_get_var_and_attr(y,refoffset);
      dvar_init(LVAR(j), refoffset, refoffset+1);
    }
				/* init required[], which is being maintained */
    for (j=0; j<nbins; j++)
      pdata->required[j] = 0;
				/* init doubly linked list */
    for (i=1; i<nitems; i++) {
      pdata->prev[i] = i-1;
      pdata->next[i] = i+1;
    }
    pdata->prev[0] = nitems;
    pdata->next[0] = 1;
    pdata->prev[nitems] = nitems-1;
    pdata->next[nitems] = 0;
  }

  /* RESUME HERE */

  k = pdata->nfixed;
  while (k > stamp) {
    i = pdata->trail[--k];
    unfix_item(pdata, i, label2bin(pdata, dvar_min_t(BVAR(i))));
  }
  pdata->nfixed = k;

  for (i=pdata->next[nitems]; i<nitems; i = pdata->next[i]) {
    pdata->refreshed[nrefreshed++] = i;
    dvar_refresh(BVAR(i));
  }
  for (j=0; j<nbins; j++) {
    dvar_refresh(LVAR(j));
  }

  for (i=pdata->next[nitems]; i<nitems; i = pdata->next[i])
    if (!dvar_is_integer(BVAR(i)))
      ground = FALSE;
    else
      fix_item(pdata, i, label2bin(pdata, dvar_min_t(BVAR(i))));

  /* BASIC RULES */

  /* from now on, we will consider unfixed items only */
  /* build possible and required sums */
  
  for (j=0; j<nbins; j++) {
    pdata->possible[j] = pdata->required[j];
    a = b = b + pdata->required[j];
  }
  for (i=pdata->next[nitems]; i<nitems; i = pdata->next[i]) {
    a = b = b + pdata->size[i];
    dviter_init(&it, BVAR(i));
    while (!dviter_empty(&it)) {
      j = label2bin(pdata, dviter_next_value_t(&it));
      pdata->possible[j] += pdata->size[i];
    }
  }
  for (j=0; j<nbins; j++) {
    if (dvar_fix_interval_l(LVAR(j), pdata->required[j], pdata->possible[j])<0)
      goto ret;
    a -= dvar_max_l(LVAR(j));
    b -= dvar_min_l(LVAR(j));
  }
  do {
    interval = b-a;
    for (j=0; j<nbins; j++) {
      SP_integer oldmin = dvar_min_l(LVAR(j));
      SP_integer oldmax = dvar_max_l(LVAR(j));
      
      if (dvar_fix_interval_l(LVAR(j), a+oldmax, b+oldmin)<0)
	goto ret;
      a += oldmax - dvar_max_l(LVAR(j));
      b -= dvar_min_l(LVAR(j)) - oldmin;
    }
  } while (interval != b-a);
  for (i=pdata->next[nitems]; i<nitems; i = pdata->next[i]) {
    dviter_init(&it, BVAR(i));
    fdcons_init(&outcons);
    fdcons_init(&incons);
    while (!dviter_empty(&it)) {
      j = label2bin(pdata, dviter_next_value_t(&it));
      if (pdata->size[i] + pdata->required[j] > dvar_max_l(LVAR(j)))
	fdcons_add(wam, &outcons, pdata->bin2label[j]);
      if (pdata->possible[j] - pdata->size[i] < dvar_min_l(LVAR(j)))
	fdcons_add(wam, &incons, pdata->bin2label[j]); 
    }
    outset = fdcons_set(&outcons);
    inset = fdcons_set(&incons);
    if (fdcons_size(&incons)==0) {
      rc = dvar_prune_set(BVAR(i), outset);
    } else if (fdcons_size(&incons)==1 && !fd_intersection_min(inset,outset)) {
      rc = dvar_fix_set(BVAR(i), inset);
    } else {
      goto ret;
    }
    if (rc<0)
      goto ret;
    if (rc>0 && dvar_is_integer(BVAR(i)))
      fix_item(pdata, i, label2bin(pdata, dvar_min_t(BVAR(i))));
  }
  
  /* KNAPSACK RULES */

  for (i=pdata->next[nitems]; i<nitems; i = pdata->next[i]) {
    if (dvar_is_pruned(BVAR(i)))
      goto suspend;
  }
  for (j=0; j<nbins; j++) {
    if (dvar_is_pruned(LVAR(j)))
      goto suspend;
  }
  butindex = Malloc(nbins,int);
  ncols = nitems - pdata->nfixed + 1;
  sizetab = Malloc((ncols)*nbins,SP_integer);
  memset(&SIZETAB(0,0), 0, (ncols)*nbins*sizeof(SP_integer));
  for (j=0; j<nbins; j++) {
    k = 0;
    for (i=pdata->next[nitems]; i<nitems; i = pdata->next[i])
      if (dvar_contains_value_t(BVAR(i), pdata->bin2label[j]))
	SIZETAB(j,k++) = pdata->size[i];
    butindex[j] = -1;
    bp_nosum(&SIZETAB(j,0), ncols, dvar_min_l(LVAR(j)) - pdata->required[j], dvar_max_l(LVAR(j)) - pdata->required[j], &a, &b);
    if (a<b)
      goto ret;
    bp_nosum(&SIZETAB(j,0), ncols, dvar_min_l(LVAR(j)) - pdata->required[j], dvar_min_l(LVAR(j)) - pdata->required[j], &a, &b);
    if (a<b && dvar_fix_min_l(LVAR(j), b + pdata->required[j])<0)
      goto ret;
    bp_nosum(&SIZETAB(j,0), ncols, dvar_max_l(LVAR(j)) - pdata->required[j], dvar_max_l(LVAR(j)) - pdata->required[j], &a, &b);
    if (a<b && dvar_fix_max_l(LVAR(j), a + pdata->required[j])<0)
      goto ret;
  }
  for (i=pdata->next[nitems]; i<nitems; i = pdata->next[i]) {
    fdcons_init(&outcons);
    fdcons_init(&incons);
    dviter_init(&it, BVAR(i));
    while (!dviter_empty(&it)) {
      j = label2bin(pdata, dviter_next_value_t(&it));
      butindex[j]++;
      bp_nosum(&SIZETAB(j,0), butindex[j],
	       dvar_min_l(LVAR(j)) - pdata->required[j] - pdata->size[i],
	       dvar_max_l(LVAR(j)) - pdata->required[j] - pdata->size[i],
	       &a, &b);
      if (a<b)
	fdcons_add(wam, &outcons, pdata->bin2label[j]);
      bp_nosum(&SIZETAB(j,0), butindex[j],
	       dvar_min_l(LVAR(j)) - pdata->required[j],
	       dvar_max_l(LVAR(j)) - pdata->required[j],
	       &a, &b);
      if (a<b)
	fdcons_add(wam, &incons, pdata->bin2label[j]);
    }
    outset = fdcons_set(&outcons);
    inset = fdcons_set(&incons);
    if (fdcons_size(&incons)==0) {
      rc = dvar_prune_set(BVAR(i), outset);
    } else if (fdcons_size(&incons)==1 && !fd_intersection_min(inset,outset)) {
      rc = dvar_fix_set(BVAR(i), inset);
    } else {
      goto ret;
    }
    if (rc<0)
      goto ret;
    if (rc>0 && dvar_is_integer(BVAR(i)))
      fix_item(pdata, i, label2bin(pdata, dvar_min_t(BVAR(i))));
  }
  
  /* LOWER BOUND RULES */

  for (i=pdata->next[nitems]; i<nitems; i = pdata->next[i])
    sizetab[ss++] = pdata->size[i];
  if (ss==0)
    goto suspend;
  for (j=0; j<nbins; j++) {
    SP_integer cap = dvar_max_l(LVAR(j)) - pdata->required[j];
    if (c < cap)
      c = cap;
  }
  for (j=0; j<nbins; j++) {
    SP_integer cap = dvar_max_l(LVAR(j)) - pdata->required[j];
    if (c > cap)
      sizetab[ss++] = c-cap;
  }
  fd_qsort_desc_long(wam, sizetab, ss);
  for (; n2<ss && sizetab[n2] > c/2; n2++)
    f2 += c - sizetab[n2];
  if (n2 > nbins)
    goto ret;
  for (n3=n2; n3<ss; n3++)
    f3 += sizetab[n3];
  /* the original algorithm would unconditionally iterate q from 0 to c/2 */
  /* instead, I jump to the next q that has any chance to increase the quantity f3 - f2 */
  for (q=0;
       q<=c/2;
       q = n1<ss ? c - sizetab[n1] + 1 : c>q ? c : q+1) {
    for (; n1<ss && sizetab[n1] > c-q; n1++)
      f2 -= c - sizetab[n1];
    for (; n3>n2 && sizetab[n3-1] < q; n3--)
      f3 -= sizetab[n3-1];
    if (f3 > f2 && n2 + (f3 - f2 + c - 1) / c > nbins)
      goto ret;
  }

  /* NO MORE RULES */

 suspend:
  CTagToArg(X(0),6) = MakeSmall(pdata->nfixed);
  ent = ground;
  for (i=0; i<nrefreshed; i++)
    dvar_pruning_done(BVAR(pdata->refreshed[i]));
  for (j=0; j<nbins; j++)
    dvar_pruning_done(LVAR(j));
  for (i=0; i<nrefreshed; i++)
    dvar_export(BVAR(pdata->refreshed[i]));
  for (j=0; j<nbins; j++)
    dvar_export(LVAR(j));
  if (ent==1)
    Pfree;
 ret:
  if (sizetab)
    Free(sizetab);
  if (butindex)
    Free(butindex);
  dvar_export_done(wam,Actions, ent);
}
