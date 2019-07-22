/****************************************************************

 Time-table-edge-finding cumulative [Schutt, Feydy, Stuckey]

The MIT License (MIT)

Copyright (c) 2015 geoffchu

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

FIXME:
It would be more elegant to define:

    pdata->ect[i] = dvar_min_l(ENDVAR(i));
    pdata->lct[i] = dvar_max_l(ENDVAR(i));

but that doesn't work; witness:

| ?- A in 0..1, B in 0..2, C in 1..2, ttef_cumulative([task(A,B,C,1,0),task(0,2,2,1,1)]).
no

 ****************************************************************/

#include "fd.h"
#include "dvars.h"
#define RefLimitAttr     (pdata->refbase)
#define RefLimit         (pdata->refbase + 1)
#define RefOrigAttr(T) (pdata->refbase + (8*(T)) + 2)
#define RefOrig(T)     (pdata->refbase + (8*(T)) + 3)
#define RefDurAttr(T)  (pdata->refbase + (8*(T)) + 4)
#define RefDur(T)      (pdata->refbase + (8*(T)) + 5)
#define RefEndAttr(T)  (pdata->refbase + (8*(T)) + 6)
#define RefEnd(T)      (pdata->refbase + (8*(T)) + 7)
#define RefResAttr(T)  (pdata->refbase + (8*(T)) + 8)
#define RefRes(T)      (pdata->refbase + (8*(T)) + 9)

#define LIMITVAR (&pdata->limitvar)
#define ORIGVAR(T) (&pdata->origvar[T])
#define DURVAR(T)     (&pdata->durvar[T])
#define ENDVAR(T) (&pdata->endvar[T])
#define RESVAR(T)     (&pdata->resvar[T])
#define DUR(T)     (dvar_min_l(&pdata->durvar[T]))
#define MAXDUR(T)     (dvar_max_l(&pdata->durvar[T]))
#define RES(T)     (dvar_min_l(&pdata->resvar[T]))
#define MAXRES(T)  (dvar_max_l(&pdata->resvar[T]))

/* [PM] 4.4.0 undefined before (re-)defining these macros prevents
   conflict with system headers (Windows 10 ucrt stdlib.h defined min
   and max macros). */
#undef min
#undef max
#define min(a,b)	((a)>(b) ? (b) : (a))
#define max(a,b)	((a)>(b) ? (a) : (b))

struct tt {
  SP_integer at;
  SP_integer d;
};

struct ttef_cumulative_data {
  void (SPCDECL *destructor)(void *);
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  SP_globref refbase;		/* static */
  int nrefs;			/* static, 4*nbtasks+2 */
  int nbtasks;
  int nbprofile;
  int nbunfixed;
  SP_integer *est;
  SP_integer *lst;
  SP_integer *ect;
  SP_integer *lct;
  SP_integer *tt_after_est;
  SP_integer *tt_after_lct;
  int *est_array;
  int *lct_array;
  int *target;
  struct tt *profile;
  struct dvar limitvar;
  struct dvar *origvar;
  struct dvar *durvar;
  struct dvar *endvar;
  struct dvar *resvar;
};

static void SPCDECL ttef_cumulative_destructor(void *pdata_v)
{
  struct ttef_cumulative_data *pdata = (struct ttef_cumulative_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

/* Sort by ascending est. */

static int cmp_est(Wam wam, int *i, int *j)
{
  struct ttef_cumulative_data *pdata = fd.gdata;
  
  SP_integer val1 = pdata->est[*i];
  SP_integer val2 = pdata->est[*j];

  (void)wam;
  return CMP(val1,val2);
}

#define QType int
#define QCmp  cmp_est
#define QSort qsort_asc_est
#include "qsort.ic"

/* Sort by ascending lct. */

static int cmp_lct(Wam wam, int *i, int *j)
{
  struct ttef_cumulative_data *pdata = fd.gdata;
  
  SP_integer val1 = pdata->lct[*i];
  SP_integer val2 = pdata->lct[*j];

  (void)wam;
  return CMP(val1,val2);
}

#define QType int
#define QCmp  cmp_lct
#define QSort qsort_asc_lct
#include "qsort.ic"

/* Sort by ascending profile date. */

static int cmp_profile(Wam wam, struct tt *i, struct tt *j)
{
  (void)wam;
  return CMP(i->at,j->at);
}

#define QType struct tt
#define QCmp  cmp_profile
#define QSort qsort_profile
#include "qsort.ic"

static void
init_tt_after_est(struct ttef_cumulative_data *pdata)
{
  int ii = pdata->nbunfixed-1;
  int pix = pdata->nbprofile-2;
  SP_integer energy = 0;	/* total profile of all segments beyond pix */
  SP_integer end = pdata->profile[pix+1].at;
  
  while (ii>=0) {
    int i = pdata->est_array[ii];
    if (pix<0 || end <= pdata->est[i]) {
      pdata->tt_after_est[ii--] = energy;
    } else if (pdata->profile[pix].at <= pdata->est[i]) {
      pdata->tt_after_est[ii--] = energy + pdata->profile[pix].d * (end - pdata->est[i]);
    } else {
      energy += pdata->profile[pix].d * (end - pdata->profile[pix].at);
      end = pdata->profile[pix--].at;
    }
  }
}

static void
init_tt_after_lct(struct ttef_cumulative_data *pdata)
{
  int ii = pdata->nbunfixed-1;
  int pix = pdata->nbprofile-2;
  SP_integer energy = 0;	/* total profile of all segments beyond pix */
  SP_integer end = pdata->profile[pix+1].at;
  
  while (ii>=0) {
    int i = pdata->lct_array[ii];
    if (pix<0 || end <= pdata->lct[i]) {
      pdata->tt_after_lct[ii--] = energy;
    } else if (pdata->profile[pix].at <= pdata->lct[i]) {
      pdata->tt_after_lct[ii--] = energy + pdata->profile[pix].d * (end - pdata->lct[i]);
    } else {
      energy += pdata->profile[pix].d * (end - pdata->profile[pix].at);
      end = pdata->profile[pix--].at;
    }
  }
}

static SP_integer
free_energy(struct ttef_cumulative_data *pdata, int i)
{
  SP_integer free_dur = DUR(i);
  
  if (pdata->ect[i] > pdata->lst[i])
    free_dur -= pdata->ect[i] - pdata->lst[i];
  return free_dur * RES(i);
}

/* prevent t from executing in interval [a,b) with RES(t) + d > limit */
/* Precond: MAXRES(t) + d > limit */
static SP_BOOL
tt_rule(Wam wam, struct ttef_cumulative_data *pdata, int t, SP_integer a, SP_integer b, SP_integer d)
{
  SP_integer limit = dvar_max_l(LIMITVAR);
  SP_integer durt = DUR(t);
  SP_integer rest = RES(t);
  
  if (rest + d <= limit) {	/* maybe limit RES(t) */
    return (pdata->ect[t]<=a ||
	    pdata->lst[t]>=b ||
	    (dvar_fix_max_l(RESVAR(t), limit-d)>=0 &&
	     dvar_fix_min_l(LIMITVAR, rest+d)>=0));
  } else if (a-durt+1 <= b-1) {	/* prevent t from executing during [a,b) */
    return
      dvar_prune_interval_l(ORIGVAR(t), a-durt+1, b-1)>=0 &&
      dvar_prune_interval_l(ENDVAR(t), a+1, b+durt-1)>=0;
  } else {
    return TRUE;
  }
}

static SP_BOOL
tt_filtering(Wam wam, struct ttef_cumulative_data *pdata)
{
  int i, j, p=0;
  SP_integer limit = dvar_max_l(LIMITVAR);
  
  for (i=0; i<pdata->nbunfixed; i++) {
    int t = pdata->est_array[i];
    SP_integer estt = pdata->est[t];
    SP_integer lstt = pdata->lst[t];
    SP_integer ectt = pdata->ect[t];
    SP_integer lctt = pdata->lct[t];
    SP_integer cura;

    for (; p+1<pdata->nbprofile && pdata->profile[p+1].at <= estt; p++)
      ;
    cura = pdata->profile[p].at;
    for (j=p; j+1<pdata->nbprofile && cura < lctt; j++) {
      SP_integer a = cura;
      SP_integer b = cura = pdata->profile[j+1].at;
      SP_integer d = pdata->profile[j].d;
      if (DUR(t) > 0 && MAXRES(t) + d > limit) {
	if (lstt >= ectt) {	/* simple case: no comp. part */
	  if (!tt_rule(wam, pdata, t, a, b, d))
	    return FALSE;
	} else {		/* complex case: comp. part */
	  if (lstt < a) {
	    if (ectt <= a) {
	      if (!tt_rule(wam, pdata, t, a, b, d)) /* comp. part meets/before [a,b) */
		return FALSE;
	    } else if (ectt < b) {
	      if (!tt_rule(wam, pdata, t, ectt, b, d)) /* comp. part overlaps [a,b) */
		return FALSE;
	    }
	  } else if (lstt==a) {
	    if (ectt < b) {
	      if (!tt_rule(wam, pdata, t, ectt, b, d)) /* comp. part starts [a,b) */
		return FALSE;
	    }
	  } else {
	    if (ectt < b) {
	      if (!tt_rule(wam, pdata, t, a, lstt, d) || /* comp. part during [a,b) */
		  !tt_rule(wam, pdata, t, ectt, b, d))
		return FALSE;
	    } else if (ectt==b) {
	      if (!tt_rule(wam, pdata, t, a, lstt, d)) /* comp. part finishes [a,b) */
		return FALSE;
	    } else if (lstt < b) {
	      if (!tt_rule(wam, pdata, t, a, lstt, d)) /* comp. part overlapped by [a,b) */
		return FALSE;
	    } else {
	      if (!tt_rule(wam, pdata, t, a, b, d)) /* comp. part met by/after [a,b) */
		return FALSE;
	    }
	  }
	}
      }
    }
  }
  return TRUE;
}

static SP_BOOL
ttef_bounds_propagation_lb(struct ttef_cumulative_data *pdata)
{
  SP_integer begin, end;
  SP_integer smallest = pdata->est[pdata->est_array[0]];
  SP_integer largest = pdata->lct[pdata->lct_array[pdata->nbunfixed-1]];
  SP_integer limit = dvar_max_l(LIMITVAR);
  int est_idx_last = pdata->nbunfixed-1;
  int ii, jj;
	
  end = largest+1;
  for (ii = pdata->nbunfixed-1; ii >= 0; ii--) {
    int i = pdata->lct_array[ii];
    if (end != pdata->lct[i]) {
      int u = -1;
      SP_integer update_en_req_start = -1;
      SP_integer min_en_avail = limit * (largest - smallest);
      SP_integer min_begin = -1;
      SP_integer en_req_free = 0;
		
      end = pdata->lct[i];
      while (est_idx_last>=0 && pdata->est[pdata->est_array[est_idx_last]] >= end)
	est_idx_last--;

      for (jj = est_idx_last; jj >= 0; jj--) {
	int j = pdata->est_array[jj];
	SP_integer min_en_in = RES(j) * max((SP_integer)0, min(end, pdata->ect[j]) - max(min_begin, pdata->lst[j]));
	
	begin = pdata->est[j];
	if (min_begin >= 0 && min_en_avail + min_en_in < RES(j) * (min(end, pdata->lct[j]) - max(min_begin, pdata->lst[j]))) {
	  SP_integer dur_avail = (min_en_avail + min_en_in) / RES(j);
	  SP_integer lct_new = min_begin + dur_avail;

	  if (dvar_fix_max_l(ENDVAR(j), lct_new)<0)
	    return FALSE;
	}
	if (pdata->lct[j] <= end) {
	  en_req_free += free_energy(pdata, j);
	} else {				
	  SP_integer dur_fixed = max((SP_integer)0, pdata->ect[j] - pdata->lst[j]);
	  SP_integer dur_shift = max((SP_integer)0, end - pdata->lst[j] - dur_fixed);
	  SP_integer en_req_start = min(free_energy(pdata, j), RES(j) * (end - pdata->est[j])) - RES(j) * dur_shift;
	  
	  en_req_free += RES(j) * dur_shift;
	  if (en_req_start > update_en_req_start) {
	    update_en_req_start = en_req_start;
	    u = j;
	  }
	}
	{
	  SP_integer en_req = en_req_free + pdata->tt_after_est[jj] - pdata->tt_after_lct[ii];
	  SP_integer en_avail = limit * (end - begin) - en_req;
	  
	  if (min_en_avail > en_avail) {
	    min_en_avail = en_avail;
	    min_begin = begin;
	  }
	  if (en_avail < 0)
	    return FALSE;
	  if (en_avail < update_en_req_start) {
	    SP_integer dur_mand  = max((SP_integer)0, min(end, pdata->ect[u]) - pdata->lst[u]);
	    SP_integer dur_shift = max((SP_integer)0, end - pdata->lst[u] - dur_mand);
	    SP_integer en_in     = RES(u) * (dur_mand + dur_shift);
	    SP_integer en_avail_new = en_avail + en_in;
	    SP_integer dur_avail = en_avail_new / RES(u);
	    SP_integer start_new = end - dur_avail;
	    
	    if (dvar_fix_min_l(ORIGVAR(u), start_new)<0)
	      return FALSE;
	  }
	}
      }
    }
  }
  return TRUE;
}

static SP_BOOL
ttef_bounds_propagation_ub(struct ttef_cumulative_data *pdata)
{
  SP_integer begin, end;
  SP_integer smallest = pdata->est[pdata->est_array[0]];
  SP_integer largest = pdata->lct[pdata->lct_array[pdata->nbunfixed-1]];
  SP_integer limit = dvar_max_l(LIMITVAR);
  int lct_idx_last = pdata->nbunfixed-1;
  int ii, jj;
	
  begin = smallest-1;
  for (ii = 0; ii < pdata->nbunfixed; ii++) {
    int i = pdata->est_array[ii];
    if (begin != pdata->est[i]) {
      int u = -1;
      SP_integer update_en_req_end = -1;
      SP_integer min_en_avail = limit * (largest - smallest);
      SP_integer min_end = -1;
      SP_integer en_req_free = 0;

      begin = pdata->est[i];
      while (lct_idx_last<pdata->nbunfixed && pdata->lct[pdata->lct_array[lct_idx_last]] <= begin)
	lct_idx_last++;

      for (jj = lct_idx_last; jj < pdata->nbunfixed; jj++) {
	int j = pdata->lct_array[jj];
	SP_integer min_en_in = RES(j) * max((SP_integer)0, min(min_end, pdata->ect[j]) - max(begin, pdata->lst[j]));
	
	end = pdata->lct[j];
	if (min_end >= 0 && min_en_avail + min_en_in < RES(j) * (min(min_end, pdata->ect[j]) - max(begin, pdata->est[j]))) {
	  SP_integer dur_avail = (min_en_avail + min_en_in) / RES(j);
	  SP_integer est_new = min_end - dur_avail;

	  if (dvar_fix_min_l(ORIGVAR(j), est_new)<0)
	    return FALSE;
	}
	if (begin <= pdata->est[j]) {
	  en_req_free += free_energy(pdata, j);
	} else {
	  SP_integer dur_fixed = max((SP_integer)0, pdata->ect[j] - pdata->lst[j]);
	  SP_integer dur_shift = max((SP_integer)0, pdata->ect[j] - begin - dur_fixed);
	  SP_integer en_req_end = min(free_energy(pdata, j), RES(j) * (pdata->lct[j] - begin)) - RES(j) * dur_shift;
	  
	  en_req_free += RES(j) * dur_shift;
	  if (en_req_end > update_en_req_end) {
	    update_en_req_end = en_req_end;
	    u = j;
	  }
	}
	{
	  SP_integer en_req = en_req_free + pdata->tt_after_est[ii] - pdata->tt_after_lct[jj];
	  SP_integer en_avail = limit * (end - begin) - en_req;
	  
	  if (min_en_avail > en_avail) {
	    min_en_avail = en_avail;
	    min_end = end;
	  }
	  if (en_avail < 0)
	    return FALSE;
	  if (en_avail < update_en_req_end) {
	    SP_integer dur_mand  = max((SP_integer)0, pdata->ect[u] - max(begin, pdata->lst[u]));
	    SP_integer dur_shift = max((SP_integer)0, pdata->ect[u] - begin - dur_mand);
	    SP_integer en_in     = RES(u) * (dur_mand + dur_shift);
	    SP_integer en_avail_new = en_avail + en_in;
	    SP_integer dur_avail = en_avail_new / RES(u);
	    SP_integer end_new   = begin + dur_avail;
	    
	    if (dvar_fix_max_l(ENDVAR(u), end_new)<0)
	      return FALSE;
	  }
	}
      }
    }
  }
  return TRUE;
}

/* f(Tasks,Limit-LimitA,NbUnfixed,_Handle,0) */
void SPCDECL
prolog_fd_ttef_cumulative(Wam wam,
			  SP_term_ref State0,
			  SP_term_ref State,
			  SP_term_ref Actions)
{
  TAGGED tasks, task, handle, tmp;
  SP_BOOL committed, ground = TRUE;
  int ii, i, j, ent = -1;		/* disentailed unless otherwise */
  int nbtasks;
  SP_integer limit, ignore;
  struct ttef_cumulative_data *pdata;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&ignore,&committed);
  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct ttef_cumulative_data,handle);
    fd.gdata = pdata;
    nbtasks = pdata->nbtasks;
  } else {			/* build persistent state */
    char *ptr;
    SP_integer total_size;
    
    DerefArg(tmp,X(0),1);		/* get Tasks */
    nbtasks = fd_list_length(tmp);
    total_size = 4*nbtasks*sizeof(struct dvar) +
      2*nbtasks*sizeof(struct tt) +
      6*nbtasks*sizeof(SP_integer) +
      3*nbtasks*sizeof(int);
    pdata = Palloc(struct ttef_cumulative_data, total_size, handle);
    fd.gdata = pdata;
    ptr = (char *)(pdata+1);
    pdata->origvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->durvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->endvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->resvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->profile = (struct tt *)ptr;
    ptr += 2*nbtasks*sizeof(struct tt);
    pdata->est = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->lst = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->ect = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->lct = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->tt_after_est = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->tt_after_lct = (SP_integer *)ptr;
    ptr += nbtasks*sizeof(SP_integer);
    pdata->est_array = (int *)ptr;
    ptr += nbtasks*sizeof(int);
    pdata->lct_array = (int *)ptr;
    ptr += nbtasks*sizeof(int);
    pdata->target = (int *)ptr;
    ptr += nbtasks*sizeof(int);
    SP_ASSERT(ptr==(char *)(pdata+1)+total_size);
    pdata->destructor = ttef_cumulative_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->nrefs = 8*nbtasks+2;
    pdata->refbase = SP_alloc_globrefs(pdata->nrefs);
    pdata->nbtasks = nbtasks;

    DerefArg(tmp,X(0),2);	/* get Limit */
    fd_get_var_and_attr(tmp,RefLimitAttr);
    DerefArg(tasks,X(0),1);	/* get Tasks */
    for (i=0; i<nbtasks; i++) {
      DerefCar(task,tasks);
      DerefCdr(tasks,tasks);
      DerefArg(tmp,task,1);
      fd_get_var_and_attr(tmp,RefOrigAttr(i));
      DerefArg(tmp,task,2);
      fd_get_var_and_attr(tmp,RefDurAttr(i));
      DerefArg(tmp,task,3);
      fd_get_var_and_attr(tmp,RefEndAttr(i));
      DerefArg(tmp,task,4);
      fd_get_var_and_attr(tmp,RefResAttr(i));
    }

    dvar_init(LIMITVAR, RefLimitAttr, RefLimit);
    for (i=0; i<nbtasks; i++) {
      pdata->target[i] = i;
      dvar_init(ORIGVAR(i), RefOrigAttr(i), RefOrig(i));
      dvar_init(DURVAR(i), RefDurAttr(i), RefDur(i));
      dvar_init(ENDVAR(i), RefEndAttr(i), RefEnd(i));
      dvar_init(RESVAR(i), RefResAttr(i), RefRes(i));
    }
  }
  				/* RESUME HERE */
  dvar_refresh(LIMITVAR);
  if (!dvar_is_integer(LIMITVAR))
    ground = FALSE;
  limit = dvar_max_l(LIMITVAR);
  DerefArg(tmp,X(0),3);		/* get NbUnfixed */
  pdata->nbunfixed = GetSmall_int(tmp);

  for (ii=0; ii<pdata->nbunfixed; ii++) {
    int i = pdata->target[ii];

    dvar_refresh(ORIGVAR(i));
    dvar_refresh(DURVAR(i));
    dvar_refresh(ENDVAR(i));
    dvar_refresh(RESVAR(i));
    pdata->est[i] = dvar_min_l(ORIGVAR(i));
    pdata->lst[i] = dvar_max_l(ORIGVAR(i));
    pdata->ect[i] = pdata->est[i] + DUR(i);
    pdata->lct[i] = pdata->lst[i] + DUR(i);
  }
  for (ii=0; ground && ii<pdata->nbunfixed; ii++) {
    int i = pdata->target[ii];
    
    if (!dvar_is_integer(ORIGVAR(i)) ||
	!dvar_is_integer(DURVAR(i)) ||
	!dvar_is_integer(ENDVAR(i)) ||
	!dvar_is_integer(RESVAR(i)))
      ground = FALSE;
  }
  for (ii=0, j=0; ii<pdata->nbunfixed; ii++) {
    int i = pdata->target[ii];
    if (DUR(i)>0 &&
	(dvar_fix_min_l(LIMITVAR, RES(i))<0 ||
	 dvar_fix_max_l(RESVAR(i), limit)<0))
      goto ret;
    else if (RES(i)>limit &&
	     dvar_fix_max_l(DURVAR(i), 0)<0)
      goto ret;
    pdata->est_array[j] = i;
    pdata->lct_array[j++] = i;
  }
				/* build arrays of unfixed tasks sorted ascending on est and lct */
  qsort_asc_est(wam, pdata->est_array, j);
  qsort_asc_lct(wam, pdata->lct_array, j);
				/* build compulsory part profile */
  for (i=0, j=0; i<nbtasks; i++) {
    if (pdata->ect[i] > pdata->lst[i] && RES(i)>0) {
      pdata->profile[j].at = pdata->lst[i];
      pdata->profile[j++].d  = RES(i);
      pdata->profile[j].at = pdata->ect[i];
      pdata->profile[j++].d  = -RES(i);
    }
  }
  if (j>0) {
    int k = j;
    SP_integer h = 0;

    qsort_profile(wam, pdata->profile, j);
    for (i=0, j=0; i<k; i++) {
      h += pdata->profile[i].d;
      if ((i+1==k || pdata->profile[i].at < pdata->profile[i+1].at) &&
	  (j==0 || h != pdata->profile[j-1].d)) {
	pdata->profile[j].at = pdata->profile[i].at;
	pdata->profile[j++].d  = h;
	if (dvar_fix_min_l(LIMITVAR, h) < 0)
	  goto ret;
      }
    }
  }
  pdata->nbprofile = j;
  
  if (pdata->nbunfixed>0) {
    init_tt_after_est(pdata);
    init_tt_after_lct(pdata);
    
    if ((pdata->nbprofile>0 && !tt_filtering(wam, pdata)) ||
	!ttef_bounds_propagation_lb(pdata) ||
	!ttef_bounds_propagation_ub(pdata))
      goto ret;
  }
  ent = ground;
  
  dvar_pruning_done(LIMITVAR);
  for (ii=0; ii<pdata->nbunfixed; ii++) {
    int i = pdata->target[ii];
    dvar_pruning_done(ORIGVAR(i));
    dvar_pruning_done(DURVAR(i));
    dvar_pruning_done(ENDVAR(i));
    dvar_pruning_done(RESVAR(i));
  }
  dvar_export(LIMITVAR);
  for (ii=0; ii<pdata->nbunfixed; ii++) {
    int i = pdata->target[ii];
    dvar_export(ORIGVAR(i));
    dvar_export(DURVAR(i));
    dvar_export(ENDVAR(i));
    dvar_export(RESVAR(i));
  }

  /* partition into [ unfixed | fixed ] */

  {
    int inf = 0;
    int sup = pdata->nbunfixed-1;
    int held = pdata->target[sup]; /* sup is the hole */
    int current = pdata->target[inf];
    
    while (inf<=sup) {
      if (MAXDUR(current) > 0  &&
	  MAXRES(current) > 0  &&
	  (!dvar_is_integer(ORIGVAR(current)) ||
	   !dvar_is_integer(DURVAR(current)) ||
	   !dvar_is_integer(ENDVAR(current)) ||
	   !dvar_is_integer(RESVAR(current)))) {
	pdata->target[inf] = current;
	inf++;
	current = (inf>=sup ? held : pdata->target[inf]);
      } else {
	pdata->est[current] = dvar_min_l(ORIGVAR(current)); /* keep pdata->est pdata->lst pdata->ect pdata->lct up to date */
	pdata->lst[current] = dvar_max_l(ORIGVAR(current));
	pdata->ect[current] = pdata->est[current] + DUR(current);
	pdata->lct[current] = pdata->lst[current] + DUR(current);
	pdata->target[sup] = current;
	sup--;
	current = (inf>=sup ? held : pdata->target[sup]);
      }
    }
    CTagToArg(X(0),3) = MakeSmall(inf);	/* set NbUnfixed */
  }

  if (ent==1)
    Pfree;
 ret:
  dvar_export_done(wam,Actions, ent);
}
