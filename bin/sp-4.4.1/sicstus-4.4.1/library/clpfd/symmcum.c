/* Copyright(C) 2000, Swedish Institute of Computer Science */

/****************************************************************

 cumulatives(TASKS,MACHINES,OPTIONS).
 Based on Nicolas Beldiceanu's algoritm which sweeps an interval at a time
 + frontier reasoning + generalization + task intervals.

 ****************************************************************/

#include "fd.h"
#include "dvars.h"


typedef SP_integer TASK;
#undef MACHINE                  /* [PM] 3.9.0 MACHINE is a #define on Tru64 */
typedef SP_integer MACHINE;
typedef SP_integer EVENT;
#define NOTASK ((TASK)-1)
#define NOMACHINE ((MACHINE)-1)
#define NOEVENT ((EVENT)-1)
#define NOPROFILE ((PROFILE)-1)

#define EV_START 1
#define EV_END 2

#define EV_CHECK_GE 4
#define EV_CHECK_NONE 8
#define EV_CHECK_LT 16

#define EV_COMPULSORY_PART 32

#define EV_TYPE_PROFILE 64
#define EV_TYPE_PRUNING 128

/* task flags */
#define PRUNED_ORIGIN 1
#define PRUNED_END 2
#define PRUNED_DURATION 4
#define PRUNED_HEIGHT 8
#define PRUNED_MACHINE 16
/* [MC] 4.4: tasks _never_ lose the SOURCE property */
#define STATUS_TARGET 32
#define STATUS_SOURCE 64

/* The constraint frame. */
/* ===================== */
/* Valid options: */
/* bound(lower|upper)   ==> 0x0|0x1 */
/* generalization(Bool) ==> 0x0|0x2 */
/* task_intervals(Bool) ==> 0x0|0x4 */
/* prune(all|next)      ==> 0x0|0x8 */

#define OPT_ATMOST 0x1
#define OPT_GENERALIZATION 0x2
#define OPT_TASK_INTERVALS 0x4
#define OPT_PRUNE_NEXT 0x8

struct symmcum_data {
  void (SPCDECL *destructor)(void*);
  SPEnv *spenv;
  SP_globref refbase;		/* static */
  int options;
  SP_integer stamp;
  SP_integer machine_size;
  int ntasks;			/* static */
  int nmachines;		/* static */
  int last_to_prune;		/* volatile: min{t | TARGET(t)} or ntargets */
  int ntargets;			/* # TARGET+SOURCE tasks */
  int nsources;			/* # SOURCE tasks */
  int nfiltered;		/* # filtered tasks */
  TASK *target;			/* [2*ntasks] */
  TASK *filtered;		/* [ntasks], malloced */
  SP_integer *minest;			/* [ntasks] */
  EVENT *eventqc;		/* [4*ntasks], qsorted, malloced */
  EVENT *eventqnc;		/* [8*ntasks], qsorted, malloced */
  int top_prune;		/* stack top */
  TASK *stack_prune;		/* [ntasks], pruning stack, malloced */
  SP_integer capacity_min;		/* min of all capacities */
  SP_integer capacity_max;		/* max of all capacities */
  SP_integer max_duration;		/* max duration of the tasks for which
				   the height attribute may cause a problem */
  SP_integer min_min_height;		/* min value of min initial height of tasks */
  SP_integer max_min_height;		/* max value of min initial height of tasks */
  SP_integer min_max_height;		/* min value of max initial height of tasks */
  SP_integer max_max_height;		/* max value of max initial height of tasks */
  SP_integer nconstrained;		/* counter */
  SP_integer profile;		/* counter */
  /* valid values upon backtracking */
  TAGGED *initial_est;
  TAGGED *initial_lct;
  /* valid values in forward execution */
  TAGGED *previous_est;
  TAGGED *previous_lct;
  /* either one of the two above */
  TAGGED *table_est;
  TAGGED *table_lct;
  TAGGED global_est;
  TAGGED global_lct;
  
  /* fields for the generalization */
  SP_integer *size_;			/* [ntasks], malloced even if !OPT_GENERALIZATION */
  PROFILE *origin_profile;	/* [ntasks], malloced */
  PROFILE *end_profile;		/* [ntasks], malloced */
  PROFILE *duration_profile;	/* [ntasks], malloced */
  PROFILE *height_profile;	/* [ntasks], malloced */
  PROFILE *origin_tempo;	/* [ntasks], malloced */
  PROFILE *end_tempo;		/* [ntasks], malloced */
  PROFILE *duration_tempo;	/* [ntasks], malloced */
  PROFILE *height_tempo;	/* [ntasks], malloced */
  SP_integer *origin_stamp;		/* [ntasks], malloced */
  SP_integer *end_stamp;		/* [ntasks], malloced */
  SP_integer *duration_stamp;		/* [ntasks], malloced */
  SP_integer *height_stamp;		/* [ntasks], malloced */
  int generalize_nvar;		/* volatile, used even if !OPT_GENERALIZATION */
  PROFILE **generalize_tempo_marray; /* [4*nfiltered], malloced */
  PROFILE **generalize_profile_marray; /* [4*nfiltered], malloced */
  TASK *generalize_itask;	/* [4*nfiltered], malloced */
  int *generalize_dvar;		/* [4*nfiltered], malloced */
  
  /* fields for task intervals */
  SP_integer sum_max_length_inter;
  TASK *try_to_prune;		/* [ntasks], malloced */
  SP_BOOL *include_in_machines;	/* [ntasks], malloced */
  SP_integer *min_length_inter;	/* [ntasks], malloced */
  SP_integer *max_length_inter;	/* [ntasks], malloced */
  SP_integer *max_min_length_inter;	/* [nmachines], malloced */
  SP_integer *contribution;		/* [ntasks], malloced */
  SP_integer *hcontribution;		/* [ntasks], malloced */
  
  /* fields for frontier-based pruning */
  SP_integer *start_frontier;		/* [ntasks], malloced */
  SP_integer *end_frontier;		/* [ntasks], malloced */
  TASK *first_sweep_frontier;	/* [nmachines], malloced */
  TASK *next_sweep_frontier;	/* [ntasks], malloced */
  TASK *first_prune_frontier;	/* [nmachines], malloced */
  TASK *next_prune_frontier;	/* [ntasks], malloced */
  TASK *first_check_frontier;	/* [nmachines], malloced */
  TASK *next_check_frontier;	/* [ntasks], malloced */
  SP_integer *init_status_frontier;	/* [ntasks], malloced */
  SP_integer *best_status_frontier;	/* [ntasks], malloced */
  struct {
    MACHINE *real;		/* virtual mach -> real mach */
    SP_integer *capacity;		/* virtual mach -> min or max */
  } machine;			/* [nmachines] */
  struct {
    SP_integer *flags;		/* flags for the five dvars + in_profile */
    MACHINE *m_status;
    SP_integer *htask;
    Dvar dvorg;
    Dvar dvdur;
    Dvar dvend;
    Dvar dvheight;
    Dvar dvmach;
    SP_integer *lctkey;		/* LCT just after backtracking */
  } task;			/* [ntask] */
  struct {
    SP_integer *date;			/* location in time, malloced */
    TASK *task;			/* generating task, malloced */
    SP_integer *height;		/* quantity to add to the profile, malloced */
    MACHINE *machine;		/* assigned machine, malloced */
    unsigned int *flags;	/* EV_* + EV_TYPE_* + EV_CHECK_*, malloced */
  } event;			/* [10*ntasks] */
  /* space for the above arrays */
};

#define TASK(I) (pdata->target[I])
#define MINEST(I) (pdata->minest[I])
#define FILTERED(I) (pdata->filtered[I])
#define SIZE_(I) (pdata->size_[I])
#define ORIGIN_PROFILE(I) (pdata->origin_profile[I])
#define END_PROFILE(I) (pdata->end_profile[I])
#define DURATION_PROFILE(I) (pdata->duration_profile[I])
#define HEIGHT_PROFILE(I) (pdata->height_profile[I])
#define ORIGIN_TEMPO(I) (pdata->origin_tempo[I])
#define END_TEMPO(I) (pdata->end_tempo[I])
#define DURATION_TEMPO(I) (pdata->duration_tempo[I])
#define HEIGHT_TEMPO(I) (pdata->height_tempo[I])
#define ORIGIN_STAMP(I) (pdata->origin_stamp[I])
#define END_STAMP(I) (pdata->end_stamp[I])
#define DURATION_STAMP(I) (pdata->duration_stamp[I])
#define HEIGHT_STAMP(I) (pdata->height_stamp[I])

#define REAL(M) (pdata->machine.real[M])
#define CAPACITY(M) (pdata->machine.capacity[M])

#define FLAGS(T) (pdata->task.flags[T])
#define M_STATUS(T) (pdata->task.m_status[T])
#define HTASK(T) (pdata->task.htask[T])

#define DVORG(T)  (pdata->task.dvorg + (T))
#define EST(T)    dvar_min_l(DVORG(T))
#define EST_T(T)  dvar_min_t(DVORG(T))
#define LST(T)    dvar_max_l(DVORG(T))
#define LST_T(T)  dvar_max_t(DVORG(T))
#define SETORG(T)   ((FLAGS(T)&STATUS_TARGET) ? dvar_set(DVORG(T)) : fd_interval(wam,EST_T(T),EST_T(T)))

#define DVEND(T)  (pdata->task.dvend + (T))
#define ECT(T)    dvar_min_l(DVEND(T))
#define ECT_T(T)  dvar_min_t(DVEND(T))
#define LCT(T)    dvar_max_l(DVEND(T))
#define LCT_T(T)  dvar_max_t(DVEND(T))
#define LCTKEY(T) (pdata->task.lctkey[T])

#define DVDUR(T)    (pdata->task.dvdur + (T))
#define MINDUR(T)   dvar_min_l(DVDUR(T))
#define MINDUR_T(T) dvar_min_t(DVDUR(T))
#define MAXDUR(T)   dvar_max_l(DVDUR(T))
#define MAXDUR_T(T) dvar_max_t(DVDUR(T))

#define DVHEIGHT(T)    (pdata->task.dvheight + (T))
#define MINHEIGHT(T)   dvar_min_l(DVHEIGHT(T))
#define MINHEIGHT_T(T) dvar_min_t(DVHEIGHT(T))
#define MAXHEIGHT(T)   dvar_max_l(DVHEIGHT(T))
#define MAXHEIGHT_T(T) dvar_max_t(DVHEIGHT(T))

#define DVMACH(T)    (pdata->task.dvmach + (T))
#define MINMACH_T(T) dvar_min_t(DVMACH(T))
#define FIXEDMACH(T) dvar_is_integer(DVMACH(T))
#define SIZEMACH(T)  dvar_value_count(DVMACH(T))
#define SETMACH(T)   ((FLAGS(T)&STATUS_TARGET) ? dvar_set(DVMACH(T)) : fd_interval(wam,MINMACH_T(T),MINMACH_T(T)))

#define TABLE_EST_T(T) (pdata->table_est[T])
#define TABLE_EST(T) GetSmall(pdata->table_est[T])
#define TABLE_LCT_T(T) (pdata->table_lct[T])
#define TABLE_LCT(T) GetSmall(pdata->table_lct[T])

#define RefOrgAttr(T) (pdata->refbase + 10*(T))
#define RefOrgVar(T) (pdata->refbase + 10*(T) + 1)
#define RefEndAttr(T) (pdata->refbase + 10*(T) + 2)
#define RefEndVar(T) (pdata->refbase + 10*(T) + 3)
#define RefDurAttr(T) (pdata->refbase + 10*(T) + 4)
#define RefDurVar(T) (pdata->refbase + 10*(T) + 5)
#define RefHeightAttr(T) (pdata->refbase + 10*(T) + 6)
#define RefHeightVar(T) (pdata->refbase + 10*(T) + 7)
#define RefMachAttr(T) (pdata->refbase + 10*(T) + 8)
#define RefMachVar(T) (pdata->refbase + 10*(T) + 9)
#define RefMachineLimit(M) (pdata->refbase + 10*pdata->ntasks + 2*(M))

#define EV_DATE(E) (pdata->event.date[E])
#define EV_TASK(E) (pdata->event.task[E])
#define EV_HEIGHT(E) (pdata->event.height[E])
#define EV_MACHINE(E) (pdata->event.machine[E])
#define EV_FLAGS(E) (pdata->event.flags[E])

static void SPCDECL symmcum_destructor(void *pdata_v)
{
  struct symmcum_data *pdata = (struct symmcum_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,10*pdata->ntasks+2*pdata->nmachines);
  SP_free(pdata);
}


#define HAS_COMPULSORY_PART(t) (LST(t)<ECT(t))

#define MUST_OVERLAP(t,low,up) (low<ECT(t) && LST(t)<up && MINDUR(t)>0)

#define MAY_OVERLAP(t,low,up) (low<LCT(t) && EST(t)<up && MAXDUR(t)>0)

#define ORIGIN_OVERLAPS(t,low,up) (low<LST(t) && EST(t)<up && EST(t)<LST(t))

#define END_OVERLAPS(t,low,up) (low<LCT(t) && ECT(t)<up && ECT(t)<LCT(t))

#define OVERFLOWS(h,vr)							   \
	  (!((pdata->options&OPT_ATMOST) ? ((h)<=0 && (h)<=CAPACITY(vr))   \
	                                 : ((h)>=0 && (h)>=CAPACITY(vr))))

/* sort by <ascending machine, ascending date> */
static int 
cmp_asc_eventc(Wam wam,
		      EVENT *e1, EVENT *e2)
{
  struct symmcum_data *pdata = fd.gdata;
  MACHINE m1 = EV_MACHINE(*e1);
  MACHINE m2 = EV_MACHINE(*e2);
  SP_integer ev_date1, ev_date2;

  if (m1<m2)
    return -1;
  if (m1>m2)
    return 1;
  ev_date1 = EV_DATE(*e1);
  ev_date2 = EV_DATE(*e2);
  return CMP(ev_date1,ev_date2);
}


#define QType EVENT
#define QCmp  cmp_asc_eventc
#define QSort qsort_asc_eventc
#include "qsort.ic"

/* sort by <ascending date, PROFILE before PRUNING> */
static int 
cmp_asc_eventnc(Wam wam,
		       EVENT *e1, EVENT *e2)
{
  struct symmcum_data *pdata = fd.gdata;

  SP_integer d1 = EV_DATE(*e1);
  SP_integer d2 = EV_DATE(*e2);

  if (d1<d2)
    return -1;
  if (d1>d2)
    return 1;
  return QSORT_COMPARE(EV_FLAGS(*e1), EV_FLAGS(*e2));
}


#define QType EVENT
#define QCmp  cmp_asc_eventnc
#define QSort qsort_asc_eventnc
#include "qsort.ic"

static int 
cmp_asc_lctkey(Wam wam,
		      TASK *e1, TASK *e2)
{
  struct symmcum_data *pdata = fd.gdata;
  SP_integer lctkey1 = LCTKEY(*e1);
  SP_integer lctkey2 = LCTKEY(*e2);

  return CMP(lctkey1,lctkey2);
}


#define QType TASK
#define QCmp  cmp_asc_lctkey
#define QSort qsort_asc_lctkey
#include "qsort.ic"

static MACHINE 
virtual_machine(Wam wam, MACHINE real)
{
  int j, mid, sup;
  struct symmcum_data *pdata = fd.gdata;
  
  /* dichotomic search for j such that REAL(j)==real */
  j = 0;
  sup = pdata->nmachines;
  while (j<sup) {
    mid = (j+sup)>>1;
    if (REAL(mid)<real)
      j = mid+1;
    else
      sup = mid;
  }
  return j;
}

/* Add a unit height interval [b,e] to a profile p */
static void
cd_update(Wam wam,
	  TASK t, MACHINE vr, int dvar,
	  PROFILE *profile, PROFILE *tempo, SP_integer *stamp,
	  SP_integer b, SP_integer e)
{
  struct symmcum_data *pdata = fd.gdata;
  int i;
  
  if (b<=e) {
    if (*stamp != vr && *tempo != NOPROFILE) {
      fd_profile_dispose(wam, *tempo);
      *tempo = NOPROFILE;
    }
    if (*tempo == NOPROFILE) {
      *tempo = fd_empty_profile();
      *stamp = vr;

      /* record information needed to update marray_profile as soon as
	 we finish the sweep proess on resource vr */

      i = pdata->generalize_nvar++;
      pdata->generalize_tempo_marray[i] = tempo;
      pdata->generalize_profile_marray[i] = profile;
      pdata->generalize_itask[i] = t;
      pdata->generalize_dvar[i] = dvar;
    }
    *tempo = fd_profile_update(wam,*tempo,b,e+1,1);
  }
}


static SP_BOOL 
prune_resource_consumption(Wam wam,
			   TASK t, MACHINE vr,
			   SP_integer low, SP_integer up,
			   SP_integer contrib_t /* contribution of t in profile */
			   )
{
  struct symmcum_data *pdata = fd.gdata;

  if (MUST_OVERLAP(t,low,up)) {

    /* forbid too big or small height for t */

    SP_integer limit = CAPACITY(vr) - (pdata->profile - contrib_t);

    if (FIXEDMACH(t)) {
      if ((pdata->options&OPT_ATMOST)) {
	if (limit<MAXHEIGHT(t)) {
	  return (dvar_fix_max_l(DVHEIGHT(t),limit)>=0);
	}
      } else {
	if (limit>MINHEIGHT(t)) {
	  return (dvar_fix_min_l(DVHEIGHT(t),limit)>=0);
	}
      }
    } else if (SIZE_(t)>1) {
      if ((pdata->options&OPT_ATMOST))
	cd_update(wam,t,vr,PRUNED_HEIGHT,
		  &HEIGHT_PROFILE(t), &HEIGHT_TEMPO(t),&HEIGHT_STAMP(t),
		  limit+1,MAXHEIGHT(t));
      else
	cd_update(wam,t,vr,PRUNED_HEIGHT,
		  &HEIGHT_PROFILE(t), &HEIGHT_TEMPO(t),&HEIGHT_STAMP(t),
		  MINHEIGHT(t),limit-1);
    }
  }
  return TRUE;
}

static SP_BOOL
prune_origin_interval(Wam wam, TASK t, SP_integer lb, SP_integer ub)
{
  struct symmcum_data *pdata = fd.gdata;

  if (lb>ub || (lb>EST(t) && ub<LST(t)))
    return TRUE;		/* would cut a hole, at most */

  switch (dvar_prune_interval_l(DVORG(t),lb,ub)) {
  case -1:
    return FALSE;
  default:
    return TRUE;
  }
}

static SP_BOOL
prune_end_interval(Wam wam, TASK t, SP_integer lb, SP_integer ub)
{
  struct symmcum_data *pdata = fd.gdata;

  if (lb>ub || (lb>ECT(t) && ub<LCT(t)))
    return TRUE;		/* would cut a hole, at most */

  switch (dvar_prune_interval_l(DVEND(t),lb,ub)) {
  case -1:
    return FALSE;
  default:
    return TRUE;
  }
}

static SP_BOOL
prune_duration_interval(Wam wam, TASK t, SP_integer lb, SP_integer ub)
{
  struct symmcum_data *pdata = fd.gdata;

  if (lb>ub || (lb>MINDUR(t) && ub<MAXDUR(t)))
    return TRUE;		/* would cut a hole, at most */

  switch (dvar_prune_interval_l(DVDUR(t),lb,ub)) {
  case -1:
    return FALSE;
  default:
    return TRUE;
  }
}

static SP_BOOL
prune_height_interval(Wam wam, TASK t, SP_integer lb, SP_integer ub)
{
  struct symmcum_data *pdata = fd.gdata;

  if (lb>ub || (lb>EST(t) && ub<LST(t)))
    return TRUE;		/* would cut a hole, at most */

  switch (dvar_prune_interval_l(DVHEIGHT(t),lb,ub)) {
  case -1:
    return FALSE;
  default:
    return TRUE;
  }
}

static SP_BOOL 
task_renormalize(Wam wam, TASK t)	/* origin + duration = end */
{
  struct symmcum_data *pdata = fd.gdata;
  for (;;) {
    SP_integer est = EST(t);
    SP_integer lst = LST(t);
    SP_integer ect = ECT(t);
    SP_integer lct = LCT(t);
    SP_integer mindur = MINDUR(t);
    SP_integer maxdur = MAXDUR(t);
    SP_integer lb1 = ect-maxdur;
    SP_integer ub1 = lct-mindur;
    SP_integer lb2 = ect-lst;
    SP_integer ub2 = lct-est;
    SP_integer lb3 = est+mindur;
    SP_integer ub3 = lst+maxdur;

    if (!(est<lb1 || lst>ub1 ||
	  mindur<lb2 || maxdur>ub2 ||
	  ect<lb3 || lct>ub3))
      return TRUE;
    
    /* origin in min(end)-max(dur) ... max(end)-min(dur) */
    if (dvar_fix_interval_l(DVORG(t), lb1, ub1)<0)
      return FALSE;
  
  /* dur in min(end)-max(origin) ... max(end)-min(origin) */
    if (dvar_fix_interval_l(DVDUR(t), lb2, ub2)<0)
      return FALSE;
  
  /* end in min(origin)+min(dur) ... max(origin)-max(dur) */
    if (dvar_fix_interval_l(DVEND(t), lb3, ub3)<0)
      return FALSE;
  }
}



static SP_BOOL 
prune_constrained_task(Wam wam,
		       TASK t, /* the task */
		       MACHINE r, /* virtual machine */
		       SP_integer low, SP_integer up, /* compulsory part */
		       SP_integer contrib_t	/* contribution of t in profile */
		       )
{
  struct symmcum_data *pdata = fd.gdata;
  SP_integer optval = pdata->profile - contrib_t;

  /* if there would be a problem if task t overlaps low..up
     (no need to prune min height since max would already cause problem) */
  
  if ((pdata->options&OPT_ATMOST)
      ? optval+HTASK(t)>CAPACITY(r)
      : optval+HTASK(t)<CAPACITY(r)) {

    /* if task t overlaps low..up then forbid assignment on r */

    if (MUST_OVERLAP(t,low,up)) {
      return dvar_prune_value_l(DVMACH(t),REAL(r))>=0;
    } else {
      SP_integer maxd, l2;

      maxd = low-EST(t);
      l2 = LCT(t)-up;
      if (maxd<l2)
	maxd = l2;
      if (maxd<0)
	maxd = 0;

      /* if task t assigned on r then forbid overlap with low..up */

      if (FIXEDMACH(t)) {
	if (MINDUR(t)>0 &&
	    !(prune_origin_interval(wam, t, low-MINDUR(t)+1, up-1) &&
	      prune_end_interval(wam, t, low+1, up-1+MINDUR(t))))
	  return FALSE;
	return
	  dvar_fix_interval_l(DVDUR(t), MINDUR(t), maxd)>=0;
      } else if (SIZE_(t)>1) {

	/* virtual pruning for the generalization */

	if (MINDUR(t)>0) {
	  cd_update(wam, t,r,PRUNED_ORIGIN,
		    &ORIGIN_PROFILE(t), &ORIGIN_TEMPO(t),&ORIGIN_STAMP(t),
		    low-MINDUR(t)+1,up-1);
	  cd_update(wam, t,r,PRUNED_END,
		    &END_PROFILE(t), &END_TEMPO(t),&END_STAMP(t),
		    low+1,up+MINDUR(t)-1);
	}
	cd_update(wam, t,r,PRUNED_DURATION,
		  &DURATION_PROFILE(t), &DURATION_TEMPO(t),&DURATION_STAMP(t),
		  maxd+1,MAXDUR(t));
      }
    }
  }
  
  return TRUE;
}




static SP_BOOL 
prune_unconstrained_task(Wam wam,
				TASK t, /* the task */
				MACHINE r, /* virtual machine */
				SP_integer low, SP_integer up, /* compulsory part */
				SP_integer contrib_t	/* contribution of t in profile */
				)
{
  struct symmcum_data *pdata = fd.gdata;
  SP_integer optval = pdata->profile - contrib_t;
  SP_integer mind, l2;

  /* exit if do not need absolutely task t on interval low..up */

  if (pdata->nconstrained==0 ||
      ((pdata->options&OPT_ATMOST) ? optval<=CAPACITY(r) : optval>=CAPACITY(r)))
    return TRUE;

  /* assign task t to machine r */

  if (dvar_fix_value_l(DVMACH(t),REAL(r))<0)
    return FALSE;

  /* prune origin of t to enforce task t to overlap low..up */

  if (dvar_fix_interval_l(DVORG(t), up-MAXDUR(t), low)<0)
    return FALSE;

  /* prune end of t to enforce task t to overlap low..up */

  if (dvar_fix_interval_l(DVEND(t), up, low+MAXDUR(t))<0)
    return FALSE;

  /* prune duration of t to enforce task t to overlap low..up */

  mind = up-LST(t);
  l2 = ECT(t)-low;
  if (mind>l2)
    mind = l2;
  if (dvar_fix_interval_l(DVDUR(t), mind, MAXDUR(t))<0)
    return FALSE;

  return TRUE;    
}


static SP_BOOL
prune_generalize(Wam wam,
		 TASK t, SP_integer size, PROFILE p,
		 SP_BOOL (*fun)(Wam, TASK, SP_integer, SP_integer))
{
  SP_BOOL rc = TRUE;
  SP_integer low, up, h;

  while (p && rc) {
    fd_profile_next(p,&low,&up,&h,&p);
    if (h==size) {
      rc = (*fun)(wam, t,low,up-1);
    }
  }
  return rc;
}



static SP_BOOL 
check_resource(Wam wam,
	       MACHINE r, /* virtual machine */
	       SP_integer low, SP_integer up /* compulsory part */
	       )
{
  struct symmcum_data *pdata = fd.gdata;
  SP_integer status = pdata->profile;
  SP_integer capr = CAPACITY(r);
  TAGGED trr = MakeSmall(REAL(r));
  int i, newi;

  if (pdata->nconstrained>0)
    if ((pdata->options&OPT_ATMOST) ? status>capr : status<capr)
      return FALSE;

  /* scan the stack of tasks and compress the stack */

  newi = 0;
  for (i=0; i<pdata->top_prune; i++) {
    TASK t = pdata->stack_prune[i];

    if (LCT(t)>up)
      pdata->stack_prune[newi++] = t;	/* keep t for the next check */
    if (dvar_contains_value_t(DVMACH(t),trr)) {
      SP_integer h = ((pdata->options&OPT_ATMOST) ? MINHEIGHT(t) : MAXHEIGHT(t));
      SP_integer contrib_t = (M_STATUS(t)==r ? HTASK(t) : 0);

      /* if t is an unconstrained or a reserve task */
      
      if (((pdata->options&OPT_ATMOST) ? h<=0 : h>=0) &&
	  !prune_unconstrained_task(wam, t,r,low,up,contrib_t))
	return FALSE;
      
      if (dvar_contains_value_t(DVMACH(t),trr)) { /* currently redundant test */

	/* if t is a constrained or a reserve task */

	if (((pdata->options&OPT_ATMOST) ? (h>0 || h>capr) : (h<0 || h<capr)) &&
	    !prune_constrained_task(wam, t,r,low,up,contrib_t))
	  return FALSE;

	if (dvar_contains_value_t(DVMACH(t),trr)) { /* NECESSARY test */

	  /* if necessary, prune the resource consumption of task t */

	  if (!prune_resource_consumption(wam, t,r,low,up,contrib_t))
	    return FALSE;
	}
      }
    }
  }
  pdata->top_prune = newi;
  return TRUE;
}


/*** support for frontier-based pruning ***/

static void
update_status_of_frontier_tasks(Wam wam,
				       MACHINE vr,
				       SP_integer low,SP_integer up,
				       int status)
{
  struct symmcum_data *pdata = fd.gdata;
  TASK t = pdata->first_sweep_frontier[vr];
  TASK previous_t = NOTASK;
  TASK next_t;
  SP_BOOL remove_t;

  while (t != NOTASK) {
    next_t = pdata->next_sweep_frontier[t];
    remove_t = FALSE;

    /* if frontier of task t intersects low..up
       then update status of task t if necessary */

    if (pdata->end_frontier[t]>=low && pdata->start_frontier[t]<up) {
      if (pdata->init_status_frontier[t]==0 ||
	  ((pdata->options&OPT_ATMOST)
	   ? pdata->best_status_frontier[t]>status
	   : pdata->best_status_frontier[t]<status)) {

	/* if margin in height is too big,
	   then can't prune according to frontier of task t;
	   so remove task t from the list of tasks
	   for which the status must be updated,
	   and don't transfer t to the list of tasks
	   according to which we will prune */

	if ((pdata->options&OPT_ATMOST) 
	    ? status+MINHEIGHT(t)+pdata->max_min_height <= CAPACITY(vr)
	    : status+MAXHEIGHT(t)+pdata->min_max_height >= CAPACITY(vr)) {
	  remove_t = TRUE;
	} else {

	  /* if margin in height is not too big, then update status */

	  pdata->best_status_frontier[t] = status;
	  pdata->init_status_frontier[t] = 1;
	}
      }
    }

    /* if end of frontier of task t is before up, then transfer task t
       FROM the list of tasks for which the status must be updated
       TO the list of tasks according to which we will prune */

    if (!remove_t && pdata->end_frontier[t]<up) {
      remove_t = TRUE;
      pdata->next_prune_frontier[t] = pdata->first_prune_frontier[vr];
      pdata->first_prune_frontier[vr] = t;
    }

    /* remove t if necessary and get next task */

    if (remove_t) {
      if (previous_t==NOTASK)
	pdata->first_sweep_frontier[vr] = next_t;
      else
	pdata->next_sweep_frontier[previous_t] = next_t;
    } else {
      previous_t = t;
    }
    t = next_t;
  }
}


static SP_BOOL
try_to_prune_according_to_frontier(Wam wam, MACHINE vr)
{
  struct symmcum_data *pdata = fd.gdata;
  TASK t, t2;
  SP_integer start, end, size, delta;
  SP_integer min_start, max_end, min_size, best_delta;

  /* exit if no task with a small frontier according to which to prune */

  t = pdata->first_prune_frontier[vr];
  if (t==NOTASK)
    return TRUE;

  /* compute common characteristics of all the frontiers
     according to which we will try to prune (for filtering purpose):
     - min_start:  EST of the frontier parts
     - max_end:    LCT of the frontier parts
     - min_size:   size of the smallest frontier
     - best_delta: (cum_atmost ? smallest : largest) height
                   which can lead to some pruning */

  min_start = CLPFD_MAXINT;
  max_end = -CLPFD_MAXINT;
  min_size = CLPFD_MAXINT;
  best_delta = (pdata->options&OPT_ATMOST) ? CLPFD_MAXINT : -CLPFD_MAXINT;
  while (t!=NOTASK) {
    start = pdata->start_frontier[t];
    end = pdata->end_frontier[t];
    size = end-start+1;
    if ((pdata->options&OPT_ATMOST))
      delta = CAPACITY(vr)-pdata->best_status_frontier[t]-MINHEIGHT(t);
    else
      delta = CAPACITY(vr)-pdata->best_status_frontier[t]-MAXHEIGHT(t);
    if (min_start>start)
      min_start = start;
    if (max_end<end)
      max_end = end;
    if (min_size>size)
      min_size = size;
    if (best_delta>((pdata->options&OPT_ATMOST) ? delta : -delta))
      best_delta = delta;
    t = pdata->next_prune_frontier[t];
  }

  /* pruning loop thru the list of candidates to prune or machine vr */

  t2 = pdata->first_check_frontier[vr];
  while (t2!=NOTASK) {

    /* make a first filter in order to have a chance that task t2
       can be pruned according to one of the small frontiers */

    if (LCT(t2)>min_start &&	/* task t2 interferes in time with frontiers */
	EST(t2)<=max_end &&	/* smallest frontier can be covered by task t2 */
	MAXDUR(t2)>=min_size &&	/* height of task t2 can cause a problem
				   for at least one frontier */
	((pdata->options&OPT_ATMOST)
	 ? MINHEIGHT(t2)>best_delta
	 : MAXHEIGHT(t2)<best_delta)) {

      /* scan the different frontiers in order to prune task t2
	 according to these different frontier parts */

      t = pdata->first_prune_frontier[vr];
      while (t!=NOTASK) {
	if (t==t2)
	  goto next;
	start = pdata->start_frontier[t];
	end = pdata->end_frontier[t];
	best_delta = pdata->best_status_frontier[t];

	/* if task t2 has no compulsory part
	   or has a compulsory part that ends before start of frontier of t
	   or has a compulsory part that starts after end of frontier of t
	   (no interference of task t2 with best_status_frontier[t]) */

	if (!HAS_COMPULSORY_PART(t2) ||
	    (HAS_COMPULSORY_PART(t2) && ECT(t2)-1 < start) ||
	    (HAS_COMPULSORY_PART(t2) && LST(t2)   > end) ||
	    ((pdata->options&OPT_ATMOST)
	     ? (pdata->min_min_height>=0 && MINHEIGHT(t)+MINHEIGHT(t2)>CAPACITY(vr))
	     : (pdata->max_max_height<=0 && MAXHEIGHT(t)+MAXHEIGHT(t2)<CAPACITY(vr)))) {

	  /* if there is a capacity overflow or underflow
	     then forbid task t2 to cover completely the frontier of task t */

	  if ((pdata->options&OPT_ATMOST)
	      ? best_delta+MINHEIGHT(t)+MINHEIGHT(t2) > CAPACITY(vr)
	      : best_delta+MAXHEIGHT(t)+MAXHEIGHT(t2) < CAPACITY(vr)) {

	    /* try to prune origin and end attributes of task t2 */

	    if (MINDUR(t2)>=end-start+1)
	      if (!prune_origin_interval(wam, t2, end+1-MINDUR(t2), start) ||
		  !prune_end_interval(wam, t2, end+1, start+MINDUR(t2)))
		return FALSE;

	    /* try to prune maximum duration of task t2 */

	    if (MAXDUR(t2)>=end-start+1 && EST(t2)<=start && LCT(t2)>=end+1) {
	      SP_integer maxd = end-EST(t2);
	      SP_integer l2 = LCT(t2)-start-1;

	      if (maxd<l2)
		maxd = l2;
	      if (maxd<0)
		maxd = 0;
	      if (dvar_fix_interval_l(DVDUR(t2), MINDUR(t2), maxd)<0)
		return FALSE;
	    }
	  }
	}
      next:
	t = pdata->next_prune_frontier[t];
      }
    }
    t2 = pdata->next_check_frontier[t2]; /* get next candidate to prune */
  }
  return TRUE;  
}



/*** support for scanning multiple event lists ***/

static int
get_first_event(Wam wam,
		       int maxevnc, MACHINE vr)
{
  struct symmcum_data *pdata = fd.gdata;
  int jnc = -1;
  TAGGED trr = MakeSmall(REAL(vr));
  EVENT evnc;
  
  do {
    jnc++;
    evnc = jnc==maxevnc ? NOEVENT : pdata->eventqnc[jnc];
  } while (evnc!=NOEVENT && !fd_member(trr,(TAGGED)EV_MACHINE(evnc)));
  return jnc;
}




/* precond: either *jcp == firstnonvr or *jcp is a valid event for vr,
   where firstnonvr is the first event index that is invalid for vr */
/* precond: either *jncp == maxevnc or *jncp is a valid event for vr */
/* We must process PROFILE events before PRUNING events.
   Only non-compulsory events can be PRUNING events.
   Hence, break ties (same date) by choosing compulsory events. */
static EVENT
get_next_event(Wam wam,
		      int *jcp,
		      int maxevc,
		      int *jncp,
		      int maxevnc,
		      MACHINE vr)
{
  struct symmcum_data *pdata = fd.gdata;
  EVENT evc, evnc, evnc1;
  TAGGED trr = MakeSmall(REAL(vr));
  int jc = *jcp;
  int jnc = *jncp;

  evc = jc==maxevc ? NOEVENT : pdata->eventqc[jc];
  if (evc!=NOEVENT && EV_MACHINE(evc)!=vr)
    evc = NOEVENT;
  evnc = jnc==maxevnc ? NOEVENT : pdata->eventqnc[jnc];

  if (evc!=NOEVENT && (evnc==NOEVENT || EV_DATE(evc)<=EV_DATE(evnc))) {
    *jcp = jc+1;
    return evc;
  } else if (evnc!=NOEVENT) {
    do {
      jnc++;
      evnc1 = jnc==maxevnc ? NOEVENT : pdata->eventqnc[jnc];
    } while (evnc1!=NOEVENT && !fd_member(trr,(TAGGED)EV_MACHINE(evnc1)));
    *jncp = jnc;
    return evnc;
  } else {
    return NOEVENT;
  }
}


static void
add_compulsory_events(Wam wam,
			     SP_integer from, SP_integer to,
			     TASK t, SP_integer h, MACHINE vr, int flags,
			     SP_integer low, SP_integer up, int *jcp, EVENT *evp)
{
  struct symmcum_data *pdata = fd.gdata;
  int jc = *jcp;
  EVENT ev = *evp;

  if (from<low)
    from = low;
  EV_DATE(ev) = from;
  EV_TASK(ev) = t;
  EV_HEIGHT(ev) = h;
  EV_MACHINE(ev) = vr;
  EV_FLAGS(ev) = flags|EV_START|EV_COMPULSORY_PART;
  pdata->eventqc[jc++] = ev++;
      
  if (to<up) {
    EV_DATE(ev) = to;
    EV_TASK(ev) = t;
    EV_HEIGHT(ev) = h;
    EV_MACHINE(ev) = vr;
    EV_FLAGS(ev) = flags|EV_END|EV_COMPULSORY_PART;
    pdata->eventqc[jc++] = ev++;
  }

  *jcp = jc;
  *evp = ev;
}


static void
add_non_compulsory_events(Wam wam,
			  SP_integer from, SP_integer to, TASK t, int flags,
			  SP_integer low, SP_integer up, int *jncp, EVENT *evp)
{
  struct symmcum_data *pdata = fd.gdata;
  int jnc = *jncp;
  EVENT ev = *evp;
  SP_integer h = HTASK(t);
  
  if (from<low)
    from = low;
  EV_DATE(ev) = from;
  EV_TASK(ev) = t;
  EV_HEIGHT(ev) = h;
  EV_MACHINE(ev) = SETMACH(t);
  EV_FLAGS(ev) = flags|EV_START;
  pdata->eventqnc[jnc++] = ev++;
      
  if (to<up) {
    EV_DATE(ev) = to;
    EV_TASK(ev) = t;
    EV_HEIGHT(ev) = h;
    EV_MACHINE(ev) = SETMACH(t);
    EV_FLAGS(ev) = flags|EV_END;
    pdata->eventqnc[jnc++] = ev++;
  }

  *jncp = jnc;
  *evp = ev;
}


static SP_BOOL
check_interval(Wam wam, SP_integer low, SP_integer up)
{
  struct symmcum_data *pdata = fd.gdata;
  int i, jc, jnc, maxevc, maxevnc, nfiltered=0, rc=FALSE;
  SP_integer d;
  MACHINE vr;
  EVENT ev;
  TAGGED trr;
  int k, sup, iend, mid;
  int ntasks = pdata->ntasks;
  int nmachines = pdata->nmachines;
  TAGGED comp_ev_machines = EmptySet;
  FDITER it;

  if (low>=up)
    return TRUE;

  /* allocate volatile arrays */

  pdata->filtered = Malloc(ntasks,TASK);
  pdata->eventqc = Malloc(4*ntasks,EVENT);
  pdata->eventqnc = Malloc(8*ntasks,EVENT);
  pdata->stack_prune = Malloc(ntasks,TASK);
  pdata->size_ = Malloc(ntasks,SP_integer);
  if (pdata->options&OPT_GENERALIZATION) {
    pdata->origin_profile = Malloc(ntasks,PROFILE);
    pdata->end_profile = Malloc(ntasks,PROFILE);
    pdata->duration_profile = Malloc(ntasks,PROFILE);
    pdata->height_profile = Malloc(ntasks,PROFILE);
    pdata->origin_tempo = Malloc(ntasks,PROFILE);
    pdata->end_tempo = Malloc(ntasks,PROFILE);
    pdata->duration_tempo = Malloc(ntasks,PROFILE);
    pdata->height_tempo = Malloc(ntasks,PROFILE);
    pdata->origin_stamp = Malloc(ntasks,SP_integer);
    pdata->end_stamp = Malloc(ntasks,SP_integer);
    pdata->duration_stamp = Malloc(ntasks,SP_integer);
    pdata->height_stamp = Malloc(ntasks,SP_integer);
  }
  pdata->start_frontier = Malloc(ntasks,SP_integer);
  pdata->end_frontier = Malloc(ntasks,SP_integer);
  pdata->first_sweep_frontier = Malloc(nmachines,TASK);
  pdata->next_sweep_frontier = Malloc(ntasks,TASK);
  pdata->first_prune_frontier = Malloc(nmachines,TASK);
  pdata->next_prune_frontier = Malloc(ntasks,TASK);
  pdata->first_check_frontier = Malloc(nmachines,TASK);
  pdata->next_check_frontier = Malloc(ntasks,TASK);
  pdata->init_status_frontier = Malloc(ntasks,SP_integer);
  pdata->best_status_frontier = Malloc(ntasks,SP_integer);
  pdata->event.date = Malloc(10*ntasks,SP_integer);
  pdata->event.task = Malloc(10*ntasks,TASK);
  pdata->event.height = Malloc(10*ntasks,SP_integer);
  pdata->event.machine = Malloc(10*ntasks,MACHINE);
  pdata->event.flags = Malloc(10*ntasks,unsigned int);

  /* record tasks that may intersect at least one point of low..up
     (candidates for potential pruning) */

  for (k=0; k<2; k++) {

    /* dichotomic search for min i such that LCTKEY(TASK(i)) > low 
       and min iend such that EST(TASK(j | j>=i)) >= up */

    if (k==0) {
      i = 0;
      sup = iend = pdata->ntargets;
      while (i<sup) {
	mid = (i+sup)>>1;
	if (LCTKEY(TASK(mid))<=low)
	  i = mid+1;
	else
	  sup = mid;
      }
    } else {
      i = pdata->ntargets;
      iend = i + pdata->nsources;
    }


    while (i<iend && MINEST(i)<up) {
      TASK t = TASK(i++);

      if ((FLAGS(t)&STATUS_SOURCE) &&
	  MAY_OVERLAP(t,low,up)) {
	TAGGED setmach = SETMACH(t);
	
	FILTERED(nfiltered++) = t;
	comp_ev_machines = fd_merge_into(wam, setmach,comp_ev_machines);
	SIZE_(t) = 1;
	if ((pdata->options&OPT_GENERALIZATION) && (FLAGS(t)&STATUS_TARGET)) {
	  if ((SIZE_(t) = SIZEMACH(t)) > 1) {
	    ORIGIN_PROFILE(t) = fd_empty_profile();
	    END_PROFILE(t) = fd_empty_profile();
	    DURATION_PROFILE(t) = fd_empty_profile();
	    HEIGHT_PROFILE(t) = fd_empty_profile();
	  } else {
	    ORIGIN_PROFILE(t) = NULL;
	    END_PROFILE(t) = NULL;
	    DURATION_PROFILE(t) = NULL;
	    HEIGHT_PROFILE(t) = NULL;
	  }
	  ORIGIN_TEMPO(t) = NOPROFILE;
	  END_TEMPO(t) = NOPROFILE;
	  DURATION_TEMPO(t) = NOPROFILE;
	  HEIGHT_TEMPO(t) = NOPROFILE;
	  ORIGIN_STAMP(t) = NOMACHINE;
	  END_STAMP(t) = NOMACHINE;
	  DURATION_STAMP(t) = NOMACHINE;
	  HEIGHT_STAMP(t) = NOMACHINE;
	}
      }
    }
  }

  for (vr=nmachines-1; vr>=0; vr--) {
    pdata->first_sweep_frontier[vr] = NOTASK;
    pdata->first_prune_frontier[vr] = NOTASK;
    pdata->first_check_frontier[vr] = NOTASK;
  }

  pdata->generalize_nvar = 0;
  if (pdata->options&OPT_GENERALIZATION) {
    pdata->generalize_tempo_marray = Malloc(4*nfiltered,PROFILE *);
    pdata->generalize_profile_marray = Malloc(4*nfiltered,PROFILE *);
    pdata->generalize_itask = Malloc(4*nfiltered,TASK);
    pdata->generalize_dvar = Malloc(4*nfiltered,int);
  }

  /* initialize the event queue with all start and end events */

  jc = 0;
  jnc = 0;
  ev = 0;
  for (i=0; i<nfiltered; i++) {
    TASK t = FILTERED(i);
    SP_integer h = ((pdata->options&OPT_ATMOST) ? MINHEIGHT(t) : MAXHEIGHT(t));
    SP_BOOL assigned_t = (FIXEDMACH(t));
    SP_integer capa, capb;

    trr = MINMACH_T(t);
    vr = virtual_machine(wam, GetSmall(trr));
    M_STATUS(t) = NOMACHINE;
    HTASK(t) = h;

    /* generate pruning events for target tasks */

    if (t<=pdata->last_to_prune && (FLAGS(t)&STATUS_TARGET)) {
      add_non_compulsory_events(wam, EST(t),LCT(t),t,
				EV_TYPE_PRUNING|EV_CHECK_NONE,
				low,up,&jnc,&ev);
    }

    if (assigned_t) {
      capa = capb = CAPACITY(vr);
    } else if ((pdata->options&OPT_ATMOST)) {
      capa = pdata->capacity_max;
      capb = pdata->capacity_min;
    } else {
      capa = pdata->capacity_min;
      capb = pdata->capacity_max;
    }

    /* if t is an unconstrained task on at least one machine */

    if ((pdata->options&OPT_ATMOST) ? (h<=0 && h<=capa) : (h>=0 && h>=capa)) {
      int chk = (assigned_t ? EV_CHECK_NONE : EV_CHECK_GE);
      
      /* generate an event for EST(t)..LCT(t) */
      
      add_non_compulsory_events(wam, EST(t),LCT(t),t,
				EV_TYPE_PROFILE|chk,
				low,up,&jnc,&ev);
    } else

      /* if t is a constrained task that is assigned to a machine,
	 insert i in the list of tasks subject to frontier-based pruning */

      if (assigned_t && ((pdata->options&OPT_ATMOST) ? h>0 : h<0)) {
	pdata->next_check_frontier[t] = pdata->first_check_frontier[vr];
	pdata->first_check_frontier[vr] = t;

	/* if t definitely intersects low..up */

	if (MUST_OVERLAP(t,low,up)) {

	  /* if t has a compulsory part */

	  if (HAS_COMPULSORY_PART(t)) {
	    add_compulsory_events(wam, LST(t),ECT(t),t,HTASK(t),vr,
				  EV_TYPE_PROFILE|EV_CHECK_NONE,
				  low,up,&jc,&ev);
	  } else

	    /* if t has a not too big frontier part
	       (which is included in low..up, already verified) */

	    if (LST(t)-ECT(t)+2 <= pdata->max_duration) {
	      pdata->start_frontier[t] = ECT(t)-1;
	      pdata->end_frontier[t] = LST(t);
	      pdata->next_sweep_frontier[t] = pdata->first_sweep_frontier[vr];
	      pdata->first_sweep_frontier[vr] = t;
	      pdata->first_prune_frontier[vr] = NOTASK;
	      pdata->init_status_frontier[t] = 0;

	    }
	}
      }

    /* if t may be a reserve task */

    if ((pdata->options&OPT_ATMOST) ? (h<=0 && h>capb) : (h>=0 && h<capb)) {
      int chk = (assigned_t ? EV_CHECK_NONE : EV_CHECK_LT);

      /* if t is assigned to a machine and has a compulsory part
	 which intersects low..up */

      if (assigned_t && HAS_COMPULSORY_PART(t) && MUST_OVERLAP(t,low,up))
	add_compulsory_events(wam, LST(t),ECT(t),t,0,vr,
			      EV_TYPE_PROFILE|EV_CHECK_NONE,
			      low,up,&jc,&ev);
	
      add_non_compulsory_events(wam, EST(t),LCT(t),t,
				EV_TYPE_PROFILE|chk,
				low,up,&jnc,&ev);
    }
  }
  
  /* sort the different lists of events: compulsory events associated
     to each resource, and non-compulsory events */

  pdata->nfiltered = nfiltered;
  maxevc = jc;
  maxevnc = jnc;
  qsort_asc_eventc(wam, pdata->eventqc, maxevc);
  qsort_asc_eventnc(wam, pdata->eventqnc, maxevnc);

  /* build the cumulative profile resource by resource */

  jc = 0;
  fditer_init(&it, comp_ev_machines);
  while (!fditer_empty(&it)) {
    trr = fditer_next(&it);
    vr = virtual_machine(wam, GetSmall(trr));
    pdata->profile = 0;
    pdata->nconstrained = 0;
    pdata->top_prune = 0;
    d = low;
    jnc = get_first_event(wam, maxevnc,vr);
    ev = get_next_event(wam, &jc,maxevc,&jnc,maxevnc,vr);
    while (ev!=NOEVENT) {
      TASK t = EV_TASK(ev);
      unsigned int flags = EV_FLAGS(ev);
      SP_integer nextd = EV_DATE(ev);

      if (flags & EV_TYPE_PRUNING) {
	if (flags & EV_START)
	  pdata->stack_prune[pdata->top_prune++] = t;
      } else {
	SP_integer h = EV_HEIGHT(ev);
	int inc = ((flags & EV_START) ? 1 : -1);
	
	if (nextd>d) {
	  if (!check_resource(wam, vr,d,nextd))
	    goto ret;
	  update_status_of_frontier_tasks(wam, vr,d,nextd,(int)pdata->profile);
	  d = nextd;
	}
	if ((flags & EV_CHECK_NONE) ||
	    ((flags & EV_CHECK_GE) && !OVERFLOWS(h,vr)) ||
	    ((flags & EV_CHECK_LT) && OVERFLOWS(h,vr))) {
	  pdata->profile += h*inc;
	  if (flags & EV_COMPULSORY_PART)
	    pdata->nconstrained += inc;
	  M_STATUS(t) = (flags & EV_START) ? vr : NOMACHINE;
	}
      }
      ev = get_next_event(wam, &jc,maxevc,&jnc,maxevnc,vr);
    }
    if (!check_resource(wam, vr,d,up))
      goto ret;
    update_status_of_frontier_tasks(wam, vr,d,up,(int)pdata->profile);

    /* Transfer the temporary marrays that were modified during the current
       sweep of the events of resource vr to the profile marray
       associated to each variable we try to prune 
       (for all nonzero values of marray_source, add 1 to marray_destination)
       If empty domain after sweep on a resource, prune machine attribute.
    */

    for (i=pdata->generalize_nvar-1; i>=0; --i) {
      PROFILE src  = *pdata->generalize_tempo_marray[i];
      PROFILE *dest = pdata->generalize_profile_marray[i];
      TASK t       = pdata->generalize_itask[i];
      int dvar     = pdata->generalize_dvar[i];
      TAGGED fdset=0;
      SP_integer b, e, h;

      switch (dvar) {
      case PRUNED_ORIGIN:
	fdset = dvar_set(DVORG(t));
	break;
      case PRUNED_END:
	fdset = dvar_set(DVEND(t));
	break;
      case PRUNED_DURATION:
	fdset = dvar_set(DVDUR(t));
	break;
      case PRUNED_HEIGHT:
	fdset = dvar_set(DVHEIGHT(t));
	break;
      }

      while (fd_profile_next(src,&b,&e,&h,&src)) {
	*dest = fd_profile_update(wam, *dest,b,e,1);
	fdset = fd_subtract_interval(wam, fdset,MakeSmall(b),MakeSmall(e-1));
      }

      if (fdset==EmptySet && dvar_contains_value_t(DVMACH(t),trr)) {
	if (dvar_prune_value_t(DVMACH(t),trr)<0)
	  goto ret;
      }
    }
    pdata->generalize_nvar = 0;

    /* try to prune according to the "small" frontier part
       of the tasks assigned to machine vr */

    if (!try_to_prune_according_to_frontier(wam, vr))
      goto ret;
  }
  
  for (i=0; i<nfiltered; i++) {
    TASK t = FILTERED(i);
    SP_integer size = SIZE_(t);

    if ((FLAGS(t) & STATUS_TARGET) && size>1) {
      if (!prune_generalize(wam, t,size,ORIGIN_PROFILE(t),(prune_origin_interval)) ||
	  !prune_generalize(wam, t,size,END_PROFILE(t),(prune_end_interval)) ||
	  !prune_generalize(wam, t,size,DURATION_PROFILE(t),(prune_duration_interval)) ||
	  !prune_generalize(wam, t,size,HEIGHT_PROFILE(t),(prune_height_interval)))
	goto ret;
    }
  }
  rc = TRUE;
 ret:
  if (pdata->options&OPT_GENERALIZATION)
    for (i=0; i<nfiltered; i++) {
      TASK t = FILTERED(i);

      if (FLAGS(t) & STATUS_TARGET) {
	if (SIZE_(t)>1) {
	  fd_profile_dispose(wam, ORIGIN_PROFILE(t));
	  fd_profile_dispose(wam, END_PROFILE(t));
	  fd_profile_dispose(wam, DURATION_PROFILE(t));
	  fd_profile_dispose(wam, HEIGHT_PROFILE(t));
	}
	if (ORIGIN_TEMPO(t)!=NOPROFILE)
	  fd_profile_dispose(wam, ORIGIN_TEMPO(t));
	if (END_TEMPO(t)!=NOPROFILE)
	  fd_profile_dispose(wam, END_TEMPO(t));
	if (DURATION_TEMPO(t)!=NOPROFILE)
	  fd_profile_dispose(wam, DURATION_TEMPO(t));
	if (HEIGHT_TEMPO(t)!=NOPROFILE)
	  fd_profile_dispose(wam, HEIGHT_TEMPO(t));
      }
    }
  Free(pdata->filtered);
  Free(pdata->eventqc);
  Free(pdata->eventqnc);
  Free(pdata->stack_prune);
  Free(pdata->size_);
  if (pdata->options&OPT_GENERALIZATION) {
    Free(pdata->origin_profile);
    Free(pdata->end_profile);
    Free(pdata->duration_profile);
    Free(pdata->height_profile);
    Free(pdata->origin_tempo);
    Free(pdata->end_tempo);
    Free(pdata->duration_tempo);
    Free(pdata->height_tempo);
    Free(pdata->origin_stamp);
    Free(pdata->end_stamp);
    Free(pdata->duration_stamp);
    Free(pdata->height_stamp);
    Free(pdata->generalize_tempo_marray);
    Free(pdata->generalize_profile_marray);
    Free(pdata->generalize_itask);
    Free(pdata->generalize_dvar);
  }
  Free(pdata->start_frontier);
  Free(pdata->end_frontier);
  Free(pdata->first_sweep_frontier);
  Free(pdata->next_sweep_frontier);
  Free(pdata->first_prune_frontier);
  Free(pdata->next_prune_frontier);
  Free(pdata->first_check_frontier);
  Free(pdata->next_check_frontier);
  Free(pdata->init_status_frontier);
  Free(pdata->best_status_frontier);
  Free(pdata->event.date);
  Free(pdata->event.task);
  Free(pdata->event.height);
  Free(pdata->event.machine);
  Free(pdata->event.flags);
  return rc;
}


/*** support for task intervals ***/

static SP_integer 
ti_min_length_intersection(Wam wam, SP_integer low,SP_integer up,TASK t)
{
  struct symmcum_data *pdata = fd.gdata;
  SP_integer j1, j2, j3, j4;

  j1 = MINDUR(t);
  j2 = up-low;
  j3 = ECT(t)-low;
  j4 = up-LST(t);
  if (j1>j2)
    j1 = j2;
  if (j1>j3)
    j1 = j3;
  if (j1>j4)
    j1 = j4;
  if (j1<0)
    j1 = 0;
  return j1;
}


static SP_integer 
ti_max_length_intersection(Wam wam, SP_integer low,SP_integer up,TASK t)
{
  struct symmcum_data *pdata = fd.gdata;
  SP_integer mino = EST(t);
  SP_integer maxo = LST(t);
  SP_integer maxd = MAXDUR(t);
  SP_integer max_inter, start_max_inter, end_max_inter, min, max;
  TAGGED setorg;

  if (maxo<low-maxd+1 || mino>=up || maxd==0)
    return 0;
  if (up-low>maxd) {
    max_inter = maxd;
    start_max_inter = low;
  } else {
    max_inter = up-low;
    start_max_inter = up-maxd;
  }
  if (maxo<start_max_inter)
    return max_inter - (start_max_inter-maxo);
  end_max_inter = up-max_inter;
  if (mino>end_max_inter)
    return max_inter - (mino-end_max_inter);

  min = FDMAX(start_max_inter,mino);
  max = FDMIN(end_max_inter,maxo);
  if (min<=max &&
      dvar_compare_interval_l(DVORG(t), min, max)!=FDI_DISJOINT)
    return max_inter;

  /* TODO: simplify this loop */
  setorg = SETORG(t);
  for (max_inter--;
       max_inter>0 &&
       !fd_member(MakeSmall(start_max_inter-1),setorg) &&
       !fd_member(MakeSmall(end_max_inter+1),setorg);
       max_inter--) {
    start_max_inter--;
    end_max_inter++;
  }
  return max_inter;
}



#define ti_min_use(low,up,r) (pdata->max_min_length_inter[r])

#define ti_max_use(low,up,r) FDMIN((up)-(low),pdata->sum_max_length_inter)

/* Input: Prune the attributes of the task t in order that the surface
   of the intersection of task t with interval low..up
   is at least equal to the value delta (> 0).
   If cum_atmost=1 then h corresponds to -MINHEIGHT(t), otherwise to
   MAXHEIGHT(t); in either case, h > 0.
   Output: An indication as to whether a solution may exist.
*/
static SP_BOOL
ti_force_to_cover_atleast(Wam wam,
			  SP_integer low,SP_integer up,
			  TASK t,SP_integer delta,SP_integer h)
{
  struct symmcum_data *pdata = fd.gdata;
  SP_integer maxd = MAXDUR(t);
  SP_integer T, limit, inter;

  /* Invariant: delta>0 and h>0 */
  /* Try to prune the origin, end and duration attributes of task t
     according to the required minimum intersection in time T
     of task t with interval low..up */

  T = (delta+h-1) / h;

  /* Restrict the range of the origin attribute of task t */

  if (dvar_fix_interval_l(DVORG(t), low+T-maxd, up-T)<0)
    return FALSE;

  /* Restrict the range of the end attribute of task t */

  if (dvar_fix_interval_l(DVEND(t), low+T, up-T+maxd)<0)
    return FALSE;

  /* Adjust the minimum duration of task t */

  if (dvar_fix_interval_l(DVDUR(t), T, maxd)<0 ||
      (LST(t)<low && dvar_fix_interval_l(DVDUR(t), low-LST(t)+T, maxd)<0) ||
      (ECT(t)>up  && dvar_fix_interval_l(DVDUR(t), ECT(t)-up+T, maxd)<0))
    return FALSE;

  /* Adjust the maximum or minimum height of task t */

  inter = ti_max_length_intersection(wam, low,up,t);
  if (inter==0)
    return FALSE;
  limit = (delta+inter-1) / inter;
  if ((pdata->options&OPT_ATMOST)
      ? dvar_fix_interval_l(DVHEIGHT(t), MINHEIGHT(t), -limit)<0
      : dvar_fix_interval_l(DVHEIGHT(t), limit, MAXHEIGHT(t))<0)
    return FALSE;

  return TRUE;
}


/* Input: Prune the attributes of the task t in order that the surface
   of the intersection of task t with interval low..up
   is at most equal to the value delta (>= 0).
   If cum_atmost=1 then h corresponds to MINHEIGHT(t), otherwise to
   -MAXHEIGHT(t); in either case, h > 0.
   Output: An indication as to whether a solution may exist.
*/
static SP_BOOL
ti_forbid_to_cover_more(Wam wam,
			SP_integer low,SP_integer up,
			TASK t,SP_integer delta,SP_integer h)
{
  struct symmcum_data *pdata = fd.gdata;
  SP_integer mind = MINDUR(t);
  SP_integer T, inter;

  /* Invariant: delta>=0 and h>0 */
  /* Try to prune the origin, end and duration attributes of task t
     according to the maximum possible intersection in time T-1
     of task t with interval low..up */

  T = (delta+h) / h;
  if (T<=up-low) {
    
    /* Adjust the origin and end attributes of task t */

    if (mind>=T) {
      if (!prune_origin_interval(wam, t, low+T-mind, up-T) ||
	  !prune_end_interval(wam, t, low+T, up-T+mind))
	return FALSE;
    }

    /* Adjust the maximum duration of task t */
    /* Reasoning based on the fact that overlap = max(0,min(CT-low,CT-ST=dur,up-low,up-ST)) */
    /* So prevent the conjunction:
       CT(t)-low >= T [check ECT(t)-low >= T]
       CT(t)-ST(t)=dur >= T [enforce if need be, check MAXDUR(t)>=T]
       up-low >= T [being assumed]
       up-ST(t) >= T [check LST<=up-T]
    */

    if (LST(t)<=up-T && ECT(t)>=low+T && MAXDUR(t)>=T) {
      if (dvar_fix_interval_l(DVDUR(t), mind, T-1)<0)
	return FALSE;
    }

#if 0 /* pre-4.4, can't rationalize this */
    if (EST(t)<=up-T && LCT(t)>=low+T && MAXDUR(t)>=T) {
      SP_integer limit = FDMAX(low+T-EST(t)-1,LCT(t)-up+T-1);

      if (limit<0)
	limit = 0;
      if (dvar_fix_interval_l(DVDUR(t), mind, limit)<0)
	return FALSE;
    }
#endif
  }

  /* Adjust the maximum or minimum height of task t */

  inter = ti_min_length_intersection(wam, low,up,t);
  if (inter>0) {
    SP_integer limit = delta / inter;    
    if ((pdata->options&OPT_ATMOST)
	? dvar_fix_interval_l(DVHEIGHT(t), MINHEIGHT(t), limit)<0
	: dvar_fix_interval_l(DVHEIGHT(t), -limit, MAXHEIGHT(t))<0)
      return FALSE;
  }
  
  return TRUE;
}



/* Input: an interval low..up and the machines machines for which we do the check.
   Output: An indication as to whether a solution may exist.
*/
static SP_BOOL
ti_task_interval(Wam wam,
		 SP_integer low,SP_integer up,TAGGED machines)
{
  struct symmcum_data *pdata = fd.gdata;
  int ntasks = pdata->ntasks;
  int nmachines = pdata->nmachines;
  int ntry_to_prune = 0;
  SP_integer sum_slack, slack, capr, sum_contribution, sum_contribution_i, h, inter, delta, absh;
  MACHINE vr;
  SP_BOOL result = FALSE;
  int i, k, sup, iend, mid;
  FDITER it;

  if (low>=up)
    return TRUE;

  /* Allocate temporary arrays. */

  pdata->sum_max_length_inter = 0;
  pdata->try_to_prune = Malloc(ntasks,TASK);
  pdata->include_in_machines = Malloc(ntasks,SP_BOOL);
  pdata->min_length_inter = Malloc(ntasks,SP_integer);
  pdata->max_length_inter = Malloc(ntasks,SP_integer);
  pdata->max_min_length_inter = Malloc(nmachines,SP_integer);
  pdata->contribution = Malloc(ntasks,SP_integer);
  pdata->hcontribution = Malloc(ntasks,SP_integer);

  /* Get tasks that interact with interval low..up and machines
     and compute required information. */

  for (vr=nmachines-1; vr>=0; vr--)
    pdata->max_min_length_inter[vr] = 0;

  for (k=0; k<2; k++) {


  /* dichotomic search for min i such that LCTKEY(TASK(i)) > low 
                       and min iend such that EST(TASK(j | j>=i)) >= up */

    if (k==0) {
      i = 0;
      sup = iend = pdata->ntargets;
      while (i<sup) {
	mid = (i+sup)>>1;
	if (LCTKEY(TASK(mid))<=low)
	  i = mid+1;
	else
	  sup = mid;
      }
    } else {
      i = pdata->ntargets;
      iend = i + pdata->nsources;
    }

    while (i<iend && MINEST(i)<up) {
      TASK t = TASK(i++);
      int overlap = fd_compare(SETMACH(t),machines);

      if ((FLAGS(t)&STATUS_SOURCE) &&
	  overlap != FDI_DISJOINT &&
	  MAY_OVERLAP(t,low,up)) {
	pdata->try_to_prune[ntry_to_prune++] = t;
	pdata->include_in_machines[t] = (overlap==FDI_SUBSET || overlap==FDI_EQUAL);
	pdata->min_length_inter[t] = ti_min_length_intersection(wam, low,up,t);
	pdata->max_length_inter[t] = ti_max_length_intersection(wam, low,up,t);
	if (FIXEDMACH(t)) {
	  vr = virtual_machine(wam, dvar_min_l(DVMACH(t)));
	  if (pdata->max_min_length_inter[vr]<pdata->min_length_inter[t])
	    pdata->max_min_length_inter[vr] = pdata->min_length_inter[t];
	}
	pdata->sum_max_length_inter += pdata->max_length_inter[t];
      }
    }
  }

  /* Compute sum of available slack on the different machines
     and on interval low..up */

  sum_slack = 0;
  fditer_init(&it, machines);
  while (!fditer_empty(&it)) {
    vr = virtual_machine(wam, GetSmall(fditer_next(&it)));
    capr = CAPACITY(vr);
    if ((pdata->options&OPT_ATMOST) == (capr>=0))
      slack = ti_max_use(low,up,vr) * capr;
    else
      slack = ti_min_use(low,up,vr) * capr;
    sum_slack += slack;
  }

  /* Compute sum of contributions of the tasks on interval low..up */

  sum_contribution = 0;
  for (i=0; i<ntry_to_prune; i++) {
    TASK t = pdata->try_to_prune[i];	/* MACHINE[t] intersects machines */
    
    h = ((pdata->options&OPT_ATMOST) ? MINHEIGHT(t) : MAXHEIGHT(t));
    pdata->hcontribution[t] = h;
    inter = 0;

    /* If task t can produce some free space then consume maximum free space */

    if ((pdata->options&OPT_ATMOST) ? h<=0 : h>=0)
      inter = pdata->max_length_inter[t];

    /* If task t consumes some available space then compute minimum used space */

    else if (pdata->include_in_machines[t])
      inter = pdata->min_length_inter[t];

    pdata->contribution[t] = inter*h;
    sum_contribution += pdata->contribution[t];
  }

  /* Fail of necessary condition does not hold */

  if ((pdata->options&OPT_ATMOST)
      ? sum_contribution>sum_slack
      : sum_contribution<sum_slack)
    goto ret;

  /* Try to prune the attributes of the tasks in order to try to avoid
     the necessary condition to be false */

  for (i=0; i<ntry_to_prune; i++) {
    TASK t = pdata->try_to_prune[i];

    if (FLAGS(t) & STATUS_TARGET) {
      h = pdata->hcontribution[t];
      absh = FDMAX(h,-h);
      sum_contribution_i = sum_contribution - pdata->contribution[t];
      delta = sum_contribution_i - sum_slack;
      if (delta<0)
	delta = -delta;

      /* If task t can produce some free space */

      if ((pdata->options&OPT_ATMOST) ? h<=0 : h>=0) {

	/* If this free space is absolutely needed then:
	   - force task t to be assigned on one of machines,
	   - force task t to cover a minimum surface */

	if ((pdata->options&OPT_ATMOST)
	    ? sum_contribution_i>sum_slack
	    : sum_contribution_i<sum_slack)
	  if (dvar_fix_set(DVMACH(t),machines)<0 ||
	      !ti_force_to_cover_atleast(wam, low,up,t,delta,absh))
	    goto ret;
      } else {
	if (pdata->include_in_machines[t]) {
	  if (!ti_forbid_to_cover_more(wam, low,up,t,delta,absh))
	    goto ret;
	} else {

	  /* If the minimum intersection is too important
	     then forbid to assign the task on one of machines
	     (since !include_in_machines[t], the contribution of task t
	     in sum_contribution was null) */

	  if ((pdata->options&OPT_ATMOST)
	       ? sum_contribution+pdata->min_length_inter[t]*h > sum_slack
	       : sum_contribution+pdata->min_length_inter[t]*h < sum_slack)
	    if (dvar_prune_set(DVMACH(t),machines)<0)
	      goto ret;
	}
      }

    }
  }

  /* Deallocate temporary arrays and exit */
  
  result = TRUE;
 ret:
  Free(pdata->try_to_prune);
  Free(pdata->include_in_machines);
  Free(pdata->min_length_inter);
  Free(pdata->max_length_inter);
  Free(pdata->max_min_length_inter);
  Free(pdata->contribution);
  Free(pdata->hcontribution);

  return result;
}


/* Compute for each i in [low,up): MINEST(i) = EST(TASK(i...up-1)) */
static void
build_minest(Wam wam, int low, int up)
{
  struct symmcum_data *pdata = fd.gdata;
  SP_integer min, est;
  int i;

  if (low<up) {
    min = EST(TASK(up-1));
    MINEST(up-1) = min;
    for (i=up-2; i>=low; i--) {
      est = EST(TASK(i));
      if (min>est)
	min = est;
      MINEST(i) = min;
    }
  }
}


/* Repartition all tasks into [ TARGET+SOURCE | SOURCE | 0 ] */
/* Update pdata->ntargets, pdata->nsources */
/* 3.9 invariant: target partition sorted by ascending lctkey */
/* 3.9 invariant: source partition sorted by ascending age (of becoming source) */
/* 3.9 invariant: 0 partition sorted by ascending age, older than source part */
static void
update_task_states(Wam wam, struct symmcum_data *pdata)
{
  TAGGED timepoints = EmptySet;	/* projection of targets on time axis */
  /* 3.9 TAGGED machines = EmptySet;  projection of targets on machine axis */
  int ntargets = pdata->ntargets;
  int nactive = ntargets + pdata->nsources;
  int i, delta1=0, delta2=0, inf, sup;
  TASK *target = pdata->target;
  TASK held, t;
  TAGGED lct;

  /* Reached fixpoint i.e. there was no pruning? */
  for (i=0; i<ntargets; i++) {
    TASK t = TASK(i);
    if (dvar_is_pruned(DVORG(t)) ||
	dvar_is_pruned(DVEND(t)) ||
	dvar_is_pruned(DVDUR(t)) ||
	dvar_is_pruned(DVHEIGHT(t)) ||
	dvar_is_pruned(DVMACH(t)))
      return;
  }

  /* Phase 1: filter targets */
  inf = 0;
  sup = ntargets-1;
  t = target[inf];
  held = target[sup];
  while (inf<=sup) {
    if (!dvar_is_integer(DVORG(t)) || /* not yet ground OR was pruned  */
	!dvar_is_integer(DVEND(t)) ||
	!dvar_is_integer(DVDUR(t)) ||
	!dvar_is_integer(DVHEIGHT(t)) ||
	!dvar_is_integer(DVMACH(t))) {
      if (EST_T(t)!=LCT_T(t)) {
	lct = pdata->initial_lct[t];
	timepoints = fd_union_interval(wam, timepoints,
				    pdata->initial_est[t],
				    FDdecr(lct));
      }
      /* 3.9 machines = fd_merge_into(wam, SETMACH(t),machines); */
      target[inf] = t;
      inf++;
      t = (inf>=sup ? held : target[inf]);
    } else {
      FLAGS(t) &= ~STATUS_TARGET;
      target[sup] = t;
      sup--;
      t = (inf>=sup ? held : target[sup]);
    }
  }

  delta1 = ntargets-inf;
  if (delta1>0) {
    ntargets -= delta1;
    pdata->ntargets -= delta1;
    pdata->nsources += delta1;
    qsort_asc_lctkey(wam, target, ntargets);
  }

  /* Phase 3: rebuild MINEST array */

  if (delta1>0 || delta2>0) {
    build_minest(wam, 0, ntargets);
    build_minest(wam, ntargets, nactive);
  }
}


/* '$fd_cumulatives'(+State0, -State, -Actions).

   State = f(NT,NM,Opt,Tasks,Machines,NTargets,NSources,_Handle,0)

*/
void SPCDECL 
prolog_fd_cumulatives(Wam wam,
		      SP_term_ref State0,
		      SP_term_ref State,
		      SP_term_ref Actions)
{
  TAGGED list, handle, tmp;
  int i, ntasks;
  int ent = -1;			/* disentailed unless otherwise */
  SP_BOOL committed;
  SP_integer total_size, absmin, absmax, maxdur, minmin, minmax, maxmin, maxmax;
  SP_integer state_stamp, machine_size;
  struct symmcum_data *pdata;
  char *ptr;
  int ntargets, nactive;

/*    X(0) = RefTerm(State0); */
  (void)State0;
  dvar_export_start(wam);
  fd_init_profile(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct symmcum_data,handle);
    fd.gdata = pdata;
    ntasks = pdata->ntasks;
  } else {			/* build persistent state */
    int nmachines;
				/* compute flags, ntasks, use_limit */
    DerefArg(tmp,X(0),1);		/* get ntasks */
    ntasks = GetSmall_int(tmp);

    /* ensure heap space for integer attributes */

    RequireHeap1(ntasks*5*INT_ATTRIBUTE_SIZE,handle,3);
    
    DerefArg(tmp,X(0),2);		/* get nmachines */
    nmachines = GetSmall_int(tmp);
    total_size =
      5*ntasks*sizeof(struct dvar) +
      (11*ntasks + 2*nmachines)*sizeof(SP_integer);
    pdata = Palloc(struct symmcum_data, total_size, handle);
    fd.gdata = pdata;
    ptr = (char *)(pdata+1);
    pdata->target = (TASK *)ptr;
    ptr = (char *)(pdata->target+2*ntasks);
    pdata->minest = (SP_integer *)ptr;
    ptr = (char *)(pdata->minest+ntasks);
    pdata->initial_est = (TAGGED *)ptr;
    ptr = (char *)(pdata->initial_est+ntasks);
    pdata->initial_lct = (TAGGED *)ptr;
    ptr = (char *)(pdata->initial_lct+ntasks);
    pdata->previous_est = (TAGGED *)ptr;
    ptr = (char *)(pdata->previous_est+ntasks);
    pdata->previous_lct = (TAGGED *)ptr;
    ptr = (char *)(pdata->previous_lct+ntasks);
    /* arrays for machine variables */
    pdata->machine.real = (MACHINE *)ptr;
    ptr = (char *)(pdata->machine.real+nmachines);
    pdata->machine.capacity = (SP_integer *)ptr;
    ptr = (char *)(pdata->machine.capacity+nmachines);
    /* arrays for task variables */
    pdata->task.dvorg = (Dvar)ptr;
    ptr += ntasks*sizeof(struct dvar);
    pdata->task.dvdur = (Dvar)ptr;
    ptr += ntasks*sizeof(struct dvar);
    pdata->task.dvend = (Dvar)ptr;
    ptr += ntasks*sizeof(struct dvar);
    pdata->task.dvheight = (Dvar)ptr;
    ptr += ntasks*sizeof(struct dvar);
    pdata->task.dvmach = (Dvar)ptr;
    ptr += ntasks*sizeof(struct dvar);
    pdata->task.flags = (SP_integer *)ptr;
    ptr = (char *)(pdata->task.flags+ntasks);
    pdata->task.m_status = (MACHINE *)ptr;
    ptr = (char *)(pdata->task.m_status+ntasks);
    pdata->task.htask = (SP_integer *)ptr;
    ptr = (char *)(pdata->task.htask+ntasks);
    pdata->task.lctkey = (SP_integer *)ptr;
    ptr = (char *)(pdata->task.lctkey+ntasks);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    pdata->destructor = symmcum_destructor;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(10*ntasks+2*nmachines);
    DerefArg(tmp,X(0),3);		/* get cum_atmost */
    pdata->options = GetSmall_int(tmp);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    pdata->machine_size = 0;
    pdata->ntasks = ntasks;
    pdata->nmachines = nmachines;
    absmin = CLPFD_MAXINT;
    absmax = -CLPFD_MAXINT;
    DerefArg(list,X(0),5);
    for (i=0; i<nmachines; i++) {
      SP_globref ref = RefMachineLimit(i);
      TAGGED elt, tmp1, tmin, tmax;
      SP_integer c;
    
      DerefCar(elt,list);
      DerefCdr(list,list);
      DerefArg(tmp1,elt,1);
      REAL(i) = GetSmall(tmp1);
      DerefArg(tmp1,elt,2);
      fd_get_var_and_attr(tmp1,ref);
      REF_GET_BOUNDS(ref, tmin, tmax);
      c = ((pdata->options&OPT_ATMOST) ? GetSmall(tmax) : GetSmall(tmin));
      CAPACITY(i) = c;
      if (absmin>c)
	absmin = c;
      if (absmax<c)
	absmax = c;
    }
    pdata->capacity_min = absmin;
    pdata->capacity_max = absmax;

    minmin = CLPFD_MAXINT;
    minmax = CLPFD_MAXINT;
    maxmin = -CLPFD_MAXINT;
    maxmax = -CLPFD_MAXINT;
    maxdur = -CLPFD_MAXINT;
    DerefArg(list,X(0),4);
    for (i=0; i<ntasks; i++) {
      TASK t = TASK(i) = i;
      TAGGED elt, tmp1;
      SP_integer l, u;
    
      DerefCar(elt,list);
      DerefCdr(list,list);

      DerefArg(tmp1,elt,1);	/* origin */
      fd_get_var_and_attr(tmp1,RefOrgAttr(t));
      tmp1 = RefGlob(RefOrgAttr(t));
      DerefAttribute(tmp1,tmp1); /* dom/4 term */
      pdata->initial_est[t] = DomainMin(tmp1);

      DerefArg(tmp1,elt,3);	/* end */
      fd_get_var_and_attr(tmp1,RefEndAttr(t));
      tmp1 = RefGlob(RefEndAttr(t));
      DerefAttribute(tmp1,tmp1); /* dom/4 term */
      pdata->initial_lct[t] = DomainMax(tmp1);

      DerefArg(tmp1,elt,4);	/* height */
      fd_get_var_and_attr(tmp1,RefHeightAttr(t));
      tmp1 = RefGlob(RefHeightAttr(t));
      DerefAttribute(tmp1,tmp1); /* dom/4 term */
      l = GetSmall(DomainMin(tmp1)); /* min. height */
      if (minmin>l)
	minmin = l;
      if (maxmin<l)
	maxmin = l;
      u = GetSmall(DomainMax(tmp1)); /* max. height */
      if (minmax>u)
	minmax = u;
      if (maxmax<u)
	maxmax = u;
      
      DerefArg(tmp1,elt,2);	/* duration */
      fd_get_var_and_attr(tmp1,RefDurAttr(t));
      tmp1 = RefGlob(RefDurAttr(t));
      DerefAttribute(tmp1,tmp1); /* dom/4 term */
      if ((pdata->options&OPT_ATMOST)
	  ? (l>0 || l>pdata->capacity_min)
	  : (u<0 || u<pdata->capacity_max)) {
	l = GetSmall(DomainMax(tmp1)); /* max. dur */
	if (maxdur<l)
	  maxdur = l;
      }
      
      DerefArg(tmp1,elt,5);	/* machine */
      fd_get_var_and_attr(tmp1,RefMachAttr(t));
    }
    pdata->min_min_height = minmin;
    pdata->min_max_height = maxmin;
    pdata->max_min_height = minmax;
    pdata->max_max_height = maxmax;
    pdata->max_duration = maxdur;
    CTagToArg(X(0),4) = atom_nil; /* [MC] 3.12: free for GC */
    CTagToArg(X(0),5) = atom_nil; /* [MC] 3.12: free for GC */
    pdata->global_est = Sup;
    pdata->global_lct = Inf;
    for (i=0; i<ntasks; i++) {
      dvar_init(DVORG(i), RefOrgAttr(i), RefOrgVar(i));
      dvar_init(DVDUR(i), RefDurAttr(i), RefDurVar(i));
      dvar_init(DVEND(i), RefEndAttr(i), RefEndVar(i));
      dvar_init(DVHEIGHT(i), RefHeightAttr(i), RefHeightVar(i));
      dvar_init(DVMACH(i), RefMachAttr(i), RefMachVar(i));
      if (FDgt(pdata->global_est,pdata->initial_est[i]))
	pdata->global_est = pdata->initial_est[i];
      if (FDlt(pdata->global_lct,pdata->initial_lct[i]))
	pdata->global_lct = pdata->initial_lct[i];
    }
  }

  /* RESUME HERE */
  
  /* refresh machine capacities */
  
  absmin = CLPFD_MAXINT;
  absmax = -CLPFD_MAXINT;
  machine_size = 0;
  for (i=0; i<pdata->nmachines; i++) {
    SP_globref ref = RefMachineLimit(i);
    TAGGED tmin, tmax;
    SP_integer c;
    
    REF_GET_BOUNDS(ref, tmin, tmax);
    machine_size += GetSmall(tmax) - GetSmall(tmin);
    c = ((pdata->options&OPT_ATMOST) ? GetSmall(tmax) : GetSmall(tmin));
    CAPACITY(i) = c;
    if (absmin>c)
      absmin = c;
    if (absmax<c)
      absmax = c;
  }
  pdata->capacity_min = absmin;
  pdata->capacity_max = absmax;
  if (pdata->machine_size!=machine_size)
    pdata->stamp = state_stamp+1; /* limits have changed -- disable optimizations */
  
  /* Get the correct table according to whether we backtrack or not */

  if (state_stamp != pdata->stamp) {
    pdata->table_est = pdata->initial_est;
    pdata->table_lct = pdata->initial_lct;
  } else {
    pdata->table_est = pdata->previous_est;
    pdata->table_lct = pdata->previous_lct;
  }
  DerefArg(tmp,X(0),6);
  pdata->ntargets = GetSmall_int(tmp);
  DerefArg(tmp,X(0),7);
  pdata->nsources = GetSmall_int(tmp);
  ntargets = pdata->ntargets;
  nactive = ntargets + pdata->nsources;
  
  /* refresh target tasks */
  
  pdata->last_to_prune = ntasks;
  for (i=0; i<ntargets; i++) {
    TASK t = TASK(i);      

    FLAGS(t) = STATUS_TARGET+STATUS_SOURCE;
    if ((pdata->options&OPT_PRUNE_NEXT) && /* prune(next) selected */
	state_stamp != pdata->stamp && /* not fwd execution */
	pdata->last_to_prune>t && /* found an earlier task */
	(!dvar_is_integer(DVORG(t)) || /* not all attributes ground */
	 !dvar_is_integer(DVEND(t)) ||
	 !dvar_is_integer(DVDUR(t)) ||
	 !dvar_is_integer(DVHEIGHT(t)) ||
	 !dvar_is_integer(DVMACH(t))))
      pdata->last_to_prune = (int)t;
    dvar_refresh(DVORG(t));
    dvar_refresh(DVDUR(t));
    dvar_refresh(DVEND(t));
    dvar_refresh(DVHEIGHT(t));
    dvar_refresh(DVMACH(t));
    if (!task_renormalize(wam, t))
      goto ret;
  }
  if (state_stamp != pdata->stamp) {
    for (i=ntargets-1; i>=0; i--) {
      TASK t = TASK(i);
    
      LCTKEY(t) = LCT(t);
    }
    qsort_asc_lctkey(wam, pdata->target, ntargets);
    build_minest(wam, 0, ntargets);
    build_minest(wam, ntargets, nactive); /* 3.9 */
  }

  /* END OF RESUMPTION */
  
  pdata->stamp = state_stamp+1;
  pdata->machine_size = machine_size;  
  if (!check_interval(wam, GetSmall(pdata->global_est), GetSmall(pdata->global_lct)))
    goto ret;
  
  if (pdata->options&OPT_TASK_INTERVALS) {
    for (i=0; i<ntargets; i++) {
      TASK t = TASK(i);
      SP_integer minh = MINHEIGHT(t);
      SP_integer maxh = MAXHEIGHT(t);

      /* Check task interval(s) of the modified tasks */
      /* Consider interval of placement of the task
	 (filter out tasks with compulsory part) */

      if ((pdata->options&OPT_ATMOST)
	  ? (minh>0 || minh>pdata->capacity_min)
	  : (maxh<0 || maxh<pdata->capacity_max)) {
	if ((!FIXEDMACH(t) || !HAS_COMPULSORY_PART(t)) &&
	    !ti_task_interval(wam, EST(t),LCT(t),SETMACH(t)))
	  goto ret;
      } else {
	if (!ti_task_interval(wam, TABLE_EST(t),EST(t),SETMACH(t)) ||
	    !ti_task_interval(wam, LCT(t),TABLE_LCT(t),SETMACH(t)))
	  goto ret;
      }
    }
  }
  
  /* Prepare for pruning - protect from GC */
  
  for (i=0; i<ntargets; i++) {
    TASK t = TASK(i);

    dvar_pruning_done( DVORG(t));
    dvar_pruning_done( DVDUR(t));
    dvar_pruning_done( DVEND(t));
    dvar_pruning_done( DVHEIGHT(t));
    dvar_pruning_done( DVMACH(t));
  }

  /* Record the state of the variables after saturation, and prune */
  
  for (i=0; i<ntargets; i++) {
    TASK t = TASK(i);
    SP_integer est = EST(t);
    SP_integer lct = LCT(t);

    pdata->previous_est[t] = MakeSmall(est);
    pdata->previous_lct[t] = MakeSmall(lct);
    if (committed && machine_size==0) {
      pdata->initial_est[t] = MakeSmall(est);
      pdata->initial_lct[t] = MakeSmall(lct);
    }

    dvar_export(DVORG(t));
    dvar_export(DVDUR(t));
    dvar_export(DVEND(t));
    dvar_export(DVHEIGHT(t));
    dvar_export(DVMACH(t));
  }

  /* Update TARGET/SOURCE properties and repartition tasks */

  if (machine_size==0) {
    update_task_states(wam, pdata);
    ent = (pdata->ntargets==0);
  } else {
    ent = 0;
  }
  
  CTagToArg(X(0),6) = MakeSmall(pdata->ntargets);
  CTagToArg(X(0),7) = MakeSmall(pdata->nsources);

 ret:
  if (ent==1)
    Pfree;
  dvar_export_done(wam, Actions, ent);
}
