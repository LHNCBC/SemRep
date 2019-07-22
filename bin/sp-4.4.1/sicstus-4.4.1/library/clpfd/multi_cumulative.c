/* Copyright(C) 2004, Swedish Institute of Computer Science */

/****************************************************************

 Sweep-based multi-resource cumulative [CPAIOR 2013]

 ****************************************************************/

#include "fd.h"
#include "dvars.h"

#define STATUS_SOURCE 0x1
#define STATUS_TARGET 0x2
#define TYPE_PR         0x0
#define TYPE_SCP        0x1
#define TYPE_ECP        0x2
#define TYPE_SFP        0x3
#define TYPE_EFP        0x4
#define TYPE_RS         0x5
#define TYPE_DELTA      0x6

#define SCHED_MIN       0x1
#define SCHED_MAX       0x2

typedef SP_integer TASK;

#define TARGET(i) (pdata->target[i])
#define STATUS(it) (pdata->status[it])
#define ORIGVAR(T) (pdata->origvar + (T))
#define ENDVAR(t) (pdata->endvar+(t))
#define DUR(T)     (pdata->dur[T])
/***
#define EST(T) dvar_min_l(ORIGVAR(T))
#define LaST(T) dvar_max_l(ORIGVAR(T))
#define ECT(T) dvar_min_l(ENDVAR(T))
#define LCT(T) dvar_max_l(ENDVAR(T))
***/
#define EST(T)  (pdata->est[T])
#define LaST(T) (pdata->lst[T])
#define ECT(T)  (pdata->ect[T])
#define LCT(T)  (pdata->lct[T])
#define RefLimAttr     (pdata->refbase)
#define RefLim         (pdata->refbase + 1)
#define RefOrigAttr(T) (pdata->refbase + ((T)<<2) + 2)
#define RefOrig(T)     (pdata->refbase + ((T)<<2) + 3)
#define RefEndAttr(T)  (pdata->refbase + ((T)<<2) + 4)
#define RefEnd(T)      (pdata->refbase + ((T)<<2) + 5)

#define COLOR(T,R) (pdata->color[kr*(T)+(R)])
#define USE(T,R) (pdata->use[kr*(T)+(R)])

#define RING_NONE 0
#define RING_READY 1
#define RING_CHECK 2
#define RING_CONFLICT(R) ((R)+3)

#define IN_RING_NONE(T) (pdata->dlist_task[T].ring == RING_NONE)
#define IN_RING_CHECK(T) (pdata->dlist_task[T].ring == RING_CHECK)
#define IN_RING_CONFLICT(T) (pdata->dlist_task[T].ring >= RING_CONFLICT(0))

#define EMPTY_RING_READY (pdata->dlist_ready.forward == &pdata->dlist_ready)

#define SET_RING_NONE(T)					\
{								\
  struct dlist *m_b = pdata->dlist_task[T].backward;		\
  struct dlist *m_f = pdata->dlist_task[T].forward;		\
  m_b->forward = m_f;						\
  m_f->backward = m_b;						\
  pdata->dlist_task[T].forward = &pdata->dlist_task[T];		\
  pdata->dlist_task[T].backward = &pdata->dlist_task[T];	\
  pdata->dlist_task[T].ring = RING_NONE;			\
}

#define SET_RING_READY(T)				\
{							\
  struct dlist *m_b = pdata->dlist_task[T].backward;	\
  struct dlist *m_f = pdata->dlist_task[T].forward;	\
  struct dlist *r_f = pdata->dlist_ready.forward;	\
  m_b->forward = m_f;					\
  m_f->backward = m_b;					\
  pdata->dlist_task[T].forward = r_f;			\
  r_f->backward = &pdata->dlist_task[T];		\
  pdata->dlist_ready.forward = &pdata->dlist_task[T];		\
  pdata->dlist_task[T].backward = &pdata->dlist_ready;	\
  pdata->dlist_task[T].ring = RING_READY;		\
}

#define SET_RING_CHECK(T)				\
{							\
  struct dlist *m_b = pdata->dlist_task[T].backward;	\
  struct dlist *m_f = pdata->dlist_task[T].forward;	\
  struct dlist *r_f = pdata->dlist_check.forward;	\
  m_b->forward = m_f;					\
  m_f->backward = m_b;					\
  pdata->dlist_task[T].forward = r_f;			\
  r_f->backward = &pdata->dlist_task[T];		\
  pdata->dlist_check.forward = &pdata->dlist_task[T];		\
  pdata->dlist_task[T].backward = &pdata->dlist_check;	\
  pdata->dlist_task[T].ring = RING_CHECK;		\
}

#define SET_RING_CONFLICT(R,T)					\
{								\
  struct dlist *m_b = pdata->dlist_task[T].backward;		\
  struct dlist *m_f = pdata->dlist_task[T].forward;		\
  struct dlist *r_f = pdata->dlist_conflict[R].forward;		\
  m_b->forward = m_f;						\
  m_f->backward = m_b;						\
  pdata->dlist_task[T].forward = r_f;				\
  r_f->backward = &pdata->dlist_task[T];			\
  pdata->dlist_conflict[R].forward = &pdata->dlist_task[T];		\
  pdata->dlist_task[T].backward = &pdata->dlist_conflict[R];	\
  pdata->dlist_task[T].ring = RING_CONFLICT(R);			\
}

struct dlist {
  unsigned int ring:32;
  unsigned int task:32;
  struct dlist *forward;
  struct dlist *backward;
};

struct cons {
  SP_integer head;
  struct cons *tail;
};

struct kv {
  SP_integer key;
  struct cons *value;
};

struct heap {
  SP_BOOL increasing;
  SP_integer pop;			/* SP_integer for trailing */
  struct kv *kv;
};

struct trail_item {
  SP_uinteger tagged;
  /* unsigned int tag:3; */
  /* SP_integer data; */
};

#define TRAIL_ITEM_TAG(P) ((P)->tagged & 0x7)
#define TRAIL_ITEM_DATA(P) ((SP_integer)((P)->tagged) >> 3)
#define TRAIL_ITEM_SET(P,Tag,Data) ((P)->tagged = (((Data)<<3) | (Tag)))

#define CONS(Target,Head,Tail)			\
{						\
  struct cons *m_value = pdata->free_list;	\
  pdata->free_list = m_value->tail;		\
  m_value->head = (Head);			\
  m_value->tail = (Tail);			\
  (Target) = m_value;				\
}
						\
#define DECONS(Source,Head,Tail)		\
{						\
  struct cons *m_value = (Source);		\
  (Head) = m_value->head;			\
  (Tail) = m_value->tail;			\
  m_value->tail = pdata->free_list;		\
  pdata->free_list = m_value;			\
}

struct multi_cumulative_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (handle,attr_ref,global) */
#if MULTI_SP_AWARE
  SPEnv *spenv;
#endif /* MULTI_SP_AWARE */

  int nrefs;			/* static, 4*nbtasks+2 */
  SP_globref refbase;		/* static */
  SP_integer stamp;
  int schedule;		        /* subset of SCHED_MIN|SCHED_MAX */
  int nbtasks;			/* static, #tasks */
  int ntargets;			/* #tasks that may be targets, := nbtasks */
  int nsources;			/* #tasks that may be sources only, := 0 */
  int kresources;
  int kcolors;
  SP_BOOL change;		/* for invariants only */
  Dvar fix_dvar;		/* [1], greedy */
  Dvar origvar;
  Dvar endvar;
  struct dlist dlist_ready;	/* at fixpoint */
  struct dlist dlist_check;
  struct dlist *dlist_task;	/* [nbtasks] */
  struct dlist *dlist_conflict;	/* [kresources] */
  SP_integer *batch;			/* [5*nbtasks] */
  SP_integer *rsbuf;			/* [nbtasks], NEW */
  SP_integer *dur;			/* [nbtasks] */
  TASK *target;			/* [nbtasks] */
  SP_integer *status;			/* [nbtasks] */
  SP_integer *est;			/* [nbtasks] */
  SP_integer *lst;			/* [nbtasks] */
  SP_integer *ect;			/* [nbtasks] */
  SP_integer *lct;			/* [nbtasks] */
  SP_integer *nbpconst;		/* [nbtasks], NEW */
  SP_integer *nbp;			/* [nbtasks], NEW */
  SP_integer *nbsconst;		/* [nbtasks], NEW */
  SP_integer *nbs;			/* [nbtasks], NEW */
  struct cons **predecessors;	/* [nbtasks], NEW */
  struct cons **successors;	/* [nbtasks], NEW */
  SP_integer *is_colored;		/* [kresources] */
  SP_integer *capa;			/* [kresources] */
  SP_integer *distinct_colors;	/* [kresources] */
  SP_integer *count_colors;		/* [kresources*kcolors] */
  SP_integer *color;			/* [nbtasks*kresources] */
  SP_integer *gap;			/* [kresources] */
  SP_integer *gap0;			/* [kresources] */
  SP_integer *use;			/* [nbtasks*kresources, OVERLAY color] */
  struct heap h_events;		/* [5*nbtasks] */
  struct cons *free_list;	/* [5*nbtasks + 2*precedences], NEW */  
  struct trail_item *trail;
  struct trail_item *ttop;
  struct trail_item *tend;
  /* space for the above arrays */
};

static void SPCDECL multi_cumulative_destructor(void *pdata_v)
{
  struct multi_cumulative_data *pdata = (struct multi_cumulative_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv);

  SP_free_globrefs(pdata->refbase,pdata->nrefs);
  SP_free(pdata);
}

static DAEMON_RC SPCDECL 
multi_cumulative_daemon(Wam wam, void *vdata, SP_globref attr_ref, TAGGED *global)
{
  struct multi_cumulative_data *pdata = (struct multi_cumulative_data *)vdata;
  TAGGED tstate = RefMutable(CTagToArg(*global,1));
  int ar = Arity(TagToHeadfunctor(tstate));
  int state_stamp = (int)GetSmall(CTagToArg(tstate,ar));
  
  (void)wam;
  if (pdata->stamp!=state_stamp) { /* non-incremental */
    pdata->schedule = SCHED_MIN+SCHED_MAX; /* over-conservative, but doesn't hurt */
  } else if (attr_ref!=RefLimAttr) {
    int subtask = (attr_ref - pdata->refbase - 2) & 3;
    int task = (int)(attr_ref - pdata->refbase - 2)>>2;
    if (subtask & 0x2) {	/* LCT was pruned */
      pdata->schedule |= SCHED_MAX;
      if (pdata->schedule != SCHED_MIN+SCHED_MAX) {
	dvar_refresh(ENDVAR(task));
	if (dvar_max_l(ENDVAR(task)) - dvar_min_l(ENDVAR(task)) <= DUR(task))
	  pdata->schedule = SCHED_MIN+SCHED_MAX;
      }
    } else {			/* EST was pruned */
      pdata->schedule |= SCHED_MIN;
      if (pdata->schedule != SCHED_MIN+SCHED_MAX) {
	dvar_refresh(ORIGVAR(task));
	if (dvar_max_l(ORIGVAR(task)) - dvar_min_l(ORIGVAR(task)) <= DUR(task))
	  pdata->schedule = SCHED_MIN+SCHED_MAX;
      }
    }
  }
  return DAEMON_NOFIX;
}

static void heapify(struct heap *heap,int i)
{
  SP_integer pop = heap->pop;
  int invert = !heap->increasing;
  struct kv kv;
  
  for (;;) {
    int l = (i<<1)+1;
    int smallest = i;
    if (l<pop && ((heap->kv[l].key<heap->kv[smallest].key) ^ invert))
      smallest = l;
    if (l+1<pop && ((heap->kv[l+1].key<heap->kv[smallest].key) ^ invert))
      smallest = l+1;
    if (smallest==i)
      break;
    kv = heap->kv[smallest];
    heap->kv[smallest] = heap->kv[i];
    heap->kv[i] = kv;
    i = smallest;
  }
}

static void heap_init(struct heap *heap, int increasing)
{
  heap->increasing = increasing;
  heap->pop = 0;
}

static int cmp_asc_kv(Wam wam, struct kv *t1, struct kv *t2)
{
  (void)wam;
  return CMP(t1->key,t2->key);
}

#define QType struct kv 
#define QCmp  cmp_asc_kv
#define QSort qsort_asc_kv
#include "qsort.ic"

static int cmp_dec_kv(Wam wam, struct kv *t1, struct kv *t2)
{
  (void)wam;
  return -CMP(t1->key,t2->key);
}

#define QType struct kv 
#define QCmp  cmp_dec_kv
#define QSort qsort_dec_kv
#include "qsort.ic"

static void heap_sort(Wam wam, struct heap *heap)
{
  int i=0, j=0;

  if (heap->increasing)
    qsort_asc_kv(wam, heap->kv,heap->pop);
  else
    qsort_dec_kv(wam, heap->kv,heap->pop);
  while (i<heap->pop) {
    struct cons *last = heap->kv[i].value;

    heap->kv[j++] = heap->kv[i++];
    while (i<heap->pop && heap->kv[i].key==heap->kv[i-1].key) {
      last->tail = heap->kv[i++].value;
      last = last->tail;
    }
  }
  heap->pop = j;
}

static void heap_extract_top(struct heap *heap,struct kv *kv)
{
  SP_integer pop = --heap->pop;

  *kv = heap->kv[0];
  if (pop>0) {
    heap->kv[0] = heap->kv[pop];
    heapify(heap,0);
  }
}

#define heap_peek_key(H) ((H)->kv[0].key)

static void heap_add(struct heap *heap,SP_integer key,struct cons *value)
{
  SP_integer i = heap->pop;
  int invert = !heap->increasing;

  while (i>0 && ((heap->kv[(i-1)>>1].key > key) ^ invert)) {
    i = (i-1)>>1;
  }
  if (i>0 && heap->kv[(i-1)>>1].key == key) { /* merge two bins */
    struct cons *last = value;
    while (last->tail)
      last = last->tail;
    last->tail = heap->kv[(i-1)>>1].value;
    heap->kv[(i-1)>>1].value = value;
  } else {
    SP_integer ilast = i;

    i = heap->pop++;
    while (i>ilast) {
      heap->kv[i] = heap->kv[(i-1)>>1];
      i = (i-1)>>1;
    }
    heap->kv[i].key = key;
    heap->kv[i].value = value;
  }
}

#define heap_push(H,K,V)			\
{						\
  SP_integer m_i = (H)->pop++;				\
  struct kv m_kv;				\
  m_kv.key = (K);				\
  CONS(m_kv.value,V,NULL);			\
  (H)->kv[m_i] = m_kv;				\
}

static int heap_extract_batch(struct multi_cumulative_data *pdata,struct heap *heap,SP_integer *key)
{
  int n = 0;
  struct kv kv;

  do {
    heap_extract_top(heap,&kv);
    while (kv.value) {
      DECONS(kv.value, pdata->batch[n++], kv.value);
    }
  } while (heap->pop>0 && heap->kv[0].key==kv.key);
  *key = kv.key;
  return n;
}

#if 0
static SP_BOOL heap_is_free_of(struct multi_cumulative_data *pdata, SP_integer data)
{
  int i;

  for (i=0; i<pdata->h_events.pop; i++) {
    struct cons *cons = pdata->h_events.kv[i].value;

    while (cons) {
      if (cons->head==data)
	return FALSE;
      cons = cons->tail;
    }
  }
  return TRUE;
}
#endif

static void heap_add_singleton(struct multi_cumulative_data *pdata,
			       struct heap *heap,SP_integer key,SP_integer data)
{
  struct cons *value;

  /* SP_ASSERT(heap_is_free_of(pdata,data)); */
  CONS(value, data, NULL);
  heap_add(heap,key,value);
}

#define TRAIL(TAG,TASK)				\
{						\
  if (pdata->ttop >= pdata->tend)		\
    trail_expand(wam,pdata,delta);		\
  TRAIL_ITEM_SET(pdata->ttop,TAG,TASK);         \
  pdata->ttop++;				\
}

#define IN_CONFLICT(TASK,RES)				\
(pdata->is_colored[RES] ?				\
 (pdata->distinct_colors[RES]==pdata->capa[RES] &&	\
  pdata->count_colors[kc*(RES)+COLOR(TASK,RES)]==0) :	\
 (USE(TASK,RES) > pdata->gap[RES]))			\

#define EXCEEDED(RES)					\
(pdata->is_colored[RES] ?				\
 (pdata->distinct_colors[RES]>pdata->capa[RES]) :	\
 (pdata->gap[RES] < 0))					\

#define DEC_GAP(TASK,RES);				\
if (pdata->is_colored[RES]) {				\
  SP_integer color = COLOR(TASK,RES);			\
  if (color!=0) {					\
    if (pdata->count_colors[kc*RES+color]++ == 0)	\
      pdata->distinct_colors[RES]++;			\
  }							\
} else {						\
  pdata->gap[RES] -= USE(TASK,RES);		\
}							\
							
#define INC_GAP(TASK,RES);				\
if (pdata->is_colored[RES]) {				\
  SP_integer color = COLOR(TASK,RES);			\
  if (color!=0) {					\
    if (--pdata->count_colors[kc*RES+color] == 0)	\
      --pdata->distinct_colors[RES];			\
  }							\
} else {						\
  pdata->gap[RES] += USE(TASK,RES);		\
}							\
							\


static void reclaim_free(struct multi_cumulative_data *pdata,
			 struct cons *cons)
{
  struct cons *last = cons;

  while (last->tail)
    last = last->tail;
  last->tail = pdata->free_list;
  pdata->free_list = cons;
}

static void trail_expand(Wam wam,
			 struct multi_cumulative_data *pdata,
			 SP_integer virdelta)
{
  SP_integer inuse, oldsize = pdata->tend - pdata->trail;
  struct trail_item *p, *q;

  /* pass 1: clean out outdated PR entries */
  p = q = pdata->ttop;
  while (p > pdata->trail) {
    --p;
    switch (TRAIL_ITEM_TAG(p)) {
    case TYPE_DELTA:
      virdelta = TRAIL_ITEM_DATA(p);
      break;
    case TYPE_PR:
      if (EST(TRAIL_ITEM_DATA(p)) != virdelta)
	continue;
    }
    *--q = *p;
  }

  /* pass 2: clean out entried for fixed tasks */
  p = q;
  q = pdata->trail;
  while (p < pdata->ttop) {
    switch (TRAIL_ITEM_TAG(p)) {
    case TYPE_PR:
    case TYPE_SCP:
    case TYPE_ECP:
      if (!(STATUS(TRAIL_ITEM_DATA(p)) & STATUS_TARGET)) {
	p++;
	continue;
      }
    }
    *q++ = *p++;
  }
  pdata->ttop = q;
  inuse = pdata->ttop - pdata->trail;
  if (10*(oldsize-inuse) < oldsize) { /* less than 10% free */
    pdata->trail = SP_realloc(pdata->trail, 2*oldsize*sizeof(struct trail_item));
    pdata->ttop = pdata->trail + inuse;
    pdata->tend = pdata->trail + 2*oldsize;
  }
}

static void drain_heaps(struct multi_cumulative_data *pdata)
{
  int n = pdata->ntargets+pdata->nsources;
  int i;

  for (i=0; i<n; i++) {
    TASK t = TARGET(i);
    SET_RING_NONE(t);
  }
  for (i=0; i<pdata->h_events.pop; i++)
    reclaim_free(pdata,pdata->h_events.kv[i].value);
}

static void gen_min_events(Wam wam, struct multi_cumulative_data *pdata)
{
  int n = pdata->ntargets+pdata->nsources;
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int i, j;
  
  heap_init(&pdata->h_events, TRUE);
  for (i=0; i<kr; i++) {
    pdata->gap0[i] = pdata->capa[i]; 
    pdata->gap[i] = pdata->capa[i]; 
    pdata->distinct_colors[i] = 0;
    for (j=0; j<kc; j++)
      pdata->count_colors[kc*i+j] = (j==0);
  }
  for (i=0; i<n; i++) {
    TASK t = TARGET(i);
    SET_RING_NONE(t);
    if ((STATUS(t) & STATUS_SOURCE) && pdata->nbp[t]==0) {
      if (DUR(t)>0) {
	if (EST(t) < LaST(t)) {
	  heap_push(&pdata->h_events,EST(t),(t<<3)+TYPE_PR);
	}
	heap_push(&pdata->h_events,LaST(t),(t<<3)+TYPE_SCP);
	if (LaST(t) < ECT(t)) {
	  heap_push(&pdata->h_events,ECT(t),(t<<3)+TYPE_ECP);
	}
      }
      if (pdata->nbs[t]>0) { /* also if DUR(t)==0 */
	heap_push(&pdata->h_events,ECT(t),(t<<3)+TYPE_RS);
      }
    }
  }
  heap_sort(wam, &pdata->h_events);
}

static void gen_max_events(Wam wam, struct multi_cumulative_data *pdata)
{
  int n = pdata->ntargets+pdata->nsources;
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int i, j;
  
  heap_init(&pdata->h_events, FALSE);
  for (i=0; i<kr; i++) {
    pdata->gap0[i] = pdata->capa[i]; 
    pdata->gap[i] = pdata->capa[i]; 
    pdata->distinct_colors[i] = 0;
    for (j=0; j<kc; j++)
      pdata->count_colors[kc*i+j] = (j==0);
  }
  for (i=0; i<n; i++) {
    TASK t = TARGET(i);
    SET_RING_NONE(t);
    if ((STATUS(t) & STATUS_SOURCE) && pdata->nbs[t]==0) {
      if (DUR(t)>0) {
	if (LCT(t) > ECT(t)) {
	  heap_push(&pdata->h_events,LCT(t),(t<<3)+TYPE_PR);
	}
	heap_push(&pdata->h_events,ECT(t),(t<<3)+TYPE_SCP);
	if (ECT(t) > LaST(t)) {
	  heap_push(&pdata->h_events,LaST(t),(t<<3)+TYPE_ECP);
	}
      }
      if (pdata->nbp[t]>0) { /* also if DUR(t)==0 */
	heap_push(&pdata->h_events,LaST(t),(t<<3)+TYPE_RS);
      }
    }
  }
  heap_sort(wam, &pdata->h_events);
}

#if SP_ASSERTIONS

extern int sizeof_free(struct multi_cumulative_data *pdata);
int sizeof_free(struct multi_cumulative_data *pdata)
{
  struct cons *last;
  int i=0;
  for (last=pdata->free_list; last; last=last->tail)
    i++;
  return i;
}

static SP_BOOL check_ok(struct multi_cumulative_data *pdata,SP_integer delta)
{
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int r;

  for (r=0; r<kr; r++) {
    struct dlist *check = pdata->dlist_check.forward;
    while (check != &pdata->dlist_check) {
      TASK t = check->task;
      check = check->forward;
      if (ECT(t) > delta && IN_CONFLICT(t,r))
	return FALSE;
    }
  }
  return TRUE;
}

static SP_BOOL min_property(struct multi_cumulative_data *pdata)
{
  int n = pdata->ntargets+pdata->nsources;
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int i, r, ti, tj;

  for (ti=0; ti<n; ti++) {
    TASK t = TARGET(ti);
    struct cons *ss = pdata->successors[t];
    SP_integer ect = ECT(t);
    
    if (DUR(t)>0) {
      for (i=0; i<kr; i++) {
	pdata->distinct_colors[i] = 0;
	pdata->gap[i] = pdata->capa[i];
      }
      for (i=0; i<kr*kc; i++) {
	pdata->count_colors[i] = 0;
      }
      for (r=0; r<kr; r++) {
	DEC_GAP(t,r);
      }
      for (tj=0; tj<n; tj++) {
	TASK t1 = TARGET(tj);
	if (t1!=t && LaST(t1) <= EST(t) && EST(t) < ECT(t1)) {
	  for (r=0; r<kr; r++) {
	    DEC_GAP(t1,r);
	  }
	}
      }
      for (r=0; r<kr; r++)
	if (EXCEEDED(r))
	  return FALSE;
    }
    while (ss) {
      TASK s1 = ss->head;
      ss = ss->tail;
      if (ect > EST(s1))
	return FALSE;
    }
  }
  return TRUE;
}

static SP_BOOL max_property(struct multi_cumulative_data *pdata)
{
  int n = pdata->ntargets+pdata->nsources;
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int i, r, ti, tj;

  for (ti=0; ti<n; ti++) {
    TASK t = TARGET(ti);
    struct cons *pp = pdata->predecessors[t];
    SP_integer lst = LaST(t);
    
    if (DUR(t)>0) {
      for (i=0; i<kr; i++) {
	pdata->distinct_colors[i] = 0;
	pdata->gap[i] = pdata->capa[i];
      }
      for (i=0; i<kr*kc; i++) {
	pdata->count_colors[i] = 0;
      }
      for (r=0; r<kr; r++) {
	DEC_GAP(t,r);
      }
      for (tj=0; tj<n; tj++) {
	TASK t1 = TARGET(tj);
	if (t1!=t && LaST(t1) <= LaST(t) && LaST(t) < ECT(t1)) {
	  for (r=0; r<kr; r++) {
	    DEC_GAP(t1,r);
	  }
	}
      }
      for (r=0; r<kr; r++)
	if (EXCEEDED(r))
	  return FALSE;
    }
    while (pp) {
      TASK p1 = pp->head;
      pp = pp->tail;
      if (lst < LCT(p1))
	return FALSE;
    }
  }
  return TRUE;
}

static SP_BOOL no_leak(struct multi_cumulative_data *pdata)
{
  int nbtasks = pdata->nbtasks;
  struct cons *cons = pdata->free_list;
  int i;

  for (i=0; cons; i++)
    cons = cons->tail;

  return (i == 5*nbtasks);
}
#endif

/* init pdata->nbp[], pdata->nbs[] with counts of SOURCE pred/succ tasks */
static void sweep_refresh_precedences(struct multi_cumulative_data *pdata)
{
  int n = pdata->ntargets+pdata->nsources;
  int i;
  for (i=0; i<n; i++) {
    TASK t = TARGET(i);
    struct cons *pp = pdata->predecessors[t];
    struct cons *ss = pdata->successors[t];
    int nbp=0, nbs=0;

    while (pp) {
      TASK p1 = pp->head;
      pp = pp->tail;
      if (STATUS(p1) & STATUS_SOURCE)
	nbp++;
    }
    while (ss) {
      TASK s1 = ss->head;
      ss = ss->tail;
      if (STATUS(s1) & STATUS_SOURCE)
	nbs++;
    } 
    pdata->nbp[t] = nbp;
    pdata->nbs[t] = nbs;
  }
}

/* -1 = failure */
/*  0 = success */
/*  1 = success, and handle PR event for t as well*/
static int 
sweep_min_add_task(struct multi_cumulative_data *pdata, SP_integer delta, TASK t)
{
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int rc=0, j;
  
  if (delta > LaST(t))
    return -1;
  if (EST(t) < delta) {
    EST(t) = delta;
    ECT(t) = delta+DUR(t);
  }
  if (DUR(t)>0) {
    if (EST(t) == delta) {
      if (EST(t) == LaST(t)) {
	for (j=0; j<kr; j++) {
	  DEC_GAP(t,j);
	}
	SET_RING_READY(t);
      } else {
	rc = 1;
	heap_add_singleton(pdata,&pdata->h_events,LaST(t),(t<<3)+TYPE_SCP);
      }
    } else {
      heap_add_singleton(pdata,&pdata->h_events,LaST(t),(t<<3)+TYPE_SCP);
      if (EST(t) < LaST(t)) {
	heap_add_singleton(pdata,&pdata->h_events,EST(t),(t<<3)+TYPE_PR);
      } else {
	SET_RING_READY(t);
      }
    }
    if (LaST(t) < ECT(t)) {
      heap_add_singleton(pdata,&pdata->h_events,ECT(t),(t<<3)+TYPE_ECP);
    }
  }
  if (pdata->nbs[t]>0) {	/* also if DUR(t)==0 */
    heap_add_singleton(pdata,&pdata->h_events,ECT(t),(t<<3)+TYPE_RS);
  }
  return rc;
}

static SP_BOOL sweep_min_process(Wam wam,
				 struct multi_cumulative_data *pdata,
				 SP_integer *pdelta, SP_integer *pdate)
{
  SP_integer delta, date;
  int n = heap_extract_batch(pdata,&pdata->h_events,&delta);
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int ti, j, pr, rs=0;
  (void)wam;
  
  for (ti=0, pr=0; ti<n; ti++) {
    SP_integer ev = pdata->batch[ti];
    SP_integer t  = ev >> 3;
    int type = (ev & 0x7);
    SP_integer cur_ecp;
    switch (type) {
    case TYPE_SCP:
      cur_ecp = ECT(t);
      if (IN_RING_CONFLICT(t)) {
	EST(t) = LaST(t);
	ECT(t) = LCT(t);
	SET_RING_READY(t);
      } else if (IN_RING_CHECK(t)) {
	SET_RING_READY(t);
      }
      if (delta < ECT(t)) {
	for (j=0; j<kr; j++) {
	  DEC_GAP(t,j);
	}
	if (cur_ecp <= delta)
	  heap_add_singleton(pdata,&pdata->h_events,ECT(t),(t<<3)+TYPE_ECP); /* reachable */
      }
      break;
    case TYPE_ECP:
      if (delta < ECT(t)) {
	heap_add_singleton(pdata,&pdata->h_events,ECT(t),(t<<3)+TYPE_ECP);
      } else {
	for (j=0; j<kr; j++) {
	  INC_GAP(t,j);
	}
      }
      break;
    case TYPE_RS:
      pdata->rsbuf[rs++] = t;
      break;
    case TYPE_PR:
      pdata->batch[pr++] = t;
      break;
    }
  }
  for (ti=0; ti<rs; ti++) {
    TASK t = pdata->rsbuf[ti];
    
    if (IN_RING_CONFLICT(t)) {
      heap_add_singleton(pdata,&pdata->h_events,delta+DUR(t),(t<<3)+TYPE_RS);
    } else if (delta==ECT(t)) {
      struct cons *ss = pdata->successors[t];
      while (ss) {
	TASK s1 = ss->head;
	ss = ss->tail;
	if (--pdata->nbp[s1]==0) {
	  switch (sweep_min_add_task(pdata,delta,s1)) {
	  case -1:
	    return FALSE;
	  case 1:
	    pdata->batch[pr++] = s1;
	  }
	}
      }
    } else {
      SP_ASSERT(delta<ECT(t));
      heap_add_singleton(pdata,&pdata->h_events,ECT(t),(t<<3)+TYPE_RS);
    }
  }
  *pdelta = delta;
  *pdate = date = pdata->h_events.pop>0 ? heap_peek_key(&pdata->h_events) : CLPFD_MAXINT2;
  for (ti=0; ti<pr; ti++) {
    SP_integer t = pdata->batch[ti];
    int rc;

    for (rc=0; rc<kr && !IN_CONFLICT(t,rc); rc++) ;
    if (rc<kr) {
      SET_RING_CONFLICT(rc,t);
    } else if (ECT(t)>date) {
      SET_RING_CHECK(t);
    } else {
      SET_RING_READY(t);
    }      
  }
  return TRUE;
}

static SP_BOOL sweep_min_filter(struct multi_cumulative_data *pdata,
				SP_integer delta, SP_integer date)
{
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int r;
  
  for (r=0; r<kr; r++) {
    if (EXCEEDED(r))
      return FALSE;
  }
  for (r=0; r<kr; r++) {
    SP_integer iscolr = pdata->is_colored[r];
    if (iscolr ? pdata->distinct_colors[r]==pdata->capa[r] :
	pdata->gap0[r] > pdata->gap[r]) {
      struct dlist *check = pdata->dlist_check.forward;
      pdata->gap0[r] = pdata->gap[r];
      while (check != &pdata->dlist_check) {
	TASK t = check->task;
	check = check->forward;
	if (ECT(t) <= delta) {
	  SET_RING_READY(t);
	} else if (IN_CONFLICT(t,r)) {
	  SET_RING_CONFLICT(r,t);
	}
      }
    }
  }
  for (r=0; r<kr; r++) {
    SP_integer iscolr = pdata->is_colored[r];
    if (iscolr ? TRUE : pdata->gap0[r] < pdata->gap[r]) {
      int b = (pdata->distinct_colors[r] < pdata->capa[r]);
      struct dlist *conflict = pdata->dlist_conflict[r].forward;
      pdata->gap0[r] = pdata->gap[r];
      while (conflict != &pdata->dlist_conflict[r]) {
	TASK t = conflict->task;
	conflict = conflict->forward;
	if (iscolr ?
	    (b || pdata->count_colors[kc*r+COLOR(t,r)]>0) :
	    (USE(t,r) <= pdata->gap[r])) {
	  int rc;
	  for (rc=0; rc<kr && !IN_CONFLICT(t,rc); rc++) ;
	  if (rc<kr) {
	    SET_RING_CONFLICT(rc,t);
	  } else {
	    SP_integer cur_ecp = ECT(t);
	    EST(t) = delta;
	    ECT(t) = delta+DUR(t);
	    if (LaST(t) >= cur_ecp && LaST(t) < ECT(t))
	      heap_add_singleton(pdata,&pdata->h_events,ECT(t),(t<<3)+TYPE_ECP);
	    if (ECT(t) <= date) {
	      SET_RING_READY(t);
	    } else {
	      SET_RING_CHECK(t);
	    }
	  }
	}
      }
    }
  }
  return TRUE;
}

static SP_BOOL sweep_min(Wam wam,struct multi_cumulative_data *pdata)
{
  SP_BOOL rc = TRUE;
  
  sweep_refresh_precedences(pdata);
  gen_min_events(wam,pdata);
  while (rc && pdata->h_events.pop>0) {
    SP_integer delta, date;

    if (!sweep_min_process(wam,pdata,&delta,&date) ||
        !sweep_min_filter(pdata,delta,date))
      rc = FALSE;
  }
  drain_heaps(pdata);
  SP_ASSERT(!rc || pdata->dlist_check.forward == &pdata->dlist_check);
  SP_ASSERT(no_leak(pdata));
  SP_ASSERT(!rc || min_property(pdata));
  return rc;
}

static SP_BOOL sweep_min_fixpoint(Wam wam,struct multi_cumulative_data *pdata)
{
  int i;
  
  pdata->change = FALSE;
  pdata->schedule &= ~SCHED_MIN;
  if (!sweep_min(wam,pdata))
    return FALSE;
  for (i=0; i<pdata->ntargets; i++) {
    TASK t = TARGET(i);
    SP_integer lb = EST(t);
    int rc = 0;

    while (lb > dvar_min_l(ORIGVAR(t)) ||
	   lb+DUR(t) > dvar_min_l(ENDVAR(t))) {
      rc |= dvar_fix_min_l(ORIGVAR(t),lb);
      rc |= dvar_fix_min_l(ENDVAR(t),lb+DUR(t));
      if (rc < 0)			     
	return FALSE;
      lb = dvar_min_l(ORIGVAR(t));
      if (lb < dvar_min_l(ENDVAR(t))-DUR(t))
	lb = dvar_min_l(ENDVAR(t))-DUR(t);
    }
    if (rc>0) {
      pdata->change = TRUE;
      if (lb!=EST(t)) { /* pruned, hole */
	EST(t) = lb;
	ECT(t) = lb+DUR(t);
	pdata->schedule |= SCHED_MIN;
      }
      if (LaST(t) < ECT(t)) { /* cp after pruning */
	pdata->schedule |= SCHED_MAX;
      }
    }
  }
  return TRUE;
}

/* -1 = failure */
/*  0 = success */
/*  1 = success, and handle PR event for t as well*/
static int
sweep_max_add_task(struct multi_cumulative_data *pdata,
		   SP_integer delta, TASK t)
{
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int rc=0, j;
  
  if (delta < ECT(t))
    return -1;
  if (LCT(t) > delta) {
    LCT(t) = delta;
    LaST(t) = delta-DUR(t);
  }
  if (DUR(t)>0) {
    if (LCT(t) == delta) {
      if (LCT(t) == ECT(t)) {
	for (j=0; j<kr; j++) {
	  DEC_GAP(t,j);
	}
	SET_RING_READY(t);
      } else {
	rc = 1;
	heap_add_singleton(pdata,&pdata->h_events,ECT(t),(t<<3)+TYPE_SCP);
      }
    } else {
      heap_add_singleton(pdata,&pdata->h_events,ECT(t),(t<<3)+TYPE_SCP);
      if (LCT(t) > ECT(t)) {
	heap_add_singleton(pdata,&pdata->h_events,LCT(t),(t<<3)+TYPE_PR);
      } else {
	SET_RING_READY(t);
      }
    }
    if (ECT(t) > LaST(t)) {
      heap_add_singleton(pdata,&pdata->h_events,LaST(t),(t<<3)+TYPE_ECP);
    }
  }
  if (pdata->nbp[t]>0) {	/* also if DUR(t)==0 */
    heap_add_singleton(pdata,&pdata->h_events,LaST(t),(t<<3)+TYPE_RS);
  }
  return rc;
}

static int sweep_max_process(Wam wam,
			     struct multi_cumulative_data *pdata,
			     SP_integer *pdelta, SP_integer *pdate)
{
  SP_integer delta, date;
  int n = heap_extract_batch(pdata,&pdata->h_events,&delta);
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int ti, j, pr, rs=0;
  (void)wam;
  
  for (ti=0, pr=0; ti<n; ti++) {
    SP_integer ev = pdata->batch[ti];
    SP_integer t  = ev >> 3;
    int type = (ev & 0x7);
    SP_integer cur_ecp;
    switch (type) {
    case TYPE_SCP:
      cur_ecp = LaST(t);
      if (IN_RING_CONFLICT(t)) {
	LCT(t) = ECT(t);
	LaST(t) = EST(t);
	SET_RING_READY(t);
      } else if (IN_RING_CHECK(t)) {
	SET_RING_READY(t);
      }
      if (delta > LaST(t)) {
	for (j=0; j<kr; j++) {
	  DEC_GAP(t,j);
	}
	if (cur_ecp >= delta)
	  heap_add_singleton(pdata,&pdata->h_events,LaST(t),(t<<3)+TYPE_ECP); /* reachable */
      }
      break;
    case TYPE_ECP:
      if (delta > LaST(t)) {
	heap_add_singleton(pdata,&pdata->h_events,LaST(t),(t<<3)+TYPE_ECP);
      } else {
	for (j=0; j<kr; j++) {
	  INC_GAP(t,j);
	}
      }
      break;
    case TYPE_RS:
      pdata->rsbuf[rs++] = t;
      break;
    case TYPE_PR:
      pdata->batch[pr++] = t;
      break;
    }
  }
  for (ti=0; ti<rs; ti++) {
    SP_integer t = pdata->rsbuf[ti];
    
    if (IN_RING_CONFLICT(t)) {
      heap_add_singleton(pdata,&pdata->h_events,delta-DUR(t),(t<<3)+TYPE_RS);
    } else if (delta==LaST(t)) {
      struct cons *pp = pdata->predecessors[t];
      while (pp) {
	TASK p1 = pp->head;
	pp = pp->tail;
	if (--pdata->nbs[p1]==0) {
	  switch (sweep_max_add_task(pdata,delta,p1)) {
	  case -1:
	    return FALSE;
	  case 1:
	    pdata->batch[pr++] = p1;
	  }
	}
      }
    } else {
      SP_ASSERT(delta>LaST(t));
      heap_add_singleton(pdata,&pdata->h_events,LaST(t),(t<<3)+TYPE_RS);
    }
  }
  *pdelta = delta;
  *pdate = date = pdata->h_events.pop>0 ? heap_peek_key(&pdata->h_events) : -CLPFD_MAXINT2;
  for (ti=0; ti<pr; ti++) {
    SP_integer t = pdata->batch[ti];
    int rc;

    for (rc=0; rc<kr && !IN_CONFLICT(t,rc); rc++) ;
    if (rc<kr) {
      SET_RING_CONFLICT(rc,t);
    } else if (LaST(t)<date) {
      SET_RING_CHECK(t);
    } else {
      SET_RING_READY(t);
    }      
  }
  return TRUE;
}

static SP_BOOL sweep_max_filter(struct multi_cumulative_data *pdata,
				SP_integer delta, SP_integer date)
{
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int r;
  
  for (r=0; r<kr; r++) {
    if (EXCEEDED(r))
      return FALSE;
  }
  for (r=0; r<kr; r++) {
    SP_integer iscolr = pdata->is_colored[r];
    if (iscolr ? pdata->distinct_colors[r]==pdata->capa[r] :
	pdata->gap0[r] > pdata->gap[r]) {
      struct dlist *check = pdata->dlist_check.forward;
      pdata->gap0[r] = pdata->gap[r];
      while (check != &pdata->dlist_check) {
	TASK t = check->task;
	check = check->forward;
	if (LaST(t) >= delta) {
	  SET_RING_READY(t);
	} else if (IN_CONFLICT(t,r)) {
	  SET_RING_CONFLICT(r,t);
	}
      }
    }
  }
  for (r=0; r<kr; r++) {
    SP_integer iscolr = pdata->is_colored[r];
    if (iscolr ? TRUE : pdata->gap0[r] < pdata->gap[r]) {
      int b = (pdata->distinct_colors[r] < pdata->capa[r]);
      struct dlist *conflict = pdata->dlist_conflict[r].forward;
      pdata->gap0[r] = pdata->gap[r];
      while (conflict != &pdata->dlist_conflict[r]) {
	TASK t = conflict->task;
	conflict = conflict->forward;
	if (iscolr ?
	    (b || pdata->count_colors[kc*r+COLOR(t,r)]>0) :
	    (USE(t,r) <= pdata->gap[r])) {
	  int rc;
	  for (rc=0; rc<kr && !IN_CONFLICT(t,rc); rc++) ;
	  if (rc<kr) {
	    SET_RING_CONFLICT(rc,t);
	  } else {
	    SP_integer cur_ecp = LaST(t);
	    LCT(t) = delta;
	    LaST(t) = delta-DUR(t);
	    if (ECT(t) <= cur_ecp && ECT(t) > LaST(t))
	      heap_add_singleton(pdata,&pdata->h_events,LaST(t),(t<<3)+TYPE_ECP);
	    if (LaST(t) >= date) {
	      SET_RING_READY(t);
	    } else {
	      SET_RING_CHECK(t);
	    }
	  }
	}
      }
    }
  }
  return TRUE;
}

static SP_BOOL sweep_max(Wam wam,struct multi_cumulative_data *pdata)
{
  SP_BOOL rc = TRUE;
  
  sweep_refresh_precedences(pdata);
  gen_max_events(wam,pdata);
  while (rc && pdata->h_events.pop>0) {
    SP_integer delta, date;

    if (!sweep_max_process(wam,pdata,&delta,&date) ||
        !sweep_max_filter(pdata,delta,date))
      rc = FALSE;
  }
  drain_heaps(pdata);
  SP_ASSERT(!rc || pdata->dlist_check.forward == &pdata->dlist_check);
  SP_ASSERT(no_leak(pdata));
  SP_ASSERT(!rc || max_property(pdata));
  return rc;
}

static SP_BOOL sweep_max_fixpoint(Wam wam,struct multi_cumulative_data *pdata)
{
  int i;
  
  pdata->change = FALSE;
  pdata->schedule &= ~SCHED_MAX;
  if (!sweep_max(wam,pdata))
    return FALSE;
  for (i=0; i<pdata->ntargets; i++) {
    TASK t = TARGET(i);
    SP_integer ub = LCT(t);
    int rc = 0;

    while (ub < dvar_max_l(ENDVAR(t)) ||
	   ub-DUR(t) < dvar_max_l(ORIGVAR(t))) {
      rc |= dvar_fix_max_l(ENDVAR(t),ub);
      rc |= dvar_fix_max_l(ORIGVAR(t),ub-DUR(t));
      if (rc < 0)			     
	return FALSE;
      ub = dvar_max_l(ENDVAR(t));
      if (ub > dvar_max_l(ORIGVAR(t))+DUR(t))
	ub = dvar_max_l(ORIGVAR(t))+DUR(t);
    }
    if (rc>0) {
      pdata->change = TRUE;
      if (ub!=LCT(t)) { /* pruned, hole */
	LCT(t) = ub;
	LaST(t) = ub-DUR(t);
	pdata->schedule |= SCHED_MAX;
      }
      if (LaST(t) < ECT(t)) { /* cp after pruning */
	pdata->schedule |= SCHED_MIN;
      }
    }
  }
  return TRUE;
}

static SP_BOOL postpone_successors(struct multi_cumulative_data *pdata, TASK t)
{
  struct cons *ss = pdata->successors[t];
  SP_BOOL rc = TRUE;
  while (ss && rc) {
    TASK s1 = ss->head;
    ss = ss->tail;
    if ((STATUS(s1) & STATUS_SOURCE) && EST(s1) < ECT(t)) {
      EST(s1) = ECT(t);
      ECT(s1) = EST(s1)+DUR(s1);
      if (EST(s1) > LaST(s1) || !postpone_successors(pdata,s1))
	rc = FALSE;
    }
  }
  return rc;
}

static void gen_greedy_events(Wam wam, struct multi_cumulative_data *pdata)
{
  int n = pdata->ntargets+pdata->nsources;
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int i, j;
  
  heap_init(&pdata->h_events, TRUE);
  for (i=0; i<kr; i++) {
    pdata->gap0[i] = pdata->capa[i]; 
    pdata->gap[i] = pdata->capa[i]; 
    pdata->distinct_colors[i] = 0;
    for (j=0; j<kc; j++)
      pdata->count_colors[kc*i+j] = (j==0);
  }
  for (i=0; i<n; i++) {
    TASK t = TARGET(i);
    if (STATUS(t) & STATUS_SOURCE) {
      SET_RING_NONE(t);
      if (DUR(t)>0) {
	if (EST(t)==LaST(t)) {
	  STATUS(t) &= ~STATUS_TARGET;
	  heap_push(&pdata->h_events,LaST(t),(t<<3)+TYPE_SFP);
	  heap_push(&pdata->h_events,ECT(t),(t<<3)+TYPE_EFP);
	} else {
	  heap_push(&pdata->h_events,EST(t),(t<<3)+TYPE_PR);
	  heap_push(&pdata->h_events,LaST(t),(t<<3)+TYPE_SCP);
	  if (LaST(t) < ECT(t)) {
	    heap_push(&pdata->h_events,ECT(t),(t<<3)+TYPE_ECP);
	  }
	}
      } else {
	if (EST(t)<LaST(t) && pdata->predecessors[t]) {
	  heap_push(&pdata->h_events,EST(t),(t<<3)+TYPE_PR);
	} else { /* fixed or no predecessors */
	  STATUS(t) &= ~STATUS_TARGET;	    /* for greedy_property() */
	  LaST(t) = EST(t);
	  LCT(t) = ECT(t);
	}
      }
    }
  }
  heap_sort(wam, &pdata->h_events);
}

/* TRUE if t was fixed eagerly and gap decreased, FALSE otherwise */
static SP_BOOL greedy_fix(Wam wam,
			  struct multi_cumulative_data *pdata,
			  TASK t, SP_integer delta)
{
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int j;
  
  if (EST(t)<delta) {
    SET_RING_READY(t);		/* reachable */
    return FALSE;
  } else {
    STATUS(t) &= ~STATUS_TARGET; /* reachable */
    SET_RING_NONE(t);
    LaST(t) = EST(t);
    LCT(t) = ECT(t);
    TRAIL(TYPE_SFP,t);
    heap_add_singleton(pdata,&pdata->h_events,ECT(t),(t<<3)+TYPE_EFP);
    for (j=0; j<kr; j++) {
      DEC_GAP(t,j);
    }
    return TRUE;
  }
}

static int sweep_greedy_process(Wam wam,
				struct multi_cumulative_data *pdata,
				SP_integer *pdelta, SP_integer *pdate, int *npr)
{
  SP_integer delta;
  int n = heap_extract_batch(pdata,&pdata->h_events,&delta);
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int ti, j, pr;
  
  for (ti=0, pr=0; ti<n; ti++) {
    SP_integer ev = pdata->batch[ti];
    SP_integer t  = ev >> 3;
    int type = (ev & 0x7);
    switch (type) {
    case TYPE_SFP:
      TRAIL(TYPE_SFP,t);
      for (j=0; j<kr; j++) {
	DEC_GAP(t,j);
      }
      break;
    case TYPE_EFP:
      TRAIL(TYPE_EFP,t);
      for (j=0; j<kr; j++) {
	INC_GAP(t,j);
      }
      break;
    case TYPE_SCP:
      if (STATUS(t) & STATUS_TARGET) {
	if (IN_RING_CONFLICT(t)) {
	  EST(t) = LaST(t);	/* reachable */
	  ECT(t) = LCT(t);
	  if (!postpone_successors(pdata,t))
	    return FALSE;
	  if (greedy_fix(wam,pdata,t,delta)) /* always eager */
	    break;			     /* reachable */
	} else if (IN_RING_CHECK(t)) {
	  greedy_fix(wam,pdata,t,delta); /* reachable, never eager */
	}
	TRAIL(TYPE_SCP,t);
	if (delta < ECT(t)) {
	  for (j=0; j<kr; j++) {
	    DEC_GAP(t,j);
	  }
	}
      }
      break;
    case TYPE_PR:
      if (STATUS(t) & STATUS_TARGET) {
	if (EST(t)>delta) {
	  heap_add_singleton(pdata,&pdata->h_events,EST(t),(t<<3)+TYPE_PR);
	} else {
	  /* TODO: special case for DUR=0 */
	  pdata->batch[pr++] = t;
	}
      }
      break;
    }
  }
  *pdelta = delta;
  *pdate = pdata->h_events.pop>0 ? heap_peek_key(&pdata->h_events) : CLPFD_MAXINT2;
  *npr = pr;
  return TRUE;
}

/* Now process PR events */
static SP_BOOL sweep_greedy_phase2(Wam wam,
				   struct multi_cumulative_data *pdata,
				   int tk, SP_integer delta, SP_integer date)
{
  int ti, tj;
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  
  for (ti=0, tj=0; ti<tk; ti++) {
    SP_integer t = pdata->batch[ti];
    int rc;

    for (rc=0; rc<kr && !IN_CONFLICT(t,rc); rc++)
      ;
    if (rc<kr || ECT(t)>date) {
      pdata->batch[tj++] = t;	/* reachable */
    } else {
      greedy_fix(wam,pdata,t,delta); /* reachable, always eager */
    }      
  }
  for (ti=0; ti<tj; ti++) {
    SP_integer t = pdata->batch[ti];
    int rc;

    TRAIL(TYPE_PR,t);
    for (rc=0; rc<kr && !IN_CONFLICT(t,rc); rc++)
      ;
    if (rc<kr) {
      SET_RING_CONFLICT(rc,t);	/* reachable */
    } else {
      SET_RING_CHECK(t);	/* reachable */
    }      
  }
  return TRUE;
}

static SP_BOOL sweep_greedy_filter(Wam wam,
				   struct multi_cumulative_data *pdata,
				   SP_integer delta, SP_integer date)
{
  int kr = pdata->kresources;
  int kc = pdata->kcolors;
  int r;
  SP_BOOL some_fixed = FALSE;
  
  for (r=0; r<kr; r++) {
    if (EXCEEDED(r))
      return FALSE;		/* reachable */
  }
  for (r=0; r<kr; r++) {
    SP_integer iscolr = pdata->is_colored[r];
    int colorgap = (pdata->distinct_colors[r] < pdata->capa[r]);
    struct dlist *conflict = pdata->dlist_conflict[r].forward;
    while (conflict != &pdata->dlist_conflict[r]) {
      TASK t = conflict->task;
      conflict = conflict->forward;
      if (iscolr ?
	  (colorgap || pdata->count_colors[kc*r+COLOR(t,r)]>0) :
	  ( USE(t,r) <= pdata->gap[r] &&
	    delta+DUR(t) <= date &&
	    pdata->gap0[r] < pdata->gap[r])) {
	int rc;
	for (rc=0; rc<kr && !IN_CONFLICT(t,rc); rc++)
	  ;
	if (rc<kr) {
	  SET_RING_CONFLICT(rc,t); /* reachable */
	} else {
	  if (EST(t) < delta) {
	    EST(t) = delta; 	/* reachable */
	    ECT(t) = delta+DUR(t);
	    if (!postpone_successors(pdata,t))
	      return FALSE;
	  }
	  some_fixed |= greedy_fix(wam,pdata,t,delta); /* always eager */
	}
      }
    }
  }
  for (r=0; r<kr; r++) {
    SP_integer iscolr = pdata->is_colored[r];
    if (iscolr || pdata->gap0[r] < pdata->gap[r]) {
      int colorgap = (pdata->distinct_colors[r] < pdata->capa[r]);
      struct dlist *conflict = pdata->dlist_conflict[r].forward;
      while (conflict != &pdata->dlist_conflict[r]) {
	TASK t = conflict->task;
	conflict = conflict->forward;
	if (iscolr ?
	    (colorgap || pdata->count_colors[kc*r+COLOR(t,r)]>0) :
	    USE(t,r) <= pdata->gap[r]) {
	  int rc;
	  for (rc=0; rc<kr && !IN_CONFLICT(t,rc); rc++)
	    ;
	  if (rc<kr) {
	    SET_RING_CONFLICT(rc,t); /* reachable */
	  } else {
	    TRAIL(TYPE_PR,t); 	/* reachable */
	    if (EST(t) < delta) {
	      EST(t) = delta;
	      ECT(t) = delta+DUR(t);
	      if (!postpone_successors(pdata,t))
		return FALSE;
	    }
	    SET_RING_CHECK(t);
	  }
	}
      }
      pdata->gap0[r] = pdata->gap[r];
    }
  }
  for (r=0; r<kr; r++) {
    SP_integer iscolr = pdata->is_colored[r];
    int colorgap = (pdata->distinct_colors[r] < pdata->capa[r]);
    if (iscolr ? !colorgap : (pdata->gap0[r] > pdata->gap[r] || some_fixed)) {
      struct dlist *check = pdata->dlist_check.forward;
      while (check != &pdata->dlist_check) {
	TASK t = check->task;
	check = check->forward;
	if (iscolr ?
	    pdata->count_colors[kc*r+COLOR(t,r)]==0 :
	    USE(t,r) > pdata->gap[r]) {
	  if (ECT(t) <= delta) {
	    greedy_fix(wam,pdata,t,delta); /* reachable */
	  } else {
	    SET_RING_CONFLICT(r,t); /* reachable */
	  }
	}
      }
      pdata->gap0[r] = pdata->gap[r];
    }
  }
  return TRUE;
}

static SP_BOOL sweep_greedy_assign(Wam wam,
				   struct multi_cumulative_data *pdata,
				   SP_integer *pdelta, SP_integer *pdate)
{
  struct dlist *ring;
  int j, kr = pdata->kresources;
  int kc = pdata->kcolors;
  SP_integer delta = *pdelta, date = *pdate, dest = delta; /* backtrack destination */
  TASK winner = -1;
  (void)wam;

  /* 1. select winner */
  ring = pdata->dlist_ready.forward;
  while (ring != &pdata->dlist_ready) {
    TASK t = ring->task;
    ring = ring->forward;

    if (dest >= EST(t)) {
      dest = EST(t);
      winner = t;
    }
  }
  SET_RING_NONE(winner);

  /* 2. backtrack */
  while (delta >= dest) {
    struct trail_item ti = * --pdata->ttop;
    TASK t = TRAIL_ITEM_DATA(&ti);
    
    switch (TRAIL_ITEM_TAG(&ti)) {
    case TYPE_SFP:
      heap_add_singleton(pdata,&pdata->h_events,delta,(t<<3)+TYPE_SFP);
      for (j=0; j<kr; j++) {
	INC_GAP(t,j);
      }
      break;
    case TYPE_EFP:
      heap_add_singleton(pdata,&pdata->h_events,delta,(t<<3)+TYPE_EFP);
      for (j=0; j<kr; j++) {
	DEC_GAP(t,j);
      }
      break;
    case TYPE_SCP:
      if (STATUS(t) & STATUS_TARGET) {
	heap_add_singleton(pdata,&pdata->h_events,delta,(t<<3)+TYPE_SCP);
	if (delta < ECT(t)) {	/* can be either */
	  for (j=0; j<kr; j++) {
	    INC_GAP(t,j);
	  }
	}
      }
      break;
    case TYPE_PR:
      if ((STATUS(t) & STATUS_TARGET) && !IN_RING_NONE(t)) {
	heap_add_singleton(pdata,&pdata->h_events,EST(t),(t<<3)+TYPE_PR);
	SET_RING_NONE(t);
      }
      break;
    case TYPE_DELTA:
      if (delta > TRAIL_ITEM_DATA(&ti)) {
	date = delta;
	delta = TRAIL_ITEM_DATA(&ti);
      }
      break;
    }
  }
  STATUS(winner) &= ~STATUS_TARGET;
  pdata->ttop++;		/* leave a DELTA as topmost item */
  *pdelta = delta;
  *pdate = date;

  SP_ASSERT(pdata->dlist_ready.forward == &pdata->dlist_ready);

  /* 3. can we flush the trail? */
  ring = pdata->dlist_check.forward;
  while (ring != &pdata->dlist_check) {
    TASK t = ring->task;
    ring = ring->forward;
    if (dest > EST(t))
      dest = EST(t);
  }
  if (dest==date && pdata->trail+1 < pdata->ttop) {
    pdata->ttop = pdata->trail+1;
  }

  /* 4. assign winner */
  LaST(winner) = EST(winner);
  LCT(winner) = ECT(winner);
  heap_add_singleton(pdata,&pdata->h_events,EST(winner),(winner<<3)+TYPE_SFP);
  heap_add_singleton(pdata,&pdata->h_events,ECT(winner),(winner<<3)+TYPE_EFP);
  return TRUE;
}

static SP_BOOL sweep_greedy(Wam wam,struct multi_cumulative_data *pdata)
{
  int n = pdata->ntargets+pdata->nsources;
  SP_BOOL rc = TRUE;
  SP_integer delta, date;
  
  /* trail size must be >10 for compress+expand to work */
  n = (n>16 ? n : 16);
  pdata->trail = SP_malloc(n*sizeof(struct trail_item));
  pdata->ttop = pdata->trail;
  pdata->tend = pdata->trail+n;
  gen_greedy_events(wam,pdata);
  date = heap_peek_key(&pdata->h_events);
  delta = date-1;
  TRAIL(TYPE_DELTA,delta);
  while (rc && (pdata->h_events.pop>0 || !EMPTY_RING_READY)) {
    if (!EMPTY_RING_READY) {
      if (!sweep_greedy_assign(wam,pdata,&delta,&date) ||
	  !sweep_greedy_filter(wam,pdata,delta,date))
	rc = FALSE;
    } else {
      if (!sweep_greedy_process(wam,pdata,&delta,&date,&n) ||
	  !sweep_greedy_phase2(wam,pdata,n,delta,date) ||
	  !sweep_greedy_filter(wam,pdata,delta,date))
	rc = FALSE;
    }
    TRAIL(TYPE_DELTA,delta);
    SP_ASSERT(!rc || check_ok(pdata,delta));
  }
  drain_heaps(pdata);
  SP_ASSERT(!rc || (pdata->dlist_check.forward == &pdata->dlist_check));
  SP_ASSERT(!rc || min_property(pdata));
  SP_ASSERT(no_leak(pdata));
  return rc;
}

/* f(N,KR,KC,Tasks,Capas,Greedy1-Greedy2,Ntargets,Nsources,Precedences,_Handle,0) */
/* Each Capa is either colored(N) or cumulative(N) */
void SPCDECL
prolog_fd_multi_cumulative(Wam wam,
			     SP_term_ref State0,
			     SP_term_ref State,
			     SP_term_ref Actions)
{
  TAGGED tasks, handle, tmp;
  int i, j, nbtasks, kr, kc, nbprecedences;
  int ent = -1;			/* disentailed unless otherwise */
  SP_BOOL committed;
  int nactive_items;		/* caches pdata->ntargets + pdata->nsources */
  SP_integer total_size, state_stamp;
  struct multi_cumulative_data *pdata;
  char *ptr;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */

/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct multi_cumulative_data,handle);
    fd.gdata = pdata;
  } else {			/* build persistent state */
				/* nbtasks */
    DerefArg(tmp,X(0),1);		/* get N */
    nbtasks = GetSmall_int(tmp);
    DerefArg(tmp,X(0),2);		/* get KR */
    kr = GetSmall_int(tmp);
    DerefArg(tmp,X(0),3);		/* get KC */
    kc = GetSmall_int(tmp);
    DerefArg(tmp,X(0),9);	/* get Precedences */
    nbprecedences = fd_list_length(tmp);
    if (nbtasks==0) {
      ent = 1;
      goto fail;
    }
    total_size = 17*nbtasks*sizeof(TASK) +
      5*kr*sizeof(TASK) +
      kr*kc*sizeof(TASK) +
      nbtasks*kr*sizeof(TASK) +
      5*nbtasks*sizeof(struct kv) +
      2*nbtasks*sizeof(struct cons *) +			/* NEW */
      (5*nbtasks+2*nbprecedences)*sizeof(struct cons) + /* NEW */
      (2*nbtasks+1)*sizeof(struct dvar) +
      (nbtasks+kr)*sizeof(struct dlist);
    pdata = Palloc(struct multi_cumulative_data, total_size, handle);
    fd.gdata = pdata;
    ptr = (char *)(pdata+1);
    pdata->fix_dvar = (Dvar)ptr;
    ptr += sizeof(struct dvar);
    pdata->origvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->endvar = (Dvar)ptr;
    ptr += nbtasks*sizeof(struct dvar);
    pdata->batch = (SP_integer *)ptr;
    ptr = (char *)(pdata->batch+5*nbtasks);
    pdata->rsbuf = (SP_integer *)ptr;
    ptr = (char *)(pdata->rsbuf+nbtasks);
    pdata->dur = (SP_integer *)ptr;
    ptr = (char *)(pdata->dur+nbtasks);
    pdata->target = (TASK *)ptr;
    ptr = (char *)(pdata->target+nbtasks);
    pdata->status = (SP_integer *)ptr;
    ptr = (char *)(pdata->status+nbtasks);
    pdata->est = (SP_integer *)ptr;
    ptr = (char *)(pdata->est+nbtasks);
    pdata->lst = (SP_integer *)ptr;
    ptr = (char *)(pdata->lst+nbtasks);
    pdata->ect = (SP_integer *)ptr;
    ptr = (char *)(pdata->ect+nbtasks);
    pdata->lct = (SP_integer *)ptr;
    ptr = (char *)(pdata->lct+nbtasks);
    pdata->nbpconst = (SP_integer *)ptr;
    ptr = (char *)(pdata->nbpconst+nbtasks);
    pdata->nbp = (SP_integer *)ptr;
    ptr = (char *)(pdata->nbp+nbtasks);
    pdata->nbsconst = (SP_integer *)ptr;
    ptr = (char *)(pdata->nbsconst+nbtasks);
    pdata->nbs = (SP_integer *)ptr;
    ptr = (char *)(pdata->nbs+nbtasks);
    pdata->predecessors = (struct cons **)ptr;
    ptr = (char *)(pdata->predecessors+nbtasks);
    pdata->successors = (struct cons **)ptr;
    ptr = (char *)(pdata->successors+nbtasks);
    pdata->is_colored = (SP_integer *)ptr;
    ptr = (char *)(pdata->is_colored+kr);
    pdata->capa = (SP_integer *)ptr;
    ptr = (char *)(pdata->capa+kr);
    pdata->distinct_colors = (SP_integer *)ptr;
    ptr = (char *)(pdata->distinct_colors+kr);
    pdata->count_colors = (SP_integer *)ptr;
    ptr = (char *)(pdata->count_colors+kr*kc);
    pdata->color = (SP_integer *)ptr;
    ptr = (char *)(pdata->color+nbtasks*kr);
    pdata->gap = (SP_integer *)ptr;
    ptr = (char *)(pdata->gap+kr);
    pdata->gap0 = (SP_integer *)ptr;
    ptr = (char *)(pdata->gap0+kr);
    pdata->use = pdata->color;
    pdata->h_events.kv = (struct kv *)ptr;
    ptr = (char *)(pdata->h_events.kv+5*nbtasks);
    pdata->free_list = (struct cons *)ptr;
    ptr = (char *)(pdata->free_list+5*nbtasks+2*nbprecedences);
    pdata->dlist_task = (struct dlist *)ptr;
    ptr = (char *)(pdata->dlist_task+nbtasks);
    pdata->dlist_conflict = (struct dlist *)ptr;
    ptr = (char *)(pdata->dlist_conflict+kr);
    SP_ASSERT(ptr==(char *)(pdata+1)+total_size);
    pdata->destructor = multi_cumulative_destructor;
    pdata->daemon = multi_cumulative_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->nrefs = 4*nbtasks+2;
    pdata->refbase = SP_alloc_globrefs(pdata->nrefs);
    pdata->stamp = state_stamp-1; /* DON'T trust initially */
    pdata->nbtasks = nbtasks;
    pdata->kresources = kr;
    pdata->kcolors = kc;

    DerefArg(tmp,X(0),6);	/* get FixAll flag */
    fd_get_var_and_attr(tmp,RefLimAttr);
    dvar_init(pdata->fix_dvar, RefLimAttr, RefLim);
    dvar_attach_daemon(wam, pdata->fix_dvar, pdata, X(1), fd.functor_val);

    DerefArg(tmp,X(0),5);	/* get Capa array */
    i = 0;
    while (TagIsLST(tmp)) {
      TAGGED tint;
      DerefCar(tint,tmp);
      DerefCdr(tmp,tmp);
      pdata->is_colored[i] = (TagToHeadfunctor(tint) == fd.functor_colored);
      DerefArg(tint,tint,1);
      pdata->capa[i++] = GetSmall(tint);
    }
    DerefArg(tasks,X(0),4);	/* get Tasks */
    j = 0;
    for (i=0; i<nbtasks; i++) {
      TASK t = i;
      TAGGED elt;
      DerefCar(elt,tasks);
      DerefCdr(tasks,tasks);
      DerefArg(tmp,elt,1);
      fd_get_var_and_attr(tmp,RefOrigAttr(t));
      DerefArg(tmp,elt,2);
      pdata->dur[i] = GetSmall(tmp);
      DerefArg(tmp,elt,3);
      fd_get_var_and_attr(tmp,RefEndAttr(t));
      DerefArg(tmp,elt,4);
      while (TagIsLST(tmp)) {
	TAGGED t1;
	DerefCar(t1,tmp);
	DerefCdr(tmp,tmp);
	pdata->color[j++] = GetSmall(t1);
      }
      TARGET(i) = t;
      STATUS(t) = (STATUS_SOURCE|STATUS_TARGET);
    }

    for (i=0; i<nbtasks; i++) {
      TASK t = i;
      
      dvar_init(ORIGVAR(t), RefOrigAttr(t), RefOrig(t));
      dvar_attach_daemon(wam, ORIGVAR(t), pdata, X(1), fd.functor_min);
      dvar_init(ENDVAR(t), RefEndAttr(t), RefEnd(t));
      dvar_attach_daemon(wam, ENDVAR(t), pdata, X(1), fd.functor_max);
      pdata->nbpconst[t] = 0;
      pdata->nbsconst[t] = 0;
      pdata->predecessors[t] = NULL;
      pdata->successors[t] = NULL;
    }
    CTagToArg(X(0),4) = atom_nil; /* [MC] 3.12: free for GC */
    /* init free_list */
    total_size = 5*nbtasks + 2*nbprecedences;
    for (i=0; i<total_size-1; i++) {
      (pdata->free_list+i)->tail = pdata->free_list+i+1;
    }
    (pdata->free_list+i)->tail = NULL;
    /* init predecessors and successors */
    DerefArg(tasks,X(0),9);	/* get Precedences */
    while (TagIsLST(tasks)) {
      TAGGED elt, tp, ts;
      int ip, is;
      DerefCar(elt,tasks);
      DerefCdr(tasks,tasks);
      DerefArg(tp,elt,1);
      DerefArg(ts,elt,2);
      ip = GetSmall_int(tp)-1;	/* 1-based */
      is = GetSmall_int(ts)-1;
      pdata->nbpconst[is]++;
      pdata->nbsconst[ip]++;
      CONS(pdata->successors[ip],is,pdata->successors[ip]);
      CONS(pdata->predecessors[is],ip,pdata->predecessors[is]);
    }
    /* init dlists */
    pdata->dlist_ready.ring = RING_READY;
    pdata->dlist_ready.forward = &pdata->dlist_ready;
    pdata->dlist_ready.backward = &pdata->dlist_ready;
    pdata->dlist_check.ring = RING_CHECK;
    pdata->dlist_check.forward = &pdata->dlist_check;
    pdata->dlist_check.backward = &pdata->dlist_check;
    for (i=0; i<nbtasks; i++) {
      pdata->dlist_task[i].ring = RING_NONE;
      pdata->dlist_task[i].task = i;
      pdata->dlist_task[i].forward = &pdata->dlist_task[i];
      pdata->dlist_task[i].backward = &pdata->dlist_task[i];
    }
    for (i=0; i<kr; i++) {
      pdata->dlist_conflict[i].ring = RING_CONFLICT(i);
      pdata->dlist_conflict[i].forward = &pdata->dlist_conflict[i];
      pdata->dlist_conflict[i].backward = &pdata->dlist_conflict[i];
    }
    /* init schedule */
    pdata->schedule = SCHED_MIN+SCHED_MAX;
  }

  				/* RESUME HERE */
  fd.gdata = pdata;
  DerefArg(tmp,X(0),7);
  pdata->ntargets = GetSmall_int(tmp);
  DerefArg(tmp,X(0),8);
  pdata->nsources = GetSmall_int(tmp);
  nactive_items = pdata->ntargets + pdata->nsources;

  if (state_stamp != pdata->stamp) {
    for (i=nactive_items-1; i>=0; i--) {
      TASK t = TARGET(i);
      STATUS(t) |= STATUS_SOURCE;
      if (i<pdata->ntargets)
	STATUS(t) |= STATUS_TARGET;
    }
  }
  pdata->stamp = state_stamp+1;
				/* END OF RESUMPTION */
  dvar_refresh(pdata->fix_dvar);
  for (i=0; i<nactive_items; i++) {
    TASK t = TARGET(i);
    if (STATUS(t) & STATUS_TARGET) {
      dvar_refresh(ORIGVAR(t));
      dvar_refresh(ENDVAR(t));
      pdata->est[t] = dvar_min_l(ORIGVAR(t));
      pdata->lst[t] = dvar_max_l(ORIGVAR(t));
      pdata->ect[t] = dvar_min_l(ENDVAR(t));
      pdata->lct[t] = dvar_max_l(ENDVAR(t));
    }
  }

  if (dvar_min_l(pdata->fix_dvar)==1) { /* FixAll mode */
    if (!sweep_greedy(wam,pdata))
      goto fail;
    for (i=pdata->ntargets-1; i>=0; i--) {
      TASK si = TARGET(i);
      if (dvar_fix_value_l(ORIGVAR(si), EST(si))<0 ||
	  dvar_fix_value_l( ENDVAR(si), EST(si)+DUR(si))<0)
	goto fail;
    }
    for (i=pdata->ntargets-1; i>=0; i--) {
      TASK si = TARGET(i);
      dvar_pruning_done(ORIGVAR(si));
      dvar_pruning_done(ENDVAR(si));
    }

    /* OK to GC from here */
  
    for (i=pdata->ntargets-1; i>=0; i--) {
      TASK si = TARGET(i); 
      dvar_export(ORIGVAR(si));
      dvar_export(ENDVAR(si));
    }
    pdata->ntargets = 0;
    pdata->nsources = 0;
    ent = 1;
    goto exit;
  }

  while (pdata->schedule) {
    if ((pdata->schedule & SCHED_MIN) && !sweep_min_fixpoint(wam,pdata))
      goto fail;
    if ((pdata->schedule & SCHED_MAX) && !sweep_max_fixpoint(wam,pdata))
      goto fail;
  }

  SP_ASSERT(sweep_min_fixpoint(wam,pdata) && !pdata->change);
  SP_ASSERT(sweep_max_fixpoint(wam,pdata) && !pdata->change);

  for (i=0; i<nactive_items; i++) {
    TASK t = TARGET(i);
    SP_ASSERT(EST(t) + DUR(t) == ECT(t));
    SP_ASSERT(LaST(t) + DUR(t) == LCT(t));
    if ((STATUS(t) & STATUS_TARGET) &&
	dvar_is_integer(ORIGVAR(t)) && 
	dvar_is_integer(ENDVAR(t)))
      STATUS(t) &= ~STATUS_TARGET;
  }
  {
    SP_integer est = CLPFD_MAXINT;
    SP_integer lct = -CLPFD_MAXINT;
  
    for (i=0; i<nactive_items; i++) {
      TASK t = TARGET(i);
    
      if (STATUS(t)&STATUS_TARGET) {
	if (est>dvar_min_l(ORIGVAR(t)))
	  est = dvar_min_l(ORIGVAR(t));
	if (lct<dvar_max_l(ENDVAR(t)))
	  lct = dvar_max_l(ENDVAR(t));
      }
    }

    /* forget sources that can no longer prune */
    /* NB. if DUR(t)==0, can still prune via precedences */
    for (i=0; i<nactive_items; i++) {
      TASK t = TARGET(i);
    
      if ((STATUS(t)&(STATUS_SOURCE|STATUS_TARGET))==STATUS_SOURCE &&
	  (dvar_max_l(ENDVAR(t))<=est || lct<=dvar_min_l(ORIGVAR(t)) /*|| DUR(t)==0*/))
	STATUS(t) -= STATUS_SOURCE;
    }
  }

  for (i=pdata->ntargets-1; i>=0; i--) {
    TASK t = TARGET(i);
    dvar_pruning_done(ORIGVAR(t));
    dvar_pruning_done(ENDVAR(t));
  }

  /* OK to GC from here */
  
  for (i=pdata->ntargets-1; i>=0; i--) {
    TASK t = TARGET(i);
    dvar_export(ORIGVAR(t));
    dvar_export(ENDVAR(t));
  }

  /* partition into SOURCE+TARGET and SOURCE */

  {
    int delta;
    int inf = 0;
    int sup = pdata->ntargets-1;
    TASK held = TARGET(sup); /* sup is the hole */
    TASK current = TARGET(inf);
    
    while (inf<=sup) {
      if (STATUS(current) & STATUS_TARGET) {
	TARGET(inf) = current;
	inf++;
	current = (inf>=sup ? held : TARGET(inf));
      } else {
	TARGET(sup) = current;
	sup--;
	current = (inf>=sup ? held : TARGET(sup));
      }
    }
    delta = pdata->ntargets - inf;
    pdata->ntargets -= delta;
    pdata->nsources += delta;
    ent = (pdata->ntargets==0);
  }
  
  /* find the last SOURCE */

  {
    int delta, sup;
    
    for (sup=nactive_items; sup>pdata->ntargets; --sup)
      if (STATUS(TARGET(sup-1)) & STATUS_SOURCE)
        break;
    delta = nactive_items - sup;
    pdata->nsources -= delta;
    nactive_items -= delta;
  }
  CTagToArg(X(0),7) = MakeSmall(pdata->ntargets);
  CTagToArg(X(0),8) = MakeSmall(pdata->nsources);
 exit:
  if (ent==1)
    Pfree;
 fail:
  dvar_export_done(wam,Actions, ent);
}
