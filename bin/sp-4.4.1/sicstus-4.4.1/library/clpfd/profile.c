/* Copyright(C) 2000, Swedish Institute of Computer Science */
/************************************************************/
/* Profile management for cumulative and sweep algorithms.  */
/************************************************************/

#include "fd.h"


static struct {
  int a;
  int b;
  int c;
} crand;

#if _MSC_VER >= 1400            /* [PM] 4.0 MS Visual Studio .NET 2005 (VS8) */
/* [PM] 4.0 Division is not a constant expression with strict floating
   point semantics. Unfortunately there does not seem to be a way to
   turn off strict semantics (which we turn on for other reasons). */
#define DIVISION_IS_NOT_CONSTANT_EXPRESSION 1
#endif  /* _MSC_VER >= 1400 */

#define zx_divisor 30269.0
#define zy_divisor 30307.0
#define zz_divisor 30323.0
#if !DIVISION_IS_NOT_CONSTANT_EXPRESSION
static double const zx = 1.0/zx_divisor;
static double const zy = 1.0/zy_divisor;
static double const zz = 1.0/zz_divisor;
#endif  /* !DIVISION_IS_NOT_CONSTANT_EXPRESSION */

static void creset(void)
{
  crand.a = 27134;
  crand.b = 9213;
  crand.c = 17773;
}

static double crandom(void)
{
  int x, y, z;
  double t;
  
  x = (crand.a*171) % 30269;
  y = (crand.b*172) % 30307;
  z = (crand.c*170) % 30323;
#if DIVISION_IS_NOT_CONSTANT_EXPRESSION
  t = (x/zx_divisor) + (y/zy_divisor) + (z/zz_divisor);
#else
  t = x*zx + y*zy + z*zz;
#endif
  crand.a = x;
  crand.b = y;
  crand.c = z;
  return t-(int)t;
}    



#undef  PROFILE_STATISTICS

#if PROFILE_STATISTICS
static int profile_stat[1000];

static void clear_profile_stat(void)
{
  int i;

  for (i=0; i<1000; i++)
    profile_stat[i] = 0;
}


static void update_profile_stat(struct profile *p)
{
  int i;

  for (i=0; p; i++) {
    p = p->next;
  }
  
  profile_stat[i]++;
  /* printf("profile_length(%d).\n", i); */
}

#endif

void fd_init_profile(Wam wam)
{
  fd.profile_pool = NULL;
  creset();
}


struct profile *fd_empty_profile(void)
{
#if PROFILE_STATISTICS
  update_profile_stat(NULL);
#endif
  return NULL;
}


/* Allocating a new segment */
struct profile *fd_profile_cons(Wam wam,
				SP_integer begin,
				SP_integer end, /* begin < end */
				SP_integer erg, /* erg > 0 */
				struct profile *next) 
/* next==NULL || next->begin > end || next->erg != erg */
{
  struct profile *cons = fd.profile_pool;
  
  if (cons)
    fd.profile_pool = cons->next;
  else {
    TAGGED *h;
    NumstackAlloc(sizeof(*cons)/sizeof(SP_integer),h);
    cons = (struct profile *)h;
  }
  cons->begin = begin;
  cons->end = end;
  cons->erg = erg;
  cons->next = next;
  return cons;
}


/* true if two segments overlap */
#define OVERLAP(B1,E1,B2,E2) \
((E1) > (B2) && (E2) > (B1)) \

/* Coalesce or else appending a new segment to tail of profile */
#define profile_emit(WAM,B,E,Y)					      \
      if (tail->end==(B) && tail->erg==(Y)) { \
        tail->end = (E); \
      } else if (Y) { \
	tail->next = fd_profile_cons(WAM,(B), (E), (Y), NULL);	 \
      tail = tail->next; \
      } \

#define profile_emit_fast(WAM,B,E,Y) {							\
    tail->next = fd_profile_cons(WAM,(B), (E), (Y), NULL);	 \
      tail = tail->next; \
      } \


/* Compute the temporal relation between two segments. */
static INLINE unsigned int profile_cmp(SP_integer b1, SP_integer e1,
				       SP_integer b2, SP_integer e2)
{
  if (b1 < b2) {
    if (e1 <= b2)
      return FD_BEFORE;		/* or MEETS */
    else if (e1 < e2)
      return FD_OVERLAPS;
    else if (e1 == e2)
      return FD_FINISHED_BY;
    else
      return FD_CONTAINS;
  } else if (b1 == b2) {
    if (e1 < e2)
      return FD_STARTS;
    else if (e1 == e2)
      return FD_EQUALS;
    else
      return FD_STARTED_BY;
  } else {
    if (e1 < e2)
      return FD_DURING;
    else if (e1 == e2)
      return FD_FINISHES;
    else if (b1 < e2)
      return FD_OVERLAPPED_BY;
    else
      return FD_AFTER;		/* or MET_BY */
  }
}


/* Disposing of a list of segment */
void fd_profile_dispose(Wam wam,
			struct profile *cons)
{
  struct profile *tail = cons;

  if (tail) {
    while (tail->next)
      tail = tail->next;
    tail->next = fd.profile_pool;
    fd.profile_pool = cons;
  }
}

/* Add an interval to profile with structure sharing,
   i.e. dispose of any parts of profile that can't be shared.
   */
struct profile *fd_profile_update(Wam wam,
				  struct profile *p,
				  SP_integer b2, SP_integer e2,
				  SP_integer y2)
{
  SP_integer b1, e1;
  int y1;
  struct profile *tmp;
  struct profile part;
  struct profile *tail = &part;

  part.end = CLPFD_MAXINT;
  part.erg = 0;
  part.next = NULL;
  
list:
  if (p==NULL)
    goto null_and_head_tail;
  b1 = p->begin;
  e1 = p->end;
  y1 = (int)p->erg;
  tmp = p;
  p = p->next;
  tmp->next = fd.profile_pool;
  fd.profile_pool = tmp;
  switch (profile_cmp(b1,e1,b2,e2)) {
  case FD_BEFORE:		/* or MEETS */
    profile_emit(wam, b1,e1,y1);
    goto list;
  case FD_OVERLAPS:
    profile_emit(wam, b1,b2,y1);
    profile_emit(wam, b2,e1,y1+y2);
    b2 = e1;
    goto list;
  case FD_FINISHED_BY:
    profile_emit(wam, b1,b2,y1);
    profile_emit(wam, b2,e1,y1+y2);
    goto list_and_null;
  case FD_CONTAINS:
    profile_emit(wam, b1,b2,y1);
    profile_emit(wam, b2,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_null;
  case FD_STARTS:
    profile_emit(wam, b1,e1,y1+y2);
    b2 = e1;
    goto list;
  case FD_EQUALS:
    profile_emit(wam, b1,e1,y1+y2);
    goto list_and_null;
  case FD_STARTED_BY:
    profile_emit(wam, b1,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_null;
  case FD_DURING:
    profile_emit(wam, b2,b1,y2);
    profile_emit(wam, b1,e1,y1+y2);
    b2 = e1;
    goto list;
  case FD_FINISHES:
    profile_emit(wam, b2,b1,y2);
    profile_emit(wam, b1,e1,y1+y2);
    goto list_and_null;
  case FD_OVERLAPPED_BY:
    profile_emit(wam, b2,b1,y2);
    profile_emit(wam, b1,e2,y1+y2);
    b1 = e2;
    goto head_tail_and_null;
  case FD_AFTER:		/* or MET_BY */
    profile_emit(wam, b2,e2,y2);
    goto head_tail_and_null;
  }
null_and_head_tail:
  profile_emit(wam, b2,e2,y2);
  tail->next = NULL;
  goto ret;
head_tail_and_null:
  profile_emit(wam, b1,e1,y1);
list_and_null:
  tail->next = p;
 ret:
#if PROFILE_STATISTICS
  update_profile_stat(part.next);
#endif
  return part.next;
}



/* TRUE iff the "energy" is zero somewhere inside */
SP_BOOL fd_profile_zero_at(struct profile *p,
		     SP_integer b1, SP_integer e1, SP_integer *wit)
{
  SP_integer b2, e2, bb = 0, ee = 0;
  int count = 0;

list:
  if (!p)
    goto before;
  b2 = p->begin;
  e2 = p->end;
  p = p->next;
  while (p && e2 == p->begin) {	/* extend nonzero segment [b2,e2) */
    e2 = p->end;
    p = p->next;
  }
  switch (profile_cmp(b1,e1,b2,e2)) {
  case FD_BEFORE:
  before:
    if (1.0 / ++count > crandom()) {
      bb = b1;
      ee = e1;
    }
    break;
  case FD_OVERLAPS:
  case FD_FINISHED_BY:
    if (1.0 / ++count > crandom()) {
      bb = b1;
      ee = b2;
    }
    break;
  case FD_CONTAINS:
    if (1.0 / ++count > crandom()) {
      bb = b1;
      ee = b2;
    }
    b1 = e2;
    goto list;
  case FD_STARTS:
  case FD_EQUALS:
  case FD_DURING:
  case FD_FINISHES:
    break;
  case FD_STARTED_BY:
  case FD_OVERLAPPED_BY:
    b1 = e2;
    goto list;
  case FD_MET_BY:
  case FD_AFTER:
    goto list;
  }
  if (bb==ee)
    return FALSE;
  if (bb+1 < ee)		/* otherwise, unit interval */
    bb += (SP_integer)((double)(ee-bb)*crandom());
  *wit = bb;
  return TRUE;
}

/* TRUE iff the "energy" is nonzero somewhere inside */
SP_integer fd_profile_max(struct profile *p,
		 SP_integer b2, SP_integer e2)
{
  SP_integer b1=e2-1, e1, max=0;

  while (p && b1<e2) {
    b1 = p->begin;
    e1 = p->end;
    if (b1<e2 && b2<e1 && max<p->erg)
      max = p->erg;
    p = p->next;
  }
  return max;
}

/* TRUE iff limit not exceeded */
SP_BOOL fd_profile_at_most(struct profile *p,
			SP_integer limit)
{
  SP_BOOL ok = TRUE;

  while (p && ok) {
    ok = (p->erg<=limit);
    p = p->next;
  }
  return ok;
}

SP_BOOL fd_profile_next(struct profile *prof,
		  SP_integer *bp, SP_integer *ep, SP_integer *hp,
		  struct profile **nextp)
{
  if (!prof)
    return FALSE;
  *bp = prof->begin;
  *ep = prof->end;
  *hp = prof->erg;
  *nextp = prof->next;
  return TRUE;
}

/* access "array" contents at index i */
SP_integer fd_profile_get(struct profile *p,
			  SP_integer ix)
{
  for (; p; p=p->next) {
    if (ix<p->begin)
      return 0;
    else if (ix<p->end)
      return p->erg;
  }
  return 0;
}

struct profile *fd_profile_copy(Wam wam, struct profile *p)
{
  if (!p) {
    return p;
  } else {
    return fd_profile_cons(wam, p->begin, p->end, p->erg,
			   fd_profile_copy(wam, p->next));
  }
}
