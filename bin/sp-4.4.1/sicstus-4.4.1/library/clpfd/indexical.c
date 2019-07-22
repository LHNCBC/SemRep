/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "fd_insn.h"
#include "dvars.h"

#if DBG
#if defined(_MSC_VER)
/* [PM] 3.9.2b1 if using MS C compile we use inline assembly to break into the debugger (see support2.h) */
#else  /* !_MSC_VER */
#undef DEBUG_BREAK              /* from support2.h */
#define DEBUG_BREAK() clpfd_debug_break()
#endif /* defined(_MSC_VER) */
#endif /* DBG */


#define PositiveBound(B) (Tgez(B) && TagIsSmall(B))

#define Top (top[-1])

#define Pop (*(--top))

#define Push(X) (*top++ = (X))

static SP_BOOL fd_infinite(TAGGED d1)
{
  TAGGED r1;

  r1 = CTagToCar(d1);
  d1 = CTagToCdr(d1);
  if (RangeMin(r1)==Inf)
    return TRUE;
  while (d1!=EmptySet) {
    r1 = CTagToCar(d1);
    d1 = CTagToCdr(d1);
  }
  if (RangeMax(r1)==Sup)
    return TRUE;
  return FALSE;
}

#define EMIT_INTERVAL(H,B,E)			\
      *valuep = MakeList(H);			\
      H[0] = MakeList(H+2);			\
      valuep = H+1;				\
      H[2] = (B);				\
      H[3] = (E);				\
      H += 4;    				\

/* Compute the set [0, value, ..., count*value] */

TAGGED
fd_multiples(Wam wam, int count, SP_integer value)
{
  TAGGED set;
  int i;
  
  if (value==1)
    set = fd_interval(wam, TaggedZero, MakeSmall(count));
  else {
    TAGGED *h;
    
    NumstackAlloc(4*count+4,h);
    set = MakeList(h);
    for (i=0; i<=count; i++, h+=4) {
      h[0] = MakeList(h+2);
      h[1] = MakeList(h+4);
      h[2] = h[3] = MakeSmall(i*value);
    }
    h[-3] = EmptySet;
  }
  return set;
}

/* d1 x d2 -> {s+t | s in d1 & t in d2 & lb <= s+t <= ub} */
TAGGED fd_plus(Wam wam,
	       TAGGED d1, TAGGED d2,
	       TAGGED lb, TAGGED ub)
{
  int i, j, count;
  TAGGED t1, t2, *h, *h0, *h1;
  TAGGED value;
  TAGGED *valuep = &value;

  if (d1==EmptySet || d2==EmptySet)
    return EmptySet;
  for (i=0, t1=d1; t1!=EmptySet; i+=1)
    t1 = CTagToCdr(t1);
  for (j=0, t2=d2; t2!=EmptySet; j+=i)
    t2 = CTagToCdr(t2);
				/* collect start and end events */
  NumstackAlloc(6*j,h);
  h0 = h;
  count = 0;
  for (t1=d1; t1!=EmptySet;) {
    TAGGED min1, max1, r1;
    r1 = CTagToCar(t1);
    t1 = CTagToCdr(t1);
    min1 = RangeMin(r1);
    max1 = RangeMax(r1);
    for (t2=d2; t2!=EmptySet;) {
      TAGGED min2, max2, r2;
      r2 = CTagToCar(t2);
      t2 = CTagToCdr(t2);
      min2 = fd_safe_plus(min1,RangeMin(r2));
      max2 = fd_safe_plus(max1,RangeMax(r2));
      if (FDgt(min2,ub))
	break;
      if (FDle(lb,max2)) {
	if (TagIsSmall(min2))
	  *h++ = min2;
	else
	  count++;
	if (TagIsSmall(max2)) {
	  TAGGED t = (max2==TaggedHigh ? TaggedHigh+3 : max2+IStep(1)+1); /* SPRM 13738 */
	  *h++ = t;
	}
      }
    }
  }

				/* perform sweep */
  h1 = h;
  t1 = Inf;
  fd_qsort_asc_long(wam, (SP_integer *)h0,(int)(h1-h0));
  while (h0<h1) {
    TAGGED event = *h0++;
    if (TagIsSmall(event)) {	/* start event */
      if (count++ == 0) {
	t1 = event;
      }
    } else {			/* end event */
      if (--count == 0) {
	TAGGED t = (event==TaggedHigh+3 ? TaggedHigh : event-IStep(1)-1); /* SPRM 13738 */
	EMIT_INTERVAL(h,t1,t);
      }
    }
  }
  if (count>0) {
    EMIT_INTERVAL(h,t1,Sup);
  }
  *valuep = EmptySet;
  return value;
}

/* d1 x d2 -> {s-t | s in d1 & t in d2 & lb <= s-t <= ub} */
TAGGED fd_minus(Wam wam,
		TAGGED d1, TAGGED d2,
		TAGGED lb, TAGGED ub)
{
  int i, j, count;
  TAGGED t1, t2, *h, *h0, *h1;
  TAGGED value;
  TAGGED *valuep = &value;

  if (d1==EmptySet || d2==EmptySet)
    return EmptySet;
  for (i=0, t1=d1; t1!=EmptySet; i+=1)
    t1 = CTagToCdr(t1);
  for (j=0, t2=d2; t2!=EmptySet; j+=i)
    t2 = CTagToCdr(t2);
				/* collect start and end events */
  NumstackAlloc(6*j,h);
  h0 = h;
  count = 0;
  for (t1=d1; t1!=EmptySet;) {
    TAGGED min1, max1, r1;
    r1 = CTagToCar(t1);
    t1 = CTagToCdr(t1);
    min1 = RangeMin(r1);
    max1 = RangeMax(r1);
    for (t2=d2; t2!=EmptySet;) {
      TAGGED min2, max2, r2;
      r2 = CTagToCar(t2);
      t2 = CTagToCdr(t2);
      min2 = fd_safe_minus(min1,RangeMax(r2));
      max2 = fd_safe_minus(max1,RangeMin(r2));
      if (FDle(lb,max2) && FDle(min2,ub)) {
	if (TagIsSmall(min2))
	  *h++ = min2;
	else
	  count++;
	if (TagIsSmall(max2)) {
	  TAGGED t = (max2==TaggedHigh ? TaggedHigh+3 : max2+IStep(1)+1); /* SPRM 13738 */
	  *h++ = t;
	}
      }
    }
  }
				/* perform sweep */
  h1 = h;
  t1 = Inf;
  fd_qsort_asc_long(wam, (SP_integer *)h0,(int)(h1-h0));
  while (h0<h1) {
    TAGGED event = *h0++;
    if (TagIsSmall(event)) {	/* start event */
      if (count++ == 0) {
	t1 = event;
      }
    } else {			/* end event */
      if (--count == 0) {
	TAGGED t = (event==TaggedHigh+3 ? TaggedHigh : event-IStep(1)-1); /* SPRM 13738 */
	EMIT_INTERVAL(h,t1,t);
      }
    }
  }
  if (count>0) {
    EMIT_INTERVAL(h,t1,Sup);
  }
  *valuep = EmptySet;
  return value;
}

/* support for FD_SETMOD */

static TAGGED 
fd_setmod_interval(Wam wam,
		   TAGGED tmin,
		   TAGGED tmax,
		   SP_integer divisor)
{
  SP_integer min=GetSmall(tmin), max=GetSmall(tmax);
  SP_integer absd = (divisor>=0 ? divisor : -divisor);

  if (tmin==Inf || tmax==Sup || max-min+1>=absd) {
    if (divisor<0)
      return fd_interval(wam, MakeSmall(divisor+1),TaggedZero);
    else
      return fd_interval(wam, TaggedZero,MakeSmall(divisor-1));
  } else {
    SP_integer mod1 = min % absd;
    SP_integer mod2 = max % absd;
    if (mod1 != 0 && (mod1 < 0) != (divisor < 0))
      mod1 += divisor;
    if (mod2 != 0 && (mod2 < 0) != (divisor < 0))
      mod2 += divisor;
    if (mod1 <= mod2)
      return fd_interval(wam, MakeSmall(mod1), MakeSmall(mod2));
    else if (divisor<0)
      return fd_union_dest(fd_interval(wam, MakeSmall(divisor+1),MakeSmall(mod2)),
			   fd_interval(wam, MakeSmall(mod1),TaggedZero));
    else
      return fd_union_dest(fd_interval(wam, TaggedZero,MakeSmall(mod2)),
			   fd_interval(wam, MakeSmall(mod1),MakeSmall(divisor-1)));
  }
}

static TAGGED 
fd_setmod(Wam wam, TAGGED t1, TAGGED t2)
{
  TAGGED value = EmptySet;
  SP_integer divisor = GetSmall(t2);
  
  if (divisor==0)
    return ERRORTAG;
    
  while (t1!=EmptySet) {
    TAGGED r1 = CTagToCar(t1);
    t1 = CTagToCdr(t1);

    value = fd_union_dest(value,fd_setmod_interval(wam, RangeMin(r1),RangeMax(r1),divisor));
  }
  return value;
}

/* support for FD_SETREM */

static TAGGED 
fd_setrem_interval(Wam wam, TAGGED tmin, TAGGED tmax, SP_integer absd)
{
  SP_integer min=GetSmall(tmin), max=GetSmall(tmax);
  if (tmin==Inf || tmax==Sup || max-min+1>=absd) {
    if (FDlt(tmin,TaggedZero))
      return fd_interval(wam, MakeSmall(1-absd),TaggedZero);
    else
      return fd_interval(wam, TaggedZero,MakeSmall(absd-1));
  } else {
    SP_integer rem1 = min % absd;
    SP_integer rem2 = max % absd;
    if (rem1 <= rem2)
      return fd_interval(wam, MakeSmall(rem1), MakeSmall(rem2));
    else if (min<0)
      return fd_union_dest(fd_interval(wam, MakeSmall(1-absd),MakeSmall(rem2)),
			   fd_interval(wam, MakeSmall(rem1),TaggedZero));
    else
      return fd_union_dest(fd_interval(wam, TaggedZero,MakeSmall(rem2)),
			   fd_interval(wam, MakeSmall(rem1),MakeSmall(absd-1)));
  }
}

static TAGGED 
fd_setrem(Wam wam, TAGGED t1, TAGGED t2)
{
  TAGGED value = EmptySet;
  SP_integer divisor = GetSmall(t2);
  
  if (divisor==0)
    return ERRORTAG;
  else if (divisor<0)
    divisor = -divisor;
    
  while (t1!=EmptySet) {
    TAGGED tmin, tmax, r1, subset;

    r1 = CTagToCar(t1);
    t1 = CTagToCdr(t1);
    tmin = RangeMin(r1);
    tmax = RangeMax(r1);

    if (FDlt(tmin,TaggedZero) && FDlt(TaggedZero,tmax))
      subset = fd_union_dest(fd_setrem_interval(wam, tmin,TaggedZero,divisor),
			     fd_setrem_interval(wam, TaggedOne,tmax,divisor));
    else
      subset = fd_setrem_interval(wam, tmin,tmax,divisor);
    value = fd_union_dest(value,subset);
  }
  return value;
}

/*** telling ***/

/* Precondition:
   X(EVAL_ARITY) = dest_attribute;
*/
int fd_tell_value(Wam wam, TAGGED value)
{
  TAGGED *h, t1;
  TAGGED domain, mutable;
  int why = MASK_DOM+MASK_MINMAX+MASK_VAL;

  AttrToDomM(X(EVAL_ARITY),mutable);
  domain = RefMutable(mutable);
  if (!TagIsSmall(value))
    fd.fd_overflow = (value==Sup ? 2 : 1);
  if (DomainMin(domain)!=value)
    why += MASK_MIN;
  if (DomainMax(domain)!=value)
    why += MASK_MAX;
  
  if (TagToSTR(domain) >= w->global_uncond && TagToLST(DomainSet(domain)) >= w->global_uncond) { /* can safely smash domain _and set_ */
    TAGGED old = DomainSet(domain);
    DomainMin(domain) = value;
    DomainMax(domain) = value;
    DomainSize(domain) = TaggedOne;
    CTagToCar(old) = MakeList(&DomainMin(domain));
    CTagToCdr(old) = EmptySet;
  } else if (value==TaggedZero) {
    TAGGED dom_zero = RefTerm(CLPFD_DATA_TERM_REF) + WD(15);
    DECL_UPDATE_MUTABLE;
    AttrToDomM(X(EVAL_ARITY),mutable); /* refresh dest_attribute */
    FD_UPDATE_MUTABLE(dom_zero, mutable);
  } else if (value==TaggedOne) {
    TAGGED dom_one = RefTerm(CLPFD_DATA_TERM_REF) + WD(15+18);
    DECL_UPDATE_MUTABLE;
    AttrToDomM(X(EVAL_ARITY),mutable); /* refresh dest_attribute */
    FD_UPDATE_MUTABLE(dom_one, mutable);
  } else {
    DECL_UPDATE_MUTABLE;
    RequireHeap(7,EVAL_ARITY+2);	/* GC */
    AttrToDomM(X(EVAL_ARITY),mutable); /* refresh dest_attribute */
    h = w->global_top;
    t1 = MakeList(h);
    h[0] = t1+WD(4);
    h[1] = EmptySet;
    h[2] = functor_dom4;
    h[3] = t1;
    h[4] = value;
    h[5] = value;
    h[6] = TaggedOne;
    w->global_top = h+7;
    FD_UPDATE_MUTABLE(MakeStructure(h+2), mutable);
  }
  return why;
}


/* Precondition:
   X(EVAL_ARITY) = dest_attribute;
*/
int fd_tell_interval(Wam wam,
		     TAGGED min, TAGGED max,
		     int why)
{
  TAGGED *h, t1;
  TAGGED card, domain, mutable;
  
  if (min==max)
    return fd_tell_value(wam, min); /* GC */
  if (AreSmall(min,max)) {
    SP_integer size = GetSmall(max)-GetSmall(min)+1;
    card = (IntIsSmall(size) ? MakeSmall(size) : Sup);
  } else {
    card = Sup;
  }
  AttrToDomM(X(EVAL_ARITY),mutable);
  domain = RefMutable(mutable);
  if (min==Sup)
    fd.fd_overflow = 2;		/* FD integer overflow */
  else if (max==Inf)
    fd.fd_overflow = 1;		/* FD integer underflow */
  if (TagToSTR(domain) >= w->global_uncond && TagToLST(DomainSet(domain)) >= w->global_uncond) { /* can safely smash domain _and set_ */
    TAGGED old = DomainSet(domain);
    DomainMin(domain) = min;
    DomainMax(domain) = max;
    DomainSize(domain) = card;
    CTagToCar(old) = MakeList(&DomainMin(domain));
    CTagToCdr(old) = EmptySet;
  } else {
    DECL_UPDATE_MUTABLE;
    RequireHeap(7,EVAL_ARITY+2);	/* GC */
    AttrToDomM(X(EVAL_ARITY),mutable); /* refresh dest_attribute */
    h = w->global_top;
    t1 = MakeList(h);
    h[0] = t1+WD(4);
    h[1] = EmptySet;
    h[2] = functor_dom4;
    h[3] = t1;
    h[4] = min;
    h[5] = max;
    h[6] = card;
    w->global_top = h+7;
    FD_UPDATE_MUTABLE(MakeStructure(h+2), mutable);
  }
  SP_ASSERT(card!=TaggedOne);
  return why;
}


/* Precondition:
   X(EVAL_ARITY) = dest_attribute;
   X(EVAL_ARITY+1) = dest_var;

   Precondition: new is an interval, or does not point to the heap
*/
int fd_tell(Wam wam, TAGGED new)
{
  int why = MASK_DOM;
  TAGGED *h;
  TAGGED min=0, max, card, domain, dmin, dmax, mutable;
  
  DerefAttribute(domain,X(EVAL_ARITY));
  dmin = DomainMin(domain);
  dmax = DomainMax(domain);  
  if (CTagToCdr(new)==EmptySet) { /* new domain is an interval */
    new = CTagToCar(new);
    min = RangeMin(new);
    max = RangeMax(new);
    if (min!=dmin)
      why |= MASK_MIN+MASK_MINMAX;
    if (max!=dmax)
      why |= MASK_MAX+MASK_MINMAX;
    return fd_tell_interval(wam, min,max,why);
  } else {
    TAGGED d2=new, r2, b2, e2;
    SP_integer size=0;

    do {
      r2 = CTagToCar(d2); d2 = CTagToCdr(d2);
      b2 = RangeMin(r2);  e2 = RangeMax(r2);
      if (size==0)
	min = b2;
      if (IntIsSmall(size) && AreSmall(b2,e2))
	size += GetSmall(e2)-GetSmall(b2)+1;
      else
	size = CLPFD_MAXINT2;
    } while (d2!=EmptySet);
    max = e2;
    card = (IntIsSmall(size) ? MakeSmall(size) : Sup);

    if (min!=dmin)
      why |= MASK_MIN+MASK_MINMAX;
    if (max!=dmax)
      why |= MASK_MAX+MASK_MINMAX;
    new = fd_globalize(wam, new,5,EVAL_ARITY+2); /* can GC */
    AttrToDomM(X(EVAL_ARITY),mutable);
    domain = RefMutable(mutable);
    {
      if (TagToSTR(domain) >= w->global_uncond) {
	h = TagToArg(domain,0);	  
	h[1] = new;
	h[2] = min;
	h[3] = max;
	h[4] = card;
      } else {
	DECL_UPDATE_MUTABLE;
	h = w->global_top;
	h[0] = functor_dom4;
	h[1] = new;
	h[2] = min;
	h[3] = max;
	h[4] = card;
	w->global_top = h+5;
	FD_UPDATE_MUTABLE(MakeStructure(h), mutable);
      }
    }
  }
  SP_ASSERT(card!=TaggedOne);
  return why;
}

/* Special case of the above for indexicals.
   Preconditions:
   X(EVAL_ARITY) = dest_attribute;
   X(EVAL_ARITY+1) = dest_var;
   new has NOT been fd_localized.
*/
int fd_tell_unsafe(Wam wam, TAGGED new)
{
  int why = MASK_DOM;
  TAGGED *h;
  TAGGED min=0, max, card, domain, dmin, dmax, mutable;
  
  DerefAttribute(domain,X(EVAL_ARITY));
  dmin = DomainMin(domain);
  dmax = DomainMax(domain);  
  if (CTagToCdr(new)==EmptySet) { /* new domain is an interval */
    new = CTagToCar(new);
    min = RangeMin(new);
    max = RangeMax(new);
    if (min!=dmin)
      why |= MASK_MIN+MASK_MINMAX;
    if (max!=dmax)
      why |= MASK_MAX+MASK_MINMAX;
    return fd_tell_interval(wam, min,max,why);
  } else {
    TAGGED d2=new, r2, b2, e2;
    SP_integer size=0;

    do {
      r2 = CTagToCar(d2); d2 = CTagToCdr(d2);
      b2 = RangeMin(r2);  e2 = RangeMax(r2);
      if (size==0)
	min = b2;
      if (size>=0 && AreSmall(b2,e2))
	size += GetSmall(e2)-GetSmall(b2)+1;
      else
	size = -1;
    } while (d2!=EmptySet);
    max = e2;
    card = (size>=0 ? MakeSmall(size) : Sup);

    if (min!=dmin)
      why |= MASK_MIN+MASK_MINMAX;
    if (max!=dmax)
      why |= MASK_MAX+MASK_MINMAX;
    new = fd_globalize_unsafe(wam, new,5,EVAL_ARITY+2); /* can GC */
    AttrToDomM(X(EVAL_ARITY),mutable);
    domain = RefMutable(mutable);
    {
      if (TagToSTR(domain) >= w->global_uncond) {
	h = TagToArg(domain,0);	  
	h[1] = new;
	h[2] = min;
	h[3] = max;
	h[4] = card;
      } else {
	DECL_UPDATE_MUTABLE;
	h = w->global_top;
	h[0] = functor_dom4;
	h[1] = new;
	h[2] = min;
	h[3] = max;
	h[4] = card;
	w->global_top = h+5;
	FD_UPDATE_MUTABLE(MakeStructure(h), mutable);
      }
    }
  }
  SP_ASSERT(card!=TaggedOne);
  return why;
}


/* This function performs two actions:
   1. On entry, why is a bitmask of the effect of the pruning just done.
      This is and:ed with the suspension list bitmask, and or:ed with
      MASK_SINGLETON if appropriate.
   2. If the domain is now a singleton, the variable in question is bound. 

   Precondition:
   X(EVAL_ARITY) = dest_attribute;
   X(EVAL_ARITY+1) = dest_var;
*/
static int finish_pruning(Wam wam, int why)
{
  TAGGED mutable;
  int mask;
  TAGGED dest_var;
  
  fd.prunings++;
  AttrToSuspM(X(EVAL_ARITY),mutable);
  dest_var = X(EVAL_ARITY+1);
  mask = GetSmall_int(CTagToArg(RefMutable(mutable),2)); /* bitmask of susp. lists */
  if (why & MASK_VAL) {
    TAGGED value;

    DerefAttribute(value,X(EVAL_ARITY));
    value = DomainMin(value);
				/* purify and bind argument */
    DerefSwitch(dest_var,
    {
      if (single_attribute(dest_var,fd.fd_module)) {
	BindHVA(dest_var,value);
      } else {
 	TAGGED *h;
	struct propagator *cur = fd.current_propagator;
 	
 	RequireHeap(3,EVAL_ARITY+2);
 	dest_var = X(EVAL_ARITY+1);
 	DerefSwitch(dest_var,;);
 	h = w->global_top;
 	h[0] = functor_minus;
 	h[1] = dest_var;
 	h[2] = value;
 	w->global_top = h+3;
 	fd_enqueue_wake(wam, MakeStructure(h));
      }
    });
    return (why & mask) + MASK_SINGLETON;
  } else
    return (why & mask);
}

/*** predicates ***/

/* $fd_tell(wam, Var, +FDSet, -Effect)
   Effect is (value returned by fd_tell(wam, ))/\7.
   Similar to prune_set().
*/
SP_integer SPCDECL
prolog_fd_tell(Wam wam,
	       SP_term_ref VarR,
	       SP_term_ref SetR)
{
  TAGGED domain, old, dest_attribute, src, var;
  int why = 0;

  var = RefTerm(VarR);
  src = RefTerm(SetR);
  DerefNonvar(src);
  DerefSwitch(var,goto prune;);
  if (!TagIsSmall(var) || !fd_member(var,src))
    goto fail;
  else
    goto ret;
 prune:
  dest_attribute = fd_check_argument(wam, var,Inf,Sup,Sup);
  w->numstack_end = NULL;
  DerefAttribute(domain,dest_attribute);
  old = DomainSet(domain);
  switch (fd_compare(old,src)) {
  case FDI_SUBSET:
  case FDI_EQUAL:
    break;
  case FDI_DISJOINT:
    goto fail;
  case FDI_SUPERSET:
    X(3) = dest_attribute;		/* preserve over GC */
    X(4) = var;
    why = fd_tell_unsafe(wam,src); /* GC */
    break;
  case FDI_INTERSECT:
    X(3) = dest_attribute;		/* preserve over GC */
    X(4) = var;
    why = fd_tell_unsafe(wam,fd_intersection(wam,old,src)); /* GC */
  }
 ret:
  return why&7;
 fail:
  SP_fail();
  return 0;
}


/* Guts of below, used in fd_prune_and_enqueue(wam, ).
   Precondition:
   X(EVAL_ARITY) = dest_attribute;
   X(EVAL_ARITY+1) = dest_var;
*/
void
fd_told(Wam wam, int why, TAGGED value)
{
				/* maybe bind; filter why */
  why = finish_pruning(wam, why);
  if (why > 0) {
    TAGGED mutable;

    AttrToSuspM(X(EVAL_ARITY),mutable);
    fd_enqueue_all(wam, why, mutable, value);
  }
}

static  int fd_prune_and_enqueue(Wam wam, 
				 SP_term_ref ActionsR,
				 SP_term_ref GlobalR)
{
  TAGGED dest_var, glob;
  TAGGED statmut, status;
  DECL_UPDATE_MUTABLE;

  fd_sync(wam);
  X(0) = RefTerm(ActionsR);
  DerefNonvar(X(0));
  while (TagIsLST(X(0))) {
    TAGGED item;
    
    DerefCar(item,X(0));
    DerefCdr(X(0),X(0));
    if (TagIsATM(item)) {
      if (item==atom_fail) {
	return FALSE;
      } else if (item==atom_exit) {
	TAGGED ent;
	DEREF(glob,RefTerm(GlobalR)); /* get global/5 */
	DerefArg(ent,glob,4);	/* get Ent */
	if (IsVar(ent)) {	/* SPRM 12958 */
	  BindHVA(ent,TaggedOne);
	  fd.entailments++;
	  DerefArg(statmut,glob,3); /* get status mutable */
	  status = (RefMutable(statmut)|IStep(16)); /* STATUS: entailed */
	  FD_UPDATE_MUTABLE(status,statmut);
	}
      }
    } else {
      TAGGED functor = TagToHeadfunctor(item);
      int why = 0;
      
      if (functor == functor_dom1)
	why = 1;
      else if (functor == fd.functor_min)
	why = 3;
      else if (functor == fd.functor_max)
	why = 5;
      else if (functor == fd.functor_minmax)
	why = 7;
      SP_ASSERT(why);
      DerefArg(dest_var,item,1);
      if (IsVar(dest_var)) {	/* otherwise, co-reference already dealt with */
	TAGGED domain;
	X(EVAL_ARITY) = domain = get_attributes(dest_var,fd.fd_module);
	X(EVAL_ARITY+1) = dest_var;
	DerefAttribute(domain, domain);
	fd_told(wam, why, DomainMin(domain));
      }
    }
  }
  DEREF(glob,RefTerm(GlobalR));	/* get global/5 */
  DerefArg(statmut,glob,3);	/* get status mutable */
  status = RefMutable(statmut);
  if (status&IStep(4))		/* STATUS: not idempotent */
    status &= ~IStep(8);	/* STATUS: not current */
  else
    status &= ~IStep(9);	/* STATUS: not current, not enqueued */
  FD_UPDATE_MUTABLE(status,statmut);
  return TRUE;
}

/*
% prune_and_enqueue(+Actions, +Global)

*/
void SPCDECL
prolog_fd_prune_and_enqueue(Wam wam,
			    SP_term_ref ActionsR,
			    SP_term_ref GlobalR)
{
  switch (fd_prune_and_enqueue(wam,ActionsR,GlobalR)) {
  case SP_FAILURE:
    fd.failures++;
    SP_fail();
    break;
  case SP_ERROR:
    SP_exception_term(ActionsR);
    SP_raise_exception(ActionsR);
  default:
    break;
  }
}



/* $fd_check_arguments(+Goal, -Attv) */
void SPCDECL
prolog_fd_check_arguments(Wam wam,
			  SP_term_ref GoalR,
			  SP_term_ref AttvR)
{
  int i, ar;
  TAGGED attr[ARITYLIMIT];
  TAGGED *h, v;

  X(0) = RefTerm(GoalR);
  DerefNonvar(X(0));
  if (!TagIsSTR(X(0))) {
    RefTerm(AttvR) = X(0);
  } else {
    attr[0] = TagToHeadfunctor(X(0));
    ar = Arity(attr[0]);
    RequireHeap(ar*FD_CHECK_ARGUMENT_NEED, 2); /* GC */
    for (i=1; i<=ar; i++)
      if (!(attr[i]=fd_check_argument(wam, CTagToArg(X(0),i),Inf,Sup,Sup))) {
	SP_fail();
	return;
      }
    h = w->global_top;
    v = MakeStructure(h);
    *h++ = attr[0];
    for (i=1; i<=ar; i++)
      *h++ = attr[i];
    w->global_top = h;
    RefTerm(AttvR) = v;
  }
}


static ix_byte *fd_store_token(Wam wam,
				      ix_byte *ptr,
				      TAGGED token,
				      TAGGED *literals)
{
  if (TagIsSIN(token)) {	/* opcode */
    token = DispatchLabel((TAGGED)GetSmall(token));
  } else {
    TAGGED f = TagToHeadfunctor(token);
    TAGGED arg;
    int i;

    DerefArg(arg,token,1);
    i = GetSmall_int(arg);
    if (f==fd.token_a)		/* argreg offset */
      token = (ix_byte)i;
    else if (f==fd.token_t)	/* tagged literal offset */
      token = (ix_byte)literals[i];
    else if (f==fd.token_d) {	/* ground domain */
      token = (ix_byte)literals[i];
      if (TagIsSTR(token))
	token = MakeList(TagToArg(token,1));
    } else if (f==fd.token_h)		/* hash table literal offset */
      token = (ix_byte)TermToPointer(LIST_INT(literals[i]));
    else if (f==fd.token_l)		/* code offset, RELATIVE */
      token = (ix_byte)i;
  }
  *ptr++ = token;
  return ptr;
}



static TAGGED *fd_store_literal(Wam wam,
				       TAGGED *literals,
				       TAGGED token)
{
  switch (TagOf(token)) {
  case CONST_TAG:			/* constant term or inf/sup */
    break;
  case STRUCT_TAG:
    {
      TAGGED f = TagToHeadfunctor(token);
      DerefArg(token, token, 1);
      if (f==fd.token_d) {	/* domain */
	int k = 1;
	TAGGED p = token;
	TAGGED *q, car, cdr;

	if (p==atom_nil)
	  break;
	k += fd_list_length(p)<<2;
	q = sp_checkalloc(k*sizeof(TAGGED), TRUE);
	q[0] = BIGNUM_HEADER - LStep(k-2);
	k = 1;
	p = token;
	while (TagIsLST(p)) {
	  DerefCar(car,p);
	  DerefCdr(cdr,car);
	  DerefCar(car,car);
	  DerefCdr(p,p);
	  q[k+0] = MakeList(q+k+2);
	  q[k+1] = MakeList(q+k+4);
	  q[k+2] = car;
	  q[k+3] = cdr;
	  k += 4;
	}
	q[k-3] = atom_nil;
	token = MakeStructure(q);
      } else {		/* hash table */
	struct sw_on_key *sw = new_switch_on_key(2,NULL);
	TAGGED item, key, value;
	    
	while (TagIsLST(token)) {
	  DerefCar(item, token);
	  DerefCdr(token, token);
	  DerefArg(key, item, 1);
	  DerefArg(value, item, 2);
	  dyn_puthash(&sw,key)->value.arities = value;
	}
	token = INT_LIST(PointerToTerm(sw)); /* mark for dealloc */
      }
    }
    break;
  }
  *literals++ = token;
  return literals;
}

static void free_fd_info(Wam wam,
				struct indexical_info **infop)
{
  struct indexical_info *ixinfo;

  /* [PM] 3.9b4 note that *infop may be NULL (except when called from free_fd_info_hook)*/

  while ((ixinfo = (*infop))) { 

#if DBG
    if (ixinfo->identifier != fd.generation) {
#if DBG>1
      fprintf(stderr, "DBG CLPFD: free_fd_info clpfd has been unloaded/reloaded since info was created (id==%d, clpfd_generation==%d) (this is expected)\n",
	      (int)ixinfo->identifier, (int)fd.generation); fflush(stderr);
#endif
    }
#endif/* DBG */

    if (ixinfo->linkage)
      sp_checkdealloc((TAGGED *)(ixinfo->linkage),
		      ixinfo->length_of_linkage*sizeof(ix_byte),
		      FALSE);
    if (ixinfo->code)
      sp_checkdealloc((TAGGED *)(ixinfo->code),
		      ixinfo->length_of_bytecode*sizeof(ix_byte),
		      FALSE);
    if (ixinfo->literals) {
      int i, len = ixinfo->length_of_literals;
      TAGGED *p = ixinfo->literals;
      struct sw_on_key *htab;
      TAGGED *q;

      for (i=0; i<len; i++) {
	switch (TagOf(p[i])) {
	case LIST_TAG:	/* hash table */
	  htab = (struct sw_on_key *)TermToPointer(LIST_INT(p[i]));
	  dispose_switch_on_key(htab);
	  break;
	case STRUCT_TAG:	/* FD set */
	  q = TagToSTR(p[i]);
	  sp_checkdealloc(q, LargeArity(q[0])*sizeof(TAGGED), TRUE);
	}
      }
      sp_checkdealloc(p, len*sizeof(TAGGED), FALSE);
    }
    (*infop) = ixinfo->next;
    sp_checkdealloc((TAGGED *)ixinfo, sizeof(struct indexical_info), FALSE);
  }
}

void SPCDECL fd_destructor_hook(Wam wam, struct indexical_info **infop)
{
  free_fd_info(wam, infop);
}

static void fd_install_indexicals(Wam wam,
					 TAGGED Info,
					 int type, int no_indexicals,
					 struct definition *f)
{
  TAGGED Indexical, tmp, tmp2, tmp3;
  int i,j,length;
  struct indexical_info *tmp_ix=NULL;
  struct indexical_info *Ix;
  ix_byte *code;
  TAGGED *literals;

  Ix = (struct indexical_info *)
    sp_checkalloc(sizeof(struct indexical_info), FALSE);

#if DBG                         /* [PM] 3.9.2b1 */
  Ix->identifier = fd.generation;
#endif


  f->proc.code.fdinfo->info[type] = Ix;

  for (i=0; i<no_indexicals; i++) { /* for each of the indexicals */
    if (i == 0) {
      tmp_ix = Ix;
    } else {
      tmp_ix = tmp_ix->next =
	(struct indexical_info *)
	sp_checkalloc(sizeof(struct indexical_info), FALSE);
#if DBG               /* [PM] 3.9.2b1 */
      tmp_ix->identifier = fd.generation;
#endif
    }
    tmp_ix->pred = f;
    tmp_ix->next = NULL;
    tmp_ix->checking = (unsigned)((type>>1)&0x1);
    tmp_ix->truth_value = (unsigned)((type)&0x1);
    DerefCar(Indexical, Info);
    DerefCdr(Info, Info);
    DerefArg(tmp, Indexical, 2); /* get length of fd linkage */
    tmp_ix->length_of_linkage = length = GetSmall_int(tmp);
    if (!length)		/* If the linkage list is empty */
      tmp_ix->linkage = NULL;
    else {
      tmp_ix->linkage =
	(ix_byte *)sp_checkalloc(length*sizeof(ix_byte), FALSE);
      DerefArg(tmp, Indexical, 1); /* tmp is now the linkage list */
      for (j = 0; j<length; j++) {
	DerefCar(tmp2, tmp);
	DerefArg(tmp3, tmp2, 1);
	DerefArg(tmp2, tmp2, 2);
	tmp_ix->linkage[j] = (GetSmall(tmp3)<<8) + GetSmall(tmp2);
	DerefCdr(tmp, tmp);
      }
    }
    DerefArg(tmp, Indexical, 3); /* get pruned */
    tmp_ix->pruned = (unsigned char)GetSmall_int(tmp);

    DerefArg(tmp, Indexical, 7); /* get length of literals */
    tmp_ix->length_of_literals = length = GetSmall_int(tmp);
    if (!length)
      tmp_ix->literals = NULL;
    else {
      tmp_ix->literals =
	literals =
	sp_checkalloc(length*sizeof(TAGGED), FALSE);
      DerefArg(tmp, Indexical, 6);
      while (TagIsLST(tmp)) {
	DerefCar(tmp2, tmp);
	DerefCdr(tmp, tmp);
	literals = fd_store_literal(wam, literals, tmp2);
      }
    }

    DerefArg(tmp, Indexical, 5); /* get length of bytecode */
    tmp_ix->length_of_bytecode = length = GetSmall_int(tmp);
    if (!length)
      tmp_ix->code = NULL;
    else {
      tmp_ix->code =
	code =
	(ix_byte *)sp_checkalloc(length*sizeof(ix_byte), FALSE);
      DerefArg(tmp, Indexical, 4);
      while (TagIsLST(tmp)) {
	DerefCar(tmp2, tmp);
	DerefCdr(tmp, tmp);
	code = fd_store_token(wam, code, tmp2, tmp_ix->literals);
      }
    }
    tmp_ix->next = NULL;
  }
}

/* Precondition: the predicate is currently undefined. */
static void init_fd_constraint(Wam wam,
				      struct definition *f)
{
  int i;
  
  /* [MC 4.0] TODO: memory leak here is currently not FD_CONSTRAINT */
  /* was: SP_install_c_predicate(s,f->proc.arity,f->module,0(*uninstall*),NULL,NULL); */
  f->proc.code.fdinfo =
    (struct fd_info *)sp_checkalloc(sizeof(struct fd_info), FALSE);
  for (i=0; i<4; i++)
    f->proc.code.fdinfo->info[i] = NULL;
  update_exports(f,FD_CONSTRAINT);
}


static void fd_install(Wam wam,
			      TAGGED head, TAGGED Module,
			      SP_integer Type, SP_integer InfoLength,
			      TAGGED Info)
{
  TAGGED *junk;
  struct mod_def *m = find_module(Module,TRUE);
  struct definition *f = find_definition(m,head,&junk,TRUE);
    
  if (f->proc.properties.prop.predtyp!=FD_CONSTRAINT)
    init_fd_constraint(wam, f);
  else
    free_fd_info(wam, (struct indexical_info **)&f->proc.code.fdinfo->info[Type]);
  fd_install_indexicals(wam, Info, (int)Type, (int)InfoLength, f);
}



/* $fd_install(wam, +Name/+Arity, +Module, +Type, +InfoLength, +Info) */
void SPCDECL
prolog_fd_install(Wam wam,
		  SP_term_ref PredSpecR,
		  SP_atom module,
		  SP_integer type,
		  SP_integer length,
		  SP_term_ref InfoR)
{
  TAGGED name, arity, spec, info, head;

  spec = RefTerm(PredSpecR);
  info = RefTerm(InfoR);
  DerefNonvar(spec);
  DerefNonvar(info);
  DerefArg(name, spec, 1);
  DerefArg(arity, spec, 2);
  if (Tnez(arity)) {
    int i = GetSmall_int(arity);
    TAGGED *h = w->global_top;
	
    *h++ = SetArity(name,i);
    for (; i>0; --i)
      Load0HVA(h);
    head = MakeStructure(w->global_top);
  } else {
    head = name;
  }
  /*
  printf("$FD_INSTALL: pred=%s:%s/%ld type=%ld info=",
	 SP_string_from_atom(module), SP_string_from_atom(name), arity, type);
  wr_tagged(info);
  printf("\n");
  */
  fd_install(wam, head, module, type, length, info);
}

/* link in the indexical X(0) upon initial suspension on all non-monotone variables */
/* called by $fd_evaluate_indexical only */
static void link_indexical(Wam wam, struct indexical_info *current)
{
  TAGGED t1;
  unsigned int tmp_link;
  int length, i;
  ix_byte mask;
  TAGGED ix, *args;

  mask = (current->checking ? 16 : 8);
  length = current->length_of_linkage;
  RequireHeap(length*FD_LINK_NEED,EVAL_ARITY); /* GC */
  ix = X(0);
  args = TagToArg(CTagToArg(ix,2),1);	/* refreshed after GC */
  for (i=0; i<length; i++) {
    tmp_link = (unsigned int)current->linkage[i];
    if (!(tmp_link & mask)) {
      t1 = args[tmp_link>>8];
      DerefSwitch(t1, fd_link(wam, t1, fd.linkage_keys[tmp_link & 7], ix, 0););
    }
  }
}

/* link in the indexical X(0) on one unbound monotone variable */
/* called by $fd_evaluate_indexical only */
static void suspend_indexical(Wam wam, struct indexical_info *current)
{
  TAGGED t1;
  unsigned int tmp_link;
  int length, i;
  ix_byte mask;
  TAGGED ix, *args;

  mask = (current->checking ? 16 : 8);
  length = current->length_of_linkage;
  RequireHeap(FD_LINK_NEED,EVAL_ARITY); /* GC */
  ix = X(0);
  args = TagToArg(CTagToArg(ix,2),1);	/* refreshed after GC */
  for (i=0; i<length; i++) {
    tmp_link = (unsigned int)current->linkage[i];
    if (tmp_link & mask) {
      t1 = args[tmp_link>>8];
      DerefSwitch(t1, {fd_link(wam, t1, fd.linkage_keys[tmp_link & 7], ix, 0); return;});
    }
  }
}

/* $fd_post_reified(+DefPtr, +Goal, +Attv, -ZeroOne, -ZeroOneAttr) */
/* Ensure: ZeroOne in 0..1 has been done. */
/* [MC] 4.0: DefPtr is passed as a PtrToTerm. */
void SPCDECL
prolog_fd_post_reified(Wam wam,
		       SP_term_ref DefPtr,
		       SP_term_ref Goal,
		       SP_term_ref Attv,
		       SP_term_ref ZeroOne,
		       SP_term_ref ZeroOneAttr)
{
  struct propagator *cur = fd.current_propagator;
  struct definition *def;
  struct indexical_info *ix, **p, **q;
  struct indexical_info *array[512];
  TAGGED *h, domain, dmin, dmax;
  int type, count, arity, i, j;
  int coref = 0;		/* [MC] 4.1.3 */
  int mintype = 0;
  int maxtype = 3;
  DECL_UPDATE_MUTABLE;

  DEREF(X(0),RefTerm(DefPtr));
  def = (struct definition *)TermToPointer(X(0));
  arity = def->proc.arity;
  DEREF(X(0),RefTerm(Goal));
  DEREF(X(1),RefTerm(Attv));
  DEREF(X(2),RefTerm(ZeroOne));
  h = w->global_top;
  LoadHVA(X(3),h);
  LoadHVA(X(4),h);
  LoadHVA(X(5),h);
  DEREF(X(6),RefTerm(ZeroOneAttr));
  DerefAttribute(domain,X(6));
  dmin = DomainMin(domain);
  dmax = DomainMax(domain);  
  w->global_top = h;
  fd.constraints++;
  fd_sync(wam);
  if (dmin==TaggedOne) {	/* post positive, [4.4] */
    mintype = maxtype = 1;
  } else if (dmax==TaggedZero) { /* post negative, [4.4] */
    mintype = maxtype = 0;
  }

  for (i=1; i<=arity && !coref; i++) {
    TAGGED argi;
    DerefArg(argi,X(0),i);
    if (IsVar(argi)) {
      for (j=i+1; j<=arity && !coref; j++) {
	TAGGED argj;
	DerefArg(argj,X(0),j);
	if (argi==argj)
	  coref = 4;
      }
    }
  }
  
  count = 0;
  for (type=mintype; type<=maxtype; type++)
    for (ix=(struct indexical_info *)def->proc.code.fdinfo->info[type]; ix; ix=ix->next)
      count++;

  count *= (17+FD_LINK_NEED_SAFE); /* ix(9) + $mutable(3) + iff(5) */
  RequireHeap(count,7);	

  for (type=mintype; type<=maxtype; type++) {
    p = q = array+256;
    for (ix=(struct indexical_info *)def->proc.code.fdinfo->info[type]; ix; ix=ix->next) {
      TAGGED t1;
      DerefArg(t1, X(0), ix->pruned+1);
      if (IsVar(t1))
	*(--p) = ix;
      else
	*q++ = ix;
    }
    for (ix = *(--q); p<=q; ix = *(--q)) {
      TAGGED item;

      h = w->global_top;
      h[0] = functor_Dmutable;
      h[1] = MakeSmall(coref+1); /* STATUS: init value [MC] 4.1.3 */
      h[2] = TaggedZero;
      h[3] = functor_ix8;
      h[4] = PointerToTerm(ix);	/* pointer */
      h[5] = X(0);		/* goal */
      h[6] = MakeStructure(h);	/* status mutable */
      h[7] = (type<2 ? X(5) : X(3)); /* entailment flag */
      h[8] = (type<2 ? X(4) : X(2)); /* zero-one var */
      h[9] = X(1);		/* Attv */
      h[10] = X(6);		/* zero-one attribute */
      h[11] = (fd.hiding ? atom_true : X(0)); /* source */
      item = MakeStructure(h+3);
      w->global_top = h+12;

      if (type<2 && mintype<maxtype) { /* suspend the indexical on an iff/4 */
	h = w->global_top;
	h[0] = functor_iff4;
	h[1] = item;
	h[2] = X(2);
	h[3] = MakeSmall(type);
	h[4] = X(3);
	w->global_top = h+5;
	fd_link(wam, X(2), fd.functor_val, MakeStructure(h), 0);
      } else {			/* post the indexical */
	fd_enqueue_append(wam,item,FD_QUEUE_IX);
      }
    }
  }
}

/* $fd_post_global(+Constraint, +State, +Status, +Source, +Susp, -Global) */
void SPCDECL
prolog_fd_post_global(Wam wam,
		      SP_term_ref Constraint,
		      SP_term_ref State,
		      SP_integer status,
		      SP_term_ref Source,
		      SP_term_ref Susp,
		      SP_term_ref Global)
{
  struct propagator *cur = fd.current_propagator;
  TAGGED *h;
  DECL_UPDATE_MUTABLE;

  fd.constraints++;
  DEREF(RefTerm(Constraint),RefTerm(Constraint));
  DEREF(RefTerm(Susp),RefTerm(Susp));
  h = w->global_top;
  h[0] = functor_Dmutable;
  h[1] = RefTerm(State);
  h[2] = TaggedZero;
  h[3] = functor_Dmutable;
  h[4] = MakeSmall(status|2);
  h[5] = TaggedZero;
  h[6] = functor_global5;
  h[7] = MakeStructure(h+0);
  h[8] = RefTerm(Constraint);
  h[9] = MakeStructure(h+3);
  h[10] = TagREF(h+10);
  h[11] = (fd.hiding ? atom_true : RefTerm(Source));
  RefTerm(Global) = MakeStructure(h+6);
  w->global_top = h+12;

  while (TagIsLST(RefTerm(Susp))) {
    TAGGED var, request, fun;
    RequireHeap(FD_LINK_NEED,0);
    DerefCar(request,RefTerm(Susp));
    DerefCdr(RefTerm(Susp),RefTerm(Susp));
    fun = TagToHeadfunctor(request);
    DerefArg(var,request,1);
    if (IsVar(var) && fun!=fd.functor_none)
      fd_link(wam, var, fun, RefTerm(Global), 0);
  }
  fd_enqueue_append(wam,RefTerm(Global), FD_QUEUE_VAL); /* for immediate dequeue */
}

static void post_implication(Wam wam, TAGGED PosLiterals, TAGGED NegLiterals, int npos, int nneg)
{
  TAGGED x, y, condvalue, actionvalue, *h = w->global_top;

  if (npos==0 && nneg==2) {
    DerefCar(x,NegLiterals);
    DerefCdr(NegLiterals,NegLiterals);
    DerefCar(y,NegLiterals);
    condvalue = TaggedOne;
    actionvalue = TaggedZero;
  } else if (npos==1 && nneg==1) {
    DerefCar(x,PosLiterals);
    DerefCar(y,NegLiterals);
    condvalue = TaggedZero;
    actionvalue = TaggedZero;
  } else {
    DerefCar(x,PosLiterals);
    DerefCdr(PosLiterals,PosLiterals);
    DerefCar(y,PosLiterals);
    condvalue = TaggedZero;
    actionvalue = TaggedOne;
  }

  h[0] = fd.functor_eq;
  h[1] = y;
  h[2] = actionvalue;
  h[3] = fd.functor_eq;
  h[4] = x;
  h[5] = (condvalue ^ IStep(1));
  w->global_top = h+6;
  fd_link(wam, CTagToArg(x,1), fd.functor_val, MakeStructure(h), condvalue);
  fd_link(wam, CTagToArg(y,1), fd.functor_val, MakeStructure(h+3), actionvalue ^ IStep(1));
}

static void post_clause(Wam wam, TAGGED PosLiterals, TAGGED NegLiterals, int npos, int nneg)
{
  TAGGED *h, clause, entvar, lits, status1=TaggedZero, status2=TaggedZero, pair1=atom_nil, pair2=atom_nil;
  int i, no;

  h = w->global_top;
				/* ensure lists are dereffed */
  if (npos>0) {
    for (i=0; i<npos; i++) {
      TAGGED item;
      DerefCar(item,PosLiterals);
      DerefCdr(PosLiterals,PosLiterals);
      h[0] = item;
      h[1] = MakeList(h+2);
      h += 2;
    }
    h[-1] = atom_nil;
    PosLiterals = MakeList(h-2*npos);
  }
  if (nneg>0) {
    for (i=0; i<nneg; i++) {
      TAGGED item;
      DerefCar(item,NegLiterals);
      DerefCdr(NegLiterals,NegLiterals);
      h[0] = item;
      h[1] = MakeList(h+2);
      h += 2;
    }
    h[-1] = atom_nil;
    NegLiterals = MakeList(h-2*nneg);
  }
  h[0] = functor_clause6;
  if (npos==0) {
    h[1] = MakeSmall(-nneg);
    h[2] = MakeSmall(-nneg+1);
  } else if (nneg==0) {
    h[1] = MakeSmall(npos-1);
    h[2] = MakeSmall(npos);
  } else {
    h[1] = MakeSmall(npos);
    h[2] = MakeSmall(-nneg);
  }
  h[3] = TaggedZero;
  h[4] = entvar = TagREF(h+4);
  h[5] = PosLiterals;
  h[6] = NegLiterals;
  clause = MakeStructure(h+0);
  w->global_top = h+7;

  if (npos+nneg==2) {
    status1 = status2 = TaggedOne;
    lits = PosLiterals;
    if (lits==EmptySet) {
      status1 = status2 = MakeSmall(2);
      lits = NegLiterals;
    }
    pair1 = CTagToCar(lits);
    lits = CTagToCdr(lits);
    if (lits==EmptySet) {
      status2 = MakeSmall(2);
      lits = NegLiterals;
    }
    pair2 = CTagToCar(lits);
  }
  no = 1;
  while (TagIsLST(PosLiterals)) {
    TAGGED pair = CTagToCar(PosLiterals), watcher;
    
    h = w->global_top;
    h[0] = functor_watcher5;
    h[1] = MakeSmall(no++);
    h[2] = (pair==pair1 ? pair2 : pair1);
    h[3] = (pair==pair1 ? status2 : status1);
    h[4] = entvar;
    h[5] = clause;
    watcher = MakeStructure(h+0);
    w->global_top = h+6;
    fd_link(wam, CTagToArg(pair,1), fd.functor_val, watcher, 0);
    PosLiterals = CTagToCdr(PosLiterals);
  }

  no = -1;
  while (TagIsLST(NegLiterals)) {
    TAGGED pair = CTagToCar(NegLiterals), watcher;
    
    h = w->global_top;
    h[0] = functor_watcher5;
    h[1] = MakeSmall(no--);
    h[2] = (pair==pair1 ? pair2 : pair1);
    h[3] = (pair==pair1 ? status2 : status1);
    h[4] = entvar;
    h[5] = clause;
    watcher = MakeStructure(h+0);
    w->global_top = h+6;
    fd_link(wam, CTagToArg(pair,1), fd.functor_val, watcher, 0);
    NegLiterals = CTagToCdr(NegLiterals);
  }
}


/* $fd_post_clause(+PosLiterals, +NegLiterals) */
void SPCDECL
prolog_fd_post_clause(Wam wam,
		      SP_term_ref PosLiterals,
		      SP_term_ref NegLiterals)
{
  SP_BOOL fdbg = (SP_BOOL)fd.debugging;
  int npos, nneg;

  fd.constraints++;
  DEREF(RefTerm(PosLiterals),RefTerm(PosLiterals));
  DEREF(RefTerm(NegLiterals),RefTerm(NegLiterals));
  npos = fd_list_length(RefTerm(PosLiterals));
  nneg = fd_list_length(RefTerm(NegLiterals));
  if (!fdbg && (npos+nneg==2)) {
    post_implication(wam, RefTerm(PosLiterals), RefTerm(NegLiterals), npos, nneg);
  } else {
    RequireHeap(7 + npos*(8+FD_LINK_NEED_SAFE) + nneg*(8+FD_LINK_NEED_SAFE), 0); /* GC */
    post_clause(wam, RefTerm(PosLiterals), RefTerm(NegLiterals), npos, nneg);
  }
}

/* $fd_post_disequation(+Coeffs, +Vars, +RHS) */
void SPCDECL
prolog_fd_post_disequation(Wam wam,
			   SP_term_ref Coeffs,
			   SP_term_ref Vars,
			   SP_integer RHS)
{
  TAGGED *h, disequation, vars;
  int i, nvars;

  fd.constraints++;
  DEREF(RefTerm(Coeffs),RefTerm(Coeffs));
  DEREF(RefTerm(Vars),RefTerm(Vars));
  nvars = fd_list_length(RefTerm(Coeffs));
  RequireHeap(8 + nvars*(4+FD_LINK_NEED_SAFE), 0); /* GC */
  h = w->global_top;
				/* ensure lists are dereffed */
  for (i=0; i<nvars; i++) {
    TAGGED item;
    DerefCar(item,RefTerm(Coeffs));
    DerefCdr(RefTerm(Coeffs),RefTerm(Coeffs));
    h[0] = item;
    h[1] = MakeList(h+2);
    h += 2;
  }
  h[-1] = atom_nil;
  RefTerm(Coeffs) = MakeList(h-2*nvars);
  for (i=0; i<nvars; i++) {
    TAGGED item;
    DerefCar(item,RefTerm(Vars));
    DerefCdr(RefTerm(Vars),RefTerm(Vars));
    h[0] = item;
    h[1] = MakeList(h+2);
    h += 2;
  }
  h[-1] = atom_nil;
  vars = MakeList(h-2*nvars);
  h[0] = functor_disequation4;
  h[1] = MakeStructure(h+5);
  h[2] = RefTerm(Coeffs);
  h[3] = vars;
  h[4] = MakeSmall(RHS);
  h[5] = functor_Dmutable;
  h[6] = MakeSmall(nvars);
  h[7] = MakeSmall(0);
  disequation = MakeStructure(h+0);
  w->global_top = h+8;

  while (TagIsLST(vars)) {
    fd_link(wam, CTagToArg(CTagToCar(vars),1), fd.functor_val, disequation, 0);
    vars = CTagToCdr(vars);
  }
}

/* $fd_find_definition(+Constraint, +Module, -DefPtr) */
/* [MC] 4.0: DefPtr is passed as a PtrToTerm. */
void SPCDECL
prolog_fd_find_definition(Wam wam,
			  SP_term_ref Constraint,
			  SP_atom module,
			  SP_term_ref DefPtrR)
{
  struct definition *f;
  TAGGED *junk;
  
/*    X(0) = RefTerm(Constraint); */
  (void)Constraint;
  DerefNonvar(X(0));
  f = find_definition(find_module(module,FALSE),X(0),&junk,FALSE);
  if (f && f->proc.properties.prop.predtyp!=FD_CONSTRAINT)
    f = NULL;

  RefTerm(DefPtrR) = PointerToTerm(f);
}


/* $fd_indexical_data(+Ptr, -Type, -Module)
   [MC] 4.0: Ptr is passed as a PtrToTerm.
*/
SP_atom SPCDECL
prolog_fd_indexical_data(Wam wam,
			 SP_term_ref PtrR,
			 SP_integer *type)
{
  struct indexical_info *current = (struct indexical_info *)TermToPointer(RefTerm(PtrR));
  
  *type = (current->checking<<1) + current->truth_value;
  return current->pred->module->name;
}


typedef void (SPCDECL *FdFilterFun)(Wam, SP_term_ref, SP_term_ref, SP_term_ref);

static int
dispatch_global_fast(Wam wam,
		     TAGGED constraint,	/* dereffed */
		     SP_term_ref State,	/* dereffed */
		     SP_term_ref NewState,
		     SP_term_ref Actions,
		     SP_term_ref Global) /* dereffed */
{
  TAGGED f = IsAtomic(constraint) ? constraint : TagToHeadfunctor(constraint);
  struct sw_on_key_node *hnode = incore_gethash(fd.dispatch,f);

  if (!hnode->value.arities)
    return 0;
  X(0) = RefTerm(State);
  X(1) = RefTerm(Global);
  (*(FdFilterFun)hnode->value.arities)(wam, State, NewState, Actions);
  return !fd.fd_overflow;
}

/* $fd_dispatch_global_fast(wam, +Constraint, +State, -NewState, -Actions, +Global)
   Effect is (value returned by fd_tell())/\7.
   Similar to prune_set().
*/
SP_integer SPCDECL
prolog_fd_dispatch_global_fast(Wam wam,
			       SP_term_ref Constraint,
			       SP_term_ref State,
			       SP_term_ref NewState,
			       SP_term_ref Actions,
			       SP_term_ref Global)
{
  TAGGED constraint = RefTerm(Constraint);
  DEREF(constraint,constraint);
  DEREF(RefTerm(State),RefTerm(State));
  DEREF(RefTerm(Global),RefTerm(Global));

  return dispatch_global_fast(wam, constraint, State, NewState, Actions, Global);
}

static SP_BOOL
eval_indexical(Wam wam, SP_term_ref Global)
{
  int ground=1;
  SP_BOOL fdbg = (SP_BOOL)fd.debugging;
  TAGGED gtemp, *atts, *args, *ix_args;
  TAGGED truth_value=TaggedOne, min2=0, max2=0;
  ix_byte *code;
  TAGGED *top=&X(EVAL_ARITY);
  TAGGED qval[128];
  struct dvar dvar;
  struct indexical_info *current;
  DECL_UPDATE_MUTABLE;
  DispatchDef;

 rerun_ix:
  ix_args = TagToArg(X(0),1);    
  current = (struct indexical_info *)TermToPointer(ix_args[0]);
  args = TagToArg(ix_args[1],1); /* Goal */
  X(1) = ix_args[2];		 /* StatusM */
  atts = TagToArg(ix_args[5],1); /* Attv */
  dvar_init_ix(&dvar,atts[current->pruned],args[current->pruned]);
  gtemp = RefMutable(X(1));
  if ((gtemp&IStep(5))==IStep(5)) { /* STATUS: enqueued not idempotent [MC] 4.1.3 */
    gtemp &= ~IStep(1);		    /* STATUS: not enqueued */
    if (!(gtemp&IStep(2))) {	    /* STATUS: linked */
      gtemp |= IStep(2);	    /* STATUS: linked */
      link_indexical(wam, current); /* GC */
    }
    FD_UPDATE_MUTABLE(gtemp,X(1));
    goto rerun_ix;		     /* in case of GC */
  }
  w->numstack_end = NULL;
  code = current->code;
  DispatchFirst {
    DispatchHead {
    CaseX(FD_DUP_RANGE):	/* int x int -> set */
      {
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_interval(wam, t1,t1);
	Dispatch;
      }

    CaseX(FD_RANGE_OO):		/* -> set */
      {
	Prefetch;
	
	Push(fd_interval(wam, Inf, Sup));
	Dispatch;
      }

    CaseX(FD_RANGE_OC):		/* int -> set */
      {
	TAGGED t2 = Top;
	Prefetch;
	
	Top = (t2==Inf ? EmptySet : fd_interval(wam, Inf, t2));
	Dispatch;
      }

    CaseX(FD_RANGE_CO):		/* int -> set */
      {
	TAGGED t1 = Top;
	Prefetch;
	
	Top = (t1==Sup ? EmptySet : fd_interval(wam, t1, Sup));
	Dispatch;
      }

    CaseX(FD_RANGE_CC):		/* int x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (t1==t2 && !TagIsSmall(t1)) {
	  fd.fd_overflow = (t1==Sup ? 2 : 1);
	  goto ov_loop;
	}
	Top = (FDgt(t1,t2) ? EmptySet : fd_interval(wam, t1, t2));
	Dispatch;
      }

    CaseX(FD_SETADD):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_lsh(wam, t1,t2);
	Dispatch;
      }

    CaseX(FD_SETSUB):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_lsh(wam, t1,Tminus(t2));
	Dispatch;
      }

    CaseX(FD_SETNEG):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_neg_offset(wam, t1,t2);
	Dispatch;
      }

    CaseX(FD_SETMOD):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if ((Top = fd_setmod(wam, t1,t2))==ERRORTAG)
	  goto abort;
	Dispatch;
      }

    CaseX(FD_SETREM):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if ((Top = fd_setrem(wam, t1,t2))==ERRORTAG)
	  goto abort;
	Dispatch;
      }

    CaseX(FD_SETPLUS):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_plus(wam, t1,t2,Inf,Sup);
	Dispatch;
      }

    CaseX(FD_SETMINUS):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_minus(wam, t1,t2,Inf,Sup);
	Dispatch;
      }

    CaseX(FD_COMPL_T):		/* int -> set */
      {
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_compl_interval(wam, t1,t1);
	Dispatch;
      }

    CaseX(FD_COMPL_D):		/* set -> set */
      {
	Prefetch;
	
	Top = fd_complement(wam, Top);
	Dispatch;
      }

    CaseX(FD_UNION_TT):		/* int x int -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	Top = fd_pair(wam, t1,t2);
	Dispatch;
      }

    CaseX(FD_UNION_TD):		/* int x set -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	Top = fd_insert_into(wam, t2,t1);
	Dispatch;
      }
      
    CaseX(FD_UNION_DT):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_insert_into(wam, t2,t1);
	Dispatch;
      }

    CaseX(FD_UNION_DD):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_union(wam, t1,t2);
	Dispatch;
      }

    CaseX(FD_INTER_TT):		/* int x int -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	if (t1!=t2)
	  Top = EmptySet;
	else
	  Top = fd_interval(wam, t1,t1);
	Dispatch;
      }

    CaseX(FD_INTER_TD):		/* int x set -> set */
      {
	TAGGED t1 = Pop;
	TAGGED t2 = Top;
	Prefetch;
	
	if (fd_member(t2,t1))
	  Top = fd_interval(wam, t2,t2);
	else
	  Top = EmptySet;
	Dispatch;
      }
      
    CaseX(FD_INTER_DT):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (fd_member(t2,t1))
	  Top = fd_interval(wam, t2,t2);
	else
	  Top = EmptySet;
	Dispatch;
      }

    CaseX(FD_INTER_DD):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_intersection(wam, t1,t2);
	Dispatch;
      }

    CaseX(FD_SUBTRACT_TT):		/* int x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (t1==t2)
	  Top = EmptySet;
	else
	  Top = fd_interval(wam, t1,t1);
	Dispatch;
      }

    CaseX(FD_SUBTRACT_TD):		/* int x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (fd_member(t1,t2))
	  Top = EmptySet;
	else
	  Top = fd_interval(wam, t1,t1);
	Dispatch;
      }
      
    CaseX(FD_SUBTRACT_DT):		/* set x int -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_delete(wam, t1,t2);
	Dispatch;
      }

    CaseX(FD_SUBTRACT_DD):		/* set x set -> set */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	Top = fd_subtract(wam, t1,t2);
	Dispatch;
      }

    CaseX(FD_CHECK_UNION):	/* set -> set (sort of) */
      {				/* (D1 ? (inf..sup) \/ D2) */
	TAGGED t1 = Pop;
	ix_byte *altcode = current->code + (*code++);
	
	if (t1!=EmptySet) {
	  Push(fd_interval(wam, Inf, Sup));
	  code = altcode;
	}
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_CHECK):		/* set -> set (sort of) */
      {
	TAGGED t1 = Pop;
	ix_byte *altcode = current->code + (*code++);
	
	if (t1==EmptySet) {
	  Push(t1);
	  code = altcode;
	}
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_ADD):		/* int x int -> int */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (TagIsSmall(t1)) {
	  if (TagIsSmall(t2)) {
	    TADDCHK(t1,t2);
	  } else
	    t1 = t2;
	} else if (t1!=t2 && !TagIsSmall(t2))
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_SUB):		/* int x int -> int */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (TagIsSmall(t1)) {
	  if (TagIsSmall(t2)) {
	    TSUBCHK(t1,t2);
	  } else
	    t1 = (t2==Inf ? Sup : Inf);
	} else if (t1==t2)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MULT_IMM):		/* int -> int */
      {
	TAGGED t2 = *code++;
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = fd_safe_mul(t1,t2);
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVD_IMM):		/* int -> int */
      {				/* floor of quotient */
	TAGGED t2 = *code++;
	TAGGED t1 = Top;
	Prefetch;

	t1 = fd_safe_divd(t1,t2);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVU_IMM):		/* int -> int */
      {				/* ceiling of quotient */
	TAGGED t2 = *code++;
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = fd_safe_divu(t1,t2);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MULT_VAL):		/* int -> int */
      {
	int i = (int)(*code++);
	TAGGED t2;
	TAGGED t1 = Top;
	Prefetch;
	
	t2 = args[i];
	DerefSwitch(t2, {goto suspend;});
	t1 = fd_safe_mul(t1,t2);
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVD_VAL):		/* int -> int */
      {				/* floor of quotient */
	int i = (int)(*code++);
	TAGGED t2;
	TAGGED t1 = Top;
	Prefetch;

	t2 = args[i];
	DerefSwitch(t2, {goto suspend;});
	t1 = fd_safe_divd(t1,t2);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVU_VAL):		/* int -> int */
      {				/* ceiling of quotient */
	int i = (int)(*code++);
	TAGGED t2;
	TAGGED t1 = Top;
	Prefetch;
	
	t2 = args[i];
	DerefSwitch(t2, {goto suspend;});
	t1 = fd_safe_divu(t1,t2);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MULT_QVAL):		/* int -> int */
      {
	int i = (int)(*code++);
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = fd_safe_mul(t1,qval[i]);
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVD_QVAL):		/* int -> int */
      {				/* floor of quotient */
	int i = (int)(*code++);
	TAGGED t1 = Top;
	Prefetch;

	t1 = fd_safe_divd(t1,qval[i]);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_DIVU_QVAL):		/* int -> int */
      {				/* ceiling of quotient */
	int i = (int)(*code++);
	TAGGED t1 = Top;
	Prefetch;
	
	t1 = fd_safe_divu(t1,qval[i]);
	if (t1==ERRORTAG)
	  goto abort;
	Top = t1;
	Dispatch;
      }

    CaseX(FD_MOD):		/* int x int -> int */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (Teqz(t2) || !TagIsSmall(t1)) {
	  goto abort;	/* inf mod any OR any mod 0 -> give up */
	} else if (TagIsSmall(t2)) {
	  SP_integer i1 = GetSmall(t1);
	  SP_integer i2 = GetSmall(t2);
	  SP_integer rem = i1 % i2;

	  if (rem!=0 && (rem<0) != (i2<0))
	    rem += i2;

	  Top = MakeSmall(rem);
	}
	Dispatch;
      }

    CaseX(FD_REM):		/* int x int -> int */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Top;
	Prefetch;
	
	if (Teqz(t2) || !TagIsSmall(t1)) {
	  goto abort;	/* inf mod any OR any mod 0 -> give up */
	} else if (TagIsSmall(t2)) {
	  SP_integer i1 = GetSmall(t1);
	  SP_integer i2 = GetSmall(t2);
	  SP_integer rem = i1 % i2;

	  Top = MakeSmall(rem);
	}
	Dispatch;
      }

    CaseX(FD_QVAL):		/* -> int */
      {
	TAGGED t1 = *code++;
	{
	  Prefetch;
	  Push(qval[t1]);
	  Dispatch;
	}
      }

    CaseX(FD_VAL):		/* -> int */
      {
	gtemp = atts[*code++];
      fd_val:
	{
	  TAGGED *pt1;
	  Prefetch;

	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3])	/* [MC] 4.3.4 */
	    goto suspend;
	  Push(pt1[2]);
	  Dispatch;
	}
      }
    
    CaseX(FD_VAL_0):		/* -> int */
      gtemp = atts[0];
      goto fd_val;
    
    CaseX(FD_VAL_1):		/* -> int */
      gtemp = atts[1];
      goto fd_val;
    
    CaseX(FD_VAL_2):		/* -> int */
      gtemp = atts[2];
      goto fd_val;

    CaseX(FD_DOM):		/* -> set */
      {
	gtemp = atts[*code++];
      fd_dom:
	{
	  TAGGED *pt1;
	  Prefetch;

	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3])
	    ground = 0;
	  Push(pt1[1]);
	  Dispatch;
	}
      }

    CaseX(FD_DOM_0):		/* -> set */
      gtemp = atts[0];
      goto fd_dom;

    CaseX(FD_DOM_1):		/* -> set */
      gtemp = atts[1];
      goto fd_dom;

    CaseX(FD_DOM_2):		/* -> set */
      gtemp = atts[2];
      goto fd_dom;

    CaseX(FD_SET_1):		/* -> set */
	{
	  Prefetch;

	  Push(atts[1]);
	  Dispatch;
	}


    CaseX(FD_MIN):		/* -> int */
      {
	gtemp = atts[*code++];
      fd_min:
	{
	  TAGGED *pt1;
	  Prefetch;
	  
	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3])
	    ground = 0;
	  Push(pt1[2]);
	  Dispatch;
	}
      }

    CaseX(FD_MIN_0):		/* -> int */
      gtemp = atts[0];
      goto fd_min;

    CaseX(FD_MIN_1):		/* -> int */
      gtemp = atts[1];
      goto fd_min;

    CaseX(FD_MIN_2):		/* -> int */
      gtemp = atts[2];
      goto fd_min;

    CaseX(FD_MAX):		/* -> int */
      {
	gtemp = atts[*code++];
      fd_max:
	{
	  TAGGED *pt1;
	  Prefetch;

	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3])
	    ground = 0;
	  Push(pt1[3]);
	  Dispatch;
	}
      }

    CaseX(FD_MAX_0):		/* -> int */
      gtemp = atts[0];
      goto fd_max;

    CaseX(FD_MAX_1):		/* -> int */
      gtemp = atts[1];
      goto fd_max;

    CaseX(FD_MAX_2):		/* -> int */
      gtemp = atts[2];
      goto fd_max;

    CaseX(FD_CARD):		/* -> int */
      {
	gtemp = atts[*code++];
      fd_card:
	{
	  TAGGED *pt1;
	  Prefetch;
	  
	  DerefAttribute(gtemp,gtemp);
	  pt1 = TagToSTR(gtemp);
	  if (pt1[2]!=pt1[3])
	    ground = 0;
	  Push(pt1[4]);
	  Dispatch;
	}
      }

    CaseX(FD_CARD_0):		/* -> int */
      gtemp = atts[0];
      goto fd_card;

    CaseX(FD_CARD_1):		/* -> int */
      gtemp = atts[1];
      goto fd_card;

    CaseX(FD_CARD_2):		/* -> int */
      gtemp = atts[2];
      goto fd_card;

    CaseX(FD_CONST):		/* -> int or set */
      {
	Push(*code++);
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_UNIONOF):		/* set -> set x set */
      {				/* FD_UNIONOF LabelB */
	TAGGED t1 = Top;
	TAGGED t2;
	
	Top = EmptySet;
	code = current->code + (*code);
	if (t1!=EmptySet) {
	  if (fd_infinite(t1))
	    goto abort;
	  t2 = code[-2];	  
	  code = current->code + code[-1];
	  Push(t1);
	  fd_first_and_rest(wam, t1,qval+t2,&top[-1]);
	}
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_UNIONOF_NEXT):	/* set x set x set -> set x set */
      {				/* FD_UNIONOF_NEXT Qvar LabelB */
	TAGGED t2 = Pop;
	TAGGED t1 = Pop;	/* remaining bits */
	ix_byte *altcode;
      
	Top = fd_union(wam, Top,t2);	/* update union so far */
	t2 = *code++;
	altcode = current->code + (*code++);
	if (t1!=EmptySet) {
	  Push(t1);
	  fd_first_and_rest(wam, t1,qval+t2,&top[-1]);
	  code = altcode;
	}
	{
	  Prefetch;
	  Dispatch;
	}
      }
	  
	  
    CaseX(FD_SWITCH):		/* -> set or label */
      {
	struct sw_on_key *sw =	/* get hash table */
	  (struct sw_on_key *)(*code++);
	ix_byte *altcode = current->code + (*code++); /* get join label */
	TAGGED t2 = incore_gethash(sw,Top)->value.arities;
	if (!t2) {
	  Top = EmptySet;	/* default value is empty set */
	  code = altcode;	/* branch to join */
	} else {
	    Top = (TAGGED)altcode; /* push join label */
	    code = current->code+GetSmall(t2); /* and branch to case */
	}
	{
	  Prefetch;
	  Dispatch;
	}
      }

    CaseX(FD_POPJ):		/* label x set -> set */
      {
	TAGGED t2 = Pop;
	code = (ix_byte *)Top;
	Top = t2;
	{
	  Prefetch;
	  Dispatch;
	}
      }
    
    CaseX(FD_ERROR):
      return FALSE;
    
    CaseX(FD_PRUNE_RANGE_OO):	/* inf..sup (weird) */
      goto ent_link_ov_loop;
      
    CaseX(FD_PRUNE_RANGE_OC):	/* int -> */
				/* inf..Max */
      max2 = Pop;
      min2 = Inf;
      goto prune_range;

    CaseX(FD_PRUNE_RANGE_CO):	/* int -> */
				/* Min..sup */
      max2 = Sup;
      min2 = Pop;
      goto prune_range;

    CaseX(FD_PRUNE_RANGE_CC):	/* int x int -> */
				/* Min..Max */
      max2 = Pop;		
      min2 = Pop; 
    prune_range:
      if (min2==max2 && !TagIsSmall(min2)) {
	fd.fd_overflow = (min2==Sup ? 2 : 1);
	goto ov_loop;
      }
      switch (dvar_fix_interval_t(&dvar,min2,max2)) {
      case -1:
	goto fail;
      case 0:
	goto ent_link_ov_loop;
      }
      goto export_ent_link_ov_loop;
	  
    CaseX(FD_PRUNE_TERM_COMPL):	/* int -> */
				/* unify LHS with complement */
      gtemp = Pop;
    prune_term_compl:
      switch (dvar_prune_interval_t(&dvar,gtemp,gtemp)) {
      case -1:
	goto fail;
      case 0:
	goto ent_link_ov_loop;
      }
      goto export_ent_link_ov_loop;

	  
    CaseX(FD_PRUNE_TERM):	/* int -> */
				/* unify LHS with value */
      gtemp = Pop;
      switch (dvar_fix_interval_t(&dvar,gtemp,gtemp)) {
      case -1:
	goto fail;
      case 0:
	goto ent_link_ov_loop;
      }
      goto export_ent_link_ov_loop;


    CaseX(FD_PRUNE_COMPL):	/* set -> */
      gtemp = Pop;
      switch (dvar_prune_set(&dvar,gtemp)) {
      case -1:
	goto fail;
      case 0:
	goto ent_link_ov_loop;
      }
      goto export_ent_link_ov_loop;

    CaseX(FD_PRUNE):		/* set -> */
      {
	gtemp = Pop;
      prune:
	if (gtemp==EmptySet) {
	  goto fail;
	} else if (CTagToCdr(gtemp)==EmptySet) {
	  gtemp = CTagToCar(gtemp);
	  min2 = RangeMin(gtemp);
	  max2 = RangeMax(gtemp);
	  goto prune_range;
	}
	switch (dvar_fix_set(&dvar,
			     fd_localize(wam, gtemp))) { /* [MC] SPRM 10707 avoid structure sharing */
	case -1:
	  goto fail;
	case 0:
	  goto ent_link_ov_loop;
	}
      export_ent_link_ov_loop:
	dvar_export_ix(wam, &dvar, fdbg); /* GC */
	goto ent_link_ov_loop;
      }

    CaseX(FD_PRUNE_PLUS):	/* set x set -> */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Pop;
	gtemp = fd_plus(wam, t1,t2,dvar.min,dvar.max);
	goto prune;
      }

    CaseX(FD_PRUNE_MINUS):	/* set x set -> */
      {
	TAGGED t2 = Pop;
	TAGGED t1 = Pop;
	gtemp = fd_minus(wam, t1,t2,dvar.min,dvar.max);
	goto prune;
      }

    CaseX(FD_PRUNE_BOOL):	/* [MC] 4.2.1-4.3.3 */
    CaseX(FD_AX_EQ_T_3_2):	/* [MC] 4.3 */
      {
	TAGGED *pt1, t1, t2;

	// FD_MIN_2 
	t1 = atts[2];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[2];
	// FD_DIVU_VAL 0 
      	t2 = args[0];
	DerefSwitch(t2, {goto suspend;});
	min2 = fd_safe_divu(t1,t2);
	// FD_MAX_2 
	t1 = pt1[3];
	// FD_DIVD_VAL 0 
	max2 = fd_safe_divd(t1,t2);
	if (min2==ERRORTAG || max2==ERRORTAG)
	  goto abort;
	// FD_PRUNE_RANGE_CC 
	goto prune_range;
      }

    CaseX(FD_AX_EQ_T_3_3):	/* [MC] 4.3 */
      {
	TAGGED *pt1, t1, t2;

	// FD_MIN_1 
	t1 = atts[1];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[2];
	// FD_MULT_VAL 0 
      	t2 = args[0];
	DerefSwitch(t2, {goto suspend;});
	min2 = fd_safe_mul(t1,t2);
	// FD_MAX_1 
	t1 = pt1[3];
	// FD_MULT_VAL 0 
	max2 = fd_safe_mul(t1,t2);
	// FD_PRUNE_RANGE_CC 
	goto prune_range;
      }

    CaseX(FD_X_PLUS_Y_EQ_T_3_123): /* [MC] 4.3 */
      {
	TAGGED *pt1, *pt2, t1, t2;
	int add3;

	// FD_MIN_$1
	t2 = atts[*code++];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	if (pt2[2]!=pt2[3])
	  ground = 0;
	t2 = pt2[2];
	// FD_MIN/FD_MAX_$2
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	add3 = (int)*code++;
	// FD_ADD/FD_SUB $3
	if (add3) {
	  t1 = pt1[2];
	  if (TagIsSmall(t2)) {
	    if (TagIsSmall(t1)) {
	      TADDCHK(t2,t1);
	    } else
	      t2 = t1;
	  } else if (t2!=t1 && !TagIsSmall(t1))
	    goto abort;
	} else {
	  t1 = pt1[3];
	  if (TagIsSmall(t2)) {
	    if (TagIsSmall(t1)) {
	      TSUBCHK(t2,t1);
	    } else
	      t2 = (t1==Inf ? Sup : Inf);
	  } else if (t2==t1)
	    goto abort;
	}
	min2 = t2;
	// FD_MAX_$1
	t2 = pt2[3];
	// FD_MAX/FD_MIN_$2
	// FD_ADD/FD_SUB $3
	if (add3) {
	  t1 = pt1[3];
	  if (TagIsSmall(t2)) {
	    if (TagIsSmall(t1)) {
	      TADDCHK(t2,t1);
	    } else
	      t2 = t1;
	  } else if (t2!=t1 && !TagIsSmall(t1))
	    goto abort;
	} else {
	  t1 = pt1[2];
	  if (TagIsSmall(t2)) {
	    if (TagIsSmall(t1)) {
	      TSUBCHK(t2,t1);
	    } else
	      t2 = (t1==Inf ? Sup : Inf);
	  } else if (t2==t1)
	    goto abort;
	}
	max2 = t2;
	// FD_PRUNE_RANGE_CC 
	goto prune_range;
      }

    CaseX(FD_T_PLUS_U_EQ_C_3_12):	/* [MC] 4.3 */
      {
	TAGGED *pt1, *pt2, t1, t2;

	// FD_DOM_$1
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[1];
	// FD_VAL_2 
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	t2 = pt2[2];
	if (t2!=pt2[3])	/* [MC] 4.3.4 */
	  goto suspend;
	// FD_SETNEG 
	gtemp = fd_neg_offset(wam, t1,t2);
	// FD_PRUNE 
	goto prune;
      }

    CaseX(FD_T_EQ_U_PLUS_C_3_12):	/* [MC] 4.3 */
      {
	TAGGED *pt1, *pt2, t1, t2;

	// FD_DOM_$1 
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[1];
	// FD_VAL_2 
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	t2 = pt2[2];
	if (t2!=pt2[3])	/* [MC] 4.3.4 */
	  goto suspend;
	// FD_SETADD/FD_SETSUB $2
	gtemp = fd_lsh(wam, t1,(*code++ ? t2 : Tminus(t2)));
	// FD_PRUNE 
	goto prune;
      }

    CaseX(FD_T_LE_U_PLUS_C_3_12):	/* [MC] 4.3 */
      {
	TAGGED *pt1, *pt2, t1, t2;

	// FD_MIN/FD_MAX_$1
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	// FD_VAL_2 
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	t2 = pt2[2];
	if (t2!=pt2[3])	/* [MC] 4.3.4 */
	  goto suspend;
	// FD_ADD/FD_SUB $2
	if (*code++) {
	  t1 = pt1[3];
	  if (TagIsSmall(t1)) {
	    TADDCHK(t1,t2);
	  }
	  max2 = t1;
	  min2 = Inf;
	} else {
	  t1 = pt1[2];
	  if (TagIsSmall(t1)) {
	    TSUBCHK(t1,t2);
	  }
	  min2 = t1;
	  max2 = Sup;
	}
	// FD_PRUNE_RANGE_OC 
	goto prune_range;
      }

    CaseX(FD_T_NE_U_PLUS_C_3_12):	/* [MC] 4.3 */
      {
	TAGGED *pt1, *pt2, t1, t2;

	// FD_VAL_$1
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	t1 = pt1[2];
	if (t1!=pt1[3])	/* [MC] 4.3.4 */
	  goto suspend;
	// FD_VAL_2 
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	t2 = pt2[2];
	if (t2!=pt2[3])	/* [MC] 4.3.4 */
	  goto suspend;
	// FD_ADD/FD_SUB $2
	if (*code++) {
	  TADDCHK(t1,t2);
	} else {
	  TSUBCHK(t1,t2);
	}
	gtemp = t1;
	// FD_PRUNE_TERM_COMPL 
	goto prune_term_compl;
      }

    CaseX(FD_T_GE_U_PLUS_C_3_12):	/* [MC] 4.3 */
      {
	TAGGED *pt1, *pt2, t1, t2;

	// FD_MAX/FD_MIN_$1
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	// FD_VAL_2 
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	t2 = pt2[2];
	if (t2!=pt2[3])	/* [MC] 4.3.4 */
	  goto suspend;
	// FD_ADD/FD_SUB $2
	if (*code++) {
	  t1 = pt1[2];
	  if (TagIsSmall(t1)) {
	    TADDCHK(t1,t2);
	  }
	  min2 = t1;
	  max2 = Sup;
	} else {
	  t1 = pt1[3];
	  if (TagIsSmall(t1)) {
	    TSUBCHK(t1,t2);
	  }
	  max2 = t1;
	  min2 = Inf;
	}
	// FD_PRUNE_RANGE_CO 
	goto prune_range;
      }

    CaseX(FD_AX_PLUS_Y_EQ_T_4_2):	/* [MC] 4.3 */
      {
	TAGGED *pt2, *pt3, t0, t2, t3;

	// FD_MIN_3 
	t3 = atts[3];
	DerefAttribute(t3,t3);
	pt3 = TagToSTR(t3);
	if (pt3[2]!=pt3[3])
	  ground = 0;
	t3 = pt3[2];
	// FD_MAX_2 
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	if (pt2[2]!=pt2[3])
	  ground = 0;
	t2 = pt2[3];
	// FD_SUB 
	if (TagIsSmall(t3)) {
	  if (TagIsSmall(t2)) {
	    TSUBCHK(t3,t2);
	  } else
	    t3 = (t2==Inf ? Sup : Inf);
	} else if (t3==t2)
	  goto abort;
	// FD_DIVU_VAL 0 
      	t0 = args[0];
	DerefSwitch(t0, {goto suspend;});
	min2 = fd_safe_divu(t3,t0);
	// FD_MAX_3 
	t3 = pt3[3];
	// FD_MIN_2 
	t2 = pt2[2];
	// FD_SUB 
	if (TagIsSmall(t3)) {
	  if (TagIsSmall(t2)) {
	    TSUBCHK(t3,t2);
	  } else
	    t3 = (t2==Inf ? Sup : Inf);
	} else if (t3==t2)
	  goto abort;
	// FD_DIVD_VAL 0 
	max2 = fd_safe_divd(t3,t0);
	if (min2==ERRORTAG || max2==ERRORTAG)
	  goto abort;
	// FD_PRUNE_RANGE_CC 
	goto prune_range;
      }

    CaseX(FD_AX_PLUS_Y_EQ_T_4_3):	/* [MC] 4.3 */
      {
	TAGGED *pt1, *pt3, t0, t1, t3;

	// FD_MIN_3 
	t3 = atts[3];
	DerefAttribute(t3,t3);
	pt3 = TagToSTR(t3);
	if (pt3[2]!=pt3[3])
	  ground = 0;
	t3 = pt3[2];
	// FD_MAX_1
	t1 = atts[1];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[3];
	// FD_MULT_VAL 0 
      	t0 = args[0];
	DerefSwitch(t0, {goto suspend;});
	t1 = fd_safe_mul(t1,t0);
	// FD_SUB 
	if (TagIsSmall(t3)) {
	  if (TagIsSmall(t1)) {
	    TSUBCHK(t3,t1);
	  } else
	    t3 = (t1==Inf ? Sup : Inf);
	} else if (t3==t1)
	  goto abort;
	min2 = t3;
	// FD_MAX_3 
	t3 = pt3[3];
	// FD_MIN_1 
	t1 = pt1[2];
	// FD_MULT_VAL 0 
	t1 = fd_safe_mul(t1,t0);
	// FD_SUB 
	if (TagIsSmall(t3)) {
	  if (TagIsSmall(t1)) {
	    TSUBCHK(t3,t1);
	  } else
	    t3 = (t1==Inf ? Sup : Inf);
	} else if (t3==t1)
	  goto abort;
	max2 = t3;
	// FD_PRUNE_RANGE_CC 
	goto prune_range;
      }

    CaseX(FD_AX_PLUS_Y_EQ_T_4_4):	/* [MC] 4.3 */
      {
	TAGGED *pt2, *pt1, t0, t2, t1;

	// FD_MIN_1 
	t1 = atts[1];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[2];
	// FD_MULT_VAL 0 
      	t0 = args[0];
	DerefSwitch(t0, {goto suspend;});
	t1 = fd_safe_mul(t1,t0);
	// FD_MIN_2 
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	if (pt2[2]!=pt2[3])
	  ground = 0;
	t2 = pt2[2];
	// FD_ADD 
	if (TagIsSmall(t1)) {
	  if (TagIsSmall(t2)) {
	    TADDCHK(t1,t2);
	  } else
	    t1 = t2;
	} else if (t1!=t2 && !TagIsSmall(t2))
	  goto abort;
	min2 = t1;
	// FD_MAX_1 
	t1 = pt1[3];
	// FD_MULT_VAL 0 
	t1 = fd_safe_mul(t1,t0);
	// FD_MAX_2 
	t2 = pt2[3];
	// FD_ADD 
	if (TagIsSmall(t1)) {
	  if (TagIsSmall(t2)) {
	    TADDCHK(t1,t2);
	  } else
	    t1 = t2;
	} else if (t1!=t2 && !TagIsSmall(t2))
	  goto abort;
	max2 = t1;
	// FD_PRUNE_RANGE_CC 
	goto prune_range;
      }

    CaseX(FD_T_PLUS_U_LE_C_3_12):	/* [MC] 4.3 */
      {
	TAGGED *pt1, *pt2, t1, t2;

	// FD_VAL_2 
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	t2 = pt2[2];
	if (t2!=pt2[3])	/* [MC] 4.3.4 */
	  goto suspend;
	// FD_MIN_$1
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[2];
	// FD_SUB 
	if (TagIsSmall(t1)) {
	  TSUBCHK(t2,t1);
	} else
	  t2 = (t1==Inf ? Sup : Inf);
	max2 = t2;
	// FD_PRUNE_RANGE_OC 
	min2 = Inf;
	goto prune_range;
      }

    CaseX(FD_T_PLUS_U_NE_C_3_12):	/* [MC] 4.3 */
      {
	TAGGED *pt1, *pt2, t1, t2;

	// FD_VAL_2 
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	t2 = pt2[2];
	if (t2!=pt2[3])	/* [MC] 4.3.4 */
	  goto suspend;
	// FD_VAL_$1
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	t1 = pt1[2];
	if (t1!=pt1[3])	/* [MC] 4.3.4 */
	  goto suspend;
	// FD_SUB 
	if (TagIsSmall(t1)) {
	  TSUBCHK(t2,t1);
	} else
	  t2 = (t1==Inf ? Sup : Inf);
	gtemp = t2;
	// FD_PRUNE_TERM_COMPL 
	goto prune_term_compl;
      }

    CaseX(FD_T_PLUS_U_GE_C_3_12):	/* [MC] 4.3 */
      {
	TAGGED *pt1, *pt2, t1, t2;

	// FD_VAL_2 
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	t2 = pt2[2];
	if (t2!=pt2[3])	/* [MC] 4.3.4 */
	  goto suspend;
	// FD_MAX_$1
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[3];
	// FD_SUB 
	if (TagIsSmall(t1)) {
	  TSUBCHK(t2,t1);
	} else
	  t2 = (t1==Inf ? Sup : Inf);
	min2 = t2;
	// FD_PRUNE_RANGE_CO 
	max2 = Sup;
	goto prune_range;
      }

    CaseX(FD_X_PLUS_Y_EQ_U_PLUS_C_4_123):
      {
	TAGGED *pt1, *pt2, *pt3, t1, t2, t3;
	int add3, add5;
	
        // FD_MIN_$1
	t2 = atts[*code++];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	if (pt2[2]!=pt2[3])
	  ground = 0;
	t2 = pt2[2];
        // FD_MAX_$2
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	add3 = (int)*code++;
        // FD_SUB/FD_ADD $3
	if (add3) {
	  t1 = pt1[2];
	  if (TagIsSmall(t2)) {
	    if (TagIsSmall(t1)) {
	      TADDCHK(t2,t1);
	    } else
	      t2 = t1;		/* [MC] SPRM 13718-13720 */
	  } else if (t2==t1)
	    goto abort;
	} else {
	  t1 = pt1[3];
	  if (TagIsSmall(t2)) {
	    if (TagIsSmall(t1)) {
	      TSUBCHK(t2,t1);
	    } else
	      t2 = (t1==Inf ? Sup : Inf);
	  } else if (t2==t1)
	    goto abort;
	}
        // FD_VAL $4
	t3 = atts[*code++];
	DerefAttribute(t3,t3);
	pt3 = TagToSTR(t3);
	t3 = pt3[2];
	if (t3!=pt3[3])	/* [MC] 4.3.4 */
	  goto suspend;
	add5 = (int)*code++;
        // FD_ADD/FD_SUB $5
	if (add5) {
	  if (TagIsSmall(t2)) {
	    TADDCHK(t2,t3);
	  }
	} else {
	  if (TagIsSmall(t2)) {
	    TSUBCHK(t2,t3);
	  }
	}
	min2 = t2;
        // FD_MAX_$1
	t2 = pt2[3];
        // FD_MIN_$2
        // FD_SUB/FD_ADD $3
	if (add3) {
	  t1 = pt1[3];
	  if (TagIsSmall(t2)) {
	    if (TagIsSmall(t1)) {
	      TADDCHK(t2,t1);
	    } else
	      t2 = t1;		/* [MC] SPRM 13718-13720 */
	  } else if (t2==t1)
	    goto abort;
	} else {
	  t1 = pt1[2];
	  if (TagIsSmall(t2)) {
	    if (TagIsSmall(t1)) {
	      TSUBCHK(t2,t1);
	    } else
	      t2 = (t1==Inf ? Sup : Inf);
	  } else if (t2==t1)
	    goto abort;
	}
        // FD_VAL $4
        // FD_SUB/FD_ADD $5
	if (add5) {
	  if (TagIsSmall(t2)) {
	    TADDCHK(t2,t3);
	  }
	} else {
	  if (TagIsSmall(t2)) {
	    TSUBCHK(t2,t3);
	  }
	}
	max2 = t2;
        // FD_PRUNE_RANGE_CC
	goto prune_range;
      }
      
    CaseX(FD_X_PLUS_Y_PLUS_Z_EQ_C_4_123):
      {
	TAGGED *pt1, *pt2, *pt3, t1, t2, t3;
	
        // FD_VAL 3
	t3 = atts[3];
	DerefAttribute(t3,t3);
	pt3 = TagToSTR(t3);
	t3 = pt3[2];
	if (t3!=pt3[3])	/* [MC] 4.3.4 */
	  goto suspend;
        // FD_MAX_$1
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[3];
        // FD_SUB
	if (TagIsSmall(t1)) {
	  TSUBCHK(t3,t1);
	} else
	  t3 = (t1==Inf ? Sup : Inf);
        // FD_MAX_$2
	t2 = atts[*code++];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	if (pt2[2]!=pt2[3])
	  ground = 0;
	t2 = pt2[3];
        // FD_SUB
	if (TagIsSmall(t3)) {
	  if (TagIsSmall(t2)) {
	    TSUBCHK(t3,t2);
	  } else
	    t3 = (t2==Inf ? Sup : Inf);
	} else if (t3==t2)
	  goto abort;
	min2 = t3;
        // FD_VAL 3
	t3 = pt3[2];
        // FD_MIN_$1
	t1 = pt1[2];
        // FD_SUB
	if (TagIsSmall(t1)) {
	  TSUBCHK(t3,t1);
	} else
	  t3 = (t1==Inf ? Sup : Inf);
        // FD_MIN_$2
	t2 = pt2[2];
        // FD_SUB
	if (TagIsSmall(t3)) {
	  if (TagIsSmall(t2)) {
	    TSUBCHK(t3,t2);
	  } else
	    t3 = (t2==Inf ? Sup : Inf);
	} else if (t3==t2)
	  goto abort;
	max2 = t3;
        // FD_PRUNE_RANGE_CC
	goto prune_range;
      }
      
    CaseX(FD_ONEOF_X_Y_EQ_Z_3_12):
      {
	TAGGED *pt1, *pt2, t1, t2;
	
        // FD_DOM_$1
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[1];
        // FD_DOM_2
	t2 = atts[2];
	DerefAttribute(t2,t2);
	pt2 = TagToSTR(t2);
	if (pt2[2]!=pt2[3])
	  ground = 0;
	t2 = pt2[1];
        // FD_INTER_DD
        // FD_CHECK_UNION offset=6
	if (fd_compare(t1,t2)!=FDI_DISJOINT)
	  goto ent_link_ov_loop;
        // FD_DOM_2
        // FD_PRUNE
	gtemp = t2;
	goto prune;
      }

    CaseX(FD_ONEOF_X_Y_EQ_Z_3_3):
      {
	TAGGED *pt0, *pt1, t0, t1;
	
        // FD_DOM_0
	t0 = atts[0];
	DerefAttribute(t0,t0);
	pt0 = TagToSTR(t0);
	if (pt0[2]!=pt0[3])
	  ground = 0;
	t0 = pt0[1];
        // FD_DOM_1
	t1 = atts[1];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[1];
        // FD_UNION_DD
	gtemp = fd_union(wam, t0,t1);
        // FD_PRUNE
	goto prune;
      }
 
    CaseX(FD_ABS_X_EQ_Y_2_12):
      {
	TAGGED *pt1, t1, t2;
	
        // FD_DOM_$1
	t1 = atts[*code++];
	DerefAttribute(t1,t1);
	pt1 = TagToSTR(t1);
	if (pt1[2]!=pt1[3])
	  ground = 0;
	t1 = pt1[1];
        // FD_DOM_$1
        // FD_CONST 0
        // FD_SETNEG
	t2 = fd_neg_offset(wam, t1,TaggedZero);
        // FD_UNION_DD
	gtemp = fd_union(wam, t1,t2);
        // FD_PRUNE
	goto prune;
      }
      
    CaseX(FD_TEST_RANGE_OO):	/* inf..sup (weird) */
      truth_value = MakeSmall(current->truth_value);
      goto ret_entailed;
      
    CaseX(FD_TEST_RANGE_OC):	/* int -> */
				/* inf..Max */
      max2 = Pop;
      min2 = Inf;
      goto test_range;

    CaseX(FD_TEST_RANGE_CO):	/* int -> */
				/* Min..sup */
      max2 = Sup;
      min2 = Pop;
      goto test_range;

    CaseX(FD_TEST_RANGE_CC):	/* int x int -> */
				/* Min..Max */
      max2 = Pop;		
      min2 = Pop; 
    test_range:
      truth_value = MakeSmall(current->truth_value);
      if (min2==max2) {
	if (!TagIsSmall(min2)) {
	  /* bug in 3.8.3
	     fd.fd_overflow = TRUE;
	     goto ov_loop;
	  */
	  goto ret_disentailed;
	}
	gtemp = min2;
	goto test_term;
      } else if (FDgt(min2,max2)) {
	goto ret_disentailed;
      } else if (dvar.min==dvar.max) {
	if (InInterval(dvar.min,min2,max2))
	  goto ret_entailed;
	else
	  goto ret_disentailed;
      } else {		/* need precise test here! */
	switch (fd_compare_interval(dvar.set,min2,max2)) {
	case FDI_SUBSET:
	case FDI_EQUAL:
	  goto ret_entailed;
	case FDI_INTERSECT:
	case FDI_SUPERSET:
	  goto link_ov_loop;
	default: /* disjoint */
	  goto ret_disentailed;
	}
      }
	  
    CaseX(FD_TEST_TERM_COMPL):	/* int -> */
      truth_value = MakeSmall(current->truth_value)^IStep(1);
      gtemp = Pop;
      goto test_term;

    CaseX(FD_TEST_TERM):	/* int -> */
      truth_value = MakeSmall(current->truth_value);
      gtemp = Pop; 
    test_term:
      if (dvar.min==dvar.max) {
	if (dvar.min!=gtemp)
	  goto ret_disentailed;
	else
	  goto ret_entailed;
      } else {
	if (!fd_member(gtemp,dvar.set))
	  goto ret_disentailed;
	else
	  goto link_ov_loop;
      }

    CaseX(FD_TEST_COMPL):	/* set -> */
      truth_value = MakeSmall(current->truth_value);
      gtemp = Pop;
      if (gtemp==EmptySet) {
	goto ret_entailed;
      } else if (dvar.min==dvar.max) {
	if (!fd_member(dvar.min,gtemp))
	  goto ret_entailed;
	else
	  goto ret_disentailed;
      } else {
	switch (fd_compare(dvar.set,gtemp)) {
	case FDI_DISJOINT:
	  goto ret_entailed;
	case FDI_INTERSECT:
	case FDI_SUPERSET:
	  goto link_ov_loop;
	default: /* subset */
	  goto ret_disentailed;
	}
      }

    CaseX(FD_TEST):		/* set -> */
      truth_value = MakeSmall(current->truth_value);
      gtemp = Pop;
      if (gtemp==EmptySet) {
	goto ret_disentailed;
      } else if (dvar.min==dvar.max) {
	if (fd_member(dvar.min,gtemp))
	  goto ret_entailed;
	else
	  goto ret_disentailed;
      } else {
	switch (fd_compare(dvar.set,gtemp)) {
	case FDI_SUBSET:
	case FDI_EQUAL:
	  goto ret_entailed;
	case FDI_INTERSECT:
	case FDI_SUPERSET:
	  goto link_ov_loop;
	default: /* disjoint */
	  goto ret_disentailed;
	}
      }
    }
  }

 ret_disentailed:
  truth_value ^= IStep(1);
  if (! ground)
    goto link_ov_loop;
 ret_entailed:
  {
    TAGGED zero_one, entvar;
      
    fd.entailments++;
    DerefArg(entvar,X(0),4);	/* Ent, always a HVA */
    BindHVA(entvar,truth_value);
				/* propagate to the zero_one variable */
    zero_one = CTagToArg(X(0),5);
    DerefSwitch(zero_one,goto ret_entailed_2;);
    if (0) {
    ret_entailed_2:
      dvar_init_ix(&dvar,CTagToArg(X(0),7),zero_one);
      switch (dvar_fix_value_t(&dvar,truth_value)) {
      case -1:
	goto fail;
      case 0:
	goto link_ov_loop;
      }
      dvar_export_ix(wam, &dvar, fdbg);	/* GC */
    }
  }
  goto link_ov_loop;

 abort:
 ent_link_ov_loop:
  if (ground) {
    TAGGED entvar;
    
    fd.entailments++;
    DerefArg(entvar,X(0),4);	/* Ent, always a HVA */
    BindHVA(entvar,truth_value);
  }
 link_ov_loop:
  gtemp = RefMutable(X(1));
  if (gtemp&IStep(4))		/* STATUS: not idempotent */
    gtemp &= ~IStep(8);		/* STATUS: not current */
  else
    gtemp &= ~IStep(9);	       /* STATUS: not current, not enqueued */
  if (!(gtemp&IStep(2))) {     /* STATUS: linked */
    gtemp |= IStep(2);	       /* STATUS: linked */
    link_indexical(wam, current);
  }
  FD_UPDATE_MUTABLE(gtemp,X(1));
 ov_loop:
  return fd_check_overflow(wam, CTagToArg(X(0),2));
 fail:
  return FALSE;
 suspend:
  gtemp = RefMutable(X(1));
  gtemp &= ~IStep(9);	       /* STATUS: not current, not enqueued */
  FD_UPDATE_MUTABLE(gtemp,X(1));
  suspend_indexical(wam, current);
  return TRUE;
}

/* '$fd_eval_indexical'(+Constraint, -Actions, +Indexical) -- only called if fd.debugging */
void SPCDECL
prolog_fd_eval_indexical(Wam wam, SP_term_ref Constraint, SP_term_ref Actions, SP_term_ref Indexical)
{
  SP_uinteger entailments = fd.entailments;
  (void)Constraint;
  
  DEREF(X(0),RefTerm(Indexical));
  dvar_export_start(wam);	/* X(2) = [], for actions */
  if (!eval_indexical(wam,Indexical)) {
    if (!fd.fd_overflow) {
      --fd.failures;		/* did ++; will do it one more time */
      dvar_export_done(wam,Actions,-1);
    }
  } else if (entailments < fd.entailments) {
    --fd.entailments;		/* did ++; will do it one more time */
    dvar_export_done(wam,Actions,TRUE);
  } else {
    dvar_export_done(wam,Actions,FALSE);
  }
}


/* '$fd_evaluate_indexical'(-RC, -Global)

Indexical = ix(+Ptr,+Goal,+StatusM,?Ent,-ZeroOne,+Attv,+LAttr)

X(1) holds status mutable
X(2) holds action list if fd.debugging

Xref EVAL_ARITY
*/
SP_integer SPCDECL
prolog_fd_evaluate_indexical(Wam wam, SP_term_ref Global)
{
  TAGGED statem, statmut, gtemp;
  SP_term_ref State = SP_new_term_ref();
  SP_term_ref Actions = SP_new_term_ref();
  DECL_UPDATE_MUTABLE;
  
  if (!Global)
    return eval_indexical(wam,Global);
  else if (fd.batching)
    return 0;
  fd_sync(wam);
  fd.fd_overflow = 0;
  X(EVAL_ARITY-1) = atom_nil;	/* ensure EVAL_ARITY valid X regs */
 restart:
  if (OffHeaptopUnlikely(w->global_top,w->heap_warn_soft)) {
    SP_ASSERT(w->heap_warn_soft || w->wake_count==0);
    return 1;
  }
  switch (fd_dequeue(wam,&X(0))) {
  case 1:
    RefTerm(Global) = X(0);
    fd.resumptions++;
    if (/*!w->heap_warn_soft ||*/ fd.debugging)
      return 2;
    if (eval_indexical(wam,Global))
      goto restart;
  case -1:
    goto fail;
  case 2:
    RefTerm(Global) = X(0);
    statmut = CTagToArg(X(0),3);
    gtemp = RefMutable(statmut);
    if (gtemp&IStep(4))		/* STATUS: not idempotent */
      gtemp &= ~IStep(1);	/* STATUS: not enqueued */
    FD_UPDATE_MUTABLE(gtemp|IStep(8), statmut); /* STATUS: current */
    fd.resumptions++;
    if (/*!w->heap_warn_soft ||*/ fd.debugging) /* status must be updated even if debugging */
      return 2;
    statem = CTagToArg(X(0),1);
    RefTerm(State) = RefMutable(statem);
    if (!dispatch_global_fast(wam, CTagToArg(X(0),2),State,State,Actions,Global))
      return 2;
    /* X(1) = RefTerm(Global); */
    DerefArg(statem,X(1),1);
    FD_UPDATE_MUTABLE(RefTerm(State),statem);
    switch (fd_prune_and_enqueue(wam,Actions,Global)) {
    case SP_FAILURE:
    fail:
      fd.failures++;
      SP_fail();
      return -1;
    case SP_ERROR:
      SP_exception_term(Actions);
      SP_raise_exception(Actions);
      return 0;
    default:
      goto restart;
    }
  }
  fd_end(wam);
  return 0;
}

/* '$fd_in_interval'(+Var, +Min, +Max, +Init) */
/* X(1) temporarily holds Var's FD att */
/* implies $fd_begin if Init=1 */
void SPCDECL
prolog_fd_in_interval(Wam wam,
		      SP_term_ref Var,
		      SP_term_ref Min,
		      SP_term_ref Max,
		      SP_integer init)
{
  TAGGED var, min, max;
  struct dvar dvar;

  if (init)
    SP_MANGLE(prolog_fd_begin)(wam);
  var = RefTerm(Var);
  min = RefTerm(Min);
  max = RefTerm(Max);
  DerefNonvar(min);
  DerefNonvar(max);
  DerefSwitch(var,goto prune;);
  if (TagIsSmall(var)) {
    if (!InInterval(var,min,max))
      goto fail;
    else
      return;
  }
prune:
  fd_sync(wam);
  X(1) = fd_check_argument(wam, var,Inf,Sup,Sup);
  if (!X(1))
    goto fail;
  w->numstack_end = NULL;
  dvar_init_ix(&dvar,X(1),var);
  switch (dvar_fix_interval_t(&dvar,min,max)) {
  case -1:
    goto fail;
  case 0:
    goto ret;
  }
  dvar_export_ix(wam, &dvar, FALSE); /* GC */
 ret:
  return;
fail:
  fd.failures++;
  SP_fail();
}

/* '$fd_in_set'(+Var, +Domain, +Init) */
/* X(1) temporarily holds Var's FD att */
/* implies $fd_begin if Init=1 */
void SPCDECL
prolog_fd_in_set(Wam wam,
		 SP_term_ref Var,
		 SP_term_ref Domain,
		 SP_integer init)
{
  TAGGED var, domain;
  struct dvar dvar;
  
  var = RefTerm(Var);
  domain = RefTerm(Domain);
  DerefNonvar(domain);
  if (domain==EmptySet)
    goto fail;
  if (init)
    SP_MANGLE(prolog_fd_begin)(wam);
  DerefSwitch(var,goto prune;);
  if (TagIsSmall(var)) {
    if (!fd_member(var,domain))
      goto fail;
    else
      return;
  }
 prune:
  fd_sync(wam);
  X(1) = fd_check_argument(wam, var,Inf,Sup,Sup);
  if (!X(1))
    goto fail;
  w->numstack_end = NULL;
  dvar_init_ix(&dvar,X(1),var);
  if (CTagToCdr(domain)==EmptySet) { /* [MC] SPRM 10707 prefer interval to set */
    domain = CTagToCar(domain);
    switch (dvar_fix_interval_t(&dvar,RangeMin(domain),RangeMax(domain))) {
    case -1:
      goto fail;
    case 0:
      goto ret;
    }
  } else {
    switch (dvar_fix_set(&dvar,
			 fd_localize(wam, domain))) {	/* [MC] SPRM 10707 avoid structure sharing */
    case -1:
      goto fail;
    case 0:
      goto ret;
    }
  }
  dvar_export_ix(wam, &dvar, FALSE); /* GC */
 ret:
  return;
fail:
  fd.failures++;
  SP_fail();
}

