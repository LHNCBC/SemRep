/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

#if LogSizeOfWord==2
# define SignBit 0x80000000
#else
# define SignBit 0x8000000000000000
#endif

static SP_BOOL
set_singleton(Wam wam, TAGGED val, TAGGED mutable)
{
  TAGGED t1, *h;
  DECL_UPDATE_MUTABLE;
  
  t1 = RefMutable(mutable);
  if (!fd_member(val, DomainSet(t1))) {
    return FALSE;
  } else if (TagToSTR(t1) >= w->global_uncond) {
				/* can safely smash it */
    TAGGED *arg = TagToArg(t1,0);
      
    h = w->global_top;
    *h++ = MakeList(arg+2);
    *h++ = atom_nil;
    w->global_top = h;
    arg[1] = MakeList(h-2);
    arg[2] = val;
    arg[3] = val;
    arg[4] = TaggedOne;
  } else if (val==TaggedZero) {
    TAGGED dom_zero = RefTerm(CLPFD_DATA_TERM_REF) + WD(15);
    FD_UPDATE_MUTABLE(dom_zero, mutable);
  } else if (val==TaggedOne) {
    TAGGED dom_one = RefTerm(CLPFD_DATA_TERM_REF) + WD(15+18);
    FD_UPDATE_MUTABLE(dom_one, mutable);
  } else {
    h = w->global_top;
    *h++ = MakeList(w->global_top+4);
    *h++ = atom_nil;
    *h++ = functor_dom4;
    *h++ = MakeList(w->global_top);
    *h++ = val;
    *h++ = val;
    *h++ = TaggedOne;
    w->global_top = h;
    FD_UPDATE_MUTABLE(MakeStructure(h-5), mutable);
  }
  return TRUE;
}

/* '$fd_set_singleton(wam, +Val, +Mutable) */
void SPCDECL
prolog_fd_set_singleton(Wam wam,
			SP_term_ref ValR,
			SP_term_ref MutR)
{
  TAGGED val, mutable;

  val = RefTerm(ValR);
  mutable = RefTerm(MutR);
  DerefNonvar(val);
  DerefNonvar(mutable);
  if (!set_singleton(wam,val,mutable))
    SP_fail();
}

/* '$fd_unify(+X, +Y, -RC) */
SP_integer SPCDECL
prolog_fd_unify(Wam wam,
		SP_term_ref XR,
		SP_term_ref YR)
{
  int wc, refs, refs0;
  TAGGED *tr, *tw, *h=w->global_top;

  if (!cunify(w,RefTerm(XR),RefTerm(YR)))
    goto fail;
  tr = w->trail_top;
  wc = w->wake_count;
  while (wc>0) {
    TAGGED ref = *--tr;
    if (IsTrailedCVA(ref,h)) {
      wc--;
      if (!TagIsSmall(CTagToREF(ref)) || !pure_attributes(ref,fd.fd_module))
	return 0;	      /* use generic wakeup mechanism */	
    }
  }
  /* use accelerated CLFPD specific wakeup mechanism */
  refs0 = refs = SP_new_term_refs(2*w->wake_count); /* Foreign language Interface resets it */
  tw = tr;
  while (tr<w->trail_top) {
    TAGGED ref = *tr++;
    if (IsTrailedCVA(ref,h)) {
      TAGGED attr = get_attributes(ref,fd.fd_module);
      TAGGED value = CTagToREF(ref);
      
      RefTerm(refs++) = attr;
      RefTerm(refs++) = value;
      if (TagIsSTR(attr)) {
	if (!CondCVA(ref)) {
	  CTagToHeader(ref) = atom_nil; /* destruct into HVA and untrail  */
	} else {
	  *tw++ = ref;
	}
      } else {
	*tw++ = ref;
      }
    } else {
      *tw++ = ref;
    }
  }
  w->trail_top = tw;
  w->wake_count = 0;
#if !NO_INT_HEAP_WARN
  w->heap_warn_soft = w->heap_warn;
  if (!w->int_heap_warn)
    SetSoftEvent;
#endif  /* !NO_INT_HEAP_WARN */
  SP_MANGLE(prolog_fd_begin)(wam); /* calls fd_update_mutable(wam, ) -- must be AFTER the untrailing above */
  while (refs0<refs) {
    TAGGED attr = RefTerm(refs0++);
    TAGGED value = RefTerm(refs0++);
    TAGGED suspm, domm;
      
    if (TagIsSTR(attr)) {
      RequireHeap1(7,attr,0);
      AttrToSuspM(attr,suspm);
      AttrToDomM(attr,domm);
      if (!set_singleton(wam,value,domm)) /* trail hazard */
	goto fail;
      fd_enqueue_all(wam, MASK_SINGLETON, suspm, value); /* heap+trail hazard */
    }
  }
  return 1;
 fail:
  SP_fail();
  return 0;
}

/* Support for incremental variable choice in labeling */

struct labeling_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;

  SP_integer stamp;
  SP_integer *key;			/* cached domain size */
  int *heap;			/* kept in whack wrt. key */
  int *vheap;
  SP_globref refbase;
  int heapsize;
  int heapsize_committed;
  int nvars;
  int type;			/* 1-min, 2-max, 3-ff/ffc */
};

static void SPCDECL labeling_destructor(void *pdata_v)
{
  struct labeling_data *pdata = (struct labeling_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata);
}

#define SWAP(I,J)				\
{						\
  int vi = pdata->heap[I];			\
  int vj = pdata->heap[J];			\
  pdata->heap[I] = vj;				\
  pdata->heap[J] = vi;				\
  pdata->vheap[vi] = (J);			\
  pdata->vheap[vj] = (I);			\
}

/* TODO: generalize
   For now: (i) size, (ii) ref
*/
static int
cmp_items(struct labeling_data *pdata, int i1, int i2)
{
  return (pdata->key[i1] < pdata->key[i2] ? -1 :
          pdata->key[i1] > pdata->key[i2] ?  1 :
	                i1 < i2               ? -1 : 1);
}



/* the key if heap[i] has decreased -
   move it up until it is at the top or GT its parent.
*/
static void 
decrease_key(struct labeling_data *pdata,
	     int i)
{
  int p = (i-1)>>1;
  while (i>0 && cmp_items(pdata,pdata->heap[i],pdata->heap[p])<0) {
    SWAP(i,p);
    i = p;
    p = (i-1)>>1;
  }
}

static void 
spheapify(struct labeling_data *pdata,
	  int i)
{
  int *heap = pdata->heap;
  
  for (;;) {
    int l = (i<<1)+1;
    int topmost = i;
    if (l<pdata->heapsize && cmp_items(pdata,heap[l],heap[topmost])<0)
      topmost = l;
    if (l+1<pdata->heapsize && cmp_items(pdata,heap[l+1],heap[topmost])<0)
      topmost = l+1;
    if (topmost==i)
      break;
    SWAP(i,topmost);
    i = topmost;
  }
}

static void
set_key(struct labeling_data *pdata,
	int ix)
{
  TAGGED tmp = RefGlob((ix<<1)+pdata->refbase);
  
  DerefAttribute(tmp,tmp); /* get dom/4 term */
  switch (pdata->type) {
  case 1:
    pdata->key[ix] =  GetSmall0(DomainMin(tmp));
    break;
  case 2:
    pdata->key[ix] = -GetSmall0(DomainMax(tmp));
    break;
  case 3:
  default:
    pdata->key[ix] =  DomainSizeAsInt(tmp);
  }
}

static DAEMON_RC SPCDECL 
labeling_daemon(Wam wam,
		void *vdata,
		SP_globref attr_ref,
		TAGGED *global)
{
  struct labeling_data *pdata = (struct labeling_data *)vdata;
  TAGGED tstate;
  int ar, state_stamp;

  tstate = RefMutable(CTagToArg(*global,1));
  ar = Arity(TagToHeadfunctor(tstate));
  state_stamp = GetSmall_int(CTagToArg(tstate,ar));
  if (pdata->stamp==state_stamp) { /* incremental */
    int ix = (int)((attr_ref - pdata->refbase)>>1);
    int vh = pdata->vheap[ix];
    TAGGED tvar;
    SP_BOOL buried;

    (void)fd_daemon_copy_state(wam,global,&buried);
    pdata->stamp++;
    tvar = RefGlob(attr_ref+1);
    DerefSwitch(tvar,goto isvar;);
    --pdata->heapsize;
    if (pdata->heapsize>vh) {
      SWAP(vh,pdata->heapsize);
      spheapify(pdata,vh);
    }
    return DAEMON_FIX;
  isvar:    
    set_key(pdata,ix);
    switch (pdata->type) {
    case 1:
    case 2:
      spheapify(pdata,vh);
      break;
    case 3:
    default:
      decrease_key(pdata,vh);
    }
  }
  return DAEMON_FIX;
}

void SPCDECL
prolog_fd_labeling(Wam wam,
		   SP_term_ref State0,
		   SP_term_ref State,
		   SP_term_ref SelectedOrNil,
		   SP_term_ref Global)
{
  TAGGED tvec, telt, handle, functor;
  SP_BOOL committed;		/* TRUE if state can't be backtracked over */
  int i;
  SP_integer state_stamp;
  SP_integer total_size;
  char *ptr;
  struct labeling_data *pdata;

  (void)State0;                 /* [PM] 3.9b5 avoid -Wunused */
/*    X(0) = RefTerm(State0); */
  RefTerm(State) = fd_unify_output_state(wam,&handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct labeling_data,handle);
  } else {			/* build persistent state */
    int nvars;
    DerefArg(tvec,X(0),1);
    DerefArg(functor,X(0),2);
    nvars = fd_list_length(tvec);
    total_size = nvars*(sizeof(int)+sizeof(int)+sizeof(SP_integer));
    pdata = Palloc(struct labeling_data, total_size, handle);
    ptr = (char *)(pdata+1);
  
    pdata->key = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->heap = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->vheap = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    pdata->destructor = labeling_destructor;
    pdata->daemon = labeling_daemon;
    FD_STORE_SPENV(pdata->spenv);
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    pdata->stamp = state_stamp-1;
    pdata->nvars = nvars;
    pdata->heapsize_committed = nvars;
    pdata->heapsize = nvars;
    functor = SetArity(functor,1);
    pdata->type = (functor==fd.functor_min ? 1 :
		   functor==fd.functor_max ? 2 : 3);    
    DerefArg(tvec,X(0),1);
    for (i=0; i<nvars; i++) {
      SP_globref ref = (i<<1)+pdata->refbase;
      
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      fd_get_var_and_attr(telt,ref);
      pdata->heap[i] = pdata->vheap[i] = i;
    }
    for (i=0; i<nvars; i++) {
      SP_globref ref = (i<<1)+pdata->refbase;
      struct dvar dv;
      
      dvar_init(&dv, ref, ref+1);
      dvar_attach_daemon(wam, &dv, pdata, RefTerm(Global), functor);
    }
  }

  if (state_stamp != pdata->stamp) { /* non-incremental */
    pdata->heapsize = pdata->heapsize_committed;
    for (i=0; i<pdata->heapsize; i++) { /* refresh key[] */
      int ix = pdata->heap[i];
      SP_globref ref = (ix<<1)+pdata->refbase;
      TAGGED tvar = RefGlob(ref+1);

      DerefSwitch(tvar,goto isvar;);
      pdata->key[ix] = SignBit;
      continue;
    isvar:
      set_key(pdata,ix);
    }
    for (i=(pdata->heapsize-2)>>1; i>=0; i--) {	/* restore heap property */
      spheapify(pdata,i);
    }
    while (pdata->heapsize>0 &&
	   pdata->key[pdata->heap[0]]==(SP_integer)SignBit) { /* delete integers */
      --pdata->heapsize;
      if (pdata->heapsize>0) {
	SWAP(0,pdata->heapsize);
	spheapify(pdata,0);
      }
    }
  }
  pdata->stamp = state_stamp+1;

  /* INVARIANT: heap property holds + no integers in heap */

  if (committed)
    pdata->heapsize_committed = pdata->heapsize;
  if (pdata->heapsize>0) {
    int elt = pdata->heap[0];
    telt = RefGlob((elt<<1)+1+pdata->refbase);
  } else {
    telt = atom_nil;
    Pfree;
  }
  RefTerm(SelectedOrNil) = telt;
}

/******* Variable choice support. *******/

enum var_choice {
  min=0, max, ff, ffc, aff, occ, mreg
};

static void
rank_var(Wam wam,
	 TAGGED var,
	 enum var_choice option,
	 SP_integer *key1, SP_integer *key2)
{
  TAGGED attr = fd_check_argument(wam, var,Inf,Sup,Sup);
  TAGGED dom4, susp_mut;

  DerefAttribute(dom4,attr); /* get dom/4 term */

  switch (option) {
  case min:
    *key1 = GetSmall(DomainMin(dom4));
    break;
  case max:			/* maximize */
    *key1 = -GetSmall(DomainMax(dom4));
    break;
  case ffc:			/* maximize occurrence */
    AttrToSuspM(attr,susp_mut);
    *key2 = -GetSmall(CTagToArg(RefMutable(susp_mut),1));
    /* FALLTHROUGH */
  case ff:
    *key1 = DomainSizeAsInt(dom4);
    break;
  case aff:			/* maximize */
    *key1 = -DomainSizeAsInt(dom4);
    break;
  case occ:			/* maximize */
    AttrToSuspM(attr,susp_mut);
    *key1 = -GetSmall(CTagToArg(RefMutable(susp_mut),1));
    break;
  case mreg:			/* maximize */
    {
      TAGGED first = DomainMin(dom4);
      TAGGED set = DomainSet(dom4);
      TAGGED head = CTagToCar(set);
      if (RangeMax(head)!=first) { /* |first interval|>1 */
	*key1 = -1;
      } else {
	set = CTagToCdr(set);
	head = CTagToCar(set);
	*key1 = GetSmall(first) - GetSmall(RangeMin(head));
      }
    }
    break;
  }
}

/* '$fd_delete'(+List, -Variable, +Option) */
/* Option in [min,max,ff,ffc,anti_first_fail,occurrence,max_regret] */
void SPCDECL
prolog_fd_delete(Wam wam,
			SP_term_ref ListR,
			SP_term_ref VarR,
			SP_atom aoption)
{
  enum var_choice option;
  SP_integer bestk1=0, bestk2=0;
  SP_integer currk1=0, currk2=0;
  TAGGED bestvar, list, var;
  int i;
 
  for (i=0;; i++)
    if (fd.var_options[i]==aoption)
      break;
  option = i;
  list = RefTerm(ListR);	/* known to be list */
  DerefNonvar(list);
  DerefCar(var,list);		/* known to be var */
  DerefCdr(list,list);
  rank_var(wam, var, option, &bestk1, &bestk2);
  bestvar = var;
  while (TagIsLST(list) && !(option==ff && bestk1==2)) {
    DerefCar(var,list);
    DerefCdr(list,list);
    if (!IsVar(var))
      continue;
    rank_var(wam, var, option, &currk1, &currk2);
    if (currk1<bestk1 || (currk1==bestk1 && currk2<bestk2)) {
      bestk1 = currk1;
      bestk2 = currk2;
      bestvar = var;
    }
  }
  RefTerm(VarR) = bestvar;  
}

