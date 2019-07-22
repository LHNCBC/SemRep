/* Copyright(C) 1999, Swedish Institute of Computer Science */

#include "fd.h"
#include "dvars.h"

/* operations on double word entities, for scalar_product */

/* Some "numbers" and their representation:

   0          : < 0, 0, 0>
   0-sup      : <-1, 0, 0>
   0+sup      : < 1, 0, 0>
  -1          : <-1,-1,-1>
  -1-sup      : <-2,-1,-1>
  -1+sup      : < 0,-1,-1>
   1          : < 0, 0, 1>
   1-sup      : <-1, 0, 1>
   1+sup      : < 1, 0, 1>

 */

struct dword {
  SP_WORD infw;
  union {
    SP_UWORD asuword;
    SP_WORD asword;
  } msw;
  union {
    SP_UWORD asuword;
    SP_WORD asword;
  } lsw;
};

typedef struct dword *Dword;

#define issmall_d(x)					\
    (((x)->infw==0 &&					\
      (x)->msw.asword==0 &&				\
      (x)->lsw.asword>=0 && (x)->lsw.asword<HighInt) ||	\
     ((x)->infw==-1 &&					\
      (x)->msw.asword==-1 &&				\
      (x)->lsw.asword<0 && (x)->lsw.asword>=-HighInt))	\

#define isfinite_d(x)				\
    (((x)->infw==0 &&				\
      (x)->msw.asword>=0) ||			\
     ((x)->infw==-1 &&				\
      (x)->msw.asword<0))			\

#define islong_d(x)				\
    (((x)->infw==0 &&				\
      (x)->msw.asword==0 &&			\
      (x)->lsw.asword>=0) ||			\
     ((x)->infw==-1 &&				\
      (x)->msw.asword==-1 &&			\
      (x)->lsw.asword<0))			\

#define cmp_deqd(X,Y)				\
    (((X)->infw==(Y)->infw) &&			\
     ((X)->lsw.asword==(Y)->lsw.asword) &&	\
     ((X)->msw.asword==(Y)->msw.asword))	\

#define cmp_dltd(x,y)				\
    (((x)->infw==(y)->infw) ?			\
      (((x)->msw.asuword==(y)->msw.asuword) ?	\
       ((x)->lsw.asuword < (y)->lsw.asuword) :	\
       ((x)->msw.asuword < (y)->msw.asuword)) :	\
     ((x)->infw < (y)->infw))			\

#define cmp_dgtd(x,y)				\
    (((x)->infw==(y)->infw) ?			\
      (((x)->msw.asuword==(y)->msw.asuword) ?	\
       ((x)->lsw.asuword > (y)->lsw.asuword) :	\
       ((x)->msw.asuword > (y)->msw.asuword)) :	\
     ((x)->infw > (y)->infw))			\

#define cmp_deqz(x)				\
    ((x)->infw==0 &&				\
     (x)->msw.asword==0 &&			\
     (x)->lsw.asword==0)			\

#define cmp_dltz(x)				\
    ((x)->infw < 0)				\

#define cmp_dgtz(x)				\
    (((x)->infw==0) ?				\
      (((x)->msw.asuword==0) ?			\
       ((x)->lsw.asuword > 0) :			\
       ((x)->msw.asuword > 0)) :		\
     ((x)->infw > 0))				\

#define cmp_dnez(x) (!cmp_deqz(x))

#define cmp_dgez(x) (!cmp_dltz(x))

#define cmp_dlez(x) (!cmp_dgtz(x))

static void 
add_ztd(TAGGED x,Dword y)
{
  if (TagIsSmall(x)) {
    y->lsw.asword = GetSmall0(x);
    y->msw.asword = (Tltz(x) ? -1 : 0);
  } else {
    TAGGED *s = TagToSTR(x);
    y->lsw.asuword = s[1];
    if (s[0]==BIGNUM_HEADER)
      y->msw.asword = ((SP_WORD)s[1]<0 ? -1 : 0);
    else
      y->msw.asuword = s[2];	/* a two-word bignum */
  }
  y->infw = (y->msw.asword<0 ? -1 : 0);
}

/* carry logic ripped off from bn_add() */
static void 
add_ddd(Wam wam, Dword x,Dword y,Dword z)
{
  struct dword sum_mem;
  Dword sum = &sum_mem;
  SP_UWORD carry;

  sum->lsw.asuword = x->lsw.asuword + y->lsw.asuword;
  carry = (sum->lsw.asuword < x->lsw.asuword);
  sum->msw.asuword = x->msw.asuword + y->msw.asuword + carry;
  if (carry) {
    carry = (sum->msw.asuword <= x->msw.asuword);
  } else {
    carry = (sum->msw.asuword <  x->msw.asuword);
  }
  sum->infw = x->infw + y->infw + carry;
  if ((x->infw<0)==(y->infw<0) && (y->infw<0)!=(sum->infw<0))
    fd.fd_overflow = (x->infw<0 ? 1 : 2);
  *z = *sum;
}

/* carry logic ripped off from bn_subtract() */
static void 
sub_ddd(Wam wam, Dword x,Dword y,Dword z)
{
  struct dword difference_mem;
  Dword difference = &difference_mem;
  SP_UWORD carry;

  difference->lsw.asuword = x->lsw.asuword - y->lsw.asuword;
  carry = (difference->lsw.asuword > x->lsw.asuword);
  difference->msw.asuword = x->msw.asuword - y->msw.asuword - carry;
  if (carry) {
    carry = (difference->msw.asuword >= x->msw.asuword);
  } else {
    carry = (difference->msw.asuword >  x->msw.asuword);
  }
  difference->infw = x->infw - y->infw - carry;
  if ((x->infw<0)!=(y->infw<0) && (y->infw<0)==(difference->infw<0))
    fd.fd_overflow = (x->infw<0 ? 1 : 2);
  *z = *difference;
}

static void 
sub_dld(Dword x,SP_integer y,Dword z)
{
  struct dword difference_mem;
  Dword difference = &difference_mem;
  SP_UWORD carry;
  SP_UWORD sign_extension = (y<0 ? -1 : 0);

  difference->lsw.asuword = x->lsw.asuword - (SP_UWORD)y;
  carry = (difference->lsw.asuword > x->lsw.asuword);
  difference->msw.asuword = x->msw.asuword - sign_extension - carry;
  if (carry) {
    carry = (difference->msw.asuword >= x->msw.asuword);
  } else {
    carry = (difference->msw.asuword >  x->msw.asuword);
  }
  difference->infw = x->infw - sign_extension - carry;
  *z = *difference;
}

static void 
sub_zdd(Dword y,Dword z)
{
  SP_UWORD carry;

  z->lsw.asuword = - y->lsw.asuword;
  carry = (z->lsw.asuword > 0);
  z->msw.asuword = - y->msw.asuword - carry;
  if (carry) {
    carry = 1;
  } else {
    carry = (z->msw.asuword >  0);
  }
  z->infw = - y->infw - carry;
}

static void 
add_dud(Dword x,SP_UWORD y,Dword z)
{
  struct dword sum_mem;
  Dword sum = &sum_mem;
  SP_UWORD carry;

  sum->lsw.asuword = x->lsw.asuword + y;
  carry = (sum->lsw.asuword < x->lsw.asuword);
  sum->msw.asuword = x->msw.asuword + carry;
  if (carry) {
    carry = (sum->msw.asuword <= x->msw.asuword);
  } else {
    carry = (sum->msw.asuword <  x->msw.asuword);
  }
  sum->infw = x->infw + carry;
  *z = *sum;
}

static void 
mul_uud(SP_UWORD x,SP_UWORD y,Dword z)
{
  int carry = 0;
  SP_UWORD lsw;
  SP_UWORD x0 = LOWER_UHALF(x);
  SP_UWORD x1 = (UHALF)(x>>HALFSHIFT);
  SP_UWORD y0 = LOWER_UHALF(y);
  SP_UWORD y1 = (UHALF)(y>>HALFSHIFT);
  SP_UWORD p01 = (SP_UWORD)x0*(SP_UWORD)y1;
  SP_UWORD p10 = (SP_UWORD)x1*(SP_UWORD)y0;
  
  z->lsw.asuword = (SP_UWORD)x0*(SP_UWORD)y0;
  z->msw.asuword = (SP_UWORD)x1*(SP_UWORD)y1;
  lsw = z->lsw.asuword + (p01<<HALFSHIFT);
  if (lsw<z->lsw.asuword)
    carry++;
  z->lsw.asuword = lsw;
  lsw = z->lsw.asuword + (p10<<HALFSHIFT);
  if (lsw<z->lsw.asuword)
    carry++;
  z->lsw.asuword = lsw;
  z->msw.asuword += (p01>>HALFSHIFT) + (p10>>HALFSHIFT) + carry;
  z->infw = 0;
}

static void 
mul_ltd(SP_WORD x,TAGGED ty,Dword z)
{
  z->infw = 0;
  z->msw.asword = 0;
  z->lsw.asword = 0;
  if (ty==Inf) {
    z->infw = (x>0 ? -1 : 1);	/* N.B. just +- infinity */
  } else if (ty==Sup) {
    z->infw = (x>0 ? 1 : -1);	/* N.B. just +- infinity */
  } else {
    SP_WORD y = GetSmall0(ty);
  
    if (x>>HALFSHIFT==x>>(HALFSHIFT-1) &&
	y>>HALFSHIFT==y>>(HALFSHIFT-1)) {
      z->lsw.asword = x*y;
      z->msw.asword = (z->lsw.asword<0 ? -1 : 0);
      z->infw = (z->lsw.asword<0 ? -1 : 0);
    } else {
      int negated = 0;
      
      if (x<0)
	x = -x, negated ^= 1;
      if (y<0)
	y = -y, negated ^= 1;

      mul_uud(x,y,z);
      if (negated)
	sub_zdd(z,z);
    }
  }
}

/* Roll-your-own division and remainder algorithms.
   Assume everything is unsigned.

   Consider X/Y, X = (A<<32)+B, |X|<=63, |Y|<=25.

   Let:    Q1 = (1<<32)/Y       |Q1|<=31
           R1 = (1<<32)%Y       |R1|<=25
	   Q2 = B/Y             |Q2|<=6
	   R2 = B%Y             |R2|<=25

   Case 1 (A=0): X/Y = Q2, X%Y = R2.

   Case 2 (A>0): X/Y = A*Q1 + Q2 + (A*R1 + R2)/Y
                 X%Y = (A*R1 + R2)%Y
*/

/* x is nonnegative! */
static void 
div_dud(Wam wam, Dword x,SP_UWORD y,Dword z)
{
  struct dword dividend = *x;
  struct dword term;
  SP_UWORD q1, r1, q2, r2;

  z->infw = x->infw;
  if (y==1) {
    z->lsw.asuword = x->lsw.asuword;
    z->msw.asuword = x->msw.asuword;
  } else {
    z->lsw.asuword = 0;
    z->msw.asuword = 0;
    {
      /* [PM] 4.3 avoid undefined overflow behavior when left-shifting signed integers. Clang sanitizer traps it. */
      SP_UWORD scaled_overflow_bit = (~(SP_UWORD)0)<<(WORDSIZE-1); /* [PM] 4.3 Was: ((SP_UWORD)(((SP_WORD)-1)<<(WORDSIZE-1))); */
      /* [PM] 4.3.3 GCC 6 now (correctly) chokes with "left shift of negative value" so we can not even have the assert here */
      /* [PM] 4.3.3 Was: COMPILE_TIME_ASSERT( ((~(SP_UWORD)0)<<(WORDSIZE-1)) == ((SP_UWORD)(((SP_WORD)-1)<<(WORDSIZE-1))) ); */

      /* note: dividend is two times too small */
      q1 = (scaled_overflow_bit/y)*2;
      r1 = (scaled_overflow_bit%y)*2;
    }
    if (r1>=y) {
      q1++;
      r1 -= y;
    }
    q2 = dividend.lsw.asuword/y;
    r2 = dividend.lsw.asuword%y;
    while (dividend.msw.asuword>0) {
      mul_uud(q1,dividend.msw.asuword,&term);
      add_ddd(wam, z,&term,z);
      add_dud(z,q2,z);
      mul_uud(r1,dividend.msw.asuword,&dividend);
      add_dud(&dividend,r2,&dividend);
      q2 = dividend.lsw.asuword/y;
      r2 = dividend.lsw.asuword%y;
    }
    add_dud(z,q2,z);
  }
}

/* round towards zero */
static void 
div_dld(Wam wam, Dword x,SP_WORD y,Dword z)
{
  if (y==1) {
    *z = *x;
  } else {
    int negx=0, negy=0;
    if (cmp_dltz(x)) {
      negx=1;
      sub_zdd(x,x);
    }
    if (y<0) {
      negy=1;
      y = -y;
    }
    div_dud(wam, x,y,z);
    if (negx)
      sub_zdd(x,x);
    if (negx^negy)
      sub_zdd(z,z);
  }
}

static SP_WORD 
div_dll(Wam wam, Dword x,SP_WORD y)
{
  struct dword z_mem;
  Dword z = &z_mem;

  div_dld(wam, x,y,z);
  return z->lsw.asword;
}

/* round towards -INF (floor) */
static void 
div_dldf(Wam wam, Dword x,SP_WORD y,Dword z)
{
  if (y==1) {
    *z = *x;
  } else {
    int negx=0, negy=0;
    if (cmp_dltz(x)) {
      negx=1;
      sub_zdd(x,x);
    }
    if (y<0) {
      negy=1;
      y = -y;
    }
    if (negx^negy)
      sub_dld(x,1-y,x);
    div_dud(wam, x,y,z);
    if (negx^negy)
      sub_dld(x,y-1,x);
    if (negx)
      sub_zdd(x,x);
    if (negx^negy)
      sub_zdd(z,z);
  }
}

/* round towards +INF (ceiling) */
static void 
div_dldc(Wam wam, Dword x,SP_WORD y,Dword z)
{
  if (y==1) {
    *z = *x;
  } else {
    int negx=0, negy=0;
    if (cmp_dltz(x)) {
      negx=1;
      sub_zdd(x,x);
    }
    if (y<0) {
      negy=1;
      y = -y;
    }
    if (negx==negy)
      sub_dld(x,1-y,x);
    div_dud(wam, x,y,z);
    if (negx==negy)
      sub_dld(x,y-1,x);
    if (negx)
      sub_zdd(x,x);
    if (negx^negy)
      sub_zdd(z,z);
  }
}

/* x is nonnegative! */
static SP_UWORD 
mod_duu_internal(Dword x,SP_UWORD y)
{
  struct dword dividend = *x;
  SP_UWORD r1, r2;

  if (y==1)
    return 0;

  {
    /* [PM] 4.3 avoid undefined overflow behavior when left-shifting signed integers. Clang sanitizer traps it. */
    SP_UWORD scaled_overflow_bit = (~(SP_UWORD)0)<<(WORDSIZE-1); /* [PM] 4.3 Was: ((SP_UWORD)(((SP_WORD)-1)<<(WORDSIZE-1))); */
    /* [PM] 4.3.3 GCC 6 now (correctly) chokes with "left shift of negative value" so we can not even have the assert here */
    /* [PM] 4.3.3 Was: COMPILE_TIME_ASSERT( ((~(SP_UWORD)0)<<(WORDSIZE-1)) == ((SP_UWORD)(((SP_WORD)-1)<<(WORDSIZE-1)))); */

    /* note: dividend is two times too small */
    r1 = (scaled_overflow_bit%y)<<1;
  }

  if (r1>=y) {
    r1 -= y;
  }
  r2 = dividend.lsw.asuword%y;
  while (dividend.msw.asuword>0) {
    mul_uud(r1,dividend.msw.asuword,&dividend);
    add_dud(&dividend,r2,&dividend);
    r2 = dividend.lsw.asuword%y;
  }
  return r2;
}

/* see adjust_bounds_gcd for x<0 case! */
static SP_UWORD 
mod_duu(Dword x,SP_UWORD y)
{
  SP_UWORD m;

  if (cmp_dgez(x))
    return mod_duu_internal(x,y);
  sub_zdd(x,x);
  m = mod_duu_internal(x,y);
  sub_zdd(x,x);
  return (m==0 ? 0 : y-m);	/* modulo, not remainder */
}

static SP_WORD 
mod_dll(Dword x,SP_WORD y)
{
  return mod_duu(x,(y >= 0 ? y : -y));
}

typedef int TERM;

struct trail_item {
  TERM elt;
  int depth;
};

#define TRAIL(E,D)				\
{						\
  if (pdata->ttop >= pdata->tend)		\
    trail_expand(wam, pdata);			\
  pdata->ttop->elt = (E);			\
  pdata->ttop->depth = DEPTH(E);		\
  pdata->ttop++;				\
  DEPTH(E) = (D);				\
}

struct linear_data {
  void (SPCDECL *destructor)(void *);
  DAEMON_RC (SPCDECL *daemon)(Wam,void *,SP_globref,TAGGED*); /* (wam,handle,attr_ref,global) */
  SPEnv *spenv;

  SP_globref refbase;
  int op;			/* 1(#=<), 2(#>=), 3(#=), 4(#\=) */
  int depth;			/* choicept depth */
  int nonground;		/* maintained incrementally */
  int nvars;			/* #terms */
  int overflow;			/* set by the daemon */
  SP_BOOL gcd_due;		/* whether gcd check is due */
  struct dword bige_mem;
  struct dword bigf_mem;
  Dword bige;
  Dword bigf;
  TERM *cand;			/* temporary buffer */
  TERM *heap;
  int *vheap;
  TERM *target;
  int *tloc;
  Dvar dvar;
  struct {
    Dword cmin;			/* min(ai*xi) */
    Dword cmax;			/* max(ai*xi) */
    Dword interval;		/* cmax-cmin */
    SP_integer *coeff;		/* ai */
    SP_integer *abscoeff;	/* |ai| */
    int *depth;			/* depth at which I was modified */
  } term;
  struct trail_item *trail;
  struct trail_item *ttop;
  struct trail_item *tend;
};

  /* Maintain:
     bigf = rhs - sum{min(a_i x_i)}
     bige = sum{max(a_i x_i)} - rhs

     I_i = max(a_i x_i) - min(a_i x_i)
  */

#define SV(I)   (pdata->target[I])
#define VS(I)   (pdata->tloc[I])
#define COEFF(t) (pdata->term.coeff[t])
#define ABSCOEFF(t) (pdata->term.abscoeff[t])
#define DEPTH(t) (pdata->term.depth[t])
#define DVAR(t) (pdata->dvar+(t))
#define RefAttr(T) (pdata->refbase + 2*(T))
#define RefVar(T) (pdata->refbase + 2*(T) + 1)
#define CMIN(t) (pdata->term.cmin+(t))
#define CMAX(t) (pdata->term.cmax+(t))
#define INTERVAL(t) (pdata->term.interval+(t))

static void SPCDECL linear_destructor(void *pdata_v)
{
  struct linear_data *pdata = (struct linear_data *)pdata_v;
  FD_SETUP_SPENV(pdata->spenv)

  SP_free_globrefs(pdata->refbase,pdata->nvars<<1);
  SP_free(pdata->trail);
  SP_free(pdata);
}

#define SWAP(I,J)				\
{						\
  TERM vi = heap[I];				\
  TERM vj = heap[J];				\
  heap[I] = vj;					\
  heap[J] = vi;					\
  vheap[vi] = (J);				\
  vheap[vj] = (I);				\
}

static void 
heap_demote(struct linear_data *pdata, int i)
{
  TERM *heap = pdata->heap;
  int *vheap = pdata->vheap;
  Dword key = pdata->term.interval;
  
  for (;;) {
    int l = (i<<1)+1;
    int topmost = i;
    if (l<pdata->nvars && cmp_dgtd((key+heap[l]),(key+heap[topmost])))
      topmost = l;
    if (l+1<pdata->nvars && cmp_dgtd((key+heap[l+1]),(key+heap[topmost])))
      topmost = l+1;
    if (topmost==i)
      break;
    SWAP(i,topmost);
    i = topmost;
  }
}

static void
heap_promote(struct linear_data *pdata, int i)
{
  TERM *heap = pdata->heap;
  int *vheap = pdata->vheap;
  Dword key = pdata->term.interval;
  Dword key0 = key+heap[i];
  int dest = i;

  while (dest>0 && cmp_dltd(key+heap[(dest-1)>>1],key0)) {
    dest = (dest-1)>>1;
  }
  while (i>dest) {
    SWAP(i,(i-1)>>1);
    i = (i-1)>>1;
  }
}

static void trail_expand(Wam wam, struct linear_data *pdata)
{
  SP_integer oldsize = pdata->tend - pdata->trail;
  pdata->trail = SP_realloc(pdata->trail, 2*oldsize*sizeof(struct trail_item));
  pdata->ttop = pdata->trail + oldsize;
  pdata->tend = pdata->trail + 2*oldsize;
}

static SP_BOOL
not_fixpoint(Wam wam, struct linear_data *pdata)
{
  TERM elt;
  if (fd.fd_overflow) {
    pdata->overflow = fd.fd_overflow;
    return TRUE;
  } else if (pdata->nonground==0) {
    return TRUE;
  } else {
    switch (pdata->op) {
    case 1:
      elt = pdata->heap[0];
      return (cmp_dltd(pdata->bigf,INTERVAL(elt)) || cmp_dlez(pdata->bige));
    case 2:
      elt = pdata->heap[0];
      return (cmp_dltd(pdata->bige,INTERVAL(elt)) | cmp_dlez(pdata->bigf));
    case 3:
      elt = pdata->heap[0];
      return (pdata->gcd_due ||
	      cmp_dltd(pdata->bigf,INTERVAL(elt)) ||
	      cmp_dltd(pdata->bige,INTERVAL(elt)));
    case 4:
    default:
      return (pdata->nonground <= 1);
    }
  }
}

static DAEMON_RC SPCDECL 
linear_daemon(Wam wam,
	      void *vdata,
	      SP_globref attr_ref,
	      TAGGED *global)
{
  struct linear_data *pdata = (struct linear_data *)vdata;
  TERM elt = (int)((attr_ref - pdata->refbase)>>1);
  SP_BOOL buried;
  TAGGED tstate;
  struct trail_item *ttop;
  struct dword cminj_mem;
  Dword cminj = &cminj_mem;
  struct dword cmaxj_mem;
  Dword cmaxj = &cmaxj_mem;
  DAEMON_RC rc = DAEMON_FIX;

  tstate = fd_daemon_copy_state(wam, global,&buried);
  pdata->depth = GetSmall_int(CTagToArg(tstate,3)) + buried;
  TRAIL(elt,DEPTH(elt));
  ttop = pdata->trail + GetSmall(CTagToArg(tstate,4));
  while (pdata->ttop > ttop) {
    struct trail_item item = *--pdata->ttop;
    SP_globref ref;
    SP_integer c = COEFF(item.elt);
    struct dword key_mem;
    TAGGED tmin, tmax;

    key_mem = *INTERVAL(item.elt);
    DEPTH(item.elt) = item.depth;
    ref = pdata->refbase + 2*item.elt;
    REF_GET_BOUNDS(ref, tmin, tmax);
    if (c>0) {
      mul_ltd(c,tmin,cminj);
      mul_ltd(c,tmax,cmaxj);
    } else {
      mul_ltd(c,tmax,cminj);
      mul_ltd(c,tmin,cmaxj);
    }
    /* refresh state for item.elt, unless it was unchanged */
    if (cmp_deqd(cminj,CMIN(item.elt)) && cmp_deqd(cmaxj,CMAX(item.elt)))
      continue;
    add_ddd(wam, pdata->bigf,CMIN(item.elt),pdata->bigf);
    sub_ddd(wam, pdata->bige,CMAX(item.elt),pdata->bige);
    *CMIN(item.elt) = *cminj;
    *CMAX(item.elt) = *cmaxj;
    sub_ddd(wam, cmaxj,cminj,INTERVAL(item.elt));
    sub_ddd(wam, pdata->bigf,cminj,pdata->bigf);
    add_ddd(wam, pdata->bige,cmaxj,pdata->bige);
    if (cmp_deqz(&key_mem) && !cmp_deqd(cminj,cmaxj)) {
      int loc = VS(item.elt);
      TERM swap = SV(pdata->nonground);
      SV(loc) = swap;
      VS(swap) = loc;
      SV(pdata->nonground) = item.elt;
      VS(item.elt) = pdata->nonground++;
    } else if (cmp_deqd(cminj,cmaxj) && !cmp_deqz(&key_mem)) {
      int loc = VS(item.elt);
      TERM swap = SV(--pdata->nonground);
      SV(loc) = swap;
      VS(swap) = loc;
      SV(pdata->nonground) = item.elt;
      VS(item.elt) = pdata->nonground;
    }
    if (pdata->op < 4) {
      if (cmp_dltd(&key_mem,INTERVAL(item.elt))) {
	heap_promote(pdata,pdata->vheap[item.elt]);
      } else if (cmp_dgtd(&key_mem,INTERVAL(item.elt))) {
	heap_demote(pdata,pdata->vheap[item.elt]);
      }
    }
  }
  if (pdata->depth > DEPTH(elt)) {
    TRAIL(elt,pdata->depth);
  }
  CTagToArg(tstate,3) = MakeSmall0(pdata->depth);
  CTagToArg(tstate,4) = MakeSmall0(pdata->ttop - pdata->trail);
  if (not_fixpoint(wam,pdata))
    rc = DAEMON_NOFIX;
  return rc;
}

/* 0 - fail/overflow, 1 - pruned, 2 - gave up */
static int
scalar_product_le(Wam wam, struct linear_data *pdata, TERM elt)
{
  SP_integer c = COEFF(elt);
  Dvar dv = DVAR(elt);
  struct dword decr_mem;
  Dword decr = &decr_mem;
  struct dword cmax0_mem;
  Dword cmax0 = &cmax0_mem;
  *cmax0 = *CMAX(elt);

  /*
    Phase 2:

    For <=
    ******
    bigf>=0 is a necessary condition.
    bige<=0 is a sufficient condition.

    enforce:
    bigf >= I_i for all i

    rules:
    x_i <=  floor(F / a_i) + min(x_i)  if a_i>0
    x_i >= -floor(F /-a_i) + max(x_i)  if a_i<0

  */

  if (cmp_dltz(pdata->bigf))
    return 0;
  if (c>0) {
    struct dword tmp_mem;
    Dword tmp = &tmp_mem;
    struct dword ub_mem;
    Dword ub = &ub_mem;

    add_ddd(wam, pdata->bigf,CMIN(elt),tmp); /* [MC] 3.11.3 */
    div_dldf(wam, tmp,c,ub);
    if (issmall_d(ub)) {/* representable ub */
      if (dvar_fix_max_l(dv,ub->lsw.asword)<0)
	return 0;
      mul_ltd(c,dvar_max_t(dv),CMAX(elt));
    } else if (cmp_dgtz(ub)) { /* ub overflow - can't prune the variable */
      return 2;
    } else {			/* ub underflow - signal int overflow */
      fd.fd_overflow = 1;
      return 0;
    }
  } else {
    struct dword tmp_mem;
    Dword tmp = &tmp_mem;
    struct dword lb_mem;
    Dword lb = &lb_mem;

    add_ddd(wam, pdata->bigf,CMIN(elt),tmp); /* [MC] 3.11.3 */
    div_dldc(wam, tmp,c,lb);
    if (issmall_d(lb)) {/* representable lb */
      if (dvar_fix_min_l(dv,lb->lsw.asword)<0)
	return 0;
      mul_ltd(c,dvar_min_t(dv),CMAX(elt));
    } else if (cmp_dltz(lb)) {	/* lb underflow - can't prune the variable */
      return 2;
    } else {			/* lb overflow - signal int overflow */
      fd.fd_overflow = 2;
      return 0;
    }
  }
  if (pdata->depth > DEPTH(elt)) {
    TRAIL(elt,pdata->depth);
  }
  sub_ddd(wam, cmax0,CMAX(elt),decr);
  sub_ddd(wam, INTERVAL(elt),decr,INTERVAL(elt));
  sub_ddd(wam, pdata->bige,decr,pdata->bige);
  heap_demote(pdata,pdata->vheap[elt]);
  if (dvar_is_integer(dv)) {
    int loc = VS(elt);
    TERM swap = SV(--pdata->nonground);
    SV(loc) = swap;
    VS(swap) = loc;
    SV(pdata->nonground) = elt;
    VS(elt) = pdata->nonground;
    pdata->gcd_due = TRUE;
  }
  return 1;
}

/* 0 - fail/overflow, 1 - pruned, 2 - gave up */
static int
scalar_product_ge(Wam wam, struct linear_data *pdata, TERM elt)
{
  SP_integer c = COEFF(elt);
  Dvar dv = DVAR(elt);
  struct dword decr_mem;
  Dword decr = &decr_mem;
  struct dword cmin0_mem;
  Dword cmin0 = &cmin0_mem;
  *cmin0 = *CMIN(elt);
  
  /*
    Phase 2:

    For >=
    ******
    bige>=0 is a necessary condition.
    bigf<=0 is a sufficient condition.

    enforce:
    bige >= I_i for all i

    rules:
    x_i >= -floor(E / a_i) + max(x_i)  if a_i>0
    x_i <=  floor(E /-a_i) + min(x_i)  if a_i<0
  */

  if (cmp_dltz(pdata->bige))
    return 0;
  if (c>0) {
    struct dword tmp_mem;
    Dword tmp = &tmp_mem;
    struct dword lb_mem;
    Dword lb = &lb_mem;

    sub_ddd(wam, pdata->bige,CMAX(elt),tmp); /* [MC] 3.11.3 */
    div_dldc(wam, tmp,-c,lb);
    if (issmall_d(lb)) {/* representable lb */
      if (dvar_fix_min_l(dv,lb->lsw.asword)<0)
	return 0;
      mul_ltd(c,dvar_min_t(dv),CMIN(elt));
    } else if (cmp_dltz(lb)) {	/* lb underflow - can't prune the variable */
      return 2;
    } else {			/* lb overflow - signal int overflow */
      fd.fd_overflow = 2;
      return 0;
    }
  } else {
    struct dword tmp_mem;
    Dword tmp = &tmp_mem;
    struct dword ub_mem;
    Dword ub = &ub_mem;

    sub_ddd(wam, pdata->bige,CMAX(elt),tmp); /* [MC] 3.11.3 */
    div_dldf(wam, tmp,-c,ub);
    if (issmall_d(ub)) {/* representable ub */
      if (dvar_fix_max_l(dv,ub->lsw.asword)<0)
	return 0;
      mul_ltd(c,dvar_max_t(dv),CMIN(elt));
    } else if (cmp_dgtz(ub)) {	/* ub overflow - can't prune the variable */
      return 2;
    } else {			/* ub underflow - signal int overflow */
      fd.fd_overflow = 1;
      return 0;
    }
  }
  if (pdata->depth > DEPTH(elt)) {
    TRAIL(elt,pdata->depth);
  }
  sub_ddd(wam, CMIN(elt),cmin0,decr);
  sub_ddd(wam, INTERVAL(elt),decr,INTERVAL(elt));
  sub_ddd(wam, pdata->bigf,decr,pdata->bigf);
  heap_demote(pdata,pdata->vheap[elt]);
  if (dvar_is_integer(dv)) {
    int loc = VS(elt);
    TERM swap = SV(--pdata->nonground);
    SV(loc) = swap;
    VS(swap) = loc;
    SV(pdata->nonground) = elt;
    VS(elt) = pdata->nonground;
    pdata->gcd_due = TRUE;
  }
  return 1;
}

/* assert: c1>1 && c2>1 */
static INLINE SP_integer gcd(SP_integer c1,SP_integer c2)
{
  while (TRUE) {
    if (c1==c2) {
      return c1;
    } else if (c1<c2) {
      if (c1==0)
	return c2;
      else
	c2 %= c1;
    } else {
      if (c2==0)
	return c1;
      else
	c1 %= c2;
    }
  }
}

static SP_BOOL
gcd_check(Wam wam, struct linear_data *pdata)
{
  SP_integer g = ABSCOEFF(SV(0));
  struct dword rhs = *pdata->bigf;
  int i;

  for (i=1; i<pdata->nonground && g!=1; i++) {
    g = gcd(g,ABSCOEFF(SV(i)));
  }
  pdata->gcd_due = FALSE;
  if (g==1)
    return TRUE;
  if (!(g & (g-1))) {		/* g is a power of 2 -- fast mod */
    return !(rhs.lsw.asword & (g-1));
  }
  /* General case. If any CMIN is Inf, it will distort the mod operation. */
  for (i=0; i<pdata->nonground; i++) {
    add_ddd(wam, &rhs,CMIN(SV(i)),&rhs);
  }
  return (mod_duu(&rhs,g)==0);
}

/* collect all candidates subject to pruning -- pruning may not happen due to inf/sup */
static int
lin_gather(struct linear_data *pdata, int op, int h, int ncand)
{
  if (h < pdata->nonground) {
    TERM elt = pdata->heap[h];
    if ((op!=2 && cmp_dltd(pdata->bigf,INTERVAL(elt))) ||
	(op!=1 && cmp_dltd(pdata->bige,INTERVAL(elt)))) {
      pdata->cand[ncand++] = elt;
      ncand = lin_gather(pdata, op, (h<<1)+1, ncand);
      ncand = lin_gather(pdata, op, (h<<1)+2, ncand);
    }
  }
  return ncand;
}

SP_BOOL
fd_linear_filter(Wam wam, SP_term_ref State)
{
  TAGGED tvec, telt, handle, t1;
  SP_BOOL committed; /* TRUE if state can't be backtracked over */
  SP_integer state_stamp;
  int nonground0, nvars, i, j, k, ent = -1;
  SP_integer total_size;
  char *ptr;
  struct linear_data *pdata;

/*    X(0) = RefTerm(State0); */
  dvar_export_start(wam);
  RefTerm(State) = fd_unify_output_state(wam, &handle,&state_stamp,&committed);

  if (!IsVar(handle)) {		/* got [Flag | '$free'(Ptr)] */
    pdata = Pdata(struct linear_data,handle);
  } else {			/* build persistent state */
    DerefArg(tvec,X(0),1);	/* get CX0 */
    nvars = fd_list_length(tvec);	/* count terms */
    total_size = 
      nvars*sizeof(struct dvar) +
      3*nvars*sizeof(struct dword) +
      2*nvars*sizeof(SP_integer) +
      3*nvars*sizeof(TERM) +
      3*nvars*sizeof(int);
  
    pdata = Palloc(struct linear_data, total_size, handle);
    pdata->bige = &pdata->bige_mem;
    pdata->bigf = &pdata->bigf_mem;
    ptr = (char *)(pdata+1);
    pdata->dvar = (Dvar)ptr;
    ptr += nvars*sizeof(struct dvar);
    pdata->term.cmin = (Dword )ptr;
    ptr += nvars*sizeof(struct dword);
    pdata->term.cmax = (Dword )ptr;
    ptr += nvars*sizeof(struct dword);
    pdata->term.interval = (Dword )ptr;
    ptr += nvars*sizeof(struct dword);
    pdata->term.coeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->term.abscoeff = (SP_integer *)ptr;
    ptr += nvars*sizeof(SP_integer);
    pdata->target = (TERM *)ptr;
    ptr += nvars*sizeof(TERM);
    pdata->cand = (TERM *)ptr;
    ptr += nvars*sizeof(TERM);
    pdata->heap = (TERM *)ptr;
    ptr += nvars*sizeof(TERM);
    pdata->vheap = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->tloc = (int *)ptr;
    ptr += nvars*sizeof(int);
    pdata->term.depth = (int *)ptr;
    ptr += nvars*sizeof(int);
    SP_ASSERT(ptr == (char *)(pdata+1)+total_size);
    FD_STORE_SPENV(pdata->spenv);
    pdata->trail = SP_malloc(nvars*sizeof(struct trail_item));
    pdata->ttop = pdata->trail;
    pdata->tend = pdata->trail+nvars;
    pdata->depth = 0;
    pdata->overflow = 0;
    pdata->refbase = SP_alloc_globrefs(nvars<<1);
    DerefArg(telt,X(0),2);
    pdata->op = GetSmall(telt) & 0x7;
    pdata->destructor = linear_destructor;
    pdata->daemon = linear_daemon;
    pdata->nvars = nvars;
    DerefArg(tvec,X(0),1);	/* get CX0 */
    DerefArg(telt,X(0),3);	/* get RHS */
    add_ztd(telt,pdata->bigf);
    sub_zdd(pdata->bigf,pdata->bige);
				/* xfer all info to the struct linear_terms */
    for (i=0; i<nvars; i++) {
      TERM elt = i;
      SP_integer c;
      
      DerefCar(telt,tvec);
      DerefCdr(tvec,tvec);
      DerefArg(t1,telt,1);
      COEFF(elt) = c = GetSmall(t1);
      ABSCOEFF(i) = (c>=0 ? c : -c);
      DEPTH(elt) = 0;
      fd_get_var_and_attr(telt+WD(1),RefAttr(elt));
    }
    CTagToArg(X(0),1) = atom_nil; /* [MC] 3.12: free for GC */
    for (i=0; i<nvars; i++) {
      TERM elt = i;
      Dvar dv = DVAR(elt);
      TAGGED functor;
      dvar_init(dv, RefAttr(elt), RefVar(elt));
      switch (pdata->op) {
      case 4:
	functor = fd.functor_val;
	break;
      default:
	functor = fd.functor_minmax;
	break;
      }
      dvar_attach_daemon(wam, dv, pdata, X(1), functor);
    }
    /*pasted*/
    for (i=0, j=0, k=nvars; i<nvars; i++) {
      TERM elt = i;
      SP_WORD c = COEFF(elt);
      Dvar dv = DVAR(elt);

      if (c>0) {
	mul_ltd(c,dvar_min_t(dv),CMIN(elt));
	mul_ltd(c,dvar_max_t(dv),CMAX(elt));
      } else {
	mul_ltd(c,dvar_max_t(dv),CMIN(elt));
	mul_ltd(c,dvar_min_t(dv),CMAX(elt));
      }
      sub_ddd(wam, CMAX(elt),CMIN(elt),INTERVAL(elt));
      if (pdata->op<4) {
	pdata->heap[i] = elt;
	pdata->vheap[elt] = i;
      }
      sub_ddd(wam, pdata->bigf,CMIN(elt),pdata->bigf);
      add_ddd(wam, pdata->bige,CMAX(elt),pdata->bige);
      if (dvar_is_integer(dv)) {
	VS(elt) = --k;
	SV(k) = elt;
      } else {
	VS(elt) = j;
	SV(j++) = elt;
      }
    }
    pdata->nonground = j;
    if (pdata->op<4)
      for (i=(nvars-2)>>1; i>=0; i--)
	heap_demote(pdata,i);
    /*pasted*/
  }
  
				/* RESUME HERE */

  fd.fd_overflow = pdata->overflow;
  pdata->gcd_due = TRUE;
  nonground0 = pdata->nonground;
  for (i=0; i<nonground0; i++) {
    TERM elt = SV(i);
    dvar_refresh(DVAR(elt));
  }

  /* [4.4] Two-phase scheme: (1) find all candidates for pruning. (2) prune them. */

  if (pdata->op==4) {		/* #\= */
    if (pdata->nonground==0 && cmp_deqz(pdata->bigf)) {
      goto ret;
    } else if (pdata->nonground==1) {
      TERM elt = SV(0);
      Dvar dv = DVAR(elt);
      struct dword rhs;		/* [MC] SPRM 13689: if CMIN(elt)==Inf,
				   then (mod_dll(pdata->bigf,COEFF(elt))==0)
				   doesn't imply (mod_dll(&rhs,COEFF(elt))==0)
				 */
      add_ddd(wam, pdata->bigf,CMIN(elt),&rhs); 
      if (mod_dll(&rhs,COEFF(elt))==0) { /* RHS a multiple of coefficient */
	dvar_prune_value_l(dv,div_dll(wam, &rhs,COEFF(elt)));
      }
    }
  } else if (pdata->nonground>0) {
    int ncand = lin_gather(pdata, pdata->op, 0, 0);
    for (i=0; i<ncand; i++) {
      TERM elt = pdata->cand[i];
      switch (pdata->op) {
      case 1:			/* #=< */
	if (!scalar_product_le(wam, pdata,elt))
	  goto ret;
	break;
      case 2:			/* #>= */
	if (!scalar_product_ge(wam, pdata,elt))
	  goto ret;
	break;
      case 3:			/* #= */
	if (pdata->gcd_due && !gcd_check(wam, pdata))
	  goto ret;
	if (cmp_dltd(pdata->bigf,INTERVAL(elt))) {
	  if (!scalar_product_le(wam, pdata,elt))
	    goto ret;
	}
	if (cmp_dltd(pdata->bige,INTERVAL(elt))) {
	  if (!scalar_product_ge(wam, pdata,elt)) 
	    goto ret;
	}
      }
    }
  }
  switch (pdata->op) {
  case 1:
    ent = (cmp_dltz(pdata->bigf) ? -1 : cmp_dlez(pdata->bige));
    break;
  case 2:
    ent = (cmp_dltz(pdata->bige) ? -1 : cmp_dlez(pdata->bigf));
    break;
  case 3:
    ent = (cmp_dltz(pdata->bigf) ? -1 :
	   cmp_dltz(pdata->bige) ? -1 : 
	   cmp_dgtz(pdata->bigf) ? 0 :
	   cmp_dgtz(pdata->bige) ? 0 : 1);
    break;
  case 4:
    ent = (pdata->nonground<=1);
    break;
  }
  if (!ent && not_fixpoint(wam,pdata))
    fd_not_fixpoint(wam);
  for (i=0; i<nonground0; i++) {
    TERM elt = SV(i);
    dvar_export(DVAR(elt));
  }
  CTagToArg(X(0),3) = MakeSmall0(pdata->depth);
  CTagToArg(X(0),4) = MakeSmall0(pdata->ttop - pdata->trail);
 ret:
  return ent;
}
