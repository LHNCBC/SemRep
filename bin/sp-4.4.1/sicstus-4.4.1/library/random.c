/*  File   : library.d/random.c
    Author : Richard A. O'Keefe
    Updated: 12/10/98
    Purpose: C-coded half of the "random" procedures.

    Adapted from shared code written by the same author; all changes
    Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

    This is an implementation of algorithm AS 183 from the journal
    "Applied Statistics", recoded in C. There is also a version in
    Prolog available to people who cannot use the Quintus Prolog
    foreign function interface.
*/

#include <sicstus/sicstus.h>
#include "random_glue.h"

struct random_state {
  SP_uinteger A;
  SP_uinteger B;
  SP_uinteger C;
  SP_uinteger S;
  double ZX;
  double ZY;
  double ZZ;
};

#if MULTI_SP_AWARE
/* [PM] 3.9b4 ensures local.foo works. Also avoids need for SP_CONTEXT_SWITCH_HOOK. */
#define local (*(struct random_state *)*SP_foreign_stash())
#else  /* !MULTI_SP_AWARE */
static struct random_state local;
#endif /* !MULTI_SP_AWARE */

void SPCDECL rand_init(SPAPI_ARG_PROTO_DECL 
		       int when)
{
  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

#if MULTI_SP_AWARE
  LAZY_NULL_CHECK((*SP_foreign_stash()) = (void*)SP_malloc(sizeof(struct random_state)));
#endif/* MULTI_SP_AWARE */

  local.A = 27134;
  local.B = 9213;
  local.C = 17773;
  local.S = 0x19551011;

  /* [PM] 4.0 These used to be static but floating point expressions are not constant with VS 8 (for good reasons) */
  local.ZX = 1.0/30269.0;
  local.ZY = 1.0/30307.0;
  local.ZZ = 1.0/30323.0;
}

void SPCDECL rand_deinit(SPAPI_ARG_PROTO_DECL 
			 int when)
{
  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

#if MULTI_SP_AWARE
  SP_free((void*)*SP_foreign_stash());
  (*SP_foreign_stash()) = NULL; /* not needed */
#endif
}


void SPCDECL QPRget(SPAPI_ARG_PROTO_DECL 
		    SP_integer *a, SP_integer *b, SP_integer *c, SP_integer *s)
{
  *a = local.A;
  *b = local.B;
  *c = local.C;
  *s = local.S;
}


SP_integer SPCDECL QPRput(SPAPI_ARG_PROTO_DECL 
		    SP_integer a, SP_integer b, SP_integer c, SP_integer s)
{
  if (a==0 || b==0 || c==0 || s==0 ||
      (SP_uinteger)(a) >= 30269 ||
      (SP_uinteger)(b) >= 30307 ||
      (SP_uinteger)(c) >= 30323)
    return 0;
  local.A = a;
  local.B = b;
  local.C = c;
  local.S = s & 0x3FFFFFFF;	/* modulo 1<<30 */
  return 1;
}


double SPCDECL QPRnxt(SPAPI_ARG_PROTO_DECL0)
{
  SP_uinteger X,Y,Z;
  double T;
  
  X = (local.A*171) % 30269;
  Y = (local.B*172) % 30307;
  Z = (local.C*170) % 30323;
  T = (((double)X)*local.ZX + ((double)Y)*local.ZY + ((double)Z)*local.ZZ);
  local.A = X;
  local.B = Y;
  local.C = Z;
  return T-(double)(SP_integer)T;
}

/* { */
/*   double T = (A = (A*171) % 30269)/30269.0 */
/*     + (B = (B*172) % 30307)/30307.0 */
/*     + (C = (C*170) % 30323)/30323.0; */
/*   return T-(int)T; */
/* } */


SP_integer SPCDECL QPRmyb(SPAPI_ARG_PROTO_DECL SP_integer P, SP_integer N)
{
  SP_uinteger X,Y,Z;
  double T;
  
  X = (local.A*171) % 30269;
  Y = (local.B*172) % 30307;
  Z = (local.C*170) % 30323;
  T = ((double)X)*local.ZX + ((double)Y)*local.ZY + ((double)Z)*local.ZZ;
  local.A = X;
  local.B = Y;
  local.C = Z;

  return (SP_uinteger)(P) > (SP_uinteger)(N) ? 0 : ((SP_integer)(T*(double)N))%N < P;
}


SP_integer SPCDECL QPRbit(SPAPI_ARG_PROTO_DECL0)
{
  SP_uinteger r = local.S;

  r += r;
  if (r & (1<<30)) {
    local.S = r^(1<<6 | 1<<2 | 1<<1 | 1);
    return 1;
  } else {
    local.S = r;
    return 0;
  }
}

