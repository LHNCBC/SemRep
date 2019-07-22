/* Copyright (C) 1993 Swedish Institute of Computer Science */

/*  Hash functions for terms, portable across Prolog systems.   */

#ifdef SICSTUS

#include <sicstus/sicstus.h>
#include <stdlib.h>             /* [PM] 3.9.1 !! See bdb.c */
#include <db.h>                 /* needed for types used in bdb_glue.h */
#include "bdb.h"                /* needed for types used in bdb_glue.h */
#include "bdb_glue.h"

#define XP_init_term(T)         (T) = SP_new_term_ref()
#define XP_term SP_term_ref
#define XP_term_type SP_term_type
#define XP_TYPE_VARIABLE SP_TYPE_VARIABLE
#define XP_TYPE_ATOM SP_TYPE_ATOM
#define XP_TYPE_COMPOUND SP_TYPE_COMPOUND
#define XP_TYPE_INTEGER SP_TYPE_INTEGER
#define XP_TYPE_FLOAT SP_TYPE_FLOAT
#define XP_get_string(TERM,STR) SP_get_string(TERM,&(STR))
#define XP_get_arg(I,TERM,ARG) SP_get_arg(I,TERM,ARG)
#define XP_get_float_codes(TERM,STR) SP_get_number_codes(TERM,&(STR))
#define XP_get_integer_codes(TERM,STR) SP_get_number_codes(TERM,&(STR))

#endif

#ifdef QUINTUS

#include <quintus/quintus.h>

#ifndef PROTOTYPE
#ifdef __STDC__
#define PROTOTYPE(argl) argl
#else
#define PROTOTYPE(ignore) ()
#endif
#endif

#define XP_init_term(T)         (T) = QP_new_term_ref()
#define XP_term QP_term_ref
#define XP_term_type QP_term_type
#define XP_TYPE_VARIABLE QP_VARIABLE
#define XP_TYPE_ATOM QP_ATOM
#define XP_TYPE_COMPOUND QP_COMPOUND
#define XP_TYPE_INTEGER QP_INTEGER
#define XP_TYPE_FLOAT QP_FLOAT
#define XP_get_string(TERM,STR) QP_get_string(TERM,&(STR))
#define XP_get_arg(I,TERM,ARG) QP_get_arg(I,TERM,ARG)
#define XP_get_float_codes(TERM,STR) QP_get_float_codes(TERM,&(STR))
#define XP_get_integer_codes(TERM,STR) QP_get_integer_codes(TERM,&(STR))

static void QP_get_string(t, s)
     QP_term_ref t;
     char **s;
{
  SP_uinteger qp_atom;
  
  QP_get_atom(t, &qp_atom);
  *s = QP_string_from_atom(qp_atom);
}

static void QP_get_integer_codes(t, s)
     QP_term_ref t;
     char **s;
{
  SP_integer l;
  
  QP_get_integer(t, &l);
#error "[PM] use snprintf"
  sprintf(*s=frw_buf, "%d", l);
}

static void QP_get_float_codes(t, s)
     QP_term_ref t;
     char **s;
{
  double d;
  
  QP_get_float(t, &d);
#error "[PM] use snprintf"
  sprintf(*s=frw_buf, "%.17g", d);
}
#endif

#define HASHADD(H,T) (H=((H<<5)+(H>>27))^T)

static unsigned short tab[] = {
  0x3760, 0xA26C, 0xC31A, 0x6623, 0x3D47, 0x5495, 0xD8F3, 0xAF81,
  0x455C, 0xD463, 0x43DD, 0xFA7A, 0x4641, 0xAF18, 0x7FF, 0x3626,
  0xA8F1, 0x42DF, 0x6EEC, 0xE1DA, 0x4C0F, 0x28D9, 0x7546, 0xC69C,
  0xE94D, 0xF796, 0x9D7F, 0x75E7, 0xE1F0, 0x2904, 0x294C, 0xB235,
  0x5C6A, 0x4DE, 0xE5EA, 0x19CC, 0x8DB4, 0xEEC1, 0x1B7F, 0xAC58,
  0x63F6, 0x8E9A, 0x186F, 0x4E8E, 0xAD0, 0x6C04, 0x7B20, 0x169,
  0xE5F9, 0x22FA, 0x7286, 0xF019, 0x86B3, 0x6C49, 0x8988, 0x38C1,
  0x9B3C, 0xDF5B, 0x6030, 0x356F, 0x5371, 0x7268, 0x61BB, 0xD0C,
  0x8F5E, 0xBBC1, 0x4876, 0xFAFA, 0x968B, 0x6894, 0x4566, 0x9DC9,
  0x4D31, 0x63A5, 0xD3D3, 0x8E98, 0xA231, 0x25BE, 0x5A62, 0xE33A,
  0xD7A2, 0x4555, 0x8E25, 0x3234, 0xD40E, 0xB410, 0x4F4E, 0x86A3,
  0xC2C9, 0xED06, 0xEB1F, 0xC66C, 0x8346, 0x7AFC, 0x797A, 0x3B7A,
  0xD902, 0xAB45, 0xBDB8, 0xD564, 0xEEA5, 0xDCE5, 0xDD4C, 0xDFB2,
  0x6ECA, 0xB59A, 0x3731, 0x3B7F, 0x475F, 0xCCCA, 0xF987, 0xEAD3,
  0xE226, 0x53FD, 0x4404, 0x6CD4, 0x1869, 0xF861, 0x8BC1, 0x4967,
  0x8967, 0x1657, 0x617E, 0xEDFA, 0x89C6, 0x4084, 0xD086, 0x930D,
  0x25EC, 0x3083, 0x374E, 0x50D2, 0xFE5C, 0x4249, 0xA601, 0x3761,
  0x9016, 0xB838, 0xD63D, 0x55DA, 0xCB34, 0xC1F, 0xA3D4, 0x60C1,
  0xAB4B, 0xD1A1, 0x302A, 0x2AF0, 0xAFED, 0x8D44, 0xA74E, 0xE7C,
  0x852A, 0x825B, 0xA9D5, 0xDD59, 0x2C56, 0x861C, 0x90B4, 0xEEB7,
  0x76E5, 0x1125, 0x9C02, 0xAE4A, 0xCE3C, 0xA234, 0xBEE7, 0x4354,
  0x2F2, 0xB351, 0x74FE, 0xFE4A, 0xCDA0, 0x5B6A, 0x1B92, 0x5805,
  0xE87F, 0x91A4, 0x533E, 0x7458, 0xF89, 0x5DC9, 0x27E3, 0x741C,
  0xD716, 0xC5FC, 0xC8F1, 0xE8A9, 0x1784, 0x747B, 0x983A, 0x14D3,
  0x3085, 0x741E, 0x905A, 0x7691, 0x2532, 0x9F07, 0x12D2, 0x5882,
  0xCC08, 0x2FC5, 0xE644, 0xB3E3, 0xAFCC, 0xD692, 0x9B31, 0x7AF3,
  0x3A4D, 0xB119, 0xE039, 0x57E0, 0x1C4A, 0xF00D, 0x3A0F, 0x7504,
  0x4AFD, 0x5C69, 0x235C, 0xA1F1, 0x16DA, 0xD972, 0x509F, 0xE69B,
  0x682E, 0x54B0, 0xFC0F, 0xC924, 0xDBD6, 0xC946, 0x9249, 0x3F35,
  0x9FAB, 0x7391, 0xB2A7, 0xE736, 0x8370, 0x34BB, 0x9ACC, 0xC6FD,
  0x636C, 0x437B, 0x38BD, 0x7A57, 0x8D27, 0xFE1A, 0xA742, 0xF690,
  0x9944, 0xB08D, 0xF227, 0x7FD8, 0x42B6, 0xEC83, 0xD27A, 0x4ED4};


/*
  Like SP term_hash/4, or QP hash_term/2, portable across both systems.
  The argument is a list of atomic items.
*/
SP_integer SPCDECL db_term_hash(XP_term term)
{
  register int i;
  register SP_uinteger code = 0;
  XP_term elt;
  char const *s;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  XP_init_term(elt);
  while (XP_term_type(term) == XP_TYPE_COMPOUND) {
    XP_get_arg(1, term, elt);
    switch (XP_term_type(elt)) {
    case XP_TYPE_ATOM:
      XP_get_string(elt, s);
      goto hash_codes;
    case XP_TYPE_INTEGER:
      XP_get_integer_codes(elt, s);
      goto hash_codes;  
    case XP_TYPE_FLOAT:
      XP_get_float_codes(elt, s);
    hash_codes:
      for (i=0; s[i]; i++)
        HASHADD(code, tab[*(unsigned char *)(s+i)]);
      break;
    }
    XP_get_arg(2, term, term);
  }
  return code & 0xffffffff;
}
