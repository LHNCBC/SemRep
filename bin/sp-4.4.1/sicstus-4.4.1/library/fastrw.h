/* Copyright (C) 1993, 2012 Swedish Institute of Computer Science */

/* Definitions and headers for fast term I/O. */

#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <stdlib.h>
#include <stdio.h>
#define INIT_BUFSIZE 256

#define XP_TERM_REF_DECL(t) XP_term t
#define MAKE_INDIRECT_TERM(t)
#define UNMAKE_INDIRECT_TERM(t)

#include <sicstus/sicstus.h>
#include "fastrw_glue.h"

#define Malloc(Size) SP_malloc(Size)
#define Realloc(Ptr,OldSize,NewSize) SP_realloc((void *)(Ptr),NewSize)
#define Free(Ptr,OldSize) SP_free((void *)(Ptr))

#define XP_new_term_refs(arity)  SP_new_term_refs(arity) /* [PD] 3.11.1 */
#define XP_init_term(T)         (T) = SP_new_term_ref()
#define XP_term SP_term_ref
#define XP_term_type SP_term_type

#define XP_stream SP_stream
#define XP_TYPE_VARIABLE SP_TYPE_VARIABLE
#define XP_TYPE_ATOM SP_TYPE_ATOM
#define XP_TYPE_COMPOUND SP_TYPE_COMPOUND
#define XP_TYPE_INTEGER SP_TYPE_INTEGER
#define XP_TYPE_FLOAT SP_TYPE_FLOAT
#define XP_atom_from_string(STR) SP_atom_from_string(STR)
#define XP_string_from_atom(ATM) SP_string_from_atom(ATM)

#define XP_put_term(DEST,SRC) SP_put_term((DEST),(SRC))
#define XP_put_atom(TERM,ATM) SP_put_atom((TERM),ATM)
#define XP_put_string(TERM,STR) SP_put_string((TERM),STR)
#define XP_put_list(TERM) SP_put_list(TERM)
#define XP_put_functor(TERM,ATM,ARITY) SP_put_functor(TERM,ATM,ARITY)
#define XP_put_integer(TERM,INT) SP_put_integer((TERM),INT)
#define XP_put_integer_chars(TERM,STR) SP_put_number_codes((TERM),STR)
#define XP_put_float_chars(TERM,STR) SP_put_number_codes((TERM),STR)
#define XP_get_atom(TERM,ATM) SP_get_atom(TERM,&(ATM))
#define XP_get_string(TERM,STR) SP_get_string(TERM,&(STR))
#define XP_get_functor(TERM,ATM,ARITY) SP_get_functor(TERM,&(ATM),&(ARITY))
#define XP_get_arg(I,TERM,ARG) SP_get_arg(I,TERM,(ARG))
#define XP_get_integer(TERM,INT) SP_get_integer(TERM,&(INT))
#define XP_get_integer_chars(TERM,STR) SP_get_number_codes(TERM,&(STR))
#define XP_get_float_chars(TERM,STR) SP_get_number_codes(TERM,&(STR))
#define XP_putc(STREAM,CHAR) ( SPIO_FAILED(SP_put_byte((STREAM),(CHAR))) ? -1 : 0 )
#define XP_getc frw_getc
#define XP_ungetc frw_ungetc
#define XP_unify(X,Y) SP_unify(X,Y)
#define XP_register_atom(X) (void)SP_register_atom(X)
#define XP_unregister_atom(X) (void)SP_unregister_atom(X)
