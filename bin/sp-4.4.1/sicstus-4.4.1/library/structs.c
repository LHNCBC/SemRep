#error "[PM] 4.2.1+ no longer used."

/*  File   : structs.c
    Author : Peter Schachte
    Updated: 06/01/94
    Purpose: to allow access to C/Pascal data structures from Prolog

    Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved

    This file provides access to memory locations to the structs
    package.  Actual functions supplied include the following:

	    _Sget_functor(pointer)
	    _Sget_atom(pointer, offset)
	    _Sget_string(pointer, offset)
	    _Sget_integer(pointer, offset)
	    _Sget_integer_32(pointer, offset)
	    _Sget_integer_16(pointer, offset)
	    _Sget_integer_8(pointer, offset)
	    _Sget_unsigned(pointer, offset, mask)
	    _Sget_unsigned_32(pointer, offset, mask)
	    _Sget_unsigned_16(pointer, offset)
	    _Sget_unsigned_8(pointer, offset)
	    _Sget_float(pointer, offset)
	    _Sget_float_32(pointer, offset)

	    _Sput_atom(pointer, offset, newvalue)
	    _Sput_string(pointer, offset, newvalue)
	    _Sput_integer(pointer, offset, newvalue)
	    _Sput_integer_32(pointer, offset, newvalue)
	    _Sput_integer_16(pointer, offset, newvalue)
	    _Sput_integer_8(pointer, offset, newvalue)
	    _Sput_unsigned(pointer, offset, newvalue)
	    _Sput_unsigned_32(pointer, offset, newvalue)
	    _Sput_unsigned_16(pointer, offset, newvalue)
	    _Sput_unsigned_8(pointer, offset, newvalue)
	    _Sput_float(pointer, offset, newvalue)
	    _Sput_float_32(pointer, offset, newvalue)

*/


typedef void opaque;

#include <sicstus/config.h>
#include <sicstus/sicstus.h>
#include <string.h>
#include <limits.h>             /* LONG_MIN */
#include "structs_glue.h"

#if 1                           /* [PM] 4.0.2 */
/* The idea is that converting a signed long X into a unsigned long U can be done with U is X - 2*SIGNMASK, see get_unsigned an */
# define SIGNMASK LONG_MIN
#else

/* [PM] 4.0.2 These constants overflows signed long so (win32)
   compiler complained that "unary minus operator applied to unsigned
   type, result still unsigned"
*/
#if LogSizeOfWord==3
# define SIGNMASK -0x8000000000000000L
#else
# define SIGNMASK -0x80000000L
#endif
#endif

SP_atom SPCDECL _Sget_functor(pointer)
char *pointer;
{
  return ((SP_atom *)(pointer))[-1];
}


SP_atom SPCDECL _Sget_atom(pointer, offset)
char *pointer;
SP_integer offset;
{
	return *(SP_atom *)(pointer+offset);
}


char SP_FLI_CONST * SPCDECL _Sget_string(char *pointer, SP_integer offset)
{
	return *(char **)(pointer+offset);
}


SP_integer SPCDECL _Sget_integer(pointer, offset)
char *pointer;
SP_integer offset;
{
	return *(SP_integer *)(pointer+offset);
}


SP_integer SPCDECL _Sget_integer_32(pointer, offset)
char *pointer;
SP_integer offset;
{
	return *(int *)(pointer+offset);
}


SP_integer SPCDECL _Sget_integer_16(pointer, offset)
char *pointer;
SP_integer offset;
{
	return *(short *)(pointer+offset);
}


SP_integer SPCDECL _Sget_integer_8(pointer, offset)
char *pointer;
SP_integer offset;
{
	return *(signed char *)(pointer+offset); /* char can be unsigned! */
}


SP_integer SPCDECL _Sget_unsigned(pointer, offset, mask)
char *pointer;
SP_integer offset;
SP_integer *mask;
{
        *mask = SIGNMASK;
	return *(SP_uinteger *)(pointer+offset);
}


SP_integer SPCDECL _Sget_unsigned_32(pointer, offset, mask)
char *pointer;
SP_integer offset;
SP_integer *mask;
{
        *mask = SIGNMASK;
	return *(unsigned int *)(pointer+offset);
}


SP_integer SPCDECL _Sget_unsigned_16(pointer, offset)
char *pointer;
SP_integer offset;
{
	return *(unsigned short *)(pointer+offset);
}


SP_integer SPCDECL _Sget_unsigned_8(pointer, offset)
char *pointer;
SP_integer offset;
{
	return *(unsigned char *)(pointer+offset);
}


double SPCDECL _Sget_float_32(pointer, offset)
char *pointer;
SP_integer offset;
{
	return *(float *)(pointer+offset);
}


double SPCDECL _Sget_float(pointer, offset)
char *pointer;
SP_integer offset;
{
	return *(double *)(pointer+offset);
}


void SPCDECL _Sput_atom(pointer, offset, newvalue)
char *pointer;
SP_integer offset;
SP_atom newvalue;
{
	/*
	 *  slots are initialized to zeros on 'new' - an impossible value
	 *  for an atom, so we can tell if we need to unregister the old atom
	 */
	SP_atom oldvalue = (*(SP_atom *)(pointer+offset));
	if (oldvalue != 0)
		SP_unregister_atom(oldvalue);

	SP_register_atom(newvalue);
	(*(SP_atom *)(pointer+offset)) = newvalue;
}


void SPCDECL _Sput_string(char *pointer, SP_integer offset, char SP_FLI_CONST *newvalue)
{
  /* [MC] 4.0 prevent newvalue from atom GC, at the cost of locking the atom forever */
  SP_register_atom(SP_atom_from_string(newvalue));
  /* [PM] 4.0 Kludge: cast away const to hide the fact that newvalue is a read-only atom name. */
  (*(char **)(pointer+offset)) = (char *) newvalue;
}

void SPCDECL _Sput_integer(char *pointer, SP_integer offset, SP_integer newvalue)
{
	(*(SP_integer *)(pointer+offset)) = newvalue;
}

void SPCDECL _Sput_integer_32(pointer, offset, newvalue)
char *pointer;
SP_integer offset;
SP_integer newvalue;
{
	(*(int *)(pointer+offset)) = newvalue;
}

void SPCDECL _Sput_integer_16(pointer, offset, newvalue)
char *pointer;
SP_integer offset;
SP_integer newvalue;
{
	(*(short *)(pointer+offset)) = (short)newvalue;
}

void SPCDECL _Sput_integer_8(pointer, offset, newvalue)
char *pointer;
SP_integer offset;
SP_integer newvalue;
{
	(*(char *)(pointer+offset)) = (char)newvalue;
}

/* the next four are in fact identical to the signed versions */
void SPCDECL _Sput_unsigned(pointer, offset, newvalue)
char *pointer;
SP_integer offset;
SP_integer newvalue;
{
	(*(SP_integer *)(pointer+offset)) = newvalue;
}

void SPCDECL _Sput_unsigned_32(pointer, offset, newvalue)
char *pointer;
SP_integer offset;
SP_integer newvalue;
{
	(*(int *)(pointer+offset)) = newvalue;
}

void SPCDECL _Sput_unsigned_16(pointer, offset, newvalue)
char *pointer;
SP_integer offset;
SP_integer newvalue;
{
	(*(short *)(pointer+offset)) = (short)newvalue;
}

void SPCDECL _Sput_unsigned_8(pointer, offset, newvalue)
char *pointer;
SP_integer offset;
SP_integer newvalue;
{
	(*(char *)(pointer+offset)) = (char)newvalue;
}

void SPCDECL _Sput_float_32(pointer, offset, newvalue)
char *pointer;
SP_integer offset;
double newvalue;
{
	(*(float *)(pointer+offset)) = (float)newvalue;
}

void SPCDECL _Sput_float(pointer, offset, newvalue)
char *pointer;
SP_integer offset;
double newvalue;
{
	(*(double *)(pointer+offset)) = newvalue;
}

void * SPCDECL _Scalloc(SP_integer count, SP_integer size)
{
  int csize = count*size;	/* alignment, rounding? */
  void *ptr;

  LAZY_NULL_CHECK(ptr = SP_malloc(csize));

  memset(ptr, 0, csize);
  return ptr;
}

void SPCDECL _Sfree(void *ptr)
{
  SP_free(ptr);
}

