/* Copyright (C) 1993, 2012 Swedish Institute of Computer Science */

#if !FORCE_BUILD && SICSTUS_RELEASE_BUILD && SICSTUS_VERSION > 40203
#error "[PM] 4.2.3 Add error handling (conside reimplementing a new serialization format based on copy_term.c"
#endif  /* SICSTUS_RELEASE_BUILD */

/* Fast term I/O */

/* [PM] 3.9.1 including stdlib.h before string.h avoids 'warning:
   conflicting types for built-in function `memcmp'" on HP-UX 11 with
   gcc 2.95.2. I do not know why.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "fastrw.h"
#include "sicstus/config.h"     /* [PM] 3.10.2 HAVE_SNPRINTF, HAVE__SNPRINTF ([PM] 4.2.1 this also defines SICSTUS_DBG) */
#include "fastrw_glue.h"

#define Version 'D'
#define Pref_Int 'I'
#define Pref_Float 'F'
#define Pref_Atom 'A'
#define Pref_Compound 'S'
#define Pref_Variable '_'
#define Pref_List '['
#define Pref_Nil ']'
#define Pref_Ascii_List '"'


#if SICSTUS_DBG && 0
#define IO_CHECK_DBG do {fprintf(stderr, "%s:%d IO ERROR\n", __FILE__, (int)__LINE__);fflush(stderr);} while(0)
#define DBG_TRACE do {fprintf(stderr, "%s:%d DBG TRACE\n", __FILE__, (int)__LINE__);fflush(stderr);} while(0)
#endif  /* SICSTUS_DBG */

#ifndef IO_CHECK_DBG
#define IO_CHECK_DBG            /* empty */
#endif  /* IO_CHECK_DBG */
#ifndef DBG_TRACE
#define DBG_TRACE               /* empty */
#endif  /* DBG_TRACE */

#define IO_CHECK(EXPR) do { if ((EXPR) == -1) { IO_CHECK_DBG; goto io_error; } } while(0)

#if !FORCE_BUILD && SICSTUS_RELEASE_BUILD && SICSTUS_VERSION > 40401 /* [PM] 4.3.2 postpone. */
#error "[PM] 4.0 This file should use SP_{put,get} byte directly (and use a byte_device instead of local.{read,write}_buffer"
#endif  /* !FORCE_BUILD */

#if !FORCE_BUILD && SICSTUS_RELEASE_BUILD && SICSTUS_VERSION > 40401 /* [PM] 4.3.2 postpone */
#error "[PM] 4.2.3 Nothing should use int"
#endif  /* !FORCE_BUILD */

struct frw_buffer {
  char *chars;
  int index;
  int size;
};

struct fastrw_state {
  char *frw_buf;
  int frw_buf_size;
  SP_atom frw_nil;
  SP_atom frw_var;
  SP_atom frw_period;
  int var_count;
  struct frw_buffer write_buffer;
  struct frw_buffer read_buffer;
  /* recursion stacks */
  /* termref-2, termref-1 are temporary */
  /* [termref,varbase) are for the recursion */
  /* [varref,freeref) hold variables */
  XP_term termref;
  XP_term varref;
  XP_term freeref;
  int tos;
  int *argno;
}; 

#if MULTI_SP_AWARE

/* [PM] 3.9b4 ensures local.foo works. Also avoids need for SP_CONTEXT_SWITCH_HOOK. */
#define local (*(struct fastrw_state *)*SP_foreign_stash())

#else  /* !MULTI_SP_AWARE */

static struct fastrw_state local;

#endif /* !MULTI_SP_aware */

#define RESET_INDIRECT_STACK

static int
frw_getc(SPAPI_ARG_PROTO_DECL
         XP_stream *stream)
{
  spio_t_error_code code = SP_get_byte(stream);
  if (SPIO_FAILED(code)) return -1;
  return (int)code;
}

/* This is only called with c == -1 (FIXME: which may happen for other errors than EOF) */
static void
frw_ungetc(SPAPI_ARG_PROTO_DECL
           XP_stream *stream, int c)
{
  SP_unget_byte(stream, c);
}

/* frw_put_string(string, stream)
   writes 'string' onto 'stream' (which defaults to local.write_buffer)

   [PM] 4.0 return 0 on success, -1 on error (but should be changed to return spio_t_error_code)
*/
static int
frw_put_string(SPAPI_ARG_PROTO_DECL
	       char const *string, XP_stream *stream)
{
  char c;

  if (stream)
    {
      do
	{
	  c = *string++;
	  IO_CHECK(XP_putc(stream,c));
	}
      while (c);
    }
  else
    {
      int index = local.write_buffer.index;
      size_t len = strlen(string);
      SP_ASSERT(len < INT_MAX);
      {
	int nbytes = (int)(len+1);
      
	while (index+nbytes > local.write_buffer.size)
	  {
            void *tmp;
	    LAZY_NULL_CHECK(tmp = Realloc(local.write_buffer.chars,
                                          local.write_buffer.size,
                                          local.write_buffer.size<<1));
            local.write_buffer.chars = (char *)tmp;
	    local.write_buffer.size <<= 1;
	  }
	memcpy(&local.write_buffer.chars[index], string, nbytes);
	local.write_buffer.index += nbytes;
      }
    }
  return 0;
 io_error:
  return -1;
}

/* frw_get_string(string, stream)
   copies a string to 'string' from 'stream' (which defaults to local.read_buffer)
   string == local.frw_buf
*/
static int 
frw_get_string(SPAPI_ARG_PROTO_DECL 
	       char *string,
	       XP_stream *stream)
{
  int c=0;
  
  if (stream)
    {
      char *frw_buf_end = local.frw_buf+local.frw_buf_size;
      do
        {
          /* [PM] 3.10.2 Prevent buffer-overrun, frw_buf may be full on entry. */
          if (string==frw_buf_end) /* a.k.a ! (string < frw_buf_end) */
            {
              void *tmp;
              LAZY_NULL_CHECK(tmp = Realloc(local.frw_buf,
                                            local.frw_buf_size,
                                            local.frw_buf_size<<1));
              local.frw_buf = (char *)tmp;
              string = local.frw_buf + local.frw_buf_size;
              local.frw_buf_size <<= 1;
              frw_buf_end = local.frw_buf+local.frw_buf_size;
            }

          c = XP_getc(SPAPI_ARG stream);
          if (c >= 0)
            {
              *string++ = (char)c;
            }
        }
      while (c>0);
    }
  else
    {
      char *src = &local.read_buffer.chars[local.read_buffer.index];
      size_t len = strlen(src);
      SP_ASSERT(len < INT_MAX);
      {
	int nbytes = (int)(len+1);
      
	while (string+nbytes > local.frw_buf+local.frw_buf_size)
	  {
            void *tmp;
	    LAZY_NULL_CHECK(tmp = Realloc(local.frw_buf,
                                          local.frw_buf_size,
                                          local.frw_buf_size<<1));
            string = local.frw_buf = (char *)tmp;
	    local.frw_buf_size <<= 1;
	  }
	memcpy(string,src,nbytes);
	local.read_buffer.index += nbytes;
      }
    }
  return c;
}

/* frw_put_char(c, stream)
   writes 'c' onto 'stream' (which defaults to local.write_buffer)
   return 0 on succes, -1 on error.
*/
static int 
frw_put_char(SPAPI_ARG_PROTO_DECL 
	     int c,
	     XP_stream *stream)
{
  if (stream)
    {
      DBG_TRACE;

      IO_CHECK(XP_putc(stream,(char)c));

      DBG_TRACE;
      
    }
  else
    {
      int index = local.write_buffer.index;

      DBG_TRACE;
      
      if (index+1 > local.write_buffer.size)
        {
          void *tmp;
          LAZY_NULL_CHECK(tmp = Realloc(local.write_buffer.chars,
                                        local.write_buffer.size,
                                        local.write_buffer.size<<1));
          local.write_buffer.chars = (char *) tmp;
          local.write_buffer.size <<= 1;
        }
      *(unsigned char *)(local.write_buffer.chars+local.write_buffer.index++) = (unsigned char)c;
      DBG_TRACE;
    }
      DBG_TRACE;
  return 0;
 io_error:
      DBG_TRACE;
  return -1;
}

/* frw_get_char(stream)
   gets a character from 'stream' (which defaults to local.read_buffer)

   Return -1 on error
*/
static int 
frw_get_char(SPAPI_ARG_PROTO_DECL 
	     XP_stream *stream)
{
  if (stream)
    return XP_getc(SPAPI_ARG stream);
  else
    return *(unsigned char *)(local.read_buffer.chars+local.read_buffer.index++);
}

/* frw_unget_char(stream)
   ungets a character to 'stream' (which defaults to local.read_buffer)
*/
static void
frw_unget_char(SPAPI_ARG_PROTO_DECL 
	       XP_stream *stream,
	       int c)
{
  if (stream)
    XP_ungetc(SPAPI_ARG stream,c);
  else {
    int ix = --local.read_buffer.index;
    *(unsigned char *)(local.read_buffer.chars+ix) = (unsigned char)c;
  }
}

static void
frw_push(SPAPI_ARG_PROTO_DECL
	 XP_term term,
	 int argno)
{
  int tos = local.tos;
  int size = local.varref - local.termref;
  size_t lsize = size*sizeof(int);

  if (tos==size) {
    void *tmp;
    XP_term r = XP_new_term_refs(size);
    for (r=local.freeref-1; r>=local.varref ; r--)
      XP_put_term(r+size,r);
    local.varref += size;
    local.freeref += size;
    LAZY_NULL_CHECK(tmp = Realloc(local.argno, lsize, lsize<<1));
    local.argno = (int *) tmp;
  }
  
  XP_put_term(local.termref+tos,term);
  local.argno[tos] = argno;
  local.tos++;
}

static int 
frw_read_term(SPAPI_ARG_PROTO_DECL 
	      XP_stream *stream,
	      XP_term term)
{
  int tos;
  XP_term t1 = local.termref-2;
  XP_term t2 = local.termref-1;
 
start:
  switch (frw_get_char(SPAPI_ARG stream)) {

  case -1:    /* [PM] 3.5 do not use QP_ERROR or SP_ERROR, instead
                 ensure -1 is returned from frw_get_char() on EOF */
    return -1;
  case Pref_Variable: {		/* variable */
    int c;
    int varno = 0;
    XP_term t3;
    
    c = frw_get_char(SPAPI_ARG stream);
    while (c >= '0' && c <= '9') {
      varno = 10*varno + c - '0';
      c = frw_get_char(SPAPI_ARG stream);
    }
    if (c!=0)
      return (c<0 ? -1 : -3);
    if (local.varref+varno >= local.freeref) {
      if (local.varref+varno > local.freeref)
	return -3;
      XP_init_term(t3);
      if (local.freeref != t3)
	return -4;
      local.freeref++;
      XP_put_term(t3,term);
    } else {
      XP_unify(term,local.varref+varno);
    }
    break;
  }

  case Pref_Nil:		/* the atom [] */
    XP_put_atom(t1,local.frw_nil);
    XP_unify(term, t1);
    break;

  case Pref_Atom:		/* some other atom */
    if (frw_get_string(SPAPI_ARG local.frw_buf, stream)<0)
      return -1;
    XP_put_string(t1, local.frw_buf);
    XP_unify(term, t1);
    break;

  case Pref_Ascii_List: {	/* list of character codes */
    int c;

    while ((c=frw_get_char(SPAPI_ARG stream))>0) {
      XP_put_list(t1);
      XP_unify(term, t1);
      XP_get_arg(1, term, t2);
      XP_put_integer(t1, c);
      XP_unify(t1, t2);
      XP_get_arg(2, term, term);
    }
    if (c<0)
      return -1;
    goto start;
  }

  case Pref_List:
    XP_put_list(t1);
    XP_unify(term, t1);
    frw_push(SPAPI_ARG term, 2);
    XP_get_arg(1, term, term);
    goto start;

  case Pref_Compound: {		/* some other compound term */
    int arity;

    if (frw_get_string(SPAPI_ARG local.frw_buf, stream)<0)
      return -1;
    arity = frw_get_char(SPAPI_ARG stream);
    if (arity<=0)
      return (arity<0 ? -1 : -3);
    XP_put_functor(t1, XP_atom_from_string(local.frw_buf), arity);
    XP_unify(term, t1);
    for (; arity>1; arity--)
      frw_push(SPAPI_ARG term, arity);
    XP_get_arg(1, term, term);
    goto start;
  }

  case Pref_Int:		/* integer */
    if (frw_get_string(SPAPI_ARG local.frw_buf, stream)<0)
      return -1;
    if (!XP_put_integer_chars(t1, local.frw_buf))
      return -3;
    XP_unify(term, t1);
    break;

  case Pref_Float:		/* float */
    if (frw_get_string(SPAPI_ARG local.frw_buf, stream)<0)
      return -1;
    if (!XP_put_float_chars(t1, local.frw_buf))
      return -3;
    XP_unify(term, t1);
    break;

  default:
    return -3;
  }
  tos = --local.tos;
  if (tos>=0) {
    int argno = local.argno[tos];
    XP_put_term(term,local.termref+tos);
    XP_get_arg(argno, term, term);
    goto start;
  }
  return 0;
}
  

static int frw_write_term(SPAPI_ARG_PROTO_DECL 
			  XP_term term,
			  XP_stream *stream)
{
  char const *s;
  SP_atom atm;
  int arity;
  int in_ascii_list = 0;
  int tos;
  XP_term ref1 = local.termref-2;
  
 start:
  switch (XP_term_type(term)) {
  case XP_TYPE_ATOM:		/* atom */
    if (in_ascii_list) {
      IO_CHECK(frw_put_char(SPAPI_ARG 0, stream));
      in_ascii_list = 0;
    }
    XP_get_atom(term, atm);
    if (atm==local.frw_nil)	/* the atom [] */
      {
        IO_CHECK(frw_put_char(SPAPI_ARG Pref_Nil, stream));
      }
    else {			/* some other atom */
      IO_CHECK(frw_put_char(SPAPI_ARG Pref_Atom, stream));
      XP_get_string(term, s);
      IO_CHECK(frw_put_string(SPAPI_ARG s, stream));
    }
    break;

  case XP_TYPE_COMPOUND:	/* compound term */
    XP_get_functor(term, atm, arity);
    if (arity==1 && atm==local.frw_var) { /* variable */
      XP_get_arg(1, term, ref1);
      XP_get_integer_chars(ref1, s);
      if (in_ascii_list) {
	IO_CHECK(frw_put_char(SPAPI_ARG 0, stream));
        in_ascii_list = 0;
      }
      IO_CHECK(frw_put_char(SPAPI_ARG Pref_Variable, stream));
      IO_CHECK(frw_put_string(SPAPI_ARG s, stream));
    } else if (arity==2 && atm==local.frw_period) {
      SP_integer head;

      XP_get_arg(1, term, ref1);
      if (XP_term_type(ref1) == XP_TYPE_INTEGER &&
	  XP_get_integer(ref1, head) &&
	  0 < head && head <= 255) { /* list of character codes */
	if (!in_ascii_list) {
	  IO_CHECK(frw_put_char(SPAPI_ARG Pref_Ascii_List, stream));
	  in_ascii_list = 1;
	}
	IO_CHECK(frw_put_char(SPAPI_ARG ((int)head), stream));
	XP_get_arg(2, term, term);
	goto start;
      } else {			/* list of non-characters */
	if (in_ascii_list) {
	  IO_CHECK(frw_put_char(SPAPI_ARG 0, stream));
          in_ascii_list = 0;
        }

        IO_CHECK(frw_put_char(SPAPI_ARG Pref_List, stream));
	frw_push(SPAPI_ARG term, 2);
	XP_put_term(term,ref1);
	goto start;
      }
    } else {			/* non-list compound term */
      if (in_ascii_list) {
        IO_CHECK(frw_put_char(SPAPI_ARG 0, stream));
	in_ascii_list = 0;
      }
      IO_CHECK(frw_put_char(SPAPI_ARG Pref_Compound, stream));
      IO_CHECK(frw_put_string(SPAPI_ARG XP_string_from_atom(atm), stream));
      IO_CHECK(frw_put_char(SPAPI_ARG arity, stream));
      for (; arity>1; arity--)
	frw_push(SPAPI_ARG term, arity);
      XP_get_arg(1, term, term);
      goto start;
    }
    break;

  case XP_TYPE_INTEGER:		/* integer */
    if (in_ascii_list) {
      IO_CHECK(frw_put_char(SPAPI_ARG 0, stream));
      in_ascii_list = 0;
    }
    IO_CHECK(frw_put_char(SPAPI_ARG Pref_Int, stream));
    XP_get_integer_chars(term, s);
    IO_CHECK(frw_put_string(SPAPI_ARG s, stream));
    break;

  case XP_TYPE_FLOAT:		/* float */
    if (in_ascii_list) {
      IO_CHECK(frw_put_char(SPAPI_ARG 0, stream));
      in_ascii_list = 0;
    }
    IO_CHECK(frw_put_char(SPAPI_ARG Pref_Float, stream));
    XP_get_float_chars(term, s);
    IO_CHECK(frw_put_string(SPAPI_ARG s, stream));
    break;      
  }
  tos = --local.tos;
  if (tos>=0) {
    int argno = local.argno[tos];
    XP_put_term(term,local.termref+tos);
    XP_get_arg(argno, term, term);
    goto start;
  }
  return 0;

 io_error:
  return -1;
}



/* User def. streams.  Although this idea is the cleanest, we don't want to
   take the overhead of opening and closing a stream for every buffered
   read or write operation, so we do it in a somewhat dirtier way.
*/

void SPCDECL 
frw_init(SPAPI_ARG_PROTO_DECL int when)
{
  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

#if MULTI_SP_AWARE
  LAZY_NULL_CHECK((*SP_foreign_stash()) = (void*)Malloc(sizeof(struct fastrw_state)));
#endif/* MULTI_SP_AWARE */

  local.var_count = 0;
  XP_register_atom(local.frw_nil = XP_atom_from_string("[]"));
  XP_register_atom(local.frw_var = XP_atom_from_string("$VAR"));
  XP_register_atom(local.frw_period = XP_atom_from_string("."));
  LAZY_NULL_CHECK(local.frw_buf = (char *)Malloc(local.frw_buf_size = 512));
  local.write_buffer.chars = NULL;
  local.write_buffer.index = 0;
  local.write_buffer.size = 0;
  local.read_buffer.chars = NULL;
  local.read_buffer.index = 0;
  local.read_buffer.size = 0;
}

void SPCDECL 
frw_deinit(SPAPI_ARG_PROTO_DECL int when)
{
  (void)when;                   /* [PM] 3.9b5 avoid -Wunused */

  XP_unregister_atom(local.frw_nil);
  XP_unregister_atom(local.frw_var);
  XP_unregister_atom(local.frw_period);
  Free(local.frw_buf,local.frw_buf_size);
#if MULTI_SP_AWARE
  Free((void*)*SP_foreign_stash(), sizeof(struct fastrw_state));
  (*SP_foreign_stash()) = NULL; /* not needed */
#endif  /* MULTI_SP_AWARE */
}


void *SPCDECL 
plc_open_buf_write(SPAPI_ARG_PROTO_DECL0)
{
  register struct frw_buffer *buf = &local.write_buffer;

  if (!buf->size)
    {
      LAZY_NULL_CHECK(buf->chars = (char *)Malloc(INIT_BUFSIZE));
      buf->index = 0;
      buf->size = INIT_BUFSIZE;
    }
  buf->index = 0;
  
  return NULL;
}


void *SPCDECL 
plc_open_buf_read(SPAPI_ARG_PROTO_DECL SP_integer lsource_raw)
{
  register struct frw_buffer *buf = &local.read_buffer;

  buf->chars = (char *)lsource_raw;
  buf->size = -1;		/* unused */
  buf->index = 0;
  
  return NULL;
}

void SPCDECL 
plc_buffer_data(SPAPI_ARG_PROTO_DECL 
		void *s_raw,
		SP_integer *size,
		SP_integer *laddr)
{
  XP_stream *s = (XP_stream *)s_raw;
  register struct frw_buffer *buf = &local.write_buffer;

  (void)s;                      /* [PM] 3.9b5 avoid -Wunused */

  *size = buf->index;
  *laddr = (SP_integer)buf->chars;
}



/* Main functions. */


/* Return codes:
    0 - OK
   -1 - EOF or error during read
   -2 - wrong version
   -3 - malformed input
   -4 - internal error: termrefs out of sync
*/
SP_integer SPCDECL 
plc_fast_read(SPAPI_ARG_PROTO_DECL 
	      XP_TERM_REF_DECL(term),
	      void *stream_raw)
{
  XP_stream *stream = (XP_stream *)stream_raw;
  int magic, rc;
  MAKE_INDIRECT_TERM(term);

  magic = frw_get_char(SPAPI_ARG stream);
  if (magic == -1)
    rc = -1;
  else if (magic != Version)
    rc = -2;
  else {
    local.termref = XP_new_term_refs(3)+2;
    local.varref = local.termref+1;
    local.freeref = local.varref;
    local.tos = 0;
    LAZY_NULL_CHECK(local.argno = (int *)Malloc(sizeof(int)));
    rc = frw_read_term(SPAPI_ARG stream, term);
    Free(local.argno,(local.varref-local.termref)*sizeof(int));
  }
  if (rc == -1)
    frw_unget_char(SPAPI_ARG stream, -1);
  RESET_INDIRECT_STACK;
  return rc;
}

/* Return codes:
    0 - OK
   -1 - EOF or error during write
*/
SP_integer SPCDECL 
plc_fast_write(SPAPI_ARG_PROTO_DECL 
	       XP_TERM_REF_DECL(term),
	       void *stream_raw)

{
  int rc;
  XP_stream *stream = (XP_stream *)stream_raw;
  MAKE_INDIRECT_TERM(term);

  local.termref = XP_new_term_refs(3)+2;
  local.varref = local.termref+1;
  local.freeref = local.varref;
  local.tos = 0;
  LAZY_NULL_CHECK(local.argno = (int *)Malloc(sizeof(int)));

  IO_CHECK(frw_put_char(SPAPI_ARG Version, stream));


  IO_CHECK(frw_write_term(SPAPI_ARG term, stream));
  rc = 0;

 cleanup:
  Free(local.argno,(local.varref-local.termref)*sizeof(int));
  RESET_INDIRECT_STACK;
  return rc;

 io_error:
  rc = -1;
  goto cleanup;
}
