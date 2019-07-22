/* Copyright(C) 1994-95, Swedish Institute of Computer Science */
#define SICSTUS_HIDDEN_API 1
#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <sicstus/sicstus.h>
#include <sicstus/config.h>
#define BARF SPIO_BARF
#define CHECK SPIO_CHECK
#define SPIO_BARF_LABEL barf
#define NULL_CHECK SPIO_NULL_CHECK
#define POSIX_CHECK SPIO_POSIX_CHECK
#define BARF_LAST_ERROR SPIO_BARF_LAST_ERROR
#define HANDLE_CHECK SPIO_HANDLE_CHECK
#define CHECK_LAST_ERROR SPIO_CHECK_LAST_ERROR

/* [PM] 4.0 see support.c */
#if defined(__GNUC__) && defined(__GLIBC__) && defined(__GLIBC_MINOR__)
#if (__GLIBC__ == 2 && __GLIBC_MINOR__ == 2) /* glibc 2.2 has this problem (RH 7.2) */
#undef strcpy
#endif /* glibc 2.2 */
#endif /* gcc && glibc */

#include "tcl.h"

#if HAVE__SNPRINTF && !HAVE_SNPRINTF /* Win32 */
/* #undef snprintf */
#define snprintf _snprintf
#endif

#if MULTI_SP_AWARE
#error "[PM] 4.0 This code is not multi-sp capable"
#endif  /* MULTI_SP_AWARE */

/*   --------------------------------------------------------------  */

/* Local function prototypes */


/*   --------------------------------------------------------------  */

void ptr_to_wrapper(SP_atom functor, void *ptr, SP_term_ref tTerm)
{
  SP_term_ref  ID = tTerm; /* ID = SP_new_term_ref(); */

  SP_put_address(ID, ptr);
  SP_cons_functor(tTerm, functor, 1, ID);
}

/*   --------------------------------------------------------------  */

void *wrapper_to_ptr(SP_atom functor, SP_term_ref t)
{
  void *ptr;
  SP_atom atm_name;
  int arity;
  SP_term_ref tmp = SP_new_term_ref();

  /* Is it a term '$TclInterp'(Address) ? */

  if (SP_get_functor(t, &atm_name, &arity)
      && (atm_name == functor)
      && arity == 1)
    {
      SP_get_arg(1, t, tmp);
      if (SP_get_address(tmp, &ptr))
	return ptr;
    }
  return NULL;
}

/*   --------------------------------------------------------------  */

/*
  [PM] 4.3 Copy message to local.err_msg[], with UTF8-aware truncation if necessary.
*/
static void save_error_message(char const *msg_UTF8)
{
  size_t const size = sizeof local.err_msg;
  size_t const len = strlen(msg_UTF8);
        
  if (len < size)
    {
      memcpy(local.err_msg, msg_UTF8, len +1); /* +1 for NUL */
    }
  else
    {
      memcpy(local.err_msg, msg_UTF8, size);

      /* [PM] 4.0 find a UTF-8 boundary at which to NUL-terminate */
      {
        size_t i;

        /* could use BACK_WIDE_CHAR */
        SP_ASSERT(size > 0);
        for (i = size-1; i > 0; i--)
          {
            unsigned char byte = local.err_msg[i];
            if ( (byte & 0x80) == 0) break; /* start of one-byte (7-bit) sequence */
            if ( (byte & (0x80 | 0x40)) == (0x80 | 0x40) ) break; /* start of multi-byte sequence */
          }
        local.err_msg[i] = '\0';
      }
    }
}

void sptcl_save_error(
     int type,
     char const *msg_UTF8,
     SP_term_ref culprit,
     int argno)
{
  SP_ASSERT(local.err_culprit == 0); /* xref CLEAR_ERROR() */
  local.err_type = type;
  local.err_argno = argno;
  switch (type&0xff)
    {
    case SPTCL_ERROR:
    case SPTK_ERROR:
      save_error_message(msg_UTF8);
      break;
    default:
      {
        SP_ASSERT(local.err_culprit == 0); /* xref CLEAR_ERROR() */
        local.err_culprit = culprit;
        /* Tcl/Tk strings has string encoding close enough to SP ienc'd.  */
        save_error_message(msg_UTF8);
      }
    }
}

void sptcl_raise_error(char *mod, char *name, int arity)
{
  char *err_funct = "tcl_error";

  switch (local.err_type&0xff)
    {
    case SPTK_ERROR:
      SP_ASSERT(local.err_culprit == 0); /* xref CLEAR_ERROR() */
      err_funct = "tk_error";
    case SPTCL_ERROR:
      SP_ASSERT(local.err_culprit == 0); /* xref CLEAR_ERROR() */
      {
      SP_term_ref
	t1 = SP_new_term_ref(),
	t2 = SP_new_term_ref(),
	t3 = SP_new_term_ref();

      SP_put_string(t1, name);
      SP_put_integer(t2, arity);
      SP_cons_functor(t1, SP_atom_from_string("/"), 2, t1, t2);
      SP_put_list_codes(t2, t3, local.err_msg);
      SP_cons_functor(t1, SP_atom_from_string(err_funct), 2, t1, t2);
      SP_raise_exception(t1);
      break;
    }
    default:
      SP_save_and_raise_error(local.err_type, local.err_msg, local.err_culprit, mod, name, arity, local.err_argno);
    }
  CLEAR_ERROR(); /* paranoia */
}

/*   --------------------------------------------------------------  */

#define SP_TCL_BUF_ALIGNMENT 1024     /* must be a power of two (for SP_ALIGN_UP) */
#define PAD 128
/* [PM] 4.0 Used to align to p->size but alignment must be a power of two.  */
#define ENSURE_SPACE(I) do{                                             \
  if (p->index + (I) + PAD >= p->size)                                  \
    {                                                                   \
      size_t size_ = SP_ALIGN_UP(p->index+(I)+PAD+1, SP_TCL_BUF_ALIGNMENT); \
      char *tmp_ = (char *)SP_realloc(p->buffer, size_);                \
                                                                        \
      SPIO_NULL_CHECK(tmp_);                                            \
      p->size = size_;                                                  \
      p->buffer = tmp_;                                                 \
    }                                                                   \
}while(0)


static spio_t_error_code SPIO_CDECL user_read(void *user_data,
                                              void *buf, /* a spio_t_wchar[] */
                                              size_t *pbuf_size,
                                              spio_t_bits read_options
                                              )
{
  spio_t_error_code code = SPIO_E_ERROR;
  int encoding_state_needs_cleanup = FALSE;
  struct event_stream_data *p = (struct event_stream_data *)user_data;
  
  SP_ASSERT(read_options & SPIO_DEVICE_READ_OPTION_TEXT);
  SP_ASSERT(p->size >= p->index);
  (void)read_options;


  if (p->index >= p->length)
    {
      BARF(SPIO_E_END_OF_FILE);
    }

  {
    spio_t_byte const *src = (spio_t_byte*)&(p->buffer[p->index]);
    size_t src_size = p->length - p->index;
    size_t src_size_read = 0;
    spio_t_wchar *dst = (spio_t_wchar *)buf;
    size_t dst_size = *pbuf_size;

    size_t dst_size_wrote = 0;
    /* size_t src_items_read = 0; */
    spio_t_bits encode_options = 0;

    if ( !(SPIO_MASK_IS_SET(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED)) ) /* first call to encoder (may happen more than once, see do_flush)*/
      {
        CHECK(SP_encoding_open(p->encoding, &p->encoding_state, NULL, SP_ENCODING_OPEN_OPTION_DECODE|SP_ENCODING_OPEN_OPTION_UTF8_TCL));
        encoding_state_needs_cleanup = TRUE;
        SPIO_SET_MASK(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED);
      }

    SP_ASSERT(SPIO_MASK_IS_SET(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED));
    CHECK(SP_encode_to_codes(p->encoding,
                             src, src_size,
                             &p->encoding_state,
                             dst, dst_size,
                             &src_size_read, &dst_size_wrote,
                             encode_options));
#if SICSTUS_TODO
    /* In genereal SP_encode_to_codes could succeed with
       dst_size_wrote == 0 if src starts with something that does not
       produce any output (such as a encoding signature, shift
       sequence, etc). However, nothing like that should happen for
       UTF-8 (we do not treat ZWNBS as a Unicode signature here).  */
#error "[PM] 4.0 Need to handle the case when dst_size_wrote == 0"
#endif  /* SICSTUS_TODO */
    SP_ASSERT(dst_size_wrote > 0);
    SP_ASSERT(SPIO_MASK_IS_SET(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED));
    SP_ASSERT(src_size_read <= src_size);
    SP_ASSERT(dst_size_wrote <= dst_size);
    p->index += src_size_read;
    SP_ASSERT(p->index <= p->length);

    SP_ASSERT(dst_size_wrote <= *pbuf_size);
    *pbuf_size = dst_size_wrote;
    encoding_state_needs_cleanup = FALSE; /* protect from cleanup */
    code = SPIO_S_NOERR;
  }

 cleanup:
  if (encoding_state_needs_cleanup)
    {
      SPIO_CLEAR_MASK(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED);
      (void) SP_encode_to_codes(p->encoding,
                                NULL, 0,
                                &p->encoding_state,
                                NULL, 0,
                                NULL, NULL, /* NULL, */
                                SP_ENCODE_CODES_OPTION_ABORT);
    }
  return code;
 barf:
  goto cleanup;
}


static spio_t_error_code SPIO_CDECL
user_write(void *user_data,
           void const *buf,           /* a spio_t_wchar[] */
           size_t *pbuf_size,
           spio_t_bits write_options
           )
{
  spio_t_error_code code = SPIO_E_ERROR;
  int encoding_state_needs_cleanup = FALSE;
  struct event_stream_data *p = (struct event_stream_data *)user_data;
  size_t buf_size = *pbuf_size;
  
  SP_ASSERT(write_options & SPIO_DEVICE_WRITE_OPTION_TEXT);
  SP_ASSERT(p->size >= p->index);
  (void)write_options;

  ENSURE_SPACE(buf_size);       /* An educated guess but will be enough if strict UTF-8 (i.e. max 4 bytes per code) since sizeof spio_t_wchar == 4. */
  SP_ASSERT(p->index + buf_size <= p->size);

  {
    spio_t_wchar const *src = (spio_t_wchar const *)buf;
    size_t src_size = *pbuf_size;
    size_t src_size_read = 0;
    spio_t_byte *dst = (spio_t_byte*)&(p->buffer[p->index]);
    size_t dst_size = p->size - p->index;

    size_t dst_size_wrote = 0;
    spio_t_bits encode_options = 0;

    if ( !(SPIO_MASK_IS_SET(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED)) ) /* first call to encoder (may happen more than once, see do_flush)*/
      {
        CHECK(SP_encoding_open(p->encoding, &p->encoding_state, NULL, SP_ENCODING_OPEN_OPTION_UTF8_TCL));
        encoding_state_needs_cleanup = TRUE;
        SPIO_SET_MASK(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED);
      }

    SP_ASSERT(SPIO_MASK_IS_SET(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED));
    CHECK(SP_encode_from_codes(p->encoding,
                               src, src_size,
                               &p->encoding_state,
                               dst, dst_size,
                               &src_size_read, &dst_size_wrote,
                               encode_options));
#if SICSTUS_TODO
    /* src_size_read == 0 could perhaps happen but hardly when going from codes to UTF-8 */
#error "Handle src_size_read == 0 (by doing p->index += dst_size_wrote and retrying)"
#endif  /* SICSTUS_TODO */

    SP_ASSERT(src_size_read > 0 || src_size == 0); /* something was written */
    SP_ASSERT(SPIO_MASK_IS_SET(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED));
    SP_ASSERT(src_size_read <= src_size);
    SP_ASSERT(dst_size_wrote <= dst_size);
    p->index += dst_size_wrote;

    *pbuf_size = src_size_read; /* number of bytes consumed from buf */
    encoding_state_needs_cleanup = FALSE; /* protect from cleanup */
    code = SPIO_S_NOERR;

  }

 cleanup:
  if (encoding_state_needs_cleanup)
    {
      SPIO_CLEAR_MASK(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED);
      (void) SP_encode_from_codes(p->encoding,
                                  NULL, 0,
                                  &p->encoding_state,
                                  NULL, 0,
                                  NULL, NULL, /* NULL, */
                                  SP_ENCODE_CODES_OPTION_ABORT);
    }
  return code;
 barf:
  goto cleanup;
}

/* End or abort the encoder. On failure from a non-abortive call it should be called again with abortive true. */
static spio_t_error_code close_stream_encoder(struct event_stream_data *p, int read_direction, int abortive)
{
  spio_t_error_code code = SPIO_E_ERROR;

  /* if we use do_flush from trans_command_list then the encoder may not be inited */

  if (read_direction)
    {
      if (SPIO_MASK_IS_SET(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED))
        {
          code = SP_encode_to_codes(p->encoding,
                                    NULL, 0,
                                    &p->encoding_state,
                                    NULL, 0,
                                    NULL, NULL, /* NULL, */
                                    SP_ENCODE_CODES_OPTION_ABORT); /* always abortive since we will not read further */
          
          SP_ASSERT(!SPIO_FAILED(code)); /* we do not expect errors from SP_ENCODE_CODES_OPTION_ABORT */
          code = SPIO_S_NOERR;  /* but we will ignore any such errors and pretend it became closed */
          (void)code;           /* overwritten below */

          SPIO_CLEAR_MASK(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED);
          p->encoding_state = NULL; /* dbg, not needed */
        }
    }
  else                          /* write direction encoder */
    {
      spio_t_byte *dst = (spio_t_byte*)&(p->buffer[p->index]);
      size_t dst_size = p->size - p->index;
      size_t dst_size_wrote = 0;

      if (SPIO_MASK_IS_SET(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED))
        {
          code = SP_encode_from_codes(p->encoding,
                                      NULL, 0,
                                      &p->encoding_state,
                                      dst, dst_size,
                                      NULL, &dst_size_wrote,
                                      ( abortive ? SP_ENCODE_CODES_OPTION_ABORT : SP_ENCODE_CODES_OPTION_END ));

          if (abortive)             /* ignore errors if abortive close */
            {
              SP_ASSERT(!SPIO_FAILED(code)); /* we do not expect errors from SP_ENCODE_CODES_OPTION_ABORT */
              code = SPIO_S_NOERR;  /* but we will ignore any such errors and pretend it became closed */
              (void)code;           /* overwritten below */

              dst_size_wrote = 0;
            }
          else                      /* !abortive */
            {
              CHECK(code);
            }


          SPIO_CLEAR_MASK(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED);
          p->encoding_state = NULL; /* dbg, not needed */
        
          SP_ASSERT(dst_size_wrote == 0); /* We do not expect the UTF-8 encoder to need to emit any additional bytes (although this would not really be an error) */
          SP_ASSERT(dst_size_wrote < dst_size); /* we certainly expect dst to have more than enough room for any remaining output (such as final newline or some such) and for NUL termination in user_close */

          p->index += dst_size_wrote;
        }
    }

  SP_ASSERT(SPIO_MASK_IS_CLEAR(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED));

  code = SPIO_S_NOERR;

 cleanup:
  return code;
 barf:
  goto cleanup;
}

static spio_t_error_code SPIO_CDECL
user_close(void **puser_data,
           spio_t_bits close_options
           )
{
  spio_t_error_code code = SPIO_E_ERROR;
  struct event_stream_data *p = (struct event_stream_data *)*puser_data;

  if ( (close_options & SPIO_DEVICE_CLOSE_OPTION_READ)
       &&
       (p->flags & TCLTK_STREAM_FLAGS_READ) )
    {
      /* was open in and now closing read direction */
      p->flags &= ~TCLTK_STREAM_FLAGS_READ;

      code = close_stream_encoder(p, TRUE, SPIO_MASK_IS_SET(close_options, SPIO_DEVICE_CLOSE_OPTION_FORCE));
      if (!SPIO_MASK_IS_SET(close_options, SPIO_DEVICE_CLOSE_OPTION_FORCE))
        {
          CHECK(code);          /* ignore stream encoder error if force */
        }
    }
  
  if ( (close_options & SPIO_DEVICE_CLOSE_OPTION_WRITE)
       &&
       (p->flags & TCLTK_STREAM_FLAGS_WRITE) )
    {
      /* was open in and now closing write direction */
      SPIO_CLEAR_MASK(p->flags, TCLTK_STREAM_FLAGS_WRITE);


      code = close_stream_encoder(p, FALSE, SPIO_MASK_IS_SET(close_options, SPIO_DEVICE_CLOSE_OPTION_FORCE));
      if (!SPIO_MASK_IS_SET(close_options, SPIO_DEVICE_CLOSE_OPTION_FORCE))
        {
          CHECK(code);          /* ignore stream encoder error if force */
        }

      /* move the resulting buffer into interp_data unless abortive close */
      if (!SPIO_MASK_IS_SET(close_options, SPIO_DEVICE_CLOSE_OPTION_FORCE))
        {
          SP_ASSERT(p->buffer != NULL);
          SP_ASSERT(p->index < p->size); /* PAD should ensure there is room for NUL */
          p->buffer[p->index] = '\0';
          if (p->presult != NULL)
            {
              *p->presult = p->buffer;
            }
          p->buffer = NULL;         /* interp_data now owns the buffer */
          p->presult = NULL;    /* dbg, not needed */
        }
    }

  if ( !(p->flags & (TCLTK_STREAM_FLAGS_READ | TCLTK_STREAM_FLAGS_WRITE)) )
    {
      /* no direction open, deallocate */
      if (p->buffer != NULL)
        {
          SP_free(p->buffer);
          p->buffer = NULL;
        }
      SP_ASSERT(!SPIO_MASK_IS_SET(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED));
      if (p->encoding != NULL)
        {
          SP_encoding_release(p->encoding);
          p->encoding = NULL;
        }
      SP_free(p);
      *puser_data = NULL;       /* tell caller we are gone */
    }
  code = SPIO_S_NOERR;

 cleanup:
  return code;
 barf:
  goto cleanup;
}

/* arbitrary but unique pointer used to identify our streams */
#define TCLTK_STREAM_CLASS ((void*)&init_tcl_stream)

static spio_t_error_code init_tcl_stream(struct interp_data *interp_data, int want_read_stream, SP_stream **pstream, struct event_stream_data **pp)
{
  spio_t_error_code code = SPIO_E_ERROR;
  struct event_stream_data *p = NULL; /* needs cleanup */
  struct event_stream_data *p_out; /* no cleanup */
  SP_stream *stream = NULL;     /* needs cleanup */
  (void)want_read_stream;
  
  NULL_CHECK(p = (struct event_stream_data *) SP_malloc(sizeof(struct event_stream_data)));

  p->flags = ( want_read_stream ? TCLTK_STREAM_FLAGS_READ : TCLTK_STREAM_FLAGS_WRITE );

  p->interp = interp_data->interp;
  p->encoding = NULL;
  p->encoding_state = NULL; /* DBG, not needed */
  p->size = BUFLEN;
  p->buffer = NULL;
  p->length = 0;            /* not used by write stream */
  p->index = 0;
  p->presult = NULL;        /* caller sets this for write streams */
  /* no failures allowed above */

  NULL_CHECK(p->buffer = (char *)SP_malloc(p->size));

  CHECK(SP_get_encoding("UTF-8", &p->encoding, SPIO_OPTION_NONE));
        
  CHECK(SP_create_stream((void*)p, /* user_data */
                         TCLTK_STREAM_CLASS,
                         ( want_read_stream ? user_read : NULL ),
                         ( want_read_stream ? NULL : user_write ),
                         NULL, /* user_flush_output not needed (or we could use it to flush the write-encoder */
                         NULL, /* user_seek */
                         user_close,
                         NULL, /* user_interrupt */
                         NULL, /* user_ioctl */
                         NULL, /* args */
                         /* No auto flush. We need to flush
                            explicitly so better do it only
                            once. */
                         /* We may want
                            SP_CREATE_STREAM_OPTION_RESET_ON_EOF
                            so we can read-to-eof to cleanup
                            when re-using read-stream (When
                            that gets implemented) */
                         SP_CREATE_STREAM_OPTION_TEXT,
                         &stream
                         ));
  p_out = p;
  p = NULL;               /* stream owns p */


  /* no failure allowed past this point */

  *pp = p_out;
  *pstream = stream;
  stream = NULL;                /* protect from cleanup */
  code = SPIO_S_NOERR;
 cleanup:

  if (stream != NULL)
    {
      (void)SP_fclose(stream, SP_FCLOSE_OPTION_FORCE);
    }

  if (p != NULL)
    {
      if (p->buffer != NULL) SP_free(p->buffer);
      if (p->encoding != NULL) SP_encoding_release(p->encoding);
      SP_free(p);
    }
  return code;
 barf:
  goto cleanup;
}

void tcl_stream_deinit(void)
{
  (void)SP_fclose((SP_stream*)TCLTK_STREAM_CLASS, SP_FCLOSE_OPTION_FORCE|SP_FCLOSE_OPTION_USER_STREAMS);
}

SP_stream *get_tcl_stream(struct interp_data *interp_data, char *goal_utf8)
{
  spio_t_error_code code = SPIO_E_ERROR;
  SP_stream *stream = NULL;
  struct event_stream_data *p = NULL;
  size_t const goal_len = strlen(goal_utf8);
  char const * const pad = " . "; /* Adds extra dot so never miss it */
  size_t const pad_len = strlen(pad);
  size_t const length = goal_len + pad_len;

  CHECK(init_tcl_stream(interp_data, TRUE, &stream, &p));
  SP_ASSERT(stream != NULL && p != NULL);

  ENSURE_SPACE(length +1);      /* +1 for NUL */
  p->length = length;

  memcpy(p->buffer, goal_utf8, goal_len);
  memcpy(p->buffer+goal_len, pad, pad_len);
  p->buffer[length] = '\0';     /* debug, not needed */

  return stream;

 barf:
  if (stream != NULL) (void)SP_fclose(stream, SP_FCLOSE_OPTION_FORCE);
  return NULL;
}

/*   --------------------------------------------------------------  */

static int SP_is_nil(SP_term_ref t)
{
  SP_atom name;

  return (SP_get_atom(t, &name), name == local.atm_nil);
}

char const * translate_command(SP_term_ref tScript, struct interp_data *interp_data)
{
  char const *buf = NULL;
  SP_term_ref t = SP_new_term_ref();

  SP_put_term(t, tScript);
  if ((buf = trans_command(t, interp_data)) == NULL)
    {
      goto barf;
    }

 cleanup:
  return buf;
 barf:
  buf = NULL;
  goto cleanup;

}




/* [PM] 4.0 Ensure all written data have reached p->buffer after writing to the stream. buffer will not be NUL terminated. */
static spio_t_error_code do_flush(SP_stream *stream, struct event_stream_data *p)
{
  spio_t_error_code code = SPIO_E_ERROR;

  CHECK(SP_flush_output(stream, 0));

  /* give the encoding a chance to finish and deallocate its state 
     The encoding will be re-initialized as needed from user_write()
  */
  /* if we use do_flush from trans_command_list then the encoder may not be inited */

    if (SPIO_MASK_IS_SET(p->flags, TCLTK_STREAM_FLAGS_ENCODER_INITED))
      {
        code = close_stream_encoder(p, FALSE, FALSE); /* not read-direction, not abortive */
     
        if (SPIO_FAILED(code))
          {
            /* free write_encoding_state */
            (void) close_stream_encoder(p, FALSE, TRUE); /* not read-direction, abortive */
            BARF(code);
          }
      }

  code = SPIO_S_NOERR;

 cleanup:
  return code;

 barf:
  goto cleanup;
}

static int write_term(
     SP_pred_ref pred,
     SP_term_ref t1,
     SP_stream *stream)
{

  int res;
  SP_term_ref t2 = SP_new_term_ref();

  SP_put_address(t2, (void *)stream);
  if (SP_query_cut_fail(pred, t2, t1) != SP_SUCCESS)
    {
      goto barf;
    }
  res = 1;
 cleanup:
  return res;
 barf:
  res = 0;
  goto cleanup;
}

static int do_format(
     SP_term_ref t_string,
     SP_term_ref t_args,
     SP_stream *stream)
{
  SP_term_ref t3 = SP_new_term_ref();
  int res;

  SP_put_address(t3, (void *)stream);
  if(SP_query_cut_fail(local.format_pred, t3, t_string, t_args) != SP_SUCCESS)
    {
      goto barf;
    }
  res = 1;
 cleanup:
  return res;
 barf:
  res = 0;
  goto cleanup;
}


#define PUT_BUFFER_CHAR(P, STREAM, CHAR) do { code = SP_put_code((STREAM), (CHAR)); if (SPIO_FAILED(code)) goto failure;  } while (0)

static int trans_command_list(SP_term_ref t1, SP_stream *stream, struct event_stream_data *p);

static int trans_command_rec(SP_term_ref t1, SP_stream *stream, struct event_stream_data *p)
{
#define buffer no_access_to_buffer_here
  spio_t_error_code code = SPIO_E_ERROR;
  SP_term_ref t2 = SP_new_term_ref();
  SP_atom name;
  SP_pred_ref pred;
  int i;
  char const *s;
  char c1, c2;

  switch (SP_term_type(t1))
    {
    case SP_TYPE_ATOM:
      if (SP_get_atom(t1, &name)
          && name == local.atm_nil)
        {
          goto success;
        }
      s = SP_string_from_atom(name);
      goto atomic;

    case SP_TYPE_INTEGER:
    case SP_TYPE_FLOAT:
      SP_get_number_codes(t1, &s);
  atomic:
      {
        code = SP_put_encoded_string(stream, s, SPIO_OPTION_NONE);
        if (SPIO_FAILED(code))
          {
            goto failure;
          }
        else
          {
            goto success;
          }
      }
      
    case SP_TYPE_COMPOUND:
      SP_get_functor(t1, &name, &i);
      if (name==local.atm_period && i==2)
	for (;;)
	  {
	    SP_get_list(t1, t2, t1);
	    if (!trans_command_rec(t2, stream, p))
	      goto failure;
	    if (SP_is_list(t1))
	      {
                PUT_BUFFER_CHAR(p, stream, ' ');
		continue;
	      }
	    else
	      if (SP_is_atom(t1) && SP_get_atom(t1, &name) && name==local.atm_nil)
		goto success;
	      else
		goto list_error;
	  }
/* Ignore compiler warnings here! */
      else if (((name==local.atm_dq && (c1='"', c2='"', 1)) ||
		(name==local.atm_sqb && (c1='[', c2=']', 1)) ||
		(name==local.atm_br && (c1='{', c2='}', 1)) ||
		(name==local.atm_min && (c1='-', c2=' ', 1))) &&
	       i==1)
	{
          PUT_BUFFER_CHAR(p, stream, c1);
	  SP_get_arg(1, t1, t2);
	  if (!trans_command_rec(t2, stream, p))
	    goto failure;
          PUT_BUFFER_CHAR(p, stream, c2);
	  /* p->buffer[p->index] = '\0'; */
	  goto success;
	}
      else if (name==local.atm_dot && i==1) {
	SP_get_arg(1, t1, t1);

	if (SP_is_atom(t1))	/* the root path, "." */
          PUT_BUFFER_CHAR(p, stream, '.');
	else while (SP_is_list(t1)) { /* .first.second.third ... */
	  SP_get_list(t1, t2, t1);
          PUT_BUFFER_CHAR(p, stream, '.');
	  if (!trans_command_rec(t2, stream, p))
	    goto failure;
	}
	/* p->buffer[p->index] = '\0'; */
	goto success;
      }
      else if (i == 1
               && (    (name==local.atm_write           && ( pred = local.write_pred, 1))
                    || (name==local.atm_writeq          && ( pred = local.writeq_pred, 1))
                    || (name==local.atm_write_canonical && ( pred = local.write_canonical_pred, 1)) ) )
	{
	  SP_get_arg(1, t1, t1);
          if (write_term(pred, t1, stream))
            goto success;
          else
            goto failure;
        }
      else if (name==local.atm_format && i==2)
	{
	  SP_get_arg(2, t1, t2);
	  SP_get_arg(1, t1, t1);
          if (do_format(t1, t2, stream))
            {
              goto success;
            }
          else
            {
              goto failure;
            }
	}
      else if ((name == local.atm_chars || name==local.atm_codes) && i==1)
	{
	  SP_get_arg(1, t1, t1);
	  if (!SP_get_list_codes(t1, &s)) /* [PM] May 2000 used to ignore errors */
            {
              goto list_error;  /* Misleading error message */
            }
	  goto atomic;
	}
      else if (name==local.atm_list && i==1)
        {
	  SP_get_arg(1, t1, t1);
          if (trans_command_list(t1, stream, p))
            goto success;
          else
            goto failure;
        }
      else
        {
          SAVE_ERROR(DOMAIN_ERROR, "tcl_command_spec", t1, 2);
        }
	    
    case SP_TYPE_VARIABLE:
  instantiation_error:
      SAVE_ERROR(INSTANTIATION_ERROR, "", t1, 2);
    }
  SP_ASSERT(0);                 /* not reached */

 success:
  SP_ASSERT(p->index < p->size);
  /* p->buffer[p->index] = '\0'; */
  return 1;

list_error:
  if (SP_is_variable(t1))
    goto instantiation_error;
  else
    SAVE_ERROR(DOMAIN_ERROR+DOMAIN_LIST, "", t1, 2);
failure:
error:
  return 0;

#undef buffer
}

static int trans_command_list(
     SP_term_ref t1,            /* ListOfCommands */
     SP_stream *stream,
     struct event_stream_data *p)
{
  spio_t_error_code code = SPIO_E_ERROR;
  SP_term_ref tList = SP_new_term_ref(), tCar = SP_new_term_ref();
  int res = 0;                  /* assume error */
  Tcl_Obj *listObj = NULL, *elementObj = NULL;
  char *s;
  int s_len;

  SP_put_term(tList, t1);

  listObj = Tcl_NewListObj(0, NULL); /* empty list */
  Tcl_IncrRefCount(listObj);

  /* need to flush any previous output into buffer */
  CHECK(do_flush(stream, p));

  while (SP_is_list(tList))     /* Note: tList may be [] on entry */
    {
      size_t index;

      /* invariant: do_flush called for any previous stream data */

      SP_get_list(tList, tCar, tList/*cdr*/);
      /* use p->buffer as a scratch area */
      index = p->index;
      if (!trans_command_rec(tCar, stream, p))
        {
          goto error;
        }
      CHECK(do_flush(stream, p));
      SP_ASSERT(index <= p->index);
      if (elementObj) { Tcl_DecrRefCount(elementObj); elementObj = NULL; }
      elementObj = Tcl_NewStringObj(p->buffer+index, (int)(p->index-index));
      Tcl_IncrRefCount(elementObj);
      
      p->index = index;         /* restore p->buffer contents */

      if (Tcl_ListObjAppendElement(p->interp, listObj, elementObj) != TCL_OK)
        {
          goto error;
        }
    }
  if (!SP_is_nil(tList))
    {
      goto list_error;
    }

  s = Tcl_GetStringFromObj(listObj, &s_len);
  ENSURE_SPACE(s_len);
  memcpy(p->buffer+p->index, s, s_len);
  p->index += s_len;

  res = 1;                      /* succeeded */
  goto cleanup;

 cleanup:
  if (elementObj) {
    Tcl_DecrRefCount(elementObj);
  }
  if (listObj) {
    Tcl_DecrRefCount(listObj);
  }
  return res;

 barf:
  (void)code;
  res = 0;
  goto cleanup;

 error:
  res = 0;
  goto cleanup;

 list_error:
  if (SP_is_variable(t1))
    {
      SAVE_ERROR(INSTANTIATION_ERROR, "", t1, 2); /* implicit goto error */
    }
  else
    {
      SAVE_ERROR(DOMAIN_ERROR+DOMAIN_LIST, "", t1, 2); /* implicit goto error */
    }
}

static char const *trans_helper(int (*fun)(SP_term_ref, SP_stream *, struct event_stream_data *p), SP_term_ref t1, struct interp_data *interp_data)
{
  spio_t_error_code code = SPIO_E_ERROR;
  SP_stream *stream = NULL;
  struct event_stream_data *p = NULL;
  char const *buf;

  if (interp_data->stream_buf != NULL)
    {
      SP_free(interp_data->stream_buf);
      interp_data->stream_buf = NULL;
    }

  CHECK(init_tcl_stream(interp_data, FALSE, &stream, &p));
  SP_ASSERT(stream != NULL && p != NULL);
  p->presult = &interp_data->stream_buf;

  if (!(*fun)(t1, stream, p))
    {
      goto barf;
    }

  SP_ASSERT(interp_data->stream_buf == NULL);

  CHECK(SP_fclose(stream, 0));
  stream = NULL;
  SP_ASSERT(interp_data->stream_buf != NULL); /* set by user_close */

  buf = interp_data->stream_buf;

 cleanup:
  if (stream != NULL)
    {
      if (p != NULL)
        {
          p->presult = NULL;        /* dbg, not needed (but maybe if we cache stream) */
        }
      (void)SP_fclose(stream, SP_FCLOSE_OPTION_FORCE); /* this will change if we cache the stream */
    }

  return buf;

 barf:
  (void)code;
  buf = NULL;
  goto cleanup;
}

char const *trans_command(SP_term_ref t1, struct interp_data *interp_data)
{
  return trans_helper(&trans_command_rec, t1, interp_data);
}

static int writeq_term(SP_term_ref t1, SP_stream *stream, struct event_stream_data *p)
{
  (void) p;
  return write_term(local.writeq_pred, t1, stream);
}

/* [PM] writeq a term (used to stringify a prolog exception term). Returns NUL terminated temporary buffer */
char const * trans_term(SP_term_ref t1, struct interp_data *interp_data)
{
  return trans_helper(&writeq_term, t1, interp_data);
}


/*   --------------------------------------------------------------  */

int put_event_queue(struct interp_data *interp_data, int all_flag, SP_term_ref tList)
{
  SP_term_ref
    tTerm = SP_new_term_ref(),
    tStream = SP_new_term_ref();
  SP_stream *stream;
  struct event_q *p = interp_data->event_list;
  int res;
  char *errmsg, errbuf_UTF8[BUFLEN];

  if (p == NULL)
    return 0;			/* tList == atm_nil initially */
  do
    {
      interp_data->event_list = p->next;

      stream = get_tcl_stream(interp_data, p->event_string);
      SP_free(p);
      if (!stream)
	{
	  strcpy(errbuf_UTF8, "Could not find stream");
	  goto error2;
	}
      SP_put_address(tStream, (void *)stream);
      SP_put_variable(tTerm);
      res = SP_query(local.read_pred, tStream, tTerm); /* Read string from Prolog */

      switch (res)
	{
	case SP_ERROR:
	  errmsg = "exception";
	  goto error1;
	case SP_FAILURE:
	  errmsg = "unexpected failure";
	  goto error1;
	case SP_SUCCESS:
	  if (all_flag)
	    SP_cons_list(tList, tTerm, tList);
	  else
	    SP_put_term(tList, tTerm);
	}
      p = interp_data->event_list;
    }
  while (all_flag && p != NULL);

  return 1;
error1:
  snprintf(errbuf_UTF8, (sizeof errbuf_UTF8), "%s in Prolog reading of: %s", errmsg, p->event_string);
  errbuf_UTF8[(sizeof errbuf_UTF8)-1] = '\0';
error2:
  SAVE_ERROR(SPTCL_ERROR, errbuf_UTF8, tStream, 0);
error:
  return -1;
}
