/* Copyright (C) 2006, Swedish Institute of Computer Science. */

#include <sicstus/sicstus.h>
#include <string.h>

/* These are the functions imported by codesio.pl */
#include "codesio_glue.h"

typedef struct  codesio_t_open_codes_ codesio_t_open_codes;
struct codesio_t_open_codes_ {
  int *codes;                   /* UNICODE chars */

#define CODESIO_OPEN_CODES_FLAG_READ  ((spio_t_bits)0x0001)
#define CODESIO_OPEN_CODES_FLAG_WRITE ((spio_t_bits)0x0002)
  spio_t_bits flags;

  size_t index;                 /* index in codes[], 0..(len-1) */
  size_t len;                   /* number of items in codes[] */
  #if MULTI_SP_AWARE
  void* (SPCDECL *realloc_fun)(void*, size_t); /* [PM] 3.9b4 pointer to SP_realloc */
  void (SPCDECL *free_fun)(void*); /* [PM] 3.9b4 pointer to SP_free */
  #endif/* MULTI_SP_AWARE */
};


static spio_t_error_code SPCDECL codesio_user_read(void *user_data, void *buf, size_t *pbuf_size, spio_t_bits read_options)
{
  spio_t_error_code code = SPIO_E_ERROR;
  codesio_t_open_codes *open_codes;
  size_t nitems;
  size_t nremaining_items;
  size_t nbytes;
  (void)read_options;
  /* We can ignore SPIO_DEVICE_READ_OPTION_NONBLOCKING */
  /* We assume SPIO_DEVICE_READ_OPTION_TEXT */

  open_codes = (codesio_t_open_codes*)user_data;

  nitems = *pbuf_size / (sizeof *open_codes->codes); /* round down */

  nremaining_items = open_codes->len - open_codes->index;
  if (nremaining_items == 0) { code=SPIO_E_END_OF_FILE; goto barf; }

  if (nremaining_items < nitems) nitems = nremaining_items;
  nbytes = nitems * (sizeof *open_codes->codes);

  memcpy(buf, open_codes->codes+open_codes->index, nbytes);
  open_codes->index += nitems;
  *pbuf_size = nbytes;
  code = SPIO_S_NOERR;

 cleanup:
  return code;

 barf:
  goto cleanup;
}

static spio_t_error_code SPCDECL codesio_user_write(void *user_data, void const *buf, size_t *pbuf_size, spio_t_bits write_options)
{
  spio_t_error_code code = SPIO_E_ERROR;
  codesio_t_open_codes *open_codes;
  size_t nitems;
  size_t nremaining_items;
  size_t nbytes;
  (void)write_options;

  /* We can ignore SPIO_DEVICE_WRITE_OPTION_NONBLOCKING */
  /* We assume SPIO_DEVICE_WRITE_OPTION_TEXT */

  open_codes = (codesio_t_open_codes*)user_data;

  nitems = *pbuf_size / (sizeof *open_codes->codes); /* round down */
  nbytes = nitems * (sizeof *open_codes->codes);

  nremaining_items = open_codes->len - open_codes->index;
  if (nremaining_items < nitems)
    {
      size_t new_len = open_codes->len * 2;
      void *tmp;

      nremaining_items = new_len - open_codes->index;
      if (nremaining_items < nitems)
        {
          new_len += (nitems-nremaining_items);
        }

#if MULTI_SP_AWARE
      tmp = open_codes->realloc_fun(open_codes->codes, new_len * (sizeof *open_codes->codes));
#else  /* !MULTI_SP_AWARE */
      tmp = SP_realloc(open_codes->codes, new_len * (sizeof *open_codes->codes));
#endif  /* !MULTI_SP_AWARE */

      if (tmp == NULL) { code = SPIO_E_OUT_OF_MEMORY; goto barf; }
      open_codes->codes = tmp;
      open_codes->len = new_len;
    }

  memcpy(open_codes->codes+open_codes->index, buf, nbytes);
  open_codes->index += nitems;
  *pbuf_size = nbytes;
  code = SPIO_S_NOERR;
 cleanup:
  return code;

 barf:
  goto cleanup;
}

static spio_t_error_code SPCDECL codesio_user_close(void **puser_data, spio_t_bits close_options)
{
  codesio_t_open_codes *open_codes;

  open_codes = (codesio_t_open_codes*)*puser_data;

  /* we can ignore SPIO_DEVICE_CLOSE_OPTION_FORCE */

  if (open_codes->flags & CODESIO_OPEN_CODES_FLAG_READ
      &&
      close_options & SPIO_DEVICE_CLOSE_OPTION_READ)
    {
      open_codes->flags &= ~CODESIO_OPEN_CODES_FLAG_READ; /* clear it */
    }
  if (open_codes->flags & CODESIO_OPEN_CODES_FLAG_WRITE
      &&
      close_options & SPIO_DEVICE_CLOSE_OPTION_WRITE)
    {
      open_codes->flags &= ~CODESIO_OPEN_CODES_FLAG_WRITE; /* clear it */
    }
  if ((open_codes->flags & CODESIO_OPEN_CODES_FLAG_READ) == 0
      && (open_codes->flags & CODESIO_OPEN_CODES_FLAG_WRITE) == 0) /* all directions closed */
    {
      *puser_data = NULL;       /* tell caller we are gone */
#if MULTI_SP_AWARE
      open_codes->free_fun(open_codes->codes);
      open_codes->free_fun(open_codes);
#else  /* !MULTI_SP_AWARE */
      SP_free(open_codes->codes);
      SP_free(open_codes);
#endif  /* !MULTI_SP_AWARE */
    }
  return SPIO_S_NOERR;
}

#if MULTI_SP_AWARE && 0         /* Disable since there should be no need to have separate stream class for separate sicstuses */
#define CODESIO_STREAM_CLASS_USE_STASH 1
#else  /* !MULTI_SP_AWARE */
#define CODESIO_STREAM_CLASS_USE_STASH 0
#endif  /* !MULTI_SP_AWARE */

/* any, unique, address will do */
#if CODESIO_STREAM_CLASS_USE_STASH
#define CODESIO_STREAM_CLASS (*SP_foreign_stash())
#else  /* !CODESIO_STREAM_CLASS_USE_STASH */
#define CODESIO_STREAM_CLASS ((void*)&codes_to_stream)
#endif  /* !CODESIO_STREAM_CLASS_USE_STASH */

static spio_t_error_code codesio_stream(SPAPI_ARG_PROTO_DECL
                                        char const *str, SP_stream **streamp)
{
  spio_t_error_code code = SPIO_E_ERROR;
  codesio_t_open_codes *open_codes;
  SP_stream *stream = NULL;

  open_codes = (codesio_t_open_codes *)SP_malloc(sizeof(codesio_t_open_codes));
  if (open_codes == NULL) goto barf;

  #if MULTI_SP_AWARE
  open_codes->realloc_fun = SP_realloc;
  open_codes->free_fun = SP_free;
  #endif/* MULTI_SP_AWARE */

  if (str == NULL)              /* output stream */
    {
      open_codes->flags = CODESIO_OPEN_CODES_FLAG_WRITE;

      open_codes->len = 1024;   /* arbitrary initial length */
      open_codes->codes = (int *)SP_malloc(open_codes->len * (sizeof *open_codes->codes));
      if (open_codes->codes == NULL) goto out_of_memory;
      open_codes->index = 0;
    }
  else                          /* input stream */
    {
      char const *p;
      size_t slen;
      int *q;
      int tmp;

      open_codes->flags = CODESIO_OPEN_CODES_FLAG_READ;

      slen = strlen(str);
      /* assume (likely) worst case, all chars are 7-bit */
      open_codes->codes = (int *)SP_malloc(slen * (sizeof *open_codes->codes));
      if (open_codes->codes == NULL) goto out_of_memory;
      open_codes->index = 0;
      open_codes->len = 0;
      for (p = str, q = open_codes->codes; (tmp = SP_wci_code(q, p)) > 0; p += tmp, q++, open_codes->len++)
        {
          ;                         /* empty */
        }
      if (tmp != 0) goto barf;      /* mis-coded 'cannot' happen */
      SP_ASSERT(open_codes->len <= slen);
    }

  if (SPIO_FAILED(code = SP_create_stream((void*)open_codes,
                                          CODESIO_STREAM_CLASS,
                                          ( str == NULL ? NULL : codesio_user_read),
                                          ( str != NULL ? NULL : codesio_user_write),
                                          NULL, /* flush_output */
                                          NULL, /* seek */
                                          codesio_user_close,
                                          NULL, /* interrupt */
                                          NULL, /* ioctl */
                                          NULL, /* args */
                                          SP_CREATE_STREAM_OPTION_TEXT,
                                          &stream)))
    {
      goto barf;
    }

  *streamp = stream;
  stream = NULL;                /* protect against cleanup */
  open_codes = NULL;            /* protect against cleanup */
  code = SPIO_S_NOERR;

 cleanup:
  if (open_codes != NULL)
    {
      if (open_codes->codes != NULL) SP_free(open_codes->codes);
      SP_free(open_codes);
    }
  if (stream != NULL) (void)SP_fclose(stream, SP_FCLOSE_OPTION_FORCE);

  return code;

 out_of_memory:
  code = SPIO_E_OUT_OF_MEMORY;
  goto barf;

 barf:
  goto cleanup;
}

void SPCDECL codes_to_stream(SPAPI_ARG_PROTO_DECL char SP_FLI_CONST *str, SP_stream **streamp)
{
  spio_t_error_code code = SPIO_E_ERROR;

  code = codesio_stream(SPAPI_ARG str, streamp);
  if (SPIO_FAILED(code))
    {
      *streamp = NULL;
    }
  return;
}

void SPCDECL open_buf_stream(SPAPI_ARG_PROTO_DECL
                             SP_stream **streamp)
{
  spio_t_error_code code = SPIO_E_ERROR;

  code = codesio_stream(SPAPI_ARG NULL, streamp);
  if (SPIO_FAILED(code))
    {
      *streamp = NULL;
    }
  return;
}

void SPCDECL stream_to_codes(SPAPI_ARG_PROTO_DECL
                             SP_stream  *stream,
                             SP_term_ref head, SP_term_ref tail)
{
  spio_t_error_code code = SPIO_E_ERROR;
  void *user_data = NULL;
  codesio_t_open_codes *open_codes = NULL;
  
  if (SPIO_FAILED(code = SP_flush_output(stream, 0))) /* ! */
    {
      goto barf;
    }

  if (SPIO_FAILED(code = SP_get_stream_user_data(stream, CODESIO_STREAM_CLASS, &user_data)))
    {
      goto barf;                /* not a codesio stream? */
    }

  open_codes = (codesio_t_open_codes*)user_data;

  if (! (open_codes->flags & CODESIO_OPEN_CODES_FLAG_WRITE) ) /* not write stream */
    {
      goto barf;
    }
  
  {
    int const *p;
    int const *limit;
    SP_term_ref ch_tr = SP_new_term_ref();
    SP_term_ref tail_tr = SP_new_term_ref();

    SP_put_variable(tail);
    SP_put_term(tail_tr, tail);

    for (p = &open_codes->codes[0], limit = p + open_codes->index; p < limit; limit--)
      {
        SP_put_integer(ch_tr, *(limit-1));
        
        SP_cons_list(tail_tr, ch_tr, tail_tr);
      }
    SP_put_term(head, tail_tr);
  }
  return;
 barf:
  (void)code;
  SP_put_string(head, "error");
  return;
}

void SPCDECL codesio_deinit(SPAPI_ARG_PROTO_DECL 
                            int when)
{
  (void)when;

  /* Close any un-closed streams before unloading this foreign resource. */
  (void)SP_fclose((SP_stream*)CODESIO_STREAM_CLASS, SP_FCLOSE_OPTION_FORCE|SP_FCLOSE_OPTION_USER_STREAMS);

#if CODESIO_STREAM_CLASS_USE_STASH
  {
    void *stash = *SP_foreign_stash();
    SP_ASSERT(stash != NULL);

    SP_free(stash);
    *SP_foreign_stash() = NULL;   /* dbg */
  }
#endif  /* CODESIO_STREAM_CLASS_USE_STASH */
}

void SPCDECL codesio_init(SPAPI_ARG_PROTO_DECL 
                          int when)
{
  (void)when;
#if MULTI_SP_AWARE
  (void)spenv_arg;
#endif  /* MULTI_SP_AWARE */

#if CODESIO_STREAM_CLASS_USE_STASH
  {
    void *stash;

    stash = SP_malloc(1);         /* arbitrary but unique address */
    if (stash == NULL) goto out_of_memory;
    

    /* used as CODESIO_STREAM_CLASS */
    *SP_foreign_stash() = stash;
  }
#endif  /* CODESIO_STREAM_CLASS_USE_STASH */
}

