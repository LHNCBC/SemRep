/* Copyright(C) 1996, Swedish Institute of Computer Science */
#define SICSTUS_HIDDEN_API 1

#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */

/* [PM] 3.9.1 including stdlib.h before string.h avoids 'warning:
   conflicting types for built-in function `memcmp'" on HP-UX 11 with
   gcc 2.95.2. I do not know why.
 */
#include <stdlib.h>
#include <string.h>
#include <tcl.h>
#include <tk.h>
#include <sicstus/sicstus.h>
#include <sicstus/config.h>     /* [PM] 4.3 HAVE__SNPRINTF et al. */
#include "tcl.h"

#if HAVE__SNPRINTF && !HAVE_SNPRINTF /* Win32 */
/* #undef snprintf */
#define snprintf _snprintf
#endif

#define TK_NUMVERSION (TK_MAJOR_VERSION*10+TK_MINOR_VERSION)

#if  TK_NUMVERSION >= 41
#define Tk_DoOneEvent Tcl_DoOneEvent
#define TK_DONT_WAIT TCL_DONT_WAIT
#define TK_ALL_EVENTS TCL_ALL_EVENTS
#endif


#define SPTKCONBUFSIZ 1024
#define EOL 10

/* [PM] 4.3 tk_terminal/5 resurrected */
#if !defined(EXPERIMENTAL_TK_TERM)
#define EXPERIMENTAL_TK_TERM 1
#endif /* !defined(EXPERIMENTAL_TK_TERM) */

#if !EXPERIMENTAL_TK_TERM

void tk_term(
             SP_term_ref tInterp,
             char SP_FLI_CONST *tktermsrc,
             char SP_FLI_CONST *text_widget,
             SP_stream **in_stream,
             SP_stream **out_stream,
             SP_stream **err_stream
             )
{
  (void)tInterp; (void)tktermsrc; (void)text_widget; (void)in_stream; (void)out_stream; (void)err_stream;
  CLEAR_ERROR(); /* [PM] 4.3 */
  SAVE_ERROR(SPTCL_ERROR, "tk_term not implemented", tInterp, 0);
  RAISE_ERROR("tk_terminal", 5);
}

#else /* EXPERIMENTAL_TK_TERM */

struct legacy_stream {
  void *handle;
  int (*sgetc)(void *handle);
  int (*sputc)(char c, void *handle);
  int (*sflush)(void *handle);
  int (*seof)(void *handle);
  void (*sclrerr)(void *handle);
  int (*sclose)(void *handle);
};

/* any, unique, address will do */
#define LEGACY_STREAM_CLASS ((void*)&tk_term)

static spio_t_error_code SPCDECL legacy_user_read(void *user_data, void *buf, size_t *pbuf_size, spio_t_bits read_options)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  struct legacy_stream *legacy_handle;
  spio_t_wchar c;
  size_t buf_item_count;
  (void)read_options;
  /* We ignore SPIO_DEVICE_READ_OPTION_NONBLOCKING */

  /* We assume SPIO_DEVICE_READ_OPTION_TEXT */
  SP_ASSERT(SPIO_MASK_IS_SET(read_options, SPIO_DEVICE_READ_OPTION_TEXT));

  legacy_handle = (struct legacy_stream*)user_data;

  SP_ASSERT(legacy_handle->sgetc != NULL);
  c = legacy_handle->sgetc(legacy_handle->handle);

  if (c == -1) { code=SPIO_E_END_OF_FILE; goto barf; }
  

  /* Number of items available that will fit in buf */
  buf_item_count = *pbuf_size / (sizeof c); /* round down */

  if (buf_item_count >= 1) {
    size_t nbytes = sizeof c;
    /* room for at least one wide char */
    memcpy(buf, &c, nbytes);
    *pbuf_size = nbytes;
  } else {
    /* No room in buf. Should not happen */
    SP_ASSERT(0);
    *pbuf_size = 0;
  }

  code = SPIO_S_NOERR;

 cleanup:
  return code;

 barf:
  goto cleanup;
}

static spio_t_error_code SPCDECL legacy_user_write(void *user_data, void const *buf, size_t *pbuf_size, spio_t_bits write_options)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  struct legacy_stream *legacy_handle;
  size_t i;
  spio_t_wchar c;
  size_t buf_item_count;
  (void)write_options;

  /* We ignore SPIO_DEVICE_WRITE_OPTION_NONBLOCKING */

  /* We assume SPIO_DEVICE_WRITE_OPTION_TEXT */
  SP_ASSERT(SPIO_MASK_IS_SET(write_options, SPIO_DEVICE_WRITE_OPTION_TEXT));

  legacy_handle = (struct legacy_stream*)user_data;
  SP_ASSERT(legacy_handle->sputc != NULL);

  /* Number of items in buf */
  buf_item_count = *pbuf_size / (sizeof c); /* round down */


  for (i = 0; i < buf_item_count; i++) {
    int tmp;
    char byte;
    memcpy(&c, ((char*)buf)+(i * sizeof c), sizeof c);
    byte = (char)(c & 0xFF);
    tmp = legacy_handle->sputc(byte, legacy_handle->handle);
    if (tmp == -1) {
      break;
    }
  }
  /* i is the first unwritten item index */

  *pbuf_size = (i * sizeof c);
  code = SPIO_S_NOERR;
  goto cleanup;
 cleanup:
  return code;
}

static spio_t_error_code SPCDECL legacy_flush_output(void *user_data, spio_t_bits flush_options)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  struct legacy_stream *legacy_handle;
  (void)flush_options;

  /* We do not support SPIO_DEVICE_FLUSH_OPTION_NONBLOCKING */
  if (SPIO_MASK_IS_SET(flush_options, SPIO_DEVICE_FLUSH_OPTION_NONBLOCKING)) {
    code = SPIO_E_NOT_SUPPORTED;
    goto barf;
  }

  legacy_handle = (struct legacy_stream*)user_data;
  SP_ASSERT(legacy_handle->sflush != NULL);
  if (legacy_handle->sflush(legacy_handle->handle) == -1) {
    /* FIXME: Proper error handling */
    code = SPIO_E_END_OF_FILE;
    goto barf;
  }

  code = SPIO_S_NOERR;
 cleanup:
  return code;
 barf:
  goto cleanup;
}


static spio_t_error_code SPCDECL legacy_user_close(void **puser_data, spio_t_bits close_options)
{
  struct legacy_stream *legacy_handle;

  legacy_handle = (struct legacy_stream*)*puser_data;

  /* we can ignore SPIO_DEVICE_CLOSE_OPTION_FORCE */

  if (legacy_handle->sgetc != NULL
      &&
      close_options & SPIO_DEVICE_CLOSE_OPTION_READ)
    {
      legacy_handle->sgetc = NULL; /* clear read direction */
    }
  if (legacy_handle->sputc != NULL
      &&
      close_options & SPIO_DEVICE_CLOSE_OPTION_WRITE)
    {
      legacy_handle->sputc = NULL; /* clear write direction */
    }
  if (legacy_handle->sgetc == NULL
      && legacy_handle->sputc == NULL) /* all directions closed */
    {
      *puser_data = NULL;       /* tell caller we are gone */
      legacy_handle->sclose(legacy_handle->handle);
      SP_free(legacy_handle);
    }
  return SPIO_S_NOERR;
}

/* [PM] 4.3 Emulate SP_make_stream_context() from SICStus 3 */
static int make_stream_context(
                               void *handle,
                               int (*sgetc)(void *handle),
                               int (*sputc)(char c, void *handle),
                               int (*sflush)(void *handle),
                               int (*seof)(void *handle),
                               void (*sclrerr)(void *handle),
                               int (*sclose)(void *handle),
                               SP_stream **stream_code,
                               unsigned long option,
                               int context)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  int rc;
  SP_stream *stream = NULL; /* Needs cleanup */
  struct legacy_stream *legacy_handle = NULL; /* Needs cleanup */
  (void) option;
  (void) context;

  legacy_handle = SP_malloc(sizeof *legacy_handle);
  if (legacy_handle == NULL) { code = SPIO_E_OUT_OF_MEMORY; goto barf; }
  
  legacy_handle->handle = handle;
  legacy_handle->sgetc = sgetc;
  legacy_handle->sputc = sputc;
  legacy_handle->sflush = sflush;
  legacy_handle->seof = seof;
  legacy_handle->sclrerr = sclrerr;
  legacy_handle->sclose = sclose;
  
  if (SPIO_FAILED(code = SP_create_stream((void*)legacy_handle,
                                          LEGACY_STREAM_CLASS,
                                          ( sgetc == NULL ? NULL : legacy_user_read),
                                          ( sputc == NULL ? NULL : legacy_user_write),
                                          ( sflush == NULL ? NULL : legacy_flush_output ),
                                          NULL, /* seek */
                                          legacy_user_close,
                                          NULL, /* interrupt */
                                          NULL, /* ioctl */
                                          NULL, /* args */
                                          (SP_CREATE_STREAM_OPTION_TEXT
                                           |
                                           SP_CREATE_STREAM_OPTION_INTERACTIVE
                                           |
                                           SP_CREATE_STREAM_OPTION_RESET_ON_EOF
                                           ),
                                          &stream)))
    {
      goto barf;
    }
  legacy_handle = NULL; /* protect from cleanup */

  if (stream_code != NULL) {
    *stream_code = stream;
    stream = NULL; /* protect from cleanup */
  }


  rc = SP_SUCCESS;
 cleanup:
  if (stream != NULL) {
    /* FIXME: cleanup */
  }
  if (legacy_handle != NULL) {
    /* FIXME: cleanup */
  }
  return rc;
 barf:
  rc = SP_FAILURE;
  goto cleanup;
}



struct sptkcon {
  Tcl_Interp *interp;
  char *text_widget;
  size_t cnt;
  size_t index;
  size_t size;
  char *buf;
  int ready;
  int eof;
  char *tcloutfun;
};

static char *tcloutout = "sptcout";
static char *tclouterr = "sptcerr";

static int sptkcon_flush PROTOTYPE((void *raw_handle));

static int sptkcon_getc(void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;
  
  if (!con->cnt)
    {
      sptkcon_flush(local.sptkcon_errh);
      while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT));
      sptkcon_flush(local.sptkcon_outh);
      while (!con->ready)
	Tk_DoOneEvent(0);
      con->ready = 0;
      if (Tcl_Eval(con->interp, "sptcin") != TCL_OK)
	{
	  con->eof = 1;
	  return -1;
	}
#if SICSTUS_DBG
      SP_fprintf(local.old_err, ">>%s<<\n", Tcl_GetStringResult(con->interp));
#endif  /* SICSTUS_DBG */
      strcpy(con->buf, Tcl_GetStringResult(con->interp));
      con->cnt = strlen(con->buf);
      if (!con->cnt)
	{
	  con->eof = 1;
	  return -1;
	}
      con->index = 0;
    }
  con->cnt--;
  return con->buf[con->index++];
}

static int sptkcon_putc(char c, void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;
  if (con->index == con->size)
    {
      size_t new_size = con->size*2;
      void *new_mem = SP_realloc(con->buf, new_size);
      if (con->buf == NULL) {
        goto out_of_memory;
      }
      con->size = new_size;
      con->buf = (char *)new_mem;
    }
  con->buf[con->index++] = c;
  if (c == EOL)
    {
      con->buf[con->index++] = '\0';      
      Tcl_SetVar(con->interp, "sptc_out", con->buf, TCL_GLOBAL_ONLY);
      if (Tcl_Eval(con->interp, con->tcloutfun) != TCL_OK)
	{
	  con->eof = 1;
	  return -1;
	}
      while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT));
      con->index = 0;
    }
  return c;
 out_of_memory:
  /* [PM] 4.3 FIXME: Proper error handling */
  con->eof = 1;
  return -1;  
}

/* For unbuffered stderr (slow!)
static int sptkcon_errputc(int c, struct sptkcon *handle)
{
  handle->buf[0] = c;
  Tcl_SetVar(handle->interp, "sptc_out", handle->buf, TCL_GLOBAL_ONLY);
  if (Tcl_Eval(handle->interp, "sptcerr") != TCL_OK)
    {
      handle->eof = 1;
      return -1;
    }
  if (c == EOL)
    while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT));
  return c;
}
*/

static int sptkcon_flush(void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;
  if (con->index > 0)
    {
      con->buf[con->index++] = '\0';      
      Tcl_SetVar(con->interp, "sptc_out", con->buf, TCL_GLOBAL_ONLY);
      if (Tcl_Eval(con->interp, con->tcloutfun) != TCL_OK)
	{
	  con->eof = 1;
	  return -1;
	}
      while(Tk_DoOneEvent(TK_ALL_EVENTS|TK_DONT_WAIT));
      con->index = 0; 
    }
  return 0;
}

static int sptkcon_eof(void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;

  return con->eof;
}

static void sptkcon_clrerr(void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;

  con->eof = 0;
}

static int sptkcon_close(void *handle)
{
  struct sptkcon *con = (struct sptkcon *)handle;

  SP_free(con->buf);
  SP_free(con);
  return 0;
}

static int sptkcon_set_ready(
    ClientData clientData,
    Tcl_Interp *interp,
    int argc,
    char **argv)
{
  struct sptkcon *handle = (struct sptkcon *)clientData;

  (void)interp;                 /* avoid -Wunused */
  (void)argc;
  (void)argv;

  handle->ready = 1;
  return TCL_OK;
}

static int sptkcon_interrupt( /* (clientData, interp, argc, argv) */
    ClientData clientData,
    Tcl_Interp *interp,
    int argc,
    char **argv)
{

  (void)clientData;             /* avoid -Wunused */
  (void)interp;
  (void)argc;
  (void)argv;

  SP_ctrlc_action();
  return TCL_OK;
}

void tk_term(
             SP_term_ref tInterp,
             char SP_FLI_CONST *tktermsrc,
             char SP_FLI_CONST *text_widget,
             SP_stream **in_stream,
             SP_stream **out_stream,
             SP_stream **err_stream
             )
{
  struct interp_data *interp_data =
    (struct interp_data *)wrapper_to_ptr(local.atm_interp, tInterp);
  Tcl_Interp *interp;
  Tcl_CmdInfo cmd_info;
  struct sptkcon *inh, *outh, *errh;
  char buf[1024];
  CLEAR_ERROR(); /* [PM] 4.3 */
  
  strcpy(buf,"source ");
  {
    size_t const buf_remain = (sizeof buf)-strlen(buf);
    strncat(buf,tktermsrc, buf_remain);
    buf[(sizeof buf)-1] = '\0';
  }

  CHECK_INTERP(interp_data, interp, tInterp, 1);

  if (Tcl_GetCommandInfo(interp, "sptc_set_ready", &cmd_info) != 0)
    SAVE_ERROR(SPTCL_ERROR, "Previous tk-term exists", tInterp, 0);
  if (Tcl_Eval(interp, buf) != TCL_OK)
    SAVE_ERROR(SPTCL_ERROR, "Can't source tkterm.tcl", tInterp, 0);
  {
    char buf[256];
    snprintf(buf, (sizeof buf), "sptc_start %s", text_widget);
    buf[(sizeof buf)-1] = '\0';
    if (Tcl_Eval(interp, buf) != TCL_OK)
      SAVE_ERROR(SPTCL_ERROR, "Can't initiate tk-term", tInterp, 0);
  }

  inh  = (struct sptkcon *)SP_malloc(sizeof(struct sptkcon));
  if (inh == NULL) goto out_of_memory;
  outh  = (struct sptkcon *)SP_malloc(sizeof(struct sptkcon));
  if (outh == NULL) goto out_of_memory;
  errh  = (struct sptkcon *)SP_malloc(sizeof(struct sptkcon));
  if (errh == NULL) goto out_of_memory;

  Tcl_CreateCommand(interp, "sptc_set_ready",
		    (Tcl_CmdProc *)sptkcon_set_ready,
		    (ClientData)inh, (Tcl_CmdDeleteProc *)NULL);
  Tcl_CreateCommand(interp, "sptc_interrupt",
		    (Tcl_CmdProc *)sptkcon_interrupt,
		    (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL);


  inh->interp = outh->interp = errh->interp = interp;
  /* text_widget is an atom name so may be reclaimed by atom-GC. */
  {
    char *tmp1 = SP_strdup(text_widget);
    char *tmp2 = SP_strdup(text_widget);
    char *tmp3 = SP_strdup(text_widget);
    
    if (tmp1 == NULL || tmp2 == NULL || tmp3 == NULL) {
      if (tmp1 != NULL) { SP_free(tmp1); }
      if (tmp2 != NULL) { SP_free(tmp2); }
      if (tmp3 != NULL) { SP_free(tmp3); }
      goto out_of_memory;
    }
    
    inh->text_widget = tmp1;
    outh->text_widget = tmp2;
    errh->text_widget = tmp3;
  }


  inh->cnt = outh->cnt = errh->cnt = 0;
  inh->index = outh->index = errh->index = 0;
  inh->ready = 0;
  inh->size = outh->size = errh->size = SPTKCONBUFSIZ;
  inh->eof = outh->eof = errh->eof = 0;
  inh->buf = SP_malloc(SPTKCONBUFSIZ);
  if (inh->buf == NULL) goto implement_proper_cleanup;
  outh->buf = SP_malloc(SPTKCONBUFSIZ);
  if (outh->buf == NULL) goto implement_proper_cleanup;
  errh->buf = SP_malloc(SPTKCONBUFSIZ);
  if (errh->buf == NULL) goto implement_proper_cleanup;

  outh->tcloutfun = tcloutout;
  errh->tcloutfun = tclouterr;
  local.sptkcon_outh = outh;
  local.sptkcon_errh = errh;

#if SICSTUS_DBG
  local.old_err = SP_stderr;
#endif  /* SICSTUS_DBG */
  if (make_stream_context(inh, sptkcon_getc, NULL, NULL,
		 sptkcon_eof, sptkcon_clrerr, sptkcon_close, in_stream,
		 SP_WCX_FLAG, SP_STREAMHOOK_LIB) != SP_SUCCESS) {
    goto stream_creation_error;
  };
  if (make_stream_context(outh, NULL, sptkcon_putc, sptkcon_flush,
		 NULL, NULL, sptkcon_close, out_stream, 
		 SP_WCX_FLAG, SP_STREAMHOOK_LIB) != SP_SUCCESS) {
    goto stream_creation_error;
  };
  if (make_stream_context(errh, NULL, sptkcon_putc, sptkcon_flush,
                          NULL, NULL, sptkcon_close, err_stream, 
                          SP_WCX_FLAG, SP_STREAMHOOK_LIB) != SP_SUCCESS) {
    goto stream_creation_error;
  };


  return;

 out_of_memory:
  SAVE_ERROR(SPTCL_ERROR, "tk_term got out of memory", tInterp, 0);
  /* NOT REACHED */

 implement_proper_cleanup:
#if !SICSTUS_FORCE_BUILD && !SICSTUS_BETA_VERSION && SICSTUS_VERSION > 40401 /* [PM] 4.3.1 postpone */
#error "[PM] 4.3 implement proper cleanup"
#endif /* !SICSTUS_FORCE_BUILD */

  SP_ASSERT(0);
  goto stream_creation_error;

 stream_creation_error:
  SAVE_ERROR(SPTCL_ERROR, "tk_term could not create streams", tInterp, 0);

  RAISE_ERROR("tk_terminal", 5);
  return;

}
#endif  /* EXPERIMENTAL_TK_TERM */
