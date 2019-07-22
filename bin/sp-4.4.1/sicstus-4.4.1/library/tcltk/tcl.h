/* Copyright(C) 1994, Swedish Institute of Computer Science */

#define BUFLEN 1024             /* [PM] 4.0 Must be a a power of two (so it is suitable as Alignment arg to SP_ALIGN_UP, see ENSURE_SPACE) */
#define TRUE 1
#define FALSE 0

/* Local error types */
#define SPTCL_ERROR (INSTANTIATION_ERROR+32)
#define SPTK_ERROR (SPTCL_ERROR+1)

/*   --------------------------------------------------------------  */

/* Type declarations */

struct interp_data {
  struct interp_data *self;	/* For checking */
  Tcl_Interp *interp;
  /* [PM] 4.0 No stream re-use yet: SP_stream *stream; */
  char *stream_buf;             /* [PM] 4.0 data written to stream goes here on close (SP_malloc-ed) */
  struct event_q *event_list;
};

struct event_q {
  struct event_q *next;
  char event_string[1];
};

enum {
  TCLTK_STREAM_FLAGS_READ  = 0x0001,
  TCLTK_STREAM_FLAGS_WRITE = 0x0002, /* read/write are mutually exclusive (we use separate streams where SP3 used a single bidi stream) */

  TCLTK_STREAM_FLAGS_ENCODER_INITED  = 0x0010,

  TCLTK_STREAM_FLAGS_END_
};


struct event_stream_data {
  Tcl_Interp *interp;

  SP_uinteger flags;
  size_t index;
  size_t length;
  size_t size;
  char *buffer;
  char **presult;               /* store buffer here on successful close */
  spio_t_encoding *encoding;    /* from spio_t_wchar to/from (the Tcl/Tk dialect of) UTF-8 */
  spio_t_encoding_state *encoding_state;
};

/*   --------------------------------------------------------------  */

struct local_state {
  SP_atom atm_nil;
  SP_atom atm_true;
  SP_atom atm_false;
  SP_atom atm_interp;
  SP_atom atm_period;
  SP_atom atm_write;
  SP_atom atm_writeq;
  SP_atom atm_write_canonical;
  SP_atom atm_format;
  SP_atom atm_codes;
  SP_atom atm_chars;
  SP_atom atm_br;
  SP_atom atm_dq;
  SP_atom atm_sqb;
  SP_atom atm_min;
  SP_atom atm_dot;
  SP_atom atm_list;
  SP_atom atm_term;
  SP_pred_ref call_pred;
  SP_pred_ref read_pred;
  SP_pred_ref write_pred;
  SP_pred_ref writeq_pred;
  SP_pred_ref write_canonical_pred;
  SP_pred_ref format_pred;
  int tcl_no_registry_check; /* = 0 [PM] */
  SP_atom atm_window;
  int tk_inited;		/* = 0 */
#if SICSTUS_DBG
  SP_stream *old_err;
#endif
  struct sptkcon *sptkcon_outh; /* = NULL */
  struct sptkcon *sptkcon_errh; /* = NULL */
  int err_type;
  char err_msg[BUFLEN]; /* [PM] 4.2.1+ Caller use sizeof err_msg so err_msg must be an array, not a pointer to buffer. */
  int err_argno;
  SP_term_ref err_culprit; /* [PM] 4.3 zero means none. See CLEAR_ERROR(). */
};

/* [PM] 3.9b5 PRM 2939 resource specific name for local state variable
   using external linking is needed to avoid conflict when linking
   statically with other resources using a 'local' state */
#define local tcl_resource_local

extern struct local_state local;

/*   --------------------------------------------------------------  */
/* Exported functions */

#include "tcltk_glue.h"

/* called from tk_initialize/deinitialize */
extern void tcl_initialize PROTOTYPE((int));
extern void tcl_deinitialize PROTOTYPE((int));

/*   --------------------------------------------------------------  */

void ptr_to_wrapper PROTOTYPE((SP_atom functor,void *,SP_term_ref));
void *wrapper_to_ptr PROTOTYPE((SP_atom functor,SP_term_ref t));
void sptcl_raise_error PROTOTYPE((char *, char *, int));
void sptcl_save_error (int, char const *msg_UTF8, SP_term_ref, int);
/* SP_stream *init_tcl_stream PROTOTYPE((struct interp_data *)); */
SP_stream *get_tcl_stream PROTOTYPE((struct interp_data *, char *));

char const * translate_command(SP_term_ref, struct interp_data *);
char const * trans_command(SP_term_ref, struct interp_data *);
char const * trans_term(SP_term_ref, struct interp_data *);

void tcl_stream_deinit(void);


int put_event_queue PROTOTYPE((struct interp_data *, int, SP_term_ref));

/*   --------------------------------------------------------------  */

/* This macro is used in predicate definitions that need a efficient */
/* buffer allocation  */
/* You give it a static buffer or a buffer allocated on the stack */
/* with a fixed size. If it is to small this macro allocates it */
/* with SP_malloc */

/* _LEN is the needed size of the buffer */
/* _SIZE is the size of the allocated buffer on the stack */
/* _BUF is the name of the stack buffer */
/* _ERROR is code to execute on error */
/* _PTR is a pointer to the buffer to use */
#define BUF_ALLOC(_LEN,_SIZE,_BUF,_ERROR,_PTR) \
{ \
  if ((_LEN) >= (_SIZE)) \
    { \
      _PTR = (char *)SP_malloc((_LEN)); \
      if (_PTR == NULL) \
	{ \
	  _ERROR; \
	} \
    } \
  else \
    _PTR = _BUF; \
}

/* Deallocate if dynamically allocated */

#define BUF_FREE(_BUF,_PTR) \
{ \
  if (_BUF != _PTR) \
    SP_free(_PTR); \
}

/*   --------------------------------------------------------------  */

#define SP_ALIGN_DOWN(X, Alignment) \
    (((size_t)(X)) & ~(size_t)((Alignment)-1))

/* Round up to even multiple of Alignment (a power of two) */
#define SP_ALIGN_UP(X, Alignment) \
    SP_ALIGN_DOWN(((size_t)(X) + ((Alignment)-1)), (Alignment))

#define SP_ALIGN(X, Alignment) SP_ALIGN_UP((X), (Alignment))

/*   --------------------------------------------------------------  */

/* [PM] 4.3 Msg_ienc will be static ASCII strings or a Tcl/Tk string (which uses UTF-8 nowadays). */
#define MSG_BARF(MSG_ienc) do{ error_message=(MSG_ienc); goto msg_barf_; }while(0)

/* [PM] 4.3 Call CLEAR_ERROR() early in all functions that calls RAISE_ERROR() */
#define CLEAR_ERROR() do{ local.err_culprit = 0; local.err_argno = 0; local.err_msg[0] = '\0'; }while(0)

/* [PM] 4.0 Msg_ienc will be static ASCII strings or a Tcl/Tk string (which uses UTF-8 nowadays). */
#define SAVE_ERROR(Type, Msg_UTF8, Culprit, ArgNo) \
{ \
  sptcl_save_error((Type), (Msg_UTF8), (Culprit), (ArgNo)); \
  goto error; \
}

/* [PM] 4.3 Call CLEAR_ERROR() early in all functions that calls RAISE_ERROR() */
#define RAISE_ERROR(PredName, Arity) \
{ \
 error: \
  sptcl_raise_error("tcltk", PredName, Arity); \
  return; \
}

#define SAVE_AND_RAISE_ERROR(Type, Msg_UTF8, Culprit, ArgNo, PredName, Arity) do{ \
  SAVE_ERROR((Type), (Msg_UTF8), (Culprit), (ArgNo));                   \
  RAISE_ERROR((PredName), (Arity));                                     \
}while(0);

/*   --------------------------------------------------------------  */

#define CHECK_INTERP(Interp_data, Interp, TInterp, Argno) \
{ \
  if (Interp_data == NULL || \
      (Interp=Interp_data->interp) == NULL || \
      Interp_data->self != Interp_data) \
    SAVE_ERROR(DOMAIN_ERROR, "tcl_interpreter", TInterp, 1); \
}

#define GetInterp(TInterp, PInterp, Argno) \
{ \
  struct interp_data *gi_p = \
    (struct interp_data *)wrapper_to_ptr(local.atm_interp,(TInterp)); \
  CHECK_INTERP(gi_p, PInterp, TInterp, Argno); \
}
