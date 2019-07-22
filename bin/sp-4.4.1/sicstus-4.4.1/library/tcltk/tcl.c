/* Copyright(C) 1994-95, Swedish Institute of Computer Science */
#define SICSTUS_HIDDEN_API 1    /* [PM] 4.0 Needed for SP_reset_term_refs */
/*
[PM] 3.10.2 See http://www.tcl.tk/cgi-bin/tct/tip/66.html for some details on embedding Tcl/Tk. (There may be things there that we should do but don't).
 */

/*
cl -I'S:\sicstus\sicstus38p\include' -nologo  -MD  -Zi -Yd -GB -GF -Ox  -ID:/Progra~1/Tcl/include   -DSPDLL -MD  tcltk/tcl.c -c -Fox86-win32-nt-4/tcltk/tcl_d.obj
tcl.c
cl -I'S:\sicstus\sicstus38p\include' -nologo  -MD  -Zi -Yd -GB -GF -Ox  -ID:/Progra~1/Tcl/include   -DSPDLL -MD  tcltk/tk.c -c -Fox86-win32-nt-4/tcltk/tk_d.obj
tk.c
cl -I'S:\sicstus\sicstus38p\include' -nologo  -MD  -Zi -Yd -GB -GF -Ox  -ID:/Progra~1/Tcl/include   -DSPDLL -MD  tcltk/tkappini.c -c -Fox86-win32-nt-4/tcltk/tkappini_d.obj
tkappini.c
cl -I'S:\sicstus\sicstus38p\include' -nologo  -MD  -Zi -Yd -GB -GF -Ox  -ID:/Progra~1/Tcl/include   -DSPDLL -MD  tcltk/tkterm.c -c -Fox86-win32-nt-4/tcltk/tkterm_d.obj
tkterm.c
cl -I'S:\sicstus\sicstus38p\include' -nologo  -MD  -Zi -Yd -GB -GF -Ox  -ID:/Progra~1/Tcl/include   -DSPDLL -MD  tcltk/util.c -c -Fox86-win32-nt-4/tcltk/util_d.obj
util.c
*/


#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <stdio.h>
#include <string.h>
#include <tcl.h>
#include <sicstus/sicstus.h>
#include "tcl.h"

#if WINCE
#include <windows.h>
#endif

#define TCL_NUMVERSION (TCL_MAJOR_VERSION*10+TCL_MINOR_VERSION)

/*   --------------------------------------------------------------  */

/* Local and non local variable definitions */

/* [PM] 3.9b5 explicit init to force link-time conflict if multiply defines.

   Could have used local = {0}; but that will give a GCC warning about
   to few initializers which will break together with -Werror.

 */

struct local_state local = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                            0,0,
                            0,0,0,0,0,0,0,0,0,
#if SICSTUS_DBG
                            0,  /* old_err */
#endif /* SICSTUS_DBG */
                            0,0,0,0,
                            {0},  /* err_msg[] */
                            0,   /* err_argno */
                            0    /* err_culprit */
};

/*   --------------------------------------------------------------  */
/* Local function prototypes */

static int SPCDECL prolog_call _ANSI_ARGS_((ClientData clientData,
				  Tcl_Interp *interp, int argc, char **argv));

static int SPCDECL prolog_event _ANSI_ARGS_((ClientData clientData,
				  Tcl_Interp *interp, int argc, char **argv));

/*   --------------------------------------------------------------  */


/* Create and init a new interpreter */
/* Define some SICStus specific commands */

void SPCDECL tcl_new(SP_term_ref tInterp)
{
  char const * error_message;
  struct interp_data *interp_data;
  Tcl_Interp *interp;

  CLEAR_ERROR(); /* [PM] 4.3 */

  interp_data = (struct interp_data *)SP_malloc(sizeof(struct interp_data));

  if (interp_data == NULL
      || !(interp_data->interp = interp = Tcl_CreateInterp()))
    {
      if (interp_data != NULL)
        {
          SP_free(interp_data);
        }
      error_message = "Couldn't create Tcl interpreter";
      goto barf_;
    }
  interp_data->self = interp_data; /* Enables (unsafe) check */
  interp_data->event_list = NULL;
  interp_data->stream_buf = NULL;
  ptr_to_wrapper(local.atm_interp, interp_data, tInterp);

  Tcl_SetVar(interp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);

  if (Tcl_Init(interp) == TCL_ERROR) {
    error_message = Tcl_GetStringResult(interp);
    goto barf_;
  }

  Tcl_CreateCommand(interp, "prolog", (Tcl_CmdProc *)prolog_call,
		    (ClientData)interp_data, (Tcl_CmdDeleteProc *)NULL);
  Tcl_CreateCommand(interp, "prolog_event", (Tcl_CmdProc *)prolog_event,
		    (ClientData)interp_data, (Tcl_CmdDeleteProc *)NULL);
  
  return;
 barf_:
  SAVE_AND_RAISE_ERROR(SPTCL_ERROR, error_message, tInterp, 0, "tcl_new", 1);
}

/*   --------------------------------------------------------------  */

/* Delete an interpreter */

void SPCDECL tcl_delete_interp(SP_term_ref tInterp)
{
  struct interp_data *interp_data =
    (struct interp_data *)wrapper_to_ptr(local.atm_interp, tInterp);
  Tcl_Interp *interp;

  CLEAR_ERROR(); /* [PM] 4.3 */  

  CHECK_INTERP(interp_data, interp, tInterp, 1);

#if 0                           /* [PM] 4.0 no stream caching yet */
  if (interp_data->stream)
    {
      (void)SP_fclose(interp_data->stream, SP_FCLOSE_OPTION_FORCE);
      interp_data->stream = NULL;
    }
#endif  /* 0 */

  {
    struct event_q *p=interp_data->event_list, *q;

    for (; p; p=q)
      {
	q = p->next;
	SP_free(p);
      }
  }

  Tcl_DeleteInterp(interp);  

  if (interp_data->stream_buf != NULL)
    {
      SP_free(interp_data->stream_buf);
      interp_data->stream_buf = NULL; /* DBG, not needed */
    }
  SP_free(interp_data);
  return;

  RAISE_ERROR("tcl_delete", 1);
}

/*   --------------------------------------------------------------  */

/*
 *  
 *  PROLOG CALL:  tcl_eval()
 *  
 *  This routine should now be reentrant, i.e. you can call
 *  Tcl that call Prolog that call Tcl....
 *  
 *  '*current_event' is a pointer to the head of a chain of
 *  events. A new event has the initial value of [],
 *  i.e. nil. If Prolog is called recursively 'current_event'
 *  has to be stored. After the call it should be restored.
 *  
 */

void SPCDECL tcl_eval(SP_term_ref tInterp, SP_term_ref tScript, SP_term_ref tTerm)
{
  char const *error_message;

  int code;
  size_t len;
  char const *script;
  char *script_copy;
  char buf[BUFLEN];
  struct interp_data *interp_data =
    (struct interp_data *)wrapper_to_ptr(local.atm_interp, tInterp);
  Tcl_Interp *interp;

  CLEAR_ERROR(); /* [PM] 4.3 */  

  CHECK_INTERP(interp_data, interp, tInterp, 1);

  if ((script = translate_command(tScript, interp_data)) == NULL)
    {
      goto error;
    }
 
  len = strlen(script)+1;
  BUF_ALLOC(len, BUFLEN, buf, {goto memerr;}, script_copy);
  memcpy(script_copy, script, len);

  code = Tcl_Eval(interp, script_copy);

  BUF_FREE(buf, script_copy);

  switch (code)
    {
    case TCL_OK:
    case TCL_RETURN:
    case TCL_BREAK:
    case TCL_CONTINUE:
      {
	SP_term_ref tail = SP_new_term_ref();

	SP_put_atom(tail, local.atm_nil); /* End of list */
	SP_put_list_codes(tTerm, tail, Tcl_GetStringResult(interp));
	break;
      }
    case TCL_ERROR:
    default:			/* Could there be others? */
      error_message = Tcl_GetStringResult(interp);
      goto barf_;
    }

  return;

memerr:
  error_message = "Couldn't allocate memory";
  goto barf_;

 barf_:
  SAVE_AND_RAISE_ERROR(SPTCL_ERROR, error_message, tInterp, 0, "tcl_eval", 3);
}

/*   --------------------------------------------------------------  */

/*
 *  
 *  PROLOG CALL:  tcl_event()
 *  
 *  Similar to tcl_eval() above
 *  
 */

void SPCDECL tcl_event(SP_term_ref tInterp, SP_term_ref tScript, SP_term_ref tEvent)
{
  int code;
  size_t len;
  char const *script;
  char *script_copy;
  char buf[BUFLEN];
  struct interp_data *interp_data =
    (struct interp_data *)wrapper_to_ptr(local.atm_interp,(tInterp));
  Tcl_Interp *interp;

  CLEAR_ERROR(); /* [PM] 4.3 */

  CHECK_INTERP(interp_data, interp, tInterp, 1);

  if ((script = translate_command(tScript, interp_data)) == NULL)
    {
      goto error;
    }

  len = strlen(script)+1;
  BUF_ALLOC(len, BUFLEN, buf, {goto memerr;}, script_copy);
  memcpy(script_copy, script, len);

  code = Tcl_Eval(interp, script_copy);
  BUF_FREE(buf, script_copy);

  switch (code)
    {
    case TCL_OK:
      if (put_event_queue(interp_data, TRUE, tEvent) >= 0)
	return;
      goto error;
    case TCL_ERROR:
    default:
      SAVE_ERROR(SPTCL_ERROR, Tcl_GetStringResult(interp), tInterp, 0);
    }
 memerr:
  SAVE_ERROR(SPTCL_ERROR, "Couldn't allocate memory", tInterp, 0);
  RAISE_ERROR("tcl_event", 3);
}

/*   --------------------------------------------------------------  */
/*   --------------------------------------------------------------  */
/*   --------------------------------------------------------------  */

/*
 *
 *  TCL COMMAND:  prolog_call "goal"
 *  
 */

static int SPCDECL prolog_call(ClientData clientData, Tcl_Interp *interp, int argc, char **argv)
{
  struct interp_data *interp_data = (struct interp_data *)clientData;
  SP_stream *stream;
  SP_term_ref tStream;
  SP_term_ref tInterp;
  int tcl_res = TCL_OK;
  int res;
  
  if (argc != 2)
    {
      Tcl_AppendResult(interp, "Wrong number of arguments: should be \"",
		       argv[0], " term\"", (char *)NULL);
      return TCL_ERROR;
    }
#if SICSTUS_RELEASE_BUILD  && !FORCE_BUILD && SICSTUS_VERSION > 40401 /* [PM] 4.3.1 postpone */
#error "Should re-use the functionality of the SP_read_from_string stream (which then needs a local copy of the string, xref codesio too)"
#endif  /* SICSTUS_TODO */

  if ((stream = get_tcl_stream(interp_data, argv[1])) == NULL)
    {
      Tcl_AppendResult(interp, "Couldn't allocate memory", (char *)NULL);
      return TCL_ERROR;
    }
  tStream = SP_new_term_ref();
  /* All returns below this point should pass through cleanup: */

  if (!SP_put_address(tStream, (void *)stream)) goto unexpected_error;
  tInterp = SP_new_term_ref();
  ptr_to_wrapper(local.atm_interp, interp_data, tInterp);


  /* Because we copy the result to Tcl strings we can reclaim */
  /* all heap storage that we have used */
  res = SP_query_cut_fail(local.call_pred, tStream, tInterp);

  switch (res)
    {
    case SP_SUCCESS:
      /* [PM] May 2000, was interp->result = "1"; */
      Tcl_SetResult(interp, "1", TCL_STATIC);
      goto cleanup;
    default:
      SP_ASSERT(0);
      /* FALLTHROUGH */
    case SP_FAILURE:
      /* [PM] May 2000, was interp->result = "0"; */
      Tcl_SetResult(interp, "0", TCL_STATIC);
      goto cleanup;
    case SP_ERROR:
      {
        char const *excpstr;
        SP_term_ref tExcp;
        
        tExcp = SP_new_term_ref();

        if(SP_exception_term(tExcp)
           &&
           (excpstr = trans_term(tExcp, interp_data)) != NULL)
          {
            ; /* empty, excpstr already set-up */
          }
        else                    /* some failure */
          {
          unexpected_error:
            excpstr = "unexpected error in " __FILE__;
          }

        Tcl_AppendResult(interp,"Exception during Prolog execution: ",
                         argv[1], "  ", excpstr, (char *)NULL);

        tcl_res = TCL_ERROR;
        goto cleanup;
      }
    }
 cleanup:
  SP_reset_term_refs(tStream);
  return tcl_res;
}

/*   --------------------------------------------------------------  */

/*
 *
 *  TCL COMMAND:  prolog_event "term"
 *  
 */

static int SPCDECL prolog_event(ClientData clientData, Tcl_Interp *interp, int argc, char **argv)
{
  struct interp_data *interp_data = (struct interp_data *)clientData;
  struct event_q *p;
  int i;

  (void)interp;

  for (i = 1; i < argc; i++)
    {
      size_t len = strlen(argv[i]);
      LAZY_NULL_CHECK(p = (struct event_q *)SP_malloc(len + sizeof(struct event_q))); /* event_q size includes space for NUL */
      memcpy(p->event_string, argv[i], len +1);
      p->next = interp_data->event_list;
      interp_data->event_list = p;
    }
  return TCL_OK;
}


/*   --------------------------------------------------------------  */

void SPCDECL tcl_add_result(SP_term_ref tInterp, SP_term_ref tVarName, SP_term_ref tResult)
{
  char const *varName;
  struct interp_data *interp_data;
  Tcl_Interp *interp;
  char const *varval = NULL;

  CLEAR_ERROR(); /* [PM] 4.3 */

  if (!SP_get_string(tVarName, &varName))
    {
      goto unexpected_error;
    }
  /* [PM] Make it possible to ignore results not in the "special command format" */
  if (( varName[0] == '_'        /* 3.8.5 used to be too restrictive (strcmp(varName, "_") == 0) */
        || SP_is_variable(tResult) )) /* 3.8.5 as per GvN feedback */
    {
      return;
    }

  interp_data = (struct interp_data *)wrapper_to_ptr(local.atm_interp,tInterp);
  SP_ASSERT(interp_data != NULL);
  CHECK_INTERP(interp_data, interp, tInterp, 1);

  if ((varval = trans_command(tResult, interp_data)) == NULL)
    {
      SAVE_ERROR(SPTCL_ERROR, "Incorrect command format", tResult, 3);
    }

#if TCL_MAJOR_VERSION > 8 || ( TCL_MAJOR_VERSION == 8 && TCL_MINOR_VERSION >= 4) /* >= 8.4 */
  /* [PM] 4.0 const should be OK in 8.4 */
  Tcl_SetVar2(interp, "prolog_variables", varName, varval, 0);
#else  /* < 8.4 */
  /* [PM] 4.0 Need to cast away const-ness since Tcl_SetVar has incorrect (const-less) prototype before 8.4 */
  Tcl_SetVar2(interp, "prolog_variables", (char *)varName, (char *)varval, 0);
#endif  /* < 8.4 */

  return;

 unexpected_error:
  SAVE_ERROR(SPTCL_ERROR, "Unexpected error", tResult, 0);

  RAISE_ERROR("$tcl_add_result", 3);
}

/*   --------------------------------------------------------------  */

void SPCDECL tcl_initialize(int when)
{
#if WINCE
  #error "[PM] 3.11.2 We should reimplement the WinCE port"
  HMODULE celib = LoadLibrary(_T("celib.dll"));
  void (*xceinit)(const wchar_t*)  = (void*) GetProcAddress(celib, _T("xceinit"));
  // Error handling!
  xceinit(_T(""));
  // The port of tcltk we use needs the celib porting library, which is not used by SICStus
#endif  /* WINCE */

  CLEAR_ERROR(); /* [PM] 4.3 */

  (void)when;
  local.tcl_no_registry_check = 0;
  (void)SP_register_atom(local.atm_nil = SP_atom_from_string("[]"));
  (void)SP_register_atom(local.atm_period = SP_atom_from_string("."));
  (void)SP_register_atom(local.atm_true = SP_atom_from_string("true"));
  (void)SP_register_atom(local.atm_false = SP_atom_from_string("false"));
  (void)SP_register_atom(local.atm_interp = SP_atom_from_string("$TclInterp"));
  (void)SP_register_atom(local.atm_write = SP_atom_from_string("write"));
  (void)SP_register_atom(local.atm_writeq = SP_atom_from_string("writeq"));
  (void)SP_register_atom(local.atm_write_canonical = SP_atom_from_string("write_canonical"));
  (void)SP_register_atom(local.atm_format = SP_atom_from_string("format"));
  (void)SP_register_atom(local.atm_codes = SP_atom_from_string("codes"));
  (void)SP_register_atom(local.atm_chars = SP_atom_from_string("chars"));
  (void)SP_register_atom(local.atm_br = SP_atom_from_string("br"));
  (void)SP_register_atom(local.atm_dq = SP_atom_from_string("dq"));
  (void)SP_register_atom(local.atm_sqb = SP_atom_from_string("sqb"));
  (void)SP_register_atom(local.atm_min = SP_atom_from_string("min"));
  (void)SP_register_atom(local.atm_dot = SP_atom_from_string("dot"));
  (void)SP_register_atom(local.atm_list = SP_atom_from_string("list"));
  (void)SP_register_atom(local.atm_term = SP_atom_from_string("term"));

  local.call_pred = SP_predicate("call_from_tcl", 2, "tcltk");
  if (local.call_pred == NULL)
    goto existence_error;

  local.read_pred = SP_predicate("read_sc", 2, "tcltk");
  if (local.read_pred == NULL)
    goto existence_error;

  local.write_pred = SP_predicate("write_sc", 2, "tcltk");
  if (local.write_pred == NULL)
    goto existence_error;

  local.writeq_pred = SP_predicate("writeq_sc", 2, "tcltk");
  if (local.writeq_pred == NULL)
    goto existence_error;

  local.write_canonical_pred = SP_predicate("write_canonical_sc", 2, "tcltk");
  if (local.write_canonical_pred == NULL)
    goto existence_error;

  local.format_pred = SP_predicate("format_sc", 3, "tcltk");
  if (local.format_pred == NULL)
    goto existence_error;

 /* [PM] 1. Ensure init.tcl is found by Tcl_Init.
         2. Ensure non-ASCII encodings are set up correctly.
 */
  Tcl_FindExecutable("");       /* [PM] arg should be argv[0] but how? */

#if TCL_NUMVERSION >= 75 && !DO_NOT_CREATE_DUMMY_INTERPRETER
/* Tcl7.5 (and up?) has the bad habit of closing stdin etc when the
   last interpreter is deleted. To avoid this we make an interpreter
   that is never deleted. Preserve as an extra safety measure.
*/
  Tcl_Preserve((ClientData)Tcl_CreateInterp());
#endif

  return;
 existence_error:
  /* [PM] 4.0 FIXME: EXISTENCE_ERROR|EXIST_PREDICATE is not handled correctly */
  SAVE_ERROR(EXISTENCE_ERROR|EXIST_PREDICATE, "", 0, 0);
  RAISE_ERROR("tcl_initialize", 0);
}

void SPCDECL tcl_deinitialize(int when)
{
  (void)when;

  
  Tcl_Finalize();               /* [PM] 3.11.2 SPRM 7990 */

  tcl_stream_deinit();

  (void)SP_unregister_atom(local.atm_nil);
  (void)SP_unregister_atom(local.atm_period);
  (void)SP_unregister_atom(local.atm_true);
  (void)SP_unregister_atom(local.atm_false);
  (void)SP_unregister_atom(local.atm_interp);
  (void)SP_unregister_atom(local.atm_write);
  (void)SP_unregister_atom(local.atm_writeq);
  (void)SP_unregister_atom(local.atm_write_canonical);
  (void)SP_unregister_atom(local.atm_format);
  (void)SP_unregister_atom(local.atm_codes);
  (void)SP_unregister_atom(local.atm_chars);
  (void)SP_unregister_atom(local.atm_br);
  (void)SP_unregister_atom(local.atm_dq);
  (void)SP_unregister_atom(local.atm_sqb);
  (void)SP_unregister_atom(local.atm_min);
  (void)SP_unregister_atom(local.atm_dot);
  (void)SP_unregister_atom(local.atm_list);
  (void)SP_unregister_atom(local.atm_term);
}

