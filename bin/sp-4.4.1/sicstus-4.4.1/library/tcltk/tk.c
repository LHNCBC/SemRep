/* Copyright(C) 1994-95, Swedish Institute of Computer Science */
/* [PM] 3.9.0 Tru64 FD_ZERO uses bzero without ensuring bzero is
   declared. Defining _OSF_SOURCE ensures that bzero is declared in
   <string.h> */
#ifndef _OSF_SOURCE
#define _OSF_SOURCE
#endif


#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */
#include <stdio.h>
#include <string.h>

#include <sicstus/sicstus.h>
#include <sicstus/config.h>     /* ([PM] 4.2.1 this also defines SICSTUS_DBG) */

#if HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#if HAVE_SYS_TIME_H
#include <sys/time.h>           /* select et al in SUSv2 */
#endif

#if HAVE_UNISTD_H
#include <unistd.h>             /* [PM] 3.9.1 needed for select on MacOS X 10.1, it seems. */
#endif

#if HAVE_SYS_FILE_H
#include <sys/file.h>
#endif


#if SP_WIN32
#include <windows.h>
#endif


#if HAVE_STRINGS_H
#include <strings.h>            /* [PM] 3.9.1 for bzero which AIX 4.3.3 uses to define FD_ZERO */
#endif /* HAVE_STRINGS_H */

#if (__APPLE__ && __MACH__)     /* 3.10.1 MacOS X */

#define Cursor X11_Cursor       /* [PM] 3.10.1 needed to hide X11 Cursor type from the Quickdraw.h Cursor type */

#endif /* (__APPLE__ && __MACH__) */

#include <tk.h>

#if (__APPLE__ && __MACH__)     /* 3.10.1 MacOS X */

#undef Cursor

#endif /* (__APPLE__ && __MACH__) */

/* [PM] after config.h */
#if (__APPLE__ && __MACH__)     /* 3.10.1 MacOS X */


#if USE_TransformProcessType    /* [PM] 3.11.1 */
#include <ApplicationServices/ApplicationServices.h>

#ifndef CHECK_IF_BUNDLED
#define CHECK_IF_BUNDLED 1
#endif

#elif USE_CPSEnableForegroundOperation

#include <CoreFoundation/CoreFoundation.h>     /* UInt32 */
#include <mach-o/dyld.h>        /* NSModule functions */

#ifndef CHECK_IF_BUNDLED
#define CHECK_IF_BUNDLED 1
#endif
#endif /* USE_CPSEnableForegroundOperation */


#if CHECK_IF_BUNDLED
#if 0 /* [PM] This is not compatible with the X headers included by Tk
         (/Library/Frameworks/Tk.framework/Headers/X11/X.h:53:
         conflicting types for `Cursor') */

#include <Carbon/Carbon.h>      /* [PM] 3.10.1 ProcessSerialNumber() */
#else
#include <ApplicationServices/ApplicationServices.h> /* [PM] 3.10.1 ProcessSerialNumber() (via HIServices/Processes.h) */
#endif /* 0 */
#endif /* CHECK_IF_BUNDLED */


#endif /* (__APPLE__ && __MACH__) */

#include "tcl.h"

#if HAVE__SNPRINTF && !HAVE_SNPRINTF /* Win32 */
/* #undef snprintf */
#define snprintf _snprintf
#endif

static int SPCDECL tk_idle_hook(void *cookie);

/*   --------------------------------------------------------------  */

/*
  [PM] 4.3.3 It appears that OS X Tcl/Tk now (at least since Mac OS X
  10.5) does its own call to TransformProcessType et al. so we no
  longer need to. For this reason configure.in disables
  USE_TransformProcessType (and USE_CPSEnableForegroundOperation).
*/

#if (__APPLE__ && __MACH__)     /* 3.10.1 MacOS X */

#if MACOSX_TCLTK_FRAMEWORK

#if SICSTUS_DBG
#define ERROR_CODE(ECODE) do{fprintf(stderr, "ERROR: %s:%d error==%d\n", __FILE__, (int)__LINE__, (int)(ECODE));fflush(stderr);}while(0)
#define WARNING_CODE(ECODE) do{fprintf(stderr, "WARNING: %s:%d error==%d\n", __FILE__, (int)__LINE__, (int)(ECODE));fflush(stderr);}while(0)
#define GOTO_BARF do{ERROR_CODE(err); goto barf;}while(0)
#define WARN_ERR  WARNING_CODE(err)

#else  /* !SICSTUS_DBG */
#define ERROR_CODE(ECODE)
#define WARNING_CODE(ECODE)
#define GOTO_BARF goto barf
#define WARN_ERR
#endif /* !SICSTUS_DBG */


#if USE_CPSEnableForegroundOperation

/* Portions of CPS.h */
typedef struct CPSProcessSerNum
{
        UInt32          lo;
        UInt32          hi;
} CPSProcessSerNum;

extern OSErr    CPSGetCurrentProcess( CPSProcessSerNum *psn);
extern OSErr    CPSEnableForegroundOperation( CPSProcessSerNum *psn, UInt32 _arg2, UInt32 _arg3, UInt32 _arg4, UInt32 _arg5);
typedef OSErr   (*CPSEnableForegroundOperationPtr)( CPSProcessSerNum *psn, UInt32 _arg2, UInt32 _arg3, UInt32 _arg4, UInt32 _arg5);
extern OSErr    CPSSetFrontProcess( CPSProcessSerNum *psn);
extern OSErr    CPSSetProcessName(CPSProcessSerNum *psn, char const *name);


static OSErr MyCPSEnableForegroundOperation(CPSProcessSerNum const *psn)
{
  char const *name = "_CPSEnableForegroundOperation";
  static CPSEnableForegroundOperationPtr cpsenableforegroundoperation = NULL;

  if (!cpsenableforegroundoperation)
    {
      if (NSIsSymbolNameDefined(name))
        {
          void *sym_addr;
          sym_addr = NSAddressOfSymbol(NSLookupAndBindSymbol(name));
          if (sym_addr)
            {
              cpsenableforegroundoperation = (CPSEnableForegroundOperationPtr)sym_addr;
            }
        }
    }
  if (cpsenableforegroundoperation)
    {
      /* [PM] 3.10.1 The arguments are from the net, no docs available anywhere */
      return (*cpsenableforegroundoperation)(psn,0x03,0x3C,0x2C,0x1103);
    }
  else
    {
      return -1;                /* CPSEnableForegroundOperationPtr not available */
    }
}

static int Aqua_EnableForegroundoperationCPS(char const *app_name)
{
  static int CPSEnableForegroundOperation_already_called = 0;
  CPSProcessSerNum psn;
  OSErr err = 0;
  if ((err = CPSGetCurrentProcess(&psn)) != 0) GOTO_BARF;
  if ((err = CPSSetProcessName(&psn, app_name)) != 0) GOTO_BARF;

  if (!CPSEnableForegroundOperation_already_called)
    {
      if ((err = MyCPSEnableForegroundOperation(&psn)) != 0)
        {
          /* [PM] 3.10.1 if CPSEnableForegroundOperation called more
             than once. Net wisdom thinks this is harmless. (Should
             not happen to us, though, unless the foreign resource is
             unloaded so that
             CPSEnableForegroundOperation_already_called gets
             reset. */
          if (err == 1010)   /* (1010 is kCGErrorInvalidOperation?) */
            {
              WARN_ERR;
              err = 0;
            }
          else
            {
              GOTO_BARF;
            }
        }
      CPSEnableForegroundOperation_already_called = 1;
    }
  /* [PM] 3.10.1 FIXME: Should we do this or not? I do not know what
     this does, this is what the code floating on the net does. */
  if ((err = CPSSetFrontProcess(&psn)) != 0) GOTO_BARF;

  return 0;
 barf:
  return err;

}

#endif /* USE_CPSEnableForegroundOperation */

#if USE_TransformProcessType
static int Aqua_EnableForegroundoperationTPT(char const *app_name)
{
  static int already_called = 0;
  OSStatus status = 0;
  ProcessSerialNumber const psn = { 0, kCurrentProcess };

  (void)app_name;

  if (!already_called)
    {
      already_called = 1; 

      status=TransformProcessType(&psn, kProcessTransformToForegroundApplication);
      (void)status;
      if (status!=noErr)
        {
          WARNING_CODE(status);
        }
    }
  return 0;
}
#endif /* USE_TransformProcessType */


/*
From Apple Carbon Tips and Tricks:

How to determine if my app is bundled? 

Here is a snippet of code which returns true if the currently running
application is running as a bundle. This code will work on Mac OS X as
well as CarbonLib 1.5 and later too (assuming you're running on 9.0 or
later), but for 8.6 you will need to use the techniques from Tech Note
2015.
*/

#if CHECK_IF_BUNDLED
static int iAmBundled(void)            /* a.k.a AmIBundled */
{
  FSRef processRef;
  FSCatalogInfo processInfo;
  int isBundled;
  OSErr err;
  ProcessSerialNumber psn = {0, kCurrentProcess};

  err = GetProcessBundleLocation(&psn, &processRef);
  if (err != noErr)
    {
      ERROR_CODE(err);
      return 0;
    }
  /*
    [PM] 4.2.3 FSGetCatalogInfo is deprecated in 10.8.
    Could we use CFBundleCopyBundleURL() on the result from CFBundleGetMainBundle()?
   */
  err = FSGetCatalogInfo(&processRef, kFSCatInfoNodeFlags, 
                         &processInfo, NULL, NULL, NULL);
  if (err != noErr)
    {
      ERROR_CODE(err);
      return 0;
    }
  isBundled = processInfo.nodeFlags & kFSNodeIsDirectoryMask;
  #if SICSTUS_DBG
  fprintf(stderr, "DBG: %s:%d isBundled == %d\n", __FILE__, (int)__LINE__, (int)isBundled);fflush(stderr);
  #endif/* SICSTUS_DBG */

  return isBundled;
}
#else  /* !CHECK_IF_BUNDLED */
#define iAmBundled() 0          /* dummy */
#endif /* !CHECK_IF_BUNDLED */

#if USE_CPSEnableForegroundOperation || USE_TransformProcessType
/* [PM] 3.10.1 Allow Aqua windows created by Aqua Tk to be brought to
   the front (e.g., when clicked on). Failure to perform this hack
   will give error messages like SetFrontProcess
   See internals.tex */
static int Aqua_EnableForegroundoperation(char const *app_name)
{

  #if USE_CPSEnableForegroundOperation
  return Aqua_EnableForegroundoperationCPS(app_name);
  #elif USE_TransformProcessType
  return Aqua_EnableForegroundoperationTPT(app_name);
  #else
#error "cannot happen"
  (void)app_name;
  return 0;
  #endif
}
#endif /* USE_CPSEnableForegroundOperation || USE_TransformProcessType */

#endif  /* MACOSX_TCLTK_FRAMEWORK */

#endif /* (__APPLE__ && __MACH__) */


/*   --------------------------------------------------------------  */

#define PROLOG_TCL_NEW_FLAG_TOP_LEVEL_EVENTS  0x0001
#define PROLOG_TCL_NEW_FLAG_DISABLE_AQUA_HACK 0x0002
#define PROLOG_TCL_NEW_FLAG_ENABLE_AQUA_HACK  0x0004

void tk_new(
            SP_term_ref tInterp,
            char SP_FLI_CONST *app_name,
            char SP_FLI_CONST *screen_name,
            SP_integer flags
            )
{
  Tcl_Interp *interp;
  CLEAR_ERROR(); /* [PM] 4.3 */

  GetInterp(tInterp, interp, 1);

  if (screen_name[0] == '\0') {
#if SP_WIN32
    /* Necessary ? */
    screen_name = "localhost:0";
#else
    screen_name = NULL;
#endif
  }

  if (Tcl_AppInit(interp) == TCL_ERROR) {
    SAVE_ERROR(SPTK_ERROR, Tcl_GetStringResult(interp), tInterp, 0);
  }

  {
    int tcl_argc = 1;
    char tcl_argv[256], *tap = tcl_argv;
    size_t tap_size = (sizeof tcl_argv);
    char buf[8];

    tcl_argv[0] = '\0';

    if (screen_name)
      {
        size_t tmp;

	tcl_argc += 2;
	snprintf(tap, tap_size, "-display %s", screen_name);
        tap[tap_size-1] = '\0';
        tmp = strlen(tap);
        tap_size -= tmp;
	tap += tmp;
      }
    if (app_name[0] != '\0'
        &&
        tap_size > 0)
      {
	tcl_argc += 2;
	snprintf(tap, tap_size, " -name \"%s\"", app_name);
        tap[tap_size-1] = '\0';
      }
    snprintf(buf, (sizeof buf), "%d", tcl_argc);
    Tcl_SetVar(interp, "argc", buf, TCL_GLOBAL_ONLY);
    Tcl_SetVar(interp, "argv", tcl_argv, TCL_GLOBAL_ONLY);
  }

  if (Tk_Init(interp) == TCL_ERROR)
    {
      SAVE_ERROR(SPTK_ERROR, Tcl_GetStringResult(interp), tInterp, 0);
    }

  if (flags & PROLOG_TCL_NEW_FLAG_TOP_LEVEL_EVENTS)
    {
      SP_install_idle_hook(tk_idle_hook, NULL, SPIO_OPTION_NONE);
    }

#if (__APPLE__ && __MACH__)     /* 3.10.1 MacOS X */
  {
#if MACOSX_TCLTK_FRAMEWORK
#if (USE_CPSEnableForegroundOperation || USE_TransformProcessType)
    {
      int disable_aqua_hack = ((flags & PROLOG_TCL_NEW_FLAG_DISABLE_AQUA_HACK)
                               ||
                               getenv("SP_DISABLE_TCLTK_AQUA_HACK")/* another way to disable the hack */
                               );
      int enable_aqua_hack = ((flags & PROLOG_TCL_NEW_FLAG_ENABLE_AQUA_HACK)
                              ||
                              getenv("SP_ENABLE_TCLTK_AQUA_HACK") /* if explicitly requested  */
                              );
      
      {
        int const enable_default = 0; /* [PM] 4.0 Default off. No longer needed with MacOS X 10.4 Tcl/Tk */
        int enable = enable_default;

        if (disable_aqua_hack) enable = 0;
        if (enable_aqua_hack) enable = 1;

        if (enable
            &&
            !iAmBundled() /* do it only if not bundled (since bundled app do not need it) */
            )
          {
            Aqua_EnableForegroundoperation(app_name);
          }
      }
    }
#endif /* (USE_CPSEnableForegroundOperation || USE_TransformProcessType) */
#endif/* MACOSX_TCLTK_FRAMEWORK */
  }
#endif /* (__APPLE__ && __MACH__) */


  local.tk_inited = 1;
  return;
  RAISE_ERROR("tk_new", 2);
}

/*   --------------------------------------------------------------  */

void tk_main_window(tInterp, tWindow)
     SP_term_ref tInterp;
     SP_term_ref tWindow;
{
  Tcl_Interp *interp;
  Tk_Window window;
  CLEAR_ERROR(); /* [PM] 4.3 */
  GetInterp(tInterp, interp, 1);

  if (!local.tk_inited)               /* [PM] May 2000 Workaround Tcl Stub vector not being initialized in Tk (PRM 1493) */
    {
      SAVE_ERROR(SPTK_ERROR, "Not a Tk enabled Tcl interpreter", tInterp, 0);
    }

  window = Tk_MainWindow(interp);
  ptr_to_wrapper(local.atm_window, window, tWindow);
  return;
  RAISE_ERROR("tk_main_window", 2);
}

/*   --------------------------------------------------------------  */

void tk_destroy_window(tWindow)
     SP_term_ref tWindow;
{
  Tk_Window window = (Tk_Window)wrapper_to_ptr(local.atm_window,tWindow);
  CLEAR_ERROR(); /* [PM] 4.3 */

  if (window == NULL || !local.tk_inited) /* [PM] !tk_inited for PRM 1493 */
    SAVE_ERROR(DOMAIN_ERROR, "tk_window", tWindow, 1);

  Tk_DestroyWindow(window);
  return;
  RAISE_ERROR("tk_destroy_window", 1);
}

/*   --------------------------------------------------------------  */

void tk_make_window_exist(tWindow)
     SP_term_ref tWindow;
{
  Tk_Window window = (Tk_Window)wrapper_to_ptr(local.atm_window,tWindow);;
  CLEAR_ERROR(); /* [PM] 4.3 */

  if (window == NULL || !local.tk_inited) /* [PM] !tk_inited for PRM 1493 */
    SAVE_ERROR(DOMAIN_ERROR, "tk_window", tWindow, 1);

  Tk_MakeWindowExist(window);
  return;
  RAISE_ERROR("tk_make_window_exist", 1);
}

/*   --------------------------------------------------------------  */

/* [PM] 4.3 Platform independent. Passed from tcltk.pl */
#define PROLOG_TCL_DONT_WAIT     0x0002
#define PROLOG_TCL_WINDOW_EVENTS 0x0004
#define PROLOG_TCL_FILE_EVENTS   0x0008
#define PROLOG_TCL_TIMER_EVENTS  0x0010
#define PROLOG_TCL_IDLE_EVENTS   0x0020
/* [PM] 4.3 Corresponds to TCL_ALL_EVENTS. Can be combined with PROLOG_TCL_DONT_WAIT. */
#define PROLOG_TCL_ALL_EVENTS    0x0001

#define TRANSLATE_FLAG(X, PL_FLAG, TCL_FLAG) (((X) & (PL_FLAG)) != 0 ? (TCL_FLAG) : 0)

static SP_integer prolog_tk_do_one_event(SP_integer pl_flags, SP_integer tcl_flags_)
{
  int tcl_flags = (int) tcl_flags_;

  tcl_flags |= TRANSLATE_FLAG(pl_flags, PROLOG_TCL_WINDOW_EVENTS, TCL_WINDOW_EVENTS);
  tcl_flags |= TRANSLATE_FLAG(pl_flags, PROLOG_TCL_FILE_EVENTS, TCL_FILE_EVENTS);
  tcl_flags |= TRANSLATE_FLAG(pl_flags, PROLOG_TCL_TIMER_EVENTS, TCL_TIMER_EVENTS);
  tcl_flags |= TRANSLATE_FLAG(pl_flags, PROLOG_TCL_IDLE_EVENTS, TCL_IDLE_EVENTS);
  tcl_flags |= TRANSLATE_FLAG(pl_flags, PROLOG_TCL_ALL_EVENTS, TCL_ALL_EVENTS);
  /* Note: TCL_DONT_WAIT is not an event in itself (and not included
     in TCL_ALL_EVENTS) */
  tcl_flags |= TRANSLATE_FLAG(pl_flags, PROLOG_TCL_DONT_WAIT, TCL_DONT_WAIT);

  return Tcl_DoOneEvent(tcl_flags);
}


SP_integer tk_do_one_event2(SP_integer pl_flags, SP_integer tcl_flags)
{
  return prolog_tk_do_one_event(pl_flags, tcl_flags);
}


void tk_do_one_event4(SP_term_ref tInterp, SP_integer pl_flags, SP_integer tcl_flags, SP_term_ref tEvent)
{
  struct interp_data *interp_data =
    (struct interp_data *)wrapper_to_ptr(local.atm_interp, tInterp);
  CLEAR_ERROR(); /* [PM] 4.3 */

  if (interp_data == NULL)
    SAVE_ERROR(DOMAIN_ERROR, "tcl_interpreter", tInterp, 1);

  switch (put_event_queue(interp_data, FALSE, tEvent))
    {
    case 1:
      return;
    case 0:
      prolog_tk_do_one_event(pl_flags, tcl_flags);
      if (put_event_queue(interp_data, FALSE, tEvent) >= 0)
        return;
    case -1:
      RAISE_ERROR("tk_do_one_event", 3);
    }
}


/*   --------------------------------------------------------------  */

static int SPCDECL tk_idle_hook(void *cookie)
{
  (void)cookie;                 /* not used (but we will need it if we make this library MULTI_SP_AWARE) */
#if SICSTUS_DBG && 0
  fprintf(stderr, "tk_idle_hook called..");fflush(stderr);
#endif  /* SICSTUS_DBG */

  if (Tk_GetNumMainWindows() > 0
      && Tcl_DoOneEvent(TCL_ALL_EVENTS|TCL_DONT_WAIT))
    {
#if SICSTUS_DBG && 0
      fprintf(stderr, "tk_idle_hook called..");fflush(stderr);
      fprintf(stderr, "..some work done\n");fflush(stderr);
#endif  /* SICSTUS_DBG */
      return 1;                 /* some work done */
    }
  else
    {
#if SICSTUS_DBG && 0
      fprintf(stderr, "..no work done\n");fflush(stderr);
#endif  /* SICSTUS_DBG */
      return 0;                 /* no work done */
    }
}


/*   --------------------------------------------------------------  */

void tk_num_main_windows(SP_term_ref tNum)
{
  if (!local.tk_inited) /* [PM] !tk_inited for PRM 1493 */
    {
      SP_put_integer(tNum, 0);
    }
  else
    {
      SP_put_integer(tNum, Tk_GetNumMainWindows());
    }
}

/*   --------------------------------------------------------------  */

void SPCDECL tk_initialize(int when)
{
  local.tk_inited = 0;

  (void)SP_register_atom(local.atm_window = SP_atom_from_string("$TkWindow"));

  tcl_initialize(when);
}

void SPCDECL tk_deinitialize(int when)
{

  (void)SP_install_idle_hook(tk_idle_hook, NULL, SP_INSTALL_IDLE_HOOK_OPTION_REMOVE);
  
  tcl_deinitialize(when);
  (void)SP_unregister_atom(local.atm_window);
}

