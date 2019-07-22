/*
 * Copyright (c) 1998-99 SICS 
 */
#define SICSTUS_HIDDEN_API 1
#define SPIO_INCLUDE_OS_TYPES 1 /* for Win32 DebugBreak() */
/* [PD] threadsafe jasper-server hack */
#define THREADSERVER 1

/* [PM] 3.8.6 On Solaris and Linux JDK 1.3 and 1.3.1 beta MonitorExit
   will sometimes return -1 when there is a pending exception. 
   This is tracked as
   <http://developer.java.sun.com/developer/bugParade/bugs/4458083.html>.
   (It turns out that the book may be incorrect since the spec does
    not mention MonitorExit as being safe to call).

   I sent (almost) the following bug report:

     There appears to be a bug in the Hotspot JNI function MonitorExit in
     JDK 1.3 (at leaston Linux, I have not found the Hotspot source for
     1.3.0) and JDK 1.3.1 (certainly on Linux but from the sources it seems
     to be platform independent).

     The documentation (The Java Native Interface by S. Liang) says that
     MonitorExit is one of the few JNI functions that can safely be called
     while an exception is pending (p. 162). Furthermore it says that
     MonitorExit "Returns a negative number if and only if an invocation of
     this function has thrown an exception." (p. 259). I take this to mean
     that MonitorExit should not return a negative number just because
     there is an exception pending.

     The observed behavior is that if there is an exception pending then
     MonitorExit will return -1. If my code is changed to do an
     ExceptionClear() before calling MonitorExit() then zero is returned as
     expected (i.e., there is nothing wrong with the call to MonitorExit()).

     The source 1.3.1beta/hotspot1.3.1/src/share/vm/prims/jni.cpp looks like:

     JNI_ENTRY(jint, jni_MonitorExit(JNIEnv *env, jobject jobj))
       ...
       ObjectSynchronizer::jni_exit(obj(), CHECK_(JNI_ERR));
       return JNI_OK;

     Expanding CHECK_(JNI_ERR) gives:

       ObjectSynchronizer::jni_exit(obj(), THREAD);
       if (HAS_PENDING_EXCEPTION) return JNI_ERR;
       (0);

     However, this does not take into account that the pending exception
     *after* the call to jni_exit may have been present before the call to
     jni_exit and is therefore not an indication that something went wrong
     in jni_exit().
*/
#ifndef MONITOR_EXIT_EXCEPTION_BUG
#define MONITOR_EXIT_EXCEPTION_BUG 1
#endif

#define JASPER_SOURCE 1
#include "jasper.h"             /* ([PM] 4.2.1 this also defines SICSTUS_DBG) */
#include "jasper_glue.h"        /* [PM] 3.9 splfr-generated */

#include <stdlib.h>
#include <string.h>


#if SP_DARWIN
#ifndef JASPER_SUPPRESS_DEPRECATION_WARNINGS
#define JASPER_SUPPRESS_DEPRECATION_WARNINGS 1
#endif /* JASPER_SUPPRESS_DEPRECATION_WARNINGS */
#endif /* SP_DARWIN */

#if JASPER_SUPPRESS_DEPRECATION_WARNINGS
#if defined(__GNUC__) && ((__GNUC__ >= 4))
/* [PM] 4.2.3 jni.h in OS X 10.8 deprecates some functions */
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif  /* GCC >= 4 */
#endif /* JASPER_SUPPRESS_DEPRECATION_WARNINGS */


#ifndef JASPER_DBG
#define JASPER_DBG (SICSTUS_DBG+0)
#endif


#ifdef _MSC_VER
#define BreakPoint()        _asm { int 3h }
#endif

#if JASPER_DBG && 0
#define DbgBreakPoint() BreakPoint()
#else
#define DbgBreakPoint()
#endif

#if JASPER_DBG

extern void dump_memory_map(void);

#if SP_WIN32

#if defined(_MSC_VER)
#define DEBUG_BREAK() DebugBreak() /* do {_asm { int 3h };} while(0) */
#endif /* defined(_MSC_VER) */

#endif /* SP_WIN32 */


#ifndef DEBUG_BREAK
/* Fallback */
#define DEBUG_BREAK() debug_break()

/* put gdb breakpoint in this function */
static void debug_break(void)
{
  int foo = 0;
  if (!foo)
    {
      exit(42);
    }
  ;
}
#endif /* DEBUG_BREAK */


#else  /* !JASPER_DBG */
#define DEBUG_BREAK() do{}while(0)
#endif /* !JASPER_DBG */

#if JASPER_DBG
#define DEBUG_BREAK_MSG(FPRINTFARGS) \
  do{ \
    fprintf(stderr, "\n%s:%d ERROR: (debug break) ", __FILE__, (int)__LINE__); fprintf FPRINTFARGS; fprintf(stderr, "\n"); \
    fflush(stderr); \
    DEBUG_BREAK(); \
  }while(0)
#else  /* !JASPER_DBG */
#define DEBUG_BREAK_MSG(FPRINTFARGS) do{}while(0)
#endif /* !JASPER_DBG */

/***************************************************/


static void jasper_DescribeException(JNIEnv *jnienv);

#if JASPER_DBG
#define ASSERT_NO_EXCP(JNIENV, STRING) \
   do{ \
      if ( (( *(JNIENV) )->ExceptionCheck((JNIENV))) ) { \
         fprintf(stderr, "ERROR: Pending Java exception (%s)\n", (STRING)); \
         jasper_DescribeException((JNIENV));\
      } \
   } while (0)
#else /* no JASPER_DBG */
#define ASSERT_NO_EXCP(JNIENV, STRING)
#endif


#if 0                           /* 1 to disable delete of refs (DEBUG) NOTE: */
#define DELETE_LOCAL_REF(ENV, REF) fprintf(stderr, "NOT deleting local ref %" SPRIdINTEGER "\n", (SP_integer)(REF))
#define DELETE_GLOBAL_REF(ENV, REF) fprintf(stderr, "NOT deleting global ref %" SPRIdINTEGER "\n", (SP_integer)(REF))
#else /* The normal definition */
#define DELETE_LOCAL_REF(ENV, REF) ((*(ENV))->DeleteLocalRef((ENV), (REF)))
#define DELETE_GLOBAL_REF(ENV, REF) ((*(ENV))->DeleteGlobalRef((ENV), (REF)))
#endif

#if JASPER_DBG
static void dbg_print_object_class(JNIEnv *jnienv, jobject jobj, char *extra_info);
#define DBG_PRINT_OBJECT_CLASS(JNIENV, JOBJ, EXTRA_INFO) dbg_print_object_class((JNIENV), (JOBJ), (EXTRA_INFO))
#else
#define DBG_PRINT_OBJECT_CLASS(JNIENV, JOBJ, EXTRA_INFO)
#endif

/* [PM] 3.9 This a temporary solution until ENTER_C_EXT provides a stash */

#define SP_API_STASH(DBGSTRING) (*SPAPI_STASH_NAME)

/* [PM] 3.8.6 Support passing and receiving null object references */
#ifndef NULL_OBJREF
#define NULL_OBJREF 1
#endif

/* Used also when not JASPER_DBG */
#define STRINGISIZE1(X) # X
#define STRINGISIZE(X) STRINGISIZE1(X)

/*
  Per Mildner 3.8.5 Major overhaul. See SICStus.java for more information.

  Starting with 3.8.2 we allowed arbitrary (java) threads to call into
  SICStus (in 3.9 this will be arbitrary threads). The JNI env
  structure is only valid within one native thread so we cannot cache
  the jnienv since we can be running in more than one thread over
  time. Furthermore "Java 2 SDK release 1.2 do not support creating
  more than one virtual machine instance in a single process", so we
  do not have to deal with multiple Java VMs. The consequences of this
  is that the jnienv need to be picked up dynamically using
  GetEnv/AttachCurrentThread and we can cache or otherwise obtain the
  JVM (using JNI_GetCreatedJavaVMs). Therefore we can completely
  ignore the $jvm(XXX) term passed from prolog (it so happens that XXX
  is actually a jnienv pointer which we now know is useless anyway).

  
 */


/*
  [Jojo, 000209]: Subsumes the notes below. 

  NOTES on Jasper and native threads, and how to allow calls from Prolog to
  Java and back, and forwards, and backwards and upside-down, and
  inside-out... etc.
  
  These notes are my attempt to clarify some of the issues in using Jasper
  to connect SICStus and Java, especially with respect to native threads.

  Do no rely on these notes to figure out exactly how certain versions of
  Jasper work; some of these notes describe how things should be, not how
  they actually are.

  There are two basic scenarios. 

  1. Java is the toplevel application. SICStus is loaded into the JVM as a
  native code library.

  2. SICStus is the toplevel application. Java is loaded into SICStus as a
  foreign resource.

  In the first case, a JVM already exists when SICStus comes into the
  picture. In the second case, we need to create a JVM using options
  specified by the user.
  
  At the time of writing this, it is unclear whether native-threads are
  tightly or loosely coupled with their Java thread, i.e. if it is allowed
  for a JVM implementation to multiplex several Java threads on top of a
  fixed set of native threads. Reading between the lines of the JVM spec,
  I get the feeling that this one-to-one mapping is not required, but that
  all JVMs I know of uses a hard one-to-one mapping.

  One of the trickiest issues in this interface is to balance two 
  important issues:

  1. Since the SICStus emulator is not thread-safe with respect to native
  threads, we have to prevent any two native threads from accidentally
  calling the SICStus emulator simultaneously. 

  2. However, we do not want to restrict the way Prolog and Java can
  call each other. We want to allow multi-level callbacks, for example.

  Let us study the first scenario (Java as toplevel application).

  1a. Java -> Prolog. This is equivalent to a simple call to a C-function.
      This presents no problem, as long as the Java-methods performing the
      call are synchronized on the SICStus object, such that another native
      thread cannot call Prolog until the first native thread has
      returned. This is implemented in 3.8.2.

  1b. Java -> Prolog -> Java. This is equivalent to Java calling a
      C-function which in turn makes a call to Java. This complicates
      matters:

      - What Java thread should we call?

      - Should we even allow the user to specify another Java-thread to
      call (than the one which called Java?).

      - Is it at all possible to call another Java-thread than the one
      "belonging" to the current native thread?

      The current implementation does not allow the user to specify which
      Java thread to call.
      
  1c. Java -> Prolog -> Java -> Prolog. This is where it gets really
      interesting. 

      If the 1b-scenario always calls the same Java-thread, and we always
      have a one-to-one mapping between Java-threads and native threads,
      then this is probably quite problem-free. When we return to Java, we
      will still be in the (Java-)thread which holds the lock on the
      SICStus object and we can safely call Prolog. 

      If the 1b-scenario allows the user to call another Java-thread, we
      face the situation where we're inside the JVM, but in a different
      Java-thread than the one which called Prolog. Probably even in a
      different native thread.
     
      This should be possible, as long as SICStus does not do any black magic
      regarding its C-stack. For example:

      1. No longjmp():s which goes past foreign code (i.e. C-code outside
      the SICStus C-interface).

      2. No assumptions about the C-stack except what is visible from a
      regular C-function.

      3. No thread-local data used.

      If these restrictions hold, calling Prolog recursively from another
      native thread should not be any different than doing it from the same
      native thread.

      Of course, when SICStus MT comes into life and supports native
      threads, then things become *really* interesting.


  2a. Prolog -> Java. Again, this is no problem at all. There are no
      threads at the Prolog level. 

      The discussion under 1b above may apply a little, though. One might
      want to allow the user to specify what Java-thread to call.

  2b. Prolog -> Java -> Prolog. See 1c.

 */

/* NOTES on the implementation of Jasper. See also spnative.c.
 * ===========================================================
 * (These notes should probably make it into the User's Manual
 * in some form or another).
 *
 * - Jasper can run in two modes, either with Java as a toplevel
 * application or with SICStus as toplevel application.
 *
 * - In order to avoid problems due to the fact that the SICStus
 * emulator is not reentrant (i.e. only one native thread is allowed
 * inside the emulator at any given time), we must assure that when
 * SICStus is called, it is always done from the same Java (native)
 * thread.
 * [Jojo, 000209] This is too restrictive. It is probably enough that 
 * no two native threads call the emulator at the same time.
 * 
 * - JNI maintains what is known as a environment pointer
 * (jnienv). This uniquely identifies which Java thread the native
 * code is currently attached to, and so also determines which Java
 * thread will be executing any Java method calls made.  So, to assure
 * that SICStus is always called from the same Java thread, we keep
 * our own copy of jnienv. This enables us to check if the current
 * Java thread is "allowed" to call SICStus. (this check is however
 * not fully implemented yet). 
 * [Jojo] See note above.
 *
 * - When SICStus is loaded into a JavaVM, spInitialize() will be
 * called. spInitialize() will store the jnienv with which it was
 * called, so only the thread which created the SICStus object will be
 * allowed to call SICStus native methods. When Java is loaded into
 * SICStus, jasper_initialize() is called, either explicitly by the
 * user, or implicitly by the glue code of a foreign Java-method
 * (i.e. declared by foreign(<Method>,java,<Pred>)).
 *
 * jasper_initialize() can be called repeatedly to get a handle to
 * jnienv (which on the Prolog side is a term
 * '$jvm'(<ptr>)). jasper_deinitialize() can be called to unload the
 * JVM. This is however not implemented (as of JDK 1.2).  
 *
 * Unresolved issues: When Java is started from Prolog, how does the
 * Java code get a reference to the SICStus object?
 * 
 * Presumably this has to be done by not only creating a JVM, but also
 * creating a SICStus object which can be accessed by Java methods called
 * from Prolog.
 */

#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (!(TRUE))
#endif

/* These belong in a header file. This declarations is copied in spnative.c */

static jvalue
SPCDECL CallMethodByName(JNIEnv *env,
                 jboolean *hasException,
                 jobject obj, 
                 const char *name,
                 const char *descriptor,
                 ...);



/************************ State ***************************/

#if MULTI_SP_AWARE

#define INIT_STASH() LAZY_NULL_CHECK(((SP_API_STASH("INITING")) = (void *)SP_malloc(sizeof(struct jasper_state))))

#define SP_STATE(DBGSTRING) (*(struct jasper_state *)SP_API_STASH(DBGSTRING))

#define DECLARE_STATE_BEGIN struct jasper_state {
#define DECLARE_STATE_VARIABLE(TYPE, NAME) \
   TYPE NAME;                   /* struct field */
#define STATE_VARIABLE(NAME) (SP_STATE(#NAME)).NAME
#define DECLARE_STATE_END };

#else /* !MULTI_SP_AWARE */

#define INIT_STASH() 

#define DECLARE_STATE_BEGIN
#define DECLARE_STATE_VARIABLE(TYPE, NAME) \
   static TYPE NAME;
#define STATE_VARIABLE(NAME) NAME
#define DECLARE_STATE_END

#endif /* !MULTI_SP_AWARE */

#define INIT_STATE_VARIABLE(NAME, VALUE) \
   STATE_VARIABLE(NAME) = (VALUE)


/* [PM] 3.9
  For each SP run-time we keep the following state (together with the API dispatch table):
  atoms
  jvm         The same for all since we only have one JavaVM
  jnienv?     Now that SP should only be called from one thread we can
              keep the (thread specific) jnienv 
  [PD] 3.9 jasper_sp_global is eliminated.
  jasper_sp_global       The SICStus object corresponding to this SP-run-time (legacy name)
  
*/
DECLARE_STATE_BEGIN

DECLARE_STATE_VARIABLE(JavaVM *, jvm)
DECLARE_STATE_VARIABLE(JNIEnv *, state_jnienv)
DECLARE_STATE_VARIABLE(volatile SP_uinteger, context_counter)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_atom)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_boolean)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_byte)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_char)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_codes)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_chars) /* [PM] 4.0.2+ legacy */
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_dollar_java_object)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_double)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_false)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_float)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_integer)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_jvm)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_long)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_object)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_plus_sign)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_short)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_string)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_term)
DECLARE_STATE_VARIABLE(SP_atom, jasper_atom_true)
DECLARE_STATE_VARIABLE(jboolean, versionMatchingDone) /* [PD] 3.11.1 */

DECLARE_STATE_END


/***************************************************/
static jobject jasperi_new_sicstus_object(SPAPI_ARG_PROTO_DECL JNIEnv *env);


#if THREADSERVER                /* [PD] 3.9 */
static jobject jasper_get_server(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv);
#endif


/* [PM] 3.9 These are rewritten from the 3.8.6 versions in jasper.h
SPJavaGlueErrorM
SPJavaGluePropagateJavaExceptionM
SPJavaGlueGetObjectM
SPJavaGlueSPTermM
SPJavaGlueGetNativeTermRefM
SPJavaGlueGetNativeTermRefInJavaContextM
SPJavaGlueSPCanonicalAtomM
SPJavaGlueGetAtomM
SPJavaGluePostPutCodesM
SPJavaGlueStringBufferM
SPJavaGluePostToStringM
SPJavaGluePostPutStrM
*/

/* err_msg can be NULL */
static void SPJavaGlueErrorM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, int argnum /*IGNORED*/, const char *err_msg)
{
#if JASPER_DBG+0 > 0
  fprintf(stderr, "SPJavaGlueErrorM: arg# %d, `%s', ", argnum, ((err_msg != NULL) ? err_msg : "<<NULL>>"));
#endif
  (void)argnum;
  if (jnienv && (*jnienv)->ExceptionCheck(jnienv))
    {
#if JASPER_DBG
      (*jnienv)->ExceptionDescribe(jnienv); /* questionable */
      fprintf(stderr, "\n");
#endif 
      (*jnienv)->ExceptionClear(jnienv);
    }
#if JASPER_DBG
  fprintf(stderr, "\n");
#endif 

  {
    SP_term_ref excp_term = SP_new_term_ref();
    SP_put_string(excp_term, ((err_msg != NULL) ? err_msg : "Error in Java glue"));
    SP_raise_exception(excp_term);
  }
}

#ifndef JASPER_SP_INTEGER_BYTES
#ifdef SP_put_integer_bytes
#define JASPER_SP_INTEGER_BYTES 1
#endif /* SP_put_integer_bytes */
#endif /* JASPER_SP_INTEGER_BYTES */

/* [PM] 3.9.1 Use put_jlong/get_jlong for all pointers, jlongs etc.
 * Uses the new (3.9.1) bignum API
 * A copy of these are in spnative.c keep in synch!
 */
static int put_jlong(SPAPI_ARG_PROTO_DECL SP_term_ref tr, jlong x)
{
  int rc;

#if JASPER_SP_INTEGER_BYTES
  rc = SP_put_integer_bytes(tr, &x, sizeof x, 1 /* native */);
#else/* !JASPER_SP_INTEGER_BYTES */
  rc = SP_put_integer(tr, (SP_integer)x);  /* will truncate on most platforms! */
#endif
  if (!rc)
    {
      JASPER_DBGLINE(put_jlong);
#if JASPER_DBG
      /* This should not happen */
      fprintf(stderr, "ERROR: internal error: !"
#if JASPER_SP_INTEGER_BYTES
              "SP_put_integer_bytes"
#else  /* !JASPER_SP_INTEGER_BYTES */
              "SP_put_integer"
#endif /* !JASPER_SP_INTEGER_BYTES */
              " in put_jlong %s:%d\n", __FILE__, __LINE__);fflush(stderr);
      abort();
#endif /* JASPER_DBG */
      ;
    }
  return rc;
}

static int put_jlong_ptr(SPAPI_ARG_PROTO_DECL SP_term_ref tr, const void* x)
{
  jlong x_jlong = (SP_integer)x; 

  return put_jlong(SPAPI_ARG tr, x_jlong);
}

/* A copy (except for SPAPI_ARG_PROTO_DECL) of this is in spnative.c keep in synch! */
static int get_jlong(SPAPI_ARG_PROTO_DECL SP_term_ref tr, jlong *px, int allow_float)
{
  int rc;

#if JASPER_SP_INTEGER_BYTES
  {
    size_t sz = sizeof *px;

    if (allow_float && SP_is_float(tr)) /* Manual says *number* is converted to java long. The curse of backward compatibilty. */
      {
        double xf;

        if (!SP_get_float(tr, &xf))
          {
            rc = 0; /* SP_get_float should not be able to fail... */
          }
        else
          {
            jlong xl;
            #ifndef SP_int64_min /* [PM] */
            #error "SP_int64 limits not defined, surely SP_int64 should be available if java is supported"
            #endif/* SP_int64_min */

            /* [PM] 3.9.1 FIXME: what if !NAN_COMPARE_FAILS */
            if ((double)SP_int64_min <= xf 
                && xf <= (double)SP_int64_max)
              {

                #if TEST_INT64_HACK || (SP_AIX && __GNUC__)

                /* [PM] 3.9.1 when converting a double to int64 gcc
                   2.95.2 will generate __fixdfdi (from libgcc.a,
                   which is *not* linked with shared objects on AIX
                   4.3.3. so cause trouble if main program is not
                   gcc-generatied (e.g., java launcher). Also see
                   SHIFT_LEFT_ONE_BYTE_HARD.
                   
                   To avoid this we do the conversion the really hard
                   way. Speed does not matter, this code is unlikely
                   to ever be used.
                */
                {

                  /* [PM] 3.9.1 rationale for not doing large floats
                     right: First of all the fact that we do not
                     give up on non-integers is an historical and
                     unfortunate accident.  Secondly, it will only
                     happen with Java on AIX, not exactly a common
                     combination.
                     Lastly, it will properly report the error.
                  */
                  #if SICSTUS_REVISION_VERSION>2 /* [PM] 3.9.2 fix later */
                  #error "Need a way to manually convert from double to int64"
                  #else/* convert to long, and give up otherwise */
                  /* Consider xf==SP_int64_min */
                  {
                    if (LONG_MIN <= xf && xf <= LONG_MAX)
                      {
                        xl = (SP_integer)xf;
                        *px = xl;
                        rc = 1;
                      }
                    else
                      {
                        rc = 0;
                      }
                  }
                  #endif
                }
                #else /* !(SP_AIX && __GNUC__) */
                {
                  xl = (jlong)xf;
                  *px = xl;
                  rc = 1;
                }
                #endif /* !(SP_AIX && __GNUC__) */
              }
            else                /* float out of 64bit range or nan/inf */
              {
                rc = 0;
              }
          }
      }
    else                        /* !allow_float or not a float */
      {
        rc = SP_get_integer_bytes(tr, px, &sz, 1 /* native */);
        if (sz > sizeof *px && !rc) /* overflows 64bit */
          {
#if JASPER_DBG
            JASPER_DBGLINE(get_jlong);
            fprintf(stderr, "DBG WARNING: overflow from SP_get_integer_bytes (need size %d, has %d)"
                    " (this is expected)" /* for Suite scan.pl */
                    "\n",
                    (int)sz, (int)sizeof *px);fflush(stderr);
#endif /* JASPER_DBG */
            ;
          }
      }
  }
#else/* !JASPER_SP_INTEGER_BYTES */
  {
    SP_integer x;
    rc = SP_get_integer(tr, &x);  /* will truncate on most platforms! */
    if (rc) *px = (jlong)x;
  }
#endif
  if (!rc)
    {
      JASPER_DBGLINE(get_jlong);
#if JASPER_DBG
        /* likely cause is overflow or type error, not internal errors */
      fprintf(stderr, "DBG WARNING: !"
#if JASPER_SP_INTEGER_BYTES
                "SP_get_integer_bytes"
#else  /* !JASPER_SP_INTEGER_BYTES */
                "SP_get_integer"
#endif /* !JASPER_SP_INTEGER_BYTES */
                " in get_jlong %s:%d"
                " (this is expected)" /* for Suite scan.pl */
                "\n", __FILE__, __LINE__);fflush(stderr);
#endif /* JASPER_DBG */
      ;
    }
  return rc;
}



/* 0 if there was no java exception to propagate.
   err_msg can be NULL
 */
static int SPJavaGluePropagateJavaExceptionM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, int argnum /*IGNORED*/, const char *err_msg /*IGNORED*/)
{
  jobject java_excp = NULL;
  (void)argnum; (void)err_msg;
  
  java_excp = (*jnienv)->ExceptionOccurred(jnienv);
  if (java_excp)
    {
      SP_term_ref excp_term = SP_new_term_ref();
      SP_term_ref excp_obj = SP_new_term_ref();

#if JASPER_DBG
      fprintf(stderr, "SPJavaGluePropagateJavaExceptionM: arg# %d, `%s' (this is expected), ", argnum, err_msg);
      (*jnienv)->ExceptionDescribe(jnienv); /* questionable */
      fprintf(stderr, "\n");
      /* abort(); */
#endif

      (void)put_jlong_ptr(SPAPI_ARG excp_obj, (*jnienv)->NewGlobalRef(jnienv,java_excp));

      SP_cons_functor(excp_term, SP_atom_from_string("$java_object"), 1, excp_obj);
      SP_raise_exception(excp_term);
      (*jnienv)->ExceptionClear(jnienv); /* not needed if DBG since ExceptionDescribe clears it */
    }

  if (java_excp)
    {
      (*jnienv)->DeleteLocalRef(jnienv, java_excp);
      return 1;
    }
  return 0;
}


static int SPJavaGlueGetObjectM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_term_ref tobj, jobject *pjobject, int argnum, const char *err_msg)
{
  jlong obj_ptr;
  int rc;
  jobject jobj;

  if (!( SP_get_arg(1, tobj, tobj)
         && get_jlong(SPAPI_ARG tobj,&obj_ptr,FALSE) ))
    {
      rc = 0;
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, err_msg);
      jobj = NULL;
    }
  else
    {
      rc = 1;
      jobj = (obj_ptr ?
              /* [PM] 3.9.1 avoid "warning: cast to pointer from integer of different size" */
              (jobject) (SP_integer) obj_ptr
              : NULL );
    }
  *pjobject = jobj;
  return rc;
}

#if THREADSERVER
static jobject SPJavaGlueSPTermM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_term_ref term, int argnum, const char *err_msg, int init, jboolean new_style_interface)
#else
static jobject SPJavaGlueSPTermM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_term_ref term, int argnum, const char *err_msg, int init)
#endif
{
  jvalue result;
  jboolean hasException = JNI_FALSE;
  jobject sp_obj = sp_get_jasper_magic(SICSTUS_VERSION);
#if THREADSERVER
  int threadservermode;
#endif
  
  (void)err_msg;

#if JASPER_DBG
  fprintf(stderr, "SPJavaGlueSPTermM %s term=%d \n", (init ? "init from" : "ignored"), term);
#endif

  if (!sp_obj) {
    result.l = NULL;
    goto finish;
  }

#if THREADSERVER
  if(!sp_get_jasper_threadservermode(&threadservermode, SICSTUS_VERSION)) {
    result.l = NULL;
    goto finish;
  }
#endif

  if (init)
    {
#if JASPER_DBG
      fprintf(stderr, "SPJavaGlueSPTermM: first call to newGlueTerm\n");
#endif
      result = CallMethodByName(jnienv,
                                &hasException,
                                sp_obj,
                                "newGlueTerm",
                                "(J)Lse/sics/jasper/SPTerm;",
                                (jlong)term);
#if JASPER_DBG
      fprintf(stderr, "SPJavaGlueSPTermM: result.l==%p\n", result.l);
      fprintf(stderr, "SPJavaGlueSPTermM: second call to newGlueTerm\n");

      result = CallMethodByName(jnienv,
                                &hasException,
                                sp_obj,
                                "newGlueTerm",
                                "(J)Lse/sics/jasper/SPTerm;",
                                (jlong)term);

      fprintf(stderr, "SPJavaGlueSPTermM: result.l==%p\n", result.l);
      {
        int hc = CallMethodByName(jnienv, &hasException, result.l, "hashCode",
                                  "()I").i;
        fprintf(stderr, "SPJavaGlueSPTermM: result.hashCode()==%d (0x%x)\n", hc, hc);
      }
#endif
#if THREADSERVER
/* This should only be done if we previously found a method which has a
   se/sics/jasper/Term as one of its arguments AND we are running in thread-
   server mode. */
      ASSERT_NO_EXCP(jnienv, "SPJavaGlueSPTermM");
      if (threadservermode && new_style_interface) {
        jobject server = jasper_get_server(SPAPI_ARG jnienv);
        jvalue res = CallMethodByName(jnienv, &hasException,
                                      server,
                                      "newGlueJasperTerm",
                                      "(Lse/sics/jasper/SPTerm;)Lse/sics/jasper/Jasper$JasperTerm;",
                                      result.l);
        result = res;
      }
#endif
    }
  else
    {
      result = CallMethodByName(jnienv,
                                &hasException,
                                sp_obj,
                                "newGlueTerm",
                                "()Lse/sics/jasper/SPTerm;");
#if THREADSERVER
/* This should only be done if we previously found a method which has a
   se/sics/jasper/Term as one of its arguments AND we are running in thread-
   server mode. */
      if (threadservermode && new_style_interface) {
        jvalue res = CallMethodByName(jnienv, &hasException,
                                      jasper_get_server(SPAPI_ARG jnienv),
                                      "newGlueJasperTerm",
                                      "(Lse/sics/jasper/SPTerm;)Lse/sics/jasper/Jasper$JasperTerm;",
                                      /* "(Lse/sics/jasper/SPTerm;)Ljava/lang/Object;", */
                                      result.l);
        result = res;
      }
#endif
    }

 finish:
  if (hasException || result.l == NULL)
    {
#if THREADSERVER
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot create object of class se/sics/jasper/SPTerm or se/sics/jasper/Jasper$JasperTerm");
#else
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot create object of class se/sics/jasper/SPTerm");
#endif
      goto cleanup;
    }
  return result.l;

 cleanup:
  return NULL;
}

/* 0 on error (after callling SPJavaGlueErrorM) */
static int SPJavaGlueGetNativeTermRefM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject jobj, SP_term_ref *p_ref, int argnum, const char *err_msg)
{
  int rc = 0;
  jclass clazz = NULL;
  jmethodID methodID;
  jlong jterm_ref;

  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  clazz = (*jnienv)->GetObjectClass(jnienv, jobj);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  methodID = (*jnienv)->GetMethodID(jnienv, clazz, "GetNativeTermRef", "()J");
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  if (!methodID)
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, err_msg);
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  jterm_ref = (*jnienv)->CallLongMethod(jnienv, jobj, methodID);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  if ((*jnienv)->ExceptionCheck(jnienv))
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
      SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, argnum, err_msg);
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  rc = 1;                       /* success */
  *p_ref = (SP_term_ref) jterm_ref;

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefM);
  return rc;
}

/* called in Java context so may not call any prolog routines. Returns 0 on error without reporting error. */
static int SPJavaGlueGetNativeTermRefInJavaContextM(/*SPAPI_ARG_PROTO_DECL*/ JNIEnv *jnienv, jobject jobj, SP_term_ref *p_ref)
{
  int rc = 0;
  jclass clazz = NULL;
  jmethodID methodID;
  jlong jterm_ref;

  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  clazz = (*jnienv)->GetObjectClass(jnienv, jobj);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  methodID = (*jnienv)->GetMethodID(jnienv, clazz, "GetNativeTermRef", "()J");
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  if (!methodID)
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
      /* would call prolog run-time: SPJavaGlueError(jnienv, argnum, err_msg); */
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  jterm_ref = (*jnienv)->CallLongMethod(jnienv, jobj, methodID);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  if ((*jnienv)->ExceptionCheck(jnienv))
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
      /* would call prolog run-time: SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, argnum, err_msg); */
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  rc = 1;                       /* success */
  *p_ref = (SP_term_ref) jterm_ref;

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContextM);
  return rc;
}

#if THREADSERVER
static jobject SPJavaGlueSPCanonicalAtomM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_atom atom, int argnum, jboolean new_style_interface)
#else
static jobject SPJavaGlueSPCanonicalAtomM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_atom atom, int argnum)
#endif
{

  jstring string = NULL;
  const char *codes;
  jvalue result;
#if THREADSERVER
  int threadservermode;
  jboolean hasException = JNI_FALSE;
#else
  jboolean hasException;
#endif
  const char *err_msg = "Cannot create object of class se/sics/jasper/SPCanonicalAtom";
  jobject sp_obj = sp_get_jasper_magic(SICSTUS_VERSION);

#if JASPER_DBG
  fprintf(stderr, "SPJavaGlueSPTermM atom=0x%" SPRIxINTEGER" \n", (SP_uinteger)atom);
#endif

  if (!sp_obj) { goto barf; }

#if THREADSERVER
  if(!sp_get_jasper_threadservermode(&threadservermode, SICSTUS_VERSION)) goto barf;
#endif

  {
    codes = SP_string_from_atom(atom);
    if (!codes) { JASPER_DBGLINE(SPJavaGlueSPTermM); goto barf;}
    string = (*jnienv)->NewStringUTF(jnienv, codes);
    if (!string) { JASPER_DBGLINE(SPJavaGlueSPTermM); goto barf;}
  }

#if THREADSERVER
  /* If we are in threadserver mode AND have found a method which has a
     java/lang/String as one of its arguments, we should return the string. */
  if (threadservermode && new_style_interface) {
    result.l = string;
  } else {
#endif
  result = CallMethodByName(jnienv,
                            &hasException,
                            sp_obj,
                            "newGlueAtom",
                            "(Ljava/lang/String;)Lse/sics/jasper/SPCanonicalAtom;",
                            string);
#if THREADSERVER
  }
#endif

  if (hasException || result.l == NULL) goto barf;
  return result.l;

 barf:
  SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, err_msg);
  if (string) (*jnienv)->DeleteLocalRef(jnienv, string);
  return NULL;
}

#if JASPER_DBG                         /* [PD] 3.9.2 copied from spnative.c */
/* [PM] 3.9.2b3 dbeug */
static jboolean jasper_DescribeClass(JNIEnv *jnienv, jobject jobj)
{
  jclass clazz = NULL;
  jclass clazzclazz = NULL;
  jstring jname = NULL;
  jmethodID mid;
  char const *className = NULL;
  jboolean rc;
#if JASPER_DBG
  int b=0;
#endif


  if (jobj == NULL) goto barf1;

  clazz = (*jnienv)->GetObjectClass(jnienv, jobj); if (!clazz) goto barf2;
  clazzclazz = (*jnienv)->GetObjectClass(jnienv, clazz); if (!clazzclazz) goto barf3;
  mid = (*jnienv)->GetMethodID(jnienv,clazzclazz,"getName","()Ljava/lang/String;");   if (!mid) goto barf4;
  jname = (jstring)(*jnienv)->CallObjectMethod(jnienv, clazz, mid); if (!jname) goto barf5;
  className = (char *)(*jnienv)->GetStringUTFChars(jnienv,jname,NULL); if (!className) goto barf6;

  #if JASPER_DBG
  fprintf(stderr, "\"%s\"\n", className);fflush(stderr);
  #endif
  rc = JNI_TRUE;
 cleanup:

  if (className) (*jnienv)->ReleaseStringUTFChars(jnienv, jname, className);
  if (jname) (*jnienv)->DeleteLocalRef(jnienv, jname);
  if (clazzclazz) (*jnienv)->DeleteLocalRef(jnienv, clazzclazz);
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);

  return rc;
 barf6:  b++; 
 barf5:  b++; 
 barf4:  b++; 
 barf3:  b++; 
 barf2:  b++; 
 barf1:  b++; 
  
  #if JASPER_DBG
  fprintf(stderr, "ERROR: jasper_DescribeClass could not determine class (reason #%d)", (int)b);fflush(stderr);
  #endif
  rc = JNI_FALSE;
  goto cleanup;
}
#endif


/* return 0 on failure (will call SPJavaGlueError) */
#if THREADSERVER
static int SPJavaGlueGetAtomM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject obj /* SPCanonicalAtom or SPTerm */, SP_atom *p_atom, int argnum, const char *err_msg, jboolean threadservermode)
#else
static int SPJavaGlueGetAtomM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject obj /* SPCanonicalAtom or SPTerm */, SP_atom *p_atom, int argnum, const char *err_msg)
#endif
{
  int rc = 0;
  jclass clazz = NULL;          /* needs cleanup */
  jstring string = NULL;        /* needs cleanup */
  const char *codes = NULL;     /* needs cleanup */
  SP_atom atom = 0;             /* 0 marks invalid atom */


  clazz = (*jnienv)->GetObjectClass(jnienv, obj);

#if JASPER_DBG
  jasper_DescribeClass(jnienv, obj);
#endif

#if THREADSERVER
  if (threadservermode) {     /* [PD] 3.9.2 Only java/lang/String allowed */
    if ((codes = (*jnienv)->GetStringUTFChars(jnienv, obj, NULL)) == NULL) {
      goto propagate_java_exception;
    }
    /* [PM] 4.0 SPIO: FIXME: NUL (probably OK since Java and SP4 use non-shortest form to encode NUL) */
    atom = SP_atom_from_string(codes);/* zero if malformed UTF8 (e.g., embedded NUL or too long) */
  } else
#endif
    /* This block should set atom to the atom number (or zero) (or leave a pending exception) */
    {
      const char *methodName;
      const char *typesig;
      jmethodID methodID;

      methodName = "getAtom";
      typesig = "()J";
      methodID = (*jnienv)->GetMethodID(jnienv, clazz, methodName, typesig);

      if (methodID != 0)
        {
          JASPER_DBGLINE(SPJavaGlueGetAtomM);

          atom = (SP_atom) (*jnienv)->CallLongMethod(jnienv, obj, methodID);
        }
      else                          /* no getAtom method (SPTerm), try getString (SPCanonicalAtom) */
        {
          JASPER_DBGLINE(SPJavaGlueGetAtomM);
          (*jnienv)->ExceptionClear(jnienv); /* clear the failed method lookup */
          methodName = "getString";
          typesig = "()Ljava/lang/String;";
          methodID = (*jnienv)->GetMethodID(jnienv, clazz, methodName, typesig);

          if (methodID == 0) goto generic_glue_error; /* neither getAtom nor getString found */
          if ((  (string = (*jnienv)->CallObjectMethod(jnienv, obj, methodID)) == NULL
                 ||
                 (*jnienv)->ExceptionCheck(jnienv)
                 ||
                 (codes = (*jnienv)->GetStringUTFChars(jnienv, string, NULL)) == NULL))
            {
              goto propagate_java_exception;
            }
        }
      /* codes is a UTF8 string here */
      /* [PM] 4.0 SPIO: FIXME: NUL (probably OK since Java and SP4 use non-shortest form to encode NUL) */
      atom = SP_atom_from_string(codes);/* zero if malformed UTF8 (e.g., embedded NUL or too long) */
    }

  JASPER_DBGLINE(SPJavaGlueGetAtomM);
  if ((*jnienv)->ExceptionCheck(jnienv)) goto propagate_java_exception;

  /* atom valid or zero here */
  if (atom == 0) goto generic_glue_error;
  *p_atom = atom;

  rc = 1;
  /* fall through */
 cleanup:
  JASPER_DBGLINE(SPJavaGlueGetAtomM);
  if (codes) (*jnienv)->ReleaseStringUTFChars(jnienv, string, codes);
  if (string) (*jnienv)->DeleteLocalRef(jnienv, string);
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  return rc;
 propagate_java_exception:                     /* propagate any java exception (ExceptionCheck() probably true here */
  JASPER_DBGLINE(SPJavaGlueGetAtomM);
  if (SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, argnum, err_msg)) goto cleanup;
  /* fall through */
 generic_glue_error:                    /* report a generic 'error in glue' (ignore and clear pending java exceptions) */
  JASPER_DBGLINE(SPJavaGlueGetAtomM);
  SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, err_msg);
  goto cleanup;
}

/* return 0 on fail or error (will call SPJavaGlueError) */
static int SPJavaGluePostPutCodesM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jstring string_obj, SP_term_ref tstring, int argnum)
{
  int rc = 0;
  const char *codes = NULL;

  JASPER_DBGLINE(SPJavaGluePostPutCodesM);
  codes = (*jnienv)->GetStringUTFChars(jnienv, string_obj, NULL);

#if JASPER_DBG
  fprintf(stderr, "SPJavaGluePostPutCodesM \"%s\"\n", (codes ? codes : "<<ERROR: codes==NULL!! >>"));
#endif
  JASPER_DBGLINE(SPJavaGluePostPutCodesM);
  if((!codes)
     || (!SP_put_list_codes(tstring, SP_new_term_ref(), (char *)codes)))
    {
      JASPER_DBGLINE(SPJavaGluePostPutCodesM);
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot make -string or [-string]");
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGluePostPutCodesM);
  rc = 1;
  
 cleanup:
  JASPER_DBGLINE(SPJavaGluePostPutCodesM);
  if (codes)
    {
      JASPER_DBGLINE(SPJavaGluePostPutCodesM);
      (*jnienv)->ReleaseStringUTFChars(jnienv, string_obj, codes);
    }

  return rc;
}

/* return NULL on error (will call SPJavaGlueError) */
static jobject SPJavaGlueStringBufferM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, int argnum)
{
  jclass clazz = NULL;
  jmethodID methodID;
  jobject stringBuffer = NULL;
  
  clazz = (*jnienv)->FindClass(jnienv, "java/lang/StringBuffer");
  if (clazz==NULL)
    {
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot find class java/lang/StringBuffer for -atom or -string");
      goto cleanup;
    }

   methodID = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "()V");
   if (methodID == 0)
     {
       SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot find constructor for class `java/lang/StringBuffer'");
       goto cleanup;
     }
   stringBuffer = (*jnienv)->NewObject(jnienv, clazz, methodID);
   if (stringBuffer == NULL)
     {
       SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot create object of class java/lang/StringBuffer");
       goto cleanup;
     }
 cleanup:
   (*jnienv)->DeleteLocalRef(jnienv, clazz); /* safe to call with NULL */
   return stringBuffer;
}

/* return a new local ref to a String. Does not delete obj.
   Typical use:
   {
      jstring tmp = SPJavaGluePostToString(jnienv, obj, 42);
      (*jnienv)->DeleteLocalRef(jnienv, obj);
      obj = tmp;
   }
*/
static jstring SPJavaGluePostToStringM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject obj, int argnum)
{
  const char *methodName = "toString";
  const char *typesig = "()Ljava/lang/String;";
  jmethodID methodID;
  jclass clazz = NULL;
  jstring string_obj = NULL;

  clazz = (*jnienv)->GetObjectClass(jnienv, obj);
  methodID = (*jnienv)->GetMethodID(jnienv, clazz, methodName, typesig);
  if (methodID == 0)
    {
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Postconversion failed"); /* NOTE: better error message needed */
      goto cleanup;
    }
  string_obj = (jstring)(*jnienv)->CallObjectMethod(jnienv, obj, methodID);

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  return string_obj;
}

/* return 0 on fail or error (will call SPJavaGlueError) */
static int SPJavaGluePostPutStrM(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jstring string_obj, SP_term_ref tstring, int argnum)
{
  int rc = 0;
  const char *codes = NULL;
  JASPER_DBGLINE(SPJavaGluePostPutStrM);
  codes = (*jnienv)->GetStringUTFChars(jnienv, string_obj, NULL);
  JASPER_DBGLINE(SPJavaGluePostPutStrM);
  if((!codes)
     || (!SP_put_string(tstring, codes)))
    {
      JASPER_DBGLINE(SPJavaGluePostPutStrM);
      SPJavaGlueErrorM(SPAPI_ARG jnienv, argnum, "Cannot make -string or [-string]");
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGluePostPutStrM);
  rc = 1;
  
 cleanup:
  JASPER_DBGLINE(SPJavaGluePostPutStrM);
  if (codes)
    {
      JASPER_DBGLINE(SPJavaGluePostPutStrM);
      (*jnienv)->ReleaseStringUTFChars(jnienv, string_obj, codes);
    }

  return rc;
}

static void jasper_set_jvm(SPAPI_ARG_PROTO_DECL0)
{
  int const bogus_value = 630215;
  jsize numjvms = bogus_value;

  void * bogus_place = NULL;
  JavaVM * const bogus_jvm = (JavaVM*) &bogus_place;
  JavaVM *local_jvm =  bogus_jvm;
  /* Luckily "Java 2 SDK release 1.2 do not support creating more than one
     virtual machine instance in a single process" */
  if (JNI_GetCreatedJavaVMs(&local_jvm, 1, &numjvms) != 0) /* non-zero on failure */
    {
      local_jvm = NULL; numjvms = 0;
#if JASPER_DBG
      fprintf(stderr, "**** ERROR: Inside jasper_set_jvm(): JNI_GetCreatedJavaVMs() failed\n");fflush(stderr);
#endif/* JASPER_DBG */
    }
  else
    {
#if SP_DARWIN                   /* work around apple bugs */
      if (numjvms == bogus_value)
        {
          /* [PM] 4.0.2+ Apple (at least Mac OS X 10.4.11 Darwin 8.11.1
             Java 1.5.0_13) returns successfully without initializing
             numjvms or local_jvm */
          fprintf(stderr, "**** INFO: Apple BUG: Inside jasper_set_jvm(): numjvms = %" SPRIdINTEGER ", jvm==%p\n", (SP_integer)numjvms, local_jvm);
          numjvms = 0;
          local_jvm = NULL;
        }
#endif  /* SP_DARWIN */
    }
#if JASPER_DBG
  if (numjvms > 1)              /* Need re-design if this becomes possible in some future SDK release */
    {
      fprintf(stderr, "**** WARNING: Inside jasper_set_jvm(): numjvms = %" SPRIdINTEGER "\n", (SP_integer)numjvms);
    }
  if (numjvms != 0 && local_jvm == NULL)
    {
      fprintf(stderr, "**** ERROR: Inside jasper_set_jvm(): numjvms = %" SPRIdINTEGER ", jvm==NULL\n", (SP_integer)numjvms);
    }
  if (local_jvm == NULL && STATE_VARIABLE(jvm) != NULL)
    {
      JavaVM *stv_jvm = STATE_VARIABLE(jvm);
      fprintf(stderr, "**** ERROR: Inside jasper_set_jvm(): jvm==NULL, STATE_VARIABLE(jvm)==%p\n", (void*)stv_jvm);
    }
#endif

  if (numjvms == 1)
    {
      STATE_VARIABLE(jvm) = local_jvm;
    }
  else
    {
      STATE_VARIABLE(jvm) = NULL;
    }
#if JASPER_DBG
      fprintf(stderr, "**** DBG: Inside jasper_set_jvm(): numjvms=%" SPRIdINTEGER ", jvm==%p, STATE_VARIABLE(jvm)==%p\n", (SP_integer)numjvms, (void*)local_jvm, STATE_VARIABLE(jvm));fflush(stderr);
#endif
}

/* SP_foreach(): loops through the Prolog list "list", setting "elem"
   to each element in turn, "tail" to the tail of each node. "index"
   is a zero-based index for each element. */
#define SP_foreach(list,elem,tail,index)     \
for (SP_put_term((tail),(list)),(index) = 0; \
     SP_get_list((tail),(elem),(tail));      \
     (index)++)


/* Metacall method type definitions. */
typedef jobject (JNICALL *StaticMethodCallTypeObj)(JNIEnv *, jclass, jmethodID, jvalue *);
typedef jobject (JNICALL *MethodCallTypeObj)(JNIEnv *, jobject, jmethodID, jvalue *);

#if JASPER_DBG && 0

#define SET_JNIENV_EXTRA(JNIENV) do{ \
   fprintf(stderr, "Ensuring extra capacity (%d)\n", (int)(*(JNIENV))->EnsureLocalCapacity((JNIENV), 42)); \
   ASSERT_NO_EXCP((JNIENV), "SET_JNIENV_EXTRA"); \
}while(0)

#else

#define SET_JNIENV_EXTRA(JNIENV)

#endif

/* Will throw exception if cannot attach */
#define SET_JNIENV(jnienv,tr,FAILACTION) \
   do{ \
      (void) (tr); \
      if (NULL == ((jnienv) = jasper_int_get_jnienv(SPAPI_ARG 0 /* ! from glue */))) \
         { \
           FAILACTION ; \
         } \
      else \
         { \
           SET_JNIENV_EXTRA(jnienv); \
         } \
   }while(0)


/* BROKEN, all callers need error handling
#define GET_JNIENV(tr,jnienv) do{if (NULL == ((jnienv) = jasperi_get_jnienv((tr)))) { return; }}while(0)
#define GET_JNIENV_RV(tr,jnienv,rv) do{if (NULL == ((jnienv) = jasperi_get_jnienv(tr))) { return (rv); }}while(0)
*/

#define return_onzero(rv,call)                     \
do{ (rv) = (call);                                 \
     if((rv) == 0) return;                         \
}while(0)

#define MSG_BARF(MSG_ienc) do{ error_message=(MSG_ienc); goto msg_barf_; }while(0)

#define CODE_BARF(ERROR_CODE) do{ error_code = (ERROR_CODE); goto code_barf_; }while(0)

/* [PM] 3.10.1 This is the lowest version we can use (for JNI_OnLoad and attach to pre-existing thread) */
/* If you change this you need to change the error message for JNI_EVERSION */
#ifndef SP_JNI_VERSION_NEEDED
#ifdef JNI_VERSION_1_4
#define SP_JNI_VERSION_NEEDED JNI_VERSION_1_4
#endif /* JNI_VERSION_1_4 */
#endif /* SP_JNI_VERSION_NEEDED */

#ifndef SP_JNI_VERSION_NEEDED
#define SP_JNI_VERSION_NEEDED JNI_VERSION_1_2
#endif /* SP_JNI_VERSION_NEEDED */

#define SP_JNI_VERSION_WANTED_DEFAULT SP_JNI_VERSION_NEEDED

/*  [PM] 3.10.1 If we compile with 1.3.1 (which we do) then
    JNI_VERSION_1_4 will not be defined. However, our code may still
    be linked against JDK 1.4.1 at installation in which case it makes
    sense for the user to request JNI_VERSION_1_4.

    The Apple Java 1.4.1 Release Notes says
    <http://developer.apple.com/techpubs/macosx/ReleaseNotes/java141/>

       By default, the JNI_CreateJavaVM call returns the Java 1.3.1
       virtual machine (VM) when JNI_VERSION_1_2 is specified. This is
       done to maintain backward compatibility with the Java 1.3.1
       version of applications that may already be in use on the
       system. If you need to use the 1.4.1 VM and link against the
       1.4.1 version of jni.h, pass JNI_VERSION_1_4 instead of
       JNI_VERSION_1_2.

   However, perhaps fortunately, experiments indicate that that claim
   is false. JNI_CreateJavaVM will return a 1.4.1 VM for both
   JNI_VERSION_1_2 and JNI_VERSION_1_4. I have reported this (Problem
   ID 3210736 at bugreport.apple.com).

   Due to the difference between reality and Apple documentation the
   main rationale for supporting the JNI_VERSION_1_4 option to
   jasper_initialize has disappeared. For now (3.10.1) I will keep the
   supporting code but not document it.
 
*/

#ifdef JNI_VERSION_1_4
#if JNI_VERSION_1_4 != 0x00010004
#error "JNI_VERSION_1_4 != 0x00010004"
#endif
#endif /* JNI_VERSION_1_4 */
#define SP_JNI_VERSION_1_4 0x00010004


JNIEXPORT jint JNICALL
JNI_OnLoad(JavaVM *vm, void *reserved) {
  (void)vm;
  (void)reserved;

  return SP_JNI_VERSION_NEEDED;
}

static void jasper_int_raise_exception(SPAPI_ARG_PROTO_DECL char const *type, char const *desc)
{
  SP_term_ref 
    excp_term = SP_new_term_ref(),
    arg1 = SP_new_term_ref();
  
  SP_put_string(arg1, desc);
  SP_cons_functor(excp_term, SP_atom_from_string(type), 1, arg1);
                  
  SP_raise_exception(excp_term);
  return;
}

/* Does (*jnienv)->ExceptionDescribe(jnienv) but does *not* clear the pending exception */
static void jasper_DescribeException(JNIEnv *jnienv)
{
  jthrowable pending = NULL;

  pending = (*jnienv)->ExceptionOccurred(jnienv);
  if (pending)
    {
      fprintf(stderr, "jasper::jasper_DescribeException: ");
      (*jnienv)->ExceptionDescribe(jnienv); /* clears it  */
      (*jnienv)->Throw(jnienv, pending); /* restores it */
    }

  if (pending)
    {
      (*jnienv)->DeleteLocalRef(jnienv, pending);
    }
}


/* A copy is in spnative.c, keep in synch */
static jvalue SPCDECL
CallMethodByName(JNIEnv *env,
                 jboolean *hasException,
                 jobject obj, 
                 const char *name,
                 const char *descriptor,
                 ...)
{
  va_list args;
  jclass clazz;
  jmethodID mid;
  jvalue result;

  result.l = NULL;

  ASSERT_NO_EXCP(env, "CallMethodByName");
  if ((*env)->EnsureLocalCapacity(env, 2) == JNI_OK)
    {
      clazz = (*env)->GetObjectClass(env, obj);
      mid = (*env)->GetMethodID(env, clazz, name, descriptor);
      if (mid) {
        const char *p = descriptor;
        /* skip over argument types to find out the 
         * return type */
        while (*p != ')') p++;
        /* skip ')' */
        p++;
        va_start(args, descriptor);
        switch (*p) {
        case 'V':
          (*env)->CallVoidMethodV(env, obj, mid, args);
#if __MSVC_RUNTIME_CHECKS
          result.i = 42;        /* [PM] 3.9.1 so that the return of uninitialize result does not trigger debug break */
#endif/* __MSVC_RUNTIME_CHECKS */
          break;
        case '[':
        case 'L':
          result.l = (*env)->CallObjectMethodV(
                                               env, obj, mid, args);
          break;
        case 'Z':
          result.z = (*env)->CallBooleanMethodV(
                                                env, obj, mid, args);
          break;
        case 'B':
          result.b = (*env)->CallByteMethodV(
                                             env, obj, mid, args);
          break;
        case 'C':
          result.c = (*env)->CallCharMethodV(
                                             env, obj, mid, args);
          break;
        case 'S':
          result.s = (*env)->CallShortMethodV(
                                              env, obj, mid, args);
          break;
        case 'I':
          result.i = (*env)->CallIntMethodV(
                                            env, obj, mid, args);
          break;
        case 'J':
          result.j = (*env)->CallLongMethodV(
                                             env, obj, mid, args);
          break;
        case 'F':
          result.f = (*env)->CallFloatMethodV(
                                              env, obj, mid, args);
          break;
        case 'D':
          result.d = (*env)->CallDoubleMethodV(
                                               env, obj, mid, args);
          break;
        default:
          (*env)->FatalError(env, "illegal descriptor");
          result.i=42;
        }
        va_end(args);
      }
      (*env)->DeleteLocalRef(env, clazz);
      if (hasException)
        {
          *hasException = (*env)->ExceptionCheck(env);
        }
    }
  else
    {
#if JASPER_DBG
      {
        fprintf(stderr, "WARNING: EnsureLocalCapacity() != JNI_OK (%s:%d)\n", __FILE__, (int)__LINE__);fflush(stderr);
        if (!(*env)->ExceptionCheck(env))
          {
            fprintf(stderr, "ERROR: EnsureLocalCapacity()!=JNI_OK && ! ExceptionCheck() (%s:%d)\n", __FILE__, (int)__LINE__);fflush(stderr);
            exit(1);
          }
      }
#endif  /* JASPER_DBG */
      if (hasException)
        {
          *hasException = JNI_TRUE;
        }
    }
  return result;
}

/* A copy is in spnative.c, keep in synch */
static jvalue SPCDECL
CallStaticMethodByName(JNIEnv *env,
                       jboolean *hasException,
                       const char *clazzName,
                       const char *name,
                       const char *descriptor,
                       ...)
{
  va_list args;
  jclass clazz = NULL;          /* NULL for cleanup */
  jmethodID mid;
  jvalue result;
  #if JASPER_DBG
  int expect_exception = 0;     /* [PM] 3.10.1 */
  #endif

  result.l = NULL;

  if ((*env)->EnsureLocalCapacity(env, 2) == JNI_OK)
    {
      if (((clazz = (*env)->FindClass(env, clazzName)) != NULL)
          &&
          ((mid = (*env)->GetStaticMethodID(env, clazz, name, descriptor)) != NULL))
        {
          const char *p = descriptor;
          /* skip over argument types to find out the 
           * return type */
          while (*p != ')') p++;
          /* skip ')' */
          p++;
          va_start(args, descriptor);
          switch (*p) {
          case 'V':
            (*env)->CallStaticVoidMethodV(env, clazz, mid, args);
            #ifdef  __MSVC_RUNTIME_CHECKS 
            result.l = NULL;
            #endif/* __MSVC_RUNTIME_CHECKS  */
            break;
          case '[':
          case 'L':
            result.l = (*env)->CallStaticObjectMethodV(
                                                       env, clazz, mid, args);
            break;
          case 'Z':
            result.z = (*env)->CallStaticBooleanMethodV(
                                                        env, clazz, mid, args);
            break;
          case 'B':
            result.b = (*env)->CallStaticByteMethodV(
                                                     env, clazz, mid, args);
            break;
          case 'C':
            result.c = (*env)->CallStaticCharMethodV(
                                                     env, clazz, mid, args);
            break;
          case 'S':
            result.s = (*env)->CallStaticShortMethodV(
                                                      env, clazz, mid, args);
            break;
          case 'I':
            result.i = (*env)->CallStaticIntMethodV(
                                                    env, clazz, mid, args);
            break;
          case 'J':
            result.j = (*env)->CallStaticLongMethodV(
                                                     env, clazz, mid, args);
            break;
          case 'F':
            result.f = (*env)->CallStaticFloatMethodV(
                                                      env, clazz, mid, args);
            break;
          case 'D':
            result.d = (*env)->CallStaticDoubleMethodV(
                                                       env, clazz, mid, args);
            break;
          default:
            /* We should not get here unless GetMethodID goofed up */
            (*env)->FatalError(env, "illegal descriptor");
          }
          va_end(args);
        }
      else
        {
      #if JASPER_DBG
      expect_exception = 1;
      #endif/* JASPER_DBG */
        }
      if (clazz) (*env)->DeleteLocalRef(env, clazz);
    }
  else
    {
      #if JASPER_DBG
      expect_exception = 1;
      #endif/* JASPER_DBG */
    }

  if (hasException)
    {
      *hasException = (*env)->ExceptionCheck(env);
    }
  #if JASPER_DBG
  if (expect_exception && !(*env)->ExceptionCheck(env))
    {
      DEBUG_BREAK_MSG((stderr, "ERROR: expected exception but ExceptionCheck()"));
    }
  #endif/* JASPER_DBG */
  return result;
}


/* return 0 on failure. SICStus monitor should already be entered */
static SP_uinteger jasper_leave_prolog(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv)
{
  SP_uinteger context;

  (void)jnienv;
  /* we are in Prolog context */

  context = ++STATE_VARIABLE(context_counter);

#if JASPER_DBG>1
  fprintf(stderr, "Pushing context to level %lu\n", context);
#endif
  /* we are in Java context */

  return context;
}

/*
 * NOTE: DOC
 */
static SP_uinteger jasper_return_to_prolog(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_uinteger context)
{
  (void)jnienv;
  (void)context;
#if JASPER_DBG>0
  fprintf(stderr, "Returning to prolog level %"SPRIuINTEGER"\n", context);
#endif

  /* Prolog run-time monitor is owned by this thread and context==context_counter */

#if JASPER_DBG
  {
  if (context != STATE_VARIABLE(context_counter))
    {
      fprintf(stderr, "ERROR: Returned to prolog level context==%"SPRIuINTEGER", context_counter==%"SPRIuINTEGER" JNI %p\n", context, STATE_VARIABLE(context_counter), jnienv);
    }
  fprintf(stderr, "Returned to prolog level %"SPRIuINTEGER" JNI %p\n", context, jnienv);
  }
#endif /* JASPER_DBG */

  STATE_VARIABLE(context_counter)--;

                                /* We are in Prolog context */
  return STATE_VARIABLE(context_counter)+1;
}

/* Called when monitor is owned by thread (i.e., in Prolog context) */
static int jasper_notify_java_return_to_prolog(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, const char *method)
{
  jboolean hasException;
  jobject sp_obj = sp_get_jasper_magic(SICSTUS_VERSION);

  if (!sp_obj) return 0;

  (void) CallMethodByName(jnienv, &hasException, /*STATE_VARIABLE(jasper_sp_global)*/ sp_obj, method, "()V");

  if (hasException)
    {
#if JASPER_DBG
      jasper_DescribeException(jnienv);
#endif
      return 0;
    }
  return 1;
}

/* Called when monitor is owned by thread (i.e., in Prolog context) */
static int jasper_notify_java_leave_prolog(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, const char *method)
{
  jboolean hasException;
  jobject sp_obj = sp_get_jasper_magic(SICSTUS_VERSION);

  if (!sp_obj) return 0;

  (void) CallMethodByName(jnienv, &hasException, /*STATE_VARIABLE(jasper_sp_global)*/ sp_obj, method, "()V");

  if (hasException)
    {
#if JASPER_DBG
      jasper_DescribeException(jnienv);
#endif
      return 0;
    }
  return 1;
}


/* [PM] 3.8.5 called by (meta/generated) glue code to release prolog
 *            monitor and enter Java context
 *            NOTE: You may not call any SP runtime routines after
 *            this procedure returns.
 *            Must preserve pending exception
 */
static SP_uinteger jasper_push_context1(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, const char *method)
{
  SP_uinteger context;
  jthrowable pending = NULL;
  
  JASPER_DBGLINE(jasper_push_context1);

  /* we are in Prolog context */

  pending = (*jnienv)->ExceptionOccurred(jnienv);
  JASPER_DBGLINE(jasper_push_context1);
  if (pending)
    {
      JASPER_DBGLINE(jasper_push_context1);
#if JASPER_DBG
      fprintf(stderr, "Entered jasper_push_context1 with pending exception (this is expected)\n");
#endif
      (*jnienv)->ExceptionClear(jnienv);
    }
  JASPER_DBGLINE(jasper_push_context1);
  /* important that this is done while still owning prolog */
  jasper_notify_java_leave_prolog(SPAPI_ARG jnienv, method);
  
  JASPER_DBGLINE(jasper_push_context1);

#if JASPER_DBG
  fprintf(stderr, "jasper_push_context1 (: SP_term_refs==%ud\n", (int)SP_new_term_refs(0));
#endif

  /* we are in Prolog context */
  {
    /* NOTE: Error handling? */
    context = jasper_leave_prolog(SPAPI_ARG jnienv);
  }
  /* We are in Java context */

#if JASPER_DBG
  fprintf(stderr, "jasper_push_context1 context==%"SPRIuINTEGER"\n", (SP_uinteger)context);
#endif

  JASPER_DBGLINE(jasper_push_context1);

  if (pending
      /* (arbitrary) give precedence to exceptions from SICStus.<method>  */
      && !(*jnienv)->ExceptionCheck(jnienv))
    {
      JASPER_DBGLINE(jasper_push_context1);
      (*jnienv)->Throw(jnienv, pending);
    }

  if (pending)
    {
      JASPER_DBGLINE(jasper_push_context1);
      (*jnienv)->DeleteLocalRef(jnienv, pending);
    }
  JASPER_DBGLINE(jasper_push_context1);

  return context;
}

/* [PM] 3.8.5 called by (meta/generated) glue code (from Java context) to
 *      re-acquire prolog monitor and return to Prolog context
 *      Must preserve pending exception
 */
static SP_uinteger jasper_pop_context1(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_uinteger context, const char *method)
{
  SP_uinteger new_context;
  int res;
  jthrowable pending = NULL;

  /* we are in Java context */
  
  pending = (*jnienv)->ExceptionOccurred(jnienv);
  if (pending)
    {
#if JASPER_DBG
      fprintf(stderr, "Entered jasper_pop_context1 with pending exception (this is expected)\n");
#endif
      (*jnienv)->ExceptionClear(jnienv);
    }
  /* In Java context */
  {
    new_context = jasper_return_to_prolog(SPAPI_ARG jnienv, context);
  }
  /* In Prolog context */


#if JASPER_DBG
  fprintf(stderr, "jasper_pop_context1 context==%"SPRIuINTEGER"\n", (SP_uinteger)context);
  fprintf(stderr, "jasper_pop_context1 ): SP_term_refs==%ud\n", (int)SP_new_term_refs(0));
#endif

  /* important that this is done when owning prolog */
  res = jasper_notify_java_return_to_prolog(SPAPI_ARG jnienv, method);
  (void)res;


  if (pending
      /* (arbitrary) give precedence to exceptions from SICStus.<method>  */
      && !(*jnienv)->ExceptionCheck(jnienv))
    {
      (*jnienv)->Throw(jnienv, pending);
    }

  if (pending)
    {
      (*jnienv)->DeleteLocalRef(jnienv, pending);
    }

  /* we are in Prolog context */
  return new_context;
}

/* [PM] 3.8.5 called by meta glue code to enter Java context
   Must be called before creatig any SPTerm (for parameter
   passing). This is so the SPTerms get the right SPQuery context. 
   As of 3.8.5beta2/final leaving prolog monitor is done separately
   (although currently a no-op).
 */
static SP_uinteger jasper_push_context(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv)
{
  return jasper_push_context1(SPAPI_ARG jnienv, "MetaGlue_PushContext");
}

static int jasper_leave_context_monitor(JNIEnv *jnienv, SP_uinteger context)
{
  (void)jnienv;
  (void)context;

  /* a no-op since we had to abandon the free-threading after 3.8.5beta1*/
  return 1;
}

static int jasper_enter_context_monitor(JNIEnv *jnienv, SP_uinteger context)
{
  (void)jnienv;
  (void)context;
  
  /* a no-op since we had to abandon the free-threading after 3.8.5beta1*/
  return 1;
}

/* [PM] 3.8.5 called by meta glue code to leave Java context
 *            (no longer: and acquire the prolog monitor)
 *            to return to prolog
 *            NOTE: You may not call any SP runtime routines between
 *            returning from Java and this procedure returns.
 */
static SP_uinteger jasper_pop_context(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, SP_uinteger context)
{
  return jasper_pop_context1(SPAPI_ARG jnienv, context, "MetaGlue_PopContext");
}


/* Check if there is a pending Java exception. If there is, we print a
 * description of it on stderr (actually we would want to include it
 * in a Prolog exception, but that's more tricky).  If error is
 * non-zero, we throw a Prolog exception regardless. If error is zero,
 * we only throw a Prolog exception if there was a pending Java
 * exception. 
 * Return: non-zero if ERROR != 0 or Java exception 
 * Post: SP exception raised if either ERROR != 0 or Java exception.
 *       Java exception cleared
 * NOTE: Will always raise an java_exception('description'), never a $java_object
 */
static int jasper_int_handle_exception(SPAPI_ARG_PROTO_DECL JNIEnv *env, char *desc, int error)
{
  jboolean have_jexcp;
  
  /* Handle exceptions from recursive calls to Java */
  /* [PM] 3.8.5 Was: excp = (*env)->ExceptionOccurred(env); */
  have_jexcp = (*env)->ExceptionCheck(env);

  if (have_jexcp) {
    jasper_DescribeException(env); /* NOTE: Questionable to unconditionally write things (used to do ExceptionDescribe) */

    /* [PM] 3.8.5 Never leave lingering Java exceptions. */ 
    (*env)->ExceptionClear(env);
  }

  if (error != 0 || have_jexcp)
    {
      jasper_int_raise_exception(SPAPI_ARG "java_exception",desc);
      return 1;
    }
  return 0;
}

#define DECLARE_ATOM(VAR,NAME)                      \
{                                                   \
   STATE_VARIABLE(VAR) = SP_atom_from_string(NAME); \
   SP_register_atom(STATE_VARIABLE(VAR)) ;          \
}


/* Return 0 on error and sets *pobj to NULL
   Return true on success (null objref is not an error)
*/
static int termref_to_jobject(SPAPI_ARG_PROTO_DECL SP_term_ref tr, jobject *pobj)
{
  int rc;
  jobject jobj;

#if JASPER_DBG
  if ( (sizeof jobj) != (sizeof(SP_integer)))
    {
      fprintf(stderr, "FATAL ERROR ( sizeof jobj (%" SPRIdSZ ") != sizeof SP_integer (%" SPRIdSZ "))\n", (size_t) (sizeof jobj), (size_t) (sizeof (SP_integer)));
    }
#endif

  {
    SP_term_ref t0 = SP_new_term_ref();
    jlong tmp;
    if (SP_get_arg(1,tr,t0) && get_jlong(SPAPI_ARG t0,&tmp,FALSE))
      {
        rc = 1;
        if (tmp == 0)
          {
            jobj = NULL;
          }
        else
          {
            /* [PM] 3.9.1 avoid "warning: cast to pointer from integer of different size" */
            jobj = (jobject)(SP_integer)tmp;
          }
      }
    else
      {
        rc = 0;
#if JASPER_DBG
        fprintf(stderr, "termref_to_jobject failed\n");
#endif  
        jobj = NULL;
        tmp = 0;
      }
    /* 
       not needed:
       SP_reset_term_refs(t0);
    */
    *pobj = jobj;
    return rc;
  }
}

/* [PM] 3.8.5 added tr arg */
/* Return 0 if NULL object (tr will be '$java_object'(0)) */
static int jobject_to_termref(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject jobj, SP_term_ref tr)
{
  int rc;

  (void)jnienv;

  if (jobj != NULL)
    {
      ASSERT_NO_EXCP(jnienv, "jobject_to_termref");
      rc = 1;
      DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jobject_to_termref");

#if JASPER_DBG && 0                    /* NOTE: debug, enable this to globalize all */
      {
        jobject gobj = (*jnienv)->NewGlobalRef(jnienv,jobj);
        fprintf(stderr, "Globalizing %" SPRIdINTEGER " got %" SPRIdINTEGER "\n", (SP_integer) jobj, (SP_integer) gobj);
        ASSERT_NO_EXCP(jnienv, "jobject_to_termref 2");
        DBG_PRINT_OBJECT_CLASS(jnienv, gobj, "Class of global");
        jobj = gobj;
      }
#endif
      put_jlong_ptr(SPAPI_ARG tr,jobj);

#if JASPER_DBG
      {
        jlong tmp = 0;
        if ( (!get_jlong(SPAPI_ARG tr, &tmp, FALSE))
             ||
             /* [PM] 3.9.1 avoid "warning: cast to pointer from integer of different size" */
             ((jobject)(SP_integer)tmp) != jobj
             )
          {
            fprintf(stderr, "ERROR: converting jobj to term and back failed (%p -> %" SPRIxINTEGER " -> %p)\n", (void*)jobj, (SP_uinteger)jobj, 
                    /* [PM] 3.9.1 avoid "warning: cast to pointer from integer of different size" */
                    (void*)(SP_integer)tmp
                    );
          }
      }
#endif
      
    }
  else
    {
      rc = 0;
#if JASPER_DBG
      fprintf(stderr, "jobj==NULL in jobject_to_termref\n");
#endif

#if 1                           /* 3.8.6 Use 0 to signify null object (as opposed to invalid) */
      SP_put_integer(tr,0);
#else
      SP_put_integer(tr,-1);
#endif
    }
  SP_cons_functor(tr,STATE_VARIABLE(jasper_atom_dollar_java_object),1,tr);
  return rc;
}

#if JASPER_DBG
/* A global ref to the SICStus object (if any) */
static jobject jasper_get_sicstus_object(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv)
{
  ASSERT_NO_EXCP(jnienv, "jasper_get_sicstus_object");
  (void)jnienv;
  return sp_get_jasper_magic(SICSTUS_VERSION);
}
#endif /* JASPER_DBG */

/* FOREIGN */
void SPCDECL init_jasper(SPAPI_ARG_PROTO_DECL int when)
{
  DbgBreakPoint();

  (void)when;

  INIT_STASH();

#ifndef JNI_VERSION_1_2
  #error "Jasper requires JDK 1.2 or later"
#endif


  /* Init state variables */
  {
    /* M-X sort-lines */
    DECLARE_ATOM(jasper_atom_atom,"atom");
    DECLARE_ATOM(jasper_atom_boolean,"boolean");
    DECLARE_ATOM(jasper_atom_byte,"byte");
    DECLARE_ATOM(jasper_atom_char,"char");
    DECLARE_ATOM(jasper_atom_codes,"codes");
    DECLARE_ATOM(jasper_atom_chars,"chars");
    DECLARE_ATOM(jasper_atom_dollar_java_object,"$java_object"); /* xref jasper.pl */
    DECLARE_ATOM(jasper_atom_double,"double");
    DECLARE_ATOM(jasper_atom_false,"false");
    DECLARE_ATOM(jasper_atom_float,"float");
    DECLARE_ATOM(jasper_atom_integer,"integer");
    DECLARE_ATOM(jasper_atom_jvm,"$jvm");
    DECLARE_ATOM(jasper_atom_long,"long");
    DECLARE_ATOM(jasper_atom_object,"object");
    DECLARE_ATOM(jasper_atom_plus_sign,"+");
    DECLARE_ATOM(jasper_atom_short,"short");
    DECLARE_ATOM(jasper_atom_string,"string");
    DECLARE_ATOM(jasper_atom_term,"term");
    DECLARE_ATOM(jasper_atom_true,"true");

    INIT_STATE_VARIABLE(jvm, NULL);
    INIT_STATE_VARIABLE(state_jnienv, NULL);
    INIT_STATE_VARIABLE(context_counter, 0);
    INIT_STATE_VARIABLE(versionMatchingDone, JNI_FALSE); /* [PD] 3.11.1 */
  }
  /* Sets STATE_VARIABLE(jvm)  */
  jasper_set_jvm(SPAPI_ARG0);

  return;
}


/* FOREIGN */
void SPCDECL deinit_jasper(SPAPI_ARG_PROTO_DECL int when)
{
  DbgBreakPoint();
  #if JASPER_DBG>1
  fprintf(stderr, "\ndeinit_jasper(%d)\n", when); fflush(stderr);
  #endif
  (void)when;
#if MULTI_SP_AWARE
  (void)SPAPI_ARG_NAME;         /* avoid -Wunused */
#endif /* MULTI_SP_AWARE */

  /* no-op for now */
}

/* [PM] 3.8.5 Now ONLY used by this file. See SET_JNIENV
   Return current jnienv or NULL
   if NULL and not FROM_GLUE then raises SP exception.
   [PM] 3.8.7 Safe to call with silent==true even if init_jasper barfed about MM_SBRK
*/
static JNIEnv * jasper_int_get_jnienv(SPAPI_ARG_PROTO_DECL int from_glue /* a.k.a silent */)
{
  jint rv = 0;
  JNIEnv *jnienv = NULL;
  char *err_msg = NULL;


  /* [PM] See notes at top of file */

#if JASPER_DBG>1
  fprintf(stderr, "in jasperi_int_get_jnienv(): JVM = %p (from_glue=%d)\n", STATE_VARIABLE(jvm), (int)from_glue);
#endif
    
  if (!STATE_VARIABLE(jvm)) goto cleanup;

  /* [PM] 3.9 Note that, even if we constrain SP API calls to happen
     in only one thread we will come here unattached if Java was
     already initialized by someone else (in some other thread). */

  /* Attach ourselves to the JVM, unless this has already been done. */
  rv = (*STATE_VARIABLE(jvm))->GetEnv(STATE_VARIABLE(jvm), (void **)&jnienv, SP_JNI_VERSION_NEEDED);
  if (rv) jnienv = NULL;

  if (rv == JNI_EDETACHED)
    {
#if JASPER_DBG>1
      fprintf(stderr, "in jasperi_int_get_jnienv(): JVM = %p (from_glue=%d) ATTACHING\n", STATE_VARIABLE(jvm), (int)from_glue);
#endif

      if (0 > (*STATE_VARIABLE(jvm))->AttachCurrentThread(STATE_VARIABLE(jvm),(void **)&jnienv,NULL))
        {
          jnienv = NULL;
          err_msg = "Failed to attach thread to Java VM";
          goto cleanup;
        }
    }
  else if (rv == JNI_EVERSION)
    {
      err_msg = "Jasper requires JNI version"
#if defined(JNI_VERSION_1_4) && SP_JNI_VERSION_NEEDED ==  (JNI_VERSION_1_4 + 0)
        "1.4"
#else
        "1.2"
#endif

; /* xref SP_JNI_VERSION_NEEDED */
      goto cleanup;
    }

 cleanup:

  /* if we're being called from within glue-code we return NULL and
     let glue raise exception instead)
  */

#if JASPER_DBG>1
  if ( jnienv == NULL )
    {
      fprintf(stderr, "jnienv == NULL in jasperi_int_get_jnienv\n");
    }
#endif

  if ( jnienv == NULL && !from_glue )
    {
      jasper_int_raise_exception(SPAPI_ARG "java_exception", (err_msg ? err_msg :"Java engine not initialized"));
    }

  return jnienv;
}

typedef enum get_existing_jvm_error {
  GET_EXISTING_JVM_ERROR_SUCCESS,
  GET_EXISTING_JVM_ERROR_FAILED_TO_GET_VERSION,
  GET_EXISTING_JVM_ERROR_VERSION_MISMATCH
} get_existing_jvm_error;

/* [PD] 3.11.1 */
/* Fetches the existing JVM, if one exists, and makes sure that the version
   of this native code is the same as the version of the Java code.
   Returns JNI_TRUE if the version checking was OK or if there is no existing
   JVM.
   Returns JNI_FALSE if the version checking failed or there was a mismatch.
   The parameter jvmref will have the existing JVM, or [] if none exists.
   
   [PM] 4.3 Indicates error with return code. Does not call SP_save_error() or some such.
*/
static get_existing_jvm_error get_existing_jvm(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, JNIEnv **jnienv)
{
  get_existing_jvm_error error_code;
  *jnienv = jasper_int_get_jnienv(SPAPI_ARG 1 /* silent */);
  
  if (*jnienv != NULL)
    {
      /* [PD] 3.11.1 Check if we are in sync with the Java class files of
                     package se.sics.jasper, version wise. Do this once only.
      */
      if (STATE_VARIABLE(versionMatchingDone) == JNI_FALSE) {
        const char *versionChars;
        int versionMatch;
        jboolean hasException;
        jstring versionString;
        jvalue version= CallStaticMethodByName(*jnienv, &hasException,
                                               "se/sics/jasper/Version",
                                               "getSICStusVersion",
                                               "()Ljava/lang/String;");
        if (hasException) {
#if JASPER_DBG
          jasper_DescribeException(*jnienv);
#endif
          CODE_BARF(GET_EXISTING_JVM_ERROR_FAILED_TO_GET_VERSION);
        }
        versionString = version.l;
        versionChars = (**jnienv)->GetStringUTFChars(*jnienv, versionString, NULL);
#if JASPER_DBG
        fprintf(stderr, "SICSTUS_VERSION_STRING==%s\n", SICSTUS_VERSION_STRING);
        fprintf(stderr, "versionChars==%s\n", versionChars);
#endif
        versionMatch = strcmp(SICSTUS_VERSION_STRING, versionChars);
        (**jnienv)->ReleaseStringUTFChars(*jnienv, versionString, versionChars);
        if (versionMatch != 0) {
          CODE_BARF(GET_EXISTING_JVM_ERROR_VERSION_MISMATCH);
        }
        STATE_VARIABLE(versionMatchingDone) = JNI_TRUE;
      }

      put_jlong_ptr(SPAPI_ARG jvmref,STATE_VARIABLE(jvm));
      SP_cons_functor(jvmref,STATE_VARIABLE(jasper_atom_jvm),1,jvmref);
    }
  /* else jvmref is [] which is recognized in jasper.pl */

  error_code = GET_EXISTING_JVM_ERROR_SUCCESS;
 cleanup:
  return error_code;
 code_barf_:
  goto cleanup;
}


static char const * get_existing_jvm_error_message(get_existing_jvm_error error_code)
{
          char const *error_message;
          switch (error_code) {
        case GET_EXISTING_JVM_ERROR_SUCCESS:
          error_message = "No error";
          break;
        case GET_EXISTING_JVM_ERROR_VERSION_MISMATCH:
          error_message = "Version mismatch in library(jasper)";
          break;
        case GET_EXISTING_JVM_ERROR_FAILED_TO_GET_VERSION:
          error_message = "Failed to get the version of package se.sics.jasper";
          break;
        default:
          error_message = "Internal error library(jasper)";
          break;
        }
          return error_message;
        }

/* FOREIGN */
void SPCDECL jasperi_jvm_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref)
{
  JNIEnv *jnienv;
  get_existing_jvm_error error_code = get_existing_jvm(SPAPI_ARG jvmref, &jnienv);

  if (error_code != GET_EXISTING_JVM_ERROR_SUCCESS) {
    char const *error_message = get_existing_jvm_error_message(error_code);
    SP_save_and_raise_error(SYSTEM_ERROR, error_message, 0, "jasper", "jasperi_jvm", 0, 0);
  }
}

/* [PM] 3.8.5 New way */

/* strcat with automatic growing of SP_malloced buffer */
static char *jasper_strcat(SPAPI_ARG_PROTO_DECL char *buf, const char *str)
{
  size_t length = strlen(buf);
  size_t size = length+1;

  length += strlen(str);
#if JASPER_DBG>42
  fprintf(stderr, "jasper_strcat(SPAPI_ARG \"%s\" (%" SPRIdSZ "), \"%s\" (%" SPRIdSZ ")) \n", buf, (size_t)strlen(buf), str, (size_t)strlen(str));
#endif

  if (length >= size)
    {
      char *tmp_buf;

      size = length+1;
#if JASPER_DBG>42
      fprintf(stderr, "SP_realloc(\"%s\" (%" SPRIdSZ "), %" SPRIdSZ ")\n", buf, (size_t)strlen(buf), (size_t) size);
#endif

      tmp_buf = SP_realloc(buf, size);
      if (tmp_buf == NULL)
        {
          SP_free(buf);
          return NULL;
        }
      buf = tmp_buf;
    }
#if JASPER_DBG>42
  fprintf(stderr, "jasper_strcat: strcat(\"%s\" (%" SPRIdSZ "), \"%s\" (%" SPRIdSZ ")) \n", buf, (size_t)strlen(buf), str, (size_t)strlen(str));
#endif

  strcat(buf,str);
  return buf;
}



/* 3.8.5 If this has to create a JVM and thus a SICStus object then it
   should enter the SICStus monitor (representing the fact that the
   current thread is still in the prolog run-time)
*/
static SP_term_ref jasperi_initialize(SPAPI_ARG_PROTO_DECL int numopts, SP_term_ref classpath, SP_term_ref tr)
{
  int i;
  SP_term_ref elem, tail, jref;
  char *full_classpath = NULL;  /* Complete classpath */
  char *prop_cpath = "-Djava.class.path=";
  char *current_dir = NULL;
  char *user_dir_option = NULL;
  char const *error_message;

  JavaVMOption *options = NULL;
  JavaVMInitArgs vm_args;
  JNIEnv *jnienv = NULL;
  int jvm_already_present;

#if SP_WIN32
  char *pathsep = ";";
#else
  char *pathsep = ":";
#endif

  /* We depend on wm_args.ignoreUnrecognized to be true when JASPER_DBG */
  char *extra_debug_opts[] = {  /* [PM] 4.0.3 now a misnomer */
#if JASPER_DBG
    "-Xcheck:jni",
    "-verbose:jni",
    "-Dse.sics.jasper.SICStus.debugLevelDefault=" STRINGISIZE(JASPER_DBG),
    "-Dse.sics.jasper.SICStus.checkSPTermAgeDefault=true",
    "-Dse.sics.jasper.SICStus.reuseTermRefsDefault=true",
    /* , "-Xfuture" does not work even with ignoreUnrecognized !? */
#endif /* JASPER_DBG */
    /* [PM] 4.0.3 We want this iff we are a development system. For now just assume that runtime systems can cope with it too. */
    "-Xrs"                 /* [PM] 3.8.6 Only JDK 1.3.1. Reduced use of signals */
  };

  const int NUM_EXTRA_DBG_OPTS = (sizeof extra_debug_opts)/(sizeof *extra_debug_opts);

  /* We add 2 extra options ourselves, -Djava.class.path,
     and -Duser.dir */
  const int NUM_EXTRA_OPTS =
    2
    +NUM_EXTRA_DBG_OPTS;

#if JASPER_DBG
  fprintf(stderr, "Enter: jasperi_initialize\n");
#endif

  memset(&vm_args, 0, sizeof vm_args);

  elem = SP_new_term_ref();
  tail = SP_new_term_ref();
  jref = SP_new_term_ref();

  jasper_set_jvm(SPAPI_ARG0);

  /* "Is there anybody out there?" --Pink Floyd */
  if (STATE_VARIABLE(jvm) != NULL)
    {
#if JASPER_DBG
      fprintf(stderr, "jasperi_initialize found JVM, not creating\n");
#endif

      jvm_already_present = 1;
      /* Yes, someone was out there. Warn the user that any
       * options weren't used, since no JVM was created. */
#if JASPER_DBG
      if (numopts > 1)
        {
          fprintf(stderr, "*** No new JVM was created (JVM startup options ignored)\n");
        }
#endif
    }
  else
    { /* No JVM available. Lets create one. */



#if JASPER_DBG
      fprintf(stderr, "jasperi_initialize found no JVM, creating\n");
#endif
      jvm_already_present = 0;

      numopts += NUM_EXTRA_OPTS;
      options = SP_malloc((numopts) * sizeof(JavaVMOption));
      if (options == NULL)
        {
          MSG_BARF("memory allocation failure");
        }
      /* [PM] 3.10.1 Make it possible to specify 1.4 version (MacOS X JDK 1.4.1 will use the 1.3.1 JDK unless JNI_VERSION_1_4 is used) */
      {
        jint want_version = SP_JNI_VERSION_WANTED_DEFAULT;

        {
          const char *val;
          if ((val = getenv("SP_JNI_VERSION")) != 0)
            {
              if (strcmp(val, "JNI_VERSION_1_2") == 0)
                {
                  want_version = JNI_VERSION_1_2;
                }
              else
                if (strcmp(val, "JNI_VERSION_1_4") == 0)
                  {
                    want_version = SP_JNI_VERSION_1_4;
                  }
#ifdef JNI_VERSION_1_5          /* [PM] 4.0.3 Never existed, I think. */
              if (strcmp(val, "JNI_VERSION_1_5") == 0)
                {
                  want_version = JNI_VERSION_1_5;
                }
#endif /* JNI_VERSION_1_5 */
#ifdef JNI_VERSION_1_6
              if (strcmp(val, "JNI_VERSION_1_6") == 0)
                {
                  want_version = JNI_VERSION_1_6;
                }
#endif /* JNI_VERSION_1_6 */
              else
                {
#if JASPER_DBG
                  fprintf(stderr, "ERROR: %s:%d SP_JNI_VERSION=\"%s\" not supported\n", __FILE__, (int)__LINE__, val);fflush(stderr);
#endif /* JASPER_DBG */
                }
            }
        }
        /* Can be overridden from tr option list below */
        vm_args.version = want_version;
      }

      {
        jboolean ignoreUnrecognized = JNI_TRUE; /* [PM] 4.0.3 always */
        vm_args.ignoreUnrecognized = ignoreUnrecognized;
      }

      /* 3.8.5 new way to build classpath */
      {
        const char *sep = "";               /* set to pathsep at first path element */
#if JASPER_DBG
        fprintf(stderr, "Building classpath list \n");
#endif
        full_classpath = (char *) SP_malloc(1);

        if (full_classpath != NULL)
          {
            char *tmp;
            full_classpath[0] = '\0';

            LAZY_NULL_CHECK(tmp = jasper_strcat(SPAPI_ARG full_classpath, prop_cpath));
            full_classpath = tmp;
          }

        {
          char const *p_ienc;       /* [PM] 3.10.1 Ienc! */
          char const *p;            /* [PM] 3.10.1 Senc WCX_FILE */

          if (SP_is_list(classpath))
            {
              SP_foreach(classpath,elem,tail,i)
                {
                  if (!SP_get_string(elem,&p_ienc)
                      || ((p = SP_to_os(p_ienc, WCX_FILE)) == NULL))
                    {
                      MSG_BARF("Malformed classpath");
                    }
#if JASPER_DBG
                  fprintf(stderr, "Concatentating classpath list element '%s' + '%s' (Ienc=='%s') %s\n", sep, p, p_ienc, (full_classpath?"":"<<NULL>>"));
#endif
                  if (p[0])
                    {
                      char *tmp;

                      if (full_classpath)
                        {
                          LAZY_NULL_CHECK(tmp  = jasper_strcat(SPAPI_ARG full_classpath,sep));
                          full_classpath = tmp;
                        }
                      if (full_classpath)
                        {
                          LAZY_NULL_CHECK(tmp = jasper_strcat(SPAPI_ARG full_classpath,p));
                          full_classpath = tmp;
                        }
                      sep = pathsep;
                    }
                }
            }
          else
            {
              if (!SP_get_string(classpath,&p_ienc)
                  || ((p = SP_to_os(p_ienc, WCX_FILE)) == NULL))
                {
                  MSG_BARF("Malformed classpath");
                }
              if (p[0])
                {
                  char *tmp;
                  if (full_classpath)
                    {
                      LAZY_NULL_CHECK(tmp = jasper_strcat(SPAPI_ARG full_classpath,sep));
                      full_classpath = tmp;
                    }
                  if (full_classpath)
                    {
                      LAZY_NULL_CHECK(tmp = jasper_strcat(SPAPI_ARG full_classpath,p));
                      full_classpath = tmp;
                    }
                  sep = pathsep;
                }
            }
        }

        {                       /* NOTE: Should clean-up the way we get SP classpath */
          char *sp_cpath;    /* [...]/jasper.jar */
          sp_cpath = sp_get_classpath(); /* xref sicstus.c */
          if (sp_cpath != NULL && sp_cpath[0])
            {
              char *tmp;
              char const *p;              /* [PM] 3.10.1 WCX_FILE */
              if ((p = SP_to_os(sp_cpath, WCX_FILE)) == NULL)
                {
                  DEBUG_BREAK_MSG((stderr, "SP_to_os(\"%s\") failed\n", sp_cpath));
                  MSG_BARF("Malformed classpath");
                }

              if (full_classpath)
                {
                  LAZY_NULL_CHECK(tmp = jasper_strcat(SPAPI_ARG full_classpath,sep));
                  full_classpath = tmp;
                }
              if (full_classpath)
                {
                  LAZY_NULL_CHECK(tmp = jasper_strcat(SPAPI_ARG full_classpath,p));
                  full_classpath = tmp;
                }
              sep=pathsep;
            }
#if SICSTUS_RELEASE_BUILD && !FORCE_BUILD && SICSTUS_VERSION > 40401 /* [PM] 4.3.2 postpone */
#error "This function and several others leaks SP_malloced memory on error"
#endif
          if (sp_cpath != NULL)
            {
              SP_free(sp_cpath); /* [PM] 4.0 now SP_malloc'd */
            }
        }

        {
          const char *env_cpath = getenv("CLASSPATH"); /* [PM] 3.10.1 assumed to be suitably encoded */
          if (env_cpath && env_cpath[0])
            {
              char *tmp;
              if (full_classpath)
                {
                  LAZY_NULL_CHECK(tmp = jasper_strcat(SPAPI_ARG full_classpath,sep));
                  full_classpath = tmp;
                }
              if (full_classpath)
                {
                  LAZY_NULL_CHECK(tmp = jasper_strcat(SPAPI_ARG full_classpath,env_cpath));
                  full_classpath = tmp;
                }
              sep = pathsep;
            }
        }
        (void)sep; /* suppress clang warning about value stored is never read. */
        /* done */
        if (!full_classpath)
          {
            MSG_BARF("memory allocation failure");
          }
        options[0].optionString = full_classpath;
      }
      {
        current_dir = SP_get_current_dir();
        if (current_dir == NULL)
          {
            MSG_BARF("Could not determine current directory");
          }
        user_dir_option = SP_strdup("-Duser.dir=");
        if (!user_dir_option)
          {
            MSG_BARF("memory allocation failure");
          }
        {
          char *tmp = jasper_strcat(SPAPI_ARG user_dir_option, current_dir);
          if (tmp == NULL)
            {
              MSG_BARF("memory allocation failure");
            }
          user_dir_option = tmp;
        }
        
        options[1].optionString = user_dir_option;
      }

      {
        int j;
        for ( j = 0; j < NUM_EXTRA_DBG_OPTS; j++)
          {
#if JASPER_DBG
            fprintf(stderr, "DBG: Adding extra JVM option #%d=\"%s\"\n", j, extra_debug_opts[j]);
#endif /* JASPER_DBG */
            options[(NUM_EXTRA_OPTS-NUM_EXTRA_DBG_OPTS)+j].optionString = extra_debug_opts[j];
          }
      }

      /* If tr < 0, it means that this function is being called from within the
       * glue code mechanism, without user-specified options. */
    
      {
        int skipped = 0;      /* [PM] 3.10.1 how many of the options on tr that should not be passed as options.optionString */
        /* Assemble user-specified JVM options */
        for (SP_put_term(tail,tr),i = NUM_EXTRA_OPTS; SP_get_list(tail,elem,tail) && (i-NUM_EXTRA_OPTS)<numopts; i++)
          {
            char const *optionString_ienc;
            if (!SP_get_string(elem,&optionString_ienc))
              {
                MSG_BARF("Malformed options");
              }
#if JASPER_DBG
            fprintf(stderr, "DBG: %s:%d Processing JVM option \"%s\"\n", __FILE__, (int)__LINE__, optionString_ienc);fflush(stderr);
#endif/* JASPER_DBG */
            if (strcmp(optionString_ienc, "JNI_VERSION_1_2") == 0)
              {
                skipped++;    /* this option does not consume a slot on options[] */
                vm_args.version = JNI_VERSION_1_2;
              }
            else if (strcmp(optionString_ienc, "JNI_VERSION_1_4") == 0)
              {
                skipped++;    /* this option does not consume a slot on options[] */
                vm_args.version = SP_JNI_VERSION_1_4;
              }
#ifdef JNI_VERSION_1_5          /* [PM] 4.0.3 Never existed, I think. */
            else if (strcmp(optionString_ienc, "JNI_VERSION_1_5") == 0)
              {
                skipped++;    /* this option does not consume a slot on options[] */
                vm_args.version = SP_JNI_VERSION_1_5;
              }
#endif /* JNI_VERSION_1_5 */
#ifdef JNI_VERSION_1_6
            else if (strcmp(optionString_ienc, "JNI_VERSION_1_6") == 0)
              {
                skipped++;    /* this option does not consume a slot on options[] */
                vm_args.version = JNI_VERSION_1_6;
              }
#endif /* JNI_VERSION_1_6 */
            else              /* common case, the option should be passed to java */
              {
                LAZY_NULL_CHECK(options[i-skipped].optionString = SP_strdup(optionString_ienc));
              }
          }
        /* [PM] 4.0 make sure I did not mis-count */
        SP_ASSERT(i == numopts);

        numopts -= skipped;   /* adjust for real number of options */
      }
#if JASPER_DBG
      for (i = 0; i < numopts; i++)
        {
          fprintf(stderr, "option-string[%d] = %s\n",i,options[i].optionString);
        }
#endif /* JASPER_DBG */


      /* [PM] 3.8.6 Call a hook that turns off some signal handlers
         (for JDK 1.3 compatibility, a.k.a -Xrs.) */
      {
        SP_pred_ref signals_hook_pred = SP_predicate("reduce_use_of_os_signals", 0, "prolog");
        
        if (signals_hook_pred)
          {
            SP_query_cut_fail(signals_hook_pred);
          }
      }
      {
        int rc;

        {
          vm_args.options = options;
          vm_args.nOptions = numopts;
          {
            JavaVM *local_jvm = NULL;
            rc = JNI_CreateJavaVM(&local_jvm, (void **)&jnienv, &vm_args);
            if (0 <= rc)
              {
                STATE_VARIABLE(jvm) = local_jvm;
              }
          }
        }
        if (0 > rc)
          {
            jnienv = NULL;
            MSG_BARF("Could not create Java VM");
          }
      }
#if JASPER_DBG || JASPER_DBG
      {
        char *version_string = "<<UNKNOWN>> (ERROR)";
        jint version = (*jnienv)->GetVersion(jnienv);
        if (version == JNI_VERSION_1_1) version_string = "JNI_VERSION_1_1";
        if (version == JNI_VERSION_1_2) version_string = "JNI_VERSION_1_2";
        if (version == SP_JNI_VERSION_1_4) version_string = "JNI_VERSION_1_4";
#ifdef JNI_VERSION_1_5          /* [PM] 4.0.3 Never existed, I think  */
        if (version == JNI_VERSION_1_5) version_string = "JNI_VERSION_1_5";
#endif /* JNI_VERSION_1_5 */
#ifdef JNI_VERSION_1_6
        if (version == JNI_VERSION_1_6) version_string = "JNI_VERSION_1_6";
#endif /* JNI_VERSION_1_6 */
        fprintf(stderr, "*** Created JVM=%p GetVersion()==%s (0x%" SPRIxINTEGER ")\n", STATE_VARIABLE(jvm), version_string, (SP_uinteger)version);fflush(stderr);
      }
#endif /* JASPER_DBG */
    }

  /* if we get here jvm is set-up */

  {
    get_existing_jvm_error error_code = get_existing_jvm(SPAPI_ARG jref, &jnienv);
    if (error_code != GET_EXISTING_JVM_ERROR_SUCCESS) {
      MSG_BARF(get_existing_jvm_error_message(error_code));
    }
  }
  if (jnienv == NULL) {
    MSG_BARF("Failed to attach thread to Java VM");
  }

  /*
   * if we're being called from the glue-code we need to ensure there is a
   * SICStus object as well. (3.8.4 unconditionally created a new SICStus
   * object, great fun with finalizer that does SP_deinitialize...)
   */
  /* [PD] 3.9 jasper_get_sicstus_object() now calls sp_get_jasper_magic()
     which returns NULL as an error (crash! bang! if DBG>0).
     If there is no JVM present there is no SICStus.
     If there is a JVM there better be a SICStus. 
     Only create a new SICStus object if there is no JVM.
  */
  if (!jvm_already_present)
    {
#if JASPER_DBG > 1
      fprintf(stderr, "*** Creating SICStus object (JNI %p, JVM %p)\n", (void*)STATE_VARIABLE(state_jnienv), (void*)STATE_VARIABLE(jvm));
#endif

      {
        jobject spobj, gspobj;

        spobj = jasperi_new_sicstus_object(SPAPI_ARG jnienv);
        if (spobj == NULL) goto cleanup;
        gspobj = (*jnienv)->NewGlobalRef(jnienv, spobj);
        if (gspobj == NULL) goto cleanup;
        if (!sp_set_jasper_magic(gspobj, SICSTUS_VERSION)) {
          MSG_BARF("Mismatch in Jasper versions");
        }
      }

      /* jasper_get_sicstus_object() should be non-NULL here */
#if JASPER_DBG
      if (jasper_get_sicstus_object(SPAPI_ARG jnienv) == NULL)
        {
          fprintf(stderr, "\n**** ERROR: Creating SICStus object failed to set the magic global SICStus object\n");
          abort();
        }
#endif
      
    }

 cleanup:
  
  if (options != NULL)
    {
      for (i = NUM_EXTRA_OPTS; i<numopts; i++)
        {
          SP_free(options[i].optionString);
        }
      SP_free(options);
    }
  if (full_classpath != NULL) SP_free(full_classpath);
  if (current_dir != NULL) SP_free(current_dir);
  if (user_dir_option != NULL) SP_free(user_dir_option);
  return jref;

 msg_barf_:
  SP_save_and_raise_error(SYSTEM_ERROR, error_message, 0, "jasper", "jasperi_initialize", 0, 0);
  goto cleanup;
}


/* FOREIGN */
/* only called from jasper.pl */
SP_term_ref SPCDECL jasperi_initialize_pl_C(SPAPI_ARG_PROTO_DECL SP_integer numopts, SP_term_ref classpath, SP_term_ref tr)
{
  return jasperi_initialize(SPAPI_ARG (int) numopts, classpath, tr);
}

/* FOREIGN */
SP_integer SPCDECL jasperi_deinitialize_C(SPAPI_ARG_PROTO_DECL SP_term_ref tr)
{

#if MULTI_SP_AWARE
  (void)SPAPI_ARG_NAME;         /* avoid -Wunused */
#endif
  (void)tr;
  
  return 0;
}




/* Return a local class ref for class CLASSNAME
   Post: jclass != NULL or SP exception
         Java exceptions cleared
*/
static jclass jasperi_lookup_class(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, char const *className) 
{
  char buf[1024];
  jclass clazz;

  ASSERT_NO_EXCP(jnienv, "jasperi_lookup_class");

  /* Find the class */
  clazz = (*jnienv)->FindClass(jnienv,className);
  if (!clazz)
    {
#if HAVE_SNPRINTF
      snprintf(buf, 1023, "could not find class %s", className);
#elif HAVE__SNPRINTF
      _snprintf(buf, 1023, "could not find class %s", className);
#else
#error "[PM] use snprintf"
      sprintf(buf, "could not find class %s", className);
#endif

#if JASPER_DBG>1
      fprintf(stderr, "DBG: %s\n", buf);
#endif

      jasper_int_handle_exception(SPAPI_ARG jnienv,buf,1);
      return 0;
    }
  return clazz;
}

#if JASPER_DBG
static void dbg_print_object_class(JNIEnv *jnienv, jobject jobj, char *extra_info)
{
  jclass clazz;
  jclass classclazz;
  jmethodID mid;

  ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 1");

#if JASPER_DBG
  fprintf(stderr, "dbg_print_object_class of object %" SPRIdINTEGER "\n", (SP_integer) jobj);
#endif
  if (jobj == NULL) return;

  clazz = (*jnienv)->GetObjectClass(jnienv,jobj);

#if JASPER_DBG
  fprintf(stderr, "dbg_print_object_class class of %" SPRIdINTEGER " is %" SPRIdINTEGER "\n", (SP_integer) jobj, (SP_integer) clazz);
  if ((*jnienv)->ExceptionCheck(jnienv)) jasper_DescribeException(jnienv);
#endif

  ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 2");
#if JASPER_DBG
  if ((*jnienv)->ExceptionCheck(jnienv)) jasper_DescribeException(jnienv);
#endif
  classclazz = (*jnienv)->GetObjectClass(jnienv,clazz);
  ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 3");
  mid = (*jnienv)->GetMethodID(jnienv,classclazz,"getName","()Ljava/lang/String;");
  ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 4");
  if (mid)
    {
      jobject jname;
        
      jname = (*jnienv)->CallObjectMethod(jnienv, clazz, mid);

      if (jname)
        {
          char *s;
          s = (char *)(*jnienv)->GetStringUTFChars(jnienv,(jstring)jname,NULL);
          ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 5");
          if (s)
            {
              fprintf(stderr, "DBG: %s Class of object (%" SPRIdINTEGER ") = '%s' (this is expected)\n", ( extra_info ? extra_info : "" ), (SP_integer)jobj, s);
              (*jnienv)->ReleaseStringUTFChars(jnienv,(jstring)jname,s);
                ASSERT_NO_EXCP(jnienv, "dbg_print_object_class 6");
            }
        }
    }
}
#endif



static jclass jasperi_lookup_class_from_obj(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jobject jobj) 
{
  char buf[1024];
  jclass clazz;
  
  ASSERT_NO_EXCP(jnienv, "jasperi_lookup_class_from_obj");

  if (!(clazz = (*jnienv)->GetObjectClass(jnienv,jobj)))
    {
      /* according to the docs this cannot happen */
#if HAVE_SNPRINTF
      snprintf(buf, 1023, "could not find class for object %" SPRIxINTEGER "", (SP_uinteger)jobj);
#elif HAVE__SNPRINTF
      _snprintf(buf, 1023, "could not find class for object %" SPRIxINTEGER"", (SP_uinteger)jobj);
#else
#error "[PM] use snprintf"        
      sprintf(buf, "could not find class for object %" SPRIxINTEGER "", (SP_uinteger)jobj);
#endif
      jasper_int_handle_exception(SPAPI_ARG jnienv,buf,1);
      return 0;
    }
  else
    {

#if JASPER_DBG>1
      {
        jmethodID mid;

        mid = (*jnienv)->GetMethodID(jnienv,(*jnienv)->GetObjectClass(jnienv,clazz),"getName","()Ljava/lang/String;");
      
        if (mid)
          {
            jobject jname;
        
            jname = (*jnienv)->CallObjectMethod(jnienv, clazz, mid);

            if (jname)
              {
                char *s;
            
                s = (char *)(*jnienv)->GetStringUTFChars(jnienv,(jstring)jname,NULL);
                if (s)
                  {
                    fprintf(stderr, "jasperi_lookup_class_from_obj Class of object (%" SPRIdINTEGER ") = %s\n",(SP_integer)jobj, s);
                    (*jnienv)->ReleaseStringUTFChars(jnienv,(jstring)jname,s);
                  }
                (*jnienv)->DeleteLocalRef(jnienv, jname); /* [PM] 3.9.2 */
              }
          }
       

      }
#endif

      return clazz;
    }
}

/* Post: mid != 0 or SP exception
         Java exceptions cleared
*/
static jmethodID jasperi_lookup_method(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv, jclass clazz, const char *methodName, const char *typeSig, int stat)
{
  char buf[1024];
  jmethodID mid;

#if JASPER_DBG>1
  fprintf(stderr, "jasperi_lookup_method(SPAPI_ARG JNIEnv %p, jclass %p, methodName %s, typeSig %s, stat %d)\n", (void*)jnienv, (void*) clazz, methodName, typeSig, stat);
#endif

  ASSERT_NO_EXCP(jnienv, "jasperi_lookup_method");

  /* find the method */
  if (stat)
    mid = (*jnienv)->GetStaticMethodID(jnienv,clazz,methodName,typeSig);
  else
    mid = (*jnienv)->GetMethodID(jnienv,clazz,methodName,typeSig);

  if (!mid)
    {
#if HAVE_SNPRINTF
      snprintf(buf, 1023, "could not find method %s, signature %s", methodName, typeSig);
#elif HAVE__SNPRINTF
      _snprintf(buf, 1023, "could not find method %s, signature %s", methodName, typeSig);
#else
      /* unsafe version */
#error "[PM] use snprintf"        
      sprintf(buf, "could not find method %s, signature %s", methodName, typeSig);
      /*       sprintf(buf, "could not find method"); */
#endif

      jasper_int_handle_exception(SPAPI_ARG jnienv,buf,1);
      return 0;
    }

  return mid;
}

/* FOREIGN */
void SPCDECL 
jasperi_object_class_name_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref objref, SP_term_ref name)
{
  jclass clazz = NULL;
  jclass clazzclazz = NULL;
  jobject jname = NULL;
  char *className = NULL;

  jmethodID mid;
  jobject jobj;
  JNIEnv *jnienv;
  JASPER_DBGLINE(jasperi_object_class_name_C);
  SET_JNIENV(jnienv,jvmref, {goto cleanup;});
  ASSERT_NO_EXCP(jnienv, "jasperi_object_class_name_C");
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (!termref_to_jobject(SPAPI_ARG objref, &jobj) || jobj==NULL)
    {
      JASPER_DBGLINE(jasperi_object_class_name_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid on null object",1);
      goto cleanup;
    }
  JASPER_DBGLINE(jasperi_object_class_name_C);
  clazz = (*jnienv)->GetObjectClass(jnienv, jobj);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  clazzclazz = (*jnienv)->GetObjectClass(jnienv, clazz); /* should be == clazz */
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if ( (mid = (*jnienv)->GetMethodID(jnienv,clazzclazz,"getName","()Ljava/lang/String;")) == NULL )
    {
      JASPER_DBGLINE(jasperi_object_class_name_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Cannot find method clazz.getName()",1);
      goto cleanup;
    }

  JASPER_DBGLINE(jasperi_object_class_name_C);
  jname = (*jnienv)->CallObjectMethod(jnienv, clazz, mid);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (jname == NULL)
    {
      JASPER_DBGLINE(jasperi_object_class_name_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Cannot find get clazz name",1);
      goto cleanup;
    }
  JASPER_DBGLINE(jasperi_object_class_name_C);
  className = (char *)(*jnienv)->GetStringUTFChars(jnienv,(jstring)jname,NULL);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (!SP_put_string(name, className))
    {
      JASPER_DBGLINE(jasperi_object_class_name_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Cannot make an atom from class name",1);
    }
 cleanup:
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (className) (*jnienv)->ReleaseStringUTFChars(jnienv,(jstring)jname,className);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (jname) DELETE_LOCAL_REF(jnienv, jname);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (clazzclazz) DELETE_LOCAL_REF(jnienv, clazzclazz);
  JASPER_DBGLINE(jasperi_object_class_name_C);
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);
  JASPER_DBGLINE(jasperi_object_class_name_C);
}

struct meta_args_state {
  int is_void;                  /* TRUE if method is void */
  int returns_term;             /* see jasperi_process_meta_args */
  int numargs;                  /* >= 0 if valid */
  jvalue jargs[256];            /* (max arity 255 + return arg at jargs[0]) */
  int is_object[256];
};

/* 
   Should be called three times after each other:
   mode = 1 (a.k.a. calling) to set-up jargs vector
   mode = 2 (a.k.a. returning) to pick out results and unify out args
   mode = 3 (a.k.a cleanup) to perform clean-up (free temporary objects).

   . If a call with mode 1 returns with a value different from
     SP_SUCCESS then no call with mode 2 should be performed.

   . If a call with mode 2 returns SP_FAIL then the call failed in
     back unification (not an error). If mode 2 returns SP_ERROR than
     an exception has already been raised.

   . A call with mode = 3 should always be done as a last step even if
     one of the previous modes did not return SP_SUCCESS.
*/
#if THREADSERVER
static int jasperi_process_meta_args(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                     SP_term_ref methodDesc,
                                     SP_term_ref args,
                                     jvalue **pjargs,
                                     int *returns_term,
                                     struct meta_args_state *state,
                                     int instance,
                                     int mode,
                                     jboolean new_style_interface,
                                     jboolean threadservermode)
#else
static int jasperi_process_meta_args(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                     SP_term_ref methodDesc,
                                     SP_term_ref args,
                                     jvalue **pjargs,
                                     int *returns_term,
                                     struct meta_args_state *state,
                                     int instance,
                                     int mode)
#endif
{
  const int calling = (mode==1);
  const int returning = (mode==2);
  const int cleanup = (mode==3);
  int numargs;
  int i, argno;
  int first_argno = ( instance ? 2 : 1 ); /* instance method: first arg is the instance so skip it */
  const int idx_limit = ((sizeof state->is_object)/(sizeof state->is_object[0])); /* a.k.a 256 */
  int retval_seen = 0;        /* both flag and index adjustment */
  jvalue *jargs = &(state->jargs[0]);
  int *is_object = &(state->is_object[0]); /* TRUE if this object
                                              should be deleted on cleanup
                                           */

  SP_term_ref argtype = SP_new_term_ref();
  SP_term_ref arg = SP_new_term_ref();
  /* tmp_tr is for temporary use when processing an argument. Also set
     to out and return value for arg to be unified with */
  SP_term_ref tmp_tr = SP_new_term_ref();

#if JASPER_DBG
  fprintf(stderr, "jasperi_process_meta_args: %s method mode=%s\n", (instance ? "instance" : "static"), (calling ? "calling" : (returning ? "returning" : (cleanup ? "cleanup" : "ERROR"))));
#endif

  JASPER_DBGLINE(jasperi_process_meta_args);

  /* initialize state */
  if (calling)
    {
      int j;
      for (j = 0; j < idx_limit; j++) /* a.k.a. j < 256 */
        {
          is_object[j] = FALSE;
        }
      state->is_void = TRUE;    /* assume VOID until retval seen */
      state->numargs = -1;      /* mark as invalid */
      /*

        Note: Now (late 3.8.5) context popped after handling return
        values so special [-term] handling should not be needed. Still
        works though:

        Special case for [-term]. The SPTerm will be reset by
        pop_context so the term ref must be extracted immediately
        after the call. *returns_term==TRUE tells the caller to do
        this. The caller should then DeleteRef the SPTerm object and
        put the term ref in jargs[0].j and set *returns_term to FALSE
        again (so the cleanup code does not tryto DeleteRef the term
        ref).  (The same problem applies to generated glue code) */
      state->returns_term=FALSE; /* set to TRUE when [-term] seen, never reset */
      *returns_term = FALSE;    /* set to TRUE when [-term] seen, reset to false
                                   in caller when jargs[0].j set to the term ref */
      *pjargs = jargs;
    }
  numargs = state->numargs;

  if (returning || cleanup)
    {
      if (state->returns_term   /* jargs[0] used to be an SPTerm object */
          && !*returns_term)    /* jargs[0] changed by caller to jlong */
        {
          is_object[0] = FALSE;
        }
    }

  /* From now on it is safe to goto label barf et al. */
  
  if (cleanup) { JASPER_DBGLINE(jasperi_process_meta_args); goto cleanup;} /* assume caller reported error */
  JASPER_DBGLINE(jasperi_process_meta_args);

  /* onle get here when calling or returning */


  if (calling)              /* not yet valid */
    {
      SP_atom pred_name;
  
      if (!SP_get_functor(methodDesc, &pred_name, &numargs))
        {
          numargs = -1;         /* mark as invalid */
          JASPER_DBGLINE(jasperi_process_meta_args);
          goto barf;
        }
      else
        {
#if JASPER_DBG
          fprintf(stderr, "jasperi_process_meta_args: numargs=%d, pred_name=%s\n", (int)numargs, SP_string_from_atom(pred_name));
#endif
          state->numargs = numargs; /* used in subsequent calls */
        }
    }

#if JASPER_DBG
          fprintf(stderr, "jasperi_process_meta_args: numargs=%d\n", (int)numargs);
#endif

  JASPER_DBGLINE(jasperi_process_meta_args);

  if (numargs < 0) {
#if JASPER_DBG
    fprintf(stderr, "INTERNAL ERROR: jasperi_process_meta_args numargs < 0\n"); /* illegal call sequence! */
#endif
    JASPER_DBGLINE(jasperi_process_meta_args); goto barf;
  }

  JASPER_DBGLINE(jasperi_process_meta_args);

  for (argno = first_argno, i = 1; argno <= numargs; argno++)
    {
      int retval = FALSE;
      int inarg;

#if JASPER_DBG
      fprintf(stderr, "jasperi_process_meta_args: processing arg #%d (jargs[%d])\n", argno, i);
#endif

      if (!SP_get_arg(argno, methodDesc, argtype)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
      JASPER_DBGLINE(jasperi_process_meta_args);        
      if (SP_is_list(argtype))    /* argtype = [-Type] */
        {
          if (!SP_get_arg(1, argtype, argtype)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;} /* argtype = -Type */
          if (retval_seen) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
          retval_seen = 1;
          retval = TRUE;
          state->is_void = FALSE;
        }
      JASPER_DBGLINE(jasperi_process_meta_args);
      /* argtype should be +Type or -Type here  */
      {
        int type_arity;
        SP_atom type;
        {
          SP_atom plus_or_minus;
          int one;

          if (!SP_get_functor(argtype, &plus_or_minus, &one) || one!=1) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
          JASPER_DBGLINE(jasperi_process_meta_args);
          inarg = (plus_or_minus == STATE_VARIABLE(jasper_atom_plus_sign));

#if JASPER_DBG
          fprintf(stderr, "jasperi_process_meta_args: plus_or_minus=`%s'==%s\n", SP_string_from_atom(plus_or_minus), (inarg ? "input arg" : "output arg"));
#endif
        }
          
        /* argtype should be +Type or -Type here  */
        JASPER_DBGLINE(jasperi_process_meta_args);
        if (!SP_get_arg(1, argtype, argtype)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
        JASPER_DBGLINE(jasperi_process_meta_args);
        /* argtype is Type */
        if (!SP_get_functor(argtype, &type, &type_arity)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
        JASPER_DBGLINE(jasperi_process_meta_args);
        if (!SP_get_arg(argno, args, arg)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
        JASPER_DBGLINE(jasperi_process_meta_args);

#if JASPER_DBG
        fprintf(stderr, "jasperi_process_meta_args: processing %s%s%s%s\n", (retval ? "[" : ""),  (inarg ? "+" : "-"), SP_string_from_atom(type), (retval ? "]" : ""));
#endif

        /* Extremely tedious but straightforward */
        if (type == STATE_VARIABLE(jasper_atom_byte))
          {
            if (inarg)        /* +byte */
              {
                if (calling)
                  {
                    SP_integer x;
                    if (!SP_get_integer(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].b = (jbyte) x;
                  }
              }
            else
              if (retval)     /* [-byte] */
                {
                  if (returning)
                    {
                      if (!SP_put_integer(tmp_tr, jargs[0].b)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -byte */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_char))
          {
            if (inarg)        /* +char */
              {
                if (calling)
                  {
                    SP_integer x;
                    if (!SP_get_integer(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].c = (jchar) x;
                  }
              }
            else
              if (retval)     /* [-char] */
                {
                  if (returning)
                    {
                      if (!SP_put_integer(tmp_tr, jargs[0].c)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -char */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_short))
          {
            if (inarg)        /* +short */
              {
                if (calling)
                  {
                    SP_integer x;
                    if (!SP_get_integer(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].s = (jshort) x;
                  }
              }
            else
              if (retval)     /* [-short] */
                {
                  if (returning)
                    {
                      if (!SP_put_integer(tmp_tr, jargs[0].s)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -short */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_integer))
          {
            if (inarg)        /* +integer */
              {
                if (calling)
                  {
                    SP_integer x;
                    if (!SP_get_integer(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].i = (jint) x;
                  }
              }
            else
              if (retval)     /* [-integer] */
                {
                  if (returning)
                    {
                      if (!SP_put_integer(tmp_tr, jargs[0].i)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -integer */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_long))
          {
            if (inarg)        /* +long */
              {
                if (calling)
                  {
                    jlong x;
                    if (!get_jlong(SPAPI_ARG arg, &x, TRUE /* allow float */)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].j = x;
                  }
              }
            else
              if (retval)     /* [-long] */
                {
                  if (returning)
                    {
                      if (!put_jlong(SPAPI_ARG tmp_tr, jargs[0].j)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -long */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_float))
          {
            if (inarg)        /* +float */
              {
                if (calling)
                  {
                    double x;
                    if (!SP_get_float(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].f = (jfloat) x;
                  }
              }
            else
              if (retval)     /* [-float] */
                {
                  if (returning)
                    {
                      if (!SP_put_float(tmp_tr, (double) jargs[0].f)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -float */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_double))
          {
            if (inarg)        /* +double */
              {
                if (calling)
                  {
                    double x;
                    if (!SP_get_float(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].d = (jdouble) x;
                  }
              }
            else
              if (retval)     /* [-double] */
                {
                  if (returning)
                    {
                      if (!SP_put_float(tmp_tr, (double)jargs[0].d)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -double */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_boolean))
          {
            if (inarg)        /* +boolean */
              {
                if (calling)
                  {
                    SP_atom x;
                    if (!SP_get_atom(arg, &x)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].z = ((x==STATE_VARIABLE(jasper_atom_true)) ? JNI_TRUE : JNI_FALSE);
                  }
              }
            else
              if (retval)     /* [-boolean] */
                {
                  if (returning)
                    {
                      if (!SP_put_atom(tmp_tr, ((jargs[0].z != JNI_FALSE) ? STATE_VARIABLE(jasper_atom_true) : STATE_VARIABLE(jasper_atom_false)))) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -boolean */
                {
                  goto nyi;
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_object))
          {
            if (inarg)        /* +object */
              {
                JASPER_DBGLINE(jasperi_process_meta_args);
                if (calling)
                  {
                    jobject x;
                    JASPER_DBGLINE(jasperi_process_meta_args);
                    if (!SPJavaGlueGetObjectM(SPAPI_ARG jnienv, arg, &x, -1, NULL))
                        {
                          JASPER_DBGLINE(jasperi_process_meta_args);
                          goto reported_barf;
                        }
                    #if JASPER_DBG
                    fprintf(stderr, "\nx==%p\n", x);
                    #endif

                    JASPER_DBGLINE(jasperi_process_meta_args);
                    /* make it safe to DeleteRef in cleanup and also
                       safe to call jasper_delete_ref on the term
                       representation (Note that NULL is an OK arg to DeleteLocalRef */
                    jargs[i].l = (x ? (*jnienv)->NewLocalRef(jnienv, x) : NULL);
                    #if JASPER_DBG+0>0
                    fprintf(stderr, "\njargs[i].l==%p\n", jargs[i].l);
                    #endif
                    JASPER_DBGLINE(jasperi_process_meta_args);
                    is_object[i] = TRUE;
                  }
              }
            else
              if (retval)     /* [-object] */
                {
                  /* is_object[0] is set to FALSE later when it is certain that we will not fail or err */
                  if (returning)
                    {
                      if (! jobject_to_termref(SPAPI_ARG jnienv, jargs[0].l, tmp_tr) )
                        {
                          #if NULL_OBJREF
                          ;     /* do nothing, null is OK */
                          #else /* !NULL_OBJREF */
                          /* conversion failed (or NULL) NOTE: better exception needed */
                          jasper_int_handle_exception(SPAPI_ARG jnienv,"null object return value in jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__),1);
                          
                          goto reported_barf;
                          #endif
                        }
                    }
                }
              else            /* -object */
                {
                  { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_term))
          {
            if (inarg)        /* +term (se/sics/jasper/SPTerm)*/
              {
                if (calling)
                  {
                    jobject x;
                    SP_term_ref arg_copy = SP_new_term_ref();
                    
                    SP_put_term(arg_copy, arg);
#if THREADSERVER
                    x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, arg_copy, -1, NULL, TRUE /* init */, new_style_interface);
#else
                    x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, arg_copy, -1, NULL, TRUE /* init */);
#endif
                    if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
#if JASPER_DBG
                    fprintf(stderr, "jasperi_process_meta_args: calling +term jargs[%d].l = %p\n", (int)i, (void*)x);
#endif

                    jargs[i].l = x;
                    is_object[i] = TRUE;
                  }
                else /* returning */
                  {
#if JASPER_DBG
                    fprintf(stderr, "jasperi_process_meta_args: returning +term jargs[%d].l == %p\n", (int)i, (void*)jargs[i].l);
#endif
                  }
              }
            else
              if (retval)     /* [-term] */
                {
                  /* Kludge, see doc for returns_term */

                  if (calling)
                    {
                      *returns_term = TRUE;
                      state->returns_term = TRUE;
                    }
                  else /* if (returning) */
                    {
                      if ((*returns_term) /* INTERNAL ERROR: should have been reset in caller */
                          || is_object[0] /* INTERNAL ERROR: should have been reset when Mode==returning */
                          )
                        {
#if JASPER_DBG
                          fprintf(stderr, "INTERNAL ERROR: jasperi_process_meta_args ((*returns_term) || is_object[0])\n");
#endif
                          JASPER_DBGLINE(jasperi_process_meta_args); goto barf;
                        }
                      if (!SP_put_term(tmp_tr, (SP_term_ref)jargs[0].j)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    }
                }
              else            /* -term */
                {
                  if (calling)
                    {
                      jobject x;
#if THREADSERVER
                      x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, 0, -1, NULL, FALSE /* !init */, new_style_interface);
#else
                      x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, 0, -1, NULL, FALSE /* !init */);
#endif
                      if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      jargs[i].l = x;
                      is_object[i] = TRUE;
                    }
                  else /* returning */
                    {
                      int rc;
                      rc = SPJavaGlueGetNativeTermRefM(SPAPI_ARG jnienv, jargs[i].l, &tmp_tr, -1, NULL);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    }
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_atom))
          {
            if (inarg)        /* +atom (se/sics/jasper/SPCanonicalAtom) */
              {
                if (calling)
                  {
                    SP_atom the_atom;
                    jobject x;

                    if (!SP_get_atom(arg, &the_atom)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
#if THREADSERVER
                    x = SPJavaGlueSPCanonicalAtomM(SPAPI_ARG jnienv, the_atom, -1, new_style_interface);
#else
                    x = SPJavaGlueSPCanonicalAtomM(SPAPI_ARG jnienv, the_atom, -1);
#endif
                    if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    jargs[i].l = x;
                    is_object[i] = TRUE;
                  }
                else /* returning */
                  {
                    /*
                      (*jnienv)->DeleteLocalRef(jnienv, jargs[i].l);
                      jargs[i].l = NULL;
                    */
                  }
              }
            else
              if (retval)     /* [-atom] */
                {
                  if (returning)
                    {
                      int rc;
                      SP_atom the_atom = 0; /* initialize to shut up compiler */

#if THREADSERVER
                      rc = SPJavaGlueGetAtomM(SPAPI_ARG jnienv, jargs[0].l, &the_atom, -1, NULL, threadservermode);
#else
                      rc = SPJavaGlueGetAtomM(SPAPI_ARG jnienv, jargs[0].l, &the_atom, -1, NULL);
#endif
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      if (!SP_put_atom(tmp_tr, the_atom)) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    }
                }
              else            /* -atom */
                {
                  if (calling)
                    {
                      jobject x;

#if THREADSERVER
                      x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, 0, -1, NULL, FALSE /* !init */, new_style_interface);
#else
                      x = SPJavaGlueSPTermM(SPAPI_ARG jnienv, 0, -1, NULL, FALSE /* !init */);
#endif
                      if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      jargs[i].l = x;
                      is_object[i] = TRUE;
                    }
                  else /* returning */
                    {
                      int rc;
                      rc = SPJavaGlueGetNativeTermRefM(SPAPI_ARG jnienv, jargs[i].l, &tmp_tr, -1, NULL);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      if (!SP_is_atom(tmp_tr))
                        {
                          goto failure;
                        }
                    }
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_codes)
                 ||
                 type == STATE_VARIABLE(jasper_atom_chars) /* [PM] 4.0.2+ legacy */
                 )
          {
            if (inarg)        /* +codes (java/lang/String) */
              {
                if (calling)
                  {
                    char const *codes;
                    jstring x;
                  
                    if (!SP_get_list_codes(arg, &codes)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                  
                    x = (*jnienv)->NewStringUTF(jnienv, codes);
                    if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].l = x;
                    is_object[i] = TRUE;
                  }
                else /* returning */
                  {
                    /*
                      (*jnienv)->DeleteLocalRef(jnienv, jargs[i].l);
                      jargs[i].l = NULL;
                    */
                  }
              }
            else
              if (retval)     /* [-codes] */
                {
                  if (returning) /* (java/lang/String) */
                    {
                      int rc;
                                            
                      rc = SPJavaGluePostPutCodesM(SPAPI_ARG jnienv, jargs[0].l, tmp_tr, -1);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    }
                }
              else            /* -codes (java/lang/StringBuffer) */
                {
                  if (calling)
                    {
                      jobject x;
                      x = SPJavaGlueStringBufferM(SPAPI_ARG jnienv, -1);
                      if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      jargs[i].l = x;
                      is_object[i] = TRUE;
                    }
                  else /* returning */
                    {
                      jstring string;
                      int rc;
                      
                      string = SPJavaGluePostToStringM(SPAPI_ARG jnienv, jargs[i].l, -1);
                      if (!string) goto reported_barf;
                      rc = SPJavaGluePostPutCodesM(SPAPI_ARG jnienv, string, tmp_tr, -1);
                      (*jnienv)->DeleteLocalRef(jnienv, string);

                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    }
                }
          }
        else if (type == STATE_VARIABLE(jasper_atom_string))
          {
            if (inarg)        /* +string (java/lang/String)*/
              {
                if (calling)
                  {
                    SP_atom the_atom;
                    const char *codes;
                    jstring x;
                    JASPER_DBGLINE(jasperi_process_meta_args);                  
                  
                    if (!SP_get_atom(arg, &the_atom)) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    codes = SP_string_from_atom(the_atom);
                    if (!codes) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                  
                    x = (*jnienv)->NewStringUTF(jnienv, codes);
                    if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
                    jargs[i].l = x;
                    is_object[i] = TRUE;
                  }
                else /* returning */
                  {
                    JASPER_DBGLINE(jasperi_process_meta_args);
                  }
              }
            else
              if (retval)     /* [-string] */
                {
                  if (returning) /* (java/lang/String) */
                    {
                      jstring x;
                      int rc;
                      JASPER_DBGLINE(jasperi_process_meta_args);
                      x = SPJavaGluePostToStringM(SPAPI_ARG jnienv, jargs[0].l, -1);
                      if (x==NULL) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      JASPER_DBGLINE(jasperi_process_meta_args);
                      rc = SPJavaGluePostPutStrM(SPAPI_ARG jnienv, x, tmp_tr, -1);
                      (*jnienv)->DeleteLocalRef(jnienv, x);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      #if JASPER_DBG
                      {
                        char const *string = NULL;
                        fprintf(stderr, "[-string] = %s\n", ( SP_get_string(tmp_tr, &string) ? string : "<<ERROR: SP_get_string failed>>"));
                      }
                      #endif
                    }
                }
              else            /* -string (java/lang/StringBuffer) */
                {
                  if (calling)
                    {
                      jobject x;
                      JASPER_DBGLINE(jasperi_process_meta_args);
                      x = SPJavaGlueStringBufferM(SPAPI_ARG jnienv, -1);
                      if (!x) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      jargs[i].l = x;
                      is_object[i] = TRUE;
                    }
                  else /* returning */
                    {
                      jstring x;
                      int rc;
                      JASPER_DBGLINE(jasperi_process_meta_args);
                      x = SPJavaGluePostToStringM(SPAPI_ARG jnienv, jargs[i].l, 42);
                      if (x==NULL) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                      rc = SPJavaGluePostPutStrM(SPAPI_ARG jnienv, x, tmp_tr, -1);
                      (*jnienv)->DeleteLocalRef(jnienv, x);
                      if (!rc) { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}
                    }
                }
          }
        else
          {
#if JASPER_DBG
            fprintf(stderr, "ERROR illegal arg type `%s'\n", SP_string_from_atom(type));
#endif

            { JASPER_DBGLINE(jasperi_process_meta_args); goto barf;}
          }
      }
      if (!retval) i++;

      /* if return value or outarg (-Type or [-Type]) then tmp_tr should be the out term */
      if (returning && !inarg)               /* a.k.a returning && (retval || !inarg) */
        {
#if JASPER_DBG
          fprintf(stderr, "jasperi_process_meta_args: unifying return value (arg is %s (%d), tmp_arg is %s (%d)\n", (SP_is_variable(arg) ? "variable" : "NOT variable!"), SP_term_type(arg), (SP_is_atom(tmp_tr) ? "atom" : "not atom"), SP_term_type(tmp_tr));
#endif
          if (!SP_unify(arg, tmp_tr))
            {
              JASPER_DBGLINE(jasperi_process_meta_args);
              goto failure;
            }
        }
    } /* for */
  
  /* Got here without error or fail */

  if (returning && !state->is_void)
    {
      /* make sure the subsequent cleanup will not delete the return
         ref (now in a $jave_object wrapper
      */
      is_object[0] = FALSE;
    }

  return SP_SUCCESS;


 nyi:
  SPJavaGlueErrorM(SPAPI_ARG jnienv, -1, "Error in Java meta call: Unimplemented feature");
  { JASPER_DBGLINE(jasperi_process_meta_args); goto reported_barf;}

 barf:
  if (!SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, -1, "Error in Java meta call (Java)"))
    {
      SPJavaGlueErrorM(SPAPI_ARG jnienv, -1, "Error in Java meta call");
    }

  /* FALLTHROUGH */
 reported_barf:
  return SP_ERROR;

 failure:
  return SP_FAILURE;

 cleanup:
  /* 
     cleanup:
     need to go through is_object[0..numargs)] and DeleteLocalRef. ([PM] 3.9.2 SPRM 3493 was numargs-1)
     previous calls (with mode==returning) will have set is_object[0]
     to FALSE if we will not fail or err (so the returned object is
     not freed). (if numargs < 0 then we should use idx_limit).
     Note that index zero is for the return argument so the argument are indexed from one.
  */
  {
    int j;
    int limit;
    
    limit = ( numargs >= 0 ? numargs+1 : idx_limit ); /* [PM] SPMR 3493 was numargs which missed the last argument */
      
    for (j = 0; j < limit; j++)
      {
        if (is_object[j])
          {
#if JASPER_DBG
            fprintf(stderr, "jasperi_process_meta_args: DeleteLocalRef(%" SPRIdINTEGER ",%" SPRIdINTEGER ")\n", (SP_integer)jnienv, (SP_integer)jargs[j].l);
#endif
            (*jnienv)->DeleteLocalRef(jnienv, jargs[j].l);
          }
      }
  }
  return SP_SUCCESS;
}

#if !THREADSERVER             /* [PD] 3.9 Old pre-threadserver code */

/* [PM] 3.8.5 NEW API */
/* FOREIGN */
void SPCDECL
jasperi_call_static1_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref,
                       char const *className,
                       char const *methodName,
                       char const *typesig,
                       SP_term_ref methodDesc,
                       SP_term_ref args)

{
  char *return_type_sig;        /* e.g. "Ljava/lang/String;" */
  jclass clazz = NULL;
  jmethodID mid;
  jvalue *jargs = NULL;
  JNIEnv *jnienv;
  int returns_term;             /* see jasperi_call_static1_C */
  struct meta_args_state state;
  int result = SP_ERROR;
  int init = FALSE;             /* true if <init>, i.e., NewObject */
  
  SP_uinteger context = 0;

  DbgBreakPoint();
#if JASPER_DBG
  fprintf(stderr, "*** jasperi_call_static1_C Calling %s in %s, signature %s.\n", methodName, className, typesig);
#endif

  init = (strcmp(methodName, "<init>")==0); /* call NewObject */

  JASPER_DBGLINE(jasperi_call_static1_C);
  SET_JNIENV(jnienv,jvmref, {goto cleanup;});

#if JASPER_DBG
  fprintf(stderr, "*** jasperi_call_static1_C jnienv: %p\n", (void*)jnienv);
#endif /* JASPER_DBG */

  JASPER_DBGLINE(jasperi_call_static1_C);
  ASSERT_NO_EXCP(jnienv, "jasperi_call_static1_C");
  JASPER_DBGLINE(jasperi_call_static1_C);
  if ( (clazz = jasperi_lookup_class(SPAPI_ARG jnienv,className)) == NULL ) goto cleanup;
  JASPER_DBGLINE(jasperi_call_static1_C);
  if ( (mid = jasperi_lookup_method(SPAPI_ARG jnienv,clazz,methodName,typesig, ( !init ? 1/*static*/ : 0/*instance/init*/))) == 0 ) goto cleanup;
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* If we do not get here then an error was thrown before going to cleanup: */

  /* Skip over argument types to find out the return type.
     No need to guard against missing ')' since jasperi_lookup_method succeeded. */
  {
    char *p = typesig;
    while (*p != ')') p++;
    p++;    /* skip ')' */
    return_type_sig = p;
  }
  /* must be done before creating SPTerms for parameter passing */
  context = jasper_push_context(SPAPI_ARG jnienv);
  /* Prolog run-time monitor is (still) owned by this thread */

  /* get arg vector */
  {
    int rc;
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 1 /* CALLING set up jargs for call */);
    switch (rc)
      {
      case SP_SUCCESS:
        JASPER_DBGLINE(jasperi_call_static1_C);
        break;
      case SP_ERROR:
        JASPER_DBGLINE(jasperi_call_static1_C);
        goto cleanup;
        break;
      default:                    /* should not happen */
        JASPER_DBGLINE(jasperi_call_static1_C);
#if JASPER_DBG
        fprintf(stderr, "FATAL ERROR: jasperi_call_static1_C jasperi_process_meta_args returned %d\n", rc);
#endif
        break;
      }
  }
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* Prolog run-time monitor is (still) owned by this thread */

  if (!jasper_leave_context_monitor(jnienv,context)) {
#if JASPER_DBG
      fprintf(stderr, "FATAL ERROR: jasperi_call_static1_C jasper_leave_context_monitor returned 0\n");
#endif
      goto cleanup;             /* NOTE: Fix error handling */
  }
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* Prolog run-time monitor is *not* owned by this thread, i.e., Java
     context, no SP runtime calls allowed */


  /*** NOTE: if we get here we must enter monitor so cannot goto
       cleanup directly
  ****/
  JASPER_DBGLINE(jasperi_call_static1_C);

#if JASPER_DBG
  fprintf(stderr, "*** jasperi_call_static1_C Lookup complete. Performing call. %s::%s %s jargs=%p\n", className, methodName, typesig, jargs);
#endif

  {
    int result_is_object = FALSE;

    JASPER_DBGLINE(jasperi_call_static1_C);
    {
      int illegal_method = FALSE;
      int returned_term_error = FALSE;

      if (init)                 /* new <<CLASS>> */
        {
          jargs[0].l = (*jnienv)->NewObjectA(jnienv, clazz, mid, jargs+1);
          result_is_object = TRUE;
        }
      else                      /* normal case, static method */
        {
          switch (return_type_sig[0])
            {
            case 'V':
              #if JASPER_DBG>1
              {
                int i;
                for (i = 1; i <= state.numargs; i++)
                  {
                    fprintf(stderr, "CallStaticVoidMethodA %s::%s jargs[%d]==%p (%s)\n", className, methodName, i, ( state.is_object[i] ? (jargs)[i].l : 0 ), ( state.is_object[i] ? "object": "primitive"));
                  }
              }
              #endif
              (*jnienv)->CallStaticVoidMethodA(jnienv, clazz, mid, jargs+1 /* ! +1 also for VOID */);
              break;
            case '[':                   /* Arrays are objects too */
            case 'L':
              jargs[0].l = (*jnienv)->CallStaticObjectMethodA(jnienv, clazz, mid, jargs+1);
              result_is_object = TRUE;
              if (returns_term && !(*jnienv)->ExceptionCheck(jnienv))
                {                   /* see jasperi_call_static1_C */
                  SP_term_ref returned_term;
                  if (!SPJavaGlueGetNativeTermRefInJavaContextM(/*SPAPI_ARG*/ jnienv, jargs[0].l, &returned_term))
                    {
                      JASPER_DBGLINE(jasperi_call_static1_C);
                      returned_term_error = TRUE;
                    }
                  else              /* no error */
                    {
                      JASPER_DBGLINE(jasperi_call_static1_C);
                      (*jnienv)->DeleteLocalRef(jnienv,jargs[0].l);
                      returns_term = FALSE; /* signals cleanup code that jargs[0] no longer is an object */
                      jargs[0].j = (jlong) returned_term;
                    }
                }
              break;
            case 'Z':
              jargs[0].z = (*jnienv)->CallStaticBooleanMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'B':
              jargs[0].b = (*jnienv)->CallStaticByteMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'C':
              jargs[0].c = (*jnienv)->CallStaticCharMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'S':
              jargs[0].s = (*jnienv)->CallStaticShortMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'I':
              jargs[0].i = (*jnienv)->CallStaticIntMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'J':
              jargs[0].j = (*jnienv)->CallStaticLongMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'F':
              jargs[0].f = (*jnienv)->CallStaticFloatMethodA(jnienv, clazz, mid, jargs+1);
              break;
            case 'D':
              jargs[0].d = (*jnienv)->CallStaticDoubleMethodA(jnienv, clazz, mid, jargs+1);
              break;
            default:
              /* We cannot call jasper_int_handle_exception in Java context */
              illegal_method = TRUE;
            }
        }

      /* Java context */

      /* NOTE: error handling */
      jasper_enter_context_monitor(jnienv,context);


      /* Prolog context, monitor owned by this thread */

      if (illegal_method)
        {
          jasper_int_handle_exception(SPAPI_ARG jnienv,"Illegal method descriptor" " @" __FILE__ ":" STRINGISIZE(__LINE__),1);
          goto cleanup;
        }


      JASPER_DBGLINE(jasperi_call_static1_C);
      /* If we have a pending Java exception here then it comes from
         the actual method call and should be propagated as a global ref to the Exception object */
      if ( SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, -1,  ("jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__)))
           ||
           /* If we failed to get the term ref without causing a Java error then give generic error */
           ( returned_term_error
             && jasper_int_handle_exception(SPAPI_ARG jnienv,"jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__), 1) ))
        {
          if (result_is_object
              && (!returned_term_error) /* Do not NULL jargs[0].l as it was returned without error and should be freed.*/
              )
            {
              jargs[0].l = NULL;    /* ensure cleanup does not DeleteLocalRef garbage */
            }
          goto cleanup;
        }
    }
  }
  /* from here on jargs[0] is valid. */
  JASPER_DBGLINE(jasperi_call_static1_C);

  /* (Still) Prolog context, monitor owned by this thread */

  /* get arg vector */
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 2 /* RETURNING handle return values */);
  JASPER_DBGLINE(jasperi_call_static1_C);

 cleanup:
  if (context != 0) {
    jasper_pop_context(SPAPI_ARG jnienv,context);
  }

  JASPER_DBGLINE(jasperi_call_static1_C);
  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 3 /* CLEANUP */);
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);
  JASPER_DBGLINE(jasperi_call_static1_C);

  if (result == SP_FAILURE)
    {
      SP_fail();
    }
}


/* [PM] 3.8.5 NEW API */
/* FOREIGN */
void SPCDECL
jasperi_call_instance1_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref,
                         char const *methodName,
                         char const *typesig,
                         SP_term_ref methodDesc,
                         SP_term_ref args)
{
  char *return_type_sig;        /* e.g. "Ljava/lang/String;" */
  jclass clazz = NULL;
  jobject jobj = NULL;
  jmethodID mid;
  jvalue *jargs = NULL;
  JNIEnv *jnienv;
  int returns_term;             /* see jasperi_process_meta_args */
  struct meta_args_state state;
  int result = SP_ERROR;

  SP_uinteger context = 0;

#if JASPER_DBG
  fprintf(stderr, "*** jasperi_call_instance1_C Calling %s, signature %s.\n", methodName, typesig);
#endif

  SET_JNIENV(jnienv,jvmref, {goto cleanup;});

  ASSERT_NO_EXCP(jnienv, "jasperi_call_instance1_C");

  {
    SP_term_ref objref = SP_new_term_ref();
   
    if (!SP_get_arg(1, args, objref))
      {
        JASPER_DBGLINE(jasperi_call_instance1_C);
        jasper_int_handle_exception(SPAPI_ARG jnienv,"No object",1);
        goto cleanup;
      }
    if (!termref_to_jobject(SPAPI_ARG objref, &jobj) || jobj==NULL)
      {
        JASPER_DBGLINE(jasperi_call_instance1_C);
        jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid on null object",1);
        goto cleanup;
      }

  }
  DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jasperi_call_instance1_C");

  if ( (clazz = jasperi_lookup_class_from_obj(SPAPI_ARG jnienv,jobj)) == NULL ) goto cleanup;
  if ( (mid = jasperi_lookup_method(SPAPI_ARG jnienv,clazz,methodName,typesig,0/*!static*/)) == 0 ) goto cleanup;



  /* Skip over argument types to find out the return type.
     No need to guard against missing ')' since jasperi_lookup_method succeeded. */
  {
    char *p = typesig;
    while (*p != ')') p++;
    p++;    /* skip ')' */
    return_type_sig = p;
  }

  /* must be done before creating SPTerms for parameter passing */
  context = jasper_push_context(SPAPI_ARG jnienv);
  /* Prolog run-time monitor is (still) owned by this thread */

  /* get arg vector */
  {
    int rc;
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 1 /* CALLING set up jargs for call */);
    switch (rc)
      {
      case SP_SUCCESS:
        JASPER_DBGLINE(jasperi_call_instance1_C);
        break;
      case SP_ERROR:
        JASPER_DBGLINE(jasperi_call_instance1_C);
        goto cleanup;
        break;
      default:                    /* should not happen */
        JASPER_DBGLINE(jasperi_call_instance1_C);
#if JASPER_DBG
        fprintf(stderr, "FATAL ERROR: jasperi_call_instance1_C jasperi_process_meta_args returned %d\n", rc);
#endif
        break;
      }
  }

  /* Prolog run-time monitor is (still) owned by this thread */

  if (!jasper_leave_context_monitor(jnienv,context)) {
#if JASPER_DBG
      fprintf(stderr, "FATAL ERROR: jasperi_call_instance1_C jasper_leave_context_monitor returned 0\n");
#endif
      goto cleanup;             /* NOTE: Fix error handling */
  }

  /* Prolog run-time monitor is *not* owned by this thread, i.e., Java context, no SP runtime calls allowed */
  /*** NOTE: if we get here we must enter monitor so cannot goto
       cleanup directly
  ****/
  JASPER_DBGLINE(jasperi_call_instance1_C);

  {
    int result_is_object = FALSE;
    {
      int illegal_method = FALSE;
      int returned_term_error = FALSE;

      switch (return_type_sig[0])
        {
        case 'V':
          (*jnienv)->CallVoidMethodA(jnienv, jobj, mid, jargs+1); /* ! +1 also for VOID */
          break;
        case '[':                   /* Arrays are objects too */
        case 'L':
          jargs[0].l = (*jnienv)->CallObjectMethodA(jnienv, jobj, mid, jargs+1);
          result_is_object = TRUE;
          if (returns_term && !(*jnienv)->ExceptionCheck(jnienv))
            {                   /* see jasperi_process_meta_args */
              SP_term_ref returned_term;
              if (!SPJavaGlueGetNativeTermRefInJavaContextM(/*SPAPI_ARG*/ jnienv, jargs[0].l, &returned_term))
                {
                  JASPER_DBGLINE(jasperi_call_instance1_C);
                  returned_term_error = TRUE;
                }
              else              /* no error */
                {
                  JASPER_DBGLINE(jasperi_call_instance1_C);
                  (*jnienv)->DeleteLocalRef(jnienv,jargs[0].l);
                  returns_term = FALSE; /* signals cleanup code that jargs[0] no longer is an object */
                  jargs[0].j = (jlong) returned_term;
                }
            }
          break;
        case 'Z':
          jargs[0].z = (*jnienv)->CallBooleanMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'B':
          jargs[0].b = (*jnienv)->CallByteMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'C':
          jargs[0].c = (*jnienv)->CallCharMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'S':
          jargs[0].s = (*jnienv)->CallShortMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'I':
          jargs[0].i = (*jnienv)->CallIntMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'J':
          jargs[0].j = (*jnienv)->CallLongMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'F':
          jargs[0].f = (*jnienv)->CallFloatMethodA(jnienv, jobj, mid, jargs+1);
          break;
        case 'D':
          jargs[0].d = (*jnienv)->CallDoubleMethodA(jnienv, jobj, mid, jargs+1);
          break;
        default:
          /* We cannot call jasper_int_handle_exception in Java context */
          illegal_method  = TRUE;
        }
      /* Java context */

      /* NOTE: error handling */
      jasper_enter_context_monitor(jnienv,context);


      /* Prolog context, monitor owned by this thread */

      if (illegal_method)
        {
          jasper_int_handle_exception(SPAPI_ARG jnienv,"Illegal method descriptor" " @" __FILE__ ":" STRINGISIZE(__LINE__),1);
          goto cleanup;
        }

      /* If we have a pending Java exception here then it comes from
         the actual method call and should be propagated as a global ref to the Exception object */
      if ( SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, -1,  ("jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__)))
           ||
           /* If we failed to get the term ref without causing a Java error then give generic error */
           ( returned_term_error
             && jasper_int_handle_exception(SPAPI_ARG jnienv,"jasper_call_instance" " @" __FILE__ ":" STRINGISIZE(__LINE__), 1) ))
        {
          if (result_is_object
              && (!returned_term_error) /* Do not NULL jargs[0].l as it was returned without error and should be freed.*/
              )
            {
              jargs[0].l = NULL;    /* ensure cleanup does not DeleteLocalRef garbage */
            }
          goto cleanup;
        }
    }
  }
  /* from here on jargs[0] is valid. */
  
  /* (still) Prolog context, monitor owned by this thread */
  
  /* get arg vector */
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 2 /* RETURNING handle return values */);

 cleanup:
  if (context != 0) {
    jasper_pop_context(SPAPI_ARG jnienv,context);
  }

  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 3 /* CLEANUP */);
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);

  if (result == SP_FAILURE)
    {
      SP_fail();
    }
}

#else  /* #if THREADSERVER */
/* [PD] 3.9 Thread safe callback via server */


/* Make an Object wrapper for a primitive data type */
static jboolean make_wrapper(JNIEnv *jnienv,
                             jvalue jarg,
                             char jarg_type,
                             jobject *wrapper)
{
  jclass clazz = NULL; jmethodID mid = NULL;
  jboolean rc;

  JASPER_DBGLINE(make_wrapper);
#if JASPER_DBG
  fprintf(stderr, "make_wrapper: jarg_type==%c\n", jarg_type);
#endif

  switch (jarg_type)
    {
    case 'Z':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Boolean"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(Z)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.z))) { goto barf; }
        break;
      }
    case 'B':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Byte"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(B)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.b))) { goto barf; }
        break;
      }
    case 'C':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Character"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(C)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.c))) { goto barf; }
        break;
      }
    case 'S':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Short"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(S)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.s))) { goto barf; }
        break;
      }
    case 'I':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Integer"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(I)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.i))) { goto barf; }
        break;
      }
    case 'J':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Long"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(J)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.j))) { goto barf; }
        break;
      }
    case 'F':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Float"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(F)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.f))) { goto barf; }
        break;
      }
    case 'D':
      {
        if (!(clazz = (*jnienv)->FindClass(jnienv, "java/lang/Double"))) { goto barf; }
        if (!(mid = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(D)V"))) { goto barf; }
        if (!(*wrapper = (*jnienv)->NewObject(jnienv, clazz, mid, jarg.d))) { goto barf; }
        break;
      }
    case '[':
    case 'L':
      #if 1
      *wrapper = (*jnienv)->NewLocalRef(jnienv, jarg.l); /* [PM] 3.9.2b3 already an Object. Copy the ref for callers DeleteLocalRef */
      #else/* to 3.9.2b3 */
      {
        *wrapper = jarg.l;        /* already an Object */ /* [PM] FIXME: this is wrong, caller will DeleteLocalRef() it */
      }
      #endif
      break;
    default:
#if JASPER_DBG
      fprintf(stderr, "make_wrapper: illegal type descriptor: %c\n", jarg_type);
#endif
      (*jnienv)->FatalError(jnienv, "illegal type descriptor");
      goto barf;
    }
  rc = JNI_TRUE;
 cleanup:
  if (clazz) { (*jnienv)->DeleteLocalRef(jnienv, clazz); } /* [PM] 3.9.2b3 */

  return rc;

 barf:
  rc = JNI_FALSE;
  goto cleanup;

}

/* Make an Object array of Object wrappers for the arguments in 'jargs' */
static jobjectArray make_arg_array(JNIEnv *jnienv,
                                   jvalue *jargs,
                                   char const *typesig,
                                   int numargs)
{
  int i;
  jclass objclass = NULL;
  jobjectArray objarray = NULL;
  char const *tp;

#if JASPER_DBG
  fprintf(stderr, "make_arg_array: typesig==%s\n", typesig);
#endif

#if JASPER_PUSH_LOCAL_REFS
  #error "[PM] 3.9.2 JASPER_PUSH_LOCAL_REFS should be off"
  if ((*jnienv)->PushLocalFrame(jnienv, numargs + 2) < 0) { return NULL; }
#endif

  if (!(objclass = (*jnienv)->FindClass(jnienv, "java/lang/Object"))) { goto barf; }
  ASSERT_NO_EXCP(jnienv, "make_arg_array (1)");
  if (!(objarray = (*jnienv)->NewObjectArray(jnienv, (jsize)(numargs),objclass, (jobject)NULL))) { goto barf; }
  ASSERT_NO_EXCP(jnienv, "make_arg_array (2)");
  tp = typesig;

  while (*tp != '(') tp++;      /* skip to arguments */
  tp++;                         /* skip to first argument type */

#if JASPER_DBG>0
  fprintf(stderr, "make_arg_array: numargs==%d\n", numargs);
#endif
  ASSERT_NO_EXCP(jnienv, "make_arg_array (4)");

  JASPER_DBGLINE(make_arg_array);
  for (i = 0; i < numargs; i++)
    {
      jobject value = NULL;
#if JASPER_DBG
      fprintf(stderr, "make_arg_array: i==%d, tp==%s\n", i, tp);
#endif
      JASPER_DBGLINE(make_arg_array);
      if (!make_wrapper(jnienv, jargs[i], *tp, &value)) { goto barf; }

      if (*tp == '[') { tp++; }
      if (*tp == 'L') {
        while (*tp != ';') tp++; /* skip past this object type */
      } 
      tp++;                     /* skip to next argument type */

      ASSERT_NO_EXCP(jnienv, "make_arg_array (5)");
      /*
        [PM] 3.9.2b3
        this is what Sun does in hotspot1.3.1/src/os/solaris/launcher/java.c:
	(*env)->SetObjectArrayElement(env, ary, i, str);
	(*env)->DeleteLocalRef(env, str);
        So, as expected, what we do with !JASPER_PUSH_LOCAL_REFS *should* be the right thing
       */
      JASPER_DBGLINE(make_arg_array);
      (*jnienv)->SetObjectArrayElement(jnienv, objarray, i, value);

      #if JASPER_DBG && 0
      {
        jobject valueX = NULL;
        valueX = (*jnienv)->GetObjectArrayElement(jnienv, objarray, i);
        #if 0 && !JASPER_PUSH_LOCAL_REFS /* [PM] 3.9.2b3 while debugging */
        fprintf(stderr, "make_arg_array SetObjectArrayElement(%d, %p), Get()==%p\n", (int)i, (void*)value, (void*)valueX);fflush(stderr);
        #endif
        if (valueX == value)
          {
            fprintf(stderr,
                    "ERROR: GetObjectArrayElement did not return a new local ref:\n"
                    "ERROR: make_arg_array SetObjectArrayElement(%d, %p), Get()==%p\n",
                    (int)i, (void*)value, (void*)valueX);fflush(stderr);
          }
        else
          {
            (*jnienv)->DeleteLocalRef(jnienv, valueX);
          }
      }
      #endif/* JASPER_DBG */
#if !JASPER_PUSH_LOCAL_REFS
                        /* This causes a segfault. Not immediately, but
                           later. (In SPJavaGlueSPTermM?).
                           *** FIXME: INVESTIGATE THIS! *** */
      if (value) { (*jnienv)->DeleteLocalRef(jnienv, value); }
#endif
    }

  JASPER_DBGLINE(make_arg_array);
  ASSERT_NO_EXCP(jnienv, "make_arg_array (6)");
#if JASPER_PUSH_LOCAL_REFS
  {
    jobjectArray result = (*jnienv)->PopLocalFrame(jnienv, objarray);
    return result;
  }
#else
  if (objclass) { (*jnienv)->DeleteLocalRef(jnienv, objclass); }
  return objarray;
#endif

 barf:
#if JASPER_PUSH_LOCAL_REFS
  (*jnienv)->PopLocalFrame(jnienv, NULL);
#else
  if (objclass) { (*jnienv)->DeleteLocalRef(jnienv, objclass); }
  if (objarray) { (*jnienv)->DeleteLocalRef(jnienv, objarray); }
#endif
  return NULL;
}

static jobject jasper_get_server(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv)
{
  jboolean hasException;
  jvalue result;
  jobject sp_obj = sp_get_jasper_magic(SICSTUS_VERSION);

  if (!sp_obj) return NULL;

  result = CallMethodByName(jnienv,
                            &hasException,
                            sp_obj,
                            "getServer",
                            "()Lse/sics/jasper/Server;");

  if (hasException) {
#if JASPER_DBG
    jasper_DescribeException(jnienv);
#endif
    return NULL;
  }
  return result.l;
}

struct call_args {
  jstring signature;
  jstring methname;
  jmethodID imid;
  jobjectArray jarray;
  jobject server;
};

static void jasper_cleanup_after_callback(JNIEnv *jnienv, struct call_args *ca)
{
  jthrowable excp = (*jnienv)->ExceptionOccurred(jnienv);
  (*jnienv)->ExceptionClear(jnienv);

  (*jnienv)->DeleteLocalRef(jnienv, ca->signature);
  (*jnienv)->DeleteLocalRef(jnienv, ca->methname);
  (*jnienv)->DeleteLocalRef(jnienv, ca->jarray);
  (*jnienv)->DeleteLocalRef(jnienv, ca->server);

  if (excp) { (*jnienv)->Throw(jnienv, excp); }
}

static jboolean jasper_setup_for_callback(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                          char const *method,  /* method name */
                                          jvalue *jargs, /* argument array */
                                          char const *typesig, /* type signature */
                                          int numargs, /* number of arguments */
                                          const char *callbackname, /* name of
                                                                    CB method */
                                          const char *callbacksig, /* signature
                                                                     of above */
                                          struct call_args *ca)
{
  jclass cbclass = NULL;

  if (!(ca->server = jasper_get_server(SPAPI_ARG jnienv))) { goto barf; }
  if (!(cbclass = jasperi_lookup_class_from_obj(SPAPI_ARG jnienv, ca->server))) { goto barf; }
  ASSERT_NO_EXCP(jnienv, "jasper_setup_for_callback");

  if (!(ca->signature = (*jnienv)->NewStringUTF(jnienv, typesig))) { goto barf;}
  if (!(ca->methname = (*jnienv)->NewStringUTF(jnienv, method))) { goto barf; }

  ASSERT_NO_EXCP(jnienv, "jasper_setup_for_callback");
  ca->imid = (*jnienv)->GetMethodID(jnienv, cbclass, callbackname, callbacksig);
  (*jnienv)->DeleteLocalRef(jnienv, cbclass);
  if (!(ca->imid)) { goto barf; }

  /* Make an Object array with the original arguments. */
  if (!(ca->jarray = make_arg_array(jnienv, jargs, typesig, numargs))) { goto barf; }

  return JNI_TRUE;

 barf:
  jasper_cleanup_after_callback(jnienv, ca);
  return JNI_FALSE;
}

static jobject jasper_NewObjectA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                 jclass clazz,
                                 char const *method,
                                 jvalue *jargs,
                                 char const *typesig,
                                 int numargs)
{
  jobject retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};
#if JASPER_DBG
  JASPER_DBGLINE(jasper_NewObjectA);
  jasper_DescribeClass(jnienv, clazz);
#endif
  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackNewObject",
                                 "(Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;",
                                 &ca)) { return NULL; }
  retval = (*jnienv)->CallObjectMethod(jnienv, ca.server, ca.imid, clazz,
                                       ca.methname, ca.jarray, ca.signature);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

/* All callBack<Foo> methods share the same signature for the arguments,
   differing only in their return value. */

#define CALLBACKSIG(RET_TYPE) "(Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;Z)" #RET_TYPE

static void jasper_CallVoidMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                   jobject jobj,  /* object (or class) to
                                                     invoke method on */
                                   char const *method,  /* method name */
                                   jvalue *jargs, /* argument array */
                                   char const *typesig, /* type signature */
                                   int numargs,   /* number of arguments */
                                   jboolean staticP /* static or instance? */
                                   )
{
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackVoid",CALLBACKSIG(V),&ca)) {return;}
  (*jnienv)->CallVoidMethod(jnienv, ca.server, ca.imid, jobj, ca.methname,
                            ca.jarray, ca.signature, staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
}

static jobject jasper_CallObjectMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                        jobject jobj,
                                        char const *method,
                                        jvalue *jargs,
                                        char const *typesig,
                                        int numargs,
                                        jboolean staticP)
{
  jobject retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackObject",CALLBACKSIG(Ljava/lang/Object;),
                                 &ca)) { return NULL; }
  retval = (*jnienv)->CallObjectMethod(jnienv, ca.server, ca.imid, jobj,
                                       ca.methname, ca.jarray, ca.signature,
                                       staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jboolean jasper_CallBooleanMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                          jobject jobj,
                                          char const *method,
                                          jvalue *jargs,
                                          char const *typesig,
                                          int numargs,
                                          jboolean staticP)
{
  jboolean retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackBoolean", CALLBACKSIG(Z), &ca)) { return JNI_FALSE; }
  retval = (*jnienv)->CallBooleanMethod(jnienv, ca.server, ca.imid, jobj,
                                        ca.methname, ca.jarray, ca.signature,
                                        staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jbyte jasper_CallByteMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                    jobject jobj,
                                    char const *method,
                                    jvalue *jargs,
                                    char const *typesig,
                                    int numargs,
                                    jboolean staticP)
{
  jbyte retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackByte",CALLBACKSIG(B),&ca)) { return 0;}
  retval = (*jnienv)->CallByteMethod(jnienv,ca.server, ca.imid, jobj,
                                     ca.methname, ca.jarray, ca.signature,
                                     staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jchar jasper_CallCharMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                    jobject jobj,
                                    char const *method,
                                    jvalue *jargs,
                                    char const *typesig,
                                    int numargs,
                                    jboolean staticP)
{
  jchar retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackChar",CALLBACKSIG(C),&ca))
    { return '\0'; }
  retval = (*jnienv)->CallCharMethod(jnienv,ca.server, ca.imid, jobj, ca.methname,
                                     ca.jarray, ca.signature, staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jshort jasper_CallShortMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                      jobject jobj,
                                      char const *method,
                                      jvalue *jargs,
                                      char const *typesig,
                                      int numargs,
                                      jboolean staticP)
{
  jshort retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackShort",CALLBACKSIG(S),&ca)) {return 0;}
  retval = (*jnienv)->CallShortMethod(jnienv, ca.server, ca.imid, jobj,
                                    ca.methname, ca.jarray, ca.signature,
                                    staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jint jasper_CallIntMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                  jobject jobj,
                                  char const *method,
                                  jvalue *jargs,
                                  char const *typesig,
                                  int numargs,
                                  jboolean staticP)
{
  jint retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackInt",CALLBACKSIG(I),&ca)) {return 0;}
  retval = (*jnienv)->CallIntMethod(jnienv, ca.server, ca.imid, jobj,
                                    ca.methname, ca.jarray, ca.signature,
                                    staticP);
#if JASPER_DBG
    fprintf(stderr, "jasper_CallIntMethodA: retval==%d\n", (int)retval);
#endif
    jasper_cleanup_after_callback(jnienv, &ca);
    return retval;
}

static jlong jasper_CallLongMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                    jobject jobj,
                                    char const *method,
                                    jvalue *jargs,
                                    char const *typesig,
                                    int numargs,
                                    jboolean staticP)
{
  jlong retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackLong",CALLBACKSIG(J),&ca)) {return 0;}
  retval = (*jnienv)->CallLongMethod(jnienv, ca.server, ca.imid, jobj,
                                     ca.methname, ca.jarray, ca.signature,
                                     staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jfloat jasper_CallFloatMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                      jobject jobj,
                                      char const *method,
                                      jvalue *jargs,
                                      char const *typesig,
                                      int numargs,
                                      jboolean staticP)
{
  jfloat retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackFloat",CALLBACKSIG(F),&ca)) {return 0.0;}
  retval = (*jnienv)->CallFloatMethod(jnienv, ca.server, ca.imid, jobj,
                                      ca.methname, ca.jarray, ca.signature,
                                      staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

static jdouble jasper_CallDoubleMethodA(SPAPI_ARG_PROTO_DECL JNIEnv *jnienv,
                                        jobject jobj,
                                        char const *method,
                                        jvalue *jargs,
                                        char const *typesig,
                                        int numargs,
                                        jboolean staticP)
{
  jdouble retval;
  struct call_args ca = {NULL, NULL, NULL, NULL, NULL};

  if (!jasper_setup_for_callback(SPAPI_ARG jnienv,method,jargs,typesig,numargs,
                                 "callBackDouble",CALLBACKSIG(D),&ca)) {return 0.0;}
  retval = (*jnienv)->CallDoubleMethod(jnienv, ca.server, ca.imid, jobj,
                                       ca.methname, ca.jarray, ca.signature,
                                       staticP);
  jasper_cleanup_after_callback(jnienv, &ca);
  return retval;
}

/* If oldstring matches the beginning of oldstring_buf, append newstring to
   newstring_buf. Otherwise append oldstring to newstring_buf.
   newstring_buf will be incremented by the length of the appended string.
   oldstring_buf will be incremented by the length of the match.
*/
static void maybe_substitute(char **newstring_buf, char const **oldstring_buf,
                             const char *newstring, const char *oldstring,
                             int newstring_length, int oldstring_length)
{
  char const *semicolonp = *oldstring_buf;
  while (*semicolonp != ';') { semicolonp++; }
  semicolonp++;
  if (0 == strncmp(*oldstring_buf, oldstring, semicolonp - *oldstring_buf)) {
    strcpy(*newstring_buf, newstring);
    *newstring_buf += newstring_length;
    *oldstring_buf += oldstring_length;
  } else {
    strncpy(*newstring_buf, *oldstring_buf, semicolonp - *oldstring_buf);
    *newstring_buf += (semicolonp - *oldstring_buf);
    *oldstring_buf = semicolonp;
  }
}

/* Substitute all occurrences of oldType in typesig with newType.
   newtypesig is a buffer with the same size as typesig.
   Returns a char* to the newtypesig buffer.
*/
static char *make_typesig_new_style(char **newtypesig, char const *typesig,
                                    const char *newType, const char *oldType)
{
  size_t newType_lengthz = strlen(newType);
  size_t oldType_lengthz = strlen(oldType);
  int newType_length;
  int oldType_length;
  char const *tp_old = typesig;
  char *tp_new = *newtypesig;

  if (!(newType_lengthz < INT_MAX && oldType_lengthz < INT_MAX))
    {
      /* SP_SOFT_ASSERT(0); */
      return NULL;
    }
  newType_length = (int) newType_lengthz;
  oldType_length = (int) oldType_lengthz;

  if (newType_length > oldType_length) /* [PM] 4.0 detect buffer overrun */
    {
      return NULL;
    }

  /* Skip to arguments */
  while (*tp_old != '(') { tp_old++; }
  tp_old++;
  tp_new[0] = '(';
  tp_new++;
  /* Process argument list types */
  while (*tp_old != ')') {
    if (*tp_old == 'L') {
      maybe_substitute(&tp_new, &tp_old, newType, oldType, newType_length, oldType_length);
    } else {
      *tp_new = *tp_old;
      tp_new++;
      tp_old++;
    }
  }
  tp_old++;
  *tp_new = ')';
  tp_new++;
  /* Take care of return type */
  if (*tp_old == '[') {
      *tp_new = *tp_old;
      tp_new++;
      tp_old++;
  }
  if (*tp_old == 'L') {
    maybe_substitute(&tp_new, &tp_old, newType, oldType, newType_length, oldType_length);
  } else {
    *tp_new = *tp_old;
    tp_new++;
  }
  *tp_new = '\0';
  return *newtypesig;
}

void SPCDECL
jasperi_call_static1_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref,
                       char SP_FLI_CONST *className,
                       char SP_FLI_CONST *methodName,
                       char SP_FLI_CONST *typesig,
                       SP_term_ref methodDesc,
                       SP_term_ref args)

{
  char const *return_type_sig;        /* e.g. "Ljava/lang/String;" */
  jclass clazz = NULL;
  jmethodID mid;
  jvalue *jargs = NULL;
  JNIEnv *jnienv;
  int returns_term;             /* see jasperi_call_static1_C */
  struct meta_args_state state;
  int result = SP_ERROR;
  int init = FALSE;             /* true if <init>, i.e., NewObject */
#if THREADSERVER
  jboolean threadservermode = JNI_FALSE;
  char *newtypesig1 = NULL; /* Remember to SP_free() this! */
  char *newtypesig2 = NULL; /* Remember to SP_free() this! */
  char const *realtypesig;
  jboolean new_style_interface = JNI_FALSE;
#endif  /* THREADSERVER */

  SP_uinteger context = 0;

  DbgBreakPoint();

#if THREADSERVER
  /* [PM] 4.2.2 FIXME: SP_malloc(strlen(typesig) +1) would suffice */
  LAZY_NULL_CHECK(newtypesig1 = SP_strdup(typesig));
  LAZY_NULL_CHECK(newtypesig2 = SP_strdup(typesig));
#endif  /* THREADSERVER */

#if JASPER_DBG
  fprintf(stderr, "*** jasperi_call_static1_C Calling %s in %s, signature %s.\n", methodName, className, typesig);
#endif

  init = (strcmp(methodName, "<init>")==0); /* call NewObject */

  JASPER_DBGLINE(jasperi_call_static1_C);
  SET_JNIENV(jnienv,jvmref, {goto cleanup;});

#if JASPER_DBG
  fprintf(stderr, "*** jasperi_call_static1_C jnienv: %p\n", (void*)jnienv);
#endif /* JASPER_DBG */

#if LOCALREFS_RESET
    if ((*jnienv)->PushLocalFrame(jnienv, 0) < 0) { return; }
#endif

  JASPER_DBGLINE(jasperi_call_static1_C);
  ASSERT_NO_EXCP(jnienv, "jasperi_call_static1_C");
  JASPER_DBGLINE(jasperi_call_static1_C);
  if ( (clazz = jasperi_lookup_class(SPAPI_ARG jnienv,className)) == NULL ) goto cleanup;
  JASPER_DBGLINE(jasperi_call_static1_C);

#if THREADSERVER
  {
    int tmode;
    if (!sp_get_jasper_threadservermode(&tmode, SICSTUS_VERSION)) goto cleanup;
    threadservermode = (tmode == 1);
  }

  /* If an argument type is /se/sics/jasper/SPTerm, try the method lookup with
     that type substituted with se/sics/jasper/Term. If that fails, use the
     original type signature. */
  realtypesig = make_typesig_new_style(&newtypesig1, typesig,
                                       "Lse/sics/jasper/Term;",
                                       "Lse/sics/jasper/SPTerm;");
  (void)realtypesig;
  SP_ASSERT(realtypesig != NULL); /* == newtypesig1 unless overflow, which cannot happen since newType is shorter. */
  realtypesig = make_typesig_new_style(&newtypesig2, newtypesig1,
                                       "Ljava/lang/String;",
                                       "Lse/sics/jasper/SPCanonicalAtom;");
  SP_ASSERT(realtypesig != NULL);
  { /* [PD] 3.9.1 Constructors are methods, too. */
    int midflag = (
                        init
                        ?
                        ((mid = (*jnienv)->GetMethodID(jnienv,clazz,methodName,newtypesig2)) == 0)
                        :
                        ((mid = (*jnienv)->GetStaticMethodID(jnienv,clazz,methodName,newtypesig2)) == 0)
                        );
      if (midflag) {
        (*jnienv)->ExceptionClear(jnienv); /* Clear exception from failed method
                                          lookup. */
#endif
        if ( (mid = jasperi_lookup_method(SPAPI_ARG jnienv,clazz,methodName,typesig, ( !init ? 1/*static*/ : 0/*instance/init*/))) == 0 ) goto cleanup;
#if THREADSERVER
        realtypesig = typesig;
      } else {
        new_style_interface = JNI_TRUE;
      }
  }
#endif
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* If we do not get here then an error was thrown before going to cleanup: */

  /* Skip over argument types to find out the return type.
     No need to guard against missing ')' since jasperi_lookup_method succeeded. */
  {
#if THREADSERVER
    char const *p = realtypesig;
#else
    char const *p = typesig;
#endif
    while (*p != ')') p++;
    p++;    /* skip ')' */
    return_type_sig = p;
  }
  /* must be done before creating SPTerms for parameter passing */
  context = jasper_push_context(SPAPI_ARG jnienv);
  /* Prolog run-time monitor is (still) owned by this thread */

  /* get arg vector */
  {
    int rc;
#if THREADSERVER
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs,
                                   &returns_term, &state, FALSE/*!instance*/,
                                   1, /* CALLING set up jargs for call */
                                   new_style_interface, threadservermode);
#else
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 1 /* CALLING set up jargs for call */);
#endif
    switch (rc)
      {
      case SP_SUCCESS:
        JASPER_DBGLINE(jasperi_call_static1_C);
        break;
      case SP_ERROR:
        JASPER_DBGLINE(jasperi_call_static1_C);
        goto cleanup;
        break;
      default:                    /* should not happen */
        JASPER_DBGLINE(jasperi_call_static1_C);
#if JASPER_DBG
        fprintf(stderr, "FATAL ERROR: jasperi_call_static1_C jasperi_process_meta_args returned %d\n", rc);
#endif
        break;
      }
  }
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* Prolog run-time monitor is (still) owned by this thread */

  if (!jasper_leave_context_monitor(jnienv,context)) {
#if JASPER_DBG
      fprintf(stderr, "FATAL ERROR: jasperi_call_static1_C jasper_leave_context_monitor returned 0\n");
#endif
      goto cleanup;             /* NOTE: Fix error handling */
  }
  JASPER_DBGLINE(jasperi_call_static1_C);
  /* Prolog run-time monitor is *not* owned by this thread, i.e., Java
     context, no SP runtime calls allowed */


  /*** NOTE: if we get here we must enter monitor so cannot goto
       cleanup directly
  ****/
  JASPER_DBGLINE(jasperi_call_static1_C);

#if JASPER_DBG
#if THREADSERVER
  fprintf(stderr, "*** jasperi_call_static1_C Lookup complete. Performing call. %s::%s %s jargs=%p\n", className, methodName, realtypesig, jargs);
#else
  fprintf(stderr, "*** jasperi_call_static1_C Lookup complete. Performing call. %s::%s %s jargs=%p\n", className, methodName, typesig, jargs);
#endif
#endif

  {
    int result_is_object = FALSE;

    JASPER_DBGLINE(jasperi_call_static1_C);
    {
      int illegal_method = FALSE;
      int returned_term_error = FALSE;
#if THREADSERVER
      int numargs = state.numargs;
#endif

      if (init)                 /* new <<CLASS>> */
        {
#if THREADSERVER
          if (threadservermode) {
            jargs[0].l = jasper_NewObjectA(SPAPI_ARG jnienv, clazz, methodName, jargs+1,
                                           realtypesig, numargs-1);
          } else {
#endif
          jargs[0].l = (*jnienv)->NewObjectA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
          }
#endif
          result_is_object = TRUE;
        }
      else                      /* normal case, static method */
        {
          switch (return_type_sig[0])
            {
            case 'V':
#if THREADSERVER
              if (threadservermode) {
                jasper_CallVoidMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                       jargs+1, /* ! +1 also for VOID */
                                       realtypesig, numargs,
                                       JNI_TRUE); /* static */
              } else {
#endif
              #if JASPER_DBG>1
              {
                int i;
                for (i = 1; i <= state.numargs; i++)
                  {
                    fprintf(stderr, "CallStaticVoidMethodA %s::%s jargs[%d]==%p (%s)\n", className, methodName, i, ( state.is_object[i] ? (jargs)[i].l : 0 ), ( state.is_object[i] ? "object": "primitive"));
                  }
              }
              #endif
              (*jnienv)->CallStaticVoidMethodA(jnienv, clazz, mid, jargs+1 /* ! +1 also for VOID */);
#if THREADSERVER
              }
#endif
              break;
            case '[':                   /* Arrays are objects too */
            case 'L':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].l = jasper_CallObjectMethodA(SPAPI_ARG jnienv,clazz, methodName,
                                                      jargs+1, realtypesig,
                                                      numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].l = (*jnienv)->CallStaticObjectMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              result_is_object = TRUE;
              if (returns_term && !(*jnienv)->ExceptionCheck(jnienv))
                {                   /* see jasperi_call_static1_C */
                  SP_term_ref returned_term;
                  if (!SPJavaGlueGetNativeTermRefInJavaContextM(/*SPAPI_ARG*/ jnienv, jargs[0].l, &returned_term))
                    {
                      JASPER_DBGLINE(jasperi_call_static1_C);
                      returned_term_error = TRUE;
                    }
                  else              /* no error */
                    {
                      JASPER_DBGLINE(jasperi_call_static1_C);
                      (*jnienv)->DeleteLocalRef(jnienv,jargs[0].l);
                      returns_term = FALSE; /* signals cleanup code that jargs[0] no longer is an object */
                      jargs[0].j = (jlong) returned_term;
                    }
                }
              break;
            case 'Z':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].z = jasper_CallBooleanMethodA(SPAPI_ARG jnienv,clazz,methodName,
                                                       jargs+1, realtypesig,
                                                       numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].z = (*jnienv)->CallStaticBooleanMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'B':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].b = jasper_CallByteMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                    jargs+1, realtypesig,
                                                    numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].b = (*jnienv)->CallStaticByteMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'C':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].c = jasper_CallCharMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                    jargs+1, realtypesig,
                                                    numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].c = (*jnienv)->CallStaticCharMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'S':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].s = jasper_CallShortMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                     jargs+1, realtypesig,
                                                     numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].s = (*jnienv)->CallStaticShortMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'I':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].i = jasper_CallIntMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                   jargs+1, realtypesig,
                                                   numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].i = (*jnienv)->CallStaticIntMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'J':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].j = jasper_CallLongMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                    jargs+1, realtypesig,
                                                    numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].j = (*jnienv)->CallStaticLongMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'F':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].f = jasper_CallFloatMethodA(SPAPI_ARG jnienv, clazz, methodName,
                                                     jargs+1, realtypesig,
                                                     numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].f = (*jnienv)->CallStaticFloatMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            case 'D':
#if THREADSERVER
              if (threadservermode) {
                jargs[0].d = jasper_CallDoubleMethodA(SPAPI_ARG jnienv,clazz, methodName,
                                                      jargs+1, realtypesig,
                                                      numargs-1, JNI_TRUE);
              } else {
#endif
              jargs[0].d = (*jnienv)->CallStaticDoubleMethodA(jnienv, clazz, mid, jargs+1);
#if THREADSERVER
              }
#endif
              break;
            default:
              /* We cannot call jasper_int_handle_exception in Java context */
              illegal_method = TRUE;
            }
        }

      /* Java context */

      /* NOTE: error handling */
      jasper_enter_context_monitor(jnienv,context);


      /* Prolog context, monitor owned by this thread */

      if (illegal_method)
        {
          jasper_int_handle_exception(SPAPI_ARG jnienv,"Illegal method descriptor" " @" __FILE__ ":" STRINGISIZE(__LINE__),1);
          goto cleanup;
        }


      JASPER_DBGLINE(jasperi_call_static1_C);
      /* If we have a pending Java exception here then it comes from
         the actual method call and should be propagated as a global ref to the Exception object */
      if ( SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, -1,  ("jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__)))
           ||
           /* If we failed to get the term ref without causing a Java error then give generic error */
           ( returned_term_error
             && jasper_int_handle_exception(SPAPI_ARG jnienv,"jasper_call_static" " @" __FILE__ ":" STRINGISIZE(__LINE__), 1) ))
        {
          if (result_is_object
              && (!returned_term_error) /* Do not NULL jargs[0].l as it was returned without error and should be freed.*/
              )
            {
              jargs[0].l = NULL;    /* ensure cleanup does not DeleteLocalRef garbage */
            }
          goto cleanup;
        }
    }
  }
  /* from here on jargs[0] is valid. */
  JASPER_DBGLINE(jasperi_call_static1_C);

  /* (Still) Prolog context, monitor owned by this thread */

  /* get arg vector */
#if THREADSERVER
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs,
                                     &returns_term, &state, FALSE/*!instance*/,
                                     2, /* RETURNING handle return values */
                                     new_style_interface, threadservermode);
#else
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 2 /* RETURNING handle return values */);
#endif
  JASPER_DBGLINE(jasperi_call_static1_C);

 cleanup:
#if THREADSERVER
  SP_free(newtypesig1);
  SP_free(newtypesig2);
#endif
  if (context != 0) {
    jasper_pop_context(SPAPI_ARG jnienv,context);
  }

  JASPER_DBGLINE(jasperi_call_static1_C);
#if THREADSERVER
  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc,
                                              args, &jargs, &returns_term,
                                              &state, FALSE/*!instance*/,
                                              3 /* CLEANUP */,
                                              new_style_interface,
                                              threadservermode);
#else
  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, FALSE/*!instance*/, 3 /* CLEANUP */);
#endif
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);
  JASPER_DBGLINE(jasperi_call_static1_C);

  if (result == SP_FAILURE)
    {
      SP_fail();
    }

#if LOCALREFS_RESET
  (*jnienv)->PopLocalFrame(jnienv, NULL);
#endif
}

void SPCDECL
jasperi_call_instance1_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref,
                         char SP_FLI_CONST *methodName,
                         char SP_FLI_CONST *typesig,
                         SP_term_ref methodDesc,
                         SP_term_ref args)
{
  char const *return_type_sig;        /* e.g. "Ljava/lang/String;" */
  jclass clazz = NULL;
  jobject jobj = NULL;
  jmethodID mid;
  jvalue *jargs = NULL;
  JNIEnv *jnienv;
  int returns_term;             /* see jasperi_process_meta_args */
  struct meta_args_state state;
  int result = SP_ERROR;
#if THREADSERVER
  jboolean threadservermode = JNI_FALSE;
  char *newtypesig1 = NULL; /* Remember to SP_free() this! */
  char *newtypesig2 = NULL; /* Remember to SP_free() this! */
  char const *realtypesig;
  jboolean new_style_interface = JNI_FALSE;
#endif  /* THREADSERVER */

  SP_uinteger context = 0;

#if THREADSERVER
  /* [PM] 4.2.2 FIXME: SP_malloc(strlen(typesig) +1) would suffice */
  LAZY_NULL_CHECK(newtypesig1 = SP_strdup(typesig));
  LAZY_NULL_CHECK(newtypesig2 = SP_strdup(typesig));
#endif  /* THREADSERVER */

#if JASPER_DBG
  fprintf(stderr, "*** jasperi_call_instance1_C Calling %s, signature %s.\n", methodName, typesig);
#endif

  SET_JNIENV(jnienv,jvmref, {goto cleanup;});

#if LOCALREFS_RESET
    if ((*jnienv)->PushLocalFrame(jnienv, 0) < 0) { return; }
#endif

  ASSERT_NO_EXCP(jnienv, "jasperi_call_instance1_C");

  {
    SP_term_ref objref = SP_new_term_ref();
   
    if (!SP_get_arg(1, args, objref))
      {
        JASPER_DBGLINE(jasperi_call_instance1_C);
        jasper_int_handle_exception(SPAPI_ARG jnienv,"No object",1);
        goto cleanup;
      }
    if (!termref_to_jobject(SPAPI_ARG objref, &jobj) || jobj==NULL)
      {
        JASPER_DBGLINE(jasperi_call_instance1_C);
        jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid on null object",1);
        goto cleanup;
      }

  }
  DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jasperi_call_instance1_C");

  if ( (clazz = jasperi_lookup_class_from_obj(SPAPI_ARG jnienv,jobj)) == NULL ) goto cleanup;
#if THREADSERVER
  {
    int tmode;
    if (!sp_get_jasper_threadservermode(&tmode, SICSTUS_VERSION)) goto cleanup;
    threadservermode = (tmode ==1);
  }

  /* If an argument type is /se/sics/jasper/SPTerm, try the method lookup with
     that type substituted with se/sics/jasper/Term. If that fails, use the
     original type signature. */
  realtypesig = make_typesig_new_style(&newtypesig1, typesig,
                                       "Lse/sics/jasper/Term;",
                                       "Lse/sics/jasper/SPTerm;");
  (void)realtypesig;
  SP_ASSERT(realtypesig != NULL);
  realtypesig = make_typesig_new_style(&newtypesig2, newtypesig1,
                                       "Ljava/lang/String;",
                                       "Lse/sics/jasper/SPCanonicalAtom;");
  SP_ASSERT(realtypesig != NULL);
  if ( (mid = (*jnienv)->GetMethodID(jnienv,clazz,methodName,newtypesig2)) == 0) {
    (*jnienv)->ExceptionClear(jnienv); /* Clear exception from failed method
                                          lookup. */
#endif
    if ( (mid = jasperi_lookup_method(SPAPI_ARG jnienv,clazz,methodName,typesig,0/*!static*/)) == 0 ) {
#if JASPER_DBG
      fprintf(stderr, "*** jasperi_call_instance1_C: second method lookup failed. methodName==%s, typesig==%s.\n", methodName, typesig);
#endif
      goto cleanup;
    }
#if THREADSERVER
    realtypesig = typesig;
  } else {
    new_style_interface = JNI_TRUE;
  }
#endif

  /* Skip over argument types to find out the return type.
     No need to guard against missing ')' since jasperi_lookup_method succeeded. */
  {
#if THREADSERVER
    char const *p = realtypesig;
#else
    char const *p = typesig;
#endif
    while (*p != ')') p++;
    p++;    /* skip ')' */
    return_type_sig = p;
  }

  /* must be done before creating SPTerms for parameter passing */
  context = jasper_push_context(SPAPI_ARG jnienv);
  /* Prolog run-time monitor is (still) owned by this thread */

  /* get arg vector */
  {
    int rc;
#if THREADSERVER
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs,
                                   &returns_term, &state, TRUE/*instance*/,
                                   1 /* CALLING set up jargs for call */,
                                   new_style_interface, threadservermode);
#else
    rc = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 1 /* CALLING set up jargs for call */);
#endif
    switch (rc)
      {
      case SP_SUCCESS:
        JASPER_DBGLINE(jasperi_call_instance1_C);
        break;
      case SP_ERROR:
        JASPER_DBGLINE(jasperi_call_instance1_C);
        goto cleanup;
        break;
      default:                    /* should not happen */
        JASPER_DBGLINE(jasperi_call_instance1_C);
#if JASPER_DBG
        fprintf(stderr, "FATAL ERROR: jasperi_call_instance1_C jasperi_process_meta_args returned %d\n", rc);
#endif
        break;
      }
  }

  /* Prolog run-time monitor is (still) owned by this thread */

  if (!jasper_leave_context_monitor(jnienv,context)) {
#if JASPER_DBG
      fprintf(stderr, "FATAL ERROR: jasperi_call_instance1_C jasper_leave_context_monitor returned 0\n");
#endif
      goto cleanup;             /* NOTE: Fix error handling */
  }

  /* Prolog run-time monitor is *not* owned by this thread, i.e., Java context, no SP runtime calls allowed */
  /*** NOTE: if we get here we must enter monitor so cannot goto
       cleanup directly
  ****/
  JASPER_DBGLINE(jasperi_call_instance1_C);

  {
    int result_is_object = FALSE;
    {
      int illegal_method = FALSE;
      int returned_term_error = FALSE;
#if THREADSERVER
      int numargs = state.numargs - 1; /* First arg is object? */
#endif

      switch (return_type_sig[0])
        {
        case 'V':
#if THREADSERVER
          if (threadservermode) {
            jasper_CallVoidMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                   jargs+1, /* ! +1 also for VOID */
                                   realtypesig, numargs,
                                   JNI_FALSE); /* instance */
          } else {
#endif
          (*jnienv)->CallVoidMethodA(jnienv, jobj, mid, jargs+1); /* ! +1 also for VOID */
#if THREADSERVER
          }
#endif
          break;
        case '[':                   /* Arrays are objects too */
        case 'L':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].l = jasper_CallObjectMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                  jargs+1, realtypesig, numargs-1,
                                                  JNI_FALSE);
          } else {
#endif
          jargs[0].l = (*jnienv)->CallObjectMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          result_is_object = TRUE;
          if (returns_term && !(*jnienv)->ExceptionCheck(jnienv))
            {                   /* see jasperi_process_meta_args */
              SP_term_ref returned_term;
              if (!SPJavaGlueGetNativeTermRefInJavaContextM(/*SPAPI_ARG*/ jnienv, jargs[0].l, &returned_term))
                {
                  JASPER_DBGLINE(jasperi_call_instance1_C);
                  returned_term_error = TRUE;
                }
              else              /* no error */
                {
                  JASPER_DBGLINE(jasperi_call_instance1_C);
                  (*jnienv)->DeleteLocalRef(jnienv,jargs[0].l);
                  returns_term = FALSE; /* signals cleanup code that jargs[0] no longer is an object */
                  jargs[0].j = (jlong) returned_term;
                }
            }
          break;
        case 'Z':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].z = jasper_CallBooleanMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                   jargs+1, realtypesig, numargs-1,
                                                   JNI_FALSE);
          } else {
#endif
          jargs[0].z = (*jnienv)->CallBooleanMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'B':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].b = jasper_CallByteMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                jargs+1, realtypesig, numargs-1,
                                                JNI_FALSE);
          } else {
#endif
          jargs[0].b = (*jnienv)->CallByteMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'C':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].c = jasper_CallCharMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                jargs+1, realtypesig, numargs-1,
                                                JNI_FALSE);
          } else {
#endif
          jargs[0].c = (*jnienv)->CallCharMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'S':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].s = jasper_CallShortMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                 jargs+1, realtypesig, numargs-1,
                                                 JNI_FALSE);
          } else {
#endif
          jargs[0].s = (*jnienv)->CallShortMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'I':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].i = jasper_CallIntMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                               jargs+1, realtypesig, numargs-1,
                                               JNI_FALSE);
          } else {
#endif
          jargs[0].i = (*jnienv)->CallIntMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'J':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].j = jasper_CallLongMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                jargs+1, realtypesig, numargs-1,
                                                JNI_FALSE);
          } else {
#endif
          jargs[0].j = (*jnienv)->CallLongMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'F':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].f = jasper_CallFloatMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                 jargs+1, realtypesig, numargs-1,
                                                 JNI_FALSE);
          } else {
#endif
          jargs[0].f = (*jnienv)->CallFloatMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        case 'D':
#if THREADSERVER
          if (threadservermode) {
            jargs[0].d = jasper_CallDoubleMethodA(SPAPI_ARG jnienv, jobj, methodName,
                                                  jargs+1, realtypesig, numargs-1,
                                                  JNI_FALSE);
          } else {
#endif
          jargs[0].d = (*jnienv)->CallDoubleMethodA(jnienv, jobj, mid, jargs+1);
#if THREADSERVER
          }
#endif
          break;
        default:
          /* We cannot call jasper_int_handle_exception in Java context */
          illegal_method  = TRUE;
        }
      /* Java context */

      /* NOTE: error handling */
      jasper_enter_context_monitor(jnienv,context);


      /* Prolog context, monitor owned by this thread */

      if (illegal_method)
        {
          jasper_int_handle_exception(SPAPI_ARG jnienv,"Illegal method descriptor" " @" __FILE__ ":" STRINGISIZE(__LINE__),1);
          goto cleanup;
        }

      /* If we have a pending Java exception here then it comes from
         the actual method call and should be propagated as a global ref to the Exception object */
      if ( SPJavaGluePropagateJavaExceptionM(SPAPI_ARG jnienv, -1,  ("jasper_call_instance" " @" __FILE__ ":" STRINGISIZE(__LINE__)))
           ||
           /* If we failed to get the term ref without causing a Java error then give generic error */
           ( returned_term_error
             && jasper_int_handle_exception(SPAPI_ARG jnienv,"jasper_call_instance" " @" __FILE__ ":" STRINGISIZE(__LINE__), 1) ))
        {
          if (result_is_object
              && (!returned_term_error) /* Do not NULL jargs[0].l as it was returned without error and should be freed.*/
              )
            {
              jargs[0].l = NULL;    /* ensure cleanup does not DeleteLocalRef garbage */
            }
          goto cleanup;
        }
    }
  }
  /* from here on jargs[0] is valid. */
  
  /* (still) Prolog context, monitor owned by this thread */
  
  /* get arg vector */
#if THREADSERVER
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs,
                                     &returns_term, &state, TRUE/*instance*/,
                                     2 /* RETURNING handle return values */,
                                     new_style_interface, threadservermode);
#else
  result = jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 2 /* RETURNING handle return values */);
#endif

 cleanup:
#if THREADSERVER
  SP_free(newtypesig1);
  SP_free(newtypesig2);
#endif
  if (context != 0) {
    jasper_pop_context(SPAPI_ARG jnienv,context);
  }

#if THREADSERVER
  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc,
                                              args, &jargs, &returns_term,
                                              &state, TRUE/*instance*/,
                                              3 /* CLEANUP */,
                                              new_style_interface,
                                              threadservermode);
#else
  if (jargs) (void) jasperi_process_meta_args(SPAPI_ARG jnienv, methodDesc, args, &jargs, &returns_term, &state, TRUE/*instance*/, 3 /* CLEANUP */);
#endif
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);

  if (result == SP_FAILURE)
    {
      SP_fail();
    }

#if LOCALREFS_RESET
  (*jnienv)->PopLocalFrame(jnienv, NULL);
#endif
}
#endif /* THREADSERVER */


/* FOREIGN */
SP_term_ref SPCDECL
jasperi_create_global_ref_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref objref)
{
  int barf = 1;
  JNIEnv *jnienv;
  jobject jobj = NULL;
  SP_term_ref tr = SP_new_term_ref();

  /* GET_JNIENV_RV(jvmref,jnienv,objref); */
  SET_JNIENV(jnienv, jvmref, goto cleanup;);

  if (!termref_to_jobject(SPAPI_ARG objref, &jobj)) goto cleanup;

  DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jasperi_create_global_ref_C (ref)");

  barf = 0;
  
 cleanup:

  if (!barf)
    {
      jobject gobj = ( jobj ? (*jnienv)->NewGlobalRef(jnienv,jobj) : NULL );
      ASSERT_NO_EXCP(jnienv, "jasperi_create_global_ref_C");
      DBG_PRINT_OBJECT_CLASS(jnienv, gobj, "jasperi_create_global_ref_C (global ref)");

      jobject_to_termref(SPAPI_ARG jnienv, gobj, tr);
    }
  else
    {
#if JASPER_DBG
      fprintf(stderr, "barf in jasperi_create_global_ref_C\n");
#endif
      jobject_to_termref(SPAPI_ARG jnienv, NULL, tr);
    }
  return tr;
}

/* FOREIGN */
SP_term_ref SPCDECL
jasperi_create_local_ref_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref objref)
{
  int barf = 1;
  JNIEnv *jnienv;
  jobject jobj = NULL;
  SP_term_ref tr = SP_new_term_ref();

  /* GET_JNIENV_RV(jvmref,jnienv,objref); */
  SET_JNIENV(jnienv, jvmref, goto cleanup;);

  if (!termref_to_jobject(SPAPI_ARG objref, &jobj)) goto cleanup;

  DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jasperi_create_local_ref_C (ref)");

  barf = 0;
  
 cleanup:

  if (!barf)
    {
      jobject gobj = ( jobj ? (*jnienv)->NewLocalRef(jnienv,jobj) : NULL );
      ASSERT_NO_EXCP(jnienv, "jasperi_create_local_ref_C");
      DBG_PRINT_OBJECT_CLASS(jnienv, gobj, "jasperi_create_local_ref_C (local ref)");

      jobject_to_termref(SPAPI_ARG jnienv, gobj, tr);
    }
  else
    {
#if JASPER_DBG
      fprintf(stderr, "barf in jasperi_create_local_ref_C\n");
#endif
      jobject_to_termref(SPAPI_ARG jnienv, NULL, tr);
    }
  return tr;
}

/* FOREIGN */
void SPCDECL
jasperi_delete_global_ref_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref objref)
{
  JNIEnv *jnienv;
  jobject jobj;

  SET_JNIENV(jnienv,jvmref, { return; });

  if (!termref_to_jobject(SPAPI_ARG objref, &jobj)) /* NULL is OK */
    {
      JASPER_DBGLINE(jasperi_delete_global_ref_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid object",1);
      goto cleanup;
    }

  DELETE_GLOBAL_REF(jnienv,jobj);
 cleanup:
  ;
}

/* FOREIGN */ 
void SPCDECL
jasperi_delete_local_ref_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref objref)
{
  JNIEnv *jnienv;
  jobject jobj;

  SET_JNIENV(jnienv,jvmref, { return; });
  ASSERT_NO_EXCP(jnienv, "jasperi_delete_local_ref_C");
  
  if (!termref_to_jobject(SPAPI_ARG objref, &jobj)) /* NULL is OK */
    {
      JASPER_DBGLINE(jasperi_delete_local_ref_C);
      jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid object",1);
      goto cleanup;
    }

  DELETE_LOCAL_REF(jnienv,jobj);
 cleanup:
  ;
}

/* FOREIGN */
SP_integer SPCDECL
jasperi_is_same_object_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref obj1, SP_term_ref obj2)
{
  JNIEnv *jnienv;
  jobject jobj1,jobj2;


  SET_JNIENV(jnienv,jvmref, { return 0;});
  ASSERT_NO_EXCP(jnienv, "jasperi_is_same_object_C");

  if (!(termref_to_jobject(SPAPI_ARG obj1, &jobj1)
        && termref_to_jobject(SPAPI_ARG obj2, &jobj2))) goto barf;
  
  if ((*jnienv)->IsSameObject(jnienv,jobj1,jobj2))
    {
      return 1;
    }
  else
    {
      return 0;
    }
 barf:
  {
    JASPER_DBGLINE(jasperi_is_same_object_C);
    jasper_int_handle_exception(SPAPI_ARG jnienv,"Invalid object",1);
  }
  return 0;
}

/* FOREIGN */
SP_integer SPCDECL
jasperi_is_instance_of_C(SPAPI_ARG_PROTO_DECL SP_term_ref jvmref, SP_term_ref obj, char SP_FLI_CONST *classname)
{
  int rc = 0;          /* 0, 1 or -1 (triggers exception in calling Prolog code) */

  JNIEnv *jnienv;
  jclass clazz = NULL;
  jobject jobj = NULL;

  SET_JNIENV(jnienv,jvmref, { return 0;});
  ASSERT_NO_EXCP(jnienv, "jasperi_is_instance_of_C");

  if ((!termref_to_jobject(SPAPI_ARG obj, &jobj)) || jobj==NULL)
    {
      rc = -1;
      goto cleanup;
    }

  DBG_PRINT_OBJECT_CLASS(jnienv, jobj, "jasperi_is_instance_of_C");
  
  clazz = jasperi_lookup_class(SPAPI_ARG jnienv,classname);
  if (clazz == 0)
    rc =-1;
  else if ((*jnienv)->IsInstanceOf(jnienv,jobj,clazz))
    rc = 1;
  else
    rc = 0;
  
 cleanup:
  if (clazz) DELETE_LOCAL_REF(jnienv, clazz);
  return rc;
}

/* return a *local* ref to a new SICStus object */
static jobject jasperi_new_sicstus_object(SPAPI_ARG_PROTO_DECL JNIEnv *env)
{
  jvalue result;
  jboolean hasException = JNI_FALSE;
  
  result = CallStaticMethodByName(env, &hasException, "se/sics/jasper/SICStus",
                                  "getNewSICStusFromAPIPtr",
                                  "(J)Lse/sics/jasper/SICStus;",
                                  /* double cast to avoid gcc warning when sizeof jlong > sizeof ptr */
                                  (jlong) (SP_integer)SICStusDISPATCHVAR
                                  );
  if (hasException)
    {
      int x =
        jasper_int_handle_exception(SPAPI_ARG env, "Could not create SICStus object@" __FILE__ ":" STRINGISIZE(__LINE__), 1);
      (void)x;
      SP_ASSERT(x); /* CallStaticMethodByName hasException but jasper_int_handle_exception returns 0 */
      return NULL;
    }
  return result.l;
}


/** [PM] Keep the original indentation style
 *  Local variables:
 *      indent-tabs-mode: nil
 *      c-basic-offset: 2
 *  end:
 **/

