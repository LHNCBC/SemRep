/**************************************************************************
 * Filename:    spnative.c
 * Author:      Jesper Eskilson <jojo@sics.se>
 **************************************************************************
 */

/* [PD] threadserver jasper-server hack */
#define THREADSERVER 1

/*

make SPLDFLAGS='--verbose --keep' SPLFRFLAGS='--verbose --keep' && bin/spld --verbose --keep library/jasper/spnative.c --main=none --cflag=-LD,-IC:/jdk1.3/include,-IC:/jdk1.3/include/win32,-I./include,-DXMULTI_SP_AWARE=1 --output=bin/spnative.dll && cp -p se/sics/jasper/jasper.jar bin/jasper.jar && make SPLDFLAGS='--verbose --keep' SPLFRFLAGS='--verbose --keep' test TESTS=jasper

make SPLDFLAGS='--verbose --keep' SPLFRFLAGS='--verbose --keep' && bin/spld --verbose --keep library/jasper/spnative.c --main=none --cflag=-LD,-IC:/jdk1.3/include,-IC:/jdk1.3/include/win32,-I./include,-DXMULTI_SP_AWARE=1 --output=bin/spnative.dll && cp -p se/sics/jasper/jasper.jar bin

PATH="./bin:$PATH" java -Dse.sics.jasper.SICStus.debugLevel=1 -Dse.sics.jasper.SICStus.checkSPTermAge=true -Dse.sics.jasper.SICStus.reuseTermRefs=true -classpath bin/jasper.jar se.sics.jasper.SICStus
*/

/* 3.9 [PM]
   For now (3.9 beta 1) we still do not allow more than one SICStus
   run-time.
 */

/* 3.9 [PM], Only one thread may call SP API and (some future version
   of se.sics.jasper will ensure this). Thus no need for Java side
   monitor handling.

   Also, with multiple SP runtimes there is no sense in keeping a
   global sp object. Instead each SP object holds a reference to the
   SICStus API dispatch table.

   There is one instance of this file (and its global variables) in
   each Process. This contrasts with jasper.c which may exist in one
   copy for each SP runtime (if statically linked).

*/
#undef SICSTUS_HIDDEN_API
#define SICSTUS_HIDDEN_API 1
#define SPIO_INCLUDE_OS_TYPES 1 /* for Win32 DebugBreak() */
#include <sicstus/sicstus.h>
#include "jasper.h"             /* [PM] 4.2.1 Defines JASPER_DBG */


#define BARF SPIO_BARF
#define CHECK SPIO_CHECK
#define SPIO_BARF_LABEL barf
#define NULL_CHECK SPIO_NULL_CHECK

#include <jni.h>
#include <stdlib.h>
#include <string.h>

/* #include "se_sics_jasper_SPCanonicalAtom.h" */
#include "se_sics_jasper_SPException.h"
#include "se_sics_jasper_SPPredicate.h"
#include "se_sics_jasper_SPQuery.h"
#include "se_sics_jasper_SPTerm.h"
#include "se_sics_jasper_SICStus.h"
/* [PD] 3.9 */
/* #include "se_sics_jasper_Jasper.h" */

#if SP_USE_GCC_VISIBILITY && SP_HAVE_GCC_VERSION(4,0)
#define SP_JNICALLVISIBILITY __attribute__ ((__visibility__ ("default")))
#else
#define SP_JNICALLVISIBILITY
#endif
#define SP_JNICALL JNICALL SP_JNICALLVISIBILITY

#ifndef TRUE
#define TRUE (1)
#endif

#ifndef FALSE
#define FALSE (!(TRUE))
#endif


#undef DEBUG_BREAK

#if JASPER_DBG
#if SP_WIN32

#if defined(_MSC_VER)
#define DEBUG_BREAK() DebugBreak() /* do {_asm { int 3h };} while(0) */
#endif /* defined(_MSC_VER) */

#endif /* SP_WIN32 */

#ifndef DEBUG_BREAK
#define DEBUG_BREAK() do{ fprintf(stderr, "\nDEBUG BREAK: waiting for input\n");fflush(stderr); (void)getchar(); } while(0)
#endif /* DEBUG_BREAK */

#endif /* JASPER_DBG */

#ifndef DEBUG_BREAK
#define DEBUG_BREAK()           /* default to no-op */
#endif /* DEBUG_BREAK */

#ifdef _MSC_VER
#define BreakPoint()        _asm { int 3h }
#endif

#if JASPER_DBG && 0
#define DbgBreakPoint() BreakPoint()
#else
#define DbgBreakPoint()
#endif

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

static jvalue SPCDECL
CallMethodByName(JNIEnv *env,
                 jboolean *hasException,
                 jobject obj, 
                 const char *name,
                 const char *descriptor,
                 ...);

static jvalue SPCDECL
CallStaticMethodByName(JNIEnv *env,
                       jboolean *hasException,
                       const char *clazzName,
                       const char *name,
                       const char *descriptor,
                       ...);


#define ASSERT_NO_EXCP_FORCE(JNIENV, STRING)                            \
   do{                                                                  \
      if (( *(JNIENV) )->ExceptionCheck((JNIENV)) ) {                   \
         fprintf(stderr, "ERROR: Pending Java exception (%s)\n", (STRING)); \
         jasper_DescribeException((JNIENV));                            \
         /* SP_ASSERT(0); */                                            \
      }                                                                 \
   } while (0)

#if JASPER_DBG
/* Duplicated in jasper.c */
/* Does (*jnienv)->ExceptionDescribe(jnienv) but does *not* clear the pending exception */
static void jasper_DescribeException(JNIEnv *jnienv)
{
  jthrowable pending = NULL;

  pending = (*jnienv)->ExceptionOccurred(jnienv);
  if (pending)
    {
      fprintf(stderr, "spnative::jasper_DescribeException: ");
      (*jnienv)->ExceptionDescribe(jnienv); /* clears it  */
      (*jnienv)->Throw(jnienv, pending); /* restores it */
    }

  if (pending)
    {
      (*jnienv)->DeleteLocalRef(jnienv, pending);
    }
}
#endif /* JASPER_DBG */

#if 0
#if JASPER_DBG
/* [PM] 3.9.2b3 debug */
static jboolean jasper_DescribeClass(JNIEnv *jnienv, jobject jobj)
{
  jclass clazz = NULL;
  jclass clazzclazz = NULL;
  jstring jname = NULL;
  jmethodID mid;
  char const *className = NULL;
  jboolean rc;
  int b=0;

  if (jobj == NULL) goto barf1;

  clazz = (*jnienv)->GetObjectClass(jnienv, jobj); if (!clazz) goto barf2;
  clazzclazz = (*jnienv)->GetObjectClass(jnienv, clazz); if (!clazzclazz) goto barf3;
  mid = (*jnienv)->GetMethodID(jnienv,clazzclazz,"getName","()Ljava/lang/String;");   if (!mid) goto barf4;
  jname = (jstring)(*jnienv)->CallObjectMethod(jnienv, clazz, mid); if (!jname) goto barf5;
  className = (char *)(*jnienv)->GetStringUTFChars(jnienv,jname,NULL); if (!className) goto barf6;

  #if JASPER_DBG
  fprintf(stderr, "jasper_DescribeClass: \"%s\"\n", className);fflush(stderr);
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
  fprintf(stderr, "ERROR: jasper_DescribeClass could not determine class (reason #%d)\n", (int)b);fflush(stderr);
  #endif
  rc = JNI_FALSE;
  goto cleanup;
}
#endif /* JASPER_DBG */
#endif  /* 0 */


#if JASPER_DBG


#define ASSERT_NO_EXCP(JNIENV, STRING) ASSERT_NO_EXCP_FORCE((JNIENV), (STRING))
#define DBG_VALIDATE_TERM_REF(TERMREF) do{ \
   SP_term_ref HIGH_WATER_MARK__ = SP_new_term_refs(0); \
   SP_term_ref TERM_REF__ = (SP_term_ref)(TERMREF); \
   if (! (0 < TERM_REF__ && TERM_REF__ < HIGH_WATER_MARK__) ) { \
      fprintf(stderr, "ERROR: Illegal term-ref %d\n" __FILE__ ":" JASPER_STRINGISIZE(__LINE__) "\n",TERM_REF__); \
   } \
   /* else fprintf(stderr, "OK: term-ref %d in [1,%d]\n" __FILE__ ":" JASPER_STRINGISIZE(__LINE__) "\n",TERM_REF__,HIGH_WATER_MARK__); */\
}while(0)

#else /* no JASPER_DBG */
#define ASSERT_NO_EXCP(JNIENV, STRING)
#define DBG_VALIDATE_TERM_REF(TERMREF)
#endif



/* The strange bracketing is to ensure that ENTER is put immediately
   after local variable declaration and LEAVE immedately before return
*/ 

/* This is where SP_put_list et al find the dispatch table, it is
   extracted from the SICStus object (named spobj, available in the
   local environment).
*/

#undef SICStusDISPATCHVAR /* Undefine the default from spaux.h */
#define SICStusDISPATCHVAR SP_ONLY_API_ARG_NAME

/* [PM] 3.9b4 renamed from SPAPI_XXX to SP_ONLY_API_XXX since the
   former will expand to two args (api-dispatch and stash) for
   multi-sp-aware foreign functions (Later in 3.9b4 no longer true,
   stash is now part of api-dispatch). Since it does not make sense to
   pass around a stash in an embedder we need a separate set of macros
*/
#define SP_ONLY_API_ARG_NAME sp_api_dispatch
#define SP_ONLY_API_ARG0 SP_ONLY_API_ARG_NAME
#define SP_ONLY_API_ARG SP_ONLY_API_ARG0, 
#define SP_ONLY_API_ARG_PROTO_DECL0 SICSTUS_API_STRUCT_TYPE *SP_ONLY_API_ARG_NAME
#define SP_ONLY_API_ARG_PROTO_DECL SP_ONLY_API_ARG_PROTO_DECL0, 

#define SP_ENTER_SICSTUS1(SPAPI) { int jasper_enter_sicstus_monitor_dummy = 42; \
   SP_ONLY_API_ARG_PROTO_DECL0; \
   SP_ONLY_API_ARG_NAME = (SPAPI);

#define SP_ENTER_SICSTUS() SP_ENTER_SICSTUS1(api_from_spobj(jnienv, spobj));

#define SP_LEAVE_SICSTUS() jasper_enter_sicstus_monitor_dummy++; }

/* We probably want a faster way to do this. Consider passing it as an argument or accessing it through a member */
static SICSTUS_API_STRUCT_TYPE *api_from_spobj(JNIEnv *jnienv, jobject spobj)
{
  jboolean hasException = JNI_FALSE;
  jvalue result;
  SICSTUS_API_STRUCT_TYPE *api = NULL;
  
  ASSERT_NO_EXCP(jnienv,"api_from_spobj 1");

  result.j = 0;

  result = CallMethodByName(jnienv, &hasException, spobj, "getSPAPI", "()J");
  ASSERT_NO_EXCP(jnienv,"api_from_spobj 1");

  if (hasException) goto barf;
  if (result.j == 0) goto barf;
  if (result.j == (jlong)-1)
    {

#if JASPER_DBG
  fprintf(stderr, "ERROR: Could not extract SP api from SICStus object 0x%" SPRIxINTEGER " (was -1)\n", (SP_uinteger)spobj);
#endif  /* JASPER_DBG */

      goto reported_barf;
    }

  api = (SICSTUS_API_STRUCT_TYPE *)
    (SP_integer)                      /* extra cast to avoid warning about size difference ptr vs jlong */
    result.j;
  return api;

 barf:
#if JASPER_DBG
  fprintf(stderr, "ERROR: Could not extract SP api from SICStus object 0x%" SPRIxINTEGER "\n", (SP_uinteger)spobj);
#endif  /* JASPER_DBG */

#if SICSTUS_TODO /* [PM] FIXME: fix later (can this happen?) */
#error "Need a way to handle the case that there is no API in the SICStus object"
#endif /* SICSTUS_TODO */

 reported_barf:
  return NULL;
}

static int set_spobj_api(JNIEnv *jnienv, jobject spobj, SICSTUS_API_STRUCT_TYPE *api)
{
  jboolean hasException = JNI_FALSE;
  jvalue result;
  jlong api_long = (jlong)
    (SP_integer)                      /* extra cast to avoid warning about size difference ptr vs jlong */
    api;
  
  ASSERT_NO_EXCP(jnienv,"set_spobj_api 1");

  result = CallMethodByName(jnienv, &hasException, spobj, "setSPAPI", "(J)I", api_long);
  ASSERT_NO_EXCP(jnienv,"set_spobj_api 1");

  if (hasException) goto barf;

  return result.i;

 barf:
#if JASPER_DBG
  fprintf(stderr, "ERROR: Could not set SP api for SICStus object 0x%" SPRIxINTEGER "\n", (SP_uinteger)spobj);
#endif
  return SP_ERROR;
}

/* [PM] 3.9 why returning jlong? (or rather why are most 'res' of type jlong?)  */
static jlong handleExceptions(SP_ONLY_API_ARG_PROTO_DECL JNIEnv *jnienv, int query_rval);

static jint throwNCE(jobject spobj, JNIEnv *jnienv)
{
  jclass nceClass;

  nceClass = (*jnienv)->FindClass(jnienv, "se/sics/jasper/NativeCodeException");
  if (nceClass == NULL) goto error;

  /* 3.8.5 pass the sp object */
  {
    jmethodID mid;
    jobject nceObject;
    mid = (*jnienv)->GetMethodID(jnienv,nceClass,"<init>","(Lse/sics/jasper/SICStus;)V");
    if (mid == NULL) goto error;

    nceObject = (*jnienv)->NewObject(jnienv, nceClass, mid, spobj, NULL);
    if (nceObject == NULL) goto error;
    (*jnienv)->Throw(jnienv, nceObject);
  }
 error:                         /* When we get here some Java excpetion has been thrown */
  return 0;
}

/* Original is in jasper.c, keep in synch */
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
          result.l = 0; /* [PM] 4.2.1 prevent compiler warning */
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
          result.l = 0; /* [PM] 4.2.1 prevent compiler warning */
        }
        va_end(args);
      } else {
        result.l = 0; /* [PM] 4.2.1 prevent compiler warning */        
      }
      (*env)->DeleteLocalRef(env, clazz);
    }
  else
    {
      result.l = 0; /* [PM] 4.2.1 prevent compiler warning */
    }
  if (hasException)
    {
      *hasException = (*env)->ExceptionCheck(env);
    }
  return result;
}

/* Original is in jasper.c, keep in synch */
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

#if THREADSERVER      /* [PD] 3.9 threadserver hack */

/* [PD] 3.9 Make jasper aware that it should switch to thread server mode. */
/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spSetThreadServerMode
 * Signature: (Z)V
 */
JNIEXPORT jboolean SP_JNICALL
  Java_se_sics_jasper_SICStus_spSetThreadServerMode (JNIEnv *jnienv,
                                                     jobject spobj,
                                                     jboolean on)
{
  jboolean res;
#if 0                           /* [PD] 3.9 old code loading library(jasper). */
  SP_pred_ref pred;
  SP_term_ref flag;
  SP_ENTER_SICSTUS();

  pred = SP_predicate("jasperi_set_threadservermode", 1, "jasper");
  if (!pred) goto barf;
  flag = SP_new_term_ref();
  if (!SP_put_integer(flag, on)) goto barf;
  if (SP_SUCCESS != SP_query_cut_fail(pred, flag)) goto barf;
  res = JNI_TRUE;

 cleanup:
  SP_LEAVE_SICSTUS();
  return res;

 barf:
  res = JNI_FALSE;
  goto cleanup;
#else                           /* [PD] 3.9 not loading library(jasper). */
  SP_ENTER_SICSTUS();

  (void) *jnienv;               /* -Wunused */
  (void) spobj;                 /* -Wunused */

  res = ( sp_set_jasper_threadservermode((int)on, SICSTUS_VERSION)
          ? JNI_TRUE
          : JNI_FALSE );
  SP_LEAVE_SICSTUS();
  return res;
#endif
}

#if 0
static jboolean setup_method_arguments(JNIEnv *jnienv,
/*                                       jobject spobj,    */
/*                                       jstring methname, */
                                       jobjectArray args,
                                       jstring typesig,
                                       jvalue *argarray)
{
  const char *tsig = NULL;
  jsize numargs;
  jsize i;
  char *tp;

  numargs = (*jnienv)->GetArrayLength(jnienv, args);
  if (!(tsig = (*jnienv)->GetStringUTFChars(jnienv,typesig,NULL))) {goto barf;}
  tp = (char *)tsig;

  while (*tp != '(') tp++;      /* skip to arguments */
  tp++;                         /* skip to first argument type */
  
  ASSERT_NO_EXCP(jnienv, "setup_method_arguments (1)");

  for (i = 0; i < numargs; i++)
    {
      jobject arg = (*jnienv)->GetObjectArrayElement(jnienv, args, i);
      jclass wclazz = NULL;
      jmethodID wmid = NULL;

      /* If the argument is NULL we don't need its class, but it has
         to be a real object (or array), i.e. not a simple type wrapped
         in an object.
      */
      if (arg != NULL) {
        wclazz = (*jnienv)->GetObjectClass(jnienv, arg);
      } else {
        if (*tp != 'L' && *tp != '[') {
          /* [PD] 3.10.2 setup_method_arguments will be called by the
             client thread in MT-Jasper, so throwNCE cannot be called here. */
          /* throwNCE(spobj, jnienv); */
          goto barf;
        }
      }

      switch (*tp) {
      case 'Z':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "booleanValue", "()Z"))) { goto barf; }
        argarray[i].z = (*jnienv)->CallBooleanMethod(jnienv, arg, wmid);
        break;
      case 'B':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "byteValue", "()B"))) { goto barf; }
        argarray[i].b = (*jnienv)->CallByteMethod(jnienv, arg, wmid);
        break;
      case 'C':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "charValue", "()C"))) { goto barf; }
        argarray[i].c = (*jnienv)->CallCharMethod(jnienv, arg, wmid);
        break;
      case 'S':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "shortValue", "()S"))) { goto barf; }
        argarray[i].s = (*jnienv)->CallShortMethod(jnienv, arg, wmid);
        break;
      case 'I':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "intValue", "()I"))) { goto barf; }
        argarray[i].i = (*jnienv)->CallIntMethod(jnienv, arg, wmid);
        break;
      case 'J':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "longValue", "()J"))) { goto barf; }
        argarray[i].j = (*jnienv)->CallLongMethod(jnienv, arg, wmid);
        break;
      case 'F':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "floatValue", "()F"))) { goto barf; }
        argarray[i].f = (*jnienv)->CallFloatMethod(jnienv, arg, wmid);
        break;
      case 'D':
        if (!(wmid = (*jnienv)->GetMethodID(jnienv, wclazz, "doubleValue", "()D"))) { goto barf; }
        argarray[i].d = (*jnienv)->CallDoubleMethod(jnienv, arg, wmid);
        break;
      case 'L':
        argarray[i].l = arg;
        while (*tp != ';') tp++; /* skip past this object type */
        break;
      case '[':
        argarray[i].l = arg;
        if (*(tp + 1) == 'L' ) {
          while (*tp != ';') { tp++; } /* skip past this object type */
        }
        break;
      }
      tp++;                     /* skip to next argument type */
    }
#if 0                           /* [PD] 3.9.2 probably bogus */
  argarray[i+1].l = NULL;
#endif

  ASSERT_NO_EXCP(jnienv, "setup_method_arguments (2)");
  return JNI_TRUE;

 barf:
  if (tsig != NULL) { (*jnienv)->ReleaseStringUTFChars(jnienv, typesig, tsig); }
  return JNI_FALSE;
}
#endif  /* 0 */

#if 0
static jmethodID sp_get_methodID(JNIEnv *jnienv,
                                 jobject obj,
                                 jstring methname,
                                 jstring typesig,
                                 jboolean staticP)
{
  ASSERT_NO_EXCP(jnienv, "sp_get_methodID (0)");
{
  const char *mname = (*jnienv)->GetStringUTFChars(jnienv, methname, NULL);
  const char *tsig = (*jnienv)->GetStringUTFChars(jnienv, typesig, NULL);
  jmethodID mid;

  if (staticP) {
    /* obj is a jclazz pointer */
    mid = (*jnienv)->GetStaticMethodID(jnienv, obj, mname, tsig);
  } else {
    jclass clazz = NULL;
    clazz = (*jnienv)->GetObjectClass(jnienv, obj);
    ASSERT_NO_EXCP(jnienv, "sp_get_methodID (1)");
    mid = (*jnienv)->GetMethodID(jnienv, clazz, mname, tsig);
    #if JASPER_DBG
    if (!mid)
      {
        fprintf(stderr, "ERROR: sp_get_methodID(\"%s\", \"%s\", %p class==", (mname ? mname : "NULL??"), (tsig ? tsig : "NULL???"), obj);
        jasper_DescribeClass(jnienv, obj);
        fprintf(stderr, ", %s)\n", (staticP ? "static" : "instance"));fflush(stderr);
      }
    #endif

    if (NULL != clazz) { (*jnienv)->DeleteLocalRef(jnienv, clazz); }
  }
#if JASPER_DBG
  if (!mid)
    {
      fprintf(stderr, "ERROR: sp_get_methodID(\"%s\", \"%s\", %s) method not found\n", (mname ? mname : "NULL??"), (tsig ? tsig : "NULL???"), (staticP ? "static" : "instance"));fflush(stderr);
    }
#endif

  ASSERT_NO_EXCP(jnienv, "sp_get_methodID (2)");
  if (NULL != mname) { (*jnienv)->ReleaseStringUTFChars(jnienv, methname, mname); }
  if (NULL != tsig) { (*jnienv)->ReleaseStringUTFChars(jnienv, typesig, tsig); }

  return mid;
}
}
#endif  /* 0 */

#if 0
static jmethodID sp_get_init_methodID(JNIEnv *jnienv,
                                      jclass clazz,
                                      jstring methname,
                                      jstring typesig)
{
  const char *mname = (*jnienv)->GetStringUTFChars(jnienv, methname, NULL);
  const char *tsig = (*jnienv)->GetStringUTFChars(jnienv, typesig, NULL);
  jmethodID mid;
  
#if JASPER_DBG
  JASPER_DBGLINE(sp_get_init_methodID);
  jasper_DescribeClass(jnienv, clazz);
  fprintf(stderr, "sp_get_init_methodID: mname==%s, tsig==%s\n", mname, tsig);
#endif
  mid = (*jnienv)->GetMethodID(jnienv, clazz, mname, tsig);

  ASSERT_NO_EXCP(jnienv, "sp_get_init_methodID");
  if (NULL != mname) { (*jnienv)->ReleaseStringUTFChars(jnienv, methname, mname); }
  if (NULL != tsig) { (*jnienv)->ReleaseStringUTFChars(jnienv, typesig, tsig); }

  return mid;
}
#endif  /* 0 */

#if 0                           /* [PD] 4.0.5 No longer used */
/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spNewObject
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;
 */
JNIEXPORT jobject SP_JNICALL
  Java_se_sics_jasper_SICStus_spNewObject(JNIEnv *jnienv,
                                          jobject spobj,
                                          jobject clazz,
                                          jstring methname,
                                          jobjectArray args,
                                          jstring typesig)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_init_methodID(jnienv, clazz, methname, typesig);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
    return (*jnienv)->NewObjectA(jnienv, clazz, mid, argarray);
  } else {
    return (jobject)NULL;
  }
}

/*
 * Class:     jasper_SICStus
 * Method:    spCallVoidMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)V
 */
JNIEXPORT void SP_JNICALL
  Java_se_sics_jasper_SICStus_spCallVoidMethodByName(JNIEnv *jnienv,
                                                    jobject spobj,
                                                    jobject obj,
                                                    jstring methname,
                                                    jobjectArray args,
                                                    jstring typesig,
                                                    jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  JASPER_DBGLINE(spCallVoidMethodByName);
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
    if (staticP) 
      (*jnienv)->CallStaticVoidMethodA(jnienv, obj, mid, argarray);
    else
      (*jnienv)->CallVoidMethodA(jnienv, obj, mid, argarray);
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallObjectMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)Ljava/lang/Object;
 */
JNIEXPORT jobject SP_JNICALL
  Java_se_sics_jasper_SICStus_spCallObjectMethodByName(JNIEnv *jnienv,
                                                      jobject spobj,
                                                      jobject obj,
                                                      jstring methname,
                                                      jobjectArray args,
                                                      jstring typesig,
                                                      jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  JASPER_DBGLINE(spCallObjectMethodByName);
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
    JASPER_DBGLINE(spCallObjectMethodByName);
    return ( staticP
             ? (*jnienv)->CallStaticObjectMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallObjectMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jobject)NULL;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallBooleanMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)Z
 */
JNIEXPORT jboolean SP_JNICALL
Java_se_sics_jasper_SICStus_spCallBooleanMethodByName(JNIEnv *jnienv,
                                                     jobject spobj,
                                                     jobject obj,
                                                     jstring methname,
                                                     jobjectArray args,
                                                     jstring typesig,
                                                     jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
    return ( staticP
             ? (*jnienv)->CallStaticBooleanMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallBooleanMethodA(jnienv, obj, mid, argarray));
  } else {
    return JNI_FALSE;           /* Well, do you have a better suggestion?  */
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallByteMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)B
 */
JNIEXPORT jbyte SP_JNICALL
Java_se_sics_jasper_SICStus_spCallByteMethodByName(JNIEnv *jnienv,
                                                  jobject spobj,
                                                  jobject obj,
                                                  jstring methname,
                                                  jobjectArray args,
                                                  jstring typesig,
                                                  jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
    return ( staticP
             ? (*jnienv)->CallStaticByteMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallByteMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jbyte)0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallCharMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)C
 */
JNIEXPORT jchar SP_JNICALL
Java_se_sics_jasper_SICStus_spCallCharMethodByName(JNIEnv *jnienv,
                                                  jobject spobj,
                                                  jobject obj,
                                                  jstring methname,
                                                  jobjectArray args,
                                                  jstring typesig,
                                                  jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
    return ( staticP
             ? (*jnienv)->CallStaticCharMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallCharMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jchar)0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallShortMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)S
 */
JNIEXPORT jshort SP_JNICALL
Java_se_sics_jasper_SICStus_spCallShortMethodByName(JNIEnv *jnienv,
                                                   jobject spobj,
                                                   jobject obj,
                                                   jstring methname,
                                                   jobjectArray args,
                                                   jstring typesig,
                                                   jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
    return ( staticP
             ? (*jnienv)->CallStaticShortMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallShortMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jshort)0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallIntMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)I
 */
JNIEXPORT jint SP_JNICALL
Java_se_sics_jasper_SICStus_spCallIntMethodByName(JNIEnv *jnienv,
                                                 jobject spobj,
                                                 jobject obj,
                                                 jstring methname,
                                                 jobjectArray args,
                                                 jstring typesig,
                                                 jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
#if !JASPER_DBG
    return
#else
      { int retval =
#endif
          ( staticP
            ? (*jnienv)->CallStaticIntMethodA(jnienv, obj, mid, argarray)
            : (*jnienv)->CallIntMethodA(jnienv, obj, mid, argarray));
#if JASPER_DBG
      fprintf(stderr, "spCallIntMethodByName: retval==%d\n", retval);
      return retval;
      }
#endif
  } else {
    return (jint)0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallLongMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)J
 */
JNIEXPORT jlong SP_JNICALL
Java_se_sics_jasper_SICStus_spCallLongMethodByName(JNIEnv *jnienv,
                                                  jobject spobj,
                                                  jobject obj,
                                                  jstring methname,
                                                  jobjectArray args,
                                                  jstring typesig,
                                                  jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
    return ( staticP
             ? (*jnienv)->CallStaticLongMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallLongMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jlong)0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallFloatMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)F
 */
JNIEXPORT jfloat SP_JNICALL
Java_se_sics_jasper_SICStus_spCallFloatMethodByName(JNIEnv *jnienv,
                                                   jobject spobj,
                                                   jobject obj,
                                                   jstring methname,
                                                   jobjectArray args,
                                                   jstring typesig,
                                                   jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
    return ( staticP
             ? (*jnienv)->CallStaticFloatMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallFloatMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jfloat)0.0;
  }
}

/*
 * Class:     se_sics_jasper_SICStus
 * Method:    spCallDoubleMethodByName
 * Signature: (Ljava/lang/Object;Ljava/lang/String;[Ljava/lang/Object;Ljava/lang/String;)D
 */
JNIEXPORT jdouble SP_JNICALL
Java_se_sics_jasper_SICStus_spCallDoubleMethodByName(JNIEnv *jnienv,
                                                    jobject spobj,
                                                    jobject obj,
                                                    jstring methname,
                                                    jobjectArray args,
                                                    jstring typesig,
                                                    jboolean staticP)
{
  jvalue argarray[255];
  jmethodID mid = sp_get_methodID(jnienv, obj, methname, typesig, staticP);
  (void)spobj;                /* [PD] 3.10.2 avoid "unused" warning */
  if (mid != NULL && setup_method_arguments(jnienv, /*spobj,*/ /*methname,*/ args, typesig, argarray)) {
    return ( staticP
             ? (*jnienv)->CallStaticDoubleMethodA(jnienv, obj, mid, argarray)
             : (*jnienv)->CallDoubleMethodA(jnienv, obj, mid, argarray));
  } else {
    return (jdouble)0.0;
  }
}
#endif /* [PD] 4.0.5 No longer used */
#endif /* THREADSERVER */

#ifndef JASPER_SP_INTEGER_BYTES
#ifdef SP_put_integer_bytes
#define JASPER_SP_INTEGER_BYTES 1
#endif /* SP_put_integer_bytes */
#endif /* JASPER_SP_INTEGER_BYTES */


/* [PM] 3.9.1 Use put_jlong/get_jlong for all pointers, jlongs etc.
 * Uses the new (3.9.1) bignum API
 */
static int put_jlong(SP_ONLY_API_ARG_PROTO_DECL SP_term_ref tr, jlong x)
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

/* A copy (except for SP_ONLY_API_ARG_PROTO_DECL) of this is in jasper.c.c keep in synch! */
static int get_jlong(SP_ONLY_API_ARG_PROTO_DECL SP_term_ref tr, jlong *px, int allow_float)
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
            rc = 0; /* can not fail */
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
                  /* [PM] 3.9.1 see jasper.c for the rationale for this kludge */
                  #if SICSTUS_REVISION_VERSION>2 /* [PM] 3.9.2 fix later */
                  #error "Need a way to manually convert from double to int64"
                  #else/* convert to long, and give up otherwise */
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



/*
 * Class:     jasper_SPTerm
 * Method:    spPutString
 * Signature: (Ljasper/SICStus;JLjava/lang/String;)V
 */

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spPutString(JNIEnv *jnienv, jobject spobj, jlong termref, jstring value)
{
  int rc;
  char *s = NULL;

  SP_ENTER_SICSTUS();
  
  DBG_VALIDATE_TERM_REF(termref);

  s = (char *)(*jnienv)->GetStringUTFChars(jnienv, value, NULL);

  if (s != NULL)
    {
      rc = SP_put_string((SP_term_ref)termref, s);
      (*jnienv)->ReleaseStringUTFChars(jnienv, value, s);
    }
  else
    {
      rc = 0;
    }

  if (!rc)
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spPutListChars(JNIEnv *jnienv, jobject spobj, jlong termref, jstring value)
{
  int rc;
  char *s = NULL;
  SP_term_ref emptylist;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  emptylist = SP_new_term_ref();
  
  s = (char *)(*jnienv)->GetStringUTFChars(jnienv, value, NULL);
  if (s != NULL)
    {
      rc = SP_put_list_codes((SP_term_ref)termref, emptylist, s);
      (*jnienv)->ReleaseStringUTFChars(jnienv, value, s);
    }
  else
    {
      rc = 0;
    }
  SP_reset_term_refs(emptylist);

  if (!rc)
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spPutNumberChars(JNIEnv *jnienv, jobject spobj, jlong termref, jstring value)
{
  int rc;
  char *s = NULL;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  s = (char *)(*jnienv)->GetStringUTFChars(jnienv, value, NULL);

  if (s != NULL)
    {
      rc = SP_put_number_codes((SP_term_ref)termref, s);
      (*jnienv)->ReleaseStringUTFChars(jnienv, value, s);
    }
  else
    {
      rc = 0;
    }

  if (!rc)
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS()
}


JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spPutAtom(JNIEnv *jnienv, jobject spobj, jlong termref, jlong canonical)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_atom((SP_term_ref)termref, (SP_uinteger)canonical))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}


/*
 * Class:     jasper_SPTerm
 * Method:    spGetString
 * Signature: (Ljasper/SICStus;J)Ljava/lang/String;
 */
JNIEXPORT jstring SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetString(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  char const *name;
  jstring res = NULL;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  
  if (0 == SP_get_string((SP_term_ref)termref, &name))
    {
      throwNCE(spobj, jnienv);
      goto cleanup;
    }
  else
    {
      res = (*jnienv)->NewStringUTF(jnienv, name);
    }

 cleanup:
  SP_LEAVE_SICSTUS();
  return res;
}

/*
 * Class:     jasper_SPTerm
 * Method:    spGetAtom
 * Signature: (Ljasper/SICStus;J)J
 */

JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetAtom(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  SP_uinteger atmindex;
  jlong res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);  
  if (0 == SP_get_atom((SP_term_ref)termref, &atmindex))
    {
      res = throwNCE(spobj, jnienv);
      goto cleanup;
    }
  res = (jlong)atmindex;

 cleanup:
  SP_LEAVE_SICSTUS();
  return res;
}

/*
 * Class:     jasper_SPTerm
 * Method:    spConsFunctor
 * Signature: (Ljasper/SICStus;JJI)I
 */
JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spConsFunctor(JNIEnv *jnienv, jobject spobj,
                                         jlong termref, jlong atmindex, jlongArray args)
{
  jsize numargs;
  int i;
  SP_term_ref *spargs = NULL;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  numargs = (*jnienv)->GetArrayLength(jnienv, args);
  spargs = (SP_term_ref *)SP_malloc(sizeof(SP_term_ref)*numargs);
  if (spargs == NULL) goto out_of_memory;

  for (i = 0; i < numargs; i++)
    {
      jlong sparg;

      (*jnienv)->GetLongArrayRegion(jnienv, args, (jsize)i, 1, &sparg);
     
      spargs[i] = (SP_term_ref)sparg;
    }

  if (0 == SP_cons_functor_array((SP_term_ref)termref, (SP_uinteger)atmindex, (int)numargs, spargs))
    {
      goto barf;
    }
  goto cleanup;

 out_of_memory:
  goto barf;
 barf:
  throwNCE(spobj, jnienv);
  goto cleanup;

 cleanup:
  if (spargs) SP_free(spargs);  /* [PM] 3.8.7 plug leak SPRM 2327 */

  SP_LEAVE_SICSTUS();
}


JNIEXPORT jstring SP_JNICALL 
Java_se_sics_jasper_SICStus_spErrorMessage(JNIEnv *jnienv, jobject spobj, jint eno)
{
  jstring res;
  SP_ENTER_SICSTUS();
#if JASPER_DBG
  fprintf(stderr, "Java_se_sics_jasper_SPException_spErrorMessage eno==%d (this is expected)\n", (int) eno);
#endif
  res = (*jnienv)->NewStringUTF(jnienv, SP_error_message((int)eno));
#if JASPER_DBG
  fprintf(stderr, "Java_se_sics_jasper_SPException_spErrorMessage eno==%d, msg==\"%s\" (this is expected)\n",
          (int) eno, SP_error_message((int)eno));
#endif

  SP_LEAVE_SICSTUS();
  return res;
}

JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetErrno(JNIEnv *jnienv, jobject spobj)
{
  jint res;
  SP_ENTER_SICSTUS();
  res = (jint)SP_errno;
  SP_LEAVE_SICSTUS();
  return res;
}

/*
 * Class:     jasper_SPTerm
 * Method:    spGetFloat
 * Signature: (Ljasper/SICStus;J)D
 */
JNIEXPORT jdouble SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetFloat(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  double res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);  
  if (0 == SP_get_float((SP_term_ref)termref, &res))
    {
      res = throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
  return res;
}

/*
 * Class:     jasper_SPTerm
 * Method:    spGetObject
 * Signature: (Ljasper/SICStus;J)Ljava/lang/Object;
 */
JNIEXPORT jobject SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetObject(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jlong lobj;
  SP_term_ref tr;
  SP_term_ref objref;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  tr = (SP_term_ref)termref;
  objref = SP_new_term_ref();

  if (! (SP_get_arg(1,tr,objref) &&
         get_jlong(SP_ONLY_API_ARG objref,&lobj, FALSE)) )
    {
      throwNCE(spobj, jnienv);
      lobj = 0;
    }
  SP_reset_term_refs(objref);

  SP_LEAVE_SICSTUS();
  /* [PM] 3.9.1 avoid "warning: cast to pointer from integer of different size" */
  return (jobject) (SP_integer) lobj;
}

/*
 * Class:     jasper_SPTerm
 * Method:    spPutFloat
 * Signature: (Ljasper/SICStus;JD)I
 */
JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spPutFloat(JNIEnv *jnienv, jobject spobj, jlong termref, jdouble value)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_float((SP_term_ref)termref, (double)value))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetInteger(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jlong l;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == get_jlong(SP_ONLY_API_ARG (SP_term_ref)termref, &l, TRUE)) /* allow float */
    {
      l = (jlong) throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
  return l;
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spPutInteger(JNIEnv *jnienv, jobject spobj, jlong termref, jlong val)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  /* [PM] 3.9.1 On 64bit platforms val may be a full 64bit quantity (from SPTerm.putObject).
  */
  if (0 == put_jlong(SP_ONLY_API_ARG (SP_term_ref)termref, val))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spCreateGlobalRef(JNIEnv *jnienv, jobject spobj, jobject obj)
{
  jlong globref;
  (void) spobj;                 /* -Wunused */
  /* Does not need to call SP_ENTER/LEAVE_SICSTUS */

  /* [PM] 3.9.1 avoid "warning: cast to pointer from integer of different size" */
  globref = (jlong)(SP_integer)(*jnienv)->NewGlobalRef(jnienv, obj);

  return globref;
}

/*
 * Class:     jasper_SPQuery
 * Method:    spNextSolution
 * Signature: ()I
 */
JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spNextSolution(JNIEnv *jnienv, jobject spobj, /* jobject query, */ jlong qidref)
{
  jint res;

  SP_ENTER_SICSTUS();

  res = (jint) handleExceptions(SP_ONLY_API_ARG jnienv, SP_next_solution((SP_qid)qidref));

  SP_LEAVE_SICSTUS();
  return res;
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spCutQuery(JNIEnv *jnienv, jobject spobj, /* jobject query, */jlong qidref)
{
  SP_ENTER_SICSTUS();
  if (SP_ERROR == SP_cut_query((SP_qid)qidref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spCloseQuery(JNIEnv *jnienv, jobject spobj, /* jobject query, */ jlong qidref)
{

  SP_ENTER_SICSTUS();
  if (SP_ERROR == SP_close_query((SP_qid)qidref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}


/* [PD] 3.10.2 Obsolete. */
#if 0
JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spExceptionTerm(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  int r;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  r = SP_exception_term((SP_term_ref)termref);

#if 0
  fprintf(stderr, "got exception term: ");
  SP_query(SP_predicate("writeq",1,""), termref);
  fprintf(stderr, "\n");
#endif

  SP_LEAVE_SICSTUS();

  return r;
}
#endif

/* [PD] 3.10.2 Create a term of the form
               prolog:'$SPException'(<index>,<exception_term) */
static SP_term_ref make_SPException_term(SP_ONLY_API_ARG_PROTO_DECL int index, SP_term_ref tr)
{
  SP_term_ref index_term_ref, spex_term_ref, colon_term_ref, module_term_ref;
  SP_atom spex_atom, colon_atom, module_atom;

  index_term_ref = SP_new_term_ref();
  if (! SP_put_integer(index_term_ref, index)) goto barf;
  if (! (spex_atom = SP_atom_from_string("$SPException"))) goto barf;
  spex_term_ref = SP_new_term_ref();
  if (! SP_cons_functor(spex_term_ref, spex_atom, 2, index_term_ref, tr))
    goto barf;
  if (! (colon_atom = SP_atom_from_string(":"))) goto barf;
  if (! (module_atom = SP_atom_from_string("prolog"))) goto barf;
  module_term_ref = SP_new_term_ref();
  if (! SP_put_atom(module_term_ref, module_atom)) goto barf;
  colon_term_ref = SP_new_term_ref();
  if (! SP_cons_functor(colon_term_ref, colon_atom, 2, module_term_ref, spex_term_ref))
    goto barf;
  return colon_term_ref;

 barf: return 0;  
}

JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spExceptionIndex(JNIEnv *jnienv, jobject spobj, jint next_index)
{
  int r;
  int mark;
  SP_term_ref exception_term_ref, ass_term_ref;
  SP_pred_ref assertPred;

  SP_ENTER_SICSTUS();
  mark = SP_new_term_refs(0);
  exception_term_ref = SP_new_term_ref();

  r = SP_exception_term(exception_term_ref);
  if (r == 0) goto barf;

  if (! (ass_term_ref = make_SPException_term(SP_ONLY_API_ARG next_index,
                                              exception_term_ref))) goto barf;
  if (NULL == (assertPred = SP_predicate("assert", 1, ""))) goto barf;
  /* assert(prolog:'SPException'(<assertIndex>,<exceptionTerm>). */
  if (SP_SUCCESS != SP_query_cut_fail(assertPred, ass_term_ref)) goto barf;
  r = next_index;

 cleanup:
  SP_reset_term_refs(mark);
  SP_LEAVE_SICSTUS();
  return r;

 barf:
  r = 0;
  goto cleanup;
}

JNIEXPORT jboolean SP_JNICALL 
Java_se_sics_jasper_SICStus_spExceptionString(JNIEnv *jnienv, jobject spobj, jint assert_index, jlong stream_code, jint print_depth)
{
  int mark;
  jboolean res;
  SP_pred_ref call_pred;

  SP_ENTER_SICSTUS();
  mark = SP_new_term_refs(0);
  {
    SP_term_ref options = SP_new_term_ref();
    SP_term_ref depth = SP_new_term_ref();
    SP_term_ref opt_vals[2];    /* = {depth, 0 } */
    SP_term_ref goal = SP_new_term_ref();
    SP_term_ref stream = SP_new_term_ref();
    SP_term_ref streamCode = SP_new_term_ref();
    SP_term_ref index = SP_new_term_ref();
    SP_term_ref term = SP_new_term_ref();
    /* Note: The order of the term refs in the array vals must correspond to
       the order of the variables in the goal string below. */
    SP_term_ref vals[6]; /*  = {stream, streamCode, index, term, options, 0}; */

    /* [PM] 3.11.0 Sun compiles are picky about non-constant array initializers */
    vals[0]=stream;
    vals[1]=streamCode;
    vals[2]=index;
    vals[3]=term;
    vals[4]=options;
    vals[5]=0;

    opt_vals[0] = depth;
    opt_vals[1] = 0;

    if (! SP_put_variable(stream)) goto barf;
    if (! SP_put_integer(streamCode, (SP_integer)stream_code)) goto barf;
    if (! SP_put_integer(index, assert_index)) goto barf;
    if (! SP_put_variable(term)) goto barf;
    if (! SP_put_integer(depth, print_depth)) goto barf;
    if (! SP_read_from_string(options, "[max_depth(Depth)].", opt_vals))
      goto barf;
    if (! SP_read_from_string(goal,
                              "prolog:stream_code(Stream, StreamCode), prolog:'$SPException'(Index, Term), prolog:write_term(Stream, Term, Options).",
                              vals)) goto barf;
    if (NULL == (call_pred = SP_predicate("call", 1, "prolog"))) goto barf;
    if (SP_SUCCESS != SP_query_cut_fail(call_pred, goal)) goto barf;
    res = JNI_TRUE;
  }

 cleanup:
  SP_reset_term_refs(mark);
  SP_LEAVE_SICSTUS();
  return res;

 barf:
  res = JNI_FALSE;
  goto cleanup;
}

JNIEXPORT jboolean SP_JNICALL 
Java_se_sics_jasper_SICStus_spRetractException(JNIEnv *jnienv, jobject spobj, jint index)
{
  jboolean res;
  int mark;
  SP_term_ref exception_term_ref, var_term_ref;
  SP_pred_ref retractPred;

  SP_ENTER_SICSTUS();
  mark = SP_new_term_refs(0);
  var_term_ref = SP_new_term_ref();
  if (! SP_put_variable(var_term_ref)) goto barf;  
  if (! (exception_term_ref = make_SPException_term(SP_ONLY_API_ARG index, var_term_ref)))
    goto barf;
  if (NULL == (retractPred = SP_predicate("retract", 1, ""))) goto barf;
#if JASPER_DBG>1
  fprintf(stderr,"%% DBG: spRetractException(): retracting: ");
  SP_query(SP_predicate("writeq",1,""), exception_term_ref);
  fprintf(stderr,"\n");
#endif
  /* retract(prolog:'SPException'(index,_). */
  if (SP_SUCCESS != SP_query_cut_fail(retractPred, exception_term_ref))
    goto barf;
  res = JNI_TRUE;

 cleanup:
  SP_reset_term_refs(mark);
  SP_LEAVE_SICSTUS();
  return res;

 barf:
  res = JNI_FALSE;
  goto cleanup;
}


/* [PD] 3.11.1 */
JNIEXPORT jstring SP_JNICALL
Java_se_sics_jasper_SICStus_spNativeVersion(JNIEnv *jnienv, jobject spobj)
{
  jstring res;
  (void) spobj;                 /* -Wunused */
  res = (*jnienv)->NewStringUTF(jnienv, SICSTUS_VERSION_STRING);
  return res;
}

/* [PD] 3.11.1 Rename from ..._spInitialize. This makes sure that old versions
               of Java code will fail to work with spnative 3.11.1 and newer.
               The Java code will do a version check in 3.11.1 and newer.
               The suffix is based on the current version, but it does not
               necessarily have to be changed for every new version.
*/
JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spInitialize040102(JNIEnv *jnienv, jobject spobj,
                                               jobjectArray array,
                                               // [PM] 4.1.2 system properties, may be null
                                               jobjectArray keys, jobjectArray values
                                               )
{
  jsize sp_java_argc;
  char **sp_java_argv = NULL; /* [PM] 4.2.1 needs cleanup. With free() (not SP_free()). */
  jsize i;
  int rval = SP_SUCCESS;
  jstring startup_dir_string = NULL;
  char const *startup_dir = NULL;
  char const *fun = "Java_se_sics_jasper_SICStus_spInitialize040102";
  SP_options options = SP_OPTIONS_STATIC_INITIALIZER;
  SP_option *opts = NULL;

  jobject spobj_global = NULL;

  SP_ONLY_API_ARG_PROTO_DECL0;

  SP_ONLY_API_ARG_NAME=NULL;
  
  DbgBreakPoint();
  (void)fun;
#if JASPER_DBG
#if _MSC_VER >= 1400         /* VS 2005 or newer */
  /* warning C4996: 'getenv': This function or variable may be
     unsafe. Consider using _dupenv_s instead. To disable deprecation,
     use _CRT_SECURE_NO_DEPRECATE. See online help for details. */
#pragma warning (disable: 4996)
#endif  /* _MSC_VER >= 1400 */

  if (getenv("__SPNATIVE_DEBUG"))
    {
      fprintf(stderr, "%s debug break\n", fun);
      DEBUG_BREAK();
    }
#endif /* JASPER_DBG */

#if JASPER_DBG
  fprintf(stderr, "%s jnienv=%p\n", fun, jnienv);
#endif /* JASPER_DBG */

  {
    jstring propname = NULL;
    jboolean hasException = JNI_FALSE;
    jvalue val;
    
    if ((propname = (*jnienv)->NewStringUTF(jnienv, "user.dir")) == NULL)
      {
        goto barf;
      }

    val =
      CallStaticMethodByName(jnienv,
                             &hasException,
                             "java/lang/System",
                             "getProperty",
                             "(Ljava/lang/String;)Ljava/lang/String;",
                             propname);
    ASSERT_NO_EXCP(jnienv,fun);
    if (hasException) goto barf;
    startup_dir_string = val.l;
    startup_dir = (*jnienv)->GetStringUTFChars(jnienv, startup_dir_string, NULL);
#if JASPER_DBG
    fprintf(stderr, "user.dir=\"%s\"\n", (startup_dir ? startup_dir : "<<NULL>>"));fflush(stderr);
#endif  /* JASPER_DBG */
  }


#if MULTI_SP_AWARE
  {
    int res;
    SICSTUS_API_STRUCT_TYPE *sprt_dispatch;
    SP_get_dispatch_type *get_dispatch;

#if JASPER_DBG>1
    fprintf(stderr, "\nCalling SP_get_dispatch() (%p())\n", SP_get_dispatch);fflush(stderr);
#endif /* JASPER_DBG */

    sprt_dispatch = SP_get_dispatch(NULL);
    if (sprt_dispatch == NULL)
      {
#if JASPER_DBG
        fprintf(stderr, "\n*** ERROR: SP_get_dispatch() (%p()) == NULL\n", SP_get_dispatch);fflush(stderr);
#endif/* JASPER_DBG */
        goto barf;
      }
#if JASPER_DBG>1
    fprintf(stderr, "\nCalled SP_get_dispatch()==%p\n", sprt_dispatch);fflush(stderr);
#endif /* JASPER_DBG */

    SP_ONLY_API_ARG_NAME = 
      sprt_dispatch;
    
#if JASPER_DBG>1
    fprintf(stderr, "\nCalling SP_load_sicstus_run_time(%p, NULL)\n", &get_dispatch);
#endif /* JASPER_DBG */
    res = SP_load_sicstus_run_time(&get_dispatch, NULL);
#if JASPER_DBG>1
    fprintf(stderr, "\nCalled SP_load_sicstus_run_time(%p, NULL) == %d\n", &get_dispatch, res);
#endif /* JASPER_DBG */

    if (res < 0)
      {
#if JASPER_DBG
        fprintf(stderr, "\nP_load_sicstus_run_time(%p, NULL) failed\n", &get_dispatch);
#endif  
        goto barf;
      }
    SP_ONLY_API_ARG_NAME = get_dispatch(NULL);
    if (SP_ONLY_API_ARG_NAME == NULL)
      {
#if JASPER_DBG
        fprintf(stderr, "\nget_dispatch failed\n");
#endif  
        goto barf;
      }
  }
#else  /* !MULTI_SP_AWARE */
  {
#if SP_INHIBIT_IMPLICIT_APIPROCINIT /* [PM] 3.10.2 set to work around HPUX cc bug */
    SetupSICStusDISPATCH();
#endif/* SP_INHIBIT_IMPLICIT_APIPROCINIT */
  }
#endif  /* !MULTI_SP_AWARE */

  if (array != NULL)
    {
      sp_java_argc = (*jnienv)->GetArrayLength(jnienv, array);
      /* [PD] 3.9 Can't call SP_malloc here, before glue_initialize has been called. */
      sp_java_argv = (char **)malloc(sizeof(char *)*(sp_java_argc+1));
      
      if (sp_java_argv == NULL) goto barf;
      
      for (i = 0; i < sp_java_argc; i++)
        {
          jobject arg;
          
          arg = (*jnienv)->GetObjectArrayElement(jnienv, array, i);
#if SICSTUS_TODO
#error "ought to fix this leak once we start creating many sicstus instances."
#endif /* SICSTUS_TODO */
          /* FIXME: leaks. Should Release after SP_initialize */
          sp_java_argv[i] = (char *)(*jnienv)->GetStringUTFChars(jnienv, arg, NULL);
        }
      sp_java_argv[i] = NULL;
    }
  else
    {
      sp_java_argc = 0;
      sp_java_argv = NULL;
    }
  
  {
    jsize const nprops = (keys == NULL ? 0 : (*jnienv)->GetArrayLength(jnienv, keys));
    SP_ASSERT((keys != NULL) == (values != NULL));


    {
      jsize noptions = 0;
      jsize const nfixed = 3;
      jsize ntotal = nprops + nfixed;
      /* Tell SICStus to start in startup_dir and to never change the OS working directory. */
    
      if ((opts = malloc(ntotal * sizeof *opts)) == NULL) goto barf;

      if (startup_dir != NULL)
        {
          /* Make SP independent of the process working directory. Generally
             a good idea but even more so in multi-threaded apps. */
          opts[noptions].u.prop.key = "SP_STARTUP_DIR";
          opts[noptions].u.prop.value = startup_dir;
          opts[noptions].type = SP_option_type_system_property;
          noptions++;
        }
      /* Do not change process working directory (which is a no-no in multi-threaded apps) */
      opts[noptions].u.prop.key = "SP_ALLOW_CHDIR";
      opts[noptions].u.prop.value = "no";
      opts[noptions].type = SP_option_type_system_property;
      noptions++;

      /* [PM] 4.1.3 Tell SP_initialize (really spio_event_pipe()) that we are started from Java. */
      opts[noptions].u.prop.key = "SP_IN_JASPER";
      opts[noptions].u.prop.value = "yes";
      opts[noptions].type = SP_option_type_system_property;
      noptions++;

      SP_ASSERT(noptions <= nfixed);

      for (i = 0; i < nprops; i++)
        {
          jobject keyObject, valueObject;
          
          keyObject = (*jnienv)->GetObjectArrayElement(jnienv, keys, i);
          valueObject = (*jnienv)->GetObjectArrayElement(jnienv, values, i);

#if SICSTUS_TODO
#error "ought to fix this leak once we start creating many sicstus instances."
#endif /* SICSTUS_TODO */
          /* FIXME: leaks. Should Release after SP_initialize */
          opts[noptions].u.prop.key = (char *)(*jnienv)->GetStringUTFChars(jnienv, keyObject, NULL);
          opts[noptions].u.prop.value = (char *)(*jnienv)->GetStringUTFChars(jnienv, valueObject, NULL);
          opts[noptions].type = SP_option_type_system_property;
          SP_ASSERT(opts[noptions].u.prop.key != NULL);
          SP_ASSERT(opts[noptions].u.prop.value != NULL);
          noptions++;
        }
      SP_ASSERT(noptions == ntotal);

      options.options = opts;
      options.noptions = noptions;
    }
  }
  
  {
    /* Done the same with and without MULTI_SP_AWARE. The difference is in
       MULTI_SP_AWARE explicitly getting the dispatch vector above */

#if JASPER_DBG>1
    fprintf(stderr, "SP_initialize(%" SPRIuINTEGER ", %p, SP_STARTUP_DIR=\"%s\");\n", (SP_uinteger)sp_java_argc, (void*)sp_java_argv, (startup_dir ? startup_dir : "<<NULL>>"));
    fflush(stderr);
#endif /* JASPER_DBG */

    rval = SP_initialize((int)sp_java_argc, sp_java_argv, &options);

    if (rval != SP_SUCCESS)   /* [PM] 3.10 if SP_initialize failed we should not call into sprt! */
      {
        goto cleanup;
      }
  }

  {
    if (SP_ONLY_API_ARG_NAME == NULL)   /* should have been set up by  */
      {
#if JASPER_DBG
        fprintf(stderr, "ERROR: glue_initialize did not set up SP_ONLY_API_ARG_NAME");
#endif /* JASPER_DBG */
        goto glue_failure;
      }
    {
      int res = set_spobj_api(jnienv, spobj, SP_ONLY_API_ARG_NAME);
      if (res != SP_SUCCESS)
        {
#if JASPER_DBG
          fprintf(stderr,"Error (%d) from set_spobj_api, JNI=%p\n", res, jnienv);
#endif /* JASPER_DBG */
#if SICSTUS_TODO /* [PM] FIXME: fix later */
#error "need to think this through"
#endif /* SICSTUS_TODO */
          goto glue_failure;
        }
    }

    /* sprt is initialized, SICStus object has its dispatch API address
       Now tell prolog about the SICStus object.
       Use new (semi-public) API function to save the SICStus object. */
    spobj_global = (*jnienv)->NewGlobalRef(jnienv, spobj);
    if (!sp_set_jasper_magic(spobj_global,SICSTUS_VERSION)) goto glue_failure;
    spobj_global = NULL;      /* protect from DeleteGlobalRef in cleanup */
  }

 cleanup:
  if (opts != NULL) free(opts);
  if (startup_dir != NULL) (*jnienv)->ReleaseStringUTFChars(jnienv, startup_dir_string, startup_dir);
  if (spobj_global) (*jnienv)->DeleteGlobalRef(jnienv, spobj_global);
  if (sp_java_argv != NULL)
    {
      free(sp_java_argv);
      sp_java_argv = NULL;
    }
  return rval;

 glue_failure:
#if JASPER_DBG
  fprintf(stderr, "ERROR: glue_failure in %s", fun);
#endif /* JASPER_DBG */
  rval = -2;              /* SP_GLUEFAILURE */
  goto cleanup;
 barf:
  rval=SP_ERROR;
  goto cleanup;
}

/* [PM] 4.1.2 I do not think we should attempt any backward
   compatibility for internal JNI functions. (Would be an issue if
   someone uses old jasper jars with new spnative but then they are
   better of fixing their set-up.) */
#if 0
JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spInitialize040003(JNIEnv *jnienv, jobject spobj, jobjectArray array)
{
  return Java_se_sics_jasper_SICStus_spInitialize040102(jnienv, spobj, array, NULL, NULL);
}
#endif  /* 0 */

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spFinalize(JNIEnv *jnienv, jobject spobj)
{
  SP_ENTER_SICSTUS();

  SP_deinitialize();
  SP_LEAVE_SICSTUS();
}

JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spLoad(JNIEnv *jnienv, jobject spobj, jstring str)
{
  char *qlfile;
  int rval = SP_SUCCESS;

  SP_ENTER_SICSTUS();

  qlfile = (char *)(*jnienv)->GetStringUTFChars(jnienv, str, NULL);

  rval = SP_load(qlfile);

  (*jnienv)->ReleaseStringUTFChars(jnienv, str, qlfile);

  SP_LEAVE_SICSTUS();

  return (jint)rval;
}

JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spRestore(JNIEnv *jnienv, jobject spobj, jstring str)
{
  char *qlfile;
  int rval = SP_SUCCESS;
  SP_ENTER_SICSTUS();

  qlfile = (char *)(*jnienv)->GetStringUTFChars(jnienv, str, NULL);

  rval = SP_restore(qlfile);

  (*jnienv)->ReleaseStringUTFChars(jnienv, str, qlfile);

  SP_LEAVE_SICSTUS();

  return (jint)rval;
}

/* [PD] FIXME: remove this. library(jasper) should never be reloaded since it
   is not loaded by Jasper anymore. */
#if 0
JNIEXPORT jint SP_JNICALL
Java_se_sics_jasper_SICStus_spReloadJasper(JNIEnv *jnienv, jobject spobj)
{
  int rval;
  int tr_set = 0;
  SP_term_ref tr;
  SP_pred_ref pred;
  jobject spobj_global = NULL;
  SP_ENTER_SICSTUS();

  pred = SP_predicate("jasper_load", 1, "prolog");
  if (!pred) { goto barf; }
  tr = SP_new_term_ref();
  tr_set = 1;               /* ensure tr is reset at cleanup */
  spobj_global = (*jnienv)->NewGlobalRef(jnienv, spobj);
  if (!put_jlong(SP_ONLY_API_ARG tr, (jlong)spobj_global)) { goto barf; }
  rval = SP_query_cut_fail(pred, tr);
  if (rval != SP_SUCCESS) goto cleanup;
  /* succeeded in loading library(jasper) and telling it about the
     SICStus object */
  spobj_global = NULL;      /* protect from DeleteGlobalRef in cleanup */

 cleanup:
  if (spobj_global) (*jnienv)->DeleteGlobalRef(jnienv, spobj_global);
  if (tr_set) SP_reset_term_refs(tr);

  SP_LEAVE_SICSTUS();

  return rval;

 barf:
  rval = SP_ERROR;
  goto cleanup;
}
#endif


#if 0
JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spLoadLibfile(JNIEnv *jnienv, jobject spobj, jstring str)
{
  char qlpath[1024], *qlfile;
  int rval = SP_SUCCESS;
  
  SP_ENTER_SICSTUS();
  
  /* Pick up the Prolog boot-path */
  strcpy(qlpath, sp_get_boot_path());
  qlpath[strlen(qlpath)-4] = '\0'; /* Remove trailing "/bin" */
  strcat(qlpath, "/library/");

  if ((qlfile = (char *)(*jnienv)->GetStringUTFChars(jnienv, str, NULL)))
    {
      strcat(qlpath, qlfile);
      
      rval = SP_load(qlpath);
      
      (*jnienv)->ReleaseStringUTFChars(jnienv, str, qlpath);
    }
  SP_LEAVE_SICSTUS();

  return (jint)rval;
}
#endif

/*
  Exception handling:
  Calls from Prolog to Java (via Jasper) *never* leave a pending Java exception.
  Therefore if there is Java exception then it should not be ignored
 */

static jlong handleExceptions(SP_ONLY_API_ARG_PROTO_DECL JNIEnv *jnienv, int query_rval)
{
  jboolean have_jexcp;
  SP_term_ref excp_term;
  
  have_jexcp = (*jnienv)->ExceptionCheck(jnienv);

  if (query_rval == SP_ERROR)
    {
      if (have_jexcp)
        {
          /* 
           * Retract the prolog exception if there was a
           * Java exception.
           */
          excp_term = SP_new_term_ref();
          SP_exception_term(excp_term);
          SP_reset_term_refs(excp_term);
        }
    }
  else if (have_jexcp)
    {
      /* There was a java exception and no Prolog exception,
       */
      ;
    }

  return (jlong)query_rval;
}

/* The calling Java code should zap all SPTerm refs not older than termref. */
JNIEXPORT void SP_JNICALL Java_se_sics_jasper_SICStus_spResetTermRefs(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  SP_ENTER_SICSTUS();
  if (termref <= (jlong)SP_new_term_refs(0))
    {
      SP_reset_term_refs((SP_term_ref)termref);
    }
  else
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT jlong SP_JNICALL Java_se_sics_jasper_SICStus_spGetTermRefs(JNIEnv *jnienv, jobject spobj)
{
  jlong mark;
  SP_ENTER_SICSTUS();
  mark = SP_new_term_refs(0);
  SP_LEAVE_SICSTUS();
  return mark;
}

JNIEXPORT jboolean SP_JNICALL Java_se_sics_jasper_SICStus_spValidTermRef(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jboolean result;

  SP_ENTER_SICSTUS();
  result = ((0 < termref && termref < (jlong)SP_new_term_refs(0)) ? JNI_TRUE : JNI_FALSE);
  SP_LEAVE_SICSTUS();
  return result;
}


/* should be called inside an SP_ENTER_SICSTUS() block!! */
/* zero iff Java exception raised */
/* If it returns a valid (non-zero) SP_qid then it will have allocated
   a few term-refs that must be freed by the caller. The reason for
   the leak is that the term refs are needed when calling
   SP_open_query and by then it is to late to reclaim them. ([PM]
   3.8.7 SPRM 2495) */
static SP_qid jasper_open_call(SP_ONLY_API_ARG_PROTO_DECL JNIEnv *jnienv, jobject spobj, jstring module, jstring name, jlongArray args)
{
  SP_qid res = 0;
  SP_term_ref mark = SP_new_term_refs(0);
  int mark_valid = 1;           /* [PM] sprm 2495 */
  jsize numargs = (*jnienv)->GetArrayLength(jnienv, args);
  SP_term_ref *spargs;
  const char *pred_name = NULL;
  const char *module_name = NULL;
  int i;

  spargs = (SP_term_ref *)SP_malloc(sizeof(SP_term_ref)*numargs);
  if (spargs == NULL) goto out_of_memory;

  pred_name = (*jnienv)->GetStringUTFChars(jnienv, name, NULL);
  if (!pred_name) goto cleanup; /* exception raised */
  module_name = (*jnienv)->GetStringUTFChars(jnienv, module, NULL);
  if (!module_name) goto cleanup; /* exception raised */

  for (i = 0; i < numargs; i++)
    {
      jlong sparg;

      (*jnienv)->GetLongArrayRegion(jnienv, args, (jsize)i, 1, &sparg);
      DBG_VALIDATE_TERM_REF(sparg);
      spargs[i] = (SP_term_ref)sparg;
    }
  {
    SP_pred_ref call_pred = SP_predicate("jasper_call", 2, "prolog"); /* jasper_call(+Goal,+Module) */
    SP_term_ref goal_term = SP_new_term_ref();
    SP_term_ref module_term = SP_new_term_ref();
    SP_uinteger name_atom;

    if (0 == (name_atom = SP_atom_from_string(pred_name))
        ||
        !SP_put_string(module_term, module_name) ) {
      goto barf;
    }

    if (0 == SP_cons_functor_array(goal_term, name_atom, (int)numargs, spargs)) {
      goto barf;
    }
    /* Goal = PRED(ARGS) */
    res = SP_open_query(call_pred, goal_term, module_term); /* [PM] This chpt remembers current term stack index! SPRM 2495 */
    if (!res) {
      goto barf;
    }
    /* [PM] sprm 2495 We cannot reset term refs to mark since that
       would make subsequent backtracking into the query *increase*
       the term stack index (to module_term). Instead we leak the term
       refs created before SP_open_query (module_term, goal_term). */
    mark_valid = 0;
    
  }
  goto cleanup;

 out_of_memory:
  goto barf;
 barf:
  throwNCE(spobj, jnienv);
  goto cleanup;

 cleanup:

  if (pred_name) (*jnienv)->ReleaseStringUTFChars(jnienv, name, pred_name);
  if (module_name) (*jnienv)->ReleaseStringUTFChars(jnienv, module, module_name);

  if (spargs) SP_free(spargs);              /* already bombed if SP_malloc returned NULL */

  if (mark_valid) SP_reset_term_refs(mark);
  return res;                   /* zero iff Java exception raised */
}

JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spOpenContext(JNIEnv *jnienv, jobject spobj)
{
  SP_qid query;
  SP_ENTER_SICSTUS();
 
  query = SP_open_query(SP_predicate("true", 0, "prolog"));

  SP_LEAVE_SICSTUS();
  return (!query ? 0 : (jlong)query);
}


JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spCall(JNIEnv *jnienv, jobject spobj, jstring module, jstring name, jlongArray args)
{
  SP_qid query;
  int rc;
  jlong res;
  SP_term_ref mark;
  SP_term_ref mark_valid = 0;

  ASSERT_NO_EXCP(jnienv, "Java_se_sics_jasper_SICStus_spCall\n");

  SP_ENTER_SICSTUS();

#if JASPER_DBG
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spCall:%d, JNI=%p\n", __LINE__, jnienv);
#endif

  mark = SP_new_term_refs(0);
  mark_valid = 1;
  JASPER_DBGLINE(spCall);
  query = jasper_open_call(SP_ONLY_API_ARG jnienv, spobj, module, name, args);
  JASPER_DBGLINE(spCall);
  
  if (!query)                   /* Java exception raised */
    {
      rc = SP_SUCCESS;
      goto cleanup;
    }
  ASSERT_NO_EXCP(jnienv, "Java_se_sics_jasper_SICStus_spCall\n");

  JASPER_DBGLINE(spCall);
  if ((rc = SP_next_solution(query)) > 0)
    {
      JASPER_DBGLINE(spCall);
#if JASPER_DBG>1
      fprintf(stderr,"Java_se_sics_jasper_SICStus_spCall:%d, JNI=%p rc=%d\n", __LINE__, jnienv, (int)rc);
#endif
      SP_cut_query(query);
      JASPER_DBGLINE(spCall);
    }
  else
    {
      SP_close_query(query);
    }

 cleanup:
  JASPER_DBGLINE(spCall);
  res = (jlong)handleExceptions(SP_ONLY_API_ARG jnienv, rc);
  if (mark_valid) SP_reset_term_refs(mark);
  SP_LEAVE_SICSTUS();
  return res;
}


JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spCallCutFail(JNIEnv *jnienv, jobject spobj, jstring module, jstring name, jlongArray args)
{
  SP_qid query;
  int rc;
  jlong res;
  SP_term_ref mark;
  SP_term_ref mark_valid = 0;

  ASSERT_NO_EXCP(jnienv, "Java_se_sics_jasper_SICStus_spCallCutFail\n");

  SP_ENTER_SICSTUS();

#if JASPER_DBG>1
  fprintf(stderr,"Java_se_sics_jasper_SICStus_spCallCutFail:%d, JNI=%p\n", __LINE__, jnienv);
#endif
 
  mark = SP_new_term_refs(0);
  mark_valid = 1;
  query = jasper_open_call(SP_ONLY_API_ARG jnienv, spobj, module, name, args);

  if (!query)                   /* Java exception raised */
    {
      rc = SP_SUCCESS;
      goto cleanup;
    }
  ASSERT_NO_EXCP(jnienv, "Java_se_sics_jasper_SICStus_spCallCutFail\n");

  if ((rc = SP_next_solution(query)) > 0)
    {
#if JASPER_DBG>1
      fprintf(stderr,"Java_se_sics_jasper_SICStus_spCallCutFail:%d, JNI=%p rc=%d\n", __LINE__, jnienv, (int)rc);
#endif
    }
  SP_close_query(query);

 cleanup:
  res = (jlong)handleExceptions(SP_ONLY_API_ARG jnienv, rc);
  if (mark_valid) SP_reset_term_refs(mark);

  SP_LEAVE_SICSTUS();
  return res;
}


JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spOpenCall(JNIEnv *jnienv, jobject spobj, jstring module, jstring name, jlongArray args)
{
  SP_qid query;
  SP_ENTER_SICSTUS();
 
  /* 3.8.7 Note that this leaks term-refs that must be freed by caller after the query is closed ([PM] 3.8.7 sprm 2495) */
  query = jasper_open_call(SP_ONLY_API_ARG jnienv, spobj, module, name, args);

  SP_LEAVE_SICSTUS();
  return (!query ? 0 : (jlong)query);
}


JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spMakeTermRef(JNIEnv *jnienv, jobject spobj)
{
  jlong res;

  SP_ENTER_SICSTUS();

  res = (jlong)SP_new_term_ref();
  DBG_VALIDATE_TERM_REF(res);
  SP_LEAVE_SICSTUS();
  return res;
}


JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spTermType(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jint res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  res = (jint)SP_term_type((SP_term_ref)termref);

  SP_LEAVE_SICSTUS();
  return res;
}

JNIEXPORT jstring SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetListChars(JNIEnv *jnienv, jobject spobj, /* jobject term, */ jlong termref)
{
  jstring str = NULL;
  char const *name = NULL;
  
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_get_list_codes((SP_term_ref)termref, &name))
    {
      str = (*jnienv)->NewStringUTF(jnienv, name);
    }
  else
    {
      throwNCE(spobj, jnienv);
    }
  
  SP_LEAVE_SICSTUS();
  return str;
}

/* [PM] 3.8.7 Was declared but not defined! */
JNIEXPORT jstring SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetNumberChars(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jstring str = NULL;
  char const *name = NULL;
  
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_get_number_codes((SP_term_ref)termref, &name))
    {
      str = (*jnienv)->NewStringUTF(jnienv, name);
    }
  else
    {
      throwNCE(spobj, jnienv);
    }
  
  SP_LEAVE_SICSTUS();
  return str;
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetList(JNIEnv *jnienv, jobject spobj, jlong termref, jlong headref, jlong tailref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_get_list((SP_term_ref)termref, (SP_term_ref)headref, (SP_term_ref)tailref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spPutTerm(JNIEnv *jnienv, jobject spobj, jlong termref, jlong termref2)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_put_term((SP_term_ref)termref, (SP_term_ref)termref2))
    {
      throwNCE(spobj, jnienv);
    }

  SP_LEAVE_SICSTUS();
}

/*
 * Class:     jasper_SPTerm
 * Method:    spGetFunctorCanonical
 * Signature: (Ljasper/SICStus;J)J
 */
JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetFunctorCanonical(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  SP_uinteger canonical;
  int dummy_arity;
  jlong res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_get_functor((SP_term_ref)termref, &canonical, &dummy_arity))
    {
      throwNCE(spobj, jnienv);
      res = 0;
    }
  else
    {
      res = (jlong)canonical;
    }

  SP_LEAVE_SICSTUS();
  return res;
}

/*
 * Class:     jasper_SPTerm
 * Method:    spGetFunctorArity
 * Signature: (Ljasper/SICStus;J)J
 */
JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetFunctorArity(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  SP_uinteger dummy_canonical;
  int arity;
  jint res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_get_functor((SP_term_ref)termref, &dummy_canonical, &arity))
    {
      throwNCE(spobj, jnienv);
      res = 0;
    }
  else
    {
      res = (jint) arity;
    }

  SP_LEAVE_SICSTUS();
  return res;
}

/*
 * Class:     jasper_SPTerm
 * Method:    spGetArg
 * Signature: (Ljasper/SICStus;JJJ)J
 */
JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetArg(JNIEnv *jnienv, jobject spobj, jlong i, jlong termref, jlong arg)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_get_arg((int)i, (SP_term_ref)termref, (SP_term_ref)arg))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spPutVariable(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_variable((SP_term_ref)termref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spPutList(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_put_list((SP_term_ref)termref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT jboolean SP_JNICALL 
Java_se_sics_jasper_SICStus_spIsList(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_is_list((SP_term_ref)termref))
    {
      res = JNI_TRUE;
    }
  else
    {
      res = JNI_FALSE;
    }

  SP_LEAVE_SICSTUS();
  return res;
}

JNIEXPORT jboolean SP_JNICALL 
Java_se_sics_jasper_SICStus_spIsEmptyList(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  res = JNI_FALSE;
  /* SP API really need SP_is_empty_list()/SP_is_nil() */
  if (SP_is_atom((SP_term_ref)termref))
    {
      char const *pname;
      
      if (SP_get_string((SP_term_ref)termref, &pname))
        {
          if (pname[0] == '['
              && pname[1] == ']'
              && pname[2] == '\0')
            {
              res = JNI_TRUE;
            }
        }
    }

  SP_LEAVE_SICSTUS();
  return res;
}

JNIEXPORT jboolean SP_JNICALL 
Java_se_sics_jasper_SICStus_spIsAtomic(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_is_atomic((SP_term_ref)termref))
    res = JNI_TRUE;
  else
    res = JNI_FALSE;

  SP_LEAVE_SICSTUS();
  return res;

}

JNIEXPORT jboolean SP_JNICALL 
Java_se_sics_jasper_SICStus_spIsNumber(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (SP_is_atomic((SP_term_ref)termref))
    res = JNI_TRUE;
  else
    res = JNI_FALSE;
  
  SP_LEAVE_SICSTUS();
  return res;
}

/*
 * Class:     jasper_SPTerm
 * Method:    spGetStringFromAtom
 * Signature: (Ljasper/SICStus;J)Ljava/lang/String;
 */
JNIEXPORT jstring SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetStringFromAtom(JNIEnv *jnienv, jobject spobj, /* jobject term, */ jlong canonical)
{
  jstring str = NULL;
  char const *name = NULL;

  SP_ENTER_SICSTUS();

  name = SP_string_from_atom((SP_uinteger) canonical);

  if (name)
    str = (*jnienv)->NewStringUTF(jnienv, name);
  
  SP_LEAVE_SICSTUS();
  return str;  
}

JNIEXPORT jstring SP_JNICALL
Java_se_sics_jasper_SICStus_spPrintVariable(JNIEnv *jnienv, jobject spobj, jlong termref)
{
  char atom_buffer[256];
  jstring res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

#if 0
  printf("in spPrintVariable(): termref = %" SPRIdINTEGER "\n", (SP_integer)termref);
#endif  

  sp_variable_to_string((SP_term_ref)termref, atom_buffer);
  
  res = (*jnienv)->NewStringUTF(jnienv, atom_buffer);

  SP_LEAVE_SICSTUS();
  return res;
}

JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spGetAtomFromString(JNIEnv *jnienv, jobject spobj, jstring str)
{
  SP_uinteger canonical = 0;
  char *s;

  SP_ENTER_SICSTUS();

  if ((s = (char *)(*jnienv)->GetStringUTFChars(jnienv, str, NULL)))
    {
      canonical = SP_atom_from_string(s);
      
      (*jnienv)->ReleaseStringUTFChars(jnienv, str, s);
    }

  if (!canonical)
    {
      throwNCE(spobj, jnienv);
    }

  SP_LEAVE_SICSTUS();
  return canonical;
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spPutFunctor(JNIEnv *jnienv, jobject spobj, jlong termref, jlong atom, jint arity)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);

  if (0 == SP_put_functor((SP_term_ref)termref, (SP_uinteger)atom, arity))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

JNIEXPORT void SP_JNICALL 
Java_se_sics_jasper_SICStus_spConsList(JNIEnv *jnienv, jobject spobj, jlong termref, jlong headref, jlong tailref)
{
  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(termref);
  if (0 == SP_cons_list((SP_term_ref)termref, (SP_term_ref)headref, (SP_term_ref)tailref))
    {
      throwNCE(spobj, jnienv);
    }
  SP_LEAVE_SICSTUS();
}

/*
 * Class:     jasper_SPTerm
 * Method:    spCompare
 * Signature: (Ljasper/SICStus;JJ)I
 */
JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spCompare(JNIEnv *jnienv, jobject spobj, jlong t1, jlong t2)
{
  jint res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(t1);
  DBG_VALIDATE_TERM_REF(t2);
  res = SP_compare((SP_term_ref)t1, (SP_term_ref)t2);

  SP_LEAVE_SICSTUS();
  return res;
}


/*
 * Class:     jasper_SPTerm
 * Method:    spUnify
 * Signature: (Ljasper/SICStus;JJ)Z
 */
JNIEXPORT jboolean SP_JNICALL 
Java_se_sics_jasper_SICStus_spUnify(JNIEnv *jnienv, jobject spobj, jlong t1, jlong t2)
{
  jboolean res;

  SP_ENTER_SICSTUS();
  DBG_VALIDATE_TERM_REF(t1);
  DBG_VALIDATE_TERM_REF(t2);

  if (SP_unify((SP_term_ref)t1, (SP_term_ref)t2))
    res = JNI_TRUE;
  else
    res = JNI_FALSE;

  SP_LEAVE_SICSTUS();
  return res;
}

JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spRegisterAtom(JNIEnv *jnienv, jobject spobj, jlong t1)
{
  jint res;

  SP_ENTER_SICSTUS();

  res = SP_register_atom((SP_uinteger)t1);

  SP_LEAVE_SICSTUS();
  return res;
}


JNIEXPORT jint SP_JNICALL 
Java_se_sics_jasper_SICStus_spUnRegisterAtom(JNIEnv *jnienv, jobject spobj, jlong t1)
{
  jint res;

  SP_ENTER_SICSTUS();

  res = SP_unregister_atom((SP_uinteger)t1);

  SP_LEAVE_SICSTUS();
  return res;
}

JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spAtomFromString(JNIEnv *jnienv, jobject spobj, /* jobject t, */ jstring string)
{
  char *str;
  jlong cAtom = 0;

  SP_ENTER_SICSTUS();

  str = (char *)(*jnienv)->GetStringUTFChars(jnienv, string, NULL);
  if (str) {
    cAtom = (jlong)SP_atom_from_string(str);
    (*jnienv)->ReleaseStringUTFChars(jnienv, string, str);
  }

  if (!cAtom) {
    throwNCE(spobj, jnienv);
    cAtom = 0;
  }

  SP_LEAVE_SICSTUS();
  return cAtom;
}

JNIEXPORT jstring SP_JNICALL 
Java_se_sics_jasper_SICStus_spStringFromAtom(JNIEnv *jnienv, jobject spobj, jlong cAtom)
{
  jstring res;

  SP_ENTER_SICSTUS();
  res =  (*jnienv)->NewStringUTF(jnienv,SP_string_from_atom((SP_uinteger)cAtom));
  SP_LEAVE_SICSTUS();
  return res;
}

/*
 * CODESIO support
 * xref library(codesio)
 */

struct open_String {
  SP_stream *stream;
  struct jasper_stream_data *p; /* for write stream */
  JNIEnv *jnienv;
  struct open_String *next;
};


/*********************************************************************************/
/*********************************************************************************/
#define SP_ALIGN_DOWN(X, Alignment) \
    (((size_t)(X)) & ~(size_t)((Alignment)-1))

/* Round up to even multiple of Alignment (a power of two) */
#define SP_ALIGN_UP(X, Alignment) \
    SP_ALIGN_DOWN(((size_t)(X) + ((Alignment)-1)), (Alignment))

#define SP_ALIGN(X, Alignment) SP_ALIGN_UP((X), (Alignment))

enum {
  /* JASPER_STREAM_FLAGS_READ  = 0x0001, */
  JASPER_STREAM_FLAGS_WRITE = 0x0002,

  JASPER_STREAM_FLAGS_ENCODER_INITED  = 0x0010,

  JASPER_STREAM_FLAGS_END_
};

#define JASPER_STREAM_BUFLEN 1024 /* Must be a a power of two (so it is suitable as Alignment arg to SP_ALIGN_UP, see ENSURE_SPACE) */
#define SP_JASPER_BUF_ALIGNMENT 1024 /* must be a power of two (for SP_ALIGN_UP) */
#define PAD 128
#define ENSURE_SPACE(I) do{                                             \
  if (p->index + (I) + PAD >= p->size)                                  \
    {                                                                   \
      size_t size_ = SP_ALIGN_UP(p->index+(I)+PAD+1, SP_JASPER_BUF_ALIGNMENT); \
      char *tmp_ = (char *)SP_realloc(p->buffer, size_);                \
                                                                        \
      SPIO_NULL_CHECK(tmp_);                                            \
      p->size = size_;                                                  \
      p->buffer = tmp_;                                                 \
    }                                                                   \
}while(0)

struct jasper_stream_data {
  spio_t_bits flags;
  SP_ONLY_API_ARG_PROTO_DECL0;  /* the field is named SP_ONLY_API_ARG_NAME */
  SP_stream *stream;             /* set to NULL to tell Java that stream has been closed */
  jmethodID midAppendCodePoint; /* java/lang/StringBuffer#appendCodePoint(int) */
  jmethodID midAppend; /* java/lang/StringBuffer#append(char) used as fallback on < Java 1.5 */
  JNIEnv *jnienv;
  jobject global_stringBuf;     /* global ref to stringbuffer (needs cleanup) */
};

static void jasper_stream_data_deinit(struct jasper_stream_data *p)
{
  if (p != NULL)
    {
      if (p->global_stringBuf != NULL)
        {
          (*p->jnienv)->DeleteGlobalRef(p->jnienv, p->global_stringBuf);
          p->global_stringBuf = NULL; /* not needed */
        }
      p->SP_ONLY_API_ARG_NAME = NULL; /* not needed */
      p->jnienv = NULL;         /* not needed */
      p->midAppendCodePoint = NULL; /* not needed */
      p->midAppend = NULL;      /* not needed */
      /* should not attempt to close p->stream even if != NULL */
      p->stream = NULL;
      p->flags = 0;
    }
}

static spio_t_error_code SPIO_CDECL
user_write(void *user_data,
           void const *buf,           /* a spio_t_wchar[] */
           size_t *pnbytes,
           spio_t_bits write_options
           )
{
  spio_t_error_code code = SPIO_E_ERROR;
  struct jasper_stream_data *p = (struct jasper_stream_data *)user_data;
  SP_ENTER_SICSTUS1(p->SP_ONLY_API_ARG_NAME);

  SP_ASSERT(SPIO_MASK_IS_SET(write_options, SPIO_DEVICE_WRITE_OPTION_TEXT));
  (void)write_options;

  {
    spio_t_wchar const *src = (spio_t_wchar const *)buf;
    size_t buf_size = *pnbytes;
    size_t len = buf_size/(sizeof src[0]);
    size_t i;
    JNIEnv * const jnienv = p->jnienv;
    SP_ASSERT(jnienv != NULL);

    if (p->midAppendCodePoint != NULL) /* Java 5 */
      {
        for (i = 0; i < len; i++)
          {
            jobject localRef = (*jnienv)->CallObjectMethod(jnienv, p->global_stringBuf, p->midAppendCodePoint,(jint)src[i]);
            if (localRef != NULL)
              {
                (*jnienv)->DeleteLocalRef(jnienv, localRef);
              }
          }
      }
    else                        /* Java <= 1.4  */
      {
        SP_ASSERT(p->midAppend != NULL);
        for (i = 0; i < len; i++)
          {
            jobject localRef = (*jnienv)->CallObjectMethod(jnienv, p->global_stringBuf, p->midAppend,(jchar)src[i]);
            if (localRef != NULL)
              {
                (*jnienv)->DeleteLocalRef(jnienv, localRef);
              }
          }
      }
    SP_ASSERT(buf_size == (len * (sizeof src[0])));
    *pnbytes = (len * (sizeof src[0])); /* nbytes written */

    ASSERT_NO_EXCP(p->jnienv,"user_write");
    /* FIXME: report exception? */
    code = SPIO_S_NOERR;
  }
  goto cleanup;

 cleanup:
  SP_LEAVE_SICSTUS();
  return code;
}

static spio_t_error_code SPIO_CDECL
user_close(void **puser_data,
           spio_t_bits close_options
           )
{
  spio_t_error_code code = SPIO_E_ERROR;
  struct jasper_stream_data *p = (struct jasper_stream_data *)*puser_data;
  SP_ENTER_SICSTUS1(p->SP_ONLY_API_ARG_NAME);
  (void)SP_ONLY_API_ARG_NAME; /* Avoid warning: set but not used. */

  if ( SPIO_MASK_IS_SET(close_options, SPIO_DEVICE_CLOSE_OPTION_WRITE)
       &&
       SPIO_MASK_IS_SET(p->flags, JASPER_STREAM_FLAGS_WRITE))
    {
      /* was open in and now closing write direction */
      SPIO_CLEAR_MASK(p->flags, JASPER_STREAM_FLAGS_WRITE);
    }

  if ( /* !SPIO_MASK_IS_SET(p->flags, JASPER_STREAM_FLAGS_READ)
          && */
      !SPIO_MASK_IS_SET(p->flags, JASPER_STREAM_FLAGS_WRITE)
      )
    {
      /* no direction open, do real close */

      /* Tell Java_se_sics_jasper_SICStus_spCloseStringStream that we are closed */
      p->stream = NULL;
      /* Further cleanup of p is done by Java */
      *puser_data = NULL;       /* tell caller we are gone */
    }
  code = SPIO_S_NOERR;
  goto cleanup;

#if 0
 barf:
  goto cleanup;
#endif

 cleanup:
  SP_LEAVE_SICSTUS();
  return code;
}

/* arbitrary but unique pointer used to identify our streams */
#define JASPER_STREAM_CLASS ((void*)&init_jasper_stream)

static spio_t_error_code init_jasper_stream(JNIEnv *jnienv, jobject spobj, jobject stringBuf, SP_stream **pstream, struct jasper_stream_data **pp)
{
  spio_t_error_code code = SPIO_E_ERROR;
  struct jasper_stream_data *p = NULL; /* needs cleanup */
  SP_stream *stream = NULL;     /* needs cleanup */
  jobject global_stringBuf = NULL; /* needs cleanup */
  jclass clazz = NULL;     /* needs cleanup */
  jmethodID midAppend = NULL;
  jmethodID midAppendCodePoint = NULL;
  
  SP_ENTER_SICSTUS();

  ASSERT_NO_EXCP(jnienv, "Entry init_jasper_stream");

  if (!(global_stringBuf = (*jnienv)->NewGlobalRef(jnienv, stringBuf))) {
    BARF(SPIO_E_ERROR);
  }
  clazz = (*jnienv)->GetObjectClass(jnienv,stringBuf);
  SP_ASSERT(clazz != NULL);
  if (clazz == NULL) BARF(SPIO_E_ERROR);

  /* StringBuffer#appendCodePoint since 1.5 */
  midAppendCodePoint = (*jnienv)->GetMethodID(jnienv, clazz, "appendCodePoint","(C)Ljava/lang/StringBuffer;");
  if (midAppendCodePoint == NULL)
    {
      SP_ASSERT((*jnienv)->ExceptionCheck(jnienv) == JNI_TRUE);
      (*jnienv)->ExceptionClear(jnienv);
    }

  if (midAppendCodePoint == NULL)
    {
      midAppend = (*jnienv)->GetMethodID(jnienv, clazz, "append","(C)Ljava/lang/StringBuffer;");
      SP_ASSERT(midAppend != NULL);
      if (midAppend == NULL) BARF(SPIO_E_ERROR);
    }

  { /* not allowed to barf in this block if p allocated */
    NULL_CHECK(p = (struct jasper_stream_data *) SP_malloc(sizeof(struct jasper_stream_data)));
    p->SP_ONLY_API_ARG_NAME = SP_ONLY_API_ARG_NAME;
    p->flags = JASPER_STREAM_FLAGS_WRITE;
    p->jnienv = jnienv;
    p->stream = NULL;
    p->midAppendCodePoint = midAppendCodePoint;
    p->midAppend = midAppend;
    p->global_stringBuf = global_stringBuf;
    global_stringBuf = NULL;      /* protect from cleanup; */
  }
  
  CHECK(SP_create_stream((void*)p, /* user_data */
                         JASPER_STREAM_CLASS, /* user_class */
                         NULL,  /* user_read */
                         user_write,
                         NULL, /* user_flush_output not needed */
                         NULL, /* user_seek */
                         user_close,
                         NULL, /* user_interrupt */
                         NULL, /* user_ioctl */
                         NULL, /* args */
                         SP_CREATE_STREAM_OPTION_TEXT,
                         &stream
                         ));
  p->stream = stream;
  /* no failure allowed past this point */
  *pp = p;
  p = NULL;               /* stream owns p */

  *pstream = stream;
  stream = NULL;                /* protect from cleanup */
  code = SPIO_S_NOERR;

  goto cleanup;

 barf:

  if ((*jnienv)->ExceptionCheck(jnienv) == JNI_TRUE)
    {
      (*jnienv)->ExceptionClear(jnienv);
    }

  goto cleanup;

 cleanup:

  if (p != NULL)
    {
      jasper_stream_data_deinit(p);
      SP_free(p);
    }
  if (stream != NULL)
    {
      (void)SP_fclose(stream, SP_FCLOSE_OPTION_FORCE);
    }
  if (global_stringBuf != NULL)
    {
      (*jnienv)->DeleteGlobalRef(jnienv, global_stringBuf);
    }
  if (clazz != NULL)
    {
      (*jnienv)->DeleteLocalRef(jnienv, clazz);
    }

  SP_LEAVE_SICSTUS();
  return code;
}


/*********************************************************************************/
/*********************************************************************************/


/* Linked list of all open_String structures. Access to this is
   protected by SP_ENTER_SICSTUS (?), or rather, the SICStus clazz
   monitor:

   [PM] 3.9b5 We synchronize the access to open_Strings by all SICStus
   objects using the SICStus class (in
   SICStus.openStringStream/closeStringStream). This way it will not
   matter that there may be string streams belonging to more than one
   run-time on the list.
*/

static struct open_String *open_Strings = NULL;

/* [PM] 3.9b5 must have a lock on SICStus Clazz to enter this  */
static SP_stream *open_String_stream(SP_ONLY_API_ARG_PROTO_DECL JNIEnv *jnienv, jobject spobj, jstring string)
{
  SP_stream *stream = NULL;
  SP_stream *result_stream = NULL;
  struct open_String *buf = NULL;
  char const *utf8_bytes = NULL;
  
  buf = (struct open_String *)SP_malloc(sizeof(struct open_String));
  if (buf == NULL) goto out_of_memory;
  buf->stream = NULL;
  buf->jnienv = jnienv;
  buf->p = NULL;                /* not a StringBuffer stream */
  buf->next = NULL;

  buf->next = open_Strings;

  utf8_bytes = (*jnienv)->GetStringUTFChars(jnienv, string, NULL);

  if (!SP_stream_from_string(utf8_bytes, &stream)) goto barf;
  buf->stream = stream;
  stream = NULL;                /* protect from cleanup */


  /* Must not barf past this point */
  result_stream  = buf->stream;
  open_Strings = buf;
  buf = NULL;                   /* protect from cleanup */
  
  goto cleanup;

 cleanup:
  if (utf8_bytes != NULL)
    {
      (*jnienv)->ReleaseStringUTFChars(jnienv, string, utf8_bytes);
    }
  if (stream != NULL)
    {
      SP_fclose(stream, SP_FCLOSE_OPTION_FORCE);
    }
  if (buf != NULL)
    {
      SP_ASSERT(open_Strings != buf);
      if (buf->stream != NULL)
        {
          SP_fclose(stream, SP_FCLOSE_OPTION_FORCE);
          buf->stream = NULL;
        }
    }
  return result_stream;

 out_of_memory:
  goto barf;
 barf:
  throwNCE(spobj, jnienv);
  goto cleanup;
}


JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spOpenStringStream(JNIEnv *jnienv, jobject spobj, jstring string)
{
  SP_stream *stream;
  SP_integer stream_code;
  
  SP_ENTER_SICSTUS();
  stream = open_String_stream(SP_ONLY_API_ARG jnienv, spobj, string);
  
  if (stream != NULL)
  {
    SP_term_ref tmp;
    
    tmp = SP_new_term_ref();
    
    if (! ( SP_put_address(tmp, stream)
            &&
            SP_get_integer(tmp, &stream_code))) /* [PM] 3.9.1 *not* get_jlong */
      {
        stream_code = 0;
      }
    SP_reset_term_refs(tmp);
  }
  else
    {
      stream_code = 0;
      ; /* [PM] 4.0.2+ exception thrown */
    }
  
  SP_LEAVE_SICSTUS();

  return (jlong)stream_code;
}

JNIEXPORT jboolean SP_JNICALL
Java_se_sics_jasper_SICStus_spCloseStringStream(JNIEnv *jnienv, jobject spobj, jlong stream_code)
{
  jboolean closed = JNI_FALSE;
  SP_stream *stream;
  SP_ENTER_SICSTUS();           /* also to protect open_Strings (huh, caller should do the locking?) */

  /* convert from stream code to address */
  {
    SP_term_ref tmp;
    
    tmp = SP_new_term_ref();
    if (! (put_jlong(SP_ONLY_API_ARG tmp, stream_code)
           &&
           SP_get_address(tmp, (void*)&stream) ) )
      {
        stream = NULL;
      }
    SP_reset_term_refs(tmp);
  }

  {
    struct open_String **pp;

    /* Go through *all* buf objects. Close stream if not already and
       free all pending bufs.
       Obviously this code assumes there are very few opened String
       streams. Unless there are multiple Java threads there will be
       only one. 

       [PM] 4.0.2+ FIXME: the right thing would be to have one stream
       (per direction) for each SICStus object and to never close that
       stream (but what about re-entrancy?).
    */
    for (pp = &open_Strings; *pp != NULL;)
      {
        struct open_String *buf = *pp;
        /*
          [PM] 4.0.2+ We no longer have a close hook when reading from
          string. Instead we assume that the prolog code will not try
          to close a stream opened by open_String_stream.
         */
        if (buf->stream == stream) /* this is the stream */
          {
            buf->stream = NULL;
            if (buf->p != NULL) /* Stringbuffer stream */
              {
                if (buf->p->stream != NULL) /* not closed */
                  {
                    SP_ASSERT(buf->p->stream == stream);
                    (void) SP_fclose(stream, SPIO_OPTION_NONE);
                    SP_ASSERT(buf->p->stream == NULL);

                    jasper_stream_data_deinit(buf->p);
                    SP_free(buf->p);
                    buf->p = NULL;
                  }
              }
            else                /* String stream */
              {
                closed = JNI_TRUE;

                (void) SP_fclose(stream, SPIO_OPTION_NONE);
              }
          }
        /* Both if closed here and if closed from prolog */
        /* The caller should have obtained a lock on the SICStus class
           so that we do not risk seeing a buf owned by some other
           jnienv */

        if (buf->stream == NULL)
          {
            *pp = buf->next;   /* unlink buf */


            SP_ASSERT(jnienv == buf->jnienv);
            SP_free(buf);
          }
        else
          {
            pp = &buf->next;
          }
        /* INVARIANT *pp == buf->next */
      }
  }
  
  SP_LEAVE_SICSTUS();
  return closed;
}

/* [PD] 3.9.2 We must have a lock on SICStus Clazz to enter this
              (see open_String_stream).
*/
static SP_stream *open_StringBuffer_stream(SP_ONLY_API_ARG_PROTO_DECL JNIEnv *jnienv, jobject spobj, jobject stringBuf)
{
  spio_t_error_code code = SPIO_E_ERROR;
  SP_stream *result_stream;
  SP_stream *stream = NULL;     /* needs cleanup */
  struct open_String *buf = NULL;

  NULL_CHECK(buf = (struct open_String *)SP_malloc(sizeof(struct open_String)));
  buf->stream = NULL;
  buf->p = NULL;
  buf->jnienv = jnienv;
  buf->next = open_Strings;

  CHECK(init_jasper_stream(jnienv, spobj, stringBuf, &stream, &buf->p));

  buf->stream = stream;
  stream = NULL;              /* protect from cleanup */

  /* Must not barf past this point */
  result_stream  = buf->stream;
  open_Strings = buf;
  buf = NULL;                   /* protect from cleanup */
  goto cleanup;

 barf:
  throwNCE(spobj, jnienv);
  (void)code;
  result_stream = NULL;
  goto cleanup;

 cleanup:
  
  if (stream != NULL)
    {
      SP_fclose(stream, SP_FCLOSE_OPTION_FORCE);
    }

  if (buf != NULL)
    {
      SP_ASSERT(open_Strings != buf);
      if (buf->stream != NULL)
        {
          SP_fclose(stream, SP_FCLOSE_OPTION_FORCE);
          buf->stream = NULL;
        }
      if (buf->p != NULL)
        {
          jasper_stream_data_deinit(buf->p);
          buf->p = NULL;
        }
      buf->jnienv = NULL;
      buf->next = NULL;
      SP_free(buf);
      buf = NULL;
    }
  return result_stream;
}


JNIEXPORT jlong SP_JNICALL 
Java_se_sics_jasper_SICStus_spOpenStringBufferStream(JNIEnv *jnienv, jobject spobj, jobject stringBuf)
{
  SP_stream *stream;
  SP_integer stream_code;

  SP_ENTER_SICSTUS();
  stream = open_StringBuffer_stream(SP_ONLY_API_ARG jnienv, spobj, stringBuf);
  {
    SP_term_ref tmp = SP_new_term_ref();
    if (! ( SP_put_address(tmp, stream)
            && SP_get_integer(tmp, &stream_code))) {
      stream_code = 0;
    }
    SP_reset_term_refs(tmp);
  }

  SP_LEAVE_SICSTUS();
  return (jlong)stream_code;
}




/** [PM] Keep the original indentation style
 *  Local variables:
 *      indent-tabs-mode: nil
 *      c-basic-offset: 2
 *  end:
 **/

