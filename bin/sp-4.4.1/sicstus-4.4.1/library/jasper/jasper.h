/*
 * Copyright (c) 1999, SICS
 */


/* [PM] 3.8.5 Glue code includes this and defines JASPER_GLUE */
/* [PM] 3.8.5 jasper.c  includes this and defines JASPER_SOURCE */

#ifndef INCLUDED_JASPER_H_
#define INCLUDED_JASPER_H_

#include <jni.h>
#include <sicstus/sicstus.h>
#include <sicstus/config.h>     /* ([PM] 4.2.1 this also defines SICSTUS_DBG) */
#ifndef JASPER_DBG
#if SICSTUS_DBG
#define JASPER_DBG SICSTUS_DBG
#endif  /* SICSTUS_DBG */
#endif  /* !defined JASPER_DBG */

/* [PM] 3.9 */
#ifndef MULTI_SP
#define MULTI_SP 1
#endif /* MULTI_SP */


#if (JASPER_DBG+0) > 0
#define JASPER_STRINGISIZE1(X) #X
#define JASPER_STRINGISIZE(X) JASPER_STRINGISIZE1(X)
/* #define JASPER_DBGLINE(PROC) fprintf(stderr, __FILE__ " @`" JASPER_STRINGISIZE(PROC) "':" JASPER_STRINGISIZE(__LINE__) "\n") */
#define JASPER_DBGLINE(PROC) do {fprintf(stderr, __FILE__ " @`" JASPER_STRINGISIZE(PROC) "':" JASPER_STRINGISIZE(__LINE__) "\n"); fflush(stderr);} while(0)
#else
#define JASPER_DBGLINE(_)
#endif

#if defined(JASPER_SOURCE)

#if 1  /* 3.9 */                  /* no linking with libjasper.so in 3.9 */
#else  /* 3.8 */                  /*foreign resources link with libjasper.so */

#if defined(_MSC_VER)
#define JASPER_EXPORT __declspec(dllexport)
#else
#define JASPER_EXPORT
#endif

#endif /* 3.8 */

#else /* !JASPER_SOURCE */
#define JASPER_EXPORT
#endif /* !JASPER_SOURCE */



#if JASPER_RESOURCES
extern JASPER_EXPORT JNIEnv * SPCDECL jasperi_get_jnienv PROTOTYPE((SP_term_ref));
/* extern JASPER_EXPORT int SPCDECL jasperi_deinitialize_C PROTOTYPE((SP_term_ref)); */
extern JASPER_EXPORT SP_term_ref SPCDECL jasperi_initialize_C PROTOTYPE((int, SP_term_ref, SP_term_ref));
extern JASPER_EXPORT void SPCDECL jasperi_new_object_C PROTOTYPE((SP_term_ref jvmref,char *className,char *typesig, SP_term_ref argmap, SP_term_ref args, SP_integer numargs, SP_term_ref obj));

/* [PM] 3.8.5 should be called by generated glue code */
extern JASPER_EXPORT SP_uinteger SPCDECL jasper_glue_push_context(JNIEnv *jnienv);
/* [PM] 3.8.5 should be called by generated glue code */
extern JASPER_EXPORT SP_uinteger SPCDECL jasper_glue_pop_context(JNIEnv *jnienv, SP_uinteger context);
/* [PM] 3.8.5 should be called by generated glue code */
extern JASPER_EXPORT int SPCDECL jasper_glue_enter_context_monitor(JNIEnv *jnienv, SP_uinteger context);
/* [PM] 3.8.5 should be called by generated glue code */
extern JASPER_EXPORT int SPCDECL jasper_glue_leave_context_monitor(JNIEnv *jnienv, SP_uinteger context);
#endif /* JASPER_RESOURCES */

/* [PM] 3.8.5 Glue code includes this and defines JASPER_GLUE */
/* [PM] 3.8.5 jasper.c  includes this and defines JASPER_SOURCE */


/* [PM] No longer used by jasper.c */
/* #if JASPER_GLUE || JASPER_SOURCE */
#if JASPER_GLUE

#if !JASPER_RESOURCES
#error "Java foreign resources not supported. (Use jasper_call/4)"
#endif

#define JGLUEERR_CLASS 1
#define JGLUEERR_METHOD 2


/* err_msg can be NULL */
static void SPJavaGlueError(JNIEnv *jnienv, int argnum /*IGNORED*/, const char *err_msg)
{
#if JASPER_DBG+0 > 0
  fprintf(stderr, "SPJavaGlueError: arg# %d, `%s'\n", argnum, ((err_msg != NULL) ? err_msg : "<<NULL>>"));
#endif
  if (jnienv && (*jnienv)->ExceptionCheck(jnienv))
    {
      (*jnienv)->ExceptionDescribe(jnienv); /* questionable */
      (*jnienv)->ExceptionClear(jnienv);
    }
  {
    SP_term_ref excp_term = SP_new_term_ref();
    SP_put_string(excp_term, ((err_msg != NULL) ? err_msg : "Error in Java glue"));
    SP_raise_exception(excp_term);
  }
}

/* 0 if there was no java exception to propagate.
   err_msg can be NULL
 */
static int SPJavaGluePropagateJavaException(JNIEnv *jnienv, int argnum /*IGNORED*/, const char *err_msg /*IGNORED*/)
{
  jobject java_excp = NULL;

  
  java_excp = (*jnienv)->ExceptionOccurred(jnienv);
  if (java_excp)
    {
      SP_term_ref excp_term = SP_new_term_ref();
      SP_term_ref excp_obj = SP_new_term_ref();

#if JASPER_DBG+0 > 0
      fprintf(stderr, "SPJavaGluePropagateJavaException: arg# %d, `%s'\n", argnum, err_msg);
#endif

      SP_put_integer(excp_obj, (SP_integer)(*jnienv)->NewGlobalRef(jnienv,java_excp));
      SP_cons_functor(excp_term, SP_atom_from_string("$java_object"), 1, excp_obj);
      SP_raise_exception(excp_term);
      (*jnienv)->ExceptionDescribe(jnienv); /* questionable */
      (*jnienv)->ExceptionClear(jnienv); /* not needed since ExceptionDescribe clears it */
    }

  if (java_excp)
    {
      (*jnienv)->DeleteLocalRef(jnienv, java_excp);
      return 1;
    }
  return 0;
}

/* NULL on error (in which case SPJavaGlueError has been called) */
static JNIEnv *SPJavaGlueGetJNIEnv(void)
{
  JNIEnv *jnienv = NULL;

  if (-1 == jasperi_initialize_C(0,-1,-1))
    {
      SPJavaGlueError(jnienv, -1, "could not initialize/attach JVM");
      return NULL;
    }
  if (!(jnienv = jasperi_get_jnienv(-42)))
    {
      SPJavaGlueError(jnienv, -1, "internal error: not attached to JVM");
      return NULL;
    }
  return jnienv;
}


/* Return 0 on failure (after calling SPJavaGlueError) */
static SP_uinteger SPJavaGlueLeaveContextMonitor(JNIEnv *jnienv, SP_uinteger context)
{
  /* no-op */
  return 1;
}


static SP_uinteger SPJavaGlueEnterContextMonitor(JNIEnv *jnienv, SP_uinteger context)
{
  /* no-op */

  return 1;
}



/* Return 0 on failure (after calling SPJavaGlueError) */
static SP_uinteger SPJavaGluePushContext(JNIEnv *jnienv)
{
  SP_uinteger context;

  context = jasper_glue_push_context(jnienv);
  
  if (!context) SPJavaGlueError(jnienv, -1, "Could not push SICStus context");
  
  return context;
}

/* Return 0 on failure (after calling SPJavaGlueError) */
static SP_uinteger SPJavaGluePopContext(JNIEnv *jnienv, SP_uinteger context)
{
  /* Prolog run-time monitor is *not* owned by this thread */
  SP_uinteger new_context;
  
  new_context = jasper_glue_pop_context(jnienv, context);
  /* Prolog run-time monitor *is* owned by this thread */

  if (!new_context) SPJavaGlueError(jnienv, -1, "Could not pop SICStus context");
  
  return new_context;
}



/* 0 on error (after callling SPJavaGlueError) */
static int SPJavaGlueGetNativeTermRef(JNIEnv *jnienv, jobject jobj, SP_term_ref *p_ref, int argnum, const char *err_msg)
{
  int rc = 0;
  jclass clazz = NULL;
  jmethodID methodID;
  jlong jterm_ref;

  JASPER_DBGLINE(SPJavaGlueGetNativeTermRef);
  clazz = (*jnienv)->GetObjectClass(jnienv, jobj);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRef);
  methodID = (*jnienv)->GetMethodID(jnienv, clazz, "GetNativeTermRef", "()J");
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRef);
  if (!methodID)
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRef);
      SPJavaGlueError(jnienv, argnum, err_msg);
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRef);
  jterm_ref = (*jnienv)->CallLongMethod(jnienv, jobj, methodID);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRef);
  if ((*jnienv)->ExceptionCheck(jnienv))
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRef);
      SPJavaGluePropagateJavaException(jnienv, argnum, err_msg);
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRef);
  rc = 1;                       /* success */
  *p_ref = (SP_term_ref) jterm_ref;

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRef);
  return rc;
}

/* called in Java context so may not call any prolog routines. Returns 0 on error without reporting error. */
static int SPJavaGlueGetNativeTermRefInJavaContext(JNIEnv *jnienv, jobject jobj, SP_term_ref *p_ref)
{
  int rc = 0;
  jclass clazz = NULL;
  jmethodID methodID;
  jlong jterm_ref;

  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContext);
  clazz = (*jnienv)->GetObjectClass(jnienv, jobj);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContext);
  methodID = (*jnienv)->GetMethodID(jnienv, clazz, "GetNativeTermRef", "()J");
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContext);
  if (!methodID)
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContext);
      /* would call prolog run-time: SPJavaGlueError(jnienv, argnum, err_msg); */
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContext);
  jterm_ref = (*jnienv)->CallLongMethod(jnienv, jobj, methodID);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContext);
  if ((*jnienv)->ExceptionCheck(jnienv))
    {
      JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContext);
      /* would call prolog run-time: SPJavaGluePropagateJavaException(jnienv, argnum, err_msg); */
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContext);
  rc = 1;                       /* success */
  *p_ref = (SP_term_ref) jterm_ref;

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  JASPER_DBGLINE(SPJavaGlueGetNativeTermRefInJavaContext);
  return rc;
}

static jmethodID SPJavaGlueGetObjectMethodID(JNIEnv *jnienv, jobject jobj, const char *methodName, const char *typesig, int argnum, const char *err_msg)
{
  jclass clazz = NULL;
  jmethodID methodID = 0;

  JASPER_DBGLINE(SPJavaGlueGetObjectMethodID);
  
  clazz = (*jnienv)->GetObjectClass(jnienv, jobj);
  JASPER_DBGLINE(SPJavaGlueGetObjectMethodID);

  methodID = (*jnienv)->GetMethodID(jnienv, clazz, methodName, typesig);
  JASPER_DBGLINE(SPJavaGlueGetObjectMethodID);
  if (!methodID)
    {
      JASPER_DBGLINE(SPJavaGlueGetObjectMethodID);
      SPJavaGlueError(jnienv, argnum, err_msg);
      goto cleanup;
    }

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  return methodID;
}

static jmethodID SPJavaGlueGetStaticMethodID(JNIEnv *jnienv, jclass clazz, const char *methodName, const char *typesig, int argnum, const char *err_msg)
{
  jmethodID methodID = 0;
  
  methodID = (*jnienv)->GetStaticMethodID(jnienv, clazz, methodName, typesig);
  if (!methodID)
    {
      SPJavaGlueError(jnienv, argnum, err_msg);
      goto cleanup;
    }

 cleanup:
  return methodID;
}

#if 1 /* NULL_OBJREF */
static int SPJavaGlueGetObject(JNIEnv *jnienv, SP_term_ref tobj, jobject *pjobject, int argnum, const char *err_msg)
{
  SP_integer obj_ptr;
  int rc;
  jobject jobj;

  if (!( SP_get_arg(1, tobj, tobj)
         && SP_get_integer(tobj,&obj_ptr) ))
    {
      rc = 0;
      SPJavaGlueError(jnienv, argnum, err_msg);
      jobj = NULL;
    }
  else
    {
      rc = 1;
      jobj = (obj_ptr ? (jobject) obj_ptr : NULL );
    }
  *pjobject = jobj;
  return rc;
}
#else  /* !NULL_OBJREF */
static jobject SPJavaGlueGetObject(JNIEnv *jnienv, SP_term_ref tobj, int argnum, const char *err_msg)
{
  SP_integer obj_ptr;

  if (!( SP_get_arg(1, tobj, tobj)
         && SP_get_integer(tobj,&obj_ptr) ))
    {
      SPJavaGlueError(jnienv, argnum, err_msg);
      return NULL;
    }
  return (jobject) obj_ptr;
}
#endif

/* return 0 on failure (will call SPJavaGlueError) */
static int SPJavaGluePutObject(JNIEnv *jnienv, SP_term_ref tobj, jobject obj, int argnum, const char *err_msg)
{
  if ( SP_put_integer(tobj,(obj ? (SP_integer)obj : 0))
       && SP_cons_functor(tobj,SP_atom_from_string("$java_object"),1,tobj))
    {
      return 1;
    }
  /* unlikely */
  SPJavaGlueError(jnienv, argnum, err_msg);
  return 0;
}

#if !MULTI_SP
/* This cannot be used in Multi-SP since it does not specify SICStus object */
static jobject SPJavaGlueSPTerm(JNIEnv *jnienv, SP_term_ref term, int argnum, const char *err_msg, int init)
{
  const char *classname = "se/sics/jasper/SPTerm";
  jclass clazz = NULL;
  jmethodID initMethodID;
  jobject obj = NULL;
  const char *typesig = (init ? "(J)V" : "()V");
  JASPER_DBGLINE(SPJavaGlueSPTerm);

#if JASPER_DBG
  fprintf(stderr, "SPJavaGlueSPTerm %s %s term=%d \n", typesig, (init ? "init from" : "ignored"), term);
#endif

  clazz = (*jnienv)->FindClass(jnienv, classname);
  if (clazz == 0)
    {
      SPJavaGlueError(jnienv, argnum, "Cannot find class se/sics/jasper/SPTerm");
      goto cleanup;
    }
  initMethodID = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", typesig);
  if (initMethodID == 0)
    {
      SPJavaGlueError(jnienv, argnum, "Cannot find constructor for class se/sics/jasper/SPTerm");
      goto cleanup;
    }
  if (init)
    {
      obj = (*jnienv)->NewObject(jnienv, clazz, initMethodID, (jlong)term);
    }
  else
    {
      obj = (*jnienv)->NewObject(jnienv, clazz, initMethodID);
    }
  if (obj == NULL)
    {
      SPJavaGlueError(jnienv, argnum, "Cannot create object of class se/sics/jasper/SPTerm");
      goto cleanup;
    }
 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  return obj;
}
#endif /* !MULTI_SP */

#if !MULTI_SP
/* This cannot be used in Multi-SP since it does not specify SICStus object */
static jobject SPJavaGlueSPCanonicalAtom(JNIEnv *jnienv, SP_uinteger atom, int argnum)
{
  const char *classname = "se/sics/jasper/SPCanonicalAtom";
  jclass clazz = NULL;
  jmethodID initMethodID;
  jobject obj = NULL;

  JASPER_DBGLINE(SPJavaGlueSPCanonicalAtom);
  clazz = (*jnienv)->FindClass(jnienv, classname);
  JASPER_DBGLINE(SPJavaGlueSPCanonicalAtom); 
  if (clazz == 0)
    {
      SPJavaGlueError(jnienv, argnum, "Cannot find class se/sics/jasper/SPCanonicalAtom");
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueSPCanonicalAtom);
  initMethodID = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "(J)V");
  JASPER_DBGLINE(SPJavaGlueSPCanonicalAtom);
  if (initMethodID == 0)
    {
      SPJavaGlueError(jnienv, argnum, "Cannot find constructor for class se/sics/jasper/SPCanonicalAtom");
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueSPCanonicalAtom);
  obj = (*jnienv)->NewObject(jnienv, clazz, initMethodID, (jlong)atom);
  JASPER_DBGLINE(SPJavaGlueSPCanonicalAtom);
  if (obj == NULL)
    {
      SPJavaGlueError(jnienv, argnum, "Cannot create object of class se/sics/jasper/SPCanonicalAtom");
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGlueSPCanonicalAtom);

 cleanup:
  JASPER_DBGLINE(SPJavaGlueSPCanonicalAtom);
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  return obj;
}
#endif /* !MULTI_SP */

/* return 0 on failure (will call SPJavaGlueError) */
static int SPJavaGlueGetAtom(JNIEnv *jnienv, jobject obj /* SPCanonicalAtom or SPTerm */, SP_uinteger *p_atom, int argnum, const char *err_msg)
{
  int rc = 0;
  const char *methodName = "getAtom";
  const char *typesig = "()J";
  jmethodID methodID;
  jclass clazz = NULL;

  clazz = (*jnienv)->GetObjectClass(jnienv, obj);
  methodID = (*jnienv)->GetMethodID(jnienv, clazz, methodName, typesig);
  if (methodID == 0)
    {
      SPJavaGlueError(jnienv, argnum, err_msg);
      goto cleanup;
    }
  *p_atom = (SP_uinteger) (*jnienv)->CallLongMethod(jnienv, obj, methodID);
  if (SPJavaGluePropagateJavaException(jnienv, argnum, err_msg)) goto cleanup;

  rc = 1;

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  return rc;
}
  
  
static jboolean SPJavaGlueAtomToBoolean(JNIEnv *jnienv, SP_uinteger atom)
{
  return ( (atom == SP_atom_from_string("true")) ? JNI_TRUE : JNI_FALSE );
}


/* return a new local ref to a String. Does not delete obj.
   Typical use:
   {
      jstring tmp = SPJavaGluePostToString(jnienv, obj, 42);
      (*jnienv)->DeleteLocalRef(jnienv, obj);
      obj = tmp;
   }
*/
static jstring SPJavaGluePostToString(JNIEnv *jnienv, jobject obj, int argnum)
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
      SPJavaGlueError(jnienv, argnum, "Postconversion failed"); /* NOTE: better error message needed */
      goto cleanup;
    }
  string_obj = (jstring)(*jnienv)->CallObjectMethod(jnienv, obj, methodID);

 cleanup:
  if (clazz) (*jnienv)->DeleteLocalRef(jnienv, clazz);
  return string_obj;
}


/* return NULL on error (will call SPJavaGlueError) */
static jobject SPJavaGlueStringBuffer(JNIEnv *jnienv, int argnum)
{
  jclass clazz = NULL;
  jmethodID methodID;
  jobject stringBuffer = NULL;
  
  clazz = (*jnienv)->FindClass(jnienv, "java/lang/StringBuffer");
  if (clazz==NULL)
    {
      SPJavaGlueError(jnienv, argnum, "Cannot find class java/lang/StringBuffer for -atom or -string");
      goto cleanup;
    }

   methodID = (*jnienv)->GetMethodID(jnienv, clazz, "<init>", "()V");
   if (methodID == 0)
     {
       SPJavaGlueError(jnienv, argnum, "Cannot find constructor for class `java/lang/StringBuffer'");
       goto cleanup;
     }
   stringBuffer = (*jnienv)->NewObject(jnienv, clazz, methodID);
   if (stringBuffer == NULL)
     {
       SPJavaGlueError(jnienv, argnum, "Cannot create object of class java/lang/StringBuffer");
       goto cleanup;
     }
 cleanup:
   (*jnienv)->DeleteLocalRef(jnienv, clazz); /* safe to call with NULL */
   return stringBuffer;
}


/* return 0 on fail or error (will call SPJavaGlueError) */
static int SPJavaGluePostPutCodes(JNIEnv *jnienv, jstring string_obj, SP_term_ref tstring, int argnum)
{
  int rc = 0;
  const char *codes = NULL;

  JASPER_DBGLINE(SPJavaGluePostPutCodes);
  codes = (*jnienv)->GetStringUTFCodes(jnienv, string_obj, NULL);

#if JASPER_DBG
  fprintf(stderr, "SPJavaGluePostPutCodes \"%s\"\n", (codes ? codes : "<<ERROR: codes==NULL!! >>"));
#endif
  JASPER_DBGLINE(SPJavaGluePostPutCodes);
  if((!codes)
     || (!SP_put_list_codes(tstring, SP_new_term_ref(), (char *)codes)))
    {
      JASPER_DBGLINE(SPJavaGluePostPutCodes);
      SPJavaGlueError(jnienv, argnum, "Cannot make -string or [-string]");
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGluePostPutCodes);
  rc = 1;
  
 cleanup:
  JASPER_DBGLINE(SPJavaGluePostPutCodes);
  if (codes) 
    {
      JASPER_DBGLINE(SPJavaGluePostPutCodes);
      (*jnienv)->ReleaseStringUTFCodes(jnienv, string_obj, codes);
    }

  return rc;
}

/* return 0 on fail or error (will call SPJavaGlueError) */
static int SPJavaGluePostPutStr(JNIEnv *jnienv, jstring string_obj, SP_term_ref tstring, int argnum)
{
  int rc = 0;
  const char *codes = NULL;
  JASPER_DBGLINE(SPJavaGluePostPutStr);
  codes = (*jnienv)->GetStringUTFCodes(jnienv, string_obj, NULL);
  JASPER_DBGLINE(SPJavaGluePostPutStr);
  if((!codes)
     || (!SP_put_string(tstring, codes)))
    {
      JASPER_DBGLINE(SPJavaGluePostPutStr);
      SPJavaGlueError(jnienv, argnum, "Cannot make -string or [-string]");
      goto cleanup;
    }
  JASPER_DBGLINE(SPJavaGluePostPutStr);
  rc = 1;
  
 cleanup:
  JASPER_DBGLINE(SPJavaGluePostPutStr);
  if (codes)
    {
      JASPER_DBGLINE(SPJavaGluePostPutStr);
      (*jnienv)->ReleaseStringUTFCodes(jnienv, string_obj, codes);
    }

  return rc;
}

#endif /* JASPER_GLUE */


#endif /* INCLUDED_JASPER_H_ */

