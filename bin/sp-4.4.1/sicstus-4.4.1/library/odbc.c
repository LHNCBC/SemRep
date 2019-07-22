/* Copyright(C) 1999, Swedish Institute of Computer Science */
/*
ODBC implementation links
"Explanation of Length Arguments for Unicode ODBC Functions" http://support.microsoft.com/kb/294169

"Unicode Function Arguments" http://msdn.microsoft.com/en-us/library/windows/desktop/ms716246(v=vs.85).aspx

"UNDERSTANDING UNICODE AND ODBC DATA ACCESS" http://www.datadirect.com/resources/odbc/unicode/index.html

"Data Management: SQL Call Level Interface (CLI)" http://pubs.opengroup.org/onlinepubs/009654899/toc.pdf

PosgreSQL: http://flylib.com/books/en/2.290.1.126/1/ll

Some information about the Unix SQLWCHAR mess: http://www.freetds.org/userguide/uodbc.htm

The Windows Platform SDK odbc test application, odbct32w.exe.

The horrors of Unicode and MySQL ODBC:
http://word.wardosworld.com/?p=164 (at a minimum, configure the driver
to use the character set utf8 (or pass 'Charset=UTF8' as part of the
DSN). MySQL 5.5 and later supports more of Unicode.
 */

#define SICSTUS_UNHIDE_SP_get_encoding 1
#define SICSTUS_UNHIDE_SP_encoding_open 1
#define SICSTUS_UNHIDE_SP_encoding_release 1
#define SICSTUS_UNHIDE_SP_encode_from_codes 1
#define SICSTUS_UNHIDE_SP_encode_to_codes 1
#define SICSTUS_UNHIDE_SP_codes_to_multi_bytes 1
#include <sicstus/config.h>     /* ([PM] 4.2.1 this also defines SICSTUS_DBG) */
#include <sicstus/sicstus.h>

#if SP_WIN32
#include <windows.h>
#endif

#if SP_DARWIN
#ifndef SP_ODBC_SUPPRESS_DEPRECATION_WARNINGS
#define SP_ODBC_SUPPRESS_DEPRECATION_WARNINGS 1
#endif /* SP_ODBC_SUPPRESS_DEPRECATION_WARNINGS */
#endif /* SP_DARWIN */

#if SP_ODBC_SUPPRESS_DEPRECATION_WARNINGS
#if defined(__GNUC__) && ((__GNUC__ >= 4))
/* [PM] 4.2.3 sql.h in OS X 10.8 deprecates all functions */
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif  /* GCC >= 4 */
#endif /* SP_ODBC_SUPPRESS_DEPRECATION_WARNINGS */

/*
  [PM] 4.2.2 Unless SQL_NOUNICODEMAP is defined, sqlucode.h in
  unixODBC and iodbc remaps SQLColAttribute to SQLColAttributeW etc if
  UNICODE is defined.  We do not want that to happen, ever. (Even
  though we do not define UNICODE currently).  */

#define SQL_NOUNICODEMAP [PM] 4.2.2 we assume the specific value does not m A tT 3 r

#ifndef HAVE_LONG_LONG
/* [PM] 4.2 Force UnixODBC to not use a struct when declaring
   SQLBIGINT. This is what Qt does so presumably safe despite the fact
   that we do not know whether the unixODBC driver uses long long or a
   (big-endian!?) struct (which could lead to endianness issues) */
#define HAVE_LONG_LONG 1
#endif

#if !defined(ODBC_USE_CODER_BUG_WORKAROUND)
#if SPIO_CODER_DO_NOT_OVERALLOCATE
/* [PM] 4.3 the bug has been fixed (was fixed in 4.3.0) */
#define ODBC_USE_CODER_BUG_WORKAROUND 0
#else /* !SPIO_CODER_DO_NOT_OVERALLOCATE */
#define ODBC_USE_CODER_BUG_WORKAROUND 1
#endif  /* !SPIO_CODER_DO_NOT_OVERALLOCATE */
#endif /* !defined(ODBC_USE_CODER_BUG_WORKAROUND) */

#if !defined ODBC_FORCE_BUILD
#if FORCE_BUILD || SICSTUS_BETA_VERSION || SICSTUS_VERSION <= 40401 /* [PM] 4.3.2 Revisit these issues again, if needed */
#define ODBC_FORCE_BUILD 1
#endif  /* SICSTUS_BETA_VERSION */
#endif /* !defined ODBC_FORCE_BUILD */


#if !defined POSTPONE
#if ODBC_FORCE_BUILD
#define POSTPONE 1
#endif  /* ODBC_FORCE_BUILD */
#endif  /* !defined POSTPONE */

#if !POSTPONE
/* [PM] 4.2.2 Perhaps we should drop the use of buffers and replace this with explict prolog-variants of SQLGetData? */
#error "We should update odbc.pl+odbc.c to know about the use of SQLGetData."
#endif  /* !POSTPONE */

#if !POSTPONE
/* The potential problem is if you "SELECT foo from bar where foo=(?)"
   and the parameter is the euro-symbol but the foo column is not a
   W-column. Instead we could convert to SQL_WCHAR and let the DB
   attempt to convert to SQL_CHAR (which may work depending on the
   charset used for SQL_CHAR by the DB.
*/
#error "Consider converting SQL_CHAR-parameters to SQL_WCHAR (or SQL_WVARCHAR or whatever to avoid padding issues) if non-ASCII."
#endif  /* !POSTPONE */

#if !POSTPONE
#error "[PM] 4.2.2 optimize ASCII (and Latin1?) case when converting to/from SQLWCHAR/IENC"
#endif  /* !POSTPONE */

#if !(UNICODE+0)
/* Some headers tests "#ifdef UNICODE" so it is important that it is not defined as zero. */
#undef UNICODE
#endif /* !UNICODE */
#if !(_UNICODE+0)
/* Some headers tests "#ifdef _UNICODE" so it is important that it is not defined as zero. */
#undef _UNICODE
#endif /* !UNICODE */
/* Ensure UNICODE and _UNICODE gets the same non-zero value, if any. */
#if UNICODE
#undef _UNICODE
#define _UNICODE UNICODE
#elif _UNICODE
#undef UNICODE
#define UNICODE _UNICODE
#endif  /* _UNICODE */

#include <sql.h>
#include <sqlext.h>

#if SP_WIN32
/* empty */ /* Windows ODBC */
#elif defined SQL_ATTR_UNIXODBC_SYSPATH /* defined by UnixODBC header sqlext.h */
#define SP_ODBC_UNIXODBC 1
#elif SP_DARWIN
#define SP_ODBC_IODBC 1
#else
#error "[PM] 4.2.1 Can not determine the ODBC implementation version"
#endif

#if SP_ODBC_IODBC
/* iODBC uses UTF-8 internally so it is pointless to convert to SQLWCHAR (iODBC uses UTF-16 for these). */

/* [PM] 4.3 Note: With the MySQL ODBC Connector the ANSI Driver should
   used on Mac OS, not the UNICODE driver:

   With MySQL ODBC 5.3.1beta Unicode driver Mac OS X 10.7.5 against TurnKey Linux MySQL Server 5.1:

   Creating the "foobar" "kaka" colum with the euro-symbol (0x20AC)
   works but when reading it back as a SQL_C_CHAR it gets
   garbled. Manually changing the TargetType to SQL_WCHAR gets back a
   wchar_t (int) with the correct value so, in this case, no UTF-8
   conversion seems to be happening.

   Instead using the ANSI driver makes everything work well (UTF-8 encoded).
 */

#if !defined(ENABLE_WCHAR)
#define ENABLE_WCHAR 0
#endif  /* !defined(ENABLE_WCHAR) */

#if !defined(SP_SQL_UNICODE)
#define SP_SQL_UNICODE 0
#endif /* !defined(SP_SQL_UNICODE) */

#endif  /* SP_ODBC_IODBC */

#if SP_ODBC_UNIXODBC
/* [PM] 4.2.2 Do not use the Unicode (...W) API with unixODBC. The ANSI API
   seems to work at least as well.

   On CentOS 6.2 the built-in Postgres ODBC is completely broken when
   using the Unicode (...W) API. Using the default "ANSI" API, with
   UTF-8 data, seems to work well, even with code points > 16bits.

   On CentOS 6.2 the built-in MySQL ODBC driver does not read SQL_BIT
   correctly but this is no worse with the ANSI API.
*/
#if !defined(ENABLE_WCHAR)
#define ENABLE_WCHAR 0
#endif  /* !defined(ENABLE_WCHAR) */

#if !defined(SP_SQL_UNICODE)
#define SP_SQL_UNICODE 0
#endif /* !defined(SP_SQL_UNICODE) */

#endif	/* SP_ODBC_UNIXODBC */

#if SP_ODBC_UNIXODBC
/* Uses UTF-16 */
#define SP_ODBC_SQLWCHAR_IS_UTF16 1 /* 2 bytes */
#define SP_ODBC_SQLWCHAR_IS_WCHAR_T 0
#elif SP_ODBC_IODBC
/* Uses UTF-32 (wchar_t) */
#define SP_ODBC_SQLWCHAR_IS_UTF32 1 /* 4 bytes */
#define SP_ODBC_SQLWCHAR_IS_WCHAR_T 1
#elif SP_WIN32
#define SP_ODBC_SQLWCHAR_IS_UTF16 1 /* 2 bytes */
#define SP_ODBC_SQLWCHAR_IS_WCHAR_T 0
#endif	/* SP_WIN32 */


#include "odbc_glue.h"
#define BARF SPIO_BARF
#define CHECK SPIO_CHECK
#define SPIO_BARF_LABEL barf
#define NULL_CHECK SPIO_NULL_CHECK
#define POSIX_CHECK SPIO_POSIX_CHECK
#define BARF_LAST_ERROR SPIO_BARF_LAST_ERROR
#define HANDLE_CHECK SPIO_HANDLE_CHECK
#define CHECK_LAST_ERROR SPIO_CHECK_LAST_ERROR

/* ODBC_XXX macros sets the SQLRETURN rc to an SQL error (or an
   ODBC_ERROR_...) and goto barf. This is similar to the corresponding
   SPIO_xxx macros (which sets the spio_t_error_code code). */

/* Set rc an goto barf. RC must be a failure code, i.e. !SQL_SUCCEEDED(RC) must hold. */
#define ODBC_BARF(RC) do{                       \
      rc = (RC);                                \
      SP_ASSERT(!SQL_SUCCEEDED(rc));            \
      goto barf;                                \
   } while(0)

#define ODBC_NULL_CHECK(EXPR) do{               \
      if ((EXPR) == NULL) {                     \
         ODBC_BARF(ODBC_ERROR_OUT_OF_MEMORY);   \
      }                                         \
   } while(0)

#define ODBC_HANDLE_CHECK_(EXPR, NULL_VALUE) do{        \
    if ((EXPR) == (NULL_VALUE)) {                       \
         ODBC_BARF(ODBC_ERROR_INVALID_HANDLE);          \
      }                                                 \
   } while(0)


#define ODBC_HSTMT_CHECK(EXPR) ODBC_HANDLE_CHECK_((EXPR), SQL_NULL_HSTMT)
#define ODBC_HDBC_CHECK(EXPR) ODBC_HANDLE_CHECK_((EXPR), SQL_NULL_HDBC)
#define ODBC_HENV_CHECK(EXPR) ODBC_HANDLE_CHECK_((EXPR), SQL_NULL_HENV)
/* Always sets rc */
#define ODBC_CHECK(RC) do{                      \
      SQLRETURN rc_tmp_ = (RC);                 \
      if (!SQL_SUCCEEDED(rc_tmp_)) {            \
         ODBC_BARF(rc_tmp_);                    \
      } else {                                  \
         rc = rc_tmp_;                          \
      }                                         \
   } while(0)


#if !defined CONNECTION_HANDLE_FLAG_UTF8_CHAR_DEFAULT
#if SP_WIN32
/* [PM] 4.2.2 On Windows we assume that ODBC strings (SQL_C_CHAR et
   al) is in Latin-1 regardless of locale (this is surely wrong but on
   Windows we should use the Unicode ODBC API so the value of this
   option should not matter). */
#define CONNECTION_HANDLE_FLAG_UTF8_CHAR_DEFAULT 0
#else /* !SP_WIN32 */
/* [PM] 4.2.2 On non-Windows we assume that the ODBC strings (SQL_C_CHAR et al) is in UTF-8 encoding regardless of locale. */
#define CONNECTION_HANDLE_FLAG_UTF8_CHAR_DEFAULT 1
#endif /* !SP_WIN32 */
#endif /* !defined CONNECTION_HANDLE_FLAG_UTF8_CHAR_DEFAULT */

#if !defined ODBC_PREFER_SQLGETDIAGREC
/* [PM] 4.2.2 SQLGetDiagField was unreliable with SQL_DIAG_SQLSTATE
   (PostgreSQL, psqlODBC, Mac OS X 10.7 64-bit).
   It should be OK to use SQLGetDiagRec instead, on all platforms.
*/
#define ODBC_PREFER_SQLGETDIAGREC 1
#endif  /* !defined ODBC_PREFER_SQLGETDIAGREC */

#if !defined SP_SQL_UNICODE
#define SP_SQL_UNICODE 1
#endif  /* !defined SP_SQL_UNICODE */

#if !defined ENABLE_WCHAR
#define ENABLE_WCHAR 1
#endif /* !defined ENABLE_WCHAR */

#if !defined ODBC_BROKEN_SHORT
/* 
   [PM] 4.2.2 In some drivers SQL_C_SSHORT et al are written using, in
   effect, "*((long*)p) = value", i.e will always write 32-bits (seen
   in psqlodbc-08.03.0100 and earlier convert.c, not verified whether
   it really happens).
 */
#define ODBC_BROKEN_SHORT 1
#endif  /* !defined ODBC_BROKEN_SHORT */


#if SP_SQL_UNICODE
#define tchars_strnlen wchars_strnlen
#define tchars_streq wchars_streq
#define SP_SQLTCHAR SQLWCHAR
#define SP_SQLDataSourcesT SQLDataSourcesW
#define SP_SQLDriverConnectT SQLDriverConnectW
#define SP_SQLGetDiagRecT SQLGetDiagRecW
#define SP_SQLDescribeColT SQLDescribeColW
#define SP_SQLExecDirectT SQLExecDirectW
#define SP_SQLColumnsT SQLColumnsW
#define SP_SQLTablesT SQLTablesW
#define SP_SQLConnectT SQLConnectW
#define SP_SQLColAttributeT SQLColAttributeW
#define SP_SQLGetDiagFieldT SQLGetDiagFieldW
#else  /* !SP_SQL_UNICODE */
#define tchars_strnlen strnlen
#define tchars_streq(A,B) (strcmp((A),(B))==0)
#define SP_SQLTCHAR SQLCHAR
#define SP_SQLDataSourcesT SQLDataSourcesA
#define SP_SQLDriverConnectT SQLDriverConnectA
#define SP_SQLGetDiagRecT SQLGetDiagRecA
#define SP_SQLDescribeColT SQLDescribeColA
#define SP_SQLExecDirectT SQLExecDirectA
#define SP_SQLColumnsT SQLColumnsA
#define SP_SQLTablesT SQLTablesA
#define SP_SQLConnectT SQLConnectA
#define SP_SQLColAttributeT SQLColAttributeA
#define SP_SQLGetDiagFieldT SQLGetDiagFieldA
#endif  /* !SP_SQL_UNICODE*/

#if SP_BIGENDIAN
#define NATIVE_UTF16_NAME "UTF-16BE"
#else /* !SP_BIGENDIAN */
#define NATIVE_UTF16_NAME "UTF-16LE"
#endif	/* !SP_BIGENDIAN */


#ifndef ODBC_DEBUG
#define ODBC_DEBUG 0
#endif

#if ODBC_DEBUG
#include <stdio.h>              /* DEBUG */

/* SPRIdSZ format size_t as decimal. */
#if !defined(SPRIdSZ) /* [PM] 4.2.2 now in sicstus.h */
#if SP_WIN32
#define SPRIdSZ "Id"
#else /* !SP_WIN32 */
/* POSIX standard */
#define SPRIdSZ "zd"
#endif /* !SP_WIN32 */
#endif /* !defined(SPRIdSZ) */

#endif /* ODBC_DEBUG */

#ifndef SP_SOFT_ASSERT
/* [PM] 4.2 SP_SOFT_ASSERT is like SP_ASSERT for unexpected, but not
   impossible/illegal conditions, i.e. it is a no-op in non-debug
   builds. */
#if (ODBC_DEBUG || SICSTUS_DBG)
#define SP_SOFT_ASSERT(TEST) SP_ASSERT((TEST))
#else  /* !SICSTUS_DBG */
#define SP_SOFT_ASSERT(_) do { ; } while (0)
#endif  /* !SICSTUS_DBG */
#endif  /* SP_SOFT_ASSERT */

/* [PM] 4.2 Net wisdom has it that some ODBC implementations do not have the CLI SQL_SUCCEEDED macro.  */
#ifndef SQL_SUCCEEDED
#if (ODBC_DEBUG || SICSTUS_DBG || !SICSTUS_RELEASE_BUILD) && !ODBC_FORCE_BUILD
#error "[PM] 4.2 expected SQL_SUCCEEDED to be defined"
#endif  /* ODBC_DEBUG */
static int odbc_sql_succeeded(SQLRETURN rc)
{
  if (rc == SQL_SUCCESS)
    {
      return 1;
    }
  if (rc == SQL_SUCCESS_WITH_INFO)
    {
      return 1;
    }
  return 0;
}
/* [PM] 4.2 It is important that this evaluates its argument exactly once */
#define SQL_SUCCEEDED(RC) odbc_sql_succeeded((RC))
#endif  /* SQL_SUCCEEDED */

#if SICSTUS_DBG>1 || ODBC_DEBUG
#define DEBUG_PRINT(args)                       \
  do {                                          \
    fprintf args;                               \
    fflush(stderr);                             \
  } while(0)
#define DEBUG_PRINT0(format)                    \
  do {                                          \
    fprintf(stderr, (format));                  \
    fflush(stderr);                             \
  } while(0)
#define DEBUG_PRINT1(format,arg1)               \
  do {                                          \
    fprintf(stderr, (format), (arg1));          \
    fflush(stderr);                             \
  } while(0)
#define DEBUG_PRINT2(format,arg1,arg2)          \
  do {                                          \
    fprintf(stderr, (format), (arg1), (arg2));  \
    fflush(stderr);                             \
  } while(0)

#else  /* if !(SICSTUS_DBG>1 || ODBC_DEBUG) */
#define DEBUG_PRINT(args)      /* empty */
#define DEBUG_PRINT0(format)       /* empty */
#define DEBUG_PRINT1(format,arg1)       /* empty */
#define DEBUG_PRINT2(format,arg1,arg2)       /* empty */
#endif /* !(SICSTUS_DBG>1 || ODBC_DEBUG) */

#if !defined FROM_SQLWCHAR_ASCII_FAST_PATH
#define FROM_SQLWCHAR_ASCII_FAST_PATH 1
#endif  /* ! define FROM_SQLWCHAR_ASCII_FAST_PATH */

#if MULTI_SP_AWARE
#error "[PM] 4.2 this code is not mult-sp-aware"
#endif  /* MULTI_SP_AWARE */

#define MY_CATENATE_SYMBOLS_(X,Y) X ## Y
#define MY_CATENATE_SYMBOLS(X,Y) MY_CATENATE_SYMBOLS_(X,Y)
#define COMPILE_TIME_ASSERT_DECLARATION(COND) extern int MY_CATENATE_SYMBOLS(boobytrap_on_line_, __LINE__)[(COND) ? 1 : -1]
#define COMPILE_TIME_ASSERT(COND) { int MY_CATENATE_SYMBOLS(boobytrap_on_line_, __LINE__)[(COND) ? 1 : -1]; (void)MY_CATENATE_SYMBOLS(boobytrap_on_line_, __LINE__); }

#define MY_MIN(X,Y) (((X) < (Y)) ? (X) : (Y))

#define MIN_SQL_ERROR MY_MIN(SQL_SUCCESS,\
                      MY_MIN(SQL_SUCCESS_WITH_INFO,\
                      MY_MIN(SQL_STILL_EXECUTING, \
                      MY_MIN(SQL_ERROR, \
                      MY_MIN(SQL_INVALID_HANDLE, \
                      MY_MIN(SQL_NEED_DATA, \
                             SQL_NO_DATA \
                      ))))))
#define ODBC_ERROR_BASE MY_MIN(MIN_SQL_ERROR, -1000)
/* [PM] 4.2 The exact value of ODBC_ERROR_BASE is not really critical
   but sql headers must have very unusal values for this assertion to
   trigger (last I looked, all headers had all predefined SQL errors
   >= -2). */
COMPILE_TIME_ASSERT_DECLARATION(ODBC_ERROR_BASE == -1000);
#define FIRST_ODBC_ERROR_ ODBC_ERROR_1_
#define ODBC_ERROR_1_    (ODBC_ERROR_BASE - 1)
#define ODBC_ERROR_2_    (ODBC_ERROR_BASE - 2)
#define ODBC_ERROR_3_    (ODBC_ERROR_BASE - 3)
#define ODBC_ERROR_4_    (ODBC_ERROR_BASE - 4)
#define ODBC_ERROR_5_    (ODBC_ERROR_BASE - 5)
#define ODBC_ERROR_6_    (ODBC_ERROR_BASE - 6)
#define ODBC_ERROR_7_    (ODBC_ERROR_BASE - 7)
#define ODBC_ERROR_8_    (ODBC_ERROR_BASE - 8)
#define ODBC_ERROR_9_    (ODBC_ERROR_BASE - 9)

/* [PM] 4.2 These indicate errors from the odbc foreign resource, not from the ODBC driver.
   
   ### NOTE: YOU MUST EXTEND sql_error(), and macro DO_ATOMS, IF YOU ADD TO THIS LIST!

*/
#define ODBC_ERROR_OUT_OF_MEMORY   ODBC_ERROR_1_
#define ODBC_ERROR_CREATING_HANDLE ODBC_ERROR_2_
#define ODBC_ERROR_INVALID_HANDLE  ODBC_ERROR_3_
#define ODBC_ERROR_INVALID_PARAMETER ODBC_ERROR_4_
#define ODBC_ERROR_DATA_CONVERSION ODBC_ERROR_5_
#define ODBC_ERROR_UNSUPPORTED_DATA_TYPE ODBC_ERROR_6_
#define ODBC_ERROR_UNKNOWN_DATA_TYPE ODBC_ERROR_7_
#define ODBC_ERROR_TYPE_ERROR ODBC_ERROR_8_
#define ODBC_ERROR_IMPOSSIBLE_ERROR ODBC_ERROR_9_
#define LAST_ODBC_ERROR_ ODBC_ERROR_9_

#define IS_ODBC_ERROR(RC) (FIRST_ODBC_ERROR_ <= (RC) && (RC) <= LAST_ODBC_ERROR_)

/* [PM] 4.2 buffer-related result codes. Result must be positive iff successful. xref odbc:buffer_check_return/2 */
#define PROLOG_RESULT_NOERR                  1 /* Success */
#define PROLOG_RESULT_DATA_CONVERSION_FAILED 0 /* Data conversion failed */
#define PROLOG_RESULT_UNSUPPORTED_DATA_TYPE -1 /* SQL data type is unsupported */
#define PROLOG_RESULT_UNKNOWN_DATATYPE      -2 /* SQL data type is unknown */
#define PROLOG_RESULT_TYPE_ERROR            -3 /* Prolog data type is incompatible with the SQL data type */
#define PROLOG_RESULT_INVALID_HANDLE        -4 /* Statement handle (-index) is invalid */
#define PROLOG_RESULT_INVALID_BUFFER        -5 /* Buffer (-index) is invalid */
#define PROLOG_RESULT_INVALID_BUFFER_SIZE   -6 /* Input buffer size does not match allocated size */

/* [PM] 4.2.2 other errors */

#define PROLOG_RESULT_IMPOSSIBLE_ERROR      -19 /* "Cannot happen" */
#define PROLOG_RESULT_OUT_OF_MEMORY         -20 /* Out of memory */


/* Pass to sp_check_return/2 */
#define PROLOG_NATIVE_CODE_SUCCESS           1
#define PROLOG_NATIVE_CODE_ERROR             0

#if SP_SQL_UNICODE
static SQLRETURN from_sqlwchars(SQLWCHAR const *wStr, char **ps);
#endif  /* SP_SQL_UNICODE */

#if ENABLE_WCHAR
static SQLRETURN from_n_sqlwchars(SQLWCHAR const *wStr, size_t cchWideChar, char **ps, size_t *psize);
#endif  /* ENABLE_WCHAR */

#if SP_SQL_UNICODE || ENABLE_WCHAR
static SQLRETURN to_sqlwchars(char const *s, SQLWCHAR **pWStr);
#endif  /* SP_SQL_UNICODE || ENABLE_WCHAR */

/*
  FROM_BUF_TO_IENC(TCHARS,IENC)

     Set IENC to, possibly allocated, (UTF-8) encoded version of the T
     chars. Success iff returnd SQL_SUCCESS

  FREE_STATIC_IENC(X)
     
     Free any allocated memory and set X to NULL

  FROM_IENC_TO_TCHARS(IENC_PTR, TCHARS_PTR)

     Set TCHARS_PTR to, possibly allocated, T char version of the
     IENC_PTR. Success iff returnd SQL_SUCCESS. The memory should be
     reclaimed on cleanup with a corresponding call to
     FROM_IENC_TO_TCHARS(IENC_PTR, TCHARS_PTR)

  FREE_IENC_TO_TCHARS(IENC_PTR, TCHARS_PTR)
  
     Free any allocated memory and set TCHARS_PTR to
     NULL. Precondition: TCHARS_PTR is NULL or
     FROM_IENC_TO_TCHARS(IENC_PTR, TCHARS_PTR) has been called.

 */
#if SP_SQL_UNICODE
#define FROM_BUF_TO_IENC(TCHARS, IENC_PTR) from_sqlwchars((TCHARS), &(IENC_PTR))

/* [PM] 4.2.2. The ienc pointers are odbc_malloc()-ed blocks and must be freed. */
#define FREE_STATIC_IENC(IENC_PTR) do {         \
    if ((IENC_PTR) != NULL) {                   \
      odbc_free((IENC_PTR));                    \
      (IENC_PTR) = NULL;                        \
    }                                           \
  } while(0)

#define FROM_IENC_TO_TCHARS(IENC_PTR, TCHARS_PTR) to_sqlwchars((IENC_PTR), &(TCHARS_PTR))

#define FREE_IENC_TO_TCHARS(IENC_PTR, TCHARS_PTR) do {                  \
    if ((TCHARS_PTR) != NULL) {                                         \
        SP_ASSERT(((void*)(IENC_PTR)) != ((void*)(TCHARS_PTR)));        \
        odbc_free((TCHARS_PTR));                                        \
       (TCHARS_PTR) = NULL;                                             \
    }                                                                   \
  } while(0)

#else /* !SP_SQL_UNICODE */

/* [PM] 4.2.2. The ienc pointers are just pointers to the
   corresponding static buffer so should not be freed. */

#define FROM_BUF_TO_IENC(TCHARS, IENC_PTR) (((IENC_PTR) = (/* cast to avoid signedness mismatch */ char *) &((TCHARS)[0])), (SQLRETURN)SQL_SUCCESS)

#define FREE_STATIC_IENC(IENC_PTR) do {         \
    (IENC_PTR) = NULL;                          \
  } while(0)

#define FROM_IENC_TO_TCHARS(IENC_PTR, TCHARS_PTR) (((TCHARS_PTR) = (/* cast to avoid signedness mismatch */ SP_SQLTCHAR *) (IENC_PTR)), (SQLRETURN)SQL_SUCCESS)

#define FREE_IENC_TO_TCHARS(IENC_PTR, TCHARS_PTR) do {          \
    if ((TCHARS_PTR) != NULL) {                                 \
      SP_ASSERT(((void*)(IENC_PTR)) == ((void*)(TCHARS_PTR)));  \
      (TCHARS_PTR) = NULL;                                      \
    }                                                           \
  } while(0)

#endif  /* !SP_SQL_UNICODE */

#if ENABLE_WCHAR
static size_t wchars_strlen(SQLWCHAR const *wStr)
{
  SQLWCHAR const *p;
  SP_ASSERT(wStr != NULL);
  for (p = wStr; *p != ((SQLWCHAR)0); p++)
    {
      ; /* empty */
    }
  return (size_t)(p-wStr);
}

static size_t wchars_strnlen(SQLWCHAR const *wStr, size_t n)
{
  size_t i;
  SP_ASSERT(wStr != NULL);
  for (i = 0; i < n && wStr[i] != ((SQLWCHAR)0); i++)
    {
      ; /* empty */
    }
  return i;
}
#endif  /* ENABLE_WCHAR */


#define IS_FLAG_SET(FLAGS, FLAG) (((FLAGS) & (FLAG)) != 0)
#define SET_FLAG(FLAGS, FLAG) do { (FLAGS) |= (FLAG); } while(0)
#define CLEAR_FLAG(FLAGS, FLAG) do { (FLAGS) &= ~(FLAG); } while(0)


/****************************************
Local data
****************************************/

struct buflist {
  void * buf;
  size_t size;
  struct buflist * next;
#define BUFLIST_FLAG_NULL 0x0001 /* Value is null. Implies buf==NULL. */
  unsigned int flags;
  SQLSMALLINT TargetType; /* [PM] 4.2 The TargetType (like SQL_C_SLONG, or SQL_C_DEFAULT if unknown) */
  SQLSMALLINT ColumnNumber;
};

struct statementhandle {
  SQLHSTMT real_handle; /* the handle we get from SQLAllocHandle(). [PM] 4.2 or SQL_NULL_HSTMT if invalid */
  SP_integer  connection_handle_index;   /* [PM] 4.2 the owning connection, or SQL_NULL_HDBC */
  int is_definitely_unbound; /* [PM] 4.2 whether it is definitiely not bound to any buffers. We allow false positive. */
  struct buflist * buffers;     /* linked list of buffers */
};

struct environmenthandle {
  SQLHENV real_handle; /* the handle we get from SQLAllocHandle() or SQL_NULL_HENV if invalid/unused */
};

struct connectionhandle {
  SQLHDBC real_handle; /* the handle we get from SQLAllocHandle() or SQL_NULL_HDBC if invalid/unused */
  /* int is_connected; */
#define CONNECTION_HANDLE_FLAG_NONE      0x0000
#define CONNECTION_HANDLE_FLAG_CONNECTED 0x0001
  /* Assume SQL_C_CHAR et al use UTF-8 encoding (i.e. POSIX systems). */
#define CONNECTION_HANDLE_FLAG_UTF8_CHAR 0x0002
  unsigned int flags;
  SP_integer environment_handle_index; /* The owning environment handle index */
};

#define STATEMENTHANDLEINITSTACKSIZE 10
#define STATEMENTHANDLEINCREMENTSTACKSIZE STATEMENTHANDLEINITSTACKSIZE

#define ENVIRONMENTHANDLEINITSTACKSIZE 5
#define ENVIRONMENTHANDLEINCREMENTSTACKSIZE ENVIRONMENTHANDLEINITSTACKSIZE

#define CONNECTIONHANDLEINITSTACKSIZE 5
#define CONNECTIONHANDLEINCREMENTSTACKSIZE CONNECTIONHANDLEINITSTACKSIZE

/* [PM] 4.2 ensure output buffers are sane even if SQL... do not set
   them, e.g. on error. This prevents SP from attempting to convert
   garbage into atoms etc (which is the likely cause of a reported
   crash (SPRM ?) on Windows).
  */
#define INIT_STATIC_BUFFER(BUF) do{             \
  (BUF)[0] = 0;                                 \
} while(0)

/* The number of elements of an array. Note: BUF must be a real array,
   i.e. not a pointer to first element, so that sizeof (BUF) does the
   right thing. */
#define STATIC_BUFFER_NUMBER_OF_ELEMENTS(BUF) ((sizeof (BUF))/ (sizeof (BUF)[0]))

/* Put 0 in last element of array buffer. Note: BUF must be a real
   array, i.e. not a pointer to first element, so that sizeof (BUF)
   does the right thing. */
#define NUL_TERMINATE_STATIC_BUFFER(BUF) do{                    \
    (BUF)[STATIC_BUFFER_NUMBER_OF_ELEMENTS((BUF))-1] = 0;       \
} while(0)


#define DO_ATOMS                                                        \
    do_atom(sql_success, "SQL_SUCCESS");                                \
    do_atom(sql_success_with_info, "SQL_SUCCESS_WITH_INFO");            \
    do_atom(sql_no_data, "SQL_NO_DATA");                                \
    do_atom(sql_error, "SQL_ERROR");                                    \
    do_atom(sql_invalid_handle, "SQL_INVALID_HANDLE");                  \
    do_atom(odbc_error_out_of_memory, "ODBC_ERROR_OUT_OF_MEMORY");      \
    do_atom(odbc_error_creating_handle, "ODBC_ERROR_CREATING_HANDLE");  \
    do_atom(odbc_error_invalid_handle, "ODBC_ERROR_INVALID_HANDLE");    \
    do_atom(odbc_error_invalid_parameter, "ODBC_ERROR_INVALID_PARAMETER"); \
    do_atom(odbc_error_data_conversion, "ODBC_ERROR_DATA_CONVERSION");  \
    do_atom(odbc_error_unsupported_data_type, "ODBC_ERROR_UNSUPPORTED_DATA_TYPE"); \
    do_atom(odbc_error_unknown_data_type, "ODBC_ERROR_UNKNOWN_DATA_TYPE"); \
    do_atom(odbc_error_type_error, "ODBC_ERROR_TYPE_ERROR");            \
    do_atom(odbc_error_impossible_error, "ODBC_ERROR_IMPOSSIBLE_ERROR");\
    do_atom(invalid_return_code, "invalid_return_code");                \
    do_atom(datatype_conversion_error, "datatype_conversion_error");    \
    /* date and time struct names */                                    \
    do_atom(date_name, "date");                                         \
    do_atom(time_name, "time");                                         \
    do_atom(timestamp_name, "timestamp");                               \
    do_atom(null, "null");                                              \
    do_atom(odbc_buffer, "odbc_buffer");                                \
    do_atom(statement_handle, "statement_handle");                      \
    do_atom(environment_handle, "environment_handle");                  \
    do_atom(connection_handle, "connection_handle")


struct odbc_state {
  /* ODBC return codes */
#define do_atom(var,name) SP_atom var
  DO_ATOMS;
#undef do_atom

  size_t stmthandle_stacksize;
  struct statementhandle * statementhandlestack;
  size_t statementhandle_tos;

  size_t environmenthandle_stacksize;
  struct environmenthandle * environmenthandlestack;
  size_t environmenthandle_tos;

  size_t connectionhandle_stacksize;
  struct connectionhandle * connectionhandlestack;
  size_t connectionhandle_tos;

  SQLLEN null_StrLen_or_Ind; /* Contains SQL_NULL_DATA. For use with SQL_PARAM_INPUT. */


#if ODBC_DEBUG
  SP_integer alloc_counter;
#endif
#if SP_ASSERTIONS
  SP_integer atom_counter;
#endif  /* SP_ASSERTIONS */

#define SERVER_NAME_BUFFER_LENGTH (SQL_MAX_DSN_LENGTH +1)
  SP_SQLTCHAR server_name_T_buffer[SERVER_NAME_BUFFER_LENGTH];
  char *server_name_ienc; /* server_name_T_buffer or odbc_malloc()-ed. */

#define DESCRIPTION_BUFFER_LENGTH (100 +1)
  SP_SQLTCHAR description_T_buffer[DESCRIPTION_BUFFER_LENGTH];
  char *description_ienc; /* description_T_buffer or odbc_malloc()-ed */

  /* "Applications should allocate at least 1024 characters for this buffer."
     http://msdn.microsoft.com/en-us/library/ms715433(VS.85).aspx */
#define OutConnectionStringCharCount 1024
  SP_SQLTCHAR OutConnectionStringBufferT[OutConnectionStringCharCount];
  char *OutConnectionStringBuffer_ienc; /* OutConnectionStringBufferT or odbc_malloc()-ed */

  /* Large enough? */
#define MessageTextCharCount 1024
  SP_SQLTCHAR MessageTextT[MessageTextCharCount];
  char *MessageText_ienc; /* MessageTextT or odbc_malloc()-ed */

#if !SP_SQL_UNICODE
  /* [PM] 4.2 Fallback buffers for sanitize_string() */
#define OutConnectionStringBuffer_tmp_buf_size OutConnectionStringCharCount
  char OutConnectionStringBuffer_tmp_buf[OutConnectionStringBuffer_tmp_buf_size];
#define MessageText_tmp_buf_size MessageTextCharCount
  char MessageText_tmp_buf[MessageText_tmp_buf_size];
#endif  /* !SP_SQL_UNICODE */

  SP_SQLTCHAR SqlstateT[6];
  char *Sqlstate_ienc; /* SqlstateT or odbc_malloc()-ed */


#define COLUMN_NAME_BUFFER_LENGTH SQL_MAX_OPTION_STRING_LENGTH /* 256 */
  SP_SQLTCHAR column_name_bufferT[COLUMN_NAME_BUFFER_LENGTH];
  char *column_name_ienc; /* column_name_bufferT or odbc_malloc()-ed */
  char *term_as_string_store; /* odbc_malloc()-ed */
} state;

/* static struct buflist * find_buflist_entry(SP_integer StatementHandleIndex, SP_integer buffer_id); */
static SQLRETURN odbc_disconnect_helper(struct connectionhandle *entry);
static void free_connectionhandle(struct connectionhandle *entry);

#if SICSTUS_DBG>1 || ODBC_DEBUG>1
static void debug_print_stmthandle_stack(void)
{
  if (state.statementhandlestack != NULL)
    {
      size_t i;
      for (i = 1; i <= state.statementhandle_tos; i++) {
        struct buflist * bl = state.statementhandlestack[i].buffers;
        fprintf(stderr, "state.statementhandlestack[%" SPRIdSZ "].real_handle==%p\n", i, (void*)state.statementhandlestack[i].real_handle);
        fprintf(stderr, "state.statementhandlestack[%" SPRIdSZ "].connection_handle_index==%" SPRIdINTEGER"\n", i, (SP_integer)state.statementhandlestack[i].connection_handle_index);
        fprintf(stderr, "state.statementhandlestack[%" SPRIdSZ "].buffers==%p\n", i, bl);
        if (bl != NULL) {
          fprintf(stderr, "    buf==%p\n", bl->buf);
          fprintf(stderr, "    next==%p\n", bl->next);
          while (bl->next != NULL) {
            bl = bl->next;
            fprintf(stderr, "    buf==%p\n", bl->buf);
            fprintf(stderr, "    next==%p\n", bl->next);
          }
        }
      }
      fflush(stderr);
    }
}

static void debug_print_environmenthandle_stack(void)
{
  /* [PM] 4.2. FIXME: Implement */
}

static void debug_print_connhandle_stack(void)
{
  /* [PM] 4.2. FIXME: Implement */
}

static void debug_print_handle_stacks(void)
{
  debug_print_environmenthandle_stack();
  debug_print_connhandle_stack();
  debug_print_stmthandle_stack();
}

#define DEBUG_PRINT_HANDLE_STACKS debug_print_handle_stacks()

#else
#define DEBUG_PRINT_HANDLE_STACKS
#endif

#define INVALID_HANDLE_INDEX -1
#define INVALID_STATEMENT_HANDLE_INDEX INVALID_HANDLE_INDEX
#define INVALID_ENVIRONMENT_HANDLE_INDEX INVALID_HANDLE_INDEX
#define INVALID_CONNECTION_HANDLE_INDEX INVALID_HANDLE_INDEX

#define STATEMENT_HANDLE_INDEX_IN_RANGE(INDEX) (0 < (INDEX) && ((size_t)(INDEX)) <= state.statementhandle_tos)
#define ENVIRONMENT_HANDLE_INDEX_IN_RANGE(INDEX) (0 < (INDEX) && ((size_t)(INDEX)) <= state.environmenthandle_tos)
#define CONNECTION_HANDLE_INDEX_IN_RANGE(INDEX) (0 < (INDEX) && ((size_t)(INDEX)) <= state.connectionhandle_tos)

static void *odbc_malloc(size_t sz)
{
  SP_ASSERT(sz != (size_t)SQL_NTS);
  SP_ASSERT(((SQLSMALLINT)sz) >= 0);

  {
  void *tmp = SP_malloc(sz);
#if ODBC_DEBUG
  if (tmp != NULL)
    {
      state.alloc_counter++;
    }
#endif  /* ODBC_DEBUG */
  return tmp;
  }
}

#if (SP_SQL_UNICODE || ENABLE_WCHAR) && !SP_WIN32 && (SP_ODBC_SQLWCHAR_IS_UTF32 || SP_ODBC_SQLWCHAR_IS_UTF16)
#define NEEDS_ODBC_MALLOC_FROM_SP_MALLOC 1
#endif /* (SP_SQL_UNICODE || ENABLE_WCHAR) && !SP_WIN32 && (SP_ODBC_SQLWCHAR_IS_UTF32 || SP_ODBC_SQLWCHAR_IS_UTF16) */

#if NEEDS_ODBC_MALLOC_FROM_SP_MALLOC
/* [PM] 4.3 Convert a SP_malloc()-ed block to a odbc_malloc()-ed
   block, and takes ownerships (frees or re-uses) the SP_malloc()-ed
   block.
   
   The returned pointer should eventuall be freed with odbc_free().
   
   This just returns its argument if !ODBC_DEBUG.
*/
static void *odbc_malloc_from_sp_malloc(void *p, size_t sz)
{
  (void)sz;
#if ODBC_DEBUG
  if (p != NULL)
    {
      state.alloc_counter++;
    }
#endif  /* ODBC_DEBUG */
  return p;
}

#endif /* NEEDS_ODBC_MALLOC_FROM_SP_MALLOC */

static void *odbc_realloc(void *p, size_t sz)
{
  void *q;
  SP_ASSERT(p != NULL);

#if ODBC_DEBUG
  SP_ASSERT(state.alloc_counter > 0);
#endif  /* ODBC_DEBUG */
  q = SP_realloc(p, sz);
  /* DEBUG_PRINT((stderr, "ALLOC: odbc_realloc(%p) == %p\n", p, q)); */
  return q;
}

/* Argument may be NULL */
static void odbc_free(void *p)
{
  if (p != NULL)
    {
      /* DEBUG_PRINT((stderr, "ALLOC: odbc_free(%p)\n", p)); */

#if ODBC_DEBUG
      SP_ASSERT(state.alloc_counter > 0);
      state.alloc_counter--;
#endif  /* ODBC_DEBUG */
      SP_free(p);
    }
}



/* [PM] find the entry, or NULL. The returned entry may currently not be in use. */
static struct statementhandle *real_statement_handle_entry(SP_integer handle_index)
{
  SP_ASSERT(STATEMENT_HANDLE_INDEX_IN_RANGE(handle_index));
  if (STATEMENT_HANDLE_INDEX_IN_RANGE(handle_index))
    {
      /* [PM] 4.2 The entry may be unused but in this case real_handle is SQL_NULL_HSTMT */
      return &state.statementhandlestack[handle_index];
    }
  return NULL;
}

/* [PM] 4.2 Returns SQL_NULL_HSTMT if handle_index is invalid */
static SQLHSTMT real_statement_handle(SP_integer handle_index)
{
  struct statementhandle *entry = real_statement_handle_entry(handle_index);
  
  if (entry != NULL)
    {
      /* [PM] 4.2 The entry may be unused but in this case real_handle is SQL_NULL_HSTMT */
      return entry->real_handle;
    }
  return SQL_NULL_HSTMT;
}

/* [PM] find the entry, or NULL. The returned entry may currently not be in use. */
static struct environmenthandle *real_environment_handle_entry(SP_integer handle_index)
{
  SP_ASSERT(ENVIRONMENT_HANDLE_INDEX_IN_RANGE(handle_index));
  if (ENVIRONMENT_HANDLE_INDEX_IN_RANGE(handle_index))
    {
      /* [PM] 4.2 The entry may be unused but in this case real_handle is SQL_NULL_HENV */
      return &state.environmenthandlestack[handle_index];
    }
  return NULL;
}

static SQLHENV real_environment_handle(SP_integer handle_index)
{
  struct environmenthandle *entry = real_environment_handle_entry(handle_index);

  if (entry != NULL)
    {
      /* [PM] 4.2 The entry may be unused but in this case real_handle is SQL_NULL_HENV */
      return entry->real_handle;
    }
  return SQL_NULL_HENV;
}

static struct connectionhandle *real_connection_handle_entry(SP_integer handle_index)
{
  SP_ASSERT(CONNECTION_HANDLE_INDEX_IN_RANGE(handle_index));
  if (CONNECTION_HANDLE_INDEX_IN_RANGE(handle_index))
    {
      /* [PM] 4.2 The entry may be unused but in this case real_handle is SQL_NULL_HDBC */
      return &state.connectionhandlestack[handle_index];
    }
  return NULL;
}

static SQLHDBC real_connection_handle(SP_integer handle_index)
{
  struct connectionhandle *entry = real_connection_handle_entry(handle_index);
  if (entry != NULL)
    {
      /* [PM] 4.2 The entry may be unused but in this case real_handle is SQL_NULL_HDBC */
      return entry->real_handle;
    }
  return SQL_NULL_HDBC;
}

/* [PM] 4.2 Reinitialize all fields */
static void clear_statementandle(struct statementhandle *entry)
{
  entry->real_handle = SQL_NULL_HSTMT;
  entry->connection_handle_index = 0;
  entry->is_definitely_unbound = SPIO_TRUE;
  entry->buffers = NULL;
}


/* [PM] 4.2 Returns positive index of unused statementhandle. Returns
   zero on failure (out of memory) */
static size_t allocate_statement_handle_index(void)
{
  size_t i;

  /* [PM] 4.2 [1..state.statementhandle_tos] (inclusive) are existing, potentially freed, entries */
  for (i = 1; i <= state.statementhandle_tos; i++) {
    if (state.statementhandlestack[i].real_handle == SQL_NULL_HSTMT) {
      SP_ASSERT(state.statementhandlestack[i].buffers == NULL);
      SP_ASSERT(state.statementhandlestack[i].is_definitely_unbound);
      break;
    }
  }
  if (i <= state.statementhandle_tos) {
    /* re-used previously freed entry. Nothing to do. */
    SP_ASSERT(state.statementhandlestack[i].real_handle == SQL_NULL_HSTMT);
    SP_ASSERT(state.statementhandlestack[i].connection_handle_index == 0);
    SP_ASSERT(state.statementhandlestack[i].is_definitely_unbound);
    SP_ASSERT(state.statementhandlestack[i].buffers == NULL);
  } else {
    /* Need to allocate new entry, potentially growing the stack. */
    SP_ASSERT(i > state.statementhandle_tos);

    if ( !(state.statementhandle_tos + 1 < state.stmthandle_stacksize) ) {
      /* Need to grow stack */
      size_t new_stack_size = state.stmthandle_stacksize + STATEMENTHANDLEINCREMENTSTACKSIZE;
      void *tmp = odbc_realloc(state.statementhandlestack, new_stack_size * sizeof state.statementhandlestack[0]);
      if (tmp == NULL) {
        i = 0;
        goto barf;
      }
      state.statementhandlestack = tmp;
      state.stmthandle_stacksize = new_stack_size;
    }
    /* Now there is room */
    SP_ASSERT(state.statementhandle_tos + 1 < state.stmthandle_stacksize);
    /* Push on stack */
    state.statementhandle_tos++;
    i = state.statementhandle_tos;
  }
  SP_ASSERT(STATEMENT_HANDLE_INDEX_IN_RANGE(i));
  clear_statementandle(&state.statementhandlestack[i]);
 cleanup:
  return i;
 barf:
  goto cleanup;
}

/* [PM] 4.2 Returns positive index of unused environmenthandle. Returns
   zero on failure (out of memory) */
static size_t allocate_environment_handle_index(void)
{
  size_t i;

  /* [PM] 4.2 [1..state.environmenthandle_tos] (inclusive) are existing, potentially freed, entries */
  for (i = 1; i <= state.environmenthandle_tos; i++) {
    if (state.environmenthandlestack[i].real_handle == SQL_NULL_HENV) {
      break;
    }
  }
  if (i <= state.environmenthandle_tos) {
    /* re-used previously freed entry. Nothing to do. */
    SP_ASSERT(state.environmenthandlestack[i].real_handle == SQL_NULL_HENV);
  } else {
    /* Need to allocate new entry, potentially growing the stack. */
    SP_ASSERT(i > state.environmenthandle_tos);

    if ( !(state.environmenthandle_tos + 1 < state.environmenthandle_stacksize) ) {
      /* Need to grow stack */
      size_t new_stack_size = state.environmenthandle_stacksize + ENVIRONMENTHANDLEINCREMENTSTACKSIZE;
      void *tmp = odbc_realloc(state.environmenthandlestack, new_stack_size * sizeof state.environmenthandlestack[0]);
      if (tmp == NULL) {
        i = 0;
        goto barf;
      }
      state.environmenthandlestack = tmp;
      state.environmenthandle_stacksize = new_stack_size;
    }
    /* Now there is room */
    SP_ASSERT(state.environmenthandle_tos + 1 < state.environmenthandle_stacksize);
    /* Push on stack */
    state.environmenthandle_tos++;
    i = state.environmenthandle_tos;
  }
  SP_ASSERT(ENVIRONMENT_HANDLE_INDEX_IN_RANGE(i));
  state.environmenthandlestack[i].real_handle = SQL_NULL_HENV;

 cleanup:
  return i;
 barf:
  goto cleanup;
}

/* [PM] 4.2 Reinitialize all fields */
static void clear_connectionhandle(struct connectionhandle *entry)
{
  entry->real_handle = SQL_NULL_HDBC;
  /* entry->is_connected = SPIO_FALSE; */
  entry->flags = CONNECTION_HANDLE_FLAG_NONE;

#if CONNECTION_HANDLE_FLAG_UTF8_CHAR_DEFAULT
  SET_FLAG(entry->flags, CONNECTION_HANDLE_FLAG_UTF8_CHAR);
#else  /* !CONNECTION_HANDLE_FLAG_UTF8_CHAR_DEFAULT */
  CLEAR_FLAG(entry->flags, CONNECTION_HANDLE_FLAG_UTF8_CHAR);
#endif  /* !CONNECTION_HANDLE_FLAG_UTF8_CHAR_DEFAULT */

  entry->environment_handle_index = 0;
}

/* [PM] 4.2 Returns positive index of unused connectionhandle. Returns
   zero on failure (out of memory) */
static size_t allocate_connection_handle_index(void)
{
  size_t i;

  /* [PM] 4.2 [1..state.connectionhandle_tos] (inclusive) are existing, potentially freed, entries */
  for (i = 1; i <= state.connectionhandle_tos; i++) {
    if (state.connectionhandlestack[i].real_handle == SQL_NULL_HDBC) {
      break;
    }
  }
  if (i <= state.connectionhandle_tos) {
    /* re-used previously freed entry. Nothing to do. */
    SP_ASSERT(state.connectionhandlestack[i].real_handle == SQL_NULL_HDBC);
    /* SP_ASSERT(!state.connectionhandlestack[i].is_connected); */
    SP_ASSERT(!IS_FLAG_SET(state.connectionhandlestack[i].flags, CONNECTION_HANDLE_FLAG_CONNECTED));
    SP_ASSERT(state.connectionhandlestack[i].environment_handle_index == 0);
  } else {
    /* Need to allocate new entry, potentially growing the stack. */
    SP_ASSERT(i > state.connectionhandle_tos);

    if ( !(state.connectionhandle_tos + 1 < state.connectionhandle_stacksize) ) {
      /* Need to grow stack */
      size_t new_stack_size = state.connectionhandle_stacksize + CONNECTIONHANDLEINCREMENTSTACKSIZE;
      void *tmp = odbc_realloc(state.connectionhandlestack, new_stack_size * sizeof state.connectionhandlestack[0]);
      if (tmp == NULL) {
        i = 0;
        goto barf;
      }
      state.connectionhandlestack = tmp;
      state.connectionhandle_stacksize = new_stack_size;
    }
    /* Now there is room */
    SP_ASSERT(state.connectionhandle_tos + 1 < state.connectionhandle_stacksize);
    /* Push on stack */
    state.connectionhandle_tos++;
    i = state.connectionhandle_tos;
  }
  SP_ASSERT(CONNECTION_HANDLE_INDEX_IN_RANGE(i));
  clear_connectionhandle(&state.connectionhandlestack[i]);

 cleanup:
  return i;
 barf:
  goto cleanup;
}

/* Decodes a FUNCTOR(ID) or ID term_ref. NOTE: the term_ref gets
   overwritten. */
static SP_integer decode_spec(SP_term_ref spec_ref, SP_atom functor, SP_integer error_value)
{
  SP_integer id = error_value;
  if (SP_get_integer(spec_ref, &id)) {
    /* Was an integer. */
  } else {
#if SP_ASSERTIONS
    /* Verify that the functor is FUNCTOR/1 */
    {
      int arity = -1;
      SP_atom name_atom = 0;
      if (SP_get_functor(spec_ref, &name_atom, &arity)
          &&
          arity == 1
          &&
          functor == name_atom) {
        /* FUNCTOR/1 */
      } else {
        SP_ASSERT(SPIO_FALSE);
      }
    }
#endif /* SP_ASSERTIONS */
    (void) functor;

    /* decode FUNCTOR(ID). Overwrites contents of spec_ref to avoid allocating extra term-ref. */
    if (SP_get_arg(1, spec_ref, spec_ref)) {
      if (!SP_get_integer(spec_ref, &id)) {
        id = error_value;
      }
    } else {
      /* invalid spec */
      id = error_value;
    }
  }
  return id;
}

/* Decodes a odbc_buffer(BUFFER_ID) or BUFFER_ID term_ref. NOTE: the
   term_ref gets overwritten. Return zero on failure (which can never
   be a valid BUFFER_ID) */
static SP_integer decode_buffer_spec(SP_term_ref buffer_spec_ref)
{
  return decode_spec(buffer_spec_ref, state.odbc_buffer, 0);
}

/* Decodes a statement_handle(IDX) or IDX term_ref. NOTE: the term_ref
   gets overwritten. Returns INVALID_STATEMENT_HANDLE_INDEX on
   failure */
static SP_integer decode_statement_handle_spec(SP_term_ref spec_ref)
{
  return decode_spec(spec_ref, state.statement_handle, INVALID_STATEMENT_HANDLE_INDEX);
}

/* Decodes a environment_handle(IDX) or IDX term_ref. NOTE: the
   term_ref gets overwritten. Returns INVALID_ENVIRONMENT_HANDLE_INDEX
   on failure */
static SP_integer decode_environment_handle_spec(SP_term_ref spec_ref)
{
  return decode_spec(spec_ref, state.environment_handle, INVALID_ENVIRONMENT_HANDLE_INDEX);
}

/* Decodes a connection_handle(IDX) or IDX term_ref. NOTE: the
   term_ref gets overwritten. Returns INVALID_CONNECTION_HANDLE_INDEX
   on failure */
static SP_integer decode_connection_handle_spec(SP_term_ref spec_ref)
{
  return decode_spec(spec_ref, state.connection_handle, INVALID_CONNECTION_HANDLE_INDEX);
}

/****************************************
SQLRETURN SQLAllocHandle(
     SQLSMALLINT     HandleType,
     SQLHANDLE     InputHandle,
     SQLHANDLE *     OutputHandlePtr);
*****************************************/

/*
  '$odbc_allocate_environment_handle'(-OutputEnvHandle, -ResultCode)
  foreign(odbc_allocate_environment_handle,
          '$odbc_allocate_environment_handle'(-integer,[-integer])).
*/
SP_integer SPCDECL odbc_allocate_environment_handle(SPAPI_ARG_PROTO_DECL SP_integer* OutputEnvHandle)
{
  SQLHANDLE OutputHandle = SQL_NULL_HENV;
  SQLRETURN rc;
  size_t index = allocate_environment_handle_index();
  if (index <= 0)
    {
      ODBC_BARF(ODBC_ERROR_OUT_OF_MEMORY);
    }

  rc = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &OutputHandle);
  SP_ASSERT(!SQL_SUCCEEDED(rc) || OutputHandle != SQL_NULL_HENV);

  if (!SQL_SUCCEEDED(rc))
    {
      if (OutputHandle != SQL_NULL_HENV)
        {
          /* 
             [PM] 4.2 The CLI standard says "If AllocHandle() fails to
             allocate an environment handle for any other reason, it
             returns[SQL_ERROR] and OutputHandle contains a restricted
             handle to a skeleton environment.  The application can
             use this handle only on CLI calls that obtain diagnostic
             information or that free the handle."
             
             Similar wording is in some DB2 ODBC docs.

             The possibility of a (restricted) handle being returned
             on SQL_ERROR is not in the Microsoft ODBC documentation
             so we ignore this case and just leak.
          */
          SP_ASSERT(0);
        }
      goto barf;
    }

  SP_SOFT_ASSERT(rc != SQL_SUCCESS_WITH_INFO); /* [PM] 4.2 tell me if SQL_SUCCESS_WITH_INFO happens */
  state.environmenthandlestack[index].real_handle = OutputHandle;
  *OutputEnvHandle = index;
  
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs */
  SP_ASSERT(!SQL_SUCCEEDED(rc));
  *OutputEnvHandle = 0;
  goto cleanup;
}


SP_integer SPCDECL odbc_allocate_connection_handle(SPAPI_ARG_PROTO_DECL SP_term_ref environment_handle_spec, SP_integer* OutputConnHandleIndexPtr)
{
  SQLHANDLE OutputHandle = SQL_NULL_HDBC;
  SP_integer OutputConnHandleIndex;
  SP_integer const EnvironmentHandleIndex = decode_environment_handle_spec(environment_handle_spec);
  SQLHENV InputEnvHandle = real_environment_handle(EnvironmentHandleIndex);
  SQLRETURN rc;
  
  if (InputEnvHandle == SQL_NULL_HENV)
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_INVALID_HANDLE;
      goto barf;
    }
  OutputConnHandleIndex = allocate_connection_handle_index();
  if (OutputConnHandleIndex <= 0)
    {
      ODBC_BARF(ODBC_ERROR_OUT_OF_MEMORY);
    }
 
  rc = SQLAllocHandle(SQL_HANDLE_DBC, (SQLHANDLE)InputEnvHandle, &OutputHandle);
  SP_ASSERT(!SQL_SUCCEEDED(rc) || OutputHandle != SQL_NULL_HDBC);

  if (!SQL_SUCCEEDED(rc))
    {
      SP_ASSERT(OutputHandle == SQL_NULL_HDBC);
      ODBC_BARF(rc);
    }

  SP_SOFT_ASSERT(rc != SQL_SUCCESS_WITH_INFO); /* [PM] 4.2 tell me if SQL_SUCCESS_WITH_INFO happens */
  state.connectionhandlestack[OutputConnHandleIndex].real_handle = OutputHandle;
  /* state.connectionhandlestack[OutputConnHandleIndex].is_connected = SPIO_FALSE; */
  CLEAR_FLAG(state.connectionhandlestack[OutputConnHandleIndex].flags, CONNECTION_HANDLE_FLAG_CONNECTED); /* redundant */
  
  state.connectionhandlestack[OutputConnHandleIndex].environment_handle_index = EnvironmentHandleIndex;
  
  *OutputConnHandleIndexPtr = OutputConnHandleIndex;

 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs */
  SP_ASSERT(!SQL_SUCCEEDED(rc));
  *OutputConnHandleIndexPtr = 0;
  goto cleanup;
}

SP_integer SPCDECL odbc_allocate_statement_handle(SPAPI_ARG_PROTO_DECL SP_term_ref connection_handle_spec, SP_integer * OutputStatementHandle)
{
  SQLRETURN rc;
  SQLHANDLE StatementHandle;
  SP_integer const ConnectionHandleIndex = decode_connection_handle_spec(connection_handle_spec);
  SQLHDBC ConnectionHandle = real_connection_handle(ConnectionHandleIndex);
  size_t i;

  if (ConnectionHandle == SQL_NULL_HDBC)
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_INVALID_HANDLE;
      goto barf;
    }

  i = allocate_statement_handle_index();
  
  /* [i] now points at an allocated but unused entry */
  SP_ASSERT(STATEMENT_HANDLE_INDEX_IN_RANGE(i));
  SP_ASSERT(state.statementhandlestack[i].real_handle == SQL_NULL_HSTMT);
  SP_ASSERT(state.statementhandlestack[i].connection_handle_index == 0);
  SP_ASSERT(state.statementhandlestack[i].is_definitely_unbound);
  SP_ASSERT(state.statementhandlestack[i].buffers == NULL);

  StatementHandle = SQL_NULL_HSTMT;
  rc = SQLAllocHandle(SQL_HANDLE_STMT, ConnectionHandle, &StatementHandle);
  SP_ASSERT((rc == SQL_ERROR) == (StatementHandle == SQL_NULL_HSTMT));
  if (rc == SQL_ERROR)
    {
      /* Mark the entry as unused */
      state.statementhandlestack[i].real_handle = SQL_NULL_HSTMT;
      rc = ODBC_ERROR_CREATING_HANDLE;
      goto barf;
    }
  
  state.statementhandlestack[i].connection_handle_index = ConnectionHandleIndex;
  state.statementhandlestack[i].real_handle = StatementHandle;
  *OutputStatementHandle = i;
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs */
  SP_ASSERT(IS_ODBC_ERROR(rc));
  *OutputStatementHandle = -1;
  goto cleanup;
}

void SPCDECL valid_statement_handle(SPAPI_ARG_PROTO_DECL SP_term_ref statement_handle_spec)
{
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT StatementHandle = real_statement_handle(StatementHandleIndex);

  if (StatementHandle == SQL_NULL_HSTMT) {
    SP_fail();
  }

  return;
}


/****************************************
SQLRETURN SQLSetEnvAttr(
     SQLHENV     EnvironmentHandle,
     SQLINTEGER     Attribute,
     SQLPOINTER     ValuePtr,
     SQLINTEGER     StringLength);
****************************************/

SP_integer SPCDECL odbc_set_environment_attribute(SPAPI_ARG_PROTO_DECL SP_term_ref environment_handle_spec,
                                          SP_integer Attribute,
                                          SP_integer Value)
{
  SQLRETURN rc;
  SP_integer const EnvironmentHandleIndex = decode_environment_handle_spec(environment_handle_spec);
  SQLHENV EnvHandle = real_environment_handle(EnvironmentHandleIndex);

  if (EnvHandle == SQL_NULL_HENV)
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_INVALID_HANDLE;
      goto barf;
    }

  /* [PM] 4.2 We better be sure that Value is not treated as a pointer... */
  if (Attribute != SQL_ATTR_ODBC_VERSION) {
    SP_ASSERT(0);
    rc = ODBC_ERROR_INVALID_PARAMETER;
    goto barf;
  }

  rc = SQLSetEnvAttr(EnvHandle,
                     (SQLINTEGER)Attribute,
                     (SQLPOINTER)Value,
                     (SQLINTEGER)0);
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}


/****************************************
SQLRETURN SQLFreeStmt(
     SQLHSTMT     StatementHandle,
     SQLUSMALLINT     Option);
****************************************/

static SQLRETURN odbc_free_statement_helper(struct statementhandle *entry, SQLUSMALLINT Option)
{
  SQLRETURN rc;
  SP_ASSERT(entry != NULL && entry->real_handle != SQL_NULL_HSTMT);

  /* [PM] 4.2 SQL_DROP would free the handle which is not allowed here. Use odbc_free_statement_handle for that. */
  SP_ASSERT(Option != SQL_DROP);
  rc = SQLFreeStmt(entry->real_handle, Option);
  SP_SOFT_ASSERT(SQL_SUCCEEDED(rc));
  if (SQL_SUCCEEDED(rc) && Option == SQL_UNBIND)
    {
      entry->is_definitely_unbound = SPIO_TRUE;
    }
  return rc;
}

/* Ensure statement handle is not bound to any buffers.  */
static SQLRETURN unbind_statement(struct statementhandle *entry)
{
  SQLRETURN rc1, rc2, rc3;
  SP_ASSERT(entry != NULL && entry->real_handle != SQL_NULL_HSTMT);
  /* Do all even if some fails */
  rc1 = odbc_free_statement_helper(entry, SQL_CLOSE); /* Probably not useful */
  SP_SOFT_ASSERT(SQL_SUCCEEDED(rc1));
  rc2 = odbc_free_statement_helper(entry, SQL_UNBIND); /* [PM] 4.2.2 Probably not useful now that we do not use SQLBindCol anymore. */
  SP_SOFT_ASSERT(SQL_SUCCEEDED(rc2));
  rc3 = odbc_free_statement_helper(entry, SQL_RESET_PARAMS); /* SQLBindParameter */
  SP_SOFT_ASSERT(SQL_SUCCEEDED(rc3));
  if (!SQL_SUCCEEDED(rc1))
    {
      return rc1;
    }
  if (!SQL_SUCCEEDED(rc2))
    {
      return rc2;
    }
  return rc3;
}

/* [PM] 4.2 This is redundant if odbc_free_statement_handle is about to be called */
SP_integer SPCDECL odbc_free_statement(SPAPI_ARG_PROTO_DECL SP_term_ref statement_handle_spec, SP_integer Option)
{
  SQLRETURN rc;
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  struct statementhandle *entry = real_statement_handle_entry(StatementHandleIndex);
  SQLHANDLE real_handle = (entry == NULL ? SQL_NULL_HSTMT : entry->real_handle);

  ODBC_HSTMT_CHECK(real_handle);
  rc = odbc_free_statement_helper(entry, (SQLUSMALLINT)Option);
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}


/****************************************
SQLRETURN SQLFreeHandle(
     SQLSMALLINT     HandleType,
     SQLHANDLE     Handle);
****************************************/

/*
  '$odbc_free_environment_handle'(+Handle, -ResultCode)
  foreign(odbc_free_environment_handle,
          '$odbc_free_environment_handle'(+integer, [-integer])).
*/
static void free_environmenthandle(struct environmenthandle *entry)
{
  SP_ASSERT(entry != NULL);
  {
    SQLRETURN rc;
    SQLHENV EnvironmentHandle = entry->real_handle;
    SP_ASSERT(EnvironmentHandle != SQL_NULL_HENV);

    /* free dependant connections */
    {
      size_t i;
      for (i = 1; i <= state.connectionhandle_tos; i++) {
        if (state.connectionhandlestack[i].real_handle != SQL_NULL_HDBC) {
          /* [PM] 4.2 Invariant i is in range and point to a valid enviroment entry */
          struct environmenthandle *env_entry = real_environment_handle_entry(state.connectionhandlestack[i].environment_handle_index);
          SP_ASSERT(env_entry != NULL);

          if (env_entry == entry)
            {
              DEBUG_PRINT((stderr, "ODBC_DBG: Warning: free_environmenthandle() Calling free_connectionhandle([%" SPRIdSZ "].real_handle==%p)\n",
                           i, (void*)state.connectionhandlestack[i].real_handle));
              /* This will disconnect and free statements, if needed */
              free_connectionhandle(&state.connectionhandlestack[i]);
            }
          else
            {
              SP_ASSERT(env_entry->real_handle != entry->real_handle);
            }
        }
      }
    }

    /* [PM] 4.2 freeing a environment handle with SQLFreeHandle()
       requires that its connections handles has been closed, which we
       do above (but which the user typically would do explicitly) */
    
#if ODBC_DEBUG > 1
    DEBUG_PRINT((stderr, "ODBC_DBG: Calling SQLFreeHandle(SQL_HANDLE_ENV, %p)\n", (void*)EnvironmentHandle));
#endif  /* ODBC_DEBUG */

    rc = SQLFreeHandle(SQL_HANDLE_ENV, EnvironmentHandle);
    if (!SQL_SUCCEEDED(rc))
      {
        DEBUG_PRINT((stderr, "ODBC_DBG: ERROR: SQLFreeHandle(SQL_HANDLE_ENV, %p)=%" SPRIdINTEGER "\n", (void*)EnvironmentHandle, (SP_integer)rc));
        SP_SOFT_ASSERT(0);      /* [PM] 4.2 Tell me if this happens */
        /* Quietly leak the handle. There is not much we could do. */
      }
    entry->real_handle = SQL_NULL_HENV;
  }
}

SP_integer SPCDECL odbc_free_environment_handle(SPAPI_ARG_PROTO_DECL SP_term_ref environment_handle_spec)
{
  SQLRETURN rc;
  SP_integer const EnvironmentHandleIndex = decode_environment_handle_spec(environment_handle_spec);
  SQLHENV real_handle = real_environment_handle(EnvironmentHandleIndex);
  if (real_handle == SQL_NULL_HENV) {
    SP_ASSERT(0);
    rc = ODBC_ERROR_INVALID_HANDLE;
    goto barf;
  }
  free_environmenthandle(&state.environmenthandlestack[EnvironmentHandleIndex]);
  rc = SQL_SUCCESS;
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}


static void free_connectionhandle(struct connectionhandle *entry)
{
  SP_ASSERT(entry != NULL);

  /* [PM] 4.2 freeing a connections handle with SQLFreeHandle()
     requires that it has been disconnected and that its statements
     handles has been freed. This is ensured here by disconnecting, if needed.
  */
  {
    SQLRETURN rc;
    SQLHDBC ConnectionHandle = entry->real_handle;
    SP_ASSERT(ConnectionHandle != SQL_NULL_HDBC);

    if (IS_FLAG_SET(entry->flags, CONNECTION_HANDLE_FLAG_CONNECTED))
      {
        DEBUG_PRINT((stderr, "ODBC_DBG: Warning: free_connectionhandle() Calling SQLDisconnect(%p)\n", (void*)ConnectionHandle));
        /* [PM] 4.2 this will also close any dependant statement handles */
        rc = odbc_disconnect_helper(entry);
        SP_SOFT_ASSERT(SQL_SUCCEEDED(rc));
        (void)rc;
      }
    
#if ODBC_DEBUG > 1
    DEBUG_PRINT((stderr, "ODBC_DBG: Calling SQLFreeHandle(SQL_HANDLE_DBC, %p)\n", (void*)ConnectionHandle));
#endif  /* ODBC_DEBUG */
    rc = SQLFreeHandle(SQL_HANDLE_DBC, ConnectionHandle);
    if (!SQL_SUCCEEDED(rc))
      {
        DEBUG_PRINT((stderr, "ODBC_DBG: ERROR: SQLFreeHandle(SQL_HANDLE_DBC, %p)=%" SPRIdINTEGER "\n", (void*)ConnectionHandle, (SP_integer)rc));
        SP_SOFT_ASSERT(0);      /* [PM] 4.2 Tell me if this happens */
        /* Quietly leak. There is not much we could do. */
      }
  }
  clear_connectionhandle(entry);
}

SP_integer SPCDECL odbc_free_connection_handle(SPAPI_ARG_PROTO_DECL SP_term_ref connection_handle_spec)
{
  SQLRETURN rc;
  SP_integer const ConnectionHandleIndex = decode_connection_handle_spec(connection_handle_spec);
  SQLHDBC ConnectionHandle = real_connection_handle(ConnectionHandleIndex);

  DEBUG_PRINT((stderr, "\nODBC_DBG: odbc_free_connection_handle(%" SPRIdINTEGER " (%p))\n", (SP_integer)ConnectionHandleIndex, (void*)ConnectionHandle));

  if (ConnectionHandle == SQL_NULL_HDBC) {
    SP_ASSERT(0);
    rc = ODBC_ERROR_INVALID_HANDLE;
    goto barf;
  }
  /* [PM] 4.2 This will quietly free statement handles and disconnect, if needed.  */
  free_connectionhandle(&state.connectionhandlestack[ConnectionHandleIndex]);
  rc = SQL_SUCCESS;
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}


/* [PM] 4.2 Unlink and free the non-NULL buflist entry pointed to by blp  */
static void free_buflist_entry_at(struct buflist **blp)
{
  SP_ASSERT(blp != NULL && *blp != NULL);
  {
    struct buflist * const bl = *blp;

    *blp = bl->next;            /* unlink */


    DEBUG_PRINT((stderr, "\nODBC_DBG: freeing buffer: %p\n", bl->buf));
    odbc_free(bl->buf);

    DEBUG_PRINT((stderr, "\nODBC_DBG: freeing buflist struct: %p\n", bl));
    odbc_free(bl);
  }
}


/* [PM] 4.2 Free statement handle and buffers and mark the entry as unused */
static void free_statementhandle(struct statementhandle *sh)
{
  SQLHSTMT StatementHandle = sh->real_handle;
  struct buflist ** blp = &sh->buffers;
  SP_ASSERT(StatementHandle != SQL_NULL_HSTMT);
  SP_ASSERT(real_connection_handle(sh->connection_handle_index) != SQL_NULL_HDBC);

  /* [PM] 4.2 Free handle first. Among other things, this
     ensures that no pointers remains from the statement
     into the, soon to be, freed buffers.
     
     There is no need to call odbc_free_statement/SQLFreeStmt, since
     it is implicit in SQLFreeHandle
  */
  {
    SQLRETURN rc;
#if ODBC_DEBUG > 1
    DEBUG_PRINT((stderr, "ODBC_DBG: Calling SQLFreeHandle(SQL_HANDLE_STMT, %p) (owner index %" SPRIdINTEGER ")\n", (void*)StatementHandle, (SP_integer)sh->connection_handle_index));
#endif  /* ODBC_DEBUG */

    rc = SQLFreeHandle(SQL_HANDLE_STMT, StatementHandle);
    if (SQL_SUCCEEDED(rc))
      {
        sh->is_definitely_unbound = SPIO_TRUE;
      }
    else
      {
        DEBUG_PRINT((stderr, "ODBC_DBG: ERROR: SQLFreeHandle(SQL_HANDLE_STMT, %p)=%" SPRIdINTEGER "\n", (void*)StatementHandle, (SP_integer)rc));
        /* [PM] 4.2 We are in serious trouble if the handle can not be freed */
        SP_SOFT_ASSERT(0);      /* [PM] 4.2 Tell me if this happens */
        /* [PM] 4.2 Leak the buffers since we do not know if data will be written to them by the statement. This is the best we can do. */
        *blp = NULL;
      }
  }

  while (*blp != NULL)
    {
      SP_ASSERT(sh->is_definitely_unbound);
      free_buflist_entry_at(blp);
    }
  SP_ASSERT(sh->buffers == NULL);
  clear_statementandle(sh);
}



/*
  '$odbc_free_statement_handle'(+HandleType, +Handle, -ResultCode)
  foreign(odbc_free_handle,
          '$odbc_free_statement_handle'(+integer, +integer, [-integer])).
*/
SP_integer SPCDECL odbc_free_statement_handle(SPAPI_ARG_PROTO_DECL SP_term_ref statement_handle_spec)
{
  SQLRETURN rc;
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT StatementHandle = real_statement_handle(StatementHandleIndex);

  ODBC_HSTMT_CHECK(StatementHandle);
  /* [PM] 4.2 Caller should have freed buffers (but we will free any remaining buffers in free_statementhandle() */
  SP_SOFT_ASSERT(state.statementhandlestack[StatementHandleIndex].buffers == NULL);
  free_statementhandle(&state.statementhandlestack[StatementHandleIndex]);
  rc = SQL_SUCCESS;
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}


/****************************************
SQLRETURN SQLDataSources(
     SQLHENV     EnvironmentHandle,
     SQLUSMALLINT     Direction,
     SQLCHAR *     ServerName,
     SQLSMALLINT     BufferLength1,
     SQLSMALLINT *     NameLength1Ptr,
     SQLCHAR *     Description,
     SQLSMALLINT     BufferLength2,
     SQLSMALLINT *     NameLength2Ptr);
****************************************/

/*
  '$odbc_data_sources'(+EnvironmentHandle, +Direction, -ServerName,
                       -Description, -ResultCode)
  foreign(odbc_data_sources,
          '$odbc_data_sources'(+integer, +integer, -string,
                               -string, [-integer])).
*/

SP_integer SPCDECL odbc_data_sources(SPAPI_ARG_PROTO_DECL SP_term_ref environment_handle_spec,
                                     SP_integer Direction,
                                     char const ** ServerName, char const ** Description)
{
  SQLRETURN rc, rc2;
  SQLSMALLINT NameLength1;
  SQLSMALLINT NameLength2;
  SQLHENV EnvironmentHandle;
  SP_integer const EnvironmentHandleIndex = decode_environment_handle_spec(environment_handle_spec);

  INIT_STATIC_BUFFER(state.server_name_T_buffer);
  INIT_STATIC_BUFFER(state.description_T_buffer);

  EnvironmentHandle = real_environment_handle(EnvironmentHandleIndex);
  if (EnvironmentHandle == SQL_NULL_HENV)
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_INVALID_HANDLE;
      goto barf;
    }
  FREE_STATIC_IENC(state.server_name_ienc);
  FREE_STATIC_IENC(state.description_ienc);

#if SP_ODBC_IODBC && SP_SQL_UNICODE
  /*
    Suppress a valgrind warning in Mac OS X 10.6.8 iodbc
    trace_SQLDataSourcesW where *NameLength1 is dereferenced also on
    failure.
   */
  NameLength1 = 4711;
#endif

  rc = SP_SQLDataSourcesT(EnvironmentHandle,
                      (SQLUSMALLINT)Direction,
                      &state.server_name_T_buffer[0],
                      (SQLSMALLINT)SERVER_NAME_BUFFER_LENGTH,
                      &NameLength1,
                      &state.description_T_buffer[0],
                      (SQLSMALLINT)DESCRIPTION_BUFFER_LENGTH,
                      &NameLength2);
  ODBC_CHECK(rc);

  {
    rc2 = FROM_BUF_TO_IENC(state.server_name_T_buffer, state.server_name_ienc);
    if (rc2 != SQL_SUCCESS)
      {
        ODBC_BARF(rc2);
      }
    rc2 = FROM_BUF_TO_IENC(state.description_T_buffer, state.description_ienc);
    if (rc2 != SQL_SUCCESS)
      {
        ODBC_BARF(rc2);
      }
  }
  SP_ASSERT(state.server_name_ienc != NULL);
  *ServerName = state.server_name_ienc;
  SP_ASSERT(state.description_ienc != NULL);
  *Description = state.description_ienc;
  SP_ASSERT(SQL_SUCCEEDED(rc));
 cleanup:
  return rc;
 barf:
  SP_ASSERT(!SQL_SUCCEEDED(rc));
  /* [PM] 4.2 Barf sets all outputs */
  *ServerName = "";
  *Description = "";
  goto cleanup;
}


/****************************************
SQLRETURN SQLSetConnectAttr(
     SQLHDBC     ConnectionHandle,
     SQLINTEGER     Attribute,
     SQLPOINTER     ValuePtr,
     SQLINTEGER     StringLength);
****************************************/

SP_integer SPCDECL odbc_set_connect_integer_attr(SPAPI_ARG_PROTO_DECL SP_term_ref connection_handle_spec,
                                                 SP_integer Attribute_,
                                                 SP_integer Value_)
{
  SQLRETURN rc;
  SQLINTEGER Attribute = (SQLINTEGER)Attribute_;
  SQLINTEGER Value;
  SP_integer const ConnectionHandleIndex = decode_connection_handle_spec(connection_handle_spec);
  struct connectionhandle *entry = real_connection_handle_entry(ConnectionHandleIndex);
  SQLHDBC ConnectionHandle = real_connection_handle(ConnectionHandleIndex);

  if (ConnectionHandle == SQL_NULL_HDBC)
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_INVALID_HANDLE;
      goto barf;
    }
  SP_ASSERT(entry != NULL && entry->real_handle == ConnectionHandle);
  COMPILE_TIME_ASSERT(sizeof(SQLINTEGER) == 32/8); /* [PM] 4.2 Docs explicitly says SQL_IS_INTEGER means 32-bit integer */

  /* [PM] 4.2 We must be sure that the attribute is really an integer attribute */
  switch (Attribute)
    {
    case SQL_ATTR_LOGIN_TIMEOUT:
    case SQL_ATTR_CONNECTION_TIMEOUT:
      Value = (SQLINTEGER)Value_;
      break;
    case SQL_ATTR_AUTOCOMMIT:
      Value = (SQLUINTEGER)Value_;
      break;
    case SQL_ATTR_TRACEFILE:
      /* [PM] 4.2.2 As a hack (to avoid changing the odbc.c foreign
         API at this point) we treat setting a numerical attribute for
         SQL_ATTR_TRACE_FILE as setting the boolean
         CONNECTION_HANDLE_FLAG_UTF8_CHAR on the connection. (The real
         attribute SQL_ATTR_TRACE_FILE has a string value so passing
         an integer value would not make sense. */
      if (Value_ != 0)
        {
          SET_FLAG(entry->flags, CONNECTION_HANDLE_FLAG_UTF8_CHAR);
        }
      else
        {
          CLEAR_FLAG(entry->flags, CONNECTION_HANDLE_FLAG_UTF8_CHAR);
        }
      rc = SQL_SUCCESS;
      goto cleanup;
    default:
      rc = ODBC_ERROR_INVALID_PARAMETER;
      goto barf;
    }
  rc = SQLSetConnectAttr(ConnectionHandle,
                         Attribute,
                         (SQLPOINTER)(SP_integer)Value,
                         /* [PM] 4.2 length is ignored for ODBC
                            integer values but is used for driver
                            specific integer attribues (although we
                            currenly do not support any driver
                            specific attributes) */
                         SQL_IS_INTEGER);
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  SP_ASSERT(!SQL_SUCCEEDED(rc));
  goto cleanup;
}


/****************************************
SQLRETURN SQLColumns(
     SQLHSTMT     StatementHandle,
     SQLCHAR *     CatalogName,
     SQLSMALLINT     NameLength1,
     SQLCHAR *     SchemaName,
     SQLSMALLINT     NameLength2,
     SQLCHAR *     TableName,
     SQLSMALLINT     NameLength3,
     SQLCHAR *     ColumnName,
     SQLSMALLINT     NameLength4);
****************************************/

SP_integer SPCDECL odbc_columns(SPAPI_ARG_PROTO_DECL SP_term_ref statement_handle_spec, char const * TableName)
{
  SQLRETURN rc;
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT StatementHandle = real_statement_handle(StatementHandleIndex);
  SP_SQLTCHAR *TableNameT = NULL; /* Needs cleanup */

  ODBC_HSTMT_CHECK(StatementHandle);
  rc = FROM_IENC_TO_TCHARS((char /* const */ *) TableName, TableNameT);
  if (rc != SQL_SUCCESS)
    {
      goto barf;
    }
  
  rc = SP_SQLColumnsT(StatementHandle,
                      NULL,
                      0,
                      NULL,
                      0,
                      TableNameT,
                      SQL_NTS,
                      NULL,
                      0);
 cleanup:
  FREE_IENC_TO_TCHARS(TableName, TableNameT);
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}

/****************************************
SQLRETURN SQLTables(
     SQLHSTMT       StatementHandle,
     SQLCHAR *      CatalogName,
     SQLSMALLINT    NameLength1,
     SQLCHAR *      SchemaName,
     SQLSMALLINT    NameLength2,
     SQLCHAR *      TableName,
     SQLSMALLINT    NameLength3,
     SQLCHAR *      TableType,
     SQLSMALLINT    NameLength4);
****************************************/

SP_integer SPCDECL odbc_tables(SPAPI_ARG_PROTO_DECL SP_term_ref statement_handle_spec, char const * TableName)
{
  SQLRETURN rc;
  SP_SQLTCHAR *TableNameT = NULL; /* Needs cleanup */
  SQLSMALLINT TableNameLen;
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT StatementHandle = real_statement_handle(StatementHandleIndex);

  ODBC_HSTMT_CHECK(StatementHandle);
  if (TableName[0] == '\0')
    {
      /* all tables */
      TableNameT = NULL;
      TableNameLen = 0;
    }
  else
    {
      /* [PM] 4.2 specific table */
      TableNameLen = SQL_NTS;
      rc = FROM_IENC_TO_TCHARS((char /* const */ *) TableName, TableNameT);
      if (rc != SQL_SUCCESS) { goto barf; }
    }

  rc = SP_SQLTablesT(StatementHandle,
                     NULL,
                     0,
                     NULL,
                     0,
                     TableNameT,
                     TableNameLen,
                     NULL,
                     0);
 cleanup:
  FREE_IENC_TO_TCHARS(TableName, TableNameT);
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}


/****************************************
SQLRETURN SQLConnect(
     SQLHDBC     ConnectionHandle,
     SQLCHAR *     ServerName,
     SQLSMALLINT     NameLength1,
     SQLCHAR *     UserName,
     SQLSMALLINT     NameLength2,
     SQLCHAR *     Authentication,
     SQLSMALLINT     NameLength3);
****************************************/

SP_integer SPCDECL odbc_connect(SPAPI_ARG_PROTO_DECL SP_term_ref connection_handle_spec,
                                char const * ServerName,
                                char const * UserName, char const * Authentication)
{
  SQLRETURN rc;
  SP_integer const ConnectionHandleIndex = decode_connection_handle_spec(connection_handle_spec);
  SQLHDBC ConnectionHandle = real_connection_handle(ConnectionHandleIndex);
  SP_SQLTCHAR *ServerNameT = NULL; /* needs cleanup */
  SP_SQLTCHAR *UserNameT = NULL; /* needs cleanup */
  SP_SQLTCHAR *AuthenticationT = NULL; /* needs cleanup */
  if (ConnectionHandle == SQL_NULL_HDBC)
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_INVALID_HANDLE;
      goto barf;
    }
  SP_ASSERT(!IS_FLAG_SET(state.connectionhandlestack[ConnectionHandleIndex].flags, CONNECTION_HANDLE_FLAG_CONNECTED));
  if (IS_FLAG_SET(state.connectionhandlestack[ConnectionHandleIndex].flags, CONNECTION_HANDLE_FLAG_CONNECTED))
    {
      /* [PM] 4.2 connecting should happen "atomically" together with allocating the connection  */
      SP_ASSERT(0);
      rc = ODBC_ERROR_INVALID_PARAMETER; /* [PM] 4.1 not a perfect error code for this, but no matter */
      goto barf;
    }
  rc = FROM_IENC_TO_TCHARS((char /* const */ *)ServerName, ServerNameT);
  if (rc != SQL_SUCCESS) { goto barf; }
  rc = FROM_IENC_TO_TCHARS((char /* const */ *)UserName, UserNameT);
  if (rc != SQL_SUCCESS) { goto barf; }
  rc = FROM_IENC_TO_TCHARS((char /* const */ *)Authentication, AuthenticationT);
  if (rc != SQL_SUCCESS) { goto barf; }

  DEBUG_PRINT((stderr, "ODBC_DBG: Calling SQLConnect(%p)\n", (void*)ConnectionHandle));
  rc = SP_SQLConnectT(ConnectionHandle,
                      ServerNameT,
                      SQL_NTS,
                      UserNameT,
                      SQL_NTS,
                      AuthenticationT,
                      SQL_NTS);
  ODBC_CHECK(rc);
  /* state.connectionhandlestack[ConnectionHandleIndex].is_connected = SPIO_TRUE; */
  SET_FLAG(state.connectionhandlestack[ConnectionHandleIndex].flags, CONNECTION_HANDLE_FLAG_CONNECTED);

  SP_ASSERT(SQL_SUCCEEDED(rc));
 cleanup:
  FREE_IENC_TO_TCHARS(ServerName, ServerNameT);
  FREE_IENC_TO_TCHARS(UserName, UserNameT);
  FREE_IENC_TO_TCHARS(Authentication, AuthenticationT);
  
  return rc;
 barf:
DEBUG_PRINT((stderr, "ODBC_DBG: ERROR: SQLConnect(%p)=%" SPRIdINTEGER "\n", (void*)ConnectionHandle, (SP_integer)rc));    
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}

#if !SP_SQL_UNICODE
/* Returns SPIO_TRUE if the result was successfully converted (if
   needed), otherwise truncates and returns SPIO_FALSE */
static int sanitize_string(SQLCHAR *in_buf, char *tmp_buf, size_t tmp_buf_size, char **pout_buf)
{
  int untruncated;
  /* converting to atom fails if not UTF-8 */
  if (SP_atom_from_string((char*)in_buf) == 0)
    {
      size_t i, j;
      /* [PM] 4.2 Not valid UTF-8. Some process specific locale or what-not. */
      untruncated = SPIO_TRUE; /* assume success */

      /* [PM] 4.2 As a workaround we convert byte by byte */
      for (i=0,j=0; in_buf[i] != (SQLCHAR)'\0'; i++)
        {
          unsigned char c = (unsigned char) in_buf[i];
          SP_ASSERT(j < tmp_buf_size);
          if ((c & 0x80) != 0) /* high bit set */
            {
              if (j+1 +1 < tmp_buf_size) /* room for two more plus NUL*/
                {
                  /* Each byte with high bit set will need two bytes in UTF-8 */
                  unsigned char const abcdefgh = c;
                  unsigned char const II000000 = (0x80 | 0x40);
                  unsigned char const O00000ab = ((abcdefgh >> 6) & 0x03); /* two msbits */
                  unsigned char const II0000ab = (II000000 | O00000ab);
                  unsigned char const O0cdefgh = (abcdefgh & 0x3F); /* six lsbits */
                  unsigned char const I0000000 = 0x80;
                  unsigned char const I0cdefgh = I0000000 | O0cdefgh;
                  tmp_buf[j++] = (char) II0000ab;
                  tmp_buf[j++] = (char) I0cdefgh;
                }
              else
                {
                  untruncated = SPIO_FALSE;
                  break;
                }
            }
          else              /* 7-bit */
            {
              if (j +1 < tmp_buf_size) /* room for one more plus NUL */
                {
                  tmp_buf[j++] = (char) c;
                }
              else
                {
                  untruncated = SPIO_FALSE;
                  break;
                }
            }
        }
      SP_ASSERT(j<tmp_buf_size);
      tmp_buf[j] = (char)'\0';

      *pout_buf = tmp_buf;
      SP_ASSERT(SP_atom_from_string((char*)*pout_buf) != 0);
    }
  else
    {
      untruncated = SPIO_TRUE;
      *pout_buf = (char *)in_buf;
    }
  return untruncated;
}
#endif  /* !SP_SQL_UNICODE */

/****************************************
SQLRETURN SQLDriverConnect(
     SQLHDBC     ConnectionHandle,
     SQLHWND     WindowHandle,
     SQLCHAR *     InConnectionString,
     SQLSMALLINT     StringLength1,
     SQLCHAR *     OutConnectionString,
     SQLSMALLINT     BufferLength,
     SQLSMALLINT *     StringLength2Ptr,
     SQLUSMALLINT     DriverCompletion);
****************************************/

/*
  '$odbc_driver_connect'(+ConnectionHandle, +InConnectionString,
                         -OutConnectionString, -ResultCode)
  foreign(odbc_driver_connect,
          '$odbc_driver_connect'(+integer, +codes, -string, 
                                 [-integer])).
*/

SP_integer SPCDECL odbc_driver_connect(SPAPI_ARG_PROTO_DECL SP_term_ref connection_handle_spec,
                                       char const * InConnectionString,
                                       char const ** OutConnectionString)
{
  SQLRETURN rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
  SQLSMALLINT StringLength2 = 0;
  SP_SQLTCHAR *InConnectionStringT = NULL; /* needs cleanup if != InConnectionString */
  SQLCHAR *OutConnectionStringBufferA = NULL; /* needs cleanup */
  SP_integer const ConnectionHandleIndex = decode_connection_handle_spec(connection_handle_spec);
  SQLHDBC ConnectionHandle = real_connection_handle(ConnectionHandleIndex);
  int retry_with_ascii;

#if SP_SQL_UNICODE
  retry_with_ascii = SPIO_TRUE;
#else  /* !SP_SQL_UNICODE */
  retry_with_ascii = SPIO_FALSE;
#endif	/* !SP_SQL_UNICODE */

  INIT_STATIC_BUFFER(state.OutConnectionStringBufferT);
  FREE_STATIC_IENC(state.OutConnectionStringBuffer_ienc);

  if (ConnectionHandle == SQL_NULL_HDBC)
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_INVALID_HANDLE;
      goto barf;
    }
  SP_SOFT_ASSERT(!IS_FLAG_SET(state.connectionhandlestack[ConnectionHandleIndex].flags, CONNECTION_HANDLE_FLAG_CONNECTED));

  rc = FROM_IENC_TO_TCHARS((char /* const */ *)InConnectionString, InConnectionStringT);
  if (rc != SQL_SUCCESS)
    {
      goto barf;
    }

 retry:
  DEBUG_PRINT((stderr, "ODBC_DBG: Calling SQLConnect(%p)\n", (void*)ConnectionHandle));
  rc = SP_SQLDriverConnectT(ConnectionHandle,
                            (SQLHWND)NULL,
                            InConnectionStringT,
                            SQL_NTS,
                            state.OutConnectionStringBufferT,
                            OutConnectionStringCharCount,
                            &StringLength2,
                            SQL_DRIVER_NOPROMPT);
  state.OutConnectionStringBufferT[OutConnectionStringCharCount-1] = ((SP_SQLTCHAR)'\0'); /* paranoia */

  if (!SQL_SUCCEEDED(rc) && retry_with_ascii) {
    retry_with_ascii = SPIO_FALSE;
    OutConnectionStringBufferA = odbc_malloc(OutConnectionStringCharCount);
    if (OutConnectionStringBufferA == NULL)
      {
	ODBC_BARF(ODBC_ERROR_OUT_OF_MEMORY);
      }
    /*

Note: PostgreSQL on Linux may use a non-standard port if two servers
are running (e.g. if Stack Builder was used to install a newer
server. Another symptom is that the corresponding psql can not
connect and that the port-number reported when psql fails is different
from the one reported by pgadmin for the server.

Note: PostgreSQL on Linux may give errors if 'ident' is configured as authentication method.

See http://odolshinu.wordpress.com/2012/03/16/postgresql-operationalerror-in-django/

     */
    rc = SQLDriverConnect(ConnectionHandle,
			  (SQLHWND)NULL,
			  (SQLCHAR /* discards const */ *) InConnectionString,
			  SQL_NTS,
			  OutConnectionStringBufferA,
			  OutConnectionStringCharCount,
			  &StringLength2,
			  SQL_DRIVER_NOPROMPT);
    if (!SQL_SUCCEEDED(rc))
      {
	/* Retry the original SP_SQLDriverConnectT() so we get its
	   diagnostic records (but, at least for postgresql odbc the
	   Unicode diagnostics records was uninformative
	   ('[unixODBC]c') when the ASCII records pointed out the real
	   problem ('[unixODBC]FATAL: Ident authentication failed for
	   user "spluser"') 

	   Note: That particular error "... Ident authentication
	   failed..." is due to a configuration error, see
	   http://odolshinu.wordpress.com/2012/03/16/postgresql-operationalerror-in-django/.

	*/
	SP_ASSERT(!retry_with_ascii);
	goto retry;
      }
    else
      {
	/* The driver is buggy if SP_SQLDriverConnectT fails but SP_SQLDriverConnect does not */
	SP_SOFT_ASSERT(SPIO_FALSE);
        /* Prevent the uninitialized state.OutConnectionStringBufferT from being used below (rc is success here!) */
        state.OutConnectionStringBufferT[0] = 0;
#if !ODBC_FORCE_BUILD && !POSTPONE
#error "fixme: convert from OutConnectionStringBufferA to state.OutConnectionStringBufferT"
#endif	/* !POSTPONE */
      }
    
  }

  ODBC_CHECK(rc);
  
  /* state.connectionhandlestack[ConnectionHandleIndex].is_connected = SPIO_TRUE; */
  SET_FLAG(state.connectionhandlestack[ConnectionHandleIndex].flags, CONNECTION_HANDLE_FLAG_CONNECTED);

  {
    SQLRETURN rc2 = FROM_BUF_TO_IENC(state.OutConnectionStringBufferT, state.OutConnectionStringBuffer_ienc);
    if (rc2 != SQL_SUCCESS)
      {
        /* We have already connected so what should we do? */
        SP_SOFT_ASSERT(0);
        SP_ASSERT(state.OutConnectionStringBuffer_ienc == NULL);
        state.OutConnectionStringBuffer_ienc = NULL;
      }
  }

  if (state.OutConnectionStringBuffer_ienc != NULL)
    {
#if SP_SQL_UNICODE
      /* [PM] 4.2.2 No need to sanitize when already converted to SP-UTF-8 */
      *OutConnectionString = state.OutConnectionStringBuffer_ienc;
#else  /* !SP_SQL_UNICODE */
      {
        char *out_buf = NULL;
        if (!sanitize_string((/* cast to avoid signedness mismatch */ SQLCHAR*) state.OutConnectionStringBuffer_ienc,
                             state.OutConnectionStringBuffer_tmp_buf, OutConnectionStringBuffer_tmp_buf_size,
                             &out_buf))
          {
            /* truncated */
            SP_SOFT_ASSERT(0);
          }
        *OutConnectionString = out_buf;
      }
#endif  /* !SP_SQL_UNICODE */
    }
  else
    {
      *OutConnectionString = "Connection string not available";
    }
  SP_ASSERT(SQL_SUCCEEDED(rc));
 cleanup:
  FREE_IENC_TO_TCHARS(InConnectionString, InConnectionStringT);
  if (OutConnectionStringBufferA != NULL)
    {
      odbc_free(OutConnectionStringBufferA);
      OutConnectionStringBufferA = NULL;
    }
  return rc;
 barf:
  SP_ASSERT(!SQL_SUCCEEDED(rc));
  DEBUG_PRINT((stderr, "ODBC_DBG: ERROR: SQLDriverConnect(%p)=%" SPRIdINTEGER "\n", (void*)ConnectionHandle, (SP_integer)rc));

  /* [PM] 4.2 Barf sets all outputs */
  *OutConnectionString = "";
  goto cleanup;
}

/****************************************
SQLRETURN SQLGetDiagRec(
     SQLSMALLINT     HandleType,
     SQLHANDLE     Handle,
     SQLSMALLINT     RecNumber,
     SQLCHAR *     Sqlstate,
     SQLINTEGER *     NativeErrorPtr,
     SQLCHAR *     MessageText,
     SQLSMALLINT     BufferLength,
     SQLSMALLINT *     TextLengthPtr);
****************************************/

/*
  '$odbc_get_diag_rec'(+HandleType, +HandleSpec, +RecNumber, -Sqlstate,
                       -NativeError, -MessageText, -TextLength, -ResultCode)
  foreign(odbc_get_diag_rec,
          '$odbc_get_diag_rec'(+integer, +term, +integer, -string,
                               -integer, -string, -integer, [-integer])
*/

SP_integer SPCDECL odbc_get_diag_rec(SPAPI_ARG_PROTO_DECL SP_integer HandleType,
                             SP_term_ref handle_spec,
                             SP_integer RecNumber, char const ** SqlstateAtom,
                             SP_integer * NativeError,
                             char const ** MessageTextAtom, SP_integer * TextLengthPtr)
{
  SQLHANDLE RealHandle;
  SQLINTEGER LocalNativeError;
  SQLRETURN rc;
  SQLSMALLINT TextLength = 0;

  INIT_STATIC_BUFFER(state.MessageTextT);
  INIT_STATIC_BUFFER(state.SqlstateT);
  FREE_STATIC_IENC(state.MessageText_ienc);
  FREE_STATIC_IENC(state.Sqlstate_ienc);

  if (SQL_HANDLE_STMT == HandleType) {
    SP_integer const StatementHandleIndex = decode_statement_handle_spec(handle_spec); /* may overwrite statement_handle_spec */
    SQLHSTMT real_handle = real_statement_handle(StatementHandleIndex);

    ODBC_HSTMT_CHECK(real_handle);
    RealHandle = real_handle;
  } else if (SQL_HANDLE_DBC == HandleType) {
    SP_integer const ConnectionHandleIndex = decode_connection_handle_spec(handle_spec);
    SQLHDBC real_handle = real_connection_handle(ConnectionHandleIndex);

    ODBC_HDBC_CHECK(real_handle);
    RealHandle = real_handle;
  } else if (SQL_HANDLE_ENV == HandleType) {
    SP_integer const EnvironmentHandleIndex = decode_environment_handle_spec(handle_spec);
    SQLHENV real_handle = real_environment_handle(EnvironmentHandleIndex);

    ODBC_HENV_CHECK(real_handle);
    RealHandle = real_handle;
  } else {
    SP_ASSERT(0);
    ODBC_BARF(ODBC_ERROR_INVALID_PARAMETER);
  }
  
  rc = SP_SQLGetDiagRecT((SQLSMALLINT)HandleType,
                         RealHandle,
                         (SQLSMALLINT)RecNumber,
                         state.SqlstateT,
                         &LocalNativeError,
                         state.MessageTextT,
                         MessageTextCharCount,
                         &TextLength);


  /* [PM] 4.2.1 SQLGetDiagRec() may fail (e.g. if no db driver on
     giraff). */
  ODBC_CHECK(rc);

  state.SqlstateT[STATIC_BUFFER_NUMBER_OF_ELEMENTS(state.SqlstateT)-1] = (SP_SQLTCHAR)'\0'; /* paranoia */

  {
    SQLRETURN rc2;
    rc2 = FROM_BUF_TO_IENC(state.MessageTextT, state.MessageText_ienc);
    if (rc2 != SQL_SUCCESS)
      {
        SP_SOFT_ASSERT(0);
        rc = rc2;
        goto barf;
      }
    rc2 = FROM_BUF_TO_IENC(state.SqlstateT, state.Sqlstate_ienc);
    if (rc2 != SQL_SUCCESS)
      {
        SP_SOFT_ASSERT(0);
        rc = rc2;
        goto barf;
      }
  }
  *SqlstateAtom = state.Sqlstate_ienc;
  *NativeError = LocalNativeError;

  SP_ASSERT(state.MessageText_ienc != NULL);

#if SP_SQL_UNICODE
  *MessageTextAtom = state.MessageText_ienc;
#else /* !SP_SQL_UNICODE */
  /* MessageText is not UTF-8 and can contain 8-bit chars, e.g. in
     Swedish locale. Should investigate whether we can do better when
     converting to UTF-8. */
  {
    char *out_buf = NULL;
    if (!sanitize_string(state.MessageTextT,
                         state.MessageText_tmp_buf, MessageText_tmp_buf_size,
                         &out_buf))
      {
        /* truncated */
        SP_SOFT_ASSERT(0);
      }
    *MessageTextAtom = out_buf;
  }
#endif  /* !SP_SQL_UNICODE */

  /* [PM] 4.2.1 TextLength is useless (and misleading if non-ASCII)
     but part of error exception so leave it in for backward
     compatibility. */
  *TextLengthPtr = TextLength;
  SP_ASSERT(SQL_SUCCEEDED(rc));
 cleanup:
  return rc;
 barf:
  SP_ASSERT(!SQL_SUCCEEDED(rc));
  /* [PM] 4.2 Barf sets all outputs */
  *SqlstateAtom = "";
  *NativeError = 0;
  *MessageTextAtom = "";
  *TextLengthPtr = 0;
  goto cleanup;
}

/* 
   Concatenate the names of the atoms in the proper list atom_list (which is not modified).
   Two temporary term-refs, tmp1 and tmp2, must be allocated by the caller and will be overwritten by this function.
   The buffer buf should have a size of *psize. If buf is NULL then *psize must be zero.

   If the buffer is large enough, the concatenated atom names and a
   NUL termination is written to the buffer. If the buffer is not
   large enough, the contents of the valid part of the buffer is
   unspecified.

   Regardless of whether the buffer is large enough, the required
   length, including a NUL termination, is returned in *psize.

   It is not an error to pass a too small buffer, and no error is
   returned. This case can be determined by noting that *psize has
   increased after a successful call.

   I.e. An initial call, with *psize == 0, can be used to determine the required size of the buffer.

   If the list is not a proper list, or if any of its elements are not atoms, an error is returned.
 */
static spio_t_error_code concat_atom_list(SP_term_ref atom_list, SP_term_ref tmp1, SP_term_ref tmp2, char *buf, size_t *psize)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  SP_term_ref data = tmp1;
  size_t const buf_size = *psize;
  size_t nbytes = 0;

  if (!SP_put_term(data, atom_list))
    {
      SP_ASSERT(SPIO_FALSE);
      BARF(SPIO_E_INTERNAL_ERROR);
    }

  while (SP_is_list(data))
    {
      char const *tmp_string = NULL;
      size_t tmp_size;
      if (!SP_get_arg(1, data, tmp2))
        {
          SP_ASSERT(SPIO_FALSE);
          break;
        }
      if (!SP_get_string(tmp2, &tmp_string))
        {
          break;
        }
      tmp_size = strlen(tmp_string);
      if ((nbytes + tmp_size) < buf_size)
        {
          memcpy(buf+nbytes, tmp_string, tmp_size);
        }
      nbytes += tmp_size;

      /* Do not move down list until previous argument has been processed/validated */
      if (!SP_get_arg(2, data, data))
        {
          SP_ASSERT(SPIO_FALSE);
          break;
        }
    }

  {
    char const *tmp_string = NULL;  
    if (!( SP_get_string(data, &tmp_string) && strcmp(tmp_string, "[]") == 0 ) )
      {
        /* Not terminated by '[]' */
        BARF(SPIO_E_PARAMETER_ERROR);
      }
  }

  *psize = (nbytes +1);         /* +1 for NUL */

  if (nbytes < buf_size)
    {
      buf[nbytes] = 0; /* NUL termination */
    }
  else
    {
      /* We should not report this as an error: BARF(SPIO_E_INSUFFICIENT_BUFFER); */
    }

  code = SPIO_S_NOERR;
 cleanup:
  return code;
 barf:
  goto cleanup;
}

/* [PM] 4.2 Return a static buffer containing the data when treated as a SQL
   CHAR string, i.e. an empty list is treated as an empty
   string. Return error code if the data cannot be treated as a
   string. Should not do any fancy conversion, like converting
   integers to their base 10 representations.

   Returns success with *putf8_data == NULL if the value should be
   interpreted as an database null.

   Does not modify the term-ref argument (data)

 */
#define TERM_AS_STRING_OPTION_SQL_BIT_WORKAROUND 0x0001 /* treat integer 0 and 1 as "0" and "1" respectively. */
static spio_t_error_code term_as_string(SP_term_ref data, char const **putf8_data, spio_t_bits options)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  char const * utf8_bytes;     /* pseudo static, no cleanup needed. */

  if (SP_is_atom(data))
    {
      SP_atom tmp_atom = 0;

      if (SP_get_atom(data, &tmp_atom) && tmp_atom == state.null)
        {
          *putf8_data = NULL;
          code = SPIO_S_NOERR;
          goto cleanup;
        }

      if (!SP_get_string(data, &utf8_bytes))
        {
          BARF(SPIO_E_PARAMETER_ERROR);
        }
      if (strcmp(utf8_bytes, "[]") == 0)
        {
          /* [PM] 4.2 Was an empty list, i.e. "" so treat it as empty
             string. Use [0'[,0']] if the two char string "[]" is
             desired.*/
          utf8_bytes = "";
        }
    } else
    if (SP_is_list(data))
      {
	utf8_bytes = NULL;
        if (SP_get_list_codes(data, &utf8_bytes))
          {
            /* Was list of character codes. We are done. */
	    SP_ASSERT(utf8_bytes != NULL);
          }
        else   /* Not a list of codes, see if it is a list of atoms */
          {
            size_t nbytes;
            SP_term_ref tmp1 = SP_new_term_ref();
            SP_term_ref tmp2 = SP_new_term_ref();

            if (state.term_as_string_store != NULL)
              {
                SP_free(state.term_as_string_store);
                state.term_as_string_store = NULL;
              }

            nbytes = 0; /* size of buffer on first call */
            if (SPIO_FAILED(concat_atom_list(data, tmp1, tmp2, NULL, &nbytes)))
              {
                BARF(SPIO_E_PARAMETER_ERROR);
              }
            SP_ASSERT(nbytes > 0);
            /* Was a list of atoms, totalling nbytes in internal encoding (including the terminating NUL) */
            state.term_as_string_store = odbc_malloc(nbytes); /* nbytes includes NUL */
            if (state.term_as_string_store == NULL)
              {
                BARF(SPIO_E_OUT_OF_MEMORY);
              }

            code = concat_atom_list(data, tmp1, tmp2, state.term_as_string_store, &nbytes);
            if (SPIO_FAILED(code)) /* cannot happen */
              {
                SP_ASSERT(SPIO_FALSE);
                BARF(SPIO_E_INTERNAL_ERROR);
              }
            utf8_bytes = state.term_as_string_store;
          }
      }
    else if (IS_FLAG_SET(options, TERM_AS_STRING_OPTION_SQL_BIT_WORKAROUND))
      {
        SP_integer ival = 4711;

	/* [PM] 4.2.2 Sometimes (psqlODBC @Mac OS X) SQL_BIT is
	   reported as SQL_C_CHAR. To work around this we treat an
	   integer term 0 or 1 as the strings "0" and "1",
	   respectively, and rely on ODBC/database to do the correct
	   conversion back to SQL_BIT.

	   http://msdn.microsoft.com/en-us/library/windows/desktop/ms714651(v=vs.85).aspx
	   says: "...the ODBC SQL data types to which C character data
	   may be converted... SQL_BIT [if data is 0 or 1] ..."

	*/
	if (SP_is_integer(data) && SP_get_integer(data, &ival) && (ival == 0 || ival == 1))
	  {
	    utf8_bytes = ( ival == 0 ? "0" : "1" );
	    DEBUG_PRINT2("Treating integer %" SPRIdINTEGER " as (boolean) string \"%s\"\n", (SP_integer)ival, utf8_bytes);
	  }
	else
	  {
	    BARF(SPIO_E_PARAMETER_ERROR);
	  }
      }
    else
      {
        BARF(SPIO_E_PARAMETER_ERROR);
      }
  /* utf8_bytes == NULL means SQL null value */
  if (putf8_data != NULL)
    {
      *putf8_data = utf8_bytes;
    }
  code = SPIO_S_NOERR;
 cleanup:
  return code;
 barf:
  goto cleanup;
}

/* True iff each position s[i] is equal to either alt1[i] or
   alt2[i]. The strings alt1 and alt2 must be the same length.
 */

static int string_equal_either(char const *s, char const *alt1, char const *alt2)
{
  size_t i;
  SP_ASSERT(strlen(alt1) == strlen(alt2));

  for (i = 0; s[i] != '\0' && (s[i] == alt1[i] || s[i] == alt2[i]); i++)
    {
      ; /* empty */
    }
  return s[i] == '\0';
}

static int is_boolean_false_string(char const *s)
{
  return string_equal_either(s, "no", "NO");
}

static int is_boolean_true_string(char const *s)
{
  return string_equal_either(s, "yes", "YES");
}


/* [PM] 4.2 Return true iff term is a compound name/arity term. */
static int has_functor(SP_term_ref term, SP_atom name, int arity)
{
  SP_atom actual_name;
  int actual_arity;

  if (SP_is_compound(term)
      &&
      SP_get_functor(term, &actual_name, &actual_arity)
      &&
      arity == actual_arity
      &&
      name == actual_name)
    {
      return SPIO_TRUE;
    }
  return SPIO_FALSE;
}

/* Get an integer at position argno of compound, using scratch for temporary work. Return 0 on failure. */
#define GET_INTEGER_ARG_TYPE__BODY(TYPE)  do {  \
  typedef TYPE value_type;                      \
  value_type value;                             \
  SP_integer tmp;                               \
                                                \
  if (!SP_get_arg(argno, compound, scratch)     \
      ||                                        \
      !SP_get_integer(scratch, &tmp))           \
    {                                           \
      return 0;                                 \
    }                                           \
  value = (value_type)tmp;                      \
  if (value != tmp)                             \
    {                                           \
      return 0;                                 \
    }                                           \
  *pvalue = value;                              \
  return 1;                                     \
} while(0)

/* [PM] 4.2 DEFINE_GET_INTEGER_ARG_TYPE(SQLSMALLINT) defines the function get_integer_arg_SQLSMALLINT(). */
#define DEFINE_GET_INTEGER_ARG_TYPE(TYPE)                               \
static int get_integer_arg_ ## TYPE(SP_term_ref compound, SP_term_ref scratch, int argno, TYPE *pvalue) \
{                                                                       \
  GET_INTEGER_ARG_TYPE__BODY(TYPE);                                     \
}

DEFINE_GET_INTEGER_ARG_TYPE(SQLUSMALLINT);
DEFINE_GET_INTEGER_ARG_TYPE(SQLSMALLINT);
/* DEFINE_GET_INTEGER_ARG_TYPE(SQLINTEGER); */
DEFINE_GET_INTEGER_ARG_TYPE(SQLUINTEGER);





/* [PM] 4.2 TargetType should be something like SQL_C_SLONG or SQL_C_DEFAULT if the type is unknown. */
static struct buflist *allocate_buflist(SP_integer StatementHandleIndex, SQLSMALLINT TargetType, size_t bufsize)
{
  struct statementhandle *entry = real_statement_handle_entry(StatementHandleIndex);
  void * buf = NULL;
  struct buflist *bl = NULL;
  struct buflist *result;

  /* Caller should ensure valid index */
  SP_ASSERT(entry != NULL && entry->real_handle != SQL_NULL_HSTMT);
  if (bufsize == 0)
    {
      buf = NULL;
    }
  else
    {
      buf = odbc_malloc(bufsize);
      if (buf == NULL)
        {
          goto barf;
        }
    }

  {
    bl = (struct buflist *)odbc_malloc(sizeof(struct buflist));

    if (bl == NULL)
      {
        goto barf;
      }
    bl->size = bufsize;
    bl->TargetType = TargetType;
    bl->flags = 0;
    bl->buf = NULL;               /* make safe for cleanup */
    bl->ColumnNumber = 0; /* [PM] 4.2.2 set by caller if needed */
  }
  if (buf != NULL)
    {
      /* [PM] 4.2 We assume that it will soon be bound (using
         SQLBindParameter ([PM] 4.2.2. no longer used: or SQLBindCol)
         so before subsequent freeing of buffers we must use
         SQL_UNBIND et al on the statement handle. */
      entry->is_definitely_unbound = SPIO_FALSE;
    }

  bl->buf = buf; /* [PM] 4.2.2 May be NULL if USE_GET_DATA */
  buf = NULL;                 /* protect from cleanup */
  bl->next = state.statementhandlestack[StatementHandleIndex].buffers;
  state.statementhandlestack[StatementHandleIndex].buffers = bl;
  result = bl;
  DEBUG_PRINT((stderr, "\nODBC_DBG: allocating buflist: %p, buf=%p, size=%" SPRIdINTEGER "\n", (void*)bl, (void*)buf, (SP_integer)bufsize));
  bl = NULL;                  /* protect from cleanup */

  DEBUG_PRINT_HANDLE_STACKS;

  SP_ASSERT(result != NULL);
 cleanup:
  if (bl != NULL)
    {
      odbc_free(bl->buf); /* may be NULL */
      odbc_free(bl);
    }
  if (buf != NULL)
    {
      odbc_free(buf);
    }
  return result;
 barf:
  result = NULL;
  goto cleanup;
}

/* [PM] 4.2 Allocate and inititialize buffer contents. If bufsize==0 and bytes == NULL, then set BUFLIST_FLAG_NULL */
static struct buflist *allocate_buflist_data(SP_integer StatementHandleIndex, SQLSMALLINT TargetType, size_t bufsize, void const *bytes)
{
  struct buflist * bl = allocate_buflist(StatementHandleIndex, TargetType, bufsize);
  SP_ASSERT(bytes != NULL || bufsize == 0);
  if (bl != NULL)
    {
      SP_ASSERT( !IS_FLAG_SET(bl->flags,BUFLIST_FLAG_NULL) );
      if (bufsize == 0 && bytes == NULL)
        {
          SET_FLAG(bl->flags, BUFLIST_FLAG_NULL);
        }
      else if (bufsize > 0)
        {
          memcpy(bl->buf, bytes, bufsize);
        }
    }
  return bl;
}

static SP_integer buflist_index(struct buflist *bl)
{
  return (SP_integer)bl;
}


/* [PM] 4.2 Return address of pointer to buflist entry (e.g. address of the
   previous next field). Never null but *value is NULL if buffer was
   not found. */
static struct buflist ** find_buflist_entry_location1(SP_integer StatementHandleIndex, void *target)
{
  struct buflist ** blp;
  /* Caller should validate index */
  SP_ASSERT(real_statement_handle(StatementHandleIndex) != SQL_NULL_HSTMT);

  blp = &(state.statementhandlestack[StatementHandleIndex].buffers);
  while (*blp != NULL) {
    struct buflist * bl = *blp;
    if (bl == target) {
      break;
    }
    blp = &(*blp)->next;
  }

  SP_ASSERT(blp != NULL);
  return blp;
}

static struct buflist ** find_buflist_entry_location(SP_integer StatementHandleIndex, SP_integer buffer_id)
{
  return find_buflist_entry_location1(StatementHandleIndex, (void*) buffer_id);
}


/* [PM] 4.2 Return address of buflist entry, or NULL if not
   found. StatementHandleIndex is trusted and must be validated by
   caller */
static struct buflist * find_buflist_entry(SP_integer StatementHandleIndex, SP_integer buffer_id)
{
  return *find_buflist_entry_location(StatementHandleIndex, buffer_id);
}


/*
De-allocate a buffer

foreign(free_buffer,
        '$free_buffer'(+integer, +address, [-integer])).

Return PROLOG_NATIVE_CODE_ERROR on error, PROLOG_NATIVE_CODE_SUCCESS on success.
*/
SP_integer SPCDECL free_buffer(SPAPI_ARG_PROTO_DECL SP_term_ref statement_handle_spec, SP_term_ref buffer_spec)
{
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  struct statementhandle *entry = real_statement_handle_entry(StatementHandleIndex);
  DEBUG_PRINT_HANDLE_STACKS;

  /* [PM] 4.2 Validate handle */
  {
    SQLHSTMT StatementHandle = (entry == NULL ? SQL_NULL_HSTMT : entry->real_handle);

    if (StatementHandle == SQL_NULL_HSTMT) {
      SP_ASSERT(0);
      goto barf;
    }
  }

  {
    SP_integer buffer_id = decode_buffer_spec(buffer_spec); /* may overwrite buffer_spec */
    struct buflist ** const blp = find_buflist_entry_location(StatementHandleIndex, buffer_id);
    struct buflist * const bl = *blp;

    DEBUG_PRINT((stderr, "\nODBC_DBG: Attempting to odbc_free buffer %p\n", (void*)buffer_id));

    if (bl == NULL)
      {
        /* not found */
        SP_ASSERT(0);
        goto barf;
      }

    if (!entry->is_definitely_unbound)
      {
        SQLRETURN rc;
        DEBUG_PRINT((stderr, "\nODBC_DBG: Warning: free_buffer(%p), unbinding statementhandle %p (index==%" SPRIdINTEGER ")\n", (void*)buffer_id, (void*)entry, (SP_integer)StatementHandleIndex));
        /* 4.2 Ensure statement is not bound to this buffer (or any other buffer) */

#if !POSTPONE
#error "[PM] 4.2.2 Does it really make sense to unbind all other buffers for this statement handle?"
#endif  /* !POSTPONE */
        rc = unbind_statement(entry);
        SP_SOFT_ASSERT(SQL_SUCCEEDED(rc));
        (void)rc;
      }

    SP_ASSERT(entry->is_definitely_unbound);
    free_buflist_entry_at(blp);
  }

  return PROLOG_NATIVE_CODE_SUCCESS;
 barf:
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  return PROLOG_NATIVE_CODE_ERROR;
}


/* ODBC__SQL_TYPE_ID__TO__C_TYPE_ID(SQL_INTEGER) becomes SQL_C_SLONG */
#define ODBC__SQL_TYPE_ID__TO__C_TYPE_ID(SQL_TYPE_ID) odbc__sql_type_id__to__c_type_id__map__ ## SQL_TYPE_ID
/* ODBC__C_TYPE_ID__TO__C_TYPE(SQL_C_SLONG) becomes SQLINTEGER */
#define ODBC__C_TYPE_ID__TO__C_TYPE(C_TYPE_ID) odbc__c_type_id__to__c_type__map__ ## C_TYPE_ID

#if ODBC_BROKEN_SHORT
#if 0 /* Caller must manually figure out what to do if ODBC_BROKEN_SHORT */
#define odbc__sql_type_id__to__c_type_id__map__SQL_SMALLINT SQL_C_SSHORT
#define                   odbc__c_type_id__to__c_type__map__SQL_C_SSHORT SQLSMALLINT
#endif /* 0 */
#else  /* !ODBC_BROKEN_SHORT */
#define odbc__sql_type_id__to__c_type_id__map__SQL_SMALLINT SQL_C_SSHORT
#define                   odbc__c_type_id__to__c_type__map__SQL_C_SSHORT SQLSMALLINT
#endif  /* !ODBC__C_TYPE_ID__TO__C_TYPE__MAP__SQL_C_SSHORT */

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTEGER  SQL_C_SLONG
#define                   odbc__c_type_id__to__c_type__map__SQL_C_SLONG SQLINTEGER

#define odbc__sql_type_id__to__c_type_id__map__SQL_REAL     SQL_C_FLOAT
#define                   odbc__c_type_id__to__c_type__map__SQL_C_FLOAT SQLREAL

#define odbc__sql_type_id__to__c_type_id__map__SQL_FLOAT    SQL_C_DOUBLE
#define odbc__sql_type_id__to__c_type_id__map__SQL_DOUBLE   SQL_C_DOUBLE
#define                   odbc__c_type_id__to__c_type__map__SQL_C_DOUBLE SQLDOUBLE

#define odbc__sql_type_id__to__c_type_id__map__SQL_BIT      SQL_C_BIT
#define                   odbc__c_type_id__to__c_type__map__SQL_C_BIT SQLCHAR

#define odbc__sql_type_id__to__c_type_id__map__SQL_TINYINT  SQL_C_STINYINT
#define                   odbc__c_type_id__to__c_type__map__SQL_C_STINYINT SQLSCHAR

#define odbc__sql_type_id__to__c_type_id__map__SQL_BIGINT   SQL_C_SBIGINT
#define                   odbc__c_type_id__to__c_type__map__SQL_C_BIGINT SQLBIGINT

#define odbc__sql_type_id__to__c_type_id__map__SQL_TYPE_DATE SQL_C_TYPE_DATE
#define                   odbc__c_type_id__to__c_type__map__SQL_C_TYPE_DATE SQL_DATE_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_TYPE_TIME SQL_C_TYPE_TIME
#define                   odbc__c_type_id__to__c_type__map__SQL_C_TYPE_TIME SQL_TIME_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_TYPE_TIMESTAMP SQL_C_TYPE_TIMESTAMP
#define                   odbc__c_type_id__to__c_type__map__SQL_C_TYPE_TIMESTAMP SQL_TIMESTAMP_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_MONTH SQL_C_INTERVAL_MONTH
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_MONTH SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_YEAR SQL_C_INTERVAL_YEAR
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_YEAR SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_YEAR_TO_MONTH SQL_C_INTERVAL_YEAR_TO_MONTH
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_YEAR_TO_MONTH SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_DAY SQL_C_INTERVAL_DAY
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_DAY SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_HOUR SQL_C_INTERVAL_HOUR
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_HOUR SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_MINUTE SQL_C_INTERVAL_MINUTE
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_MINUTE SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_SECOND SQL_C_INTERVAL_SECOND
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_SECOND SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_DAY_TO_HOUR SQL_C_INTERVAL_DAY_TO_HOUR
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_DAY_TO_HOUR SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_DAY_TO_MINUTE SQL_C_INTERVAL_DAY_TO_MINUTE
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_DAY_TO_MINUTE SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_DAY_TO_SECOND SQL_C_INTERVAL_DAY_TO_SECOND
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_DAY_TO_SECOND SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_HOUR_TO_MINUTE SQL_C_INTERVAL_HOUR_TO_MINUTE
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_HOUR_TO_MINUTE SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_HOUR_TO_SECOND SQL_C_INTERVAL_HOUR_TO_SECOND
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_HOUR_TO_SECOND SQL_INTERVAL_STRUCT

#define odbc__sql_type_id__to__c_type_id__map__SQL_INTERVAL_MINUTE_TO_SECOND SQL_C_INTERVAL_MINUTE_TO_SECOND
#define                   odbc__c_type_id__to__c_type__map__SQL_C_INTERVAL_MINUTE_TO_SECOND SQL_INTERVAL_STRUCT



/* [PM] 4.2 Map a SQL data type, e.g. SQL_INTEGER, as returned in
   DataType argument of SQLDescribeCol(), into the corresponding C
   data type, e.g. SQL_C_SLONG, as used with SQLBindCol TargetType
   argument. Also returns the size necessary to hold the C data
   type, e.g. SQLINTEGER. 

   Returns SQL_SUCCESS with *TargetValuePtr set appropriately but with
   *SizePtr == 0 for variable size data, like SQL_CHAR.

   Similar to a combination of sql_to_c_type_numeric() and column_size_numeric() and parts of buffer_get_data().

   See "Converting Data from SQL to C Data Types" at <http://msdn.microsoft.com/en-us/library/ms709280(v=VS.85).aspx>.
   See "C Data Types" <http://msdn.microsoft.com/en-us/library/ms714556(v=VS.85).aspx>

   NOTE: for each TargetType emitted by this function there must be
   corresponding cases in buffer_get_data(), odbc_bind_parameter() (and elsewhere)
*/
static SQLRETURN map_sql_data_type_to_c_data_type_and_size(SQLSMALLINT DataType, SQLSMALLINT *TargetTypePtr, SQLLEN *SizePtr)
{
  SQLRETURN rc;
  SQLSMALLINT TargetType;
  SQLLEN Size;

  rc = SQL_SUCCESS;
  switch (DataType)
    {
    case SQL_CHAR:
    case SQL_VARCHAR:
    case SQL_LONGVARCHAR:
      TargetType = SQL_C_CHAR;
      Size = 0;
      break;

    case SQL_WCHAR:
    case SQL_WVARCHAR:
    case SQL_WLONGVARCHAR:
#if ENABLE_WCHAR
      TargetType = SQL_C_WCHAR;
#else  /* !ENABLE_WCHAR */
      /* [PM] 4.2 We fake it. This will work for values that only
         contain plain SQL_C_CHARs 
         (PM] 4.2.2 or if UTF-8 and CONNECTION_HANDLE_FLAG_UTF8_CHAR)*/
      TargetType = SQL_C_CHAR;
#endif /* !ENABLE_WCHAR */
      Size = 0;
      break;
      
      /* [PM] 4.2 FIXME: Support these (as SQL_CHAR? Perhaps as bignums) */
    case SQL_DECIMAL:
    case SQL_NUMERIC:
      SP_SOFT_ASSERT(0);
      rc = ODBC_ERROR_UNSUPPORTED_DATA_TYPE; /* Hmm */
      goto barf;

    case SQL_SMALLINT:
      /* [PM] 4.2 SQL_SMALLINT does not tell us enough, i.e. whether to use a signed or unsigned type.*/
      /* [PM] 4.2 We could force use of SQL_C_SLONG/SQLINTEGER which
         would ensure that both signed and unsigned SMALLINT would
         fit. */
#if ODBC_BROKEN_SHORT
      TargetType = SQL_C_SLONG;
      Size = sizeof(SQLINTEGER);
#else  /* !ODBC_BROKEN_SHORT */
      TargetType = SQL_C_SSHORT;
      Size = sizeof(SQLSMALLINT);
#endif  /* !ODBC_BROKEN_SHORT */
      break;

    case SQL_INTEGER:
      /* SQL_INTEGER does not tell us enough, i.e. whether to use a signed or unsigned type. */
      TargetType = SQL_C_SLONG;
      Size = sizeof(SQLINTEGER);
      break;

    case SQL_REAL:
      TargetType = SQL_C_FLOAT;
      Size = sizeof(SQLREAL);
      break;

    case SQL_FLOAT:
    case SQL_DOUBLE:
      TargetType = SQL_C_DOUBLE;
      COMPILE_TIME_ASSERT(sizeof(SQLDOUBLE) == sizeof(SQLFLOAT));
      Size = sizeof(SQLDOUBLE);
      break;

    case SQL_BIT:
      TargetType = SQL_C_BIT;
      Size = sizeof(SQLCHAR);
      break;

    case SQL_TINYINT:
      /* [PM] 4.2 See SQL_SMALLINT for how we could treat this as SQL_INTEGER. */
      TargetType = SQL_C_STINYINT;
      Size = sizeof(SQLSCHAR);  /* (!) */
      break;

    case SQL_BIGINT:
      /* [PM] 4.2 See SQL_INTEGER for a discussion about the signedness problem */
      TargetType = SQL_C_SBIGINT;
      Size = sizeof(SQLBIGINT);
      break;

      /* [PM] 4.2 FIXME: Support these */
    case SQL_BINARY:
    case SQL_VARBINARY:
    case SQL_LONGVARBINARY:
      SP_SOFT_ASSERT(0);
      rc = ODBC_ERROR_UNSUPPORTED_DATA_TYPE; /* Hmm */
      goto barf;

    case SQL_TYPE_DATE:
      TargetType = SQL_C_TYPE_DATE;
      Size = sizeof(SQL_DATE_STRUCT);
      break;
    case SQL_TYPE_TIME:
      TargetType = SQL_C_TYPE_TIME;
      Size = sizeof(SQL_TIME_STRUCT);
      break;
    case SQL_TYPE_TIMESTAMP:
      TargetType = SQL_C_TYPE_TIMESTAMP;
      Size = sizeof(SQL_TIMESTAMP_STRUCT);
      break;

#ifdef SQL_TYPE_UTCDATETIME     /* [PM] 4.2 Not on Mac OS X 10.5 */
    case SQL_TYPE_UTCDATETIME:
      /* [PM] 4.2 I have not been able to find any information about a
         suitable C type for UTC times (except, perhaps, SQL_CHAR) */
      SP_SOFT_ASSERT(0);                 /* [PM] 4.2 Tell me if this happens */
      rc = ODBC_ERROR_UNSUPPORTED_DATA_TYPE; /* Hmm */
      goto barf;
#endif  /* SQL_TYPE_UTCDATETIME */

#ifdef SQL_TYPE_UTCTIME     /* [PM] 4.2 Not on Mac OS X 10.5 */
    case SQL_TYPE_UTCTIME:
      /* [PM] 4.2 I have not been able to find any information about a
         suitable C type for UTC times (except, perhaps, SQL_CHAR) */
      SP_SOFT_ASSERT(0);                 /* [PM] 4.2 Tell me if this happens */
      rc = ODBC_ERROR_UNSUPPORTED_DATA_TYPE; /* Hmm */
      goto barf;
#endif  /* SQL_TYPE_UTCTIME */

    case SQL_INTERVAL_MONTH:
      TargetType = SQL_C_INTERVAL_MONTH;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_YEAR:
      TargetType = SQL_C_INTERVAL_YEAR;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_YEAR_TO_MONTH:
      TargetType = SQL_C_INTERVAL_YEAR_TO_MONTH;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_DAY:
      TargetType = SQL_C_INTERVAL_DAY;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_HOUR:
      TargetType = SQL_C_INTERVAL_HOUR;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_MINUTE:
      TargetType = SQL_C_INTERVAL_MINUTE;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_SECOND:
      TargetType = SQL_C_INTERVAL_SECOND;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_DAY_TO_HOUR:
      TargetType = SQL_C_INTERVAL_DAY_TO_HOUR;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_DAY_TO_MINUTE:
      TargetType = SQL_C_INTERVAL_DAY_TO_MINUTE;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_DAY_TO_SECOND:
      TargetType = SQL_C_INTERVAL_DAY_TO_SECOND;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_HOUR_TO_MINUTE:
      TargetType = SQL_C_INTERVAL_HOUR_TO_MINUTE;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_HOUR_TO_SECOND:
      TargetType = SQL_C_INTERVAL_HOUR_TO_SECOND;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;
    case SQL_INTERVAL_MINUTE_TO_SECOND:
      TargetType = SQL_C_INTERVAL_MINUTE_TO_SECOND;
      Size = sizeof(SQL_INTERVAL_STRUCT);
      break;

    case SQL_GUID:
      SP_SOFT_ASSERT(0);
      rc = ODBC_ERROR_UNSUPPORTED_DATA_TYPE; /* Hmm */
      goto barf;

    case SQL_UNKNOWN_TYPE:
      SP_SOFT_ASSERT(0);                 /* [PM] 4.2 Tell me if this happens */
      rc = ODBC_ERROR_UNSUPPORTED_DATA_TYPE; /* Hmm */
      goto barf;

    default:                    /* e.g. driver specific type (e.g., -151 SQL Server geometry type SPRM 11946) */
      SP_SOFT_ASSERT(0);                 /* [PM] 4.2 Tell me if this happens */
      rc = ODBC_ERROR_UNSUPPORTED_DATA_TYPE; /* Hmm */
      goto barf;
    }
  
  SP_ASSERT(SQL_SUCCEEDED(rc));
  SP_ASSERT(TargetType != SQL_C_DEFAULT);
  *TargetTypePtr = TargetType;
  *SizePtr = Size;
 cleanup:
  return rc;
 barf:
  goto cleanup;
}

SP_integer SPCDECL odbc_describe_and_bind_column(SPAPI_ARG_PROTO_DECL
                                                 SP_term_ref statement_handle_spec,
                                                 SP_integer ColumnNumber_,
                                                 char const **ColumnNamePtr,
                                                 SP_integer *DataTypePtr,
                                                 SP_integer *buffer_id
                                                 )
{
  SQLRETURN rc;
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT const StatementHandle = real_statement_handle(StatementHandleIndex);
  SQLSMALLINT const ColumnNumber = (SQLSMALLINT)ColumnNumber_;
  struct buflist * bl = NULL;     /* needs cleanup */
  SQLSMALLINT NameLength = 0;
  SQLSMALLINT DataType = SQL_UNKNOWN_TYPE;
  SQLSMALLINT TargetType = SQL_C_DEFAULT; /* never a legal value here */
  SQLLEN TargetTypeSize = 0;
  SQLULEN ColumnSize = 0;
  SQLSMALLINT DecimalDigits = 0;
  SQLSMALLINT Nullable = 0;

  FREE_STATIC_IENC(state.column_name_ienc);

  ODBC_HSTMT_CHECK(StatementHandle);

  {
#if !POSTPONE
#error "Should call twice, first to determine column name length."
#endif  /* !POSTPONE */
    INIT_STATIC_BUFFER(state.column_name_bufferT);
    rc = SP_SQLDescribeColT(StatementHandle,
                            ColumnNumber,
                            state.column_name_bufferT,
                            (SQLSMALLINT)COLUMN_NAME_BUFFER_LENGTH,
                            &NameLength,
                            &DataType,
                            &ColumnSize,
                            &DecimalDigits,
                            &Nullable);
    ODBC_CHECK(rc);
    rc = map_sql_data_type_to_c_data_type_and_size(DataType, &TargetType, &TargetTypeSize);
    SP_SOFT_ASSERT(SQL_SUCCEEDED(rc));
    ODBC_CHECK(rc);

    {
      SQLRETURN rc2;
      rc2 = FROM_BUF_TO_IENC(state.column_name_bufferT,state.column_name_ienc);
      if (rc2 != SQL_SUCCESS)
        {
          SP_SOFT_ASSERT(0);
          rc = rc2;
          goto barf;
        }
    }

    if (TargetTypeSize == 0)
      {
        /* A variable size type, like SQL_CHAR */
        ;                       /* handled below */
      }
  }

    /* Also, is it a problem that columns are read in backwards order? Yes, SQLGetData would croak. */
    /* Workaround: add column number field to buflist. If > 0 it is a
       column number. Then set the buf once read so that multiple
       reads of the same buffer just returns the previously
       SQLGetData-gotten value. */
  ODBC_NULL_CHECK(bl = allocate_buflist(StatementHandleIndex, TargetType, TargetTypeSize));

  SP_ASSERT(TargetTypeSize != 0 || bl->buf == NULL);
  SP_ASSERT(ColumnNumber > 0); /* zero means no column number */
  bl->ColumnNumber = ColumnNumber;

  SP_ASSERT(state.column_name_ienc != NULL);
  *ColumnNamePtr = state.column_name_ienc;
  *DataTypePtr = DataType;
  *buffer_id  = buflist_index(bl);
  bl = NULL;                    /* protect from cleanup */
  SP_ASSERT(SQL_SUCCEEDED(rc));
 cleanup:
  if (bl != NULL)
    {
      /* [PM] 4.2 Could free bl here, i.e. on failure, but it will be
         freed with statement, which may be good enough */
    }
  return rc;
 barf:
  SP_ASSERT(!SQL_SUCCEEDED(rc));
  /* [PM] 4.2 Barf sets all outputs */
  *ColumnNamePtr = "";
  *DataTypePtr = SQL_UNKNOWN_TYPE;
  *buffer_id = 0;
  goto cleanup;
}

/* 
   Creates a SQL_C_CHAR buflist containing UTF-8 data. The assumption
   is that the DB uses UTF-8 (or, otherwise reports the column as
   SQL_WCHAR).
   
   Handles database null
*/
static SQLRETURN allocate_term_as_utf8_string_buf(SP_integer StatementHandleIndex, SP_term_ref data, struct buflist **pbl)
{
  SQLRETURN rc;
  char const * utf8_bytes = NULL; /* pseudo-static, no cleanup here */
  struct buflist *bl = NULL; /* needs cleanup */

  {
    spio_t_error_code code;
    code = term_as_string(data, &utf8_bytes, TERM_AS_STRING_OPTION_SQL_BIT_WORKAROUND);
    if (SPIO_FAILED(code))
      {
	ODBC_BARF((code == SPIO_E_OUT_OF_MEMORY ? ODBC_ERROR_OUT_OF_MEMORY : ODBC_ERROR_DATA_CONVERSION));
      }
  }

  {
    size_t bufsize;

    if (utf8_bytes == NULL)
      {
        /* database null */
        bufsize = 0;
      }
    else
      {
        size_t len = strlen(utf8_bytes);
        bufsize = (len +1) * (sizeof utf8_bytes[0]); /* +1 for NUL */
      }
    /* Note: This can not be SQL_C_WCHAR since the characters are not SQLWCHAR but rather (variable length sequences of) bytes. */
    ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, SQL_C_CHAR, bufsize, utf8_bytes));
    SP_ASSERT(bufsize > 0 || IS_FLAG_SET(bl->flags, BUFLIST_FLAG_NULL));
  }

  if (pbl != NULL) {
    *pbl = bl;
    bl = NULL; /* protect from cleanup */
  }
  rc = SQL_SUCCESS;
 cleanup:
  if (bl != NULL)
    {
      /* Should free bl here (but it will be freed with statement and currently it can not be non-null here unless pbl != NULL) */
      bl = NULL;
    }
  return rc;
 barf:
  goto cleanup;
}


/* Handles database null */
SP_integer SPCDECL odbc_bind_parameter(SPAPI_ARG_PROTO_DECL
                                       SP_term_ref statement_handle_spec,
                                       SP_integer ParameterNumber,
                                       SP_integer ParameterType_, SP_term_ref data,
                                       SP_integer *buffer_id
                                       )
{
  SQLRETURN rc;
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT StatementHandle = real_statement_handle(StatementHandleIndex);
  SQLSMALLINT ParameterType = (SQLSMALLINT)ParameterType_; /* SQL ParameterType */
  SQLSMALLINT TargetType = SQL_C_DEFAULT; /* C TargetType. SQL_C_DEFAULT is never a legal value here */
  SQLLEN TargetTypeSize = 0;
  struct buflist * bl = NULL; /* needs cleanup (although proper cleanup is not yet implemented) */

  ODBC_HSTMT_CHECK(StatementHandle);
  ODBC_CHECK(map_sql_data_type_to_c_data_type_and_size(ParameterType, &TargetType, &TargetTypeSize));
  SP_ASSERT(TargetType != SQL_C_DEFAULT);

#if !POSTPONE
#error "[PM] 4.2.2 We should use SQLPrepare + SQLDescribeParam to automatically determine parameter types."
#endif  /* !POSTPONE */

  /* [PM] 4.2 determine how to convert and allocate a buffer with the
     data. One case for each TargetType emitted by
     map_sql_data_type_to_c_data_type_and_size() and vice versa */
  {
    switch (TargetType)
      {
      case SQL_C_CHAR:            /* (1) handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_CHAR\n");
          SP_ASSERT(TargetTypeSize == 0);
          ODBC_CHECK(allocate_term_as_utf8_string_buf(StatementHandleIndex, data, &bl));
        }
        break;
      case SQL_C_WCHAR:            /* handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_WCHAR\n");
#if ENABLE_WCHAR
          {
            SQLWCHAR *wStr = NULL;
            {
              /* handles null */
              char const * utf8_bytes = NULL;

              {
                spio_t_error_code code;
                code = term_as_string(data, &utf8_bytes, TERM_AS_STRING_OPTION_SQL_BIT_WORKAROUND);
                if (SPIO_FAILED(code))
                  {
                    ODBC_BARF((code == SPIO_E_OUT_OF_MEMORY ? ODBC_ERROR_OUT_OF_MEMORY : ODBC_ERROR_DATA_CONVERSION));
                  }
              }
              /* utf8_bytes == NULL implies database null */
              if (utf8_bytes == NULL)
                {
                  wStr = NULL;
                }
              else
                {
                  rc = to_sqlwchars(utf8_bytes, &wStr);
                  if (rc != SQL_SUCCESS) { goto barf; }
                  SP_ASSERT(wStr != NULL);
                }
            }
            /* wStr == NULL implies database null */
            {
              size_t bufsize;
              SP_ASSERT(TargetTypeSize == 0);
              if (wStr == NULL)
                {
                  /* database null */
                  bufsize = 0;
                }
              else
                {
                  size_t len = wchars_strlen(wStr);
                  bufsize = (len +1) * (sizeof wStr[0]); /* +1 for NUL */
                }
              /* wStr == NULL && bufsize == 0 implies BUFLIST_FLAG_NULL */
              bl = allocate_buflist_data(StatementHandleIndex, TargetType, bufsize, wStr);
              odbc_free(wStr); /* before checking bl for success. */
              wStr = NULL;
              ODBC_NULL_CHECK(bl);
            }
          }
#else /* !ENABLE_WCHAR */
          {
            SP_SOFT_ASSERT(0); /* [PM] 4.2.2. tell me if this happens */
            /* Use SQL_C_CHAR instead and let ODBC convert. */
            /* handles database null */
            SP_ASSERT(TargetTypeSize == 0);
            ODBC_CHECK(allocate_term_as_utf8_string_buf(StatementHandleIndex, data, &bl));
          }
#endif  /* !ENABLE_WCHAR */
        }
        break;
      case SQL_C_BIT: /* handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_BIT\n");
          {
            SQLCHAR value;
            int is_null = SPIO_FALSE;
            if (SP_is_integer(data)) {
              SP_integer tmp;
              if (!SP_get_integer(data, &tmp))
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
              switch (tmp)
                {
                case 0: value = 0; break;
                default:            /* [PM] 4.2 FIXME: Consider falling through to 1-case */
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                case 1: value = 1; break;
                }
            } else {
              char const * utf8_bytes = NULL;
              {
                spio_t_error_code code;
                code = term_as_string(data, &utf8_bytes, 0); /* do not treat 0 and 1 specially, handled above instead. */
                if (SPIO_FAILED(code))
                  {
                    ODBC_BARF((code == SPIO_E_OUT_OF_MEMORY ? ODBC_ERROR_OUT_OF_MEMORY : ODBC_ERROR_DATA_CONVERSION));
                  }
              }
          
              if (utf8_bytes == NULL)
                {
                  /* database null */
                  is_null = SPIO_TRUE;
                  value = 0; /* silence compiler */
                }
              else if (is_boolean_false_string(utf8_bytes)) /* FIXME: Why? Not documented and "0" used by ODBC and elsewhere. */
                {
                  value = 0;
                }
              else if (is_boolean_true_string(utf8_bytes)) /* FIXME: Why? Not documented and "1" used by ODBC and elsewhere. */
                {
                  value = 1;
                }
              else
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            }
            if (TargetTypeSize != sizeof value) 
              {
                SP_ASSERT(0); // cannot happen
                ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
              }
            /*
              [PM] 4.2.2 valgrind complains about mysql (CentOS 6.2
              x64). The MySQL driver Seems to be taking strlen() of a
              non-string, the result of which is then discarded.
            */
            ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, TargetType, (is_null ? 0 : TargetTypeSize), (is_null ? NULL : &value)));
          }
        }
        break;

      case SQL_C_STINYINT: /* handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_STINYINT\n");
          {
            SQLSCHAR value; /* (!) */
            int is_null = SPIO_FALSE;

            if (SP_is_integer(data)) {
              SP_integer tmp;
              if (!SP_get_integer(data, &tmp))
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
              value = (SQLSCHAR)tmp;
              if (((SP_integer)value) != tmp)
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            } else {
              SP_atom tmp = 0;

              if (SP_get_atom(data, &tmp) && tmp == state.null)
                {
                  is_null = SPIO_TRUE;
                }
              else
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            }
            if (TargetTypeSize != sizeof value) 
              {
                SP_ASSERT(0); // cannot happen
                ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
              }
            ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, TargetType, (is_null ? 0 : TargetTypeSize), (is_null ? NULL : &value)));
          }
        }
        break;

      case SQL_C_SSHORT: /* handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_SSHORT\n");
#if ODBC_BROKEN_SHORT
          {
            SP_ASSERT(SPIO_FALSE);
            ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
          }
#else /* !ODBC_BROKEN_SHORT */
          {
            SQLSMALLINT value;
            int is_null = SPIO_FALSE;

            if (SP_is_integer(data)) {
              SP_integer tmp;
              if (!SP_get_integer(data, &tmp))
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
              value = (SQLSMALLINT)tmp;
              if (((SP_integer)value) != tmp)
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            } else {
              SP_atom tmp = 0;

              if (SP_get_atom(data, &tmp) && tmp == state.null)
                {
                  is_null = SPIO_TRUE;
                }
              else
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            }
            if (TargetTypeSize != sizeof value) 
              {
                SP_ASSERT(0); // cannot happen
                ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
              }
            ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, TargetType, (is_null ? 0 : TargetTypeSize), (is_null ? NULL : &value)));
          }
#endif  /* !ODBC_BROKEN_SHORT */
        }
        break;

      case SQL_C_SLONG: /* (-16) handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_SLONG\n");
          {
            SQLINTEGER value;
            int is_null = SPIO_FALSE;

            if (SP_is_integer(data)) {
              SP_integer tmp;
              if (!SP_get_integer(data, &tmp))
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
              value = (SQLINTEGER)tmp;
              if (((SP_integer)value) != tmp)
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            } else {
              SP_atom tmp = 0;

              if (SP_get_atom(data, &tmp) && tmp == state.null)
                {
                  is_null = SPIO_TRUE;
                }
              else
		{
		  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
		}
            }
            if (TargetTypeSize != sizeof value) 
              {
                SP_ASSERT(0); // cannot happen
                ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
              }
            ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, TargetType, (is_null ? 0 : TargetTypeSize), (is_null ? NULL : &value)));
          }
        }
        break;

      case SQL_C_SBIGINT: /* handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_SBIGINT\n");
          {
            typedef SQLBIGINT value_type;
            value_type value;
            int is_null = SPIO_FALSE;

            if (SP_is_integer(data)) {
              SP_int64 tmp;
              size_t tmp_size = sizeof tmp;
              COMPILE_TIME_ASSERT((sizeof tmp) == (sizeof value));
              if (!SP_get_integer_bytes(data, &tmp, &tmp_size, 1)) /* native */
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
              /* [PM] 4.2 This will give an error if the odbc headers has
                 defined SQLBIGINT as a struct instead of as a native
                 64-bit type. Hopefully all current versions of unixodbc
                 does use a native integer type. */
              value = (value_type)tmp;
              if (((value_type)value) != tmp)
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            } else {
              SP_atom tmp = 0;

              if (SP_get_atom(data, &tmp) && tmp == state.null)
                {
                  is_null = SPIO_TRUE;
                }
              else
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            }
            if (TargetTypeSize != sizeof value) 
              {
                SP_ASSERT(0); // cannot happen
                ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
              }
            ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, TargetType, (is_null ? 0 : TargetTypeSize), (is_null ? NULL : &value)));
          }
        }
        break;

      case SQL_C_FLOAT: /* handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_FLOAT\n");
          {
            typedef SQLREAL value_type;
            value_type value;
            int is_null = SPIO_FALSE;

            if (SP_is_float(data)) {
              typedef double tmp_type;
              tmp_type tmp;
              if (!SP_get_float(data, &tmp))
                {
                  SP_ASSERT(0);
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
              value = (value_type)tmp;
#if 0                           /* [PM] 4.2 FIXME: Should we range check floating point values? */
              if (((tmp_type)value) != tmp)
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
#endif  /* 0 */
            } else if (SP_is_integer(data)) {
              typedef SP_integer tmp_type;
              tmp_type tmp;
              if (!SP_get_integer(data, &tmp))
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
              value = (value_type)tmp;
              if (((tmp_type)value) != tmp)
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            } else {
              SP_atom tmp = 0;

              if (SP_get_atom(data, &tmp) && tmp == state.null)
                {
                  is_null = SPIO_TRUE;
                }
              else
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            }
            if (TargetTypeSize != sizeof value) 
              {
                SP_ASSERT(0); // cannot happen
                ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
              }
            ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, TargetType, (is_null ? 0 : TargetTypeSize), (is_null ? NULL : &value)));
          }
        }
        break;

      case SQL_C_DOUBLE: /* handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_DOUBLE\n");
          {
            typedef SQLDOUBLE value_type;
            value_type value;
            int is_null = SPIO_FALSE;

            if (SP_is_float(data)) {
              typedef double tmp_type;
              tmp_type tmp;
              if (!SP_get_float(data, &tmp))
                {
                  SP_ASSERT(0);
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
              value = (value_type)tmp;
#if 0                           /* [PM] 4.2 FIXME: Should we range check floating point values? */
              if (((tmp_type)value) != tmp)
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
#endif  /* 0 */
            } else if (SP_is_integer(data)) {
              typedef SP_integer tmp_type;
              tmp_type tmp;
              if (!SP_get_integer(data, &tmp))
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
              value = (value_type)tmp;
              if (((tmp_type)value) != tmp)
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            } else {
              SP_atom tmp = 0;

              if (SP_get_atom(data, &tmp) && tmp == state.null)
                {
                  is_null = SPIO_TRUE;
                }
              else
                {
                  ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                }
            }
            if (TargetTypeSize != sizeof value) 
              {
                SP_ASSERT(0); // cannot happen
                ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
              }
            ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, TargetType, (is_null ? 0 : TargetTypeSize), (is_null ? NULL : &value)));
          }
        }
        break;

      case SQL_C_TYPE_DATE: /* handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_TYPE_DATE\n");
          {
            typedef SQL_DATE_STRUCT value_type;
            value_type value;
            int is_null = SPIO_FALSE;
        
            {
              int const expected_arity = 3;
              SP_atom const expected_name = state.date_name;
              SP_term_ref scratch = SP_new_term_ref();

              if (has_functor(data, expected_name, expected_arity))
                {
                  if (!get_integer_arg_SQLSMALLINT(data, scratch, 1, &value.year)
                      ||
                      !get_integer_arg_SQLUSMALLINT(data, scratch, 2, &value.month)
                      ||
                      !get_integer_arg_SQLUSMALLINT(data, scratch, 3, &value.day))
                    {
                      ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                    }
                }
              else
                {
                  SP_atom tmp = 0;
                  
                  if (SP_get_atom(data, &tmp) && tmp == state.null)
                    {
                      is_null = SPIO_TRUE;
                    }
                  else
                    {
                      if (!SP_is_compound(data))
                        {
                          ODBC_BARF(ODBC_ERROR_TYPE_ERROR);
                        }
                      ODBC_BARF(ODBC_ERROR_UNKNOWN_DATA_TYPE);
                    }
                }
            }

            if (TargetTypeSize != sizeof value)
              {
                SP_ASSERT(0); // cannot happen
                ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
              }
            ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, TargetType, (is_null ? 0 : TargetTypeSize), (is_null ? NULL : &value)));
          }
        }
        break;

      case SQL_C_TYPE_TIME: /* handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_TYPE_TIME\n");
          {
            typedef SQL_TIME_STRUCT value_type;
            value_type value;
            int is_null = SPIO_FALSE;
        
            {
              int const expected_arity = 3;
              SP_atom const expected_name = state.time_name;
              SP_term_ref scratch = SP_new_term_ref();

              if (has_functor(data, expected_name, expected_arity))
                {
                  if (!get_integer_arg_SQLUSMALLINT(data, scratch, 1, &value.hour)
                      ||
                      !get_integer_arg_SQLUSMALLINT(data, scratch, 2, &value.minute)
                      ||
                      !get_integer_arg_SQLUSMALLINT(data, scratch, 3, &value.second))
                    {
                      ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                    }
                }
              else
                {
                  SP_atom tmp = 0;

                  if (SP_get_atom(data, &tmp) && tmp == state.null)
                    {
                      is_null = SPIO_TRUE;
                    }
                  else
                    {
                      if (!SP_is_compound(data))
                        {
                          ODBC_BARF(ODBC_ERROR_TYPE_ERROR);
                        }
                      ODBC_BARF(ODBC_ERROR_UNKNOWN_DATA_TYPE);
                    }
                }
            }

            if (TargetTypeSize != sizeof value)
              {
                SP_ASSERT(0); // cannot happen
                ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
              }
            ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, TargetType, (is_null ? 0 : TargetTypeSize), (is_null ? NULL : &value)));
          }
        }
        break;

      case SQL_C_TYPE_TIMESTAMP: /* handles database null */
        {
          DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_TYPE_TIMESTAMP\n");
          {
            typedef SQL_TIMESTAMP_STRUCT value_type;
            value_type value;
            int is_null = SPIO_FALSE;
        
            {
              int const expected_arity = 7;
              SP_atom const expected_name = state.timestamp_name;
              SP_term_ref scratch = SP_new_term_ref();

              if (has_functor(data, expected_name, expected_arity))
                {
                  if (!get_integer_arg_SQLSMALLINT(data, scratch, 1, &value.year)
                      ||
                      !get_integer_arg_SQLUSMALLINT(data, scratch, 2, &value.month)
                      ||
                      !get_integer_arg_SQLUSMALLINT(data, scratch, 3, &value.day)
                      ||
                      !get_integer_arg_SQLUSMALLINT(data, scratch, 4, &value.hour)
                      ||
                      !get_integer_arg_SQLUSMALLINT(data, scratch, 5, &value.minute)
                      ||
                      !get_integer_arg_SQLUSMALLINT(data, scratch, 6, &value.second)
                      ||
                      /* nanoseconds (Postgres seems to round at milliseconds, i.e. values less than 1000 (? 1000 means microseconds)) */
                      !get_integer_arg_SQLUINTEGER(data, scratch, 7, &value.fraction))
                    {
                      ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
                    }
                }
              else
                {
                  SP_atom tmp = 0;

                  if (SP_get_atom(data, &tmp) && tmp == state.null)
                    {
                      is_null = SPIO_TRUE;
                    }
                  else
                    {
                      if (!SP_is_compound(data))
                        {
                          ODBC_BARF(ODBC_ERROR_TYPE_ERROR);
                        }
                      ODBC_BARF(ODBC_ERROR_UNKNOWN_DATA_TYPE);
                    }
                }
            }

            if (TargetTypeSize != sizeof value)
              {
                SP_ASSERT(0); // cannot happen
                ODBC_BARF(ODBC_ERROR_DATA_CONVERSION);
              }
            ODBC_NULL_CHECK(bl = allocate_buflist_data(StatementHandleIndex, TargetType, (is_null ? 0 : TargetTypeSize), (is_null ? NULL : &value)));
          }
        }
        break;

      case SQL_C_INTERVAL_MONTH:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_MONTH (not supported)\n");
        {
#if !POSTPONE
#error "[PM] 4.2 implement interval types?"
#endif                                                 /* !POSTPONE */
          ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        }
        break;
      case SQL_C_INTERVAL_YEAR:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_YEAR (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_YEAR_TO_MONTH:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_YEAR_TO_MONTH (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_DAY:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_DAY (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_HOUR:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_HOUR (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_MINUTE:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_MINUTE (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_SECOND:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_SECOND (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_DAY_TO_HOUR:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_DAY_TO_HOUR (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_DAY_TO_MINUTE:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_DAY_TO_MINUTE (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_DAY_TO_SECOND:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_DAY_TO_SECOND (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_HOUR_TO_MINUTE:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_HOUR_TO_MINUTE (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_HOUR_TO_SECOND:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_HOUR_TO_SECOND (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;
      case SQL_C_INTERVAL_MINUTE_TO_SECOND:
        DEBUG_PRINT0("\nODBC_DBG: odbc_bind_parameter: SQL_C_INTERVAL_MINUTE_TO_SECOND (not supported)\n");
        ODBC_BARF(ODBC_ERROR_UNSUPPORTED_DATA_TYPE); /* FIXME: support this datatype(s) */
        break;

      default:
        DEBUG_PRINT((stderr, "\nODBC_DBG: odbc_bind_parameter: unhandled TargetType: %" SPRIdINTEGER "\n", (SP_integer)TargetType));
        SP_ASSERT(0);
        ODBC_BARF(ODBC_ERROR_UNKNOWN_DATA_TYPE);
      }
  }
  /* [PM] 4.2 bl is now set-up with the data, or null */
  SP_ASSERT(bl != NULL);

  /* [PM] 4.2
     http://msdn.microsoft.com/en-us/library/ms709314(v=VS.85).aspx
     says "The DecimalDigits argument of SQLBindParameter is ignored
     for this data type." for everything except: SQL_DECIMAL,
     SQL_NUMERIC, all datetime types except SQL_TYPE_DATE, and all
     interval types with a seconds component, and SQL_GUID (but
     Decimal Digits is n/a for SQL_GUID so SQL_GUID probably ignores
     DecimalDigits as well).

     So, DecimalDigits zero should be safe for all supported types
     except the datetime types. FIXME: Need to handle DecimalDigits
     for affected types.
  */
  {
    SQLSMALLINT const DecimalDigits = 0;
    SQLSMALLINT const ValueType = bl->TargetType;
    SQLULEN ColumnSize;
    SQLLEN *StrLen_or_IndPtr;

    

    /* StrLen_or_IndPtr Argument "The length of the parameter value
       stored in *ParameterValuePtr. This is ignored except for
       character or binary C data."
       
       So we set it to something harmless and override it below for null, and for character and binary data
    */
    StrLen_or_IndPtr = NULL;

    /* Set ColumnSize and StrLen_or_IndPtr.

       See http://msdn.microsoft.com/en-us/library/ms711786(v=VS.85).aspx for information about Column Size.

       One case for each DataType accepted by
       map_sql_data_type_to_c_data_type_and_size() and vice versa. */
    switch (ParameterType)
      {
        /* http://msdn.microsoft.com/en-us/library/ms711786(v=VS.85).aspx
           says "All character types." */

      case SQL_WCHAR:
      case SQL_WVARCHAR:
      case SQL_WLONGVARCHAR:
        /* happens, also on Mac OS X, also with !ENABLE_WCHAR. */
        /* FALLTHROUGH */
      case SQL_CHAR:
      case SQL_VARCHAR:
      case SQL_LONGVARCHAR:
        /* "If StrLen_or_IndPtr is a null pointer, the driver assumes that ... character ... data is null-terminated. " */
        StrLen_or_IndPtr = NULL;
        /* [PM] 4.2 This is what SP < 4.2 did. Not clear what to do here. */
        ColumnSize = bl->size;
        break;

      case SQL_DECIMAL:
      case SQL_NUMERIC:
        /* [PM] 4.2 We do not support these, map_sql_data_type_to_c_data_type_and_size() should have failed */
        SP_ASSERT(0);
        ColumnSize = 0;
        break;

        /* "All binary types" */
      case SQL_BINARY:
      case SQL_VARBINARY:
      case SQL_LONGVARBINARY:
        /* [PM] 4.2 We do not support these, map_sql_data_type_to_c_data_type_and_size() should have failed */
        SP_ASSERT(0);
        ColumnSize = 0;
        /* Would need StrLen_or_IndPtr = ... */
        break;

        /* 
           On ColumnSize and numberic types:

           http://msdn.microsoft.com/en-us/library/ms711786(v=VS.85).aspx
           says "The ColumnSize argument of SQLBindParameter is
           ignored for this data type." (for, among others, SQL_FLOAT,
           SQL_REAL, or SQL_DOUBLE).

           Passing zero column size is what some IBM sample code does
           too.

           NOTE: THE MSDN DOCUMENTATION IS CONTRADICTORY:

           http://msdn.microsoft.com/en-us/library/windows/desktop/ms710963(v=vs.85).aspx
           says "If ParameterType is SQL_DECIMAL, SQL_NUMERIC,
           SQL_FLOAT, SQL_REAL, or SQL_DOUBLE, the SQL_DESC_PRECISION
           field of the IPD is set to the value of ColumnSize."

           Also, AskMonty says ColumnSize matters
           <http://kb.askmonty.org/v/sqlbindparameter-effect-numeric>.
        */
      case SQL_BIT:
      case SQL_TINYINT:
      case SQL_SMALLINT:
      case SQL_INTEGER:
      case SQL_BIGINT:
      case SQL_REAL:
      case SQL_FLOAT:
      case SQL_DOUBLE:
      case SQL_TYPE_DATE:
      case SQL_TYPE_TIME:
        ColumnSize = 0;
        break;

#ifdef SQL_TYPE_UTCDATETIME     /* [PM] 4.2 Not on Mac OS X 10.5 */
      case SQL_TYPE_UTCDATETIME:
        /* [PM] 4.2 We do not support these, map_sql_data_type_to_c_data_type_and_size() should have failed */
        SP_ASSERT(0);
        ColumnSize = 0;
        break;
#endif  /* SQL_TYPE_UTCDATETIME */

#ifdef SQL_TYPE_UTCTIME     /* [PM] 4.2 Not on Mac OS X 10.5 */
      case SQL_TYPE_UTCTIME:
        /* [PM] 4.2 We do not support these, map_sql_data_type_to_c_data_type_and_size() should have failed */
        SP_ASSERT(0);
        ColumnSize = 0;
        break;
#endif  /* SQL_TYPE_UTCTIME */
      
      case SQL_TYPE_TIMESTAMP:
        /* [PM] 4.2.2
           http://msdn.microsoft.com/en-us/library/ms711786(v=VS.85).aspx says:

           16 (the number of characters in the yyyy-mm-dd hh:mm format)
           19 (the number of characters in the yyyy-mm-dd hh:mm:ss format)
           or
           20 + s (the number of characters in the yyyy-mm-dd hh:mm:ss[.fff...] format, where s is the seconds precision).

           For SQLBindParameter, http://msdn.microsoft.com/en-us/library/windows/desktop/ms710963(v=vs.85).aspx says:

              If ParameterType is SQL_CHAR, SQL_VARCHAR,
              SQL_LONGVARCHAR, SQL_BINARY, SQL_VARBINARY,
              SQL_LONGVARBINARY, or one of the concise SQL datetime or
              interval data types, the SQL_DESC_LENGTH field of the
              IPD is set to the value of ColumnSize. (For more
              information, see the Column Size, Decimal Digits,
              Transfer Octet Length, and Display Size section in
              Appendix D: Data Types.)

              If ParameterType is SQL_DECIMAL, SQL_NUMERIC, SQL_FLOAT,
              SQL_REAL, or SQL_DOUBLE, the SQL_DESC_PRECISION field of
              the IPD is set to the value of ColumnSize.

              For other data types, the ColumnSize argument is ignored.

           I take this to mean that it is ignored for
           SQL_TYPE_TIMESTAMP. In either case zero seems to be treated
           as 'use default' so zero is a better guess.
        */

#if !ODBC_FORCE_BUILD
        #error "What to use for ColumnSize?"
#endif  /* !ODBC_FORCE_BUILD */

#if 1
        ColumnSize = 0;
#else  /* <= 4.2.1 */
        /* [PM] 4.2 This is what SP 4.1.x does but this is clearly wrong */
        ColumnSize = sizeof(SQL_TIMESTAMP_STRUCT);
#endif  /* <= 4.2.1 */

        break;

      case SQL_INTERVAL_MONTH:
      case SQL_INTERVAL_YEAR:
      case SQL_INTERVAL_YEAR_TO_MONTH:
      case SQL_INTERVAL_DAY:
      case SQL_INTERVAL_HOUR:
      case SQL_INTERVAL_MINUTE:
      case SQL_INTERVAL_SECOND:
      case SQL_INTERVAL_DAY_TO_HOUR:
      case SQL_INTERVAL_DAY_TO_MINUTE:
      case SQL_INTERVAL_DAY_TO_SECOND:
      case SQL_INTERVAL_HOUR_TO_MINUTE:
      case SQL_INTERVAL_HOUR_TO_SECOND:
      case SQL_INTERVAL_MINUTE_TO_SECOND:
        /* FIXME: What to use here? */
#if !POSTPONE
        /* See http://msdn.microsoft.com/en-us/library/ms711786(v=VS.85).aspx */
#error "Figure out what to use for ColumnSize"
#endif
#if 1
        ColumnSize = 0;
#else  /* <= 4.2.1 */
        /* [PM] 4.2 This is what SP 4.1.x does but this is clearly wrong */
        ColumnSize = sizeof(SQL_INTERVAL_STRUCT);
#endif  /* <= 4.2.1 */
        break;

      case SQL_GUID:
        ColumnSize = 36;
        break;
      default:
        SP_ASSERT(0);
        ColumnSize = 0;
        break;
      }
#if !POSTPONE
#error "[PM] 4.2.2 Ensure we do the right thing for ColumnSize (esp if W-char type it seems strange to use byte-len (bl->size). MSDN says 'column size in characters', i.e. not in bytes)"
#endif  /* !POSTPONE */

    {
      SQLPOINTER      ParameterValuePtr;
      SQLLEN          BufferLength;

      if (IS_FLAG_SET(bl->flags, BUFLIST_FLAG_NULL))
        {
          /*
            "... the *ParameterValuePtr buffer must contain a valid
            input value, or the *StrLen_or_IndPtr buffer must contain
            SQL_NULL_DATA ..."
          */
          ParameterValuePtr = NULL;
          BufferLength = 0;
          StrLen_or_IndPtr = &state.null_StrLen_or_Ind;
        }
      else
        {
          SP_ASSERT(real_statement_handle_entry(StatementHandleIndex) != NULL && !real_statement_handle_entry(StatementHandleIndex)->is_definitely_unbound);
          ParameterValuePtr = bl->buf;
          BufferLength = bl->size;
          /* Keep StrLen_or_IndPtr from above */
        }
      rc = SQLBindParameter(StatementHandle,
                            (SQLUSMALLINT)ParameterNumber,
                            SQL_PARAM_INPUT,
                            ValueType,     /* C Type */
                            ParameterType, /* SQL Type */
                            ColumnSize,
                            DecimalDigits,
                            ParameterValuePtr,
                            BufferLength,
                            StrLen_or_IndPtr
                            );
      ODBC_CHECK(rc);
    }
    *buffer_id = buflist_index(bl);
    bl = NULL;                /* protect from cleanup ([PM] 4.2 currently redundant since no cleanup is done) */
  }


  SP_ASSERT(SQL_SUCCEEDED(rc));
 cleanup:
  if (bl != NULL)
    {
      /* [PM] 4.2 We could free bl here (if non-NULL), but it will be
         dallocated with the statement which may suffice. */
    }
  return rc;

 barf:
  SP_ASSERT(!SQL_SUCCEEDED(rc));
  /* [PM] 4.2 Barf sets all outputs */
  *buffer_id = 0;
  goto cleanup;
}


/****************************************
SQLRETURN SQLExecDirect(
     SQLHSTMT     StatementHandle,
     SQLCHAR *     StatementText,
     SQLINTEGER     TextLength);
****************************************/


SP_integer SPCDECL odbc_exec_direct(SPAPI_ARG_PROTO_DECL SP_term_ref statement_handle_spec, char const * StatementText)
{
  SQLRETURN rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT StatementHandle = real_statement_handle(StatementHandleIndex);
  SP_SQLTCHAR *StatementTextT = NULL; /* Needs cleanup */

  ODBC_HSTMT_CHECK(StatementHandle);
  rc = FROM_IENC_TO_TCHARS((char /* const */ *)StatementText, StatementTextT);
  if (rc != SQL_SUCCESS)
    {
      goto barf;
    }

  rc = SP_SQLExecDirectT(StatementHandle,
                         StatementTextT,
                         SQL_NTS);
 cleanup:
  FREE_IENC_TO_TCHARS(StatementText, StatementTextT);
  return rc;
 barf:
  SP_ASSERT(!SQL_SUCCEEDED(rc));
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}


/****************************************
SQLRETURN SQLDisconnect(
     SQLHDBC     ConnectionHandle);
****************************************/

static SQLRETURN odbc_disconnect_helper(struct connectionhandle *entry)
{
  SQLRETURN rc;
  size_t i;
  SQLHDBC ConnectionHandle = entry->real_handle;
  SP_ASSERT(ConnectionHandle != SQL_NULL_HDBC);

  DEBUG_PRINT((stderr, "\nODBC_DBG: SQLDisconnect(%p)\n", (void*)ConnectionHandle));


#if 0
  /* [PM] 4.2.1 happens in suite when no DB available */
  SP_SOFT_ASSERT(entry->is_connected);
#endif  /* 0 */

  /* [PM] 4.2 Need to free all dependant handles so SQLDisconnect does not do it behind our back */
  for (i = 1; i <= state.statementhandle_tos; i++)
    {
      struct statementhandle * const sh = &(state.statementhandlestack[i]);
#if ODBC_DEBUG > 1
      DEBUG_PRINT((stderr, "ODBC_DBG: sh==%p\n", sh));
      DEBUG_PRINT((stderr, "ODBC_DBG: index %" SPRIdSZ ", real_handle==%p, connection_handle_index==%" SPRIdINTEGER ", buffers=%p\n", i, (void*)sh->real_handle, (SP_integer)sh->connection_handle_index, (void*)sh->buffers));
      DEBUG_PRINT_HANDLE_STACKS;
#endif  /* ODBC_DEBUG */
      {
        SQLHSTMT StatementHandle = sh->real_handle;
        if (StatementHandle != SQL_NULL_HSTMT)
          {
            struct connectionhandle *conn_entry = real_connection_handle_entry(sh->connection_handle_index);
            SP_ASSERT(conn_entry != NULL);
            if (conn_entry == entry)
              {
                DEBUG_PRINT((stderr, "\nODBC_DBG: Warning: SQLDisconnect(%p), freeing owned statementhandle %p (index==%" SPRIdSZ ")\n", (void*)ConnectionHandle, (void*)sh, i));
                free_statementhandle(sh);
              }
            else
              {
                SP_ASSERT(conn_entry->real_handle != entry->real_handle);
              }
          }
      }
    }
  
  ODBC_CHECK(SQLDisconnect(ConnectionHandle));

  CLEAR_FLAG(entry->flags, CONNECTION_HANDLE_FLAG_CONNECTED);

  SP_SOFT_ASSERT(rc != SQL_SUCCESS_WITH_INFO); /* [PM] 4.2 tell me if SQL_SUCCESS_WITH_INFO happens */
  SP_ASSERT(SQL_SUCCEEDED(rc));
 cleanup:
  return rc;
 barf:
DEBUG_PRINT((stderr, "ODBC_DBG: ERROR: SQLDisconnect(%p)=%" SPRIdINTEGER "\n", (void*)ConnectionHandle, (SP_integer)rc));
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}

/*
  '$odbc_disconnect'(+ConnectionHandle, -ResultCode)
  foreign(odbc_disconnect,
          '$odbc_disconnect'(+integer, [-integer])).
*/

SP_integer SPCDECL odbc_disconnect(SPAPI_ARG_PROTO_DECL SP_term_ref connection_handle_spec)
{
  SQLRETURN rc;
  SP_integer const ConnectionHandleIndex = decode_connection_handle_spec(connection_handle_spec);
  SQLHDBC ConnectionHandle = real_connection_handle(ConnectionHandleIndex);

  if (ConnectionHandle == SQL_NULL_HDBC)
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_INVALID_HANDLE;
      goto barf;
    }

  rc = odbc_disconnect_helper(&state.connectionhandlestack[ConnectionHandleIndex]);
 cleanup:
  return rc;
 barf:
  goto cleanup;
}


/****************************************
SQLRETURN SQLGetInfo(
     SQLHDBC     ConnectionHandle,
     SQLUSMALLINT     InfoType,
     SQLPOINTER     InfoValuePtr,
     SQLSMALLINT     BufferLength,
     SQLSMALLINT *     StringLengthPtr);
****************************************/

SP_integer SPCDECL odbc_getinfo(SPAPI_ARG_PROTO_DECL SP_term_ref connection_handle_spec,
                                SP_integer InfoType,
                                SP_term_ref InfoValue)
{
  SQLPOINTER InfoValuePtr = NULL; /* needs cleanup */
  SQLSMALLINT BufferLength;
  SQLSMALLINT StringLength = 0;
  SQLRETURN rc;
  SP_integer const ConnectionHandleIndex = decode_connection_handle_spec(connection_handle_spec);
  SQLHDBC ConnectionHandle = real_connection_handle(ConnectionHandleIndex);

  if (ConnectionHandle == SQL_NULL_HDBC)
    {
      SP_ASSERT(0);
      ODBC_BARF(ODBC_ERROR_INVALID_HANDLE);
    }

  switch (InfoType) {
  case SQL_MAX_COLUMN_NAME_LEN:
    BufferLength = sizeof(SQLSMALLINT);
    break;
  case SQL_TXN_CAPABLE:
    BufferLength = sizeof(SQLUSMALLINT);
    break;
  default:
    SP_ASSERT(0);               /* parameter error */
    ODBC_BARF(ODBC_ERROR_INVALID_PARAMETER);
  }
  ODBC_NULL_CHECK(InfoValuePtr = odbc_malloc(BufferLength));
  rc = SQLGetInfo(ConnectionHandle,
                  (SQLUSMALLINT)InfoType,
                  InfoValuePtr,
                  BufferLength,
                  &StringLength);
  switch (InfoType) {
  case SQL_MAX_COLUMN_NAME_LEN:
    {
      SQLSMALLINT value, *value_ptr = (SQLSMALLINT*)InfoValuePtr;
      SP_ASSERT((sizeof *value_ptr) == BufferLength);
      value =  *value_ptr;
      if (0 == SP_put_integer(InfoValue, value)) {
        ODBC_BARF(SQL_ERROR);
      }
    }
    break;
  case SQL_TXN_CAPABLE:
    {
      SQLUSMALLINT value, *value_ptr = (SQLUSMALLINT*)InfoValuePtr;
      SP_ASSERT((sizeof *value_ptr) == BufferLength);
      value =  *value_ptr;
      if (0 == SP_put_integer(InfoValue, value)) {
        ODBC_BARF(SQL_ERROR);
      }
    }
    break;

  default:
    SP_ASSERT(0);               /* internal error */
    rc = SQL_ERROR;
    goto barf;
  }
 cleanup:
  odbc_free(InfoValuePtr);
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs */
  /* Implied: SP_put_atom(InfoValue, SP_atom_from_string("[]")); */
  goto cleanup;
}


/****************************************
SQLRETURN SQLNumResultCols(
     SQLHSTMT     StatementHandle,
     SQLSMALLINT *     ColumnCountPtr);
****************************************/

SP_integer SPCDECL odbc_num_result_cols(SPAPI_ARG_PROTO_DECL SP_term_ref statement_handle_spec,
                                SP_integer * ColumnCountPtr)
{
  SQLRETURN rc;
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT StatementHandle = real_statement_handle(StatementHandleIndex);
  SQLSMALLINT ColumnCount = 0;

  ODBC_HSTMT_CHECK(StatementHandle);
  rc = SQLNumResultCols(StatementHandle,
                        &ColumnCount);
#if !POSTPONE
#error "Here and elsewhere odbc.pl treats SQL_SUCCESS_WITH_INFO the same as SQL_SUCCESS but perhaps it should be treated as an error by default?"
#endif  /* !POSTPONE */
  *ColumnCountPtr = ColumnCount;
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs */
  *ColumnCountPtr = 0;
  goto cleanup;
}


/****************************************
SQLRETURN SQLRowCount(
     SQLHSTMT     StatementHandle,
     SQLINTEGER *     RowCountPtr);
****************************************/

SP_integer SPCDECL odbc_row_count(SPAPI_ARG_PROTO_DECL SP_term_ref statement_handle_spec, SP_integer * RowCountPtr)
{
  SQLLEN RowCount;
  SQLRETURN rc;
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT StatementHandle = real_statement_handle(StatementHandleIndex);

  ODBC_HSTMT_CHECK(StatementHandle);
  rc = SQLRowCount(StatementHandle,
                   &RowCount);
#if !POSTPONE
#error "what to return if rc != SQL_SUCCESS? if !SQL_SUCCEEDED?"
#endif  /* !POSTPONE */

  /* [PM] 4.2 May be -1 (or some "driver defined value") */
  *RowCountPtr = RowCount;
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs */
  *RowCountPtr = 0;
  goto cleanup;
}


/****************************************
SQLRETURN SQLFetch(
     SQLHSTMT     StatementHandle);
****************************************/

SP_integer SPCDECL odbc_fetch(SPAPI_ARG_PROTO_DECL SP_term_ref statement_handle_spec)
{
  SQLRETURN rc;
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT StatementHandle = real_statement_handle(StatementHandleIndex);

  ODBC_HSTMT_CHECK(StatementHandle);
  /* Ensure all column-related buffers are cleared so they are re-read with SQLGetData */
  {
    struct buflist * bl = state.statementhandlestack[StatementHandleIndex].buffers;

    for (;bl != NULL;bl = bl->next)
      {
        if (bl->ColumnNumber > 0)
          {
            void *buf  = bl->buf;
            SP_ASSERT( (!IS_FLAG_SET(bl->flags, BUFLIST_FLAG_NULL)) || buf == NULL); /* null implies no buf data */
            bl->buf = NULL;
            odbc_free(buf);
            CLEAR_FLAG(bl->flags,BUFLIST_FLAG_NULL);
          }
      }
  }
  

  rc = SQLFetch(StatementHandle);
 cleanup:
  return rc;
 barf:
  /* [PM] 4.2 Barf sets all outputs (i.e. none) */
  goto cleanup;
}

/*****************

 *****************/

SP_integer SPCDECL odbc_end_transaction(SPAPI_ARG_PROTO_DECL SP_term_ref connection_handle_spec,
                                        SP_integer commit)
{
  SQLRETURN rc;
  SP_integer const ConnectionHandleIndex = decode_connection_handle_spec(connection_handle_spec);
  SQLHDBC ConnectionHandle = real_connection_handle(ConnectionHandleIndex);
  SQLSMALLINT   CompletionType;

  if (ConnectionHandle == SQL_NULL_HDBC)
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_INVALID_HANDLE;
      goto barf;
    }
  switch (commit)
    {
    case 0:
      CompletionType = SQL_ROLLBACK;
      break;
    case 1:
      CompletionType = SQL_COMMIT;
      break;
    default:
      ODBC_BARF(ODBC_ERROR_INVALID_PARAMETER);
    }

  ODBC_CHECK(SQLEndTran(SQL_HANDLE_DBC, ConnectionHandle, CompletionType));
  
 cleanup:
  return rc;
 barf:
  goto cleanup;
}


#if ! HAVE_STRNLEN
/* strnlen is a linuxism (mostly ...). We must roll our own. */
static size_t strnlen(const char *s, size_t length)
{
  size_t n;

  for(n = 0; n < length && 0 != s[n]; n++)
    ;
  return n;
}
#endif /* ! HAVE_STRNLEN */

/* True if the T-string matches the 8-bit template */
static int matchesT(char const *templateA, SP_SQLTCHAR const *stringT)
{
  char const *pa = templateA;
  SP_SQLTCHAR const *pt = stringT;
  while (*pa != 0 && ((SQLTCHAR)*pa) == *pt)
    {
      pa++;
      pt++;
    }
  if (((SQLTCHAR)*pa) == *pt)
    {
      return SPIO_TRUE;
    }
  return SPIO_FALSE;
}

/* Returns a sqlstate, or the empty string if no state could be
   obtained. The returned value is a pointer to state.SqlstateT but
   you should not depend on this. */
static SP_SQLTCHAR const *get_sqlstateT(SQLHSTMT StatementHandle)
{
  SQLRETURN sql_rc;
  memset(state.SqlstateT, 0, sizeof state.SqlstateT);

#if ODBC_PREFER_SQLGETDIAGREC
  {
    SQLSMALLINT MessageLength = 0; /* DBG */
    SQLINTEGER NativeError = 0;
    SP_SQLTCHAR MessageTextT[142];
    /* [PM] 4.2.2 Got strange results (no
       SQLSTATE) if NULL message buffer was passed
       (instead of MessageText). (PostgreSQL,
       psqlODBC, Mac OS X 10.7 64-bit) */
    MessageTextT[0] = (SP_SQLTCHAR)0;
    memset(&MessageTextT, 0, sizeof MessageTextT);
    memset(state.SqlstateT, 0, sizeof state.SqlstateT); /* redundant, for debugging */
    sql_rc = SP_SQLGetDiagRecT(SQL_HANDLE_STMT,
                               StatementHandle,
                               1, /* SQLSMALLINT RecNumber; */
                               state.SqlstateT,
                               &NativeError,
                               MessageTextT,
                               (sizeof MessageTextT)/(sizeof MessageTextT[0]),
                               &MessageLength);
    if (SQL_SUCCEEDED(sql_rc))
      {
        if (sql_rc == SQL_SUCCESS_WITH_INFO)
          {
            /* MessageT truncated. */
            sql_rc = SQL_SUCCESS;
          }
#if !SP_SQL_UNICODE
        DEBUG_PRINT((stderr, "SP_SQLGetDiagRecT() returned State=\"%s\", MessageTestT=\"%s\", MessageLength=%" SPRIdINTEGER "\n", (char*)state.SqlstateT, (char*)MessageTextT, (SP_integer)MessageLength));
#endif  /* !SP_SQL_UNICODE */
      }
  }
#else /* !ODBC_PREFER_SQLGETDIAGREC */
  {
    SQLSMALLINT StringLength = 0; /* DBG */
    SP_SQLTCHAR Buffer[(sizeof state.SqlstateT)*3]; /* plenty of room. */
    size_t const BufferSize = sizeof Buffer;

    memset((void*) &Buffer[0], 0, BufferSize);

    /* [PM] 4.2.2 Got strange results (sql_rc ==
       SQL_SUCCESS_WITH_INFO with partially filled
       in SQLSTATE (PostgreSQL, psqlODBC, Mac OS X
       10.7 64-bit) */
    sql_rc = SP_SQLGetDiagFieldT(SQL_HANDLE_STMT,
                                 StatementHandle,
                                 1, /* SQLSMALLINT RecNumber; */
                                 (SQLSMALLINT)SQL_DIAG_SQLSTATE,
                                 &Buffer,
                                 BufferSize,
                                 &StringLength);
    SP_ASSERT(!SQL_SUCCEEDED(sql_rc) || StringLength == sizeof state.SqlstateT);
    SP_SOFT_ASSERT(sql_rc != SQL_SUCCESS_WITH_INFO); /* Do not expect truncation */
    memcpy(state.SqlstateT, Buffer, sizeof state.SqlstateT);
  }
#endif  /* !ODBC_PREFER_SQLGETDIAGREC */

  if (sql_rc != SQL_SUCCESS)
    {
      state.SqlstateT[0] = (SP_SQLTCHAR)0;

      /* Failure, unexpected */
      SP_SOFT_ASSERT(SPIO_FALSE);         
    }
  return state.SqlstateT;
}


/*
Get data from a preallocated buffer
Return PROLOG_RESULT_...

datatype: a SQL data type.

This now uses SQLGetData so the first access to a buffer must be in
column order. Subsequent accesses can be in any order since the value
is cached in the buffer.
*/
SP_integer SPCDECL buffer_get_data(SPAPI_ARG_PROTO_DECL
                                   SP_term_ref buffer_spec,
                                   SP_term_ref statement_handle_spec,
                                   SP_term_ref data)
{
  SP_integer prolog_rc; /* Not an SQLRETURN code */
  SP_integer const StatementHandleIndex = decode_statement_handle_spec(statement_handle_spec); /* may overwrite statement_handle_spec */
  SQLHSTMT const StatementHandle = real_statement_handle(StatementHandleIndex);
  SQLSMALLINT TargetType;
  void *TargetTypePtr;
  size_t TargetTypeSize;
  void *buf = NULL; /* Needs cleanup */

  if (StatementHandle == SQL_NULL_HSTMT) {
    SP_ASSERT(0);
    prolog_rc = PROLOG_RESULT_INVALID_HANDLE;
    goto barf;
  }

  {
    SP_integer buffer_id = decode_buffer_spec(buffer_spec); /* may overwrite buffer_spec */
    struct buflist * bl = find_buflist_entry(StatementHandleIndex, buffer_id);
    if (bl == NULL)
      {
        SP_ASSERT(0);
        prolog_rc = PROLOG_RESULT_INVALID_BUFFER;
        goto barf;      
      }

    if (IS_FLAG_SET(bl->flags, BUFLIST_FLAG_NULL))
      {
        goto return_null;
      }

    TargetType = bl->TargetType; /* [PM] 4.2 May be SQL_C_DEFAULT, handled below */
    DEBUG_PRINT((stderr, "\nODBC_DBG: buffer_get_data: %p Type %d, Size=%" SPRIdINTEGER " bytes, ColumnNumber=%d\n", (void*)buffer_id, (int)bl->TargetType, (SP_integer)bl->size, (int)bl->ColumnNumber));
    {
      SP_ASSERT(!IS_FLAG_SET(bl->flags, BUFLIST_FLAG_NULL));
      if (bl->buf == NULL)
        {
          SQLRETURN sql_rc;
          int nul_size = 0; /* sizeof(SQLCHAR) (1) or sizeof(SQLWCHAR) (2 or more). */
          SP_ASSERT(bl->ColumnNumber > 0);
          if (! (bl->ColumnNumber > 0) )
            {
              prolog_rc = PROLOG_RESULT_IMPOSSIBLE_ERROR;
              goto barf;
            }

          {
            switch (TargetType) {
            case SQL_C_DEFAULT:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_DEFAULT\n");
              {
                /* [PM] 4.2.2 Should not happen with USE_GET_DATA (and NULL buf) */
                SP_ASSERT(0);
                goto unknown_datatype;
              }
              break;
            case SQL_C_CHAR:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_CHAR\n");
              {
#if !POSTPONE
#error "Should we mark the buffer as 'not fixed type' or some such"
#error "Should perhaps not blindly zap the size"
#endif  /* !POSTPONE */
                bl->size = 0;
                SP_ASSERT(bl->size == 0);
                nul_size = sizeof (SQLCHAR);
              }
              break;

            case SQL_C_WCHAR:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_WCHAR\n");
#if ENABLE_WCHAR
              {
#if !POSTPONE
#error "Should we mark the buffer as 'not fixed type' or some such"
#error "Should perhaps not blindly zap the size"
#endif  /* !POSTPONE */
                bl->size = 0;
                SP_ASSERT(bl->size == 0);
                nul_size = sizeof (SQLWCHAR);
              }
#else /* !ENABLE_WCHAR */
              goto unsupported_datatype;
#endif  /* ENABLE_WCHAR */
              break;
            case SQL_C_BIT:
            case SQL_C_STINYINT:
            case SQL_C_SSHORT:
            case SQL_C_SLONG:
            case SQL_C_SBIGINT:
            case SQL_C_FLOAT:
            case SQL_C_DOUBLE:
            case SQL_C_TYPE_DATE:
            case SQL_C_TYPE_TIME:
            case SQL_C_TYPE_TIMESTAMP:
            case SQL_C_INTERVAL_MONTH:
              SP_ASSERT(bl->size > 0);
              break;
            case SQL_C_INTERVAL_YEAR:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_YEAR (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_YEAR_TO_MONTH:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_YEAR_TO_MONTH (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_DAY:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_DAY (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_HOUR:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_HOUR (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_MINUTE:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_MINUTE (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_SECOND:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_SECOND (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_DAY_TO_HOUR:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_DAY_TO_HOUR (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_DAY_TO_MINUTE:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_DAY_TO_MINUTE (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_DAY_TO_SECOND:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_DAY_TO_SECOND (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_HOUR_TO_MINUTE:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_HOUR_TO_MINUTE (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_HOUR_TO_SECOND:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_HOUR_TO_SECOND (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;
            case SQL_C_INTERVAL_MINUTE_TO_SECOND:
              DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_MINUTE_TO_SECOND (not supported)\n");
              goto unsupported_datatype; /* FIXME: support this datatype(s) */
              break;

            default:
              DEBUG_PRINT((stderr, "\nODBC_DBG: buffer_get_data: unhandled TargetType: %" SPRIdINTEGER "\n", (SP_integer)TargetType));
              SP_ASSERT(0);
              goto unknown_datatype;    /* Unknown data type */
              break;
            }

            /* Here bl->size is either correct, and thus > 0, or zero
               for the char types where the size is not yet known.

               Here TargetType is already set (and is the same as
               bl->TargetType).

            */
            SP_ASSERT((bl->size == 0) == (TargetType == SQL_C_CHAR || TargetType == SQL_C_WCHAR));
            SP_ASSERT(bl->TargetType == TargetType);
            DEBUG_PRINT((stderr, "\nODBC_DBG: After normalization: buffer_get_data: %p Size=%" SPRIdINTEGER "bytes, ColumnNumber=%d, nul_size=%d\n", (void*)buffer_id, (SP_integer)bl->size, (int)bl->ColumnNumber, (int)nul_size));

            /*
              "within a row of data, the value of the Col_or_Param_Num
              argument in each call to SQLGetData must be greater than
              or equal to the value of Col_or_Param_Num in the
              previous call; that is, data must be retrieved in
              increasing column number order."

              "Finally, if no extensions are supported, SQLGetData
              cannot be called if the rowset size is greater than 1."
            */
            if (bl->size == 0)
              {
                /* [PM] 4.2.1 Note: Even if driver is using UTF-8
                   (which could require four bytes to encode one
                   characters) it seems to work well to only have one
                   byte of data available (excluding NUL).
                */
                SQLLEN const buf_size_min_available = ( nul_size == 0 ? 1 : nul_size +nul_size); /* Room for One data item + (if char type) NUL. */

                SQLLEN buf_size_increment;
                SQLLEN StrLen_or_Ind; /* Note: byte size. Also for character types (!) */
                SQLLEN buf_size; /* Note: byte size. Also for character types (!) */
                SQLLEN byte_offset;
                int num_calls; /* Number of calls to SQLGetData() (only the first call may return $NULL */
                (void)buf_size_min_available; /* assertions only */
                SP_ASSERT(TargetType == SQL_C_CHAR || TargetType == SQL_C_WCHAR); /* Must never come here for "fixed-length data, such as an integer or a date structure" */

                num_calls = 0;
                buf = NULL;
                byte_offset = 0;
                buf_size = 0;

#if !ODBC_FORCE_BUILD && SICSTUS_VERSION > 40301 /* [PM] 4.3.1 postpone. */
#error "test ODBC_DEBUG_SMALL_GET_DATA on all odbc variants (Windows (UTF-16), iODBC (2byte wchar), unixODBC (4byte wchar). I worry about nul_size"
#endif /* !ODBC_FORCE_BUILD */

#if ODBC_DEBUG_SMALL_GET_DATA
                buf_size_increment = buf_size_min_available;
#else /* !ODBC_DEBUG_SMALL_GET_DATA */
                buf_size_increment = 1024*sizeof(SQLWCHAR); /* Just a heuristic, OK also for binary or plain SQLCHAR types. */
#endif /* !ODBC_DEBUG_SMALL_GET_DATA */

                /* Exits loop with buf and buf_size set */
                while (SPIO_TRUE) {
                  /* Invariants:
                     buf contains already read data (or is NULL if num_calls==0).
                     byte_offset is size of existing prefix in buf.
                     buf_size is size of data buffer (>= byte_offset)
                     buf_increment is how much larger buf should become before calling SQLGetData()

                     (buf_size_min_available is how much larger
                     buf_size must be than buf_offset before calling
                     SQLGetData(), i.e. one data item + (if text) NUL)
                  */
                  SP_ASSERT(buf_size >= byte_offset);
                  /* buf_size is only modified here, and always increased with at least 1 so we make some progress in each iteration. */
                  SP_ASSERT(buf_size_increment > 0); /* we make progress */
                  buf_size += buf_size_increment;

                  {
                    void *tmp;

                    if (buf == NULL)
                      {
                        SP_ASSERT(num_calls == 0 && buf_size == buf_size_increment);
                        tmp = odbc_malloc(buf_size);
                      }
                    else
                      {
                        SP_ASSERT(num_calls >= 0 && buf_size > buf_size_increment);
                        tmp = odbc_realloc(buf, buf_size);
                      }
                    if (tmp == NULL)
                      {
                        SP_SOFT_ASSERT(SPIO_FALSE);
                        goto out_of_memory;
                      }
                    buf = tmp;
                  }
                  /* Here size of buf is buf_size, and there is room for at least one data item and possibly NUL if text. */
                  SP_ASSERT(buf != NULL && buf_size >= byte_offset && buf_size - byte_offset >= buf_size_min_available);
                  {
                    SQLLEN const remaining_buffer_size = buf_size-byte_offset;
                    SP_ASSERT(remaining_buffer_size >= (nul_size == 0 ? 1 : nul_size)); /* Must have room for NUL, at least */
                    SP_ASSERT(remaining_buffer_size >= buf_size_min_available);
                    /* Must be even multiple of nul_size (so we can
                       figure out how much was written on
                       truncation). */
                    SP_ASSERT(nul_size == 0 || ((remaining_buffer_size/nul_size)*nul_size) == remaining_buffer_size);

                    StrLen_or_Ind = 0;
                    /*
                      [PM] 4.2.2 iodbc on 64-bit Mac OS X 10.6.8
                      valgrind complains about "Warning: silly arg
                      (-3) to malloc()". This seems to be because bugs
                      in the iodbc tracing code which fails to
                      recognize SQL_NULL_DATA (-1) (because it does,
                      in effect (((size_t)len)<0) in dm_SQL_W2A used
                      by trace_GetData()).
                    */
                    sql_rc = SQLGetData(
                                        StatementHandle,
                                        bl->ColumnNumber,
                                        bl->TargetType,
                                        (SQLPOINTER)(((char*)buf)+byte_offset), /* skip already obtained data */
                                        remaining_buffer_size,
                                        &StrLen_or_Ind);
                    num_calls++;
                    if (num_calls > 1)
                      {
                        DEBUG_PRINT((stderr, 
                                     "SQLGetData() Assembling parts, call %d, ColumnNumber=%d, TargetType=%d, byte_offset=%" SPRIdINTEGER ", Strlen_Or_Ind = %" SPRIdINTEGER ", buf=\"%s\"\n",
                                     (int)num_calls,
                                     (int)bl->ColumnNumber,
                                     (int)bl->TargetType,
                                     (SP_integer)byte_offset,
                                     (SP_integer)StrLen_or_Ind,
                                     (char*)buf));
                      }
                    /* 01004 (Data Truncated) */
                    if (sql_rc == SQL_SUCCESS_WITH_INFO && matchesT("01004", get_sqlstateT(StatementHandle)))
                      {
                        /* Entire (remaining_buffer_size byte suffix
                           of) buf was filled.

                           [PM] 4.2.2. Docs are unclear about how much
                           data has been written.  I assume an
                           integral number of items + a nul_size-ed
                           NUL.
                        */
                        /* There should always be room for one item and any required NUL. */
                        SP_ASSERT(remaining_buffer_size >= buf_size_min_available);
                        SP_ASSERT(remaining_buffer_size > nul_size);
                        /* The next iteration should skip everything
                           written by the preceeeding iteration except
                           that it should overwrite the
                           NUL-termination written by the preceeding
                           iteration */
                        byte_offset += (remaining_buffer_size - nul_size);

                        /* StrLen_or_Ind indicates how much data was
                           available. Use this, if available to
                           compute a suitable buffer size. */
                        if (StrLen_or_Ind == SQL_NO_TOTAL || StrLen_or_Ind <= 0)
                          {
                            /* StrLen_or_Ind should not be zero or
                               less (unless SQL_NO_TOTAL) but, if it
                               is, we just pretend we got
                               SQL_NO_TOTAL. */
                            SP_ASSERT(StrLen_or_Ind == SQL_NO_TOTAL);

                            /*
                              "If the driver cannot determine the
                              length of the data after conversion, as
                              is sometimes the case with long data, it
                              returns SQL_SUCCESS_WITH_INFO and sets
                              the length to SQL_NO_TOTAL."
                            */
                            /* Keep existing buf_size_increment */
                            SP_ASSERT(buf_size_increment >= buf_size_min_available);
                          }
                        else    /* StrLen_or_Ind is valid. */
                          {
                            SP_ASSERT(StrLen_or_Ind > 0);
#if ODBC_DEBUG_SMALL_GET_DATA /* Debug */
                            {
                              /* There is already room for NUL, if any. */
                              SP_ASSERT(buf_size >= buf_offset && ((buf_size-buf_offset) >= nul_size));
                              /* Make room for one additional data item (over and above, the already available, room for NUL) */
                              buf_size_increment = (nul_size == 0 ? 1 : nul_size );
                              /* Now there is room for buf_size_increment, i.e. one item and, if text, one NUL. */
                              SP_ASSERT(buf_size > buf_offset && ((buf_size + buf_size_increment)-buf_offset) >= buf_size_min_available);
                            }
#else /* !ODBC_DEBUG_SMALL_GET_DATA */
                            {
                              /* Expected case. StrLen_or_Ind tells us how much data is available. */
                              /* This should be exactly how much data
                                 is available (not including the NUL
                                 character). Note that buf already
                                 contains room for a NUL character (if
                                 non-binary type). */
                              buf_size_increment = StrLen_or_Ind;
                            }
#endif  /* !ODBC_DEBUG_SMALL_GET_DATA */
                          }
                        continue;
                      }

                    if (sql_rc == SQL_SUCCESS)
                      {
                        /* Done. Either null (on first call) or final tail of buffer (on first or subsequent call) */
                        if (StrLen_or_Ind == SQL_NULL_DATA)
                          {
                            SP_ASSERT(num_calls == 1);
                            SP_ASSERT(bl->buf == NULL);
                            /* Remember null-ness for subsequent reads. */
                            SET_FLAG(bl->flags, BUFLIST_FLAG_NULL);
                            goto return_null;
                          }

                        /*
                          "The last call to SQLGetData must always
                          return the length of the data, not zero or
                          SQL_NO_TOTAL."
                          But, of course, this is not true for empty columns...
                        */
                        SP_ASSERT(num_calls == 1 || StrLen_or_Ind != 0);
                        SP_ASSERT(StrLen_or_Ind != SQL_NO_TOTAL); /* Says the docs. Not clear if this holds */
                        SP_ASSERT(StrLen_or_Ind <= remaining_buffer_size);
                        /* Only the first call is allowed to return zero length. */
                        if (! ((num_calls == 1 ? 0 : 1) <= StrLen_or_Ind && StrLen_or_Ind <= remaining_buffer_size) )
                          {
                            goto impossible_error;
                          }
                        byte_offset += StrLen_or_Ind;
                        SP_ASSERT(byte_offset+nul_size <= buf_size);
                        if (nul_size == 0) /* binary type */
                          {
                            /* Should only happen for binary variable-size data (which is not NUL-terminated). */
                            /* We do not support binary data so should not get here */
                            SP_ASSERT(SPIO_FALSE);
                            goto impossible_error;
                          }
                        else if (byte_offset+nul_size <= buf_size) /* room for NUL */
                          {
                            SP_ASSERT(byte_offset <= buf_size);
#if ODBC_DEBUG
                            {
                              SQLLEN i;
                              for (i = byte_offset; i < buf_size; i++)
                                {
                                  /* Fill with non-NUL garbage to ensure that nothing can depend on NUL-termination. */
                                  ((char*)buf)[i] = (char)(((i+1) & 0xFF) | 0x40);
                                }
                            }
#endif  /* ODBC_DEBUG */
                          }
                        else
                          {
                            /* SQLGetData should have NUL-terminated which is not included in StrLen_or_Ind */
                            SP_ASSERT(SPIO_FALSE);
                            goto impossible_error;
                          }
                        /* Done. buf contains byte_offset (<=
                           buf_size) of data. */
                        break;
                      }
                    else if (sql_rc == SQL_SUCCESS_WITH_INFO)
                      {
                        /* some other warning/error. Unexpected. */
                        SP_SOFT_ASSERT(SPIO_FALSE);
                        goto unexpected_error;
                      }
                    else        /* SQLGetData failed */
                      {
                        SP_SOFT_ASSERT(SPIO_TRUE);
                        goto data_conversion_failed;
                      }
                  }
                }; /* SQLGetData loop */
                bl->size = byte_offset;
                bl->buf = buf;
                buf = NULL; /* protect from cleanup */
              }
            else /* buffer size known */
              {
                SQLLEN StrLen_or_Ind = 0;

                SP_ASSERT( !(TargetType == SQL_C_CHAR || TargetType == SQL_C_WCHAR) );
                buf  = odbc_malloc(bl->size);
                if (buf == NULL)
                  {
                    goto out_of_memory;
                  }
                bl->buf = buf;
                buf = NULL; /* protect from cleanup */
                DEBUG_PRINT((stderr, "\nODBC_DBG: buffer_get_data: Fixed type(%d), %p Size=%" SPRIdINTEGER " bytes, ColumnNumber=%d\n", (int)TargetType, (void*)buffer_id, (SP_integer)bl->size, (int)bl->ColumnNumber));
                /*
                  [PM] 4.2.2 iodbc on 64-bit Mac OS X 10.6.8 valgrind
                  complains about Invalid read of size 8 (0 bytes
                  inside block of size 4) when TargetType ==
                  SQL_C_SLONG (-16). This seems to be because bugs in
                  the iodbc tracing code which uses long (8 bytes)
                  instead of SQLINTEGER (4 bytes) in trace_SQLGetData
                  to read the data.

		  [PM] 4.2.2 On CentOS 6.2 with
		  mysql-connector-odbc-5.1.5r1144-7 (x86_64)
		  odbc_suite type_test fails because a BIT value is
		  read as 1 for both rows whereas one of the rows was
		  written as zero (the correct values are written,
		  verified from a Windows box). I blame the ODBC
		  driver, possibly related to
		  <http://bugs.mysql.com/39644>.
                 */
                sql_rc = SQLGetData(StatementHandle, /* SQLHSTMT       StatementHandle, */
                                    bl->ColumnNumber, /* SQLUSMALLINT   Col_or_Param_Num, */
                                    TargetType, /* SQLSMALLINT    TargetType, */
                                    bl->buf,
                                    bl->size, /* Note: Size is ignored for fixed-size types (!) */
                                    &StrLen_or_Ind);
                if (sql_rc == SQL_SUCCESS)
                  {
                    DEBUG_PRINT((stderr, "\nODBC_DBG: buffer_get_data: After SQLGetData fixed type(%d): %p Size=%" SPRIdINTEGER " bytes, ColumnNumber=%d, StrLen_or_Ind=%" SPRIdINTEGER "\n", (int)TargetType, (void*)buffer_id, (SP_integer)bl->size, (int)bl->ColumnNumber, (SP_integer)StrLen_or_Ind));
                    if (StrLen_or_Ind == SQL_NULL_DATA)
                      {
                        void *tmp = bl->buf;

                        bl->buf = NULL;
                        odbc_free(tmp);

                        SP_ASSERT(bl->buf == NULL);
                        /* Remember null-ness for subsequent reads. */
                        SET_FLAG(bl->flags, BUFLIST_FLAG_NULL);
                        goto return_null;
                      }

                    /* Done, bl->buf is set, bl->size is OK. */
                    SP_ASSERT(SPIO_TRUE);
                  }
                else if (sql_rc == SQL_SUCCESS_WITH_INFO)
                  {
                    SP_SOFT_ASSERT(SPIO_FALSE); /* What to do? */
                    goto type_error;
                  }
                else
                  {
                    SP_ASSERT(!SQL_SUCCEEDED(sql_rc));
                    SP_SOFT_ASSERT(SPIO_TRUE);
                    goto data_conversion_failed;
                  }
              }
            /* By now data should be available */
            SP_ASSERT(bl->buf != NULL);

            SP_ASSERT(bl->size > 0 || (bl->size == 0 && (bl->TargetType == SQL_C_CHAR || bl->TargetType == SQL_C_WCHAR)));

          }
        }
      else                      /* bl->buf != NULL */
        {
          /* Data already obtained, somehow, and not null. */
          SP_ASSERT(bl->buf != NULL && !IS_FLAG_SET(bl->flags, BUFLIST_FLAG_NULL));
          SP_ASSERT(bl->size > 0 || (bl->size == 0 && (bl->TargetType == SQL_C_CHAR || bl->TargetType == SQL_C_WCHAR)));
        }
    }

    TargetTypePtr = bl->buf;
    TargetTypeSize = bl->size;
  }

  prolog_rc = PROLOG_RESULT_NOERR;     /* assume success */

  /* [PM] 4.2 One case for each TargetType emitted by
     map_sql_data_type_to_c_data_type_and_size() and vice versa (except that we
     also must handle SQL_C_DEFAULT here) */
  switch (TargetType) {
  case SQL_C_DEFAULT:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_DEFAULT\n");
    {
      /* [PM] 4.2 Only used for SQLLEN (StrIndBuf) which do not
         correspond to any SQL_C_... constant. We could fake it by
         selecting SQL_C_INTEGER and SQL_C_SBIGINT, on 32 and 64-bit,
         respectively. */
      if (TargetTypeSize == sizeof(SQLLEN))
        {
          SQLLEN tmp = *(SQLLEN*)TargetTypePtr;
          SP_ASSERT(0);         /* [PM] 4.2.2 We do not use this anymore */
          if (!SP_put_integer_bytes(data, &tmp, sizeof tmp, SPIO_TRUE))
            {
              goto data_conversion_failed;
            }
        }
      else
        {
          SP_ASSERT(0);
          goto unknown_datatype;
        }
    }
    break;
  case SQL_C_CHAR:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_CHAR\n");
    {
      unsigned char const *tmp = TargetTypePtr;
      /* [PM] 4.2.2. Beware that TargetTypeSize can be zero (and no NUL-termination) */
      size_t len = strnlen((char const *)tmp,TargetTypeSize); /* [PM] 4.2 SQL_C_CHAR (i.e. SQL_CHAR, SQL_VARCHAR) is NUL-terminated */
      SP_ASSERT(len <= TargetTypeSize);
#if ODBC_DEBUG
      {
        size_t i;
        DEBUG_PRINT((stderr, "Got SQL_C_CHAR: \""));
        for (i = 0; i < len; i++)
          {
            char c = ((char*)tmp)[i];
            if (c == 0)
              {
                break;
              }
            DEBUG_PRINT((stderr, "%c", (char)c));
          }
        DEBUG_PRINT((stderr, "\"\n"));
      }
#endif  /* ODBC_DEBUG */
      {
        struct statementhandle *sh_entry = real_statement_handle_entry(StatementHandleIndex);
        SP_ASSERT(sh_entry != NULL && sh_entry->real_handle == StatementHandle);
        {
          struct connectionhandle *ch_entry = real_connection_handle_entry(sh_entry->connection_handle_index);
          SP_ASSERT(ch_entry != NULL && ch_entry->real_handle != SQL_NULL_HDBC);
          {
            int const char_utf8 = IS_FLAG_SET(ch_entry->flags, CONNECTION_HANDLE_FLAG_UTF8_CHAR);

            if (char_utf8)
              {
                char *ienc = (char*)tmp;
                /* [PM] 4.2.2 Note, at least up to 4.2.2 beta, SP_put_list_n_codes() does not report encoding errors */
                if (!SP_put_list_n_codes(data, SP_new_term_ref(), len, ienc)) {
                  goto data_conversion_failed;
                }
              }
            else
              {
                if (!SP_put_list_n_bytes(data, SP_new_term_ref(), len, tmp)) {
                  goto data_conversion_failed;
                }
              }
          }
        }
      }
    }
    break;

  case SQL_C_WCHAR:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_WCHAR\n");
#if ENABLE_WCHAR
    {
      SQLWCHAR const *tmp = TargetTypePtr;
      size_t const item_size = sizeof tmp[0];
      size_t const target_type_len = (TargetTypeSize/item_size);
      SP_ASSERT(target_type_len*item_size == TargetTypeSize); /* should be even multiple */
      {
        SQLRETURN sql_rc;
        int sp_rc;
        char *ienc = NULL; /* Not NUL-terminated */
	size_t ienc_size = 0;
        /* [PM] 4.2.2 Something is wrong, getting here with TargetTypeSize == 3 */
        size_t len = wchars_strnlen(tmp,target_type_len);
        SP_ASSERT(len <= target_type_len);

        /* tmp may not be NUL-terminated (apparently) (seen for single wchar column) */
        sql_rc = from_n_sqlwchars(tmp, len, &ienc, &ienc_size); /* Does not NUL-terminate */
        if (sql_rc != SQL_SUCCESS) {
          goto data_conversion_failed; /* FIXME: improve error reporting */
        }
#if ODBC_DEBUG
        {
	  int i;
          DEBUG_PRINT((stderr, "Got SQL_C_WCHAR: (as ienc) \""));
          for (i = 0; i < ienc_size; i++)
            {
              DEBUG_PRINT((stderr, "%c", (char)ienc[i]));
            }
          DEBUG_PRINT((stderr, "\"\n"));
        }
#endif  /* ODBC_DEBUG */
        /* [PM] 4.2.2 Note, at least up to 4.2.2 beta,
           SP_put_list_n_codes() does not report encoding errors (but
           that is unexpected here since from_n_sqlwchars should produce
           valid ienc). */
        sp_rc = SP_put_list_n_codes(data, SP_new_term_ref(), ienc_size, ienc);
        odbc_free(ienc); /* Before checking sp_rc */
        ienc = NULL;
        if (!sp_rc) {
          goto data_conversion_failed;
        }
      }
    }
#else
    goto unsupported_datatype;
#endif  /* ENABLE_WCHAR */
    break;
  case SQL_C_BIT:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_BIT\n");
    {
      SQLCHAR tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQLCHAR*)TargetTypePtr;
      if (!SP_put_integer(data, tmp)) {
        goto data_conversion_failed;
      }
    }
    break;

  case SQL_C_STINYINT:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_STINYINT\n");
    {
      SQLSCHAR tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQLSCHAR*)TargetTypePtr;
      if (!SP_put_integer(data, tmp)) {
        goto data_conversion_failed;
      }
    }
    break;

  case SQL_C_SSHORT:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_SSHORT\n");
#if ODBC_BROKEN_SHORT
    SP_ASSERT(SPIO_FALSE);
    goto data_conversion_failed;
#else /* !ODBC_BROKEN_SHORT */
    {
      SQLSMALLINT tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQLSMALLINT*)TargetTypePtr;
      if (!SP_put_integer(data, tmp)) {
        goto data_conversion_failed;
      }
    }
    break;
#endif /* !ODBC_BROKEN_SHORT */

  case SQL_C_SLONG:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_SLONG\n");
    {
      SQLINTEGER tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQLINTEGER*)TargetTypePtr;
      if (!SP_put_integer(data, tmp)) {
        goto data_conversion_failed;
      }
    }
    break;

  case SQL_C_SBIGINT:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_SBIGINT\n");
    {
      SQLBIGINT tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQLBIGINT*)TargetTypePtr;
      if (!SP_put_integer_bytes(data, &tmp, sizeof tmp, SPIO_TRUE)) {
        goto data_conversion_failed;
      }
    }
    break;

  case SQL_C_FLOAT:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_FLOAT\n");
    {
      SQLREAL tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQLREAL*)TargetTypePtr;
      if (!SP_put_float(data, tmp)) {
        goto data_conversion_failed;
      }
    }
    break;

  case SQL_C_DOUBLE:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_DOUBLE\n");
    {
      SQLDOUBLE tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQLDOUBLE*)TargetTypePtr;
      if (!SP_put_float(data, tmp)) {
        goto data_conversion_failed;
      }
    }
    break;

  case SQL_C_TYPE_DATE:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_TYPE_DATE\n");
    {
      SQL_DATE_STRUCT tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQL_DATE_STRUCT*)TargetTypePtr;
      {
        SQL_DATE_STRUCT * date = &tmp;
        SP_term_ref y = SP_new_term_ref();
        SP_term_ref m = SP_new_term_ref();
        SP_term_ref d = SP_new_term_ref();
        if (!SP_put_integer(y, date->year)
            ||
            !SP_put_integer(m, date->month)
            ||
            !SP_put_integer(d, date->day)
            ||
            !SP_cons_functor(data, state.date_name, 3, y, m, d)) {
          goto data_conversion_failed;
        }
      }
    }
    break;

  case SQL_C_TYPE_TIME:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_TYPE_TIME\n");
    {
      SQL_TIME_STRUCT tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQL_TIME_STRUCT*)TargetTypePtr;
      {
        SQL_TIME_STRUCT * time = &tmp;
        SP_term_ref h = SP_new_term_ref();
        SP_term_ref m = SP_new_term_ref();
        SP_term_ref s = SP_new_term_ref();
        if (!SP_put_integer(h, time->hour)
            ||
            !SP_put_integer(m, time->minute)
            ||
            !SP_put_integer(s, time->second)
            ||
            !SP_cons_functor(data, state.time_name, 3, h, m, s)) {
          goto data_conversion_failed;
        }
      }
    }
    break;

  case SQL_C_TYPE_TIMESTAMP:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_TYPE_TIMESTAMP\n");
    {
      SQL_TIMESTAMP_STRUCT tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQL_TIMESTAMP_STRUCT*)TargetTypePtr;
      {
        SQL_TIMESTAMP_STRUCT * timestamp = &tmp;
        SP_term_ref y = SP_new_term_ref();
        SP_term_ref mo = SP_new_term_ref();
        SP_term_ref d = SP_new_term_ref();
        SP_term_ref h = SP_new_term_ref();
        SP_term_ref mi = SP_new_term_ref();
        SP_term_ref s = SP_new_term_ref();
        SP_term_ref f = SP_new_term_ref();
        if (!SP_put_integer(y, timestamp->year)
            ||
            !SP_put_integer(mo, timestamp->month)
            ||
            !SP_put_integer(d, timestamp->day)
            ||
            !SP_put_integer(h, timestamp->hour)
            ||
            !SP_put_integer(mi, timestamp->minute)
            ||
            !SP_put_integer(s, timestamp->second)
            ||
            !SP_put_integer(f, timestamp->fraction)
            ||
            !SP_cons_functor(data, state.timestamp_name, 7, y, mo, d, h, mi, s, f)) {
          goto data_conversion_failed;
        }
      }
    }
    break;

  case SQL_C_INTERVAL_MONTH:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_MONTH (not supported)\n");
    {
      SQL_INTERVAL_STRUCT tmp;
      SP_ASSERT(TargetTypeSize == sizeof tmp);
      tmp = *(SQL_INTERVAL_STRUCT*)TargetTypePtr;
      (void)tmp;
      goto unsupported_datatype; /* FIXME: support this datatype(s) */
    }
    break;
  case SQL_C_INTERVAL_YEAR:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_YEAR (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_YEAR_TO_MONTH:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_YEAR_TO_MONTH (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_DAY:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_DAY (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_HOUR:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_HOUR (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_MINUTE:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_MINUTE (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_SECOND:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_SECOND (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_DAY_TO_HOUR:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_DAY_TO_HOUR (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_DAY_TO_MINUTE:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_DAY_TO_MINUTE (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_DAY_TO_SECOND:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_DAY_TO_SECOND (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_HOUR_TO_MINUTE:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_HOUR_TO_MINUTE (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_HOUR_TO_SECOND:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_HOUR_TO_SECOND (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;
  case SQL_C_INTERVAL_MINUTE_TO_SECOND:
    DEBUG_PRINT0("\nODBC_DBG: buffer_get_data: SQL_C_INTERVAL_MINUTE_TO_SECOND (not supported)\n");
    goto unsupported_datatype; /* FIXME: support this datatype(s) */
    break;

  default:
    DEBUG_PRINT((stderr, "\nODBC_DBG: buffer_get_data: unhandled TargetType: %" SPRIdINTEGER "\n", (SP_integer)TargetType));
    SP_ASSERT(0);
    goto unknown_datatype;    /* Unknown data type */
    break;
  }
  SP_ASSERT(prolog_rc == PROLOG_RESULT_NOERR);

 cleanup:
  if (buf != NULL)
    {
      odbc_free(buf);
      buf = NULL;
    }
  return prolog_rc;

 barf:
  SP_ASSERT(prolog_rc <= 0);
  goto cleanup;

 return_null:
  {
    /* SP_ASSERT(bl != null && IS_FLAG_SET(bl->flags, BUFLIST_FLAG_NULL) && bl->buf == NULL); */
    if (!SP_put_atom(data, state.null)) {
      SP_ASSERT(SPIO_FALSE); /* unexpected */
      goto data_conversion_failed;
    }
    prolog_rc = PROLOG_RESULT_NOERR;
    goto cleanup;
  }

 unknown_datatype:
  prolog_rc = PROLOG_RESULT_UNKNOWN_DATATYPE;
  goto barf;

 unsupported_datatype:
  prolog_rc = PROLOG_RESULT_UNSUPPORTED_DATA_TYPE;
  goto barf;
  
 unexpected_error:
  prolog_rc = PROLOG_RESULT_DATA_CONVERSION_FAILED;
  goto barf;
 impossible_error:
  prolog_rc = PROLOG_RESULT_IMPOSSIBLE_ERROR;
  goto barf;
 out_of_memory:
  prolog_rc = PROLOG_RESULT_OUT_OF_MEMORY;
  goto barf;

 type_error:
  prolog_rc = PROLOG_RESULT_TYPE_ERROR;
  goto barf;

 data_conversion_failed:
  prolog_rc = PROLOG_RESULT_DATA_CONVERSION_FAILED;
  goto barf;
}

/* Header symbol lookup

return the numeric value of a header symbolic constant.
return PROLOG_NATIVE_CODE_ERROR if unknown symbol. return PROLOG_NATIVE_CODE_SUCCESS otherwise.

foreign(symbol_lookup,
        '$symbol_lookup'(+string, -integer, [-integer])).

*/
SP_integer SPCDECL symbol_lookup(SPAPI_ARG_PROTO_DECL char const * symbol, SP_integer * value)
{
#define symret(sym) if (0 == strcmp(#sym , symbol)) \
    { *value = sym; return PROLOG_NATIVE_CODE_SUCCESS; }

/* SQL data types */
  symret(SQL_CHAR);
  symret(SQL_VARCHAR);
  symret(SQL_LONGVARCHAR);
  symret(SQL_WCHAR);
  symret(SQL_WVARCHAR);
  symret(SQL_WLONGVARCHAR);
  symret(SQL_DECIMAL);
  symret(SQL_NUMERIC);
  symret(SQL_BIT);
  symret(SQL_TINYINT);
  symret(SQL_SMALLINT);
  symret(SQL_INTEGER);
  symret(SQL_BIGINT);
  symret(SQL_REAL);
  symret(SQL_FLOAT);
  symret(SQL_DOUBLE);
  symret(SQL_BINARY);
  symret(SQL_VARBINARY);
  symret(SQL_LONGVARBINARY);
  symret(SQL_TYPE_DATE);
  symret(SQL_TYPE_TIME);
  symret(SQL_TYPE_TIMESTAMP);
  symret(SQL_INTERVAL_MONTH);
  symret(SQL_INTERVAL_YEAR);
  symret(SQL_INTERVAL_YEAR_TO_MONTH);
  symret(SQL_INTERVAL_DAY);
  symret(SQL_INTERVAL_HOUR);
  symret(SQL_INTERVAL_MINUTE);
  symret(SQL_INTERVAL_SECOND);
  symret(SQL_INTERVAL_DAY_TO_HOUR);
  symret(SQL_INTERVAL_DAY_TO_MINUTE);
  symret(SQL_INTERVAL_DAY_TO_SECOND);
  symret(SQL_INTERVAL_HOUR_TO_MINUTE);
  symret(SQL_INTERVAL_HOUR_TO_SECOND);
  symret(SQL_INTERVAL_MINUTE_TO_SECOND);
  symret(SQL_GUID);
  symret(SQL_ALL_TYPES);
  symret(SQL_UNKNOWN_TYPE);
/* C data types */
  symret(SQL_C_CHAR);
  symret(SQL_C_WCHAR);
  symret(SQL_C_BIT);
  symret(SQL_C_STINYINT);
  symret(SQL_C_UTINYINT);
  symret(SQL_C_SSHORT);
  symret(SQL_C_USHORT);
  symret(SQL_C_SLONG);
  symret(SQL_C_ULONG);
  symret(SQL_C_SBIGINT);
  symret(SQL_C_UBIGINT);
  symret(SQL_C_FLOAT);
  symret(SQL_C_DOUBLE);
  symret(SQL_C_BINARY);
  symret(SQL_C_TYPE_DATE);
  symret(SQL_C_TYPE_TIME);
  symret(SQL_C_TYPE_TIMESTAMP);
  symret(SQL_C_INTERVAL_MONTH);
  symret(SQL_C_INTERVAL_YEAR);
  symret(SQL_C_INTERVAL_YEAR_TO_MONTH);
  symret(SQL_C_INTERVAL_DAY);
  symret(SQL_C_INTERVAL_HOUR);
  symret(SQL_C_INTERVAL_MINUTE);
  symret(SQL_C_INTERVAL_SECOND);
  symret(SQL_C_INTERVAL_DAY_TO_HOUR);
  symret(SQL_C_INTERVAL_DAY_TO_MINUTE);
  symret(SQL_C_INTERVAL_DAY_TO_SECOND);
  symret(SQL_C_INTERVAL_HOUR_TO_MINUTE);
  symret(SQL_C_INTERVAL_HOUR_TO_SECOND);
  symret(SQL_C_INTERVAL_MINUTE_TO_SECOND);
  symret(SQL_C_GUID);
/* Codes used for FetchOrientation in SQLFetchScroll(),
   and in SQLDataSources() */
  symret(SQL_FETCH_NEXT);
  symret(SQL_FETCH_FIRST);
/* Handle types */
  symret(SQL_HANDLE_ENV);
  symret(SQL_HANDLE_DBC);
  symret(SQL_HANDLE_STMT);
  symret(SQL_HANDLE_DESC);
/* Handle */
  symret(SQL_NULL_HANDLE);
/* Environment attributes */
  symret(SQL_ATTR_ODBC_VERSION);
  symret(SQL_ATTR_CONNECTION_POOLING);
  symret(SQL_ATTR_CP_MATCH);
/* ODBC Versions */
  symret(SQL_OV_ODBC2);
  symret(SQL_OV_ODBC3);
/* Connection attributes */
  symret(SQL_ACCESS_MODE);
  symret(SQL_AUTOCOMMIT);
  symret(SQL_ATTR_CONNECTION_TIMEOUT);
  symret(SQL_ATTR_AUTOCOMMIT);
  symret(SQL_AUTOCOMMIT_ON);
  symret(SQL_AUTOCOMMIT_OFF);
  symret(SQL_ATTR_LOGIN_TIMEOUT);
  symret(SQL_ATTR_TRACEFILE); /* [PM] 4.2.2 Beware, special usage. */
  symret(SQL_OPT_TRACE);
  symret(SQL_OPT_TRACEFILE);
  symret(SQL_TRANSLATE_DLL);
  symret(SQL_TRANSLATE_OPTION);
  symret(SQL_TXN_ISOLATION);
  symret(SQL_CURRENT_QUALIFIER);
  symret(SQL_ODBC_CURSORS);
  symret(SQL_QUIET_MODE);
  symret(SQL_PACKET_SIZE);
/* SQL driver completion flag */
  symret(SQL_DRIVER_NOPROMPT);
  symret(SQL_DRIVER_COMPLETE);
  symret(SQL_DRIVER_PROMPT);
  symret(SQL_DRIVER_COMPLETE_REQUIRED);
/* Parameter types */
  symret(SQL_PARAM_TYPE_UNKNOWN);
  symret(SQL_PARAM_INPUT);
  symret(SQL_PARAM_INPUT_OUTPUT);
  symret(SQL_RESULT_COL);
  symret(SQL_PARAM_OUTPUT);
  symret(SQL_RETURN_VALUE);
/* Options for SQLFreeStmt */
  symret(SQL_CLOSE);
  symret(SQL_UNBIND);
  symret(SQL_RESET_PARAMS);
/* NULL value */
  symret(SQL_NULL_DATA);
#if 0
/* For null terminated strings */
  symret(SQL_NTS);
#endif  /* 0 */
/* Information requested by SQLGetInfo() */
  symret(SQL_MAX_COLUMN_NAME_LEN);
  symret(SQL_TXN_CAPABLE);
  symret(SQL_TC_NONE);
  symret(SQL_TC_DML);
  symret(SQL_TC_DDL_COMMIT);
  symret(SQL_TC_DDL_IGNORE);
  symret(SQL_TC_ALL);

  /* FieldIdentifiers for SQLColAttribute() */
  symret(SQL_DESC_BASE_COLUMN_NAME);
/*  symret(SQL_DESC_TYPE); */   /* Does not exist in unixODBC. ([PM] 4.2 huh? It does exist everywhere I looked.)*/
  symret(SQL_DESC_OCTET_LENGTH);
  /* SQLColumns() */
  symret(SQL_NULLABLE);
  return PROLOG_NATIVE_CODE_ERROR;
}

/*
Lookup the symbolic names of the SQL return codes.
return 0 if unknown code. return 1 otherwise.
*/
SP_integer SPCDECL sql_return(SPAPI_ARG_PROTO_DECL SP_term_ref code_ref, SP_atom * atom)
{
  SP_integer code = ODBC_ERROR_IMPOSSIBLE_ERROR;
  /* [PM] 4.2.2 We should migrate towards returning symbolic (atom) codes from C to Prolog. */
  if (SP_get_atom(code_ref, atom)) 
    {
      /* It was already symbolic so return it as is. */
      return 1;
    }
  if (!SP_get_integer(code_ref, &code))
    {
      /* Neither atom nor integer. Barf */
      SP_ASSERT(SPIO_FALSE);
      *atom = state.invalid_return_code;
      return 0;
    }

  switch (code) {
  case SQL_SUCCESS:
    *atom = state.sql_success;
    return 1;
  case SQL_SUCCESS_WITH_INFO:
    *atom = state.sql_success_with_info;
    return 1;
  case SQL_NO_DATA:
    *atom = state.sql_no_data;
    return 1;
  case SQL_ERROR:
    *atom = state.sql_error;
    return 1;
  case SQL_INVALID_HANDLE:
    *atom = state.sql_invalid_handle;
    return 1;
  case ODBC_ERROR_OUT_OF_MEMORY:
    *atom = state.odbc_error_out_of_memory;
    return 1;
  case ODBC_ERROR_CREATING_HANDLE:
    *atom = state.odbc_error_creating_handle;
    return 1;
  case ODBC_ERROR_INVALID_HANDLE:
    *atom = state.odbc_error_invalid_handle;
    return 1;
  case ODBC_ERROR_INVALID_PARAMETER:
    *atom = state.odbc_error_invalid_parameter;
    return 1;

  case ODBC_ERROR_DATA_CONVERSION: /* like PROLOG_RESULT_DATA_CONVERSION_FAILED */
    *atom = state.odbc_error_type_error;
    return 1;
  case ODBC_ERROR_UNSUPPORTED_DATA_TYPE: /* like PROLOG_RESULT_UNSUPPORTED_DATA_TYPE */
    *atom = state.odbc_error_unsupported_data_type;
    return 1;
  case ODBC_ERROR_UNKNOWN_DATA_TYPE: /* like PROLOG_RESULT_UNKNOWN_DATATYPE */
    *atom = state.odbc_error_unknown_data_type;
    return 1;
  case ODBC_ERROR_TYPE_ERROR:   /* like PROLOG_RESULT_TYPE_ERROR */
    *atom = state.odbc_error_type_error;
    return 1;
  case ODBC_ERROR_IMPOSSIBLE_ERROR:   /* like PROLOG_RESULT_IMPOSSIBLE_ERROR */
    *atom = state.odbc_error_impossible_error;
    return 1;

  default:
    SP_SOFT_ASSERT(SPIO_FALSE);
    *atom = state.invalid_return_code;
    return 0;
  }
}


/****************************************/


#define define_atom_(var,name) do {             \
  state.var = SP_atom_from_string(name);        \
  SP_register_atom(state.var) ;                 \
 } while(0)

#define undefine_atom_(var) do {                \
  SP_unregister_atom(state.var) ;               \
  state.var = 0;                                \
 } while(0)

#if SP_ASSERTIONS
#define define_atom(var,name) do{               \
    define_atom_(var,name);                     \
    state.atom_counter++;                       \
} while(0)
#define undefine_atom(var) do{                  \
    SP_ASSERT(state.atom_counter > 0);          \
    state.atom_counter--;                       \
    undefine_atom_(var);                        \
} while(0)
#else  /* !SP_ASSERTIONS */
#define define_atom(var,name) define_atom_(var,name)
#define undefine_atom(var) undefine_atom_(var)
#endif  /* !SP_ASSERTIONS */

/****************************************
Init and deinit
****************************************/

void SPCDECL
odbc_init(SPAPI_ARG_PROTO_DECL int when)
{
  (void)when;                   /* avoid -Wunused */

  /* Set up the bookkeeping data structures here (for keeping track of
     odbc_malloc'd stuff). */
  /* Also initialize anything that needs initializing. */

#if ODBC_DEBUG
  state.alloc_counter = 0;      /* This should be back to 0 when odbc_deinit() runs. */
#endif
  state.null_StrLen_or_Ind = SQL_NULL_DATA;
#if SP_ASSERTIONS
  state.atom_counter = 0;
#endif  /* SP_ASSERTIONS */

  /* Register atoms for use later. */
#define do_atom define_atom
  DO_ATOMS;
#undef do_atom

 /* Set up statement handle entries */
  {
    state.stmthandle_stacksize =  STATEMENTHANDLEINITSTACKSIZE;
    state.statementhandlestack = odbc_malloc(STATEMENTHANDLEINITSTACKSIZE * sizeof(state.statementhandlestack[0]));
    state.statementhandle_tos = 0;
  }
  
  /* Set up environment handle entries */
  {
    state.environmenthandle_stacksize =  ENVIRONMENTHANDLEINITSTACKSIZE;
    state.environmenthandlestack = odbc_malloc(ENVIRONMENTHANDLEINITSTACKSIZE * sizeof(state.environmenthandlestack[0]));
    state.environmenthandle_tos = 0;
  }

  /* Set up connection handle entries */
  {
    state.connectionhandle_stacksize =  CONNECTIONHANDLEINITSTACKSIZE;
    state.connectionhandlestack = odbc_malloc(CONNECTIONHANDLEINITSTACKSIZE * sizeof(state.connectionhandlestack[0]));
    state.connectionhandle_tos = 0;
  }

  /* Set up backing store */
  {
    state.server_name_ienc = NULL;
    state.description_ienc = NULL;
    state.OutConnectionStringBuffer_ienc = NULL;
    state.MessageText_ienc = NULL;
    state.Sqlstate_ienc = NULL;
    state.column_name_ienc = NULL;
    state.term_as_string_store = NULL;
  }

#if ODBC_DEBUG
  {
#define DUMP_SYM(S) DEBUG_PRINT((stderr, "%s: %d\n",  #S, (int) S))
    /*
        SQL_C_BINARY: -2
        SQL_C_BIT: -7
        SQL_C_BOOKMARK: -18
        SQL_C_CHAR: 1
        SQL_C_DATE: 9
        SQL_C_DEFAULT: 99
        SQL_C_DOUBLE: 8
        SQL_C_FLOAT: 7
        SQL_C_GUID: -11
        SQL_C_INTERVAL_DAY: 103
        SQL_C_INTERVAL_DAY_TO_HOUR: 108
        SQL_C_INTERVAL_DAY_TO_MINUTE: 109
        SQL_C_INTERVAL_DAY_TO_SECOND: 110
        SQL_C_INTERVAL_HOUR: 104
        SQL_C_INTERVAL_HOUR_TO_MINUTE: 111
        SQL_C_INTERVAL_HOUR_TO_SECOND: 112
        SQL_C_INTERVAL_MINUTE: 105
        SQL_C_INTERVAL_MINUTE_TO_SECOND: 113
        SQL_C_INTERVAL_MONTH: 102
        SQL_C_INTERVAL_SECOND: 106
        SQL_C_INTERVAL_YEAR: 101
        SQL_C_INTERVAL_YEAR_TO_MONTH: 107
        SQL_C_LONG: 4
        SQL_C_NUMERIC: 2
        SQL_C_SBIGINT: -25
        SQL_C_SHORT: 5
        SQL_C_SLONG: -16
        SQL_C_SSHORT: -15
        SQL_C_STINYINT: -26
        SQL_C_TIME: 10
        SQL_C_TIMESTAMP: 11
        SQL_C_TINYINT: -6
        SQL_C_TYPE_DATE: 91
        SQL_C_TYPE_TIME: 92
        SQL_C_TYPE_TIMESTAMP: 93
        SQL_C_UBIGINT: -27
        SQL_C_ULONG: -18
        SQL_C_USHORT: -17
        SQL_C_UTINYINT: -28
        SQL_C_VARBOOKMARK: -2
        SQL_C_WCHAR: -8
    */
    DUMP_SYM(SQL_C_BINARY);
    DUMP_SYM(SQL_C_BIT);
    DUMP_SYM(SQL_C_BOOKMARK);
    DUMP_SYM(SQL_C_CHAR);
    DUMP_SYM(SQL_C_DATE);
    DUMP_SYM(SQL_C_DEFAULT);
    DUMP_SYM(SQL_C_DOUBLE);
    DUMP_SYM(SQL_C_FLOAT);
    DUMP_SYM(SQL_C_GUID);
    DUMP_SYM(SQL_C_INTERVAL_DAY);
    DUMP_SYM(SQL_C_INTERVAL_DAY_TO_HOUR);
    DUMP_SYM(SQL_C_INTERVAL_DAY_TO_MINUTE);
    DUMP_SYM(SQL_C_INTERVAL_DAY_TO_SECOND);
    DUMP_SYM(SQL_C_INTERVAL_HOUR);
    DUMP_SYM(SQL_C_INTERVAL_HOUR_TO_MINUTE);
    DUMP_SYM(SQL_C_INTERVAL_HOUR_TO_SECOND);
    DUMP_SYM(SQL_C_INTERVAL_MINUTE);
    DUMP_SYM(SQL_C_INTERVAL_MINUTE_TO_SECOND);
    DUMP_SYM(SQL_C_INTERVAL_MONTH);
    DUMP_SYM(SQL_C_INTERVAL_SECOND);
    DUMP_SYM(SQL_C_INTERVAL_YEAR);
    DUMP_SYM(SQL_C_INTERVAL_YEAR_TO_MONTH);
    DUMP_SYM(SQL_C_LONG);
    DUMP_SYM(SQL_C_NUMERIC);
    DUMP_SYM(SQL_C_SBIGINT);
    DUMP_SYM(SQL_C_SHORT);
    DUMP_SYM(SQL_C_SLONG);
    DUMP_SYM(SQL_C_SSHORT);
    DUMP_SYM(SQL_C_STINYINT);
    DUMP_SYM(SQL_C_TIME);
    DUMP_SYM(SQL_C_TIMESTAMP);
    DUMP_SYM(SQL_C_TINYINT);
    DUMP_SYM(SQL_C_TYPE_DATE);
    DUMP_SYM(SQL_C_TYPE_TIME);
    DUMP_SYM(SQL_C_TYPE_TIMESTAMP);
    DUMP_SYM(SQL_C_UBIGINT);
    DUMP_SYM(SQL_C_ULONG);
    DUMP_SYM(SQL_C_USHORT);
    DUMP_SYM(SQL_C_UTINYINT);
    DUMP_SYM(SQL_C_VARBOOKMARK);
    DUMP_SYM(SQL_C_WCHAR);


    /*
        SQL_CHAR: 1
        SQL_DECIMAL: 3
        SQL_DOUBLE: 8
        SQL_FLOAT: 6
        SQL_GUID: -11
        SQL_INTEGER: 4
        SQL_INTERVAL_DAY: 103
        SQL_INTERVAL_DAY_TO_HOUR: 108
        SQL_INTERVAL_DAY_TO_MINUTE: 109
        SQL_INTERVAL_DAY_TO_SECOND: 110
        SQL_INTERVAL_HOUR: 104
        SQL_INTERVAL_HOUR_TO_MINUTE: 111
        SQL_INTERVAL_HOUR_TO_SECOND: 112
        SQL_INTERVAL_MINUTE: 105
        SQL_INTERVAL_MINUTE_TO_SECOND: 113
        SQL_INTERVAL_MONTH: 102
        SQL_INTERVAL_SECOND: 106
        SQL_INTERVAL_YEAR: 101
        SQL_INTERVAL_YEAR_TO_MONTH: 107
        SQL_LONGVARBINARY: -4
        SQL_LONGVARCHAR: -1
        SQL_NUMERIC: 2
        SQL_REAL: 7
        SQL_SMALLINT: 5
        SQL_TINYINT: -6
        SQL_TYPE_DATE: 91
        SQL_TYPE_TIME: 92
        SQL_TYPE_TIMESTAMP: 93
        SQL_VARBINARY: -3
        SQL_VARCHAR: 12
        SQL_WCHAR: -8
        SQL_WLONGVARCHAR: -10
        SQL_WVARCHAR: -9
    */

    DUMP_SYM(SQL_BIGINT);
    DUMP_SYM(SQL_BINARY);
    DUMP_SYM(SQL_BIT);
    DUMP_SYM(SQL_CHAR);
    DUMP_SYM(SQL_DECIMAL);
    DUMP_SYM(SQL_DOUBLE);
    DUMP_SYM(SQL_FLOAT);
    DUMP_SYM(SQL_GUID);
    DUMP_SYM(SQL_INTEGER);
    DUMP_SYM(SQL_INTERVAL_DAY);
    DUMP_SYM(SQL_INTERVAL_DAY_TO_HOUR);
    DUMP_SYM(SQL_INTERVAL_DAY_TO_MINUTE);
    DUMP_SYM(SQL_INTERVAL_DAY_TO_SECOND);
    DUMP_SYM(SQL_INTERVAL_HOUR);
    DUMP_SYM(SQL_INTERVAL_HOUR_TO_MINUTE);
    DUMP_SYM(SQL_INTERVAL_HOUR_TO_SECOND);
    DUMP_SYM(SQL_INTERVAL_MINUTE);
    DUMP_SYM(SQL_INTERVAL_MINUTE_TO_SECOND);
    DUMP_SYM(SQL_INTERVAL_MONTH);
    DUMP_SYM(SQL_INTERVAL_SECOND);
    DUMP_SYM(SQL_INTERVAL_YEAR);
    DUMP_SYM(SQL_INTERVAL_YEAR_TO_MONTH);
    DUMP_SYM(SQL_LONGVARBINARY);
    DUMP_SYM(SQL_LONGVARCHAR);
    DUMP_SYM(SQL_NUMERIC);
    DUMP_SYM(SQL_REAL);
    DUMP_SYM(SQL_SMALLINT);
    DUMP_SYM(SQL_TINYINT);
    DUMP_SYM(SQL_TYPE_DATE);
    DUMP_SYM(SQL_TYPE_TIME);
    DUMP_SYM(SQL_TYPE_TIMESTAMP);
#if defined SQL_TYPE_UTCDATETIME
    DUMP_SYM(SQL_TYPE_UTCDATETIME);
#endif  /* defined SQL_TYPE_UTCDATETIME */
#if defined SQL_TYPE_UTCTIME
    DUMP_SYM(SQL_TYPE_UTCTIME);
#endif  /* SQL_TYPE_UTCTIME */
    DUMP_SYM(SQL_VARBINARY);
    DUMP_SYM(SQL_VARCHAR);
    DUMP_SYM(SQL_WCHAR);
    DUMP_SYM(SQL_WLONGVARCHAR);
    DUMP_SYM(SQL_WVARCHAR);


  }
#endif  /* ODBC_DEBUG */
}




/* odbc_free the buffers that have been odbc_malloc'd, and unregister the
   buffer addresses.   
 */
void SPCDECL
odbc_deinit(SPAPI_ARG_PROTO_DECL int when)
{
  size_t i;
  size_t const expected_allocations = 3; /* one for each handle-stack, one for each backing store */
  (void)when;
  (void) expected_allocations;

  DEBUG_PRINT((stderr, "\nODBC_DBG: odbc_deinit(%d)\n", (int)when));

  /* Free backing store (to get alloc_counter into expected state). */
  {
    FREE_STATIC_IENC(state.server_name_ienc);
    FREE_STATIC_IENC(state.description_ienc);
    FREE_STATIC_IENC(state.OutConnectionStringBuffer_ienc);
    FREE_STATIC_IENC(state.MessageText_ienc);
    FREE_STATIC_IENC(state.Sqlstate_ienc);
    FREE_STATIC_IENC(state.column_name_ienc);
    odbc_free(state.term_as_string_store);
    state.term_as_string_store = NULL;
  }

  DEBUG_PRINT((stderr, "\nODBC_DBG: stmthandle_stacksize==%" SPRIdSZ ", statementhandle_tos==%" SPRIdSZ "\n", (size_t)state.stmthandle_stacksize, (size_t)state.statementhandle_tos));
  DEBUG_PRINT((stderr, "\nODBC_DBG: alloc_counter==%" SPRIdINTEGER " %s\n", (SP_integer)state.alloc_counter, ((state.alloc_counter == expected_allocations) ? "(OK)" : "WARNING: some allocations unfreed")));

  /* Free statement handles (before connections) */
  {
    for (i = 1; i <= state.statementhandle_tos; i++)
      {
        struct statementhandle * const sh = &(state.statementhandlestack[i]);
#if ODBC_DEBUG > 1
        DEBUG_PRINT((stderr, "ODBC_DBG: sh==%p\n", sh));
        DEBUG_PRINT((stderr, "ODBC_DBG: index %" SPRIdSZ ", real_handle==%p, connection_handle_index==%" SPRIdINTEGER ", buffers=%p\n", i, (void*)sh->real_handle, (SP_integer)sh->connection_handle_index, (void*)sh->buffers));
        DEBUG_PRINT_HANDLE_STACKS;
#endif  /* ODBC_DEBUG */
        {
          SQLHSTMT StatementHandle = sh->real_handle;
          if (StatementHandle != SQL_NULL_HSTMT)
            {
              free_statementhandle(sh);
            }
          else
            {
              SP_ASSERT(sh->buffers == NULL);
            }
        }
      }
    odbc_free(state.statementhandlestack);
    state.statementhandlestack = NULL;
    state.statementhandle_tos = 0; /* [PM] 4.2 important for connection cleanup */
  }

  /* Free connection handles (after statements, before environments) */
  {
    for (i = 1; i <= state.connectionhandle_tos; i++)
      {
        struct connectionhandle * const entry = &(state.connectionhandlestack[i]);
#if ODBC_DEBUG > 1
        DEBUG_PRINT((stderr, "ODBC_DBG: connectionhandle entry==%p\n", entry));
        DEBUG_PRINT((stderr, "ODBC_DBG: index %" SPRIdSZ ", real_handle==%p\n", i, (void*)entry->real_handle));
        DEBUG_PRINT_HANDLE_STACKS;
#endif  /* ODBC_DEBUG */
        {
          SQLHDBC ConnectionHandle = entry->real_handle;
          if (ConnectionHandle != SQL_NULL_HDBC)
            {
              free_connectionhandle(entry);
            }
        }
      }
    odbc_free(state.connectionhandlestack);
    state.connectionhandlestack = NULL;
    state.connectionhandle_tos = 0; /* [PM] 4.2 important for environment cleanup */
  }

  /* Free environment handles (after connections) */
  {
    for (i = 1; i <= state.environmenthandle_tos; i++)
      {
        struct environmenthandle * const entry = &(state.environmenthandlestack[i]);
#if ODBC_DEBUG > 1
        DEBUG_PRINT((stderr, "ODBC_DBG: environmenthandle entry==%p\n", entry));
        DEBUG_PRINT((stderr, "ODBC_DBG: index %" SPRIdSZ ", real_handle==%p\n", i, (void*)entry->real_handle));
        DEBUG_PRINT_HANDLE_STACKS;
#endif  /* ODBC_DEBUG */
        {
          SQLHENV EnvironmentHandle = entry->real_handle;
          if (EnvironmentHandle != SQL_NULL_HENV)
            {
              free_environmenthandle(entry);
            }
        }
      }
    odbc_free(state.environmenthandlestack);
    state.environmenthandlestack = NULL;
    state.environmenthandle_tos = 0;
  }

#if ODBC_DEBUG
  if (state.alloc_counter != 0)
    {
      DEBUG_PRINT((stderr, "\nODBC_DBG: ERROR: alloc_counter==%" SPRIdINTEGER "\n", (SP_integer)state.alloc_counter));
      SP_ASSERT(0);
    }
#endif  /* ODBC_DEBUG */

  /* Unregister atoms that were registered by the init function. */
#define do_atom(var,name) undefine_atom(var)
  DO_ATOMS;
#undef do_atom

  SP_ASSERT(state.atom_counter == 0);

  return;
}

#if SP_SQL_UNICODE || ENABLE_WCHAR
#if SP_WIN32
/* Returns SQL_SUCCESS on success, otherwise an error code. The returned string must be freed with odbc_free(). */
static SQLRETURN to_sqlwchars(char const *s, SQLWCHAR **pWStr)
{
  SQLRETURN rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
  int cchWideChar, cchWideChar2;
  UINT const CodePage = CP_UTF8;
  DWORD const dwFlags = MB_ERR_INVALID_CHARS;
  LPCSTR const lpMultiByteStr = s;
  LPWSTR lpWideCharStr = NULL; /* needs cleanup */
  
  /* Find the length */
  cchWideChar = 
    MultiByteToWideChar(
                        CodePage, // UINT CodePage,     // code page
                        dwFlags, // DWORD dwFlags,         // character-type options
                        lpMultiByteStr,       // LPCSTR lpMultiByteStr, // string to map
                        -1,      // int cbMultiByte,       // number of bytes in string
                        NULL,    // LPWSTR lpWideCharStr,  // wide-character buffer
                        0        // int cchWideChar        // size of buffer
                        );
  if (! (cchWideChar > 0) ) {
    // failed
    rc = ODBC_ERROR_DATA_CONVERSION;
    goto barf;
  }
  SP_ASSERT(cchWideChar >= 1);

  ODBC_NULL_CHECK(lpWideCharStr = (LPWSTR) odbc_malloc(cchWideChar*(sizeof lpWideCharStr[0])));

  cchWideChar2 =
    MultiByteToWideChar(
                        CodePage,       // code page
                        dwFlags,        // character-type options
                        lpMultiByteStr, // string to map
                        -1, // int cbMultiByte,       // number of bytes in string
                        lpWideCharStr, // wide-character buffer
                        cchWideChar
                        );
  if (cchWideChar2 == 0)
    {
      DWORD gle = GetLastError(); /* for debugging */
      ODBC_BARF(ODBC_ERROR_OUT_OF_MEMORY);
    }
  // should always be equal unless cchWideChar2 is 0 due to out of memory
  if (cchWideChar2 != cchWideChar)
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
      goto barf;
    }
  if (pWStr != NULL)
    {
      *pWStr = lpWideCharStr;
      lpWideCharStr = NULL; /* protect from cleanup */
    }
  rc = SQL_SUCCESS;
 cleanup:
  if (lpWideCharStr != NULL)
    {
      odbc_free(lpWideCharStr);
      lpWideCharStr = NULL;
    }
  return rc;
 barf:
  SP_ASSERT(rc != SQL_SUCCESS);
  goto cleanup;
}
#else /* !SP_WIN32 */



/*
  Similar to Win32 MultiByteToWideChar()
  
  *pdst_count_used will tell how many dst items would be needed. This may be more than dst_count_.
  To determine the needed buffer size, call with dst_count_ == 0 (and dst_ == NULL).
 */
static spio_t_error_code multi_bytes_to_codes(spio_t_encoding * const encoding,
                                              spio_t_byte const * const src_,
                                              size_t const src_count,
                                              spio_t_wchar * const dst_,
                                              size_t const dst_count_,
                                              size_t * const pdst_count_used,
                                              spio_t_bits const options)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  typedef spio_t_byte src_type;
  typedef spio_t_wchar dst_type;

  int encoding_opened = SPIO_FALSE;
  spio_t_encoding_state *encoding_state = NULL; /* Needs cleanup of encoding_opened is true */
  dst_type dst_dummy[100];
  size_t const dst_dummy_count = (sizeof dst_dummy)/(sizeof dst_dummy[0]);
  spio_t_bits encode_options = SPIO_OPTION_NONE;
  size_t src_size_read = 0;
  size_t dst_size_wrote = 0;
  COMPILE_TIME_ASSERT((sizeof dst_[0]) == (sizeof dst_dummy[0]));

  (void)options;
  
  if (dst_ == NULL && dst_count_ != 0)
    {
      BARF(SPIO_E_PARAMETER_ERROR);
    }


  if (src_count == 0)
    {
      dst_size_wrote = 0;
    }
  else
    {
      size_t src_offset = 0;
      size_t dst_offset = 0;
      
      /* Decode from multi-byte */
      CHECK(SP_encoding_open(encoding, &encoding_state, NULL, SP_ENCODING_OPEN_OPTION_DECODE));
      encoding_opened = SPIO_TRUE;

      /* We pass all indata */
      SPIO_SET_MASK(encode_options, SP_ENCODE_CODES_OPTION_END);

      /* Until SPIO_S_DEALLOCATED */
      while (SPIO_TRUE)
        {
          src_type const * const src = src_ + src_offset;
          size_t const src_remaining_count = (src_count-src_offset);
          dst_type * const dst = (dst_offset < dst_count_ ? dst_+dst_offset : &dst_dummy[0]);
          size_t const dst_remaining_count = (dst_offset < dst_count_ ? dst_count_ - dst_offset : dst_dummy_count);
          size_t src_size_read_delta = 0;
          size_t dst_size_wrote_delta = 0;

          code = SP_encode_to_codes(encoding,
                                    src,
                                    src_remaining_count*(sizeof src[0]),
                                    &encoding_state,
                                    dst,
                                    dst_remaining_count*(sizeof dst[0]),
                                    &src_size_read_delta,
                                    &dst_size_wrote_delta,
                                    encode_options);
          CHECK(code);
          src_size_read += src_size_read_delta;
          dst_size_wrote += dst_size_wrote_delta;
          
          if (code == SPIO_S_DEALLOCATED)
            {
              encoding_opened = SPIO_FALSE;
              SP_ASSERT(src_size_read_delta == 0 && dst_size_wrote_delta == 0);
              break;
            }
          /* Even multiple of item size */
          SP_ASSERT((src_size_read_delta/(sizeof src[0]))*(sizeof src[0]) == src_size_read_delta);
          SP_ASSERT((dst_size_wrote_delta/(sizeof dst[0]))*(sizeof dst[0]) == dst_size_wrote_delta);
          src_offset += (src_size_read_delta/(sizeof src[0]));
          dst_offset += (dst_size_wrote_delta/(sizeof dst[0]));
        }
    }
  /* Here with dst_size_wrote */

  {
    size_t dst_count_used = dst_size_wrote/(sizeof dst_[0]);
    /* A multiple of dst item size */
    SP_ASSERT(dst_count_used * (sizeof dst_[0]) == dst_size_wrote);
    if (pdst_count_used != NULL)
      {
        *pdst_count_used = dst_count_used;
      }
    if (dst_count_used > dst_count_)
      {
        code = SPIO_S_TRUE;
      }
    else
      {
        COMPILE_TIME_ASSERT(SPIO_S_TRUE != SPIO_S_NOERR);
        code = SPIO_S_NOERR;
      }
  }
    SP_ASSERT(SPIO_SUCCEEDED(code));

 cleanup:

  if (encoding_opened)
    {
      (void) SP_encode_to_codes(encoding,
                                  NULL, 0,
                                  &encoding_state,
                                  NULL, 0,
                                  NULL, NULL,
                                  SP_ENCODE_CODES_OPTION_ABORT);
      encoding_opened = SPIO_FALSE;
      encoding_state = NULL;
    }

  return code;
 barf:
  goto cleanup;
}

/*
  The returned utf32-string always has a final NUL char which is not
  included in the returned length.
*/
static spio_t_error_code multi_bytes_to_utf32_helper(char const *encoding_name, spio_t_byte const * const src, size_t const src_size, spio_t_wchar ** const putf32, size_t *putf32_len)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  spio_t_encoding *encoding = NULL; /* Needs cleanup */
  size_t dst_count_needed = 0;
  spio_t_wchar *dst = NULL; /* Needs cleanup */
  size_t dst_size;
  size_t dst_count_used = 0;

  CHECK(SP_get_encoding(encoding_name, &encoding, SPIO_OPTION_NONE));

  {
    CHECK(multi_bytes_to_codes(encoding,
                               src, src_size,
                               NULL, 0,
                               &dst_count_needed,
                               SPIO_OPTION_NONE));

    dst_size = (dst_count_needed +1) * (sizeof dst[0]); /* +1 for NUL */
    NULL_CHECK(dst = odbc_malloc(dst_size));
  
    CHECK(multi_bytes_to_codes(encoding,
                               src, src_size,
                               dst, dst_count_needed,
                               &dst_count_used,
                               SPIO_OPTION_NONE));
    SP_ASSERT(dst_count_needed == dst_count_used);
    dst[dst_count_needed] = 0; /* NUL-terminate */
  }

  if (putf32 != NULL)
    {
      *putf32 = dst;
      dst = NULL; /* protect from cleanup */
    }
  if (putf32_len != NULL)
    {
      *putf32_len = dst_count_needed; /* does not include the extra NUL */
    }
  code = SPIO_S_NOERR;

 cleanup:
  if (dst != NULL)
    {
      odbc_free(dst);
      dst = NULL;
    }
  if (encoding != NULL)
    {
      SP_encoding_release(encoding);
      encoding = NULL;
    }
  return code;
 barf:
  goto cleanup;
}

#if SP_ODBC_SQLWCHAR_IS_UTF32
/* The returned SQLWCHAR string is always NUL-terminated. No optimization for all-ASCII input. */
static spio_t_error_code to_n_sqlwchars_helper(char const * const str, size_t const len, SQLWCHAR ** const pw_str)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  spio_t_byte const * const ienc = (spio_t_byte const *)str;
  size_t ienc_len = len;
  spio_t_wchar *utf32 = NULL; /* Needs cleanup */

  COMPILE_TIME_ASSERT((sizeof str[0]) == (sizeof ienc[0]));

  CHECK(multi_bytes_to_utf32_helper("UTF-8", ienc, ienc_len, &utf32, NULL)); /* Always adds an uncounted NUL */
  if (pw_str != NULL)
    {
      COMPILE_TIME_ASSERT((sizeof (*pw_str)[0]) == (sizeof utf32[0]));
      *pw_str = (SQLWCHAR*) utf32;
      utf32 = NULL; /* Protect from cleanup */
    }
  code = SPIO_S_NOERR;
 cleanup:
  if (utf32 != NULL)
    {
      odbc_free(utf32);
      utf32 = NULL;
    }
  return code;
 barf:
  goto cleanup;
}
#elif SP_ODBC_SQLWCHAR_IS_UTF16

/* Does not add extra NUL (callers have been updated) */
static spio_t_error_code utf32_to_n_utf16_helper(spio_t_wchar *utf32, size_t utf32_len, void **putf16, size_t *putf16_len)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  void *utf16 = NULL; /* Needs cleanup */
  size_t utf16_size = 0;
  size_t utf16_len;
  /* does not add extra NUL */
  CHECK(codes_to_multi_bytes_helper(NATIVE_UTF16_NAME, utf32, utf32_len, &utf16, &utf16_size));
  utf16_len = utf16_size/2;
  SP_ASSERT(utf16_len*2 == utf16_size); /* even multiple */

#if ODBC_DEBUG_VALGRIND
  {
    int i;
    for (i = 0; i < utf16_len; i++) {
      /* This is just to tell valgrind that we expect all entries to be initialized. */
      if (((short*)utf16)[i] == 0x2345) {
	break;
      }
    }
  }
#endif	/* ODBC_DEBUG */

  if (putf16 != NULL)
    {
      *putf16 = utf16;
      utf16 = NULL; /* Protect from cleanup */
    }
  if (putf16_len != NULL)
    {
      *putf16_len = utf16_len;
    }
  code = SPIO_S_NOERR;

 cleanup:
  if (utf16 != NULL)
    {
      odbc_free(utf16);
      utf16 = NULL;
    }
  return code;
 barf:
  goto cleanup;

}


/* The returned SQLWCHAR string is always NUL-terminated. No optimization for all-ASCII input. */
static spio_t_error_code to_n_sqlwchars_helper(char const * const str, size_t const len, SQLWCHAR ** const pw_str)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  spio_t_byte const * const ienc = (spio_t_byte const *)str;
  size_t const ienc_len = len;
  size_t utf32_len = 0;
  spio_t_wchar *utf32 = NULL; /* Needs cleanup */
  void *utf16 = NULL; /* Needs cleanup */

  COMPILE_TIME_ASSERT((sizeof str[0]) == (sizeof ienc[0]));

  CHECK(multi_bytes_to_utf32_helper("UTF-8", ienc, ienc_len, &utf32, &utf32_len)); /* Always adds an uncounted NUL */
  CHECK(utf32_to_n_utf16_helper(utf32, utf32_len +1, &utf16, NULL)); /* +1 to include the NUL (does not add extra NUL) */

  if (pw_str != NULL)
    {
      COMPILE_TIME_ASSERT((sizeof (*pw_str)[0]) == 2);
      *pw_str = utf16;
      utf16 = NULL; /* Protect from cleanup */
    }
  code = SPIO_S_NOERR;

 cleanup:
  if (utf32 != NULL)
    {
      odbc_free(utf32);
      utf32 = NULL;
    }
  if (utf16 != NULL)
    {
      odbc_free(utf16);
      utf16 = NULL;
    }
  return code;
 barf:
  goto cleanup;
}
#endif	/* SP_ODBC_SQLWCHAR_IS_UTF16 */

static SQLRETURN to_sqlwchars(char const *str, SQLWCHAR **pw_str)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  SQLRETURN rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
  SQLWCHAR *w_str = NULL; /* Needs cleanup */
  size_t len = strlen(str);

  CHECK(to_n_sqlwchars_helper(str, len, &w_str)); /* Always adds a NUL, no need for len+1 */
  if (pw_str != NULL)
    {
      *pw_str = w_str;
      w_str = NULL; /* protect from cleanup */
    }
  rc = SQL_SUCCESS;
 cleanup:
  if (w_str != NULL)
    {
      odbc_free(w_str);
      w_str = NULL;
    }

  return rc;
 barf:
  SP_ASSERT(!SPIO_SUCCEEDED(code));
  switch (code)
    {
    case SPIO_E_OUT_OF_MEMORY:
      rc = ODBC_ERROR_OUT_OF_MEMORY;
      break;
    default:
      rc = ODBC_ERROR_DATA_CONVERSION;
      break;
    }
  goto cleanup;
}

#endif  /* !SP_WIN32 */
#endif /* SP_SQL_UNICODE || ENABLE_WCHAR */

#if SP_SQL_UNICODE || ENABLE_WCHAR
#if SP_WIN32

#if SP_SQL_UNICODE
/* Returns SQL_SUCCESS on success, otherwise an error code. The returned string must be freed with odbc_free(). */
static SQLRETURN from_sqlwchars(SQLWCHAR const *wStr, char **ps)
{
  SQLRETURN rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
  int cbMultiByte, cbMultiByte2; // Note: intentionally int, not size_t
  UINT const CodePage = CP_UTF8;
  LPCWSTR const lpWideCharStr = wStr;
  LPSTR lpMultiByteStr = NULL; /* Needs cleanup */

  // Find the length
  cbMultiByte = 
    WideCharToMultiByte(
                        CodePage, // UINT CodePage,            // code page
                        0, // DWORD dwFlags,            // performance and mapping flags
                        lpWideCharStr, // LPCWSTR lpWideCharStr,    // wide-character string
                        -1, // int cchWideChar,          // number of chars in string
                        NULL, // LPSTR lpMultiByteStr,     // buffer for new string
                        0, //int cbMultiByte,          // size of buffer
                        NULL, // LPCSTR lpDefaultChar,     // default for unmappable chars
                        NULL // LPBOOL lpUsedDefaultChar  // set when default char used
                        );

  if (! (cbMultiByte > 0) ) {
    // failed
    rc = ODBC_ERROR_DATA_CONVERSION;
    goto barf;
  }
  SP_ASSERT(cbMultiByte >= 1);

  ODBC_NULL_CHECK(lpMultiByteStr = (LPSTR) odbc_malloc(cbMultiByte));

  cbMultiByte2 =
    WideCharToMultiByte(
                        CodePage, // UINT CodePage,            // code page
                        0, // DWORD dwFlags,            // performance and mapping flags
                        lpWideCharStr, // wide-character string
                        -1, // int cchWideChar,          // number of chars in string
                        lpMultiByteStr, // buffer for new string
                        cbMultiByte,    // size of buffer
                        NULL, // LPCSTR lpDefaultChar,     // default for unmappable chars
                        NULL // LPBOOL lpUsedDefaultChar  // set when default char used
                        );
  if (cbMultiByte2 == 0)
    {
      DWORD gle = GetLastError(); /* for debugging */
      ODBC_BARF(ODBC_ERROR_OUT_OF_MEMORY);
    }
  if (cbMultiByte2 != cbMultiByte)      // cannot happen
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
      goto barf;
    }
  if (ps != NULL)
    {
      *ps = lpMultiByteStr;
      lpMultiByteStr = NULL; /* protect from cleanup */
    }
  rc = SQL_SUCCESS;
 cleanup:
  if (lpMultiByteStr != NULL)
    {
      odbc_free(lpMultiByteStr);
      lpMultiByteStr = NULL;
    }
  return rc;
 barf:
  SP_ASSERT(rc != SQL_SUCCESS);
  goto cleanup;
}
#endif  /* SP_SQL_UNICODE */

#if ENABLE_WCHAR
/* Does not NUL-terminate (callers has been updated) */
static SQLRETURN from_n_sqlwchars(SQLWCHAR const *wStr, size_t cchWideChar, char **ps, size_t *psize)
{
  SQLRETURN rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
  int cbMultiByte, cbMultiByte2; // Note: intentionally int, not size_t
  UINT const CodePage = CP_UTF8;
  LPCWSTR const lpWideCharStr = wStr;
  LPSTR lpMultiByteStr = NULL; /* Needs cleanup */
  if (cchWideChar == 0) {
    /* WideCharToMultiByte() does not accept cchWideChar == 0 */
    cbMultiByte = 0;
  } else {
    // Find the length
    cbMultiByte = 
      WideCharToMultiByte(
                          CodePage, // UINT CodePage,            // code page
                          0, // DWORD dwFlags,            // performance and mapping flags
                          lpWideCharStr, // LPCWSTR lpWideCharStr,    // wide-character string
                          (int) cchWideChar, // number of chars in string
                          NULL, // LPSTR lpMultiByteStr,     // buffer for new string
                          0, //int cbMultiByte,          // size of buffer
                          NULL, // LPCSTR lpDefaultChar,     // default for unmappable chars
                          NULL // LPBOOL lpUsedDefaultChar  // set when default char used
                          );

    if (! (cbMultiByte > 0) ) {
      // failed
      rc = ODBC_ERROR_DATA_CONVERSION;
      goto barf;
    }
    SP_ASSERT(cbMultiByte >= 1);
  }

  ODBC_NULL_CHECK(lpMultiByteStr = (LPSTR) odbc_malloc(cbMultiByte)); /* No extra NUL-termination. */

  if (cchWideChar == 0) {
    /* WideCharToMultiByte() does not accept cchWideChar == 0 */
    cbMultiByte2 = 0;
  } else {
    cbMultiByte2 =
      WideCharToMultiByte(
                          CodePage, // UINT CodePage,            // code page
                          0, // DWORD dwFlags,            // performance and mapping flags
                          lpWideCharStr, // wide-character string
                          (int) cchWideChar, // number of chars in string
                          lpMultiByteStr,       // buffer for new string
                          cbMultiByte,  // size of buffer
                          NULL, // LPCSTR lpDefaultChar,     // default for unmappable chars
                          NULL // LPBOOL lpUsedDefaultChar  // set when default char used
                          );
    if (cbMultiByte2 == 0)
      {
        DWORD gle = GetLastError(); /* for debugging */
        ODBC_BARF(ODBC_ERROR_OUT_OF_MEMORY);
      }
  }

  if (cbMultiByte2 != cbMultiByte)      // cannot happen
    {
      SP_ASSERT(0);
      rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
      goto barf;
    }

  if (ps != NULL)
    {
      *ps = lpMultiByteStr;
      lpMultiByteStr = NULL; /* protect from cleanup */
    }
  if (psize != NULL)
    {
      *psize = cbMultiByte;
    }
  rc = SQL_SUCCESS;
 cleanup:
  if (lpMultiByteStr != NULL)
    {
      odbc_free(lpMultiByteStr);
      lpMultiByteStr = NULL;
    }
  return rc;
 barf:
  SP_ASSERT(rc != SQL_SUCCESS);
  goto cleanup;
}
#endif /* ENABLE_WCHAR */

#else /* !SP_WIN32 */

#if SP_ODBC_SQLWCHAR_IS_UTF32
/* Does not NUL-terminate (callers has been updated). No optimization for all-ASCII input.
   
   The returned ps should be freed with odbc_free().
 */
static spio_t_error_code from_n_sqlwchars_helper(SQLWCHAR const *w_str, size_t const w_len, char **ps, size_t *psize)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  void *dst = NULL; /* Needs cleanup (SP_free()) */
  size_t dst_size = 0;

  COMPILE_TIME_ASSERT(sizeof(SQLWCHAR) == 4);

  /* Does not add extra NUL */
  COMPILE_TIME_ASSERT(sizeof w_str[0] == sizeof(spio_t_wchar));
  CHECK(SP_codes_to_multi_bytes("UTF-8", (spio_t_wchar const *)w_str, w_len, &dst, &dst_size));

  if (ps != NULL)
    {
      *ps = odbc_malloc_from_sp_malloc(dst, dst_size);
      dst = NULL; /* Protect from cleanup */
    }
  if (psize != NULL)
    {
      *psize = dst_size;
    }

  code = SPIO_S_NOERR;
 cleanup:
  if (dst != NULL)
    {
      SP_free(dst);
      dst = NULL;
    }
  return code;
 barf:
  goto cleanup;
}
#elif SP_ODBC_SQLWCHAR_IS_UTF16
/* Does not NUL-terminate (callers has been updated). No optimization
   for all-ASCII input.
   
   The returned ps should be freed with odbc_free().
*/
static spio_t_error_code from_n_sqlwchars_helper(SQLWCHAR const *w_str, size_t const w_len, char **ps, size_t *psize)
{
  spio_t_error_code code = SPIO_E_INTERNAL_ERROR;
  spio_t_wchar *utf32 = NULL; /* Needs cleanup (odbc_free()) */
  size_t utf32_len = 0;
  void *dst = NULL; /* Needs cleanup (SP_free()) */
  size_t dst_size = 0;

  COMPILE_TIME_ASSERT(sizeof(SQLWCHAR) == 2);
  /* This adds an extra, uncounted NUL */
  CHECK(multi_bytes_to_utf32_helper(NATIVE_UTF16_NAME, (spio_t_byte const *)w_str, w_len * (sizeof w_str[0]), &utf32, &utf32_len));
  /* Does not add an extra NUL (and we do not include the extra NUL added by multi_bytes_to_utf32_helper() */
  CHECK(SP_codes_to_multi_bytes("UTF-8", utf32, utf32_len, &dst, &dst_size));

  if (ps != NULL)
    {
      *ps = odbc_malloc_from_sp_malloc(dst, dst_size);
      dst = NULL; /* Protect from cleanup */
    }
  if (psize != NULL)
    {
      *psize = dst_size;
    }
  code = SPIO_S_NOERR;
 cleanup:
  if (utf32 != NULL)
    {
      odbc_free(utf32);
      utf32 = NULL;
    }
  if (dst != NULL)
    {
      SP_free(dst);
      dst = NULL;
    }
  return code;
 barf:
  goto cleanup;
}
#endif /* SP_ODBC_SQLWCHAR_IS_UTF16 */

#if SP_SQL_UNICODE
/* Returns SQL_SUCCESS on success, otherwise an error code. The returned string must be freed with odbc_free(). */
static SQLRETURN from_sqlwchars(SQLWCHAR const *w_str, char **ps)
{
  spio_t_error_code code = SPIO_E_IMPOSSIBLE_ERROR;
  SQLRETURN rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
  char *ienc = NULL; /* Needs cleanup (odbc_free()) */
  size_t w_len;
  int ascii_only;
  int latin1_only;


#if FROM_SQLWCHAR_ASCII_FAST_PATH
  {
    SQLWCHAR const *pw;
    SQLWCHAR bits = 0;

    for (pw = w_str; *pw != 0; pw++)
      {
	bits |= *pw;
      }
    w_len = pw-w_str; /* Len in SQLWCHAR, exluding NUL. */
    ascii_only = ((bits & ~(SQLWCHAR)0x3F) == 0);
    latin1_only = ((bits & ~(SQLWCHAR)0xFF) == 0);
  }
#else /* !FROM_SQLWCHAR_ASCII_FAST_PATH */
  {
    /* Unknown */
    ascii_only = SPIO_FALSE;
    latin1_only = SPIO_FALSE;
  }
#endif  /* !FROM_SQLWCHAR_ASCII_FAST_PATH */

#if !POSTPONE
#error "[PM] 4.2.2 optimize Latin 1 case too?"
#else  /* POSTPONE */
  (void)latin1_only;
#endif	/* POSTPONE */

  if (ascii_only)
    {
      size_t i;
      NULL_CHECK(ienc = odbc_malloc(w_len +1)); /* +1 for NUL */
      for (i = 0; i < w_len; i++)
        {
          ienc[i] = (char)w_str[i];
        }
      SP_ASSERT(i == w_len);
      ienc[i] = '\0';
    }
  else
    {
      /* not ASCII, slow path */
      /* +1 to include the NUL so the returned ienc is always NUL-terminated. */
      CHECK(from_n_sqlwchars_helper(w_str, w_len +1, &ienc, NULL)); /* Does not NUL-terminate */
    }
  if (ps != NULL)
    {
      *ps = ienc;
      ienc = NULL;         /* protect from cleanup */
    }
  rc = SQL_SUCCESS;
 cleanup:
  if (ienc != NULL)
    {
      odbc_free(ienc);
    }
  return rc;
 barf:
  SP_ASSERT(!SPIO_SUCCEEDED(code));
  switch (code)
    {
    case SPIO_E_OUT_OF_MEMORY:
      rc = ODBC_ERROR_OUT_OF_MEMORY;
      break;
    default:
      rc = ODBC_ERROR_DATA_CONVERSION;
      break;
    }
  goto cleanup;
}
#endif  /* SP_SQL_UNICODE */

#if ENABLE_WCHAR

/* Does not NUL-terminate (callers has been updated) */
static SQLRETURN from_n_sqlwchars(SQLWCHAR const *w_str, size_t const w_len, char **ps, size_t *psize)
{
  spio_t_error_code code = SPIO_E_IMPOSSIBLE_ERROR;
  SQLRETURN rc = ODBC_ERROR_IMPOSSIBLE_ERROR;
  char *ienc = NULL; /* Needs cleanup (odbc_free()) */
  size_t ienc_size = 0;
  int ascii_only;

#if FROM_SQLWCHAR_ASCII_FAST_PATH
  {
    SQLWCHAR bits = 0;
    {
      size_t i;
      for (i = 0; i < w_len; i++)
        {
	  bits |= w_str[i];
        }
    }
    ascii_only = ((bits & ~(SQLWCHAR)0x3F) == 0);
  }
#endif  /* FROM_SQLWCHAR_ASCII_FAST_PATH */

  if (ascii_only)
    {
      size_t i;
      NULL_CHECK(ienc = odbc_malloc(w_len));
      ienc_size = w_len;
      for (i = 0; i < w_len; i++)
        {
          ienc[i] = (char)w_str[i];
        }
      SP_ASSERT(i == ienc_size);
    }
  else
    {
      /* not ASCII, slow path */
      CHECK(from_n_sqlwchars_helper(w_str, w_len, &ienc, &ienc_size)); /* Does not NUL-terminate */
    }

  if (ps != NULL)
    {
      *ps = ienc;
      ienc = NULL;         /* protect from cleanup */
    }
  if (psize != NULL)
    {
      *psize = ienc_size;
    }
  rc = SQL_SUCCESS;
 cleanup:
  if (ienc != NULL)
    {
      odbc_free(ienc);
      ienc = NULL;
    }
  return rc;
 barf:
  SP_ASSERT(!SPIO_SUCCEEDED(code));
  switch (code)
    {
    case SPIO_E_OUT_OF_MEMORY:
      rc = ODBC_ERROR_OUT_OF_MEMORY;
      break;
    default:
      rc = ODBC_ERROR_DATA_CONVERSION;
      break;
    }
  goto cleanup;
}
#endif /* ENABLE_WCHAR */
#endif  /* !SP_WIN32 */
#endif  /* SP_SQL_UNICODE */

