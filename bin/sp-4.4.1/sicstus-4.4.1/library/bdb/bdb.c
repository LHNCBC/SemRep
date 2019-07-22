#define SICSTUS_HIDDEN_API 1
/* [PM] 3.9.1 including stdlib.h before string.h avoids 'warning:
   conflicting types for built-in function `memcmp'" on HP-UX 11 with
   gcc 2.95.2. I do not know why.
 */
#include <stdlib.h>

#include <errno.h>
#include <string.h>

#include <sicstus/sicstus.h>
#include <sicstus/config.h>     /* [PM] 4.2.1 this also defines SICSTUS_DBG */
#ifndef SP_BIGENDIAN
#error "[PM] 4.1.2 SP_BIGENDIAN should be zero or one here (from config.h)"
#endif  /* !SP_BIGENDIAN */
#include <sys/types.h>
#include <fcntl.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */

#include <stdlib.h>             /* [PM] 3.9.1 !! must be included
                                   before db.h to work around a bug in
                                   db.h (specifically the way db.h
                                   uses __BIT_TYPES_DEFINED__ is
                                   totally brain damaged.). Caused
                                   problem on Tru64 with
                                   _XOPEN_SOURCE_EXTENDED */

#include <db.h>

#include "bdb.h"

#include "bdb_glue.h"           /* [PM] 3.9 splfr-generated */

#ifndef BDB_DBTYPE
#define BDB_DBTYPE DB_HASH
#endif  /* BDB_DBTYPE */

#ifndef TERMSDB_DBTYPE
#define TERMSDB_DBTYPE BDB_DBTYPE
#endif  /* !TERMSDB_DBTYPE */
#ifndef ADMINDB_DBTYPE
#define ADMINDB_DBTYPE BDB_DBTYPE
#endif  /* !ADMINDB_DBTYPE */
#ifndef INDEXDB_DBTYPE
#define INDEXDB_DBTYPE BDB_DBTYPE
#endif  /* !INDEXDB_DBTYPE */

/*
Berkeley DB reserves all values from -30,800 to -30,999 to itself as possible error values
*/
#define BDB_PARAMETER_ERROR -20001

#ifndef WRONG_ENDIANNESS
#if !SP_BIGENDIAN
#define WRONG_ENDIANNESS 1
#else  /* !wrong endianness */
#define WRONG_ENDIANNESS 0
#endif  /* !SP_BIGENDIAN */
#endif  /* !WRONG_ENDIANNESS */

#if WRONG_ENDIANNESS
#define ADJUST_KEY_ENDIANNESS_(KEY) do {                        \
      u_int32_t key_ = (KEY);                                   \
      unsigned int byte0 = (key_>>(0*8)) & 0xFF; /* lsb */      \
      unsigned int byte1 = (key_>>(1*8)) & 0xFF;                \
      unsigned int byte2 = (key_>>(2*8)) & 0xFF;                \
      unsigned int byte3 = (key_>>(3*8)) & 0xFF; /* msb */      \
      u_int32_t bk;                                             \
      bk = byte0;                                               \
      bk<<=8;                                                   \
      bk |= byte1;                                              \
      bk<<=8;                                                   \
      bk |= byte2;                                              \
      bk<<=8;                                                   \
      bk |= byte3;                                              \
      (KEY) = bk;                                               \
   } while (0)
#else  /* !WRONG_ENDIANNESS */
#define ADJUST_KEY_ENDIANNESS_(KEY) do{ /* nothing */ }while(0)
#endif  /* !WRONG_ENDIANNESS */

/* [PM]] 4.2.2 xref db_term_hash() in hash.c */
#define IS_VALID_HASH_CODE(HC) ((((SP_uinteger)(HC)) & ((SP_uinteger)0xffffffff)) == ((SP_uinteger)(HC)))

#if USE_BIGENDIAN_KEYS
#define ADJUST_KEY_ENDIANNESS(KEY) ADJUST_KEY_ENDIANNESS_((KEY))
#else                                  /* !USE_BIGENDIAN_KEYS */
#define ADJUST_KEY_ENDIANNESS(KEY) do{ /* nothing */ }while(0)
#endif                                 /* !USE_BIGENDIAN_KEYS */

/* Let next_termref() return bigendian keys (so nothing else must change) */
#if GENERATE_BIGENDIAN_KEYS
#define ADJUST_GENERATED_KEY_ENDIANNESS(KEY) ADJUST_KEY_ENDIANNESS_((KEY))
#else                                  /* !GENERATE_BIGENDIAN_KEYS */
#define ADJUST_GENERATED_KEY_ENDIANNESS(KEY) do{ /* nothing */ }while(0)
#endif                                 /* !GENERATE_BIGENDIAN_KEYS */

/* [PM] 4.0 see support.c */
#if defined(__GNUC__) && defined(__GLIBC__) && defined(__GLIBC_MINOR__)
#if (__GLIBC__ == 2 && __GLIBC_MINOR__ == 2) /* glibc 2.2 has this problem (RH 7.2) */
#undef strcpy
#endif /* glibc 2.2 */
#endif /* gcc && glibc */

/* [PM] 4.1.2 Postpone since we do not use any of the APIs that
        requires malloc (e.g. DB_DBT_MALLOC, DB_DBT_REALLOC) (or at
        least, we never free() anything returned from BDB). However,
        calling set_alloc should not hurt so it is probably a good
        idea, eventually. */
#if !FORCE_BUILD && SICSTUS_RELEASE_BUILD && SICSTUS_VERSION > 40301 && 0 /* [PM] 4.3.2 postpone indefinitely */
/*
   <http://www.oracle.com/technology/documentation/berkeley-db/db/programmer_reference/general_am_conf.html>

   Berkeley DB allocates memory for returning key/data pairs and
   statistical information which becomes the responsibility of the
   application. There are also interfaces where an application will
   allocate memory which becomes the responsibility of Berkeley DB. 

   On systems in which there may be multiple library versions of the
   standard allocation routines (notably Windows NT), transferring
   memory between the library and the application will fail because
   the Berkeley DB library allocates memory from a different heap than
   the application uses to free it, or vice versa. To avoid this
   problem, the DB_ENV->set_alloc() and DB->set_alloc() methods can be
   used to give Berkeley DB references to the application's allocation
   routines.

 */
#error "[PM] 4.1.1+ need to use set_alloc on windows (and may as well on all OSes)"
#endif  /* SICSTUS_VERSION > 4.1.1 */

/*
From the Berkeley DB manual:

  Due to the constraints of the PA-RISC memory architecture, HP-UX does not
  allow a process to map a file into its address space multiple times.  For
  this reason, each Berkeley DB environment may be opened only once by a
  process on HP-UX, i.e., calls to DBENV->open() will fail if the specified
  Berkeley DB environment has been opened and not subsequently closed.

Currently, the Prolog module tries to ensure this.
*/

/*
  Windows/95 note:

  On Windows/95, files that are opened by multiple processes do not share
  data correctly. For this reason, the DB_SYSTEM_MEM flag is implied for
  any application that does not specify the DB_PRIVATE flag.
 */

/* [PD] 3.9.2 Upgrade for BDB 4.1 */
/* [PM] 3.11.2 Make BDB_4_1 true for versions >= 4.1 */
#ifndef BDB_4_1
#if (DB_VERSION_MAJOR == 4 && DB_VERSION_MINOR >= 1) || DB_VERSION_MAJOR > 4
#define BDB_4_1 1
#else
#define BDB_4_1 0
#endif
#endif

/* [PD] 4.0.5 This was inside open_env() which is rather late in the file.  */
#ifndef BDB_DEBUG
# if SICSTUS_DBG>0
#  define BDB_DEBUG SICSTUS_DBG
# endif
#endif


#if STAT
#if HAVE_SYS_TIME_H
#include <sys/times.h>
#endif /* HAVE_SYS_TIME_H */

struct stattable_ {
  char *name;
  unsigned count;
  struct tms time;
  struct tms stv;               /* [PM] 4.1.2 for nested timings */
  clock_t walltime;             /* [PM] 4.1.3 */
  clock_t stv_walltime;         /* [PM] 4.1.3 */
};

#define ENTRY(NAME) {NAME,             0, {0, 0, 0, 0}, {0, 0, 0, 0}, 0,0}
struct stattable_ stattable[] = {
#define OPEN_ENV             0
  ENTRY("open_env"),
#define CLOSE_ENV            1
  ENTRY("close_env"),
#define OPEN_DB              2
  ENTRY("open_db"),
#define CLOSE_DB             3
  ENTRY("close_db"),
#define READ_SPEC            4
  ENTRY("read_spec"),
#define NEXT_TERMREF         5
  ENTRY("next_termref"),
#define STORE_TERMREF        6
  ENTRY("store_termref"),
#define DELETE_TERMREF       7
  ENTRY("delete_termref"),
#define STORE_TERM           8
  ENTRY("store_term"),
#define DELETE_TERM          9
  ENTRY("delete_term"),
#define FETCH_TERM           10
  ENTRY("fetch_term"),
#define GLOBAL_ITERATOR      11
  ENTRY("global_iterator"),
#define GLOBAL_ITERATOR_NEXT 12
  ENTRY("global_iterator_next"),
#define GLOBAL_ITERATOR_DONE 13
  ENTRY("global_iterator_done"),
#define TERM_ITERATOR        14
  ENTRY("term_iterator"),
#define TERM_ITERATOR_NEXT   15
  ENTRY("term_iterator_next"),
#define TERM_ITERATOR_DONE   16
  ENTRY("term_iterator_done"),
#define DB_SYNC              17
  ENTRY("db_sync"),

#define TERM_ITERATOR_NEXT_C_GET1              18
  ENTRY("term_iterator_next_c_get1"),
#define TERM_ITERATOR_NEXT_C_GET2              19
  ENTRY("term_iterator_next_c_get2"),
#define TERM_ITERATOR_NEXT_GET                 20
  ENTRY("term_iterator_next_get"),

  ENTRY(NULL)
};


/* static struct tms stv; */

static void start_stat(int i)
{
  ++stattable[i].count;
  stattable[i].stv_walltime = times(&stattable[i].stv);
}

#define timeop(a, op, b)                \
  do {                                  \
    (a).tms_utime op (b).tms_utime;     \
    (a).tms_stime op (b).tms_stime;     \
    (a).tms_cutime op (b).tms_cutime;   \
    (a).tms_cstime op (b).tms_cstime;   \
  } while (0)

static void end_stat(int i)
{
  struct tms etv;
  clock_t wallnow;

  wallnow = times(&etv);
  timeop(etv, -=, stattable[i].stv);
  timeop(stattable[i].time, +=, etv);

  stattable[i].walltime += (wallnow-stattable[i].stv_walltime);
}

#define START_STAT(x) start_stat(x)
#define END_STAT(x)   end_stat(x)

#ifndef CLOCKS_PER_SECOND
/* This is not the right way to do this, nowadays */
#define CLOCKS_PER_SECOND CLK_TCK
#endif /* CLOCKS_PER_SECOND */

void SPCDECL printstat(void)
{
  int i;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  SP_printf("name                      calls     user   system   walltime\n"
            "-------------------------------------------------\n");
  for (i = 0; stattable[i].name != NULL; ++i) {
    SP_printf("%-21s %9u %8.3f %8.3f %8.3f\n",
              stattable[i].name, stattable[i].count,
              stattable[i].time.tms_utime/(double)CLOCKS_PER_SECOND,
              stattable[i].time.tms_stime/(double)CLOCKS_PER_SECOND,
              stattable[i].walltime/(double)CLOCKS_PER_SECOND
              );
  }
  SP_flush_output(SP_stdout, SPIO_OPTION_NONE);
#if PRINTSTAT_ZEROSTAT
  zerostat();
#endif  /* PRINTSTAT_ZEROSTAT */
}

void SPCDECL zerostat(void)
{
  int i;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  for (i = 0; stattable[i].name; ++i) {
    stattable[i].count = 0;
    stattable[i].time.tms_utime = 0;
    stattable[i].time.tms_stime = 0;
    stattable[i].time.tms_cutime = 0;
    stattable[i].time.tms_cstime = 0;
    stattable[i].walltime = 0;
    /* [PM] 4.1.2 no need to zero out stattable[i].stv, stv_walltime */
  }
}

#else  /* !STAT */

#define START_STAT(x)
#define END_STAT(x)

void zerostat(void)
{
}
void printstat(void)
{
}

#endif /* !STAT */

void SPCDECL db_init(int when)
{
  (void)when;                  /* [PM] 3.9b5 avoid -Wunused */  
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  /* [PM] 3.8.6 PRM 1870 (patch version change require recompile)
   
     Barf if run with a version that does not exactly match what was
     used when compiling.
     [PD] 3.9 patch version may change.
  */
  {
    int major,minor,patch;
    char *versionstring;

    versionstring = db_version (&major,&minor,&patch);
    (void)versionstring;

#if BDB_DEBUG
      fprintf(stderr, "library(bdb) compiled with %d.%d.%d \"" DB_VERSION_STRING "\"\n", (int) DB_VERSION_MAJOR, (int)DB_VERSION_MINOR, (int)DB_VERSION_PATCH );
      fprintf(stderr, "library(bdb) linked to bdb run-time %d.%d.%d \"%s\"\n", major, minor, patch, versionstring );
#endif /* BDB_DEBUG */
    
    if (! ( major == DB_VERSION_MAJOR
            && minor == DB_VERSION_MINOR
/* [PD] 3.9 patch version may change. */
            && patch >= DB_VERSION_PATCH
            ) )
      {
        /* [PM] 3.10 gcc will warn that it cannot verify non-literal format strings. */
#define formatstring "Installed BDB is %d.%d.%d. library(bdb) requires \"" DB_VERSION_STRING "\""
        char buf_ienc[10+10+10+sizeof formatstring];
#if HAVE_SNPRINTF
        snprintf
#elif HAVE__SNPRINTF
          _snprintf
#else
#error "no snprintf"
#endif
          (buf_ienc, sizeof buf_ienc, formatstring, major, minor, patch);
        buf_ienc[(sizeof buf_ienc)-1] = '\0';
#undef formatstring

        SP_save_and_raise_error(SYSTEM_ERROR, buf_ienc, 0, "bdb", "db_init", 0, 0);
        return;
      }
  }
#if 0 /* [PD] BDB 3 does not have db_value_set. The patch from IQSOFT
	 eliminates the call to db_value_set. */
  /* [PM] 3.8.6 init functions should not return a value (used to return non-zero on failure) */
#if SP_WIN32
  if (db_value_set(1, DB_REGION_NAME)) /* see Berkeley DB db_appinit */
    {
        SP_save_and_raise_error(SYSTEM_ERROR, "could not initialize BDB", 0, "bdb", "db_init", 0, 0);
        return;
    }
#endif /* SP_WIN32 */
#endif /* [PD] BDB 3 */
#if BDB_DEBUG
#if USE_BIGENDIAN_KEYS
  {
    u_int32_t right = 0x12345678;
    u_int32_t wrong = 0x78563412;
    u_int32_t k = right;
    ADJUST_KEY_ENDIANNESS(k);
    fprintf(stderr, "USE_BIGENDIAN_KEYS: right=0x%" SPRIxINTEGER ", wrong=0x%" SPRIxINTEGER ", ADJUST_KEY_ENDIANNESS(right)==0x%" SPRIxINTEGER " (%s), WRONG_ENDIANNESS=%d\n",
            (SP_uinteger)right, (SP_uinteger)wrong, (SP_uinteger)k,
            ((WRONG_ENDIANNESS ? k==wrong : k==right) ? "OK" : "ERROR"),
            (int)WRONG_ENDIANNESS);
  }
#endif  /* USE_BIGENDIAN_KEYS */
#endif  /* BDB_DEBUG */
}

void SPCDECL db_deinit(int when)
{
  (void)when;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
#if STAT
  printstat();
#endif  /* STAT */
}


/*
  Environment functions
 */
/* [PD] 4.0.5 Use library(file_systems). Do not manipulate file names and 
              directories in C code */
SP_integer SPCDECL open_env(char SP_FLI_CONST *env_name, SP_integer cache_size, SP_integer *lenv)
{
  DB_ENV *myenv;
  DB_ENV **env = &myenv;
  int err;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  START_STAT(OPEN_ENV);

  if ((err = db_env_create(env, 0)) != 0) return err;
  *lenv = (SP_integer)(*env);
  if (cache_size < 20) cache_size = 20; /* 20KB is the min. for BDB */
  { /* [PD] 3.9 convert to unsigned */
    SP_uinteger u_cache_size = (SP_uinteger)cache_size;
    (*env)->set_cachesize(*env,(u_int32_t)(u_cache_size>>20),(u_int32_t)((u_cache_size&0xfffff)<<10), 0);
  }
  if ((err = (*env)->open(*env, env_name,
                            DB_CREATE|DB_INIT_CDB|DB_INIT_MPOOL, 0)) != 0) {
#if BDB_DEBUG
    fprintf(stderr, "db: %s: %s (%d)\n", env_name, db_strerror(err), err);
#endif /* BDB_DEBUG */
    (*env)->close(*env, 0);
  }

  /* [PM] 3.11.2 Was: if (err) SP_free(*env); */

  END_STAT(OPEN_ENV);
  return err;
}

SP_integer SPCDECL close_env(SP_integer lenv)
{
  DB_ENV *env = (DB_ENV *)lenv;
  int err;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  START_STAT(CLOSE_ENV);
  
  err = env->close(env, 0);

  END_STAT(CLOSE_ENV);
  
  return err;
}

/*
  Database functions
 */

/* filename0 = <dbname>/terms.db
   filename1 = <dbname>/index.db
   filename2 = <dbname>/admin.db
   
[PM] 4.1 Cleans up after itself on error and sets
termsdb,admindb,indexdb to NULL. Thus caller should not do
dbp->XXXdb->close(...)

*/
static int db_open_files(char const * filename0, char const * filename1, char const * filename2, u_int32_t flags,
                  DB_ENV *env, db_struct *dbp)
{
  int err = 0;
  DB *cleanup_termsdb = NULL;   /* [PM] non-NULL if close() needed. */
  DB *cleanup_admindb = NULL;
  DB *cleanup_indexdb = NULL;

#if BDB_DEBUG
  fprintf(stderr, "db_open_files: env = %p\n", env);
#endif

  if ((err = db_create(&dbp->termsdb, env, 0)) != 0)
    {
      goto barf;
    }
  cleanup_termsdb = dbp->termsdb;


  if ((err = db_create(&dbp->admindb, env, 0)) != 0)
    {
      goto barf;
    }
  cleanup_admindb = dbp->admindb;

  if ((err = db_create(&dbp->indexdb, env, 0)) != 0)
    {
      goto barf;
    }
  cleanup_indexdb = dbp->indexdb;

  if ((err = dbp->indexdb->set_flags(dbp->indexdb, DB_DUP))) /* there can be duplicates here */
    {
      goto barf;
    }

  /* [PD] 4.0.5 Moved setting of cachesize to *after* the creation of all
     databases ... */
  if (dbp->cache_size>0) {
    SP_uinteger u_cache_size = (SP_uinteger)dbp->cache_size;
    if ((err = dbp->termsdb->set_cachesize(dbp->termsdb,
                                           (u_int32_t)(u_cache_size>>20),
                                           (u_int32_t)((u_cache_size&0xfffff)<<10), 0)) != 0) {
      goto barf;
    }
    if ((err = dbp->admindb->set_cachesize(dbp->admindb,
                                           (u_int32_t)(u_cache_size>>20),
                                           (u_int32_t)((u_cache_size&0xfffff)<<10), 0)) != 0) {
      goto barf;
    }
    if ((err = dbp->indexdb->set_cachesize(dbp->indexdb,
                                           (u_int32_t)(u_cache_size>>20),
                                           (u_int32_t)((u_cache_size&0xfffff)<<10), 0)) != 0) {
      goto barf;
    }
  }

  if ((err = dbp->termsdb->open(dbp->termsdb,
#if BDB_4_1
                                NULL,
#endif  /* BDB_4_1 */
                                filename0, NULL, TERMSDB_DBTYPE,
                                flags, FMODE)) != 0) {
#if BDB_DEBUG
    fprintf(stderr, "db: %s: %s (%d)\n", filename0, db_strerror(err), err);
#endif /* BDB_DEBUG */
    goto barf;
  }

  if ((err = dbp->admindb->open(dbp->admindb,
#if BDB_4_1			/* [PD] 3.9.2 */
                                NULL,
#endif  /* BDB_4_1 */
                                filename2, NULL, ADMINDB_DBTYPE,
                                flags, FMODE)) != 0) {
#if BDB_DEBUG
    fprintf(stderr, "db: %s: %s (%d)\n", filename2, db_strerror(err), err);
#endif /* BDB_DEBUG */
    goto barf;
  }

  if ((err = dbp->indexdb->open(dbp->indexdb,
#if BDB_4_1
                                NULL,
#endif  /* BDB_4_1 */
                                filename1, NULL, INDEXDB_DBTYPE,
                                flags, FMODE)) != 0) {
#if BDB_DEBUG
    fprintf(stderr, "db: %s: %s (%d)\n", filename1, db_strerror(err), err);
#endif /* BDB_DEBUG */
    goto barf;
  }
  /* [PM] 4.1 We will succeed. Protect from cleanup. */
  cleanup_termsdb = NULL;
  cleanup_admindb = NULL;
  cleanup_indexdb = NULL;

#if BDB_DEBUG
  {
    u_int32_t gbytes = 0;
    u_int32_t bytes = 0;
    int ncache = 0;
    if (dbp->env != NULL)
      {
        if ((err = dbp->env->get_cachesize(dbp->env, &gbytes, &bytes, &ncache)) != 0)
          {
#if BDB_DEBUG
            fprintf(stderr, "db: Could not get cachesize for environment: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
            goto barf;        
          }
        else
          {
#if BDB_DEBUG
            fprintf(stderr, "db: cachesize %dGB + %d bytes in %d caches for environment\n", (int)gbytes, (int)bytes, (int)ncache);
#endif /* BDB_DEBUG */        
          }
      }
    else
      {
        if ((err = dbp->termsdb->get_cachesize(dbp->termsdb, &gbytes, &bytes, &ncache)) != 0)
          {
#if BDB_DEBUG
            fprintf(stderr, "db: Could not get cachesize for %s: %s (%d)\n", filename0, db_strerror(err), err);
#endif /* BDB_DEBUG */
            goto barf;        
          }
        else
          {
#if BDB_DEBUG
            fprintf(stderr, "db: cachesize %dGB + %d bytes in %d caches for %s\n", (int)gbytes, (int)bytes, (int)ncache, filename0);
#endif /* BDB_DEBUG */        
          }
      }
  }
#endif  /* BDB_DEBUG */


  err = 0;
 cleanup:
  if (cleanup_indexdb != NULL) {
    (void) cleanup_indexdb->close(cleanup_indexdb, 0);
    dbp->indexdb = NULL;
  }
  if (cleanup_admindb != NULL) {
    (void) cleanup_admindb->close(cleanup_admindb, 0);
    dbp->admindb = NULL;
  }
  if (cleanup_termsdb != NULL) {
    (void) cleanup_termsdb->close(cleanup_termsdb, 0);
    dbp->termsdb = NULL;
  }

  return err;
 barf:
  goto cleanup;
}

static int prepare_db(DB_ENV *env, db_struct **db, SP_integer cache_size);

SP_integer SPCDECL open_db_enumerate(SP_integer lenv, char const *termsdbname, db_struct **db, SP_integer cache_size)
{
  int err;
  DB_ENV *env = (DB_ENV *)lenv;
  db_struct *dbp = NULL;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  err = prepare_db(env, db, cache_size);
  if (0 != err) { goto error; }
  dbp = *db;

  if ((err = db_create(&dbp->termsdb, env, 0)) != 0) {
    goto error;
  }
  if (dbp->cache_size>0) {
    SP_uinteger u_cache_size = (SP_uinteger)dbp->cache_size;
    dbp->termsdb->set_cachesize(dbp->termsdb,
				(u_int32_t)(u_cache_size>>20),
				(u_int32_t)((u_cache_size&0xfffff)<<10), 0);
  }
#if BDB_4_1			/* [PD] 3.9.2 */
  if ((err = dbp->termsdb->open(dbp->termsdb, NULL, termsdbname, NULL, TERMSDB_DBTYPE,
                                  DB_RDONLY, FMODE)) != 0)
#else
  if ((err = dbp->termsdb->open(dbp->termsdb, termsdbname, NULL, TERMSDB_DBTYPE,
                                  DB_RDONLY, FMODE)) != 0)
#endif
  {
    dbp->termsdb->close(dbp->termsdb, 0);
#if BDB_DEBUG > 1
    fprintf(stderr, "db: %s: %s (%d)\n", termsdbname, db_strerror(err), err);
#endif /* BDB_DEBUG */
    goto error;
  }

  return 0;

 error:
  close_db(dbp);
  return err;
}

SP_integer SPCDECL open_db_read(SP_integer lenv, char const *termsdbname, char const *indexdbname, char const *admindbname, db_struct **db, SP_integer cache_size)
{
  int err;
  DB_ENV *env = (DB_ENV *)lenv;
  db_struct *dbp = NULL;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  err = prepare_db(env, db, cache_size);
  if (0 != err) { goto error; }
  dbp = *db;

  if ((err = db_open_files(termsdbname, indexdbname, admindbname, DB_RDONLY, env, dbp)) != 0) {
#if 0                           /* [PM] 4.1 */
    if (dbp->termsdb) dbp->termsdb->close(dbp->termsdb, 0);
    if (dbp->indexdb) dbp->indexdb->close(dbp->indexdb, 0);
    if (dbp->admindb) dbp->admindb->close(dbp->admindb, 0);
#endif  /* 0 */
    memset(dbp, 0, sizeof(db_struct));
    goto error;
  }
  return 0;

 error:
  close_db(dbp);
  return err;
}

#if BDB_DEBUG
/* Return db_home of env, if available or the empty string (never
   NULL). Argument env may be NULL. */
static char const *debug_env_get_home(DB_ENV *env)
{
  if (env != NULL)
    {
      char const *home = NULL;
      /* [PM] 4.1 Note: the db_home field was presumably never API and is gone in BDB 4.8 */
      if (env->get_home(env, &home) == 0)
        {
          if (home != NULL)
            {
              return home;
            }
          else                  /* unexpected */
            {
              SP_ASSERT(0);
            }
        }
    }
  return "";
}
#endif  /* BDB_DEBUG */

SP_integer SPCDECL open_db_update(SP_integer lenv, char const *termsdbname, char const *indexdbname, char const *admindbname,
			  db_struct **db, SP_integer lspec, SP_integer specsize_, SP_integer cache_size, SP_integer nfiles)
{
  u_int32_t flags;
  int err;
  DB_ENV *env = (DB_ENV *)lenv;
  db_struct *dbp = NULL;
  void *spec = (void *)lspec;
  u_int32_t specsize;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

#if BDB_DEBUG
  fprintf(stderr, "open_db_update: home = \"%s\", db = \"%s\", \"%s\", \"%s\"\n", 
          debug_env_get_home(env), termsdbname, indexdbname, admindbname);
#endif
  if (! (0 < specsize_ && specsize_ <= ((u_int32_t)-1))) {
      err = BDB_PARAMETER_ERROR;
      goto error;
    }
  specsize = (u_int32_t)specsize_;

  err = prepare_db(env, db, cache_size);
  if (0 != err) { goto error; }
  dbp = *db;

  flags = nfiles ? 0 : DB_CREATE;
  if ((err = db_open_files(termsdbname, indexdbname, admindbname, flags, env, dbp)) != 0)
    goto error;

  if (nfiles == 0) {            /* create admin file */
    u_int32_t k, r = 0;
    DBT key, data;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    k = NREFKEY;                /* store the next available TERMREF */
    key.data = &k;
    key.size = sizeof(k);
    data.data = &r;
    data.size = sizeof(r);
    if ((err = dbp->admindb->put(dbp->admindb, NULL, &key, &data, 0)) != 0)
      goto error; 

    k = SPECKEY;                /* store the speclist */
    key.data = &k;
    data.data = spec;
    data.size = specsize;
    if ((err = dbp->admindb->put(dbp->admindb, NULL, &key, &data, 0)) != 0)
      goto error;
    if (dbp->cache_size == -1) { /* flush cache */
      START_STAT(DB_SYNC);
      dbp->admindb->sync(dbp->admindb, 0);
      END_STAT(DB_SYNC);
    }
  }
  return 0;

error:
#if 0                           /* [PM] 4.1 Not needed since we call close_db */
  if (dbp->termsdb) dbp->termsdb->close(dbp->termsdb, 0);
  if (dbp->indexdb) dbp->indexdb->close(dbp->indexdb, 0);
  if (dbp->admindb) dbp->admindb->close(dbp->admindb, 0);
  memset(dbp, 0, sizeof(db_struct));
#endif  /* 0 */

  close_db(dbp); /* [PD] 4.0.5 Yes we can, and must, use close_db, since we have called prepare_db.
		    FIXME: investigate if the low level close-stuff right after the label error
		    above is necessary. */
  return err;
}

SP_integer SPCDECL close_db(db_struct *db)
{
  int err;

#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  START_STAT(CLOSE_DB);
  
  if (db) {
    if (db->termsdb) db->termsdb->close(db->termsdb, 0); /* error handling? */
    if (db->indexdb) db->indexdb->close(db->indexdb, 0);
    if (db->admindb) db->admindb->close(db->admindb, 0);
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d SP_free(db==%p)\n", __FUNCTION__, __FILE__, (int)__LINE__, (void*)db);
#endif
    SP_free(db);
    
    err = 0;
  } else {
    err = EINVAL;
  }
  END_STAT(CLOSE_DB);
  return err;
}

/* [PD] 4.0.5 Use library(file_systems). Do not manipulate file names and 
              directories in C code */
static int prepare_db(DB_ENV *env, db_struct **db, SP_integer cache_size)
{
  int err;
  db_struct *dbp;

#if BDB_DEBUG > 1
  fprintf(stderr, "prepare_db: env = %p, cache_size==%" SPRIdINTEGER "\n", env, (SP_integer)cache_size);
  fflush(stderr);
#endif

/*  START_STAT(OPEN_DB); */  /* [PD] 4.0.5 FIXME: START_STAT() and END_STAT must be called somewhere else. */

  *db = dbp = (db_struct *)SP_malloc(sizeof(db_struct));
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d SP_malloc(): db==%p\n", __FUNCTION__, __FILE__, (int)__LINE__, (void*)db);
#endif
  if (dbp == NULL) {
    err = ENOMEM;
    goto error;
  }
  memset(dbp, 0, sizeof(db_struct));
  dbp->env = env;
  if (cache_size>0 && cache_size<20)
    {
#if BDB_DEBUG
      fprintf(stderr, "Using default cachesize\n");
#endif                 /* BDB_DEBUG */
      cache_size = 20;		/* 20KB is the min. for BDB */
    }

  dbp->cache_size = cache_size;

/*  END_STAT(OPEN_DB); */  /* [PD] 4.0.5 FIXME: START_STAT() and END_STAT must be called somewhere else. */
  
  return 0;

error:
  return err;
}

SP_integer SPCDECL read_spec(db_struct *db, SP_integer *lspeclist)
{
  int err;

#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  START_STAT(READ_SPEC);
  
  if (db && db->admindb) {
    u_int32_t k = SPECKEY;
    DBT key, data;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    key.data = &k;
    key.size = sizeof(k);
    err = db->admindb->get(db->admindb, NULL, &key, &data, 0);
#if BDB_DEBUG
    switch (err) {
    case 0:
      break;
    case DB_NOTFOUND:
      fprintf(stderr, "speclist not found.\n");
      break;
    default:
      fprintf(stderr, "get speclist: %s (%d)\n", db_strerror(err), err);
    }
#endif /* BDB_DEBUG */
    if (err) return err;
    *lspeclist = (SP_integer)data.data;
    
    END_STAT(READ_SPEC);
    
    return 0;
  }
  return EINVAL;
}

SP_integer SPCDECL next_termref(db_struct *db, SP_integer *termref)
{
  int err;
  u_int32_t k = NREFKEY, d, lockid;
  DBT key, data;
  DB_LOCK lock;
  DB_ENV *env;

#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  START_STAT(NEXT_TERMREF);
  
  if (db == NULL || db->admindb == NULL) return EINVAL;
  env = db->env;

  memset(&key, 0, sizeof(key));
  key.data = &k;
  key.size = sizeof(k);

  if (env) {
    if ((err = LOCK_ID(env, &lockid)) != 0) {
#if BDB_DEBUG
      fprintf(stderr, "cannot get lockid in env %p: %s (%d)\n", env, db_strerror(err), err);
#endif /* BDB_DEBUG */
      return err;
    }

    if ((err = LOCK_GET(env, lockid, 0, &key, DB_LOCK_WRITE, &lock)) != 0) {
#if BDB_DEBUG > 1
      fprintf(stderr, "cannot acquire lock: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
      return err;
    }
  }

  memset(&data, 0, sizeof(data));
  if ((err = db->admindb->get(db->admindb, NULL, &key, &data, 0)) != 0) {
#if BDB_DEBUG > 1
    fprintf(stderr, "get next termref: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
    goto unlock;
  }

  d = *(u_int32_t *)data.data;
  {
    u_int32_t tmp = d;
    /* Experiment with generating bigendian keys */
    ADJUST_GENERATED_KEY_ENDIANNESS(tmp);
    *termref = tmp;

#if BDB_DEBUG > 1
    fprintf(stderr, "Generated termref 0x%" SPRIxINTEGER ", returned as 0x%" SPRIxINTEGER "\n", (SP_uinteger)d, (SP_uinteger)tmp);
#endif  /* BDB_DEBUG */
  }

  if (++d == 0) {
    err = ERANGE;             /* Result too large */
    goto unlock;
  }
  data.data = &d;
  data.size = sizeof(d);
  if ((err = db->admindb->put(db->admindb, NULL, &key, &data, 0)) != 0) {
#if BDB_DEBUG > 1
    fprintf(stderr, "put next termref: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
    goto unlock;
  }
  if (db->cache_size == -1) {   /* flush cache */
    START_STAT(DB_SYNC);
    db->admindb->sync(db->admindb, 0);
    END_STAT(DB_SYNC);
  }

unlock:
  if (env)
    {
      int unlock_err;

      if ((unlock_err = LOCK_PUT(env, &lock)) != 0) {
#if BDB_DEBUG
        fprintf(stderr, "cannot release lock: %s (%d)\n", db_strerror(unlock_err), unlock_err);
#endif /* BDB_DEBUG */
      }
      if ((unlock_err = LOCK_ID_FREE(env, lockid)) != 0) {
#if BDB_DEBUG
        fprintf(stderr, "cannot free lock: %s (%d)\n", db_strerror(unlock_err), unlock_err);
#endif /* BDB_DEBUG */
      }
      if (err == 0)
        {
          err = unlock_err;
        }
    }

  END_STAT(NEXT_TERMREF);
  
  return err;
}

SP_integer SPCDECL store_termref(db_struct *db, SP_integer hc_, SP_integer termref_)
{
  int err;
  u_int32_t hc, termref;

#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  START_STAT(STORE_TERMREF);
  if (! IS_VALID_HASH_CODE(hc_)) {
    err = BDB_PARAMETER_ERROR;
    return err;
  }
  if (! (0 <= termref_ && termref_ <= ((u_int32_t)-1))) {
    err = BDB_PARAMETER_ERROR;
    return err;
  }
  hc = (u_int32_t)hc_;
  termref = (u_int32_t)termref_;

  if (db && db->indexdb) {
    u_int32_t k = hc;
    u_int32_t d = termref;
    DBT key, data;

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));
    ADJUST_KEY_ENDIANNESS(k);
    key.data = &k;
    key.size = sizeof(k);
    data.data = &d;
    data.size = sizeof(d);
    err = db->indexdb->put(db->indexdb, NULL, &key, &data, 0);
#if BDB_DEBUG
    if (err)
      fprintf(stderr, "store termref: %s (%d)\n", db_strerror(err), (int)err);
#endif /* BDB_DEBUG */

    if (db->cache_size == -1) { /* flush cache */
      START_STAT(DB_SYNC);
      db->indexdb->sync(db->indexdb, 0);
      END_STAT(DB_SYNC);
    }
    END_STAT(STORE_TERMREF);

    return err;
  }
  return EINVAL;
}

SP_integer SPCDECL delete_termref(db_struct *db, SP_integer hc_, SP_integer termref_)
{
  int err;
  u_int32_t hc, termref;
  u_int32_t k, d;
  DBT key, data;
  DBC *cursor;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif

  if (! IS_VALID_HASH_CODE(hc_)) {
    err = BDB_PARAMETER_ERROR;
    return err;
  }
  if (! (0 <= termref_ && termref_ <= ((u_int32_t)-1))) {
    err = BDB_PARAMETER_ERROR;
    return err;
  }
  hc = (u_int32_t)hc_;
  termref = (u_int32_t)termref_;
  k = hc;
  d = termref;

  START_STAT(DELETE_TERMREF);
  
  if (db == NULL || db->indexdb == NULL) return EINVAL;

  err = db->indexdb->cursor(db->indexdb, NULL, &cursor,
                            db->env != NULL ? DB_WRITECURSOR : 0);
  if (err) {
#if BDB_DEBUG
    fprintf(stderr, "create cursor: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
    return err;
  }

  memset(&key, 0, sizeof(key));
  memset(&data, 0, sizeof(data));

  ADJUST_KEY_ENDIANNESS(k);
  key.data = &k;
  key.size = sizeof(k);
  for (err = cursor->c_get(cursor, &key, &data, DB_SET); /* DB_NOTFOUND!!! */
       err == 0;
       err = cursor->c_get(cursor, &key, &data, DB_NEXT_DUP)) {
    if (d == *(u_int32_t *)data.data) {
      err = cursor->c_del(cursor, 0);
      break;
    }
  }

  cursor->c_close(cursor);
  if (db->cache_size == -1) {   /* flush cache */
    START_STAT(DB_SYNC);
    db->indexdb->sync(db->indexdb, 0);
    END_STAT(DB_SYNC);
  }

  END_STAT(DELETE_TERMREF);

  return err == DB_NOTFOUND ? 0 : err;
}

SP_integer SPCDECL store_term(db_struct *db, SP_integer termref_, SP_integer lterm, SP_integer termsize_)
{
  int err;
  u_int32_t termref, termsize;

#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  START_STAT(STORE_TERM);

  if (! (0 <= termref_ && termref_ <= ((u_int32_t)-1))) {
    err = BDB_PARAMETER_ERROR;
    return err;
  }
  termref = (u_int32_t)termref_;
  if (! (0 <= termsize_ && termsize_ <= ((u_int32_t)-1))) {
    err = BDB_PARAMETER_ERROR;
    return err;
  }
  termsize = (u_int32_t)termsize_;
  
  if (db && db->termsdb) {
    u_int32_t k = termref;
    DBT key, data;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    ADJUST_KEY_ENDIANNESS(k);
    key.data = &k;
    key.size = sizeof(k);
    data.data = (void *)lterm;
    data.size = termsize;
    err = db->termsdb->put(db->termsdb, NULL, &key, &data, 0);
#if BDB_DEBUG
    if (err)
      fprintf(stderr, "store term: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
    if (db->cache_size == -1) { /* flush cache */
      START_STAT(DB_SYNC);
      db->termsdb->sync(db->termsdb, 0);
      END_STAT(DB_SYNC);
    }

    END_STAT(STORE_TERM);
    
    return err;
  }
  return EINVAL;
}

SP_integer SPCDECL delete_term(db_struct *db, SP_integer termref_)
{
  int err;
  u_int32_t termref;

#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  START_STAT(DELETE_TERM);
  if (! (0 <= termref_ && termref_ <= ((u_int32_t)-1))) {
    err = BDB_PARAMETER_ERROR;
    return err;
  }
  termref = (u_int32_t)termref_;
  
  if (db && db->termsdb) {
    u_int32_t k = termref;
    DBT key;
    memset(&key, 0, sizeof(key));
    ADJUST_KEY_ENDIANNESS(k);
    key.data = &k;
    key.size = sizeof(k);
    err = db->termsdb->del(db->termsdb, NULL, &key, 0);
#if BDB_DEBUG
    if (err)
      fprintf(stderr, "delete term: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
    if (db->cache_size == -1) { /* flush cache */
      START_STAT(DB_SYNC);
      db->termsdb->sync(db->termsdb, 0);
      END_STAT(DB_SYNC);
    }

    END_STAT(DELETE_TERM);
    
    return err;
  }
  return EINVAL;
}

SP_integer SPCDECL fetch_term(db_struct *db, SP_integer termref_, SP_integer *lterm)
{
  int err;
  u_int32_t termref;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  START_STAT(FETCH_TERM);

  if (! (0 <= termref_ && termref_ <= ((u_int32_t)-1))) {
    err = BDB_PARAMETER_ERROR;
    return err;
  }
  termref = (u_int32_t)termref_;
  
  if (db && db->termsdb) {
    u_int32_t k = termref;
    DBT key, data;
    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    ADJUST_KEY_ENDIANNESS(k);
    key.data = &k;
    key.size = sizeof(k);
    err = db->termsdb->get(db->termsdb, NULL, &key, &data, 0);
    if (err) {
#if BDB_DEBUG
      fprintf(stderr, "fetch term: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
      return err;
    }
    *lterm = (SP_integer)data.data;

    END_STAT(FETCH_TERM);
    
    return 0;
  }
  return EINVAL;
}

/*
  If there are no more solutions the iterator functions succeed but set
  term to NULL.
 */
SP_integer SPCDECL global_iterator(db_struct *db, SP_integer *litp)
{
  int err;
  DBC *myitp = NULL;
  DBC **itp = &myitp;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  START_STAT(GLOBAL_ITERATOR);
  
  if (db == NULL || db->termsdb == NULL) return EINVAL;

  err = db->termsdb->cursor(db->termsdb, NULL, itp, 0);
  *litp = (SP_integer)(*itp);
#if BDB_DEBUG > 1
  if (err != 0)
    fprintf(stderr, "create global iterator: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */

  END_STAT(GLOBAL_ITERATOR);

  return err;
}

SP_integer SPCDECL global_iterator_next(SP_integer lit, SP_integer *lterm, SP_integer *termref)
{
  int err;
  DBC *it = (DBC *)lit;
  DBT key, data;

#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  START_STAT(GLOBAL_ITERATOR_NEXT);
  
  if (it == NULL) return EINVAL;

  memset(&key, 0, sizeof(key));
  memset(&data, 0, sizeof(data));
  err = it->c_get(it, &key, &data, DB_NEXT);
  if (err != 0) {
    if (err == DB_NOTFOUND) { /* no more solutions */
      *lterm = (SP_integer)NULL;
      return 0;
    }
#if BDB_DEBUG > 1
    fprintf(stderr, "set global iterator: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
    return err;
  }

  *lterm = (SP_integer)data.data;
  *termref = *(u_int32_t *)key.data;

  END_STAT(GLOBAL_ITERATOR_NEXT);

  return 0;
}

SP_integer SPCDECL global_iterator_done(SP_integer lit)
{
  DBC *it = (DBC *)lit;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  START_STAT(GLOBAL_ITERATOR_DONE);
  
  if (it) {
    it->c_close(it);            /* error handling? */
    END_STAT(GLOBAL_ITERATOR_DONE);
    return 0;
  }
  return EINVAL;
}

SP_integer SPCDECL term_iterator(db_struct *db, SP_term_ref hclist, iterator **itp)
{
  int err;
  iterator *it;
  SP_term_ref ref = SP_new_term_ref();
  int hcnum;
  SP_integer *keyptr;

#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  START_STAT(TERM_ITERATOR);

  if (db == NULL || db->indexdb == NULL || db->termsdb == NULL)
    return EINVAL;

  /* count the length of the list */
  hcnum = SP_is_compound(hclist) ? 1 : 0;
  if (hcnum) {
    SP_get_arg(2, hclist, ref);
    for (; SP_is_compound(ref); ++hcnum)
      SP_get_arg(2, ref, ref);
  }
#if USE_BIGENDIAN_KEYS
#if !BDB_DEBUG
#error "[PM] 4.1.2 this function has not been updated for use with USE_BIGENDIAN_KEYS"
#else  /* BDB_DEBUG */
  exit(42);
#endif  /* BDB_DEBUG */
#endif  /* USE_BIGENDIAN_KEYS */
  *itp = it = (iterator *)SP_malloc(sizeof(iterator) + (hcnum-1)*sizeof(SP_integer));
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d SP_malloc(): it==%p\n", __FUNCTION__, __FILE__, (int)__LINE__, (void*)it);
#endif

  if (it == NULL) return ENOMEM;

  it->db = db;
  it->nkeys = hcnum;
  it->ckey = -1;
  for (keyptr = it->key; SP_is_compound(hclist);
       SP_get_arg(2, hclist, hclist), ++keyptr) {
    SP_get_arg(1, hclist, ref);
    SP_get_integer(ref, keyptr);
  }
  if ((err = db->indexdb->cursor(db->indexdb, NULL, &it->cursor, 0)) != 0) {
#if BDB_DEBUG > 1
    fprintf(stderr, "create term iterator: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d SP_free(it==%p)\n", __FUNCTION__, __FILE__, (int)__LINE__, (void*)it);
#endif
    SP_free(it);
  }

  END_STAT(TERM_ITERATOR);

  return err;
}

SP_integer SPCDECL term_iterator_next(iterator *it, SP_integer *lterm, SP_integer *termref)
{
  int err;
  DBT key, data, tdata;
  u_int32_t k;
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
#if USE_BIGENDIAN_KEYS
#if !BDB_DEBUG
#error "[PM] 4.1.2 this function has not been updated for use with USE_BIGENDIAN_KEYS"
#else  /* BDB_DEBUG */
  exit(42);
#endif  /* BDB_DEBUG */
#endif  /* USE_BIGENDIAN_KEYS */

  START_STAT(TERM_ITERATOR_NEXT);

  if (it == NULL || it->cursor == NULL ||
      it->db == NULL || it->db->termsdb == NULL) return EINVAL;

  if (it->ckey == -1) goto newhc; /* is this the very first time? */

next:                 /* find the next termref under the current hash code */
  memset(&key, 0, sizeof(key));
  memset(&data, 0, sizeof(data));

  START_STAT(TERM_ITERATOR_NEXT_C_GET1);
  err = it->cursor->c_get(it->cursor, &key, &data, DB_NEXT_DUP);
  END_STAT(TERM_ITERATOR_NEXT_C_GET1);
  if (err != 0)
    {
      if (err == DB_NOTFOUND) goto newhc; /* maybe the next key */
      else return err;          /* real error */
    }
fetch:                          /* DB_SET or DB_NEXT_DUP succeeded */
  memset(&tdata, 0, sizeof(tdata));
  START_STAT(TERM_ITERATOR_NEXT_GET);
  err = it->db->termsdb->get(it->db->termsdb, NULL, &data, &tdata, 0);
  END_STAT(TERM_ITERATOR_NEXT_GET);
  if (err == 0) {             /* success */
    *lterm = (SP_integer)tdata.data;
    *termref = *(u_int32_t *)data.data;

    END_STAT(TERM_ITERATOR_NEXT);
  
    return 0;
  }
  if (err != DB_NOTFOUND) return err; /* real error */
#if 0                           /* See notes.txt! (it->cursor is read-only) */
  it->cursor->c_del(it->cursor, 0); /* dangling termref */
#endif
#if BDB_DEBUG
  fprintf(stderr, "dangling termref found\n");
#endif
  goto next;

newhc:                          /* a new hash code */
  if (++it->ckey >= it->nkeys) { /* no more keys to try */
    *lterm = (SP_integer)NULL;

    END_STAT(TERM_ITERATOR_NEXT);
  
    return 0;
  }
  
  k = (u_int32_t)it->key[it->ckey];
  memset(&key, 0, sizeof(key));
  memset(&data, 0, sizeof(data));
  key.data = &k;
  key.size = sizeof(u_int32_t);
  START_STAT(TERM_ITERATOR_NEXT_C_GET2);
  err = it->cursor->c_get(it->cursor, &key, &data, DB_SET);
  END_STAT(TERM_ITERATOR_NEXT_C_GET2);
  if (err == 0) goto fetch;
  if (err == DB_NOTFOUND) goto newhc; /* maybe the next key */
#if BDB_DEBUG > 1
  fprintf(stderr, "set term iterator: %s (%d)\n", db_strerror(err), err);
#endif /* BDB_DEBUG */
  return err;                 /* real error */
}

SP_integer term_iterator_done(iterator *it)
{
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  START_STAT(TERM_ITERATOR_DONE);
  
  if (it && it->cursor) {
    it->cursor->c_close(it->cursor); /* error handling? */
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d SP_free(it==%p)\n", __FUNCTION__, __FILE__, (int)__LINE__, (void*)it);
#endif
    SP_free(it);

    END_STAT(TERM_ITERATOR_DONE);

    return 0;
  }
  return EINVAL;
}

char SP_FLI_CONST * SPCDECL decode_error(SP_integer err)
{
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  switch (err)
    {
    case BDB_PARAMETER_ERROR:
      return "Parameter error";
    default:
      break;
    }
  return SP_from_os(db_strerror((int)err), WCX_BEST_EFFORT);
}

/***************************
 * [PM] 3.11.0+ Conversion between fastrw binary formats used in
 * 3.11.0 (version D) and the format used pre 3.11.0 (version C).
 * Luckily the only difference is the version number
 */

#ifndef BDB_FASTRW_BACKWARD_COMPATIBILITY
/* 
   0 means use whatever version fastrw uses and do not convert fastrw terms read from DB.
   1 means store in fastrw version 'C' and convert from 'C' to 'D' when reading (to 'D').
   2 means store in fastrw version 'C' and convert either 'C' or 'D' when reading (to 'D')
     2 makes it possible to read a data base created with fastrw 'D'
     stored in the DB (as in the initial 3.11.0 release).
 */

#define BDB_FASTRW_BACKWARD_COMPATIBILITY 2
#endif  /* BDB_FASTRW_BACKWARD_COMPATIBILITY */

#define FASTRW_OLD_VERS 'C'
#define FASTRW_NEW_VERS 'D'

static int bdb_fastrw_backward_compatibility = (BDB_FASTRW_BACKWARD_COMPATIBILITY+0);

static SP_integer convert_fastrw_block(SP_integer from_block, char from_type,
                                 SP_integer *to_block, char to_type)
{

  /* We can do the conversion in place by just changing the version
     byte from 'C' to 'D' */

  *to_block = from_block;

  if (bdb_fastrw_backward_compatibility)
    {
      char *p_from = (char*)from_block;
      char *p_to = p_from;

      if (p_from == NULL) return 1;
      if (bdb_fastrw_backward_compatibility > 1)
        {
          /* very backward compatible: accept both new and old version byte on stored terms */
          if (*p_from != from_type
              && *p_from != to_type)
            return 2;
        }
      else
        {
          if (*p_from != from_type) return 2;
        }
      *p_to=to_type;              /* a.k.a. *p_from = to_type */
    }
  return 0;
}

SP_integer SPCDECL from_fastrw_old(SP_integer old_block, SP_integer *new_block)
{
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  return convert_fastrw_block(old_block, FASTRW_OLD_VERS, new_block, FASTRW_NEW_VERS);
}

SP_integer SPCDECL from_fastrw_new(SP_integer new_block, SP_integer new_size, SP_integer *old_block, SP_integer *old_size)
{
#if BDB_DEBUG > 1
    fprintf(stderr, "%s()@%s:%d ENTER\n", __FUNCTION__, __FILE__, (int)__LINE__);
#endif
  if (new_size <= 0) return 2;
  *old_size=new_size;
  return convert_fastrw_block(new_block, FASTRW_NEW_VERS, old_block, FASTRW_OLD_VERS);
}
