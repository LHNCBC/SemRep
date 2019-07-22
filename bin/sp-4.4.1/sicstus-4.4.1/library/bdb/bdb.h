#ifndef INCLUDED_NEWDB_H
#define INCLUDED_NEWDB_H

#define TERMSNAME "terms.db"    /* the TERMREF -> TERM mapping */
#define INDEXNAME "index.db"    /* the hash code -> TERMREF mapping */
#define ADMINNAME "admin.db"    /* the next TERMREF & the speclist */
#define FMODE 0644              /* mode for files */
#define DMODE 0755              /* mode for directories */
#define NREFKEY 0               /* key for the next available TERMREF */
#define SPECKEY 1               /* key for the speclist */

typedef struct 
{
  DB_ENV *env;                  /* env == 0 if outside any env */
  DB *termsdb, *indexdb, *admindb;
  SP_integer cache_size;		/* -1=off, 0=default, >0=use */
} db_struct;

typedef struct
{
  db_struct *db;
  DBC *cursor;
  int nkeys;                    /* total number of keys */
  int ckey;                     /* current key */
  /* [PM] 4.2.1+ FIXME: Make key u_int32_t? */
  SP_integer key[1];            /* can be larger than 1! */
} iterator;

#if DB_VERSION_MAJOR >= 4
# define LOCK_ID(ENV,A) ((ENV)->lock_id(ENV,A))
# define LOCK_ID_FREE(ENV,A) ((ENV)->lock_id_free(ENV,A))
# define LOCK_GET(ENV,A,B,C,D,E) ((ENV)->lock_get(ENV,A,B,C,D,E))
# define LOCK_PUT(ENV,A) ((ENV)->lock_put(ENV,A))
# define LOCK_DETECT(ENV,A,B,C) ((ENV)->lock_detect(ENV,A,B,C))
#else
# define LOCK_ID(ENV,A) lock_id(ENV,A)
# define LOCK_ID_FREE(ENV,A) 0
# define LOCK_GET(ENV,A,B,C,D,E) lock_get(ENV,A,B,C,D,E)
# define LOCK_PUT(ENV,A) lock_put(ENV,A)
# define LOCK_DETECT(ENV,A,B,C) lock_detect(ENV,A,B,C)
#endif

#endif /* INCLUDED_NEWDB_H */
