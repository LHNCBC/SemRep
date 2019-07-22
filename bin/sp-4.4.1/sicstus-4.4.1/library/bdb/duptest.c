/*
  build:
  [g]cc duptest.c -o duptest -I/usr/local/BerkeleyDB/include \
     /usr/local/BerkeleyDB/lib/libdb.so
*/

#include <sys/types.h>
#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>             /* [PM] 3.9.1 !! See bdb.c */
#include <db.h>

#include "ltimer.h"

#define MAX_KEYL (120*4)
#define MAX_KEYN 1000

#define DATAHOME   "/tmp"
#define DATABASE   "duptest.db"
#define MAXDATALEN 256

/* [PM] 4.0 not a good idea since 1970:
   extern int errno;
*/

int put(DB *dbp, int i, u_int32_t k, u_int32_t d)
{
  DBT key, data;
  int ret;
  
  memset(&key, 0, sizeof(key));
  memset(&data, 0, sizeof(data));
  key.data = &k;
  key.size = sizeof(u_int32_t);
  data.data = &d;
  data.size = sizeof(u_int32_t);

  if ((errno = dbp->put(dbp, NULL, &key, &data, 0)) != 0) {

#error "[PM] 4.0 strerror is not thread safe. use strerror_r instead"
    fprintf(stderr, "db: put: %s\n", strerror(errno));
    return 1;
  }

  return 0;
}

u_int32_t identity(const void *key, u_int32_t size)
{
  return *(u_int32_t *)key;
}

int main(int argc)
{
  DB *dbp;
  DB_INFO dbinfo;
  DB_ENV dbenv;
  int ret = 0, delta = 1, j, i;
  u_int32_t key = 0, data = 0;

  memset(&dbenv, 0, sizeof(DB_ENV));
  if ((errno = db_appinit(DATAHOME, NULL, &dbenv,
                          DB_CREATE|DB_INIT_LOCK|DB_INIT_MPOOL)) != 0) {
#error "[PM] 4.0 strerror is not thread safe. use strerror_r instead"
    fprintf(stderr, "db: %s: %s\n", DATAHOME, strerror(errno));
    return 1;
  }

  if ((errno = LOCK_DETECT(dbenv, dbenv.lk_info, DB_LOCK_CONFLICT, 0)) != 0) {
#error "[PM] 4.0 strerror is not thread safe. use strerror_r instead"
    fprintf(stderr, "db: %s\n", strerror(errno));
    ret = 1;
    goto exit;
  }

  memset(&dbinfo, 0, sizeof(DB_INFO));
  dbinfo.h_hash = identity;
  if (argc > 1) {
    /* sorting speeds up things (3/4), which is quite a wonder */
    dbinfo.flags = DB_DUP|DB_DUPSORT;
    delta = 0;
    fprintf(stderr, "storing duplicates...\n");
  }

  if ((errno = db_open(DATABASE, DB_HASH, DB_CREATE, 0664,
                       &dbenv, &dbinfo, &dbp)) != 0) {
#error "[PM] 4.0 strerror is not thread safe. use strerror_r instead"
    fprintf(stderr, "db: %s: %s\n", DATABASE, strerror(errno));
    ret = 1;
    goto exit;
  }

  for (j = 1; j <= MAX_KEYL; ++j)
    STAT(for (i = 1; i <= MAX_KEYN; ++i, key += delta, ++data)
         if (put(dbp, i, key, data)) goto close_db,
         fprintf(stderr, "#%.3d: ", j));

close_db:
  if ((errno = dbp->close(dbp, 0)) != 0) {
    fprintf(stderr, "db: close: %s\n", strerror(errno)); /* [PM] strerror not thread safe (but not using threads here) */
    ret = 1;
  }

exit:
  db_appexit(&dbenv);
  return ret;
}
