#ifndef INCLUDED_DBAUX_H
#define INCLUDED_DBAUX_H
#include <stdarg.h>             /* [PM] April 2000 before stdio.h fixes va_list issue on dec-osf */

#include <db.h>
#include <time.h>
#include <sicstus/sicstus.h>
#include <sicstus/config.h>

#if SP_WIN32
#include <stdlib.h>

struct tms {
  clock_t tms_utime;		/* user time */
  clock_t tms_stime;		/* system time */
  clock_t tms_cutime;		/* user time, children */
  clock_t tms_cstime;		/* system time, children */
};

#endif /* SP_WIN32 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#if SP_WIN32
extern int times(struct tms *buffer);  
#endif /* SP_WIN32 */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* INCLUDED_DBAUX_H */
