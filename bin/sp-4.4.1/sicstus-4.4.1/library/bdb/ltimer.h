#ifndef INCLUDED_LTIMER_H
#define INCLUDED_LTIMER_H

#include <sys/times.h>
#include <time.h>
#include <stdio.h>

#ifndef CLOCKS_PER_SECOND
#define CLOCKS_PER_SECOND CLK_TCK
#endif /* CLOCKS_PER_SECOND */

#define timeop(a, op, b)                 \
  do {                                   \
    (a).tms_utime op= (b).tms_utime;     \
    (a).tms_stime op= (b).tms_stime;     \
    (a).tms_cutime op= (b).tms_cutime;   \
    (a).tms_cstime op= (b).tms_cstime;   \
  } while (0)

#define START_STAT       \
  struct tms ltimer_stv; \
  times(&ltimer_stv)

#define END_STAT(p)          \
  do {                       \
    struct tms ltimer_etv;   \
    times(&ltimer_etv);      \
    timeop(ltimer_etv, -, ltimer_stv);     \
    p;                                     \
    fprintf(stderr, "user: %.5f, sys: %.5f, sum: %.5f\n", \
            ltimer_etv.tms_utime/(double)CLOCKS_PER_SECOND, \
            ltimer_etv.tms_stime/(double)CLOCKS_PER_SECOND, \
            (ltimer_etv.tms_utime+ltimer_etv.tms_stime)/    \
            (double)CLOCKS_PER_SECOND);                     \
  } while (0)

#define STAT(p, p1) \
  do {              \
    START_STAT;     \
    p;              \
    END_STAT(p1);   \
  } while (0)

#endif /* INCLUDED_LTIMER_H */
