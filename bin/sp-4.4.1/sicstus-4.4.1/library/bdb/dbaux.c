#include "dbaux.h"

#include <ctype.h>

#include <string.h>
#include <sys/types.h>          /* [PM] 3.11.2 needed for _stat on Win32 says docs */
#include <sys/stat.h>
#if HAVE_UNISTD_H
#include <unistd.h>
#endif /* HAVE_UNISTD_H */
#if SP_WIN32
#include <direct.h>             /* _mkdir */
#undef stat
#define stat _stat              /* [PM] 3.11.2 Win32 uses _stat() and struct _stat */
#endif  /* SP_WIN32 */

#if SICSTUS_RELEASE_BUILD  && !FORCE_BUILD && SICSTUS_VERSION >= 40300
/* [PM] 4.3 Has this been fixed already? */
#error "[PM] 4.0 FIXME BDB: use SICStus/SPIO OS functions instead of home-brewed lossage"
#endif  /* SICSTUS_TODO */

/* [PM] (This used to mention SPRM 4917 but that PRM is not about BDB at all)

   Using a custom hash function is bad because it makes some of the
   standard tools (e.g., db_stat) unusable and makes it harder to
   access the data base tables directly through the various Berkeley
   DB APIs.

   Using the identity hash function below has the following additional problems:
   . Assumes size >= 4. There is no guarantees that this will be
     true. BDB may call the hash function for its own purposes (and
     does, although with a key longer than 4 bytes).
   . Assumes the key is suitably aligned so that reading a 4byte word
     will not trap. There is no reason to believe this will always be
     true.
   . The hash function depends on endianess which makes the data base
     platform dependant for no good reason.

   Unless there are compelling advantages with using a custom hash
   function we should instead rely on the built in
   default. (Unfortunately one compelling reason may be backward
   compatibility...)

*/

#ifdef STAT
#if SP_WIN32
#include <windows.h>

int times(struct tms *buffer)
{
    static int initialized = FALSE;
    static FARPROC proc;
    FILETIME Createtm, Exittm, Kerneltm, Usertm;

    buffer->tms_cutime = 0; /* child process user time (N/A) */
    buffer->tms_cstime = 0; /* child process system time (N/A) */

    if (!initialized) {
	/* TNT extender doesn't include GetProcessTimes */
	HINSTANCE hnd = GetModuleHandle("kernel32.dll");

	proc = hnd ? GetProcAddress(hnd, "GetProcessTimes") : NULL;
	initialized = TRUE;
    }

    /* Get 64 bit process time representing 100ns units */
    if (!proc ||
	!(*proc)(GetCurrentProcess(), &Createtm, &Exittm, &Kerneltm, &Usertm))
    {   /* GetProcessTimes() not supported */
	buffer->tms_utime = clock();	/* user time */
	buffer->tms_stime = 0;		/* system time */
	return 0;
    }

    /* convert process time to number of elasped milliseconds */
    buffer->tms_utime  = Usertm.dwHighDateTime * 429496.7296;
    buffer->tms_utime += Usertm.dwLowDateTime / 10000;
    buffer->tms_stime  = Kerneltm.dwHighDateTime * 429496.7296;
    buffer->tms_stime += Kerneltm.dwLowDateTime / 10000;

    return 0;
}
#endif
#endif /* STAT */

#include "bdb.h"
#include "bdb_glue.h"


