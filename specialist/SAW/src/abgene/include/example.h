#ifndef EXAMPLE
#define EXAMPLE


/*
** example.h
**
** This is the header file that goes with the Sybase 
** Client-Library example programs.  
**
**
*/

/* Sccsid %Z% %M% %I% %G% */

/*
** Define symbolic names, constants, and macros
*/
#define	EX_MAXSTRINGLEN		255
#define	EX_BUFSIZE		1024
#define	EX_CTLIB_VERSION	CS_VERSION_110
#define	EX_BLK_VERSION		BLK_VERSION_110
#define	EX_ERROR_OUT		stderr

/*
** exit status values
*/
#ifdef vms
#include <stsdef.h>
#define EX_EXIT_SUCCEED		(STS$M_INHIB_MSG | STS$K_SUCCESS)
#define EX_EXIT_FAIL		(STS$M_INHIB_MSG | STS$K_ERROR)
#else
#define EX_EXIT_SUCCEED		0
#define EX_EXIT_FAIL		1
#endif /* vms */

/*
** Define global variables used in all sample programs
*/
/* #define EX_SERVER 	NULL	 */	/* use DSQUERY env var */
#define EX_SERVER 	"STRAUSS"
#define EX_USERNAME	"anyone"
#define EX_PASSWORD 	"allowed"

/*
** For some platforms (e.g. windows 3.1), additional work needs to be done
** to insure that the output of some of the example programs can be displayed.
** This macro will insure that any setup is done for the platform.
**
** For windows, _wsetscreenbuf(_fileno(stdout), _WINBUFINT) will set 
** QuickWin's standard output screen buffer to unlimited size.  
*/

#if QWIN

#define EX_SCREEN_INIT() _wsetscreenbuf(_fileno(stdout), _WINBUFINF)

#else /* QWIN */

#define EX_SCREEN_INIT() 

#endif /* QWIN */

#endif EXAMPLE
