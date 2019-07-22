/*==========================================================

%SOURCE FILE
	nls_srvr.h

%DESCRIPTION OF FILE
	C include file.

%REVISED
	10Oct95 divita -- Initial Version

%%
==========================================================*/


/* 
   prevent sccs_id_nls_srvr_h from being re-defined in case
   nls_srvr.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_nls_srvr_h
static char sccs_id_nls_srvr_h[] = "@(#)nls_srvr.h	1.2 09/27/06";
#define sccs_id_nls_srvr_h 1
#endif

#ifndef _SRVR_INET_H

#define _SRVR_INET_H

/*----------------------------
%INCLUDES
----------------------------*/
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#define MAX_HOSTNAME_LENGTH 128
#define SRVR_BUFFER_SIZE 4096
#define SRVR_BACKLOG 10

/*end of constants ---------*/

/*----------------------------
%MACROS
----------------------------*/
/*end of macros ------------*/

/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/
/*end of static functions --*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/*end of global variables --*/

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
/*end_of_function_prototypes*/

/*----------------------------
%PRIVATE STRUCTURES
----------------------------*/
/*end of private structures */

/*----------------------------
%TYPEDEFS
----------------------------*/
/*end of typedefs ----------*/

/*----------------------------
%SCCS MARKER
----------------------------*/

#endif				/* _SRVR_INET_H */
