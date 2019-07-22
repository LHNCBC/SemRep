/* meta_srvr_inet.h - network params

*/


/* 
   prevent sccs_id_meta_inet_h from being re-defined in case
   meta_inet.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_meta_inet_h
static char sccs_id_meta_inet_h[] = "@(#)meta_inet.h	1.2 09/27/06";
#define sccs_id_meta_inet_h 1
#endif

#ifndef _META_SRVR_INET_H

#define _META_SRVR_INET_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <errno.h>

#define META_SRVR_DEFAULT_TCP_PORT 8002
#define META_SRVR_BACKLOG 10
#define MAX_HOSTNAME_LENGTH 128
#define CSB1 "csb1.nlm.nih.gov"
#define NLS6 "nls6.nlm.nih.gov"

#endif				/* _META_SRVR_INET_H */
