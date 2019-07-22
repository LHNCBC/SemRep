/* wsd_meta_srvr_inet.h - wsd network params

	@(#)wsd_meta_inet.h	1.1	09/28/06
*/

#ifndef sccs_id_wsd_meta_inet_h
static char sccs_id_wsd_meta_inet_h[] = "@(#)wsd_meta_inet.h	1.1 09/28/06";
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

#define META_SRVR_DEFAULT_TCP_PORT 8003
#define META_SRVR_BACKLOG 10
#define MAX_HOSTNAME_LENGTH 128

#endif				/* _META_SRVR_INET_H */
