/* network utility functions - from Stevens, Chapter 6.
*/

#include <stdio.h>
#include <string.h>
#include "meta_inet.h"

#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>

#define END_OF_QUERY	"%%\n"
#define QUERYSIZE 1024


int nls_sock_read();
int nls_sock_write();
int nls_sock_fgets();
void nls_hostname_to_addr();
void nls_trim_crlf( char *s );
void nls_fatal ();

static struct sockaddr_in serverAddr;

static char sccs_id_com_utils_c[] = "@(#)com_utils.c	1.1 10/06/06";

/*  fatal exit
*/
void nls_fatal(char *message )
{
    fprintf(stderr, "%s.\n", message);
    /*exit(2);*/
}
 
/*  trims trailing CR/LF, if any
*/
void nls_trim_crlf( char *s )
{
    char *cp = s+strlen(s);
 
    if (cp-- > (s+1)) {
        if (*cp == '\n')
            *cp-- = '\0';
        if (*cp == '\r')
            *cp-- = '\0';
    }
}

/*  reads a buffer from a socket
*/
int nls_sock_read(int sockfd, char *buf, int n) 
{
    int nLeft;
    int nRead;

    nLeft = n;
    while (nLeft > 0) {
	nRead = read(sockfd, buf, nLeft);
	if (nRead < 0)
	    return(nRead);
	else if (nRead == 0)
	    break;
	nLeft -= nRead;
	buf += nRead;
    }
    return(n-nLeft);
}

/*  writes a buffer to a socket.
*/
int nls_sock_write(int sockfd, char *buf, int n ) 
{
    int nLeft;
    int nWritten;

    nLeft = n;
    while (nLeft > 0) {
	nWritten = write(sockfd, buf, nLeft);
	if (nWritten <= 0)
	    return(nWritten);
	nLeft -= nWritten;
	buf += nWritten;
    }
    return(n-nLeft);
}

/*  same as fgets(3) but on a socket
*/
int nls_sock_fgets(int sockfd, char *buf, int max )
{
    int n;
    int rc;
    char c;

    for (n=1; n<max; n++) {
	if ((rc = read(sockfd, &c, 1)) == 1) {
	    *buf++ = c;
	    if (c == '\n')
		break;
	}
	else if (rc == 0) {
	    if (n == 1)
		return(0);
	    else
		break;
	}
	else
	    return(-1);
    }
    *buf = 0;
    return(n);
}

/*  converts hostname to an address
*/
void nls_hostname_to_addr( char *host, struct in_addr *addr )
{
    struct hostent *hp;
    hp = NULL;

    hp = gethostbyname(host);
    if (hp == 0) {
	char s[256];

	sprintf(s, "Unknown host: %s", host);
	nls_fatal(s);
	exit(2);
    }
    memcpy((void *)addr, (void *)hp->h_addr,  hp->h_length);
}

/*
 * nls_close_connection closes the default connection to server.
 */

int nls_close_connection(int ClientSock)
{
	if(close(ClientSock)==-1)
		return(-1);
	else
		return(1);
}


/* 
 * nls_get_next_line requests the next line from the result-set
 * using the default connection
 */

int nls_get_next_line(int ClientSock, char *buf, int  bufsize )
{
	if (nls_sock_fgets(ClientSock, buf, bufsize) <= 0)
	{
	  
	  return(-2);
	}
	if (strcmp(buf,END_OF_QUERY)==0) 
	{
	  return(-1);
	}
	if (buf[0]==END_OF_QUERY[0]) /* query separator */
	  if (buf[1]==END_OF_QUERY[1]) /* query separator */
	    return(nls_get_next_line(ClientSock, buf, bufsize));
	return(1);
}

/* 
 * nls_open_connection establishes a connection and returns the socket_id
 */

int nls_open_connection(char *serverName, int serverPort )
{

    int ClientSock;
    /* establish server params */
    /*bzero((char *) &serverAddr, sizeof(serverAddr));*/
    memset((void *) &serverAddr, 0, sizeof(serverAddr));
    serverAddr.sin_family = AF_INET;
    serverAddr.sin_port = htons(serverPort);
    nls_hostname_to_addr(serverName, &serverAddr.sin_addr);

    /* create socket */
    if ((ClientSock = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
	nls_fatal("Cannot create socket");

	switch(errno) {
	   case EACCES: nls_fatal("socket errno: EACCES");  break;
	   case EMFILE: nls_fatal("socket errno: EMFILE");  break;
	   case ENFILE: nls_fatal("socket errno: ENFILE");  break;
	   case ENOBUFS: nls_fatal("socket errno: ENOBUFS");  break;
	   case EPROTONOSUPPORT: nls_fatal("socket errno: EPROTONOSUPPORT");  break;
	   case EPROTOTYPE: nls_fatal("socket errno: EPROTOTYPE");  break;
	   default: nls_fatal("socket errno: unknown");
	}
	exit(2);
    }
    if (connect(ClientSock, (struct sockaddr *) &serverAddr, sizeof(serverAddr)) < 0) {
	/*nls_fatal("Cannot connect to server");*/
	close(ClientSock);
	return(-1);
    }
    return(ClientSock);
}
/*****************************************************************/

/* 
 * nls_post_query simply writes the query onto the socket
 */

int nls_post_query(int ClientSock, char *query )
{
	if (nls_sock_write(ClientSock, query, strlen(query)) <= 0)
	    return(-1);
	else
	    return(1);
}

