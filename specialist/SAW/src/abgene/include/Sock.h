#ifndef SOCK_H
#define SOCK_H

#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <unistd.h>
#include <memory.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <netdb.h>
// #include <sys/systeminfo.h>

const int BACKLOG_NUM = 5;

class Sock {
     private:
    	int sid;     	// socket descriptor
        int domain; 	// socket domain
        int socktype;   // socket type
        int rc;      	// member function return status code

	/* Build a socket name based on the given hostname and port number */
	int constr_name(struct sockaddr_in&, const char*, int);

        /* Convert an IP address to a host name */
        char* ip2name(const struct in_addr);

     public:
	Sock();
     	Sock(int, int, int);
        ~Sock();			// destructor
	int fd();	// return the socket file descriptor
        int good();			// check sock object status
	int bind(const char*, int); 	// assign a UNIX name
        int accept(char*, int*);
       	int connect(const char*, int port); 	// connect to a UNIX socket
        int write(const char*, int, int, int); 	// write a msg
        int read( char*, int, int, int); 	// read a msg
       	int shutdown(int); 	// shutdown a socket
}; 		/* class sock */

#endif
