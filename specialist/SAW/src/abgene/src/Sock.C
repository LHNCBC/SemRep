#include <fstream>
#include "Sock.h" 
using namespace std;

int Sock::constr_name(struct sockaddr_in& addr, const char* hostnm, int port )
{
           addr.sin_family = domain;
            if (!hostnm) 
           	addr.sin_addr.s_addr = INADDR_ANY;
            else {
	      	struct hostent *hp = gethostbyname(hostnm);
            	if (hp==0) {
                	perror("gethostbyname"); return -1;
            	}

              	memcpy((char*)&addr.sin_addr,(char*)hp->h_addr, hp->h_length);
           }
           addr.sin_port = htons(port);
           return sizeof(addr);
}

/* Convert an IP address to a host name */
char* Sock::ip2name(const struct in_addr in)
{
         u_long laddr;
         if ((int)(laddr = inet_addr(inet_ntoa(in))) == -1) return 0;
         struct hostent *hp = gethostbyaddr((char *)&laddr, sizeof (laddr), AF_INET);
         if (hp == NULL) return 0;
         for (char **p = hp->h_addr_list; *p != 0; p++) {
              (void) memcpy((char*)&in.s_addr, *p, sizeof (in.s_addr));
              if (hp->h_name) return hp->h_name;
         }
         return 0;
}

Sock::Sock() {
} 

Sock::Sock(int dom, int type, int protocol)
{
         if ((sid=socket(domain=dom, socktype=type,protocol))<0)
            perror("socket");
}
Sock::~Sock()  
{ 
	shutdown(2); 
	close(sid);  
}
			
int Sock::fd()	
{ 
	return sid; 
}

int Sock::good()    
{ 
	return sid >= 0;  
}			
	
int Sock::bind(const char* name, int port) 
{
	 ofstream fout;
         fout.open("/net/ray/web/private/htdocs/Rebase/THREAD_TEST/doltestdb/port.out", ios::out);
	
	 struct sockaddr_in addr;
         socklen_t len = constr_name( addr, name, port);
         if ((rc= ::bind(sid, (struct sockaddr *)&addr, len))<0  ||
                (rc=getsockname(sid, (struct sockaddr*)&addr, &len))<0)
                perror("bind or getsockname");
         else {
	     cerr << "Socket port: " << ntohs(addr.sin_port) << endl;
             fout<<ntohs(addr.sin_port)<<endl;
	 }
         fout.close();	     
         if (rc!=-1 && socktype!=SOCK_DGRAM && (rc=listen(sid,BACKLOG_NUM)) < 0) 
          	perror("listen");
	 return rc;
}     

int Sock::accept(char* name, int* port_p)
{
         if (!name) return ::accept(sid, 0, 0);
 	 struct sockaddr_in addr;
	 socklen_t size = sizeof (addr);
         if ((rc = ::accept( sid, (struct sockaddr*)&addr, &size)) >-1)
         {
	    if (name) strcpy(name,ip2name(addr.sin_addr));
	    if (port_p) *port_p = ntohs(addr.sin_port);
         }
         return rc;
}

int Sock::connect(const char* hostnm, int port)  // connect to a UNIX socket
{
         struct sockaddr_in addr;
         int len = constr_name( addr, hostnm, port);
         if ((rc= ::connect(sid,(struct sockaddr *)&addr,len))<0) perror("bind");
         return rc;
}

int Sock::write(const char* buf, int len, int flag=0, int nsid=-1 ) 	
{
         return ::send(nsid==-1 ? sid : nsid, buf, len, flag );
}	

int Sock::read(char* buf, int len, int flag=0, int nsid=-1 ) 	// read a msg
{
         return ::recv(nsid==-1 ? sid : nsid, buf, len, flag );
}

int Sock::shutdown(int mode = 2 ) 	// shutdown a socket
{       
         return ::shutdown (sid,mode);
}
