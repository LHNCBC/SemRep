/*==========================================================

%SOURCE FILE
        nls_clnt.c

%DESCRIPTION OF FILE
        C source file.

%REVISED
        10Oct95 divita -- Initial Version
	19Nov03 kilicoglu -- Removed the debugging stuff 
	                     that causes problems with large
			     texts sent over sockets.
        14Apr04 kilicoglu -- Removed all debugging			     

%%
==========================================================*/



/*----------------------------
%INCLUDES
----------------------------*/
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <debug.h>

#include <nls_srvr.h>

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#define MAX_CONNECTIONS  20
/*end of constants ---------*/

/*----------------------------
%MACROS
----------------------------*/
/*end of macros ------------*/

/*----------------------------
%STATIC FUNCTIONS
----------------------------*/
/*end of static functions --*/

/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/
extern char * cfg_get( char * );
extern int nls_open_connection(char *, int );
extern int nls_get_next_line(int, char *, int );
extern int nls_post_query(int, char * );
extern int not_smart_concat ( char **, char *, int * );
extern int nls_close_connection( int );

/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/

/* int a_wsd_client(char *a_query,     */  /* Input  */
/*              char **a_response  */  /* Output */
/* 	     ); */
int wsd_client(char *host,             /* Input  */
	       ushort tcp_port,        /* Input  */
	       char *server_delimiter, /* Input */
	       char *a_query,          /* Input  */
	       char **a_response       /* Output */
	       );
int wsd_client_stop(char     *host,    /* Input */
                    ushort   tcp_port  /* Input */
		    );
int wsd_client_catalog(char     *host,    /* Input */
                   ushort   tcp_port  /* Input */
		   );

/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/*end of global variables --*/


/*----------------------------
%PRIVATE STRUCTURES
----------------------------*/
typedef struct _x {
  char host[20];
  int  port;
  int  socket;
} Connect_structure;

static  Connect_structure catalog_list[MAX_CONNECTIONS];

/*end of private structures */

/*----------------------------
%TYPEDEFS
----------------------------*/
/*end of typedefs ----------*/

/*----------------------------
%DEBUG FLAGS
----------------------------*/

#define DT402  402          /* DT for a_client() */
#define DF403  403          /* DF for a_client() */
#define DT486  486          /* DT for nls_client() */
#define DF487  487          /* DF for nls_client() */
#define DT576  576          /* DT for nls_client_stop() */
#define DF577  577          /* DF for nls_client_stop() */
#define DT578  578          /* DT for client_cataloged() */
#define DF579  579          /* DF for client_cataloged() */
#define DT580  580          /* DT for catalog_client() */
#define DF581  581          /* DF for catalog_client() */
/*end_of_debug_flags---------*/

/*----------------------------
%SCCS MARKER
----------------------------*/
static char sccs_id_wsd_clnt_c[] = "@(#)wsd_clnt.c	1.2 11/02/06";


/**/
/*==========================================================
%FUNCTION NAME
        a_client
%PURPOSE
        A generic client to that talks with a generic server
        mallocs space, and returns the server's response.

        Remember to deallocate the space when you are finished.
%SYNTAX
        void  a_client ( char *a_query, char** a_response )
%EXAMPLE CALL
         char *response  = NULL;
         a_client("a test",&response);
         CHAR_FREE ( response );
%RETURNS
        N/A
%SCOPE
        public
%NEEDED INCLUDES
        #include ""
%METHOD
        
%FILES
        
%TABLES
        
%NOTES
        
%BUGS
        
%FLAGS  
        TRACE DT402
        FULL  DF403
%HEADER END
==========================================================*/
/* int a_wsd_client( */
/*              char *a_query,       */
/*              char **a_response */
/*              ) */

/* { */
/*   int    return_code = D_S_SUCCESS; */
/*   int    a_socket = -1; */
/*   char   response[SRVR_BUFFER_SIZE]; */
/*   char   *tmp_response = NULL; */
/*   ushort tcp_port = 0; */
/*   int    response_size = 0; */
/*   char   *host = NULL; */
/*   int    allocated_space = 0; */
/*   char   *server_delimiter = NULL; */
/*   int    not_done = D_TRUE; */
/*   int    ret_code = -40; */

/*   DFNAME("a_wsd_client"); */
/*   DENTER(DT402); */

/*   server_delimiter = cfg_get ("SERVER_DELIMITER"); */

/*   tcp_port = (ushort) atoi ( cfg_get ( "SERVER_DEFAULT_TCP_PORT" ) ); */
/*   sprintf(msg,"going to send to %d", tcp_port ); */
/*   DPR(DF403,msg); */

 
/*   if ( (  host = cfg_get ( "SERVER_HOST" ) ) == NULL ) */
/*   { */
/*     sprintf(msg,"The SERVER_HOST has not been set in the configuration file"); */
/*     return_code = D_E_ERROR; */
/*     DPE(msg); */
/*     goto bottom ; */
/*   } */
  
  
/*   sprintf(msg,"host = {%s}",host );DPR(DF403,msg); */
/*   if((a_socket=nls_open_connection(host, tcp_port)) == -1 ) */
/*   { */
/*     sprintf(msg, */
/*             "Not able to connect to the server %s at port %d", */
/*             host, */
/*             tcp_port); */
/*     DPE(msg); */
/*     return_code = D_E_ERROR; */
/*     goto bottom ; */
/*   } */

/*   sprintf(msg,"The sockid = %d, query = {%s}",a_socket,a_query );DPR(DF403,msg); */

/*   if (return_code = nls_post_query(a_socket, a_query) < 0) */
/*   { */
/*     sprintf(msg,"Error posting the query %s= %d",a_query,return_code); */
/*     return_code = D_E_ERROR; */
/*     DPE(msg); */
/*     goto bottom ; */
/*   } */
/*   sprintf(msg,"posted the query %s= %d",a_query,return_code); */
/*   DPR(DF403,msg); */

/*   sprintf(msg,"Just posted the query, haven't gotten a response yet"); */
/*   DPR(DF403,msg); */
/*   response_size = sizeof( response ); */

/*   not_done = D_TRUE; */
/*   strcpy(response,""); */
/*   while (((ret_code =nls_get_next_line(a_socket, response, response_size)) > 0)  && */
/*          ( not_done == D_TRUE )) */
/*   { */
/* sprintf(msg,"Response: %s\n",response);   DPR(DF403,msg);  */
/*     if ( strstr ( response, server_delimiter ) != NULL ) */
/*     { */
/*       not_done = D_FALSE; */
/*       sprintf(msg,"Caught the server side delimiter, ending"); */
/*       DPR(DF403,msg); */
/*     } */


/*     not_smart_concat ( &tmp_response, response, &allocated_space ); */
/*     sprintf(msg,"received-> %s\n",response); */
/*     DPR(DF403,msg); */
    
/*     strcpy(response,""); */

/*   } */
/*   sprintf(msg,"The last return code from nls_get next line = %d", ret_code ); */
/*   DPR(DF403,msg); */

/*  bottom: */

/*   *a_response = tmp_response ; */
/*   nls_close_connection(a_socket); */

/*   DEXIT(DT402); */
/*   return ( return_code ); */

/* } */ /*** End a_client */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
        nls_client
%PURPOSE
        ?
%USAGE
%SYNTAX
        ?
%EXAMPLE CALL
        ?ret_val = nls_client(arg);
%RETURNS
        ?
%SCOPE
        ?public | private | static
%NEEDED INCLUDES
        #include "?"
%METHOD
        ?
%FILES
        ?
%TABLES
        ?
%NOTES
        ?
%BUGS
        ?
%FLAGS  
        TRACE DT486
        FULL  DF487
%HEADER END
==========================================================*/
int  wsd_client(
		char *host,             /* Input  */
		ushort tcp_port,        /* Input  */
		char *server_delimiter, /* Input */
		char *a_query,          /* Input  */
		char **a_response       /* Output */
               )
   
{
  int    return_code = D_S_SUCCESS;
  int    a_socket = -1;
  char   response[SRVR_BUFFER_SIZE];
  char   *tmp_response = NULL;
  int    response_size = 0;
  int    allocated_space = 0;
  char   *server_query = NULL;
  int    not_done = D_TRUE;
  int    ret_code = -40;


/*   DFNAME("wsd_client"); */
/*   DENTER(DT486); */

/*   sprintf(msg,"going to send to %d", tcp_port ); */
/*   DPR(DF487,msg); */

 
  if ( host == NULL )
  {
/*     sprintf(msg,"The SERVER_HOST has not been set in the configuration file"); */
    return_code = D_E_ERROR;
/*     DPE(msg); */
    goto bottom ;
  }
/*   sprintf(msg,"host = {%s}",host );DPR(DF487,msg); */
  
  a_socket = wsd_client_catalog ( host, tcp_port );
  

/* ------------------------------------------------
   Add the client delimiter to the end of the query
   ------------------------------------------------ */

  server_query = (char *) malloc(sizeof(char) * (strlen(a_query)+40));
  sprintf(server_query,"%s\n^THE_END^\n",a_query);

  if(a_socket == -1 )
  {
/*     sprintf(msg, */
/*             "Not able to connect to the server %s at port %d", */
/*             host, */
/*             tcp_port); */
/*     DPE(msg); */
    return_code = D_E_ERROR;
    goto bottom ;
  } 

/*   sprintf(msg,"The sockid = %d, query = {%s}",a_socket,server_query );DPR(DF487,msg); */

  if (ret_code = nls_post_query(a_socket, server_query) < 0)
  {
/*     sprintf(msg,"Error posting the query %s= %d",server_query,return_code); */
    return_code = D_E_ERROR;
/*     DPE(msg); */
    goto bottom ;
  }
/*   sprintf(msg,"posted the query %s= %d",server_query,return_code);  */
/*   DPR(DF487,msg); */

/*   sprintf(msg,"Just posted the query, haven't gotten a response yet"); */
/*   DPR(DF487,msg); */
  response_size = sizeof( response );

  not_done = D_TRUE;
  while (((ret_code = nls_get_next_line(a_socket, response, response_size)) > 0) && 
         ( not_done == D_TRUE ) 
	 )
  {
/* sprintf(msg, "Response: %s", response); DPR(DF403,msg);  */   
    if (( strstr( response, server_delimiter ) != NULL ) ||
        ( strstr( response, "%%") != NULL ))
    {
      not_done = D_FALSE;
/*       sprintf(msg,"Caught the server side delimiter, ending,{%s}",response); */
/*       DPR(DF403,msg); */
    }

    
    not_smart_concat ( &tmp_response, response, &allocated_space );
/*     sprintf(msg,"received-> %s\n",response); */
/*     DPR(DF487,msg); */
    
  }

/*   sprintf(msg,"The last return code = %d", ret_code ); DPR(DF487,msg); */
 bottom:

  *a_response = tmp_response ;

  CHAR_FREE ( server_query );

/*   DEXIT(DT486); */
  return ( return_code );

} /*** End nls_client */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
        nls_client_stop
%PURPOSE
        ?
%USAGE
%SYNTAX
        ?
%EXAMPLE CALL
        ?ret_val = nls_client_stop(arg);
%RETURNS
        ?
%SCOPE
        ?public | private | static
%NEEDED INCLUDES
        #include "?"
%METHOD
        ?
%FILES
        ?
%TABLES
        ?
%NOTES
        ?
%BUGS
        ?
%FLAGS  
        TRACE DT576
        FULL  DF577
%HEADER END
==========================================================*/
int wsd_client_stop(
                    char     *host,    /* Input */
                    ushort   tcp_port  /* Input */
                    )

{
  int return_code = D_S_SUCCESS;
  int i = 0;
  int not_found = D_TRUE;

/*   DFNAME("wsd_client_stop"); */
/*   DENTER(DT576); */

  while (( not_found == D_TRUE) && ( i < MAX_CONNECTIONS ))
  {
    if (( catalog_list[i].port == tcp_port ) &&
        ( strcmp(catalog_list[i].host,host ) == 0 ))
    {
/*       sprintf(msg,"About to close the connection for %d", catalog_list[i].socket); */
/*       printf("About to close the connection for %d", catalog_list[i].socket); */
/*       DPR(DF577,msg); */
      nls_close_connection(catalog_list[i].socket );
      catalog_list[i].socket  = -1;
      not_found = D_FALSE;
    }
    else
    {
      i++;
    }
  }

/*   DEXIT(DT576); */
  return ( return_code );

} /*** End nls_client_stop */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
        client_catalog
%PURPOSE
        ?
%USAGE
%SYNTAX
        ?
%EXAMPLE CALL
        ?ret_val = client_catalog(arg);
%RETURNS
        ?
%SCOPE
        ?public | private | static
%NEEDED INCLUDES
        #include "?"
%METHOD
        ?
%FILES
        ?
%TABLES
        ?
%NOTES
        ?
%BUGS
        ?
%FLAGS  
        TRACE DT578
        FULL  DF579
%HEADER END
==========================================================*/
int wsd_client_catalog(
                   char     *host,    /* Input */
                   ushort   tcp_port  /* Input */
                   )
   
{
  int return_code = -1;
  static int current_connections = 0;
  int not_found = D_TRUE;
  int i = 0;
  int a_socket = -1;

/*   DFNAME("wsd_client_catalog"); */
/*   DENTER(DT578); */


  while (( not_found == D_TRUE     ) && 
	 ( i < current_connections ) && 
	 ( i < MAX_CONNECTIONS     ) 
	 )
  {
    if (( catalog_list[i].port == tcp_port ) &&
        ( strcmp(catalog_list[i].host,host ) == 0 ))
    {
      return_code = catalog_list[i].socket ;
      not_found = D_FALSE;
    }
    else
    {
      i++;
    }
  }

  if ( not_found == D_TRUE )
  {

    if((a_socket=nls_open_connection(host, tcp_port)) == -1 )
    {
/*       sprintf(msg, */
/*               "Not able to connect to the server %s at port %d", */
/*               host, */
/*               tcp_port); */
/*       DPE(msg); */
      return_code = -1;
      goto bottom ;
    }

/*     sprintf(msg,"The sockid = %d,",a_socket ); DPR(DF579,msg); */

    catalog_list[current_connections].port = tcp_port;
    strcpy(catalog_list[current_connections].host,host );
    catalog_list[current_connections].socket = a_socket;
    
    current_connections++;
    return_code = a_socket;
    
  }
  else
  {
    if ( a_socket == -1 )
    {
      if((a_socket=nls_open_connection(host, tcp_port)) == -1 )
      {
/* 	sprintf(msg, */
/* 		"Not able to connect to the server %s at port %d", */
/* 		host, */
/* 		tcp_port); */
/* 	DPE(msg); */
	return_code = -1;
	goto bottom ;
      }
      
/*       sprintf(msg,"The sockid = %d,",a_socket ); DPR(DF579,msg); */
      catalog_list[i].socket = a_socket;
      return_code = a_socket;
    
    }
  }

 bottom:

/*   DEXIT(DT578); */
  return ( return_code );

} /*** End client_cataloged */
/**/
