
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

/*==========================================================

%SOURCE FILE
	debug.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	9May94 divita -- Initial Version

%%
==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include "debug.h"
#include "derror.h"
/*----------------------------
%STATIC FUNCTIONS
----------------------------*/

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int cgi_debug_init( char *program_name, char *debug_out);
void trace_off(void);
int DPE(char *msg);
int DPR(int flag, char *msg);
int in_flag_list(int flag);
int debug_init(char *program_name);
int debug_term(void);
int DINTERPRET(int  message_code, int  *message_type, char *message);
int DPRN(int flag, char *msg);
int DPM(int message_number);

/*end_of_function_prototypes*/


/*----------------------------
%STATIC GLOBAL VARIABLES
----------------------------*/
static char proposed_function_name[100];
static int  nested_level = 0;
static char function_stack[100][100];
static short flag_list[MAX_FLAGS];
static FILE *log_fp;
static int  d_trace = D_FALSE;
static int  debug_initialized = D_FALSE;

#ifdef __DEBUG
/*----------------------------
%DEBUG FLAGS
----------------------------*/
#define DT170  170          /* DT for DINTERPRET() */
#define DF171  171          /* DF for DINTERPRET() */
/**/

/*==========================================================
%FUNCTION NAME
	PR
%SCOPE
	?public | private | static
%PURPOSE
	?
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	?ret_val = PR(arg);
%RETURNS
	?
%METHOD
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
int DPR(
	int flag,  /* Input - flag this print statement belongs to */
	char *msg  /* Input - msg to print */
	)
{
   int i;
   char nested_tabs[1000];

   if ( debug_initialized == D_FALSE )
   {
     debug_init( NULL );
   }

   strcpy(nested_tabs," ");

   if ((d_trace == D_TRUE ) || ( in_flag_list ( flag ) == D_TRUE ) )
   {
      
      for ( i =0; i < nested_level; i++ )
      {
	 strcat(nested_tabs,"     ");
      }
      fprintf(log_fp,"%s%s\n",nested_tabs,msg);
      fflush(log_fp);
   }

   return ( D_S_SUCCESS );
   

} /*** End DPR */

/*==========================================================
%FUNCTION NAME
	dfname
%SCOPE
	?public | private | static
%PURPOSE
	?
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	?ret_val = dfname(arg);
%RETURNS
	?
%METHOD
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
int dfname(
	   char *function_name /* Input function name */
	   )
{

  if ( debug_initialized == D_FALSE )
  {
    debug_init ( NULL );
  }
  
  strcpy(proposed_function_name, function_name );
  
  /*fprintf(log_fp,"in dfname, name = %s\n",function_name);*/
  return ( D_S_SUCCESS );
  
} /*** End dfname */


/*==========================================================
%FUNCTION NAME
	debug_init
%SCOPE
	?public | private | static
%PURPOSE
	?
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	?ret_val = debug_init(arg);
%RETURNS
	?
%METHOD
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
int cgi_debug_init( 
	       char *program_name,       /* Input */
	       char *debug_out           /* Input (can be null ) */
	       )
{
   FILE *flag_fp = NULL;
   char *debug_out_name = NULL;
   char *flag_file_name = NULL;
   char flag_index_string[100];
   char flag_index_string2[10];
   char s[100];
   int  i = 0;
   int  n = 0;
   int  flag_size = 0;
   int  flag_index_length = 0;
   int  flag_index = 0;
   int  debug_diagnostics_on = D_FALSE;
   char msg[8][255];
   char *prog_name = NULL;
   
   strcpy(flag_index_string,"");
   strcpy(flag_index_string2,"");

   if ( debug_initialized == D_TRUE )
   {
     goto bottom;
   }
   else
   {
     debug_initialized = D_TRUE;
   }


   for (i=0; i<6; i++ ) strcpy(msg[i],"");

/* ----------------------------------------------------
   Get the output filename from the env var "DBG_OUT"
   ---------------------------------------------------- */
   if ((debug_out_name =(char *) getenv("DBG_OUT")) == NULL)
   {
    /* ------------------------------------------------
       None has been defined, use std_err as the output
       ------------------------------------------------ */
     if ( debug_out != NULL )
     {
       debug_out_name = debug_out;
     } 

     if ( debug_out_name != NULL )
     {
       if ( (log_fp=fopen(debug_out_name,"w")) == NULL )
       {
	 log_fp = stderr;
	 if (debug_diagnostics_on == D_TRUE )
         sprintf(msg[0],"setting the debug to stderr \n");
       }
     }
     else
     {
       log_fp = stderr;
       if (debug_diagnostics_on == D_TRUE )
       sprintf(msg[0],"setting the debug to stderr \n");
     }
   }
   else
   {
     if ( debug_out != NULL )
     {
       debug_out_name = debug_out;
       fprintf(stdout,"changed_direction\n");
       fflush(stdout);
     } 
     if ( (log_fp=fopen(debug_out_name,"w")) == NULL )
     {
	 log_fp = stderr;
	 if (debug_diagnostics_on == D_TRUE )
	 sprintf(msg[1],"Unable to send output to %s, using std err instead\n",
		debug_out_name );
      }
     /* fprintf(stdout,"opened %s\n",debug_out_name); */
     /* fflush(stdout); */
   }

   
   /* --------------------------------------------------
      Get the flag filename from the env var "DBG_FLAGS"
      -------------------------------------------------- */
   if ((flag_file_name = (char *)getenv("DBG_FLAGS")) == NULL)
   {
     flag_fp = fopen ( "./debug.fla","r") ;
     if ( flag_fp == NULL )
     {	 
       sprintf(msg[2],"No flag file found \n");
     }
     else
     {
       sprintf(msg[3],"Using local version of flag file \n");
     }
   }
   else
   {
     flag_fp = fopen ( flag_file_name,"r") ;
     if ( flag_fp == NULL )
     {  
       sprintf(msg[4],"Using flag file  %s\n",flag_file_name);
     }
     else
     {
       flag_fp = fopen ( "debug.fla","r");
       if ( flag_fp == NULL )
       {
	 sprintf(msg[5],"No flag file found \n");
       }
       else
       { 
	 sprintf(msg[6],"Using local version of flag file \n");
       }
       
     }
     
   }
   /* ------------------------
      Initialize the flag list
      ------------------------ */
   
   for (i=0; i < MAX_FLAGS;i++ )  flag_list[i]=D_FALSE; 
   
   /* ------------------------
      Read in the active flags 
      ------------------------ */
   flag_size = sizeof( flag_index_string );
   
   if ( flag_fp != NULL )
   {
     while (fgets(flag_index_string,flag_size,flag_fp) != NULL)
     {
       
       flag_index_length = strlen(flag_index_string);
       strncpy(s,flag_index_string,(size_t)flag_index_length-1);
       s[flag_index_length]='\0';
       
       
       
       n = 0;
       flag_index_string2[n]='\0';
       while ((n < flag_index_length) 
	      && ((flag_index_string[n] != '#')    /* comment       */
		  || (flag_index_string[n]!='!'))) /* turn flag off */
       {
	 flag_index_string2[n]=flag_index_string[n];
	 n++;
       }
       flag_index_string2[n]='\0';
       
       flag_index = atoi(flag_index_string2);

       if ( debug_diagnostics_on == D_TRUE )
       fprintf(stderr,"Got flag %d\n",flag_index);
       fflush(stderr);
       
       switch ( flag_index )
       {
	 
       case 1:
	 if ( debug_diagnostics_on == D_TRUE )
	 {
	   fprintf(log_fp, "\tloading (%s) = OFF\n", s);
	   fprintf(log_fp, "Turning off all functions");
	   fflush(log_fp );
	 }
	 break;
	 
       case 2: /* Flag to turn on All Trace functions */
	 for (i=5; i < MAX_FLAGS;i=i+2 )  flag_list[i]=D_TRUE; 
	 fprintf(log_fp, "Turning on all trace functions");
	 fflush(log_fp );
	 break;
	 
       case 3: /* Flag to turn on All functions */
	 for (i=1; i < MAX_FLAGS;i++ )  flag_list[i]=D_TRUE; 
	 fprintf(log_fp, "Turning on all functions");
	 fflush(log_fp );
	 break;
	 
       case 4: /* Reserved for future use */
	 fprintf(log_fp, "4: Reserved for future use");
       case 5: /* Reserved for future use */
	 fprintf(log_fp, "5: Reserved for future use");
       case 6: /* Reserved for future use */
	 fprintf(log_fp, "6: Reserved for future use");
	 break;
	 
       default:
	 if (flag_index < MAX_FLAGS )
	 {
	   if ( debug_diagnostics_on == D_TRUE ) {
	     fprintf(log_fp, "\tloading (%d) = ON\n", flag_index);
	     fflush(log_fp );
	   }
	   flag_list[flag_index]=D_TRUE;
	 }
	 else
	 if ( debug_diagnostics_on == D_TRUE )
	 fprintf(log_fp, "missed this (%d) \n", flag_index);
	 break;
       }
       
     }
     /* -------------------
	Close the flag file
	------------------- */
     fclose ( flag_fp );
   }
   
   if ( debug_diagnostics_on == D_TRUE )
   {
     for (i=0; i<6; i++ )
     fprintf(log_fp,"%s\n",msg[i]);
     fprintf(log_fp, "Exiting int \n");
   }
   
   
   CHAR_FREE ( prog_name );

 bottom:

   fflush(log_fp );
   fflush(stderr);
   return ( D_S_SUCCESS );
   
   
} /*** End debug_init */

/*==========================================================
%FUNCTION NAME
	debug_term
%SCOPE
	?public | private | static
%PURPOSE
	?
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	?ret_val = debug_term(arg);
%RETURNS
	?
%METHOD
	?
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
int debug_term(void)

{
  
  if (( log_fp != NULL ) && ( log_fp != stdin ))
  {
    fclose(log_fp);
  }

  debug_initialized = D_FALSE;

   return(D_S_SUCCESS );
   
} /*** End debug_term */
/**/
/*==========================================================
%FUNCTION NAME
	DINTERPRET
%SCOPE
	?public | private | static
%PURPOSE
	?
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	?ret_val = DINTERPRET(arg);
%RETURNS
	?
%METHOD
	?
%FILES
	?
%TABLES
	?
%FLAGS  
	TRACE DT170
	FULL  DF171
%SYNTAX ARGS
==========================================================*/
int DINTERPRET(
   int  message_code,  /* Input - message number */
   int  *message_type, /* Output - SUCCESS|MESSAGE|WARNING|ERROR|FATAL */
   char *message       /* Output message - expects space already alloc'd */
	       )
{
   int return_code = D_S_SUCCESS;
   int not_found   = D_TRUE;
   int ctr         = 0;
   char msg[255];




   strcpy(message,"");
   while (( ctr < D_NUMBER_OF_MESSAGES ) && (not_found == D_TRUE ))
   {
     sprintf(msg,"ctr = %d ",ctr);
     
     if ( _derror[ctr].key == message_code )
     {
       sprintf(message,"%s:%s",_derror[ctr].neumonic,_derror[ctr].message);
       *message_type = _derror[ctr].message_type;
       not_found = D_FALSE;
     }
     else
     {
       ctr++;
     }
   }

   
   return ( return_code );

} /*** End DINTERPRET */
/**/
/*==========================================================
%FUNCTION NAME
	DPRN
%SCOPE
	public
%PURPOSE
	Writes a message to a debug file
%SYNTAX INCLUDES
	#include "debug.h"
	#include "derrno.h"
%EXAMPLE CALL
        int flag;
        char *msg = "hello";
	ret_val = DPRN(flag,msg);
%RETURNS
	D_S_SUCCESS if it is able to write to the log file
	D_E_DBGLOG  if it is unable to write to the log file
%METHOD
	?
%FILES
	local debug.fla file (set by environement variable ( DBG_FLAGS)
                                if not set, no flags are turned on.

	debug log file   ( set by envirnomnet variable DBG_OUT )
                           if not set, the default is standard error.
%TABLES
	?
%FLAGS  
	TRACE DT178
	FULL  DF179
%SYNTAX ARGS
==========================================================*/
int DPRN(
	 int flag,  /* Input - flag this print statement belongs to */
	 char *msg  /* Input - msg to print */
	 )
{
   int i;
   char nested_tabs[1000];
   int return_code = D_S_SUCCESS;

   strcpy(nested_tabs," ");
   if ( in_flag_list ( flag ) == D_TRUE )
   {
      
     for ( i =0; i < nested_level; i++ )
     {
       strcat(nested_tabs,"     ");
     }
     if ( fprintf(log_fp,"%s%s",nested_tabs,msg) == EOF ) 
     {
       return_code = D_E_DBGLOG;
     }

     if ( fflush(log_fp) == EOF )
     {
       return_code = D_E_DBGLOG;
     }
   }

   return ( return_code );
   


} /*** End DPRN */
/**/
/**/
/*==========================================================
%FUNCTION NAME
	DPM
%PURPOSE
	Given a message Number, write the interpreted message to 
        the error log file.
%SYNTAX
	DPM ( int message_number );
%EXAMPLE CALL
	ret_val = DPM(D_F_ERROR);
%RETURNS
	D_S_SUCCESS if it is able to write to the error log
	D_E_ERRLOG if it is unable to write to the error log
%SCOPE
	public
%NEEDED INCLUDES
	#include "debug.h"
	#include "derrno.h"
%METHOD
	
%FILES
	error_log
%NOTES
	?
%BUGS
	?
%HEADER END
==========================================================*/
int DPM( 
	int message_number  /* Input */
       )
{
   int return_code = D_S_SUCCESS;
   char message[MAXLINE];
   int message_type;
   char message_type_string[10];

   if ( debug_initialized == D_FALSE )
   {
     debug_init( NULL );
   }

   DINTERPRET(message_number,&message_type,message);
   switch ( message_type )
   {
   case SUCCESS: strcpy(message_type_string,"SUCCESS");
   case MESSAGE: strcpy(message_type_string,"MESSAGE");
   case WARNING: strcpy(message_type_string,"WARNING");
   case ERROR:   strcpy(message_type_string,"ERROR");
   case FATAL:   strcpy(message_type_string,"FATAL");
   }

   fprintf(stderr,"%s:%s\n",message_type_string,message);
   fflush(stderr);
   fprintf(log_fp,"%s:%s\n",message_type_string,message);
   fflush(log_fp);


   return ( return_code );

} /*** End DPM */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	debug_init
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = debug_init(arg);
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
	TRACE DT432
	FULL  DF433
%HEADER END
==========================================================*/
int debug_init(
	       char *program_name        /* Input */
	       )

{

  cgi_debug_init ( program_name, NULL );
  
  return ( D_S_SUCCESS );
  
} /*** End debug_init */
#endif

/*==========================================================
%FUNCTION NAME
	DPE
%PURPOSE
	Writes a message to the error log
%SYNTAX
	DPE(char * message);
%EXAMPLE CALL
        char *message = "Success";
	ret_val = DPE(message);
%RETURNS
	D_S_SUCCESS if it is able to write to the error log
	D_E_ERRLOG if it is unable to write to the error log
%SCOPE
	public
%NEEDED INCLUDES
	#include "debug.h"
	#include "derrno.h"
%METHOD
	
%FILES
	error_log
%NOTES
	?
%BUGS
	?
%FLAGS  
%HEADER END
==========================================================*/
int DPE(
	char *msg   /* Input */
	)

{
   int return_code = D_S_SUCCESS;
   

   if ( debug_initialized == D_FALSE )
   {
     fprintf(stderr,"%s\n",msg);
     fflush(stderr);
   }
   else {
     fprintf(log_fp,"%s\n",msg);
     fflush(log_fp);
   }

   return ( return_code );

} /*** End DPE */


/*==========================================================
%FUNCTION NAME
        in_flag_list
%SCOPE
        ?public | private | static
%PURPOSE
        ?
%SYNTAX INCLUDES
        #include "?"
%EXAMPLE CALL
        ?ret_val = in_flag_list(arg);
%RETURNS
        ?
%METHOD
        ?
%FILES
        ?
%TABLES
        ?
%SYNTAX ARGS
==========================================================*/
int in_flag_list(
                 int flag  /* Input */
                 )

{
   int return_code = D_FALSE;

   /*fprintf(log_fp,"in flag list, flag = %d\n",flag);*/
   
   if ( flag_list[flag] == D_TRUE )
     return_code = D_TRUE;
   else
     return_code = D_FALSE;

   return ( return_code );
      


} /*** End in_flag_list */

/*==========================================================
%FUNCTION NAME
        denter
%SCOPE
        ?public | private | static
%PURPOSE
        ?
%SYNTAX INCLUDES
        #include "?"
%EXAMPLE CALL
        ?ret_val = denter(arg);
%RETURNS
        ?
%METHOD
        ?
%FILES
        ?
%TABLES
        ?
%SYNTAX ARGS
==========================================================*/
int denter(
           int flag /* Input */
           )

{
   int i;
   char nested_tabs[1000];

   if ( debug_initialized == D_FALSE )
   {
     debug_init( NULL );
   }
   
   /*fprintf(log_fp,"in denter with flag %d\n",flag);*/
   strcpy(nested_tabs,"");

   /*fprintf(log_fp,"nested level = %d\n",nested_level);*/

   if ( in_flag_list ( flag ) == D_TRUE )
   {
      
      /*fprintf(log_fp,"This guy is in the list \n");*/
      
      nested_level++;
      for ( i =0; i <( nested_level -1) ; i++ )
      {
         /*fprintf(log_fp,"tabs=%d\n",i);*/
         strcat(nested_tabs,"-----");
      }
      
      strcat(nested_tabs,"--> +");
      /*fprintf(log_fp,"(%s)\n",nested_tabs); */
      strcpy(function_stack[nested_level], proposed_function_name );
      fprintf(log_fp,"%s%s\n",nested_tabs,proposed_function_name);
      fflush(log_fp);
   }

   /* fprintf(log_fp,"exiting denter\n"); */
   
   return ( D_S_SUCCESS );
   

} /*** End denter */

/*==========================================================
%FUNCTION NAME
        dexit
%SCOPE
        ?public | private | static
%PURPOSE
        ?
%SYNTAX INCLUDES
        #include "?"
%EXAMPLE CALL
        ?ret_val = dexit(arg);
%RETURNS
        ?
%METHOD
        ?
%FILES
        ?
%TABLES
        ?
%SYNTAX ARGS
==========================================================*/
int dexit(
          int flag  /* Input - flag */
          )
   
{
   int i;
   char nested_tabs[1000];
   strcpy(nested_tabs,"");
   if ( in_flag_list ( flag ) == D_TRUE )
   {
      
      for ( i =0; i < nested_level -1; i++ )
      {
         strcat(nested_tabs,"-----");
      }
      
      strcat(nested_tabs,"--> -");
      fprintf(log_fp,"%s%s\n",nested_tabs,function_stack[nested_level]);
      fflush(log_fp);
      nested_level--;
   }

   
   
   return ( D_S_SUCCESS );
   

} /*** End dexit */

