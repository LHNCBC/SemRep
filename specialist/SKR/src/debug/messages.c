/*==========================================================

%SOURCE FILE
	messages.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	25Jan95 divita -- Initial Version

%%
==========================================================*/

static char sccs_id_messages_c[] = "@(#)messages.c	1.2	09/27/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include <stdio.h>
#include <errno.h>
#include "debug.h"
#include "messages.h"

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
/*end of constants ---------*/

/*----------------------------
%MACROS
----------------------------*/
/*end of macros ------------*/

/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/
/*end of external functions */

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/*end of global variables --*/



/*----------------------------
%TYPEDEFS
----------------------------*/
typedef struct struct_Stack_record {
   struct struct_Stack_record     *next;
   char * message;
 }Stack_record,     *Stack_recordPtr;

typedef struct struct_File_record {

   struct struct_File_record   *next;
   char *                       name;
   Stack_recordPtr                messages;
   FILE                          *fp;

 }File_record,     *File_recordPtr;

/*end of typedefs ----------*/

/*----------------------------
%PRIVATE STRUCTURES
----------------------------*/
static File_recordPtr open_files = NULL;
/*end of private structures */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
/*
int close_msg_stack(char *file     * Input   * );
int add_msg_to_stack(char * file,       * Input * 
		     char * message     * Input * 
		     );
int print_msg_stack(char *file  * Input * );
int clear_msg_stack(char *file   * Input * );
int print_msg_to_stack(char *file,     * Input * 
		       char *message   * Input * 
		       );
*/
/*end_of_function_prototypes*/

/*----------------------------
%STATIC FUNCTIONS
----------------------------*/
static File_recordPtr get_stack(char * file  /* input */);
/*end of static functions --*/


/*----------------------------
%DEBUG FLAGS
----------------------------*/

#define DT830  830          /* DT for close_msg_stack() */
#define DF831  831          /* DF for close_msg_stack() */
#define DT832  832          /* DT for add_msg_to_stack() */
#define DF833  833          /* DF for add_msg_to_stack() */
#define DT834  834          /* DT for print_msg_stack() */
#define DF835  835          /* DF for print_msg_stack() */
#define DT836  836          /* DT for clear_msg_stack() */
#define DF837  837          /* DF for clear_msg_stack() */
#define DT838  838          /* DT for get_stack() */
#define DF839  839          /* DF for get_stack() */
#define DT876  876          /* DT for print_msg_to_stack() */
#define DF877  877          /* DF for print_msg_to_stack() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	close_msg_stack
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = close_msg_stack(arg);
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
	TRACE DT830
	FULL  DF831
%HEADER END
==========================================================*/
int close_msg_stack( 
		    char *file    /* Input   */
		    )

{
  int              return_code   = D_S_SUCCESS;
  File_recordPtr   current_file  = NULL;
  File_recordPtr   previous_file = NULL;
  int              found         = D_FALSE;
  
  DFNAME("close_msg_stack");
  DENTER(DT830);
  
  
  print_msg_stack ( file );
  clear_msg_stack ( file );
  

  current_file = open_files;

  while (( current_file != NULL ) && (found == D_FALSE))
  {
    if ( strcmp ( file, current_file->name ) != 0 )
    {
      previous_file = current_file;
      current_file = current_file->next;
    }
    else
    {
      found = D_TRUE;
    }
  }

  if ( found == D_TRUE )
  {
    if (( current_file->fp != stdout ) && 
	( current_file->fp != stderr ) && 
	(current_file->fp != (FILE *) NULL ) )
    {
      fclose( current_file->fp );
    }

    CHAR_FREE ( current_file->name );
    
    if ( previous_file != NULL )
    {
      previous_file->next = current_file->next;
    }
    else
    {
      open_files = current_file->next;
    }
    
    free ( current_file );
  }

  DEXIT(DT830);
  return ( return_code );
  
} /*** End close_msg_stack */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	add_msg_to_stack
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = add_msg_to_stack(arg);
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
	TRACE DT832
	FULL  DF833
%HEADER END
==========================================================*/
int add_msg_to_stack(
		     char * file,      /* Input */
		     char * message    /* Input */
		     )
{
  int return_code = D_S_SUCCESS;
  File_recordPtr  current_file;
  Stack_recordPtr old_top = NULL;
  Stack_recordPtr new_top = NULL;
  
  DFNAME("add_msg_to_stack");
  DENTER(DT832);
  
  current_file  = get_stack ( file );

  if ( current_file == NULL )
  {
    /* -------------------------------------------
       Create a new file record to store the stack 
       Put the file on the list of files
       ------------------------------------------- */
    current_file = (File_recordPtr ) calloc ( 1,sizeof ( File_record ) );
    if ( current_file == (File_recordPtr) NULL )
      {return_code = D_F_MALLOC; DPM(return_code); goto bottom;}

    not_smart_copy (current_file->name, file );
    current_file->next = open_files; 
    open_files = current_file;
    
    /* -------------------------------------------------
       If one puts in stdout, or stderr as the file name
       instantiate the file pointers to their values
       ------------------------------------------------- */
    if ( strcmp(file, "stdout") == 0 )
    {
      current_file->fp = stdout;
    }
    else if ( strcmp(file, "stderr") == 0 )
    {
      current_file->fp = stderr;
    }
    else
    {
      current_file->fp = (FILE * ) fopen((char * ) file, "w");
      if ( current_file->fp == NULL )
      {
	sprintf(msg,
		"When creating the stack: Unable to open file %s error no %d",
		file,errno );
	DPE(msg);
	if ( errno == EMFILE )
	{
	  sprintf(msg,"Too many files open" );DPE(msg);
	}
	goto bottom;
      }
      else
      {
	fclose( current_file->fp );
      }
    }
  }
  
  /* ------------------------------
     Add this message to this stack
     ------------------------------ */
  if ( message != NULL )
  {
    /* ----------------------------------
       Note where the top of the stack is
       ---------------------------------- */
    old_top = current_file->messages ;
    
    /* -----------------------
       Make a new message node
       ----------------------- */
    new_top = (Stack_recordPtr) calloc (1, sizeof (Stack_record) ) ;
    if ( new_top == (Stack_recordPtr) NULL )
      {return_code = D_F_MALLOC; DPM(return_code); goto bottom;}

    not_smart_copy (new_top->message, message);
    /* ------------------------------------------
       Insert this record on the top of the stack
       ------------------------------------------ */
    new_top->next = old_top;
    current_file->messages = new_top;
  }
 bottom:

  DEXIT(DT832);
  return ( return_code );
  
} /*** End add_msg_to_stack */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	print_msg_stack
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = print_msg_stack(arg);
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
	TRACE DT834
	FULL  DF835
%HEADER END
==========================================================*/
int print_msg_stack(
		    char *file /* Input */
		    )
{
  int              return_code     = D_S_SUCCESS;
  File_recordPtr   current_file    = NULL;
  Stack_recordPtr  current_message = NULL;
  FILE            *fp              = NULL;  
  
  DFNAME("print_msg_stack");
  DENTER(DT834);
  
  current_file = get_stack ( file );

  if ( current_file != NULL )
  {
    /* -----------------------------
       Open this file for appending.
       (if this file is not stderr, or sdtin )
       ----------------------------- */
    if (( current_file->fp != stderr ) && (current_file->fp != stdout ) )
    {
      fp =(FILE * )  fopen((CharPtr) current_file->name, "a+");
    }
    else
    {
      /* -------------------------------------
	 This file is either stderr, or sdtin
         ------------------------------------- */
      fp = current_file->fp; 
    }

    if ( fp == NULL )
    {
      sprintf(msg,
	   "When Printing the stack: Unable to open file %s [%d] using stderr",
	      file,
	      errno);
      DPE(msg);
      fp = stderr;
      
    }

    /* -------------------------------------------------
       While there are messages on the stack, print them
       ------------------------------------------------- */
    current_message = current_file->messages;
    while ( current_message != (Stack_recordPtr) NULL )
    {
      if ( current_message->message != NULL )
      {
	fprintf(fp,"%s\n",current_message->message );
	fflush(fp);
      }
      else
      {
	fprintf(fp,"\n");
	fflush(fp);
      }
      current_message = current_message->next;
    }
    if (( fp != stderr ) && (fp != stdout ) )
    {
      fclose(fp);
    }
    
  }
  else
  {
  }
  

    
  DEXIT(DT834);
  return ( return_code );
  
} /*** End print_msg_stack */
  /**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	clear_msg_stack
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = clear_msg_stack(arg);
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
	TRACE DT836
	FULL  DF837
%HEADER END
==========================================================*/
int clear_msg_stack(
		    char *file  /* Input */
		    )
{
  int              return_code     = D_S_SUCCESS;
  File_recordPtr   current_file    = NULL;
  Stack_recordPtr  current_message = NULL;
  Stack_recordPtr  previous_message = NULL;
  

  DFNAME("clear_msg_stack");
  DENTER(DT836);
  
  /* ---------------------------------------------------------------------
     if this file has already been opened, get it's file position pointer.
     --------------------------------------------------------------------- */
  current_file = get_stack ( file );

  if ( current_file != (File_recordPtr ) NULL )
  {
  
    /* -----------------------------------------------------
       While there are messages on the stack, clear them out
       ----------------------------------------------------- */
    current_message = current_file->messages;
    while ( current_message != (Stack_recordPtr) NULL )
    {
      CHAR_FREE( current_message->message );
      previous_message = current_message;
      current_message = current_message->next;
      if ( previous_message != (Stack_recordPtr) NULL )
      {
	free ( previous_message );
      }
    }
    current_file->messages = (Stack_recordPtr) NULL ;
    
  }
  DEXIT(DT836);
  return ( return_code );
  
} /*** End clear_msg_stack */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_stack
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_stack(arg);
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
	TRACE DT838
	FULL  DF839
%HEADER END
==========================================================*/
File_recordPtr get_stack( 
			 char * file  /* input */
			 )

{
  File_recordPtr      current_file = NULL;
  int                 found        = D_FALSE;
  
  DFNAME("get_stack");
  DENTER(DT838);
  

  current_file = open_files;

  while (( current_file != NULL ) && (found == D_FALSE))
  {

    if ( strcmp ( file, current_file->name ) != 0 )
    {
      current_file = current_file->next;
    }
    else
    {
      found = D_TRUE;
    }
  }

  
  DEXIT(DT838);
  return ( current_file );
  
} /*** End get_stack */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	print_msg_to_stack
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = print_msg_to_stack(arg);
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
	TRACE DT876
	FULL  DF877
%HEADER END
==========================================================*/
int print_msg_to_stack(
		       char *file,    /* Input */
		       char *message  /* Input */
		       )
{
  int return_code = D_S_SUCCESS;
  File_recordPtr   current_file    = NULL;
  Stack_recordPtr  current_message = NULL;
  FILE            *fp              = NULL;  
  
  
  DFNAME("print_msg_to_stack");
  DENTER(DT876);
  
  
  current_file = get_stack ( file );

  if ( current_file != NULL )
  {
    /* ---------------------------------------------------------------------
       Open this file for appending. (if this file is not stderr, or sdtin )
       --------------------------------------------------------------------- */
    if (( current_file->fp != stderr ) && (current_file->fp != stdout ) )
    {
      fp = (FILE * ) fopen((CharPtr) current_file->name, "a+");
      current_file->fp = fp;
    }
    if ( fp == NULL )
    {
      sprintf(msg, "When Printing the stack: Unable to open file %s %d",
	      file, errno);
      DPE(msg);
      fp = stderr;
    }
    else
    {
      fp = current_file->fp ;
    }

    /* -----------------
       Print the message
       ----------------- */
    if ( message != NULL )
    {
      fprintf(fp,"%s\n",message );
      fflush(fp);
    }
    else
    {
      fprintf(fp,"\n");
      fflush(fp);
    }
    
    if (( current_file->fp != stderr ) && (current_file->fp != stdout ) )
    {
      fclose(current_file->fp);
    }
    
  }
  else
  {
     add_msg_to_stack(file,message);
     print_msg_stack(file);
     clear_msg_stack(file);
  }
  
    
  
  DEXIT(DT876);
  return ( return_code );

} /*** End print_msg_to_stack */
