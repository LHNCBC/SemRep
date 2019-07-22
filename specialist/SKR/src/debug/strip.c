/*==========================================================

%SOURCE FILE
	strip.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	13Jan95 divita -- Version taken from lvg

%%
==========================================================*/

static char sccs_id_strip_c[] = "@(#)strip.c	1.2	09/27/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include "debug.h"
#include "useful.h"
/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#define      LEFT_HAND_SIDE     5
#define      RIGHT_HAND_SIDE   10
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
/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
/*
int strip_white_spaces();
int strip_lefthandside_spaces();
*/
/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/*end of global variables --*/

/*----------------------------
%PRIVATE STRUCTURES
----------------------------*/
/*end of private structures */

/*----------------------------
%TYPEDEFS
----------------------------*/
/*end of typedefs ----------*/

/*----------------------------
%DEBUG FLAGS
----------------------------*/
#define DT554  554          /* DT for strip_white_spaces() */
#define DF555  555          /* DF for strip_white_spaces() */
#define DT500  500          /* DT for strip_lefthandside_spaces() */
#define DF501  501          /* DF for strip_lefthandside_spaces() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
	strip_white_spaces
%PURPOSE
	Strips spaces from a string
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = strip_white_spaces(arg);
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
	TRACE DT554
	FULL  DF555
%HEADER END
==========================================================*/
int strip_white_spaces(char *line ) 

{
  int return_code = D_S_SUCCESS;
  char *tmp_line = NULL;
  int n = 0;
  int line_size = 0;
  int ctr = 0;
  int done = D_FALSE;
  int i = 0;
  
  DFNAME("strip_white_spaces");
  DENTER(DT554);
  
  line_size = strlen ( line );
  tmp_line = (char *) malloc ( line_size + 1 );
  
  n = 0;
  done = D_FALSE;
  while (( n < line_size ) && ( done == D_FALSE ))
  {
    switch (line[n])
    {
    case '\0':
      done = D_TRUE;
      break;
      
    case TAB:
    case SPACE:
      break;

    default:
      tmp_line[ctr]= line[n];
      ctr++;
    }
    n++;
  }
  tmp_line[ctr]= '\0';
  sprintf(msg,"The tmp line = [%s]", tmp_line);
  DPR(DF555,msg);
  i = 0;
  while ( i < ctr )
  {
    line[i] = tmp_line[i];
    i++;
  }
  line[i]='\0';

  sprintf(msg,"The resulting line = [%s]", line);
  DPR(DF555,msg);

  if ( tmp_line != NULL ) free ( tmp_line );

  DEXIT(DT554);
  return ( return_code );

} /*** End strip_white_spaces */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	strip_lefthandside_spaces
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = strip_lefthandside_spaces(arg);
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
	TRACE DT500
	FULL  DF501
%HEADER END
==========================================================*/
int strip_lefthandside_spaces(
			      char *line  /* Input /Output */
			      )

{
  int return_code = D_S_SUCCESS;
  char *tmp_line = NULL;
  int n = 0;
  int line_size = 0;
  int ctr = 0;
  int done = D_FALSE;
  int i = 0;
  
  int side = LEFT_HAND_SIDE;

  DFNAME("strip_lefthandside_spaces");
  DENTER(DT500);

  DPR(DF501,"");
  
  line_size = strlen ( line );
  tmp_line = (char *) malloc ( line_size + 1 );
  
  n = 0;
  done = D_FALSE;
  while (( n < line_size ) && ( done == D_FALSE ))
  {
    switch (line[n])
    {
    case '\0':
      done = D_TRUE;
      break;
      
    case EQUALS:
      side = RIGHT_HAND_SIDE ;
      tmp_line[ctr]= line[n];
      ctr++;
      break;

    case TAB:

    case SPACE:
      if ( side == RIGHT_HAND_SIDE )
      {
	tmp_line[ctr]= line[n];
	ctr++;
      }

      break;

    default:
      tmp_line[ctr]= line[n];
      ctr++;
    }
    n++;
  }
  tmp_line[ctr]= '\0';
  sprintf(msg,"The tmp line = [%s]", tmp_line);
  DPR(DF555,msg);
  i = 0;
  while ( i < ctr )
  {
    line[i] = tmp_line[i];
    i++;
  }
  line[i]='\0';

  sprintf(msg,"The resulting line = [%s]", line);
  DPR(DF555,msg);

  if ( tmp_line != NULL ) free ( tmp_line );


  DEXIT(DT500);
  return ( return_code );

} /*** End strip_lefthandside_spaces */
