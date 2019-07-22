/*==========================================================

%SOURCE FILE
	ischar.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	9Sep99 divita -- Initial Version

%%
==========================================================*/

static char sccs_id_ischar_c[] = "@(#)ischar.c	1.2 09/27/06";

/*----------------------------
%INCLUDES
----------------------------*/

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
int isSpace( char c );
int isPunct( char c );
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

#define DT2624  2624          /* DT for isSpace() */
#define DF2625  2625          /* DF for isSpace() */
#define DT2626  2626          /* DT for isPunct() */
#define DF2627  2627          /* DF for isPunct() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	isSpace
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = isSpace(arg);
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
	TRACE DT2624
	FULL  DF2625
%HEADER END
==========================================================*/
int isSpace(char c)

{

  if (( c == ' ' ) || 
      ( c == '\t' ) || 
      ( c == '\n' ) || 
      ( c == '\r' ) )

     return ( 1 ); 

  return ( 0 ); 

} /*** End isSpace */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	isPunct
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = isPunct(arg);
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
	TRACE DT2626
	FULL  DF2627
%HEADER END
==========================================================*/
int isPunct( char c)

{
  

  if ( ( ( c >= '!' ) && ( c <= '/' )) ||
       ( ( c >= '|' ) && ( c <= '-' )) ||
       ( ( c >= '[' ) && ( c <= '`' )) ||
       ( ( c >= '{' ) && ( c <= '~' )) 
       )
    return ( 1 );
  
  return ( 0 );

} /*** End isPunct */
