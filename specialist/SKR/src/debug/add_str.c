/*==========================================================

%SOURCE FILE
	add_str2str.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	16Apr96 divita -- Initial Version

%%
==========================================================*/

static char sccs_id_add_str_c[] = "@(#)add_str.c	1.2	09/27/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include <debug.h>

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
/*
int add_string_to_string();
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

#define DT870  870          /* DT for add_string_to_string() */
#define DF871  871          /* DF for add_string_to_string() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	add_string_to_string
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = add_string_to_string(arg);
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
	TRACE DT870
	FULL  DF871
%HEADER END
==========================================================*/
int add_string_to_string( 
			 char **string1,  /* Input/Output */
			 char *string2   /* Input */
			 )

{
  int return_code = D_S_SUCCESS;
  char tmp_string[MAXLINE];

  DFNAME("add_string_to_string");
  DENTER(DT870);

  if ( string2 != NULL )
  {
    if ( *string1 == NULL ) 
    {
      *string1 = strdup(string2 );
    }
    else
    {

      if ( string2 != NULL ) {
	
	sprintf(tmp_string,"%s %s", *string1, string2 );

	free ( *string1 );
	*string1 = strdup ( tmp_string );
      }
      
    }
  }

  DEXIT(DT870);
  return ( return_code );

} /*** End add_string_to_string */
