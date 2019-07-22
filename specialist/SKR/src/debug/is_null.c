/*==========================================================

%SOURCE FILE
	is_null.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	19Jan95 divita -- Initial Version

%%
==========================================================*/

static char sccs_id_is_null_c[] = "@(#)is_null.c	1.2	09/27/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include "debug.h"

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
int is_null();
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

#define DT774  774          /* DT for is_null() */
#define DF775  775          /* DF for is_null() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	is_null
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = is_null(arg);
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
	TRACE DT774
	FULL  DF775
%HEADER END
==========================================================*/
int is_null(char *string )

{
  int return_code = D_FALSE;
  
  DFNAME("is_null");
  DENTER(DT774);
  
  
  if ( string[0] == EOS )
  {
    DPR(DF775,"This string is NULL");
    return_code = D_TRUE;
  }
  
  DEXIT(DT774);
  return ( return_code );

} /*** End is_null */
