/*==========================================================

%SOURCE FILE
	lvg_min.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	7Jul99 divita -- Initial Version

%%
==========================================================*/

static char sccs_id_lvg_min_c[] = "@(#)lvg_min.c	1.2 09/27/06";

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
signed int lvg_min( 
	    signed int a, /* Input */
	    signed int b  /* Input */
	     );
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

#define DT2600  2600          /* DT for lvg_min() */
#define DF2601  2601          /* DF for lvg_min() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	lvg_min
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lvg_min(arg);
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
	TRACE DT2600
	FULL  DF2601
%HEADER END
==========================================================*/
signed int lvg_min( 
	    signed int a, /* Input */
	    signed int b  /* Input */
	     )

{
  int return_code = 0; 

  DFNAME("lvg_min");
  DENTER(DT2600);

  if ( (signed int )  a <  (signed int ) b )
    return_code = a; 
  else
    return_code = b; 

  DEXIT(DT2600);
  return ( return_code );

} /*** End lvg_min */
