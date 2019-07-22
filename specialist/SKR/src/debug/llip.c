/*==========================================================

%SOURCE FILE
	llip.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	18Sep96 divita -- Initial Version

%%
==========================================================*/

static char sccs_id_llip_c[] = "@(#)llip.c	1.2	09/27/06";

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
void nls_llip(register char *s );
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

#define DT1144  1144          /* DT for nls_llip() */
#define DF1145  1145          /* DF for nls_llip() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	nls_llip
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = nls_llip(arg);
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
	TRACE DT1144
	FULL  DF1145
%HEADER END
==========================================================*/
void nls_llip(
	      register char *s /* input/ Output */
	      )
   
{
  
  DFNAME("nls_llip");
  DENTER(DT1144);

  for (; *s != EOS; s++)
	*s = (isupper(*s) ? tolower(*s) : *s);

  DEXIT(DT1144);

} /*** End nls_llip */
