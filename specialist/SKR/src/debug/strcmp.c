/*==========================================================

%SOURCE FILE
	strcmp.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	18Sep96 divita -- Initial Version

%%
==========================================================*/

static char sccs_id_strcmp_c[] = "@(#)strcmp.c	1.2	09/27/06";

/*----------------------------
%INCLUDES
----------------------------*/
#ifdef _NT_
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
int strcasecmp();
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

#define DT1142  1142          /* DT for strcasecmp() */
#define DF1143  1143          /* DF for strcasecmp() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	strcasecmp
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = strcasecmp(arg);
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
	TRACE DT1142
	FULL  DF1143
%HEADER END
==========================================================*/
int strcasecmp(
	       char *string1,  /* Input */
	       char *string2   /* Input */
	       )

{
  int return_code = D_S_SUCCESS;
  char *astring = NULL;
  char *bstring = NULL;

  DFNAME("strcasecmp");
  DENTER(DT1142);

  astring= (char * ) stdup( string1);
  bstring= (char * ) stdup( string2);
  
  llip( astring );
  llip( bstring );

  return_code = strcmp( astring, bstring );
  
  CHAR_FREE ( astring );
  CHAR_FREE ( bstring );

  DEXIT(DT1142);
  return ( return_code );

} /*** End strcasecmp */


#endif
