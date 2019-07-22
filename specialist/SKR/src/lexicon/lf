/*==========================================================

%SOURCE FILE
	lf.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	6Dec96 divita -- Initial Version

%%
==========================================================*/



/*----------------------------
%INCLUDES
----------------------------*/
#include <stdio.h>
#include <debug.h>
#include <wl.h>


/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#define CASE_SENSITIVE        1
#define CASE_INSENSITIVE      2
#define WORD_PREFIX           1
#define STRING_PREFIX         2 
#define EXACT                 3 

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
extern int close_lexicon_record_file();

extern int lf__api(
	   char *orig_term,                /* Input  */
	   char argc,            
	   char **argv,            
	   FILE *output_file_ptr      /* Output */
	   );
extern int close_lex_btree();

/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int main( char argc, char **argv );

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

#define DT1292  1292          /* DT for main() */
#define DF1293  1293          /* DF for main() */
/*end_of_debug_flags---------*/

/*----------------------------
%SCCS MARKER
----------------------------*/
static char sccs_id[] = "@(#)bourne_skel	1.1	28 Feb 1995";


/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	main
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = main(arg);
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
	TRACE DT1292
	FULL  DF1293
%HEADER END
==========================================================*/
int main(
	 char argc,
	 char **argv
	 )

{
  int return_code = D_S_SUCCESS;
  char line[MAXLINE];
  WordList *wl = NULL;
  int i = 0;
  int   not_found = D_TRUE;	
  int   one_time_call = D_FALSE;

  DFNAME("main");
  DENTER(DT1292);

  //  nls_cfg_read("lexicon2007.cfg"); 


  for ( i = 1; i < argc; i++ )
    {
      if (( strncmp( argv[i],"-h",2 ) == 0 ) ||
	  ( strncmp( argv[i],"-f",2 ) == 0 ) )
	{
	  one_time_call = D_TRUE;
	}
    }
  if ( one_time_call == D_TRUE )
    {
      lf__api( NULL, argc, argv, stdout );
    }
  else
    {
      fgets(line,MAXLINE,stdin);
      while ( !feof (stdin ) ) {
	  
	  line[ strlen( line ) - 1 ] = EOS;
	  
	  
	  /* -------------------------------
	     Lookup this string in the index
	     
	     wl = (WordList *) query_lex_info( line,CASE_SENSITIVE,WORD_PREFIX,NULL); 
	     ------------------------------- */
	  
	  

	  lf__api( line, argc, argv, stdout );

    
    
    
	  strcpy(line,"");
	  fgets(line,MAXLINE,stdin) ;
	}
      
    }

  /* ---- 
     CALL the database term, to close the database
     ----  */
  close_lex_btree();
  close_lexicon_record_file();

  /*cfg_free();*/
  
  DEXIT(DT1292);
  return ( return_code );

} /*** End main */
