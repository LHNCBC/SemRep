
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
	tokenize.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	13Mar98 divita -- Initial Version

%%
==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include <debug.h>
#include <lvg.h>

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#ifndef STRIP_WHITE_SPACE
#define STRIP_WHITE_SPACE     2
#endif
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
WordList *tokenize(
		   char *term,            /* Input */
		   int tokenize_style    /* Input */
		   ) ;

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

#define DT1648  1648          /* DT for tokenize() */
#define DF1649  1649          /* DF for tokenize() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	tokenize
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = tokenize(arg);
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
	TRACE DT1648
	FULL  DF1649
%HEADER END
==========================================================*/
WordList *tokenize( 
		    char *term,            /* Input */
		    int tokenize_style    /* Input */
		     ) 

{
  WordList * wl = NULL;
  int x = 0;
  char current_word[MAXLINE];
  char *aptr = NULL;
  int word_boundry = 0;
  int punct_boundry = 0;
  
  

  DFNAME("tokenize");
  DENTER(DT1648);

  strcpy(current_word,"");

  wl = (WordList *) malloc ( sizeof(WordList )) ;
  wl->words = (char **) malloc ( sizeof( char *) * (strlen (term )));
			     
  wl->n = 0;


  

  for ( aptr = term; *aptr != EOS; aptr++ )
    {

      
	    if (( isspace((int) *aptr )) || ( ispunct( (int)*aptr )))
	{

		if ( ispunct((int) *aptr ))
	    {
	      if ( strlen( current_word ) > 0 )
		{
		  WLWN(wl,wl->n)= strdup ( current_word );
		  wl->n++;
		}
	      strcpy( current_word,"");
	      x = 0;
	      punct_boundry = 1;
	      word_boundry = 0;
	    }
	  else
	    {
	      if ( word_boundry == 0 )
		{
		  
		  if ( strlen( current_word ) > 0 )
		    {
		      WLWN(wl,wl->n)= strdup ( current_word );
		      wl->n++;
		    }
		  strcpy( current_word,"");
		  x = 0;
		  word_boundry = 1;
		  punct_boundry = 0;
		}
	    }

		if (( tokenize_style == STRIP_WHITE_SPACE ) && ( isspace((int) *aptr) ))
	    {
	    }
	  else
	    {
	      current_word[x]= *aptr;
	      current_word[x + 1]= EOS; 
	      x++;
	    }
	  

	}
      else
	{
	  if (( word_boundry == 1 ) || ( punct_boundry == 1 ))
	    {
	      if ( strlen( current_word ) > 0 )
		{
		  WLWN(wl,wl->n)= strdup ( current_word );
		  wl->n++;
		}
	      strcpy( current_word,"");
	      x = 0;
	      word_boundry = 0;
	      punct_boundry = 0;
	    }
	      
	  current_word[x]= *aptr;
	  current_word[x + 1]= EOS; 
	  x++;
	}
    }
	      
  WLWN(wl,wl->n)= strdup ( current_word );
  strcpy( current_word,"");
  wl->n++;

  
  DEXIT(DT1648);

  return ( wl );

} /*** End tokenize */
