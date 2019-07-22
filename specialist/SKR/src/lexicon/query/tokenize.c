
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
	29May98 divita -- Initial Version

==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include <string.h>
#include <debug.h>
#include <ctype.h>
#include <lvg.h>

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#define DELIMITER " "
#define kRemoveAtStart        "\"\'‘“" 
#define kRemoveAtEnd          "\"\'’”†‡*"
#define kMayRemoveAtEnd       ".?!:;,…" 
#define kStartParens          "([{|"
#define kEndParens            ")]}|" 
#define kMapFromChars         "ÀÁÂÃÄÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäçèéêëìíîïñòóôõöùúûüýÿ" 
#define kMapFromChars         "ÀÁÂÃÄÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäçèéêëìíîïñòóôõöùúûüýÿ" 
#define kMapToChars           "AAAAACEEEEIIIINOOOOOUUUUYaaaaaceeeeiiiinooooouuuuyy" 

/*end of constants ---------*/

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
/* int lvg_tokenize(); */
WordList *tokenize_everything(
		   char *term,            /* Input */
		   int tokenize_style    /* Input */
		   ) ;

WordList * tokenize_by_russells_rules( 
				      char *term /* Input */
				      );

WordList *tokenize_replace_punct_with_space(
					    char *term  /* Input */
					    );

char *tokenize_replace_punct_with_space_aux(
					    char *term /* Input */
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

#define DT1648  1648          /* DT for tokenize_everything() */
#define DF1649  1649          /* DF for tokenize_everything() */
#define DT1842  1842          /* DT for lvg_tokenize() */
#define DF1843  1843          /* DF for lvg_tokenize() */
#define DT2548  2548          /* DT for tokenize_by_russells_rules() */
#define DF2549  2549          /* DF for tokenize_by_russells_rules() */
#define DT2640  2640          /* DT for tokenize_replace_punct_with_space() */
#define DF2641  2641          /* DF for tokenize_replace_punct_with_space() */
#define DT2644  2644          /* DT for tokenize_replace_punct_with_space_aux() */
#define DF2645  2645          /* DF for tokenize_replace_punct_with_space_aux() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	tokenize_everything
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = tokenize_everything(arg);
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
	TRACE DT1840
	FULL  DF1841
%HEADER END
==========================================================*/
WordList *tokenize_everything( 
		    char *term,            /* Input */
		    int tokenize_style    /* Input */
		     ) 

{
  WordList * wl = NULL;
  int x = 0;
  char current_word[MAXLINE];
  char *current_word_ptr = NULL;
  register char *aptr = NULL;
  int word_boundry = 0;
  int punct_boundry = 0;
  
  char aterm[MAXLINE];
  

  DFNAME("tokenize_everything");
  DENTER(DT1648);

  strcpy(current_word,"");

  if ( tokenize_style == RUSSELLS_RULES ) {
    wl = tokenize_by_russells_rules( term );
  }
  else if ( tokenize_style == REPLACE_PUNCT_WITH_SPACE ) {

    wl = tokenize_replace_punct_with_space(  term );



  } else {
    wl = (WordList *) malloc ( sizeof(WordList )) ;
    wl->words = (char **) malloc ( sizeof( char *) * (strlen (term )));
    
    wl->n = 0;
    
    if ( tokenize_style == BREAK_ON_SPACE ) {
      strcpy (aterm, term );
      current_word_ptr = strtok( aterm,DELIMITER );
      while ( current_word_ptr != NULL ) {
	WLWN(wl,wl->n)= strdup ( current_word_ptr );
	wl->n++;
	current_word_ptr = strtok( NULL, DELIMITER );
      }
      
    } else {
      
      for ( aptr = (char *)term; *aptr != EOS; aptr++ ) {
	
	
	if (!( isspace( (int) *aptr )) && ( tokenize_style == BREAK_ON_SPACE )) {
	  current_word[x]= *aptr;
	  current_word[x + 1]= EOS; 
	  x++;
	  
	  
	}else if ((*aptr == '-' ) && ( tokenize_style == DONT_BREAK_ON_HYPHENS )) {
	  current_word[x]= *aptr;
	  current_word[x + 1]= EOS; 
	  x++;
	  
	} else if  (( isspace( (int ) *aptr )) || ( ispunct( (int ) *aptr ))) {
	  
	  if ( ispunct( (int) *aptr )) {
	    
	    if ( strlen( current_word ) > 0 ) {
	      WLWN(wl,wl->n)= strdup ( current_word );
	      wl->n++;
	    }
	    strcpy( current_word,"");
	    x = 0;
	    punct_boundry = 1;
	    word_boundry = 0;
	  }
	  else if ( word_boundry == 0 ) {
	    
	    if ( strlen( current_word ) > 0 ) {
	      
	      WLWN(wl,wl->n)= strdup ( current_word );
	      wl->n++;
	    }
	    strcpy( current_word,"");
	    x = 0;
	    word_boundry = 1;
	    punct_boundry = 0;
	    
	  }
	  
	  
	  if (( tokenize_style == ORIG_WORD_DEF ) && 
	      (( isspace( (int ) *aptr) )||(ispunct((int) *aptr) ))) {
	  }
	  else if (( tokenize_style != KEEP_EVERYTHING ) && 
		   ( isspace( (int ) *aptr) )) {
	  }
	  else {
	    current_word[x]= *aptr;
	    current_word[x + 1]= EOS; 
	    x++;
	  }
	  
	  
	}
	
	else {
	  
	  if (( word_boundry == 1 ) || ( punct_boundry == 1 )) {
	    
	    if ( strlen( current_word ) > 0 ) {
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
      
      if (( current_word != NULL ) && ( strlen( current_word ) > 0 ) ) {
	WLWN(wl,wl->n)= strdup ( current_word );
	strcpy( current_word,"");
	wl->n++;
      }
    }
    
  } /* End of whether this is from Russell's rules or not */

  DEXIT(DT1648);

  return ( wl );

} /*** End tokenize_everything */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	tokenize_by_russells_rules
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = tokenize_by_russells_rules(arg);
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
	TRACE DT2548
	FULL  DF2549
%HEADER END
==========================================================*/
WordList * tokenize_by_russells_rules( 
				      char *term /* Input */
				      )

{
  WordList       *wl = NULL;
  int        theDone = D_FALSE;
  char        *theCT = NULL; 
  int    theCTLength = 0; 
  char *leadingParen = NULL;
  char  *endParenPtr = NULL;
  int              i =  0;
  int              j =  0;
  char       theChar = '\0';
  char     *theIndex = NULL;
  WordList      *rWl = NULL;

  DFNAME("tokenize_by_russells_rules");
  DENTER(DT2548);

  wl = tokenize_everything( term,BREAK_ON_SPACE );

  if (( wl != NULL ) && ( wl->n > 0 ) ) {

    
    /* ---------------------------------
       Russell's rules
       --------------------------------- */
    
    /* -----------------------------
       Repeatedly strip punctuation
       ----------------------------- */

    rWl        = (WordList *) malloc ( sizeof(WordList ) );
    rWl->words = (char **) malloc ( sizeof (char *) * wl->n );
    rWl->n     = 0;
    
    for ( i = 0; i < wl->n; i++ ) {
      
      theCT = WLWN( wl, i );
      theCTLength = strlen( theCT );

      theDone = D_FALSE;

      leadingParen = NULL;
      endParenPtr = NULL;
      j =  0;
      theChar = '\0';
      theIndex = NULL;
      
      while (theDone != D_TRUE ) {
	
	
	theDone=D_TRUE;
	
	/* -----------------------------
	   Strip leading quotes
	   ----------------------------- */ 
	if ( theCTLength >0 ) {
	  
	  
	  if ( strchr( kRemoveAtStart, theCT[0] ) != NULL ) {
	    theCT   = theCT + 1;       /* char arithmatic !! */
	    theDone = D_FALSE;
	    theCTLength = strlen( theCT );
	  }
	}
	
      /* ---------------------------------------------
	 Strip trailing quotes and footnote indicators
         --------------------------------------------- */
	
	if (theCTLength>0) {
	  
	  if ( strchr( kRemoveAtEnd, theCT[ theCTLength - 1 ] ) != NULL ) {
	    
	    theCT [ theCTLength -1 ] = EOS;
	    theDone = D_FALSE;
	    theCTLength = strlen( theCT );
	  }
	}

	/* -------------------------------------------
	   Strip trailing punctuation, 
	   if not also imbedded in string
	   This still screws up some abbreviations, 
	   pg. and Mr., but not Ph.D. and e.g.
	   ------------------------------------------- */
	
	if ( theCTLength  > 0 ) {
	  
	  if ( strchr( kMayRemoveAtEnd, theCT[ theCTLength - 1 ] )!= NULL ) {
	    theCT [ theCTLength -1 ] = EOS;
	    theDone = D_FALSE;
	    theCTLength = strlen( theCT );
	  }

	}

	/* --------------------------------------------
	   Strip trailing parens, if no embedded partner
	   -------------------------------------------- */

	if (theCTLength >0) {
	  
	  if ( (endParenPtr = (char *)strchr( kEndParens, theCT[ theCTLength - 1 ] )) != NULL ) {
	    
	    if ( strchr( kStartParens, endParenPtr[0] ) == NULL ) {
	      theCT [ theCTLength -1 ] = EOS;
	      theDone = D_FALSE;
	      theCTLength = strlen( theCT );
	    }
	    
	  }
	}
	/* --------------------------------------------
	   Strip leading parens, if no embedded partner
	   
	   This preserved things like:  page(s)
	   -------------------------------------------- */

	
	if (theCTLength >0) {
	  
	  if ( (leadingParen = (char *)strchr( kStartParens, theCT[0] )) != NULL ) {
	    
	    if ( strchr( kEndParens, leadingParen[0] ) == NULL ) {
	      
	      theCT   = theCT + 1;
	      theDone = D_FALSE;
	      theCTLength = strlen( theCT );
	    }
	  }
	}

      } /* while (theDone != D_TRUE )  */
      
      
      /* --------------------
         Map accented chars 
         -------------------- */
      
      for ( j = 0; j< theCTLength; j++) {
	
	theChar=theCT[j];

	if (( theIndex = strchr( kMapFromChars, theChar )) != NULL  ) {
	
	  
		theCT[ j ] = kMapToChars[ (int)theIndex[0] ] ; 
	  
	}
      }

      
      if ( theCT != NULL ) {
	
	WLWN( rWl, rWl->n ) = strdup( theCT );
	rWl->n++;

      }
      
      free( WLWN( wl,i ) );
      
  
    } /* End for loop through each space delimited term */
    
    if ( wl != NULL )   free (wl ) ; 
  }    
    
  DEXIT(DT2548);
  return ( rWl );

} /*** End tokenize_by_russells_rules */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	tokenize_replace_punct_with_space
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = tokenize_replace_punct_with_space(arg);
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
	TRACE DT2640
	FULL  DF2641
%HEADER END
==========================================================*/
WordList *tokenize_replace_punct_with_space(
					    char *term  /* Input */
					    )
{

  WordList *wl = NULL;

  DFNAME("tokenize_replace_punct_with_space");
  DENTER(DT2640);

  wl = malloc ( sizeof (WordList ) );
  
  wl->words = malloc ( sizeof( char *) );
  
  wl->n = 1;
  WLWN(wl,0) = strdup ( tokenize_replace_punct_with_space_aux( term ));
  


  DEXIT(DT2640);
  return ( wl );

} /*** End tokenize_replace_punct_with_space */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	tokenize_replace_punct_with_space_aux
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = tokenize_replace_punct_with_space_aux(arg);
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
	TRACE DT2644
	FULL  DF2645
%HEADER END
==========================================================*/
char *tokenize_replace_punct_with_space_aux(
					    char *term /* Input */
					    )
{
  static char newTerm[MAXLINE];
  int n = 0;
  int i = 0;
  

  DFNAME("tokenize_replace_punct_with_space");
  DENTER(DT2644);

  strcpy( newTerm,"");

  n = strlen( term );

  for ( i = 0; i < n; i++ ) {

    if ( ispunct( (int) term[i] ) ) {

      newTerm[i] = ' ';

    } else {
      newTerm[i] = term[i];
    }
    newTerm[i + 1 ] = EOS;
    
  }
  

  DEXIT(DT2644);
  return ( newTerm );

} /*** End tokenize_replace_punct_with_space_aux */
