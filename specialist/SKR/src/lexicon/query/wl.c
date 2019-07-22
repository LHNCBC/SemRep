
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
	wl.c

%DESCRIPTION OF FILE
	C source file.  functions to support word lists

%REVISED
	29May98 divita -- Initial Version

==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/

#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <ctype.h>
#include "lvg.h"
#include <debug.h>
#include <math.h>
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
extern int get_value_from_field(int field_position, char *value, char   *umls_record);
/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/

void sort_words( char **a, int n );

void sort_words_by_number( char **a, int n,int columnNumber ) ;

void fsort_words( char **a, int n, int field );

WordList * str2words( char *s, int sortFlag );

WordList *str2words_break_only_on_spaces( char *s );

int wl_cmp( WordList *wl1, WordList *wl2 );

void free_wl( WordList ** wl );

void add_word_to_list( WordList **a_wl, char *row  );

int is_on_list ( char *term , int   field, WordList* wl );

int add_wordLists( WordList **combinedList, WordList *list, int *a, int uniq );

int add_wordListsWithMsg(WordList **combinedList, WordList  *list, int *a, int uniq, char *msg);

WordList *alt_str2words( char *s, int sortFlag );

WordList *tokenize_everything( char *term, int tokenize_style) ;

/*end_of_function_prototypes*/

/*----------------------------
%PRIVATE STRUCTURES
----------------------------*/
/*end of private structures */

/*----------------------------
%TYPEDEFS
----------------------------*/
/*end of typedefs ----------*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/*end of global variables --*/



/*==========================================================
%FUNCTION NAME
        WordList * str2words( char *s, int sortFlag )
%PURPOSE
	Generates words from a string using the word index notion
        of a 'word'.  If sortFlag is set, sorts the words.
%HEADER END
==========================================================*/
WordList * str2words( char *s, int sortFlag )
{

  WordList *wl = NULL;

  wl = (WordList *) tokenize_everything( s, ORIG_WORD_DEF );

  if (sortFlag)
    sort_words(wl->words, wl->n);
  return(wl);
}

/*==========================================================
%FUNCTION NAME
        int wl_cmp( WordList *wl1, WordList *wl2 )
%PURPOSE
	compares two word lists for equality
%HEADER END
==========================================================*/
int wl_cmp( WordList *wl1, WordList *wl2 )
{

    if (wl1->n == wl2->n) {
	int i;

	for (i=0; i<wl1->n; i++)
	    if (strcmp(*(wl1->words+i), *(wl2->words+i)) != 0)
		return(0);
	return(1);
    }
    return(0);
}

/*==========================================================
%FUNCTION NAME
        void free_wl( WordList **a_wl )
%PURPOSE
	frees up memory allocated to a word list 
%HEADER END
==========================================================*/
void free_wl( WordList **a_wl )
{
    int i;
    WordList *wl = NULL;

    wl = *a_wl;

    if ( wl != NULL )
    {
      for (i=0; i< wl->n; i++)
      {
	if ( WLWN(wl, i ) != NULL ) 
	{
	  free ( WLWN(wl, i ) ); 
	}
      }

      if ( wl->words != NULL )
      {
	free( (char **) wl->words); /*<------ May cause problems on other machines*/
      }
      wl->n = 0;


      free ( wl );
      wl = NULL;
    }
    *a_wl = NULL;
    
}

/*==========================================================
%FUNCTION NAME
        void sort_words( char **a, int n ) 
%PURPOSE
	sorts the words in a word list (alg from K&R)
%HEADER END
==========================================================*/
void sort_words( char **a, int n ) 
{
    int gap, i, j;
    char *cp;

    for (gap = n/2; gap>0; gap /= 2) {
	for (i=gap; i<n; i++) {
	    for (j=i-gap; j>=0; j-=gap) {
		if (strcmp(*(a+j), *(a+j+gap)) <= 0)
		    break;
		cp = *(a+j);
		*(a+j) = *(a+j+gap);
		*(a+j+gap) = cp;
	    }
	}
    }
}
/*==========================================================
%FUNCTION NAME
        void fsort_words( char **a, int n, int field ) 
%PURPOSE
	sorts the words in a word list (alg from K&R) fielded 
%HEADER END
==========================================================*/
void fsort_words( char **a, int n, int field ) 
{
    int gap, i, j;
    char *cp;
    char string1[MAXLINE];
    char string2[MAXLINE];

    strcpy( string1,"");
    strcpy( string2,"");

    for (gap = n/2; gap>0; gap /= 2) {
	for (i=gap; i<n; i++) {
	    for (j=i-gap; j>=0; j-=gap) {
	        
		get_value_from_field(field, string1, *a+j );
		get_value_from_field(field, string2, *(a+j+gap) );
	      
		if (strcmp(string1, string2) <= 0)
		    break;
		cp = *(a+j);
		*(a+j) = *(a+j+gap);
		*(a+j+gap) = cp;
	    }
	}
    }
}

/*==========================================================
%FUNCTION NAME
        WordList *alt_str2words( char *s, int sortFlag )
%PURPOSE
	Breaks strings into words, but does not break on hyphens
%HEADER END
==========================================================*/
WordList *alt_str2words( char *s, int sortFlag )
{

  WordList * wl = NULL;

  wl =  (WordList *)tokenize_everything(s, DONT_BREAK_ON_HYPHENS );

  if (sortFlag)
    sort_words(wl->words, wl->n);
  return(wl);

}

/*==========================================================
%FUNCTION NAME
        WordList *str2words_break_only_on_spaces( char *s )
%PURPOSE
	Generates words from a string using the word index notion
	of a 'word'.  If sortFlag is set, sorts the words.
%HEADER END
==========================================================*/
WordList *str2words_break_only_on_spaces( char *s )
{


  WordList * wl = NULL;
  int i = 0;
  char *cptr = NULL;
  char a_word[MAXLINE];

  wl = malloc ( sizeof ( WordList ) );

  wl->n = 0;

  wl->words = malloc ( sizeof ( char * ) * 255 );
  
  cptr = s;

  while (( *cptr != EOS ) && ( wl->n < 255 ))
    {
      
      if ( *cptr == SPACE )
	{
	  WLWN(wl,wl->n) = strdup( a_word );
	  strcpy(a_word,"");
	  wl->n++;
	  i = 0;
	}
      else
	{
	  a_word[i]= *cptr;
	  a_word[i+1] = EOS;
	  i++;
	}
      cptr++;
    }
  if ( strlen( a_word ) > 0 )
    {
      WLWN(wl,wl->n) = strdup( a_word );
      strcpy(a_word,"");
      wl->n++;
      i = 0;
    }

  return ( wl );
}

/*==========================================================
%FUNCTION NAME
        void add_word_to_list( 
  		      WordList **a_wl,   * Input/Output * 
		      char      *row     * input        * 
		      )
%PURPOSE
	add_word_to_list adds a term to the list
%HEADER END
==========================================================*/
void add_word_to_list( 
		      WordList **a_wl,  /* Input/Output */
		      char      *row  /* input */
		      )

{
  WordList *wl = NULL;

  wl = *a_wl;

  if ( wl != NULL )
    {
      if ( fmod(  (double)(wl->n), (double)255) == 0.0  )
	{
	  wl->words = realloc( wl->words, sizeof( char *) * (wl->n + 255 ));
	}
    }
  else
    {
      wl = malloc( sizeof( WordList ) );
      wl->words = malloc ( sizeof ( char *) * 255 );
      wl->n = 0;
    }

  WLWN( wl, wl->n) = strdup( row );
  wl->n++;
  
  *a_wl = wl;
  
}

/*==========================================================
%FUNCTION NAME
        is_on_list 
%PURPOSE
	?
%HEADER END
==========================================================*/
int is_on_list ( 
		char *term ,  /* Input */
		int   field,  /* Input */
		WordList* wl  /* Input */ 
		)
{
  int return_code = D_FALSE;
  int i = 0;
  char current_term[MAXLINE];
  char *row  = NULL ;
  
  /* ----------------------------------------------------------------
     We are going to assume that the list is pipe delimited, and the
     term in question is the n'th field, and the list is not sorted by
     that field.
     ---------------------------------------------------------------- */

  if ( wl != NULL )
    {
      for ( i = 0; i < wl->n; i++ )
	{
	  row = WLWN( wl, i );
	  
	  get_value_from_field( field, current_term, row );
	  
	  if ( strcasecmp( term, current_term ) == 0 )
	    {
	      return_code = D_TRUE;
	      goto bottom;
	    }
	}
      
    }
    bottom:
      
  return ( return_code );
}
  
/*==========================================================
%FUNCTION NAME
       sort_words_by_number 
%PURPOSE
	?
%HEADER END
==========================================================*/
void sort_words_by_number( char **a, int n,int columnNumber ) 
{
    int gap, i, j;
    char *cp = NULL;
    char *pattern1 = NULL;
    char *pattern2 = NULL;
    char rank1[20]; 
    char rank2[20]; 
    int r1 = 0;
    int r2 = 0;

    for (gap = n/2; gap>0; gap /= 2) {
	for (i=gap; i<n; i++) {
	    for (j=i-gap; j>=0; j-=gap) {

	       pattern1 = *(a+j);
	       pattern2 = *(a+j+gap);
	       
	       r1 = 0;
	       if ( pattern1 != NULL ) {
		 get_value_from_field(columnNumber,rank1, pattern1 );
		 r1 = atoi( rank1 );
	       }
	       
	       r2 = 0;
	       if ( pattern2 != NULL ) {
		 get_value_from_field(columnNumber,rank2, pattern2 );
		 r2 = atoi( rank2 );
	       }

	       if ( r1 < r2 )  /* if (strcmp(*(a+j), *(a+j+gap)) <= 0) */
		    break;
		cp = *(a+j);
		*(a+j) = *(a+j+gap);
		*(a+j+gap) = cp;
	    }
	}
    }
}
/**/
/*==========================================================
%FUNCTION NAME
	add_wordLists
%PURPOSE
	This function combines two wordLists into one.
%BUGS
	?
%FLAGS  
	TRACE DT2456
	FULL  DF2457
%HEADER END
==========================================================*/
int add_wordLists(
		  WordList **combinedList, /* Input/Output */
		  WordList  *list,         /* Input        */
		  int       *a,            /* Input/Output */
		  int       uniq           /* Input        */
		  )

{
  int return_code = D_S_SUCCESS;

  return_code = add_wordListsWithMsg( combinedList, list, a, uniq, NULL );

  return ( return_code );

} /*** End add_wordLists */
/**/
/*==========================================================
%FUNCTION NAME
	add_wordListsWithMsg
%PURPOSE
	This routine combines WordLists and attaches a message
	to the end of each line in the process.
%SYNTAX
	int add_wordListsWithMsg(
  			          WordList **combinedList,  * Input/Output * 
			          WordList  *list,          * Input        * 
			          int       *a,             * Input/Output * 
			          int       uniq,           * Input        * 
			          char      *msg            * Input        *  
			         )

%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2532
	FULL  DF2533
%HEADER END
==========================================================*/
int add_wordListsWithMsg(
			 WordList **combinedList, /* Input/Output */
			 WordList  *list,         /* Input        */
			 int       *a,            /* Input/Output */
			 int       uniq,          /* Input        */
			 char      *msg           /* Input        */ 
			 )


{
  int return_code = D_S_SUCCESS;
  WordList *cList = NULL;
  char term[MAXLINE];
  char tmp[MAXLINE];
  int count = 0;
  int j = 0;
  int al = 0;


  cList = *combinedList;
  al = *a;
  
  if ( cList == NULL ) {
    cList = malloc ( sizeof(WordList ) ); 
    cList->words = malloc ( sizeof( char *) * (list->n + LVG_DEFAULT_ALLOCATION ));
    al =  list->n + LVG_DEFAULT_ALLOCATION;
    cList->n = 0;
    j = 0;
    count = 0; 
    while ( j < list->n ) {
      if ( msg == NULL ) {
	cList->words[count] = strdup (list->words[j] ); 
      } else {
	sprintf(tmp,"%s%c%s", list->words[j], LVG_FIELD_SEPARATOR, msg );
	cList->words[count] = strdup ( tmp ); 
      }
      count++;
      j++;
    }
    cList->n = count; 
      

  } else {
    
    if ( list != NULL ) {
      
      if ( al <= ( cList->n + list->n + 1) ){

	cList->words = realloc( cList->words, (sizeof(int *) * 
			 (al + cList->n + list->n + LVG_DEFAULT_ALLOCATION)));
	al = al + cList->n + list->n + LVG_DEFAULT_ALLOCATION ;
	
      }
      
      j = 0;
      count = cList->n;
      while ( j < list->n ) {
	
	if ( uniq == D_TRUE ) {

	  get_value_from_field(2,term, list->words[j] );
	  if ( is_on_list ( term, 2, cList ) == D_FALSE ) {
	    if ( msg == NULL ) {
	      cList->words[count] = strdup (list->words[j] ); 
	    } else {
	      sprintf(tmp,"%s%c%s", list->words[j], LVG_FIELD_SEPARATOR, msg );
	      cList->words[count] = strdup ( tmp ); 
	    }
	    count++;
	  }
	} else {
	    if ( msg == NULL ) {
	      cList->words[count] = strdup (list->words[j] ); 
	    } else {
	      sprintf(tmp,"%s%c%s", list->words[j], LVG_FIELD_SEPARATOR, msg );
	
	 
	      cList->words[count] = strdup ( tmp ); 
	    }
	    count++;
	} 
	j++;
      }
      cList->n = count; 
      
    }
    
  }


  

  *combinedList = cList;
  *a = al;


  return ( return_code );

} /*** End add_wordListsWithMsg */
