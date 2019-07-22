/*==========================================================

%SOURCE FILE
	c_lexicon.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	11Mar98 divita -- Initial Version

%%
==========================================================*/

static char sccs_id_c_lexicon_c[] = "@(#)c_lexicon.c	1.4 10/16/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include <debug.h>
#include <lexicon.h>
#include <quintus.h>
#include <wl.h>

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#define KEY_FIELD        1
#define INFLECTED_FIELD  1
#define FLAG_FIELD       2
#define OFFSET_FIELD     3

#define CASE_SENSITIVE        0
#define CASE_INSENSITIVE      1
#define WORD_PREFIX           1
#define STRING_PREFIX         2 
#define EXACT                 3 

#define LEX_DEFAULT_ALLOCATION	64
#define LEX_MATCH_EXACT	0
#define LEX_MATCH_LOWER 1
#define LEX_MATCH_PUNCT	2


#define STRIP_WHITE_SPACE     2

/*end of constants ---------*/

/*----------------------------
%MACROS
----------------------------*/
/*end of macros ------------*/

/*----------------------------
%STATIC FUNCTIONS
----------------------------*/
/* end of static functions --*/

/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/
extern WordList *query_lex_info();
extern WordList *tokenize( char *term,           /* Input */
			   int tokenize_style    /* Input */
			   ) ;
extern void  free_wl ( );
extern int grab_lex_fields_from_btree_row(
					  char *row,             /* Input   */
					  lex_t *flag,           /* Output  */
					  long  *ofs,            /* Output  */ 
					  char  **inflected_term  /* Output */
					  );

/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
long int c_lex_cit();
long int c_lex_root();
long int c_lex_form();
long int c_lex_cit_cats();
long int c_lex_root_cats();
long int c_lex_form_cats();
long int c_lex_is_a_root();
long int c_lex_is_a_form();
long int c_lex_is_a_root_cats();
static lex_t get_cat( char *cat );
long int c_lex_form_input( 
			  char *indexFile,
			  long int singleWordLexicon,
			  QP_term_ref input,
			  QP_term_ref matchList 
			  );

static void *incr_buf_alloc();

/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/* structure for capturing lexicon header */
typedef struct _lexHead {
    lex_t flags;
    long int ofs;
} LexHead;

static LexHead *gLexHead = (LexHead *)NULL;
static int n_gLexHead = 0;
static int a_gLexHead = 0;

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

#define DT1576  1576          /* DT for c_lex_cit() */
#define DF1577  1577          /* DF for c_lex_cit() */
#define DT1578  1578          /* DT for c_lex_root() */
#define DF1579  1579          /* DF for c_lex_root() */
#define DT1580  1580          /* DT for c_lex_form() */
#define DF1581  1581          /* DF for c_lex_form() */
#define DT1582  1582          /* DT for c_lex_cit_cats() */
#define DF1583  1583          /* DF for c_lex_cit_cats() */
#define DT1584  1584          /* DT for c_lex_root_cats() */
#define DF1585  1585          /* DF for c_lex_root_cats() */
#define DT1586  1586          /* DT for c_lex_form_cats() */
#define DF1587  1587          /* DF for c_lex_form_cats() */
#define DT1590  1590          /* DT for c_lex_is_a_root() */
#define DF1591  1591          /* DF for c_lex_is_a_root() */
#define DT1592  1592          /* DT for c_lex_is_a_form() */
#define DF1593  1593          /* DF for c_lex_is_a_form() */
#define DT1596  1596          /* DT for c_lex_is_a_root_cats() */
#define DF1597  1597          /* DF for c_lex_is_a_root_cats() */
#define DT1642  1642          /* DT for get_cat() */
#define DF1643  1643          /* DF for get_cat() */
#define DT1644  1644          /* DT for c_lex_form_input() */
#define DF1645  1645          /* DF for c_lex_form_input() */
#define DT1646  1646          /* DT for incr_buf_alloc() */
#define DF1647  1647          /* DF for incr_buf_alloc() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_lex_cit
%PURPOSE
	searches for citation forms (base only) 
	gets the offsets of matching records whose citation form matches term.
        If the singleWordLexicon flag is turned on (=1), only
        those terms that contain orphan tokens are returned.
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_lex_cit(arg);
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
	TRACE DT1576
	FULL  DF1577
%HEADER END
==========================================================*/
long int c_lex_cit(
		   char       *indexFile,
		   char       *term,
		   long int    singleWordLexicon,  /* 0 if no, 1 if yes */
		   long int    lowerFlag,
		   long int    flushFlag,
		   QP_term_ref pOfsList 
		   )

{
  int i;
  long int          returnFlag     = 0;
  QP_atom           pNil           = QP_atom_from_string("[]");
  QP_term_ref       pOfs;
  long int          ofs            = 0;
  lex_t             flag           = NULL;
  WordList         *wl             = NULL;
  char             *inflected_term = NULL; 
  

  DFNAME("c_lex_cit");
  DENTER(DT1576);

  /* ----------------------------------------------
     Return a set of offsets for the citation form.
     ---------------------------------------------- */
  wl =  query_lex_info( term, CASE_INSENSITIVE, EXACT, indexFile ); 


  QP_put_atom(pOfsList, pNil);
  for (i=0; i<wl->n ; i++)
    {

      grab_lex_fields_from_btree_row ( WLWN(wl,i), &flag, &ofs, &inflected_term );
      
      CHAR_FREE( inflected_term );
      if (IS_LEX_BASE(flag))
	{
	  sprintf(msg,"%s is a baseform", term );
	  DPR(DF1577,msg);
	}
      
      
      if ( lowerFlag && IS_LEX_BASELOWER(flag))
	{
	  sprintf(msg,"case insensitive is on, and %s is a base lower term", term );
	  DPR(DF1577,msg);
	}
      

      if ((IS_LEX_BASE(flag) || (lowerFlag && IS_LEX_BASELOWER(flag))) &&
	  ( ( singleWordLexicon == 0 ) ||
	    ((singleWordLexicon == 1) && (CONTAINS_ORPHAN_TOKEN( flag)))
	   ))
	{
	  sprintf(msg,"Interested in the offsets here %d", ofs );
	  DPR(DF1577,msg);

	    pOfs = QP_new_term_ref();
	    QP_put_integer(pOfs, (long int)(ofs));
	    QP_cons_list(pOfsList, pOfs, pOfsList);
	    returnFlag = 1;
	}
    }
  
  free_wl ( &wl );
  
  DEXIT(DT1576);
  return ( returnFlag );
  
} /*** End c_lex_cit */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_lex_root
%PURPOSE
	gets the offsets of matching records 
	with 'term' as base or spelling variant
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_lex_root(arg);
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
	TRACE DT1578
	FULL  DF1579
%HEADER END
==========================================================*/
long int c_lex_root(
		    char *indexFile,
		    char *term,
		    long int singleWordLexicon,  /* 0 if no, 1 if yes */
		    long int lowerFlag,
		    long int flushFlag,
		    QP_term_ref pOfsList 
		    )

{
  int i;
  long int     returnFlag = 0;
  QP_atom      pNil       = QP_atom_from_string("[]");
  QP_term_ref  pOfs;
  lex_t        flag       = 0;
  long int     ofs        = 0;
  WordList    *wl         = NULL;
  char        *inflected_term = NULL; 
  

  DFNAME("c_lex_root");
  DENTER(DT1578);

  wl = query_lex_info( term, CASE_INSENSITIVE, EXACT, indexFile ); 

  QP_put_atom(pOfsList, pNil);

  for (i=0; i<wl->n ; i++)
    {
      grab_lex_fields_from_btree_row ( WLWN(wl,i), &flag, &ofs, &inflected_term );
      CHAR_FREE( inflected_term );
      
      if (( singleWordLexicon == 0 ) || 
	  (( singleWordLexicon == 1 ) &&  (CONTAINS_ORPHAN_TOKEN( flag) )))
	{
	  if ((IS_LEX_BASE(flag) || IS_LEX_SV(flag)) ||
	      (lowerFlag && (IS_LEX_BASELOWER(flag) || IS_LEX_SVLOWER(flag))))
	    {
	      pOfs = QP_new_term_ref();
	      QP_put_integer(pOfs, (long int)ofs);
	      QP_cons_list(pOfsList, pOfs, pOfsList);
	      returnFlag = 1;
	    }
	}
    }
  
  free_wl ( &wl );
  
  DEXIT(DT1578);
  return(returnFlag);

} /*** End c_lex_root */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_lex_form
%PURPOSE
	returns the offsets of records matched by term
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_lex_form(arg);
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
	TRACE DT1580
	FULL  DF1581
%HEADER END
==========================================================*/
long int c_lex_form(
		    char       *indexFile,
		    char       *term,    	/* should be +string */
		    long int    singleWordLexicon,  /* 0 if no, 1 if yes */
		    long int    lowerFlag,
		    long int    flushFlag,
		    QP_term_ref pOfsList 
		    )

{
  int i = 0;
  QP_atom paNil = QP_atom_from_string("[]");
  long int returnFlag = 0;
  QP_term_ref pOfs;
  lex_t flag;
  long int     ofs        = 0;
  WordList *wl = NULL;
  char *inflected_term = NULL; 
  

  DFNAME("c_lex_form");
  DENTER(DT1580);
  
  wl = query_lex_info( term, lowerFlag, EXACT, indexFile ); 
  
  
  /* construct return list */
  
  QP_put_atom(pOfsList, paNil);
  
  for (i=0; i<wl->n ; i++)
    {
      grab_lex_fields_from_btree_row ( WLWN(wl,i), &flag, &ofs, &inflected_term );

      if ( ( singleWordLexicon == 0 ) ||
	   ((singleWordLexicon == 1) && (CONTAINS_ORPHAN_TOKEN( flag))) ) {

	pOfs = QP_new_term_ref();
	QP_put_integer(pOfs, (long int)ofs);
	QP_cons_list(pOfsList, pOfs, pOfsList);
	returnFlag = 1;
	sprintf(msg,"%s^|^%s|%d", WLWN(wl,i),inflected_term, ofs );
	DPR(DF1581,msg);
        
      } else {
        
	sprintf(msg,"%s is a multiword, ignoring because in singleWordLexicon mode", inflected_term );
	DPR(DF1581,msg);
	
      }
      
      CHAR_FREE( inflected_term );
    }

  free_wl ( &wl );
  /* wl = NULL; */
  
  DEXIT(DT1580);
  
  return(returnFlag);

} /*** End c_lex_form */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_lex_cit_cats
%PURPOSE
	returns the categories that a citation form belongs to
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_lex_cit_cats(arg);
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
	TRACE DT1582
	FULL  DF1583
%HEADER END
==========================================================*/
long int c_lex_cit_cats(
			char       *indexFile,
			char       *term,	/* should be +string */
			long int    singleWordLexicon,  /* 0 if no, 1 if yes */
			long int    lowerFlag,
			QP_term_ref pCats 
			)

{
  long int return_code = 0; 
  lex_t lCat = 0;
  long int     ofs        = 0;
  int i;
  static char *sCat[] = { "adj",   "adv",  "aux",  "compl", "conj", "det",
			  "modal", "noun", "prep", "pron",  "verb" };

  QP_atom paNil       = QP_atom_from_string("[]");
  lex_t flag   = 0;
  WordList *wl = NULL;
  char *inflected_term = NULL; 
  

  DFNAME("c_lex_cit_cats");
  DENTER(DT1582);

  wl = query_lex_info( term, lowerFlag, EXACT, indexFile ); 


  for (i=0; i<wl->n ; i++)
    {
      grab_lex_fields_from_btree_row ( WLWN(wl,i), &flag, &ofs, &inflected_term );

      if (( singleWordLexicon == 0 ) || 
	  (( singleWordLexicon == 1 ) &&  (CONTAINS_ORPHAN_TOKEN( flag) )))
	
	{
	  if (IS_LEX_BASE(flag) || (lowerFlag && IS_LEX_BASELOWER(flag)))
	    lCat |= (flag & (lex_t)2047);   /* extract just the cats */
	  CHAR_FREE( inflected_term );
	}
    }


  if (lCat == 0)
    {
      return_code = 0; 
    }
  else
    {

      /* construct return list */
      QP_put_atom(pCats, paNil);
      for (i=0; i<11; i++)
	{
	  if (lCat & (0x1 << i))
	    {
	      QP_term_ref pCat = QP_new_term_ref();

	      QP_put_atom(pCat, QP_atom_from_string(sCat[i]));
	      QP_cons_list(pCats, pCat, pCats);
	    }
	}
    return_code = 1;
    }

  free_wl ( &wl );

  DEXIT(DT1582);
  return ( return_code );

} /*** End c_lex_cit_cats */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_lex_root_cats
%PURPOSE
	returns the categories that a root form belongs to 
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_lex_root_cats(arg);
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
	TRACE DT1584
	FULL  DF1585
%HEADER END
==========================================================*/
long int c_lex_root_cats(
			 char       *indexFile,
			 char       *term,	/* should be +string */
			 long int    singleWordLexicon,  /* 0 if no, 1 if yes */
			 long int    lowerFlag,
			 QP_term_ref pCats 
			 )

{
  long int return_code = D_S_SUCCESS;
  lex_t lCat = 0;
  int i;
  static char *sCat[] = {"adj", "adv", "aux", "compl", "conj", 
			 "det", "modal", "noun", "prep", "pron", "verb" };

  QP_atom       paNil          = QP_atom_from_string("[]");
  lex_t         flag           = 0;
  long int      ofs            = 0;
  WordList     *wl             = NULL;
  char         *inflected_term = NULL; 
  


  DFNAME("c_lex_root_cats");
  DENTER(DT1584);


  wl = query_lex_info( term, lowerFlag, EXACT, indexFile ); 
  


  for (i=0; i<wl->n ; i++)
    {
      grab_lex_fields_from_btree_row ( WLWN(wl,i), &flag, &ofs, &inflected_term );
      
      if (( singleWordLexicon == 0 ) || 
	  (( singleWordLexicon == 1 ) &&  (CONTAINS_ORPHAN_TOKEN( flag) )))
	{	
	  if ((IS_LEX_BASE(flag) || IS_LEX_SV(flag)) ||
	      (lowerFlag && (IS_LEX_BASELOWER(flag) || IS_LEX_SVLOWER(flag))))
	    {
	      lCat |= (flag & (lex_t)2047);   /* extract just the cats */
	    }
	  CHAR_FREE( inflected_term );
	}

    }
  if (lCat == 0)
    {
      return_code = 0;
    }
  else
    {

      /* construct return list */
      QP_put_atom(pCats, paNil);
      for (i=0; i<11; i++)
	{
	  if (lCat & (0x1 << i))
	    {
	      QP_term_ref pCat = QP_new_term_ref();
	      
	      QP_put_atom(pCat, QP_atom_from_string(sCat[i]));
	      QP_cons_list(pCats, pCat, pCats);
	    }
	}
      return_code = 1;
    }

  free_wl ( &wl );

  DEXIT(DT1584);
  return ( return_code );

} /*** End c_lex_root_cats */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_lex_form_cats
%PURPOSE
	returns the categories that a lexical item belongs to 
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_lex_form_cats(arg);
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
	TRACE DT1586
	FULL  DF1587
%HEADER END
==========================================================*/
long int c_lex_form_cats(
			 char *indexFile,
			 char *term,			/* should be +string */
			 long int singleWordLexicon,  /* 0 if no, 1 if yes */
			 long int lowerFlag,
			 QP_term_ref pCats 
			 )
{
  long int return_code = D_S_SUCCESS;
  lex_t lCat = 0;
  int i;
  static char *sCat[] = {
    "adj", "adv", "aux", "compl", "conj", "det", "modal", "noun", "prep", "pron", "verb"
  };
  QP_atom paNil = QP_atom_from_string("[]");
  lex_t flag = 0;
  long int      ofs            = 0;
  WordList *wl = NULL;
  char *inflected_term = NULL; 
  
  

  DFNAME("c_lex_form_cats");
  DENTER(DT1586);


  wl = query_lex_info( term, lowerFlag, EXACT, indexFile ); 

  for (i=0; i<wl->n ; i++)
    {
      grab_lex_fields_from_btree_row ( WLWN(wl,i), &flag, &ofs, &inflected_term );
      
      CHAR_FREE( inflected_term );
      inflected_term = NULL;
      
      if (( singleWordLexicon == 1 ) &&  (!CONTAINS_ORPHAN_TOKEN( flag) ))
	continue;
      
      if (!lowerFlag &&
	  (   IS_LEX_BASELOWER(flag) ||
	      IS_LEX_SVLOWER(flag) ||
	      IS_LEX_BASEINFLLOWER(flag) ||
	      IS_LEX_SVINFLLOWER(flag)))
	continue;

      lCat |= (flag & (lex_t)2047);   /* extract just the cats */

    }
  
  if (lCat == 0)
    {
      return_code = 0;
    }
  else
    {

      /* construct return list */
      QP_put_atom(pCats, paNil);
      for (i=0; i<11; i++)
	{
	  if (lCat & (0x1 << i))
	    {
	      QP_term_ref pCat = QP_new_term_ref();
	      
	      QP_put_atom(pCat, QP_atom_from_string(sCat[i]));
	      QP_cons_list(pCats, pCat, pCats);
	    }
	}

      return_code = 1;
    }

  free_wl ( &wl );

  DEXIT(DT1586);
  return ( return_code );

} /*** End c_lex_form_cats */
/**/

/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_lex_is_a_root
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_lex_is_a_root(arg);
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
	TRACE DT1590
	FULL  DF1591
%HEADER END
==========================================================*/
long int c_lex_is_a_root(
			 char *indexFile,
			 char *term,			/* should be +string */
			 long int singleWordLexicon,    /* 0 if no, 1 if yes */
			 long int lowerFlag 
			 )

{
  
  long int return_code     = 0; 
  int      i               = 0;
  int      found           = D_FALSE;
  lex_t    flag            = 0;
  long int ofs             = 0;
  WordList *wl             = NULL;
  char     *inflected_term = NULL; 
  

  DFNAME("c_lex_is_a_root:");
  DENTER(DT1590);


  /* returns 1 if term is in the lexicon as citation form */

  wl = query_lex_info( term, lowerFlag, EXACT, indexFile ); 


  while  ( ( found == D_FALSE ) && ( i < wl->n ))
    {
      grab_lex_fields_from_btree_row ( WLWN(wl,i), &flag, &ofs, &inflected_term );
      CHAR_FREE( inflected_term );
      

      /* returns 1 if term is in the lexicon as a root */
      if ((( singleWordLexicon == 0 ) || 
	  (( singleWordLexicon == 1 ) &&  (CONTAINS_ORPHAN_TOKEN( flag) ))) &&
	
	   ((IS_LEX_BASE(flag) || IS_LEX_SV(flag)) ||
	    (lowerFlag && (IS_LEX_BASELOWER(flag) || IS_LEX_SVLOWER(flag)))))
	  {
	    return_code = 1;
	    found = D_TRUE;
	  }
	else
	  {
	    i++;
	  }
    }
  
  free_wl ( &wl );

  DEXIT(DT1590);
  return ( return_code );

} /*** End c_lex_is_a_root */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_lex_is_a_form
%PURPOSE
	returns 1 if term is in the lexicon, 0 it not
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_lex_is_a_form(arg);
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
	TRACE DT1592
	FULL  DF1593
%HEADER END
==========================================================*/
long int c_lex_is_a_form(
			 char *indexFile,
			 char *term,			/* should be +string */
			 long int singleWordLexicon,  /* 0 if no, 1 if yes */
			 long int lowerFlag 
			 )
     
{
  
  long int return_code     = 0; 
  int      i               = 0;
  int      found           = D_FALSE;
  long int ofs             = 0;
  lex_t    flag            = 0;
  WordList *wl             = NULL;
  char     *inflected_term = NULL; 
  
  
  DFNAME("c_lex_is_a_form");
  DENTER(DT1592);
  
    
    wl = query_lex_info( term, lowerFlag, EXACT, indexFile ); 
    
    if (( lowerFlag ) && ( wl->n > 0 ))
      {
	return_code = 1;
      }
    else
      {
	
	while  ( ( found == D_FALSE ) && ( i < wl->n ))
	  {
	    grab_lex_fields_from_btree_row ( WLWN(wl,i), &flag, &ofs, &inflected_term );
	    CHAR_FREE( inflected_term );
	    
	    if ( IS_LEX_BASELOWER(flag)     ||
		 IS_LEX_SVLOWER(flag)       ||
		 IS_LEX_BASEINFLLOWER(flag) ||
		 IS_LEX_SVINFLLOWER(flag)   ||
		 ((singleWordLexicon == 1 ) &&  (!CONTAINS_ORPHAN_TOKEN( flag) )))
	      {
		i++;
	      }
	    else
	      {
		return_code = 1;
		found = D_TRUE;
	      } 
	  }
	
      }
  
    
    free_wl ( &wl );
    
    DEXIT(DT1592);
    
    return ( return_code );
} /*** End c_lex_is_a_form */
/**/

/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_lex_is_a_root_cats
%PURPOSE
	succeeds if term is a root form in any of cats
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_lex_is_a_root_cats(arg);
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
	TRACE DT1596
	FULL  DF1597
%HEADER END
==========================================================*/
long int c_lex_is_a_root_cats(
			      char *indexFile,
			      char *term,			/* should be +string */
			      long int singleWordLexicon,       /* 0 if no, 1 if yes */
			      long int lowerFlag,
			      QP_term_ref pCats 		/* +term */
			      )
  
  {

    long int   return_code     = 0; 
    int        i               = 0;
    int        found           = D_FALSE;
    long int   ofs             = 0;
    lex_t      lCat            = 0;
    lex_t      flag            = 0;
    WordList   *wl             = NULL;
    char       *inflected_term = NULL; 
    QP_term_ref prHead         = QP_new_term_ref();
    QP_term_ref prTail         = QP_new_term_ref();
    QP_atom     paAtom;

  
    DFNAME("c_lex_is_a_root_cats");
    DENTER(DT1596);

    /* succeeds if term is a root form in any of cats */

	
    wl = query_lex_info( term, lowerFlag, EXACT, indexFile ); 

    QP_get_list(pCats, prHead, prTail);
    QP_get_atom(prHead, &paAtom);
    lCat |= get_cat(QP_string_from_atom(paAtom));

    while (QP_is_list(prTail))
    {
	QP_get_list(prTail, prHead, prTail);
	QP_get_atom(prHead, &paAtom);
	lCat |= get_cat(QP_string_from_atom(paAtom));
    }

    while  ( ( found == D_FALSE ) && ( i < wl->n ))
      {
	grab_lex_fields_from_btree_row ( WLWN(wl,i), &flag, &ofs, &inflected_term );
	CHAR_FREE( inflected_term );
	
	if ((( singleWordLexicon == 0 ) || 
	     (( singleWordLexicon == 1 ) &&  (CONTAINS_ORPHAN_TOKEN( flag) ))) &&
	    
	    ((flag & lCat) && ((IS_LEX_BASE(flag) || IS_LEX_SV(flag)) ||
			       (lowerFlag && (IS_LEX_BASELOWER(flag) || IS_LEX_SVLOWER(flag))))))
	{
	  return_code = 1;
	  found = D_TRUE;
	}
	else
	  {
	    i++;
	  }
      }

    free_wl ( &wl );

  DEXIT(DT1596);
  return ( return_code );

} /*** End c_lex_is_a_root_cats */
/**/

/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_cat
%PURPOSE
	returns category as a lex_t 
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_cat(arg);
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
	TRACE DT1642
	FULL  DF1643
%HEADER END
==========================================================*/
static lex_t get_cat( char *cat )
     
{

  lex_t  return_code = (lex_t) 0;
  static struct _catStruct 
  {
    char *cat;
    lex_t lcat;
  } 
  
  cats[] = {
    "adj", LEX_CAT_ADJ,
    "adv", LEX_CAT_ADV,
    "aux", LEX_CAT_AUX,
    "compl", LEX_CAT_COMPL,
    "conj", LEX_CAT_CONJ,
    "det", LEX_CAT_DET,
    "modal", LEX_CAT_MODAL,
    "noun", LEX_CAT_NOUN,
    "prep", LEX_CAT_PREP,
    "pron", LEX_CAT_PRON,
    "verb", LEX_CAT_VERB
  };
  int low   = 0;
  int high  = 10;
  int mid   = 0;
  int cmp   = 0;
  int found = D_FALSE;
  

  DFNAME("get_cat");
  DENTER(DT1642);

  while ((low<=high) && ( found == D_FALSE ))
    {
      mid = (low+high)/2;
      cmp = strcmp(cat, cats[mid].cat);
      if (cmp > 0)
	low = mid+1;
      else if (cmp < 0)
	high = mid-1;
      else
	{
	  found = D_TRUE;
	  return_code =  cats[mid].lcat ;
	}
    }
  
  DEXIT(DT1642);
  return ( return_code );

} /*** End get_cat */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_lex_form_input
%PURPOSE
	
   Given a list of tokens in input, this function returns a list of
   possible matches in matchList.  The matchList elements are terms
   of the following type.

     match(exact, matchTerm, Ofs, Length)
     match(lower, matchTerm, Ofs, Length)
     match(punct, matchTerm, Ofs, Length)

   where:

   the first argument indicates exact match, lower case match,
	or a match ignoring punctuation tokens,
   matchTerm is the matching term (from the index),
   Ofs is the matching record offset,
   Length is the number of tokens from input that were matched.

   Note that if the first token in the input is a punctuation token,
   it is not ignored and the function returns failure.


%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_lex_form_input(arg);
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
	TRACE DT1644
	FULL  DF1645
%HEADER END
==========================================================*/
long int c_lex_form_input( 
			  char *indexFile,
			  long int    singleWordLexicon,  /* 0 if no, 1 if yes */
			  QP_term_ref input,
			  QP_term_ref matchList 
			  )
{

    int matchFlag;
    int returnFlag = 0;
    int i;
    int j;
    int l;
    lex_t      flag            = 0;
    long int   ofs             = 0;
    WordList   *inputTokens   = NULL;
    WordList   *wl             = NULL;
    WordList   *indexTokens    = NULL;
    char       first_term[MAXLINE];
    char       *inflected_form = NULL;  
    int        wl_ctr          = 0;

    QP_atom paMatch = QP_atom_from_string("match");
    QP_atom paExact = QP_atom_from_string("exact");
    QP_atom paLower = QP_atom_from_string("lower");
    QP_atom paPunct = QP_atom_from_string("punct");
    QP_atom paNil = QP_atom_from_string("[]");
    QP_atom paAtom;
    QP_term_ref prHead = QP_new_term_ref();
    QP_term_ref prTail = QP_new_term_ref();
    QP_term_ref prTerm = QP_new_term_ref();

    DFNAME("c_lex_form_input");
    DENTER(DT1644);
    
    /* malloc lots of space for inputTokens */
    inputTokens = malloc ( sizeof( WordList )  );
    inputTokens->words = (char ** ) malloc ( sizeof ( char *) * 2000 );
    inputTokens->n = 0;

   /* initialize */
    QP_put_atom(matchList, paNil);

   /* set input token pointers */
    if (!QP_get_list(input, prHead, prTail))
    {
	fprintf(stderr, "List expected\n");
	return(0);
    }
    if (!QP_is_atom(prHead))
    {
	fprintf(stderr, "Atom expected\n");
	return(0);	
    }
    QP_get_atom(prHead, &paAtom);
   
    WLWN( inputTokens, inputTokens->n ) = strdup ( QP_string_from_atom(paAtom) ); 

   /* I'm assuming this is mallocing space*/


   /* fail on null input */
    if (strcmp(WLWN( inputTokens, inputTokens->n ), "[]") == 0)
      {
	returnFlag = 0;
	goto bottom;
      }
    else
      {
	inputTokens->n++; 
      }

    while (QP_is_list(prTail))
    {
	QP_get_list(prTail, prHead, prTail);
		
	if (!QP_is_atom(prHead))
	{
	    fprintf(stderr, "Atom expected\n");
	    {
	      returnFlag = 0;
	      goto bottom;
	    }
	}
	QP_get_atom(prHead, &paAtom);
	WLWN( inputTokens, inputTokens->n ) = strdup( QP_string_from_atom(paAtom));
	inputTokens->n++;
    }

   /* make up search keys based on first input token */

    strcpy(first_term, WLWN( inputTokens, 0 ) );

    wl = query_lex_info( first_term, CASE_INSENSITIVE, WORD_PREFIX, indexFile ); 
	

    if ( wl == NULL )
      {
	sprintf(msg,"%s: not found", first_term);
	DPR(DF1645,msg);
	goto bottom;
      }
    
    for ( wl_ctr = 0; wl_ctr < wl->n ; wl_ctr ++ ) {
	grab_lex_fields_from_btree_row ( WLWN(wl,wl_ctr), &flag, &ofs, &inflected_form );


	if (( singleWordLexicon == 1 ) &&  ( !CONTAINS_ORPHAN_TOKEN( flag) )) {
        
	  sprintf(msg,"The inflected form |%s| is a multi-word", inflected_form );
	  DPR(DF1645,msg);
	  CHAR_FREE( inflected_form );
	  inflected_form = NULL;
	  continue;
        }

	/* ----------------------------
	   tokenize the inflected term  
	   (ignoring white space)
	   ---------------------------- */
	sprintf(msg,"The inflected form is |%s|", inflected_form );
	DPR(DF1645,msg);
	

	indexTokens = (WordList *)tokenize(inflected_form, STRIP_WHITE_SPACE );
	
	/* ----------------------------
	   do a tokenwise comparison of 
	   inputTokens and indexTokens 
	   ---------------------------- */

	for (l = 0, j = 0, matchFlag=LEX_MATCH_EXACT;;) { /* loop*/
	  if (j == indexTokens->n ) {  /* if j = noTokens */
	    QP_term_ref prFlag      = QP_new_term_ref();
	    QP_term_ref prMatchTerm = QP_new_term_ref();
	    QP_term_ref prOfs       = QP_new_term_ref();
	    QP_term_ref prLength    = QP_new_term_ref();
	    
	    /* -------------------------------------
	       add matching entry to the output list 
	       ------------------------------------- */
	    
	    switch (matchFlag) {  /* switch */
	    case LEX_MATCH_EXACT:
	      QP_put_atom(prFlag, paExact); break;
	    case LEX_MATCH_LOWER:
	      QP_put_atom(prFlag, paLower); break;
	    case LEX_MATCH_PUNCT:
	      QP_put_atom(prFlag, paPunct); break;
	    default:
	      break;
	    } /* end of switch */
	    
	    paAtom = QP_atom_from_string( inflected_form );
	    
	    sprintf(msg,"Adding atom |%s|", inflected_form );
	    DPR(DF1645,msg);
	    
	    
	    QP_put_atom(prMatchTerm, paAtom);
	    
	    QP_put_integer(prOfs, ofs ); 
	    QP_put_integer(prLength, (long int) l);
	    QP_cons_functor(prTerm, paMatch, 4, prFlag, prMatchTerm, prOfs, prLength);
	    
	    /* prepend to output list */
	    QP_cons_list(matchList, prTerm, matchList);
	    returnFlag = 1;
	    break;
	  } /* end if the j == no of tokens */
	  
	  if ( l == inputTokens->n ) {
	    break;
	  }
	  if (strcmp(WLWN(inputTokens,l), WLWN(indexTokens,j)) == 0) {

	    if ((IS_LEX_BASELOWER( flag ) || IS_LEX_SVLOWER(flag)) && (matchFlag == LEX_MATCH_EXACT))

	      matchFlag = LEX_MATCH_LOWER;
	    l++;
	    j++;
	  } else if (strcasecmp(WLWN(inputTokens,l), WLWN(indexTokens,j)) == 0) {
	    matchFlag = LEX_MATCH_LOWER;
	    l++;
	    j++;
	  } else if (strlen(WLWN(inputTokens,l)) == 1 && ispunct(*(WLWN(inputTokens,l)))) {
	    matchFlag = LEX_MATCH_PUNCT;
	    l++;
	  } else if (strlen(WLWN(indexTokens,j)) == 1 && ispunct(*(WLWN(indexTokens,j)))) {
	    matchFlag = LEX_MATCH_PUNCT;
	    j++;
	  } else {
	    break;
	  }
	} /* End for loop through the tokenized index term */ 

	CHAR_FREE( inflected_form );
        inflected_form = NULL;
	free_wl( &indexTokens );
	indexTokens = NULL;
	
    } /* End for loop through each index term  */



 bottom:

    free_wl ( &inputTokens );
    inputTokens = NULL;
    
    free_wl( &wl );
    wl = NULL;

    DEXIT(DT1644);

    return(returnFlag);
    
} /*** End c_lex_form_input */

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	incr_buf_alloc
%PURPOSE
    Increases the allocation of a buffer, buf using malloc/realloc.
    buf has alloc bytes currently allocated and size bytes are in use.
    n additional bytes are needed.  Allocation jumps in steps of incr.

%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = incr_buf_alloc(arg);
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
	TRACE DT1646
	FULL  DF1647
%HEADER END
==========================================================*/
static void *incr_buf_alloc(
			    void *buf,	/* pointer to buffer */
			    int unit,	/* size of one of what buf points to in bytes */
			    int size,	/* current size of buffer in units */
			    int *alloc,	/* current allocation of buffer in units */
			    int n,	/* units needed */
			    int incr 	/* increment size in units */
			    )


{

  void *return_code = NULL;

  DFNAME("incr_buf_alloc");
  DENTER(DT1646);

  if ((size + n) > *alloc)
    {
      while ((size+n) > *alloc)
	*alloc += incr;
      if (size == 0)
	{
	  if ((buf = (void *) malloc((unsigned)(unit*(*alloc)))) == (void *)NULL)
	    return_code = (void *) NULL;
	  else
	    return_code = buf;
	}
      else
	{
	  if ((buf = (void *) realloc((char *)buf, (unsigned)(unit*(*alloc)))) == (void *)NULL)
	    return_code = (void *) NULL;
	  else
	    return_code = buf;
	}
    }
  
  DEXIT(DT1646);

  return(buf);

} /*** End incr_buf_alloc */

