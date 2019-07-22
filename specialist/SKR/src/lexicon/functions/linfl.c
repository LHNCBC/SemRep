
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
	linfl.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	11Mar98 divita -- Initial Version

%%
==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include <stdio.h>
#include <debug.h>
#include <lexicon.h>
#include <lm.h>
#include <stdlib.h>

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#define PIPE '|'
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
extern void llip(unsigned char *s);
extern LmStruct *lm_variants(char *term, lm_t cats, lm_t rules, lm_t infls, int *numV);
/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
LexIndexKey * lex_index_keys(char *record, int  *num);
char *get_line(char *record);
static int get_cat(char *cat, int *lexCat, int *inflCat, int *inflInfl);
static int lex_load(char *term, int flags);
static int gen_and_load(int lexCat, int inflCat, int inflRule, int inflInfl);
static void * lex_reallocate_buffer(void *buf, int unit, int size, int *alloc, int n, int incr);
/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/

static LexIndexKey *keyBuf = (LexIndexKey *)NULL;
static int n_keyBuf = 0;
static int a_keyBuf = 0;

static char *charBuf = (char *)NULL;
static int n_charBuf = 0;
static int a_charBuf = 0;

static int baseOfs;		/* offset into char buf for base */
static int *svoBuf = (int *)NULL;   /* offsets into char buf for spelling variants */
static int n_svoBuf = 0;
static int a_svoBuf = 0;

static int catFlags = 0;	/* all categories for record */
static int lexCat = 0;		/* current lexical category */
static int inflCat = 0;		/* current infl category */
static int inflInfl = 0;	/* current infl inflection */

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

#define DT1604  1604          /* DT for lex_index_keys() */
#define DF1605  1605          /* DF for lex_index_keys() */
#define DT1606  1606          /* DT for get_line() */
#define DF1607  1607          /* DF for get_line() */
#define DT1608  1608          /* DT for get_cat() */
#define DF1609  1609          /* DF for get_cat() */
#define DT1610  1610          /* DT for lex_load() */
#define DF1611  1611          /* DF for lex_load() */
#define DT1612  1612          /* DT for gen_and_load() */
#define DF1613  1613          /* DF for gen_and_load() */
#define DT1614  1614          /* DT for lex_reallocat_buffer() */
#define DF1615  1615          /* DF for lex_reallocat_buffer() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	lex_index_keys
%PURPOSE
	derives the index keys for a lexical record 
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lex_index_keys(arg);
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
	TRACE DT1604
	FULL  DF1605
%HEADER END
==========================================================*/
LexIndexKey * lex_index_keys(
			     char *record,   /* Input   */
			     int  *num       /* Output */
			     )
{
  /*  int return_code = D_S_SUCCESS; */
  char *line = NULL;
  char *lp;
  char *np;
  int i;
  int size;
  int startFlag = 0;


  DFNAME("lex_index_keys");
  DENTER(DT1604);

  catFlags = n_charBuf = n_svoBuf = n_keyBuf = 0;

/* read record */


  while ( strlen( (line = get_line( record ) )) != 0 )
    {
	startFlag = 1;
	line[strlen(line)-1] = EOS;
	for (lp=line; isspace((int)*lp); lp++)
	    ;

	if (strncmp(lp, "{base=", 6) == 0)
	{
	    lp += 6;
	    size = strlen(lp)+1;
	    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf,
							  sizeof(char),
							  n_charBuf,
							  &a_charBuf,
							  size,
							  LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
	    return((LexIndexKey *)NULL);
	    (void) strcpy(&charBuf[n_charBuf], lp);
	    baseOfs = n_charBuf;
	    n_charBuf += size;
	}
	else if (strncmp(lp, "spelling_variant=", 17) == 0)
	{
	    lp += 17;
	    size = strlen(lp)+1;
	    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf,
							  sizeof(char),
							  n_charBuf,
							  &a_charBuf,
							  size,
							  LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
		return((LexIndexKey *)NULL);
	    (void) strcpy(&charBuf[n_charBuf], lp);
	    if ((svoBuf = (int *) lex_reallocate_buffer((void *)svoBuf,
							sizeof(int),
							n_svoBuf,
							&a_svoBuf,
							(int)1,
							LEX_DEFAULT_ALLOCATION*4)) == (int *)NULL)
		return((LexIndexKey *)NULL);
	    *(svoBuf+n_svoBuf) = n_charBuf;
	    n_svoBuf++;
	    n_charBuf += size;
	}
	else if (strncmp(lp, "entry=", 6) == 0)
	{
	    lexCat = inflCat = 0;
	}
	else if (strncmp(lp, "cat=", 4) == 0)
	{
	    lp += 4;
	    if (!get_cat(lp, &lexCat, &inflCat, &inflInfl))
	    {
		lexCat = inflCat = inflInfl = 0;		
	    }
	    catFlags |= lexCat;
	}
	else if (strncmp(lp, "variants=", 9) == 0)
	{
	    lp += 9;
	    if (strcmp(lp, "reg") == 0 || strcmp(lp, "group(reg)") == 0)
	    {
		if (inflCat > 0)
		    if (!gen_and_load(lexCat, inflCat, (int)LM_RULE_REG, inflInfl))
			fprintf(stderr, "ERROR: Cannot generate 'reg' variants for: %s\n", charBuf+baseOfs);
	    }
	    else if (strcmp(lp, "regd") == 0)
	    {
		if (inflCat > 0)
		    if (!gen_and_load(lexCat, inflCat, (int)LM_RULE_REGD, inflInfl))
			fprintf(stderr, "ERROR: Cannot generate 'regd' variants for: %s\n", charBuf+baseOfs);
	    }
	    else if (strcmp(lp, "glreg") == 0 || strcmp(lp, "group(glreg)") == 0)
	    {
		if (inflCat > 0)
		    if (!gen_and_load(lexCat, inflCat, (int)LM_RULE_GLREG, inflInfl))
			fprintf(stderr, "ERROR: Cannot generate 'glreg' variants for: %s\n", charBuf+baseOfs);
	    }
	    else if (strcmp(lp, "metareg") == 0 || strcmp(lp, "group(metareg)") == 0)
	    {
		if (inflCat > 0)
		{
/* generate the "s" variant for base */
		    size = strlen(charBuf+baseOfs)+3;
		    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
			    &a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
			return((LexIndexKey *)NULL);
		    (void) strcpy(charBuf+n_charBuf, charBuf+baseOfs);
		    (void) strcat(charBuf+n_charBuf, "s");
		    if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
			return((LexIndexKey *)NULL);

/* generate the "'s" variant for base */
		    (void) strcpy(charBuf+n_charBuf, charBuf+baseOfs);
		    (void) strcat(charBuf+n_charBuf, "'s");
		    if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
			return((LexIndexKey *)NULL);

		    for (i=0; i<n_svoBuf; i++)
		    {

/* generate the "s" variant for spelling variant */
			size = strlen(charBuf+(int)(*(svoBuf+i)))+3;
			if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
				&a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
			    return((LexIndexKey *)NULL);
			(void) strcpy(charBuf+n_charBuf, charBuf+(int)(*(svoBuf+i)));
			(void) strcat(charBuf+n_charBuf, "s");
			if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
			    return((LexIndexKey *)NULL);

/* generate the "'s" variant for spelling variant */
			(void) strcpy(charBuf+n_charBuf, charBuf+(int)(*(svoBuf+i)));
			(void) strcat(charBuf+n_charBuf, "'s");
			if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
			    return((LexIndexKey *)NULL);
		    }
		}
	    }
	    else if (strncmp(lp, "group(irreg|", 12) == 0)
	    {
		char *tp;
		/*fprintf(stderr, "found a group irreg= [%s]\n", lp);*/

		if (inflCat == 0)
		    continue;

		lp += 12;

	        if ((np = strchr(lp, PIPE)) == (char *)NULL)
		  break;
	        lp = np + 1;
	        /*fprintf(stderr, "found a group irreg np = [%s]\n", np);*/
	        /*fprintf(stderr, "found a group irreg lp = [%s]\n", lp);*/

		if ((tp = strchr(lp, PIPE)) == (char *)NULL)
		    break;
	        /*fprintf(stderr, "found a group irreg tp = [%s]\n", tp); */
		if ((tp-lp) > 0)
		{
		    /*fprintf(stderr, "size = = [%d]\n", tp-lp);*/
		    size = (tp-lp)+1;
		    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf,
								  sizeof(char),
								  n_charBuf,
								  &a_charBuf,
								  size,
								  LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
			return((LexIndexKey *)NULL);
		    (void) strncpy(charBuf+n_charBuf, lp, (size_t)(tp-lp));
		    *(charBuf+n_charBuf+(tp-lp)) = EOS;
		    /*fprintf(stderr, "the group irreg= [%s]\n",  charBuf+n_charBuf );*/
		    if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
			return((LexIndexKey *)NULL);
		}
	      break;
	    }
	    else if (strncmp(lp, "irreg|", 6) == 0)
	    {
		char *tp;
		/*fprintf(stderr, "Found a irreg = %s\n", lp ); */

		if (inflCat == 0)
		    continue;

		lp += 6;
		switch (lexCat)
		{
		case LEX_CAT_ADJ:
		case LEX_CAT_ADV:

		  /* get Base - we are going pass over this*/
		  if ((np = strchr(lp, PIPE)) == (char *)NULL)
		    break;
		  lp = np + 1;
		  /* get comparative */
			if ((tp = strchr(lp, PIPE)) == (char *)NULL)
			    break;
			if ((tp-lp) > 0)
			{
			    size = (tp-lp)+1;
			    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
				    &a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
				return((LexIndexKey *)NULL);
			    (void) strncpy(charBuf+n_charBuf, lp, (size_t)(tp-lp));
			    *(charBuf+n_charBuf+(tp-lp)) = EOS;

			/* all irreg variants are assigned as inflections of the base */
			    if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
				return((LexIndexKey *)NULL);
		        /*fprintf(stderr, "the comparitive= [%s]\n",  charBuf+n_charBuf );*/
			}
			lp = tp+1;
			/* get superlative */
			if ((tp = strchr(lp, PIPE )) == (char *)NULL)
			    break;

			
			if ((tp-lp) > 0)
			{
			    size = (tp-lp)+1;
			    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
				    &a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
				return((LexIndexKey *)NULL);
			    (void) strncpy(charBuf+n_charBuf, lp, (size_t)(tp-lp));
			    *(charBuf+n_charBuf+(tp-lp)) = EOS;
			    if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
				return((LexIndexKey *)NULL);
		            /*fprintf(stderr, "the superlative  = [%s]\n", charBuf+n_charBuf); */
			}
			break;

		    case LEX_CAT_NOUN:

		      /* get Base - we are going pass over this*/
		      if ((np = strchr(lp, PIPE)) == (char *)NULL) 
		       break;

		      /* get plural form */
		      lp = np + 1;
			if ((tp = strchr(lp, PIPE)) == (char *)NULL)
			    break;

		      /*fprintf(stderr, "irreg noun  np = |%s|\n",  np );*/
		      /*fprintf(stderr, "irreg noun  tp = |%s|\n",  tp );*/
		      /*fprintf(stderr, "irreg noun  lp = |%s|\n",  lp );*/

			if ((tp-lp) > 0)
			{
			    size = (tp-lp)+1;
			    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf,
									  sizeof(char),
									  n_charBuf,
									  &a_charBuf,
									  size,
									  LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL) {
				return((LexIndexKey *)NULL);
			    }
			    (void) strncpy(charBuf+n_charBuf, lp, (size_t)(tp-lp));
			    *(charBuf+n_charBuf+(tp-lp)) = EOS;
			    if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL))) {
				return((LexIndexKey *)NULL);
			    }
			}
			break;

		    case LEX_CAT_AUX:
		    case LEX_CAT_VERB:

		      /* get Base - we are going pass over this*/
		      if ((np = strchr(lp, PIPE)) == (char *)NULL)
			break;
		      lp = np +1 ;

			/* get present */
			tp = lp ;
			if ((tp = strchr(lp, PIPE)) == (char *)NULL)
			    break;
			if ((tp-lp) > 0)
			{
			    size = (tp-lp)+1;
			    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
				    &a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
				return((LexIndexKey *)NULL);
			    (void) strncpy(charBuf+n_charBuf, lp, (size_t)(tp-lp));
			    *(charBuf+n_charBuf+(tp-lp)) = EOS;
			    if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
				return((LexIndexKey *)NULL);
			}
			lp = tp+1;

/*  get past */
			if ((tp = strchr(lp, PIPE)) == (char *)NULL)
			    break;
			if ((tp-lp) > 0)
			{
			    size = (tp-lp)+1;
			    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
				    &a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
				return((LexIndexKey *)NULL);
			    (void) strncpy(charBuf+n_charBuf, lp, (size_t)(tp-lp));
			    *(charBuf+n_charBuf+(tp-lp)) = EOS;
			    if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
				return((LexIndexKey *)NULL);
			}
			lp = tp+1;

/* get past participle */
			if ((tp = strchr(lp, PIPE)) == (char *)NULL)
			    break;
			if ((tp-lp) > 0)
			{
			    size = (tp-lp)+1;
			    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
				    &a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
				return((LexIndexKey *)NULL);
			    (void) strncpy(charBuf+n_charBuf, lp, (size_t)(tp-lp));
			    *(charBuf+n_charBuf+(tp-lp)) = EOS;
			    if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
				return((LexIndexKey *)NULL);
			}
			lp = tp+1;

/* present participle */
			if ((tp = strchr(lp, PIPE)) == (char *)NULL)
			    break;
			if ((tp-lp) > 0)
			{
			    size = (tp-lp)+1;
			    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
				    &a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
				return((LexIndexKey *)NULL);
			    (void) strncpy(charBuf+n_charBuf, lp, (size_t)(tp-lp));
			    *(charBuf+n_charBuf+(tp-lp)) = EOS;
			    if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
				return((LexIndexKey *)NULL);
			}
			break;

		    default:
			break;
		}
	    }
	    else if (lexCat == LEX_CAT_MODAL)
	    {
		size = strlen(lp)+1;
		if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
			&a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
		    return((LexIndexKey *)NULL);
		(void) strcpy(charBuf+n_charBuf, lp);
		if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
		    return((LexIndexKey *)NULL);
	    }
	}
	else if (strncmp(lp, "key=", 4) == 0)
	{
	    lp += 4;
	    if (lexCat > 0)
	    {
		size = strlen(lp)+1;
		if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
			&a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
		    return((LexIndexKey *)NULL);
		(void) strcpy(charBuf+n_charBuf, lp);
		if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
		    return((LexIndexKey *)NULL);
	    }
	}
	else if (strncmp(lp, "variant=", 8) == 0)
	{
	  char *tp;
	    lp += 8;
	    if (lexCat > 0)
	    {
		size = strlen(lp)+1;
		if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
			&a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
		    return((LexIndexKey *)NULL);

		 /* ---------------------------------
		    Pick up ONLY up to the semi colon	
		    --------------------------------- */
		 if ((tp = strchr(lp, ';')) == (char *)NULL)
		 {
		   break;
		 }
		 (void) strncpy(charBuf+n_charBuf, lp, (size_t)(tp-lp));
		*(charBuf+n_charBuf+(tp-lp)) = EOS;
		if (!lex_load(charBuf+n_charBuf, (int)(lexCat|LEX_BASEINFL)))
		    return((LexIndexKey *)NULL);
	    }
	}
	else if (strcmp(lp, "}") == 0)
	{
/* load base and spelling variants */
	    if (!lex_load(charBuf+baseOfs, (int)(catFlags|LEX_BASE)))
		return((LexIndexKey *)NULL);
	    for (i=0; i<n_svoBuf; i++)
	      {
		if (!lex_load(charBuf+(*(svoBuf+i)), (int)(catFlags|LEX_SV)))
		  return((LexIndexKey *)NULL);
	      }
	    break;
	}
    }

    if (!startFlag)
	return((LexIndexKey *)NULL);

    *num = n_keyBuf;
    for (i=0; i<n_keyBuf; i++)
	(keyBuf+i)->likKey = charBuf+((keyBuf+i)->likHide);



  DEXIT(DT1604);
  return(keyBuf);

} /*** End lex_index_keys */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_line
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_line(arg);
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
	TRACE DT1606
	FULL  DF1607
%HEADER END
==========================================================*/
char *get_line(
               char *record /* Input */
	       )

{
  static char  returnVal[MAXLINE];
  static char a_record[MAXLINE];
  static char  *ptr = NULL;
  int   i = 0; 
  int not_done = D_TRUE;
  
  DFNAME("get_line");
  DENTER(DT1606);
  
  if ( strcmp(a_record, record ) != 0 )
    {
      strcpy(a_record,record);
      ptr = a_record;
      strcpy(returnVal,"");
    }
      
  
  while (( *ptr != EOS ) && ( not_done == D_TRUE ))
    {
      returnVal[i]=*ptr;
      returnVal[i+1]=EOS;
      
      if ( *ptr == '\n' )
	{
	  not_done = D_FALSE;
	}
      
      ptr++;
      i++;
      
    }

  /* bottom: */

  DEXIT(DT1606);
  return ( returnVal );

} /*** End get_line */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_cat
%PURPOSE
	returns the syntactic category 
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
	TRACE DT1608
	FULL  DF1609
%HEADER END
==========================================================*/
static int get_cat(
		   char *cat,
		   int *lexCat,
		   int *inflCat,
		   int *inflInfl 
		   )

{
  int return_code = D_S_SUCCESS;
    static struct {
	char *strCat;
	int lexCat;
	int inflCat;
	int inflInfl;
    } x[] = {
	{"adj",	    LEX_CAT_ADJ,    LM_CAT_ADJ,	    (LM_INFL_COMPARATIVE|LM_INFL_SUPERLATIVE)},
	{"adv",	    LEX_CAT_ADV,    LM_CAT_ADJ,	    (LM_INFL_COMPARATIVE|LM_INFL_SUPERLATIVE)},
	{"aux",	    LEX_CAT_AUX,    LM_CAT_VERB,    (LM_INFL_PRESENT|LM_INFL_ING|LM_INFL_PAST)},
	{"compl",   LEX_CAT_COMPL,  0,		    0},
	{"conj",    LEX_CAT_CONJ,   0,		    0},
	{"det",	    LEX_CAT_DET,    0,		    0},
	{"modal",   LEX_CAT_MODAL,  0,		    0},
	{"noun",    LEX_CAT_NOUN,   LM_CAT_NOUN,    LM_INFL_PLURAL},
	{"prep",    LEX_CAT_PREP,   0,		    0},
	{"pron",    LEX_CAT_PRON,   0,		    0},
	{"verb",    LEX_CAT_VERB,   LM_CAT_VERB,    (LM_INFL_PRESENT|LM_INFL_ING|LM_INFL_PAST)}
    };
    static int xSize = 11;
    int low = 0;
    int high = xSize - 1;
    int mid;
    int cmp;



  DFNAME("get_cat");
  DENTER(DT1608);


/* initialize */

    *lexCat = *inflCat = *inflInfl = 0;

/* do binary search */
    while (low <= high)
    {
	mid = (low + high)/2;
	cmp = strcmp(cat, x[mid].strCat);
	if (cmp < 0)
	    high = mid - 1;
	else if (cmp > 0)
	    low = mid + 1;
	else
	{
	    *lexCat = x[mid].lexCat;
	    *inflCat = x[mid].inflCat;
	    *inflInfl = x[mid].inflInfl;
	    return_code = 1;
	    goto bottom;
	}
    }

    return_code = 0;

 bottom:

  DEXIT(DT1608);
  return ( return_code );

} /*** End get_cat */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	lex_load
%PURPOSE
	loads the table with a term 
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lex_load(arg);
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
	TRACE DT1610
	FULL  DF1611
%HEADER END
==========================================================*/
static int lex_load(
		    char *term,
		    int flags 
		    )
     
{
  int return_code = D_S_SUCCESS;
  LexIndexKey *key;
  int i;
  int flag = 0;
  int size;
  

  DFNAME("lex_load");
  DENTER(DT1610);

/* search table for term */
    for (i=0; i<n_keyBuf; i++)
    {
	key = keyBuf+i;
	if (strcmp(term, charBuf+(key->likHide)) == 0)
	{
	    key->likFlags |= flags;
	    flag = 1;
	}
    }

    if (!flag)
    {
	size = strlen(term)+1;
	if ((keyBuf = (LexIndexKey *) lex_reallocate_buffer((void *)keyBuf, sizeof(LexIndexKey), n_keyBuf,
		&a_keyBuf, (int)1, LEX_DEFAULT_ALLOCATION)) == (LexIndexKey *)NULL)
	  {
	    return_code = 0;
	    goto bottom;
	  }
	if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
		&a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
	  {
	    return_code = 0;
	    goto bottom;
	  }
	(void) strcpy(charBuf+n_charBuf, term);
	key = keyBuf+n_keyBuf;
	key->likKey = (char *)NULL;
	key->likHide = n_charBuf;
	key->likFlags = flags;
	n_charBuf += size;
	n_keyBuf++;
    }

/* load lower cased version, if necessary */

    size = strlen(term)+1 ;
    if ((charBuf = (char *) lex_reallocate_buffer((void *)charBuf, sizeof(char), n_charBuf,
	    &a_charBuf, size, LEX_DEFAULT_ALLOCATION*4)) == (char *)NULL)
	  {
	    return_code = 0;
	    goto bottom;
	  }

    (void) strcpy(charBuf+n_charBuf, term);
    llip((unsigned char *)(charBuf+n_charBuf));
    if (strcmp(term, charBuf+n_charBuf) != 0)
    {
	int lflags = flags & ~(LEX_BASE|LEX_SV|LEX_BASEINFL|LEX_SVINFL);

	if (IS_LEX_BASE(flags))
	    MAKE_LEX_BASELOWER( lflags );
	else if (IS_LEX_SV(flags))
	    MAKE_LEX_SVLOWER( lflags);
	else if (IS_LEX_BASEINFL(flags))
	    MAKE_LEX_BASEINFLLOWER( lflags);
	else if (IS_LEX_SVINFL(flags))
	    MAKE_LEX_SVINFLLOWER( lflags);
    

/* search table for term */
	for (flag=0, i=0; i<n_keyBuf; i++)
	{
	    key = keyBuf+i;
	    if (strcmp(charBuf+n_charBuf, charBuf+(key->likHide)) == 0)
	    {
		key->likFlags |= lflags;
		flag = 1;
	    }
	}

	if (!flag)
	{
	    if ((keyBuf = (LexIndexKey *) lex_reallocate_buffer((void *)keyBuf, sizeof(LexIndexKey), n_keyBuf,
		    &a_keyBuf, (int)1, LEX_DEFAULT_ALLOCATION)) == (LexIndexKey *)NULL)
	  {
	    return_code = 0;
	    goto bottom;
	  }
	    key = keyBuf+n_keyBuf;
	    key->likKey = (char *)NULL;
	    key->likHide = n_charBuf;
	    key->likFlags = lflags;
	    n_charBuf += size;
	    n_keyBuf++;
	}
    }
    return_code = 1;

 bottom:

  DEXIT(DT1610);
  return ( return_code );

} /*** End lex_load */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	gen_and_load
%PURPOSE
	generates the inflections and loads table.
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = gen_and_load(arg);
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
	TRACE DT1612
	FULL  DF1613
%HEADER END
==========================================================*/
static int gen_and_load(
			int lexCat,
			int inflCat,
			int inflRule,
			int inflInfl 
			)

{
  int return_code = D_S_SUCCESS;
  LmStruct *lms;
  int n;
  int i;
  int j;


  DFNAME("gen_and_load");
  DENTER(DT1612);


/* generate & insert variants for base */
  if ((lms = (LmStruct *)lm_variants(charBuf+baseOfs,
				     (lm_t)inflCat,
				     (lm_t)inflRule,
				     (lm_t)inflInfl, &n)) == (LmStruct *)NULL)

      {
	return_code = 0;
	goto bottom;
      }
    for (i=0; i<n; i++)
    {
	if (!lex_load((lms+i)->lmVar, (int)(lexCat|LEX_BASEINFL)))
	  {
	    return_code = 0;
	    goto bottom;
	  }
    }

/* generate and insert variants of spelling variants */
    for (j=0; j<n_svoBuf; j++)
    {
	if ((lms = (LmStruct *)lm_variants(charBuf+(*(svoBuf+j)),
					   (lm_t)inflCat,
					   (lm_t)inflRule,
					   (lm_t)inflInfl, &n)) == (LmStruct *)NULL)
	  {
	    return_code = 0;
	    goto bottom;
	  }
	for (i=0; i<n; i++)
	{
	    if (!lex_load((lms+i)->lmVar, (int)(lexCat|LEX_SVINFL)))
	      {
		return_code = 0;
		goto bottom;
	      }
	}
    }
    return_code = 1;

bottom:

  DEXIT(DT1612);
  return ( return_code );

} /*** End gen_and_load */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	lex_reallocate_buffer
%PURPOSE
	 reallocates a generic buffer 
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lex_reallocate_buffer(arg);
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
	TRACE DT1614
	FULL  DF1615
%HEADER END
==========================================================*/
static void * lex_reallocate_buffer(
				    void *buf,	/* pointer to buffer */
				    int unit,	/* size of one of what buf points to in bytes */
				    int size,	/* current size of buffer in units */
				    int *alloc,	/* current allocation of buffer in units */
				    int n,	/* units needed */
				    int incr 	/* increment size in units */
				    )
{
  DFNAME("lex_reallocat_buffer");
  DENTER(DT1614);

  if ((size + n) > *alloc)
    {
      while ((size+n) > *alloc)
	*alloc += incr + 1;
      if (size == 0)
	{
	    if ((buf = (void *) malloc((size_t)(unit*(*alloc)))) == (void *)NULL)
	      memset(buf, 0, unit*(*alloc));
	      goto bottom;
	}
	else
	{
	    if ((buf = (void *) realloc(buf, (size_t)(unit*(*alloc)))) == (void *)NULL)
	      goto bottom;
	}
    }

 bottom:

  DEXIT(DT1614);
  return ( buf );

} /*** End lex_reallocat_buffer */
