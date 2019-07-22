
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
	cc_linfl.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	13Apr00 divita -- Initial Version

%%
==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <debug.h>
#include "lm.h"
#include "lexicon.h"
#include "sicstus/sicstus.h"
#include "qp_lexicon_glue.h"

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#define DEFAULT_ALLOC_SIZE 32

/*end of constants ---------*/

/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/
extern LmStruct *lm_variants(
			     char *term,
			     lm_t cats,
			     lm_t rules,
			     lm_t infls,
			     int *numV
			     );
extern int DPR(int flag, char *msg);


/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/

/* With SICStus Prolog's splfr, Per Mildner recommended that we
 * remove all function prototypes for foreign functions from our `C' code.

 * long int c_get_varlist(
 * 		       char *lexiconFile,
 * 		       long int ofs,
 *		       SP_term_ref varList
 * 		       );
 */

static int gen_and_load(
			char *catStr,        /* I/O */ 
			int inflCat,         /* I/O */
			int inflRule,        /* I/O */
			int inflInfl,        /* I/O */
			SP_term_ref varList  /* I/O */
			);
     
static int get_cat(
		   char *cat,      /* I/O */
		   int *lexCat,    /* I/O */
		   int *inflCat,   /* I/O */
		   int *inflInfl   /* I/O */
		   );

static char * get_infl( lm_t infl  /* I */ );
     
static SP_term_ref make_term(
			     char *term,
			     char *catStr,
			     char *inflStr 
			     );

long int c_close_lexrec_file(void);

/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/

static char *base = (char *)NULL;    /* malloc'ed area for base              */
static char **svs = (char **)NULL;   /* spelling variants                    */
static int n_svs = 0;		     /* number of spelling variants          */
static int n_alloc_svs = 0;	     /* number of spelling variants alloc'ed */

static int lexCat = 0;		     /* current lexical category             */
static char catStr[DEFAULT_ALLOC_SIZE];
static int inflCat = 0;		     /* current infl category                */
static int inflInfl = 0;	     /* current infl inflection              */
static FILE *c_get_varlist_fp = NULL;

/*end of global variables --*/

/*----------------------------
%DEBUG FLAGS
----------------------------*/

#define DT2790  2790          /* DT for c_get_varlist() */
#define DF2791  2791          /* DF for c_get_varlist() */
#define DT2792  2792          /* DT for gen_and_load() */
#define DF2793  2793          /* DF for gen_and_load() */
#define DT2794  2794          /* DT for get_cat() */
#define DF2795  2795          /* DF for get_cat() */
#define DT2796  2796          /* DT for get_infl() */
#define DF2797  2797          /* DF for get_infl() */
#define DT2798  2798          /* DT for make_term() */
#define DF2799  2799          /* DF for make_term() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
	c_get_varlist
%SCOPE
	public
%PURPOSE
       In varList is returned a list of terms in the format:
	 word(cat, feature)
	 e.g., [bear(noun, base), bear(verb, base), bore(verb, past),...], etc
	
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	ret_val = c_get_varlist();
z%RETURNS
	?
%METHOD
	?
%FLAGS  
	TRACE DT2790
	FULL  DF2791
%HEADER END
==========================================================*/
long int c_get_varlist(
		       char const   *lexiconFile, /* Input */
		       long int     ofs,          /* Input */
		       SP_term_ref  varList       /* O list of returned variants */
		       )
{
   int return_code = D_S_SUCCESS;

   char 	line[256];
   char 	*lp 	 = NULL;
   char 	*np;
   int 		i 	 = 0;
   char		*inflStr = NULL;
   SP_term_ref  pTerm 	 = SP_new_term_ref();
   char 	*x 	 = NULL;
   SP_atom	pNil	 = SP_atom_from_string("[]");

   DFNAME("c_get_varlist");
   DENTER(DT2790);

   /* printf("Coming in with %s|%d\n",lexiconFile, ofs ); */


/* -------------------
    set up the lexicon 
   ------------------- */
  if (c_get_varlist_fp == (FILE *)NULL) {
     if ((c_get_varlist_fp = fopen(lexiconFile, "r")) == (FILE *)NULL) {
       fprintf(stderr, "FAILURE #1: fpopen\n");
       return_code = D_E_ERROR;
       goto bottom;
     }
  }

  if (fseek(c_get_varlist_fp, (long) ofs, 0) == (-1)) {
    fprintf(stderr, "FAILURE #2: fseek\n");
    return_code = D_E_ERROR;
    goto bottom;
  }
   
/* --------------
   Start with nil
   -------------- */
   SP_put_atom(varList, pNil);
   
/* ---------------------------------
   Free previous allocations, if any 
   --------------------------------- */
   if (base != (char *)NULL)
   {
      (void) free((char *)base);
      base = (char *)NULL;
   }
   
   if (svs != (char **)NULL)
   {
      for (i=0; i<n_svs; i++)
      {
	 if (*(svs+i) != (char *)NULL)
	 {
	    (void) free((char *)(*(svs+i)));
	    *(svs+i) = (char *)NULL;
	 }
      }
      n_svs = 0;
      n_alloc_svs = 0;
      (void) free((char *)(svs));
      svs = (char **)NULL;
   }
   
/* -----------
   Read record
   ----------- */
   while (fgets(line, sizeof(line), c_get_varlist_fp) != (char *)NULL)
   {
      line[strlen(line)-1] = EOS;
      /* printf(">>>0>> %s <<<<<<\n", line); */

      for (lp=line; isspace((int)*lp); lp++)
      ;
      
      if (strncmp(lp, "{base=", 6) == 0)
      {
	 lp += 6;

	 if ((base = malloc((size_t)(strlen(lp)+1))) == (char *)NULL) {
	   fprintf(stderr, "FAILURE #3: malloc\n");
	   return_code = D_E_ERROR;
	   goto bottom;
	 }

	 (void) strcpy(base, lp);
	 /* printf(">>>>>> %s <<<<<<\n", base ); */
      }
      else if (strncmp(lp, "spelling_variant=", 17) == 0)
      {
	 lp += 17;
	 if (n_svs == 0)
	 {
	    n_alloc_svs = DEFAULT_ALLOC_SIZE;

	    if ((svs=(char **)malloc((size_t)(n_alloc_svs*sizeof(char *))))==
		(char **)NULL) {
	      fprintf(stderr, "FAILURE #4: malloc\n");
	      return_code = D_E_ERROR;
	      goto bottom; 
	    }

	 }
	 else if (n_svs == n_alloc_svs)
	 {
	    n_alloc_svs += DEFAULT_ALLOC_SIZE;
	    if ((svs = (char **) realloc((char *) svs, (unsigned) (n_alloc_svs*sizeof(char *)))) ==
		(char **)NULL) {

	      fprintf(stderr, "FAILURE #5: realloc\n");
	      return_code = D_E_ERROR;
	      goto bottom;
	    }

	 }
	 if ((*(svs+n_svs) = (char *)malloc((size_t)(strlen(lp)+1))) == (char *)NULL) {
	   fprintf(stderr, "FAILURE #6: malloc\n");
	   return_code = D_E_ERROR;
	   goto bottom; 
	 }
	 
	 (void) strcpy(*(svs+n_svs), lp);
	 n_svs++;
      }
      else if (strncmp(lp, "entry=", 6) == 0)
      {
	 lexCat = inflCat = 0;
      }
      else if (strncmp(lp, "cat=", 4) == 0)
      {
	 lp += 4;
	 strncpy(catStr, lp, sizeof(catStr));
	 if (get_cat(catStr, &lexCat, &inflCat, &inflInfl) == LEX_ERROR) {
	   fprintf(stderr, "FAILURE #7: get_cat\n");
	   return_code = D_E_ERROR;
	   goto bottom; 
	 }
	 
	 pTerm = make_term(base, catStr, "base");
	 SP_cons_list(varList, pTerm, varList);
	 for (i=0; i<n_svs; i++)
	 {
	    pTerm = make_term(*(svs+i), catStr, "spvar");
	    SP_cons_list(varList, pTerm, varList);
	 }
      }
      else if (strncmp(lp, "variants=", 9) == 0)
      {
	 lp += 9;
	 if (strcmp(lp, "reg") == 0 || strcmp(lp, "group(reg)") == 0)
	 {
	    if (inflCat > 0)
	    {
	      /* printf(">>>3>> %s <<<<<<\n", base ); */
	       if (gen_and_load(catStr, 
				inflCat, 
				(int)LM_RULE_REG, 
				inflInfl, varList) == LEX_ERROR)
	       {
	          fprintf(stderr, 
			  "ERROR: Cannot generate 'reg' variants for: %s\n",
			  base);
	       }
	    }
	 }
	 else if (strcmp(lp, "regd") == 0)
	 {
	    if (inflCat > 0)
	    {
	       if (gen_and_load(catStr, 
				inflCat, 
				(int)LM_RULE_REGD, 
				inflInfl, 
				varList) == LEX_ERROR)
	       {
	         fprintf(stderr, 
			 "ERROR: Cannot generate 'regd' variants for: %s\n",
			 base);
	      }
	    }
	 }
	 else if (strcmp(lp, "glreg") == 0 || strcmp(lp, "group(glreg)") == 0)
	 {
	    if (inflCat > 0)
	    {
	       if (gen_and_load(catStr, 
				inflCat, 
				(int)LM_RULE_GLREG, 
				inflInfl, 
				varList) == LEX_ERROR)
	       {
		  fprintf(stderr, 
			  "ERROR: Cannot generate 'glreg' variants for: %s\n",
			  base);
	       }
	    }
	 }
	 else if (strcmp(lp, "metareg") == 0 || 
		  strcmp(lp, "group(metareg)") == 0)
	 {
	    if (inflCat > 0)
	    {
	       char *mreg;
	    /* ---------------------------------   
	       generate the "s" variant for base
	       --------------------------------- */
	       if ((mreg = (char *) malloc((size_t)(strlen(base)+2))) 
		   == (char *)NULL) {
		 fprintf(stderr, "FAILURE #8: malloc\n");
		 return_code = D_E_ERROR;
		 goto bottom; 
	       }
	       
	       strcpy(mreg, base);
	       strcat(mreg, "s");
	       inflStr = get_infl(LM_INFL_PLURAL);
	       pTerm = make_term(mreg, catStr, inflStr);
	       SP_cons_list(varList, pTerm, varList);
	       (void) free(mreg);
	    /* ----------------------------------  
	       Generate the "'s" variant for base
	       ---------------------------------- */
	       if ((mreg = (char *) malloc((size_t)(strlen(base)+3))) 
		   == (char *)NULL) {
		 fprintf(stderr, "FAILURE #9: malloc\n");
		 return_code = D_E_ERROR;
		 goto bottom; 
	       }
	       
	       strcpy(mreg, base);
	       strcat(mreg, "'s");
	       inflStr = get_infl(LM_INFL_PLURAL);
	       pTerm = make_term(mreg, catStr, inflStr);
	       SP_cons_list(varList, pTerm, varList);
	       (void) free(mreg);
	       
	       for (i=0; i<n_svs; i++)
	       {
		  
	       /* ---------------------------------------------  
		  Generate the "s" variant for spelling variant
	          --------------------------------------------- */
		 if ((mreg = (char *) malloc((size_t)(strlen(*(svs+i))+2))) == (char *)NULL) {
		   fprintf(stderr, "FAILURE #10: malloc\n");
		   return_code = D_E_ERROR;
		   goto bottom; 
		 }
		 
		  strcpy(mreg, *(svs+i));
		  strcat(mreg, "s");
		  inflStr = get_infl(LM_INFL_PLURAL);
		  pTerm = make_term(mreg, catStr, inflStr);
		  SP_cons_list(varList, pTerm, varList);
		  (void) free(mreg);
		  
	       /* ---------------------------------------------  
		  Generate the "'s" variant for spelling variant
	          --------------------------------------------- */
		  if ((mreg = (char *) malloc((size_t)(strlen(*(svs+i))+3))) == (char *)NULL) {
		    fprintf(stderr, "FAILURE #11: malloc\n");
		    return_code = D_E_ERROR;
		    goto bottom;
		  }
		  
		  strcpy(mreg, *(svs+i));
		  strcat(mreg, "'s");
		  inflStr = get_infl(LM_INFL_PLURAL);
		  pTerm = make_term(mreg, catStr, inflStr);
		  SP_cons_list(varList, pTerm, varList);
		  (void) free(mreg);
	       }
	    }
	 }
	 else if (strncmp(lp, "group(irreg|", 12) == 0)
	 {
	    char *tp;
	    char *irreg;
	    
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
	      if ((irreg = malloc((size_t)((tp-lp)+1))) == (char *)NULL) {
	        fprintf(stderr, "FAILURE #12: malloc\n");
		return_code = D_E_ERROR;
		goto bottom; 
	      }
	      
	       strncpy(irreg, lp, (size_t)(tp-lp));
	       *(irreg+(tp-lp)) = EOS;
	       inflStr = get_infl(LM_INFL_PLURAL);
	       pTerm = make_term(irreg, catStr, inflStr);
	       SP_cons_list(varList, pTerm, varList);
	       (void) free(irreg);
	    }
	 }
	 else if (strncmp(lp, "irreg|", 6) == 0)
	 {
	    char *tp;
	    char *irreg;
	    
	    if (inflCat == 0)
	    continue;
	    
	    lp += 6;
	    switch (lexCat)
	    {
	     case LEX_CAT_ADJ:
	     case LEX_CAT_ADV:

	    /* ---------------  
	       Get comparative
	       --------------- */
	       /* get Base - we are going pass over this*/
	       if ((np = strchr(lp, PIPE)) == (char *)NULL)
		 break;
	       lp = np + 1;

	       if ((tp = strchr(lp, PIPE)) == (char *)NULL)
	         break;

	       if ((tp-lp) > 0)
	       {
		 if ((irreg = malloc((size_t)((tp-lp)+1))) == (char *)NULL) {
	           fprintf(stderr, "FAILURE #13: malloc\n");		   
		   return_code = D_E_ERROR;
		   goto bottom; 
		 }
		 
		  strncpy(irreg, lp, (size_t)(tp-lp));
		  *(irreg+(tp-lp)) = EOS;
		  inflStr = get_infl(LM_INFL_COMPARATIVE);
		  pTerm = make_term(irreg, catStr, inflStr);
		  SP_cons_list(varList, pTerm, varList);
		  (void) free(irreg);
	       }
	       lp = tp+1;
	    /* ---------------  
	       Get superlative 
	       --------------- */
	       if ((tp = strchr(lp, PIPE)) == (char *)NULL)
	         break;

	       if ((tp-lp) > 0)
	       {
		 if ((irreg = malloc((size_t)((tp-lp)+1))) == (char *)NULL) {
	           fprintf(stderr, "FAILURE #14: malloc\n");
		   return_code = D_E_ERROR;
		   goto bottom; 
		 }
		 
		  strncpy(irreg, lp, (size_t)(tp-lp));
		  *(irreg+(tp-lp)) = EOS;
		  inflStr = get_infl(LM_INFL_SUPERLATIVE);
		  pTerm = make_term(irreg, catStr, inflStr);
		  SP_cons_list(varList, pTerm, varList);
		  (void) free(irreg);
	       }
	       break;
	       
	     case LEX_CAT_NOUN:
	       
	    /* ----------
	       Get plural
	       ---------- */
	       /* get Base - we are going pass over this*/
	       if ((np = strchr(lp, PIPE)) == (char *)NULL) 
	         break;
					
	       /* get plural form*/
	       lp = np + 1;

	       if ((tp = strchr(lp, PIPE)) == (char *)NULL)
	       break;

	       if ((tp-lp) > 0)
	       {
		 if ((irreg = malloc((size_t)((tp-lp)+1))) == (char *)NULL) {
	           fprintf(stderr, "FAILURE #15: malloc\n");
		   return_code = D_E_ERROR;
		   goto bottom; 
		 }

		  strncpy(irreg, lp, (size_t)(tp-lp));
		  *(irreg+(tp-lp)) = EOS;
		  inflStr = get_infl(LM_INFL_PLURAL);
		  pTerm = make_term(irreg, catStr, inflStr);
		  SP_cons_list(varList, pTerm, varList);
		  (void) free(irreg);
	       }
	       break;
	       
	     case LEX_CAT_AUX:
	     case LEX_CAT_VERB:
	       
	    /* -----------
	       variants=irreg|show|shows|showed|shown|showing|
	       Skip over infinitive
	       ----------- */

	       if ((tp = strchr(lp, PIPE)) == (char *)NULL)
	       break;
	       lp = tp+1;

	    /* -----------
	       Get present 
	       ----------- */
	       if ((tp = strchr(lp, PIPE)) == (char *)NULL)
	       break;
	       if ((tp-lp) > 0)
	       {
		 if ((irreg = malloc((size_t)((tp-lp)+1))) == (char *)NULL) {
	           fprintf(stderr, "FAILURE #16: malloc\n");
		   return_code = D_E_ERROR;
		   goto bottom; 
		 }

		  strncpy(irreg, lp, (size_t)(tp-lp));
		  *(irreg+(tp-lp)) = EOS;
		  inflStr = get_infl(LM_INFL_PRESENT);
		  pTerm = make_term(irreg, catStr, inflStr);
		  SP_cons_list(varList, pTerm, varList);
		  (void) free(irreg);
	       }
	       lp = tp+1;
	       
	    /* --------
	       Get past
	       -------- */
	       if ((tp = strchr(lp, PIPE)) == (char *)NULL)
	       break;
	       if ((tp-lp) > 0)
	       {
		 if ((irreg = malloc((size_t)((tp-lp)+1))) == (char *)NULL) {
	           fprintf(stderr, "FAILURE #17: malloc\n");
		   return_code = D_E_ERROR;
		   goto bottom;
		 }

		  strncpy(irreg, lp, (size_t)(tp-lp));
		  *(irreg+(tp-lp)) = EOS;
		  inflStr = get_infl(LM_INFL_PAST);
		  pTerm = make_term(irreg, catStr, inflStr);
		  SP_cons_list(varList, pTerm, varList);
		  (void) free(irreg);
			}
	       lp = tp+1;
	       
            /* -------------------
	       Get past participle
               ------------------- */
	       if ((tp = strchr(lp, PIPE)) == (char *)NULL)
	       break;
	       if ((tp-lp) > 0)
	       {
		 if ((irreg = malloc((size_t)((tp-lp)+1))) == (char *)NULL) {
	           fprintf(stderr, "FAILURE #18: malloc\n");
		   return_code = D_E_ERROR;
		   goto bottom; 
		 }
		 
		  strncpy(irreg, lp, (size_t)(tp-lp));
		  *(irreg+(tp-lp)) = EOS;
		  inflStr = get_infl(LM_INFL_PASTPART);        /* GD 08/14/09 */
                  pTerm = make_term(irreg, catStr, inflStr ); 
		  SP_cons_list(varList, pTerm, varList);
		  (void) free(irreg);
	       }
	       lp = tp+1;
	       
            /* ------------------
	       present participle 
               ------------------ */
	       if ((tp = strchr(lp, PIPE)) == (char *)NULL)
	       break;
	       if ((tp-lp) > 0)
	       {
		 if ((irreg = malloc((size_t)((tp-lp)+1))) == (char *)NULL) {
	           fprintf(stderr, "FAILURE #19: malloc\n");
		   return_code = D_E_ERROR;
		   goto bottom;
		 }

		  strncpy(irreg, lp, (size_t)(tp-lp));
		  *(irreg+(tp-lp)) = EOS;
		  inflStr = get_infl(LM_INFL_ING);
		  pTerm = make_term(irreg, catStr, inflStr);
		  SP_cons_list(varList, pTerm, varList);
		  (void) free(irreg);
	       }
	       break;
	       
	     default:
	       break;
	    }
	 }
	 else if (lexCat == LEX_CAT_MODAL)
	 {
	    char *infl;
	    
	    if ((infl = malloc((size_t)(strlen(lp)+1))) == (char *)NULL) {
              fprintf(stderr, "FAILURE #20: malloc\n");
	      return_code = D_E_ERROR;
	      goto bottom;
	    }

	    strcpy(infl, lp);
	    inflStr = get_infl(LM_INFL_PAST);
	    pTerm = make_term(infl, catStr, inflStr);
	    SP_cons_list(varList, pTerm, varList);
	    (void) free(infl);
	 }
      }
      else if (strncmp(lp, "key=", 4) == 0)
      {
	 char *key;
	 
	 lp += 4;
	 if (lexCat > 0)
	 {
	   if ((key=malloc((size_t)(strlen(lp) + 1))) == (char *)NULL) {
	     fprintf(stderr, "FAILURE #21: malloc\n");
	     return_code = D_E_ERROR;
	     goto bottom; 
	   }
	    (void) strcpy(key, lp);
	    pTerm = make_term(key, catStr, "key");
	    SP_cons_list(varList, pTerm, varList);
	    (void) free(key);
	 }
      }
      else if (strncmp(lp, "variant=", 8) == 0)
      {
	 char *variant;
	 
	 lp += 8;
	 if (lexCat > 0)
	 {
	   if ((variant=malloc((size_t)(strlen(lp) + 1))) == (char *)NULL) {
	     fprintf(stderr, "FAILURE #22: malloc\n");
	     return_code = D_E_ERROR;
	     goto bottom; 
	   }
	   (void) strcpy(variant, lp);

	    /* ---------------------------------
	       The variant part is only to the ;
	       --------------------------------- */
	    x = strchr( variant,';' ); 
	    if ( x != NULL ) *x = EOS;


	    pTerm = make_term(variant, catStr, "variant");
	    SP_cons_list(varList, pTerm, varList);
	    (void) free(variant);
	 }
      }
      else if (strcmp(lp, "}") == 0)
      {
	 break;
      }
   }

 bottom:
   

  DEXIT(DT2790);
  return ( return_code );

} /*** End c_get_varlist */


long int c_close_lexrec_file(void) {
  fclose(c_get_varlist_fp);
  return(0);
}


/**/
/*==========================================================
%FUNCTION NAME
	gen_and_load
%SCOPE
	private | static
%PURPOSE
	Generates the inflections and loads table.
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	ret_val =  gen_and_load(catStr, 
				inflCat, 
				(int)LM_RULE_REGD, 
				inflInfl, 
				varList);
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
	TRACE DT2792
	FULL  DF2793
%HEADER END
==========================================================*/
static int gen_and_load(
			char *catStr,                /* I/O */ 
			int inflCat,                 /* I/O */
			int inflRule,                /* I/O */
			int inflInfl,                /* I/O */
			SP_term_ref varList          /* I/O */
			)
     

{
  int return_code = LEX_OK; 
  
  char 		*inflStr   = NULL;
  SP_term_ref   pTerm      = SP_new_term_ref();
  LmStruct 	*lms	   = NULL;
  int 		n	   = 0;
  int 		i	   = 0;
  int 		j	   = 0;
  
  
  DFNAME("gen_and_load");
  DENTER(DT2792);
  

   if ((lms = lm_variants(base, 
			  (lm_t)inflCat, 
			  (lm_t)inflRule, 
			  (lm_t)inflInfl, 
			  &n)) == (LmStruct *)NULL) {
     return_code = LEX_ERROR;
     goto bottom;
   }

   for (i=0; i<n; i++)
   {
      inflStr = get_infl((lm_t)(lms+i)->lmInfl);
      pTerm = make_term((lms+i)->lmVar, catStr, inflStr);
      SP_cons_list(varList, pTerm, varList);
      if ((lms+i)->lmInfl == LM_INFL_PAST)
      {
	 pTerm = make_term((lms+i)->lmVar, catStr, "pastpart");
	 SP_cons_list(varList, pTerm, varList);
      }
   }
/* ------------------------------------------------- 
   generate and insert variants of spelling variants
   ------------------------------------------------- */
   for (j=0; j<n_svs; j++)
   {
      if ((lms = lm_variants(*(svs+j), 
			     (lm_t)inflCat, 
			     (lm_t)inflRule, 
			     (lm_t)inflInfl, 
			     &n)) == (LmStruct *)NULL) {
        return_code = LEX_ERROR;
	goto bottom;
      }

      for (i=0; i<n; i++)
      {
	 inflStr = get_infl((lm_t)(lms+i)->lmInfl);
	 pTerm = make_term((lms+i)->lmVar, catStr, inflStr);
	 SP_cons_list(varList, pTerm, varList);
	 if ((lms+i)->lmInfl == LM_INFL_PAST)
	 {
	    pTerm = make_term((lms+i)->lmVar, catStr, "pastpart");
	    SP_cons_list(varList, pTerm, varList);
	 }
      }
   }
 bottom:
   
  DEXIT(DT2792);
  return ( return_code );

} /*** End gen_and_load */
/**/
/*==========================================================
%FUNCTION NAME
	get_cat
%SCOPE
	private | static
%PURPOSE
	Returns the syntactic category
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	 ret_val = get_cat(catStr, &lexCat, &inflCat, &inflInfl);
%RETURNS
	?
%METHOD
	?
%FILES
	?
%TABLES
	?
%FLAGS  
	TRACE DT2794
	FULL  DF2795
%HEADER END
==========================================================*/
static int get_cat(
		   char *cat,              /* I/O */
		   int *lexCat,            /* I/O */
		   int *inflCat,           /* I/O */
		   int *inflInfl           /* I/O */
		   )


{
  int return_code = D_S_SUCCESS;
/* ----------------------------------------------
   Define a global structure for binary searching 
   ---------------------------------------------- */
   static struct {
      char *strCat;
      int lexCat;
      int inflCat;
      int inflInfl;
   } x[] = {
      {"adj",	    LEX_CAT_ADJ,    LM_CAT_ADJ,	    (LM_INFL_COMPARATIVE|LM_INFL_SUPERLATIVE)},
      {"adv",	    LEX_CAT_ADV,    LM_CAT_ADJ,	    (LM_INFL_COMPARATIVE|LM_INFL_SUPERLATIVE)},
      {"aux",	    LEX_CAT_AUX,    LM_CAT_VERB,    (LM_INFL_PRESENT|LM_INFL_ING|LM_INFL_PAST)},
      {"compl",     LEX_CAT_COMPL,  0,		    0},
      {"conj",      LEX_CAT_CONJ,   0,		    0},
      {"det",	    LEX_CAT_DET,    0,		    0},
      {"modal",     LEX_CAT_MODAL,  0,		    0},
      {"noun",      LEX_CAT_NOUN,   LM_CAT_NOUN,    LM_INFL_PLURAL},
      {"prep",      LEX_CAT_PREP,   0,		    0},
      {"pron",      LEX_CAT_PRON,   0,		    0},
      {"verb",      LEX_CAT_VERB,   LM_CAT_VERB,    (LM_INFL_PRESENT|LM_INFL_ING|LM_INFL_PAST)}
   };
   static int xSize = 11;

   int low = 0;
   int high = xSize - 1;
   int mid;
   int cmp;
   

  DFNAME("get_cat");
  DENTER(DT2794);

  DPR(DF2795,"");


/* -----------
   Initialize 
   ----------- */
   *lexCat = *inflCat = *inflInfl = 0;

/* ---------------- 
   Do binary search
   ---------------- */
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
	    return_code = LEX_OK;
	    goto bottom;
	}
    }
    
    return_code = LEX_ERROR;

   
 bottom:


  DEXIT(DT2794);
  return ( return_code );

} /*** End get_cat */
/**/
/*==========================================================
%FUNCTION NAME
	*get_infl
%SCOPE
	private | static
%PURPOSE
	Returns an inflection string 
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
        inflStr = get_infl(LM_INFL_PLURAL);
%RETURNS
	?
%METHOD
	?
%FILES
	?
%TABLES

%FLAGS  
	TRACE DT2796
	FULL  DF2797
%HEADER END
==========================================================*/
static char * get_infl(
		       lm_t infl          /* I */
		       )
     
{

   static char *s[] = {
      "comparative",
      "superlative",
      "plural",
      "present",
      "ing",
      "past",
      "pastpart"    /* was missing from the orig code GD 8/14/09 */ 
   };
   
   char *return_code = NULL;


  DFNAME("get_infl");
  DENTER(DT2796);

   switch (infl)
   {
    case LM_INFL_COMPARATIVE:
         return_code = s[0];
	 break;

    case LM_INFL_SUPERLATIVE:
         return_code = s[1];
	 break;

    case LM_INFL_PLURAL:
         return_code = s[2];
	 break;

    case LM_INFL_PRESENT:
         return_code = s[3];
	 break;

    case LM_INFL_ING:
	 return_code = s[4];  /* changed from 5 to 4 GD 08/14/09 */
	 break;

    case LM_INFL_PAST:
         return_code = s[5];
	 break;

    case LM_INFL_PASTPART:
	 return_code = s[6];   /*  changed from 3 to this new code  GD 08/14/09 */
	 break;

   }


  DEXIT(DT2796);
  return ( return_code );

} /*** End get_infl */
/**/
/*==========================================================
%FUNCTION NAME
	make_term
%SCOPE
	private | static
%PURPOSE
	Returns a SP_term_ref of the form: term(lexCat, inflStr)
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	?ret_val = SP_term_ref(arg);
%RETURNS
	?
%METHOD
	?
%FILES
	?
%TABLES
	?
%FLAGS  
	TRACE DT2798
	FULL  DF2799
%HEADER END
==========================================================*/
static SP_term_ref make_term(
			     char *term,
			     char *catStr,
			     char *inflStr 
			     )

{
  static SP_term_ref pTerm;
  unsigned long      pAtom;
  SP_term_ref 	     pCat  = SP_new_term_ref();
  SP_term_ref 	     pInfl = SP_new_term_ref();
  
   DFNAME("make_term");
   DENTER(DT2798);

   pTerm = SP_new_term_ref();
   pAtom = SP_atom_from_string(catStr);

   SP_put_atom(pCat, pAtom);

   pAtom = SP_atom_from_string(inflStr);

   SP_put_atom(pInfl, pAtom);
   
   pAtom = SP_atom_from_string(term);

   SP_cons_functor(pTerm, pAtom, 2, pCat, pInfl);
   
   DEXIT(DT2798);
   
   return(pTerm);
   
} /*** End make_term */
