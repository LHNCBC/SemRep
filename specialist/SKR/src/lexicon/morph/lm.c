
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

/* lm.c - inflectional morphology module for the SPECIALIST lexicon.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#include <fcntl.h>
#include "lm.h"
#include "lexicon_types.h"
#include <rpc/types.h>
#include <rpc/xdr.h>

#define is_vowel(c)  (((c)=='a')||((c)=='e')||((c)=='i')||((c)=='o')||((c)=='u'))
#define is_consonant(c)	(isalpha((c)) && !(is_vowel((c))))

/* function prototypes */
extern void *incr_buf_alloc(
			    void *buf,	/* pointer to buffer */
			    int unit,	/* size of one of what buf points to in bytes */
			    int size,	/* current size of buffer in units */
			    int *alloc,	/* current allocation of buffer in units */
			    int n,	/* units needed */
			    int incr 	/* increment size in units */
			    );

LmStruct *lm_variants(char *term, lm_t cats, lm_t rules, lm_t infls, int *numV);
static int walk_trie(LmTrie *trie, char *term, lm_t cats, lm_t rules,lm_t infls, int index);
static int try_sib(LmTrie *trie, char *term, lm_t cats, lm_t rules, lm_t infls, int index);
static int read_lm_translated_rules_xdr(void);
static void sort_by_weight(void);
static void unique_lms_buf(void);
static void delete_lms_buf(int n);
bool_t xdr_lm_t(XDR *xdrs, lm_t* val);
bool_t xdr_lm_rule_header(XDR *xdrs, LmRuleHeader* ruleHeader);
bool_t xdr_lm_trie(XDR *xdrs, LmTrie* trie);
bool_t xdr_lm_rule(XDR *xdrs, LmRule* rule);


/* static globals */
static LmTrie *trieBuf = (LmTrie *)NULL;
static LmRule *ruleBuf = (LmRule *)NULL;
static char *charBuf = (char *)NULL;

static int n_trieBuf = 0;
static int n_ruleBuf = 0;
static int n_charBuf = 0;

static char *outBuf = (char *)NULL;		/* buffer for storing output variants */
static int n_outBuf = 0;
static int a_outBuf = 0;

static LmStruct *lmsBuf = (LmStruct *)NULL;	/* where the variants are stored */
static int n_lmsBuf = 0;
static int a_lmsBuf = 0;

static LmDRule *druleBuf = (LmDRule *)NULL;
static int n_druleBuf = 0;
static int a_druleBuf = 0;

static char symbol_table[26];	/* for upper-case letters */

/*  gets all possible inflections of term in any of categories: cats,
    generating inflections: infls, and using rules: rules.

	cats: are an OR'ing of:
	    LM_CAT_ADJ, LM_CAT_NOUN, LM_CAT_VERB
	infls: are an OR'ing of:
	    LM_INFL_BASE, LM_INFL_POSITIVE, LM_INFL_SINGULAR,
	    LM_INFL_INFINITIVE, LM_INFL_COMPARTIVE, LM_INFL_SUPERLATIVE,
	    LM_INFL_PLURAL, LM_INFL_PRESENT, LM_INFL_PAST, LM_INFL_ING.
*/
LmStruct *
lm_variants(term, cats, rules, infls, numV)
    char *term;			/* input term */
    lm_t cats;			/* cats to consider */
    lm_t rules;			/* rules to apply */
    lm_t infls;			/* infls to obtain */
    int *numV;			/* number of variants generated */
{
    int i;

    /* read rules */
    if (!read_lm_translated_rules_xdr())
	return((LmStruct *)NULL);

    /* clear symbol table */
    for (i=0; i<26; i++)
	symbol_table[i] = EOS;

    n_outBuf = n_lmsBuf = n_druleBuf = *numV = 0;
    if (!walk_trie(trieBuf, term, cats, rules, infls, (int)strlen(term)) || n_lmsBuf == 0)
	return((LmStruct *)NULL);

    /* set the variant member */
    for (i=0; i<n_lmsBuf; i++)
	(lmsBuf+i)->lmVar = (outBuf+(lmsBuf+i)->lmHide);

    /* sort by wt */
    sort_by_weight();

    /* pick the best inflections */
    unique_lms_buf();

    *numV = n_lmsBuf;
    return(lmsBuf);
}

/* walks the trie constructing inflections */
static int
walk_trie(trie, term, cats, rules, infls, index)
    LmTrie *trie;
    char *term;
    lm_t cats;
    lm_t rules;
    lm_t infls;
    int index;
{
    int i;
    int j;
    LmRule *rule;
    LmStruct *lms;
    int length;
    int sFlag = 0;		/* was symbol instantiated at this level? */

    if (index < 0)
	return(1);

    if (isupper(trie->lmData))
    {
/* already instantiated? */
	if (symbol_table[trie->lmData-'A'] != EOS && symbol_table[trie->lmData-'A'] != *(term+index))
	    return(try_sib(trie, term, cats, rules, infls, index));

/* any vowel */
	if (is_vowel(tolower(trie->lmData)) && is_vowel(*(term+index)))
	{
	    symbol_table[trie->lmData-'A'] = *(term+index);
	    sFlag = 1;		
	}
/* any digit */
	else if (trie->lmData == 'D')
	{
	    if (!isdigit((int)*(term+index)))
		return(try_sib(trie, term, cats, rules, infls, index));
	    symbol_table[trie->lmData-'A'] = *(term+index);
	    sFlag = 1;
	}
/* any letter */
	else if (trie->lmData == 'L')
	{
	    if (!isalpha((int)*(term+index)))
		return(try_sib(trie, term, cats, rules, infls, index));
	    symbol_table[trie->lmData-'A'] = *(term+index);
	    sFlag = 1;
	}
/* any other consonant */
	else if (is_consonant(tolower(trie->lmData)) && is_consonant((int)*(term+index)))
	{
	    symbol_table[trie->lmData-'A'] = *(term+index);
	    sFlag = 1;
	}
	else
	    return(try_sib(trie, term, cats, rules, infls, index));
    }
    else
    {
	if (trie->lmData != *(term+index))
	    return(try_sib(trie, term, cats, rules, infls, index));
    }

/* add inflections */
    for (i=trie->lmRule; i<(trie->lmRule)+(trie->lmNumRule); i++)
    {
	LmDRule *drule;

	rule = ruleBuf+i;
	if (((cats & rule->lmInCat) == 0) || ((rules & rule->lmRule) == 0) ||
		((infls & rule->lmOutInfl) == 0))
	    continue;
	if ((rule->lmFlags & LM_INPUT_SOP) && (index > 0))
	    continue;

	if ((lmsBuf = (LmStruct *) incr_buf_alloc((void *)lmsBuf,
						  sizeof(LmStruct),
						  n_lmsBuf,
						  &a_lmsBuf,
						  (int)1,
						  (int)LM_DEFAULT_ALLOCATION)) == (LmStruct *)NULL)
	    return(0);
	if ((druleBuf = (LmDRule *) incr_buf_alloc((void *)druleBuf,
						   sizeof(LmDRule),
						   n_druleBuf,
						   &a_druleBuf,
						   (int)1,
						   (int)LM_DEFAULT_ALLOCATION)) == (LmDRule *)NULL)
	    return(0);

	lms = lmsBuf + n_lmsBuf;
	lms->lmCat = rule->lmOutCat;
	lms->lmRule = rule->lmRule;
	lms->lmInfl = rule->lmOutInfl;
	lms->lmWt = strlen(charBuf+(rule->lmInSuf));
	drule = druleBuf + n_druleBuf;
	drule->lmInSuf = charBuf+rule->lmInSuf;
	drule->lmInCat = rule->lmInCat;
	drule->lmInInfl = rule->lmInInfl;
	drule->lmRule = rule->lmRule;
	drule->lmOutSuf = charBuf+rule->lmOutSuf;
	drule->lmOutCat = rule->lmOutCat;
	drule->lmOutInfl = rule->lmOutInfl;
	lms->lmDRule = drule;
	length = strlen(term) - strlen(charBuf+(rule->lmInSuf)) + strlen(charBuf+(rule->lmOutSuf)) + 1;
	if ((outBuf = (char *) incr_buf_alloc((void *)outBuf,
					      sizeof(char),
					      n_outBuf,
					      &a_outBuf,
					      (int)length,
					      (int)(LM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
	    return(0);
	strncpy(outBuf+n_outBuf, term, (size_t)length);
	strcpy(outBuf+n_outBuf+strlen(term)-strlen(charBuf+(rule->lmInSuf)), charBuf+(rule->lmOutSuf));
	lms->lmHide = n_outBuf;
	for (j=n_outBuf+strlen(term)-strlen(charBuf+(rule->lmInSuf)); j<n_outBuf+length; j++)
		if (isupper((int)*(outBuf+j)) && symbol_table[*(outBuf+j) - 'A'] != EOS)
		*(outBuf+j) = symbol_table[*(outBuf+j) - 'A'];
	n_outBuf += length;
	n_lmsBuf++;
	n_druleBuf++;
    }

/* recurse for child */
    if (trie->lmChild != (-1))
	if (!walk_trie(trieBuf+(trie->lmChild), term, cats, rules, infls, index-1))
	    return(0);

/* unset any symbols that may have been instantiated at this level of recursion */
    if (sFlag == 1)
    {
	symbol_table[trie->lmData-'A'] = EOS;
	sFlag = 0;
    }

/* recurse for sibling */
    return(try_sib(trie, term, cats, rules, infls, index));
}

/* walks the sibling node */
static int
try_sib(trie, term, cats, rules, infls, index)
    LmTrie *trie;
    char *term;
    lm_t cats;
    lm_t rules;
    lm_t infls;
    int index;
{
    return(trie->lmSib == (-1) ? (int)1 :
	walk_trie(trieBuf+(trie->lmSib), term, cats, rules, infls, index));
}

bool_t
xdr_lm_t(XDR *xdrs, lm_t* val)
{
    return(xdr_u_int(xdrs, val));    
}

bool_t
xdr_lm_rule_header(XDR *xdrs, LmRuleHeader* ruleHeader)
{
    return(xdr_int(xdrs, &ruleHeader->lmMagic) &&
	   xdr_int(xdrs, &ruleHeader->lmTrie) &&
	   xdr_int(xdrs, &ruleHeader->lmRule) &&
	   xdr_int(xdrs, &ruleHeader->lmChar));

}

bool_t
xdr_lm_trie(XDR *xdrs, LmTrie* trie)
{
    return (xdr_int(xdrs, &trie->lmData) &&
	    xdr_int(xdrs, &trie->lmChild) &&
	    xdr_int(xdrs, &trie->lmSib) &&
	    xdr_int(xdrs, &trie->lmNumRule) &&
	    xdr_int(xdrs, (int*)(&trie->lmRule)) &&
	    xdr_int(xdrs, (int*)(&trie->lmHide)));
}

bool_t
xdr_lm_rule(XDR *xdrs, LmRule* rule)
{
    return (xdr_int(xdrs,  &rule->lmInSuf) &&
	    xdr_lm_t(xdrs, &rule->lmInCat) &&
	    xdr_lm_t(xdrs, &rule->lmInInfl) &&
	    xdr_lm_t(xdrs, &rule->lmRule) &&
	    xdr_int(xdrs,  &rule->lmOutSuf) &&
	    xdr_lm_t(xdrs, &rule->lmOutCat) &&
	    xdr_lm_t(xdrs, &rule->lmOutInfl) &&
	    xdr_lm_t(xdrs, &rule->lmFlags));
}

/* reads in the translated rules from the rules file */
static int
read_lm_translated_rules_xdr()
{
    LmRuleHeader lmh;
    int fd;
    XDR xdrs;			/* pointer to an XDR *stream* */
    int n;
    FILE *fp;
    char *lm_translated_rules_file = NULL;

    if ( (lm_translated_rules_file = getenv("LM_TRANSLATED_RULES_FILE")) == NULL )
    {
	fprintf(stderr, "Cannot getenv  LM_TRANSLATED_RULES_FILE\n");
	return(0);
    }

    if (trieBuf != (LmTrie *)NULL)
	return(1);

    if ((fd = open(lm_translated_rules_file, O_RDONLY, 0644)) == (-1))
    {
	fprintf(stderr, "Cannot open LM translated rules file: %s for reading.\n",
		lm_translated_rules_file);
	return(0);
    }
    if ((fp = fdopen(fd, "r")) == NULL) 
    {
	fprintf(stderr, "Cannot open file buffer for DM translated rules file: %s for reading.\n",
		lm_translated_rules_file);
	return(0);
    }
    xdrstdio_create(&xdrs, fp, XDR_DECODE);

    if (xdr_lm_rule_header(&xdrs, &lmh) == 0) 
    {
	fprintf(stderr, "Cannot read translated rules header in file: %s.\n", lm_translated_rules_file);
	return(0);
    }

    if (lmh.lmMagic != LM_RULE_MAGIC)
    {
	fprintf(stderr, "Bad magic number in translated rules header in file: %s.\n", lm_translated_rules_file);
	return(0);
    }


/* allocate and load buffers */
    n_trieBuf = lmh.lmTrie;
    n = n_trieBuf;
    if ((trieBuf = (LmTrie *) malloc((unsigned)(sizeof(LmTrie)*n_trieBuf))) == (LmTrie *)NULL)
	return(0);

    if (xdr_array(&xdrs,
		  (caddr_t *)&trieBuf,
		  (uint_t *)&n_trieBuf,
		  (const uint_t)n,
		  sizeof(LmTrie),
		  (xdrproc_t)xdr_lm_trie) == 0)
	return(0);

    n_ruleBuf = lmh.lmRule;
    n = n_ruleBuf;
    if ((ruleBuf = (LmRule *) malloc((unsigned)(sizeof(LmRule)*n_ruleBuf))) == (LmRule *)NULL)
	return(0);
    if (xdr_array(&xdrs,
		  (caddr_t *)&ruleBuf,
		  (uint_t *)&n_ruleBuf,
		  (const uint_t)n,
		  sizeof(LmRule),
		  (xdrproc_t)xdr_lm_rule) == 0)
	return(0);

    n_charBuf = lmh.lmChar;
    n = n_charBuf;
    if ((charBuf = malloc ((unsigned)(n_charBuf))) == (char *)NULL)
	return(0);
    if (xdr_bytes(&xdrs, &charBuf, (uint_t *)&n_charBuf, (const uint_t)n) == 0)
	return(0);

    xdr_destroy(&xdrs);
    return((fclose(fp) == EOF) ? 0 : 1);
}

/* sorts the lms buffer by reverse length of match */
static void
sort_by_weight()
{
    int gap, i, j;
    int tmpI;
    char *tmpC;
    LmDRule *drule;

    for (gap = n_lmsBuf/2; gap>0; gap /= 2)
    {
	for (i=gap; i<n_lmsBuf; i++)
	{
	    for (j=i-gap; j>=0; j-=gap)
	    {
		if ((lmsBuf+j)->lmWt >= (lmsBuf+j+gap)->lmWt)
		    break;
		tmpI = (int)(lmsBuf+j)->lmCat;
		(lmsBuf+j)->lmCat = (lmsBuf+j+gap)->lmCat;
		(lmsBuf+j+gap)->lmCat = (lm_t)tmpI;

		tmpI = (int)(lmsBuf+j)->lmRule;
		(lmsBuf+j)->lmRule = (lmsBuf+j+gap)->lmRule;
		(lmsBuf+j+gap)->lmRule = (lm_t)tmpI;

		tmpI = (int)(lmsBuf+j)->lmInfl;
		(lmsBuf+j)->lmInfl = (lmsBuf+j+gap)->lmInfl;
		(lmsBuf+j+gap)->lmInfl = (lm_t)tmpI;

		tmpI = (int)(lmsBuf+j)->lmHide;
		(lmsBuf+j)->lmHide = (lmsBuf+j+gap)->lmHide;
		(lmsBuf+j+gap)->lmHide = (lm_t)tmpI;

		tmpC = (lmsBuf+j)->lmVar;
		(lmsBuf+j)->lmVar = (lmsBuf+j+gap)->lmVar;
		(lmsBuf+j+gap)->lmVar = tmpC;

		tmpI = (int)(lmsBuf+j)->lmWt;
		(lmsBuf+j)->lmWt = (lmsBuf+j+gap)->lmWt;
		(lmsBuf+j+gap)->lmWt = (lm_t)tmpI;

		drule = (lmsBuf+j)->lmDRule;
		(lmsBuf+j)->lmDRule = (lmsBuf+j+gap)->lmDRule;
		(lmsBuf+j+gap)->lmDRule = drule;

	    }
	}
    }
}

/* uniques the lms buffer preferring the best match */
static void
unique_lms_buf()
{
    int i;
    int j;
    LmStruct *lmsi, *lmsj;

    for (i=0; i<n_lmsBuf; i++)
    {
	lmsi = lmsBuf+i;
	for (j=i+1; j<n_lmsBuf; j++)
	{
	    lmsj = lmsBuf+j;
	    if (lmsi->lmCat == lmsj->lmCat &&
		    lmsi->lmRule == lmsj->lmRule &&
		    lmsi->lmInfl == lmsj->lmInfl)
	    {
		delete_lms_buf(j);
		j--;
	    }
	}
    }
}

/* removes the n'th entry */
static void
delete_lms_buf(n)
    int n;
{
    /* ARA bcopy((char *)(lmsBuf+n+1), (char *)(lmsBuf+n), (int)(sizeof(LmStruct)*(n_lmsBuf-n-1))); */
    memmove((char *)(lmsBuf+n), (char *)(lmsBuf+n+1), (size_t)(sizeof(LmStruct)*(n_lmsBuf-n-1)));
    n_lmsBuf--;
}
