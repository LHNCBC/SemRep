
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

/* dm.c - derivational morphology module
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#include <fcntl.h>
#include <unistd.h>
#include "dm.h"
#include "im.h"
#include "lexicon_types.h"
#include <rpc/types.h>
#include <rpc/xdr.h>

#define is_vowel(c)  (((c)=='a')||((c)=='e')||((c)=='i')||((c)=='o')||((c)=='u'))
#define is_consonant(c)	(isalpha((c)) && !(is_vowel((c))))

/* externs */

/* function prototypes */
extern void *incr_buf_alloc(
			    void *buf,	/* pointer to buffer */
			    int unit,	/* size of one of what buf points to in bytes */
			    int size,	/* current size of buffer in units */
			    int *alloc,	/* current allocation of buffer in units */
			    int n,	/* units needed */
			    int incr 	/* increment size in units */
			    );

DmStruct *dm_variants(const char *term, dm_t cats, int *numV);
static int walk_trie(DmTrie *trie, const char *term, dm_t cats, int index);
static int try_sib(DmTrie *trie, const char *term, dm_t cats, int index);
static int read_dm_translated_rules_xdr(void);
static int read_dm_translated_facts_xdr(void);
static int check_facts(const char *term, dm_t cats);
static int is_exception(const char *term, DmRule *rule);
static void sort_by_weight(void);
bool_t xdr_dm_t(XDR *xdrs, dm_t* val);
bool_t xdr_dm_rule_header(XDR *xdrs, DmRuleHeader* ruleHeader);
bool_t xdr_dm_rule(XDR *xdrs, DmRule* rule);
bool_t xdr_dm_trie(XDR *xdrs, DmTrie* trie);
bool_t xdr_dm_xpn(XDR *xdrs, DmXpn* xpn);
bool_t xdr_dm_fact_header(XDR *xdrs, DmFactHeader* factHeader);
bool_t xdr_dm_fact(XDR *xdrs, DmFact* fact);

/* static globals */
static DmTrie *trieBuf = (DmTrie *)NULL;
static DmRule *ruleBuf = (DmRule *)NULL;
static DmXpn *xpnBuf = (DmXpn *)NULL;
static char *charBuf = (char *)NULL;
static DmFact *factBuf = (DmFact *)NULL;
static char *fcharBuf = (char *)NULL;

static int n_trieBuf = 0;
static int n_ruleBuf = 0;
static int n_xpnBuf = 0;
static int n_charBuf = 0;
static int n_factBuf = 0;
static int n_fcharBuf = 0;

static char *outBuf = (char *)NULL;		/* buffer for storing output variants */
static int n_outBuf = 0;
static int a_outBuf = 0;

static DmStruct *dmsBuf = (DmStruct *)NULL;	/* where the variants are stored */
static int n_dmsBuf = 0;
static int a_dmsBuf = 0;

static DmDRule *druleBuf = (DmDRule *)NULL;
static int n_druleBuf = 0;
static int a_druleBuf = 0;

static DmDFact *dfactBuf = (DmDFact *)NULL;
static int n_dfactBuf = 0;
static int a_dfactBuf = 0;

static char symbol_table[26];	/* for upper-case letters */

/* returns a list of derivational variants */
DmStruct *
dm_variants(const char *term, /* input term */
	    dm_t cats,  /* cats to consider */
	    int *numV  /* number of variants generated */
	    )
{
    int i;

/* read rules */
    if (!read_dm_translated_rules_xdr())
	return((DmStruct *)NULL);
/* read facts */
    if (!read_dm_translated_facts_xdr())
	return((DmStruct *)NULL);
/* clear symbol table */
    for (i=0; i<26; i++)
	symbol_table[i] = EOS;
    n_outBuf = n_dmsBuf = n_druleBuf = n_dfactBuf = *numV = 0;
    if (!walk_trie(trieBuf, term, cats, (int)strlen(term)) || n_dmsBuf == 0) {
	return((DmStruct *)NULL);
    }
/* check facts */
    if (!check_facts(term, cats))
	return((DmStruct *)NULL);
/* set the variant member */
    *numV = n_dmsBuf;
    for (i=0; i<n_dmsBuf; i++)
	(dmsBuf+i)->dmVar = (outBuf+(dmsBuf+i)->dmHide);

/* sort by wt */
    sort_by_weight();
    return(dmsBuf);
}

/* walks the trie constructing inflections */
static int
walk_trie(
	  DmTrie *trie,
	  const char *term,
	  dm_t cats,
	  int index
	  )
{
    int i;
    int j;
    DmRule *rule;
    DmStruct *dms;
    int length;
    int sFlag = 0;		/* was symbol instantiated at this level? */

    if (index < 0)
	return(1);

    if (isupper(trie->dmData))
    {
/* already instantiated? */
	if (symbol_table[trie->dmData-'A'] != EOS && symbol_table[trie->dmData-'A'] != *(term+index))
	    return(try_sib(trie, term, cats, index));

/* any vowel */
	if (is_vowel(tolower(trie->dmData)) && is_vowel(*(term+index)))
	{
	    symbol_table[trie->dmData-'A'] = *(term+index);
	    sFlag = 1;		
	}
/* any digit */
	else if (trie->dmData == 'D')
	{
	    if (!isdigit(*(term+index)))
		return(try_sib(trie, term, cats, index));
	    symbol_table[trie->dmData-'A'] = *(term+index);
	    sFlag = 1;
	}
/* any letter */
	else if (trie->dmData == 'L')
	{
	    if (!isalpha(*(term+index)))
		return(try_sib(trie, term, cats, index));
	    symbol_table[trie->dmData-'A'] = *(term+index);
	    sFlag = 1;
	}
/* any other consonant */
	else if (is_consonant(tolower(trie->dmData)) && is_consonant(*(term+index)))
	{
	    symbol_table[trie->dmData-'A'] = *(term+index);
	    sFlag = 1;
	}
	else
	    return(try_sib(trie, term, cats, index));
    }
    else
    {
	if (trie->dmData != *(term+index))
	    return(try_sib(trie, term, cats, index));
    }

    for (i=trie->dmRule; i<(trie->dmRule)+(trie->dmNumRule); i++)
    {
	DmDRule *drule;

	rule = ruleBuf+i;
	if (((cats & rule->dmInCat) == 0))
	    continue;
	if ((rule->dmFlags & DM_INPUT_SOP) && (index > 0))
	    continue;

	if (is_exception(term, rule))
	    continue;

/* check minimum stem length */
	if ((strlen(term) - strlen(charBuf+(rule->dmInSuf))) < DM_SMALLEST_STEM)
	    continue;

	if ((dmsBuf = (DmStruct *) incr_buf_alloc((void *)dmsBuf,
						  sizeof(DmStruct),
						  n_dmsBuf,
						  &a_dmsBuf,
						  (int)1,
						  (int)DM_DEFAULT_ALLOCATION)) == (DmStruct *)NULL)
	    return(0);
	if ((druleBuf = (DmDRule *) incr_buf_alloc((void *)druleBuf,
						   sizeof(DmDRule),
						   n_druleBuf,
						   &a_druleBuf,
						   (int)1,
						   (int)DM_DEFAULT_ALLOCATION)) == (DmDRule *)NULL)
	    return(0);
	dms = dmsBuf + n_dmsBuf;
	dms->dmCat = rule->dmOutCat;
	dms->dmWt = strlen(charBuf+(rule->dmInSuf));
	dms->dmFact = (DmDFact *)NULL;
	drule = druleBuf + n_druleBuf;
	drule->dmInSuf = charBuf + rule->dmInSuf;
	drule->dmInCat = rule->dmInCat;
	drule->dmOutSuf = charBuf + rule->dmOutSuf;
	drule->dmOutCat = rule->dmOutCat;
	dms->dmRule = drule;
	length = strlen(term) - strlen(charBuf+(rule->dmInSuf)) + strlen(charBuf+(rule->dmOutSuf)) + 1;
	if ((outBuf = (char *) incr_buf_alloc((void *)outBuf,
					      sizeof(char),
					      n_outBuf,
					      &a_outBuf,
					      (int)length,
					      (int)(DM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
	    return(0);
	strncpy(outBuf+n_outBuf, term, (size_t)length);
	strcpy(outBuf+n_outBuf+strlen(term)-strlen(charBuf+(rule->dmInSuf)), charBuf+(rule->dmOutSuf));
	dms->dmHide = n_outBuf;
	for (j=n_outBuf; j<n_outBuf+length; j++)
	    if (isupper(*(outBuf+j)) && symbol_table[*(outBuf+j) - 'A'] != EOS)
		*(outBuf+j) = symbol_table[*(outBuf+j) - 'A'];
	n_outBuf += length;
	n_dmsBuf++;
	n_druleBuf++;
    }

/* recurse for child */
    if (trie->dmChild != (-1))
	if (!walk_trie(trieBuf+(trie->dmChild), term, cats, index-1))
	    return(0);

/* unset any symbols that may have been instantiated at this node */
    if (sFlag == 1)
    {
	symbol_table[trie->dmData-'A'] = EOS;
	sFlag = 0;
    }

/* recurse for sibling */
    return(try_sib(trie, term, cats, index));
}

/* walks the sibling node */
static int
try_sib(DmTrie *trie, const char *term, dm_t cats, int index)
{
    return(trie->dmSib == (-1) ? (int)1 : walk_trie(trieBuf+(trie->dmSib), term, cats, index));
}

bool_t
xdr_dm_t(XDR *xdrs, dm_t* val)
{
    return(xdr_u_int(xdrs, val));    
}

bool_t
xdr_dm_rule_header(XDR *xdrs, DmRuleHeader* ruleHeader)
{
	return(xdr_long(xdrs, (long *)(&ruleHeader->dmMagic)) &&
	       xdr_long(xdrs, (long *)(&ruleHeader->dmTrie)) &&
	       xdr_long(xdrs, (long *)(&ruleHeader->dmRule)) &&
	       xdr_long(xdrs, (long *)(&ruleHeader->dmXpn) )&&
	       xdr_long(xdrs, (long *)(&ruleHeader->dmChar)));
}

bool_t
xdr_dm_rule(XDR *xdrs, DmRule* rule)
{
    return (xdr_int(xdrs,  &rule->dmInSuf) &&
	    xdr_dm_t(xdrs, &rule->dmInCat) &&
	    xdr_int(xdrs,  &rule->dmOutSuf) &&
	    xdr_dm_t(xdrs, &rule->dmOutCat) &&
	    xdr_dm_t(xdrs, &rule->dmFlags) &&
	    xdr_dm_t(xdrs, (dm_t *)(&rule->dmNumXpn)) &&
	    xdr_dm_t(xdrs, (dm_t *)(&rule->dmXpn)));
}

bool_t
xdr_dm_trie(XDR *xdrs, DmTrie* trie)
{
    return (xdr_int(xdrs,  &trie->dmData) &&
	    xdr_dm_t(xdrs, (dm_t *)(&trie->dmChild)) &&
	    xdr_dm_t(xdrs, (dm_t *)(&trie->dmSib)) &&
	    xdr_dm_t(xdrs, (dm_t *)(&trie->dmNumRule)) &&
	    xdr_dm_t(xdrs, (dm_t *)(&trie->dmRule)) &&
	    xdr_dm_t(xdrs, (dm_t *)(&trie->dmHide)));
}

bool_t
xdr_dm_xpn(XDR *xdrs, DmXpn* xpn)
{
    return (xdr_int(xdrs, &xpn->dmInTerm) &&
	    xdr_dm_t(xdrs, (dm_t *)(&xpn->dmOutTerm)));
}

/* reads in the translated rules from the rules file */
static int
read_dm_translated_rules_xdr(void)
{
    DmRuleHeader dmh;
    int fd;
    XDR xdrs;			/* pointer to an XDR *stream* */
    int n;
    FILE *fp;
    char *dm_translated_rules_file = NULL;

    if ( (dm_translated_rules_file = getenv("DM_TRANSLATED_RULES_FILE")) == NULL )
    {
	fprintf(stderr, "Cannot getenv  DM_TRANSLATED_RULES_FILE\n");
	return(0);
    }

    if (trieBuf != (DmTrie *)NULL)
	return(1);

    if ((fd = open(dm_translated_rules_file, O_RDONLY, 0644)) == (-1))
    {
	fprintf(stderr, "Cannot open DM translated rules file: %s for reading.\n",
		dm_translated_rules_file);
	return(0);
    }
    if ((fp = fdopen(fd, "r")) == NULL) 
    {
	fprintf(stderr, "Cannot open file buffer for DM translated rules file: %s for reading.\n",
		dm_translated_rules_file);
	return(0);
    }
    xdrstdio_create(&xdrs, fp, XDR_DECODE);

    if (xdr_dm_rule_header(&xdrs, &dmh) == 0) 
    {
	fprintf(stderr, "Cannot read translated rules header in file: %s.\n", dm_translated_rules_file);
	return(0);
    }
    if (dmh.dmMagic != DM_RULE_MAGIC)
    {
	fprintf(stderr, "Bad magic number in translated rules header in file: %s.\n", dm_translated_rules_file);
	return(0);
    }
    n_trieBuf = dmh.dmTrie;
    n_ruleBuf = dmh.dmRule;
    n_xpnBuf = dmh.dmXpn;
    n_charBuf = dmh.dmChar;

/* allocate and load buffers */
    if ((trieBuf = (DmTrie *) malloc((unsigned)(sizeof(DmTrie)*n_trieBuf))) == (DmTrie *)NULL)
	return(0);
    if ((ruleBuf = (DmRule *) malloc((unsigned)(sizeof(DmRule)*n_ruleBuf))) == (DmRule *)NULL)
	return(0);
    if ((xpnBuf = (DmXpn *) malloc((unsigned)(sizeof(DmXpn)*n_xpnBuf))) == (DmXpn *)NULL)
	return(0);
    if ((charBuf = malloc ((unsigned)(n_charBuf))) == (char *)NULL)
	return(0);

    n = n_trieBuf;
    if (xdr_array(&xdrs,
		  (caddr_t *)&trieBuf,
		  (uint_t *)&n_trieBuf,
		  (const uint_t)n,
		  sizeof(DmTrie),
		  (xdrproc_t)xdr_dm_trie) == 0)
	return(0);
    n = n_ruleBuf;
    if (xdr_array(&xdrs,
		  (caddr_t *)&ruleBuf,
		  (uint_t *)&n_ruleBuf,
		  (const uint_t)n,
		  sizeof(DmRule),
		  (xdrproc_t)xdr_dm_rule) == 0)
	return(0);
    n = n_xpnBuf;
    if (xdr_array(&xdrs,
		  (caddr_t *)&xpnBuf,
		  (uint_t *)&n_xpnBuf,
		  (const uint_t)n,
		  sizeof(DmXpn),
		  (xdrproc_t)xdr_dm_xpn) == 0)
	return(0);
    n = n_charBuf;
    if (xdr_bytes(&xdrs, &charBuf, (uint_t *)&n_charBuf, (const uint_t)n) == 0)
	return(0);

    xdr_destroy(&xdrs);
    return(fclose(fp) == (EOF) ? 0 : 1);
}

bool_t
xdr_dm_fact_header(XDR *xdrs, DmFactHeader* factHeader)
{
    return(xdr_int(xdrs, &factHeader->dmMagic) &&
	   xdr_int(xdrs, &factHeader->dmFact) &&
	   xdr_int(xdrs, &factHeader->dmChar));

}

bool_t
xdr_dm_fact(XDR *xdrs, DmFact* fact)
{
    return (xdr_int(xdrs, &fact->dmInTerm) &&
	    xdr_dm_t(xdrs, &fact->dmInCat) &&
	    xdr_int(xdrs, &fact->dmOutTerm) &&
	    xdr_dm_t(xdrs, &fact->dmOutCat));
}

/* reads in the translated facts from the facts file */
static int
read_dm_translated_facts_xdr(void)
{
    DmFactHeader head;
    XDR xdrs;			/* pointer to an XDR *stream* */
    int n;
    FILE *fp;
    int fd;
    char *dm_translated_facts_file = NULL;

    if ( (dm_translated_facts_file = getenv("DM_TRANSLATED_FACTS_FILE")) == NULL )
    {
	fprintf(stderr, "Cannot getenv  DM_TRANSLATED_FACTS_FILE\n");
	return(0);
    }

    if (factBuf != (DmFact *)NULL)
	return(1);

    if ((fd = open(dm_translated_facts_file, O_RDONLY, 0644)) == (-1))
    {
	fprintf(stderr, "Cannot open DM translated facts file: %s for reading.\n",
		dm_translated_facts_file);
	return(0);
    }
    if ((fp = fdopen(fd, "r")) == NULL) 
    {
	fprintf(stderr, "Cannot open file buffer for DM translated facts file: %s for reading.\n",
		dm_translated_facts_file);
	return(0);
    }
    xdrstdio_create(&xdrs, fp, XDR_DECODE);

    if (xdr_dm_fact_header(&xdrs, &head) == 0) {
	 fprintf(stderr, "Cannot read translated facts header in file: %s.\n", dm_translated_facts_file);
	 return(0);
    }
    if (head.dmMagic != DM_FACT_MAGIC)
    {
	fprintf(stderr, "Bad magic number in translated facts header in file: %s.\n", dm_translated_facts_file);
	return(0);
    }
    n_factBuf = head.dmFact;
    n_fcharBuf = head.dmChar;

/* allocate and load buffers */
    if ((factBuf = (DmFact *) malloc((unsigned)(sizeof(DmFact)*n_factBuf))) == (DmFact *)NULL)
	return(0);
    if ((fcharBuf = malloc ((unsigned)(n_fcharBuf))) == (char *)NULL)
	return(0);

    n = n_factBuf;
    if (xdr_array(&xdrs, (caddr_t *)&factBuf, (uint_t *)&n_factBuf, (const uint_t) n, sizeof(DmFact), (xdrproc_t)xdr_dm_fact) == 0)
	return(0);
    n = n_fcharBuf;
    if (xdr_bytes(&xdrs, &fcharBuf, (uint_t *)&n_fcharBuf, (const uint_t)n) == 0)
	return(0);

    xdr_destroy(&xdrs);
    return(fclose(fp) == (EOF) ? 0 : 1);
}

/* checks the facts */
static int
check_facts(const char *term, dm_t cats)
{
    int low, mid, high;
    int cmp, length;
    DmFact *fact;
    DmStruct *dms;

    low=0;
    high=n_factBuf-1;

    while (low<=high)
    {
	mid = (low+high)/2;
	fact = factBuf+mid;
	cmp = strcmp(term, fcharBuf+(fact->dmInTerm));
	if (cmp > 0)
	    low = mid+1;
	else if (cmp < 0)
	    high = mid-1;
	else
	{
	    while (mid >= low && strcmp(term, fcharBuf+(fact->dmInTerm)) == 0)
	    {
		mid--;
		fact = factBuf+mid;
	    }
	    mid++;
	    fact = factBuf+mid;

	    while (mid <= high && strcmp(term, fcharBuf+(fact->dmInTerm)) == 0)
	    {
		if ((cats & fact->dmInCat))
		{
		    DmDFact *dfact;

		    if ((dmsBuf = (DmStruct *) incr_buf_alloc((void *)dmsBuf,
							      sizeof(DmStruct),
							      n_dmsBuf,
							      &a_dmsBuf,
							      (int)1,
							      (int)(DM_DEFAULT_ALLOCATION))) == (DmStruct *)NULL)
			return(0);
		    if ((dfactBuf = (DmDFact *) incr_buf_alloc((void *)dfactBuf,
							       sizeof(DmDFact),
							       n_dfactBuf,
							       &a_dfactBuf,
							       (int)1,
							       (int)DM_DEFAULT_ALLOCATION)) == (DmDFact *)NULL)
			return(0);
		    dms = dmsBuf+n_dmsBuf;
		    dms->dmCat = fact->dmOutCat;
/* variants stated as facts get a high weight */
		    dms->dmWt = 50;
		    dms->dmRule = (DmDRule *)NULL;
		    dfact = dfactBuf + n_dfactBuf;
		    dfact->dmInTerm = fcharBuf + (fact->dmInTerm);
		    dfact->dmInCat = fact->dmInCat;
		    dfact->dmOutTerm = fcharBuf + (fact->dmOutTerm);
		    dfact->dmOutCat = fact->dmOutCat;
		    dms->dmFact = dfact;
		    length = strlen(fcharBuf+(fact->dmOutTerm)) + 1;
		    if ((outBuf = (char *) incr_buf_alloc((void *)outBuf,
							  sizeof(char),
							  n_outBuf,
							  &a_outBuf,
							  (int)length,
							  (int)(DM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
			return(0);
		    strcpy(outBuf+n_outBuf, fcharBuf+(fact->dmOutTerm));
		    dms->dmHide = n_outBuf;
		    n_outBuf += length;
		    n_dmsBuf++;
		    n_dfactBuf++;
		}
		mid++;
		fact = factBuf+mid;
	    }
	    return(1);
	}
    }
    return(1);
}

/* (binary) search exception list */
static int
is_exception(const char *term, DmRule *rule)
{
    DmXpn *xpn;
    int low, mid, high;
    int cmp;

    low=rule->dmXpn;
    high=rule->dmXpn+rule->dmNumXpn-1;

    while (low<=high)
    {
	mid = (low+high)/2;
	xpn = xpnBuf+mid;
	cmp = strcmp(term, charBuf+(xpn->dmInTerm));
	if (cmp > 0)
	    low = mid+1;
	else if (cmp < 0)
	    high = mid-1;
	else
	    return(1);
    }
    return(0);
}

/* sorts the dms buffer by reverse length of match */
static void
sort_by_weight(void)
{
    int gap, i, j;
    int tmpI;
    char *tmpC;
    DmDRule *drule;
    DmDFact *dfact;

    for (gap = n_dmsBuf/2; gap>0; gap /= 2)
    {
	for (i=gap; i<n_dmsBuf; i++)
	{
	    for (j=i-gap; j>=0; j-=gap)
	    {
		if ((dmsBuf+j)->dmWt >= (dmsBuf+j+gap)->dmWt)
		    break;
		tmpI = (int)(dmsBuf+j)->dmCat;
		(dmsBuf+j)->dmCat = (dmsBuf+j+gap)->dmCat;
		(dmsBuf+j+gap)->dmCat = (dm_t)tmpI;

		tmpI = (int)(dmsBuf+j)->dmHide;
		(dmsBuf+j)->dmHide = (dmsBuf+j+gap)->dmHide;
		(dmsBuf+j+gap)->dmHide = (dm_t)tmpI;

		tmpC = (dmsBuf+j)->dmVar;
		(dmsBuf+j)->dmVar = (dmsBuf+j+gap)->dmVar;
		(dmsBuf+j+gap)->dmVar = tmpC;

		tmpI = (int)(dmsBuf+j)->dmWt;
		(dmsBuf+j)->dmWt = (dmsBuf+j+gap)->dmWt;
		(dmsBuf+j+gap)->dmWt = (dm_t)tmpI;

		drule = (dmsBuf+j)->dmRule;
		(dmsBuf+j)->dmRule = (dmsBuf+j+gap)->dmRule;
		(dmsBuf+j+gap)->dmRule = drule;

		dfact = (dmsBuf+j)->dmFact;
		(dmsBuf+j)->dmFact = (dmsBuf+j+gap)->dmFact;
		(dmsBuf+j+gap)->dmFact = dfact;
	    }
	}
    }
}

/*
main()
{
    char s[256];
    DmStruct *dms;
    int n;
    int i;

    while (gets(s) != (char *)NULL)
    {
	if ((dms=dm_variants(s, DM_CAT_NOUN, &n)) == (DmStruct *)NULL)
	{
	    printf("No variants.\n");
	}
	else
	{
	    for (i=0; i<n; i++)
		printf("%d|%s\n", (dms+i)->dmCat, (dms+i)->dmVar);
	}
    }
}
*/
