
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

/* lm_rule_tran.c - creates a trie from rules in a file
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#include <errno.h>
#include "lm.h"
#include "lexicon_types.h"
#include <rpc/types.h>
#include <rpc/xdr.h>

/*  Translates (suffix) rules in the rules file to a trie.
    Prints error messages to stderr if the rules or exceptions lines
    have incorrect format, or the exceptions don't have the right
    suffixes.  Prints warnings for duplicate rules.

    Input structure is a file containing:

    InSuf|InCat|InInfl|Rule|OutSuf|OutCat|OutInfl

    where the line contains seven fields.

    Comment lines (a la sh) and lines with whitespace are allowed
    in the input.

    Results are written to standard output.
*/

int *obsolete_get_fields(char *line, int sep, int *numF);
void *incr_buf_alloc(
		     void *buf,	/* pointer to buffer */
		     int unit,	/* size of one of what buf points to in bytes */
		     int size,	/* current size of buffer in units */
		     int *alloc,	/* current allocation of buffer in units */
		     int n,	/* units needed */
		     int incr 	/* increment size in units */
		     );
int push_loc(char *includeFile, int *linenoP, char **curFileP);
int pop_loc( int *linenoP, char **curFileP);
static int read_rules(void);
static int reorganize_rules(void);
static int load_rule(void);
static int reverse_rule(int ruleNum);
static int same_rule(LmRule *r1, LmRule *r2);
static int get_cat(char *s, lm_t *c);
static char *cat_get(lm_t c);
static int get_rule(char *s, lm_t *c);
static char *rule_get(lm_t c);
static int get_infl(char *s, lm_t *c);
static char *infl_get(lm_t c);

bool_t xdr_lm_t(XDR *xdrs, lm_t* val);
bool_t xdr_lm_rule_header(XDR *xdrs, LmRuleHeader* ruleHeader);
bool_t xdr_lm_trie(XDR *xdrs, LmTrie* trie);
bool_t xdr_lm_rule(XDR *xdrs, LmRule* rule);
static int write_lm_rules_xdr(void);

/* globals */
LmTrie *trieBuf = (LmTrie *)NULL;	/* in-memory trie */
int n_trieBuf = 0;			/* number of nodes in trie */
int a_trieBuf = 0;			/* allocated nodes in trie */

char *charBuf = (char *)NULL;	/* buffer containing character data, i.e., suffixes */
int n_charBuf = 0;		/* actual size of buffer */
int a_charBuf = 0;		/* bytes allocated */

LmRule *ruleBuf = (LmRule *)NULL;   /* buffer for storing the rules */
int n_ruleBuf = 0;		/* actual size of buffer */
int a_ruleBuf = 0;		/* allocated size */

LmRule *ruleBuf2 = (LmRule *)NULL;   /* buffer for storing the rules after reorganization */

static FILE *fp=NULL;

/* MAIN */
int main(
	 int	argc,
	 char	*argv[]
	 )
{
    fp=stdin;
    if (!read_rules())
	return(2);

    if (!reorganize_rules())
	return(2);

    if (!write_lm_rules_xdr())
	return(2);

    if (fclose(fp) == EOF)
	return(2);

    return(0);
}

/* reads in rules from file and creates in-memory trie */
static int
read_rules()
{
    static char s[] = "lm.rules";
    int lineno = 0;
    char *curFile = s;
    int i;
    int errFlag = 0;
    LmRule *rule;
    char line[512];
    int *fields;
    int fieldNum;
    int n;
    char buf[64];
    char *start;
    char *sp;
    int length;
    int l;


    while (fgets(line, sizeof(line), fp) != (char *)NULL)
    {
	line[strlen(line)-1] = EOS;
	lineno++;

/* include file */
	if (strncmp(line, "#include ", 9) == 0 && *(line+9) == '"')
	{
	    (void) push_loc(line, &lineno, &curFile);
	    continue;
	}

/* endinclude */
	if (strcmp(line, "#endinclude") == 0)
	{
	    (void) pop_loc(&lineno, &curFile);
	    continue;
	}

/* comment */
	if (*line == LM_COMMENT_CHAR)
	    continue;
	i=0;
	while (isspace((int)line[i]))
	    i++;

/* all whitespace? */
	if (line[i] == EOS)
	    continue;

	if ((ruleBuf = (LmRule *) incr_buf_alloc((void *)ruleBuf,
						 sizeof(LmRule),
						 n_ruleBuf,
						 &a_ruleBuf,
						 (int)1,
						 (int)LM_DEFAULT_ALLOCATION)) == (LmRule *)NULL)
	    return(0);
	rule = ruleBuf+n_ruleBuf;
	rule->lmInSuf = rule->lmOutSuf = (-1);
	rule->lmFlags = 0;

	if ((fields=obsolete_get_fields(line, LM_FIELD_SEPARATOR_CHAR, &n)) == (int *)NULL)
	{
	    fprintf(stderr, "ERROR: Cannot break rule in file: %s, line: %d into fields!\n", curFile, lineno);
	    errFlag++;
	    continue;
	}
	if (n != 7)
	{
	    fprintf(stderr, "ERROR: Expecting 7 fields in file: %s, line: %d, found: %d!\n", curFile, lineno, n);
	    errFlag++;
	    continue;
	}

/* input suffix */
	fieldNum = 0;
	start = sp = &line[*(fields+2*fieldNum)];
	length = l = *(fields+2*fieldNum+1);
	if (*sp == LM_SOP_CHAR)
	{
	    rule->lmFlags |= LM_INPUT_SOP;
	    start = sp+1;
	    length--;
	}
	if (l>0 && *(sp+l-1) == LM_EOP_CHAR)
	{
	    rule->lmFlags |= LM_INPUT_EOP;
	    length--;
	}
	if ((charBuf = (char *) incr_buf_alloc((void *)charBuf,
					       sizeof(char),
					       n_charBuf,
					       &a_charBuf,
					       (int)(length+1),
					       (int)(LM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
	    return(0);
	strncpy(charBuf+n_charBuf, start, (size_t)length);
	*(charBuf+n_charBuf+length) = EOS;
	rule->lmInSuf = n_charBuf;
	n_charBuf += length+1;

/* input category */
	fieldNum++;
	start = &line[*(fields+2*fieldNum)];
	length = *(fields+2*fieldNum+1);
	strncpy(buf, start, (size_t)length);
	*(buf+length) = EOS;
	if (!get_cat(buf, &(rule->lmInCat)))
	{
	    fprintf(stderr, "ERROR: Input category incorrect in file: %s, line: %d\n", curFile, lineno);
	    errFlag++;
	    continue;
	}

/* input inflection */
	fieldNum++;
	start = &line[*(fields+2*fieldNum)];
	length = *(fields+2*fieldNum+1);
	strncpy(buf, start, (size_t)length);
	*(buf+length) = EOS;
	if (!get_infl(buf, &(rule->lmInInfl)))
	{
	    fprintf(stderr, "ERROR: Input inflection incorrect in file: %s, line: %d\n", curFile, lineno);
	    errFlag++;
	    continue;
	}

/* RULE */
	fieldNum++;
	start = &line[*(fields+2*fieldNum)];
	length = *(fields+2*fieldNum+1);
	strncpy(buf, start, (size_t)length);
	*(buf+length) = EOS;
	if (!get_rule(buf, &(rule->lmRule)))
	{
	    fprintf(stderr, "ERROR: Rule incorrect in file: %s, line: %d\n", curFile, lineno);
	    errFlag++;
	    continue;
	}

/* output suffix */
	fieldNum++;
	start = sp = &line[*(fields+2*fieldNum)];
	length = l = *(fields+2*fieldNum+1);
	if (*start == LM_SOP_CHAR)
	{
	    rule->lmFlags |= LM_OUTPUT_SOP;
	    start = sp+1;
	    length--;
	}
	if (l>0 && *(sp+l-1) == LM_EOP_CHAR)
	{
	    rule->lmFlags |= LM_OUTPUT_EOP;
	    length--;
	}
	if ((charBuf = (char *) incr_buf_alloc((void *)charBuf,
					       sizeof(char),
					       n_charBuf,
					       &a_charBuf,
					       (int)(length+1),
					       (int)(LM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
	    return(0);
	strncpy(charBuf+n_charBuf, start, (size_t)length);
	*(charBuf+n_charBuf+length) = EOS;
	rule->lmOutSuf = n_charBuf;
	n_charBuf += length+1;

/* output category */
	fieldNum++;
	start = &line[*(fields+2*fieldNum)];
	length = *(fields+2*fieldNum+1);
	strncpy(buf, start, (size_t)length);
	*(buf+length) = EOS;
	if (!get_cat(buf, &(rule->lmOutCat)))
	{
	    fprintf(stderr, "ERROR: Output category incorrect in file: %s, line: %d\n", curFile, lineno);
	    errFlag++;
	    continue;
	}

/* output inflection */
	fieldNum++;
	start = &line[*(fields+2*fieldNum)];
	length = *(fields+2*fieldNum+1);
	strncpy(buf, start, (size_t)length);
	*(buf+length) = EOS;
	if (!get_infl(buf, &(rule->lmOutInfl)))
	{
	    fprintf(stderr, "ERROR: Output inflection incorrect in file: %s, line: %d\n", curFile, lineno);
	    errFlag++;
	    continue;
	}
	if (!load_rule())
	    return(2);
	n_ruleBuf++;
	if ((ruleBuf = (LmRule *) incr_buf_alloc((void *)ruleBuf,
						 sizeof(LmRule),
						 n_ruleBuf,
						 &a_ruleBuf,
						 (int)1,
						 (int)LM_DEFAULT_ALLOCATION)) == (LmRule *)NULL)
	    return(0);
	if (!reverse_rule(n_ruleBuf-1))
	    return(0);
	if (!load_rule())
	    return(0);
	n_ruleBuf++;
    }
    return(errFlag ? 0 : 1);
}

/* loads a rule in trie */
static int
load_rule()
{
    int cNode;		/* current node */
    int pNode = 0;	/* parent node */
    int rNode = 0;	/* previous node */
    int new = 0;
    char *cp;
    LmRule *rule = ruleBuf+n_ruleBuf;
    int n;

    cNode = pNode;

    if (n_trieBuf == 0)
    {
	if ((trieBuf = (LmTrie *) incr_buf_alloc((void *)trieBuf,
						 sizeof(LmTrie),
						 n_trieBuf,
						 &a_trieBuf,
						 (int)1,
						 (int)(LM_DEFAULT_ALLOCATION*2))) == (LmTrie *)NULL)
	    return(0);
	trieBuf->lmData = (int)EOS;
	trieBuf->lmChild = trieBuf->lmSib = (-1);
	trieBuf->lmNumRule = 0;
	trieBuf->lmHide = (int *)NULL;
	n_trieBuf++;
    }

/* find trie node for insertion */
    for (cp = &charBuf[rule->lmInSuf] + strlen(&charBuf[rule->lmInSuf]); cp >= &charBuf[rule->lmInSuf]; cp--)
    {
	if (cNode == (-1) || ((int)(*cp) < (trieBuf+cNode)->lmData))
	{
	    if ((trieBuf = (LmTrie *) incr_buf_alloc((void *)trieBuf,
						     sizeof(LmTrie),
						     n_trieBuf,
						     &a_trieBuf,
						     (int)1,
						     (int)(LM_DEFAULT_ALLOCATION*2))) == (LmTrie *)NULL)
		return(0);
	    new = n_trieBuf;
	    (trieBuf+new)->lmData = (int)(*cp);
	    (trieBuf+new)->lmChild = (-1);
	    (trieBuf+new)->lmSib = cNode;
	    (trieBuf+new)->lmNumRule = 0;
	    (trieBuf+new)->lmHide = (int *)NULL;
	    (trieBuf+pNode)->lmChild = new;
	    n_trieBuf++;
	}
	else
	{
	    while (cNode != (-1) && (trieBuf+cNode)->lmData < (int)(*cp))
	    {
		rNode = cNode;
		cNode = (trieBuf+cNode)->lmSib;
	    }
	    if (cNode == (-1) || (trieBuf+cNode)->lmData != (int)(*cp))
	    {
		if ((trieBuf = (LmTrie *) incr_buf_alloc((void *)trieBuf,
							 sizeof(LmTrie),
							 n_trieBuf,
							 &a_trieBuf,
							 (int)1,
							 (int)(LM_DEFAULT_ALLOCATION*2))) == (LmTrie *)NULL)
		    return(0);
		new = n_trieBuf;
		(trieBuf+new)->lmData = (int)(*cp);
		(trieBuf+new)->lmChild = (-1);
		(trieBuf+new)->lmSib = cNode;
		(trieBuf+new)->lmNumRule = 0;
		(trieBuf+new)->lmHide = (int *)NULL;
		(trieBuf+rNode)->lmSib = new;
		n_trieBuf++;
	    }
	    else
	    {
		new = cNode;
	    }
	}
	pNode = rNode = new;
	cNode = (trieBuf+pNode)->lmChild;
    }

    cNode = new;
    rule = ruleBuf+n_ruleBuf;

/* check for duplicates */
    for (n=0; n<(trieBuf+cNode)->lmNumRule; n++)
    {
	if (same_rule(rule, ruleBuf + *(((trieBuf+cNode)->lmHide)+n)))
	{
	    fprintf(stderr, "ERROR: There were multiple rules of the type: \
		%s|%s|%s|%s|%s|%s|%s\n",
		    (char *)rule->lmInSuf, cat_get(rule->lmInCat),
		    infl_get(rule->lmInInfl), rule_get(rule->lmRule),
		    (char *)rule->lmOutSuf, cat_get(rule->lmOutCat), infl_get(rule->lmOutInfl));
	    return(0);
	}
    }

/* add rule */
    n = (trieBuf+cNode)->lmNumRule;
    if (n == 0)
    {
	if ((((trieBuf+cNode)->lmHide) = (int *) malloc((unsigned)(sizeof(int)*(n+1)))) == (int *)NULL)
	    return(0);
    }
    else
    {
	if ((((trieBuf+cNode)->lmHide) = (int *) realloc((char *)(((trieBuf+cNode)->lmHide)),
		(unsigned)(sizeof(int)*(n+1)))) == (int *)NULL)
	    return(0);
    }
    *(((trieBuf+cNode)->lmHide)+n) = n_ruleBuf;
    ((trieBuf+cNode)->lmNumRule)++;

    return(1);
}

/* check to see if two rules are the same modulo exception list */
static int
same_rule(
	  LmRule *r1,
	  LmRule *r2
	  )
{
    if (strcmp(charBuf+(r1->lmInSuf), charBuf+(r2->lmInSuf)) == 0 &&
	    r1->lmInCat == r2->lmInCat && r1->lmInInfl == r2->lmInInfl &&
	    r1->lmRule == r2->lmRule &&
	    strcmp(charBuf+(r1->lmOutSuf), charBuf+(r2->lmOutSuf)) == 0 &&
	    r1->lmOutCat == r2->lmOutCat && r1->lmOutInfl == r2->lmOutInfl)
	return(1);
    return(0);
}

/* generates a reverse rule */
static int
reverse_rule(ruleNum)
    int ruleNum;
{
    LmRule *rule;
    LmRule *revRule;

    if ((ruleBuf = (LmRule *) incr_buf_alloc((void *)ruleBuf,
					     sizeof(LmRule),
					     n_ruleBuf,
					     &a_ruleBuf,
					     (int)1,
					     (int)LM_DEFAULT_ALLOCATION)) == (LmRule *)NULL)
	return(0);

    rule = ruleBuf+ruleNum;
    revRule = ruleBuf+n_ruleBuf;

    revRule->lmInSuf = rule->lmOutSuf;
    revRule->lmInCat = rule->lmOutCat;
    revRule->lmInInfl = rule->lmOutInfl;
    revRule->lmRule = rule->lmRule;
    revRule->lmOutSuf = rule->lmInSuf;
    revRule->lmOutCat = rule->lmInCat;
    revRule->lmOutInfl = rule->lmInInfl;
    return(1);
}

/* rewrites the rules to be sequential */
static int
reorganize_rules()
{
    int i, j, k;
    int n;

    if ((ruleBuf2 = (LmRule *) malloc((unsigned)(sizeof(LmRule)*n_ruleBuf))) == (LmRule *)NULL)
	return(0);
    n = 0;
    for (i=0; i<n_trieBuf; i++)
    {
	(trieBuf+i)->lmRule = n;
	for (j=0; j<(trieBuf+i)->lmNumRule; j++)
	{
	    k = *(((trieBuf+i)->lmHide)+j);
	    (ruleBuf2+n)->lmInSuf = (ruleBuf+k)->lmInSuf;
	    (ruleBuf2+n)->lmInCat = (ruleBuf+k)->lmInCat;
	    (ruleBuf2+n)->lmInInfl = (ruleBuf+k)->lmInInfl;
	    (ruleBuf2+n)->lmRule = (ruleBuf+k)->lmRule;
	    (ruleBuf2+n)->lmOutSuf = (ruleBuf+k)->lmOutSuf;
	    (ruleBuf2+n)->lmOutCat = (ruleBuf+k)->lmOutCat;
	    (ruleBuf2+n)->lmOutInfl = (ruleBuf+k)->lmOutInfl;
	    (ruleBuf2+n)->lmFlags = (ruleBuf+k)->lmFlags;
	    n++;
	}
    }
    return(1);
}

bool_t
xdr_lm_t(XDR *xdrs, lm_t* val)
{
    return(xdr_u_int(xdrs, val));    
}

bool_t
xdr_lm_rule_header(XDR *xdrs, LmRuleHeader* ruleHeader)
{
/*   fprintf(stderr, "ruleHeader->lmMagic: %d\n", ruleHeader->lmMagic); */
/*   fprintf(stderr, "ruleHeader->lmTrie: %d\n", ruleHeader->lmTrie); */
/*   fprintf(stderr, "ruleHeader->lmRule: %d\n", ruleHeader->lmRule); */
/*   fprintf(stderr, "ruleHeader->lmChar: %d\n", ruleHeader->lmChar); */
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
	    xdr_int(xdrs, &trie->lmRule) &&
	    xdr_int(xdrs, (int *)&trie->lmHide));
}

bool_t
xdr_lm_rule(XDR *xdrs, LmRule* rule)
{
    return (xdr_int(xdrs, &rule->lmInSuf) &&
	    xdr_lm_t(xdrs, &rule->lmInCat) &&
	    xdr_lm_t(xdrs, &rule->lmInInfl) &&
	    xdr_lm_t(xdrs, &rule->lmRule) &&
	    xdr_int(xdrs, &rule->lmOutSuf) &&
	    xdr_lm_t(xdrs, &rule->lmOutCat) &&
	    xdr_lm_t(xdrs, &rule->lmOutInfl) &&
	    xdr_lm_t(xdrs, &rule->lmFlags));
}

/* writes the rules to standard output */
static int
write_lm_rules_xdr()
{
    XDR xdrs;			/* pointer to an XDR *stream* */
    LmRuleHeader head;

    int n;

    xdrstdio_create(&xdrs, stdout, XDR_ENCODE);

    head.lmMagic = LM_RULE_MAGIC;
    head.lmTrie = n_trieBuf;
    head.lmRule = n_ruleBuf;
    head.lmChar = n_charBuf;

    if (xdr_lm_rule_header(&xdrs, &head) == 0)
	 return(0);

    n = n_trieBuf;
    if (xdr_array(&xdrs, (caddr_t *)&trieBuf, (uint_t *)&n_trieBuf, (const uint_t)n, sizeof(LmTrie), (xdrproc_t)xdr_lm_trie) == 0)
	return(0);

    n = n_ruleBuf;
    if (xdr_array(&xdrs, (caddr_t *)&ruleBuf2, (uint_t *)&n_ruleBuf, (const uint_t)n, sizeof(LmRule), (xdrproc_t)xdr_lm_rule) == 0)
	return(0);

    n = n_charBuf;
    if (xdr_bytes(&xdrs, &charBuf, (uint_t *)&n_charBuf, (const uint_t)n) == 0)
	return(0);

    return(1);
}

/* maps category string to lm_t */
static int
get_cat(s, c)
    char *s;
    lm_t *c;
{
    if (strcmp(s, "adj") == 0)
	*c = LM_CAT_ADJ;
    else if (strcmp(s, "noun") == 0)
	*c = LM_CAT_NOUN;
    else if (strcmp(s, "verb") == 0)
	*c = LM_CAT_VERB;
    else
	return(0);
    return(1);
}

/* maps lm_t category to string */
static char *
cat_get(c)
    lm_t c;
{
    static char *s[] = {"adj", "noun", "verb", "unknown" };

    switch (c)
    {
	case LM_CAT_ADJ:
	    return(s[0]);
	case LM_CAT_NOUN:
	    return(s[1]);
	case LM_CAT_VERB:
	    return(s[2]);
	default:
	    return(s[3]);
    }
}

/* maps rule string to lm_t */
static int
get_rule(
	 char *s,
	 lm_t *c
	 )
{
    if (strcmp(s, "reg") == 0)
	*c = LM_RULE_REG;
    else if (strcmp(s, "regd") == 0)
	*c = LM_RULE_REGD;
    else if (strcmp(s, "glreg") == 0)
	*c = LM_RULE_GLREG;
    else
	return(0);
    return(1);
}

/* maps lm_t rule to string */
static char *
rule_get(
	 lm_t c
	 )
{
    static char *s[] = {"reg", "regd", "glreg", "unknown" };

    switch (c)
    {
	case LM_RULE_REG:
	    return(s[0]);
	case LM_RULE_REGD:
	    return(s[1]);
	case LM_RULE_GLREG:
	    return(s[2]);
	default:
	    return(s[3]);
    }
}

/* maps inflection string to internal macro */
static int
get_infl(
	 char *s,
	 lm_t *c
	 )
{
    if (strcmp(s, "base") == 0)
	*c = LM_INFL_BASE;
    else if (strcmp(s, "positive") == 0)
	*c = LM_INFL_POSITIVE;
    else if (strcmp(s, "singular") == 0)
	*c = LM_INFL_SINGULAR;
    else if (strcmp(s, "infinitive") == 0)
	*c = LM_INFL_INFINITIVE;
    else if (strcmp(s, "comparative") == 0)
	*c = LM_INFL_COMPARATIVE;
    else if (strcmp(s, "superlative") == 0)
	*c = LM_INFL_SUPERLATIVE;
    else if (strcmp(s, "plural") == 0)
	*c = LM_INFL_PLURAL;
    else if (strcmp(s, "present") == 0)
	*c = LM_INFL_PRESENT;
    else if (strcmp(s, "ing") == 0)
	*c = LM_INFL_ING;
    else if (strcmp(s, "past") == 0)
	*c = LM_INFL_PAST;
    else
	return(0);
    return(1);
}

/* maps internal macro to inflection string */
static char *
infl_get(
	 lm_t c
	 )
{
    static char *s[] = {"base", "comparative", "superlative", "plural",
	"present", "ing", "past", "unknown" };

    switch (c)
    {
	case LM_INFL_BASE:
	    return(s[0]);
	case LM_INFL_COMPARATIVE:
	    return(s[1]);
	case LM_INFL_SUPERLATIVE:
	    return(s[2]);
	case LM_INFL_PLURAL:
	    return(s[3]);
	case LM_INFL_PRESENT:
	    return(s[4]);
	case LM_INFL_ING:
	    return(s[5]);
	case LM_INFL_PAST:
	    return(s[6]);
	default:
	    return(s[7]);
    }
}
