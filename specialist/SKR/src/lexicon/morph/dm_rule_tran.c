
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

/* dm_rule_tran.c - creates a trie from rules in a file
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#include <unistd.h>
#include <errno.h>
#include "dm.h"
#include "lexicon_types.h"
#include <rpc/types.h>
#include <rpc/xdr.h>

/*  Translates (suffix) rules in the rules file to a trie.
    Prints error messages to stderr if the rules or exceptions lines
    have incorrect format, or the exceptions don't have the right
    suffixes.  Prints warnings for duplicate rules.

    Input structure is a file containing:

    InSuf|InCat|OutSuf|OutCat
	word1|word2; word3|word4;
	word5|word6; word7|word8;

    where the rule line contains six fields, and the optional
    exceptions line(s) contain exceptions to the rule as '|'
    separated pairs of words.  Each pair is terminated by a ';'.
    whitespace is tolerated in these lines.

    Comment lines (a la sh) and lines with whitespace are allowed
    in the input.

    Results are written to standard output.
*/

int *obsolete_get_fields(char *line, int sep, int *numF);
extern void *incr_buf_alloc(
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
static int same_rule(DmRule *r1, DmRule *r2);
static int same_exceptions(DmRule *r1, DmRule *r2);
static int correct_suffix(void);
static int duplicate_exceptions(void);
static int get_cat(char *s, dm_t *c);
static char *cat_get(dm_t c);
static void sort_exception_list(int start, int length);

bool_t xdr_dm_t(XDR *xdrs, dm_t* val);
bool_t xdr_dm_rule_header(XDR *xdrs, DmRuleHeader* ruleHeader);
bool_t xdr_dm_rule(XDR *xdrs, DmRule* rule);
bool_t xdr_dm_trie(XDR *xdrs, DmTrie* trie);
bool_t xdr_dm_xpn(XDR *xdrs, DmXpn* xpn);
static int write_dm_rules_xdr(void);

/* globals */
DmTrie *trieBuf = (DmTrie *)NULL;	/* in-memory trie */
int n_trieBuf = 0;			/* number of nodes in trie */
int a_trieBuf = 0;			/* allocated nodes in trie */

char *charBuf = (char *)NULL;	/* buffer containing character data, i.e., suffixes, exceptions */
int n_charBuf = 0;		/* actual size of buffer */
int a_charBuf = 0;		/* bytes allocated */

DmXpn *xpnBuf = (DmXpn *)NULL;	/* buffer for storing pointer to exceptions */
int n_xpnBuf = 0;		/* actual size of buffer */
int a_xpnBuf = 0;		/* allocated size */

DmRule *ruleBuf = (DmRule *)NULL;   /* buffer for storing the rules */
int n_ruleBuf = 0;		/* actual size of buffer */
int a_ruleBuf = 0;		/* allocated size */

DmRule *ruleBuf2 = (DmRule *)NULL;   /* buffer for storing the rules after reorganization */

static FILE *fp = NULL;

/* MAIN */
int main(
	 int	argc,
	 char	*argv[]
	 )
{
    fp = stdin;
    if (!read_rules())
	return(2);

    if (!reorganize_rules())
	return(2);

    if (!write_dm_rules_xdr())
	return(2);

    if (fclose(fp) == EOF)
	return(2);

    return(0);
}

/* reads in rules from file and creates in-memory trie */
static int
read_rules(void)
{
    static char s[] = "dm.rules";
    int lineno = 0;
    char *curFile = s;
    int i;
    int errFlag = 0;
    int startFlag = 0;
    DmRule *rule;
    char line[512];
    int *fields;
    int fieldNum;
    int n;

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
	if (*line == DM_COMMENT_CHAR)
	    continue;
	i=0;
	while (isspace((int)line[i]))
	    i++;

/* all whitespace? */
	if (line[i] == EOS)
	    continue;

/* leading whitespace? - read exceptions to current rule */
	if (isspace((int)line[0]))
	{
	    char *lp;
	    char *sp;		/* start of pair */
	    char *ep;		/* end of pair */
	    char *mp;		/* middle of pair */
	    int flag = 0;
	    DmXpn *xpn;

	    rule = ruleBuf+n_ruleBuf;

/* get exceptions list */
	    lp = line;
	    while (isspace((int)*lp))
		lp++;
	    for (sp=lp; !flag;)
	    {
		if ((ep=strchr(sp, DM_EXCEPTION_PAIR_TERMINATOR)) == (char *)NULL)
		{
		    fprintf(stderr, "ERROR: Exception pair not terminated by a '%c' in file: %s, line: %d.\n",
			    DM_EXCEPTION_PAIR_TERMINATOR, curFile, lineno);
		    errFlag++;
		    flag++;
		    continue;
		}
		if ((mp=strchr(sp, DM_EXCEPTION_PAIR_SEPARATOR)) == (char *)NULL)
		{
		    fprintf(stderr, "ERROR: Exception pair not separated by a '%c' in file: %s, line: %d.\n",
			    DM_EXCEPTION_PAIR_SEPARATOR, curFile, lineno);
		    errFlag++;
		    flag++;
		    continue;
		}
/* add pair */
		if ((xpnBuf = (DmXpn *) incr_buf_alloc((void *)xpnBuf,
						       sizeof(DmXpn),
						       n_xpnBuf,
						       &a_xpnBuf,
						       (int)1,
						       (int)DM_DEFAULT_ALLOCATION)) == (DmXpn *)NULL)
		    return(0);

/* save old value of n_charBuf in case of errors */
		n = n_charBuf;

		xpn = xpnBuf+n_xpnBuf;
		xpn->dmInTerm = n_charBuf;
		if ((charBuf = (char *) incr_buf_alloc((void *)charBuf,
						       sizeof(char),
						       n_charBuf,
						       &a_charBuf,
						       (int)(mp-sp+1),
						       (int)(DM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
		    return(0);
		strncpy(charBuf+n_charBuf, sp, (size_t)(mp-sp));
		*(charBuf+n_charBuf+(int)(mp-sp)) = EOS;
		n_charBuf += (int)(mp-sp)+1;

		xpn->dmOutTerm = n_charBuf;
		if ((charBuf = (char *) incr_buf_alloc((void *)charBuf,
						       sizeof(char),
						       n_charBuf,
						       &a_charBuf,
						       (int)(ep-mp),
						       (int)(DM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
		    return(0);
		strncpy(charBuf+n_charBuf, mp+1, (size_t)(ep-mp)-1);
		*(charBuf+n_charBuf+(int)(ep-mp)-1) = EOS;
		n_charBuf += (int)(ep-mp);

		if (duplicate_exceptions())
		{
		    fprintf(stderr, "WARNING: Duplicates in exception list ignored: %s|%s\n",
			&charBuf[xpn->dmInTerm], &charBuf[xpn->dmOutTerm]);
		    n_charBuf = n;
		}
		else if (!correct_suffix())
		{
		    fprintf(stderr, "ERROR: Exception pair (%s/%s) in file: %s, line: %d: incorrect suffix.\n",
			    &charBuf[xpn->dmInTerm], &charBuf[xpn->dmOutTerm], curFile, lineno);
		    n_charBuf = n;
		    errFlag++;
		    flag++;
		    continue;
		}
		else
		{
		    n_xpnBuf++;
		    (rule->dmNumXpn)++;
		}
		sp = ep+1;
		while (isspace((int)*sp))
		    sp++;
		if (*sp == EOS)
		    break;
	    }	    
	    if (flag)
		continue;
	}
	else
	{
	    char buf[64];
	    char *start;
	    char *sp;
	    int length;
	    int l;

/* load old (with its reverse) and start new rule */
	    if (startFlag)
	    {
		if (!load_rule())
		    return(0);
		n_ruleBuf++;
		if ((ruleBuf = (DmRule *) incr_buf_alloc((void *)ruleBuf,
							 sizeof(DmRule),
							 n_ruleBuf,
							 &a_ruleBuf,
							 (int)1,
							 (int)DM_DEFAULT_ALLOCATION)) == (DmRule *)NULL)
		    return(0);
		if (!reverse_rule(n_ruleBuf-1))
		    return(0);
		if (!load_rule())
		    return(0);
		n_ruleBuf++;
		if ((ruleBuf = (DmRule *) incr_buf_alloc((void *)ruleBuf,
							 sizeof(DmRule),
							 n_ruleBuf,
							 &a_ruleBuf,
							 (int)1,
							 (int)DM_DEFAULT_ALLOCATION)) == (DmRule *)NULL)
		    return(0);
		startFlag = 0;
	    }
	    if ((ruleBuf = (DmRule *) incr_buf_alloc((void *)ruleBuf,
						     sizeof(DmRule),
						     n_ruleBuf,
						     &a_ruleBuf,
						     (int)1,
						     (int)DM_DEFAULT_ALLOCATION)) == (DmRule *)NULL)
		return(0);
	    rule = ruleBuf+n_ruleBuf;
	    rule->dmInSuf = rule->dmOutSuf = rule->dmXpn = (-1);
	    rule->dmNumXpn = 0;
	    rule->dmFlags = 0;
	    rule->dmXpn = n_xpnBuf;

	    if ((fields=obsolete_get_fields(line, DM_FIELD_SEPARATOR_CHAR, &n)) == (int *)NULL)
	    {
		fprintf(stderr, "ERROR: Cannot break rule in file: %s, line: %d into fields!\n",
			curFile, lineno);
		errFlag++;
		continue;
	    }
	    if (n != 4)
	    {
		fprintf(stderr, "ERROR: Expecting 4 fields in file: %s, line: %d, found: %d!\n",
			curFile, lineno, n);
		errFlag++;
		continue;
	    }

/* input suffix */
	    fieldNum = 0;
	    start = sp = &line[*(fields+2*fieldNum)];
	    length = l = *(fields+2*fieldNum+1);
	    if (*sp == DM_SOP_CHAR)
	    {
		rule->dmFlags |= DM_INPUT_SOP;
		start = sp+1;
		length--;
	    }
	    if (l>0 && *(sp+l-1) == DM_EOP_CHAR)
	    {
		rule->dmFlags |= DM_INPUT_EOP;
		length--;
	    }
	    if ((charBuf = (char *) incr_buf_alloc((void *)charBuf,
						   sizeof(char),
						   n_charBuf,
						   &a_charBuf,
						   (int)(length+1),
						   (int)(DM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
		return(0);
	    strncpy(charBuf+n_charBuf, start, (size_t)length);
	    *(charBuf+n_charBuf+length) = EOS;
	    rule->dmInSuf = n_charBuf;
	    n_charBuf += length+1;

/* input category */
	    fieldNum++;
	    start = &line[*(fields+2*fieldNum)];
	    length = *(fields+2*fieldNum+1);
	    strncpy(buf, start, (size_t)length);
	    *(buf+length) = EOS;
	    if (!get_cat(buf, &(rule->dmInCat)))
	    {
		fprintf(stderr, "ERROR: Input category incorrect in file: %s, line: %d\n", curFile, lineno);
		errFlag++;
		continue;
	    }

/* output suffix */
	    fieldNum++;
	    start = sp = &line[*(fields+2*fieldNum)];
	    length = l = *(fields+2*fieldNum+1);
	    if (*start == DM_SOP_CHAR)
	    {
		rule->dmFlags |= DM_OUTPUT_SOP;
		start = sp+1;
		length--;
	    }
	    if (l>0 && *(sp+l-1) == DM_EOP_CHAR)
	    {
		rule->dmFlags |= DM_OUTPUT_EOP;
		length--;
	    }
	    if ((charBuf = (char *) incr_buf_alloc((void *)charBuf,
						   sizeof(char),
						   n_charBuf,
						   &a_charBuf,
						   (int)(length+1),
						   (int)(DM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
		return(0);
	    strncpy(charBuf+n_charBuf, start, (size_t)length);
	    *(charBuf+n_charBuf+length) = EOS;
	    rule->dmOutSuf = n_charBuf;
	    n_charBuf += length+1;

/* output category */
	    fieldNum++;
	    start = &line[*(fields+2*fieldNum)];
	    length = *(fields+2*fieldNum+1);
	    strncpy(buf, start, (size_t)length);
	    *(buf+length) = EOS;
	    if (!get_cat(buf, &(rule->dmOutCat)))
	    {
		fprintf(stderr, "ERROR: Output category incorrect in file: %s, line: %d\n", curFile, lineno);
		errFlag++;
		continue;
	    }
	    startFlag = 1;
	}
    }
    if (startFlag)
    {
	if (!load_rule())
	    return(2);
	n_ruleBuf++;
	if ((ruleBuf = (DmRule *) incr_buf_alloc((void *)ruleBuf,
						 sizeof(DmRule),
						 n_ruleBuf,
						 &a_ruleBuf,
						 (int)1,
						 (int)DM_DEFAULT_ALLOCATION)) == (DmRule *)NULL)
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
load_rule(void)
{
    int cNode;		/* current node */
    int pNode = 0;	/* parent node */
    int rNode = 0;	/* previous node */
    int new = 0;
    char *cp;
    DmRule *rule = ruleBuf+n_ruleBuf;
    int n;

    cNode = pNode;

    if (n_trieBuf == 0)
    {
	if ((trieBuf = (DmTrie *) incr_buf_alloc((void *)trieBuf,
						 sizeof(DmTrie),
						 n_trieBuf,
						 &a_trieBuf,
						 (int)1,
						 (int)(DM_DEFAULT_ALLOCATION*2))) == (DmTrie *)NULL)
	    return(0);
	trieBuf->dmData = (int)EOS;
	trieBuf->dmChild = trieBuf->dmSib = (-1);
	trieBuf->dmNumRule = 0;
	trieBuf->dmHide = (int *)NULL;
	n_trieBuf++;
    }

/* find trie node for insertion */
    for (cp = &charBuf[rule->dmInSuf] + strlen(&charBuf[rule->dmInSuf]); cp >= &charBuf[rule->dmInSuf]; cp--)
    {
	if (cNode == (-1) || ((int)(*cp) < (trieBuf+cNode)->dmData))
	{
	    if ((trieBuf = (DmTrie *) incr_buf_alloc((void *)trieBuf,
						     sizeof(DmTrie),
						     n_trieBuf,
						     &a_trieBuf,
						     (int)1,
						     (int)(DM_DEFAULT_ALLOCATION*2))) == (DmTrie *)NULL)
		return(0);
	    new = n_trieBuf;
	    (trieBuf+new)->dmData = (int)(*cp);
	    (trieBuf+new)->dmChild = (-1);
	    (trieBuf+new)->dmSib = cNode;
	    (trieBuf+new)->dmNumRule = 0;
	    (trieBuf+new)->dmHide = (int *)NULL;
	    (trieBuf+pNode)->dmChild = new;
	    n_trieBuf++;
	}
	else
	{
	    while (cNode != (-1) && (trieBuf+cNode)->dmData < (int)(*cp))
	    {
		rNode = cNode;
		cNode = (trieBuf+cNode)->dmSib;
	    }
	    if (cNode == (-1) || (trieBuf+cNode)->dmData != (int)(*cp))
	    {
		if ((trieBuf = (DmTrie *) incr_buf_alloc((void *)trieBuf,
							 sizeof(DmTrie),
							 n_trieBuf,
							 &a_trieBuf,
							 (int)1,
							 (int)(DM_DEFAULT_ALLOCATION*2))) == (DmTrie *)NULL)
		    return(0);
		new = n_trieBuf;
		(trieBuf+new)->dmData = (int)(*cp);
		(trieBuf+new)->dmChild = (-1);
		(trieBuf+new)->dmSib = cNode;
		(trieBuf+new)->dmNumRule = 0;
		(trieBuf+new)->dmHide = (int *)NULL;
		(trieBuf+rNode)->dmSib = new;
		n_trieBuf++;
	    }
	    else
	    {
		new = cNode;
	    }
	}
	pNode = rNode = new;
	cNode = (trieBuf+pNode)->dmChild;
    }

    cNode = new;
    rule = ruleBuf+n_ruleBuf;

/* check for duplicates */
    for (n=0; n<(trieBuf+cNode)->dmNumRule; n++)
    {
	if (same_rule(rule, ruleBuf + *(((trieBuf+cNode)->dmHide)+n)))
	{
	    if (same_exceptions(rule, ruleBuf + *(((trieBuf+cNode)->dmHide)+n)))
	    {
		return(1);
	    }
	    else
	    {
		fprintf(stderr, "ERROR: There were multiple rules of the type: %s|%s|%s|%s\n",
			charBuf+rule->dmInSuf, cat_get(rule->dmInCat),
			charBuf+rule->dmOutSuf, cat_get(rule->dmOutCat));
		fprintf(stderr, "\twith different exception lists\n");
		return(0);
	    }
	}
    }

/* add rule */
    n = (trieBuf+cNode)->dmNumRule;
    if (n == 0)
    {
	if ((((trieBuf+cNode)->dmHide) = (int *) malloc((unsigned)(sizeof(int)*(n+1)))) == (int *)NULL)
	    return(0);
    }
    else
    {
	if ((((trieBuf+cNode)->dmHide) = (int *) realloc((char *)(((trieBuf+cNode)->dmHide)),
		(unsigned)(sizeof(int)*(n+1)))) == (int *)NULL)
	    return(0);
    }
    *(((trieBuf+cNode)->dmHide)+n) = n_ruleBuf;
    ((trieBuf+cNode)->dmNumRule)++;

    return(1);
}

/* checks to see if the exception terms have the correct suffix */
static int
correct_suffix(void)
{
    static char buf[256];
    DmRule *rule = ruleBuf+n_ruleBuf;
    DmXpn *xpn = xpnBuf+n_xpnBuf;
    char *inTerm, *outTerm;
    char *inSuf, *outSuf;

    inTerm = &charBuf[xpn->dmInTerm];
    outTerm = &charBuf[xpn->dmOutTerm];
    inSuf = &charBuf[rule->dmInSuf];
    outSuf = &charBuf[rule->dmOutSuf];

    if (strlen(inTerm) >= strlen(inSuf) && (strcmp(inSuf, inTerm+strlen(inTerm)-strlen(inSuf)) != 0))
	return(0);
    if (strlen(outTerm) >= strlen(outSuf) && (strcmp(outSuf, outTerm+strlen(outTerm)-strlen(outSuf)) != 0))
	return(0);
    strcpy(buf, inTerm);
    strcpy(&buf[strlen(buf)-strlen(inSuf)], outSuf);
    return((strcasecmp(buf, outTerm) == 0) ? 1 : 0);
}

/* check to see if two rules are the same modulo exception list */
static int
same_rule(
	  DmRule *r1,
	  DmRule *r2
	  )
{
    if (strcmp(charBuf+(r1->dmInSuf), charBuf+(r2->dmInSuf)) == 0 &&
	    r1->dmInCat == r2->dmInCat &&
	    strcmp(charBuf+(r1->dmOutSuf), charBuf+(r2->dmOutSuf)) == 0 &&
	    r1->dmOutCat == r2->dmOutCat)
	return(1);
    return(0);
}

/* check to see if two rules have the same exception list */
static int
same_exceptions(
		DmRule *r1,
		DmRule *r2
		)
{
    int i;
    DmXpn *xpn1;
    DmXpn *xpn2;

    if (r1->dmNumXpn != r2->dmNumXpn)
	return(0);
	
    for (i=0; i<r1->dmNumXpn; i++)
    {
	xpn1 = xpnBuf+(r1->dmXpn)+i;
	xpn2 = xpnBuf+(r2->dmXpn)+i;
	if (strcmp(charBuf+(xpn1->dmInTerm), charBuf+(xpn2->dmInTerm)) != 0 ||
		strcmp(charBuf+(xpn1->dmOutTerm), charBuf+(xpn2->dmOutTerm)) != 0)
	    return(0);
    }
    return(1);
}

/* check to see if a rule already has a given exception pair */
static int
duplicate_exceptions(void)
{
    int i;
    DmXpn *xpn = xpnBuf+n_xpnBuf;
    DmXpn *lxpn;
    DmRule *rule = ruleBuf+n_ruleBuf;

    for (i=0; i<rule->dmNumXpn; i++)
    {
	lxpn = xpnBuf+rule->dmXpn+i;
	if (strcmp(&charBuf[xpn->dmInTerm], &charBuf[lxpn->dmInTerm]) == 0 &&
		strcmp(&charBuf[xpn->dmOutTerm], &charBuf[lxpn->dmOutTerm]) == 0)
	    return(1);
    }
    return(0);
}

/* generates a reverse rule */
static int
reverse_rule(
	     int ruleNum
	     )
{
    int i;
    DmRule *rule;
    DmRule *revRule;
    DmXpn *xpn;

    if ((ruleBuf = (DmRule *) incr_buf_alloc((void *)ruleBuf,
					     sizeof(DmRule),
					     n_ruleBuf,
					     &a_ruleBuf,
					     (int)1,
					     (int)DM_DEFAULT_ALLOCATION)) == (DmRule *)NULL)
	return(0);

    rule = ruleBuf+ruleNum;
    revRule = ruleBuf+n_ruleBuf;

    revRule->dmInSuf = rule->dmOutSuf;
    revRule->dmInCat = rule->dmOutCat;
    revRule->dmOutSuf = rule->dmInSuf;
    revRule->dmOutCat = rule->dmInCat;
    revRule->dmNumXpn = rule->dmNumXpn;
    revRule->dmFlags  = rule->dmFlags;
    revRule->dmXpn = n_xpnBuf;

    for (i=0; i<rule->dmNumXpn; i++)
    {
	if ((xpnBuf = (DmXpn *) incr_buf_alloc((void *)xpnBuf,
					       sizeof(DmXpn),
					       n_xpnBuf,
					       &a_xpnBuf,
					       (int)1,
					       (int)DM_DEFAULT_ALLOCATION)) == (DmXpn *)NULL)
	    return(0);
	xpn = xpnBuf+(rule->dmXpn)+i;
	(xpnBuf+n_xpnBuf)->dmInTerm = xpn->dmOutTerm;
	(xpnBuf+n_xpnBuf)->dmOutTerm = xpn->dmInTerm;
	n_xpnBuf++;
    }
    return(1);
}

/* rewrites the rules to be sequential while sorting the exceptions */
static int
reorganize_rules(void)
{
    int i, j, k;
    int n;

    if ((ruleBuf2 = (DmRule *) malloc((unsigned)(sizeof(DmRule)*n_ruleBuf))) == (DmRule *)NULL)
	return(0);
    n = 0;
    for (i=0; i<n_trieBuf; i++)
    {
	(trieBuf+i)->dmRule = n;
	for (j=0; j<(trieBuf+i)->dmNumRule; j++)
	{
	    k = *(((trieBuf+i)->dmHide)+j);
	    (ruleBuf2+n)->dmInSuf = (ruleBuf+k)->dmInSuf;
	    (ruleBuf2+n)->dmInCat = (ruleBuf+k)->dmInCat;
	    (ruleBuf2+n)->dmOutSuf = (ruleBuf+k)->dmOutSuf;
	    (ruleBuf2+n)->dmOutCat = (ruleBuf+k)->dmOutCat;
	    (ruleBuf2+n)->dmFlags = (ruleBuf+k)->dmFlags;
	    (ruleBuf2+n)->dmNumXpn = (ruleBuf+k)->dmNumXpn;
	    (ruleBuf2+n)->dmXpn = (ruleBuf+k)->dmXpn;
	    sort_exception_list((ruleBuf2+n)->dmXpn, (ruleBuf2+n)->dmNumXpn);
	    n++;
	}
    }
    return(1);
}

bool_t
xdr_dm_t(XDR *xdrs, dm_t* val)
{
    return(xdr_u_int(xdrs, val));    
}

bool_t
xdr_dm_rule_header(XDR *xdrs, DmRuleHeader* ruleHeader)
{
    return(xdr_int(xdrs, &ruleHeader->dmMagic) &&
	   xdr_int(xdrs, &ruleHeader->dmTrie) &&
	   xdr_int(xdrs, &ruleHeader->dmRule) &&
	   xdr_int(xdrs, &ruleHeader->dmXpn) &&
	   xdr_int(xdrs, &ruleHeader->dmChar));
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
    return (xdr_int(xdrs, &trie->dmData) &&
	    xdr_dm_t(xdrs, (dm_t *)(&trie->dmChild)) &&
	    xdr_dm_t(xdrs, (dm_t *)(&trie->dmSib)) &&
	    xdr_dm_t(xdrs, (dm_t *)(&trie->dmNumRule)) &&
	    xdr_dm_t(xdrs, (dm_t *)(&trie->dmRule)) &&
	    xdr_int(xdrs,  (int *)&trie->dmHide));
}

bool_t
xdr_dm_xpn(XDR *xdrs, DmXpn* xpn)
{
    return (xdr_int(xdrs, &xpn->dmInTerm) &&
	    xdr_dm_t(xdrs, (dm_t *)(&xpn->dmOutTerm)));
}

/* writes the rules in xdr format to standard output */
static int
write_dm_rules_xdr(void)
{
    XDR xdrs;			/* pointer to an XDR *stream* */
    int n;
    DmRuleHeader head;

    xdrstdio_create(&xdrs, stdout, XDR_ENCODE);

    head.dmMagic = DM_RULE_MAGIC;
    head.dmTrie = n_trieBuf;
    head.dmRule = n_ruleBuf;
    head.dmXpn = n_xpnBuf;
    head.dmChar = n_charBuf;

    if (xdr_dm_rule_header(&xdrs, &head) == 0) 
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
		  (caddr_t *)&ruleBuf2,
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

    return(1);
}

/* maps category string to dm_t */
static int
get_cat(
	char *s,
	dm_t *c
	)
{
    if (strcmp(s, "adj") == 0)
	*c = DM_CAT_ADJ;
    else if (strcmp(s, "adv") == 0)
	*c = DM_CAT_ADV;
    else if (strcmp(s, "aux") == 0)
	*c = DM_CAT_AUX;
    else if (strcmp(s, "compl") == 0)
	*c = DM_CAT_COMPL;
    else if (strcmp(s, "conj") == 0)
	*c = DM_CAT_CONJ;
    else if (strcmp(s, "det") == 0)
	*c = DM_CAT_DET;
    else if (strcmp(s, "modal") == 0)
	*c = DM_CAT_MODAL;
    else if (strcmp(s, "noun") == 0)
	*c = DM_CAT_NOUN;
    else if (strcmp(s, "prep") == 0)
	*c = DM_CAT_PREP;
    else if (strcmp(s, "pron") == 0)
	*c = DM_CAT_PRON;
    else if (strcmp(s, "verb") == 0)
	*c = DM_CAT_VERB;
    else
	return(0);
    return(1);
}

/* maps dm_t category to string */
static char *
cat_get(
	dm_t c
	)
{
    static char *s[] = {"adj", "adv", "aux", "compl", "conj", "det",
	    "modal", "noun", "prep", "pron", "verb", "unknown" };

    switch (c)
    {
	case DM_CAT_ADJ:
	    return(s[0]);
	case DM_CAT_ADV:
	    return(s[1]);
	case DM_CAT_AUX:
	    return(s[2]);
	case DM_CAT_COMPL:
	    return(s[3]);
	case DM_CAT_CONJ:
	    return(s[4]);
	case DM_CAT_DET:
	    return(s[5]);
	case DM_CAT_MODAL:
	    return(s[6]);
	case DM_CAT_NOUN:
	    return(s[7]);
	case DM_CAT_PREP:
	    return(s[8]);
	case DM_CAT_PRON:
	    return(s[9]);
	case DM_CAT_VERB:
	    return(s[10]);
	default:
	    return(s[11]);
    }
}

/* (shell) sorts the exceptions list (K&R, ed 1, pg 116) */
static void
sort_exception_list(
		    int start,			/* where the exceptions start */
		    int length			/* how many */
		    )
{
    int gap, i, j;
    int ofs1, ofs2;

    for (gap = length/2; gap>0; gap /= 2)
    {
	for (i=gap; i<length; i++)
	{
	    for (j=i-gap; j>=0; j-=gap)
	    {
		ofs1 = ((xpnBuf+j+start)->dmInTerm);
		ofs2 = ((xpnBuf+j+gap+start)->dmInTerm);
		if (strcmp(&charBuf[ofs1], &charBuf[ofs2]) <= 0)
		    break;
		((xpnBuf+j+start)->dmInTerm) = ofs2;
		((xpnBuf+j+gap+start)->dmInTerm) = ofs1;
	    }
	}
    }
}
