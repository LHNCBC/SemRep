/* im_rule_tran.c - creates a trie from rules in a file
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#include <errno.h>
#include "im.h"

static char sccs_id_im_rule_tran_c[] = "@(#)im_rule_tran.c	1.2	09/27/06";

/*  Translates (suffix) rules in the rules file to a trie.
    Prints error messages to stderr if the rules or exceptions lines
    have incorrect format, or the exceptions don't have the right
    suffixes.  Prints warnings for duplicate rules.

    Input structure is a file containing:

    InSuf|InCat|InInfl|OutSuf|OutCat|OutInfl
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

extern int *obsolete_get_fields();	/* in field.c */
extern void *incr_buf_alloc();	/* in util.c */
extern int push_loc();		/* in loc.c */
extern int pop_loc();
static int read_rules();
static int reorganize_rules();
static int write_rules();
static int load_rule();
static int reverse_rule();
static int same_rule();
static int same_exceptions();
static int correct_suffix();
static int duplicate_exceptions();
static int get_cat();
static char *cat_get();
static int get_infl();
static char *infl_get();
static void sort_exception_list();

/* globals */
ImTrie *trieBuf = (ImTrie *)NULL;	/* in-memory trie */
int n_trieBuf = 0;			/* number of nodes in trie */
int a_trieBuf = 0;			/* allocated nodes in trie */

char *charBuf = (char *)NULL;	/* buffer containing character data, i.e., suffixes, exceptions */
int n_charBuf = 0;		/* actual size of buffer */
int a_charBuf = 0;		/* bytes allocated */

ImXpn *xpnBuf = (ImXpn *)NULL;	/* buffer for storing pointer to exceptions */
int n_xpnBuf = 0;		/* actual size of buffer */
int a_xpnBuf = 0;		/* allocated size */

ImRule *ruleBuf = (ImRule *)NULL;   /* buffer for storing the rules */
int n_ruleBuf = 0;		/* actual size of buffer */
int a_ruleBuf = 0;		/* allocated size */

ImRule *ruleBuf2 = (ImRule *)NULL;   /* buffer for storing the rules after reorganization */

static FILE *fp=NULL;

/* MAIN */
main(argc, argv)
int	argc;
char	*argv[];
{
    fp=stdin;
    if (!read_rules())
	return(2);

    if (!reorganize_rules())
	return(2);

    if (!write_rules())
	return(2);

    if (fclose(fp) == EOF)
	return(2);

    return(0);
}

/* reads in rules from file and creates in-memory trie */
static int
read_rules()
{
    static char s[] = "im.rules";
    int lineno = 0;
    char *curFile = s;
    int i;
    int errFlag = 0;
    int startFlag = 0;
    ImRule *rule;
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
	if (*line == IM_COMMENT_CHAR)
	    continue;

	i=0;
	while (isspace(line[i]))
	    i++;

/* all whitespace? */
	if (line[i] == EOS)
	    continue;

/* leading whitespace? - read exceptions to current rule */
	if (isspace(line[0]))
	{
	    char *lp;
	    char *sp;		/* start of pair */
	    char *ep;		/* end of pair */
	    char *mp;		/* middle of pair */
	    int flag = 0;
	    ImXpn *xpn;

	    rule = ruleBuf+n_ruleBuf;

/* get exceptions list */
	    lp = line;
	    while (isspace(*lp))
		lp++;
	    for (sp=lp; !flag;)
	    {
		if ((ep=strchr(sp, IM_EXCEPTION_PAIR_TERMINATOR)) == (char *)NULL)
		{
		    fprintf(stderr, "ERROR: Exception pair not terminated by a '%c' in file: %s, line: %d.\n",
			    IM_EXCEPTION_PAIR_TERMINATOR, curFile, lineno);
		    errFlag++;
		    flag++;
		    continue;
		}
		if ((mp=strchr(sp, IM_EXCEPTION_PAIR_SEPARATOR)) == (char *)NULL)
		{
		    fprintf(stderr, "ERROR: Exception pair not separated by a '%c' in file: %s, line: %d.\n",
			    IM_EXCEPTION_PAIR_SEPARATOR, curFile, lineno);
		    errFlag++;
		    flag++;
		    continue;
		}
/* add pair */
		if ((xpnBuf = (ImXpn *) incr_buf_alloc((void *)xpnBuf, sizeof(ImXpn), n_xpnBuf,
			&a_xpnBuf, (int)1, (int)IM_DEFAULT_ALLOCATION)) == (ImXpn *)NULL)
		    return(0);

/* save old value of n_charBuf in case of errors */
		n = n_charBuf;

		xpn = xpnBuf+n_xpnBuf;
		xpn->imInTerm = n_charBuf;
		if ((charBuf = (char *) incr_buf_alloc((void *)charBuf, sizeof(char), n_charBuf,
			&a_charBuf, (int)(mp-sp+1), (int)(IM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
		    return(0);
		strncpy(charBuf+n_charBuf, sp, mp-sp);
		*(charBuf+n_charBuf+(int)(mp-sp)) = EOS;
		n_charBuf += (int)(mp-sp)+1;

		xpn->imOutTerm = n_charBuf;
		if ((charBuf = (char *) incr_buf_alloc((void *)charBuf, sizeof(char), n_charBuf,
			&a_charBuf, (int)(ep-mp), (int)(IM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
		    return(0);
		strncpy(charBuf+n_charBuf, mp+1, (int)(ep-mp)-1);
		*(charBuf+n_charBuf+(int)(ep-mp)-1) = EOS;
		n_charBuf += (int)(ep-mp);

		if (duplicate_exceptions())
		{
		    fprintf(stderr, "WARNING: Duplicates in exception list ignored: %s|%s\n",
			&charBuf[xpn->imInTerm], &charBuf[xpn->imOutTerm]);
		    n_charBuf = n;
		}
		else if (!correct_suffix())
		{
		    fprintf(stderr, "ERROR: Exception pair (%s/%s) in file: %s, line: %d: incorrect suffix.\n",
			    &charBuf[xpn->imInTerm], &charBuf[xpn->imOutTerm], curFile, lineno);
		    n_charBuf = n;
		    errFlag++;
		    flag++;
		    continue;
		}
		else
		{
		    n_xpnBuf++;
		    (rule->imNumXpn)++;
		}
		sp = ep+1;
		while (isspace(*sp))
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
		if ((ruleBuf = (ImRule *) incr_buf_alloc((void *)ruleBuf, sizeof(ImRule), n_ruleBuf,
			&a_ruleBuf, (int)1, (int)IM_DEFAULT_ALLOCATION)) == (ImRule *)NULL)
		    return(0);
		if (!reverse_rule(n_ruleBuf-1))
		    return(0);
		if (!load_rule())
		    return(0);
		n_ruleBuf++;
		if ((ruleBuf = (ImRule *) incr_buf_alloc((void *)ruleBuf, sizeof(ImRule), n_ruleBuf,
			&a_ruleBuf, (int)1, (int)IM_DEFAULT_ALLOCATION)) == (ImRule *)NULL)
		    return(0);
		startFlag = 0;
	    }
	    if ((ruleBuf = (ImRule *) incr_buf_alloc((void *)ruleBuf, sizeof(ImRule), n_ruleBuf,
		    &a_ruleBuf, (int)1, (int)IM_DEFAULT_ALLOCATION)) == (ImRule *)NULL)
		return(0);
	    rule = ruleBuf+n_ruleBuf;
	    rule->imInSuf = rule->imOutSuf = rule->imXpn = (-1);
	    rule->imNumXpn = 0;
	    rule->imFlags = 0;
	    rule->imXpn = n_xpnBuf;

	    if ((fields=obsolete_get_fields(line, IM_FIELD_SEPARATOR_CHAR, &n)) == (int *)NULL)
	    {
		fprintf(stderr, "ERROR: Cannot break rule in file: %s, line: %d into fields!\n",
			curFile, lineno);
		errFlag++;
		continue;
	    }
	    if (n != 6)
	    {
		fprintf(stderr, "ERROR: Expecting 6 fields in file: %s, line: %d, found: %d!\n",
			curFile, lineno, n);
		errFlag++;
		continue;
	    }

/* input suffix */
	    fieldNum = 0;
	    start = sp = &line[*(fields+2*fieldNum)];
	    length = l = *(fields+2*fieldNum+1);
	    if (*sp == IM_SOP_CHAR)
	    {
		rule->imFlags |= IM_INPUT_SOP;
		start = sp+1;
		length--;
	    }
	    if (l>0 && *(sp+l-1) == IM_EOP_CHAR)
	    {
		rule->imFlags |= IM_INPUT_EOP;
		length--;
	    }
	    if ((charBuf = (char *) incr_buf_alloc((void *)charBuf, sizeof(char), n_charBuf,
		    &a_charBuf, (int)(length+1), (int)(IM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
		return(0);
	    strncpy(charBuf+n_charBuf, start, length);
	    *(charBuf+n_charBuf+length) = EOS;
	    rule->imInSuf = n_charBuf;
	    n_charBuf += length+1;

/* input category */
	    fieldNum++;
	    start = &line[*(fields+2*fieldNum)];
	    length = *(fields+2*fieldNum+1);
	    strncpy(buf, start, length);
	    *(buf+length) = EOS;
	    if (!get_cat(buf, &(rule->imInCat)))
	    {
		fprintf(stderr, "ERROR: Input category incorrect in file: %s, line: %d\n", curFile, lineno);
		errFlag++;
		continue;
	    }

/* input inflection */
	    fieldNum++;
	    start = &line[*(fields+2*fieldNum)];
	    length = *(fields+2*fieldNum+1);
	    strncpy(buf, start, length);
	    *(buf+length) = EOS;
	    if (!get_infl(buf, &(rule->imInInfl)))
	    {
		fprintf(stderr, "ERROR: Input inflection incorrect in file: %s, line: %d\n", curFile, lineno);
		errFlag++;
		continue;
	    }

/* output suffix */
	    fieldNum++;
	    start = sp = &line[*(fields+2*fieldNum)];
	    length = l = *(fields+2*fieldNum+1);
	    if (*start == IM_SOP_CHAR)
	    {
		rule->imFlags |= IM_OUTPUT_SOP;
		start = sp+1;
		length--;
	    }
	    if (l>0 && *(sp+l-1) == IM_EOP_CHAR)
	    {
		rule->imFlags |= IM_OUTPUT_EOP;
		length--;
	    }
	    if ((charBuf = (char *) incr_buf_alloc((void *)charBuf, sizeof(char), n_charBuf,
		    &a_charBuf, (int)(length+1), (int)(IM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
		return(0);
	    strncpy(charBuf+n_charBuf, start, length);
	    *(charBuf+n_charBuf+length) = EOS;
	    rule->imOutSuf = n_charBuf;
	    n_charBuf += length+1;

/* output category */
	    fieldNum++;
	    start = &line[*(fields+2*fieldNum)];
	    length = *(fields+2*fieldNum+1);
	    strncpy(buf, start, length);
	    *(buf+length) = EOS;
	    if (!get_cat(buf, &(rule->imOutCat)))
	    {
		fprintf(stderr, "ERROR: Output category incorrect in file: %s, line: %d\n", curFile, lineno);
		errFlag++;
		continue;
	    }

/* output inflection */
	    fieldNum++;
	    start = &line[*(fields+2*fieldNum)];
	    length = *(fields+2*fieldNum+1);
	    strncpy(buf, start, length);
	    *(buf+length) = EOS;
	    if (!get_infl(buf, &(rule->imOutInfl)))
	    {
		fprintf(stderr, "ERROR: Output inflection incorrect in file: %s, line: %d\n", curFile, lineno);
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
	if ((ruleBuf = (ImRule *) incr_buf_alloc((void *)ruleBuf, sizeof(ImRule), n_ruleBuf,
		&a_ruleBuf, (int)1, (int)IM_DEFAULT_ALLOCATION)) == (ImRule *)NULL)
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
    int new;
    char *cp;
    ImRule *rule = ruleBuf+n_ruleBuf;
    int n;

    cNode = pNode;

    if (n_trieBuf == 0)
    {
	if ((trieBuf = (ImTrie *) incr_buf_alloc((void *)trieBuf, sizeof(ImTrie), n_trieBuf,
		&a_trieBuf, (int)1, (int)(IM_DEFAULT_ALLOCATION*2))) == (ImTrie *)NULL)
	    return(0);
	trieBuf->imData = (int)EOS;
	trieBuf->imChild = trieBuf->imSib = (-1);
	trieBuf->imNumRule = 0;
	trieBuf->imHide = (int *)NULL;
	n_trieBuf++;
    }

/* find trie node for insertion */
    for (cp = &charBuf[rule->imInSuf] + strlen(&charBuf[rule->imInSuf]); cp >= &charBuf[rule->imInSuf]; cp--)
    {
	if (cNode == (-1) || ((int)(*cp) < (trieBuf+cNode)->imData))
	{
	    if ((trieBuf = (ImTrie *) incr_buf_alloc((void *)trieBuf, sizeof(ImTrie), n_trieBuf,
		    &a_trieBuf, (int)1, (int)(IM_DEFAULT_ALLOCATION*2))) == (ImTrie *)NULL)
		return(0);

	    new = n_trieBuf;
	    (trieBuf+new)->imData = (int)(*cp);
	    (trieBuf+new)->imChild = (-1);
	    (trieBuf+new)->imSib = cNode;
	    (trieBuf+new)->imNumRule = 0;
	    (trieBuf+new)->imHide = (int *)NULL;
	    (trieBuf+pNode)->imChild = new;
	    n_trieBuf++;
	}
	else
	{
	    while (cNode != (-1) && (trieBuf+cNode)->imData < (int)(*cp))
	    {
		rNode = cNode;
		cNode = (trieBuf+cNode)->imSib;
	    }
	    if (cNode == (-1) || (trieBuf+cNode)->imData != (int)(*cp))
	    {
		if ((trieBuf = (ImTrie *) incr_buf_alloc((void *)trieBuf, sizeof(ImTrie), n_trieBuf,
			&a_trieBuf, (int)1, (int)(IM_DEFAULT_ALLOCATION*2))) == (ImTrie *)NULL)
		    return(0);
		new = n_trieBuf;
		(trieBuf+new)->imData = (int)(*cp);
		(trieBuf+new)->imChild = (-1);
		(trieBuf+new)->imSib = cNode;
		(trieBuf+new)->imNumRule = 0;
		(trieBuf+new)->imHide = (int *)NULL;
		(trieBuf+rNode)->imSib = new;
		n_trieBuf++;
	    }
	    else
	    {
		new = cNode;
	    }
	}
	pNode = rNode = new;
	cNode = (trieBuf+pNode)->imChild;
    }

    cNode = new;
    rule = ruleBuf+n_ruleBuf;

/* check for duplicates */
    for (n=0; n<(trieBuf+cNode)->imNumRule; n++)
    {
	if (same_rule(rule, ruleBuf + *(((trieBuf+cNode)->imHide)+n)))
	{
	    if (same_exceptions(rule, ruleBuf + *(((trieBuf+cNode)->imHide)+n)))
	    {
		return(1);
	    }
	    else
	    {
		fprintf(stderr, "ERROR: There were multiple rules of the type: \
			%s|%s|%s|%s|%s|%s with different exception lists\n",
			rule->imInSuf, cat_get(rule->imInCat), infl_get(rule->imInInfl),
			rule->imOutSuf, cat_get(rule->imOutCat), infl_get(rule->imOutInfl));
		return(0);
	    }
	}
    }

/* add rule */
    n = (trieBuf+cNode)->imNumRule;
    if (n == 0)
    {
	if ((((trieBuf+cNode)->imHide) = (int *) malloc((unsigned)(sizeof(int)*(n+1)))) == (int *)NULL)
	    return(0);
    }
    else
    {
	if ((((trieBuf+cNode)->imHide) = (int *) realloc((char *)(((trieBuf+cNode)->imHide)),
		(unsigned)(sizeof(int)*(n+1)))) == (int *)NULL)
	    return(0);
    }
    *(((trieBuf+cNode)->imHide)+n) = n_ruleBuf;
    ((trieBuf+cNode)->imNumRule)++;

    return(1);
}

/* checks to see if the exception terms have the correct suffix */
static int
correct_suffix()
{
    static char buf[256];
    ImRule *rule = ruleBuf+n_ruleBuf;
    ImXpn *xpn = xpnBuf+n_xpnBuf;
    char *inTerm, *outTerm;
    char *inSuf, *outSuf;

    inTerm = &charBuf[xpn->imInTerm];
    outTerm = &charBuf[xpn->imOutTerm];
    inSuf = &charBuf[rule->imInSuf];
    outSuf = &charBuf[rule->imOutSuf];

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
same_rule(r1, r2)
    ImRule *r1;
    ImRule *r2;
{
    if (strcmp(charBuf+(r1->imInSuf), charBuf+(r2->imInSuf)) == 0 &&
	    r1->imInCat == r2->imInCat && r1->imInInfl == r2->imInInfl &&
	    strcmp(charBuf+(r1->imOutSuf), charBuf+(r2->imOutSuf)) == 0 &&
	    r1->imOutCat == r2->imOutCat && r1->imOutInfl == r2->imOutInfl)
	return(1);
    return(0);
}

/* check to see if two rules have the same exception list */
static int
same_exceptions(r1, r2)
    ImRule *r1;
    ImRule *r2;
{
    int i;
    ImXpn *xpn1;
    ImXpn *xpn2;

    if (r1->imNumXpn != r2->imNumXpn)
	return(0);
	
    for (i=0; i<r1->imNumXpn; i++)
    {
	xpn1 = xpnBuf+(r1->imXpn)+i;
	xpn2 = xpnBuf+(r2->imXpn)+i;
	if (strcmp(charBuf+(xpn1->imInTerm), charBuf+(xpn2->imInTerm)) != 0 ||
		strcmp(charBuf+(xpn1->imOutTerm), charBuf+(xpn2->imOutTerm)) != 0)
	    return(0);
    }
    return(1);
}

/* check to see if a rule already has a given exception pair */
static int
duplicate_exceptions()
{
    int i;
    ImXpn *xpn = xpnBuf+n_xpnBuf;
    ImXpn *lxpn;
    ImRule *rule = ruleBuf+n_ruleBuf;

    for (i=0; i<rule->imNumXpn; i++)
    {
	lxpn = xpnBuf+rule->imXpn+i;
	if (strcmp(&charBuf[xpn->imInTerm], &charBuf[lxpn->imInTerm]) == 0 &&
		strcmp(&charBuf[xpn->imOutTerm], &charBuf[lxpn->imOutTerm]) == 0)
	    return(1);
    }
    return(0);
}

/* generates a reverse rule */
static int
reverse_rule(ruleNum)
    int ruleNum;
{
    int i;
    ImRule *rule;
    ImRule *revRule;
    ImXpn *xpn;

    if ((ruleBuf = (ImRule *) incr_buf_alloc((void *)ruleBuf, sizeof(ImRule), n_ruleBuf,
	    &a_ruleBuf, (int)1, (int)IM_DEFAULT_ALLOCATION)) == (ImRule *)NULL)
	return(0);

    rule = ruleBuf+ruleNum;
    revRule = ruleBuf+n_ruleBuf;

    revRule->imInSuf = rule->imOutSuf;
    revRule->imInCat = rule->imOutCat;
    revRule->imInInfl = rule->imOutInfl;
    revRule->imOutSuf = rule->imInSuf;
    revRule->imOutCat = rule->imInCat;
    revRule->imOutInfl = rule->imInInfl;
    revRule->imNumXpn = rule->imNumXpn;
    revRule->imXpn = n_xpnBuf;

    for (i=0; i<rule->imNumXpn; i++)
    {
	if ((xpnBuf = (ImXpn *) incr_buf_alloc((void *)xpnBuf, sizeof(ImXpn), n_xpnBuf,
		&a_xpnBuf, (int)1, (int)IM_DEFAULT_ALLOCATION)) == (ImXpn *)NULL)
	    return(0);
	xpn = xpnBuf+(rule->imXpn)+i;
	(xpnBuf+n_xpnBuf)->imInTerm = xpn->imOutTerm;
	(xpnBuf+n_xpnBuf)->imOutTerm = xpn->imInTerm;
	n_xpnBuf++;
    }
    return(1);
}

/* rewrites the rules to be sequential while sorting the exceptions */
static int
reorganize_rules()
{
    int i, j, k;
    int n;

    if ((ruleBuf2 = (ImRule *) malloc((unsigned)(sizeof(ImRule)*n_ruleBuf))) == (ImRule *)NULL)
	return(0);
    n = 0;
    for (i=0; i<n_trieBuf; i++)
    {
	(trieBuf+i)->imRule = n;
	for (j=0; j<(trieBuf+i)->imNumRule; j++)
	{
	    k = *(((trieBuf+i)->imHide)+j);
	    (ruleBuf2+n)->imInSuf = (ruleBuf+k)->imInSuf;
	    (ruleBuf2+n)->imInCat = (ruleBuf+k)->imInCat;
	    (ruleBuf2+n)->imInInfl = (ruleBuf+k)->imInInfl;
	    (ruleBuf2+n)->imOutSuf = (ruleBuf+k)->imOutSuf;
	    (ruleBuf2+n)->imOutCat = (ruleBuf+k)->imOutCat;
	    (ruleBuf2+n)->imOutInfl = (ruleBuf+k)->imOutInfl;
	    (ruleBuf2+n)->imFlags = (ruleBuf+k)->imFlags;
	    (ruleBuf2+n)->imNumXpn = (ruleBuf+k)->imNumXpn;
	    (ruleBuf2+n)->imXpn = (ruleBuf+k)->imXpn;
	    sort_exception_list((ruleBuf2+n)->imXpn, (ruleBuf2+n)->imNumXpn);
	    n++;
	}
    }
    return(1);
}

/* writes the rules to standard output */
static int
write_rules()
{
    ImRuleHeader head;

    head.imMagic = IM_RULE_MAGIC;
    head.imTrie = n_trieBuf;
    head.imRule = n_ruleBuf;
    head.imXpn = n_xpnBuf;
    head.imChar = n_charBuf;

    if (write(1, (char *)&head, sizeof(ImRuleHeader)) == (-1))
	return(0);
    if (write(1, (char *)trieBuf, sizeof(ImTrie)*n_trieBuf) == (-1))
	return(0);
    if (write(1, (char *)ruleBuf2, sizeof(ImRule)*n_ruleBuf) == (-1))
	return(0);
    if (write(1, (char *)xpnBuf, sizeof(ImXpn)*n_xpnBuf) == (-1))
	return(0);
    if (write(1, charBuf, n_charBuf) == (-1))
	return(0);
    return(1);
}

/* maps category string to im_t */
static int
get_cat(s, c)
    char *s;
    im_t *c;
{
    if (strcmp(s, "adj") == 0)
	*c = IM_CAT_ADJ;
    else if (strcmp(s, "adv") == 0)
	*c = IM_CAT_ADV;
    else if (strcmp(s, "noun") == 0)
	*c = IM_CAT_NOUN;
    else if (strcmp(s, "verb") == 0)
	*c = IM_CAT_VERB;
    else
	return(0);
    return(1);
}

/* maps im_t category to string */
static char *
cat_get(c)
    im_t c;
{
    static char *s[] = {"adj", "adv", "noun", "verb", "unknown" };

    switch (c)
    {
	case IM_CAT_ADJ:
	    return(s[0]);
	case IM_CAT_ADV:
	    return(s[1]);
	case IM_CAT_NOUN:
	    return(s[2]);
	case IM_CAT_VERB:
	    return(s[3]);
	default:
	    return(s[4]);
    }
}

/* maps inflection string to internal macro */
static int
get_infl(s, c)
    char *s;
    im_t *c;
{
    if (strcmp(s, "base") == 0)
	*c = IM_INFL_BASE;
    else if (strcmp(s, "positive") == 0)
	*c = IM_INFL_POSITIVE;
    else if (strcmp(s, "singular") == 0)
	*c = IM_INFL_SINGULAR;
    else if (strcmp(s, "infinitive") == 0)
	*c = IM_INFL_INFINITIVE;
    else if (strcmp(s, "comparative") == 0)
	*c = IM_INFL_COMPARATIVE;
    else if (strcmp(s, "superlative") == 0)
	*c = IM_INFL_SUPERLATIVE;
    else if (strcmp(s, "plural") == 0)
	*c = IM_INFL_PLURAL;
    else if (strcmp(s, "present") == 0)
	*c = IM_INFL_PRESENT;
    else if (strcmp(s, "ing") == 0)
	*c = IM_INFL_ING;
    else if (strcmp(s, "past") == 0)
	*c = IM_INFL_PAST;
    else if (strcmp(s, "pastpart") == 0)
	*c = IM_INFL_PASTPART;
    else
	return(0);
    return(1);
}

/* maps internal macro to inflection string */
static char *
infl_get(c)
    im_t c;
{
    static char *s[] = {"base", "comparative", "superlative", "plural",
	"present", "ing", "past", "pastpart", "unknown" };

    switch (c)
    {
	case IM_INFL_BASE:
	    return(s[0]);
	case IM_INFL_COMPARATIVE:
	    return(s[1]);
	case IM_INFL_SUPERLATIVE:
	    return(s[2]);
	case IM_INFL_PLURAL:
	    return(s[3]);
	case IM_INFL_PRESENT:
	    return(s[4]);
	case IM_INFL_ING:
	    return(s[5]);
	case IM_INFL_PAST:
	    return(s[6]);
	case IM_INFL_PASTPART:
	    return(s[7]);
	default:
	    return(s[8]);
    }
}

/* (shell) sorts the exceptions list (K&R, ed 1, pg 116) */
static void
sort_exception_list(start, length)
    int start;			/* where the exceptions start */
    int length;			/* how many */
{
    int gap, i, j;
    int ofs1, ofs2;

    for (gap = length/2; gap>0; gap /= 2)
    {
	for (i=gap; i<length; i++)
	{
	    for (j=i-gap; j>=0; j-=gap)
	    {
		ofs1 = ((xpnBuf+j+start)->imInTerm);
		ofs2 = ((xpnBuf+j+gap+start)->imInTerm);
		if (strcmp(&charBuf[ofs1], &charBuf[ofs2]) <= 0)
		    break;
		((xpnBuf+j+start)->imInTerm) = ofs2;
		((xpnBuf+j+gap+start)->imInTerm) = ofs1;
	    }
	}
    }
}
