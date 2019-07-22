/* im.c - inflectional morphology module
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#include <fcntl.h>
#include "im.h"

static char sccs_id_im_c[] = "@(#)im.c	1.2	09/27/06";

#define is_vowel(c)  (((c)=='a')||((c)=='e')||((c)=='i')||((c)=='o')||((c)=='u'))
#define is_consonant(c)	(isalpha((c)) && !(is_vowel((c))))

/* function prototypes */
extern void *incr_buf_alloc();

ImStruct *im_variants();
static int walk_trie();
static int try_sib();
static int read_translated_rules();
static int read_translated_facts();
static int check_facts();
static int is_exception();
static void sort_by_weight();

/* static globals */
static ImTrie *trieBuf = (ImTrie *)NULL;
static ImRule *ruleBuf = (ImRule *)NULL;
static ImXpn *xpnBuf = (ImXpn *)NULL;
static char *charBuf = (char *)NULL;
static ImFact *factBuf = (ImFact *)NULL;
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

static ImStruct *imsBuf = (ImStruct *)NULL;	/* where the variants are stored */
static int n_imsBuf = 0;
static int a_imsBuf = 0;

static ImDRule *druleBuf = (ImDRule *)NULL;
static int n_druleBuf = 0;
static int a_druleBuf = 0;

static ImDFact *dfactBuf = (ImDFact *)NULL;
static int n_dfactBuf = 0;
static int a_dfactBuf = 0;

static char symbol_table[26];	/* for upper-case letters */

/*  gets all possible inflections of term in any of categories: cats,
    generating inflections: infls, and using rules: rules.

	cats: are an OR'ing of:
	    IM_CAT_ADJ, IM_CAT_ADV, IM_CAT_NOUN, IM_CAT_VERB
	infls: are an OR'ing of:
	    IM_INFL_BASE, IM_INFL_POSITIVE, IM_INFL_SINGULAR,
	    IM_INFL_INFINITIVE, IM_INFL_COMPARTIVE, IM_INFL_SUPERLATIVE,
	    IM_INFL_PLURAL, IM_INFL_PRESENT, IM_INFL_PAST, IM_INFL_ING.
*/
ImStruct *
im_variants(term, cats, infls, numV)
    char *term;			/* input term */
    im_t cats;			/* cats to consider */
    im_t infls;			/* infls to obtain */
    int *numV;			/* number of variants generated */
{
    int i;

/* read rules */
    if (!read_translated_rules())
	return((ImStruct *)NULL);

/* read facts */
    if (!read_translated_facts())
	return((ImStruct *)NULL);

/* clear symbol table */
    for (i=0; i<26; i++)
	symbol_table[i] = EOS;

    n_outBuf = n_imsBuf = n_druleBuf = n_dfactBuf = *numV = 0;
    if (!walk_trie(trieBuf, term, cats, infls, strlen(term)) || n_imsBuf == 0)
	return((ImStruct *)NULL);

/* check facts */
    if (!check_facts(term, cats, infls))
	return((ImStruct *)NULL);

/* set the variant member */
    *numV = n_imsBuf;
    for (i=0; i<n_imsBuf; i++)
	(imsBuf+i)->imVar = (outBuf+(imsBuf+i)->imHide);

/* sort by wt */
    sort_by_weight();
    return(imsBuf);
}

/* walks the trie constructing inflections */
static int
walk_trie(trie, term, cats, infls, index)
    ImTrie *trie;
    char *term;
    im_t cats;
    im_t infls;
    int index;
{
    int i;
    int j;
    ImRule *rule;
    ImStruct *ims;
    int length;
    int sFlag = 0;		/* was symbol instantiated at this level? */

    if (index < 0)
	return(1);

    if (isupper(trie->imData))
    {
/* already instantiated? */
	if (symbol_table[trie->imData-'A'] != EOS && symbol_table[trie->imData-'A'] != *(term+index))
	    return(try_sib(trie, term, cats, infls, index));

/* any vowel */
	if (is_vowel(tolower(trie->imData)) && is_vowel(*(term+index)))
	{
	    symbol_table[trie->imData-'A'] = *(term+index);
	    sFlag = 1;		
	}
/* any digit */
	else if (trie->imData == 'D')
	{
	    if (!isdigit(*(term+index)))
		return(try_sib(trie, term, cats, infls, index));
	    symbol_table[trie->imData-'A'] = *(term+index);
	    sFlag = 1;
	}
/* any letter */
	else if (trie->imData == 'L')
	{
	    if (!isalpha(*(term+index)))
		return(try_sib(trie, term, cats, infls, index));
	    symbol_table[trie->imData-'A'] = *(term+index);
	    sFlag = 1;
	}
/* any other consonant */
	else if (is_consonant(tolower(trie->imData)) && is_consonant(*(term+index)))
	{
	    symbol_table[trie->imData-'A'] = *(term+index);
	    sFlag = 1;
	}
	else
	    return(try_sib(trie, term, cats, infls, index));
    }
    else
    {
	if (trie->imData != *(term+index))
	    return(try_sib(trie, term, cats, infls, index));
    }

/* add inflections */
    for (i=trie->imRule; i<(trie->imRule)+(trie->imNumRule); i++)
    {
	ImDRule *drule;

	rule = ruleBuf+i;
	if (((cats & rule->imInCat) == 0) || ((infls & rule->imOutInfl) == 0))
	    continue;
	if ((rule->imFlags & IM_INPUT_SOP) && (index > 0))
	    continue;

	if (is_exception(term, rule))
	    continue;

/* check minimum stem length */
	if ((strlen(term) - strlen(charBuf+(rule->imInSuf))) < IM_SMALLEST_STEM)
	    continue;

	if ((imsBuf = (ImStruct *) incr_buf_alloc((void *)imsBuf, sizeof(ImStruct), n_imsBuf,
		&a_imsBuf, (int)1, (int)IM_DEFAULT_ALLOCATION)) == (ImStruct *)NULL)
	    return(0);
	if ((druleBuf = (ImDRule *) incr_buf_alloc((void *)druleBuf, sizeof(ImDRule), n_druleBuf,
		&a_druleBuf, (int)1, (int)IM_DEFAULT_ALLOCATION)) == (ImDRule *)NULL)
	    return(0);
	ims = imsBuf + n_imsBuf;
	ims->imCat = rule->imOutCat;
	ims->imInfl = rule->imOutInfl;
	ims->imWt = strlen(charBuf+(rule->imInSuf));
	ims->imFact = (ImDFact *)NULL;
	drule = druleBuf + n_druleBuf;
	drule->imInSuf = charBuf+rule->imInSuf;
	drule->imInCat = rule->imInCat;
	drule->imInInfl = rule->imInInfl;
	drule->imOutSuf = charBuf+rule->imOutSuf;
	drule->imOutCat = rule->imOutCat;
	drule->imOutInfl = rule->imOutInfl;
	ims->imRule = drule;
	length = strlen(term) - strlen(charBuf+(rule->imInSuf)) + strlen(charBuf+(rule->imOutSuf)) + 1;
	if ((outBuf = (char *) incr_buf_alloc((void *)outBuf, sizeof(char), n_outBuf,
		&a_outBuf, (int)length, (int)(IM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
	    return(0);
	strncpy(outBuf+n_outBuf, term, length);
	strcpy(outBuf+n_outBuf+strlen(term)-strlen(charBuf+(rule->imInSuf)), charBuf+(rule->imOutSuf));
	ims->imHide = n_outBuf;
	for (j=n_outBuf; j<n_outBuf+length; j++)
	    if (isupper(*(outBuf+j)) && symbol_table[*(outBuf+j) - 'A'] != EOS)
		*(outBuf+j) = symbol_table[*(outBuf+j) - 'A'];
	n_outBuf += length;
	n_imsBuf++;
	n_druleBuf++;
    }

/* recurse for child */
    if (trie->imChild != (-1))
	if (!walk_trie(trieBuf+(trie->imChild), term, cats, infls, index-1))
	    return(0);

/* unset any symbols that may have been instantiated at this node */
    if (sFlag == 1)
    {
	symbol_table[trie->imData-'A'] = EOS;
	sFlag = 0;
    }

/* recurse for sibling */
    return(try_sib(trie, term, cats, infls, index));
}

/* walks the sibling node */
static int
try_sib(trie, term, cats, infls, index)
    ImTrie *trie;
    char *term;
    im_t cats;
    im_t infls;
    int index;
{
    return(trie->imSib == (-1) ? (int)1 : walk_trie(trieBuf+(trie->imSib), term, cats, infls, index));
}

/* reads in the translated rules from the rules file */
static int
read_translated_rules()
{
    ImRuleHeader imh;
    int fd;
    char *im_translated_rules_file = NULL;

    if ( (im_translated_rules_file = getenv("IM_TRANSLATED_RULES_FILE")) == NULL )
    {
	fprintf(stderr, "Cannot getenv  IM_TRANSLATED_RULES_FILE\n");
	return(0);
    }

    if (trieBuf != (ImTrie *)NULL)
	return(1);

    if ((fd = open(im_translated_rules_file, O_RDONLY, 0644)) == (-1))
    {
	fprintf(stderr, "Cannot open IM translated rules file: %s for reading.\n",
		im_translated_rules_file);
	return(0);
    }
    if (read(fd, (char *)(&imh), sizeof(imh)) == (-1))
    {
	fprintf(stderr, "Cannot read translated rules header in file: %s.\n", im_translated_rules_file);
	return(0);
    }
    if (imh.imMagic != IM_RULE_MAGIC)
    {
	fprintf(stderr, "Bad magic number in translated rules header in file: %s.\n", im_translated_rules_file);
	return(0);
    }
    n_trieBuf = imh.imTrie;
    n_ruleBuf = imh.imRule;
    n_xpnBuf = imh.imXpn;
    n_charBuf = imh.imChar;

/* allocate and load buffers */
    if ((trieBuf = (ImTrie *) malloc((unsigned)(sizeof(ImTrie)*n_trieBuf))) == (ImTrie *)NULL)
	return(0);
    if ((ruleBuf = (ImRule *) malloc((unsigned)(sizeof(ImRule)*n_ruleBuf))) == (ImRule *)NULL)
	return(0);
    if ((xpnBuf = (ImXpn *) malloc((unsigned)(sizeof(ImXpn)*n_xpnBuf))) == (ImXpn *)NULL)
	return(0);
    if ((charBuf = malloc ((unsigned)(n_charBuf))) == (char *)NULL)
	return(0);

    if (read(fd, (char *)trieBuf, (sizeof(ImTrie)*n_trieBuf)) == (-1))
	return(0);
    if (read(fd, (char *)ruleBuf, (sizeof(ImRule)*n_ruleBuf)) == (-1))
	return(0);
    if (read(fd, (char *)xpnBuf, (sizeof(ImXpn)*n_xpnBuf)) == (-1))
	return(0);
    if (read(fd, charBuf, n_charBuf) == (-1))
	return(0);

    return(close(fd) == (-1) ? 0 : 1);
}

/* reads in the translated facts from the facts file */
static int
read_translated_facts()
{
    ImFactHeader head;
    int fd;
    char *im_translated_facts_file = NULL;

    if ( (im_translated_facts_file = getenv("IM_TRANSLATED_FACTS_FILE")) == NULL )
    {
	fprintf(stderr, "Cannot getenv  IM_TRANSLATED_FACTS_FILE\n");
	return(0);
    }

    if (factBuf != (ImFact *)NULL)
	return(1);

    if ((fd = open(im_translated_facts_file, O_RDONLY, 0644)) == (-1))
    {
	fprintf(stderr, "Cannot open IM translated facts file: %s for reading.\n",
		im_translated_facts_file);
	return(0);
    }
    if (read(fd, (char *)(&head), sizeof(head)) == (-1))
    {
	fprintf(stderr, "Cannot read translated facts header in file: %s.\n", im_translated_facts_file);
	return(0);
    }
    if (head.imMagic != IM_FACT_MAGIC)
    {
	fprintf(stderr, "Bad magic number in translated facts header in file: %s.\n", im_translated_facts_file);
	return(0);
    }
    n_factBuf = head.imFact;
    n_fcharBuf = head.imChar;

/* allocate and load buffers */
    if ((factBuf = (ImFact *) malloc((unsigned)(sizeof(ImFact)*n_factBuf))) == (ImFact *)NULL)
	return(0);
    if ((fcharBuf = malloc ((unsigned)(n_fcharBuf))) == (char *)NULL)
	return(0);

    if (read(fd, (char *)factBuf, (sizeof(ImFact)*n_factBuf)) == (-1))
	return(0);
    if (read(fd, fcharBuf, n_fcharBuf) == (-1))
	return(0);

    return(close(fd) == (-1) ? 0 : 1);
}

/* checks the facts */
static int
check_facts(term, cats, infls)
    char *term;
    im_t cats;
    im_t infls;
{
    int low, mid, high;
    int cmp, length;
    ImFact *fact;
    ImStruct *ims;

    low=0;
    high=n_factBuf-1;

    while (low<=high)
    {
	mid = (low+high)/2;
	fact = factBuf+mid;
	cmp = strcmp(term, fcharBuf+(fact->imInTerm));
	if (cmp > 0)
	    low = mid+1;
	else if (cmp < 0)
	    high = mid-1;
	else
	{
	    while (mid >= low && strcmp(term, fcharBuf+(fact->imInTerm)) == 0)
	    {
		mid--;
		fact = factBuf+mid;
	    }
	    mid++;
	    fact = factBuf+mid;

	    while (mid <= high && strcmp(term, fcharBuf+(fact->imInTerm)) == 0)
	    {
		if ((cats & fact->imInCat) && (infls & fact->imOutInfl))
		{
		    ImDFact *dfact;

		    if ((imsBuf = (ImStruct *) incr_buf_alloc((void *)imsBuf, sizeof(ImStruct), n_imsBuf,
			    &a_imsBuf, (int)1, (int)(IM_DEFAULT_ALLOCATION))) == (ImStruct *)NULL)
			return(0);
		    if ((dfactBuf = (ImDFact *) incr_buf_alloc((void *)dfactBuf, sizeof(ImDFact), n_dfactBuf,
			    &a_dfactBuf, (int)1, (int)IM_DEFAULT_ALLOCATION)) == (ImDFact *)NULL)
			return(0);
		    ims = imsBuf+n_imsBuf;
		    ims->imCat = fact->imOutCat;
		    ims->imInfl = fact->imOutInfl;
/* variants stated as facts get a high weight */
		    ims->imWt = 50;
		    ims->imRule = (ImDRule *)NULL;
		    dfact = dfactBuf+n_dfactBuf;
		    dfact->imInTerm = fcharBuf+(fact->imInTerm);
		    dfact->imInCat = fact->imInCat;
		    dfact->imInInfl = fact->imInInfl;
		    dfact->imOutTerm = fcharBuf+(fact->imOutTerm);
		    dfact->imOutCat = fact->imOutCat;
		    dfact->imOutInfl = fact->imOutInfl;
		    ims->imFact = dfact;
		    length = strlen(fcharBuf+(fact->imOutTerm)) + 1;
		    if ((outBuf = (char *) incr_buf_alloc((void *)outBuf, sizeof(char), n_outBuf,
			    &a_outBuf, (int)length, (int)(IM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
			return(0);
		    strcpy(outBuf+n_outBuf, fcharBuf+(fact->imOutTerm));
		    ims->imHide = n_outBuf;
		    n_outBuf += length;
		    n_imsBuf++;
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
is_exception(term, rule)
    char *term;
    ImRule *rule;
{
    ImXpn *xpn;
    int low, mid, high;
    int cmp;

    low=rule->imXpn;
    high=rule->imXpn+rule->imNumXpn-1;

    while (low<=high)
    {
	mid = (low+high)/2;
	xpn = xpnBuf+mid;
	cmp = strcmp(term, charBuf+(xpn->imInTerm));
	if (cmp > 0)
	    low = mid+1;
	else if (cmp < 0)
	    high = mid-1;
	else
	    return(1);
    }
    return(0);
}

/* sorts the ims buffer by reverse length of match */
static void
sort_by_weight()
{
    int gap, i, j;
    int tmpI;
    char *tmpC;
    ImDRule *drule;
    ImDFact *dfact;

    for (gap = n_imsBuf/2; gap>0; gap /= 2)
    {
	for (i=gap; i<n_imsBuf; i++)
	{
	    for (j=i-gap; j>=0; j-=gap)
	    {
		if ((imsBuf+j)->imWt >= (imsBuf+j+gap)->imWt)
		    break;
		tmpI = (int)(imsBuf+j)->imCat;
		(imsBuf+j)->imCat = (imsBuf+j+gap)->imCat;
		(imsBuf+j+gap)->imCat = (im_t)tmpI;

		tmpI = (int)(imsBuf+j)->imInfl;
		(imsBuf+j)->imInfl = (imsBuf+j+gap)->imInfl;
		(imsBuf+j+gap)->imInfl = (im_t)tmpI;

		tmpI = (int)(imsBuf+j)->imHide;
		(imsBuf+j)->imHide = (imsBuf+j+gap)->imHide;
		(imsBuf+j+gap)->imHide = (im_t)tmpI;

		tmpC = (imsBuf+j)->imVar;
		(imsBuf+j)->imVar = (imsBuf+j+gap)->imVar;
		(imsBuf+j+gap)->imVar = tmpC;

		tmpI = (int)(imsBuf+j)->imWt;
		(imsBuf+j)->imWt = (imsBuf+j+gap)->imWt;
		(imsBuf+j+gap)->imWt = (im_t)tmpI;

		drule = (imsBuf+j)->imRule;
		(imsBuf+j)->imRule = (imsBuf+j+gap)->imRule;
		(imsBuf+j+gap)->imRule = drule;

		dfact = (imsBuf+j)->imFact;
		(imsBuf+j)->imFact = (imsBuf+j+gap)->imFact;
		(imsBuf+j+gap)->imFact = dfact;
	    }
	}
    }
}
/*
main()
{
    char s[256];
    ImStruct *ims;
    int n;
    int i;

    while (gets(s) != (char *)NULL)
    {
	if ((ims=im_variants(s, IM_CAT_ALL, IM_INFL_ALL, &n)) == (ImStruct *)NULL)
	{
	    printf("No variants.\n");
	}
	else
	{
	    for (i=0; i<n; i++)
		printf("%d|%d|%s\n", (ims+i)->imCat, (ims+i)->imInfl, (ims+i)->imVar);
	}
    }
}
*/
