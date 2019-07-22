/* tlm.c - contains sample main() program for inflectional morphology.
*/

#include <stdio.h>
#include <malloc.h>
#include "lm.h"

#define TLM_HELPFILE "tlm.help"

static char sccs_id_tlm_c[] = "@(#)tlm.c	1.2	09/27/06";

extern char *optarg;
extern int optind;

extern int usage();

void print_infls();
int parse_args();
extern LmStruct *lm_variants();
static char *cat_get();
static char *infl_get();
static char *rule_get();

/* globals */
lm_t catFlag;
lm_t ruleFlag;
lm_t inflFlag;
int debugFlag = 0;
char *query = (char *)NULL;

/* MAIN */
main(argc, argv)
int argc;
char *argv[];
{
    int n;
    LmStruct *lms;

    query = (char *)NULL;
    if (!parse_args(argc, argv))
	return(2);

    if (query != (char *)NULL)
    {
	lms=lm_variants(query, catFlag, ruleFlag, inflFlag, &n);
	print_infls(query, lms, n);
    }
    else
    {
	char s[256];

	while (gets(s) != (char *)NULL)
	{
	    lms=lm_variants(s, catFlag, ruleFlag, inflFlag, &n);
	    print_infls(s, lms, n);
	}
    }
    return(0);
}

/* parses command-line arguments */
int
parse_args(argc, argv)
    int argc;
    char *argv[];
{
    int opt;
    char *cp;
    extern lm_t catFlag;
    extern lm_t ruleFlag;
    extern lm_t inflFlag;
    extern char *query;
    int errFlag;

    errFlag = catFlag = ruleFlag = inflFlag = 0;

    while ((opt=getopt(argc, argv, "c:f:r:i:h:d")) != (-1))
    {
	switch (opt)
	{
	    case 'c': /* category */

		for (cp = optarg; *cp != EOS; cp++)
		    switch (*cp)
		    {
			case 'a': /* ADJ */
			    catFlag |= LM_CAT_ADJ;
			    break;
			case 'n': /* NOUN */
			    catFlag |= LM_CAT_NOUN;
			    break;
			case 'v': /* VERB */
			    catFlag |= LM_CAT_VERB;
			    break;
			default:
			    printf("Unknown category: %c\n", *cp);
			    usage('c');
			    errFlag++;
		    }
		break;

	    case 'r': /* rule */

		for (cp = optarg; *cp != EOS; cp++)
		    switch (*cp)
		    {
			case 'r': /* REG */
			    ruleFlag |= LM_RULE_REG;
			    break;
			case 'd': /* REGD */
			    ruleFlag |= LM_RULE_REGD;
			    break;
			case 'g': /* GLREG */
			    ruleFlag |= LM_RULE_GLREG;
			    break;
			default:
			    printf("Unknown rule: %c\n", *cp);
			    usage('r');
			    errFlag++;
		    }
		break;

	    case 'i': /* infl */

		for (cp = optarg; *cp != EOS; cp++)
		    switch (*cp)
		    {
			case 'b':
			    inflFlag |= LM_INFL_BASE;
			    break;
			case 'p':
			    inflFlag |= LM_INFL_PLURAL;
			    break;
			case 'c':
			    inflFlag |= LM_INFL_COMPARATIVE;
			    break;
			case 's':
			    inflFlag |= LM_INFL_SUPERLATIVE;
			    break;
			case 'd':
			    inflFlag |= LM_INFL_PAST;
			    break;
			case 'g':
			    inflFlag |= LM_INFL_ING;
			    break;
			case 'r':
			    inflFlag |= LM_INFL_PRESENT;
			    break;
			default:
			    printf("Unknown infl: %c\n", *cp);
			    usage('i', TLM_HELPFILE);
			    errFlag++;
		    }
		break;

	    case 'd':		/* debug option */
		debugFlag++;
		break;

	    case 'f': /* find term */
		if ((query = malloc((unsigned)(strlen(optarg)+1))) == (char *)NULL)
		    return(0);
		strcpy(query, optarg);
		break;

	    case 'h': /* help option */
		if (strlen(optarg) == 1)
		{
		    (void) usage(*optarg, TLM_HELPFILE);
		    return(0);
		}
		(void) usage(':', TLM_HELPFILE);
		return(0);
	    default:
		errFlag++;
		break;
	}
    }
    if (errFlag)
    {
	(void) usage(':', TLM_HELPFILE);
	return(0);
    }
    if (!catFlag)
    {
	catFlag = LM_CAT_ALL;
    }
    if (!ruleFlag)
    {
	ruleFlag = LM_RULE_ALL;
    }
    if (!inflFlag)
    {
	inflFlag = LM_INFL_ALL;
    }
    return(1);
}

/* pretty prints output */
void
print_infls(query, lms, n)
    char *query;
    LmStruct *lms;
    int n;
{
    register int i;
    register int j;

    for (i=0; i<n; i++)
    {
	printf("%s|%s|%s|%s|%s",
		query,
		(lms+i)->lmVar,
		cat_get((lms+i)->lmCat),
		infl_get((lms+i)->lmInfl),
		rule_get((lms+i)->lmRule));

	if (debugFlag)
	{
	    if ((lms+i)->lmDRule != (LmDRule *)NULL)
		printf(" --RULE--> %s|%s|%s|%s|%s|%s|%s",
		    (((lms+i)->lmDRule)->lmInSuf),
		    cat_get(((lms+i)->lmDRule)->lmInCat),
		    infl_get(((lms+i)->lmDRule)->lmInInfl),
		    rule_get(((lms+i)->lmDRule)->lmRule),
		    (((lms+i)->lmDRule)->lmOutSuf),
		    cat_get(((lms+i)->lmDRule)->lmOutCat),
		    infl_get(((lms+i)->lmDRule)->lmOutInfl));
	}
	printf("\n");
    }
    return;
}

static char *
cat_get(c)
    lm_t c;
{
    static char *s[] = {
	"adj", "noun", "verb", ""
    };

    switch (c)
    {
	case LM_CAT_ADJ:	return(s[0]);
	case LM_CAT_NOUN:	return(s[1]);
	case LM_CAT_VERB:	return(s[2]);
	default:		return(s[3]);
    }
}

static char *
infl_get(i)
    lm_t i;
{
    static char *s[] = {
	"base", "plural", "comparative", "superlative", "past", "ing", "present", ""
    };

    switch(i)
    {
	case LM_INFL_BASE:	    return(s[0]);
	case LM_INFL_PLURAL:	    return(s[1]);
	case LM_INFL_COMPARATIVE:   return(s[2]);
	case LM_INFL_SUPERLATIVE:   return(s[3]);
	case LM_INFL_PAST:	    return(s[4]);
	case LM_INFL_ING:	    return(s[5]);
	case LM_INFL_PRESENT:	    return(s[6]);
	default:		    return(s[7]);
    }
}

static char *
rule_get(r)
    lm_t r;
{
    static char *s[] = {
	"reg", "regd", "glreg", ""
    };

    switch(r)
    {
	case LM_RULE_REG:	    return(s[0]);
	case LM_RULE_REGD:	    return(s[1]);
	case LM_RULE_GLREG:	    return(s[2]);
	default:		    return(s[3]);
    }
}
