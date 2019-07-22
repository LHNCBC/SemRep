/* tdm.c - contains sample main() program for derivational morphology.
*/

#include <stdio.h>
#include <malloc.h>
#include "dm.h"

#define TDM_HELPFILE "/vol/Specialist/Utilities/morph/Source/tdm.help"

static char sccs_id_tdm_c[] = "@(#)tdm.c	1.2	09/27/06";

extern char *optarg;
extern int optind;

extern int usage();

void print_infls();
int parse_args();
static char *cat_get();
extern DmStruct *dm_variants();
extern DmStruct *dm_deinfl_variants();

/* globals */
dm_t catFlag;
int debugFlag = 0;
char *query = (char *)NULL;
DmStruct *(*dmfn)();

/* MAIN */
main(argc, argv)
int argc;
char *argv[];
{
    int n;
    DmStruct *dms;

    query = (char *)NULL;
    dmfn = dm_variants;
    if (!parse_args(argc, argv))
	return(2);

    if (query != (char *)NULL)
    {
	dms=(*dmfn)(query, catFlag, &n);
	print_infls(query, dms, n);
    }
    else
    {
	char s[256];

	while (gets(s) != (char *)NULL)
	{
	    dms=(*dmfn)(s, catFlag, &n);
	    print_infls(s, dms, n);
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
    extern dm_t catFlag;
    extern char *query;
    int errFlag;

    errFlag = catFlag = 0;

    while ((opt=getopt(argc, argv, "c:f:h:sd")) != (-1))
    {
	switch (opt)
	{
	    case 'c': /* category */

		for (cp = optarg; *cp != EOS; cp++)
		    switch (*cp)
		    {
			case 'a': catFlag |= DM_CAT_ADJ;    break;
			case 'b': catFlag |= DM_CAT_ADV;    break;
			case 'x': catFlag |= DM_CAT_AUX;    break;
			case 'c': catFlag |= DM_CAT_COMPL;  break;
			case 'j': catFlag |= DM_CAT_CONJ;   break;
			case 'd': catFlag |= DM_CAT_DET;    break;
			case 'm': catFlag |= DM_CAT_MODAL;  break;
			case 'n': catFlag |= DM_CAT_NOUN;   break;
			case 'p': catFlag |= DM_CAT_PREP;   break;
			case 'r': catFlag |= DM_CAT_PRON;   break;
			case 'v': catFlag |= DM_CAT_VERB;   break;
			default:
			    printf("Unknown category: %c\n", *cp);
			    usage('c', TDM_HELPFILE);
			    errFlag++;
		    }
		break;

	    case 'f': /* find term */
		if ((query = malloc((unsigned)(strlen(optarg)+1))) == (char *)NULL)
		    return(0);
		strcpy(query, optarg);
		break;

	    case 's': /* de-inflect */
		dmfn = dm_deinfl_variants;
		break;

	    case 'd':		/* debug */
		debugFlag++;
		break;

	    case 'h': /* help option */
		if (strlen(optarg) == 1)
		{
		    (void) usage(*optarg, TDM_HELPFILE);
		    return(0);
		}
		(void) usage(':', TDM_HELPFILE);
		return(0);
	    default:
		errFlag++;
		break;
	}
    }
    if (errFlag)
    {
	(void) usage(':', TDM_HELPFILE);
	return(0);
    }
    if (!catFlag)
	catFlag = DM_CAT_ALL;

    return(1);
}

/* pretty prints output */
void
print_infls(query, dms, n)
    char *query;
    DmStruct *dms;
    int n;
{
    register int i;
    register int j;

    for (i=0; i<n; i++)
    {
	printf("%s|%s|%s", query, (dms+i)->dmVar, cat_get((dms+i)->dmCat));
	if (debugFlag)
	{
	    if ((dms+i)->dmRule != (DmDRule *)NULL)
		printf(" --RULE--> %s|%s|%s|%s",
		    (((dms+i)->dmRule)->dmInSuf),
		    cat_get(((dms+i)->dmRule)->dmInCat),
		    (((dms+i)->dmRule)->dmOutSuf),
		    cat_get(((dms+i)->dmRule)->dmOutCat));

	    if ((dms+i)->dmFact != (DmDFact *)NULL)
		printf(" --FACT--> %s|%s|%s|%s",
		    (((dms+i)->dmFact)->dmInTerm),
		    cat_get(((dms+i)->dmFact)->dmInCat),
		    (((dms+i)->dmFact)->dmOutTerm),
		    cat_get(((dms+i)->dmFact)->dmOutCat));
	}
	printf("\n");
    }
}

static char *
cat_get(c)
    dm_t c;
{
    static char *s[] = {
	"adj", "adv", "aux", "compl", "conj", "det",
	"modal", "noun", "prep", "pron", "verb", ""
    };

    switch (c)
    {
	case DM_CAT_ADJ:	return(s[0]);
	case DM_CAT_ADV:	return(s[1]);
	case DM_CAT_AUX:	return(s[2]);
	case DM_CAT_COMPL:	return(s[3]);
	case DM_CAT_CONJ:	return(s[4]);
	case DM_CAT_DET:	return(s[5]);
	case DM_CAT_MODAL:	return(s[6]);
	case DM_CAT_NOUN:	return(s[7]);
	case DM_CAT_PREP:	return(s[8]);
	case DM_CAT_PRON:	return(s[9]);
	case DM_CAT_VERB:	return(s[10]);
	default:		return(s[11]);
    }
}
