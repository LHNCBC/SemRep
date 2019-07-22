/* im_fact_tran.c - translates facts from a facts file.
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>
#include <malloc.h>
#include <errno.h>
#include "im.h"

static char sccs_id_im_fact_tran_c[] = "@(#)im_fact_tran.c	1.2	09/27/06";

/*
    Facts are lines of the form:

    InTerm|InCat|InInfl|OutTerm|OutCat|OutInfl

    these are read in, sorted and output.
    if left empty, cat and infl fields are set to ALL.
*/

extern int *obsolete_get_fields();
extern int *incr_buf_alloc();
extern int push_loc();		/* in loc.c */
extern int pop_loc();
static int read_facts();
static int write_facts();
static int is_duplicate();
static int get_cat();
static int get_infl();
static void sort_facts();

/* globals */
ImFact *factBuf = (ImFact *)NULL;
int n_factBuf = 0;
int a_factBuf = 0;

char *fcharBuf = (char *)NULL;
int n_fcharBuf = 0;
int a_fcharBuf = 0;

static FILE *fp=NULL;

/* MAIN */
main(argc, argv)
int	argc;
char	*argv[];
{
    fp=stdin;
    if (!read_facts())
	return(2);

    sort_facts();

    if (!write_facts())
	return(2);

    if (fclose(fp) == EOF)
	return(2);

    return(0);
}

/* reads in facts from file */
static int
read_facts()
{
    static char s[] = "im.facts";
    int lineno = 0;
    char *curFile = s;
    int i;
    int errFlag = 0;
    ImFact *fact, *oldfact;
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

/* plain old comment */
	if (*line == IM_COMMENT_CHAR)
	    continue;
	i=0;
	while (isspace(line[i]))
	    i++;

/* all whitespace? */
	if (line[i] == EOS)
	    continue;

	if ((factBuf = (ImFact *) incr_buf_alloc((void *)factBuf, sizeof(ImFact), n_factBuf,
		&a_factBuf, (int)1, (int)(IM_DEFAULT_ALLOCATION*2))) == (ImFact *)NULL)
	    return(0);
	fact = factBuf+n_factBuf;
	fact->imInTerm = fact->imOutTerm = (-1);

	if ((fields=obsolete_get_fields(line, IM_FIELD_SEPARATOR_CHAR, &n)) == (int *)NULL)
	{
	    fprintf(stderr, "ERROR: Cannot break fact in file: %s, line: %d into fields!\n", curFile, lineno);
	    errFlag++;
	    continue;
	}
	if (n != 6)
	{
	    fprintf(stderr, "ERROR: Expecting 6 fields in file: %s, line: %d, found: %d!\n", curFile, lineno, n);
	    errFlag++;
	    continue;
	}

/* input term */
	fieldNum = 0;
	start = sp = &line[*(fields+2*fieldNum)];
	length = l = *(fields+2*fieldNum+1);

	if ((fcharBuf = (char *) incr_buf_alloc((void *)fcharBuf, sizeof(char), n_fcharBuf,
		&a_fcharBuf, (int)(length+1), (int)(IM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
	    return(0);
	strncpy(fcharBuf+n_fcharBuf, start, length);
	*(fcharBuf+n_fcharBuf+length) = EOS;
	fact->imInTerm = n_fcharBuf;
	n_fcharBuf += length+1;

/* input category */
	fieldNum++;
	start = &line[*(fields+2*fieldNum)];
	length = *(fields+2*fieldNum+1);
	strncpy(buf, start, length);
	*(buf+length) = EOS;
	if (!get_cat(buf, &(fact->imInCat)))
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
	if (!get_infl(buf, &(fact->imInInfl)))
	{
	    fprintf(stderr, "ERROR: Input inflection incorrect in file: %s, line: %d\n", curFile, lineno);
	    errFlag++;
	    continue;
	}

/* output term */
	fieldNum++;
	start = sp = &line[*(fields+2*fieldNum)];
	length = l = *(fields+2*fieldNum+1);
	if ((fcharBuf = (char *) incr_buf_alloc((void *)fcharBuf, sizeof(char), n_fcharBuf,
		&a_fcharBuf, (int)(length+1), (int)(IM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
	    return(0);
	strncpy(fcharBuf+n_fcharBuf, start, length);
	*(fcharBuf+n_fcharBuf+length) = EOS;
	fact->imOutTerm = n_fcharBuf;
	n_fcharBuf += length+1;

/* output category */
	fieldNum++;
	start = &line[*(fields+2*fieldNum)];
	length = *(fields+2*fieldNum+1);
	strncpy(buf, start, length);
	*(buf+length) = EOS;
	if (!get_cat(buf, &(fact->imOutCat)))
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
	if (!get_infl(buf, &(fact->imOutInfl)))
	{
	    fprintf(stderr, "ERROR: Output inflection incorrect in file: %s, line: %d\n", curFile, lineno);
	    errFlag++;
	    continue;
	}

	if (is_duplicate())
	    continue;

	n_factBuf++;

/* load reverse related terms */
	if ((factBuf = (ImFact *) incr_buf_alloc((void *)factBuf, sizeof(ImFact), n_factBuf,
		&a_factBuf, (int)1, (int)(IM_DEFAULT_ALLOCATION*2))) == (ImFact *)NULL)
	    return(0);
	fact = factBuf+n_factBuf;
	oldfact = factBuf+n_factBuf-1;
	fact->imInTerm = oldfact->imOutTerm;
	fact->imInCat = oldfact->imOutCat;
	fact->imInInfl = oldfact->imOutInfl;
	fact->imOutTerm = oldfact->imInTerm;
	fact->imOutCat = oldfact->imInCat;
	fact->imOutInfl = oldfact->imInInfl;
	n_factBuf++;
    }
    return(errFlag ? 0 : 1);
}

/* writes the facts to standard output */
static int
write_facts()
{
    ImFactHeader head;

    head.imMagic = IM_FACT_MAGIC;
    head.imFact = n_factBuf;
    head.imChar = n_fcharBuf;

    if (write(1, (char *)&head, sizeof(ImFactHeader)) == (-1))
	return(0);
    if (write(1, (char *)factBuf, sizeof(ImFact)*n_factBuf) == (-1))
	return(0);
    if (write(1, fcharBuf, n_fcharBuf) == (-1))
	return(0);
    return(1);
}

/* checks to see if a given fact is already in the database */
static int
is_duplicate()
{
    int i;
    ImFact *curFact = (factBuf+n_factBuf);
    ImFact *fact;

    for (i=0; i<n_factBuf-1; i++)
    {
	fact = factBuf+i;
	if (fact->imInInfl == curFact->imInInfl &&
		fact->imOutInfl == curFact->imOutInfl &&
		fact->imInCat == curFact->imInCat &&
		fact->imOutCat == curFact->imOutCat &&
		strcmp(fcharBuf+fact->imInTerm, fcharBuf+curFact->imInTerm) == 0 &&
		strcmp(fcharBuf+fact->imOutTerm, fcharBuf+curFact->imOutTerm) == 0)
	    return(1);
    }
    return(0);
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
    else if (strcmp(s, "") == 0)
	*c = (IM_CAT_ALL);
    else
	return(0);
    return(1);
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
    else if (strcmp(s, "prespart") == 0)
	*c = IM_INFL_ING;
    else if (strcmp(s, "past") == 0)
	*c = IM_INFL_PAST;
    else if (strcmp(s, "pastpart") == 0)
	*c = IM_INFL_PASTPART;
    else if (strcmp(s, "") == 0)
	*c = IM_INFL_PRES3PS;
    else if (strcmp(s, "pres3ps") == 0)

	*c = IM_INFL_ALL;
    else
	return(0);
    return(1);
}

/* (shell) sorts the facts list (K&R, ed 1, pg 116) */
static void
sort_facts()
{
    int gap, i, j;
    ImFact *fact1, *fact2;
    int tempI;

    for (gap = n_factBuf/2; gap>0; gap /= 2)
    {
	for (i=gap; i<n_factBuf; i++)
	{
	    for (j=i-gap; j>=0; j-=gap)
	    {
		fact1 = factBuf+j;
		fact2 = factBuf+j+gap;
		if (strcmp(&fcharBuf[fact1->imInTerm], &fcharBuf[fact2->imInTerm]) <= 0)
		    break;

		tempI = fact1->imInTerm;
		fact1->imInTerm = fact2->imInTerm;
		fact2->imInTerm = tempI;

		tempI = fact1->imInCat;
		fact1->imInCat = fact2->imInCat;
		fact2->imInCat = tempI;

		tempI = fact1->imInInfl;
		fact1->imInInfl = fact2->imInInfl;
		fact2->imInInfl = tempI;

		tempI = fact1->imOutTerm;
		fact1->imOutTerm = fact2->imOutTerm;
		fact2->imOutTerm = tempI;

		tempI = fact1->imOutCat;
		fact1->imOutCat = fact2->imOutCat;
		fact2->imOutCat = tempI;

		tempI = fact1->imOutInfl;
		fact1->imOutInfl = fact2->imOutInfl;
		fact2->imOutInfl = tempI;
	    }
	}
    }
}
