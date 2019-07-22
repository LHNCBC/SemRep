
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

/* dm_fact_tran.c - translates facts from a facts file.
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <malloc.h>
#include <errno.h>
#include "dm.h"
#include "lexicon_types.h"
#include <rpc/types.h>
#include <rpc/xdr.h>

/*
    Facts are lines of the form:

    InTerm|InCat|OutTerm|OutCat

    these are read in, sorted and output.
*/

int *obsolete_get_fields(char *line, int sep, int *numF);
void *incr_buf_alloc(void *buf, int unit, int size, int *alloc, int n, int incr);
int push_loc(char *includeFile, int *linenoP, char **curFileP );
int pop_loc( int *linenoP, char **curFileP );
static int read_facts(void);
static int is_duplicate(void);
static int get_cat( char *s, dm_t *c);
static void sort_facts(void);

bool_t xdr_dm_t(XDR *xdrs, dm_t* val);
bool_t xdr_dm_fact_header(XDR *xdrs, DmFactHeader* factHeader);
bool_t xdr_dm_fact(XDR *xdrs, DmFact* fact);
static int write_dm_facts_xdr(void);


/* globals */
DmFact *factBuf = (DmFact *)NULL;
int n_factBuf = 0;
int a_factBuf = 0;

char *fcharBuf = (char *)NULL;
int n_fcharBuf = 0;
int a_fcharBuf = 0;

static FILE *fp = NULL;

/* MAIN */
int main(
	 int	argc,
	 char	*argv[]
	 )
{
    fp = stdin;
    if (!read_facts())
	return(2);

    sort_facts();

    if (!write_dm_facts_xdr())
	return(2);

    if (fclose(fp) == EOF)
	return(2);

    return(0);
}

/* reads in facts from file */
static int
read_facts(void)
{
    static char s[] = "dm.facts";
    int lineno = 0;
    char *curFile = s;
    int i;
    int errFlag = 0;
    DmFact *fact, *oldfact;
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
	if (*line == DM_COMMENT_CHAR)
	    continue;
	i=0;
	while (isspace((int)line[i]))
	    i++;

/* all whitespace? */
	if (line[i] == EOS)
	    continue;

	if ((factBuf = (DmFact *) incr_buf_alloc((void *)factBuf,
						 sizeof(DmFact),
						 n_factBuf,
						 &a_factBuf,
						 (int)1,
						 (int)(DM_DEFAULT_ALLOCATION*2))) == (DmFact *)NULL)
	    return(0);
	fact = factBuf+n_factBuf;
	fact->dmInTerm = fact->dmOutTerm = (-1);

	if ((fields=obsolete_get_fields(line, DM_FIELD_SEPARATOR_CHAR, &n)) == (int *)NULL)
	{
	    fprintf(stderr, "ERROR: Cannot break fact in file: %s, line: %d into fields!\n", curFile, lineno);
	    errFlag++;
	    continue;
	}
	if (n != 4)
	{
	    fprintf(stderr, "ERROR: Expecting 4 fields in file: %s, line: %d, found: %d!\n", curFile, lineno, n);
	    errFlag++;
	    continue;
	}

/* input term */
	fieldNum = 0;
	start = sp = &line[*(fields+2*fieldNum)];
	length = l = *(fields+2*fieldNum+1);

	if ((fcharBuf = (char *) incr_buf_alloc((void *)fcharBuf,
						sizeof(char),
						n_fcharBuf,
						&a_fcharBuf,
						(int)(length+1),
						(int)(DM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
	    return(0);
	strncpy(fcharBuf+n_fcharBuf, start, (size_t)length);
	*(fcharBuf+n_fcharBuf+length) = EOS;
	fact->dmInTerm = n_fcharBuf;
	n_fcharBuf += length+1;

/* input category */
	fieldNum++;
	start = &line[*(fields+2*fieldNum)];
	length = *(fields+2*fieldNum+1);
	strncpy(buf, start, (size_t)length);
	*(buf+length) = EOS;
	if (!get_cat(buf, &(fact->dmInCat)))
	{
	    fprintf(stderr, "ERROR: Input category incorrect in file: %s, line: %d\n", curFile, lineno);
	    errFlag++;
	    continue;
	}

/* output term */
	fieldNum++;
	start = sp = &line[*(fields+2*fieldNum)];
	length = l = *(fields+2*fieldNum+1);
	if ((fcharBuf = (char *) incr_buf_alloc((void *)fcharBuf,
						sizeof(char),
						n_fcharBuf,
						&a_fcharBuf,
						(int)(length+1),
						(int)(DM_DEFAULT_ALLOCATION*4))) == (char *)NULL)
	    return(0);
	strncpy(fcharBuf+n_fcharBuf, start, (size_t)length);
	*(fcharBuf+n_fcharBuf+length) = EOS;
	fact->dmOutTerm = n_fcharBuf;
	n_fcharBuf += length+1;

/* output category */
	fieldNum++;
	start = &line[*(fields+2*fieldNum)];
	length = *(fields+2*fieldNum+1);
	strncpy(buf, start, (size_t)length);
	*(buf+length) = EOS;
	if (!get_cat(buf, &(fact->dmOutCat)))
	{
	    fprintf(stderr, "ERROR: Output category incorrect in file: %s, line: %d\n", curFile, lineno);
	    errFlag++;
	    continue;
	}

	if (is_duplicate())
	    continue;

	n_factBuf++;

/* load reverse related terms */
	if ((factBuf = (DmFact *) incr_buf_alloc((void *)factBuf,
						 sizeof(DmFact),
						 n_factBuf,
						 &a_factBuf,
						 (int)1,
						 (int)(DM_DEFAULT_ALLOCATION*2))) == (DmFact *)NULL)
	    return(0);
	fact = factBuf+n_factBuf;
	oldfact = factBuf+n_factBuf-1;
	fact->dmInTerm = oldfact->dmOutTerm;
	fact->dmInCat = oldfact->dmOutCat;
	fact->dmOutTerm = oldfact->dmInTerm;
	fact->dmOutCat = oldfact->dmInCat;
	n_factBuf++;
    }
    return(errFlag ? 0 : 1);
}

bool_t
xdr_dm_t(XDR *xdrs, dm_t* val)
{
    return(xdr_u_int(xdrs, val));    
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

/* writes the facts in xdr format to standard output */
static int
write_dm_facts_xdr(void)
{
    XDR xdrs;			/* pointer to an XDR *stream* */
    int n;
    DmFactHeader head;

    xdrstdio_create(&xdrs, stdout, XDR_ENCODE);

    head.dmMagic = DM_FACT_MAGIC;
    head.dmFact = n_factBuf;
    head.dmChar = n_fcharBuf;

    if (xdr_dm_fact_header(&xdrs, &head) == 0) 
	return(0);

    n = n_factBuf;
    if (xdr_array(&xdrs, (caddr_t *)&factBuf, (uint_t *)&n_factBuf, (const uint_t)n, sizeof(DmFact), (xdrproc_t)xdr_dm_fact) == 0)
	return(0);

    n = n_fcharBuf;
    if (xdr_bytes(&xdrs, &fcharBuf, (uint_t *)&n_fcharBuf, (const uint_t)n) == 0)
	return(0);

    return(1);
}

/* checks to see if a given fact is already in the database */
static int
is_duplicate(void)
{
    int i;
    DmFact *curFact = (factBuf+n_factBuf);
    DmFact *fact;

    for (i=0; i<n_factBuf-1; i++)
    {
	fact = factBuf+i;
	if (	fact->dmInCat == curFact->dmInCat &&
		fact->dmOutCat == curFact->dmOutCat &&
		strcmp(fcharBuf+fact->dmInTerm, fcharBuf+curFact->dmInTerm) == 0 &&
		strcmp(fcharBuf+fact->dmOutTerm, fcharBuf+curFact->dmOutTerm) == 0)
	    return(1);
    }
    return(0);
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
    else if (strcmp(s, "") == 0)
	*c = (DM_CAT_ALL);
    else
	return(0);
    return(1);
}

/* (shell) sorts the facts list (K&R, ed 1, pg 116) */
static void
sort_facts()
{
    int gap, i, j;
    DmFact *fact1, *fact2;
    int tempI;

    for (gap = n_factBuf/2; gap>0; gap /= 2)
    {
	for (i=gap; i<n_factBuf; i++)
	{
	    for (j=i-gap; j>=0; j-=gap)
	    {
		fact1 = factBuf+j;
		fact2 = factBuf+j+gap;
		if (strcmp(&fcharBuf[fact1->dmInTerm], &fcharBuf[fact2->dmInTerm]) <= 0)
		    break;

		tempI = fact1->dmInTerm;
		fact1->dmInTerm = fact2->dmInTerm;
		fact2->dmInTerm = tempI;

		tempI = fact1->dmInCat;
		fact1->dmInCat = fact2->dmInCat;
		fact2->dmInCat = tempI;

		tempI = fact1->dmOutTerm;
		fact1->dmOutTerm = fact2->dmOutTerm;
		fact2->dmOutTerm = tempI;

		tempI = fact1->dmOutCat;
		fact1->dmOutCat = fact2->dmOutCat;
		fact2->dmOutCat = tempI;
	    }
	}
    }
}
