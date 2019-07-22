
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

/* util.c - some utilities
*/

#include <stdio.h>
#include <string.h>
#include <malloc.h>

extern void *realloc_buf(void *buf, int unit, int size, int *alloc, int n, int incr);
void *incr_buf_alloc( void *buf, int unit, int size, int *alloc, int n, int incr );
int cat_str_to_int(char *cs, int *ip);
char *cat_int_to_str(int i);

/*  Increases the allocation of a buffer, buf using malloc/realloc.
    buf has alloc bytes currently allocated and size bytes are in use.
    n additional bytes are needed.  Allocation jumps in steps of incr.
*/

void *
incr_buf_alloc(
	       void *buf,       /* pointer to buffer */
	       int unit,	/* size of one of what buf points to in bytes */
	       int size,	/* current size of buffer in units */
	       int *alloc,	/* current allocation of buffer in units */
	       int n,		/* units needed */
	       int incr		/* increment size in units */
	       )
{
/* these functions are identical! */
    return(realloc_buf(buf, unit, size, alloc, n, incr));
}


/* maps a category string to an integer */
int
cat_str_to_int(
	       char *cs,
	       int *ip
	       )
{
    static struct {
	char *s;
	int i;
    } x[] = {
	{ "adj",      0}, 
	{ "adv",      1},
	{ "aux",      2},
	{ "compl",    3},
	{ "conj",     4},
	{ "det",      5},
	{ "modal",    6},
	{ "noun",     7},
	{ "prep",     8},
	{ "pron",     9},
	{ "verb",     10}
    };
    static int n_cats = 11;

    int low, mid, high;
    int cmp;

    low = 0;
    high = n_cats-1;
    while (low<=high)
    {
	mid = (low+high)/2;
	cmp = strcmp(cs, x[mid].s);
	if (cmp > 0)
	    low = mid+1;
	else if (cmp < 0)
	    high = mid-1;
	else
	{
	    *ip = x[mid].i;
	    return(1);
	}
    }
    return(0);
}

/* maps an integer category to a string */
char *cat_int_to_str(
		     int i
		     )
{
    static char *s[] = {
	"adj", "adv", "aux", "compl", "conj", "det",
	"modal", "noun", "prep", "pron", "verb"
    };
    static int n_cats = 11;

    return((i<n_cats) ? s[i] : (char *)NULL);
}
