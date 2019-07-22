
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

/* general utility functions
*/

#include <ctype.h>
#include <stdlib.h>

#define MUTIL_DEFAULT_ALLOCATION 64
#define EOS '\0'

/* prototypes */
int *obsolete_get_fields(char *line, int sep, int *numF);
void *realloc_buf(void *buf, int unit, int size, int *alloc, int n, int incr);
void llip(register unsigned char *s);

/*  breaks a line into fields and returns an integer array
    such that a[2*n]=start offset and a[2*n+1] = length of
    the n'th field.
*/
int
*obsolete_get_fields(
		     char *line,		/* line to be broken up */
		     int sep,			/* separator char */
		     int *numF			/* number of fields */
		     )
{
    static int n_fieldBuf = 0;
    static int a_fieldBuf = 0;
    static int *fieldBuf = (int *)NULL;

    int start;
    int length;
    char *lp;

    for (start=0, n_fieldBuf=0, length=0, lp=line, *numF = 0; *lp != EOS; lp++) {
	if (*lp == sep)	{
	    if (n_fieldBuf >= a_fieldBuf)
		if ((fieldBuf = (int *) realloc_buf((void *)fieldBuf,
			sizeof(int), n_fieldBuf,
			&a_fieldBuf, (int)1, MUTIL_DEFAULT_ALLOCATION)) ==
			(int *)NULL)
		    return((int *)NULL);

	    *(fieldBuf + 2*n_fieldBuf) = start;
	    *(fieldBuf + 2*n_fieldBuf + 1) = length;
	    start = lp-line+1;
	    length = 0;
	    n_fieldBuf++;
	}
	else
	    length++;
    }
    
    if (n_fieldBuf >= a_fieldBuf)
	if ((fieldBuf = (int *) realloc_buf((void *)fieldBuf, sizeof(int),
			n_fieldBuf, &a_fieldBuf, (int)1,
			MUTIL_DEFAULT_ALLOCATION)) == (int *)NULL)
		    return((int *)NULL);

	    *(fieldBuf + 2*n_fieldBuf) = start;
	    *(fieldBuf + 2*n_fieldBuf + 1) = length;
    n_fieldBuf++;
    *numF = n_fieldBuf;
    return(fieldBuf);
}

/*  Increases the allocation of a buffer, buf using malloc/realloc.
    buf has alloc bytes currently allocated and size bytes are in use.
    n additional bytes are needed.  Allocation jumps in steps of incr.
*/
void *
realloc_buf(
	    void *buf,		/* pointer to buffer */
	    int unit,		/* size of one of what buf points to in bytes */
	    int size,		/* current size of buffer in units */
	    int *alloc,		/* current allocation of buffer in units */
	    int n,		/* units needed */
	    int incr		/* increment size in units */
	    )
{
    if ((size + n) > *alloc) {
	while ((size+n) > *alloc)
	    *alloc += incr;
	if (size == 0) {
	    if ((buf = (void *) malloc((unsigned)(unit*(*alloc)))) ==
		    (void *)NULL)
		return((void *)NULL);
	}
	else {
	    if ((buf = (void *) realloc((char *)buf,
		    (unsigned)(unit*(*alloc)))) == (void *)NULL)
		return((void *)NULL);
	}
    }
    return(buf);
}

/* lowercases uppercase letters of in s in place. */
void
llip(
     register unsigned char *s
     )
{
    for (; *s != EOS; s++)
	*s = (isupper(*s) ? tolower(*s) : *s);
    return;
}
