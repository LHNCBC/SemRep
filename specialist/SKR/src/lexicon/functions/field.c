/* field.c - splits a line on fields
*/

static char sccs_id_field_c[] = "@(#)field.c	1.2 09/27/06";

#include <stdio.h>
#include <string.h>
#include <malloc.h>

#define DEFAULT_FIELD_ALLOCATION 16

static int n_field_alloc = 0;
static int *fp = (int *)NULL;

int
*get_fields(line, sep, n)
    char *line;			/* line to be broken up */
    char sep;			/* separator char */
    int *n;			/* number of fields */
{
    int start;
    int length;
    int field;
    char *lp;

    if (fp == (int *)NULL)
    {
	n_field_alloc = DEFAULT_FIELD_ALLOCATION;
	if ((fp = (int *) malloc((unsigned)(n_field_alloc * 2 * sizeof(int)))) == (int *)NULL)
	    return((int *)NULL);
    }

    for (start=0, field=0, length=0, lp=line; *lp != '\0'; lp++)
    {
	if (*lp == sep)
	{
	    if (field == n_field_alloc)
	    {
		n_field_alloc += DEFAULT_FIELD_ALLOCATION;
		if ((fp = (int *) realloc(fp, (unsigned)(n_field_alloc * 2 * sizeof(int)))) == (int *)NULL)
		    return((int *)NULL);
	    }
	    *(fp + 2*field) = start;
	    *(fp + 2*field + 1) = length;
	    start = lp-line+1;
	    length = 0;
	    field++;
	}
	else
	    length++;
    }
    
    if (field == n_field_alloc)
    {
	n_field_alloc += DEFAULT_FIELD_ALLOCATION;
	if ((fp = (int *) realloc(fp, (unsigned)(n_field_alloc * 2 * sizeof(int)))) == (int *)NULL)
	    return((int *)NULL);
    }
    *(fp + 2*field) = start;
    *(fp + 2*field + 1) = length;
    field++;
    *n = field;
    return(fp);
}
/*
main()
{
    char s[256];
    int n;
    int i;
    int *fp;
    
    while (gets(s) != (char *)NULL)
    {
	if ((fp = get_fields(s, '|', &n)) == (int *)NULL)
	    return(1);
	for (i=0; i<n; i++)
	    printf("field #: %d, start: %d, length: %d\n", i+1, *(fp+2*i), *(fp+2*i+1));
    }
    return(0);
}
*/
