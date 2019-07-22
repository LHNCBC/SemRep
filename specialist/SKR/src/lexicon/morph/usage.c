/* usage.c - used to print help messages.
*/

#include <stdio.h>
#include <string.h>

static char sccs_id_usage_c[] = "@(#)usage.c	1.2	09/27/06";

int usage();

/* prints a help message from a help file */
int
usage(option, helpFile)
    char option;
    char *helpFile;
{
    FILE *fp;
    char s[128];

/* check helpFile and locally */
    if ((fp=fopen(helpFile, "r")) == (FILE *)NULL)
    {
	char *sp = helpFile;
	char *ep = strrchr(sp, '/');

	if (ep != (char *)NULL && ((fp=fopen(++ep, "r")) == (FILE *)NULL))
	{
	    fprintf(stderr, "Help file: \"%s\" not found.\n", helpFile);
	    fprintf(stderr, "Please edit the line #define TIM_HELPFILE in file \"tim.c\".\n");
	    return(1);
	}
    }
    while (fgets(s, 128, fp))
    {
	if ((s[0] == ':') && (s[1] == option))
	{
	    while (fgets(s, 128, fp) && s[0] != ':')
		if (s[0] != '#')
		    fputs(s, stdout);
	    fclose(fp);
	    return(0);
	}
    }
    fclose(fp);
    return(0);
}
