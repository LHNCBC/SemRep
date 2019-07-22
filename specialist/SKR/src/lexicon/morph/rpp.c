
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

/* rpp.c - rule pre-processor for handling #includes
*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "im.h"

static void output( FILE *fp);

int main(void)
{
    output(stdin);
    return(0);
}

/* recursively outputs any included files */
static void
output(
       FILE *fp
       )
{
    char line[512];

    while (fgets(line, sizeof(line), fp) != (char *)NULL)
    {
	fputs(line, stdout);
	if (line[0] == IM_COMMENT_CHAR)
	{
		if ((strncmp(line, "#include", 8) == 0) && (isspace((int)line[8])))
	    {
		int i;

		i = 8;
		while (isspace((int)line[i]))  i++;
		if (line[i] == '"')
		{
		    FILE *incFp;
		    char filename[64];
		    int j;

		    for (++i, j=0; line[i] != EOS && line[i] != '"' && j<64; i++, j++)
			filename[j] = line[i];
		    filename[j] = EOS;
		    if ((incFp = fopen(filename, "r")) == (FILE *)NULL)
		    {
			fprintf(stderr, "Warning: include file: \"%s\" does not exist.\n", filename);
		    }
		    else
		    {
			output(incFp);
			printf("#endinclude\n");
		    }
		}
	    }
	}
    }
    (void) fclose(fp);
    return;
}
