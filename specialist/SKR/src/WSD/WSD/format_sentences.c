/*==========================================================

%SOURCE FILE
	format_sentences.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	06May02 kilicoglu -- Initial Version

%%
==========================================================*/

static char sccs_id_format_sentences[] = "@(#)format_sentences.c	1.1 10/06/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include <stdio.h>

/*end of includes ----------*/

#define MAXLINE 40096
#define FALSE 0
#define TRUE 1

main(int argc, char *argv[])
{
    FILE *fin, *fout;
    char line[MAXLINE + 1];
    char sentence[MAXLINE + 1];
    char inparentheses[MAXLINE + 1];
    char rest[MAXLINE + 1];
    char sentence_ui[50];
    char firstLetter = NULL;
    int done;

    fin = fopen(argv[1], "r");
    fout = fopen(argv[2], "w");

    done = FALSE;

    while (!done)
    {
        if(fgets(line, MAXLINE, fin) == (char *)NULL)
            done = TRUE;
        else
        {
            sscanf(line,"%[^|]|%[^\n]",sentence_ui,sentence);
            firstLetter = sentence[0];
            /*if the sentence starts with a square bracket, get rid of the brackets. */
            if (firstLetter == '[')
            {
                sscanf(sentence,"[%[^]]]%[^\n]",inparentheses,rest);
                strcpy(sentence,"");
                strcat(sentence,inparentheses);
                strcat(sentence,rest);
            }
            /* uppercase the first letter of the sentence */
            if (islower(firstLetter))
            {
                firstLetter = toupper(firstLetter);
                sentence[0] = firstLetter;
            }
            /* write the new formatted sentence to the output file */
            fprintf(fout,"[%s]%s\n\n",sentence_ui, sentence);
        }
    }

    fflush(fout);
    fclose(fin);
    fclose(fout);
}
