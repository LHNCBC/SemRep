#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define MAXLINE 4096
#define TRUE 1
#define FALSE 0

/* extern char *c_abgene(char *input_str); */
/* int c_abgene(char *input_str, char **output_str); */
char *c_abgene(char *input_str);

int main(int argc, char *argv[])
{
    char machineOutputLine[MAXLINE + 1];
    char *machineOutput = NULL;
    char *parsedMachineOutput = NULL;
    int done;
    int eoc;
    FILE *fmach_out,*fout;

printf("Started processing.\n");
    fmach_out = fopen(argv[argc-2], "r");

    /* read machine output file */
    done = FALSE;
    eoc = FALSE;
    machineOutput = (char *)malloc(1);
    strcpy(machineOutput,"");
    while (!done)
    {
        if(fgets(machineOutputLine, MAXLINE, fmach_out) == (char *)NULL)
            done = TRUE;
        else
        {
printf("Line: \"%s\"\n", machineOutputLine);
	    if (!strcmp(machineOutputLine,"\n"))
		eoc= TRUE;
	    else
		eoc = FALSE;
            if (eoc)
	    {
		if (strlen(machineOutput) > 0)
		{
		    /*     memset(parsedMachineOutput,0,MAXLINE); */
		    parsedMachineOutput = c_abgene(machineOutput);
		    /*     c_abgene(machineOutput, parsedMachineOutput); */

		    /* Write the parse to output file */
		    if (strlen(parsedMachineOutput) > 0)
		    {
			printf("parsed machine output is not null.\n");
			fout = fopen(argv[argc-1], "a");
			fputs(parsedMachineOutput,fout); 
			fprintf(fout,"\n");
			fflush(fout);
			fclose(fout);
		    }
		    free(machineOutput);
		    machineOutput = (char *)malloc(1);
		    strcpy(machineOutput,"");
		    eoc = FALSE;
		}
	    }
	    else
	    {
		machineOutput = (char *)realloc(machineOutput, strlen(machineOutput) + strlen(machineOutputLine) + 1);
		strcat(machineOutput,machineOutputLine);
	    }
        }
    }
    fclose(fmach_out);
    free(parsedMachineOutput);

    printf("Finished parsing.\n");
}

