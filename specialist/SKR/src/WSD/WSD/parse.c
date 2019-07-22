/*==========================================================

%SOURCE FILE
	parse.c

%DESCRIPTION OF FILE
	C source file. Parses the MetaMap output in a given file.

%REVISED
	06May02 kilicoglu -- Initial Version

%%
==========================================================*/

static char sccs_id_parse_c[] = "@(#)parse.c	1.1 10/06/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include <debug.h>
#include <wsd_client.h>
/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
/*end of constants ---------*/

/*----------------------------
%MACROS
----------------------------*/
/*end of macros ------------*/

/*----------------------------
%STATIC FUNCTIONS
----------------------------*/
/*end of static functions --*/

/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/
extern char *Process_Machine_Output(); 
/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int usage();
int initialize_parse();
/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/*end of global variables --*/


/*==========================================================
%FUNCTION NAME
	main
%PURPOSE
	Parses the machine output in a given file.
%SYNTAX
%RETURNS
%HEADER END
==========================================================*/
int main(int argc, char *argv[])
{
    char machineOutputLine[MAXLINE + 1];
    char *machineOutput = NULL;
    char *parsedMachineOutput = NULL;
    int done;
    FILE *fmach_out,*fout;

    if (argc < 3 || (argc == 3 && strcmp(argv[1],"-d") == 0))
    {
        usage();
        exit(0);
    }

    if (!initialize_parse())
	exit(0);

    if (strcmp(argv[1],"-d") == 0)
    {
	DEBUG = 1;
	fdebug = fopen(debug_file,"a");
	printf("Logging turned on. Writing to file %s.\n", debug_file);
    }
    else
    {
	DEBUG = 0;
	printf("Logging turned off.\n");
    }

    if (DEBUG)
    {
	currtime = time(NULL); 
	fprintf(fdebug, "%s Start parsing.\n", asctime(localtime(&currtime)));
    }

    fmach_out = fopen(argv[argc-2], "r");
    if (DEBUG)
    {
	fprintf(fdebug, "Opened the machine output file %s for reading.\n", argv[argc-2]);
    }

    /* read machine output file */
    done = FALSE;
    machineOutput = (char *)malloc(1);
    strcpy(machineOutput,"");
    while (!done)
    {
        if(fgets(machineOutputLine, MAXLINE, fmach_out) == (char *)NULL)
            done = TRUE;
        else
        {
            machineOutput = (char *)realloc(machineOutput, strlen(machineOutput) + strlen(machineOutputLine) + 1);
            strcat(machineOutput,machineOutputLine);
        }
    }
    fclose(fmach_out);

    if (DEBUG)
    {
	fprintf(fdebug,"Finished reading machine output[%d] from %s.\n",strlen(machineOutput),argv[argc-2]);
	fprintf(fdebug,"Parsing the machine output.\n");
    }

    parsedMachineOutput = Process_Machine_Output(machineOutput,1);

    if (DEBUG)
	fprintf(fdebug, "Completed parsing the machine output.\n");

    /* Write the parse to output file */
    if (strlen(parsedMachineOutput) > 0)
    {
        fout = fopen(argv[argc-1], "w");
	if (DEBUG)
	    fprintf(fdebug,"Opened the output file %s for writing.\n", argv[argc-1]);

        fputs(parsedMachineOutput,fout); 

	if (DEBUG)
	    fprintf(fdebug,"The parse output[%d] written to %s.\n", strlen(parsedMachineOutput), argv[argc-1]);

        fflush(fout);
        fclose(fout);
    }

    free(machineOutput);
    if (DEBUG)
    {
	currtime = time(NULL); 
	fprintf(fdebug,"%s Finished parsing the machine output.\n", asctime(localtime(&currtime)));
	fflush(fdebug);
	fclose(fdebug);
    }
    printf("Finished parsing.\n");
}

/*==========================================================
%FUNCTION NAME
	usage
%PURPOSE
	Prints out syntax information.
%SYNTAX
	usage()
%RETURNS
%HEADER END
==========================================================*/
int usage()
{
    printf("Usage: parse [option] input_text_machine_output_file parse_output_file \n");
    printf("Options: \n");
    printf("    -d turns logging/debugging on.\n");
}


/*==========================================================
%FUNCTION NAME
	initialize_parse
%PURPOSE
	Reads the config info.
%SYNTAX
	int initialize_parse()
%RETURNS
	1 if successful
	0 if not successful
%HEADER END
==========================================================*/
int initialize_parse()
{
  int return_code = D_S_SUCCESS;
  char wsd_root[MAXLINE+1];
  char config_file[MAXLINE+1];

  /* Figure out where the config file is */
  if (( char *) getenv("SKR_WSD") != NULL )
  {
    strcpy(wsd_root ,(char *) getenv("SKR_WSD"));
    printf("The $WSD environment variable is: %s\n", wsd_root);
    sprintf(config_file,"%s/config/parse.cfg",wsd_root);
    printf("The config file is: %s\n", config_file);
    return_code = cfg_read( config_file );
  }
  else
  {
    /* The WSD environment variable has not been set */
    printf("The $WSD environment variables has not been set.\n");
    printf("Can't find the location of the config file.\n");
    printf("Unable to continue.\n");
    return_code = D_E_ERROR;
  }

  /* Pick up the logfile name */
  if ( return_code == D_S_SUCCESS )
  {
      strcpy(debug_file, wsd_root);
      strcat(debug_file, "/");
      strcat(debug_file, cfg_get("PARSE_LOG_FILE"));
      printf("The log file is: %s\n", debug_file);
      return_code = 1;
  }
  else
      return_code = 0;

  return ( return_code );

} /*** End initialize_parse*/


