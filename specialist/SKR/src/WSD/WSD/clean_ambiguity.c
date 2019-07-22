/*==========================================================

%SOURCE FILE
	clean_ambiguity.c

%DESCRIPTION OF FILE
	C source file. Eliminates the ambiguities that 
	are needed for evaluation.

%REVISED
	06May02 kilicoglu -- Initial Version

%%
==========================================================*/

static char sccs_id_clean_ambiguity_c[] = "@(#)clean_ambiguity.c	1.1 10/06/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include <debug.h>
#include <wsd_client.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
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
/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int initialize_clean();
/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/*end of global variables --*/


/*==========================================================
%FUNCTION NAME
	main
%PURPOSE
        marks the ambiguities that are not needed for 
	evaluation from the input file
%RETURNS
%HEADER END
==========================================================*/
main(int argc, char *argv[])
{
    int i,j;
    int ambiguity_num;
    int ambiguity_found = FALSE;
    int index=0;
    char ambiguity[20];
    char pos_line[MAXLINE + 1];
    char ui[20];
    char phrase[500];
    char index_str[5];
    FILE *fpositions, *fout;
    xmlDocPtr doc, docOut;
    xmlNodePtr curr, utterances, xmlUtterance, xmlInputText;
    xmlNodePtr phrases, xmlPhrase;
    xmlNodePtr ambiguities, xmlAmbiguity;
    xmlNodePtr ambiguity_node, candidate;
    xmlNodePtr sense, phraseChild;


    if (argc < 6  || (argc == 6 && strcmp(argv[1],"-d") == 0))
    {
        usage();
        exit(0);
    }

    if (!initialize_clean())
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
	currtime=time(NULL); 
	fprintf(fdebug, "%s Determining the ambiguity instance in the citation.\n", asctime(localtime(&currtime)));
    }

        /* read the arguments */
        strcpy(ambiguity, argv[argc-5]);

        /* blood_pressure is a special case where the directory name will not match the matched_words */
        if (strcmp(ambiguity,"blood_pressure") == 0)
	{
	    strcpy(ambiguity,"blood,pressure");
	}
	/* the order of ambiguity in the test collection directory */
        ambiguity_num = atoi(argv[argc-3]); 

	if (DEBUG)
	    fprintf(fdebug, "[Ambiguity:%s | Order:%d | Position file:%s]\n", ambiguity, ambiguity_num, argv[argc-4]);

	/* the file that contains the phrase position information for an ambiguity case*/
	fpositions = fopen(argv[argc-4],"r");
	i = 0;
	while (i < ambiguity_num)
	{
	    if(fgets(pos_line, MAXLINE, fpositions) == (char *)NULL)
		break;
	    i++;
	}

	/* read the target phrase and ui from the file */
	sscanf(pos_line,"%[^|]|%[^|]|%[^|]|",ui,index_str,phrase);
	fclose(fpositions);

        index = atoi(index_str); 
	if (DEBUG)
	    fprintf(fdebug,"[Phrase:%s | UI:%s | Index:%d]\n", phrase, ui, index);

	/* Now, we will find the ambiguity in a given ui and phrase */
	if (DEBUG)
	    fprintf(fdebug,"Parsing file %s.\n",argv[argc-2]); 

        doc = xmlParseFile(argv[argc-2]);        

	if (DEBUG)
	    fprintf(fdebug,"Parsed file %s.\n",argv[argc-2]);

        curr = doc->children;
	j = 1;

	/* "next" at the end seems to be necessary because when a node is created with no content, 
	   a child node is created automatically with the name "TEXT" */
	if (curr->xmlChildrenNode != NULL) 
	{
	    utterances = curr->xmlChildrenNode->next;

	    while (utterances != NULL)
	    {
                if (utterances->xmlChildrenNode != NULL) 
		{
		    phrases  = utterances->xmlChildrenNode->next;
		    while (phrases != NULL)
		    {

		         if (phrases->xmlChildrenNode != NULL) {
			     phraseChild = phrases->xmlChildrenNode->next;
			     while (phraseChild != NULL)
			     {			   
				 if (!xmlStrcmp(phraseChild->name,"ambiguities"))
				 {
					if (phraseChild->xmlChildrenNode != NULL)
					{ 
					    ambiguity_node = phraseChild->xmlChildrenNode->next;
					    while (ambiguity_node != NULL)	    
					    {
                                                if (ambiguity_node->xmlChildrenNode != NULL)
						{
						    candidate = ambiguity_node->xmlChildrenNode->next;

						    if (!xmlStrcmp(xmlGetProp(utterances,"ui"),ui)) 
						    {
							if (!xmlStrcmp(xmlGetProp(candidate,"matched_words"),ambiguity) ||
							    (!xmlStrcmp(xmlGetProp(candidate,"matched_words"),"conditioning") && !strcmp(ambiguity,"condition")) ||
							    (!xmlStrcmp(xmlGetProp(candidate,"matched_words"),"mol") && !strcmp(ambiguity,"mole")))
							{
							    if (j != index) 
								xmlSetProp(ambiguity_node,"process","no");

							    if (DEBUG)
								fprintf(fdebug, "Ambiguity instance [Utterance UI:%s | Phrase:%s | Candidate matched words:%s]\n", xmlGetProp(utterances,"ui"), xmlGetProp(phrases,"noun_phrase"), xmlGetProp(candidate,"matched_words"));

							    j++;
							}
							else
							    xmlSetProp(ambiguity_node,"process","no");
						    }
						    else
							xmlSetProp(ambiguity_node,"process","no");
						}
				    
						ambiguity_node = ambiguity_node->next->next;
					    } /* while (ambiguity_node != NULL) */

					 } /* if (phraseChild->xmlChildrenNode != NULL) */

				    } /* if (!xmlStrcmp(phraseChild->name,"ambiguities") */

				    phraseChild = phraseChild->next->next;
			        } /* while (phraseChild != NULL) */

			    } /* if (phrases->xmlChildrenNode != NULL) */

			    phrases = phrases->next->next;
		        } /* while (phrases != NULL) */

		    } /* if (utterances->xmlChildrenNode != NULL  */

		    utterances = utterances->next->next;
	        } /* while (utterances != NULL) */

	    } /* if (curr->xmlChildrenNode- != NULL) */
       
	/* dump the xml tree to a file. */

	if (DEBUG)
	    fprintf(fdebug,"Dumping XML tree to %s file.\n",argv[argc-1]); 

	fout = fopen(argv[argc-1],"w");
	xmlDocDump(fout,doc);
	fclose(fout);

	if (DEBUG)
	{
	    currtime=time(NULL); 
	    fprintf(fdebug,"%s  Determined the ambiguity instance in the citation.\n",asctime(localtime(&currtime)));
	    fflush(fdebug);
	    fclose(fdebug);
	}

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
usage()
{
    printf("Usage: clean_ambiguity [option] ambiguity_name positions_file position xml_file output_file\n");
    printf("Options: \n");
    printf("    -d turns logging/debugging on.\n");
}


/*==========================================================
%FUNCTION NAME
	initialize_clean
%PURPOSE
	Reads the config info.
%SYNTAX
	int initialize_clean()
%RETURNS
	1 if successful
	0 if not successful
%HEADER END
==========================================================*/
int initialize_clean()
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

} /*** End initialize_clean*/
