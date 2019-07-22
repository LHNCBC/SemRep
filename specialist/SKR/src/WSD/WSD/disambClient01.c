/*==========================================================

%SOURCE FILE
	disambClient01.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	06May02 kilicoglu -- Initial Version

%%
==========================================================*/

static char sccs_id_diambClient01_c[] = "@(#)disambClient01.c	1.1 10/06/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include <debug.h>
#include <wsd_srvr.h>
#include <wsd_client.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
/*end of includes ----------*/

/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/
extern int wsd_client();
extern int wsd_client_stop();
/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int initialize_disamb();
int stop_disamb();
char *query_server(char *text);
/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/*end of global variables --*/


/*==========================================================
%FUNCTION NAME
	main
%PURPOSE
	Disambiguates the ambiguities in a given file.
%SYNTAX
%RETURNS
%HEADER END
==========================================================*/
main(int argc, char *argv[])
{
    char *buff = NULL;
    char *serverInput = NULL;
    char *methods[10];
    char *weights[10];
    int i,j,done;
    char *xml_input = NULL;
    FILE *fout, *fin;
    xmlNodePtr xmlMethods, xmlMethod;
    xmlDocPtr doc;
    xmlNodePtr curr, machineOutputNode;
    xmlBufferPtr nodeBuf;

    if (argc < 4  || (argc == 4 && strcmp(argv[1],"-d") == 0))
    {
	usage();
        exit(0);
    }
    
    /* initialize WSD */
    if (!initialize_disamb())
	exit(0);

    if (strcmp(argv[1],"-d") == 0)
    {
	DEBUG = 1;
        fdebug = fopen(debug_file,"a");
        printf("Logging turned on. Writing to file %s.\n",debug_file);
        for (i=0; i<argc-4; i++)
        {
	    methods[i] = (char *)malloc(strlen(argv[i+2]));
            weights[i] = (char *)malloc(strlen(argv[i+2]));
            sscanf(argv[i+2],"-w%[^=]=%s",methods[i],weights[i]);
        }
     }
     else
     {
         DEBUG = 0;
         printf("Logging turned off.\n");
         for (i=0; i<argc-3; i++)
         {
	     methods[i] = (char *)malloc(strlen(argv[i+1]));
             weights[i] = (char *)malloc(strlen(argv[i+1]));
             sscanf(argv[i+1],"-w%[^=]=%s",methods[i],weights[i]);
         }
      }

    if (!checkWeights(weights,i))
	printf("Problem with methods weights.\n");
    else
    {
	/* Parse XML file */
	doc = xmlParseFile(argv[argc-2]);
	if (DEBUG)
	{
	    currtime = time (NULL);
	    fprintf(fdebug, "%s Started disambiguation.\n", asctime(localtime(&currtime)));
	    fprintf(fdebug, "Parsed XML file %s.\n", argv[argc-2]);
	}
	curr = doc->children;

	if (curr->xmlChildrenNode != NULL)
	{
	    machineOutputNode = curr->xmlChildrenNode->next;
	    /* Get the node for an individual citation */
	    while (machineOutputNode != NULL)
	    {
		nodeBuf = xmlBufferCreate();
		xmlMethods = xmlNewChild(machineOutputNode,NULL,"methods",NULL);
		/* Add methods to the node */
		for (j=0; j < i; j++)
		{
		    xmlMethod = xmlNewChild(xmlMethods,NULL,"method",NULL);
		    xmlSetProp(xmlMethod,"method_name",methods[j]);
		    xmlSetProp(xmlMethod,"weight",weights[j]);
		    if (DEBUG)
			fprintf(fdebug, "Method: %s Weight: %s\n", methods[j],weights[j]);
		}
		if (DEBUG)
		    fprintf(fdebug,"Added method names and weights to XML node.\n");

		xmlNodeDump(nodeBuf,doc,machineOutputNode,0,0);
		if (DEBUG)
		    fprintf(fdebug, "Dumped XML node to buffer.\n");
		
		/* Add version information to the top of the document, since xmlNodeDump() doesn't automatically do it. */
		xml_input = (char *)malloc(xmlStrlen(nodeBuf->content)+100);
		strcpy(xml_input,"");
		strcpy(xml_input,"<?xml version=\"1.0\"?>\n");
		strcat(xml_input,nodeBuf->content);
		strcat(xml_input,"\n");
		    
		/* Create the server string and send it to WSD Server */
		serverInput = (char *)malloc(strlen(xml_input) + 1);
		serverInput = xml_input;
		if (DEBUG)
		{
		    fprintf(fdebug, "Sending the XML tree to WSD Server.\n");
		    fprintf(fdebug, "XML tree: \n%s\n", serverInput);
		}
		buff = query_server(serverInput);

		if (DEBUG)
		{
		    fprintf(fdebug, "Received output from WSD Server.\n");
		    fprintf(fdebug, "DISAMBIGUATION RESULTS: %s", buff);
		}

		/* Print the results to output file */
		fout = fopen(argv[argc-1],"a");
		fprintf(fout,"%s", buff);
		fflush(fout);
		fclose(fout);
		if (DEBUG)
		    fprintf(fdebug, "Disambiguation results written to %s.\n", argv[argc-1]);

		free(xml_input);
		free(serverInput);
		xmlBufferFree(nodeBuf);
		
		machineOutputNode = machineOutputNode->next->next;

	    } /*while */
	} /* if */
	xmlFreeDoc(doc);
	xmlCleanupParser();
    } /* if then else */

    stop_disamb();
    free(methods);
    free(weights);
    if (DEBUG)
    {
	currtime = time(NULL);
	fprintf(fdebug, "%s Finished disambiguation.\n", asctime(localtime(&currtime)));
	fflush(fdebug);
	fclose(fdebug);
    }
    printf("Finished disambiguation.\n");

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
    printf("Usage: disambClient01 [options] input_file output_file\n");
    printf("Options: \n");
    printf("    -d turns logging/debugging on.\n");
    printf("    -wMETHOD=METHOD_WEIGHT assigns weights to the disambiguation methods.\n");
    printf("        At least one METHOD must be used. METHOD_WEIGHTs must total 1.0.\n");
    printf("        More than one -w option can be used.\n");
}

/*==========================================================
%FUNCTION NAME
	checkWeights
%PURPOSE
	Determines whether method weights supplied by the
	user are valid.
%SYNTAX
	int checkWeights(weights,weight size)
%RETURNS
	1 if successful
	0 if not successful
%HEADER END
==========================================================*/
int checkWeights(char *weights[], int size)
{
    /* Make sure weights total 1.0 */
    float total = 0.0;
    int i;

    for (i=0; i<size; i++)
    {
        total = total + atof(weights[i]);
    }

    if (total == 1.0)
        return TRUE;
    else
        return FALSE;

}

/*==========================================================
%FUNCTION NAME
	initialize_disamb
%PURPOSE
	Reads the config info and connects to the WSD Server.
%SYNTAX
	unsigned long initialize_disamb()
%RETURNS
	1 if successful
	0 if not successful
%HEADER END
==========================================================*/
int initialize_disamb()
{
  unsigned long return_code = D_S_SUCCESS;
  char wsd_root[MAXLINE+1];
  char config_file[MAXLINE+1];

  strcpy(wsd_root,"");
  /* Figure out where the config file is */
  if (( char *) getenv("SKR_WSD") != NULL )
  {
    strcpy(wsd_root ,(char *) getenv("SKR_WSD"));
    printf("The $WSD environment variable is: %s\n", wsd_root);
    sprintf(config_file,"%s/config/disambClient.cfg",wsd_root);
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
    strcat(debug_file, cfg_get("DISAMB_LOG_FILE"));
    printf("The log file is: %s\n", debug_file);
    return_code = 1;
  }
  else
      return_code = 0;

  return ( return_code );

} /*** End initialize_disamb */


/*==========================================================
%FUNCTION NAME
	stop_disamb
%PURPOSE
	Calls wsd_client_stop to disconnect from the WSD 
	server gracefully.
%SYNTAX
	unsigned long stop_disamb()
%RETURNS
	1 if successful
	0 if not successful
%HEADER END
==========================================================*/
int stop_disamb()
{
  unsigned long return_code;

  return_code = wsd_client_stop(
                        cfg_get("DISAMB_SERVER_HOST"),
                        (ushort)atoi(cfg_get("DISAMB_SERVER_TCP_PORT")));

  if (return_code == D_S_SUCCESS )
  {
    return_code = 1;
    if (DEBUG)
	fprintf(fdebug,"Socket closed.\n");
  }
  else
  {
    return_code = 0;
    if (DEBUG)
	fprintf(fdebug, "Problem with closing the socket.\n");
  }

  return ( return_code );

} /*** End stop_disamb */

/*==========================================================
%FUNCTION NAME
	query_server
%PURPOSE
	This routine calls the disambiguation server and 
	returns the result to a string
%SYNTAX
	char *query_server(char *text)
%RETURNS
        the string from the server
%HEADER END
==========================================================*/
char *query_server(char *text)
{
  int return_code = D_S_SUCCESS;
  char *buff;
  static char *newBuff;
  char *ptr;
  char *endPtr;

  if (DEBUG)
      fprintf(fdebug, "Opening socket to WSD Server.\n");

  return_code = wsd_client(cfg_get("DISAMB_SERVER_HOST"),
             (ushort)atoi(cfg_get("DISAMB_SERVER_TCP_PORT")),
             cfg_get("DISAMB_SERVER_DELIMITER"),
             text,
             &buff );

  if (DEBUG)
  {
      fprintf(fdebug, "Returned result from WSD Server.\n");
      fprintf(fdebug, "BUFF: %s\n", buff);
  }

  /*  Strip off the <Response> </Response> tags */

  if ( buff != NULL ) {
    ptr = strstr(buff,"<Response>" );
    endPtr = strstr(buff,"</Response>");
    *endPtr = EOS;
    newBuff = (char *)malloc(strlen(buff));
    sprintf(newBuff,"%s",ptr+10 );
    
  }
  else
  {
      newBuff = (char *)malloc(1);
      strcpy(newBuff,"");
  }
    
  free(buff);
  return newBuff;

} /*** End query_server */

