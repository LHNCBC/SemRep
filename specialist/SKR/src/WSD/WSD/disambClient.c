/*==========================================================

%SOURCE FILE
	disambClient.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	06May02 kilicoglu -- Initial Version

%%
==========================================================*/

static char sccs_id_diambClient_c[] = "@(#)disambClient.c	1.2 11/02/06";

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
extern char *Process_Machine_Output();
/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int c_initialize_disamb();
int c_stop_disamb();
int c_recv_text(char *text);
char *c_disambiguate();

int query_server(char *text, char **response);
int checkMethods(char *methods_in, char *weights);
int checkWeights(char *weights[], int size);
/*end_of_function_prototypes*/

char *method_names[10];
char *weight_values[10];
int  method_length;
char *machine_output = NULL;

/*==========================================================
%FUNCTION NAME
	c_recv_text
%PURPOSE
	Receives text from Prolog program in pieces 
	and assembles it back. (This routine was written to
	overcome the size limit (64K) on Prolog atoms.)
%SYNTAX
	int c_recv_text(char *text)
%RETURNS
	0 
%HEADER END
==========================================================*/
int c_recv_text(char *text)
{ 
  static int previous_text_len = 0;

  if (DEBUG) {
    fprintf(fdebug, "Receiving line: %s.", text);
    fflush(fdebug);
  }

    if (machine_output == NULL)
    {
	machine_output = (char *)calloc(1,sizeof(char));
	strcpy(machine_output,"");
    }
    machine_output = (char *)realloc(machine_output,strlen(machine_output)+strlen(text)+3);
    if (strstr(text, "#END#OF#ATOM#") != NULL ) {
      if (previous_text_len < 65529) {
        strcat(machine_output,"\n");
      } 
    } else {
      strcat(machine_output, text);
      previous_text_len = strlen(text);
    }
    if (DEBUG) {
      fprintf(fdebug,"Done.\n");
        fflush(fdebug);
    }
    return TRUE;
} /*** End c_recv_text */



/*==========================================================
%FUNCTION NAME
	c_disambiguate
%PURPOSE
	Disambiguation routine. 
%SYNTAX
	char *c_disambiguate()
%RETURNS
	the disambiguation output.
%HEADER END
==========================================================*/
char *c_disambiguate()
{
    char *buff = NULL;
    char *parsedMachineOutput = NULL;
    char *serverInput = NULL;
    char *output = NULL;
    char *xmlInput;
    xmlNodePtr xmlMethods, xmlMethod;
    xmlDocPtr doc;
    xmlChar *xmlOutput = NULL;
    int i,j, done, len;
    int return_code = 0;

    done = FALSE;
    if (DEBUG)
    {
	currtime = time(NULL); 
	fprintf(fdebug, "%s Started disambiguation.\n", asctime(localtime(&currtime)));
	fprintf(fdebug, "Parsing machine output.\n");
	fprintf(fdebug, "length: %d: %s\n", strlen(machine_output), machine_output);
        fflush(fdebug);
    }

    strcat(machine_output,"\n");
    parsedMachineOutput = Process_Machine_Output(machine_output,0);
   
    if (strlen(parsedMachineOutput) > 0)
    {
	if (DEBUG)
	    fprintf(fdebug, " Parsed machine output, determined the ambiguities.\n");

	xmlInput = (char *)calloc(strlen(parsedMachineOutput)+200,sizeof(char)); 
 	strcpy(xmlInput,parsedMachineOutput);
 	doc = xmlParseMemory(xmlInput,strlen(xmlInput));
	
	/*Add method weights  to XML tree */
	xmlMethods = xmlNewChild(doc->children,NULL,"methods",NULL);
	for (i = 0; i < method_length; i++)
	{
	    if (atof(weight_values[i]) != 0.0)
	    {
		if (DEBUG)
		    fprintf(fdebug, "[Method Name:%s | Weight:%s]\n", method_names[i],weight_values[i]);

		xmlMethod = xmlNewChild(xmlMethods,NULL,"method",NULL);
		xmlSetProp(xmlMethod,"method_name", method_names[i]);
		xmlSetProp(xmlMethod,"weight", weight_values[i]);
		
		if (DEBUG)
		    fprintf(fdebug,"Added method names and weights to XML tree.\n");
	    }
	 }
	 xmlDocDumpMemory(doc, &xmlOutput, &len);

	 if (DEBUG)
	     fprintf(fdebug, "Dumped XML tree to buffer.\n");

 	 serverInput = (char *)calloc(xmlStrlen(xmlOutput) + 1,sizeof(char));
	 strcpy(serverInput,xmlOutput);
	 
	 if (DEBUG)
	 {
	     fprintf(fdebug, "Server input: %s\n", serverInput);
	     fprintf(fdebug, "Sending the XML tree to WSD Server.\n");
             fflush(fdebug);
	 }

         return_code = query_server(serverInput, &buff);
         if (return_code == D_E_ERROR) {
           fprintf(stderr, "ERROR: Error attempting to contact WSD Server; is WSD Server running?\n");
           return strdup("[Error attempting to contact WSD Server; is WSD Server running?]");
         }

	 if (DEBUG)
	     fprintf(fdebug, "Received output from WSD Server.\n");

         if (buff == NULL)
              fprintf(stderr, "The buffer returned from query_server is null. Is WSD Server running?\n");
	 else {
	     if (strlen(buff) > 0) {
		 output = (char *)calloc(strlen(buff) + 1, sizeof(char));
		 strcpy(output,""); strcpy(output,buff);
	     } else {
		 output = (char *)calloc(6,sizeof(char));
		 strcpy(output,""); strcpy(output, "None.");
	     }
	 }
	     

	 if (DEBUG)
	 {
	     fprintf(fdebug, "DISAMBIGUATION RESULTS: %s\n", output);
	     fprintf(fdebug, "Sending disambiguation results back to MetaMap.\n");
	 }
    } 
    else
    {
        output = (char *)calloc(1,sizeof(char));
        strcpy(output,"");
	
	if (DEBUG)
	    fprintf(fdebug, "No ambiguity found in the machine output.\n");
    }

    /* Empty machine output buffer. */
    strcpy(machine_output,"");
    free(serverInput);
    free(xmlInput);

    if (DEBUG)
    {
	currtime=time(NULL);
	fprintf(fdebug, "%s Finished disambiguation.\n", asctime(localtime(&currtime)));
    }

    return output;    
} /*** End c_disambiguate */


/*==========================================================
%FUNCTION NAME
	checkWeights
%PURPOSE
	This routine checks the weight information and ensures
	they total 100%.
%SYNTAX
	unsigned long  checkWeights(char *weights[], int size)
%RETURNS
        1 if weight total is 1.0
	0 else
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
} /*** end of checkWeights */


/*==========================================================
%FUNCTION NAME
        c_initialize_disamb
%PURPOSE
	The prolog interface that connects to the Disambiguation server.
%SYNTAX
	unsigned long initialize_disamb()
%RETURNS
	1 if successful
	0 if not successful
%HEADER END
==========================================================*/
int c_initialize_disamb()
{
  int return_code = D_S_SUCCESS;
  char wsd_root[MAXLINE+1];
  char config_file[MAXLINE+1];
  char debug[5];

  /* Figure out where the config file is */     
  if (( char *) getenv("SKR_WSD") != NULL )
  {
    strcpy(wsd_root ,(char *) getenv("SKR_WSD"));
    printf("The WSD environment variable is: %s\n", wsd_root);
    sprintf(config_file,"%s/config/disambClient_metamap.cfg",wsd_root);
    printf("The config file is: %s\n", config_file);
    return_code = cfg_read( config_file );
    choose_wsd_initialize(wsd_root);
  }
  else
  {
    /* The WSD environment variable has not been set */
    printf("ERROR: The $SKR_WSD environment variable has not been set.\n");
    printf("Can't find the location of the config file.\n");
    printf("Unable to continue.\n");
    return_code = D_E_ERROR;
  }

  /* Pick up the needed configuration information */
  if ( return_code == D_S_SUCCESS )
  {
    if (strcmp(cfg_get("DISAMB_LOG"),"true") == 0)
    {
	DEBUG = 1;
	strcpy(debug_file, wsd_root);
	strcat(debug_file, "/");
	strcat(debug_file, cfg_get("DISAMB_LOG_FILE"));
	printf("The log file is: %s\n", debug_file);
	fdebug = fopen(debug_file,"a");
    }
    else
	DEBUG = 0;
    return_code = checkMethods(cfg_get("DISAMB_METHODS"), cfg_get("DISAMB_WEIGHTS"));
    printf("Initialized the WSD Proxy.\n");
  }
  else
      return_code = 0;

  return(return_code);
} /*** End c_initialize_disamb */

/*==========================================================
%FUNCTION NAME
	c_stop_disamb
%PURPOSE
	The prolog interface which calls nls_client_stop to disconnect
        from the Disambiguation server.
%SYNTAX
	unsigned long stop_disamb()
%RETURNS
	1 if successful
	0 if not successful
%HEADER END
==========================================================*/
int c_stop_disamb()
{
  int return_code;

  if (DEBUG)
  {
      fflush(fdebug);
      fclose(fdebug);
      printf("Closed log file.\n");
  }

  return_code = wsd_client_stop(
                        cfg_get("DISAMB_SERVER_HOST"),
                        (ushort)atoi(cfg_get("DISAMB_SERVER_TCP_PORT")));
  if ( return_code == D_S_SUCCESS )
  {
    return_code = 1;
    printf("Socket closed.\n");
  }
  else
  {
    return_code = 0;
    printf("Problem with closing the socket.\n");
  }

  return ( return_code );
} /*** End c_stop_disamb */

/*==========================================================
%FUNCTION NAME
	query_server
%PURPOSE
	This routine calls the disambiguation server and 
	returns the result to a string
%SYNTAX
	int query_server(char *text, char **response)
%RETURNS
        1 if successful
	0 if not successful
%HEADER END
==========================================================*/
int query_server( char *text, char **response)
{
  int return_code = D_S_SUCCESS;
  char *buff;
  char *newBuff;
  char *ptr;
  char *endPtr;
  char choice;

  if (DEBUG)
      fprintf(fdebug, "Opening socket to WSD Server.\n");

  choose_wsd(&choice);
  if (DEBUG)
    fprintf(fdebug, "choice: [%c]", choice);
  if (choice == 'b') {
    if (DEBUG) {
/*       fprintf(stderr, "using WSD server on %s\n", cfg_get("DISAMB_SERVERB_HOST")); */
      fprintf(fdebug, "using WSD server on %s\n", cfg_get("DISAMB_SERVERB_HOST"));
      fflush(fdebug);
    }
    return_code = wsd_client(cfg_get("DISAMB_SERVERB_HOST"),
                             (ushort)atoi(cfg_get("DISAMB_SERVER_TCP_PORT")),
                             cfg_get("DISAMB_SERVER_DELIMITER"),
                             text,
                             &buff );
  } else {
    if (DEBUG) {
      /*fprintf(stderr, "using WSD server on %s\n", cfg_get("DISAMB_SERVER_HOST"));*/
      fprintf(fdebug, "using WSD server on %s\n", cfg_get("DISAMB_SERVER_HOST"));
      fflush(fdebug);
    }
    return_code = wsd_client(cfg_get("DISAMB_SERVER_HOST"),
                             (ushort)atoi(cfg_get("DISAMB_SERVER_TCP_PORT")),
                             cfg_get("DISAMB_SERVER_DELIMITER"),
                             text,
                             &buff );
  }

  if (DEBUG)
      fprintf(fdebug, "Returned result from WSD Server.\n");


  /*  Strip off the <Response> </Response> tags */
  if (return_code == D_E_ERROR) {
    fprintf(stderr, "ERROR: disambigClient: wsd_client returned D_E_ERROR whose value is %d\n", D_E_ERROR);
  } else {
    if ( buff != NULL ) {
      ptr = strstr(buff,"<Response>" );
      endPtr = strstr(buff,"</Response>");
      *endPtr = EOS;
      newBuff = (char *)calloc(strlen(buff),sizeof(char));
      sprintf(newBuff,"%s",ptr+10 );
      
      *response = newBuff;
      free(newBuff);
      free(buff);
    }
  }
  return ( return_code );

} /*** End query_server */

/*==========================================================
%FUNCTION NAME
	checkMethods
%PURPOSE
	This routine organizes the disambiguation method and
	their weight information 
%SYNTAX
	unsigned long  checkMethods(char *methods_in, char *weights)
%RETURNS
        1 if successful
	0 if not successful
%HEADER END
==========================================================*/
int checkMethods(char *methods_in, char *weights)
{
    char *method;
    char *weight;
    char *separator = ":";
    int return_code;
    int i=1;
    int j=1;

    if ((method = strtok(methods_in,separator)) != NULL)
    {
        method_names[0] = method;
        while ((method = strtok(NULL,separator)) != NULL)
        {
            method_names[i] = method;
            i++;
        }
    }

    if ((weight = strtok(weights,separator)) != NULL)
    {
        weight_values[0] = weight;
        while ((weight = strtok(NULL,separator)) != NULL)
        {
            weight_values[j] = weight;
            j++;
        }
    }

    if (i != j)
    {
	if (DEBUG)
	    fprintf(fdebug,"Problem with method and weight counts[%d | %d] in the config file.\n",i,j);

        return_code = 0;
    }
    else if (!checkWeights(weight_values,j))
    {
	if (DEBUG)
	    fprintf(fdebug, "Problem with method weights.\n");

        return_code = 0;
    }
    else
    {
	method_length = i;
        return_code = 1;

	if (DEBUG)
	    fprintf(fdebug, "Methods and weights are verified.\n");
    }
    free(method);
    
    return (return_code);
} /*** end of checkMethods */

