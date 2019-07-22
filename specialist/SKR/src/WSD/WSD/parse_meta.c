const LOCAL_MAX = 100;
/*==========================================================

%SOURCE FILE
	parse_meta.c

%DESCRIPTION OF FILE
	C source file. Machine output parser

%REVISED
	06May02 kilicoglu -- Initial Version

%%
==========================================================*/

static char sccs_id_parse_meta_c[] = "@(#)parse_meta.c	1.2 11/02/06";

/*----------------------------
%INCLUDES
----------------------------*/
/* #include <debug.h> */
#include <wsd_client.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <string.h>
/*end of includes ----------*/

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
char *Process_Machine_Output(char *machineOutput, int useSuppressed);
void Parse_Utterance(char *utterance_line);

void Parse_Phrase(char *phrase_line);
void Parse_Fields(char *phrase_str);
int Get_Term_Length(char *term_str);

void Parse_Candidates(char *candidate_line);
void Parse_Candidate(char *ev_str);
void Parse_Mappings(char *map_line);
void Parse_Ambiguities();
void Free_Fields();

char *Trim_Quotes(char quote_char, char *input_text);
char *Trim_Brackets(char *input_text);

void Create_Suppressed_List();
int Is_Suppressed(char *sense);
/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
long utterance_pos = -1;
long noun_phrase_pos = -1;
int useSuppressed = 0;
char suppressed_list[100][100];
int suppressed_cnt = 0;

struct phrase_element_struct  fields;
struct candidate_struct candidate;
struct candidate_struct candidates[50000];
int candidate_cnt = 0;

xmlDocPtr doc = NULL;
xmlNodePtr xml_utterance;
xmlNodePtr xml_phrase, xml_phrase_elements, xml_phrase_element, xml_element_field;
xmlNodePtr xml_candidates, xml_candidate;
xmlNodePtr xml_mappings, xml_mapping, xml_ambiguities, xml_ambiguity;
/*end of global variables --*/

/*----------------------------
%PRIVATE STRUCTURES
----------------------------*/
/*end of private structures */

/*----------------------------
%TYPEDEFS
----------------------------*/
/*end of typedefs ----------*/

/*----------------------------
%DEBUG FLAGS
----------------------------*/
/* #define DT602  602         */ /* DT for Process_Machine_Output() */
/* #define DF603  603         */  /* DF for Process_Machine_Output() */
/* #define DT604  604         */ /* DT for Parse_Utterance() */
/* #define DF605  605         */  /* DF for Parse_Utterance() */
/* #define DT606  606         */  /* DT for Parse_Phrase() */
/* #define DF607  607         */  /* DF for Parse_Phrase() */
/* #define DT608  608         */  /* DT for Parse_Fields() */
/* #define DF609  609         */  /* DF for Parse_Fields() */
/* #define DT610  610         */ /* DT for Trim_Brackets() */
/* #define DF611  611         */   /* DF for Trim_Brackets() */
/* #define DT612  612          */ /* DT for Get_Term_Length() */
/* #define DF613  613         */  /* DF for Get_Term_Length() */
/* #define DT614  614         */  /* DT for Parse_Candidates() */
/* #define DF615  615          */ /* DF for Parse_Candidates() */
/* #define DT616  616         */  /* DT for Parse_Candidate() */
/* #define DF617  617        */   /* DF for Parse_Candidate() */
/* #define DT618  618         */  /* DT for Parse_Mappings() */
/* #define DF619  619        */   /* DF for Parse_Mappings() */
/* #define DT620  620         */  /* DT for Parse_Ambiguities() */
/* #define DF621  621        */   /* DF for Parse_Ambiguities() */
/* #define DT622  622         */  /* DT for Trim_Quotes() */
/* #define DF623  623         */  /* DF for Trim_Quotes() */
/* #define DT624  624          */ /* DT for Create_SuppressedList() */
/* #define DF625  625         */  /* DF for Creae_SuppressedList() */
/* #define DT626  626        */   /* DT for Is_Suppressed() */
/* #define DF627  627         */  /* DF for Is_Suppressed() */
/*end_of_debug_flags---------*/


/*==========================================================
%FUNCTION NAME
	Process_Machine_Output
%PURPOSE
	Main entry to the program. Parses the machine output,
	creates an xml representation of the machine output 
	and the ambiguities in the machine output
%SYNTAX
	char *Process_Machine_Output(char *machineOutput, int useSuppressed)
%RETURNS
        the machine output in XML format
%HEADER END
==========================================================*/
char *Process_Machine_Output(char *MachineOutput, int suppress)
{
   int done, len, NextCR, MachineOutputLength;
   char *OrigMachineOutput = NULL;
   xmlChar *xml_output = NULL;
   static char *parseOutput= NULL;
   char sample[LOCAL_MAX];

   done = FALSE;

   useSuppressed = suppress;
   if (useSuppressed) 
       Create_Suppressed_List();

   doc = xmlNewDoc("1.0");

   doc->children = xmlNewDocNode(doc, NULL, "machine_output", NULL);

   MachineOutputLength = strlen(MachineOutput)+1;

   if (DEBUG) {
     fprintf(fdebug, "MachineOutput is %d bytes\n\n", MachineOutputLength);
     fflush(fdebug);
   }

   OrigMachineOutput = MachineOutput;

   utterance_pos = -1;

   /* while there are valid (i.e., non-null) characters in MachineOutput */
   while (*MachineOutput)
   {
      if (DEBUG) {
	fprintf(fdebug, "At beginning of while ! done loop,");
	strncpy(sample, MachineOutput, LOCAL_MAX);
	sample[LOCAL_MAX] = '\0';
	fprintf(fdebug, "MachineOutput = >>%s<<\n", sample);
	fprintf(fdebug, "chars consumed = %d\n", MachineOutput-OrigMachineOutput);
	fprintf(fdebug, "MachineOutputLength = %d\n", MachineOutputLength);
	fflush(fdebug);
      }

      /* if next input line is empty, just advance the input pointer */
      if (MachineOutput[0] == '\n') {
	++MachineOutput;
	break;
      }

      NextCR = strchr(MachineOutput, '\n') - MachineOutput;

      MachineOutput[NextCR] = NULL;

      if (DEBUG) {
	  strncpy(sample, MachineOutput, LOCAL_MAX);
	  sample[LOCAL_MAX] = '\0';
	  fprintf(fdebug, "The machine output line (%d chars):\n>>%s<<\n", NextCR, sample);
	  fflush(fdebug);
      }

      /* utterance line, read the utterance and the utterance id */
      if (MachineOutput[0] == 'u') {
	  Parse_Utterance(MachineOutput);
      }
      /* phrase line, read the phrase, and determine the noun phrase
         position in the utterance */
      else if (MachineOutput[0] == 'p') {
	   Parse_Phrase(MachineOutput);
      }
      /* candidates line, if candidates exist, call Parse_Candidates function */
      else if (MachineOutput[0] == 'c') {
	   xml_candidates = xmlNewChild(xml_phrase,NULL,"candidates", NULL);
	   /* candidates exist */
	   if (strlen(MachineOutput) > 15) {
	      Parse_Candidates(MachineOutput);
	   }
      } /* else if */
      /* mapping line, if a mapping exists, call Parse_Mappings function */
      else if (MachineOutput[0] == 'm') {
	   xml_mappings = xmlNewChild(xml_phrase,NULL,"mappings", NULL);
	   xml_ambiguities = xmlNewChild(xml_phrase,NULL,"ambiguities",NULL);
	   /* mapping exists */
	   if (strlen(MachineOutput) > 13) {
	     Parse_Mappings(MachineOutput);
	   }
      } /* else if */
      MachineOutput += NextCR + 1;
   }/* end of while */

   if (DEBUG) {
       fprintf(fdebug, "Dumping parsed machine output to memory.\n");
   }

   xmlDocDumpFormatMemory(doc, &xml_output, &len, 1);

   if (DEBUG) {
       fprintf(fdebug,"Dumped parsed machine output to memory.\n");
   }

   parseOutput = (char *)malloc(strlen(xml_output) + 1);
   strcpy(parseOutput,"");
   strcpy(parseOutput,xml_output); 
   xmlFree(xml_output);
   xmlCleanupParser();

   if (DEBUG) {
       fprintf(fdebug, "Returning from machine output parser.\n");
   }

   if (parseOutput != NULL)
       return parseOutput;
 
   return NULL;
}


/*==========================================================
%FUNCTION NAME
	Parse_Utterance
%PURPOSE
	Parses an utterance line.
%SYNTAX
        void Parse_Utterance(char *utterance_line)
%RETURNS
%HEADER END
==========================================================*/
void Parse_Utterance(char *utterance_line)
{
   char str_utterance_pos[5];
   char *utterance;
   char ui[20];
   char *rest_of_utterance = NULL;
   char *end_of_sentence = NULL;

   utterance_pos++;
   noun_phrase_pos = -1;

   /* read UI */
   utterance = (char *)malloc(strlen(utterance_line));
   strcpy(utterance,"");
   rest_of_utterance = (char *)malloc(strlen(utterance_line));
   strcpy(rest_of_utterance,"");
   sscanf(utterance_line, "%*[^']'%[^']%*[^\"]%[^\n]", ui,rest_of_utterance);
  
   if (DEBUG) {
       fprintf(fdebug,"[Utterance UI:%s]\n",ui);
   }

   /* Handle the double-quotes in the sentence */
   end_of_sentence = strstr(rest_of_utterance,"\").");
   rest_of_utterance[strlen(rest_of_utterance)-strlen(end_of_sentence)] = '\0';
   strcpy(utterance, Trim_Quotes('"',rest_of_utterance));

   if (DEBUG) {
       fprintf(fdebug,"[Sentence:%s]\n", utterance);
   }

   /* Write utterance to XML tree */
   xml_utterance = xmlNewChild(doc->children, NULL, "utterance", NULL);
   xmlSetProp(xml_utterance, "ui", ui);
   strcpy(str_utterance_pos,"");
   sprintf(str_utterance_pos,"%d",utterance_pos);
   xmlSetProp(xml_utterance, "pos",str_utterance_pos);
   xmlSetProp(xml_utterance, "sentence", utterance);

   if (DEBUG) {
       fprintf(fdebug,"Added utterance to XML tree.\n");
   }

   free(rest_of_utterance);
}

/*==========================================================
%FUNCTION NAME
	Parse_Phrase
%PURPOSE
	Parses a phrase line.
%SYNTAX
        void Parse_Phrase(char *phrase_line)
%RETURNS
%HEADER END
==========================================================*/
void Parse_Phrase(char *phrase_line)
{
   char *phrase;
   char str_phrase_pos[5];
   char *rest_of_phrase = NULL;
   char *tmp_phrase = NULL;
   char *syntactic_unit = NULL; /* mod, head, punc, verb etc. */
   char *input_phrase_removed = NULL;
   char *input_functor_removed =  NULL;
   int syntactic_unit_len;
   int j;

   noun_phrase_pos++;

   /* First retrieve the noun phrase string */
   /* remove the "phrase(" at the beginning of the line */
   input_functor_removed = (char *)malloc(strlen(phrase_line)-6);
   strcpy(input_functor_removed,"");
   sscanf(phrase_line,"phrase(%[^\n]",input_functor_removed);

   /* inputline_functor_removed nowlooks like this: */
   /* 'noun_phrase',[other information]). */
   /* rest_of_phrase will look like: other information]). at the end of the block */
   input_phrase_removed = strstr(input_functor_removed,",[");
   rest_of_phrase = (char *)malloc(strlen(input_functor_removed));
   strcpy(rest_of_phrase,"");
   phrase = (char *)malloc(strlen(input_functor_removed));
   strcpy(phrase,"");
   sscanf(input_phrase_removed,",[%[^\n]",rest_of_phrase);

   /* Now, inputline_functor_removes looks like: 'noun_phrase' */
   /* Quotes may or may not exist, handle them if any  */
   input_functor_removed[strlen(input_functor_removed)-strlen(input_phrase_removed)] = '\0';
   if (input_functor_removed[0] == 39)
   {
	tmp_phrase = Trim_Quotes('\'',input_functor_removed);
	strcpy(phrase,tmp_phrase);
   }
   else
	strcpy(phrase,input_functor_removed);

   if (DEBUG) {
       fprintf(fdebug,"[Noun phrase:%s | Pos:%d]\n", phrase, noun_phrase_pos);
   }
 
   /* add the phrase information to the tree */
   xml_phrase = xmlNewChild(xml_utterance, NULL, "phrase", NULL);
   strcpy(str_phrase_pos,"");
   sprintf(str_phrase_pos,"%d",noun_phrase_pos);
   xmlSetProp(xml_phrase, "pos",str_phrase_pos);
   xmlSetProp(xml_phrase, "noun_phrase", phrase);

   if (DEBUG) {
       fprintf(fdebug,"Added phrase to XML tree.\n");
   }

   xml_phrase_elements = xmlNewChild(xml_phrase,NULL,"phrase_elements",NULL);

   /* search for syntactic units (mod, head etc.) in the rest_of_phrase and analyze them */
   if (DEBUG) {
       fprintf(fdebug,"The rest of phrase: %s\n", rest_of_phrase);
   }

   while (strcmp(rest_of_phrase,")."))
   {
       /* Find the length of the first syntactic unit in the rest of phrase */
       syntactic_unit_len = Get_Term_Length(rest_of_phrase)+1;
       syntactic_unit = (char *)malloc(syntactic_unit_len+1);
       strcpy(syntactic_unit,"");
       strncpy(syntactic_unit,rest_of_phrase,syntactic_unit_len);
       syntactic_unit[syntactic_unit_len] = '\0';

       if (DEBUG) {
	   fprintf(fdebug,"Syntactic unit[%d]: %s\n", strlen(syntactic_unit), syntactic_unit);
       }

       /* Now we have the syntactic unit, we will analyze its fields */ 
       /* e.g. for a "head", these fields would be lexmatch, inputmatch,tag and tokens */
       /* fields = Parse_Fields(syntactic_unit); */
       Parse_Fields(syntactic_unit);

       /* Add the fields to the XML tree */
       xml_phrase_element = xmlNewChild(xml_phrase_elements,NULL,"phrase_element",NULL);
       xmlSetProp(xml_phrase_element,"type",fields.type);
       for (j=0; j < fields.field_cnt; j++)
       {
	   xml_element_field = xmlNewChild(xml_phrase_element,NULL,"field",NULL);
	   xmlSetProp(xml_element_field,"name",fields.name[j]);
	   xmlSetProp(xml_element_field,"value",fields.value[j]);
	   if (DEBUG) {
	       fprintf(fdebug, "[Field type: %s | Name:%s | Value:%s]\n",
		       	      fields.type, fields.name[j], fields.value[j]);
	   }
       }

       strcpy(rest_of_phrase, rest_of_phrase + syntactic_unit_len + 1);
       free(syntactic_unit);
       Free_Fields();
   }

   if (DEBUG) {
       fprintf(fdebug,"Added phrase fields to XML tree.\n");
   }

   /* free memory */
   free(rest_of_phrase);
   free(input_functor_removed);
}

/*==========================================================
%FUNCTION NAME
	Parse_Fields
%PURPOSE
	Parses the elements of a syntactic unit. A syntactic unit
	is a Prolog term. An example: 
	head([lexmatch([cold],inputmatch(['Cold']),tag(adj),tokens([cold])])
	Different syntactic units may have different number of fields.
%SYNTAX
        void Parse_Fields(char *str)
%RETURNS
        the phrase_element_struct representation of a syntactic unit.
%HEADER END
==========================================================*/
void Parse_Fields(char *str)
{
    char *type = NULL;
    char *rest_of_string = NULL;
    char *element = NULL;
    char *name = NULL;
    char *temp_value = NULL;
    char *trimmed_temp_value = NULL;
    char *value = NULL;
    int i = 0;
    int k;
    int element_len;

    rest_of_string = (char *)malloc(strlen(str));
    strcpy(rest_of_string,"");
    type = (char *)malloc(strlen(str));
    strcpy(type,"");
    sscanf(str,"%[^(]([%[^\n]", type, rest_of_string);
    fields.type = (char *)malloc(strlen(type) + 2);
    strcpy(fields.type, type);

    /* Find the name and value pairs for each term */
    /* e.g. name="lexmatch" value="cold" */ 
    while (strcmp(rest_of_string,")"))
    {
	/* Get the length of the first field from the rest of string */
	element_len = Get_Term_Length(rest_of_string) + 1;
	element = (char *)malloc(element_len+1);
	strcpy(element,"");
	strncpy(element,rest_of_string,element_len);
	element[element_len] = '\0';

	if (DEBUG) {
	    fprintf(fdebug,"The phrase element[%d]: %s\n", strlen(element), element);
	}

	/* Get the name and value of the field */
	name = (char *)malloc(strlen(element)); 
	strcpy(name,"");
	temp_value = (char *)malloc(strlen(element)); 
        strcpy(temp_value,"");
        sscanf(element,"%[^(]%[^\n]", name, temp_value);
        if (temp_value[0] == '[' || temp_value[0] == '(')
	{
	    trimmed_temp_value = Trim_Brackets(temp_value);
	    value = Trim_Quotes('\'',trimmed_temp_value);
        }
	else
	{
	    value = Trim_Quotes('\'',temp_value);
	}

	/* Add the name and value to the phrase_element struct */
	fields.name[i] = (char *)malloc(strlen(name) + 2); 
	strcpy(fields.name[i],"");
        strcpy(fields.name[i],name);
	fields.value[i] = (char *)malloc(strlen(value) + 2); 
	strcpy(fields.value[i],"");
	strcpy(fields.value[i],value);
	strcpy(rest_of_string, rest_of_string + element_len + 1);
        free(element);
	free(name);
	free(temp_value);
	i++;
    }
    free(type);
    free(rest_of_string);
    fields.field_cnt = i;
}

/*==========================================================
%FUNCTION NAME
	Trim_Brackets
%PURPOSE
	Trims the brackets -- [], () -- around Prolog terms.
%SYNTAX
        char *Trim_Brackets(char *str)
%RETURNS
        the stripped input
%HEADER END
==========================================================*/
char *Trim_Brackets(char *str)
{
    static char trimmed_str[MAX_UMLS_STRING + 1];
    int i,k;
    int j=0;

    if (DEBUG) {
	fprintf(fdebug,"The input string: %s\n", str);
    }

    strcpy(trimmed_str,"");

    if (str[0] == '(')
    {
	if (str[1] == '[')
	{
	    i = 2;
	    k = strlen(str) -2;
	}
	else
        {
	    i = 1;
	    k = strlen(str) - 1;
	}
	while (i < k)
	    trimmed_str[j++] = str[i++];
    }

    trimmed_str[j] = '\0';

    if (DEBUG) {
	fprintf(fdebug,"The output string: %s\n", trimmed_str);
    }

    return trimmed_str;
}


/*==========================================================
%FUNCTION NAME
	Get_Term_Length
%PURPOSE
	Returns the length of a Prolog term. Does this by counting
	the number of parentheses and brackets in a string.
%SYNTAX
        int Get_Term_Length(char *str)
%RETURNS
        the length of the Prolog term.
%HEADER END
==========================================================*/
int Get_Term_Length(char *str)
{
   int i = 0;
   int brackets = 0;
   int parentheses = 0;

   while (str[i] != '\0')
   {
      if (str[i] == '[')
      {
          if (str[i-1] != '\'' || str[i+1] != '\'')
	      brackets++;
      }
      else if (str[i] == '(')
      {
          if (str[i-1] != '\'' || str[i+1] != '\'')
	      parentheses++;
      }
      else if (str[i] == ')')
      {
	  if (str[i-1] != '\'' || str[i+1] != '\'')
	  {
	      parentheses--;
            if (brackets == 0 && parentheses == 0)
                  break;
	  }
      }
      else if (str[i] == ']')
      {
          if (str[i-1] != '\'' || str[i+1] != '\'')
	      brackets--;
      }
      i++;
   }
   
   if (DEBUG) {
       fprintf(fdebug,"The term[%d]: %s\n", i,str);
   }

    return i;
}


/*==========================================================
%FUNCTION NAME
	Parse_Candidates
%PURPOSE
	Parses a candidates line.
%SYNTAX
        void Parse_Candidates(char *candidate_line)
%RETURNS
%HEADER END
==========================================================*/
void Parse_Candidates(char *candidate_line)
{
    char *ev_str = NULL;
    char *ev_str_score_removed;
    char *rest_of_candidates;
    int i;

    while(strcmp(candidate_line,""))
    {
	/* Retrieve the candidate (ev_str) */
	ev_str = strstr(candidate_line,"ev(");

	ev_str_score_removed = (char *)malloc(strlen(candidate_line));
	strcpy(ev_str_score_removed,"");

	sscanf(ev_str,"ev(%*[^,],%[^\n]", ev_str_score_removed);

	rest_of_candidates = (char *)malloc(strlen(candidate_line));
	strcpy(rest_of_candidates,"");

	/* If there are more ev_str's */
	if (strstr(ev_str_score_removed,"ev(") != 0) {
	    strcpy(rest_of_candidates,strstr(ev_str_score_removed,"ev("));
	    ev_str[strlen(ev_str)-strlen(rest_of_candidates)] = '\0';
	}
	else
	    strcpy(rest_of_candidates,"");

	if (DEBUG) {
	    fprintf(fdebug,"The ev string: %s\n", ev_str);
	}

	Parse_Candidate(ev_str);
	xml_candidate = xmlNewChild(xml_candidates,NULL,"candidate",NULL);
	xmlSetProp(xml_candidate,"score", candidate.score);
	xmlSetProp(xml_candidate,"cui", candidate.cui);
	xmlSetProp(xml_candidate,"umls_concept",candidate.umls_concept);
	xmlSetProp(xml_candidate,"preferred_name",candidate.preferred_name);
	xmlSetProp(xml_candidate,"matched_words",candidate.matched_words);
	xmlSetProp(xml_candidate,"semtypes",candidate.semtypes);
	xmlSetProp(xml_candidate,"matchmap",candidate.matchmap);
	xmlSetProp(xml_candidate,"head_flag", candidate.head_flag);
	xmlSetProp(xml_candidate,"overmatch_flag",candidate.overmatch_flag);

	if (DEBUG) {
	    fprintf(fdebug,"Added candidate to XML tree.\n");
	}

	strcpy(candidate_line, rest_of_candidates); 
    }
}

/*==========================================================
%FUNCTION NAME
	Parse_Candidate
%PURPOSE
	Parses a candidate string. An example:
        ev(-872,'C0009264','Cold <1>','cold temperature',[cold],[npop],[[[1,1],[1,1],0]],yes,no)
	Note: CUI field is currently optional.
%SYNTAX
        void Parse_Candidate(char *candidate_str)
%RETURNS
        the candidate_struct representation of a ev string.
%HEADER END
==========================================================*/
void Parse_Candidate(char *candidate_str)
{
    char *rest;
    char score[10], cui[10], temp_cui[10];
    char matchmap[1024], head_flag[4], overmatch_flag[4];
    char matched_words[MAX_UMLS_STRING + 1], semtypes[1024];
    char preferred_name[MAX_UMLS_STRING + 1], umls_concept[MAX_UMLS_STRING + 1];
    char temp_concept_combo[MAX_UMLS_STRING + 1], concept_combo[MAX_UMLS_STRING + 1];
    char end_of_umls_concept[MAX_UMLS_STRING + 1], temp_preferred_name[MAX_UMLS_STRING + 1];

    rest = (char *)malloc(strlen(candidate_str));
    strcpy(rest,"");
    strcpy(score,"");
    strcpy(temp_cui,"");
    strcpy(cui,"");
    strcpy(matchmap,"");
    strcpy(head_flag,"");
    strcpy(overmatch_flag,"");
    strcpy(matched_words,"");
    strcpy(semtypes,"");
    strcpy(preferred_name,"");
    strcpy(umls_concept,"");
    strcpy(temp_concept_combo,"");
    strcpy(concept_combo,"");
    strcpy(end_of_umls_concept,"");
    strcpy(temp_preferred_name,"");

    sscanf(candidate_str,"ev(%[^,],%*[^\n]",score); 
    strcpy(rest,strstr(candidate_str,",["));
    sscanf(rest,",[%[^]]%*[^[][%[^]]],%[^yn]%[^,],%[^)]", matched_words, semtypes, matchmap, head_flag, overmatch_flag);
    matchmap[strlen(matchmap)-1] = '\0';
   
    candidate_str[strlen(candidate_str)- strlen(rest)] = '\0';
    sscanf(candidate_str,"%*[^,],%[^\n]",temp_concept_combo);

    /* Handle the concept id if any, a concept id looks like CXXXXXXX, where X's are numbers  */
    sscanf(temp_concept_combo,"'C%[0-9]',%[^\n]", temp_cui,concept_combo);
    if (strlen(temp_cui) != 7 )
        strcpy(concept_combo, temp_concept_combo);
    else
    {
	strcpy(cui,""); strcpy(cui,"C");
	strcat(cui,temp_cui);
    }

    /* If both umls concept and preferred name has quotes */
    if (concept_combo[0] == '\'' && concept_combo[strlen(concept_combo)-1] == '\'' )
    {
        strcpy(end_of_umls_concept,strstr(concept_combo,"','"));
        sscanf(end_of_umls_concept,"',%[^\n]",temp_preferred_name);

        concept_combo[strlen(concept_combo)-strlen(temp_preferred_name)-1] = '\0';
        strcpy(umls_concept, Trim_Quotes('\'',concept_combo));
        strcpy(preferred_name, Trim_Quotes('\'',temp_preferred_name));
    }
    /* if only umls concept has quotes */
    else if(concept_combo[0] == '\'' ) {
        strcpy(end_of_umls_concept,strstr(concept_combo,"',"));
        sscanf(end_of_umls_concept,"',%[^\n]",preferred_name);
        concept_combo[strlen(concept_combo)-strlen(preferred_name)-1] = '\0';
        strcpy(umls_concept, Trim_Quotes('\'',concept_combo));
    }
    /* if only preferred name has quotes */
    else if(concept_combo[strlen(concept_combo)-1] == '\'' ) {
        sscanf(concept_combo,"%[^,],%[^\n]",umls_concept, temp_preferred_name);
        strcpy(preferred_name, Trim_Quotes('\'',temp_preferred_name));
    }
    /* neither has quotes */
    else
	sscanf(concept_combo,"%[^,],%[^\n]",umls_concept,preferred_name);

    strcpy(candidate.score,"");  strcpy(candidate.score,score);
    strcpy(candidate.cui,"");  strcpy(candidate.cui,cui);
    strcpy(candidate.umls_concept,"");  strcpy(candidate.umls_concept,umls_concept);
    strcpy(candidate.preferred_name,"");  strcpy(candidate.preferred_name,preferred_name);
    strcpy(candidate.matched_words,"");  strcpy(candidate.matched_words,matched_words);
    strcpy(candidate.semtypes,"");  strcpy(candidate.semtypes,semtypes);
    strcpy(candidate.matchmap,"");   strcpy(candidate.matchmap,matchmap);
    strcpy(candidate.head_flag,"");   strcpy(candidate.head_flag,head_flag);
    strcpy(candidate.overmatch_flag,"");  strcpy(candidate.overmatch_flag,overmatch_flag);

    if (DEBUG)
    {
	fprintf(fdebug,
		"Candidate elements: [Score:%s | CUI:%s | UMLS Concept:%s | Preferred Name:%s | Wordlist:%s | Semantic Types:%s | Matchmap:%s | Head Flag:%s | Overmatch Flag:%s]\n",
		candidate.score, candidate.cui, candidate.umls_concept, candidate.preferred_name, 
		candidate.matched_words, candidate.semtypes, candidate.matchmap, candidate.head_flag,candidate.overmatch_flag); 
    }

}

/*==========================================================
%FUNCTION NAME
	Parse_Mappings
%PURPOSE
	Parses a mappings line. 
%SYNTAX
        void Parse_Mappings(char *map_line)
%RETURNS
%HEADER END
==========================================================*/
void Parse_Mappings(char *map_line)
{
    char *map_str = NULL;
    char *map_str_score_removed;
    char *rest_of_mappings;
    char *ev_str = NULL;
    char *ev_str_score_removed;
    char *rest_of_candidates;
    char map_score[10];
    int mapping_cnt=0;
    int ambiguity = FALSE;
    int i = 0;
    char map_str_save_char;
    char ev_str_save_char;
    int outer_count = 0;
    int inner_count = 0;
    char sample[LOCAL_MAX];


    map_str_score_removed = (char *)malloc(strlen(map_line) + 1);
    /* rest_of_mappings      = (char *)malloc(strlen(map_line) + 1); */
    ev_str_score_removed  = (char *)malloc(strlen(map_line) + 1);
    /* rest_of_candidates    = (char *)malloc(strlen(map_line) + 1); */

    /* OUTER while */
    while(*map_line) {
      if (DEBUG) {
	outer_count = 0;
	inner_count = 0;
	fprintf(fdebug, "\n[%d.%d] top of OUTER while loop\n", ++outer_count, inner_count);
	fflush(fdebug);

	strncpy(sample, map_line, LOCAL_MAX);
	sample[LOCAL_MAX] = '\0';
	fprintf(fdebug, "\n[%d.%d] map_line (%d chars; pos %d ):\n>>%s<<\n",
		++outer_count, inner_count, strlen(map_line), map_line-map_line, sample);
	fflush(fdebug);
      }

       /* Retrieve the mapping (map_str) */
       map_str = strstr(map_line,"map(");
       sscanf(map_str,"map(%[^,],[%[^\n]", map_score, map_str_score_removed);

       if (DEBUG) {
	 strncpy(sample, map_str_score_removed, LOCAL_MAX);
	 sample[LOCAL_MAX] = '\0';
	 fprintf(fdebug, "\n[%d.%d] map_str_score_removed (%d chars; pos + %d ):\n>>%s<<\n",
		 ++outer_count, inner_count, strlen(map_str_score_removed),
		 map_str_score_removed-map_line, sample);
	 fflush(fdebug);
       }
       mapping_cnt++;

 	 /* If there are more map_str's -- ambiguity exists*/
       if (strstr(map_str_score_removed,"map(") != 0) {
	 /*  ???????? */
	 /* strcpy(rest_of_mappings,strstr(map_str_score_removed,"map(")); */

	 rest_of_mappings = strstr(map_str_score_removed,"map(");

	 if (DEBUG) {
	   strncpy(sample, rest_of_mappings, LOCAL_MAX);
	   sample[LOCAL_MAX] = '\0';
	   fprintf(fdebug, "\n[%d.%d] rest_of_mappings (%d chars; pos + %d ):\n>>%s<<\n",
		   ++outer_count, inner_count, strlen(rest_of_mappings), rest_of_mappings-map_line, sample);
	   fflush(fdebug);
	 }

	 map_str_save_char = map_str[strlen(map_str)-strlen(rest_of_mappings)];

	 if (DEBUG) {
	   fprintf(fdebug, "\n[%d.%d] nulling out char %d (pos %d ) of map_str: %c\n",
		   ++outer_count, inner_count, strlen(map_str)-strlen(rest_of_mappings),
		   map_str+strlen(map_str)-strlen(rest_of_mappings),
		   map_str_save_char);
	   fflush(fdebug);
	 }

	 map_str[strlen(map_str)-strlen(rest_of_mappings)] = '\0';
	 if (mapping_cnt == 1) ambiguity = TRUE;
       }
       else {
	     rest_of_mappings = "";
       }

	xml_mapping = xmlNewChild(xml_mappings,NULL,"mapping",NULL);
	xmlSetProp(xml_mapping,"score",map_score); 

	if (DEBUG) {
	    fprintf(fdebug,"[Mapping string:%s | Score:%s]\n",map_str,map_score);
	}

	/* INNER while */
	while (*map_str) { 
	    inner_count = 0;
	    ev_str = strstr(map_str,"ev(");
	    sscanf(ev_str,"ev(%*[^,],%[^\n]",ev_str_score_removed);

	    if (DEBUG) {
	      strncpy(sample, ev_str, LOCAL_MAX);
	      sample[LOCAL_MAX] = '\0';
	      fprintf(fdebug, "\n[%d.%d] ev_str (%d chars; pos + %d ):\n>>%s<<\n",
		      outer_count, ++inner_count, strlen(ev_str), ev_str-map_line, sample);
	      strncpy(sample, ev_str_score_removed, LOCAL_MAX);
	      sample[LOCAL_MAX] = '\0';
	      fprintf(fdebug, "\n[%d.%d] ev_str_score_removed (%d chars; pos + %d ):\n>>%s<<\n",
		      outer_count, ++inner_count, strlen(ev_str_score_removed),
		      ev_str_score_removed-map_line, sample);
	      fflush(fdebug);
	    }
	    if (strstr(ev_str_score_removed,"ev(") != 0) {
	      /* ??????????  */
	      /* strcpy(rest_of_candidates,strstr(ev_str_score_removed,"ev(")); */
	    
	       rest_of_candidates = strstr(ev_str_score_removed,"ev(");

	       if (DEBUG) {
		 strncpy(sample, rest_of_candidates, LOCAL_MAX);
		 sample[LOCAL_MAX] = '\0';
		 fprintf(fdebug, "\n[%d.%d] rest_of_candidates (%d chars; pos + %d ):\n>>%s<<\n",
			 outer_count, ++inner_count, strlen(rest_of_candidates),
			 rest_of_candidates-map_line, sample);
		 fflush(fdebug);
	       }

	       ev_str_save_char = ev_str[strlen(ev_str)-strlen(rest_of_candidates)];

	       if (DEBUG) {
		 fprintf(fdebug, "\n[%d.%d] nulling out char %d (pos %d ) of ev_str: %c\n",
			 outer_count, ++inner_count, strlen(ev_str)-strlen(rest_of_candidates),
			 ev_str+strlen(ev_str)-strlen(rest_of_candidates),
			 ev_str_save_char);
		 fflush(fdebug);
	       }

               ev_str[strlen(ev_str)-strlen(rest_of_candidates)] = '\0';
	    }
	    else {
		  rest_of_candidates = "";

		  if (DEBUG) {
		    fprintf(fdebug, "\n[%d.%d] Set rest_of_candidates to NULL\n",
			    outer_count, ++inner_count);
		    fprintf(fdebug, "rest_of_candidates: %s\n", rest_of_candidates);
		    fflush(fdebug);
		  }
	    }

	    if (DEBUG) {
		fprintf(fdebug,"Ev str: %s\n",ev_str);
		fprintf(fdebug, "\n[%d.%d] ev_str BEFORE P_C (%d chars; pos + %d ):\n>>%s<<\n",
			outer_count, ++inner_count, strlen(ev_str), ev_str-map_line, ev_str);
	    }		    	    
	    Parse_Candidate(ev_str);

	    if (DEBUG) {
	      fprintf(fdebug, "\n[%d.%d] ev_str AFTER P_C (%d chars; pos + %d ):\n>>%s<<\n",
		      outer_count, ++inner_count, strlen(ev_str), ev_str-map_line, ev_str);
	    }

	    xml_candidate = xmlNewChild(xml_mapping,NULL,"candidate",NULL);
	    xmlSetProp(xml_candidate, "score",          candidate.score);
	    xmlSetProp(xml_candidate, "cui",            candidate.cui);
	    xmlSetProp(xml_candidate, "umls_concept",   candidate.umls_concept);
	    xmlSetProp(xml_candidate, "preferred_name", candidate.preferred_name);
	    xmlSetProp(xml_candidate, "matched_words",  candidate.matched_words);
	    xmlSetProp(xml_candidate, "semtypes",       candidate.semtypes);
	    xmlSetProp(xml_candidate, "matchmap",       candidate.matchmap);
	    xmlSetProp(xml_candidate, "head_flag",      candidate.head_flag);
	    xmlSetProp(xml_candidate, "overmatch_flag", candidate.overmatch_flag);

	    if (DEBUG) {
		fprintf(fdebug, "Added mapping candidate to XML tree.\n");
	    }

	  /* If ambiguities exists in the mappings, then create a list of candidates  */
	  /* that will be used by Parse_Ambiguities to determines the ambiguous words */
	  if (ambiguity) {

	      strcpy(candidates[i].score,"");	      strcpy(candidates[i].score, candidate.score);
	      strcpy(candidates[i].cui,"");	      strcpy(candidates[i].cui, candidate.cui);
	      strcpy(candidates[i].umls_concept,"");  strcpy(candidates[i].umls_concept, candidate.umls_concept);
	      strcpy(candidates[i].preferred_name,"");strcpy(candidates[i].preferred_name, candidate.preferred_name);
	      strcpy(candidates[i].matched_words,""); strcpy(candidates[i].matched_words, candidate.matched_words);
	      strcpy(candidates[i].semtypes,"");      strcpy(candidates[i].semtypes, candidate.semtypes);
	      strcpy(candidates[i].matchmap,"");      strcpy(candidates[i].matchmap, candidate.matchmap);
	      strcpy(candidates[i].head_flag,"");     strcpy(candidates[i].head_flag, candidate.head_flag);
	      strcpy(candidates[i].overmatch_flag,"");strcpy(candidates[i].overmatch_flag, candidate.overmatch_flag);
	      candidates[i].checked = FALSE;
	      i++;

	  } /* if (ambiguity) */
	  strcpy(map_str,rest_of_candidates);
	} /* INNER while (strcmp(map_str,"")) */
	strcpy(map_line, rest_of_mappings);
    } /* OUTER while(strcmp(map_line,"")) */
    if (ambiguity)
    {
      if (DEBUG) {
	    fprintf(fdebug,"Ambiguity exists.\n");
      }
	candidate_cnt = i;
	Parse_Ambiguities();
    }
}

/*==========================================================
%FUNCTION NAME
	Parse_Ambiguities
%PURPOSE
	Determines the ambiguities from a list of candidates. 
%SYNTAX
        void Parse_Ambiguities()
%RETURNS
%HEADER END
==========================================================*/
void Parse_Ambiguities()
{
    int i,j,l;
    int k=0; 
    int inList = FALSE;
    struct candidate_struct ambiguity[100];

    for (i=0; i < candidate_cnt; i++)
    {
	if (useSuppressed && Is_Suppressed(candidates[i].umls_concept))
	    candidates[i].checked = TRUE;
    }

    for (i = 0; i < candidate_cnt; i++)
    {
	if (!candidates[i].checked)
	{
	    k = 0;
	    for (j = i+1; j < candidate_cnt; j++)
	    {
		if (!candidates[j].checked)
		{
		    /* Two ambiguous candidates are found */
		    if (!strcmp(candidates[j].score,candidates[i].score) &&
			!strcmp(candidates[j].matchmap,candidates[i].matchmap) &&
			!strcmp(candidates[j].head_flag,candidates[i].head_flag) &&
			!strcmp(candidates[j].overmatch_flag,candidates[i].overmatch_flag) &&
			(strcmp(candidates[j].umls_concept,candidates[i].umls_concept) ||
			 strcmp(candidates[j].preferred_name,candidates[i].preferred_name) ||
			 strcmp(candidates[j].semtypes,candidates[i].semtypes)))
		    {
			
			    /* Add the first candidate, if it is not already added */
			    if (k == 0)
			    {
				xml_ambiguity = xmlNewChild(xml_ambiguities,NULL,"ambiguity",NULL);
				xmlSetProp(xml_ambiguity,"process","yes");
				xml_candidate = xmlNewChild(xml_ambiguity,NULL,"candidate",NULL);
				xmlSetProp(xml_candidate,"score",candidates[i].score);
				xmlSetProp(xml_candidate,"cui",candidates[i].cui);
				xmlSetProp(xml_candidate,"umls_concept",candidates[i].umls_concept);
				xmlSetProp(xml_candidate,"preferred_name",candidates[i].preferred_name);
				xmlSetProp(xml_candidate,"matched_words",candidates[i].matched_words);
				xmlSetProp(xml_candidate,"semtypes",candidates[i].semtypes);
				xmlSetProp(xml_candidate,"matchmap",candidates[i].matchmap);
				xmlSetProp(xml_candidate,"head_flag",candidates[i].head_flag);
				xmlSetProp(xml_candidate,"overmatch_flag",candidates[i].overmatch_flag);

				if (DEBUG) {
				    fprintf(fdebug,
					    "Ambiguity elements: [Score:%s | CUI:%s | UMLS Concept:%s | Preferred Name:%s | Wordlist:%s | Semantic Types:%s | Matchmap:%s | Head Flag:%s | Overmatch Flag:%s]\n",
					    candidates[i].score, candidates[i].cui, candidates[i].umls_concept, candidates[i].preferred_name, 
					    candidates[i].matched_words, candidates[i].semtypes, candidates[i].matchmap, candidates[i].head_flag,candidates[i].overmatch_flag); 
				}

				ambiguity[k] = candidates[i];
				k++;

			    }

			    inList = FALSE;
			    /* Check the existing ambiguity list for the first candidate */
			    /* If second candidate is not found in the list, we can add it as a candidate to XML tree */
			    for (l=0; l < k; l++)
			    {
				/* Check the ambiguity list */
				if (!strcmp(ambiguity[l].umls_concept,candidates[j].umls_concept) &&
				    !strcmp(ambiguity[l].preferred_name,candidates[j].preferred_name) &&
				    !strcmp(ambiguity[l].matched_words,candidates[j].matched_words))
				{
				    inList = TRUE;
				    break;
				}
			    }
			    /* If not found in the ambiguity list, add */
			    if (!inList)
			    {
				xml_candidate = xmlNewChild(xml_ambiguity,NULL,"candidate",NULL);
				xmlSetProp(xml_candidate,"score",candidates[j].score);
				xmlSetProp(xml_candidate,"cui",candidates[j].cui);
				xmlSetProp(xml_candidate,"umls_concept",candidates[j].umls_concept);
				xmlSetProp(xml_candidate,"preferred_name",candidates[j].preferred_name);
				xmlSetProp(xml_candidate,"matched_words",candidates[j].matched_words);
				xmlSetProp(xml_candidate,"semtypes",candidates[j].semtypes);
				xmlSetProp(xml_candidate,"matchmap",candidates[j].matchmap);
				xmlSetProp(xml_candidate,"head_flag",candidates[j].head_flag);
				xmlSetProp(xml_candidate,"overmatch_flag",candidates[j].overmatch_flag);

				if (DEBUG) {
				    fprintf(fdebug,
					    "Ambiguity elements: [Score:%s | CUI:%s | UMLS Concept:%s | Preferred Name:%s | Wordlist:%s | Semantic Types:%s | Matchmap:%s | Head Flag:%s | Overmatch Flag:%s]\n",
					    candidates[i].score, candidates[i].cui, candidates[i].umls_concept, candidates[i].preferred_name, 
					    candidates[i].matched_words, candidates[i].semtypes, candidates[i].matchmap, candidates[i].head_flag,candidates[i].overmatch_flag); 
				}

				ambiguity[k] = candidates[j];
				k++;
			    }
			candidates[j].checked = TRUE;
		    } /* if (two ambiguous candidates) */
		}
	    } /* for j */

  	    candidates[i].checked = TRUE;
		
	}
    } /* for i */
}


/*==========================================================
%FUNCTION NAME
	Trim_Quotes
%PURPOSE
	Removes the quotes in a Prolog atom.
%SYNTAX
        char *Trim_Quotes(char quote_char, char *input_text)
%RETURNS
        the input with the quotes removed.
%HEADER END
==========================================================*/
char *Trim_Quotes(char quote_char, char *input_text)
{
      int i=0;
      int j=0;
      int prev_char_quote = 1;
      static char trimmed_text[MAX_UMLS_STRING + 1];
      
      if (DEBUG) {
	  fprintf(fdebug,"Input String: %s\n", input_text);
      }

      strcpy(trimmed_text,"");

      /* Check each character and convert the atom to a string. */
      while (input_text[i] != '\0') {
	  if (input_text[i] == quote_char) {
	      if (i == 0 || 
		  i == strlen(input_text) -1 ||
		  input_text[i+1] == ',' || 
		  input_text[i-1] == ',' ) {
	          /* ignore */
	      }		      
	      else {
		  if (prev_char_quote) {
		      trimmed_text[j] = input_text[i];
                      j++;
		      prev_char_quote = 0;
		  }
		  else {
		      prev_char_quote = 1;
		  }
	      }
	  }
	  else
	  {
	      trimmed_text[j] = input_text[i];
	      j++;
	      prev_char_quote = 1;
	  }
	  i++;
      }
      trimmed_text[j] = '\0';

      if (DEBUG) {
	  fprintf(fdebug,"Output string: %s\n",trimmed_text);
      }

      return trimmed_text;
}

/*==========================================================
%FUNCTION NAME
	Create_Suppressed_List
%PURPOSE
        Creates an array of suppressed concepts by reading
	from a file.
%SYNTAX
        void Create_Suppressed_List() 
%RETURNS
%HEADER END
==========================================================*/
void Create_Suppressed_List()
{
    int done = FALSE;
    int i = 0;
    FILE *f_suppressed;
    int found = FALSE;
    char *suppressed_conc_file;
    char suppressed_line[100];
    char suppressed_concept[100];

    if ( (suppressed_conc_file = getenv("SUPPRESSED_CONC_FILE")) == NULL ) {
	fprintf(stderr, "Cannot getenv  SUPPRESSED_CONC_FILE\n");
	return;
    }

    f_suppressed = fopen(suppressed_conc_file, "r");

    while (!done)
    {
        if(fgets(suppressed_line, 100, f_suppressed) == (char *)NULL)
            done = TRUE;
        else
        {
            sscanf(suppressed_line, "%[^\n]\n", suppressed_concept);
	    strcpy(suppressed_list[i], suppressed_concept);
	    i++;
	}
    }
    suppressed_cnt = i+1;
}

/*==========================================================
%FUNCTION NAME
	Is_Suppressed
%PURPOSE
	Finds out if a word sense is suppressed in the Test Collection
%SYNTAX
        int Is_Suppressed(char *sense)
%RETURNS
        1 if suppressed
	0 if not suppressed
%HEADER END
==========================================================*/
int Is_Suppressed(char *sense)
{
    int found = FALSE;
    int i;

    for (i=0; i< suppressed_cnt; i++)
    {
        if (strcmp(sense,suppressed_list[i]) == 0)
        {
            found = TRUE;
            break;
        }
    }

    return found;
}

/*==========================================================
%FUNCTION NAME
	Free_Fields
%PURPOSE
	Free the memory allocated by Parse_Fields()
%SYNTAX
        void Free_Fields()
%RETURNS
%HEADER END
==========================================================*/
void Free_Fields()
{
    int i;

    free(fields.type);
    for (i = 0; i < fields.field_cnt; i++)
    {
	free(fields.name[i]);
	free(fields.value[i]);
    }

}
