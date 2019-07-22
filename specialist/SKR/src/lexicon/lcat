/*==========================================================

%SOURCE FILE
	lcat.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	2Aug94 divita -- Initial Version

%%
==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include <stdio.h>
#include <debug.h>
#include <lexicon.h>

/*----------------------------
%SCCS Version Flag
----------------------------*/
static char sccs_id[] = "@(#)lcat.c	1.4	09 Feb 1995";

/*----------------------------
%STATIC FUNCTIONS
----------------------------*/
#define MAXCOPT 10

/*----------------------------
%External FUNCTIONS
----------------------------*/
extern int getopt();

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int main(
	 int	argc,
	 char *argv[] 
	 );
     
void usage(char option);
/*--------PROTOTYPES--------*/



/*----------------------------
%DEBUG FLAGS
----------------------------*/

#define DT118  118          /* DT for main() */
#define DF119  119          /* DF for main() */
#define DT124  124          /* DT for usage() */
#define DF125  125          /* DF for usage() */
/*-------DEBUG_FLAGS--------*/
/**/
/*==========================================================
%FUNCTION NAME
	main
%SCOPE
	?public | private | static
%PURPOSE
	?
%SYNTAX INCLUDES
	#include "?"
%EXAMPLE CALL
	?ret_val = main(arg);
%RETURNS
	?
%METHOD

%FILES
	Lexicon
	Lexicon Index
	Input file
	Temporary File
%TABLES
	None
%FLAGS  
	TRACE DT118
	FULL  DF119
%SYNTAX ARGS
==========================================================*/
int main(
	 int	argc,
	 char *argv[] 
	 )
     
{
  int   return_code = 1; 
  char  *msg = NULL;
  char lexicon_file_name[MAXLINE]; 
  char btree_index_files[MAXLINE];
  char recFile[MAXLINE];
  FILE *recFp = NULL;
  char cfg_name[40];
  char version[20];
  int done = D_FALSE;
  char c;
  char record[MAXLINE];

  int  popt=PRINT_RECORD;		/* output option */
  char topt = DEFAULT_FIELD_SEPARATOR;
  int copt[MAXCOPT];
  int ncopt = 0;			/* number of cat options specified. */

  char *cp = NULL;

  extern char *optarg;
  extern int opotind, opterr, optopt;

  debug_init(__FILE__);
  
  dfname("main");
  DENTER(DT118);
  

  strcpy(version,"2009"); 
  /*strcpy(version,"Current"); */


  /* ----------------------------------------------------------- 
     Get Program Options
     -----------------------------------------------------------  */
  DPR(DF119,"Get Program Options");
  while ((c=getopt(argc, argv, "p:l:i:c:t:h:v:")) != (-1)) {
    switch (c) {
    case 'l': strcpy(lexicon_file_name, optarg);   break;
      
    case 'i': strcpy(btree_index_files, optarg); break;
      
    case 'p':
      if (strlen(optarg) != 1) {
	usage('p');
	return_code = -1;
	goto bottom;
      }

      switch (optarg[0]) {
      case 'r':
	popt = PRINT_RECORD;
	break;
      case 'b':
	popt = PRINT_BASE;
	break;
      case 's':
	popt = PRINT_BASESV;
	break;
      case 'k':
	popt = PRINT_KEY;
	break;
      case 'i':
	popt = PRINT_INDEX;
	break;
      default:
	usage('p');
	return_code = -1;
	goto bottom;
      }
      break;

    case 'c':
      for (cp = optarg; *cp != EOS; cp++) {

	switch (*cp) {
	case 'a':
	  copt[ncopt] |= LEX_CAT_ADJ;
	  break;
	case 'b':
	  copt[ncopt] |= LEX_CAT_ADV;
	  break;
	case 'x':
	  copt[ncopt] |= LEX_CAT_AUX;
	  break;
	case 'c':
	  copt[ncopt] |= LEX_CAT_COMPL;
	  break;
	case 'j':
	  copt[ncopt] |= LEX_CAT_CONJ;
	  break;
	case 'd':
	  copt[ncopt] |= LEX_CAT_DET;
	  break;
	case 'm':
	  copt[ncopt] |= LEX_CAT_MODAL;
	  break;
	case 'n':
	  copt[ncopt] |= LEX_CAT_NOUN;
	  break;
	case 'p':
	  copt[ncopt] |= LEX_CAT_PREP;
	  break;
	case 'r':
	  copt[ncopt] |= LEX_CAT_PRON;
	  break;
	case 'v':
	  copt[ncopt] |= LEX_CAT_VERB;
	  break;
	default:
	  usage('c');
	  return_code = -1;
	  goto bottom;
	}
      }
      ncopt++;
      break;
      
      
    case 't':
      if (strlen(optarg) != 1)
	{
	  usage('t');
	  return_code = -1;
	  goto bottom;
	}
      topt = *optarg;
      break;
      
      
    case 'v': strcpy(version, optarg);   break;
      
    case 'h': 
    case '?': 
      if (strlen(optarg) >= 1)
	if (strchr("cplith?", optarg[0]) != (char *)NULL)
	  usage(optarg[0] );
	else
	  usage('?');
      else
	usage('?');
      goto bottom; 
      break; 
      
    default:
      usage('?');
      return_code = -1; 
      goto bottom;
    }
  }

  
  /* --------------------------
     read in the config file
     -------------------------- */
  
  if ( strlen( lexicon_file_name ) <= 0 ) {
    strcpy(lexicon_file_name, getenv("DEFAULT_LEXICON_FILE" )); 
  }
  
  if ( strlen( btree_index_files ) <= 0 ) {
    strcpy(btree_index_files, getenv("DEFAULT_LEXICON_INDEX_FILE" )); 
  } 
  
  if (recFile == (char *)NULL) {
    usage('?');
    return_code = -1; 
    goto bottom;
  }
  


  
   /* ----------------------------------------------------------- 
       Cycle through all the records 
      ----------------------------------------------------------- */


   /* ----------------------------------------------------------- 
      grab the first record from the lexicon 
      ----------------------------------------------------------- */
  done = get_first_lcat_record (record,  lexicon_file_name, btree_index_files, copt, ncopt,topt,popt );
      

  while ( done == D_FALSE ) {
 
    fprintf(stdout,"%s", record );
   /* ----------------------------------------------------------- 
      grab the next record from the lexicon 
      ----------------------------------------------------------- */
    done = get_next_lcat_record (record );
  }
    
   /* ----------------------------------------------------------- 
        close all open files 
      ----------------------------------------------------------- */
  DPR(DF119,"Close all open files");
  fclose(recFp);


  close_lex_btree();

  close_lexicon_record_file();
  
  
 bottom:

   DEXIT(DT118);
   debug_term();

   return ( return_code );

} /*** End main */

/**/
/*==========================================================
%FUNCTION NAME
	usage
%SCOPE
	public
%PURPOSE
	Prints out the usage when the command line options dictate, or
	when no command line options are given, or when unrecognizable
	command line options are given.
%SYNTAX INCLUDES
	#include ""
%EXAMPLE CALL
	ret_val = usage();
%RETURNS
	NLS_S_SUCCESS
%METHOD
	Lots of fprintf's to the stderr
%FILES
	stderr
%TABLES
	None
%FLAGS  
	TRACE DT124
	FULL  DF125
%SYNTAX ARGS
==========================================================*/
void usage(
	   char option /* Input */
	   )

{

  FILE *fp = NULL; 
  char s[128];
  char *lcat_help_file = NULL;

  DFNAME("usage");
  DENTER(DT124);
  
  /* --------------------------
     read in the config file
     -------------------------- */
  
  /* nls_cfg_read( "lexicon2005.cfg" ); */
  
  /* lcat_help_file = cfg_get( "LCAT_HELP_FILE"); */
  lcat_help_file = getenv("LCAT_HELP_FILE");
  fp = fopen(lcat_help_file , "r");
   
  
  if (fp != (FILE *)NULL) {
    
    while (fgets(s, 128, fp)) {
      
      if ((s[0] == ':') && (s[1] == option)) {
	
	while (fgets(s, 128, fp) && s[0] != ':')
	  
	  if (s[0] != '#')
	    fputs(s, stdout);
	fclose(fp);
	goto bottom;
	
      }
    }
  }
  
 bottom:
  
  fclose(fp);
  
  DEXIT(DT124);
  
} /*** End usage */
