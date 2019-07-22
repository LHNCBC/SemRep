
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

/*==========================================================

%SOURCE FILE
	btree_nat.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	8Oct96 divita -- Initial Version

==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include <stdio.h>
#include <stdlib.h>
#include <debug.h>
#include <ctype.h>
#include <db.h>
#include <lvg.h>
#include <lexicon.h>

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#define  KEY_FIELD        1
#define  INFLECTED_FIELD  1
#define  FLAG_FIELD       2
#define  OFFSET_FIELD     3

char* next_entry_num_file = "/tmp/lex_last_#"; /* default filename */

#define STRIP_WHITE_SPACE        1
#define KEEP_EVERYTHING          2
#define DONT_BREAK_ON_HYPHENS    3
#define ORIG_WORD_DEF            4
#define DONT_BREAK_ON_QUESTION   5
#define BREAK_ON_SPACE           6
#define RUSSELLS_RULES           7
#define REPLACE_PUNCT_WITH_SPACE 8

#define CASE_SENSITIVE        0
#define CASE_INSENSITIVE      1
#define WORD_PREFIX           1
#define STRING_PREFIX         2 
#define EXACT                 3 


#define MAXCOPT 10
#define DEFAULT_FIELD_SEPARATOR '|'
#define END_OF_RECORD "}\n"

/* The euiTable has rows:	eui|inflected_form|keys|record_offset */

#define EUI_KEY    1
#define EUI_FLAGS  3
#define EUI_TERM   2
#define EUI_OFFSET 4

#define FROM_EUI_TABLE        1
#define FROM_INFLECTION_TABLE 2

#define WL_BUFFER_SIZE      50000     /* 1000 too small - > salminila and  non- */

#define SWITCH(x) strcpy(stuff,x);
#define BEGINS_WITH(x) if ( strncmp(stuff,x,strlen(x)) == 0 ) {
#define BREAK    }else
#define DEFAULT {
#define END     }

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
extern void llip(unsigned char *s);
extern int DPE(char *msg);
extern int DPR(int flag, char *msg);
extern int get_value_from_field(int field_position, char *value, char   *umls_record);

/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
WordList * query_lex_info( char *query, int caseSensitivity, int   prefix_type,
			   char *lexicon_index_name );

int init_lex_btree( char *index_file, int write_mode );

int close_lex_btree(void);

void free_wl( WordList ** wl );

static DB_ENV *db_init(char *home, char *program );

LexIndexKey * lex_index_keys(char *record, int  *num);

char * get_record_from_offset( long record_offset, char *lexicon_file_name) ;

FILE * init_lex_record_file( char *record_file, int write_mode );

int close_lexicon_record_file(void);

int lf__api( char *orig_term, int argc, char **argv, FILE *output_file_ptr );

int lf_api_get_options( int argc, char **argv );

int lf_get_this_option( char **option_list, char *this_option );

int process_term( char *term, FILE *output_file_ptr );

int process_eui( char *eui, FILE *output_file_ptr );

int output_entries( char *query, int  which_table, WordList *wl, FILE *output_file_ptr );

int lf_usage( int );

int grab_lex_fields_from_btree_row( char *row, lex_t *flag, long  *ofs, char  **inflected_term );

int lex_insert( char *btree_index_file, char *lexicon_file_name, char *orig_record );

int lex_update( char *btree_index_file, char *lexicon_file_name, char *record );

int lex_delete( char *btree_index_file, char *lexicon_file_name, char *eui );

int retrieve_eui_from_record( char *eui, char *record) ;

int get_next_record_from_input( FILE *fp, char *record );

int get_record_from_eui( char *btree_index_file, char *lexicon_file_name, char *eui, char *record );

int get_first_lcat_record( char *record, char *lexicon_file_name, char *btree_index_files,
			   int  *_copt, int _ncopt, char _topt, int   _popt) ;

int get_next_lcat_record( char *record );

int output_lcat_entry( char *eui_row, char *record );

WordList *query_by_eui( char *btree_index_file, char *lexicon_file_name, char *eui );

WordList *tokenize_everything( char *term, int tokenize_style) ;

int generate_and_add_eui_to_record( char *lexFile, char *orig_record,
				    char *new_record, char *next_entry_number);

static int c_get_next_entry_number( char *counter_file, char  *next_entry_number );

int lf_api( char *term, char *options, FILE *ouput_file_ptr );

int bq_pad_with_E0s( int  OldNo, char *number_string );

/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
static  DB               *eui_db = NULL;
static  DB    *inflected_form_db = NULL;
/*static	DB_INFO            eui_b; */
/*static	DB_INFO inflected_form_b; */
static  DB_ENV     *dbenv = NULL;

static char stuff[MAXLINE]; /* formerly in debug/include/useful.h */
static FILE *lexicon_record_fp = NULL ;
static int lexicon_record_file_first_time = D_TRUE;

static int popt=PRINT_RECORD;		/* output option */
static char lopt[MAXLINE];	        /* initially set to default lexicon file */
static char iopt[MAXLINE];	        /* initially set to default lexicon index file */
static char fopt[MAXLINE];
static char eui_opt[255];
static char topt = DEFAULT_FIELD_SEPARATOR;

static int ncopt = 0;			/* number of cat options specified. */
static lex_t copt[MAXCOPT];
static int bopt = 0;
static int sopt = 0;
static int eopt = 0;
static int xopt = 0;
static int wopt = 0;
static int nopt = 0;

static int  lcat_first_time = D_TRUE;
static int lex_insert_delete_first_time = D_TRUE;

static DBC *static_eui_dbcp = NULL;


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

#define DT1156  1156          /* DT for query_lex_info() */
#define DF1157  1157          /* DF for query_lex_info() */
#define DT1378  1378          /* DT for query_first_word_nat_string() */
#define DF1379  1379          /* DF for query_first_word_nat_string() */
#define DT1158  1158          /* DT for init_lex_btree() */
#define DF1159  1159          /* DF for init_lex_btree() */
#define DT1312  1312          /* DT for close_lex_btree() */
#define DF1313  1313          /* DF for close_lex_btree() */
#define DT1616  1616          /* DT for get_record_from_offset() */
#define DF1617  1617          /* DF for get_record_from_offset() */
#define DT1620  1620          /* DT for init_lex_record_file() */
#define DF1621  1621          /* DF for init_lex_record_file() */
#define DT1624  1624          /* DT for close_lexicon_record_file() */
#define DF1625  1625          /* DF for close_lexicon_record_file() */

#define DT210  210          /* DT for lf__api() */
#define DF211  211          /* DF for lf__api() */
#define DT214  214          /* DT for output_entry() */
#define DF215  215          /* DF for output_entry() */
#define DT260  260          /* DT for */ 
#define DF261  261          /* DF for */ 
#define DT262  262          /* DT for lf_get_this_option() */
#define DF263  263          /* DF for lf_get_this_option() */
#define DT1626  1626          /* DT for db_init() */
#define DF1627  1627          /* DF for db_init() */
#define DT1630  1630          /* DT for process_term() */
#define DF1631  1631          /* DF for process_term() */
#define DT1632  1632          /* DT for output_entries() */
#define DF1633  1633          /* DF for output_entries() */
#define DT1638  1638          /* DT for lf_usage() */
#define DF1639  1639          /* DF for lf_usage() */
#define DT1640  1640          /* DT for grab_lex_fields_from_btree_row() */
#define DF1641  1641          /* DF for grab_lex_fields_from_btree_row() */
#define DT2628  2628          /* DT for lex_insert() */
#define DF2629  2629          /* DF for lex_insert() */
#define DT2630  2630          /* DT for lex_update() */
#define DF2631  2631          /* DF for lex_update() */
#define DT2632  2632          /* DT for lex_delete() */
#define DF2633  2633          /* DF for lex_delete() */
#define DT2634  2634          /* DT for lex_init_record_file() */
#define DF2635  2635          /* DF for lex_init_record_file() */
#define DT2636  2636          /* DT for retrieve_eui_from_record() */
#define DF2637  2637          /* DF for retrieve_eui_from_record() */
#define DT1602  1602          /* DT for get_next_record_from_input() */
#define DF1603  1603          /* DF for get_next_record_from_input() */
#define DT2638  2638          /* DT for get_record_from_eui() */
#define DF2639  2639          /* DF for get_record_from_eui() */
#define DT2646  2646          /* DT for get_first_lcat_record() */
#define DF2647  2647          /* DF for get_first_lcat_record() */
#define DT2648  2648          /* DT for get_next_lcat_record() */
#define DF2649  2649          /* DF for get_next_lcat_record() */
#define DT2650  2650          /* DT for output_lcat_entry() */
#define DF2651  2651          /* DF for output_lcat_entry() */
#define DT2652  2652          /* DT for process_eui() */
#define DF2653  2653          /* DF for process_eui() */
#define DT2654  2654          /* DT for query_by_eui() */
#define DF2655  2655          /* DF for query_by_eui() */
#define DT2692  2692          /* DT for generate_and_add_eui_to_record() */
#define DF2693  2693          /* DF for generate_and_add_eui_to_record() */
#define DT2696  2696          /* DT for c_get_next_entry_number() */
#define DF2697  2697          /* DF for c_get_next_entry_number() */
#define DT2744  2744          /* DT for lf_api() */
#define DF2745  2745          /* DF for lf_api() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	query_lex_info
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = query_lex_info(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1156
	FULL  DF1157
%HEADER END
==========================================================*/
WordList * query_lex_info(
			  char *query,                 /* Input  */
			  int   caseSensitivity,       /* Input */
			  int   prefix_type,           /* Input */
			  char *lexicon_index_name     /* Input */
			  )
     
{
  DBT data;  
  DBT key ;
  WordList *wl = NULL;


  int record_found = D_FALSE;
  int status = 0;
  static int first_time = D_TRUE;
  DBC *dbcp = NULL;
  char row[MAXLINE+1];
  char term[MAXLINE+1];
  char key_word[MAXLINE+1];
  char query_word[MAXLINE+1];
  char entry_key[MAXLINE+1];
  
  int errno = 0;

  
  DFNAME("query_lex_info");
  DENTER(DT1156);
  
  /* ---------------------------------
   --------------------------------- */

  if ( first_time == D_TRUE )
  {
    init_lex_btree( lexicon_index_name , D_FALSE); 
    first_time = D_FALSE;
  }

  strcpy(term, query);
  strcpy(query_word,query);
  strcat(query_word," ");
  llip((unsigned char *)term);
           
  memset(&key,  0, sizeof(DBT));
  memset(&data, 0, sizeof(DBT));

  /* FML added this call to memset */
  memset(row, 0, MAXLINE);

  wl = (WordList * ) malloc ( sizeof ( WordList ) );
  wl->words = (char **) malloc ( sizeof ( char *) * WL_BUFFER_SIZE) ;
  wl->n = 0;
  
  sprintf(msg,"looking for |%s| as |%s|", query, term );
  DPR(DF1157,msg);

  key.data  = term; 
  key.size  = sizeof(char ) * (strlen( (char *) key.data )   ) ;
      
  /* Acquire a cursor for the database. */
  if ((errno = inflected_form_db->cursor(inflected_form_db, NULL, &dbcp, 0)) != 0) 
  {
    sprintf(msg,"%s: cursor: %s\n", __FILE__, strerror(errno));
    DPE(msg);
    goto bottom;
  }
  
  if ( prefix_type != EXACT )
    {
      status = dbcp->c_get(dbcp, &key, &data, DB_SET_RANGE); 

      if (( status == 0 ) && ( (void *) data.data  == NULL ))
	{
	  status = dbcp->c_get(dbcp, &key, &data, DB_SET); 
	}
    }
  else
    {
      status = dbcp->c_get(dbcp, &key, &data, DB_SET); 
    }

  
  memset(&key_word, 0, sizeof(char) * MAXLINE);
  strncpy( key_word, (char *) key.data , key.size );

  switch ( prefix_type )
    {
    case EXACT:
      while ( ( status == 0 )
	   && ( (void *) data.data != NULL )
	   && ( strcmp( key_word, term ) == 0 )
	   && ( wl->n < WL_BUFFER_SIZE ) )
	
	{
	  
		/* printf("FML 1: data.data = >%s<\n", data.data); */
		/* printf("FML 1: data.size = >%d<\n", data.size); */
	  memset(&row, 0, sizeof(char) * MAXLINE);
	  strncpy(row, (char *) data.data, data.size );
	  
	  if ( caseSensitivity == CASE_INSENSITIVE )
	  {
		  /* printf("FML 1: setting wl->words[ wl->n ] to >%s<\n", row); */
	    wl->words[ wl->n ] = strdup( row ); 
	    wl->n++;	
	    record_found = D_TRUE;
	  }
	  else
	  {
	    /* ------------------------
	       Get the exact entry type
	       ------------------------ */
	    get_value_from_field(INFLECTED_FIELD, entry_key , row );
	    sprintf(msg,"Query = |%s| Key = |%s|", query, entry_key );
	    DPR(DF1157,msg);
	    if (strcmp( query, entry_key) == 0 )
	    {
		    /* printf("FML 2: setting wl->words[ wl->n ] to >%s<\n", row); */
	      wl->words[ wl->n ] = strdup( row ); 
	      wl->n++;	
	      record_found = D_TRUE;
	    }
	  } 
	  memset(&data, 0, sizeof(DBT));
	  memset(&key,  0, sizeof(DBT));
	  
	  status = dbcp->c_get(dbcp,&key, &data, DB_NEXT); 
	  memset(&key_word, 0, sizeof(char) * MAXLINE);
	  strncpy( key_word, (char *) key.data, key.size );
	  
	  /*
	  sprintf(msg,"looking at %s|%s", key.data, (char *) data.data);
	  DPR(DF1157,msg);
	  */
    
	}

      break;
    case WORD_PREFIX:
	  
      while(  ( status == 0 )
	   && ( data.data == NULL ) 
	   && ( (strcmp( key_word, term ) == 0 )
	       || (( !isalnum( (int)term[0]) ) && (term[0] == key_word[0]))
	       ||  (  (strncmp( term, key_word, strlen( term) ) == 0 )
		  && ( strlen ( key_word)  >= (strlen (term ) + 1  ))
		  && ( ! isalnum ((int)key_word[ strlen(term)  ] ))
		   )
	      )
	   )
	      
	{
	  sprintf(msg,"key %s is empty ",  (char *) key.data );
	  DPR(DF1157,msg);
	  memset(&data, 0, sizeof(DBT));
	  memset(&key,  0, sizeof(DBT));

	  status = dbcp->c_get(dbcp,&key, &data, DB_NEXT); 
	  memset(&key_word, 0, sizeof(char) * MAXLINE);
	  strncpy( key_word, (char *) key.data, key.size );
	}
	
	
      while(  ( status == 0 )
	   && ( data.data != NULL )
	   && (  (strcmp( key_word, term ) == 0 )
	      || (( !isalnum( (int)term[0]) ) && (term[0] == key_word[0]))
		 || (( (strncmp( term, key_word, strlen( term) ) == 0 )
		 && ( strlen ( key_word)  >= (strlen (term ) + 1  ))
		 && ( ! isalnum ((int)key_word[ strlen(term)  ] ))
		 )
		     && ( wl->n < WL_BUFFER_SIZE ))
	      )
	   )
	
	{
	  record_found = D_TRUE;
	  
	  /* printf("FML 3: data.data = >%s<\n", data.data); */
  	  /* printf("FML 3: data.size = >%d<\n", data.size); */
  	  /* printf("FML 3: strlen(data.data) = >%d<\n", strlen(data.data)+1); */
	  memset(&row, 0, sizeof(char) * MAXLINE);
	  strncpy(row, (char *) data.data, data.size );

	  if ( caseSensitivity == CASE_INSENSITIVE )
	  {
	    /* printf("FML 3: setting wl->words[ wl->n ] to >%s<\n", row); */
	    wl->words[ wl->n ] = strdup( row ); 
	    wl->n++;	
	    record_found = D_TRUE;
	  }
	  else
	  {
	    /* ------------------------
	       Get the exact entry type
	       ------------------------ */
	    get_value_from_field(INFLECTED_FIELD, entry_key , row );
	    sprintf(msg,"Query = |%s| Key = |%s| %d", key_word, entry_key, strlen( key_word) );
	    DPR(DF1157,msg);
	    if (strncmp(query_word , entry_key, (strlen( query_word ) -1 )) == 0 )
	    {
	      /* printf("FML 4: setting wl->words[ wl->n ] to >%s<\n", row); */
	      wl->words[ wl->n ] = strdup( row ); 
	      wl->n++;	
	      record_found = D_TRUE;
	    }
	  } 
	  memset(&data, 0, sizeof(DBT));
	  memset(&key,  0, sizeof(DBT));
	  

	  status = dbcp->c_get(dbcp,&key, &data, DB_NEXT); 
	  memset(&key_word, 0, sizeof(char) * MAXLINE);
	  strncpy( key_word, (char * )key.data, key.size );
	  
	  /*
	  sprintf(msg,"looking at %s|%s", key.data, (char *) data.data);
	  DPR(DF1157,msg);
	  */

	}
      break;

    case STRING_PREFIX:
      
      while(( status == 0 ) && ( data.data == NULL ) && 
	    (strncmp( term, (char * ) key.data, strlen( term) ) == 0 ) )
	{
	  sprintf(msg,"key %s is empty",  (char *) key.data );
	  DPR(DF1157,msg);
	  memset(&data, 0, sizeof(DBT));
	  memset(&key,  0, sizeof(DBT));

	  status = dbcp->c_get(dbcp,&key, &data, DB_NEXT); 
	}

     
      while(( status == 0 ) && ( data.data != NULL ) && 
	    (strncmp( term, (char *) key.data, strlen( term) ) == 0 ) && ( wl->n < WL_BUFFER_SIZE ))
	
	{
	  
	  memset(&row, 0, sizeof(char) * MAXLINE);
	  strncpy(row, (char *) data.data, data.size );

	  if ( caseSensitivity == CASE_INSENSITIVE )
	  {
		  /* printf("FML 5: setting wl->words[ wl->n ] to >%s<\n", row); */
	    wl->words[ wl->n ] = strdup( row ); 
	    wl->n++;	
	    record_found = D_TRUE;
	  }
	  else
	  {
	    /* ------------------------
	       Get the exact entry type
	       ------------------------ */
	    get_value_from_field(INFLECTED_FIELD, entry_key , row );
	    sprintf(msg,"Query = |%s| Key = |%s|", query , entry_key );
	    DPR(DF1157,msg);
	    if (strncmp( query, entry_key, strlen( query) ) == 0 )
	    {
	      /* printf("FML 6: setting wl->words[ wl->n ] to >%s<\n", row); */
	      wl->words[ wl->n ] = strdup( row ); 
	      wl->n++;	
	      record_found = D_TRUE;
	    }
	  } 
	  memset(&data, 0, sizeof(DBT));
	  memset(&key,  0, sizeof(DBT));
	  

	  status = dbcp->c_get(dbcp,&key, &data, DB_NEXT); 
	  
	  /*
	  sprintf(msg,"looking at %s|%s", key.data, (char *) data.data);
	  DPR(DF1157,msg);
	  */

	}
      break;
    }
  
  dbcp->c_close(dbcp); 
  dbcp = NULL;

  if ( record_found != D_TRUE )
  {
    sprintf(msg,"Record not found for %s", term);
    DPR(DF1157,msg);
  }

  
 bottom:
  
  
  DEXIT(DT1156);
  return ( wl );

} /*** End query_lex_info */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	init_lex_btree
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = init_lex_btree(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1158
	FULL  DF1159
%HEADER END
==========================================================*/
int init_lex_btree( 
		   char *index_file, /* Input */
		   int  write_mode   /* Input */ 
		   )
     
{
  int    return_code = D_S_SUCCESS;
  char   lexicon_data_dir[MAXLINE];
  int    mode = DB_RDONLY; 
  char   eui_index[MAXLINE];
  char   infl_index[MAXLINE];
  int    errno = 0;
  
  DFNAME("init_lex_btree");
  DENTER(DT1158);


  if ( index_file == NULL ) {
    index_file = getenv("DEFAULT_LEXICON_INDEX_FILE");
  }

  if ( index_file == NULL ) {
    sprintf(msg,"No index file has been specified either on the command line, or from the config file");
    DPE(msg);
    goto bottom;

  }

  strcpy( lexicon_data_dir , getenv("LEXICON_DATA") );

  if (( lexicon_data_dir == NULL ) || ( strlen( lexicon_data_dir ) < 1 ) ) {
     sprintf(msg,"The DATABASE_HOME environment variable has not been set.");
     DPE(msg);
  }

  if ((( lexicon_data_dir == NULL ) || ( strlen( lexicon_data_dir ) < 1 ) ) ||
      ( strcmp( lexicon_data_dir, index_file ) == 0 )) { 
    strcpy(lexicon_data_dir,"./");
  }

  sprintf(eui_index,  "%sByEui.dbx",  index_file);
  sprintf(infl_index, "%sByInfl.dbx", index_file);

  sprintf(msg,"%s|%s|%s|%s", lexicon_data_dir, index_file,infl_index, eui_index );
  DPR(DF1159,msg);



  dbenv = (DB_ENV *) db_init(lexicon_data_dir,__FILE__ ); 

  if ( write_mode == D_TRUE ) {
    mode = (DB_CREATE);
  } else {
    mode = (DB_RDONLY);
  }

  

  if ( index_file != NULL ) {

    if ((errno = db_create(&inflected_form_db, NULL, 0)) != 0) {
      sprintf(msg, "db_create for inflected form: %s", db_strerror(errno));
      return_code = D_E_ERROR;
      goto bottom;
      
    }
    
    errno = inflected_form_db->set_flags(inflected_form_db, DB_DUP ); 
    if ( errno != 0 ) { 
      
      inflected_form_db->err(inflected_form_db, errno, "inflected form set flags" );
      return_code = D_E_ERROR;
      goto bottom; 
    }
    
    errno = inflected_form_db->open(inflected_form_db,
				    NULL,
				    infl_index, 
				    NULL, 
				    DB_BTREE, 
				    (u_int32_t)(mode), 
				    0644) ;
    if ( errno != 0 ) { 
      
      inflected_form_db->err(inflected_form_db, errno, "inflected form db-open-> %s", infl_index);
      return_code = D_E_ERROR;
      goto bottom; 
    }
    
    if ((errno = db_create(&eui_db, NULL, 0)) != 0) {
      sprintf(msg, "db_create for eui_db: %s", db_strerror(errno));
      return_code = D_E_ERROR;
      goto bottom;
      
    }
    
    eui_db->set_flags(eui_db, DB_DUP ); 
    
    errno = eui_db->open(eui_db,
			 NULL,
			 eui_index, 
			 NULL, 
			 DB_BTREE, 
			 (u_int32_t)(mode), 
			 0644) ;

    if ( errno != 0 ) { 
      
      eui_db->err(eui_db, errno, "eui_db->open-> %s", eui_index);
      return_code = D_E_ERROR;
      goto bottom; 
    }
    

    
    if ( eui_db != NULL ) {

      sprintf(msg,"Going to use the Berkeley Btree version of the lexicon");
      return_code = D_S_SUCCESS ;
      DPR(DF1159,msg);
      
      
    } else {
      sprintf(msg,"Not able to open the database: dbopen: %s\n", strerror(errno));
      DPE(msg);
    }
  } else {
    sprintf(msg,"The LEXICON_INDEX has not been set in the config file");
    DPE(msg);
  }

 bottom:
	
  DEXIT(DT1158);
  return ( return_code );

} /*** End init_lex_btree */

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	close_lex_btree
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = close_lex_btree(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1312
	FULL  DF1313
%HEADER END
==========================================================*/
int close_lex_btree(void)

{
  int return_code = D_S_SUCCESS;

  DFNAME("close_lex_btree");
  DENTER(DT1312);

  if (inflected_form_db != NULL ) 
  {
    inflected_form_db->close(inflected_form_db,0);
  }
  if (eui_db != NULL ) 
  {
    eui_db->close(eui_db,0);
  }

  if ( dbenv != NULL )
    dbenv->close(dbenv, 0);
  dbenv = NULL;

  DEXIT(DT1312);
  return ( return_code );

} /*** End close_lex_btree */


/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_record_from_offset
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_record_from_offset(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1616
	FULL  DF1617
%HEADER END
==========================================================*/
char * get_record_from_offset( 
			      long record_offset, /* Input */
			      char *lexicon_file_name /* Input */
			      ) 

{
  
  static char  aRecord[MAXLINE] ; 
  char  line[MAXLINE] ; 

  int not_done = D_TRUE;

  DFNAME("get_record_from_offset");
  DENTER(DT1616);

  strcpy(aRecord,"");
  strcpy(line,"");

  /*-----------------------------------------------------------
    If This is the first time the record file is being accessed,
    open it up
    ----------------------------------------------------------- */
  lexicon_record_fp = (FILE *) init_lex_record_file( lexicon_file_name , D_FALSE );

  if ( lexicon_record_fp == NULL ) {
    sprintf(msg,"Not able to open the lexicon record file");
    DPE(msg);
    goto bottom;
  }
  
  fseek ( lexicon_record_fp, record_offset, SEEK_SET ); 
  
  
  fgets(line,MAXLINE, lexicon_record_fp );

  while (  (!feof( lexicon_record_fp )) && (not_done == D_TRUE )) {

    strcat(aRecord, line );
    
    if ( strstr ( line, "}\n") != NULL ) {
      
      not_done = D_FALSE;
      
    } else {
      
      fgets(line,MAXLINE, lexicon_record_fp );
    }

    sprintf(msg,"looking at |%s|", line );
    DPR(DF1617,msg);
  } 
  
 bottom:
  
  

  DEXIT(DT1616);
  return ( aRecord );

} /*** End get_record_from_offset */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	init_lex_record_file
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = init_lex_record_file(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1620
	FULL  DF1621
%HEADER END
==========================================================*/
FILE * init_lex_record_file( 
			    char *record_file, /* Input */
			    int   write_mode   /* Input */
			    )

{

  char mode[4];

  DFNAME("init_lex_record_file");
  DENTER(DT1620);

  
  /* ---------------------------------------
     Open up the record file 
     --------------------------------------- */

  
  if ( write_mode == D_TRUE ) {

    strcpy(mode,"r+" );

  } else {

    strcpy(mode,"r" );
  }
  


  if (  lexicon_record_file_first_time == D_TRUE) { 
  
    if (  (lexicon_record_fp = fopen ( record_file, mode )) == NULL ) {

      if ( strcmp( mode,"r+" ) == 0 ) {

	if (  (lexicon_record_fp = fopen ( record_file, "a")) == NULL ) {
	  sprintf(msg,"Not able to open the LEXICON_RECORD_FILE");
	  DPE(msg); 
	  goto bottom;
	}
	
      } else {
	sprintf(msg,"Not able to open the LEXICON_RECORD_FILE");
	DPE(msg); 
	goto bottom;
      }
    }
    lexicon_record_file_first_time = D_FALSE; 

    
    
  }

 bottom:

  DEXIT(DT1620);
  return ( lexicon_record_fp );

} /*** End init_lex_record_file */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	close_lexicon_record_file
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = close_lexicon_record_file(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1624
	FULL  DF1625
%HEADER END
==========================================================*/
int close_lexicon_record_file(void)

{
  int return_code = D_S_SUCCESS;

  DFNAME("close_lexicon_record_file");
  DENTER(DT1624);

  
  if ( lexicon_record_fp != NULL )
    fclose(  lexicon_record_fp );

  DEXIT(DT1624);
  return ( return_code );

} /*** End close_lexicon_record_file */



/**/
/*==========================================================
%FUNCTION NAME
	lf__api
%PURPOSE
	Given a term, will return the lexical record
	for that term.
%SYNTAX
	int lf__api(
	           char *term,                * Input  * 
		   int  argc,
		   char **argv,
	           FILE *ouput_file_ptr       * Output * 
	          )
%EXAMPLE CALL
        char term[] = "dogs";
	char options = NULL;
	FILE *file_output_ptr = stdout;

	ret_val = lf__api( term, NULL ,NULL, stdout);
	if ( ret_val == 0 )
	{
	   printf("success\n");
	}
%RETURNS
	D_S_SUCCESS   if successful,    ( the value 0  )
	D_E_ERROR     if not successful ( the value 1  )
	D_S_NOT_FOUND if it does not    ( the value 10 )
                      find the term 
		      in the lexicon.
%SCOPE
	public
%NEEDED INCLUDES
	#include <stdio.h>
	#include <derrno.h>   * To interpret D_S_SUCCESS,  *
	                      *              D_E_ERROR,    *
                              *              D_S_NOT_FOUND *
%METHOD
	
%FILES
	$NLS/config/lexicon2004.cfg
	$NLS/specialist/lexicion/lexicon/data/lexicon
	$NLS/specialist/lexicion/lexicon/data/lexicon_index
%TABLES
	N/A
%NOTES
        In the next go around, the command line options to lf

        will be added, with the exception of the -f option,
        which does not make sence to be called via an api.
%BUGS
	?
%FLAGS  
	TRACE DT210
	FULL  DF211
%HEADER END
==========================================================*/
int lf__api(
	   char *term,                /* Input  */
	   int  argc,                 /* Input  */
	   char **argv,               /* Input  */
	   FILE *output_file_ptr      /* Output */
	   )

{

  /* ----------------------------------
     Initialize the debugging routines. 
     
     This is necessary because 
     the underlying routines contain 
     the debugging hooks.
     ---------------------------------- */

  int return_code = D_S_SUCCESS;
  char *lf_help_file  = NULL;     /*LF_HELP_FILE */
  static int first_time = D_TRUE;

  DFNAME( "lf__api" );

  DENTER(DT210); 
  

  
  if ( first_time == D_TRUE )
    {
      

      /* ------------------------------------------------
	 Determine if the output file pointer has been set or not
	 ------------------------------------------------ */
      if ( output_file_ptr == NULL )
	{
	  output_file_ptr = stdout;
	}

      /* ------------------------------------------------
	 Retrieve the contents of the configuration file
	 ------------------------------------------------ */
    /* if ( nls_cfg_read ("lexiconCurrent.cfg" ) == D_S_SUCCESS ) {  */
    /* if ( nls_cfg_read ("lexicon2004.cfg" ) == D_S_SUCCESS ) {   */
    /* if ( nls_cfg_read (getenv("LEXICON_CONFIG_FILE" ) ) == D_S_SUCCESS ) { */

      /* ------------------------------------------------
	 Only determine the options the first time 
         ------------------------------------------------ */
      if ( lf_api_get_options( argc, argv ) != D_S_SUCCESS ) 
	{
	  goto bottom;
	}

      if ( strlen( fopt ) > 0 )
	term = fopt;
	  
      /* -------------------------------------
	 Determine the location of the lexicon 
	 ------------------------------------- */
      if ( strlen( lopt ) == 0 )
	strcpy(lopt , getenv("DEFAULT_LEXICON_FILE"));
	  
      if ( strlen( iopt ) == 0 )
	strcpy(iopt , getenv("DEFAULT_LEXICON_INDEX_FILE"));
	  
      lf_help_file    = getenv("LF_HELP_FILE");

      first_time = D_FALSE;
    }

  if (( term != NULL ) && ( strlen( term ) > 0 )) {

    /* -----------------------------------
       see if the term is an eui or a term 
       ----------------------------------- */
    if (( term[0] == 'E' ) && 
	( strlen( term) == 8 ) && 
	( isdigit( (int)term[1] ) )&& 
	( isdigit( (int)term[2] ) )&& 
	( isdigit( (int)term[3] ) )&& 
	( isdigit( (int)term[4] ) )&& 
	( isdigit( (int)term[5] ) )&& 
	( isdigit( (int)term[6] ) )&& 
	( isdigit( (int)term[7] ) ) ) {
      
      process_eui( term ,output_file_ptr );
      
    } else {  
      
      process_term( term ,output_file_ptr );
      
    } 

  } else {
      
      sprintf(msg," No term or eui specified");
      DPE(msg);
      return_code = D_E_ERROR;
      goto bottom;
  }
      
  
  
 bottom:
  DEXIT(DT210);
  


  return(return_code );

} /*** End lf__api */
/**/
/*==========================================================
%FUNCTION NAME
	process_term
%PURPOSE
	The guts of the lf function
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = process_term(arg);
%RETURNS
	?
%SCOPE
	private
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1630
	FULL  DF1631
%HEADER END
==========================================================*/
int process_term(
		 char *term,             /* Input */
		 FILE *output_file_ptr   /* Output */
		 )
   
{

  int return_code = D_S_SUCCESS;
  WordList *wl = NULL;

  DFNAME("process_term");
  DENTER(DT1630);


  /* --------------------------------------------------
     Switch on the exact, word prefix, and string prefix 
     -------------------------------------------------- */

  if ( eopt )
    {

      if ( wopt )
	{
	  wl  = query_lex_info( term, CASE_SENSITIVE, WORD_PREFIX, iopt );
	}
      else if ( xopt )
	{
	  wl  = query_lex_info( term, CASE_SENSITIVE, STRING_PREFIX,iopt );
	}
      else  /* case insensitive, exact match */
	{
	  wl  = query_lex_info( term, CASE_SENSITIVE, EXACT, iopt );
	}

    }
  else
    {
      if ( wopt )
	{
	  wl  = query_lex_info( term, CASE_INSENSITIVE, WORD_PREFIX, iopt );
	}
      else if ( xopt )
	{
	  wl  = query_lex_info( term, CASE_INSENSITIVE, STRING_PREFIX,iopt );
	}
      else  /* case insensitive, exact match */
	{
	  wl  = query_lex_info( term, CASE_INSENSITIVE, EXACT, iopt );
	}
      
    }

  output_entries(term, FROM_INFLECTION_TABLE, wl ,output_file_ptr);

  /*  free_wl( &wl ); */
  free(wl);
  wl = NULL;

  DEXIT(DT1630);

  return( return_code );
    
} /*** End process_term */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	output_entry
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = output_entry(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT214
	FULL  DF215
%HEADER END
==========================================================*/
int output_entries(
		   char *query,          /* Input  */
		   int  which_table,     /* Input  */
		   WordList *wl,          /* Input  */
		   FILE *output_file_ptr /* Output */
		   )
     
{
  int return_code = D_S_SUCCESS;

    lex_t      flags; 
    char       key[MAXLINE];
    off_t      ofs           = 0; 
    off_t      previous_offset   = -1; 
    int        i,j;
    char       string_flags[64];
    char       string_offset[64];
    char       *aRecord = NULL;
    static     lex_t allcats = (LEX_CAT_ADJ | LEX_CAT_ADV | 
				LEX_CAT_AUX | LEX_CAT_COMPL |
				LEX_CAT_CONJ | LEX_CAT_DET | 
				LEX_CAT_MODAL | LEX_CAT_NOUN |
				LEX_CAT_PREP | LEX_CAT_PRON | 
				LEX_CAT_VERB
				);

    int record_found = D_FALSE;

    
    DFNAME("output_entries");
    DENTER(DT1632);

    if ( wl != NULL ) {

    for ( j = 0; j < wl->n; j++ ) {
	
      switch ( which_table ) {
	case  FROM_EUI_TABLE:  
	  get_value_from_field( EUI_FLAGS,   string_flags, WLWN(wl, j ));
	  get_value_from_field( EUI_OFFSET, string_offset, WLWN(wl, j ));
	  get_value_from_field( EUI_KEY, key, WLWN(wl, j ));

	  break;

	case  FROM_INFLECTION_TABLE:  

	  get_value_from_field( FLAG_FIELD,   string_flags, WLWN(wl, j ));
	  get_value_from_field( OFFSET_FIELD, string_offset, WLWN(wl, j ));
	  get_value_from_field( KEY_FIELD, key, WLWN(wl, j ));
	  break;

	}
	
	sprintf(msg," the row = |%s|\n", WLWN(wl,j) );       DPR(DF1633,msg);
	sprintf(msg," The offset = |%s|\n", string_offset ); DPR(DF1633,msg);
	sprintf(msg," The flags  = |%s|\n", string_flags );  DPR(DF1633,msg);
	flags = atoi( string_flags );
	ofs   = atoi( string_offset );
	
	
	/* check for appropriate syntactic categories */
	for (i=0; i<ncopt; i++)
	  {
	    if (((flags & allcats) & copt[i]) == 0)
	      goto next_term;
	  }
	
	/* only base forms */
	if (bopt)
	  {
	    if (!IS_LEX_BASE(flags) && !IS_LEX_BASELOWER(flags))
	      goto next_term; 
	  }
	
	/* only base forms or spelling variants */
	if (sopt)
	  {
	    if (!IS_LEX_BASE(flags) && !IS_LEX_BASELOWER(flags))
	      goto next_term;
	    if (!IS_LEX_SV(flags) && !IS_LEX_SVLOWER(flags))
	      goto next_term; 
	  }
	
	/* only exact */
	if (eopt)
	  {
	    if (IS_LEX_BASELOWER(flags) || IS_LEX_SVLOWER(flags) ||
		IS_LEX_BASEINFLLOWER(flags) || IS_LEX_SVINFLLOWER(flags))
	      goto next_term;
	  }
	
	switch (popt)
	  {
	  case PRINT_RECORD:

	    /* ---------------------
	       This may be a cluge but if you've just printed
	       a record pointing to the same place, skip it.

	       This situation occurs when in the inflection table
	       there are multiple rows for the same record, (one
	       for each inflection of each term and spelling variant)
	       
	       so for example, cX and CX are spelling variants of
	       one another. Two rows from the inflection table
	       come up. We only need to print one of them.
	       --------------------- */
	       
	    if ( ofs != previous_offset ) {
	      
	      aRecord = get_record_from_offset( ofs,lopt ) ;
	      fprintf(output_file_ptr,"%s",aRecord );  
	      record_found = D_TRUE;
	      previous_offset = ofs;
	    }
	    
	    break;
	    
	  case PRINT_QUERY:
	    fprintf(output_file_ptr,"%s\n", query);
	    record_found = D_TRUE;
	    break;
	    
	  case PRINT_KEY:
	    
	    fprintf(output_file_ptr,"%s%c%s\n", query, topt, key);
	    record_found = D_TRUE;
	    break;
	    
	  case PRINT_INDEX:
		  fprintf(output_file_ptr, "%s%c%d%c%s%c%d\n",
			  query, topt, (int)flags, topt, key, topt, (int)ofs);
	    record_found = D_TRUE;
	    break;
	    
	  default:
	    goto next_term;
	  }	

      next_term: ;;
      
      } 
    }
    
    if (( record_found == D_FALSE ) && ( nopt ) )
      {
	fprintf(output_file_ptr, "Not found: %s\n", query);
      } 
	  

  DEXIT(DT1632);
  return ( return_code );


} /*** End output_entries */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	lf_api_get_options
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lf_api_get_options(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT260
	FULL  DF261
%HEADER END
==========================================================*/
int lf_api_get_options( 
		       int  argc,
		       char **argv 

		       )
{
  int return_code = D_S_SUCCESS;
  char *cp;
  char *optarg = NULL;
  char *current_arg = NULL;
  char *j ;
  int i = 0;
  int next_arg_is_term = D_FALSE; 
  int next_arg_is_eui  = D_FALSE; 
  
  DFNAME("lf_api_get_options");

  DENTER(DT260);
  
  strcpy( fopt,"");

  for ( i = 1; i < argc; i++ )
    {

      if ( next_arg_is_term == D_TRUE ) {
	  if ( (argv[i] !=NULL )&& ( strlen( argv[i] ) > 0 )) {
	    strcpy( fopt,argv[i]);
	    next_arg_is_term = D_FALSE; 
	  } else {
	    sprintf(msg, "No term specified ");
	    DPE(msg);
	    return_code = D_E_ERROR;
	    goto bottom;
	  }

      } else if ( next_arg_is_eui == D_TRUE ) {

	if ( (argv[i] != NULL )&& ( strlen( argv[i] ) > 0 )) {
	  strcpy( eui_opt,argv[i]);
	} else {
	  strcpy( eui_opt,"Take_from_stdin");
	}
	next_arg_is_eui = D_FALSE; 

      } else {
	
	current_arg = argv[i];
	/* optarg = current_arg + 2;  */

	optarg = argv[ i + 1 ]; 
      
	SWITCH ( current_arg ) {

	  BEGINS_WITH("-c")             /* <cats> */
	    {
	      optarg = current_arg+2;
	      /* ---------------------------------------
		 Pickup everything up to the next option
		 --------------------------------------- */
	      for (cp = optarg; *cp != EOS; cp++)
		{
		  switch (*cp)
		    {
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
		      lf_usage('c');
		      return_code = D_S_NOT_FOUND;
		    }
		}
	      ncopt++;
	    }
	  BREAK  
	  
	  BEGINS_WITH("-p") /* <print options> */
	    {
	      optarg = current_arg+2;

	      if (strlen(optarg) != 1)
		{
		  lf_usage('p');
		  return_code = D_S_NOT_FOUND;
		}
	      else
		{
		  switch (optarg[0])
		    {
		    case 'r':
		      popt = PRINT_RECORD;
		      break;
		    case 'k':
		      popt = PRINT_KEY;
		      break;
		    case 'i':
		      popt = PRINT_INDEX;
		      break;
		    default:
		      lf_usage('p');
		      return_code = D_S_NOT_FOUND;
		    }
		  
		}
	    }
	  BREAK 
	  
	  BEGINS_WITH("-l") /* <lexicon>       */
	    {
	      strcpy(lopt, optarg);
	      sprintf(msg,"Going to use %s as the lexicon", optarg );
	      DPR(DF261,msg);
	    }
	  BREAK 
      
	  
	  BEGINS_WITH("-i") /* <index>         */
	    {
	      strcpy(iopt, optarg);
	      sprintf(msg,"Going to use %s as the indexes", optarg );
	      DPR(DF261,msg);
	    }
	  BREAK 
	
	  BEGINS_WITH("-t") /* <seporator>     */
	    {
	      if (strlen(optarg) != 1)
		{
		  lf_usage('t');
		  return_code = D_S_NOT_FOUND;
		}
	      else
		{
		  topt = *optarg ;
		}
	    }
	  BREAK 
	  
	  BEGINS_WITH("-f") /* <term>          */
	    {
	      next_arg_is_term = D_TRUE;
	    }
	  BREAK 

	
	  BEGINS_WITH("-b") 
	    {
	      bopt = 1;
	    }
	  BREAK 

	  BEGINS_WITH("-s") 
	    {
	      sopt = 1;
	    }
	  BREAK 
	  
	  BEGINS_WITH("-x") 
	    {
	      if (wopt)
		{
		  lf_usage('x');
		  return_code = D_S_NOT_FOUND;
		  
		}
	      else
		{
		  xopt = 1;
		}
	    }
	  BREAK 
	  
	  BEGINS_WITH("-w") 
	    {
	      if (xopt)
		{
		  lf_usage('w');
		  return_code = D_S_NOT_FOUND;
		}
	      else
		{
		  wopt = 1;
		}
	    }
	  BREAK 
	    
	    BEGINS_WITH("-e") 
	    {
	      eopt = 1;
	    }
	  BREAK 
	    
	  BEGINS_WITH("-n") 
	    {
	      nopt = 1;
	    }
	  BREAK 
	    
	  BEGINS_WITH("-h") /* {cplitfbsxwenh} */
	    {
	      if (( optarg != NULL ) && ( (int) strlen ( optarg ) > 0 ))
		{
		  for ( j = optarg; *j != EOS; j++ )
		    {
		      lf_usage( *j );
		      return_code = D_S_NOT_FOUND;
		    }
		}
	      else
		{
		  lf_usage('?');
		  return_code = D_S_NOT_FOUND;
		}
	      
	    }
	  BREAK

	    
          BEGINS_WITH("?") 
	    {
	      lf_usage('?');
	      return_code = D_S_NOT_FOUND;
	    }
	  BREAK  
	  
	  DEFAULT
	    {
	    }
	  END 


	} /* end of SWITCH statement */

	} /* Arg is not a term */
      
    } /* end of for loop */

 bottom:
  
  DEXIT(DT260);

  return ( return_code );
  
} /*** End lf_api_get_options */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	lf_get_this_option
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lf_get_this_option(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT262
	FULL  DF263
%HEADER END
==========================================================*/
int lf_get_this_option( 
		       char **option_list,  /* Input/Output */
		       char *this_option    /* Output -- premalloced */
		       )

{
  int return_code = D_S_SUCCESS;
  char *a_ptr = NULL;
  int  i      = 0;
  int  not_done = D_TRUE;
  
  DFNAME("lf_get_this_option");
  DENTER(DT262);
  
  DPR(DF263,"");


  memset( this_option,0,MAXLINE);

  a_ptr = *option_list;
  
  if ( strstr( a_ptr,"-f " ) != NULL )
    {
      a_ptr = a_ptr + 3;
    }
  else if ((a_ptr[0]== '-') && ( a_ptr[1]>= 'a') && ( a_ptr[1] <= 'z'))
  {
    a_ptr = a_ptr + 2;
  }

  while ( not_done == D_TRUE ) 
  {
    
    if ( *a_ptr != EOS )
    {
      if ( strncmp(a_ptr," ",1) == 0 ) 
      {
	not_done = D_FALSE;
      }
      else
      {
	this_option[i]=*a_ptr;
	i++;
      }
      a_ptr++;
    }
    else
    {
      not_done = D_FALSE;
    }
  }

  *option_list = a_ptr;
  this_option[i]=EOS;
    

  DEXIT(DT262);
  return ( return_code );
  
} /*** End lf_get_this_option */


/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	db_init
%PURPOSE
  	Initialize the berkely btree environment.
	
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = db_init(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1626
	FULL  DF1627
%HEADER END
==========================================================*/
static DB_ENV *db_init(
		       char *home, 
		       char *progname 
		       )

{
  DB_ENV *dbenv;
  int errno = 0;

  DFNAME("db_init");
  DENTER(DT1626);

  if ( db_env_create(&dbenv, 0) != 0) {
    dbenv = NULL;
    goto bottom;
  } 
   
  /* if ((errno = dbenv->open(dbenv, home, NULL, DB_USE_ENVIRON, 0)) == 0) { */
  if ((errno = dbenv->open(dbenv, home, 0, 0)) == 0) { 
    ;;
  } else {
    (void)dbenv->close(dbenv,0);
    dbenv = NULL;
    
  }
 bottom:

  DEXIT(DT1626);
  return (dbenv);

} /*** End db_init */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	lf_usage
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lf_usage(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1638
	FULL  DF1639
%HEADER END
==========================================================*/
int lf_usage(int option)

{
  int return_code = D_S_SUCCESS;
  char *lf_help_file ;
  FILE *fp = NULL; 
  char s[256];

  DFNAME("lf_usage");
  DENTER(DT1638);

  
  lf_help_file = getenv("LF_HELP_FILE");
  fp = fopen(lf_help_file, "r");
  
  lf_help_file = getenv( "LF_HELP_FILE");
  
  if (fp == (FILE *)NULL)
    {
      sprintf(msg, "Help file: \"%s\" not found.", lf_help_file);
      DPE(msg);
      return_code = D_E_ERROR;
      goto bottom;
    }

  while (fgets(s, sizeof(s), fp))
    {
      if ((s[0] == ':') && (s[1] == option))
	{
	  while (fgets(s, sizeof(s), fp) && s[0] != ':')
	    if (s[0] != '#')
	      fputs(s, stdout);
	  
	}
    }
  fclose(fp);

 bottom:
 
  DEXIT(DT1638);
  return ( return_code );

} /*** End lf_usage */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	grab_lex_fields_from_btree_row
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = grab_lex_fields_from_btree_row(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1640
	FULL  DF1641
%HEADER END
==========================================================*/
int grab_lex_fields_from_btree_row(
				   char *row,             /* Input   */
				   lex_t *flag,           /* Output  */
				   long  *ofs,            /* Output  */ 
				   char  **inflected_term  /* Output */
				   )

{
  int return_code = D_S_SUCCESS;
  static char ainflected_term[MAXLINE];
  char string_flags[64];
  char string_offset[64];
  

  DFNAME("grab_lex_fields_from_btree_row");
  DENTER(DT1640);

  strcpy(ainflected_term, "");

  get_value_from_field(INFLECTED_FIELD,  ainflected_term,   row );
  get_value_from_field(FLAG_FIELD,       string_flags,      row );
  get_value_from_field(OFFSET_FIELD,     string_offset,     row );

  *flag = atoi ( string_flags );
  *ofs = atoi  ( string_offset );

  *inflected_term = strdup( ainflected_term );

  sprintf(msg,"The inflected term is |%s|", *inflected_term );
  DPR(DF1641,msg);

  DEXIT(DT1640);
  return ( return_code );

} /*** End grab_lex_fields_from_btree_row */
/**/
/*==========================================================
%FUNCTION NAME
	lex_insert
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lex_insert(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2628
	FULL  DF2629
%HEADER END
==========================================================*/
int 
lex_insert( 
	       char *btree_index_file,   /* Input */
	       char *lexicon_file_name,  /* Input */ 
	       char *orig_record         /* Input */
	       )

{
  int                  return_code = D_S_SUCCESS;
  LexIndexKey                *keys = NULL;
  char                         eui[16];
  int                            n = 0;
  int                            i = 0;
  int                        flags = 0;
  long               record_offset = 0;
  char inflection_flags_and_offset[MAXLINE];
  char              inflected_form[MAXLINE];
  DBT                          key;
  DBT                      content;
  int                      status = 0;
  char               old_record[MAXLINE];
  char                   record[MAXLINE];
  int         this_is_a_new_record = D_FALSE;
  

  DFNAME("lex_insert");
  DENTER(DT2628);

  if ( lex_insert_delete_first_time == D_TRUE ) {

    init_lex_btree( btree_index_file, D_TRUE ); 
    lexicon_record_fp = (FILE *) init_lex_record_file( lexicon_file_name, D_TRUE );
    
    lex_insert_delete_first_time = D_FALSE;
  }
  
  if ( lexicon_record_fp == NULL ) {
    sprintf(msg,"Not able to open the lexicon record file");
    DPE(msg);
    goto bottom;
  }


  /* -----------------------------------------------------
     If this is a new record i.e., it's entry=\n, generate
     a new eui for this record
     ----------------------------------------------------- */
  this_is_a_new_record = D_FALSE;
  if ( strstr( orig_record, "entry=\n" ) != NULL ) {
    
    
    generate_and_add_eui_to_record(lexicon_file_name, orig_record, record, eui );

    if ( strlen( record ) <=3 ) {
      sprintf(msg," problem with generating the eui from record %s", orig_record );
      DPE(msg);
      goto bottom;
    }
    
    this_is_a_new_record = D_TRUE;
    
  } else {
    strcpy( record, orig_record );
    
  }

  sprintf(msg,"================== the record in hand is ============= ");
  DPR(DF2629,msg);
  sprintf(msg,"%s", record );
  DPR(DF2629,msg);
  sprintf(msg,"================== %s ============= ", eui);
  DPR(DF2629,msg);
  
  /* -------------------------------
     Push the record through lvar to 
     generate the flags 
     ------------------------------- */
  if ((keys = (LexIndexKey * ) lex_index_keys(record , &n)) == (LexIndexKey *)NULL) {
    sprintf(msg,"Not able to create keys for \n%s\n", record );
    DPE(msg);
    goto bottom;
  }
  sprintf(msg,"there are %d keys made", n );
  DPR(DF2629,msg);
  

  if ( this_is_a_new_record == D_FALSE ) {

     retrieve_eui_from_record ( (char *)&eui, record );

    sprintf(msg,"The eui = %s", eui );
    DPR(DF2629,msg);

    /* -----------------------------------------
       See if this record has already been added
       
       if (this record has been added, don't add
       it again).
       
       ----------------------------------------- */
    return_code = get_record_from_eui(
				      btree_index_file,   
				      lexicon_file_name,  
				      eui,                
				      old_record              
				      );
    
  
    if ( return_code == D_TRUE ) {
      sprintf(msg,"This record already exists, not adding it:\nold_record= \n%s\nnew record = \n%s", old_record , record);
      DPE(msg);
      return_code = D_E_ERROR;
      goto bottom;
    }

  }
  return_code = D_S_SUCCESS;
  

  /* -------------------------------
     go to the bottom of the file 
     ------------------------------- */
  fseek ( lexicon_record_fp, 0, SEEK_END ); 
  
  /* -------------------------------
     Figure out the offset in the
     output file
     ------------------------------- */
  record_offset = (long)  ftell( lexicon_record_fp );
  
  
  /* -------------------------------
     Write the record to the output file
     ------------------------------- */
  fprintf(lexicon_record_fp,"%s", record );
  fflush( lexicon_record_fp );
  
  /* --------------------------------------------------------------
     Write the keys and the flags, offset and eui to the index file
     -------------------------------------------------------------- */
  for (i=0; i< n; i++) {
    

    flags = (keys+i)->likFlags ;
    
    if (( ( IS_LEX_BASELOWER ( flags )) || 
	  ( IS_LEX_SVLOWER ( flags )) || 
	  ( IS_LEX_SVLOWER ( flags )) || 
	  ( IS_LEX_BASEINFLLOWER( flags )) ||
	  ( IS_LEX_SVINFLLOWER( flags ))) && ( !IS_LEX_BASE( flags ))) {

      /* --------------------------------------------------------
	 We are not putting the lowercase 
	 versions in the index rather, 
	 the keys are all lowercased
	 
	 unless the it's a base and a lowercase spelling variant
	 ------------------------------------------------------- */
    }
    else {

      /*-------------------------------
	Add to the inflected form index

	inflected_form|keys|record_offset|eui
        ------------------------------- */

      strcpy(inflected_form, (keys+i)->likKey);

      memset(inflection_flags_and_offset, 0, sizeof( char )*MAXLINE);
      sprintf(inflection_flags_and_offset,"%s|%d|%d|%s", 
	      inflected_form, 
	      (int)(keys+i)->likFlags, 
	      (int)record_offset,
	      eui);
      
      
      memset(&key, 0, sizeof(DBT));
      memset(&content, 0, sizeof(DBT));
      
      
      llip( (unsigned char *)inflected_form );

      /*
	sprintf(msg,"The key added to the inflected table = %s", inflected_form );
	DPR(DF2629,msg);
	sprintf(msg,"The data added to the inflected table = %s", inflection_flags_and_offset ); 
	DPR(DF2629,msg);
      */
      
      sprintf(msg,"%s|%s",inflected_form,inflection_flags_and_offset );
      DPR(DF2629,msg);
      
      key.data = inflected_form; 
      
      key.size = sizeof( char ) * (strlen( key.data )  ) ;
	
      
      content.data = (char *) strdup( inflection_flags_and_offset ); 
      
      content.size = sizeof( char ) * (strlen(inflection_flags_and_offset) ) ;
      
      if (( content.data == NULL ) ||( strlen( content.data) <= 0 )) {
	  sprintf(msg,"key |%s| has no data", (char *)key.data );
	DPE(msg);
	continue; 
      }
      
      status = inflected_form_db->put(inflected_form_db, NULL, &key, &content, 0);
      
      if ( status != 0 ) {

	perror("insert/put");
	sprintf(msg,"%s: cursor: %d\n", __FILE__, status );
	DPE(msg);
      }

      free ( content.data );

      /*-------------------------------
	Add to the eui index
	eui|inflected_form|keys|record_offset

         #DEFINE EUI_FLAGS  3
         #DEFINE EUI_TERM   2
         #DEFINE EUI_OFFSET 4
        ------------------------------- */
      strcpy(inflected_form, (keys+i)->likKey);
      memset(inflection_flags_and_offset, 0, sizeof( char )*MAXLINE);
      sprintf(inflection_flags_and_offset,"%s|%s|%d|%d", 
	      eui,
	      inflected_form, 
	      (int)(keys+i)->likFlags, 
	      (int)record_offset);
	      
      
      memset(&key, 0, sizeof(DBT));
      memset(&content, 0, sizeof(DBT));
      

      sprintf(msg,"The key to the eui table is %s", eui );
      DPR(DF2629,msg);
      sprintf(msg,"The data to the eui table is %s", inflection_flags_and_offset);
      DPR(DF2629,msg);

      
      key.data = eui; 
      
      key.size = sizeof( char ) * (strlen( key.data )  ) ;
      
      content.data = (char *) strdup( inflection_flags_and_offset ); 
      
      content.size = sizeof( char  ) * (strlen(inflection_flags_and_offset) ) ;
      
      if (( content.data == NULL ) ||( strlen( content.data) <= 0 )) {
	      sprintf(msg,"key |%s| has no data", (char *)key.data );
	DPE(msg);
	continue; 
      }
      
      status = eui_db->put(eui_db, NULL, &key, &content, 0);
      
      if ( status != 0 ) {

	perror("insert/put");
	sprintf(msg,"%s: cursor: %d\n", __FILE__, status );
	DPE(msg);
      }

      free ( content.data );
    } /* ----------------------- end of putting elements in */
    
  } 

 bottom:
  

  DEXIT(DT2628);
  return ( return_code );

} /*** End lex_insert */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	lex_update
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lex_update(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2630
	FULL  DF2631
%HEADER END
==========================================================*/
int lex_update( 
	       char *btree_index_file,   /* Input */
	       char *lexicon_file_name,  /* Input */ 
	       char *record /* Input */
	       )

{
  int return_code = D_S_SUCCESS;
  char eui[16];

  DFNAME("lex_update");
  DENTER(DT2630);
  
  /* -------------------------------------------
     Figure out what eui this is
     ------------------------------------------- */
  retrieve_eui_from_record( eui, record );
  
  if ( strlen( eui) > 6  ) {
    
    
    /* -------------------------------------------
       Remove all entries that have this eui in it
       ------------------------------------------- */
    
    lex_delete( btree_index_file, 
		lexicon_file_name,
		eui );
    
    
    /* -------------------------------------------
      Add this record to the lexicon 
       ------------------------------------------- */
    
   lex_insert( btree_index_file,  
	       lexicon_file_name, 
	       record );
   

  }

  DEXIT(DT2630);
  return ( return_code );

} /*** End lex_update */
/**/
/*==========================================================
%FUNCTION NAME
	lex_delete
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lex_delete(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2632
	FULL  DF2633
%HEADER END
==========================================================*/
int lex_delete(  
	       char *btree_index_file,   /* Input */
	       char *lexicon_file_name,  /* Input */ 
	       char *eui		 /* Input */ 
	       )

{
  int return_code = D_S_SUCCESS;
  char           row[MAXLINE];
  char           *term = NULL;
  DBC     *eui_dbcp = NULL;
  DBC     *inflected_form_dbcp = NULL;
  DBT       eui_key;
  DBT       inflected_form_key ;
  DBT      eui_data;
  DBT      inflected_form_data;
  int          status = 0;
  char       eui_key_word[MAXLINE];
  char       inflected_form_key_word[MAXLINE];
  int           errno = -1;
  char         rowEui[20];
  char        rowTerm[MAXLINE] ; 
  int       delStatus = 0;
 

  DFNAME("lex_delete");

  DENTER(DT2632);

  if ( lex_insert_delete_first_time == D_TRUE ) {
    
    init_lex_btree( btree_index_file, D_TRUE ); 
    lexicon_record_fp = (FILE *) init_lex_record_file( lexicon_file_name, D_TRUE );
    
    lex_insert_delete_first_time = D_FALSE;
  }
  
  /* -----------------------------------------------
     Grab the inflected forms that have that eui
    forms from the index
     -----------------------------------------------
     eui|inflected form|keys

     inflected_form|eui|keys
     ----------------------------------------------------- */
  
  /* -----------------------------------------------------
     create a curser to lookup the inflected forms from the eui table
     ----------------------------------------------------- */
  if ((errno = eui_db->cursor(eui_db, NULL, &eui_dbcp,0)) != 0) {
    sprintf(msg,"Not able to grab a cursor for the eui_table") ;
    DPE(msg);
    goto bottom;
  }

  memset(&eui_key,  0, sizeof(DBT));
  memset(&eui_data, 0, sizeof(DBT));
  
  
  eui_key.data  = strdup( eui) ; 
  eui_key.size  = sizeof(char ) * (strlen( (char *) eui_key.data )   ) ;
  
  /* -----------------------------------------------------
     Lookup the inflected form in the inflected form table 
     ----------------------------------------------------- */
  status    = eui_dbcp->c_get(eui_dbcp,&eui_key, &eui_data, DB_SET); 
  
  strncpy( eui_key_word, (char *) eui_key.data, eui_key.size );
  eui_key_word[eui_key.size]=EOS;
  

  if ( status != 0 ) {

    if ( status == -7 ) {
    sprintf(msg,"The eui %s was not found", eui );
    DPE(msg);
    } else {
    sprintf(msg,"Not able to find the eui %s from the cursur : %d\n",eui, status );
    DPE(msg);
    }
  }
  
  while(( status == 0 ) && ( (void *) eui_data.data != NULL ) && 
	(strcasecmp( eui_key_word, eui ) == 0 )  ) { 
    
    strncpy(row,(char *) eui_data.data, eui_data.size );
    row[eui_data.size]=EOS;
    
    get_value_from_field(1, rowEui,  row );
    get_value_from_field(2, rowTerm, row );
    
    
    sprintf(msg,"looking at %s : %s [%s]", rowTerm, rowEui,row );
    DPR(DF2633,msg);
    
    
    if ( strcmp( eui, rowEui ) == 0 ) { 				   
      
      
      term = rowTerm; 
      
      /* ------------------------------------------------------
	 retrieve all the inflection enties from the inflection
	 table 
	 
	 If the entry has the same eui, remove it.
	 
	 ------------------------------------------------------ */
      
      /* -----------------------------------------------------
	 create a curser to lookup the uninflected term in the base table
	 ----------------------------------------------------- */
      if ((errno = inflected_form_db->cursor(inflected_form_db, NULL, &inflected_form_dbcp,0)) != 0) {
	sprintf(msg,"Not able to grab a cursor for the inflected_form_table") ;
	DPE(msg);
	goto bottom;
      }
      memset(&inflected_form_key,  0, sizeof(DBT));
      memset(&inflected_form_data, 0, sizeof(DBT));
      
      
      inflected_form_key.data  = strdup( term ) ; 

      llip( inflected_form_key.data) ;

      inflected_form_key.size  = sizeof(char ) * (strlen( (char *) inflected_form_key.data )   ) ;
      
      /* -----------------------------------------------------
	 Lookup the inflected form in the inflected form table 
	 ----------------------------------------------------- */
      status = inflected_form_dbcp->c_get(inflected_form_dbcp,&inflected_form_key, &inflected_form_data, DB_SET); 
      
      strncpy( inflected_form_key_word, (char *) inflected_form_key.data, inflected_form_key.size );
      inflected_form_key_word[inflected_form_key.size]=EOS;
      

      if ( status != 0 ) {
	sprintf(msg,"Not able to find the inflected form |%s| because %d", term,  status );
	DPE(msg);
      }
      
      while(( status == 0 ) && ( (void *) inflected_form_data.data != NULL ) && 
	    (strcasecmp( inflected_form_key_word, term ) == 0 )  ) { 
	
	strncpy(row,(char *) inflected_form_data.data, inflected_form_data.size );
	row[inflected_form_data.size]=EOS;
	
	get_value_from_field(4, rowEui,  row );
	get_value_from_field(1, rowTerm, row );
	
	
	sprintf(msg,"In inflected Table: for |%s|%s| looking at %s : %s [%s]", term, eui, rowTerm, rowEui,row );
	DPR(DF2633,msg);
	
	if (( strcmp( term, rowTerm ) == 0 ) && 
	    ( strcmp( eui, rowEui ) == 0 )) { 				   
	  
	  delStatus = inflected_form_dbcp->c_del(inflected_form_dbcp, 0 ); 
	  sprintf(msg,"In inflected Table: =========== just deleted %s ======== ", rowTerm ); 
	  DPR(DF2633,msg);
	  
	  if ( delStatus != 0 ) {
	    
	    return_code = D_E_ERROR;
	    sprintf(msg,"Problem removing %s from inflected_form_db: %s", 
		    row, 
		    strerror(errno));
	    DPE(msg);
	  }
	  
	}
	
	
	memset(&inflected_form_data, 0, sizeof(DBT));
	memset(&inflected_form_key,  0, sizeof(DBT));
	
	
	status = inflected_form_dbcp->c_get(inflected_form_dbcp,&inflected_form_key, &inflected_form_data, DB_NEXT); 
	strncpy( inflected_form_key_word, (char *) inflected_form_key.data, inflected_form_key.size );
	inflected_form_key_word[ inflected_form_key.size ] = EOS;
	
    } /*----  End of the tour through each inflected form */
      
      /* --------------------------
	 Close the curser
	 -------------------------- */
      
      status = inflected_form_dbcp->c_close(inflected_form_dbcp); 
      inflected_form_dbcp = NULL;
      if ( status != 0 ) {
	
	return_code = D_E_ERROR;
	sprintf(msg,"Problem closing the cursor for the inflected table %d", 
		status );	
	DPE(msg);
      } 
      
      /* ----------------------------------------------- 
	 Remove the entries from the eui table
	 ----------------------------------------------- */
      
      delStatus = eui_dbcp->c_del(eui_dbcp, 0 ); 
      
      if ( delStatus != 0 ) {
	
	return_code = D_E_ERROR;
	sprintf(msg,"Problem removing %s from eui_index: %s", 
		row, 
		strerror(errno));
	DPE(msg);
      } else {
	sprintf(msg,"In eui Table: =========== just deleted %s ======== ", rowTerm ); 
	DPR(DF2633,msg);
      }
      
    } /* This is a matching eui */
    
    /* -----------------------------------------------
       grab the next inflected form from the eui_index 
       ----------------------------------------------- */
    
    memset(&eui_data, 0, sizeof(DBT));
    memset(&eui_key,  0, sizeof(DBT));
  
    
    status = eui_dbcp->c_get(eui_dbcp,&eui_key, &eui_data, DB_NEXT); 
    strncpy( eui_key_word, (char *) eui_key.data, eui_key.size );
    eui_key_word[ eui_key.size ] = EOS;

    if ( eui_key_word != NULL ) {
      sprintf(msg,"Next eui to look at from the cursor is %s", eui_key_word );
      DPR(DF2633,msg);
    }
    
  } /*----  End of the tour through each inflected form from the eui table*/

  /* --------------------------
     Close the curser
     -------------------------- */
  
  eui_dbcp->c_close(eui_dbcp); 
  
  eui_dbcp = NULL;
  
  
 bottom:

  
  DEXIT(DT2632);
  return ( return_code );

} /*** End lex_delete */
/**/
/*==========================================================
%FUNCTION NAME
	retrieve_eui_from_record
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = retrieve_eui_from_record(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2636
	FULL  DF2637
%HEADER END
==========================================================*/
int retrieve_eui_from_record(
			     char *eui,    /* Output */
			     char *record  /* Input */
			     )

{
  int return_code = D_S_SUCCESS;
  char *ptr = NULL;
  
  DFNAME("retrieve_eui_from_record");
  DENTER(DT2636);

  sprintf(msg,"Looking at |%s|", record );
  DPR(DF2637,msg);

  if ( (ptr = strstr( record,"entry=E")) != NULL ) {
    
   strncpy( eui, ptr + 6, 8 );
   eui[8]=EOS;

  }
  else {
    strcpy(eui,"");
  }
  
  DEXIT(DT2636);
  return ( return_code );

} /*** End retrieve_eui_from_record */

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_next_record_from_input
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_next_record_from_input(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1602
	FULL  DF1603
%HEADER END
==========================================================*/
int get_next_record_from_input( 
			       FILE *fp,        /* Input  */
			       char *record     /* Output */
			       )

{
  int return_code = D_FALSE; 
  char line[MAXLINE];
  int record_gotten = D_FALSE;

  DFNAME("get_next_record_from_input");
  DENTER(DT1602);


  /* ---------------------------------------------------
     READ from fp until the end of a record is found
     or the eof has been hit
     --------------------------------------------------- */
  strcpy(record, "");
  
  fgets(line, MAXLINE, fp);
  while (( !feof( fp )) && ( record_gotten == D_FALSE ))
  {

    strcat( record, line );

    if ( strcmp( line, END_OF_RECORD ) == 0 )
    {
      record_gotten = D_TRUE ;
    } 
    else
    {
      fgets(line, MAXLINE, fp );
    }
  }

  if ( feof(fp ))
  {
      return_code = D_TRUE;
  }
     

  DEXIT(DT1602);
  return ( return_code );

} /*** End get_next_record_from_input */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_record_from_eui
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_record_from_eui(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2638
	FULL  DF2639
%HEADER END
==========================================================*/
int get_record_from_eui(
			char *btree_index_file,   /* Input   */
			char *lexicon_file_name,  /* Input   */ 
			char *eui,                /* Input   */
			char *record              /* Output  */
			)

{
  int return_code = D_FALSE;
  char           row[MAXLINE];
  DBC     *eui_dbcp = NULL;
  DBT       eui_key;
  DBT      eui_data;
  int          status = 0;
  char       eui_key_word[MAXLINE];
  int           errno = -1;
  char         rowEui[20];
  char        rowTerm[MAXLINE] ; 
  char        offset[256];
  long       record_offset= 0;
  char       *tmp_record = NULL;

  DFNAME("get_record_from_eui");
  DENTER(DT2638);

  DPR(DF2639,"");

  /* -----------------------------------------------
     Grab the inflected forms that have that eui
     forms from the index

     eui|inflected form|keys|offset
     ----------------------------------------------- */
  
  
  /* -----------------------------------------------------
     create a curser to lookup the inflected forms from the eui table
     ----------------------------------------------------- */
  if ((errno = eui_db->cursor(eui_db, NULL, &eui_dbcp,0)) != 0) {
    sprintf(msg,"Not able to grab a cursor for the eui_table") ;
    DPE(msg);
    goto bottom;
  }
  
  memset(&eui_key,  0, sizeof(DBT));
  memset(&eui_data, 0, sizeof(DBT));
  
  
  eui_key.data  = strdup( eui) ; 
  eui_key.size  = sizeof(char ) * (strlen( (char *) eui_key.data )   ) ;
  
  /* -----------------------------------------------------
     Lookup the inflected form in the inflected form table 
     ----------------------------------------------------- */
  status    = eui_dbcp->c_get(eui_dbcp,&eui_key, &eui_data, DB_SET); 
  
  strncpy( eui_key_word, (char *) eui_key.data, eui_key.size );
  eui_key_word[eui_key.size]=EOS;
  
  
  if (( status == 0 ) && ( (void *) eui_data.data != NULL ) && 
	(strcasecmp( eui_key_word, eui ) == 0 )  ) { 
    
    strncpy(row,(char *) eui_data.data, eui_data.size );
    row[eui_data.size]=EOS;
    
    get_value_from_field(1, rowEui,  row );
    get_value_from_field(2, rowTerm, row );
    get_value_from_field(4, offset, row );
    
    
    sprintf(msg,"looking at %s : %s %s [%s]", rowTerm, rowEui,offset, row );
    DPR(DF2633,msg);
    
    
    if ( strcmp( eui, rowEui ) == 0 ) { 				   
      
      record_offset = atol( offset );
      
      tmp_record = get_record_from_offset( record_offset, lexicon_file_name  );

      if ( tmp_record != NULL ) {
	strcpy( record, tmp_record );
	return_code = D_TRUE ;
      } else {
	strcpy( record, "");
	return_code = D_FALSE;
      }
      
    }
  } else {
    strcpy( record, "");
    return_code = D_FALSE;
  }


  eui_dbcp->c_close(eui_dbcp); 
  
  eui_dbcp = NULL;
  
      
 bottom:
      
  DEXIT(DT2638);
  return ( return_code );

} /*** End get_record_from_eui */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_first_lcat_record
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_first_lcat_record(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2646
	FULL  DF2647
%HEADER END
==========================================================*/
int get_first_lcat_record( 
			  char *record,              /* output */
			  char *lexicon_file_name,   /* Input  */
			  char *btree_index_files,   /* Input  */
			  int  *_copt,                /* Input  */
			  int   _ncopt,               /* Input  */
			  char  _topt,                /* Input  */
			  int   _popt                 /* Input  */
			  ) 
     
{
  int return_code = D_FALSE;
  int i = 0;
  DBT       eui_key;
  DBT      eui_data;
  char     eui[255];
  int      errno = 0;
  int      status = 0;
  char       eui_key_word[MAXLINE];
  char       row[MAXLINE];

  DFNAME("get_first_lcat_record");
  DENTER(DT2646);


  strcpy(row,"");
  /* -------------------------------------------------------
     If this is called the first time, initialize the tables
     ------------------------------------------------------- */
  if ( lcat_first_time == D_TRUE ) {

    init_lex_btree( btree_index_files, D_FALSE);
    lexicon_record_fp = (FILE *) init_lex_record_file( lexicon_file_name, D_FALSE );
    lcat_first_time = D_FALSE;

    ncopt = _ncopt;
    topt  = _topt;
    popt  = _popt;
    for ( i = 0; i < ncopt; i++ ) copt[i] = _copt[i];
    
  }
  
  /* --------------------
     Get the first record 
     -------------------- */
  
  /* -----------------------------------------------------
     create a curser to lookup the inflected forms from the eui table
     ----------------------------------------------------- */
  if ((errno = eui_db->cursor(eui_db, NULL, &static_eui_dbcp,0)) != 0) {
    sprintf(msg,"Not able to grab a cursor for the eui_table") ;
    DPE(msg);
    goto bottom;
  }

  memset(&eui_key,  0, sizeof(DBT));
  memset(&eui_data, 0, sizeof(DBT));
  
  
  eui_key.data  = eui; 
  eui_key.size  = sizeof(char ) * (strlen( (char *) eui_key.data )   ) ;
  
  /* -----------------------------------------------------
     Lookup the inflected form in the inflected form table 
     ----------------------------------------------------- */
  status    = static_eui_dbcp->c_get(static_eui_dbcp,&eui_key, &eui_data, DB_FIRST); 
  
  strncpy( eui_key_word, (char *) eui_key.data, eui_key.size );
  eui_key_word[eui_key.size]=EOS;
  

  if ( status != 0 ) {
    sprintf(msg,"Not able to find the first eui cursur : %d\n",status );
    DPE(msg);
    return_code = D_TRUE;
    goto bottom;
  }
  
  strncpy(row,(char *) eui_data.data, eui_data.size );
  row[eui_data.size]=EOS;

  output_lcat_entry( row, record );
		     
     
 bottom:

  DEXIT(DT2646);
  return ( return_code );

} /*** End get_first_lcat_record */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_next_lcat_record
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_next_lcat_record(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2648
	FULL  DF2649
%HEADER END
==========================================================*/
int get_next_lcat_record( 
			 char *record /* Output */
			 )

{
  int return_code = D_FALSE;
  DBT       eui_key;
  DBT      eui_data;
  char     eui[255];
  int      status = 0;
  char       eui_key_word[MAXLINE];
  char        row[MAXLINE];

  DFNAME("get_next_lcat_record");
  DENTER(DT2648);

  memset(&eui_key,  0, sizeof(DBT));
  memset(&eui_data, 0, sizeof(DBT));
  
  strcpy(row,"");
  
  eui_key.data  = eui; 
  eui_key.size  = sizeof(char ) * (strlen( (char *) eui_key.data )   ) ;
  
  /* -----------------------------------------------------
     Lookup the inflected form in the inflected form table 
     ----------------------------------------------------- */
  status    = static_eui_dbcp->c_get(static_eui_dbcp,&eui_key, &eui_data, DB_NEXT); 
  

  strncpy( eui_key_word, (char *) eui_key.data, eui_key.size );
  eui_key_word[eui_key.size]=EOS;
  

  if ( status != 0 ) {
    sprintf(msg,"Not able to find the next eui cursur : %d\n",status );
    DPR(DF2649,msg);
    return_code = D_TRUE;
    goto bottom;
  }
  
  strncpy(row,(char *) eui_data.data, eui_data.size );
  row[eui_data.size]=EOS;

  output_lcat_entry( row, record );

 bottom:
		     
     
  DEXIT(DT2648);
  return ( return_code );

} /*** End get_next_lcat_record */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	output_lcat_entry
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = output_lcat_entry(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2650
	FULL  DF2651
%HEADER END
==========================================================*/
int output_lcat_entry( 
		      char *eui_row,  /* Input  */
		      char *record    /* Output */
		      )
     
{
  int return_code = D_S_SUCCESS;
  int i = 0;
  char str_flags[255];
  int flags = 0;
  char str_offset[255];
  long offset = 0;
  char s[255];
  char key[255];
  static lex_t allcats = (
	LEX_CAT_ADJ | LEX_CAT_ADV | LEX_CAT_AUX | LEX_CAT_COMPL |
	LEX_CAT_CONJ | LEX_CAT_DET | LEX_CAT_MODAL | LEX_CAT_NOUN |
	LEX_CAT_PREP | LEX_CAT_PRON | LEX_CAT_VERB
	);

  DFNAME("output_lcat_entry");
  DENTER(DT2650);


  get_value_from_field( EUI_FLAGS, str_flags,    eui_row );
  get_value_from_field( EUI_TERM,  key ,         eui_row );
  get_value_from_field( EUI_OFFSET,str_offset ,  eui_row );

  flags = atoi( str_flags );
  offset = atol( str_offset );
  
  strcpy(record,"");
    

/* check for appropriate syntactic categories */
    for (i=0; i<ncopt; i++) {
      if (((flags & allcats) & copt[i]) == 0)
	goto bottom; 
    }
    
    switch (popt)
    {
	case PRINT_RECORD:
	    if (!IS_LEX_BASE(flags))	/* only base forms are printed */
		break;

	    if (fseek(lexicon_record_fp, (long) offset, 0) == (-1)) {
	      return_code = D_E_ERROR;
	      goto bottom;
	    }

	    while (fgets(s, sizeof(s), lexicon_record_fp) != (char *)NULL)
	    {

	      strcat(record, s );

	      if (ISEOR(s))
		break;
	    }
	    break;

	case PRINT_BASE:
	    if (!IS_LEX_BASE(flags))	/* only base forms are printed */
		break;
	    
	    strcat(record, key );
	    strcat(record, "\n");
	    break;

	case PRINT_BASESV:
	    if (!IS_LEX_BASE(flags) && !IS_LEX_SV(flags))
		break;
	    
	    strcat(record, key );
	    strcat(record, "\n");
	    break;

	case PRINT_KEY:
	    
	    strcat(record, key );
	    strcat(record, "\n");
	    break;

	case PRINT_INDEX:
		sprintf(record,"%d%c%s%c%d\n", flags, topt, key, topt, (int)offset);
	    break;
	default:
	    return_code = D_E_ERROR;
    }
    
 bottom:
    
  DEXIT(DT2650);
  return ( return_code );

} /*** End output_lcat_entry */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	process_eui
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = process_eui(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2652
	FULL  DF2653
%HEADER END
==========================================================*/
int process_eui(
		 char *eui,              /* Input */
		 FILE *output_file_ptr   /* Output */
		 )

{
  int return_code = D_S_SUCCESS;
  WordList *wl = NULL;

  DFNAME("process_eui");
  DENTER(DT2652);

  
  wl = query_by_eui(iopt, lopt, eui );

    
  output_entries(eui, FROM_EUI_TABLE, wl ,output_file_ptr);
    
  if ( wl != NULL ) 
    free_wl( &wl );

  wl = NULL;
    
  

  DEXIT(DT2652);
  return ( return_code );

} /*** End process_eui */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	query_by_eui
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = query_by_eui(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2654
	FULL  DF2655
%HEADER END
==========================================================*/
WordList * query_by_eui(
			char *btree_index_files,   /* Input   */
			char *lexicon_file_name,  /* Input   */ 
			char *eui                 /* Input   */
			)

{

  WordList *wl = NULL;
  char           row[MAXLINE];
  DBC     *eui_dbcp = NULL;
  DBT       eui_key;
  DBT      eui_data;
  int          status = 0;
  int         errno = 0;
  char       eui_key_word[MAXLINE];
  static int lf_first_time = D_TRUE;


  DFNAME("query_by_eui");
  DENTER(DT2654);

  if ( lf_first_time == D_TRUE ) {

    init_lex_btree( btree_index_files, D_FALSE);
    lexicon_record_fp = (FILE *) init_lex_record_file( lexicon_file_name, D_FALSE );
    lf_first_time = D_FALSE;

  }

  /* -----------------------------------------------
     Grab the rows that have this eui from the eui index :
     forms from the index

     eui|inflected form|keys|offset
     ----------------------------------------------- */
  
  
  /* -----------------------------------------------------
     create a curser to lookup the inflected forms from the eui table
     ----------------------------------------------------- */
  if ((errno = eui_db->cursor(eui_db, NULL, &eui_dbcp,0)) != 0) {
    sprintf(msg,"Not able to grab a cursor for the eui_table") ;
    DPE(msg);
    goto bottom;
  }
  
  memset(&eui_key,  0, sizeof(DBT));
  memset(&eui_data, 0, sizeof(DBT));
  
  
  eui_key.data  = strdup( eui) ; 
  eui_key.size  = sizeof(char ) * (strlen( (char *) eui_key.data )   ) ;
  
  status    = eui_dbcp->c_get(eui_dbcp,&eui_key, &eui_data, DB_SET); 
  

  eui_key_word[eui_key.size]=EOS;
  
  
  if ( status == 0 ) {


    wl = (WordList* )  malloc ( sizeof ( WordList )  );
    wl->words = (char **) malloc ( sizeof ( char * ) * 255 ) ;
    wl->n = 0;
  
    strncpy( eui_key_word, (char *) eui_key.data, eui_key.size );

    while (( status == 0 ) && ( (void *) eui_data.data != NULL ) && 
	   (strcasecmp( eui_key_word, eui ) == 0 )   && 
	   (wl->n < 255 ) ) {
							
      
      strncpy(row,(char *) eui_data.data, eui_data.size );
      row[eui_data.size]=EOS;
      
      WLWN( wl, wl->n ) = strdup (row );
      wl->n++;
      
      status    = eui_dbcp->c_get(eui_dbcp,&eui_key, &eui_data, DB_NEXT); 
      
      strncpy( eui_key_word, (char *) eui_key.data, eui_key.size );
      eui_key_word[eui_key.size]=EOS;
      
      
    } /* end of while loop through all records that match */

  } /* There are any records that match */

  eui_dbcp->c_close(eui_dbcp); 
  
  eui_dbcp = NULL;
  
      
 bottom:

  DEXIT(DT2654);
  return ( wl );

} /*** End query_by_eui */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	generate_and_add_eui_to_record
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = generate_and_add_eui_to_record(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2692
	FULL  DF2693
%HEADER END
==========================================================*/
int generate_and_add_eui_to_record(
				   char *lexFile,          /* Input */
				   char *orig_record,      /* Input */
				   char *new_record,       /* Output - already malloc'd */
				   char *next_entry_number /* Output - already malloc'd */
				   )
     
{
  int return_code = D_S_SUCCESS;
  /* char next_entry_number[9]                             ; */
  char *tok = NULL;
  char eui_line[255];

  DFNAME("generate_and_add_eui_to_record");
  DENTER(DT2692);

  /* -------------------------------------------------
     Derive the path of the entry number file from the
     base of the lexFile path
     -------------------------------------------------*/
  
  
  /* -------------------------------------------------
     get the next number
     -------------------------------------------------*/
	 
  if (c_get_next_entry_number(next_entry_num_file, next_entry_number) !=D_E_ERROR ) {
    
    sprintf(eui_line,"entry=%s",next_entry_number);
    
    
    /* -------------------------------------------------
       replace this in the record 
       -------------------------------------------------*/
      
    strcpy(new_record,"");
    tok = strtok(orig_record,"\n");
    while( tok) {
      if ( strstr( tok, "entry=" ) != NULL ) {
	strcat( new_record , eui_line );
      } else {
	strcat( new_record , tok );
      }
      strcat( new_record , "\n");

      tok = strtok(NULL,"\n");
    }
    
    
  } else {
    fprintf(stderr,"Error Retrieving the next entry Number\n");
    return_code = D_E_ERROR;
  }
  
  DEXIT(DT2692);
  return ( return_code );
  
} /*** End generate_and_add_eui_to_record */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	c_get_next_entry_number
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = c_get_next_entry_number(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2696
	FULL  DF2697
%HEADER END
==========================================================*/
static int  c_get_next_entry_number(
			     char *counter_file,          /* I */
			     char  *next_entry_number     /* O - assumed to have been already mallocd*/ 
			     )

{
  int return_code = D_S_SUCCESS;
  FILE *CounterFp;
  char line[255];
  char next_number_string[9] ;
  int  current_number = 0;
  int  next_number = 0;

  DFNAME("c_get_next_entry_number");
  DENTER(DT2696);

   
  fprintf(stderr,"in c_get_entry_number with %s\n",counter_file );
  
  if ((CounterFp = fopen(counter_file, "r")) == (FILE *)NULL)
  {
     fprintf(stderr, "Could not open lexicon counter file:%s\n", counter_file);
     return_code = D_E_ERROR; 
     goto bottom ;
  }
  
  fgets(line,7,CounterFp);
  fprintf(stderr,"Picked up |%s|\n",line);
  
  /* --------------------------
     Convert line into a number
     -------------------------- */
  current_number =atoi(line);
  fprintf(stderr,"Converted %s to %d\n",line,current_number);
  
  next_number = current_number + 1;
  
  /* --------------------------
     Close the file
     -------------------------- */
  fclose ( CounterFp );
  
  /* --------------------------
     Open the file for write
     -------------------------- */
  
  if ((CounterFp = fopen(counter_file, "w")) == (FILE *)NULL)
  {
     fprintf(stderr,"Could not open counter file for write:\n");
     return_code = D_E_ERROR ;
     goto bottom ;
  }
  
  /* --------------------------------------------------------------------
     Write the next number out to the file, appending a period at the end
     For compatability with prolog's "full stop character"
     -------------------------------------------------------------------- */
  fprintf(CounterFp,"%d.\n",next_number);
  
  /* --------------------------
     Close the file
     -------------------------- */
  fclose ( CounterFp );
  
  /* ---------------------------------------------------
     Format the number by adding an E, and pad with 0's 
     --------------------------------------------------- */
  bq_pad_with_E0s(next_number,next_number_string);
  /*fprintf(stderr,"The next number string is %s\n",next_number_string);*/
  
  
 bottom:
  
  strcpy(next_entry_number, next_number_string);
  sprintf(msg,"Returning %s\n",next_entry_number);
  
  fprintf(stderr,"%s",msg);

  
  

  DEXIT(DT2696);
  return ( return_code );

} /*** End c_get_next_entry_number */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	lf_api
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = lf_api(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2744
	FULL  DF2745
%HEADER END
==========================================================*/
int lf_api(
	   char *term,                /* Input  */
	   char *options,             /* Input  */
	   FILE *output_file_ptr       /* Output */
	   )

{
  int return_code = D_S_SUCCESS;
  WordList *wl = NULL;

  DFNAME("lf_api");
  DENTER(DT2744);


  if ( options != NULL ) {
    /* wl = (WordList *) str2words( options ); */

    wl = (WordList *) tokenize_everything( options, BREAK_ON_SPACE ); 

    return_code = lf__api( term, wl->n, wl->words, output_file_ptr );

    free_wl( &wl );

  } else {

    return_code = lf__api( term, 0, NULL, output_file_ptr );
  }

  
  DEXIT(DT2744);
  return ( return_code );

} /*** End lf_api */


/*==========================================================
%FUNCTION NAME
	pad_with_E0s
%PURPOSE
	Pads the number coming in with 0's up to 7 places, and returns
        an atom of the padded number
%I/O PARAMETER LIST
	+OldNo    Number
        -NewAtom  Atom

%EXAMPLE CALL
	pad_with_EOs(7,NewCtr).
%METHOD
        if the number < 10 
          append "E"
          append 6 0's
        else if number is < 100
          append "E"
          append 5 0's
               .
               .
%RETURNS 
        NLS_S_SUCCESS
%NOTES
	This is a quick and dirty way to do this for the case of 7
        places. At some other time come up with an elegant way to
        do this for X places.
%FILES
	?
%TABLES
	?
%SYNTAX ARGS
==========================================================*/
int
bq_pad_with_E0s(
		 int  OldNo,         /* I */
		 char *number_string /* O */)
{
   /* DFNAME( pad_with_E0s); */
  
   int return_code = D_S_SUCCESS;
   char NewAtom[9];

   /* DENTER(DT); */

   /*fprintf(stderr,"In pad with EOs with %d\n",OldNo);*/

    if (OldNo < 10) 
       sprintf(NewAtom,"E000000%1d",OldNo);

    else if (OldNo < 100)
       sprintf(NewAtom,"E00000%2d",OldNo);

    else if (OldNo < 1000)  
       sprintf(NewAtom,"E0000%3d",OldNo);

    else if (OldNo < 10000)  
       sprintf(NewAtom,"E000%4d",OldNo);

    else if (OldNo < 100000)  
       sprintf(NewAtom,"E00%5d",OldNo);

    else if (OldNo < 1000000)  
       sprintf(NewAtom,"E0%6d",OldNo);

    else 
       sprintf(NewAtom,"E%7d",OldNo);

   /*fprintf(stderr,"NewAtom=%s\n",NewAtom);*/

   strcpy(number_string,NewAtom);
   /*fprintf(stderr,"number_string=%s\n",number_string);*/
     

    /* DEXIT(DT) */

    return ( return_code );
    
 } /*** End pad_with_E0s */
