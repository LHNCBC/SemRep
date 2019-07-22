/*==========================================================

%SOURCE FILE
	lvg_api.h

%DESCRIPTION OF FILE
	C source header file.
%REVISED
	Sep 18 1996 divita -- Initial Version

==========================================================*/


/* 
   prevent sccs_id_lvg_p_h from being re-defined in case
   lvg_p.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_lvg_p_h
static char sccs_id_lvg_p_h[] = "@(#)lvg_p.h	1.3 09/27/06";
#define sccs_id_lvg_p_h 1
#endif

#ifndef _LVG_API_
#define _LVG_API_
#endif




     /* ==========================================
        Prototypes for file acronyms.c
        ==========================================  */
WordList *compress_acronyms( char *term );
WordList *expand_acronyms( char *term );

int close_acronyms_exp_index();
int close_acronyms_index();


     /* ==========================================
        Prototypes for file baser.c
        ==========================================  */
WordList *word2base(
		    char *word,
		    lsv_t cats
		    );
		    
WordList *generateBaseForms();


     /* ==========================================
        Prototypes for file captureMS.c
        ==========================================  */
int lvgCaptureMS();
int captureMS();


     /* ==========================================
        Prototypes for file case.c
        ==========================================  */
int isOnlyFirstCharCapitalized();
int isEntireTermLowerCase();
int isEntireTermUpperCase();
int capitalizeFirstChar();
int lowercaseEntireTerm();
int upperCaseEntireTerm();
int fixCase();
int isMixedCase();


     /* ==========================================
        Prototypes for file cit_form.c
        ==========================================  */
int get_citation_form();


     /* ==========================================
        Prototypes for file comMspel.c
        ==========================================  */
int commonMisspelledWords();
int lvg_commonMisspelledWords();


     /* ==========================================
        Prototypes for file diacritic.c
        ==========================================  */
int normalizeDiacritics( 
			unsigned char *origTerm,    /* Input  */
			unsigned char *outputTerm   /* Output */
			) ;

int normalizeDiacritic( unsigned char x );
int lvg_normalizeDiacritics();


     /* ==========================================
        Prototypes for file format.c
        ==========================================  */
char  *formatColumns( 
		     GlobalState *globalState,     /* Input  */  
		     char        *rawOutput        /* Input  */
		     ); 
     
int initialize_global_columns( 
			      GlobalState *globalState, /* Input /Output */
			      char *arg_options         /* Input         */
			      ) ;
int formatPreserveCase();


     /* ==========================================
        Prototypes for file genVars.c
        ==========================================  */
int generate_variants();


     /* ==========================================
        Prototypes for file genitive.c
        ==========================================  */
int removeGenitives();


     /* ==========================================
        Prototypes for file hyphen.c
        ==========================================  */
int squeeze_out_hyphens();


     /* ==========================================
        Prototypes for file hyphen_words.c
        ==========================================  */
int lookup_hyphenated_words();
int load_hyphens();


     /* ==========================================
        Prototypes for file i2sands2i.c
        ==========================================  */
int cat_i2s();
int cat_s2i();
int infl_i2s();
int infl_s2i();


     /* ==========================================
        Prototypes for file inflect.c
        ==========================================  */
int lvg_infl();
int lvg_uninflect();


     /* ==========================================
        Prototypes for file info.c
        ==========================================  */
int strip_non_info_words();
int is_word_a_conjunction();
int free_non_info_words();


     /* ==========================================
        Prototypes for file isAcronym.c
        ==========================================  */
int isAcronym();


     /* ==========================================
        Prototypes for file kCompare.c
        ==========================================  */
int kCompare( char *pattern1, char *pattern2 ) ;
int showD(int ***D, int m, int n);


     /* ==========================================
        Prototypes for file look.c
        ==========================================  */
long bsearch_start();
long bsearch_get();
long bsearch_next();
int bsearch_close();
long bsearch_get_all_rows();


     /* ==========================================
        Prototypes for file lookup.c
        ==========================================  */
int lvg_lookup();
int is_in_lexicon();


     /* ==========================================
        Prototypes for file lragr.c
        ==========================================  */
int lvg_lragr();
int lvg_lragr_with_lragr_output();
int lvg_lragr_with_lvg_output();
int lvg_lragr_with_prefix_search();
int lragr_lookup();
int lragr_lookup_with_prefix_search();


     /* ==========================================
        Prototypes for file lvg.c
        ==========================================  */
int lvg(
	char *options,  /* Input  */
	char *line,     /* Input  */
	char **variants /* Output */
	);

WordList *lvg_aux1(
		   char *options,  /* Input  */
		   char *line      /* Input  */
		   );
WordList *lvg_aux2(
		   char *options,  /* Input  */
		   char *line      /* Input  */
		   );
int  lvgInit(
	     char      *commandLineOptions,   /*input  */
	     GlobalState  *globalState        /* Output */
	     );
     
int lvgProcessTerm(
		   GlobalState  *globalState,   /* Input  */
		   char         *line,          /* Input  */
		   WordList    **results        /* Output */
		   );

int lvg__api(
	     char *line,             /* Input  */
	     char *options,          /* Input  */
	     FILE *output_file_ptr   /* Output */
	     );

int lvgParseNewCommand(
		       char        *commandLineOptions, /* input  */
		       GlobalState *globalState         /* Output */
		       );
int lvgCleanUp( GlobalState *globalState /* Input */);

int norm(
	 char *options,  /* Input  -Can be [-n|-m|-tX|-s'X'|-v] */
	 char *line,     /* Input  */
	 char **variants /* Output */
	 );

int normCleanUp();

int norm_api(
	     char *line,             /* Input  */
	     char *options,          /* Input  */
	     FILE *output_file_ptr   /* Output */
	     );

int lvg_no_clean_up(
		    char *options,  /* Input  */
		    char *line,     /* Input  */
		    char **variants /* Output */
		    );



     /* ==========================================
        Prototypes for file lvgNGram.c
        ==========================================  */
int lvgSuggestNGramSpellings(
			     LexItemList *in,
			     LexItemList **outP,
			     int         spellOpt
			     );

int lvgSuggestBestSpellings(
			    LexItemList *in,
			    LexItemList **outP, 
			     int         spellOpt
			    );



     /* ==========================================
        Prototypes for file lvgUtil.c
        ==========================================  */


     /* ==========================================
        Prototypes for file lvg_opts.c
        ==========================================  */
int get_lvg_options( 
		    char         *options,                            /* Input  */
		    GlobalState  *globalState                         /* Output */
		    ) ;

/*
int add_flow(
	     char         *opt,              * Input * 
	     FlowElem      flows[LVG_MAX_FLOWS][LVG_MAX_TAGS],  * Output * 
	     int           flowSize[LVG_MAX_FLOWS],
	     int          *numFlows   * Input/Output * 
	     );
*/
int add_flow(
	     char         *opt,             /* Input */
	     GlobalState  *globalState                         /* Output */
	     );
	     
int initialize_lvg();
int lvgInitGlobalState();


     /* ==========================================
        Prototypes for file lvgapi.c
        ==========================================  */
int init_lexlist( LexItemList **llpp );
void free_lexlist( LexItemList **listPP );
LexItemList * lvg_api(
		      LexItemList *input,		/* must be allocated from heap    */
		      char        *flow,		/* string of module abbreviations */
		      lsv_t        cats,		/* output categories              */
		      lsv_t        infls,		/* output inflections             */
		      int          inflOpt,
		      int          derivOpt,
		      int          tokenOpt,
		      int          caseOpt,
		      int          spellOpt,             /*<- New for 2000                 */
                      char         *originalInputTerm   /*<- New for 2000                 */
		      );




     /* ==========================================
        Prototypes for file lvgapinorm.c
        ==========================================  */
int lvg_norm();
int lvg_norm_db_word();


     /* ==========================================
        Prototypes for file lvggenvr.c
        ==========================================  */
int lvg_retrieve_variants();
int lvg_retrieve_recursive_derivations();
int lvg_retrieve_recursive_synonyms();
int lvg_generate_variants();
int recursive_synonyms();
int recursive_derivations(
			  char *term,                  /* Input  */
			  int  cats,                   /* Input  */
			  WordList **all_derivations,  /* Output */
			  char  *iteration_string,     /* Output */
			  int   *iterations            /* Output */
			  );


     /* ==========================================
        Prototypes for file metaPhon.c
        ==========================================  */
int generateKey();


     /* ==========================================
        Prototypes for file modules.c
        ==========================================  */
int lvg_noop( LexItemList *in, LexItemList **outP );
int lvg_lower( LexItemList *in, LexItemList **outP );
int lvg_uninv( LexItemList *in, LexItemList **outP );
int lvg_gen( LexItemList *in, LexItemList **outP );
int lvg_spell( LexItemList *in, LexItemList **outP );
int lvg_wordord( LexItemList *in, LexItemList **outP );
int lvg_punct( LexItemList *in, LexItemList **outP );
int lvg_base( LexItemList *in, LexItemList **outP );
int lvg_deriv( LexItemList *in, LexItemList **outP, lsv_t outCats, int derivOpt );
int lvg_stop( LexItemList *in, LexItemList **outP );
int lvg_old_wordbase( LexItemList *in, LexItemList **outP );
int lvg_expand_acronyms( LexItemList *in, LexItemList **outP );
int lvg_compress_acronyms( LexItemList *in, LexItemList **outP );
int lvg_synonym( LexItemList *in, LexItemList **outP );
int lvg_syntatic_uninvert( LexItemList *in, LexItemList **outP );
int lvg_base_spell( LexItemList *in, LexItemList **outP );
int lvg_lexfilter( LexItemList *in, LexItemList **outP );
int lvg_strip_ambigious_tags( LexItemList *in, LexItemList **outP );
int lvg_strip_nec_nos( LexItemList *in, LexItemList **outP );
int lvg_ipunct( LexItemList *in, LexItemList **outP );
int lvg_stripNumberWordsFromTerm( LexItemList *in, LexItemList **outP );
int lvg_infl_old( LexItemList *in, LexItemList **outP, lsv_t outCats, lsv_t outInfls, int inflOpt );
   



     /* ==========================================
        Prototypes for file nGram.c
        ==========================================  */
void       nGramIndex         ( char *docName       /* Input */ );
WordList * nGramBreakIntoGrams( char *documentName  /* Input */ );
/*
  Gram *nGramGetGram(
  char *gramName  //  Input  //
  );
*/

void nGramAddGram( 
		  char *gram,           /* Input */
		  int documentIndex     /* Input */
		  );

void nGramAddDocToGram ( );

/*
			Gram *gram,             // Input //
			int   currentDocument   // Input //
			) ;
*/

void nGramAddDocument( 
		      char *docName,         /* Input */
		      int   documentIndex    /* Input */  
		      );
/*
NumberList *nGramGetDocumentsFromGram( int gramRow // Input // );
int nGramGetDocument();
*/

WordList *nGramFind( 
		    char *docName,    /* Input */
		    int  find_level   /* Input */
		    );

int nGramKRank();
int nGramOrDocumentIndexes();
int nGramAndDocumentIndexes();
int nGramFreeGram();
void  nGramAddDocumentToDocuments( 
				  int row,            /* Input */
				  int documentNumber  /* Input */
				  );

void nGramAddDocuments( 
		       int rowNumber,         /* Input */
		       int documentNumber     /* Input */
		       );

void nGramInitFind();
void nGramIndexAll();
void initIndexAll();
void freeDocuments();
void nGramFindAll( int level );

int nGramClose();


     /* ==========================================
        Prototypes for file norm.c
        ==========================================  */
int word2NormForm();
int query_norm_form();


     /* ==========================================
        Prototypes for file proper.c
        ==========================================  */
int lvg_filter_out_proper_nouns();
int is_this_a_proper_noun();
int lvg_filter_out_acronyms();


     /* ==========================================
        Prototypes for file query.c
        ==========================================  */
int query_norm_word();
int close_norm_index();
int norm_alt();
int init_norm_lookup();


     /* ==========================================
        Prototypes for file setPaths.c
        ==========================================  */
int nls_set_paths();


     /* ==========================================
        Prototypes for file load_lragrlist.c
        ==========================================  */
int load_lragrlist();
int attach_lragrlist();
int clean_up_lragrlist();
int shm_query_lragr_word();
static int node_compare();
static int node_compare1();
int lragr_update_shared_memory_file();
char *query_lragr_name_from_number();
int  lr_strcat();
int shm_query_lragr_term();
int shm_query_lragr_term_with_lookahead();


     /* ==========================================
        Prototypes for file load_normlist.c
        ==========================================  */
int load_normlist();
int attach_normlist();
int clean_up_normlist();
int shm_query_norm_word();
static int node_compare();
int update_shared_memory_file();
char *query_norm_name_from_number();


     /* ==========================================
        Prototypes for file signal.c
        ==========================================  */
void handle_kills();
int lvg_clean_up( GlobalState *globalState );
int cleanupGlobalState();


     /* ==========================================
        Prototypes for file sortLexI.c
        ==========================================  */
int sortLexicalItems();
int lexItemCompare();


     /* ==========================================
        Prototypes for file spellNrm.c
        ==========================================  */
int lvg_spelling_normalization();


     /* ==========================================
        Prototypes for file stop_wds2.c
        ==========================================  */
int read_stop_words();
int is_this_a_stop_word(
			char *word 
			);
static int str_compare(
			const void *node1, 
			const void *node2
		       );
int retrieve_normed_stop_words(
			       char *word,      /* input */
			       char **norm_row, /*output */
			       int   *norm_number  /*output */
			       );
int strip_multi_word_stop_words_out( char *old_term, char *new_term );


     /* ==========================================
        Prototypes for file strip_nec.c
        ==========================================  */
int strip_nec_and_nos();


     /* ==========================================
        Prototypes for file strip_pun.c
        ==========================================  */
int strip_punctuation();


     /* ==========================================
        Prototypes for file strip_tags.c
        ==========================================  */
int strip_ambigious_tags();


     /* ==========================================
        Prototypes for file suggestSpelling.c
        ==========================================  */
int suggestSpelling(
		    char *term,               /* Input   */ 
		    WordList **suggestions,   /* Output  */ 
		    int   spellOpt            /* Input   */
		    );
int lvgSuggestSpellings(
			LexItemList   *in,           /* Input  */
			char          *origTerm,     /* Input  */
			LexItemList  **outP,         /* Output */
			int            spellOpt      /* Input  */
			);


     /* ==========================================
        Prototypes for file sv.c
        ==========================================  */
int spelling_variants();


     /* ==========================================
        Prototypes for file syn_unin.c
        ==========================================  */
int syntatic_uninvert();


     /* ==========================================
        Prototypes for file synonyms.c
        ==========================================  */
WordList *synonyms( char *term, /* Input */
		    int  cats   /* Input */
		    );

int close_synonyms();


     /* ==========================================
        Prototypes for file token.c
        ==========================================  */
int * old_tokenize(
		   char *s,			/* string to tokenize */
		   int *nP 			/* number of tokens generated */
		   );

int * old_alt_tokenize(
		       char *s,			/* string to tokenize */
		       int *nP 			/* number of tokens generated */
		       );

void free_Static_tokens();




     /* ==========================================
        Prototypes for file tokenize.c
        ==========================================  */
int lvg_tokenize();
WordList *tokenize_everything(
		   char *term,            /* Input */
		   int tokenize_style    /* Input */
		   ) ;

WordList * tokenize_by_russells_rules( 
				      char *term /* Input */
				      );


     /* ==========================================
        Prototypes for file tokenize.c
        ==========================================  */
WordList *tokenize(
		   char *term,            /* Input */
		   int tokenize_style    /* Input */
		   ) ;



     /* ==========================================
        Prototypes for file untag.c
        ==========================================  */
int lvg_untag();
int untag();


     /* ==========================================
        Prototypes for file usage.c
        ==========================================  */
int usage(
	  char optArg,
	  char *fileName );


     /* ==========================================
        Prototypes for file version.c
        ==========================================  */
int version( char version_options );


     /* ==========================================
        Prototypes for file wdbase.c
        ==========================================  */
int lvg_wordbase();


     /* ==========================================
        Prototypes for file wl.c
        ==========================================  */

void sort_words( char **a, int n );
void sort_words_by_number( char **a, int n,int columnNumber ) ;
void fsort_words( char **a, int n, int field );
WordList * str2words( char *s, int sortFlag );
WordList *str2words_break_only_on_spaces( char *s );
int wl_cmp( WordList *wl1, WordList *wl2 );
void free_wl( WordList ** wl );
void add_word_to_list( WordList **a_wl, char *row  );
int is_on_list ( char *term , int   field, WordList* wl );

int add_wordLists(
		  WordList **combinedList, /* Input/Output */
		  WordList  *list,         /* Input        */
		  int       *a,            /* Input/Output */
		  int       uniq           /* Input        */
		  );

int add_wordListsWithMsg();


     /* ==========================================
        Prototypes for file words.c
        ==========================================  */
char **generate_words(
		      char *term,
		      int *n_words 
		      );

char ** alt_generate_words(
			   char *term,
			   int *n_words 
			   );

void wl_free_static_buff( 
			 char *charbuf, 
			 char **wordbuf
			 );

