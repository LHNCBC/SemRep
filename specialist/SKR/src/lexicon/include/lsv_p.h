
/* 
   prevent sccs_id_lsv_p_h from being re-defined in case
   lsv_p.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_lsv_p_h
static char sccs_id_lsv_p_h[] = "@(#)lsv_p.h	1.3 09/27/06";
#define sccs_id_lsv_p_h 1
#endif

/*==========================================================
  PROTOTYPES FOR SOURCE FILE     	compare.c
  ==========================================================*/
extern int categoryCompare( 
		    int catsOne, 
		    int catsTwo 
		    );

extern int inflectionCompare(
		      int inflOne,
		      int inflTwo
		      );

extern int convertCat( int cat);

extern int convertInfl(
		int infl 
		);

/*==========================================================
  PROTOTYPES FOR SOURCE FILE     	loc.c
  ==========================================================*/
extern FILE * push_loc(
		char *includeFile,
		FILE *curFp,
		char **curFileP,
		int *linenoP 
		);

extern FILE * pop_loc(
	       int *linenoP,
	       char **curFileP 
	       );

/*==========================================================
  PROTOTYPES FOR SOURCE FILE     	lragr2im.c
  ==========================================================*/
extern int convert_lragr_principle_part_to_lsv_principle_part(
						       char *cat,
						       char *base_form,
						       char *inflected_form,
						       char *lragr_principle_part,
						       char *principle_part
						       );

/*==========================================================
  PROTOTYPES FOR SOURCE FILE     	lsv.c
  ==========================================================*/
extern LsvOut *lsv_variants(
		     LsvInp *input ,  /* Input  */
		     int *nVarP       /* Output */
		     );

extern int apply_facts( 
		LsvInp      *lsvInp,     /* Input  */
		LsvMatches  *matches     /* Output */
		);

extern int add_match( 
	      int               module_type,     /* Input     */
	      int               type,            /* Input     */
	      int               num,             /* Input     */
	      char              *table,          /* Input     */
	      LsvMatches	*matches          /* Output    */
	      );

extern int make_variants(
		  LsvInp       *lsvInp,      /* Input  */
		  LsvMatches   *matches,     /* Input  */
		  LsvOut      **the_lsvOut,  /* Output */
		  int          *the_n_out    /* Output */ 
		  );

extern int sort_matches( 
		 lsv_t        module_type,    /* Input        */
		 LsvMatches  *matches         /* Input/Output */
		 );

extern void match_xchg(
		LsvMatches *matches,  /* Input/Output */
		int         n1,       /* Input */ 
		int         n2        /* Input */ 
		);

extern int match_cmpfn(
		lsv_t        module_type,     /* Input */
		LsvMatches  *matches,         /* Input */
		int          n1,              /* Input */
		int          n2               /* Input */
		);

extern int match_weight(
		 lsv_t       module_type, /* Input */           
		 LsvMatches  *matches,    /* Input */
		 int          n           /* Input */
		 );

extern int make_im_fact(
		 LsvInp  *lsvInp,    /* Input  */
                 char    *fact,      /* Input  */
		 LsvOut *lsvOut,     /* Output */
		 int     n_out       /* Input  */
		 );

extern int make_im_rule(
		 LsvInp  *lsvInp,        /* Input  */
		 int     ruleNum,       /* Input  */
		 char    *table,        /* Input  */
		 LsvOut  *lsvOut,       /* Output */
		 int     n_out          /* Output */
		 );

extern int make_dm_fact(
		 LsvInp   *lsvInp ,      /* Input  */
		 int       factNum,      /* Input  */
		 LsvOut   *lsvOut,       /* Output */
		 int       n_out         /* Output */
		 );

extern int make_dm_rule(
		 LsvInp   *lsvInp ,      /* Input  */
		 int       ruleNum,      /* Input  */
		 char     *table,        /* Input  */
		 LsvOut   *lsvOut,       /* Output */
		 int       n_out         /* Output */
		 );

extern char *replace_suffix(
		     char *term,   /* Input */
		     char *old,    /* Input */
		     char *new     /* Input */
		     );

extern char * catstr(
	      char    *s,
	      int     *nP,	/* current size of s */
	      char    *t 
	      );

extern int load_im(
	    char *file 
	    );

extern int load_dm(
	    char *file 
	    );

extern int apply_rules(
		LsvInp *lsvInp,
		int trieNum,
		char *term,
		int index,
		char *symbol_table,
		LsvMatches  *matches
		);

extern int free_matches( LsvMatches *the_matches );

extern int is_exception(
		 LsvInp  *lsvInp,
		 char    *term,
		 int     ruleNum 
		 );

extern int free_lsvOut( LsvOut **the_lsvOut,  /* Input /Output */
		 int n_out         /* Input */
		 );

extern int lsv_is_in_lexicon( 
		      char *term,  /* Input */
		      int  cat     /* Input */
		      );

extern int im_facts_lookup( 
		    char             *term,         /* Input  */
		    unsigned int     *cats,         /* Output */
		    unsigned int     *infls         /* Output */
		    );

extern int add_match_aux(
		  char              *fact,            /* input     */ 
		  LsvMatches	    *matches          /* Output    */
		  );

/*
extern int im_facts_lookup_with_euis(
			      LsvInp  *lsvInp,                 * Input  * 
			      unsigned int     *cats,          * Output * 
			      unsigned int     *infls,         * Output * 
			      WordList **euis                  * Output * 
			      );
*/

extern int print_im_rules();

extern int im_pos(
	   LsvInp           *lsvInp,       /* Input  */
	   unsigned int     *cats,         /* Output */
	   unsigned int     *infls         /* Output */
	   );

/*==========================================================
  PROTOTYPES FOR SOURCE FILE     	lsv_dm.c
  ==========================================================*/
extern int read_dm_rule(
		 char *line,
		 char *curFile,
		 int lineno,
		 int ruleNum 
		 );

extern int read_dm_exceptions(
		       char *line,
		       char *curFile,
		       int lineno,
		       int ruleNum,
		       int xpnNum 
		       );

extern int reverse_dm_rule( int ruleNum );

extern void copy_dm_rule(
		  LsvDmRule *toA,
		  int        toN,
		  LsvDmRule *fromA,
		  int        fromN 
		  );

extern int same_dm_rule(
		 int num1,
		 int num2 
		 );

extern int same_dm_exceptions(
		       int num1,
		       int num2 
		       );

extern int read_dm_fact(
		 char *line,
		 char *curFile,
		 int lineno,
		 int factNum 
		 );

extern int is_duplicate_dm_fact( int factNum  );

extern int reverse_dm_fact( int factNum ) ;

/*==========================================================
  PROTOTYPES FOR SOURCE FILE     	lsv_im.c
  ==========================================================*/
extern int read_im_rule(
		 char *line,
		 char *curFile,
		 int lineno,
		 int ruleNum 
		 );

extern int read_im_exceptions(
		       char *line,
		       char *curFile,
		       int lineno,
		       int ruleNum,
		       int xpnNum 
		       );

extern int reverse_im_rule( int ruleNum ) ;

extern void copy_im_rule(
		  LsvImRule *toA,
		  int        toN,
		  LsvImRule *fromA,
		  int        fromN 
		  );

extern int same_im_rule(
		 int num1,
		 int num2 
		 );

extern int same_im_exceptions(
		       int num1,
		       int num2 
		       );

extern int read_im_fact(
		 char *line,
		 char *curFile,
		 int lineno,
		 int factNum 
		 );

extern int is_duplicate_im_fact( int factNum );

extern int reverse_im_fact( int factNum );

/*==========================================================
  PROTOTYPES FOR SOURCE FILE     	lsv_util.c
  ==========================================================*/
extern lsv_t cat_str2lsv(
		  char *cat 
		  );

extern char *cat_lsv2str(
		  lsv_t cat 
		  );

extern lsv_t infl_str2lsv(
		   char *infl 
		   );

extern char *infl_lsv2str(
		   lsv_t infl 
		   );

extern int has_upper(
	      char *s 
	      );

extern void fatal( char *message  );

extern void trim_crlf( char *s);

/*==========================================================
  PROTOTYPES FOR SOURCE FILE     	lsvtrutl.c
  ==========================================================*/
extern int add_to_charbuf(
		   char *s 
		   );

extern int correct_suffix( 
		   int ruleNum,
		   int xpnNum 
		   );

extern int duplicate_exceptions(
			 int ruleNum,
			 int xpnNum 
			 );

extern int load_exception(
		   int ruleNum 
		   );

extern void sort_exception_list(
			 int start,	/* where the exceptions start */
			 int length 	/* how many */
			 );

extern int sort_facts();

extern int * build_fact_index();

