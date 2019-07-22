#define MAXLINE   5120	/* Default max size of a string                    */
#define MAXCOLS     10	/* Max columns specified in the config file        */
#define TRUE         1	/* Defined value for TRUE                          */
#define FALSE        0	/* Defined value for FALSE                         */
#define INT_TYPE   350	/* Berkeley DB config file field type for ints     */
#define TXT_TYPE   375	/* Berkeley DB config file field type for text     */
#define NEEDMAX     10	/* Max number of tables we might need opened       */
#define MAX_JDS     10	/* Max number of JDs associated w/ a JC            */
#define NUM_JDS    127	/* Max number of JDs                               */

static char embed_sccs_id[] = "@(#)embedD.h	1.2 07/20/04";

/* BERKELEY DB SPECIFIC INFO --------------------------------------------- */

/* Table for holding the configuration information read in from the
   DB/config file.
*/

struct config_struct
{
     char file_name[31];        /* formally MAXLINE */
     char table[31];            /* formally MAXLINE */
     int opened;		/* Boolean TRUE/FALSE if opened already */
     int num_fields;
     char fields[MAXCOLS][21];  /* formally 50 */
     int field_types[MAXCOLS];
};
 
/* Structure definition for holding the tables we want opened */

struct db_needed_struct
{
     int num_needed;
     char table[NEEDMAX][31];
     int table_pos[NEEDMAX];
};

/* Berkley DB specific global static variables */

static int NUM_TABLES;
static struct config_struct **config_info;

/* ----------------------------------------------- END OF BERKELEY DB INFO */

/* PROGRAM SPECIFIC ROUTINE INFO ----------------------------------------- */
 
/* Structure definition for holding the input text we get from user */

struct input_struct
{
    long ui;
    char *title;
    char *abstract;
    char *jc;
    int num_jds;
    char *jds[MAX_JDS];
    int jdsP[MAX_JDS];
};

/* Structure definition for a scaled down input_struct so we only hold
   what we absolutly need for the profiling process.
*/

struct profile_struct
{
    char *uid;
    char *title;
    char *abstract;
    char *jc;
    float jd_profiles[NUM_JDS];
};

/* Our unique word btree structure definition. */

struct tree_node {
  char *word;
  long lui;
  long num_docs_in;
  long tot_cnt;
  long luiT;
  long num_docs_in_Title;
  long tot_cnt_Title;
  long luiA;
  long num_docs_in_Abstract;
  long tot_cnt_Abstract;
  
  int num_uniq_jds;
  long jd_cnts[NUM_JDS];
  float jd_profiles[NUM_JDS];
  long jdcit_cnts[NUM_JDS];
  float jdcit_profiles[NUM_JDS];
  struct tree_node *left;
  struct tree_node *right;

  int loc_freq;
  float norm_cnt;
};

struct tree_node *root;


/* Phrases to be removed from the user supplied text before we do
   any further processing.  This is done before we tokenize the string.
*/

#define  num_remv 6           /* Number of phrases entered below */
#define  published_remv 4     /* Position of published erratum - spec handle */
#define  copyright_remv 5
#define  see_comments 3

char *remv_str[] = { "(ABSTRACT TRUNCATED AT 250 WORDS)", 
                     "(ABSTRACT TRUNCATED AT 400 WORDS)",
                     "(see comments)",
                     "[see comments]",
                     "published erratum appears in",
                     "Copyright 199" }; 


/* array to store stopwords from file */

#define StopwordsSize 16384

static char *stopwords[StopwordsSize];
static int stopwords_count;

#define RestrictwordsSize 400000

static char *restrictwords[RestrictwordsSize];
static int restrictwords_count;

static char *outsiderangewords[1024];


/* --------------------------------------------------- END OF PROGRAM INFO */
