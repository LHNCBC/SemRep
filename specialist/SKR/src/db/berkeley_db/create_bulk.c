
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

/*
% File:	    create_bulk.c
% Module:   Berkeley DB
% Author:   Jim
*/

#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <db.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "embed.h"

void Setup_Env(void);
void parse_config_line(void); 
int main(void);
void get_value_from_field(int pos, char *term, char *line);
void setup_db(char *database_home, char *index_file);
void process_db(char *input_file);

DBT key, content;
DB *db = NULL;
char config_file[MAXLINE + 1];
char database_home[MAXLINE + 1];

int NUM_TABLES;
int first_flag = TRUE;
struct config_struct **config_info;

int main()

{
  char input_file[MAXLINE + 1];
  int i;

  /* Initialize environment dependant variables */

  Setup_Env();

  /* Now deal with the config file */

  parse_config_line();

  /* Now get the data into the program & process it */

  for(i = 0; i < NUM_TABLES; i++)
  {
     printf("Processing table %s (%d of %d) from %s\n",
	    config_info[i]->table, i+1, NUM_TABLES, database_home);

     setup_db(database_home, config_info[i]->table);
     /* sprintf(input_file, "%s/%s\0", database_home, config_info[i]->file_name); */
     sprintf(input_file, "%s/%s", database_home, config_info[i]->file_name);
     process_db(input_file);

     /* ----------------- Close the index ----------------- */
     printf("Closing DB %s...", config_info[i]->table);
     fflush(stdout);
     db->close(db, 0);
     printf("DB %s closed.\n", config_info[i]->table);
     fflush(stdout);
     if (i < NUM_TABLES-1) {
	printf("Sleeping for 5 seconds before processing table %s; hit Control-C to interrupt",
	       config_info[i+1]->table);
	fflush(stdout);
	sleep(5);
	printf("\n\n");
     }
  } /* for */

  /* Free up the structure */

  for(i = 0; i < NUM_TABLES; i++) {
    printf("Freeing space for table %s (%d of %d)\n",
	   config_info[i]->table, i+1, NUM_TABLES);
    fflush(stdout);
    free((char *)config_info[i]);
  }

  printf("Freeing space for config_info\n");
  fflush(stdout);
  free((char*) config_info);
  printf("ALL DONE!\n");
  fflush(stdout);
  return(0);
} /*** End main */

/************************************************************************/

void get_value_from_field(int pos, char *term, char *line)
{
    int i;
    char tmp1[MAXLINE + 1], tmp2[MAXLINE + 1];

    strcpy(tmp1, line);
    for(i = 0; i < pos; i++)
    {
       sscanf(tmp1, "%[^|]|%[^\n]", term, tmp2);
       strcpy(tmp1, tmp2);
    } /* for */
} /* get_value_from_field */

/************************************************************************/

void setup_db(char *database_home, char *index_file)
{
    char database[MAXLINE + 1];

    /* Remove any existing db first */

    sprintf(database,"%s/%s", database_home, index_file );
    (void)unlink(database);

    /* Create, Initialize database object, and open db */

    if((errno = db_create(&db, NULL, 0)) != 0)
      fprintf(stderr, "%s:db_create: %s\n", __FILE__, db_strerror(errno));
    else
    {
       db->set_errfile(db, stderr);
       db->set_errpfx(db, __FILE__);
       db->set_pagesize(db, 8 * 1024);
       db->set_cachesize(db, 0, 64*1024, 0);
       db->set_flags(db, DB_DUP);
       if((errno = db->open(db, NULL, database, NULL, DB_BTREE, DB_CREATE, 0644))!=0)
          db->err(db, errno, "open:%s", database);
    } /* else */
} /* setup_db */

/************************************************************************/

void process_db(char *input_file)
{
    FILE *fp = NULL;
    char line[MAXLINE + 1];
    char term[MAXLINE + 1];
    long term_counter = 0;
    int status = 0;
    int len;

    /* -----------------
       Open the index up
       ----------------- */

    if ((fp = fopen( input_file,"r")) == NULL )
    {
	fprintf(stderr, "fopen error on file %s: %s\n", input_file, strerror(errno));
       exit(1);
    } /* fi */

    /* -----------------
       Add keys
       ----------------- */

    strcpy(line,"");
    fgets(line,MAXLINE,fp) ;
    /* printf("Line %d: %s\n", ++term_counter, line); */
    while ( !feof( fp) )
    {
       len = (int)strlen(line);
       if (len > 0 )
       {      
           get_value_from_field(1, term, line);
           memset(&key, 0, sizeof(DBT));
           memset(&content, 0, sizeof(DBT));
      
           key.data =  term ;
           key.size = (int)strlen(term) + 1;
      
           term_counter++;

           content.data = line;       
           content.size = len + 1;
           status = db->put(db, NULL, &key, &content, 0);

           if((term_counter % 10000) == 0)
           {
	      printf("Processed %s %ld\n", input_file, term_counter);
              fflush(stdout);
           } /* fi */


           if (status != 0 ) 
           {
	      perror("insert/put");
	      fprintf(stderr,"%s: cursor: %s\n", __FILE__, strerror(errno));
           } /* if */
       } /* if */

       /* clean everyone up for the next round */

       strcpy(line,"");
       strcpy(term,"");
       fgets(line,MAXLINE,fp) ;
       /* printf("Line %d: %s\n", term_counter, line); */
    } /* while */
    printf("FINISHED  %s %ld\n", input_file, term_counter);
    fflush(stdout);  

    /* ----------------- Close the data file ----------------- */

    fclose(fp);
} /* process_db */

/************************************************************************
*  parse_config_line - Parse through the config file line and pull out  *
*      the things we think are important at this time.                  *
************************************************************************/

void parse_config_line()
{
   FILE *fp;
   char line[MAXLINE + 1];
   int num_fields, i;
   int pos;
   char tmp[MAXLINE + 1], tmp2[MAXLINE + 1], tmp3[5];

   if ((fp = fopen(config_file,"r")) == NULL)
   {
      fprintf(stderr, "fopen: %s\n", strerror(errno));
      exit(1);
   } /* fi */

   /* Grab the Number of Tables from the FIRST line */

   if(fgets(line, MAXLINE, fp) != NULL)
   {
       sscanf(line, "NUM_TABLES: %d", &NUM_TABLES);
       config_info = (struct config_struct **)
                       malloc(sizeof(struct config_struct *) * NUM_TABLES);
   } /* fi */
   else
   {
      fprintf(stderr, "fgets: %s\n", strerror(errno));
      exit(2);
   } /* else */

   pos = 0;
   while(fgets(line, MAXLINE, fp) != NULL)
   {
      if(line[0] != '#')    /* Skip over comment lines */
      {
        config_info[pos] = 
            (struct config_struct *)malloc(sizeof(struct config_struct));

        config_info[pos]->opened = FALSE;

        /* Grab the easy information */

        sscanf(line, "%[^|]|%[^|]|%d|%[^\n]", config_info[pos]->file_name, 
               config_info[pos]->table, &num_fields, tmp);

        config_info[pos]->num_fields = num_fields;

        /* Grab the field names */

        for(i = 0; i < num_fields; i++)
        {
           sscanf(tmp, "%[^|]|%[^\n]", config_info[pos]->fields[i], tmp2);
           strcpy(tmp, tmp2);
        } /* for */

        /* Now grab the Type information */

        for(i = 0; i < num_fields; i++)
        {
           sscanf(tmp, "%[^|]|%[^\n]", tmp3, tmp2);

           if(strcmp(tmp3, "TXT") == 0)
             config_info[pos]->field_types[i] = TXT_TYPE;
           else
             config_info[pos]->field_types[i] = INT_TYPE;

           strcpy(tmp, tmp2);
        } /* for */
        pos++;
      } /* fi */
   } /* while */

   if(pos < NUM_TABLES)
   {
       NUM_TABLES = pos;
       config_info = (struct config_struct **) realloc(config_info,
                      sizeof(struct config_struct *) * (NUM_TABLES + 1));
   } /* fi */

   fclose(fp);
} /* parse_config_line */


void Setup_Env()
{
  char tmp[MAXLINE + 1];
  char* ptr;
  struct stat buf;

  /* Do we have a default DB_HOME environment variable set ? */

  if(getenv("DB_HOME") != NULL)
     strcpy((char *)database_home,(char *) getenv("DB_HOME"));
  else
     strcpy((char *)database_home, "");

  /* Now prompt user to verify (or get) database home address */

  fprintf(stdout, "Database Home: [%s]: ", database_home); fflush(stdout);
  strcpy(tmp, "");
  fgets(tmp, MAXLINE, stdin);
  ptr = rindex(tmp, '\n');
  if(ptr != NULL) *ptr = '\0';

  /* Make sure we have something or transfer tmp to database_home */

  if((strlen(tmp) == 0) && (strlen(database_home) == 0))
  {
     fprintf(stderr, "ERROR: Must have database home specified!\n");
     exit(3);
  } /* fi */

  else if(strlen(tmp) > 1)
  {
     strcpy((char *)database_home, "");
     sscanf(tmp, "%[^\n]", (char *)database_home);
  } /* else fi */

  /* Now make sure what was specified is accessible */

  if(lstat(database_home, &buf) == -1)
  {
     fprintf(stderr, "ERROR: %s does NOT exist!\n", database_home);
     exit(4);
  } /* fi */


  /* Do we have a default DB_CONFIG environment variable set ? */

  if(getenv("DB_CONFIG") != NULL)
     strcpy(config_file,(char *) getenv("DB_CONFIG"));
  else
     sprintf(config_file, "%s/config", database_home);

  /* Now prompt user to verify (or get) config file home address */

  fprintf(stdout, "Database Config File: [%s]: ", config_file); 
  fflush(stdout);
  strcpy(tmp, "");
  fgets(tmp, MAXLINE, stdin);
  ptr = rindex(tmp, '\n');
  if(ptr != NULL) *ptr = '\0';

  /* Make sure we have something or transfer tmp to config_file */

  if((strlen(tmp) == 0) && (strlen(config_file) == 0))
  {
     fprintf(stderr, "ERROR: Must have database config file specified!\n");
     exit(5);
  } /* fi */
  else if(strlen(tmp) > 0)
    strcpy(config_file, tmp);

  /* Now make sure what was specified is accessible */

  if(stat(config_file, &buf) == -1)
  {
     fprintf(stderr, "ERROR: %s does NOT exist!\n", config_file);
     exit(6);
  } /* fi */

  printf("\n\n");
} /* Setup_Env */
