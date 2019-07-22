
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
% File:	    sql.c
% Module:   Berkeley DB
% Author:   Jim
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "embed.h"


extern int NUM_TABLES;
extern int first_flag;
extern struct config_struct **config_info;
void btree_query(char *query, char ***q_results, int *numberOfResults, int pos);
extern void init_dbs(char *db_path);
extern void destroy_dbs(void);
extern void open_dbs(int dbptr);

int main(void);
struct results_struct process_normal_query(const char *line);
struct results_struct  process_special_query(const char *line);
struct query_struct parse_query(const char *line);
int get_config(char *tablename);
struct res_rows_struct *parse_results(struct query_struct query,
				      int config_ptr, char *result);
void get_value_from_field_jgm(int pos, char *to, char *from, int flag);
int find_fieldpos(char *find, int config_ptr);
struct results_struct return_results(struct query_struct query, int config_ptr);
struct results_struct return_result(struct query_struct query, int config_ptr);
void free_query(struct query_struct query);
char *parse_string(char *str, char *leftover);

int main(void)
{
   char line[MAXLINE];
   struct results_struct results;
   int row,col = 0;
   init_dbs("");
   while(fgets(line, MAXLINE, stdin) != NULL)
   {
      if(strlen(line) > 1)
      {
         printf("\n\nQuery:\n  %s\n", line); fflush(stdout);

         if(((strstr(line, "all_words") != NULL) &&
            (strstr(line, "all_words_counts") == NULL)) ||
           ((strstr(line, "first_words") != NULL) &&
            (strstr(line, "first_words_counts") == NULL)))
          results = process_special_query(line);
        else
          results = process_normal_query(line);

        for(row = results.num_rows - 1; row >= 0; row--)
        {    
           for(col = results.rows[row]->num_cols - 1; col >= 0; col--)
           {
              switch(results.rows[row]->col_type[col])
              {
                 case TXT_TYPE:
                   printf("Char: %s\n", results.rows[row]->str_result[col]);
                   free(results.rows[row]->str_result[col]);
                 break;

                 case INT_TYPE:
                   printf("Int: %d\n", results.rows[row]->int_result[col]);
                 break;
               } /* switch */
           } /* End for loop thru cols */
           free((char *)results.rows[row]);
        }/* End for loop thru rows */

        free((char *)results.rows);
      } /* fi */
   } /* while */

   destroy_dbs();
   return(1);
} /*** End main */

/************************************************************************/

struct results_struct process_normal_query(const char *line)
{
   struct query_struct query;
   struct results_struct rtn;
   int config_ptr;

   /* printf("NORMAL QUERY\n"); fflush(stdout); */

   query = parse_query(line);
   /* printf("Query = >%s<\n", line);
    * fflush(stdout);
    * printf("query.table = >%s<\n", query.table);
    * fflush(stdout);
    */
   config_ptr = get_config(query.table);
   /* printf("Got config_ptr %d\n", config_ptr);
    * fflush(stdout);
   */
   rtn = return_results(query, config_ptr);
   free_query(query);
   return(rtn);
} /* process_normal_query */ 

/************************************************************************/
 
struct results_struct process_special_query(const char *line)
{
   /* Known information -
      suistrings - Will always test sui & request nmstr, str
      cuiconcept - Will always test cui & request concept
      Use ONE of -- all_words, first_words_of_one, first_words_of_two,
                    first_wordsb or first_words
        requesting sui & cui fields where word = some query string.
   */
 
   int i, j, k = 0;
   struct results_struct outer_results, res1, rtn;
   struct query_struct query, query_strings, query_concepts;
   int strings_ptr, concepts_ptr, query_ptr;
   char *tmpJ, *tmpJ1, *tmpJ2;
   long linelen = (long)strlen(line);
   long maxlen;
 
   maxlen = (linelen > MAXLINE) ? linelen : MAXLINE;

   tmpJ  = (char *)malloc((size_t)(maxlen + 1));
   tmpJ1 = (char *)malloc((size_t)(maxlen + 1));
   tmpJ2 = (char *)malloc((size_t)(maxlen + 1));

   /* Initialize return structure */

   /* printf("SPECIAL QUERY\n"); fflush(stdout); */
 
   rtn.rows = (struct res_rows_struct **)malloc(sizeof(struct res_rows_struct));
   rtn.num_rows = 0;
   rtn.config_ptr = -1;
 
   /* Initialize the query_strings structure based on strings table needs */
 
   query_strings.num_fields = 2;
   query_strings.fields[0] = (char *)strdup("nmstr");
   query_strings.fields[1] = (char *)strdup("str");
   query_strings.table = (char *)strdup("suistrings");
   query_strings.query = (char *)malloc((size_t)MAXLINE);
   query_strings.where2 = NULL;
   query_strings.query2 = NULL;
   strings_ptr = get_config(query_strings.table);
 
   /* Initialize the query_concepts structure based on concept table needs */
 
   query_concepts.num_fields = 1;
   query_concepts.fields[0] = (char *)strdup("concept");
   query_concepts.table = (char *)strdup("cuiconcept");
   query_concepts.query = (char *)malloc((size_t)MAXLINE);
   query_concepts.where2 = NULL;
   query_concepts.query2 = NULL;
   concepts_ptr = get_config(query_concepts.table);
 
   /* --- Based on known/anticipated reasons for entering this routine --- */
 
   if(strstr(line, "all_words") != NULL)
     query.table = (char *)strdup("all_words");
   else if(strstr(line, "first_words_of_one") != NULL)
     query.table = (char *)strdup("first_words_of_one");
   else if(strstr(line, "first_words_of_two") != NULL)
     query.table = (char *)strdup("first_words_of_two");
   else if(strstr(line, "first_wordsb") != NULL)
     query.table = (char *)strdup("first_wordsb");
   else
     query.table = (char *)strdup("first_words");
 
   query_ptr = get_config(query.table);

   strcpy(tmpJ1, "");
   strcpy(tmpJ2, "");
   sscanf(line, "%s %*s %*[^=]='%[^\n]", tmpJ1, tmpJ2);
   query.query = parse_string(tmpJ2, tmpJ);
   query.num_fields = 2;
   query.fields[0] = (char *)strdup("sui");
   query.fields[1] = (char *)strdup("cui");
   query.where2 = NULL;
   query.query2 = NULL;
 
   /* --- Now retrieve the outer layer of rows (sui, cui from words table --- */
 
   outer_results = return_results(query, query_ptr);
 
   /* ------ Now loop through all of the sui,cui combinations we find ------ */
 
   if (outer_results.num_rows > 0)
   {
      rtn.rows = (struct res_rows_struct **)realloc(rtn.rows, sizeof(struct res_rows_struct*) * (outer_results.num_rows * 2));
 
      /* ------------ Grab Strings information ------------ */
 
      for (i = 0; i < outer_results.num_rows; i++)
      {
          free(query_strings.query);
          query_strings.query = 
             (char *)malloc(strlen(outer_results.rows[i]->str_result[0]) + 1);
          strcpy(query_strings.query, outer_results.rows[i]->str_result[0]);
          free(outer_results.rows[i]->str_result[0]);
          res1 = return_result(query_strings, strings_ptr);
 
          if(res1.num_rows > 0)
          {
             rtn.rows[rtn.num_rows] = (struct res_rows_struct *)malloc(sizeof(struct res_rows_struct));
             rtn.rows[rtn.num_rows]->num_cols = 3;
             for(k = 0; k < rtn.rows[rtn.num_rows]->num_cols; k++)
               rtn.rows[rtn.num_rows]->col_type[k] = TXT_TYPE;
 
             rtn.rows[rtn.num_rows]->str_result[0] =
                                  (char *)strdup(res1.rows[0]->str_result[0]);
             rtn.rows[rtn.num_rows]->str_result[1] =
                                  (char *)strdup(res1.rows[0]->str_result[1]);
             rtn.num_rows++;
 
             free(res1.rows[0]->str_result[0]);
             free(res1.rows[0]->str_result[1]);
             free((char *)res1.rows[0]);
          } /* fi */
          free((char *)res1.rows);
      } /* for */
 
      /* ------------ Grab Concepts information ------------ */
 
      j = 0;
      for (i = 0; i < outer_results.num_rows; i++)
      {
          if(strcmp("C.......", outer_results.rows[i]->str_result[1]) == 0)
            rtn.rows[j++]->str_result[2] = (char *)strdup("X");
          else
          {                                                /* cui */
             free(query_concepts.query);
             query_concepts.query = 
               (char *)malloc(strlen(outer_results.rows[i]->str_result[1]) + 1);
             strcpy(query_concepts.query, outer_results.rows[i]->str_result[1]);
             res1 = return_result(query_concepts, concepts_ptr);

             if(res1.num_rows > 0)
               rtn.rows[j++]->str_result[2] = 
                      (char *)strdup(res1.rows[0]->str_result[0]);
             else
               rtn.rows[j++]->str_result[2] = (char *)strdup("");

             free(res1.rows[0]->str_result[0]);
             free((char *)res1.rows[0]);
             free((char *)res1.rows);
          } /* else */
 
          free(outer_results.rows[i]->str_result[1]);
          free((char *)outer_results.rows[i]);
      } /* for */
 
      if(outer_results.num_rows <= 0)
        free((char *)outer_results.rows[0]);
   } /* fi */
   else
     rtn.num_rows = 0;
   free((char *)outer_results.rows);

   free_query(query); free_query(query_strings);free_query(query_concepts);
   free(tmpJ); free(tmpJ1); free(tmpJ2);
   return(rtn);
} /* process_special_query */


/************************************************************************/
 
struct results_struct return_results(struct query_struct query, int config_ptr)
{
   /* Given query and config information, apply the query to the given table,
      pull out the specified column(s) and pass back only the first row found.
   */
 
   struct results_struct rtn;
   char **rows;
   int numrows = 0, i, db_id;
 
   rtn.num_rows = 0;
   rtn.config_ptr = config_ptr;
 
   db_id = get_config(query.table);

   /* Check to make sure the db we want to use is open already, if not, open
      it and set the opened flag so we don't do it again. 
   */

   if(!config_info[db_id]->opened)
   {
      open_dbs(db_id);
      config_info[db_id]->opened = TRUE;
   } /* fi */

   btree_query(query.query, &rows, &numrows, db_id);

   if (numrows > 0)
   {
      rtn.rows = (struct res_rows_struct **) malloc(sizeof(struct res_rows_struct) * numrows);
      for(i = 0; i < numrows; i++)
      {
         if (rows[i] != NULL)
         {
            rtn.rows[rtn.num_rows] = parse_results(query, config_ptr, rows[i]);
            if(rtn.rows[rtn.num_rows]->num_cols > 0)
              rtn.num_rows++;
            else
               free((char *)rtn.rows[rtn.num_rows]);

            free(rows[i]);
         } /* fi */
      } /* for */

      free(rows);
      rows = NULL;
   } /* fi */
   else
   { /* So we have at least defined/allocated the 0th element to return */
      rtn.rows = (struct res_rows_struct **)malloc(sizeof(struct res_rows_struct));
      rtn.num_rows = 0;
   } /* else */
   return(rtn);
} /* return_results */
 
/************************************************************************/
 
struct results_struct return_result(struct query_struct query, int config_ptr)
{
   /* Given query
   */
 
   struct results_struct rtn;
   char **rows;
   int numrows = 0, db_id;
 
   rtn.num_rows = 0;
   rtn.config_ptr = config_ptr;
   rtn.rows = (struct res_rows_struct **)malloc(sizeof(struct res_rows_struct));
 
   db_id = get_config(query.table);

   /* Check to make sure the db we want to use is open already, if not, open
      it and set the opened flag so we don't do it again. 
   */

   if(!config_info[db_id]->opened)
   {
      open_dbs(db_id);
      config_info[db_id]->opened = TRUE;
   } /* fi */

   btree_query(query.query, &rows, &numrows, db_id);
 
   if (numrows > 0)
   {
      rtn.num_rows = 1;
      if (rows[0] != NULL)
      {
         rtn.rows[0] = parse_results(query, config_ptr, rows[0]);
         free(rows[0]);
      } /* fi */
      free(rows);
      rows = NULL;
   } /* fi */
   return(rtn);
} /* return_result */
 
/************************************************************************/
 
struct query_struct parse_query(const char *line)
{
   struct query_struct rtn;
   int cnt, done;
   char *tmp, *tmpf, *tmp1, *tmp2, *modline, *field;
   long linelen = (long)strlen(line);
   long maxlen;
 
   maxlen = (linelen > MAXLINE) ? linelen : MAXLINE;
 
   tmp     = (char *)malloc((size_t)(maxlen + 1));
   tmpf    = (char *)malloc((size_t)(maxlen + 1));
   tmp1    = (char *)malloc((size_t)(maxlen + 1));
   tmp2    = (char *)malloc((size_t)(maxlen + 1));
   modline = (char *)malloc((size_t)(maxlen + 1));
   field   = (char *)malloc((size_t)(maxlen + 1));

   memset(tmp,     0, (size_t)maxlen);
   memset(tmpf,    0, (size_t)maxlen);
   memset(tmp1,    0, (size_t)maxlen);
   memset(tmp2,    0, (size_t)maxlen);
   memset(modline, 0, (size_t)maxlen);
   memset(field,   0, (size_t)maxlen);
 
   /* Initialize all of the structure to nulls */
 
   rtn.num_fields = 0;
   rtn.where2 = NULL;
   rtn.query2 = NULL;
 
   /* SELECT fields FROM  */
 
   strcpy(tmp, ""); 
   sscanf(line, "%*s %[^\n]", tmp);
   cnt = 0;
   done = FALSE;
   while(!done && (cnt < MAXCOLS))
   {
      strcpy(field, "");
      strcpy(tmpf, "");
      sscanf(tmp, "%s %[^\n]", field, tmpf);
 
      if((strlen(field) <= 1) || (strcmp(field, "from") == 0))
        done = TRUE;
      else /* Parse out the field we have and store it */
      {
         rtn.fields[cnt] = (char *)malloc(strlen(field) + 1);
         strcpy(rtn.fields[cnt], "");
         sscanf(field, "%[^,]", rtn.fields[cnt]);
         cnt++;
      } /* else */
 
      strcpy(tmp, tmpf);
   } /* while */
   rtn.num_fields = cnt;
   strcpy(modline, tmp);
 
   /* table WHERE field='querystring' */
 
   strcpy(tmp, "");
   strcpy(tmp1, "");
   strcpy(tmp2, "");
 
   sscanf(modline, "%s %*s %*[^=]='%[^\n]", tmp1, tmp2);
   rtn.table = (char *)strdup(tmp1);
   rtn.query = parse_string(tmp2, tmp);
 
   if((int)strlen(tmp) > 0) /* we have a query with an "and" clause */
   {
      /* AND field2='querystring2' */
 
      strcpy(tmp1, "");
      strcpy(tmp2, "");
      sscanf(tmp, "%[^=]='%[^\n]'", tmp1, tmp2);
      if(strlen(tmp1) > 0)
        rtn.where2 = (char *)strdup(tmp1);
      if(strlen(tmp2) > 0)
        rtn.query2 = parse_string(tmp2, tmp);
   } /* fi */
 
   free(tmp); free(tmpf); free(tmp1); free(tmp2); free(modline); free(field);
   return(rtn);
} /* parse_query */
 
/************************************************************************/
 
int get_config(char *tablename)
{
   /* Given a tablename - Open the config DB file and search for the requested
      table.  If we find the tablename, return the position in the table, 
      otherwise, return -1.
   */
 
   int i, done, rtn = -1;
 
   done = FALSE;
   for(i = 0; !done && (i < NUM_TABLES); i++)
   {
      if(strcmp(tablename, config_info[i]->table) == 0)
      {
         done = TRUE;
         rtn = i;
      } /* fi */
   } /* for */
   return(rtn);
} /* get_config */
 
/************************************************************************/
 
struct res_rows_struct *parse_results(struct query_struct query,
           int config_ptr, char *result)
{
   /* Given a query structure with the SQL information, table configuration
      information, and the result line from the search -- see if we have
      any other checks (any and clauses) and then parse the results into
      the requested columns and print the final line out.
   */
 
   struct res_rows_struct *rtn;
   int i, pos, cont = TRUE;
   char *tmp;
 
   /* If we have an "and" clause to worry about, then check the proper */
   /* field based on the initial query's second where clause.  If we   */
   /* have a match, allow the program to finish parsing out the proper */
   /* fields that have been requested.  If we don't have a match, then */
   /* stop work on this line and move on.                              */
 
   rtn = (struct res_rows_struct *)malloc(sizeof(struct res_rows_struct));
   rtn->num_cols = -1;
   tmp = (char *)malloc((size_t)(MAXLINE + 1));
 
   if(query.query2 != NULL) /* AND clause to check */
   {
      cont = FALSE;
      pos = find_fieldpos(query.where2, config_ptr);
      if(pos > 0)
      {
         if(pos >= config_info[config_ptr]->num_fields)
           get_value_from_field_jgm(pos, tmp, result, TRUE);
         else
           get_value_from_field_jgm(pos, tmp, result, FALSE);

         if(strcmp(tmp, query.query2) == 0)
           cont = TRUE;
      } /* fi */
   } /* if */
 
   if(cont)
   {
     rtn->num_cols = query.num_fields;
     for(i = 0; i < query.num_fields; i++)
     {
         pos = find_fieldpos(query.fields[i], config_ptr);
         rtn->col_type[i] = config_info[config_ptr]->field_types[pos - 1];
 
         if(pos > 0)
         {
            strcpy(tmp, "");
            if(pos >= config_info[config_ptr]->num_fields)
               get_value_from_field_jgm(pos, tmp, result, TRUE);
            else
               get_value_from_field_jgm(pos, tmp, result, FALSE);

            if(rtn->col_type[i] == INT_TYPE)
            {
               sscanf(tmp, "%d", &rtn->int_result[i]);
               rtn->str_result[i] = NULL;
            } /* fi */
            else
            {
               rtn->int_result[i] = -1;
               rtn->str_result[i] = (char *)strdup(tmp);
            } /* else */
         } /* if */
         else
           rtn->str_result[i] = NULL;
     } /* for */
   } /* if */
 
   free(tmp);
   return(rtn);
} /* parse_results */
 
/************************************************************************/
 
void get_value_from_field_jgm(int pos, char *to, char *from, int flag)
{
    /* Given a position, result variable, and from string - Assume that
       the bar "|" is used to separate the fields.  Cycle through the from
       string until the requested position is found.  Then copy the findings
       into the result variable (to).

       to is assumed to have already been malloc/assigned memory.
    */
 
    int i, j;
    int current_field  = 1;
    int char_len = 0;
    char tmpvalue[MAXLINE];

    char_len = (int)strlen(from);

    i = 0;
    while((current_field < pos) && (i < char_len))
    {
       if(from[i] == '|')
         current_field++;
       i++;
    } /* while */

    j = 0;
    strcpy(tmpvalue, "");

    if(flag) /* Pull to end of line */
    {
        while((i + 1) < char_len)
          tmpvalue[j++] = from[i++];
    } /* fi */

    else /* Just pull to next field */
    {
        while((from[i] != '|') && ((i + 1) < char_len))
          tmpvalue[j++] = from[i++];
    } /* else */

    tmpvalue[j] = '\0';

    strcpy(to, tmpvalue);
} /* get_value_from_field_jgm */
 
 
/************************************************************************/
 
int find_fieldpos(char *find, int config_ptr)
{
    /* Given a field name and a list of fields in position order - return
       the position of the field matching the passed in field name.  If
       field name is not found, return 0.
    */
 
    int rtn = 0, j, done;
 
    /* Find matching table column to requested field name */
 
    done = FALSE;
    j = 0;
    while(!done && (j < config_info[config_ptr]->num_fields))
    {
       if(strcmp(find, config_info[config_ptr]->fields[j]) == 0)
       {
          done = TRUE;
          rtn = j + 1;
       } /* fi */
       else
          j++;
    } /* while */
    return(rtn);
} /* find_fieldpos */
 
/************************************************************************/

void free_query(struct query_struct query)
{
   int i;
 
   for(i = 0; i < query.num_fields; i++)
     if(query.fields[i] != NULL)
       free(query.fields[i]);
 
   free(query.table);
   free(query.query);
   if(query.where2 != NULL)
     free(query.where2);
   if(query.query2 != NULL)
     free(query.query2);
} /* free_query */
 
/************************************************************************/
 
 
char *parse_string(char *str, char *leftover)
{
   /* Given a query string, parse the string to ensure we grab all of
      the embedded quotes.  We also want to return the remainder of the
      string just in case there is an "and" clause.
   */
 
   char *rtn;
   int i, j, pos, lpos, len, done, epos;
 
   len = (int)strlen(str);
   rtn = (char *)malloc((size_t)(len + 1));
   lpos = 0;
   done = FALSE;
 
   /* Parse through the string removing any secondary queries and returning
      them to the caller.
      We also want to strip out any dual embedded single quotes so that the
      end string is readable for DB.
   */
 
   if((strstr(str, "' and ") != NULL) && (strstr(str, "=") != NULL))
   {
      /* Find the equal sign */
 
      for(epos = 0; (epos < len) && (str[epos] != '='); epos++);
 
      /* Now back-track until we get the full field name */
 
      while((epos > 0) && (str[--epos]!= ' '));
 
      /* Now copy into leftover field='query' so we can send back */
 
      j = 0;
      for(i = epos + 1; i < len; i++)
        leftover[j++] = str[i];
      leftover[j] = '\0';
 
      /* Now back-track until we get to the last single quote mark */
 
      while((epos > 0) && (str[--epos]!= '\''));
 
      /* Reset str to point at the beginning to this point */
 
      str[epos + 1] = '\0';
      len = strlen(str);
   } /* fi */
 
   /* Now clean out any occurrences of double single-quotes */
 
   pos = 0;
   while(!done && (pos < len))
   {
      if(str[pos] != '\'')
        rtn[lpos++] = str[pos++];
      else if((str[pos] == '\'') && (str[pos + 1] == '\''))
      {  /* Skip over this single quote and include the next one */
         pos++;
         rtn[lpos++] = str[pos++];
      } /* else */
      else if((str[pos + 1] == '\n') || (str[pos + 1] == (int)NULL))
        done = TRUE;
      else
        rtn[lpos++] = str[pos++];
   } /* while */
   rtn[lpos] = '\0';
 
   return(rtn);
} /* parse_string */
