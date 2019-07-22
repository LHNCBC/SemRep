#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <db.h>
#include "entrezgene.h"

const char *progname = "retrieve_entrezgene";
void c_init_env();
void c_close_env();
void c_retrieve_gene(char *term, char **ret);
void normalize_gene(char *term, char **normalized);


DB_ENV *dbenv;
DB *dbp_symbol;
DB *dbp_alias;

void c_init_env() {
    char *config[2], *home;

    int ret;
    home = DB_HOME;
    config[0] = CONFIG_DATA_DIR;
    config[1] = NULL;

    /*
     * Create an environment object and initialize it for error
     * reporting.
     */
    if ((ret = db_env_create(&dbenv, 0)) != 0) {
	fprintf(stderr, "%s: %s\n", progname, db_strerror(ret));
	exit (1);
    } 

    dbenv->set_errfile(dbenv, stderr);
    dbenv->set_errpfx(dbenv, progname);

    /*
     * We want to specify the shared memory buffer pool cachesize,
     * but everything else is the default.
     */
    if ((ret = dbenv->set_cachesize(dbenv, 0, 64 * 1024, 0)) != 0) {
	dbenv->err(dbenv, ret, "set_cachesize");
	dbenv->close(dbenv, 0);
	exit (1);
    }

    /*
     * We have multiple processes reading/writing these files, so
     * we need concurrency control and a shared buffer pool, but
     * not logging or transactions.
     */
    if ((ret = dbenv->open(dbenv, home, config,
			   DB_INIT_LOCK | DB_INIT_MPOOL, 0)) != 0) {
	dbenv->err(dbenv, ret, "environment open: %s", home);
	dbenv->close(dbenv, 0);
	exit (1);
    }

   if ((ret = db_create(&dbp_symbol, dbenv, 0)) != 0)
   {
   	fprintf(stderr, "db_create: %s\n", db_strerror(ret));
   }
   
   if ((ret = dbp_symbol->open(dbp_symbol,SYMDB, NULL, DB_BTREE, DB_RDONLY, 0444)) !=0)
   {
      fprintf(stderr, "Not able to open the database: dbp_symbol->open: %s\n", strerror(ret));
      exit (1);
   }

   if ((ret = db_create(&dbp_alias, dbenv, 0)) != 0)
   {
   	fprintf(stderr, "db_create: %s\n", db_strerror(ret));
   }
   
   if ((ret = dbp_alias->open(dbp_alias,ALIASDB, NULL, DB_BTREE, DB_RDONLY, 0444)) !=0)
   {
      fprintf(stderr, "Not able to open the database: dbp_alias->open: %s\n", strerror(ret));
      exit (1);
   } 

}

void c_close_env() {
    int ret;

    /* Close the handle. */
    if ((ret = dbenv->close(dbenv, 0)) != 0) {
	fprintf(stderr, "DBENV->close: %s\n", db_strerror(ret));
	exit (1);
    }    
}

void c_retrieve_gene(char *term, char **ret_str) {
    DBT key_alias, data_alias;
    DBT key_symbol, data_symbol;
    char output_str[MAXLINE+1];
    char normalized[MAXLINE+1];
    int get_status_alias, get_status_symbol;
    struct alias_struct aliases[100];
    struct symbol_struct potential_symbols[100];
    char best_symbol[MAXLINE+1];
    char *alias_str;
    char rest_of_alias[MAXLINE+1];
    char alias[MAXLINE+1];
    char alias_id[50];
    int i=0;
    int j=0;
    int k=0;
    
    strcpy(ret_str,"");

    normalize_gene(term, &normalized); 
    memset(&key_alias,0,sizeof(DBT));
    memset(&data_alias,0,sizeof(DBT));
    data_alias.flags = DB_DBT_MALLOC;

    key_alias.data = normalized;
    key_alias.size = strlen(key_alias.data);
    get_status_alias = dbp_alias->get(dbp_alias,NULL,&key_alias,&data_alias,0);

    if (get_status_alias == 0) {
        alias_str= (char *)malloc(data_alias.size+1);
        strcpy(alias_str,"");
	strncpy(alias_str, data_alias.data, data_alias.size);
	if (DEBUG) printf("Alias: %s|%d\n", alias_str, data_alias.size);
	while (strcmp(alias_str,"")) {
	    strcpy(rest_of_alias,"");
	    strcpy(alias,"");
	    strcpy(alias_id,"");
	    if (alias_str[0] == '|')
		sscanf(alias_str,"|%[^|]|%[0-9]%s", alias,alias_id,rest_of_alias);
	    else
		sscanf(alias_str,"%[^|]|%[0-9]%s", alias,alias_id,rest_of_alias);
	    strcpy(aliases[i].alias,alias);
	    strcpy(aliases[i].id,alias_id);
	    i++;
	    strcpy(alias_str,rest_of_alias);
	}
	
	for (j=0; j<i; j++) {
	    memset(&key_symbol,0,sizeof(DBT));
	    memset(&data_symbol,0,sizeof(DBT));
	    data_symbol.flags = DB_DBT_MALLOC;
	    key_symbol.data = aliases[j].id;
	    key_symbol.size = strlen(key_symbol.data);

	    get_status_symbol = dbp_symbol->get(dbp_symbol,NULL,&key_symbol,&data_symbol,0);
	    if (DEBUG) printf("Symbol data: %s|%d\n",data_symbol.data, data_symbol.size);
	    if (get_status_symbol == 0) {
		strcpy(output_str,"");
		strncpy(output_str,data_symbol.data,data_symbol.size);
		output_str[data_symbol.size] = '\0';
		if (DEBUG) printf("Output: %s\n", output_str);
                sscanf(output_str,"%*[^|]|%[^|]|%[^|]", potential_symbols[k].symbol,potential_symbols[k].type); 
		strcpy(potential_symbols[k].id, key_symbol.data);
		k++;
		
	    }
	    else if (get_status_symbol == DB_NOTFOUND) {
		strcat(ret_str, "None");
	    }
	    /* Added to fix memory leak, but somehow causes core dump when used without dynleak. */
/* 	    free(data_symbol.data); */
	}
	free(alias_str);
	find_best_symbol(alias,potential_symbols,k,&best_symbol);
	if (DEBUG) printf("Best symbol: %s\n", best_symbol);
	strcat(ret_str,best_symbol);
    }
    else if (get_status_alias == DB_NOTFOUND) {
	strcat(ret_str, "None");
    }
/*     free(data_alias.data); */
}

void find_best_symbol(char *alias, struct symbol_struct in_symbols[], int cnt, char **best_symbol) {
    int i;
    int j=0;
    int len;
    char *ids;
    char *symbols;

    strcpy(best_symbol,"");
    if (cnt == 1) {

	strcpy(best_symbol,in_symbols[0].id);
	strcat(best_symbol,"|");
	strcat(best_symbol,in_symbols[0].symbol); 	
    } else if (cnt > 1) {	
	ids = (char *)malloc(1);
	symbols = (char *)malloc(1);
	strcpy(ids,"");
	strcpy(symbols,"");
	for (i=0; i< cnt; i++) {
	    if (DEBUG) printf("%s %s %s %s\n",alias,in_symbols[i].type,in_symbols[i].id,in_symbols[i].symbol);
	    if (!strcmp(alias,in_symbols[i].symbol)) {
		ids = (char *)realloc(ids, strlen(ids) + strlen(in_symbols[i].id)+1);
		strcpy(ids,"");
		strcpy(ids,in_symbols[i].id);
		symbols = (char *)realloc(symbols, strlen(symbols) + strlen(in_symbols[i].symbol)+1);		
		strcpy(symbols,"");
		strcpy(symbols,in_symbols[i].symbol);
/* 		strcpy(best_symbol,""); */
/* 		strcpy(best_symbol,in_symbols[i].symbol); */
		break;
	    }
	    else {
		if (!strcmp(in_symbols[i].type,"OFFICIAL_SYMBOL")) {
/* 		    if (strcmp(best_symbol,"")) strcat(best_symbol,",");		       			 */
/* 		    strcat(best_symbol,in_symbols[i].symbol); */
		    ids = (char *)realloc(ids, strlen(ids) + strlen(in_symbols[i].id)+2);
		    if (strcmp(ids,"")) strcat(ids,",");		       			
		    strcat(ids,in_symbols[i].id);
		    symbols = (char *)realloc(symbols, strlen(symbols) + strlen(in_symbols[i].symbol)+2);
		    if (strcmp(symbols,"")) strcat(symbols,",");		       			
		    strcat(symbols,in_symbols[i].symbol);
		}
	    }
	}
	strcpy(best_symbol,ids);
	strcat(best_symbol,"|");
	strcat(best_symbol,symbols);
	free(ids);
	free(symbols);
    }
}

void normalize_gene(char *term, char **rtn) {
   long len = (long)strlen(term);
   char *normalized = (char *)malloc(len + 1);
   long i, pos;

   strcpy(normalized,"");
   pos = 0;
   for(i = 0; i < len; i++)
   {
      if(isalnum(term[i]))
        if(islower(term[i]))
          normalized[pos++] = toupper(term[i]);
        else
          normalized[pos++] = term[i];
   } /* for */
   normalized[pos] = '\0';
   strcpy(rtn, normalized);
   free(normalized);
}
