#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <db.h>
#include "entrezgene.h"

const char *progname = "retrieve_entrezgene";
void c_init_dbs();
void c_close_dbs();
void c_retrieve_gene(char *term, char **ret);
void normalize_gene(char *term, char **normalized);

static char sccs_id[] = "%W% %G%";

/* DB_ENV *dbenv; */
DB *dbp_symbol;
DB *dbp_alias;

/* Initialize the Berkeley DB environment and databases. */
void c_init_dbs() {
/*     char *config[2], *home; */

    int ret;
/*     home = DB_HOME; */
/*     config[0] = CONFIG_DATA_DIR; */
/*     config[1] = NULL; */

    /*
     * Create an environment object and initialize it for error
     * reporting.
     */
/*     if ((ret = db_env_create(&dbenv, 0)) != 0) { */
/* 	fprintf(stderr, "%s: %s\n", progname, db_strerror(ret)); */
/* 	exit (1); */
/*     }  */

/*     dbenv->set_errfile(dbenv, stderr); */
/*     dbenv->set_errpfx(dbenv, progname); */

    /*
     * We want to specify the shared memory buffer pool cachesize,
     * but everything else is the default.
     */
/*     if ((ret = dbenv->set_cachesize(dbenv, 0, 64 * 1024, 0)) != 0) { */
/* 	dbenv->err(dbenv, ret, "set_cachesize"); */
/* 	dbenv->close(dbenv, 0); */
/* 	exit (1); */
/*     } */

    /*
     * We have multiple processes reading/writing these files, so
     * we need concurrency control and a shared buffer pool, but
     * not logging or transactions.
     */
/*     if ((ret = dbenv->open(dbenv, home, config, */
/* 			   DB_INIT_LOCK | DB_INIT_MPOOL, 0)) != 0) { */
/* 	dbenv->err(dbenv, ret, "environment open: %s", home); */
/* 	dbenv->close(dbenv, 0); */
/* 	exit (1); */
/*     } */

/*    if ((ret = db_create(&dbp_symbol, dbenv, 0)) != 0) */
   if ((ret = db_create(&dbp_symbol, NULL, 0)) != 0)
   {
   	fprintf(stderr, "db_create: %s\n", db_strerror(ret));
   }
   
   if ((ret = dbp_symbol->open(dbp_symbol,SYMDB, NULL, DB_BTREE, DB_RDONLY, 0444)) !=0)
   {
      fprintf(stderr, "Not able to open the database: dbp_symbol->open: %s\n", strerror(ret));
      exit (1);
   }

/*    if ((ret = db_create(&dbp_alias, dbenv, 0)) != 0) */
   if ((ret = db_create(&dbp_alias, NULL, 0)) != 0)
   {
   	fprintf(stderr, "db_create: %s\n", db_strerror(ret));
   }
   
   if ((ret = dbp_alias->open(dbp_alias,ALIASDB, NULL, DB_BTREE, DB_RDONLY, 0444)) !=0)
   {
      fprintf(stderr, "Not able to open the database: dbp_alias->open: %s\n", strerror(ret));
      exit (1);
   } 

}

/* Close the Berkeley DB environment. */
void c_close_dbs() {
    int ret;
    int close_status;

    if ((close_status = dbp_alias->close(dbp_alias,0)) != 0)
    {
	fprintf(stderr,"Not able to close the database: dbp_alias->close: %s\n", 
		strerror(close_status));
	exit(1);
    }
    if ((close_status = dbp_symbol->close(dbp_symbol,0)) != 0)
    {
	fprintf(stderr,"Not able to close the database: dbp_symbol->close: %s\n", 
		strerror(close_status));
	exit(1);
    }
    
    /* Close the handle. */
/*     if ((ret = dbenv->close(dbenv, 0)) != 0) { */
/* 	fprintf(stderr, "DBENV->close: %s\n", db_strerror(ret)); */
/* 	exit (1); */
/*     }   */  
}

/* Retrieve the id, official symbol and gene ontology functions for a given gene string. */
void c_retrieve_gene(char *term, char **ret) {
    DBT key_alias, data_alias;
    DBT key_symbol, data_symbol;
    char output_str[MAXLINE_LONG+1];
    char ret_str[MAXLINE_LONG+1];
    char normalized[MAXLINE+1];
    int get_status_alias, get_status_symbol;
    struct alias_struct aliases[100];
    struct symbol_struct potential_symbols[100];
    char best_symbol[MAXLINE_LONG+1];
    char *alias_str;
    char rest_of_alias[MAXLINE+1];
    char alias[MAXLINE+1];
    char alias_id[50];
    int duplicate = FALSE;
    int i=0;
    int j=0;
    int k=0;
    int l=0;
    int m=0;

    if (DEBUG) printf("Term: %s\n",term);
    strcpy(ret_str,"");
    normalize_gene(term, &normalized); 
    memset(&key_alias,0,sizeof(DBT));
    memset(&data_alias,0,sizeof(DBT));
    data_alias.flags = DB_DBT_MALLOC;
    if (DEBUG) printf("Normalized: %s\n", normalized);
    key_alias.data = normalized;
    key_alias.size = strlen(key_alias.data);
    if (DEBUG) printf("Alias_key|size: %s|%d\n", key_alias.data, key_alias.size);
    get_status_alias = dbp_alias->get(dbp_alias,NULL,&key_alias,&data_alias,0);
    if (get_status_alias == 0) {
	if (DEBUG) printf("Alias data: %s\n",data_alias.data);
        alias_str= (char *)malloc(data_alias.size+1);
        strcpy(alias_str,"");
	strncpy(alias_str, data_alias.data, data_alias.size);
	alias_str[data_alias.size] = '\0';
	if (DEBUG) printf("Alias: %s|%d\n", alias_str, data_alias.size);
	while (strcmp(alias_str,"")) {
	    strcpy(rest_of_alias,"");
	    strcpy(alias,"");
	    strcpy(alias_id,"");
	    if (alias_str[0] == '|')
		sscanf(alias_str,"|%[^|]|%[0-9]%s", alias,alias_id,rest_of_alias);
	    else
		sscanf(alias_str,"%[^|]|%[0-9]%s", alias,alias_id,rest_of_alias);
	    
	    printf("alias_str %s %s %s\n", alias_str, alias, alias_id);

	    /* check if alias already exists */
	    m = i;
	    duplicate = FALSE;
	    for (l=0; l<m; l++) {
		printf("%d %d %d\n",l,m,i);
		if (!strcmp(aliases[l].id,alias_id)) 
		    duplicate = TRUE;
		break;
	    }
	    if (!duplicate) {
		strcpy(aliases[i].alias,alias);
		strcpy(aliases[i].id,alias_id);
		i++;		    
	    }

	    strcpy(alias_str,rest_of_alias);
	}

	for (j=0; j<i; j++) {
	    memset(&key_symbol,0,sizeof(DBT));
	    memset(&data_symbol,0,sizeof(DBT));
	    data_symbol.flags = DB_DBT_MALLOC;
	    key_symbol.data = aliases[j].id;
	    key_symbol.size = strlen(key_symbol.data);
	    if (DEBUG) printf("Symbol key|size: %s|%d\n", key_symbol.data,key_symbol.size);
	    get_status_symbol = dbp_symbol->get(dbp_symbol,NULL,&key_symbol,&data_symbol,0);
	    if (get_status_symbol == 0) {
		if (DEBUG) printf("Symbol data: %s\n",data_symbol.data);
		strcpy(output_str,"");
		strncpy(output_str,data_symbol.data,data_symbol.size);
		output_str[data_symbol.size] = '\0';
		if (DEBUG) printf("Output: %s\n", output_str);
                sscanf(output_str,"%*[^|]|%[^|]|%[^|]|%[^\n]", potential_symbols[k].symbol,potential_symbols[k].type,potential_symbols[k].go_functions); 
		strcpy(potential_symbols[k].id, key_symbol.data);
		k++;
		
	    }
	    else if (get_status_symbol == DB_NOTFOUND) {
		strcat(ret_str, "|None||||");
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
	strcat(ret_str, "|None||||");
    }
    if (DEBUG) printf("Return str: %s\n", ret_str);
    *ret = ret_str;
/*     free(data_alias.data); */
}

/* From a list of potential gene symbols, select the best ones */
void find_best_symbol(char *alias, struct symbol_struct in_symbols[], int cnt, char **best_symbol) {
    int i,j;

    strcpy(best_symbol,"");
    if (cnt == 1) {
	sprintf(best_symbol,"%s|%s|%s", in_symbols[0].id,in_symbols[0].symbol,in_symbols[0].go_functions);
    } else if (cnt > 1) {	
	for (i=0; i< cnt; i++) {
	    if (DEBUG) printf("%s %s %s %s\n",alias,in_symbols[i].type,in_symbols[i].id,in_symbols[i].symbol);
	    if (!strcmp(alias,in_symbols[i].symbol)) {
		sprintf(best_symbol,"%s|%s|%s", in_symbols[i].id,in_symbols[i].symbol,in_symbols[i].go_functions);
		break;
	    }
	    else {
		if (!strcmp(in_symbols[i].type,"OFFICIAL_SYMBOL")) {
		    if (strcmp(best_symbol,""))
			sprintf(best_symbol,"%s\n%s|%s|%s",best_symbol,in_symbols[i].id,in_symbols[i].symbol,in_symbols[i].go_functions);
		    else 
			sprintf(best_symbol,"%s|%s|%s",in_symbols[i].id,in_symbols[i].symbol,in_symbols[i].go_functions);			
		}
	    }
	}
    }
}


/* Uppercase the given gene string. */
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
