#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/types.h>
#include <db.h>

#define DEBUG 0
#define MAX_DB 10

long c_init_dbs_genspec(char *data_year);
void c_destroy_dbs_genspec();
void c_genspec(char *cui1, char *cui2, char **genspec);
void create_key_value(char *cui1, char *cui2, char **key);
void int2bin(unsigned num, char **bin);

/* database handle */
DB *dbps[MAX_DB];
int db_count;

/* create a database connection and initialize databases*/
long c_init_dbs_genspec(char *data_year)
{
   int open_status;
   int i;
   char genspecdb_file_prefix[200];
   char genspecdb_file[200];
   DB *dbp;

   /* DB directory and prefix */
   if ((char *)getenv("UMLS_HIERARCHY_DB_PREFIX") != NULL) {
	strcpy(genspecdb_file_prefix, (char *)getenv("UMLS_HIERARCHY_DB_PREFIX"));
   }
   else {
	fprintf(stderr, "The $UMLS_HIERARCHY_DB_PREFIX environment variable not found.\n");
	exit(1);
    }

    if (data_year != NULL) {
      strcat(genspecdb_file_prefix,data_year);
    }
    /* number of databases in DB directory, should not be more than MAX_DB */
    /*
    if ((char *)getenv("GENSPECDB_COUNT") != NULL) {
        db_count = atoi((const char *)getenv("GENSPECDB_COUNT"));
    }
    else {
        fprintf(stderr, "The $GENSPECDB_COUNT environment variable not found.\n");
        exit(1);
    }
    */ 
    
    if (DEBUG) fprintf(stderr,"UMLS_HIERARCHY_DB_PREFIX: %s\n",genspecdb_file_prefix);
    /* initialize databases and add them to the array */
    
    i =1;
    while (1) {
      /*    for (i=1;i<=db_count;i++) { */
   	if ((open_status = db_create(&dbp, NULL, 0)) != 0)
   	{
   		fprintf(stderr, "db_create: %s\n", db_strerror(open_status));
   	}
        sprintf(genspecdb_file,"%s_%d",genspecdb_file_prefix, i);
        if (DEBUG) fprintf(stderr,"UMLS_HIERARCHY_FILE: %s\n", genspecdb_file);
	if( access( genspecdb_file, F_OK ) == -1 ) {
	  db_count = i -1;
	  break;
	} 
#ifdef BDB_3_0_55
	open_status = dbp->open(dbp,genspecdb_file,NULL,DB_BTREE,DB_RDONLY,0444);
#else
	open_status = dbp->open(dbp,NULL,genspecdb_file,NULL,DB_BTREE,(u_int32_t)DB_RDONLY,0444);
#endif   
	if (open_status !=0)
	{
	  fprintf(stderr, "Not able to open the database: dbp->open: %s: %s\n", strerror(open_status),genspecdb_file);
	  c_destroy_dbs_genspec(dbp);
	} 	
	else {
	  if (DEBUG) fprintf(stderr,"Opened database file %s.\n", genspecdb_file);
	  dbps[i] = dbp;
	} 
	i++;
    }
    if (DEBUG) fprintf(stderr,"Opened %d files.\n", db_count);
    return db_count;
}

/* close database connection */
void c_destroy_dbs_genspec()
{
    int close_status;
    int i;
    DB *dbp;
    if (db_count == 0) return;

    for (i=1;i<=db_count;i++) {
	dbp = dbps[i];
    	if ((close_status = dbp->close(dbp,0)) != 0)
    	{	
		fprintf(stderr,"Not able to close the database: dbp->close: %s\n", strerror(close_status));
    	}
    }
}


/* search for the cui1-cui2 pair in the database. */
void c_genspec(char *cui1, char *cui2, char **ret_str)
{
    int get_status;
    int i;
    DBT key,data;
    char vkey[6];
    /*    char output_str[18]; */
    char *output_str = malloc(20*sizeof(char));
    DB *dbp;
    if (db_count == 0) {
        strcpy(output_str,"None.");
    } 
    for (i=1;i<=db_count;i++) {
    	dbp = dbps[i];
    	create_key_value(cui1, cui2, &vkey);
    	memset(&key,0,sizeof(DBT));
    	memset(&data,0,sizeof(DBT));
    	key.data = vkey;
    	key.size = 6;
    	get_status= dbp->get(dbp,NULL,&key,&data,0);
    	/* Key found, no need to try the reverse order */
    	if (get_status == 0)
    	{
	  	sprintf(output_str,"%s %s", cui1, cui2);
		if (DEBUG) fprintf(stderr, "Found %s %s in DB num %d.\n", cui1, cui2, i);
       		break;
    	}	
    	/* Key not found, try the reverse order */
    	else if (get_status == DB_NOTFOUND)
    	{
	  if (DEBUG) fprintf(stderr,"Did not find %s %s in DB num: %d.\n",cui1,cui2,i);
		create_key_value(cui2, cui1, &vkey);
		memset(&key,0,sizeof(DBT));
		memset(&data,0,sizeof(DBT));
		key.data = vkey;
		key.size = 6;
		get_status = dbp->get(dbp,NULL,&key,&data,0);

		if (get_status == 0)
     		{
		  sprintf(output_str,"%s %s", cui2, cui1);		  
			if (DEBUG) fprintf(stderr, "Found %s %s in DB num %d.\n", cui2, cui1, i);
	    		break;
		}	
		/* Reverse order failed too, no relation in this database*/
		else if (get_status == DB_NOTFOUND)
        	{
		  if (DEBUG) fprintf(stderr,"Did not find %s %s in DB num: %d.\n",cui2,cui1,i);
	    		if (i == db_count) {
			  strcpy(output_str,"None.");
	    		}
		}
		else
        	{                                           
	    		fprintf (stderr, "ERROR: Can not get data - %s\n", db_strerror(get_status));
		}
      	}
      	else
      	{
	  	fprintf (stderr, "ERROR: Can not get data - %s\n", db_strerror(get_status));
      	}
    }
    if (DEBUG) fprintf(stdout,"GENSPEC returning %s\n.",output_str);
    *ret_str = output_str; 
}


/* Create 6-byte key value from two concept UIs */
void create_key_value(char *cui1, char *cui2, char **ret_key) 
{
      size_t i;
      char cui1_7[8];
      char cui2_7[8];
      unsigned cui1_int, cui2_int;
      char cui1_bin[3];
      char cui2_bin[3];
      char key[6];

      sscanf(cui1,"C%s", cui1_7);
      sscanf(cui2,"C%s", cui2_7);
      cui1_int = atoi(cui1_7);
      cui2_int = atoi(cui2_7);

      int2bin(cui1_int, &cui1_bin);
      memcpy(key,cui1_bin,3);

      int2bin(cui2_int, &cui2_bin);
      memcpy(key+3,cui2_bin,3);

      memcpy(ret_key,key,6);
      if (DEBUG) {
	fprintf(stderr,"KEY TO SEARCH:");
	for (i=0;i<6;i++) {
		fprintf(stderr,"%02X", ((unsigned char*)ret_key)[i] );
	}	
	fprintf(stderr,"\n"); 
      }
}


/* Gets the binary representation of a CUI number */ 
void int2bin(unsigned num, char **bin)
{  
    int c = 24;
    int c1 = 8;
    int i = 0;
    unsigned temp_num = num;
    char onebyte[9];
    unsigned long one_char[3];
    char buf[3];

    strcpy(onebyte,"");

    temp_num = temp_num << 8;    
    while (c) 
    {
	c1=8;
	strcpy(onebyte,"");
	while(c1)
	{
	    if (temp_num & 0x80000000)
	    {
		strcat(onebyte,"1");
	    }
	    else
	    {
		strcat(onebyte,"0");
	    }
	    temp_num = temp_num << 1;
	    c1--;
	    c--;
	}
 	one_char[i++] = strtoul(onebyte,NULL,2);	
    }
    buf[0] = one_char[0];
    buf[1] = one_char[1];
    buf[2] = one_char[2];

    if (DEBUG) fprintf(stderr,"BIN VALUE: %d %d %d\n", buf[0], buf[1], buf[2]);
    memcpy(bin,buf,3);
}

