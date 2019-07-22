#ifndef sccs_id_wsd_client_h
static char sccs_id_wsd_client_h[] = "@(#)wsd_client.h	1.1 09/28/06";
#define sccs_id_wsd_client_h 1
#endif

#include <time.h>
#include <stdio.h>

#define TRUE 1
#define FALSE 0
#define SUCCESS 1
#define FAILURE 0
#define MAXLINE 500000
#define MAX_UMLS_STRING 4096


typedef struct candidate_struct
{
    char score[10];
    char cui[10];
    char umls_concept[MAX_UMLS_STRING + 1];
    char preferred_name[MAX_UMLS_STRING + 1];
    char matched_words[MAX_UMLS_STRING + 1];
    char semtypes[1024];
    char matchmap[1024];
    char head_flag[4];
    char overmatch_flag[4];
    int checked;
} candidate_struct;

typedef struct phrase_element_struct
{
    char *type;
    char *name[10];
    char *value[10];
    int field_cnt;
} phrase_element_struct;

char debug_file[1024];
FILE *fdebug;    
int DEBUG;
int INITIALIZED;

time_t currtime;
