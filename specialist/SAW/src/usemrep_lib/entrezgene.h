#define MAXLINE 4096
#define MAXLINE_LONG 40096
#define TRUE 1
#define FALSE 0
#define DEBUG 0

static char entrez_sccs_id[] = "@(#)entrezgene.h	1.4 10/31/05";

struct alias_struct {
    char alias[MAXLINE+1];
    char id[50];
};

struct symbol_struct {
    char symbol[MAXLINE+1];
    char id[50];
    char type[50];
    char go_functions[MAXLINE_LONG+1];
};
