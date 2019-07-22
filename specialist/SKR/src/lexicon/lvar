/* lvar.c - generates inflectional variants for indexing
*/

#include <stdio.h>
#include <debug.h>
#include "lexicon.h"

static char sccs_id[] = "@(#)lvar.c	1.1	4/20/94";

/* function prototypes */
extern LexIndexKey *lex_index_keys();

main()
{
    int n;
    int i;
    LexIndexKey *keys;
    char line[MAXLINE];
    char record[MAXLINE];

    strcpy(record,"");
    fgets(line,MAXLINE,stdin);
    while ( !feof( stdin) )
      {
	strcat(record,line);
	
	if ( strstr(line,"}" ) != NULL )
	  {
	    if ((keys = lex_index_keys(record, &n)) == (LexIndexKey *)NULL)
	      return(0);
	    
	    for (i=0; i<n; i++)
	      {
		printf("%d|%s\n", (keys+i)->likFlags, (keys+i)->likKey);
	      }
	    strcpy(record,"");
	  }
	
	fgets(line,MAXLINE,stdin);
      }
}
