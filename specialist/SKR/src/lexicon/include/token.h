
/* 
   prevent sccs_id_token_h from being re-defined in case
   token.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_token_h
static char sccs_id_token_h[] = "@(#)token.h	1.3 09/27/06";
#define sccs_id_token_h 1
#endif

/* define some portable types */
typedef unsigned long u32int;
typedef unsigned short int u16int;
typedef unsigned char u8int;
typedef int s32int;
typedef short s16int;
typedef char s8int;

/* define some character classes */
#define U 0			/* unknown */
#define W 1			/* word class */
#define S 2			/* space class */
#define P 3			/* punct class */
#define I 4			/* ignore class */
#define R 5			/* remove class */

#define DEFAULT_TOKEN_ALLOCATION 16

/* token type */
typedef struct _token {
    char *s;			/* where it starts */
    int len;			/* its length */
#define WT 1			/* word token */
#define ST 2			/* space token */
#define PT 3			/* punctuation token */
    u8int type;			/* one of the above */
} Token;

#define TOKP(t,n)   (*((t)+(n)))    /* token pointer */
