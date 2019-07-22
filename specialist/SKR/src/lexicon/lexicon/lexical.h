/*
   lex.h - header file for the lex library
*/


/* 
   prevent sccs_id_lexical_h from being re-defined in case
   lexical.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_lexical_h
static char sccs_id_lexical_h[] = "@(#)lexical.h	1.3 09/27/06";
#define sccs_id_lexical_h 1
#endif

#define MY_NIL (-1)
#define MY_OK 0
#define MATCH	    MY_NIL
#define NO_MATCH    MY_OK
#define EOS '\0'		    /* end-of-string */
#define PERIOD '.'
#define QUOTE '\''
#define COMMA ','
#define TAB '\t'
#define SPACE ' '
#define STR4096 4096
#define STR256	256
#define STR128	128
#define STR32	32
#define CODESIZE 3		    /* size of numeric code MH, DE etc */
#define NEWLINE '\n'
#define IGNORE_PUNCT	"-' ."	    /* punctuation to ignore in making comparisons */
