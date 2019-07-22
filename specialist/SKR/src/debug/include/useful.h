/*==========================================================

%SOURCE FILE
	useful.h

%DESCRIPTION OF FILE
	C include file.

%REVISED
	21Nov94 divita -- Initial Version
	13Jan95 divita -- Made some inhansements taken from lvg
%%
==========================================================*/


/* 
   prevent sccs_id_useful_h from being re-defined in case
   useful.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_useful_h
static char sccs_id_useful_h[] = "@(#)useful.h	1.2 09/27/06";
#define sccs_id_useful_h 1
#endif

#ifndef _USEFULDEFS_
#define _USEFULDEFS_
/*----------------------------
%INCLUDES
----------------------------*/
#include <stdio.h>
/*end of includes ----------*/


#ifdef NT
#define strcasecmp(x,y) stricmp(x,y)
#endif

/*----------------------------
%CONSTANTS
----------------------------*/
#define EOS             '\0'
#define TAB             '\t'
#define NEW_LINE        '\n'
#define SPACE           ' '
#define BANG            '!'
#define HYPHON          '-'
#define EQUALS          '='
#define AND_SIGN        '&'
#define BAR             '|'
#define CARRET          '^'
#define STAR_SIGN       '*'
#define PLUS            '+'
#define LESS_THAN       '<'
#define GREATER_THAN    '>'
#define LEFT_PAREN      '('
#define LEFT_BRACES     '{'
#define LEFT_BRACKET    '['
#define RIGHT_PAREN     ')'
#define RIGHT_BRACES    '}'
#define RIGHT_BRACKET   ']'
#define DOUBLE_QUOTE    '\"'
#define QUOTE           '\''
#define UNQUOTE         '`'
#define AT_SIGN         '@'
#define POUND           '#'
#define DOLLAR_SIGN     '$'
#define PERCENT         '%'
#define UNDERBAR        '_'
#define TILDA           '~'
#define BACKSLASH       '\\'
#define COLON           ':'
#define SEMI_COLON      ';'
#define QUESTION_MARK   '?'
#define COMMA           ','
#define SLASH           '/'
#define PIPE            '|'
#define PERIOD          '.'
#define ZERO            '0'
#define ONE             '1'
#define TWO             '2'
#define THREE           '3'
#define FOUR            '4'
#define FIVE            '5'
#define SIX             '6'
#define SEVEN           '7'
#define EIGHT           '8'
#define NINE            '9'


#ifdef NT
#define PATH_DELIMITER  '\\'       /* For NT  */
#else
#define PATH_DELIMITER  '/'       /* For unix */
#endif

#define MAX_LIST_SIZE   100
#define BIG_LINE_SIZE  9000
#define MAXLINE         BIG_LINE_SIZE
#define INULL        999999

/*end of constants ---------*/

/*----------------------------
%MACROS
----------------------------*/
#define CHAR_FREE(x)  if (x != NULL ) free (x); x = (char *)NULL;  
#define NOT_NULL(x) ( x[0] != '\0' )
#define not_smart_copy(x,y) \
if (x != NULL ) \
  free (x); \
if (y != NULL ) \
  x = (char *) strdup( y );\
if ( (char * ) x == NULL ) \
   DPM(D_F_MALLOC);
#define SWITCH(x) strcpy(stuff,x);
#define CASE(x) if (strcmp (stuff,x) == 0 ) {
#define NCASE(x) if (strcasecmp (stuff,x) == 0 ) {
#define INCASE(x) if (strstr (stuff,x) != NULL ) {
#define BEGINS_WITH(x) if ( strncmp(stuff,x,strlen(x)) == 0 ) {
#define BREAK    }else
#define DEFAULT {
#define END     }

/*end of macros ------------*/


/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/
/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
static char stuff[MAXLINE] ;
/*end of global variables --*/

/*----------------------------
%PRIVATE STRUCTURES
----------------------------*/
/*end of private structures */

/*----------------------------
%TYPEDEFS
----------------------------*/
/*end of typedefs ----------*/
   

/*==========================================================
%CSC NAME
	?
%INFORMATION FOR CSC
	?
%LIBRARIES
	?
%%
==========================================================*/
#endif
