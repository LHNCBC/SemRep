/*==========================================================

%SOURCE FILE
	utils.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	17Aug94 divita -- Initial Version

%%
==========================================================*/

static char sccs_id_utils_c[] = "@(#)utils.c	1.2	09/27/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include <stdio.h>
#include "debug.h"
/*end of includes ----------*/

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
/*
int get_right_hand_side();
int get_stuff_before_semicolon();
int get_stuff_after_semicolon();
int get_parameter();
int strip();
int get_stuff_before_comma();
int get_filler_slot();
int get_left_hand_side();
int null_string_cmp();
int replace_string();
int base_path();
int not_smart_concat();
int strip_non_ascii_chars();
int nls_copy();
int nls_concat();
int nls_replace_string();
char *get_next_row();
int getNumberOfFields();
int substring();
*/
/*end_of_function_prototypes*/


/*----------------------------
%DEBUG FLAGS
----------------------------*/
#define DT132  132          /* DT for get_right_hand_side() */
#define DF133  133          /* DF for get_right_hand_side() */
#define DT248  248          /* DT for get_stuff_before_semicolon() */
#define DF249  249          /* DF for get_stuff_before_semicolon() */
#define DT250  250          /* DT for get_stuff_after_semicolon() */
#define DF251  251          /* DF for get_stuff_after_semicolon() */
#define DT254  254          /* DT for get_parameter() */
#define DF255  255          /* DF for get_parameter() */
#define DT392  392          /* DT for strip() */
#define DF393  393          /* DF for strip() */
#define DT404  404          /* DT for get_stuff_before_comma() */
#define DF405  405          /* DF for get_stuff_before_comma() */
#define DT406  406          /* DT for get_filler_slot() */
#define DF407  407          /* DF for get_filler_slot() */
#define DT852  852          /* DT for get_left_hand_side() */
#define DF853  853          /* DF for get_left_hand_side() */
#define DT586  586          /* DT for null_string_cmp() */
#define DF587  587          /* DF for null_string_cmp() */
#define DT1078  1078          /* DT for replace_string() */
#define DF1079  1079          /* DF for replace_string() */
#define DT362  362          /* DT for not_smart_ccat() */
#define DF363  363          /* DF for not_smart_ccat() */
#define DT460  460          /* DT for base_path() */
#define DF461  461          /* DF for base_path() */
#define DT484  484          /* DT for not_smart_concat() */
#define DF485  485          /* DF for not_smart_concat() */
#define DT490  490          /* DT for strip_non_ascii_chars() */
#define DF491  491          /* DF for strip_non_ascii_chars() */
#define DT514  514          /* DT for nls_copy() */
#define DF515  515          /* DF for nls_copy() */
#define DT516  516          /* DT for nls_concat() */
#define DF517  517          /* DF for nls_concat() */
#define DT520  520          /* DT for nls_replace_string() */
#define DF521  521          /* DF for nls_replace_string() */
#define DT724  724          /* DT for get_next_row() */
#define DF725  725          /* DF for get_next_row() */
#define DT2116  2116          /* DT for getNumberOfFields() */
#define DF2117  2117          /* DF for getNumberOfFields() */
#define DT2296  2296          /* DT for substring() */
#define DF2297  2297          /* DF for substring() */
/*end_of_debug_flags---------*/

/*-------DEBUG_FLAGS--------*/
/**/
/*==========================================================
%FUNCTION NAME
	get_right_hand_side
%SCOPE
	public
%PURPOSE
	Given a line with an =, return the right hand side of the line. And
        oh, by the way malloc space for the string that it is put in.
%NEEDED INCLUDES
	#include ""
%EXAMPLE CALL
        char *line;
        char *rhs = NULL;
	ret_val = get_right_hand_side(line,&rhs);
%RETURNS
	D_S_SUCCESS
	D_S_NOT_FOUND  When no = appears in the string.
	D_F_MALLOC     when not able to malloc any more space.
%METHOD
	
%FILES
	None
%TABLES
	None
%NOTES
	
%BUGS

%FLAGS  
	TRACE DT132
	FULL  DF133
%SYNTAX ARGS
==========================================================*/
int get_right_hand_side(
			char *line,      /* Input  */
			char **rhs      /* Output */
			)

{
   int return_code = D_S_SUCCESS;
   int i = 0;
   int j = 0;
   int string_length = 0;
   char *tmp_rhs = NULL;

   DFNAME("get_right_hand_side");
   DENTER(DT132);

   string_length = strlen(line);
   sprintf(msg,"incoming line = |%s|",line);
   DPR(DF133,msg);
   while (( line[i] != '=') && (i < string_length )) i++;
   
   if ( i < string_length )          /* A "=" found */
   {
      i++;                           /* Go beyond the = */
                                     /* Malloc space for the string */
      tmp_rhs = (char * ) malloc ( (sizeof (char )) * ((string_length - i) + 1) ) ;
      if ( tmp_rhs == NULL )
      {
	DPE("GET_RIGHT_HAND_SIDE: unable to malloc space");
	return_code = D_F_MALLOC;
	goto bottom;
      }
      while ( i < string_length )
      {
	tmp_rhs[j] = line[i];
	i++;
	j++;
      }
      tmp_rhs[j]='\0';
      *rhs = tmp_rhs;
      sprintf(msg,"returning |%s|",*rhs);
      DPR(DF133,msg);
      
   }
   else
   {
     sprintf(msg,"No \"=\" found in the string |%s|",line);
     DPR(DF133,msg);
     return_code = D_S_NOT_FOUND;
   }

 bottom:

   DEXIT(DT132);
   return ( return_code );

} /*** End get_right_hand_side */

/**/
/*==========================================================
%FUNCTION NAME
	get_stuff_before_semicolon
%PURPOSE
	Given a line [with a semi colon], This 
        function will return the part of the line
        before the semicolon.
        
%SYNTAX
	int get_stuff_before_semicolon(char *line,char *lhs)
%EXAMPLE CALL
	char *line = "intran;part(of)";       * Input  *
	char lhs[30];                         * Output *
	ret_val =  get_stuff_before_semicolon(line, lhs);
%RETURNS
	D_S_SUCCESS
%SCOPE
	public 
%NEEDED INCLUDES
	None
%METHOD
	Go through the line, character, by character, saving
        the characters until a semicolon is hit.
%FILES
	None
%NOTES
        The left hand side is assumed to have had space malloced
%BUGS
	
%FLAGS  
	TRACE DT248
	FULL  DF249
%HEADER END
==========================================================*/
int get_stuff_before_semicolon(
			       char *line,       /* Input */
			       char *lhs         /* Output - assumed that space has been malloced for this */
			       )

{
  int return_code = D_S_SUCCESS;
  int i = 0;
  int string_length = 0;

   DFNAME("get_stuff_before_semicolon");
   DENTER(DT248);

   string_length = strlen(line);
   while (( line[i] != ';') && (i < string_length ))
   {
     lhs[i]=line[i];
     i++;
   }
   lhs[i]='\0';
   

   DEXIT(DT248);
   return ( return_code );

} /*** End get_stuff_before_semicolon */
/**/
/*==========================================================
%FUNCTION NAME
	get_stuff_after_semicolon
%PURPOSE
	Given a string, return the part of the string after
        the semicolon.
%SYNTAX
	int get_stuff_after_semicolon(char *line,char *rhs);
%EXAMPLE CALL
	char * line = "intran;part(of);       * Input  *
	char  rhs[30]; ;                      * Output *
	int get_stuff_after_semicolon(line,rhs);
%RETURNS
	D_S_SUCCESS
%SCOPE
	public
%NEEDED INCLUDES
	None
%METHOD
	Taverse the string, character by character until a 
	semicolon is hit. Then traverse the rest of the
        string, character by character, saving each in a
        return string until the end of line is hit.
%FILES
	None
%NOTES
        The right hand side is assumed to have had space malloced
%BUGS
	
%FLAGS  
	TRACE DT250
	FULL  DF251
%HEADER END
==========================================================*/
int get_stuff_after_semicolon(
			      char * line ,       /* Input */
			      char * rhs         /* Output */
			      )

{
   int return_code = D_S_SUCCESS;
   int i = 0;
   int j = 0;
   int string_length = 0;

   DFNAME("get_stuff_after_semicolon");
   DENTER(DT250);


   rhs[0] = '\0';
   string_length = strlen(line);
   while (( line[i] != ';') && (i < string_length )) i++; i++;

   while (i < string_length )
   {
     rhs[j]=line[i];
     i++;
     j++;
   }
   rhs[j]='\0';
   

   DEXIT(DT250);
   return ( return_code );

} /*** End get_stuff_after_semicolon */
/**/
/*==========================================================
%FUNCTION NAME
	get_parameter
%PURPOSE
	Given a parenthesized string of the from 
	foo(xxx,xxx,xxx ... ), where each of the parameters of 
	the predicate foo are delimited by a comma, and given
	the parameter to get, return that parameter.
        
%SYNTAX
	
	int get_parameter(int slot, char *predicate, char * parameter)
%EXAMPLE CALL
	int  slot          = 1;             * Input  ( first slot = 1.
                                                       not 0)  *
	char *predicate;   = "foo(a,b,c)";  * Input *
	char parameter[30];                 * Output -- asumed that space 
                                                        has aready been 
                                                        malloced for it  
	int get_parameter(slot, predicate, parameter);
%RETURNS
	D_S_SUCCESS
%SCOPE
	public
%NEEDED INCLUDES
	None
%METHOD
	Traverse the string until a "(" is hit.
	Advance to the next character.
        For  [slot -1] times
	{
          Traverse the resulting string until either
          a comma or newline is hit
	}

	if not at the end of the string,
          Traverse the rest of the string, saving
          the result, until a comma, ), or end of the
          string is hit.
%FILES
	None
%NOTES
	The fisrt slot is 1, not 0. 

        The resulting string is asumed that space has 
        aready been malloced for it before the invocation
        of this routine.
%BUGS
	
%FLAGS  
	TRACE DT254
	FULL  DF255
%HEADER END
==========================================================*/
int get_parameter(
		  int slot ,          /* Input  ( first slot = 1.  not 0)  */
		  char *predicate,    /* Input */
		  char *parameter     /* Output -- asumed that space has aready been malloced for it*/
		  )

{
   int return_code = D_S_SUCCESS;
   int i = 0;
   int j  = 0;
   int string_length = 0;

   DFNAME("get_parameter");
   DENTER(DT254);
   
   parameter[0] = '\0';
   string_length = strlen(predicate) -1; /* Dont count the final paren */
   while (( predicate[i] != '(') && (i < string_length )) i++; i++;

   for ( j = 1; j < slot; j ++ )
   {
     while (( predicate[i] != ',') &&  (i < string_length ) ) i++;i++;
   }

   j=0;
   while (( predicate[i] != ',') && (i < string_length ) ) 
   {
     parameter[j] = predicate[i];
     i++; j++;
   }
   parameter[j]='\0';

   if ( j == 0 )
     parameter = NULL ;

   sprintf(msg,"returning [%s]",parameter);DPR(DF255,msg);

   DEXIT(DT254);
   return ( return_code );

} /*** End get_parameter */

/**/
/*==========================================================
%FUNCTION NAME
	get_stuff_after_colon
%PURPOSE
	Given a string, return the part of the string after
        the colon.
%SYNTAX
	int get_stuff_after_colon(char *line,char *rhs);
%EXAMPLE CALL
	char *line = "past:negative";         * Input  *
	char  rhs[30];                        * Output *
	int get_stuff_after_colon(line,rhs);
%RETURNS
	D_S_SUCCESS
%SCOPE
	public
%NEEDED INCLUDES
	None
%METHOD
	Taverse the string, character by character until a 
	colon is hit. Then traverse the rest of the
        string, character by character, saving each in a
        return string until the end of line is hit.
%FILES
	None
%NOTES
        The right hand side is assumed to have had space malloced
%BUGS
	
%FLAGS  
	TRACE DT250
	FULL  DF251
%HEADER END
==========================================================*/
int get_stuff_after_colon(
			  char * line ,       /* Input */
			  char * rhs         /* Output */
			  )

{
   int return_code = D_S_SUCCESS;
   int i = 0;
   int j = 0;
   int string_length = 0;

   DFNAME("get_stuff_after_colon");
   DENTER(DT250);


   rhs[0] = '\0';
   string_length = strlen(line);
   while (( line[i] != ':') && (i < string_length )) i++; i++;

   while (i < string_length )
   {
     rhs[j]=line[i];
     i++;
     j++;
   }
   rhs[j]='\0';
   

   DEXIT(DT250);
   return ( return_code );

} /*** End get_stuff_after_colon */

/**/
/*==========================================================
%FUNCTION NAME
	get_stuff_before_colon
%PURPOSE
	Given a line [with a colon], This 
        function will return the part of the line
        before the colon.
%SYNTAX
	int get_stuff_before_colon(char *line,char *lhs)
%EXAMPLE CALL
	char *line = "past:negative";       * Input  *
	char lhs[30];                         * Output *
	ret_val =  get_stuff_before_colon(line, lhs);
%RETURNS
	D_S_SUCCESS
%SCOPE
	public
%NEEDED INCLUDES
	None
%METHOD
	Go through the line, character, by character, saving
        the characters until a semicolon is hit.
%FILES
	None
%NOTES
        The left hand side is assumed to have had space malloced
%BUGS
	
%FLAGS  
	TRACE DT248
	FULL  DF249
%HEADER END
==========================================================*/
int get_stuff_before_colon(
			   char *line,       /* Input */
			   char *lhs         /* Output - assumed that space has been malloced for this */
			   )

{
  int return_code = D_S_SUCCESS;
  int i = 0;
  int string_length = 0;

   DFNAME("get_stuff_before_colon");
   DENTER(DT248);

   string_length = strlen(line);
   while (( line[i] != ':') && (i < string_length ))
   {
     lhs[i]=line[i];
     i++;
   }
   lhs[i]='\0';
   

   DEXIT(DT248);
   return ( return_code );

} /*** End get_stuff_before_colon */
/**/
/*==========================================================
%FUNCTION NAME
	strip
%PURPOSE
	Given a string, and a pattern, this routine will
        strip the first occurrance of the pattern from the
        string.
%SYNTAX
        int strip(char *new_string, char *old_string, char *pattern)
%EXAMPLE CALL
        int strip(char *new_string, char *old_string, char *pattern)
	char new_string[30];         * Output *
	char *old_string  = "aabbcc"; * Input  *
	char *pattern     = "bb";     * Input  *
	ret_val = strip(new_string,old_string,pattern);
%RETURNS
	D_S_SUCCESS
%SCOPE
	public
%NEEDED INCLUDES
	None
%METHOD
	The usual way.
%FILES
	
%NOTES
	The new string is assumed to have had space malloc'd 
        before the invocation of this routine.
%BUGS
	
%FLAGS  
	TRACE DT392
	FULL  DF393
%HEADER END
==========================================================*/
int strip(
	  char * new_string, /* Output */
	  char * old_string, /* Input  */
	  char * pattern     /* Input  */
	  )
   
{
   int return_code = D_S_SUCCESS;
   char *begin_ptr = NULL;
   char beginning_string[MAXLINE];
   char *ending_string = NULL;
   int beginning;
   
   DFNAME("strip");
   DENTER(DT392);


   if ( (begin_ptr = ((char *) strstr(old_string,pattern))) != NULL )
   {
     beginning = (begin_ptr - old_string) + 1;
     ending_string  = begin_ptr  + (strlen (pattern) -1 );
     
     if ( begin_ptr != old_string )
     {
       strncpy(beginning_string,old_string, beginning );
     }
     
     sprintf(new_string,"%s%s",beginning_string,ending_string );
   }
 
   DEXIT(DT392);
   return ( return_code );

} /*** End strip */
/**/
/*==========================================================
%FUNCTION NAME
	get_stuff_before_comma
%PURPOSE
	Given a line [with a comma], This 
        function will return the part of the line
        before the comma.
%SYNTAX
	int get_stuff_before_comma(char *line,char *lhs)
%EXAMPLE CALL
	char *line = "irreg(has,had,having)";       * Input  *
	char lhs[30];                         * Output *
	ret_val =  get_stuff_before_comma(line, lhs);
%RETURNS
	D_S_SUCCESS
%SCOPE
	public
%NEEDED INCLUDES
	None
%METHOD
	Go through the line, character, by character, saving
        the characters until a comma is hit.
%FILES
	None
%NOTES
        The left hand side is assumed to have had space malloced
%BUGS
	
%FLAGS  
	TRACE DT404
	FULL  DF405
%HEADER END
==========================================================*/
int get_stuff_before_comma(
			   char *line,    /* Input  */
			   char *lhs      /* Output */
			   )
{

  
  int return_code = D_S_SUCCESS;
  int i = 0;
  int string_length = 0;

   DFNAME("get_stuff_before_comma");
   DENTER(DT404);

   string_length = strlen(line);
   while (( line[i] != ',') && (i < string_length ))
   {
     lhs[i]=line[i];
     i++;
   }
   lhs[i]='\0';

   DEXIT(DT404);
   return ( return_code );

} /*** End get_stuff_before_comma */
/**/
/*==========================================================
%FUNCTION NAME
	get_filler_slot
%PURPOSE
	Given a string of the form (xxx,xxx(xx,xx),xx(xx(xx))),
	and the slot one wants, return that slot
%SYNTAX
	int get_filler_slot ( int slot_number, char *string, char *slot );
%EXAMPLE CALL
        int    slot = 3;
        char *complement_ = "pphr(to,np,pphr(for,np))";
        char first_phrase[30];
        ret_val =  get_filler_slot ( slot, complement_,first_phrase );
%RETURNS
	D_S_SUCCESS
%SCOPE
	public
%NEEDED INCLUDES
	None
%METHOD
	The usual way.
%FILES
	None
%NOTES
	
%BUGS
	
%FLAGS  
	TRACE DT406
	FULL  DF407
%HEADER END
==========================================================*/
int get_filler_slot(
		    int   slot_number, /* Input */
		    char *filler,     /* Input */
		    char *slot       /* Output */
		    )

{
   int return_code     = 0;
   int i               = 0;
   int j               = 0;
   int see_slot        = 1;
   int predicate_count = 0;
   int not_done        = D_TRUE;
   int string_length   = 0;
   
   DFNAME("get_filler_slot");
   DENTER(DT406);

   
   string_length = strlen(filler) ; 

   while (( not_done == D_TRUE ) && ( i < string_length ) )
   {
     switch ( filler[i] )
     {
     case '(':
       predicate_count++;
       break;

     case ')':
       predicate_count--;
       break;

     case ',':
       if ( predicate_count == 0 )
       {
	 see_slot++;
	 i++;
       }
     }

     if ( see_slot == slot_number )
     {
       slot[j]=filler[i];
       j++;
       slot[j]='\0';
     }
     else if ( see_slot > slot_number )
     {
       not_done = D_FALSE;
     }
     
     i++;

   }


   DEXIT(DT406);
   return ( return_code );

} /*** End get_filler_slot */
/**/
/*==========================================================
%FUNCTION NAME
	get_left_hand_side
%PURPOSE
	Given a line with an =, return the left hand side of the line. And
        oh, by the way malloc space for the string that it is put in.
%SYNTAX
	int get_left_hand_side (char *line char **lhs);
%EXAMPLE CALL
        char *line;
        char *lhs = NULL;
	ret_val = get_left_hand_side(line,&lhs);
%RETURNS
	D_S_SUCCESS
	D_S_NOT_FOUND  When no = appears in the string.
	D_F_MALLOC     when not able to malloc any more space.
%SCOPE
	public
%NEEDED INCLUDES
	#include "?"
%METHOD
	
%FILES
	None
%TABLES
	
%NOTES
	
%BUGS
	
%FLAGS  
	TRACE DT852
	FULL  DF853
%HEADER END
==========================================================*/
int get_left_hand_side( 
		       char *line,       /* Input  */
		       char **lhs        /* Output */
		       )

{
  int return_code = D_S_SUCCESS;
  int i = 0;
  int j = 0;
  int string_length = 0;
  char tmp_lhs[MAXLINE];
  char *tmp_x = NULL;
  

  DFNAME("get_left_hand_side");
  DENTER(DT852);

  string_length = strlen(line);

  sprintf(msg,"incoming line = |%s|",line);
  DPR(DF853,msg);
  while (( line[i] != '=') && (i < string_length ))
  {
    if ( line[i] != SPACE )
    {
      tmp_lhs[j]=line[i];
      j++;
    }
    i++;
  }
  tmp_lhs[j]='\0';
  sprintf(msg,"result is %s",tmp_lhs);DPR(DF853,msg);
  tmp_x =(char *) strdup ( tmp_lhs );
  *lhs = tmp_x;
  

  DEXIT(DT852);
  return ( return_code );
  
} /*** End get_left_hand_side */

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	null_string_cmp
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = null_string_cmp(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT586
	FULL  DF587
%HEADER END
==========================================================*/
int null_string_cmp( 
		    char *word1,
		    char *word2 
		    )
{
  int return_code = -1;
  
  DFNAME("null_string_cmp");
  DENTER(DT586);
  
  DPR(DF587,"");

  if ( word1[0] != '\0' )
  {
    if ( word2[0] != '\0' )
    {
      return_code = strcmp(word1,word2);
    }
    else
    {
      return_code =  1;
    }
  }
  else
  {
    if  (word2[0] == '\0' )
    {
      return_code = 0;
    }
    else
    {
      return_code = -1;
    }
  }
  DEXIT(DT586);

  return ( return_code );
  
} /*** End null_string_cmp */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	replace_string
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = replace_string(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT1078
	FULL  DF1079
%HEADER END
==========================================================*/
int replace_string( 
		   char *pattern_text,
		   char *replacement_text,
		   char *replaced_text)

{
  int return_code = D_S_SUCCESS;
  register char *b_ptr = NULL;
  char tmp_text[10000] = "";
  char mid_text[10000] = "";
  char *first_occurrence = NULL;
  char  marker_text[MAXLINE]="_";
  int done = D_FALSE;
  int i = 0;
  
  DFNAME("replace_string");
  DENTER(DT1078);


  

  memset(tmp_text,0,10000);
  memset(mid_text,0,10000);
  
  
  
  

  if ( (int)  strlen ( replacement_text ) > 0 )
  {
    memset(marker_text,'\177', (int) strlen( replacement_text));
    marker_text[(int)(strlen(replacement_text) + 1) ]=EOS;
  }
  else
  {
    strcpy(marker_text,"\177");
  }

  if ( strcmp( pattern_text, replacement_text ) == 0 )
  {
    sprintf(msg,"The pattern text and replacement text are the same (%s) (%s)",
	    pattern_text,replacement_text);
    DPR(DF1079,msg);
    goto bottom;
  }
  strcpy(mid_text,replaced_text);
  first_occurrence = NULL;
  
  first_occurrence = (char *) strstr(mid_text,pattern_text );

  while (first_occurrence != (char *)  NULL )
  {
    strncpy ( tmp_text, mid_text, (first_occurrence - mid_text));
    
    strcat ( tmp_text, marker_text );
    
    b_ptr = (char *) first_occurrence + strlen ( pattern_text );
    
    strcat ( tmp_text, b_ptr );
    strcpy( mid_text,tmp_text);

    first_occurrence = NULL;
    memset( tmp_text,0,10000);
    b_ptr = NULL;
  

    first_occurrence = (char *) strstr(mid_text,pattern_text );
  }

  first_occurrence = NULL;
  memset( tmp_text,0,10000);
  b_ptr = NULL;
  
  first_occurrence = (char *) strstr(mid_text,marker_text );

  while (first_occurrence != (char *)  NULL )
  {
    strncpy ( tmp_text, mid_text, (first_occurrence - mid_text));
    
    if ( (int ) strlen( replacement_text) > 0 )
    {
      strcat ( tmp_text, replacement_text );
    }
    
    b_ptr = (char *) first_occurrence + strlen ( marker_text );
    
    strcat ( tmp_text, b_ptr );
    strcpy( mid_text,tmp_text);

    first_occurrence = NULL;
    memset(tmp_text,0,10000);
    b_ptr = NULL;

    first_occurrence = (char *) strstr(mid_text,marker_text );
  }
  
  strcpy( replaced_text, mid_text );
  
  
 bottom:
  DEXIT(DT1078);
  return ( return_code );
  
} /*** End replace_string */
/**/
/*==========================================================
%FUNCTION NAME
	not_smart_ccat
%PURPOSE
	Adds characters to a string. This routine increases
	the space of the string, if needed. (i.e., it mallocs
	blocks of space at a time for efficiency)
%SYNTAX
	int not_smart_ccat( char c,               * Input        * 
		            int  *used_space,     * Input/Output * 
		            int  *available_space,* Input/Output * 
		            char **output_string  * Input/Output * 
		          )

%EXAMPLE CALL
	not_smart_ccat( c,&used_space,&available_space,&output_string );
%RETURNS
	D_S_SUCCESS
	D_F_MALLOC
%SCOPE
	public
%NEEDED INCLUDES
	#include "?"
%METHOD
	
%FILES
	
%TABLES
	
%NOTES
	
%BUGS
	
%FLAGS  
	TRACE DT362
	FULL  DF363
%HEADER END
==========================================================*/
int not_smart_ccat( 
		   char c,                     /* Input        */
		   int  *a_used_space ,        /* Input/Output */
		   int  *a_available_space ,   /* Input/Output */
		   char **a_output_string      /* Input/Output */
		   )
{
  int return_code = D_S_SUCCESS;
  int used_space;
  int available_space;
  char *output_string = NULL;

  DFNAME("not_smart_ccat");
  DENTER(DT362);


  used_space = *a_used_space;
  available_space = *a_available_space;
  output_string = *a_output_string;
      if (( used_space == 0) && ( available_space == 0 ))
      {
	output_string = (char * ) malloc ( sizeof ( char ) * MAXLINE );
	available_space = MAXLINE;
      }
      else
      {
	if ( used_space >= available_space )
	{
	  output_string = (char * )realloc ( output_string, sizeof(char ) * 
				   (available_space + MAXLINE ));
	  available_space = available_space + MAXLINE;
	}
      }
      output_string[used_space]=c;
      output_string[ used_space + 1 ] = EOS;
      used_space++;

  *a_used_space = used_space;
  *a_available_space = available_space;
  *a_output_string = output_string;

  DEXIT(DT362);
  return ( return_code );

} /*** End not_smart_ccat */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	base_path
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = base_path(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT460
	FULL  DF461
%HEADER END
==========================================================*/
int base_path(
	      char *full_path, /* Input */
	      char *base_path  /* Output */
	      )

{
  int return_code = D_S_SUCCESS;
  int i,k;
  int finished = D_FALSE;

  DFNAME("base_path");
  DENTER(DT460);

  k = strlen ( full_path );

  strcpy ( base_path, full_path );
  i = k;
  
  while ((i > 0 ) && ( finished == D_FALSE ))
  {
    if ( base_path[i] == PATH_DELIMITER )
    {
      base_path[i]=EOS;
      finished = D_TRUE;
    }
    i--;
  }
  sprintf(msg,"returning (%s)",base_path);DPR(DF461,msg);
  

  DPR(DF461,"");

  DEXIT(DT460);
  return ( return_code );

} /*** End base_path */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	not_smart_concat
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = not_smart_concat(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT484
	FULL  DF485
%HEADER END
==========================================================*/
int not_smart_concat( 
		     char **target,  /* Output */
		     char *source,   /* Input  */
		     int  *allocated_space /* Input/Output */
)


{
  int return_code = D_S_SUCCESS;
  int available_space = 0;
  int current_space = 0;
  int needed_space = 0;
  int new_space = 0;
  char *tmp_target = NULL;

  DFNAME("not_smart_concat");
  DENTER(DT484);


  tmp_target = *target;
  available_space = *allocated_space;

  if ( tmp_target != NULL ) 
  {
    current_space = strlen ( tmp_target );
    needed_space    = strlen ( source ) + strlen ( tmp_target );
  }
  else
  {
    tmp_target = (char * ) malloc (sizeof( char )* MAXLINE );
    available_space = MAXLINE;
    strcpy(tmp_target,"");
    needed_space    = strlen ( source ) ;
  }


/*
  sprintf(msg,"available_space = %d, current_space = %d, needed_space = %d",
	  available_space,
	  current_space,
	  needed_space );
  DPR(DF485,msg);
*/
  if ( needed_space > available_space )
  {
    new_space = sizeof ( char ) * available_space + MAXLINE;
    available_space = new_space;
/*
    sprintf(msg,"makeing space to be %d", new_space );
    DPR(DF485,msg);
*/
    tmp_target = (char * )realloc ( tmp_target, new_space );
  }
  
  strcat ( tmp_target, source );
  
  *target =  tmp_target;
  *allocated_space = available_space;
  

  DEXIT(DT484);
  return ( return_code );

} /*** End not_smart_concat */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	strip_non_ascii_chars
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = strip_non_ascii_chars(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT490
	FULL  DF491
%HEADER END
==========================================================*/
int strip_non_ascii_chars(
			  char *buffer  /* Input/Output */
			  )

{
  int return_code = D_S_SUCCESS;
  int i = 0;
  int j = 0;
  int ln = 0;
  char tmp_buffer[MAXLINE];

  DFNAME("strip_non_ascii_chars");
  DENTER(DT490);


  sprintf(msg,"input = {%s}", buffer ); DPR(DF491,msg);
  strcpy(tmp_buffer,buffer );
  ln = strlen ( buffer );

  for ( i = 0; i < ln; i++ )
  {
    if (( tmp_buffer[i] == TAB )||
       ( tmp_buffer[i] == NEW_LINE ) ||
       (( tmp_buffer[i] >= SPACE) && (tmp_buffer[i] <= TILDA )))
       {
	 buffer[j]=tmp_buffer[i];
	 j++;
       }
    i++;
  }
  buffer[j]=EOS;

  sprintf(msg,"output = {%s}", buffer ); DPR(DF491,msg);
  DEXIT(DT490);
  return ( return_code );

} /*** End strip_non_ascii_chars */
/**/
/*==========================================================
  %FUNCTION NAME
%COMMAND NAME
	nls_copy
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = nls_copy(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT514
	FULL  DF515
%HEADER END
==========================================================*/
int nls_copy(
	     char **target,         /* Output       */
	     char  *source,         /* Input        */
	     int  *allocated_space  /* Input/Output */
	     )

{
  int return_code = D_S_SUCCESS;
  int available_space = 0;
  int current_space = 0;
  int needed_space = 0;
  int new_space = 0;
  char *tmp_target = NULL;


  DFNAME("nls_copy");
  DENTER(DT514);

  DPR(DF515,"");

  tmp_target = *target;
  available_space = *allocated_space;

  if ( tmp_target != NULL ) 
  {
    current_space   = strlen ( tmp_target );
    needed_space    = strlen ( source ) + strlen ( tmp_target );
  }
  else
  {
    tmp_target = (char * ) malloc (sizeof( char )* MAXLINE );
    available_space = MAXLINE;
    strcpy(tmp_target,"");
    needed_space    = strlen ( source ) ;
  }


  if ( needed_space > available_space )
  {
    new_space = sizeof ( char ) * available_space + MAXLINE;
    available_space = new_space;

    tmp_target = (char * )realloc ( tmp_target, new_space );
  }
  
  strcpy ( tmp_target, source );
  
  *target =  tmp_target;
  *allocated_space = available_space;
  

  DEXIT(DT514);
  return ( return_code );

} /*** End nls_copy */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	nls_concat
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = nls_concat(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT516
	FULL  DF517
%HEADER END
==========================================================*/
int nls_concat(
	       char **target,  /* Output */
	       char *source,   /* Input  */
	       int  *allocated_space /* Input/Output */
	       )
	       
{
  int return_code = D_S_SUCCESS;

  DFNAME("nls_concat");
  DENTER(DT516);

  return_code = not_smart_concat ( target, source, allocated_space );

  DEXIT(DT516);
  return ( return_code );

} /*** End nls_concat */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	nls_replace_string
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = nls_replace_string(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT520
	FULL  DF521
%HEADER END
==========================================================*/
int nls_replace_string(
		       char *pattern_text,
		       char *replacement_text,
		       char **replaced_text)

{
  int return_code = D_S_SUCCESS;
  register char *b_ptr = NULL;
  char *tmp_text = NULL;
  char *mid_text = NULL;
  char *first_occurrence = NULL;
  char  marker_text[MAXLINE]="_";
  int done = D_FALSE;
  int i = 0;
  int replaced_text_size = 0;
  int pattern_ctr = 0;
  int pattern_size = 0;
  int replacement_size = 0;
  int new_size = 0;
  

  DFNAME("nls_replace_string");
  DENTER(DT520);

  DPR(DF521,"");

  replaced_text_size = strlen ( *replaced_text );

  tmp_text = (char *) malloc ( (sizeof (char ) * replaced_text_size)+2 );
  if ( tmp_text == NULL )
  {
    return_code = D_F_MALLOC;
    DPM(return_code );
    goto bottom;
  }
  mid_text = (char *) malloc ( (sizeof (char ) * replaced_text_size)+2 );

  if ( tmp_text == NULL )
  {
    return_code = D_F_MALLOC;
    DPM(return_code );
    goto bottom;
  }

  memset(tmp_text,0,replaced_text_size);
  memset(mid_text,0,replaced_text_size);
  
  
  
  

  if ( (int)  strlen ( replacement_text ) > 0 )
  {
    memset(marker_text,'\177', (int) strlen( replacement_text));
    marker_text[(int)(strlen(replacement_text) + 1) ]=EOS;
  }
  else
  {
    strcpy(marker_text,"\117");
  }

  if ( strcmp( pattern_text, replacement_text ) == 0 )
  {
    sprintf(msg,"The pattern text and replacement text are the same (%s) (%s)",
	    pattern_text,replacement_text);
    DPR(DF1079,msg);
    goto bottom;
  }
  not_smart_copy(mid_text,*replaced_text);
  first_occurrence = NULL;
  
  first_occurrence = (char *) strstr(mid_text,pattern_text );

  while (first_occurrence != (char *)  NULL )
  {
    strncpy ( tmp_text, mid_text, (first_occurrence - mid_text));
    
    strcat ( tmp_text, marker_text );
    
    b_ptr = (char *) first_occurrence + strlen ( pattern_text );
    
    strcat ( tmp_text, b_ptr );
    not_smart_copy( mid_text,tmp_text);

    first_occurrence = NULL;
    memset( tmp_text,0,10000);
    b_ptr = NULL;
  
    pattern_ctr++;

    first_occurrence = (char *) strstr(mid_text,pattern_text );
  }

/* ---------------------------------------------------
   realloc space to fit the string about to be altered
   --------------------------------------------------- */
  pattern_size = strlen ( pattern_text );
  replacement_size = strlen ( replacement_text );
  new_size = 
  ((replaced_text_size  + 
    (replacement_size * pattern_ctr )) - 
   (pattern_size * pattern_ctr ));

  tmp_text = (char * )realloc ( tmp_text , new_size );
  mid_text = (char * )realloc ( mid_text, new_size );


  first_occurrence = NULL;
  memset( tmp_text,0,new_size);
  b_ptr = NULL;
  
  first_occurrence = (char *) strstr(mid_text,marker_text );

  while (first_occurrence != (char *)  NULL )
  {
    strncpy ( tmp_text, mid_text, (first_occurrence - mid_text));
    
    if ( (int ) strlen( replacement_text) > 0 )
    {
      strcat ( tmp_text, replacement_text );
    }
    
    b_ptr = (char *) first_occurrence + strlen ( marker_text );
    
    strcat ( tmp_text, b_ptr );
    not_smart_copy( mid_text,tmp_text);

    first_occurrence = NULL;
    memset(tmp_text,0,new_size);
    b_ptr = NULL;

    first_occurrence = (char *) strstr(mid_text,marker_text );
  }
  
  *replaced_text =  mid_text ;
  
 bottom:

  CHAR_FREE ( tmp_text );

  DEXIT(DT520);
  return ( return_code );

} /*** End nls_replace_string */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_next_row
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_next_row(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT724
	FULL  DF725
%HEADER END
==========================================================*/
char *get_next_row(
		   char *list,          /* Input  */
		   char **current_ptr   /* Output */
		   )

{
  static char return_code[MAXLINE];
  char *ptr = NULL;
  int i = 0;
  char *return_val = NULL;

  
  DFNAME("get_next_row");
  DENTER(DT724);

  ptr = *current_ptr;

  if ( ptr == NULL )
  {
    ptr = list;
  }
  
  if ( ptr != NULL )
  {
    while (( *ptr != EOS ) && ( *ptr != NEW_LINE ) )
    {
      return_code[i]= *ptr;
      i++;
      ptr++;
    }
    
    return_code[i]= EOS;
    i = 0;
    
    if ( *ptr == NEW_LINE )
    {
      ptr++;
    }

    else if ( *ptr == EOS )
    {
      ptr = NULL;
      return_val = NULL;
      sprintf(msg,"Exiting, and returning a NULL PTR");
      DPR(DF725,msg);
    }

  }
  
  *current_ptr = ptr;
  
  if ( ptr == NULL )
  {
    return_val = NULL;
  }
  else
  {
    return_val = return_code;
  }


  DEXIT(DT724);
  return ( return_val );

} /*** End get_next_row */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	getNumberOfFields
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = getNumberOfFields(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2116
	FULL  DF2117
%HEADER END
==========================================================*/
int getNumberOfFields( char fs, char *row )

{
  int numberOfFields = 1; 
  char *ptr = NULL;

  DFNAME("getNumberOfFields");
  DENTER(DT2116);

  ptr = row;

  while (*ptr != EOS ) {
    if ( *ptr == fs ) 
      numberOfFields++;
    ptr++;
  }

  DEXIT(DT2116);
  return ( numberOfFields );

} /*** End getNumberOfFields */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	substring
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = substring(arg);
%RETURNS
	?
%SCOPE
	?public | private | static
%NEEDED INCLUDES
	#include "?"
%METHOD
	?
%FILES
	?
%TABLES
	?
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2296
	FULL  DF2297
%HEADER END
==========================================================*/
int subString(
	      char *origString,   /* Input  */
	      char *newString,    /* Output / U Malloc space for it */
	      int  begin,         /* Input, starts with 0 */
	      int  end            /* Input, exclusive     */
	      )
{
  int return_code = D_S_SUCCESS;
  
  int k = 0;
  int i = 0;

  if (( end > begin ) && ( origString != NULL ) && ( (signed int) strlen ( origString ) >= (signed int) end )) {  
    for (i = begin; i <end; i++ ) {
      newString[k]= origString[i];
      newString[ k + 1 ] = EOS;
      k++;
    }
    
  }

  return ( return_code );

} /*** End substring */
