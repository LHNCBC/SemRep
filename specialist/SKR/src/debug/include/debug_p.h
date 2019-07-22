/*==========================================================

%SOURCE FILE
	debug.h

%DESCRIPTION OF FILE
	C source header file.
%REVISED
	Sep 18 1996 divita -- Initial Version

%%
==========================================================*/


/* 
   prevent sccs_id_debug_p_h from being re-defined in case
   debug_p.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_debug_p_h
static char sccs_id_debug_p_h[] = "@(#)debug_p.h	1.2 09/27/06";
#define sccs_id_debug_p_h 1
#endif

#ifndef _DEBUG_
#define _DEBUG_

     /* ==========================================
        Prototypes for file add2set.c
        ==========================================  */
int add_string_to_set();


     /* ==========================================
        Prototypes for file add_str2str.c
        ==========================================  */
int add_string_to_string();


     /* ==========================================
        Prototypes for file cfg_utils.c
        ==========================================  */
int     cfg_read(char *config_filename       /* Input */);
char   *cfg_get (char   *variable_name         /* Input */);
int     cfg_free();
int     cfg_show();
int     nls_cfg_read();
int     www_cfg_read();
int     cfg_get2();
int     add_cfg();
void    cfg_write( char *config_filename       /* Input */ );

void    cfg_update(
		   char *variable_name,  /* Input */
		   char *value           /* Input */
		   ) ;
 



     /* ==========================================
        Prototypes for file debug.c
        ==========================================  */
int DPR(
	int flag,  /* Input - flag this print statement belongs to */
	char *msg  /* Input - msg to print */
	);

int dfname(
	   char *function_name /* Input function name */
	   );
int denter(
	   int flag /* Input */
	   );

int dexit(
	  int flag  /* Input - flag */
	  );

int in_flag_list(
		 int flag  /* Input */
		 );

int cgi_debug_init( 
	       char *program_name,        /* Input */
	       char *output_file        /* Input */
	       );

int debug_term();

int DINTERPRET(
   int  message_code,  /* Input - message number */
   int  *message_type, /* Output - SUCCESS|MESSAGE|WARNING|ERROR|FATAL */
   char *message       /* Output message - expects space already alloc'd */
	       );

int DPRN(
	 int flag,  /* Input - flag this print statement belongs to */
	 char *msg  /* Input - msg to print */
	 );
   
int DPE(
	char *msg   /* Input */
	);

int DPM( 
	int message_number  /* Input */
       );

int debug_init( char *program_name );



     /* ==========================================
        Prototypes for file get_value.c
        ==========================================  */
int get_value_from_field(int  field_position, /* Input                */
			  char   *value,       /* Output -space should */
                                               /* already be malloced  */
			  char   *umls_record  /* Input                */
			  );

int get_meta_field(
		    int field_position,    /* Input                  */
		    char   **value,        /* Output alloc's space -  */
		    char   *umls_record    /* Input                   */
		    );
int get_ism_field(
		   int     field_position,  /* Input                   */
		   char   **value,           /* Output alloc's space -  */
		   char    *umls_record      /* Input                   */
		   );

int getValueFromField(
		      char     fieldDelimiter, /* Input */
		      int      field_position, /* Input                */
		      char    *value,          /* Output -space should */
                                               /* already be malloced  */
		      char   *umls_record      /* Input                */
		      );

     /* ==========================================
        Prototypes for file is_null.c
        ==========================================  */
int is_null();


     /* ==========================================
        Prototypes for file messages.c
        ==========================================  */
int close_msg_stack(char *file    /* Input   */);
int add_msg_to_stack(char * file,      /* Input */
		     char * message    /* Input */
		     );
int print_msg_stack(char *file /* Input */);
int clear_msg_stack(char *file  /* Input */);
int print_msg_to_stack(char *file,    /* Input */
		       char *message  /* Input */
		       );



     /* ==========================================
        Prototypes for file strip.c
        ==========================================  */
int strip_white_spaces();
int strip_lefthandside_spaces();


     /* ==========================================
        Prototypes for file utils.c
        ==========================================  */
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
void nls_llip(register char *s );
void nls_luip(register char *s );
int getNumberOfFields( char fs, char *row );
int subString(
	      char *origString,   /* Input  */
	      char *newString,    /* Output / U Malloc space for it */
	      int  begin,         /* Input, starts with 0 */
	      int  end            /* Input, exclusive     */
	      );

/*
#ifndef strcasecmp
int strcasecmp(
	       char *string1,   * Input * 
	       char *string2    * Input * 
	       );
#endif
*/

#endif
