/*==========================================================

%SOURCE FILE
	cfg_utils.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	26Jan95 divita -- Initial Version

%%
==========================================================*/

static char sccs_id_cfg_util_c[] = "@(#)cfg_util.c	1.2	09/27/06";

/*----------------------------
%INCLUDES
----------------------------*/
#include "debug.h"
#include <stdio.h>

/*end of includes ----------*/

/*----------------------------
%CONSTANTS
----------------------------*/
/*end of constants ---------*/

/*----------------------------
%MACROS
----------------------------*/
/*end of macros ------------*/

/*----------------------------
%STATIC FUNCTIONS
----------------------------*/
/*end of static functions --*/

/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/
extern int strip_lefthandside_spaces ( char * );
extern int get_left_hand_side(char *, char **);
extern int get_right_hand_side(char *, char **);
extern int base_path ( char *, char * );
/*end of external functions */

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
/*
int     cfg_read(char *config_filename        * Input * );
char   *cfg_get (char   *variable_name          * Input * );
int     cfg_free();
int     cfg_show();
int     nls_cfg_read();
int     www_cfg_read();
int     cfg_get2();
int     add_cfg();
int cfg_update();
int cfg_write();
*/
/*end_of_function_prototypes*/

/*----------------------------
%GLOBAL VARIABLES
----------------------------*/
/*end of global variables --*/


/*----------------------------
%TYPEDEFS
----------------------------*/
typedef struct struct_ConfigurableVariable {
   struct struct_ConfigurableVariable        *next;
   char                                      *name;
   char                                      *value;
 }ConfigurableVariable,     *ConfigurableVariablePtr;
/*end of typedefs ----------*/


/*----------------------------
%PRIVATE STRUCTURES
----------------------------*/
static ConfigurableVariablePtr cv      = NULL;
static ConfigurableVariablePtr last_cv = NULL;

/*end of private structures */


/*----------------------------
%DEBUG FLAGS
----------------------------*/

#define DT842  842          /* DT for cfg_read() */
#define DF843  843          /* DF for cfg_read() */
#define DT844  844          /* DT for cfg_get() */
#define DF845  845          /* DF for cfg_get() */
#define DT846  846          /* DT for cfg_free() */
#define DF847  847          /* DF for cfg_free() */
#define DT848  848          /* DT for cfg_show() */
#define DF849  849          /* DF for cfg_show() */
#define DT456  456          /* DT for nls_cfg_read() */
#define DF457  457          /* DF for nls_cfg_read() */
#define DT458  458          /* DT for www_cfg_read() */
#define DF459  459          /* DF for www_cfg_read() */
#define DT494  494          /* DT for cfg_get2() */
#define DF495  495          /* DF for cfg_get2() */
#define DT706  706          /* DT for add_cfg() */
#define DF707  707          /* DF for add_cfg() */
#define DT2526  2526          /* DT for cfg_update() */
#define DF2527  2527          /* DF for cfg_update() */
#define DT2528  2528          /* DT for cfg_write() */
#define DF2529  2529          /* DF for cfg_write() */
/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	cfg_read
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = cfg_read(arg);
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
	TRACE DT842
	FULL  DF843
%HEADER END
==========================================================*/
int cfg_read( 
	     char *config_filename       /* Input */
	     )

{
  int                       return_code             = D_S_SUCCESS;
  FILE                     *config_file_fp          = (FILE *) NULL;
  char                      local_cfg_file[MAXLINE];
  char                      line[MAXLINE];
  ConfigurableVariablePtr   new_cv                  = NULL;
  char                      *name = NULL;
  char                      *value = NULL;
  
  DFNAME("cfg_read");
  DENTER(DT842);
  
  strcpy(local_cfg_file,"");
  strcpy(line,"");

  if ( config_filename == ( char *) NULL )
  {
    sprintf(msg,"No config filename given, using default");
    DPE(msg);
    strcpy(local_cfg_file, "./config.txt");
  }
  else
  {
    strcpy(local_cfg_file, config_filename);
    sprintf(msg,"using %s",local_cfg_file );
    DPR(DF843,msg);
  }

  config_file_fp = (FILE *)fopen( local_cfg_file, "r");

  if ( config_file_fp == (FILE *) NULL )
  {
    sprintf(msg,"Unable to open [%s]", local_cfg_file );
    DPE(msg);
    return_code = D_E_FILE; 
    goto bottom;
  }
  else
  {
    sprintf(msg,"opened %s",local_cfg_file );
    DPR(DF843,msg);
  }

  strcpy(line,"");
  fgets(line, MAXLINE, config_file_fp );
  
  while ( !feof( config_file_fp ) )
  {

    line[ strlen(line) -1] = EOS;

    sprintf(msg,"|%s|",line);
    DPR(DF843,msg);


    if ( line != NULL )
    {
/*
      strip_white_spaces ( line );
*/
      strip_lefthandside_spaces ( line );
      
      if (( line[0]!= '#' ) && ((int)strlen( line ) > 3 ))
      {
	/*
	new_cv=(ConfigurableVariablePtr)malloc(sizeof(ConfigurableVariable));
	if (new_cv == (ConfigurableVariablePtr) NULL )
	{
	  return_code = D_F_MALLOC;
	  DPM(return_code );
	  goto bottom ;
	}
	else
	{
	  new_cv->name  = (char *) (NULL);
	  new_cv->value = (char *) (NULL);

	  if (cv == NULL )
	  {
	    cv = new_cv;
	    last_cv = cv;
	  }
	  else
	  {
	    last_cv->next = new_cv;
	    new_cv->next = (ConfigurableVariablePtr) NULL;
	    last_cv = new_cv;
	  }
	}
	get_left_hand_side(line, &new_cv->name);
	get_right_hand_side(line,&new_cv->value);
	*/
	get_left_hand_side(line, &name);
	get_right_hand_side(line,&value);
	cfg_update( name, value );
	CHAR_FREE( name );
	CHAR_FREE( value );
      }
    }
    strcpy(line,"");
    fgets(line, MAXLINE, config_file_fp );
  }
 bottom:
  fclose(config_file_fp);

  DEXIT(DT842);
  return ( return_code );

} /*** END cfg_read */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	cfg_get
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = cfg_get(arg);
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
	TRACE DT844
	FULL  DF845
%HEADER END
==========================================================*/
char *cfg_get( 
		char *variable_name  /* Input */
		)

{

  char                 *return_ptr      = NULL;
  ConfigurableVariablePtr current_cv      = NULL;
  int                     found           = D_FALSE;

  DFNAME("cfg_get");
  DENTER(DT844);
  
  current_cv = cv;
  while ((current_cv != NULL ) && (found == D_FALSE ))
  {
    if ( strcmp(current_cv->name,variable_name) == 0 )
    {
      found = D_TRUE;
    }
    else
    {
      current_cv = current_cv->next;
    }
  }

  if ( found == D_TRUE )
  {
    return_ptr = current_cv->value;
  }

  DEXIT(DT844);
  return ( return_ptr );
  
} /*** End cfg_get */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	cfg_free
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = cfg_free(arg);
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
	TRACE DT846
	FULL  DF847
%HEADER END
==========================================================*/
int cfg_free()

{
  int                     return_code     = D_S_SUCCESS;
  ConfigurableVariablePtr current_cv      = NULL;
  ConfigurableVariablePtr previous_cv     = NULL;

  
  DFNAME("cfg_free");
  DENTER(DT846);
  
  /* -----------------------------------

     cv
      |
     \|/
     
     +--+     +--+    +--+    +--+
     |  |  -> |  | -> |  | -> |  |
     +--+     +--+    +--+    +--+
               ^
               current cv

      ^
      prev cv

     ----------------------------------- */
  current_cv = cv;
  while (current_cv != NULL ) 
  {
    if ( current_cv->name != NULL ) {
      free( current_cv->name );
      current_cv->name = NULL;
    }
    if ( current_cv->value != NULL ) {
      free( current_cv->value );
      current_cv->value = NULL; 

    }
      
    previous_cv = current_cv;
    current_cv  = current_cv->next;
    previous_cv->next = NULL;
    free ( previous_cv );
    previous_cv = NULL;
  }

  /*------------------------------------
    The first previous_cv points to cv,
    and it gets free'd so cv is already 
    freed
    ------------------------------------ */
  cv = NULL;
  
  
  DEXIT(DT846);
  return ( return_code );

} /*** End cfg_free */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	cfg_show
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = cfg_show(arg);
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
	TRACE DT848
	FULL  DF849
%HEADER END
==========================================================*/
int cfg_show()
{

  int                     return_code     = D_S_SUCCESS;
  ConfigurableVariablePtr current_cv      = NULL;

  DFNAME("cfg_show");
  DENTER(DT848);
  
  current_cv = cv;
  while (current_cv != NULL ) 
  {
    if  ( current_cv->name == NULL ) 
    {
      sprintf(msg,"current name = NULL ");
      DPE(msg);
      goto bottom;
    }
    if  ( current_cv->value == NULL ) 
    {
      sprintf(msg,"current value = NULL" );
      DPE(msg);
      goto bottom;
    }
    sprintf(msg,"%s=%s", current_cv->name,current_cv->value );
    DPR(DF849,msg);
    current_cv = current_cv->next;
  }
  
 bottom:
  DEXIT(DT848);
  return ( return_code );
  
} /*** End cfg_show */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	nls_cfg_read
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = nls_cfg_read(arg);
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
	TRACE DT456
	FULL  DF457
%HEADER END
==========================================================*/
int nls_cfg_read(
		 char *config_filename       /* Input */
		 )

{
  int return_code = D_S_SUCCESS;
  char  nls_root[MAXLINE];
  char  config_file[MAXLINE];

  DFNAME("nls_cfg_read");
  DENTER(DT456);

  strcpy(nls_root,"");
  strcpy(config_file,"");

/* -----------------------------------
   Figure out where the config file is 
   ----------------------------------- */


  if (( char *) getenv("NLS") != NULL )
  {
    strcpy(nls_root ,(char *) getenv("NLS"));
    sprintf(config_file,"%s%cconfig%c%s",
	    nls_root,
	    PATH_DELIMITER,
	    PATH_DELIMITER,
	    config_filename);
  }
  else
  {
    /* ----------------------------------------------
       The NLS environement variable has not been set 
       ---------------------------------------------- */
    sprintf(msg,"The $NLS environment variable has not been set"); DPE(msg);
    sprintf(msg,"Can't find the location of the config file");     DPE(msg);
    sprintf(msg,"Unable to continue");                             DPE(msg);
    return_code = D_E_ERROR;
    goto bottom;
  }

/* --------------------------------------------
   Pick up the needed configuration information
   -------------------------------------------- */
  return_code = cfg_read( config_file );
  
 bottom:

  DEXIT(DT456);
  return ( return_code );

} /*** End nls_cfg_read */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	www_cfg_read
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = www_cfg_read(arg);
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
	TRACE DT458
	FULL  DF459
%HEADER END
==========================================================*/
int www_cfg_read(
		 char *config_filename       /* Input */
		 )

{
  int return_code = D_S_SUCCESS;
  char  nls_root[MAXLINE];
  char  config_file[MAXLINE];
  char  config_path[MAXLINE];

  DFNAME("www_cfg_read");
  DENTER(DT458);

  DPR(DF459,"");

/* -----------------------------------
   Figure out where the config file is 
   ----------------------------------- */
  if (( char *) getenv("DOCUMENT_ROOT") != NULL )
  {
    strcpy(nls_root ,(char *) getenv("DOCUMENT_ROOT"));
    
    /* -------------------------------------------------------
       Go up one directory to find out where the config dir is
       ------------------------------------------------------- */
    base_path ( nls_root, config_path );
    sprintf(config_file,"%s/config/%s",config_path,config_filename);
  }
  else
  {
    /* ----------------------------------------------
       The NLS environement variable has not been set 
       ---------------------------------------------- */
    sprintf(msg,"The $DOCUMENT_ROOT environment variable has not been set"); DPE(msg);
    sprintf(msg,"Can't find the location of the config file");     DPE(msg);
    sprintf(msg,"Unable to continue");                             DPE(msg);
    return_code = D_E_ERROR;
    goto bottom;
  }

/* --------------------------------------------
   Pick up the needed configuration information
   -------------------------------------------- */
  return_code = cfg_read( config_file );
  
 bottom:

  DEXIT(DT458);
  return ( return_code );

} /*** End www_cfg_read */
/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	add_cfg
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = add_cfg(arg);
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
	TRACE DT706
	FULL  DF707
%HEADER END
==========================================================*/
int add_cfg(
	    char *name, /* Input */
	    char *value /* Input */
	    )

{
  int return_code = D_S_SUCCESS;
  ConfigurableVariablePtr new_cv    = NULL;

  DFNAME("add_cfg");
  DENTER(DT706);


  new_cv=(ConfigurableVariablePtr)calloc(1,sizeof(ConfigurableVariable));
  if (new_cv == (ConfigurableVariablePtr) NULL )
  {
    return_code = D_F_MALLOC;
    DPM(return_code );
    goto bottom ;
  }
  else
  {
    new_cv->name  = (char *) (NULL);
    new_cv->value = (char *) (NULL);
    
    if (cv == NULL )
    {
      cv = new_cv;
      last_cv = cv;
    }
    else
    {
      last_cv->next = new_cv;
      new_cv->next = (ConfigurableVariablePtr) NULL;
      last_cv = new_cv;
    }
  }
  new_cv->name  = strdup( name );
  new_cv->value = strdup( value);

 bottom:
  
  DEXIT(DT706);
  return ( return_code );
  
} /*** End add_cfg */
/**/
/*==========================================================
%FUNCTION NAME
	cfg_update
%PURPOSE
        This routine updates a value for a particular field.
%SYNTAX
	int cfg_update(
	               char *variable_name,   * Input * 
	               char *value            * Input * 
	               ) 
 
%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2526
	FULL  DF2527
%HEADER END
==========================================================*/
void cfg_update(
	       char *variable_name,  /* Input */
	       char *value           /* Input */
	       ) 
{
  ConfigurableVariablePtr current_cv      = NULL;
  int                     found           = D_FALSE;

  DFNAME("cfg_update");
  DENTER(DT2526);

  current_cv = cv;
  while ((current_cv != NULL ) && (found == D_FALSE ))
  {
    if ( strcmp(current_cv->name,variable_name) == 0 )
    {
      found = D_TRUE;
    }
    else
    {
      current_cv = current_cv->next;
    }
  }

  if ( found == D_TRUE )
  {
    free(  current_cv->value  );
    
    current_cv->value = strdup ( value );
     
  } else {

    add_cfg( variable_name, value );

  }

  

  DEXIT(DT2526);

} /*** End cfg_update */
/**/
/*==========================================================
%FUNCTION NAME
	cfg_write
%PURPOSE
	This function writes out the contents of the cfg settings
	to a file
%SYNTAX
	int cfg_write(
	              char *config_filename        * Input * 
	             )

%NOTES
	?
%BUGS
	?
%FLAGS  
	TRACE DT2528
	FULL  DF2529
%HEADER END
==========================================================*/
void cfg_write(
	      char *config_filename       /* Input */
	      )

{
  FILE                     *config_file_fp          = (FILE *) NULL;
  char                      config_file[MAXLINE];
  ConfigurableVariablePtr current_cv      = NULL;

  DFNAME("cfg_write");
  DENTER(DT2528);


    sprintf(config_file,"%s%cconfig%c%s",
	    getenv("NLS"),
	    PATH_DELIMITER,
	    PATH_DELIMITER,
	    config_filename);
  

  config_file_fp = (FILE *)fopen( config_file, "w");

  if ( config_file_fp == (FILE *) NULL )
  {
    sprintf(msg,"Unable to open [%s]", config_file);
    DPE(msg);
    goto bottom;
  }
  else
  {
    sprintf(msg,"opened %s",config_file); DPR(DF843,msg);
  }
  
  current_cv = cv;
  while (current_cv != NULL ) 
  {
    if  ( current_cv->name == NULL ) 
    {
      continue;
    }
    if  ( current_cv->value == NULL ) 
    {
      continue;
    }
    fprintf(config_file_fp,"%s=%s\n", current_cv->name,current_cv->value );
    current_cv = current_cv->next;
  }

  fclose( config_file_fp );
  

 bottom:

  DEXIT(DT2528);

} /*** End cfg_write */
