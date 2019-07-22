/*==========================================================

%SOURCE FILE
	derrno.h

%DESCRIPTION OF FILE
	C include file.

%REVISED
	10Aug94 divita -- Initial Version

%%
==========================================================*/


/* 
   prevent sccs_id_derrno_h from being re-defined in case
   derrno.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_derrno_h
static char sccs_id_derrno_h[] = "@(#)derrno.h	1.1 09/28/06";
#define sccs_id_derrno_h 1
#endif

/*----------------------------
%INCLUDES
----------------------------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#ifndef _DERNO
#define _DERNO


#define SUCCESS 1
#define MESSAGE 5
#define WARNING 4 
#define ERROR   2
#define FATAL   3


#endif
/*----------------------------
%MACROS
----------------------------*/

/*----------------------------
%EXTERNAL FUNCTIONS
----------------------------*/

/*----------------------------
%PRIVATE STRUCTURES
----------------------------*/

/*----------------------------
%TYPEDEFS
----------------------------*/
typedef struct{
     int    key;
     int    message_type;
     char  *neumonic;
     char  *message;
   } Derror;

#define D_S_SUCCESS          1
#define D_E_ERROR            0
#define D_F_MALLOC           2      /* Call to Malloc Failed */
#define D_E_FILE             3      /* Error trying to open or close a file*/
#define D_E_FSEEK            4
#define D_E_FTELL            5
#define D_E_FPUTS            6
#define D_F_ERROR            7
#define D_S_NUMBER_EXISTS    8
#define D_S_EOF              9
#define D_S_NOT_FOUND       10
#define D_E_ERRLOG          11
#define D_E_DBGLOG          12
#define D_F_VALNODENEW       13
#define D_W_UNEXPECTED_VALUE 14
#define D_E_UNEXPECTED_VALUE 15
#define D_W_DESIGN_DEFECT    16
#define D_F_ASNLOAD          17
#define D_F_ASNIOOPEN        18 

static Derror _derror [] =
{
  D_S_SUCCESS,       SUCCESS, "D_S_SUCCESS",      "Success",
  D_E_ERROR,         ERROR  , "D_E_ERROR",        "Error",
  D_F_MALLOC,        FATAL  , "D_E_MALLOC",       "Call to malloc failed",
  D_E_FILE,          ERROR  , "D_E_FILE",         "Error trying to open or close a file",
  D_E_FSEEK,         ERROR  , "D_E_FSEEK",        "Call to fseek failed",  
  D_E_FTELL,         ERROR  , "D_E_FTELL",        "Call to ftell failed",  
  D_E_FPUTS,         ERROR  , "D_E_FPUTS",        "Call to fputs failed", 
  D_F_ERROR,         FATAL  , "D_F_ERROR",        "Fatal Error",
  D_S_NUMBER_EXISTS, SUCCESS, "D_S_NUMBER_EXISTS","The Entry is not a new entry",
  D_S_EOF,           SUCCESS, "D_S_EOF",          "End of File Hit",
  D_S_NOT_FOUND,     SUCCESS, "D_S_NOT_FOUND",    "Not found",
  D_E_ERRLOG,        ERROR  , "D_E_ERRLOG",       "Unable to write to the error log",
  D_E_DBGLOG,        ERROR  , "D_E_DBGLOG",       "Unable to write to the debug log",
  D_F_VALNODENEW,    FATAL,   "D_F_VALNODENEW",   "Unable to create a new valnode",
  D_W_UNEXPECTED_VALUE, WARNING, "D_W_UNEXPECTED_VALUE", "Unexpected or Unknown value has been seen by this routine",
  D_E_UNEXPECTED_VALUE, ERROR, "D_E_UNEXPECTED_VALUE", "An Unexpected or Unknown value has been seen by this routine",
  D_W_DESIGN_DEFECT, WARNING, "D_W_DESIGN_DEFECT", "A Design defect",
  D_F_ASNLOAD,       FATAL,   "D_F_ASNLOAD",      "Unable to initialize asn api s",
  D_F_ASNIOOPEN,     FATAL,   "D_F_ASNIOOPEN",    "Unable to open ASN encoded file"
};
#define D_NUMBER_OF_MESSAGES 19


/*==========================================================
%CSC NAME
	?
%INFORMATION FOR CSC
	?
%LIBRARIES
	?
%%
==========================================================*/
