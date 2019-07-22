
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

/*==========================================================

%SOURCE FILE
	debug.h

%DESCRIPTION OF FILE
	C include file.

%REVISED
	9May94 divita -- Initial Version

%%
==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include <malloc.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

/*----------------------------
%CONSTANTS
----------------------------*/
#define DT 		      1
#define DF 		      2
#define MAX_MSG_SIZE 	  10000
#define D_TRUE  	      1
#define D_FALSE 	      0
#define MAX_FLAGS 	   4500

#define SUCCESS 	      1
#define MESSAGE 	      5
#define WARNING 	      4 
#define ERROR   	      2
#define FATAL   	      3

#define D_S_SUCCESS           1
#define D_E_ERROR             0
#define D_F_MALLOC            2      /* Call to Malloc Failed */
#define D_E_FILE              3      /* Error trying to open or close a file*/
#define D_E_FSEEK             4
#define D_E_FTELL             5
#define D_E_FPUTS             6
#define D_F_ERROR             7
#define D_S_NUMBER_EXISTS     8
#define D_S_EOF               9
#define D_S_NOT_FOUND        10
#define D_E_ERRLOG           11
#define D_E_DBGLOG           12
#define D_F_VALNODENEW       13
#define D_W_UNEXPECTED_VALUE 14
#define D_E_UNEXPECTED_VALUE 15
#define D_W_DESIGN_DEFECT    16
#define D_F_ASNLOAD          17
#define D_F_ASNIOOPEN        18 

#define D_NUMBER_OF_MESSAGES 19

#define EOS             '\0'
#define TAB             '\t'
#define SPACE           ' '
#define PIPE            '|'

#define BIG_LINE_SIZE   9000
#define MAXLINE         BIG_LINE_SIZE

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int dfname(char *function_name);
int denter(int flag);
int dexit(int flag);

/*----------------------------
%MACROS
----------------------------*/

#define DFNAME(x) \
          char *msg = NULL; \
          dfname(x);

#define DENTER(x) \
          denter(x); \
          msg = (char *) malloc ( BIG_LINE_SIZE );

#define DEXIT(x) \
        CHAR_FREE(msg); \
        dexit(x);

#define CHAR_FREE(x)  if (x != NULL ) free (x); x = (char *)NULL;  

/*----------------------------
%TYPEDEFS
----------------------------*/
typedef struct{
     int    key;
     int    message_type;
     char  *neumonic;
     char  *message;
   } Derror;


#ifdef NT
#define strcasecmp(x,y) stricmp(x,y)
#endif

/*end of constants ---------*/
