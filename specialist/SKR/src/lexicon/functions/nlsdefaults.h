/*==========================================================

%SOURCE FILE
	nlsdefaults.h

%DESCRIPTION OF FILE
	C include file.

%REVISED
	20Apr94 divita -- Initial Version

==========================================================*/


/* 
   prevent sccs_id_nlsdefaults_h from being re-defined in case
   nlsdefaults.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_nlsdefaults_h
static char sccs_id_nlsdefaults_h[] = "@(#)nlsdefaults.h	1.3 09/27/06";
#define sccs_id_nlsdefaults_h 1
#endif

/*----------------------------
%INCLUDES
----------------------------*/

/*----------------------------
%CONSTANTS
----------------------------*/
#define NLS_S_SUCCESS  0
#define NLS_E_ERROR    1              /* Generic Error */
#define NLS_E_MALLOC   2              /* Call to Malloc Failed */
#define NLS_E_FILE     3              /* Error trying to open or close a file*/
#define NLS_E_FSEEK    4
#define NLS_E_FTELL    5
#define NLS_E_FPUTS    6
#define NLS_S_NUMBER_EXISTS 10      



#define NLS_TRUE     1
#define NLS_FALSE    0


#define SEEK_SET  0                  /* These should have been in the stdio.h*/
#define SEEK_CUR  1                  /* But are not in sun's implementation  */
#define SEEK_END  2

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

/*==========================================================
%CSC NAME
	?
%INFORMATION FOR CSC
	?
%LIBRARIES
	?
%%
==========================================================*/
