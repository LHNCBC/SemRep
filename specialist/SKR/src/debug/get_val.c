
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
	get_value.c

%DESCRIPTION OF FILE
	C source file.

%REVISED
	17Oct94 divita -- Initial Version

%%
==========================================================*/

/*----------------------------
%INCLUDES
----------------------------*/
#include "debug.h"

/*end of includes ----------*/

/*----------------------------
%FUNCTION PROTOTYPES
----------------------------*/
int get_value_from_field(int field_position, char *value, char   *umls_record);
/*end_of_function_prototypes*/

/*----------------------------
%DEBUG FLAGS
----------------------------*/

#define DT410  410          /* DT for get_value_from_field() */
#define DF411  411          /* DF for get_value_from_field() */

/*end_of_debug_flags---------*/

/**/
/*==========================================================
%FUNCTION NAME
%COMMAND NAME
	get_value_from_field
%PURPOSE
	?
%USAGE
%SYNTAX
	?
%EXAMPLE CALL
	?ret_val = get_value_from_field(arg);
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
	TRACE DT410
	FULL  DF411
%HEADER END
==========================================================*/
int get_value_from_field(
			  int field_position,  /* Input                */
			  char   *value,       /* Output -space should */
                                               /* already be malloced  */
			  char   *umls_record  /* Input                */
			  )

{
   int return_code = D_S_SUCCESS;
   int current_field  = 1;
   int i,char_len,j=0;
   char   tmpvalue[MAXLINE];

   
   DFNAME("get_value_from_field");
   DENTER(DT410);

   
  /* ----------------------------------------------
     Some yutes, like me have taken files off the
     cd, and not stripped off the cr's on them 
     this bit of code is an attempt to stip that off
   if (( umls_record != NULL ) && 
      ( strlen(umls_record) > 2 ) &&
      ( umls_record[ strlen(umls_record) -2] < ' ')
   {
     char_len = strlen(umls_record) -2 ;
   }
   else
     ---------------------------------------------- */
   {
     char_len = strlen(umls_record);
   }
                                  
   
   strcpy(tmpvalue,"");
   strcpy(value,"");


   for (i =0;i < char_len; i++ )
   {
      if ( umls_record[i] == '|' )
      {
	 current_field ++;
	 continue;
      }


      if ( current_field > field_position )
	 break;

      if (field_position == current_field )
      {
	if ( umls_record[i] != '\n' )
	{
	  tmpvalue[j] = umls_record[i];
	  j++;
	}
      }

   }

   if (j == 0 )
   {

     value[0] = EOS;
   }
   else
   {
     tmpvalue[j]=EOS;
     strcpy(value,tmpvalue);
   }
   

   DEXIT(DT410);
   return ( return_code );

} /*** End get_value_from_field */
