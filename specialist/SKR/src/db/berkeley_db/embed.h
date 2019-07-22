
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

/*
% File:	    embed.h
% Module:   Berkeley DB
% Author:   Jim
*/

#define MAXLINE  65536
#define MAXCOLS     10
#define TRUE         1
#define FALSE        0
#define INT_TYPE   350
#define TXT_TYPE   375
 
struct query_struct
{
     int num_fields;
     char *fields[MAXCOLS];
     char *table;
     char *query;
     char *where2;
     char *query2;
};
 
struct config_struct
{
     char file_name[100];        /* formerly MAXLINE */
     char table[100];            /* formerly MAXLINE */
     int opened;		/* Boolean TRUE/FALSE if opened already */
     int num_fields;
     char fields[MAXCOLS][21];  /* formerly 50 */
     int field_types[MAXCOLS];
};
 
struct res_rows_struct
{
    int num_cols;
    int col_type[MAXCOLS];
    int int_result[MAXCOLS];
    char *str_result[MAXCOLS];
};
 
struct results_struct
{
    int num_rows;
    int config_ptr;
    struct res_rows_struct **rows;
};

