
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
   word.h - header dealing with word tokens
*/


/* 
   prevent sccs_id_wl_h from being re-defined in case
   wl.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_wl_h
static char sccs_id_wl_h[] = "@(#)wl.h	1.3 09/27/06";
#define sccs_id_wl_h 1
#endif

/* for handling a list of words */
typedef struct _wordList {
    char **words;		/* pointer to words */
    int	n;			/* number of words */
} WordList;

/* ptr to n'th word of a wordlist */
#define WLWN(wl,n)	    (*(((wl)->words)+(n)))

#define STRIP_WHITE_SPACE        1
#define KEEP_EVERYTHING          2
#define DONT_BREAK_ON_HYPHENS    3
#define ORIG_WORD_DEF            4
#define DONT_BREAK_ON_QUESTION   5
#define BREAK_ON_SPACE           6
#define RUSSELLS_RULES           7
#define REPLACE_PUNCT_WITH_SPACE 8

