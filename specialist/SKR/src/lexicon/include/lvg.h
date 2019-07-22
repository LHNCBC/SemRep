
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
	lvg.h

%DESCRIPTION OF FILE
	C include file.
	Header for lexical variant generation.

%REVISED
	23Dec94 divita -- Modified Version

==========================================================*/


/*----------------------------
Some platform depenant things
----------------------------*/

#define LVG_DEFAULT_ALLOCATION 256

#define FS  '|'
#define LVG_FIELD_SEPARATOR '|'
#define EOS '\0'		/* end-of-string */
#define NEWLINE '\n'
#define newLine "\n"
#define SPACE ' '
#define COMMA ','
#define QUOTE '\''
#define TAB '\t'
#define EOL '\n'

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

#define CASE_INSENSITIVE        1
#define CASE_SENSITIVE          2
