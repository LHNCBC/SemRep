
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

/* loc.c - input file locator stack functions
*/

#include <stdio.h>
#include <string.h>
#include <malloc.h>

int push_loc(
	     char *includeFile,
	     int *linenoP,
	     char **curFileP
		);
int pop_loc(
	    int *curLineNumP,
	    char **curFileNameP
	    );

int int_loc(
	int *linenoP,
	char **curFileP
	);

typedef struct _locNode {
    int lineno;
    char *filename;
    struct _locNode *next;
} LocNode;

typedef LocNode *LocStack;
static LocNode *locStack = (LocNode *) NULL;

/* pushes old location on stack */
int
push_loc(
	 char *inputLine,
	 int *curLineNumP,
	 char **curFileNameP
	 )
{
    LocNode *loc;
    char *incFile;
    char *sp, *ep;

    if ((loc = (LocNode *) malloc((unsigned)(sizeof(LocNode)))) == (LocNode *)NULL)
	return(0);
    loc->lineno = *curLineNumP;
    loc->filename = *curFileNameP;
    loc->next = locStack;
    locStack = loc;

    sp = &inputLine[10];
    if ((ep = strchr(sp, '"')) == (char *)NULL)
	return(0);
    if ((incFile = malloc((unsigned)(ep-sp+1))) == (char *)NULL)
	return(0);
    strncpy(incFile, sp, (size_t)(ep-sp));
    *(incFile + (int)(ep-sp)) = '\0';
    *curFileNameP = incFile;
    *curLineNumP = 0;
    return(1);
}

/* pops stack */
int
pop_loc(
	int *curLineNumP,
	char **curFileNameP
	)
{
    LocNode *node = locStack;

    if (node == (LocNode *)NULL)
	return(0);
    locStack = node->next;

    if (*curFileNameP != (char *)NULL)
	(void) free(*curFileNameP);

    *curLineNumP = node->lineno;
    *curFileNameP = node->filename;
    (void) free((char *)node);
    return(1);
}
