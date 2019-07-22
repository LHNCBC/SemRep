
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

/* tokenizer.c - tokenizes terms
*/

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include "lexicon.h"

void etok(char *s, char *t);
void ltok(char *s, char *t);
int tok_size(register char *s);
void detok(char *s);
static void tok(register char *s, register char *t, int flag);

/*  Tokenization algorithm:

    A token consists of runs of alphanumerics or single character punctation.
    Tokens are separated by the character: LEX_TOKEN_SEPARATOR.

    For example assuming the separator to be "|":

    "this is a string" -> "this| |is| |a| |string"
    "C3/C5 convertase" -> "C3|/|C5| |convertase"
    "(+)-cyanid" -> "(|+|)|-|cyanid"

*/

/* tokenizes retaining case, s into t*/
void etok(
	  char *s,
	  char *t
	  )
{
    tok(s, t, LEX_EXACT_TOKENIZE);
    return;
}

/* tokenizes and lowercases s into t */
void ltok(
	  char *s,
	  char *t
	  )
{
    tok(s, t, LEX_LOWER_TOKENIZE);
    return;
}

/* tokenizer function */
static void tok(
		register char *s,
		register char *t,
		int flag
		)
{
    register char *sp = s;
    register int boundary = 0;

    while (*sp != EOS)
    {
	    if (!isalnum((int)*sp))
	{
	    if (sp > s)
		*t++ = LEX_TOKEN_SEPARATOR;
	    *t++ = *sp++;
	    boundary = 1;
	}
	else
	{
	    if (boundary == 1)
		*t++ = LEX_TOKEN_SEPARATOR;
	    boundary = 0;
	    *t++ = (char)((isupper((int)*sp) && (flag == LEX_LOWER_TOKENIZE)) ? tolower((int)*sp) : *sp);
	    sp++;
	}
    }
    *t = EOS;
    return;
}

/* size of tokenized term */
int tok_size(
	     register char *s
	     )
{
    register int n = 0;
    register char *sp = s;
    register int boundary = 0;

    while (*sp != EOS)
    {
	    if (!isalnum((int)*sp))
	{
	    if (sp > s)
		n++;
	    n++;
	    sp++;
	    boundary = 1;
	}
	else
	{
	    if (boundary == 1)
		n++;
	    boundary = 0;
	    n++;
	    sp++;
	}
    }
    return(n+1);
}

/* de-tokenizer (in place) */
void detok(char *s)
{
    register char *to;
    register char *from;

    to = from = s;
    while (*from != EOS)
    {
	if (*from == LEX_TOKEN_SEPARATOR)
	    from++;
	else
	    *to++ = *from++;
    }
    *to = EOS;
    return;
}

/*
main()
{
    char s[64], t[64];

    while (gets(s) != (char *)NULL)
    {
	printf("%s --> ", s);
	etok(s, t);
	printf("%s [%d]\n", t, tok_size(s));
    }
}
*/
