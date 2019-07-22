
/*
% File:	    lexical.c
% Module:   Lexical
% Author:   Lan
% Purpose:  Provide access to uninvert function in the lexical library.
*/

/* lexical.c -- access uninvert function in the lexical library.
*/

static char sccs_id_lexical_c[] = "@(#)lexical.c	1.3 09/27/06";


extern void uninvert();

char invertedTerm[8192];

static char inputString[8192];
static char outputString[8192];


/*****************************************************************************/
/* Get the input and output cstring addresses
*/

long int C_get_cstring_addresses(inputAddress, outputAddress)
unsigned long *inputAddress;
unsigned long *outputAddress;
{
    *inputAddress = (unsigned long) &inputString[0];
    *outputAddress = (unsigned long) &outputString[0];
    return(0);
} /* C_get_cstring_addresses */


/*******************************************************************************/
/* Function to call uninvert using statically defined inputString and
   outputString.  inputString must be set before calling C_uninvert_no_args.
   outputString is set to the result.
*/
long int C_uninvert_no_args()
{
	uninvert(inputString,outputString);
	return(0);
} /* C_uninvert_no_args */


/*
	lex.c - functions for the lex library
*/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "lexical.h"

/************************************************************************/
/*
	Recursively uninverts a string, i.e., injury, abdominal ==> abdominal injury
	INPUT:	string "s" containing the term to be uninverted.
	OUTPUT: string "t" containing the uninverted string.
*/
void uninvert(s,t)
char *s;
char *t;
{
	register char *sp;
	register char *cp;
	register char *tp;

	if (*s==EOS)
	{
		*t = EOS;
		return;
	}
	sp = s;
	while ((sp=strchr(sp,COMMA))!=(char *)NULL)
	{
		cp = sp;
		cp++;
		if (*cp==SPACE)
		{
			while (*cp!=EOS && *cp==SPACE)	cp++;
			uninvert(cp,t);
			strcat(t," ");
			tp = &t[strlen(t)];
			while (s<sp)
			{
				*tp++ = *s++;
			}
			*tp = EOS;
			return;
		}
		else
		{
			sp++;
		}
	}
	strcpy(t,s);
}
