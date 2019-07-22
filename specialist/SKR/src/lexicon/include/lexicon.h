
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
  lexicon.h - defines some macros for the lexicon
*/


/*  A lexicon index entry has a key and data element.  The key is the
    tokenized version of the word in question and is NUL terminated.
    The first 4 bytes of the key however are reserved for flags, described
    below.  The data element simply consists of a 4 byte offset into
    the lexicon file for the actual record.

    The flags in the index entry are encoded thus:

    Bit 1: set for adjectives
    Bit 2: set for nouns
    Bit 3: set for adverbs
    Bit 4: set for auxiliaries
    Bit 5: set for complementizers
    Bit 6: set for conjunctions
    Bit 7: set for determiners
    Bit 7: set for modals
    Bit 8: set for nouns
    Bit 9: set for prepositions
    Bit 10: set for pronouns
    Bit 11: set for verbs

    Bit 12: set for base forms
    Bit 13: set for spelling variants
    Bit 14: set for inflections of base forms
    Bit 15: set for inflections of spelling variants
    Bit 16: set for lowercase versions of base forms
    Bit 17: set for lowercase versions of spelling variants
    Bit 18: set for lowercase versions of inflections of base forms
    Bit 19: set for lowercase versions of inflections of spelling variants

    Bit 20 thru 32: unused.

    For example, if the bit string is:

	21098765432109876543210987654321
                       00010010000000010

    stands for the inflection of a word that is both a noun and a verb, e.g., insults.

*/

#define LEXICON

/* define the lex type */
typedef unsigned long lex_t;

#define EOS		'\0'	    /* End-of-string */
#define COMMA		','
#define RPAREN		')'
#define LPAREN		'('
#define PIPE		'|'
#define LEX_TOKEN_SEPARATOR '|'
#define LEX_EXACT_TOKENIZE 0
#define LEX_LOWER_TOKENIZE 1

#define LEX_ERROR   (-1)
#define LEX_OK	    (0)

#define LEX_INDEX_PAGE_SIZE	8192
#define LEX_DEFAULT_ALLOCATION	64


/*
    The lexicon B-tree index entry consists of key and data elements.
    A key consists of some flags and the characters of the key, while
    the data element contains the byte offset of the matching record.
*/

/* bits #1 -> #11 encode syntactic category */
#define LEX_CAT_ADJ	(lex_t)(0x1 << 0)
#define LEX_CAT_ADV	(lex_t)(0x1 << 1)
#define LEX_CAT_AUX	(lex_t)(0x1 << 2)
#define LEX_CAT_COMPL	(lex_t)(0x1 << 3)
#define LEX_CAT_CONJ	(lex_t)(0x1 << 4)
#define LEX_CAT_DET	(lex_t)(0x1 << 5)
#define LEX_CAT_MODAL	(lex_t)(0x1 << 6)
#define LEX_CAT_NOUN	(lex_t)(0x1 << 7)
#define LEX_CAT_PREP	(lex_t)(0x1 << 8)
#define LEX_CAT_PRON	(lex_t)(0x1 << 9)
#define LEX_CAT_VERB	(lex_t)(0x1 << 10)

/* bit #12 is set if entry is a base= */
#define LEX_BASE		(lex_t)(0x1 << 11)
#define IS_LEX_BASE(U)		(((lex_t)(U) & LEX_BASE) == LEX_BASE)

/* bit #13 is set if entry is a spelling variant */
#define LEX_SV			(lex_t)(0x1 << 12)
#define IS_LEX_SV(U)		(((lex_t)(U) & LEX_SV) == LEX_SV)

/* bit #14 is set if entry is the infl of a base */
#define LEX_BASEINFL		(lex_t)(0x1 << 13)
#define IS_LEX_BASEINFL(U)	(((lex_t)(U) & LEX_BASEINFL) == LEX_BASEINFL)

/* bit #15 is set if entry is the infl of a spelling variant */
#define LEX_SVINFL		(lex_t)(0x1 << 14)
#define IS_LEX_SVINFL(U)	(((lex_t)(U) & LEX_SVINFL) == LEX_SVINFL)

/* bit #16 is set if entry is the lowercased version of a base form */
#define LEX_BASELOWER		(lex_t)(0x1 << 15)
#define IS_LEX_BASELOWER(U)	(((lex_t)(U) & LEX_BASELOWER) == LEX_BASELOWER)
#define MAKE_LEX_BASELOWER(U)	( U |= LEX_BASELOWER)

/* bit #17 is set if entry is the lowercased version of a spelling variant */
#define LEX_SVLOWER		(lex_t)(0x1 << 16)
#define IS_LEX_SVLOWER(U)	(((lex_t)(U) & LEX_SVLOWER) == LEX_SVLOWER)
#define MAKE_LEX_SVLOWER(U)	( U |= LEX_SVLOWER)

/* bit #18 is set if entry is the lowercased version of an inflection of a base form */
#define LEX_BASEINFLLOWER	(lex_t)(0x1 << 17)
#define IS_LEX_BASEINFLLOWER(U)	(((lex_t)(U) & LEX_BASEINFLLOWER) == LEX_BASEINFLLOWER)
#define MAKE_LEX_BASEINFLLOWER(U)	( U |= LEX_BASEINFLLOWER)

/* bit #19 is set if entry is the lowercased version of an inflection of a spelling */
#define LEX_SVINFLLOWER		(lex_t)(0x1 << 18)
#define IS_LEX_SVINFLLOWER(U)	(((lex_t)(U) & LEX_SVINFLLOWER) == LEX_SVINFLLOWER)
#define MAKE_LEX_SVINFLLOWER(U)	(U |= LEX_SVINFLLOWER)

/* bit #20 is set if the term contains an orphan token*/
#define LEX_CONTAINS_ORPHAN_TOKEN	(lex_t)(0x1 << 19)
#define CONTAINS_ORPHAN_TOKEN(U)	(((lex_t)(U) & LEX_CONTAINS_ORPHAN_TOKEN) == LEX_CONTAINS_ORPHAN_TOKEN)

/* start and end of lexical record */

#define ISSOR(s)		(strncmp((s),"{base=",6)==0)
#define ISEOR(s)		((*(s))=='}' && ((*((s)+1))=='\n'))

/*  for lcat  and lf */
#define PRINT_RECORD 0
#define PRINT_BASE   1
#define PRINT_BASESV 2
#define PRINT_QUERY  3
#define PRINT_KEY    4
#define PRINT_INDEX  5

#define DEFAULT_FIELD_SEPARATOR '|'

typedef struct _lexIndexKey {
    lex_t likFlags;
    char *likKey;
    int likHide;
} LexIndexKey;

