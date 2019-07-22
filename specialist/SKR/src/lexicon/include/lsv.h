
/* 
   prevent sccs_id_lsv_h from being re-defined in case
   lsv.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_lsv_h
static char sccs_id_lsv_h[] = "@(#)lsv.h	1.3 09/27/06";
#define sccs_id_lsv_h 1
#endif

#ifndef _LSV_H
#define _LSV_H

#include <trie.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <miscutil_p.h>

typedef unsigned int lsv_t;
/* macro to convert bcopy into memmove , and bzero to memset
*/
#define old_bcopy(x,y,z) memmove(y,x,z)
#define old_bzero(x,y)   memset(x,0,y)


/*  macros for syntactic category.
*/
#define LSV_CAT_ADJ	((lsv_t)(0x1 << 0))
#define LSV_CAT_ADV	((lsv_t)(0x1 << 1))
#define LSV_CAT_AUX	((lsv_t)(0x1 << 2))
#define LSV_CAT_COMPL	((lsv_t)(0x1 << 3))
#define LSV_CAT_CONJ	((lsv_t)(0x1 << 4))
#define LSV_CAT_DET	((lsv_t)(0x1 << 5))
#define LSV_CAT_MODAL	((lsv_t)(0x1 << 6))
#define LSV_CAT_NOUN	((lsv_t)(0x1 << 7))
#define LSV_CAT_PREP	((lsv_t)(0x1 << 8))
#define LSV_CAT_PRON	((lsv_t)(0x1 << 9))
#define LSV_CAT_VERB	((lsv_t)(0x1 << 10))

#define LSV_CAT_ALL	(LSV_CAT_ADJ|LSV_CAT_ADV|LSV_CAT_AUX|\
			    LSV_CAT_COMPL|LSV_CAT_CONJ|\
			    LSV_CAT_DET|LSV_CAT_MODAL|\
			    LSV_CAT_NOUN|LSV_CAT_PREP|\
			    LSV_CAT_PRON|LSV_CAT_VERB)

#define IS_LSV_CAT_ADJ(c)   (((c) & LSV_CAT_ADJ) == LSV_CAT_ADJ)
#define IS_LSV_CAT_ADV(c)   (((c) & LSV_CAT_ADV) == LSV_CAT_ADV)
#define IS_LSV_CAT_AUX(c)   (((c) & LSV_CAT_AUX) == LSV_CAT_AUX)
#define IS_LSV_CAT_COMPL(c)   (((c) & LSV_CAT_COMPL) == LSV_CAT_COMPL)
#define IS_LSV_CAT_CONJ(c)   (((c) & LSV_CAT_CONJ) == LSV_CAT_CONJ)
#define IS_LSV_CAT_DET(c)   (((c) & LSV_CAT_DET) == LSV_CAT_DET)
#define IS_LSV_CAT_MODAL(c)   (((c) & LSV_CAT_MODAL) == LSV_CAT_MODAL)
#define IS_LSV_CAT_NOUN(c)   (((c) & LSV_CAT_NOUN) == LSV_CAT_NOUN)
#define IS_LSV_CAT_PREP(c)   (((c) & LSV_CAT_PREP) == LSV_CAT_PREP)
#define IS_LSV_CAT_PRON(c)   (((c) & LSV_CAT_PRON) == LSV_CAT_PRON)
#define IS_LSV_CAT_VERB(c)   (((c) & LSV_CAT_VERB) == LSV_CAT_VERB)

/*  macros for inflection (first four are aliases)
*/
#define LSV_INFL_BASE		((lsv_t)(0x1 << 0))
#define LSV_INFL_POSITIVE	((lsv_t)(0x1 << 0))
#define LSV_INFL_SINGULAR	((lsv_t)(0x1 << 0))
#define LSV_INFL_INFINITIVE	((lsv_t)(0x1 << 0))
#define LSV_INFL_COMPARATIVE	((lsv_t)(0x1 << 1))
#define LSV_INFL_SUPERLATIVE	((lsv_t)(0x1 << 2))
#define LSV_INFL_PLURAL		((lsv_t)(0x1 << 3))
#define LSV_INFL_PRESPART	((lsv_t)(0x1 << 4))
#define LSV_INFL_PAST		((lsv_t)(0x1 << 5))
#define LSV_INFL_PASTPART	((lsv_t)(0x1 << 6))
#define LSV_INFL_PRES3PS	((lsv_t)(0x1 << 7))
#define LSV_INFL_PRESENT	((lsv_t)(0x1 << 8))

#define  LSV_INFL_BEAM               ((lsv_t)(0x1 <<9 ))
#define  LSV_INFL_BEWERENEGATIVE     ((lsv_t)(0x1 <<10))
#define  LSV_INFL_BEWERE             ((lsv_t)(0x1 <<11))
#define  LSV_INFL_BEWASNEGATIVE      ((lsv_t)(0x1 <<12))
#define  LSV_INFL_BEARE              ((lsv_t)(0x1 <<13))
#define  LSV_INFL_BEARENEGATIVE      ((lsv_t)(0x1 <<14))
#define  LSV_INFL_BEWAS              ((lsv_t)(0x1 <<15))
#define  LSV_INFL_BASENEGATIVE       ((lsv_t)(0x1 <<16))
#define  LSV_INFL_PASTNEGATIVE       ((lsv_t)(0x1 <<17))
#define  LSV_INFL_PRESENTNEGATIVE    ((lsv_t)(0x1 <<18))
#define  LSV_INFL_PRES3PSNEGATIVE    ((lsv_t)(0x1 <<19))

#define LSV_INFL_ALL		(LSV_INFL_BASE|LSV_INFL_COMPARATIVE|\
				LSV_INFL_SUPERLATIVE|LSV_INFL_PLURAL|\
				LSV_INFL_PRESPART|LSV_INFL_PAST|\
				LSV_INFL_PASTPART|LSV_INFL_PRES3PS|LSV_INFL_PRESENT|\
				LSV_INFL_BEAM|LSV_INFL_BEWERENEGATIVE|\
                                LSV_INFL_BEWERE|LSV_INFL_BEWASNEGATIVE|\
                                LSV_INFL_BEARE|LSV_INFL_BEARENEGATIVE|\
                                LSV_INFL_BEWAS|LSV_INFL_BASENEGATIVE|\
                                LSV_INFL_PASTNEGATIVE|LSV_INFL_PRESENTNEGATIVE|\
                                LSV_INFL_PRES3PSNEGATIVE)



#define LSV_DEFAULT_ADJ_INFLS    (LSV_INFL_BASE|LSV_INFL_COMPARATIVE|LSV_INFL_SUPERLATIVE)
#define LSV_DEFAULT_ADV_INFLS    (LSV_INFL_BASE|LSV_INFL_COMPARATIVE|LSV_INFL_SUPERLATIVE)
#define LSV_DEFAULT_NOUN_INFLS   (LSV_INFL_BASE|LSV_INFL_PLURAL)
#define LSV_DEFAULT_VERB_INFLS   (LSV_INFL_BASE|LSV_INFL_PRESPART|LSV_INFL_PAST|\
                                  LSV_INFL_PASTPART|LSV_INFL_PRES3PS|LSV_INFL_PRESENT\
                                  LSV_INFL_BEAM|LSV_INFL_BEWERENEGATIVE|\
                                  LSV_INFL_BEWERE|LSV_INFL_BEWASNEGATIVE|\
                                  LSV_INFL_BEARE|LSV_INFL_BEARENEGATIVE|\
                                  LSV_INFL_BEWAS|LSV_INFL_BASENEGATIVE|\
                                  LSV_INFL_PASTNEGATIVE|LSV_INFL_PRESENTNEGATIVE|\
                                  LSV_INFL_PRES3PSNEGATIVE)

#define IS_LSV_INFL_BASE(c)         (((c) & LSV_INFL_BASE) == LSV_INFL_BASE)
#define IS_LSV_INFL_POSITIVE(c)     (((c) & LSV_INFL_POSITIVE) == LSV_INFL_POSITIVE)
#define IS_LSV_INFL_SINGULAR(c)     (((c) & LSV_INFL_SINGULAR) == LSV_INFL_SINGULAR)
#define IS_LSV_INFL_INFINITIVE(c)   (((c) & LSV_INFL_INFINITIVE) == LSV_INFL_INFINITIVE)
#define IS_LSV_INFL_COMPARATIVE(c)  (((c) & LSV_INFL_COMPARATIVE) == LSV_INFL_COMPARATIVE)
#define IS_LSV_INFL_SUPERLATIVE(c)  (((c) & LSV_INFL_SUPERLATIVE) == LSV_INFL_SUPERLATIVE)
#define IS_LSV_INFL_PLURAL(c)       (((c) & LSV_INFL_PLURAL) == LSV_INFL_PLURAL)
#define IS_LSV_INFL_PRESPART(c)     (((c) & LSV_INFL_PRESPART) == LSV_INFL_PRESPART)
#define IS_LSV_INFL_PAST(c)         (((c) & LSV_INFL_PAST) == LSV_INFL_PAST)
#define IS_LSV_INFL_PASTPART(c)     (((c) & LSV_INFL_PASTPART) == LSV_INFL_PASTPART)
#define IS_LSV_INFL_PRES3PS(c)      (((c) & LSV_INFL_PRES3PS) == LSV_INFL_PRES3PS)
#define IS_LSV_INFL_PRESENT(c)      (((c) & LSV_INFL_PRESENT) == LSV_INFL_PRESENT)

#define IS_LSV_INFL_BEAM(c)           (((c) & LSV_INFL_BEAM) == LSV_INFL_BEAM)
#define IS_LSV_INFL_BEWERENEGATIVE(c) (((c) & LSV_INFL_BEWERENEGATIVE) == LSV_INFL_BEWERENEGATIVE)
#define IS_LSV_INFL_BEWERE(c)         (((c) & LSV_INFL_BEWERE) == LSV_INFL_BEWERE)
#define IS_LSV_INFL_BEWASNEGATIVE(c)  (((c) & LSV_INFL_BEWASNEGATIVE) == LSV_INFL_BEWASNEGATIVE)
#define IS_LSV_INFL_BEARE(c)          (((c) & LSV_INFL_BEARE) == LSV_INFL_BEARE)
#define IS_LSV_INFL_BEARENEGATIVE(c)  (((c) & LSV_INFL_BEARENEGATIVE) == LSV_INFL_BEARENEGATIVE)
#define IS_LSV_INFL_BEWAS(c)          (((c) & LSV_INFL_BEWAS) == LSV_INFL_BEWAS)
#define IS_LSV_INFL_BASENEGATIVE(c)   (((c) & LSV_INFL_BASENEGATIVE) == LSV_INFL_BASENEGATIVE)
#define IS_LSV_INFL_PASTNEGATIVE(c)   (((c) & LSV_INFL_PASTNEGATIVE) == LSV_INFL_PASTNEGATIVE)
#define IS_LSV_INFL_PRESENTNEGATIVE(c)(((c) & LSV_INFL_PRESENTNEGATIVE) == LSV_INFL_PRESENTNEGATIVE)
#define IS_LSV_INFL_PRES3PSNEGATIVE(c)(((c) & LSV_INFL_PRES3PSNEGATIVE) == LSV_INFL_PRES3PSNEGATIVE)





/*  other macros
*/
#define LSV_SOR_CHAR	'^'	/* anchors rule to beginning of word */
#define LSV_EOR_CHAR	'$'	/* anchors rule to end of word */
#define LSV_COMMENT_CHAR '#'	/* marks comments in rule/fact files. */
#define LSV_FIELD_SEPARATOR '|'	/* separates fields in rules/fact files. */
#define LSV_EXCEPTION_PAIR_TERMINATOR ';'   /* terminates pair of exceptions */
#define LSV_EXCEPTION_PAIR_SEPARATOR '|'    /* separates terms of an exception */

/*  some magic numbers
*/
#define LSV_IM_MAGIC	0x19316
#define LSV_DM_MAGIC	0x19317
#define LSV_SYN_MAGIC	0x19318
#define LSV_ACR_MAGIC	0x19319

/*  locator macros
*/
#define LSV_INP_SOR	((lsv_t)(0x1 << 0))
#define LSV_INP_EOR	((lsv_t)(0x1 << 1))
#define LSV_OUT_SOR	((lsv_t)(0x1 << 2))
#define LSV_OUT_EOR	((lsv_t)(0x1 << 3))

/*  general macros
*/
#define IS_VOWEL(c)  (((c)=='a')||((c)=='e')||((c)=='i')||((c)=='o')||((c)=='u'))
#define IS_CONSONANT(c)	(isalpha((c)) && !(IS_VOWEL((c))))
#define LSV_IM_SMALLEST_STEM 1	/* at least a single character stem. used to be 0. */
#define LSV_DM_SMALLEST_STEM 3	/* at least a three character stem. Started out at 2, then 0, now 3. */

#define EOS '\0'
#define NEWLINE '\n'
#define LSV_DEFAULT_ALLOCATION 256
#define LSV_LONGEST_CAT_STR 16
#define LSV_LONGEST_INFL_STR 16
#define LSV_LONGEST_TYPE_STR 16
#define LSV_INPUT_BUFFER_SIZE 256

#define LSV_DEBUG_OFF        0
#define LSV_DEBUG_ON         1
#define LSV_DEBUG_WITH_EUIS  2




/*  macros for different modules
*/
#define LSV_IM	    ((lsv_t)(0x1 << 0))
#define LSV_DM	    ((lsv_t)(0x1 << 1))
#define LSV_SYN	    ((lsv_t)(0x1 << 2))
#define LSV_ACR	    ((lsv_t)(0x1 << 3))

/*  structure for a IM rule
*/
typedef struct _lsvImRule {
    int		    inpSuffix;
    lsv_t	    inpCat;
    lsv_t	    inpInfl;
    int		    outSuffix;
    lsv_t	    outCat;
    lsv_t	    outInfl;
    lsv_t	    locs;	/* locator flags */
    int		    numXpn;	/* number of exceptions */
    int		    xpn;	/* offset into array of exceptions */
}   LsvImRule;

/*  structure for a DM rule
*/
typedef struct _lsvDmRule {
    int		    inpSuffix;
    lsv_t	    inpCat;
    int		    outSuffix;
    lsv_t	    outCat;
    lsv_t	    locs;	/* locator flags */
    int		    numXpn;	/* number of exceptions */
    int		    xpn;	/* offset into array of exceptions */
}   LsvDmRule;

/*  structure for an IM fact
*/
typedef struct _lsvImFact {
    int		    inpTerm;
    int		    inpLower;
    lsv_t	    inpCat;
    lsv_t	    inpInfl;
    int		    outTerm;
    int		    outLower;
    lsv_t	    outCat;
    lsv_t	    outInfl;
}   LsvImFact;

/*  structure for a DM fact
*/
typedef struct _lsvDmFact {
    int		    inpTerm;
    int		    inpLower;
    lsv_t	    inpCat;
    int		    outTerm;
    int		    outLower;
    lsv_t	    outCat;
}   LsvDmFact;

/*  structure for a SYN fact
*/
typedef struct _lsvSynFact {
    int		    inpTerm;
    int		    inpLower;
    lsv_t	    inpCat;
    int		    outTerm;
    int		    outLower;
    lsv_t	    outCat;
}   LsvSynFact;

/*  structure for a ACR fact
*/
typedef struct _lsvAcrFact {
    int		    inpTerm;
    int		    inpLower;
    int		    outTerm;
    int		    outLower;
    lsv_t	    cat;
#define LSV_ACRONYM		((lsv_t)(0x1 << 0))
#define LSV_ABBREVIATION	((lsv_t)(0x1 << 1))
#define LSV_ACRONYM_OF		((lsv_t)(0x1 << 3))
#define LSV_ABBREVIATION_OF	((lsv_t)(0x1 << 4))
#define LSV_TYPES_ALL		(LSV_ACRONYM|LSV_ABBREVIATION|LSV_ACRONYM_OF|LSV_ABBREVIATION_OF)
    lsv_t	    type;
}   LsvAcrFact;

/*  structure for exceptions
*/
typedef struct _xpn {
    int	    input;
    int	    output;
}   Xpn;

/*  structure for trie nodes
*/
typedef struct _lsvTrie {
    int	    child;
    int	    sibling;
    int	    key;		/* node content */
    int	    num;		/* number of associated rules */
    int	    rule;		/* offset where rules start */
}   LsvTrie;

/*  structure for storing all the rules in a trie node
*/
typedef struct _trieRules {
    int	    num;		/* number of rules */
    int	    *array;		/* stores all rules */
}   TrieRules;

/*  header struct for inflectional morphology file
*/
typedef struct _lsvImHeader {
    lsv_t	    type;
    int		    n_trie;
    int		    n_rules;
    int		    n_xpns;
    int		    n_facts;
    int		    n_chars;
}   LsvImHeader;

/*  header struct for derivational morphology file
*/
typedef struct _lsvDmHeader {
    lsv_t	    type;
    int		    n_trie;
    int		    n_rules;
    int		    n_xpns;
    int		    n_facts;
    int		    n_chars;
}   LsvDmHeader;

/*  header struct for synonym file
*/
typedef struct _lsvSynHeader {
    lsv_t	    type;
    int		    n_facts;
    int		    n_chars;
}   LsvSynHeader;

/*  header struct for acronym file
*/
typedef struct _lsvAcrHeader {
    lsv_t	    type;
    int		    n_facts;
    int		    n_chars;
}   LsvAcrHeader;

/*  structure to store matches
*/
typedef struct _lsvMatch {
#define LSV_RULE    0
#define LSV_FACT    1
    int		type;
    int		num;
    char	table[26];
    int         db;
    char        *fact;
}   LsvMatch;

typedef struct _lsvMatches {
  int              a_match;
  int              n_match;
  LsvMatch         *match;
}   LsvMatches;

/*  Memory structure for IM
*/
typedef struct _imMem {
    int		numTrie;
    int		numRules;
    int		numXpn;
    int		numFacts;
    int		numChar;
    LsvTrie	*trie;
    LsvImRule	*rules;
    Xpn		*xpn;
    LsvImFact	*facts;
    char	*buf;
    int		index[28];
    int		n_match;
    int		a_match;
    LsvMatch	*match;
}   ImMem;

/*  Memory structure for DM
*/
typedef struct _dmMem {
    int		numTrie;
    int		numRules;
    int		numXpn;
    int		numFacts;
    int		numChar;
    LsvTrie	*trie;
    LsvDmRule	*rules;
    Xpn		*xpn;
    LsvDmFact	*facts;
    char	*buf;
    int		index[28];
    int		n_match;
    int		a_match;
    LsvMatch	*match;
}   DmMem;

/*  Memory structure for SYN
*/
typedef struct _synMem {
    int		numFacts;
    int		numChar;
    LsvSynFact	*facts;
    char	*buf;
    int		index[28];
    int		n_match;
    int		a_match;
    LsvMatch	*match;
}   SynMem;

/*  Memory structure for ACR
*/
typedef struct _acrMem {
    int		numFacts;
    int		numChar;
    LsvAcrFact	*facts;
    char	*buf;
    int		index[28];
    int		n_match;
    int		a_match;
    LsvMatch	*match;
}   AcrMem;

/*  input structure
*/
typedef struct _lsvInp {
    char	*term;
                                /*    char	*file; ---< now obsolete */
    int		moduleType;
    lsv_t	inpCats;	/* categories of term */
    lsv_t	inpInfls;	/* inflections of term */
    lsv_t	outCats;	/* categories of output desired */
    lsv_t	outInfls;	/* inflections of output desired */
    lsv_t	inpTypes;	/* acronym/abbreviation types. */
    lsv_t	outTypes;	/* acronym/abbreviation types. */
    int		debug;		/* if >0, then matching fact or rule is copied to output */
                                /* if = 2, then print out euis as well.                  */
    int         restrictions;   /* Restrict generation to either rules or facts          */
}   LsvInp;

#define LSV_RESTRICTED                 1    /* EQUIV to RESTRICTED in lvg.h       */
#define LSV_GENERATE_MIXED_RESTRICTED  2    /* EQUIV to MIXED_RESTRICTED in lvg.h */ 
#define LSV_GENERATE_ALL               3    /* EQUIV to UNRESTRICTED in lvg.h     */ 
#define LSV_GENERATE_RULES             4    /* NOT EQUIV to anything in lvg.h     */ 
#define LSV_GENERATE_FACTS             5    /* NOT EQIV to anything in lvg.h      */


/*  output structure for IM
*/
typedef struct _lsvImOut {
    char	*var;
    lsv_t	cat;
    lsv_t	infl;
    char	*info;		/* matching rule/fact as string */
}   LsvImOut;

/*  output structure for DM
*/
typedef struct _lsvDmOut {
    char	*var;
    lsv_t	cat;
    char	*info;		/* matching rule/fact as string */
}   LsvDmOut;

/*  output structure for SYN
*/
typedef struct _lsvSynOut {
    char	*var;
    lsv_t	cat;
    char	*info;		/* matching rule/fact as string */
}   LsvSynOut;

/*  output structure for ACR
*/
typedef struct _lsvAcrOut {
    char	*var;
    lsv_t	cat;
    lsv_t	type;		/* acronym of abbreviation */
    char	*info;		/* matching rule/fact as string */
}   LsvAcrOut;

/*  generic output structure
*/
typedef struct _lsvOut {
    lsv_t	moduleType;	/* IM, DM, SYN or ACR */
    lsv_t	rfType;		/* rule or fact */
    union {
	LsvImOut    im;
	LsvDmOut    dm;
	LsvSynOut   syn;
	LsvAcrOut   acr;
    }	out;
}   LsvOut;

#define LSVOUTIM(i)	(((lsvOut+(i))->out).im)
#define LSVOUTDM(i)	(((lsvOut+(i))->out).dm)
#define LSVOUTSYN(i)	(((lsvOut+(i))->out).syn)
#define LSVOUTACR(i)	(((lsvOut+(i))->out).acr)

#endif
#include "lsv_p.h"

#ifdef NT
#define strcasecmp(x,y) stricmp(x,y)
#endif
