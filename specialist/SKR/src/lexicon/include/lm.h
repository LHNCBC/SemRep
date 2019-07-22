
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

/* lm.h - header file for inflectional morphology module
    as used by the SPECIALIST lexicon.

*/

typedef unsigned int lm_t;

/* macros for syntactic category (for external use and internal indexing) */
#define LM_CAT_ADJ		((lm_t)(0x1 << 0))
#define LM_CAT_NOUN		((lm_t)(0x1 << 1))
#define LM_CAT_VERB		((lm_t)(0x1 << 2))
#define LM_CAT_ALL		(LM_CAT_ADJ|LM_CAT_NOUN|LM_CAT_VERB)

/* macros for inflection (first 4 are aliases) */
#define LM_INFL_BASE            ((lm_t)(0x1 << 0))      /* base form */
#define LM_INFL_POSITIVE        ((lm_t)(0x1 << 0))      /* positive of adj */
#define LM_INFL_SINGULAR        ((lm_t)(0x1 << 0))      /* singular of nouns */
#define LM_INFL_INFINITIVE      ((lm_t)(0x1 << 0))      /* infinitives of verbs */
#define LM_INFL_COMPARATIVE     ((lm_t)(0x1 << 1))      /* comparative of adj */
#define LM_INFL_SUPERLATIVE     ((lm_t)(0x1 << 2))      /* superlative of adj */
#define LM_INFL_PLURAL          ((lm_t)(0x1 << 3))      /* plural of nouns */
#define LM_INFL_PRESENT         ((lm_t)(0x1 << 4))      /* present singular (3rd person) of verbs */
#define LM_INFL_ING             ((lm_t)(0x1 << 5))      /* present participle of verbs */
#define LM_INFL_PAST            ((lm_t)(0x1 << 6))      /* simple past tense of verbs */
#define LM_INFL_PASTPART        ((lm_t)(0x1 << 7))      /* past participle of verbs */
#define LM_INFL_ALL             (LM_INFL_BASE|LM_INFL_COMPARATIVE|LM_INFL_SUPERLATIVE|\
				 LM_INFL_PLURAL|LM_INFL_PRESENT|LM_INFL_ING|LM_INFL_PAST|LM_INFL_PASTPART)

/* macros for rules */
#define LM_RULE_REG		((lm_t)(0x1 << 0))
#define LM_RULE_REGD		((lm_t)(0x1 << 1))
#define LM_RULE_GLREG		((lm_t)(0x1 << 2))
#define LM_RULE_ALL		(LM_RULE_REG|LM_RULE_REGD|LM_RULE_GLREG)

/* other macros */
#define LM_SOP_CHAR		'^'
#define LM_EOP_CHAR		'$'
#define LM_COMMENT_CHAR		'#'
#define LM_FIELD_SEPARATOR_CHAR	'|'

#define EOS			'\0'
#define NEWLINE			'\n'

/* locator macros */
#define LM_INPUT_EOP	    (lm_t)(0x1 << 0)  /* input end-of-pattern */
#define LM_INPUT_SOP        (lm_t)(0x1 << 1)  /* input start-of-pattern */
#define LM_OUTPUT_EOP       (lm_t)(0x1 << 2)  /* output end-of-pattern */
#define LM_OUTPUT_SOP       (lm_t)(0x1 << 3)  /* output start-of-pattern */

/* other macros */
#define LM_EXCEPTION_PAIR_TERMINATOR   ';'
#define LM_EXCEPTION_PAIR_SEPARATOR   '|'
#define LM_RULE_MAGIC 0x52E9
#define LM_DEFAULT_ALLOCATION 64

/* general rule struct */
typedef struct _lmRule {
    int  lmInSuf;               /* input suffix */
    lm_t lmInCat;               /* input category */
    lm_t lmInInfl;              /* input inflection */
    lm_t lmRule;                /* SPECIALIST rule code */
    int  lmOutSuf;              /* output suffix */
    lm_t lmOutCat;              /* output category */
    lm_t lmOutInfl;             /* output inflection */
    lm_t lmFlags;               /* for anchors */
} LmRule;

/* Trie */
typedef struct _lmTrie {
    int lmData;                 /* data at node */
    int lmChild;                /* pointer to child */
    int lmSib;                  /* pointer to sibling */
    int lmNumRule;              /* number of rules */
    int lmRule;                 /* where the rules start */
    int *lmHide;                /* should be a hidden member */
} LmTrie;

/* header for translated rule file */
typedef struct _lmRuleHeader {
    int lmMagic;                /* magic number */
    int lmTrie;                 /* number of trie nodes */
    int lmRule;                 /* number of rules */
    int lmChar;                 /* size of string buffer */
} LmRuleHeader;

typedef struct _lmDRule {
    char *lmInSuf;              /* input suffix */
    lm_t lmInCat;               /* input category */
    lm_t lmInInfl;              /* input inflection */
    lm_t lmRule;
    char *lmOutSuf;             /* output suffix */
    lm_t lmOutCat;              /* output category */
    lm_t lmOutInfl;             /* output inflection */
} LmDRule;

typedef struct _lmStruct {
    lm_t lmCat;                 /* category of generated inflection */
    lm_t lmRule;
    lm_t lmInfl;                /* inflection code */
    char *lmVar;                /* actual variant */
    int lmWt;                   /* weight of match (number of chars matched in the input) */
    LmDRule *lmDRule;           /* for debugging */
    lm_t lmHide;                /* hidden member */
} LmStruct;

