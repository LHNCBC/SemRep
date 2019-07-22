
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
   dm.h - header file for derivational morphology module.
*/

typedef unsigned int dm_t;

/* macros for syntactic category (for external use and internal indexing) */
#define DM_CAT_ADJ		((dm_t)(0x1 << 0))
#define DM_CAT_ADV		((dm_t)(0x1 << 1))
#define DM_CAT_AUX		((dm_t)(0x1 << 2))
#define DM_CAT_COMPL		((dm_t)(0x1 << 3))
#define DM_CAT_CONJ		((dm_t)(0x1 << 4))
#define DM_CAT_DET		((dm_t)(0x1 << 5))
#define DM_CAT_MODAL		((dm_t)(0x1 << 6))
#define DM_CAT_NOUN		((dm_t)(0x1 << 7))
#define DM_CAT_PREP		((dm_t)(0x1 << 8))
#define DM_CAT_PRON		((dm_t)(0x1 << 9))
#define DM_CAT_VERB		((dm_t)(0x1 << 10))
#define DM_CAT_ALL		(DM_CAT_ADJ|DM_CAT_ADV|DM_CAT_AUX|\
				    DM_CAT_COMPL|DM_CAT_CONJ|DM_CAT_DET|\
				    DM_CAT_MODAL|DM_CAT_NOUN|DM_CAT_PREP|\
				    DM_CAT_PRON|DM_CAT_VERB)

/* other macros */
#define DM_SOP_CHAR		'^'
#define DM_EOP_CHAR		'$'
#define DM_COMMENT_CHAR		'#'
#define DM_FIELD_SEPARATOR_CHAR	'|'

#define EOS			'\0'
#define NEWLINE			'\n'

/* locator macros */
#define DM_INPUT_EOP	    (dm_t)(0x1 << 0)  /* input end-of-pattern */
#define DM_INPUT_SOP        (dm_t)(0x1 << 1)  /* input start-of-pattern */
#define DM_OUTPUT_EOP       (dm_t)(0x1 << 2)  /* output end-of-pattern */
#define DM_OUTPUT_SOP       (dm_t)(0x1 << 3)  /* output start-of-pattern */

/* other macros */
#define DM_EXCEPTION_PAIR_TERMINATOR   ';'
#define DM_EXCEPTION_PAIR_SEPARATOR   '|'
#define DM_RULE_MAGIC 0xD75E15
#define DM_FACT_MAGIC (0xD75E15+1)
#define DM_DEFAULT_ALLOCATION 64
#define DM_SMALLEST_STEM    0	/* a stem should be at least this long */

/* structure for the exceptions list */
typedef struct _dmXpn {
    int dmInTerm;               /* offset of input term */
    int dmOutTerm;              /* offset of output term */
} DmXpn;

/* general rule struct */
typedef struct _dmRule {
    int dmInSuf;                /* input suffix */
    dm_t dmInCat;               /* input category */
    int dmOutSuf;               /* output suffix */
    dm_t dmOutCat;              /* output category */
    dm_t dmFlags;               /* for anchors */
    int dmNumXpn;               /* number of exceptions for this rule */
    int dmXpn;                  /* offset into array of exceptions */
} DmRule;

/* Trie */
typedef struct _dmTrie {
    int dmData;                 /* data at node */
    int dmChild;                /* pointer to child */
    int dmSib;                  /* pointer to sibling */
    int dmNumRule;              /* number of rules */
    int dmRule;                 /* where the rules start */
    int *dmHide;                /* should be a hidden member */
} DmTrie;

/* structure for facts */
typedef struct _dmFact {
    int dmInTerm;
    dm_t dmInCat;
    int dmOutTerm;
    dm_t dmOutCat;
} DmFact;

/* header for translated output file */
typedef struct _dmRuleHeader {
    int dmMagic;                /* magic number */
    int dmTrie;                 /* number of trie nodes */
    int dmRule;                 /* number of rules */
    int dmXpn;                  /* number of exceptions */
    int dmChar;                 /* size of string buffer */
} DmRuleHeader;

/* header for translated facts file */
typedef struct _dmFactHeader {
    int dmMagic;                /* magic number */
    int dmFact;                 /* number of trie nodes */
    int dmChar;                 /* size of string buffer */
} DmFactHeader;

/* debugging rule struct */
typedef struct _dmDRule {
    char *dmInSuf;              /* input suffix */
    dm_t dmInCat;               /* input category */
    char *dmOutSuf;             /* output suffix */
    dm_t dmOutCat;              /* output category */
} DmDRule;

/* debugging fact struct */
typedef struct _dmDFact {
    char *dmInTerm;
    dm_t dmInCat;
    char *dmOutTerm;
    dm_t dmOutCat;
} DmDFact;

				/* before a rule can apply */
/* output structure returned by the derivational module */
typedef struct _dmStruct {
    dm_t dmCat;                 /* category of generated inflection */
    char *dmVar;                /* actual variant */
    int dmWt;                   /* weight of match (number of chars matched in the input) */
    DmDRule *dmRule;            /* used for debugging */
    DmDFact *dmFact;            /* used for debugging */
    dm_t dmHide;                /* hidden member */
} DmStruct;
