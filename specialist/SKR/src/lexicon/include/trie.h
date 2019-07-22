/*  trie.h - header file for trie management

*/


/* 
   prevent sccs_id_trie_h from being re-defined in case
   trie.h is loaded multiple times in same compilation 
*/

#ifndef sccs_id_trie_h
static char sccs_id_trie_h[] = "@(#)trie.h	1.3 09/27/06";
#define sccs_id_trie_h 1
#endif

#ifndef _TRIE_H
#define _TRIE_H

#include <stdio.h>

typedef struct _trieNode {
    char key;
#define USER_NODE 0
#define INTERNAL_NODE 1
    int type;			/* internal or user */
    struct _trieNode *sibling;
    struct _trieNode *child;
} TrieNode;

typedef struct _trie {
    int numNodes;		/* number of nodes in trie */
    int nodeSize;		/* size of a trie node (user area) */
    TrieNode *root;		/* pointer to the root node */
} Trie;

#define NODETYPE(node)	(((TrieNode *)(((TrieNode *)(node))-1))->type)

/* function prototypes */
Trie *create_trie();		/* creates and initializes a trie */
void free_trie();		/* destroys a trie */
void *create_trie_node();	/* creates a new trie node */
int add_trie_node();		/* adds new node to trie */
void *find_trie_node();	/* finds a node */

#endif
