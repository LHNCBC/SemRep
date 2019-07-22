#ifndef PostBrill_FN_H
#define PostBrill_FN_H
#include <iostream>
#include <fstream>
#include <Btree.h>
#include <Word.h>
#include <Thes.h>
#include <runn.h>
#include <vector>
#include <string>
#include <Hash.h>
#include <Blist.h>
#include <pcre.h>
#include "LinClass.h"

#define XSTR 100000

using namespace std;                                                              
namespace iret {

  class PostBrill_FN {
   
  public:
    PostBrill_FN(); 
    PostBrill_FN(const char*);
   
    ~PostBrill_FN();
    
    char* lc(char*);
    void free_vec_str(vector<char*> &vc); //Free the strings
       //pointed to by vc elements and call vc.clear().

    // Allows to change the name string for class.
    void change_name(char*); 

    // Fill data objects
    void gopen_PostBrill_FN(void);
    // Added to prevent memory leak -- Halil
    void gclose_PostBrill_FN(void);

    // Read input string and create word and tag vectors -- Halil
    string filter_str_sub(char*);
    string filter_string_sub(char*);

    // Apply rules to word and tag vectors -- Halil
    string apply_filter_sub(void);

    // Rule sets -- Halil
    string apply_rules_sub(void);
    string get_contextgenes_sub(void);

    // Read input string and create word and tag vectors
    void filter_string(char*);

    // Read each line, create word and tag vectors for each line
    void filter_file(ifstream&);
    void filter_file(long m,long n,ifstream&);

    // Apply rules to word and tag vectors
    void apply_filter(void);

    // Set first/last/next-to-last words/tags
    void set_special_words_and_tags(void);

    // Get the current input string
    char* get_str(void);

    // Clear current word/tags vectors and special words/tags
    void clear_all(void);

    // Added to prevent memory leak -- Halil
    void free_vec_pcre(vector<pcre*> &vc);
    void free_vec_pcre_extra(vector<pcre_extra*> &vc);

    // Rule sets
    void apply_rules(void);
    void get_newgenes(void);
    void get_multigenes(void);
    void get_contextgenes(void);
    void get_knowngenes(void);
    float get_score(char*);
    float get_big_score(char*);
    Count*  set_substrings(char*);

    // data objects 
    
    Hash Gen;
    Hash Aa;
    Hash Genes;                      /* known single word gene names
                                        from 50K + UMLS */
    Hash Ren; 
    Hash Ce;
    Hash Org;
    Hash Bad_After_Num;
    Hash Bani;
    Hash J;
    Hash Lj;
    Hash Freq;
    Hash Gin;
    Hash Lt;
    Hash Stop;
    Hash Gaft;
    Hash Gbef;
    Hash GoodTags; 
    Hash Mg;                         // score >= 10 on merged.genes
    Lexicon* plex; 
    Partial_match PM;                /* known multiple word gene names
                                        from 50K + UMLS */
    List Ten_PM;                     // merged.genes 10+ scores
    LinClass LC;
    Word* Wrd;
    Word* Wsw;

    BTList NotGene;                  // pmid, list of nongenes in abstract
    BTList NewContextGene;           // pmid, list genes in abstract
    List HasGene;
    
    
    // for processing 
    char* fname;                      // input filename
    char *wfirstw, *wlastw, *wnlastw; // first, last, next-to-last words/tags
    char *tfirstw, *tlastw, *tnlastw; // of current string
    char *lfirstw, *llastw, *lnlastw; // lc words
    vector<char*> Words;              // input words
    vector<long>  Tags;               // assigned tags to Words
    vector<char*> Ids;                // pubmed id numbers
    vector<char*> Before;
    vector<char*> After;
    vector<char*> lcwords;
    char* name;                       // holds the string of name information 
                                      // given at time of construction.
    char instr[XSTR];                 // input string with tags
    char no_tagstr[XSTR];             // input string with tags removed
    char tagstr[XSTR];                // list of tags as strings
    char last_no_tagstr[XSTR];        // last input string with tags removed
    char last_tagstr[XSTR];           // last list of tags as strings
  
    vector<pcre*> Re;                 // internal rep of regular expressions
    vector<pcre_extra*> Extra;        // after pcre_study
    vector<char*> Rules;              // rule triggered by each phrase
  };
}

#endif











































