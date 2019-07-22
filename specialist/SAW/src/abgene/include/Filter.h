#ifndef FILTER_H
#define FILTER_H

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

using namespace std;                                                              
namespace iret {

  class Filter {
   
  public:
    Filter(); 
    Filter(const char*);
   
    ~Filter();

    // Allows to change the name string for class.
    void change_name(char*); 

    // Fill data objects
    void gopen_filter(void);
    void gclose_filter(void);

    // Read input string and create word and tag vectors
    void filter_string(char*);

    // Read each line, create word and tag vectors for each line
    void filter_file(ifstream&);

    // Apply rules to word and tag vectors
    void apply_filter(void);

    // Set first/last/next-to-last words/tags
    void set_special_words_and_tags(void);

    // Get the current input string
    char* get_str(void);

    // Get the current phrase label, good or bad
    int get_flag(void);

    // Clear current word/tags vectors and special words/tags
    void clear_all(void);

    // Rule sets
    int  first_rules (void);
    void ends_in_verb(void);
    void ends_in_adv (void);
    void ends_in_adj (void);
    void last_rules  (void);

    // data objects   
    Hash NotNoun;
    Hash Intran;
    Hash Tran;
    Hash Ditran;
    Hash Bad_Before_Num;
    Hash Bad_After_Num;
    Hash Good_Before_Adj_Adv;
    Hash Stopwords;
    Hash Umls;
    //Hash Good_Suffixes;
    Hash Good_Num_Words;
    Hash Numbers;
    Lexicon* plex;   
    
    // for processing 
   
    char *wfirstw, *wlastw, *wnlastw; // first, last, next-to-last words/tags
    char *tfirstw, *tlastw, *tnlastw; // of current string
    vector<char*> Words;              // input words
    vector<long>  Tags;               // assigned tags to Words
    char* name;                       // holds the string of name information 
                                      // given at time of construction.
    char instr[max_str];              // input string with tags
    char no_tagstr[max_str];          // input string with tags removed
    char tagstr[max_str];             // list of tags as strings
    char last_no_tagstr[max_str];     // last input string with tags removed
    char last_tagstr[max_str];        // last list of tags as strings
    int fflag;                        // 1 if good, 0 if bad phrase
    vector<pcre*> Re;                 // internal rep of regular expressions
    vector<pcre_extra*> Extra;        // after pcre_study
    vector<const char*> Rules;        // rule triggered by each phrase
  };
}

#endif
