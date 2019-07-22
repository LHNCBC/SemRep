#ifndef PostBrill_FP_H
#define PostBrill_FP_H

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

#define MAX_STR 10000

using namespace std;                                                              
namespace iret {

  class PostBrill_FP {
   
  public:
    PostBrill_FP(); 
    PostBrill_FP(const char*);
   
    ~PostBrill_FP();

    //Free the strings
    //pointed to by vc elements and call vc.clear().
    void free_vec_str(vector<char*> &vc); 
    
    // Allows to change the name string for class.
    void change_name(char*); 

    // Fill data objects
    void gopen_PostBrill_FP(void);

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

    // Clear current word/tags vectors and special words/tags
    void clear_all(void);

    // Rule sets
    void apply_rules(void);

    // Added by Halil
    string filter_str_sub(char*);
    string filter_string_sub(char*);
    string apply_filter_sub(void);
    string apply_rules_sub(void);
    void gclose_PostBrill_FP(void);
    void free_vec_pcre(vector<pcre*> &vc);
    void free_vec_pcre_extra(vector<pcre_extra*> &vc);

    // data objects   
    Hash Gen;
    Hash Aa;
    Hash Genes;
    Hash Ren;
    Hash Ce;
    Hash Org;
    Hash Freq;
    Hash Bad_After_Num;
    Hash GoodTags;
    Lexicon* plex;  

    // for processing 
   
    char *wfirstw, *wlastw, *wnlastw; // first, last, next-to-last words/tags
    char *tfirstw, *tlastw, *tnlastw; // of current string
    vector<char*> Words;              // input words
    vector<long>  Tags;               // assigned tags to Words
    char* name;                       // holds the string of name information 
                                      // given at time of construction.
    char instr[MAX_STR];              // input string with tags
    char no_tagstr[MAX_STR];          // input string with tags removed
    char tagstr[MAX_STR];             // list of tags as strings
    char last_no_tagstr[MAX_STR];     // last input string with tags removed
    char last_tagstr[MAX_STR];        // last list of tags as strings
  
    vector<pcre*> Re;                 // internal rep of regular expressions
    vector<pcre_extra*> Extra;        // after pcre_study
    vector<char*> Rules;              // rule triggered by each phrase
  };
}

#endif











































