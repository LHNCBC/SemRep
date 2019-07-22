#ifndef SEMTAG_H
#define SEMTAG_H

#include <iostream>
#include <fstream>
#include <Btree.h>
#include <Word.h>
#include <Thes.h>
#include <runn.h>
#include <vector>
#include <cstring>
#include <Hash.h>
#include <Strset.h>

#define XSTR 100000

using namespace std;                                                              
namespace iret {

  class SemTag {
   
  public:
    SemTag(); 
    SemTag(const char*);
   
    ~SemTag();

    //Free the strings
    //pointed to by vc elements and call vc.clear().
    void free_vec_str(vector<char*> &vc); 

    // Allows to change the name string for class.
    void change_name(char*); 

    // Fill data objects
    void open_tagger(void);
    void close_tagger(void);

    // Overloaded function to read input and tokenize it
    void read_and_tok(char*);
    void read_and_tok(ifstream&);

    // tagger
    void tagger(void);
    
    // Get the input string
    char* get_str(void);

    // write tagged string
    void write_tagged(void);

    // data objects          
    vector<LexPrb*> LPv;      // Probabilities
    vector<LexPrb*> MLPv;     // Medline Probabilities
    vector<float*> Rv;        // Ratios
    Word* Wrd;
    Hash Stop;

    // for processing 
   
    vector<char*> Terms;      // tokenized string to tag
    vector<char*> Tags;       // assigned tags to Terms
    char* name;               // holds the string of name information 
                              // given at time of construction.
    char* tagstr;             // string to tag
    char nid[7];
  };
}

#endif











































