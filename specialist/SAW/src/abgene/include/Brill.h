#ifndef BRILL_H
#define BRILL_H

#include <iostream>
#include <fstream>
#include <Btree.h>
#include <Word.h>
#include <Thes.h>
#include <runn.h>
#include <vector>
#include <string>
#include <Hash.h>


#define XSTR 100000

using namespace std;                                                              
namespace iret {

  class Brill {
   
  public:
    Brill(); 
    Brill(const char*);
   
    ~Brill();

    //Free the strings
    //pointed to by vc elements and call vc.clear().
    void free_vec_str(vector<char*> &vc); 

    // Allows to change the name string for class.
    void change_name(char*); 

    // Fill data objects
    void open_tagger(void);

    // Overloaded function to read input and tokenize it
    void read_and_tok(char*);
    void read_and_tok(ifstream&);

    // added by Halil
    void close_tagger(void);
    void read_and_tok_sub(char*);
    string get_results(void);

    // taggers
    void start_state_tagger(void);
    void final_state_tagger(void);
    
    // Get the input string
    char* get_str(void);


    // data objects   
    Hash Lw;                  // wordlist, eventually made from all MEDLINE
    RelateA Rb;
    RelateA Lb;
    Lexicon* plex;            // LEXICON
    vector<char*> Lrv;        // lexical rules
    vector<char*> Crv;        // contextual rules
    Count CrvCount;           // assign each contextual rule type an int
    vector<long> Old; 
    vector<long> Next2t;
    vector<long> Newt; 
    vector<long> Prev1t;
    vector<long> Lft; 
    vector<long> Prev2t;
    vector<long> Rght; 
    vector<long> Next1t;

    vector<char*> Next2;
    vector<char*> Prev1;
    vector<char*> Prev2;
    vector<char*> Next1;  
    vector<char*> When; 
    vector<char*> TheWord;
    vector<long> Tag;

    Count LrvCount;           // assign each lexical rule type an int
    vector<char*> LWhen;
    vector<long> LOld;
    vector<long> LNewt;
    vector<int> LLen;
    vector<char*> LWord;      // char, prefix or suffix 
    Word* Wrd;

    // for processing 
   
    vector<char*> Terms;      // tokenized string to tag
    vector<long> Tags;        // assigned tags to Terms
    char* name;               // holds the string of name information 
                              // given at time of construction.
    char* tagstr;             // string to tag
    // added to prevent memory leak -- Halil
    char* cptr;
    char nid[7];
  };
}

#endif











































