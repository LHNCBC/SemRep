#ifndef REPEATS_H
#define REPEATS_H

#include <Word.h>

namespace iret {

class Repeats {
public:
   Repeats();

   ~Repeats();

   void add_phrase(char *str,long n = 1); //Adds the phrase
      //Uses all start points to add. n is number of times to count
      //Assumes words of a phrase are separated by single spaces.

   void process2(List *lst); //Processes the input data to find duplicate strings
      //May be more efficient than process for the N=2 case

   void process(List *lst,int N = 2); //Processes the input data to find strings
      // that appear at least N times

   //Data
   Count *pct; // phrases and their counts
};

class Inflect {
public:
   Inflect(Word *pW,long phr_spc); //phrase_space is an upper bound on the 
      //number of phrases expected in a single document or round of processing.

   ~Inflect();

   void add_text(long len,char *text); //Can call this function repeatedly to add strings
      //of text to be included and compared together to find repeats of stemmed text

   void proc_write(ofstream &fout); //Processes the collection of text and writes out
      //inflectional variants found as pairs. This function frees all space used 
      //by add_text and class is ready for another round of processing.

   long phrase_space; //Upper bound on the # of phrases found
   long num; //Number of phrases found. 
   char **stm; //Pointer at the stemmed versions of phrases
   char **reg; //Pointer at the unstemmed verions
   Word *pWrd;
};

} // namespace iret

#endif 
