#ifndef WORD_H
#define WORD_H

#include <cmath>
#include <iostream>
#include <fstream>
#include "runn.h"
#include "Btree.h"
using namespace std;
namespace iret {

#define LOWERCASE 0
#define ALLCASE  1
#define STOPWORDS 1
#define NOSTOPWORDS 0

class Word {
public:
   Word();//Builds btree for stop words and allocates space for the output of
	  //10k singlets, multiplets or phrases. By default sets byte_lim=65
          //back_lim=0, and stop=1. By default sets zlt[] to map all chars to 0
          //except digits and alphabetic. Lowercases by default.
 
   Word(long wrd_spc); // As previous but sets word_space to
         // wrd_spc. This is the output space.

   Word(long wrd_spc,const char *list_name); // As previous but accesses a stop list
         // that is has a file name "wordset_(*list_name).stop" and a path that must be
         // found in the file "path_wordset".  
	
   ~Word();//Frees memory allocated to zlt_convert[] and list[] 

   //INITIALIZATION 
   void set_map(const char *punct, const char mrkk, int cas);
         //Sets all characters in array punct[] to map to char mark by zlt[].
         //sets zlt to lowercase if cas=0, to be case sensitive if cas=1.
   void restore_map(const char *restore);
         //Sets zlt[c]=c for all char c in array restore[].
   void erase_map(const char *erase);
         //Sets zlt[c]=0 for all char c in array erase[].
   void replace_map(char a,char b); 
         //Sets zlt[a]=b.
   void all_print_map(); 
         //Sets zlt[c]=c for all printable char c.
   void pre_punct(void);
         //Sets special mappings for ".,'".
   int byte_lim;  
         //a string must have a char c, with (int)c>=byte_lim to be a word
         //Set to 0 to allow pure digits, otherwise 65 
   int back_lim;  
         //End of word must have a char c, with (int)c>back_lim.
   int stop;   
         //Set to 1 and stop words act like punctuation marks.
         //Set to 0 and stop words are treated like any other word.
       
   //PREPROCESSING FUNCTIONS.
   void remove_sgml_tags(long n,char *text);
         //Maps tags such as "<...>" to ".  "
         //Here n is length of text string.
   void remove_html_list_commas(long n,char *text);
         //Maps commas to nulls inside a pair <UL> </UL>.
         //Here n is length of text string.
   void punct(long n,char *text);
         //removes any "*#$". Then converts any period that is preceded
         //by two or fewer letters or not followed by a space to an asterisk.
         //converts any comma not followed by a space to a pound sign.
         //Converts any apostrophe that is at beginning of a word to a dollar
         //sign. These special characters are then converted back by convert.
   void convert(const char *text,long len);
         //Applies the zlt[] mapping array to txt to produce zlt_convert
         //array. Here len is length of text string.
   void modify(const char c);
         //Removes any occurrence of c at beginning of a word in internal
         //array zlt_convert. Must follow the convert function.

   //INPUT DATA
   const char *txt;//Pointer at text string input by convert function.
         //This string is not modifiable by the class.
   long  length;//Length of input string pointed at by txt.
    
   //SINGLE WORD EXTRACTION
   long wordf(long &i, long len);	
         //len marks the end of the text string to be processed. It is in
         //the array zlt_convert. i marks the beginning point in the same
         //array. Each time wordf is called it updates i and returns the
         //length of the word it has found. When there are no more words
         //it returns 0. The word found is recorded in the array wrd.
   char wrd[max_str];
         //Array that holds the string produced by wordf().
   int  al;
         //1 if all char in the array wrd[] are
         //above the byte_lim, otherwise 0.

   //EXTRACTION FUNCTIONS (BASED ON SINGLE WORD EXTRACTION FUNCTION)
   void single();
         //Given input text, it produces a list of all single words
   void multiple(int nplet);	
         //Given input text, it produces a list of all contiguous nplets 
         //of words between punctuation marks and stop words. 
   void phrase();	
         //Given input text, it produces a list of all phrases
         //demarcated by punctuation marks and stop words.
   void single_stem();
   void multiple_stem(int nplet);	
   void phrase_stem();	
         //Does exaclty what previous functions do with the addition of
         //stemming each individual word using the Porter algorithm.

   //DATA PRODUCED BY EXTRACTION FUNCTIONS
   char **list;	
         //List of singlets, multiplets or phrases produced
   long cnt;
         //number of terms in the list produced. 

   //FREES MEMORY USED TO STORE DATA PRODUCED BY EXTRACTION 
   void clear_list();

         //Frees the memore devoted to each term stored in list.
   //Porter stemmer function
   int stmprt(char *pch,int len);
         //Word pointed to by pch is of length len and is replaced by 
         //stemmed verions of word and new length is returned.
   //LOW LEVEL ENTITIES USED IN EXTRACTION PROCESS
   void step_zero();	
         //Initializing beg = 0 and end =0 
   long step_next();	
         //Finding the character positions for next pair of punct
         //marks and set beg and end appropriately.
   char *acc_conv(void){return(zlt_convert);}
   protected:
   char **temp_list; //Space for strings in processing.
   long *tmp_length; //Space for length of strings in processing.
   char *zlt_convert;	
         //zlt[] maps txt to zlt_convert under convert function.
   long beg;	
         //Position of beginning mark
   long end;	
         //Position of ending mark
   Btree *btr;	
         //Btree used to hold list of stop words 
   char mrkk;   
         //Marker for punctuation 
   char *zlt;    
         // Used to map ascii character set to desired subset
   long word_space; 
         //Limit on the number of singlets, multiplets or phrases
         //allowed. Bound on list size. 
};

class Mark : public Word {
   public:
      Mark(long max_len, long wrd_spc, char *infile); //max_len is maximum
         //length of input string to be marked up. wrd_spc is maximum number 
         //of terms that can be listed in output. infile is input file of a 
         //lexically ordered list.
         //but allows one to set the word_space size.
      Mark(long max_len, long wrd_spc, char *infile,const char *list_name);
         //as prev, but allows to access a special stop list.

      ~Mark();

      void mark_long(void); //Marks up the text input to the word class.
         //Generally one must have done preprocessing on this using the
         //functions of the word class such as modify and convert.
         //Finds the longest phase at a given starting point and then
         //skips beyond this phrase to the next starting point.
      void mark_all(void); //As previous, but finds all phrases
         //and marks them in the arrays st,nd,ph_cnt.

      long *st; //Beginning position of a marked phrase
      long *nd; //Ending position of a marked phrase
      long *ph_cnt; //Number of the phrase in the list
  
      char *str; //Working space of length length.
      long *pos; //Marks position of origin of chars in str.

      Partial_match *pPart; //Holds the list of phrases and the numbers
         //as data.
};

}
#endif
