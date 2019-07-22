#ifndef CLIP_H
#define CLIP_H
#include <fstream>
#include <iostream>
#include <runn.h>
#include <Btree.h>
#include <Hash.h>

using namespace std;
namespace iret {

class Clip : public Count {
   public:
      Clip(long nm=10000); //nm size of longest string to handle
      ~Clip(void);

      void set1(void); //Setup for proc0-3
      void prc0(const char *str); //Produces the strings having alpha or
         //digit and demarcated by other. Other is not included.
      void prc1(const char *str); //Produces the strings demarcated
         //by changes between alpha, digit, and other.
         //The other is always left out of the strings produced.
      void prc2(const char *str,Hash &Hs); //Produces the strings demarcated
         //by changes between alpha, digit, and other and found in Hs.
      void prc2(const char *str,Chash &Cs,long llim); //Produces the strings demarcated
         //by changes between alpha, digit, and other. Only adds strings found
         //in Cs with count less than llim.
      void prc3(const char *str,Hash &Hs); //Removes all chars that are not
         //alpha or digit. From resulting single string adds all contiguous
         //substrings found in Hs.
      void prc3(const char *str,Chash &Cs,long llim); //Removes all chars that are not
         //alpha or digit. From resulting single string adds all contiguous
         //substrings found in Cs and with count less then  llim.

      void fill_grm(const char *txt,int ngram,int aug,Word &Wrd);
           //Fills a Count with ngrams made from the words in the array txt
           //of length ln. Links consecutive words by ngrams of the form "a_bcd"
           //where the a is the first letter of a word and the bcd is the initial
           //part of the next word and total length is ngram. Augments a marked
           //form the first trigram of each word  by entering it aug times and
           //also marks the first letter of each word and adds it in once.
           //Uses the object Wrd of class Word to produce words from which grams
           //made.
      void fill_brm(const char *txt,int lgram,Word &Wrd);
           //Fills a Count with 2-lgram ngrams. Takes the ngrams as
           //composed from consecutive letters in the whole string. All
           //ngrams are added in only once.
      void fill_scn(const char *txt,int lgram,Word &Wrd);
           //Fills a Count with lgrams. Takes the lgrams as
           //composed from consecutive chars in the words of the string. All
           //lgrams are added in only once.


   //Data
      long num; //Size of longest string to handle
      char zt[128];
      char *pck;
      long *nck;
};
}
#endif
