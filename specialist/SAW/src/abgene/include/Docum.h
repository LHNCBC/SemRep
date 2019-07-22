#ifndef DOCUM_H
#define DOCUM_H

#include <iostream>
#include <fstream>
#include "Btree.h"
#include "FBase.h"
#include "Word.h"

#define READ_W 1 //Sets up reading for w and reads in the .a file of addresses.
#define READ_U 2 //Reads in the .u file.
#define READ_D 4 //Reads in the .d file.
#define READ_M 8 //Reads in the .m file.
#define READ_S 16 //Reads in the .s file.
#define WRITE_U 1 //Defines U array and writes it out on gclose_write().
#define WRITE_D 2 //Defines D array and writes it out on gclose_write().
#define WRITE_S 4 //Defines S array and writes it out on gclose_write().

using namespace std;
namespace iret {

class Post; //forward declaration

class Docum : public FBase {
   friend class Post;
   friend class Regist;
   friend class Rate;

   public:
      Docum(); //Creates the space for a document to be stored.
      Docum(const char *nm); //Creates the space for a document to be stored. 
           //Stores the string *nm in *name for use in reading or writing.
      Docum(const char *nm,long wrd_spc); //Creates the space for a document to 
           //be stored. Name stored. wrd_spc is word_space. 
      ~Docum();

   //Reading functions.
      void gopen_read(int rfil); //Reads data and sets up w file reading depending
           //on which bits are set in rfil. Sets gct=-1.
      void gopen_map(int rfil); //Mmaps data and sets up w file reading depending
           //on which bits are set in rfil. Sets gct=-1.
      void read(); //Fills a document. To be used only after a call to 
           //gopen_read(). Reads at the current file pointer position.
           //Increments gct by 1.
      void read(long n); //Fills a document. To be used only after a
           //call to gopen_read(). Reads in document numbered n (starts at 0). 
           //Sets gct=n.
      void gclose_read(int rfil); //closes files and frees memory depending on 
           //bits set in rfil. 
      void gclose_map(int rfil); //munmaps files depending on 
           //bits set in rfil. 

   //Writing functions.
      void gopen_write(long n,int rfil); //Sets a limit on the number of documents 
           //to be written of n. Sets up for U and D arrays and writes out iff
           //bits are set for these in rfil.
      void gopen_append(long n,int rfil);
      void write(); //Writes out a document into the "w" file and records the
           //relevant data in associated arrays. Increments the global counter.
      void gclose_write(); //Writes out document number and associated data arrays.
           //Closes "w" file and deletes associated data arrays.

   //Filling functions.
      void fill_grm(const char *txt,long ln,int ngram,int aug,Word &Wrd);
           //Fills a document with ngrams made from the words in the array txt
           //of length ln. Links consecutive words by ngrams of the form "a_bcd"
           //where the a is the first letter of a word and the bcd is the initial
           //part of the next word and total length is ngram. Augments a marked
           //form the first trigram of each word  by entering it aug times and 
           //also marks the first letter of each word and adds it in once. 
           //Uses the object Wrd of class Word to produce words from which grams 
           //made.
      void fill_brm(const char *txt,long ln,int lgram,Word &Wrd);
           //Fills a document with 2-lgram ngrams. Takes the ngrams as 
           //composed from consecutive letters in the whole string. All
           //ngrams are added in only once.
      void fill_scn(const char *txt,long ln,int lgram,Word &Wrd);
           //Fills a document with lgrams. Takes the lgrams as 
           //composed from consecutive chars in the words of the string. All
           //lgrams are added in only once. 
      void open(); //Creates the Btree btr and enters the document data into it. 
      void add(char *str,int n); //Adds the string into the data and adds its 
           //count n also. During this addition the Btree btr is used.
      void close(); //Transfers the data into the proper document structures 
           //and deletes *btr. Close means close for additions to the document. 
           //It can always be opened again.
      void clear(); //Clears the data from a document so that it can be used to 
           //build a new document. Simply sets sizes to zero.
      void copy(Docum *pDoc); //Assumes a document is in memory in pDoc and copies
           //to fill the local document. Assumes the class is open for writing.
           //At the same time clears the local document memory in pDoc.
      void copy(Count &Ct); //Assumes that the current document has been cleared
           //Requires no open or close calls. Creates a closed document ready to 
           //process.

   //Access functions.
      void reset(){ct=0;}; //A call to the show function advances the 
            //counter in the document. This function resets.
      inline char *show(int &j){ //Gives the address to a term and its 
                 //local frequency is placed in j.
         if(ct<nw){              
            j=lcnt[ct];
            ct++;
            return(word[ct-1]);
         }
         else {
            j=0;
            return(NULL);
         }
      }
      int sum_lcnt(){return len;}; //Returns the sum of local count 
               //for terms in the document.
      int num_wrds(){return nw;};  //Returns the number of words in 
               //the document.
      long glb_cnt(){return ndoc;}  //Returns the global count of documents;

   //Debugging functions.
      void debug(void);
           //Like write except writes to cout.

   //Global data.
      Btree *btr; //Used with the open, add, and close functions.
      long ndoc; //Number of documents in set.
      long *auid; //Array for uid number of each document. This file is read in 
         //if it exists. Otherwise the point is set to NULL. Likewise if the first
         //element is negative no file is written.
      long *date; //Array for date number of each document. This file is read in 
         //if it exists. Otherwise the pointer is set to NULL. Likewise if the
         //first element is negative then no file is written.

   //private:
   //Global data.
      long *addr; //Array for addresses of documents.
      long *alen; //Array for document lengths.
      long *size; //Array for document size (number of terms).

   //Local data.
      int nw; //number of words in document
      int len; //sum of local counts in the document
      char **word; //Holds the words in the document.    
      int *lcnt;//Holds the local terms frequencies in the document.
      int *hld;//Holds the local terms frequencies in the Btree.
      char sword[max_str]; //Holds the individual word read from a file.
      long word_space; //Number of words allowed in a single document.

   //Accessory 
      long gct; //Global document counter used in writing documents.
      int ct; //current word in current document
      fstream *pfwd; //Points at the file stream object for .w file.
};
}
#endif
