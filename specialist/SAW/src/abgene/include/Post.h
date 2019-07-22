#ifndef POST_H
#define POST_H

#include <iostream>
#include <fstream>
#include "Docum.h"
#include "Regist.h"
#include "Bnum.h"
using namespace std;
namespace iret {

typedef struct Pdat { //Used to construct linked lists of postings data.
   long num; //Posting or document number of the term occurrence.
   unsigned char lnt; //Local count of the term in the document held in a character.
   Pdat *pdt; 
};

typedef Pdat *pPdat;

class Post {
   friend class Vnab;
   public:
      Post(); //Creates the Btree.
      Post(const char *nm); //Creates the Btree. 
          //Stores the string *nm in *name for use in reading or writing.
      ~Post();
      void change_name(const char *nm); //Allows to change the name string for class.

      void readp(); //Reads in the data from the postings files.
      void writep(); //Writes out the postings data to the postings files
      void add(char *str,long n,int j); //Adds a posting for a string str
         //From document number n & with local count j.
      void load(Docum &Doc); //Constructs post data from Doc.
      long count(Pdat *pdat); //Counts the number of elements in the linked
         //list and returns. 
      Pdat *data(char *str); //Returns the pointer to the associated data if
         //their are any. Otherwise returns NULL.

         //Standard local count forms.
      void segs_on_disk(Docum &Doc); //process large set
		//of documents by dividing into smaller sets, then merge
      void make_single_seg_first(Regist &Rg);
      void make_single_seg_first(Regist &Rg,long sz_seg); //sz_seg sets
      void make_single_seg_extend(Regist &Rg);
      void make_single_seg_overwrite(Regist &Rg, int seg_num);

       //Makes merged p file on disk.
      void s_disk_merge(long list_size); //merge the segs into single n,d,s files.
           //Uses list_size to limit the number of terms in a tree at a single time.
      void p_disk_merge(void); //merge the segs into single f,a,p files.

       //Accesses the single p file system on disk.
      void gopen_disk_merge(long n); //includes only terms whose frq>=n
      long data_disk_merge(char *str); //Fills the arrays idx and lct if string is
         //found and returns the freq. Otherwise returns zero.
      long data_disk_merge(long n); //Fills the arrays for nth term.

       //Makes sliced p files on disk.
      void s_disk_slice(long list_size); //merge the segs into single n,d,s files.
      void p_disk_slice(long disk_size); //merge the segs into single f,a, but 
           //slice the p file to several slices approximately <=disk_size.
	   //includes the file number and the address in the a file.

       //Access the sliced p files on disk.
      void gopen_disk_slice(long n); //includes only terms whose frq>=n
      long data_disk_slice(char *str); //Fills the arrays idx and lct if string is
         //found and returns the freq. Otherwise returns zero.
      long data_disk_slice(long n); //Fills the arrays for nth term. 
   
       //Access the sliced p files on disk. Smaller memory required.
      void gopen_small_slice(long n); //includes only terms whose frq>=n
      long data_small_slice(char *str); //Fills the arrays idx and lct if string is
         //found and returns the freq. Otherwise returns zero.
         //Note that data_disk_slice(n) works also here.

       //Access the sliced p files on disk. By index of term only.
      void gopen_index(void); //Opens the arrays for index access to postings.
         //Note that data_disk_slice(n) works here.

      long ndoc; //Number of documents in set from which postings were taken
      long nwrd; //Number of strings for which there are postings.
      Btree *btr; //Holds the postings data.

      //Used by disk systems.
      long swrd;	//selected subset of terms
      long *freq; //Frequency of each term selected. 
      long *addr; //Address of each term selected.
      long cfln; //Number of files/slices.
      Count *ctt; //Holds the terms that have frequency greater then cutoff
      Blist *lst; //Holds the terms with freq>cutoff (like ctt).
         //Smaller memory requirements than ctt.
      Bnum *pBn; //Binary number search for file number of term.
      long *idx; //Holds the document id number for a term occurrence.
      unsigned char *lct; //Holds the local counts for a term occurrence.
      ifstream *pfln; //Holds the slice file stream objects.

   protected:
      char *name; //Holds the string of name information given at time of 
         //construction.
                 
   private:
      void tread(ifstream &fin,long n,pPdat &dat); //Reads in n data elements
         //and attaches them to the dat pointer.
      long twrite(ofstream &fout,Pdat *dat); //Writes out the data elements
         //associated with dat and returns the number so written.

      void seg_load(Docum &Doc,long start,long end,long start_load); //Constructs
         //limited post files from Doc.
      void seg_load(Regist &Reg,long start_load, long end); //Constructs
         //limited post files from Reg.
};
}
#endif
