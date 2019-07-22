#ifndef POSTG_H
#define POSTG_H

#include <iostream>
#include <fstream>
#include "FBase.h"
#include "DataObj.h"
#include "Btree.h"
using namespace std;
namespace iret {

template <class X>
class Zdat { //Used to construct linked lists of postings data.
   public:
      Zdat(void);
      Zdat(long n, const X &dtx);
      ~Zdat();
      long num; //Posting or document number of the term occurrence.
      X datx; //Local data for the term in the document.
      Zdat *pdt; 
};

template <class X>
class Postg : public FBase {
   public:
      Postg(void); //Creates the Btree. 
      Postg(const char *nm); //Creates the Btree. 
          //Stores the string *nm in *name for use in reading or writing.
      ~Postg();

      void readp(); //Reads in the data from the postings files.
      void readp(long sn); //Reads in the data from the postings files.
         //sn is the number of the particular postings object.
      void writep(); //Writes out the postings data to the postings files
         //Writes address for mapping
      void writep(long sn); //Writes out the postings data to the postings files
         //sn is the number of the particular postings object.
      void add(const char *str,long n,const X &dtx); //Adds a posting for 
         //a string str from document number n & with local data dtx.
      long count(Zdat<X> *pdat); //Counts the number of elements in the linked
         //list and returns. 
      Zdat<X> *data(const char *str); //Returns pointer at linked list if one exists,
         //else returns NULL;
      Zdat<X> *data(void); //To apply if tree pointers already set
      long set_ptr(const char *str); //Returns the number of associated data
         //if their are any. Otherwise returns zero. Sets pointers.
      long set_ptr(void); //Apply if tree pointers already set.
      Zdat<X> *next_ptr(void); //Iterate through the list of data pointers for
         //a particular term. Returns NULL when the traversal is completed.
         //Must have called a version of set_ptr function first.

      //Disk access
      void gopen_map(void); //Set up for disk access. Memory maps.
      void gopen_map(long sn); //Set up for disk access. Memory maps.
         //sn is the number of the particular postings object.
      long set_ptr_map(const char *str); //Construct list and
         //set pointers to access it through next_ptr function.
      long get_idx_map(const char *str); //If string is in list
         //returns index+1, else returns 0.
      void clear_data(void); //Remove the list created
      void fill_post(void); //Makes the array of postings index object
         //pointers.
      void dele_post(void); //Removes the array of postings pointers.
      void gclose_map(void);
      void gclose_map(long sn);
         //sn is the number of the particular postings object.

      //Data
      long nwrd; //Number of strings for which there are postings.
      long *freq; //Room to store the frequency of the terms.
      long *pddr; //Room to store the address of the post for any term.
      long *sddr; //Room to store the address of terms.
      char *term; //For 's' file memory map.
      long *pfil; //For map of 'p' file
      X *xfil; //For map of 'x' file
      Index *Pst; //Pointer at postings (array of
         //index objects).
      Btree *btr; //Holds the postings data.

   private:
      Zdat<X> *pPdat; //Moving pointer.
      Zdat<X> *pQdat; //Fixed pointer.
      Zdat<X> Udat; //Starting point.
      Zdat<X> *tread(long m,long n); //Begins at element m in file and 
         //reads in n data elements
      long twrite(ofstream *pfout,ofstream *xfout,Zdat<X> *dat); //Writes out the data elements
         //associated with dat and returns the number so written.
};
}
#endif
