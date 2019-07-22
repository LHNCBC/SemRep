#ifndef REGIST_H
#define REGIST_H

#include <iostream>
#include <fstream>
#include <semaphore.h>
#include <pthread.h>
#include "Docum.h"
#include "Blist.h"
#include "Patt.h"
#include "Bnum.h"
#define READ_B 32 //Sets up the binary for reading.
#define READ_C 64 //Marks the set_class function.
#define FILE_NUM 100 //Number of docsets allowed in system

using namespace std;
namespace iret {

typedef long* pLong;

class Regist {
   public:
      Regist(const char *nam); // assign the regist group name
      Regist(const char *nam,long wrd_spc); // assign the regist group name
         //Sets word_space equal to wrd_spc.
      ~Regist();
         //move the doc "nam" to the new posit in doc[]
      void regstr();   //Used to create or modify a register file. 
      void set_class(); //set all pointer pDrg to each docset
      void gopen_read(int rfil);  // open rfil objects.
      void gclose_read(int rfil); // close rfil objects.
         //mapping functions
      void gopen_map(int rfil);  // map rfil objects.
      void gclose_map(int rfil); // unmap rfil objects.
         //access functions
      Docum *read(long n);
                   //return a pointer at the docset which has document n 
                   //read into RAM ready for use,
      long xlen(long index);  //given the docu num, returns it's length.
      long pmid(long index);  //given the docu num, return it's pmid
      long index(long pmid);  //givin a pmid, if found returns index+1
                  //else returns 0.
      long index(long im, long* pmid);  //giving a array of pmid, if found,
                  //replace by docu num, and return the num of pmid found
      long datx(long index);  //given the docu num, return it's date
      long datrang(long dat1, long dat2, pLong &idx);  //given a date range, 
                  //return the num of docs found and their indices in idx.
      long datgrat(long dat1, pLong &idx);  //given a date, return the num 
                  //of docs with dates >= dat1 found and their indices in idx.

      //Special functions to make and access a docset in binary representation.
      void convert(long num,IdTerm *idt); //Converts a docset numbered num
                  //to a binary representation. Makes files 'b','s', and 'a'
                  //at path_binset.
      void gopen_binary(); //Sets up for access to binary files.
      void gopen_binary_map(); //Sets maps for access to binary files.
      long readb(long n); //reads data into arrays and returns
                  //number of terms in document n.
      long *tnm;  //pointer at term ids for a document.
      unsigned char *lct;  //pointer at local counts for the terms. 
      long **offs; //Holds the arrays of offset information for the files.
      ifstream *pfin; //Holds the file stream pointers.
      void gclose_binary(); //Shuts down access and frees memory.
      void gclose_binary_map(); //Shuts down maps.
      
      //Standard docset information.
      long ndst;        // num. of docsets
      long ntot;        // num. of total documents
      long ndoc[FILE_NUM];       // array of ndoc
      char *nams[FILE_NUM];      // array of name of docsets
      char *paths[FILE_NUM];     // array of docset paths
      Docum *pDrg[FILE_NUM];     // point to each docset
      long word_space; //Number of words allowed in a single doc.
  
      //Special functions to set up and access the itame files.
      void gopen_itame_map(void); //Sets up memory maps and opens files.
      int set_itame_ptr(long index); //Returns 1 if successful.
      int read_itame_pmid(long &len,char *text);
      int read_itame_date(long &len,char *text);
      int read_itame_title(long &len,char *text);
      int read_itame_abstract(long &len,char *text);
      int read_itame_mesh(long &len,char *text);
         //all these functions return 1 if successful.
      int read_itame_doc(long &len,char *text); //Must follow set_itame_ptr
      void gclose_itame_map(void); //Shuts down maps and closes files
         //and frees memory.
      //related to itame access.
      long nit; //number of itame files.
      long *idoc; //Array of sizes of itame files.
      long itot; //Total number of itame docs.
      long **iadd; //Pointer at array long pointers
      ifstream *pfit; //Itame file pointers array.
      ifstream *pcit; //Current itame file position.
      long cit; //Current pattern number.
      Bnum *pIt; //Pointer at the binary search list (itame files).
      Patt *pIpat; //Pointer at Patt object for reading itame files.

   private:
      unsigned long ostate; //Bit array to keep the open state of class. 
           //First four bits are for state of standard files and 5th for binary.
           //Sixth for setup of pointers in set_class.
      char *name;     // holds the name of the regist group
      void add(char *nam,char *path); // add one docset to this register
      void replace(char *nam,char *path);
          //replace the doc "nam", the path may be different
      void delet(char *nam);  //delete the doc "nam"
      void move(char *nam, int posit);
      void screenr();  // write the registered docsets to screen
      void readr();  // read the associated variables of this register
                        // from a file, using the group name
      void writer(); // write the associated variables of this register
                        // to a file, using the group name
      Bnum *pBn; //Pointer at the binary search list (diff docsets).
};

//Multithreading version

class Regist_pth {
   public:
      Regist_pth(const char *nam); // assign the regist group name
      Regist_pth(const char *nam,long wrd_spc); // assign the regist group name
         //Sets word_space equal to wrd_spc.
      ~Regist_pth();
         //move the doc "nam" to the new posit in doc[]
      void regstr();   //Used to create or modify a register file. 
      void set_class(); //set all pointer pDrg to each docset
      void gopen_read(int rfil);  // open rfil objects.
      void gclose_read(int rfil); // close rfil objects.
         //mapping functions
      void gopen_map(int rfil);  // map rfil objects.
      void gclose_map(int rfil); // unmap rfil objects.
         //access functions
      Docum *read(long n);
                   //return a pointer at the docset which has document n 
                   //read into RAM ready for use,
      long xlen(long index);  //given the docu num, returns it's length.
      long pmid(long index);  //given the docu num, return it's pmid
      long index(long pmid);  //givin a pmid, if found returns index+1
                  //else returns 0.
      long index(long im, long* pmid);  //giving a array of pmid, if found,
                  //replace by docu num, and return the num of pmid found
      long datx(long index);  //given the docu num, return it's date
      long datrang(long dat1, long dat2, pLong &idx);  //given a date range, 
                  //return the num of docs found and their indices in idx.
      long datgrat(long dat1, pLong &idx);  //given a date, return the num 
                  //of docs with dates >= dat1 found and their indices in idx.

      //Special functions to make and access a docset in binary representation.
      void convert(long num,IdTerm *idt); //Converts a docset numbered num
                  //to a binary representation. Makes files 'b','s', and 'a'
                  //at path_binset.
      void gopen_binary_pth(); //Sets up for access to binary files.
      void gopen_binary_map(); //Sets maps for access to binary files.
      long readb(long n); //reads data into arrays and returns
                  //number of terms in document n.
      long *tnm;  //pointer at term ids for a document.
      unsigned char *lct;  //pointer at local counts for the terms. 
      long **offs; //Holds the arrays of offset information for the files.
      ifstream *pfin; //Holds the file stream pointers.
      void gclose_binary_pth(); //Shuts down access and frees memory.
      void gclose_binary_map(); //Shuts down maps.
      Bnum *pBn; //Pointer at the binary search list (diff docsets).
      
      //Standard docset information.
      long ndst;        // num. of docsets
      long ntot;        // num. of total documents
      long ndoc[FILE_NUM];       // array of ndoc
      char *nams[FILE_NUM];      // array of name of docsets
      char *paths[FILE_NUM];     // array of docset paths
      Docum *pDrg[FILE_NUM];     // point to each docset
      pthread_mutex_t acc[FILE_NUM]; //Mutex locks for file access.
      long word_space; //Number of words allowed in a single doc.

      //Special functions to set up and access the itame files.
      void gopen_itame_map(void); //Sets up memory maps and opens files.
      int set_itame_ptr(long index); //Returns 1 if successful.
      int read_itame_pmid(long &len,char *text);
      int read_itame_date(long &len,char *text);
      int read_itame_title(long &len,char *text);
      int read_itame_abstract(long &len,char *text);
      int read_itame_mesh(long &len,char *text);
         //all these functions return 1 if successful.
      int read_itame_doc(long &len,char *text); //Must follow set_itame_ptr
      void gclose_itame_map(void); //Shuts down maps and closes files
         //and frees memory.
      //related to itame access.
      long nit; //number of itame files.
      long *idoc; //Array of sizes of itame files.
      long itot; //Total number of itame docs.
      long **iadd; //Pointer at array long pointers
      ifstream *pfit; //Itame file pointers array.
      ifstream *pcit; //Current itame file position.
      long cit; //Current pattern number.
      Bnum *pIt; //Pointer at the binary search list (itame files).
      Patt *pIpat; //Pointer at Patt object for reading itame files.

   private:
      unsigned long ostate; //Bit array to keep the open state of class. 
           //First four bits are for state of standard files and 5th for binary.
           //Sixth for setup of pointers in set_class.
      char *name;     // holds the name of the regist group
      void add(char *nam,char *path); // add one docset to this register
      void replace(char *nam,char *path);
          //replace the doc "nam", the path may be different
      void delet(char *nam);  //delete the doc "nam"
      void move(char *nam, int posit);
      void screenr();  // write the registered docsets to screen
      void readr();  // read the associated variables of this register
                        // from a file, using the group name
      void writer(); // write the associated variables of this register
                        // to a file, using the group name
};

class Tb_data { //Thread argument data.
   public:
      Tb_data(Regist_pth *pReg,long i);
      ~Tb_data();
      void set_sco_data(long i,long b,long n,long *nx,float *scx,long *su);
       //i is file #, b is beginning of set. n # of docs & nx the list of docs.
       //Set pointer to scx array
       //Also pointer to sx which now holds weights.
      void set_cnt_data(long i,long n,long *nx,long *su,long *tu);
       //Set pointers to count arrays.
       //i is # of file to read, n and nx mark the set of doc index numbers.

       //Generic data
      Regist_pth *pRgg;
      long iz; //This objects array index;
       //Data to process
      long ii; //File number
      long beg; //Beginning number of list.
      long nm; //Number of doc index numbers in set
      long *nmx; //Pointer at list of doc index numbers
       //Pointers at arrays to be altered.
      float *sco; //score array
      long *sx; //Subject array
      long *tx; //Total array
       //Work space
      long *idn; //Memory for data read in.
      long **psx; //stack memory
      long **ptx; //stack memory
};

}
#endif

