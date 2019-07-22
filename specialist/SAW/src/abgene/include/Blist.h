#ifndef BLIST_H
#define BLIST_H

#include <iostream>
#include <fstream>
#include <unistd.h>
#include <semaphore.h>
#include <pthread.h>
#include "Bnum.h"

#define LIST_S 1 //gopen_string called
#define LIST_I 2 //id_load called
#define LIST_V 4 //inv_load called
#define LIST_A 8 //post_access called

using namespace std;
namespace iret {

class Blist {
public:
   Blist(); 
   Blist(const char *nam); //nam holds the occurrence name of the object.
   Blist(long n,ifstream &); //Reads in a list of n strings in lexical 
   ~Blist();
   void size(long n); //Sets size of list but does not add strings. Strings
      //added by add_string function. Must be added in lexical order. 
   void fill(long n,ifstream &); //Reads in a list of n strings in lexical 
      //order and sets up array to hold the list of strings. Must be in ascii.
   long find(const char *); //If finds the string, returns the number in the 
                            //list beginning with 1. Otherwise returns 0.
   inline char *string(long k){return(*(lstr+k));} //Returns pointer to string 
                            //numbered k.
   int add_string(long len,const char *str); //Adds the string to the list and 
      //increments ct. Returns 1 if successful and 0 otherwise.
   long ct; //Counter for string additions.

  //Special functions for large lists.
   void shrink(void); //Takes a standard list and reduces the memory requirements.
   void write(void); //Writes out the shrunken form of the list.
   void write_new(void); //Writes out same as write, but uses path_blistnew.
   void load(void);  //Loads a shrunken list into memory.
   long sfind(const char *); //If finds the string in the shrunken list, returns 
      //the number in the list beginning with 1, Otherwise returns 0.
   void nfind(long n,char *); //If 0<n<nwrd the string that corresponds to n
      //is constructed and returned in the second argument, else NULL is returned
  //Disk access functions.
   void create_addr_string(const char *slice_nam); //Used to create address file 
      //for slice_*.s file.
   void gopen_string(const char *slice_nam); //Open files prepared to convert 
      //index to string. Relies on slice_*.s file.
   void disk_nfind(long n,char *); //If 0<=n<nwrd the string that corresponds to n
      //is constructed and returned in the second argument, else NULL is returned
   long disk_snfind(const char *); //If finds the string in the list returns index
      //plus 1, else returns 0.
   void gclose_string(void); //Closes files.

   long nwrd; //Holds the number of strings in the list.

protected:
   char *name; //Holds the occurrence name of the object.
   unsigned long ostate; //Bit indicator of what is open.

private:
   int stc_my(const char *,const char *); //Function used to compare two strings.
   int stc_mys(const char *,long); //Function used to compare two strings.
       //The second argument holds the index number of a string in lstr.

   int a; //Variable used for comparison of strings.
   int b; //Variable used for comparison of strings.
   char **lstr; //Holds the strings in the list.
   char *ign; //Array the initial length of each string to ignore.
   fstream fadd; //File of addresses to the truncated string file.
   fstream fstr; //File of strings.
};   

class IdTerm : public Blist {
public:
   IdTerm(const char *nam); //nam holds the occurrence name of the object.
   ~IdTerm();
   void create(void); //Creates a new id structure from the slice s file
      //of the same nam. Uses path_slice and path_blistnew.
   void update1(void); //Loads in the current Id structures from path_blist
   void update2(void); 
      //and makes new structures for updating in path_blistnew and in process
      //uses path_slice (new slice n and s file location). Both 1 & 2 needed.
   //Standard access functions from string to id number. Uses shrunken list
   void id_load(void); //Loads in the current Id structures ready for use.
      //Uses path_blistset.
   long id_find(const char *str); //Returns the id number of the string (>=1) or 0
      //if the string is not found.

   //Inverse access functions from id number to index and string off disk.
   void inv_load(void); //Create the inversion array to find index from id.
   void inv_destroy(void); //Destroy the inversion array.
      //mapped versions
   void create_inv_file(void); //Creates the inversion in a binary file.
   void inv_map(void); //Sets up the memory map for inv.
   void inv_map_destroy(void); //Unmaps inv.

   inline long index(long id){ //Returns index+1 if exists, else 0.
      if((0<=id)&&(id<nid))return(*(inv+id));
      else return(0);   
      }
   void gopen_idstring(const char *slice_nam); //Open files prepared to convert 
      //index to string. Relies on slice_*.s file. Also string to index & id.
   void disk_ifind(long id,char *); //If 0<n<nid the string that corresponds to id 
      //is constructed and returned in the second argument, else NULL string is 
      //returned Must have called gopen_string() (or gopen_idstring()) & inv_load() 
      //function before use of this function.
   long disk_sifind(const char *); //If string is found returns id, else returns
      //zero. To follow gopen_idstring(). 
   void gclose_idstring(void); //Closes files.

//protected:
   long nid; //Next Id number available for use.
   long *idn; //Array of Id numbers of length nwrd (number of terms in system).
   long *inv; //Array of indices that correspond to id numbers, 0's otherwise.
      //Length is nid.
   fstream fkid; //File of list of ids by index number.
};

class Slice_Access : public IdTerm {
public:
   Slice_Access(const char *nam,const char *slice_nam); //nam holds the occurrence
      //name and slice_nam the slice files occurrence name.
   ~Slice_Access();
   void post_access(void);
   ifstream *pfile(long &f,const char *str); //Given a string it returns freq f
      //and pointer to stream object from which postings can be read, if not
      //found returns f=0 and NULL pointer. To follow load (or id_load).
   ifstream *d_pfile(long &f,const char *str); //Given a string it returns freq f
      //and pointer to stream object from which postings can be read, if not
      //found returns f=0 and NULL pointer. To follow gopen_string.
   ifstream *pfile(long &f,long id); //Given an id it returns freq f
      //and pointer to stream object from which postings can be read, if not
      //found returns f=0 and NULL pointer. To follow inv_load.
   ifstream *i_pfile(long &f,long ix); //Given an index ix it returns freq f
      //and pointer to stream object from which postings can be read, if not
      //found returns f=0 and NULL pointer. 
   void post_close(void);

   long *freq; //Frequency of each term selected.
   long *addr; //Address of each term selected.
   long cfln; //Number of files/slices.
   Bnum *pBn; //Binary search object for file numbers.
   ifstream *pfln; //Holds the slice file stream objects.
   char *slice_name; //Holds the name of slice file occurrence.
};

class Slice_Accomp : public IdTerm {
public:
   Slice_Accomp(const char *nam,const char *slice_nam); //nam holds the occurrence
      //name and slice_nam the slice files occurrence name.
   ~Slice_Accomp();
   void post_access(void);
   void post_access_map(void);
   long read_comp(long ix); //Reads in compressed data for ix and fills
      //cod with cnb bytes. Returns pk for this term.
   long str_post(const char *str,long *idx,unsigned char *lct); //Given a 
      //string it returns freq f. Returns 0 if string not found.
      //Returns postings and local counts in arrays. To follow load (or id_load).
   long dstr_post(const char *str,long *idx,unsigned char *lct); //Given a 
      //string it returns freq f. Returns 0 if string not found.
      //Returns postings and local counts in arrays. To follow gopen_string.
   long id_post(long id,long *idx,unsigned char *lct); //Given an id it 
      //returns freq f. Returns postings and local counts in arrays. 
      //To follow inv_load. If not found returns 0.
   long ix_post(long ix,long *idx,unsigned char *lct); //Given an index ix 
      //it returns freq f. If not found returns 0.
   void post_close(void);
   void post_close_map(void);

   long ndoc; //Number of docs in system.
   long *freq; //Frequency of each term selected.
   long *addr; //Address of each term selected.
   long pw[31]; //Holds powers of two.
   long fm[31]; //Holds frequency divisions.
   Bnum *pBn; //Binary search object for file numbers.
   long cfln; //Number of files/slices.
   ifstream *pfln; //Holds the slice file stream objects.
   long cnb; //Number bytes to read
   unsigned char *cod; //Compressed data read in.
   char *slice_name; //Holds the name of slice file occurrence.
};

//Multithreading version
class Slice_Accpth : public IdTerm {
public:
   Slice_Accpth(const char *nam,const char *slice_nam); //nam holds the occurrence
      //name and slice_nam the slice files occurrence name.
   ~Slice_Accpth();
   void post_access(void);
   void post_access_map(void);
   long read_pthr(long ix,long *cn,unsigned char *cch); //For external calls
      //with space supplied.
   long read_comp(long ix); //Reads in compressed data for ix and fills
      //cod with cnb bytes. Returns pk for this term.
   long str_post(const char *str,long *idx,unsigned char *lct); //Given a 
      //string it returns freq f. Returns 0 if string not found.
      //Returns postings and local counts in arrays. To follow load (or id_load).
   long dstr_post(const char *str,long *idx,unsigned char *lct); //Given a 
      //string it returns freq f. Returns 0 if string not found.
      //Returns postings and local counts in arrays. To follow gopen_string.
   long id_post(long id,long *idx,unsigned char *lct); //Given an id it 
      //returns freq f. Returns postings and local counts in arrays. 
      //To follow inv_load. If not found returns 0.
   long ix_post(long ix,long *idx,unsigned char *lct); //Given an index ix 
      //it returns freq f. If not found returns 0.
   void post_close(void);
   void post_close_map(void);

   long ndoc; //Number of docs in system.
   long *freq; //Frequency of each term selected.
   long *addr; //Address of each term selected.
   long pw[31]; //Holds powers of two.
   long fm[31]; //Holds frequency divisions.
   pthread_mutex_t acc[31]; //Mutex locks for file access.
   Bnum *pBn; //Binary search object for file numbers.
   long cfln; //Number of files/slices.
   ifstream *pfln; //Holds the slice file stream objects.
   long cnb; //Number bytes to read
   unsigned char *cod; //Compressed data read in.
   char *slice_name; //Holds the name of slice file occurrence.
};

class Th_data { //Thread argument data.
   public:
      Th_data(Slice_Accpth *pSac,long i);
      ~Th_data();
      void set_nab_data(long *dd,float **tt,float *sxx);
         //Set document length pointer to dd, local wt pointer to tt
         //Set sco pointer to sxx.
      void set_bay_data(float *sxx);

       //Generic data
      Slice_Accpth *pSaa;
      float *sco; //score array
      long iz; //This objects array index;
       //Variable data
      long *dl; //length array
      float **tf; //local weight array.
       //Work space
      unsigned char *xch; //Memory for data read in.
      float **ad; //Memory for pointers at sco array
      float *wd; //Memory for weight to add to sco
};

}
#endif
