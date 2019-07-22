#ifndef FBASE_H
#define FBASE_H

#include <iostream>
#include <fstream>

using namespace std;
namespace iret {

typedef char *pChr;

class FBase {
   public:
      FBase(const char *tp,const char *nm); //tp is type name, nm is name 
      ~FBase();
      void change_type(const char *nm); //Allows change of type string for class.
      void change_name(const char *nm); //Allows change of name string for class.
      void set_name(const char *nm); //Allows change of name string for class.
         //Included for compatibility

      //Path access functions
      void get_pathx(char *cn,const char *a);
         //Reads the path from a file "path_(*name)" and constructs the
         //file name from as "(*type)_(*name).(*a)". Cats path and file
         //name and returns the full info in cn.
      void get_pathx(char *cn,long n,const char *a);
      char *add_num(const char *ptr,long n,char *buf); //converts long to ascii
         //and cats to end of string and returns pointer to new string
         //that results. Does not change input string. The new string is
         //held in buffer space and this is overwritten at each call.

      //Stream object pointers
      ifstream *get_Istr(const char *a,ios::openmode m=ios::in);
         //Opens input file stream by path and name composition.
      ofstream *get_Ostr(const char *a,ios::openmode m=ios::out);
         //Opens output file stream by path and name composition.
      fstream *get_Fstr(const char *a,ios::openmode m=ios::in|ios::out);
         //Opens output file stream by path and name composition.
      ifstream *get_Istr(long n,const char *a,ios::openmode m=ios::in);
      ofstream *get_Ostr(long n,const char *a,ios::openmode m=ios::out);
      fstream *get_Fstr(long n,const char *a,ios::openmode m=ios::in|ios::out);
      void dst_Istr(ifstream *pfin);
      void dst_Ostr(ofstream *pfout);
      void dst_Fstr(fstream *pfstr);

      //Get file size in bytes
      long get_Fsiz(const char *a);
      long get_Fsiz(long n,const char *a);

      //File existence
      int Exists(const char *a); //returns 1 if file exists

      //Read in array pointers
      char *get_Read(const char *a);
         //Reads in a file into an char array and returns pointer
      char *get_Read(long n,const char *a);

      //Memory map pointers
      char *get_Mmap(const char *a);
         //Memory maps file by path and name composition.
      char *get_Mmap(long n,const char *a);
      void dst_Mmap(const char *a,char *ptr);
         //Removes the memory map for ptr based on path and name composition.
      void dst_Mmap(long n,const char *a,char *ptr);

      //Array of chars and binary write
      void bin_Writ(const char *a,long nm,char *ptr); 
         //Writes out nm bytes binary
      void bin_Writ(long n,const char *a,long nm,char *ptr); 

      //Logical accounting functions 
      int Gcom(int sflag); //sflag is bit marker such as READ_W, etc.
         //This returns 1 if sflag bit not set in oflag and is in cflag
         //If this is the case then it sets sflag in oflag.
      int Rcom(int sflag);
         //This returns 1 if sflag bit set in oflag and in cflag
         //If this is the case then it turns off sflag in oflag.

//Data
     int cflag; //Command, what should happen to resources.
     int oflag; //Bit string status of resources, 1 open, 0 closed.
     char *type;
     char *name;
};

}
#endif
