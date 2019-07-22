#ifndef STRSET_H
#define STRSET_H

#include <iostream>
#include <fstream>
#include <runn.h>
#include <Btree.h>
#include <FBase.h>

#define MAP_F 1

using namespace std;
namespace iret {

class Strset : public FBase{
public:
   Strset(const char *);
   ~Strset();
   void gopen_write(void); 
   void add_str(const char *pch); //Call to add a string to the set
   void gclose_write(void);
     //Files are .n, .a, and .s

   void gopen_map(void);
   char *show_str(long n); //Returns pointer at the nth string
   void gclose_map(void);
  
   //Data
   long num; //number of strings in the set (file .n)
   long *addr; //array of offsets to strings (file .a)
   char *str;  //pointer at string map (file .s)
   ofstream *pfa; //Used for file creation
   ofstream *pfs; //Used for file creation
};

class Lexos : public Strset {
public:
   Lexos(const char *);
   ~Lexos();
   void create_Lexos(List &Ls);
   long find(const char *str); //If finds string
      //returns its number+1 else returns 0.
      //Does binary search.
   int stc_my(const char *,const char *);
      //Function used to compare two strings.
   long lfind(const char *str); //returns number
      //+1 for longest string that matches an
      //initial segment of the string str.
      //Otherwise returns 0.
   long tfind(const char *str); //returns number of
      //string matches found which begin at beginning
      //of str. Held in ht. Calls ifind.
   long ifind(long,long,const char*); //Called by lfind
      //and tfind
   int stc_ly(const char *,long); //Called in ifind
   //Data
   int slen; //Length of string str in tfind call.
   int a; //Variable used for comparison of strings.
   int b; //Variable used for comparison of strings.
   char *tx; //Holds string copy for lfind and tfind.
   long *ht; //Holds the matchs at various depths.
      //At i puts the index+1 of a match of length i.
      //If no match of length i, holds 0.
   long *sn; //Holds the beginning of pair
   long *sm; //Holds the end of pair
};

}
#endif
