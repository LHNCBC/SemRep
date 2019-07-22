#ifndef BOOL_H
#define BOOL_H

#include <iostream>
#include <fstream>
#include "Docum.h"
#include "Blist.h"
#include "Regist.h"
#include "DataObj.h"
using namespace std;
namespace iret {

typedef long* pLong;

class Pnxde {
   public:
      Pnxde(void); //Simple constructor;
      Pnxde(const char*); //Fills string slot.
      ~Pnxde(void); //removes string memory.
      void set_name(const char *); //Sets nam.
      char *nam; //String.
      Pnxde *lft; //Left pointer.
      Pnxde *rht; //Right pointer.
};

class Bool {
   public:
      Bool(void);  //Assumes length of parse string is <=max_str.
      Bool(long lim);  //limits length of parse string to <=lim.
      ~Bool(void);

      void gopen_Bool(Regist *pReg,Slice_Accomp *pSac); //Opens for operating. 
         //Sets up slice access based on inv_load in Slice_Accomp class.
         //sets up arrays needed for function of this class.
      void gopen_Bool_map(Regist *pReg,Slice_Accomp *pSac); //Opens for operating. 
         //Sets up access based on mapping.

      //String functions
      void interpret(char *str,char *ptr); //Interprets a string str to give 
         //the best chance of finding a match with the vocabulary of indexing.
         //Puts the interpretation in ptr.
      void extract(char *,char *,long &); //Extracts a piece that is optimal for
         //interpretation and gives offset to next piece of string.
      long findw(char *str,const char *w); //Returns 0 unless finds w in str
         //surrounded by white space. Then returns offset+1 to beginning.
      void standard(char *str,char *ptr); //Takes database string and makes
         //standard string. 
      char zlt[128]; //Maps out characters and lower cases string.
      char elt[128]; //Tests for spacers ' ', '(', '"'. 
      
      void to_Pmid(Index *,long &pm,pLong &pmd); //Maps from Index object to set of
         //PMIDs in same order.

      Index *from_Pmid(long pm,long *pmd); //Returns a pointer at an index object
         //populated by the PMIDs input.

      Index *from_String(const char *str); //Returns a pointer to index object
         //if string is in system. Otherwise returns NULL.
      Index *from_String(const char *str,long dp,long br); //Returns a pointer 
         //at an index object populated by the indices of occurrence of the 
         //string. Is null if string is not in system. dp and br are depth and
         //branch (0,1,2) from which called.
      Index *from_Date_Range(long date1,long date2); //Populates Index object with
         //indices at or above the first (date1) and before the second (date2).
      Index *from_Date_Above(long date); //Populates Index object with
         //indices at or above date.
      Index *bool_And(Index *,Index *); //Performs boolean "and" on two index sets. 
      Index *bool_Or(Index *,Index *); //Performs boolean "or" on two index sets. 
      Index *bool_Butnot(Index *,Index *); //Performs boolean "butnot"  
         //or compliment on two index sets. 

         //Same as previous three except do not destroy their argument index sets.
      Index *cbool_And(Index *,Index *); //Performs boolean "and" on two index sets. 
      Index *cbool_Or(Index *,Index *); //Performs boolean "or" on two index sets. 
      Index *cbool_Butnot(Index *,Index *); //Performs boolean "butnot"  

      Index *bool_Recurs(char *str,long dp,long br); //Performs parse of 
         //string by recursive calls and use of previous functions.
         //dp and br are depth and branch of caller.
      Index *bool_Parse(char *str); //Builds parse tree & index set.
         //Warning: Do not give const char * type to this function
         //It changes the contents of the space pointed at by str.
      void standard_String(char *str); //Uses a parse tree to make string.
      Regist *pRgg; //Pointer to register object.
      Slice_Accomp *pSaa; //Pointer to slice access object.

      //Parse tree data.
      long bra[100]; //Branch of tree last seen.
      Pnxde *pxn[100]; //Pointer at current path nodes.
      long str_lim; //Limit on length of parse string.
      int err_flag; //No error is 0, exceed str_lim is 1.
         //Unable to find a string in database is 2.
};

//Multithreaded version

class Bool_pth {
   public:
      Bool_pth(void);  //Assumes length of parse string is <=max_str.
      Bool_pth(long lim);  //limits length of parse string to <=lim.
      ~Bool_pth(void);

      void gopen_Bool_pth(Regist_pth *pReg,Slice_Accpth *pSac); //Opens for operating. 
         //Sets up slice access based on inv_load in Slice_Accomp class.
         //sets up arrays needed for function of this class.
      void gopen_Bool_map(Regist_pth *pReg,Slice_Accpth *pSac); //Opens for operating. 
         //Sets up access based on mapping.

      //String functions
      void interpret(char *str,char *ptr); //Interprets a string str to give 
         //the best chance of finding a match with the vocabulary of indexing.
         //Puts the interpretation in ptr.
      void extract(char *,char *,long &); //Extracts a piece that is optimal for
         //interpretation and gives offset to next piece of string.
      long findw(char *str,const char *w); //Returns 0 unless finds w in str
         //surrounded by white space. Then returns offset+1 to beginning.
      void standard(char *str,char *ptr); //Takes database string and makes
         //standard string. 
      char zlt[128]; //Maps out characters and lower cases string.
      char elt[128]; //Tests for spacers ' ', '(', '"'. 
      
      void to_Pmid(Index *,long &pm,pLong &pmd); //Maps from Index object to set of
         //PMIDs in same order.

      Index *from_Pmid(long pm,long *pmd); //Returns a pointer at an index object
         //populated by the PMIDs input.

      Index *from_String(const char *str); //Returns a pointer to index object
         //if string is in system. Otherwise returns NULL.
      Index *from_String(const char *str,long dp,long br); //Returns a pointer 
         //at an index object populated by the indices of occurrence of the 
         //string. Is null if string is not in system. dp and br are depth and
         //branch (0,1,2) from which called.
      Index *from_Date_Range(long date1,long date2); //Populates Index object with
         //indices at or above the first (date1) and before the second (date2).
      Index *from_Date_Above(long date); //Populates Index object with
         //indices at or above date.
      Index *bool_And(Index *,Index *); //Performs boolean "and" on two index sets. 
      Index *bool_Or(Index *,Index *); //Performs boolean "or" on two index sets. 
      Index *bool_Butnot(Index *,Index *); //Performs boolean "butnot"  
         //or compliment on two index sets. 

         //Same as previous three except do not destroy their argument index sets.
      Index *cbool_And(Index *,Index *); //Performs boolean "and" on two index sets. 
      Index *cbool_Or(Index *,Index *); //Performs boolean "or" on two index sets. 
      Index *cbool_Butnot(Index *,Index *); //Performs boolean "butnot"  

      Index *bool_Recurs(char *str,long dp,long br); //Performs parse of 
         //string by recursive calls and use of previous functions.
         //dp and br are depth and branch of caller.
      Index *bool_Parse(char *str); //Builds parse tree & index set.
         //Warning: Do not give const char * type to this function
         //It changes the contents of the space pointed at by str.
      void standard_String(char *str); //Uses a parse tree to make string.
      Regist_pth *pRgg; //Pointer to register object.
      Slice_Accpth *pSaa; //Pointer to slice access object.

      //Parse tree data.
      long bra[100]; //Branch of tree last seen.
      Pnxde *pxn[100]; //Pointer at current path nodes.
      long str_lim; //Limit on length of parse string.
      int err_flag; //No error is 0, exceed str_lim is 1.
         //Unable to find a string in database is 2.
};

}

#endif
