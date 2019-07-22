#ifndef ISLAND_H
#define ISLAND_H

#include <iostream>
#include <fstream>
#include <DataObj.h>
#include <Strset.h>

using namespace std;
namespace iret {

class Island {
public:
   Island(long llm,long maxstr);
      //llm is lower limit of island len.
      //Sets up arrays for processing  
      //any string up to maxstr in length.
   ~Island();
   void Dpset(char *tst); //Input string and
      //set duplication avoidance values in
      //szlm. Remembers tst in test.
   void Zerot(void); //Zeros ctc array.
   void Iscount(long beg,Strset *pSt); //Counts island
      //matches between test and members of pSt >=beg.
      //Adds to ctc array.
   void Iscount(long beg,Index *pnd,Strset *pSt); //Counts island
      //matches between test and members of pSt >=beg in pnd.
      //Adds to ctc array.
   void Incl_Add(Count *pCt); //Adds islands and counts
      //+1 from ctc array to pCt.
   void Excl_Add(Count *pCt); //Adds islands and counts
      //from ctc array to pCt.
   void Add(List *pLt); //Adds islands 
      //from ctc array to pLt.
  
   //Data
   char *test; //String to compare
   long slen; //length of test
   long lowlm; //Lower limit island length. 
   long lcap; //maximum string length.
   long *szlm; //Size limit array.
   long *mm1; //Alignment arrays
   long *mm2;
   long **ctc; //Array for counts.
};
}
#endif
