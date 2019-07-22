#ifndef THEMDEC_H
#define THEMDEC_H

#include <iostream>
#include <fstream>
#include <DataObj.h>
#include <Docum.h>
#include <Postg.h>

using namespace std;
namespace iret {

class ThemDec {
public:
   ThemDec(Postg<float> *pPg);
   ~ThemDec(void);
   void gopen_map(void);
   void gclose_map(void);

   float *ScoreAll(Docum &Doc); //Scores the whole space of themes
      //scores appear in sco and a pointer is returned
   Order *TopScore(long n,Docum &Doc); //Scores the whole space of themes
      //scores appear in sco and a pointer to an order object with 
      //top n is returned. 

   long num; //Number of themes
   float *sco; //Scores of themes
   float *cs; //Pointer at mapped set of cs
   long nwrd; //Number of terms in the set
   Index *Pst; //Pointer at postings Index objects.
   float **flc; //Pointer at mapped memory for weights.

   Postg<float> *pPsg;
};

}
#endif
