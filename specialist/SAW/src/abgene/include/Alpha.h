#ifndef ALPHA_H
#define ALPHA_H

#include <iostream>
#include <fstream>
#include "Docum.h"
#include "Blist.h"
#include "Bool.h"
#include "Post.h"
#include "Regist.h"
#include "Nabor.h"
#include "Hyper.h"
using namespace std;
namespace iret {

//Multithreading version

extern "C" void *cnt_t(void *arg); //Thread function.
extern "C" void *cnt_s(void *arg); //Thread function.
extern "C" void *cnt_r(void *arg); //Thread function.
extern "C" void *score_s(void *arg); //Thread function.

class Alpha_pth {
   public:
      Alpha_pth();  
      ~Alpha_pth();

      void gopen_Alpha_map(Regist_pth *pReg,Slice_Accpth *pSac); //Opens for operating. 
         //Sets up slice access based on inv_load in Slice_Access class.
         //sets up arrays needed for function of this class.
     
      //Counting functions.

      void create_cut_array(long ns); //Compute .
      void read_cut(const char *cnam);
      void zerot(void); //Zeroes tx array.
      void zeros(void); //Zeroes sx array.
      void zeror(void);
      void transfer(void); //Transfers all counts from freq array to tx.
      void countTot(long ix,long *ixx); //adds all counts in array to tx.
      void countSub(long ix,long *ixx); //adds all counts in array to sx.
      void countRxx(long ix,long *ixx); //adds all counts in array to sx and tx.

      //Scoring functions
      float scoreSet(long im, float &salpha); //Score the indexed set only.

      //Sampling functions.
      Index *Subsample(long n,Index *pind); //Generates a random sample of
         //pind of size n if pind is larger pind->ix>n, else returns pind.
      Index *Randsample(long n); //Generates a random sample of size n from
         //the whole space.
      
      //Global data
      long ndoc; //Number of documents in the system.
      long nid; //Next unused term id number.
      long *tx; //For total counts based on current set. nid size.
      long *sx; //For total counts based on subject set. nid size.
      long nnx; //Total documents for purposes of weighting.
      long nsx; //Number of documents in the subject area for weighting.
      long nrx;
      
      Regist_pth *pRgg; //Holds pointer to Regist_pth object that is used for document access.
      Slice_Accpth *pSaa; //Holds pointer to Slice_Access object that is used for
         //postings access.

};

}

#endif
