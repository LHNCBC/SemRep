#ifndef BNBAYES_H
#define BNBAYES_H

#include <iostream>
#include <fstream>
#include "Docum.h"
#include "Blist.h"
#include "Bool.h"
#include "Post.h"
#include "Regist.h"
#include "Nabor.h"
using namespace std;
namespace iret {

class BnBayes {
   public:
      BnBayes(); //By default count_lim is set to 200,000.
      BnBayes(long lim); //count_lim is set to lim. 
      ~BnBayes();

      void gopen_BnBayes(Regist *pReg,Slice_Accomp *pSac); //Opens for operating. 
         //Sets up slice access based on inv_load in Slice_Access class.
         //sets up arrays needed for function of this class.
      void gopen_BnBayes_map(Regist *pReg,Slice_Accomp *pSac); //Opens for operating. 
         //Sets up access based on mapping.
      void gopen_NoPost(Regist *pReg,Slice_Accomp *pSac); //Opens for operating. 
         //sets up arrays needed for all but postings access 
         //function of this class.

      //Counting functions.
      void zerot(void); //Zeroes tx array.
      void zeros(void); //Zeroes sx array.
      void transfer(void); //Transfers all counts from freq array to tx.
      void countDoc(long i); //Adds the counts from doc i to tx using read function.
      void counsDoc(long i); //Adds the counts from doc i to sx using read function.
      void counstDoc(long i); //Adds the counts from doc i to sx & tx using read function.
      void ncountDoc(long i); //Subt the counts from doc i from tx using read function.
      void ncounsDoc(long i); //Subt the counts from doc i from sx using read function.
      void ncounstDoc(long i); //Subt the counts from doc i from sx using read function.
      void countTot(long ix,long *ixx); //adds all counts in array to tx.
      void countSub(long ix,long *ixx); //adds all counts in array to sx.
      void countBth(long ix,long *ixx); //adds all counts in array to sx and tx.
      void ncountTot(long ix,long *ixx); //subtracts all counts in array from tx.
      void ncountSub(long ix,long *ixx); //subtracts all counts in array from sx.
      void ncountBth(long ix,long *ixx); //subtracts all counts in array from sx and tx.

      //Weighting functions.
      void weightSall(float cut); //Weights assigned to all terms with |wt|>cut.
      void weightSneg(float cut); //Weights assigned to all terms with wt<-cut.
      void weightSpos(float cut); //Weights assigned to all terms with wt>cut.
      void weightApos(float alpha_cut); //Weights assigned to alpha>alpha_cut.

      //Scoring functions.
      void scoreAll(void); //Performs scoring for all documents.
      void scoreSet(long im,long *imd); //Score the indexed set only.

      //Sampling functions.
      Index *Whole(void); //Makes the universal index set of all indices.
      Index *Interval(long n,long m); //Makes the index with indices from
         //n<=x<m.
      Index *Subsample(long n,Index *pind); //Generates a random sample of
         //pind of size n if pind is larger pind->ix>n, else returns pind.
      Index *Initial_seg(long n,Index *pind); 
         //Generates an index object containing the initial n elements of pind
      Index *Final_seg(long n,Index *pind); 
         //Generates an index object containing the final n elements of pind
      Index *Randsample(long n); //Generates a random sample of size n from
         //the whole space.

      //Retrieval functions
      Order *Similar_One(long n,Index *pind,Index *qind); //Return pointer to an order with
         //top n scoring docs outside of (*pind union *qind). qind can be NULL.
         //Based on *pind and ~(*pind + *qind) contrast.
      Order *Similar_Two(long n,Index *pind,Index *qind); //Return pointer to an 
         //order with top n scoring docs outside of *pind and inside *qind.
         //Based on *pind and ~(*pind + *qind) contrast.
      Order *Similar_Three(long n,Index *pind,Index *zind,Index *qind); //Return pointer to an 
         //order with top n scoring docs inside *qind.
         //Based on *pind and *zind contrast.
      Order *Similar2_Two(long n,Index *pind,Index *qind); //Return pointer to an 
         //order with top n scoring docs outside of *pind and inside *qind.
         //Based on *pind and ~(*pind + *qind) contrast.
         //Two stages. Second stage based on top 100k docs.
      Order *Similar3_Two(long n,Index *pind,Index *qind); //Return pointer to an 
         //order with top n scoring docs outside of *pind and inside *qind.
         //Based on *pind and ~(*pind + *qind) contrast.
         //Three stages. Second stage based on top 100k docs.
         //Third stage based on top 5% from stage 2.

      //Debugging funcion
      void debug(long id); //Prints out docs terms with weights and score for doc.

      //Global data
      long ndoc; //Number of documents in the system.
      long nid; //Next unused term id number.
      long *tx; //For total counts based on current set. nid size.
      long *sx; //For total counts based on subject set. nid size.
      float *weg; //For storage of weights. Points at sx.
      long cflag; //0 if sx use, 1 if weg use of sx.
      long nnx; //Total documents for purposes of weighting.
      long nsx; //Number of documents in the subject area for weighting.
      float cs; //Additve constant for correct scoring of docs.
      float *sco; //For storage of scores.

      Regist *pRgg; //Holds pointer to Regist object that is used for document access.
      Slice_Accomp *pSaa; //Holds pointer to Slice_Access object that is used for
         //postings access.
      long count_lim; //Limit on the size of a set of docs to count. 
      int err_flag; //When no error is 0. When count_lim exceeded is 1.

};

//Multithreading version

extern "C" void *bscore(void *arg); //Thread function.
extern "C" void *cnt_t(void *arg); //Thread function.
extern "C" void *cnt_s(void *arg); //Thread function.
extern "C" void *cnt_b(void *arg); //Thread function.
extern "C" void *ncnt_t(void *arg); //Thread function.
extern "C" void *ncnt_s(void *arg); //Thread function.
extern "C" void *ncnt_b(void *arg); //Thread function.
extern "C" void *xscore(void *arg); //Thread function.

class BnBayes_pth {
   public:
      BnBayes_pth(); //By default count_lim is set to 200,000.
      BnBayes_pth(long lim); //count_lim is set to lim. 
      ~BnBayes_pth();

      void gopen_BnBayes_pth(Regist_pth *pReg,Slice_Accpth *pSac); //Opens for operating. 
         //Sets up slice access based on inv_load in Slice_Access class.
         //sets up arrays needed for function of this class.
      void gopen_BnBayes_map(Regist_pth *pReg,Slice_Accpth *pSac); //Opens for operating. 
         //Sets up access based on mapping.
      void gopen_NoPost_pth(Regist_pth *pReg,Slice_Accpth *pSac); //Opens for operating. 
         //sets up arrays needed for all but postings access 
         //function of this class.

      //Counting functions.
      void zerot(void); //Zeroes tx array.
      void zeros(void); //Zeroes sx array.
      void transfer(void); //Transfers all counts from freq array to tx.
      void countTot(long ix,long *ixx); //adds all counts in array to tx.
      void countSub(long ix,long *ixx); //adds all counts in array to sx.
      void countBth(long ix,long *ixx); //adds all counts in array to sx and tx.
      void ncountTot(long ix,long *ixx); //subtracts all counts in array from tx.
      void ncountSub(long ix,long *ixx); //subtracts all counts in array from sx.
      void ncountBth(long ix,long *ixx); //subtracts all counts in array from sx and tx.

      //Weighting functions.
      void weightSall(float cut); //Weights assigned to all terms with |wt|>cut.
      void weightSneg(float cut); //Weights assigned to all terms with wt<-cut.
      void weightSpos(float cut); //Weights assigned to all terms with wt>cut.
      void weightApos(float alpha_cut); //Weights assigned to alpha>alpha_cut.

      //Scoring functions.
      void scoreAll(void); //Performs scoring for all documents.
      void scoreSet(long im,long *imd); //Score the indexed set only.

      //Sampling functions.
      Index *Whole(void); //Makes the universal index set of all indices.
      Index *Interval(long n,long m); //Makes the index with indices from
         //n<=x<m.
      Index *Subsample(long n,Index *pind); //Generates a random sample of
         //pind of size n if pind is larger pind->ix>n, else returns pind.
      Index *Randsample(long n); //Generates a random sample of size n from
         //the whole space.
      Index *Initial_seg(long n,Index *pind); 
         //Generates an index object containing the initial n elements of pind
      Index *Final_seg(long n,Index *pind); 
         //Generates an index object containing the final n elements of pind

      //Retrieval functions
      Order *Similar_One(long n,Index *pind,Index *qind); //Return pointer to an order with
         //top n scoring docs outside of (*pind union *qind). qind can be NULL.
         //Based on *pind and ~(*pind + *qind) contrast.
      Order *Similar_Two(long n,Index *pind,Index *qind); //Return pointer to an 
         //order with top n scoring docs outside of *pind and inside *qind.
         //Based on *pind and ~(*pind + *qind) contrast.
      Order *Similar_Three(long n,Index *pind,Index *zind,Index *qind); //Return pointer to an 
         //order with top n scoring docs inside *qind.
         //Based on *pind and *zind contrast.
      Order *Similar2_Two(long n,Index *pind,Index *qind); //Return pointer to an 
         //order with top n scoring docs outside of *pind and inside *qind.
         //Based on *pind and ~(*pind + *qind) contrast.
         //Two stages. Second stage based on top 100k docs.
      Order *Similar3_Two(long n,Index *pind,Index *qind); //Return pointer to an 
         //order with top n scoring docs outside of *pind and inside *qind.
         //Based on *pind and ~(*pind + *qind) contrast.
         //Three stages. Second stage based on top 100k docs.
         //Third stage based on top 5% from stage 2.

      //Debugging funcion
      void debug(long id); //Prints out docs terms with weights and score for doc.

      //Global data
      long ndoc; //Number of documents in the system.
      long nid; //Next unused term id number.
      long *tx; //For total counts based on current set. nid size.
      long *sx; //For total counts based on subject set. nid size.
      float *weg; //For storage of weights. Points at sx.
      long cflag; //0 if sx use, 1 if weg use of sx.
      long nnx; //Total documents for purposes of weighting.
      long nsx; //Number of documents in the subject area for weighting.
      float cs; //Additve constant for correct scoring of docs.
      float *sco; //For storage of scores.

      Regist_pth *pRgg; //Holds pointer to Regist_pth object that is used for document access.
      Slice_Accpth *pSaa; //Holds pointer to Slice_Access object that is used for
         //postings access.
      long count_lim; //Limit on the size of a set of docs to count. 
      int err_flag; //When no error is 0. When count_lim exceeded is 1.
};

}

#endif
