#ifndef NABOR_H
#define NABOR_H

#include <iostream>
#include <fstream>
#include <pthread.h>
#include "Blist.h"
#include "Regist.h"
#include "Docum.h"
#include "Post.h"
#include "DataObj.h"
using namespace std;
namespace iret {

class Nabor {
   public:
      Nabor(); //By default term_lim is 1000. 
      Nabor(long lim); //term_lim is set to lim. 
      ~Nabor();

      void gopen_Nabor(Regist *pReg,Slice_Accomp *pSac); //Opens for operating. 
         //Sets up slice access based on inv_load in Slice_Accomp class.
         //sets up arrays needed for function of this class.
      void gopen_Nabor_map(Regist *pReg,Slice_Accomp *pSac); //Opens for operating. 
         //Sets up access based on mapping.

      //Vector retrieval functions.
      float *scoreVec(long n); //Scores all terms in document index n.
      float *scoreVec_pmid(long pmid); //Scores all terms in document pmid.
      float *scoreVec_Doc(Docum &Doc); //Scores all terms in document.
      float *scoreVec_Thm(Theme *pThm); //Scores all terms in theme.
      float scoreDoc_Doc(Docum &Doc,Docum &Dcc); //Similarity score of Docs
      float scoreDoc_BDoc(Docum &Doc,long pmid); //Similarity score of Docs
      void skim(long n,long *idx); //Reorders sco so that top n docs are 
          //first n elements. Assumes total number of scored docs is m.
      Order *skim(long n); //Obtains the n top scoring elements and puts
          //in order object.
      Order *skim(long n,Index *pind); //Obtains the n top scoring elements
          //from the index object pind.

      //Global data
      long nid; //Next unused term id number.
      long ndoc; //Number of documents in the system.
      float *sco; //For storage of scores.
      long *dl; //Lengths of documents
      float **tf; //Precomputed local weights held in this array.

      Regist *pRgg; //Holds pointer to Regist object that is used for document access.
      Slice_Accomp *pSaa; //Holds pointer to Slice_Accomp object that is used for
         //postings access.
      long term_lim; //Limit on number of terms in document or theme to score.
      int err_flag; //No error is 0, exceeded the limit is 1.

};

//Multithreading version

extern "C" void *fscore(void *arg); //Thread function.
extern "C" void *tmscore(void *arg); //Thread function.

class Nabor_pth {
   public:
      Nabor_pth(); //By default term_lim is 1000. 
      Nabor_pth(long lim); //term_lim is set to lim. 
      ~Nabor_pth();

      void gopen_Nabor_pth(Regist_pth *pReg,Slice_Accpth *pSac); //Opens for operating. 
         //Sets up slice access based on inv_load in Slice_Accpth class.
         //sets up arrays needed for function of this class.
      void gopen_Nabor_map(Regist_pth *pReg,Slice_Accpth *pSac); //Opens for operating. 
         //Sets up access based on mapping.

      //Vector retrieval functions.
      float *scoreVec(long n); //Scores all terms in document index n.
      float *scoreVec_pmid(long pmid); //Scores all terms in document pmid.
      float *scoreVec_Doc(Docum &Doc); //Scores all terms in document.
      float *scoreVec_Title(Docum &Doc); //Scores only title terms in document.
      float *scoreVec_Thm(Theme *pThm); //Scores all terms in theme.
      float scoreDoc_Doc(Docum &Doc,Docum &Dcc); //Similarity score of Docs
      float scoreDoc_BDoc(Docum &Doc,long pmid); //Similarity score of Docs
      float scoreBDoc_BDoc(long pmid1,long pmid2,char *mrk); //Similarity score of Docs
      float scoreIDoc_IDoc(long idx1,long idx2,char *mrk); //Similarity score of Docs
      Order *skim(long n); //Obtains the n top scoring elements and puts
          //in order object.
      Order *skim(long n,Index *pind); //Obtains the n top scoring elements
          //from the index object pind.

      //Global data
      long nid; //Next unused term id number.
      long ndoc; //Number of documents in the system.
      float *sco; //For storage of scores.
      long *dl; //Lengths of documents
      float **tf; //Precomputed local weights held in this array.

      Regist_pth *pRgg; //Holds pointer to Regist object that is used for document access.
      Slice_Accpth *pSaa; //Holds pointer to Slice_Accpth object that is used for
         //postings access.
      long term_lim; //Limit on number of terms in document or theme to score.
      int err_flag; //No error is 0, exceeded the limit is 1.
};

}

#endif
