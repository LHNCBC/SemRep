#ifndef THLN_H
#define THLN_H

#include <iostream>
#include <fstream>
#include <DataObj.h>
#include <Dbinbase.h>
#include <Postg.h>

using namespace std;
namespace iret {

class WOrder : public Order {
  public:
   WOrder(void);
   WOrder(Order *pOrd,float *weg); //Uses weg[pOrd->pInd->idx[i]]
      //to obtain the weights for terms
   WOrder(Order *pOrd,double *weg);
      //Both these constructors destroy the Order object pointed
      //to by pOrd. A case of catabolism.
   ~WOrder(void);
   float ave_sco(long n); //Avererages the n top scores
   float dice_sco(WOrder *wpord); //Produces dice similarity score.
   //Data
   float *weight;
};

class Thln {
public:
  Thln(Dbinbase *pDb,Postg<char> *pPg);
  Thln(Docum &Doc);
  ~Thln(void);
  void create_Postg(Docum &Doc);
  void create_Dbinbase(Docum &Doc); //Should only be
     //called after create_Postg is called.

  void Set_Train(Index *gd,Index *bd); 
     //gdd good docs, bdd bad docs
  void Set_Param(long ntrm,long stng); 
     //ntrm, # terms in theme, stng, stringency

      //Counting functions.
   void read(long n); //Sets pointers in pDnb structures to doc #n
   void zerot(void); //Zeroes tx array.
   void zeros(void); //Zeroes sx array.
   void countDoc(long i); //Adds the counts from doc i to 
      //tx using read function.
   void counsDoc(long i); //Adds the counts from doc i to 
      //sx using read function.
   void counbDoc(long i); //Adds the counts from doc i to 
      //sx & tx using read function.
      //Set reads
   void countTX(void); //adds all counts in gdd and bdd to tx.
   void countTX(Index *cnd); //adds all counts in cnd to tx.
   void countSX(Index *cnd); //adds all counts in cnd to sx.
   void countBX(Index *cnd); //adds all counts in cnd to sx & tx.

      //Weighting functions.
   WOrder *weightSall(void); //Weights assigned to all terms with !=0.
   double *ScoreAll(WOrder *pord); //Scores the whole space of ndoc docs
      //scores appear in sco and a pointer is returned
   double ScoreLim(WOrder *pord); //Sets xlim

      //Processing
   WOrder *Initiator(void); //Starts theme generation
   WOrder *Initiator(WOrder *wpord); //Starts theme generation
   WOrder *Iterator(void); //Makes one round of theme EM algorithm
   WOrder *Moderator(WOrder *wpord,long clim); //Runs to convergence
      //or clim iterations, whichever comes first.
   void show_terms(WOrder *wzord);
   void Theme_write(ofstream &fout,WOrder *wpord); //Writes the data 
      //out in standard Theme format

      //Global data
   long nwrd; //Number of terms in the set
   double *tx; //For total counts based on current set. nwrd size.
   double *sx; //For total counts based on subject set. nwrd size.
   double nnx; //Total documents for purposes of weighting.
   double nsx; //Number of documents in the subject area for weighting.
   double *wt; //weights of the process
   double *ax; //For alpha values.

  //Relate to the documents
   long nw; //Number of terms in a particular doc.
   long *nwd; //Term numbers of terms in the doc.
   Index *gdd; //Good docs
   Index *bdd; //Bad docs
   long ndoc; //Number of documents in whole space
   long tdoc; //Number of training documents 
      //good+bad
   double *dt; //Probability distribution
      //Gets this from GBoost2
   double eps; //Size used in weighting
      //to avoid zero or one as prob.
   double *sco;

   //Related to themes
   long ntrm; //Number of terms in theme
   long stng; //Stringency
   double xlim; //Threshhold for docs
   Index *cur; //Pointer at current select docs

   Dbinbase *pDnb;
   Postg<char> *pPsg;
};

}
#endif
