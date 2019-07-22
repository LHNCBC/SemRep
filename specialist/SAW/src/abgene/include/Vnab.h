#ifndef VNAB_H
#define VNAB_H

#include <iostream>
#include <fstream>
#include <DataObj.h>
#include <Docum.h>
#include <Post.h>
#include <Mang.h>
using namespace std;
namespace iret {

const double l2=1.0/log(2.0),dmt=20.0,lfac=log(0.65);
const double lfab=log(0.7);

class Vterm { //Holds the postings information including precalculated values 
              //for a single term.
   public:
      Vterm(){};
      ~Vterm();
      float gwgt; //global term weight.
      Index *post; //Postings list.
      float *cwgt; //Precomputed weight factors for term occurrences in the 
         //postings list.
};

class Vnab {
   public:
      Vnab(Docum *dcc); //Uses the standard files from docset 
         //Must call dcc->gopen_map(arg) before passing dcc to this constructor
         //arg must include READ_W and READ_M and may include READ_S (and READ_U)
      ~Vnab();
      void load(Post *pct,float (*d_local)(int,long),float (*global)(long)); 
         //Loads in the postings using Vterm. Sets *cwgt=(d_local)*(global).

      //Before these functions are called it is assumed that load will have
      //been called as load(pct,d_lc_func,global_const). This sets the cwgt
      //array to the local counts of terms and is used by veclen & reload.
      void veclen(float (*d_local)(int,long),float (*global)(long));
         //Computes and stores the vector length of documents in vln.
         //Does not change Vterm's.
      void reload(float (*d_local)(int,long),float (*global)(long));
         //Reloads the cwgt array based on current values and vln.  
         //Also reloads gwgt from global weight function.

      void Score(long nd,float (*q_local)(int,long)); 
         //Scores all against document index #nd.
      void Score(Docum &Doc,float (*q_local)(int,long));
         //Same as above function except scores all against an outside document.
      float Normalize(Docum &Doc,float (*q_local)(int,long),float (*global)(long));
      float Normal_Score(Docum &Dc1,Docum &Dc2,float (*q_local)(int,long),float (*global)(long));

      long ndoc; //Number of documents in the database.
      long nwrd; //Number of terms in the database.
      float *sxx; //Array used for scoring all the documents in the database.
      float *vln; //Array for vector lengths of documents in database.
      Docum *doc; //Holds the Docum object that is used by the class.
      Btree *btr; //Btree containing all the index terms in the database and all
                  //postings data as Vterms.
};

class Vnak : public Manf {
   public:
      Vnak(const char *namdbn,const char *nampsg); //Assumes the Dbinbase and
         //Postg<float> are already made and may have different names.
      Vnak(const char *namdoc); //Assumes the Dbinbase and Postg<float> are
         //already made and have the same name namdoc.
      ~Vnak(void);

      //Build the precalculated retrieval data as float in same format as the
      //Postg<float> weight data and in Postg file space.
      void pre_normz(float (*global)(long));  //Normalized
      void pre_unorm(float (*global)(long)); //Unnormalized
 
      //Open for operation
      void gopen_operate(void);
      void gclose_operate(void);
      
      //Scoring, scores all of whole database.
      void Score(long nd);
      void Screen(long llim,long nd);
         //llim used as lower limit to judge a trigram important and increase score.
         //As a result the score will be larger than 1.0 if such a trigram appears in
         //the document (unless the document has many trigrams that are not low freq).
      void Score_un(Docum &Doc,float (*q_local)(int,long));
      void Score_nz(Docum &Doc,float (*q_local)(int,long),float (*global)(long));
      void Screen_nz(long llim,Docum &Doc,float (*q_local)(int,long),float (*global)(long));
         //llim used as lower limit to judge a trigram important and increase score.
         //As a result the score will be larger than 1.0 if such a trigram appears in
         //the document (unless the document has many trigrams that are not low freq).

      //Data
      float *sxx; //Score array for whole database
      float *rfil; //for memory map of pre-calc.
};

//Local Weighting Functions

float q_const(int lc,long n); //constant 1.0

float d_lc_func(int lc,long n);  //Just returns lc

float d_lc_ratio(int lc,long n); //simple ratio lc/(2+lc)

float d_lc_log(int lc,long n); //simple ln(lc+1)

   //Trec local functions.
float d_len_trec(int lc,long n); //func of len only

float d_inquery_trec(int lc,long n); //Inquery formula (Robertson)

float d_robertson_trec(int lc,long n); //Robertson

float d_wilbur_trec(int lc,long n); //Wilbur Poisson formula

float d_experiment_trec(int lc,long n); //Experimental

   //MED local functions
float d_inquery_med(int lc,long n); //Inquery formula (Robertson)

float d_wilbur_med(int lc,long n); //Wilbur Poisson formula

//Global Weighting Functions

float global_idf(long n); //IDF 

float global_const(long n); //Constant 1.0

}
#endif
