#ifndef GBOOST_H
#define GBOOST_H

#include <iostream>
#include <fstream>
#include <DataObj.h>
#include <Isgrid.h>

using namespace std;
namespace iret {

class GBoost {
public:
  GBoost(long trd,long tsd,double eps); //trd # training docs
     //tsd # of test docs, eps the limit on p, eps <=p<=1-eps
  ~GBoost();
  void init(Index *gind,long gr=10000); //Initializes the pdoc array
     //Also sets the mark array for the good using gind
     //Also sets the granularity for Isgrid use.
  void update(double *trs,double *tss); //trs is pointer at
     //training score array from some function h & tss is
     //the same pointer at testing score array by same h.
  void update_store(double *trs,const char *nam); //trs is pointer at
     //at training score array. nam is name under which to store
     //the Isgrid results.

  //Data
  long tr_doc; //number of training docs
  long *mark; //Array that marks the good with 1, bad 0
  double *pdoc; //Probability array for the training docs

  long ts_doc; //number of testing docs
  double *ts_sxx; //Final output score for testing docs

  long grn; //Granularity, default 10,000.
  long t; //Number of the iteration.
  double zprod; //Product of Zs
  double epsilon; //Limit on p.
  Isgrid *pIsg; //Pointer at Isgrid object last used in update.
};

class GBoost2 {
public:
  GBoost2(long nd,Index *gd,Index *bd,Index *ts); //ndoc whole space
     //good docs, bad docs, test docs
  ~GBoost2();
  void init(double eps,long gr=10000); //Initializes the pdoc 
     // and ts_sxx arrays
     //eps the limit on p, eps <=p<=1-eps
  void update(double *sco); 
     //score array from some function h, over whole space
  void update(float *sco); 
     //score array from some function h, over whole space
  void update_store(double *sco, const char *nam); //sco is pointer at
     //score array. nam is name under which to store
     //the Isgrid results.

  //Data
  double *pdoc; //Probability array for the training docs

  long ndoc; //number of docs in whole space
  long tdoc; //number of docs in training set
  double *ts_sxx; //Final output score for testing docs
     //array covers all of ndoc, same as sco in update

  Index *gdd; //Good set
  Index *bdd; //Bad set
  Index *tst; //Test set

  long grn; //Granularity, default 10,000.
  long t; //Number of the iteration.
  double zprod; //Product of Zs
  double epsilon; //Limit on p.
  Isgrid *pIsg; //Pointer at Isgrid object last used in update.
};

class ABoost {
public:
  ABoost(long trd,long tsd); //trd # training docs
     //tsd # of test docs
  ~ABoost();
  void init(Index *gind); //Initializes the pdoc array
     //Also sets the mark array for the good using gind
  double update(double *trs,double *tss); //trs is pointer at
     //training score array from some function h & tss is
     //the same pointer at testing score array by same h.
     //Returns the optimal alpha value used in update.
  double update(double *trs); //trs is pointer at
     //training score array from some function h 
     //Returns the optimal alpha value used in update.
  double Z_alpha(double alp); //computes the value for alp.

  //Data
  long tr_doc; //number of training docs
  long *mark; //Array that marks the good with 1, bad 0
  double *tr_sco; //Array of training scores.
  double *pdoc; //Probability array for the training docs

  long ts_doc; //number of testing docs
  double *ts_sxx; //Final output score for testing docs

  long t; //Number of the iteration.
  double zprod; //Product of Zs
};

class ABoost2 {
public:
  ABoost2(long nd,Index *gd,Index *bd,Index *ts); //ndoc whole space
     //good docs, bad docs, test docs
  ~ABoost2();
  void init(void); //Initializes the pdoc
     // and ts_sxx arrays
  double update(double *sco);
     //score array from some function h, over whole space
     //Returns the optimal alpha value used in update.
  double update(float *sco);
  double Z_alpha(double alp); //computes the value for alp.
  double Z_alphf(double alp); //computes the value for alp.
     //Uses scf instead of sco

  //Data
  double *pdoc; //Probability array for the training docs
  double *sco; //Array of scores
  float *scf; //Alternative array of scores

  long ndoc; //number of docs in whole space
  long tdoc; //number of docs in training set
  double *ts_sxx; //Final output score for testing docs
     //array covers all of ndoc, same as sco in update

  Index *gdd; //Good set
  Index *bdd; //Bad set
  Index *tst; //Test set

  long t; //Number of the iteration.
  double zprod; //Product of Zs
};

class ABoost3 {
public:
  ABoost3(long nd,Index *gd,Index *bd,Index *ts); //ndoc whole space
     //good docs, bad docs, test docs
  ~ABoost3();
  void init(void); //Initializes the pdoc
     // and ts_sxx arrays

  double update(double *sco);
     //score array from some function h, over whole space
     //Returns the optimal alpha value used in update.
  double find_alpha(double alp,double &hnv); //Calls Z_alpha and finds min. point
  double find_beta(double bet,double &hnv);  //Calls Z_beta and finds min. point

  double update(float *sco);
  double Z_alpha(double alp); //computes the value for alp.
  double Z_alphf(double alp); //computes the value for alp.
     //Uses scf instead of sco
  double Z_beta(double bet); //computes the value for bet.
  double Z_betf(double bet); //computes the value for bet.
     //Uses scf instead of sco

  //Data
  double *pdoc; //Probability array for the training docs
  double *sco; //Array of scores
  float *scf; //Alternative array of scores
  double alpha;
  double beta;

  long ndoc; //number of docs in whole space
  long tdoc; //number of docs in training set
  double *ts_sxx; //Final output score for testing docs
     //array covers all of ndoc, same as sco in update

  Index *gdd; //Good set
  Index *bdd; //Bad set
  Index *tst; //Test set

  long t; //Number of the iteration.
  double zprod; //Product of Zs
};


}
#endif
