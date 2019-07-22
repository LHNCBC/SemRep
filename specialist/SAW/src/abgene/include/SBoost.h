#ifndef SBOOST_H
#define SBOOST_H

#include <iostream>
#include <fstream>
#include <Isgrid.h>

using namespace std;
namespace iret {

class SBoost {
public:
  SBoost(long dm, long ln, Index *gind);
  //dim is dimention of the data, i.e. how many score arrays are to be 
  //combined.
  //len is the length of score arrays
  //gInd is the pointer to the good elements

  SBoost(long dm);
  //dim is dimention of the data, i.e. how many score arrays are to be 
  //combined.
  
  ~SBoost();
 
  void Setup(long i, double *sco);
  //Sets up scr through dim calls
 
  void Learn_Boost(const char* nm);
  void Load_Boost(const char* nm);
 
  double Score(double *scc);
  //Predicts the class

  //Data
  long dim;
  long len;//total number of points
  long n;//number of goods
  Index *gInd;
  double **scr;
  Isgrid **pIs;
  long grn; //Granularity, default 10,000
  double zprod; //Product of Zs
  double epsilon; //Limit on probabilities
  long t; //Number of the iteration.
  double epl;//Stopping Criteria for Optimization
};

class SABoost : public FBase {
public:
  SABoost(long dm, long ln, Index *gind, const char *nam);
  //dim is dimention of the data, i.e. how many score arrays are to be 
  //combined.
  //len is the length of score arrays
  //gInd is the pointer to the good elements
  //constructor to be used in train programs

  SABoost(long dm, const char *nam);
  //COnstructor to be used in test programs
  
  ~SABoost();
 
  void Setup(long i, double *sco);
  //Sets up scr through dim calls
 
  void Learn_Boost();
  void Load_Boost();
 
  double Score(double *scc);
  //Predicts the class

  double Z_alpha(double alp, long dm); //computes the value for alp.

  //Data
  long dim;
  long len;//total number of points
  long n;//number of goods
  Index *gInd;
  double **scr;
  double **alpha_h;
  double *alpha;
  double *label;
  double zprod; //Product of Zs
  double epsilon; //Limit on probabilities
  long t; //Number of the iteration.
  double epl;//Stopping Criteria for Optimization
};
}
#endif
