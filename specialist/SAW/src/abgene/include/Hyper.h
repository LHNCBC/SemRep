#ifndef HYPER_H
#define HYPER_H

#include <iostream>
#include <fstream>
using namespace std;
namespace iret {

class Hyper {
public:
  Hyper(long n);
     //The number n is the largest set size that will be used 
     //in calls to functions.
  Hyper(long n,double eps);
     //Like above except allows the approximate function 
     //to be used also.
  ~Hyper();
  double nlog_pval(long n_st,long n_s, long n_t, long N);
     //This returns the negative log10 of the pval that the 
     //overlap will be n_st or greater for the given subsets.
  double nlog_pval_appx(long n_st,long n_s, long n_t, long N);
     //This returns the negative log10 of the pval just
     //as previous function except pval allowed to be less by
     //factor of (1-eps). 
  double log_prob(long n_st,long n_s, long n_t, long N);
     //Returns the log10 of the probability that the overlap
     //will be exactly n_st for the two sets.
  double addl(double x,double y);
     //Returns the log of the sum of the two numbers whose
     //logs are input. All logs base 10.
private:
  long nobj; //total set size;
  double *log_fac; //array for log(factorials).
  double *log_num; //array for log(numbers).
  double eps; //Error factor for speed.
};
}
#endif
