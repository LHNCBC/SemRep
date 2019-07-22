#ifndef DIST_H
#define DIST_H

using namespace std;
namespace iret {

class LBin{
public:
   LBin(long n); //Allocate n double space 
   ~LBin();
   double combination(long n, long r); 
   //Compute log of the binomial cofficient
   double log_binomial(long n, long r, double p);
   //logarithm of one of term in the binomial expansion
   double log_binomial_pval(long n, long r, double p); 
   //p-value of the above 
   double addl(double x,double y);
   //Given x=log(X) and y=log(Y), compute log(X+Y)

 private:
   double *log_fac; //array for log(factorials).
};

class Binomial{
public:
   Binomial(double errs, double errp);
   //errs for searching error eps 
   //errp for p-value error epp
   ~Binomial();
   void combnr(long n, long r); 
   //computes log binomial coefficient 
   double binomial_pval_appx(long n, long r, double p); 
   //fast way compute the p-value
   double upper_limit(long N, long r, double sig);
   //N-number of trials
   //r-number of sucesses
   //sig-significance level
   double lower_limit(long N, long r, double sig);
 private:
  double eps;//error searching
  double epp;//error in p-value 
  double com_N_R;
};
 
}
#endif
