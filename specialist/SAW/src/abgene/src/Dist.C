#include <iostream>
#include <fstream>
#include <cstdlib>
#include <fcntl.h>
#include <sys/mman.h>
#include <cmath>
#include <cstring>
#include <cassert>
#include "Dist.h"

using namespace std;
namespace iret {


double lphi(double x){
   long i;
   double y,z,u,s,xi;

   if(fabs(x)<10.0){
      y=sqrt(2.0);
      if(x>=0)return(log((1.0+erf(x/y))/2.0));
      else return(log(erfc(-x/y)/2.0));
   }
   else if(x>0){
      y=sqrt(2.0*3.14159)*x;
      y=exp(-x*x/2.0)/y;
      z=1.0;
      u=-1.0/(x*x);
      s=1.0;
      for(i=1;i<6;i++){
        xi=(double)i;
        z=z*xi*u;
        s+=z;
      }
      return(log(1.0-y*s));
   }
   else {
      y=-x*x/2.0-log(2.0*3.14159)/2.0-log(fabs(x));
      z=1.0;
      u=-1.0/(x*x);
      s=1.0;
      for(i=1;i<6;i++){
        xi=(double)i;
        z=z*xi*u;
        s+=z;
      }
      return(y+log(s));
   }
}


LBin::LBin(long n){
   log_fac=new double[n+1];
   *log_fac=*(log_fac+1)=0;
   for(long i=2;i<n+1;i++)*(log_fac+i)=*(log_fac+i-1)+log((double)i);
}

LBin::~LBin() {
  if(log_fac) delete [] log_fac;
}

double LBin::combination(long n, long r){
     return(log_fac[n] - (log_fac[r] + log_fac[n-r]));
}

double LBin::log_binomial(long n, long r, double p){
     double x; 
     x = combination(n, r) + 1.0 * r*log(p);
     x= x + 1.0*(n-r)*log(1.0 -p);
     return (x);
}


double LBin::log_binomial_pval(long n, long r, double p){
     double x,y;
     long i;
     x= log_binomial(n, 0, p);
     for(i=1;i<=r;i++){ 
      y = log_binomial(n, i, p);
      x = addl(x,y);
     }
     return(x); 

}

double LBin::addl(double x,double y){
   double xt,u;
   u=x-y;
   if(u<=0){
      xt=exp(u);
      return(y+log(1.0+xt));
   }
   else {
      xt=exp(-1.0*u);
      return(x+log(1.0+xt));
   }
}

//Binomial Confidence Limit

Binomial::Binomial(double errs, double errp){
  eps=errs;
  epp=errp;
}

Binomial::~Binomial() {
}

void Binomial::combnr(long n,long r){
   long i; 
   double log_fac_N, log_fac_R, log_fac_diff;
   if(r<2)log_fac_R=0.0;
   if((n-r)<2) log_fac_diff=0.0;

   log_fac_N=0.0;
   if(n>=2){
        for(i=2;i<n+1;i++) {
           log_fac_N+=log((double)i);
           if(i==r) log_fac_R=log_fac_N;
           if(i==(n-r)) log_fac_diff = log_fac_N;
        }
   }
   com_N_R=log_fac_N - (log_fac_R + log_fac_diff);

}
 
double Binomial::binomial_pval_appx(long n, long r, double p){
     long i;
     double xx, yy,f;


     if(r) f=(1.0-p)/p;
     else {
       yy= exp(1.0*(n)*log(1.0 -p));
       return(yy);

     }

     yy=0.0;
     xx = com_N_R + 1.0 * r*log(p);
     xx= xx + 1.0*(n-r)*log(1.0 -p);
     xx= exp(xx);

     yy+=xx;
     i=r;
     while((i*xx)>=epp && i>0){
        xx=((i*f)/(double)(n-i+1))*xx;
        yy+=xx;
        i--;
     }
     return(yy);

}

double Binomial::upper_limit(long n, long r, double lim){
   long i;
   double p, qn, xx;
   double x,y,z;
   qn=lim;
   combnr(n,r);
   p=(double)r/(double)n;

   xx=this->binomial_pval_appx(n, r, p);
   if(xx<=qn){cout << "check the lim" << endl;return(xx);}
   if(n==r) return(p);
   x=p;
   y=1.0;
   while(y-x>eps){
      p=(y+x)/2;
      xx=this->binomial_pval_appx(n, r, p);
      if(qn<xx)x=p;
      else y=p;
   }
   return(x);
}

double Binomial::lower_limit(long n, long r, double lim){
   long i;
   double p,q,x;
   p=(double)r/(double)n;
   q=(double)(n-r)/(double)n;
   x=upper_limit(n,n-r,lim);
   return(p-x+q);
}

}

