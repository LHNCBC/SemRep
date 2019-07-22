#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "Hyper.h"
using namespace std;
namespace iret {

Hyper::Hyper(long n) {
   nobj=n;

   log_fac=new double[n+1];
   *log_fac=*(log_fac+1)=0;
   for(long i=2;i<n+1;i++)*(log_fac+i)=*(log_fac+i-1)+log10((double)i);

   log_num = NULL;
}

Hyper::Hyper(long n,double epx) {
   long i;
   nobj=n;

   log_num=new double[n+1];
   for(i=1;i<n+1;i++)log_num[i]=log10((double)i);

   log_fac=new double[n+1];
   *log_fac=*(log_fac+1)=0;
   for(i=2;i<n+1;i++)*(log_fac+i)=*(log_fac+i-1)+log_num[i];

   if(epx>0)eps=-log(epx);
   else {cout << "Error, eps>0 is necessary!" << cout;exit(0);}
}


Hyper::~Hyper() {
  delete [] log_fac;
  if(log_num!=NULL)delete [] log_num;
}

double Hyper::nlog_pval(long n_st,long n_s, long n_t, long N){
   double x,y;
   long i,j,m;
  
   if(N>nobj){cout << "Error in size nobj = " << nobj << " and N = " << N << endl;exit(0);}
   m=(n_s<n_t)?n_s:n_t;
   x=log_prob(n_st,n_s,n_t,N);
   for(i=n_st+1;i<=m;i++){
      y=log_prob(i,n_s,n_t,N);
      x=addl(x,y);
   }
   return(-x);
}

double Hyper::nlog_pval_appx(long n_st,long n_s, long n_t, long N){
   double x,y,*mt;
   long i,j,m;
  
   if(N>nobj){cout << "Error in size nobj = " << nobj << " and N = " << N << endl;exit(0);}
   m=(n_s<n_t)?n_s:n_t;
   x=log_prob(n_st,n_s,n_t,N);
   if(n_st>=((double)n_s)*((double)n_t)/((double)N)){
      mt=log_num+m-n_st;
      for(i=n_st+1;i<=m;i++){
         y=log_prob(i,n_s,n_t,N);
         if(y+eps+*(mt--)>x){
            x=addl(x,y);
         }
         else break;
      }
      return(-x);
   }
   else return(this->nlog_pval(n_st,n_s,n_t,N));
}

double Hyper::log_prob(long n_st,long n_s, long n_t, long N){

return log_fac[n_s] + log_fac[n_t] + log_fac[N - n_s] + log_fac[N - n_t]
-log_fac[N] -log_fac[n_st]- log_fac[n_s - n_st] - log_fac[n_t -n_st]
- log_fac[N -n_s -n_t + n_st];
}

double Hyper::addl(double x,double y)
{
double xt,u;
u=x-y;
if(u<=0){
   xt=pow(10.0,u);
   return(y+log10(1.0+xt));
         }
else {
   xt=pow(10.0,-u);
   return(x+log10(1.0+xt));
      }
}

}
