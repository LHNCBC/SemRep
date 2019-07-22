#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "runn.h"
#include "Istreg.h"
using namespace std;
namespace iret {

#define LNEG -1.0E299;

Istreg::Istreg(void) : FBase("istreg","null"){
   num=0;
   xr=NULL;
   mem_r=0;
   xs=NULL;
   mem_s=0;
   xd=NULL;
   xn=NULL;
   p=NULL;
   eps=1.0E-12;
   slim=10000;
   delt=30;
}

Istreg::Istreg(const char *nam) : FBase("istreg",nam){
   num=0;
   xr=NULL;
   mem_r=0;
   xs=NULL;
   mem_s=0;
   xd=NULL;
   xn=NULL;
   p=NULL;
   eps=1.0E-12;
   slim=10000;
   delt=30;
}

Istreg::Istreg(const char *nam,double xeps) : FBase("istreg",nam){
   num=0;
   xr=NULL;
   mem_r=0;
   xs=NULL;
   mem_s=0;
   xd=NULL;
   xn=NULL;
   p=NULL;
   eps=xeps;
   slim=10000;
   delt=30;
}

Istreg::Istreg(const char *nam,double xeps,long xlim,long angl) : FBase("istreg",nam){
   num=0;
   xr=NULL;
   mem_r=0;
   xs=NULL;
   mem_s=0;
   xd=NULL;
   xn=NULL;
   p=NULL;
   eps=xeps;
   slim=xlim;
   delt=angl;
}

Istreg::~Istreg(){
   if(mem_r&&xr!=NULL)delete [] xr;
   if(mem_s&&xs!=NULL)delete [] xs;
   if(p!=NULL)delete [] p;
   }

void Istreg::set_data(long n,double *x,double *sd, double *sn){
   num=n;
   if(mem_r&&xr!=NULL)delete [] xr;
   xr=x;
   mem_r=0;
   xs=NULL;
   xd=sd;
   xn=sn;
}

void Istreg::set_data(long n,double *x,double *y,double *sd, double *sn){
   num=n;
   if(mem_r&&xr!=NULL)delete [] xr;
   xr=x;
   mem_r=0;
   if(mem_s&&xs!=NULL)delete [] xs;
   xs=y;
   mem_s=0;
   xd=sd;
   xn=sn;
}

void Istreg::dim1(int ser){
   long *idx=new long[num];
   for(long i=0;i<num;i++)*(idx+i)=i;
   if(p!=NULL)delete [] p;
   p=new double[num];

   if(ser)sco=xs;
   else sco=xr;

   //Sort in simple order.
   this->sSort(num,idx);

   this->padjv(idx);

   delete [] idx;
}

void Istreg::dim1_ord(int ser){
   long *idx=new long[num];
   for(long i=0;i<num;i++)*(idx+i)=i;
   if(p!=NULL)delete [] p;
   p=new double[num];

   if(ser)sco=xs;
   else sco=xr;

   this->padjv(idx);

   delete [] idx;
}

void Istreg::dim2(void){
   long *idx=new long[num];
   for(long i=0;i<num;i++)*(idx+i)=i;
   if(p!=NULL)delete [] p;
   p=new double[num];

   //Sort in lexical order.
   this->dSort(num,idx);

   this->split(num,idx);
}

void Istreg::dim2_redux(void){
   long *idx=new long[num];
   for(long i=0;i<num;i++)*(idx+i)=i;
   if(p!=NULL)delete [] p;
   p=new double[num];

   //Sort in lexical order.
   this->dSort(num,idx);

   this->split_redux(num,idx);
}

void Istreg::split(long npp,long *idx){
   double sum_n=0,sum_d=0,pav;
   double ps,ts,smax,rmax,x;
   long i,j,k,kp;

   smax=*(xs+*idx);
   rmax=*(xr+*idx);
   for(i=0;i<npp;i++){
      j=*(idx+i);
      x=*(xs+j);
      smax=(smax<x)?x:smax;
      x=*(xr+j);
      rmax=(rmax<x)?x:rmax;
      sum_n+=*(xn+j);
      sum_d+=*(xd+j);
   }
   smax+=smax;
   rmax+=rmax;
   pav=sum_n/sum_d;

   //If there is only one element solution is trivial.
   if(npp==1){
      *(p+*(idx+0))=pav;
      delete [] idx;
      return;
   }

   //If the value is extreme the solution is trivial.
   if(sum_n==0){
      for(i=0;i<npp;i++)*(p+*(idx+i))=0.0;
      delete [] idx;
      return;
   }
   else if(sum_n==sum_d){
      for(i=0;i<npp;i++)*(p+*(idx+i))=1.0;
      delete [] idx;
      return;
   }

   //Initialize values.
   double *a=new double[npp];
   double *t=new double[npp];
   long  *b=new long[npp];

   for(i=0;i<npp;i++){
      j=*(idx+i);
      *(a+i)=*(xn+j)-*(xd+j)*pav;
      *(t+i)=0.0;
   }
   //Enter loop to optimize an upper set
   long m,n;
   double u,v,a1,a2,pn;
   double os,tt,w;
   kp=-1;
   tt=0;
   i=0;
   while(i<npp){
      a1=*(a+i);
      if(a1>0){
         m=*(idx+i);
         u=*(xr+m);
         os=*(xs+m);
         ps=0;
         pn=a1;
         ts=LNEG;
         k=-1;
         j=i-1;
         while(j>=0){
            n=*(idx+j);
            v=*(xr+n);
            if(*(xs+n)>os){
               ps+=pn;
               pn=0;
               os=*(xs+n);
            }
            a2=*(a+j);
            if(u<=v)pn+=a2;
            else if(a2>0){
               w=ps+*(t+j);
               if(w>ts){
                  ts=w;
                  k=j;
               }
            }
            j--;
         }
         ps+=pn;
         if(ps>ts){
            ts=ps;
            k=-1;
         }
         *(t+i)=ts;
         *(b+i)=k;
         if(ts>tt){
            tt=ts;
            kp=i;
         }
      }
      i++;
   }

   //If zero max, terminate process.
   if(tt<eps){
      for(i=0;i<npp;i++)*(p+*(idx+i))=pav;
      delete [] a;
      delete [] t;
      delete [] b;
      delete [] idx;
      return;
   }

   //Otherwise perform the split.
   long nm1=0,nm2=0;
   i=npp-1;
   j=kp;
   m=*(idx+j);
   u=*(xs+m);
   v=rmax;
   while(i>=0){
      n=*(idx+i);
      a1=*(xs+n);
      a2=*(xr+n);
      if(a1<u){
         if(a2<v)nm2++;
         else nm1++;
      }
      else {
         while(a1>=u){
            x=*(xr+m);
            v=(x<v)?x:v;
            j=*(b+j);
            if(j>=0){
               m=*(idx+j);
               u=*(xs+m);
            }
            else u=smax;
         }
         if(a2<v)nm2++;
         else nm1++;
      }
      i--;
   }
   cout << nm2 << " " << nm1 << " " << npp << endl;

   //If zero split, terminate process.
   if(nm1==0 || nm2==0){
      for(i=0;i<npp;i++)*(p+*(idx+i))=pav;
      delete [] a;
      delete [] t;
      delete [] b;
      delete [] idx;
      return;
   }

   long *id1=new long[nm1];
   long *id2=new long[nm2];
   long nn1=0,nn2=0;
   i=npp-1;
   j=kp;
   m=*(idx+j);
   u=*(xs+m);
   v=rmax;
   while(i>=0){
      n=*(idx+i);
      a1=*(xs+n);
      a2=*(xr+n);
      if(a1<u){
         if(a2<v)*(id2+nm2-(++nn2))=n;
         else *(id1+nm1-(++nn1))=n;
      }
      else {
         while(a1>=u){
            x=*(xr+m);
            v=(x<v)?x:v;
            j=*(b+j);
            if(j>=0){
               m=*(idx+j);
               u=*(xs+m);
            }
            else u=smax;
         }
         if(a2<v)*(id2+nm2-(++nn2))=n;
         else *(id1+nm1-(++nn1))=n;
      }
      i--;
   }

   //Free memory.
   delete [] a;
   delete [] t;
   delete [] b;
   delete [] idx;

   //Make recursive calls
   this->split(nm1,id1);
   this->split(nm2,id2);
}

void Istreg::simple_split(double pav,long npp,long *idx,long &nm2,pLong &id2,long &nm1,pLong &id1){
   double sum_n=0,sum_d=0;
   double ps,ts,smax,rmax,x;
   long i,j,k,kp;

   smax=*(xs+*idx);
   rmax=*(xr+*idx);
   for(i=0;i<npp;i++){
      j=*(idx+i);
      x=*(xs+j);
      smax=(smax<x)?x:smax;
      x=*(xr+j);
      rmax=(rmax<x)?x:rmax;
      sum_n+=*(xn+j);
      sum_d+=*(xd+j);
   }
   smax+=smax;
   rmax+=rmax;

   //If there is only one element solution is trivial.
   if(npp==1){
      if(sum_n/sum_d>=pav){
         nm1=1;
         id1=idx;
         nm2=0;
      }
      else {
         nm2=1;
         id2=idx;
         nm1=0;
      }
      return;
   }

   //If the value is extreme the solution is trivial.
   if(sum_n==0){
      nm2=npp;
      id2=idx;
      nm1=0;
      return;
   }
   else if(sum_n==sum_d){
      nm1=npp;
      id1=idx;
      nm2=0;
      return;
   }

   //Initialize values.
   double *a=new double[npp];
   double *t=new double[npp];
   long  *b=new long[npp];

   for(i=0;i<npp;i++){
      j=*(idx+i);
      *(a+i)=*(xn+j)-*(xd+j)*pav;
      *(t+i)=0.0;
   }
   //Enter loop to optimize an upper set
   long m,n;
   double u,v,a1,a2,pn;
   double os,tt,w;
   kp=-1;
   tt=0;
   i=0;
   while(i<npp){
      a1=*(a+i);
      if(a1>0){
         m=*(idx+i);
         u=*(xr+m);
         os=*(xs+m);
         ps=0;
         pn=a1;
         ts=LNEG;
         k=-1;
         j=i-1;
         while(j>=0){
            n=*(idx+j);
            v=*(xr+n);
            if(*(xs+n)>os){
               ps+=pn;
               pn=0;
               os=*(xs+n);
            }
            a2=*(a+j);
            if(u<=v)pn+=a2;
            else if(a2>0){
               w=ps+*(t+j);
               if(w>ts){
                  ts=w;
                  k=j;
               }
            }
            j--;
         }
         ps+=pn;
         if(ps>ts){
            ts=ps;
            k=-1;
         }
         *(t+i)=ts;
         *(b+i)=k;
         if(ts>tt){
            tt=ts;
            kp=i;
         }
      }
      i++;
   }

   //If zero max, terminate process.
   if(tt<eps){
      delete [] a;
      delete [] t;
      delete [] b;
      if(sum_n/sum_d>=pav){
         nm1=npp;
         nm2=0;
         id1=idx;
      }
      else {
         nm1=0;
         nm2=npp;
         id2=idx;
      }
      return;
   }

   //Otherwise perform the split.
   nm1=nm2=0;
   i=npp-1;
   j=kp;
   m=*(idx+j);
   u=*(xs+m);
   v=rmax;
   while(i>=0){
      n=*(idx+i);
      a1=*(xs+n);
      a2=*(xr+n);
      if(a1<u){
         if(a2<v)nm2++;
         else nm1++;
      }
      else {
         while(a1>=u){
            x=*(xr+m);
            v=(x<v)?x:v;
            j=*(b+j);
            if(j>=0){
               m=*(idx+j);
               u=*(xs+m);
            }
            else u=smax;
         }
         if(a2<v)nm2++;
         else nm1++;
      }
      i--;
   }
   cout << nm2 << " " << nm1 << " " << npp << endl;

   //If zero split, terminate process.
   if(nm1==0 || nm2==0){
      delete [] a;
      delete [] t;
      delete [] b;
      if(nm1==0)id2=idx;
      else id1=idx;
      return;
   }

   id1=new long[nm1];
   id2=new long[nm2];
   long nn1=0,nn2=0;
   i=npp-1;
   j=kp;
   m=*(idx+j);
   u=*(xs+m);
   v=rmax;
   while(i>=0){
      n=*(idx+i);
      a1=*(xs+n);
      a2=*(xr+n);
      if(a1<u){
         if(a2<v)*(id2+nm2-(++nn2))=n;
         else *(id1+nm1-(++nn1))=n;
      }
      else {
         while(a1>=u){
            x=*(xr+m);
            v=(x<v)?x:v;
            j=*(b+j);
            if(j>=0){
               m=*(idx+j);
               u=*(xs+m);
            }
            else u=smax;
         }
         if(a2<v)*(id2+nm2-(++nn2))=n;
         else *(id1+nm1-(++nn1))=n;
      }
      i--;
   }

   //Free memory.
   delete [] a;
   delete [] t;
   delete [] b;
   delete [] idx;
}

void Istreg::split_redux(long npp,long *idx){
   double sum_n=0,sum_d=0,pav;
   long i,j,k;
   int pflag=get_qflag();

   if(npp<slim){
      this->split(npp,idx);
      return;
   }

   for(i=0;i<npp;i++){
      j=*(idx+i);
      sum_n+=*(xn+j);
      sum_d+=*(xd+j);
   }
   pav=sum_n/sum_d;

   //If the value is extreme the solution is trivial.
   if(sum_n==0){
      for(i=0;i<npp;i++)*(p+*(idx+i))=0.0;
      delete [] idx;
      return;
   }
   else if(sum_n==sum_d){
      for(i=0;i<npp;i++)*(p+*(idx+i))=1.0;
      delete [] idx;
      return;
   }

   //Initialize values.
   double *a=new double[npp];
   long  *mrk=new long[npp];

   for(i=0;i<npp;i++){
      j=*(idx+i);
      *(a+i)=*(xn+j)-*(xd+j)*pav;
      *(mrk+i)=0;
   }

   //Loop to accumulate p-solid and n-solid sets
   double aa,xx,yy,xa,pl,ph;
   long difo,difx,df,n,ix,jx,nt1,nt2;
   long neg,pos,*idn=NULL,*idp=NULL,im;
   int flag,theta;
   long *id1=new long[npp];
   long *id2=new long[npp];
   long *imd=new long[npp];
   long *ihd=new long[npp];
   sco=new double[npp];
   im=npp;
   do {
      flag=0;
      for(theta=0;theta<=90;theta+=delt){
         xa=2.0*M_PI*theta/360.0;
         xx=cos(xa);
         yy=sin(xa);
         j=0;
         for(i=0;i<npp;i++){
            if(!*(mrk+i)){
               k=*(idx+i);
               *(ihd+j)=*(imd+j)=k;
               *(sco+j++)=*(xr+k)*xx+*(xs+k)*yy;
            }
         }
         if(j!=im){cout << "Error in counting!" << endl;exit(0);}
         //Sort in simple order.
         if(im){
            this->rSort(im,imd);
            this->padjv_redux(im,imd,pav,pl,ph);
         }
         nt1=nt2=0;
         for(i=0;i<im;i++){
            j=*(ihd+i);
            aa=*(p+j);
            if(ph<=aa)*(id2+nt2++)=j;
            else if(aa<=pl)*(id1+nt1++)=j;
         }   
         if((nt1)&&(nt1<100000)){
            if(nt1>slim)cout << "direct split of " << nt1 << endl;
            this->simple_split(pav,nt1,id1,neg,idn,pos,idp);
            id1=new long[npp];
            if((!nt2)&&(!neg)){
               neg=pos;
               pos=0;
               idn=idp;
               idp=NULL;
            }
         }
         else {neg=pos=0;}
         //Mark new n-solid set.
         n=0;
         for(i=0;i<neg;i++){
            ix=*(idn+i);
            while(*(idx+n)!=ix)n++;
            *(mrk+n)=-1;
            im--;
         }
         cout << "neg " << neg << endl;
         if(neg){delete [] idn;flag=1;}
         if(pos)delete [] idp;

         if((nt2)&&(nt2<100000)){
            if(nt2>slim)cout << "direct split of " << nt2 << endl;
            this->simple_split(pav,nt2,id2,neg,idn,pos,idp);
            id2=new long[npp];
            if((!nt1)&&(!pos)){
               pos=neg;
               neg=0;
               idp=idn;
               idn=NULL;
            }
         }
         else {neg=pos=0;}
         //Mark new p-solid set.
         n=0;
         for(i=0;i<pos;i++){
            ix=*(idp+i);
            while(*(idx+n)!=ix)n++;
            *(mrk+n)=1;
            im--;
         }
         cout << "pos " << pos << endl;
         if(neg)delete [] idn;
         if(pos){delete [] idp;flag=1;}
      }
   }while((flag)&&(im));

   //Split the remnant.
   if(im){
      cout << "split remnant " << im << endl;
      j=0;
      for(i=0;i<npp;i++){
         if(!*(mrk+i))*(imd+(j++))=*(idx+i);
      }
      if(j!=im){cout << "Error in remnant!" << endl;exit(0);}
      this->simple_split(pav,im,imd,neg,idn,pos,idp);
      n=0;
      for(i=0;i<neg;i++){
         ix=*(idn+i);
         while(*(idx+n)!=ix)n++;
         *(mrk+n)=-1;
      }
      n=0;
      for(i=0;i<pos;i++){
         ix=*(idp+i);
         while(*(idx+n)!=ix)n++;
         *(mrk+n)=1;
      }
      if(neg)delete [] idn;
      if(pos)delete [] idp;
   }
   else delete [] imd;
   delete [] id1;
   delete [] id2;

   //Find set sizes and positive sum.
   nt1=nt2=0;
   aa=0;   
   for(i=0;i<npp;i++){
      if(*(mrk+i)==1){nt1++;aa+=*(a+i);}
      else if(*(mrk+i)==-1)nt2++;
      else {cout << "Error in mark array!" << endl;exit(0);}
   }

   //If zero max, terminate process.
   if(aa<eps){
      for(i=0;i<npp;i++)*(p+*(idx+i))=pav;
      delete [] a;
      delete [] sco;
      delete [] mrk;
      delete [] ihd;
      delete [] idx;
      return;
   }

   //Otherwise perform the split.

   cout << "redux split " << nt2 << " " << nt1 << " " << npp << endl;

   //If zero split, terminate process.
   if(nt1==0 || nt2==0){
      for(i=0;i<npp;i++)*(p+*(idx+i))=pav;
      delete [] a;
      delete [] sco;
      delete [] mrk;
      delete [] ihd;
      delete [] idx;
      return;
   }

   id1=new long[nt1];
   id2=new long[nt2];
   long nn1=0,nn2=0;
   for(i=0;i<npp;i++){
      if(*(mrk+i)==1)*(id1+nn1++)=*(idx+i);
      else *(id2+nn2++)=*(idx+i);
   }

   //Free memory.
   delete [] a;
   delete [] sco;
   delete [] mrk;
   delete [] ihd;
   delete [] idx;

   //Make recursive calls
   this->split_redux(nt1,id1);
   this->split_redux(nt2,id2);
}

double Istreg::avg(){
   long i;
   double sum_n=0,sum_d=0,pav;

   for(i=0;i<num;i++){
      sum_n+=*(xn+i);
      sum_d+=*(xd+i);
   }
   if(sum_d>0)pav=sum_n/sum_d;
   else pav=0;
   
   return(pav);
}

double Istreg::info(){
   long i;
   double sum=0,l2=1.0/log(2.0),z;
   double pav=this->avg();

   for(i=0;i<num;i++){
      if((z=*(p+i))>0){
         sum+=*(xd+i)*z*l2*log(z/pav);
      }
   }
   
   return(sum);
}

double Istreg::sigma(){
   long i;
   double sum=0,sum_d=0,xx,yy;
   double pav=this->avg(),z;

   for(i=0;i<num;i++){
      xx=*(xn+i);
      yy=*(xd+i);
      sum_d+=yy;
      z=xx/yy-pav;
      sum+=yy*z*z;
   }
   return(sqrt(sum/sum_d));
}

double Istreg::rms(){
   long i;
   double sum=0,sum_d=0,xx,yy,z;

   for(i=0;i<num;i++){
      xx=*(xn+i);
      yy=*(xd+i);
      sum_d+=yy;
      z=xx/yy-*(p+i);
      sum+=yy*z*z;
   }
   return(sqrt(sum/sum_d));
}

int Istreg::lexcomp(const long s,const long t){
   if(xs[s]<xs[t])return(1);
   else if(xs[s]>xs[t])return(-1);
   else {
      if(xr[s]<xr[t])return(1);
      else if(xr[s]>xr[t])return(-1);
      else return(0);
   }
}

void Istreg::dSort(const long n, long *ra) {
  long k, j, ir, i;
  long rra;

  k=(n>>1);
  ir=n-1;
  for(;;) {
    if(k>0) {
      rra=ra[--k];
    }
    else {
      rra=ra[ir];
      ra[ir] = ra[0];
      if(--ir ==0) {
        ra[0]=rra;
        return;
      }
    }
    i=k;
    j=((k+1)<<1)-1;
    while(j<=ir) {
      if(j<ir && (lexcomp(ra[j],ra[j+1])<0)) ++j;
      if(lexcomp(rra,ra[j])<0) {
        ra[i]=ra[j];
        j +=(i=j)+1;
      }
      else j=ir+1;
    }
    ra[i]=rra;
  }
}

void Istreg::sSort(const long n,long *ra){
  long k, j, ir, i;
  long rra;

  k=(n>>1);
  ir=n-1;
  for(;;) {
    if(k>0) {
      rra=ra[--k];
    }
    else {
      rra=ra[ir];
      ra[ir] = ra[0];
      if(--ir ==0) {
        ra[0]=rra;
        return;
      }
    }
    i=k;
    j=((k+1)<<1)-1;
    while(j<=ir) {
      if(j<ir && (sco[ra[j]]<sco[ra[j+1]])) ++j;
      if(sco[rra]<sco[ra[j]]) {
        ra[i]=ra[j];
        j +=(i=j)+1;
      }
      else j=ir+1;
    }
    ra[i]=rra;
  }
}

void Istreg::rSort(const long n, long *rb) {
  long k, j, ir, i, rrb;
  double rra;

  if(n<=1)return;

  k=(n>>1);
  ir=n-1;
  for(;;) {
    if(k>0) {
      rra=sco[--k];
      rrb=rb[k];
    }
    else {
      rra=sco[ir];
      rrb=rb[ir];
      sco[ir] = sco[0];
      rb[ir] = rb[0];
      if(--ir ==0) {
        sco[0]=rra;
        rb[0]=rrb;
        return;
      }
    }
    i=k;
    j=((k+1)<<1)-1;
    while(j<=ir) {
      if(j<ir && sco[j] < sco[j+1]) ++j;
      if(rra<sco[j]) {
        sco[i]=sco[j];
        rb[i]=rb[j];
        j +=(i=j)+1;
      }
      else j=ir+1;
    }
    sco[i]=rra;
    rb[i]=rrb;
  }
}

void Istreg::padjv(long *ord){
long i,j;
int flag;
double xx,pav;

long last,next;
long *bg=new long[num];
long *nd=new long[num];
long *pin=new long[num];
double *sd=new double[num];
double *sn=new double[num];
//Construct linked list with consolidation by score.
*(bg)=0;
*(nd)=1;
*(pin)=1;
j=*ord;
*(sd)=*(xd+j);
*(sn)=*(xn+j);
last=0;
xx=*(sco+j);
for(i=1;i<num;i++){
   j=*(ord+i);
   if(*(sco+j)>xx){
      xx=*(sco+j);
      next=*(pin+last);
      *(sd+next)=*(xd+j);
      *(sn+next)=*(xn+j);
      *(bg+next)=*(nd+last);
      *(nd+next)=i+1;
      *(pin+next)=next+1;
      last=next;
   }
   else {
      *(nd+last)=i+1;
      (*(sd+last))+=*(xd+j);
      (*(sn+last))+=*(xn+j);
   }
}
*(pin+last)=0;

//Apply pool adjacent violators.
flag=1;
while(flag){
   flag=0;
   last=0;
   next=*(pin+last);
   while(next){
      if(*(sn+last)*(*(sd+next))>=*(sd+last)*(*(sn+next))){
         (*(sn+last))+=*(sn+next);
         (*(sd+last))+=*(sd+next);
          *(nd+last)=*(nd+next);
          next=*(pin+last)=*(pin+next);
          flag=1;
      }
      else {
         last=next;
         next=*(pin+last);
      }
   }
}

//Produce monotonic probability values.
pav=*(sn)/(*(sd));
for(i=*(bg);i<*(nd);i++){
   *(p+*(ord+i))=pav;
}
next=*(pin);
while(next){
   pav=*(sn+next)/(*(sd+next));
   for(i=*(bg+next);i<*(nd+next);i++){
      *(p+*(ord+i))=pav;
   }
   last=next;
   next=*(pin+next);
}
delete [] bg;
delete [] nd;
delete [] pin;
delete [] sd;
delete [] sn;
}


void Istreg::padjv_redux(long npp,long *ord,double xpv,double &pl,double &ph){
long i,j,beg,nnd;
int flag,fl,fh;
double xx,pav;

long last,next;
long *bg=new long[npp];
long *nd=new long[npp];
long *pin=new long[npp];
double *sd=new double[npp];
double *sn=new double[npp];
//Construct linked list with consolidation by score.
*(bg)=0;
*(nd)=1;
*(pin)=1;
j=*ord;
*(sd)=*(xd+j);
*(sn)=*(xn+j);
last=0;
xx=*sco;
for(i=1;i<npp;i++){
   j=*(ord+i);
   if(*(sco+i)>xx){
      xx=*(sco+i);
      next=*(pin+last);
      *(sd+next)=*(xd+j);
      *(sn+next)=*(xn+j);
      *(bg+next)=*(nd+last);
      *(nd+next)=i+1;
      *(pin+next)=next+1;
      last=next;
   }
   else {
      *(nd+last)=i+1;
      (*(sd+last))+=*(xd+j);
      (*(sn+last))+=*(xn+j);
   }
}
*(pin+last)=0;

//Apply pool adjacent violators.
flag=1;
while(flag){
   flag=0;
   last=0;
   next=*(pin+last);
   while(next){
      if(*(sn+last)*(*(sd+next))>=*(sd+last)*(*(sn+next))){
         (*(sn+last))+=*(sn+next);
         (*(sd+last))+=*(sd+next);
          *(nd+last)=*(nd+next);
          next=*(pin+last)=*(pin+next);
          flag=1;
      }
      else {
         last=next;
         next=*(pin+last);
      }
   }
}

//Produce monotonic probability values.
fl=0;
fh=1;
pl=-1.0;
ph=2.0;
pav=*(sn)/(*(sd));
for(i=*(bg);i<*(nd);i++){
   *(p+*(ord+i))=pav;
}
if(pav<xpv){
   pl=pav;
   fl=1;
}
if(pav>xpv){
   ph=pav;
}
next=*(pin);
while(next){
   beg=*(bg+next);
   nnd=*(nd+next);
   pav=*(sn+next)/(*(sd+next));
   for(i=beg;i<nnd;i++){
      *(p+*(ord+i))=pav;
   }
   if(pav<xpv){
      if(nnd<slim){pl=pav;fl=1;}
      else if(fl==0){pl=pav;fl=1;}
   }
   if(pav>xpv){
      if(npp-beg>=slim){ph=pav;}
      else if((npp-beg>0)&&(fh==1)){ph=pav;fh=0;}
   }
   last=next;
   next=*(pin+next);
}
delete [] bg;
delete [] nd;
delete [] pin;
delete [] sd;
delete [] sn;
}

void Istreg::write_1df(void){
   long i,j,k,ct=0;
   int pflag=get_qflag();
   double p1;
   long *idx=new long[num];
   long *mrx=new long[num];
   for(long i=0;i<num;i++){
      *(idx+i)=i;
      *(mrx+i)=0;
   }

   //Sort in simple order.
   hSort(num,sco,idx);

   i=0;
   p1=*(p+*idx);
   *mrx=1;
   while(i<num){
      j=i;
      k=*(idx+j);
      while((j<num)&&(*(p+k)==p1)){j++;k=*(idx+j);}
      if(j==num){
         if(!*(mrx+j-1)){*(mrx+j-1)=1;ct++;}
         i=j;
      }
      else {
         if(!*(mrx+j-1)){*(mrx+j-1)=1;ct++;}
         *(mrx+j)=1;
         ct++;
         p1=*(p+k);
         i=j;
      }
   }
         
   ofstream *pfout=get_Ostr("1d");
   *pfout << ct << endl;
   for(i=0;i<num;i++){
      j=*(idx+i);
      if(*(mrx+i))*pfout << *(sco+i) << "\t" << *(p+j) << endl;
   }
   dst_Ostr(pfout);
   cout << ct << " function points" << endl;
}

void Istreg::read_1df(void){
   long i,j,k,ct=0,*mrx;
   
   ifstream *pfin=get_Istr("1d");
   *pfin >> num;
   if(sco!=NULL)delete [] sco;
   sco=new double[num];
   if(p!=NULL)delete [] p;
   p=new double[num];
   for(i=0;i<num;i++){
      *pfin >> *(sco+i) >> *(p+i);
   }
   dst_Istr(pfin);
}
   
double Istreg::val_1df(double x){
   long i,j,k,m;
   double y;
   
   if(x<=*sco)return(*p);
   if(x>*(sco+num-1))return(*(p+num-1));
   i=0;
   j=num-1;
   while(j-i>1){
      m=(i+j)/2;
      y=*(sco+m);
      if(x<=y)j=m;
      else i=m;
   }
   if(x==y)return(*(p+j));
   else return((*(p+i)+*(p+j))/2.0);
}

void Istreg::write_2df(void){
   long i,j,k,ct=0,*mrx;
   int pflag=get_qflag();
   double p1,x1;
   long *idx=new long[num],flah,flal,m,n;
   for(long i=0;i<num;i++)*(idx+i)=i;

   //Sort in lexical order.
   this->dSort(num,idx);
   
   mrx=new long[num];
   for(i=0;i<num;i++)*(mrx+i)=0;
   for(i=0;i<num;i++){
      flal=0;
      flah=0;
      m=*(idx+i);
      p1=*(p+m);
      x1=*(xr+m);
      j=i-1;
      while((j>=0)&&(!flal)){
         n=*(idx+j);
         if((xr[n]>=x1)&&(p[n]==p1))flal=1;
         j--;
      }
      j=i+1;
      while((j<num)&&(!flah)){
         n=*(idx+j);
         if((xr[n]<=x1)&&(p[n]==p1))flah=1;
         j++;
      }
      if(!(flah*flal)){
         *(mrx+i)=1;
         ct++;
      }
   }
         
   ofstream *pfout=get_Ostr("2d");
   *pfout << ct << endl;
   for(i=0;i<num;i++){
      m=*(idx+i);
      if(*(mrx+i))*pfout << *(xr+m) << "\t" << *(xs+m) << "\t" << *(p+m) << endl;
   }
   dst_Ostr(pfout);
   cout << ct << " function points" << endl;
}

void Istreg::read_2df(void){
   long i,j,k,ct=0,*mrx;
   double p1;
   
   ifstream *pfin=get_Istr("2d");
   *pfin >> num;
   if(mem_r&&xr!=NULL)delete [] xr;
   xr=new double[num];
   mem_r=1;
   if(mem_s&&xs!=NULL)delete [] xs;
   xs=new double[num];
   mem_s=1;
   if(p!=NULL)delete [] p;
   p=new double[num];
   *pfin >> *(xr) >> *(xs) >> p1;
   *p=p1;
   pmin=pmax=p1;
   for(i=1;i<num;i++){
      *pfin >> *(xr+i) >> *(xs+i) >> p1;
      *(p+i)=p1;
      pmin=(pmin>p1)?p1:pmin;
      pmax=(pmax<p1)?p1:pmax;
   }
   dst_Istr(pfin);
}
   
double Istreg::val_2df(double x,double y){
   long i,j,k;
   double ph=pmax,pl=pmin,xr1,xs1,p1;

   for(i=0;i<num;i++){
      xr1=xr[i];
      xs1=xs[i];
      p1=p[i];
      if(xr1<x){
         if(xs1<=y)pl=(pl<p1)?p1:pl;
      }
      else if(xr1>x){
         if(xs1>=y)ph=(ph>p1)?p1:ph;
      }
      else {
         if(xs1<y)pl=(pl<p1)?p1:pl;
         else if(xs1>y)ph=(ph>p1)?p1:ph;
         else return(p1);
      }
   }
   return((pl+ph)/2.0);
}

}
