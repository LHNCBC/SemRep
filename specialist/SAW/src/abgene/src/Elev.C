#include <unistd.h>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include "runn.h"
#include "Elev.h"
using namespace std;
namespace iret {
 
Elev::Elev(long nm){
   long i;
   num=nm;
   rev=new double[num];
   tm=0;
   for(i=0;i<11;i++)sc[i]=0;
   for(i=0;i<11;i++)tc[i]=0;
   for(i=0;i<11;i++)rc[i]=0.1*i;
}

Elev::~Elev(){
   if(rev!=NULL)delete [] rev;
}   

void Elev::load(ifstream &fin){
   long i,ix,qi,qn;
   double pz;
   
   pt=0;
   for(i=0;i<num;i++)*(rev+i)=0;
   fin >> qi >> qn;
   for(i=0;i<qn;i++){
      fin >> ix >> pz;
      pt+=*(rev+ix)=pz;
   }
}

void Elev::load(Index *ind){
   long i;

   pt=0;
   for(i=0;i<num;i++)*(rev+i)=0;
   for(i=0;i<ind->ix;i++){
      pt+=*(rev+ind->idx[i])=1.0;
   }
}

void Elev::process_ranks(long *idx){
   long i,u;
   double xx,tt,pz,rz;

   tt=0;
   for(i=0;i<11;i++)pc[i]=0;
   if(pt<=0)return;
   for(i=0;i<num;i++){
      if((xx=*(rev+*(idx+i)))>0){
         tt+=xx;
         pz=tt/((double)(i+1));
         rz=tt/pt;
         for(u=0;u<11;u++){
            if(rz>=rc[u])pc[u]=(pc[u]<pz)?pz:pc[u];
         }
      }
   }
   for(i=0;i<11;i++)sc[i]+=pc[i];
   tm++;
} 

void Elev::process_ranks(long ix,long *idx){
   long i,u;
   double xx,tt,pz,rz;

   tt=0;
   for(i=0;i<11;i++)pc[i]=0;
   if(pt<=0)return;
   for(i=0;i<ix;i++){
      if((xx=*(rev+*(idx+i)))>0){
         tt+=xx;
         pz=tt/((double)(i+1));
         rz=tt/pt;
         for(u=0;u<11;u++){
            if(rz>=rc[u])pc[u]=(pc[u]<pz)?pz:pc[u];
         }
      }
   }
   for(i=0;i<11;i++)sc[i]+=pc[i];
   tm++;
}

void Elev::process_ranks(Order *pord){
   long i,j,k,u,ix;
   double xx,tt,pz,rz;
   double *rnw,sum,ptt;
   float so,si;

   for(i=0;i<11;i++)pc[i]=0;
   if(pt<=0)return;

   ix=pord->pInd->ix;
   rnw=new double[ix];
   u=pord->ind(0,so);
   sum=rev[u];
   j=0;
   for(i=1;i<ix;i++){
      u=pord->ind(i,si);
      if(so>si){
         xx=(double)(i-j);
         for(k=j;k<i;k++)rnw[k]=sum/xx;
         sum=0;
         j=i;
         so=si;
      }
      sum+=rev[u];
   }
   xx=(double)(i-j);
   for(k=j;k<i;k++)rnw[k]=sum/xx;

   ptt=0;
   for(i=0;i<ix;i++)ptt+=rnw[i];

   tt=0;
   for(i=0;i<ix;i++){
      if((xx=*(rnw+i))>0){
         tt+=xx;
         pz=tt/((double)(i+1));
         rz=tt/ptt;
         for(u=0;u<11;u++){
            if(rz>=rc[u])pc[u]=(pc[u]<pz)?pz:pc[u];
         }
      }
   }
   for(i=0;i<11;i++)sc[i]+=pc[i];
   tm++;
   delete [] rnw;
}

void Elev::recall_prec_graph(double rc1,double rc2,Order *pord,ofstream &fout){
   long i,j,k,u,ix=pord->pInd->ix;
   double sum=0,rss;
   float si;

   for(i=0;i<ix;i++){
      u=pord->ind(i,si);
      if(rev[u]>0){
         sum+=rev[u];
         rss=sum/pt;
         if((rc1<=rss)&&(rss<=rc2)){
            fout << rss << " " << sum/((double)(i+1)) << " " << si << endl;
         }
      }
   }
}

void Elev::roc_graph(double rc1,double rc2,Order *pord,ofstream &fout){
   long i,j,k,u,ix=pord->pInd->ix;
   double sum=0,rss,xss;
   float si;

   xss=num-pt;
   for(i=0;i<ix;i++){
      u=pord->ind(i,si);
      if(rev[u]>0){
         sum+=rev[u];
         rss=sum/pt;
         if((rc1<=rss)&&(rss<=rc2)){
            fout << (i+1-sum)/xss << " " << rss << " " << si << endl;
         }
      }
   }
}

double Elev::current(void){
   long u;

   spl=0;
   if(tm==0)return(0); for(u=0;u<11;u++)spl+=pc[u];
   spl=spl/11.0;
   return(spl);
}

double Elev::summary(void){
   long u;

   spc=0;
   if(tm==0)return(0); for(u=0;u<11;u++)spc+=(tc[u]=sc[u]/tm);
   spc=spc/11.0;
   return(spc);
}

void Elev::feat_xnum(Vterm *ptm,double &xt,double &xr,double &xnt,double &xst){
   long j,k,ig,ip;

   xt=(double)num;
   xr=0.0;
   for(j=0;j<num;j++){
      xr+=*(rev+j);
   }
   ig=ptm->post->ix;
   xnt=(double)ig;
   xst=0.0;
   for(k=0;k<ig;k++){
      ip=*(ptm->post->idx+k);
      xst+=*(rev+ip);
   }
}

double Elev::feat_wtt(long nm,long dm,double yt,double yr,double ynt,double yst){
   long i,j;
   double xx,xt,xr,xnt,xst,del=0.5;
   double wt,ptt,qt,rv;
   rv=(double)nm/((double)dm);
   xt=yt-1.0;
   xr=yr-rv;
   xnt=ynt-1.0;
   xst=yst-rv;

   ptt =(xst+del)/(xr+2*del);
   qt =(xnt-xst+del)/(xt-xr+2*del);
   if(ptt<=qt)return(0);
   wt=log((ptt*(1-qt))/ (qt*(1- ptt)));
   return(wt);
}

void Elev::feat_xps(Vterm *ptm,double pri,double &ps,double &ptt,double &qt){
   long j,k,ig,ip;
   double xt,xr,xnt,xst,del=0.5;
   double rt,xx;
   xt=(double)num;
   
   xr=0.0;
   for(j=0;j<num;j++){
      xr+=*(rev+j);
   }
   
   ig=ptm->post->ix;
   xnt=(double)ig;
   xst=0.0;
   for(k=0;k<ig;k++){
      ip=*(ptm->post->idx+k);
      xst+=*(rev+ip);
   }
   ptt =(xst+del)/(xr+2*del);
   qt =(xnt-xst+del)/(xt-xr+2*del);
   rt =(xnt+del)/(xt+2*del);
   xx=-xst*log(rt/ptt) - (xnt-xst)*log(rt/qt);
   xx-=(xr-xst)*log((1.0-rt)/(1.0-ptt));
   xx-=(xt-xr-xnt+xst)*log((1.0-rt)/(1.0-qt));
   ps=1.0/(1.0+(1.0/pri-1.0)*exp(-xx));
}

//Bootstrapping functions

void Elev::rev_load(Resamp &Rsp){
   long i;

   pt=0;
   for(i=0;i<num;i++)pt+=rev[i]*Rsp.zsamp[i];
}

void Elev::process_ranks(long *idx,Resamp &Rsp){
   long i,j,u,ic,im;
   double xx,tt,pz,rz;

   tt=0;
   for(i=0;i<11;i++)pc[i]=0;
   if(pt<=0)return;
   im=0;
   for(i=0;i<num;i++){
      if(ic=Rsp.zsamp[idx[i]]){
         for(j=0;j<ic;j++){
            if((xx=*(rev+*(idx+i)))>0){
               tt+=xx;
               pz=tt/((double)(im+1));
               rz=tt/pt;
               for(u=0;u<11;u++){
                  if(rz>=rc[u])pc[u]=(pc[u]<pz)?pz:pc[u];
               }
            }
            im++;
         }
      }
   }
   for(i=0;i<11;i++)sc[i]+=pc[i];
   tm++;
}

void Elev::process_ranks(long ix,long *idx,Resamp &Rsp){
   long i,j,u,ic,im;
   double xx,tt,pz,rz;

   tt=0;
   for(i=0;i<11;i++)pc[i]=0;
   if(pt<=0)return;
   im=0;
   for(i=0;i<ix;i++){
      if(ic=Rsp.zsamp[idx[i]]){
         for(j=0;j<ic;j++){
            if((xx=*(rev+*(idx+i)))>0){
               tt+=xx;
               pz=tt/((double)(im+1));
               rz=tt/pt;
               for(u=0;u<11;u++){
                  if(rz>=rc[u])pc[u]=(pc[u]<pz)?pz:pc[u];
               }
            }
            im++;
         }
      }
   }
   for(i=0;i<11;i++)sc[i]+=pc[i];
   tm++;
}

void Elev::process_ranks(Order *pord,Resamp &Rsp){
   long i,j,k,u,ix,im,*ornew,ic;
   double xx,tt,pz,rz;
   float so,si,*scnew;

   ix=pord->pInd->ix;
   u=0;
   for(i=0;i<ix;i++){
      u+=Rsp.zsamp[pord->pInd->idx[i]];
   }
   Index *knd=new Index(u);
   scnew=new float[u];
   ornew=new long[u];
   double *rnv=new double[num];
   for(i=0;i<num;i++){
      rnv[i]=rev[i];
      rev[i]=0;
   }
   im=0;
   for(i=0;i<ix;i++){
      k=pord->ind(i,si);
      if(ic=Rsp.zsamp[k]){
         for(j=0;j<ic;j++){
            rev[im]=rnv[k];
            knd->idx[im]=im;
            ornew[im]=im;
            scnew[im]=si;
            im++;
         }
      }
   }
   Order *qord=new Order;
   qord->pInd=knd;
   qord->order=ornew;
   qord->score=scnew;
   process_ranks(qord);
   delete [] rnv;
   delete qord;
}

//Resamp class

Resamp::Resamp(long nm){
   num=nm;
   zsamp=new long[num];
}

Resamp::~Resamp(void){
   delete [] zsamp;
}

void Resamp::zero(void){
   long i;
   for(i=0;i<num;i++)zsamp[i]=0;
}

void Resamp::sample(long m,Index *pind){
   long i,j;
   for(i=0;i<m;i++){  
      j=zrand(pind->ix);
      (zsamp[pind->idx[j]])++;
   }
}

void Resamp::origin(Index *pind){
   long i;
   for(i=0;i<pind->ix;i++){  
      zsamp[pind->idx[i]]=1;
   }
}

}
