#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <runn.h>
#include <Btree.h>
#include <Postg.h>
#include <Vnab.h>
#include "Cmls.h"

using namespace std;
namespace iret {

Cmls::Cmls(Dbinbase *pDb,Postg<char> *pPg) : Mang(pDb,pPg){
   wt=NULL;
   dl=NULL;
   rt=NULL;
   mrk=NULL;
   sco=NULL;
   cls=NULL;
   tx=NULL;
   sx=NULL;
   pdoc=NULL;
}

Cmls::Cmls(const char *namdbn,const char *nampsg) : Mang(namdbn,nampsg){
   wt=NULL;
   dl=NULL;
   rt=NULL;
   mrk=NULL;
   sco=NULL;
   cls=NULL;
   tx=NULL;
   sx=NULL;
   pdoc=NULL;
}

Cmls::Cmls(Docum &Doc) : Mang(Doc){
   wt=NULL;
   dl=NULL;
   rt=NULL;
   mrk=NULL;
   sco=NULL;
   cls=NULL;
   tx=NULL;
   sx=NULL;
   pdoc=NULL;
}

Cmls::Cmls(const char *namdoc) : Mang(namdoc){
   wt=NULL;
   dl=NULL;
   rt=NULL;
   mrk=NULL;
   sco=NULL;
   cls=NULL;
   tx=NULL;
   sx=NULL;
   pdoc=NULL;
}

Cmls::~Cmls(){
   if(wt!=NULL)delete [] wt;
   if(dl!=NULL)delete [] dl;
   if(rt!=NULL)delete [] rt;
   if(mrk!=NULL)delete [] mrk;
   if(sco!=NULL)delete [] sco;
   if(cls!=NULL)delete [] cls;
   if(tx!=NULL)delete [] tx;
   if(sx!=NULL)delete [] sx;
   if(pdoc!=NULL)delete [] pdoc;
}

void Cmls::init_cnt(void){
   long i;
   eps=1.0/((double)ndoc);

   if(tx!=NULL)delete [] tx;
   tx=new double[nwrd];

   if(sx!=NULL)delete [] sx;
   sx=new double[nwrd];

   if(pdoc!=NULL)delete [] pdoc;
   pdoc=new double[ndoc];
   for(i=0;i<ndoc;i++)pdoc[i]=eps;
}

void Cmls::zerot(void){
   nnx=0.0;
   for(long i=0;i<nwrd;i++)*(tx+i)=0;
}

void Cmls::zeros(void){
   nsx=0.0;
   for(long i=0;i<nwrd;i++)*(sx+i)=0;
}

void Cmls::countDoc(long i){
   long j,k;
   double pt;
   pt=pdoc[i];

   read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(tx+j))+=pt;
   }
}

void Cmls::counsDoc(long i){
   long j,k;
   double pt;
   pt=pdoc[i];

   read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(sx+j))+=pt;
   }
}

void Cmls::counbDoc(long i){
   long j,k;
   double pt;
   pt=pdoc[i];

   read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(sx+j))+=pt;
      (*(tx+j))+=pt;
   }
}

void Cmls::countTX(Index *cnd){
   long i,j;

   for(i=0;i<cnd->ix;i++){
      j=cnd->idx[i];
      this->countDoc(j);
      nnx+=pdoc[j];
   }
}

void Cmls::countSX(Index *cnd){
   long i,j;

   for(i=0;i<cnd->ix;i++){
      j=cnd->idx[i];
      this->counsDoc(j);
      nsx+=pdoc[j];
   }
}

void Cmls::countBX(Index *cnd){
   long i,j;

   for(i=0;i<cnd->ix;i++){
      j=cnd->idx[i];
      this->counbDoc(j);
      nnx+=pdoc[j];
      nsx+=pdoc[j];
   }
}

void Cmls::Set_Term_wt(double cut){
   double n_t,n_st,min;
   long nstw=0, flag;
   double xx,wtt,frc,pt,qt,rtx;
   if(mrk!=NULL)delete [] mrk;
   mrk=new long[nwrd];

   frc=nsx/nnx;

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      if(n_t){
         min=(n_t<nsx)?n_t:nsx;
         n_st=*(sx+i);
         flag=1;
         if(n_st==min){
            if(n_st-eps >n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st==0){
            if(eps<n_t*frc)n_st=eps;
            else flag=0;
         }
      }
      else flag=0;

      if(flag){
         pt =n_st/nsx;
         qt =(n_t-n_st)/(nnx-nsx);
         //calculate wt for this term
         wtt=log(pt*(1.0-qt))-log(qt*(1.0-pt));

         if(wtt>=cut){
            mrk[i]=1;
            nstw++;
         }
         else mrk[i]=0;
      }
      else {
         mrk[i]=0;
      }
   }
   cout << "Number of weight marked terms= " << nstw << endl;
}

void Cmls::Set_Term_alp(double cut){
   double n_t,n_st,min;
   long nstw=0, flag;
   double xx,frc,pt,qt,rtx;
   if(mrk!=NULL)delete [] mrk;
   mrk=new long[nwrd];

   frc=nsx/nnx;

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      if(n_t){
         min=(n_t<nsx)?n_t:nsx;
         n_st=*(sx+i);
         flag=1;
         if(n_st==min){
            if(n_st-eps >n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st==0){
            if(eps<n_t*frc)n_st=eps;
            else flag=0;
         }
      }
      else flag=0;

      if(flag){
         pt =n_st/nsx;
         qt =(n_t-n_st)/(nnx-nsx);
         rtx =n_t/nnx;

         xx=-n_st*log(rtx/pt) - (n_t-n_st)*log(rtx/qt);
         xx-=(nsx-n_st)*log((1.0-rtx)/(1.0-pt));
         xx-=(nnx-nsx-n_t+n_st)*log((1.0-rtx)/(1.0-qt));
         if(xx>=cut){
            mrk[i]=1;
            nstw++;
         }
         else mrk[i]=0;

      }
      else {
         mrk[i]=0;
      }
   }
   cout << "Number of alpha marked terms= " << nstw << endl;
}

void Cmls::Set_Term_chi(double cut){
   double n_t,n_st,min;
   long nstw=0, flag;
   double xx,frc,thr;
   if(mrk!=NULL)delete [] mrk;
   mrk=new long[nwrd];

   frc=nsx/nnx;

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      if(n_t){
         min=(n_t<nsx)?n_t:nsx;
         n_st=*(sx+i);
         flag=1;
         if(n_st==min){
            if(n_st-eps >n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st==0){
            if(eps<n_t*frc)n_st=eps;
            else flag=0;
         }
      }
      else flag=0;

      if(flag){
         xx=frc*n_t;
         thr=eps*cut*xx*(1.0-xx/((double)nnx));
         xx-=(double)n_st;

         if(xx*xx>=thr){
            mrk[i]=1;
            nstw++;
         }
         else mrk[i]=0;

      }
      else {
         mrk[i]=0;
      }
   }
   cout << "Number of chi sq marked terms= " << nstw << endl;
}

void Cmls::Set_Term_freq(long nm){
   long i,nstw=0;
   if(mrk!=NULL)delete [] mrk;
   mrk=new long[nwrd];
   for(i=0;i<nwrd;i++){
      if(Pst[i].ix<nm)mrk[i]=0;
      else {
         mrk[i]=1;
         nstw++;
      }
   }
   cout << "Number of freq marked terms= " << nstw << endl;
}

void Cmls::Set_Docs(Index *gdd,Index *bdd){
   long i;

   if(cls!=NULL)delete [] cls;
   cls=new long[ndoc];
   for(i=0;i<ndoc;i++)cls[i]=0;
   for(i=0;i<gdd->ix;i++){
      cls[gdd->idx[i]]=2;
   }
   for(i=0;i<bdd->ix;i++){
      cls[bdd->idx[i]]=-2;
   }
   tdoc=gdd->ix+bdd->ix;
}

void Cmls::Set_Lambda(double lam){
   lambda=2.0*tdoc*lam;
}

void Cmls::Learn(long rnds){
   int pflag=get_qflag();
   long i,j,k,n;
   double *cc,sumn,sumd;
   double xx,yy,dv,dw;
   Index *pnd;

   if(wt!=NULL)delete [] wt;
   if(dl!=NULL)delete [] dl;
   if(rt!=NULL)delete [] rt;
   wt=new double[nwrd];
   dl=new double[nwrd];
   rt=new double[ndoc];
   for(i=0;i<nwrd;i++){
      wt[i]=0;
      dl[i]=1.0;
   }
   th=0;
   td=1.0;
   for(i=0;i<ndoc;i++){
      if(cls[i])rt[i]=-1.0;
      else rt[i]=0;
   }
   
   cc=new double[rnds];
   for(i=0;i<rnds-50;i++)cc[i]=1.0-i/(rnds-50.0);
   for(i=rnds-50;i<rnds;i++)cc[i]=0.0;

   for(k=0;k<rnds;k++){
      for(j=0;j<nwrd;j++){
         if(mrk[j]){
            sumn=sumd=0;
            yy=dl[j];
            for(n=0;n<Pst[j].ix;n++){
               i=Pst[j].idx[n];
               if(xx=cls[i]){
                  if(rt[i]<0.0)sumn+=rt[i]*xx; 
                  else sumn+=rt[i]*xx*cc[k];
                  if(rt[i]<=yy)sumd+=2.0;
                  else sumd+=2.0*cc[k];
               }
            }
            dv=-(sumn+lambda*wt[j])/(sumd+lambda);
            dw=(dv<-yy)?-yy:dv;
            dw=0.5*((dw>yy)?yy:dw);
            for(n=0;n<Pst[j].ix;n++){
               i=Pst[j].idx[n];
               if(xx=cls[i])rt[i]+=dw*xx;
            }     
            wt[j]+=2.0*dw;
            dl[j]=4.0*fabs(dw)+0.1;
         }
         mark(pflag,j+1,50,"terms");
      }
      //Threshhold adjustment
      sumn=sumd=0;
      for(i=0;i<ndoc;i++){
         if(xx=cls[i]){
            if(rt[i]<0.0)sumn+=rt[i]*xx; 
            else sumn+=rt[i]*xx*cc[k];
            if(rt[i]<=th)sumd+=2.0;
            else sumd+=2.0*cc[k];
         }
      }
      dv=-(sumn+lambda*th)/(sumd+lambda);
      dw=(dv<-td)?-td:dv;
      dw=0.5*((dw>td)?td:dw);
      for(i=0;i<ndoc;i++){
         if(xx=cls[i])rt[i]+=dw*xx;
      }     
      th+=2.0*dw;
      td=4.0*fabs(dw)+0.1;
      //End of threshhold adjustment
cout << "Func " << Func(cc[k]) << endl;
      mark(pflag,k+1,1,"rounds");
   }
}
            
double *Cmls::ScoreAll(void){
   int pflag=get_qflag();
   long j,n;
   double xx;
   Index *pnd;

   if(sco!=NULL)delete [] sco;
   sco=new double[ndoc];
   for(n=0;n<ndoc;n++)sco[n]=th;

   for(j=0;j<nwrd;j++){
      if(xx=wt[j]){
         for(n=0;n<Pst[j].ix;n++){
            sco[Pst[j].idx[n]]+=xx;
         }
      }
      mark(pflag,j+1,1000,"terms scored");
   }
   return(sco);
}

double *Cmls::ScoreSet(Index *ind){
   int pflag=get_qflag();
   long i,n;
   double sum;

   if(sco!=NULL)delete [] sco;
   sco=new double[ind->ix];

   for(i=0;i<ind->ix;i++){
      read(ind->idx[i]);
      sum=th;
      for(n=0;n<nw;n++)sum+=wt[nwd[n]];
      sco[i]=sum;
      mark(pflag,i+1,100,"docs scored");
   }
   return(sco);
}

double Cmls::Func(double cx){ 
   long i;
   double sum=0,xx;
   for(i=0;i<ndoc;i++){
      xx=rt[i];
      if(xx<0)sum+=xx*xx;
      else if(xx>0)sum+=cx*xx*xx;
   }
   return(sum);
}

//Cmlsf - local weighted version

Cmlsf::Cmlsf(Dbinbase *pDb,Postg<float> *pPg) : Manf(pDb,pPg){
   wt=NULL;
   dl=NULL;
   rt=NULL;
   mrk=NULL;
   sco=NULL;
   cls=NULL;
   tx=NULL;
   sx=NULL;
   pdoc=NULL;
}

Cmlsf::Cmlsf(const char *namdbn,const char *nampsg) : Manf(namdbn,nampsg){
   wt=NULL;
   dl=NULL;
   rt=NULL;
   mrk=NULL;
   sco=NULL;
   cls=NULL;
   tx=NULL;
   sx=NULL;
   pdoc=NULL;
}

Cmlsf::~Cmlsf(){
   if(wt!=NULL)delete [] wt;
   if(dl!=NULL)delete [] dl;
   if(rt!=NULL)delete [] rt;
   if(mrk!=NULL)delete [] mrk;
   if(sco!=NULL)delete [] sco;
   if(cls!=NULL)delete [] cls;
   if(tx!=NULL)delete [] tx;
   if(sx!=NULL)delete [] sx;
   if(pdoc!=NULL)delete [] pdoc;
}

void Cmlsf::init_cnt(void){
   long i;
   eps=1.0/((double)ndoc);

   if(tx!=NULL)delete [] tx;
   tx=new double[nwrd];

   if(sx!=NULL)delete [] sx;
   sx=new double[nwrd];

   if(pdoc!=NULL)delete [] pdoc;
   pdoc=new double[ndoc];
   for(i=0;i<ndoc;i++)pdoc[i]=eps;
}

void Cmlsf::zerot(void){
   nnx=0.0;
   for(long i=0;i<nwrd;i++)*(tx+i)=0;
}

void Cmlsf::zeros(void){
   nsx=0.0;
   for(long i=0;i<nwrd;i++)*(sx+i)=0;
}

void Cmlsf::countDoc(long i){
   long j,k;
   double pt;
   pt=pdoc[i];

   read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(tx+j))+=pt;
   }
}

void Cmlsf::counsDoc(long i){
   long j,k;
   double pt;
   pt=pdoc[i];

   read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(sx+j))+=pt;
   }
}

void Cmlsf::counbDoc(long i){
   long j,k;
   double pt;
   pt=pdoc[i];

   read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(sx+j))+=pt;
      (*(tx+j))+=pt;
   }
}

void Cmlsf::countTX(Index *cnd){
   long i,j;

   for(i=0;i<cnd->ix;i++){
      j=cnd->idx[i];
      this->countDoc(j);
      nnx+=pdoc[j];
   }
}

void Cmlsf::countSX(Index *cnd){
   long i,j;

   for(i=0;i<cnd->ix;i++){
      j=cnd->idx[i];
      this->counsDoc(j);
      nsx+=pdoc[j];
   }
}

void Cmlsf::countBX(Index *cnd){
   long i,j;

   for(i=0;i<cnd->ix;i++){
      j=cnd->idx[i];
      this->counbDoc(j);
      nnx+=pdoc[j];
      nsx+=pdoc[j];
   }
}

void Cmlsf::Set_Term_wt(double cut){
   double n_t,n_st,min;
   long nstw=0, flag;
   double xx,wtt,frc,pt,qt,rtx;
   if(mrk!=NULL)delete [] mrk;
   mrk=new long[nwrd];

   frc=nsx/nnx;

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      if(n_t){
         min=(n_t<nsx)?n_t:nsx;
         n_st=*(sx+i);
         flag=1;
         if(n_st==min){
            if(n_st-eps >n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st==0){
            if(eps<n_t*frc)n_st=eps;
            else flag=0;
         }
      }
      else flag=0;

      if(flag){
         pt =n_st/nsx;
         qt =(n_t-n_st)/(nnx-nsx);
         //calculate wt for this term
         wtt=log(pt*(1.0-qt))-log(qt*(1.0-pt));

         if(wtt>=cut){
            mrk[i]=1;
            nstw++;
         }
         else mrk[i]=0;
      }
      else {
         mrk[i]=0;
      }
   }
   cout << "Number of weight marked terms= " << nstw << endl;
}

void Cmlsf::Set_Term_alp(double cut){
   double n_t,n_st,min;
   long nstw=0, flag;
   double xx,frc,pt,qt,rtx;
   if(mrk!=NULL)delete [] mrk;
   mrk=new long[nwrd];

   frc=nsx/nnx;

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      if(n_t){
         min=(n_t<nsx)?n_t:nsx;
         n_st=*(sx+i);
         flag=1;
         if(n_st==min){
            if(n_st-eps >n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st==0){
            if(eps<n_t*frc)n_st=eps;
            else flag=0;
         }
      }
      else flag=0;

      if(flag){
         pt =n_st/nsx;
         qt =(n_t-n_st)/(nnx-nsx);
         rtx =n_t/nnx;

         xx=-n_st*log(rtx/pt) - (n_t-n_st)*log(rtx/qt);
         xx-=(nsx-n_st)*log((1.0-rtx)/(1.0-pt));
         xx-=(nnx-nsx-n_t+n_st)*log((1.0-rtx)/(1.0-qt));
         if(xx>=cut){
            mrk[i]=1;
            nstw++;
         }
         else mrk[i]=0;

      }
      else {
         mrk[i]=0;
      }
   }
   cout << "Number of alpha marked terms= " << nstw << endl;
}

void Cmlsf::Set_Term_chi(double cut){
   double n_t,n_st,min;
   long nstw=0, flag;
   double xx,frc,thr;
   if(mrk!=NULL)delete [] mrk;
   mrk=new long[nwrd];

   frc=nsx/nnx;

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      if(n_t){
         min=(n_t<nsx)?n_t:nsx;
         n_st=*(sx+i);
         flag=1;
         if(n_st==min){
            if(n_st-eps >n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st==0){
            if(eps<n_t*frc)n_st=eps;
            else flag=0;
         }
      }
      else flag=0;

      if(flag){
         xx=frc*n_t;
         thr=eps*cut*xx*(1.0-xx/((double)nnx));
         xx-=(double)n_st;

         if(xx*xx>=thr){
            mrk[i]=1;
            nstw++;
         }
         else mrk[i]=0;

      }
      else {
         mrk[i]=0;
      }
   }
   cout << "Number of chi sq marked terms= " << nstw << endl;
}

void Cmlsf::Set_Term_freq(long nm){
   long i,nstw=0;
   if(mrk!=NULL)delete [] mrk;
   mrk=new long[nwrd];
   for(i=0;i<nwrd;i++){
      if(Pst[i].ix<nm)mrk[i]=0;
      else {
         mrk[i]=1;
         nstw++;
      }
   }
   cout << "Number of freq marked terms= " << nstw << endl;
}

void Cmlsf::Set_Docs(Index *gdd,Index *bdd){
   long i;

   if(cls!=NULL)delete [] cls;
   cls=new long[ndoc];
   for(i=0;i<ndoc;i++)cls[i]=0;
   for(i=0;i<gdd->ix;i++){
      cls[gdd->idx[i]]=2;
   }
   for(i=0;i<bdd->ix;i++){
      cls[bdd->idx[i]]=-2;
   }
   tdoc=gdd->ix+bdd->ix;
}

void Cmlsf::Set_Lambda(double lam){
   lambda=2.0*tdoc*lam;
}

void Cmlsf::Learn(long rnds){
   int pflag=get_qflag();
   long i,j,k,n;
   double *cc,sumn,sumd;
   double xx,yy,dv,dw,zz;
   Index *pnd;

   if(wt!=NULL)delete [] wt;
   if(dl!=NULL)delete [] dl;
   if(rt!=NULL)delete [] rt;
   wt=new double[nwrd];
   dl=new double[nwrd];
   rt=new double[ndoc];
   for(i=0;i<nwrd;i++){
      wt[i]=0;
      dl[i]=1.0;
   }
   th=0;
   td=1.0;
   for(i=0;i<ndoc;i++){
      if(cls[i])rt[i]=-1.0;
      else rt[i]=0;
   }

   cc=new double[rnds];
   for(i=0;i<rnds-50;i++)cc[i]=1.0-i/(rnds-50.0);
   for(i=rnds-50;i<rnds;i++)cc[i]=0.0;

   for(k=0;k<rnds;k++){
      for(j=0;j<nwrd;j++){
         if(mrk[j]){
            sumn=sumd=0;
            yy=dl[j];
            for(n=0;n<Pst[j].ix;n++){
               i=Pst[j].idx[n];
               if(xx=cls[i]){
                  zz=(double)flc[j][n];
                  if(rt[i]<0.0)sumn+=rt[i]*xx*zz;
                  else sumn+=rt[i]*xx*cc[k]*zz;
                  if(rt[i]<=yy*zz)sumd+=2.0*zz*zz;
                  else sumd+=2.0*cc[k]*zz*zz;
               }
            }
            dv=-(sumn+lambda*wt[j])/(sumd+lambda);
            dw=(dv<-yy)?-yy:dv;
            dw=0.5*((dw>yy)?yy:dw);
            for(n=0;n<Pst[j].ix;n++){
               i=Pst[j].idx[n];
               if(xx=cls[i]){
                  zz=(double)flc[j][n];
                  rt[i]+=dw*xx*zz;
               }
            }
            wt[j]+=2.0*dw;
            dl[j]=4.0*fabs(dw)+0.1;
         }
         mark(pflag,j+1,50,"terms");
      }
      //Threshhold adjustment
      sumn=sumd=0;
      for(i=0;i<ndoc;i++){
         if(xx=cls[i]){
            if(rt[i]<0.0)sumn+=rt[i]*xx;
            else sumn+=rt[i]*xx*cc[k];
            if(rt[i]<=th)sumd+=2.0;
            else sumd+=2.0*cc[k];
         }
      }
      dv=-(sumn+lambda*th)/(sumd+lambda);
      dw=(dv<-td)?-td:dv;
      dw=0.5*((dw>td)?td:dw);
      for(i=0;i<ndoc;i++){
         if(xx=cls[i])rt[i]+=dw*xx;
      }
      th+=2.0*dw;
      td=4.0*fabs(dw)+0.1;
      //End of threshhold adjustment
cout << "Func " << Func(cc[k]) << endl;
      mark(pflag,k+1,1,"rounds");
   }
}

double *Cmlsf::ScoreAll(void){
   int pflag=get_qflag();
   long j,n;
   double xx,zz;
   Index *pnd;

   if(sco!=NULL)delete [] sco;
   sco=new double[ndoc];
   for(n=0;n<ndoc;n++)sco[n]=th;

   for(j=0;j<nwrd;j++){
      if(xx=wt[j]){
         for(n=0;n<Pst[j].ix;n++){
            zz=(double)flc[j][n];
            sco[Pst[j].idx[n]]+=xx*zz;
         }
      }
      mark(pflag,j+1,1000,"terms scored");
   }
   return(sco);
}

double *Cmlsf::ScoreSet(Index *ind){
   int pflag=get_qflag();
   long i,n;
   double sum;
   float *xp;

   if(sco!=NULL)delete [] sco;
   sco=new double[ind->ix];

   for(i=0;i<ind->ix;i++){
      read(ind->idx[i]);
      sum=th;
      for(n=0;n<nw;n++)sum+=wt[nwd[n]]*lwt[n];
      sco[i]=sum;
      mark(pflag,i+1,100,"docs scored");
   }
   return(sco);
}

double Cmlsf::Func(double cx){
   long i;
   double sum=0,xx;
   for(i=0;i<ndoc;i++){
      xx=rt[i];
      if(xx<0)sum+=xx*xx;
      else if(xx>0)sum+=cx*xx*xx;
   }
   return(sum);
}

}
