#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <runn.h>
#include <Btree.h>
#include <Postg.h>
#include <Vnab.h>
#include "Thln.h"

using namespace std;
namespace iret {

Thln::Thln(Dbinbase *pDb,Postg<char> *pPg){
   pDnb=pDb;
   pPsg=pPg;

   pDnb->gopen_operate(DBIN_A|DBIN_W);
   ndoc=pDnb->ndoc;
   pPsg->gopen_map();
   nwrd=pPsg->nwrd;
   pPsg->fill_post();

   tx=new double[nwrd];
   sx=new double[nwrd];
   wt=new double[nwrd];
   ax=new double[nwrd];
   sco=NULL;
   cur=NULL;
}

Thln::Thln(Docum &Doc){
   create_Postg(Doc);
   create_Dbinbase(Doc);
   pPsg=new Postg<char>(Doc.name);
   pDnb=new Dbinbase(Doc.name);

   pDnb->gopen_operate(DBIN_A|DBIN_W);
   ndoc=pDnb->ndoc;
   pPsg->gopen_map();
   nwrd=pPsg->nwrd;
   pPsg->fill_post();

   tx=new double[nwrd];
   sx=new double[nwrd];
   wt=new double[nwrd];
   ax=new double[nwrd];
   sco=NULL;
   cur=NULL;
}

Thln::~Thln(){
   if(wt!=NULL)delete [] wt;
   if(tx!=NULL)delete [] tx;
   if(sx!=NULL)delete [] sx;
   if(ax!=NULL)delete [] ax;
   if(sco!=NULL)delete [] sco;
   if(cur!=NULL)delete cur;
}

void Thln::create_Postg(Docum &Doc){
   int pflag=get_qflag();
   Postg<char> Pos(Doc.name);

   Doc.gopen_read(READ_W);
   char *spr;
   int j;

   for(long i=0;i<Doc.ndoc;i++){
      Doc.clear();
      Doc.read();
      while((spr=Doc.show(j))!=NULL){
         if(j>255)j=255;
         Pos.add(spr,i,j);
      }
      mark(pflag,i+1,100,"documents loaded");
   }
   Doc.gclose_read(READ_W);
   Pos.writep();
}

void Thln::create_Dbinbase(Docum &Doc){
   Postg<char> Pos(Doc.name);
   Pos.gopen_map();
   Count Ct;
   long i;
   for(i=0;i<Pos.nwrd;i++){
      Ct.add_count2(Pos.term+*(Pos.sddr+i),i+1);
   }
   Dbinbase Db(Doc.name);
   Db.create_files(Doc,Ct,q_const);
}

void Thln::read(long n){
   nw=*(pDnb->size+n);
   nwd=pDnb->don+*(pDnb->dad+n);
}

void Thln::Set_Train(Index *gd,Index *bd){
   gdd=gd;
   bdd=bd;
   tdoc=gdd->ix+bdd->ix;
   eps=1.0/((double)tdoc);
}

void Thln::Set_Param(long ntm,long stg){
   ntrm=ntm;
   stng=stg;
}

void Thln::zerot(void){
   nnx=0.0;
   for(long i=0;i<nwrd;i++)*(tx+i)=0;
}

void Thln::zeros(void){
   nsx=0.0;
   for(long i=0;i<nwrd;i++)*(sx+i)=0;
}

void Thln::countDoc(long i){
   long j,k;
   double pt;
   pt=dt[i];

   this->read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(tx+j))+=pt;
   }
}

void Thln::counsDoc(long i){
   long j,k;
   double pt;
   pt=dt[i];

   this->read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(sx+j))+=pt;
   }
}

void Thln::counbDoc(long i){
   long j,k;
   double pt;
   pt=dt[i];

   this->read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(sx+j))+=pt;
      (*(tx+j))+=pt;
   }
}

void Thln::countTX(void){
   long i,j;

   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      this->countDoc(j);
      nnx+=dt[j];
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      this->countDoc(j);
      nnx+=dt[j];
   }
}

void Thln::countTX(Index *cnd){
   long i,j;

   for(i=0;i<cnd->ix;i++){
      j=cnd->idx[i];
      this->countDoc(j);
      nnx+=dt[j];
   }
}

void Thln::countSX(Index *cnd){
   long i,j;

   for(i=0;i<cnd->ix;i++){
      j=cnd->idx[i];
      this->counsDoc(j);
      nsx+=dt[j];
   }
}

void Thln::countBX(Index *cnd){
   long i,j;

   for(i=0;i<cnd->ix;i++){
      j=cnd->idx[i];
      this->counbDoc(j);
      nnx+=dt[j];
      nsx+=dt[j];
   }
}

WOrder *Thln::weightSall(void){
   double n_t,n_st,min,tnx;
   long nstw=0, flag;
   double xx,frc,pt,qt,rt;

   tnx=nnx+nsx;
   frc=nsx/tnx;

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i)+*(sx+i);
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
         nstw++;
         pt =n_st/nsx;
         qt =(n_t-n_st)/(tnx-nsx);
         rt =n_t/tnx;
         //calculate wt for this term
         *(wt+i)=log(pt*(1.0-qt))-log(qt*(1.0-pt));

         xx=-n_st*log(rt/pt) - (n_t-n_st)*log(rt/qt);
         xx-=(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         xx-=(tnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));
         *(ax+i)=xx;
      }
      else {
         *(wt+i)=0;
         *(ax+i)=0;
      }
   }
   cout << "Number of weighted terms= " << nstw << endl;
   Order *pord=new Order(ntrm,nwrd,ax);
   WOrder *wpord=new WOrder(pord,wt);
   return(wpord);
}

double *Thln::ScoreAll(WOrder *wpord){
   int pflag=get_qflag();
   long i,j,n;
   double xx;
   Index *pnd,*qnd=wpord->pInd;
   float *wtt=wpord->weight;

   if(sco!=NULL)delete [] sco;
   sco=new double[ndoc];
   for(n=0;n<ndoc;n++)sco[n]=0;

   for(i=0;i<qnd->ix;i++){
      j=qnd->idx[i];
      pnd=pPsg->Pst+j;
      xx=wtt[i];
      for(n=0;n<pnd->ix;n++){
         sco[pnd->idx[n]]+=xx;
      }
      mark(pflag,i+1,10,"terms scored");
   }
   return(sco);
}

double Thln::ScoreLim(WOrder *wpord){
   long i,j,n;
   float sy;
   double xx,sum=0;
   Index *pnd,*qnd=wpord->pInd;
   float *wtt=wpord->weight;
   if(!qnd->ix){cout << "Error, ix=0" << endl;exit(0);}
   for(i=0;i<qnd->ix;i++){
      sum+=wtt[i];
   }
   xx=(double)qnd->ix;
   return(stng*sum/xx);
}

WOrder *Thln::Initiator(void){
   zerot();
   zeros();
   countTX(bdd);
   countSX(gdd);
   WOrder *wpord=weightSall();
   ScoreAll(wpord);
   xlim=ScoreLim(wpord);
   cout << "xlim " << xlim << endl;
   if(cur) delete cur;
   cur=gdd->Greateq(xlim,sco);
   if((!cur)||(cur->ix<200)){
      if(cur) delete cur;
      Order *qord=new Order(200,gdd,sco);
      cur=qord->pInd;
      qord->pInd=NULL;
      delete qord;
   }
   return(wpord);
}

WOrder *Thln::Initiator(WOrder *wpord){
   ScoreAll(wpord);
   xlim=ScoreLim(wpord);
   cout << "xlim " << xlim << endl;
   if(cur) delete cur;
   cur=gdd->Greateq(xlim,sco);
   if((!cur)||(cur->ix<200)){
      if(cur) delete cur;
      Order *qord=new Order(200,gdd,sco);
      cur=qord->pInd;
      qord->pInd=NULL;
      delete qord;
   }
   return(wpord);
}

WOrder *Thln::Iterator(void){
   zeros();
   countSX(cur);
   WOrder *wpord=weightSall();
   ScoreAll(wpord);
   xlim=ScoreLim(wpord);
   cout << "xlim " << xlim << endl;
   if(cur) delete cur;
   cur=gdd->Greateq(xlim,sco);
   if((!cur)||(cur->ix<200)){
      if(cur) delete cur;
      Order *qord=new Order(200,gdd,sco);
      cur=qord->pInd;
      qord->pInd=NULL;
      delete qord;
   }
   return(wpord);
}

void Thln::show_terms(WOrder *wzord){
   long i,j;
   float alpha;
   for(i=0;i<wzord->pInd->ix;i++){
      j=wzord->ind(i,alpha);
      cout << i+1 <<'\t' << alpha << '\t' << wzord->weight[i] << '\t' <<\
      tx[j] << '\t' << sx[j] << '\t' << pPsg->term+pPsg->sddr[j] << endl;
   }
}

WOrder *Thln::Moderator(WOrder *wpord,long clim){
   double nwt,owt;
   long cmt=0;
   WOrder *wzord;
   float alpha;

   nwt=wt[wpord->ind(0,alpha)];
   owt=nwt+1.0;
   while((owt!=nwt)&&(cmt<clim)){
      owt=nwt;
      wzord=Iterator();
      show_terms(wzord);
      cout << "docs " << cur->ix << endl;
      nwt=wt[wzord->ind(0,alpha)];
      if((owt!=nwt)&&(cmt+1<clim))delete wzord;
      cmt++;
   }
   return(wzord);
}

void Thln::Theme_write(ofstream &fout,WOrder *wpord){
   long i,j,k;
   long n=wpord->pInd->ix;
   float alpha;

   fout << ntrm << '\t' << n << endl;
   for(i=0;i<n;i++){
      j=wpord->ind(i,alpha);
      fout << alpha << '\t' << *(wpord->weight+i) << '\t' << pPsg->term+pPsg->sddr[j] << endl;
   }

   k=0;
   fout << k << '\t' << k << endl;
   fout << stng << '\t' << k << endl;
}

//WOrder

WOrder::WOrder(void) : Order(){
weight=NULL;
}

WOrder::WOrder(Order *pOrd,float *weg){
   if(!pOrd){cout << "Error, pOrd is NULL!" << endl;exit(0);}
   pInd=pOrd->pInd;
   pOrd->pInd=NULL;
   order=pOrd->order;
   pOrd->order=NULL;
   score=pOrd->score;
   pOrd->score=NULL;

   long i,n=pInd->ix;
   weight=new float[n];
   for(i=0;i<n;i++)weight[i]=weg[pInd->idx[i]];
   delete pOrd;
}
   
WOrder::WOrder(Order *pOrd,double *weg){
   if(!pOrd){cout << "Error, pOrd is NULL!" << endl;exit(0);}
   pInd=pOrd->pInd;
   pOrd->pInd=NULL;
   order=pOrd->order;
   pOrd->order=NULL;
   score=pOrd->score;
   pOrd->score=NULL;

   long i,n=pInd->ix;
   weight=new float[n];
   for(i=0;i<n;i++)weight[i]=(float)weg[pInd->idx[i]];
   delete pOrd;
}

WOrder::~WOrder(void){
   if(weight!=NULL)delete [] weight;
}

float WOrder::ave_sco(long n){
   long i;
   float xx,sum=0;

   for(i=0;i<n;i++){
      ind(i,xx);
      sum+=xx;
   }
   return(xx/((double)n));
}

float WOrder::dice_sco(WOrder *wpord){
 
   long i,j,k;
   float s1=0,s2=0,sc=0;
   float xi,yj;
   long m=pInd->ix;
   long n=wpord->pInd->ix;

   i=0;
   j=0;   
   while((i<m)&&(j<n)){
      if(ind(i,xi)<(k=wpord->ind(j,yj))){
         while((i<m)&&(ind(i,xi)<k)){
            s1+=xi;
            i++;
         }
      }
      if(i<m){
         if(ind(i,xi)==k){
            sc+=xi+yj;
            i++;j++;
         }
         else {
            k=ind(i,xi);
            while((j<n)&&(wpord->ind(j,yj)<k)){
               s2+=yj;
               j++;
            }
         }
      }
   }
   if(i<m){
      while(i<m){
         ind(i,xi);
         s1+=xi;
         i++;
      }
   }
   if(j<n){
      while(j<n){
         wpord->ind(j,yj);
         s2+=yj;
         j++;
      }
   }
   return(sc/(s1+s2+sc));
}

}
