#include <iostream>
#include <fstream>
#include <cstdlib>
#include <fcntl.h>
#include <sys/mman.h>
#include <cmath>
#include <cstring>
#include <cassert>
#include "Dist.h"
#include "Dtree.h"
#include <Btree.h>
#include <runn.h>
#include <Docum.h>
#include <Istreg.h>
#include <DataObj.h>
#include <Dbinbase.h>
#include <FBase.h>
#include <GBoost.h>
#include <Mang.h>


using namespace std;
namespace iret {

TreeBase::TreeBase(Dbinbase *pDb, Postg<char> *pPg) : Mang(pDb,pPg)  {
  tx=NULL;
  sx=NULL;
}

TreeBase::TreeBase(const char *namdbn,const char *nampsg) : Mang(namdbn,nampsg){
   tx=NULL;
   sx=NULL;
}

TreeBase::TreeBase(Docum &Doc) : Mang(Doc)  {
  tx=NULL;
  sx=NULL;
}

TreeBase::TreeBase(const char *namdoc) : Mang(namdoc){
   tx=NULL;
   sx=NULL;
}

TreeBase::~TreeBase(){
   if(tx) delete [] tx;
   if(sx) delete [] sx;
}

void TreeBase::init(long n){
   ptxx=(double)n;
   tx=new double[nwrd];
   sx=new double[nwrd];
}

long TreeBase::get_max_alpha(double cut){
   double n_t,n_st,min, max, diff;
   long flag;
   long nstw=0,sid=-1;
   double xx,frc,pt,qt,rt, xxi,xxo,xxstd, eps;
   double alx=-1000000.0,wstd;
   diff=nsx-nnx;   
   frc=nsx/nnx;
   eps=nnx/ncdoc;
  
   for(long i=0;i<nwrd;i++){
      if(!(n_t=*(tx+i)))continue;
      
      if(n_t&&(n_t<nnx)){
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         n_st=*(sx+i);
         flag=1;
         if(n_st==min){
            if(n_st-eps>n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+eps<n_t*frc)n_st=max+eps;
            else flag=0;
         }
      }
     
      else flag=0;

      if(flag){
         
         pt =n_st/nsx;
         qt =(n_t-n_st)/(nnx-nsx);
         rt =n_t/nnx;
         
         xx=-n_st*log(rt/pt) - (n_t-n_st)*log(rt/qt);
         xx-=(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         xx-=(nnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));
         if(alx<xx){
            wstd=log(pt*(1.0-qt))-log(qt*(1.0-pt));
            if(fabs(wstd)>=cut){
                alx=xx;
                sid=i;
            }
            
         }
         
         
         nstw++;
        
      }
      tx[i]=sx[i]=0;
   }
   
   return(sid);
}

long TreeBase::get_max_delta(double cut){
   double n_t,n_st,min, max, diff;
   long nstw=0,sid=-1,flag;
   double xx,rt_l,rt_r,rt;
   double alx=-1000000.0,wstd;
   double frc, eps;
   
   diff=nsx-nnx;   
   frc=nsx/nnx;
   eps=nnx/ncdoc;

   for(long i=0;i<nwrd;i++){
      if(!(n_t=*(tx+i))) {
         tx[i]=sx[i]=0;
         continue;
      }
      if(n_t&&(n_t<nnx)){
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         n_st=*(sx+i);
         flag=1;
         if(n_st==min){
            if(n_st-eps>n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+eps<n_t*frc)n_st=max+eps;
            else flag=0;
         }
      }
      else flag=0;

      if(flag){

         rt_l = (1.0/ptxx)*(n_st-(n_st*n_st)/n_t);
         rt_r = (1.0/ptxx)*((nsx-n_st) -((nsx-n_st)*(nsx-n_st))/(nnx-n_t));
         rt = (1.0/ptxx)*(nsx -(nsx*nsx)/nnx);
         
         xx=rt-rt_l-rt_r;
         
         if(alx<xx){
            wstd=rt;
            if(wstd>=cut){
                alx=xx;
                sid=i;
            }
            
         }
     }
     tx[i]=sx[i]=0;
   }
   return(sid);
}

long TreeBase::get_max_gain(double cut){
   double n_t,n_st,min, max;
   long nstw=0,sid=-1,flag;
   double xx,px_1, px_2, py_1, py_2, info, info_x, p1, p2;
   double alx=-1000000.0,wstd;
   double diff, frc,eps;
   long i;
   if(nsx==0 || nsx==nnx) {
      for(i=0;i<nwrd;i++) tx[i]=sx[i]=0.0;
      return(sid);
   }

   diff=nsx-nnx;   
   frc=nsx/nnx;
   eps=nnx/ncdoc;

   p1=nsx/nnx;
   p2=(nnx-nsx)/nnx;
   info=-p1*Log2(p1)-p2*Log2(p2);

   for(i=0;i<nwrd;i++){
      if(!(n_t=*(tx+i))) {
         tx[i]=sx[i]=0;
         continue;
      }
      if(n_t&&(n_t<nnx)){
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         n_st=*(sx+i);
         flag=1;
         if(n_st==min){
            if(n_st-eps>n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+eps<n_t*frc)n_st=max+eps;
            else flag=0;
         }
      }
      else flag=0;
      
      if(flag){
         px_1= n_st/n_t;
         px_2= (n_t-n_st)/n_t;
         py_1= (nsx-n_st)/(nnx-n_t);
         py_2= (nnx-nsx-n_t+n_st)/(nnx-n_t);
         info_x=(n_t/nnx)*(-px_1*Log2(px_1)-px_2*Log2(px_2));
         info_x+=((nnx-n_t)/nnx)*(-py_1*Log2(py_1)-py_2*Log2(py_2));
         
         xx=info-info_x;
         if(alx<xx){
            if(info>=cut){
                alx=xx;
                sid=i;
            }
            
         }
     }
     tx[i]=sx[i]=0;
   }
   return(sid);
}

long TreeBase::get_min_partition(double cut){
   double rt_l, rt_r, rt, frc;

   double max,min, diff, eps, tmax=0.0;
   long flag;
      
   diff=nsx-nnx;
   frc=(nsx)/(nnx);
   eps=nnx/ncdoc;

   double n_t,n_st;
   double w00,w01,w10,w11, w_gd,w_bd,w0,w1,w;
   
   long sid=-1;

   long i;
   if(nsx==0 || nsx==nnx) {
      for(i=0;i<nwrd;i++) tx[i]=sx[i]=0.0;
      return(sid);
   }
  
   w_gd=nsx;
   w_bd=nnx-nsx;
   w0=2.0*sqrt(w_gd*w_bd);
   if(w0<=cut){
      for(i=0;i<nwrd;i++) tx[i]=sx[i]=0.0;
      return(sid);
   }
 
   for(i=0;i<nwrd;i++){
      n_t=*(tx+i);
      
      if(n_t&&(n_t<nnx)){
         n_st=*(sx+i);
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         flag=1;
         if(n_st==min){
            if(n_st-eps>n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+eps<n_t*frc)n_st=max+eps;
            else flag=0;
         }
      }
      else flag=0;
      if(flag){
         w00=(nnx-nsx-n_t+n_st);
         w01=(n_t-n_st);
         w10=(nsx-n_st);
         w11=(n_st);
         w1=2.0*(sqrt(w11*w01)+sqrt(w10*w00));
         w=w0-w1;
         if(tmax<w) {
            tmax=w;
            sid=i;
         }  
      }
      tx[i]=sx[i]=0.0;

   }
   return(sid);


}

void TreeBase::countST(Index *ind1,Index *ind2){
   long i;
   nsx=nnx=0.0;
   ncdoc=0;
   if(ind1){ 
      for(i=0;i<ind1->ix;i++){
         this->counbDoc(ind1->idx[i]);
         nsx+=dt[ind1->idx[i]];
      }
      nnx=nsx;
      ncdoc=ind1->ix;
   }
   if(ind2){
      for(i=0;i<ind2->ix;i++){
         this->countDoc(ind2->idx[i]);
         nnx+=dt[ind2->idx[i]];
      }
      ncdoc+=ind2->ix;  
   }
  
}

void TreeBase::countDoc(long i){
   long j;
   double pt;
   pt=dt[i];
   this->read(i);
   for(i=0;i<nw;i++){
      j=*(nwd+i);
      (*(tx+j))+=pt;
   }
}


void TreeBase::counbDoc(long i){
   long j;
   double pt;
   pt=dt[i];
   this->read(i);
   for(i=0;i<nw;i++){
      j=*(nwd+i);
      (*(sx+j))+=pt;
      (*(tx+j))+=pt;
   }
}

void TreeBase::zerot(void){
   for(long i=0;i<nwrd;i++)*(tx+i)=0.0;
}

void TreeBase::zeros(void){
   for(long i=0;i<nwrd;i++)*(sx+i)=0.0;
}

//Knode code

KNode::KNode(Index *ind1, Index *ind2){
   snd=ind1;
   nnd=ind2;
   
}

KNode::~KNode(){
   if(snd != NULL) delete snd;
   if(nnd != NULL) delete nnd;
}


void KNode_CT::debug(void){
   cout <<" address of the snd: " << (long) snd <<endl;
   cout <<" address of the nnd: " << (long) nnd <<endl;
   cout <<" address of the up: " << (long) up <<endl;
   cout <<" address of the bin: " << (long)bin <<endl;
   cout <<" address of the bout: " << (long)bout <<endl;
   cout <<" term number : " << split <<endl;
   cout <<" alpha value : " << alpha <<endl;
   cout <<" win value: " << win <<endl;
   cout <<" wout value : " << wout <<endl;
   cout <<" wstd value : " << wstd <<endl;
}

KNode_CT::KNode_CT(Index *ind1, Index *ind2) : KNode(ind1, ind2){

}

KNode_CT::~KNode_CT() {

}

int KNode_CT::Split(double cut, int level,TreeBase *pBb){
   
   long flag, min, max, diff,i;
   double pt, qt, rt, frc;
   if(dep==level) {
      if(snd) { 
        delete snd;
        snd=NULL;
      }
      if(nnd) {
         delete nnd;
         nnd=NULL;
      }
      return(0);
   }
   pBb->countST(snd,nnd);
   split=pBb->get_max_alpha(cut);
   
   if(split<0) {
      if(snd) { 
        delete snd;
        snd=NULL;
      }
      if(nnd) {
         delete nnd;
         nnd=NULL;
      }
      return(0);
   }
   Index *sbin, *nbin, *sbout, *nbout;
   if(snd){
      sbin=snd->cbool_And(pBb->Pst+split);
      sbout=snd->cbool_Butnot(pBb->Pst+split);
   }
   else {
      sbin=NULL;
      sbout=NULL;
   }
   if(nnd){
      nbin=nnd->cbool_And(pBb->Pst+split);
      nbout=nnd->cbool_Butnot(pBb->Pst+split);
   }
   else {
      nbin=NULL;
      nbout=NULL;
   }
   
   bin=new KNode_CT(sbin,nbin);
   bin->up=this;
   bin->flio=0;
   bin->dep=dep+1;
   
      

   bout=new KNode_CT(sbout,nbout);
   bout->up=this;
   bout->flio=1;
   bout->dep=dep+1;

   
   
   n_st=n_t=0.0;
   if(bin->snd){
      for(i=0;i<bin->snd->ix;i++) 
         n_st+=pBb->dt[bin->snd->idx[i]];
   }
   n_t=n_st;
   if(bin->nnd){
      for(i=0;i<bin->nnd->ix;i++) n_t+=pBb->dt[bin->nnd->idx[i]];
   }

   nsx=nnx=0.0;
   if(snd){
      for(i=0;i<snd->ix;i++) nsx+=pBb->dt[snd->idx[i]];
   }
   
   nnx=nsx;
   if(nnd){
      for(i=0;i<nnd->ix;i++) nnx+=pBb->dt[nnd->idx[i]];
   }     
   
   
   Param<KNode_CT> Pm(this);
   Pm.ptxx=pBb->ptxx;
   Pm.Set_alpha();
   win=Pm.win;
   wout=Pm.wout;
   wstd=Pm.wstd;
   alpha=Pm.alpha;
   
   if(snd) { 
     delete snd;
     snd=NULL;
   }
  
   if(nnd) {
      delete nnd;
      nnd=NULL;
   }
   return(1);         
}

double KNode_CT::Prune(void){
   double xx=0;
   rdiff=alpha;
   lfcnt=2.0;
   if(bin){
      rdiff+=bin->alpha;
      lfcnt+=(bin->lfcnt-1.0);
   }
   if(bout){
      rdiff+=bout->alpha;
      lfcnt+=(bout->lfcnt-1.0);
   }  
   return(rdiff/(lfcnt-1.0));

}

void KNode_CT::Score(double *sxx, TreeBase *pBx){
   long i,j,k;
   Index *ind, *ond;
   if(wstd && snd){
      ind=snd->cbool_And(pBx->Pst+split);
      ond=snd->cbool_Butnot(pBx->Pst+split);
      
      delete snd;
      snd=NULL;
      
      if(ind){ 
         for(i=0;i<ind->ix;i++) sxx[ind->idx[i]]+=win;
         
      }
      if(ond){
         for(i=0;i<ond->ix;i++) sxx[ond->idx[i]]+=wout;
      }
      if(bin) bin->snd=ind;
      else delete ind;
      
      if(bout) bout->snd=ond;
      else delete ond;      
      
   }
} 





//----Begin of the Cart----------------------------

void KNode_Cart::debug(void){
   cout <<" address of the snd: " << (long) snd <<endl;
   cout <<" address of the nnd: " << (long) nnd <<endl;
   cout <<" address of the up: " << (long) up <<endl;
   cout <<" address of the bin: " << (long)bin <<endl;
   cout <<" address of the bout: " << (long)bout <<endl;
   cout <<" term number : " << split <<endl;
   cout <<" alpha value : " << alpha <<endl;
   cout <<" win value: " << win <<endl;
   cout <<" wout value : " << wout <<endl;
   cout <<" wstd value : " << wstd <<endl;
}

KNode_Cart::KNode_Cart(Index *ind1, Index *ind2) : KNode(ind1, ind2){

}

KNode_Cart::~KNode_Cart() {

}

int KNode_Cart::Split(double cut, int level,TreeBase *pBb){
   long i;
   if(dep==level) {
      if(snd) { 
        delete snd;
        snd=NULL;
      }
      if(nnd) {
         delete nnd;
         nnd=NULL;
      }
      return(0);
   }

   pBb->countST(snd,nnd);
   split=pBb->get_max_delta(cut);
   
   if(split<0) {
      if(snd) { 
        delete snd;
        snd=NULL;
      }
      if(nnd) {
         delete nnd;
         nnd=NULL;
      }
      return(0);
   }
   Index *sbin, *nbin, *sbout, *nbout;
   if(snd){
      sbin=snd->cbool_And(pBb->Pst+split);
      sbout=snd->cbool_Butnot(pBb->Pst+split);
   }
   else {
      sbin=NULL;
      sbout=NULL;
   }
   if(nnd){
      nbin=nnd->cbool_And(pBb->Pst+split);
      nbout=nnd->cbool_Butnot(pBb->Pst+split);
   }
   else {
      nbin=NULL;
      nbout=NULL;
   }

   bin=new KNode_Cart(sbin,nbin);
   bin->up=this;
   bin->flio=0;
   bin->dep=dep+1;

   bout=new KNode_Cart(sbout,nbout);
   bout->up=this;
   bout->flio=1;
   bout->dep=dep+1;

   n_st=n_t=0.0;
   if(bin->snd){
      for(i=0;i<bin->snd->ix;i++) 
         n_st+=pBb->dt[bin->snd->idx[i]];
   }
   n_t=n_st;
   if(bin->nnd){
      for(i=0;i<bin->nnd->ix;i++) n_t+=pBb->dt[bin->nnd->idx[i]];
   }

   nsx=nnx=0.0;
   if(snd){
      for(i=0;i<snd->ix;i++) nsx+=pBb->dt[snd->idx[i]];
   }
   
   nnx=nsx;
   if(nnd){
      for(i=0;i<nnd->ix;i++) nnx+=pBb->dt[nnd->idx[i]];
   }     



   Param<KNode_Cart> Pm(this);
   Pm.ptxx=pBb->ptxx;
   Pm.Set_delta();
   
   win=Pm.win;
   wout=Pm.wout;
   wstd=Pm.wstd;
   alpha=Pm.alpha;

   if(snd) { 
     delete snd;
     snd=NULL;
   }
  
   if(nnd) {
      delete nnd;
      nnd=NULL;
   }
   return(1);   
}

void KNode_Cart::Score(double *sxx, TreeBase *pBx){
   long i,j,k;
   Index *ind, *ond;
   if(wstd && snd){
      ind=snd->cbool_And(pBx->Pst+split);
      ond=snd->cbool_Butnot(pBx->Pst+split);
      delete snd;
      snd=NULL;
      
      if(bin) bin->snd=ind;
      else if(ind){
         for(i=0;i<ind->ix;i++) sxx[ind->idx[i]]+=win;
         delete ind;
         
      }
      if(bout) bout->snd=ond;
      else if(ond){
         for(i=0;i<ond->ix;i++) sxx[ond->idx[i]]+=wout;
         delete ond;
         
      }
      
   }
   

} 


double KNode_Cart::Prune(void){
   rdiff=alpha;
   lfcnt=2.0;
   if(bin){
      rdiff+=bin->alpha;
      lfcnt+=bin->lfcnt-1.0;
   }
   if(bout){
      rdiff+=bout->alpha;
      lfcnt+=bout->lfcnt-1.0;
   }  
   return(rdiff/(lfcnt-1.0));

}




/*void KNode_Cart::debug(void){
   rdiff=alpha;
   lfcnt=2.0;
   if(bin){
      rdiff-=bin->alpha;
      lfcnt-=bin->lfcnt-1.0;
   }
   if(bout){
      rdiff-=bout->alpha;
      lfcnt-=bout->lfcnt-1.0;
   }  
}*/





//-------------------------End of Cart--------------------------




//Begin of C45//
void KNode_C45::debug(void){
   cout <<" address of the snd: " << (long) snd <<endl;
   cout <<" address of the nnd: " << (long) nnd <<endl;
   cout <<" address of the up: " << (long) up <<endl;
   cout <<" address of the bin: " << (long)bin <<endl;
   cout <<" address of the bout: " << (long)bout <<endl;
   cout <<" term number : " << split <<endl;
   cout <<" alpha value : " << alpha <<endl;
   cout <<" win value: " << win <<endl;
   cout <<" wout value : " << wout <<endl;
   cout <<" wstd value : " << wstd <<endl;
}


KNode_C45::KNode_C45(Index *ind1, Index *ind2) : KNode(ind1, ind2){

}

KNode_C45::~KNode_C45() {

}

int KNode_C45::Split(double cut, int level,TreeBase *pBb){
   long i;
   if(dep==level) {
      if(snd) { 
        delete snd;
        snd=NULL;
      }
      if(nnd) {
         delete nnd;
         nnd=NULL;
      }
      return(0);
   }

   double lim=pBb->lim;
   pBb->countST(snd,nnd);
   split=pBb->get_max_gain(cut);
  
   if(split<0) {
      if(snd) { 
        delete snd;
        snd=NULL;
      }
      if(nnd) {
         delete nnd;
         nnd=NULL;
      }
      return(0);
   }
      
   Index *sbin, *nbin, *sbout, *nbout;
   if(snd){
      sbin=snd->cbool_And(pBb->Pst+split);
      sbout=snd->cbool_Butnot(pBb->Pst+split);
   }
   else {
      sbin=NULL;
      sbout=NULL;
   }
   if(nnd){
      nbin=nnd->cbool_And(pBb->Pst+split);
      nbout=nnd->cbool_Butnot(pBb->Pst+split);
   }
   else {
      nbin=NULL;
      nbout=NULL;
   }

   bin=new KNode_C45(sbin,nbin);
   bin->up=this;
   bin->flio=0;
   bin->dep=dep+1;

   bout=new KNode_C45(sbout,nbout);
   bout->up=this;
   bout->flio=1;
   bout->dep=dep+1;
   
   n_st=n_t=0.0;
   if(bin->snd){
      for(i=0;i<bin->snd->ix;i++) 
         n_st+=pBb->dt[bin->snd->idx[i]];
   }
   n_t=n_st;
   if(bin->nnd){
      for(i=0;i<bin->nnd->ix;i++) n_t+=pBb->dt[bin->nnd->idx[i]];
   }

   nsx=nnx=0.0;
   if(snd){
      for(i=0;i<snd->ix;i++) nsx+=pBb->dt[snd->idx[i]];
   }
   
   nnx=nsx;
   if(nnd){
      for(i=0;i<nnd->ix;i++) nnx+=pBb->dt[nnd->idx[i]];
   }     



 
   Param<KNode_C45> Pm(this);
   Pm.ptxx=pBb->ptxx;

   Pm.Set_gain(lim);
   win=Pm.win;
   wout=Pm.wout;
   wstd=Pm.wstd;
   alpha=Pm.alpha;
   
   //Pm.Set_delta();
   //alpha=Pm.alpha;

   if(snd) { 
     delete snd;
     snd=NULL;
   }
  
   if(nnd) {
      delete nnd;
      nnd=NULL;
   }
   return(1);   
}

double KNode_C45::Prune(void){
   rdiff=alpha;
   if(bin) rdiff+=bin->alpha;
   if(bout) rdiff+=bout->alpha;
   return(rdiff/wstd);

}

/*double KNode_C45::Prune(void){
   rdiff=alpha;
   lfcnt=2.0;
   if(bin){
      rdiff+=bin->alpha;
      lfcnt+=bin->lfcnt-1.0;
   }
   if(bout){
      rdiff+=bout->alpha;
      lfcnt+=bout->lfcnt-1.0;
   }  
   return(rdiff/(lfcnt-1.0));

}*/


void KNode_C45::Score(double *sxx, TreeBase *pBx){
   long i,j,k;
   Index *ind, *ond;
   if(wstd && snd){
      ind=snd->cbool_And(pBx->Pst+split);
      ond=snd->cbool_Butnot(pBx->Pst+split);
      delete snd;
      snd=NULL;
      
      if(bin) bin->snd=ind;
      else if(ind){
         for(i=0;i<ind->ix;i++) sxx[ind->idx[i]]+=win;
         delete ind;
      }
      if(bout) bout->snd=ond;
      else if(ond){
         for(i=0;i<ond->ix;i++) sxx[ond->idx[i]]+=wout;
         delete ond;
      }
      
   }
  

} 



//End of C45




//Begin of Boost


void KNode_Boost::debug(void){
   cout <<" address of the snd: " << (long) snd <<endl;
   cout <<" address of the nnd: " << (long) nnd <<endl;
   cout <<" address of the up: " << (long) up <<endl;
   cout <<" address of the bin: " << (long)bin <<endl;
   cout <<" address of the bout: " << (long)bout <<endl;
   cout <<" term number : " << split <<endl;
   cout <<" alpha value : " << alpha <<endl;
   cout <<" win value: " << win <<endl;
   cout <<" wout value : " << wout <<endl;
   cout <<" wstd value : " << wstd <<endl;
}

KNode_Boost::KNode_Boost(Index *ind1, Index *ind2) : KNode(ind1, ind2){

}

KNode_Boost::~KNode_Boost() {

}


int KNode_Boost::Split(double cut,int level,TreeBase *pBb){
   
   long flag, min, max, diff,i,nn;
   double pt, qt, rt, frc;
   
   //Index *train,*sndc;
   //train=snd->cbool_Or(nnd);
   
   /*GBoost Gb(train->ix,train->ix,eps);
   //sndc=train->Subvalue(snd);
   //Gb.init(sndc, 10000);*/
   //for(i=0;i<train->ix;i++) pBb->dt[train->idx[i]]=1.0/train->ix;
   if(dep==level) {
      if(snd) { 
        delete snd;
        snd=NULL;
      }
      if(nnd) {
         delete nnd;
         nnd=NULL;
      }
      return(0);
   }
   pBb->countST(snd,nnd);
   
      
   split=pBb->get_min_partition(cut);
   
   if(split<0) {
      if(snd) { 
        delete snd;
        snd=NULL;
      }
      if(nnd) {
         delete nnd;
         nnd=NULL;
      }
      return(0);
   }

     
   Index *sbin, *nbin, *sbout, *nbout;
   if(snd){
      sbin=snd->cbool_And(pBb->Pst+split);
      sbout=snd->cbool_Butnot(pBb->Pst+split);
      
   }
   else {
      sbin=NULL;
      sbout=NULL;
   }
   if(nnd){
      nbin=nnd->cbool_And(pBb->Pst+split);
      nbout=nnd->cbool_Butnot(pBb->Pst+split);
      
   }
   else {
      nbin=NULL;
      nbout=NULL;
   }
   
   bin=new KNode_Boost(sbin,nbin);
   bin->up=this;
   bin->flio=0;
   bin->dep=dep+1;

   bout=new KNode_Boost(sbout,nbout);
   bout->up=this;
   bout->flio=1;
   bout->dep=dep+1;
   
   n_st=n_t=0.0;
   if(bin->snd){
      for(i=0;i<bin->snd->ix;i++) 
         n_st+=pBb->dt[bin->snd->idx[i]];
   }
   n_t=n_st;
   if(bin->nnd){
      for(i=0;i<bin->nnd->ix;i++) n_t+=pBb->dt[bin->nnd->idx[i]];
   }
  
   

   nsx=nnx=0.0;
   if(snd){
      for(i=0;i<snd->ix;i++) nsx+=pBb->dt[snd->idx[i]];
   }
   
   nnx=nsx;
   if(nnd){
      for(i=0;i<nnd->ix;i++) nnx+=pBb->dt[nnd->idx[i]];
   }     




   Param<KNode_Boost> Pm(this);
   Pm.ptxx=pBb->ptxx;
   
   
   Pm.Set_boost();
   alpha=Pm.alpha;
   win=Pm.win;
   wout=Pm.wout;
   wstd=Pm.wstd;
    
   if(snd) { 
     delete snd;
     snd=NULL;
   }
  
   if(nnd) {
      delete nnd;
      nnd=NULL;
   }
   return(1);         
}

double KNode_Boost::Prune(void){
   double xx=0;
   rdiff=alpha;
   lfcnt=2.0;
   if(bin){
      rdiff+=bin->alpha;
      lfcnt+=(bin->lfcnt-1.0);
   }
   if(bout){
      rdiff+=bout->alpha;
      lfcnt+=(bout->lfcnt-1.0);
   }  
   return(rdiff/(lfcnt-1.0));

}

/*void KNode_Boost::Score(double *sxx, TreeBase *pBx){
   long i,j,k;
   Index *ind, *ond;
   if(wstd && snd){
      ind=snd->cbool_And(pBx->Pst+split);
      ond=snd->cbool_Butnot(pBx->Pst+split);
      
      delete snd;
      snd=NULL;
      
      if(ind){ 
         for(i=0;i<ind->ix;i++) sxx[ind->idx[i]]+=win;
         
      }
      if(ond){
         for(i=0;i<ond->ix;i++) sxx[ond->idx[i]]+=wout;
      }
      if(bin) bin->snd=ind;
      
      
      if(bout) bout->snd=ond;
      
      
   }
} *///score every node 


void KNode_Boost::Score(double *sxx, TreeBase *pBx){
   long i,j,k;
   Index *ind, *ond;
   if(wstd && snd){
      ind=snd->cbool_And(pBx->Pst+split);
      ond=snd->cbool_Butnot(pBx->Pst+split);
      delete snd;
      snd=NULL;
      
      if(bin) bin->snd=ind;
      else if(ind) {
        for(i=0;i<ind->ix;i++) sxx[ind->idx[i]]+=win;
        delete ind;
      }
      if(bout) bout->snd=ond;
      else if(ond){
        for(i=0;i<ond->ix;i++) sxx[ond->idx[i]]+=wout;
        delete ond;
      }
      
   }
   

} 

//End of BOOST





template <class KNodx> 
Dtree<KNodx>::Dtree(long nd,TreeBase *pBx) : FBase("ktree","null"){
   ndoc=nd;
   pBb=pBx;
   root=NULL;
   sco=NULL;
   pflag=1;
}

template <class KNodx> 
Dtree<KNodx>::Dtree(long nd,TreeBase *pBx, int flag) : FBase("ktree","null"){
   ndoc=nd;
   pBb=pBx;
   root=NULL;
   sco=NULL;
   pflag=flag;
}

template <class KNodx>
Dtree<KNodx>::~Dtree(){
   long flag;
   if(sco!=NULL) delete [] sco;
   KNodx *kkn, *kko;
   if(!root) return;
   kkn=root;
   
   if(root->bin){
      kkn=root->bin;
   }else if(root->bout){
      kkn=root->bout;
   }
   else{
      delete root;
      return;
   }      
   while( kkn != root ){
      flag=1;
      while(flag){
         while(kkn->bin){
            kkn=kkn->bin;
         }
         if(kkn->bout) kkn=kkn->bout;
         else flag=0;
      }
      
      kko=kkn;  
      flag=kkn->flio;
      while(flag){
         kkn=kkn->up;
         kkn->bout=NULL;
         delete kko;
         kko=kkn;
         flag=kkn->flio; 
      }
      
      if(kkn !=root){
         kkn=kkn->up;
         kkn->bin=NULL;
         delete kko;
         if(kkn->bout) kkn=kkn->bout;
      }
   }
   delete root;
}

template <class KNodx>
int Dtree<KNodx>::Build_Tree(double cut, int level, Index *subb, Index *nubb){
   Index *sub=new Index(subb);
   Index *nub=new Index(nubb);
   int flag;
   nnode=1;
   KNodx *kko, *kkn;
   pBb->zerot();
   pBb->zeros(); 
   root = new KNodx(sub, nub);
   kkn=root; 
   kkn->flio=0;
   kkn->dep=0;
   
   //kkn->flot=1;//ch

   flag=0;
   if(kkn->Split(cut,level,pBb)==0) return 0;
   
   kkn=kkn->bin;
   kko=NULL;
   nnode+=2;
   while( kkn != root ){
      while(kkn->Split(cut,level,pBb)){
         kkn=kkn->bin;
         nnode+=2;
         mark(pflag,nnode,1,"nodes are split");
         
      }
      kko=kkn;
      
      flag=kkn->flio;
      while(flag){
         kkn=kkn->up;
         if(kko){
            kkn->bout=NULL;
            delete kko;
            nnode--;
            kko=NULL; 
         }  
         flag=kkn->flio;
      }
      if(kkn !=root){
         kkn=kkn->up;
         if(kko){
            kkn->bin=NULL;
            delete kko;
            nnode--;
            kko=NULL;
         }  
         kkn=kkn->bout;
      }
   }
   mark(pflag,nnode,1,"nodes are constructed");
   return 1;
}

template <class KNodx>
void Dtree<KNodx>::Dest_Tree(void){
   long flag;
   KNodx *kkn, *kko;
   if(!root) return;
   kkn=root;

   if(root->bin){
      kkn=root->bin;
   }else if(root->bout){
      kkn=root->bout;
   }
   else{
      delete root;
      return;
   }
   while( kkn != root ){
      flag=1;
      while(flag){
         while(kkn->bin){
            kkn=kkn->bin;
         }
         if(kkn->bout) kkn=kkn->bout;
         else flag=0;
      }

      kko=kkn;
      flag=kkn->flio;
      while(flag){
         kkn=kkn->up;
         kkn->bout=NULL;
         delete kko;
         kko=kkn;
         flag=kkn->flio;
      }

      if(kkn !=root){
         kkn=kkn->up;
         kkn->bin=NULL;
         delete kko;
         if(kkn->bout) kkn=kkn->bout;
      }
   }
   delete root;
   root=NULL;
}

template <class KNodx>
void Dtree<KNodx>::Score_Tree(Index *temp){
   Index *test=new Index(temp);
   long flag,i, hflag;
   
   if(sco!=NULL) {
      delete [] sco;
      sco=NULL;
   }

   sco=new double[ndoc];
   for(i=0;i<ndoc;i++) sco[i]=0.0;

   KNodx *kkn, *kko;
   kkn=NULL;
   if(!root) return;
   root->snd=test;
   root->Score(sco, pBb);
   
   if(root->bin){
      kkn=root->bin;
      hflag=0;
      kkn->Score(sco, pBb);   
   }else if(root->bout){
      kkn=root->bout;
      hflag=0;
      kkn->Score(sco, pBb);
   }
       
   while( kkn != root && kkn ){
      flag=1;
      
      while(flag){
         
         while(kkn->bin && !hflag){
            
            kkn=kkn->bin;
            hflag=0;
            kkn->Score(sco, pBb);
         }
         if(kkn->bout) {
            kkn=kkn->bout;
            hflag=0;
            kkn->Score(sco, pBb);
         }
         else flag=0;
      }
      
       
      flag=kkn->flio;
      while(flag){
    
         kkn=kkn->up;
         hflag=2;
         flag=kkn->flio; 
      }
      
      if(kkn !=root){
         kkn=kkn->up;
         hflag=1;
         if(kkn->bout){
            kkn=kkn->bout;
            hflag=0;
            kkn->Score(sco, pBb);
         } 

     }
   }
   
}

template <class KNodx>
double Dtree<KNodx>::Prune_Tree(void){
   
   long flag,i, hflag;
   int fl_rem;
   double alpha_min=1000000.0,xx, delta=1.0E-20;
   KNodx *kkn, *kko, *kkm=NULL;
   
   if(!root) {
      cout <<"root node is null"<<endl;
      return(0.0);
   }   
   if(root->bin){
      kkn=root->bin;
      hflag=0;
   }else if(root->bout){
      kkn=root->bout;
      hflag=0;
   }else{
      kkn=root;
      xx=kkn->Prune();
      if(xx < alpha_min) {
         alpha_min=xx;
         kkm=kkn;
         
      }
   }
   
   
   while( kkn != root ){
      flag=1;
      
      while(flag){
         
         while(kkn->bin && !hflag){
            kkn=kkn->bin;
            hflag=0;
         }
         if(kkn->bout) {
            kkn=kkn->bout;
            hflag=0;
         }
         else if(!hflag){
            xx=kkn->Prune();
            if(xx < alpha_min) {
               alpha_min=xx;
               kkm=kkn;
               
            }
            flag=0;
         }
         else{
            //cout <<"Is it possible?"<<" "<<kkn->bin<<" "<<hflag<<endl;
            if(!kkn->bin) {cout <<"should not be NULL"<<endl; exit(0);}
            if(hflag!=1) {cout <<"The current node should come from the left child"<<endl; exit(0);}
            flag=0;
         }
                           
         
      }
            
       
      flag=kkn->flio;
      while(flag){
         
         kkn=kkn->up;
         flag=kkn->flio;
         hflag=2; 
         if(!kkn->bin){
            xx=kkn->Prune();
            if(xx < alpha_min) {
               alpha_min=xx;
               kkm=kkn;
               
            }

         }
         
         else{
            xx=kkn->Prune();
            if(xx < alpha_min) {
               alpha_min=xx;
               kkm=kkn;
               
            }

         }
         
      }
       
   
      if(kkn !=root){
         
         kkn=kkn->up;
         hflag=1;
         if(kkn->bout){
            kkn=kkn->bout;
            hflag=0;
         } else{
            xx=kkn->Prune();
            if(xx < alpha_min) {
               alpha_min=xx;
               kkm=kkn;
               
            }

         }
         
      }
      
   }
   
   if(!kkm) { cout <<"Unexpected NULL kkm"<<endl; exit(0);}
   fl_rem=kkm->flio;
   kkm->flio=0;
   kkn=kkm;
   
         

   if(kkm->bin){
     
      kkn=kkm->bin;
   }else if(kkm->bout){
     
      kkn=kkm->bout;
   }
   else{
      
      if(kkm==root) {
         cout <<"Attempt to prune root"<<endl;
         return(0.0);
      }
      kkn=kkm->up;
      if(fl_rem) kkn->bout=NULL;
      else kkn->bin=NULL;
      delete kkm;
      return(alpha_min+delta);
      
   } 
       
   while( kkn != kkm ){
      
      flag=1;
      while(flag){
         while(kkn->bin){
            kkn=kkn->bin;
         }
         if(kkn->bout) kkn=kkn->bout;
         else flag=0;
         
      }
      
      kko=kkn;  
      flag=kkn->flio;
      while(flag){
         
         kkn=kkn->up;
         kkn->bout=NULL;
         delete kko;
         kko=kkn;
         flag=kkn->flio; 
      }
      
      if(kkn !=kkm){
         kkn=kkn->up;
         kkn->bin=NULL;
         delete kko;
         if(kkn->bout) kkn=kkn->bout;
      }
      
   }
   
   if(kkm==root) {
      cout <<"Attempt to back up from the root"<<endl;
      return(0.0);
   }
   
   kkn=kkm->up;
   if(fl_rem) kkn->bout=NULL;
   else kkn->bin=NULL;
   delete kkm;

     
   cout <<"value of minimum alpha= "<<alpha_min<<endl;
   return(alpha_min+delta);



}


  

template <class KNodx>
int Dtree<KNodx>::Prune_Tree(double level){
  
   long flag,i, hflag;
   int fl_rem;
   double alpha_min=1000000.0,xx, xxx;
   
   
   KNodx *kkn, *kko, *kkm=NULL;
   
   if(!root) {
      cout <<"NULL root"<<endl;
      return(0);
   }   
   if(root->bin){
      kkn=root->bin;
      hflag=0;
   }else if(root->bout){
      kkn=root->bout;
      hflag=0;
   }else{
      kkn=root;
      xx=kkn->Prune();
      if(xx < alpha_min) {
         alpha_min=xx;
         kkm=kkn;
         
      }
   }
   
   
   while( kkn != root ){
      flag=1;
      while(flag){
         while(kkn->bin && !hflag){
            kkn=kkn->bin;
            hflag=0;
         }
         if(kkn->bout) {
            kkn=kkn->bout;
            hflag=0;
         }
         else if(!hflag){
            xx=kkn->Prune();
            if(xx < alpha_min) {
               alpha_min=xx;
               kkm=kkn;
               
            }
            flag=0;
         }
         else{
            if(!kkn->bin) {cout <<"should not be NULL"<<endl; exit(0);}
            if(hflag!=1) {cout <<"The current node should come from the left child"<<endl; exit(0);}
            flag=0;
         }
         
         
      }
            
       
      flag=kkn->flio;
      while(flag){
         kkn=kkn->up;
         flag=kkn->flio;
         hflag=2; 
         if(!kkn->bin){
            xx=kkn->Prune();
            if(xx < alpha_min) {
               alpha_min=xx;
               kkm=kkn;
               
            }

         }
         else{
            xx=kkn->Prune();
            if(xx < alpha_min) {
               alpha_min=xx;
               kkm=kkn;
               
            }

         }
      }
        
   
      if(kkn !=root){
         kkn=kkn->up;
         hflag=1;
         if(kkn->bout){
            kkn=kkn->bout;
            hflag=0;
         } else{
            xx=kkn->Prune();
            if(xx < alpha_min) {
               alpha_min=xx;
               kkm=kkn;
               
            }

         }
         
      }
   }
  
   if(alpha_min > level) {
      cout <<"Minimum Alpha = " << alpha_min<<endl;
      cout <<"Level to prune = " << level<<endl;
      return(0);
   }
   if(!kkm) { cout <<"Unexpected NULL kkm"<<endl; exit(0);}
   fl_rem=kkm->flio;
   kkm->flio=0;
   kkn=kkm;
   
      

   if(kkm->bin){
      kkn=kkm->bin;
   }else if(kkm->bout){
      kkn=kkm->bout;
   }
   else{
      if(kkm==root) {
         cout <<"Attempt to prune root"<<endl;
         return(0);
      }
      kkn=kkm->up;
      if(fl_rem) kkn->bout=NULL;
      else kkn->bin=NULL;
      delete kkm;
      return(1);
   }      
   while( kkn != kkm ){
      flag=1;
      while(flag){
         while(kkn->bin){
            kkn=kkn->bin;
         }
         if(kkn->bout) kkn=kkn->bout;
         else flag=0;
      }
      
      kko=kkn;  
      flag=kkn->flio;
      while(flag){
         kkn=kkn->up;
         kkn->bout=NULL;
         delete kko;
         kko=kkn;
         flag=kkn->flio; 
      }
      
      if(kkn !=kkm){
         kkn=kkn->up;
         kkn->bin=NULL;
         delete kko;
         if(kkn->bout) kkn=kkn->bout;
      }
   }
   kkn=kkm->up;
   if(fl_rem) kkn->bout=NULL;
   else kkn->bin=NULL;
   delete kkm;

     
   cout <<"value of minimum alpha= "<<alpha_min<<endl;
   return(1);



}

template <class KNodx>
void Dtree<KNodx>::gopen_write(const char *nam){
   set_name(nam);
   pfout=get_Ostr("t",ios::out);
   ntree=0;
}

template <class KNodx>
void Dtree<KNodx>::gclose_write(void){
   dst_Ostr(pfout);
   pfout=get_Ostr("n",ios::out);
   *pfout << ntree << endl;
   dst_Ostr(pfout);
}

template <class KNodx>
void Dtree<KNodx>::write_Tree(void){
   long i;
   long *pr=new long[nnode];
   long *lc=new long[nnode];
   long *rc=new long[nnode];
   ntree++;
   for(i=0;i<nnode;i++) {
      lc[i]=-1;
      rc[i]=-1;
   }
   *pfout <<nnode<<endl;
   
   long flag, hflag, cnod=0, curr=0;
  

   KNodx *kkn, *kko;
   kkn=NULL;
   
   if(!root) { cout <<"Tree is NULL"<<endl; return;}
   
   *pfout<<curr<<" "<<root->split<<" "<<root->win<<"  "<<root->wout<<endl;
   pr[curr]=0;
  
   if(root->bin){
      lc[curr]=++cnod;
      kkn=root->bin;
      pr[cnod]=curr;
      curr=cnod;
      *pfout<<curr <<" "<<kkn->split<<" "<<kkn->win<<"  "<<kkn->wout<<endl;
      hflag=0;
   }else if(root->bout){
      rc[curr]=++cnod;
      kkn=root->bout;
      pr[cnod]=curr;
      curr=cnod;
      *pfout<<curr<<" "<<kkn->split<<" "<<kkn->win<<"  "<<kkn->wout<<endl;
      hflag=0;
   }
       
   while( kkn != root && kkn ){
      flag=1;
      
      while(flag){
         
         while(kkn->bin && !hflag){
            lc[curr]=++cnod;
            kkn=kkn->bin;
            pr[cnod]=curr;
            curr=cnod;
            *pfout<<curr<<" "<<kkn->split<<" "<<kkn->win<<"  "<<kkn->wout<<endl;
            hflag=0;
         }
         if(kkn->bout) {
            rc[curr]=++cnod;
            kkn=kkn->bout;
            pr[cnod]=curr;
            curr=cnod;
            *pfout<<curr<<" "<<kkn->split<<" "<<kkn->win<<"  "<<kkn->wout<<endl;
            hflag=0;
         }
         else flag=0;
      }
      
       
      flag=kkn->flio;
      while(flag){
         curr=pr[curr];
         kkn=kkn->up;
         hflag=2;
         flag=kkn->flio; 
      }
      
      if(kkn !=root){
         curr=pr[curr];
         kkn=kkn->up;
         hflag=1;
         if(kkn->bout){
            rc[curr]=++cnod;
            kkn=kkn->bout;
            pr[cnod]=curr;
            curr=cnod;
            *pfout<<curr<<" "<<kkn->split<<" "<<kkn->win<<"  "<<kkn->wout<<endl;
            hflag=0;
         } 

     }
   }
   for(i=0;i<nnode;i++) *pfout<<pr[i]<<" "<<lc[i]<<" "<<rc[i]<<endl;
  

}



template <class KNodx>
void Dtree<KNodx>::write_Tree_Binary(void){
   long i;
   long *pr=new long[nnode];
   long *lc=new long[nnode];
   long *rc=new long[nnode];
   ntree++;
   for(i=0;i<nnode;i++) {
      lc[i]=-1;
      rc[i]=-1;
   }
   pfout->write((char*)&nnode,sizeof(long));
   
   
   long flag, hflag, cnod=0, curr=0;
  

   KNodx *kkn, *kko;
   kkn=NULL;
   
   if(!root) { cout <<"Tree is NULL"<<endl; return;}
   
   pfout->write((char*)&(root->split),sizeof(long));
   pfout->write((char*)&(root->win),sizeof(double));
   pfout->write((char*)&(root->wout),sizeof(double));
   
   pr[curr]=0;
  
   if(root->bin){
      lc[curr]=++cnod;
      kkn=root->bin;
      pr[cnod]=curr;
      curr=cnod;
      pfout->write((char*)&(kkn->split),sizeof(long));
      pfout->write((char*)&(kkn->win),sizeof(double));
      pfout->write((char*)&(kkn->wout),sizeof(double));
      
      hflag=0;
   }else if(root->bout){
      rc[curr]=++cnod;
      kkn=root->bout;
      pr[cnod]=curr;
      curr=cnod;
      pfout->write((char*)&(kkn->split),sizeof(long));
      pfout->write((char*)&(kkn->win),sizeof(double));
      pfout->write((char*)&(kkn->wout),sizeof(double));
      
      hflag=0;
   }
       
   while( kkn != root && kkn ){
      flag=1;
      
      while(flag){
         
         while(kkn->bin && !hflag){
            lc[curr]=++cnod;
            kkn=kkn->bin;
            pr[cnod]=curr;
            curr=cnod;
            pfout->write((char*)&(kkn->split),sizeof(long));
            pfout->write((char*)&(kkn->win),sizeof(double));
            pfout->write((char*)&(kkn->wout),sizeof(double));
            
            hflag=0;
         }
         if(kkn->bout) {
            rc[curr]=++cnod;
            kkn=kkn->bout;
            pr[cnod]=curr;
            curr=cnod;
            pfout->write((char*)&(kkn->split),sizeof(long));
            pfout->write((char*)&(kkn->win),sizeof(double));
            pfout->write((char*)&(kkn->wout),sizeof(double));
            
            hflag=0;
         }
         else flag=0;
      }
      
       
      flag=kkn->flio;
      while(flag){
         curr=pr[curr];
         kkn=kkn->up;
         hflag=2;
         flag=kkn->flio; 
      }
      
      if(kkn !=root){
         curr=pr[curr];
         kkn=kkn->up;
         hflag=1;
         if(kkn->bout){
            rc[curr]=++cnod;
            kkn=kkn->bout;
            pr[cnod]=curr;
            curr=cnod;
            pfout->write((char*)&(kkn->split),sizeof(long));
            pfout->write((char*)&(kkn->win),sizeof(double));
            pfout->write((char*)&(kkn->wout),sizeof(double));
            
            hflag=0;
         } 

     }
   }
   for(i=0;i<nnode;i++) { 
      pfout->write((char*)(lc+i),sizeof(long));
      pfout->write((char*)(rc+i),sizeof(long));
      
   }

}


template <class KNodx>
void Dtree<KNodx>::Convert_AllScore_Boost(Isgrid *pIsg, double eps){
   double deps=1.0-eps;
   long flag,i, hflag;
   double score=0.0, xx, px;
   KNodx *kkn, *kko;
   kkn=NULL;
   if(!root) return;
   
   if(root->bin){
      kkn=root->bin;
      score+=root->win;
      hflag=0;
   }else if(root->bout){
      xx=score+root->win;
      px=pIsg->val_1df(xx);
      if(px<eps)px=eps;
      if(px>deps)px=deps;
      root->win=0.5*log(px/(1.0-px));
      kkn=root->bout;
      score+=root->wout;
      hflag=0;
   }else{
      xx=score+root->win;
      px=pIsg->val_1df(xx);
      if(px<eps)px=eps;
      if(px>deps)px=deps;
      root->win=0.5*log(px/(1.0-px));      

      xx=score+root->wout;
      px=pIsg->val_1df(xx);
      if(px<eps)px=eps;
      if(px>deps)px=deps;
      root->wout=0.5*log(px/(1.0-px));
   }
       
   while( kkn != root && kkn ){
      flag=1;
      
      while(flag){
         
         while(kkn->bin && !hflag){
            score+=kkn->win;
            kkn=kkn->bin;
            hflag=0;
         }
         if(!hflag){
            xx=score+kkn->win;
            cout<<xx<<endl;
            px=pIsg->val_1df(xx);
            if(px<eps)px=eps;
            if(px>deps)px=deps;
            kkn->win=0.5*log(px/(1.0-px));
         }
         if(kkn->bout) {
            score+=kkn->wout;
            kkn=kkn->bout;
            hflag=0;
         }
         else {
            xx=score+kkn->wout;
            cout<<xx<<endl;
            px=pIsg->val_1df(xx);
            if(px<eps)px=eps;
            if(px>deps)px=deps;
            kkn->wout=0.5*log(px/(1.0-px));
            flag=0;
         }
      }
      
       
      flag=kkn->flio;
      while(flag){
         kkn=kkn->up;
         score-=kkn->wout;
         hflag=2;
         flag=kkn->flio; 
      }
      
      if(kkn !=root){
         kkn=kkn->up;
         score-=kkn->win;
         hflag=1;
         if(kkn->bout){
            score+=kkn->wout;
            kkn=kkn->bout;
            hflag=0;
         } 
         
      }
   }
}




template <class KNodx>
void Dtree<KNodx>::Convert_LeafScore_Boost(Isgrid *pIsg, double eps){
   double deps=1.0-eps;
   long flag,i, hflag;
   double px;
   KNodx *kkn, *kko;
   kkn=NULL;
   if(!root) return;
   
   if(root->bin){
      kkn=root->bin;
      hflag=0;
   }else if(root->bout){
      px=pIsg->val_1df(root->win);
      if(px<eps)px=eps;
      if(px>deps)px=deps;
      root->win=0.5*log(px/(1.0-px));
      kkn=root->bout;
      hflag=0;
   }else{
      px=pIsg->val_1df(root->win);
      if(px<eps)px=eps;
      if(px>deps)px=deps;
      root->win=0.5*log(px/(1.0-px));      

      px=pIsg->val_1df(root->wout);
      if(px<eps)px=eps;
      if(px>deps)px=deps;
      root->wout=0.5*log(px/(1.0-px));
   }
       
   while( kkn != root && kkn ){
      flag=1;
      
      while(flag){
         
         while(kkn->bin && !hflag){
            kkn=kkn->bin;
            hflag=0;
         }
         if(!hflag){
            px=pIsg->val_1df(kkn->win);
            if(px<eps)px=eps;
            if(px>deps)px=deps;
            kkn->win=0.5*log(px/(1.0-px));
         }
         if(kkn->bout) {
            kkn=kkn->bout;
            hflag=0;
         }
         else {
            px=pIsg->val_1df(kkn->wout);
            if(px<eps)px=eps;
            if(px>deps)px=deps;
            kkn->wout=0.5*log(px/(1.0-px));
            flag=0;
         }
      }
      
       
      flag=kkn->flio;
      while(flag){
         kkn=kkn->up;
         hflag=2;
         flag=kkn->flio; 
      }
      
      if(kkn !=root){
         kkn=kkn->up;
         hflag=1;
         if(kkn->bout){
            kkn=kkn->bout;
            hflag=0;
         } 
         
      }
   }
}




template <class KNodx>
Param<KNodx>::Param(KNodx *pKn){
    n_st=pKn->n_st;
    n_t=pKn->n_t;
    nsx=pKn->nsx;
    nnx=pKn->nnx;
    ncdoc=pKn->snd->ix+pKn->nnd->ix;
}
template <class KNodx>
Param<KNodx>::~Param(){
}

template <class KNodx>
void Param<KNodx>::Set_alpha(void){
      double pt, qt, rt, frc, min, max, diff, eps;
      long flag;
      
      diff=nsx-nnx;
      frc=(nsx)/(nnx);
      eps=nnx/ncdoc;

      if(n_t&&(n_t<nnx)){
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         flag=1;
         if(n_st==min){
            if(n_st-eps>n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+eps<n_t*frc)n_st=max+eps;
            else flag=0;
         }
      }
      if(flag){
         
         pt =n_st/nsx;
         qt =(n_t-n_st)/(nnx-nsx);
         rt =n_t/nnx;
         
         win=log(pt)-log(qt);
         wout=log(1.0-pt)-log(1.0-qt);
         alpha=-n_st*log(rt/pt) - (n_t-n_st)*log(rt/qt);
         alpha-=(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         alpha-=(nnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));
         wstd=win-wout;

      }

}

template <class KNodx>
void Param<KNodx>::Set_delta(void){
      int flag;
      double rt_l, rt_r, rt;
      double diff, frc,max,min, eps;
      diff=nsx-nnx;
      frc=(nsx)/(nnx);
      eps=nnx/ncdoc;
     
      if(n_t&&(n_t<nnx)){
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         flag=1;
         if(n_st==min){
            if(n_st-eps>n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+eps<n_t*frc)n_st=max+eps;
            else flag=0;
         }
      } else flag=0;

      if(flag){
         rt_l=(1.0/ptxx)*(n_st-(n_st*n_st)/n_t);
         rt_r=(1.0/ptxx)*((nsx-n_st) -((nsx-n_st)*(nsx-n_st))/(nnx-n_t));
         rt =(1.0/ptxx)*(nsx -(nsx*nsx)/nnx);
         
         win=n_st/n_t;
         wout=(nsx-n_st)/(nnx-n_t);
         alpha=rt-rt_l-rt_r;
         wstd=rt;
      }
     
}

template <class KNodx>
void Param<KNodx>::Set_gain(double lim){
      long R, flag, nsx_rnd, n_st_rnd,n_t_rnd, nnx_rnd, min_rnd;
      double p, eps=0.01, rt, rt_l, rt_r;
      Binomial b(1.0E-8,1.0E-8);
      double diff, frc, eps1, max, min;
      diff=nsx-nnx;
      frc=(nsx)/(nnx);
      eps1=nnx/ncdoc;
     
      if(n_t&&(n_t<nnx)){
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         flag=1;
         if(n_st==min){
            if(n_st-eps1>n_t*frc)n_st -=eps1;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+eps1<n_t*frc)n_st=max+eps1;
            else flag=0;
         }
      } else flag=0;

      if(flag){
         win=n_st/n_t;
         wout=(nsx-n_st)/(nnx-n_t);
        
         //cout <<nnx<<" "<<ncdoc <<endl;
         nsx_rnd=rnd(((1.0*ncdoc)/nnx)*nsx);
         n_st_rnd=rnd(((1.0*ncdoc)/nnx)*n_st);
         n_t_rnd=rnd(((1.0*ncdoc)/nnx)*n_t);
         nnx_rnd=ncdoc;
         
         
         min_rnd=(nsx_rnd<(nnx_rnd-nsx_rnd))?nsx_rnd:(nnx_rnd-nsx_rnd);
         p=b.upper_limit(nnx_rnd,min_rnd,lim);
         rt=nnx_rnd*p;
         //cout <<"# of errors="<<min_rnd<<"|"<<rt<<endl;

         min_rnd=(n_st_rnd<(n_t_rnd-n_st_rnd))?n_st_rnd:(n_t_rnd - n_st_rnd);
         p=b.upper_limit(n_t_rnd, min_rnd, lim);
         rt_l=n_t_rnd*p;
         //cout <<"# of left child errors="<<min_rnd<<"|"<<rt_l<<endl;

         min_rnd=((nsx_rnd-n_st_rnd)<(nnx_rnd-nsx_rnd-n_t_rnd+n_st_rnd))?(nsx_rnd-n_st_rnd):(nnx_rnd-nsx_rnd-n_t_rnd+n_st_rnd);
         p=b.upper_limit(nnx_rnd-n_t_rnd, min_rnd, lim);
         rt_r=(nnx_rnd-n_t_rnd)*p;
         //cout <<"# of right child errors="<<min_rnd<<"|"<<rt_r<<endl;
         
         alpha=rt-rt_l-rt_r;
         //cout <<"# of error difference="<<alpha<<endl;
         wstd=nnx;

      }
     
}

/*template <class KNodx>
void Param<KNodx>::Set_boost(void){
      double w00,w01,w10,w11;
      double rt_l, rt_r, rt, frc;
      if(n_t&&(n_t<nnx)){
         w00=nnx-nsx-n_t+n_st;
         w01=n_t-n_st;
         w10=nsx-n_st;
         w11=n_st;
         
         win=(1.0/2.0)*log(w11/w01);
         wout=(1.0/2.0)*log(w10/w00);

         rt_l=(1.0/ptxx)*(n_st-(n_st*n_st)/n_t);
         rt_r=(1.0/ptxx)*((nsx-n_st) -((nsx-n_st)*(nsx-n_st))/(nnx-n_t));
         rt =(1.0/ptxx)*(nsx -(nsx*nsx)/nnx);
         
         
         alpha=rt-rt_l-rt_r;
         wstd=rt;
      }



}*/



template <class KNodx>
void Param<KNodx>::Set_boost(void){
      double w00,w01,w10,w11;
      double rt_l, rt_r, rt, frc;

      double pt, qt,max,min, diff, eps;
      long flag;
      
      diff=nsx-nnx;
      frc=(nsx)/(nnx);
      eps=nnx/ncdoc;
      
      if(n_t&&(n_t<nnx)){
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         flag=1;
         if(n_st==min){
            if(n_st-eps>n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+eps<n_t*frc)n_st=max+eps;
            else flag=0;
         }
      }
      else flag=0;

      if(flag){
         
         w00=nnx-nsx-n_t+n_st;
         w01=n_t-n_st;
         w10=nsx-n_st;
         w11=n_st;
         
         win=(1.0/2.0)*log(w11/w01);
         wout=(1.0/2.0)*log(w10/w00);
         
          
         rt_l=(1.0/ptxx)*(n_st-(n_st*n_st)/n_t);
         rt_r=(1.0/ptxx)*((nsx-n_st) -((nsx-n_st)*(nsx-n_st))/(nnx-n_t));
         rt =(1.0/ptxx)*(nsx -(nsx*nsx)/nnx);
         
         
         alpha=rt-rt_l-rt_r;
         wstd=rt;

      }

}



}
