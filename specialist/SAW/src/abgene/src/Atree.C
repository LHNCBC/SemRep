#include <iostream>
#include <fstream>
#include <cstdlib>
#include <fcntl.h>
#include <sys/mman.h>
#include <cmath>
#include <cstring>
#include <cassert>
#include "Atree.h"
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

ATreeBase::ATreeBase(Dbinbase *pDb, Postg<char> *pPg) : Mang(pDb,pPg)  {
  tx=NULL;
  sx=NULL;
  dt=NULL;
}

ATreeBase::ATreeBase(const char *namdbn,const char *nampsg) : Mang(namdbn,nampsg){
   tx=NULL;
   sx=NULL;
   dt=NULL;
}

ATreeBase::ATreeBase(Docum &Doc) : Mang(Doc)  {
  tx=NULL;
  sx=NULL;
  dt=NULL;
}

ATreeBase::ATreeBase(const char *namdoc) : Mang(namdoc){
   tx=NULL;
   sx=NULL;
   dt=NULL;
}

ATreeBase::~ATreeBase(){
   if(tx) delete [] tx;
   if(sx) delete [] sx;
   if(dt) delete [] dt;
}

void ATreeBase::init(void){
   
   dt=new double[ndoc];
   for(long i=0;i<ndoc;i++) dt[i]=0.0;
   tx=new double[nwrd];
   sx=new double[nwrd];
}

long ATreeBase::get_min_partition(double rem){
   double rt_l, rt_r, rt, frc;

   double max,min, diff, eps, tmin=1000000;
   long i,flag;
      
   diff=nsx-nnx;
   frc=(nsx)/(nnx);
   eps=nnx/ncdoc;
   double n_t,n_st;
   double w00,w01,w10,w11,w0,w1,w;
   
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
         w=w1+rem;
         if(tmin>w) {
            tmin=w;
            split=i;
            win=(1.0/2.0)*log((w11+eps)/(w01+eps));
            wout=(1.0/2.0)*log((w10+eps)/(w00+eps));

         }  
      }
      tx[i]=sx[i]=0.0;

   }
   err=tmin;
   
   return(split);


}

void ATreeBase::countST(Index *ind1,Index *ind2){
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

void ATreeBase::countDoc(long i){
   long j;
   double pt;
   pt=dt[i];
   this->read(i);
   for(i=0;i<nw;i++){
      j=*(nwd+i);
      (*(tx+j))+=pt;
   }
}


void ATreeBase::counbDoc(long i){
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

void ATreeBase::zerot(void){
   for(long i=0;i<nwrd;i++)*(tx+i)=0.0;
}

void ATreeBase::update_dt(Index *ind, double a){
   long i;
   for(i=0;i<ind->ix;i++) dt[ind->idx[i]]=dt[ind->idx[i]]*exp(-a);
   
}

void ATreeBase::norm(void){
   double Z=0.0;
   for(long i=0;i<ndoc;i++) Z+=dt[i];
   for(long i=0;i<ndoc;i++) dt[i]=dt[i]/Z;
}

void ATreeBase::zeros(void){
   for(long i=0;i<nwrd;i++)*(sx+i)=0.0;
}



//Aset

Aset::Aset(void){

}

Aset::~Aset(){
   if(snd != NULL) delete snd;
   if(nnd != NULL) delete nnd;
}

double Aset::erf(ATreeBase *pBb){
   long i;
   double rem,sum;
   pBb->countST(snd,nnd);
   sum=0.0;
   rem=0.0;
   for(i=0;i<pBb->ndoc;i++)sum+=pBb->dt[i];
   if(snd) for(i=0;i<snd->ix;i++) rem+=pBb->dt[snd->idx[i]];
   if(nnd) for(i=0;i<nnd->ix;i++) rem+=pBb->dt[nnd->idx[i]];
   rem=sum-rem;
   csplit=pBb->get_min_partition(rem);
   win=pBb->win;
   wout=pBb->wout;
   
   return(pBb->err);
}



Atree::Atree(long nd,ATreeBase *pBx){
   ndoc=nd;
   pBb=pBx;
   sco=NULL;
   pflag=1;
   zprod=1.0;
}

 
Atree::Atree(long nd,ATreeBase *pBx,int flag){
   ndoc=nd;
   pBb=pBx;
   sco=NULL;
   pflag=flag;
   zprod=1.0;
}

Atree::~Atree(){
    delete [] aset;
 
}


void Atree::Build_ATree(Index *subb, Index *nubb, long lim){
  
   long iteration, target_split, target_node,i;
   double a,b,Z;
   Index *snd,*nnd;
   snd=new Index(subb);
   nnd=new Index(nubb);
   aset=new Aset[2*lim+10];
   
   
   for(i=0;i<snd->ix;i++) pBb->dt[snd->idx[i]]=1.0/(1.0*snd->ix+1.0*nnd->ix);
   for(i=0;i<nnd->ix;i++) pBb->dt[nnd->idx[i]]=1.0/(1.0*snd->ix+1.0*nnd->ix);
   
   
   
   
   node=0;
   aset[node].node=node;
   aset[node].snd=snd;
   aset[node].nnd=nnd;
   
   aset[node].flio=0;
   aset[node].split=-1;
   aset[node].up=&aset[node];
   aset[node].depth=0;
   aset[node].weg=(1.0/2.0)*log((1.0*snd->ix)/(1.0*nnd->ix));
   node++;

   iteration=0;
   while(iteration<lim){
      i=0;
      min_err=-1.0*LNEG;
      while(i<node){
         
         Z=aset[i].erf(pBb);
        
         if(min_err>Z){
            min_err=Z;
            target_node=i;
            target_split=aset[i].csplit;
            a=aset[i].win;
            b=aset[i].wout;
            
         }
         i++;
          
      }
      
      
      snd=aset[target_node].snd->cbool_And(pBb->Pst+target_split);
      nnd=aset[target_node].nnd->cbool_And(pBb->Pst+target_split);
      
      if(snd || nnd ){
         aset[node].node=node;
         aset[node].snd=snd;
         aset[node].nnd=nnd;
         aset[node].split=target_split;
         
         aset[node].flio=+1;
         aset[node].up=&aset[target_node];
         aset[node].depth=aset[target_node].depth+1;
         aset[node].weg=a;
         if(snd) pBb->update_dt(snd,a);
         if(nnd) pBb->update_dt(nnd,-a);
         
         node++;
      }
      
      snd=aset[target_node].snd->cbool_Butnot(pBb->Pst+target_split);
      nnd=aset[target_node].nnd->cbool_Butnot(pBb->Pst+target_split);
      if(snd || nnd ){
         aset[node].node=node;
         aset[node].snd=snd;
         aset[node].nnd=nnd;
         aset[node].split=target_split;
         
         aset[node].flio=-1;
         aset[node].up=&aset[target_node];
         aset[node].depth=aset[target_node].depth+1;
         aset[node].weg=b;
         if(snd) pBb->update_dt(snd,b);
         if(nnd) pBb->update_dt(nnd,-b);
            
         node++;
      }
      
      pBb->norm();
      
      iteration++;
      mark(pflag,node,1,"Asets are constructed");
   }
   
}


void Atree::Score_ATree(Index *temp){
      long i,j;
      Index *test, *in,*out, *up;
      test=new Index(temp);
      if(sco!=NULL){
         delete [] sco;
         sco=NULL;
      }
      sco=new double[ndoc];
      for(i=0;i<test->ix;i++) sco[test->idx[i]]=aset[0].weg;
      aset[0].ind=test;
     
      i=1;
      while(i<node){
         if(aset[i].flio==1){
            up=aset[i].up->ind;
            if(up){
               in=up->cbool_And(pBb->Pst+aset[i].split);
               if(in){
                  aset[i].ind=in;
                  for(j=0;j<in->ix;j++) sco[in->idx[j]]+=aset[i].weg;
               }
            }
         }
         
         if(aset[i].flio==-1){
            up=aset[i].up->ind;
            if(up){
               out=up->cbool_Butnot(pBb->Pst+aset[i].split);
               if(out){
                  aset[i].ind=out;
                  for(j=0;j<out->ix;j++) sco[out->idx[j]]+=aset[i].weg;
               }
            }
         }
         i++;
         
                  
          
      }
      
}



void Atree::BuildInit(Index *subb, Index *nubb){
   Index *snd,*nnd;
   snd=new Index(subb);
   nnd=new Index(nubb);
   long i;
   for(i=0;i<snd->ix;i++) pBb->dt[snd->idx[i]]=1.0/(1.0*snd->ix+1.0*nnd->ix);
   for(i=0;i<nnd->ix;i++) pBb->dt[nnd->idx[i]]=1.0/(1.0*snd->ix+1.0*nnd->ix);
   node=0;
   aset[node].node=node;
   aset[node].snd=snd;
   aset[node].nnd=nnd;
   
   aset[node].flio=0;
   aset[node].split=-1;
   aset[node].up=&aset[node];
   aset[node].depth=0;
   aset[node].weg=(1.0/2.0)*log((1.0*snd->ix)/(1.0*nnd->ix));
   node++;
   


}


long Atree::Build_ATree(void){
   long target_split, target_node,i;
   double a,b,Z;
   Index *snd,*nnd;

   i=0;
   min_err=-1.0*LNEG;
   while(i<node){
         
      Z=aset[i].erf(pBb);
      //cout<<Z<<endl;  
      if(min_err>Z){
         min_err=Z;
         target_node=i;
         target_split=aset[i].csplit;
         a=aset[i].win;
         b=aset[i].wout;
            
      }
      i++;
          
   }
   zprod=zprod*min_err;   
      
   snd=aset[target_node].snd->cbool_And(pBb->Pst+target_split);
   nnd=aset[target_node].nnd->cbool_And(pBb->Pst+target_split);
      
   if(snd || nnd ){
      aset[node].node=node;
      aset[node].snd=snd;
      aset[node].nnd=nnd;
      aset[node].split=target_split;
         
      aset[node].flio=+1;
      aset[node].up=&aset[target_node];
      aset[node].depth=aset[target_node].depth+1;
      aset[node].weg=a;
      if(snd) pBb->update_dt(snd,a);
      if(nnd) pBb->update_dt(nnd,-a);
         
      node++;
   }
      
   snd=aset[target_node].snd->cbool_Butnot(pBb->Pst+target_split);
   nnd=aset[target_node].nnd->cbool_Butnot(pBb->Pst+target_split);
   if(snd || nnd ){
      aset[node].node=node;
      aset[node].snd=snd;
      aset[node].nnd=nnd;
      aset[node].split=target_split;
         
      aset[node].flio=-1;
      aset[node].up=&aset[target_node];
      aset[node].depth=aset[target_node].depth+1;
      aset[node].weg=b;
      if(snd) pBb->update_dt(snd,b);
      if(nnd) pBb->update_dt(nnd,-b);
            
      node++;
   }
   pBb->norm();
   return(node);
}

void Atree::Dest_ATree(void){
   delete [] aset;
}



}
