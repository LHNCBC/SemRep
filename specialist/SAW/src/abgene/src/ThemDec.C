#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <runn.h>
#include <Btree.h>
#include <Postg.h>
#include <Vnab.h>
#include "ThemDec.h"

using namespace std;
namespace iret {

ThemDec::ThemDec(Postg<float> *pPg){
   pPsg=pPg;
}

ThemDec::~ThemDec(){
   if(sco!=NULL)delete [] sco;
}

void ThemDec::gopen_map(void){
   long i;
   pPsg->gopen_map();
   nwrd=pPsg->nwrd;
   pPsg->fill_post();
   Pst=pPsg->Pst;
   flc=new float*[nwrd];
   for(i=0;i<nwrd;i++)flc[i]=pPsg->xfil+pPsg->pddr[i];

   cs=(float*)pPsg->get_Mmap("cs");
   num=pPsg->get_Fsiz("cs")/sizeof(float);
   sco=NULL;
}

void ThemDec::gclose_map(void){
   pPsg->gclose_map();
   pPsg->dele_post();
   delete [] flc;
   pPsg->dst_Mmap("cs",(char*)cs);
}

float *ThemDec::ScoreAll(Docum &Doc){
   long i,k,ux;
   int j,pflag=get_qflag(),iz;
   char *spr;

   if(sco==NULL)sco=new float[num];
   for(i=0;i<num;i++)sco[i]=cs[i];

   Doc.reset();
   iz=0;
   while((spr=Doc.show(j))!=NULL){
      if(ux=pPsg->get_idx_map(spr)){
         ux--;
         for(i=0;i<Pst[ux].ix;i++){
            k=Pst[ux].idx[i];
            sco[k]+=flc[ux][i];  
         }
      }
      mark(pflag,iz+1,10,"terms");
   }
   return(sco);
}

Order *ThemDec::TopScore(long n,Docum &Doc){
   long i,k,ux;
   int j,pflag=get_qflag(),iz;
   char *spr;

   if(sco==NULL)sco=new float[num];
   for(i=0;i<num;i++)sco[i]=cs[i];

   Doc.reset();
   iz=0;
   while((spr=Doc.show(j))!=NULL){
      if(ux=pPsg->get_idx_map(spr)){
         ux--;
         for(i=0;i<Pst[ux].ix;i++){
            k=Pst[ux].idx[i];
            sco[k]+=flc[ux][i];
         }
      }
      mark(pflag,iz+1,10,"terms");
   }
   Order *pOrd=new Order(n,num,sco);
   return(pOrd);
}

}
