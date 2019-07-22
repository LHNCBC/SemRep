#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <runn.h>
#include <Btree.h>
#include <Postg.h>
#include <Vnab.h>
#include "Mang.h"

using namespace std;
namespace iret {

Mang::Mang(void){}

Mang::Mang(Dbinbase *pDb,Postg<char> *pPg){
   pDnb=pDb;
   pPsg=pPg;
}

Mang::Mang(const char *namdbn,const char *nampsg){
   pPsg=new Postg<char>(namdbn);
   pDnb=new Dbinbase(nampsg);
}

Mang::Mang(Docum &Doc){
   create_Postg(Doc);
   create_Dbinbase(Doc);
   pPsg=new Postg<char>(Doc.name);
   pDnb=new Dbinbase(Doc.name);
}

Mang::Mang(const char *namdoc){
   pPsg=new Postg<char>(namdoc);
   pDnb=new Dbinbase(namdoc);
}

Mang::~Mang(){
   delete pDnb;
   delete pPsg;
}

void Mang::create_Postg(Docum &Doc){
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

void Mang::create_Dbinbase(Docum &Doc){
   Postg<char> Pos(Doc.name);
   Pos.gopen_map();
   Count Ct;
   long i;
   for(i=0;i<Pos.nwrd;i++){
      Ct.add_count2(Pos.term+*(Pos.sddr+i),i+1);
   }
   Dbinbase Db(Doc.name);
   Db.create_files(Doc,Ct,d_lc_func);
}

void Mang::gopen_Dbinbase(void){
   pDnb->gopen_operate(DBIN_A|DBIN_W);
   ndoc=pDnb->ndoc;
   size=pDnb->size;
   dad=pDnb->dad;
   don=pDnb->don;
}

void Mang::read(long n){
   long m;
   nw=*(size+n);
   m=*(dad+n);
   nwd=don+m;
}

void Mang::gclose_Dbinbase(void){
   pDnb->gclose_operate();
}

void Mang::gopen_Postg(void){
   pPsg->gopen_map();
   nwrd=pPsg->nwrd;
   pPsg->fill_post();
   Pst=pPsg->Pst;
}   

void Mang::gclose_Postg(void){
   pPsg->gclose_map();
}

//Manf for local weighted postings
 
Manf::Manf(void){}

Manf::Manf(Dbinbase *pDb,Postg<float> *pPg){
   pDnb=pDb;
   pPsg=pPg;
}

Manf::Manf(const char *namdbn,const char *nampsg){
   pPsg=new Postg<float>(namdbn);
   pDnb=new Dbinbase(nampsg);
}

Manf::Manf(Docum &Doc,float (*d_local)(int,long)){
   create_Postg(Doc,d_local);
   create_Dbinbase(Doc,d_local);
   pPsg=new Postg<float>(Doc.name);
   pDnb=new Dbinbase(Doc.name);
}

Manf::Manf(const char *namdoc){
   pPsg=new Postg<float>(namdoc);
   pDnb=new Dbinbase(namdoc);
}


Manf::~Manf(){
   delete pDnb;
   delete pPsg;
}

void Manf::create_Postg(Docum &Doc,float (*d_local)(int,long)){
   int pflag=get_qflag();
   Postg<float> Pos(Doc.name);

   Doc.gopen_read(READ_W);
   char *spr;
   int j,k;
   float xx;

   for(long i=0;i<Doc.ndoc;i++){
      Doc.clear();
      Doc.read();
      k=Doc.sum_lcnt();
      while((spr=Doc.show(j))!=NULL){
         xx=d_local(j,k);
         Pos.add(spr,i,xx);
      }
      mark(pflag,i+1,100,"documents loaded");
   }
   Doc.gclose_read(READ_W);
   Pos.writep();
}

void Manf::create_Dbinbase(Docum &Doc,float (*d_local)(int,long)){
   Postg<char> Pos(Doc.name);
   Pos.gopen_map();
   Count Ct;
   long i;
   for(i=0;i<Pos.nwrd;i++){
      Ct.add_count2(Pos.term+*(Pos.sddr+i),i+1);
   }
   Dbinbase Db(Doc.name);
   Db.create_files(Doc,Ct,d_local);
}

void Manf::gopen_Dbinbase(void){
   pDnb->gopen_operate(DBIN_A|DBIN_W|DBIN_L);
   ndoc=pDnb->ndoc;
   size=pDnb->size;
   dad=pDnb->dad;
   don=pDnb->don;
   dow=pDnb->dow;
}

void Manf::read(long n){
   long m;
   nw=*(size+n);
   m=*(dad+n);
   nwd=don+m;
   lwt=dow+m;
}

void Manf::gclose_Dbinbase(void){
   pDnb->gclose_operate();
}

void Manf::gopen_Postg(void){
   long i;
   pPsg->gopen_map();
   nwrd=pPsg->nwrd;
   pPsg->fill_post();
   Pst=pPsg->Pst;
   flc=new float*[nwrd];
   for(i=0;i<nwrd;i++)flc[i]=pPsg->xfil+pPsg->pddr[i];
}

void Manf::gclose_Postg(void){
   pPsg->gclose_map();
   pPsg->dele_post();
   delete [] flc;
}

}
