#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include "Btree.h"
#include "runn.h"
#include "Docum.h"
#include "Postg.h"
using namespace std;
namespace iret {

template<class X>
Zdat<X>::Zdat(void){}

template<class X>
Zdat<X>::Zdat(long n, const X &dtx) : num(n), datx(dtx) {
}

template<class X>
Zdat<X>::~Zdat(void){
}

template<class X>
Postg<X>::Postg(void) : FBase("postsetg","null"){
   btr=new Btree;
   nwrd =0;    //initialized here
}

template<class X>
Postg<X>::Postg(const char *nam) : FBase("postsetg",nam){
   btr=new Btree;
   nwrd =0;    //initialized here
}

template<class X>
Postg<X>::~Postg(){
   Zdat<X> *ppp,*qqq,*zzz;

   if(btr!=NULL){
      btr->node_first();
      while(btr->node_next()){
         qqq=ppp=(Zdat<X> *)(btr->give_ptr());
	 if(qqq !=NULL)  //will have trouble when Postg has skeleton Btree.
	   {
	     do {
	       zzz=qqq->pdt;
	       delete qqq;
	       qqq=zzz;
	     }while(qqq!=ppp);
	   }
      }
      delete btr;
   }
}

template<class X>
Zdat<X> *Postg<X>::tread(long m,long n){
   Zdat<X> *ppp,*qqq;

   ppp=new Zdat<X>;
   ppp->num=pfil[m];
   ppp->datx=xfil[m];
   ppp->pdt=ppp;
   qqq=ppp;
   for(long i=1;i<n;i++){
      ppp=new Zdat<X>;
      ppp->num=pfil[m+i];
      ppp->datx=xfil[m+i];
      ppp->pdt=qqq->pdt;
      qqq->pdt=ppp;
      qqq=ppp;
   }
   return(qqq);
}  

template<class X>
long Postg<X>::twrite(ofstream *pfout,ofstream *xfout,Zdat<X> *dat){
   Zdat<X> *ppp,*qqq;
   long pt=0;
   
   ppp=dat;
   qqq=ppp;
   do {
      qqq=qqq->pdt;
      pfout->write((char *)(&qqq->num),sizeof(long));
      xfout->write((char *)(&qqq->datx),sizeof(X));
      pt++;
   } while(qqq!=ppp);
   return(pt);
}

template<class X>
void Postg<X>::add(const char *str,long n,const X &dtx){
   Zdat<X> *ppp,*qqq;
   Node *npt;
   ppp=new Zdat<X>(n,dtx);

   if(btr->search(str)){
      qqq=(Zdat<X> *)btr->give_ptr();
      ppp->pdt=qqq->pdt;
      qqq->pdt=ppp;
      btr->set_ptr(ppp);
   }
   else {
      ppp->pdt=ppp;
      npt=new Node(str,ppp);
      btr->insert(npt);
      nwrd++;
   }
}

template<class X>
Zdat<X> *Postg<X>::data(const char *str){
   if(btr->search(str)){
      return((Zdat<X> *)btr->give_ptr());
   } 
   else return(NULL);
}

template<class X>
Zdat<X> *Postg<X>::data(void){
   return((Zdat<X> *)btr->give_ptr());
}

template<class X>
long Postg<X>::set_ptr(const char *str){
   if(pQdat=this->data(str)){
     pPdat=&Udat;
     Udat.pdt=pQdat->pdt;
     return(this->count(pQdat));
   }
   else return(0);
}

template<class X>
long Postg<X>::set_ptr(void){
   pQdat=(Zdat<X> *)btr->give_ptr();
   pPdat=&Udat;
   Udat.pdt=pQdat->pdt;
   return(this->count(pQdat));
}

template<class X>
Zdat<X> *Postg<X>::next_ptr(void){
   if(pPdat!=pQdat){
      pPdat=pPdat->pdt;
      return(pPdat);
   }
   else return(NULL);
}

template<class X>
long Postg<X>::count(Zdat<X> *dat){
   Zdat<X> *ppp,*qqq;
   long pt=1;
   
   ppp=dat;
   qqq=ppp->pdt;
   while(qqq!=ppp){
      qqq=qqq->pdt;
      pt++;
   }
   return(pt);
}

template<class X>
void Postg<X>::readp(void){
   int pflag=get_qflag();
   long frq,i;
   char *str;
   Node *npt;
   Zdat<X> *dat;

   ifstream *pfin=get_Istr("n",ios::in);
   *pfin >> nwrd;
   dst_Istr(pfin);

   freq=(long*)get_Mmap("f");
   term=get_Mmap("s");
   sddr=(long*)get_Mmap("sa");
   pddr=(long*)get_Mmap("pa");
   pfil=(long*)get_Mmap("p");
   xfil=(X *)get_Mmap("x");

   for(i=0;i<nwrd;i++){
      str=term+sddr[i];
      dat=tread(pddr[i],freq[i]);
      if(btr->search(str)){
         cout << "Repeat in term list at " << i << endl;
         exit(0);
      }
      else {
         npt=new Node(str,dat);
         btr->insert(npt);
      }
      mark(pflag,i+1,1000,"postings in");
   }
   dst_Mmap("f",(char*)freq);
   dst_Mmap("s",term);
   dst_Mmap("sa",(char*)sddr);
   dst_Mmap("pa",(char*)pddr);
   dst_Mmap("p",(char*)pfil);
   dst_Mmap("x",(char*)xfil);
}

template<class X>
void Postg<X>::readp(long sn){
   int pflag=get_qflag();
   long frq,i;
   char *str;
   Node *npt;
   Zdat<X> *dat;

   ifstream *pfin=get_Istr(sn,"n",ios::in);
   *pfin >> nwrd;
   dst_Istr(pfin);

   freq=(long*)get_Mmap(sn,"f");
   term=get_Mmap(sn,"s");
   sddr=(long*)get_Mmap(sn,"sa");
   pddr=(long*)get_Mmap(sn,"pa");
   pfil=(long*)get_Mmap(sn,"p");
   xfil=(X *)get_Mmap(sn,"x");

   for(i=0;i<nwrd;i++){
      str=term+sddr[i];
      dat=tread(pddr[i],freq[i]);
      if(btr->search(str)){
         cout << "Repeat in term list at " << i << endl;
         exit(0);
      }
      else {
         npt=new Node(str,dat);
         btr->insert(npt);
      }
      mark(pflag,i+1,1000,"postings in");
   }
   dst_Mmap(sn,"f",(char*)freq);
   dst_Mmap(sn,"s",term);
   dst_Mmap(sn,"sa",(char*)sddr);
   dst_Mmap(sn,"pa",(char*)pddr);
   dst_Mmap(sn,"p",(char*)pfil);
   dst_Mmap(sn,"x",(char*)xfil);
}

template<class X>
void  Postg<X>::writep(){
   int pflag=get_qflag();
   long frq,ct,addr_off,pos;
   char cnam[max_str];
  
   ofstream *pfout=get_Ostr("n",ios::out);
   *pfout << nwrd << endl;
   dst_Ostr(pfout);

   ofstream *pfis=get_Ostr("s",ios::out);
   ofstream *psaddr=get_Ostr("sa",ios::out);
   pfout=get_Ostr("p",ios::out);
   ofstream *xfout=get_Ostr("x",ios::out);
   ofstream *pfaddr=get_Ostr("pa",ios::out);
   ofstream *pfqout=get_Ostr("f",ios::out);

   ct=0;
   pos=0;
   btr->node_first();
   while(btr->node_next()){
      addr_off = pfis->tellp();
      psaddr->write((char*)&addr_off, sizeof(long));
      *pfis << btr->show_str() << ends;
      pfaddr->write((char*)&pos, sizeof(long));
      frq=twrite(pfout,xfout,(Zdat<X> *)(btr->give_ptr()));
      pos+=frq;
      pfqout->write((char*)&frq, sizeof(long));
      mark(pflag,++ct,100,"postings out");
   }
   dst_Ostr(pfis);
   dst_Ostr(pfout);
   dst_Ostr(xfout);
   dst_Ostr(pfaddr);
   dst_Ostr(psaddr);
   dst_Ostr(pfqout);
}

template<class X>
void  Postg<X>::writep(long sn){
   int pflag=get_qflag();
   long frq,ct,addr_off,pos;
   char cnam[max_str];
 
   ofstream *pfout=get_Ostr(sn,"n",ios::out);
   *pfout << nwrd << endl;
   dst_Ostr(pfout);

   ofstream *pfis=get_Ostr(sn,"s",ios::out);
   ofstream *psaddr=get_Ostr(sn,"sa",ios::out);
   pfout=get_Ostr(sn,"p",ios::out);
   ofstream *xfout=get_Ostr(sn,"x",ios::out);
   ofstream *pfaddr=get_Ostr(sn,"pa",ios::out);
   ofstream *pfqout=get_Ostr(sn,"f",ios::out);

   ct=0;
   pos=0;
   btr->node_first();
   while(btr->node_next()){
      addr_off = pfis->tellp();
      psaddr->write((char*)&addr_off, sizeof(long));
      *pfis << btr->show_str() << ends;
      pfaddr->write((char*)&pos, sizeof(long));
      frq=twrite(pfout,xfout,(Zdat<X> *)(btr->give_ptr()));
      pos+=frq;
      pfqout->write((char*)&frq, sizeof(long));
      mark(pflag,++ct,100,"postings out");
   }
   dst_Ostr(pfis);
   dst_Ostr(pfout);
   dst_Ostr(xfout);
   dst_Ostr(pfaddr);
   dst_Ostr(psaddr);
   dst_Ostr(pfqout);
}

template<class X>
void Postg<X>::gopen_map(void){
   ifstream *pfin=get_Istr("n",ios::in);
   *pfin >> nwrd;
   dst_Istr(pfin);

   freq=(long*)get_Mmap("f");
   pddr=(long*)get_Mmap("pa");
   sddr=(long*)get_Mmap("sa");
   term=get_Mmap("s");

   pfil=(long*)get_Mmap("p");
   xfil=(X *)get_Mmap("x");
}

template<class X>
void Postg<X>::gopen_map(long sn){
   ifstream *pfin=get_Istr(sn,"n",ios::in);
   *pfin >> nwrd;
   dst_Istr(pfin);

   freq=(long*)get_Mmap(sn,"f");
   pddr=(long*)get_Mmap(sn,"pa");
   sddr=(long*)get_Mmap(sn,"sa");
   term=get_Mmap(sn,"s");

   pfil=(long*)get_Mmap(sn,"p");
   xfil=(X *)get_Mmap(sn,"x");
}

template<class X>
long Postg<X>::set_ptr_map(const char *str){
   long i,j,k,m;

   if((i=strcmp(str,term))<0)return(0);
   else if(i==0){
      pQdat=this->tread(pddr[0],freq[0]);
      pPdat=&Udat;
      Udat.pdt=pQdat->pdt;
      return(freq[0]);
   }
   if((i=strcmp(str,term+sddr[nwrd-1]))>0)return(0);
   else if(i==0){
      pQdat=this->tread(pddr[nwrd-1],freq[nwrd-1]);
      pPdat=&Udat;
      Udat.pdt=pQdat->pdt;
      return(freq[nwrd-1]);
   }

   i=0;
   j=nwrd-1;
   while(j-i>1){
      m=(j+i)/2;
      if((k=strcmp(str,term+sddr[m]))>0)i=m;
      else if(k<0)j=m;
      else {
         pQdat=this->tread(pddr[m],freq[m]);
         pPdat=&Udat;
         Udat.pdt=pQdat->pdt;
         return(freq[m]);
      }
   }
   return(0);
}

template<class X>
long Postg<X>::get_idx_map(const char *str){
   long i,j,k,m;

   if((i=strcmp(str,term))<0)return(0);
   else if(i==0){
      return(1);
   }
   if((i=strcmp(str,term+sddr[nwrd-1]))>0)return(0);
   else if(i==0){
      return(nwrd);
   }

   i=0;
   j=nwrd-1;
   while(j-i>1){
      m=(j+i)/2;
      if((k=strcmp(str,term+sddr[m]))>0)i=m;
      else if(k<0)j=m;
      else {
         return(m+1);
      }
   }
   return(0);
}

template<class X>
void Postg<X>::clear_data(void){
   Zdat<X> *ppp,*qqq,*zzz;
   qqq=ppp=pQdat;
   if(qqq !=NULL){
      do{
         zzz=qqq->pdt;
         delete qqq;
         qqq=zzz;
      }while(qqq!=ppp);
   }
}   

template<class X>
void Postg<X>::fill_post(void){
   long i;
   Pst=new Index[nwrd];

   for(i=0;i<nwrd;i++){
      Pst[i].ix=freq[i];
      Pst[i].idx=pfil+pddr[i];
   }
}

template<class X>
void Postg<X>::dele_post(void){
   long *ptr=(long*)Pst;
   delete [] ptr;
}

template<class X>
void Postg<X>::gclose_map(void){
   dst_Mmap("f",(char*)freq);
   dst_Mmap("s",term);
   dst_Mmap("sa",(char*)sddr);
   dst_Mmap("pa",(char*)pddr);
   dst_Mmap("p",(char*)pfil);
   dst_Mmap("x",(char*)xfil);
}

template<class X>
void Postg<X>::gclose_map(long sn){
   dst_Mmap(sn,"f",(char*)freq);
   dst_Mmap(sn,"s",term);
   dst_Mmap(sn,"sa",(char*)sddr);
   dst_Mmap(sn,"pa",(char*)pddr);
   dst_Mmap(sn,"p",(char*)pfil);
   dst_Mmap(sn,"x",(char*)xfil);
}
template class Postg<char>;
template class Postg<float>;

}
