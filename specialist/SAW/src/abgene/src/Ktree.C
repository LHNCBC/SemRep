#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <Dbinbase.h>
#include <FBase.h>
#include "Ktree.h"
using namespace std;
namespace iret {

Ktree::Ktree(void){
   split=NULL;
   win=NULL;
   wout=NULL;
   pr=NULL;
   lc=NULL;
   rc=NULL;
}

Ktree::~Ktree() {
   if(split!=NULL)delete [] split;
   if(win!=NULL)delete [] win;
   if(wout!=NULL)delete [] wout;
   if(pr!=NULL)delete [] pr;
   if(lc!=NULL)delete [] lc;
   if(rc!=NULL)delete [] rc;
}

long Ktree::read_Ktree(ifstream &fin){
   long i,j;
   fin>>nnode;
   if(split!=NULL)delete [] split;
   if(win!=NULL)delete [] win;
   if(wout!=NULL)delete [] wout;
   if(pr!=NULL)delete [] pr;
   if(lc!=NULL)delete [] lc;
   if(rc!=NULL)delete [] rc;
   split=new long[nnode];
   win=new double[nnode];
   wout=new double[nnode];
   pr=new long[nnode];
   lc=new long[nnode];
   rc=new long[nnode];
   
   for(i=0;i<nnode;i++) fin >>j>>split[i]>>win[i]>>wout[i];
   for(i=0;i<nnode;i++) fin>>pr[i]>>lc[i]>>rc[i];
   return nnode;
}

long Ktree::read_Ktree_Binary(ifstream &fin){
   
   long i,j;
   fin.read((char*)&nnode,sizeof(long));
   
   
   if(split!=NULL)delete [] split;
   if(win!=NULL)delete [] win;
   if(wout!=NULL)delete [] wout;
   if(lc!=NULL)delete [] lc;
   if(rc!=NULL)delete [] rc;
   
   split=new long[nnode+1];
   win=new double[nnode+1];
   wout=new double[nnode+1];
   lc=new long[nnode+1];
   rc=new long[nnode+1];
   
   for(i=0;i<nnode;i++) {
      fin.read((char*)(split+i),sizeof(long));
      fin.read((char*)(win+i),sizeof(double));
      fin.read((char*)(wout+i),sizeof(double));
   }
   
   for(i=0;i<nnode;i++) {
      fin.read((char*)(lc+i),sizeof(long));
      fin.read((char*)(rc+i),sizeof(long));
   }
   return nnode;
}


double Ktree::AllScore(Mang &Dbn, long n){
   long i,j,m, flag;
   Dbn.read(n);
   double sum=0.0;
   long pos=0, sp;
   while(pos>=0){
     sp=split[pos];
     i=0;
     j=Dbn.nw-1;
     if(sp<Dbn.nwd[i]) flag=0; 
     else if(sp>=Dbn.nwd[j]){
        if(sp==Dbn.nwd[j]) flag=1;
        else flag=0;
     }else {
        while(j-i>1){
           m=(j+i)/2;
           if(sp<Dbn.nwd[m])j=m;
           else i=m;
        }
        if(sp==Dbn.nwd[i]) flag=1;
        else flag=0;        
     }
     if(flag) {
        sum+=win[pos];
        pos=lc[pos];
     }
     else{
        sum+=wout[pos];
        pos=rc[pos]; 
     }
   }
   return sum;
}

double Ktree::LeafScore(Mang &Dbn, long n){
   long i,j,m, flag;
   Dbn.read(n);
   double sum;
   long pos=0, sp;
   while(pos>=0){
     sp=split[pos];
     i=0;
     j=Dbn.nw-1;
     if(sp<Dbn.nwd[i]) flag=0; 
     else if(sp>=Dbn.nwd[j]){
        if(sp==Dbn.nwd[j]) flag=1;
        else flag=0;
     }else {
        while(j-i>1){
           m=(j+i)/2;
           if(sp<Dbn.nwd[m])j=m;
           else i=m;
        }
        if(sp==Dbn.nwd[i]) flag=1;
        else flag=0;        
     }
     if(flag) {
        sum=win[pos];
        pos=lc[pos];
     }
     else{
        sum=wout[pos];
        pos=rc[pos]; 
     }
   }
   return sum;
}

//Ktree_set

Ktree_set::Ktree_set(const char *nam) : FBase("ktree",nam){
   Kt=NULL;
   ntree=0;
}

Ktree_set::~Ktree_set(void){
   long i;
   if(Kt!=NULL){
      for(i=0;i<ntree;i++)delete &Kt[i];
      delete [] Kt;
   }
}
 
void Ktree_set::Read(void){
   long i;
   ifstream *pfin=get_Istr("n",ios::in);
   *pfin >> ntree;
   dst_Istr(pfin);
   pfin=get_Istr("t",ios::in);
   Kt=new Ktree[ntree];
   for(i=0;i<ntree;i++){
      Kt[i].read_Ktree(*pfin);
   }
   dst_Istr(pfin);
}

void Ktree_set::Read_Binary(void){
   long i;
   ifstream *pfin=get_Istr("n",ios::in);
   *pfin >> ntree;
   dst_Istr(pfin);
   pfin=get_Istr("t",ios::in);
   Kt=new Ktree[ntree];
   for(i=0;i<ntree;i++){
      Kt[i].read_Ktree_Binary(*pfin);
   }
   dst_Istr(pfin);
}

double Ktree_set::AllScore(Mang &Dbn,long n){
   long i;
   double sum=0;
   for(i=0;i<ntree;i++){
      sum+=Kt[i].AllScore(Dbn,n);
   }
   return(sum);
}

double Ktree_set::LeafScore(Mang &Dbn,long n){
   long i;
   double sum=0;
   for(i=0;i<ntree;i++){
      sum+=Kt[i].LeafScore(Dbn,n);
   }
   return(sum);
}

//Stree 

Stree::Stree(){
   schr=NULL;
   spos=NULL;
   wout=NULL;
   win=NULL;
   lc=NULL;
   rc=NULL;
}

void Stree::Convert(Ktree &Kt, Postg<char> *Posg){
   long i,split;
   char *str;

   if(spos!=NULL)delete [] spos;
   if(schr!=NULL)delete [] schr;
   if(win!=NULL)delete [] win;
   if(wout!=NULL)delete [] wout;
   if(lc!=NULL)delete [] lc;
   if(rc!=NULL)delete [] rc;

   nnode=Kt.nnode;
   schr=new char[nnode];
   spos=new long[nnode];
   rc=new long[nnode];
   lc=new long[nnode];
   win=new double[nnode];
   wout=new double[nnode];

   for(i=0;i<nnode;i++){
     rc[i]=Kt.rc[i];
     lc[i]=Kt.lc[i];
     win[i]=Kt.win[i];
     wout[i]=Kt.wout[i];
     split=Kt.split[i];
     str=Posg->term+Posg->sddr[split];
     schr[i]=str[0];
     str=&str[1];
     str_long(str,spos[i]);
   }
}

Stree::~Stree() {
   if(spos!=NULL)delete [] spos;
   if(schr!=NULL)delete [] schr;
   if(win!=NULL)delete [] win;
   if(wout!=NULL)delete [] wout;
   if(lc!=NULL)delete [] lc;
   if(rc!=NULL)delete [] rc;

}

long Stree::write_Stree_Binary(ofstream &fout){
   long i;
   fout.write((char*)&nnode,sizeof(long));
   fout.write(schr,nnode*sizeof(char));
   fout.write((char*)spos,nnode*sizeof(long));
   fout.write((char*)win,nnode*sizeof(double));
   fout.write((char*)wout,nnode*sizeof(double));
   fout.write((char*)lc,nnode*sizeof(long));
   fout.write((char*)rc,nnode*sizeof(long));
   return nnode;

}

long Stree::read_Stree_Binary(ifstream &fin){
   long i;
   fin.read((char*)&nnode,sizeof(long));

   if(schr!=NULL)delete [] schr;
   if(spos!=NULL)delete [] spos;
   if(wout!=NULL)delete [] wout;
   if(win!=NULL)delete [] win;
   if(lc!=NULL)delete [] lc;
   if(rc!=NULL)delete [] rc;

   schr=new char[nnode];
   spos=new long[nnode];
   rc=new long[nnode];
   lc=new long[nnode];
   win=new double[nnode];
   wout=new double[nnode];

   fin.read(schr,nnode*sizeof(char));
   fin.read((char*)spos,nnode*sizeof(long));
   fin.read((char*)win,nnode*sizeof(double));
   fin.read((char*)wout,nnode*sizeof(double));
   fin.read((char*)lc,nnode*sizeof(long));
   fin.read((char*)rc,nnode*sizeof(long));
   return nnode;

}

double Stree::LeafScore(const char *str){
   long pos=0, sp;
   char ch;
   double sum;
   while(pos>=0){
     sp=spos[pos];
     ch=schr[pos];

     if(str[sp]==ch) {
        sum=win[pos];
        pos=lc[pos];
     }
     else{
        sum=wout[pos];
        pos=rc[pos];
     }
   }
   return sum;
}

double Stree::AllScore(const char *str){
   long pos=0, sp;
   char ch;
   double sum=0;
   while(pos>=0){
     sp=spos[pos];
     ch=schr[pos];

     if(str[sp]==ch) {
        sum+=win[pos];
        pos=lc[pos];
     }
     else{
        sum+=wout[pos];
        pos=rc[pos];
     }
   }
   return sum;
}

//Stree_set

Stree_set::Stree_set(const char *nam) : FBase("stree",nam){
   St=NULL;
   ntree=0;
}

Stree_set::Stree_set(const char *nam,Ktree_set *pKst,Postg<char> *pPosg) : FBase("stree",nam){
   ntree=pKst->ntree;
   St=new Stree[ntree];
   long i;
   for(i=0;i<ntree;i++)St[i].Convert(pKst->Kt[i],pPosg);
}

Stree_set::~Stree_set(void){
   long i;
   if(St!=NULL){
      for(i=0;i<ntree;i++)delete &St[i];
      delete [] St;
   }
}

void Stree_set::Read_Binary(void){
   long i;
   ifstream *pfin=get_Istr("n",ios::in);
   *pfin >> ntree;
   dst_Istr(pfin);
   pfin=get_Istr("t",ios::in);
   St=new Stree[ntree];
   for(i=0;i<ntree;i++){
      St[i].read_Stree_Binary(*pfin);
   }
   dst_Istr(pfin);
}

void Stree_set::Write_Binary(void){
   long i;
   ofstream *pfout=get_Ostr("n",ios::out);
   *pfout << ntree << endl;
   dst_Ostr(pfout);
   pfout=get_Ostr("t",ios::out);
   for(i=0;i<ntree;i++){
      St[i].write_Stree_Binary(*pfout);
   }
   dst_Ostr(pfout);
}

double Stree_set::AllScore(char *str){
   long i;
   double sum=0;
   for(i=0;i<ntree;i++){
      sum+=St[i].AllScore(str);
   }
   return(sum);
}

double Stree_set::LeafScore(char *str){
   long i;
   double sum=0;
   for(i=0;i<ntree;i++){
      sum+=St[i].LeafScore(str);
   }
   return(sum);
}

}
