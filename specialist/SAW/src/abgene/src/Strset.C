#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "Strset.h"
using namespace std;
namespace iret {

Strset::Strset(const char *nam) : FBase("strset",nam){
}

Strset::~Strset(void){
}

void Strset::gopen_write(void){
   num=0;
   pfa=get_Ostr("a");
   pfs=get_Ostr("s");

}

void Strset::add_str(const char *pch){
   long i=pfs->tellp();   
   pfa->write((const char*)&i,sizeof(long));
   *pfs << pch << ends;
   num++;
}

void Strset::gclose_write(void){
   ofstream *pfn=get_Ostr("n");
   *pfn << num << endl;
   dst_Ostr(pfn);
   dst_Ostr(pfa);
   dst_Ostr(pfs);
}

void Strset::gopen_map(void){
   cflag=1;
   if(Gcom(MAP_F)){
      ifstream *pfn=get_Istr("n");
      *pfn >> num;
      dst_Istr(pfn);

      addr=(long*)get_Mmap("a");
      str=get_Mmap("s");
   }
}

char *Strset::show_str(long n){
   if(n<0)return(NULL);
   if(n>=num)return(NULL);
   return(str+addr[n]);
}

void Strset::gclose_map(void){
   cflag=1;
   if(Rcom(MAP_F)){
      dst_Mmap("a",(char*)addr);
      dst_Mmap("s",str);
   }
}

//Lexos is built on Strset

Lexos::Lexos(const char *nam) : Strset(nam){
   this->change_type("lexos");
   tx=new char[max_str];
   ht=new long[max_str];
   sn=new long[max_str];
   sm=new long[max_str];
}

Lexos::~Lexos(void){
   delete [] tx;
   delete [] ht;
   delete [] sn;
   delete [] sm;
}

void Lexos::create_Lexos(List &Ls){
   this->gopen_write();
   Ls.node_first();
   while(Ls.node_next()){
      this->add_str(Ls.show_str());
   }
   this->gclose_write();
}

long Lexos::find(const char *ssr){
   int j;
   a=b=0;
   if((j=stc_my(ssr,str+addr[0]))<0)return(0);
   else if(j==0)return(1);

   if((j=stc_my(ssr,str+addr[num-1]))>0)return(0);
   else if(j==0)return(num);

   long i,x=0,y=num-1;
   while(y-x>1){
      i=(y+x)/2;
      if((j=stc_my(ssr,str+addr[i]))==0)return(i+1);
      else if(j<0)y=i;
      else x=i;
   }
   return(0);
}

int Lexos::stc_my(const char *ssr,const char *ptr)
   {register int i=(a<b) ? a : b;
   register const char *p1=ssr+i;
   register const char *p2=ptr+i;
   register int j=0;
   while((*p1==*p2)&&(*p1!='\0')){
      j++;
      p1++;
      p2++;
   }
   if(*p1==*p2)return(0);
   else if(*p1<*p2){
      b=i+j;
      return(-1);
   }
   else {
      a=i+j;
      return(1);
   }
}

long Lexos::lfind(const char *ssr){
   int i,j,k;
   char *p1;
   const char *p2;

   a=b=0;
   slen=strlen(ssr);
   strcpy(tx,ssr);
   for(i=0;i<slen;i++){
      ht[i]=0;
      sn[i]=0;
      sm[i]=0;
   }

   //Process first string
   p1=tx;
   p2=str+addr[0];
   j=0;
   while((*p1==*p2)&&(*p1!='\0')){
      j++;
      p1++;
      p2++;
   }
   if(*p1==*p2)return(1);
   else if(*p1<*p2)return(0);
   else {
      if(*p2=='\0'){
         ht[j]=1;
      }
      a=j;
   }
   //Process last string
   p1=tx;
   p2=str+addr[num-1];
   j=0;
   while((*p1==*p2)&&(*p1!='\0')){
      j++;
      p1++;
      p2++;
   }
   if(*p1==*p2)return(num);
   else if(*p1<*p2){
      b=j;
   }
   else {
      if(*p2=='\0'){
         return(num);
      }
      *p1='\0';
      b=j;
   }

   if(k=ifind(0,num,tx))return(k);
   i=slen-1;
   while(i>0){
      if(ht[i]>0)return(ht[i]);
      else if(ht[i]<0){
         tx[i]='\0';
         if(k=ifind(sn[i],sm[i],tx))return(k);
      }
      i--;
   }
   return(0);
}

long Lexos::ifind(long n,long m,const char *ssr){
   int j;
   a=b=0;

   long i,x=n,y=m;
   while(y-x>1){
      i=(y+x)/2;
      if((j=stc_ly(ssr,i))==0){
         if(a&&!ht[a]){
            ht[a]=-1;
            sn[a]=x;
            sm[a]=i;
         }
         return(i+1);
      }
      else if(j<0)y=i;
      else {
         if((j>1)&&(!ht[j-1])){
            ht[j-1]=-1;
            sn[j-1]=x;
            sm[j-1]=i;
         }
         x=i;
      }
   }
   return(0);
}

int Lexos::stc_ly(const char *ssr,long m)
   {register int i=(a<b) ? a : b;
   register const char *p1=ssr+i;
   register const char *p2=str+addr[m]+i;
   register int j=0;
   while((*p1==*p2)&&(*p1!='\0')){
      j++;
      p1++;
      p2++;
   }
   if(*p1==*p2){ht[i+j]=m+1;a=i+j-1;return(0);}
   else if(*p1<*p2){
      b=i+j;
      return(-1);
   }
   else {
      a=i+j;
      if(*p2=='\0'){
         ht[i+j]=m+1;
         return(i+j);
      }
      return(i+j+1);
   }
}

long Lexos::tfind(const char *ssr){
   int i,j,k;
   char *p1;
   const char *p2;

   a=b=0;
   slen=strlen(ssr);
   strcpy(tx,ssr);
   for(i=0;i<=slen;i++){
      ht[i]=0;
      sn[i]=0;
      sm[i]=0;
   }

   //Process first string
   p1=tx;
   p2=str+addr[0];
   j=0;
   while((*p1==*p2)&&(*p1!='\0')){
      j++;
      p1++;
      p2++;
   }
   if(*p1==*p2){ht[j]=1;return(1);}
   else if(*p1<*p2)return(0);
   else {
      if(*p2=='\0'){
         ht[j]=1;
      }
   }
   //Process last string
   p1=tx;
   p2=str+addr[num-1];
   j=0;
   while((*p1==*p2)&&(*p1!='\0')){
      j++;
      p1++;
      p2++;
   }
   if(*p1==*p2){
      ht[j]=num;
      if(j>1)*(--p1)='\0';
   }
   else if(*p2<*p1){
      if(*p2=='\0'){
         ht[j]=num;
         if(j>1)*(--p1)='\0';
      }
      else *p1='\0';
   }

   ifind(0,num-1,tx);
   i=slen;
   k=0;
   while(i>0){
      if(ht[i]>0)k++;
      else if(ht[i]<0){
         tx[i]='\0';
         if(ifind(sn[i],sm[i],tx))k++;
      }
      i--;
   }
   return(k);
}

}
