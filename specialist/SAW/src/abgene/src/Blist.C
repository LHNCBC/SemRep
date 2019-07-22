#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstdlib>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <cmath>
#include <cstring>
#include <cassert>
#include <runn.h>
#include "Blist.h"
using namespace std;
namespace iret {

Blist::Blist(void){
   name=NULL;

   ostate=0;
   nwrd=0;
   ct=0;
   lstr=NULL;
   ign=NULL;
}

Blist::Blist(const char *nam){
   int len=strlen(nam);
   name=new char[len+1];
   strcpy(name,nam);

   ostate=0;
   nwrd=0;
   ct=0;
   lstr=NULL;
   ign=NULL;
}

Blist::Blist(long n,ifstream &fin){
   int i,pflag=get_qflag();
   long j;
   char cnam[max_str],*str;

   ostate=0;
   ct=0;
   ign=NULL;
   nwrd=n;
   lstr=(char **)new long[n];

   for(j=0;j<n;j++){
      fin.getline(cnam,max_str,'\n');
      str=new char[strlen(cnam)+1];
      strcpy(str,cnam);
      *(lstr+j)=str;
      mark(pflag,j+1,10000,"string");
   }
}

Blist::~Blist(){
   long i;
   if(lstr!=NULL){
      for(i=0;i<nwrd;i++)delete [] *(lstr+i);
      delete [] lstr;
   }
   if(ign!=NULL)delete [] ign;
}

void Blist::fill(long n,ifstream &fin){
   int pflag=get_qflag();
   long k;
   char cnam[max_str];
   if(ign!=NULL){
      delete [] ign;
      ign=NULL;
   }
   if(lstr!=NULL){
      for(k=0;k<nwrd;k++)delete [] *(lstr+k);
      delete [] lstr;
   }
   nwrd=n;
   lstr=(char **)new long[n];

   int i;
   char *str;
   for(long j=0;j<n;j++){
      fin.getline(cnam,max_str,'\n');
      str=new char[strlen(cnam)+1];
      strcpy(str,cnam);
      *(lstr+j)=str;
      mark(pflag,j+1,10000,"string");
   }
}

void Blist::size(long n){
   long k;
   if(lstr!=NULL){
      for(k=0;k<nwrd;k++)delete [] *(lstr+k);
      delete [] lstr;
   }
   nwrd=n;
   lstr=(char **)new long[n];
}

int Blist::add_string(long len,const char *str){
   if(ct==nwrd)return(0);
   char *ptr=new char[len+1];
   strcpy(ptr,str);
   *(lstr+ct)=ptr;
   ct++;
   return(1);
}
   
long Blist::find(const char *str){
   int j;
   a=b=0;
   if((j=stc_my(str,*(lstr+0)))<0)return(0);
   else if(j==0)return(1);
  
   if((j=stc_my(str,*(lstr+(nwrd-1))))>0)return(0);
   else if(j==0)return(nwrd);
      
   long i,x=0,y=nwrd-1;
   while(y-x>1){
      i=(y+x)/2;
      if((j=stc_my(str,*(lstr+i)))==0)return(i+1);
      else if(j<0)y=i;
      else x=i;
   }
   return(0);
}

int Blist::stc_my(const char *str,const char *ptr)
   {register int i=(a<b) ? a : b;
   register const char *p1=str+i;
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

void Blist::shrink(){
   long *lf,*rt,bt,sz;
   long i,j,x,y;
   int n,m;
   int pflag=get_qflag();
   char *dp,*pt1,*pt2;

   sz=(long)(log((double)nwrd)/log(2.0))+3;
   lf=new long[sz];
   rt=new long[sz];
   dp=new char[sz];

   if(ign==NULL)ign=new char[nwrd];
   ign[0]=0;
   ign[nwrd-1]=0;
   bt=1;
   *lf=0;
   *rt=nwrd-1;
   *dp=0;
   j=0;
   while(bt){
      j++;
      bt--;   //Read a pair off the stack.
      x=*(lf+bt);
      y=*(rt+bt);
      n=(int)(*(dp+bt)); //Read a pair off the stack.
      while(y-x>1){
         pt1=*(lstr+x)+n;
         pt2=*(lstr+y)+n;
         m=0;
         while(*pt1==*pt2){
            pt1++;
            pt2++;
            m++;
         }
         i=(x+y)/2;
         if(n+m<256)n+=m;
         else n=255;
         *(ign+i)=n;
         if(y-i>1){ //Put upper pair on stack.
            *(lf+bt)=i;
            *(rt+bt)=y;
            *(dp+bt)=n;
            bt++;
         }
         y=i;
      }
      mark(pflag,j,10000,"descents");
   }
  
   delete [] lf;
   delete [] rt;
   delete [] dp;
}

void Blist::write(){
   long i,ap;
   int pflag=get_qflag();
   char *pt1,cnam[max_str],c;

   get_pathw(cnam,"blistset",name,"n");
   ofstream fout(cnam,ios::out);
   fout << nwrd << endl;
   fout.close();

   get_pathw(cnam,"blistset",name,"g");
   ofstream fgout(cnam,ios::out|ios::binary);
   
   get_pathw(cnam,"blistset",name,"t");
   fout.open(cnam,ios::out);
   for(i=0;i<nwrd;i++){
      c=*(ign+i);
      pt1=*(lstr+i)+c;
      fgout.write(&c,sizeof(char));
      fout << pt1 << endl;
      mark(pflag,i+1,10000,"truncated strings");
   }
   fgout.close();
   fout.close();
}

void Blist::write_new(){
   long i,ap;
   int pflag=get_qflag();
   char *pt1,cnam[max_str],c;

   get_pathw(cnam,"blistnew",name,"n");
   ofstream fout(cnam,ios::out);
   fout << nwrd << endl;
   fout.close();

   get_pathw(cnam,"blistnew",name,"g");
   ofstream fgout(cnam,ios::out|ios::binary);
  
   get_pathw(cnam,"blistnew",name,"t");
   fout.open(cnam,ios::out);
   for(i=0;i<nwrd;i++){
      c=*(ign+i);
      pt1=*(lstr+i)+c;
      fgout.write(&c,sizeof(char));
      fout << pt1 << endl;
      mark(pflag,i+1,10000,"truncated strings");
   }
   fgout.close();
   fout.close();
}

void Blist::load(){
   long i,j;
   int pflag=get_qflag();
   char *str,cnam[max_str];

   if(lstr!=NULL){
      for(i=0;i<nwrd;i++){
         delete [] *(lstr+i);
      }
      delete [] lstr;
   }
   if(ign!=NULL)delete [] ign;

   get_pathw(cnam,"blistset",name,"n");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> nwrd;
   fin.close();

   ign=new char[nwrd];
   get_pathw(cnam,"blistset",name,"g");
   fin.open(cnam,ios::in|ios::binary);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin.read((char*)ign,sizeof(char)*nwrd);
   fin.close();
  
   lstr=(char **)new long[nwrd];
   get_pathw(cnam,"blistset",name,"t");
   fin.open(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   for(i=0;i<nwrd;i++){
      j=get_string(cnam,fin,'\n');
      str=new char[j+1];
      strcpy(str,cnam);
      *(lstr+i)=str;
      mark(pflag,i+1,10000,"truncated strings in");
   }
   fin.close();
}

long Blist::sfind(const char *str){
   int j;
   a=b=0;
   if((j=stc_my(str,*(lstr+0)))<0)return(0);
   else if(j==0)return(1);

   if((j=stc_my(str,*(lstr+(nwrd-1))))>0)return(0);
   else if(j==0)return(nwrd);

   long i,x=0,y=nwrd-1;
   while(y-x>1){
      i=(y+x)/2;
      if((j=stc_mys(str,i))==0)return(i+1);
      else if(j<0)y=i;
      else x=i;
   }
   return(0);
}

void Blist::nfind(long n,char *str){
   long x,y,i,j,k;
   char *ptr;

   if(n<0){str=NULL;return;}
   if(n>=nwrd){str=NULL;return;}
   x=0;
   y=nwrd-1;
   j=0;
   while(y-x>1){
      i=(y+x)/2;
      k=ign[x];
      ptr=*(lstr+x);
      while(j<ign[i]){*(str+j)=*(ptr+j-k);j++;}
      if(n<i)y=i;
      else x=i;
   }
   ptr=*(lstr+n);
   k=ign[n];
   i=strlen(ptr)+k;
   while(j<i){*(str+j)=*(ptr+j-k);j++;}
   *(str+j)='\0';
   return;
}

void Blist::create_addr_string(const char *slice_nam){
   char cnam[max_str];
   int pflag=get_qflag();
   long ap,cp=0;

   get_pathw(cnam,"slice",slice_nam,"s");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   get_pathw(cnam,"slice",slice_nam,"as");
   ofstream fout(cnam,ios::out|ios::binary);
   ap=0;
   fout.write((char*)&ap,sizeof(long));
   while(get_string(cnam,fin,'\n')){
      ap=fin.tellg();
      fout.write((char*)&ap,sizeof(long));
      mark(pflag,++cp,10000,"terms");
   }
   fout.close();
   fin.close();
}    

void Blist::gopen_string(const char *slice_nam){
   char cnam[max_str];

 if((ostate&LIST_S)==0){
   get_pathw(cnam,"slice",slice_nam,"n");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> nwrd;
   fin.close();

   if(fadd.is_open())fadd.close();
   get_pathw(cnam,"slice",slice_nam,"as");
   fadd.open(cnam,ios::in|ios::binary);  
   if(!fadd.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   if(fstr.is_open())fstr.close();
   get_pathw(cnam,"slice",slice_nam,"s");
   fstr.open(cnam,ios::in);  
   if(!fstr.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
 }
 ostate=ostate|LIST_S;
}

void Blist::disk_nfind(long n,char *str){
   long ap;
   char cnam[max_str];

   if(n<0){*str='\0';return;}
   if(n>=nwrd){*str='\0';return;}
   fadd.seekg(n*sizeof(long),ios::beg);
   fadd.read((char*)&ap,sizeof(long));
   fstr.seekg(ap,ios::beg);
   fstr.getline(str,max_str,'\n');
   return;
}

long Blist::disk_snfind(const char *str){
   long i,j,k,m;
   char cnam[max_str];

   this->disk_nfind(0,cnam);
   if((i=strcmp(str,cnam))<0)return(0);
   else if(i==0)return(1);
   this->disk_nfind(nwrd-1,cnam);
   if((i=strcmp(str,cnam))>0)return(0);
   else if(i==0)return(nwrd);

   i=0;
   j=nwrd-1;
   while(j-i>1){
      m=(j+i)/2;
      this->disk_nfind(m,cnam);
      if((k=strcmp(str,cnam))>0)i=m;
      else if(k<0)j=m;
      else return(m+1);
   }
   return(0);
}

void Blist::gclose_string(void){
 if(ostate&LIST_S){
   fadd.close();
   fstr.close();
 }
 ostate=ostate&(~LIST_S);
}

int Blist::stc_mys(const char *str,long k)
   {int i=(a<b) ? a : b;
   const char *p1=str+i;
   const char *p2=*(lstr+k)+i-ign[k];
   int j=0;
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

IdTerm::IdTerm(const char *nam) : Blist(nam) {
   idn=NULL;
   inv=NULL;
}

IdTerm::~IdTerm(){
   if(idn!=NULL)delete [] idn;
   if(inv!=NULL)delete [] inv;
}

void IdTerm::create(void){
   long i,j;
   int pflag=get_qflag();
   char *pt1,cnam[max_str];

   get_pathw(cnam,"slice",name,"n");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> nwrd;
   fin.close();

   get_pathw(cnam,"slice",name,"s");
   fin.open(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   this->fill(nwrd,fin);
   fin.close();
 
   this->shrink();
   this->write_new();

   get_pathw(cnam,"blistnew",name,"z");
   ofstream fout(cnam,ios::out);
   fout << nwrd+1 << endl;
   fout.close();

   get_pathw(cnam,"blistnew",name,"i");
   fout.open(cnam,ios::out|ios::binary);
   for(i=1;i<nwrd+1;i++){
      fout.write((char*)&i,sizeof(long));
   }
   fout.close();
}
 
void IdTerm::id_load(void){
   long i,*j;
   int pflag=get_qflag();
   char *pt1,cnam[max_str];

 if((ostate&LIST_I)==0){
   this->load();

   get_pathw(cnam,"blistset",name,"z");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> nid;
   fin.close();

   if(idn!=NULL)delete [] idn;
   idn=new long[nwrd];
   get_pathw(cnam,"blistset",name,"i");
   fin.open(cnam,ios::in|ios::binary);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin.read((char*)idn,sizeof(long)*nwrd);
   fin.close();
 }
 ostate=ostate|LIST_I;
} 

void IdTerm::update1(void){
   long i,j,n;
   int pflag=get_qflag();
   char *pt1,cnam[max_str];

   this->id_load();

   get_pathw(cnam,"slice",name,"n");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> n;
   fin.close();

   get_pathw(cnam,"slice",name,"s");
   fin.open(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   get_pathw(cnam,"blistnew",name,"i");
   ofstream fout(cnam,ios::out|ios::binary);
   for(i=0;i<n;i++){
      get_string(cnam,fin,'\n');
      if((j=this->id_find(cnam))){
          fout.write((char*)(&j),sizeof(long));
      }
      else {
          fout.write((char*)(&nid),sizeof(long));
          nid++;
      }
   }
   fout.close();
   fin.close();

   get_pathw(cnam,"blistnew",name,"z");
   fout.open(cnam,ios::out);
   fout << nid << endl;
   fout.close();
}

void IdTerm::update2(void){
   long n;
   char cnam[max_str];

   get_pathw(cnam,"slice",name,"n");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> n;
   fin.close();

   get_pathw(cnam,"slice",name,"s");
   fin.open(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   this->fill(n,fin);
   fin.close();

   this->shrink();
   this->write_new();
}

long IdTerm::id_find(const char *str){
   long n;
  
   n=sfind(str);
   if(n)return(*(idn+n-1));
   else return(0);
}

void IdTerm::inv_load(void){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

 if((ostate&LIST_V)==0){
   get_pathw(cnam,"blistset",name,"n");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> nwrd;
   fin.close();
   get_pathw(cnam,"blistset",name,"z");
   fin.open(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> nid;
   fin.close();

   if(inv!=NULL)delete [] inv;
   long *ptr;
   inv=new long[nid];
   ptr=inv;
   for(i=0;i<nid;i++)*(ptr++)=0;
   get_pathw(cnam,"blistset",name,"i");
   fin.open(cnam,ios::in|ios::binary);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   if(pflag)cout << "Making conversion array" << endl;
   long *temp=new long[nwrd];
   fin.read((char *)temp,sizeof(long)*nwrd);
   ptr=temp;
   for(i=0;i<nwrd;i++){
      *(inv+*(ptr++))=i+1;
   }
   fin.close();
   delete [] temp;
 }
 ostate=ostate|LIST_V;
}

void IdTerm::create_inv_file(void){
   char cnam[max_str];
   int fld,pflag=get_qflag();
   long i,j;

   get_pathw(cnam,"blistset",name,"n");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> nwrd;
   fin.close();
   get_pathw(cnam,"blistset",name,"z");
   fin.open(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> nid;
   fin.close();

   if(inv!=NULL)delete [] inv;
   inv=new long[nid];
   for(i=0;i<nid;i++)*(inv+i)=0;

   get_pathw(cnam,"blistset",name,"i");
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   idn=(long*)mmap(0,sizeof(long)*nwrd,PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
   if(idn==MAP_FAILED){cout << cnam << " failed to map" << endl;exit(0);}
   ::close(fld);

   if(pflag)cout << "Making conversion array" << endl;
   for(i=0;i<nwrd;i++){
      *(inv+*(idn+i))=i+1;
   }
   munmap((char*)idn,sizeof(long)*nwrd);
   idn=NULL;

   get_pathw(cnam,"blistset",name,"inv");
   ofstream fout(cnam,ios::out);
   for(i=0;i<nid;i++){
      fout.write((char*)(inv+i),sizeof(long));
   }
   fout.close();
}

void IdTerm::inv_map(void){
   char cnam[max_str];
   int fld,pflag=get_qflag();
   long i,j;

 if((ostate&LIST_V)==0){
   get_pathw(cnam,"blistset",name,"z");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin >> nid;
   fin.close();

   get_pathw(cnam,"blistset",name,"inv");
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   inv=(long*)mmap(0,sizeof(long)*nid,PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
   if(inv==MAP_FAILED){cout << cnam << " failed to map" << endl;exit(0);}
   ::close(fld);

 }
 ostate=ostate|LIST_V;
}

void IdTerm::inv_destroy(void){
 if(ostate&LIST_V){
   if(inv!=NULL){
      delete [] inv;
      inv=NULL;
   }
 }
 ostate=ostate&(~LIST_V);
}

void IdTerm::inv_map_destroy(void){
 if(ostate&LIST_V){
   if(inv!=NULL){
      munmap((char*)inv,sizeof(long)*nid);
      inv=NULL;
   }
 }
 ostate=ostate&(~LIST_V);
}

void IdTerm::gopen_idstring(const char *slice_nam){
   char cnam[max_str];

   this->gopen_string(slice_nam);
   if(fkid.is_open())fkid.close();
   get_pathw(cnam,"blistset",name,"i");
   fkid.open(cnam,ios::in|ios::binary);  
   if(!fkid.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
}

void IdTerm::disk_ifind(long id,char *str){
   long n;

   if(id<0){*str='\0';return;}
   if(id>=nid){*str='\0';return;}
   if(n=*(inv+id)){
      this->disk_nfind(n-1,str);
   }
   return;
}

long IdTerm::disk_sifind(const char *str){
   long i,id;

   if(i=this->disk_snfind(str)){
      fkid.seekg((i-1)*sizeof(long),ios::beg);
      fkid.read((char*)&id,sizeof(long));
      return(id);
   }
   else return(0);
}

void IdTerm::gclose_idstring(void){
   this->gclose_string();
   fkid.close();
}

Slice_Access::Slice_Access(const char *nam,const char *slice_nam) : IdTerm(nam){
   int len=strlen(slice_nam);
   slice_name=new char[len+1];
   strcpy(slice_name,slice_nam);

   freq=NULL;
   addr=NULL;
   cfln=0;
   pfln=NULL;
   pBn=NULL;
}

Slice_Access::~Slice_Access(void){
   if (freq!=NULL)delete [] freq;
   if (addr!=NULL)delete [] addr;
   if (pfln!=NULL)delete [] pfln;
   if (pBn!=NULL)delete pBn;
   delete [] slice_name;
}

void Slice_Access::post_access(void){
   int pflag=get_qflag();
   long i,j,ax[2],ix;
   char cnam[256],ctstring[25];
   
 if((ostate&LIST_A)==0){
   freq=new long[nwrd];
   addr=new long[nwrd];

   get_pathw(cnam,"slice",slice_name,"z");
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   while(fin >> ix >> j);
   cfln=ix+1;
   fin.close();
   pBn=new Bnum(cfln+1);
   for(i=0;i<cfln+1;i++)pBn->mm[i]=0;

   get_pathw(cnam,"slice",slice_name,"f");
   fin.open(cnam,ios::in|ios::binary);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   get_pathw(cnam,"slice",slice_name,"a");
   ifstream fia(cnam,ios::in|ios::binary);
   if(!fia.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   for (i=0;i<nwrd;i++){
      fin.read((char*)&ix,sizeof(long));
      fia.read((char*)ax,2*sizeof(long));
      *(freq+i)=ix;
      (pBn->mm[ax[0]+1])++;
      *(addr+i)=ax[1];
      mark(pflag,i+1,100000,"term data");
   }
   fia.close();
   fin.close();
   for(i=1;i<cfln+1;i++)(pBn->mm[i])+=pBn->mm[i-1];

   pfln=new ifstream[cfln];
   for(i=0;i<cfln;i++){
      get_pathw(cnam,"slice",slice_name,"p");
      long_str(ctstring, i);
      strcat(cnam,ctstring);
      pfln[i].open(cnam,ios::in|ios::binary);
      if(!pfln[i].is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   }
 }
 ostate=ostate|LIST_A;
}

void Slice_Access::post_close(void){
 if(ostate&LIST_A){
   if (freq!=NULL)delete [] freq;
   if (addr!=NULL)delete [] addr;
   if (pfln!=NULL)delete [] pfln;
   if (pBn!=NULL)delete pBn;
 }
 ostate=ostate&(~LIST_A);
}   
   
ifstream *Slice_Access::pfile(long &f,const char *str){
   long ix,nc;

   ix=this->sfind(str);
   if(!ix){
      f=0;
      return(NULL);
   }
   else {
      ix--;
      nc=pBn->index(ix);
      f=*(freq+ix);
      pfln[nc].seekg(*(addr+ix), ios::beg);
      return(&pfln[nc]);
   }
}

ifstream *Slice_Access::d_pfile(long &f,const char *str){
   long ix,nc;

   ix=this->disk_snfind(str);
   if(!ix){
      f=0;
      return(NULL);
   }
   else {
      ix--;
      nc=pBn->index(ix);
      f=*(freq+ix);
      pfln[nc].seekg(*(addr+ix), ios::beg);
      return(&pfln[nc]);
   }
}

ifstream *Slice_Access::pfile(long &f,long id){
   long ix,nc;

   ix=this->index(id);
   if(!ix){
      f=0;
      return(NULL);
   }
   else {
      ix--;
      nc=pBn->index(ix);
      f=*(freq+ix);
      pfln[nc].seekg(*(addr+ix), ios::beg);
      return(&pfln[nc]);
   }
}

ifstream *Slice_Access::i_pfile(long &f,long ix){
   long nc;

   nc=pBn->index(ix);
   f=*(freq+ix);
   pfln[nc].seekg(*(addr+ix), ios::beg);
   return(&pfln[nc]);
}

Slice_Accomp::Slice_Accomp(const char *nam,const char *slice_nam) : IdTerm(nam){
   int len=strlen(slice_nam);
   slice_name=new char[len+1];
   strcpy(slice_name,slice_nam);

   freq=NULL;
   addr=NULL;
   cfln=0;
   pfln=NULL;
   pBn=NULL;
   cod=NULL;
}

Slice_Accomp::~Slice_Accomp(void){
   if (freq!=NULL)delete [] freq;
   if (addr!=NULL)delete [] addr;
   if (pfln!=NULL)delete [] pfln;
   if (pBn!=NULL)delete pBn;
   if (cod!=NULL)delete cod;
   delete [] slice_name;
}

void Slice_Accomp::post_access(void){
   int pflag=get_qflag();
   long i,j,maxcnb;
   char cnam[256],ctstring[25];
   ifstream fin;
   long mk;
   
 if((ostate&LIST_A)==0){
   get_pathw(cnam,"slice",slice_name,"n");
   fin.open(cnam,ios::in);
   fin >> nwrd;
   fin.close();
   freq=new long[nwrd];
   addr=new long[nwrd];

   get_pathw(cnam,"slice",slice_name,"d");
   fin.open(cnam,ios::in);
   fin >> ndoc;
   fin.close();
   get_pathw(cnam,"slice",slice_name,"cs");
   fin.open(cnam,ios::in);
   fin >> maxcnb;
   fin.close();
   cod=new unsigned char[maxcnb];

   pw[0]=1;
   for(i=1;i<31;i++)pw[i]=2*pw[i-1];
   for(i=0;i<31;i++)fm[i]=(long)floor((double)ndoc/((double)pw[i]+1.0));
   mk=0;
   while((mk<31)&&(fm[mk]>=1))mk++;
   pfln=new ifstream[mk];
   get_pathw(cnam,"slice",name,"paths");
   fin.open(cnam,ios::in);
   for(i=0;i<mk;i++){
      fin.getline(cnam,max_str);
      pfln[i].open(cnam,ios::in|ios::binary);
      if(!pfln[i].is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   }
   fin.close();
   cfln=mk;

   pBn=new Bnum(cfln+1);
   for(i=0;i<cfln;i++)pBn->mm[i]=fm[mk-i];
   pBn->mm[cfln]=ndoc+1; //This allows for f>ndoc/2;

   get_pathw(cnam,"slice",slice_name,"f");
   fin.open(cnam,ios::in|ios::binary);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin.read((char*)freq,sizeof(long)*nwrd);
   fin.close();
   get_pathw(cnam,"slice",slice_name,"a");
   fin.open(cnam,ios::in|ios::binary);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin.read((char*)addr,sizeof(long)*nwrd);
   fin.close();
 }
 ostate=ostate|LIST_A;
}

void Slice_Accomp::post_access_map(void){
   int fld,pflag=get_qflag();
   long i,j,maxcnb;
   char cnam[256],ctstring[25];
   ifstream fin;
   long mk;
   
 if((ostate&LIST_A)==0){
   get_pathw(cnam,"slice",slice_name,"n");
   fin.open(cnam,ios::in);
   fin >> nwrd;
   fin.close();

   get_pathw(cnam,"slice",slice_name,"d");
   fin.open(cnam,ios::in);
   fin >> ndoc;
   fin.close();
   get_pathw(cnam,"slice",slice_name,"cs");
   fin.open(cnam,ios::in);
   fin >> maxcnb;
   fin.close();
   cod=new unsigned char[maxcnb];

   pw[0]=1;
   for(i=1;i<31;i++)pw[i]=2*pw[i-1];
   for(i=0;i<31;i++)fm[i]=(long)floor((double)ndoc/((double)pw[i]+1.0));
   mk=0;
   while((mk<31)&&(fm[mk]>=1))mk++;
   pfln=new ifstream[mk];
   get_pathw(cnam,"slice",name,"paths");
   fin.open(cnam,ios::in);
   for(i=0;i<mk;i++){
      fin.getline(cnam,max_str);
      pfln[i].open(cnam,ios::in|ios::binary);
      if(!pfln[i].is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   }
   fin.close();
   cfln=mk;

   pBn=new Bnum(cfln+1);
   for(i=0;i<cfln;i++)pBn->mm[i]=fm[mk-i];
   pBn->mm[cfln]=ndoc+1; //This allows for f>ndoc/2;

   get_pathw(cnam,"slice",slice_name,"f");
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   freq=(long*)mmap(0,sizeof(long)*nwrd,PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
   if(freq==MAP_FAILED){cout << cnam << " failed to map" << endl;exit(0);}
   ::close(fld);
   get_pathw(cnam,"slice",slice_name,"a");
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   addr=(long*)mmap(0,sizeof(long)*nwrd,PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
   if(addr==MAP_FAILED){cout << cnam << " failed to map" << endl;exit(0);}
   ::close(fld);
 }
 ostate=ostate|LIST_A;
}

void Slice_Accomp::post_close(void){
 if(ostate&LIST_A){
   if (freq!=NULL)delete [] freq;
   if (addr!=NULL)delete [] addr;
   if (pfln!=NULL)delete [] pfln;
   if (pBn!=NULL)delete pBn;
   if (cod!=NULL)delete [] cod;
 }
 ostate=ostate&(~LIST_A);
}   

void Slice_Accomp::post_close_map(void){
 if(ostate&LIST_A){
   if (freq!=NULL)munmap((char*)freq,sizeof(long)*nwrd);
   if (addr!=NULL)munmap((char*)addr,sizeof(long)*nwrd);
   if (pfln!=NULL)delete [] pfln;
   if (pBn!=NULL)delete pBn;
   if (cod!=NULL)delete [] cod;
 }
 ostate=ostate&(~LIST_A);
}   

long Slice_Accomp::read_comp(long ix){
   long fq,ba,pk;

   fq=*(freq+ix);
   pk=cfln-pBn->index(fq-1)-1; 
   ba=pw[pk];
   pfln[pk].seekg(*(addr+ix),ios::beg);
   pfln[pk].read((char*)&cnb,sizeof(long));
   pfln[pk].read((char*)cod,cnb);
   return(pk);
}
   
long Slice_Accomp::str_post(const char *str,long *idx,unsigned char *lct){
   unsigned char c,*utr,*rtr;
   long nt,yi,od,fq,i,*mtr,ix;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   ix=this->sfind(str);
   if(!ix){
      return(0);
   }
   else {
      ix--;
      pk=this->read_comp(ix);
      fq=*(freq+ix);
      ba=pw[pk];
      utr=cod;
      rtr=lct;
      mtr=idx;
      od=-1;
      yi=0;
      sg=0;
      cu1=0;
      i=0;
      mi=0;
      while(i<fq){
         if(!yi){c=*(utr++);yi=8;}
         switch(sg){
            case 0: while((!sg)&&yi){
                       if(c&128){c=(c << 1);yi--;cu1++;}
                       else {
                          c=(c << 1);
                          yi--;
                          mi+=cu1*ba;
                          sg=1;
                          bc=pk;
                       }
                    }
                    break;
            case 1: if(8<=bc){
                       mi+=c*pw[bc-8];
                       bc-=yi;
                       yi=0;
                    }
                    else {
                       mi+=(c >> (8-bc));
                       if(bc<yi){c=(c << bc);yi-=bc;bc=0;}
                       else {bc-=yi;yi=0;}
                    }
                    if(!bc){
                       sg=2;
                       mj=1;
                    }
                    break;
            case 2: while(sg&&yi){
                       if(c&128){c=(c << 1);yi--;mj++;}
                       else {
                          c=(c << 1);
                          yi--;
                          sg=0;
                          cu1=0;
                          mi+=od;
                          //Process mi and mj here!
                          *(mtr++)=mi;
                          *(rtr++)=mj;
                          i++;
                          od=mi;
                          mi=0;
                       }
                    }
                    break;
          }
      }
      return(fq);
   }
}

long Slice_Accomp::dstr_post(const char *str,long *idx,unsigned char *lct){
   unsigned char c,*utr,*rtr;
   long nt,yi,od,fq,i,*mtr,ix;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   ix=this->disk_snfind(str);
   if(!ix){
      return(0);
   }
   else {
      ix--;
      pk=this->read_comp(ix);
      fq=*(freq+ix);
      ba=pw[pk];
      utr=cod;
      rtr=lct;
      mtr=idx;
      od=-1;
      yi=0;
      sg=0;
      cu1=0;
      i=0;
      mi=0;
      while(i<fq){
         if(!yi){c=*(utr++);yi=8;}
         switch(sg){
            case 0: while((!sg)&&yi){
                       if(c&128){c=(c << 1);yi--;cu1++;}
                       else {
                          c=(c << 1);
                          yi--;
                          mi+=cu1*ba;
                          sg=1;
                          bc=pk;
                       }
                    }
                    break;
            case 1: if(8<=bc){
                       mi+=c*pw[bc-8];
                       bc-=yi;
                       yi=0;
                    }
                    else {
                       mi+=(c >> (8-bc));
                       if(bc<yi){c=(c << bc);yi-=bc;bc=0;}
                       else {bc-=yi;yi=0;}
                    }
                    if(!bc){
                       sg=2;
                       mj=1;
                    }
                    break;
            case 2: while(sg&&yi){
                       if(c&128){c=(c << 1);yi--;mj++;}
                       else {
                          c=(c << 1);
                          yi--;
                          sg=0;
                          cu1=0;
                          mi+=od;
                          //Process mi and mj here!
                          *(mtr++)=mi;
                          *(rtr++)=mj;
                          i++;
                          od=mi;
                          mi=0;
                       }
                    }
                    break;
          }
      }
      return(fq);
   }
}

long Slice_Accomp::id_post(long id,long *idx,unsigned char *lct){
   unsigned char c,*utr,*rtr;
   long nt,yi,od,fq,i,*mtr,ix;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   ix=this->index(id);
   if(!ix){
      return(0);
   }
   else {
      ix--;
      pk=this->read_comp(ix);
      fq=*(freq+ix);
      ba=pw[pk];
      utr=cod;
      rtr=lct;
      mtr=idx;
      od=-1;
      yi=0;
      sg=0;
      cu1=0;
      i=0;
      mi=0;
      while(i<fq){
         if(!yi){c=*(utr++);yi=8;}
         switch(sg){
            case 0: while((!sg)&&yi){
                       if(c&128){c=(c << 1);yi--;cu1++;}
                       else {
                          c=(c << 1);
                          yi--;
                          mi+=cu1*ba;
                          sg=1;
                          bc=pk;
                       }
                    }
                    break;
            case 1: if(8<=bc){
                       mi+=c*pw[bc-8];
                       bc-=yi;
                       yi=0;
                    }
                    else {
                       mi+=(c >> (8-bc));
                       if(bc<yi){c=(c << bc);yi-=bc;bc=0;}
                       else {bc-=yi;yi=0;}
                    }
                    if(!bc){
                       sg=2;
                       mj=1;
                    }
                    break;
            case 2: while(sg&&yi){
                       if(c&128){c=(c << 1);yi--;mj++;}
                       else {
                          c=(c << 1);
                          yi--;
                          sg=0;
                          cu1=0;
                          mi+=od;
                          //Process mi and mj here!
                          *(mtr++)=mi;
                          *(rtr++)=mj;
                          i++;
                          od=mi;
                          mi=0;
                       }
                    }
                    break;
          }
      }
      return(fq);
   }
}

long Slice_Accomp::ix_post(long ix,long *idx,unsigned char *lct){
   unsigned char c,*utr,*rtr;
   long nt,yi,od,fq,i,*mtr;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   pk=this->read_comp(ix);
   fq=*(freq+ix);
   ba=pw[pk];
   utr=cod;
   rtr=lct;
   mtr=idx;
   od=-1;
   yi=0;
   sg=0;
   cu1=0;
   i=0;
   mi=0;
   while(i<fq){
      if(!yi){c=*(utr++);yi=8;}
      switch(sg){
         case 0: while((!sg)&&yi){
                    if(c&128){c=(c << 1);yi--;cu1++;}
                    else {
                       c=(c << 1);
                       yi--;
                       mi+=cu1*ba;
                       sg=1;
                       bc=pk;
                    }
                 }
                 break;
         case 1: if(8<=bc){
                    mi+=c*pw[bc-8];
                    bc-=yi;
                    yi=0;
                 }
                 else {
                    mi+=(c >> (8-bc));
                    if(bc<yi){c=(c << bc);yi-=bc;bc=0;}
                    else {bc-=yi;yi=0;}
                 }
                 if(!bc){
                    sg=2;
                    mj=1;
                 }
                 break;
         case 2: while(sg&&yi){
                    if(c&128){c=(c << 1);yi--;mj++;}
                    else {
                       c=(c << 1);
                       yi--;
                       sg=0;
                       cu1=0;
                       mi+=od;
                       //Process mi and mj here!
                       *(mtr++)=mi;
                       *(rtr++)=mj;
                       i++;
                       od=mi;
                       mi=0;
                    }
                 }
                 break;
      }
   }
   return(fq);
}

//Pthreaded code

//Multithreading entities (global)
   unsigned num_thr; //Number of threads allowed in process.
   pthread_t *tid; //Threads (num_thr);
   Th_data **pTd; //Array of thread arguments (num_thr).
   sem_t smp; //Semaphore.
   int *add; //Array num_thr long for controlling access to Td[].
   int bot; //first available index
   int top; //first empty spot
   pthread_mutex_t lbot; //Lock for bot variable
   pthread_mutex_t ltop; //Lock for top variable

Slice_Accpth::Slice_Accpth(const char *nam,const char *slice_nam) : IdTerm(nam){
   int len=strlen(slice_nam);
   slice_name=new char[len+1];
   strcpy(slice_name,slice_nam);

   freq=NULL;
   addr=NULL;
   cfln=0;
   pfln=NULL;
   pBn=NULL;
   cod=NULL;
}

Slice_Accpth::~Slice_Accpth(void){
   if (freq!=NULL)delete [] freq;
   if (addr!=NULL)delete [] addr;
   if (pfln!=NULL)delete [] pfln;
   if (pBn!=NULL)delete pBn;
   if (cod!=NULL)delete cod;
   delete [] slice_name;
}

void Slice_Accpth::post_access(void){
   int pflag=get_qflag();
   long i,j,maxcnb;
   char cnam[256],ctstring[25];
   ifstream fin;
   long mk;
   
 if((ostate&LIST_A)==0){
   get_pathw(cnam,"slice",slice_name,"n");
   fin.open(cnam,ios::in);
   fin >> nwrd;
   fin.close();
   freq=new long[nwrd];
   addr=new long[nwrd];

   get_pathw(cnam,"slice",slice_name,"d");
   fin.open(cnam,ios::in);
   fin >> ndoc;
   fin.close();
   get_pathw(cnam,"slice",slice_name,"cs");
   fin.open(cnam,ios::in);
   fin >> maxcnb;
   fin.close();
   cod=new unsigned char[maxcnb];

   pw[0]=1;
   for(i=1;i<31;i++)pw[i]=2*pw[i-1];
   for(i=0;i<31;i++)fm[i]=(long)floor((double)ndoc/((double)pw[i]+1.0));
   mk=0;
   while((mk<31)&&(fm[mk]>=1))mk++;
   pfln=new ifstream[mk];
   get_pathw(cnam,"slice",name,"paths");
   fin.open(cnam,ios::in);
   for(i=0;i<mk;i++){
      fin.getline(cnam,max_str);
      pfln[i].open(cnam,ios::in|ios::binary);
      if(!pfln[i].is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   }
   fin.close();
   cfln=mk;

   pBn=new Bnum(cfln+1);
   for(i=0;i<cfln;i++)pBn->mm[i]=fm[mk-i];
   pBn->mm[cfln]=ndoc+1; //This allows for f>ndoc/2;

   get_pathw(cnam,"slice",slice_name,"f");
   fin.open(cnam,ios::in|ios::binary);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin.read((char*)freq,sizeof(long)*nwrd);
   fin.close();
   get_pathw(cnam,"slice",slice_name,"a");
   fin.open(cnam,ios::in|ios::binary);
   if(!fin.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   fin.read((char*)addr,sizeof(long)*nwrd);
   fin.close();

//Multithreading setup
   for(i=0;i<cfln;i++){
      if(pthread_mutex_init(&acc[i],NULL)){
         cout << "Error in mutex initialization!" << endl; 
         exit(0);
      }
   }
   num_thr=10;
   tid=new pthread_t[num_thr];
   pTd=(Th_data **)new long[num_thr];
   for(i=0;i<num_thr;i++){
      pTd[i]=new Th_data(this,i);
   }
   if(sem_init(&smp,0,num_thr)){
      cout << "Semi_init failed!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lbot,NULL)){
      cout << "Error in lbot mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&ltop,NULL)){
      cout << "Error in ltop mutex initialization!" << endl;
      exit(0);
   }
   bot=0;
   top=num_thr;
   add=new int[num_thr];
   for(i=0;i<num_thr;i++)add[i]=i;
 }
 ostate=ostate|LIST_A;
}

void Slice_Accpth::post_access_map(void){
   int fld,pflag=get_qflag();
   long i,j,maxcnb;
   char cnam[256],ctstring[25];
   ifstream fin;
   long mk;
   
 if((ostate&LIST_A)==0){
   get_pathw(cnam,"slice",slice_name,"n");
   fin.open(cnam,ios::in);
   fin >> nwrd;
   fin.close();

   get_pathw(cnam,"slice",slice_name,"d");
   fin.open(cnam,ios::in);
   fin >> ndoc;
   fin.close();
   get_pathw(cnam,"slice",slice_name,"cs");
   fin.open(cnam,ios::in);
   fin >> maxcnb;
   fin.close();
   cod=new unsigned char[maxcnb];

   pw[0]=1;
   for(i=1;i<31;i++)pw[i]=2*pw[i-1];
   for(i=0;i<31;i++)fm[i]=(long)floor((double)ndoc/((double)pw[i]+1.0));
   mk=0;
   while((mk<31)&&(fm[mk]>=1))mk++;
   pfln=new ifstream[mk];
   get_pathw(cnam,"slice",name,"paths");
   fin.open(cnam,ios::in);
   for(i=0;i<mk;i++){
      fin.getline(cnam,max_str);
      pfln[i].open(cnam,ios::in|ios::binary);
      if(!pfln[i].is_open()){cout << cnam << " failed to open" << endl;exit(0);}
   }
   fin.close();
   cfln=mk;

   pBn=new Bnum(cfln+1);
   for(i=0;i<cfln;i++)pBn->mm[i]=fm[mk-i];
   pBn->mm[cfln]=ndoc+1; //This allows for f>ndoc/2;

   get_pathw(cnam,"slice",slice_name,"f");
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   freq=(long*)mmap(0,sizeof(long)*nwrd,PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
   if(freq==MAP_FAILED){cout << cnam << " failed to map" << endl;exit(0);}
   ::close(fld);
   get_pathw(cnam,"slice",slice_name,"a");
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   addr=(long*)mmap(0,sizeof(long)*nwrd,PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
   if(addr==MAP_FAILED){cout << cnam << " failed to map" << endl;exit(0);}
   ::close(fld);

//Multithreading setup
   for(i=0;i<cfln;i++){
      if(pthread_mutex_init(&acc[i],NULL)){
         cout << "Error in mutex initialization!" << endl; 
         exit(0);
      }
   }
   num_thr=10;
   tid=new pthread_t[num_thr];
   pTd=(Th_data **)new long[num_thr];
   for(i=0;i<num_thr;i++){
      pTd[i]=new Th_data(this,i);
   }
   if(sem_init(&smp,0,num_thr)){
      cout << "Semi_init failed!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lbot,NULL)){
      cout << "Error in lbot mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&ltop,NULL)){
      cout << "Error in ltop mutex initialization!" << endl;
      exit(0);
   }
   bot=0;
   top=num_thr;
   add=new int[num_thr];
   for(i=0;i<num_thr;i++)add[i]=i;
 }
 ostate=ostate|LIST_A;
}

void Slice_Accpth::post_close(void){
 long i;
 if(ostate&LIST_A){
   if (freq!=NULL)delete [] freq;
   if (addr!=NULL)delete [] addr;
   if (pfln!=NULL)delete [] pfln;
   if (pBn!=NULL)delete pBn;
   if (cod!=NULL)delete [] cod;
   for(i=0;i<cfln;i++){
      if(pthread_mutex_destroy(&acc[i])){
         cout << "Error in mutex destruction!" << endl; 
         exit(0);
      }
   }
   pthread_mutex_destroy(&lbot);
   pthread_mutex_destroy(&ltop);
   for(i=0;i<num_thr;i++){
      delete pTd[i];
   }
   delete [] pTd;
   delete [] tid;
   delete [] add;
 }
 ostate=ostate&(~LIST_A);
}   

void Slice_Accpth::post_close_map(void){
 long i;
 if(ostate&LIST_A){
   if (freq!=NULL)munmap((char*)freq,sizeof(long)*nwrd);
   if (addr!=NULL)munmap((char*)addr,sizeof(long)*nwrd);
   if (pfln!=NULL)delete [] pfln;
   if (pBn!=NULL)delete pBn;
   if (cod!=NULL)delete [] cod;
   for(i=0;i<cfln;i++){
      if(pthread_mutex_destroy(&acc[i])){
         cout << "Error in mutex destruction!" << endl; 
         exit(0);
      }
   }
   pthread_mutex_destroy(&lbot);
   pthread_mutex_destroy(&ltop);
   for(i=0;i<num_thr;i++){
      delete pTd[i];
   }
   delete [] pTd;
   delete [] tid;
   delete [] add;
 }
 ostate=ostate&(~LIST_A);
}   

long Slice_Accpth::read_pthr(long ix,long *cn,unsigned char *cch){
   long fq,ba,pk;

   fq=*(freq+ix);
   pk=cfln-pBn->index(fq-1)-1;
   ba=pw[pk];
   pthread_mutex_lock(&acc[pk]);
   pfln[pk].seekg(*(addr+ix),ios::beg);
   pfln[pk].read((char*)cn,sizeof(long));
   pfln[pk].read((char*)cch,*cn);
   pthread_mutex_unlock(&acc[pk]);
   return(pk);
}
  
long Slice_Accpth::read_comp(long ix){
   long fq,ba,pk;

   fq=*(freq+ix);
   pk=cfln-pBn->index(fq-1)-1; 
   ba=pw[pk];
   pthread_mutex_lock(&acc[pk]);
   pfln[pk].seekg(*(addr+ix),ios::beg);
   pfln[pk].read((char*)&cnb,sizeof(long));
   pfln[pk].read((char*)cod,cnb);
   pthread_mutex_unlock(&acc[pk]);
   return(pk);
}
   
long Slice_Accpth::str_post(const char *str,long *idx,unsigned char *lct){
   unsigned char c,*utr,*rtr;
   long nt,yi,od,fq,i,*mtr,ix;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   ix=this->sfind(str);
   if(!ix){
      return(0);
   }
   else {
      ix--;
      pk=this->read_comp(ix);
      fq=*(freq+ix);
      ba=pw[pk];
      utr=cod;
      rtr=lct;
      mtr=idx;
      od=-1;
      yi=0;
      sg=0;
      cu1=0;
      i=0;
      mi=0;
      while(i<fq){
         if(!yi){c=*(utr++);yi=8;}
         switch(sg){
            case 0: while((!sg)&&yi){
                       if(c&128){c=(c << 1);yi--;cu1++;}
                       else {
                          c=(c << 1);
                          yi--;
                          mi+=cu1*ba;
                          sg=1;
                          bc=pk;
                       }
                    }
                    break;
            case 1: if(8<=bc){
                       mi+=c*pw[bc-8];
                       bc-=yi;
                       yi=0;
                    }
                    else {
                       mi+=(c >> (8-bc));
                       if(bc<yi){c=(c << bc);yi-=bc;bc=0;}
                       else {bc-=yi;yi=0;}
                    }
                    if(!bc){
                       sg=2;
                       mj=1;
                    }
                    break;
            case 2: while(sg&&yi){
                       if(c&128){c=(c << 1);yi--;mj++;}
                       else {
                          c=(c << 1);
                          yi--;
                          sg=0;
                          cu1=0;
                          mi+=od;
                          //Process mi and mj here!
                          *(mtr++)=mi;
                          *(rtr++)=mj;
                          i++;
                          od=mi;
                          mi=0;
                       }
                    }
                    break;
          }
      }
      return(fq);
   }
}

long Slice_Accpth::dstr_post(const char *str,long *idx,unsigned char *lct){
   unsigned char c,*utr,*rtr;
   long nt,yi,od,fq,i,*mtr,ix;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   ix=this->disk_snfind(str);
   if(!ix){
      return(0);
   }
   else {
      ix--;
      pk=this->read_comp(ix);
      fq=*(freq+ix);
      ba=pw[pk];
      utr=cod;
      rtr=lct;
      mtr=idx;
      od=-1;
      yi=0;
      sg=0;
      cu1=0;
      i=0;
      mi=0;
      while(i<fq){
         if(!yi){c=*(utr++);yi=8;}
         switch(sg){
            case 0: while((!sg)&&yi){
                       if(c&128){c=(c << 1);yi--;cu1++;}
                       else {
                          c=(c << 1);
                          yi--;
                          mi+=cu1*ba;
                          sg=1;
                          bc=pk;
                       }
                    }
                    break;
            case 1: if(8<=bc){
                       mi+=c*pw[bc-8];
                       bc-=yi;
                       yi=0;
                    }
                    else {
                       mi+=(c >> (8-bc));
                       if(bc<yi){c=(c << bc);yi-=bc;bc=0;}
                       else {bc-=yi;yi=0;}
                    }
                    if(!bc){
                       sg=2;
                       mj=1;
                    }
                    break;
            case 2: while(sg&&yi){
                       if(c&128){c=(c << 1);yi--;mj++;}
                       else {
                          c=(c << 1);
                          yi--;
                          sg=0;
                          cu1=0;
                          mi+=od;
                          //Process mi and mj here!
                          *(mtr++)=mi;
                          *(rtr++)=mj;
                          i++;
                          od=mi;
                          mi=0;
                       }
                    }
                    break;
          }
      }
      return(fq);
   }
}

long Slice_Accpth::id_post(long id,long *idx,unsigned char *lct){
   unsigned char c,*utr,*rtr;
   long nt,yi,od,fq,i,*mtr,ix;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   ix=this->index(id);
   if(!ix){
      return(0);
   }
   else {
      ix--;
      pk=this->read_comp(ix);
      fq=*(freq+ix);
      ba=pw[pk];
      utr=cod;
      rtr=lct;
      mtr=idx;
      od=-1;
      yi=0;
      sg=0;
      cu1=0;
      i=0;
      mi=0;
      while(i<fq){
         if(!yi){c=*(utr++);yi=8;}
         switch(sg){
            case 0: while((!sg)&&yi){
                       if(c&128){c=(c << 1);yi--;cu1++;}
                       else {
                          c=(c << 1);
                          yi--;
                          mi+=cu1*ba;
                          sg=1;
                          bc=pk;
                       }
                    }
                    break;
            case 1: if(8<=bc){
                       mi+=c*pw[bc-8];
                       bc-=yi;
                       yi=0;
                    }
                    else {
                       mi+=(c >> (8-bc));
                       if(bc<yi){c=(c << bc);yi-=bc;bc=0;}
                       else {bc-=yi;yi=0;}
                    }
                    if(!bc){
                       sg=2;
                       mj=1;
                    }
                    break;
            case 2: while(sg&&yi){
                       if(c&128){c=(c << 1);yi--;mj++;}
                       else {
                          c=(c << 1);
                          yi--;
                          sg=0;
                          cu1=0;
                          mi+=od;
                          //Process mi and mj here!
                          *(mtr++)=mi;
                          *(rtr++)=mj;
                          i++;
                          od=mi;
                          mi=0;
                       }
                    }
                    break;
          }
      }
      return(fq);
   }
}

long Slice_Accpth::ix_post(long ix,long *idx,unsigned char *lct){
   unsigned char c,*utr,*rtr;
   long nt,yi,od,fq,i,*mtr;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   pk=this->read_comp(ix);
   fq=*(freq+ix);
   ba=pw[pk];
   utr=cod;
   rtr=lct;
   mtr=idx;
   od=-1;
   yi=0;
   sg=0;
   cu1=0;
   i=0;
   mi=0;
   while(i<fq){
      if(!yi){c=*(utr++);yi=8;}
      switch(sg){
         case 0: while((!sg)&&yi){
                    if(c&128){c=(c << 1);yi--;cu1++;}
                    else {
                       c=(c << 1);
                       yi--;
                       mi+=cu1*ba;
                       sg=1;
                       bc=pk;
                    }
                 }
                 break;
         case 1: if(8<=bc){
                    mi+=c*pw[bc-8];
                    bc-=yi;
                    yi=0;
                 }
                 else {
                    mi+=(c >> (8-bc));
                    if(bc<yi){c=(c << bc);yi-=bc;bc=0;}
                    else {bc-=yi;yi=0;}
                 }
                 if(!bc){
                    sg=2;
                    mj=1;
                 }
                 break;
         case 2: while(sg&&yi){
                    if(c&128){c=(c << 1);yi--;mj++;}
                    else {
                       c=(c << 1);
                       yi--;
                       sg=0;
                       cu1=0;
                       mi+=od;
                       //Process mi and mj here!
                       *(mtr++)=mi;
                       *(rtr++)=mj;
                       i++;
                       od=mi;
                       mi=0;
                    }
                 }
                 break;
      }
   }
   return(fq);
}

Th_data::Th_data(Slice_Accpth *pSac,long i){
   char cnam[max_str];
   pSaa=pSac;
   dl=NULL;
   tf=NULL;
   get_pathw(cnam,"slice",pSaa->slice_name,"cs");
   ifstream fin(cnam,ios::in);
   long maxcnb;
   fin >> maxcnb;
   fin.close();
   xch=new unsigned char[maxcnb];
   iz=i;
   ad=(float**)new long[100000];
   wd=(float*)new long[100000];
}

Th_data::~Th_data(){
   delete [] xch;
}

void Th_data::set_nab_data(long *dd,float **tt,float *sxx){
   dl=dd;
   tf=tt;
   sco=sxx;
}

void Th_data::set_bay_data(float *sxx){
   sco=sxx;
}

}
