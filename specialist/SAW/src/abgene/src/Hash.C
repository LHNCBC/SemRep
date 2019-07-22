#include <iostream> 
#include <fstream> 
#include <cstdlib>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <cmath>
#include <cstring>
#include <cassert>
#include "runn.h"
#include "Btree.h"
#include "Hash.h"

using namespace std;
namespace iret {

Hash::Hash(void) : FBase("hshset","null"){
}

Hash::Hash(const char *nam) : FBase("hshset",nam){
}

Hash::~Hash(){
}

void Hash::create_htable(List &Lst,int excess){
   char cnam[max_str],*cptr,*uptr;
   int u,pflag=get_qflag(),len;
   int ct,i,j,k;
   ofstream *pfout;

   nwrds=Lst.cnt_key;
   ct=nwrds;
   tnum=1;
   u=0;
   while(ct=ct/2){tnum*=2;u++;}
   if(u>30){cout << "Error in size, " << u << endl;exit(0);}
   i=0;
   while((u<32)&&(i<excess)){tnum*=2;u++;i++;}
   if(pflag)cout << "excess: " << i << endl;
   tnum--;
   harr=new int[tnum+2];
   for(ct=0;ct<tnum+2;ct++)harr[ct]=0;

   farr=new int[1536];
   ct=1;
   for(i=0;i<1536;i++){
      farr[i]=ct=(ct*331)&tnum;
   }
  
   int *pc0=farr,*pc1=farr+128,*pc2=farr+256;
   int *pc3=farr+384,*pc4=farr+512,*pc5=farr+640;
   int *pc6=farr+768,*pc7=farr+896,*pc8=farr+1024;
   int *pc9=farr+1152,*pc10=farr+1280,*pc11=farr+1408;
   
   Lst.node_first();
   while(Lst.node_next()){
      cptr=Lst.show_str();
      ct=0;
      i=0;
      while(u=*(cptr++)){
         switch(i){
            case 0: ct+=*(pc0+u);
                    break;
            case 1: ct+=*(pc1+u);
                    break;
            case 2: ct+=*(pc2+u);
                    break;
            case 3: ct+=*(pc3+u);
                    break;
            case 4: ct+=*(pc4+u);
                    break;
            case 5: ct+=*(pc5+u);
                    break;
            case 6: ct+=*(pc6+u);
                    break;
            case 7: ct+=*(pc7+u);
                    break;
            case 8: ct+=*(pc8+u);
                    break;
            case 9: ct+=*(pc9+u);
                    break;
            case 10: ct+=*(pc10+u);
                    break;
            case 11: ct+=*(pc11+u);
                     i-=12;
                    break;
         }
         i++;
      }
      (harr[ct&tnum])++;
   }
   if(pflag)cout << "tnum: " << tnum << " nwrds: " << nwrds << endl;

   //Set start points in harr.
   k=0;
   for(i=0;i<tnum+2;i++){
      j=harr[i];
      harr[i]=k;
      k+=j;
   }
   if(k!=nwrds){cout << "Error in summing!" << endl;exit(0);}

   //Write out harr.
   bin_Writ("ha",(tnum+2)*sizeof(long),(char*)harr);

   //Set addresses
   char **addt=(char **)new long[nwrds];
   Lst.node_first();
   while(Lst.node_next()){
      uptr=cptr=Lst.show_str();
      ct=0;
      i=0;
      while(u=*(cptr++)){
         switch(i){
            case 0: ct+=*(pc0+u);
                    break;
            case 1: ct+=*(pc1+u);
                    break;
            case 2: ct+=*(pc2+u);
                    break;
            case 3: ct+=*(pc3+u);
                    break;
            case 4: ct+=*(pc4+u);
                    break;
            case 5: ct+=*(pc5+u);
                    break;
            case 6: ct+=*(pc6+u);
                    break;
            case 7: ct+=*(pc7+u);
                    break;
            case 8: ct+=*(pc8+u);
                    break;
            case 9: ct+=*(pc9+u);
                    break;
            case 10: ct+=*(pc10+u);
                    break;
            case 11: ct+=*(pc11+u);
                     i-=12;
                    break;
         }
         i++;
      }
      k=ct&tnum;
      addt[harr[k]]=uptr;
      (harr[k])++;
   }

   //Write out string file
   pfout=get_Ostr("str");
   k=0;
   for(i=0;i<nwrds;i++){
      *pfout << addt[i] << ends;
      len=strlen((char*)addt[i])+1;
      addt[i]=(char*)k;
      k+=len;
   }
   dst_Ostr(pfout);

   //Write out addr file
   bin_Writ("ad",nwrds*sizeof(int),(char*)addt);
   delete [] addt;
   // Added to prevent memory leak -- Halil
   delete [] harr;
   delete [] farr;

   //Write out counts
   pfout=get_Ostr("nm");
   *pfout << nwrds << " " << tnum << " " << k << endl;
   dst_Ostr(pfout);
}

void Hash::gopen_htable_map(void){
   char cnam[max_str],*cptr;
   int fld,pflag=get_qflag();
   int ct,asize,i;
   
   ifstream *pfin=get_Istr("nm");
   *pfin >> nwrds >> tnum >> asize;
   if(pflag) cout << nwrds << " " << tnum << " " << asize << endl;
   dst_Istr(pfin);

   harr=(int*)get_Mmap("ha");
   addr=(int*)get_Mmap("ad");
   strmap=get_Mmap("str");

   farr=new int[1536];
   ct=1;
   for(i=0;i<1536;i++){
      farr[i]=ct=(ct*331)&tnum;
   }

   px0=farr,px1=farr+128,px2=farr+256;
   px3=farr+384,px4=farr+512,px5=farr+640;
   px6=farr+768,px7=farr+896,px8=farr+1024;
   px9=farr+1152,px10=farr+1280,px11=farr+1408;
}

// Added to prevent memory leak -- Halil
void Hash::gclose_htable_map(void){
    dst_Mmap("ha",(char*)harr);
    dst_Mmap("ad",(char*)addr);
    dst_Mmap("str",strmap);
    delete [] farr;
}

int Hash::find(const char *str){
   register int ct=0,i=0,k;
   register int ic;
   register const char *utr=str;

   while(ic=*(utr++)){
      switch(i){
         case 0: ct+=*(px0+ic);
                 break;
         case 1: ct+=*(px1+ic);
                 break;
         case 2: ct+=*(px2+ic);
                 break;
         case 3: ct+=*(px3+ic);
                 break;
         case 4: ct+=*(px4+ic);
                 break;
         case 5: ct+=*(px5+ic);
                 break;
         case 6: ct+=*(px6+ic);
                 break;
         case 7: ct+=*(px7+ic);
                 break;
         case 8: ct+=*(px8+ic);
                 break;
         case 9: ct+=*(px9+ic);
                 break;
         case 10: ct+=*(px10+ic);
                 break;
         case 11: ct+=*(px11+ic);
                  i-=12;
                 break;
      }
      i++;
   }
   k=ct&tnum;
   ct=harr[k+1];
   i=harr[k];
   switch(ct-i){
      case 0: return(0);
              break;
      case 1: if(!strcmp(str,strmap+addr[i]))return(i+1);
              else return(0);
              break;
      case 2: ic=strcmp(str,strmap+addr[i]);
              if(ic>0){
                 if(!strcmp(str,strmap+addr[i+1]))return(i+2);
                 else return(0);
              }
              else if(ic<0)return(0);
              else return(i+1);
              break;
      default: ic=strcmp(str,strmap+addr[i]);
               if(ic<0)return(0);
               else if(!ic)return(i+1);
               ct--;
               ic=strcmp(str,strmap+addr[ct]);
               if(ic>0)return(0);
               else if(!ic)return(ct+1);
               while(ct-i>1){
                  k=(ct+i)/2;
                  ic=strcmp(str,strmap+addr[k]);
                  if(ic>0)i=k;
                  else if(ic<0)ct=k;
                  else return(k+1);
               }
               return(0);
   }
}

//Chash code

Chash::Chash() : Hash(){
   change_type("cshset");
}

Chash::Chash(const char *str) : Hash(str){
   change_type("cshset");
}

Chash::~Chash(void){}

void Chash::create_ctable(Count &Ct,int excess){
   int pflag=get_qflag();
   change_type("cshset");
   create_htable(Ct,excess);
   gopen_htable_map();
   int n,i=0;
   int *pct=new int[Ct.cnt_key];
   Ct.node_first();
   while(Ct.node_next()){
      if(n=find(Ct.show_str())){
         pct[n-1]=Ct.count();
      }        
      else {
         cout << "Error in Count tree!" << endl;exit(0);
      }
      mark(pflag,++i,10000,"count terms");
   }
   bin_Writ("ct",Ct.cnt_key*sizeof(long),(char*)pct);
   delete [] pct;
   cnt=(int*)get_Mmap("ct");
   // Added to prevent memory leak -- Halil
   gclose_htable_map();
   dst_Mmap("ct",(char*)cnt);
}

void Chash::gopen_ctable_map(void){
   change_type("cshset");
   gopen_htable_map();
   cnt=(int*)get_Mmap("ct");
}   

// Added to prevent memory leak -- Halil
void Chash::gclose_ctable_map(void){
    gclose_htable_map();
    dst_Mmap("ct",(char*)cnt);
}

long Chash::count(const char *str){
   long n=find(str);
   if(n)return(cnt[n-1]);
   else return(0);
}

//RelateA code

RelateA::RelateA(void) : FBase("relatea","null"){
   // Added to prevent memory leak -- Halil
   if (name != NULL) delete [] name;   
   name=NULL;
}

RelateA::RelateA(const char *nam) : FBase("relatea",nam){
}

RelateA::~RelateA(){
}

int RelateA::create_bmatrix(BTList &Btl,int exc1,int exc2){
   char cnam[max_str],*cptr;
   int pflag=get_qflag();
   long i,j,k,ct,mapsize;

   nwrd1=Btl.cnt_key;
   nwrd2=Btl.lst->cnt_key;
   cout << nwrd1 << " " << nwrd2 << endl;

   //Create Hash for strings set 1
   strcpy(cnam,name);
   strcat(cnam,"-s1");
   Coord1.change_name(cnam);
   Coord1.create_htable(Btl,exc1);
   //Create Hash for strings set 2
   strcpy(cnam,name);
   strcat(cnam,"-s2");
   Coord2.change_name(cnam);
   Coord2.create_htable(*(Btl.lst),exc2);

   //Write out numbers of strings
   get_pathw(cnam,"relatea",name,"nm");
   ofstream fout(cnam,ios::out);
   fout << nwrd1 << " " << nwrd2 << endl;
   fout.close();

   //Map Hash structures
   Coord1.gopen_htable_map();
   Coord2.gopen_htable_map();

   //Make map
   ad=new int[nwrd1+2];
   ad[0]=0;
   ad[nwrd1+1]=0;
   ct=0;
   mapsize=0;
   Btl.node_first();
   while(Btl.node_next()){
      if(i=Coord1.find(Btl.show_str())){
         mapsize+=ad[i]=Btl.list_size();
      }
      else {cout << "Error in term handling!" << endl;exit(0);}
      mark(pflag,++ct,10000,"lex_pair counts");
   }
   map=new int[mapsize];
   k=0;
   for(i=1;i<nwrd1+2;i++){
      j=ad[i];
      ad[i]=k;
      k+=j;
   }
   if(k!=mapsize){cout << "Error in summing!" << endl;exit(0);}

   //Write out "ad" file (offsets into map);
   bin_Writ("ad",(nwrd1+2)*sizeof(long),(char*)ad);
   
   //Set numbers in map.
   ct=0;
   Btl.node_first();
   while(Btl.node_next()){
      if(i=Coord1.find(Btl.show_str())){
         ad1=ad[i];
         k=0;
         Btl.set_ptr();
         while((cptr = Btl.next_ptr()) != NULL) {
            j=Coord2.find(cptr);
            map[ad1+(k++)]=j;
         }
         sSort(k,map+ad1);
      }
      mark(pflag,++ct,10000,"lex_entries");
   }
   //Write out map.
   bin_Writ("map",sizeof(long)*mapsize,(char*)map);
   ofstream *pfout=get_Ostr("nm");
   *pfout << nwrd1 << " " << nwrd2 << " " << mapsize << endl;
   dst_Ostr(pfout);
   if(pflag) cout << nwrd1 << " " << nwrd2 << " " << mapsize << endl;

   // Added to prevent memory leak -- Halil
   Coord1.gclose_htable_map();
   Coord2.gclose_htable_map();
   delete [] ad;
   delete [] map;

   return(1);
}

void RelateA::gopen_bmatrix(void){
   char cnam[max_str],*cptr;
   int pflag=get_qflag();
   long ct,mapsize;

   ifstream *pfin=get_Istr("nm");
   *pfin >> nwrd1 >> nwrd2 >> mapsize;
   if(pflag)cout << nwrd1 << " " << nwrd2 << " " << mapsize << endl;
   dst_Istr(pfin);

   //map Hash structures
   strcpy(cnam,name);
   strcat(cnam,"-s1");
   Coord1.change_name(cnam);
   Coord1.gopen_htable_map();
   strcpy(cnam,name);
   strcat(cnam,"-s2");
   Coord2.change_name(cnam);
   Coord2.gopen_htable_map();

   //map binary files
   ad=(int*)get_Mmap("ad");
   map=(int*)get_Mmap("map");
}

// Added to prevent memory leak -- Halil
void RelateA::gclose_bmatrix(void) {
   Coord1.gclose_htable_map();
   Coord2.gclose_htable_map();
   dst_Mmap("ad",(char*)ad);
   dst_Mmap("map",(char*)map);
}

void RelateA::set_wrd(long n){
   ad1=ad[n];
   ad2=ad[n+1]-1;
}

int RelateA::exs_tag(long m){
   register long x=ad1;
   register long y=ad2;
   register long i,j;
   switch(y-x){
      case 0: if(map[x]!=m)return(0);
              else return(1);
              break;
      case 1: if((map[x]!=m)&&(map[y]!=m))return(0);
              else return(1);
              break;
      default: if((j=map[x])>=m){
                  if(j==m)return(1);
                  else return(0);
               }
               else if((j=map[y])<=m){
                  if(j==m)return(1);
                  else return(0);
               }
               else {
                  while(y-x>1){
                     i=(x+y)/2;
                     if((j=map[i])==m)return(1);
                     else if(j<m)x=i;
                     else y=i;
                  }
               }
               return(0);
   }
}
                     
int RelateA::exs_pair(long n,long m){
   set_wrd(n);
   return(exs_tag(m));
}

int RelateA::exs_pair(const char *str1,const char *str2){
   register long n,m;
   if(n=Coord1.find(str1)){
      set_wrd(n);
      if(m=Coord2.find(str2)){
         return(exs_tag(m));
      }
      else return(0);
   }
   else return(0);
}

//RelateB code

RelateB::RelateB(void) : FBase("relateb","null"){
   name=NULL;
}

RelateB::RelateB(const char *nam) : FBase("relateb",nam){
}

RelateB::~RelateB(){
}

int RelateB::create_bmatrix(BTList &Btl,int exc1,int exc2){
   char cnam[max_str],*cptr;
   int pflag=get_qflag();
   long ct;

   nwrd1=Btl.cnt_key;
   nwrd2=Btl.lst->cnt_key;
   cout << nwrd1 << " " << nwrd2 << endl;

   //Create Hash for strings set 1
   strcpy(cnam,name);
   strcat(cnam,"-s1");
   Coord1.change_name(cnam);
   Coord1.create_htable(Btl,exc1);
   //Create Hash for strings set 2
   strcpy(cnam,name);
   strcat(cnam,"-s2");
   Coord2.change_name(cnam);
   Coord2.create_htable(*(Btl.lst),exc2);

   //Write out number of words and number of tags
   ofstream *pfout=get_Ostr("nm");
   *pfout << nwrd1 << " " << nwrd2 << endl;
   dst_Ostr(pfout);

   //Load into Blist structures
   Coord1.gopen_htable_map();
   Coord2.gopen_htable_map();

   //Make bitmap
   long i,j,k;
   rd=(nwrd2+1)/8+1;
   map=new unsigned char[(nwrd1+1)*rd];
   for(i=0;i<(nwrd1+1)*rd;i++){
      map[i]=0;
      mark(pflag,i,100000,"zeros");
   }
   ct=0;
   Btl.node_first();
   while(Btl.node_next()){
      if(i=Coord1.find(Btl.show_str())){
         wrd=map+i*rd;
         Btl.set_ptr();
         while((cptr = Btl.next_ptr()) != NULL) {
            j=Coord2.find(cptr);
            (*(wrd+j/8))+=1<<(j%8);
         }
      }
      else {cout << "Error in term handling!" << endl;exit(0);}
      mark(pflag,++ct,10000,"lex_entries");
   }
   //Write out bit map.
   bin_Writ("bit",(nwrd1+1)*rd,(char*)map);

   // Added to prevent memory leak -- Halil
   Coord1.gclose_htable_map();
   Coord2.gclose_htable_map();
   delete [] map;

   return(1);
}

void RelateB::gopen_bmatrix(void){
   char cnam[max_str],*cptr;
   int pflag=get_qflag();
   long ct;

   ifstream *pfin=get_Istr("nm");
   *pfin >> nwrd1 >> nwrd2;
   if(pflag)cout << nwrd1 << " " << nwrd2 << endl;
   dst_Istr(pfin);

   //map Hash structures
   strcpy(cnam,name);
   strcat(cnam,"-s1");
   Coord1.change_name(cnam);
   Coord1.gopen_htable_map();
   strcpy(cnam,name);
   strcat(cnam,"-s2");
   Coord2.change_name(cnam);
   Coord2.gopen_htable_map();

   //map binary file
   rd=(nwrd2+1)/8+1;
   map=(unsigned char*)get_Mmap("bit");
}

// Added to prevent memory leak -- Halil
void RelateB::gclose_bmatrix(void) {
   Coord1.gclose_htable_map();
   Coord2.gclose_htable_map();
   dst_Mmap("bit",(char*)map);
}

void RelateB::set_wrd(long n){
   wrd=map+n*rd;
}

int RelateB::exs_tag(long m){
   return(*(wrd+m/8)&(1<<(m%8)));
}

int RelateB::exs_pair(long n,long m){
   wrd=map+n*rd;
   return(*(wrd+m/8)&(1<<(m%8)));
}

int RelateB::exs_pair(const char *str1,const char *str2){
   register long n,m;
   if(n=Coord1.find(str1)){
      set_wrd(n);
      if(m=Coord2.find(str2)){
         return(exs_tag(m));
      }
      else return(0);
   }
   else return(0);
}

//Lexicon code

Lexicon::Lexicon(const char *nam) : FBase("lexset",nam){
}

Lexicon::~Lexicon(){
}

int Lexicon::create_bmatrix(const char *path,int exc1,int exc2){
   char cnam[max_str],*cptr,*uptr;
   int pflag=get_qflag();
   long ct;
   BTList Btl;

   ifstream fin(path,ios::in);
   if(!fin.is_open()){cout << cnam << " failed to open!" << endl;exit(0);}
   ct=0;
   while(fin.getline(cnam, max_str, '\n')) {
      cptr = strtok(cnam, " ");
      if(*cptr){
         while((uptr = strtok(NULL, " ")) != NULL) {
            if(*uptr)Btl.add_unique(cptr,uptr);
         }
      }
      mark(pflag,++ct,10000,"lex_entries");
   }
   fin.close();

   nwrds=Btl.cnt_key;
   ntags=Btl.lst->cnt_key;
   cout << nwrds << " " << ntags << endl;

   //Create Hash for strings set 1
   strcpy(cnam,name);
   strcat(cnam,"-s1");
   Bterms.change_name(cnam);
   Bterms.create_htable(Btl,exc1);
   //Create Hash for strings set 2
   strcpy(cnam,name);
   strcat(cnam,"-s2");
   Btags.change_name(cnam);
   Btags.create_htable(*(Btl.lst),exc2);

   //Write out number of words and number of tags
   ofstream *pfout=get_Ostr("nm");
   *pfout << nwrds << " " << ntags << endl;
   dst_Ostr(pfout);

   //Map Hash structures
   Bterms.gopen_htable_map();
   Btags.gopen_htable_map();

   //Make bitmap 
   long i,j,k;
   rd=(ntags+1)/8+1;
   map=new unsigned char[(nwrds+1)*rd];
   ptag=new int[nwrds+1];
   for(i=0;i<(nwrds+1)*rd;i++){
      map[i]=0; 
      mark(pflag,i,100000,"zeros");
   }
   ct=0;
   Btl.node_first();
   while(Btl.node_next()){
      if(i=Bterms.find(Btl.show_str())){
         wrd=map+i*rd;
         Btl.set_ptr();
         cptr = Btl.next_ptr();
         j=Btags.find(cptr);
         ptag[i]=j;
         (*(wrd+j/8))+=1<<(j%8);
         while((cptr = Btl.next_ptr()) != NULL) {
            j=Btags.find(cptr);
            (*(wrd+j/8))+=1<<(j%8);
         }
      }
      else {cout << "Error in term handling!" << endl;exit(0);}
      mark(pflag,++ct,10000,"lex_entries");
   }
 
   bin_Writ("bit",(nwrds+1)*rd,(char*)map);
   bin_Writ("pt",(nwrds+1)*sizeof(long),(char*)ptag);

   // Added to prevent memory leak -- Halil
   Bterms.gclose_htable_map();
   Btags.gclose_htable_map();
   delete [] map;
   delete [] ptag;
   
   return(1);
}

void Lexicon::gopen_bmatrix(void){
   char cnam[max_str],*cptr;
   int pflag=get_qflag();
   long ct;
   
   ifstream *pfin=get_Istr("nm");
   *pfin >> nwrds >> ntags;
   dst_Istr(pfin);
   if(pflag)cout << nwrds << " " << ntags << endl;

   //map Hash structures
   strcpy(cnam,name);
   strcat(cnam,"-s1");
   Bterms.change_name(cnam);
   Bterms.gopen_htable_map();
   strcpy(cnam,name);
   strcat(cnam,"-s2");
   Btags.change_name(cnam);
   Btags.gopen_htable_map();

   //map binary files
   rd=(ntags+1)/8+1;
   map=(unsigned char*)get_Mmap("bit");
   ptag=(int*)get_Mmap("pt");
}

// Added to prevent memory leak -- Halil
void Lexicon::gclose_bmatrix(void) {
   Bterms.gclose_htable_map();
   Btags.gclose_htable_map();
   dst_Mmap("bit",(char*)map);
   dst_Mmap("pt",(char*)ptag);
}

void Lexicon::set_wrd(long n){
   wrd=map+n*rd;
}
   
int Lexicon::exs_tag(long m){
   return(*(wrd+m/8)&(1<<(m%8)));
}

int Lexicon::exs_pair(long n,long m){
   wrd=map+n*rd; 
   return(*(wrd+m/8)&(1<<(m%8)));
}

}

