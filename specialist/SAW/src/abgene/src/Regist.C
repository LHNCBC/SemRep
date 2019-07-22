#include <iostream>
#include <fstream>
#include <cstdlib>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <cmath>
#include <cstring>
#include <cassert>
#include "Btree.h"
#include "Regist.h"
#include "runn.h"
#include "Docum.h"
#include "Post.h"
#include "Word.h"
using namespace std;
namespace iret {

Regist::Regist(const char *nam) {
   int len=strlen(nam);
   name=new char[len+1];
   strcpy(name,nam);

   ostate=0;
   ndst =0;    //initialized here
   word_space=10000; //Default value.
   pBn=NULL;
   pIt=NULL;
   pIpat=NULL;
   long i;
   for(i=0;i<FILE_NUM;i++)pDrg[i]=NULL;
   tnm=new long[word_space];
   lct=new unsigned char[word_space];
}

Regist::Regist(const char *nam,long wrd_spc) {
   int len=strlen(nam);
   name=new char[len+1];
   strcpy(name,nam);

   ostate=0;
   ndst =0;    //initialized here
   word_space=wrd_spc; 
   pBn=NULL;
   pIt=NULL;
   pIpat=NULL;
   long i;
   for(i=0;i<FILE_NUM;i++)pDrg[i]=NULL;
   tnm=new long[word_space];
   lct=new unsigned char[word_space];
}

Regist::~Regist() {
   delete [] name;
	
   if(ndst){
      for (int i=0;i<ndst;i++) {
         delete [] nams[i];
         delete [] paths[i];
      }
   }
   if(pBn!=NULL)delete pBn;
   delete [] tnm;
   delete [] lct;
}

void Regist::add(char *nam,char *path) {
  char cnam[max_str];
  ifstream fin;
  int i, len=strlen(nam);

  nams[ndst] = new char[len+1];
  strcpy(nams[ndst], nam);

  len=strlen(path); 
  paths[ndst] = new char[len+1];
  strcpy(paths[ndst], path);

  strcpy(cnam, "docset_");
  strcat(cnam, nam);
  strcat(path,cnam);
  strcat(path,".n");
  fin.open(path,ios::in);
  if (!fin.is_open()) {
	cout<<"\nThere is no such docset!!\n\n";
	return;
  }
  fin>>i;
  ndoc[ndst] = i;
  fin.close();

  ndst++;
}

void Regist::replace(char *nam, char *path)     
{
  ifstream fin;
  char cnam[255];
  long num, len;
  int i;

  for (i=0;i<ndst;i++) {
    if (strcmp(nams[i],nam) == 0)
      break;
  }
  if(i==ndst){cout << "Error in replacement request name!" << endl;exit(0);}
  ntot -= ndoc[i];
  len=strlen(path); 
  paths[i] = new char[len+1];
  strcpy(paths[i], path);

  strcpy(cnam, "docset_");
  strcat(cnam, nam);
  strcat(path,cnam);
  strcat(path,".n");
  fin.open(path,ios::in);
  if (!fin.is_open()) {
        cout<<"\nThere is no such docset!!\n\n";
        return;
  }
  fin>>num;
  ndoc[i] = num;
  ntot += num;   
  fin.close();
}

void Regist::delet(char *nam) {
  int i, j, len;

  for (i=0;i<ndst;i++) {
    if (strcmp(nams[i],nam) == 0)
      break;
  }
  ntot -= ndoc[i];
  for (j=i;j<ndst-1;j++) {
    ndoc[j] = ndoc[j+1];
    len=strlen(nams[j+1]);
    nams[j] = new char[len+1];
    strcpy(nams[j], nams[j+1]);
    len=strlen(paths[j+1]);
    paths[j] = new char[len+1];
    strcpy(paths[j], paths[j+1]);
  }
  ndst--;
}

void Regist::move(char *nam, int posit) 
{
  int i, j, len;
  char temp1[255], temp2[255];
  long temp;
 
  if (posit>ndst) {
    cout<<"Posit is too big\n";
    exit(0);
  }
  for (i=0;i<ndst;i++) {
    if (strcmp(nams[i],nam) == 0)
      break;
  }
  if (i==posit) {
    cout<<"No move requested!" << endl;
    return;
  }
  else {
    temp = ndoc[i];
    strcpy(temp1,nams[i]);
    strcpy(temp2,paths[i]);
  }

  if (i<posit) {
    for (j=i;j<posit;j++) {
      ndoc[j] = ndoc[j+1];
      len=strlen(nams[j+1]);
      nams[j] = new char[len+1];
      strcpy(nams[j], nams[j+1]);
      len=strlen(paths[j+1]);
      paths[j] = new char[len+1];
      strcpy(paths[j], paths[j+1]);
    }
  }

  if (i>posit) {
    for (j=i;j>posit;j--) {
      ndoc[j] = ndoc[j-1];
      len=strlen(nams[j-1]);
      nams[j] = new char[len+1];
      strcpy(nams[j], nams[j-1]);
      len=strlen(paths[j-1]);
      paths[j] = new char[len+1];
      strcpy(paths[j], paths[j-1]);
    }
  }
  ndoc[posit] = temp; 
  len=strlen(temp1);
  nams[posit] = new char[len+1];
  strcpy(nams[posit],temp1);
  len=strlen(temp2);
  paths[posit] = new char[len+1];
  strcpy(paths[posit],temp2);
}
      
void Regist::readr() {
  ifstream fin;
  char cnam[max_str];

  get_pathw(cnam,"regist",name,"r");

  fin.open(cnam,ios::in);
  if (!fin.is_open()) {
     cout<<"no regist.r file available!\n";
     exit(0);
  }

  ntot = 0;
  fin>>ndst;           // the num. of docsets
  for (int i=0; i<ndst; i++) {
     nams[i] = new char[255];
     paths[i] = new char[255];
     fin>>nams[i];
     fin>>ndoc[i];
     fin>>paths[i];
     ntot += ndoc[i];
  }
  fin.close();
}

void Regist::writer() {
  ofstream fout;
  char cnam[max_str];

  get_pathw(cnam,"regist",name,"r");
  fout.open(cnam, ios::out);
  if (!fout) { 
     cout<<"no regist.r file available!\n";
     exit(0);
  }
  fout<<ndst<<endl;
  for (int i=0; i<ndst; i++) {
     fout<<nams[i]<<"\t"<<ndoc[i]<<"\t"<<paths[i]<<endl;
  }
  fout.close();
}

void Regist::screenr() {
  char cnam[max_str];
  ifstream fin;

  this->readr();
  cout<<"\nThe registing class includes the following docsets:\n";
  cout<<ndst<<endl;
  for (int i=0; i<ndst; i++) {
     cout<<nams[i]<<"\t"<<ndoc[i]<<"\t"<<paths[i]<<endl;
  }
}

void Regist::regstr() {
  char choice, cnam[max_str],path[max_str];
  int i;
  char mess2[50];  
  char mess1[50]; 
  ifstream fin;
 
  strcpy(mess2, "Enter the path, eg. /net/dolphin/d2/REGIST/\n");
  strcpy(mess1, "Enter the docset name you want add or x to exit\n");
  get_pathw(cnam,"regist",name,"r");
  fin.open(cnam, ios::in);
  if (!fin.is_open()) {
     cout<<"No regist.r file available! Do you want to creat one? y/n?\n";
     cin>>choice;
  }
  else 
    this->screenr();
  if (choice == 'y') {
    cout<<mess1;
    cin>>cnam;
    cin.get();
    while (strcmp(cnam,"x") != 0) {
     cout<<mess2;
     cin>>path;
     this->add(cnam,path);
     cout<<mess1;
     cin>>cnam;
     cin.get();
    }
    this->writer();
    this->screenr(); 
  }
   
  cout<<"\na--add   r--replace path   d--delete  m--move x--exit\n";
  cout<<"Enter a char, your update choice: ";
  cin>>choice;

  while (choice != 'x') { 
    this->readr();  
    if (choice=='a') { 
      cout<<mess1;
      cin>>cnam;
      cin.get();
      cout<<mess2;
      cin>>path;
      this->add(cnam,path);
    }
    if (choice == 'r') {
      cout<<"Enter the docset name you want to replace\n";
      cin>>cnam;
      cin.get();
      cout<<mess2;
      cin>>path;
      this->replace(cnam,path);
    }
    if (choice == 'd') {
      cout<<"Enter the docset name you want to delete\n";
      cin>>cnam;
      cin.get();
      this->delet(cnam);
    }
    if (choice == 'm') {
      cout<<"Enter the docset name you want to move\n";
      cin>>cnam;
      cin.get();
      cout<<"Enter the position you want to move it to, counting from 0\n";
      cin>>i;
      this->move(cnam, i);
    }
    this->writer();
    this->screenr();
    cout<<"\na--add   r--replace path   d--delete  m--move x--exit\n";
    cout<<"Enter a char, your update choice: ";
    cin>>choice;
  }
}

void Regist::set_class(void) {
  int i;
  char cnam[max_str];
  if((ostate&READ_C)==0){
     this->readr();

     for (i=0; i<ndst; i++) {
        strcpy(cnam, nams[i]);
        if(pDrg[i]==NULL)pDrg[i] = new Docum(cnam,word_space);
     }
     ostate=ostate|READ_C;
  }
}

void Regist::gopen_read(int rfil) {
  int i,pflag=get_qflag(); 
  ofstream fout;
  pBn=new Bnum(ndst+1);
  pBn->mm[0]=0;

  for (i=0; i<ndst; i++) {
     fout.open("path_docset", ios::out);
     fout<<paths[i]<<endl;
     fout.close();
     (*(pDrg[i])).gopen_read(rfil);
     pBn->mm[i+1]=ndoc[i]+pBn->mm[i];
     mark(pflag,i+1,1,"docset opened");
  }
  ostate=ostate|rfil;
}

void Regist::gopen_map(int rfil) {
  int i,pflag=get_qflag();
  ofstream fout;
  pBn=new Bnum(ndst+1);
  pBn->mm[0]=0;

  for (i=0; i<ndst; i++) {
     fout.open("path_docset", ios::out);
     fout<<paths[i]<<endl;
     fout.close();
     (*(pDrg[i])).gopen_map(rfil);
     pBn->mm[i+1]=ndoc[i]+pBn->mm[i];
     mark(pflag,i+1,1,"docset mapped");
  }
  ostate=ostate|rfil;
}

void Regist::gclose_read(int rfil) {
  int i;

  for (i=0; i<ndst; i++) {
     (*(pDrg[i])).gclose_read(rfil);
  }
  ostate=ostate&(~rfil);
}

void Regist::gclose_map(int rfil) {
  int i;
  ofstream fout;

  for (i=0; i<ndst; i++) {
     fout.open("path_docset", ios::out);
     fout<<paths[i]<<endl;
     fout.close();
     (*(pDrg[i])).gclose_map(rfil);
  }
  ostate=ostate&(~rfil);
}

Docum* Regist::read(long n) {
  int i=0, num=0;
 
  i=pBn->index(n);
  pDrg[i]->clear();
  pDrg[i]->read(n-(pBn->mm[i]));
  return(*(pDrg+i));
}

long Regist::xlen(long n) {
  int i=0;

  i=pBn->index(n);
  return (pDrg[i]->alen[n-(pBn->mm[i])]);
} 

long Regist::pmid(long n) {
  int i=0;

  if((n<0)||(n>=ntot)){cout << "Error in index!" << endl;exit(0);}
  i=pBn->index(n);
  return (pDrg[i]->auid[n-(pBn->mm[i])]);
} 

long Regist::index(long pmid) {
   long i,j,m,pmd,pmj;

   if(pmid < (pmd=this->pmid(0)))return(0);
   else if(pmid==pmd)return(1);
   if(pmid > (pmd=this->pmid(ntot-1)))return(0); 
   else if(pmid==pmd)return(ntot);
   i=0;
   j=ntot-1;
   pmj=pmd;
   while(j-i>1){
      m=(j+i)/2;
      pmd=this->pmid(m);
      if(pmid>pmd)i=m;
      else {j=m;pmj=pmd;}
   }
   if(pmid==pmj)return(j+1);
   else return(0);
}
  
long Regist::datx(long n) {
  int i=0;

  i=pBn->index(n);
  return (pDrg[i]->date[n-(pBn->mm[i])]);
} 
  
long Regist::index(long im, long *pmid) {
  long i,ix,match_ct;

  match_ct = 0;
  for (i=0;i<im;i++) {
    ix = this->index(*(pmid+i));
    if(ix>0){
       pmid[i] = ix-1;
       match_ct++;
    }
    else pmid[i]=-1;
  }
  return match_ct;
}

long Regist::datrang(long dat1,long dat2,pLong &idx){
   long ct=0,m,i,j,k=0,n,*daa;
   Docum *pDoc;

   long *udx=new long[ntot];

   for(i=0;i<ndst;i++){
      pDoc=pDrg[i];
      n=(pDoc->ndoc);
      daa=pDoc->date;
      for(j=0;j<n;j++){
         m=daa[j];
         if((dat1<=m)&&(m<dat2)){
            *(udx+ct++)=j+k;
         }
      }
      k+=pDrg[i]->ndoc;
   }

   cout << ct << " ids found in range" << endl;
 
   idx=new long[ct];
   for(i=0;i<ct;i++)*(idx+i)=*(udx+i);
   delete udx;

   return(ct);
}

long Regist::datgrat(long dat1,pLong &idx){
   long ct=0,m,i,j,k=0,n,*daa;
   Docum *pDoc;

   long *udx=new long[ntot];

   for(i=0;i<ndst;i++){
      pDoc=pDrg[i];
      n=(pDoc->ndoc);
      daa=pDoc->date;
      for(j=0;j<n;j++){
         m=daa[j];
         if(dat1<=m){
            *(udx+ct++)=j+k;
         }
      }
      k+=pDrg[i]->ndoc;
   }

   cout << ct << " ids found in range" << endl;
 
   idx=new long[ct];
   for(i=0;i<ct;i++)*(idx+i)=*(udx+i);
   delete udx;

   return(ct);
}

void Regist::convert(long num,IdTerm *idt){
   long i,j,k,n,err=0;
   int jx,pflag=get_qflag();
   char cnam[max_str],*ptr;

   long *nwd=new long[word_space];
   unsigned char *lwt=new unsigned char[word_space];
   Docum *pDoc=pDrg[num];

   ofstream ferr("Error.log",ios::app);
   ofstream fout("path_binset", ios::out);
   fout<<paths[num]<<endl;
   fout.close();

   get_pathw(cnam,"binset",nams[num],"a");
   ofstream faa(cnam,ios::out|ios::binary);
   get_pathw(cnam,"binset",nams[num],"b");
   fstream fbb(cnam,ios::out|ios::binary);
   for(i=0;i<pDoc->ndoc;i++){
      k=fbb.tellp();
      faa.write((char*)&k,sizeof(long));
      pDoc->clear();
      pDoc->read();
      k=pDoc->num_wrds();
      fbb.write((char*)&k,sizeof(long));
      n=0;
      while((ptr=pDoc->show(jx))!=NULL){
         j=idt->id_find(ptr);
         if(!j){
            cout << "Error, term: " << ptr << " from doc # " << i << " not found!" << endl;
            ferr << "Error, term: " << ptr << " from doc # " << i << " not found!" << endl;
            err++;
         }
         *(nwd+n)=j;
         if(jx>255)jx=255;
         *(lwt+n)=jx;
         n++;
      }
      fbb.write((char*)nwd,sizeof(long)*k);
      fbb.write((char*)lwt,k);
      mark(pflag,i+1,100,"docs converted");
   }
   faa.close();
   fbb.close();
   cout << err << " Errors in docset " << num << endl;
   ferr << err << " Errors in docset " << num << endl;
   ferr.close();
   delete [] nwd;
   delete [] lwt;
}

void Regist::gopen_binary(void){
   long i,j,tot;
   int pflag=get_qflag();
   char cnam[max_str];
   ifstream fss;
   ofstream fout;

 if((ostate&READ_B)==0){
   this->readr();
   if(pBn==NULL)pBn=new Bnum(ndst+1);
   pBn->mm[0]=0;

   offs=(long **)new long[ndst];
   pfin=new ifstream[ndst];
   for(i=0;i<ndst;i++){
      pBn->mm[i+1]=ndoc[i]+pBn->mm[i];

      fout.open("path_binset", ios::out);
      fout<<paths[i]<<endl;
      fout.close();

      get_pathw(cnam,"binset",nams[i],"a");
      fss.open(cnam,ios::in|ios::binary);
      if(!fss.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
      *(offs+i)=new long[ndoc[i]];
      fss.read((char*)(*(offs+i)),sizeof(long)*ndoc[i]);
      fss.close();

      get_pathw(cnam,"binset",nams[i],"b");
      pfin[i].open(cnam,ios::in|ios::binary);
      if(!pfin[i].is_open()){cout << cnam << " failed to open" << endl;exit(0);}
      mark(pflag,i+1,1,"binary file opened");
   }
 }
 ostate=ostate|READ_B;
}

void Regist::gopen_binary_map(void){
   long i,j,tot;
   int fld,pflag=get_qflag();
   char cnam[max_str];
   ifstream fss;
   ofstream fout;

 if((ostate&READ_B)==0){
   this->readr();
   if(pBn==NULL)pBn=new Bnum(ndst+1);
   pBn->mm[0]=0;

   offs=(long **)new long[ndst];
   pfin=new ifstream[ndst];
   for(i=0;i<ndst;i++){
      pBn->mm[i+1]=ndoc[i]+pBn->mm[i];

      fout.open("path_binset", ios::out);
      fout<<paths[i]<<endl;
      fout.close();

      get_pathw(cnam,"binset",nams[i],"a");
      fld=::open(cnam,O_RDONLY);
      if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
      *(offs+i)=(long*)mmap(0,sizeof(long)*ndoc[i],PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
      if(*(offs+i)==MAP_FAILED){cout << cnam << " failed to map" << endl;exit(0);}
      ::close(fld);

      get_pathw(cnam,"binset",nams[i],"b");
      pfin[i].open(cnam,ios::in|ios::binary);
      if(!pfin[i].is_open()){cout << cnam << " failed to open" << endl;exit(0);}
      mark(pflag,i+1,1,"binary file opened");
   }
 }
 ostate=ostate|READ_B;
}

long Regist::readb(long n){
   int i=0; 
   long num,adr;

   i=pBn->index(n);
   n-=pBn->mm[i];
   adr=*(*(offs+i)+n);
   pfin[i].seekg(adr,ios::beg);
   pfin[i].read((char*)&num,sizeof(long));
   pfin[i].read((char*)tnm,sizeof(long)*num);
   pfin[i].read((char*)lct,num);
   return(num);
}

void Regist::gclose_binary(void){
   long i;
 if(ostate=(ostate|READ_B)){
   if(pfin!=NULL){
      for(i=0;i<ndst;i++)pfin[i].close();
      delete [] pfin;
      pfin=NULL;
   }
   if(offs!=NULL){
      for(i=0;i<ndst;i++){
         if(*(offs+i)!=NULL)delete [] *(offs+i);
      }
      delete [] offs;
      offs=NULL;
   }  
 }
 ostate=ostate&(~READ_B);
}

void Regist::gclose_binary_map(void){
   long i;
 if(ostate=(ostate|READ_B)){
   if(pfin!=NULL){
      for(i=0;i<ndst;i++)pfin[i].close();
      delete [] pfin;
      pfin=NULL;
   }
   if(offs!=NULL){
      for(i=0;i<ndst;i++){
         if(*(offs+i)!=NULL)munmap((char*)(*(offs+i)),sizeof(long)*ndoc[i]);
      }
      delete [] offs;
      offs=NULL;
   } 
 }
 ostate=ostate&(~READ_B);
}

void Regist::gopen_itame_map(void){
   char cnam[max_str],inam[max_str],ipath[max_str];
   char pat[10];
   long i,j,k,flag;
   int pflag=get_qflag(),fld;

   ifstream fnn;
   get_pathw(cnam,"itameset",name,"paths");
   ifstream fin(cnam,ios::in);
   nit=0;
   flag=1;
   while(flag){
      fin >> inam >> ipath;
      strcat(ipath,"itameset_");
      strcat(ipath,inam);
      strcat(ipath,".n");
      fnn.open(ipath,ios::in);
      if(fnn.is_open()){
         nit++;
         fnn.close();
      }
      else flag=0;
   }
   fin.close();

   idoc=new long[nit];
   iadd=(long**)new long[nit];
   pfit=new ifstream[nit];

   fin.open(cnam,ios::in);
   itot=0;
   for(i=0;i<nit;i++){
      fin >> inam >> ipath;
      strcat(ipath,"itameset_");
      strcat(ipath,inam);
      strcat(ipath,".n");
      fnn.open(ipath,ios::in);
      fnn >> idoc[i];
      itot+=idoc[i];
      j=strlen(ipath);
      ipath[j-1]='a';
      fnn.close();
      fld=::open(ipath,O_RDONLY);
      if(fld<=0){cout << ipath << " failed to open" << endl;exit(0);}
      iadd[i]=(long*)mmap(0,sizeof(long)*idoc[i],PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
      if(iadd[i]==MAP_FAILED){cout << ipath << " failed to map" << endl;exit(0);}
      ::close(fld);
      ipath[j-1]='s';
      pfit[i].open(ipath,ios::in);
      if(!pfit[i].is_open()){cout << ipath << " failed to open!" << endl;exit(0);}
      mark(pflag,i+1,1,"itame file opened");
   }
   fin.close();

   if(pIt==NULL)pIt=new Bnum(nit+1);
   pIt->mm[0]=0;
   for(i=0;i<nit;i++)pIt->mm[i+1]=idoc[i]+pIt->mm[i];
   
   pIpat=new Patt(5);               // length of pattern
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".I");
   pIpat->add_patt(pat,1);
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".D");
   pIpat->add_patt(pat,2);
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".T");
   pIpat->add_patt(pat,3);
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".A");
   pIpat->add_patt(pat,4);
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".M");
   pIpat->add_patt(pat,5);
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".E");
   pIpat->add_patt(pat,10);
   pIpat->make_chain();
   pIpat->reset();
}

int Regist::set_itame_ptr(long index){
   long i,j,k;

   if(index>=itot)return(0);
   i=pIt->index(index);
   j=*(iadd[i]+(index-pIt->mm[i]));
   pfit[i].seekg(j-1,ios::beg);
   pcit=&pfit[i];
   cit=0;
   return(1);
}

int Regist::read_itame_pmid(long &len,char *text){
   long i,j,k;
   char c;

   i=cit;
   pIpat->reset();
   while(i<1){
      while(((c=pcit->get())!=EOF)&&(!(i=pIpat->trigger(c))));
      if((c==EOF)||(i>1)){cit=i;len=0;return(0);}
      else pIpat->reset();  // reset after reading pattern
   }
   if(i>1){cit=i;len=0;return(0);}
   len=0;
   while(!(j=pIpat->capture(len,text,(c=pcit->get()))));
   cit=j;
   return(1);
}

int Regist::read_itame_date(long &len,char *text){
   long i,j,k;
   char c;

   i=cit;
   pIpat->reset();
   while(i<2){
      while(((c=pcit->get())!=EOF)&&(!(i=pIpat->trigger(c))));
      if((c==EOF)||(i>2)){cit=i;len=0;return(0);}
      else pIpat->reset();  // reset after reading pattern
   }
   if(i>2){cit=i;len=0;return(0);}
   len=0;
   while(!(j=pIpat->capture(len,text,(c=pcit->get()))));
   cit=j;
   return(1);
}

int Regist::read_itame_title(long &len,char *text){
   long i,j,k;
   char c;

   i=cit;
   pIpat->reset();
   while(i<3){
      while(((c=pcit->get())!=EOF)&&(!(i=pIpat->trigger(c))));
      if((c==EOF)||(i>3)){cit=i;len=0;return(0);}
      else pIpat->reset();  // reset after reading pattern
   }
   if(i>3){cit=i;len=0;return(0);}
   len=0;
   while(!(j=pIpat->capture(len,text,(c=pcit->get()))));
   cit=j;
   return(1);
}

int Regist::read_itame_abstract(long &len,char *text){
   long i,j;
   char c;

   i=cit;
   pIpat->reset();
   while(i<4){
      while(((c=pcit->get())!=EOF)&&(!(i=pIpat->trigger(c))));
      if((c==EOF)||(i>4)){cit=i;len=0;return(0);}
      else pIpat->reset();  // reset after reading pattern
   }
   if(i>4){cit=i;len=0;return(0);}
   len=0;
   while(!(j=pIpat->capture(len,text,(c=pcit->get()))));
   cit=j;
   return(1);
}

int Regist::read_itame_mesh(long &len,char *text){
   long i,j;
   char c;

   i=cit;
   pIpat->reset();
   while(i<5){
      while(((c=pcit->get())!=EOF)&&(!(i=pIpat->trigger(c))));
      if((c==EOF)||(i>5)){cit=i;len=0;return(0);}
      else pIpat->reset();  // reset after reading pattern
   }
   if(i>5){cit=i;len=0;return(0);}
   len=0;
   while(!(j=pIpat->capture(len,text,(c=pcit->get()))));
   cit=j;
   return(1);
}

int Regist::read_itame_doc(long &len,char *text){
   long i,j,k;
   char *ptr;

   len=0;
   *(text+len)='\n';
   *(text+len+1)='.';
   ptr=text+len+3;
   read_itame_pmid(i,ptr);
   if(i>0){
      *(text+len+2)='I';
      len+=i+3;
      *(text+len)='\n';
      *(text+len+1)='.';
      ptr=text+len+3;
   }
   read_itame_date(i,ptr);
   if(i>0){
      *(text+len+2)='D';
      len+=i+3;
      *(text+len)='\n';
      *(text+len+1)='.';
      ptr=text+len+3;
   }
   read_itame_title(i,ptr);
   if(i>0){
      *(text+len+2)='T';
      len+=i+3;
      *(text+len)='\n';
      *(text+len+1)='.';
      ptr=text+len+3;
   }
   read_itame_abstract(i,ptr);
   if(i>0){
      *(text+len+2)='A';
      len+=i+3;
      *(text+len)='\n';
      *(text+len+1)='.';
      ptr=text+len+3;
   }
   read_itame_mesh(i,ptr);
   if(i>0){
      *(text+len+2)='M';
      len+=i+3;
      *(text+len)='\n';
      *(text+len+1)='.';
      ptr=text+len+3;
   }
   *(text+len+2)='E';
   len+=3;
   return(len);
}

void Regist::gclose_itame_map(void){
   long i,j,k;

   if(iadd!=NULL){
      for(i=0;i<nit;i++){
         if(iadd[i]!=NULL)munmap((char*)(iadd[i]),sizeof(long)*idoc[i]);
         pfit[i].close();
      }
      delete [] iadd;
      iadd=NULL;
      delete [] pfit;
      pfit=NULL;
      delete [] idoc;
      idoc=NULL;
      delete pIt;
      delete pIpat;
   }
}   

//Multithreading version.
   unsigned num_thb; //Number of threads allowed in process.
   pthread_t *tib; //Threads (num_thb);
   Tb_data **pTb; //Array of thread arguments (num_thb).
   sem_t smb; //Semaphore.
   int *adb; //Array num_thb long for controlling access to Tb[].
   int bbt; //first available index
   int tbp; //first empty spot
   pthread_mutex_t lbbt; //Lock for bbt variable
   pthread_mutex_t ltbp; //Lock for tbp variable

Regist_pth::Regist_pth(const char *nam) {
   int len=strlen(nam);
   name=new char[len+1];
   strcpy(name,nam);

   ostate=0;
   ndst =0;    //initialized here
   word_space=10000; //Default value.
   pBn=NULL;
   long i;
   for(i=0;i<FILE_NUM;i++)pDrg[i]=NULL;
   tnm=new long[word_space];
   lct=new unsigned char[word_space];
}

Regist_pth::Regist_pth(const char *nam,long wrd_spc) {
   int len=strlen(nam);
   name=new char[len+1];
   strcpy(name,nam);

   ostate=0;
   ndst =0;    //initialized here
   word_space=wrd_spc; 
   pBn=NULL;
   long i;
   for(i=0;i<FILE_NUM;i++)pDrg[i]=NULL;
   tnm=new long[word_space];
   lct=new unsigned char[word_space];
}

Regist_pth::~Regist_pth() {
   delete [] name;
	
   if(ndst){
      for (int i=0;i<ndst;i++) {
         delete [] nams[i];
         delete [] paths[i];
      }
   }
   if(pBn!=NULL)delete pBn;
   delete [] tnm;
   delete [] lct;
}

void Regist_pth::add(char *nam,char *path) {
  char cnam[max_str];
  ifstream fin;
  int i, len=strlen(nam);

  nams[ndst] = new char[len+1];
  strcpy(nams[ndst], nam);

  len=strlen(path); 
  paths[ndst] = new char[len+1];
  strcpy(paths[ndst], path);

  strcpy(cnam, "docset_");
  strcat(cnam, nam);
  strcat(path,cnam);
  strcat(path,".n");
  fin.open(path,ios::in);
  if (!fin.is_open()) {
	cout<<"\nThere is no such docset!!\n\n";
	return;
  }
  fin>>i;
  ndoc[ndst] = i;
  fin.close();

  ndst++;
}

void Regist_pth::replace(char *nam, char *path)     
{
  ifstream fin;
  char cnam[255];
  long num, len;
  int i;

  for (i=0;i<ndst;i++) {
    if (strcmp(nams[i],nam) == 0)
      break;
  }
  if(i==ndst){cout << "Error in replacement request name!" << endl;exit(0);}
  ntot -= ndoc[i];
  len=strlen(path); 
  paths[i] = new char[len+1];
  strcpy(paths[i], path);

  strcpy(cnam, "docset_");
  strcat(cnam, nam);
  strcat(path,cnam);
  strcat(path,".n");
  fin.open(path,ios::in);
  if (!fin.is_open()) {
        cout<<"\nThere is no such docset!!\n\n";
        return;
  }
  fin>>num;
  ndoc[i] = num;
  ntot += num;   
  fin.close();
}

void Regist_pth::delet(char *nam) {
  int i, j, len;

  for (i=0;i<ndst;i++) {
    if (strcmp(nams[i],nam) == 0)
      break;
  }
  ntot -= ndoc[i];
  for (j=i;j<ndst-1;j++) {
    ndoc[j] = ndoc[j+1];
    len=strlen(nams[j+1]);
    nams[j] = new char[len+1];
    strcpy(nams[j], nams[j+1]);
    len=strlen(paths[j+1]);
    paths[j] = new char[len+1];
    strcpy(paths[j], paths[j+1]);
  }
  ndst--;
}

void Regist_pth::move(char *nam, int posit) 
{
  int i, j, len;
  char temp1[255], temp2[255];
  long temp;
 
  if (posit>ndst) {
    cout<<"Posit is too big\n";
    exit(0);
  }
  for (i=0;i<ndst;i++) {
    if (strcmp(nams[i],nam) == 0)
      break;
  }
  if (i==posit) {
    cout<<"No move requested!" << endl;
    return;
  }
  else {
    temp = ndoc[i];
    strcpy(temp1,nams[i]);
    strcpy(temp2,paths[i]);
  }

  if (i<posit) {
    for (j=i;j<posit;j++) {
      ndoc[j] = ndoc[j+1];
      len=strlen(nams[j+1]);
      nams[j] = new char[len+1];
      strcpy(nams[j], nams[j+1]);
      len=strlen(paths[j+1]);
      paths[j] = new char[len+1];
      strcpy(paths[j], paths[j+1]);
    }
  }

  if (i>posit) {
    for (j=i;j>posit;j--) {
      ndoc[j] = ndoc[j-1];
      len=strlen(nams[j-1]);
      nams[j] = new char[len+1];
      strcpy(nams[j], nams[j-1]);
      len=strlen(paths[j-1]);
      paths[j] = new char[len+1];
      strcpy(paths[j], paths[j-1]);
    }
  }
  ndoc[posit] = temp; 
  len=strlen(temp1);
  nams[posit] = new char[len+1];
  strcpy(nams[posit],temp1);
  len=strlen(temp2);
  paths[posit] = new char[len+1];
  strcpy(paths[posit],temp2);
}
      
void Regist_pth::readr() {
  ifstream fin;
  char cnam[max_str];

  get_pathw(cnam,"regist",name,"r");

  fin.open(cnam,ios::in);
  if (!fin.is_open()) {
     cout<<"no regist.r file available!\n";
     exit(0);
  }

  ntot = 0;
  fin>>ndst;           // the num. of docsets
  for (int i=0; i<ndst; i++) {
     nams[i] = new char[255];
     paths[i] = new char[255];
     fin>>nams[i];
     fin>>ndoc[i];
     fin>>paths[i];
     ntot += ndoc[i];
  }
  fin.close();
}

void Regist_pth::writer() {
  ofstream fout;
  char cnam[max_str];

  get_pathw(cnam,"regist",name,"r");
  fout.open(cnam, ios::out);
  if (!fout) { 
     cout<<"no regist.r file available!\n";
     exit(0);
  }
  fout<<ndst<<endl;
  for (int i=0; i<ndst; i++) {
     fout<<nams[i]<<"\t"<<ndoc[i]<<"\t"<<paths[i]<<endl;
  }
  fout.close();
}

void Regist_pth::screenr() {
  char cnam[max_str];
  ifstream fin;

  this->readr();
  cout<<"\nThe registing class includes the following docsets:\n";
  cout<<ndst<<endl;
  for (int i=0; i<ndst; i++) {
     cout<<nams[i]<<"\t"<<ndoc[i]<<"\t"<<paths[i]<<endl;
  }
}

void Regist_pth::regstr() {
  char choice, cnam[max_str],path[max_str];
  int i;
  char mess2[50];  
  char mess1[50]; 
  ifstream fin;
 
  strcpy(mess2, "Enter the path, eg. /net/dolphin/d2/REGIST/\n");
  strcpy(mess1, "Enter the docset name you want add or x to exit\n");
  get_pathw(cnam,"regist",name,"r");
  fin.open(cnam, ios::in);
  if (!fin.is_open()) {
     cout<<"No regist.r file available! Do you want to creat one? y/n?\n";
     cin>>choice;
  }
  else 
    this->screenr();
  if (choice == 'y') {
    cout<<mess1;
    cin>>cnam;
    cin.get();
    while (strcmp(cnam,"x") != 0) {
     cout<<mess2;
     cin>>path;
     this->add(cnam,path);
     cout<<mess1;
     cin>>cnam;
     cin.get();
    }
    this->writer();
    this->screenr(); 
  }
   
  cout<<"\na--add   r--replace path   d--delete  m--move x--exit\n";
  cout<<"Enter a char, your update choice: ";
  cin>>choice;

  while (choice != 'x') { 
    this->readr();  
    if (choice=='a') { 
      cout<<mess1;
      cin>>cnam;
      cin.get();
      cout<<mess2;
      cin>>path;
      this->add(cnam,path);
    }
    if (choice == 'r') {
      cout<<"Enter the docset name you want to replace\n";
      cin>>cnam;
      cin.get();
      cout<<mess2;
      cin>>path;
      this->replace(cnam,path);
    }
    if (choice == 'd') {
      cout<<"Enter the docset name you want to delete\n";
      cin>>cnam;
      cin.get();
      this->delet(cnam);
    }
    if (choice == 'm') {
      cout<<"Enter the docset name you want to move\n";
      cin>>cnam;
      cin.get();
      cout<<"Enter the position you want to move it to, counting from 0\n";
      cin>>i;
      this->move(cnam, i);
    }
    this->writer();
    this->screenr();
    cout<<"\na--add   r--replace path   d--delete  m--move x--exit\n";
    cout<<"Enter a char, your update choice: ";
    cin>>choice;
  }
}

void Regist_pth::set_class(void) {
  int i;
  char cnam[max_str];
  if((ostate&READ_C)==0){
     this->readr();

     for (i=0; i<ndst; i++) {
        strcpy(cnam, nams[i]);
        if(pDrg[i]==NULL)pDrg[i] = new Docum(cnam,word_space);
     }
     ostate=ostate|READ_C;
  }
}

void Regist_pth::gopen_read(int rfil) {
  int i,pflag=get_qflag(); 
  ofstream fout;
  pBn=new Bnum(ndst+1);
  pBn->mm[0]=0;

  for (i=0; i<ndst; i++) {
     fout.open("path_docset", ios::out);
     fout<<paths[i]<<endl;
     fout.close();
     (*(pDrg[i])).gopen_read(rfil);
     pBn->mm[i+1]=ndoc[i]+pBn->mm[i];
     mark(pflag,i+1,1,"docset opened");
  }
  ostate=ostate|rfil;
}

void Regist_pth::gopen_map(int rfil) {
  int i,pflag=get_qflag();
  ofstream fout;
  pBn=new Bnum(ndst+1);
  pBn->mm[0]=0;

  for (i=0; i<ndst; i++) {
     fout.open("path_docset", ios::out);
     fout<<paths[i]<<endl;
     fout.close();
     (*(pDrg[i])).gopen_map(rfil);
     pBn->mm[i+1]=ndoc[i]+pBn->mm[i];
     mark(pflag,i+1,1,"docset mapped");
  }
  ostate=ostate|rfil;
}

void Regist_pth::gclose_read(int rfil) {
  int i;

  for (i=0; i<ndst; i++) {
     (*(pDrg[i])).gclose_read(rfil);
  }
  ostate=ostate&(~rfil);
}

void Regist_pth::gclose_map(int rfil) {
  int i;
  ofstream fout;

  for (i=0; i<ndst; i++) {
     fout.open("path_docset", ios::out);
     fout<<paths[i]<<endl;
     fout.close();
     (*(pDrg[i])).gclose_map(rfil);
  }
  ostate=ostate&(~rfil);
}

Docum* Regist_pth::read(long n) {
  int i=0, num=0;
 
  i=pBn->index(n);
  pDrg[i]->clear();
  pDrg[i]->read(n-(pBn->mm[i]));
  return(*(pDrg+i));
}

long Regist_pth::xlen(long n) {
  int i=0;

  i=pBn->index(n);
  return (pDrg[i]->alen[n-(pBn->mm[i])]);
} 

long Regist_pth::pmid(long n) {
  int i=0;
  if((n<0)||(n>=ntot)){cout << "Error in index!" << endl;exit(0);}
  i=pBn->index(n);
  return (pDrg[i]->auid[n-(pBn->mm[i])]);
} 

long Regist_pth::index(long pmid) {
   long i,j,m,pmd,pmj;

   if(pmid < (pmd=this->pmid(0)))return(0);
   else if(pmid==pmd)return(1);
   if(pmid > (pmd=this->pmid(ntot-1)))return(0); 
   else if(pmid==pmd)return(ntot);
   i=0;
   j=ntot-1;
   pmj=pmd;
   while(j-i>1){
      m=(j+i)/2;
      pmd=this->pmid(m);
      if(pmid>pmd)i=m;
      else {j=m;pmj=pmd;}
   }
   if(pmid==pmj)return(j+1);
   else return(0);
}
  
long Regist_pth::datx(long n) {
  int i=0;

  i=pBn->index(n);
  return (pDrg[i]->date[n-(pBn->mm[i])]);
} 
  
long Regist_pth::index(long im, long *pmid) {
  long i,ix,match_ct;

  match_ct = 0;
  for (i=0;i<im;i++) {
    ix = this->index(*(pmid+i));
    if(ix>0){
       pmid[i] = ix-1;
       match_ct++;
    }
    else pmid[i]=-1;
  }
  return match_ct;
}

long Regist_pth::datrang(long dat1,long dat2,pLong &idx){
   long ct=0,m,i,j,k=0,n,*daa;
   Docum *pDoc;

   long *udx=new long[ntot];

   for(i=0;i<ndst;i++){
      pDoc=pDrg[i];
      n=(pDoc->ndoc);
      daa=pDoc->date;
      for(j=0;j<n;j++){
         m=daa[j];
         if((dat1<=m)&&(m<dat2)){
            *(udx+ct++)=j+k;
         }
      }
      k+=pDrg[i]->ndoc;
   }

   cout << ct << " ids found in range" << endl;
 
   idx=new long[ct];
   for(i=0;i<ct;i++)*(idx+i)=*(udx+i);
   delete udx;

   return(ct);
}

long Regist_pth::datgrat(long dat1,pLong &idx){
   long ct=0,m,i,j,k=0,n,*daa;
   Docum *pDoc;

   long *udx=new long[ntot];

   for(i=0;i<ndst;i++){
      pDoc=pDrg[i];
      n=(pDoc->ndoc);
      daa=pDoc->date;
      for(j=0;j<n;j++){
         m=daa[j];
         if(dat1<=m){
            *(udx+ct++)=j+k;
         }
      }
      k+=pDrg[i]->ndoc;
   }

   cout << ct << " ids found in range" << endl;
 
   idx=new long[ct];
   for(i=0;i<ct;i++)*(idx+i)=*(udx+i);
   delete udx;

   return(ct);
}

void Regist_pth::convert(long num,IdTerm *idt){
   long i,j,k,nwd[word_cnt],n,err=0;
   int jx,pflag=get_qflag();
   char cnam[max_str],*ptr;
   unsigned char lwt[word_cnt];
   Docum *pDoc=pDrg[num];

   ofstream ferr("Error.log",ios::app);
   ofstream fout("path_binset", ios::out);
   fout<<paths[num]<<endl;
   fout.close();

   get_pathw(cnam,"binset",nams[num],"a");
   ofstream faa(cnam,ios::out|ios::binary);
   get_pathw(cnam,"binset",nams[num],"b");
   fstream fbb(cnam,ios::out|ios::binary);
   for(i=0;i<pDoc->ndoc;i++){
      k=fbb.tellp();
      faa.write((char*)&k,sizeof(long));
      pDoc->clear();
      pDoc->read();
      k=pDoc->num_wrds();
      fbb.write((char*)&k,sizeof(long));
      n=0;
      while((ptr=pDoc->show(jx))!=NULL){
         j=idt->id_find(ptr);
         if(!j){
            cout << "Error, term: " << ptr << " from doc # " << i << " not found!" << endl;
            ferr << "Error, term: " << ptr << " from doc # " << i << " not found!" << endl;
            err++;
         }
         *(nwd+n)=j;
         if(jx>255)jx=255;
         *(lwt+n)=jx;
         n++;
      }
      fbb.write((char*)nwd,sizeof(long)*k);
      fbb.write((char*)lwt,k);
      mark(pflag,i+1,100,"docs converted");
   }
   faa.close();
   fbb.close();
   cout << err << " Errors in docset " << num << endl;
   ferr << err << " Errors in docset " << num << endl;
   ferr.close();
}

void Regist_pth::gopen_binary_pth(void){
   long i,j,tot;
   int pflag=get_qflag();
   char cnam[max_str];
   ifstream fss;
   ofstream fout;

 if((ostate&READ_B)==0){
   this->readr();
   if(pBn==NULL)pBn=new Bnum(ndst+1);
   pBn->mm[0]=0;

   offs=(long **)new long[ndst];
   pfin=new ifstream[ndst];
   for(i=0;i<ndst;i++){
      pBn->mm[i+1]=ndoc[i]+pBn->mm[i];

      fout.open("path_binset", ios::out);
      fout<<paths[i]<<endl;
      fout.close();

      get_pathw(cnam,"binset",nams[i],"a");
      fss.open(cnam,ios::in|ios::binary);
      if(!fss.is_open()){cout << cnam << " failed to open" << endl;exit(0);}
      *(offs+i)=new long[ndoc[i]];
      fss.read((char*)(*(offs+i)),sizeof(long)*ndoc[i]);
      fss.close();

      get_pathw(cnam,"binset",nams[i],"b");
      pfin[i].open(cnam,ios::in|ios::binary);
      if(!pfin[i].is_open()){cout << cnam << " failed to open" << endl;exit(0);}
      mark(pflag,i+1,1,"binary file opened");
   }

//Multithreading setup
   for(i=0;i<ndst;i++){
      if(pthread_mutex_init(&acc[i],NULL)){
         cout << "Error in mutex initialization!" << endl;
         exit(0);
      }
   }
   num_thb=10;
   tib=new pthread_t[num_thb];
   pTb=(Tb_data **)new long[num_thb];
   for(i=0;i<num_thb;i++){
      pTb[i]=new Tb_data(this,i);
   }
   if(sem_init(&smb,0,num_thb)){
      cout << "Semi_init failed!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lbbt,NULL)){
      cout << "Error in lbot mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&ltbp,NULL)){
      cout << "Error in ltop mutex initialization!" << endl;
      exit(0);
   }
   bbt=0;
   tbp=num_thb;
   adb=new int[num_thb];
   for(i=0;i<num_thb;i++)adb[i]=i;
 }
 ostate=ostate|READ_B;
}

void Regist_pth::gopen_binary_map(void){
   long i,j,tot;
   int fld,pflag=get_qflag();
   char cnam[max_str];
   ifstream fss;
   ofstream fout;

 if((ostate&READ_B)==0){
   this->readr();
   if(pBn==NULL)pBn=new Bnum(ndst+1);
   pBn->mm[0]=0;

   offs=(long **)new long[ndst];
   pfin=new ifstream[ndst];
   for(i=0;i<ndst;i++){
      pBn->mm[i+1]=ndoc[i]+pBn->mm[i];

      fout.open("path_binset", ios::out);
      fout<<paths[i]<<endl;
      fout.close();

      get_pathw(cnam,"binset",nams[i],"a");
      fld=::open(cnam,O_RDONLY);
      if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
      *(offs+i)=(long*)mmap(0,sizeof(long)*ndoc[i],PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
      if(*(offs+i)==MAP_FAILED){cout << cnam << " failed to map" << endl;exit(0);}
      ::close(fld);

      get_pathw(cnam,"binset",nams[i],"b");
      pfin[i].open(cnam,ios::in|ios::binary);
      if(!pfin[i].is_open()){cout << cnam << " failed to open" << endl;exit(0);}
      mark(pflag,i+1,1,"binary file opened");
   }

//Multithreading setup
   for(i=0;i<ndst;i++){
      if(pthread_mutex_init(&acc[i],NULL)){
         cout << "Error in mutex initialization!" << endl;
         exit(0);
      }
   }
   num_thb=10;
   tib=new pthread_t[num_thb];
   pTb=(Tb_data **)new long[num_thb];
   for(i=0;i<num_thb;i++){
      pTb[i]=new Tb_data(this,i);
   }
   if(sem_init(&smb,0,num_thb)){
      cout << "Semi_init failed!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lbbt,NULL)){
      cout << "Error in lbot mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&ltbp,NULL)){
      cout << "Error in ltop mutex initialization!" << endl;
      exit(0);
   }
   bbt=0;
   tbp=num_thb;
   adb=new int[num_thb];
   for(i=0;i<num_thb;i++)adb[i]=i;
 }
 ostate=ostate|READ_B;
}

long Regist_pth::readb(long n){
   int i=0; 
   long num,adr;

   i=pBn->index(n);
   n-=pBn->mm[i];
   adr=*(*(offs+i)+n);
   pthread_mutex_lock(&acc[i]);
   pfin[i].seekg(adr,ios::beg);
   pfin[i].read((char*)&num,sizeof(long));
   pfin[i].read((char*)tnm,sizeof(long)*num);
   pfin[i].read((char*)lct,num);
   pthread_mutex_unlock(&acc[i]);
   return(num);
}

void Regist_pth::gclose_binary_pth(void){
   long i;
 if(ostate=(ostate|READ_B)){
   if(pfin!=NULL){
      for(i=0;i<ndst;i++)pfin[i].close();
      delete [] pfin;
      pfin=NULL;
   }
   if(offs!=NULL){
      for(i=0;i<ndst;i++){
         if(*(offs+i)!=NULL)delete [] *(offs+i);
      }
      delete [] offs;
      offs=NULL;
   }  
   for(i=0;i<ndst;i++){
      if(pthread_mutex_destroy(&acc[i])){
         cout << "Error in mutex destruction!" << endl;
         exit(0);
      }
   }
   pthread_mutex_destroy(&lbbt);
   pthread_mutex_destroy(&ltbp);
   for(i=0;i<num_thb;i++){
      delete pTb[i];
   }
   delete [] pTb;
   delete [] tib;
   delete [] adb;
 }
 ostate=ostate&(~READ_B);
}

void Regist_pth::gclose_binary_map(void){
   long i;
 if(ostate=(ostate|READ_B)){
   if(pfin!=NULL){
      for(i=0;i<ndst;i++)pfin[i].close();
      delete [] pfin;
      pfin=NULL;
   }
   if(offs!=NULL){
      for(i=0;i<ndst;i++){
         if(*(offs+i)!=NULL)munmap((char*)(*(offs+i)),sizeof(long)*ndoc[i]);
      }
      delete [] offs;
      offs=NULL;
   }  
   for(i=0;i<ndst;i++){
      if(pthread_mutex_destroy(&acc[i])){
         cout << "Error in mutex destruction!" << endl;
         exit(0);
      }
   }
   pthread_mutex_destroy(&lbbt);
   pthread_mutex_destroy(&ltbp);
   for(i=0;i<num_thb;i++){
      delete pTb[i];
   }
   delete [] pTb;
   delete [] tib;
   delete [] adb;
 }
 ostate=ostate&(~READ_B);
}

void Regist_pth::gopen_itame_map(void){
   char cnam[max_str],inam[max_str],ipath[max_str];
   char pat[10];
   long i,j,k,flag;
   int pflag=get_qflag(),fld;

   ifstream fnn;
   get_pathw(cnam,"itameset",name,"paths");
   ifstream fin(cnam,ios::in);
   nit=0;
   flag=1;
   while(flag){
      fin >> inam >> ipath;
      strcat(ipath,"itameset_");
      strcat(ipath,inam);
      strcat(ipath,".n");
      fnn.open(ipath,ios::in);
      if(fnn.is_open()){
         nit++;
         fnn.close();
      }
      else flag=0;
   }
   fin.close();

   idoc=new long[nit];
   iadd=(long**)new long[nit];
   pfit=new ifstream[nit];

   fin.open(cnam,ios::in);
   itot=0;
   for(i=0;i<nit;i++){
      fin >> inam >> ipath;
      strcat(ipath,"itameset_");
      strcat(ipath,inam);
      strcat(ipath,".n");
      fnn.open(ipath,ios::in);
      fnn >> idoc[i];
      itot+=idoc[i];
      j=strlen(ipath);
      ipath[j-1]='a';
      fnn.close();
      fld=::open(ipath,O_RDONLY);
      if(fld<=0){cout << ipath << " failed to open" << endl;exit(0);}
      iadd[i]=(long*)mmap(0,sizeof(long)*idoc[i],PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
      if(iadd[i]==MAP_FAILED){cout << ipath << " failed to map" << endl;exit(0);}
      ::close(fld);
      ipath[j-1]='s';
      pfit[i].open(ipath,ios::in);
      if(!pfit[i].is_open()){cout << ipath << " failed to open!" << endl;exit(0);}
      mark(pflag,i+1,1,"itame file opened");
   }
   fin.close();

   if(pIt==NULL)pIt=new Bnum(nit+1);
   pIt->mm[0]=0;
   for(i=0;i<nit;i++)pIt->mm[i+1]=idoc[i]+pIt->mm[i];
   
   pIpat=new Patt(5);               // length of pattern
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".I");
   pIpat->add_patt(pat,1);
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".D");
   pIpat->add_patt(pat,2);
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".T");
   pIpat->add_patt(pat,3);
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".A");
   pIpat->add_patt(pat,4);
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".M");
   pIpat->add_patt(pat,5);
   pat[0]='\n';
   pat[1]='\0';
   strcat(pat,".E");
   pIpat->add_patt(pat,10);
   pIpat->make_chain();
   pIpat->reset();
}

int Regist_pth::set_itame_ptr(long index){
   long i,j,k;

   if(index>=itot)return(0);
   i=pIt->index(index);
   j=*(iadd[i]+(index-pIt->mm[i]));
   pfit[i].seekg(j-1,ios::beg);
   pcit=&pfit[i];
   cit=0;
   return(1);
}


int Regist_pth::read_itame_pmid(long &len,char *text){
   long i,j,k;
   char c;

   i=cit;
   pIpat->reset();
   while(i<1){
      do {
         pcit->get(c);
      }while((c!=EOF)&&(!(i=pIpat->trigger(c))));
      if((c==EOF)||(i>1)){cit=i;len=0;return(0);}
      else pIpat->reset();  // reset after reading pattern
   }
   if(i>1){cit=i;len=0;return(0);}
   len=0;
   do {
      pcit->get(c);
   }while(!(j=pIpat->capture(len,text,c)));
   cit=j;
   return(1);
}

int Regist_pth::read_itame_date(long &len,char *text){
   long i,j,k;
   char c;

   i=cit;
   pIpat->reset();
   while(i<2){
      do {
         pcit->get(c);
      }while((c!=EOF)&&(!(i=pIpat->trigger(c))));
      if((c==EOF)||(i>2)){cit=i;len=0;return(0);}
      else pIpat->reset();  // reset after reading pattern
   }
   if(i>2){cit=i;len=0;return(0);}
   len=0;
   do {
      pcit->get(c);
   }while(!(j=pIpat->capture(len,text,c)));
   cit=j;
   return(1);
}

int Regist_pth::read_itame_title(long &len,char *text){
   long i,j,k;
   char c;

   i=cit;
   pIpat->reset();
   while(i<3){
      do {
         pcit->get(c);
      }while((c!=EOF)&&(!(i=pIpat->trigger(c))));
      if((c==EOF)||(i>3)){cit=i;len=0;return(0);}
      else pIpat->reset();  // reset after reading pattern
   }
   if(i>3){cit=i;len=0;return(0);}
   len=0;
   do {
      pcit->get(c);
   }while(!(j=pIpat->capture(len,text,c)));
   cit=j;
   return(1);
}

int Regist_pth::read_itame_abstract(long &len,char *text){
   long i,j;
   char c;

   i=cit;
   pIpat->reset();
   while(i<4){
      do {
         pcit->get(c);
      }while((c!=EOF)&&(!(i=pIpat->trigger(c))));
      if((c==EOF)||(i>4)){cit=i;len=0;return(0);}
      else pIpat->reset();  // reset after reading pattern
   }
   if(i>4){cit=i;len=0;return(0);}
   len=0;
   do {
      pcit->get(c);
   }while(!(j=pIpat->capture(len,text,c)));
   cit=j;
   return(1);
}

int Regist_pth::read_itame_mesh(long &len,char *text){
   long i,j;
   char c;

   i=cit;
   pIpat->reset();
   while(i<5){
      do {
         pcit->get(c);
      }while((c!=EOF)&&(!(i=pIpat->trigger(c))));
      if((c==EOF)||(i>5)){cit=i;len=0;return(0);}
      else pIpat->reset();  // reset after reading pattern
   }
   if(i>5){cit=i;len=0;return(0);}
   len=0;
   do {
      pcit->get(c);
   }while(!(j=pIpat->capture(len,text,c)));
   cit=j;
   return(1);
}


int Regist_pth::read_itame_doc(long &len,char *text){
   long i,j,k;
   char *ptr,c;

   len=0;
   *(text+len)='\n';
   *(text+len+1)='.';
   ptr=text+len+3;
   read_itame_pmid(i,ptr);
   if(i>0){
      *(text+len+2)='I';
      len+=i+3;
      *(text+len)='\n';
      *(text+len+1)='.';
      ptr=text+len+3;
   }
   read_itame_date(i,ptr);
   if(i>0){
      *(text+len+2)='D';
      len+=i+3;
      *(text+len)='\n';
      *(text+len+1)='.';
      ptr=text+len+3;
   }
   read_itame_title(i,ptr);
   if(i>0){
      *(text+len+2)='T';
      len+=i+3;
      *(text+len)='\n';
      *(text+len+1)='.';
      ptr=text+len+3;
   }
   read_itame_abstract(i,ptr);
   if(i>0){
      *(text+len+2)='A';
      len+=i+3;
      *(text+len)='\n';
      *(text+len+1)='.';
      ptr=text+len+3;
   }
   read_itame_mesh(i,ptr);
   if(i>0){
      *(text+len+2)='M';
      len+=i+3;
      *(text+len)='\n';
      *(text+len+1)='.';
      ptr=text+len+3;
   }
   *(text+len+2)='E';
   len+=3;
   return(len);
}

void Regist_pth::gclose_itame_map(void){
   long i,j,k;

   if(iadd!=NULL){
      for(i=0;i<nit;i++){
         if(iadd[i]!=NULL)munmap((char*)(iadd[i]),sizeof(long)*idoc[i]);
         pfit[i].close();
      }
      delete [] iadd;
      iadd=NULL;
      delete [] pfit;
      pfit=NULL;
      delete [] idoc;
      idoc=NULL;
      delete pIt;
      delete pIpat;
   }
}   

Tb_data::Tb_data(Regist_pth *pReg,long i){
   pRgg=pReg;
   iz=i;

   sco=NULL;
   sx=NULL;
   tx=NULL;

   idn=new long[pReg->word_space];
   psx=(long**)new long[50000];
   ptx=(long**)new long[50000];
}

Tb_data::~Tb_data(){
   delete [] idn;
   delete [] psx;
   delete [] ptx;
}

void Tb_data::set_sco_data(long i,long b,long n,long *nx,float *scx,long *su){
   ii=i;
   beg=b;
   nm=n;
   nmx=nx;
   sco=scx;
   sx=su;
}

void Tb_data::set_cnt_data(long i,long n,long *nx,long *su,long *tu){
   ii=i;
   nm=n;
   nmx=nx;
   sx=su;
   tx=tu;
}

}   
