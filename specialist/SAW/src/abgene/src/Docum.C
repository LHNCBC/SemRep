#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include <runn.h>
#include <Btree.h>
#include <Docum.h>
#include <Post.h>
#include <Word.h>
using namespace std;
namespace iret {

Docum::Docum(void) : FBase("docset","null"){
   word_space=10000; //Default value.
   word=(char **)new long[word_space];
   lcnt=new int[word_space];
   hld=new int[word_space];
   nw=0;
}

Docum::Docum(const char *nam) : FBase("docset",nam){
   word_space=10000; //Default value.
   word=(char **)new long[word_space];
   lcnt=new int[word_space];
   hld=new int[word_space];
   nw=0;
}

Docum::Docum(const char *nam,long wrd_spc) : FBase("docset",nam){
   word_space=wrd_spc; 
   word=(char **)new long[word_space];
   lcnt=new int[word_space];
   hld=new int[word_space];
   nw=0;
}

Docum::~Docum(){
   for(int i=0;i<nw;i++){
      delete [] word[i];
   }
   delete [] word;
   delete [] lcnt;
   delete [] hld;
}

void Docum::gopen_read(int rfil){
   char cnam[max_str];
   ifstream *pfin;

   cflag=rfil;

   pfin=get_Istr("n");
   *pfin >> ndoc;
   pfin->close();

   if(Gcom(READ_W)){
      addr=(long*)get_Read("a");
      pfwd=get_Fstr("w",ios::in);

      nw=0;
      gct=-1;
   }
   else if(cflag&READ_W){ //may have problem when fwd is already being read
      pfwd->seekg(0, ios::beg);
   }

   if(Gcom(READ_M))alen=(long*)get_Read("m");
   if(Gcom(READ_U))auid=(long*)get_Read("ub");
   if(Gcom(READ_D))date=(long*)get_Read("d");
   if(Gcom(READ_S))size=(long*)get_Read("s");
}

void Docum::gopen_map(int rfil){
   char cnam[max_str];
   ifstream *pfin;

   cflag=rfil;

   pfin=get_Istr("n");
   *pfin >> ndoc;
   pfin->close();

   if(Gcom(READ_W)){
      addr=(long*)get_Read("a");
      pfwd=get_Fstr("w",ios::in);

      nw=0;
      gct=-1;
   }
   else if(cflag&READ_W){ //may have problem when fwd is already being read
      pfwd->seekg(0, ios::beg);
   }

   if(Gcom(READ_M))alen=(long*)get_Mmap("m");
   if(Gcom(READ_U))auid=(long*)get_Mmap("ub");
   if(Gcom(READ_D))date=(long*)get_Mmap("d");
   if(Gcom(READ_S))size=(long*)get_Mmap("s");
}

void Docum::read(void){
   *pfwd >> nw >> len;
   pfwd->get();
   for(int i=0;i<nw;i++){
      word[i]=new char[get_strinf(sword,*pfwd,'|')+1];
      strcpy(word[i],sword);
      *pfwd >> lcnt[i];
      pfwd->get();
   }
   ct=0;
   gct++;
}

void Docum::read(long n){
   pfwd->seekg(*(addr+n));
   *pfwd >> nw >> len;
   pfwd->get();
   for(int i=0;i<nw;i++){
      word[i]=new char[get_strinf(sword,*pfwd,'|')+1];
      strcpy(word[i],sword);
      *pfwd >> lcnt[i];
      pfwd->get();
   }
   ct=0;
   gct=n;
}

void Docum::gclose_read(int rfil){
   char *ptr;
   cflag=rfil;
   if(Rcom(READ_W)){
      delete [] addr;
      dst_Fstr(pfwd);
   }
   if(Rcom(READ_M))delete [] alen;
   if(Rcom(READ_U))delete [] auid;
   if(Rcom(READ_D))delete [] date;
   if(Rcom(READ_S))delete [] size;
}

void Docum::gclose_map(int rfil){
   cflag=rfil;
   if(Rcom(READ_W)){
      delete [] addr;
      dst_Fstr(pfwd);
   }
   if(Rcom(READ_M))dst_Mmap("m",(char*)alen);
   if(Rcom(READ_U))dst_Mmap("ub",(char*)auid);
   if(Rcom(READ_D))dst_Mmap("d",(char*)date);
   if(Rcom(READ_S))dst_Mmap("s",(char*)size);
}

void Docum::gopen_write(long n,int rfil){
   cflag=rfil;
   ndoc=n;

   addr=new long[ndoc];
   alen=new long[ndoc];
   if(Gcom(WRITE_U))auid=new long[ndoc];
   if(Gcom(WRITE_D))date=new long[ndoc];
   if(Gcom(WRITE_S))size=new long[ndoc];

   gct=0;
   pfwd=get_Fstr("w",ios::out);
}

void Docum::gopen_append(long n,int rfil) {
   ifstream *pfin;

   pfin=get_Istr("n");
   *pfin >> ndoc;
   pfin->close();
   gct = ndoc;
   cflag=rfil;

   pfwd=get_Fstr("w",ios::app|ios::ate);

   addr=new long[ndoc+n];
   pfin=get_Istr("a");
   pfin->read((char*)addr,sizeof(long)*ndoc);
   pfin->close();

   alen=new long[ndoc+n];
   pfin=get_Istr("m");
   pfin->read((char*)alen,sizeof(long)*ndoc);
   pfin->close();

   if(Gcom(WRITE_U)){
      auid=new long[ndoc+n];
      pfin=get_Istr("ub");
      pfin->read((char*)auid,sizeof(long)*ndoc);
      pfin->close();
   }

   if(Gcom(WRITE_D)){
      date=new long[ndoc+n];
      pfin=get_Istr("d");
      pfin->read((char*)date,sizeof(long)*ndoc);
      pfin->close();
   }

   if(Gcom(WRITE_S)){
      size=new long[ndoc+n];
      pfin=get_Istr("s");
      pfin->read((char*)size,sizeof(long)*ndoc);
      pfin->close();
   }
}   

void Docum::write(void){
   *(addr+gct)=pfwd->tellp();
   *(alen+gct)=len;

   *pfwd << endl;
   *pfwd << nw << " " << len << endl;
   for(ct=0;ct<nw;ct++)*pfwd << word[ct] << "|" << lcnt[ct] << endl;

   gct++;
}

void Docum::gclose_write(void){
   ofstream *pfout;

   ndoc=gct;
   pfout=get_Ostr("n"); 
   *pfout << ndoc << endl;
   pfout->close();

   dst_Fstr(pfwd);

   bin_Writ("a",ndoc*sizeof(long),(char*)addr);
   bin_Writ("m",ndoc*sizeof(long),(char*)alen);
   if(Rcom(WRITE_U))bin_Writ("ub",ndoc*sizeof(long),(char*)auid);
   if(Rcom(WRITE_D))bin_Writ("d",ndoc*sizeof(long),(char*)date);
   if(Rcom(WRITE_S))bin_Writ("s",ndoc*sizeof(long),(char*)size);
}

void Docum::open(void){
   int k;
   Node *pnod;
   btr=new Btree;

   for(long i=0;i<nw;i++){
      if(btr->search(word[i])){
         cout << "Error, list not unique!" << endl;
         exit(0);
      }
      else {
         hld[i]=lcnt[i];
         pnod = new Node(word[i],(void *)(hld+i));
         btr->insert(pnod);
         delete [] word[i];
      }
   }
}

void Docum::add(char *str,int n){
   int *pint;
   Node *pnod;

   if(btr->search(str)){
      pint = (int *)btr->give_ptr();
      (*pint)+=n;
   }
   else {
      hld[nw]=n;
      pnod = new Node(str,(void *)(hld+(nw++)));
      btr->insert(pnod);
   }
}

void Docum::close(void){
   long i=0;
   int j,lxn;
   char *pch;

   len=0;
   btr->node_first();
   while(btr->node_next()){
      j=*((int *)btr->give_ptr());
      lcnt[i]=j;
      pch=btr->show_str();
      lxn=strlen(pch);
      if(*(pch+lxn-2)=='!'){
         if(*(pch+lxn-1)=='t')len+=j;
      }
      else if(!strchr(pch,'*'))len+=j;
      word[i]=new char[lxn+1];
      strcpy(word[i],pch);
      i++;
   }
   delete btr;
}

void Docum::clear(void){
   for(int i=0;i<nw;i++){
      delete [] word[i];
   }
   ct=nw=len=0;
}

void Docum::copy(Docum *pDoc){
   int i;
   len=pDoc->len;
   nw=pDoc->nw;
   for(i=0;i<nw;i++){
      word[i]=pDoc->word[i];
      lcnt[i]=pDoc->lcnt[i];
   }
   pDoc->len=0;
   pDoc->nw=0;
   pDoc->ct=0;
   ct=0;
   if(oflag&WRITE_U)*(auid+gct)=*(pDoc->auid+pDoc->gct);
   if(oflag&WRITE_D)*(date+gct)=*(pDoc->date+pDoc->gct);
   if(oflag&WRITE_S)*(size+gct)=*(pDoc->size+pDoc->gct);
}

void Docum::copy(Count &Ct){
   long i;
   ct=0;
   len=0;
   nw=Ct.cnt_key;
   Ct.node_first();
   i=0;
   while(Ct.node_next()){
      len+=lcnt[i]=Ct.count();
      word[i]=new char[strlen(Ct.show_str())+1];
      strcpy(word[i],Ct.show_str());
      i++;
   }
}

void Docum::fill_grm(const char *txt,long ln,int gram_size,int aug,Word &Wrd){ 
   long j,k,n,lxn;
   int *pint,gp,grmm2;
   char ch,str[256],*pch,brd[256],frs[3];

   grmm2=gram_size-2;
   brd[0]='\0';
   brd[1]=' ';
   frs[1]='$';
   frs[2]='\0';
   n=0;
   j=0;
   Wrd.convert(txt,ln);
   while(lxn=Wrd.wordf(j,ln)){
         k=0;
         gp=(lxn<grmm2)?lxn:grmm2;
         while(k<gp){brd[2+k]=Wrd.wrd[k];k++;}
         brd[k+2]='\0';
         if(brd[0]!='\0')this->add(brd,1);

         frs[0]=brd[0]=Wrd.wrd[0];
         if(lxn>gram_size){
            //Handle first character
            this->add(frs,1);
            //Define first ngram
            ch=Wrd.wrd[gram_size];
            Wrd.wrd[gram_size]='\0';
            //Enter the first ngram into the tree
            strcpy(str,Wrd.wrd);
            this->add(str,1);
            //Produce the marked ngram and enter into tree
            strcat(str,"!");
            this->add(str,aug);
            Wrd.wrd[gram_size]=ch;
            //Enter the remainder of the ngrams into tree
            for(k=1;k<lxn-gram_size+1;k++){
               ch=Wrd.wrd[gram_size+k];
               Wrd.wrd[gram_size+k]='\0';
               strcpy(str,&Wrd.wrd[k]);
               this->add(str,1);
               Wrd.wrd[gram_size+k]=ch;
            }
         }
         else {
            //Handle first character
            this->add(frs,1);
            //Enter the first ngram into tree
            strcpy(str,Wrd.wrd);
            this->add(str,1);
            //Mark the ngram as first and enter into tree
            strcat(str,"!");
            this->add(str,aug);
         }
   }
}

void Docum::fill_brm(const char *txt,long ln,int lim_gram_size,Word &Wrd){
   long i,j,k,n,lxn;
   int *pint,gp,grmm2;
   char *chd,*chz,*czz,*pch,cho;
   char *zlt;

   Wrd.convert(txt,ln);
   zlt=Wrd.acc_conv();
   pch=new char[ln+1];
   j=0;
   for(i=0;i<ln;i++){
      if(gp=zlt[i])pch[j++]=gp;
   }
   chd=pch;
   if(j>1)chz=pch+2;
   for(i=1;i<j;i++){
      czz=chd;
      cho=*chz;
      *chz='\0';
      while((czz+1)!=chz){
         this->add(czz,1);
         czz++;
      }
      *chz=cho;
      chz++;
      if(i+2>lim_gram_size)chd++;
   }
   delete [] pch;
}

void Docum::fill_scn(const char *txt,long ln,int lim_gram_size,Word &Wrd){
   long i,j,lxn;
   char *chd;

   j=0;
   Wrd.convert(txt,ln);
   while(lxn=Wrd.wordf(j,ln)){
      if(lxn>=3){
         chd=new char[lxn+1];
         strcpy(chd,Wrd.wrd);
         i=3;
         while(i<=lxn){
            chd[i]='\0';
            add(chd+i-3,1);
            chd[i]=Wrd.wrd[i];
            i++;
         }
      }
   }
}

void Docum::debug(void){
   cout << nw << " " << len << endl;
   for(ct=0;ct<nw;ct++)cout << word[ct] << "|" << lcnt[ct] << endl;
}

}
