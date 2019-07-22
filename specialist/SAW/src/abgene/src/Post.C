#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include "Btree.h"
#include "runn.h"
#include "Docum.h"
#include "Post.h"
#include "Word.h"
#include "Bnum.h"
using namespace std;
namespace iret {

Post::Post(){
   btr=new Btree;
   ctt = NULL;
   freq=NULL;
   addr=NULL;
   pfln=NULL;
   idx=NULL;
   lct=NULL;
   pBn=NULL;
}

Post::Post(const char *nam){
   int len=strlen(nam);
   name=new char[len+1];
   strcpy(name,nam);

   btr=new Btree;
   ctt = NULL;
   freq=NULL;
   addr=NULL;
   pfln=NULL;
   idx=NULL;
   lct=NULL;
   pBn=NULL;
   ndoc =0;
   nwrd =0;    //initialized here
}

Post::~Post(){
   Pdat *ppp,*qqq,*zzz;

   delete [] name;

   if(btr!=NULL){
      btr->node_first();
      while(btr->node_next()){
         qqq=ppp=(Pdat *)(btr->give_ptr());
	 if(qqq !=NULL)  //will have trouble when Post has skeleton Btree.
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
   if (ctt!=NULL) delete ctt;
   if (freq!=NULL)delete [] freq;
   if (addr!=NULL)delete [] addr;
   if (pfln!=NULL)delete [] pfln;
   if (idx!=NULL)delete [] idx;
   if (lct!=NULL)delete [] lct;
   if (pBn!=NULL)delete pBn;
}

void Post::change_name(const char *nam){
   int len=strlen(nam);
   name=new char[len+1];
   strcpy(name,nam);
}

void Post::tread(ifstream &fin,long n, pPdat &dat){
   Pdat *ppp,*qqq;

   ppp=new Pdat;
   fin.read((char *)ppp,sizeof(long)+1);
   ppp->pdt=ppp;
   qqq=ppp;
   for(long i=1;i<n;i++){
      ppp=new Pdat;
      fin.read((char *)ppp,sizeof(long)+1);
      ppp->pdt=qqq->pdt;
      qqq->pdt=ppp;
      qqq=ppp;
   }
   dat=qqq;
}   

long Post::twrite(ofstream &fout,Pdat *dat){
   Pdat *ppp,*qqq;
   long pt=0;
   
   ppp=dat;
   qqq=ppp;
   do {
      qqq=qqq->pdt;
      fout.write((char *)qqq,sizeof(long)+1);
      pt++;
   } while(qqq!=ppp);
   return(pt);
}

void Post::add(char *str,long n,int j){
   Pdat *ppp,*qqq;
   Node *npt;
   ppp=new Pdat;
   ppp->num=n;
   ppp->lnt=(unsigned char)j;

   if(btr->search(str)){
      qqq=(Pdat*)btr->give_ptr();
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

Pdat *Post::data(char *str){
   if(btr->search(str)){
     return((Pdat *)btr->give_ptr());
   } 
   else return(NULL);
}

long Post::data_disk_slice(char *str) {
   long ix,nc,fc;

   ix=ctt->count(str);
   if(!ix)return(0);
   else {
      ix--;
      nc=pBn->index(ix);
      fc=*(freq+ix);
      pfln[nc].seekg(*(addr+ix), ios::beg);
      for(long i=0;i<fc;i++){
         pfln[nc].read((char*)(idx+i),sizeof(long));
         pfln[nc].read((char*)(lct+i),sizeof(char));
      }
      return(fc);
   }
}

long Post::data_disk_slice(long ix) {
   long nc,fc;

   nc=pBn->index(ix);
   fc=*(freq+ix);
   pfln[nc].seekg(*(addr+ix), ios::beg);
   for(long i=0;i<fc;i++){
      pfln[nc].read((char*)(idx+i),sizeof(long));
      pfln[nc].read((char*)(lct+i),sizeof(char));
   }
   return(fc);
}

long Post::data_small_slice(char *str) {
   long ix,nc,fc;

   ix=lst->find(str);
   if(!ix)return(0);
   else {
      ix--;
      nc=pBn->index(ix);
      fc=*(freq+ix);
      pfln[nc].seekg(*(addr+ix), ios::beg);
      for(long i=0;i<fc;i++){
         pfln[nc].read((char*)(idx+i),sizeof(long));
         pfln[nc].read((char*)(lct+i),sizeof(char));
      }
      return(fc);
   }
}

long Post::data_disk_merge(char *str) {
   long ix,nc=0,fc;

   ix=ctt->count(str);
   if(!ix)return(0);
   else {
      ix--;
      fc=*(freq+ix);
      pfln[nc].seekg(*(addr+ix), ios::beg);
      for(long i=0;i<fc;i++){
         pfln[nc].read((char*)(idx+i),sizeof(long));
         pfln[nc].read((char*)(lct+i),sizeof(char));
      }
      return(fc);
   }
}

long Post::data_disk_merge(long ix) {
   long nc=0,fc;

   fc=*(freq+ix);
   pfln[nc].seekg(*(addr+ix), ios::beg);
   for(long i=0;i<fc;i++){
      pfln[nc].read((char*)(idx+i),sizeof(long));
      pfln[nc].read((char*)(lct+i),sizeof(char));
   }
   return(fc);
}

long Post::count(Pdat *dat){
   Pdat *ppp,*qqq;
   long pt=1;
   
   ppp=dat;
   qqq=ppp->pdt;
   while(qqq!=ppp){
      qqq=qqq->pdt;
      pt++;
   }
   return(pt);
}

void Post::readp(){
   int pflag=get_qflag();
   long frq,i;
   char cnam[max_str];
   Node *npt;
   Pdat *dat;
   
   get_pathw(cnam,"postset",name,"d");
   ifstream fin(cnam,ios::in);
   fin >> ndoc;
   fin.close();

   get_pathw(cnam,"postset",name,"n");
   fin.open(cnam,ios::in);
   fin >> nwrd;
   fin.close();

   get_pathw(cnam,"postset",name,"f");
   ifstream fiq(cnam,ios::in|ios::binary);

   get_pathw(cnam,"postset",name,"s");
   ifstream fis(cnam,ios::in);
   get_pathw(cnam,"postset",name,"p");
   fin.open(cnam,ios::in|ios::binary);

   for(i=0;i<nwrd;i++){
      get_string(cnam,fis,'\n');
      fiq.read((char*)&frq,sizeof(long));
      tread(fin,frq,dat);
      if(btr->search(cnam)){
         cout << "Repeat in term list at " << i << endl;
         exit(0);
      }
      else {
         npt=new Node(cnam,dat);
         btr->insert(npt);
      }
      mark(pflag,i+1,1000,"postings in");
   }
   fin.close();
   fis.close();
   fiq.close();
}

void  Post::writep(){
   int pflag=get_qflag();
   long frq,ct,addr_off;
   char cnam[max_str];
   
   get_pathw(cnam,"postset",name,"d");
   ofstream fout(cnam,ios::out);
   fout << ndoc << endl;
   fout.close();

   get_pathw(cnam,"postset",name,"n");
   fout.open(cnam,ios::out);
   fout << nwrd << endl;
   fout.close();

   get_pathw(cnam,"postset",name,"s");
   ofstream fis(cnam,ios::out);
   get_pathw(cnam,"postset",name,"p");
   fout.open(cnam,ios::out|ios::binary);

   get_pathw(cnam,"postset",name,"a");
   ofstream faddr(cnam,ios::out|ios::binary);
   get_pathw(cnam,"postset",name,"f");
   ofstream fqout(cnam,ios::out|ios::binary);

   get_pathw(cnam,"postset",name,"fa");
   ofstream fqask(cnam,ios::out);

   ct=0;
   btr->node_first();
   while(btr->node_next()){
      fis << btr->show_str() << endl;
      addr_off = fout.tellp();
      faddr.write((char*)&addr_off, sizeof(long));
      frq=twrite(fout,(Pdat *)(btr->give_ptr()));
      fqout.write((char*)&frq, sizeof(long));
      fqask << frq << endl;
      mark(pflag,++ct,100,"postings out");
   }
   fis.close();
   fout.close();
   faddr.close();
   fqout.close();
}

void Post::load(Docum &Dcm){
   int j,pflag=get_qflag();
   char *spr;
   double xlen,zx;
   Dcm.gopen_read(READ_W);
   ndoc=Dcm.ndoc;
   nwrd=0;

   for(long i=0;i<ndoc;i++){
      Dcm.clear();
      Dcm.read();
      while((spr=Dcm.show(j))!=NULL){
         if(j>255)j=255;
         this->add(spr,i,j);
      }
      mark(pflag,i+1,100,"documents loaded");
   }
   Dcm.gclose_read(READ_W);
}

void Post::seg_load(Docum &Dcm, long start, long end, long start_load)
{
  int j,pflag=get_qflag(), cnt=0;
  char *spr,cnam[max_str];
  double xlen,zx;
  Dcm.gopen_read(READ_W);
  end =(end>Dcm.ndoc)? Dcm.ndoc:end;
  ndoc = ndoc+(end -start);
  cnt = ndoc;
  for(long i=start;i<end;i++){
    Dcm.clear();
    Dcm.read(i);
    ++cnt; 
    while((spr=Dcm.show(j))!=NULL){
       if(j>255)j=255;
       this->add(spr,i+start_load,j);
    }
    mark(pflag,i+1,100,"documents loaded");
  }
  if (cnt !=ndoc) cout<<"documents loaded not equal to ndoc\n";
  Dcm.gclose_read(READ_W);
}

void Post::seg_load(Regist &Reg,long start_load,long end)
{
  int j,pflag=get_qflag(), cnt=0;
  char *spr,cnam[max_str];
  double xlen,zx;
  Docum *pDoc; 

  if (start_load >= end) {
	cout<<"the number entered is too big\n"; exit(0);
  }
  for(long i=start_load;i<end;i++){
    pDoc = Reg.read(i);
    ++cnt;
    while((spr=pDoc->show(j))!=NULL){
       if(j>255)j=255;
       this->add(spr,i,j);
    }
    mark(pflag,i+1,100,"documents loaded");
  }
}

void Post::gopen_disk_slice(long n) {
   int pflag=get_qflag();
   long i,j,k,ix,ax[2];
   char cnam[256],ctstring[25];
   ctt = new Count;

   get_pathw(cnam,"slice",name,"d");
   ifstream fin(cnam,ios::in);
   fin >> ndoc;
   fin.close();
   get_pathw(cnam,"slice",name,"n");
   fin.open(cnam,ios::in);
   fin >> nwrd;
   fin.close();

   swrd=0;
   get_pathw(cnam,"slice",name,"f");
   fin.open(cnam,ios::in|ios::binary);
   if (!fin) {cout<<"cant' open "<<cnam<<endl; exit(0);}
   for(i=0;i<nwrd;i++){
      fin.read((char*)&ix,sizeof(long));
      if(ix>=n)swrd++;
   }
   fin.close();
   if(pflag)cout << "number of selected terms = " << swrd << endl;

   freq=new long[swrd];
   addr=new long[swrd];

   get_pathw(cnam,"slice",name,"z");
   fin.open(cnam,ios::in);
   while(fin >> ix >> j);
   cfln=ix+1;
   fin.close();
   pBn=new Bnum(cfln+1);
   for(i=0;i<cfln+1;i++)pBn->mm[i]=0;

   get_pathw(cnam,"slice",name,"f");
   fin.open(cnam,ios::in|ios::binary);
   get_pathw(cnam,"slice",name,"a");
   ifstream fia(cnam,ios::in|ios::binary);
   get_pathw(cnam,"slice",name,"s");
   ifstream fis(cnam,ios::in);
   j=0;
   for (i=0;i<nwrd;i++){
      get_string(cnam,fis,'\n');
      fin.read((char*)&ix,sizeof(long));
      fia.read((char*)ax,2*sizeof(long));
      if (ix >=n) {
         *(freq+j)=ix;
         (pBn->mm[ax[0]+1])++;
         *(addr+j)=ax[1];
         ctt->add_count(cnam,++j);
      }
      mark(pflag,i+1,10000,"postings gopen");
   }
   fis.close();
   fia.close();
   fin.close();
   for(i=1;i<cfln+1;i++)(pBn->mm[i])+=pBn->mm[i-1];
 
   idx=new long[ndoc];
   lct=new unsigned char[ndoc];

   pfln=new ifstream[cfln];
   for (i=0; i<cfln; i++) {
      get_pathw(cnam,"slice",name,"p");
      long_str(ctstring, i);
      strcat(cnam,ctstring);
      pfln[i].open(cnam,ios::in|ios::binary);
   }
}

void Post::gopen_small_slice(long n) {
   int pflag=get_qflag();
   long i,j,k,ix,ax[2],len;
   char cnam[256],ctstring[25];

   get_pathw(cnam,"slice",name,"d");
   ifstream fin(cnam,ios::in);
   fin >> ndoc;
   fin.close();
   get_pathw(cnam,"slice",name,"n");
   fin.open(cnam,ios::in);
   fin >> nwrd;
   fin.close();

   swrd=0;
   get_pathw(cnam,"slice",name,"f");
   fin.open(cnam,ios::in|ios::binary);
   if (!fin) {cout<<"cant' open "<<cnam<<endl; exit(0);}
   for(i=0;i<nwrd;i++){
      fin.read((char*)&ix,sizeof(long));
      if(ix>=n)swrd++;
   }
   fin.close();
   if(pflag)cout << "number of selected terms = " << swrd << endl;

   lst = new Blist("local");
   lst->size(swrd);

   freq=new long[swrd];
   addr=new long[swrd];

   get_pathw(cnam,"slice",name,"z");
   fin.open(cnam,ios::in);
   while(fin >> ix >> j);
   cfln=ix+1;
   fin.close();
   pBn=new Bnum(cfln+1);
   for(i=0;i<cfln+1;i++)pBn->mm[i]=0;

   get_pathw(cnam,"slice",name,"f");
   fin.open(cnam,ios::in|ios::binary);
   get_pathw(cnam,"slice",name,"a");
   ifstream fia(cnam,ios::in|ios::binary);
   get_pathw(cnam,"slice",name,"s");
   ifstream fis(cnam,ios::in);
   j=0;
   for (i=0;i<nwrd;i++){
      len=get_string(cnam,fis,'\n');
      fin.read((char*)&ix,sizeof(long));
      fia.read((char*)ax,2*sizeof(long));
      if (ix >=n) {
         *(freq+j)=ix;
         (pBn->mm[ax[0]+1])++;
         *(addr+j)=ax[1];
         lst->add_string(len,cnam);
         j++;
      }
      mark(pflag,i+1,10000,"postings gopen");
   }
   fis.close();
   fia.close();
   fin.close();
   for(i=1;i<cfln+1;i++)(pBn->mm[i])+=pBn->mm[i-1];
 
   idx=new long[ndoc];
   lct=new unsigned char[ndoc];

   pfln=new ifstream[cfln];
   for (i=0; i<cfln; i++) {
      get_pathw(cnam,"slice",name,"p");
      long_str(ctstring, i);
      strcat(cnam,ctstring);
      pfln[i].open(cnam,ios::in|ios::binary);
   }
}

void Post::gopen_index(void) {
   int pflag=get_qflag();
   long i,j,k,ix,ax[2],len;
   char cnam[256],ctstring[25];

   get_pathw(cnam,"slice",name,"d");
   ifstream fin(cnam,ios::in);
   fin >> ndoc;
   fin.close();
   get_pathw(cnam,"slice",name,"n");
   fin.open(cnam,ios::in);
   fin >> nwrd;
   fin.close();

   swrd=nwrd;
   freq=new long[nwrd];
   addr=new long[nwrd];

   get_pathw(cnam,"slice",name,"z");
   fin.open(cnam,ios::in);
   while(fin >> ix >> j);
   cfln=ix+1;
   fin.close();
   pBn=new Bnum(cfln+1);
   for(i=0;i<cfln+1;i++)pBn->mm[i]=0;

   get_pathw(cnam,"slice",name,"f");
   fin.open(cnam,ios::in|ios::binary);
   get_pathw(cnam,"slice",name,"a");
   ifstream fia(cnam,ios::in|ios::binary);
   for (i=0;i<nwrd;i++){
      fin.read((char*)&ix,sizeof(long));
      fia.read((char*)ax,2*sizeof(long));
      *(freq+i)=ix;
      (pBn->mm[ax[0]+1])++;
      *(addr+i)=ax[1];
      mark(pflag,i+1,10000,"postings gopen");
   }
   fia.close();
   fin.close();
   for(i=1;i<cfln+1;i++)(pBn->mm[i])+=pBn->mm[i-1];
 
   idx=new long[ndoc];
   lct=new unsigned char[ndoc];

   pfln=new ifstream[cfln];
   for (i=0; i<cfln; i++) {
      get_pathw(cnam,"slice",name,"p");
      long_str(ctstring, i);
      strcat(cnam,ctstring);
      pfln[i].open(cnam,ios::in|ios::binary);
   }
}

void Post::gopen_disk_merge(long n) {
   int pflag=get_qflag();
   long i,j,k,ix,ax;
   char cnam[256];
   ctt = new Count;

   get_pathw(cnam,"merge",name,"d");
   ifstream fin(cnam,ios::in);
   fin >> ndoc;
   fin.close();
   get_pathw(cnam,"merge",name,"n");
   fin.open(cnam,ios::in);
   fin >> nwrd;
   fin.close();

   swrd=0;
   get_pathw(cnam,"merge",name,"f");
   fin.open(cnam,ios::in|ios::binary);
   if (!fin) {cout<<"cant' open "<<cnam<<endl; exit(0);}
   for(i=0;i<nwrd;i++){
      fin.read((char*)&ix,sizeof(long));
      if(ix>=n)swrd++;
   }
   fin.close();
   if(pflag)cout << "number of selected terms = " << swrd << endl;

   freq=new long[swrd];
   addr=new long[swrd];

   get_pathw(cnam,"merge",name,"f");
   fin.open(cnam,ios::in|ios::binary);
   get_pathw(cnam,"merge",name,"a");
   ifstream fia(cnam,ios::in|ios::binary);
   get_pathw(cnam,"merge",name,"s");
   ifstream fis(cnam,ios::in);
   j=0;
   for (i=0;i<nwrd;i++){
      get_string(cnam,fis,'\n');
      fin.read((char*)&ix,sizeof(long));
      fia.read((char*)&ax,sizeof(long));
      if (ix >=n) {
         *(freq+j)=ix;
         *(addr+j)=ax;
         ctt->add_count(cnam,++j);
      }
      mark(pflag,i+1,1000,"postings gopen");
   }
   fis.close();
   fia.close();
   fin.close();
 
   idx=new long[ndoc];
   lct=new unsigned char[ndoc];

   cfln=1;
   pfln=new ifstream[cfln];
   get_pathw(cnam,"merge",name,"p");
   pfln[0].open(cnam,ios::in|ios::binary);
}

void Post::segs_on_disk(Docum &Doc)
{
  ifstream fin;
  ofstream fout; 
  char cnam[max_str], bnam[max_str];
  int pflag=get_qflag();
  long size_of_seg,total_num,cnt,start,end,start_load;

  get_pathw(cnam,"docset",name,"n");
  fin.open(cnam, ios::in);
  if (!fin)
    {cout<<"No docset "<<cnam<<" \n"; exit(0);}
  fin>>total_num;
  fin.close();

  cout << "enter number of documents to be processed in a set:\n";
  cin >>size_of_seg;
  cin.get();

  char* file_nam = new char[max_str];  
  cnt = 0;
  start = 0;
  end =size_of_seg;
  start_load = 0;
  
  while (total_num > 0)
    {
        long_str(file_nam, cnt);
        Post Pst(file_nam);
	Pst.seg_load(Doc, start, end, start_load);
        Pst.writep();
	start += size_of_seg;
	end += size_of_seg;
	total_num -= size_of_seg;
	cnt++;
    }
  cout << cnt <<" sets of post files were created. \n";

  get_pathw(cnam,"postset",name,"#");
  fout.open(cnam, ios::out);
  fout<<size_of_seg<<"	the #of docu in one piece\n";
  fout<<(cnt*size_of_seg+total_num)<<"	the #of total docu\n";
  fout<<cnt<<"	the #of pieces\n";
  fout<<size_of_seg+total_num<<"	the #of docu in the last\n";
  fout.close();
  delete [] file_nam;
}
 
void Post::make_single_seg_extend(Regist &Reg)
{
  ifstream fin;
  ofstream fout;
  char cnam[max_str], file_nam[max_str];
  long size_of_seg,start_load,num_of_segs,last_size;
  long start, end, cnt, docu_num, total_num;
  Docum *pDoc;

  get_pathw(cnam,"postset",name,"#");
  fin.open(cnam, ios::in);
  fin>>size_of_seg; fin.getline(file_nam,max_str);
  fin>>start_load;  fin.getline(file_nam,max_str);
  fin>>num_of_segs; fin.getline(file_nam,max_str);
  fin>>last_size;   fin.getline(file_nam,max_str);

  if (start_load == Reg.ntot) {
    cout<<"All postsegs are made!\n";
    exit(0);
  }

  if (last_size == size_of_seg) {
    cnt = num_of_segs;
    long_str(file_nam, cnt);
    Post Pst(file_nam);
    end = start_load+size_of_seg;
    end = (Reg.ntot<end)? Reg.ntot:end;
    Pst.seg_load(Reg,start_load,end);
    Pst.ndoc = end-start_load;
    Pst.writep(); 
  }
  else {
    cnt = num_of_segs-1;
    long_str(file_nam, cnt);
    Post Pst(file_nam);
     end = start_load+size_of_seg-last_size;
     end = (Reg.ntot<end)? Reg.ntot:end;
     Pst.readp();
     Pst.seg_load(Reg, start_load, end);
     Pst.ndoc = end-start_load+last_size;
     Pst.writep();
  }

  get_pathw(cnam,"postset",name,"#");
  fout.open(cnam,ios::out);
  fout<<size_of_seg<<"  the #of docu in one piece\n";
  fout<<end<<"  the #of total docu\n";
  fout<<cnt+1<<"  the #of pieces\n";
  fout<<end-cnt*size_of_seg<<"     the #of docu in the last\n";
}

void Post::make_single_seg_first(Regist &Reg)
{
  ifstream fin;
  ofstream fout;
  char cnam[max_str], file_nam[max_str];
  long size_of_seg,start_load,num_of_segs,last_size;
  long start, end, cnt, docu_num, total_num;
  Docum *pDoc;

  cout << "enter number of documents to be processed in a set:\n";
  cin >>size_of_seg;
  cin.get();
  start_load=0;
  cnt = 0;

  long_str(file_nam, cnt);
  Post Pst(file_nam);
  end = start_load+size_of_seg;
  end = (Reg.ntot<end)? Reg.ntot:end;
  Pst.seg_load(Reg,start_load,end);
  Pst.ndoc = end-start_load;
  Pst.writep();
  cnt++;

  get_pathw(cnam,"postset",name,"#");
  fout.open(cnam,ios::out);
  fout<<size_of_seg<<"  the #of docu in one piece\n";
  fout<<end<<"  the #of total docu\n";
  fout<<cnt<<"  the #of pieces\n";
  fout<<end<<"     the #of docu in the last\n";
}

void Post::make_single_seg_first(Regist &Reg,long sz_seg)
{
  ifstream fin;
  ofstream fout;
  char cnam[max_str], file_nam[max_str];
  long size_of_seg,start_load,num_of_segs,last_size;
  long start, end, cnt, docu_num, total_num;
  Docum *pDoc;

  size_of_seg=sz_seg;
  start_load=0;
  cnt = 0;

  long_str(file_nam, cnt);
  Post Pst(file_nam);
  end = start_load+size_of_seg;
  end = (Reg.ntot<end)? Reg.ntot:end;
  Pst.seg_load(Reg,start_load,end);
  Pst.ndoc = end-start_load;
  Pst.writep();
  cnt++;

  get_pathw(cnam,"postset",name,"#");
  fout.open(cnam,ios::out);
  fout<<size_of_seg<<"  the #of docu in one piece\n";
  fout<<end<<"  the #of total docu\n";
  fout<<cnt<<"  the #of pieces\n";
  fout<<end<<"     the #of docu in the last\n";
}

void Post::make_single_seg_overwrite(Regist &Reg, int seg_num)
{
  ifstream fin;
  ofstream fout,outf;
  char cnam[max_str], file_nam[max_str];
  long size_of_seg,start_load,num_of_segs,last_size;
  long start, end, cnt, docu_num, total_num;
  Docum *pDoc;

  get_pathw(cnam,"postset",name,"#");
  fin.open(cnam, ios::in);
  fin>>size_of_seg; fin.getline(file_nam,max_str);
  fin>>start_load;  fin.getline(file_nam,max_str);
  fin>>num_of_segs; fin.getline(file_nam,max_str);
  fin>>last_size;   fin.getline(file_nam,max_str);

  if (seg_num > (num_of_segs-1)) {
    cout<<"There's no such seg, the num is too big\n";
    exit(0);
  }

  if (start_load == Reg.ntot) {
    cout<<"All postsegs are made!\n";
    exit(0);
  }

  cnt = seg_num;
  start_load = seg_num*size_of_seg;

  long_str(file_nam, cnt);
  Post Pst(file_nam);
  end = start_load+size_of_seg;
  end = (Reg.ntot<end)? Reg.ntot:end;
  Pst.seg_load(Reg,start_load,end);
  Pst.ndoc = end-start_load;
  Pst.writep();

  get_pathw(cnam,"postset",name,"#");
  fout.open(cnam,ios::out);
  fout<<size_of_seg<<"  the #of docu in one piece\n";
  fout<<end<<"  the #of total docu\n";
  fout<<seg_num+1<<"  the #of pieces\n";
  fout<<end-cnt*size_of_seg<<"	the #of docu in the last\n";
  fout.close();
}

void Post::p_disk_slice(long disk_size) {
  int pflag=get_qflag(), size_of_seg;
  long ct,filect=0,addr_off,file_size=0;
  char cnam[max_str],ctstring[25];

  long cnt, i, j;
  ifstream fin, *s_infile, *p_infile, *f_infile;
  ofstream fout;

  get_pathw(cnam,"postset",name,"#");
  fin.open(cnam, ios::in);
  if (!fin) cout<<"no file postset_ .#\n";
  fin>>size_of_seg; fin.getline(cnam, max_str);
  fin>>cnt; fin.getline(cnam, max_str);
  fin>>cnt; fin.getline(cnam, max_str);  // get the # of piceces
  fin.close();

  char **term = new char *[cnt];
  char **file_nam = new char*[cnt];
  ofstream ffout;  // for Pos "f" file;
  ofstream pfout;  // for Pos "p" file;
  ofstream afout;  // for Pos "a" file;
  ofstream zfout;

  char phra[max_str];
  Pdat* ppp;
  long *fq=new long[cnt], frq =0;

  get_pathw(cnam,"slice",name,"f");
  ffout.open(cnam, ios::out|ios::binary);    //remember to close at the end.

  get_pathw(cnam,"slice",name,"z");
  zfout.open(cnam, ios::out);    //remember to close at the end.

  get_pathw(cnam,"slice",name,"a");
  afout.open(cnam, ios::out|ios::binary);    //remember to close at the end.

  p_infile = new ifstream[cnt]; 
  f_infile = new ifstream[cnt];
  s_infile = new ifstream[cnt];

  //open all segs files and get first terms.
  for (i=0; i<cnt; i++) {
    file_nam[i] = new char[max_str];
    long_str(file_nam[i], i);
    term[i] = new char[max_str];

    get_pathw(cnam,"postset",file_nam[i],"s");
    s_infile[i].open(cnam, ios::in);
    get_pathw(cnam,"postset",file_nam[i],"p");
    p_infile[i].open(cnam, ios::in|ios::binary);
    get_pathw(cnam,"postset",file_nam[i],"f");
    f_infile[i].open(cnam, ios::in|ios::binary);

    get_string(term[i],s_infile[i],'\n');
  }

  long total_terms, c_terms=0, a_offset;
  ct = 0;
  file_size=0;
  filect=0;
  get_pathw(cnam,"slice",name,"p");
  long_str(ctstring, filect);
  strcat(cnam,ctstring);
  pfout.open(cnam,ios::out|ios::binary);

  ppp = new Pdat[size_of_seg];
  get_pathw(cnam,"slice",name,"n");
  fin.open(cnam,ios::in);
  fin>>total_terms;
  fin.close();
  get_pathw(cnam,"slice",name,"s");
  fin.open(cnam,ios::in);

  while(c_terms < total_terms) {
    c_terms++;
    get_string(phra,fin,'\n');
  
    a_offset = pfout.tellp();
    afout.write((char*)&filect,sizeof(long));
    afout.write((char*)&a_offset, sizeof(long));

    for (i=0; i<cnt; i++)  {
      if (strcmp(term[i], phra) ==0)  {
        get_string(term[i],s_infile[i],'\n'); //get next term
        f_infile[i].read((char*)&fq[i], sizeof(long));
        frq +=fq[i];
        p_infile[i].read((char*)ppp, (sizeof(long)+1)*fq[i]);
        pfout.write((char*)ppp, (sizeof(long)+1)*fq[i]);
      }
    }
    file_size += frq*(sizeof(long)+1);
    if (file_size > disk_size) {
      zfout<<filect<<"\t"<<file_size<<endl;
      filect++;
      file_size = 0;
      pfout.close();
      get_pathw(cnam,"slice",name,"p");
      long_str(ctstring, filect);
      strcat(cnam,ctstring);
      pfout.open(cnam,ios::out|ios::binary);
    }
    //write into Pos's "f" file.
    ffout.write((char*)&frq, sizeof(long));
    frq =0; //reset.
    mark(pflag,++ct,2000,"postings out........wait.....");
  }
  zfout<<filect<<"\t"<<file_size<<endl;
  ffout.close();
  pfout.close();
  afout.close();
  zfout.close();
  fin.close();

  //need to delete allocated term and file_nam.
  for (i=0; i<cnt; i++) {
    delete [] file_nam[i];
    delete [] term[i];
    s_infile[i].close();
    p_infile[i].close();
    f_infile[i].close();
  }
  delete [] term;
  delete [] file_nam;
  delete [] fq;
  delete [] ppp;
  delete [] s_infile;
  delete [] p_infile;
  delete [] f_infile;
}

void Post::p_disk_merge(void) {
  int pflag=get_qflag(), size_of_seg;
  long ct,addr_off,file_size=0;
  char cnam[max_str];

  long cnt, i, j;
  ifstream fin, *s_infile, *p_infile, *f_infile;
  ofstream fout;

  get_pathw(cnam,"postset",name,"#");
  fin.open(cnam, ios::in);
  if (!fin) cout<<"no file postset_ .#\n";
  fin>>size_of_seg; fin.getline(cnam, max_str);
  fin>>cnt; fin.getline(cnam, max_str);
  fin>>cnt; fin.getline(cnam, max_str);  // get the # of piceces
  fin.close();

  char **term = new char *[cnt];
  char **file_nam = new char*[cnt];
  ofstream ffout;  // for Pos "f" file;
  ofstream pfout;  // for Pos "p" file;
  ofstream afout;  // for Pos "a" file;

  char phra[max_str];
  long *fq=new long[cnt], frq =0;

  get_pathw(cnam,"merge",name,"f");
  ffout.open(cnam, ios::out|ios::binary);    //remember to close at the end.

  get_pathw(cnam,"merge",name,"a");
  afout.open(cnam, ios::out|ios::binary);    //remember to close at the end.

  get_pathw(cnam,"merge",name,"p");
  pfout.open(cnam,ios::out|ios::binary);     //remember to close at the end.

  p_infile = new ifstream[cnt]; 
  f_infile = new ifstream[cnt];
  s_infile = new ifstream[cnt];

  //open all segs files and get first terms.
  for (i=0; i<cnt; i++) {
    file_nam[i] = new char[max_str];
    long_str(file_nam[i], i);
    term[i] = new char[max_str];

    get_pathw(cnam,"postset",file_nam[i],"s");
    s_infile[i].open(cnam, ios::in);
    get_pathw(cnam,"postset",file_nam[i],"p");
    p_infile[i].open(cnam, ios::in|ios::binary);
    get_pathw(cnam,"postset",file_nam[i],"f");
    f_infile[i].open(cnam, ios::in|ios::binary);

    get_string(term[i],s_infile[i],'\n');
  }

  long total_terms, c_terms=0, a_offset;

  char *ppp = new char[size_of_seg*5];
  get_pathw(cnam,"merge",name,"n");
  fin.open(cnam,ios::in);
  fin>>total_terms;
  fin.close();
  get_pathw(cnam,"merge",name,"s");
  fin.open(cnam,ios::in);

  while(c_terms < total_terms) {
    c_terms++;
    get_string(phra,fin,'\n');
  
    a_offset = pfout.tellp();
    afout.write((char*)&a_offset, sizeof(long));

    for (i=0; i<cnt; i++)  {
      if (strcmp(term[i], phra) ==0)  {
        get_string(term[i],s_infile[i],'\n'); //get next term
        f_infile[i].read((char*)&fq[i], sizeof(long));
        frq +=fq[i];
        p_infile[i].read(ppp, 5*fq[i]);
        pfout.write(ppp, 5*fq[i]);
      }
    }
    //write into Pos's "f" file.
    ffout.write((char*)&frq, sizeof(long));
    frq =0; //reset.
    mark(pflag,c_terms,2000,"postings out........wait.....");
  }
  ffout.close();
  pfout.close();
  afout.close();
  fin.close();

  //need to delete allocated term and file_nam.
  for (i=0; i<cnt; i++) {
    delete [] file_nam[i];
    delete [] term[i];
    s_infile[i].close();
    p_infile[i].close();
    f_infile[i].close();
  }
  delete [] term;
  delete [] file_nam;
  delete [] fq;
  delete [] ppp;
  delete [] s_infile;
  delete [] p_infile;
  delete [] f_infile;
}

void Post::s_disk_slice(long list_size) {
  int pflag=get_qflag(), size_of_seg;
  long ct, cnt, i,j,ix,jx,finish_cnt, file_size=0;
  char cnam[max_str],ctstring[255],*phra,path[255];
  List *plst1, *plst2;
  ifstream fin, *infile;
  ofstream fout;
  
  get_pathw(cnam,"postset",name,"#");
  fin.open(cnam, ios::in);
  fin>>size_of_seg; fin.getline(cnam, max_str);
  fin>>ndoc; fin.getline(cnam, max_str); //get the total number of docs.
  fin>>cnt; fin.getline(cnam, max_str);  // get the # of piceces
  fin.close();

  get_pathw(cnam,"slice",name,"d");
  fout.open(cnam,ios::out);
  fout << ndoc << endl;
  fout.close();

  char **term = new char *[cnt];
  char **file_nam = new char*[cnt];
  infile = new ifstream[cnt];
  for (i=0; i<cnt; i++) {
        file_nam[i] = new char[max_str];
        long_str(file_nam[i], i);
        term[i] = new char[max_str];
      get_pathw(cnam,"postset",file_nam[i],"s");
      infile[i].open(cnam,ios::in);
  }

  ifstream nfin;   //for "n" input
  ofstream nfout;
  ofstream sfout;  //for Post "s" file;

  //keep track of location.
  long *n_term=new long[cnt];
  long *c_term=new long[cnt];
  long *g_term=new long[cnt],step=1000,xstep,k;
  char *pch;
  ifstream *sfin;

  //initialize
  long total_term=0;    // the total num of terms
  for (i=0; i<cnt; i++)
    {
      get_pathw(cnam,"postset",file_nam[i],"n");
      nfin.open(cnam, ios::in);
      nfin>>n_term[i];
      total_term += n_term[i];
      nfin.close();
      c_term[i]=0;
      g_term[i]=1;
    }
  ct = 0;
  plst1 = new List;
  get_pathw(cnam,"slice",name,"n");
  nfout.open(cnam, ios::out);
  long terms=0;
  get_pathw(cnam,"slice",name,"s");
  sfout.open(cnam,ios::out);

  next:

  while (plst1->cnt_key < list_size) {
    for (i=0; i<cnt; i++)
    { 
      if (g_term[i]) {
         k=n_term[i]-c_term[i];
         xstep=(step<k)?step:k;
         pch=term[i];
         sfin=&infile[i];
         for(k=0;k<xstep;k++){
            get_string(pch,*sfin,'\n');
            plst1->add_key_count(pch);
         }
         c_term[i]+=xstep;
         ct+=xstep;
         if (c_term[i] == n_term[i]) {
            delete [] term[i];
            term[i] = NULL;
            g_term[i]=0;
         } 
      }
    }
    if(pflag){cout << ct << " postings in........" << endl;}
    if (ct == total_term) {
        if(pflag)cout << "   *** writing on disk ***" << endl;
        plst1->node_first();
        while (plst1->node_next()) {
          phra = plst1->show_str();
          sfout<<phra<<endl;
          terms++;
        }
        nfout<<terms<<endl;
        nfout.close();
        sfout.close();
        for (i=0; i<cnt; i++) {
          delete [] file_nam[i];
          delete [] term[i];
          infile[i].close();
        }
        delete [] term;
        delete [] file_nam;
        delete [] infile;
        delete [] n_term;
        delete [] c_term;
        delete [] g_term;
        delete plst1;
        exit(0);
    }
    for (i=0;i<cnt;i++) {
       if (term[i] != NULL) {
         ix = jx = i;
         break;
       } 
    }
    for (i=jx+1;i<cnt;i++) {
       if ((term[i]!=NULL)&&(strcmp(term[jx],term[i]) < 0))jx = i;
    }
    g_term[jx]=0; //Turn off the highest.
    for (i=ix+1;i<cnt;i++) {
       if ((term[i]!=NULL)&&(strcmp(term[ix],term[i]) > 0))ix = i;
    }
    g_term[ix]=1; //Turn on the lowest.
  }

  if(pflag)cout << "   *** writing on disk ***" << endl;
  pch=term[ix];
  plst1->node_first();
  while (plst1->node_next()) {
    phra = plst1->show_str();
    if (strcmp(phra,pch) <= 0) {
      sfout<<phra<<endl;
      terms++;
    }
    else
      break;
  }
  plst2 = new List;
  plst2->add_key_count(phra);
  while (plst1->node_next()) {
    phra = plst1->show_str();
    plst2->add_key_count(phra);
  }
  delete plst1;
  plst1 = plst2;
  goto next;
}

void Post::s_disk_merge(long list_size) {
  int pflag=get_qflag(), size_of_seg;
  long ct, cnt, i,j,ix,jx,finish_cnt, file_size=0;
  char cnam[max_str],ctstring[255],*phra,path[255];
  List *plst1, *plst2;
  ifstream fin, *infile;
  ofstream fout;
  
  get_pathw(cnam,"postset",name,"#");
  fin.open(cnam, ios::in);
  fin>>size_of_seg; fin.getline(cnam, max_str);
  fin>>ndoc; fin.getline(cnam, max_str); //get the total number of docs.
  fin>>cnt; fin.getline(cnam, max_str);  // get the # of piceces
  fin.close();

  get_pathw(cnam,"merge",name,"d");
  fout.open(cnam,ios::out);
  fout << ndoc << endl;
  fout.close();

  char **term = new char *[cnt];
  char **file_nam = new char*[cnt];
  infile = new ifstream[cnt];
  for (i=0; i<cnt; i++) {
        file_nam[i] = new char[max_str];
        long_str(file_nam[i], i);
        term[i] = new char[max_str];
      get_pathw(cnam,"postset",file_nam[i],"s");
      infile[i].open(cnam,ios::in);
  }

  ifstream nfin;   //for "n" input
  ofstream nfout;
  ofstream sfout;  //for Post "s" file;

  //keep track of location.
  long *n_term=new long[cnt];
  long *c_term=new long[cnt];
  long *g_term=new long[cnt],step=1000,xstep,k;
  char *pch;
  ifstream *sfin;

  //initialize
  long total_term=0;    // the total num of terms
  for (i=0; i<cnt; i++)
    {
      get_pathw(cnam,"postset",file_nam[i],"n");
      nfin.open(cnam, ios::in);
      nfin>>n_term[i];
      total_term += n_term[i];
      nfin.close();
      c_term[i]=0;
      g_term[i]=1;
    }
  ct = 0;
  plst1 = new List;
  get_pathw(cnam,"merge",name,"n");
  nfout.open(cnam, ios::out);
  long terms=0;
  get_pathw(cnam,"merge",name,"s");
  sfout.open(cnam,ios::out);

  next:

  while (plst1->cnt_key < list_size) {
    for (i=0; i<cnt; i++)
    { 
      if (g_term[i]) {
         k=n_term[i]-c_term[i];
         xstep=(step<k)?step:k;
         pch=term[i];
         sfin=&infile[i];
         for(k=0;k<xstep;k++){
            get_string(pch,*sfin,'\n');
            plst1->add_key_count(pch);
         }
         c_term[i]+=xstep;
         ct+=xstep;
         if (c_term[i] == n_term[i]) {
            delete [] term[i];
            term[i] = NULL;
            g_term[i]=0;
         } 
      }
    }
    if(pflag){cout << ct << " postings in........" << endl;}
    if (ct == total_term) {
        if(pflag)cout << "   *** writing on disk ***" << endl;
        plst1->node_first();
        while (plst1->node_next()) {
          phra = plst1->show_str();
          sfout<<phra<<endl;
          terms++;
        }
        nfout<<terms<<endl;
        nfout.close();
        sfout.close();
        for (i=0; i<cnt; i++) {
          delete [] file_nam[i];
          delete [] term[i];
          infile[i].close();
        }
        delete [] term;
        delete [] file_nam;
        delete [] infile;
        delete [] n_term;
        delete [] c_term;
        delete [] g_term;
        delete plst1;
        exit(0);
    }
    for (i=0;i<cnt;i++) {
       if (term[i] != NULL) {
         ix = jx = i;
         break;
       } 
    }
    for (i=jx+1;i<cnt;i++) {
       if ((term[i]!=NULL)&&(strcmp(term[jx],term[i]) < 0))jx = i;
    }
    g_term[jx]=0; //Turn off the highest.
    for (i=ix+1;i<cnt;i++) {
       if ((term[i]!=NULL)&&(strcmp(term[ix],term[i]) > 0))ix = i;
    }
    g_term[ix]=1; //Turn on the lowest.
  }

  if(pflag)cout << "   *** writing on disk ***" << endl;
  pch=term[ix];
  plst1->node_first();
  while (plst1->node_next()) {
    phra = plst1->show_str();
    if (strcmp(phra,pch) <= 0) {
      sfout<<phra<<endl;
      terms++;
    }
    else
      break;
  }
  plst2 = new List;
  plst2->add_key_count(phra);
  while (plst1->node_next()) {
    phra = plst1->show_str();
    plst2->add_key_count(phra);
  }
  delete plst1;
  plst1 = plst2;
  goto next;
}

}
