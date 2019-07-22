#include <iostream>
#include <fstream>
#include <cstdlib>
#include <iomanip>
#include <cstring>
#include <cmath>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include "runn.h"
#include "FBase.h"

using namespace std;
namespace iret {

FBase::FBase(const char *typ,const char *nam){
   int lxn=strlen(typ);
   type=new char[lxn+1];
   strcpy(type,typ);
   lxn=strlen(nam);
   name=new char[lxn+1];
   strcpy(name,nam);
   cflag=0;
   oflag=0;
}

FBase::~FBase(void){
   if (type!=NULL) delete [] type;
   if (name!=NULL) delete [] name;
}

void FBase::change_type(const char *typ){
   if(type!=NULL)delete [] type;
   int lxn=strlen(typ);
   type=new char[lxn+1];
   strcpy(type,typ);
}

void FBase::change_name(const char *nam){
   if(name!=NULL)delete [] name;
   int lxn=strlen(nam);
   name=new char[lxn+1];
   strcpy(name,nam);
}

void FBase::set_name(const char *nam){
   if(name!=NULL)delete [] name;
   int lxn=strlen(nam);
   name=new char[lxn+1];
   strcpy(name,nam);
}

void FBase::get_pathx(char *nam,const char *ch){
   char cnam[256];
   // Added for portability 
/*   strcpy(cnam,getenv("ABGENE_W_PREFIX"));
   strcat(cnam,"path_");
   strcat(cnam,type);
   strcat(cnam,"_");
   strcat(cnam,name);
   strcat(cnam,".");
   strcat(cnam,ch);
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){
      fin.clear();
      strcpy(cnam,getenv("ABGENE_W_PREFIX"));
      strcat(cnam,"path_");
      strcat(cnam,type);
      strcat(cnam,"_");
      strcat(cnam,name);
      fin.open(cnam,ios::in);
      if(!fin.is_open()){
	 fin.clear();
	 strcpy(cnam,getenv("ABGENE_W_PREFIX"));
         strcat(cnam,"path_");
         strcat(cnam,type);
         fin.open(cnam,ios::in);
         if(!fin.is_open()){
            cout << "Path file " << cnam << " does not exist!" << endl;
            exit(0);
         }
      }
   } */

   //fin.getline(nam,256);
   //fin.close();
   strcpy(nam,getenv("ABGENE_DATA_DIR"));
   strcat(nam,type);
   strcat(nam,"_");
   strcat(nam,name);
   strcat(nam,".");
   strcat(nam,ch);
}

void FBase::get_pathx(char *nam,long n,const char *ch){
   char cnam[256],bnam[256];

  /* strcpy(cnam,getenv("ABGENE_W_PREFIX"));
   strcat(cnam,"path_");
   strcat(cnam,type);
   strcat(cnam,"_");
   strcat(cnam,name);
   strcat(cnam,".");
   strcat(cnam,ch);
   ifstream fin(cnam,ios::in);
   if(!fin.is_open()){
      fin.clear();
      strcpy(cnam,getenv("ABGENE_W_PREFIX"));
      strcat(cnam,"path_");
      strcat(cnam,type);
      strcat(cnam,"_");
      strcat(cnam,name);
      fin.open(cnam,ios::in);
      if(!fin.is_open()){
	 fin.clear();
	 strcpy(cnam,getenv("ABGENE_W_PREFIX"));
         strcat(cnam,"path_");
         strcat(cnam,type);
         fin.open(cnam,ios::in);
         if(!fin.is_open()){
            cout << "Path file " << cnam << " does not exist!" << endl;
            exit(0);
         }
      }
   }*/

   //fin.getline(nam,256);
   //fin.close();
   strcpy(nam,"ABGENE_DATA_DIR");
   strcat(nam,type);
   strcat(nam,"_");
   strcat(nam,add_num(name,n,bnam));
   strcat(nam,".");
   strcat(nam,ch);
}

char *FBase::add_num(const char *ptr,long n,char *buf){
   char cnam[100];
   long_str(cnam,n);
   strcpy(buf,ptr);
   strcat(buf,cnam);
   return(buf);
}

int FBase::Gcom(int sflag){
   if((cflag&sflag)&&!(oflag&sflag)){
      oflag=oflag|sflag;
      return(1);
   }
   else return(0);
}

int FBase::Rcom(int sflag){
   if((cflag&sflag)&&(oflag&sflag)){
      oflag=oflag&(~sflag);
      return(1);
   }
   else return(0);
}

ifstream *FBase::get_Istr(const char *a,ios::openmode mode){
   char cnam[max_str];
   get_pathx(cnam,a);
   ifstream *pfin=new ifstream(cnam,mode);
   if(pfin->is_open())return(pfin);
   else {
      cout << "Error: " << cnam << " failed to open!" << endl;
      exit(0);
   }
}

ofstream *FBase::get_Ostr(const char *a,ios::openmode mode){
   char cnam[max_str];
   get_pathx(cnam,a);
   ofstream *pfout=new ofstream(cnam,mode);
   if(pfout->is_open())return(pfout);
   else {
      cout << "Error: " << cnam << " failed to open!" << endl;
      exit(0);
   }
}

fstream *FBase::get_Fstr(const char *a,ios::openmode mode){
   char cnam[max_str];
   get_pathx(cnam,a);
   fstream *pfstr=new fstream(cnam,mode);
   if(pfstr->is_open())return(pfstr);
   else {
      cout << "Error: " << cnam << " failed to open!" << endl;
      exit(0);
   }
}

ifstream *FBase::get_Istr(long n,const char *a,ios::openmode mode){
   char cnam[max_str];
   get_pathx(cnam,n,a);
   ifstream *pfin=new ifstream(cnam,mode);
   if(pfin->is_open())return(pfin);
   else {
      cout << "Error: " << cnam << " failed to open!" << endl;
      exit(0);
   }
}

ofstream *FBase::get_Ostr(long n,const char *a,ios::openmode mode){
   char cnam[max_str];
   get_pathx(cnam,n,a);
   ofstream *pfout=new ofstream(cnam,mode);
   if(pfout->is_open())return(pfout);
   else {
      cout << "Error: " << cnam << " failed to open!" << endl;
      exit(0);
   }
}

fstream *FBase::get_Fstr(long n,const char *a,ios::openmode mode){
   char cnam[max_str];
   get_pathx(cnam,n,a);
   fstream *pfstr=new fstream(cnam,mode);
   if(pfstr->is_open())return(pfstr);
   else {
      cout << "Error: " << cnam << " failed to open!" << endl;
      exit(0);
   }
}

void FBase::dst_Istr(ifstream *pfin){
   if(!pfin->is_open()){
      cout << "File not open!" << endl;
      exit(0);
   }
   pfin->close();
   delete pfin;
}

void FBase::dst_Ostr(ofstream *pfout){
   if(!pfout->is_open()){
      cout << "File not open!" << endl;
      exit(0);
   }
   pfout->close();
   delete pfout;
}

void FBase::dst_Fstr(fstream *pfstr){
   if(!pfstr->is_open()){
      cout << "File not open!" << endl;
      exit(0);
   }
   pfstr->close();
   delete pfstr;
}

long FBase::get_Fsiz(const char *a){
   int fld;
   struct stat datf;
   char cnam[max_str];
   get_pathx(cnam,a);
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   if(fstat(fld,&datf)){cout << cnam << " failed on size \
      determination" << endl;exit(0);}
   ::close(fld);
   return(datf.st_size);
}

long FBase::get_Fsiz(long n,const char *a){
   int fld;
   struct stat datf;
   char cnam[max_str];
   get_pathx(cnam,n,a);
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   if(fstat(fld,&datf)){cout << cnam << " failed on size \
      determination" << endl;exit(0);}
   ::close(fld);
   return(datf.st_size);
}

char *FBase::get_Read(const char *a){
   int fld;
   struct stat datf;
   char cnam[max_str];
   get_pathx(cnam,a);
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   if(fstat(fld,&datf)){cout << cnam << " failed on size \
      determination" << endl;exit(0);}
   ::close(fld);
   char *ptr=new char[datf.st_size];
   ifstream *pfin=new ifstream(cnam);
   if(!pfin->is_open()){
      cout << "Error: " << cnam << " failed to open!" << endl;
      exit(0);
   }
   pfin->read(ptr,datf.st_size);
   return(ptr);
}

char *FBase::get_Read(long n,const char *a){
   int fld;
   struct stat datf;
   char cnam[max_str];
   get_pathx(cnam,n,a);
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   if(fstat(fld,&datf)){cout << cnam << " failed on size \
      determination" << endl;exit(0);}
   ::close(fld);
   char *ptr=new char[datf.st_size];
   ifstream *pfin=new ifstream(cnam);
   if(!pfin->is_open()){
      cout << "Error: " << cnam << " failed to open!" << endl;
      exit(0);
   }
   pfin->read(ptr,datf.st_size);
   return(ptr);
}

char *FBase::get_Mmap(const char *a){
   int fld;
   struct stat datf;
   char cnam[max_str];
   get_pathx(cnam,a);
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   if(fstat(fld,&datf)){cout << cnam << " failed on size determination" << endl;exit(0);}
   char *ptr=(char*)mmap(0,datf.st_size,PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
   if(ptr==MAP_FAILED){cout << cnam << " failed to map" << endl;exit(0);}
   ::close(fld);
   return(ptr);
}

char *FBase::get_Mmap(long n,const char *a){
   int fld;
   struct stat datf;
   char cnam[max_str];
   get_pathx(cnam,n,a);
   fld=::open(cnam,O_RDONLY);
   if(fld<=0){cout << cnam << " failed to open" << endl;exit(0);}
   if(fstat(fld,&datf)){cout << cnam << " failed on size determination" << endl;exit(0);}
   char *ptr=(char*)mmap(0,datf.st_size,PROT_READ,MAP_PRIVATE|MAP_NORESERVE,fld,0);
   if(ptr==MAP_FAILED){cout << cnam << " failed to map" << endl;exit(0);}
   ::close(fld);
   return(ptr);
}

void FBase::dst_Mmap(const char *a,char *ptr){
   struct stat datf;
   char cnam[max_str];
   get_pathx(cnam,a);
   if(stat(cnam,&datf)){cout << cnam << " failed on size determination" << endl;exit(0);}
   if(munmap(ptr,datf.st_size)){cout << cnam << " failed to unmap" << endl;exit(0);}
}

void FBase::dst_Mmap(long n,const char *a,char *ptr){
   struct stat datf;
   char cnam[max_str];
   get_pathx(cnam,n,a);
   if(stat(cnam,&datf)){cout << cnam << " failed on size determination" << endl;exit(0);}
   if(munmap(ptr,datf.st_size)){cout << cnam << " failed to unmap" << endl;exit(0);}
}

void FBase::bin_Writ(const char *a,long nm,char *ptr){
   ofstream *pfout=get_Ostr(a,ios::out);
   long k=100000,i=0;
   while(i+k<nm){
      pfout->write((char*)ptr,k);
      i+=k;
      ptr=ptr+k;
   }
   pfout->write((char*)ptr,nm-i);
   pfout->close();
   delete pfout;
}

void FBase::bin_Writ(long n,const char *a,long nm,char *ptr){
   ofstream *pfout=get_Ostr(n,a,ios::out);
   long k=100000,i=0;
   while(i+k<nm){
      pfout->write((char*)ptr,k);
      i+=k;
      ptr=ptr+k;
   }
   pfout->write((char*)ptr,nm-i);
   pfout->close();
   delete pfout;
}

int FBase::Exists(const char *a){
   char cnam[max_str];
   get_pathx(cnam,a);
   ifstream fin(cnam,ios::in);
   if(fin.is_open()){
      fin.close();
      return(1);
   }
   else return(0);
}
   
}
