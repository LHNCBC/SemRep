#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include "Dbinbase.h"
#include <Post.h>
#include <Btree.h>
#include <runn.h>
#include <Docum.h>
#include <Istreg.h>
#include <DataObj.h>
namespace iret {

Dbinbase::Dbinbase(const char *nam) : FBase("dbnset",nam) {
}

Dbinbase::~Dbinbase(){
}

void Dbinbase::create_files(const char *dnam,const char *pnam,float (*d_local)(int,long)){
   char cnam[max_str],pch[max_str],*ptr;
   long i,j,k,nw1;
   int pflag=get_qflag(),jx,sum;
   long *nwd1=new long[word_cnt];
   float *lwt1=new float[word_cnt];

   FBase Fp("postset",pnam);
   
   ifstream *pfin=Fp.get_Istr("d",ios::in);
   *pfin >> ndoc;
   pfin->close();

   pfin=Fp.get_Istr("n",ios::in);
   *pfin >> nwrd;
   pfin->close();

   Count Cnt;
   pfin=Fp.get_Istr("s",ios::in);
   i=1;
   while(get_string(cnam,*pfin,'\n')){
      Cnt.add_count(cnam,i++);
      mark(pflag,i,10000,"terms");
   }
   pfin->close();
   if(i-1!=nwrd){cout << "Error in term entrance!" << endl; exit(0);}
   
   ofstream *pfow=get_Ostr("w",ios::out);
   ofstream *pfol=get_Ostr("l",ios::out);
   long *addr=new long[ndoc],pos=0;
   size=new long[ndoc];
   float x,s;
   Docum Doc(dnam);
   Doc.gopen_read(READ_W|READ_M);
   for(i=0;i<ndoc;i++){
      Doc.read();
      nw1=0;
      sum=*(Doc.alen+i);
      while((ptr=Doc.show(jx))!=NULL){
         x=d_local(jx,sum);
         j=nwd1[nw1]=Cnt.count(ptr)-1;
         if(j<0){cout << "Error in term id!" << endl; exit(0);}
         lwt1[nw1++]=x;
      }
      *(addr+i)=pos;
      *(size+i)=nw1;
      pfow->write((char*)nwd1,nw1*sizeof(long));
      pfol->write((char*)lwt1,nw1*sizeof(float));
      pos+=nw1;
      mark(pflag,i,100,"documents");
   }
   pfow->close();
   pfol->close();
   Doc.gclose_read(READ_W|READ_M);

   pfow=get_Ostr("n",ios::out);
   *pfow << nwrd << endl;
   pfow->close();

   pfow=get_Ostr("d",ios::out);
   *pfow << ndoc << endl;
   pfow->close();

   bin_Writ("a",ndoc*sizeof(long),(char*)addr);
   bin_Writ("s",ndoc*sizeof(long),(char*)size);

   delete [] nwd1;
   delete [] lwt1;
   delete [] addr;
   delete [] size;
}

void Dbinbase::create_files(Docum &Doc,Count &Ct,float (*d_local)(int,long)){
   char cnam[max_str],pch[max_str],*ptr;
   long i,j,k,nw1;
   int pflag=get_qflag(),jx,sum;
   long *nwd1=new long[word_cnt];
   float *lwt1=new float[word_cnt];

   nwrd=Ct.cnt_key;

   ofstream *pfow=get_Ostr("w",ios::out);
   ofstream *pfol=get_Ostr("l",ios::out);
   float x,s;

   Doc.gopen_read(READ_W|READ_M);
   ndoc=Doc.ndoc;
   long *addr=new long[ndoc],pos=0;
   size=new long[ndoc];

   for(i=0;i<ndoc;i++){
      Doc.read();
      nw1=0;
      sum=*(Doc.alen+i);
      while((ptr=Doc.show(jx))!=NULL){
         x=d_local(jx,sum);
         j=nwd1[nw1]=Ct.count(ptr)-1;
         if(j<0){cout << "Error in term id!" << endl; exit(0);}
         lwt1[nw1++]=x;
      }
      *(addr+i)=pos;
      *(size+i)=nw1;
      pfow->write((char*)nwd1,nw1*sizeof(long));
      pfol->write((char*)lwt1,nw1*sizeof(float));
      pos+=nw1;
      mark(pflag,i,100,"documents");
   }
   pfow->close();
   pfol->close();
   Doc.gclose_read(READ_W|READ_M);

   pfow=get_Ostr("n",ios::out);
   *pfow << nwrd << endl;
   pfow->close();

   pfow=get_Ostr("d",ios::out);
   *pfow << ndoc << endl;
   pfow->close();

   bin_Writ("a",ndoc*sizeof(long),(char*)addr);
   bin_Writ("s",ndoc*sizeof(long),(char*)size);

   delete [] nwd1;
   delete [] lwt1;
   delete [] addr;
   delete [] size;
}

void Dbinbase::gopen_operate(int rfil){
   cflag=rfil;
   ifstream *pfin;

   pfin=get_Istr("d",ios::in);
   *pfin >> ndoc;
   dst_Istr(pfin);

   pfin=get_Istr("n",ios::in);
   *pfin >> nwrd;
   dst_Istr(pfin);

   if(Gcom(DBIN_A)){
      dad=(long*)get_Mmap("a");
      size=(long*)get_Mmap("s");
   }

   if(Gcom(DBIN_W)){
      don=(long*)get_Mmap("w");
   }
   if(Gcom(DBIN_L)){
      dow=(float*)get_Mmap("l");
   }

}

void Dbinbase::gclose_operate(void){
   if(Rcom(DBIN_A)){
      dst_Mmap("a",(char*)dad);
      dst_Mmap("s",(char*)size);
   }
   if(Rcom(DBIN_W)){
      dst_Mmap("w",(char*)don);
   }
   if(Rcom(DBIN_L)){
      dst_Mmap("l",(char*)dow);
   }
}

//naive Bayes

Bayes::Bayes(const char *nam) : Dbinbase(nam){
   tx=NULL;
   sx=NULL;
   weg=NULL;
   ax=NULL;
   csg=NULL;
   sco=NULL;
   tsel=NULL;
}

Bayes::~Bayes(void){
   if(tx!=NULL)delete [] tx;
   if(sx!=NULL)delete [] sx;
   if(weg!=NULL) delete [] weg;
   if(ax!=NULL)delete [] ax;
   if(csg!=NULL)delete [] csg;
   if(sco!=NULL)delete [] sco;
   if(tsel!=NULL)delete [] tsel;
}

void Bayes::gopen_Bayes(void){
   this->gopen_operate(DBIN_A|DBIN_W);

   tx=new long[nwrd];
   sx=new long[nwrd];
   weg=new double[nwrd];
   ax=new double[nwrd];
   csg=new double[nwrd];
}

void Bayes::gclose_Bayes(void){
   if(tx!=NULL){delete [] tx;tx=NULL;}
   if(sx!=NULL){delete [] sx;sx=NULL;}
   if(weg!=NULL){delete [] weg;weg=NULL;}
   if(ax!=NULL){delete [] ax;ax=NULL;}
   if(csg!=NULL){delete [] csg;csg=NULL;}
   if(tsel!=NULL){delete [] tsel;tsel=NULL;}

   this->gclose_operate();
}

void Bayes::read(long n){
   nw=*(size+n);
   nwd=don+*(dad+n);
}
 
void Bayes::zerot(void){
   for(long i=0;i<nwrd;i++)*(tx+i)=0;
}

void Bayes::zeros(void){
   for(long i=0;i<nwrd;i++)*(sx+i)=0;
}


void Bayes::countDoc(long i){
   long j,k;

   this->read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(tx+j))++;
   }
}

void Bayes::counsDoc(long i){
   long j,k;

   this->read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(sx+j))++;
   }
}

void Bayes::countTot(void){
   long i;
   nnx=ndoc;
   for(i=0;i<ndoc;i++)this->countDoc(i);
}

void Bayes::countTot(Index *idx){
   long i;
   nnx=idx->ix;
   for(i=0;i<idx->ix;i++)this->countDoc(*(idx->idx+i));
}

void Bayes::countSub(Index *idx){
   long i;
   nsx=idx->ix;
   for(i=0;i<idx->ix;i++)this->counsDoc(*(idx->idx+i));
}

void Bayes::weightSall(void){
   long n_t,n_st,flag,min;
   long nstw=0,max,diff;
   double xx,frc,pt,qt,rt;

   cs=0;
   frc=((double)nsx)/((double)nnx);
   diff=nsx-nnx;

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      if(n_t&&(n_t<nnx)){
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         n_st=*(sx+i);
         flag=1;
         if(n_st==min){
            if(n_st-1>n_t*frc)n_st -=1;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+1<n_t*frc)n_st=max+1;
            else flag=0;
         }
      }
      else flag=0;

      if(flag){
         nstw++;
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         *(weg+i)=log(pt*(1.0-qt))-log(qt*(1.0-pt));
 
         xx=-(double)n_st*log(rt/pt) - (double)(n_t-n_st)*log(rt/qt);
         xx-=(double)(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         xx-=(double)(nnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));
         *(ax+i)=xx;

         xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
         xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
         *(csg+i)=xx;
         cs+=xx;
      }
      else {
         *(weg+i)=0;
         *(csg+i)=0;
         *(ax+i)=0;
      }
   }
cout << "cs= " << cs << endl;
cout << "Number of weighted terms= " << nstw << endl;
}

void Bayes::weightSneg(void){
   long n_t,n_st,flag,min;
   long nstw=0;
   double xx,frc,pt,qt,rt;

   cs=0;
   frc=((double)nsx)/((double)nnx);

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      n_st=*(sx+i);
      flag=1;
      if(n_st==0){
         if(n_st+1<n_t*frc)n_st +=1;
         else flag=0;
      }
      else if(n_st>=n_t*frc){
         flag=0;
      }

      if(flag){
         nstw++;
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         *(weg+i)=log(pt*(1.0-qt))-log(qt*(1.0-pt));
 
         xx=-(double)n_st*log(rt/pt) - (double)(n_t-n_st)*log(rt/qt);
         xx-=(double)(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         xx-=(double)(nnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));
         *(ax+i)=xx;

         xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
         xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
         *(csg+i)=xx;
         cs+=xx;
      }
      else {
         *(weg+i)=0;
         *(csg+i)=0;
         *(ax+i)=0;
      }
   }
cout << "cs= " << cs << endl;
cout << "Number of weighted terms= " << nstw << endl;
}

void Bayes::weightSpos(void){
   long n_t,n_st,flag,min;
   long nstw=0;
   double xx,frc,pt,qt,rt;

   cs=0;
   frc=((double)nsx)/((double)nnx);

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      min=(n_t<nsx)?n_t:nsx;
      n_st=*(sx+i);
      flag=1;
      if(n_st==min){
         if(n_st-1>n_t*frc)n_st -=1;
         else flag=0;
      }
      else if(n_st<=n_t*frc){
         flag=0;
      }

      if(flag){
         nstw++;
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         *(weg+i)=log(pt*(1.0-qt))-log(qt*(1.0-pt));
 
         xx=-(double)n_st*log(rt/pt) - (double)(n_t-n_st)*log(rt/qt);
         xx-=(double)(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         xx-=(double)(nnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));
         *(ax+i)=xx;

         xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
         xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
         *(csg+i)=xx;
         cs+=xx;
      }
      else {
         *(weg+i)=0;
         *(csg+i)=0;
         *(ax+i)=0;
      }
   }
cout << "cs= " << cs << endl;
cout << "Number of weighted terms= " << nstw << endl;
}

void Bayes::weightSidf(void){
   long i,j;
   double xx=(double)nnx;
   for(i=0;i<nwrd;i++){
      if(j=*(tx+i))*(weg+i)=log(xx/((double)j));
      else *(weg+i)=0;
   }
   cs=0;
}

double *Bayes::ScoreAll(void){
   int pflag=get_qflag();
   long i,j,n;
   double sum;

   if(sco!=NULL)delete [] sco;
   sco=new double[ndoc];

   for(i=0;i<ndoc;i++){
      read(i);
      sum=cs;
      for(n=0;n<nw;n++)sum+=weg[nwd[n]];
      sco[i]=sum;
      mark(pflag,i+1,100,"docs scored");
   }

   return(sco);
}

double *Bayes::ScoreSet(Index *ind){
   int pflag=get_qflag();
   long i,n;
   double sum;

   if(sco!=NULL)delete [] sco;
   sco=new double[ind->ix];

   for(i=0;i<ind->ix;i++){
      read(ind->idx[i]);
      sum=cs;
      for(n=0;n<nw;n++)sum+=weg[nwd[n]];
      sco[i]=sum;
      mark(pflag,i+1,100,"docs scored");
   }
   return(sco);
}


void Bayes::select_chi(double cut){
   long n_t,n_st,flag,min;
   double xx,frc,thr;

   if(tsel==NULL)tsel=new long[nwrd];
   ntsel=0;
   frc=((double)nsx)/((double)nnx);

   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      min=(n_t<nsx)?n_t:nsx;
      n_st=*(sx+i);
      flag=1;
      if(n_st==min){
         if(n_st-1>n_t*frc)n_st -=1;
         else flag=0;
      }
      else if(n_st==0){
         if(1<n_t*frc)n_st=1;
         else flag=0;
      }
      if(flag){
         xx=frc*n_t;
         thr=cut*xx*(1.0-xx/((double)nnx));
         xx-=(double)n_st;
         if(xx*xx<=thr)flag=0;
      }
      if(flag){*(tsel+i)=1;ntsel++;}
      else *(tsel+i)=0;
   }
   cout << "chi terms " << ntsel << endl;
}

void Bayes::select_axx(double cut){
   if(tsel==NULL)tsel=new long[nwrd];
   ntsel=0;

   for(long i=0;i<nwrd;i++){
      if(*(ax+i)>cut){*(tsel+i)=1;ntsel++;}
      else *(tsel+i)=0;
   }
   cout << "axx terms " << ntsel << endl;
}

void Bayes::select_weg(double cut){
   if(tsel==NULL)tsel=new long[nwrd];
   ntsel=0;
   for(long i=0;i<nwrd;i++){
      if(fabs(*(weg+i))>cut){*(tsel+i)=1;ntsel++;}
      else *(tsel+i)=0;
   }
   cout << "weg terms " << ntsel << endl;
}

void Bayes::zero_bsel(void){
   cs=0;
   for(long i=0;i<nwrd;i++){
      if(!*(tsel+i))*(weg+i)=0;
      else cs+=*(csg+i);
   }
}

//naive MBayes

MBayes::MBayes(const char *nam) : Dbinbase(nam){
   tx=NULL;
   sx=NULL;
   weg=NULL;
   ax=NULL;
   sco=NULL;
   tsel=NULL;
}

MBayes::~MBayes(void){
   if(tx!=NULL)delete [] tx;
   if(sx!=NULL)delete [] sx;
   if(weg!=NULL) delete [] weg;
   if(ax!=NULL)delete [] ax;
   if(sco!=NULL)delete [] sco;
   if(tsel!=NULL)delete [] tsel;
}

void MBayes::gopen_MBayes(void){
   this->gopen_operate(DBIN_A|DBIN_W|DBIN_L);

   tx=new double[nwrd];
   sx=new double[nwrd];
   weg=new double[nwrd];
   ax=new double[nwrd];
}

void MBayes::gclose_MBayes(void){
   if(tx!=NULL){delete [] tx;tx=NULL;}
   if(sx!=NULL){delete [] sx;sx=NULL;}
   if(weg!=NULL){delete [] weg;weg=NULL;}
   if(ax!=NULL){delete [] ax;ax=NULL;}
   if(tsel!=NULL){delete [] tsel;tsel=NULL;}

   this->gclose_operate();
}

void MBayes::read(long n){
   nw=*(size+n);
   nwd=don+*(dad+n);
   lwt=dow+*(dad+n);
}
 
void MBayes::zerot(void){
   for(long i=0;i<nwrd;i++)*(tx+i)=0;
}

void MBayes::zeros(void){
   for(long i=0;i<nwrd;i++)*(sx+i)=0;
}


void MBayes::countDoc(long i){
   long j,k;

   this->read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(tx+j))+=*(lwt+k);
   }
}

void MBayes::counsDoc(long i){
   long j,k;

   this->read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(sx+j))+=*(lwt+k);
   }
}

void MBayes::countTot(void){
   long i;
   nnx=(double)ndoc;
   for(i=0;i<ndoc;i++)this->countDoc(i);
}

void MBayes::countTot(Index *idx){
   long i;
   nnx=(double)idx->ix;
   for(i=0;i<idx->ix;i++)this->countDoc(*(idx->idx+i));
}

void MBayes::countSub(Index *idx){
   long i;
   nsx=(double)idx->ix;
   for(i=0;i<idx->ix;i++)this->counsDoc(*(idx->idx+i));
}


void MBayes::weightSall(void){
   double n_t,n_st;
   long nstw=0;
   double xx,frc,pt,qt,rt,st,ut,vt,kt,jt,zt;

   nnx=nsx=0;
   for(long i=0;i<nwrd;i++){
      nsx+=sx[i];
      nnx+=tx[i];
   }
   ut=(double)(nnx+2.0*nwrd);

   for(long i=0;i<nwrd;i++){
      n_t=tx[i];
      n_st=sx[i];

      nstw++;
      pt =n_st+1.0;
      qt =n_t-n_st+1.0;
      rt =(double)(nsx+nwrd);
      st =(double)(nnx-nsx+nwrd);
      //calculate wt for this term
      *(weg+i)=log(pt)-log(rt)-log(qt)+log(st);
      vt=pt+qt; //tx
      kt=ut-vt; //nnx-tx
      jt=rt-pt; //nsx-sx
      zt=kt-jt; //nnx-nsx-(tx-sx)
      *(ax+i)=log(ut)+(pt*log(pt/(rt*vt))+qt*log(qt/(st*vt))+ \
              jt*log(jt/(rt*kt))+zt*log(zt/(st*kt)))/ut;
   }
}

double *MBayes::ScoreAll(void){
   int pflag=get_qflag();
   long i,j,n;
   double sum;

   if(sco!=NULL)delete [] sco;
   sco=new double[ndoc];

   for(i=0;i<ndoc;i++){
      read(i);
      sum=0;
      for(n=0;n<nw;n++)sum+=weg[nwd[n]]*lwt[n];
      sco[i]=sum;
      mark(pflag,i+1,100,"docs scored");
   }

   return(sco);
}

double *MBayes::ScoreSet(Index *ind){
   int pflag=get_qflag();
   long i,n;
   double sum;

   if(sco!=NULL)delete [] sco;
   sco=new double[ind->ix];

   for(i=0;i<ind->ix;i++){
      read(ind->idx[i]);
      sum=0;
      for(n=0;n<nw;n++)sum+=weg[nwd[n]]*lwt[n];
      sco[i]=sum;
      mark(pflag,i+1,100,"docs scored");
   }
   return(sco);
}

void MBayes::select_axx(double cut){
   if(tsel==NULL)tsel=new long[nwrd];
   ntsel=0;

   for(long i=0;i<nwrd;i++){
      if(*(ax+i)>cut){*(tsel+i)=1;ntsel++;}
      else *(tsel+i)=0;
   }
   cout << "axx terms " << ntsel << endl;
}

void MBayes::zero_nsel(void){
   for(long i=0;i<nwrd;i++){
      if(!*(tsel+i))*(weg+i)=0;
   }
}

//Support Vector Machine with local weight
Svmach::Svmach(const char *nam) : Dbinbase(nam){
   weg=NULL;
   kerc=NULL;
   alp=NULL;
   tgt=NULL;
   err=NULL;
   mrk=NULL;
   gd=NULL;
   bd=NULL;
   sizc=NULL;
   danc=NULL;
   dawc=NULL;
   sco=NULL;
   dawc=NULL;
}

Svmach::~Svmach(void){
   if(sco!=NULL)delete [] sco;
   if(kerc!=NULL)delete [] kerc;
   if(alp!=NULL)delete [] alp;
   if(tgt!=NULL)delete [] tgt;
   if(err!=NULL)delete [] err;
   if(mrk!=NULL)delete [] mrk;
   if(gd!=NULL)delete gd;
   if(bd!=NULL)delete bd;
   if(sizc!=NULL)delete [] sizc;
   if(danc!=NULL)delete [] danc;
   if(dawc!=NULL)delete [] dawc;

   
}

void Svmach::gopen_Svmach(void){
   this->gopen_operate(DBIN_A|DBIN_W|DBIN_L);
   weg=new double[nwrd];
   
}

void Svmach::gclose_Svmach(void){
   
   if(weg!=NULL){delete [] weg;weg=NULL;}
   this->gclose_operate();
}

void Svmach::read(long n){
   nw=*(size+n);
   nwd=don+*(dad+n);
   lwt=dow+*(dad+n);

}

void Svmach::read(long n,int sflag){
   switch(sflag){
      case 1: nw1=*(sizc+n);
              nwd1=*(danc+n);
              lwt1=*(dawc+n);
              break;
      case 2: nw2=*(sizc+n);
              nwd2=*(danc+n);
              lwt2=*(dawc+n);
              break;
      case 3: nw3=*(sizc+n);
              nwd3=*(danc+n);
              lwt3=*(dawc+n);
              break;
      default:break;
   }
}


void Svmach::setup_Svmach(Index *gdd,Index *bdd){
   double s,xx;
   long i,j,k,n;
   bth=0;
   if(gd!=NULL)delete gd;
   if(bd!=NULL)delete bd;
   gd=new Index(gdd);
   bd=new Index(bdd);
   
   ncset=gd->ix+bd->ix;
   
   if(sizc==NULL)sizc=new long[ncset];
   if(danc==NULL)danc=(long**)new long[ncset];
   if(dawc==NULL)dawc=(float**)new float[ncset];

   for(i=0;i<gd->ix;i++){
      j=gd->idx[i];
      *(sizc+i)=*(size+j);
      *(danc+i)=don+*(dad+j);
      *(dawc+i)=dow+*(dad+j);
   }

   for(i=0;i<bd->ix;i++){
      j=bd->idx[i];
      k=i+gd->ix;
      *(sizc+k)=*(size+j);
      *(danc+k)=don+*(dad+j);
      *(dawc+k)=dow+*(dad+j);
   }

 
   kerc=new double[ncset];
   alp=new double[ncset];
   tgt=new double[ncset];
   err=new double[ncset];
   mrk=new long[ncset+1];
   for(i=0;i<gd->ix;i++){
      this->read(i,1);
      s=0;
      for(n=0;n<nw1;n++){
         xx=*(lwt1++);
         s+=xx*xx;
      }
      *(kerc+i)=s;
      *(alp+i)=0;
      *(tgt+i)=1.0;
      *(err+i)=0;
   }

   for(i=0;i<bd->ix;i++){
      k=i+gd->ix;
      this->read(k,1);
      s=0;
      for(n=0;n<nw1;n++){
         xx=*(lwt1++);
         s+=xx*xx;
      }
      *(kerc+k)=s;
      *(alp+k)=0;
      *(tgt+k)=-1.0;
      *(err+k)=0;
   }

   *mrk=0; //Zero error cache
   sch=0;

   for(i=0;i<nwrd;i++){
      *(weg+i)=0;
   }
}

double Svmach::fasValue(long n){
   double s=0;
   long i,j,*p1=nwd1;
   

   this->read(n,1);
   for(i=0;i<nw1;i++){
      j=*(nwd1++);
      s+=*(weg+j);
   }
   nwd1=p1;
  
   return(s+bth);
}

void Svmach::remove_Svmach(void){
   if(kerc!=NULL){delete [] kerc;kerc=NULL;}
   if(alp!=NULL){delete [] alp;alp=NULL;}
   if(tgt!=NULL){delete [] tgt;tgt=NULL;}
   if(err!=NULL){delete [] err;err=NULL;}
   if(mrk!=NULL){delete [] mrk;mrk=NULL;}
}

void Svmach::debug_err(void){
   if(sch){
      cout << sch << "{" << endl;
      cur=*(mrk);
      while(cur){
         cout << cur << " c " << *(err+cur-1) << " a " << fasValue(cur-1)-*(tgt+cur-1) << endl;
         cur=*(mrk+cur);
      }
      cout << "bth=" << bth << " } " << endl;
    }
}

void Svmach::debug_alpha(void){
   long i;
   cout << "bth " << bth << endl;
   float *alps=new float[ncset];
   long *tgts=new long[ncset];
   for(i=0;i<ncset;i++){
       *(alps+i)=(float)*(alp+i);
       *(tgts+i)=rnd(*(tgt+i));
   }
   hSort(ncset,alps,tgts);
   cout << "alp:" << endl;
   for(i=0;i<ncset;i++){
      cout << *(alps+i) << "\t" << *(tgts+i) << endl;
   }
}

double Svmach::kern12(void){
   double s=0;
   int i,j;
   long u,v,*p1=nwd1,*p2=nwd2;

   i=j=0;
   next:
      if((u=*nwd1)<(v=*nwd2)){
         if(++i<nw1){u=*(++nwd1);goto next;}
      }
      else if(v<u){
         if(++j<nw2){v=*(++nwd2);goto next;}
      }
      else {
         s+=lwt1[i]*lwt2[j];
         if((++i<nw1)&&(++j<nw2)){
            u=*(++nwd1);
            v=*(++nwd2);
            goto next;
         }
      }
   nwd1=p1;
   nwd2=p2;
   return(s);
}

double Svmach::kern13(void){
   double s=0;
   int i,j;
   long u,v,*p1=nwd1,*p3=nwd3;

   i=j=0;
   next:
      if((u=*nwd1)<(v=*nwd3)){
         if(++i<nw1){u=*(++nwd1);goto next;}
      }
      else if(v<u){
         if(++j<nw3){v=*(++nwd3);goto next;}
      }
      else {
         s+=lwt1[i]*lwt3[j];
         if((++i<nw1)&&(++j<nw3)){
            u=*(++nwd1);
            v=*(++nwd3);
            goto next;
         }
      }
   nwd1=p1;
   nwd3=p3;
   return(s);
}

double Svmach::kern23(void){
   double s=0;
   int i,j;
   long u,v,*p2=nwd2,*p3=nwd3;

   i=j=0;
   next:
      if((u=*nwd2)<(v=*nwd3)){
         if(++i<nw2){u=*(++nwd2);goto next;}
      }
      else if(v<u){
         if(++j<nw3){v=*(++nwd3);goto next;}
      }
      else {
         s+=lwt2[i]*lwt3[j];
         if((++i<nw2)&&(++j<nw3)){
            u=*(++nwd2);
            v=*(++nwd3);
            goto next;
         }
      }
   nwd2=p2;
   nwd3=p3;
   return(s);
}

double Svmach::svmObject(void){
   double s=0,x=0,t;
   long i;
 
   for(i=0;i<ncset;i++)x+=*(alp+i);
   for(i=0;i<nwrd;i++){
      t=*(weg+i);
      s+=t*t;
   }
   return(x-0.5*s);
}

int Svmach::takeStep(long i1,long i2){
   long i,j,k;
   double xx,yy,zz;

   if(i1==i2)return(0);

   alpha1=*(alp+i1);
   y1=*(tgt+i1);
   if((alpha1>0)&&(alpha1<cbd))e1=*(err+i1);
   else e1=fasValue(i1)-y1;

   double s=y1*y2,a1,a2;
   double low=0,hig=cbd;
   double gam=alpha2+s*alpha1;
   if(y1!=y2){
      if(low<gam)low=gam;
      if(hig>cbd+gam)hig=cbd+gam;
   }
   else {
      if(low<gam-cbd)low=gam-cbd;
      if(hig>gam)hig=gam;
   }
   if(low==hig)return(0);

   this->read(i1,1);
   this->read(i2,2);
   double k12=kern12();
   double eta=2.0*k12-*(kerc+i1)-*(kerc+i2);
   if(eta<0){
      a2=alpha2-y2*(e1-e2)/eta;
      if(a2<low)a2=low;
      else if(a2>hig)a2=hig;
   }
   else {
      double lobj,hobj,s=0;
      s=fasValue(i2)-bth;
      xx=low-alpha2;
      yy=y2*(low-alpha2);
      lobj=xx-0.5*yy*(2.0*s+yy*(*(kerc+i2)));//Values sufficient for
           //testing but not actual Objective function values.
      xx=hig-alpha2;
      yy=y2*(hig-alpha2);
      hobj=xx-0.5*yy*(2.0*s+yy*(*(kerc+i2)));
      if(lobj>hobj+eps)a2=low;
      else if(lobj<hobj-eps)a2=hig;
      else return(0);
   } 
   if(fabs(a2-alpha2)<eps*(a2+alpha2+eps))return(0);

   a1=alpha1+s*(alpha2-a2);

   //Update threshold bth
   double bold=bth;
   double ya1=y1*(a1-alpha1),ya2=y2*(a2-alpha2);

   if((a1>0)&&(a1<cbd)){
      bth=bold-e1-ya1*(*(kerc+i1))-ya2*k12;
   }
   else if((a2>0)&&(a2<cbd)){
      bth=bold-e2-ya1*k12-ya2*(*(kerc+i2));
   }
   else {
      bth=bold-0.5*(e1+e2+ya1*(k12+*(kerc+i1))+ya2*(k12+*(kerc+i2)));
   }

   //Update weight vector weg
   for(i=0;i<nw1;i++){
      j=*(nwd1+i);
      *(weg+j)+=ya1*(*(lwt1+i));
   }
   for(i=0;i<nw2;i++){
      j=*(nwd2+i);
      *(weg+j)+=ya2*(*(lwt2+i));
   }

   //Update error cache
   cur=*(mrk);
   while(cur){
      this->read(cur-1,3);
      *(err+cur-1)+=ya1*kern13()+ya2*kern23()+bth-bold;
      cur=*(mrk+cur);
   }
   if((alpha1>0)&&(alpha1<cbd)){
      if((a1>0)&&(a1<cbd)){
         *(err+i1)=fasValue(i1)-y1;
      }
      else {
         i=0;
         cur=*(mrk);
         while(cur-1<i1){
            i=cur;
            cur=*(mrk+cur);
         }
         *(mrk+i)=*(mrk+cur);
         sch--;
      }
   }
   else {
      if((a1>0)&&(a1<cbd)){
         *(err+i1)=fasValue(i1)-y1;
         i=0;
         cur=*(mrk);
         while(cur&&(cur-1<i1)){
            i=cur;
            cur=*(mrk+cur);
         }
         *(mrk+i)=i1+1;
         *(mrk+i1+1)=cur;
         sch++;
      }
   }

   if((alpha2>0)&&(alpha2<cbd)){
      if((a2>0)&&(a2<cbd)){
         *(err+i2)=fasValue(i2)-y2;
      }
      else {
         i=0;
         cur=*(mrk);
         while(cur-1<i2){
            i=cur;
            cur=*(mrk+cur);
         }
         *(mrk+i)=*(mrk+cur);
         sch--;
      }
   }
   else {
      if((a2>0)&&(a2<cbd)){
         *(err+i2)=fasValue(i2)-y2;
         i=0;
         cur=*(mrk);
         while(cur&&(cur-1<i2)){
            i=cur;
            cur=*(mrk+cur);
         }
         *(mrk+i)=i2+1;
         *(mrk+i2+1)=cur;
         sch++;
      }
   }

   *(alp+i1)=a1;
   *(alp+i2)=a2;

   return(1);
}

int Svmach::examineExample(long i2){
   long i1,ix,jx;
   double ediff,xdiff,et;
   y2=*(tgt+i2);
   alpha2=*(alp+i2);

   if((alpha2>0)&&(alpha2<cbd))e2=*(err+i2);
   else e2=fasValue(i2)-y2;

   double r2=e2*y2;

   if(((r2<-tol)&&(alpha2<cbd))||((r2>tol)&&(alpha2>0))){
      mtri++;
      msuc++;
      if(sch){
         cur=*(mrk);
         ediff=0;
         while(cur){
            et=*(err+cur-1);
            xdiff=fabs(et-e2);
            if(xdiff>ediff){
               ediff=xdiff;
               i1=cur-1;
            }
            cur=*(mrk+cur);
         }
         if((ediff>0)&&(takeStep(i1,i2)))return(1);
         else {
            ix=zrand(sch);
            jx=0;
            cur=*mrk;
            while(jx<ix){
               cur=*(mrk+cur);
               jx++;
            }
            while(cur){
               if(takeStep(cur-1,i2))return(1);
               cur=*(mrk+cur);
            }
            jx=0;
            cur=*mrk;
            while(jx<ix){
               if(takeStep(cur-1,i2))return(1);
               cur=*(mrk+cur);
               jx++;
            }
         }
      }
      ix=zrand(ncset);
      for(jx=ix;jx<ncset;jx++)if(takeStep(jx,i2))return(1);
      for(jx=0;jx<ix;jx++)if(takeStep(jx,i2))return(1);
      msuc--;
   }
   if(mtri>1000){
      cout << mtri << "\t" << msuc << "\t" << tol << endl;
      mtri=msuc=0;
   }
   else if(mtri-msuc>100){
      tol=tol*2.0;
      cout << mtri << "\t" << msuc << "\t" << tol << endl;
      mtri=msuc=0;
   }
   return(0);
}

void Svmach::optimizeSvm(double c){
   long i,j,ttchngd;
   double alph;
  
   tol=1.0e-3;
   eps=1.0e-2;
   cbd=c;
   mtri=0;
   msuc=0;
   hsuc=990;
   lsuc=900;

   long numChanged=0,examineAll=1;
   while((numChanged>0)||examineAll){
      numChanged=0;
      ttchngd=100;
      if(examineAll){
         cout << "ExamineAll" << endl;
         for(i=0;i<ncset;i++){
            numChanged+=examineExample(i);
            if(numChanged>ttchngd){
               ttchngd+=100;
               cout << i << "\t" << svmObject() << endl;
            }
         }
      }
      else {
         cout << "ExamineSubset" << endl;
         for(i=0;i<ncset;i++){
            alph=*(alp+i);
            if((alph>0)&&(alph<cbd)){
               numChanged+=examineExample(i);
               if(numChanged>ttchngd){
                  ttchngd+=100;
                  cout << i << "\t" << svmObject() << endl;
               }
            }
         }
      }
      if(examineAll)examineAll=0;
      else if (numChanged==0)examineAll=1;
      cout << "eps=" << eps << "\t" << svmObject() << endl;
      cout << "sch=" << sch << endl;
   }
   this->write_coef();
}

void Svmach::optimizeSvm(double c,double eps1){
   long i,j,ttchngd;
   double alph;
  
   tol=1.0e-4;
   eps=eps1;
   cbd=c;
   mtri=0;
   msuc=0;
   hsuc=990;
   lsuc=900;

   restart:
   long numChanged=0,examineAll=1;
   while((numChanged>0)||examineAll){
      numChanged=0;
      ttchngd=100;
      if(examineAll){
         cout << "ExamineAll" << endl;
         for(i=0;i<ncset;i++){
            numChanged+=examineExample(i);
            if(numChanged>ttchngd){
               ttchngd+=100;
               cout << i << "\t" << svmObject() << endl;
            }
         }
      }
      else {
         cout << "ExamineSubset" << endl;
         for(i=0;i<ncset;i++){
            alph=*(alp+i);
            if((alph>0)&&(alph<cbd)){
               numChanged+=examineExample(i);
               if(numChanged>ttchngd){
                  ttchngd+=100;
                  cout << i << "\t" << svmObject() << endl;
               }
            }
         }
      }
      if(examineAll)examineAll=0;
      else if (numChanged==0)examineAll=1;
      cout << "eps=" << eps << "\t" << svmObject() << endl;
      cout << "sch=" << sch << endl;
   }
   eps=eps/10.0;
   tol=eps/100.0;
   if(eps>=1.0e-2)goto restart;
   this->write_coef();
}

void Svmach::write_coef(void){
   char cnam[256];
  
   get_pathw(cnam,"dbnset",name,"c");
   ofstream fout(cnam,ios::out|ios::binary);
   fout.write((char*)alp,ncset*sizeof(double));
   fout.close();
}


double *Svmach::ScoreAll(void){
   int pflag=get_qflag();
   long i,j,n;
   double sum;

   if(sco!=NULL)delete [] sco;
   sco=new double[ndoc];

   for(i=0;i<ndoc;i++){
      read(i);
      sum=0;
      for(n=0;n<nw;n++)sum+=weg[nwd[n]]*lwt[n];
      sco[i]=sum;
      mark(pflag,i+1,100,"docs scored");
   }

   return(sco);
}

double *Svmach::ScoreSet(Index *ind){
   int pflag=get_qflag();
   long i,n;
   double sum;

   if(sco!=NULL)delete [] sco;
   sco=new double[ind->ix];

   for(i=0;i<ind->ix;i++){
      read(ind->idx[i]);
      sum=0;
      for(n=0;n<nw;n++)sum+=weg[nwd[n]]*lwt[n];
      sco[i]=sum;
      mark(pflag,i+1,100,"docs scored");
   }
   return(sco);
}

//Bayes_boost
Bayes_boost:: Bayes_boost(const char *nam) : Dbinbase(nam){
   tx=NULL;
   sx=NULL;
   weg=NULL;
   ax=NULL;
   sco=NULL;
   csg=NULL;
   tsel=NULL;
   gd=NULL;
   bd=NULL;
   
}

Bayes_boost::~ Bayes_boost(void){
   if(tx!=NULL)delete [] tx;
   if(sx!=NULL)delete [] sx;
   if(weg!=NULL)delete [] weg;
   if(ax!=NULL)delete [] ax;
   if(csg!=NULL)delete [] csg;
   if(tsel!=NULL)delete [] tsel;
   if(sco!=NULL)delete [] sco;
   if(gd!=NULL)delete gd;
   if(bd!=NULL)delete bd;
   

}

void Bayes_boost::gopen_Bayes_boost(void){
   this->gopen_operate(DBIN_A|DBIN_W);
   tx=new double[nwrd];
   sx=new double[nwrd];
   weg=new double[nwrd];
   ax=new double[nwrd];
   csg=new double[nwrd];
}

void Bayes_boost::gclose_Bayes_boost(void){
   if(tx!=NULL){delete [] tx;tx=NULL;}
   if(sx!=NULL){delete [] sx;sx=NULL;}
   if(weg!=NULL){delete [] weg;weg=NULL;}
   if(ax!=NULL){delete [] ax;ax=NULL;}
   if(sco!=NULL){delete [] sco;sco=NULL;}
   if(csg!=NULL){delete [] csg;csg=NULL;}
   if(tsel!=NULL){delete [] tsel;tsel=NULL;}
   this->gclose_operate();
}

void Bayes_boost::setup_Bayes_boost(Index *gdd, Index *bdd){
   double s,xx;
   long i,j,k,n;
   if(gd!=NULL)delete gd;
   if(bd!=NULL)delete bd;
   gd=new Index(gdd);
   bd=new Index(bdd);
   eps=1.0/(gd->ix+bd->ix);
}

void Bayes_boost::zerot(void){
   for(long i=0;i<nwrd;i++)*(tx+i)=0;
}

void Bayes_boost::zeros(void){
   for(long i=0;i<nwrd;i++)*(sx+i)=0;
}

void Bayes_boost::read(long n){
   nw=*(size+n);
   nwd=don+*(dad+n);
}


void Bayes_boost::countDoc(long i){
   long j,k;
   double pt;
   pt=dt[i];

   this->read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(tx+j))+=pt;
   }
}

void Bayes_boost::counsDoc(long i){
   long j,k;
   double pt;
   pt=dt[i];

   this->read(i);
   for(k=0;k<nw;k++){
      j=*(nwd+k);
      (*(sx+j))+=pt;
   }
}


void Bayes_boost::countTot(void){
   long i;
   nnx=0.0;
   for(i=0;i<ndoc;i++){
      this->countDoc(i);
      nnx+=dt[i];
   }
}

void Bayes_boost::countTot(Index *idx){
   long i;
   nnx=0.0;
   for(i=0;i<idx->ix;i++){
      this->countDoc(idx->idx[i]);
      nnx+=dt[idx->idx[i]];
   }
}

void Bayes_boost::countSub(Index *idx){
   long i;
   
   nsx=0.0;
   for(i=0;i<idx->ix;i++){
     this->counsDoc(idx->idx[i]);
     nsx+=dt[idx->idx[i]];
   }
}

void Bayes_boost::weightSall(void){
   double n_t,n_st,min,max;
   long nstw=0, flag;
   double xx,frc,pt,qt,rt,diff;

   cs=0;
   diff=nsx-nnx;
   frc=nsx/nnx;
  
   for(long i=0;i<nwrd;i++){
      n_t=*(tx+i);
      n_st=*(sx+i);
      if(n_t&&(n_t<nnx)){
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         flag=1;
         if(n_st==min){
            if(n_st-eps>n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+eps<n_t*frc)n_st=max+eps;
            else flag=0;
         }
      }
      else flag=0;


      if(flag){
         nstw++;
         pt =n_st/nsx;
         qt =(n_t-n_st)/(nnx-nsx);
         rt =n_t/nnx;
         //calculate wt for this term
         *(weg+i)=log(pt*(1.0-qt))-log(qt*(1.0-pt));



         xx=-n_st*log(rt/pt) - (n_t-n_st)*log(rt/qt);
         xx-=(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         xx-=(nnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));
         *(ax+i)=xx;

         xx=log((nsx-n_st))+log((nnx-nsx));
         xx-=(log(nsx)+log((nnx-nsx-n_t+n_st)));
         *(csg+i)=xx;
         cs+=xx;
      }
      else {
         *(weg+i)=0;
         *(csg+i)=0;
         *(ax+i)=0;
      }
   }
   cout << "cs= " << cs << endl;
   cout << "Number of weighted terms= " << nstw << endl;
}

void Bayes_boost::zero_bsel(void){
   cs=0;
   for(long i=0;i<nwrd;i++){
      if(!*(tsel+i))*(weg+i)=0;
      else cs+=*(csg+i);
   }
   
}

void Bayes_boost::select_chi(double cut){
   long flag,i;
   double n_t,n_st,min,max;
   double xx,frc,thr,diff;
   long ncset=(gd->ix+bd->ix);
   if(tsel==NULL)tsel=new long[nwrd];
   ntsel=0;
   diff=nsx-nnx;
   frc=nsx/nnx;

   for(i=0;i<nwrd;i++){
      n_t=*(tx+i);
      n_st=*(sx+i);
      if(n_t&&(n_t<nnx)){
         min=(n_t<nsx)?n_t:nsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         flag=1;
         if(n_st==min){
            if(n_st-eps>n_t*frc)n_st -=eps;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+eps<n_t*frc)n_st=max+eps;
            else flag=0;
         }
      }
      else flag=0;

      if(flag){
         xx=frc*n_t;
         thr=cut*xx*(1.0-xx/((double)nnx))/((double)ncset);
         xx-=(double)n_st;
         if(xx*xx<=thr)flag=0;
      }
      if(flag){*(tsel+i)=1;ntsel++;}
      else *(tsel+i)=0;
   }
   cout << "chi terms " << ntsel << endl;
}

void Bayes_boost::select_axx(double cut){
   if(tsel==NULL)tsel=new long[nwrd];
   ntsel=0;
   cs=0;

   for(long i=0;i<nwrd;i++){
      if(*(ax+i)>cut){
         *(tsel+i)=1;
         ntsel++;
         cs+=csg[i];
      }
      else *(tsel+i)=0;
   }
   cout << "axx terms " << ntsel << endl;
}

void Bayes_boost::select_axx(long num){
   long j,k;
   float yy;
   cs=0;
   if(tsel==NULL)tsel=new long[nwrd];
   ntsel=num;
   Order Ord(num,nwrd,ax);
   for(k=0;k<nwrd;k++) tsel[k]=0;
   for(k=0;k<num;k++){
      j=Ord.ind(k,yy);
      cs+=csg[j];
      tsel[j]=1;
   }
}

double *Bayes_boost::ScoreAll(void){
   int pflag=get_qflag();
   long i,j,n;
   double sum;

   if(sco!=NULL)delete [] sco;
   sco=new double[ndoc];

   for(i=0;i<ndoc;i++){
      read(i);
      sum=cs;
      for(n=0;n<nw;n++)sum+=weg[nwd[n]];
      sco[i]=sum;
      //mark(pflag,i+1,1000,"docs scored");
   }

   return(sco);
}

double *Bayes_boost::ScoreSet(Index *ind){
   int pflag=get_qflag();
   long i,n;
   double sum;

   if(sco!=NULL)delete [] sco;
   sco=new double[ind->ix];

   for(i=0;i<ind->ix;i++){
      read(ind->idx[i]);
      sum=cs;
      for(n=0;n<nw;n++)sum+=weg[nwd[n]];
      sco[i]=sum;
      //mark(pflag,i+1,1000,"docs scored");
   }
   return(sco);
}

}
