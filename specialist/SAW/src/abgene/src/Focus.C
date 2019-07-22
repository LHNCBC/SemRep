#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "runn.h"
#include "Focus.h"
#include "Btree.h"
#include "Docum.h"
#include "DataObj.h"
#include "Bool.h"
using namespace std;
namespace iret{

Focus::Focus(const char *nam) {
   int lxn=strlen(nam);
   name=new char[lxn+1];
   strcpy(name,nam);

   tx=NULL;
   sx=NULL;
   weg=NULL;
   alp=NULL;
   cst=NULL;
   mti=NULL;
   sco=NULL;
   mdi=NULL;
   idx=NULL;
   msa=NULL;
   msm=NULL;
   pRgg=NULL;
   pSaa=NULL;
   
   mt=0;
   md=0;
   ms=0;
   dlim=0;
   stn=0;

   bflag=1;
   err_flag=0;
   count_lim=200000;
}

Focus::Focus(const char *nam,long lim) {
   int lxn=strlen(nam);
   name=new char[lxn+1];
   strcpy(name,nam);

   tx=NULL;
   sx=NULL;
   weg=NULL;
   alp=NULL;
   cst=NULL;
   mti=NULL;
   sco=NULL;
   mdi=NULL;
   idx=NULL;
   msa=NULL;
   msm=NULL;
   pRgg=NULL;
   pSaa=NULL;

   mt=0;
   md=0;
   ms=0;
   dlim=0;
   stn=0;

   bflag=1;
   err_flag=0;
   count_lim=lim;
}

Focus::~Focus(){
   if (tx!=NULL)delete [] tx;
   if (sx!=NULL)delete [] sx;
   if (weg!=NULL)delete [] weg;
   if (alp!=NULL)delete [] alp;
   if (cst!=NULL)delete [] cst;
   if (mti!=NULL)delete [] mti;
   if (sco!=NULL)delete [] sco;
   if (mdi!=NULL)delete [] mdi;
   if (idx!=NULL)delete [] idx;
   if (msa!=NULL)delete [] msa;
   if (msm!=NULL)delete [] msm;
}

void Focus::gopen_Focus(Regist *pReg,Slice_Accomp *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

   pRgg=pReg;
   pSaa=pSac;

   pSaa->inv_load();
   pSaa->post_access();
   pSaa->gopen_idstring(pSaa->slice_name);
   nid=pSaa->nid;

   tx=new long[nid];
   sx=new long[nid];
   ms=100;
   msc=0;
   msa=new long[ms];
   msm=new long[ms];
   
   if(pflag)cout << "Opening document files" << endl;
   pRgg->set_class();
   pRgg->gopen_read(READ_U);
   pRgg->gopen_binary();
}

void Focus::gopen_Focus_map(Regist *pReg,Slice_Accomp *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

   pRgg=pReg;
   pSaa=pSac;

   pSaa->inv_map();
   pSaa->post_access_map();
   pSaa->gopen_idstring(pSaa->slice_name);
   nid=pSaa->nid;

   tx=new long[nid];
   sx=new long[nid];
   ms=100;
   msc=0;
   msa=new long[ms];
   msm=new long[ms];

   if(pflag)cout << "Opening document files" << endl;
   pRgg->set_class();
   pRgg->gopen_map(READ_U);
   pRgg->gopen_binary_map();
}

void Focus::mem_Terms(long num){
   if((mt)&&(mt!=num)){
      delete [] mti;
      delete [] weg;
      delete [] alp;
      delete [] cst;
   }
   mt=num;
   mti=new long[mt];
   weg=new float[mt];
   alp=new float[mt];
   cst=new float[mt];
}

void Focus::mem_Docms(long num){
   long i;
   if(mdi!=NULL){
      delete [] mdi;
   }
   md=num;
   mdi=new long[md];
   for(i=0;i<md;i++)*(mdi+i)=0;
}

void Focus::restart(void){
   msc=0;
   mtc=0;
}

void Focus::zerot(void){
   long i,*pt;
   nnx=0;
   pt=tx;
   for(i=0;i<nid;i++)*(pt++)=0;
}

void Focus::zeros(void){
   long i,*ps;
   nsx=0;
   ps=sx;
   for(i=0;i<nid;i++)*(ps++)=0;
}

void Focus::transfer(void){
   long i,j,*pt,*pz;
   nnx=pRgg->ntot;
   pz=pSaa->inv+1;
   pt=tx+1;
   for(i=1;i<nid;i++){
      if(j=*(pz++)){
         j--;
         *(pt++)=*(pSaa->freq+j);
      }
      else *(pt++)=0;
   }
}

void Focus::countDoc(long i){
   long k,n,*nt;
   n=pRgg->readb(i);
   nt=pRgg->tnm;
   for(k=0;k<n;k++){
      (*(tx+*(nt++)))++;
   }
   nnx++;
}

void Focus::counsDoc(long i){
   long k,n,*nt;
   n=pRgg->readb(i);
   nt=pRgg->tnm;
   for(k=0;k<n;k++){
      (*(sx+*(nt++)))++;
   }
   nsx++;
}

void Focus::counstDoc(long i){
   long k,n,*nt;
   n=pRgg->readb(i);
   nt=pRgg->tnm;
   for(k=0;k<n;k++){
      (*(sx+*nt))++;
      (*(tx+*(nt++)))++;
   }
   nnx++;
   nsx++;
}

void Focus::ncountDoc(long i){
   long k,n,*nt;
   n=pRgg->readb(i);
   nt=pRgg->tnm;
   for(k=0;k<n;k++){
      (*(tx+*(nt++)))--;
   }
   nnx--;
}

void Focus::ncounsDoc(long i){
   long k,n,*nt;
   n=pRgg->readb(i);
   nt=pRgg->tnm;
   for(k=0;k<n;k++){
      (*(sx+*(nt++)))--;
   }
   nsx--;
}
void Focus::ncounstDoc(long i){
   long k,n,*nt;
   n=pRgg->readb(i);
   nt=pRgg->tnm;
   for(k=0;k<n;k++){
      (*(sx+*nt))--;
      (*(tx+*(nt++)))--;
   }
   nnx--;
   nsx--;
}

void Focus::countTot(long ik,long *ikx){
   long i;
   int pflag=get_qflag();

   for(i=0;i<ik;i++){
      this->countDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void Focus::countSub(long ik,long *ikx){
   long i;
   int pflag=get_qflag();

   for(i=0;i<ik;i++){
      this->counsDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void Focus::countBth(long ik,long *ikx){
   long i;
   int pflag=get_qflag();

   for(i=0;i<ik;i++){
      this->counstDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void Focus::ncountTot(long ik,long *ikx){
   long i;
   int pflag=get_qflag();

   for(i=0;i<ik;i++){
      this->ncountDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void Focus::ncountSub(long ik,long *ikx){
   long i;
   int pflag=get_qflag();

   for(i=0;i<ik;i++){
      this->ncounsDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void Focus::ncountBth(long ik,long *ikx){
   long i;
   int pflag=get_qflag();

   for(i=0;i<ik;i++){
      this->ncounstDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void Focus::weightApos(void){
   long n_t,n_st,flag,min=0,trg=1;
   long i,j,mnn,iu,fneg,id;
   double xx,yy,zz,frc,pt,qt,rt;
   float uu;
   char cnam[max_str];

   if(mt==0){mtc=0;return;}
   if(nsx==0){mtc=0;return;}
   frc=((double)nsx)/((double)nnx);
   tlim=0;
   mtc=0;
   if(msc){ //Find initial term id.
     iu=0;
     id=*msa;
   }
   else id=0;

   for(i=1;i<nid;i++){
      if(i==id){
         yy=*(msm+iu)*1000000.0;
         iu++;
         if(iu<msc)id=*(msa+iu);
         else id=0;
      }
      else yy=0.0;
      n_t=*(tx+i);
      if(n_t){
         mnn=(n_t<nsx)?n_t:nsx;
         n_st=*(sx+i);
         flag=1;
         if(n_st==mnn){
            if(n_st-1>n_t*frc)n_st -=1;
            else flag=0;
         }
         else if(n_st<=n_t*frc){
            flag=0;
         }
      }
      else flag=0;

      if(flag){
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         zz=(float)(log(pt*(1.0-qt))-log(qt*(1.0-pt)));

         yy-=(double)n_st*log(rt/pt) + (double)(n_t-n_st)*log(rt/qt);
         yy-=(double)(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         yy-=(double)(nnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));

         if(yy>tlim){
            xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
            xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
            if(mtc<mt){
               *(mti+mtc)=i;
               *(weg+mtc)=zz;
               *(alp+mtc)=yy;
               *(cst+mtc)=xx;
               mtc++;
            }
            else {
               *(mti+min)=i;
               *(weg+min)=zz;
               *(alp+min)=yy;
               *(cst+min)=xx;
               tlim=yy;
               for(j=0;j<mtc;j++){
                  if((uu=*(alp+j))<tlim){
                     tlim=uu;
                     min=j;
                  }
               }
            }
         }
      }
      if((trg)&&(mtc==mt)){
         trg=0;
         tlim=*(alp);
         min=0;
         for(j=0;j<mtc;j++){
            if((uu=*(alp+j))<tlim){
               tlim=uu;
               min=j;
            }
         }
      }
   }
   cs=0;
   for(j=0;j<mtc;j++)cs+=*(cst+j);
   cout << "cs= " << cs << " terms: " << mtc << endl;
}

void Focus::weightApos(long flm){
   long n_t,n_st,flag,min=0,trg=1;
   long i,j,mnn,iu,fneg,id;
   double xx,yy,zz,frc,pt,qt,rt;
   float uu;
   char cnam[max_str];

   if(mt==0){mtc=0;return;}
   if(nsx==0){mtc=0;return;}
   frc=((double)nsx)/((double)nnx);
   tlim=0;
   mtc=0;
   if(msc){ //Find initial term id.
     iu=0;
     id=*msa;
   }
   else id=0;

   for(i=1;i<nid;i++){
      if(i==id){
         yy=*(msm+iu)*1000000.0;
         iu++;
         if(iu<msc)id=*(msa+iu);
         else id=0;
      }
      else yy=0.0;
      n_t=*(tx+i);
      if((n_t)&&(n_t<=flm)){
         mnn=(n_t<nsx)?n_t:nsx;
         n_st=*(sx+i);
         flag=1;
         if(n_st==mnn){
            if(n_st-1>n_t*frc)n_st -=1;
            else flag=0;
         }
         else if(n_st<=n_t*frc){
            flag=0;
         }
      }
      else flag=0;

      if(flag){
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         zz=(float)(log(pt*(1.0-qt))-log(qt*(1.0-pt)));

         yy-=(double)n_st*log(rt/pt) + (double)(n_t-n_st)*log(rt/qt);
         yy-=(double)(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         yy-=(double)(nnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));

         if(yy>tlim){
            xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
            xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
            if(mtc<mt){
               *(mti+mtc)=i;
               *(weg+mtc)=zz;
               *(alp+mtc)=yy;
               *(cst+mtc)=xx;
               mtc++;
            }
            else {
               *(mti+min)=i;
               *(weg+min)=zz;
               *(alp+min)=yy;
               *(cst+min)=xx;
               tlim=yy;
               for(j=0;j<mtc;j++){
                  if((uu=*(alp+j))<tlim){
                     tlim=uu;
                     min=j;
                  }
               }
            }
         }
      }
      if((trg)&&(mtc==mt)){
         trg=0;
         tlim=*(alp);
         min=0;
         for(j=0;j<mtc;j++){
            if((uu=*(alp+j))<tlim){
               tlim=uu;
               min=j;
            }
         }
      }
   }
   cs=0;
   for(j=0;j<mtc;j++)cs+=*(cst+j);
   cout << "cs= " << cs << " terms: " << mtc << endl;
}

void Focus::set_Dlim(void){
   long i;
   float sum=0.0;
   if(!mtc){dlim=100000000;return;}

   for(i=0;i<mtc;i++)sum+=*(weg+i);
   dlim=stn*sum/((double)mtc)+cs;
}

float *Focus::scoreAll(void){
   long i,j,k,m,n,id,*pi;
   int pflag=get_qflag();
   float s,*ps,*wwg;
   unsigned char c,*utr;
   long nt,yi,od,fq,iix;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   if(mtc==0)return(sco);
   if(sco!=NULL)delete [] sco;
   ps=sco=new float[pRgg->ntot];
   for(i=0;i<pRgg->ntot;i++)*(ps++)=cs;

   pi=mti;
   wwg=weg;
   for(j=0;j<mtc;j++){
      id=*(pi++);
      s=*(wwg++);
      iix=*(pSaa->inv+id);
      if(s&&iix){
         pk=pSaa->read_comp((--iix));
         ba=pSaa->pw[pk];
         utr=pSaa->cod;
         fq=*(pSaa->freq+iix);
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
                          mi+=c*(pSaa->pw[bc-8]);
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
                             *(sco+mi)+=s;
                             i++;
                             od=mi;
                             mi=0;
                          }
                       }
                       break;
            }
         }
      }
      mark(pflag,j,10000,"terms scored");
   }
   return(sco);
}

long Focus::scoreCnt(void){
   long i;
   float *ps;

   mdc=0;
   ps=sco;
   for(i=0;i<md;i++){
      if(*(ps++)>dlim)mdc++;
   }
   cout << "Ndocs: " << mdc << endl;
   return(mdc);
}

long Focus::scoreCnt_Sub(void){
   long i,*pi;
  
   mdc=0;  
   pi=idx;
   for(i=0;i<ix;i++){
      if(*(sco+*(pi++))>dlim)mdc++;
   }
   cout << "Ndocs: " << mdc << endl;
   return(mdc);
}

void Focus::init_Full(long ik,long *ikx){
   long i,j,*pi;
   int pflag=get_qflag();

   pi=ikx;
   for(i=0;i<ik;i++){
      j=*(pi++);
      this->counsDoc(j);
      *(mdi+j)=1;
      mark(pflag,i,100,"docs");
   }
   mdc=ik;
}

void Focus::countUpdate(void){
   long i,j,k,*pi;
   float *ps;
   int pflag=get_qflag();
  
   ps=sco;
   pi=mdi;
   for(i=0;i<md;i++){
      if(*(ps++)>dlim){
         if(!(*pi)){
            this->counsDoc(i);
            *pi=1;
         }
      }
      else {
         if(*pi){
            this->ncounsDoc(i);
            *pi=0;
         }
      }
      pi++;
      mark(pflag,i+1,10000,"docs");
   }
}

void Focus::countUpidx(void){
   long i,j,k,*pi,*pm;
   int pflag=get_qflag();
 
   pi=idx;
   pm=mdi;
   for(j=0;j<md;j++){
      i=*pi;
      k=*pm;
      if(*(sco+i)>dlim){
         if(!k){
            this->counsDoc(i);
            *pm=1;
         }
      }
      else {
         if(k){
            this->ncounsDoc(i);
            *pm=0;
         }
      }
      pi++;pm++;
      mark(pflag,i+1,10000,"docs");
   }
}

void Focus::show_Terms(void){
   long i,j,*ord,id;
   char cnam[max_str];
   float ss=0;
   
   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      if(i>=mtc-stn)ss+=alp[i];
      j=*(ord+i);
      id=*(mti+j);
      pSaa->disk_ifind(id,cnam);
      cout << id << "\t" << *(alp+i) << " " << *(weg+j) << " " << *(tx+id) << " "  \
       << *(sx+id) << " " << cnam << endl;
   }
   cout << endl << "Ave_Alpha " << ss/((double)stn) << endl;
   delete [] ord;
}

void Focus::show_Docms(long n){
   long i,j,k;
   long ct,*jdx,pmid;
   float min,*scx,ss;
   Order *pOrd=new Order(n,pRgg->ntot,sco);

   k=pOrd->num();
   for(i=0;i<k;i++){
      j=pOrd->ind(i,ss);
      pmid=pRgg->pmid(j);
      cout << pmid << " " << j << " " << ss << endl;
   }
   delete pOrd;
}
      
void Focus::promote(long id,long k){
   long i,j,*pa,*pm;

   if(msc==ms){  
      pa=msa;
      pm=msm;
      ms+=100;
      msa=new long[ms];
      msm=new long[ms];
      for(i=0;i<msc;i++){
         *(msa+i)=*(pa+i);
         *(msm+i)=*(pm+i);
      }
      delete [] pa;
      delete [] pm;
   }
   i=0;
   while((i<msc)&&(*(msa+i)<id))i++;
   if((i<msc)&&(*(msa+i)==id)){
      *(msm+i)=k;
      return;
   }
   for(j=msc;j>i;j--){
      *(msa+j)=*(msa+j-1);
      *(msm+j)=*(msm+j-1);
   }
   *(msa+i)=id;
   *(msm+i)=k;
   msc++;
   return;
}

void Focus::remove(long id){
   long i,j;

   i=0;
   while((i<msc)&&(*(msa+i)<id))i++;
   if(((i<msc)&&(*(msa+i)!=id))||(i==msc)){ 
      cout << "Term not in promoted set!" << endl;
      return;    
   }
   for(j=i;j<msc-1;j++){
      *(msa+j)=*(msa+j+1);
      *(msm+j)=*(msm+j+1);
   }
   msc--;
   return;
}

void Focus::promote_all(void){
   long i,j,k,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      pSaa->disk_ifind(id,cnam);
      cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j) << " " << *(tx+id) << " "  \
       << *(sx+id) << " " << cnam << endl;
      if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
   }
   delete [] ord;
}

void Focus::promote_unprom(void){
   long i,j,k,n,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      k=0;
      while((k<msc)&&(*(msa+k)!=id))k++;
      if((k==msc)||(*(msm+k)==0)){
         pSaa->disk_ifind(id,cnam);
         cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j)\
         << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
         if(n=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,n);
      }
   }
   delete [] ord;
}

void Focus::promote_prom(void){
   long i,j,k,*ord,id;
   char cnam[max_str];

   j=0;
   for(i=0;i<msc;i++){
      id=*(msa+i);
      if(*(msm+i)>0){
         pSaa->disk_ifind(id,cnam);
         k=0;
         while((k<mtc)&&(*(mti+k)!=id))k++;
         if(k<mtc){
            j++;
            cout << j << "\t" << id << "\t" << *(alp+k) << " " << *(weg+k)\
            << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
            if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
         }
      }
   }
}

void Focus::promote_demt(void){
   long i,j,k,*ord,id;
   char cnam[max_str];

   j=0;
   for(i=0;i<msc;i++){
      id=*(msa+i);
      if(*(msm+i)<0){
         pSaa->disk_ifind(id,cnam);
         j++;
         cout << j << "\t" << id << "\t" \
         << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
         if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
      }
   }
}

void Focus::promote_subs_all(const char *str){
   long i,j,k,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      pSaa->disk_ifind(id,cnam);
      if(strstr(cnam,str)){
         cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j)\
         << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
         if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
      }
   }
   delete [] ord;
}

void Focus::promote_subs_unprom(const char *str){
   long i,j,k,n,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      k=0;
      while((k<msc)&&(*(msa+k)!=id))k++;
      if((k==msc)||(*(msm+k)==0)){
         pSaa->disk_ifind(id,cnam);
         if(strstr(cnam,str)){
            cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j)\
            << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
            if(n=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,n);
         }
      }
   }
   delete [] ord;
}

void Focus::promote_subs_auto(const char *str,long k){
   long i,j,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      pSaa->disk_ifind(id,cnam);
      if(strstr(cnam,str)){
         cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j)\
         << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
         this->promote(id,k);
      }
   }
   delete [] ord;
}

void Focus::promote_subs_unprom_auto(const char *str){
   long i,j,k,n,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      k=0;
      while((k<msc)&&(*(msa+k)!=id))k++;
      if((k==msc)||(*(msm+k)==0)){
         pSaa->disk_ifind(id,cnam);
         if(strstr(cnam,str)){
            cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j)\
            << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
            this->promote(id,1);
         }
      }
   }
   delete [] ord;
}

void Focus::promote_subs_prom(const char *str){
   long i,j,k,*ord,id;
   char cnam[max_str];

   j=0;
   for(i=0;i<msc;i++){
      id=*(msa+i);
      if(*(msm+i)>0){
         pSaa->disk_ifind(id,cnam);
         if(strstr(cnam,str)){
            k=0;
            while((k<mtc)&&(*(mti+k)!=id))k++;
            if(k<mtc){
               j++;
               cout << j << "\t" << id << "\t" << *(alp+k) << " " << *(weg+k)\
               << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
               if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
            }
         }
      }
   }
}

void Focus::promote_subs_demt(const char *str){
   long i,j,k,*ord,id;
   char cnam[max_str];

   j=0;
   for(i=0;i<msc;i++){
      id=*(msa+i);
      if(*(msm+i)<0){
         pSaa->disk_ifind(id,cnam);
         if(strstr(cnam,str)){
            j++;
            cout << j << "\t" << id << "\t" \
            << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
            if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
         }
      }
   }
}

void Focus::Initiator(long tnm,long sn,Index *pInd){
   if(pInd->ix>count_lim){
      err_flag=1;
      return;
   }
   else err_flag=0;
   if((pInd==NULL)||(pInd->ix==0)){mt=0;return;}
   this->restart();
   stn=sn;
   this->mem_Terms(tnm);
   this->mem_Docms(pRgg->ntot);
   if(bflag){this->transfer();bflag=0;}
   this->zeros();
   this->init_Full(pInd->ix,pInd->idx);
   if(!flim)this->weightApos();
   else this->weightApos(flim);
}

void Focus::Initiator_s(long tnm,long sn,Index *pInd){
   if((pInd==NULL)||(pInd->ix==0)){mt=0;return;}
   if(!bflag){cout << "Error, background not called!" << endl;exit(0);}
   this->restart();
   stn=sn;
   this->mem_Terms(tnm);
   Index *jind=new Index;
   jind->ix=ix;
   jind->idx=idx;
   Bool Bl;
   Index *kind=Bl.cbool_And(jind,pInd);
   if((kind==NULL)||(kind->ix==0)){mt=0;return;}
   long i,j=0;
   for(i=0;i<md;i++)mdi[i]=0;
   mdc=0;
   i=0;
   while((i<ix)&&(j<kind->ix)){
      while(*(idx+i)<*(kind->idx+j))i++;
      if((i<ix)&&(*(idx+i)==*(kind->idx+j))){*(mdi+i)=1;j++;mdc++;}
      while((j<kind->ix)&&(*(kind->idx+j)<*(idx+i)))j++;
   }
   if(kind->ix>count_lim){
      err_flag=1;
      return;
   }
   else err_flag=0;
   zeros();
   countSub(kind->ix,kind->idx);
   delete kind;
   jind->idx=NULL;
   delete jind;
   if(!flim)weightApos();
   else weightApos(flim);
}

void Focus::Initiator(Theme *pThm,long bfl){
   long i,id;
   if(mti!=NULL)delete [] mti;
   if(weg!=NULL)delete [] weg;
   if(alp!=NULL)delete [] alp;
   if(cst!=NULL)delete [] cst;
   if(msa!=NULL)delete [] msa;
   if(msm!=NULL)delete [] msm;
   mt=pThm->mt;
   mtc=pThm->mtc;
   ms=pThm->ms;
   msc=pThm->msc;
   mti=new long[mt];
   weg=new float[mt];
   alp=new float[mt];
   cst=new float[mt];
   msa=new long[ms];
   msm=new long[ms];
   for(i=0;i<mtc;i++){
      *(mti+i)=*(pThm->mti+i);
      *(weg+i)=*(pThm->weg+i);
      *(alp+i)=*(pThm->alp+i);
   }
   for(i=0;i<msc;i++){
      id=*(pThm->msa+i);
      if(id<0){
         *(msa+i)=-id;
         *(msm+i)=-1;
      }
      else {
         *(msa+i)=id;
         *(msm+i)=1;
      }
   }
   stn=pThm->stn;
   cs=pThm->cs;
   this->set_Dlim();
   if((!bfl)&&bflag){
      this->transfer();
      this->mem_Docms(pRgg->ntot);
      bflag=0;
   }
   else if(bfl&&(!bflag)){cout << "Error in flags!" << endl;exit(0);}
   else {
      for(i=0;i<md;i++)mdi[i]=0;
   }
   mdc=0;
   this->zeros();
}

void Focus::Iterator(long tnm,long sn){
   if(mt==0)return;
   stn=sn;
   this->scoreAll();
   this->set_Dlim();
   this->scoreCnt();
   if(mdc>count_lim){
      err_flag=1;
      return;
   }
   else err_flag=0;
   this->countUpdate();
   if(tnm!=mt)mem_Terms(tnm);
   if(!flim)this->weightApos();
   else this->weightApos(flim);
}

void Focus::Iterator_s(long tnm,long sn){
   if(mt==0)return;
   stn=sn;
   this->scoreAll();
   this->set_Dlim();
   this->scoreCnt_Sub();
   if(mdc>count_lim){
      err_flag=1;
      return;
   }
   else err_flag=0;
   this->countUpidx();
   if(tnm!=mt)mem_Terms(tnm);
   if(!flim)this->weightApos();
   else this->weightApos(flim);
}

void Focus::Background_s(Index *pInd){
   if(pInd->ix>count_lim){
      err_flag=1;
      return;
   }
   else err_flag=0;
   this->mem_Docms(pInd->ix);
   ix=pInd->ix;
   if(idx!=NULL)delete [] idx;
   idx=new long[ix];
   long i;
   for(i=0;i<ix;i++)idx[i]=(pInd->idx)[i];
   zerot();
   countTot(ix,idx);
   bflag=1;
}

Theme *Focus::Save(void){
   if(!mtc)return(NULL);
   long i;
   Theme *qThm=new Theme;
   qThm->mt=mt;
   qThm->mtc=mtc;
   qThm->alp=alp;
   qThm->weg=weg;
   qThm->ms=ms;
   qThm->msc=msc;
   qThm->mti=mti;
   for(i=0;i<msc;i++){
      if(*(msm+i)<0)*(msa+i)=-*(msa+i);
   }
   qThm->msa=msa;
   qThm->stn=stn;
   qThm->cs=cs;
  
   delete [] msm;
   delete [] cst;
   mt=0;
   mtc=0;
   ms=0;
   msc=0;
   mti=NULL;
   weg=NULL;
   alp=NULL;
   cst=NULL;
   msa=NULL;
   msm=NULL;
   return(qThm);
}

void Focus::Write(Theme *pThm){
   long i,j,id;
   char cnam[max_str],bnam[max_str],c;
   
   cout << "Enter file extension:" << endl;
   while(cin.peek()=='\n')cin.get(c);
   j=0;
   while((cin.get(c))&&(c!='\n'))cnam[j++]=c;
   cnam[j]='\0';
   cout << "File extension entered: " << cnam << endl;
   get_pathw(bnam,"focus",name,cnam);
   ofstream fout(bnam,ios::out);
   
   fout << pThm->mt << endl;
   fout << pThm->mtc << endl;
   for(i=0;i<pThm->mtc;i++){
      pSaa->disk_ifind(*(pThm->mti+i),cnam);
      fout << *(pThm->alp+i) << endl;
      fout << *(pThm->weg+i) << endl;
      fout << cnam << endl;
   }
      
   fout << pThm->ms << endl;
   fout << pThm->msc << endl;
   for(i=0;i<pThm->msc;i++){
      id=*(pThm->msa+i);
      if(id<0){
         j=-1;
         pSaa->disk_ifind(-id,cnam);
      }
      else {
         j=1;
         pSaa->disk_ifind(id,cnam);
      }
      fout << j << endl;
      fout << cnam << endl;
   }
   fout << pThm->stn << endl;
   fout << pThm->cs << endl;
}

Theme *Focus::Read(void){
   long i,j,id,*ord;
   char cnam[max_str],bnam[max_str],c;

   cout << "Enter file extension:" << endl;
   while(cin.peek()=='\n')cin.get(c);
   j=0;
   while((cin.get(c))&&(c!='\n'))cnam[j++]=c;
   cnam[j]='\0';
   cout << "File extension entered: " << cnam << endl;
   get_pathw(bnam,"focus",name,cnam);
   ifstream fin(bnam,ios::in);
  
   Theme *pThm=new Theme;
   fin >> pThm->mt;
   fin >> pThm->mtc;
   pThm->mti=new long[pThm->mtc];
   pThm->alp=new float[pThm->mtc];
   pThm->weg=new float[pThm->mtc];

   for(i=0;i<pThm->mtc;i++){
      fin >> *(pThm->alp+i);
      fin >> *(pThm->weg+i);
      while(isspace(fin.peek()))fin.get(c);
      fin.get(cnam,max_str);
      id=pSaa->disk_sifind(cnam);
      if(id)*(pThm->mti+i)=id;
      else {
         cout << "Error! Unable to find term " << cnam << " in term list." << endl;
         return(NULL);
      }
   }
     
   fin >> pThm->ms;
   fin >> pThm->msc;
   pThm->msa=new long[pThm->msc];
   ord=new long[pThm->msc];

   for(i=0;i<pThm->msc;i++){
      fin >> j;
      while(isspace(fin.peek()))fin.get(c);
      fin.get(cnam,max_str);
      id=pSaa->disk_sifind(cnam);
      if(id){
         *(pThm->msa+i)=j*id;
         ord[i]=id;  
      }
      else {
         cout << "Error! Unable to find term " << cnam << " in term list." << endl;
         return(NULL);
      }
   }
   hSort(pThm->msc,ord,pThm->msa);
   delete [] ord;
   fin >> pThm->stn;
   fin >> pThm->cs;
   return(pThm);
}

void Focus::Write(ofstream &fout,Theme *pThm){
   long i,j,id;
   char cnam[max_str],bnam[max_str],c;

   fout << pThm->mt << endl;
   fout << pThm->mtc << endl;
   for(i=0;i<pThm->mtc;i++){
      pSaa->disk_ifind(*(pThm->mti+i),cnam);
      fout << *(pThm->alp+i) << endl;
      fout << *(pThm->weg+i) << endl;
      fout << cnam << endl;
   }

   fout << pThm->ms << endl;
   fout << pThm->msc << endl;
   for(i=0;i<pThm->msc;i++){
      id=*(pThm->msa+i);
      if(id<0){
         j=-1;
         pSaa->disk_ifind(-id,cnam);
      }
      else {
         j=1;
         pSaa->disk_ifind(id,cnam);
      }
      fout << j << endl;
      fout << cnam << endl;
   }
   fout << pThm->stn << endl;
   fout << pThm->cs << endl;
}

Theme *Focus::Read(ifstream &fin){
   long i,j,id,*ord;
   char cnam[max_str],bnam[max_str],c;

   Theme *pThm=new Theme;
   fin >> pThm->mt;
   fin >> pThm->mtc;
   pThm->mti=new long[pThm->mtc];
   pThm->alp=new float[pThm->mtc];
   pThm->weg=new float[pThm->mtc];

   for(i=0;i<pThm->mtc;i++){
      fin >> *(pThm->alp+i);
      fin >> *(pThm->weg+i);
      while(isspace(fin.peek()))fin.get(c);
      fin.get(cnam,max_str);
      id=pSaa->disk_sifind(cnam);
      if(id)*(pThm->mti+i)=id;
      else {
         cout << "Error! Unable to find term " << cnam << " in term list." << endl;
         return(NULL);
      }
   }

   fin >> pThm->ms;
   fin >> pThm->msc;
   pThm->msa=new long[pThm->msc];
   ord=new long[pThm->msc];

   for(i=0;i<pThm->msc;i++){
      fin >> j;
      while(isspace(fin.peek()))fin.get(c);
      fin.get(cnam,max_str);
      id=pSaa->disk_sifind(cnam);
      if(id){
         *(pThm->msa+i)=j*id;
         ord[i]=id;
      }
      else {
         cout << "Error! Unable to find term " << cnam << " in term list." << endl;
         return(NULL);
      }
   }
   hSort(pThm->msc,ord,pThm->msa);
   delete [] ord;
   fin >> pThm->stn;
   fin >> pThm->cs;
   return(pThm);
}

//Multithreading version

   extern unsigned num_thr; //Number of threads allowed in process.
   extern pthread_t *tid; //Threads (num_thr);
   extern Th_data **pTd; //Array of thread arguments (num_thr).
   extern sem_t smp; //Semaphore.
   extern int *add; //Array num_thr long for controlling access to Td[].
   extern int bot; //first available index
   extern int top; //first empty spot
   extern pthread_mutex_t lbot; //Lock for bot variable
   extern pthread_mutex_t ltop; //Lock for top variable

   static pthread_attr_t thread_attr; //Attribute object for threads
   static pthread_cond_t conda; //Condition varialble for end.
   static pthread_mutex_t lsco; //lock for sco updating
   static pthread_mutex_t lther; //lock for thread counting
   static int ther; //Number of access threads deployed in this class
   static long snm; //Number of terms in a doc
   static long *xnm; //List of term ids
   static float *wnm; //List of term weights
   static long cnm; //Term process counter
   static pthread_mutex_t lcnm; //Lock for term process counter

   //Regist variables
   extern unsigned num_thb; //Number of threads allowed in process.
   extern pthread_t *tib; //Threads (num_thb);
   extern Tb_data **pTb; //Array of thread arguments (num_thb).
   extern sem_t smb; //Semaphore.
   extern int *adb; //Array num_thb long for controlling access to Tb[].
   extern int bbt; //first available index
   extern int tbp; //first empty spot
   extern pthread_mutex_t lbbt; //Lock for bbt variable
   extern pthread_mutex_t ltbp; //Lock for tbp variable

   static pthread_cond_t condb; //Condition varialble for end.
   static pthread_mutex_t lcnt; //lock for tx count updating
   static pthread_mutex_t lcns; //lock for sx count updating
   static pthread_mutex_t ltber; //lock for thread counting
   static int tber; //Number of register threads deployed in this class

extern "C" void *tscore(void *pth_data){
   unsigned char c,*utr;
   long nt,yi,od,fq,jfq,cn,i,j,ncl;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;
   long flag_end,ix,*pz;
   float yy,*yt,**mt,wx,*sz;

   Th_data *ptd=(Th_data *)pth_data;
   flag_end=1;
   do {
      pthread_mutex_lock(&lcnm);
      if(cnm<snm){
         pz=xnm+cnm;
         sz=wnm+cnm;
         while((cnm<snm)&&((!(*(sz)))||(!(*(pz))))){sz++;pz++;cnm++;}
         if(cnm<snm){
            ix=*(xnm+cnm)-1;
            wx=*(wnm+(cnm++));
         }
         else flag_end=0;
      }
      else flag_end=0;
      pthread_mutex_unlock(&lcnm);
      if(flag_end){
         pk=(ptd->pSaa)->read_pthr(ix,&cn,ptd->xch);

         fq=*((ptd->pSaa)->freq+ix);
         ba=(ptd->pSaa)->pw[pk];
         utr=ptd->xch;
         od=-1;
         yi=0;
         sg=0;
         cu1=0;
         i=0;
         mi=0;
         while(i<fq){
            j=0;
            jfq=(i+100000<=fq)?100000:fq-i;
         while(j<jfq){
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
                    mi+=c*((ptd->pSaa)->pw[bc-8]);
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
                       ptd->ad[j]=(ptd->sco)+mi;
                       j++;
                       od=mi;
                       mi=0;
                    }
                 }
                 break;
            }
            }
            pthread_mutex_lock(&lsco);
            mt=ptd->ad;
            for(j=0;j<jfq;j++)(**(mt++))+=wx;
            pthread_mutex_unlock(&lsco);
            i+=jfq;
         }
      }
   }while(flag_end);

   pthread_mutex_lock(&ltop);
   add[top%num_thr]=ptd->iz;
   top++;
   pthread_mutex_unlock(&ltop);
   pthread_mutex_lock(&lther);
   ther--;
   pthread_cond_signal(&conda);
   pthread_mutex_unlock(&lther);
   sem_post(&smp);
   return(NULL);
}

extern "C" void *fcnt_t(void *pth_data){
   long i,j,k,bb,*ad,num;
   long u,nu,*pu,**mt,*pz;
   Tb_data *ptd=(Tb_data *)pth_data;
   i=ptd->ii;
   ifstream *pfln=(ptd->pRgg)->pfin+i;
   bb=((ptd->pRgg)->pBn)->mm[i];
   ad=(ptd->pRgg)->offs[i];

   pthread_mutex_lock(&((ptd->pRgg)->acc[i]));
   j=0;
   pz=ptd->nmx;
   nu=0;
   while((j++)<ptd->nm){
      k=*(pz++)-bb;
      pfln->seekg(*(ad+k),ios::beg);
      pfln->read((char*)&num,sizeof(long));
      pfln->read((char*)(ptd->idn),sizeof(long)*num);
      if(nu+num<=50000){
         pu=ptd->idn;
         for(u=0;u<num;u++)*((ptd->ptx)+(nu++))=(ptd->tx)+*(pu++);
      }
      else {
         mt=ptd->ptx;
         pthread_mutex_lock(&lcnt);
         for(u=0;u<nu;u++)(**(mt++))++;
         pthread_mutex_unlock(&lcnt);
         nu=0;
         pu=ptd->idn;
         for(u=0;u<num;u++)*((ptd->ptx)+(nu++))=(ptd->tx)+*(pu++);
      }
   }
   mt=ptd->ptx;
   pthread_mutex_lock(&lcnt);
   for(u=0;u<nu;u++)(**(mt++))++;
   pthread_mutex_unlock(&lcnt);
   pthread_mutex_unlock(&((ptd->pRgg)->acc[i]));

   pthread_mutex_lock(&ltbp);
   adb[tbp%num_thb]=ptd->iz;
   tbp++;
   pthread_mutex_unlock(&ltbp);
   pthread_mutex_lock(&ltber);
   tber--;
   pthread_cond_signal(&condb);
   pthread_mutex_unlock(&ltber);
   sem_post(&smb);
   return((void*)ptd);
}

extern "C" void *fcnt_s(void *pth_data){
   long i,j,k,bb,*ad,num;
   long u,nu,*pu,**mt,*pz;
   Tb_data *ptd=(Tb_data *)pth_data;
   i=ptd->ii;
   ifstream *pfln=(ptd->pRgg)->pfin+i;
   bb=((ptd->pRgg)->pBn)->mm[i];
   ad=(ptd->pRgg)->offs[i];

   pthread_mutex_lock(&((ptd->pRgg)->acc[i]));
   j=0;
   pz=ptd->nmx;
   nu=0;
   while((j++)<ptd->nm){
      k=*(pz++)-bb;
      pfln->seekg(*(ad+k),ios::beg);
      pfln->read((char*)&num,sizeof(long));
      pfln->read((char*)(ptd->idn),sizeof(long)*num);
      if(nu+num<=50000){
         pu=ptd->idn;
         for(u=0;u<num;u++)*((ptd->psx)+(nu++))=(ptd->sx)+*(pu++);
      }
      else {
         mt=ptd->psx;
         pthread_mutex_lock(&lcns);
         for(u=0;u<nu;u++)(**(mt++))++;
         pthread_mutex_unlock(&lcns);
         nu=0;
         pu=ptd->idn;
         for(u=0;u<num;u++)*((ptd->psx)+(nu++))=(ptd->sx)+*(pu++);
      }
   }
   mt=ptd->psx;
   pthread_mutex_lock(&lcns);
   for(u=0;u<nu;u++)(**(mt++))++;
   pthread_mutex_unlock(&lcns);
   pthread_mutex_unlock(&((ptd->pRgg)->acc[i]));

   pthread_mutex_lock(&ltbp);
   adb[tbp%num_thb]=ptd->iz;
   tbp++;
   pthread_mutex_unlock(&ltbp);
   pthread_mutex_lock(&ltber);
   tber--;
   pthread_cond_signal(&condb);
   pthread_mutex_unlock(&ltber);
   sem_post(&smb);
   return((void*)ptd);
}

extern "C" void *fcnt_b(void *pth_data){
   long i,j,k,bb,*ad,num;
   long u,nu,*pu,**mt,**ms,*pz;
   Tb_data *ptd=(Tb_data *)pth_data;
   i=ptd->ii;
   ifstream *pfln=(ptd->pRgg)->pfin+i;
   bb=((ptd->pRgg)->pBn)->mm[i];
   ad=(ptd->pRgg)->offs[i];

   pthread_mutex_lock(&((ptd->pRgg)->acc[i]));
   j=0;
   pz=ptd->nmx;
   nu=0;
   while((j++)<ptd->nm){
      k=*(pz++)-bb;
      pfln->seekg(*(ad+k),ios::beg);
      pfln->read((char*)&num,sizeof(long));
      pfln->read((char*)(ptd->idn),sizeof(long)*num);
      if(nu+num<=50000){
         pu=ptd->idn;
         for(u=0;u<num;u++){
            *((ptd->ptx)+nu)=(ptd->tx)+*pu;
            *((ptd->psx)+(nu++))=(ptd->sx)+*(pu++);
         }
      }
      else {
         mt=ptd->ptx;
         ms=ptd->psx;
         pthread_mutex_lock(&lcnt);
         for(u=0;u<nu;u++)(**(mt++))++;
         pthread_mutex_unlock(&lcnt);
         pthread_mutex_lock(&lcns);
         for(u=0;u<nu;u++)(**(ms++))++;
         pthread_mutex_unlock(&lcns);
         nu=0;
         pu=ptd->idn;
         for(u=0;u<num;u++){
            *((ptd->ptx)+nu)=(ptd->tx)+*pu;
            *((ptd->psx)+(nu++))=(ptd->sx)+*(pu++);
         }
      }
   }
   mt=ptd->ptx;
   ms=ptd->psx;
   pthread_mutex_lock(&lcnt);
   for(u=0;u<nu;u++)(**(mt++))++;
   pthread_mutex_unlock(&lcnt);
   pthread_mutex_lock(&lcns);
   for(u=0;u<nu;u++)(**(ms++))++;
   pthread_mutex_unlock(&lcns);
   pthread_mutex_unlock(&((ptd->pRgg)->acc[i]));

   pthread_mutex_lock(&ltbp);
   adb[tbp%num_thb]=ptd->iz;
   tbp++;
   pthread_mutex_unlock(&ltbp);
   pthread_mutex_lock(&ltber);
   tber--;
   pthread_cond_signal(&condb);
   pthread_mutex_unlock(&ltber);
   sem_post(&smb);
   return((void*)ptd);
}

extern "C" void *fncnt_t(void *pth_data){
   long i,j,k,bb,*ad,num;
   long u,nu,*pu,**mt,*pz;
   Tb_data *ptd=(Tb_data *)pth_data;
   i=ptd->ii;
   ifstream *pfln=(ptd->pRgg)->pfin+i;
   bb=((ptd->pRgg)->pBn)->mm[i];
   ad=(ptd->pRgg)->offs[i];

   pthread_mutex_lock(&((ptd->pRgg)->acc[i]));
   j=0;
   pz=ptd->nmx;
   nu=0;
   while((j++)<ptd->nm){
      k=*(pz++)-bb;
      pfln->seekg(*(ad+k),ios::beg);
      pfln->read((char*)&num,sizeof(long));
      pfln->read((char*)(ptd->idn),sizeof(long)*num);
      if(nu+num<=50000){
         pu=ptd->idn;
         for(u=0;u<num;u++)*((ptd->ptx)+(nu++))=(ptd->tx)+*(pu++);
      }
      else {
         mt=ptd->ptx;
         pthread_mutex_lock(&lcnt);
         for(u=0;u<nu;u++)(**(mt++))--;
         pthread_mutex_unlock(&lcnt);
         nu=0;
         pu=ptd->idn;
         for(u=0;u<num;u++)*((ptd->ptx)+(nu++))=(ptd->tx)+*(pu++);
      }
   }
   mt=ptd->ptx;
   pthread_mutex_lock(&lcnt);
   for(u=0;u<nu;u++)(**(mt++))--;
   pthread_mutex_unlock(&lcnt);
   pthread_mutex_unlock(&((ptd->pRgg)->acc[i]));

   pthread_mutex_lock(&ltbp);
   adb[tbp%num_thb]=ptd->iz;
   tbp++;
   pthread_mutex_unlock(&ltbp);
   pthread_mutex_lock(&ltber);
   tber--;
   pthread_cond_signal(&condb);
   pthread_mutex_unlock(&ltber);
   sem_post(&smb);
   return((void*)ptd);
}

extern "C" void *fncnt_s(void *pth_data){
   long i,j,k,bb,*ad,num;
   long u,nu,*pu,**mt,*pz;
   Tb_data *ptd=(Tb_data *)pth_data;
   i=ptd->ii;
   ifstream *pfln=(ptd->pRgg)->pfin+i;
   bb=((ptd->pRgg)->pBn)->mm[i];
   ad=(ptd->pRgg)->offs[i];

   pthread_mutex_lock(&((ptd->pRgg)->acc[i]));
   j=0;
   pz=ptd->nmx;
   nu=0;
   while((j++)<ptd->nm){
      k=*(pz++)-bb;
      pfln->seekg(*(ad+k),ios::beg);
      pfln->read((char*)&num,sizeof(long));
      pfln->read((char*)(ptd->idn),sizeof(long)*num);
      if(nu+num<=50000){
         pu=ptd->idn;
         for(u=0;u<num;u++)*((ptd->psx)+(nu++))=(ptd->sx)+*(pu++);
      }
      else {
         mt=ptd->psx;
         pthread_mutex_lock(&lcns);
         for(u=0;u<nu;u++)(**(mt++))--;
         pthread_mutex_unlock(&lcns);
         nu=0;
         pu=ptd->idn;
         for(u=0;u<num;u++)*((ptd->psx)+(nu++))=(ptd->sx)+*(pu++);
      }
   }
   mt=ptd->psx;
   pthread_mutex_lock(&lcns);
   for(u=0;u<nu;u++)(**(mt++))--;
   pthread_mutex_unlock(&lcns);
   pthread_mutex_unlock(&((ptd->pRgg)->acc[i]));

   pthread_mutex_lock(&ltbp);
   adb[tbp%num_thb]=ptd->iz;
   tbp++;
   pthread_mutex_unlock(&ltbp);
   pthread_mutex_lock(&ltber);
   tber--;
   pthread_cond_signal(&condb);
   pthread_mutex_unlock(&ltber);
   sem_post(&smb);
   return((void*)ptd);
}

extern "C" void *fncnt_b(void *pth_data){
   long i,j,k,bb,*ad,num;
   long u,nu,*pu,**mt,**ms,*pz;
   Tb_data *ptd=(Tb_data *)pth_data;
   i=ptd->ii;
   ifstream *pfln=(ptd->pRgg)->pfin+i;
   bb=((ptd->pRgg)->pBn)->mm[i];
   ad=(ptd->pRgg)->offs[i];

   pthread_mutex_lock(&((ptd->pRgg)->acc[i]));
   j=0;
   pz=ptd->nmx;
   nu=0;
   while((j++)<ptd->nm){
      k=*(pz++)-bb;
      pfln->seekg(*(ad+k),ios::beg);
      pfln->read((char*)&num,sizeof(long));
      pfln->read((char*)(ptd->idn),sizeof(long)*num);
      if(nu+num<=50000){
         pu=ptd->idn;
         for(u=0;u<num;u++){
            *((ptd->ptx)+nu)=(ptd->tx)+*pu;
            *((ptd->psx)+(nu++))=(ptd->sx)+*(pu++);
         }
      }
      else {
         mt=ptd->ptx;
         ms=ptd->psx;
         pthread_mutex_lock(&lcnt);
         for(u=0;u<nu;u++)(**(mt++))--;
         pthread_mutex_unlock(&lcnt);
         pthread_mutex_lock(&lcns);
         for(u=0;u<nu;u++)(**(ms++))--;
         pthread_mutex_unlock(&lcns);
         nu=0;
         pu=ptd->idn;
         for(u=0;u<num;u++){
            *((ptd->ptx)+nu)=(ptd->tx)+*pu;
            *((ptd->psx)+(nu++))=(ptd->sx)+*(pu++);
         }
      }
   }
   mt=ptd->ptx;
   ms=ptd->psx;
   pthread_mutex_lock(&lcnt);
   for(u=0;u<nu;u++)(**(mt++))--;
   pthread_mutex_unlock(&lcnt);
   pthread_mutex_lock(&lcns);
   for(u=0;u<nu;u++)(**(ms++))--;
   pthread_mutex_unlock(&lcns);
   pthread_mutex_unlock(&((ptd->pRgg)->acc[i]));

   pthread_mutex_lock(&ltbp);
   adb[tbp%num_thb]=ptd->iz;
   tbp++;
   pthread_mutex_unlock(&ltbp);
   pthread_mutex_lock(&ltber);
   tber--;
   pthread_cond_signal(&condb);
   pthread_mutex_unlock(&ltber);
   sem_post(&smb);
   return((void*)ptd);
}

Focus_pth::Focus_pth(const char *nam) {
   int lxn=strlen(nam);
   name=new char[lxn+1];
   strcpy(name,nam);

   tx=NULL;
   sx=NULL;
   weg=NULL;
   alp=NULL;
   cst=NULL;
   mti=NULL;
   sco=NULL;
   mdi=NULL;
   idx=NULL;
   msa=NULL;
   msm=NULL;
   pRgg=NULL;
   pSaa=NULL;
   
   mt=0;
   md=0;
   ms=0;
   dlim=0;
   stn=0;
 
   bflag=1;
   err_flag=0;
   count_lim=200000;
}

Focus_pth::Focus_pth(const char *nam,long lim) {
   int lxn=strlen(nam);
   name=new char[lxn+1];
   strcpy(name,nam);

   tx=NULL;
   sx=NULL;
   weg=NULL;
   alp=NULL;
   cst=NULL;
   mti=NULL;
   sco=NULL;
   mdi=NULL;
   idx=NULL;
   msa=NULL;
   msm=NULL;
   pRgg=NULL;
   pSaa=NULL;

   mt=0;
   md=0;
   ms=0;
   dlim=0;
   stn=0;

   bflag=1;
   err_flag=0;
   count_lim=lim;
}

Focus_pth::~Focus_pth(){
   if (tx!=NULL)delete [] tx;
   if (sx!=NULL)delete [] sx;
   if (weg!=NULL)delete [] weg;
   if (alp!=NULL)delete [] alp;
   if (cst!=NULL)delete [] cst;
   if (mti!=NULL)delete [] mti;
   if (sco!=NULL)delete [] sco;
   if (mdi!=NULL)delete [] mdi;
   if (idx!=NULL)delete [] idx;
   if (msa!=NULL)delete [] msa;
   if (msm!=NULL)delete [] msm;
}

void Focus_pth::gopen_Focus_pth(Regist_pth *pReg,Slice_Accpth *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

   pRgg=pReg;
   pSaa=pSac;

   pSaa->inv_load();
   pSaa->post_access();
   pSaa->gopen_idstring(pSaa->slice_name);
   nid=pSaa->nid;

   tx=new long[nid];
   sx=new long[nid];
   ms=100;
   msc=0;
   msa=new long[ms];
   msm=new long[ms];
   
   if(pflag)cout << "Opening document files" << endl;
   pRgg->set_class();
   pRgg->gopen_read(READ_U);
   pRgg->gopen_binary_pth();

   //Multithreading setup
   if(pthread_mutex_init(&lsco,NULL)){
      cout << "Error in lsco mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lther,NULL)){
      cout << "Error in lther mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_cond_init(&conda,NULL)){
      cout << "Error in conda initialization!" << endl;
      exit(0);
   }
   //Regist variables for counting
   if(pthread_mutex_init(&lcnt,NULL)){
      cout << "Error in lcnt mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lcns,NULL)){
      cout << "Error in lcns mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&ltber,NULL)){
      cout << "Error in ltber mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_cond_init(&condb,NULL)){
      cout << "Error in condb initialization!" << endl;
      exit(0);
   }
   pthread_attr_init(&thread_attr);
   pthread_attr_setdetachstate(&thread_attr, PTHREAD_CREATE_DETACHED);
   pthread_attr_setscope(&thread_attr, PTHREAD_SCOPE_SYSTEM);
   pthread_attr_setinheritsched(&thread_attr, PTHREAD_EXPLICIT_SCHED);
}

void Focus_pth::gopen_Focus_map(Regist_pth *pReg,Slice_Accpth *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

   pRgg=pReg;
   pSaa=pSac;

   pSaa->inv_map();
   pSaa->post_access_map();
   pSaa->gopen_idstring(pSaa->slice_name);
   nid=pSaa->nid;

   tx=new long[nid];
   sx=new long[nid];
   ms=100;
   msc=0;
   msa=new long[ms];
   msm=new long[ms];
  
   if(pflag)cout << "Opening document files" << endl;
   pRgg->set_class();
   pRgg->gopen_map(READ_U);
   pRgg->gopen_binary_map();

   //Multithreading setup
   if(pthread_mutex_init(&lsco,NULL)){
      cout << "Error in lsco mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lther,NULL)){
      cout << "Error in lther mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_cond_init(&conda,NULL)){
      cout << "Error in conda initialization!" << endl;
      exit(0);
   }
   //Regist variables for counting
   if(pthread_mutex_init(&lcnt,NULL)){
      cout << "Error in lcnt mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lcns,NULL)){
      cout << "Error in lcns mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&ltber,NULL)){
      cout << "Error in ltber mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_cond_init(&condb,NULL)){
      cout << "Error in condb initialization!" << endl;
      exit(0);
   }
   pthread_attr_init(&thread_attr);
   pthread_attr_setdetachstate(&thread_attr, PTHREAD_CREATE_DETACHED);
   pthread_attr_setscope(&thread_attr, PTHREAD_SCOPE_SYSTEM);
   pthread_attr_setinheritsched(&thread_attr, PTHREAD_EXPLICIT_SCHED);
}

void Focus_pth::mem_Terms(long num){
   if((mt)&&(mt!=num)){
      delete [] mti;
      delete [] weg;
      delete [] alp;
      delete [] cst;
   }
   mt=num;
   mti=new long[mt];
   weg=new float[mt];
   alp=new float[mt];
   cst=new float[mt];
}

void Focus_pth::mem_Docms(long num){
   long i;
   if(mdi!=NULL){
      delete [] mdi;
   }
   md=num;
   mdi=new long[md];
   for(i=0;i<md;i++)*(mdi+i)=0;
}

void Focus_pth::restart(void){
   msc=0;
   mtc=0;
}

void Focus_pth::zerot(void){
   long i,*pt;
   nnx=0;
   pt=tx;
   for(i=0;i<nid;i++)*(pt++)=0;
}

void Focus_pth::zeros(void){
   long i,*ps;
   nsx=0;
   ps=sx;
   for(i=0;i<nid;i++)*(ps++)=0;
}

void Focus_pth::transfer(void){
   long i,j,*pt,*pz;
   nnx=pRgg->ntot;
   pz=pSaa->inv+1;
   pt=tx+1;
   for(i=1;i<nid;i++){
      if(j=*(pz++)){
         j--;
         *(pt++)=*(pSaa->freq+j);
      }
      else *(pt++)=0;
   }
}

void Focus_pth::countTot(long ik,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ik==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ik-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,fcnt_t,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nnx+=v;
            if(pflag)cout << "thread f countTot " << i << endl;
         }
         else {sem_post(&smb);cout << "Thread(b) launch in BnBayes failed!" << endl;}
         pthread_mutex_unlock(&lbbt);
         pthread_mutex_unlock(&ltber);
      }
      else i++;
   }
   flag_end=1;
   do {
      pthread_mutex_lock(&ltber);
      if(tber)pthread_cond_wait(&condb,&ltber);
      else flag_end=0;
      pthread_mutex_unlock(&ltber);
   }while(flag_end);
}

void Focus_pth::countSub(long ik,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ik==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ik-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,fcnt_s,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nsx+=v;
            if(pflag)cout << "thread f countSub " << i << endl;
         }
         else {sem_post(&smb);cout << "Thread(b) launch in BnBayes failed!" << endl;}
         pthread_mutex_unlock(&lbbt);
         pthread_mutex_unlock(&ltber);
      }
      else i++;
   }
   flag_end=1;
   do {
      pthread_mutex_lock(&ltber);
      if(tber)pthread_cond_wait(&condb,&ltber);
      else flag_end=0;
      pthread_mutex_unlock(&ltber);
   }while(flag_end);
}

void Focus_pth::countBth(long ik,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ik==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ik-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,fcnt_b,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nnx+=v;nsx+=v;
            if(pflag)cout << "thread f countBth " << i << endl;
         }
         else {sem_post(&smb);cout << "Thread(b) launch in BnBayes failed!" << endl;}
         pthread_mutex_unlock(&lbbt);
         pthread_mutex_unlock(&ltber);
      }
      else i++;
   }
   flag_end=1;
   do {
      pthread_mutex_lock(&ltber);
      if(tber)pthread_cond_wait(&condb,&ltber);
      else flag_end=0;
      pthread_mutex_unlock(&ltber);
   }while(flag_end);
}

void Focus_pth::ncountTot(long ik,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ik==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ik-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,fncnt_t,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nnx-=v;
            if(pflag)cout << "thread f ncountTot " << i << endl;
         }
         else {sem_post(&smb);cout << "Thread(b) launch in BnBayes failed!" << endl;}
         pthread_mutex_unlock(&lbbt);
         pthread_mutex_unlock(&ltber);
      }
      else i++;
   }
   flag_end=1;
   do {
      pthread_mutex_lock(&ltber);
      if(tber)pthread_cond_wait(&condb,&ltber);
      else flag_end=0;
      pthread_mutex_unlock(&ltber);
   }while(flag_end);
}

void Focus_pth::ncountSub(long ik,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ik==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ik-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,fncnt_s,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nsx-=v;
            if(pflag)cout << "thread f ncountSub " << i << endl;
         }
         else {sem_post(&smb);cout << "Thread(b) launch in BnBayes failed!" << endl;}
         pthread_mutex_unlock(&lbbt);
         pthread_mutex_unlock(&ltber);
      }
      else i++;
   }
   flag_end=1;
   do {
      pthread_mutex_lock(&ltber);
      if(tber)pthread_cond_wait(&condb,&ltber);
      else flag_end=0;
      pthread_mutex_unlock(&ltber);
   }while(flag_end);
}

void Focus_pth::ncountBth(long ik,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ik==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ik-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,fncnt_b,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nnx-=v;nsx-=v;
            if(pflag)cout << "thread f ncountBth " << i << endl;
         }
         else {sem_post(&smb);cout << "Thread(b) launch in BnBayes failed!" << endl;}
         pthread_mutex_unlock(&lbbt);
         pthread_mutex_unlock(&ltber);
      }
      else i++;
   }
   flag_end=1;
   do {
      pthread_mutex_lock(&ltber);
      if(tber)pthread_cond_wait(&condb,&ltber);
      else flag_end=0;
      pthread_mutex_unlock(&ltber);
   }while(flag_end);
}

void Focus_pth::weightApos(void){
   long n_t,n_st,flag,min=0,trg=1;
   long i,j,mnn,iu,fneg,id;
   double xx,yy,zz,frc,pt,qt,rt;
   float uu;
   char cnam[max_str];

   if(mt==0){mtc=0;return;}
   if(nsx==0){mtc=0;return;}
   frc=((double)nsx)/((double)nnx);
   tlim=0;
   mtc=0;
   if(msc){ //Find initial term id.
     iu=0;
     id=*msa;
   }
   else id=0;

   for(i=1;i<nid;i++){
      if(i==id){
         yy=*(msm+iu)*1000000.0;
         iu++;
         if(iu<msc)id=*(msa+iu);
         else id=0;
      }
      else yy=0.0;
      n_t=*(tx+i);
      if(n_t){
         mnn=(n_t<nsx)?n_t:nsx;
         n_st=*(sx+i);
         flag=1;
         if(n_st==mnn){
            if(n_st-1>n_t*frc)n_st -=1;
            else flag=0;
         }
         else if(n_st<=n_t*frc){
            flag=0;
         }
      }
      else flag=0;

      if(flag){
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         zz=(float)(log(pt*(1.0-qt))-log(qt*(1.0-pt)));

         yy-=(double)n_st*log(rt/pt) + (double)(n_t-n_st)*log(rt/qt);
         yy-=(double)(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         yy-=(double)(nnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));

         if(yy>tlim){
            xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
            xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
            if(mtc<mt){
               *(mti+mtc)=i;
               *(weg+mtc)=zz;
               *(alp+mtc)=yy;
               *(cst+mtc)=xx;
               mtc++;
            }
            else {
               *(mti+min)=i;
               *(weg+min)=zz;
               *(alp+min)=yy;
               *(cst+min)=xx;
               tlim=yy;
               for(j=0;j<mtc;j++){
                  if((uu=*(alp+j))<tlim){
                     tlim=uu;
                     min=j;
                  }
               }
            }
         }
      }
      if((trg)&&(mtc==mt)){
         trg=0;
         tlim=*(alp);
         min=0;
         for(j=0;j<mtc;j++){
            if((uu=*(alp+j))<tlim){
               tlim=uu;
               min=j;
            }
         }
      }
   }
   cs=0;
   for(j=0;j<mtc;j++)cs+=*(cst+j);
   cout << "cs= " << cs << " terms: " << mtc << endl;
}

void Focus_pth::weightApos(long flm){
   long n_t,n_st,flag,min=0,trg=1;
   long i,j,mnn,iu,fneg,id;
   double xx,yy,zz,frc,pt,qt,rt;
   float uu;
   char cnam[max_str];

   if(mt==0){mtc=0;return;}
   if(nsx==0){mtc=0;return;}
   frc=((double)nsx)/((double)nnx);
   tlim=0;
   mtc=0;
   if(msc){ //Find initial term id.
     iu=0;
     id=*msa;
   }
   else id=0;

   for(i=1;i<nid;i++){
      if(i==id){
         yy=*(msm+iu)*1000000.0;
         iu++;
         if(iu<msc)id=*(msa+iu);
         else id=0;
      }
      else yy=0.0;
      n_t=*(tx+i);
      if((n_t)&&(n_t<=flm)){
         mnn=(n_t<nsx)?n_t:nsx;
         n_st=*(sx+i);
         flag=1;
         if(n_st==mnn){
            if(n_st-1>n_t*frc)n_st -=1;
            else flag=0;
         }
         else if(n_st<=n_t*frc){
            flag=0;
         }
      }
      else flag=0;

      if(flag){
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         zz=(float)(log(pt*(1.0-qt))-log(qt*(1.0-pt)));

         yy-=(double)n_st*log(rt/pt) + (double)(n_t-n_st)*log(rt/qt);
         yy-=(double)(nsx-n_st)*log((1.0-rt)/(1.0-pt));
         yy-=(double)(nnx-nsx-n_t+n_st)*log((1.0-rt)/(1.0-qt));

         if(yy>tlim){
            xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
            xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
            if(mtc<mt){
               *(mti+mtc)=i;
               *(weg+mtc)=zz;
               *(alp+mtc)=yy;
               *(cst+mtc)=xx;
               mtc++;
            }
            else {
               *(mti+min)=i;
               *(weg+min)=zz;
               *(alp+min)=yy;
               *(cst+min)=xx;
               tlim=yy;
               for(j=0;j<mtc;j++){
                  if((uu=*(alp+j))<tlim){
                     tlim=uu;
                     min=j;
                  }
               }
            }
         }
      }
      if((trg)&&(mtc==mt)){
         trg=0;
         tlim=*(alp);
         min=0;
         for(j=0;j<mtc;j++){
            if((uu=*(alp+j))<tlim){
               tlim=uu;
               min=j;
            }
         }
      }
   }
   cs=0;
   for(j=0;j<mtc;j++)cs+=*(cst+j);
   cout << "cs= " << cs << " terms: " << mtc << endl;
}

void Focus_pth::set_Dlim(void){
   long i;
   float sum=0.0;
   if(!mtc){dlim=100000000;return;}

   for(i=0;i<mtc;i++)sum+=*(weg+i);
   dlim=stn*sum/((double)mtc)+cs;
}

float *Focus_pth::scoreAll(void){
   long i,j,k,m,n,id,*pi;
   int pflag=get_qflag();
   float s,*ps,*wwg;
   unsigned char c,*utr;
   long flag_end;

   if(mtc==0)return(sco);
   pthread_mutex_lock(&lbot);
   bot=bot%num_thr;
   pthread_mutex_unlock(&lbot);
   pthread_mutex_lock(&ltop);
   top=top%num_thr;
   pthread_mutex_unlock(&ltop);

   if(sco!=NULL)delete [] sco;
   ps=sco=new float[pRgg->ntot];
   for(i=0;i<pRgg->ntot;i++)*(ps++)=cs;

   ther=0; //Initially zero threads deployed here.

   snm=mtc;
   xnm=new long[snm];
   for(i=0;i<snm;i++)xnm[i]=*(pSaa->inv+mti[i]);
   wnm=weg;
   cnm=0;
   flag_end=1;
   do {
      //Launch thread
      pthread_mutex_lock(&lcnm);
      if(cnm==snm)flag_end=0;
      pthread_mutex_unlock(&lcnm);
      if(flag_end){
         sem_wait(&smp);
         pthread_mutex_lock(&lther);
         pthread_mutex_lock(&lbot);
         i=add[bot%num_thr];
         pTd[i]->set_bay_data(sco);
         if(!pthread_create(&tid[i],&thread_attr,tscore,(void*)pTd[i])){
            ther++;bot++;
            if(pflag)cout << "thread fsa " << i << endl;
         }
         else {sem_post(&smp);cout << "Thread launch in Focus failed!" << endl;}
         pthread_mutex_unlock(&lbot);
         pthread_mutex_unlock(&lther);
      }
   }while(flag_end);
   flag_end=1;
   do {
      pthread_mutex_lock(&lther);
      if(ther)pthread_cond_wait(&conda,&lther);
      else flag_end=0;
      pthread_mutex_unlock(&lther);
   }while(flag_end);
   delete [] xnm;
   return(sco);
}

long Focus_pth::scoreCnt(void){
   long i;
   float *ps;

   mdc=0;
   ps=sco;
   for(i=0;i<md;i++){
      if(*(ps++)>dlim)mdc++;
   }
   cout << "Ndocs: " << mdc << endl;
   return(mdc);
}

long Focus_pth::scoreCnt_Sub(void){
   long i,*pi;
  
   mdc=0;  
   pi=idx;
   for(i=0;i<ix;i++){
      if(*(sco+*(pi++))>dlim)mdc++;
   }
   cout << "Ndocs: " << mdc << endl;
   return(mdc);
}

void Focus_pth::init_Full(long ik,long *ikx){
   long i,j,*pi;
   int pflag=get_qflag();

   pi=ikx;
   for(i=0;i<ik;i++){
      j=*(pi++);
      *(mdi+j)=1;
      mark(pflag,i,100,"docs");
   }
   mdc=ik;
   this->countSub(ik,ikx);
}

void Focus_pth::countUpdate(void){
   long i,j,k,*pi,ct,*ict;
   float *ps;
   int pflag=get_qflag();
  
   ict=new long[pRgg->ntot];
   ps=sco;
   pi=mdi;
   ct=0;
   for(i=0;i<md;i++){
      if(*(ps++)>dlim){
         if(!(*pi)){
            *(ict+(ct++))=i;
            *pi=1;
         }
      }
      pi++;
   }
   this->countSub(ct,ict);
   ps=sco;
   pi=mdi;
   ct=0;
   for(i=0;i<md;i++){
      if(*(ps++)<=dlim){
         if(*pi){
            *(ict+(ct++))=i;
            *pi=0;
         }
      }
      pi++;
   }
   this->ncountSub(ct,ict);
   delete [] ict;
}

void Focus_pth::countUpidx(void){
   long i,j,k,*pi,*pm,ct,*ict;
   int pflag=get_qflag();

   ict=new long[ix];
   pi=idx;
   pm=mdi;
   ct=0;
   for(j=0;j<md;j++){
      i=*pi;
      k=*pm;
      if(*(sco+i)>dlim){
         if(!k){
            *(ict+(ct++))=i;
            *pm=1;
         }
      }
      pi++;pm++;
   }
   this->countSub(ct,ict);
   pi=idx;
   pm=mdi;
   ct=0;
   for(j=0;j<md;j++){
      i=*pi;
      k=*pm;
      if(*(sco+i)<=dlim){
         if(k){
            *(ict+(ct++))=i;
            *pm=0;
         }
      }
      pi++;pm++;
   }
   this->ncountSub(ct,ict);
   delete [] ict;
}

void Focus_pth::show_Terms(void){
   long i,j,*ord,id;
   char cnam[max_str];
   float ss=0;

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      if(i>=mtc-stn)ss+=alp[i];
      j=*(ord+i);
      id=*(mti+j);
      pSaa->disk_ifind(id,cnam);
      cout << id << "\t" << *(alp+i) << " " << *(weg+j) << " " << *(tx+id) << " "  \
       << *(sx+id) << " " << cnam << endl;
   }
   cout << endl << "Ave_Alpha " << ss/((double)stn) << endl;
   delete [] ord;
}

void Focus_pth::show_Docms(long n){
   long i,j,k;
   long ct,*jdx,pmid;
   float min,*scx,ss;

   jdx=new long[n];
   for(i=0;i<n;i++)*(jdx+i)=i;
   k=0;
   min=*(sco+*(jdx+k));
   for(i=0;i<n;i++){
      if(*(sco+*(jdx+i))<*(sco+*(jdx+k))){
         k=i;
         min=*(sco+*(jdx+k));
      }
   }
   for(ct=n;ct<pRgg->ntot;ct++){
      if(min<*(sco+ct)){
         *(jdx+k)=ct;
         for(i=0;i<n;i++){
            if(*(sco+*(jdx+i))<*(sco+*(jdx+k))){
               k=i;
               min=*(sco+*(jdx+k));
            }
         }
      }
   }
   scx=new float[n];
   for(i=0;i<n;i++)*(scx+i)=-*(sco+*(jdx+i));
   hSort(n,scx,jdx);
   for(i=0;i<n;i++){
      ss=-*(scx+i);
      pmid=pRgg->pmid(*(jdx+i));
      cout << pmid << " " << *(jdx+i) << " " << ss << endl;
   }
   delete [] scx;
   delete [] jdx;
}
      
void Focus_pth::promote(long id,long k){
   long i,j,*pa,*pm;

   if(msc==ms){  
      pa=msa;
      pm=msm;
      ms+=100;
      msa=new long[ms];
      msm=new long[ms];
      for(i=0;i<msc;i++){
         *(msa+i)=*(pa+i);
         *(msm+i)=*(pm+i);
      }
      delete [] pa;
      delete [] pm;
   }
   i=0;
   while((i<msc)&&(*(msa+i)<id))i++;
   if((i<msc)&&(*(msa+i)==id)){
      *(msm+i)=k;
      return;
   }
   for(j=msc;j>i;j--){
      *(msa+j)=*(msa+j-1);
      *(msm+j)=*(msm+j-1);
   }
   *(msa+i)=id;
   *(msm+i)=k;
   msc++;
   return;
}

void Focus_pth::remove(long id){
   long i,j;

   i=0;
   while((i<msc)&&(*(msa+i)<id))i++;
   if(((i<msc)&&(*(msa+i)!=id))||(i==msc)){ 
      cout << "Term not in promoted set!" << endl;
      return;    
   }
   for(j=i;j<msc-1;j++){
      *(msa+j)=*(msa+j+1);
      *(msm+j)=*(msm+j+1);
   }
   msc--;
   return;
}

void Focus_pth::promote_all(void){
   long i,j,k,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      pSaa->disk_ifind(id,cnam);
      cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j) << " " << *(tx+id) << " "  \
       << *(sx+id) << " " << cnam << endl;
      if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
   }
   delete [] ord;
}

void Focus_pth::promote_unprom(void){
   long i,j,k,n,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      k=0;
      while((k<msc)&&(*(msa+k)!=id))k++;
      if((k==msc)||(*(msm+k)==0)){
         pSaa->disk_ifind(id,cnam);
         cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j)\
         << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
         if(n=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,n);
      }
   }
   delete [] ord;
}

void Focus_pth::promote_prom(void){
   long i,j,k,*ord,id;
   char cnam[max_str];

   j=0;
   for(i=0;i<msc;i++){
      id=*(msa+i);
      if(*(msm+i)>0){
         pSaa->disk_ifind(id,cnam);
         k=0;
         while((k<mtc)&&(*(mti+k)!=id))k++;
         if(k<mtc){
            j++;
            cout << j << "\t" << id << "\t" << *(alp+k) << " " << *(weg+k)\
            << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
            if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
         }
      }
   }
}

void Focus_pth::promote_demt(void){
   long i,j,k,*ord,id;
   char cnam[max_str];

   j=0;
   for(i=0;i<msc;i++){
      id=*(msa+i);
      if(*(msm+i)<0){
         pSaa->disk_ifind(id,cnam);
         j++;
         cout << j << "\t" << id << "\t" \
         << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
         if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
      }
   }
}

void Focus_pth::promote_subs_all(const char *str){
   long i,j,k,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      pSaa->disk_ifind(id,cnam);
      if(strstr(cnam,str)){
         cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j)\
         << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
         if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
      }
   }
   delete [] ord;
}

void Focus_pth::promote_subs_unprom(const char *str){
   long i,j,k,n,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      k=0;
      while((k<msc)&&(*(msa+k)!=id))k++;
      if((k==msc)||(*(msm+k)==0)){
         pSaa->disk_ifind(id,cnam);
         if(strstr(cnam,str)){
            cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j)\
            << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
            if(n=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,n);
         }
      }
   }
   delete [] ord;
}

void Focus_pth::promote_subs_auto(const char *str,long k){
   long i,j,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      pSaa->disk_ifind(id,cnam);
      if(strstr(cnam,str)){
         cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j)\
         << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
         this->promote(id,k);
      }
   }
   delete [] ord;
}

void Focus_pth::promote_subs_unprom_auto(const char *str){
   long i,j,k,n,*ord,id;
   char cnam[max_str];

   ord=new long[mtc];
   for(i=0;i<mtc;i++)*(ord+i)=i;
   hSort(mtc,alp,ord);

   for(i=mtc-1;i>-1;i--){
      j=*(ord+i);
      id=*(mti+j);
      k=0;
      while((k<msc)&&(*(msa+k)!=id))k++;
      if((k==msc)||(*(msm+k)==0)){
         pSaa->disk_ifind(id,cnam);
         if(strstr(cnam,str)){
            cout << mtc-i << "\t" << id << "\t" << *(alp+i) << " " << *(weg+j)\
            << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
            this->promote(id,1);
         }
      }
   }
   delete [] ord;
}

void Focus_pth::promote_subs_prom(const char *str){
   long i,j,k,*ord,id;
   char cnam[max_str];

   j=0;
   for(i=0;i<msc;i++){
      id=*(msa+i);
      if(*(msm+i)>0){
         pSaa->disk_ifind(id,cnam);
         if(strstr(cnam,str)){
            k=0;
            while((k<mtc)&&(*(mti+k)!=id))k++;
            if(k<mtc){
               j++;
               cout << j << "\t" << id << "\t" << *(alp+k) << " " << *(weg+k)\
               << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
               if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
            }
         }
      }
   }
}

void Focus_pth::promote_subs_demt(const char *str){
   long i,j,k,*ord,id;
   char cnam[max_str];

   j=0;
   for(i=0;i<msc;i++){
      id=*(msa+i);
      if(*(msm+i)<0){
         pSaa->disk_ifind(id,cnam);
         if(strstr(cnam,str)){
            j++;
            cout << j << "\t" << id << "\t" \
            << " " << *(tx+id) << " " << *(sx+id) << " " << cnam << endl;
            if(k=clnga(0,NULL,"-prom","0 to leave, nonzero to promote"))this->promote(id,k);
         }
      }
   }
}

void Focus_pth::Initiator(long tnm,long sn,Index *pInd){
   if(pInd->ix>count_lim){
      err_flag=1;
      return;
   }
   else err_flag=0;
   if((pInd==NULL)||(pInd->ix==0)){mt=0;return;}
   this->restart();
   stn=sn;
   this->mem_Terms(tnm);
   this->mem_Docms(pRgg->ntot);
   if(bflag){this->transfer();bflag=0;}
   this->zeros();
   this->init_Full(pInd->ix,pInd->idx);
   if(!flim)this->weightApos();
   else this->weightApos(flim);
}

void Focus_pth::Background_s(Index *pInd){
   if(pInd->ix>count_lim){
      err_flag=1;
      return;
   }
   else err_flag=0;
   this->mem_Docms(pInd->ix);
   ix=pInd->ix;
   if(idx!=NULL)delete [] idx;
   idx=new long[ix];
   long i;
   for(i=0;i<ix;i++)idx[i]=(pInd->idx)[i];
   zerot();
   countTot(ix,idx);
   bflag=1;
}

void Focus_pth::Initiator_s(long tnm,long sn,Index *pInd){
   if((pInd==NULL)||(pInd->ix==0)){mt=0;return;}
   if(!bflag){cout << "Error, background not called!" << endl;exit(0);}
   this->restart();
   stn=sn;
   this->mem_Terms(tnm);
   Index *jind=new Index;
   jind->ix=ix;
   jind->idx=idx;
   Bool_pth Bl;
   Index *kind=Bl.cbool_And(jind,pInd);
   if((kind==NULL)||(kind->ix==0)){mt=0;return;}
   long i,j=0;
   for(i=0;i<md;i++)mdi[i]=0;
   mdc=0;
   i=0;
   while((i<ix)&&(j<kind->ix)){
      while(*(idx+i)<*(kind->idx+j))i++;
      if((i<ix)&&(*(idx+i)==*(kind->idx+j))){*(mdi+i)=1;j++;mdc++;}
      while((j<kind->ix)&&(*(kind->idx+j)<*(idx+i)))j++;
   }
   if(kind->ix>count_lim){
      err_flag=1;
      return;
   }
   else err_flag=0;
   zeros();
   countSub(kind->ix,kind->idx);
   delete kind;
   jind->idx=NULL;
   delete jind;
   if(!flim)weightApos();
   else weightApos(flim);
}

void Focus_pth::Initiator(Theme *pThm,long bfl){
   long i,id;
   if(mti!=NULL)delete [] mti;
   if(weg!=NULL)delete [] weg;
   if(alp!=NULL)delete [] alp;
   if(cst!=NULL)delete [] cst;
   if(msa!=NULL)delete [] msa;
   if(msm!=NULL)delete [] msm;
   mt=pThm->mt;
   mtc=pThm->mtc;
   ms=pThm->ms;
   msc=pThm->msc;
   mti=new long[mt];
   weg=new float[mt];
   alp=new float[mt];
   cst=new float[mt];
   msa=new long[ms];
   msm=new long[ms];
   for(i=0;i<mtc;i++){
      *(mti+i)=*(pThm->mti+i);
      *(weg+i)=*(pThm->weg+i);
      *(alp+i)=*(pThm->alp+i);
   }
   for(i=0;i<msc;i++){
      id=*(pThm->msa+i);
      if(id<0){
         *(msa+i)=-id;
         *(msm+i)=-1;
      }
      else {
         *(msa+i)=id;
         *(msm+i)=1;
      }
   }
   stn=pThm->stn;
   cs=pThm->cs;
   this->set_Dlim();
   if((!bfl)&&bflag){
      this->transfer();
      this->mem_Docms(pRgg->ntot);
      bflag=0;
   }
   else if(bfl&&(!bflag)){cout << "Error in flags!" << endl;exit(0);}
   else {
      for(i=0;i<md;i++)mdi[i]=0;
   }
   mdc=0;
   this->zeros();
}

void Focus_pth::Iterator(long tnm,long sn){
   if(mt==0)return;
   stn=sn;
   this->scoreAll();
   this->set_Dlim();
   this->scoreCnt();
   if(mdc>count_lim){
      err_flag=1;
      return;
   }
   else err_flag=0;
   this->countUpdate();
   if(tnm!=mt)mem_Terms(tnm);
   if(!flim)this->weightApos();
   else this->weightApos(flim);
}

void Focus_pth::Iterator_s(long tnm,long sn){
   if(mt==0)return;
   stn=sn;
   this->scoreAll();
   this->set_Dlim();
   this->scoreCnt_Sub();
   if(mdc>count_lim){
      err_flag=1;
      return;
   }
   else err_flag=0;
   this->countUpidx();
   if(tnm!=mt)mem_Terms(tnm);
   if(!flim)this->weightApos();
   else this->weightApos(flim);
}

Theme *Focus_pth::Save(void){
   if(!mtc)return(NULL);
   long i;
   Theme *qThm=new Theme;
   qThm->mt=mt;
   qThm->mtc=mtc;
   qThm->alp=alp;
   qThm->weg=weg;
   qThm->ms=ms;
   qThm->msc=msc;
   qThm->mti=mti;
   for(i=0;i<msc;i++){
      if(*(msm+i)<0)*(msa+i)=-*(msa+i);
   }
   qThm->msa=msa;
   qThm->stn=stn;
   qThm->cs=cs;
  
   delete [] msm;
   delete [] cst;
   mt=0;
   mtc=0;
   ms=0;
   msc=0;
   mti=NULL;
   weg=NULL;
   alp=NULL;
   cst=NULL;
   msa=NULL;
   msm=NULL;
   return(qThm);
}

void Focus_pth::Write(Theme *pThm){
   long i,j,id;
   char cnam[max_str],bnam[max_str],c;
   
   cout << "Enter file extension:" << endl;
   while(cin.peek()=='\n')cin.get(c);
   j=0;
   while((cin.get(c))&&(c!='\n'))cnam[j++]=c;
   cnam[j]='\0';
   cout << "File extension entered: " << cnam << endl;
   get_pathw(bnam,"focus",name,cnam);
   ofstream fout(bnam,ios::out);
   
   fout << pThm->mt << endl;
   fout << pThm->mtc << endl;
   for(i=0;i<pThm->mtc;i++){
      pSaa->disk_ifind(*(pThm->mti+i),cnam);
      fout << *(pThm->alp+i) << endl;
      fout << *(pThm->weg+i) << endl;
      fout << cnam << endl;
   }
      
   fout << pThm->ms << endl;
   fout << pThm->msc << endl;
   for(i=0;i<pThm->msc;i++){
      id=*(pThm->msa+i);
      if(id<0){
         j=-1;
         pSaa->disk_ifind(-id,cnam);
      }
      else {
         j=1;
         pSaa->disk_ifind(id,cnam);
      }
      fout << j << endl;
      fout << cnam << endl;
   }
   fout << pThm->stn << endl;
   fout << pThm->cs << endl;
}

Theme *Focus_pth::Read(void){
   long i,j,id,*ord;
   char cnam[max_str],bnam[max_str],c;

   cout << "Enter file extension:" << endl;
   while(cin.peek()=='\n')cin.get(c);
   j=0;
   while((cin.get(c))&&(c!='\n'))cnam[j++]=c;
   cnam[j]='\0';
   cout << "File extension entered: " << cnam << endl;
   get_pathw(bnam,"focus",name,cnam);
   ifstream fin(bnam,ios::in);
  
   Theme *pThm=new Theme;
   fin >> pThm->mt;
   fin >> pThm->mtc;
   pThm->mti=new long[pThm->mt];
   pThm->alp=new float[pThm->mt];
   pThm->weg=new float[pThm->mt];

   for(i=0;i<pThm->mtc;i++){
      fin >> *(pThm->alp+i);
      fin >> *(pThm->weg+i);
      while(isspace(fin.peek()))fin.get(c);
      fin.get(cnam,max_str);
      id=pSaa->disk_sifind(cnam);
      if(id)*(pThm->mti+i)=id;
      else {
         cout << "Error! Unable to find term " << cnam << " in term list." << endl;
         return(NULL);
      }
   }
     
   fin >> pThm->ms;
   fin >> pThm->msc;
   pThm->msa=new long[pThm->ms];
   ord=new long[pThm->msc];

   for(i=0;i<pThm->msc;i++){
      fin >> j;
      while(isspace(fin.peek()))fin.get(c);
      fin.get(cnam,max_str);
      id=pSaa->disk_sifind(cnam);
      if(id){
         *(pThm->msa+i)=j*id;
         ord[i]=id;  
      }
      else {
         cout << "Error! Unable to find term " << cnam << " in term list." << endl;
         return(NULL);
      }
   }
   hSort(pThm->msc,ord,pThm->msa);
   delete [] ord;
   fin >> pThm->stn;
   fin >> pThm->cs;
   return(pThm);
}

void Focus_pth::Write(ofstream &fout,Theme *pThm){
   long i,j,id;
   char cnam[max_str],bnam[max_str],c;

   fout << pThm->mt << endl;
   fout << pThm->mtc << endl;
   for(i=0;i<pThm->mtc;i++){
      pSaa->disk_ifind(*(pThm->mti+i),cnam);
      fout << *(pThm->alp+i) << endl;
      fout << *(pThm->weg+i) << endl;
      fout << cnam << endl;
   }

   fout << pThm->ms << endl;
   fout << pThm->msc << endl;
   for(i=0;i<pThm->msc;i++){
      id=*(pThm->msa+i);
      if(id<0){
         j=-1;
         pSaa->disk_ifind(-id,cnam);
      }
      else {
         j=1;
         pSaa->disk_ifind(id,cnam);
      }
      fout << j << endl;
      fout << cnam << endl;
   }
   fout << pThm->stn << endl;
   fout << pThm->cs << endl;
}

Theme *Focus_pth::Read(ifstream &fin){
   long i,j,id,*ord;
   char cnam[max_str],bnam[max_str],c;

   Theme *pThm=new Theme;
   fin >> pThm->mt;
   fin >> pThm->mtc;
   pThm->mti=new long[pThm->mt];
   pThm->alp=new float[pThm->mt];
   pThm->weg=new float[pThm->mt];

   for(i=0;i<pThm->mtc;i++){
      fin >> *(pThm->alp+i);
      fin >> *(pThm->weg+i);
      while(isspace(fin.peek()))fin.get(c);
      fin.get(cnam,max_str);
      id=pSaa->disk_sifind(cnam);
      if(id)*(pThm->mti+i)=id;
      else {
         cout << "Error! Unable to find term " << cnam << " in term list." << endl;
         return(NULL);
      }
   }

   fin >> pThm->ms;
   fin >> pThm->msc;
   pThm->msa=new long[pThm->ms];
   ord=new long[pThm->msc];

   for(i=0;i<pThm->msc;i++){
      fin >> j;
      while(isspace(fin.peek()))fin.get(c);
      fin.get(cnam,max_str);
      id=pSaa->disk_sifind(cnam);
      if(id){
         *(pThm->msa+i)=j*id;
         ord[i]=id;  
      }
      else {
         cout << "Error! Unable to find term " << cnam << " in term list." << endl;
         return(NULL);
      }
   }
   hSort(pThm->msc,ord,pThm->msa);
   delete [] ord;
   fin >> pThm->stn;
   fin >> pThm->cs;
   return(pThm);
}

void Focus_pth::debug_mdi(void){
   long i,ct=0;;
   cout << " *** Document dlim " << dlim << endl;
   cout << " *** mdc " << mdc << endl;
   for(i=0;i<md;i++){
      if(mdi[i]){
         cout << idx[i] << " " << sco[idx[i]] << endl;
         ct++;
      }
   }
   cout << " *** Marked docs " << ct << endl;
}

}
