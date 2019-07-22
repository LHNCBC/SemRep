#include <iostream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "runn.h"
#include "Blist.h"
#include "Regist.h"
#include "Btree.h"
#include "Docum.h"
#include "DataObj.h"
#include "Bool.h"
#include "Vnab.h"
#include "Nabor.h"
using namespace std;
namespace iret {

Nabor::Nabor(void) {
   sco=NULL;
   pRgg=NULL;
   pSaa=NULL;
   err_flag=0;
   term_lim=1000;
}

Nabor::Nabor(long lim) {
   sco=NULL;
   pRgg=NULL;
   pSaa=NULL;
   err_flag=0;
   term_lim=lim;
}

Nabor::~Nabor(){
   if (sco!=NULL)delete [] sco;
}

void Nabor::gopen_Nabor(Regist *pReg,Slice_Accomp *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j,k,m;
   double xcd=0.0044,xlt=log(0.7);
   Docum *pDoc;

   pRgg=pReg;
   pSaa=pSac;
   pSaa->inv_load();
   pSaa->post_access();
   pSaa->gopen_idstring(pSaa->slice_name);
   nid=pSaa->nid;

   if(pflag)cout << "Opening document files" << endl;
   tf=(float**)new long[128];
   for(i=1;i<128;i++){
      *(tf+i)=new float[2000];
      for(k=1;k<2000;k++){
         *(*(tf+i)+k)=(float)(1.0/(1.0+exp(xcd*k+xlt*(i-1))));
      }
   }
   pRgg->set_class();
   pRgg->gopen_read(READ_M);
   dl=new long[pRgg->ntot];
   i=0;
   m=0;
   for(j=0;j<pRgg->ndst;j++){
      pDoc=pRgg->pDrg[j];
      for(k=0;k<pRgg->ndoc[j];k++){
         *(dl+i+k)=*(pDoc->alen+k);
         if(m<*(dl+i+k))m=*(dl+i+k);
      }
      i+=pRgg->ndoc[j];
      mark(pflag,j+1,1,"docset lengths processed");
   }
   cout << "Max doc length = " << m << endl;
   pRgg->gclose_read(READ_M);
   pRgg->gopen_read(READ_U);
   pRgg->gopen_binary();
   ndoc=pRgg->ntot;
}

void Nabor::gopen_Nabor_map(Regist *pReg,Slice_Accomp *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j,k,m;
   double xcd=0.0044,xlt=log(0.7);
   Docum *pDoc;

   pRgg=pReg;
   pSaa=pSac;
   pSaa->inv_map();
   pSaa->post_access_map();
   pSaa->gopen_idstring(pSaa->slice_name);
   nid=pSaa->nid;

   if(pflag)cout << "Opening document files" << endl;
   tf=(float**)new long[128];
   for(i=1;i<128;i++){
      *(tf+i)=new float[2000];
      for(k=1;k<2000;k++){
         *(*(tf+i)+k)=(float)(1.0/(1.0+exp(xcd*k+xlt*(i-1))));
      }
   }
   pRgg->set_class();
   pRgg->gopen_map(READ_M);
   dl=new long[pRgg->ntot];
   i=0;
   m=0;
   for(j=0;j<pRgg->ndst;j++){
      pDoc=pRgg->pDrg[j];
      for(k=0;k<pRgg->ndoc[j];k++){
         *(dl+i+k)=*(pDoc->alen+k);
         if(m<*(dl+i+k))m=*(dl+i+k);
      }
      i+=pRgg->ndoc[j];
      mark(pflag,j+1,1,"docset lengths processed");
   }
   cout << "Max doc length = " << m << endl;
   pRgg->gclose_map(READ_M);
   pRgg->gopen_map(READ_U);
   pRgg->gopen_binary_map();
   ndoc=pRgg->ntot;
}

float *Nabor::scoreVec(long n){
   long i,j,k,m,ni,num,id,ix,zi;
   long icut=1000000,dn;
   int pflag=get_qflag(),len,iz;
   char cnam[max_str];
   double xx,yy,xct,xttrc,dx;
   unsigned char c,*utr;
   long nt,yi,od,fq;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   if(sco==NULL)sco=new float[ndoc];
   for(i=0;i<ndoc;i++)*(sco+i)=0;
   xttrc=(double)ndoc;

   num=pRgg->readb(n);
   dn=*(dl+n);
   for(ni=0;ni<num;ni++){
      id=pRgg->tnm[ni];
      if(ix=*(pSaa->inv+id)){
         pSaa->disk_nfind((--ix),cnam);
         len=strlen(cnam);
         if(((cnam[len-1]=='t')&&(cnam[len-2]=='!'))|| \
           ((cnam[len-2]!='!')&&(!strchr(cnam,'*')))){
            fq=*(pSaa->freq+ix);
            if(fq<=icut){
               j=pRgg->lct[ni];
               dx=*(*(tf+j)+dn);
               if(fq>dmt)xct=(double)fq;
               else xct=dmt;
               xx=dx*l2*log(xttrc/xct);
               pk=pSaa->read_comp(ix);
               ba=pSaa->pw[pk];
               utr=pSaa->cod;
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
                                   yy=*(*(tf+mj)+*(dl+mi));
                                   *(sco+mi)+=yy*xx;
                                   i++;
                                   od=mi;
                                   mi=0;
                                }
                             }
                             break;
                  }
               }
            }
         }
      }
      //mark(pflag,ni,10,"terms scored");
   }
   return(sco);
}

float *Nabor::scoreVec_pmid(long pmid){
   long n=pRgg->index(pmid);
   if(n){
      this->scoreVec(n-1);
      return(sco);
   }
   else {
      cout << "Document not in system!" << endl;
      return(NULL);
   }

}

float *Nabor::scoreVec_Doc(Docum &Doc){
   long i,j,k,m,ni,num,id,ix,zi;
   long icut=1000000;
   int pflag=get_qflag(),len,iz;
   char cnam[max_str],*pch;
   double xx,yy,xct,xttrc,dx;
   long dn=Doc.sum_lcnt();
   unsigned char c,*utr;
   long nt,yi,od,fq;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   if(Doc.num_wrds()>term_lim){
      err_flag=1;
      sco=NULL;
      return(NULL);
   }
   else err_flag=0;

   if(sco==NULL)sco=new float[ndoc];
   for(i=0;i<ndoc;i++)*(sco+i)=0;
   xttrc=(double)ndoc;

   ni=0;
   while(pch=Doc.show(iz)){
      if(ix=pSaa->disk_snfind(pch)){
         len=strlen(pch);
         if(((pch[len-1]=='t')&&(pch[len-2]=='!'))|| \
           ((pch[len-2]!='!')&&(!strchr(pch,'*')))){
            fq=*(pSaa->freq+(--ix));
            if(fq<=icut){
               dx=*(*(tf+iz)+dn);
               if(fq>dmt)xct=(double)fq;
               else xct=dmt;
               xx=dx*l2*log(xttrc/xct);
  cout << pch << endl;
  cout << "    " << dx << " " << l2*log(xttrc/xct) << endl;
               pk=pSaa->read_comp(ix);
               ba=pSaa->pw[pk];
               utr=pSaa->cod;
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
                                   yy=*(*(tf+mj)+*(dl+mi));
                                   *(sco+mi)+=yy*xx;
                                   i++;
                                   od=mi;
                                   mi=0;
                                }
                             }
                             break;
                  }
               }
            }
         }
      }
      mark(pflag,++ni,10,"terms scored");
   }
   return(sco);
}

float *Nabor::scoreVec_Thm(Theme *pThm){
   long i,j,k,m,n,id,*pi;
   int pflag=get_qflag();
   float s,*ps,*wwg;
   unsigned char c,*utr;
   long nt,yi,od,fq,iix;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   if(pThm->mtc>term_lim){
      err_flag=1;
      sco=NULL;
      return(NULL);
   }
   else err_flag=0;

   if(sco!=NULL)delete [] sco;
   ps=sco=new float[pRgg->ntot];
   for(i=0;i<pRgg->ntot;i++)*(ps++)=pThm->cs;

   pi=pThm->mti;
   wwg=pThm->weg;
   for(j=0;j<pThm->mtc;j++){
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
      mark(pflag,j,10,"terms scored");
   }
   return(sco);
}

float Nabor::scoreDoc_Doc(Docum &Doc,Docum &Dcc){
   long i,j,k,m,ni,num,id,ix,zi;
   long icut=1000000,fq;
   int pflag=get_qflag(),len,iz,jz;
   char *pch,*pck;
   double xx,yy,xct,xttrc,dx,dy;
   long dn=Doc.sum_lcnt(),dm=Dcc.sum_lcnt();
   float sum=0;

   if((Doc.num_wrds()>term_lim)||(Dcc.num_wrds()>term_lim)){
      err_flag=1;
      return(0);
   }
   else err_flag=0;

   xttrc=(double)ndoc;

   Doc.reset();
   Dcc.reset();
   ni=0;
   pch=Doc.show(iz);
   pck=Dcc.show(jz);
   while(pch&&pck){
      if(i=strcmp(pch,pck)){
         if(i<0){
            while(pch=Doc.show(iz)){
               if((i=strcmp(pch,pck))>=0)break;
            }
         }
         else if(i>0){
            while(pck=Dcc.show(jz)){
               if((i=strcmp(pch,pck))<=0)break;
            }
         }
      }
      else {
         if(ix=pSaa->disk_snfind(pch)){
            len=strlen(pch);
            if(((pch[len-1]=='t')&&(pch[len-2]=='!'))|| \
              ((pch[len-2]!='!')&&(!strchr(pch,'*')))){
               fq=*(pSaa->freq+(--ix));
               if(fq<=icut){
                  dx=*(*(tf+iz)+dn);
                  dy=*(*(tf+jz)+dm);
                  if(fq>dmt)xct=(double)fq;
                  else xct=dmt;
                  sum+=dx*dy*l2*log(xttrc/xct);
                  cout << ix << "    " << dx << " " << dy << " " << \
                       l2*log(xttrc/xct) << " " << pch << endl;
               }
            }
         }
         pch=Doc.show(iz);
         pck=Dcc.show(jz);
      }
   }
   return(sum);
}

float Nabor::scoreDoc_BDoc(Docum &Doc,long pmid){
   long i,j,k,m,ni,num,id,jd,ix,zi;
   long icut=1000000,fq,ct,*dtid,*dtft,*dtfq;
   int pflag=get_qflag(),len,iz;
   char *pch;
   double xct,xttrc,dx,dy;
   long dn=Doc.sum_lcnt(),dm;
   float sum=0;

   if((m=Doc.num_wrds())>term_lim){
      err_flag=1;
      return(0);
   }
   else err_flag=0;

   xttrc=(double)ndoc;

   dtid=new long[m];
   dtft=new long[m];
   dtfq=new long[m];
   ct=0;
   Doc.reset();
   while(pch=Doc.show(iz)){
      if(id=pSaa->disk_sifind(pch)){
         ix=*(pSaa->inv+id);
         len=strlen(pch);
         if(((pch[len-1]=='t')&&(pch[len-2]=='!'))|| \
           ((pch[len-2]!='!')&&(!strchr(pch,'*')))){
            fq=*(pSaa->freq+(--ix));
            if(fq<=icut){
               dtid[ct]=id;
               dtft[ct]=iz;
               dtfq[ct]=fq;
               ct++;
            }
         }
      }
   }
   hSort(ct,dtid,dtft,dtfq);
   ni=pRgg->index(pmid);
   if(!ni){
      cout << "Document not in system!" << endl;
      return(0);
   }
   ni--;
   num=pRgg->readb(ni);
   hSort(num,pRgg->tnm,pRgg->lct);
   dm=*(dl+ni);
   i=j=0;
   while((i<ct)&&(j<num)){
      id=dtid[i];
      jd=pRgg->tnm[j];
      if(id<jd){
         while((i<ct)&&((id=dtid[i])<jd))i++;
      }
      else if(jd<id){
         while((j<num)&&((jd=pRgg->tnm[j])<id))j++;
      }
      else {
          iz=pRgg->lct[j];
          dy=*(*(tf+iz)+dm);
          iz=dtft[i];
          dx=*(*(tf+iz)+dn);
          fq=dtfq[i];
          if(fq>dmt)xct=(double)fq;
          else xct=dmt;
          sum+=dx*dy*l2*log(xttrc/xct);
          i++;j++;
      }
   }
   return(sum);
}

Order *Nabor::skim(long n){
   if(sco==NULL)return(NULL);
   float sx;
   Order *pOrd=new Order(n,ndoc,sco);

   long i=0;
   pOrd->ind(i,sx);
   if(sx>0)return(pOrd);
   else return(NULL);
}

Order *Nabor::skim(long n,Index *pind){
   if(sco==NULL)return(NULL);
   float sx,*scc;

   if(pind==NULL)return(NULL);
   if(pind->ix<1)return(NULL);

   Order *pOrd=new Order(n,pind,sco);

   long i=0;
   pOrd->ind(i,sx);
   if(sx>0)return(pOrd);
   else return(NULL);
}

//Multithreading version.

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
   static pthread_cond_t conda; //Condition variable for end.
   static pthread_mutex_t lsco; //lock for sco updating
   static pthread_mutex_t lther; //lock for thread count
   static int ther; //Number of access threads deployed in this class
   static long snm; //Number of terms in a doc
   static long *xnm; //List of term ids
   static float *wnm; //List of term weights
   static long cnm; //Term process counter
   static pthread_mutex_t lcnm; //Lock for term process counter

extern "C" void *fscore(void *pth_data){
   unsigned char c,*utr;
   long nt,yi,od,fq,jfq,cn,i,j,ncl;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;
   long flag_end,ix;
   float yy,*yt,**mt,wx;

   Th_data *ptd=(Th_data *)pth_data;
   flag_end=1;
   do {
      pthread_mutex_lock(&lcnm);
      if(cnm<snm){
         ix=*(xnm+cnm);
         wx=*(wnm+(cnm++));
         cout << "word " << ix << endl;
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
                       yy=*(*((ptd->tf)+mj)+*((ptd->dl)+mi));
                       ptd->ad[j]=(ptd->sco)+mi;
                       ptd->wd[j]=yy*wx;
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
            yt=ptd->wd;
            for(j=0;j<jfq;j++)(**(mt++))+=*(yt++);
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

extern "C" void *tmscore(void *pth_data){
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

Nabor_pth::Nabor_pth(void) {
   sco=NULL;
   pRgg=NULL;
   pSaa=NULL;
   err_flag=0;
   term_lim=1000;
}

Nabor_pth::Nabor_pth(long lim) {
   sco=NULL;
   pRgg=NULL;
   pSaa=NULL;
   err_flag=0;
   term_lim=lim;
}

Nabor_pth::~Nabor_pth(){
   if (sco!=NULL)delete [] sco;
}

void Nabor_pth::gopen_Nabor_pth(Regist_pth *pReg,Slice_Accpth *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j,k,m;
   double xcd=0.0044,xlt=log(0.7);
   Docum *pDoc;

   pRgg=pReg;
   pSaa=pSac;
   pSaa->inv_load();
   pSaa->post_access();
   pSaa->gopen_idstring(pSaa->slice_name);
   nid=pSaa->nid;

   if(pflag)cout << "Opening document files" << endl;
   tf=(float**)new long[128];
   for(i=1;i<128;i++){
      *(tf+i)=new float[2000];
      for(k=1;k<2000;k++){
         *(*(tf+i)+k)=(float)(1.0/(1.0+exp(xcd*k+xlt*(i-1))));
      }
   }
   pRgg->set_class();
   pRgg->gopen_read(READ_M);
   dl=new long[pRgg->ntot];
   i=0;
   m=0;
   for(j=0;j<pRgg->ndst;j++){
      pDoc=pRgg->pDrg[j];
      for(k=0;k<pRgg->ndoc[j];k++){
         *(dl+i+k)=*(pDoc->alen+k);
         if(m<*(dl+i+k))m=*(dl+i+k);
      }
      i+=pRgg->ndoc[j];
      mark(pflag,j+1,1,"docset lengths processed");
   }
   cout << "Max doc length = " << m << endl;
   pRgg->gclose_read(READ_M);
   pRgg->gopen_read(READ_U);
   pRgg->gopen_binary_pth();
   ndoc=pRgg->ntot;

   //Multithreading setup
   if(pthread_mutex_init(&lsco,NULL)){
      cout << "Error in lsco mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lther,NULL)){
      cout << "Error in lther mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lcnm,NULL)){
      cout << "Error in lcnm mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_cond_init(&conda,NULL)){
      cout << "Error in conda initialization!" << endl;
      exit(0);
   }
   pthread_attr_init(&thread_attr);
   pthread_attr_setdetachstate(&thread_attr, PTHREAD_CREATE_DETACHED);
   pthread_attr_setscope(&thread_attr, PTHREAD_SCOPE_SYSTEM);
   pthread_attr_setinheritsched(&thread_attr, PTHREAD_EXPLICIT_SCHED);
} 

void Nabor_pth::gopen_Nabor_map(Regist_pth *pReg,Slice_Accpth *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j,k,m;
   double xcd=0.0044,xlt=log(0.7);
   Docum *pDoc;

   pRgg=pReg;
   pSaa=pSac;
   pSaa->inv_map();
   pSaa->post_access_map();
   pSaa->gopen_idstring(pSaa->slice_name);
   nid=pSaa->nid;

   if(pflag)cout << "Opening document files" << endl;
   tf=(float**)new long[128];
   for(i=1;i<128;i++){
      *(tf+i)=new float[2000];
      for(k=1;k<2000;k++){
         *(*(tf+i)+k)=(float)(1.0/(1.0+exp(xcd*k+xlt*(i-1))));
      }
   }
   pRgg->set_class();
   pRgg->gopen_map(READ_M);
   dl=new long[pRgg->ntot];
   i=0;
   m=0;
   for(j=0;j<pRgg->ndst;j++){
      pDoc=pRgg->pDrg[j];
      for(k=0;k<pRgg->ndoc[j];k++){
         *(dl+i+k)=*(pDoc->alen+k);
         if(m<*(dl+i+k))m=*(dl+i+k);
      }
      i+=pRgg->ndoc[j];
      mark(pflag,j+1,1,"docset lengths processed");
   }
   cout << "Max doc length = " << m << endl;
   pRgg->gclose_map(READ_M);
   pRgg->gopen_map(READ_U);
   pRgg->gopen_binary_map();
   ndoc=pRgg->ntot;

   //Multithreading setup
   if(pthread_mutex_init(&lsco,NULL)){
      cout << "Error in lsco mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lther,NULL)){
      cout << "Error in lther mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_mutex_init(&lcnm,NULL)){
      cout << "Error in lcnm mutex initialization!" << endl;
      exit(0);
   }
   if(pthread_cond_init(&conda,NULL)){
      cout << "Error in conda initialization!" << endl;
      exit(0);
   }
   pthread_attr_init(&thread_attr);
   pthread_attr_setdetachstate(&thread_attr, PTHREAD_CREATE_DETACHED);
   pthread_attr_setscope(&thread_attr, PTHREAD_SCOPE_SYSTEM);
   pthread_attr_setinheritsched(&thread_attr, PTHREAD_EXPLICIT_SCHED);
}

float *Nabor_pth::scoreVec(long n){
   long i,j,k,m,ni,num,id,ix,zi;
   long icut=1000000,dn;
   int pflag=get_qflag(),len,iz;
   char cnam[max_str];
   double xx,yy,xct,xttrc,dx;
   float *sxx;
   unsigned char c,*utr;
   long fq,flag_end;

   pthread_mutex_lock(&lbot);
   bot=bot%num_thr;
   pthread_mutex_unlock(&lbot);
   pthread_mutex_lock(&ltop);
   top=top%num_thr;
   pthread_mutex_unlock(&ltop);

   if(sco==NULL)sco=new float[ndoc];
   sxx=sco;
   for(i=0;i<ndoc;i++)*(sxx++)=0;
   xttrc=(double)ndoc;
   
   ther=0; //Initially zero threads deployed here.

   snm=pRgg->readb(n);
   dn=*(dl+n);
   xnm=new long[snm];
   wnm=new float[snm];
   cnm=0;
   for(ni=0;ni<snm;ni++){
      id=pRgg->tnm[ni];
      if(ix=*(pSaa->inv+id)){
         pSaa->disk_nfind((--ix),cnam);
         len=strlen(cnam);
         if(((cnam[len-1]=='t')&&(cnam[len-2]=='!'))|| \
           ((cnam[len-2]!='!')&&(!strchr(cnam,'*')))){
            fq=*(pSaa->freq+ix);
            if(fq<=icut){
               j=pRgg->lct[ni];
               dx=*(*(tf+j)+dn);
               if(fq>dmt)xct=(double)fq;
               else xct=dmt;
               *(wnm+cnm)=dx*l2*log(xttrc/xct);
               *(xnm+cnm)=ix;
               cnm++;
            }
         }
      }
   }
   snm=cnm;
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
         pTd[i]->set_nab_data(dl,tf,sco);
         if(!pthread_create(&tid[i],&thread_attr,fscore,(void*)pTd[i])){
            ther++;bot++;
            if(pflag)cout << "thread npmid " << i << endl;
         }
         else {sem_post(&smp);cout << "Thread launch in Nabor failed!" << endl;}
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
   delete [] wnm;
   return(sco);
}

float *Nabor_pth::scoreVec_pmid(long pmid){
   long n=pRgg->index(pmid);
   if(n){
      this->scoreVec(n-1);
      return(sco);
   }
   else {
      cout << "Document not in system!" << endl;
      return(NULL);
   }

}

float *Nabor_pth::scoreVec_Doc(Docum &Doc){
   long i,j,k,m,ni,num,id,ix,zi;
   long icut=1000000;
   int pflag=get_qflag(),len,iz;
   char cnam[max_str],*pch;
   double xx,yy,xct,xttrc,dx;
   long dn=Doc.sum_lcnt();
   unsigned char c,*utr;
   long fq,flag_end;

   if(Doc.num_wrds()>term_lim){
      err_flag=1;
      sco=NULL;
      return(NULL);
   }
   else err_flag=0;

   pthread_mutex_lock(&lbot);
   bot=bot%num_thr;
   pthread_mutex_unlock(&lbot);
   pthread_mutex_lock(&ltop);
   top=top%num_thr;
   pthread_mutex_unlock(&ltop);

   if(sco==NULL)sco=new float[ndoc];
   for(i=0;i<ndoc;i++)*(sco+i)=0;
   xttrc=(double)ndoc;

   ther=0;
   snm=Doc.num_wrds();
   xnm=new long[snm];
   wnm=new float[snm];
   cnm=0;

   ni=0;
   while(pch=Doc.show(iz)){
      if(ix=pSaa->disk_snfind(pch)){
         len=strlen(pch);
         if(((pch[len-1]=='t')&&(pch[len-2]=='!'))|| \
           ((pch[len-2]!='!')&&(!strchr(pch,'*')))){
            fq=*(pSaa->freq+(--ix));
            if(fq<=icut){
               dx=*(*(tf+iz)+dn);
               if(fq>dmt)xct=(double)fq;
               else xct=dmt;
               *(wnm+cnm)=dx*l2*log(xttrc/xct);
               *(xnm+cnm)=ix;
               cnm++;
            }
         }
      }
   }
   snm=cnm;
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
         pTd[i]->set_nab_data(dl,tf,sco);
         if(!pthread_create(&tid[i],&thread_attr,fscore,(void*)pTd[i])){
            ther++;bot++;
            if(pflag)cout << "thread nfly " << i << endl;
         }
         else {sem_post(&smp);cout << "Thread launch in Nabor failed!" << endl;}
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
   delete [] wnm;
   return(sco);
}

float *Nabor_pth::scoreVec_Title(Docum &Doc){
   long i,j,k,m,ni,num,id,ix,zi;
   long icut=1000000;
   int pflag=get_qflag(),len,iz;
   char cnam[max_str],*pch;
   double xx,yy,xct,xttrc,dx;
   unsigned char c,*utr;
   long fq,flag_end;

   if(Doc.num_wrds()>term_lim){
      err_flag=1;
      sco=NULL;
      return(NULL);
   }
   else err_flag=0;

   pthread_mutex_lock(&lbot);
   bot=bot%num_thr;
   pthread_mutex_unlock(&lbot);
   pthread_mutex_lock(&ltop);
   top=top%num_thr;
   pthread_mutex_unlock(&ltop);

   if(sco==NULL)sco=new float[ndoc];
   for(i=0;i<ndoc;i++)*(sco+i)=0;
   xttrc=(double)ndoc;

   ther=0;
   snm=Doc.num_wrds();
   xnm=new long[snm];
   wnm=new float[snm];
   cnm=0;

   ni=0;
   while(pch=Doc.show(iz)){
      if(ix=pSaa->disk_snfind(pch)){
         len=strlen(pch);
         if((pch[len-1]=='T')&&(pch[len-2]=='!')){
            fq=*(pSaa->freq+(--ix));
            if(fq>dmt)xct=(double)fq;
            else xct=dmt;
            *(wnm+cnm)=l2*log(xttrc/xct);
            *(xnm+cnm)=ix+1;
            cnm++;
         }
      }
   }
   snm=cnm;
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
         if(!pthread_create(&tid[i],&thread_attr,tmscore,(void*)pTd[i])){
            ther++;bot++;
            if(pflag)cout << "thread nfly " << i << endl;
         }
         else {sem_post(&smp);cout << "Thread launch in Nabor failed!" << endl;}
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
   delete [] wnm;
   return(sco);
}

float *Nabor_pth::scoreVec_Thm(Theme *pThm){
   long i,j,k,m,n,id,*pi;
   int pflag=get_qflag();
   float s,*ps,*wwg;
   unsigned char c,*utr;
   long flag_end;

   if(pThm->mtc>term_lim){
      err_flag=1;
      sco=NULL;
      return(NULL);
   }
   else err_flag=0;

   pthread_mutex_lock(&lbot);
   bot=bot%num_thr;
   pthread_mutex_unlock(&lbot);
   pthread_mutex_lock(&ltop);
   top=top%num_thr;
   pthread_mutex_unlock(&ltop);

   if(sco!=NULL)delete [] sco;
   ps=sco=new float[pRgg->ntot];
   for(i=0;i<pRgg->ntot;i++)*(ps++)=pThm->cs;

   ther=0; //Initially zero threads deployed here.

   snm=pThm->mtc;
   xnm=new long[snm];
   for(i=0;i<snm;i++)xnm[i]=*(pSaa->inv+(pThm->mti)[i]);
   wnm=pThm->weg;
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
         if(!pthread_create(&tid[i],&thread_attr,tmscore,(void*)pTd[i])){
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

float Nabor_pth::scoreDoc_Doc(Docum &Doc,Docum &Dcc){
   long i,j,k,m,ni,num,id,ix,zi;
   long icut=1000000,fq;
   int pflag=get_qflag(),len,iz,jz;
   char *pch,*pck;
   double xx,yy,xct,xttrc,dx,dy;
   long dn=Doc.sum_lcnt(),dm=Dcc.sum_lcnt();
   float sum=0;

   if((Doc.num_wrds()>term_lim)||(Dcc.num_wrds()>term_lim)){
      err_flag=1;
      return(0);
   }
   else err_flag=0;

   xttrc=(double)ndoc;

   Doc.reset();
   Dcc.reset();
   ni=0;
   pch=Doc.show(iz);
   pck=Dcc.show(jz);
   while(pch&&pck){
      if(i=strcmp(pch,pck)){
         if(i<0){
            while(pch=Doc.show(iz)){
               if((i=strcmp(pch,pck))>=0)break;
            }
         }
         else if(i>0){
            while(pck=Dcc.show(jz)){
               if((i=strcmp(pch,pck))<=0)break;
            }
         }
      }
      else {
         if(ix=pSaa->disk_snfind(pch)){
            len=strlen(pch);
            if(((pch[len-1]=='t')&&(pch[len-2]=='!'))|| \
              ((pch[len-2]!='!')&&(!strchr(pch,'*')))){
               fq=*(pSaa->freq+(--ix));
               if(fq<=icut){
                  dx=*(*(tf+iz)+dn);
                  dy=*(*(tf+jz)+dm);
                  if(fq>dmt)xct=(double)fq;
                  else xct=dmt;
                  sum+=dx*dy*l2*log(xttrc/xct);
                  cout << ix << "    " << dx << " " << dy << " " << \
                       l2*log(xttrc/xct) << " " << pch << endl;
               }
            }
         }
         pch=Doc.show(iz);
         pck=Dcc.show(jz);
      }
   }
   return(sum);
}

float Nabor_pth::scoreDoc_BDoc(Docum &Doc,long pmid){
   long i,j,k,m,ni,num,id,jd,ix,zi;
   long icut=1000000,fq,ct,*dtid,*dtft,*dtfq;
   int pflag=get_qflag(),len,iz;
   char *pch;
   double xct,xttrc,dx,dy;
   long dn=Doc.sum_lcnt(),dm;
   float sum=0;

   if((m=Doc.num_wrds())>term_lim){
      err_flag=1;
      return(0);
   }
   else err_flag=0;

   xttrc=(double)ndoc;

   dtid=new long[m];
   dtft=new long[m];
   dtfq=new long[m];
   ct=0;
   Doc.reset();
   while(pch=Doc.show(iz)){
      if(id=pSaa->disk_sifind(pch)){
         ix=*(pSaa->inv+id);
         len=strlen(pch);
         if(((pch[len-1]=='t')&&(pch[len-2]=='!'))|| \
           ((pch[len-2]!='!')&&(!strchr(pch,'*')))){
            fq=*(pSaa->freq+(--ix));
            if(fq<=icut){
               dtid[ct]=id;
               dtft[ct]=iz;
               dtfq[ct]=fq;
               ct++;
            }
         }
      }
   }
   hSort(ct,dtid,dtft,dtfq);
   ni=pRgg->index(pmid);
   if(!ni){
      cout << "Document not in system!" << endl;
      return(0);
   }
   ni--;
   num=pRgg->readb(ni);
   hSort(num,pRgg->tnm,pRgg->lct);
   dm=*(dl+ni);
   i=j=0;
   while((i<ct)&&(j<num)){
      id=dtid[i];
      jd=pRgg->tnm[j];
      if(id<jd){
         while((i<ct)&&((id=dtid[i])<jd))i++;
      }
      else if(jd<id){
         while((j<num)&&((jd=pRgg->tnm[j])<id))j++;
      }
      else {
          iz=pRgg->lct[j];
          dy=*(*(tf+iz)+dm);
          iz=dtft[i];
          dx=*(*(tf+iz)+dn);
          fq=dtfq[i];
          if(fq>dmt)xct=(double)fq;
          else xct=dmt;
          sum+=dx*dy*l2*log(xttrc/xct);
          i++;j++;
      }
   }
   return(sum);
}

float Nabor_pth::scoreBDoc_BDoc(long pmid1,long pmid2, char *mrk){
   long i,j,k,m,ni,num,id,jd,ix,zi;
   long icut=1000000,fq,ct,*dtid,*dtft,*dtfq;
   int pflag=get_qflag(),len,iz;
   char *pch;
   float sum=0;
   double xct,xttrc,dx,dy;
   long dm,dn;
   ni=pRgg->index(pmid1);
   if(!ni){
      cout << "Document not in system!" << endl;
      return(0);
   }
   ni--;
   dn=*(dl+ni);
   m=pRgg->readb(ni);

   xttrc=(double)ndoc;

   dtid=new long[m];
   dtft=new long[m];
   dtfq=new long[m];
   ct=0;
   for(j=0;j<m;j++){
      jd=pRgg->tnm[j] ; 
      ix=*(pSaa->inv+jd);
      if(mrk[--ix]){
         fq=*(pSaa->freq+ix);
         if(fq<=icut){
           dtid[ct]=jd;
           dtft[ct]=pRgg->lct[j];
           dtfq[ct]=fq;
           ct++;
         }
      }
   } 
   hSort(ct,dtid,dtft,dtfq);
   ni=pRgg->index(pmid2);
   if(!ni){
      cout << "Document not in system!" << endl;
      return(0);
   }
   ni--;
   num=pRgg->readb(ni);
   hSort(num,pRgg->tnm,pRgg->lct);
   dm=*(dl+ni);
   i=j=0;
   while((i<ct)&&(j<num)){
      id=dtid[i];
      jd=pRgg->tnm[j];
      if(id<jd){
         while((i<ct)&&((id=dtid[i])<jd))i++;
      }
      else if(jd<id){
         while((j<num)&&((jd=pRgg->tnm[j])<id))j++;
      }
      else {
          iz=pRgg->lct[j];
          dy=*(*(tf+iz)+dm);
          iz=dtft[i];
          dx=*(*(tf+iz)+dn);
          fq=dtfq[i];
          if(fq>dmt)xct=(double)fq;
          else xct=dmt;
          sum+=dx*dy*l2*log(xttrc/xct);
          i++;j++;
      }
   }
   return(sum);
}

float Nabor_pth::scoreIDoc_IDoc(long idx1,long idx2, char *mrk){
   long i,j,k,m,ni,num,id,jd,ix,zi;
   long icut=1000000,fq,ct,*dtid,*dtft,*dtfq;
   int pflag=get_qflag(),len,iz;
   char *pch;
   float sum=0;
   double xct,xttrc,dx,dy;
   long dm,dn;
   dn=*(dl+idx1);
   m=pRgg->readb(idx1);

   xttrc=(double)ndoc;

   dtid=new long[m];
   dtft=new long[m];
   dtfq=new long[m];
   ct=0;
   for(j=0;j<m;j++){
      jd=pRgg->tnm[j] ;
      ix=*(pSaa->inv+jd);
      if(mrk[--ix]){
         fq=*(pSaa->freq+ix);
         if(fq<=icut){
           dtid[ct]=jd;
           dtft[ct]=pRgg->lct[j];
           dtfq[ct]=fq;
           ct++;
         }
      }
   }
   hSort(ct,dtid,dtft,dtfq);
   num=pRgg->readb(idx2);
   hSort(num,pRgg->tnm,pRgg->lct);
   dm=*(dl+idx2);
   i=j=0;
   while((i<ct)&&(j<num)){
      id=dtid[i];
      jd=pRgg->tnm[j];
      if(id<jd){
         while((i<ct)&&((id=dtid[i])<jd))i++;
      }
      else if(jd<id){
         while((j<num)&&((jd=pRgg->tnm[j])<id))j++;
      }
      else {
          iz=pRgg->lct[j];
          dy=*(*(tf+iz)+dm);
          iz=dtft[i];
          dx=*(*(tf+iz)+dn);
          fq=dtfq[i];
          if(fq>dmt)xct=(double)fq;
          else xct=dmt;
          sum+=dx*dy*l2*log(xttrc/xct);
          i++;j++;
      }
   }
   return(sum);
}

Order *Nabor_pth::skim(long n){
   if(sco==NULL)return(NULL);
   float sx;
   long i=0;
   Order *pOrd=new Order(n,ndoc,sco);
   pOrd->ind(i,sx);
   if(sx>0)return(pOrd);
   else return(NULL);
}

Order *Nabor_pth::skim(long n,Index *pind){
   if(sco==NULL)return(NULL);
   float sx,*scc;

   if(pind==NULL)return(NULL);
   if(pind->ix<1)return(NULL);

   Order *pOrd=new Order(n,pind,sco);

   long i=0;
   pOrd->ind(i,sx);
   if(sx>0)return(pOrd);
   else return(NULL);
}

}
