#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "runn.h"
#include "Blist.h"
#include "Regist.h"
#include "Btree.h"
#include "Docum.h"
#include "Post.h"
#include "BnBayes.h"
using namespace std;
namespace iret {

BnBayes::BnBayes(void) {
   tx=NULL;
   sx=NULL;
   sco=NULL;
   pRgg=NULL;
   pSaa=NULL;
   err_flag=0;
   count_lim=200000;
}

BnBayes::BnBayes(long lim) {
   tx=NULL;
   sx=NULL;
   sco=NULL;
   pRgg=NULL;
   pSaa=NULL;
   err_flag=0;
   count_lim=lim;
}

BnBayes::~BnBayes(){
   if (tx!=NULL)delete [] tx;
   if (sx!=NULL)delete [] sx;
   if (sco!=NULL)delete [] sco;
}

void BnBayes::gopen_BnBayes(Regist *pReg,Slice_Accomp *pSac){
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
   weg=(float*)sx;
   cflag=1;
   
   if(pflag)cout << "Opening binary document files" << endl;
   pRgg->set_class();
   ndoc=pRgg->ntot;
   pRgg->gopen_read(READ_U);
   pRgg->gopen_binary();
}

void BnBayes::gopen_BnBayes_map(Regist *pReg,Slice_Accomp *pSac){
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
   weg=(float*)sx;
   cflag=1;

   if(pflag)cout << "Opening binary document files" << endl;
   pRgg->set_class();
   ndoc=pRgg->ntot;
   pRgg->gopen_map(READ_U);
   pRgg->gopen_binary_map();
}

void BnBayes::gopen_NoPost(Regist *pReg,Slice_Accomp *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

   pRgg=pReg;
   pSaa=pSac;
   pSaa->inv_load();
   pSaa->gopen_idstring(pSaa->slice_name);

   nid=pSaa->nid;

   tx=new long[nid];
   sx=new long[nid];
   weg=(float*)sx;
   cflag=1;
   
   if(pflag)cout << "Opening binary document files" << endl;
   pRgg->set_class();
   ndoc=pRgg->ntot;
   pRgg->gopen_read(READ_U);
   pRgg->gopen_binary();
}

void BnBayes::zerot(void){
   long i,*pt;
   nnx=0;
   pt=tx;
   for(i=0;i<nid;i++)*(pt++)=0;
}

void BnBayes::zeros(void){
   long i,*ps;
   nsx=0;
   ps=sx;
   for(i=0;i<nid;i++)*(ps++)=0;
   cflag=0;
}

void BnBayes::transfer(void){
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

void BnBayes::countDoc(long i){
   long k,n,*nt;
   n=pRgg->readb(i);
   nt=pRgg->tnm;
   for(k=0;k<n;k++){
      (*(tx+*(nt++)))++;
   }
   nnx++;
}

void BnBayes::counsDoc(long i){
   long k,n,*nt;
   n=pRgg->readb(i);
   nt=pRgg->tnm;
   for(k=0;k<n;k++){
      (*(sx+*(nt++)))++;
   }
   nsx++;
}

void BnBayes::counstDoc(long i){
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

void BnBayes::ncountDoc(long i){
   long k,n,*nt;
   n=pRgg->readb(i);
   nt=pRgg->tnm;
   for(k=0;k<n;k++){
      (*(tx+*(nt++)))--;
   }
   nnx--;
}

void BnBayes::ncounsDoc(long i){
   long k,n,*nt;
   n=pRgg->readb(i);
   nt=pRgg->tnm;
   for(k=0;k<n;k++){
      (*(sx+*(nt++)))--;
   }
   nsx--;
}
void BnBayes::ncounstDoc(long i){
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

void BnBayes::countTot(long ix,long *ikx){
   long i;
   int pflag=get_qflag();

   for(i=0;i<ix;i++){
      this->countDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void BnBayes::countSub(long ix,long *ikx){
   long i;
   int pflag=get_qflag();
   if(cflag){cout << "Error, cflag=1!" << endl;exit(0);}

   for(i=0;i<ix;i++){
      this->counsDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void BnBayes::countBth(long ix,long *ikx){
   long i;
   int pflag=get_qflag();
   if(cflag){cout << "Error, cflag=1!" << endl;exit(0);}

   for(i=0;i<ix;i++){
      this->counstDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void BnBayes::ncountTot(long ix,long *ikx){
   long i;
   int pflag=get_qflag();

   for(i=0;i<ix;i++){
      this->ncountDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void BnBayes::ncountSub(long ix,long *ikx){
   long i;
   int pflag=get_qflag();
   if(cflag){cout << "Error, cflag=1!" << endl;exit(0);}

   for(i=0;i<ix;i++){
      this->ncounsDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void BnBayes::ncountBth(long ix,long *ikx){
   long i;
   int pflag=get_qflag();
   if(cflag){cout << "Error, cflag=1!" << endl;exit(0);}

   for(i=0;i<ix;i++){
      this->ncounstDoc(*(ikx+i));
      mark(pflag,i,100,"docs");
   }
}

void BnBayes::weightSall(float cut){
   long n_t,n_st,flag,min,max;
   long nstw=0,*tt,diff;
   float *wwg;
   double xx,zz,frc,pt,qt,rt;

   if(cflag){cout << "Error, cflag=1!" << endl;exit(0);}
   cs=0;
   frc=((double)nsx)/((double)nnx);
   diff=nsx-nnx;

   *(weg)=0;
   tt=tx+1;
   wwg=weg+1;
   for(long i=1;i<nid;i++){
      n_t=*(tt++);
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
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         zz=(float)(log(pt*(1.0-qt))-log(qt*(1.0-pt)));

         if(fabs((double)zz)>cut){
            xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
            xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
            cs+=(float)xx;
            *(wwg++)=zz;
            nstw++;
         }
         else *(wwg++)=0;
      }
      else {
         *(wwg++)=0;
      }
   }
cout << "cs= " << cs << endl;
cout << "Number of weighted terms= " << nstw << endl;
cflag=1;
}

void BnBayes::weightSneg(float cut){
   long n_t,n_st,flag,min;
   long nstw=0,*tt;
   float *wwg;
   double xx,zz,frc,pt,qt,rt;

   if(cflag){cout << "Error, cflag=1!" << endl;exit(0);}
   cs=0;
   frc=((double)nsx)/((double)nnx);

   *weg=0;
   tt=tx+1;
   wwg=weg+1;
   for(long i=1;i<nid;i++){
      n_t=*(tt++);
      if(n_t){
         n_st=*(sx+i);
         flag=1;
         if(n_st==0){
            if(n_st+1<n_t*frc)n_st +=1;
            else flag=0;
         }
         else if(n_st>=n_t*frc){
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

         if(zz<-cut){
            xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
            xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
            cs+=(float)xx;
            *(wwg++)=zz;
            nstw++;
         }
         else *(wwg++)=0;
      }
      else {
         *(wwg++)=0;
      }
   }
cout << "cs= " << cs << endl;
cout << "Number of weighted terms= " << nstw << endl;
cflag=1;
}

void BnBayes::weightSpos(float cut){
   long n_t,n_st,flag,min;
   long nstw=0,*tt;
   float *wwg;
   double xx,zz,frc,pt,qt,rt;

   if(cflag){cout << "Error, cflag=1!" << endl;exit(0);}
   cs=0;
   frc=((double)nsx)/((double)nnx);

   *weg=0;
   tt=tx+1;
   wwg=weg+1;
   for(long i=1;i<nid;i++){
      n_t=*(tt++);
      if(n_t){
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
      }
      else flag=0;

      if(flag){
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         zz=(float)(log(pt*(1.0-qt))-log(qt*(1.0-pt)));

         if(zz>cut){
            xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
            xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
            cs+=(float)xx;
            *(wwg++)=zz;
            nstw++;
         }
         else *(wwg++)=0;
      }
      else {
         *(wwg++)=0;
      }
   }
cout << "cs= " << cs << endl;
cout << "Number of weighted terms= " << nstw << endl;
cflag=1;
}

void BnBayes::scoreAll(void){
   long i,j,k,m,n,*pz;
   int pflag=get_qflag();
   float s,*ps,*wwg;
   unsigned char c,*utr;
   long nt,yi,od,fq,ix;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   if(!cflag){cout << "Error, cflag=0!" << endl;exit(0);}

   if(sco!=NULL)delete [] sco;
   ps=sco=new float[pRgg->ntot];
   for(i=0;i<pRgg->ntot;i++)*(ps++)=cs;

   pz=pSaa->inv+1;
   wwg=weg+1;
   for(j=1;j<nid;j++){
      ix=*(pz++);
      s=*(wwg++);
      if(s&&ix){
         pk=pSaa->read_comp((--ix));
         ba=pSaa->pw[pk];
         utr=pSaa->cod;
         fq=*(pSaa->freq+ix);
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
   return;
}

void BnBayes::scoreSet(long im,long *imd){
   long i,j,k,n,m,*pi,*nt;
   int pflag=get_qflag();
   float s,*ss;
   if(sco!=NULL)delete [] sco;
   sco=new float[im];

   if(!cflag){cout << "Error, cflag=0!" << endl;exit(0);}

   pi=imd;
   ss=sco;
   for(n=0;n<im;n++){
      i=*(pi++);
      s=0;
      j=pRgg->readb(i);
      nt=pRgg->tnm;
      for(k=0;k<j;k++){
         s+=*(weg+*(nt++));
      }
      *(ss++)=s+cs;
      mark(pflag,n,100,"documents scored");
   }
   return;
}

Order *BnBayes::Similar_One(long n,Index *pind,Index *qind){
   int flag=0;
   long i;
   Index *qnp=NULL;
   if((n<=0)||(pind==NULL))return(NULL);
   if(pind->ix<=0)return(NULL);
   if((qind!=NULL)&&(qind->ix>0)){
      Bool *pBl;
      qnp=pBl->cbool_Butnot(qind,pind);
      if((qnp!=NULL)&&(qnp->ix>0))flag=1;
   }
   if(flag){
      if(n>ndoc-(pind->ix)-(qnp->ix))n=ndoc-(pind->ix)-(qnp->ix);
   }
   else if(n>ndoc-(pind->ix))n=ndoc-(pind->ix);
   
   if((pind->ix>count_lim)||(flag&&(qnp->ix>count_lim))){
      err_flag=1;
      if(qnp!=NULL)delete qnp;
      return(NULL);
   }
   else err_flag=0;

   this->transfer();
   this->zeros();

   this->countSub(pind->ix,pind->idx);
   if(flag)this->ncountTot(qnp->ix,qnp->idx);
   this->weightSall(1.5);

   this->scoreAll();
   for(i=0;i<pind->ix;i++)*(sco+*(pind->idx+i))=-1000000.0;
   if(flag)for(i=0;i<qnp->ix;i++)*(sco+*(qnp->idx+i))=-1000000.0;
   Order *jOrd=new Order(n,ndoc,sco);
   if(qnp!=NULL)delete qnp;
   delete [] sco;
   sco=NULL;
   return(jOrd);
}

Order *BnBayes::Similar_Two(long n,Index *pind,Index *qind){
   if(n<=0)return(NULL);
   if(pind==NULL)return(NULL);
   if(qind==NULL)return(NULL);
   if(pind->ix<=0)return(NULL);
   if(qind->ix<=0)return(NULL);
   Bool *pBl;
   Index *qnp=pBl->cbool_Butnot(qind,pind);
   if(qnp==NULL)return(NULL);
   if(qnp->ix<=0){delete qnp;return(NULL);}
   if(n>qnp->ix)n=qnp->ix;

   if((pind->ix>count_lim)||(qnp->ix>count_lim)){
      err_flag=1;
      delete qnp;
      return(NULL);
   }
   else err_flag=0;

   this->transfer();
   this->zeros();

   this->countSub(pind->ix,pind->idx);
   this->ncountTot(qnp->ix,qnp->idx);
   this->weightSall(1.5);

   this->scoreSet(qnp->ix,qnp->idx);

   Order *jOrd=new Order(n,sco,qnp);
   delete qnp;
   delete [] sco;
   sco=NULL;
   return(jOrd);
}

Order *BnBayes::Similar_Three(long n,Index *pind,Index *zind,Index *qind){
   if(n<=0)return(NULL);
   if(pind==NULL)return(NULL);
   if(zind==NULL)return(NULL);
   if(qind==NULL)return(NULL);
   if(pind->ix<=0)return(NULL);
   if(zind->ix<=0)return(NULL);
   if(qind->ix<=0)return(NULL);
   Bool *pBl;
   Index *qnp=pBl->cbool_Butnot(zind,pind);
   if(qnp==NULL)return(NULL);
   if(qnp->ix<=0){delete qnp;return(NULL);}
   if(n>qind->ix)n=qind->ix;

   if((pind->ix>count_lim)||(qnp->ix>count_lim)){
      err_flag=1;
      delete qnp;
      return(NULL);
   }
   else err_flag=0;

   this->zerot();
   this->zeros();

   this->countBth(pind->ix,pind->idx);
   this->countTot(qnp->ix,qnp->idx);
   this->weightSall(1.5);

   this->scoreSet(qind->ix,qind->idx);

   Order *jOrd=new Order(n,sco,qind);
   delete qnp;
   delete [] sco;
   sco=NULL;
   return(jOrd);
}

Order *BnBayes::Similar2_Two(long n,Index *pind,Index *qind){
   if(n<=0)return(NULL);
   if(pind==NULL)return(NULL);
   if(qind==NULL)return(NULL);
   if(pind->ix<=0)return(NULL);
   if(qind->ix<=0)return(NULL);
   Bool *pBl;
   Index *qnp=pBl->cbool_Butnot(qind,pind);
   if(qnp==NULL)return(NULL);
   if(qnp->ix<=0){delete qnp;return(NULL);}
   if(n>qnp->ix)n=qnp->ix;

   Index *bind=pBl->cbool_Or(pind,qind);
   Index *univ=this->Whole();
   Index *sunv=pBl->cbool_Butnot(univ,bind);
   delete bind;
   delete univ;
   if(sunv==NULL){delete qnp;return(NULL);}
   if(sunv->ix<=0){delete qnp;delete sunv;return(NULL);}
   Index *rind=this->Final_seg(100000,sunv);
   delete sunv;

   Order *jOrd=this->Similar_Three(n,pind,rind,qnp);
   delete qnp;
   delete rind;
   return(jOrd);
}

Order *BnBayes::Similar3_Two(long n,Index *pind,Index *qind){
   float cut=log(19.0);
   long i,j;
   if(n<=0)return(NULL);
   if(pind==NULL)return(NULL);
   if(qind==NULL)return(NULL);
   if(pind->ix<=0)return(NULL);
   if(qind->ix<=0)return(NULL);
   Bool *pBl;
   Index *qnp=pBl->cbool_Butnot(qind,pind);
   if(qnp==NULL)return(NULL);
   if(qnp->ix<=0){delete qnp;return(NULL);}
   if(n>qnp->ix)n=qnp->ix;

   Index *bind=pBl->cbool_Or(pind,qind);
   Index *univ=this->Whole();
   Index *sunv=pBl->cbool_Butnot(univ,bind);
   delete bind;
   delete univ;
   if(sunv==NULL){delete qnp;return(NULL);}
   if(sunv->ix<=0){delete qnp;delete sunv;return(NULL);}
   Index *rind=this->Final_seg(100000,sunv);
   delete sunv;

   if((pind->ix>count_lim)||(rind->ix>count_lim)){
      err_flag=1;
      delete rind;
      delete qnp;
      return(NULL);
   }
   else err_flag=0;

   this->zerot();
   this->zeros();
   this->countBth(pind->ix,pind->idx);
   this->countTot(rind->ix,rind->idx);
   this->weightSall(1.5);

   this->scoreSet(rind->ix,rind->idx);
   j=0;
   for(i=0;i<rind->ix;i++){
      if(*(sco+i)>=cut)j++;
   }
   if(j<100){
      this->scoreSet(qnp->ix,qnp->idx);
      Order *jOrd=new Order(n,sco,qnp);
      delete rind;
      delete qnp;
      return(jOrd);
   }
   Order *qOrd=new Order(j,sco,rind);
   Index *zind=qOrd->pInd;
   this->scoreSet(qnp->ix,qnp->idx);
   float *sc1;
   sc1=new float[qnp->ix];
   for(i=0;i<qnp->ix;i++)sc1[i]=sco[i];
   
   this->zerot();
   this->zeros();
   this->countBth(pind->ix,pind->idx);
   this->countTot(zind->ix,zind->idx);
   this->weightSall(1.5);
   this->scoreSet(qnp->ix,qnp->idx);

   for(i=0;i<qnp->ix;i++){
      if(sc1[i]>cut)sc1[i]+=3.0*sco[i];
   }

   Order *jOrd=new Order(n,sc1,qnp);
   delete [] sc1;
   delete [] sco;
   sco=NULL;
   delete qOrd;
   delete rind;
   delete qnp;
   return(jOrd);
}

Index *BnBayes::Subsample(long n,Index *pind){
   long i,j,*ptr;
   
   if(pind==NULL)return(NULL);
   if(n>=pind->ix)return(pind);
   
   long *udx=new long[pind->ix];
   ptr=udx;
   for(i=0;i<n;i++)*(ptr++)=1;
   for(i=n;i<pind->ix;i++)*(ptr++)=0;
   shuffle(pind->ix,udx);
   Index *rind=new Index(n);
   j=0;
   ptr=udx;
   for(i=0;i<pind->ix;i++){
      if(*(ptr++))*(rind->idx+(j++))=*(pind->idx+i);
   }
   return(rind);
}

Index *BnBayes::Randsample(long n){
   long i,j;
   
   if(n<=0)return(NULL);
   n=(n<ndoc)?n:ndoc;

   long *pt=new long[ndoc];
   for(i=0;i<n;i++)pt[i]=1;
   for(i=n;i<ndoc;i++)pt[i]=0;
   shuffle(ndoc,pt);
   Index *pind=new Index(n);
   j=0;
   for(i=0;i<ndoc;i++){
      if(pt[i]){
         pind->idx[j]=i;
         j++;
      }
   }
   delete [] pt;
   return(pind);
}

Index *BnBayes::Whole(void){
   Index *wind=new Index(ndoc);
   long i,*ptr;
   ptr=wind->idx;
   for(i=0;i<ndoc;i++)*(ptr++)=i;
   return(wind);
}

Index *BnBayes::Interval(long n,long m){
   if(m<=n)return(NULL);
   if(n<0)return(NULL);
   if(ndoc<m)return(NULL);
   Index *pind=new Index(m-n);
   long i,*ptr;
   ptr=pind->idx;
   for(i=n;i<m;i++)*(ptr++)=i;
   return(pind);
}

Index *BnBayes::Initial_seg(long n,Index *pind){
   long i,j,*ptr,*qtr;
   
   if((pind==NULL)||(pind->ix<=0))return(NULL);
   if(n>=pind->ix)n=pind->ix;
   
   Index *rind=new Index(n);
   j=0;
   ptr=pind->idx;
   qtr=rind->idx;
   for(i=0;i<n;i++){
      *(qtr++)=*(ptr++);
   }
   return(rind);
}

Index *BnBayes::Final_seg(long n,Index *pind){
   long i,j,*ptr,*qtr;
   
   if((pind==NULL)||(pind->ix<=0))return(NULL);
   if(n>=pind->ix)n=pind->ix;
   
   Index *rind=new Index(n);
   j=0;
   ptr=pind->idx+pind->ix-n;
   qtr=rind->idx;
   for(i=0;i<n;i++){
      *(qtr++)=*(ptr++);
   }
   return(rind);
}

void BnBayes::debug(long id){
   long i,j,n;
   float sum=0;
   char cnam[max_str];
   i=pRgg->index(id);
   if(i<=0){
      cout << "PMID not found in system!" << endl;
      return;
   }
   n=pRgg->readb(i-1);
   for(j=0;j<n;j++){
      i=pRgg->tnm[j];
      pSaa->disk_ifind(i,cnam);
      if(cflag){
         cout << *(tx+i) << " " << *(weg+i) << " " << cnam << endl;
         sum+=*(weg+i);
      }
      else  cout << *(tx+i) << " " << *(sx+i) << " " << cnam << endl;
   }
   if(cflag){
      sum+=cs;
      cout << endl << "doc score: " << sum << endl;
   }
}

//Multithreading version
   //Access variables
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

extern "C" void *bscore(void *pth_data){
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

extern "C" void *cnt_t(void *pth_data){
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

extern "C" void *cnt_s(void *pth_data){
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

extern "C" void *cnt_b(void *pth_data){
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

extern "C" void *ncnt_t(void *pth_data){
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

extern "C" void *ncnt_s(void *pth_data){
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

extern "C" void *ncnt_b(void *pth_data){
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

extern "C" void *xscore(void *pth_data){
   long i,j,k,bb,*ad,num;
   long u,nu,*pu,*pz;
   float *weg,ss;
   Tb_data *ptd=(Tb_data *)pth_data;
   i=ptd->ii;
   ifstream *pfln=(ptd->pRgg)->pfin+i;
   bb=((ptd->pRgg)->pBn)->mm[i];
   ad=(ptd->pRgg)->offs[i];
   weg=(float*)(ptd->sx);

   pthread_mutex_lock(&((ptd->pRgg)->acc[i]));
   j=0;
   pz=ptd->nmx;
   nu=ptd->beg;
   while((j++)<ptd->nm){
      k=*(pz++)-bb;
      pfln->seekg(*(ad+k),ios::beg);
      pfln->read((char*)&num,sizeof(long));
      pfln->read((char*)(ptd->idn),sizeof(long)*num);
      pu=ptd->idn;
      ss=0;
      for(u=0;u<num;u++){
            ss+=*(weg+*(pu++));
      }
      *(ptd->sco+nu+j-1)+=ss;
   }
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

BnBayes_pth::BnBayes_pth(void) {
   tx=NULL;
   sx=NULL;
   sco=NULL;
   pRgg=NULL;
   pSaa=NULL;
   err_flag=0;
   count_lim=200000;
}

BnBayes_pth::BnBayes_pth(long lim) {
   tx=NULL;
   sx=NULL;
   sco=NULL;
   pRgg=NULL;
   pSaa=NULL;
   err_flag=0;
   count_lim=lim;
}

BnBayes_pth::~BnBayes_pth(){
   if (tx!=NULL)delete [] tx;
   if (sx!=NULL)delete [] sx;
   if (sco!=NULL)delete [] sco;
}

void BnBayes_pth::gopen_BnBayes_pth(Regist_pth *pReg,Slice_Accpth *pSac){
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
   weg=(float*)sx;
   cflag=1;
   
   if(pflag)cout << "Opening binary document files" << endl;
   pRgg->set_class();
   ndoc=pRgg->ntot;
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

void BnBayes_pth::gopen_BnBayes_map(Regist_pth *pReg,Slice_Accpth *pSac){
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
   weg=(float*)sx;
   cflag=1;

   if(pflag)cout << "Opening binary document files" << endl;
   pRgg->set_class();
   ndoc=pRgg->ntot;
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

void BnBayes_pth::gopen_NoPost_pth(Regist_pth *pReg,Slice_Accpth *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

   pRgg=pReg;
   pSaa=pSac;
   pSaa->inv_load();
   pSaa->gopen_idstring(pSaa->slice_name);

   nid=pSaa->nid;

   tx=new long[nid];
   sx=new long[nid];
   weg=(float*)sx;
   cflag=1;
   
   if(pflag)cout << "Opening binary document files" << endl;
   pRgg->set_class();
   ndoc=pRgg->ntot;
   pRgg->gopen_read(READ_U);
   pRgg->gopen_binary_pth();

   //Multithreading setup
   if(pthread_mutex_init(&lsco,NULL)){
      cout << "Error in lsco mutex initialization!" << endl;
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

void BnBayes_pth::zerot(void){
   long i,*pt;
   nnx=0;
   pt=tx;
   for(i=0;i<nid;i++)*(pt++)=0;
}

void BnBayes_pth::zeros(void){
   long i,*ps;
   nsx=0;
   ps=sx;
   for(i=0;i<nid;i++)*(ps++)=0;
   cflag=0;
}

void BnBayes_pth::transfer(void){
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

void BnBayes_pth::countTot(long ix,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ix==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ix-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,cnt_t,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nnx+=v;
            if(pflag)cout << "thread countTot " << i << endl;
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

void BnBayes_pth::countSub(long ix,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ix==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ix-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,cnt_s,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nsx+=v;
            if(pflag)cout << "thread countSub " << i << endl;
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

void BnBayes_pth::countBth(long ix,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ix==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ix-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,cnt_b,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nnx+=v;nsx+=v;
            if(pflag)cout << "thread countBth " << i << endl;
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

void BnBayes_pth::ncountTot(long ix,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ix==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ix-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,ncnt_t,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nnx-=v;
            if(pflag)cout << "thread ncountTot " << i << endl;
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

void BnBayes_pth::ncountSub(long ix,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ix==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ix-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,ncnt_s,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nsx-=v;
            if(pflag)cout << "thread ncountSub " << i << endl;
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

void BnBayes_pth::ncountBth(long ix,long *ikx){
   long i,j,u,v,z,*ut,lm;
   int pflag=get_qflag(),flag_end;

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(ix==0)return;
   i=(pRgg->pBn)->index(*ikx);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=ikx+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=ix-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,ncnt_b,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nnx-=v;nsx-=v;
            if(pflag)cout << "thread ncountBth " << i << endl;
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

void BnBayes_pth::weightSall(float cut){
   long n_t,n_st,flag,min,max;
   long nstw=0,*tt,diff;
   float *wwg;
   double xx,zz,frc,pt,qt,rt;

   if(cflag){cout << "Error, cflag=1!" << endl;exit(0);}
   cs=0;
   frc=((double)nsx)/((double)nnx);
   diff=nsx-nnx;

   *(weg)=0;
   tt=tx+1;
   wwg=weg+1;
   for(long i=1;i<nid;i++){
      n_t=*(tt++);
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
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         zz=(float)(log(pt*(1.0-qt))-log(qt*(1.0-pt)));

         if(fabs((double)zz)>cut){
            xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
            xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
            cs+=(float)xx;
            *(wwg++)=zz;
            nstw++;
         }
         else *(wwg++)=0;
      }
      else {
         *(wwg++)=0;
      }
   }
cout << "cs= " << cs << endl;
cout << "Number of weighted terms= " << nstw << endl;
cflag=1;
}

void BnBayes_pth::weightSneg(float cut){
   long n_t,n_st,flag,min;
   long nstw=0,*tt;
   float *wwg;
   double xx,zz,frc,pt,qt,rt;

   if(cflag){cout << "Error, cflag=1!" << endl;exit(0);}
   cs=0;
   frc=((double)nsx)/((double)nnx);

   *weg=0;
   tt=tx+1;
   wwg=weg+1;
   for(long i=1;i<nid;i++){
      n_t=*(tt++);
      if(n_t){
         n_st=*(sx+i);
         flag=1;
         if(n_st==0){
            if(n_st+1<n_t*frc)n_st +=1;
            else flag=0;
         }
         else if(n_st>=n_t*frc){
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

         if(zz<-cut){
            xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
            xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
            cs+=(float)xx;
            *(wwg++)=zz;
            nstw++;
         }
         else *(wwg++)=0;
      }
      else {
         *(wwg++)=0;
      }
   }
cout << "cs= " << cs << endl;
cout << "Number of weighted terms= " << nstw << endl;
cflag=1;
}

void BnBayes_pth::weightSpos(float cut){
   long n_t,n_st,flag,min;
   long nstw=0,*tt;
   float *wwg;
   double xx,zz,frc,pt,qt,rt;

   if(cflag){cout << "Error, cflag=1!" << endl;exit(0);}
   cs=0;
   frc=((double)nsx)/((double)nnx);

   *weg=0;
   tt=tx+1;
   wwg=weg+1;
   for(long i=1;i<nid;i++){
      n_t=*(tt++);
      if(n_t){
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
      }
      else flag=0;

      if(flag){
         pt =(double)n_st/(double)nsx;
         qt =(double)(n_t-n_st)/(double)(nnx-nsx);
         rt =(double)n_t/(double)nnx;
         //calculate wt for this term
         zz=(float)(log(pt*(1.0-qt))-log(qt*(1.0-pt)));

         if(zz>cut){
            xx=log((double)(nsx-n_st))+log((double)(nnx-nsx));
            xx-=(log((double)nsx)+log((double)(nnx-nsx-n_t+n_st)));
            cs+=(float)xx;
            *(wwg++)=zz;
            nstw++;
         }
         else *(wwg++)=0;
      }
      else {
         *(wwg++)=0;
      }
   }
cout << "cs= " << cs << endl;
cout << "Number of weighted terms= " << nstw << endl;
cflag=1;
}

void BnBayes_pth::scoreAll(void){
   long i,j,k,m,n,*pz;
   int pflag=get_qflag();
   float s,*ps,*wwg;
   unsigned char c,*utr;
   long nt,yi,od,fq,ix,flag_end;
   long ba,pk,mi,mj,cu1,sg,cu2,bc;

   if(!cflag){cout << "Error, cflag=0!" << endl;exit(0);}

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

   snm=nid-1;
   xnm=pSaa->inv+1;
   wnm=weg+1;
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
         if(!pthread_create(&tid[i],&thread_attr,bscore,(void*)pTd[i])){
            ther++;bot++;
            if(pflag)cout << "thread scoreAll " << i << endl;
         }
         else {sem_post(&smp);cout << "Thread launch in BnBayes failed!" << endl;}
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
   return;
}

void BnBayes_pth::scoreSet(long im,long *imd){
   long i,j,u,v,z,*ut,lm;
   float *ps;
   int pflag=get_qflag(),flag_end;

   if(sco!=NULL)delete [] sco;
   ps=sco=new float[im];
   for(i=0;i<im;i++)*(ps++)=cs;

   if(!cflag){cout << "Error, cflag=0!" << endl;exit(0);}

   pthread_mutex_lock(&lbbt);
   bot=bot%num_thb;
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_lock(&ltbp);
   top=top%num_thb;
   pthread_mutex_unlock(&ltbp);

   if(im==0)return;
   i=(pRgg->pBn)->index(*imd);
   tber=0; //Initially zero threads deployed here.
   u=0;
   while(i<pRgg->ndst){
      ut=imd+u;
      v=0;
      lm=(pRgg->pBn)->mm[i+1];
      z=im-u;
      while((v<z)&&(*ut<lm)){ut++;v++;}
      if(v){
         //launch thread
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];
         pTb[j]->set_sco_data(i,u,v,imd+u,sco,sx);
         if(!pthread_create(&tib[j],&thread_attr,xscore,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;
            if(pflag)cout << "thread scoreSet " << i << endl;
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

Order *BnBayes_pth::Similar_One(long n,Index *pind,Index *qind){
   int flag=0;
   long i;
   Index *qnp=NULL;
   if((n<=0)||(pind==NULL))return(NULL);
   if(pind->ix<=0)return(NULL);
   if((qind!=NULL)&&(qind->ix>0)){
      Bool *pBl;
      qnp=pBl->cbool_Butnot(qind,pind);
      if((qnp!=NULL)&&(qnp->ix>0))flag=1;
   }
   if(flag){
      if(n>ndoc-(pind->ix)-(qnp->ix))n=ndoc-(pind->ix)-(qnp->ix);
   }
   else if(n>ndoc-(pind->ix))n=ndoc-(pind->ix);

   if((pind->ix>count_lim)||(flag&&(qnp->ix>count_lim))){
      err_flag=1;
      if(qnp!=NULL)delete qnp;
      return(NULL);
   }
   else err_flag=0;
   
   this->transfer();
   this->zeros();

   this->countSub(pind->ix,pind->idx);
   if(flag)this->ncountTot(qnp->ix,qnp->idx);
   this->weightSall(1.5);

   this->scoreAll();
   for(i=0;i<pind->ix;i++)*(sco+*(pind->idx+i))=-1000000.0;
   if(flag)for(i=0;i<qnp->ix;i++)*(sco+*(qnp->idx+i))=-1000000.0;
   Order *jOrd=new Order(n,ndoc,sco);
   if(qnp!=NULL)delete qnp;
   delete [] sco;
   sco=NULL;
   return(jOrd);
}

Order *BnBayes_pth::Similar_Two(long n,Index *pind,Index *qind){
   if(n<=0)return(NULL);
   if(pind==NULL)return(NULL);
   if(qind==NULL)return(NULL);
   if(pind->ix<=0)return(NULL);
   if(qind->ix<=0)return(NULL);
   Bool *pBl;
   Index *qnp=pBl->cbool_Butnot(qind,pind);
   if(qnp==NULL)return(NULL);
   if(qnp->ix<=0){delete qnp;return(NULL);}
   if(n>qnp->ix)n=qnp->ix;

   if((pind->ix>count_lim)||(qnp->ix>count_lim)){
      err_flag=1;
      delete qnp;
      return(NULL);
   }
   else err_flag=0;

   this->transfer();
   this->zeros();

   this->countSub(pind->ix,pind->idx);
   this->ncountTot(qnp->ix,qnp->idx);
   this->weightSall(1.5);

   this->scoreSet(qnp->ix,qnp->idx);

   Order *jOrd=new Order(n,sco,qnp);
   delete qnp;
   delete [] sco;
   sco=NULL;
   return(jOrd);
}

Order *BnBayes_pth::Similar_Three(long n,Index *pind,Index *zind,Index *qind){
   if(n<=0)return(NULL);
   if(pind==NULL)return(NULL);
   if(zind==NULL)return(NULL);
   if(qind==NULL)return(NULL);
   if(pind->ix<=0)return(NULL);
   if(zind->ix<=0)return(NULL);
   if(qind->ix<=0)return(NULL);
   Bool *pBl;
   Index *qnp=pBl->cbool_Butnot(zind,pind);
   if(qnp==NULL)return(NULL);
   if(qnp->ix<=0){delete qnp;return(NULL);}
   if(n>qind->ix)n=qind->ix;

   if((pind->ix>count_lim)||(qnp->ix>count_lim)){
      err_flag=1;
      delete qnp;
      return(NULL);
   }
   else err_flag=0;

   this->zerot();
   this->zeros();

   this->countBth(pind->ix,pind->idx);
   this->countTot(qnp->ix,qnp->idx);
   this->weightSall(1.5);

   this->scoreSet(qind->ix,qind->idx);

   Order *jOrd=new Order(n,sco,qind);
   delete qnp;
   delete [] sco;
   sco=NULL;
   return(jOrd);
}

Order *BnBayes_pth::Similar2_Two(long n,Index *pind,Index *qind){
   if(n<=0)return(NULL);
   if(pind==NULL)return(NULL);
   if(qind==NULL)return(NULL);
   if(pind->ix<=0)return(NULL);
   if(qind->ix<=0)return(NULL);
   Bool *pBl;
   Index *qnp=pBl->cbool_Butnot(qind,pind);
   if(qnp==NULL)return(NULL);
   if(qnp->ix<=0){delete qnp;return(NULL);}
   if(n>qnp->ix)n=qnp->ix;

   Index *bind=pBl->cbool_Or(pind,qind);
   Index *univ=this->Whole();
   Index *sunv=pBl->cbool_Butnot(univ,bind);
   delete bind;
   delete univ;
   if(sunv==NULL){delete qnp;return(NULL);}
   if(sunv->ix<=0){delete qnp;delete sunv;return(NULL);}
   Index *rind=this->Final_seg(100000,sunv);
   delete sunv;

   Order *jOrd=this->Similar_Three(n,pind,rind,qnp);
   delete qnp;
   delete rind;
   return(jOrd);
}

Order *BnBayes_pth::Similar3_Two(long n,Index *pind,Index *qind){
   float cut=log(19.0);
   long i,j;
   if(n<=0)return(NULL);
   if(pind==NULL)return(NULL);
   if(qind==NULL)return(NULL);
   if(pind->ix<=0)return(NULL);
   if(qind->ix<=0)return(NULL);
   Bool *pBl;
   Index *qnp=pBl->cbool_Butnot(qind,pind);
   if(qnp==NULL)return(NULL);
   if(qnp->ix<=0){delete qnp;return(NULL);}
   if(n>qnp->ix)n=qnp->ix;

   Index *bind=pBl->cbool_Or(pind,qind);
   Index *univ=this->Whole();
   Index *sunv=pBl->cbool_Butnot(univ,bind);
   delete bind;
   delete univ;
   if(sunv==NULL){delete qnp;return(NULL);}
   if(sunv->ix<=0){delete qnp;delete sunv;return(NULL);}
   Index *rind=this->Final_seg(100000,sunv);
   delete sunv;

   if((pind->ix>count_lim)||(rind->ix>count_lim)){
      err_flag=1;
      delete rind;
      delete qnp;
      return(NULL);
   }
   else err_flag=0;

   this->zerot();
   this->zeros();
   this->countBth(pind->ix,pind->idx);
   this->countTot(rind->ix,rind->idx);
   this->weightSall(1.5);

   this->scoreSet(rind->ix,rind->idx);
   j=0;
   for(i=0;i<rind->ix;i++){
      if(*(sco+i)>=cut)j++;
   }
   if(j<100){
      this->scoreSet(qnp->ix,qnp->idx);
      Order *jOrd=new Order(n,sco,qnp);
      delete rind;
      delete qnp;
      return(jOrd);
   }
   Order *qOrd=new Order(j,sco,rind);
   Index *zind=qOrd->pInd;
   this->scoreSet(qnp->ix,qnp->idx);
   float *sc1;
   sc1=new float[qnp->ix];
   for(i=0;i<qnp->ix;i++)sc1[i]=sco[i];
   
   this->zerot();
   this->zeros();
   this->countBth(pind->ix,pind->idx);
   this->countTot(zind->ix,zind->idx);
   this->weightSall(1.5);
   this->scoreSet(qnp->ix,qnp->idx);

   for(i=0;i<qnp->ix;i++){
      if(sc1[i]>cut)sc1[i]+=3.0*sco[i];
   }

   Order *jOrd=new Order(n,sc1,qnp);
   delete [] sc1;
   delete [] sco;
   sco=NULL;
   delete qOrd;
   delete rind;
   delete qnp;
   return(jOrd);
}

Index *BnBayes_pth::Subsample(long n,Index *pind){
   long i,j,*ptr;
   
   if(pind==NULL)return(NULL);
   if(n>=pind->ix)return(pind);
   
   long *udx=new long[pind->ix];
   ptr=udx;
   for(i=0;i<n;i++)*(ptr++)=1;
   for(i=n;i<pind->ix;i++)*(ptr++)=0;
   shuffle(pind->ix,udx);
   Index *rind=new Index(n);
   j=0;
   ptr=udx;
   for(i=0;i<pind->ix;i++){
      if(*(ptr++))*(rind->idx+(j++))=*(pind->idx+i);
   }
   return(rind);
}

Index *BnBayes_pth::Randsample(long n){
   long i,j;
   
   if(n<=0)return(NULL);
   n=(n<ndoc)?n:ndoc;

   long *pt=new long[ndoc];
   for(i=0;i<n;i++)pt[i]=1;
   for(i=n;i<ndoc;i++)pt[i]=0;
   shuffle(ndoc,pt);
   Index *pind=new Index(n);
   j=0;
   for(i=0;i<ndoc;i++){
      if(pt[i]){
         pind->idx[j]=i;
         j++;
      }
   }
   delete [] pt;
   return(pind);
}

Index *BnBayes_pth::Whole(void){
   Index *wind=new Index(ndoc);
   long i,*ptr;
   ptr=wind->idx;
   for(i=0;i<ndoc;i++)*(ptr++)=i;
   return(wind);
}

Index *BnBayes_pth::Interval(long n,long m){
   if(m<=n)return(NULL);
   if(n<0)return(NULL);
   if(ndoc<m)return(NULL);
   Index *pind=new Index(m-n);
   long i,*ptr;
   ptr=pind->idx;
   for(i=n;i<m;i++)*(ptr++)=i;
   return(pind);
}

Index *BnBayes_pth::Initial_seg(long n,Index *pind){
   long i,j,*ptr,*qtr;
   
   if((pind==NULL)||(pind->ix<=0))return(NULL);
   if(n>=pind->ix)n=pind->ix;
   
   Index *rind=new Index(n);
   j=0;
   ptr=pind->idx;
   qtr=rind->idx;
   for(i=0;i<n;i++){
      *(qtr++)=*(ptr++);
   }
   return(rind);
}

Index *BnBayes_pth::Final_seg(long n,Index *pind){
   long i,j,*ptr,*qtr;
   
   if((pind==NULL)||(pind->ix<=0))return(NULL);
   if(n>=pind->ix)n=pind->ix;
   
   Index *rind=new Index(n);
   j=0;
   ptr=pind->idx+pind->ix-n;
   qtr=rind->idx;
   for(i=0;i<n;i++){
      *(qtr++)=*(ptr++);
   }
   return(rind);
}

void BnBayes_pth::debug(long id){
   long i,j,n;
   float sum=0;
   char cnam[max_str];
   i=pRgg->index(id);
   if(i<=0){
      cout << "PMID not found in system!" << endl;
      return;
   }
   n=pRgg->readb(i-1);
   for(j=0;j<n;j++){
      i=pRgg->tnm[j];
      pSaa->disk_ifind(i,cnam);
      if(cflag){
         cout << *(tx+i) << " " << *(weg+i) << " " << cnam << endl;
         sum+=*(weg+i);
      }
      else  cout << *(tx+i) << " " << *(sx+i) << " " << cnam << endl;
   }
   if(cflag){
      sum+=cs;
      cout << endl << "doc score: " << sum << endl;
   }
}

}
