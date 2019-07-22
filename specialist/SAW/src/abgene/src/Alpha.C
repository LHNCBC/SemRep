#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <runn.h>
#include <Blist.h>
#include <Regist.h>
#include <Btree.h>
#include <Docum.h>
#include <Post.h>
#include "Alpha.h"
#include "Cut.h"


using namespace std;


namespace iret {

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

   
   static long snum;
   static double cut;
   static float css, ss,cssy, ssy;
   static long pnnx, pnsx, pnrx, *rx;
   static Cut *pCut;

  
   static pthread_cond_t condb; //Condition varialble for end.
   static pthread_mutex_t lcnt; //lock for tx count updating
   static pthread_mutex_t lcns; //lock for sx count updating
   static pthread_mutex_t ltber; //lock for thread counting
   static int tber; //Number of register threads deployed in this class

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


extern "C" void *cnt_r(void *pth_data){
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


Alpha_pth::Alpha_pth(void) {
   tx=NULL;
   sx=NULL;
   pRgg=NULL;
   pSaa=NULL;
   rx=NULL;
}

Alpha_pth::~Alpha_pth(){
   if (tx!=NULL)delete [] tx;
   if (sx!=NULL)delete [] sx;
   if (rx!=NULL) delete [] rx;
}

void Alpha_pth::gopen_Alpha_map(Regist_pth *pReg,Slice_Accpth *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j,k;

   pRgg=pReg;

   pSaa=pSac;
   pSaa->inv_map();
   pSaa->post_access_map();
   pSaa->gopen_idstring(pSaa->slice_name);
   nid=pSaa->nid;
   tx=new long[nid];
   sx=new long[nid];
   rx=new long[nid];
   
   if(pflag)cout << "Opening binary document files" << endl;
   pRgg->set_class();
   ndoc=pRgg->ntot;
   pRgg->gopen_map(READ_U);
   pRgg->gopen_binary_map();
   
   pCut=new Cut;
   pCut->gopen_cut_map();
   
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
  
void Alpha_pth::zerot(void){
   long i,*pt;
   nnx=0;
   pt=tx;
   for(i=0;i<nid;i++)*(pt++)=0;
}

void Alpha_pth::zeros(void){
   long i,*ps;
   nsx=0;
   ps=sx;
   for(i=0;i<nid;i++)*(ps++)=0;
   
}

void Alpha_pth::zeror(void){
   long i,*ps;
   nrx=0;
   ps=rx;
   for(i=0;i<nid;i++)*(ps++)=0;
   
}


void Alpha_pth::transfer(void){
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

void Alpha_pth::countTot(long ix,long *ikx){
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

void Alpha_pth::countSub(long ix,long *ikx){
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
         else {sem_post(&smb);cout << "Thread(b) launch in Alpha failed!" << endl;}
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

void Alpha_pth::countRxx(long ix,long *ikx){
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
         pTb[j]->set_cnt_data(i,v,ikx+u,sx,rx);
         if(!pthread_create(&tib[j],&thread_attr,cnt_r,(void*)pTb[j])){
            tber++;bbt++;i++;u+=v;nrx+=v;nsx+=v;
            if(pflag)cout << "thread countSub " << i << endl;
         }
         else {sem_post(&smb);cout << "Thread(b) launch in Alpha failed!" << endl;}
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



float Alpha_pth::scoreSet(long im, float &salpha){
   long i,j,k,n,m,dd,flag_end, begin, end;
   int pflag=get_qflag();
   float score;
   snum=im;
   pnsx=nsx;
   pnnx=nnx;
   pnrx=nrx;
   ss=0;
   css=0;
   ssy=0;
   cssy=0;
   dd = nid/4 + 1;
   tber=0;
   i=0;
   begin =0;
   while(i< 3){
         end=dd * (i+1);
         
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];

         
         pTb[j]->set_cnt_data(begin,end,&j,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,score_s,(void*)pTb[j])){
            tber++; bbt++;
            if(pflag) cout << "thread score_s " << j << endl;
         }
         else {cout << "Thread(b) launch in Alpha failed!" << endl;}
         begin=end;
         i++; 
         pthread_mutex_unlock(&lbbt);
         pthread_mutex_unlock(&ltber);   
        
   }
   sem_wait(&smb);
   pthread_mutex_lock(&ltber);
   pthread_mutex_lock(&lbbt);
   j=adb[bbt%num_thb];
   
   end = nid;
   pTb[j]->set_cnt_data(begin,end,&j,sx,tx);
   if(!pthread_create(&tib[j],&thread_attr,score_s,(void*)pTb[j])){
      tber++; bbt++;
      if(pflag) cout << "thread score_s " << j << endl;
   }
   else {cout << "Thread(b) launch in Alpha failed!" << endl;}
   pthread_mutex_unlock(&lbbt);
   pthread_mutex_unlock(&ltber);    
  
   flag_end=1;
   do {
      pthread_mutex_lock(&ltber);
      if(tber)pthread_cond_wait(&condb,&ltber);
      else flag_end=0;
      pthread_mutex_unlock(&ltber);
   }while(flag_end);

   
   score=ss/im+css;
   salpha=ssy/(im*im)+cssy;
   nsx=0;
   nrx=0;
   
   return(score);
}


extern "C" void *score_s(void *pth_data){
   long begin, end, nstw=0;
   Tb_data *ptd=(Tb_data *)pth_data;
   begin=ptd->ii;
   end=ptd->nm;
   
   
   long i,j,k,n,m;
   
   
   float s=0, sy=0,cs=0,csy=0;
   long n_t,n_st,flag,min,diff, max,sig;
   
   double xx,frc,pt,qt,rt, alpha;
   frc=((double)pnsx)/((double)(pnnx+pnrx));
   diff=pnsx-pnnx-pnrx;
   
   for(n=begin;n<end;n++){   
      n_t = ptd->tx[n]+rx[n];
      if(n_t&&(n_t<pnnx+pnrx)){
         min=(n_t<pnsx)?n_t:pnsx;
         max=n_t+diff;
         max=(0<max)?max:0;
         n_st=*(ptd->sx+n);
         flag=1;
         if(n_st==min){
            if(n_st-1>n_t*frc)n_st -=1;
            else flag=0;
         }
         else if(n_st<=max){
            if(max+1<n_t*frc)n_st=max+1;
            flag=0;
         }

       }
       else flag=0;
       if(flag){
         sig=pCut->signif(ptd->sx[n]-1,pnsx-1,n_t-1,pnnx+pnrx-1);
         if(sig){
                nstw++;  
         	pt =(double)n_st/(double)pnsx;
                qt =(double)(n_t-n_st)/(double)(pnnx+pnrx-pnsx);
                
                //calculate wt for this term
                xx=(log(pt*(1.0-qt))-log(qt*(1.0-pt)));
                sy+=xx*(ptd->sx[n])*(ptd->sx[n]);
                s+= xx*(ptd->sx[n]);           
                //originally cs is computed below
                xx=log((double)(pnsx-n_st))+log((double)(pnnx+pnrx-pnsx));
                xx-=(log((double)pnsx)+log((double)(pnnx+pnrx-pnsx-n_t+n_st)));
                csy+=(xx*ptd->sx[n])/double(pnsx);
                cs+=xx;
         }
        } 
        *(ptd->sx+n) = 0; 
         rx[n]=0;   
    }     
    pthread_mutex_lock(&lcns);
    ss+=s;
    css+=cs;
    ssy+=sy;
    cssy+=csy;   
    pthread_mutex_unlock(&lcns);
    
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

Index *Alpha_pth::Randsample(long n){
   long i,j;
   
   if(n<=0)return(NULL);
   n=(n<ndoc)?n:ndoc;

   long *pt=new long[ndoc];
   for(i=0;i<n;i++)pt[i]=1;
   for(i=n;i<ndoc;i++)pt[i]=0;
   long seed=234;
   srandom((unsigned int)seed);
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

Index *Alpha_pth::Subsample(long n, Index *idx){
   long i,j,*ptr,ix;
   ix=idx->ix;
   Index *pnd=new Index(idx);
   if(n>=ix) return(pnd);
   long seed=1203;
   srandom((unsigned int)seed);   

   shuffle(pnd->ix,pnd->idx);
   Index *rind=new Index(n);
   
   for(i=0;i<n;i++) rind->idx[i]=pnd->idx[i];
   rind->sSort();
   
  
   return(rind);
}



}
