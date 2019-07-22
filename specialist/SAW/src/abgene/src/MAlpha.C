#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "MAlpha.h"
#include <runn.h>
#include <Blist.h>
#include <Regist.h>
#include <Docum.h>

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

static double cut;
static float css, ss;
static long pnnx, pnsx;
static Hyper *pHyp;
static float *wtxx;
static long nstwxx;

  
static pthread_cond_t condb; //Condition varialble for end.
static pthread_mutex_t lcnt; //lock for tx count updating
static pthread_mutex_t lcns; //lock for sx count updating
static pthread_mutex_t ltber; //lock for thread counting
static int tber; //Number of register threads deployed in this class


extern "C" void *weight_s(void *pth_data){
   long begin, end, nstw=0;
   Tb_data *ptd=(Tb_data *)pth_data;
   begin=ptd->ii;
   end=ptd->nm;
   
   
   long i,j,k,n,m;
   
   
   float cs=0;
   long n_t,n_st,flag,min,diff, max;
   
   double xx,frc,pt,qt,rt, alpha;
   
   frc=((double)pnsx)/((double)pnnx);
   diff=pnsx-pnnx;
   
   for(n=begin;n<end;n++){   
       n_t = ptd->tx[n];
       n_st=*(ptd->sx+n); 
       if(frc*n_t+1>=n_st) flag=0;
       else if(n_t&&(n_t<pnnx)){
           min=(n_t<pnsx)?n_t:pnsx;
           max=n_t+diff;
           max=(0<max)?max:0;
            
           flag=1;
           if(n_st==min) n_st -=1;
           else if(n_st<=max) flag=0;
       }
       else flag=0;
       if(flag){
          xx=pHyp->nlog_pval_appx(ptd->sx[n]-1,pnsx-1,n_t-1,pnnx-1);
        
          if(xx > cut){
                nstw++;  
         	pt =(double)n_st/(double)pnsx;
                qt =(double)(n_t-n_st)/(double)(pnnx-pnsx);
                
                //calculate wt for this term
                xx=(log(pt*(1.0-qt))-log(qt*(1.0-pt)));
                wtxx[n]=xx;
                            
                //originally cs is computed below
                xx=log((double)(pnsx-n_st))+log((double)(pnnx-pnsx));
                xx-=(log((double)pnsx)+log((double)(pnnx-pnsx-n_t+n_st)));
                cs+=xx;
 
          }else wtxx[n]=0.0;
     
       }else wtxx[n]=0.0; 
        
         
    }     
    pthread_mutex_lock(&lcns);
   
    css+=cs;
    nstwxx+=nstw;   
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




MAlpha_pth::MAlpha_pth() : BnBayes_pth() {
   wtxx=NULL;
   
}

MAlpha_pth::~MAlpha_pth(){
   
   if(wtxx!=NULL) delete []wtxx;
   
}

void MAlpha_pth::gopen_MAlpha_map(Regist_pth *pReg,Slice_Accpth *pSac){
   this->gopen_BnBayes_map(pReg,pSac);
   wtxx=new float[nid];  
   pHyp = new Hyper(ndoc, 0.01);

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

double MAlpha_pth::scoreTerm(long n, long *idx){
   long i;
   double score=0;
   for(i=0;i<n;i++) score+=sco[idx[i]];
   return (score/n);
}

void MAlpha_pth::weight_pth(double cutxx){
   weg=NULL;
   long i,j,k,n,m,dd,flag_end, begin, end;
   int pflag=get_qflag();
   pnsx=nsx;
   pnnx=nnx;
   cut=cutxx;   
   css=0;
   nstwxx=0;
   dd = nid/10 + 1;
   tber=0;
   i=0;
   begin =0;
   while(i< 9){
         end=dd * (i+1);
         sem_wait(&smb);
         pthread_mutex_lock(&ltber);
         pthread_mutex_lock(&lbbt);
         j=adb[bbt%num_thb];

         pTb[j]->set_cnt_data(begin,end,&j,sx,tx);
         if(!pthread_create(&tib[j],&thread_attr,weight_s,(void*)pTb[j])){
            tber++; bbt++;
            if(pflag) cout << "thread weight_s " << j << endl;
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
   if(!pthread_create(&tib[j],&thread_attr,weight_s,(void*)pTb[j])){
      tber++; bbt++;
      if(pflag) cout << "thread weight_s " << j << endl;
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
   weg=wtxx;
   cs=css;
   cout << "cs= " << cs << endl;
   cout << "Number of weighted terms= " << nstwxx << endl;
   cflag=1;
   
}

}
