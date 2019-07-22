#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "Island.h"
using namespace std;
namespace iret {

Island::Island(long llm,long maxstr){
   long i;
   lowlm=llm;
   lcap=maxstr;
   szlm=new long[maxstr];
   mm1=new long[maxstr];
   mm2=new long[maxstr];
   ctc=new long*[maxstr];
   for(i=0;i<maxstr;i++){
      ctc[i]=new long[i+1];
   }
}

Island::~Island(void){
  long i;
  delete [] szlm;
  delete [] mm1;
  delete [] mm2;
  for(i=0;i<lcap;i++){
     delete [] ctc[i];
  }
  delete [] ctc; 
}

void Island::Dpset(char *tst){
   long i,j;
   char c;
   test=tst;
   slen=strlen(tst);
   for(i=0;i<slen;i++)szlm[i]=lowlm;

   //Loop
   i=1;
   while(i<slen){
      if(i&1){
         c=test[i];
         for(j=0;j<i;j++){
            if(c==test[j])mm2[j]=1;
            else mm2[j]=0;
         }
         //Look back
         for(j=1;j<i;j++){
            if(mm2[j])mm2[j]+=mm1[j-1];
            szlm[i]=(szlm[i]<mm2[j]+1)?(mm2[j]+1):szlm[i];
         }
      }
      else {
         c=test[i];
         for(j=0;j<i;j++){
            if(c==test[j])mm1[j]=1;
            else mm1[j]=0;
         }
         //Look back
         for(j=1;j<i;j++){
            if(mm1[j])mm1[j]+=mm2[j-1];
            szlm[i]=(szlm[i]<mm1[j]+1)?(mm1[j]+1):szlm[i];
         }
      }
      i++;
   }
}

void Island::Zerot(void){
   long i,j;
   for(i=0;i<slen;i++){
      for(j=0;j<i+1;j++){
         *(ctc[i]+j)=0;
      }
   }
}

void Island::Iscount(long beg,Strset *pSt){
   long i,j,k,len;
   char c,*pch;
   pSt->gopen_map();

   for(k=beg;k<pSt->num;k++){
      pch=pSt->show_str(k);
      len=strlen(pch);

      //Set first array
      c=test[0];
      for(j=0;j<len;j++){
         if(c==pch[j])mm1[j]=1;
         else mm1[j]=0;
      }
      
      //Loop
      i=1;
      while(i<slen){
         if(i&1){
            c=test[i];
            for(j=0;j<len;j++){
               if(c==pch[j])mm2[j]=1;
               else mm2[j]=0;
            }
            //Look back
            for(j=1;j<len;j++){
               if(mm2[j])mm2[j]+=mm1[j-1];
               else if(mm1[j-1]>=szlm[i-1])(*(ctc[i-1]+mm1[j-1]-1))++;
            }
            if(mm2[len-1]>=szlm[i])(*(ctc[i]+mm2[len-1]-1))++;
         }
         else {
            c=test[i];
            for(j=0;j<len;j++){
               if(c==pch[j])mm1[j]=1;
               else mm1[j]=0;
            }
            //Look back
            for(j=1;j<len;j++){
               if(mm1[j])mm1[j]+=mm2[j-1];
               else if(mm2[j-1]>=szlm[i-1])(*(ctc[i-1]+mm2[j-1]-1))++;
            }
            if(mm1[len-1]>=szlm[i])(*(ctc[i]+mm1[len-1]-1))++;
         }
         i++;
      }
      //Last row look back
      i--;
      if(i&1){
         for(j=1;j<len-1;j++){
            if(mm2[j]>=szlm[i])(*(ctc[i]+mm2[j]-1))++;
         }
      }
      else {
         for(j=1;j<len-1;j++){
            if(mm1[j]>=szlm[i])(*(ctc[i]+mm1[j]-1))++;
         }
      }
   }
}

void Island::Iscount(long beg,Index *pnd,Strset *pSt){
   long i,j,k,len;
   char c,*pch;
   pSt->gopen_map();

   for(k=beg;k<pnd->ix;k++){
      pch=pSt->show_str(pnd->idx[k]);
      len=strlen(pch);

      //Set first array
      c=test[0];
      for(j=0;j<len;j++){
         if(c==pch[j])mm1[j]=1;
         else mm1[j]=0;
      }

      //Loop
      i=1;
      while(i<slen){
         if(i&1){
            c=test[i];
            for(j=0;j<len;j++){
               if(c==pch[j])mm2[j]=1;
               else mm2[j]=0;
            }
            //Look back
            for(j=1;j<len;j++){
               if(mm2[j])mm2[j]+=mm1[j-1];
               else if(mm1[j-1]>=szlm[i-1])(*(ctc[i-1]+mm1[j-1]-1))++;
            }
            if(mm2[len-1]>=szlm[i])(*(ctc[i]+mm2[len-1]-1))++;
         }
         else {
            c=test[i];
            for(j=0;j<len;j++){
               if(c==pch[j])mm1[j]=1;
               else mm1[j]=0;
            }
            //Look back
            for(j=1;j<len;j++){
               if(mm1[j])mm1[j]+=mm2[j-1];
               else if(mm2[j-1]>=szlm[i-1])(*(ctc[i-1]+mm2[j-1]-1))++;
            }
            if(mm1[len-1]>=szlm[i])(*(ctc[i]+mm1[len-1]-1))++;
         }
         i++;
      }
      //Last row look back
      i--;
      if(i&1){
         for(j=1;j<len-1;j++){
            if(mm2[j]>=szlm[i])(*(ctc[i]+mm2[j]-1))++;
         }
      }
      else {
         for(j=1;j<len-1;j++){
            if(mm1[j]>=szlm[i])(*(ctc[i]+mm1[j]-1))++;
         }
      }
   }
}

void Island::Incl_Add(Count *pCt){
   long i,j,k,*ppt;
   Node *np;
   char *ctst;
   ctst=new char[slen+1];
   strcpy(ctst,test);

   for(i=0;i<slen;i++){
      for(j=0;j<i+1;j++){
         if(k=*(ctc[i]+j)){
            ctst[i+1]='\0';
            if(pCt->search(ctst+i-j)==0){
               ppt = new long;
               (*ppt) =k+1;
               np=new Node(ctst+i-j,(void*)ppt);
               pCt->insert(np);
               pCt->cnt_key++;
               pCt->total+=k+1;
            }
            ctst[i+1]=test[i+1];
         }
      }
   }
}
       
void Island::Excl_Add(Count *pCt){
   long i,j,k,*ppt;
   Node *np;
   char *ctst;
   ctst=new char[slen+1];
   strcpy(ctst,test);

   for(i=0;i<slen;i++){
      for(j=0;j<i+1;j++){
         if(k=*(ctc[i]+j)){
            ctst[i+1]='\0';
            if(pCt->search(ctst+i-j)==0){
               ppt = new long;
               (*ppt) =k;
               np=new Node(ctst+i-j,(void*)ppt);
               pCt->insert(np);
               pCt->cnt_key++;
               pCt->total+=k;
            }
            ctst[i+1]=test[i+1];
         }
      }
   }
}

void Island::Add(List *pLt){
   long i,j,k,*ppt;
   char *ctst;
   ctst=new char[slen+1];
   strcpy(ctst,test);

   for(i=0;i<slen;i++){
      for(j=0;j<i+1;j++){
         if(k=*(ctc[i]+j)){
            ctst[i+1]='\0';
            pLt->add_key_count(ctst+i-j);
            ctst[i+1]=test[i+1];
         }
      }
   }
}

}
