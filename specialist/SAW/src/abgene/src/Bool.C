#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "DataObj.h"
#include "Bool.h"

using namespace std;
namespace iret {

Pnxde::Pnxde(void){
   nam=NULL;
   lft=NULL;
   rht=NULL;
}

Pnxde::Pnxde(const char *str){
   long len=strlen(str);
   nam=new char[len+1];
   strcpy(nam,str);

   lft=NULL;
   rht=NULL;
}

Pnxde::~Pnxde(void){
   if(nam!=NULL)delete [] nam;
}

void Pnxde::set_name(const char *str){
   long len=strlen(str);
   if(nam!=NULL)delete [] nam;
   nam=new char[len+1];
   strcpy(nam,str);
} 

Bool::Bool(void) {
   long i;
   pRgg=NULL;
   pSaa=NULL;
   for(i=0;i<128;i++)zlt[i]=i;
   for(i=65;i<91;i++)zlt[i]=i+32;
   for(i=0;i<128;i++)elt[i]=0;
   elt[40]=1;
   elt[34]=1;
   elt[32]=1;
   err_flag=0;
   str_lim=max_str;
}

Bool::Bool(long lim) {
   long i;
   pRgg=NULL;
   pSaa=NULL;
   for(i=0;i<128;i++)zlt[i]=i;
   for(i=65;i<91;i++)zlt[i]=i+32;
   for(i=0;i<128;i++)elt[i]=0;
   elt[40]=1;
   elt[34]=1;
   elt[32]=1;
   err_flag=0;
   str_lim=lim;
}

Bool::~Bool(){
}

void Bool::gopen_Bool(Regist *pReg,Slice_Accomp *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

   pRgg=pReg;
   pSaa=pSac;

   pSaa->inv_load();
   pSaa->post_access();
   pSaa->gopen_idstring(pSaa->slice_name);
  
   if(pflag)cout << "Opening document files" << endl;
   pRgg->set_class();
   pRgg->gopen_read(READ_U|READ_D);
}

void Bool::gopen_Bool_map(Regist *pReg,Slice_Accomp *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

   pRgg=pReg;
   pSaa=pSac;

   pSaa->inv_map();
   pSaa->post_access_map();
   pSaa->gopen_idstring(pSaa->slice_name);

   if(pflag)cout << "Opening document files" << endl;
   pRgg->set_class();
   pRgg->gopen_map(READ_U|READ_D);
}

void Bool::to_Pmid(Index *ind,long &pm,pLong &pmd){
   long i;
   pm=ind->ix;
   pmd=new long[pm];
   for(i=0;i<pm;i++){
      *(pmd+i)=pRgg->pmid(*(ind->idx+i));
   }
}

Index *Bool::from_Pmid(long pm,long *pmd){
   long i,j,k,*pdx;
   Index *ind;

   pdx=new long[pm];
   j=0;
   for(i=0;i<pm;i++){
      k=pRgg->index(pmd[i]);
      if(k>0){
         pdx[j]=k-1;
         j++;
      }
   }
   if(j){
      ind=new Index();
      ind->ix=j;
      ind->idx=pdx;
      ind->sSort();
      ind->unique();
   }
   else ind=NULL;
   return ind;
}   

Index *Bool::from_String(const char *str){
   long i,k,ix;
   char c,*ptr;
   Index *ind;
   long len=strlen(str);
   err_flag=0;

   if(len>str_lim){
      err_flag=1;
      return(NULL);
   }

   ix=pSaa->disk_snfind(str);
   if(ix==0){
      err_flag=2;
      return(NULL);
   }
   k=*(pSaa->freq+(--ix));
   ind=new Index(k);
   unsigned char *lct=new unsigned char[k];
   pSaa->ix_post(ix,ind->idx,lct);
   delete [] lct;
   return(ind);
}

Index *Bool::from_String(const char *str,long dp,long br){
   long i,k,ix;
   char c,*ptr;
   Index *ind;
   Pnxde *pnn;
   if(!br)pnn=pxn[dp];
   else {
      pnn=new Pnxde;
      pxn[dp]=pnn;
      if(br==1)pxn[dp-1]->lft=pnn;
      else pxn[dp-1]->rht=pnn;
   }
   pnn->lft=NULL;
   pnn->rht=NULL;

cout << str << endl;

   ix=pSaa->disk_snfind(str);
   if(ix==0){
      ptr=new char[strlen(str)+10];
      strcpy(ptr,str);
      strcat(ptr," [absent]");  
      pnn->set_name(ptr);
      err_flag=2;
      return(NULL);
   }
   pnn->set_name(str);
   k=*(pSaa->freq+(--ix));
   ind=new Index(k);
   unsigned char *lct=new unsigned char[k];
   pSaa->ix_post(ix,ind->idx,lct);
   delete [] lct;
   return(ind);
}

Index *Bool::from_Date_Range(long date1,long date2){
   long i,j,k,*daa,n,m,nd;
   Index *ind;
   Docum *pDoc;

   k=0;
   for(i=0;i<pRgg->ndst;i++){
      pDoc=pRgg->pDrg[i];
      n=pDoc->ndoc;
      daa=pDoc->date;
      for(j=0;j<n;j++){
         if((date1<=(nd=*(daa++)))&&(nd<date2))k++;
      }
   }
   if(k==0)return(NULL);
   else {
      ind=new Index(k);
      k=0;
      m=0;
      for(i=0;i<pRgg->ndst;i++){
         pDoc=pRgg->pDrg[i];
         n=pDoc->ndoc;
         daa=pDoc->date;
         for(j=0;j<n;j++){
           if((date1<=(nd=*(daa++)))&&(nd<date2))*(ind->idx+(k++))=j+m;
         }
         m+=n;
      }
   }
   return(ind);
}

Index *Bool::from_Date_Above(long date){
   long i,j,k,*daa,n,m;
   Index *ind;
   Docum *pDoc;

   k=0;
   for(i=0;i<pRgg->ndst;i++){
      pDoc=pRgg->pDrg[i];
      n=pDoc->ndoc;
      daa=pDoc->date;
      for(j=0;j<n;j++){
         if(date<=*(daa++))k++;
      }
   }
   if(k==0)return(NULL);
   else {
      ind=new Index(k);
      k=0;
      m=0;
      for(i=0;i<pRgg->ndst;i++){
         pDoc=pRgg->pDrg[i];
         n=pDoc->ndoc;
         daa=pDoc->date;
         for(j=0;j<n;j++){
           if(date<=*(daa++))*(ind->idx+(k++))=j+m;
         }
         m+=n;
      }
   }
   return(ind);
}

Index *Bool::bool_And(Index *ind,Index *jnd){
   long i,j,k,su,w,p,m,bu;
   long *pdx,*bdx,*sdx,bx,sx;

   if((ind==NULL)||(jnd==NULL)){
      if(ind!=NULL)delete ind;
      if(jnd!=NULL)delete jnd;
      return(NULL);
   }
   if(ind->ix>jnd->ix){
      bdx=ind->idx;
      bx=ind->ix;
      sdx=jnd->idx;
      sx=jnd->ix;
   }
   else {
      bdx=jnd->idx;
      bx=jnd->ix;
      sdx=ind->idx;
      sx=ind->ix;
   }
   pdx=new long[sx];
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+k)=bu;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+k)=su;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+k)=su;
                     k++;
                  }
                  i++;j++;
               }        
            }
         }
      }
   }
   if(k==0){
      delete [] pdx;
      delete ind;
      delete jnd;
      return(NULL);
   }
   else {
      Index *pnd=new Index(k);
      for(i=0;i<k;i++)*(pnd->idx+i)=*(pdx+i);
      delete [] pdx;
      delete ind;
      delete jnd;
      return(pnd);
   }
}
   
Index *Bool::cbool_And(Index *ind,Index *jnd){
   long i,j,k,su,w,p,m,bu;
   long *pdx,*bdx,*sdx,bx,sx;

   if((ind==NULL)||(jnd==NULL)){
      return(NULL);
   }
   if(ind->ix>jnd->ix){
      bdx=ind->idx;
      bx=ind->ix;
      sdx=jnd->idx;
      sx=jnd->ix;
   }
   else {
      bdx=jnd->idx;
      bx=jnd->ix;
      sdx=ind->idx;
      sx=ind->ix;
   }
   pdx=new long[sx];
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+k)=bu;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+k)=su;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+k)=su;
                     k++;
                  }
                  i++;j++;
               }        
            }
         }
      }
   }
   if(k==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(k);
      for(i=0;i<k;i++)*(pnd->idx+i)=*(pdx+i);
      delete [] pdx;
      return(pnd);
   }
}
   
Index *Bool::bool_Or(Index *ind,Index *jnd){
   long i,j,k,*pdx,bx,ii,jj;
   long *iix,*jjx,iu,ju;

   if(ind==NULL)return(jnd);
   else if(jnd==NULL)return(ind);

   ii=ind->ix;
   iix=ind->idx;
   jj=jnd->ix;
   jjx=jnd->idx;
   bx=ii+jj;
   pdx=new long[bx];
   i=j=k=0;
   while((i<ii)&&(j<jj)){
      ju=*(jjx+j);
      while((i<ii)&&(*(iix+i)<ju)){
         *(pdx+k)=*(iix+i);
         k++;i++;
      }
      if(i<ii){
         iu=*(iix+i);
         if(iu==ju){
            *(pdx+k)=iu;
            k++;i++;j++;
         }
         else {
            while((j<jj)&&(iu>*(jjx+j))){
               *(pdx+k)=*(jjx+j);
               k++;j++;
            }
         }
      }
      if(j<jj){
         if(iu==*(jjx+j)){
            *(pdx+k)=iu;
            k++;i++;j++;
         }
      }
   }
   while(i<ii){
      *(pdx+k)=*(iix+i);
      k++;i++;
   }
   while(j<jj){
      *(pdx+k)=*(jjx+j);
      k++;j++;
   }

   if(k==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(k);
      for(i=0;i<k;i++)*(pnd->idx+i)=*(pdx+i);
      delete [] pdx;
      delete ind;
      delete jnd;
      return(pnd);
   }
}

Index *Bool::cbool_Or(Index *ind,Index *jnd){
   long i,j,k,*pdx,bx,ii,jj;
   long *iix,*jjx,iu,ju;

   if(ind==NULL){
      if(jnd==NULL)return(NULL);
      else {
         Index *pnd=new Index(jnd->ix,jnd->idx,0);
         return(pnd);
      }
   }
   else if(jnd==NULL){
      if(ind==NULL)return(NULL);
      else {
         Index *pnd=new Index(ind->ix,ind->idx,0);
         return(pnd);
      }
   }

   ii=ind->ix;
   iix=ind->idx;
   jj=jnd->ix;
   jjx=jnd->idx;
   bx=ii+jj;
   pdx=new long[bx];
   i=j=k=0;
   while((i<ii)&&(j<jj)){
      ju=*(jjx+j);
      while((i<ii)&&(*(iix+i)<ju)){
         *(pdx+k)=*(iix+i);
         k++;i++;
      }
      if(i<ii){
         iu=*(iix+i);
         if(iu==ju){
            *(pdx+k)=iu;
            k++;i++;j++;
         }
         else {
            while((j<jj)&&(iu>*(jjx+j))){
               *(pdx+k)=*(jjx+j);
               k++;j++;
            }
         }
      }
      if(j<jj){
         if(iu==*(jjx+j)){
            *(pdx+k)=iu;
            k++;i++;j++;
         }
      }
   }
   while(i<ii){
      *(pdx+k)=*(iix+i);
      k++;i++;
   }
   while(j<jj){
      *(pdx+k)=*(jjx+j);
      k++;j++;
   }

   if(k==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(k);
      for(i=0;i<k;i++)*(pnd->idx+i)=*(pdx+i);
      delete [] pdx;
      return(pnd);
   }
}

Index *Bool::bool_Butnot(Index *ind,Index *jnd){
   long i,j,k,su,w,p,m,bu,flab;
   long *pdx,*bdx,*sdx,bx,sx;

   if(ind==NULL){
      if(jnd!=NULL)delete jnd;
      return(NULL);
   }
   if(jnd==NULL)return(ind);
   if(ind->ix>jnd->ix){
      bdx=ind->idx;
      bx=ind->ix;
      sdx=jnd->idx;
      sx=jnd->ix;
      flab=1;
   }
   else {
      bdx=jnd->idx;
      bx=jnd->ix;
      sdx=ind->idx;
      sx=ind->ix;
      flab=0;
   }
   pdx=new long[ind->ix];
   for(i=0;i<ind->ix;i++)*(pdx+i)=1; //Initialize as marker.
   if(flab){ //Case ind is big.
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+j)=0;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+j+w)=0;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+j)=0;
                     k++;
                  }
                  i++;j++;
               }        
            }
         }
      }
   }
   } //End of case ind is big.
   else { //Case ind is small.
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+i)=0;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+i)=0;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+i)=0;
                     k++;
                  }
                  i++;j++;
               }
            }
         }
      }
   }
   } //End of case ind is small.

   j=ind->ix-k;
   if(k==0){
      delete [] pdx;
      delete jnd;
      return(ind);
   }
   else if(j==0){
      delete [] pdx;
      delete ind;
      delete jnd;
      return(NULL);
   }
   else {
      Index *pnd=new Index(j);
      j=0;
      for(i=0;i<ind->ix;i++){
         if(*(pdx+i)){
            *(pnd->idx+j)=*(ind->idx+i);
            j++;
         }
      }
      delete [] pdx;
      delete ind;
      delete jnd;
      return(pnd);
   }
}

Index *Bool::cbool_Butnot(Index *ind,Index *jnd){
   long i,j,k,su,w,p,m,bu,flab;
   long *pdx,*bdx,*sdx,bx,sx;

   if(ind==NULL){
      return(NULL);
   }
   if(jnd==NULL){
      Index *pnd=new Index(ind->ix,ind->idx,0);
      return(pnd);
   }

   if(ind->ix>jnd->ix){
      bdx=ind->idx;
      bx=ind->ix;
      sdx=jnd->idx;
      sx=jnd->ix;
      flab=1;
   }
   else {
      bdx=jnd->idx;
      bx=jnd->ix;
      sdx=ind->idx;
      sx=ind->ix;
      flab=0;
   }
   pdx=new long[ind->ix];
   for(i=0;i<ind->ix;i++)*(pdx+i)=1; //Initialize as marker.
   if(flab){ //Case ind is big.
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+j)=0;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+j+w)=0;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+j)=0;
                     k++;
                  }
                  i++;j++;
               }        
            }
         }
      }
   }
   } //End of case ind is big.
   else { //Case ind is small.
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+i)=0;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+i)=0;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+i)=0;
                     k++;
                  }
                  i++;j++;
               }
            }
         }
      }
   }
   } //End of case ind is small.

   j=ind->ix-k;
   if(k==0){
      delete [] pdx;
      Index *pnd=new Index(ind->ix,ind->idx,0);
      return(pnd);
   }
   else if(j==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(j);
      j=0;
      for(i=0;i<ind->ix;i++){
         if(*(pdx+i)){
            *(pnd->idx+j)=*(ind->idx+i);
            j++;
         }
      }
      delete [] pdx;
      return(pnd);
   }
}

Index *Bool::bool_Recurs(char *str,long dp,long br){
   char *ptr,*qtr;
   long i,j,k,len,u;
   long pa,po,pb;
   Index *ind;
   Pnxde *pnn;
   
   if(br){
      pnn=new Pnxde;
      pxn[dp]=pnn;
   }
   else pnn=pxn[dp];

   if(br==1)pxn[dp-1]->lft=pnn;
   else if(br==2)pxn[dp-1]->rht=pnn;
   pnn->lft=NULL;
   pnn->rht=NULL;
   
   len=strlen(str);
   if(len>str_lim){
      err_flag=1;
      return(NULL);
   }
   k=0;
   for(i=0;i<len;i++){
      j=(int)*(str+i);
      switch(j){
         case 33: k++;
                  break;
         default: if(k==2)k=3;
                  else k=0;
      }
      if(j<0)*(str+i)=' ';
      else {
         if(k<2)*(str+i)=zlt[j];
      }
   }
   i=0;
   while((i<len)&&(*(str+i)==' '))i++;
   j=len-1;
   while((j>=i)&&(*(str+j)==' '))j--;
   *(str+j+1)='\0';
   len=j+1;

   switch(*(str+i)){
      case '(':
         k=1;
         j=i+1;
         while(k&&(j<len)){
            switch(*(str+j)){
               case '(': k++;
                         break;
               case ')': k--;
                         break;
            }
            if(k)j++;
         }
         if(!k){
            *(str+j)='\0';
            u=j+1;
            while((u<len)&&(*(str+u)==' '))u++;
            if(u==len)return(this->bool_Recurs(str+i+1,dp,0));
            if((!strncmp("and",str+u,3))&&((u+4<len)&&(elt[(int)*(str+u+3)]))){
               pnn->set_name(" AND ");
               return(this->bool_And(this->bool_Recurs(str+i+1,dp+1,1),\
                      this->bool_Recurs(str+u+4,dp+1,2)));
            }
            else if((!strncmp("or",str+u,2))&&((u+3<len)&&(elt[(int)*(str+u+2)]))){
               pnn->set_name(" OR ");
               return(this->bool_Or(this->bool_Recurs(str+i+1,dp+1,1),\
                      this->bool_Recurs(str+u+3,dp+1,2)));
            }
            else if((!strncmp("butnot",str+u,6))&&((u+7<len)&&(elt[(int)*(str+u+6)]))){
               pnn->set_name(" BUTNOT ");
               return(this->bool_Butnot(this->bool_Recurs(str+i+1,dp+1,1),\
                      this->bool_Recurs(str+u+7,dp+1,2)));
            }
            else { 
               ptr=new char[len+10];
               strcpy(ptr,str);
               strcat(ptr," [ungram]");
               pnn->set_name(ptr);
               delete [] ptr;
               return(NULL);
            }
         }
         else { 
            ptr=new char[len+10];
            strcpy(ptr,str);
            strcat(ptr," [ungram]");
            pnn->set_name(ptr);
            delete [] ptr;
            return(NULL);
         }
         break;
      case '"':
         k=1;
         j=i+1;
         while(k&&(j<len)){
            if(*(str+j)=='"')k=0;
            if(k)j++;
         }
         if((!k)&&(j<len-1)){
            *(str+j+1)='\0';
            u=j+2;
            while((u<len)&&(*(str+u)==' '))u++;
            if(u==len)return(this->bool_Recurs(str+i+1,dp,0));
            if((!strncmp("and",str+u,3))&&((u+4<len)&&(elt[(int)*(str+u+3)]))){
               pnn->set_name(" AND ");
               return(this->bool_And(this->bool_Recurs(str+i,dp+1,1),\
                      this->bool_Recurs(str+u+4,dp+1,2)));
            }
            else if((!strncmp("or",str+u,2))&&((u+3<len)&&(elt[(int)*(str+u+2)]))){
               pnn->set_name(" OR ");
               return(this->bool_Or(this->bool_Recurs(str+i,dp+1,1),\
                      this->bool_Recurs(str+u+3,dp+1,2)));
            }
            else if((!strncmp("butnot",str+u,6))&&((u+7<len)&&(elt[(int)*(str+u+6)]))){
               pnn->set_name(" BUTNOT ");
               return(this->bool_Butnot(this->bool_Recurs(str+i,dp+1,1),\
                      this->bool_Recurs(str+u+7,dp+1,2)));
            }
            else if((u<len)&&(elt[(int)*(str+u)])){
               pnn->set_name(" AND ");
               return(this->bool_And(this->bool_Recurs(str+i,dp+1,1),\
                      this->bool_Recurs(str+u,dp+1,2)));
            }
            else { 
               ptr=new char[len+10];
               strcpy(ptr,str);
               strcat(ptr," [ungram]");
               pnn->set_name(ptr);
               delete [] ptr;
               return(NULL);
            }
         }
         else if((!k)&&(j==len-1)){
            *(str+j)='\0';
            ptr=new char[len+3];
            this->interpret(str+i+1,ptr);
            ind=this->from_String(ptr,dp,0);
            delete [] ptr;
            return(ind);
         }
         else {
            ptr=new char[len+10];
            strcpy(ptr,str);
            strcat(ptr," [ungram]");
            pnn->set_name(ptr);
            delete [] ptr;
            return(NULL);
         }
         break;
      default:
         pa=findw(str+i,"and");
         if(pa==1){
            ptr=new char[len+10];
            strcpy(ptr,str);
            strcat(ptr," [ungram]");
            pnn->set_name(ptr);
            delete [] ptr;
            return(NULL);
         }
         po=findw(str+i,"or");
         if(po==1){
            ptr=new char[len+10];
            strcpy(ptr,str);
            strcat(ptr," [ungram]");
            pnn->set_name(ptr);
            delete [] ptr;
            return(NULL);
         }
         pb=findw(str+i,"butnot");
         if(pb==1){
            ptr=new char[len+10];
            strcpy(ptr,str);
            strcat(ptr," [ungram]");
            pnn->set_name(ptr);
            delete [] ptr;
            return(NULL);
         }
         if(pa&&(((!po)||(po>pa))&&((!pb)||(pb>pa)))){
            *(str+i+pa-2)='\0';
            pnn->set_name(" AND ");
            return(this->bool_And(this->bool_Recurs(str+i,dp+1,1),\
                   this->bool_Recurs(str+i+pa+3,dp+1,2)));
         }
         if(po&&(((!pa)||(pa>po))&&((!pb)||(pb>po)))){
            *(str+i+po-2)='\0';
            pnn->set_name(" OR ");
            return(this->bool_Or(this->bool_Recurs(str+i,dp+1,1),\
                   this->bool_Recurs(str+i+po+2,dp+1,2)));
         }
         if(pb&&(((!pa)||(pa>pb))&&((!po)||(po>pb)))){
            *(str+i+pb-2)='\0';
            pnn->set_name(" BUTNOT ");
            return(this->bool_Butnot(this->bool_Recurs(str+i,dp+1,1),\
                   this->bool_Recurs(str+i+pb+6,dp+1,2)));
         }
         
         ptr=new char[len+4];
         qtr=new char[len+4];
         this->extract(str+i,ptr,j);
         this->interpret(ptr,qtr);
         if(j<len){
            pnn->set_name(" AND ");
            ind=this->from_String(qtr,dp+1,1);
            delete [] ptr;
            delete [] qtr;
            return(this->bool_And(ind,this->bool_Recurs(str+i+j,dp+1,2)));
         }
         else {
            ind=this->from_String(qtr,dp,0);
            delete [] ptr;
            delete [] qtr;
            return(ind);
         }
         break;
   }
}

Index *Bool::bool_Parse(char *str){
   err_flag=0;
   pxn[0]=new Pnxde;
   return(this->bool_Recurs(str,0,0));
}

void Bool::interpret(char *str,char *ptr){
   long i,j,k,len;
   long u,v,pa,po,pb;
   char *ktr;

   len=strlen(str);
   i=0;
   while((i<len)&&(*(str+i)==' '))i++;
   j=len-1;
   while((j>=i)&&(*(str+j)==' '))j--;
   *(str+j+1)='\0';
   len=j+1;

   if(strchr(str+i,'!')!=NULL){
      strcpy(ptr,str+i);
      return;
   }
   else if(strchr(str+i,'*')!=NULL){
      strcpy(ptr,str+i);
      return;
   }
   else {
      v=0;
      while((v<len)&&(*(str+i+v)!=' '))v++;
      if(k=findw(str+i,"[text]")){
         j=i+k-2;
         while((j>=i)&&(*(str+j)==' '))j--;
         *(str+j+1)='\0';
         strcpy(ptr,str+i);
         u=j+1-i;
         *(ptr+u)='!';
         *(ptr+u+1)='!';
         if(v<k-2)*(ptr+u+2)='p';
         else *(ptr+u+2)='t';
         *(ptr+u+3)='\0';
         return;
      }
      if(k=findw(str+i,"[title]")){
         j=i+k-2;
         while((j>=i)&&(*(str+j)==' '))j--;
         *(str+j+1)='\0';
         strcpy(ptr,str+i);
         u=j+1-i;
         *(ptr+u)='!';
         *(ptr+u+1)='!';
         if(v<k-2)*(ptr+u+2)='P';
         else *(ptr+u+2)='T';
         *(ptr+u+3)='\0';
         return;
      }
      if(k=findw(str+i,"[mesh]")){
         j=i+k-2;
         while((j>=i)&&(*(str+j)==' '))j--;
         *(str+j+1)='\0';
         if(ktr=strchr(str+i,'/'))*ktr='!';
         else {*(str+j+1)='!';*(str+j+2)='\0';}
         strcpy(ptr,str+i);
         return;
      }
      strcpy(ptr,str+i);
      u=len;
      *(ptr+u)='!';
      *(ptr+u+1)='!';
      if(v<len)*(ptr+u+2)='p';
      else *(ptr+u+2)='t';
      *(ptr+u+3)='\0';
      return;
   }
}

long Bool::findw(char *str,const char *w){
   long i,j,k,len,wl;
   char *ptr;

   len=strlen(str);
   wl=strlen(w);
   k=0;
   i=0;
   while(i<len){
      if((ptr=strstr(str+i,w))!=NULL){
         j=ptr-str;
         if(((!j)||(*(str+j-1)==' '))&&((j+wl==len)||(*(str+j+wl)==' '))){
            k=j+1;
            i=len;
         }
         else i=j+wl;
      }
      else i=len;
   }
   return(k);
}

void Bool::extract(char *str,char *ptr,long &n){
   long i,j,k,len,u,v;
   char *utr,c;

   utr=strchr(str,' ');
   if(utr==NULL){
      strcpy(ptr,str);
      n=strlen(str);
      return;
   }
   if(k=this->findw(str,"[text]")){
      c=*(str+k+5);
      *(str+k+5)='\0';
      strcpy(ptr,str);
      *(str+k+5)=c;
      n=k+5;
      return;
   }
   if(k=this->findw(str,"[title]")){
      c=*(str+k+6);
      *(str+k+6)='\0';
      strcpy(ptr,str);
      *(str+k+6)=c;
      n=k+6;
      return;
   }
   if(k=this->findw(str,"[mesh]")){
      c=*(str+k+5);
      *(str+k+5)='\0';
      strcpy(ptr,str);
      *(str+k+5)=c;
      n=k+5;
      return;
   }
   u=utr-str;
   utr=strchr(str,'!');
   if(utr!=NULL){
      v=utr-str;
      k=v+1;
      len=strlen(str);
      while((k<len)&&(*(str+k)!=' '))k++;
      *(str+k)='\0';
      strcpy(ptr,str);
      n=k+1;
      return;
   }
   else {
      *(str+u)='\0';
      strcpy(ptr,str);
      n=u+1;
      return;
   }
}

void Bool::standard(char *str,char *ptr){
   long i,len,uflag;
   char *ktr;

   uflag=0;
   if(strstr(str,"[absent]"))uflag=1;
   else if(strstr(str,"[ungram]"))uflag=2;

   if(ktr=strstr(str,"!!t")){
      *ktr='\0';
      strcpy(ptr,str);
      strcat(ptr," [TEXT]");
   }
   else if(ktr=strstr(str,"!!T")){
      *ktr='\0';
      strcpy(ptr,str);
      strcat(ptr," [TITLE]");
   }
   else if(ktr=strstr(str,"!!p")){
      *ktr='\0';
      strcpy(ptr,str);
      strcat(ptr," [TEXT]");
   }
   else if(ktr=strstr(str,"!!P")){
      *ktr='\0';
      strcpy(ptr,str);
      strcat(ptr," [TITLE]");
   }
   else if(ktr=strchr(str,'!')){
      len=strlen(str);
      i=ktr-str;
      if((i<len-1)&&(*(ktr+2)!='[')){
         *(ktr)='/';
         if(ktr=strchr(str,'['))*(ktr-1)='\0';
      }
      else *ktr='\0';
      strcpy(ptr,str);
      strcat(ptr," [MESH]");
   }
   else if(ktr=strchr(str,'*')){
      if(ktr=strchr(str,'['))*(ktr-1)='\0';
      strcpy(ptr,str);
      strcat(ptr," [MESH]");
   }
   else {
      ktr=strchr(str,'[');
      *(ktr-1)='\0';
      strcpy(ptr,str);
   }
   if(uflag==1)strcat(ptr," [absent]");
   else if(uflag==2)strcat(ptr," [ungram]");
}

void Bool::standard_String(char *str){
   long i,dep;
   char cnam[max_str];

   for(i=0;i<100;i++)bra[i]=0;
   dep=0;
   *str='\0';

   while((dep>0)||(bra[dep]<2)){
      if((pxn[dep]->lft==NULL)||(pxn[dep]->rht==NULL)){
         this->standard(pxn[dep]->nam,cnam);
         strcat(str,cnam);
         delete pxn[dep];
         dep--;
         if(dep==-1){
            dep=0;
            bra[0]=2;
         }
      }
      else {
         if(bra[dep]==0){
            if(dep)strcat(str,"(");
            bra[dep]=1;
            pxn[dep+1]=pxn[dep]->lft;
            dep++;
         }
         else if(bra[dep]==1){
            strcat(str,pxn[dep]->nam);
            bra[dep]=2;
            pxn[dep+1]=pxn[dep]->rht;
            dep++;
         }
         else if(bra[dep]==2){
            if(dep)strcat(str,")");
            bra[dep]=0;
            delete pxn[dep];
            dep--;
         }
      }
   }
}

//Multithreaded version

Bool_pth::Bool_pth(void) {
   long i;
   pRgg=NULL;
   pSaa=NULL;
   for(i=0;i<128;i++)zlt[i]=i;
   for(i=65;i<91;i++)zlt[i]=i+32;
   for(i=0;i<128;i++)elt[i]=0;
   elt[40]=1;
   elt[34]=1;
   elt[32]=1;
   err_flag=0;
   str_lim=max_str;
}

Bool_pth::Bool_pth(long lim) {
   long i;
   pRgg=NULL;
   pSaa=NULL;
   for(i=0;i<128;i++)zlt[i]=i;
   for(i=65;i<91;i++)zlt[i]=i+32;
   for(i=0;i<128;i++)elt[i]=0;
   elt[40]=1;
   elt[34]=1;
   elt[32]=1;
   err_flag=0;
   str_lim=lim;
}

Bool_pth::~Bool_pth(){
}

void Bool_pth::gopen_Bool_pth(Regist_pth *pReg,Slice_Accpth *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

   pRgg=pReg;
   pSaa=pSac;

   pSaa->inv_load();
   pSaa->post_access();
   pSaa->gopen_idstring(pSaa->slice_name);
  
   if(pflag)cout << "Opening document files" << endl;
   pRgg->set_class();
   pRgg->gopen_read(READ_U|READ_D);
}

void Bool_pth::gopen_Bool_map(Regist_pth *pReg,Slice_Accpth *pSac){
   char cnam[max_str];
   int pflag=get_qflag();
   long i,j;

   pRgg=pReg;
   pSaa=pSac;

   pSaa->inv_map();
   pSaa->post_access_map();
   pSaa->gopen_idstring(pSaa->slice_name);

   if(pflag)cout << "Opening document files" << endl;
   pRgg->set_class();
   pRgg->gopen_map(READ_U|READ_D);
}

void Bool_pth::to_Pmid(Index *ind,long &pm,pLong &pmd){
   long i;
   pm=ind->ix;
   pmd=new long[pm];
   for(i=0;i<pm;i++){
      *(pmd+i)=pRgg->pmid(*(ind->idx+i));
   }
}

Index *Bool_pth::from_Pmid(long pm,long *pmd){
   long i,j,k,*pdx;
   Index *ind;

   pdx=new long[pm];
   j=0;
   for(i=0;i<pm;i++){
      k=pRgg->index(pmd[i]);
      if(k>0){
         pdx[j]=k-1;
         j++;
      }
   }
   if(j){
      ind=new Index();
      ind->ix=j;
      ind->idx=pdx;
      ind->sSort();
      ind->unique();
   }
   else ind=NULL;
   return ind;
}

Index *Bool_pth::from_String(const char *str){
   long i,k,ix;
   char c,*ptr;
   Index *ind;
   long len=strlen(str);
   err_flag=0;

   if(len>str_lim){
      err_flag=1;
      return(NULL);
   }

   ix=pSaa->disk_snfind(str);
   if(ix==0){
      err_flag=2;
      return(NULL);
   }
   k=*(pSaa->freq+(--ix));
   ind=new Index(k);
   unsigned char *lct=new unsigned char[k];
   pSaa->ix_post(ix,ind->idx,lct);
   delete [] lct;
   return(ind);
}

Index *Bool_pth::from_String(const char *str,long dp,long br){
   long i,k,ix;
   char c,*ptr;
   Index *ind;
   Pnxde *pnn;
   if(!br)pnn=pxn[dp];
   else {
      pnn=new Pnxde;
      pxn[dp]=pnn;
      if(br==1)pxn[dp-1]->lft=pnn;
      else pxn[dp-1]->rht=pnn;
   }
   pnn->lft=NULL;
   pnn->rht=NULL;

cout << str << endl;

   ix=pSaa->disk_snfind(str);
   if(ix==0){
      ptr=new char[strlen(str)+10];
      strcpy(ptr,str);
      strcat(ptr," [absent]");  
      pnn->set_name(ptr);
      err_flag=2;
      return(NULL);
   }
   pnn->set_name(str);
   k=*(pSaa->freq+(--ix));
   ind=new Index(k);
   unsigned char *lct=new unsigned char[k];
   pSaa->ix_post(ix,ind->idx,lct);
   delete [] lct;
   return(ind);
}

Index *Bool_pth::from_Date_Range(long date1,long date2){
   long i,j,k,*daa,n,m,nd;
   Index *ind;
   Docum *pDoc;

   k=0;
   for(i=0;i<pRgg->ndst;i++){
      pDoc=pRgg->pDrg[i];
      n=pDoc->ndoc;
      daa=pDoc->date;
      for(j=0;j<n;j++){
         if((date1<=(nd=*(daa++)))&&(nd<date2))k++;
      }
   }
   if(k==0)return(NULL);
   else {
      ind=new Index(k);
      k=0;
      m=0;
      for(i=0;i<pRgg->ndst;i++){
         pDoc=pRgg->pDrg[i];
         n=pDoc->ndoc;
         daa=pDoc->date;
         for(j=0;j<n;j++){
           if((date1<=(nd=*(daa++)))&&(nd<date2))*(ind->idx+(k++))=j+m;
         }
         m+=n;
      }
   }
   return(ind);
}

Index *Bool_pth::from_Date_Above(long date){
   long i,j,k,*daa,n,m;
   Index *ind;
   Docum *pDoc;

   k=0;
   for(i=0;i<pRgg->ndst;i++){
      pDoc=pRgg->pDrg[i];
      n=pDoc->ndoc;
      daa=pDoc->date;
      for(j=0;j<n;j++){
         if(date<=*(daa++))k++;
      }
   }
   if(k==0)return(NULL);
   else {
      ind=new Index(k);
      k=0;
      m=0;
      for(i=0;i<pRgg->ndst;i++){
         pDoc=pRgg->pDrg[i];
         n=pDoc->ndoc;
         daa=pDoc->date;
         for(j=0;j<n;j++){
           if(date<=*(daa++))*(ind->idx+(k++))=j+m;
         }
         m+=n;
      }
   }
   return(ind);
}

Index *Bool_pth::bool_And(Index *ind,Index *jnd){
   long i,j,k,su,w,p,m,bu;
   long *pdx,*bdx,*sdx,bx,sx;

   if((ind==NULL)||(jnd==NULL)){
      if(ind!=NULL)delete ind;
      if(jnd!=NULL)delete jnd;
      return(NULL);
   }
   if(ind->ix>jnd->ix){
      bdx=ind->idx;
      bx=ind->ix;
      sdx=jnd->idx;
      sx=jnd->ix;
   }
   else {
      bdx=jnd->idx;
      bx=jnd->ix;
      sdx=ind->idx;
      sx=ind->ix;
   }
   pdx=new long[sx];
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+k)=bu;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+k)=su;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+k)=su;
                     k++;
                  }
                  i++;j++;
               }        
            }
         }
      }
   }
   if(k==0){
      delete [] pdx;
      delete ind;
      delete jnd;
      return(NULL);
   }
   else {
      Index *pnd=new Index(k);
      for(i=0;i<k;i++)*(pnd->idx+i)=*(pdx+i);
      delete [] pdx;
      delete ind;
      delete jnd;
      return(pnd);
   }
}
   
Index *Bool_pth::cbool_And(Index *ind,Index *jnd){
   long i,j,k,su,w,p,m,bu;
   long *pdx,*bdx,*sdx,bx,sx;

   if((ind==NULL)||(jnd==NULL)){
      return(NULL);
   }
   if(ind->ix>jnd->ix){
      bdx=ind->idx;
      bx=ind->ix;
      sdx=jnd->idx;
      sx=jnd->ix;
   }
   else {
      bdx=jnd->idx;
      bx=jnd->ix;
      sdx=ind->idx;
      sx=ind->ix;
   }
   pdx=new long[sx];
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+k)=bu;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+k)=su;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+k)=su;
                     k++;
                  }
                  i++;j++;
               }        
            }
         }
      }
   }
   if(k==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(k);
      for(i=0;i<k;i++)*(pnd->idx+i)=*(pdx+i);
      delete [] pdx;
      return(pnd);
   }
}
   
Index *Bool_pth::bool_Or(Index *ind,Index *jnd){
   long i,j,k,*pdx,bx,ii,jj;
   long *iix,*jjx,iu,ju;

   if(ind==NULL)return(jnd);
   else if(jnd==NULL)return(ind);

   ii=ind->ix;
   iix=ind->idx;
   jj=jnd->ix;
   jjx=jnd->idx;
   bx=ii+jj;
   pdx=new long[bx];
   i=j=k=0;
   while((i<ii)&&(j<jj)){
      ju=*(jjx+j);
      while((i<ii)&&(*(iix+i)<ju)){
         *(pdx+k)=*(iix+i);
         k++;i++;
      }
      if(i<ii){
         iu=*(iix+i);
         if(iu==ju){
            *(pdx+k)=iu;
            k++;i++;j++;
         }
         else {
            while((j<jj)&&(iu>*(jjx+j))){
               *(pdx+k)=*(jjx+j);
               k++;j++;
            }
         }
      }
      if(j<jj){
         if(iu==*(jjx+j)){
            *(pdx+k)=iu;
            k++;i++;j++;
         }
      }
   }
   while(i<ii){
      *(pdx+k)=*(iix+i);
      k++;i++;
   }
   while(j<jj){
      *(pdx+k)=*(jjx+j);
      k++;j++;
   }

   if(k==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(k);
      for(i=0;i<k;i++)*(pnd->idx+i)=*(pdx+i);
      delete [] pdx;
      delete ind;
      delete jnd;
      return(pnd);
   }
}

Index *Bool_pth::cbool_Or(Index *ind,Index *jnd){
   long i,j,k,*pdx,bx,ii,jj;
   long *iix,*jjx,iu,ju;

   if(ind==NULL){
      if(jnd==NULL)return(NULL);
      else {
         Index *pnd=new Index(jnd->ix,jnd->idx,0);
         return(pnd);
      }
   }
   else if(jnd==NULL){
      if(ind==NULL)return(NULL);
      else {
         Index *pnd=new Index(ind->ix,ind->idx,0);
         return(pnd);
      }
   }

   ii=ind->ix;
   iix=ind->idx;
   jj=jnd->ix;
   jjx=jnd->idx;
   bx=ii+jj;
   pdx=new long[bx];
   i=j=k=0;
   while((i<ii)&&(j<jj)){
      ju=*(jjx+j);
      while((i<ii)&&(*(iix+i)<ju)){
         *(pdx+k)=*(iix+i);
         k++;i++;
      }
      if(i<ii){
         iu=*(iix+i);
         if(iu==ju){
            *(pdx+k)=iu;
            k++;i++;j++;
         }
         else {
            while((j<jj)&&(iu>*(jjx+j))){
               *(pdx+k)=*(jjx+j);
               k++;j++;
            }
         }
      }
      if(j<jj){
         if(iu==*(jjx+j)){
            *(pdx+k)=iu;
            k++;i++;j++;
         }
      }
   }
   while(i<ii){
      *(pdx+k)=*(iix+i);
      k++;i++;
   }
   while(j<jj){
      *(pdx+k)=*(jjx+j);
      k++;j++;
   }

   if(k==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(k);
      for(i=0;i<k;i++)*(pnd->idx+i)=*(pdx+i);
      delete [] pdx;
      return(pnd);
   }
}

Index *Bool_pth::bool_Butnot(Index *ind,Index *jnd){
   long i,j,k,su,w,p,m,bu,flab;
   long *pdx,*bdx,*sdx,bx,sx;

   if(ind==NULL){
      if(jnd!=NULL)delete jnd;
      return(NULL);
   }
   if(jnd==NULL)return(ind);
   if(ind->ix>jnd->ix){
      bdx=ind->idx;
      bx=ind->ix;
      sdx=jnd->idx;
      sx=jnd->ix;
      flab=1;
   }
   else {
      bdx=jnd->idx;
      bx=jnd->ix;
      sdx=ind->idx;
      sx=ind->ix;
      flab=0;
   }
   pdx=new long[ind->ix];
   for(i=0;i<ind->ix;i++)*(pdx+i)=1; //Initialize as marker.
   if(flab){ //Case ind is big.
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+j)=0;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+j+w)=0;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+j)=0;
                     k++;
                  }
                  i++;j++;
               }        
            }
         }
      }
   }
   } //End of case ind is big.
   else { //Case ind is small.
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+i)=0;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+i)=0;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+i)=0;
                     k++;
                  }
                  i++;j++;
               }
            }
         }
      }
   }
   } //End of case ind is small.

   j=ind->ix-k;
   if(k==0){
      delete [] pdx;
      delete jnd;
      return(ind);
   }
   else if(j==0){
      delete [] pdx;
      delete ind;
      delete jnd;
      return(NULL);
   }
   else {
      Index *pnd=new Index(j);
      j=0;
      for(i=0;i<ind->ix;i++){
         if(*(pdx+i)){
            *(pnd->idx+j)=*(ind->idx+i);
            j++;
         }
      }
      delete [] pdx;
      delete ind;
      delete jnd;
      return(pnd);
   }
}

Index *Bool_pth::cbool_Butnot(Index *ind,Index *jnd){
   long i,j,k,su,w,p,m,bu,flab;
   long *pdx,*bdx,*sdx,bx,sx;

   if(ind==NULL){
      return(NULL);
   }
   if(jnd==NULL){
      Index *pnd=new Index(ind->ix,ind->idx,0);
      return(pnd);
   }

   if(ind->ix>jnd->ix){
      bdx=ind->idx;
      bx=ind->ix;
      sdx=jnd->idx;
      sx=jnd->ix;
      flab=1;
   }
   else {
      bdx=jnd->idx;
      bx=jnd->ix;
      sdx=ind->idx;
      sx=ind->ix;
      flab=0;
   }
   pdx=new long[ind->ix];
   for(i=0;i<ind->ix;i++)*(pdx+i)=1; //Initialize as marker.
   if(flab){ //Case ind is big.
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+j)=0;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+j+w)=0;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+j)=0;
                     k++;
                  }
                  i++;j++;
               }        
            }
         }
      }
   }
   } //End of case ind is big.
   else { //Case ind is small.
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+i)=0;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+i)=0;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+i)=0;
                     k++;
                  }
                  i++;j++;
               }
            }
         }
      }
   }
   } //End of case ind is small.

   j=ind->ix-k;
   if(k==0){
      delete [] pdx;
      Index *pnd=new Index(ind->ix,ind->idx,0);
      return(pnd);
   }
   else if(j==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(j);
      j=0;
      for(i=0;i<ind->ix;i++){
         if(*(pdx+i)){
            *(pnd->idx+j)=*(ind->idx+i);
            j++;
         }
      }
      delete [] pdx;
      return(pnd);
   }
}

Index *Bool_pth::bool_Recurs(char *str,long dp,long br){
   char *ptr,*qtr;
   long i,j,k,len,u;
   long pa,po,pb;
   Index *ind;
   Pnxde *pnn;
   
   if(br){
      pnn=new Pnxde;
      pxn[dp]=pnn;
   }
   else pnn=pxn[dp];

   if(br==1)pxn[dp-1]->lft=pnn;
   else if(br==2)pxn[dp-1]->rht=pnn;
   pnn->lft=NULL;
   pnn->rht=NULL;
   
   len=strlen(str);
   if(len>str_lim){
      err_flag=1;
      return(NULL);
   }
   k=0;
   for(i=0;i<len;i++){
      j=(int)*(str+i);
      switch(j){
         case 33: k++;
                  break;
         default: if(k==2)k=3;
                  else k=0;
      }
      if(j<0)*(str+i)=' ';
      else {
         if(k<2)*(str+i)=zlt[j];
      }
   }
   i=0;
   while((i<len)&&(*(str+i)==' '))i++;
   j=len-1;
   while((j>=i)&&(*(str+j)==' '))j--;
   *(str+j+1)='\0';
   len=j+1;

   switch(*(str+i)){
      case '(':
         k=1;
         j=i+1;
         while(k&&(j<len)){
            switch(*(str+j)){
               case '(': k++;
                         break;
               case ')': k--;
                         break;
            }
            if(k)j++;
         }
         if(!k){
            *(str+j)='\0';
            u=j+1;
            while((u<len)&&(*(str+u)==' '))u++;
            if(u==len)return(this->bool_Recurs(str+i+1,dp,0));
            if((!strncmp("and",str+u,3))&&((u+4<len)&&(elt[(int)*(str+u+3)]))){
               pnn->set_name(" AND ");
               return(this->bool_And(this->bool_Recurs(str+i+1,dp+1,1),\
                      this->bool_Recurs(str+u+4,dp+1,2)));
            }
            else if((!strncmp("or",str+u,2))&&((u+3<len)&&(elt[(int)*(str+u+2)]))){
               pnn->set_name(" OR ");
               return(this->bool_Or(this->bool_Recurs(str+i+1,dp+1,1),\
                      this->bool_Recurs(str+u+3,dp+1,2)));
            }
            else if((!strncmp("butnot",str+u,6))&&((u+7<len)&&(elt[(int)*(str+u+6)]))){
               pnn->set_name(" BUTNOT ");
               return(this->bool_Butnot(this->bool_Recurs(str+i+1,dp+1,1),\
                      this->bool_Recurs(str+u+7,dp+1,2)));
            }
            else { 
               ptr=new char[len+10];
               strcpy(ptr,str);
               strcat(ptr," [ungram]");
               pnn->set_name(ptr);
               delete [] ptr;
               return(NULL);
            }
         }
         else { 
            ptr=new char[len+10];
            strcpy(ptr,str);
            strcat(ptr," [ungram]");
            pnn->set_name(ptr);
            delete [] ptr;
            return(NULL);
         }
         break;
      case '"':
         k=1;
         j=i+1;
         while(k&&(j<len)){
            if(*(str+j)=='"')k=0;
            if(k)j++;
         }
         if((!k)&&(j<len-1)){
            *(str+j+1)='\0';
            u=j+2;
            while((u<len)&&(*(str+u)==' '))u++;
            if(u==len)return(this->bool_Recurs(str+i+1,dp,0));
            if((!strncmp("and",str+u,3))&&((u+4<len)&&(elt[(int)*(str+u+3)]))){
               pnn->set_name(" AND ");
               return(this->bool_And(this->bool_Recurs(str+i,dp+1,1),\
                      this->bool_Recurs(str+u+4,dp+1,2)));
            }
            else if((!strncmp("or",str+u,2))&&((u+3<len)&&(elt[(int)*(str+u+2)]))){
               pnn->set_name(" OR ");
               return(this->bool_Or(this->bool_Recurs(str+i,dp+1,1),\
                      this->bool_Recurs(str+u+3,dp+1,2)));
            }
            else if((!strncmp("butnot",str+u,6))&&((u+7<len)&&(elt[(int)*(str+u+6)]))){
               pnn->set_name(" BUTNOT ");
               return(this->bool_Butnot(this->bool_Recurs(str+i,dp+1,1),\
                      this->bool_Recurs(str+u+7,dp+1,2)));
            }
            else if((u<len)&&(elt[(int)*(str+u)])){
               pnn->set_name(" AND ");
               return(this->bool_And(this->bool_Recurs(str+i,dp+1,1),\
                      this->bool_Recurs(str+u,dp+1,2)));
            }
            else { 
               ptr=new char[len+10];
               strcpy(ptr,str);
               strcat(ptr," [ungram]");
               pnn->set_name(ptr);
               delete [] ptr;
               return(NULL);
            }
         }
         else if((!k)&&(j==len-1)){
            *(str+j)='\0';
            ptr=new char[len+3];
            this->interpret(str+i+1,ptr);
            ind=this->from_String(ptr,dp,0);
            delete [] ptr;
            return(ind);
         }
         else {
            ptr=new char[len+10];
            strcpy(ptr,str);
            strcat(ptr," [ungram]");
            pnn->set_name(ptr);
            delete [] ptr;
            return(NULL);
         }
         break;
      default:
         pa=findw(str+i,"and");
         if(pa==1){
            ptr=new char[len+10];
            strcpy(ptr,str);
            strcat(ptr," [ungram]");
            pnn->set_name(ptr);
            delete [] ptr;
            return(NULL);
         }
         po=findw(str+i,"or");
         if(po==1){
            ptr=new char[len+10];
            strcpy(ptr,str);
            strcat(ptr," [ungram]");
            pnn->set_name(ptr);
            delete [] ptr;
            return(NULL);
         }
         pb=findw(str+i,"butnot");
         if(pb==1){
            ptr=new char[len+10];
            strcpy(ptr,str);
            strcat(ptr," [ungram]");
            pnn->set_name(ptr);
            delete [] ptr;
            return(NULL);
         }
         if(pa&&(((!po)||(po>pa))&&((!pb)||(pb>pa)))){
            *(str+i+pa-2)='\0';
            pnn->set_name(" AND ");
            return(this->bool_And(this->bool_Recurs(str+i,dp+1,1),\
                   this->bool_Recurs(str+i+pa+3,dp+1,2)));
         }
         if(po&&(((!pa)||(pa>po))&&((!pb)||(pb>po)))){
            *(str+i+po-2)='\0';
            pnn->set_name(" OR ");
            return(this->bool_Or(this->bool_Recurs(str+i,dp+1,1),\
                   this->bool_Recurs(str+i+po+2,dp+1,2)));
         }
         if(pb&&(((!pa)||(pa>pb))&&((!po)||(po>pb)))){
            *(str+i+pb-2)='\0';
            pnn->set_name(" BUTNOT ");
            return(this->bool_Butnot(this->bool_Recurs(str+i,dp+1,1),\
                   this->bool_Recurs(str+i+pb+6,dp+1,2)));
         }
         
         ptr=new char[len+4];
         qtr=new char[len+4];
         this->extract(str+i,ptr,j);
         this->interpret(ptr,qtr);
         if(j<len){
            pnn->set_name(" AND ");
            ind=this->from_String(qtr,dp+1,1);
            delete [] ptr;
            delete [] qtr;
            return(this->bool_And(ind,this->bool_Recurs(str+i+j,dp+1,2)));
         }
         else {
            ind=this->from_String(qtr,dp,0);
            delete [] ptr;
            delete [] qtr;
            return(ind);
         }
         break;
   }
}

Index *Bool_pth::bool_Parse(char *str){
   err_flag=0;
   pxn[0]=new Pnxde;
   return(this->bool_Recurs(str,0,0));
}

void Bool_pth::interpret(char *str,char *ptr){
   long i,j,k,len;
   long u,v,pa,po,pb;
   char *ktr;

   len=strlen(str);
   i=0;
   while((i<len)&&(*(str+i)==' '))i++;
   j=len-1;
   while((j>=i)&&(*(str+j)==' '))j--;
   *(str+j+1)='\0';
   len=j+1;

   if(strchr(str+i,'!')!=NULL){
      strcpy(ptr,str+i);
      return;
   }
   else if(strchr(str+i,'*')!=NULL){
      strcpy(ptr,str+i);
      return;
   }
   else {
      v=0;
      while((v<len)&&(*(str+i+v)!=' '))v++;
      if(k=findw(str+i,"[text]")){
         j=i+k-2;
         while((j>=i)&&(*(str+j)==' '))j--;
         *(str+j+1)='\0';
         strcpy(ptr,str+i);
         u=j+1-i;
         *(ptr+u)='!';
         *(ptr+u+1)='!';
         if(v<k-2)*(ptr+u+2)='p';
         else *(ptr+u+2)='t';
         *(ptr+u+3)='\0';
         return;
      }
      if(k=findw(str+i,"[title]")){
         j=i+k-2;
         while((j>=i)&&(*(str+j)==' '))j--;
         *(str+j+1)='\0';
         strcpy(ptr,str+i);
         u=j+1-i;
         *(ptr+u)='!';
         *(ptr+u+1)='!';
         if(v<k-2)*(ptr+u+2)='P';
         else *(ptr+u+2)='T';
         *(ptr+u+3)='\0';
         return;
      }
      if(k=findw(str+i,"[mesh]")){
         j=i+k-2;
         while((j>=i)&&(*(str+j)==' '))j--;
         *(str+j+1)='\0';
         if(ktr=strchr(str+i,'/'))*ktr='!';
         else {*(str+j+1)='!';*(str+j+2)='\0';}
         strcpy(ptr,str+i);
         return;
      }
      strcpy(ptr,str+i);
      u=len;
      *(ptr+u)='!';
      *(ptr+u+1)='!';
      if(v<len)*(ptr+u+2)='p';
      else *(ptr+u+2)='t';
      *(ptr+u+3)='\0';
      return;
   }
}

long Bool_pth::findw(char *str,const char *w){
   long i,j,k,len,wl;
   char *ptr;

   len=strlen(str);
   wl=strlen(w);
   k=0;
   i=0;
   while(i<len){
      if((ptr=strstr(str+i,w))!=NULL){
         j=ptr-str;
         if(((!j)||(*(str+j-1)==' '))&&((j+wl==len)||(*(str+j+wl)==' '))){
            k=j+1;
            i=len;
         }
         else i=j+wl;
      }
      else i=len;
   }
   return(k);
}

void Bool_pth::extract(char *str,char *ptr,long &n){
   long i,j,k,len,u,v;
   char *utr,c;

   utr=strchr(str,' ');
   if(utr==NULL){
      strcpy(ptr,str);
      n=strlen(str);
      return;
   }
   if(k=this->findw(str,"[text]")){
      c=*(str+k+5);
      *(str+k+5)='\0';
      strcpy(ptr,str);
      *(str+k+5)=c;
      n=k+5;
      return;
   }
   if(k=this->findw(str,"[title]")){
      c=*(str+k+6);
      *(str+k+6)='\0';
      strcpy(ptr,str);
      *(str+k+6)=c;
      n=k+6;
      return;
   }
   if(k=this->findw(str,"[mesh]")){
      c=*(str+k+5);
      *(str+k+5)='\0';
      strcpy(ptr,str);
      *(str+k+5)=c;
      n=k+5;
      return;
   }
   u=utr-str;
   utr=strchr(str,'!');
   if(utr!=NULL){
      v=utr-str;
      k=v+1;
      len=strlen(str);
      while((k<len)&&(*(str+k)!=' '))k++;
      *(str+k)='\0';
      strcpy(ptr,str);
      n=k+1;
      return;
   }
   else {
      *(str+u)='\0';
      strcpy(ptr,str);
      n=u+1;
      return;
   }
}

void Bool_pth::standard(char *str,char *ptr){
   long i,len,uflag;
   char *ktr;

   uflag=0;
   if(strstr(str,"[absent]"))uflag=1;
   else if(strstr(str,"[ungram]"))uflag=2;

   if(ktr=strstr(str,"!!t")){
      *ktr='\0';
      strcpy(ptr,str);
      strcat(ptr," [TEXT]");
   }
   else if(ktr=strstr(str,"!!T")){
      *ktr='\0';
      strcpy(ptr,str);
      strcat(ptr," [TITLE]");
   }
   else if(ktr=strstr(str,"!!p")){
      *ktr='\0';
      strcpy(ptr,str);
      strcat(ptr," [TEXT]");
   }
   else if(ktr=strstr(str,"!!P")){
      *ktr='\0';
      strcpy(ptr,str);
      strcat(ptr," [TITLE]");
   }
   else if(ktr=strchr(str,'!')){
      len=strlen(str);
      i=ktr-str;
      if((i<len-1)&&(*(ktr+2)!='[')){
         *(ktr)='/';
         if(ktr=strchr(str,'['))*(ktr-1)='\0';
      }
      else *ktr='\0';
      strcpy(ptr,str);
      strcat(ptr," [MESH]");
   }
   else if(ktr=strchr(str,'*')){
      if(ktr=strchr(str,'['))*(ktr-1)='\0';
      strcpy(ptr,str);
      strcat(ptr," [MESH]");
   }
   else {
      ktr=strchr(str,'[');
      *(ktr-1)='\0';
      strcpy(ptr,str);
   }
   if(uflag==1)strcat(ptr," [absent]");
   else if(uflag==2)strcat(ptr," [ungram]");
}

void Bool_pth::standard_String(char *str){
   long i,dep;
   char cnam[max_str];

   for(i=0;i<100;i++)bra[i]=0;
   dep=0;
   *str='\0';

   while((dep>0)||(bra[dep]<2)){
      if((pxn[dep]->lft==NULL)||(pxn[dep]->rht==NULL)){
         this->standard(pxn[dep]->nam,cnam);
         strcat(str,cnam);
         delete pxn[dep];
         dep--;
         if(dep==-1){
            dep=0;
            bra[0]=2;
         }
      }
      else {
         if(bra[dep]==0){
            if(dep)strcat(str,"(");
            bra[dep]=1;
            pxn[dep+1]=pxn[dep]->lft;
            dep++;
         }
         else if(bra[dep]==1){
            strcat(str,pxn[dep]->nam);
            bra[dep]=2;
            pxn[dep+1]=pxn[dep]->rht;
            dep++;
         }
         else if(bra[dep]==2){
            if(dep)strcat(str,")");
            bra[dep]=0;
            delete pxn[dep];
            dep--;
         }
      }
   }
}

}
