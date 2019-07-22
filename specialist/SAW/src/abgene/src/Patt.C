#include <fstream>
#include <iostream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include "runn.h"
#include "Patt.h"
using namespace std;
namespace iret {

Anode::Anode(const char *key,int val){
   pc=strlen(key);
   sc=new char[pc+1];
   strcpy(sc,key);
   value=val;
   pa=NULL;
}

Anode::~Anode(){
   delete [] pa;
   delete [] sc;
}

Patt::Patt(int max_pat_len){
   long i;

   max_pat=max_pat_len;
   shift=(int **)new long[max_pat];
   for(i=0;i<max_pat;i++)*(shift+i)=NULL;
   pshift=new int[128];
   for(i=0;i<128;i++)*(pshift+i)=max_pat;
   root=NULL;
   sht_pat=0;
   lng_pat=0;
}

Patt::~Patt(){
   for(long i=0;i<max_pat;i++){
      if(*(shift+i))delete [] *(shift+i);
   }
   delete [] shift;
   delete [] pshift;
}

void Patt::add_patt(const char *pat,int val){
   int len=strlen(pat);
   if(sht_pat==0)sht_pat=len;
   else sht_pat=(len<sht_pat)?len:sht_pat;
   lng_pat=(len>lng_pat)?len:lng_pat;
   int i,j,k=strlen(pat),lc,n,*sqt;
   char *rev=new char[k+1],*spt;
   for(i=0;i<k;i++)*(rev+i)=*(pat+k-i-1);
   rev[k]='\0';
   
   Anode *ptr=root,*qtr;
   if(ptr==NULL)root=new Anode(rev,val);
   else {
     i=0;
     while(ptr!=NULL){
        j=0;
        while((j<ptr->pc)&&(*(rev+i+j)==*((ptr->sc)+j)))j++;
        if(j==ptr->pc){ //Current string matched perfectly.
           if(i+j==k){  //All the query string was used in the match exactly.
              ptr->value=val; 
              ptr=NULL;
           }
           else if(i+j<k){ //Some of the query string remains.
              i+=j;
              lc=(int)*(rev+i);
              if(ptr->pa==NULL){ //No provision exists at this node for extension.
                 ptr->pa=(Anode **)new long[128];
                 for(n=0;n<128;n++)(ptr->pa)[n]=NULL;
                 ptr->pa[lc]=new Anode(rev+i+1,val);
                 ptr=NULL;
              }
              else if(ptr->pa[lc]==NULL){ //No extension available for this character.
                 ptr->pa[lc]=new Anode(rev+i+1,val);
                 ptr=NULL;
              }
              else { //Extension available at this character.
                 ptr=ptr->pa[lc];
                 i++;
              }        
           }
        }
        else { //Mismatch occurred in current string at j.
            qtr=new Anode(ptr->sc+j+1,ptr->value);
            ptr->value=0;
            qtr->pa=ptr->pa;
            ptr->pa=(Anode **)new long[128];
            for(n=0;n<128;n++)(ptr->pa)[n]=NULL;
            lc=(int)*(ptr->sc+j);
            (ptr->pa)[lc]=qtr;
            spt=ptr->sc;
            *(spt+j)='\0';
            ptr->pc=j;
            ptr->sc=new char[j+1];
            strcpy(ptr->sc,spt);
            delete [] spt;
            lc=(int)*(rev+i+j);
            (ptr->pa)[lc]=new Anode(rev+i+j+1,val);
            ptr=NULL;
        }
     }
  }
   i=0; //General shift array.
   while((i<sht_pat) && (*(shift+i))){
      sqt=*(shift+i);
      for(j=0;j<128;j++){
         n=*(sqt+j);
         *(sqt+j)=(n<sht_pat-i)?n:(sht_pat-i);
      }
      i++;
   }
   while(i<sht_pat){
      sqt=*(shift+i)=new int[128];
      for(j=0;j<128;j++)*(sqt+j)=sht_pat-i;
      i++;
   }
   for(i=0;i<sht_pat;i++){
      sqt=*(shift+i);
      for(j=1;j<sht_pat-i;j++){ //Must move at least one character to obtain a match.
         lc=(int)*(rev+i+j);
         n=*(sqt+lc);
         *(sqt+lc)=(j<n)?j:n;     
      }
   }
   //Special shift array.
   for(j=0;j<128;j++){
      n=*(pshift+j);
      *(pshift+j)=(n<sht_pat)?n:sht_pat;
   }
   for(j=0;j<sht_pat;j++){ //May not have to move to obtain a match.
      lc=(int)*(rev+j);
      n=*(pshift+lc);
      *(pshift+lc)=(j<n)?j:n;     
   }
   delete [] rev;
}   

void Patt::make_chain(){
   Chain *ptr,*qtr,*rtr;
   long i,j=0;

   rtr=ptr=new Chain;
   ptr->ca='\0';
   for(i=1;i<lng_pat;i++){
      qtr=ptr->fw=new Chain;
      qtr->bw=ptr;
      qtr->ca='\0';
      ptr=qtr;
   }
   ptr->fw=rtr;
   rtr->bw=ptr;
   pchain=rtr;
}
      
void Patt::reset(){
   pchain->ca='\0';
   pchain=pchain->fw;
   pass=sht_pat;
}

int Patt::trigger(char &c){
   pchain->ca=c;
   pchain=pchain->fw;
   pass--;
   if(pass)return(0);
   if(pass=*(pshift+(int)c))return(0);
   int val=0,depth=0,lc;
   Chain *qchain;
   Anode *pnd=root;
   qchain=pchain->bw;
   int j=0;
   while(pnd){
      if(j<pnd->pc){ //Within the string.
         if(qchain->ca==*(pnd->sc+j)){ //For a match move down the string.
            j++;
            depth++;
            qchain=qchain->bw;
         }
         else {
            pnd=NULL; //For a mismatch terminate the search.
         }
      }
      else { //One step beyond the string.
         if(val=pnd->value)break;
         lc=(int)qchain->ca;
         if(pnd->pa==NULL)pnd=NULL; //No list at this node.
         else {
            pnd=pnd->pa[lc];
            if(pnd){  //If list exists and is not null at this character 
                      //we have a match.
               j=0;
               depth++;
               qchain=qchain->bw;
            }
         }
      }
   }
   if(!val){
      if(depth>=sht_pat)pass=1; //For a mismatch terminate the search.
      else {
         lc=(int)qchain->ca;
         pass=*(*(shift+depth)+lc);
      }
   }
   return(val);    
}
 
int Patt::capture(long &m,char *txt,char &c){
   *(txt+m++)=c;
   pchain->ca=c;
   pchain=pchain->fw;
   pass--;
   if(pass)return(0);
   if(pass=*(pshift+(int)c))return(0);
   int val=0,depth=0,lc;
   Chain *qchain;
   Anode *pnd=root;
   qchain=pchain->bw;
   int j=0;
   while(pnd){
      if(j<pnd->pc){ //Within the string.
         if(qchain->ca==*(pnd->sc+j)){ //For a match move down the string.
            j++;
            depth++;
            qchain=qchain->bw;
         }
         else {
            pnd=NULL; //For a mismatch terminate the search.
         }
      }
      else { //One step beyond the string.
         if(val=pnd->value)break;
         lc=(int)qchain->ca;
         if(pnd->pa==NULL)pnd=NULL; //No list at this node.
         else {
            pnd=pnd->pa[lc];
            if(pnd){  //If list exists and is not null at this character
                      //we have a match.
               j=0;
               depth++;
               qchain=qchain->bw;
            }
         }
      }
   }
   if(!val){
      if(depth>=sht_pat)pass=1; //For a mismatch terminate the search.
      else {
         lc=(int)qchain->ca;
         pass=*(*(shift+depth)+lc);
      }
   }
   else {
      m-=depth;
   }
   return(val);    
}

}
