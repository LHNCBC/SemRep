#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include "Btree.h"
#include "runn.h"

using namespace std;
namespace iret {

Node::Node(void){
   str=NULL;
   rel=NULL;
   pdn=NULL;
}

Node::Node(const char *ptr){
   int i=strlen(ptr);
   str = new char[i+1];
   strcpy(str,ptr);
   rel=NULL;
   pdn=NULL;
}

Node::Node(char const *ptr,void *dtr){
   int i=strlen(ptr);
   str = new char[i+1];
   strcpy(str,ptr);
   rel = dtr;
   pdn=NULL;
}

Node::~Node(){
   delete [] str;
}

void Node::debug(void){
   cout << "Node {" << endl;
   cout << "   str: " << this->str << endl;
   if(rel==NULL)cout << "   rel: NULL" << endl;
   else cout << "   rel: " << (long)rel << endl;
   if(pdn==NULL)cout << "   pdn: NULL" << endl;
   else cout << "   pdn: " << (long)pdn << endl;
   cout << " }" << endl;
}

Page::Page(){
   pdn=NULL;
   ndnm='\0';
}

Page::Page(Page *const pz,Page *const pn,const int n){
   pdn=pn;
   int j=(int)(pz->ndnm)-n;
   ndnm=(char)(j>0 ? j : 0);
   for(int i=0;i<(int)ndnm;i++){pnd[i]=(pz->pnd)[n+i];}
}

Page::~Page(){ 
   for(int i=0;i<(int)ndnm;i++){
      delete pnd[i];
   }
}

void Page::insert(const int n,Node * const nd,const int j){
   assert(j<ord2);
   assert(n<=j);
   if(n==j){
      pnd[j]=nd;
   } 
   else {
      for(int i=j;i>n;i--)pnd[i]=pnd[i-1];
      pnd[n]=nd;
   }   
   ndnm++;
}

int Page::search(int &a,int &b,const char *str,int &p){
   int j;
   if((j=stc_my(a,b,str,pnd[0]->str))<0){
      p=0;
      return(0);
   }
   else if(j==0){
      p=0;
      return(1);
   }
   if((j=stc_my(a,b,str,pnd[(int)(ndnm-1)]->str))>0){
      p=(int)ndnm;
      return(0);
   }
   else if(j==0){
      p=(int)(ndnm-1);
      return(1);
   }
   int x=0,i;
   int y=(int)(ndnm-1);
   while(y-x>1){
      i=(y+x)/2;
      if((j=stc_my(a,b,str,pnd[i]->str))==0){p=i;return(1);}
      else if(j<0)y=i;
      else x=i;
   }
   p=y;
   return(0);
}

int Page::search(int &a,int &b,char *str,int &p,Partial_match *btr){
   int j;
   if((j=btr->stc_my_long(a,b,str,pnd[0]->str,0))<0){
      p=0;
      return(0);
   }
   else if(j==0){
      p=0;
      return(1);
   }
   if((j=btr->stc_my_long(a,b,str,pnd[(int)(ndnm-1)]->str,(int)(ndnm-1)))>0){
      p=(int)ndnm;
      return(0);
   }
   else if(j==0){
      p=(int)(ndnm-1);
      return(1);
   }
   int x=0,i;
   int y=(int)(ndnm-1);
   while(y-x>1){
      i=(y+x)/2;
      if((j=btr->stc_my_long(a,b,str,pnd[i]->str,i))==0){p=i;return(1);}
      else if(j<0)y=i;
      else x=i;
   }
   p=y;
   return(0);
}

void Page::debug(void){
   cout << "Page {" << endl;
   cout << "   ndnm: " << (int)ndnm << endl;
   if(pdn==NULL)cout << "   pdn: NULL" << endl;
   else cout << "   pdn: " << (long)pdn << endl;
   for(int i=0;i<(int)ndnm;i++){
      cout << i << " ";
      (this->pnd[i])->debug();
   }
   cout << " }" << endl;
}

int stc_my(int &a,int &b,const char *str,const char *ptr)
   {register int i=(a<b) ? a : b;
   register const char *p1=str+i;
   register const char *p2=ptr+i;
   register int j=0;
   while((*p1==*p2)&&(*p1!='\0')){
      j++;
      p1++;
      p2++;
   }
   if(*p1==*p2)return(0);
   else if(*p1<*p2){
      b=i+j;
      return(-1);
   }
   else {
      a=i+j;
      return(1);
   }
}

Btree::Btree(){
   copy=false;
   depth=0;
   root=new Page();
   root->ndnm = 1;
   (root->pnd)[0]=new Node("");
}

int Btree::search(const char *str){
   depth=-1;
   Page *pu=root;
   register int a=0,b=0,i,j;
   while(pu!=NULL){
      depth++;
      pg[depth]=pu;
      j=(pu->search)(a,b,str,i);
      cnd[depth]=i;
      if(j==1)return(1);
      if(i==0)pu=pu->pdn;        
      else pu=(pu->pnd)[i-1]->pdn;
   }
return(0);
}

int Btree::insert(Node *nd){
   int w,k;
   Page *pm,*pz;
   while((nd!=NULL)&&(depth)){
      pm=pg[depth];
      w=pm->ndnm;
      if(w<ord2){
         pm->insert(cnd[depth],nd,w);
         nd=NULL;
      }
      else {
         k=cnd[depth];
         if(k<order){
             pz=new Page(pm,((pm->pnd)[order-1])->pdn,order);
             pm->insert(k,nd,order);
             nd=pm->pnd[order];
             nd->pdn=pz;
             pm->ndnm=order;
         }
         else if(k>order){
             pz=new Page(pm,((pm->pnd)[order])->pdn,order+1);
             pz->insert(k-order-1,nd,order-1);
             nd=pm->pnd[order];
             nd->pdn=pz;
             pm->ndnm=order;
         }
         else {
             pz=new Page(pm,nd->pdn,order);    
             nd->pdn=pz;
             pm->ndnm=order;
         }
      }
   depth--;
   }
   if(nd!=NULL){
      pm=pg[depth];
      w=pm->ndnm;
      if(w<ord2)pm->insert(cnd[depth],nd,w);
      else {
         root=new Page();
         root->pdn=pm;
         k=cnd[depth];
         if(k<order){
            pz=new Page(pm,((pm->pnd)[order-1])->pdn,order);
            pm->insert(k,nd,order);
            (root->pnd)[0]=pm->pnd[order];
            ((root->pnd)[0])->pdn=pz;
            root->ndnm=1;
            pm->ndnm=order;
         }
         else if(k>order){
            pz=new Page(pm,((pm->pnd)[order])->pdn,order+1);
            pz->insert(k-order-1,nd,order-1);
            (root->pnd)[0]=pm->pnd[order];
            ((root->pnd)[0])->pdn=pz;
            root->ndnm=1;
            pm->ndnm=order;
         }
         else {
            pz=new Page(pm,nd->pdn,order);
            (root->pnd)[0]=nd;
            nd->pdn=pz;
            root->ndnm=1;
            pm->ndnm=order;
         }
      }
   }
return(1);
}
            
void Btree::node_first(void){
   depth=0;
   pg[depth]=root;
   cnd[depth]=0;
   Page *pm;
   while((pm=(pg[depth]->pdn))!=NULL){
      depth++;
      pg[depth]=pm;
      cnd[depth]=0;
   }
}

int Btree::node_next(){
   int i=cnd[depth];
   Page *pd=((pg[depth]->pnd)[i])->pdn;
   if(pd!=NULL){
      (cnd[depth])++;
      depth++;
      pg[depth]=pd;
      cnd[depth]=0;
      while((pd=(pg[depth]->pdn))!=NULL){
         depth++;
         pg[depth]=pd;
         cnd[depth]=0;
      }
   }
   else {
      cnd[depth]=++i;
      while((depth>=1)&&(i==(pg[depth]->ndnm))){depth--;i=cnd[depth];}
      if((depth==0)&&(i==(pg[depth]->ndnm)))depth--;
      if(depth<0)return(0);
   }
return(1);
}
      
char *Btree::show_str(){
   return(((pg[depth]->pnd)[cnd[depth]])->str);
}

void *Btree::give_ptr(){
   return(((pg[depth]->pnd)[cnd[depth]])->rel);
}

void Btree::set_ptr(void *dtr){
   ((pg[depth]->pnd)[cnd[depth]])->rel=dtr;
}

Btree::~Btree(){
   if (copy) return;           // only delete original
   node_first();
   int i=depth,j;
   do{
      j=node_next();
      if(depth<i){
         while(i>depth){
            delete pg[i];
            i--;
         }
      }
      else i=depth;
   } while(j); 
}

long Btree::list_write(ofstream &fout){
   int pflag=get_qflag();
   long ct=0;
   node_first();
   while(node_next()){
      fout << show_str() << endl;
      mark(pflag,++ct,1000,"strings written");
   }
   fout.close();
   return((int)fout.good());
}

Btree::Btree(ifstream &fin){
   copy=false;
   char cnam[256];
   int pflag=get_qflag();
   depth=0;
   pg[0]=root=new Page();
   cnd[0]=root->ndnm = 1;
   (root->pnd)[0]=new Node("");
   Node *pno;
   long ct=0;
   while(get_string(cnam,fin,'\n')){
      pno = new Node(cnam);  
      add(pno);
      mark(pflag,++ct,10000,"strings read");
   }
   fin.close();
}

int Btree::add(Node *nd){
   int w,k,dp;
   Page *pm,*pz;
   dp=depth; //uses dp in place of depth in insert.
   while((nd!=NULL)&&(dp)){
      pm=pg[dp];
      w=pm->ndnm;
      if(w<ord2){
         pm->insert(cnd[dp],nd,w);
         nd=NULL;
      (cnd[dp])++; //variation from insert.
      }
      else {
         k=cnd[dp];
         if(k<order){
             pz=new Page(pm,((pm->pnd)[order-1])->pdn,order);
             pm->insert(k,nd,order);
             nd=pm->pnd[order];
             nd->pdn=pz;
             pm->ndnm=order;
         }
         else if(k>order){
             pz=new Page(pm,((pm->pnd)[order])->pdn,order+1);
             pz->insert(k-order-1,nd,order-1);
             nd=pm->pnd[order];
             nd->pdn=pz;
             pm->ndnm=order;
         }
         else {
             pz=new Page(pm,nd->pdn,order);    
             nd->pdn=pz;
             pm->ndnm=order;
         }
         pg[dp]=pz; //2 lines of variation from insert.
         cnd[dp]=order;
      }
   dp--;
   }
   if(nd!=NULL){
      pm=pg[dp];
      w=pm->ndnm;
      if(w<ord2){
         pm->insert(cnd[dp],nd,w);
         (cnd[dp])++; //variation from insert.
      }
      else {
         root=new Page();
         root->pdn=pm;
         k=cnd[dp];
         if(k<order){
            pz=new Page(pm,((pm->pnd)[order-1])->pdn,order);
            pm->insert(k,nd,order);
            (root->pnd)[0]=pm->pnd[order];
            ((root->pnd)[0])->pdn=pz;
            root->ndnm=1;
            pm->ndnm=order;
         }
         else if(k>order){
            pz=new Page(pm,((pm->pnd)[order])->pdn,order+1);
            pz->insert(k-order-1,nd,order-1);
            (root->pnd)[0]=pm->pnd[order];
            ((root->pnd)[0])->pdn=pz;
            root->ndnm=1;
            pm->ndnm=order;
         }
         else {
            pz=new Page(pm,nd->pdn,order);
            (root->pnd)[0]=nd;
            nd->pdn=pz;
            root->ndnm=1;
            pm->ndnm=order;
         }
      next_empty(); //variation from insert.
      }
   }
return(1);
}
            
void Btree::next_empty(){
   depth=0;
   pg[depth]=root;
   int i=cnd[depth]=root->ndnm;
   Page *pm;
   while((pm=((pg[depth]->pnd)[i-1])->pdn)!=NULL){
      depth++;
      pg[depth]=pm;
      i=cnd[depth]=pm->ndnm;
   }
}

Str_str::Str_str() : Btree() {
}

Str_str::~Str_str(){
   if(copy)return;
   this->node_first();
   while(this->node_next())delete [] (char*)this->give_ptr();
}

void Str_str::add_pair(const char *one,const char *two){
   Node *pnd;
   if(search(one)){
      cout << "Duplicate string in keys list = " << one << endl;
      exit(0);
   }   
   else {
      int i=strlen(two);
      char *st=new char[i+1];
      strcpy(st,two);
      pnd=new Node(one,(void *)st);
      add(pnd);
   }
}

char *Str_str::match(const char *one){
   if(search(one)){
      return((char*)give_ptr());
   }
   else {
      cout << "String not a key = " << one << endl;
      exit(0);
   }
}

List::List() : Btree() {
   cnt_key=0;
}

List::~List(){
}

void List::add_key(const char *str){
   Node *pnd;
   if(!search(str)){
      pnd=new Node(str);
      add(pnd);
   }
}

void List::add_key_count(const char *str){
   Node *pnd;
   if(!search(str)){
      pnd=new Node(str);
      add(pnd);
      cnt_key++;
   }
}

Num_num::Num_num() : Btree() {
}

Num_num::~Num_num(){
   if(copy)return;
   this->node_first();
   while(this->node_next())delete (long*)this->give_ptr();
}

void Num_num::add_pair(long i,long j){
   Node *pnd;
   char cnam[256];
   long_str(cnam,i);
   if(!search(cnam)){
      long *st=new long;
      *st=j;
      pnd=new Node(cnam,(void *)st);
      add(pnd);
   }
}

long Num_num::match(long i){
   char cnam[256];
   long_str(cnam,i);
   if(search(cnam)){
      return(*((long*)give_ptr()));
   }
   else return(LNEG);
}

Count::Count() : List() {
   total=0;
}

Count::~Count(){
   if(copy)return;
   long *pk;
   this->node_first();
   while(this->node_next()){
      pk=(long*)(this->give_ptr());
      if(pk)delete pk;
   }
}

void Count::add_count(const char *pch,long n){
   long *ppt;
   Node *np;
   total+=n;
   if(this->search(pch)==0){
      ppt = new long;
      (*ppt) =n;
      np=new Node(pch,(void*)ppt);
      this->insert(np);
   }
   else {
      (*(long*) this->give_ptr())+=n;
   }
}

void Count::add_count2(const char *pch,long n){
   long *ppt;
   Node *np;
   total+=n;
   if(this->search(pch)==0){
      ppt = new long;
      (*ppt) =n;
      np=new Node(pch,(void*)ppt);
      this->insert(np);
      cnt_key++;
   }
   else {
      (*(long*) this->give_ptr())+=n;
   }
}

long Count::count(const char *pch){
   if(this->search(pch)==0){
      return(0);
   }
   else {
      return(*((long*) this->give_ptr()));
   }
}

long Count::count(void){
   return(*((long*) this->give_ptr()));
}

Partial_match::Partial_match() : Count() {
}

Partial_match::~Partial_match(){
}

void Partial_match::long_match(char *str,List &Lst){
   char *pch;
   while(*str!='\0'){
      if(this->search_long(str)){
         pch=this->show_str();
         Lst.add_key_count(pch);
      }
      if((pch=strchr(str,' '))!=NULL)str=pch+1;
      else str=str+strlen(str);
   }   
}

void Partial_match::local_match(char *str,List &Lst){
   char *pch;
   int i,j;
   if(*str!='\0'){
      if(this->search_long(str)){
         pch=this->show_str();
         Lst.add_key_count(pch);
         i=strlen(pch)-1;
         while(0<i){
            while((0<i)&&(*(str+i)!=' '))i--;
            if(0<i){
               *(str+i)='\0';
               j=this->search(str);
               *(str+i)=' ';
               if(j){
                  pch=this->show_str();
                  Lst.add_key_count(pch);
               }
               i--;
            }
         }
      }
   }
}

void Partial_match::all_match(char *str,List &Lst){
   char *pch;
   int i,j;
   while(*str!='\0'){
      if(this->search_long(str)){
         pch=this->show_str();
         Lst.add_key_count(pch);
         i=strlen(pch)-1;
         while(0<i){
            while((0<i)&&(*(str+i)!=' '))i--;
            if(0<i){
               *(str+i)='\0';
               j=this->search(str);
               *(str+i)=' ';
               if(j){
                  pch=this->show_str();
                  Lst.add_key_count(pch);
               }
               i--;
            }
         }
      }
      if((pch=strchr(str,' '))!=NULL)str=pch+1;
      else str=str+strlen(str);
   }   
}

void Partial_match::long_match(char *str,Count &Cnt,long n){
   char *pch;
   while(*str!='\0'){
      if(this->search_long(str)){
         pch=this->show_str();
         Cnt.add_count2(pch,n);
      }
      if((pch=strchr(str,' '))!=NULL)str=pch+1;
      else str=str+strlen(str);
   }   
}

void Partial_match::local_match(char *str,Count &Cnt,long n){
   char *pch;
   int i,j;
   if(*str!='\0'){
      if(this->search_long(str)){
         pch=this->show_str();
         Cnt.add_count2(pch,n);
         i=strlen(pch)-1;
         while(0<i){
            while((0<i)&&(*(str+i)!=' '))i--;
            if(0<i){
               *(str+i)='\0';
               j=this->search(str);
               *(str+i)=' ';
               if(j){
                  pch=this->show_str();
                  Cnt.add_count2(pch,n);
               }
               i--;
            }
         }
      }
   } 
}

void Partial_match::all_match(char *str,Count &Cnt,long n){
   char *pch;
   int i,j;
   while(*str!='\0'){
      if(this->search_long(str)){
         pch=this->show_str();
         Cnt.add_count2(pch,n);
         i=strlen(pch)-1;
         while(0<i){
            while((0<i)&&(*(str+i)!=' '))i--;
            if(0<i){
               *(str+i)='\0';
               j=this->search(str);
               *(str+i)=' ';
               if(j){
                  pch=this->show_str();
                  Cnt.add_count2(pch,n);
               }
               i--;
            }
         }
      }
      if((pch=strchr(str,' '))!=NULL)str=pch+1;
      else str=str+strlen(str);
   }   
}

int Partial_match::search_long(char *str){
   int a=0,b=0,i,j;
   len=strlen(str);
   if(this->step_one(a,b,str))return(1);
   i=(a<b)?b:a;
   while(cln_o<i){
      while((cln_o<i)&&(*(str+i)!=' '))i--;
      if(cln_o<i){
         *(str+i)='\0';
         j=this->search(str);
         *(str+i)=' ';
         if(j)return(1);
         i--;
      }
   }
   if(cln_o){
      depth=depth_o;
      cnd[depth]=index_o;
      return(1);
   }
   else return(0);
}

int Partial_match::step_one(int &a,int &b,char *str){
   char c;
   cln_o=0;
   cln=0;
   while((c=*(str+cln))&&c!=32)cln++;
   *(str+cln)='\0';
   depth=-1;
   Page *pu=root;
   int i,j;
   while(pu!=NULL){
      depth++;
      pg[depth]=pu;
      j=(pu->search)(a,b,str,i,this);
      cnd[depth]=i;
      if(j==1)return(1);
      if(i==0)pu=pu->pdn;        
      else pu=(pu->pnd)[i-1]->pdn;
   }

if(cln<len)*(str+cln)=' ';
return(0);
}

int Partial_match::stc_my_long(int &a,int &b,char *str,const char *ptr,int index)
   {char c;
   int i=(a<b) ? a : b;
   const char *p1=str+i;
   const char *p2=ptr+i;
   int j=0;
   while((*p1==*p2)&&(*p1!='\0')){
      j++;
      p1++;
      p2++;
      if((*p1=='\0'&&*p2!='\0')&&(cln<len)){
         *(str+cln++)=' ';
         while((c=*(str+cln))&&c!=32)cln++;
         *(str+cln)='\0';
      }
   }
   if(*p1==*p2){
      if(cln<len){
         depth_o=depth;
         index_o=index;
         cln_o=cln;
         *(str+cln++)=' ';
         while((c=*(str+cln))&&c!=32)cln++;
         *(str+cln)='\0';
         a=i+j;
         return(1);
      }
      else return(0);
   }
   else if(*p1<*p2){
      b=i+j;
      return(-1);
   }
   else {
      a=i+j;
      return(1);
   }
}

}
