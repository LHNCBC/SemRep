#include <iostream>
#include <fstream>
#include <string>
#include "Btree.h"
#include "Word.h"
#include "Thes.h"
using namespace std;
namespace iret {

Synon::Synon(void){
   num=0;
   prf=NULL;
   list=NULL;
}

Synon::~Synon(void){
   if(list!=NULL){
      syn_lk *qtr=list;
      syn_lk *ptr;
      do{
         ptr=qtr->next;
         delete qtr;
         qtr=ptr;
      }while(qtr!=NULL);
   }
}

void Synon::add_element(char *str){
   syn_lk *em,*fm;
   int flag=1;
   if(!list){list=new syn_lk;em=list;}
   else {
      fm=em=list;
      while(em){
         if(str==em->syn){
            flag=0;
            break;
         }
         fm=em;
         em=em->next;
      }
      if(flag){
         fm->next=em=new syn_lk;
      }
   }
   if(flag){
      em->syn=str;
      em->next=NULL;
      num++;
   }
}

void Synon::rmv_element(char *str){
   syn_lk *em,*fm;
   if(!list)return;
   else if(str==list->syn){
      em=list->next;
      num--;
      delete list;
      list=em;
   }
   else if(list->next){
      fm=list;
      em=list->next;
      while(em){
         if(str==em->syn){
            fm->next=em->next;
            delete em;
            num--;
            break;
         }
         fm=em;
         em=em->next;
      }
   }
}

Thes::Thes(): List() {
}

Thes::Thes(const Thes &Th): List(Th) {
}

Thes::~Thes(){
   if(copy)return;
   Synon *pSyn;
   this->node_first();
   while(this->node_next()){
      pSyn = (Synon *)(this->give_ptr());
      if(pSyn !=NULL)delete pSyn;             
   }
}

void Thes::add_syn(const char *str1, const char *str2){
   Node *pnd;
   Synon *pSyn, *qSyn;

   //First string handled
   if(!search(str1)){
      pSyn=new Synon;
      pnd=new Node(str1,(void *)pSyn);
      add(pnd);
      cnt_key++;
      pSyn->add_element(pnd->str);
   }
   else {
      pSyn=(Synon *)give_ptr();
      if(!pSyn){
         pSyn=new Synon;
         set_ptr((void*)pSyn);
         pSyn->add_element(show_str());
      }
   }
   //Second string handled
   if(!search(str2)){
      pnd=new Node(str2,(void *)pSyn);
      add(pnd);
      cnt_key++;
      pSyn->add_element(pnd->str);
   }
   else {
      qSyn=(Synon *)give_ptr();
      if((qSyn)&&(qSyn!=pSyn)){
         if(qSyn->num<2)delete qSyn;
         else qSyn->rmv_element(show_str());
      }
      set_ptr((void*)pSyn);
      pSyn->add_element(show_str());
   }
}

int Thes::set_prf(const char *pch){
   Synon *pSyn;
   if(search(pch)){
      pSyn=(Synon *)this->give_ptr();
      if(pSyn){
         pSyn->prf=this->show_str();    
         return(1);
      }
   }
   return(0);         
}

char * Thes::prf_str(const char *pch){
   Synon *pSyn;
   if(search(pch)){
      pSyn=(Synon *)this->give_ptr();
      if(pSyn)return(pSyn->prf);
   } 
   return(NULL); 
}

char * Thes::prf_str(void){
   Synon *pSyn=(Synon *)this->give_ptr();
   if(pSyn)return(pSyn->prf);
   else return(NULL);
}

int Thes::set_Synon(const char *str){
   if(search(str)){
      pSynon=(Synon *)this->give_ptr();
      return(1);
   }
   else {
      pSynon=NULL;
      return(0);         
   }
}

int Thes::synon(const char *str){
   Synon *pSyn;
   if(search(str)){
      pSyn=(Synon *)this->give_ptr();
      if(pSyn)return((pSyn==pSynon));
   }
   return(0);
}

int Thes::synon(const char *str1,const char *str2){
   set_Synon(str1);
   return(synon(str2));
}

int Thes::set_class(const char *str){
   if(search(str)){
      sel=show_str();
      pSynon=(Synon *)this->give_ptr();
      if(pSynon){
         pl=pSynon->list;
         if(pl)return(pSynon->num);
      }
   }
   pl=NULL;
   return(0);         
}

char *Thes::next_incl(void){
   syn_lk *ul=pl;
   if(!ul)return(NULL);
   else {
      pl=pl->next;
      return(ul->syn);
   }
}

char *Thes::next_excl(void){
   syn_lk *ul=pl;
   if(!ul)return(NULL);
   else {
      if(pl->syn==sel){
         pl=pl->next;
         ul=pl;
      }
      if(!ul)return(NULL);
      pl=pl->next;
      return(ul->syn);
   }
}

//Equivalence class construction for strings

Link_list::Link_list(){
   el=NULL;
   flag=0;
}

Link_list::~Link_list(){
  if ( ! el )
    // nothing to destroy
    return;
  
  syn_lk *tmp,*elem = el->next;
  while(elem != el){
    tmp=elem;
    elem=elem->next;
    delete tmp;
  }
  delete el;
}

void Link_list::add_element(char *str){
   syn_lk *em=new syn_lk;
   em->syn=str;

   if(!el){el=em;em->next=em;}
   else {
      em->next=el->next;
      el->next=em;
      el=em;
   }
}

void Link_list::add_unique(char *str){
   syn_lk *ej,*em=new syn_lk;
   em->syn=str;

   if(!el){el=em;em->next=em;}
   else {
      ej=el->next;
      while(((ej->syn)!=str)&&(ej!=el))ej=ej->next;
      if((ej->syn)!=str){
         em->next=el->next;
         el->next=em;
         el=em;
      }
   }
}

int Link_list::list_size(void){
   int i=0;
   if(el){
      syn_lk *em=el->next;
      i=1;
      while(em!=el){em=em->next;i++;}
   }
   return(i);
}
   
Point::Point() {
   elem=NULL;
   elx=NULL;
   strt=new syn_lk;
}

Point::~Point() {
   delete strt;
}

BTList::BTList(): List() {
   elem=NULL;
   elx=NULL;
   lst=new List;
   strt=new syn_lk;
}

BTList::BTList(const BTList &Btlt): List(Btlt) {
   elem=NULL;
   elx=NULL;
   lst=new List(*(Btlt.lst));
   strt=new syn_lk;
}

BTList::~BTList(){
   delete lst;
   delete strt;
   if(copy)return;
   Link_list *pll;
   this->node_first();
   while(this->node_next()){
      pll=(Link_list *)this->give_ptr();
      delete pll;
   }
}

void BTList::add_element(const char *str1,const char *str2){
   Link_list *pll;
   Node *pnn;
   if(this->search(str1)){
      pll=(Link_list *)this->give_ptr();
   }
   else {
      pll=new Link_list;
      pnn=new Node((char * const)str1,(void *)pll);
      this->insert(pnn);
      cnt_key++;
   }
   if(!(lst->search(str2))){
      lst->add_key_count(str2);
      lst->search(str2);
   }
   pll->add_element(lst->show_str());
}

void BTList::add_unique(const char *str1,const char *str2){
   Link_list *pll;
   Node *pnn;
   if(this->search(str1)){
      pll=(Link_list *)this->give_ptr();
   }
   else {
      pll=new Link_list;
      pnn=new Node((char * const)str1,(void *)pll);
      this->insert(pnn);
      cnt_key++;
   }
   if(!(lst->search(str2))){
      lst->add_key_count(str2);
      lst->search(str2);
      pll->add_element(lst->show_str());
   }
   else pll->add_unique(lst->show_str());
}

int BTList::list_size(const char *str){
   Link_list *pll;
   if(!(this->search(str))){
      return(0);
   }
   else {
      pll=(Link_list *)this->give_ptr();
      return(pll->list_size());
   }
}

int BTList::list_size(void){
   Link_list *pll;
   pll=(Link_list *)this->give_ptr();
   return(pll->list_size());
}

syn_lk *BTList::set_ptr(const char *str){
   Link_list *pll;
   if(!(this->search(str))){
      elem=NULL;
   }
   else {
      pll=(Link_list *)this->give_ptr();
      elem=strt;
      elx=pll->el;
      strt->next=elx->next;
   }
   return(elem);
}

syn_lk *BTList::set_ptr(void){
   Link_list *pll;
   pll=(Link_list *)this->give_ptr();
   elem=strt;
   elx=pll->el;
   strt->next=elx->next;
   return(elem);
}
   
char *BTList::next_ptr(void){
   if(elem!=elx){
      elem=elem->next;
      return(elem->syn);
   }
   else return(NULL);
}     

syn_lk *BTList::set_ptr(const char *str,Point &Pt){
   Link_list *pll;
   if(!(this->search(str))){
      Pt.elem=NULL;
   }
   else {
      pll=(Link_list *)this->give_ptr();
      Pt.elem=Pt.strt;
      Pt.elx=pll->el;
      Pt.strt->next=Pt.elx->next;
   }
   return(Pt.elem);
}

syn_lk *BTList::set_ptr(Point &Pt){
   Link_list *pll;
   pll=(Link_list *)this->give_ptr();
   Pt.elem=Pt.strt;
   Pt.elx=pll->el;
   Pt.strt->next=Pt.elx->next;
   return(Pt.elem);
}

char *BTList::next_ptr(Point &Pt){
   if(Pt.elem!=Pt.elx){
      Pt.elem=Pt.elem->next;
      return(Pt.elem->syn);
   }
   else return(NULL);
}

long BTList::exs_pair(const char *qtr,const char *str){
   char *ptr,*utr;

   if(!(lst->search(str)))return(0);
   else ptr=lst->show_str();
   if(!(this->set_ptr(qtr)))return(0);
   while((utr=this->next_ptr())){
      if(utr==ptr)return(1);
   }
   return(0);
}

long BTList::num_pair(const char *qtr,const char *str){
   char *ptr,*utr;
   long n=0;

   if(!(lst->search(str)))return(0);
   else ptr=lst->show_str();
   if(!(this->set_ptr(qtr)))return(0);
   while((utr=this->next_ptr())){
      if(utr==ptr)n++;
   }
   return(n);
}

long BTList::exs_second(const char *str){
   char *ptr,*utr;

   if(!(lst->search(str)))return(0);
   else ptr=lst->show_str();
   if(!(this->set_ptr()))return(0);
   while((utr=this->next_ptr())){
      if(utr==ptr)return(1);
   }
   return(0);
}

long BTList::num_second(const char *str){
   char *ptr,*utr;
   long n=0;

   if(!(lst->search(str)))return(0);
   else ptr=lst->show_str();
   if(!(this->set_ptr()))return(0);
   while((utr=this->next_ptr())){
      if(utr==ptr)n++;
   }
   return(n);
}

void BTList::make_class(ofstream &fout){
   long i=0;
   Count *pct;
   Link_list *pll;
   char *pch,*str,*stn,bnam[max_str],cnam[max_str];
   int flpg=0,fljg;  
   cnam[0]='N';
   this->node_first();
   if(this->node_next()){
      flpg=1;
      pch=this->show_str();
   }
   while(flpg){ //flpg is nonzero while main tree has unprocessed nodes
      this->search(pch); //Find restart point
      pll=(Link_list *)this->give_ptr();
      while(flpg&&(pll->flag)){
         if(this->node_next()){    
            pll=(Link_list *)this->give_ptr();
         }
         else flpg=0;
      }
      if(flpg){
         pll->flag=1; //indicates node is processed
         pch=this->show_str(); //key is remembered as efficient restart
         i++;
         pct=new Count;
         pct->add_count(pch,1);
         if(this->set_ptr()){
            while(str=this->next_ptr()){
               pct->add_count(str,0);
            }
         }
         fljg=1;
         while(fljg){ //fljg>0 while unprocessed nodes in pct tree
            fljg=0;
            pct->node_first();
            while(pct->node_next()){
               if((pct->count())==0){
                  str=pct->show_str(); //Efficient restart point
                  fljg=1;
                  (*(long*)pct->give_ptr())++;
                  if(this->search(str)){
                     pll=(Link_list *)this->give_ptr();
                     if(!(pll->flag)){
                        pll->flag=1; //Main tree node processed
                        this->set_ptr();
                        while(stn=this->next_ptr()){
                           pct->add_count(stn,0);
                        }
                        pct->search(str); //Back to restart point
                     }
                  }
               }
            }
         }
         cnam[1]='\0';
         long_str(bnam,i);
         strcat(cnam,bnam);
         pct->node_first();
         while(pct->node_next()){
            fout << pct->show_str() << "|" << cnam << endl;
         }
         delete pct;
      }
   }
}   

void BTList::make_class(ofstream &fout,char c){
   long i=0;
   Count *pct;
   Link_list *pll;
   char *pch,*str,*stn,bnam[max_str],cnam[max_str];
   int flpg=0,fljg;  
   cnam[0]='N';
   this->node_first();
   if(this->node_next()){
      flpg=1;
      pch=this->show_str();
   }
   while(flpg){
      this->search(pch);
      pll=(Link_list *)this->give_ptr();
      while(flpg&&(pll->flag)){
         if(this->node_next()){    
            pll=(Link_list *)this->give_ptr();
         }
         else flpg=0;
      }
      if(flpg){
         pll->flag=1;
         pch=this->show_str();
         i++;
         pct=new Count;
         pct->add_count(pch,1);
         if(this->set_ptr()){
            while(str=this->next_ptr()){
               pct->add_count(str,0);
            }
         }
         fljg=1;
         while(fljg){
            fljg=0;
            pct->node_first();
            while(pct->node_next()){
               if((pct->count())==0){
                  str=pct->show_str();
                  fljg=1;
                  (*(long*)pct->give_ptr())++;
                  if(this->search(str)){
                     pll=(Link_list *)this->give_ptr();
                     if(!(pll->flag)){
                        pll->flag=1;
                        this->set_ptr();
                        while(stn=this->next_ptr()){
                           pct->add_count(stn,0);
                        }
                        pct->search(str);
                     }
                  }
               }
            }
         }
         cnam[1]='\0';
         long_str(bnam,i);
         strcat(cnam,bnam);
         pct->node_first();
         while(pct->node_next()){
            stn=pct->show_str();
            if(!strchr(stn,c))fout << stn << "|" << cnam << endl;
         }
         delete pct;
      }
   }
}   

void BTList::debug(void){
   Link_list *pll;
   char *pch;
   this->node_first();
   while(this->node_next()){
      pll=(Link_list *)this->give_ptr();
      if(!pll)cout << "data is null!" << endl;
      cout << this->show_str() << "|" << this->list_size() << endl;
      this->set_ptr();
      while(pch=this->next_ptr()){
         cout << "   " << pch << endl;
      }
   }
}


void BTList::load( const char * file_name, const char delimiter ) {

  ifstream file(file_name);
  if ( ! file ) {
    cerr << "BTList::failed to open " << file_name << '\n';
  }
  
  string key, value;
  while ( getline(file, key, delimiter ) ) {
    getline(file,value);
    
    add_element( key.c_str(), value.c_str() );
  }

}

      
}
