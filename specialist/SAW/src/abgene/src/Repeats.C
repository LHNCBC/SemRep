#include "Btree.h"
#include "Thes.h"
#include "Repeats.h"

using namespace std;
namespace iret {

Repeats::Repeats(){
   pct=new Count;
}

Repeats::~Repeats(){
   if(pct!=NULL)delete pct;
}

void Repeats::add_phrase(char *rtr,long n){
   long ui;

   ui=0;
   while(*(rtr+ui)!='\0'){
      pct->add_count(rtr+ui,n);
      ui++;
      while((*(rtr+ui)!=' ')&&(*(rtr+ui)!='\0'))ui++;
      if(*(rtr+ui)==' ')ui++;
   }
}

void Repeats::process2(List *lst){
   long ui,len,m,n;
   char *utr,ch;

   pct->node_first();
   if(pct->node_next()){
      utr=pct->show_str();

      if((m=pct->count())>1)lst->add_key(utr);
      while(pct->node_next()){
         char *rtr=pct->show_str();

         if((n=pct->count())>1)lst->add_key(rtr);
         if((m==1)&&(n==1)){
            len=strlen(utr);
            ui=strlen(rtr);
            len=(len<ui)?len:ui;
            ui=0;
            while((ui<len)&&(*(utr+ui)==*(rtr+ui)))ui++;
            if( (0 < ui && ui<len) || // part of utr matched
              ( (ui == len)  && (*(rtr+ui) != ' ')) // utr ended middle of rtr word
              ) {
               --ui;
               while((0<ui)&&(*(utr+ui)!=' '))ui--;
            }

            if(ui>0){
               ch=*(utr+ui);
               *(utr+ui)='\0';
               lst->add_key(utr);
               *(utr+ui)=ch;
            }
         }
         utr=rtr;
         m=n;
      }
   }
}

void Repeats::process(List *lst,int N){
   int flag;
   long ui,len,m,n;
   long sum,n1,n2,*nm;
   char *utr,*rtr,**rep,ch;

   rep=(char **)new long[N];
   nm =new long[N];
   n1=0;
   n2=-1;
   sum=0;

   pct->node_first();
   flag=1;
   while((sum<N)&&flag){
      if(flag=pct->node_next()){
         n2++;
         *(rep+n2)=pct->show_str();
         sum+=nm[n2]=pct->count();
      }
   }

   while(flag){
      while(sum-nm[n1]>N){
         sum-=nm[n1];
         n1=(++n1)%N;
      }
      
      if(n1==n2)lst->add_key(*(rep+n2));
      else {
         utr=*(rep+n1);
         rtr=*(rep+n2);
         len=strlen(utr);
         ui=strlen(rtr);
         len=(len<ui)?len:ui;
         ui=0;
         while((ui<len)&&(*(utr+ui)==*(rtr+ui)))ui++;
         if( (0 < ui && ui<len) || // part of utr matched
           ( (ui == len)  && (*(rtr+ui) != ' ')) // utr ended middle of rtr word
           ) {
            --ui;
            while((0<ui)&&(*(utr+ui)!=' '))ui--;
         }

         if(ui>0){
            ch=*(utr+ui);
            *(utr+ui)='\0';
            lst->add_key(utr);
            *(utr+ui)=ch;
         }
      }
      sum-=nm[n1];
      n1=(++n1)%N;
      while((sum<N)&&flag){
         if(flag=pct->node_next()){
            n2=(++n2)%N;
            *(rep+n2)=pct->show_str();
            sum+=nm[n2]=pct->count();
         }
      }
   }
}

//Inflection class

Inflect::Inflect(Word *pW,long phr_spc){
   pWrd=pW;
   phrase_space=phr_spc;
   num=0;
   stm=(char**)new long[phrase_space];
   reg=(char**)new long[phrase_space];
}

Inflect::~Inflect(void){
   delete [] stm;
   delete [] reg;
}

void Inflect::add_text(long ln,char *txt){
   long k,i;
   pWrd->punct(ln,txt);
   pWrd->convert(txt,ln);
   pWrd->phrase();
   k=pWrd->cnt;
   if(num+k>phrase_space){cout << "Error, insufficient phrase space!" << endl;exit(0);}
   for(i=0;i<k;i++)reg[num+i]=pWrd->list[i];
   pWrd->phrase_stem();
   for(i=0;i<k;i++)stm[num+i]=pWrd->list[i];
   num+=k;
}

void Inflect::proc_write(ofstream &fout){
   long i,j,k,u,m,v,len;
   char *ptr,*qtr,*utr,ch;

   Repeats Rps;
   for(i=0;i<num;i++)Rps.add_phrase(stm[i],1);
   Partial_match Pmt;
   Rps.process2((List *)&Pmt);
   BTList Btl;
   for(i=0;i<num;i++){
      List Lst;
      Pmt.all_match(stm[i],Lst);
      Lst.node_first();
      while(Lst.node_next()){
         ptr=Lst.show_str();
         qtr=stm[i];
         len=strlen(ptr);
         k=0;
         while(strncmp(qtr,ptr,len)||(((ch=qtr[len])!=' ')&&(ch))){
            u=0;
            while(qtr[u]!=' ')u++;
            k++; //Count number of spaces before the match
            qtr=qtr+u+1;
         }
         m=0;u=0;
         while(u<len){ 
            if(*(ptr+u)==' ')m++; //Count number of spaces in the match
            u++;
         }
         utr=reg[i];
         v=0;u=0;
         while(v<k){ //Find beginning of pattern in reg
            while(utr[u]!=' ')u++;
            u++;
            v++;
         }
         qtr=utr+u;
         v=0;u=0;
         while(v<m){ //Find end of pattern in reg
            while(qtr[u]!=' ')u++;
            u++;
            v++;
         }
         while((qtr[u]!=' ')&&(qtr[u]!='\0'))u++; //Pass the last word
         ch=qtr[u];
         qtr[u]='\0';
         Btl.add_unique(ptr,qtr);
         qtr[u]=ch;
      }
      delete [] stm[i];
      delete [] reg[i];
   }
   Btl.node_first();
   while(Btl.node_next()){
      Btl.set_ptr();
      if(ptr=Btl.next_ptr()){
          while(qtr=Btl.next_ptr()){
             if(strcmp(ptr,qtr)){
                fout << ptr << '|' << qtr << endl;
             }
             ptr=qtr;
          }
      }
   }
   num=0;
}

}
