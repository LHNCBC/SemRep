#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <cassert>
#include <runn.h>
#include "Bootstrap.h"
using namespace std;
namespace iret {

Bootstrap::Bootstrap(long num_q){
   num_queries = num_q;
   sel=new long[num_queries];
   fx1=new float[num_queries];
   fx2=new float[num_queries];
   
}

Bootstrap::~Bootstrap(){
   if(sel!=NULL) delete [] sel;  
   if(fx1!=NULL)delete [] fx1;
   if(fx2!=NULL)delete [] fx2;
}

void Bootstrap::gopen_write(const char *name,ios_base::openmode nMode){
   char cnam[256];
   get_pathw(cnam,"iretset", name,"r");
   fout.open(cnam,nMode);
}

void Bootstrap::load_1file(const char *fil){
   long i,j,len;

   len=strlen(fil)+6;
   iret1=new char[len];
   strcpy(iret1,fil);
   strcat(iret1,"_col1");
   iret2=new char[len];
   strcpy(iret2,fil);
   strcat(iret2,"_col2");

   ifstream fin;
   fin.open(fil, ios::in);
   if(!fin.is_open()) { cout <<fil<<" failed to open" <<endl; exit(0);}
   i = 0;
   while(fin >> fx1[i] >> fx2[i]){
       i++;
   }
   fin.close();
   if(i != num_queries) { cout <<"wrong # queries in " << fil << endl; exit(0);}
}

void Bootstrap::load_2file(const char *fil1,const char *fil2){
   long i,j,len;

   len=strlen(fil1)+1;
   iret1=new char[len];
   strcpy(iret1,fil1);
   len=strlen(fil2)+1;
   iret2=new char[len];
   strcpy(iret2,fil2);

   ifstream fin;
   fin.open(iret1, ios::in);
   if(!fin.is_open()) { cout <<iret1<<" failed to open" <<endl; exit(0);}
   i = 0;
   while(fin >> fx1[i]){
       i++; 
   }
   fin.close();
   if(i != num_queries) { cout <<"wrong # queries in " << iret1 << endl; exit(0);}
   fin.open(iret2, ios::in);
   if(!fin.is_open()) { cout <<iret2<<" failed to open" <<endl; exit(0);}
   i = 0;
   while(fin >> fx2[i]){
        i++;
   }
   fin.close();
   if(i != num_queries) { cout <<"wrong # queries in " << iret2 << endl; exit(0);}
}

void Bootstrap::print_comp(float sig,long sn){
   int pflag=get_qflag();
   float x;
   float *pt,pa;
   long i,j,seed;
   char cnam[256],bnam[256];
   seed=gseed(0,NULL,"-seed");

   fout << "Confidence limits based on 11-point averge " << endl;
   fout << "          " << iret1 << " and " << iret2 <<  endl;
   fout << "          using a paired sampling technique." << endl;
   
   float *px=new float[sn];
  
   for(i=0;i<sn;i++){
      this->ran_sel();
      px[i] = 0;
      for(j=0;j <num_queries;j++){
        px[i] += fx1[sel[j]] - fx2[sel[j]];
      }
      px[i] = px[i]/num_queries;
      mark(pflag,i+1,100,"iterations");
   }

   pa=0;
   for(i=0;i<sn;i++){
      pa+=*(px+i);
     
   }
   pa=pa/((float)sn);
   
   this->str_sel();
   x = 0;
   
   for(j=0;j <num_queries;j++){
        x += fx1[sel[j]] - fx2[sel[j]];
   }
   
   x = x/num_queries;
   fout << "Significance level " << sig << endl;
   float lw,hg;
   fout <<  "Confidence limits for difference in precisions p1-p2 = " << x << " :" << endl;
   confid(sig,sn,px,lw,hg);
   fout << "          " << lw-pa << " " << hg-pa << endl;
   fout << "Based on " << sn << " paired resamplings of the data." << endl;
   fout << "Random seed used was " << seed << endl << endl;
}

void confid(float sig,long n,float *fx,float &pl,float &ph){
   long i,*ord=new long[n];
   for(i=0;i<n;i++)*(ord+i)=i;
   hSort(n,fx,ord);
   
   i=(long)floor(sig*n/2.0);
   pl=*(fx+i);
   ph=*(fx+n-i-1);
   delete [] ord;
}

void Bootstrap::gclose_write(){
   fout.close();
}

void Bootstrap::ran_sel(void){
   for(long i=0;i<num_queries;i++)*(sel+i)=zrand(num_queries);
}

void Bootstrap::str_sel(void){
   for(long i=0;i<num_queries;i++)*(sel+i)=i;
}

}
