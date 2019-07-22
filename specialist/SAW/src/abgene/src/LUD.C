#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include "LUD.h"
#include "runn.h"

LUDecomp::~LUDecomp(){
   delete []ord;
   for(long i=0;i<num;i++)delete [](*(mat+i));
   delete []mat;
}

LUDecomp::LUDecomp(long n,double **dat){
   long i,j,k,ip;
   double *dt1,*dt2,xx,yy,zz;
   par=0;
   num=n;
   mat=(double**)new long[num];
   for(i=0;i<num;i++)*(mat+i)=new double[num];
   //Copy data
   for(i=0;i<num;i++){
      dt1=*(mat+i);
      dt2=*(dat+i);
      for(j=0;j<num;j++){
         *(dt1+j)=*(dt2+j);
      }
   }

   ord=new long[num];
   double *rrmx=new double[num];

   //Initialize ord and find row maxima
   for(i=0;i<num;i++){
      *(ord+i)=i;
      xx=0;
      dt1=*(mat+i);
      for(j=0;j<num;j++){
         yy=fabs(*(dt1+j));
         xx=(xx<yy)?yy:xx;
      }
      *(rrmx+i)=xx;
      if(xx==0){cout << "Error, matrix singular!" << endl; exit(0);}
   }
   
   //Column by column construction
   for(j=0;j<num;j++){
      //First j elements of the column
      for(i=0;i<j;i++){
         dt1=*(mat+i);
         xx=*(dt1+j);
         for(k=0;k<i;k++)xx-=*(dt1+k)*(*(*(mat+k)+j));
         *(dt1+j)=xx;
      }
      //Find maximal beta for pivoting at diagonal element j-1j-1
      xx=0;
      ip=j;
      for(i=j;i<num;i++){
         dt1=*(mat+i);
         yy=*(dt1+j);
         for(k=0;k<j;k++)yy-=*(dt1+k)*(*(*(mat+k)+j));
         yy=fabs(yy)/(*(rrmx+i));
         if(xx<yy){
            ip=i;
            xx=yy;
         }
      }
      if(xx==0){cout << "Error, matrix singular!" << endl; exit(0);}
      //Interchange rows as necessary
      if(j!=ip){
         k=*(ord+j);
         *(ord+j)=*(ord+ip);
         *(ord+ip)=k;
         xx=*(rrmx+j);
         zz=*(rrmx+ip);
         *(rrmx+j)=zz;
         *(rrmx+ip)=xx;
         dt1=*(mat+j);
         dt2=*(mat+ip);
         par++;
         for(i=0;i<num;i++){
            xx=*(dt1+i);
            *(dt1+i)=*(dt2+i);
            *(dt2+i)=xx;
         }
       }
       //Compute the pivot beta      
       dt1=*(mat+j);
       yy=*(dt1+j);
       for(k=0;k<j;k++)yy-=*(dt1+k)*(*(*(mat+k)+j));
       zz=*(dt1+j)=yy;
       //Compute the alpha's
       for(i=j+1;i<num;i++){
          dt1=*(mat+i);
          yy=*(dt1+j);
          for(k=0;k<j;k++)yy-=*(dt1+k)*(*(*(mat+k)+j));
          *(dt1+j)=yy/zz;
       }
    }
    delete []rrmx;
    if(par%2==0)par=1;
    else par=-1;
}
       
void LUDecomp::solve(double *b,double *x){
   long i,j,k;
   double xx,yy,zz,*b1,*y;
   double *dt1;

   y=new double[num];
   b1=new double[num];
   for(i=0;i<num;i++)*(b1+i)=*(b+*(ord+i));
   
   //Forward substitution
   for(i=0;i<num;i++){
     xx=*(b1+i);
     dt1=*(mat+i);
     for(j=0;j<i;j++)xx-=*(dt1+j)*(*(y+j));
     *(y+i)=xx;
   }
   //Backward substitution
   for(i=num-1;i>=0;i--){
      xx=*(y+i);
      dt1=*(mat+i);
      for(j=i+1;j<num;j++)xx-=*(dt1+j)*(*(x+j));
      *(x+i)=xx/(*(dt1+i));
   }

   //Free memory
   delete []y;
   delete []b1;
}

void LUDecomp::debug(void){
   long i,j;
   
   cout << "dimension " << num << endl;

   for(i=0;i<num;i++){
      for(j=0;j<num;j++){
         cout << *(*(mat+i)+j) << " \t";
      }
      cout << endl;
   }
   cout << endl << "Row permutations" << endl;
   for(i=0;i<num;i++)cout << *(ord+i) << ", " ;
   cout << endl;
}

void LUDecomp::value(double **dat,double *x,double *y){
   long i,j;
   double xx,*dt1;

   for(i=0;i<num;i++){
      xx=0;
      dt1=*(dat+i);
      for(j=0;j<num;j++)xx+=*(dt1+j)*(*(x+j));
      *(y+i)=xx;
   }
}

double LUDecomp::det(void){
   double prod=1.0;
   long i;
   for(i=0;i<num;i++)prod*=mat[i][i];
   return(par*prod);
}
