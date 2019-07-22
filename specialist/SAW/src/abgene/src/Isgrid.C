#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "runn.h"
#include "Btree.h"
#include "Istreg.h"
#include "Istgen.h"
#include "Isgrid.h"

using namespace std;
namespace iret {

static double l2=1.0/log(2.0);

Isgrid::Isgrid(void) : FBase("isgrid","null"){
   num=0;
   nums=0;
   ct=0;
   xd=NULL;
   xn=NULL;
   a=NULL;
   s=NULL;
   b=NULL;
   sr=NULL;
   br=NULL;
   rt=NULL;
   p=NULL;
   eps=1.0E-12;
}

Isgrid::Isgrid(const char *nam) : FBase("isgrid",nam){
   num=0;
   nums=0;
   ct=0;
   xd=NULL;
   xn=NULL;
   a=NULL;
   s=NULL;
   b=NULL;
   sr=NULL;
   br=NULL;
   rt=NULL;
   p=NULL;
   eps=1.0E-12;
}

Isgrid::Isgrid(const char *nam,double xeps) : FBase("isgrid",nam){
   num=0;
   nums=0;
   ct=0;
   xd=NULL;
   xn=NULL;
   a=NULL;
   s=NULL;
   b=NULL;
   sr=NULL;
   br=NULL;
   rt=NULL;
   p=NULL;
   eps=xeps;
}

Isgrid::~Isgrid(){
   if(xd!=NULL)delete [] xd;
   if(xn!=NULL)delete [] xn;
   if(a!=NULL)delete [] a;
   if(s!=NULL)delete [] s;
   if(b!=NULL)delete [] b;
   if(sr!=NULL)delete [] sr;
   if(br!=NULL)delete [] br;
   if(rt!=NULL)delete [] rt;
   if(p!=NULL)delete [] p;
}

void Isgrid::set_xdom(double x,double y){
   xb=x;
   xe=y;
}

void Isgrid::set_ydom(double x,double y){
   yb=x;
   ye=y;
}

void Isgrid::set_zdom(double x,double y){
   zb=x;
   ze=y;
}

void Isgrid::set_xgran(long n){
   xsize=n;
   xdel=(xe-xb)/((double)xsize);
}

void Isgrid::set_ygran(long n){
   ysize=n;
   ydel=(ye-yb)/((double)ysize);
}

void Isgrid::set_zgran(long n){
   zsize=n;
   zdel=(ze-zb)/((double)zsize);
}

void Isgrid::init3(void){
   long i;

   ct=0;
   num=xsize*ysize*zsize;
   nums=xsize*ysize;
   
   if(xd!=NULL)delete [] xd;
   if(xn!=NULL)delete [] xn;

   xd=new double[num];
   xn=new double[num];

   for(i=0;i<num;i++)*(xd+i)=*(xn+i)=0;
}

void Isgrid::init2(void){
   long i;

   ct=0;
   num=xsize*ysize;

   if(xd!=NULL)delete [] xd;
   if(xn!=NULL)delete [] xn;
   if(a!=NULL)delete [] a;
   if(s!=NULL)delete [] s;
   if(b!=NULL)delete [] b;
   if(sr!=NULL)delete [] sr;
   if(br!=NULL)delete [] br;
   if(rt!=NULL)delete [] rt;
   
   xd=new double[num];
   xn=new double[num];
   a=new double[num];
   s=new double[num];
   b=new long[num];
   sr=new double[ysize];
   br=new long[ysize];
   rt=new long[ysize];

   for(i=0;i<num;i++)*(xd+i)=*(xn+i)=0;
}

void Isgrid::init1(void){
   long i;

   ct=0;
   num=xsize;

   if(sr!=NULL)delete [] sr;
   if(a!=NULL)delete [] a;
   if(s!=NULL)delete [] s;
   if(xd!=NULL)delete [] xd;
   if(xn!=NULL)delete [] xn;
   
   sr=new double[num];
   a=new double[num];
   s=new double[num];
   xd=new double[num];
   xn=new double[num];

   for(i=0;i<num;i++)*(xd+i)=*(xn+i)=0;
}

void Isgrid::add_data(double x,double y,double z,double sd,double sn){
   long i,j,k;

   i=(long)floor((y-yb)/ydel);
   if(i<0)i=0;
   if(i>=ysize)i=ysize-1;
   j=(long)floor((x-xb)/xdel);
   if(j<0)j=0;
   if(j>=xsize)j=xsize-1;
   k=(long)floor((z-zb)/zdel);
   if(k<0)k=0;
   if(k>=zsize)k=zsize-1;

   i=num-1-k*nums-i*xsize-j;
   (*(xd+i))+=sd;
   (*(xn+i))+=sn;
   ct++;
}

void Isgrid::add_data(double x,double y,double sd,double sn){
   long i,j;

   i=(long)floor((y-yb)/ydel);
   if(i<0)i=0;
   if(i>=ysize)i=ysize-1;
   j=(long)floor((x-xb)/xdel);
   if(j<0)j=0;
   if(j>=xsize)j=xsize-1;

   i=num-1-i*xsize-j;
   (*(xd+i))+=sd;
   (*(xn+i))+=sn;
   ct++;
}

void Isgrid::add_data(double x,double sd,double sn){
   long i,j;

   j=(long)floor((x-xb)/xdel);
   if(j<0)j=0;
   if(j>=xsize)j=xsize-1;

   (*(xd+j))+=sd;
   (*(xn+j))+=sn;
   ct++;
}

void Isgrid::dim1(void){
   long i,n=0,j;

   if(p!=NULL)delete [] p;
   p=new double[num];
   for(i=0;i<num;i++){
      if(*(xd+i)>0){
         *(sr+n)=(double)i;
         *(s+n)=*(xd+i);
         *(a+n)=*(xn+i);
         n++;
      }
   }
   Istreg Ist;
   Ist.set_data(n,sr,s,a);
   Ist.dim1_ord(0);
   for(i=0;i<n;i++){
      j=rnd(*(sr+i));
      *(p+j)=*(Ist.p+i);
   }
}

void Isgrid::dim2(void){
   long i;
   long *idx=new long[num];
   for(i=0;i<num;i++)*(idx+i)=i;
   if(p!=NULL)delete [] p;
   p=new double[num];

   this->split(num,idx);
}

void Isgrid::dim3(void){
   long i,j,k,n,u;
   double *pc;

   if(p!=NULL)delete [] p;
   p=new double[num];
   for(i=0;i<num;i++)*(p+i)=0;

   total_objs=0;
   for(n=0;n<num;n++)if(*(xd+n)>0)total_objs++;
   xc=new long[total_objs];
   yc=new long[total_objs];
   zc=new long[total_objs];
   xdc=new double[total_objs];
   xnc=new double[total_objs];
   total_objs=0;
   for(n=0;n<num;n++){
      if(*(xd+n)>0){
         u=num-1-n;
         k=u/nums;
         u-=k*nums;
         i=u/xsize;
         j=u-i*xsize;
         *(xc+total_objs)=j;
         *(yc+total_objs)=i;
         *(zc+total_objs)=k;
         *(xdc+total_objs)=*(xd+n);
         *(xnc+total_objs)=*(xn+n);
         total_objs++;
      }
   }
   Istgen Isg(1.0e-8);
   Isg.set_data(total_objs,xdc,xnc);
   Isg.gen(ycmp);

   total_objs=0;
   for(n=0;n<num;n++){
      if(*(xd+n)>0){
         u=num-1-*(zc+total_objs)*nums-*(yc+total_objs)*xsize-*(xc+total_objs);
         if(u!=n){cout << "Error in matching!" << endl; exit(0);}
         *(p+n)=*(Isg.p+total_objs);
         total_objs++;
      }
   }
   delete [] xc;
   delete [] yc;
   delete [] zc;
   delete [] xdc;
   delete [] xnc;
}

void Isgrid::split(long npp,long *idx){
   double sum_n=0,sum_d=0,pav;
   double suma,sbest=0;
   long i,j,k,id,kp,ib,jb,kb,kbest=-1;
   long nrows,iz,jz,kz,dm;

   for(i=0;i<npp;i++){
      j=*(idx+i);
      sum_n+=*(xn+j);
      sum_d+=*(xd+j);
   }
   //If zero total weight then trivial.
   if(sum_d==0){
      delete [] idx;
      return;
   }

   pav=sum_n/sum_d;

   //If there is only one element solution is trivial.
   if(npp==1){
      *(p+*(idx+0))=pav;
      delete [] idx;
      return;
   }

   //If the value is extreme the solution is trivial.
   if(sum_n==0){
      for(i=0;i<npp;i++)*(p+*(idx+i))=0.0;
      delete [] idx;
      return;
   }
   else if(sum_n==sum_d){
      for(i=0;i<npp;i++)*(p+*(idx+i))=1.0;
      delete [] idx;
      return;
   }

   //Initialize values.
   for(i=0;i<npp;i++){
      j=*(idx+i);
      *(a+i)=*(xn+j)-*(xd+j)*pav;
   }

   //Enter loop to optimize an upper set
   //Process first row
   nrows=1;
   id=*idx;
   k=id/xsize;
   iz=ysize-1-k;
   rt[iz]=0;
   i=1;
   while((i<npp)&&(k==(*(idx+i)/xsize)))i++;
   suma=0;
   for(j=0;j<i;j++){
      suma+=*(a+j);
      *(s+j)=suma;
   }
   *(b+i-1)=kp=i-1;
   for(j=i-2;j>=0;j--){
      if(suma>=*(s+j))*(b+j)=kp;
      else {
         suma=*(s+j);
         kp=j;
         *(b+j)=j;
      }
   }
   if(suma>0){
      *(br+iz)=kp;
      *(sr+iz)=0;
      sbest=suma;
      kbest=kp;
   }
   else {
      *(br+iz)=-1;
      *(sr+iz)=0;
   }
   kz=0;
   jz=xsize-1-id%xsize;
   //Process all subsequent rows
   while(i<npp){
      nrows++;
      kb=i;
      id=*(idx+kb);
      k=id/xsize;
      ib=ysize-1-k;
      rt[ib]=kb;
      jb=xsize-1-id%xsize;
      while((i<npp)&&(k==(*(idx+i)/xsize)))i++;
      //Set local sums
      suma=0;
      for(j=kb;j<i;j++){
         suma+=*(a+j);
         *(s+j)=suma;
      }
      //Reset sums as upper sums 
      if(*(br+iz)==-1)*(sr+ib)=*(sr+iz);
      else *(sr+ib)=*(s+*(br+iz));
      if(jb>jz){
         if(*(br+iz)==-1){
            for(j=kb;j<kb+jb-jz;j++)*(s+j)+=*(sr+iz);
         }
         else {
            for(j=kb;j<kb+jb-jz;j++)*(s+j)+=*(s+*(br+iz));
         }
      }
      dm=kb+jb-jz;
      for(j=dm;j<i;j++)*(s+j)+=*(s+*(b+kz+j-dm));       
      //Set the b's
      *(b+i-1)=kp=i-1;
      suma=*(s+i-1);
      for(j=i-2;j>=kb;j--){
         if(suma>=*(s+j))*(b+j)=kp;
         else {
            suma=*(s+j);
            kp=j;
            *(b+j)=j;
         }
      }
      if(suma>*(sr+ib)){
         *(br+ib)=kp;
         sbest=suma;
         kbest=kp;
      }
      else *(br+ib)=-1;
      kz=kb;
      iz=ib;
      jz=jb;
   }
   //If zero max, terminate process.
   if((kbest==-1)||(sbest<eps)){
      for(i=0;i<npp;i++)*(p+*(idx+i))=pav;
      delete [] idx;
      return;
   }
   //Trace back, mark, and count   
   long nm1=0,nm2=0,kf,ibest,jbest;
   id=*(idx+kbest);
   kf=id/xsize;
   ibest=ysize-1-kf;
   jbest=xsize-1-id%xsize;
   i=kb=npp-1;
   id=*(idx+i);
   k=id/xsize;
   if(k>kf)br[ysize-1-k]=-1; //Process up to best row
   while(k>kf){
      while((i>=0)&&(k==(*(idx+i)/xsize))){i--;nm2++;}
      k=*(idx+i)/xsize;
      if(k>kf)br[ysize-1-k]=-1;
   }
   br[ibest]=kbest; //Process the best row
   while((i>=0)&&(kf==(*(idx+i)/xsize))){
      if(i>kbest)nm2++;
      else nm1++;
      i--;
   }
   while(i>=0){
      id=*(idx+i);
      k=id/xsize;
      ib=ysize-1-k;
      jb=xsize-1-id%xsize+i-rt[ib];
      if((jbest==-1)||(jbest>jb))kp=br[ib]; 
      else kp=*(b+rt[ib]+jb-jbest);
      if(kp==-1){
         while((i>=0)&&(k==(*(idx+i)/xsize))){
            nm2++;
            i--;
         }
         jbest=-1;
      }
      else {
         while((i>=0)&&(k==(*(idx+i)/xsize))){
            if(i>kp)nm2++;
            else nm1++;
            i--;
         }
         jbest=xsize-1-*(idx+kp)%xsize;
      }
      br[ib]=kp;
   }
   cout << nm2 << " " << nm1 << " " << npp << endl;

   //If zero split, terminate process.
   if(nm1==0 || nm2==0){
      for(i=0;i<npp;i++)*(p+*(idx+i))=pav;
      delete [] idx;
      return;
   }

   long *id1=new long[nm1];
   long *id2=new long[nm2];
   long nn1=0,nn2=0;
   //Split the data
   i=kb=0;
   while(i<npp){
      id=*(idx+kb);
      k=id/xsize;
      ib=ysize-1-k;
      kp=br[ib];
      if(kp==-1){
         while((i<npp)&&(k==(*(idx+i)/xsize))){
            *(id2+(nn2++))=*(idx+(i++));
         }
         kb=i;
      }
      else {
         while((i<npp)&&(k==(*(idx+i)/xsize))){
            if(i>kp)*(id2+(nn2++))=*(idx+(i++));
            else *(id1+(nn1++))=*(idx+(i++));
         }
         kb=i;
      }
   }

   //Free memory.
   delete [] idx;

   //Make recursive calls
   this->split(nm1,id1);
   this->split(nm2,id2);
}

double Isgrid::avg(){
   long i;
   double sum_n=0,sum_d=0,pav;

   for(i=0;i<num;i++){
      sum_n+=*(xn+i);
      sum_d+=*(xd+i);
   }
   if(sum_d>0)pav=sum_n/sum_d;
   else pav=0;
   
   return(pav);
}

double Isgrid::info(){
   long i;
   double sum=0,z;
   double pav=this->avg();

   for(i=0;i<num;i++){
      if((z=*(p+i))>0){
         sum+=*(xd+i)*z*l2*log(z/pav);
      }
   }
   
   return(sum);
}

double Isgrid::sigma(){
   long i;
   double sum=0,sum_d=0,xx,yy;
   double pav=this->avg(),z;

   for(i=0;i<num;i++){
      xx=*(xn+i);
      yy=*(xd+i);
      if(yy>0){
         sum_d+=yy;
         z=xx/yy-pav;
         sum+=yy*z*z;
      }
   }
   return(sqrt(sum/sum_d));
}

double Isgrid::rms(){
   long i;
   double sum=0,sum_d=0,xx,yy,z;

   for(i=0;i<num;i++){
      xx=*(xn+i);
      yy=*(xd+i);
      if(yy>0){
         sum_d+=yy;
         z=xx/yy-*(p+i);
         sum+=yy*z*z;
      }
   }
   return(sqrt(sum/sum_d));
}

void Isgrid::write_3df_data(void){
   long i,j,k,n,nt;
   int pflag=get_qflag();
  
   nt=0;
   for(i=0;i<num;i++)if(*(xd+i)>0)nt++;

   ofstream *pfout=get_Ostr("3d_data");

   *pfout << nt << endl;
   for(i=0;i<ysize;i++){
      for(j=0;j<xsize;j++){
         for(k=0;k<zsize;k++){
            n=num-1-k*nums-i*xsize-j;
            if(*(xd+n)>0)*pfout << j << " " << i << " " << k << " " << *(xd+n) << " " << *(xn+n) << endl;
         }
      }
   }
   dst_Ostr(pfout);
   cout << nt << " points" << endl;
}

void Isgrid::read_3df_data(void){
   long i,j,k,n,nt,u;

   num=xsize*ysize*zsize;
   nums=xsize*ysize;

   if(xd!=NULL)delete [] xd;
   xd=new double[num];
   for(i=0;i<num;i++)*(xd+i)=0;

   if(p!=NULL)delete [] p;
   p=new double[num];

   ifstream *pfin=get_Istr("3d_data");
   *pfin >> nt;
   for(n=0;n<nt;n++){
      *pfin >> j >> i >> k;
      u=num-1-k*nums-i*xsize-j;
      *pfin >> *(p+u);
      *(xd+u)=1.0;
   }
   dst_Istr(pfin);
}

void Isgrid::write_3df(void){
   long i,j,k,n;
   int pflag=get_qflag();

   ofstream *pfout=get_Ostr("3d");
   *pfout << xsize << endl;
   *pfout << xb << " " << xe << " " << xdel << endl;
   *pfout << ysize << endl;
   *pfout << yb << " " << ye << " " << ydel << endl;
   *pfout << zsize << endl;
   *pfout << zb << " " << ze << " " << zdel << endl;

   for(i=0;i<ysize;i++){
      for(j=0;j<xsize;j++){
         for(k=0;k<zsize;k++){
            n=num-1-k*nums-i*xsize-j;
            if(*(xd+n)>0)*pfout << n << " " << *(p+n) << endl;
         }
      }
   }
   dst_Ostr(pfout);
   cout << xsize << " by " << ysize << " by " << zsize << " grid of points" << endl;
}

void Isgrid::write_2df(void){
   long i,j,k;
   int pflag=get_qflag();

   ofstream *pfout=get_Ostr("2d");
   *pfout << xsize << endl;
   *pfout << xb << " " << xe << " " << xdel << endl;
   *pfout << ysize << endl;
   *pfout << yb << " " << ye << " " << ydel << endl;

   for(i=0;i<xsize;i++){
      for(j=0;j<ysize;j++){
         k=num-1-j*xsize-i;
         if(*(xd+k)>0)*pfout << k << " " << *(p+k) << endl;
      }
   }
   dst_Ostr(pfout);
   cout << xsize << " by " << ysize << " grid of points" << endl;
}

void Isgrid::write_1df(void){
   long i,j,k;
   int pflag=get_qflag();

   ofstream *pfout=get_Ostr("1d");
   *pfout << xsize << endl;
   *pfout << xb << " " << xe << " " << xdel << endl;

   for(i=0;i<num;i++){
      if(*(xd+i)>0)*pfout << i << " " << *(p+i) << endl;
   }
   dst_Ostr(pfout);
   cout << num << " points" << endl;
}

void Isgrid::read_3df(void){
   long i,j,k;

   ifstream *pfin=get_Istr("3d");
   *pfin >> xsize;
   *pfin >> xb >> xe >> xdel;
   *pfin >> ysize;
   *pfin >> yb >> ye >> ydel;
   *pfin >> zsize;
   *pfin >> zb >> ze >> zdel;
   num=xsize*ysize*zsize;
   nums=xsize*ysize;

   if(xd!=NULL)delete [] xd;
   xd=new double[num];
   for(i=0;i<num;i++)*(xd+i)=0;

   if(p!=NULL)delete [] p;
   p=new double[num];

   while(*pfin >> k){
      *pfin >> *(p+k);
      *(xd+k)=1.0;
   }
   dst_Istr(pfin);
}

void Isgrid::read_2df(void){
   long i,j,k;

   ifstream *pfin=get_Istr("2d");
   *pfin >> xsize;
   *pfin >> xb >> xe >> xdel;
   *pfin >> ysize;
   *pfin >> yb >> ye >> ydel;
   num=xsize*ysize;

   if(xd!=NULL)delete [] xd;
   xd=new double[num];
   for(i=0;i<num;i++)*(xd+i)=0;

   if(p!=NULL)delete [] p;
   p=new double[num];

   while(*pfin >> k){
      *pfin >> *(p+k);
      *(xd+k)=1.0;
   }
   dst_Istr(pfin);
}

void Isgrid::read_1df(void){
   long i,j,k;

   ifstream *pfin=get_Istr("1d");
   *pfin >> xsize;
   *pfin >> xb >> xe >> xdel;
   num=xsize;

   if(xd!=NULL)delete [] xd;
   xd=new double[num];
   for(i=0;i<num;i++)*(xd+i)=0;

   if(p!=NULL)delete [] p;
   p=new double[num];

   while(*pfin >> k){
      *pfin >> *(p+k);
      *(xd+k)=1.0;
   }
   dst_Istr(pfin);
}

double Isgrid::val_3df(double x,double y,double z){
   long i,j,k,n;

   i=(long)floor((y-yb)/ydel);
   if(i<0)i=0;
   if(i>=ysize)i=ysize-1;
   j=(long)floor((x-xb)/xdel);
   if(j<0)j=0;
   if(j>=xsize)j=xsize-1;
   k=(long)floor((z-zb)/zdel);
   if(k<0)k=0;
   if(k>=zsize)k=zsize-1;

   n=num-1-k*nums-i*xsize-j;
   return(*(p+n));
}

double Isgrid::val_2df(double x,double y){
   long i,j,k;

   i=(long)floor((y-yb)/ydel);
   if(i<0)i=0;
   if(i>=ysize)i=ysize-1;
   j=(long)floor((x-xb)/xdel);
   if(j<0)j=0;
   if(j>=xsize)j=xsize-1;

   k=num-1-i*xsize-j;
   return(*(p+k));
}

double Isgrid::val_1df(double x){
   long i,j,k;

   j=(long)floor((x-xb)/xdel);
   if(j<0)j=0;
   if(j>=xsize)j=xsize-1;

   return(*(p+j));
}

void Isgrid::extend_3df(void){
   long i,j,k,id,idp,n;
   double pmax=0.0,pmin=1.0;
   double xx,yy,zz,*q;

   for(k=0;k<num;k++){
      if(*(xd+k)>0){
         xx=*(p+k);
         pmax=(pmax<xx)?xx:pmax;
         pmin=(pmin>xx)?xx:pmin;
      }
   }
   q=new double[num];
   for(k=0;k<num;k++)*(q+k)=*(p+k);

   //Compute upper bounds
   if(*(xd)==0)*(p)=pmax;
   for(j=xsize-2;j>=0;j--){
      id=xsize-1-j;
      if(*(xd+id)==0)*(p+id)=*(p+id-1);
   }
   for(i=ysize-2;i>=0;i--){
      id=(ysize-1-i)*xsize;
      if(*(xd+id)==0)*(p+id)=*(p+id-xsize);
   }
   for(k=zsize-2;k>=0;k--){
      id=(zsize-1-k)*nums;
      if(*(xd+id)==0)*(p+id)=*(p+id-nums);
   }
   for(i=ysize-2;i>=0;i--){
      for(j=xsize-2;j>=0;j--){
         id=nums-1-i*xsize-j;
         if(*(xd+id)==0){
            xx=*(p+id-1);
            yy=*(p+id-xsize);
            *(p+id)=(xx<yy)?xx:yy;
         }
      }
   }
   for(k=zsize-2;k>=0;k--){
      for(j=xsize-2;j>=0;j--){
         id=(zsize-1-k)*nums+xsize-1-j;
         if(*(xd+id)==0){
            xx=*(p+id-1);
            zz=*(p+id-nums);
            *(p+id)=(xx<zz)?xx:zz;
         }
      }
   }
   for(k=zsize-2;k>=0;k--){
      for(i=ysize-2;i>=0;i--){
         id=(zsize-k)*nums-(i+1)*xsize;
         if(*(xd+id)==0){
            zz=*(p+id-nums);
            yy=*(p+id-xsize);
            *(p+id)=(zz<yy)?zz:yy;
         }
      }
   }
   for(i=ysize-2;i>=0;i--){
      for(j=xsize-2;j>=0;j--){
         for(k=zsize-2;k>=0;k--){
            id=num-1-k*nums-i*xsize-j;
            if(*(xd+id)==0){
               xx=*(p+id-1);
               yy=*(p+id-xsize);
               zz=*(p+id-nums);
               zz=(xx<zz)?xx:zz;
               *(p+id)=(zz<yy)?zz:yy;
            }
         }
      }
   }
   //Compute lower bounds
   if(*(xd+num-1)==0)*(q+num-1)=pmin;
   for(j=1;j<xsize;j++){
      id=num-1-j;
      if(*(xd+id)==0)*(q+id)=*(q+id+1);
   }
   for(i=1;i<ysize;i++){
      id=num-1-i*xsize;
      if(*(xd+id)==0)*(q+id)=*(q+id+xsize);
   }
   for(k=1;k<zsize;k++){
      id=num-1-k*nums;
      if(*(xd+id)==0)*(q+id)=*(q+id+nums);
   }
   for(i=1;i<ysize;i++){
      for(j=1;j<xsize;j++){
         id=num-1-i*xsize-j;
         if(*(xd+id)==0){
            xx=*(q+id+1);
            yy=*(q+id+xsize);
            *(q+id)=(xx<yy)?yy:xx;
         }
      }
   }
   for(k=1;k<zsize;k++){
      for(j=1;j<xsize;j++){
         id=num-1-k*nums-j;
         if(*(xd+id)==0){
            zz=*(q+id+nums);
            yy=*(q+id+1);
            *(q+id)=(zz<yy)?yy:zz;
         }
      }
   }
   for(k=1;k<zsize;k++){
      for(i=1;i<ysize;i++){
         id=num-1-k*nums-i*xsize;
         if(*(xd+id)==0){
            zz=*(q+id+nums);
            yy=*(q+id+xsize);
            *(q+id)=(zz<yy)?yy:zz;
         }
      }
   }
   for(i=1;i<ysize;i++){
      for(j=1;j<xsize;j++){
         for(k=1;k<zsize;k++){
            id=num-1-k*nums-i*xsize-j;
            if(*(xd+id)==0){
               xx=*(q+id+1);
               yy=*(q+id+xsize);
               zz=*(q+id+nums);
               zz=(xx<zz)?zz:xx;
               *(q+id)=(zz<yy)?yy:zz;
            }
         }
      }
   }

   //Average values.
   for(k=0;k<num;k++)*(p+k)=0.5*(*(p+k)+*(q+k));
   delete [] q;
}

void Isgrid::extend_2df(void){
   long i,j,k,id,idp,n;
   double pmax=0.0,pmin=1.0;
   double xx,yy,*q;

   for(k=0;k<num;k++){
      if(*(xd+k)>0){
         xx=*(p+k);
         pmax=(pmax<xx)?xx:pmax;
         pmin=(pmin>xx)?xx:pmin;
      }
   }
   q=new double[num];
   for(k=0;k<num;k++)*(q+k)=*(p+k);

   //Compute upper bounds
   if(*(xd)==0)*(p)=pmax;
   for(j=xsize-2;j>=0;j--){
      id=xsize-1-j;
      if(*(xd+id)==0)*(p+id)=*(p+id-1);
      else if(*(p+id)>*(p+id-1)){
         cout << *(p+id) << " > " << *(p+id-1) << endl;
         exit(0);
      }
   }
   for(i=ysize-2;i>=0;i--){
      id=(ysize-1-i)*xsize;
      if(*(xd+id)==0)*(p+id)=*(p+id-xsize);
      else if(*(p+id)>*(p+id-xsize)){
         cout << *(p+id) << " > " << *(p+id-xsize) << endl;
         exit(0);
      }
   }
   for(i=ysize-2;i>=0;i--){
      for(j=xsize-2;j>=0;j--){
         id=num-1-i*xsize-j;
         if(*(xd+id)==0){
            xx=*(p+id-1);
            yy=*(p+id-xsize);
            *(p+id)=(xx<yy)?xx:yy;
         }
         else {
            if(*(p+id)>*(p+id-1)){
               cout << *(p+id) << " > " << *(p+id-1) << endl;
               exit(0);
            }
            if(*(p+id)>*(p+id-xsize)){
               cout << *(p+id) << " > " << *(p+id-xsize) << endl;
               exit(0);
            }
         }
      }
   }
   //Compute lower bounds
   if(*(xd+num-1)==0)*(q+num-1)=pmin;
   for(j=1;j<xsize;j++){
      id=num-1-j;
      if(*(xd+id)==0)*(q+id)=*(q+id+1);
      else if(*(q+id)<*(q+id+1)){
         cout << *(q+id) << " < " << *(q+id+1) << endl;
         exit(0);
      }
   }
   for(i=1;i<ysize;i++){
      id=(ysize-i)*xsize-1;
      if(*(xd+id)==0)*(q+id)=*(q+id+xsize);
      else if(*(q+id)<*(q+id+xsize)){
         cout << *(q+id) << " < " << *(q+id+xsize) << endl;
         exit(0);
      }
   }
   for(i=1;i<ysize;i++){
      for(j=1;j<xsize;j++){
         id=num-1-i*xsize-j;
         if(*(xd+id)==0){
            xx=*(q+id+1);
            yy=*(q+id+xsize);
            *(q+id)=(xx<yy)?yy:xx;
         }
         else {
            if(*(q+id)<*(q+id+1)){
               cout << *(q+id) << " < " << *(q+id+1) << endl;
               exit(0);
            }
            if(*(q+id)<*(q+id+xsize)){
               cout << *(q+id) << " < " << *(q+id+xsize) << endl;
               exit(0);
            }
         }
      }
   }
   //Average values.
   for(k=0;k<num;k++)*(p+k)=0.5*(*(p+k)+*(q+k));
   delete [] q;
}

void Isgrid::extend_1df(void){
   long i,j,k,id,idp,n;
   double pmax=0.0,pmin=1.0;
   double xx,yy,*q;

   for(k=0;k<num;k++){
      if(*(xd+k)>0){
         xx=*(p+k);
         pmax=(pmax<xx)?xx:pmax;
         pmin=(pmin>xx)?xx:pmin;
      }
   }
   q=new double[num];
   for(k=0;k<num;k++)*(q+k)=*(p+k);

   //Compute lower bounds
   if(*(xd)==0)*(p)=pmin;
   for(j=1;j<num;j++){
      if(*(xd+j)==0)*(p+j)=*(p+j-1);
      else if(*(p+j)<*(p+j-1)){
         cout << *(p+j) << " < " << *(p+j-1) << endl;
         exit(0);
      }
   }
   //Compute upper bounds
   if(*(xd+num-1)==0)*(q+num-1)=pmax;
   for(j=num-2;j>=0;j--){
      if(*(xd+j)==0)*(q+j)=*(q+j+1);
      else if(*(q+j)>*(q+j+1)){
         cout << *(q+j) << " > " << *(q+j+1) << endl;
         exit(0);
      }
   }
   //Average values.
   for(k=0;k<num;k++)*(p+k)=0.5*(*(p+k)+*(q+k));
   delete [] q;
}

int ycmp(const long &su, const long &tu)
{
  if(xc[su]<xc[tu])
    {
      if((yc[su]<=yc[tu])&&(zc[su]<=zc[tu])) return(1);
      else return(2);
    }
  else if(xc[su]>xc[tu])
    {
      if((yc[su]>=yc[tu])&&(zc[su]>=zc[tu])) return(-1);
      else return(2);
    }
  else
    {
      if(yc[su]<yc[tu]){
         if(zc[su]<=zc[tu]) return(1);
         else return(2);
      }
      else if(yc[su]>yc[tu]){
         if(zc[su]>=zc[tu]) return(-1);
         else return(2);
      }
      else if(zc[su]<=zc[tu]) return(1);
      else return(-1);
    }
}

}
