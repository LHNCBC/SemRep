#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include "runn.h"
#include "Istgen.h"
using namespace std;
namespace iret {

#define LNEG -1.0E299;
int pflag=get_qflag();

Istgen::Istgen(double xeps){
   total_objs=0;
   xd=NULL;
   xn=NULL;
   p=NULL;
   eps=xeps;
   cmuf=1.0e-15;
}

Istgen::~Istgen(){
   if(p!=NULL)delete [] p;
}

void Istgen::set_data(long n,double *sd, double *sn){
   total_objs=n;
   xd=sd;
   xn=sn;
}

void Istgen::gen(int (*compar) (const long &, const long &)){
   long i,j,m,sum;
   if(p!=NULL)delete [] p;
   p=new double[total_objs];

   long *ord=new long[total_objs+1];
   long *nab=new long[total_objs+1];

   for (i=0; i<total_objs; i++){
      *(nab+i)=0;
      *(ord+i)=i;
   }
   *(ord+total_objs)=total_objs;
   *(nab+total_objs)=total_objs;

//Count the number of points above each point and store in array nab.
   for (i=0; i<total_objs; i++){
      for(j=i+1;j<total_objs;j++){
         m=compar(i,j);
         if(m==1)(*(nab+i))++;
         else if(m==-1)(*(nab+j))++;
      }
      mark(pflag,i+1,100,"counts of up points");
   }
   sum=0;
   for(i=0;i<total_objs;i++)sum+=*(nab+i);
   cout << "Total edges = " << sum << endl;

//Sort by number of points above each point.
   hSort(total_objs,nab,ord);
   delete [] nab;

   this->split(total_objs,ord,compar);
}

void Istgen::split(long npp,long *idx,int (*compar) (const long &, const long &)){
   double sum_n=0,sum_d=0,pav;
   double *bt,*ct,*at,xx;
   long i,j,k,m,sum,*ut,*vt,*wt,*mk1,*mk2,**rt;
   long *st,*yt;

   wt=idx;
   for(i=0;i<npp;i++){
      j=*(wt++);
      sum_n+=*(xn+j);
      sum_d+=*(xd+j);
   }
   if(sum_d==0)return;
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
   double *a=new double[npp];
   long  *mrk=new long[npp];

   at=a;
   wt=idx;
   vt=mrk;
   for(i=0;i<npp;i++){
      j=*(wt++);
      *(at++)=*(xn+j)-*(xd+j)*pav;
      *(vt++)=0;
   }
   //Mark mixed points.
   long *nab=new long[npp];
   at=a;
   vt=nab;
   wt=idx;
   mk1=mrk;
   for(i=0; i<npp; i++){
      *vt=0;
      if(*(at++)>0){
         ut=idx;
         bt=a;
         mk2=mrk;
         for(j=0;j<i;j++){
            m=compar(*wt,*(ut++));
            if((m==1)&&(*bt<0)){
               (*vt)++;
               *mk1=1;
               *mk2=-1;
            }
            bt++;mk2++;
         }
      }
      vt++;wt++;mk1++;
      mark(pflag,i+1,100,"counts of up points");
   }
   sum=0;
   vt=nab;
   for(i=0;i<npp;i++)sum+=*(vt++);
   cout << "Active edges = " << sum << endl;
   //Count pos and neg points.
   long pos=0,neg=0;
   mk1=mrk;
   for(i=0;i<npp;i++){
      if(*mk1==1)pos++;
      else if(*mk1==-1)neg++;
      mk1++;
   }
   cout << "Total pos points = " << pos << endl; 
   cout << "Total neg points = " << neg << endl;

   //if(!pos)goto skip_pos;
   long *cvp,*pik,*nrk;
   double *ap,*an,*dst,**dsp;
   long u,w,**vps;
   double *cp,*cn;
   long *upst,*vpst,span,iter=0;
   long *wgt; //Weights of neg points
   long *que; //Que of pos points to process
   long *lab; //Labels of que points
   long *pcs; //Marker for pos forward
   long level,flag,bflag,sflag,sw,ptt;
   if(pos){

   //Arrays for pos points.
   cvp=new long[pos];
   pik=new long[pos];
   ap=new double[pos];
   mk1=mrk;
   ut=cvp;
   vt=pik;
   wt=nab;
   at=a;
   bt=ap;
   for(i=0;i<npp;i++){
      if(*(mk1++)==1){
         *(ut++)=*wt;
         *(bt++)=*at;
         *(vt++)=i;
      }
      wt++;
      at++;
   }
   //Arrays for neg points.
   nrk=new long[npp];
   an=new double[neg];
   neg=0;
   mk1=mrk;
   vt=nrk;
   at=a;
   bt=an;
   for(i=0;i<npp;i++){
      if(*(mk1++)==-1){
         *(bt++)=-*at;
         *vt=++neg;
      }
      else *vt=0;
      at++;
      vt++;
   }
   //Make up neighbor lists.
   vps=(long **)new long[pos];
   mk1=mrk;
   ut=cvp;
   rt=vps;
   vt=idx;
   for(i=0; i<npp; i++){
      if(*(mk1++)==1){
         if(*ut)*rt=new long[*ut];
         else *rt=NULL;
         wt=idx;
         at=a;
         st=*rt;
         yt=nrk;
         for(j=0;j<i;j++){
            m=compar(*vt,*wt);
            if((m==1)&&(*at<0)){
               *(st++)=*yt-1;
            }
            wt++;at++;yt++;
         }
         ut++;rt++;
      }
      vt++;
      mark(pflag,i+1,100,"lists of up points");
   }
   //Make variable arrays.
   ut=cvp;
   dsp=(double **)new long[pos];
   for(i=0;i<pos;i++){
      m=*(ut++);
      if(m)dst=*(dsp+i)=new double[m];
      else *(dsp+i)=NULL;
      for(j=0;j<m;j++)*(dst++)=0;
   }
   cp=new double[pos]; //Cumulative volume array.
   cn=new double[neg]; //Cumulative volume array.
   wgt=new long[neg]; //Weights of neg points
   que=new long[pos]; //Que of pos points to process
   lab=new long[pos];
   pcs=new long[pos]; //Marker for pos forward

   span=1;
   iter=0;
   while(span>0){
      iter++;
      bt=ap;
      ct=cp;
      for(i=0;i<pos;i++)*(ct++)=*(bt++);
      ct=cn;
      for(i=0;i<neg;i++)*(ct++)=0;
      for(i=0;i<pos;i++){
         m=*(cvp+i);
         if(m){
            vpst=*(vps+i);
            dst=*(dsp+i);
            ct=cp+i;
            for(j=0;j<m;j++){
               xx=*(dst++);  
               *(ct)-=xx;
               *(cn+*(vpst++))+=xx;
            }
         }
      }

      //Push up
      double sum3=0;
      for(i=0;i<pos;i++){
         m=*(cvp+i);
         ct=cp+i;
         if((*ct>0)&&(m)){
            vpst=*(vps+i);
            dst=*(dsp+i);
            for(j=0;j<m;j++){
               k=*(vpst++);
               xx=*(an+k)-*(cn+k);
               if(xx>0){
                  if(xx<*ct){
                     (*ct)-=xx;
                     (*dst)+=xx;
                     sum3+=xx;
                     *(cn+k)=*(an+k);
                  }
                  else {
                     (*dst)+=*ct;
                     sum3+=*ct;
                     (*(cn+k))+=*ct;
                     *ct=0;
                     break;
                  }
               }
               dst++;
            }
         }
      }
      cout <<  "Pushed = " << sum3 << endl << endl;

      //Weight of neg points.
      ct=cn;
      at=an;
      wt=wgt;
      for(i=0;i<neg;i++){
         if(*(ct++)+cmuf<*(at++))*(wt++)=-1;
         else *(wt++)=0;
      }
      for(i=0;i<pos;i++){
         m=*(cvp+i);
         ct=cp+i;
         if((*ct>cmuf)&&(m)){
            vpst=*(vps+i);
            for(j=0;j<m;j++)*(wgt+*(vpst++))=1;
         }
      }

      //Label points.
      wt=pcs;
      for(i=0;i<pos;i++)*(wt++)=1;
      flag=1;
      level=0;
      ptt=1;
      sw=0;
      while(flag&&ptt){
         level++;
         ptt=0;
         for(i=0;i<pos;i++){
            m=*(cvp+i);
            if((m>1)&&(*(pcs+i))){
               vpst=*(vps+i);
               dst=*(dsp+i);
               bflag=0;
               for(j=0;j<m;j++){
                  if((*(wgt+*vpst)==level)&&(*dst>cmuf)){
                     bflag=1;
                     break;
                  }
                  vpst++;
                  dst++;
               }
               if(bflag){
                  vpst=*(vps+i);
                  sflag=0;
                  for(j=0;j<m;j++){
                     k=*(wgt+*vpst);
                     if(k==0)*(wgt+*vpst)=level+1;
                     else if(k==-1)flag=0;
                     if(k<1)sflag=1;
                     vpst++;
                  }
                  *(pcs+i)=0;
                  if(sflag){
                    ptt++;
                    *(que+sw)=i;
                    *(lab+(sw++))=level;
                  }
               }
            }
         }
      }
      cout << "Deepest level = " << level <<  " Iter = " << iter << endl;
      span=1-flag;

      //Push across
      if(ptt&&(!flag)){
      while(sw){
         i=*(que+sw-1);
         level=*(lab+sw-1);
         m=*(cvp+i);
         j=k=0;
         dst=*(dsp+i);
         upst=vpst=*(vps+i);
         while((j<m)&&(k<m)){
            while((j<m)&&((*dst==0)||(*(wgt+*upst)!=level))){
               dst++;upst++;j++;
            }
            while((k<m)&&((xx=*(an+*vpst)-*(cn+*vpst))<=0)){vpst++;k++;}
            if(j==k){vpst++;k++;}
            else if((j<m)&&(k<m)){
               if(xx<*dst){
                  (*dst)-=xx;
                  (*(cn+*upst))-=xx;
                  (*(*(dsp+i)+k))+=xx;
                  *(cn+*vpst)=*(an+*vpst);
                  vpst++;k++;
               }
               else {
                  (*(cn+*upst))-=*dst;
                  (*(*(dsp+i)+k))+=*dst;
                  (*(cn+*vpst))+=*dst;
                  *dst=0;
                  dst++;upst++;j++;
               }
            }
         }
         sw--;
      }
      }
   }

   } //end skip !pos
   //skip_pos:

   //Define the max positive point set.              
   long *pmp=new long[npp];
   wt=pmp;
   for(i=0;i<npp;i++)*(wt++)=0;
   wt=pmp;
   at=a;
   mk1=mrk;
   for(i=0;i<npp;i++){
      if((*mk1==0)&&(*at>0))*wt=1;
      wt++;at++;mk1++;
   }
   for(i=0;i<pos;i++){
      m=*(cvp+i);
      vpst=*(vps+i);
      sflag=1;
      for(j=0;j<m;j++){
         if(*(wgt+*vpst)<1){
            sflag=0;
            break;
         }
         vpst++;
      }
      if(sflag)*(pmp+*(pik+i))=1;
   }
     //solidify set
   wt=pmp;
   ut=idx;
   for(i=0; i<npp; i++){
      if(*(wt++)){
         vt=idx;
         yt=pmp;
         for(j=0;j<i;j++){
            m=compar(*ut,*(vt++));
            if(m==1)*yt=1;
            yt++;
         }
      }
      ut++;
   }
   double sumt,sump=0,sumn=0;   
   long pox=0,nex=0;
   at=a;
   wt=pmp;
   for(i=0;i<npp;i++){
      if(*wt){
         if((xx=*at)>0)sump+=xx;
         else sumn+=xx;
         pox++;
      }
      else nex++;
      at++;
      wt++;
   }
   sumt=sump+sumn;
   cout << "Max sum = " << sumt << endl;
                  
   //If zero max or zero split, terminate process.
   if((sumt<eps)||((!nex)||(!pox))){
      for(i=0;i<npp;i++)*(p+*(idx+i))=pav;
      delete [] a;
      delete [] mrk;
      delete [] nab;
      delete [] pmp;
      delete [] idx;
      if(pos){
         delete [] pik;
         delete [] ap;
         delete [] nrk;
         delete [] an;
         for(i=0;i<pos;i++){
            if(*(cvp+i)){
               delete [] *(vps+i);
               delete [] *(dsp+i);
            }
         }
         delete [] cvp;
         delete [] vps;
         delete [] dsp;
         delete [] cp;
         delete [] cn;
         delete [] wgt;
         delete [] que;
         delete [] lab;
         delete [] pcs;
      }
      return;
   }

   cout << nex << " " << pox << " " << npp << endl;

   long *id1=new long[pox];
   long *id2=new long[nex];
   long nn1=0,nn2=0;
   wt=pmp;
   ut=idx;
   for(i=0;i<npp;i++){
      if(*(wt++))*(id1+(nn1++))=*ut;
      else *(id2+(nn2++))=*ut;
      ut++;
   }
   if(nn1!=pox){cout << "Error in nn1!" << endl;exit(0);}
   if(nn2!=nex){cout << "Error in nn2!" << endl;exit(0);}

   //Free memory.
   delete [] a;
   delete [] mrk;
   delete [] nab;
   delete [] pmp;
   delete [] idx;
   if(pos){
      delete [] pik;
      delete [] ap;
      delete [] nrk;
      delete [] an;
      for(i=0;i<pos;i++){
         if(*(cvp+i)){
            delete [] *(vps+i);
            delete [] *(dsp+i);
         }
      }
      delete [] cvp;
      delete [] vps;
      delete [] dsp;
      delete [] cp;
      delete [] cn;
      delete [] wgt;
      delete [] que;
      delete [] lab;
      delete [] pcs;
   }

   //Make recursive calls
   this->split(nn1,id1,compar);
   this->split(nn2,id2,compar);
}

double Istgen::avg(){
   long i;
   double sum_n=0,sum_d=0,pav;

   for(i=0;i<total_objs;i++){
      sum_n+=*(xn+i);
      sum_d+=*(xd+i);
   }
   if(sum_d>0)pav=sum_n/sum_d;
   else pav=0;
   
   return(pav);
}

double Istgen::info(){
   long i;
   double sum=0,l2=1.0/log(2.0),z;
   double pav=this->avg();

   for(i=0;i<total_objs;i++){
      if((z=*(p+i))>0){
         sum+=*(xd+i)*z*l2*log(z/pav);
      }
   }
   
   return(sum);
}

double Istgen::sigma(){
   long i;
   double sum=0,sum_d=0,xx,yy;
   double pav=this->avg(),z;

   for(i=0;i<total_objs;i++){
      xx=*(xn+i);
      yy=*(xd+i);
      sum_d+=yy;
      z=xx/yy-pav;
      sum+=yy*z*z;
   }
   return(sqrt(sum/sum_d));
}

double Istgen::rms(){
   long i;
   double sum=0,sum_d=0,xx,yy,z;

   for(i=0;i<total_objs;i++){
      xx=*(xn+i);
      yy=*(xd+i);
      sum_d+=yy;
      z=xx/yy-*(p+i);
      sum+=yy*z*z;
   }
   return(sqrt(sum/sum_d));
}

void Istgen::set_name(const char *nam){
   int lxn=strlen(nam);
   name=new char[lxn+1];
   strcpy(name,nam);
}

}
