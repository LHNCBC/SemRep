#include <iostream>
#include <fstream>
#include <sstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include "runn.h"
#include "Btree.h"
#include "Word.h"
#include <Dbinbase.h>
#include <LUD.h>
#include <Elev.h>
#include <GBoost.h>
#include <Isgrid.h>
#include "SBoost.h"
#define EPS 1.0E-12

using namespace std;
namespace iret{
 
 //PAV 
SBoost::SBoost(long dm, long ln, Index *gind){
  dim=dm;
  len=ln;
  gInd=gind; 
  scr=new double*[dim];
}

SBoost::SBoost(long dm){
  dim=dm;
}

SBoost::~SBoost(){
  delete[]scr;
}

void SBoost::Setup(long i, double *sco){
  scr[i]=sco;
  cout<<"Set up for "<<i<<" completed"<<endl;

}

void SBoost::Learn_Boost(const char* nm){
  long l;
  char *bnam;
  l=strlen(nm)+5;
  bnam=new char[l];
  epsilon=1.0E-12;
  int pflag=get_qflag();
  long i,j,k,d, dm, c=0, *obj;
  double *dmax,*dmin,xx,sum,deps, **kval, *label, *D, *q, diff=1, q_sum;
  grn=10000;
  epl=0.0001;

  kval=new double*[dim]; 
  for(d=0;d<dim;d++) kval[d]=new double[len];
  dmax=new double[dim];
  dmin=new double[dim];
  D=new double[len];
  q=new double[len];
  label=new double[len];
  obj=new long[100];
 
  double *pdoc=new double[len];

  for(i=0;i<len;i++){
    pdoc[i]=1;
    for(d=0;d<dim;d++) *(kval[d]+i)=0;
    if(gInd->Subvalue(i)) label[i]=1;
    else label[i]=-1;
  }

  t++;
  if(pflag)cout << t << " Iteration" << endl;
  while(diff>epl){
    cout<<"round="<<c<<endl;
    for(d=0;d<dim;d++){
      dmax[d]=dmin[d]=*(scr[d]);
      for(i=0;i<len;i++){
         xx=*(scr[d]+i);
         dmax[d]=xx>dmax[d]?xx:dmax[d];
         dmin[d]=xx<dmin[d]?xx:dmin[d];
      }
      if(pflag) cout<<d<<" Min value: "<<dmin[d]<<";Max value: "<<dmax[d]<<endl;
    }
   
    pIs=(Isgrid**)new long[dim];
    for(d=0;d<dim;d++) pIs[d]=new Isgrid;
    for(d=0;d<dim;d++){ 
      deps=1.0-epsilon;
      sum=0;
      for(i=0;i<len;i++){
        D[i]=0;
        for(dm=0;dm<dim;dm++) D[i]+=*(kval[dm]+i);
        D[i]-=*(kval[d]+i);
        pdoc[i]=exp(-(label[i]*D[i]));
        sum+=pdoc[i];
      }
      pIs[d]->set_xdom(dmin[d],dmax[d]);
      pIs[d]->set_xgran(grn);
      pIs[d]->init1();
      for(i=0;i<len;i++){
        pdoc[i]=pdoc[i]/sum;
        if(label[i]>0) pIs[d]->add_data(*(scr[d]+i),pdoc[i],pdoc[i]);
        else pIs[d]->add_data(*(scr[d]+i),pdoc[i],0.0);
      } 
      pIs[d]->dim1();
      pIs[d]->extend_1df();
      if(pflag) cout << "Average p " << pIs[d]->avg() << " Information " << pIs[d]->info() << endl;
      for(i=0;i<len;i++){
        xx=pIs[d]->val_1df(*(scr[d]+i));
        if(xx<epsilon)xx=epsilon;
        if(xx>deps)xx=deps;
        *(kval[d]+i)=0.5*log(xx/(1-xx));
      }
    }
    obj[c]=0;
    for(i=0;i<len;i++){ 
      q_sum=0;
      for(d=0;d<dim;d++) q_sum+=*(kval[d]+i);
      q[i]=exp(-q_sum*label[i]);
      obj[c]+=q[i];
    }
    if(c>0) diff=obj[c-1]-obj[c];
    else diff=obj[c];
    if(diff<=epl){
      for(d=0;d<dim;d++){
        pIs[d]->set_name(add_num(nm, d, bnam));
        pIs[d]->write_1df();
      }
    }
    for(d=0;d<dim;d++) delete pIs[d];
    delete [] pIs; 
    c++;
  } 
}
void SBoost::Load_Boost(const char* nm){
  long i, l;
  char *bnam;
  l=strlen(nm)+5;
  bnam=new char[l];
  pIs=(Isgrid**)new long[dim];
  for(i=0;i<dim;i++){
    pIs[i]=new Isgrid(add_num(nm, i, bnam));
    pIs[i]->read_1df();
    pIs[i]->extend_1df();
  }
}

//Finds the score by adding up all ts_sxx
double SBoost::Score(double *scc){
  long d;
  double xx, score=0, eps=0.000001;
  for(d=0;d<dim;d++){
    xx=pIs[d]->val_1df(scc[d]);
    if(xx<eps) xx=eps;
    if(xx>(1-eps)) xx=(1-eps); 
    score+=0.5*log(xx/(1.0-xx));
  } 
  return score;
}

  //Optimal Alpha Method

SABoost::SABoost(long dm, long ln, Index *gind, const char *nam): FBase("SABoost", nam){
  dim=dm;
  len=ln;
  gInd=gind; 
  scr=new double*[dim];
}

SABoost::SABoost(long dm,const char *nam): FBase("SABoost", nam){
  dim=dm;
}

SABoost::~SABoost(){
  delete[]scr;
}

void SABoost::Setup(long i, double *sco){
  scr[i]=sco;
  cout<<"Set up for "<<i<<" completed"<<endl;

}

void SABoost::Learn_Boost(){
  epsilon=1.0E-12;
  int pflag=get_qflag();
  long i,j,k,d, dm, c=0, round, tc;
  double *dmax,*dmin,xx,sum,deps;
  double sum_n, sum_o=1000, diff=1, pow;
  double *ax,*bx,mx;
  epl=0.01;

  alpha_h=new double*[dim]; 
  for(d=0;d<dim;d++) alpha_h[d]=new double[len];
  alpha=new double[dim];
  dmax=new double[dim];
  dmin=new double[dim];
  ax=new double[dim];
  bx=new double[dim];
  label=new double[len];

  for(i=0;i<len;i++){
    for(d=0;d<dim;d++) *(alpha_h[d]+i)=0;
    if(gInd->Subvalue(i)) label[i]=1;
    else label[i]=-1;
  }
  for(d=0;d<dim;d++){
    dmax[d]=dmin[d]=*(scr[d]);
    for(i=0;i<len;i++){
      if(label[i]>0) xx=*(scr[d]+i);
      else xx=-*(scr[d]+i);
      dmax[d]=xx>dmax[d]?xx:dmax[d];
      dmin[d]=xx<dmin[d]?xx:dmin[d];
    }  
    if(dmax[d]==0.0) { cout <<"Max is zero"<<endl; exit(0);}
    if(dmin[d]==0.0) { cout <<"Min is zero"<<endl; exit(0);}
  }

  round=0;
  while(diff>epl){
    cout<<"round="<<round<<endl; 
    for(d=0;d<dim;d++){
      ax[d]=300.0/(-dmax[d]);
      bx[d]=300.0/(-dmin[d]);
      while(bx[d]-ax[d]>EPS){
        mx=(ax[d]+bx[d])/2.0; /*midpoint*/
        xx=Z_alpha(mx, d);
        if(xx>0.0)bx[d]=mx;
        else ax[d]=mx;
      }
      alpha[d]=mx;
      for(i=0;i<len;i++) *(alpha_h[d]+i)=*(scr[d]+i)*alpha[d];
     
      sum_n=0;
      for(i=0;i<len;i++){ 
        pow=0;
        for(c=0;c<dim;c++) pow+=*(alpha_h[c]+i);
        if(label[i]>0) sum_n+=exp(-pow);
        else sum_n+=exp(pow);
      }
      diff=sum_o-sum_n;
      sum_o=sum_n;
    }
    //Write final thetas or coefficients out in ".cf"-regular and "bcf"-binary files
    if(diff<epl){
      ofstream *pfout, *ptfout;
      pfout=get_Ostr("cf");
      ptfout=get_Ostr("bcf");
      *pfout<<"Dimention"<<endl<<'\t'<<dim<<endl;
      ptfout->write((char*)&dim, sizeof(long));
      *pfout<<"Optimal Coef. Are:"<<endl;
      for(c=0;c<dim;c++){
        *pfout<<"\t"<<alpha[c]<<endl;
        ptfout->write((char*)(alpha+c), sizeof(double));
      }
      dst_Ostr(pfout);
      dst_Ostr(ptfout);
    }
    round++;
  }
} 

void SABoost::Load_Boost(){
  long dmn;
  ifstream *pfin;
  pfin=get_Istr("bcf");
  pfin->read((char*)&dmn, sizeof(long));
  if(dim==dmn){
    char cnam[10000];
    double val;
    long i;
    alpha=new double[dim];
    //Reads in optimal alpha's
    pfin->read((char*)alpha, (dim+1)*sizeof(double));
    dst_Istr(pfin);
  }
  else{
    cout<<"Error, Dimentions Do Not Match!"<<endl;
    exit(0);
  }
}
 
double SABoost::Z_alpha(double alp, long dm){
   long i,j,d;
   double xx=0.0, *sum;
   sum=new double[len];
   for(i=0;i<len;i++){
     sum[i]=0;
     for(d=0;d<dm;d++) sum[i]+=*(alpha_h[d]+i);
     for(d=dm+1;d<dim;d++) sum[i]+=*(alpha_h[d]+i);
     if(label[i]>0) xx-=*(scr[dm]+i)*exp(-sum[i])*exp(-*(scr[dm]+i)*alp);
     else xx+=*(scr[dm]+i)*exp(sum[i])*exp(*(scr[dm]+i)*alp);
   }
   return(xx);
}


//Finds the score by adding up weighted scores
double SABoost::Score(double *scc){
  double score=0;
  long d;
  for(d=0;d<dim;d++) score+=scc[d]*alpha[d];
  score+=alpha[dim];
  return score;
}

}


