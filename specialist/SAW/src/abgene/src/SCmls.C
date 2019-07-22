#include <iostream>
#include <fstream>
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
#include "SCmls.h"

namespace iret{

SCmls::SCmls(long dm, long ln, Index *gind, const char *nam) : FBase("scmls", nam){
  dim=dm;
  len=ln;
  gInd=gind; 
  scr=new double*[dim];
}

SCmls::SCmls(long dm, const char* nam) : FBase("scmls", nam){
  dim=dm;
}

SCmls::~SCmls(){
  delete[]scr;
}

void SCmls::Setup(long i, double *sco){
  scr[i]=sco;
  cout<<"Set up for "<<i<<" completed"<<endl;

}

void SCmls::Learn_Cmls(){
  long d, i, k;
  double *del, *dv, *dw, *r, *label;
  double c, den, num, eval1, eval2, arg, sum;
  double lambda=0.001, eps=0.1;
  
  w=new double[dim+1];
  dv=new double[dim+1];
  dw=new double[dim+1];
  del=new double[dim+1];
  r=new double[len];
  label=new double[len];

  for(i=0;i<len;i++){
    if(gInd->Subvalue(i)) label[i]=1;
    else label[i]=-1;
  } 
 
  for(d=0;d<=dim;d++){
    w[d]=dv[d]=0;
    del[d]=1;
  }
  
  for(i=0;i<len;i++) r[i]=-1;
  for(k=1;k<101;k++){
    c=(1-0.02*(k-1))>0 ? (1-0.02*(k-1)) : 0;
    for(d=0;d<=dim;d++){
      den=num=0;
      for(i=0;i<len;i++){
        eval1=(r[i]<=0) ? 2*r[i] : 2*c*r[i]; 
        if(d<dim) num+=eval1*label[i]*(*(scr[d]+i));
        else num+=eval1*label[i];
	if(d<dim) {
          eval2=(r[i]<=fabs(*(scr[d]+i)*del[d])) ? 2 : 2*c;
          den+=eval2*pow(*(scr[d]+i),2);
        }
	else{
          eval2=(r[i]<=fabs(del[d])) ? 2 : 2*c;
          den+=eval2;
        }
      }
      dv[d]=-(num+2*lambda*len*w[d])/(den+2*lambda*len);
      arg=(dv[d]>(-del[d])) ? dv[d] : (-del[d]);
      dw[d]=(arg<del[d]) ? arg : del[d];

      //Updates
      for(i=0;i<len;i++){
        if(label[i]==1){
	  if(d<dim) r[i]+=*(scr[d]+i)*dw[d];
          else r[i]+=dw[d];
        } 
	else{
	  if(d<dim) r[i]-=*(scr[d]+i)*dw[d];
          else r[i]-=dw[d];
        }
      }
      w[d]+=dw[d];
      del[d]=2*fabs(dw[d])+eps;
      sum=0;
      for(i=0;i<len;i++){
       	if(r[i]<0) sum+=pow(r[i],2);
       	else sum+=pow(r[i],2)*c;
      } 
      cout<<k<<" "<<d<<" "<<sum<<endl;
    }
    cout<<endl;
  }
  cout<<"Optimal Coefficients Are:"<<endl;
  for(i=0;i<=dim;i++) cout<<w[i]<<"  ";
  cout<<endl; 

  //Write final thetas or coefficients out in ".cf"-regular and "bcf"-binary files
  ofstream *pfout, *ptfout;
  pfout=get_Ostr("cf");
  ptfout=get_Ostr("bcf");
  *pfout<<"Dimention"<<endl<<'\t'<<dim<<endl;
  ptfout->write((char*)&dim, sizeof(long));
  *pfout<<"Optimal Coef. Are:"<<endl;
  for(i=0;i<=dim;i++){
    *pfout<<"\t"<<w[i]<<endl;
    ptfout->write((char*)(w+i), sizeof(double));
  }
  dst_Ostr(pfout);
  dst_Ostr(ptfout);
}

void SCmls::Load_Cmls(){
  long dmn;
  ifstream *pfin;
  pfin=get_Istr("bcf");
  pfin->read((char*)&dmn, sizeof(long));
  if(dim==dmn){
    char cnam[10000];
    double val;
    long i;
    w=new double[dim+1];
    //Reads in optimal w's
    pfin->read((char*)w, (dim+1)*sizeof(double));
    dst_Istr(pfin);
  }
  else{
    cout<<"Error, Dimentions Do Not Match!"<<endl;
    exit(0);
  }
} 

//Finds the score using optimal theta's
double SCmls::Score(double *scc){
  double score=0;
  long d;
  for(d=0;d<dim;d++) score+=scc[d]*w[d];
  score+=w[dim];
  return score;
}
}
