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
#include "Lglin.h"

namespace iret{

Lglin::Lglin(long dm, long ln, Index *gind, const char *nam) : FBase("loglin", nam){
  dim=dm;
  len=ln;
  gInd=gind; 
  scr=new double*[dim];
}

Lglin::Lglin(long dm, const char* nam) : FBase("loglin", nam){
  dim=dm;
}

Lglin::~Lglin(){
  delete[]scr;
}

void Lglin::Setup(long i, double *sco){
  scr[i]=sco;
  cout<<"Set up for "<<i<<" completed"<<endl;
}

void Lglin::Learn_Lglin(){
  long k,r,c,i=0;
  double *sum, *ex, *max, *s, *y, **B, **B1, **B2, maxim;
  double *gr, *grad,*new_grad, *rhs, *p, *den,*num, check;
  double pr=1, grad_norm=1, new_grad_norm, eps=0,epn, z=0, den1, den2;

  gr=new double[dim];
  grad=new double[dim];
  new_grad=new double[dim];
  rhs=new double[dim];
  s=new double[dim];
  y=new double[dim];
  th=new double[dim];
  sum=new double[dim];
  max=new double[dim];
  ex=new double[dim];
  p=new double[dim];
  den=new double[dim];
  num=new double[dim];
  B=new double*[dim];
  B1=new double*[dim];
  B2=new double*[dim];
  a=new double[len];
  b=new double[len];
  
  for(r=0;r<dim;r++){
     B[r]=new double[dim];
     B1[r]=new double[dim];
     B2[r]=new double[dim];
  }
  for(r=0;r<dim;r++)
    for(c=0;c<dim;c++)
      if(r==c) B[r][c]=1;
      else B[r][c]=0;
 
  //Initialize th's
  for(r=0;r<dim;r++) max[r]=abs(*(scr[r]));
  for(k=1;k<len;k++){
    for(r=0;r<dim;r++){
	if(max[r]<abs(*(scr[r]+k))) max[r]=abs(*(scr[r]+k));
    }
  }
  for(r=0;r<dim;r++){
    th[r]=1/max[r];
    sum[r]=0;
  }

  //Sums up the good elements for each of dim features 
  n=0;
  for(k=0;k<len;k++){
    if(gInd->Subvalue(k)){
      for(r=0;r<dim;r++)
        sum[r]+=*(scr[r]+k);
      n++;
    }
  }
  
  //Finds the Z, expected values, and gradient in the initial point 
  for(r=0;r<dim;r++){
    ex[r]=0;
  }  
  z=0;
  for(k=0;k<len; k++){
    pr=1;
    for(r=0;r<dim;r++) pr*=exp(*(scr[r]+k)*th[r]);
    for(r=0;r<dim;r++) ex[r]+=*(scr[r]+k)*pr;
    z+=pr;
  }
  for(r=0;r<dim;r++) ex[r]=ex[r]/z;
   
  grad_norm=0;
  for(r=0;r<dim;r++){
    grad[r]=-sum[r]+n*ex[r];
    grad_norm+=pow(grad[r],2);
  } 
  cout<<"Initial Gradient Norm = "<<sqrt(grad_norm)<<endl;

  //Performs the algorithm for BFGS update
  while(sqrt(grad_norm)>0.001){
    i++;
    cout<<endl<<"Quasi Newton Iteration Count "<<i<<endl;
    for(r=0;r<dim;r++) rhs[r]=-grad[r];
  
    //p is the search direction, i.e. x(k+1)-x(k)=p=-B^(inv)*f'(x)
    //or B*p=-f'(x), p is an output vector
    LUDecomp Lud_1(dim, B);
    Lud_1.solve(rhs, p);
 
    sm=maxim=0;
    for(k=0;k<len;k++) a[k]=b[k]=0;
    for(k=0;k<len; k++){
      for(r=0;r<dim;r++){
        a[k]+=*(scr[r]+k)*th[r];
        b[k]+=*(scr[r]+k)*p[r];
      }
      if(fabs(b[k])>maxim) maxim=fabs(b[k]);
      sm+=exp(a[k]);
    } 
    c1=c2=0;
    for(r=0;r<dim;r++){
      c1+=th[r]*sum[r];
      c2+=p[r]*sum[r];  
    }

    //perform the line search using Newton's method

    epn=1/maxim;//initial starting point
    if(epn>=1) epn=1;
    eps=Newton(epn);

    //data update
    for(r=0;r<dim;r++){
      th[r]=th[r]+eps*p[r];
      s[r]=eps*p[r];
    } 
    
    //Compute new gradient
    for(r=0;r<dim;r++){
      ex[r]=0;
    }  
    z=0;
    for(k=0;k<len; k++){
      pr=1;
      for(r=0;r<dim;r++)
        pr*=exp(*(scr[r]+k)*th[r]);
      for(r=0;r<dim;r++){
        ex[r]+=*(scr[r]+k)*pr;
      }
      z+=pr;
    }
    for(r=0;r<dim;r++) ex[r]=ex[r]/z;
    new_grad_norm=0;
    for(r=0;r<dim;r++){
      new_grad[r]=-sum[r]+n*ex[r];
      new_grad_norm+=pow(new_grad[r],2);
    } 
   
    //new_grad_norm=sqrt(new_grad_norm);
    //cout<<"grad_norm="<<new_grad_norm<<endl;
    for(r=0;r<dim;r++) y[r]=new_grad[r]-grad[r];

    //perform positivity check, optional 
    check=0;
    for(r=0;r<dim;r++) check+=y[r]*s[r];
    //cout<<"Positivity check="<<check<<endl;
    if(check<0) {
      cout<<"Positivity Check Fails"<<endl;
      exit(0);
    }


    //update B using BFGS
    for(r=0;r<dim;r++){
      den[r]=0;
      num[r]=0;
    }  
    den1=den2=0;
    for(r=0;r<dim;r++){
      for(c=0;c<dim;c++){
        num[r]+=B[r][c]*s[c];
        den[r]+=s[c]*B[r][c];
      }
      den1+=den[r]*s[r];	
      den2+=y[r]*s[r];
    }
    for(r=0;r<dim;r++){
      for(c=0;c<dim;c++){
        B1[r][c]=num[r]*num[c]/den1;
        B2[r][c]=y[r]*y[c]/den2;
 	B[r][c]=B[r][c]-B1[r][c]+B2[r][c];
       }
    }

    for(r=0;r<dim;r++) grad[r]=new_grad[r];
    grad_norm=new_grad_norm;
   
  }  
  cout<<endl<<"Final Gradient Norm is: "<<sqrt(grad_norm)<<endl;
  cout<<"Final Theta's are:"<<endl;
  for(r=0;r<dim;r++) cout<<th[r]<<"  ";
  cout<<endl; 

  //Write final thetas or coefficients out in ".cf"-regular and "bcf"-binary files
  ofstream *pfout, *ptfout;
  pfout=get_Ostr("cf");
  ptfout=get_Ostr("bcf");
  *pfout<<"Dimention"<<endl<<'\t'<<dim<<endl;
  ptfout->write((char*)&dim, sizeof(long));
  *pfout<<"Optimal Coef. Are:"<<endl;
  for(r=0;r<dim;r++){
    *pfout<<"\t"<<th[r]<<endl;
    ptfout->write((char*)(th+r), sizeof(double));
  }
  dst_Ostr(pfout);
  dst_Ostr(ptfout);

  delete gr, grad, new_grad, rhs, s, y, th, sum, max, ex, p, den, num,a, b;
  for(r=0;r<dim;r++){
    delete B[r];
    delete B1[r];
    delete B2[r];
  }
  delete *B, *B1, *B2;
}

void Lglin::Load_Lglin(){
  long dmn;
  ifstream *pfin;
  pfin=get_Istr("bcf");
  pfin->read((char*)&dmn, sizeof(long));
  if(dim==dmn){
    char cnam[10000];
    double val;
    long r;
    th=new double[dim];
    //Reads in optimal theta's
    pfin->read((char*)th, dim*sizeof(double));
    dst_Istr(pfin);
  }
  else{
    cout<<"Error, Dimentions Do Not Match!"<<endl;
    exit(0);
  }
} 

//Finds the score using optimal theta's
double Lglin::Score(double *scc){
  double score=0;
  long r;
  for(r=0;r<dim;r++) score+=scc[r]*th[r];
  return score;
}
//Finds optimal step size to minimize(the neg of original function, or max origianl function)
//the function along the chosen optimal search direction p;
double Lglin::Newton(double ep){
  cout<<"The initial step size is "<<ep<<endl;
  double x,xp=1, xpp, epn;
  double arg, arg1, arg2, arg3;
  long k,j=0;
  while(abs(xp)>0.01){
    j++;
    arg=arg1=arg2=arg3=0;
    for(k=0;k<len; k++){
       arg=exp(a[k]+ep*b[k]);
       arg1+=arg;
       arg2+=b[k]*arg;
       arg3+=pow(b[k],2)*arg;
    }
    x=-(c1+ep*c2-n*log(arg1));
    xp=-(c2-n*(arg2/arg1));
    xpp=n*((arg3/arg1)-pow((arg2/arg1),2));
    epn=ep-xp/xpp; 
    ep=epn;
  } 
  cout<<"Line Search Iteration Count "<<j<<endl;
  return epn;
}


}
