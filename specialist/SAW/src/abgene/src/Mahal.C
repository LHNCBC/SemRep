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
#include <Elev.h>
#include "Mahal.h"

namespace iret {

Mahal::Mahal(long dm, long ln, Index *gind, const char *nam) : FBase("mahal", nam){
  dim=dm;
  len=ln;
  gInd=gind; 
  scr=new double*[dim];
}

Mahal::Mahal(long dm, const char* nam) : FBase("mahal", nam){
  dim=dm;
}

Mahal::~Mahal(){
  delete[]scr;
}

void Mahal::Setup(long i, double *sco){
  long k;
  scr[i]=sco;
}

void Mahal::Learn_Mahal(void){
  long k,r,c,g=0,b=0,l,i;
  double *sum_g, *sum_b, sc;
  cov_g=new double*[dim];
  cov_b=new double*[dim];
  for(r=0;r<dim;r++){
    cov_g[r]=new double[dim];
    cov_b[r]=new double[dim];
  }
  sum_g=new double[dim];
  sum_b=new double[dim];
  mean_g=new double[dim];
  mean_b=new double[dim];
  
  //Finds the vector of means
  for(r=0;r<dim;r++){
    sum_g[r]=mean_g[r]=0;
    sum_b[r]=mean_b[r]=0;
  }
  cout<<"dim="<<dim<<endl;
  
  Index *tInd=new Index(0,len);
  Index *bInd=tInd->cbool_Butnot(gInd);
  g=gInd->ix;
  b=bInd->ix;
  for(i=0;i<g;i++){ 
    k=gInd->idx[i];
    for(r=0;r<dim;r++) sum_g[r]+=*(scr[r]+k);
  }
  for(i=0;i<b;i++){ 
    k=bInd->idx[i];
    for(r=0;r<dim;r++) sum_b[r]+=*(scr[r]+k);
  }

  for(r=0;r<dim;r++){
    mean_g[r]=sum_g[r]/g;
    mean_b[r]=sum_b[r]/b;
    cout<<r<<"  "<<"mean_g="<<mean_g[r]<<"  mean_b="<<mean_b[r]<<endl;
  }
  
  //Find Priors
  pri_g=g*1.0/len;
  pri_b=b*1.0/len;

  //Generate the covariance matrix
  for(r=0;r<dim;r++){
    for(c=0;c<dim;c++){
      cov_g[r][c]=cov_b[r][c]=0;
    }
  }
  for(i=0;i<g;i++){ 
    k=gInd->idx[i];
    for(r=0;r<dim;r++){
      for(c=r;c<dim;c++)
        cov_g[c][r]=cov_g[r][c]+=(*(scr[r]+k)-mean_g[r])*(*(scr[c]+k)-mean_g[c]);
    }
  }  
  for(i=0;i<b;i++){ 
    k=bInd->idx[i];
    for(r=0;r<dim;r++){
      for(c=r;c<dim;c++)
        cov_b[c][r]=cov_b[r][c]+=(*(scr[r]+k)-mean_b[r])*(*(scr[c]+k)-mean_b[c]);
    }
  }
  
  for(r=0;r<dim;r++){
    for(c=0;c<dim;c++){
      cov_g[r][c]=cov_g[r][c]/g;
      cov_b[r][c]=cov_b[r][c]/b;
    }
  }
  cout<<"bad="<<b<<" good="<<g<<endl;

  //Finds determinants
  pLud_g=new LUDecomp(dim,(double**)cov_g);
  det_g=pLud_g->det();
  pLud_b=new LUDecomp(dim,(double**)cov_b);
  det_b=pLud_b->det();
  
  //Writes dimention, means, and covariance matrices out to the file
  ofstream *pfout, *ptfout;
  pfout=get_Ostr("mat");
  ptfout=get_Ostr("bmat");
  *pfout<<"Dimention:"<<endl<<"\t"<<dim<<endl;
  ptfout->write((char*)(&dim), sizeof(long));

  *pfout<<"Mean of Goods:"<<endl;
  for(r=0;r<dim;r++){
    *pfout<<"\t"<<mean_g[r]<<endl;
    ptfout->write((char*)(mean_g+r), sizeof(double));
  }
  *pfout<<"Mean of Bads:"<<endl;
  for(r=0;r<dim;r++){
    *pfout<<"\t"<<mean_b[r]<<endl;
    ptfout->write((char*)(mean_b+r), sizeof(double));
  }
  *pfout<<"Covariance Matrix of Goods:"<<endl;
  for(r=0;r<dim;r++){
    for(c=0;c<dim;c++){
      *pfout<<"\t"<<cov_g[r][c]<<"   ";
      ptfout->write((char*)(cov_g[r]+c), sizeof(double));
    }
    *pfout<<endl;
  }    
  *pfout<<"Covariance Matrix of Bads:"<<endl;;
  for(r=0;r<dim;r++){
    for(c=0;c<dim;c++){
      *pfout<<"\t"<<cov_b[r][c]<<"   ";
      ptfout->write((char*)(cov_b[r]+c), sizeof(double));
    }
    *pfout<<endl;
  }  
  sc=log(pri_g/pri_b)-0.5*log(det_g/det_b);
  *pfout<<"The Scalar part of the Score:"<<endl;
  *pfout<<"\t"<<sc<<endl; 
  ptfout->write((char*) &sc, sizeof(double));
  dst_Ostr(pfout);
  dst_Ostr(ptfout);
}

void Mahal::Load_Mahal(){
  long r, c, dmn;
  ifstream *pfin;
  pfin=get_Istr("bmat");
  pfin->read((char*)&dmn, sizeof(long));
  if(dim==dmn){
    char cnam[10000];
    double val;
    cov_g=new double*[dim];
    cov_b=new double*[dim];
    for(r=0;r<dim;r++){
      cov_g[r]=new double[dim];
      cov_b[r]=new double[dim];
    }
    mean_g=new double[dim];
    mean_b=new double[dim];
 
    //Reads in means 
    pfin->read((char*)mean_g, dim*sizeof(double));
    pfin->read((char*)mean_b, dim*sizeof(double));

    //Reads in covariance matrices
    for(r=0;r<dim;r++){
      pfin->read((char*)cov_g[r], dim*sizeof(double));
    }
    for(r=0;r<dim;r++){
      pfin->read((char*)cov_b[r], dim*sizeof(double));
    }
 
    //Reads in the scalar used in calculation of score
    pfin->read((char*)&coef, sizeof(double));
    dst_Istr(pfin);
  }
  else{
    cout<<"Error, Dimentions Do Not Match"<<endl;
    exit(0);
  }
}


// Mahalanobis distance and scores
double Mahal::Score(double *scc){ 
  double *rhs_g, *rhs_b, rating;
  double *z_g, *z_b;
  long r, c;
  double mdist_g=0, mdist_b=0;
  rhs_g=new double[dim];
  rhs_b=new double[dim];
  z_g=new double[dim];
  z_b=new double[dim];
  for(r=0;r<dim;r++){
    rhs_g[r]=scc[r]-mean_g[r];
    rhs_b[r]=scc[r]-mean_b[r];
  }
  pLud_g=new LUDecomp(dim,(double**)cov_g);
  pLud_b=new LUDecomp(dim,(double**)cov_b);
  pLud_g->solve(rhs_g, z_g);
  pLud_b->solve(rhs_b, z_b);
  for(r=0;r<dim;r++){
    mdist_g+=rhs_g[r]*z_g[r];
    mdist_b+=rhs_b[r]*z_b[r];
  }
  rating=coef-0.5*(mdist_g-mdist_b);
  return rating;
}
}


