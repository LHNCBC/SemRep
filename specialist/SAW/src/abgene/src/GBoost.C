#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <Isgrid.h>
#include "GBoost.h"
#define EPS 1.0E-12

using namespace std;
namespace iret {

GBoost::GBoost(long trd,long tsd,double eps) {
   tr_doc=trd;
   ts_doc=tsd;
   epsilon=eps;
   mark=NULL;
   pdoc=NULL;
   ts_sxx=NULL;
   pIsg=NULL;
}

GBoost::~GBoost() {
   if(mark!=NULL)delete [] mark;
   if(pdoc!=NULL)delete [] pdoc;
   if(ts_sxx!=NULL)delete [] ts_sxx;
   if(pIsg)delete pIsg;
}

void GBoost::init(Index *gind,long gr){
   long i,j;
   double xx=1.0/((double)tr_doc);

   if(mark!=NULL)delete [] mark;
   mark=new long[tr_doc];
   if(pdoc!=NULL)delete [] pdoc;
   pdoc=new double[tr_doc];

   for(i=0;i<tr_doc;i++){
      mark[i]=0;
      pdoc[i]=xx;
   }
   for(i=0;i<gind->ix;i++)mark[*(gind->idx+i)]=1; 

   if(ts_sxx!=NULL)delete [] ts_sxx;
   ts_sxx=new double[ts_doc];

   for(i=0;i<ts_doc;i++){
      ts_sxx[i]=0.0;
   }
   grn=gr;
   t=0;
   zprod=(double)tr_doc;
}

void GBoost::update(double *trs,double *tss){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum,deps;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   dmax=dmin=trs[0];
   for(i=1;i<tr_doc;i++){
      xx=trs[i];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   if(pflag)cout << "Min value: " << dmin << "; Max value: " << dmax << endl;

   if(pIsg)delete pIsg;
   pIsg=new Isgrid;
   pIsg->set_xdom(dmin,dmax);
   pIsg->set_xgran(grn);
   pIsg->init1();
   for(i=0;i<tr_doc;i++){
      if(mark[i])pIsg->add_data(trs[i],pdoc[i],pdoc[i]);
      else pIsg->add_data(trs[i],pdoc[i],0.0);
   }
   pIsg->dim1();
   pIsg->extend_1df();
   if(pflag)cout << "Average p " << pIsg->avg() << " Information " << pIsg->info() << endl;

   //Update pdoc array
   deps=1.0-epsilon;
   sum=0;
   for(i=0;i<tr_doc;i++){
      xx=pIsg->val_1df(trs[i]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      if(mark[i])pdoc[i]*=sqrt((1.0-xx)/xx);
      else pdoc[i]*=sqrt(xx/(1.0-xx));
      sum+=pdoc[i];
   }
   zprod=zprod*sum;
   for(i=0;i<tr_doc;i++){
      pdoc[i]/=sum;
   }

   //Update tr_sxx
   for(i=0;i<ts_doc;i++){
      xx=pIsg->val_1df(tss[i]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps; 
      ts_sxx[i]+=0.5*log(xx/(1.0-xx));
   }
}
        
void GBoost::update_store(double *trs,const char *nam){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum,deps;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   dmax=dmin=trs[0];
   for(i=1;i<tr_doc;i++){
      xx=trs[i];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   if(pflag)cout << "Min value: " << dmin << "; Max value: " << dmax << endl;

   if(pIsg)delete pIsg;
   pIsg=new Isgrid;
   pIsg->set_xdom(dmin,dmax);
   pIsg->set_xgran(grn);
   pIsg->init1();
   for(i=0;i<tr_doc;i++){
      if(mark[i])pIsg->add_data(trs[i],pdoc[i],pdoc[i]);
      else pIsg->add_data(trs[i],pdoc[i],0.0);
   }
   pIsg->dim1();
   pIsg->set_name(nam);
   pIsg->write_1df();

   //Update pdoc array
   pIsg->extend_1df();
   if(pflag)cout << "Average p " << pIsg->avg() << " Information " << pIsg->info() << endl;
   deps=1.0-epsilon;
   sum=0;
   for(i=0;i<tr_doc;i++){
      xx=pIsg->val_1df(trs[i]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      if(mark[i])pdoc[i]*=sqrt((1.0-xx)/xx);
      else pdoc[i]*=sqrt(xx/(1.0-xx));
      sum+=pdoc[i];
   }
   zprod=zprod*sum;
   for(i=0;i<tr_doc;i++){
      pdoc[i]/=sum;
   }
}

//GBoost2

GBoost2::GBoost2(long nd,Index *gd,Index *bd,Index *ts){
   ndoc=nd;
   pdoc=NULL;
   ts_sxx=NULL;
   gdd=gd;
   bdd=bd;
   tst=ts;
   tdoc=gdd->ix+bdd->ix;
   pIsg=NULL;
}

GBoost2::~GBoost2() {
   if(pdoc!=NULL)delete [] pdoc;
   if(ts_sxx!=NULL)delete [] ts_sxx;
   if(pIsg)delete pIsg;
}

void GBoost2::init(double eps,long gr){
   long i,j;
   epsilon=eps;
   grn=gr;

   double xx=1.0/((double)tdoc);

   if(pdoc!=NULL)delete [] pdoc;
   pdoc=new double[ndoc];
   for(i=0;i<ndoc;i++){
      pdoc[i]=xx;
   }

   if(ts_sxx!=NULL)delete [] ts_sxx;
   ts_sxx=new double[ndoc];

   for(i=0;i<ndoc;i++){
      ts_sxx[i]=0.0;
   }
   t=0;
   zprod=(double)tdoc;
}

void GBoost2::update(double *sco){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum,deps;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   j=gdd->idx[0];
   dmax=dmin=sco[j];
   for(i=1;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx=sco[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx=sco[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   if(pflag)cout << "Min value: " << dmin << "; Max value: " << dmax << endl;

   if(pIsg)delete pIsg;
   pIsg=new Isgrid;
   pIsg->set_xdom(dmin,dmax);
   pIsg->set_xgran(grn);
   pIsg->init1();
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pIsg->add_data(sco[j],pdoc[j],pdoc[j]);
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pIsg->add_data(sco[j],pdoc[j],0.0);
   }
   pIsg->dim1();

   //Update pdoc array
   pIsg->extend_1df();
   if(pflag)cout << "Average p " << pIsg->avg() << \
        " Information " << pIsg->info() << endl;
   deps=1.0-epsilon;
   sum=0;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx=pIsg->val_1df(sco[j]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      pdoc[j]*=sqrt((1.0-xx)/xx);
      sum+=pdoc[j];
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx=pIsg->val_1df(sco[j]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      pdoc[j]*=sqrt(xx/(1.0-xx));
      sum+=pdoc[j];
   }
   zprod=zprod*sum;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]/=sum;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]/=sum;
   }

   //Update tr_sxx
   for(i=0;i<tst->ix;i++){
      j=tst->idx[i];
      xx=pIsg->val_1df(sco[j]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      ts_sxx[j]+=0.5*log(xx/(1.0-xx));
   }
}

void GBoost2::update(float *sco){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum,deps;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   j=gdd->idx[0];
   dmax=dmin=(double)sco[j];
   for(i=1;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx=(double)sco[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx=(double)sco[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   if(pflag)cout << "Min value: " << dmin << "; Max value: " << dmax << endl;

   if(pIsg)delete pIsg;
   pIsg=new Isgrid;
   pIsg->set_xdom(dmin,dmax);
   pIsg->set_xgran(grn);
   pIsg->init1();
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pIsg->add_data((double)sco[j],pdoc[j],pdoc[j]);
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pIsg->add_data((double)sco[j],pdoc[j],0.0);
   }
   pIsg->dim1();

   //Update pdoc array
   pIsg->extend_1df();
   if(pflag)cout << "Average p " << pIsg->avg() << \
        " Information " << pIsg->info() << endl;
   deps=1.0-epsilon;
   sum=0;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx=pIsg->val_1df((double)sco[j]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      pdoc[j]*=sqrt((1.0-xx)/xx);
      sum+=pdoc[j];
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx=pIsg->val_1df((double)sco[j]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      pdoc[j]*=sqrt(xx/(1.0-xx));
      sum+=pdoc[j];
   }
   zprod=zprod*sum;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]/=sum;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]/=sum;
   }

   //Update tr_sxx
   for(i=0;i<tst->ix;i++){
      j=tst->idx[i];
      xx=pIsg->val_1df((double)sco[j]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      ts_sxx[j]+=0.5*log(xx/(1.0-xx));
   }
}

void GBoost2::update_store(double *sco,const char *nam){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum,deps;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   j=gdd->idx[0];
   dmax=dmin=sco[j];
   for(i=1;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx=sco[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx=sco[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   if(pflag)cout << "Min value: " << dmin << "; Max value: " << dmax << endl;

   if(pIsg)delete pIsg;
   pIsg=new Isgrid;
   pIsg->set_xdom(dmin,dmax);
   pIsg->set_xgran(grn);
   pIsg->init1();
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pIsg->add_data(sco[j],pdoc[j],pdoc[j]);
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pIsg->add_data(sco[j],pdoc[j],0.0);
   }
   pIsg->dim1();
   pIsg->set_name(nam);
   pIsg->write_1df();

   //Update pdoc array
   pIsg->extend_1df();
   if(pflag)cout << "Average p " << pIsg->avg() << \
      " Information " << pIsg->info() << endl;
   deps=1.0-epsilon;
   sum=0;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx=pIsg->val_1df(sco[j]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      pdoc[j]*=sqrt((1.0-xx)/xx);
      sum+=pdoc[j];
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx=pIsg->val_1df(sco[j]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      pdoc[j]*=sqrt(xx/(1.0-xx));
      sum+=pdoc[j];
   }
   zprod=zprod*sum;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]/=sum;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]/=sum;
   }

   //Update tr_sxx
   for(i=0;i<tst->ix;i++){
      j=tst->idx[i];
      xx=pIsg->val_1df(sco[j]);
      if(xx<epsilon)xx=epsilon;
      if(xx>deps)xx=deps;
      ts_sxx[j]+=0.5*log(xx/(1.0-xx));
   }
}

//ABoost
ABoost::ABoost(long trd,long tsd) {
   tr_doc=trd;
   ts_doc=tsd;
   mark=NULL;
   pdoc=NULL;
   ts_sxx=NULL;
}

ABoost::~ABoost() {
   if(mark!=NULL)delete [] mark;
   if(pdoc!=NULL)delete [] pdoc;
   if(ts_sxx!=NULL)delete [] ts_sxx;
}

void ABoost::init(Index *gind){
   long i,j;
   double xx=1.0/((double)tr_doc);

   if(mark!=NULL)delete [] mark;
   mark=new long[tr_doc];
   if(pdoc!=NULL)delete [] pdoc;
   pdoc=new double[tr_doc];

   for(i=0;i<tr_doc;i++){
      mark[i]=0;
      pdoc[i]=xx;
   }
   for(i=0;i<gind->ix;i++)mark[*(gind->idx+i)]=1;

   if(ts_sxx!=NULL)delete [] ts_sxx;
   ts_sxx=new double[ts_doc];

   for(i=0;i<ts_doc;i++){
      ts_sxx[i]=0.0;
   }
   t=0;
   zprod=(double)tr_doc;
}

double ABoost::update(double *trs,double *tss){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum;
   double ax,bx,mx;
   tr_sco=trs;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   dmax=dmin=0.0;
   for(i=1;i<tr_doc;i++){
      if(mark[i])xx=trs[i];
      else xx=-trs[i];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   if(dmax==0.0) { cout <<"Max is zero"<<endl; exit(0);}
   ax=300.0/(-dmax);
   if(dmin==0.0) { cout <<"Min is zero"<<endl; exit(0);}
   bx=300.0/(-dmin);

   while(bx-ax>EPS){
      mx=(ax+bx)/2.0; /*midpoint*/
      xx=Z_alpha(mx);
      if(xx>0.0)bx=mx;
      else ax=mx;
   }

   //Update pdoc array
   sum=0;
   for(i=0;i<tr_doc;i++){
      if(mark[i])pdoc[i]*=exp(-ax*trs[i]);
      else pdoc[i]*=exp(ax*trs[i]);
      sum+=pdoc[i];
   }
   zprod=zprod*sum;
   for(i=0;i<tr_doc;i++){
      pdoc[i]/=sum;
   }

   //Update tr_sxx
   for(i=0;i<ts_doc;i++){
      ts_sxx[i]+=ax*tss[i];
   }
   return(ax);
}

double ABoost::update(double *trs){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum;
   double ax,bx,mx;
   tr_sco=trs;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   dmax=dmin=0.0;
   for(i=1;i<tr_doc;i++){
      if(mark[i])xx=trs[i];
      else xx=-trs[i];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   if(dmax==0.0) { cout <<"Max is zero"<<endl; exit(0);}
   ax=300.0/(-dmax);
   if(dmin==0.0) { cout <<"Min is zero"<<endl; exit(0);}
   bx=300.0/(-dmin);

   while(bx-ax>EPS){
      mx=(ax+bx)/2.0; /*midpoint*/
      xx=Z_alpha(mx);
      if(xx>0.0)bx=mx;
      else ax=mx;
   }

   //Update pdoc array
   sum=0;
   for(i=0;i<tr_doc;i++){
      if(mark[i])pdoc[i]*=exp(-ax*trs[i]);
      else pdoc[i]*=exp(ax*trs[i]);
      sum+=pdoc[i];
   }
   zprod=zprod*sum;
   for(i=0;i<tr_doc;i++){
      pdoc[i]/=sum;
   }

   return(ax);
}

double ABoost::Z_alpha(double alp){
   long i,j;
   double xx=0.0;

   for(i=0;i<tr_doc;i++){
      if(mark[i])xx-=pdoc[i]*tr_sco[i]*exp(-alp*tr_sco[i]);
      else xx+=pdoc[i]*tr_sco[i]*exp(alp*tr_sco[i]);
   }
   return(xx);
}

//ABoost2

ABoost2::ABoost2(long nd,Index *gd,Index *bd,Index *ts){
   ndoc=nd;
   pdoc=NULL;
   ts_sxx=NULL;
   gdd=gd;
   bdd=bd;
   tst=ts;
   tdoc=gdd->ix+bdd->ix;
}

ABoost2::~ABoost2() {
   if(pdoc!=NULL)delete [] pdoc;
   if(ts_sxx!=NULL)delete [] ts_sxx;
}

void ABoost2::init(void){
   long i,j;

   double xx=1.0/((double)tdoc);

   if(pdoc!=NULL)delete [] pdoc;
   pdoc=new double[ndoc];
   for(i=0;i<ndoc;i++){
      pdoc[i]=xx;
   }

   if(ts_sxx!=NULL)delete [] ts_sxx;
   ts_sxx=new double[ndoc];

   for(i=0;i<ndoc;i++){
      ts_sxx[i]=0.0;
   }
   t=0;
   zprod=(double)tdoc;
}

double ABoost2::update(double *scx){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum;
   double ax,bx,mx;
   sco=scx;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   j=gdd->idx[0];
   dmax=dmin=sco[j];
   for(i=1;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx=sco[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx=-sco[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }

   if(dmax==0.0) { cout <<"Max is zero"<<endl; exit(0);}
   ax=300.0/(-dmax);
   if(dmin==0.0) { cout <<"Min is zero"<<endl; exit(0);}
   bx=300.0/(-dmin);

   while(bx-ax>EPS){
      mx=(ax+bx)/2.0; /*midpoint*/
      xx=Z_alpha(mx);
      if(xx>0.0)bx=mx;
      else ax=mx;
   }

   //Update pdoc array
   sum=0;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]*=exp(-ax*sco[j]);
      sum+=pdoc[j];
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]*=exp(ax*sco[j]);
      sum+=pdoc[j];
   }
   zprod=zprod*sum;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]/=sum;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]/=sum;
   }

   //Update tr_sxx
   for(i=0;i<tst->ix;i++){
      j=tst->idx[i];
      ts_sxx[j]+=ax*sco[j];
   }
   return(ax);
}

double ABoost2::update(float *scx){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum;
   double ax,bx,mx;
   scf=scx;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   j=gdd->idx[0];
   dmax=dmin=scf[j];
   for(i=1;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx=(double)scf[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx=-(double)scf[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }

   if(dmax==0.0) { cout <<"Max is zero"<<endl; exit(0);}
   ax=300.0/(-dmax);
   if(dmin==0.0) { cout <<"Min is zero"<<endl; exit(0);}
   bx=300.0/(-dmin);

   while(bx-ax>EPS){
      mx=(ax+bx)/2.0; /*midpoint*/
      xx=Z_alphf(mx);
      if(xx>0.0)bx=mx;
      else ax=mx;
   }

   //Update pdoc array
   sum=0;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]*=exp(-ax*scf[j]);
      sum+=pdoc[j];
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]*=exp(ax*scf[j]);
      sum+=pdoc[j];
   }
   zprod=zprod*sum;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]/=sum;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]/=sum;
   }

   //Update tr_sxx
   for(i=0;i<tst->ix;i++){
      j=tst->idx[i];
      ts_sxx[j]+=ax*scf[j];
   }
   return(ax);
}

double ABoost2::Z_alpha(double alp){
   long i,j;
   double xx=0.0;

   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx-=pdoc[j]*sco[j]*exp(-alp*sco[j]);
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx+=pdoc[j]*sco[j]*exp(alp*sco[j]);
   }
   return(xx);
}

double ABoost2::Z_alphf(double alp){
   long i,j;
   double xx=0.0;

   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx-=pdoc[j]*scf[j]*exp(-alp*scf[j]);
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx+=pdoc[j]*scf[j]*exp(alp*scf[j]);
   }
   return(xx);
}

//ABoost3

ABoost3::ABoost3(long nd,Index *gd,Index *bd,Index *ts){
   ndoc=nd;
   pdoc=NULL;
   ts_sxx=NULL;
   gdd=gd;
   bdd=bd;
   tst=ts;
   tdoc=gdd->ix+bdd->ix;
}

ABoost3::~ABoost3() {
   if(pdoc!=NULL)delete [] pdoc;
   if(ts_sxx!=NULL)delete [] ts_sxx;
}

void ABoost3::init(void){
   long i,j;

   double xx=1.0/((double)tdoc);

   if(pdoc!=NULL)delete [] pdoc;
   pdoc=new double[ndoc];
   for(i=0;i<ndoc;i++){
      pdoc[i]=xx;
   }

   if(ts_sxx!=NULL)delete [] ts_sxx;
   ts_sxx=new double[ndoc];

   for(i=0;i<ndoc;i++){
      ts_sxx[i]=0.0;
   }
   t=0;
   zprod=(double)tdoc;
}

double ABoost3::update(double *scx){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum,ax,bx;
   double hna,hnb,alphao,betao,lambda=0.0001;
   sco=scx;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   j=gdd->idx[0];
   dmax=dmin=sco[j];
   for(i=1;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx=sco[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx=-sco[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }

   if(dmax==0.0) { cout <<"Max is zero"<<endl; exit(0);}
   ax=300.0/(-dmax);
   if(dmin==0.0) { cout <<"Min is zero"<<endl; exit(0);}
   bx=300.0/(-dmin);

   beta=0;
   hna=bx/5.0;
   alpha=find_alpha(0,hna);
   hnb=30.0;
   beta=find_beta(0,hnb);
   alphao=0;
   betao=0;
   cout << "alpha " << alpha << " beta " << beta << endl;
   while((fabs(alpha-alphao)>lambda)||(fabs(beta-betao)>lambda)){
      alphao=alpha;
      betao=beta;
      alpha=find_alpha(alpha,hna);
      beta=find_beta(beta,hnb);
      cout << "alpha " << alpha << " beta " << beta << endl;
   }
   //Update pdoc array
   sum=0;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]*=exp(-alpha*sco[j]+beta);
      sum+=pdoc[j];
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]*=exp(alpha*sco[j]-beta);
      sum+=pdoc[j];
   }
   zprod=zprod*sum;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]/=sum;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]/=sum;
   }

   //Update tr_sxx
   for(i=0;i<tst->ix;i++){
      j=tst->idx[i];
      ts_sxx[j]+=alpha*sco[j]-beta;
   }
   return(alpha);
}

double ABoost3::find_alpha(double alp,double &hnv){
   int flag=1,ct=0;
   double at,bt,mt,xx;
   at=alp-hnv;
   if(Z_alpha(at)>0){
      while(Z_alpha(at-hnv)>0){at-=hnv;ct++;}
      at-=hnv;
      bt=at+hnv;
      flag=0;
   }
   if(flag)bt=alp+hnv;
   if(flag&&(Z_alpha(bt)<0)){
      while(Z_alpha(bt+hnv)<0){bt+=hnv;ct++;}
      bt+=hnv;
      at=bt-hnv;
   }
   if(!ct)hnv=hnv/2.0;
   while(bt-at>EPS){
      mt=(at+bt)/2.0; /*midpoint*/
      xx=Z_alpha(mt);
      if(xx>0.0)bt=mt;
      else at=mt;
   }
   return(at);
}

double ABoost3::find_beta(double bet,double &hnv){
   int flag=1,ct=0;
   double at,bt,mt,xx;
   at=bet-hnv;
   if(Z_beta(at)>0){
      while(Z_beta(at-hnv)>0){at-=hnv;ct++;}
      at-=hnv;
      bt=at+hnv;
      flag=0;
   }
   if(flag)bt=bet+hnv;
   if(flag&&(Z_beta(bt)<0)){
      while(Z_beta(bt+hnv)<0){bt+=hnv;ct++;}
      bt+=hnv;
      at=bt-hnv;
   }
   if(!ct)hnv=hnv/2.0;
   while(bt-at>EPS){
      mt=(at+bt)/2.0; /*midpoint*/
      xx=Z_beta(mt);
      if(xx>0.0)bt=mt;
      else at=mt;
   }
   return(at);
}

double ABoost3::update(float *scx){
   int pflag=get_qflag();
   long i,j,k;
   double dmax,dmin,xx,sum;
   double ax,bx,mx;
   scf=scx;

   t++;
   if(pflag)cout << t << " Iteration" << endl;

   j=gdd->idx[0];
   dmax=dmin=scf[j];
   for(i=1;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx=(double)scf[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx=-(double)scf[j];
      dmax=xx>dmax?xx:dmax;
      dmin=xx<dmin?xx:dmin;
   }

   if(dmax==0.0) { cout <<"Max is zero"<<endl; exit(0);}
   ax=300.0/(-dmax);
   if(dmin==0.0) { cout <<"Min is zero"<<endl; exit(0);}
   bx=300.0/(-dmin);

   while(bx-ax>EPS){
      mx=(ax+bx)/2.0; /*midpoint*/
      xx=Z_alphf(mx);
      if(xx>0.0)bx=mx;
      else ax=mx;
   }

   //Update pdoc array
   sum=0;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]*=exp(-ax*scf[j]);
      sum+=pdoc[j];
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]*=exp(ax*scf[j]);
      sum+=pdoc[j];
   }
   zprod=zprod*sum;
   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      pdoc[j]/=sum;
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      pdoc[j]/=sum;
   }

   //Update tr_sxx
   for(i=0;i<tst->ix;i++){
      j=tst->idx[i];
      ts_sxx[j]+=ax*scf[j];
   }
   return(ax);
}

double ABoost3::Z_alpha(double alp){
   long i,j;
   double xx=0.0;

   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx-=pdoc[j]*sco[j]*exp(-alp*sco[j]+beta);
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx+=pdoc[j]*sco[j]*exp(alp*sco[j]-beta);
   }
   return(xx);
}

double ABoost3::Z_beta(double bet){
   long i,j;
   double xx=0.0;

   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx+=pdoc[j]*exp(-alpha*sco[j]+bet);
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx-=pdoc[j]*exp(alpha*sco[j]-bet);
   }
   return(xx);
}

double ABoost3::Z_alphf(double alp){
   long i,j;
   double xx=0.0;

   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx-=pdoc[j]*scf[j]*exp(-alp*scf[j]-beta);
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx+=pdoc[j]*scf[j]*exp(alp*scf[j]+beta);
   }
   return(xx);
}

double ABoost3::Z_betf(double bet){
   long i,j;
   double xx=0.0;

   for(i=0;i<gdd->ix;i++){
      j=gdd->idx[i];
      xx+=pdoc[j]*exp(-alpha*scf[j]-bet);
   }
   for(i=0;i<bdd->ix;i++){
      j=bdd->idx[i];
      xx-=pdoc[j]*exp(alpha*scf[j]+bet);
   }
   return(xx);
}

}
