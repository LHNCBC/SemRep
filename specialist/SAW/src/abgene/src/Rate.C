#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <cassert>
#include "runn.h"
#include "Btree.h"
#include "Docum.h"
#include "Vnab.h"
#include "Rate.h"
using namespace std;
namespace iret {

static float *scx;

//To be followed by cset function.
Rate::Rate(char *nam){
   int len=strlen(nam);
   name=new char[len+1];
   strcpy(name,nam);

   nj=0;
   ix=NULL;
   sel=NULL;
   pl1=NULL;
   pl2=NULL;
   jpc=NULL;
   fx1=NULL;
   fx2=NULL;
   dnum=NULL;
   jpath=NULL;
}

//Function for general testing purposes.
void Rate::cset(int smooth,long num_q,long doc_pq,long s_size){
   long i;
   smflag=smooth;

   num_queries=num_q;
   docs_per_query=doc_pq;
   set_size=s_size;

   sel=new long[num_queries];

   pl1=(float **)new long[num_queries];
   pl2=(float **)new long[num_queries];
   for(i=0;i<num_queries;i++){
      *(pl1+i)=new float[docs_per_query];
      *(pl2+i)=new float[docs_per_query];
   }

   fx1=new float[docs_per_query];
   fx2=new float[docs_per_query];
}

//Function to use with standard test sets.
void Rate::cset(int smooth){
   int len,i;
   char cnam[256];

   num_queries=NUM_QUERIES;
   docs_per_query=DOCS_PER_QUERY;
   set_size=DOCS_PER_QUERY;

   dnum=new long[docs_per_query];

   smflag=smooth;

   get_pathw(cnam,"rateset",name,"j");
   ifstream fin(cnam,ios::in);
   fin >> nj;
   fin.get();

   jpath=(char **)new long[nj];
   for(i=0;i<nj;i++){
      len=get_string(cnam,fin,'\n');
      *(jpath+i)=new char[len+1];
      strcpy(*(jpath+i),cnam);
   }
   fin.close();

   ix =new int[nj];
   sel=new long[num_queries];

   pl1=(float **)new long[num_queries];
   pl2=(float **)new long[num_queries];
   jpc=(float **)new long[num_queries];
   for(i=0;i<num_queries;i++){
      *(pl1+i)=new float[docs_per_query];
      *(pl2+i)=new float[docs_per_query];
      *(jpc+i)=new float[docs_per_query];
   }

   fx1=new float[docs_per_query];
   fx2=new float[docs_per_query];
}

Rate::~Rate(){
   long i;
   delete [] name;

   if(dnum!=NULL)delete [] dnum;

   if(ix!=NULL)delete [] ix;
   if(sel!=NULL)delete [] sel;

   if(jpath!=NULL){
      for(i=0;i<nj;i++){
         delete [] *(jpath+i);
      }
      delete [] jpath;
   }
   if(pl1!=NULL){
      for(i=0;i<num_queries;i++){
         delete [] *(pl1+i);
      }
      delete [] pl1;
   }
   if(pl2!=NULL){
      for(i=0;i<num_queries;i++){
         delete [] *(pl2+i);
      }
      delete [] pl2;
   }
   if(jpc!=NULL){
      for(i=0;i<num_queries;i++){
         delete [] *(jpc+i);
      }
      delete [] jpc;
   }
  
   if(fx1!=NULL)delete [] fx1;
   if(fx2!=NULL)delete [] fx2;
}

void Rate::debug(int query){
   long i;
   cout << "Smooth flag= " << smflag << endl << endl;
   cout << "Current query set for " << qnum << endl;
   for(i=0;i<docs_per_query;i++)cout << " " << *(dnum+i) << endl;

   cout << "nj = " << nj;
   for(i=0;i<nj;i++)cout << " " << *(jpath+i) << endl;

   cout << "Precisions for query number = " << query << endl;
   for(i=0;i<docs_per_query;i++){
      cout << *(*(pl1+query)+i) << " " << *(*(pl2+query)+i) << " " << *(*(jpc+query)+i) << endl;
   }

   cout << endl << "Average precisions " << endl;
   for(i=0;i<docs_per_query;i++){
      cout << *(fx1+i) << " " << *(fx2+i) << endl;
   }
}

void Rate::change_name(char *nam){
   int len=strlen(nam);
   name=new char[len+1];
   strcpy(name,nam);
}

void Rate::query_fill(ifstream &fqry){
   for(long i=0;i<docs_per_query;i++){
      fqry >> qnum >> *(dnum+i);
   }
}

void Rate::construct_ifile(Docum &Doc){
   char cnam[256],anum[20],bnum[20];
   int pflag=get_qflag();
   long i,j,k,m;

   Str_str Snum;
   get_pathw(cnam,"docset",Doc.name,"u");
   ifstream fin(cnam,ios::in);
   for(i=0;i<Doc.ndoc;i++){
      fin >> k;
      long_str(anum,i);
      long_str(bnum,k);
      Snum.add_pair(bnum,anum);
   }
   fin.close();

   get_pathw(cnam,"rateset",name,"i");
   ofstream ftt(cnam,ios::out);
   get_pathw(cnam,"rateset",name,"u");
   fin.open(cnam,ios::in);
   for(i=0;i<num_queries;i++){
      query_fill(fin);
      long_str(anum,qnum);
      str_long(Snum.match(anum),j);
      for(m=0;m<docs_per_query;m++){
         long_str(anum,*(dnum+m));
         str_long(Snum.match(anum),k);
         ftt << j << " " << k << endl;
      }
   }
   fin.close();
   ftt.close();
}

void Rate::create_iret_ret(Vnab &Vnb,float (*q_local)(int,long),char *iret_name){
   char cnam[256],anum[20],bnum[20];
   int pflag=get_qflag();
   long j;
   float *fxx=new float[docs_per_query];
   
   get_pathw(cnam,"rateset",name,"i");
   ifstream fin(cnam,ios::in);
   get_pathw(cnam,"iretset",name,iret_name);
   ofstream ftt(cnam,ios::out);
   
   for(long i=0;i<num_queries;i++){
      this->query_fill(fin);
      Vnb.Score(qnum,q_local);
      for(j=0;j<docs_per_query;j++)*(fxx+j)=*(Vnb.sxx+dnum[j]);
      hRort(docs_per_query,fxx,dnum);

      ftt << qnum << endl;
      for(j=0;j<docs_per_query;j++){
	ftt << "   " << *(dnum+j) << " " << *(fxx+j) << endl;
      }
      mark(pflag,i+1,1,"query");
   }
   ftt.close();
   fin.close();
}

void Rate::set_ix(int tt){
   for(int j=0;j<nj;j++){
      if(j<tt)*(ix+j)=1;
      else *(ix+j)=0;
   }
}

int Rate::next_ix(int tt){
   int i,k=nj-1,flag=1;
   while(ix[k]==0)k--;
   if(k<nj-1){
      ix[k]=0;
      ix[k+1]=1;
   }
   else {
      int u=0;
      while(ix[k]){k--;u++;}
      if(u==tt)flag=0;
      else {
         while(ix[k]==0)k--;
         ix[k]=0;
         ix[k+1]=1;
         for(i=k+2;i<k+2+u;i++)ix[i]=1;
         for(i=k+2+u;i<nj;i++)ix[i]=0;
      }
   }
return(flag);
}

void Rate::invert_ix(void){
   for(int i=0;i<nj;i++){
      if(ix[i])ix[i]=0;
      else ix[i]=1;
   }
}

void Rate::mak_jpc(void)
{
long i,j,k;
char cnam[256];
ifstream fin;
float xnm,xdm,xn;

for(i=0;i<num_queries;i++)
{
  for(j=0;j<docs_per_query;j++)
    {
      *(*(jpc+i)+j)=0;
    }
}

xn=0;
for(k=0;k<nj;k++)
{
  if(*(ix+k))
    {
      xn++;
      fin.open(*(jpath+k),ios::in);
      if(fin==NULL){cout << "Error in file open!" << endl;}
      for(i=0;i<num_queries;i++)
	{
	  for(j=0;j<docs_per_query;j++)
	    {
	      fin >> xnm >> xdm;
	      (*(*(jpc+i)+j))+=xnm/xdm;
            }
	}
      fin.close();
    }
}
for(i=0;i<num_queries;i++)
{
  for(j=0;j<docs_per_query;j++)
    {
      *(*(jpc+i)+j)=*(*(jpc+i)+j)/xn;
    }
}
}

void Rate::create_iret_jpc(const char *iret_name){
   long i,j,k;
   int pflag=get_qflag();
   char cnam[256];
   ifstream fin;
   long *ord=new long[docs_per_query];
   
   for(i=0;i<docs_per_query;i++)*(ord+i)=i;
   get_pathw(cnam,"rateset",name,"i");
   fin.open(cnam,ios::in);
   get_pathw(cnam,"iretset",name,iret_name);
   ofstream ftt(cnam,ios::out);
   
   for(i=0;i<num_queries;i++){
      this->query_fill(fin);
      scx=*(jpc+i);
      hRort(docs_per_query,scx,dnum);
      ftt << qnum << endl;
      for(j=0;j<docs_per_query;j++){
         ftt << " " << *(dnum+j) << " " << *(scx+j) << endl;
      }
      mark(pflag,i+1,10,"query sets");
   }
}

static int fcomp(const void *s, const void *t)
{
if(*(scx+*((long *)s))<*(scx+*((long *)t)))return(1);
else if (*(scx+*((long *)s))>*(scx+*((long *)t)))return(-1);
else return(0);
}

//General load for testing.
void Rate::gen_load(int sflag,char *iret_name){
   long i,j,k,m,n;
   int pflag=get_qflag(),len;
   float **ptt,*pt,xn,xs,xv,pr,sm=0;
   char cnam[256],*pch;
   
   if(sflag==1)ptt=pl1;
   else ptt=pl2;

   get_pathw(cnam,name,iret_name,"rnk");
   ifstream fin(cnam,ios::in);

   for(i=0;i<num_queries;i++){
      fin >> m >> n;
      for(j=0;j<docs_per_query;j++){
         fin >> *(*(ptt+i)+j);
      }
   }
   fin.close();

   len=strlen(iret_name);
   pch=new char[len+1];
   strcpy(pch,iret_name);
   if(sflag==1)iret1=pch;
   else iret2=pch;
}

//Special load for standard test sets.
void Rate::prec_load(int sflag,const char *iret_name){
   long i,j,k,m,n;
   int pflag=get_qflag(),len;
   float **ptt,*pt,xn,xs,xv,pr,sm=0;
   long *idx=new long[docs_per_query];
   float *scr=new float[docs_per_query];
   float *prc=new float[docs_per_query];
   char cnam[256],*pch;
   
   if(sflag==1)ptt=pl1;
   else ptt=pl2;

   get_pathw(cnam,"rateset",name,"i");
   ifstream fin(cnam,ios::in);
   get_pathw(cnam,"iretset",name,iret_name);
   ifstream fir(cnam,ios::in);

   for(i=0;i<num_queries;i++){
      this->query_fill(fin);
      fir >> k;
      if(qnum!=k){cout << "Error in read at " << qnum << " " << k << ", latter from iret!" << endl;exit(0);}
      for(j=0;j<docs_per_query;j++){
         fir >> *(idx+j) >> *(scr+j);
      }
      for(j=0;j<docs_per_query;j++){
         m=0;
         n=*(idx+j);
         while((m<docs_per_query)&&(*(dnum+m)!=n))m++;
         if(m==docs_per_query){cout << "Error, dnum not found at query= " << qnum << " , idx= " << n << endl;exit(0);}
         *(prc+j)=*(*(jpc+i)+m);
      }

      //Levels ambigous values.
      pt=*(ptt+i);
      xv=*(scr);
      xs=*(prc);
      xn=1.0;
      k=j=0;
      while(k<docs_per_query){
         k++;
         while((k<docs_per_query)&&(xv==*(scr+k))){
            xs+=*(prc+k);
            xn++;
            k++;
         }
         pr=xs/xn;
         for(n=j;n<k;n++)*(pt+n)=pr;
         if(k<docs_per_query){
            xv=*(scr+k);
            xs=*(prc+k);
            xn=1.0;
            j=k;
         }
      }
      mark(pflag,i+1,10,"query sets");
   }

   len=strlen(iret_name);
   pch=new char[len+1];
   strcpy(pch,iret_name);
   if(sflag==1)iret1=pch;
   else iret2=pch;
   delete [] idx;
   delete [] scr;
   delete [] prc;
}

//Special load for standard test sets.
void Rate::prec_load_pmid(int sflag,const char *iret_name){
   long i,j,k,m,n;
   int pflag=get_qflag(),len;
   float **ptt,*pt,xn,xs,xv,pr,sm=0;
   long *idx=new long[docs_per_query];
   float *scr=new float[docs_per_query];
   float *prc=new float[docs_per_query];
   char cnam[256],*pch;
   
   if(sflag==1)ptt=pl1;
   else ptt=pl2;

   get_pathw(cnam,"rateset",name,"u");
   ifstream fin(cnam,ios::in);
   get_pathw(cnam,"iretset",name,iret_name);
   ifstream fir(cnam,ios::in);

   for(i=0;i<num_queries;i++){
      this->query_fill(fin);
      fir >> k;
      if(qnum!=k){cout << "Error in read at " << qnum << " " << k << ", latter from iret!" << endl;exit(0);}
      for(j=0;j<docs_per_query;j++){
         fir >> *(idx+j) >> *(scr+j);
      }
      for(j=0;j<docs_per_query;j++){
         m=0;
         n=*(idx+j);
         while((m<docs_per_query)&&(*(dnum+m)!=n))m++;
         if(m==docs_per_query){cout << "Error, dnum not found at query= " << qnum << " , idx= " << n << endl;exit(0);}
         *(prc+j)=*(*(jpc+i)+m);
      }

      //Levels ambigous values.
      pt=*(ptt+i);
      xv=*(scr);
      xs=*(prc);
      xn=1.0;
      k=j=0;
      while(k<docs_per_query){
         k++;
         while((k<docs_per_query)&&(xv==*(scr+k))){
            xs+=*(prc+k);
            xn++;
            k++;
         }
         pr=xs/xn;
         for(n=j;n<k;n++)*(pt+n)=pr;
         if(k<docs_per_query){
            xv=*(scr+k);
            xs=*(prc+k);
            xn=1.0;
            j=k;
         }
      }
      mark(pflag,i+1,10,"query sets");
   }

   len=strlen(iret_name);
   pch=new char[len+1];
   strcpy(pch,iret_name);
   if(sflag==1)iret1=pch;
   else iret2=pch;
   delete [] idx;
   delete [] scr;
   delete [] prc;
}

void Rate::str_sel(void){
   for(long i=0;i<num_queries;i++)*(sel+i)=i;
}

void Rate::ran_sel(void){
   for(long i=0;i<num_queries;i++)*(sel+i)=zrand(num_queries);
}

void Rate::ave_val(int sflag){
   long i,j,k;
   float **ptt,*pt;
   float dm;

   if(sflag==1){ptt=pl1;pt=fx1;}
   else {ptt=pl2;pt=fx2;}

   for(j=0;j<docs_per_query;j++){
      *(pt+j)=0;
   }

   for(i=0;i<num_queries;i++){
      k=*(sel+i); 
      for(j=0;j<docs_per_query;j++){
         (*(pt+j))+=*(*(ptt+k)+j);
      }
   }

   dm=(float)num_queries;

   for(j=0;j<docs_per_query;j++){
      *(pt+j)=*(pt+j)/dm;
   }
}   

void Rate::ind_val(int sflag,int n){
   long i,j;
   float **ptt,*pt;
   
   if(sflag==1){ptt=pl1;pt=fx1;}
   else {ptt=pl2;pt=fx2;}

   for(j=0;j<docs_per_query;j++){
      (*(pt+j))=*(*(ptt+n)+j);
   }
}

void Rate::smooth(int sflag){
   float *pt;
   int n=docs_per_query;

   if(sflag==1){pt=fx1;}
   else {pt=fx2;}

   long k=0,i,j;
   float *fn,xx,yy,zz,rt;
   while(k<n){
      xx=zz=0;
      fn=pt+k;
      for(i=0;i<n-k;i++){
         yy=xx+*(fn++);
         rt=yy/((float)(i+1));
         if(zz<=rt){zz=rt;j=i;}
         xx=yy;
      }
      for(i=0;i<j+1;i++)*(pt+k+i)=zz;
      k+=j+1;
   }
}

void Rate::gopen_write(ios_base::openmode nMode){
   char cnam[256];

   get_pathw(cnam,"iretset",name,"r");
   fout.open(cnam,nMode);
}

void Rate::print_prec(int sflag){
   float *pt;
   char cnam[256],*pch;
   
   if(smflag)this->smooth(sflag);

   if(sflag==1){pt=fx1;pch=iret1;}
   else {pt=fx2;pch=iret2;}

   get_pathw(cnam,"iretset",name,pch);
   fout << "Precisions for top " << docs_per_query << " ranks of " << cnam << endl;

   float vv,xx=0;
   for(long i=0;i<docs_per_query;i++){
      vv=*(pt+i);
      xx+=vv;
      fout << " " << i+1 << " " << vv << " " << xx/((float)(i+1)) << endl;
   }
   fout << "Expected relevant documents per query = " << xx << endl << endl;
}

void Rate::print_meas(int sflag,int nr){
   float *pt;
   char cnam[256],*pch;
   
   if(smflag)this->smooth(sflag);

   if(sflag==1){pt=fx1;pch=iret1;}
   else {pt=fx2;pch=iret2;}

   get_pathw(cnam,"iretset",name,pch);
   fout << "Precision & information for top " << nr << " ranks of " << cnam << endl;

   float zz;
   float p=compt(nr,pt,zz);

   fout << "precision " << p << "  information " << zz << endl << endl;
   if(sflag==1){pr1=p;if1=zz;}
   else {pr2=p;if2=zz;}
}

void Rate::print_clim(int sflag,int nr,float sig,long sn){
   int pflag=get_qflag();
   float *pt;
   long seed;
   char cnam[256],*pch;


   seed=gseed(0,NULL,"-seed");
   if(sflag==1){pt=fx1;pch=iret1;}
   else {pt=fx2;pch=iret2;}

   get_pathw(cnam,"iretset",name,pch);
   fout << "Confidence limits based on top " << nr << " ranks of " << cnam << endl;

   float *sx=new float[sn];
   float *px=new float[sn];
   for(long i=0;i<sn;i++){
      this->ran_sel();
      this->ave_val(sflag);
      if(smflag)this->smooth(sflag);
      *(px+i)=compt(nr,pt,*(sx+i));
      mark(pflag,i+1,100,"iterations");
   }
   float lw,hg;
   confid(sig,sn,px,lw,hg);
   fout << "Significance level " << sig << endl;
   fout <<  "Confidence limits for precision: " 
        << lw << " " << hg << endl;
   confid(sig,sn,sx,lw,hg);
   fout << "Confidence limits for information: "
        << lw << " " << hg << endl;
   fout << "Based on " << sn << " resamplings of the data." << endl;
   fout << "Random seed used was " << seed << endl << endl;
}

void Rate::print_comp(int nr,float sig,long sn){
   int pflag=get_qflag();
   float *pt,pa,sa;
   long i,seed;
   char cnam[256],bnam[256];

   seed=gseed(0,NULL,"-seed");
   get_pathw(cnam,"iretset",name,iret1);
   get_pathw(bnam,"iretset",name,iret2);
   fout << "Confidence limits based on top " << nr << " ranks of " << endl;
   fout << "          " << cnam << " and " << bnam <<  endl;
   fout << "          using a paired sampling technique." << endl;
   
   float *sx=new float[sn];
   float *px=new float[sn];
   float x1,x2,z1,z2;
   for(i=0;i<sn;i++){
      this->ran_sel();
      this->ave_val(1);
      this->ave_val(2);
      if(smflag)this->smooth(1);
      if(smflag)this->smooth(2);
      x1=compt(nr,fx1,z1);
      x2=compt(nr,fx2,z2);
      *(px+i)=x1-x2;
      *(sx+i)=z1-z2;
      mark(pflag,i+1,100,"iterations");
   }

   pa=sa=0;
   for(i=0;i<sn;i++){
      pa+=*(px+i);
      sa+=*(sx+i);
   }
   pa=pa/((float)sn);
   sa=sa/((float)sn);

   this->str_sel();
   this->ave_val(1);
   this->ave_val(2);
   if(smflag)this->smooth(1);
   if(smflag)this->smooth(2);
   x1=compt(nr,fx1,z1);
   pr1=x1;
   if1=z1;
   x2=compt(nr,fx2,z2);
   pr2=x2;
   if2=z2;
   fout << "Significance level " << sig << endl;
   float lw,hg;
   fout <<  "Confidence limits for difference in precisions p1-p2 = " << x1-x2 << " :" << endl;
   confid(sig,sn,px,lw,hg);
   fout << "          " << lw-pa << " " << hg-pa << endl;
   fout <<  "Confidence limits for difference in information i1-i2 = " << z1-z2 << " :" << endl;
   confid(sig,sn,sx,lw,hg);
   fout << "          " << lw-sa << " " << hg-sa << endl;
   fout << "Based on " << sn << " paired resamplings of the data." << endl;
   fout << "Random seed used was " << seed << endl << endl;
}

void Rate::gclose_write(){
   fout.close();
}
 
float Rate::compt(int nr,float *fx,float &ri){
   long i;
   float vv,r=0,xx=0;
   ri=0;
   for(i=0;i<docs_per_query;i++)r+=*(fx+i);
   r=r/((float)set_size);
   for(i=0;i<nr;i++){
      vv=*(fx+i);
      xx+=vv;
      if(vv>r)ri+=vv*l2*log(vv/r);
   }
   return(xx/((float)nr));
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


void Rate::print_uplim(int nr){
   int pflag=get_qflag();
   char cnam[256];

   get_pathw(cnam,"rateset",name,"j");
 
   fout << "Upper limiting series from " << cnam << endl << endl;
  
   this->str_sel();
   float cp,ct,ci,xx;
   for(int i=1;i<nj+1;i++){
      cp=0;
      ci=0;
      ct=0;
      this->set_ix(i);
      do {
         this->mak_jpc();
         this->create_iret_jpc("cmb");
         this->prec_load(1,"cmb");
         this->ave_val(1);
         cp+=compt(nr,fx1,xx);
         ci+=xx;
         ct++;
      }while(this->next_ix(i));
      fout << "Panel size " << i << " ";
      fout << "ave. prec. " << cp/ct << " ";
      fout << "ave. info. " << ci/ct << " num. sets " << ct << endl;
      if(pflag)cout << "subset_size " << i << " # " << ct << endl;
   }
}

void Rate::print_lolim(int nr){
   int pflag=get_qflag();
   char cnam[256];

   get_pathw(cnam,"rateset",name,"j");

   fout << "Lower limiting series from " << cnam << endl << endl;

   this->str_sel();
   float cp,ct,ci,xx;
   for(int i=1;i<nj;i++){
      cp=0;
      ci=0;
      ct=0;
      this->set_ix(i);
      do {
         this->mak_jpc();
         this->create_iret_jpc("cmb");
         this->invert_ix();
         this->mak_jpc();
         this->invert_ix();
         this->prec_load(1,"cmb");
         this->ave_val(1);
         cp+=compt(nr,fx1,xx);
         ci+=xx;
         ct++;
      }while(this->next_ix(i));
      fout << "Panel size " << i << " ";
      fout << "ave. prec. " << cp/ct << " ";
      fout << "ave. info. " << ci/ct << " num. sets " << ct << endl;
      if(pflag)cout << "subset_size " << i << " # " << ct << endl;
   }
}

void Rate::feat_val(long nm,Vterm *ptm,float pri,float &ps,float &pt,float &qt){
   long j,k,ig,ip;
   float xt,xr,xnt,xst,del=0.5;
   float rt,xx;

   xt=xr=0;
   for(j=0;j<50;j++){
      xt++;
      xr+=*(*(jpc+nm)+j);
   }
   Num_num Nm;
   for(j=0;j<50;j++){
      k=dnum[j];
      if(Nm.match(k)==LNEG)Nm.add_pair(k,j);
   }
   ig=ptm->post->ix;
   xnt=xst=0;
   for(k=0;k<ig;k++){
      ip=*(ptm->post->idx+k);
      if((j=Nm.match(ip))!=LNEG){
         xnt++;
         xst+=*(*(jpc+nm)+j);
      }
   }
   pt =(xst+del)/(xr+2*del);
   qt =(xnt-xst+del)/(xt-xr+2*del);
   rt =(xnt+del)/(xt+2*del);

   xx=-xst*log(rt/pt) - (xnt-xst)*log(rt/qt);
   xx-=(xr-xst)*log((1.0-rt)/(1.0-pt));
   xx-=(xt-xr-xnt+xst)*log((1.0-rt)/(1.0-qt));
   ps=1.0/(1.0+(1.0/pri-1.0)*exp(-xx));
}

Num_num *Rate::feat_xnum(long nm,Vterm *ptm,float &xt,float &xr,float &xnt,float &xst){
   long j,k,ig,ip;

   xt=xr=1.0;
   for(j=0;j<50;j++){
      xt++;
      xr+=*(*(jpc+nm)+j);
   }
   Num_num *pNm=new Num_num;
   for(j=0;j<50;j++){
      k=dnum[j];
      if(pNm->match(k)==LNEG)pNm->add_pair(k,j);
   }
   ig=ptm->post->ix;
   xnt=xst=1.0;
   for(k=0;k<ig;k++){
      ip=*(ptm->post->idx+k);
      if((j=pNm->match(ip))!=LNEG){
         xnt++;
         xst+=*(*(jpc+nm)+j);
      }
   }
   return(pNm);
}

float Rate::feat_wtt(long dn,Num_num *pNm,long nm,float yt,float yr,float ynt,float yst){
   long i,j;
   float xx,xt,xr,xnt,xst,del=0.5;
   float wt,pt,qt;

   xt=yt-1.0;
   j=pNm->match(dn);
   xr=yr-*(*(jpc+nm)+j);
   xnt=ynt-1.0;
   xst=yst-*(*(jpc+nm)+j);
   pt =(xst+del)/(xr+2*del);
   qt =(xnt-xst+del)/(xt-xr+2*del);
   if(pt<=qt)return(0);
   wt=log((pt*(1-qt))/ (qt*(1- pt)));
   return(wt);
}

}
