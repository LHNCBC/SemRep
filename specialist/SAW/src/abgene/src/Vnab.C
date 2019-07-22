#include <unistd.h>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include "runn.h"
#include "Btree.h"
#include "Docum.h"
#include "Post.h"
#include "Vnab.h"

double xttrc,xb,xa,xg;

using namespace std;
namespace iret {

Vterm::~Vterm(){
   delete post;
   delete [] cwgt;
}

Vnab::Vnab(Docum *dcc){
   long i;
   doc=dcc;
   ndoc=doc->ndoc;
   xttrc=(double)ndoc;
   sxx=new float[ndoc];
   vln=NULL;
}

Vnab::~Vnab(){
   Vterm *ptm;

   if(btr !=NULL){     //handle when btr is not assigned to Post::btr.
      btr->node_first();
      while(btr->node_next()){
         ptm=(Vterm *)(btr->give_ptr());
         ptm->~Vterm();
      }
      btr->~Btree();
   }

   if(sxx!=NULL)delete [] sxx;
}   

void Vnab::load(Post *pct,float (*d_local)(int,long),float (*global)(long)){
   int lc,pflag=get_qflag();
   Pdat *ppp,*qqq,*zzz;
   Vterm *ptm;
   long i,j,k,nm,*pm;
   char cnam[max_str];
   float gw,*cw;

   btr=pct->btr;
   pct->btr=NULL;
   get_pathw(cnam,"postset",pct->name,"f");
   ifstream fiq(cnam,ios::in|ios::binary);
 
   j=0;
   btr->node_first();
   while(btr->node_next()){
      fiq.read((char*)&nm,sizeof(long));
      qqq=(Pdat *)btr->give_ptr();
      ppp=qqq=qqq->pdt;
      ptm=new Vterm();
      ptm->post=new Index(nm);
      pm=ptm->post->idx;
      cw=ptm->cwgt=new float[nm];
      gw=ptm->gwgt=global(nm);
    
      btr->set_ptr((void *)ptm);
      i=0;
      do {
         k=pm[i]=qqq->num;
         lc=(int)(qqq->lnt);
         cw[i++]=d_local(lc,doc->alen[k])*gw;
         zzz=qqq->pdt;
         delete qqq;
         qqq=zzz;
      }while(qqq!=ppp);
      mark(pflag,++j,1000,"postings loaded");
   }    
   fiq.close();

   if(vln!=NULL)delete [] vln;
   vln=new float[ndoc];
   for(i=0;i<ndoc;i++)vln[i]=1.0;
}

void Vnab::veclen(float (*d_local)(int,long),float (*global)(long)){
   int pflag=get_qflag();
   long i,j,k,nmx,*idx;
   float *cwx,xx,gwx;
   Vterm *ptm;

   if(vln!=NULL)delete [] vln;
   vln=new float[ndoc];
   for(i=0;i<ndoc;i++)vln[i]=0;

   k=0;
   btr->node_first();
   while(btr->node_next()){
      ptm=(Vterm *)btr->give_ptr();
      idx=ptm->post->idx;
      nmx=ptm->post->ix;
      cwx=ptm->cwgt;
      gwx=global(nmx);
      for(i=0;i<nmx;i++){
         j=*(idx+i);
         xx=d_local(rnd(*(cwx+i)),doc->alen[j]);
         vln[j]+=xx*xx*gwx;
      }
      mark(pflag,++k,1000,"vec len");
   }
   float st=0,av=0,dmax=0;
   for(i=0;i<ndoc;i++){
      st+=vln[i];
      av+=vln[i]=(float)sqrt(vln[i]);
      if(dmax<vln[i])dmax=vln[i];
      if(vln[i]<2.0)vln[i]=2.0;
   }
   if(pflag)cout << "global max vec len " << dmax << endl;
   av=av/((float)ndoc);
   st=(float)sqrt((double)st/((float)ndoc)-av*av);
   if(pflag)cout << "av " << av << " st " << st << endl;
}

void Vnab::reload(float (*d_local)(int,long),float (*global)(long)){
   int pflag=get_qflag();
   long i,j,k,nmx,*idx;
   float *cwx,xx,gwx;
   Vterm *ptm;

   k=0;
   btr->node_first();
   while(btr->node_next()){
      ptm=(Vterm *)btr->give_ptr();
      idx=ptm->post->idx;
      nmx=ptm->post->ix;
      cwx=ptm->cwgt;
      ptm->gwgt=gwx=global(nmx);
      for(i=0;i<nmx;i++){
         j=*(idx+i);
         xx=d_local(rnd(*(cwx+i)),doc->alen[j]);
         cwx[i]=gwx*xx/vln[j];
      }
      mark(pflag,++k,1000,"reloaded");
   }
}

void Vnab::Score(long nd,float (*q_local)(int,long)){
   int j,pflag=get_qflag();
   long i,k,*kpt;
   char *spr;
   Vterm *ptm;
   Index *ind;
   float xx,*wpt;

   if(sxx!=NULL)delete [] sxx;
   sxx=new float[ndoc];
   for(i=0;i<ndoc;i++)*(sxx+i)=0;

   doc->clear();
   doc->read(nd);
   while((spr=doc->show(j))!=NULL){
      xx=q_local(j,doc->alen[nd])/vln[nd];
      if(btr->search(spr)){
         ptm=(Vterm *)(btr->give_ptr());
         ind=ptm->post;
         kpt=ind->idx;
         wpt=ptm->cwgt;
         for(i=0;i<ind->ix;i++){
            k=*(kpt+i);
            (*(sxx+k))+=xx*(*(wpt+i));
         }
      }
      else {
         if(pflag)cout << spr << " not in database!" << endl;
      }
   }
}

void Vnab::Score(Docum &Doc,float (*q_local)(int,long)){
   int j,pflag=get_qflag();
   long i,k,*kpt,nd;
   char *spr;
   Vterm *ptm;
   Index *ind;
   float xx,*wpt;

   if(sxx!=NULL)delete [] sxx;
   sxx=new float[ndoc];
   for(i=0;i<ndoc;i++)*(sxx+i)=0;

   Doc.reset();
   while((spr=Doc.show(j))!=NULL){
      xx=q_local(j,Doc.len);
      if(btr->search(spr)){
         ptm=(Vterm *)(btr->give_ptr());
         ind=ptm->post;
         kpt=ind->idx;
         wpt=ptm->cwgt;
         for(i=0;i<ind->ix;i++){
            k=*(kpt+i);
            (*(sxx+k))+=xx*(*(wpt+i));
         }
      }
      else {
         if(pflag)cout << spr << " not in database!" << endl;
      }
   }
}

float Vnab::Normalize(Docum &Doc,float (*q_local)(int,long),float (*global)(long)){
   int j,pflag=get_qflag();
   long i,k,nd;
   char *spr;
   Vterm *ptm;
   float xx,sum=0;

   Doc.reset();
   while((spr=Doc.show(j))!=NULL){
      xx=q_local(j,Doc.len);
      if(btr->search(spr)){
         ptm=(Vterm *)(btr->give_ptr());
         sum+=(ptm->gwgt)*xx*xx;
      }
      else {
         sum+=global(1)*xx*xx;
      }
   }
   sum=(float)pow((double)sum,0.5);
   sum=(sum<2.0)?2.0:sum;
   for(i=0;i<ndoc;i++)sxx[i]=sxx[i]/sum;
   return(sum);
}

float Vnab::Normal_Score(Docum &Dc1,Docum &Dc2,float (*q_local)(int,long),float (*global)(long)){
   int j,pflag=get_qflag();
   long i,k,n,nd,len1,len2;
   char *spr,*spr1,*spr2;
   Vterm *ptm;
   float xx,sum=0,sum1=0,sum2=0,*vct1,*vct2;

   Dc1.reset();
   len1=Dc1.num_wrds();
   vct1=new float[len1];
   i=0;
   while((spr=Dc1.show(j))!=NULL){
      xx=q_local(j,Dc1.len);
      if(btr->search(spr)){
         ptm=(Vterm *)(btr->give_ptr());
         sum1+=vct1[i]=(ptm->gwgt)*xx*xx;
      }
      else {
         sum1+=vct1[i]=global(1)*xx*xx;
      }
      i++;
   }
   sum1=(float)pow((double)sum1,0.5);
   sum1=(sum1<2.0)?2.0:sum1;

   Dc2.reset();
   len2=Dc2.num_wrds();
   vct2=new float[len2];
   i=0;
   while((spr=Dc2.show(j))!=NULL){
      xx=q_local(j,Dc2.len);
      if(btr->search(spr)){
         ptm=(Vterm *)(btr->give_ptr());
         sum2+=vct2[i]=(ptm->gwgt)*xx*xx;
      }
      else {
         sum2+=vct2[i]=global(1)*xx*xx;
      }
      i++;
   }
   sum2=(float)pow((double)sum2,0.5);
   sum2=(sum2<2.0)?2.0:sum2;

   Dc1.reset();
   Dc2.reset();
   spr1=Dc1.show(j);
   spr2=Dc2.show(j);
   i=k=0;
   while(spr1&&spr2){
      if(!(n=strcmp(spr1,spr2))){
         sum+=sqrt(vct1[i]*vct2[k]);
         spr1=Dc1.show(j);
         spr2=Dc2.show(j);
         i++;
         k++;
      }
      else if(n<0){
         spr1=Dc1.show(j);
         i++;
      }
      else {
         spr2=Dc2.show(j);
         k++;
      }
   }
   delete [] vct1;
   delete [] vct2;
   return(sum/(sum1*sum2));
}

//Vnak functions

Vnak::Vnak(const char *namdbn,const char *nampsg) : Manf(namdbn,nampsg){
   sxx=NULL;
}

Vnak::Vnak(const char *namdoc) : Manf(namdoc){
   sxx=NULL;
}

Vnak::~Vnak(void){
   if(sxx)delete [] sxx;
}

void Vnak::pre_normz(float (*global)(long)){
   long i,j;
   int pflag=get_qflag();
   float xx,yy,zz;

   ifstream *pfin=pDnb->get_Istr("d",ios::in);
   *pfin >> ndoc;
   pDnb->dst_Istr(pfin);
   xttrc=(double)ndoc;
     
   float *vlen=new float[ndoc];
   for(i=0;i<ndoc;i++)vlen[i]=0;
   
   gopen_Postg();
   for(i=0;i<nwrd;i++){
      xx=global(pPsg->freq[i]);
      for(j=0;j<Pst[i].ix;j++){
         yy=*(flc[i]+j);
         vlen[Pst[i].idx[j]]+=yy*yy*xx;
      }
      mark(pflag,i,1000,"terms for vlen");
   }

   float st=0,av=0,dmax=0;
   for(i=0;i<ndoc;i++){
      st+=vlen[i];
      av+=vlen[i]=(float)sqrt((double)vlen[i]);
      if(dmax<vlen[i])dmax=vlen[i];
      if(vlen[i]<2.0)vlen[i]=2.0;
   }
   if(pflag)cout << "global max vec len " << dmax << endl;
   av=av/((float)ndoc);
   st=(float)sqrt((double)st/((float)ndoc)-av*av);
   if(pflag)cout << "av " << av << " st " << st << endl;

   ofstream *pfout=pPsg->get_Ostr("r",ios::out);
   for(i=0;i<nwrd;i++){
      xx=global(pPsg->freq[i]);
      for(j=0;j<Pst[i].ix;j++){
         yy=*(flc[i]+j);
         zz=xx*yy/vlen[Pst[i].idx[j]];
         pfout->write((char*)&zz,sizeof(float));
      }
      mark(pflag,i,1000,"terms prec data");
   }
   delete [] vlen;
   pPsg->dst_Ostr(pfout);
   gclose_Postg();
}

void Vnak::pre_unorm(float (*global)(long)){
   long i,j;
   int pflag=get_qflag();
   float xx,yy,zz;

   ifstream *pfin=pDnb->get_Istr("d",ios::in);
   *pfin >> ndoc;
   pDnb->dst_Istr(pfin);
   xttrc=(double)ndoc;

   gopen_Postg();

   ofstream *pfout=pPsg->get_Ostr("r",ios::out);
   for(i=0;i<nwrd;i++){
      xx=global(pPsg->freq[i]);
      for(j=0;j<Pst[i].ix;j++){
         yy=*(flc[i]+j);
         zz=xx*yy;
         pfout->write((char*)&zz,sizeof(float));
      }
      mark(pflag,i,1000,"terms prec data");
   }
   pPsg->dst_Ostr(pfout);
   gclose_Postg();
}

void Vnak::gopen_operate(void){
   long i;
   gopen_Dbinbase();
   xttrc=(double)ndoc;
   //Modifided pPsg->gopen_map();
   ifstream *pfin=pPsg->get_Istr("n",ios::in);
   *pfin >> pPsg->nwrd;
   pPsg->dst_Istr(pfin);

   pPsg->freq=(long*)pPsg->get_Mmap("f");
   pPsg->pddr=(long*)pPsg->get_Mmap("pa");
   pPsg->sddr=(long*)pPsg->get_Mmap("sa");
   pPsg->term=pPsg->get_Mmap("s");

   pPsg->pfil=(long*)pPsg->get_Mmap("p");
   rfil=(float *)pPsg->get_Mmap("r");

   //Modified Manf gopen_Postg
   nwrd=pPsg->nwrd;
   pPsg->fill_post();
   Pst=pPsg->Pst;
   flc=new float*[nwrd];
   for(i=0;i<nwrd;i++)flc[i]=rfil+pPsg->pddr[i];
}

void Vnak::gclose_operate(void){
   gclose_Dbinbase();
   //Modifided pPsg->gclose_map();
   pPsg->dst_Mmap("f",(char*)pPsg->freq);
   pPsg->dst_Mmap("s",pPsg->term);
   pPsg->dst_Mmap("sa",(char*)pPsg->sddr);
   pPsg->dst_Mmap("pa",(char*)pPsg->pddr);
   pPsg->dst_Mmap("p",(char*)pPsg->pfil);
   pPsg->dst_Mmap("r",(char*)rfil);

   //Modified Manf gclose_Postg
   pPsg->dele_post();
   delete [] flc;
}

void Vnak::Score(long nd){
   long i,j,k;
   float xx,yy;

   if(sxx)delete [] sxx;
   sxx=new float[ndoc];
   for(i=0;i<ndoc;i++)sxx[i]=0;
   
   read(nd);
   for(i=0;i<nw;i++){
      j=nwd[i];
      xx=lwt[i];
      for(k=0;k<Pst[j].ix;k++){
         sxx[Pst[j].idx[k]]+=*(flc[j]+k)*xx;
      }
      mark(1,i,10,"terms");
   }
   xx=sxx[nd];
   if(xx<2.0)xx=2.0;
   for(i=0;i<ndoc;i++)sxx[i]=sxx[i]/xx;
}

void Vnak::Screen(long llim,long nd){
   long i,j,k,m;
   float xx,zz;

   if(sxx)delete [] sxx;
   sxx=new float[ndoc];
   for(i=0;i<ndoc;i++)sxx[i]=0;

   read(nd);
   for(i=0;i<nw;i++){
      j=nwd[i];
      xx=lwt[i];
      if((m=Pst[j].ix)<=llim){
         for(k=0;k<m;k++){
            sxx[Pst[j].idx[k]]+=*(flc[j]+k)*xx+10001.0;
         }
      }
      else {
         for(k=0;k<m;k++){
            sxx[Pst[j].idx[k]]+=*(flc[j]+k)*xx;
         }
      }
      mark(1,i,10,"terms");
   }
   xx=sxx[nd];
   zz=rnd(xx/10000.0);
   xx-=zz*10001.0;
   if(xx<2.0)xx=2.0;
   for(i=0;i<ndoc;i++)sxx[i]=sxx[i]/xx;
}

void Vnak::Score_un(Docum &Doc,float (*q_local)(int,long)){
   long i,j,k;
   float xx;

   if(sxx)delete [] sxx;
   sxx=new float[ndoc];
   for(i=0;i<ndoc;i++)sxx[i]=0;

   Doc.reset();
   for(i=0;i<Doc.nw;i++){
      if(j=pPsg->get_idx_map(Doc.word[i])){
         j--;
         xx=q_local(Doc.lcnt[i],Doc.len);
         for(k=0;k<Pst[j].ix;k++){
            sxx[Pst[j].idx[k]]+=*(flc[j]+k)*xx;
         }
      }
      mark(1,i,10,"terms");
   }
}

void Vnak::Score_nz(Docum &Doc,float (*q_local)(int,long),float (*global)(long)){
   long i,j,k;
   float xx,sum=0;

   if(sxx)delete [] sxx;
   sxx=new float[ndoc];
   for(i=0;i<ndoc;i++)sxx[i]=0;

   Doc.reset();
   for(i=0;i<Doc.nw;i++){
      if(j=pPsg->get_idx_map(Doc.word[i])){
         j--;
         xx=q_local(Doc.lcnt[i],Doc.len);
         sum+=xx*xx*global(pPsg->freq[j]);
         for(k=0;k<Pst[j].ix;k++){
            sxx[Pst[j].idx[k]]+=*(flc[j]+k)*xx;
         }
      }
      mark(1,i,10,"terms");
   }
   sum=sqrt(sum);
   if(sum<2.0)sum=2.0;
   for(i=0;i<ndoc;i++)sxx[i]=sxx[i]/sum;
}

void Vnak::Screen_nz(long llim,Docum &Doc,float (*q_local)(int,long),float (*global)(long)){
   long i,j,k,m;
   float xx,sum=0;

   if(sxx)delete [] sxx;
   sxx=new float[ndoc];
   for(i=0;i<ndoc;i++)sxx[i]=0;

   Doc.reset();
   for(i=0;i<Doc.nw;i++){
      if(j=pPsg->get_idx_map(Doc.word[i])){
         j--;
         xx=q_local(Doc.lcnt[i],Doc.len);
         sum+=xx*xx*global(pPsg->freq[j]);
         if((m=pPsg->freq[j])<=llim){ 
            for(k=0;k<m;k++){
               sxx[Pst[j].idx[k]]+=*(flc[j]+k)*xx+(10001.0);
            }
         }
         else {
            for(k=0;k<m;k++){
               sxx[Pst[j].idx[k]]+=*(flc[j]+k)*xx;
            }
         }
      }
      mark(1,i,10,"terms");
   }
   sum=sqrt(sum);
   if(sum<2.0)sum=2.0;
   for(i=0;i<ndoc;i++)sxx[i]=sxx[i]/sum;
}

   //Local weight functions

float q_const(int lc,long n){
   return(1.0);
}

float d_lc_func(int lc,long n){
   return((float)lc);
}

float d_lc_ratio(int lc,long n){
   return((float)lc/(lc+2.0));
}

float d_lc_log(int lc,long n){
   return((float)log((double)lc+1.0));
}

   //Local TREC functions

float d_len_trec(int lc,long m){
   return(1.0/(1.5+1.5*(float)m/243.128));
}

float d_inquery_trec(int lc,long m){
   return((float)lc/((float)lc+0.5+1.5*(float)m/243.128));
}

float d_robertson_trec(int lc,long m){
   return((float)lc/((float)lc+2.0*(float)m/243.128));
}

float d_wilbur_trec(int lc,long m){
  float u,v;
  double md;
  if(m<dmt)md=dmt;
  else md=(double)m;
  v=(float)exp(md*0.002564+(lc-1.0)*lfac);
  return(1.0/(1.0+v));
}

   //Local Med functions

float d_inquery_med(int lc,long m){
   return((float)lc/((float)lc+0.5+1.5*(float)m/137.924));
}

float d_wilbur_med(int lc,long m){
  float u,v;
  double md;
  if(m<dmt)md=dmt;
  else md=(double)m;
  v=(float)exp(md*0.0044+(lc-1.0)*lfab);
  return(1.0/(1.0+v));
}

   //Global weight functions

float global_idf(long n){
   if(n>dmt)return(l2*log(xttrc/((double)n)));
   else if(n>1)return(l2*log(xttrc/dmt));
   else return(0.0);
}

float global_const(long n){
   return(1.0);
}

}
