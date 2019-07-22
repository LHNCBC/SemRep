#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <cassert>
#include "Btree.h"
#include "runn.h"
#include "Slice.h"
using namespace std;
namespace iret {

Slice::Slice(void) : Post(){}

Slice::Slice(const char *nam) : Post(nam){}

Slice::~Slice(void){}

void Slice::p_slice(void) {
  int pflag=get_qflag(), size_of_seg;
  long ct,filect=0,addr_off,file_size=0,*pos;
  char cnam[max_str],numstring[25];
  unsigned char *cci,*cod,c;
  long *iix,cnb,cnm,fm[31],pw[31],pk,maxcnb=0;
  long cnt,i,j,k,r,yi,od,mk,ba,md,mask;
  ifstream fin, *s_infile, *p_infile, *f_infile;
  ofstream fout,*cfout;

  get_pathw(cnam,"postset",name,"#");
  fin.open(cnam, ios::in);
  if (!fin) cout<<"no file postset_ .#\n";
  fin>>size_of_seg; fin.getline(cnam, max_str);
  fin>>ndoc; fin.getline(cnam, max_str);
  fin>>cnt; fin.getline(cnam, max_str);  // get the # of pieces
  fin.close();

  //Setup for compressed output.
  iix=new long[ndoc];
  cci=new unsigned char[ndoc];
  cod=new unsigned char[ndoc];
  pw[0]=1;
  for(i=1;i<31;i++)pw[i]=2*pw[i-1];
  for(i=0;i<31;i++)fm[i]=(long)floor((double)ndoc/((double)pw[i]+1.0));
  mk=0;
  while((mk<31)&&(fm[mk]>=1))mk++;
  cfout=new ofstream[mk];
  pos  =new long[mk];
  get_pathw(cnam,"slice",name,"paths");
  fin.open(cnam,ios::in);
  for(i=0;i<mk;i++){
     fin.getline(cnam,max_str);
     cfout[i].open(cnam,ios::out|ios::binary);
     pos[i]=0;
  }
  fin.close();

  char **term = new char *[cnt];
  ofstream ffout;  // for Pos "f" file;
  ofstream afout;  // for Pos "a" file;

  char phra[max_str];
  long *fq=new long[cnt], frq =0;

  get_pathw(cnam,"slice",name,"f");
  ffout.open(cnam, ios::out|ios::binary);    //remember to close at the end.

  get_pathw(cnam,"slice",name,"a");
  afout.open(cnam, ios::out|ios::binary);    //remember to close at the end.

  p_infile = new ifstream[cnt]; 
  f_infile = new ifstream[cnt];
  s_infile = new ifstream[cnt];

  //open all segs files and get first terms.
  for (i=0; i<cnt; i++) {
    long_str(numstring, i);
    term[i] = new char[max_str];

    get_pathw(cnam,"postset",numstring,"s");
    s_infile[i].open(cnam, ios::in);
    get_pathw(cnam,"postset",numstring,"p");
    p_infile[i].open(cnam, ios::in|ios::binary);
    get_pathw(cnam,"postset",numstring,"f");
    f_infile[i].open(cnam, ios::in|ios::binary);

    get_string(term[i],s_infile[i],'\n');
  }

  long total_terms, c_terms=0, a_offset;
  ct = 0;
  get_pathw(cnam,"slice",name,"n");
  fin.open(cnam,ios::in);
  fin>>total_terms;
  fin.close();
  get_pathw(cnam,"slice",name,"s");
  fin.open(cnam,ios::in);

  while(c_terms < total_terms) {
    c_terms++;
    get_string(phra,fin,'\n');
    cnm=0;

    for (i=0; i<cnt; i++)  {
      if (strcmp(term[i], phra) ==0)  {
        get_string(term[i],s_infile[i],'\n'); //get next term
        f_infile[i].read((char*)&fq[i], sizeof(long));
        frq +=fq[i];
        for(k=0;k<fq[i];k++){
           p_infile[i].read((char*)(iix+cnm),sizeof(long));
           p_infile[i].read((char*)(cci+cnm),sizeof(char));
           cnm++;
        }
      }
    }
    if(cnm!=frq){cout << "Error in collection" << endl;exit(0);}
    pk=mk;
    while((pk>0)&&(cnm>fm[pk]))pk--;

    //Compress data
    ba=pw[pk];
    if(pk)md=pw[pk-1];
    mask=ba-1;
    cnb=0;
    yi=0;
    c=0;
    od=-1;
    for(i=0;i<cnm;i++){
      j=*(iix+i)-od;
      k=j/ba+1;
      r=j%ba;
      while(k){
         switch(k){
            case 1: c=(c << 1);
                    yi++;
                    k--;
                    break;
            default: c=(c << 1)+1;
                    yi++;
                    k--;
                    break;
         }
         if(yi==8){
            *(cod+(cnb++))=c;
            yi=0;
            c=0;
         }
      }
      k=pk;
      while(k){
         c=(c << 1)+r/md;
         r=mask&(r << 1);
         yi++;
         k--;
         if(yi==8){
            *(cod+(cnb++))=c;
            yi=0;
            c=0;
         }
      }
      k=*(cci+i);
      while(k){
         switch(k){
            case 1: c=(c << 1);
                    yi++;
                    k--;
                    break;
            default: c=(c << 1)+1;
                    yi++;
                    k--;
                    break;
         }
         if(yi==8){
            *(cod+(cnb++))=c;
            yi=0;
            c=0;
         }
      }
      od=*(iix+i);
    }
    if(yi){
      c=(c << (8-yi));
      *(cod+(cnb++))=c;
    }
    //End of compression
    
    //write out compressed data for term
    afout.write((char*)&(pos[pk]),sizeof(long));
    cfout[pk].write((char*)&cnb,sizeof(long));
    cfout[pk].write((char*)cod,cnb);
    (pos[pk])+=cnb+sizeof(long);
    maxcnb=(cnb>maxcnb)?cnb:maxcnb;
    
    //write into Pos's "f" file.
    ffout.write((char*)&frq, sizeof(long));
    frq =0; //reset.
    mark(pflag,++ct,2000,"postings out........wait.....");
  }
  get_pathw(cnam,"slice",name,"cs");
  fout.open(cnam,ios::out);
  fout << maxcnb << endl;
  fout.close();
  ffout.close();
  afout.close();
  fin.close();
  for(i=0;i<mk;i++)cfout[i].close();

  //need to delete allocated term and close files.
  for (i=0; i<cnt; i++) {
    delete [] term[i];
    s_infile[i].close();
    p_infile[i].close();
    f_infile[i].close();
  }
  delete [] term;
  delete [] fq;
  delete [] s_infile;
  delete [] p_infile;
  delete [] f_infile;
  delete [] cfout;
  delete [] pos;
}

void Slice::s_slice(long list_size) {
  int pflag=get_qflag(), size_of_seg;
  long ct, cnt, i,j,ix,jx,finish_cnt, file_size=0;
  char cnam[max_str],numstring[25],*phra,path[255];
  List *plst1, *plst2;
  ifstream fin, *infile;
  ofstream fout;
  
  get_pathw(cnam,"postset",name,"#");
  fin.open(cnam, ios::in);
  fin>>size_of_seg; fin.getline(cnam, max_str);
  fin>>ndoc; fin.getline(cnam, max_str); //get the total number of docs.
  fin>>cnt; fin.getline(cnam, max_str);  // get the # of pieces
  fin.close();

  get_pathw(cnam,"slice",name,"d");
  fout.open(cnam,ios::out);
  fout << ndoc << endl;
  fout.close();

  char **term = new char *[cnt];
  infile = new ifstream[cnt];
  for (i=0; i<cnt; i++) {
        long_str(numstring, i);
        term[i] = new char[max_str];
      get_pathw(cnam,"postset",numstring,"s");
      infile[i].open(cnam,ios::in);
  }

  ifstream nfin;   //for "n" input
  ofstream nfout;
  ofstream sfout;  //for Post "s" file;

  //keep track of location.
  long *n_term=new long[cnt];
  long *c_term=new long[cnt];
  long *g_term=new long[cnt],step=1000,xstep,k;
  char *pch;
  ifstream *sfin;

  //initialize
  long total_term=0;    // the total num of terms
  for (i=0; i<cnt; i++)
    {
      long_str(numstring, i);
      get_pathw(cnam,"postset",numstring,"n");
      nfin.open(cnam, ios::in);
      nfin>>n_term[i];
      total_term += n_term[i];
      nfin.close();
      c_term[i]=0;
      g_term[i]=1;
    }
  ct = 0;
  plst1 = new List;
  get_pathw(cnam,"slice",name,"n");
  nfout.open(cnam, ios::out);
  long terms=0;
  get_pathw(cnam,"slice",name,"s");
  sfout.open(cnam,ios::out);

  next:

  while (plst1->cnt_key < list_size) {
    for (i=0; i<cnt; i++)
    { 
      if (g_term[i]) {
         k=n_term[i]-c_term[i];
         xstep=(step<k)?step:k;
         pch=term[i];
         sfin=&infile[i];
         for(k=0;k<xstep;k++){
            get_string(pch,*sfin,'\n');
            plst1->add_key_count(pch);
         }
         c_term[i]+=xstep;
         ct+=xstep;
         if (c_term[i] == n_term[i]) {
            delete [] term[i];
            term[i] = NULL;
            g_term[i]=0;
         } 
      }
    }
    if(pflag){cout << ct << " postings in........" << endl;}
    if (ct == total_term) {
        if(pflag)cout << "   *** writing on disk ***" << endl;
        plst1->node_first();
        while (plst1->node_next()) {
          phra = plst1->show_str();
          sfout<<phra<<endl;
          terms++;
        }
        nfout<<terms<<endl;
        nfout.close();
        sfout.close();
        for (i=0; i<cnt; i++) {
          delete [] term[i];
          infile[i].close();
        }
        delete [] term;
        delete [] infile;
        delete [] n_term;
        delete [] c_term;
        delete [] g_term;
        delete plst1;
        exit(0);
    }
    for (i=0;i<cnt;i++) {
       if (term[i] != NULL) {
         ix = jx = i;
         break;
       } 
    }
    for (i=jx+1;i<cnt;i++) {
       if ((term[i]!=NULL)&&(strcmp(term[jx],term[i]) < 0))jx = i;
    }
    g_term[jx]=0; //Turn off the highest.
    for (i=ix+1;i<cnt;i++) {
       if ((term[i]!=NULL)&&(strcmp(term[ix],term[i]) > 0))ix = i;
    }
    g_term[ix]=1; //Turn on the lowest.
  }

  if(pflag)cout << "   *** writing on disk ***" << endl;
  pch=term[ix];
  plst1->node_first();
  while (plst1->node_next()) {
    phra = plst1->show_str();
    if (strcmp(phra,pch) <= 0) {
      sfout<<phra<<endl;
      terms++;
    }
    else
      break;
  }
  plst2 = new List;
  plst2->add_key_count(phra);
  while (plst1->node_next()) {
    phra = plst1->show_str();
    plst2->add_key_count(phra);
  }
  delete plst1;
  plst1 = plst2;
  goto next;
}

}
