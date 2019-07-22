#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <runn.h>
#include <Blist.h>
#include <Regist.h>
#include "DataObj.h"

using namespace std;
namespace iret {

//Term theme object

Theme::Theme(void){
   mt=0;
   mti=NULL;
   alp=NULL;
   weg=NULL;
   msa=NULL;
   cmm=NULL;
}

Theme::Theme(Theme *pThm){
   long i;

   mt=0;
   rnw_Mem(pThm->mt);
   mt=pThm->mt;
   mtc=pThm->mtc;
   for(i=0;i<mtc;i++){
      mti[i]=pThm->mti[i];
      weg[i]=pThm->weg[i];
      alp[i]=pThm->alp[i];
   }

   ms=pThm->ms;
   msc=pThm->msc;
   for(i=0;i<msc;i++)msa[i]=pThm->msa[i];

   stn=pThm->stn;
   cs=pThm->cs;
   cmm=NULL;
}

Theme::~Theme(void){
   if(mti!=NULL)delete [] mti;
   if(alp!=NULL)delete [] alp;
   if(weg!=NULL)delete [] weg;
   if(msa!=NULL)delete [] msa;
   if(cmm!=NULL)delete [] cmm;
}

void Theme::rnw_Mem(long n){
   if(!mt){
      mti=new long[n];
      weg=new float[n];
      alp=new float[n];
      msa=new long[n];
   }
   else if(n>mt){
      if(mti!=NULL)delete [] mti;
      if(alp!=NULL)delete [] alp;
      if(weg!=NULL)delete [] weg;
      if(msa!=NULL)delete [] msa;
      mti=new long[n];
      weg=new float[n];
      alp=new float[n];
      msa=new long[n];
   }
}

void Theme::Union(Theme *pTm){
   long i,j,k,len;

   //Order by mti increasing 
   hSort(mtc,mti,alp,weg);
   hSort(pTm->mtc,pTm->mti,pTm->alp,pTm->weg);

   //Set new memory for the union
   len=mtc+(pTm->mtc);
   long *amti=new long[len];
   float *aalp=new float[len];
   float *aweg=new float[len];
   long *zti=pTm->mti;
   i=0;
   j=0;
   k=0;
   while((i<mtc)&&(j<pTm->mtc)){
      while((i<mtc)&&(mti[i]<zti[j])){
         amti[k]=mti[i];
         aalp[k]=alp[i];
         aweg[k]=weg[i];
         i++;k++;
      }
      if((i<mtc)&&(mti[i]==zti[j])){
         amti[k]=mti[i];
         aalp[k]=alp[i]+pTm->alp[j];
         aweg[k]=weg[i]+pTm->weg[j];
         i++;j++;k++;
      }
      if(i<mtc){
         while((j<pTm->mtc)&&(mti[i]>zti[j])){
            amti[k]=zti[j];
            aalp[k]=pTm->alp[j];
            aweg[k]=pTm->weg[j];
            j++;k++;
         }
      }
   }
   while(i<mtc){
      amti[k]=mti[i];
      aalp[k]=alp[i];
      aweg[k]=weg[i];
      i++;k++;
   }
   while(j<pTm->mtc){
      amti[k]=zti[j];
      aalp[k]=pTm->alp[j];
      aweg[k]=pTm->weg[j];
      j++;k++;
   }
   mt=mtc=k;
   delete [] mti;
   delete [] alp;
   delete [] weg;
   mti=amti;
   alp=aalp;
   weg=aweg;

   //Promoted material
   if(!(pTm->msc)){
      k=0;
      for(i=0;i<msc;i++){
         if(msa[i]>0){
            msa[k]=msa[i];
            k++;
         }
      }   
      msc=k;
      return;
   }
   if(!msc){
      ms=pTm->ms;
      msc=pTm->msc;
      msa=new long[msc];
      k=0;
      for(i=0;i<msc;i++){
         if(pTm->msa[i]>0){
            msa[k]=pTm->msa[i];
            k++;
         }
      }
      msc=k;
      return;
   }

   //Set up to merge
   len=msc+(pTm->msc);
   long *pmt=new long[msc];
   long *qmt=new long[pTm->msc];
   long *amsa=new long[len];
   for(i=0;i<msc;i++){
      if(msa[i]<0)pmt[i]=-msa[i];
      else pmt[i]=msa[i];
   }
   for(i=0;i<pTm->msc;i++){
      if(pTm->msa[i]<0)qmt[i]=-pTm->msa[i];
      else qmt[i]=pTm->msa[i];
   }
   zti=pTm->msa;
   i=0;
   j=0;
   k=0;
   while((i<msc)&&(j<pTm->msc)){
      while((i<msc)&&(pmt[i]<qmt[j])){
         if(msa[i]>0){
            amsa[k]=msa[i];
            k++;
         }
         i++;
      }
      if((i<msc)&&(pmt[i]==qmt[j])){
         if(msa[i]<=zti[j]){
            amsa[k]=zti[j];
         }
         else amsa[k]=msa[i];
         i++;j++;k++;
      }
      if(i<msc){
         while((j<pTm->msc)&&(pmt[i]>qmt[j])){
            if(zti[j]>0){
               amsa[k]=zti[j];
               k++;
            }
            j++;
         }
      }
   }
   while(i<msc){
      if(msa[i]>0){
         amsa[k]=msa[i];
         k++;
      }
      i++;
   }
   while(j<pTm->msc){
      if(zti[j]>0){
         amsa[k]=zti[j];
         k++;
      }
      j++;
   }
   ms=len;
   msc=k;
   delete [] pmt;
   delete [] qmt;
   delete [] msa;
   msa=amsa;
}

void Theme::Order_lex(void){
   hSort(mtc,mti,alp,weg);
}

void Theme::Order_alp(void){
   hRort(mtc,alp,mti,weg);
}

void Theme::Order_weg(void){
   hRort(mtc,weg,mti,alp);
}

void Theme::Set_Diff(void){
   long i;
   if(cmm!=NULL)delete [] cmm;
   cmm=new long[mtc];
   for(i=0;i<mtc;i++)cmm[i]=0;
}

void Theme::Diff(Theme *pTh){
   long i,j,k,m,ct=0,imax;
   float amax=-1.0;

   i=0;
   j=0;
   while((i<mtc)&&(j<pTh->mtc)){
      m=mti[i];
      if(m>(k=pTh->mti[j])){
         j++;
         while((j<pTh->mtc)&&(m>(k=pTh->mti[j])))j++;
      }
      if(m<k){
         if(amax<alp[i]){
            imax=i;
            amax=alp[i];
         }
         i++;
      }
      else if(m==k){
         i++;j++;
      }
   }
   while(i<mtc){
      if(amax<alp[i]){
         imax=i;
         amax=alp[i];
      }
      i++;
   }
   if(amax>=0.0)(cmm[imax])++;
}

float Theme::Dice_lex(Theme *pThm){
   long i,j,k;
   float s1=0,s2=0,sc=0;

   i=0;
   j=0;
   while((i<mtc)&&(j<pThm->mtc)){
      if(mti[i]<(k=pThm->mti[j])){
         while((i<mtc)&&(mti[i]<k)){
            s1+=1.0;
            i++;
         }
      }
      if(i<mtc){
         if(mti[i]==k){
            sc+=2.0;
            i++;j++;
         }
         else {
            k=mti[i];
            while((j<pThm->mtc)&&(pThm->mti[j]<k)){
               s2+=1.0;
               j++;
            }
         }
      }
   }
   if(i<mtc){
      while(i<mtc){
         s1+=1.0;
         i++;
      }
   }
   if(j<pThm->mtc){
      while(j<pThm->mtc){
         s2+=1.0;
         j++;
      }
   }
   return(sc/(s1+s2+sc));
}

float Theme::Dice_alp(Theme *pThm){
   long i,j,k;
   float s1=0,s2=0,sc=0;

   i=0;
   j=0;
   while((i<mtc)&&(j<pThm->mtc)){
      if(mti[i]<(k=pThm->mti[j])){
         while((i<mtc)&&(mti[i]<k)){
            s1+=alp[i];
            i++;
         }
      }
      if(i<mtc){
         if(mti[i]==k){
            sc+=alp[i]+pThm->alp[j];
            i++;j++;
         }
         else {
            k=mti[i];
            while((j<pThm->mtc)&&(pThm->mti[j]<k)){
               s2+=pThm->alp[j];
               j++;
            }
         }
      }
   }
   if(i<mtc){
      while(i<mtc){
         s1+=alp[i];
         i++;
      }
   }
   if(j<pThm->mtc){
      while(j<pThm->mtc){
         s2+=pThm->alp[j];
         j++;
      }
   }
   return(sc/(s1+s2+sc));
}

float Theme::Dice_weg(Theme *pThm){
   long i,j,k;
   float s1=0,s2=0,sc=0;

   i=0;
   j=0;
   while((i<mtc)&&(j<pThm->mtc)){
      if(mti[i]<(k=pThm->mti[j])){
         while((i<mtc)&&(mti[i]<k)){
            s1+=weg[i];
            i++;
         }
      }
      if(i<mtc){
         if(mti[i]==k){
            sc+=weg[i]+pThm->weg[j];
            i++;j++;
         }
         else {
            k=mti[i];
            while((j<pThm->mtc)&&(pThm->mti[j]<k)){
               s2+=pThm->weg[j];
               j++;
            }
         }
      }
   }
   if(i<mtc){
      while(i<mtc){
         s1+=weg[i];
         i++;
      }
   }
   if(j<pThm->mtc){
      while(j<pThm->mtc){
         s2+=pThm->weg[j];
         j++;
      }
   }
   return(sc/(s1+s2+sc));
}

float Theme::Subs_lex(Theme *pThm){
   long i,j,k;
   float s1=0,sc=0;

   i=0;
   j=0;
   while((i<mtc)&&(j<pThm->mtc)){
      if(mti[i]<(k=pThm->mti[j])){
         while((i<mtc)&&(mti[i]<k)){
            s1+=1.0;
            i++;
         }
      }
      if(i<mtc){
         if(mti[i]==k){
            sc+=1.0;
            i++;j++;
         }
         else {
            k=mti[i];
            while((j<pThm->mtc)&&(pThm->mti[j]<k)){
               j++;
            }
         }
      }
   }
   if(i<mtc){
      while(i<mtc){
         s1+=1.0;
         i++;
      }
   }
   return(sc/(s1+sc));
}

float Theme::Subs_alp(Theme *pThm){
   long i,j,k;
   float s1=0,sc=0;

   i=0;
   j=0;
   while((i<mtc)&&(j<pThm->mtc)){
      if(mti[i]<(k=pThm->mti[j])){
         while((i<mtc)&&(mti[i]<k)){
            s1+=alp[i];
            i++;
         }
      }
      if(i<mtc){
         if(mti[i]==k){
            sc+=alp[i];
            i++;j++;
         }
         else {
            k=mti[i];
            while((j<pThm->mtc)&&(pThm->mti[j]<k)){
               j++;
            }
         }
      }
   }
   if(i<mtc){
      while(i<mtc){
         s1+=alp[i];
         i++;
      }
   }
   return(sc/(s1+sc));
}

float Theme::Subs_weg(Theme *pThm){
   long i,j,k;
   float s1=0,sc=0;

   i=0;
   j=0;
   while((i<mtc)&&(j<pThm->mtc)){
      if(mti[i]<(k=pThm->mti[j])){
         while((i<mtc)&&(mti[i]<k)){
            s1+=weg[i];
            i++;
         }
      }
      if(i<mtc){
         if(mti[i]==k){
            sc+=weg[i];
            i++;j++;
         }
         else {
            k=mti[i];
            while((j<pThm->mtc)&&(pThm->mti[j]<k)){
               j++;
            }
         }
      }
   }
   if(i<mtc){
      while(i<mtc){
         s1+=weg[i];
         i++;
      }
   }
   return(sc/(s1+sc));
}

float Theme::Ave_Alpha(long n){
   long i;
   float ss=0,sx;
   Order *pOrd;

   pOrd=new Order(n,mtc,alp);
   for(i=0;i<n;i++){
      pOrd->ind(i,sx);
      ss+=sx;
   }
   delete pOrd;
   return(ss/((double)n));
}
   
void Theme::debug(void){
   long i,j;
   cout << "term number " << mtc << " stringency " << stn << " cs " << cs << endl;
   for(i=0;i<mtc;i++){
      cout << mti[i] << "\t" << alp[i] << "\t" << weg[i] << endl;
   }
   cout << endl << "terms promoted " << msc << endl;
   for(i=0;i<msc;i++){
      cout << "   " << msa[i] << endl;
   }
}

void Theme::bin_Write(fstream &fout){
   long i;

   fout.write((char*)&mt,sizeof(long));
   fout.write((char*)&mtc,sizeof(long));

   fout.write((char*)alp,sizeof(float)*mtc);
   fout.write((char*)weg,sizeof(float)*mtc);
   fout.write((char*)mti,sizeof(long)*mtc);

   fout.write((char*)&ms,sizeof(long));
   fout.write((char*)&msc,sizeof(long));
   fout.write((char*)msa,sizeof(long)*msc);

   fout.write((char*)&stn,sizeof(long));
   fout.write((char*)&cs,sizeof(float));
}

void Theme::bin_Read(fstream &fin){
   long i;

   fin.read((char*)&i,sizeof(long));
   rnw_Mem(i);
   mt=i;
   fin.read((char*)&mtc,sizeof(long));

   fin.read((char*)alp,sizeof(float)*mtc);
   fin.read((char*)weg,sizeof(float)*mtc);
   fin.read((char*)mti,sizeof(long)*mtc);

   fin.read((char*)&ms,sizeof(long));
   fin.read((char*)&msc,sizeof(long));

   fin.read((char*)msa,sizeof(long)*msc);

   fin.read((char*)&stn,sizeof(long));
   fin.read((char*)&cs,sizeof(float));
}

void Theme::Write(fstream &fout,IdTerm *pIdt){
   long i,j,id;
   char cnam[max_str];

   fout << mt << '\t' << mtc << endl;
   for(i=0;i<mtc;i++){
      pIdt->disk_ifind(*(mti+i),cnam);
      fout << *(alp+i) << '\t' << *(weg+i) << '\t' << cnam << endl;
   }

   fout << ms << '\t' << msc << endl;
   for(i=0;i<msc;i++){
      id=*(msa+i);
      if(id<0){
         j=-1;
         pIdt->disk_ifind(-id,cnam);
      }
      else {
         j=1;
         pIdt->disk_ifind(id,cnam);
      }
      fout << j << '\t' << cnam << endl;
   }
   fout << stn << '\t' << cs << endl;
}

void Theme::Read(fstream &fin,IdTerm *pIdt){
   long i,j,k,id,*ord;
   char cnam[max_str],c;

   fin >> i;
   rnw_Mem(i);
   mt=i;
   fin >> mtc;

   j=0;
   for(i=0;i<mtc;i++){
      fin >> *(alp+j);
      fin >> *(weg+j);
      fin.get(c);
      fin.get(cnam,max_str);
      id=pIdt->disk_sifind(cnam);
      if(id){
         *(mti+j)=id;
         j++;
      }
   }
   mtc=j;

   fin >> ms;
   fin >> msc;
   ord=new long[msc];

   j=0;
   for(i=0;i<msc;i++){
      fin >> k;
      fin.get(c);
      fin.get(cnam,max_str);
      id=pIdt->disk_sifind(cnam);
      if(id){
         *(msa+j)=k*id;
         ord[j]=id;
         j++;
      }
   }
   msc=j;
   hSort(msc,ord,msa);
   delete [] ord;
   fin >> stn;
   fin >> cs;
}

//Theme sets
ATheme::ATheme(Slice_Accpth *pSac) : FBase("athmset","null"){
   pSaa=pSac;
}

ATheme::ATheme(const char *nam,Slice_Accpth *pSac) : FBase("athmset",nam){
   pSaa=pSac;
}

ATheme::~ATheme(){
}

void ATheme::gopen_map(int rfil){
   ifstream *pfin;

   cflag=rfil;

   pfin=get_Istr("n");
   *pfin >> nthm;
   dst_Istr(pfin);

   if(Gcom(READ_W)){
      addr=(long*)get_Mmap("a");
      pfwd=get_Fstr("w",ios::in);
      pSaa->inv_map();
      pSaa->gopen_idstring(pSaa->slice_name);
   }
   else if(cflag&READ_W){ //may have problem when fwd is already being read
      pfwd->seekg(0, ios::beg);
   }
   gct=-1;

   if(Gcom(READ_S))size=(long*)get_Mmap("s");
}

void ATheme::read(Theme &Thm){
   Thm.Read(*pfwd,(IdTerm*)pSaa);
   gct++;
}

void ATheme::read(long n,Theme &Thm){
   pfwd->seekg(*(addr+n));
   Thm.Read(*pfwd,(IdTerm*)pSaa);
   gct=n;
}

void ATheme::gclose_map(int rfil){
   cflag=rfil;
   if(Rcom(READ_W)){
      dst_Mmap("a",(char*)addr);
      dst_Fstr(pfwd);
   }
   if(Rcom(READ_S))dst_Mmap("s",(char*)size);
   pSaa->gclose_idstring();
   pSaa->inv_map_destroy();
}

void ATheme::gopen_write(void){
   cflag=3;

   if(Gcom(1)){
      pfad=get_Ostr("a");
      pfwd=get_Fstr("w",ios::out);
      pSaa->inv_map();
      pSaa->gopen_idstring(pSaa->slice_name);
   }
   else pfwd->seekp(0, ios::beg);
   gct=0;
   if(Gcom(2))pfsz=get_Ostr("s");
}

void ATheme::gopen_append(void) {
   cflag=3;

   ifstream *pfin=get_Istr("n");
   *pfin >> nthm;
   dst_Istr(pfin);
   gct = nthm;

   if(Gcom(1)){
      pfwd=get_Fstr("w",ios::app|ios::ate);
      pfad=get_Ostr("a",ios::app|ios::ate);
      pSaa->inv_map();
      pSaa->gopen_idstring(pSaa->slice_name);
   }
   if(Gcom(2)){
      pfsz=get_Ostr("s",ios::app|ios::ate);
   }
}

void ATheme::write(Theme &Thm){
   long i=pfwd->tellp();
   pfad->write((char*)&i,sizeof(long));
   pfsz->write((char*)&(Thm.mtc),sizeof(long));

   Thm.Write(*pfwd,pSaa);
   *pfwd << endl;
   gct++;
}

void ATheme::gclose_write(void){
   cflag=3;
   nthm=gct;
   ofstream *pfout=get_Ostr("n");
   *pfout << nthm << endl;
   dst_Ostr(pfout);

   if(Rcom(1)){
      dst_Ostr(pfad);
      dst_Fstr(pfwd);
      pSaa->gclose_idstring();
      pSaa->inv_map_destroy();
   }

   if(Rcom(2))dst_Ostr(pfsz);
}

//Binary theme set manager

BTheme::BTheme(void) : FBase("bthmset","null"){
   pThm=NULL;
}

BTheme::BTheme(const char *nam) : FBase("bthmset",nam){
   pThm=NULL;
}

BTheme::~BTheme(){
   if(pThm)dst_Theme_array();
}

void BTheme::gopen_map(int rfil){
   ifstream *pfin;

   cflag=rfil;

   pfin=get_Istr("n");
   *pfin >> nthm;
   dst_Istr(pfin);

   if(Gcom(READ_W)){
      addr=(long*)get_Mmap("a");
      wfil=get_Mmap("w");
   }

   gct=-1;

   if(Gcom(READ_S))size=(long*)get_Mmap("s");
}

void BTheme::read(Theme &Thm){
   gct++;
   char *ptt=wfil+*(addr+gct);
   long i=0,*lcl=(long*)ptt;
   float *lfl=(float*)ptt;

   Thm.mt=lcl[i++];
   Thm.mtc=lcl[i++];
   Thm.alp=lfl+i;i+=Thm.mtc;
   Thm.weg=lfl+i;i+=Thm.mtc;
   Thm.mti=lcl+i;i+=Thm.mtc;
   Thm.ms=lcl[i++];
   Thm.msc=lcl[i++];
   Thm.msa=lcl+i;i+=Thm.msc;
   Thm.stn=lcl[i++];
   Thm.cs=lfl[i];
}

void BTheme::read(long n,Theme &Thm){
   gct=n;
   char *ptt=wfil+*(addr+gct);
   long i=0,*lcl=(long*)ptt;
   float *lfl=(float*)ptt;

   Thm.mt=lcl[i++];
   Thm.mtc=lcl[i++];
   Thm.alp=lfl+i;i+=Thm.mtc;
   Thm.weg=lfl+i;i+=Thm.mtc;
   Thm.mti=lcl+i;i+=Thm.mtc;
   Thm.ms=lcl[i++];
   Thm.msc=lcl[i++];
   Thm.msa=lcl+i;i+=Thm.msc;
   Thm.stn=lcl[i++];
   Thm.cs=lfl[i];
}

void BTheme::crt_Theme_array(void){
   long i;

   pThm=(Theme**)new Theme*[nthm];
   for(i=0;i<nthm;i++){
      pThm[i]=new Theme;
      read(i,*pThm[i]);
   }
}

void BTheme::dst_Theme_array(void){
   long i;

   if(pThm!=NULL){
      for(i=0;i<nthm;i++){
         pThm[i]->alp=NULL;
         pThm[i]->weg=NULL;
         pThm[i]->mti=NULL;
         pThm[i]->msa=NULL;
         delete pThm[i];
      }
      delete [] pThm;
   }
}

void BTheme::gclose_map(int rfil){
   cflag=rfil;
   if(Rcom(READ_W)){
      dst_Mmap("a",(char*)addr);
      dst_Mmap("w", wfil);
   }
   if(Rcom(READ_S))dst_Mmap("s",(char*)size);
}

void BTheme::gopen_write(void){
   cflag=3;

   if(Gcom(1)){
      pfad=get_Ostr("a");
      pfwd=get_Fstr("w",ios::out);
   }
   else pfwd->seekp(0, ios::beg);
   gct=0;
   if(Gcom(2))pfsz=get_Ostr("s");
}

void BTheme::gopen_append(void) {
   cflag=3;

   ifstream *pfin=get_Istr("n");
   *pfin >> nthm;
   dst_Istr(pfin);
   gct = nthm;

   if(Gcom(1)){
      pfwd=get_Fstr("w",ios::app|ios::ate);
      pfad=get_Ostr("a",ios::app|ios::ate);
   }
   if(Gcom(2)){
      pfsz=get_Ostr("s",ios::app|ios::ate);
   }
}

void BTheme::write(Theme &Thm){
   long i=pfwd->tellp();
   pfad->write((char*)&i,sizeof(long));
   pfsz->write((char*)&(Thm.mtc),sizeof(long));

   Thm.bin_Write(*pfwd);
   gct++;
}

void BTheme::gclose_write(void){
   cflag=3;
   nthm=gct;
   ofstream *pfout=get_Ostr("n");
   *pfout << nthm << endl;
   dst_Ostr(pfout);

   if(Rcom(1)){
      dst_Ostr(pfad);
      dst_Fstr(pfwd);
   }

   if(Rcom(2))dst_Ostr(pfsz);
}

//Index set object

Index::Index(void){
   idx=NULL;
}

Index::Index(long num){
   ix=num;
   idx=new long[ix];
}

Index::Index(long begin,long end_plus_one){
   long i;
   ix=end_plus_one-begin;
   idx=new long[ix];
   for(i=begin;i<end_plus_one;i++)*(idx+i-begin)=i;
}

Index::Index(long num,long *imd,long oflag){
   long i;

   ix=num;
   idx=new long[ix];
   for(i=0;i<num;i++)*(idx+i)=*(imd+i);
   if(oflag)sSort();
   if(oflag>1)unique();
}

Index::Index(Index *ind){
   long i;

   ix=ind->ix;
   idx=new long[ix];
   for(i=0;i<ix;i++)idx[i]=ind->idx[i];
}


Index::~Index(void){
   if(idx!=NULL)delete [] idx;
}

void Index::sSort(void){
  long k, j, ir, i;
  long rra;

  if(ix<2)return;

  k=(ix>>1);
  ir=ix-1;
  for(;;) {
    if(k>0) {
      rra=idx[--k];
    }
    else {
      rra=idx[ir];
      idx[ir] = idx[0];
      if(--ir ==0) {
        idx[0]=rra;
        return;
      }
    }
    i=k;
    j=((k+1)<<1)-1;
    while(j<=ir) {
      if(j<ir && (idx[j]<idx[j+1])) ++j;
      if(rra<idx[j]) {
        idx[i]=idx[j];
        j +=(i=j)+1;
      }
      else j=ir+1;
    }
    idx[i]=rra;
  }
}

long Index::unique(void){
   long i,j,k;

   if(ix<2)return(ix);
   k=1;
   j=*idx;
   for(i=1;i<ix;i++){
      if(j<*(idx+i))*(idx+(k++))=j=*(idx+i);
   }
   if(k<ix){
      long *jdx=new long[k];
      for(i=0;i<k;i++)*(jdx+i)=*(idx+i);
      delete [] idx;
      idx=jdx;
      ix=k;
   }
   return(k);
}

Index *Index::cbool_And(Index *jnd){
   long i,j,k,su,w,p,m,bu;
   long *pdx,*bdx,*sdx,bx,sx;

   if(jnd==NULL){
      return(NULL);
   }
   if(ix>jnd->ix){
      bdx=idx;
      bx=ix;
      sdx=jnd->idx;
      sx=jnd->ix;
   }
   else {
      bdx=jnd->idx;
      bx=jnd->ix;
      sdx=idx;
      sx=ix;
   }
   pdx=new long[sx];
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+k)=bu;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+k)=su;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+k)=su;
                     k++;
                  }
                  i++;j++;
               }
            }
         }
      }
   }
   if(k==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(k);
      for(i=0;i<k;i++)*(pnd->idx+i)=*(pdx+i);
      delete [] pdx;
      return(pnd);
   }
}

Index *Index::cbool_Or(Index *jnd){
   long i,j,k,*pdx,bx,ii,jj;
   long *iix,*jjx,iu,ju;

   if(jnd==NULL){
      Index *pnd=new Index(ix,idx,0);
      return(pnd);
   }

   ii=ix;
   iix=idx;
   jj=jnd->ix;
   jjx=jnd->idx;
   bx=ii+jj;
   pdx=new long[bx];
   i=j=k=0;
   while((i<ii)&&(j<jj)){
      ju=*(jjx+j);
      while((i<ii)&&((iu=*(iix+i))<ju)){
         *(pdx+k)=iu;
         k++;i++;
      }
      if(i<ii){
         if(iu==ju){
            *(pdx+k)=iu;
            k++;i++;j++;
         }
         else {
            while((j<jj)&&(iu>(ju=*(jjx+j)))){
               *(pdx+k)=ju;
               k++;j++;
            }
            if(j<jj){
               if(iu==ju){
                  *(pdx+k)=iu;
                  k++;i++;j++;
               }
            }
         }
      }
   }
   while(i<ii){
      *(pdx+k)=*(iix+i);
      k++;i++;
   }
   while(j<jj){
      *(pdx+k)=*(jjx+j);
      k++;j++;
   }

   if(k==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(k);
      for(i=0;i<k;i++)*(pnd->idx+i)=*(pdx+i);
      delete [] pdx;
      return(pnd);
   }
}

Index *Index::cbool_Butnot(Index *jnd){
   long i,j,k,su,w,p,m,bu,flab;
   long *pdx,*bdx,*sdx,bx,sx;

   if(jnd==NULL){
      Index *pnd=new Index(ix,idx,0);
      return(pnd);
   }

   if(ix>jnd->ix){
      bdx=idx;
      bx=ix;
      sdx=jnd->idx;
      sx=jnd->ix;
      flab=1;
   }
   else {
      bdx=jnd->idx;
      bx=jnd->ix;
      sdx=idx;
      sx=ix;
      flab=0;
   }
   pdx=new long[ix];
   for(i=0;i<ix;i++)*(pdx+i)=1; //Initialize as marker.
   if(flab){ //Case ind is big.
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+j)=0;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+j+w)=0;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+j)=0;
                     k++;
                  }
                  i++;j++;
               }
            }
         }
      }
   }
   } //End of case ind is big.
   else { //Case ind is small.
   i=j=k=0;
   while((i<sx)&&(j<bx)){
      bu=*(bdx+j);
      while((i<sx)&&(*(sdx+i)<bu))i++;
      if(i<sx){
         su=*(sdx+i);
         if(su==bu){
            *(pdx+i)=0;
            k++;j++;i++;
         }
         else {
            if(bx-j>sx-i)w=(bx-j)/(sx-i);
            else w=1;
            while((j+w<bx)&&(su>*(bdx+j+w)))j+=w;
            if(j+w>=bx){
               w=bx-j-1;
               if(su>*(bdx+j+w))i=sx;
            }
            if(i<sx){
               if(su==*(bdx+j+w)){
                  *(pdx+i)=0;
                  k++;i++;j+=w+1;
               }
               else {
                  p=j+w;
                  while(p-j>1){
                     m=(j+p)/2;
                     if(su<*(bdx+m))p=m;
                     else j=m;
                  }
                  if(su==*(bdx+j)){
                     *(pdx+i)=0;
                     k++;
                  }
                  i++;j++;
               }
            }
         }
      }
   }
   } //End of case ind is small.

   j=ix-k;
   if(k==0){
      delete [] pdx;
      Index *pnd=new Index(ix,idx,0);
      return(pnd);
   }
   else if(j==0){
      delete [] pdx;
      return(NULL);
   }
   else {
      Index *pnd=new Index(j);
      j=0;
      for(i=0;i<ix;i++){
         if(*(pdx+i)){
            *(pnd->idx+j)=*(idx+i);
            j++;
         }
      }
      delete [] pdx;
      return(pnd);
   }
}

long Index::Subvalue(long j){
   long x,y,i,k;
   
   if(j<=(k=idx[0])){
      if(j!=k)return(0);
      else return(1);
   }
   if(j>=(k=idx[ix-1])){
      if(j!=k)return(0);
      else return(ix);
   }
   x=0;
   y=ix-1;
   if(y==1)return(0);

   while(y-x>1){
      i=(y+x)/2;
      if(j>(k=idx[i]))x=i;
      else if(j<k)y=i;
      else return(i+1);
   }
   return(0);
}

Index *Index::Subvalue(Index *jnd){
   if(jnd==NULL)return(NULL);
   else if(jnd->ix==0)return(NULL);

   Index *knd=new Index(jnd->ix);

   long i=0,j=0,k;
   while(j<jnd->ix){
      k=jnd->idx[j];
      while(idx[i]<k)i++;
      knd->idx[(j++)]=i;
   }
 
   return(knd);
}

Index *Index::Subinterval(long n,long m){
   if(m<=n)return(NULL);
   if(n<0)return(NULL);
   if(ix<m)return(NULL);
   Index *pind=new Index(m-n);
   long i,*ptr;
   ptr=pind->idx;
   for(i=n;i<m;i++)*(ptr++)=*(idx+i);
   return(pind);
}

Index *Index::Subsample(long n,long seed){
   long i,j,*ptr;

   if(n>=ix){
      Index *pnd=new Index(ix,idx,0);
      return(pnd);
   }

   srandom((unsigned int)seed);

   long *udx=new long[ix];
   ptr=udx;
   for(i=0;i<n;i++)*(ptr++)=1;
   for(i=n;i<ix;i++)*(ptr++)=0;
   shuffle(ix,udx);
   Index *rind=new Index(n);
   j=0;
   ptr=udx;
   for(i=0;i<ix;i++){
      if(*(ptr++))*(rind->idx+(j++))=*(idx+i);
   }
   delete [] udx;
   return(rind);
}

Index *Index::Greater(float *sxx,float thresh){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)>thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)>thresh)ind->idx[ct++]=idx[i];
   }
   return(ind);
}

Index *Index::Greateq(float *sxx,float thresh){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)>=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)>=thresh)ind->idx[ct++]=idx[i];
   }
   return(ind);
}

Index *Index::Greater(double *sxx,double thresh){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)>thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)>thresh)ind->idx[ct++]=idx[i];
   }
   return(ind);
}

Index *Index::Greateq(double *sxx,double thresh){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)>=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)>=thresh)ind->idx[ct++]=idx[i];
   }
   return(ind);
}

Index *Index::Lesser(float *sxx,float thresh){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)<thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)<thresh)ind->idx[ct++]=idx[i];
   }
   return(ind);
}

Index *Index::Lesseq(float *sxx,float thresh){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)<=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)<=thresh)ind->idx[ct++]=idx[i];
   }
   return(ind);
}

Index *Index::Lesser(double *sxx,double thresh){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)<thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)<thresh)ind->idx[ct++]=idx[i];
   }
   return(ind);
}

Index *Index::Lesseq(double *sxx,double thresh){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)<=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+i)<=thresh)ind->idx[ct++]=idx[i];
   }
   return(ind);
}

//sxx[idx[]]
Index *Index::Greater(float thresh,float *sxx){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])>thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+(k=idx[i]))>thresh)ind->idx[ct++]=k;
   }
   return(ind);
}

Index *Index::Greateq(float thresh,float *sxx){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])>=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+(k=idx[i]))>=thresh)ind->idx[ct++]=k;
   }
   return(ind);
}

Index *Index::Greater(double thresh,double *sxx){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])>thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+(k=idx[i]))>thresh)ind->idx[ct++]=k;
   }
   return(ind);
}

Index *Index::Greateq(double thresh,double *sxx){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])>=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+(k=idx[i]))>=thresh)ind->idx[ct++]=k;
   }
   return(ind);
}

Index *Index::Lesser(float thresh,float *sxx){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])<thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+(k=idx[i]))<thresh)ind->idx[ct++]=k;
   }
   return(ind);
}

Index *Index::Lesseq(float thresh,float *sxx){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])<=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+(k=idx[i]))<=thresh)ind->idx[ct++]=k;
   }
   return(ind);
}

Index *Index::Lesser(double thresh,double *sxx){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])<thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+(k=idx[i]))<thresh)ind->idx[ct++]=k;
   }
   return(ind);
}

Index *Index::Lesseq(double thresh,double *sxx){
   long i,k,ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])<=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   ct=0;
   for(i=0;i<ix;i++){
      if(*(sxx+(k=idx[i]))<=thresh)ind->idx[ct++]=k;
   }
   return(ind);
}

Order *Index::oGreater(float thresh,float *sxx){
   long i,k,ct=0;
   float ss;

   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])>thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   long *ord=new long[ct];
   float *scx=new float[ct];
   ct=0;
   for(i=0;i<ix;i++){
      if((ss=*(sxx+(k=idx[i])))>thresh){
         scx[ct]=ss;
         ord[ct]=ct;
         ind->idx[ct++]=k;
      }
   }
   if(ct>1)hRort(ct,scx,ord);
   Order *jOrd=new Order;
   jOrd->pInd=ind;
   jOrd->order=ord;
   jOrd->score=scx;
   return(jOrd);
}

Order *Index::oGreateq(float thresh,float *sxx){
   long i,k,ct=0;
   float ss;

   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])>=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   long *ord=new long[ct];
   float *scx=new float[ct];
   ct=0;
   for(i=0;i<ix;i++){
      if((ss=*(sxx+(k=idx[i])))>=thresh){
         scx[ct]=ss;
         ord[ct]=ct;
         ind->idx[ct++]=k;
      }
   }
   if(ct>1)hRort(ct,scx,ord);
   Order *jOrd=new Order;
   jOrd->pInd=ind;
   jOrd->order=ord;
   jOrd->score=scx;
   return(jOrd);
}

Order *Index::oGreater(double thresh,double *sxx){
   long i,k,ct=0;
   double ss;

   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])>thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   long *ord=new long[ct];
   float *scx=new float[ct];
   ct=0;
   for(i=0;i<ix;i++){
      if((ss=*(sxx+(k=idx[i])))>thresh){
         scx[ct]=(float)ss;
         ord[ct]=ct;
         ind->idx[ct++]=k;
      }
   }
   if(ct>1)hRort(ct,scx,ord);
   Order *jOrd=new Order;
   jOrd->pInd=ind;
   jOrd->order=ord;
   jOrd->score=scx;
   return(jOrd);
}

Order *Index::oGreateq(double thresh,double *sxx){
   long i,k,ct=0;
   double ss;

   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])>=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   long *ord=new long[ct];
   float *scx=new float[ct];
   ct=0;
   for(i=0;i<ix;i++){
      if((ss=*(sxx+(k=idx[i])))>=thresh){
         scx[ct]=(float)ss;
         ord[ct]=ct;
         ind->idx[ct++]=k;
      }
   }
   if(ct>1)hRort(ct,scx,ord);
   Order *jOrd=new Order;
   jOrd->pInd=ind;
   jOrd->order=ord;
   jOrd->score=scx;
   return(jOrd);
}

Order *Index::oLesser(float thresh,float *sxx){
   long i,k,ct=0;
   float ss;

   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])<thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   long *ord=new long[ct];
   float *scx=new float[ct];
   ct=0;
   for(i=0;i<ix;i++){
      if((ss=*(sxx+(k=idx[i])))<thresh){
         scx[ct]=ss;
         ord[ct]=ct;
         ind->idx[ct++]=k;
      }
   }
   if(ct>1)hRort(ct,scx,ord);
   Order *jOrd=new Order;
   jOrd->pInd=ind;
   jOrd->order=ord;
   jOrd->score=scx;
   return(jOrd);
}

Order *Index::oLesseq(float thresh,float *sxx){
   long i,k,ct=0;
   float ss;

   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])<=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   long *ord=new long[ct];
   float *scx=new float[ct];
   ct=0;
   for(i=0;i<ix;i++){
      if((ss=*(sxx+(k=idx[i])))<=thresh){
         scx[ct]=ss;
         ord[ct]=ct;
         ind->idx[ct++]=k;
      }
   }
   if(ct>1)hRort(ct,scx,ord);
   Order *jOrd=new Order;
   jOrd->pInd=ind;
   jOrd->order=ord;
   jOrd->score=scx;
   return(jOrd);
}

Order *Index::oLesser(double thresh,double *sxx){
   long i,k,ct=0;
   double ss;

   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])<thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   long *ord=new long[ct];
   float *scx=new float[ct];
   ct=0;
   for(i=0;i<ix;i++){
      if((ss=*(sxx+(k=idx[i])))<thresh){
         scx[ct]=(float)ss;
         ord[ct]=ct;
         ind->idx[ct++]=k;
      }
   }
   if(ct>1)hRort(ct,scx,ord);
   Order *jOrd=new Order;
   jOrd->pInd=ind;
   jOrd->order=ord;
   jOrd->score=scx;
   return(jOrd);
}

Order *Index::oLesseq(double thresh,double *sxx){
   long i,k,ct=0;
   double ss;

   for(i=0;i<ix;i++){
      if(*(sxx+idx[i])<=thresh)ct++;
   }
   if(!ct)return(NULL);
   Index *ind=new Index(ct);
   long *ord=new long[ct];
   float *scx=new float[ct];
   ct=0;
   for(i=0;i<ix;i++){
      if((ss=*(sxx+(k=idx[i])))<=thresh){
         scx[ct]=(float)ss;
         ord[ct]=ct;
         ind->idx[ct++]=k;
      }
   }
   if(ct>1)hRort(ct,scx,ord);
   Order *jOrd=new Order;
   jOrd->pInd=ind;
   jOrd->order=ord;
   jOrd->score=scx;
   return(jOrd);
}

void Index::Convert_to_pmid(Regist *pReg){
   long i;
   pReg->gopen_map(READ_U);

   for(i=0;i<ix;i++)idx[i]=pReg->pmid(idx[i]);
}

void Index::Convert_to_pmid(Regist_pth *pReg){
   long i;
   pReg->gopen_map(READ_U);

   for(i=0;i<ix;i++)idx[i]=pReg->pmid(idx[i]);
}

void Index::Convert_to_index(Regist *pReg){
   long i,j=0,k;
   pReg->gopen_map(READ_U);

   for(i=0;i<ix;i++){
      if(k=pReg->index(idx[i]))idx[j++]=k-1;
   }
   if(j<ix){
      cout << ix-j << " pmids not found" << endl;      
      ix=j;
   }
}

void Index::Convert_to_index(Regist_pth *pReg){
   long i,j=0,k;
   pReg->gopen_map(READ_U);

   for(i=0;i<ix;i++){
      if(k=pReg->index(idx[i]))idx[j++]=k-1;
   }
   if(j<ix){      
      cout << ix-j << " pmids not found" << endl;
      ix=j;
   }
}

void Index::write(ofstream &fout){
   long i;

   fout << ix << endl;
   for(i=0;i<ix;i++){
      fout << " " << *(idx+i) << endl;
   }
}

void Index::read(ifstream &fin){
   long i;

   if(idx!=NULL)delete [] idx;

   fin >> ix;
   idx=new long[ix];
   for(i=0;i<ix;i++){
      fin >> idx[i];
   }
}

void Index::debug(void){
   long i;
  
   for(i=0;i<ix;i++)cout << i << " " << idx[i] << endl;
}

//Cross validation

CValid::CValid(Index *gdd, long n){
   ind=new Index(0,n);
   gnd=new Index(gdd);
   bnd=ind->cbool_Butnot(gnd);


}

CValid::CValid(Index *gdd, Index *bdd){
   gnd=new Index(gdd);
   bnd=new Index(bdd);
   ind=gnd->cbool_Or(bnd);

}


void CValid::cross_valid(long m, long seed){

   long i,j,k,size,blk,rem;
   setn=m;

   pBTS=new Index* [setn];
   pGTS=new Index* [setn];
   pWTS=new Index* [setn];
   pBTR=new Index* [setn];
   pGTR=new Index* [setn];
   pWTR=new Index* [setn];

   size=gnd->ix;
   if(size<setn) {
      cout <<"Size of Relevant set is smaller than the the number of cross validation"<<endl;
      exit(0);
   }

   long *sizg, *sizb;

   sizg=new long[setn];
   blk=size/setn;
   rem=size%setn;

   for(i=0;i<setn;i++){
      if(i<rem){sizg[i]=blk+1;}
      else {sizg[i]=blk;}
   }

   sizb=new long[setn];
   size=bnd->ix;

   if(size<setn) {
      cout <<"Size of Non-Relevant set is smaller than the number of cross validations"<<endl;
      exit(0);
   }


   blk=size/setn;
   rem=size%setn;
   k=0;
   for(i=0;i<setn;i++){
      if(i<rem){sizb[i]=blk+1;}
      else {sizb[i]=blk;}
   }



   Index *tmp1=new Index(gnd);
   Index *tmp2=new Index(bnd);
   Index *tmp11,*tmp22;
   for(i=0;i<setn;i++){

      pGTS[i]=tmp1->Subsample(sizg[i],seed);
      pBTS[i]=tmp2->Subsample(sizb[i],seed);
      pWTS[i]=pGTS[i]->cbool_Or(pBTS[i]);

      pGTR[i]=gnd->cbool_Butnot(pGTS[i]);
      pBTR[i]=bnd->cbool_Butnot(pBTS[i]);
      pWTR[i]=pGTR[i]->cbool_Or(pBTR[i]);

      tmp11=tmp1->cbool_Butnot(pGTS[i]);
      delete tmp1;
      tmp1=NULL;
      if(tmp11) {
         tmp1=new Index(tmp11);
         delete tmp11;
         tmp11=NULL;
      }
      tmp22=tmp2->cbool_Butnot(pBTS[i]);
      delete tmp2;
      tmp2=NULL;
      if(tmp22){
         tmp2=new Index(tmp22);
         delete tmp22;
         tmp22=NULL;
      }


   }

   delete tmp1;
   delete tmp2;
   if(tmp11) delete tmp11;
   if(tmp22) delete tmp22;
   delete [] sizg;
   delete [] sizb;

}

CValid::~CValid(){
   for(long i=0;i<setn;i++){
      delete pGTS[i];
      delete pBTS[i];
      delete pWTS[i];
      delete pGTR[i];
      delete pBTR[i];
      delete pWTR[i];
   }
   delete [] pGTS;
   delete [] pBTS;
   delete [] pWTS;
   delete [] pGTR;
   delete [] pBTR;
   delete [] pWTR;  

   if (ind) delete ind;
   if (gnd) delete gnd;
   if (bnd) delete bnd;
}

//Scoring order object.

Order::Order(void){
   pInd=NULL;
   order=NULL;
   score=NULL;
}

Order::Order(long n,long m,float *sco){
   long i,j,k,*pt,ir,ii;
   long u,*idx,*ord,*inv;
   float *scx,ss,xx,*bt,*dt;
   n=(m<n)?m:n;

   if(n<2){
      if(n<1){
         pInd=NULL;
         order=NULL;
         score=NULL;
         return;
      }
      ss=*sco;
      i=1;
      j=0;
      while(i<m){
         xx=sco[i];
         if(ss<xx){
            j=i;
            ss=xx;
         }
         i++;
      }
      pInd->idx=new long[1];
      *(pInd->idx)=j;
      pInd->ix=1;
      order=new long[1];
      score=new float[1];
      *order=0;
      *score=ss;
      return;
   }

   pInd=new Index(n);
   scx=new float[n];

   pt=idx=pInd->idx;
   bt=scx;
   dt=sco;
   for(i=0;i<n;i++){
      *(pt++)=i;
      *(bt++)=*(dt++);
   }

   //Build the initial heap
   k=(n>>1);
   ir=n-1;
   while(k){
      ss=scx[(--k)];
      ii=idx[k];

      i=k;
      j=((k+1)<<1)-1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }
   //Filter the remaining points into heap
   xx=*scx;
   for(u=n;u<m;u++){
      if((ss=*(dt++))>xx){
         ii=u;
         i=0;
         j=1;
         while(j<=ir){
            if(j<ir && scx[j]>scx[j+1])++j;
            if(ss>scx[j]){
               scx[i]=scx[j];
               idx[i]=idx[j];
               j+=(i=j)+1;
            }
            else j=ir+1;
         }
         scx[i]=ss;
         idx[i]=ii;
         xx=*scx;
      }
   }
   //Order the heap by promotion & filtering
   for(;;){
      ss=scx[ir];
      ii=idx[ir];
      scx[ir]=scx[0];
      idx[ir]=idx[0];
      if((--ir)==0){
         scx[0]=ss;
         idx[0]=ii;
         break;
      }
      i=0;
      j=1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }

   ord=new long[n];
   inv=new long[n];
   pt=inv;
   for(i=0;i<n;i++)*(pt++)=i;
   hSort(n,idx,inv);
   pt=inv;
   for(i=0;i<n;i++)*(ord+*(pt++))=i;
   delete [] inv;

   order=ord;
   score=scx;
}

Order::Order(long n,long m,double *sco){
   long i,j,k,*pt,ir,ii;
   long u,*idx,*ord,*inv;
   float *scx,ss,xx,*bt;
   double *dt;
   n=(m<n)?m:n;

   if(n<2){
      if(n<1){
         pInd=NULL;
         order=NULL;
         score=NULL;
         return;
      }
      ss=(float)*sco;
      i=1;
      j=0;
      while(i<m){
         xx=(float)sco[i];
         if(ss<xx){
            j=i;
            ss=xx;
         }
         i++;
      }
      pInd->idx=new long[1];
      *(pInd->idx)=j;
      pInd->ix=1;
      order=new long[1];
      score=new float[1];
      *order=0;
      *score=ss;
      return;
   }

   pInd=new Index(n);
   scx=new float[n];

   pt=idx=pInd->idx;
   bt=scx;
   dt=sco;
   for(i=0;i<n;i++){
      *(pt++)=i;
      *(bt++)=(float)*(dt++);
   }

   //Build the initial heap
   k=(n>>1);
   ir=n-1;
   while(k){
      ss=scx[(--k)];
      ii=idx[k];

      i=k;
      j=((k+1)<<1)-1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }
   //Filter the remaining points into heap
   xx=*scx;
   for(u=n;u<m;u++){
      if((ss=*(dt++))>xx){
         ii=u;
         i=0;
         j=1;
         while(j<=ir){
            if(j<ir && scx[j]>scx[j+1])++j;
            if(ss>scx[j]){
               scx[i]=scx[j];
               idx[i]=idx[j];
               j+=(i=j)+1;
            }
            else j=ir+1;
         }
         scx[i]=ss;
         idx[i]=ii;
         xx=*scx;
      }
   }
   //Order the heap by promotion & filtering
   for(;;){
      ss=scx[ir];
      ii=idx[ir];
      scx[ir]=scx[0];
      idx[ir]=idx[0];
      if((--ir)==0){
         scx[0]=ss;
         idx[0]=ii;
         break;
      }
      i=0;
      j=1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }

   ord=new long[n];
   inv=new long[n];
   pt=inv;
   for(i=0;i<n;i++)*(pt++)=i;
   hSort(n,idx,inv);
   pt=inv;
   for(i=0;i<n;i++)*(ord+*(pt++))=i;
   delete [] inv;

   order=ord;
   score=scx;
}

Order::Order(long n,Index *ind,float *sco){
   long i,j,k,*pt,ir,ii;
   long u,*idx,*ord,*inv;
   float *scx,ss,xx,*bt,*dt;
   long m=ind->ix,*udx=ind->idx;
   n=(m<n)?m:n;

   if(n<2){
      if(n<1){
         pInd=NULL;
         order=NULL;
         score=NULL;
         return;
      }
      ss=sco[udx[0]];
      i=1;
      j=0;
      while(i<m){
         xx=sco[udx[i]];
         if(ss<xx){
            j=udx[i];
            ss=xx;
         }
         i++;
      }
      pInd->idx=new long[1];
      *(pInd->idx)=j;
      pInd->ix=1;
      order=new long[1];
      score=new float[1];
      *order=0;
      *score=ss;
      return;
   }

   pInd=new Index(n);
   scx=new float[n];

   pt=idx=pInd->idx;
   bt=scx;
   for(i=0;i<n;i++){
      *(pt++)=udx[i];
      *(bt++)=sco[udx[i]];
   }

   //Build the initial heap
   k=(n>>1);
   ir=n-1;
   while(k){
      ss=scx[(--k)];
      ii=idx[k];

      i=k;
      j=((k+1)<<1)-1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }
   //Filter the remaining points into heap
   xx=*scx;
   for(u=n;u<m;u++){
      if((ss=sco[udx[u]])>xx){
         ii=udx[u];
         i=0;
         j=1;
         while(j<=ir){
            if(j<ir && scx[j]>scx[j+1])++j;
            if(ss>scx[j]){
               scx[i]=scx[j];
               idx[i]=idx[j];
               j+=(i=j)+1;
            }
            else j=ir+1;
         }
         scx[i]=ss;
         idx[i]=ii;
         xx=*scx;
      }
   }
   //Order the heap by promotion & filtering
   for(;;){
      ss=scx[ir];
      ii=idx[ir];
      scx[ir]=scx[0];
      idx[ir]=idx[0];
      if((--ir)==0){
         scx[0]=ss;
         idx[0]=ii;
         break;
      }
      i=0;
      j=1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }

   ord=new long[n];
   inv=new long[n];
   pt=inv;
   for(i=0;i<n;i++)*(pt++)=i;
   hSort(n,idx,inv);
   pt=inv;
   for(i=0;i<n;i++)*(ord+*(pt++))=i;
   delete [] inv;

   order=ord;
   score=scx;
}

Order::Order(long n,Index *ind,double *sco){
   long i,j,k,*pt,ir,ii;
   long u,*idx,*ord,*inv;
   float *scx,ss,xx,*bt;
   double *dt;
   long m=ind->ix,*udx=ind->idx;
   n=(m<n)?m:n;

   if(n<2){
      if(n<1){
         pInd=NULL;
         order=NULL;
         score=NULL;
         return;
      }
      ss=(float)sco[udx[0]];
      i=1;
      j=0;
      while(i<m){
         xx=(float)sco[udx[i]];
         if(ss<xx){
            j=udx[i];
            ss=xx;
         }
         i++;
      }
      pInd->idx=new long[1];
      *(pInd->idx)=j;
      pInd->ix=1;
      order=new long[1];
      score=new float[1];
      *order=0;
      *score=ss;
      return;
   }

   pInd=new Index(n);
   scx=new float[n];

   pt=idx=pInd->idx;
   bt=scx;
   for(i=0;i<n;i++){
      *(pt++)=udx[i];
      *(bt++)=(float)sco[udx[i]];
   }

   //Build the initial heap
   k=(n>>1);
   ir=n-1;
   while(k){
      ss=scx[(--k)];
      ii=idx[k];

      i=k;
      j=((k+1)<<1)-1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }
   //Filter the remaining points into heap
   xx=*scx;
   for(u=n;u<m;u++){
      if((ss=(float)sco[udx[u]])>xx){
         ii=udx[u];
         i=0;
         j=1;
         while(j<=ir){
            if(j<ir && scx[j]>scx[j+1])++j;
            if(ss>scx[j]){
               scx[i]=scx[j];
               idx[i]=idx[j];
               j+=(i=j)+1;
            }
            else j=ir+1;
         }
         scx[i]=ss;
         idx[i]=ii;
         xx=*scx;
      }
   }
   //Order the heap by promotion & filtering
   for(;;){
      ss=scx[ir];
      ii=idx[ir];
      scx[ir]=scx[0];
      idx[ir]=idx[0];
      if((--ir)==0){
         scx[0]=ss;
         idx[0]=ii;
         break;
      }
      i=0;
      j=1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }

   ord=new long[n];
   inv=new long[n];
   pt=inv;
   for(i=0;i<n;i++)*(pt++)=i;
   hSort(n,idx,inv);
   pt=inv;
   for(i=0;i<n;i++)*(ord+*(pt++))=i;
   delete [] inv;

   order=ord;
   score=scx;
}

Order::Order(long n,float *sco,Index *ind){
   long i,j,k,*pt,*qt,ir,ii;
   long u,*idx,*ord,*inv;
   float *scx,ss,xx,*bt,*dt;
   n=((ind->ix)<n)?(ind->ix):n;

   if(n<2){
      if(n<1){
         pInd=NULL;
         order=NULL;
         score=NULL;
         return;
      }
      ss=*sco;
      i=1;
      j=0;
      while(i<ind->ix){
         xx=sco[i];
         if(ss<xx){
            j=i;
            ss=xx;
         }
         i++;
      }
      pInd->idx=new long[1];
      *(pInd->idx)=ind->idx[j];
      pInd->ix=1;
      order=new long[1];
      score=new float[1];
      *order=0;
      *score=ss;
      return;
   }

   pInd=new Index(n);
   scx=new float[n];

   pt=idx=pInd->idx;
   qt=ind->idx;
   bt=scx;
   dt=sco;
   for(i=0;i<n;i++){
      *(pt++)=*(qt++);
      *(bt++)=*(dt++);
   }

   //Build the initial heap
   k=(n>>1);
   ir=n-1;
   while(k){
      ss=scx[(--k)];
      ii=idx[k];

      i=k;
      j=((k+1)<<1)-1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }
   //Filter the remaining points into heap
   xx=*scx;
   for(u=n;u<ind->ix;u++){
      if((ss=*(dt++))>xx){
         ii=ind->idx[u];
         i=0;
         j=1;
         while(j<=ir){
            if(j<ir && scx[j]>scx[j+1])++j;
            if(ss>scx[j]){
               scx[i]=scx[j];
               idx[i]=idx[j];
               j+=(i=j)+1;
            }
            else j=ir+1;
         }
         scx[i]=ss;
         idx[i]=ii;
         xx=*scx;
      }
   }
   //Order the heap by promotion & filtering
   for(;;){
      ss=scx[ir];
      ii=idx[ir];
      scx[ir]=scx[0];
      idx[ir]=idx[0];
      if((--ir)==0){
         scx[0]=ss;
         idx[0]=ii;
         break;
      }
      i=0;
      j=1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }

   ord=new long[n];
   inv=new long[n];
   pt=inv;
   for(i=0;i<n;i++)*(pt++)=i;
   hSort(n,idx,inv);
   pt=inv;
   for(i=0;i<n;i++)*(ord+*(pt++))=i;
   delete [] inv;

   order=ord;
   score=scx;
}

Order::Order(long n,double *sco,Index *ind){
   long i,j,k,*pt,*qt,ir,ii;
   long u,*idx,*ord,*inv;
   float *scx,ss,xx,*bt;
   double *dt;
   n=((ind->ix)<n)?(ind->ix):n;

   if(n<2){
      if(n<1){
         pInd=NULL;
         order=NULL;
         score=NULL;
         return;
      }
      ss=(float)*sco;
      i=1;
      j=0;
      while(i<ind->ix){
         xx=(float)sco[i];
         if(ss<xx){
            j=i;
            ss=xx;
         }
         i++;
      }
      pInd->idx=new long[1];
      *(pInd->idx)=ind->idx[j];
      pInd->ix=1;
      order=new long[1];
      score=new float[1];
      *order=0;
      *score=ss;
      return;
   }

   pInd=new Index(n);
   scx=new float[n];

   pt=idx=pInd->idx;
   qt=ind->idx;
   bt=scx;
   dt=sco;
   for(i=0;i<n;i++){
      *(pt++)=*(qt++);
      *(bt++)=(float)*(dt++);
   }

   //Build the initial heap
   k=(n>>1);
   ir=n-1;
   while(k){
      ss=scx[(--k)];
      ii=idx[k];

      i=k;
      j=((k+1)<<1)-1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }
   //Filter the remaining points into heap
   xx=*scx;
   for(u=n;u<ind->ix;u++){
      if((ss=(float)*(dt++))>xx){
         ii=ind->idx[u];
         i=0;
         j=1;
         while(j<=ir){
            if(j<ir && scx[j]>scx[j+1])++j;
            if(ss>scx[j]){
               scx[i]=scx[j];
               idx[i]=idx[j];
               j+=(i=j)+1;
            }
            else j=ir+1;
         }
         scx[i]=ss;
         idx[i]=ii;
         xx=*scx;
      }
   }
   //Order the heap by promotion & filtering
   for(;;){
      ss=scx[ir];
      ii=idx[ir];
      scx[ir]=scx[0];
      idx[ir]=idx[0];
      if((--ir)==0){
         scx[0]=ss;
         idx[0]=ii;
         break;
      }
      i=0;
      j=1;
      while(j<=ir){
         if(j<ir && scx[j]>scx[j+1])++j;
         if(ss>scx[j]){
            scx[i]=scx[j];
            idx[i]=idx[j];
            j+=(i=j)+1;
         }
         else j=ir+1;
      }
      scx[i]=ss;
      idx[i]=ii;
   }

   ord=new long[n];
   inv=new long[n];
   pt=inv;
   for(i=0;i<n;i++)*(pt++)=i;
   hSort(n,idx,inv);
   pt=inv;
   for(i=0;i<n;i++)*(ord+*(pt++))=i;
   delete [] inv;

   order=ord;
   score=scx;
}

Order::Order(Order *pOrd){
   long i;

   pInd=new Index(pOrd->pInd);
   order=new long[pInd->ix];
   score=new float[pInd->ix];
   for(i=0;i<pInd->ix;i++){
      order[i]=pOrd->order[i];
      score[i]=pOrd->score[i];
   }
}

Order::~Order(void){
   if(pInd!=NULL)delete pInd;
   if(order!=NULL)delete [] order;
   if(score!=NULL)delete [] score;
}

long Order::num(void){
   return(pInd->ix);
}

long Order::ind(long i,float &sco){
   sco=*(score+i);
   return(*(pInd->idx+*(order+i)));
}

long *Order::seq(void){
   long *ssq=new long[pInd->ix];
   long i;
   for(i=0;i<pInd->ix;i++)ssq[i]=*(pInd->idx+*(order+i));
   return(ssq);
}

Order *Order::cbool_And(Index *jnd){
   long i,j,k,*sub;

   Index *pind=pInd->cbool_And(jnd);
   if(pind==NULL)return(NULL);
   if(pind->ix<1)return(NULL);

   sub=new long[pInd->ix];
   for(i=0;i<pInd->ix;i++)sub[i]=0;
   long *pi=pInd->idx;
   long *pj=pind->idx;
   i=j=0;
   while(j<pind->ix){
      while(pi[i]<pj[j])i++;
      sub[i]=j+1;
      j++;
   }
   Order *psub=new Order;
   psub->pInd=pind;
   psub->order=new long[pind->ix];
   psub->score=new float[pind->ix];
   j=0;
   for(i=0;i<pInd->ix;i++){
      if((k=sub[order[i]])>0){
         psub->order[j]=k-1;
         psub->score[j]=score[i];
         j++;
      }
   }
   delete [] sub;
   return(psub);
}

Order *Order::cbool_Butnot(Index *jnd){
   long i,j,k,*sub;

   Index *pind=pInd->cbool_Butnot(jnd);
   if(pind==NULL)return(NULL);
   if(pind->ix<1)return(NULL);

   sub=new long[pInd->ix];
   for(i=0;i<pInd->ix;i++)sub[i]=0;
   long *pi=pInd->idx;
   long *pj=pind->idx;
   i=j=0;
   while(j<pind->ix){
      while(pi[i]<pj[j])i++;
      sub[i]=j+1;
      j++;
   }
   Order *psub=new Order;
   psub->pInd=pind;
   psub->order=new long[pind->ix];
   psub->score=new float[pind->ix];
   j=0;
   for(i=0;i<pInd->ix;i++){
      if((k=sub[order[i]])>0){
         psub->order[j]=k-1;
         psub->score[j]=score[i];
         j++;
      }
   }
   delete [] sub;
   return(psub);
}

float Order::Precision(long n,Index *ind){
   if(n>pInd->ix)return(0);
   else if(!n)return(0);
   Index *jnd=pInd->Subvalue(ind);
   float cx=0.0,sx,ss,vx;
   long i,j,k=0;
   long *bz=new long[pInd->ix];
   for(i=0;i<pInd->ix;i++)bz[i]=0;
   for(i=0;i<jnd->ix;i++)bz[jnd->idx[i]]=1;
   sx=score[0];
   while(k<n){
      i=0;
      vx=0.0;
      while((k+i<pInd->ix)&&(score[k+i]==sx)){
         vx+=bz[order[k+i]];
         i++;
      }
      if(k+i<n){
         cx+=vx;
         k+=i;
         sx=score[k];
      }
      else {
         cx+=vx*(n-k)/((float)i);
         k+=i;
      }
   }
   delete jnd;
   delete [] bz;
   return(cx/((float)n));
}

long Order::CtGreateq(float thresh){
   float sss,si,sj;
   long i,j,k;

   i=0;
   ind(i,sss);
   if(sss<thresh)return(0);
   j=num()-1;
   ind(j,sss);
   if(sss>=thresh)return(j+1);
   while(j-i>1){
      k=(i+j)/2;
      ind(k,sss);
      if(sss>=thresh)i=k;
      else j=k;
   }
   return(i+1);
}

void Order::debug(void){
   long i;
   float ss;
   cout << this->num() << endl;
   for(i=0;i<this->num();i++)cout << i << " " << this->ind(i,ss) << \
      " " << ss << endl;
}

void Order::write(ofstream &fout){
   long i;

   fout << pInd->ix << endl;
   for(i=0;i<pInd->ix;i++){
      fout << " " << *(pInd->idx+i) << " " << *(order+i) << " " \
         << *(score+i) << endl;
   }
}

void Order::read(ifstream &fin){
   long i;

   if((pInd!=NULL)&&(pInd->idx!=NULL))delete [] (pInd->idx);
   if(order!=NULL)delete [] order;
   if(score!=NULL)delete [] score;

   fin >> i;
   if(!pInd)pInd=new Index(i);
   else {
      pInd->ix=i;
      pInd->idx=new long[i];
   }
   order=new long[i];
   score=new float[i];
   for(i=0;i<pInd->ix;i++){
      fin >> pInd->idx[i] >> order[i] >> score[i];
   }
}

}
