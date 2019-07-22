#include "Clip.h"
namespace iret {

Clip::Clip(long nm) : Count(){
   num=nm;
   pck=new char[num];
   nck=new long[num];
} 

Clip::~Clip(){
    if(pck!=NULL)delete [] pck;
    if(nck!=NULL)delete [] nck;
}  
 
void Clip::set1(void){
   long i,j,k;

   for(i=0;i<32;i++)zt[i]=0;
   for(i=32;i<48;i++)zt[i]=1;
   for(i=48;i<58;i++)zt[i]=2;
   for(i=58;i<65;i++)zt[i]=1;
   for(i=65;i<91;i++)zt[i]=3;
   for(i=91;i<97;i++)zt[i]=1;
   for(i=97;i<123;i++)zt[i]=3;
   for(i=123;i<127;i++)zt[i]=1;
   zt[127]=0;
}

void Clip::prc0(const char *pch){
   long i,j,k,len,jo,j2;
   char c;

   strcpy(pck,pch);
   len=strlen(pck);
   for(j=0;j<len;j++)if(zt[pck[j]]==0)pck[j]=32;
   j=0;
   while(j<len){
      while((j<len)&&(zt[pck[j]]<2))j++; //find beginning of string
      if(j<len){
         k=0;
         while(zt[pck[j+k]]>=2)k++; //Find end of string
         c=pck[j+k];
         pck[j+k]='\0';
         add_count2(pck+j,1);
         pck[j+k]=c;
         j+=k;
      }
   }
}

void Clip::prc1(const char *pch){
   long i,j,k,len,jo,j2;

   strcpy(pck,pch);
   i=0;
   while(pck[i]){
      if(!zt[pck[i]])pck[i]=32;
      i++;
   }

   i=j=0;
   while(pck[j]){ 
      while(zt[pck[i]]==1)i++;
      j=i;
      while(zt[pck[j]]>1)j++; 
      if(pck[j])j2=j+1;//Mark end of processing
      else j2=j;//so process ends at end of string
      while(j>i){
         pck[j]='\0';
         k=i;
         while(k<j){
            add_count2(pck+k,1);
            jo=zt[pck[k]];
            while((k<j)&&(zt[pck[k]]==jo))k++;
         }
         jo=zt[pck[j-1]];
         k=j-1;
         while((k>=i)&&(zt[pck[k]]==jo))k--;
         j=k+1;
      }
      i=j=j2;
   }
} 

void Clip::prc2(const char *pch,Hash &Hs){
   long i,j,k,len,jo,j2;

   len=strlen(pch);
   strcpy(pck,pch);
   for(j=0;j<len;j++)if(zt[pck[j]]==0)pck[j]=32;
   i=0;
   k=1;
   jo=zt[pck[0]];
   for(j=0;j<len;j++){
      if(zt[pck[j]]!=jo){k++;jo=zt[pck[j]];}
      if(zt[pck[j]]!=1){
         pck[i]=pck[j];
         nck[i]=k;
         i++;
      }
   }
   len=i;
   jo=nck[0];
   j=0;
   while(j<len){
      k=1;
      while((j+k<len)&&(nck[j+k]==jo)){nck[j+k]=0;k++;}
      nck[j]=1;
      j+=k;
      jo=nck[j];
   }
   j=len;
   while(j>0){
      pck[j]='\0';
      k=j-1;
      while(k>=0){
         while(!nck[k])k--;
         if(Hs.find(pck+k))add_count2(pck+k,1);
         k--;
      }
      j--;
      while(!nck[j])j--;
   }
}

void Clip::prc2(const char *pch,Chash &Cs,long llim){
   long i,j,k,len,jo,j2;

   len=strlen(pch);
   strcpy(pck,pch);
   for(j=0;j<len;j++)if(zt[pck[j]]==0)pck[j]=32;
   i=0;
   k=1;
   jo=zt[pck[0]];
   for(j=0;j<len;j++){
      if(zt[pck[j]]!=jo){k++;jo=zt[pck[j]];}
      if(zt[pck[j]]!=1){
         pck[i]=pck[j];
         nck[i]=k;
         i++;
      }
   }
   len=i;
   jo=nck[0];
   j=0;
   while(j<len){
      k=1;
      while((j+k<len)&&(nck[j+k]==jo)){nck[j+k]=0;k++;}
      nck[j]=1;
      j+=k;
      jo=nck[j];
   }
   j=len;
   while(j>0){
      pck[j]='\0';
      k=j-1;
      while(k>=0){
         while(!nck[k])k--;
         if((j2=Cs.count(pck+k))&&(j2<llim))add_count2(pck+k,1);
         k--;
      }
      j--;
      while(!nck[j])j--;
   }
}

void Clip::prc3(const char *pch,Hash &Hs){
   long i,j,k,len,jo,j2;

   len=strlen(pch);
   strcpy(pck,pch);
   for(j=0;j<len;j++)if(zt[pck[j]]==0)pck[j]=32;
   i=0;
   for(j=0;j<len;j++){
      if(zt[pck[j]]>1){
         pck[i]=pck[j];
         i++;
      }
   }
   len=i;
   j=len;
   while(j>0){
      pck[j]='\0';
      k=j-1;
      while(k>=0){
         if(Hs.find(pck+k))add_count2(pck+k,1);
         k--;
      }
      j--;
   }
}

void Clip::prc3(const char *pch,Chash &Cs,long llim){
   long i,j,k,len,jo,j2;

   len=strlen(pch);
   strcpy(pck,pch);
   for(j=0;j<len;j++)if(zt[pck[j]]==0)pck[j]=32;
   i=0;
   for(j=0;j<len;j++){
      if(zt[pck[j]]>1){
         pck[i]=pck[j];
         i++;
      }
   }
   len=i;
   j=len;
   while(j>0){
      pck[j]='\0';
      k=j-1;
      while(k>=0){
         if((jo=Cs.count(pck+k))&&(jo<llim))add_count2(pck+k,1);
         k--;
      }
      j--;
   }
}

//fill_grm functions
void Clip::fill_grm(const char *str,int gram_size,int aug,Word &Wrd){
   long j,k,n,lxn,ln=strlen(str);
   int *pint,gp,grmm2;
   char ch,srr[256],*pch,brd[256],frs[3];

   grmm2=gram_size-2;
   brd[0]='\0';
   brd[1]=' ';
   frs[1]='$';
   frs[2]='\0';
   n=0;
   j=0;
   Wrd.convert(str,ln);
   while(lxn=Wrd.wordf(j,ln)){
         k=0;
         gp=(lxn<grmm2)?lxn:grmm2;
         while(k<gp){brd[2+k]=Wrd.wrd[k];k++;}
         brd[k+2]='\0';
         if(brd[0]!='\0')this->add_count2(brd,1);

         frs[0]=brd[0]=Wrd.wrd[0];
         if(lxn>gram_size){
            //Handle first character
            this->add_count2(frs,1);
            //Define first ngram
            ch=Wrd.wrd[gram_size];
            Wrd.wrd[gram_size]='\0';
            //Enter the first ngram into the tree
            strcpy(srr,Wrd.wrd);
            this->add_count2(srr,1);
            //Produce the marked ngram and enter into tree
            strcat(srr,"!");
            this->add_count2(srr,aug);
            Wrd.wrd[gram_size]=ch;
            //Enter the remainder of the ngrams into tree
            for(k=1;k<lxn-gram_size+1;k++){
               ch=Wrd.wrd[gram_size+k];
               Wrd.wrd[gram_size+k]='\0';
               strcpy(srr,&Wrd.wrd[k]);
               this->add_count2(srr,1);
               Wrd.wrd[gram_size+k]=ch;
            }
         }
         else {
            //Handle first character
            this->add_count2(frs,1);
            //Enter the first ngram into tree
            strcpy(srr,Wrd.wrd);
            this->add_count2(srr,1);
            //Mark the ngram as first and enter into tree
            strcat(srr,"!");
            this->add_count2(srr,aug);
         }
   }
}

void Clip::fill_brm(const char *str,int lim_gram_size,Word &Wrd){
   long i,j,k,n,lxn,ln=strlen(str);
   int *pint,gp,grmm2;
   char *chd,*chz,*czz,*pch,cho;
   char *zlt;

   Wrd.convert(str,ln);
   zlt=Wrd.acc_conv();
   pch=new char[ln+1];
   j=0;
   for(i=0;i<ln;i++){
      if(gp=zlt[i])pch[j++]=gp;
   }
   chd=pch;
   if(j>1)chz=pch+2;
   for(i=1;i<j;i++){
      czz=chd;
      cho=*chz;
      *chz='\0';
      while((czz+1)!=chz){
         this->add_count2(czz,1);
         czz++;
      }
      *chz=cho;
      chz++;
      if(i+2>lim_gram_size)chd++;
   }
   delete [] pch;
}

void Clip::fill_scn(const char *str,int lim_gram_size,Word &Wrd){
   long i,j,lxn,ln=strlen(str);
   char *chd;

   j=0;
   Wrd.convert(str,ln);
   while(lxn=Wrd.wordf(j,ln)){
      if(lxn>=3){
         chd=new char[lxn+1];
         strcpy(chd,Wrd.wrd);
         i=3;
         while(i<=lxn){
            chd[i]='\0';
            add_count2(chd+i-3,1);
            chd[i]=Wrd.wrd[i];
            i++;
         }
      }
   }
}

}
