#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <iomanip>
#include <cassert>
#include "runn.h"
#include "Btree.h"
#include "Word.h"
using namespace std;
namespace iret {

// Given character array punct[], setting punctuation marks 
//and kase = LOWERCASE for the lower kase, and kase = UPPERCASE 
//for the kase sensitive  
    
void Word::set_map(const char *punct,const char mrk,int kase){
 long i = 0;
 int j;
 while(j = punct[i]){
  zlt[j] = mrk;
  i++;
 
 }
 if(kase){
    for(j=65; j < 91; j++) zlt[j] = j;
 }
 else {
    for(i=65;i<91;i++)*(zlt+i)=i+32;
 }
 mrkk=mrk;
}

/* Restoring given characters array restore[] to the input text with their original character */
void Word::restore_map(const char *restore){
 long i = 0;
 int j;
 while(j = restore[i]){
  zlt[j] = j;
  i++;
 }
}


/* Erasing given characters array erase[] from input text */

void Word::erase_map(const char *erase){
 long i = 0;
 int j;
 while(j = erase[i]){
   for(int k = 0; k < 128; k++){
     if(zlt[k] == j) {
      zlt[k] = 0; 
     }
   } 
  i++;
 }
}

void Word::replace_map(char a,char b){
   zlt[(int)a]=b;
}

//Sets all the characters that are printable to be used.
void Word::all_print_map(void){
   for(int i=33;i<127;i++)zlt[i]=i;
}

void Word::pre_punct(void){
   zlt['#']=',';
   zlt['$']='\'';
   zlt['*']='.';
}

//Building up Btree for stop words and setting up 10000 spaces for the singlets,
//multiplets and phrases 
Word::Word(){
   word_space=10000; //Default value.
   list = (char **)new long[word_space];
   temp_list=(char **)new long[word_space];
   tmp_length=new long[word_space];
   zlt_convert = NULL;
   ifstream fin;
   char cnam1[100];
   btr = new Btree;
   Node *pnod;
   fin.open("/net/dorothy/cbb5/RETR/wonkim/PROCESS/DATA/wrd_stop",ios::in);
   //fin.open("/nfsvol/nls/specialist/abgene/FIXED_DATA/wrd_stop",ios::in);
   if ( ! fin ) {
     cerr << "Error in Word::Word:  cannot open wrd_stop\n";
     exit(1);
   }
   while(fin >> cnam1) {
     if(btr->search(cnam1)) {
        cout << "Error, list not unique!" << endl;
	exit(0);
     }
     else {
       pnod = new Node(cnam1);
       btr->insert(pnod);
      }
   }
   fin.close();
   byte_lim=65; 
   back_lim=0;
   stop=1;
   zlt=new char[128];
   int i;
   for(i=0;i<128;i++)*(zlt+i)=0;
   for(i=48;i<58;i++)*(zlt+i)=i;
   for(i=97;i<123;i++)*(zlt+i)=i;
   for(i=65;i<91;i++)*(zlt+i)=i+32;
}

Word::Word(long wrd_spc){
   word_space=wrd_spc; 
   list = (char **)new long[word_space];
   temp_list=(char **)new long[word_space];
   tmp_length=new long[word_space];
   zlt_convert = NULL;
   ifstream fin;
   char cnam1[100];
   btr = new Btree;
   Node *pnod;
   fin.open("/net/dorothy/cbb5/RETR/wonkim/PROCESS/DATA/wrd_stop",ios::in);
   //fin.open("/nfsvol/nls/specialist/module/abgene/FIXED_DATA/wrd_stop",ios::in);
   if ( ! fin ) {
     cerr << "Error in Word::Word:  cannot open wrd_stop\n";
     exit(1);
   }
   while(fin >> cnam1) {
     if(btr->search(cnam1)) {
        cout << "Error, list not unique!" << endl;
	exit(0);
     }
     else {
       pnod = new Node(cnam1);
       btr->insert(pnod);
      }
   }
   fin.close();
   byte_lim=65; 
   back_lim=0;
   stop=1;
   zlt=new char[128];
   int i;
   for(i=0;i<128;i++)*(zlt+i)=0;
   for(i=48;i<58;i++)*(zlt+i)=i;
   for(i=97;i<123;i++)*(zlt+i)=i;
   for(i=65;i<91;i++)*(zlt+i)=i+32;
}

Word::Word(long wrd_spc,const char *list_name){
   char cnam[256];
   word_space=wrd_spc; 
   list = (char **)new long[word_space];
   temp_list=(char **)new long[word_space];
   tmp_length=new long[word_space];
   zlt_convert = NULL;
   ifstream fin;
   char cnam1[100];
   btr = new Btree;
   Node *pnod;
   get_pathw(cnam,"wordset",list_name,"stop");
   fin.open(cnam,ios::in);
   if ( ! fin ) {
     cerr << "Error in Word::Word:  cannot open " << cnam << "\n";
     exit(1);
   }
   while(fin >> cnam1) {
     if(btr->search(cnam1)) {
	cout << "Error, list not unique!" << endl;
	exit(0);
     }
     else {
       pnod = new Node(cnam1);
       btr->insert(pnod);
      }
   }
   fin.close();
   byte_lim=65; 
   back_lim=0;
   stop=1;
   zlt=new char[128];
   int i;
   for(i=0;i<128;i++)*(zlt+i)=0;
   for(i=48;i<58;i++)*(zlt+i)=i;
   for(i=97;i<123;i++)*(zlt+i)=i;
   for(i=65;i<91;i++)*(zlt+i)=i+32;
}

/* Mapping input text with zlt[] */
void Word:: convert(const char *text,long len){
   txt=text;
   length=len;
   if(zlt_convert) delete [] zlt_convert; 
   zlt_convert = new char[length+1];
   zlt_convert[length]=1;
   long i =0;
   while(i < length){
      zlt_convert[i] = zlt[txt[i]]; 
      i++;
   }
}

//Remove the character c from start of any word.
//Assumes that conversion has already occurred.
void Word::modify(const char c){
   long i=1;

   if(*zlt_convert==c)*zlt_convert=0;
   while(i<length){
      if(!(*(zlt_convert+i-1))&&(*(zlt_convert+i)==c))*(zlt_convert+i)=0;
      i++;
   }
}

//Save periods that are part of abreviations or numbers etc.
void Word::punct(long n,char *text){
   int a,b,c;
   long i=0;
   char *pch=text;
   if(n<1)return;

   switch(*pch){
      case '*': *pch=0;
                break;
      case '#': *pch=0;
                break;
      case '$': *pch=0;
                break;
      case '.': *pch=0;
                break;
      case ',': *pch=0;
                break;
      case '\'': *pch=0;
                break;
   }
   if(n<2)return;
   c=zlt[*(pch++)];
   switch(*pch){
      case '*': *pch=0;
                break;
      case '#': *pch=0;
                break;
      case '$': *pch=0;
                break;
      case '.': if(c)*pch='*';
                break;
      case ',': if(c && n>2 && zlt[*(pch+1)])*pch='#';
                break;
      case '\'': if(c)*pch='$';
                break;
   }
   if(n<3)return;
   a=0;
   b=c;
   c=zlt[*(pch++)];
   i=2;
   while(i<n-1){
      switch(*pch){
         case '*': *pch=0;
                   break;
         case '#': *pch=0;
                   break;
         case '$': *pch=0;
                   break;
         case '.': if((((!a)||(!b))&&c)||zlt[*(pch+1)])*pch='*';
                   break;
         case ',': if(c&&zlt[*(pch+1)])*pch='#';
                   break;
         case '\'': if( (c&&zlt[*(pch+1)]) ||
			(c=='s') ||
			(((!a)||(!b))&&c) )*pch='$';
                   break;
      }
      a=b;
      b=c;
      c=zlt[*(pch++)];
      i++;
   }
   switch(*pch){
      case '*': *pch=0;
                break;
      case '#': *pch=0;
                break;
      case '$': *pch=0;
                break;
      case '.': if(((!a)||(!b))&&c)*pch='*';
                break;
      case ',': break;
      case '\'': if( (c=='s') ||
		     (((!a)||(!b))&&c) )*pch='$';
                break;
   }
}

/*Given beginning and ending positions of input text,
 this will produce words */
   
long Word::wordf(long &i, long len)
{
        int j, alp[max_str],flag;
        char a;
restart:
        j=0;
        while(!zlt_convert[i]) i++; 
        if(i>=len) return(0);
        al=1;
        flag=0;
        while((i<len)&&(a=wrd[j]=zlt_convert[i])){
                if(a< byte_lim)al=0;
                else flag=1;
                alp[j]=al;
                j++;
                i++;
        }
       
        j--;
        if(flag==0) goto restart;
        while((j>=0)&&(wrd[j]<back_lim))j--;
        if((j<0)&&(i<len)) goto restart;
        else if(j<0) return(0);
        else wrd[j+1]='\0';
        al=alp[j];
        return(j+1);
}

/* Initializing character positions */
void Word::step_zero(){
   beg=0;
   end = 0;
}

/* Finding character positions of the beginning and ending punctuation marks
   and returning long interger which is the difference between the total
   length of input text and the current position of ending punctuation mark*/
 long Word::step_next(){
    beg= end;
    while((beg<length)&&(zlt_convert[beg] ==mrkk)) beg++;
    end = beg;
    while((end<length)&&(zlt_convert[end] !=mrkk)) end++;
    return (length - beg );      
 }

/* Producing singlets between the punctuation marks */
void Word::single(){
cnt =0;
long ln,j, len;
char *wwrd;
step_zero();
if(stop){
while(ln = step_next()){
  j= beg; 
  while(len=wordf(j, end)){
      if( btr->search(wrd) ==0 ) {
      wwrd = new char[len+1];
      strcpy(wwrd,wrd);
      list[cnt] = wwrd; 
      cnt++;
     }
  }
 }   
}
else {
  while(ln = step_next()){
    j= beg; 
    while(len=wordf(j, end)){
      wwrd = new char[len+1];
      strcpy(wwrd,wrd);
      list[cnt] = wwrd; 
      cnt++;  

    }
  }   
}
}
 

/* Producing multiplets between the punctuation marks */
void Word::multiple(int nplet){
cnt =0;
char *wwrd;
long ln,sum;
char *temp_cnam;
int a;
long i,j,len;
long temp_cnt;
step_zero();
while(ln=step_next()){
  temp_cnt = 0;
  j = beg;
    while(len=wordf(j,end)){
      wwrd = new char[len+1];
      strcpy(wwrd,wrd);
      temp_list[temp_cnt] = wwrd; 
      tmp_length[temp_cnt] = len+1;
      temp_cnt++;
       
  }
  
    // Added to prevent memory leak -- Halil
  if(temp_cnt<nplet)
  {
      if (temp_cnt > 0)
      {
	  for (i=0; i < temp_cnt; i++)	delete [] temp_list[i];
      }
      continue;
  }

 if(stop){
    for(i=0;i<temp_cnt;i++){
      if(btr->search(temp_list[i]))tmp_length[i]=0;
    }
    i =0;
    while( i < temp_cnt - nplet +1 ){
       a = 1;
       j = 0;
       sum=0;
       while(j < nplet) { 
          if( !tmp_length[i+j] )  {
             a = 0;
             i +=j+1;
             break;
          }   
          else sum+=tmp_length[i+j];
          j++; 
       }
     
       if(a ==1){
          temp_cnam = new char[sum];
          strcpy(temp_cnam, temp_list[i]);
          j=1;
          while(j < nplet){
             strcat(temp_cnam," ");
             strcat(temp_cnam, temp_list[i+j]);
             j++; 
          }
          list[cnt] = temp_cnam; 
          cnt++;
          i =i + 1;
       }
    } 
 }//stop
 else{
    i =0;
    while( i < temp_cnt - nplet +1 ){
       sum=0;
       for(j=0;j<nplet;j++)sum+=tmp_length[i+j];
       temp_cnam = new char[sum];
       strcpy(temp_cnam, temp_list[i]);
       j=1;
       while(j < nplet){
          strcat(temp_cnam," ");
          strcat(temp_cnam, temp_list[i+j]);
          j++; 
       }
       list[cnt] = temp_cnam; 
       cnt++;
       i =i + 1;
    } 
 }

 for(i =0; i < temp_cnt; i++)  delete [] temp_list[i];
 } 
}

/* Between the punctuation marks, producing phrases which is not allowed stop words within them  */
void Word::phrase(){
cnt =0;
char *wwrd;
long ln,sum;
char *temp_cnam;
int a;
long i,j,k;
long temp_cnt;
long length_cnt;
step_zero();
while(step_next()){
  length_cnt=0;
  temp_cnt = 0;
  j = beg;
  while(ln=wordf(j,end)){
      wwrd = new char[ln+1];
      strcpy(wwrd,wrd);
      temp_list[temp_cnt] = wwrd; 
      tmp_length[temp_cnt]= ln+1;
      temp_cnt++;
      length_cnt +=ln;  
  }
  if(stop){
     for(i=0;i<temp_cnt;i++){
       if(btr->search(temp_list[i]))tmp_length[i]=0;
     }
     i=0;
     while(i < temp_cnt){ 
        while( i < temp_cnt && (!tmp_length[i]) ) i++;
        if(i < temp_cnt){
           sum=tmp_length[i];
           j=1;
           while( i+j < temp_cnt && tmp_length[i+j] ){
              sum+=tmp_length[i+j];
              j++;
           }
           temp_cnam = new char[sum];
           strcpy(temp_cnam, temp_list[i]);
           k=1;
           while(k < j){
              strcat(temp_cnam, " ");
              strcat(temp_cnam, temp_list[i+k]);
              k++;
           }
           list[cnt++] = temp_cnam; 
        }
        i+=j+1;
     }
  }
  else {
     i=0;
     while(i < temp_cnt){ 
        temp_cnam = new char[length_cnt + temp_cnt];
        strcpy(temp_cnam, temp_list[i]); 
        i++; 
        while(i < temp_cnt){
           strcat(temp_cnam, " ");
           strcat(temp_cnam, temp_list[i]);         
           i++;
        }
        list[cnt++] = temp_cnam; 
     }
  }
  for(i =0; i < temp_cnt; i++) delete [] temp_list[i]; 
}  

}


/* Producing singlets between the punctuation marks */
//Stemmed version
void Word::single_stem(){
cnt =0;
long ln,j, len;
char *wwrd;
step_zero();
if(stop){
while(ln = step_next()){
  j= beg; 
  while(len=wordf(j, end)){
   if( btr->search(wrd) ==0 ) {
      len=stmprt(wrd,len);
      wwrd = new char[len+1];
      strcpy(wwrd,wrd);
      list[cnt] = wwrd; 
      cnt++;
   }
  }
 }   
}
else {
  while(ln = step_next()){
    j= beg; 
    while(len=wordf(j, end)){
      len=stmprt(wrd,len);
      wwrd = new char[len+1];
      strcpy(wwrd,wrd);
      list[cnt] = wwrd; 
      cnt++;    
    }
  }   
}
}
 

/* Producing multiplets between the punctuation marks */
//Stemmed version.
void Word::multiple_stem(int nplet){
cnt =0;
char *wwrd;
long ln,sum;
char *temp_cnam;
int a;
long i,j,len;
long temp_cnt;
step_zero();
while(ln=step_next()){
  temp_cnt = 0;
  j = beg;
    while(len=wordf(j,end)){
      wwrd = new char[len+1];
      strcpy(wwrd,wrd);
      temp_list[temp_cnt] = wwrd; 
      tmp_length[temp_cnt] = len+1;
      temp_cnt++;
       
  }
  
    // Added to prevent memory leak -- Halil
  if(temp_cnt<nplet)
  {
      if (temp_cnt > 0)
      {
	  for (i=0; i < temp_cnt; i++)	delete [] temp_list[i];
      }
      continue;
  }

 if(stop){
    for(i=0;i<temp_cnt;i++){
       if(btr->search(temp_list[i]))tmp_length[i]=0;
       else {
          tmp_length[i]=stmprt(temp_list[i],tmp_length[i]-1)+1;
       }
    }
    i =0;
    while( i < temp_cnt - nplet +1 ){
       a = 1;
       j = 0;
       sum=0;
       while(j < nplet) { 
          if(!tmp_length[i+j]){
             a = 0;
             i +=j+1;
             break;
          }   
          else sum+=tmp_length[i+j];
          j++; 
       }
     
       if(a ==1){
          temp_cnam = new char[sum];
          strcpy(temp_cnam, temp_list[i]);
          j=1;
          while(j < nplet){
             strcat(temp_cnam," ");
             strcat(temp_cnam, temp_list[i+j]);
             j++; 
          }
          list[cnt] = temp_cnam; 
          cnt++;
          i =i + 1;
       }
    } 
 }//stop
 else{
    for(i=0;i<temp_cnt;i++){
       tmp_length[i]=stmprt(temp_list[i],tmp_length[i]-1)+1;
    }
    i =0;
    while( i < temp_cnt - nplet +1 ){
       sum=0;
       for(j=0;j<nplet;j++)sum+=tmp_length[i+j];
       temp_cnam = new char[sum];
       strcpy(temp_cnam, temp_list[i]);
       j=1;
       while(j < nplet){
          strcat(temp_cnam," ");
          strcat(temp_cnam, temp_list[i+j]);
          j++; 
       }
       list[cnt] = temp_cnam; 
       cnt++;
       i =i + 1;
    } 
 }

 for(i =0; i < temp_cnt; i++) delete [] temp_list[i];
 } 
}

/* Between the punctuation marks, producing phrases which is not allowed stop words within them  */
//Stemmed version.
void Word::phrase_stem(){
cnt =0;
char *wwrd;
long ln,sum;
char *temp_cnam;
int a;
long i,j,k;
long temp_cnt;
long length_cnt;
step_zero();
while(step_next()){
  length_cnt=0;
  temp_cnt = 0;
  j = beg;
  while(ln=wordf(j,end)){
      wwrd = new char[ln+1];
      strcpy(wwrd,wrd);
      temp_list[temp_cnt] = wwrd; 
      tmp_length[temp_cnt]= ln+1;
      temp_cnt++;
      length_cnt +=ln;  
  }
  if(stop){
     for(i=0;i<temp_cnt;i++){
       if(btr->search(temp_list[i]))tmp_length[i]=0;
       else {
          tmp_length[i]=stmprt(temp_list[i],tmp_length[i]-1)+1;
       }
     }
     i=0;
     while(i < temp_cnt){ 
        while( i < temp_cnt && (!tmp_length[i]) ) i++;
        if(i < temp_cnt){
           sum=tmp_length[i];
           j=1;
           while( i+j < temp_cnt && tmp_length[i+j] ){
              sum+=tmp_length[i+j];
              j++;
           }
           temp_cnam = new char[sum];
           strcpy(temp_cnam, temp_list[i]);
           k=1;
           while(k < j){
              strcat(temp_cnam, " ");
              strcat(temp_cnam, temp_list[i+k]);
              k++;
           }
           list[cnt++] = temp_cnam; 
        }
        i+=j+1;
     }
  }
  else {
     for(i=0;i<temp_cnt;i++){
        tmp_length[i]=stmprt(temp_list[i],tmp_length[i]-1)+1;
     }
     i=0;
     temp_cnam = new char[length_cnt + temp_cnt];
     strcpy(temp_cnam, temp_list[i]); 
     i++; 
     while(i < temp_cnt){
        strcat(temp_cnam, " ");
        strcat(temp_cnam, temp_list[i]);         
        i++;
     }
     list[cnt++] = temp_cnam; 
  }
  for(i =0; i < temp_cnt; i++) delete [] temp_list[i];
}   
}

Word::~Word(){
  if(zlt_convert) delete [] zlt_convert;
  delete [] list;   
  delete btr;
  delete [] zlt;
  delete [] temp_list;
  delete [] tmp_length;
}

void Word::clear_list(){
for(long i = 0; i < cnt; i++)delete  [] list[i];
}

void Word::remove_sgml_tags(long n, char *s){
   long i=0;
   int flag = 0;
   char c;
   while(i < n){
      c=s[i];
      if(!flag){
         if(c == '<') {
            s[i] = '.';
            flag =1;
         }
      }
      else {
        s[i] = ' ';
        if(c == '>')  flag =0;
      }
      i++; 
   }
}

void Word::remove_html_list_commas(long n, char *s){
   long i=0;
   int flag = 0, ul = 0, eul =0;
   char c;
   while(i < n){
      c=s[i];
      switch (c){
        case '<' :  ul =1; break;
        case '\\' : if(ul ==1) eul=1;
                    else ul = 0;
                    break;
        case 'U' : if(ul ==1) ul++;
                   else ul = eul = 0;
                   break;
        case 'L' : if(ul ==2) ul++;
                   else ul = eul =0;
                   break;
        case '>' : if(ul ==3) ul++;
                   else ul = eul =0;
                   if(ul == 4){
                     if(eul ==0) flag = 1;
                     else flag =0;
                   }
                   break;
        case ',' : if(flag) s[i] = ' ';
        default : ul = eul =0;
      }
      i++; 
   }
} 

void form(int, int, char*, int*);

static int vl[128]={
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
        0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
        0,  1,  0,  0,  0,  1,  0,  0,  0,  1,  0,  0,  0,  0,  0,  1,
        0,  0,  0,  0,  0,  1,  0,  0,  0,  2,  0,  0,  0,  0,  0,  0};

int Word::stmprt(char *wrdl, int len)
{int i,j,k,flag,frm[100];
char wrc[100];

if(len>100)return(len);
if((len>1) && (wrdl[len-1]=='\'')){wrdl[len-1]='\0';len--;}
else if((len>1) && (wrdl[len-1]=='.')){wrdl[len-1]='\0';len--;}
if((len>2) && ((wrdl[len-2]=='\'')&&(wrdl[len-1]=='s'))){
  wrdl[len-2]='\0';len-=2;
}
if(len<4)return(len);

frm[0]=0;
wrc[0]='*';

for(i=0;i<len;i++)wrc[i+1]=wrdl[i];
len++;
form(len,1,wrc,frm);

/*1a*/
if((wrc[len-1]=='s')&&(len>2))
   switch(wrc[len-2]){
     case 'e':
       if((wrc[len-3]=='i')||((wrc[len-3]=='s')&&(wrc[len-4]=='s')))len-=2;
       else len--;
       break;
     case 's':
       break;
     default: len--;
       break;
                     }
/*1b*/
flag=0;
if(((wrc[len-1]=='d')&&(wrc[len-2]=='e'))&&((wrc[len-3]=='e')&&(frm[len-4]>1)))
  len--;
else {
  if(((wrc[len-1]=='d')&&(wrc[len-2]=='e'))&&(frm[len-3]>1)){len-=2;flag++;}
  else if(((wrc[len-1]=='g')&&(wrc[len-2]=='n'))&&((wrc[len-3]=='i')&&\
         (frm[len-4]>0))){len-=3;flag++;}
  if(flag){
     if((wrc[len-1]=='t')&&(wrc[len-2]=='a')){
                  wrc[len]='e';
                  frm[len]=frm[len-1]+1;
                  len++;}
     else if((wrc[len-1]=='l')&&(wrc[len-2]=='b')){
                  wrc[len]='e';
                  frm[len]=frm[len-1]+1;
                  len++;}
     else if((wrc[len-1]=='z')&&(wrc[len-2]=='i')){
                  wrc[len]='e';
                  frm[len]=frm[len-1]+1;
                  len++;}
     else if(((wrc[len-1]==wrc[len-2])&&(wrc[len-1]!='l'))&&((wrc[len-1]!='s')&&\
            (wrc[len-1]!='z')))len--;
     else if((((frm[len-1]==2)&&(len>3))&&((frm[len-2]==1)&&(frm[len-3]==0)))&&\
            ((wrc[len-1]<'w')||(wrc[len-1]>'y'))){
                  wrc[len]='e';
                  frm[len]=frm[len-1]+1;
                  len++;}
           }
      }
/*1c*/
if((wrc[len-1]=='y')&&(frm[len-2]>0)){wrc[len-1]='i';form(len,len-1,wrc,frm);}

/*2*/
switch(wrc[len-2]){
  case 'a':
    if((wrc[len-1]=='l')&&(wrc[len-3]=='n')){
       if((wrc[len-4]=='o')&&((wrc[len-5]=='i')&&(wrc[len-6]=='t'))){
         if((wrc[len-7]=='a')&&(frm[len-8]>1)){wrc[len-5]='e';len-=4;}
         else if(frm[len-7]>1)len-=2;
                                                                     }
                                               }
    break;
  case 'c':
    if((wrc[len-1]=='i')&&(wrc[len-3]=='n')){
      if(((wrc[len-4]=='e')||(wrc[len-4]=='a'))&&(frm[len-5]>1))wrc[len-1]='e';
                                             }
    break;
  case 'e':
    if(((wrc[len-1]=='r')&&(wrc[len-3]=='z'))&&((wrc[len-4]=='i')&&\
      (frm[len-5]>1)))len--;
    break;
  case 'l':
    if(wrc[len-1]=='i')
      switch(wrc[len-3]){
        case 'b':
          if((wrc[len-4]=='a')&&(frm[len-5]>1))wrc[len-1]='e';
          break;
        case 'l':
          if((wrc[len-4]=='a')&&(frm[len-5]>1))len-=2;
          break;
        case 't':
          if(((wrc[len-4]=='n')&&(wrc[len-5]=='e'))&&(frm[len-6]>1))len-=2;
          break;
        case 'e':
          if(frm[len-4]>1)len-=2;
          break;
        case 's':
          if(((wrc[len-4]=='u')&&(wrc[len-5]=='o'))&&(frm[len-6]>1))len-=2;
          break;
                          }
     break;
   case 'o':
     if(((wrc[len-1]=='n')&&(wrc[len-3]=='i'))&&((wrc[len-4]=='t')&&\
       (wrc[len-5]=='a'))){
        if(((wrc[len-6]=='z')&&(wrc[len-7]=='i'))&&(frm[len-8]>1)){
                                             wrc[len-5]='e';len-=4;}
        else if(frm[len-6]>1){wrc[len-3]='e';len-=2;}
        }
     else if(((wrc[len-1]=='r')&&(wrc[len-3]=='t'))&&((wrc[len-4]=='a')&&\
            (frm[len-5]>1))){wrc[len-2]='e';len--;}
     break;
   case 's':
     if(wrc[len-1]=='m'){
       if(((wrc[len-3]=='i')&&(wrc[len-4]=='l'))&&((wrc[len-5]=='a')&&\
         (frm[len-6]>1)))len-=3;
                         }
     else if((wrc[len-1]=='s')&&((wrc[len-3]=='e')&&(wrc[len-4]=='n'))){
       if(((wrc[len-5]=='e')&&(wrc[len-6]=='v'))&&((wrc[len-7]=='i')&&\
         (frm[len-8]>1)))len-=4;
       else if(((wrc[len-5]=='l')&&(wrc[len-6]=='u'))&&((wrc[len-7]=='f')&&\
              (frm[len-8]>1)))len-=4;
       else if(((wrc[len-5]=='s')&&(wrc[len-6]=='u'))&&((wrc[len-7]=='o')&&\
              (frm[len-8]>1)))len-=4;
                                                                        }
     break;
   case 't':
     if((wrc[len-1]=='i')&&(wrc[len-3]=='i')){
       if((wrc[len-4]=='l')&&((wrc[len-5]=='a')&&(frm[len-6]>1)))len-=3;
       else if((wrc[len-4]=='v')&&((wrc[len-5]=='i')&&(frm[len-6]>1))){
         wrc[len-3]='e';
         len-=2;}
       else if(((wrc[len-4]=='l')&&(wrc[len-5]=='i'))&&((wrc[len-6]=='b')&&\
              (frm[len-7]>1))){
         wrc[len-5]='l';
         wrc[len-4]='e';
         frm[len-4]=frm[len-5];
         frm[len-5]=frm[len-6];
         len-=3;}
                                               }
     break;
          }

/*3*/
switch(wrc[len-1]){
 case 'e':
  if(((wrc[len-2]=='t')&&(wrc[len-3]=='a'))&&((wrc[len-4]=='c')&&\
    (wrc[len-5]=='i'))){
    if(frm[len-6]>1)len-=3;
     }
  else if(((wrc[len-2]=='v')&&(wrc[len-3]=='i'))&&((wrc[len-4]=='t')&&\
         (wrc[len-5]=='a'))){
    if(frm[len-6]>1)len-=5;
          }
  else if(((wrc[len-2]=='z')&&(wrc[len-3]=='i'))&&((wrc[len-4]=='l')&&\
         (wrc[len-5]=='a'))){
    if(frm[len-6]>1)len-=3;
          }
  break;
 case 'i':
  if(((wrc[len-2]=='t')&&(wrc[len-3]=='i'))&&((wrc[len-4]=='c')&&\
    (wrc[len-5]=='i'))){
    if(frm[len-6]>1)len-=3;
     }
  break;
 case 'l':
  if(((wrc[len-2]=='a')&&(wrc[len-3]=='c'))&&((wrc[len-4]=='i')&&\
    (frm[len-5]>1)))len-=2;
  else if(((wrc[len-2]=='u')&&(wrc[len-3]=='f'))&&(frm[len-4]>1))len-=3;
  break;
 case 's':
  if(((wrc[len-2]=='s')&&(wrc[len-3]=='e'))&&((wrc[len-4]=='n')&&\
    (frm[len-5]>1)))len-=4;
  break;
                     }
/*4*/
switch(wrc[len-2]){
 case 'a':
  if((wrc[len-1]=='l')&&(frm[len-3]>3))len-=2;
  break;
 case 'c':
  if((wrc[len-1]=='e')&&(wrc[len-3]=='n')){
   if((wrc[len-4]=='a')&&(frm[len-5]>3))len-=4;
   else if((wrc[len-4]=='e')&&(frm[len-5]>3))len-=4;
                                           }
  break;
 case 'e':
  if((wrc[len-1]=='r')&&(frm[len-3]>3))len-=2;
  break;
 case 'i':
  if((wrc[len-1]=='c')&&(frm[len-3]>3))len-=2;
  break;
 case 'l':
  if((wrc[len-1]=='e')&&(wrc[len-3]=='b')){
   if(((wrc[len-4]=='a')||(wrc[len-4]=='i'))&&(frm[len-5]>3))len-=4;
                                           }
  break;
 case 'n':
  if(wrc[len-1]=='t'){
   if((wrc[len-3]=='a')&&(frm[len-4]>3))len-=3;
   else if(((wrc[len-3]=='e')&&(wrc[len-4]=='m'))&&((wrc[len-5]=='e')&&\
          (frm[len-6]>3)))len-=5;
   else if(((wrc[len-3]=='e')&&(wrc[len-4]=='m'))&&(frm[len-5]>3))len-=4;
   else if((wrc[len-3]=='e')&&(frm[len-4]>3))len-=3;
                      }
  break;
 case 'o':
  if((wrc[len-1]=='n')&&(wrc[len-3]=='i')){
   if(((wrc[len-4]=='s')||(wrc[len-4]=='t'))&&(frm[len-4]>3))len-=3;
                                           }
  else if((wrc[len-1]=='u')&&(frm[len-3]>3))len-=2;
  break;
 case 's':
  if(((wrc[len-1]=='m')&&(wrc[len-3]=='i'))&&(frm[len-4]>3))len-=3;
  break;
 case 't':
  if(((wrc[len-1]=='e')&&(wrc[len-3]=='a'))&&(frm[len-4]>3))len-=3;
  else if(((wrc[len-1]=='i')&&(wrc[len-3]=='i'))&&(frm[len-4]>3))len-=3;
  break;
 case 'u':
  if(((wrc[len-1]=='s')&&(wrc[len-3]=='o'))&&(frm[len-4]>3))len-=3;
  break;
 case 'v':
  if(((wrc[len-1]=='e')&&(wrc[len-3]=='i'))&&(frm[len-4]>3))len-=3;
  break;
 case 'z':
  if(((wrc[len-1]=='e')&&(wrc[len-3]=='i'))&&(frm[len-4]>3))len-=3;
  break;
               }
/*5a*/
if(wrc[len-1]=='e'){
 if(frm[len-2]>3)len--;
 else if((((frm[len-2]==2)&&(len>4))&&((frm[len-3]==1)&&(frm[len-4]==0)))&&\
        ((wrc[len-2]<'w')||(wrc[len-2]>'y')));
 else if(frm[len-2]>1)len--;
                    }
/*5b*/
if(((wrc[len-1]==wrc[len-2])&&(wrc[len-1]=='l'))&&(frm[len-1]>3))len--;

for(i=1;i<len;i++)wrdl[i-1]=wrc[i];
wrdl[len-1]='\0';
return(len-1);
}

void form(int len, int p, char *wrc, int *frm)
{int i,k;

for(i=p;i<len;i++){
   switch(vl[wrc[i]]){
     case 0:
        if(frm[i-1]%2)frm[i]=frm[i-1]+1;
        else frm[i]=frm[i-1];
        break;
     case 1:
        if(frm[i-1]%2)frm[i]=frm[i-1];
        else frm[i]=frm[i-1]+1;
        break;
     case 2:
        frm[i]=frm[i-1]+1;
        break;
                      }
                    }
}

Mark::Mark(long max_len, long wrd_spc, char *infile) : Word(wrd_spc){
   int pflag=get_qflag();
   long i;
   st = new long[wrd_spc];
   nd = new long[wrd_spc];
   ph_cnt = new long[wrd_spc];
   str = new char[max_len];
   pos = new long[max_len];
   pPart = new Partial_match;
 
   ifstream fin(infile,ios::in);
   if(!fin.is_open()) { cout <<"check" <<endl; exit(0);}
   long ccnt = 1;
   char cnam[1500];
   while(fin.getline(cnam,1500,'\n')) {
      pPart->add_count(cnam, ccnt); ccnt++;
      mark(pflag, ccnt, 100, "phrases");
   }
   fin.close();
}
   
Mark::Mark(long max_len, long wrd_spc, char *infile,const char *list_name) : Word(wrd_spc,list_name){
   int pflag=get_qflag();
   long i;
   st = new long[wrd_spc];
   nd = new long[wrd_spc];
   ph_cnt = new long[wrd_spc];
   str = new char[max_len];
   pos = new long[max_len];
   pPart = new Partial_match;
 
   ifstream fin(infile,ios::in);
   if(!fin.is_open()) { cout <<"check" <<endl; exit(0);}
   long ccnt = 1;
   char cnam[1500];
   while(fin.getline(cnam,1500,'\n')) {
      pPart->add_count(cnam, ccnt); ccnt++;
      mark(pflag, ccnt, 100, "phrases");
   }
   fin.close();
}

Mark::~Mark(){
   delete [] st;
   delete [] nd;
   delete [] ph_cnt;
   delete [] str;
   delete [] pos;
   
   delete pPart;   
}

void Mark::mark_long(void){
   char *Str;
   long i,j,k,m,p1,p2,ln1,ln2;
   char *pch, *temp;
   long local_p_punc = 0;

   cnt = 0;
   for(j=0;j<length;j++){
      if(!zlt_convert[j])zlt_convert[j]=32;
      else if(zlt_convert[j]==mrkk)zlt_convert[j]=0;
   }
   zlt_convert[j]=0;
   Str = zlt_convert;
   j = 0;
   while(j<length){
      m=strlen(Str);
      j = j + m;
      p1 = 0;
      p2 = 0;
      while(Str[p1] == 32)p1++; //find non-space for a starting string
      while(p1<=m){             //Set string and pos arrays
         str[p2] = Str[p1];
         pos[p2] = p1;
         p1++;
         p2++;
         if(Str[p1-1] == 32){
            while(Str[p1] == 32)p1++;
         }
      }
      ln1 = strlen(str);
      temp = str;
      if(ln1 ==0){  //If nothing found move on
         local_p_punc = local_p_punc + strlen(Str) +1;
         j++;
         Str = &zlt_convert[j];
         continue;
      }

      while(*temp!='\0'){
         if((k=pPart->search_long(temp))){
            pch=pPart->show_str();
            ln2 = ln1 -strlen(temp);
            list[cnt] = pch;
            st[cnt] = local_p_punc + pos[ln2];
            nd[cnt] = local_p_punc + pos[ln2 + strlen(pch)];
            ph_cnt[cnt] = pPart->count();
            cnt++;
         }
         if(k){
            temp=&temp[strlen(pch)];
            if(*temp==' ')temp++;
         }
         else if((pch=strchr(temp,' '))!=NULL)temp=pch+1;
         else temp=temp+strlen(temp);
      }

      local_p_punc = local_p_punc + strlen(Str) +1;
      j++;
      Str = &zlt_convert[j];
   }
}

void Mark::mark_all(void){
   char *Str;
   long i,j,k,m,p1,p2,ln1,ln2;
   char *pch, *temp;
   long local_p_punc = 0;

   cnt = 0;
   for(j=0;j<length;j++){
      if(!zlt_convert[j])zlt_convert[j]=32;
      else if(zlt_convert[j]==mrkk)zlt_convert[j]=0;
   }
   zlt_convert[j]=0;
   Str = zlt_convert;
   j = 0;
   while(j<length){
      m=strlen(Str);
      j = j + m;
      p1 = 0;
      p2 = 0;
      while(Str[p1] == 32)p1++; //find non-space for a starting string
      while(p1<=m){             //Set string and pos arrays
         str[p2] = Str[p1];
         pos[p2] = p1;
         p1++;
         p2++;
         if(Str[p1-1] == 32){
            while(Str[p1] == 32)p1++;
         }
      }
      ln1 = strlen(str);
      temp = str;
      if(ln1 ==0){  //If nothing found move on
         local_p_punc = local_p_punc + strlen(Str) +1;
         j++;
         Str = &zlt_convert[j];
         continue;
      }

      while(*temp!='\0'){
         if(pPart->search_long(temp)){
            pch=pPart->show_str();
            ln2 = ln1 -strlen(temp);
            list[cnt] = pch;
            st[cnt] = local_p_punc + pos[ln2];
            nd[cnt] = local_p_punc + pos[ln2 + strlen(pch)];
            ph_cnt[cnt] = pPart->count();
            cnt++;
            i=strlen(pch)-1;
            while(0<i){
               while((0<i)&&(*(temp+i)!=' '))i--;
               if(0<i){
                  *(temp+i)='\0';
                  k=pPart->search(temp);
                  *(temp+i)=' ';
                  if(k){
                     pch=pPart->show_str();
                     list[cnt] = pch;
                     st[cnt] = local_p_punc + pos[ln2];
                     nd[cnt] = local_p_punc + pos[ln2 + strlen(pch)];
                     ph_cnt[cnt] = pPart->count();
                     cnt++;
                  }
                  i--;
               }
            }
         }
         if((pch=strchr(temp,' '))!=NULL)temp=pch+1;
         else temp=temp+strlen(temp);
      }

      local_p_punc = local_p_punc + strlen(Str) +1;
      j++;
      Str = &zlt_convert[j];
   }
}

}
