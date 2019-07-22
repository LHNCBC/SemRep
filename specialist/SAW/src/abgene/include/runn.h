#ifndef RUNN_H
#define RUNN_H

#include <fstream>
#include <iostream>
#include <cctype>
#include <cstring>
#include <cstdlib>

using namespace std;
namespace iret {

const int word_cnt = 5000; //Maximum number of words in a document.
const int word_len = 1500; //Maximum word length.
const long max_str=1500;  //Maximum string length.

int get_pathw(char *cn,const char *dfl,const char *dex,const char *a);
   //Reads the path from a file "path_(*dfl)" and constructs the
   //file name from as "(*dfl)_(*dex).(*a)". Cats path and file
   //name and returns the full info in cn.
char *add_num(const char *ptr,long n,char *buf); //converts long to ascii 
   //and cats to end of string and returns pointer to new string 
   //that results. Does not change input string. The new string is
   //held in buffer space and this is overwritten at each call.

int get_qflag();
   //This function gets the value of the print flag pflag that is 
   //used to control output.
int mark(int,long,int,const char*);
   //This function is used to print out information that indicates 
   //how a function is progressing. It is dependent on the value of 
   //pflag.
long gseed(int,char**,const char*);
   //This function is called to allow the input of a seed value for 
   //the random number generator. It must be called in main or the 
   //arguments of main must be passed down to it if it is to allow 
   //command line entry. Otherwise the first argument may be set to 
   //zero and it may be used to enter the seed at run time from the 
   //console.
long clnga(int,char**,const char*,const char*);
   //Allows a long to be entered from the console at run time if the 
   //first argument is set to zero. If the first two arguments are 
   //the arguments of main, then it allows command line entry with
   //the flag that is the third argument and with a statement about 
   //the input that is the fourth argument.
double cdbla(int,char**,const char*,const char*);
char *cstra(int,char**,const char*,const char*);
long zrand(long);
   //Produces a random long integer that is in the range [0,argument).
   //Machinery of the random number generator.
void shuffle(long n,long *idx); //Randomly shuffles and array of longs.
long rnd(double);
   //Rounds off a double and returns the integer that results.

   //Reads in a string including white space and ends the string 
   //just before the character a.
inline int get_string(char *cnam,ifstream &ifile,char a){
   char *pch = cnam;
   long j=1;

   start:
   if((*(pch++)=ifile.get())!=EOF){
      if(*(pch-1)==a){pch--;goto start;}
      while(((*(pch++)=ifile.get())!=a)&&(j<max_str))j++;
      if(j<max_str){
         *(--pch)='\0';
         return(j);
      }
      else return(0);
   }
   return(0);
}

inline int get_strinf(char *cnam,fstream &ifile,char a){
   char *pch = cnam;
   long j=1;
   if((*(pch++)=ifile.get())!=EOF){
      while(((*(pch++)=ifile.get())!=a)&&(j<max_str))j++;
      if(j<max_str){
         *(--pch)='\0';
         return(j);
      }
      else return(0);
   }
   return(0);
}

//Function to lower case a string.
inline void lower_case(char *cnam){
   int i=0;
   char ch;

   while((ch=cnam[i])!='\0'){
       cnam[i++]=tolower(ch);
   }
}

template <class X>
void sSort(const long ix, X *idx){
  long k, j, ir, i;
  X rra;

  if(ix<=1)return;

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

template <class X, class Y>
void hSort(const long n, X *ra, Y *rb) {
  long k, j, ir, i;
  X rra;
  Y rrb;

  if(n<=1)return;

  k=(n>>1);
  ir=n-1;
  for(;;) {
    if(k>0) {
      rra=ra[--k];
      rrb=rb[k];
    }
    else {
      rra=ra[ir];
      rrb=rb[ir];
      ra[ir] = ra[0];
      rb[ir] = rb[0];
      if(--ir ==0) {
        ra[0]=rra;
        rb[0]=rrb;
        return;
      }
    }
    i=k;
    j=((k+1)<<1)-1;
    while(j<=ir) {
      if(j<ir && ra[j] < ra[j+1]) ++j;
      if(rra<ra[j]) {
        ra[i]=ra[j];
        rb[i]=rb[j];
        j +=(i=j)+1;
      }
      else j=ir+1;
    }
    ra[i]=rra;
    rb[i]=rrb;
  }
}

template <class X, class Y, class Z>
void hSort(const long n, X *ra, Y *rb, Z *rc) {
  long k, j, ir, i;
  X rra;
  Y rrb;
  Z rrc;

  if(n<=1)return;

  k=(n>>1);
  ir=n-1;
  for(;;) {
    if(k>0) {
      rra=ra[--k];
      rrb=rb[k];
      rrc=rc[k];
    }
    else {
      rra=ra[ir];
      rrb=rb[ir];
      rrc=rc[ir];
      ra[ir] = ra[0];
      rb[ir] = rb[0];
      rc[ir] = rc[0];
      if(--ir ==0) {
        ra[0]=rra;
        rb[0]=rrb;
        rc[0]=rrc;
        return;
      }
    }
    i=k;
    j=((k+1)<<1)-1;
    while(j<=ir) {
      if(j<ir && ra[j] < ra[j+1]) ++j;
      if(rra<ra[j]) {
        ra[i]=ra[j];
        rb[i]=rb[j];
        rc[i]=rc[j];
        j +=(i=j)+1;
      }
      else j=ir+1;
    }
    ra[i]=rra;
    rb[i]=rrb;
    rc[i]=rrc;
  }
}

template <class X, class Y>
void hRort(const long n, X *ra, Y *rb) {
  long k, j, ir, i;
  X rra;
  Y rrb;

  if(n<=1)return;

  k=(n>>1);
  ir=n-1;
  for(;;) {
    if(k>0) {
      rra=ra[--k];
      rrb=rb[k];
    }
    else {
      rra=ra[ir];
      rrb=rb[ir];
      ra[ir] = ra[0];
      rb[ir] = rb[0];
      if(--ir ==0) {
        ra[0]=rra;
        rb[0]=rrb;
        return;
      }
    }
    i=k;
    j=((k+1)<<1)-1;
    while(j<=ir) {
      if(j<ir && ra[j] > ra[j+1]) ++j;
      if(rra>ra[j]) {
        ra[i]=ra[j];
        rb[i]=rb[j];
        j +=(i=j)+1;
      }
      else j=ir+1;
    }
    ra[i]=rra;
    rb[i]=rrb;
  }
}

template <class X, class Y, class Z>
void hRort(const long n, X *ra, Y *rb, Z *rc) {
  long k, j, ir, i;
  X rra;
  Y rrb;
  Z rrc;

  if(n<=1)return;

  k=(n>>1);
  ir=n-1;
  for(;;) {
    if(k>0) {
      rra=ra[--k];
      rrb=rb[k];
      rrc=rc[k];
    }
    else {
      rra=ra[ir];
      rrb=rb[ir];
      rrc=rc[ir];
      ra[ir] = ra[0];
      rb[ir] = rb[0];
      rc[ir] = rc[0];
      if(--ir ==0) {
        ra[0]=rra;
        rb[0]=rrb;
        rc[0]=rrc;
        return;
      }
    }
    i=k;
    j=((k+1)<<1)-1;
    while(j<=ir) {
      if(j<ir && ra[j] > ra[j+1]) ++j;
      if(rra>ra[j]) {
        ra[i]=ra[j];
        rb[i]=rb[j];
        rc[i]=rc[j];
        j +=(i=j)+1;
      }
      else j=ir+1;
    }
    ra[i]=rra;
    rb[i]=rrb;
    rc[i]=rrc;
  }
}


//Function to convert a long to a null terminated string.
void long_str(char *cnam,long n);

//Function to convert a string with null termination
//to a long.
void str_long(char *cnam,long &n);
}
#endif
