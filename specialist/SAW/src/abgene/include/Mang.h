#ifndef MANG_H
#define MANG_H

#include <iostream>
#include <fstream>
#include <Dbinbase.h>
#include <Postg.h>

using namespace std;
namespace iret {

class Mang {
public:
  Mang(void);
  Mang(Dbinbase *pDb,Postg<char> *pPg);
  Mang(const char *namdbn,const char *nampsg);
  Mang(Docum &Doc); //Creates the Dbinbase and Postg
     //objects
  Mang(const char *namdoc);
  ~Mang(void);
  void create_Postg(Docum &Doc);
  void create_Dbinbase(Docum &Doc); //Should only be
     //called after create_Postg is called.

  //Relate to the documents in Dbinbase
  void gopen_Dbinbase(void);
  long ndoc; //Number of documents in the set
     //Individual document
  void read(long n); //Sets the pointers for doc n
  long nw; //Number of terms in doc.
  long *nwd; //Array of term numbers in doc.
  void gclose_Dbinbase(void);

  //Relate to the terms in Postg<char>
  void gopen_Postg(void);
  long nwrd; //Number of terms in the set
  Index *Pst; //Pointer at postings Index objects.
  void gclose_Postg(void);

private:
  Dbinbase *pDnb;
  long *size;
  long *dad;
  long *don;
  Postg<char> *pPsg;
};

class Manf {
public:
  Manf(void);
  Manf(Dbinbase *pDb,Postg<float> *pPg);
  Manf(const char *namdbn,const char *nampsg);
  Manf(Docum &Doc,float (*d_local)(int,long)); 
     //Creates the dbinbase and postg objects
  Manf(const char *namdoc);
  ~Manf(void);
  void create_Postg(Docum &Doc,float (*d_local)(int,long));
  void create_Dbinbase(Docum &Doc,float (*d_local)(int,long)); 
     //Should only be called after create_Postg is called.

  //Relate to the documents in Dbinbase
  void gopen_Dbinbase(void);
  long ndoc; //Number of documents in the set
     //Individual document
  void read(long n); //Sets the pointers for doc n
  long nw; //Number of terms in doc.
  long *nwd; //Array of term numbers in doc.
  float *lwt; //Array of local weights for terms
  void gclose_Dbinbase(void);

  //Relate to the terms in Postg<float>
  void gopen_Postg(void);
  long nwrd; //Number of terms in the set
  Index *Pst; //Pointer at postings Index objects.
  float **flc; //Pointer at mapped memory for weights.
  void gclose_Postg(void);

  //Data
  Dbinbase *pDnb;
  long *size;
  long *dad;
  long *don;
  float *dow;
  Postg<float> *pPsg;
};

}
#endif
