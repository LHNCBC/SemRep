#ifndef CMLS_H
#define CMLS_H

#include <iostream>
#include <fstream>
#include <Dbinbase.h>
#include <Postg.h>
#include <Mang.h>

using namespace std;
namespace iret {

class Cmls : public Mang {
public:
   Cmls(Dbinbase *pDb,Postg<char> *pPg);
   Cmls(const char *namdbn,const char *nampsg);
   Cmls(Docum &Doc);
   Cmls(const char *namdoc);
   ~Cmls(void);

      //Counting functions.
   void init_cnt(void); //Sets pdoc uniform.
      //Sets memory for tx and sx.
   void zerot(void); //Zeroes tx array.
   void zeros(void); //Zeroes sx array.
   void countDoc(long i); //Adds the counts from doc i to
      //tx using read function.
   void counsDoc(long i); //Adds the counts from doc i to
      //sx using read function.
   void counbDoc(long i); //Adds the counts from doc i to
      //sx & tx using read function.
      //Set reads
   void countTX(Index *cnd); //adds all counts in cnd to tx.
   void countSX(Index *cnd); //adds all counts in cnd to sx.
   void countBX(Index *cnd); //adds all counts in cnd to sx & tx.
   void Set_Term_wt(double cut); //Set the mrk array 1 if wt>=cut
   void Set_Term_alp(double cut); //Set the mrk array 1 if alpha>=cut
   void Set_Term_chi(double cut); //Set the mrk array 1 if chi sq>=cut

   void Set_Term_freq(long nm); //Set the mrk array
     //If term_freq<nm then 0, else 1

   //Learning functions
   void Set_Docs(Index *gdd,Index *bdd); //Set cls array
     //gdd good docs, bdd bad docs
   void Set_Lambda(double lam); //Sets lambda parameter to
     //2.0*tdoc*lambda of paper
   void Learn(long rnds); //rnds rounds of learning for weights
   double Func(double cx); //Computes and returns function value
     //cx is value to multiply quadratic error.
   double *ScoreAll(void);
   double *ScoreSet(Index *ind);

   //Relate to the documents
   long tdoc; //Number of training documents 
   long *cls; //Marks the class with 1 or -1

   //Relate to the terms processing
   double eps; //Size of 1/ndoc.
   double nnx;
   double nsx;
   double *tx;
   double *sx;
   double *pdoc;
   double lambda; //2.0*tdoc*lambda of paper
   double *wt; //weights of the process
   double *dl; //Delta of the process
   double *rt; //Cumulator of the process
   double th; //Threshhold
   double td; //Delta for threshhold
   long *mrk; //Marks which terms to include in process
 
   double *sco;
};

class Cmlsf : public Manf {
public:
   Cmlsf(Dbinbase *pDb,Postg<float> *pPg);
   Cmlsf(const char *namdbn,const char *nampsg);
   ~Cmlsf(void);

      //Counting functions.
   void init_cnt(void); //Sets pdoc uniform.
      //Sets memory for tx and sx.
   void zerot(void); //Zeroes tx array.
   void zeros(void); //Zeroes sx array.
   void countDoc(long i); //Adds the counts from doc i to
      //tx using read function.
   void counsDoc(long i); //Adds the counts from doc i to
      //sx using read function.
   void counbDoc(long i); //Adds the counts from doc i to
      //sx & tx using read function.
      //Set reads
   void countTX(Index *cnd); //adds all counts in cnd to tx.
   void countSX(Index *cnd); //adds all counts in cnd to sx.
   void countBX(Index *cnd); //adds all counts in cnd to sx & tx.
   void Set_Term_wt(double cut); //Set the mrk array 1 if wt>=cut
   void Set_Term_alp(double cut); //Set the mrk array 1 if alpha>=cut
   void Set_Term_chi(double cut); //Set the mrk array 1 if chi sq>=cut

   void Set_Term_freq(long nm); //Set the mrk array
     //If term_freq<nm then 0, else 1

   //Learning functions
   void Set_Docs(Index *gdd,Index *bdd); //Set cls array
     //gdd good docs, bdd bad docs
   void Set_Lambda(double lam); //Sets lambda parameter to
     //2.0*tdoc*lambda of paper
   void Learn(long rnds); //rnds rounds of learning for weights
   double Func(double cx); //Computes and returns function value
     //cx is value to multiply quadratic error.
   double *ScoreAll(void);
   double *ScoreSet(Index *ind);

   //Relate to the documents
   long tdoc; //Number of training documents
   long *cls; //Marks the class with 1 or -1

   //Relate to the terms processing
   double eps; //Size of 1/ndoc.
   double nnx;
   double nsx;
   double *tx;
   double *sx;
   double *pdoc;
   double lambda; //2.0*tdoc*lambda of paper
   double *wt; //weights of the process
   double *dl; //Delta of the process
   double *rt; //Cumulator of the process
   double th; //Threshhold
   double td; //Delta for threshhold
   long *mrk; //Marks which terms to include in process

   double *sco;
};

}
#endif
