#ifndef FOCUS_H
#define FOCUS_H

#include <iostream>
#include <fstream>
#include "Blist.h"
#include "Regist.h"
#include "Docum.h"
#include "Post.h"
#include "DataObj.h"
using namespace std;
namespace iret {

class Focus {
   public:
      Focus(const char *); //By default count_lim=200000.  
      Focus(const char *,long lim); //count_lim=lim.  
      ~Focus();

      void gopen_Focus(Regist *pReg,Slice_Accomp *pSac); //Opens for operating. 
         //Sets up slice access based on inv_load in Slice_Accomp class.
         //sets up arrays needed for function of this class.
      void gopen_Focus_map(Regist *pReg,Slice_Accomp *pSac); //Opens for operating. 
         //Sets up slice access based on mapping.

      //Memory allocation.
      void mem_Terms(long n); //Number of terms to allow in list.
      void mem_Docms(long n); //Number of documents to allow in list.
         //This is upper limit. dlim has control

      //Counting functions.
      void zerot(void); //Zeroes tx array.
      void zeros(void); //Zeroes sx array.
      void transfer(void); //Transfers all counts from freq array to tx.
      void countDoc(long i); //Adds the counts from doc i to tx using read function.
      void counsDoc(long i); //Adds the counts from doc i to sx using read function.
      void counstDoc(long i); //Adds the counts from doc i to sx & tx using read function.
      void ncountDoc(long i); //Subt the counts from doc i from tx using read function.
      void ncounsDoc(long i); //Subt the counts from doc i from sx using read function.
      void ncounstDoc(long i); //Subt the counts from doc i from sx using read function.
      void countTot(long ix,long *ixx); //adds all counts in array to tx.
      void countSub(long ix,long *ixx); //adds all counts in array to sx.
      void countBth(long ix,long *ixx); //adds all counts in array to sx and tx.
      void ncountTot(long ix,long *ixx); //subtracts all counts in array from tx.
      void ncountSub(long ix,long *ixx); //subtracts all counts in array from sx.
      void ncountBth(long ix,long *ixx); //subtracts all counts in array from sx and tx.

      //Restart process with zero memory of past.
      void restart(void);

      //Weighting functions.
      void weightApos(void); //Weights assigned to n best alpha terms.
      void weightApos(long flm); //Weights assigned to n best alpha terms.

      //Sets dlim based on weights of terms and how many terms are selected.
      void set_Dlim(void);

      //Scoring functions.
      float *scoreAll(void); //Performs scoring for all documents.
      long scoreCnt(void); //Counts docs that score above the cutoff dlim.
      long scoreCnt_Sub(void); //Counts docs from idx that
         //score above the cutoff.

      //Specialized counting functions.
      void init_Full(long im,long *imd); //Does initial count and set history array.
      void countUpdate(void); //Updates the counts in sx using dlim, sco, and mid.
      void countUpidx(void); //Updates the counts in sx using dlim, sco, and mid.
         //Uses idx array as source.

      //Special term functions.
      void promote(long id,long k); 
         //Promotes a term. Has k*1 million added to alpha.
      void promote_subs_auto(const char *str,long k); 
         //Promotes all the material containing str. k*1 million added to alpha. 
      void remove(long id); //Removes a term from the promoted list.
      void promote_all(void); 
         //Lists all terms and gives operator a chance to promote each.
      void promote_unprom(void); //Lists all the unpromoted material
      void promote_prom(void); //Lists all the positively promoted material
      void promote_demt(void); //Lists all the negatively promoted material
         //Same for functions on substrings
      void promote_subs_all(const char *str); //Lists all the material containing str.
      void promote_subs_unprom(const char *str); //Lists all the unpromoted material
      void promote_subs_unprom_auto(const char *str); 
         //Auto all the unpromoted material
      void promote_subs_prom(const char *str); //Lists all the positively promoted material
      void promote_subs_demt(const char *str); //Lists all the negatively promoted material

      //Keyboard display functions.
      void show_Terms(void);
      void show_Docms(long n); //Displays n documents.

      //integrated functions
      void Initiator(long tnm,long sn,Index *pInd); //Set up-weighting
      void Initiator(Theme *pThm,long bfl); //Set up. bfl=1 for background res.
         //0 otherwise.
      void Iterator(long tnm,long sn); //Processes through one cycle. 
      //subspace integrated functions
      void Background_s(Index *pInd); //Sets up to operate in space defined
         //by pInd
      void Initiator_s(long tnm,long sn,Index *pInd); //Set up-weighting
      void Iterator_s(long tnm,long sn); //Processes through one cycle. 

         //Assumes weights available. tnm = #terms. xbias = bias on dlim.
      Theme *Save(void); //Creates a theme to be added to history list.
      void Write(Theme *pThm); //Writes the theme to a standard file name.
         //User supplied extension.
      Theme *Read(void); //Reads in theme from standard file name.
         //User supplied extension.
      void Write(ofstream &fout,Theme *pThm); //Writes the theme to a standard file name.
         //User supplied extension.
      Theme *Read(ifstream &fin); //Reads in theme from standard file name.
         //User supplied extension.

      //Global data
      long nid; //Next unused term id number.
      long *tx; //For total counts based on current set. nid size.
      long *sx; //For total counts based on subject set. nid size.
      long nnx; //Total documents for purposes of weighting.
      long nsx; //Number of documents in the subject area for weighting.
      float cs; //Additve constant for correct scoring of docs.

      float tlim; //The lower alpha limit for terms.
      long mtc; //Number of terms selected.
      long mt; //limit on number of terms to allow.
      long *mti; //Array to hold the term ids.
      float *weg; //For storage of weights of terms.
      float *alp; //For storage of alpha values of terms.
      float *cst; //For storage of the cs value of terms.
      long *msa; //Special terms, promoted or demoted. Holds term id neg
                 //if demoted, pos. if promoted.
      long *msm; //Special terms value factor.
      long ms; //Size of the promotion array. 
      long msc; //Number of special terms.
 
      float dlim; //The lower limit for document scores.
      long stn; //stringency. This times average wt is dlim.
      long mdc; //Number of documents selected.
      long md; //Number of documents possible.
      long *mdi; //Stores the doc index history.
      float *sco; //For storage of scores.
        //Doc subset
      long ix; //Number in subset.
      long *idx; //Indexes in set.
      long bflag; //Flag =1 restricted set, 0 for full set.

      Regist *pRgg; //Holds pointer to Regist object that is used for document access.
      Slice_Accomp *pSaa; //Holds pointer to Slice_Accomp object that is used for
         //postings access.
      char *name; //Holds the occurrence name. 
      long count_lim; //Count limit
      long flim; //Limit on frequency of term, 0 ignored
      int err_flag; //Error flag is 0 for no error, 1 if count_lim exceeded.
};

//Multithreading version

extern "C" void *tscore(void *pth_data);
extern "C" void *fcnt_t(void *arg); //Thread function.
extern "C" void *fcnt_s(void *arg); //Thread function.
extern "C" void *fcnt_b(void *arg); //Thread function.
extern "C" void *fncnt_t(void *arg); //Thread function.
extern "C" void *fncnt_s(void *arg); //Thread function.
extern "C" void *fncnt_b(void *arg); //Thread function.

class Focus_pth {
   public:
      Focus_pth(const char *); //By default count_lim=200000.  
      Focus_pth(const char *,long lim); //count_lim=lim.  
      ~Focus_pth();

      void gopen_Focus_pth(Regist_pth *pReg,Slice_Accpth *pSac); //Opens for operating. 
         //Sets up slice access based on inv_load in Slice_Accpth class.
         //sets up arrays needed for function of this class.
      void gopen_Focus_map(Regist_pth *pReg,Slice_Accpth *pSac); //Opens for operating. 
         //Sets up slice access based on mapping.

      //Memory allocation.
      void mem_Terms(long n); //Number of terms to allow in list.
      void mem_Docms(long n); //Number of documents to allow in list.
         //This is upper limit. dlim has control

      //Counting functions.
      void zerot(void); //Zeroes tx array.
      void zeros(void); //Zeroes sx array.
      void transfer(void); //Transfers all counts from freq array to tx.
      void countTot(long ix,long *ixx); //adds all counts in array to tx.
      void countSub(long ix,long *ixx); //adds all counts in array to sx.  
      void countBth(long ix,long *ixx); //adds all counts in array to sx and tx.
      void ncountTot(long ix,long *ixx); //subtracts all counts in array from tx.
      void ncountSub(long ix,long *ixx); //subtracts all counts in array from sx.
      void ncountBth(long ix,long *ixx); //subtracts all counts in array from sx and tx.

      //Restart process with zero memory of past.
      void restart(void);

      //Weighting functions.
      void weightApos(void); //Weights assigned to n best alpha terms.
      void weightApos(long flm); //Weights assigned to n best alpha terms
         //of frequency <= flm.

      //Sets dlim based on weights of terms and how many terms are selected.
      void set_Dlim(void);

      //Scoring functions.
      float *scoreAll(void); //Performs scoring for all documents.
      long scoreCnt(void); //Counts docs that score above the cutoff dlim.
      long scoreCnt_Sub(void); //Counts docs from idx that
         //score above the cutoff.

      //Specialized counting functions.
      void init_Full(long im,long *imd); //Does initial count and set history array.
      void countUpdate(void); //Updates the counts in sx using dlim, sco, and mid.
      void countUpidx(void); //Updates the counts in sx using dlim, sco, and mid.
         //Uses idx array as source.

      //Special term functions.
      void promote(long id,long k); //Promotes a term. Has k*1 million added to alpha.
      void promote_subs_auto(const char *str,long k); 
         //Promotes all the material containing str. k*1 million added to alpha. 
      void remove(long id); //Removes a term from the promoted list.
      void promote_all(void); //Lists all terms and gives operator a chance to promote
                               //each.
      void promote_unprom(void); //Lists all the unpromoted material
      void promote_prom(void); //Lists all the positively promoted material
      void promote_demt(void); //Lists all the negatively promoted material
         //Same for functions on substrings
      void promote_subs_all(const char *str); //Lists all the material containing str.
      void promote_subs_unprom(const char *str); //Lists all the unpromoted material
      void promote_subs_unprom_auto(const char *str); //Auto all the unpromoted material
      void promote_subs_prom(const char *str); //Lists all the positively promoted material
      void promote_subs_demt(const char *str); //Lists all the negatively promoted material

      //Keyboard display functions.
      void show_Terms(void);
      void show_Docms(long n); //Displays n documents.

      //integrated functions
      void Initiator(long tnm,long sn,Index *pInd); //Set up-weighting
      void Initiator(Theme *pThm,long bfl); //Set up. bfl=1 for background res.
         //0 otherwise.
      void Iterator(long tnm,long sn); //Processes through one cycle. 
         //Assumes weights available. tnm = #terms. xbias = bias on dlim.
      //subspace integrated functions
      void Background_s(Index *pInd); //Sets up to operate in space defined
         //by pInd
      void Initiator_s(long tnm,long sn,Index *pInd); //Set up-weighting
      void Iterator_s(long tnm,long sn); //Processes through one cycle.

      Theme *Save(void); //Creates a theme to be added to history list.
      void Write(Theme *pThm); //Writes the theme to a standard file name.
         //User supplied extension.
      Theme *Read(void); //Reads in theme from standard file name.
         //User supplied extension.
      void Write(ofstream &fout,Theme *pThm); //Writes the theme to a standard file name.
         //User supplied extension.
      Theme *Read(ifstream &fin); //Reads in theme from standard file name.
         //User supplied extension.

      //Debug utility
      void debug_mdi(); //Prints out the marked doc indices and scores and dlim.

      //Global data
      long nid; //Next unused term id number.
      long *tx; //For total counts based on current set. nid size.
      long *sx; //For total counts based on subject set. nid size.
      long nnx; //Total documents for purposes of weighting.
      long nsx; //Number of documents in the subject area for weighting.
      float cs; //Additve constant for correct scoring of docs.

      float tlim; //The lower alpha limit for terms.
      long mtc; //Number of terms selected.
      long mt; //limit on number of terms to allow.
      long *mti; //Array to hold the term ids.
      float *weg; //For storage of weights of terms.
      float *alp; //For storage of alpha values of terms.
      float *cst; //For storage of the cs value of terms.
      long *msa; //Special terms, promoted or demoted. Holds term id neg
                 //if demoted, pos. if promoted.
      long *msm; //Special terms value factor.
      long ms; //Size of the promotion array. 
      long msc; //Number of special terms.
 
      float dlim; //The lower limit for document scores.
      long stn; //stringency. This times average wt is dlim.
      long mdc; //Number of documents selected.
      long md; //Number of documents possible.
      long *mdi; //Stores the doc index history.
      float *sco; //For storage of scores.
        //Doc subset
      long ix; //Number in subset.
      long *idx; //Indexes in set.
      long bflag; //Flag =1 restricted set, 0 for full set.

      Regist_pth *pRgg; //Holds pointer to Regist object that is used for document access.
      Slice_Accpth *pSaa; //Holds pointer to Slice_Accpth object that is used for
         //postings access.
      char *name; //Holds the occurrence name. 
      long count_lim; //Count limit
      long flim; //Limit on frequency of term, 0 ignored
      int err_flag; //Error flag is 0 for no error, 1 if count_lim exceeded.
};

}

#endif
