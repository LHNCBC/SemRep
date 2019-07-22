#ifndef DBINBASE_H
#define DBINBASE_H

#include <iostream>
#include <fstream>
#include <Hyper.h>
#include <Istreg.h>
#include <DataObj.h>

#define DBIN_A 1
#define DBIN_W 2
#define DBIN_L 4

namespace iret {

class Dbinbase : public FBase {
   friend class Docum;
   friend class Post;

   public:
      Dbinbase(const char *nm); //Creates the space for a document to be stored. 
           //Stores the string *nm in *name for use in reading or writing.
      ~Dbinbase();
      void create_files(const char *dnam,const char *pnam,float (*d_local)(int,long)); 
           //Creates the files "w", "l", "s", "a". "w" is a binary representation 
           //of documents as term numbers and "l" the local weights.
      void create_files(Docum &Doc,Count &Ct,float (*d_local)(int,long)); 
           //Creates the files "w", "l", "s", "a". "w" is a binary representation 
           //of documents as term numbers and "l" the local weights.
           //Here Ct must have the terms in lexical order with the term number
           //associated with the term. Numbering must begin at 1, not 0.

   //Operating functions.
      void gopen_operate(int rfil); //Puts files in ram ready for use.
      void gclose_operate(); //deletes the arrays for the data in ram. 

   //Global data.
      long ndoc; //Number of documents in set.
      long nwrd; //Number of terms in set.
      long *size; //Array for size of each document.
      long *don; //Space for all document word number lists.
      float *dow; //Space for all document word local weights.
      long *dad; //Addresses for documents in ram.
      
};

class Bayes : public Dbinbase {
   public:
      Bayes(const char *nm); //Creates the space for a document to be stored.
           //Stores the string *nm in *name for use in reading or writing.
           //Creates space for tx, sx, and ax.
      ~Bayes();
      void gopen_Bayes(void); //Opens for operating.
      void gclose_Bayes(void); //Closes for operating.
      
      //read function
      void read(long n); //Sets the pointers for doc n
      long nw; //Number of terms in doc.
      long *nwd; //Array of term numbers in doc.
 
      //Counting functions.
      void zerot(void); //Zeroes tx array.
      void zeros(void); //Zeroes sx array.
      void countDoc(long i); //Adds the counts from doc i to tx using slow_read function.
      void counsDoc(long i); //Adds the counts from doc i to sx using slow_read function.  
      //Reads
      void countTot(void); //adds all counts in whole set of ndoc docs to tx.
      void countTot(Index *idx); //adds all counts in array to tx.
      void countSub(Index *idx); //adds all counts in array to sx.

      //Weighting functions.
      void weightSall(void); //Weights assigned to all terms with !=0.
      void weightSneg(void); //Weights assigned to all terms with <0.
      void weightSpos(void); //Weights assigned to all terms with >0.
      void weightSidf(void); //Weights all terms by inverse document frequency
           //weighting. Assumes that tx has full counts in it.

      //Scoreing function
      double *ScoreAll(void);
      double *ScoreSet(Index *ind);

      //Selection functions.
      void select_chi(double cut); //Terms selected that satisfy chi square>cut.
      void select_axx(double cut); //Terms selected that satisfy ax > cut.
      void select_weg(double cut); //Terms selected that satisfy weg > cut.
      void zero_bsel(void); //Like zero_nsel but corrects cs for deleted terms.

      //Global data
      long *tx; //For total counts based on current set. nwrd size.
      long *sx; //For total counts based on subject set. nwrd size.
      long *tsel; //Array of selected terms. 
      long ntsel; //Number of selected terms.
      long nnx; //Total documents for purposes of weighting.
      long nsx; //Number of documents in the subject area for weighting.
      double *weg; //For weights, nwrd size.
      double *ax; //For alpha values, nwrd size.
      double *csg; //For individual cs numbers.
      double cs; //Additve constant for correct scoring of docs.
      double *sco;
};

class MBayes : public Dbinbase {
   public:
      MBayes(const char *nm); //Creates the space for a document to be stored.
           //Stores the string *nm in *name for use in reading or writing.
           //Creates space for tx, sx, and ax.
      ~MBayes();
      void gopen_MBayes(void); //Opens for operating.
      void gclose_MBayes(void); //Closes for operating.

      //read function
      void read(long n); //Sets the pointers for doc n
      long nw; //Number of terms in doc.
      long *nwd; //Array of term numbers in doc.
      float *lwt; //Array of local weights in doc

      //Counting functions.
      void zerot(void); //Zeroes tx array.
      void zeros(void); //Zeroes sx array.
      void countDoc(long i); //Adds the counts from doc i to tx using slow_read function.
      void counsDoc(long i); //Adds the counts from doc i to sx using slow_read function.
      //Reads
      void countTot(void); //adds all counts in whole set of ndoc docs to tx.
      void countTot(Index *idx); //adds all counts in array to tx.
      void countSub(Index *idx); //adds all counts in array to sx.

      //Weighting functions.
      void weightSall(void); //Weights assigned to all terms with !=0.

      //Scoreing function
      double *ScoreAll(void);
      double *ScoreSet(Index *ind);

      //Selection functions.
      void select_axx(double cut); //Terms selected that satisfy ax > cut.
      void zero_nsel(void);

      //Global data
      double *tx; //For total counts based on current set. nwrd size.
      double *sx; //For total counts based on subject set. nwrd size.
      long  *tsel; //Array of selected terms.
      long ntsel; //Number of selected terms.
      double nnx; //Total documents for purposes of weighting.
      double nsx; //Number of documents in the subject area for weighting.
      double *weg; //For weights, nwrd size.
      double *ax; //For alpha values, nwrd size.
      double *sco;
};

class Svmach : public Dbinbase {
   public:
      Svmach(const char *nm); //Creates the space for a document to be stored.
           //Stores the string *nm in *name for use in reading or writing.
      ~Svmach();
      void gopen_Svmach(void);
      void gclose_Svmach(void);

   //read function, it should go away
      void read(long n); //Sets the pointers for doc n
      long nw; //Number of terms in doc.
      long *nwd; //Array of term numbers in doc.
      float *lwt; //Array of local weights in doc
   
      void read(long n, int sflag);//read function for the training set,
         //after setup_Svmach(gdd,bdd)

  //Operating functions.
      void setup_Svmach(Index *gdd, Index *bdd); //Opens the "w" file for reading, reads in associated
           //files
      void remove_Svmach(void); //Closes for operating.
      double fasValue(long i); //Score of individual document.
      double kern12(void); //Dot product of docs 1 and 2 reads required.
      double kern13(void); //Dot product of docs 1 and 3.
      double kern23(void); //Dot product of docs 2 and 3.

      double svmObject(void); //Current value of objective function.
      int takeStep(long i,long j); //Takes a single step in optimization.
      int examineExample(long n); //Computes for a particular index.
      void optimizeSvm(double c); //Performs the SMO.
      void optimizeSvm(double c,double eps1); //Performs SMO with eps=eps1 as starting
           //value. Decreases by factor 1/10 till less than eps and ends.

      void write_coef(void); //Used to write out the alphas. Based on doc numbering
           //used after readdressing documents with set_readd(***) function.

   //Debugging functions.
      void debug_err(void); //Writes out and compares the error cache.
      void debug_alpha(void); //Writes out bth and the alphas in order.

   //Scoreing function
      double *ScoreAll(void);
      double *ScoreSet(Index *ind);

   //Global data.
      double *kerc; //Array for kernal of each document with self.
      double tol; //Tolerance used in algorithm.
      double eps; //Epsilon for step size in objective function.
      double cbd; //Bound on alphas.
      double *alp; //Set of alphas that undergo optimization.
      double *tgt; //Set of target values to learn;
      double *err; //Error cache.
      long *mrk; //Pointers for error cache.
      long sch; //Size of error cache (# alphas not at bounds).
      long cur; //Current marked position in looping over error cache.

   //Workspace variables
      double alpha1;
      double alpha2;
      double y1;
      double y2;
      double e1;
      double e2;

   //Dynamic tolerance adjustment
      long mtri; //Number of trials.
      long msuc; //Number of successes.
      long hsuc; //Upper limit for number of successes to adjust for.
      long lsuc; //Lower limit for number of successes to adjust for.
   
   //Local data.
      double bth; //Constant in linear function scoring.
      int nw1; //number of words in document one.
      long *nwd1; //Holds the word numbers in the document.
      float *lwt1;
      int nw2; //number of words in document two.
      long *nwd2; //Holds the word numbers in the document.    
      float *lwt2;
      int nw3; //number of words in document three.
      long *nwd3; //Holds the word numbers in the document.    
      float *lwt3;
      Index *gd; 
      Index *bd;
      long ncset; //size of current set.
      long *sizc; //Size array for current set.
      long **danc; //Addresses of doc data for current set. 
      float **dawc; //Addresses of doc data for current set.
      double *weg; //For weights, nwrd size.
      double *sco;

};

class Bayes_boost : public Dbinbase {
   public: //Does general Bayes to use with GBoost.
           //Does stump with boosting built in.
      Bayes_boost(const char *nm); //Creates the space for a document to be stored.
           //Stores the string *nm in *name for use in reading or writing.
           //Creates space for tx, sx, and ax.
      ~Bayes_boost();
      void gopen_Bayes_boost(void); //Opens for operating.
      void gclose_Bayes_boost(void); //Closes for operating.

      //Counting functions.
      void zerot(void); //Zeroes tx array.
      void zeros(void); //Zeroes sx array.
        
      void countDoc(long i); //Adds the counts from doc i to tx using read function.
      void counsDoc(long i); //Adds the counts from doc i to sx using read function.  
      
      //reads
      void countTot(void); //adds all counts in whole set of ndoc docs to tx.
      void countTot(Index *idx); //adds all counts in array to tx.
      void countSub(Index *idx); //adds all counts in array to sx.

      //Weighting functions.
      void weightSall(void); //Weights assigned to all terms with !=0.
            
      //Selection functions.
      void select_chi(double cut); //Terms selected that satisfy chi square>cut.
         //Only works if ncset is fixed by calling set_gdbd().
      void select_axx(double cut); //Terms selected that satisfy ax > cut.
      void select_axx(long num); //Selects top num terms for ax value.
      void zero_bsel(void); //Like zero_nsel but corrects cs for deleted terms.

      double *update_Score(Index *imd);
      
      void setup_Bayes_boost(Index *gdd, Index *bdd); 
      

            
      //Scoreing function
      double *ScoreAll(void);
      double *ScoreSet(Index *ind);
      double *sco; //score array, ndoc size

      //read function, it should go away
      void read(long n); //Sets the pointers for doc n
      long nw; //Number of terms in doc.
      long *nwd; //term number in doc


      //Global data
      double *tx; //For total counts based on current set. nwrd size.
      double *sx; //For total counts based on subject set. nwrd size.
      double nnx; //Total documents for purposes of weighting.
      double nsx; //Number of documents in the subject area for weighting.
      double *dt; //For probabilites, ndoc size. 
        //Point to GBoost pdoc
      double *weg; //For weights, nwrd size.
      double  eps; //one over the number of training objects 
      double *ax; //For alpha values based on current set. nwrd size.
      double *csg; //For individual cs numbers.
      double cs; //Additve constant for correct scoring of docs.
      long  *tsel; //Array of selected terms.
      long ntsel; //Number of selected terms.
      Index *gd; //Index object to hold up good stuffs in training set
      Index *bd; //Index object to hold up bad stuffs in training set

};

}
#endif
