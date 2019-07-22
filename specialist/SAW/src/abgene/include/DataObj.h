#ifndef DATAOBJ_H
#define DATAOBJ_H

#include <Blist.h>
#include <Regist.h>

#define THM_READ_W 1 //Sets up reading for .w and reads in the .a file of addresses.
#define THM_READ_S 2 //Reads in the .s file.


using namespace std;
namespace iret {

class Theme {
   public:
      Theme(void);
      Theme(Theme *pThm); //copy constructor
      ~Theme(void);
      void Union(Theme *pTm); //Combines this with pTm. Term number
         //becomes the count of union of terms. Stringency is
         //same as this.
      void Order_lex(void);
      void Order_alp(void);
      void Order_weg(void);
      //Functions to compare themes and derive string representations
      //for human examination of diff of a set of themes.
      void Set_Diff(void); //Need to have all themes in lex order
      void Diff(Theme *pThm); //Produces the count of number of themes
         //not containing a term as first diff for each term in 
         //the theme. Results in array cmm.
      //Function to compare themes
      float Dice_lex(Theme *pThm);
      float Dice_alp(Theme *pThm);
      float Dice_weg(Theme *pThm);
      //Function to compare themes, asymmetric
      float Subs_lex(Theme *pThm);
      float Subs_alp(Theme *pThm);
      float Subs_weg(Theme *pThm);
      //Function to monitor strength of theme
      float Ave_Alpha(long n); //Averages the top n alphas
      //Debug functions
      void debug(void); 

      void rnw_Mem(long n); //Resets memory arrays if
         //not sufficient for size n.
      void bin_Write(fstream &fout); 
      void bin_Read(fstream &fin);  
         //Functions to read and write
         //binary using term ids.
      void Write(fstream &fout,IdTerm *pIdt);
         //Writes out standard ascii version
         //with strings. 
      void Read(fstream &fin,IdTerm *pIdt);
         //Reads in standard ascii version
         //with strings.

      long mt; //#terms allowed
      long mtc; //#terms selected
      long *mti; //term ids (mt array length)
      float *alp; //alphas of terms (mt array length)
      float *weg; //weights of terms (mt array length)
      //Promotion data
      long ms; //#special terms allowed
      long msc; //#special terms
      long *msa; //special term ids, + promoted, - demoted
         //ms array length
      //Score stringency 
      long stn; 
      //Weight factor
      float cs;
      //Array for comparisons only
      long *cmm; //Only created by Set_Diff
};

class ATheme : public FBase {

   public:
      ATheme(Slice_Accpth *pSac); //Sets pointer pSaa.
      ATheme(const char *nm,Slice_Accpth *pSac); //Sets pointer pSaa.
           //Stores the string *nm in *name for use in reading or writing.
      ~ATheme();

   //Reading functions.
      void gopen_map(int rfil); //Mmaps data and sets up w file reading depending
           //on which bits are set in rfil. Sets gct=-1.
      void read(Theme &Thm); //Fills Thm. To be used only after a call to
           //gopen_read(). Reads at the current file pointer position.
           //Increments gct by 1.
      void read(long n,Theme &Thm); //Fills Thm. To be used only after a
           //call to gopen_read(). Reads in theme numbered n (starts at 0).
           //Sets gct=n.
      void gclose_map(int rfil); //munmaps files depending on
           //bits set in rfil.

   //Writing functions.
      void gopen_write(void); //Initiallizes counter and opens files for writing
           //bits are set for these in rfil.
      void gopen_append(void); //Initiallizes counter and opens files for append
      void write(Theme &Thm); //Writes out a document into the "w" file and records the
           //relevant data in associated files. Increments the global counter.
      void gclose_write(); //Writes out document number and closes files.

   //Global data.
      long nthm; //Number of themes in set.

   //private:
   //Global data.
      long *addr; //Array for addresses of themes.
      long *size; //Array for theme size (mtc).

   //Accessory
      long gct; //Global document counter used in writing documents.
      fstream *pfwd; //Points at the file stream object for .w file.
      ofstream *pfad; //For address file construction
      ofstream *pfsz; //For size file construction
      Slice_Accpth *pSaa; //Holds pointer to Slice_Accpth object that is used for
         //postings access.
};

class BTheme : public FBase {
   public:
      BTheme(void);
      BTheme(const char *nm);
           //Stores the string *nm in *name for use in reading or writing.
      ~BTheme();

   //Reading functions.
      void gopen_map(int rfil); //Mmaps data and sets up w file reading depending
           //on which bits are set in rfil. Sets gct=-1.
      void read(Theme &Thm); //Fills Thm. To be used only after a call to
           //gopen_read(). Reads at the current file pointer position.
           //Increments gct by 1.
      void read(long n,Theme &Thm); //Fills Thm. To be used only after a
           //call to gopen_read(). Reads in theme numbered n (starts at 0).
           //Sets gct=n.
      void crt_Theme_array(void); //Uses memory map to set pThm[i], all i<nthm.
           //creates pThm array first.
      void dst_Theme_array(void); //destoys pThm[i], all i<nthm, and pThm array.
      void gclose_map(int rfil); //munmaps files depending on
           //bits set in rfil.

   //Writing functions.
      void gopen_write(void); //Initiallizes counter and opens files for writing
           //bits are set for these in rfil.
      void gopen_append(void); //Initiallizes counter and opens files for append
      void write(Theme &Thm); //Writes out a document into the "w" file and records the
           //relevant data in associated files. Increments the global counter.
      void gclose_write(); //Writes out document number and closes files.

   //Global data.
      long nthm; //Number of themes in set.
      Theme **pThm; //Pointer at array of themes.

   //private:
   //Global data.
      long *addr; //Array for addresses of themes.
      long *size; //Array for theme size (mtc).
      char *wfil; //Array to point at w file.

   //Accessory
      long gct; //Global document counter used in writing documents.
      fstream *pfwd; //Points at the file stream object for .w file.
      ofstream *pfad; //For address file construction
      ofstream *pfsz; //For size file construction
};

class Order; //Forward declaration.

class Index {
   public:
      Index(void); //Simple constructor;
      Index(long num); //Sets up memory;
      Index(long begin,long end_plus_one);
      Index(long num,long *imd,long oflag); //Sets up memory and populates. Orders
         //if oflag=1. Orders and uniques if oflag=2.
      Index(Index *ind); //copy constructor
      ~Index(void); //Frees memory if pointer not null.
      void sSort(void); //Performs a simple heapsort on idx.
      long unique(void); //Checks for duplicates and removes.
        //Must be ordered first.
      Index *cbool_And(Index *jnd);//Ands with the argument
      Index *cbool_Or(Index *jnd);//Ors with the argument
      Index *cbool_Butnot(Index *jnd);//Removes the argument

        //Identifies a subset by indices
      long Subvalue(long j); //on success returns value k+1 such that idx[k]=j.
          //Otherwise returns 0.
      Index *Subvalue(Index *jnd); //jnd must represent a subset and the 
          //returned Index object will give the values k such that idx[k]
          //is among jnd->idx values.

      Index *Subinterval(long n,long m); //Makes the index from
         //interval n<=x<m (idx[n] to idx[m-1]).
      Index *Subsample(long n,long seed); //Generates a random sample of
         //idx of size n if ix>n, else returns this.

        //Subset by threshold sxx[n] and idx[n] defined.
      Index *Greater(float *sxx,float thresh); //All values such
          //that *(sxx+n)>thresh.
      Index *Greateq(float *sxx,float thresh); //All values such
          //that *(sxx+n)>=thresh.
      Index *Greater(double *sxx,double thresh); //All values such
      Index *Greateq(double *sxx,double thresh); //All values such
      Index *Lesser(float *sxx,float thresh); //All values such
          //that *(sxx+n)<thresh.
      Index *Lesseq(float *sxx,float thresh); //All values such
          //that *(sxx+n)<=thresh.
      Index *Lesser(double *sxx,double thresh); //All values such
      Index *Lesseq(double *sxx,double thresh); //All values such
          //that *(sxx+n)<=thresh.

        //Subset by threshold sxx[idx] defined.
      Index *Greater(float thresh,float *sxx); //Subset of idx values such
          //that *(sxx+idx)>thresh.
      Index *Greateq(float thresh,float *sxx); //Subset of idx values such
          //that *(sxx+idx)>=thresh.
      Index *Greater(double thresh,double *sxx); //Subset of idx values 
      Index *Greateq(double thresh,double *sxx); //Subset of idx values 
      Index *Lesser(float thresh,float *sxx); //Subset of idx values such
          //that *(sxx+idx)<thresh.
      Index *Lesseq(float thresh,float *sxx); //Subset of idx values such
          //that *(sxx+idx)<=thresh.
      Index *Lesser(double thresh,double *sxx); //Subset of idx values 
      Index *Lesseq(double thresh,double *sxx); //Subset of idx values 
        //Orders by threshold sxx[idx] defined.
      Order *oGreater(float thresh,float *sxx); //Subset of idx values such
          //that *(sxx+idx)>thresh made into an Order.
      Order *oGreateq(float thresh,float *sxx); //Subset of idx values such
          //that *(sxx+idx)>=thresh made into an Order.
      Order *oGreater(double thresh,double *sxx); //Subset of idx values
      Order *oGreateq(double thresh,double *sxx); //Subset of idx values
      Order *oLesser(float thresh,float *sxx); //Subset of idx values such
          //that *(sxx+idx)<thresh made into an Order.
      Order *oLesseq(float thresh,float *sxx); //Subset of idx values such
          //that *(sxx+idx)<=thresh made into an Order.
      Order *oLesser(double thresh,double *sxx); //Subset of idx values
      Order *oLesseq(double thresh,double *sxx); //Subset of idx values

      void Convert_to_pmid(Regist *pReg); //Converts a standard index
      void Convert_to_pmid(Regist_pth *pReg); //Converts a standard index
          //to a list of pmids in the same order (increasing).
          //Must have called set_class for the Regist class before used.
      void Convert_to_index(Regist *pReg); //Converts a pmid form
      void Convert_to_index(Regist_pth *pReg); //Converts a pmid form
          //to a standard index list in the same order (increasing).
          //Must have called set_class for the Regist class before used.
      void write(ofstream &fout); //Writes the object into a file
      void read(ifstream &fin); //Reads the object in from a file
      void debug(void); //Writes object to standard out.

      long ix; //Number of elements.
      long *idx; //Elements in order.
};

class CValid {
public:
   CValid(Index *gdd, long n);
   CValid(Index *gdd, Index *bdd);
   ~CValid();
   void cross_valid(long m, long seed);
   Index *ind;
   Index *gnd;
   Index *bnd;
   Index **pGTS;
   Index **pBTS;
   Index **pWTS;
   Index **pGTR;
   Index **pBTR;
   Index **pWTR;
   long setn;

};

class Order {
   public:
      Order(void); //Simple constructor.
      Order(long n,long m,float *sco); //sco[i] is score for index i.
         //m is database & sco array size. Produces an Order of size n.
      Order(long n,long m,double *sco); //sco[i] is score for index i.
         //m is database & sco array size. Produces an Order of size n.
      Order(long n,Index *ind,float *sco); //sco[ind[i]] is interpretation.
         //Gives the top n.
      Order(long n,Index *ind,double *sco); //sco[ind[i]] is interpretation.
         //Gives the top n.
      Order(long n,float *sco,Index *ind); //idx[i] is index of
         //sco[i]. Produces an Order size n.
      Order(long n,double *sco,Index *ind); //idx[i] is index of
         //sco[i]. Produces an Order size n.
      Order(Order *pord); //copy constructor
      ~Order(void); //Removes data.
      Order *cbool_And(Index *jnd); //Ands with the argument but produces
         //an Order as output with the same scores and order restricted
         //to the subset.
      Order *cbool_Butnot(Index *jnd); //Removes the argument set and
         //produces an Order as output with the restricted parental
         //scores and order.
      float Precision(long n,Index *ind); //Assumes that ind is a subset
         //of the index object of this order and returns the precision over
         //the top n scores. Deals with problem of identical scores.
      long CtGreateq(float thresh); //Returns the number of elements with
         //scores greater than or equal to thresh.
      void write(ofstream &fout); //Writes the object into a file
      void read(ifstream &fin); //Reads the object in from a file
      void debug(void); //Prints data to standard out.

      //Data Access
      long num(void);  //Returns number of objects in set.
      long ind(long i,float &sco); //Returns doc number (index) and
         //score pair that correlate and are in decreasing score order as i
         //increases from 0 to num-1.
      long *seq(void); //Returns pointer at new array that contains the array
         //pInd->idx in decreasing score order. 

      Index *pInd; //Holds the list in standard form of index set.
      long *order; //Holds the order of decreasing score.
      float *score; //Holds the scores in the ord order.
};

}

#endif
