#ifndef RATE_H
#define RATE_H

#include <iostream>
#include <fstream>
#include <Vnab.h>
using namespace std;
namespace iret {

const int NUM_QUERIES=100; //Number of queries in the test set.
const int DOCS_PER_QUERY=50; //Number of documents associated with each query.

class Docum; //forward declaration.
class Vnab; //forward declaration.

class Rate {
   public:
      Rate(char *nm); //Allocates space for queries. 
          //Stores the string *nm in *name for use in reading or writing.
          //Sufficient setup for use of create_iret_ret().
      void cset(int smooth); //Sets smooth flag.
          //Completes allocation of space for full function.
          //For standard test sets.
      void cset(int smooth,long num_q,long doc_pq,long s_size);
          //For general testing. Sets num_queries, docs_per_quer, and set_size.
      ~Rate();
      void change_name(char *nm); //Allows to change the name string for class.

      void query_fill(ifstream &fqry); //Fills the query array from the "i" file
          //with the next query's data.
      void construct_ifile(Docum &Doc); //Constructs the "i" file from the "u"
          //file and Doc.
      void create_iret_ret(Vnab &Vnb,float (*q_local)(int,long),char *iret_name); 
          //Uses the Score function of Vnb and the query_fill function to obtain 
          //retrieval which is written out in standard format in iret file with 
          //extension contained in iret_name.

      void set_ix(int tt); //Sets the initial configuration of the array ix to 
          //have tt nonzeroes (left).
      int next_ix(int tt); //Finds next configuration with tt nonzeroes and 
          //returns 0 if unsuccessful.
      void invert_ix(void); //Inverts the 0's and 1's in the array.
      void mak_jpc(void); //Uses the ones in ix to construct the values in jpc 
          //from the individual judges judgments.
      void feat_val(long nm,Vterm *ptm,float pri,float &ps,float &pt,float &qt);
          //Assumes that mak_jpc has been done.
          //Assumes that a particular query and its group have been read in (#nm).
          //Evaluates the distribution term using its ptm and for this query 
          //group gives probability that the feature is not random (depends on prior)
          //Also gives pt and qt for use in standard Bayesian weighting formula.
      Num_num *feat_xnum(long nm,Vterm *ptm,float &xt,float &xr,float &xnt,float &xst);
          //Returns count values and pointer to btree.
      float feat_wtt(long dn,Num_num *pNm,long qn,float yt,float yr,float ynt,float yst);
          //Returns the weight for a document that has the term in it used to set ptm in
          //previous function.

      void create_iret_jpc(const char *iret_name); //Uses the jpc file and 
          //"rate_occur.i" through query_fill() to obtain retrieval which is 
          //written out in standard format in iret file with extension contained 
          //in iret_name.

      void gen_load(int sflag,char *iret_name); //For general loading of files
          //of precisions. Path to file to load must be in path_rateset. File
          //name must be name of class and extension held in iret_name.

      void prec_load(int sflag,const char *iret_name); //Reads in the iret file using
          //the name held in iret_name. Associates precisions from jpc with
          //the pairs in iret using repetitive query_fill calls to obtain the
          //natural ordering of pairs and the numbers to match the iret numbers.
          //sflag is 1 or 2 indicating which series to fill.
      void prec_load_pmid(int sflag,const char *iret_name); //Just as previous
          //but works for PMIDs instead of index numbers in the system.

      void str_sel(void); //Fills the sel array with the full set of queries.
      void ran_sel(void); //Fills the sel array with a random selection of the 
          //queries.

      void ave_val(int sflag); //Uses the queries listed in the sel array to 
          //select From pli to average for fxi. Here i is sflag.
      void ind_val(int sflag,int n); //Uses the query numbered n to fill fxi 
          //where i=sflag.

      void smooth(int sflag); //Smooths the fxi array, i=sflag.

      void gopen_write(ios_base::openmode nMode); //Opens fout for output. nMode 
          //should be ios::app or ios::out.

      void print_prec(int sflag); //Prints out summary of the top nr
          //ranks of precision data held in fxi, i=sflag. Indicates which
          //iret file was used to construct. Data not smoothed.

      void print_meas(int sflag,int nr); //Prints out the precision and
          //information in the top nr ranks in fxi, i=sflag. Data not smoothed.

      void print_clim(int sflag,int nr,float sig,long sn); //Uses bootstrap
          //method to calculate confidence limits (two sided) at sig 
          //significance level. Based on sn trials. sflag and nr as before. 
          //Data not smoothed.

      void print_comp(int nr,float sig,long sn); //Uses bootstrap method
          //involving paired samples from both series to calculate significance
          //levels for the difference in values. Data not smoothed.

      void print_uplim(int nr); //Calculates the upper limit series and prints.
      void print_lolim(int nr); //Calculates the lower limit series and prints.

      void gclose_write(void); //Closes the fout stream.
      void debug(int query); //Used to see content for Debugging purposes.

      float compt(int nr,float *fx,float &ri); //nr is the number of ranks to
          //include in the calculation. 
          //Precision over the top nr ranks is returned and ri holds the
          //information for the same.
      float pr1; //Holds precision from 1 series
      float pr2; //Holds precision from 2 series
      float if1; //Holds information from 1 series
      float if2; //Holds information from 2 series

   //private:

      char *name; //Holds the string of name information given at time of 
         //construction.

      int smflag; //Smoothing flag. If 1 then all results get smoothed.

      ofstream fout; //Stream object for output of results.

      long num_queries; //Holds the number of queries in a data set.
      long docs_per_query; //Holds the number of documents for a query.
      long set_size; //Holds the number of documents in the database.

      long qnum; //Number of the query document.
      long *dnum; //Array of document numbers that belong to qnum.

      int nj; //Number of judges in set.
      int *ix; //Array indicating judges to include by 1, not include by 0.
      char **jpath; //Array of pointers at paths to the judgment files.

      long *sel; //Array of queries selected for evaluation.

      float **pl1; //Array of pointers at arrays of precisions for individual 
          //pairs. Order here is the retrieval order.
      float **pl2;
      float **jpc; //Array of pointers at arrays in which to construct and 
          //store precisions based on ix in their natural order.

      float *fx1; //Array of precisions, usually averaged over the set of 
          //queries in sel.
      float *fx2;
      char *iret1; //Holds extension for the iret file from which pl1 
          //constructed.
      char *iret2;
};

void confid(float sig,long sn,float *fx,float &pl,float &ph);
      //sig - significance level for two tailed test.
      //sn - number of trials.
      //fx - array holding the numerical values for the trials.
      //pl - lower confidence limit. Computed by function.
      //ph - upper confidence limit.
}
#endif
