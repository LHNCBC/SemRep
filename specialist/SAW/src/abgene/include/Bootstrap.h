#ifndef BOOTSTRAP_H
#define BOOTSTRAP_H

#include <iostream>
#include <fstream>
using namespace std;
namespace iret {

class Bootstrap {
   public:
      Bootstrap(long num_q); //Allocates space for queries. 
      ~Bootstrap();
      
      void str_sel(void); //Fills the sel array with the full set of queries.
      void ran_sel(void); //Fills the sel array with a random selection of the 
          //queries.

      void load_1file(const char *fil); //fills the arrays fx1 & fx2
      void load_2file(const char *fil1,const char *fil2); //fills arrays
      void print_comp(float sig,long sn); //Uses bootstrap method
          //involving paired samples from both series to calculate significance
          //levels for the difference in values. Data not smoothed.

      void gopen_write(const char *name,ios_base::openmode nMode); //Opens 
          //fout for output. nMode should be ios::app or ios::out.
      void gclose_write(void); //Closes the fout stream.
     
   //private:

      ofstream fout; //Stream object for output of results.
      long num_queries; //Holds the number of queries in a data set.
      long *sel; //Array of queries selected for evaluation.
      float *fx1; //Array of precisions, usually averaged over the set of 
          //queries in sel.
      float *fx2;
      char *iret1;
      char *iret2;
     
};

void confid(float sig,long sn,float *fx,float &pl,float &ph);

}
#endif
