#ifndef ELEV_H
#define ELEV_H

#include <iostream>
#include <fstream>
#include <Vnab.h>
#include <DataObj.h>

using namespace std;
namespace iret {

class Resamp {
   public:
      Resamp(long num); //num is the size of the space
      ~Resamp(void);
      void zero(void); //Sets all counts to zero.
      void sample(long m,Index *pind); //Samples m times from set defined by
         //pind. Records counts in zsamp.
      void origin(Index *pind); //Sets the pind points a count of 1.
      long num; //size of space (should agree with Elev.num).
      long *zsamp; //Array of size num to hold counts.
};

class Elev {
   public:
      Elev(long nm); //nm is the number of entities that are ranked.
         //Usually this is the number of objects in the database. 
      ~Elev();

      void load(ifstream &fin); //Loads in the relevance data for a single 
         //query. Uses a standard format. Begins with query# and # of items.
         //This is then followed by that many items in form of entity index
         //number followed by relevance (between 0 and 1). 
      void load(Index *ind); //Loads in the relevance data where the
         //entity numbers in ind->idx[i] are the relevanant ones and all
         //have a relevance of 1.0.

      void process_ranks(long *idx); //idx is the array of indices of the
         //ranked set of num entities. The arrays pc and sc and tm are updated.
      void process_ranks(long ix,long *idx); //idx is the array of indices of the
         //ranked set of ix<=num entities. The arrays pc and sc and tm are updated.
      void process_ranks(Order *pord); //Must have pord->pInd->ix<=num.
         //This function avoids the problem that may arise with duplicate scores
         //that causes ambiguous ordering and unreliable measurements.
      void recall_prec_graph(double rc1,double rc2,Order *pord,ofstream &fout); 
         //Prints out the recall-precision pair (plus the score) for those 
         //recalls that fall in the range between rc1 and rc2. Starts at the 
         //top of the order and puts out a pair for each relevant data point 
         //encountered.
      void roc_graph(double rc1,double rc2,Order *pord,ofstream &fout); 
         //Prints out the recall-false drop pair (plus the score) for those 
         //recalls that fall in the range between rc1 and rc2. Starts at the 
         //top of the order and puts out a pair for each relevant data point 
         //encountered.
      double current(void); //Uses pc to compute spl (after call to 
         //process_ranks(). Returns spl.
      double summary(void); //Uses tm and sc to compute tc. Also sets spc
         //and returns it as a value.
      
      //Processing for paired bootstrap testing
      void rev_load(Resamp &Rsp); //Recalculates pt based on zsamp and rev.
      void process_ranks(long *idx,Resamp &Rsp); //idx is the array of indices of the
         //ranked set of num entities. The arrays pc and sc and tm are updated.
      void process_ranks(long ix,long *idx,Resamp &Rsp); //idx is the array of indices of the
         //ranked set of ix<=num entities. The arrays pc and sc and tm are updated.
      void process_ranks(Order *pord,Resamp &Rsp); //Must have pord->pInd->ix<=num.
         //This function avoids the problem that may arise with duplicate scores
         //that causes ambiguous ordering and unreliable measurements.

      //One_out functions.
      void feat_xnum(Vterm *ptm,double &xt,double &xr,double &xnt,double &xst);
          //Returns count values in last four arguments.
      double feat_wtt(long nm,long dm,double yt,double yr,double ynt,double yst);
          //Takes output of previous function as input here as well as relevance
          //as ratio nm/dm. Returns a weight for term occurrence
          //in based on exclusion of one doc with relevance nm/dm. 
      void feat_xps(Vterm *ptm,double pri,double &ps,double &pt,double &qt);
          //Bayesian calculation that the term is significantly associated
          //with the relevance non-relevance distinction.

      long num; //Number of documents in the database.
      double *rev; //Array of relevances of the entities in the database.
      double pt; //Holds the sum of relevances from rev.
      double *px; //Array of probabilities of relevance (isotonic).
      
      double rc[11]; //The eleven recall points.
      double pc[11]; //The eleven precisions. For one query.
      long tm; //Total number of queries thus far entered.
      double sc[11]; //The running total of eleven precisions 
         //for current set of queries that have been entered.
      double tc[11]; //Produced by summary() as the current summary
         //eleven precisions.
      double spl; //Local 11-point average precision just for current query
         //produced from pc by call to current().
      double spc; //Summary 11-point average precision 
};

}
#endif
