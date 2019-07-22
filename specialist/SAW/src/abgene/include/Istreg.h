#ifndef ISTREG_H
#define ISTREG_H

#include <iostream>
#include <fstream>
#include <FBase.h>
using namespace std;
namespace iret {

typedef long *pLong;

class Istreg : public FBase{
public:
  Istreg(void);
  Istreg(const char *nam);
  Istreg(const char *nam,double xeps);
  Istreg(const char *nam,double xeps,long xlim,long angl); 
     //xeps is the limit for a positive
     //sum of a's to be considered significant.
     //xlim is the size of chunks to consider in redux.
  ~Istreg();

     //Note that data is always assumed to be constructed so
     //probabilities increase with increasing xr (and xs).
  void set_data(long n,double *x,double *sd, double *sn);
     //The number n is the data set size.
     //x is x-coordinate
     //sd is weight, sn is weight*value.
  void set_data(long n,double *x,double *y,double *sd, double *sn);
     //The number n is the data set size.
     //x is x-coordinate, y is y-coordinate
     //sd is weight, sn is weight*value.

  void dim2(); //Perform 2D regression.
  void dim2_redux(); //Perform 2D regression with reduction.
  void dim1(int s); //Perform 1D regression.
     //s is 0 or 1 depending on whether one
     //is processing by x or y.
  void dim1_ord(int s); //Perform 1D regression.
     //Assumes ordering is already done.
 
  double avg(); //Find average over all data.
  double info(); //Find information in p.
     //This function only works if p is a probability
  double sigma(); //Finds the standard deviation of true.
  double rms(); //Finds the rms deviation of true from p.

  void write_1df(void); //Used to write out a 1-dim function.
  void read_1df(void);  //Used to read in a 1-dim function.
  double val_1df(double x); //Returns the approximation give by function.

  void write_2df(void); //Used to write out a 2-dim function.
  void read_2df(void);  //Used to read in a 2-dim function.
  double val_2df(double x,double y); //Returns the approximation give by function.


  double *p;
     //p is order constrained probability

//private:
  //2D functions:
  void split(long mm,long *idx);
     //Perform the splitting into improved upper 
     //and lower sets. For 2D problems.
  void simple_split(double pav,long mm,long *idx,long &neg,pLong &idn,long &pos,pLong &idp);
     //Perform same split as previous function but no recursion. Just returns
     //the split.
  void split_redux(long mm,long *idx);
     //Perform the splitting into improved upper 
     //and lower sets solid sets and simple_split.
  void padjv_redux(long npp,long *idx,double xpv,double &pl,double &ph);
     //Pool adjacent violators algorithm specialized for redux operat.
  void dSort(const long n, long *ix);
     //A heapsort routine.
  int lexcomp(const long, const long);
     //Performs 2D lexical order comparisons.
  void rSort(const long n, long *ix);
     //Performs 1D sort for split_redux.

  //1D functions:
  void padjv(long *idx);
     //Pool adjacent violators algorithm
  void sSort(const long nm,long *ord);
     //One dimensional sort using heap sort.

  long num; //set size;
  double *xr;
  double *xs;
     //xr is x-coordinate, xs is y-coordinate
  double *sco; //Points at selection for 1D
  double *xd;
  double *xn;
     //xd is weight, xn is weight*value.
  double eps; //Positive sum size limit.
  long slim; //Target size for reduction.
  long delt; //Size of angle to use in sweep. 
     //Should be a factor of 90.

  double pmin; //Holds the minimum p value for 2D function.
  double pmax; //Holds the maximum p value for 2D function.

  long mem_r; //Remembers if new called to assign memory to xr.
  long mem_s; //Remembers if new called to assign memory to xs.
};
}
#endif
