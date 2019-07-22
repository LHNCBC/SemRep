#ifndef ISTGEN_H
#define ISTGEN_H

#include <iostream>
#include <fstream>
using namespace std;
namespace iret {

typedef long *pLong;
struct Pnode{
  long num;
  Pnode *next;
};

class Istgen {
public:
  Istgen(double xeps);
  ~Istgen();

     //Note that data is always assumed to be constructed so
     //probabilities increase with increasing xr (and xs).
  void set_data(long n,double *sd, double *sn);
     //The number n is the data set size.
     //sd is weight, sn is weight*value.

  void gen(int (*compar) (const long &, const long &)); //Perform regression.
 
  double avg(); //Find average over all data.
  double info(); //Find information in p.
  double sigma(); //Finds the standard deviation of true.
  double rms(); //Finds the rms deviation of true from p.

  void set_name(const char *nam); //Sets the name used by the files for 
     //functions.

  double *p;
     //p is order constrained probability

//private:
  //2D functions:
  void split(long mm,long *idx,int (*compar) (const long &, const long &));
     //Perform the splitting into improved upper 
     //and lower sets. For 2D problems.

  long total_objs; //set size;
  double *xd;
  double *xn;

     //xd is weight, xn is weight*value.
  double eps; //Positive sum size limit.
  double cmuf; //Machine underflow constant.

  char *name; //Space to hold type name.
};
}
#endif
