#ifndef SCMLS_H
#define SCMLS_H

#include <iostream>
#include <fstream>
using namespace std;
namespace iret {

class SCmls : public FBase {
public:
  SCmls(long dm, long ln, Index *gind, const char *nam);
   //constructor used in training part
   //dim is dimention of the data, i.e. how many score arrays are to be 
   //combined.
   //len is the length of score arrays
   //gInd is the pointer to the good elements

  SCmls(long dm, const char *nam);
   //Simple Constructor, used in testing part

  ~SCmls();

  void Setup(long i, double *sco);
  //Sets up scr through dim calls

  void Learn_Cmls(void);
  //Finds the separator using the CMLS algorithm(the version of Gauss-Seidel)
  //Takes dim dimentional data, but generates (dim+1) w's and theta=0
  //The resulting optimal w's are written out to the file

  void Load_Cmls(void);
  //Reads in the values of optimal w's 

  double Score(double *scc);
  //Finds average accuracy by score

  //Data
  long dim;
  long len;//total number of points
  Index *gInd;
  double **scr;
  double *w;
};
}
#endif
