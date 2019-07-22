#ifndef LGLIN_H
#define LGLIN_H

#include <iostream>
#include <fstream>
using namespace std;
namespace iret {

class Lglin : public FBase {
public:
  Lglin(long dm, long ln, Index *gind, const char *nam);
   //dim is dimention of the data, i.e. how many score arrays are to be 
   //combined.
  //len is the length of score arrays
  //gInd is the pointer to the good elements

  Lglin(long dm, const char *nam);
  //Simple Constructor

  ~Lglin();

  void Setup(long i, double *sco);
  //Sets up scr through dim calls

  void Learn_Lglin(void);
  //Maximizes a function f by generating a sequence of iterates according to 
  // scheme   x(new)=x(old)+eps*p 
  //where p is the search direction and eps is the step length
  //which maximizes function f along direction grad from the point x(old)  
  //calls the Newton function that solves one-dim max problem
  // to find an optimal step
  //p is found by solving the set of linear equations B*p=-df
  //where B is the appr. of Hessian using BFGS update formula
  //The resulting optimal theta's are written out to the file

  void Load_Lglin(void);
  //Reads in the values of optimal theta 

  double Score(double *scc);
  //Predicts the class

  double Newton(double epn);
  //solves one-dim maximisation problem to fins the optimal step size

   //Data
  long dim;
  long len;//total number of points
  long n;//number of goods
  Index *gInd;
  double **scr;
  double *th;
  double *a;
  double *b;
  double c1;
  double c2;
  double sm;
};
}
#endif
