#ifndef MAHAL_H
#define MAHAL_H

#include <iostream>
#include <fstream>
#include <FBase.h>
#include <LUD.h>
using namespace std;
namespace iret {

class Mahal : public FBase {
public:
  Mahal(long dm, long ln, Index *gind, const char *nam);
   //dim is dimention of the data, i.e. how many score arrays are to be 
   //combined.
  //len is the length of score arrays
  //gInd is the pointer to the good elements
  
  Mahal(long dm, const char *nam);
  //simple constructor
 
  ~Mahal();
  
  void Setup(long i, double *sco);
  //Sets up scr through dim calls
  
  void Learn_Mahal(void);
  //Does the learning procedures and writes stuff to the output file

  void Load_Mahal(void);
  //Loads the data from the *.mat files to use them for testing
   
  double Score(double *scc);
  //Predicts the class by a rating formula involving Mahal. distance,
  // determinants, and probabilities.   

  //Data
  long dim;
  long len;
  Index *gInd;
  double **scr;
  LUDecomp *pLud_g;
  LUDecomp *pLud_b;
  double det_g;
  double det_b;
  double pri_g;
  double pri_b;
  double *mean_g;
  double *mean_b;
  double coef;
  double **cov_g, **cov_b;
};
}
#endif
