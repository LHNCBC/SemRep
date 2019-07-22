#ifndef LINCLASS_H
#define LINCLASS_H

#include <iostream>
#include <fstream>
#include <FBase.h>
#include <Hash.h>

using namespace std;
namespace iret {

class LinClass : public FBase{
public:
   LinClass(const char *);
   ~LinClass();
   
   Count *Count_dbin(const char *post_nam,double *wg);
      //post_nam is the name of a postset object
      //wg is the array of weights learned in Dbinbase
   Count *Count_bnby(const char *slice_nam,float *wg);
      //slice_nam is the name of a slice object
      //wg is the array of weights learned in BnBayes 
   Count *Count_cmls(const char *postg_nam,double *wg);
      //postg_nam is the name of a postsetg object
      //wg is the array of weights learned in Cmls 

   void create_Hash(Count *pCt,double *w,int exc);
   void create_Hash(Count *pCt,float *w,int exc); //w an array
      //of weights. Exc is excess over list size, 1,2, or 3.
   //Save threshhold
   void save_Thresh(double th);
   void save_Thresh(float th);

   void gopen_operate(void);
   void gclose_operate(void);
   float weight(const char *str);
  
   //Data
   float thresh; //Threshhold, default 0.
   Hash Lhs;
   float *wt; //Stores weights for terms in Lhs.
};
}
#endif
