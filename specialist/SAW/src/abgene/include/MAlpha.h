#ifndef MALPHA_H
#define MALPHA_H

#include <iostream>
#include <fstream>
#include <BnBayes.h>
#include <Docum.h>
#include <Blist.h>
#include <Regist.h>
#include <Hyper.h>
using namespace std;
namespace iret {


class MAlpha_pth : public BnBayes_pth{
   public:
      MAlpha_pth();  
      ~MAlpha_pth();
      void gopen_MAlpha_map(Regist_pth *pReg,Slice_Accpth *pSac);
      double scoreTerm(long n, long *idx); //Score the indexed set only.
      void weightApos(double cut);
      void weight_pth(double cut);

};

}

#endif
