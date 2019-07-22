#ifndef CUT_H
#define CUT_H
#include <iostream>
#include <fstream>
#include <Docum.h>
#include <runn.h>
#include <FBase.h>
#include <Post.h>
#include <Hyper.h>
using namespace std;
namespace iret {

class Cut {
   public:
      Cut(const char *nam_slice);
      Cut(void); 
     ~Cut();
      void set_get_M(long *frq);
      double get_M(long n_s);
      void create_cut_array(long ns);
      void gopen_cut_map(void);
      long signif(long n_st, long n_s, long n_t, long N);
 
      long ndoc;
      long nwrd;
      long *freq;
      long f_max;
      long snum;
      long *mm;
      long *rev;
      long  non_zero;
      double *Txx;//map  log(M)
      Hyper *hyper;
};

}
#endif
