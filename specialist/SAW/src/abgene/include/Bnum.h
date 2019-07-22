#ifndef BNUM_H
#define BNUM_H
#include <fstream>
#include <iostream>
#include <runn.h>
using namespace std;
namespace iret {

class Bnum {
   public:
      Bnum(long nm); 
      Bnum(Bnum &Anm); 
     ~Bnum();
      long index(long qn); //Finds by binary search that i, 0<=i<num,
         //with qn-mm[i]>=0 and minimal.
      long num;
      long *mm;
};
}
#endif
