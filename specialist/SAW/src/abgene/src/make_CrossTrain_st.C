#include <cstdlib>
#include <cmath>
#include <cstring>
#include <iostream>
#include <fstream>
#include <cassert>
#include <Btree.h>
#include "runn.h"
#include <DataObj.h>
#include "Strset.h"

using namespace std;
using namespace iret;

main(int argc, char **argv)
{
   long flag,num,i;
   char *text,ch,txt[10000];
   LexPrb Lmed(3,"medt");
   Lmed.gopen_add();
   LexPrb Lgen(3,"gene");
   Lgen.gopen_add();
   Lmed.set_fst('!');
   Lgen.set_fst('!');

   for(i=0;i<Lgen.num;i++){
      Lgen.add_segs(txt);
      mark(1,i,1000,"genes");
   }
   Lgen.gclose_add();
   for(i=0;i<Lmed.num;i++){
      Lmed.add_segs(txt);
      mark(1,i,1000,"terms");
   }
   Lmed.gclose_add();
}
