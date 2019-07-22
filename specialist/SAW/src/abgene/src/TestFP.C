#include <cstdio>
#include <cstring>
#include <iostream>
#include <fstream>
#include <Btree.h>
#include <Word.h>
#include <runn.h>
#include <Thes.h>
#include <vector>
#include <PostBrill_FN.h>
#include <PostBrill_FP.h>
#include <string>
#include <Hash.h>
#include <Blist.h>
#include <pcre.h>

using namespace std;                                                        
using namespace iret;

main(int argc, char** argv) {
  ifstream ifs;
  
  ifs.open(argv[1], ios::in);
 
  char s[max_str];
  syn_lk* cptr;
  PostBrill_FP b("gene");
 
  int ii;
  b.gopen_PostBrill_FP(); 
  fprintf(stderr, "OPENED FILTER\n");
  b.filter_file(ifs);  
  ifs.close();
}












