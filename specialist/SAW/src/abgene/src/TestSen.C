#include <cstdio>
#include <cstring>
#include <iostream>
#include <fstream>
#include <Btree.h>
#include <Word.h>
#include <runn.h>
#include <Thes.h>
#include <vector>
#include <Brill.h>
#include <string>
#include <Hash.h>
#include <Blist.h>
#include "LinClass.h"
#include "Post.h"
#include "Regist.h"
#include "Nabor.h"
#include "BnBayes.h"


using namespace std;                                                        
using namespace iret;

main(int argc, char** argv) {
  ifstream ifs;
  ofstream ffout;
  int i,j,k;
  long ct=0,mm;
  char bnam[1000000],chp[max_str], anam[1000000], s[max_str];
  char *ptr, *cptr, *aptr, *optr;
  float sss;
  Word Wrd(max_str,"gene");                 
  Wrd.byte_lim=64;
  Wrd.back_lim=48;
  Wrd.set_map(".,:;!?",'\024',LOWERCASE);
  Wrd.restore_map("\'");
  List* lsptr;
  Brill b("gene");
  ifs.open(argv[1], ios::in);
  char* id = argv[1];
  b.open_tagger();
  fprintf(stderr, "OPENED TAGGER\n");
  b.read_and_tok(ifs);
    fprintf(stderr,"READ AND TOKENIZED\n");    
    b.start_state_tagger();
    b.final_state_tagger();
  ifs.close();
}












