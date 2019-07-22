#ifndef KTREE_H
#define KTREE_H

#include <iostream>
#include <fstream>
#include <Postg.h>
#include <Mang.h>

using namespace std;
namespace iret {

class Ktree {
public:
  Ktree(void);
  ~Ktree();
  long read_Ktree(ifstream &fin);
  long read_Ktree_Binary(ifstream &fin);
  double AllScore(Mang &Dbn, long n);
  double LeafScore(Mang &Dbn, long n);
  //Data
  long nnode;
  long *split; //store number of splitting feature
  double *win; //store weights for the left children
  double *wout; //store weights for the right children
  long *pr; //parent of node
  long *lc; //left child of node
  long *rc; //right child of node
  
};

class Ktree_set : public FBase{
public:
   Ktree_set(const char *nam);
   ~Ktree_set(void);
   void Read(void);
   void Read_Binary(void);
   double AllScore(Mang &Dbn, long n);
   double LeafScore(Mang &Dbn, long n);
   
   //Data
   Ktree *Kt; //Pointer at array of Ktrees
   int ntree; //Number of trees in array
};

class Stree {
public:

  Stree();
  ~Stree();
  void Convert(Ktree &Kt,Postg<char> *pPosg);

  long read_Stree_Binary(ifstream &fin);
  double AllScore(const char *str);
  double LeafScore(const char *str);
  long write_Stree_Binary(ofstream &fout);
  //Data

  long nnode;
  long *spos; //store number of splitting position in the input string
  char *schr;
  double *win; //store weights for the left children
  double *wout; //store weights for the right children
  long *lc; //left child of node
  long *rc; //right child of node


};

class Stree_set : public FBase{
public:
   Stree_set(const char *nam);
   Stree_set(const char *nam,Ktree_set *pKst,Postg<char> *pPosg);
   ~Stree_set(void);
   void Read_Binary(void);
   void Write_Binary(void);
   double AllScore(char *str);
   double LeafScore(char *str);

   //Data
   Stree *St; //Pointer at array of Ktrees
   int ntree; //Number of trees in array
};

}
#endif
