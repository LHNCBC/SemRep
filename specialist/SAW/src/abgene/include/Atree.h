#ifndef ATREE_H
#define ATREE_H

#include <iostream>
#include <fstream>
#include <DataObj.h>
#include <Dbinbase.h>
#include <Isgrid.h>
#include <Mang.h>


using namespace std;
namespace iret {

class ATreeBase : public Mang{
public:
   ATreeBase(Dbinbase *pDb, Postg<char> *pPg);
   ATreeBase(const char *namdbn,const char *nampsg);
   ATreeBase(Docum &Doc);
   ATreeBase(const char *namdoc);
   ~ATreeBase(void);
   void init(void);//after gopen_Postg() 
   long get_min_partition(double rem);
   void countST(Index *ind1,Index *ind2);
   void update_dt(Index *ind, double a);
   void norm(void);
   void countDoc(long i);
   void counbDoc(long i);  
   void zerot(void);
   void zeros(void);
   double *tx;
   double *sx;
   double nnx;
   double nsx;
   double *dt;
   double win;
   double wout;
   double err;
   long ncdoc;
   long split;
    

};


class Aset{
public:
   Aset(void);
   ~Aset();
   double erf(ATreeBase *pBb);
   long node;
   Index *ind;
   Index *snd;
   Index *nnd;
   Aset *up;
   long split; //term number for splitting on the mother node
   long csplit;//term number for splitting on the current node
   double weg;
   double win;
   double wout;
   int flio; //1 if left child, -1 if right child  
   int depth; //depth in tree, root at 0   
   

   
};

class Atree{
public:
   Atree(long nd, ATreeBase *pBx);
   Atree(long nd, ATreeBase *pBx,int flag);
   ~Atree();
   void Dest_ATree(void);
   void Score_ATree(Index *test);
   long node;
   long ndoc;
   Aset *aset;
   ATreeBase *pBb;
   double *sco;
   double min_err;
   double zprod;//error bound per doc
   int pflag;
   
   void BuildInit(Index *subb, Index *nubb);
   long Build_ATree(void);//build the atree continuously
   //must allocate aset enough and call BuildInit() before
   void Build_ATree(Index *sub, Index *nub, long lim);
   //same as above && the number of aset is 2*lim+1
  

   
};


inline double Log2(double x){
    return(log(x)/log(2.0));
}    
}
#endif
