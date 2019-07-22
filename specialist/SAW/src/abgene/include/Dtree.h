#ifndef DTREE_H
#define DTREE_H

#include <iostream>
#include <fstream>
#include <DataObj.h>
#include <Dbinbase.h>
#include <Isgrid.h>
#include <Mang.h>


using namespace std;
namespace iret {

class TreeBase : public Mang{
public:
   TreeBase(Dbinbase *pDb, Postg<char> *pPg);
   TreeBase(const char *namdbn,const char *nampsg);
   TreeBase(Docum &Doc);
   TreeBase(const char *namdoc);
   ~TreeBase(void);
   void init(long n);//after gopen_Postg() 
   long get_max_alpha(double cut);
   long get_max_delta(double cut);
   long get_max_gain(double cut);
   long get_min_partition(double cut);
   void countST(Index *ind1,Index *ind2);
   void countDoc(long i);
   void counbDoc(long i);  
   void zerot(void);
   void zeros(void);
   double ptxx; //1/n
  
   double *tx;
   double *sx;
   double nnx;
   double nsx;
   double *dt;
   long ncdoc;
   double lim; //Confidential Limit 

};


class KNode {
public:
   KNode(Index *ind1, Index *ind2);
   ~KNode();
   
   virtual int Split(double cut, int level,TreeBase *pBb)=0;
   virtual void Score(double *sxx, TreeBase *pBx)=0;
   virtual double Prune(void)=0;
   virtual void debug(void)=0;
   Index *snd; //Pointer at good set 
   Index *nnd; //Pointer at bad set
   long split; //term number for splitting
   double alpha; //represents improvement from splitting this node
   double win; //score to left
   double wout; //score to right
   double wstd; //goodness of this node
   double rdiff; //variable for pruning
   double lfcnt; //number of leaves below this node
   double n_st;
   double n_t;
   double nsx;
   double nnx;
   double ptxx; //sum of dt array for everything
   int flio; //0 if left child, 1 if right child  
   int dep; //depth in tree, root at 0   
   

   
};


class KNode_CT : public KNode {
public:
   KNode_CT(Index *ind1, Index *ind2);
   ~KNode_CT();
   int Split(double cut,int level, TreeBase *pBb);
   void Score(double *sxx, TreeBase *pBx);
   double Prune(void);
   void debug(void);
   KNode_CT *up;
   KNode_CT *bin;
   KNode_CT *bout;
};

class KNode_Cart : public KNode {
public:
   KNode_Cart(Index *ind1, Index *ind2);
   ~KNode_Cart();
   
    int Split(double cut,int level, TreeBase *pBb);
   void Score(double *sxx, TreeBase *pBx);
   double Prune(void);
   void debug(void);
   KNode_Cart *up;
   KNode_Cart *bin;
   KNode_Cart *bout;
};

class KNode_C45 : public KNode {
public:
   KNode_C45(Index *ind1, Index *ind2);
   ~KNode_C45();
   int Split(double cut,int level, TreeBase *pBb);
   void Score(double *sxx, TreeBase *pBx);
   double Prune(void);
   void debug(void);
   KNode_C45 *up;
   KNode_C45 *bin;
   KNode_C45 *bout;
};

class KNode_Boost : public KNode {
public:
   KNode_Boost(Index *ind1, Index *ind2);
   ~KNode_Boost();
   int Split(double cut,int level, TreeBase *pBb);
   void Score(double *sxx, TreeBase *pBx);
   double Prune(void);
   void debug(void);
   KNode_Boost *up;
   KNode_Boost *bin;
   KNode_Boost *bout;
};


template <class KNodx>
class Dtree : public FBase {
public:
   Dtree(long nd, TreeBase *pBx);
   Dtree(long nd, TreeBase *pBx, int flag);
   ~Dtree();
   int Build_Tree(double cut, int level,Index *sub, Index *nub);
   void Dest_Tree(void);
   void Score_Tree(Index *test);
   double Prune_Tree(void); //return (alpha_min+delta)
   int Prune_Tree(double level);
   void gopen_write(const char *nam);
   void write_Tree(void); //write tree to a file
   void write_Tree_Binary(void);
   void gclose_write(void);
   void Convert_AllScore_Boost(Isgrid *pIsg, double eps);
   void Convert_LeafScore_Boost(Isgrid *pIsg, double eps);
   long nnode;
   long ndoc;
   TreeBase *pBb;
   KNodx *root;
   double *sco;
   int pflag;
   int ntree; //Counts number of trees written out.
   ofstream *pfout; //Points to file being written.
};


template <class KNodx>
class Param {
public:
   Param(KNodx *pKn);
   ~Param();
   void Set_alpha(void);
   void Set_delta(void);
   void Set_gain(double lim);
   void Set_boost();
   double n_st;
   double n_t;
   double nnx;
   double nsx;
   long ndoc;
   double win;
   double wout;
   double alpha;
   double wstd;
   long ncdoc;  
   double ptxx;   

};


inline double Log2(double x){
    return(log(x)/log(2.0));
}    
}
#endif
