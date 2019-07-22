#ifndef LUDECOMP_H
#define LUDECOMP_H

class LUDecomp {
   public:
     LUDecomp(long n,double **dat); //Performs the decomposition.
     ~LUDecomp();
     void solve(double *b,double *x); //Solves for a vector of constants, 
          //Ax=b.
          //Only to be run after function decomp and the n's must agree.
     void debug(void); //Prints to cout for help in debugging.
     void value(double **dat,double *x,double *y); //Finds y in Ax=y
     double det(void); //Value of the determinant of **dat given on input.
     long num; //Dimension of square matrix.
     double **mat; //Pointer at matrix to be decomposed. Separate memory is
          //set aside here so that dat is not destroyed.
     long *ord; //Holds the permutation produced in pivoting.
     long par; //Parity of permutation in ord.
};

#endif
     
     
