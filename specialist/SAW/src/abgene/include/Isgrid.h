#ifndef ISGRID_H
#define ISGRID_H

#include <iostream>
#include <fstream>
#include <FBase.h>
using namespace std;

namespace {
  long *xc,*yc,*zc;
}

namespace iret {

class Isgrid : public FBase{
public:
  Isgrid(void);
  Isgrid(const char *nam);
  Isgrid(const char *nam,double xeps);
  ~Isgrid();

  void set_xdom(double b,double e); //Sets the domain for x, xb=b,xe=e.
  void set_ydom(double b,double e); //Sets the domain for y, yb=b,ye=e.
  void set_zdom(double b,double e); //Sets the domain for z, zb=b,ze=e.
  void set_xgran(long n); //Sets the number of divisions to make in x domain.
  void set_ygran(long n); //Sets the number of divisions to make in y domain.
  void set_zgran(long n); //Sets the number of divisions to make in z domain.
  void init1(); //Initializes the 1D version.
  void init2(); //Initializes the 2D version.
  void init3(); //Initializes the 3D version.

     //To use after call to init1(), init2(), or init3()
  void add_data(double x,double sd, double sn);
  void add_data(double x,double y,double sd, double sn);
  void add_data(double x,double y,double z,double sd, double sn);
     //x is x-coordinate, y is y-coordinate, z is z-coordinate
     //sd is weight, sn is weight*value.

  void dim1(); //Perform 1D regression.
  void dim2(); //Perform 2D regression.
  void dim3(); //Perform 3D regression.
 
  double avg(); //Find average over all data.
  double info(); //Find information in p.
     //Note this function only applicable if p is probability
  double sigma(); //Finds the standard deviation of true.
  double rms(); //Finds the rms deviation of true from p.

  //1D verion of functions.
  void write_1df(void); //Used to write out a 1-dim function.
  void read_1df(void);  //Used to read in a 1-dim function.
  double val_1df(double x); //Returns the approximation give by function.
  void extend_1df(void); //Extends the function to all values by averaging

  //2D version of functions.
  void write_2df(void); //Used to write out a 2-dim function.
  void read_2df(void);  //Used to read in a 2-dim function.
  double val_2df(double x,double y); //Returns the approximation give by function.
  void extend_2df(void); //Extends the function to all values by averaging
     //the limits imposed by the data.

  //3D version of functions.
  void write_3df(void); //Used to write out a 3-dim function.
  void read_3df(void);  //Used to read in a 3-dim function.
  double val_3df(double x,double y,double z); //Returns the approximation give by function.
  void extend_3df(void); //Extends the function to all values by averaging
     //the limits imposed by the data.
  void write_3df_data(void); //Used to write out a 3-dim dataset to be processed elsewhere.
  void read_3df_data(void);  //Used to read in the results of outside processing.
     //Must have set the parameters xsize, ysize, and zsize.

  double *p;
     //p is order constrained probability

  long ct; //Count of points added to system data.

//private:
  void split(long mm,long *idx);
     //Perform the splitting into improved upper 
     //and lower sets. For 2D problems.

  long num; //Number of points in grid.
  long nums; //Used in 3D case.
  double eps; //Positive sum size limit.

  double xb; //Beginning of x domain.
  double xe; //End of x domain.
  double xdel; //size of constant region.

  double yb; //Beginning of y domain.
  double ye; //End of y domain.
  double ydel; //size of constant region.

  double zb; //Beginning of z domain.
  double ze; //End of z domain.
  double zdel; //size of constant region.

  long xsize; //Number of values in x direction.
  long ysize; //Number of values in y direction.
  long zsize; //Number of values in z direction.

  double *xd; //Weight for grid points
  double *xn; //Weight*value for grid points
  double *a;  //a value for grid points
  double *s; //best sum value for grid points 
  long *b; //Previous row pointer to obtain best sum
  double *sr; //best sum value for grid points 
  long *br; //Previous row pointer to obtain best sum
  long *rt; //Marks right hand end of each row.

  //3D parameters
  double *xdc;
  double *xnc;
  double *temp;
  long *nab;
  long total_objs;
};

int ycmp(const long &su, const long &tu);

}
#endif
