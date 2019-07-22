#ifndef SLICE_H
#define SLICE_H

#include <iostream>
#include <fstream>
#include "Post.h"
using namespace std;
namespace iret {

class Slice : public Post {
   public:
      Slice(); 
      Slice(const char *nm);  
          //Stores the string *nm in *name for use in reading or writing.
      ~Slice();

       //Makes sliced p files on disk.
      void s_slice(long list_size); //merge the segs into single n,d,s files.
      void p_slice(void); //merge the segs into single f,a, but 
           //slice the p file to several slices approximately <=disk_size.
	   //includes the file number and the address in the a file.

};

}
#endif
