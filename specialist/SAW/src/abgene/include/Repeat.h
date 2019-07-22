#ifndef REPEAT_H
#define REPEAT_H

#include <Word.h>

namespace iret {

  class counted_string;
  template <class T>
    class counted_queue;


class Repeat : public Word {
public:
   Repeat();
   Repeat(long word_space);
   Repeat(long word_space, const char * list_name);
   
   ~Repeat();

   void add_multip(long len,char *str,long n = 1); //Adds all phrases
      //found and includes all start points in each phrase

   void process2(); //Processes the input data to find duplicate strings
      //Includes all strings entered by add_simple.
   // May be more efficient than process for the N=2 case

   void process( int N = 2); //Processes the input data to find strings
   // that appear at least N times

   //Data
   Count *pct;			// phrases and their weights
   List  *lst;			// phrases that appear multiple times

 private:
  void init(void);		// most of constructor
   counted_queue<counted_string> *my_queue;
};

} // namespace iret

#endif // REPEAT_H
