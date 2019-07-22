#ifndef PATT_H
#define PATT_H

#include <iostream>
#include <fstream>
using namespace std;
namespace iret {

typedef struct chain {
     char ca;
     chain *fw;
     chain *bw;
} Chain;

class Anode {
   public:
     Anode(const char *key,int value); 
        //Input is key string (generally the end of a pattern) and value.
     ~Anode();

     char *sc; //key string pointer
     int  pc;  //length of key string
     int  value; //Value of pattern ending here or zero if none does.
     Anode **pa; //Points at next node.
}; 

class Patt {
   public:
     Patt(int max_patt_len); 
        //Must provide the maximal pattern length that the class will hold.
     ~Patt();

     void add_patt(const char *pat,int value); 
        //Adds the null terminated character string to the list of
        //strings that will be detected and associates value with that pattern.
     void make_chain(); //Makes the chain for cycling. Needed after adding patterns.
     void reset(); //Resets the cycle to null, ready to start a new search.
     int  trigger(char &c); 
        //This function will analyze a stream of characters to detect a pattern and
        //return the value of that pattern that is detected.
     int  capture(long &i,char *txt,char &c); 
        //On repeated calls this function analyzes a stream of 
        //characters to detect a pattern and returns the value associated with 
        //that pattern and leaves all the characters since the last reset in the 
        //buffer txt with the value txt+i-1 pointing at the last character received 
        //before the pattern detected. Thus i is the length of text in the buffer.

   //private:
     int lng_pat; //Length of the longest pattern.
     int sht_pat; //Length of the shortest pattern.
     int max_pat; //Length of the longest pattern allowed for this object.
     int pass;  //Keeps count of number of characters yet to pass before a 
        //pattern is again possible.
     Chain *pchain; //Points at the cycle of chain elements.
     int **shift; //The shift array provides neccessary shifts at each possible 
        //depth based on the Boyer-Moore algorithm. Allows the setting of pass.
     int *pshift; //Special first step array the notifies of an initial match or allows 
          //reseting of pass.
     Anode *root; //Root of the search tree for patterns.
};
}
#endif
