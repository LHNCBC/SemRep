#ifndef THES_H
#define THES_H
#include "Btree.h"
#include "Word.h"
#include <iostream>
#include <fstream>
using namespace std;
namespace iret {

struct syn_lk{
   char *syn;
   syn_lk *next;
};

// data structure to hold synonym information. 
class Synon {
public:
   Synon();
   ~Synon();
   //Functions assume str is key pointer in Btree
   //and only compare pointers to test string identity
   void add_element(char *str); //Adds str, but only
      //if not already on list.
   void rmv_element(char *str); //Remove str
      //No action if not on list.
   
   long num; //Keeps acurate list size info. 
   char *prf; //Either null or prefered string.
   syn_lk *list;
};

class Thes : public List {
public:
   Thes();
   Thes(const Thes &Ths);
   ~Thes();
   void add_syn(const char *str1, const char *str2); //If str1 not in tree
      //add to tree. Give its own Synon1 if doen't have. Then 
      //if str2 not in tree add it and add it to Synon1. If 
      //str2 already in tree and has its own Synon2, remove 
      //it from Synon2 and add it to Synon1. Delete Synon2 if empty.
   int set_prf(const char *pch); //Set prf of Synon of pch to pch.
      //If pch in tree and has Synon, sets and returns 1, else 0.

   //Functions to access preferred string for a string
   char *prf_str(const char *pch); //If string in tree and has prf, returns 
      //pointer at prefered string, else returns NULL.
   char *prf_str(void); //As prior but
      //assumes that pointers are set by successful
      //search or by a traversal.
   //Functions to assess synonymy of two strings
   int set_Synon(const char *str); //Sets the pointer pSynon to 
      //data for str & returns 1 if string present, else pSynon
      //=NULL and returns 0.
   int synon(const char *str); //Returns 1 if str in tree and
      //its Synon equal to non-NULL pSynon, else returns 0.
   int synon(const char *str1,const char *str2); //Returns 1 if str1 
      //and str2 are synonyms, otherwise returns 0.
      //Uses previous two functions.
   //Functions to access the synonym list for a string
   int set_class(const char *str); //Returns number in class if 
      //str in tree and syn_lk set non-NULL, else returns 0.
   char *next_incl(void); //returns non-NULL pointer
      //until list exhausted. All strings included.
   char *next_excl(void); //returns non-NULL pointer
      //until list exhausted. All strings but one used
      //to set_class included.

   //Data
   Synon *pSynon; //Holds the pointer at a Synon.
   char *sel; //Pointer at selected string in tree.
   syn_lk *pl; //Pointer at list link.
};

class Link_list {
public:
   Link_list();
   ~Link_list();
   //Functions assume str is key pointer in Btree
   //and only compare pointers to test string identity
   void add_element(char *str);
   void add_unique(char *str);
   int list_size(void);
   //Data
   int flag; //Marks if processed or not
   syn_lk *el; //Points at the list
};

class Point {
public:
   Point(); 
   ~Point();
   syn_lk *elem; //variable pointer at linked list element
   syn_lk *elx; //pointer at linked list element el
   syn_lk *strt; //pointer at starter traversal element
};

class BTList : public List {
public:
   BTList();
   BTList(const BTList &Btlt);
   ~BTList();
   void add_element(const char *str1,const char *str2); //Both add functions
   void add_unique(const char *str1,const char *str2);  //keep counts updated.
   int list_size(const char *str); //Counts # elements on str's list
   int list_size(void); //When pointers already set
   syn_lk *set_ptr(const char *str); //If a list exists a non-NULL 
        //pointer is returned
   syn_lk *set_ptr(void); //When pointers already set
   char *next_ptr(void); //At each call a string pointer is returned until 
        //list is exhausted and a null pointer is returned.
   //Overloaded traversal functions
   syn_lk *set_ptr(const char *str,Point &Pt); //If a list exists a non-NULL 
        //pointer is returned
   syn_lk *set_ptr(Point &Pt); //When Btree pointers already set
   char *next_ptr(Point &Pt); //At each call a string pointer is returned until 
        //list is exhausted and a null pointer is returned.
   long exs_pair(const char *qtr,const char *str); //checks if pair exists
   long num_pair(const char *qtr,const char *str); //As previous but counts number of 
        //occurrences.
   long exs_second(const char *str); //Assumes the pointer for first string was
        //found and checks to see if str is on the current list.
   long num_second(const char *str); //As previous but counts number of 
        //occurrences.
   void make_class(ofstream &fout); //Makes the classes that are implied by
        //the relation in the tree of lists and outputs in file fout.
        //Marks concepts with strings beginning with an 'N' followed 
        //by a number in order of creation.
   void make_class(ofstream &fout,char c); //Makes the classes as above. 
        //Removes any string that has the character c in it.
   void debug(void);
 
  void load( const char * file_name, const char delimiter = '|');
  // Load a character delimited file into a BTList
  
   //Data
   syn_lk *elem; //variable pointer at linked list element
   syn_lk *elx; //pointer at linked list element el
   syn_lk *strt; //pointer at starter traversal element
   List *lst; //List tree in which all list elements are stored
};
   
}
#endif
