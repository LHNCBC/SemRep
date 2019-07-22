#ifndef BTREE_H
#define BTREE_H

#define LNEG -100000000

#include <iostream>
#include <fstream>
using namespace std;
namespace iret {

const int order = 5; //Half the order of the Btree that we build.
const int height_limit =12; //Limit on the height of the Btree.
const int ord2  = order*2; //The order of the Btree.

int stc_my(int &,int &,const char *,const char *); //Function used to compare 
   //two strings. The first two arguments hold information about how much the 
   //string can be ignored in the comparison.

class Page; //forward declaration
class Btree; //forward declaration
class Partial_match; //forward declaration

class Node {
   friend int stc_my(int &,int &,const char *,const char *);
   friend class Page;
   friend class Btree;
   friend class List;
   friend class Partial_match;
   friend class Thes;
public:
   Node(void); //Sets all points to NULL.
   Node(const char * ); //Argument is the string for this node.
   Node(const char * ,void *); //Arguments are first the string and then the 
   //data pointer.
   ~Node();
   void debug(); //Prints out the node in simple format.
private:
   char *str; //String pointer.
   void *rel; //Data pointer.
   Page *pdn; //Points down to the page below or to NULL.
};

class Page {
   friend int stc_my(int &,int &,const char *,const char *);
   friend class Btree;
   friend class Partial_match;
public:
   Page(); //Constructs a new empty page. Only happens at the root.
   Page(Page * const pz,Page * const pn,const int n); //Constructs a page that 
      //holds the right half of a full page. The full page is pointed at by the 
      //pz. The new pages downward pointer is set to pn. 
      //n tells how much of the full page is to remain or where to begin removal.
   ~Page();
   void insert(const int n,Node * const nd,const int j); //inserts in partially empty 
      //page. n is insertion point, j is number of nodes on page that are viable.
   int search(int &a,int &b,const char *,int &p); //searches for string on 
      //the page. Returns 1 if found, 0 otherwise. If found p is the index, otherwise
      //if p is 0 then the page downward pointer is to next page to search, but if
      //p is positive then p-1 is number of node that has the downward pointer to 
      //next page to search. 
   int search(int &a,int &b,char *,int &p,Partial_match *btr); //Looks for longest 
      //partial match. 
   void debug(); //Prints out the page for debugging purposes.

private:
   char ndnm; //Indicates the number of Nodes on the page.
   Page *pdn; //Pointer that points to the page below and also lexically below. 
   //May be NULL.
   Node *pnd[ord2]; //Pointers to the nodes on the page. Some may be NULL.
};

class Btree {
   friend class Page;
public:
   Btree();
   Btree(ifstream &); //Reads in a Btree in form of list written out by 
          //list_write() from disc. String arguments mark the path in proj file.
   Btree( const Btree & btree ) {copy = true; root = btree.root;} // Actually
   // creates another reference to the same tree.  Take great care to
   // avoid simulaneously modifying both copies.
   ~Btree();
   int search(const char *); //Searches for a string and sets the path to that 
                    //string or its insertion point.
   int insert(Node *);//Only to be called after a search has failed to find the 
                       //string.
   void node_first();//Finds the first node in the tree and sets the path to it.
   int node_next(); //Given the path is already set to a node, this function 
                    //finds the next node in lexicographic order.
   char *show_str();//Used to show the string after a call to next is successful.
   void *give_ptr();//Used to give the data pointer in the current node.
   void set_ptr(void *); //Used to set the data pointer after a call to search 
                         //has found string.
   int add(Node *); //Only to be used to construct a tree from a lexical list 
                    //as written out by list_write();
   void next_empty(); //Only used to reset the pointer arrays when the root is 
                      //split. Used in add().
   long list_write(ofstream &); //Writes out a lexical list of the strings in 
                                //the tree. 
protected:
   int depth; //Tells the depth in the tree that marks the current location.
   Page *root; //Points at the root page of the tree.
   Page *pg[height_limit]; //Descending list of pointers that mark the pages.
   int cnd[height_limit]; //Mark the positions of the nodes just above the 
       //downard page pointer at each level. Thus 0 marks the page's downward 
       //pointer, but a nonzero value must have 1 subtracted and then it gives 
       //the node whose downward pointer is the correct downward pointer.
   bool copy; //flags copies of a tree with true.
};

class List : public Btree {
public:
   List();
   List(const List & list) : Btree(list) {}
   ~List();
   void add_key(const char *str); //Adds the string *str to the tree if not already in list
   void add_key_count(const char *str); //Adds the string *str to the tree if not already in list and counts it.
   long cnt_key; //Used to count the number of keys.
};

class Count : public List {
public:
   Count();
   Count(const Count & Ct) : List(Ct){} 
   ~Count();
   void add_count(const char *str,long n); //Adds the string *str with its count
      //to the tree if not already in list. String is key and count is data.
      //If string is already a key the count is incremented by n.
   void add_count2(const char *str,long n); //Adds the string *str with its count
      //just as add_count, but also counts number of unique keys in count.
   long count(const char *str); //Returns the count if a key (in list) otherwise
      //returns 0.
   long count(void); //Returns the count of the current string. Assumes the
      //pointers have already been set by a search or node_next call.
   long total; //Holds the total of all counts added for all keys.
};

class Partial_match : public Count {
   friend class Page;
public:
   Partial_match();
   Partial_match(const Partial_match & Par_mat) : Count(Par_mat){}
   ~Partial_match();
   void long_match(char *,List &); //Finds the longest matches for all word
       //starts in the string and adds them to the list.
   void local_match(char *,List &); //Finds all matches that start at 
       //beginning of the string and adds them to the list.
   void all_match(char *,List &); //Finds all matches within the string and
       //adds them to the list.
   void long_match(char *,Count &,long n); //Finds the longest matches for all word
       //starts in the string and adds them to the list in Count.
   void local_match(char *,Count &,long n); //Finds all matches that start at
       //beginning of string and adds them to the list in Count.
   void all_match(char *,Count &,long n); //Finds all matches within the string and
       //adds them to the list in Count.
   int search_long(char *); //Searches for longest partial match to an initial
       //segment of a string that ends at a word boundary and 
       //sets the path to that string or its insertion point.

private:
   int stc_my_long(int &,int &,char *,const char *,int); //Function used to compare 
      //two strings. The first two arguments hold information about how much the 
      //string can be ignored in the comparison. The last argument holds the index
      //or number of the string's node on the page.
   int step_one(int &,int &,char *); //Looks for partial or complete match and
      //returns 1 if complete found. Partial is reflected in parameters.

    //Special parameters used in partial matching.
   int depth_o; //Depth of longest partial match thus far.
   int index_o; //index of longest partial match thus far.
   int cln_o;  //String length of longest partial match thus far.
   int len; //Length of query string. 
   int cln; //Current null position in string.
};

class Str_str : public Btree {
public:
   Str_str();
   Str_str(const Str_str & Stst) : Btree(Stst){}
   ~Str_str();
   void add_pair(const char *one,const char *two); //Adds the string *one to the tree and stores
      //the string *two at that node.
   char *match(const char *one); //Returns pointer to the string stored under string *one.
};

class Num_num : public Btree {
public:
   Num_num();
   Num_num(const Num_num & Nmnm) : Btree(Nmnm){}
   ~Num_num();
   void add_pair(long i, long j); //Adds the string for i to the tree and
      //stores the  number j at that node.
   long match(long i); //Returns the number stored under the string for i.
};

}
#endif
