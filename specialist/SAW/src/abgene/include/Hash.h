#ifndef HASH_H
#define HASH_H

#include <iostream>
#include <fstream>
#include <Btree.h>
#include <Thes.h>
#include <FBase.h>

namespace iret {

class Hash : public FBase {
public:
  Hash(void);
  Hash(const char *nm);
  ~Hash();

  void create_htable(List &Lst,int excess); //"str" for file of strings, 
      //"ad" for address file, "nm" numbers, 
      //"ha" hash array. Excess is # powers of 2 above size.

  void gopen_htable_map(void); //Creates memory maps
  int find(const char *str); //Return number+1 if present, else 0.
      //Number is not lexical order but hash order and then lexical
      //within collesion groups.
  void gclose_htable_map(void); //Removes memory maps.

  //Data
  char *strmap; //Holds the bit map.
  int *addr; //Holds the offsets to strmap.
  long nwrds; //Number of words.
  long tnum; //Truncation number, size of har.
  int *harr; //Holds hash array.
  int *farr; //Holds the hash coefficients.
  int *px0;
  int *px1;
  int *px2;
  int *px3;
  int *px4;
  int *px5;
  int *px6;
  int *px7;
  int *px8;
  int *px9;
  int *px10;
  int *px11;
};

class Chash : public Hash {
public:
  Chash(void);
  Chash(const char *nm);
  ~Chash(void);

  void create_ctable(Count &Ct,int excess); //Adds "ct" for counts
     //Calls create_htable and then prodoces the array of counts.
 
  void gopen_ctable_map(void); //Calls gopen_htable_map and also
     //maps "ct" file.
  void gclose_ctable_map(void); //Removes memory maps
  long count(const char *str); //Returns count if present, else 0.

  //Data
  int *cnt;
};

class RelateA : public FBase {
public:
  RelateA(void);
  RelateA(const char *nm);
  ~RelateA();

  int create_bmatrix(BTList &Btl,int exc1,int exc2);
      //BTList must have unique lists (add_unique).
      //exc1 and exc2 are to be between 1 & 3, higher the faster
      //as long as size is not a problem.

  void gopen_bmatrix(void); //maps the data from hash
      //and "arr" files.
  void gclose_bmatrix(void); //removes the memory maps
  void set_wrd(long n); //n is word number and sets wrd
      //pointer
  int exs_tag(long m); //m is tag number. 1 if present,
      //0 if absent.
  int exs_pair(long n,long m); //m is tag number. 1 if present,
      //0 if absent.
  int exs_pair(const char *str1,const char *str2); //1 if present, else 0.

  //Data
  int *map; //Holds the map array.
  int *ad;  //Holds offsets into map.
  long nw; //Current string number.
  int ad1; //Lower address in map.
  int ad2; //Upper address in map.
  Hash Coord1; //Holds the first coordinate strings.
  Hash Coord2; //Holds the second coordinate strings.
  long nwrd1; //Number of first coordinate strings.
  long nwrd2; //Number of second coordinate strings.
};

class RelateB : public FBase {
public:
  RelateB(void);
  RelateB(const char *nm);
  ~RelateB();

  int create_bmatrix(BTList &Btl,int exc1,int exc2);
      //BTList must have unique lists (add_unique).
      //exc1 and exc2 are to be between 1 & 3, higher the faster
      //as long as size is not a problem.

  void gopen_bmatrix(void); //maps the data from hash
      //and "bit" files.
  void gclose_bmatrix(void); //removes memory maps
  void set_wrd(long n); //n is word number and sets wrd
      //pointer
  int exs_tag(long m); //m is tag number. 1 if present,
      //0 if absent.
  int exs_pair(long n,long m); //m is tag number. 1 if present,
      //0 if absent.
  int exs_pair(const char *str1,const char *str2); //1 if present, else 0.

  //Data
  unsigned char *map; //Holds the bit map.
  unsigned char *wrd;  //Points at map for a given word.
  Hash Coord1; //Holds the first coordinate strings.
  Hash Coord2; //Holds the second coordinate strings.
  long nwrd1; //Number of first coordinate strings.
  long nwrd2; //Number of second coordinate strings.
  long rd; //Reduction of second coordinate to bits needs rd char.
};

class Lexicon : public FBase {
public:
  Lexicon(const char *nm);
  ~Lexicon();

  int create_bmatrix(const char *path,int exc1,int exc2);
      //path is the full path and name to lexicon. Data
      //must consist of one word per line and categories
      //on the same line following word and demarcated by spaces.
      //exc1 and exc2 are to be between 1 & 3, higher the faster
      //as long as size is not a problem.

  void gopen_bmatrix(void); //Loads in the data from "s1",
      //"s2" and "bit" files.
  void gclose_bmatrix(void); //removes memory maps
  void set_wrd(long n); //n is word number and sets wrd
      //pointer
  int exs_tag(long m); //m is tag number. 1 if present,
      //0 if absent.
  int exs_pair(long n,long m); //m is tag number. 1 if present,
      //0 if absent.

  //Data
  unsigned char *map; //Holds the bit map.
  unsigned char *wrd;  //Points at map for a given word.
  Hash Bterms; //Holds the words.
  Hash Btags; //Holds the tags.
  long nwrds; //Number of words.
  long ntags; //Number of tags.
  long rd; //Reduction of ntags to bits needs rd char.
  int *ptag; //Array of nwrds most probable tag numbers.
};

}
#endif
