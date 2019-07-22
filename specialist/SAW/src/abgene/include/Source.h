#ifndef SOURCE
#define SOURCE

#include <string>
#include <iostream>

using namespace std;

/*
 * A phrase source. Describe a source of phrases in the DB.  Currently
 * created by the DB access class, PhraseDB.  
 */
class Source {
 public:
  /**
     Create a Source description object.  Usually called with data
     returned from the DB.
     @param name Name of the source.  It is convenient if the first
       few characters are unique.
     @param id DB internal id number.  Convenient when working with
       the phrase table.
     @param rank Rough idea of the quality of the phrases.  Range 1 to
       10.  Human judged is 10.
  */
  Source( string name, int id, int rank);

  /* Prints information about the source to out stream..
   */
  void print(ostream& out);

 private:

  // name of source
  string myName;

  // source id code
  int myId;

  // ranks the quality of the source
  int myRank;

};

ostream& operator<<( ostream& out, Source s);

#endif // SOURCE
