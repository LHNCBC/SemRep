#ifndef PHRASE_TABLE
#define PHRASE_TABLE

#include "PhraseDB.h"
#include "Btree.h"

namespace iret {

/** A temporary table for phrase procesing. The class allows the
    possibility of multiple temporary table.
*/
class PhraseTable {
public:
  /** Create a temporary table of phrases.
      @param name Identifier for the temporary table.  Should begin
      with a pound sign (#).
  */
  PhraseTable(PhraseDB& db, string name);

  /** The Sybase table is not explicitly dropped, because temporary
      tables are dropped when the session ends.
      <p>
      Should I allow the table to be explicitly dropped?
  */      
  ~PhraseTable() {delete [] my_buffer;}

  /** Add the elements of list to the table.

      @param list Btree List that contains the phrases to place in the
      temporary table.
   */
  void add(List* list);

  /** Add the elements of the array to the table.
      
      @param array Array of phrases
      @param n Number of phrases in the array
   */
  void add(char* array[], int n);

  /** Add the string to the table.
      
      @param phrase Phrase to add to the table
   */
  void add(string phrase);

  /**
     Done adding phrases to the table.

     @param batch Set true if you only want to say you are done with
     one batch of phrases.  You still need to call done with batch
     false (the default) to finish loading the table.
  */
  void done( bool batch = false);
    
  /* Which phrases exist in the DB.
     @return An integer array that contains the array indices of the
     elements of the table that do exist.
  */
  vector<int>* exists();

private:
  PhraseDB & my_db;
  string my_name;

  // Describe bulk data transfer
  CS_BLKDESC* my_blk_pointer;

  // Pass phrases to the DB
  char* my_buffer;
  //length of data in buffer
  CS_INT my_datalen;
  // Size of my_buffer; maximum length of phrase
  int my_max_length;
};

} // namespace iret 

#endif PHRASE_TABLE
