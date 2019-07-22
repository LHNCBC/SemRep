
#ifndef PHRASEDB
#define PHRASEDB

#include <string>
#include <vector>
#include "Source.h"
#include "Btree.h"

extern "C" {
#include <ctpublic.h>
  // #include "example.h"
#include "exutils.h"
}

using namespace std;

namespace iret {

class PhraseTable;

/*
 * A simple interface to the Sybase Phrases DB.
 *
 * Since I don't know what will be useful, everything in this
 * interface is experimental.
 */
class PhraseDB {
public:
  /* Create the connection to the database
   */
  PhraseDB( string server="STRAUSS", string username="anyone",
	    string password="allowed" );

  ~PhraseDB() {  close(); }

  // Return the current Sybase context
  CS_CONTEXT* getContext(void) { return context; }
  // Return the current Sybase connection
  CS_CONNECTION* getConnection(void) { return connection; }

  /* Return a vector of sources.
   */
  vector<Source>
  sources(void);

  /* Return whether or not phrase can be found in the DB.
   */
  bool
  exists(string phrase) {
    return boolSQLcmd( "select id from Phrases..phrase \
                         where text = "+quote(phrase));
  }

  /* Return whether or not the phrase is a good phrase.  Assumes it is
     not a good phrase if it does not exist in the DB.
   */
  bool
  good(string phrase) {
    return boolSQLcmd( "select id from Phrases..phrase \
                         where good = 1 and text = "+quote(phrase));
  }

  /* Return whether or not the phrase is a wellformed phrase.  Assumes
     it is not wellformed if it does not exist in the DB.
   */
  bool
  wellformed(string phrase) {
    return boolSQLcmd( "select id from Phrases..phrase \
		        where wellformed = 1 and text = "+quote(phrase));
  }
  
  /*
   * Properly quote a string.
   * This means single quotes at each end and single quotes in the
   * middle doubled.
   */
  string
  quote (const string text);

  /** Return the list of phrases from the source as a "Btree" list.
   *
   * The caller owns the list, so be sure to delete it when finished.
   *
   * @param source The DB id code for the desired source.
   */
  List*
  list(int source);

  /** Return the list of phrases from the source as a "Btree" list.
   *
   * The caller owns the list, so be sure to delete it when finished.
   *
   * @param source A unique prefix of the name of the source.  Since
   <code>like 'source%'</code> is used to identify the source, if it
   is not unique, multiple sources may be merged. 
   */
  List*
  list(string source);
  
  /*
   * Unless ret is success, exit the program after closing the DB
   * connection.
   */
  void
  exit_on_fail(CS_RETCODE ret, string str);

  /** Create a temporary table for processing a set of phrases.
      Processing the set as a table maybe much more efficient that
      processing each element individually.

      @param tableName Identify the temporary table created.  Please
      use a pound sign (#) as the first character.  This lets Sybase know
      it is a temporary table.
  */
  PhraseTable* tempTable ( string name="#tempTable");
     
    
protected:

  /* Return whether or not the SQL command returned any results.

     Much could be done to make it more flexible.  For example,
     dynamic SQL and prepared statements.
  */
  bool
  boolSQLcmd(string sql_cmd);
    

  /* Close the database connection.  Automaticly called by destructor.
   *
   * For now, errors are handled by return code.  I would like to use
   * exceptions.
   */
  CS_RETCODE close(void);

  /** Return the list of phrases from the source as a "Btree" list.
   * @param sql_cmd Any sql command that returns a single text field.
   */
  List*
  list_prepare(string sql_cmd);


private:

  /* Context structure     */
  CS_CONTEXT *context;
  /* Connection structure. */
  CS_CONNECTION *connection;

};

} // namespace iret

#endif // PHRASEDB
