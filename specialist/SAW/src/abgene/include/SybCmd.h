#ifndef SYBCMD
#define SYBCMD

/**
 ** Try to create a more convenient C++ interface for obtaining the
 ** results of SQL queries.
 **
 ** Needed constants are in: extern "C" {#include <ctpublic.h>}
 **
 ** Overview:

 First, the constructor sets the connection with the DB.  Then call
 prepare to setup the command.  Call bind for each of the output
 columns.  Loop over fetch for each row.  Finally, calling done cleans
 house.

 **
 */

#include "PhraseDB.h"
#include <string>

extern "C" {
#include <ctpublic.h>
}

using namespace std;
using namespace iret;

class SybCmd {

public:
  // Just stores the DB reference
  SybCmd(PhraseDB& db) : myDB(db),
    results_ret(0)
    {}

  // Calls done, in case the user forgot.
  ~SybCmd() { done(); }

  /*
  * Prepare to issue the SQL command.
  *
  * This also works for executing a single SQL command without
  * fetchable results.  Nonetheless, it might be better to have an
  * execute method.
  *
  * No variables, position holders, or any other features of Dynamic
  * SQL.  Coming ...
  */
  void
  prepare( string command );

  /*
  ** Finished with command.
  */
  void
  done ();

  /*
  ** Connect the return columns to locations to store those
  ** values. 
  ** 
  ** num: which return column (1st, 2nd, etc)
  ** type: type of value
  ** format: how to return value (undeeded for binary numbers)
  ** length: how much data to return (undeeded for binary numbers)
  ** address: where to put the value
  */
  void
  bind( CS_INT num, CS_INT type, CS_INT format, CS_INT length,
	CS_VOID* address );

  /*
  ** Return the next row.  Return value tells you whether row is
  ** valid.
  **
  ** CS_ROW_FAIL: recoverable error.  This row is bad, but future ones
  **              may be OK
  ** CS_SUCCEED:  data is good.  Process it.
  ** CS_END_DATA: proper end of data
  */
  CS_RETCODE
  fetch ();

private:
  PhraseDB& myDB;
  CS_COMMAND *cmd;		/* Command structure.    */

  /* Results from most recent ct_results call */
  CS_RETCODE results_ret;

};

#endif SYBCMD
