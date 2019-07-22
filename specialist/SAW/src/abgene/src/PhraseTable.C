#include "PhraseTable.h"
#include "SybCmd.h"
#include <sstream>

extern "C" {
  #include <bkpublic.h>
}

using namespace iret;

PhraseTable::PhraseTable(PhraseDB& db, string name) :
  my_db(db),
  my_name(name)
{
  // create temporary table

  SybCmd createTable(my_db);

  ostringstream sql_cmd;
  sql_cmd << "create table " << my_name
	  << " ( text varchar(255) not null)";
  createTable.prepare(sql_cmd.str());

  // prepare to bulk load table
  CS_RETCODE return_code = 0;
  return_code = blk_alloc(my_db.getConnection(), BLK_VERSION_100,
			  &my_blk_pointer);
  my_db.exit_on_fail(return_code,"blk_alloc failed");

  return_code = blk_init(my_blk_pointer, CS_BLK_IN,
			 (char*)name.data(), name.length() );
  my_db.exit_on_fail(return_code, "blk_init failed");

  CS_DATAFMT datafmt;
  return_code = blk_describe(my_blk_pointer, 1, &datafmt);
  my_db.exit_on_fail(return_code, "blk_describe failed");

  //  cerr << "Max column width: " << datafmt.maxlength << endl;
  my_max_length = datafmt.maxlength;
  datafmt.count = 1;		// maybe try arrays sometime
  my_buffer = new char[datafmt.maxlength+1];

  return_code = blk_bind(my_blk_pointer, 1, &datafmt, my_buffer,
			 &my_datalen, NULL);
  my_db.exit_on_fail(return_code, "blk_bind failed");
  
}

void
PhraseTable::add(string phrase) {
  strncpy(my_buffer,phrase.c_str(), my_max_length);
  my_datalen = phrase.length();

  CS_RETCODE return_code = blk_rowxfer_mult( my_blk_pointer, NULL );
  my_db.exit_on_fail(return_code, "blk_rowxfer_mult failed");
}

/*
  PhraseTable::~PhraseTable() {}
*/
  /** Add the elements of list to the table.

      @param list Btree List that contains the phrases to place in the
      temporary table.
   */
void PhraseTable::add(List* list) {}

  /** Add the elements of the array to the table.
      
      @param array Array of phrases
      @param n Number of phrases in the array
   */
void PhraseTable::add(char* array[], int n) {}

void
PhraseTable::done( bool batch ) {
  if ( batch ) {
    // done with one batch
    CS_RETCODE return_code = blk_done( my_blk_pointer, CS_BLK_BATCH,
				       NULL);
    my_db.exit_on_fail(return_code, "blk_done BATCH failed");
    return;
  }

  // finished loading the table
  CS_RETCODE return_code = blk_done( my_blk_pointer, CS_BLK_ALL,
				     NULL);
  my_db.exit_on_fail(return_code, "blk_done ALL failed");

  return_code = blk_drop(my_blk_pointer);
  my_db.exit_on_fail(return_code, "blk_drop failed");
}
  
  /* Which phrases exist in the DB.
     @return An integer array that contains the array indices of the
     elements of the table that do exist.
  */
vector<int>* PhraseTable::exists() {return new vector<int>;}
