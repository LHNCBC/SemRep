
/*
** A simple interface to the Sybase Phrases DB.
**
** Since I don't know exactly what will be useful, everything in the
** interface is very experimental.
**
** In particular, many things are too specific.  Since I don't know
** exactly what is needed, I know even less about how things should be
** generalized.  First I'll do the specific.  Then see if I can be more
** flexible. 
**/

#include "PhraseDB.h"
#include "SybCmd.h"
#include "PhraseTable.h"

#include <stddef.h>
#include <iostream>
#include <sstream>

extern "C" {
#include <ctpublic.h>
#include "example.h"
#include "exutils.h"

    
#include <stdlib.h>
#include <stdio.h>


#define ERR_CH stderr
#define OUT_CH stdout


/*
** Define a macro that exits if a function return code indicates
** failure.
*/
#define EXIT_ON_FAIL(context, ret, str) \
  if (ret != CS_SUCCEED) \
  { \
    fprintf(ERR_CH, "Fatal error: %s\n", str); \
    if (context != (CS_CONTEXT *) NULL) \
    { \
      (CS_VOID) ct_exit(context, CS_FORCE_EXIT); \
      (CS_VOID) cs_ctx_drop(context); \
    } \
    exit(EX_EXIT_FAIL); \
  }

}

using namespace std;
using namespace iret;

PhraseDB::PhraseDB( string server, string username, string password ) {
  //  cerr << "beginning of db constructor\n";
  CS_RETCODE ret;

  context = (CS_CONTEXT *)NULL;
  ret = ex_init(&context);
  if ( ret != CS_SUCCEED ) ex_panic((char*)"ex_init failed");

  /*
  ** Step 3: Connect to the server. We must: - Allocate a connection
  ** structure. - Set user name and password. - Create the
  ** connection.
  */

  ret = ex_connect(context, &connection, NULL, (char*)username.data(),
		   (char*)password.data(), (char*)server.data() );
  EXIT_ON_FAIL(context, ret, "ex_connect failed");

  //  cerr << "end of db constructor\n";
}

/* Close the database connection.  Automaticly called by desctructor.
 *
 * For now, errors are handled by return code.  I would like to try
 * exceptions.
 */
CS_RETCODE
PhraseDB::close(void) {
    
  /*
  ** Close the connection and drop its control structure.
  */
  CS_RETCODE ret = CS_SUCCEED;
  if ( connection ) {
    // only if connection still exists
    ret = ex_con_cleanup(connection, ret);
    if ( ret == CS_SUCCEED )
      connection = NULL;
  }
    
  /*
  ** ct_exit tells Client-Library that we are done.
  ** and Drop the context structure.
  */
  if ( context ) {
    // only if context still exists
    ret = ex_ctx_cleanup(context, ret);
    if ( ret == CS_SUCCEED )
      context = NULL;
  }
  
  return ret;
}

/* Return a vector of sources.
 */
vector<Source>
PhraseDB::sources(void) {

  CS_COMMAND *cmd;		/* Command structure.    */
  CS_RETCODE ret;
  vector<Source> sourceVector;

  /*
  ** Allocate a command structure.
  */
  ret = ct_cmd_alloc(connection, &cmd);
  EXIT_ON_FAIL(context, ret, "ct_cmd_alloc() failed");
    
  /*
  ** Initiate a language command. This call associates a query with
  ** the command structure.
  */
  ret = ct_command(cmd, CS_LANG_CMD,
		   (char*)"select id, text, rank from Phrases..source",
		   CS_NULLTERM, CS_UNUSED);
  EXIT_ON_FAIL(context, ret, "ct_command() failed");

  /*
  ** Send the command.
  */
  ret = ct_send(cmd);
  EXIT_ON_FAIL(context, ret, "ct_send() failed");

  /*
  ** Step 5: Process the results of the command.
  */
  CS_RETCODE results_ret = 0;
  CS_INT result_type;

  while ((results_ret = ct_results(cmd, &result_type))
	 == CS_SUCCEED) {
      
    /*
    ** ct_results sets result_type to indicate when data is
    ** available and to indicate command status codes.
    */
    switch ((int)result_type) {
	
    case CS_ROW_RESULT: {
	
      /*
      ** This result_type value indicates that the rows
      ** returned by the query have arrived. We bind and
      ** fetch the rows.
      ** 
      ** For each column, fill in the relevant fields in
      ** the column's data format structure, and bind
      ** the column.
      */

      const int NUM_COLUMNS = 3;
      CS_DATAFMT format;
      CS_INT copied[NUM_COLUMNS+1];
      CS_SMALLINT indicator[NUM_COLUMNS+1];

      int id;
      format.datatype = CS_INT_TYPE;
      format.format = CS_FMT_UNUSED;
      format.maxlength = NULL;
      format.count = 1;
      format.locale = NULL;
      ret = ct_bind(cmd, 1, &format, &id, &copied[1],
		    &indicator[1]); 
      EXIT_ON_FAIL(context, ret,
		   "ct_bind() for au_lname failed");
      
      /*
      ** Same thing for the 'text' column.
      */
      const int MAX_TEXT = 255+1;
      char text[MAX_TEXT];
      format.datatype = CS_CHAR_TYPE;
      format.format = CS_FMT_NULLTERM;
      format.maxlength = MAX_TEXT;
      format.count = 1;
      format.locale = NULL;
      
      ret = ct_bind(cmd, 2, &format, text, &copied[1],
		    &indicator[1]);  
      EXIT_ON_FAIL(context, ret,
		   "ct_bind() for city failed");
	
	
      /*
      ** Same thing for the 'city' column.
      */
      int rank;
      format.datatype = CS_INT_TYPE;
      format.format = CS_FMT_UNUSED;
      format.maxlength = NULL;
      format.count = 1;
      format.locale = NULL;
      
      ret = ct_bind(cmd, 3, &format, &rank, &copied[1],
		    &indicator[1]);  
      EXIT_ON_FAIL(context, ret,
		   "ct_bind() for city failed");

      /*
      ** Now fetch and print the rows.
      */
      CS_INT count;
      while (((ret = ct_fetch(cmd, CS_UNUSED, CS_UNUSED,
			      CS_UNUSED, &count))
	      == CS_SUCCEED)
	     || (ret == CS_ROW_FAIL)) {
	  
	/*
	** Check if we hit a recoverable error.
	*/
	if (ret == CS_ROW_FAIL) {
	  fprintf(ERR_CH,
		  "Error on row %ld.\n",
		  (long)(count + 1));
	}
	  
	/*
	** We have a row, let's print it.
	*/
	Source s((string)text,id,rank);
	  
	sourceVector.push_back(s);
	  
	//	      fprintf(OUT_CH, "%s: \n", name);
      }

      /*
      ** We're finished processing rows, so check
      ** ct_fetch's final return value to see if an
      ** error occurred. The final return code should be
      ** CS_END_DATA.
      */
      if (ret == CS_END_DATA)
	{
	  fprintf(OUT_CH,
		  "\nAll done processing rows.\n");
	}
      else
				/* Failure occurred. */
	{
	  EXIT_ON_FAIL(context, CS_FAIL,
		       "ct_fetch failed");
	}
    }
    /*
    ** All done with this result set.
    */
    break;

    case CS_CMD_SUCCEED:

      /*
      ** We executed a command that never returns rows.
      */
      fprintf(OUT_CH, "No rows returned.\n");
      break;
      
    case CS_CMD_FAIL:
	
      /*
      ** The server encountered an error while
      ** processing our command. These errors will be
      ** displayed by the server-message callback that
      ** we installed earlier.
      */
      break;

    case CS_CMD_DONE:
	
      /*
      ** The logical command has been completely
      ** processed.
      */
      break;

    default:
	
      /*
      ** We got something unexpected.
      */
      EXIT_ON_FAIL(context, CS_FAIL,
		   "ct_results returned unexpected result type");
      break;
    }
  }

  /*
  ** We've finished processing results. Check the return value of
  ** ct_results() to see if everything went okay.
  */
  switch ((int)results_ret)
    {
    case CS_END_RESULTS:
	
      /*
      ** Everything went fine.
      */
      break;

    case CS_FAIL:
	
      /*
      ** Something terrible happened.
      */
      EXIT_ON_FAIL(context, CS_FAIL,
		   "ct_results() returned CS_FAIL.");
      break;
	
    default:
	
      /*
      ** We got an unexpected return value.
      */
      EXIT_ON_FAIL(context, CS_FAIL,
		   "ct_results returned unexpected return code");
      break;
    }
    
  /*
  ** Step 6:  Clean up and exit.
  */
    
  /*
  ** Drop the command structure.
  */
  ret = ct_cmd_drop(cmd);
  EXIT_ON_FAIL(context, ret, "ct_cmd_drop failed");
    
  return sourceVector;
    
}
  
/*
** Properly quote a string.
** This means single quotes at each end and single quotes in the
** middle doubled.
**
** I considered scanning for quotes and then copying the blocks text
** inbetween.  But this would require processing each character
** twice. 
*/
string
PhraseDB::quote (const string text) {
  string new_text = "'";	// start with opening quote

  // copy all the characters in the string
  for ( int i = 0; i < text.size(); ++i ) {
    new_text += text[i];
    if ( text[i] == '\'' )
      new_text += '\'';	// repeat each single quote
  }

  new_text += '\'';		// add closing quote
  return new_text;
}

    
/* Return whether or not the SQL command returned any results.
 *
 * Much needs to be done to make flexible.  For example, dynamic SQL
 * and prepared statements.
 */
bool
PhraseDB::boolSQLcmd(string sql_cmd) {

  CS_COMMAND *cmd;		/* Command structure.    */
  CS_RETCODE ret;
  bool resultsFound = false;
    
  /*
  ** Allocate a command structure.
  */
  ret = ct_cmd_alloc(connection, &cmd);
  EXIT_ON_FAIL(context, ret, "ct_cmd_alloc() failed");
    
  /*
  ** Initiate a language command. This call associates a query with
  ** the command structure.
  */

  ret = ct_command(cmd, CS_LANG_CMD, (char*)sql_cmd.c_str(),
		   CS_NULLTERM, CS_UNUSED);
  EXIT_ON_FAIL(context, ret, "ct_command() failed");

  /*
  ** Send the command.
  */
  ret = ct_send(cmd);
  EXIT_ON_FAIL(context, ret, "ct_send() failed");
    
  /*
  ** Step 5: Process the results of the command.
  */
  CS_RETCODE results_ret = 0;
  CS_INT result_type;
    
  while ((results_ret = ct_results(cmd, &result_type))
	 == CS_SUCCEED) {
      
    /*
    ** ct_results sets result_type to indicate when data is
    ** available and to indicate command status codes.
    */
    switch ((int)result_type) {
	
    case CS_ROW_RESULT: {
	
      /*
      ** This result_type value indicates that the rows
      ** returned by the query have arrived. We bind and
      ** fetch the rows.
      ** 
      ** We're expecting exactly two character columns:
      ** Column 1 is au_lname, 2 is au_city.
      ** 
      ** For each column, fill in the relevant fields in
      ** the column's data format structure, and bind
      ** the column.
      */
	
      const int NUM_COLUMNS = 1;
      CS_DATAFMT format;
      CS_INT copied[NUM_COLUMNS];
      CS_SMALLINT indicator[NUM_COLUMNS];
	
	    
      int id;
      format.datatype = CS_INT_TYPE;
      format.format = CS_FMT_UNUSED;
      format.maxlength = NULL;
      format.count = 1;
      format.locale = NULL;
      ret = ct_bind(cmd, 1, &format, &id, &copied[1],
		    &indicator[1]); 
      EXIT_ON_FAIL(context, ret,
		   "ct_bind() for au_lname failed");
	
      /*
      ** Now fetch and print the rows.
      */
      CS_INT count;
      while (((ret = ct_fetch(cmd, CS_UNUSED, CS_UNUSED,
			      CS_UNUSED, &count))
	      == CS_SUCCEED)
	     || (ret == CS_ROW_FAIL)) {
	  
	/*
	** Check if we hit a recoverable error.
	*/
	if (ret == CS_ROW_FAIL) {
	  fprintf(ERR_CH,
		  "Error on row %ld.\n",
		  (long)(count + 1));
	}
	  
	/*
	** We have a row, let's print it.
	*/
	//	  cerr << "Phrase found: " << phrase << "; Id: " << id <<
	//	    "\n";
	resultsFound = true;
	  
	// !! I want out of loop

      }
	
      /*
      ** We're finished processing rows, so check
      ** ct_fetch's final return value to see if an
      ** error occurred. The final return code should be
      ** CS_END_DATA.
      */
      if (ret == CS_END_DATA)
	{
	  //	    fprintf(OUT_CH,
	  //		    "\nAll done processing rows.\n");
	}
      else
				/* Failure occurred. */
	{
	  EXIT_ON_FAIL(context, CS_FAIL,
		       "ct_fetch failed");
	}
    }
    /*
    ** All done with this result set.
    */
    break;

    case CS_CMD_SUCCEED:

      /*
      ** We executed a command that never returns rows.
      */
      fprintf(OUT_CH, "No rows returned.\n");
      break;
      
    case CS_CMD_FAIL:
	
      /*
      ** The server encountered an error while
      ** processing our command. These errors will be
      ** displayed by the server-message callback that
      ** we installed earlier.
      */
      break;

    case CS_CMD_DONE:
	
      /*
      ** The logical command has been completely
      ** processed.
      */
      break;

    default:
	
      /*
      ** We got something unexpected.
      */
      EXIT_ON_FAIL(context, CS_FAIL,
		   "ct_results returned unexpected result type");
      break;
    }
  }

  /*
  ** We've finished processing results. Check the return value of
  ** ct_results() to see if everything went okay.
  */
  switch ((int)results_ret)
    {
    case CS_END_RESULTS:
	
      /*
      ** Everything went fine.
      */
      break;

    case CS_FAIL:
	
      /*
      ** Something terrible happened.
      */
      EXIT_ON_FAIL(context, CS_FAIL,
		   "ct_results() returned CS_FAIL.");
      break;
	
    default:
	
      /*
      ** We got an unexpected return value.
      */
      EXIT_ON_FAIL(context, CS_FAIL,
		   "ct_results returned unexpected return code");
      break;
    }
    
  /*
  ** Step 6:  Clean up and exit.
  */
    
  /*
  ** Drop the command structure.
  */
  ret = ct_cmd_drop(cmd);
  EXIT_ON_FAIL(context, ret, "ct_cmd_drop failed");
    
  return resultsFound;
    
}

void
PhraseDB::exit_on_fail(CS_RETCODE ret, string str) {
  if (ret != CS_SUCCEED) {
    cerr << "Fatal error: " << str << '\n';
    if (context != (CS_CONTEXT *) NULL) {
      (CS_VOID) ct_exit(context, CS_FORCE_EXIT);
      (CS_VOID) cs_ctx_drop(context);
    }
    exit(EX_EXIT_FAIL);
  }
}


/* Return the list of phrases from the source as a "Btree" list.
 */
List*
PhraseDB::list_prepare(string sql_cmd) {
  
  SybCmd cmd(*this);		/* Command structure.    */
  CS_RETCODE ret;
  List* phrase_list = new List();
    
  /*
  ** Initiate a language command. This call associates a query with
  ** the command structure.
  */
  cmd.prepare(sql_cmd);

  const int MAX_TEXT = 255+1;
  char text[MAX_TEXT];
  cmd.bind( 1, CS_CHAR_TYPE, CS_FMT_NULLTERM, MAX_TEXT, text );

  while ( CS_SUCCEED  == cmd.fetch() ) {
	  
    /*
    ** We have phrase, add it to the list.
    */
    phrase_list->add_key_count(text);
    //	  cerr << phrase_list->count << '\t' << text << '\n';

  }

  cmd.done();

  return phrase_list;
}

/* Return the list of phrases from the source as a "Btree" list.
 */
List*
PhraseDB::list(int source) {

  ostringstream sql_cmd;
  sql_cmd << "select text from Phrases..phrase where source = " <<
    source; 

  return list_prepare(sql_cmd.str());
}

/* Return the list of phrases from the source as a "Btree" list.
 */
List*
PhraseDB::list(string source) {

  ostringstream sql_cmd;
  sql_cmd << "select p.text from Phrases..phrase p, Phrases..source s ";
  sql_cmd << "where p.source = s.id and s.text like '" << source << "%'";

  return list_prepare(sql_cmd.str());
}

PhraseTable*
PhraseDB::tempTable( string name ) {
  PhraseTable* tp = new PhraseTable( *this, name );
  return tp;
}
