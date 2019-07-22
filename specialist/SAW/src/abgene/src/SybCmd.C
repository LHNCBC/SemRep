#include "SybCmd.h"
 
void
SybCmd::prepare( string command ) {

  CS_RETCODE ret;
  /*
  ** Allocate a command structure.
  */
  ret = ct_cmd_alloc(myDB.getConnection(), &cmd);
  myDB.exit_on_fail( ret, "ct_cmd_alloc() failed");
  
  /*
  ** Initiate a language command. This call associates a query with
  ** the command structure.
  */
  ret = ct_command(cmd, CS_LANG_CMD, (char*)command.c_str(),
		   CS_NULLTERM, CS_UNUSED); 
  myDB.exit_on_fail( ret, "ct_command() failed");

  /*
  ** Send the command.
  */
  ret = ct_send(cmd);
  myDB.exit_on_fail( ret, "ct_send() failed");

  /*
  ** Step 5: Process the results of the command.
  */
  results_ret = 0;
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
      ** We have a result set.  Let the user fetch the data.
      */
      return;
      break;
    }
    
    case CS_CMD_SUCCEED:
      
      /*
      ** We executed a command that never returns rows.
      */
      //      cerr << "No rows returned.\n";
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
      myDB.exit_on_fail( CS_FAIL,
		   "ct_results returned unexpected result type");
      break;
    }
  }

  /*
  ** We've finished processing results. Check the return value of
  ** ct_results() to see if everything went okay.
  */
  switch ((int)results_ret) {
  case CS_END_RESULTS:
      
    /*
    ** Everything went fine.
    */
    break;
      
  case CS_FAIL:
      
    /*
    ** Something terrible happened.
    */
    myDB.exit_on_fail( CS_FAIL,
		 "ct_results() returned CS_FAIL.");
    break;
      
  default:
      
    /*
    ** We got an unexpected return value.
    */
    myDB.exit_on_fail( CS_FAIL,
		 "ct_results returned unexpected return code");
    break;
  }
}

void
SybCmd::done () {
  if ( cmd == NULL )
    // our work is done
    return;

  CS_INT result_type;

  if ( results_ret ==  CS_SUCCEED ) {
    // previous methods left us some house cleaning

    while ((results_ret = ct_results(cmd, &result_type))
	   == CS_SUCCEED) {
      
      /*
      ** ct_results sets result_type to indicate when data is
      ** available and to indicate command status codes.
      */
      switch ((int)result_type) {
	
      case CS_ROW_RESULT: {

	cerr << "cannot handle multiple result sets\n";
	break;
      }
      case CS_CMD_SUCCEED:

	/*
	** We executed a command that never returns rows.
	*/
	//	cout << "No rows returned.\n";
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
	myDB.exit_on_fail( CS_FAIL,
			   "ct_results returned unexpected result type");
	break;
      }
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
      myDB.exit_on_fail( CS_FAIL,
		   "ct_results() returned CS_FAIL.");
      break;
	
    default:
	
      /*
      ** We got an unexpected return value.
      */
      myDB.exit_on_fail( CS_FAIL,
		   "ct_results returned unexpected return code");
      break;
    }

    
  /*
  ** Step 6:  Clean up and exit.
  */
    
  /*
  ** Drop the command structure.
  */
  CS_RETCODE ret = ct_cmd_drop(cmd);
  myDB.exit_on_fail( ret, "ct_cmd_drop failed");

  cmd = NULL;
}

/*
** This result_type value indicates that the rows
** returned by the query have arrived. We bind and
** fetch the rows.
** 
** For each column, fill in the relevant fields in
** the column's data format structure, and bind
** the column.
*/
void
SybCmd::bind( CS_INT num, CS_INT type, CS_INT format, CS_INT length,
	      CS_VOID* address ) {

  CS_DATAFMT datafmt;
  datafmt.datatype = type;
  datafmt.format = format;
  datafmt.maxlength = length;
  datafmt.count = 1;
  datafmt.locale = NULL;

  /* It might be useful having a way to to deal with the copied and
     indicator values. (last two NULL's in ct_bind call.
  */
  CS_RETCODE ret = ct_bind(cmd, num, &datafmt, address, NULL, NULL );

  myDB.exit_on_fail( ret, "ct_bind() failed");
}

CS_RETCODE
SybCmd::fetch () {

  /*
  ** Now fetch and print the rows.
  */
  CS_INT count;
  CS_RETCODE ret =
    ct_fetch(cmd, CS_UNUSED, CS_UNUSED, CS_UNUSED, &count);
	  
  /*
  ** Check if we hit a recoverable error.
  **
  ** Any error recovery must be made outside this routine.  Just keep
  ** calling fetch.
  */
  if (ret == CS_ROW_FAIL) {
    cerr << "Error on row " << (long)(count + 1) << ".\n";
    return ret;
  }

  /*
  ** Let the user process the row values.
  */
  if ( ret == CS_SUCCEED ) {
    return ret;
  }

  /*
  ** We're finished processing rows, so check
  ** ct_fetch's final return value to see if an
  ** error occurred. The final return code should be
  ** CS_END_DATA.
  */
  if (ret == CS_END_DATA) {
    //    cerr << "\nAll done processing rows.\n";
    return ret;
  }
				/* Failure occurred. */
    myDB.exit_on_fail( CS_FAIL, "ct_fetch failed");
}
