/* Copyright(C) 1999, Swedish Institute of Computer Science */

:- module(odbc, [
                 odbc_env_open/1,
                 odbc_list_DSN/2,
                 odbc_db_open/3,
                 odbc_db_open/4,
                 odbc_db_open/5,
                 odbc_query_open/2,
                 odbc_list_data_types/3,
                 odbc_current_table/2, % [PM] 4.2
                 odbc_current_table/3, % [PM] 4.2
                 odbc_table_column/3,  % [PM] 4.2
                 odbc_table_column/4,  % [PM] 4.2
                 odbc_query_execute_sql/3,
%%               odbc_query_execute_sql/4,
                 odbc_query_execute_sql/5,
                 odbc_sql_fetch/2,
                 odbc_query_close/1,
                 odbc_db_close/1,
                 odbc_env_close/1
                 ]).

% Not yet exported.
:- public
           odbc_set_autocommit/2,
           odbc_commit_transaction/1,
           odbc_rollback_transaction/1.


:- use_module(library(codesio)).
:- use_module(library(lists)).
:- use_module(library(types)).

%@  This library is an interface to an ODBC database driver. For an
%@  introduction to ODBC, see
%@  @uref{http://msdn.microsoft.com/en-us/library/ms715408(VS.85).aspx}
%@  ("Introduction to ODBC"; Microsoft Web Page).
%@  ODBC 3.x is supported.
%@  
%@  @menu
%@  * ODBC Overview:: Overview
%@  * ODBC Examples:: Examples
%@  * ODBC Datatypes:: Datatypes
%@  * ODBC Exceptions:: Exceptions
%@  * ODBC Predicates:: Predicates
%@  @end menu
%@  
%@  @node ODBC Overview
%@  @subsection Overview
%@  
%@  ODBC (Open Database Connectivity) is a standard API for using a
%@  DBMS (DataBase Management System). By using ODBC you can access data
%@  from a multitude of DBMSs without having to know the details of
%@  each DBMS.
%@  
%@  @code{library(odbc)} is a layer on top of ODBC. It has predicates for
%@  opening the database, starting and executing a query, and
%@  retrieving the results of a query.

%@  The ODBC client application, i.e.@: this library, accesses all
%@  ODBC functionality via a service provided by the operating system,
%@  the ODBC Driver Manager (DM).
%@  

%@  Some operating systems (e.g. Mac OS X and MS Windows) usually come
%@  with an ODBC Driver Manager preinstalled. For other, UNIX and
%@  UNIX-like, operating systems, unixODBC
%@  (@uref{http://www.unixodbc.org}) is the most common but Mac OS X
%@  use iODBC (@uref{http://www.iodbc.org}).
%@  

%@  The ODBC Driver Manager does not, in itself, provide any database
%@  functionality. Instead the DM loads a ODBC driver specific to the
%@  particular Database Management System (DBMS) (when
%@  @code{odbc_db_open/[3,4,5]} is called).
%@  

%@  How to install and configure an ODBC driver is beyond the scope of
%@  this document. Please consult the documentation for the particular
%@  DBMS you intend to use. Some popular DBMSs are MySQL and
%@  PostgreSQL which both provide ODBC drivers for many platforms.
%@  


%@  
%@  @node ODBC Examples
%@  @subsection Examples
%@  
%@  A few examples will best illustrate how to use @code{library(odbc)}.
%@  
%@  @menu
%@  * ODBC Example 1:: Example 1
%@  * ODBC Example 2:: Example 2
%@  * ODBC Example 3:: Example 3
%@  * ODBC Example 4:: Example 4
%@  @end menu
%@  

%@  @node ODBC Example 1
%@  @subsubsection Example 1
%@  
%@  The first example just verifies that ODBC is working and that some
%@  ODBC drivers have been configured in the ODBC Driver Manager.
%@  
%@  @example
%@  @group
%@  :- use_module(library(odbc)).
%@
%@  example1 :-
%@      odbc_env_open(EnvHandle),
%@      odbc_list_DSN(EnvHandle, DSNs),
%@      odbc_env_close(EnvHandle),
%@      format('The known DSNs are: ~q~n', [DSNs]).
%@  @end group
%@  @end example
%@  
%@  You begin by opening an environment. This is a handle which can be
%@  used for various calls to the ODBC Driver Manager (DM).
%@  You then ask the DM about the data sources, i.e. databases, it
%@  knows about. If this list is empty you need to install and
%@  configure the ODBC drivers appropriate for the database management
%@  system that you intend to use.
%@  

%@  @node ODBC Example 2
%@  @subsubsection Example 2
%@  
%@  This example is a simple SQL query using a fixed SQL string.
%@  
%@  @example
%@  @group
%@  :- use_module(library(odbc)).
%@
%@  example_select :-
%@      odbc_env_open(EnvHandle),
%@      odbc_db_open('MyDatabase', EnvHandle, ConnectionHandle),
%@      odbc_query_open(ConnectionHandle, StatementHandle),
%@      odbc_query_execute_sql(StatementHandle,
%@                         'SELECT cookie,soft FROM bakery order by soft',
%@                         ResultSet),
%@      show_result(ResultSet),
%@      odbc_query_close(ResultSet),
%@      odbc_db_close(ConnectionHandle),
%@      odbc_env_close(EnvHandle).
%@  
%@  show_result(ResultSet) :-
%@      odbc_sql_fetch(ResultSet, Row),
%@      show_result1(Row, ResultSet).
%@  
%@  show_result1([], _ResultSet) :- !.
%@  show_result1(Row, ResultSet) :-
%@      format('~w~n', [Row]),
%@      flush_output,
%@      odbc_sql_fetch(ResultSet, Row1),
%@      show_result1(Row1, ResultSet).
%@  @end group
%@  @end example
%@  
%@  As always, you begin by opening an environment.
%@  You then connect to the database with @code{odbc_db_open/3}.
%@  The first argument is the identifier for the database in the DBMS.
%@  In this scenario, connecting to the database does not require a
%@  username and a password.
%@  The output from @code{odbc_db_open/3} is an opaque handle on the
%@  database.
%@  
%@  First, @code{odbc_query_open/2} is used to create an SQL query, which is
%@  straightforward.
%@  Then, @code{odbc_query_execute_sql/3} is used to execute the SQL
%@  query. By executing an SQL query a @emph{result set} is created.
%@  Each consecutive call of @code{odbc_sql_fetch/2} will retrieve one
%@  row from the result set.
%@  
%@  @node ODBC Example 3
%@  @subsubsection Example 3
%@  
%@  This example shows the use of parameter binding. The positional markers
%@  (@var{?}) in the SQL string are bound to the elements in the list in the
%@  third argument of odbc_query_execute_sql/5. The fourth argument is a list of
%@  datatypes corresponding to the parameters.
%@
%@  @example
%@  @group
%@  :- use_module(library(odbc)).
%@
%@  example2 :-
%@     odbc_env_open('SQL_OV_ODBC3', EnvHandle),
%@     odbc_db_open('MyDatabase', EnvHandle, ConnectionHandle),
%@     odbc_query_open(ConnectionHandle, StatementHandle),
%@     odbc_query_execute_sql(StatementHandle,
%@                         'INSERT INTO scratch (vehicle, wheels) VALUES (?, ?)',
%@                         ["railwaycar", 8],
%@                         ['SQL_VARCHAR', 'SQL_INTEGER'],
%@                         ResultSet),
%@     odbc_query_close(ResultSet),
%@     odbc_db_close(ConnectionHandle),
%@     odbc_env_close(EnvHandle).
%@  @end group
%@  @end example
%@  
%@  @node ODBC Example 4
%@  @subsubsection Example 4
%@  
%@  This example is similar to the second, but this time we ask the
%@  database what the datatypes of the columns of the table are with
%@  odbc_list_data_types/3.
%@
%@  @example
%@  @group
%@  :- use_module(library(odbc)).
%@
%@  example3 :-
%@     odbc_env_open(EnvHandle),
%@     odbc_db_open('MyDatabase', EnvHandle, ConnectionHandle),
%@     odbc_query_open(ConnectionHandle, StatementHandle),
%@     odbc_list_data_types(StatementHandle,
%@                          scratch(vehicle, wheels),
%@                          DataTypes),
%@     odbc_query_execute_sql(StatementHandle,
%@                         'INSERT INTO scratch (vehicle, wheels) VALUES (?, ?)',
%@                         ["railwaycar", 8],
%@                         DataTypes,
%@                         ResultSet),
%@     odbc_query_close(ResultSet),
%@     odbc_db_close(ConnectionHandle),
%@     odbc_env_close(EnvHandle).
%@
%@  @end group
%@  @end example
%@  
%@  @node ODBC Datatypes
%@  @subsection Datatypes
%@
%@  @menu
%@  * ODBC Reading:: Reading from the database
%@  * ODBC Writing:: Writing to the database
%@  @end menu
%@
%@  @node ODBC Reading
%@  @subsubsection Reading from the database
%@
%@  When reading data from the database the following datatypes are supported,
%@  with conversion to the corresponding prolog datatypes.

%@  @table @asis
%@  @item @code{SQL_CHAR}, @code{SQL_VARCHAR} etc.@:
%@    A list of character codes.
%@  @item @code{SQL_BIT}
%@    The integer @code{0} for false, or @code{1} for true.
%@  @item @code{SQL_INTEGER}, @code{SQL_TINYINT}, @code{SQL_SMALLINT}, etc.@:
%@    An integer.
%@  @item @code{SQL_REAL}, @code{SQL_DOUBLE}, @code{SQL_FLOAT}
%@    A floating point number.
%@  @item @code{SQL_DATE}
%@    A term @code{date(Year, Month, DayOfMonth)}, with one-based
%@    integer arguments. E.g.@: @code{date(2012,10,22)} means October
%@    22, 2012.
%@  @item @code{SQL_TIME}
%@    A term @code{time(Hour, Minute, Second)} with one-based integer
%@    arguments. E.g.@: @code{time(22,11,5)} means eleven minutes and
%@    five seconds past ten pm.
%@  @item @code{SQL_TIMESTAMP}
%@    A term @code{timestamp(Year, Month, Day, Hour, Minute, Second,
%@    Fraction)} where the arguments have the same meaning as for
%@    @code{SQL_TIME} and @code{SQL_TIMESTAMP} and @var{Fraction}
%@    means fractional nanoseconds past, as an integer.
%@  @item the SQL null value
%@   The atom @code{null}.
%@  @item  @code{SQL_BINARY} and other binary types
%@  @itemx @code{SQL_INTERVAL_HOUR} and other interval types
%@  @itemx @code{SQL_UTCTIME} and @code{SQL_UTCDATETIME}
%@   Currently not supported.
%@  @end table
%@  @noindent Note that atoms with names that start with an upper case letter,
%@  like @code{SQL_CHAR} must be quoted in Prolog, e.g.@: @code{'SQL_CHAR'}.

% %@
% %@  @table @code
% %@  @item SQL:     CHAR
% %@  @itemx Prolog:  code list
% %@  @item SQL:     VARCHAR
% %@  @itemx Prolog:  code list
% %@  @item SQL:     BIT
% %@  @itemx Prolog:  integer
% %@  @item SQL:     TINYINT
% %@  @itemx Prolog:  integer
% %@  @item SQL:     SMALLINT
% %@  @itemx Prolog:  integer
% %@  @item SQL:     INTEGER
% %@  @itemx Prolog:  integer
% %@  @item SQL:     REAL
% %@  @itemx Prolog:  float
% %@  @item SQL:     DOUBLE
% %@  @itemx Prolog:  float
% %@  @item SQL:     FLOAT
% %@  @itemx Prolog:  float
% %@  @item SQL:     DATE
% %@  @itemx Prolog:  date(Year, Month, Day)
% %@  @item SQL:     TIME
% %@  @itemx Prolog:  time(Hour, Minute, Second)
% %@  @item SQL:     TIMESTAMP
% %@  @itemx Prolog:  timestamp(Year, Month, Day, Hour, Minute, Second, Fraction)
% %@  @end table

%@
%@  @node ODBC Writing
%@  @subsubsection Writing to the database
%@
%@  When writing data to the database the following SQL datatypes are supported.
%@

%@  @table @asis
%@  @item @code{SQL_CHAR}, @code{SQL_VARCHAR} etc.@:
%@   A list of character codes, or a list of atoms.
%@   
%@   For backwards compatibility only, an atom is also accepted, but
%@   note that the atoms @code{null} and @code{[]} have special meaning
%@   (as SQL null value and empty code list, respectively) and more
%@   atoms with special meaning may be introduced in the future. For
%@   compatibility with some ODBC drivers, the integer 0 and 1 are
%@   allowed, meaning "0" and "1".

%@  @item @code{SQL_BIT}
%@    The integer @code{0} for false, or @code{1} for true.
%@  @item @code{SQL_INTEGER}, @code{SQL_TINYINT}, @code{SQL_SMALLINT}, etc.@:
%@    An integer.
%@  @item @code{SQL_REAL}, @code{SQL_DOUBLE}, @code{SQL_FLOAT}
%@   A floating point number or a small integer.
%@  @item @code{SQL_DATE}
%@    A term @code{date(Year, Month, DayOfMonth)}, as above.
%@  @item @code{SQL_TIME}
%@    A term @code{time(Hour, Minute, Second)}, as above.
%@  @item @code{SQL_TIMESTAMP}
%@    A term @code{timestamp(Year, Month, Day, Hour, Minute, Second,
%@    Fraction)}, as above.
%@  @item the SQL null value
%@   The atom @code{null}.
%@  @item  @code{SQL_BINARY} and other binary types
%@  @itemx @code{SQL_INTERVAL_HOUR} and other interval types
%@  @itemx @code{SQL_UTCTIME} and @code{SQL_UTCDATETIME}
%@   Currently not supported.
%@  @end table
%@  @noindent if a value is out of range for the corresponding SQL
%@   type, e.g.@: a too large integer for @code{SQL_SMALLINT}, the
%@   result is undefined.
%@  Note that atoms with names that start with an upper case letter,
%@   like @code{SQL_CHAR} must be quoted in Prolog, e.g.@: @code{'SQL_CHAR'}.

% %@  @table @code
% %@  @item SQL:     CHAR, VARCHAR
% %   SQL_C_CHAR
% %@  @itemx Prolog:  atom or code list
% %@  @item SQL:     BIT
% %   SQL_C_BIT
% %@  @itemx Prolog:  integer
% %@  Any non-zero value will be stored as a @code{1}.
% %@  @item SQL:     SMALLINT
% %   SQL_C_SSHORT
% %@  @itemx Prolog:  integer
% %@  If the integer value is to large to be stored in a SMALLINT the result is
% %@  undefined.
% %@  @item SQL:     INTEGER
% %   SQL_C_SLONG
% %@  @itemx Prolog:  integer
% %@  @item SQL:     REAL
% %   SQL_C_FLOAT
% %@  @itemx Prolog:  float
% %@  If the float value is to large to be stored in a REAL the result is
% %@  undefined.
% %@  @item SQL:     DOUBLE, FLOAT
% %   SQL_C_DOUBLE
% %@  @itemx Prolog:  float
% %@  @item SQL:     DATE
% %   SQL_C_TYPE_DATE
% %@  @itemx Prolog:  date(Year, Month, Day)
% %@  @item SQL:     TIME
% %   SQL_C_TYPE_TIME
% %@  @itemx Prolog:  time(Hour, Minute, Second)
% %@  @item SQL:     TIMESTAMP
% %   SQL_C_TYPE_TIMESTAMP
% %@  @itemx Prolog:  timestamp(Year, Month, Day, Hour, Minute, Second, Fraction)
% %@  @item SQL:     LONG
% %   SQL_C_LONG
% %@  @itemx Prolog:  integer
% %@  @end table

% %@  @table @code
% %@  @item SQL:     CHAR, VARCHAR
% %   SQL_C_CHAR
% %@  @itemx Prolog:  atom or code list
% %@  @item SQL:     BIT
% %   SQL_C_BIT
% %@  @itemx Prolog:  integer
% %@  Any non-zero value will be stored as a @code{1}.
% %@  @item SQL:     SMALLINT
% %   SQL_C_SSHORT
% %@  @itemx Prolog:  integer
% %@  If the integer value is to large to be stored in a SMALLINT the result is
% %@  undefined.
% %@  @item SQL:     INTEGER
% %   SQL_C_SLONG
% %@  @itemx Prolog:  integer
% %@  @item SQL:     REAL
% %   SQL_C_FLOAT
% %@  @itemx Prolog:  float
% %@  If the float value is to large to be stored in a REAL the result is
% %@  undefined.
% %@  @item SQL:     DOUBLE, FLOAT
% %   SQL_C_DOUBLE
% %@  @itemx Prolog:  float
% %@  @item SQL:     DATE
% %   SQL_C_TYPE_DATE
% %@  @itemx Prolog:  date(Year, Month, Day)
% %@  @item SQL:     TIME
% %   SQL_C_TYPE_TIME
% %@  @itemx Prolog:  time(Hour, Minute, Second)
% %@  @item SQL:     TIMESTAMP
% %   SQL_C_TYPE_TIMESTAMP
% %@  @itemx Prolog:  timestamp(Year, Month, Day, Hour, Minute, Second, Fraction)
% %@  @item SQL:     LONG
% %   SQL_C_LONG
% %@  @itemx Prolog:  integer
% %@  @end table
% %@

%@  @node ODBC Exceptions
%@  @subsection Exceptions
%@
%@ @c [PM] 4.2 FIXME: all odbc_error terms ough to have the same arity
%@ @c (2). As it now stands it is impossible to selectively catch only
%@ @c odbc errors with a single catch/3. Also, the error/2 wrapping
%@ @c does not make much sense.
%@
%@  When an error in the ODBC layer occurs, predicates in
%@  @code{library(odbc)} throw @code{error/2} exceptions. Both arguments of the
%@  @code{error/2} exception are the same and has the following form
%@  @code{odbc_error(@var{Detail}, @var{Goal})}, where @code{Goal} is
%@  some goal where the error occurred, and @var{Detail} gives more
%@  information about the error. The @var{Detail} term can have the
%@  following form:
%@
%@  @table @code
%@  @item data_conversion
%@  Thrown in case of a error when converting to or from a SICStus data
%@  type from or to an ODBC data type.
%@
%@  @item unsupported_datatype
%@  Thrown when an SQL data type is unsupported when converting to or
%@  from a SICStus data type from or to an ODBC data type.
%@
%@  @item unknown_datatype
%@  Thrown when an unknown SQL data type is found when converting to or
%@  from a SICStus data type from or to an ODBC data type.
%@
%@  @item type_error
%@  Thrown when the Prolog data is of a type incompatible with the SQL data type
%@  when converting from a SICStus data type to an ODBC data type.
%@
%@  @item native_code
%@  Thrown in case of a error in the native code of @code{library(odbc)}.
%@
%@  @c [PM] 4.2 FIXME: The format of the invalid_handle error terms does not make sense.
%@  @c [PM] 4.2 FIXME: change to odbc_error/2
%@  @item invalid_handle(handle_type, @var{InvalidHandle}, @var{ReturnCode})
%@  Thrown when an invalid handle type is specified.
%@
%@  @item invalid_handle('HandleType'-@var{HandleType}, 'Handle'-@var{Handle})
%@  Thrown when an invalid handle is specified.
%@
%@  @item invalid_handle(result_set, @var{ResultSet})
%@
%@  Thrown when a Result Set handle is invalid.
%@
%% Undocumented until odbc_env_open/2 is exported.
% %@  @c [PM] 4.2 FIXME: change to odbc_error/2
% %@  @item unknown_env_option(@var{Option})
% %@
% %@  Thrown when an unknown option was given when calling @code{odbc_env_open/2}.
% %@
% %@  @c [PM] 4.2 FIXME: change to odbc_error/2
% %@  @item unknown_odbc_version(@var{VersionCode})
% %@
% %@  Thrown when a version code other than 'SQL_OV_ODBC2' or 'SQL_OV_ODBC3' was
% %@  given as the argument to the option @code{odbc_version/1} when calling
% %@  @code{odbc_env_open/2}.
%@
%@  @c [PM] 4.2 FIXME: change to odbc_error/2
%@  @item unknown_connection_option(@var{Options})
%@
%@  Thrown when an unknown option was given when calling @code{odbc_db_open/[3,4,5]}.
%@
%% Perhaps document this
% %@  @c [PM] 4.2 FIXME: change to odbc_error/2
% %@  @item consistency_error(@var{NCols}, @var{NTCols}, 'Number of columns not consistent with number of types')
% 
%@
%@  @item internal_error
%@  Thrown when an internal error occurs in @code{library(odbc)}. Please
%@  report this to SICStus Support.
%@
%@  @c [PM] 4.2 FIXME: change to odbc_error/2
%@  @item diag(@var{ReturnCode}, @var{Recs})
%@  Thrown when an error occurs in the ODBC layer, e.g. a SQL syntax error.
%@  @var{Recs} is bound to the diagnostic records reported from ODBC.
%@
%@  @item out_of_memory
%@  Thrown when some operation runs out of memory.

%@  @end table
%@  @noindent there may be other @var{Details} and new @var{Details}
%@  may be added in the future.
%@
%@  @node ODBC Predicates
%@  @subsection Predicates
%@  @table @code

%%========================================

% xref odbc.c
:- buffer_throw_error/2 is throwing.
buffer_throw_error(ReturnCode, Goal) :-
    ( ReturnCode == 0 ->        % PROLOG_RESULT_DATA_CONVERSION_FAILED
        E = odbc_error(data_conversion, Goal),
        throw(error(E,E))
    ; ReturnCode == -1 ->       % PROLOG_RESULT_UNSUPPORTED_DATA_TYPE
        E = odbc_error(unsupported_datatype, Goal),
        throw(error(E,E))
    ; ReturnCode == -2 ->       % PROLOG_RESULT_UNKNOWN_DATATYPE
        E = odbc_error(unknown_datatype, Goal),
        throw(error(E,E))
    ; ReturnCode == -3 ->       % PROLOG_RESULT_TYPE_ERROR
        E = odbc_error(type_error, Goal),
        throw(error(E,E))
    ; ReturnCode == -4 ->       % PROLOG_RESULT_INVALID_HANDLE
        E = odbc_error(internal_error, Goal),
        throw(error(E,E))
    ; ReturnCode == -5 ->       % PROLOG_RESULT_INVALID_BUFFER
        E = odbc_error(internal_error, Goal),
        throw(error(E,E))
    ; ReturnCode == -6 ->       % PROLOG_RESULT_INVALID_BUFFER_SIZE
        E = odbc_error(internal_error, Goal),
        throw(error(E,E))
    ; ReturnCode == -19 ->      % PROLOG_RESULT_IMPOSSIBLE_ERROR
        E = odbc_error(internal_error, Goal),
        throw(error(E,E))
    ; ReturnCode == -20 ->      % PROLOG_RESULT_OUT_OF_MEMORY
        E = odbc_error(internal_error, Goal),
        throw(error(E,E))
    ; otherwise ->
        E = odbc_error(internal_error, Goal),
        throw(error(E,E))
    ).


sp_check_return(E, _Goal) :- E == 1, !, % PROLOG_NATIVE_CODE_SUCCESS
    true.
sp_check_return(E, Goal) :-
    sp_throw_error(E, Goal).

:- sp_throw_error/2 is throwing.
sp_throw_error(E, Goal) :- E == 0,    % PROLOG_NATIVE_CODE_ERROR
    E = odbc_error(native_code, Goal),
    throw(error(E,E)).
sp_throw_error(Code, Goal) :-  % [PM] 4.2 can not happen
    E = odbc_error(internal_error, sp_check_return(Code, Goal)),
    throw(error(E,E)).


%% [PM] 4.2 Note: Ensure that caller unifies its output arguments
%% _after_ calling odbc_check_return/3 so we do not fail before
%% detecting error.
odbc_check_return(EnvHandle, ReturnCode, Goal) :-
    EnvHandle = environment_handle(_), !,
    symbol_lookup('SQL_HANDLE_ENV', HandleType),
    odbc_check_return(HandleType, EnvHandle, ReturnCode, Goal).
odbc_check_return(ConnectionHandle, ReturnCode, Goal) :-
    ConnectionHandle = connection_handle(_), !,
    symbol_lookup('SQL_HANDLE_DBC', HandleType),
    odbc_check_return(HandleType, ConnectionHandle, ReturnCode, Goal).
odbc_check_return(StatementHandle, ReturnCode, Goal) :-
     StatementHandle = statement_handle(_), !,
    symbol_lookup('SQL_HANDLE_STMT', HandleType),
    odbc_check_return(HandleType, StatementHandle, ReturnCode, Goal).
odbc_check_return(InvalidHandle, ReturnCode, Goal) :-
    E = odbc_error(invalid_handle(handle_type, InvalidHandle, ReturnCode), Goal),
    throw(error(E,E)).

odbc_check_return(HandleType, Handle, ReturnCode, Goal) :-
    '$sql_return'(ReturnCode, RC, _),
    odbc_check_return1(RC, HandleType, Handle, Goal).

odbc_check_return1('SQL_SUCCESS', _HandleType, _Handle, _Goal) :- !.
odbc_check_return1('SQL_SUCCESS_WITH_INFO', _HandleType, _Handle, _Goal) :- !.
odbc_check_return1('SQL_NO_DATA', _HandleType, _Handle, _Goal) :- !.
odbc_check_return1('SQL_INVALID_HANDLE', HandleType, Handle, Goal) :-
   E = odbc_error(invalid_handle('HandleType'-HandleType, 'Handle'-Handle), Goal),
    throw(error(E,E)).
odbc_check_return1('ODBC_ERROR_INVALID_HANDLE', HandleType, Handle, Goal) :-
    % [PM] 4.2 This indicates that Handle is an invalid handle-index
    E = odbc_error(invalid_handle('HandleType'-HandleType, 'Handle'-Handle), Goal),
    throw(error(E,E)).
odbc_check_return1('ODBC_ERROR_INVALID_PARAMETER', _HandleType, _Handle, Goal) :-
    % [PM] 4.2 "should not happen"
    E = odbc_error(parameter_error, Goal),
    throw(error(E,E)).
odbc_check_return1('ODBC_ERROR_OUT_OF_MEMORY', HandleType, Handle, Goal) :-
    odbc_error_close_handle(HandleType, Handle),
    EID = odbc_error(out_of_memory, Goal),
    throw(error(EID,EID)).
odbc_check_return1('ODBC_ERROR_DATA_CONVERSION', _HandleType, _Handle, Goal) :-
    PROLOG_RESULT_DATA_CONVERSION_FAILED = 0,
    buffer_throw_error(PROLOG_RESULT_DATA_CONVERSION_FAILED, Goal).
odbc_check_return1('ODBC_ERROR_UNSUPPORTED_DATA_TYPE', _HandleType, _Handle, Goal) :-
    PROLOG_RESULT_UNSUPPORTED_DATA_TYPE = -1,
    buffer_throw_error(PROLOG_RESULT_UNSUPPORTED_DATA_TYPE, Goal).
odbc_check_return1('ODBC_ERROR_UNKNOWN_DATA_TYPE', _HandleType, _Handle, Goal) :-
    PROLOG_RESULT_UNKNOWN_DATATYPE = -2,
    buffer_throw_error(PROLOG_RESULT_UNKNOWN_DATATYPE, Goal).
odbc_check_return1('ODBC_ERROR_TYPE_ERROR', _HandleType, _Handle, Goal) :-
    PROLOG_RESULT_TYPE_ERROR = -3,
    buffer_throw_error(PROLOG_RESULT_TYPE_ERROR, Goal).


odbc_check_return1('ODBC_ERROR_CREATING_HANDLE', _HandleType, _Handle, Goal) :-
    %% [PM] 4.2 The real, underlying, error code is SQL_ERROR
    %% [PM] 4.2 We could look up diagnostics but we would have to ask
    %% the connection handle (since there is no Handle)
    Recs = [],
    EID = odbc_error(diag('SQL_ERROR', Recs), Goal),
    throw(error(EID,EID)).
odbc_check_return1(ReturnCode, HandleType, Handle, Goal) :-
    accumulate_diag_recs(HandleType, Handle, 1, [], Recs),
    odbc_error_close_handle(HandleType, Handle),
    EID = odbc_error(diag(ReturnCode, Recs), Goal),
    throw(error(EID,EID)).

accumulate_diag_recs(HandleType, Handle, Nrec, Recs, Recs1) :-
    '$odbc_get_diag_rec'(HandleType, Handle, Nrec, SQLState, NativeError, Message,
                        TextLength, Res),
    '$sql_return'(Res, RC, _),
    (
      RC == 'SQL_SUCCESS' ->
      Recs2 = [[NativeError, SQLState, TextLength, Message] | Recs],
      Nrec1 is Nrec + 1,
      accumulate_diag_recs(HandleType, Handle, Nrec1, Recs2, Recs1)
    ;
      Recs1 = Recs
    ).

odbc_error_close_handle(HandleType,Handle) :-
    symbol_lookup('SQL_HANDLE_ENV', HandleTypeEnv),
    symbol_lookup('SQL_HANDLE_DBC', HandleTypeConn),
    ( HandleType == HandleTypeEnv ->
        '$odbc_free_environment_handle'(Handle, _)
    ; HandleType == HandleTypeConn ->
        '$odbc_disconnect'(Handle, _),
        '$odbc_free_connection_handle'(Handle, _)
    ; true
    ).

ensure_codes(AtomOrCodes, Codes) :-
    ( atom(AtomOrCodes) ->
        atom_codes(AtomOrCodes, Codes)
    ;
        Codes = AtomOrCodes
    ).

deallocate_buffer(StatementHandle, Buffer) :-
    '$free_buffer'(StatementHandle, Buffer, ReturnCode),
    ( ReturnCode == 1 -> % PROLOG_NATIVE_CODE_SUCCESS
      true
    ; otherwise ->
      Goal = '$free_buffer'(StatementHandle, Buffer, ReturnCode),
      sp_throw_error(ReturnCode, Goal)
    ).

buffer_get_data(Buffer, StatementHandle, Term) :-
    '$buffer_get_data'(Buffer, StatementHandle, Term1, ReturnCode),
    ( ReturnCode > 0 ->
      Term = Term1
    ; otherwise ->
      Goal = '$buffer_get_data'(Buffer, StatementHandle, Term1, ReturnCode),
      buffer_throw_error(ReturnCode, Goal)
    ).

odbc_allocate_environment_handle(EnvHandle1) :-
    '$odbc_allocate_environment_handle'(HandleEnv, Result),
    Goal = '$odbc_allocate_environment_handle'(HandleEnv, Result),
    EnvHandle = environment_handle(HandleEnv), % odbc_check_return needs this
    odbc_check_return(EnvHandle, Result, Goal),
    EnvHandle1 = EnvHandle.

odbc_allocate_connection_handle(EnvHandle,ConnectionHandle1) :-
    '$odbc_allocate_connection_handle'(EnvHandle,HandleConn,Result),
    Goal = '$odbc_allocate_connection_handle'(EnvHandle,HandleConn,Result),
    ConnectionHandle = connection_handle(HandleConn), % odbc_check_return needs this
    odbc_check_return(ConnectionHandle, Result, Goal),
    ConnectionHandle1 = ConnectionHandle.

odbc_allocate_statement_handle(ConnectionHandle,StatementHandle1) :-
    '$odbc_allocate_statement_handle'(ConnectionHandle, HandleStmt, Result),
    Goal = '$odbc_allocate_statement_handle'(ConnectionHandle, HandleStmt, Result),
    StatementHandle = statement_handle(HandleStmt), % odbc_check_return needs this
    odbc_check_return(StatementHandle, Result, Goal),
    StatementHandle1 = StatementHandle.

% looks like a environment handle
is_environment_handle(EnvHandle) :-
    EnvHandle = environment_handle(RH), !,
    integer(RH).

% looks like a connection handle
is_connection_handle(Handle) :-
    Handle = connection_handle(RH),!,
    integer(RH).

valid_environment_handle(EnvHandle) :-
    is_environment_handle(EnvHandle).

valid_connection_handle(Handle) :-
    is_connection_handle(Handle).


valid_statement_handle(Handle) :-
    '$valid_statement_handle'(Handle).

must_be_environment_handle(EnvHandle, Goal) :-
    (
      valid_environment_handle(EnvHandle) -> true
    ;
      symbol_lookup('SQL_HANDLE_ENV', HandleType),
      E = odbc_error(invalid_handle('HandleType'-HandleType, 'Handle'-EnvHandle), Goal),
      throw(error(E,E))
    ).

must_be_connection_handle(Handle, Goal) :-
    (
      valid_connection_handle(Handle) -> true
    ;
      symbol_lookup('SQL_HANDLE_DBC', HandleType),
      E = odbc_error(invalid_handle('HandleType'-HandleType, 'Handle'-Handle), Goal),
      throw(error(E,E))
    ).

must_be_statement_handle(Handle, Goal) :-
    (
      valid_statement_handle(Handle) -> true
    ;
      symbol_lookup('SQL_HANDLE_STMT', HandleType),
      E = odbc_error(invalid_handle('HandleType'-HandleType, 'Handle'-Handle), Goal),
      throw(error(E,E))
    ).

valid_result_set(ResultSet) :-
    ResultSet = odbc_result_set(StatementHandle, _Cols, _Rows, _Descriptions,
                   _ParameterBindings), !,
    valid_statement_handle(StatementHandle).

must_be_result_set(ResultSet, Goal) :-
    must_be(ResultSet, nonvar, Goal, 0),
    (
      valid_result_set(ResultSet) -> true
    ;
      E = odbc_error(invalid_handle(result_set, ResultSet), Goal),
      throw(error(E,E))
    ).

%%%%%%%%%%%%%%%%%%%%%%%

%@  @item odbc_env_open(@var{-EnvHandle})
%@  @PLXindex {odbc_env_open/1 (odbc)}
%@  Opens an ODBC environment.
%@  Throws an exception if the environment could not be opened.

odbc_env_open(EnvHandle) :-
    odbc_env_open([odbc_version('SQL_OV_ODBC3')], EnvHandle).

%% Not exported yet.
%% When, and if, we allow other ODBC-versions than SQL_OV_ODBC3 it will
%% be exported.
% %@  @item odbc_env_open(@var{+Options},@var{-EnvHandle})
% %@  @PLXindex {odbc_env_open/2 (odbc)}
% %@  Like @code{odbc_env_open/1}, but also allows for options.
% %@  @var{Options} should be a list of zero or more of:
% %@  @table @code
% %@  @item odbc_version(@var{+Version})
% %@  @findex odbc_version/1 (odbc_env_open/2 option)
% %@  Determines whether certain functionality exhibits ODBC 2.x
% %@  behavior or ODBC 3.x behavior. Version may be one of the atoms
% %@  'SQL_OV_ODBC2' or 'SQL_OV_ODBC3'
% %@  The default is driver dependent.
% %@  @end table
% %@  Throws an exception if the environment could not be opened, or if
% %@  the version could not be set.

odbc_env_open(Options, EnvHandle) :-
    odbc_allocate_environment_handle(EnvHandle),
    Goal = odbc_env_open(Options, EnvHandle),
    ArgNo = 1,
    odbc_env_options(EnvHandle, Options, Goal, ArgNo).

odbc_env_options(EnvHandle, Options, Goal, ArgNo) :-
    must_be_environment_handle(EnvHandle, Goal),
    must_be(Options, proper_list(nonvar), Goal, ArgNo),
    odbc_env_options(EnvHandle, Options, Goal).

odbc_env_options(_EnvHandle, [], _Goal) :- !.
odbc_env_options(EnvHandle, [Option|Options], Goal) :-    
    (
      odbc_env_option(EnvHandle, Option, Goal) -> true
    ;
      E = odbc_error(unknown_env_option(Option), Goal),
      throw(error(E,E))
    ),
    odbc_env_options(EnvHandle, Options, Goal).

odbc_env_option(EnvHandle, odbc_version(VersionCode), Goal) :-
    (
      symbol_lookup(VersionCode, VersionNumber) -> true
    ;
      E = odbc_error(unknown_odbc_version(VersionCode), Goal),
      throw(error(E,E))
    ),
    symbol_lookup('SQL_ATTR_ODBC_VERSION', Attr),
    '$odbc_set_environment_attribute'(EnvHandle, Attr, VersionNumber, Result),
    odbc_check_return(EnvHandle, Result, Goal).


%@  @item odbc_db_open(@var{+Dbname},@var{+EnvHandle},@var{-ConnectionHandle})
%@  @PLXindex {odbc_db_open/3 (odbc)}
%@  Opens a database with the name @var{Dbname}. The database cannot require a
%@  username and a password.
%@  @var{ConnectionHandle} is an opaque handle for accessing the database.

odbc_db_open(Dbname, EnvHandle, ConnectionHandle) :-
    Goal = odbc_db_open(Dbname, EnvHandle, ConnectionHandle),
    must_be_environment_handle(EnvHandle, Goal),
    odbc_connect(Dbname, EnvHandle, ConnectionHandle).

%@  @item odbc_db_open(@var{+Dbname},@var{+EnvHandle},@var{+Options},@var{-ConnectionHandle})
%@  @PLXindex {odbc_db_open/4 (odbc)}
%@  Opens a database with the name @var{Dbname}.
%@  @var{Options} should be a list of zero or more of:
%@  @table @code
%@  @item username(@var{+Username})
%@  @findex username/1 (odbc_db_open/4 option)
%@  The username for connecting to the database.
%@  The default is @code{''}.
%@
%@  @item password(@var{+Password})
%@  @findex password/1 (odbc_db_open/4 option)
%@  The password for connection to the database.
%@  The default is @code{''}.
%@
%@  @item login_timeout(@var{+Timeout})
%@  @findex login_timeout/1 (odbc_db_open/4 option)
%@  The number of seconds to wait for a login request to complete.
%@  If 0 is used, the login attempt will wait indefinitely.
%@  The default is driver-dependent.
%@
%@  @item connection_timeout(@var{+Timeout})
%@  @findex connection_timeout/1 (odbc_db_open/4 option)
%@  The number of seconds to wait for any request on the connection to
%@  complete.
%@  If the Timeout value is 0 (the default), there is no timeout.
%@
%@  @item raw(@var{+ConnectionOptions})
%@  @findex raw/1 (odbc_db_open/4 option)
%@  @var{ConnectionOptions} should be a list of atoms. They are passed, terminated by @kbd{;}, as extra options when opening the database.
%@  @end table
%@  @var{ConnectionHandle} is an opaque handle for accessing the database.

odbc_db_open(Dbname, EnvHandle, Options, ConnectionHandle) :-
    odbc_db_open(Dbname, EnvHandle, Options, ConnectionHandle, _ConnectionString).

%@  @item odbc_db_open(@var{+Dbname},@var{+EnvHandle},@var{+Options},@var{-ConnectionHandle},@var{-ConnectionString})
%@  @PLXindex {odbc_db_open/5 (odbc)}
%@  Like @code{odbc_db_open/4} but also returns the completed connection string
%@  returned by the ODBC driver.

odbc_db_open(Dbname, EnvHandle, Options, ConnectionHandle, ConnectionString) :-
    Goal = odbc_db_open(Dbname, EnvHandle, Options, ConnectionHandle, ConnectionString),
    ArgNo = 5,
    must_be_environment_handle(EnvHandle, Goal),
    get_db_open_option(username(Username), Options, username(''), Options1),
    get_db_open_option(password(Password), Options1, password(''), Options2),
    get_db_open_option('raw'(Raw), Options2, 'raw'([]), Options3),
    odbc_driver_connect(Dbname, Username, Password, Raw, EnvHandle, Options3, Goal, ArgNo, ConnectionHandle, ConnectionString).

get_db_open_option(Option, OptionList, DefaultValue, OptionListRes) :-
    (  selectchk(Option, OptionList, OptionListRes) ->
        true
    ;  otherwise ->
        Option = DefaultValue,
        OptionListRes = OptionList
    ).


%@  @item odbc_query_open(@var{+ConnectionHandle}, @var{-StatementHandle})
%@  Creates a new database query. @var{ConnectionHandle} is a handle previously
%@  allocated with @code{odbc_db_open/[3,4,5]}.

odbc_query_open(ConnectionHandle, StatementHandle) :-
    Goal = odbc_query_open(ConnectionHandle, StatementHandle),
    must_be_connection_handle(ConnectionHandle, Goal),
    odbc_allocate_statement_handle(ConnectionHandle, StatementHandle).


%% Simple connection API using SQLConnect()
odbc_connect(DataSource, EnvHandle, ConnectionHandle) :-
    odbc_connect(DataSource, '', '', EnvHandle, ConnectionHandle).
odbc_connect(DataSource, UID, PassWord, EnvHandle, ConnectionHandle) :-
    odbc_allocate_connection_handle(EnvHandle, ConnectionHandle),
    Goal = '$odbc_connect'(ConnectionHandle, DataSource, UID, PassWord, Res),
    '$odbc_connect'(ConnectionHandle, DataSource, UID, PassWord, Res),
    odbc_check_return(ConnectionHandle, Res, Goal).

%%  SQLDriverConnect() is the most flexible way to establish a connection.
odbc_driver_connect(DataSource, UID, PassWord, Raw, EnvHandle, Options, Goal, ArgNo, ConnectionHandle, ComplConnStr1) :-
    tagged_connection_string_option('DSN', DataSource, DataSourceCodes),
    tagged_connection_string_option('UID', UID, UIDCodes),
    tagged_connection_string_option('PWD', PassWord, PassWordCodes),
    (
     foreach(R, Raw),
     foreach(E, Extras)
    do
     ensure_codes(R, Prop),
     append(Prop, ";", E)
    ),
    append([DataSourceCodes, UIDCodes, PassWordCodes|Extras],
           InConnectionString),
    odbc_allocate_connection_handle(EnvHandle, ConnectionHandle),
    odbc_connection_options(ConnectionHandle, Options, Goal, ArgNo),
    '$odbc_driver_connect'(ConnectionHandle, InConnectionString, ComplConnStr,
                           ResD),
    odbc_check_return(ConnectionHandle, ResD, Goal),
    ComplConnStr1 = ComplConnStr.

%% Code list of "TAG=VALUE;" or the empty list if Value is ''.
tagged_connection_string_option(_Tag, Value, Codes) :-
    nonvar(Value),
    Value == '', % memberchk(Value, ['', ""]),
    !,
    Codes = "".
tagged_connection_string_option(Tag, Value, Codes) :-
    ensure_codes(Tag, TagCodes),
    ensure_codes(Value, ValueCodes),
    append([TagCodes, "=", ValueCodes, ";"], Codes).


odbc_connection_options(ConnectionHandle, Options, Goal, ArgNo) :-
    must_be_connection_handle(ConnectionHandle, Goal),
    must_be(Options, proper_list(nonvar), Goal, ArgNo),
    odbc_connection_options(ConnectionHandle, Options, Goal).

odbc_connection_options(_ConnectionHandle, [], _Goal) :- !.
odbc_connection_options(ConnectionHandle, [Option|Options], Goal) :-
    (
      odbc_open_option(Option, ConnectionHandle, Goal) -> true
    ;
      E = odbc_error(unknown_connection_option(Options), Goal),
      throw(error(E,E))
    ),
    odbc_connection_options(ConnectionHandle, Options, Goal).

odbc_open_option(login_timeout(Timeout), ConnectionHandle, Goal) :-
    symbol_lookup('SQL_ATTR_LOGIN_TIMEOUT', Attr),
    '$odbc_set_connect_integer_attr'(ConnectionHandle, Attr, Timeout, Result),
    odbc_check_return(ConnectionHandle, Result, Goal).
odbc_open_option(connection_timeout(Timeout), ConnectionHandle, Goal) :-
    symbol_lookup('SQL_ATTR_CONNECTION_TIMEOUT', Attr),
    '$odbc_set_connect_integer_attr'(ConnectionHandle, Attr, Timeout, Result),
    odbc_check_return(ConnectionHandle, Result, Goal).         


%@  @item odbc_list_DSN(@var{+EnvHandle},@var{-DSNs})
%@  @PLXindex {odbc_list_DSN/2 (odbc)}
%@  @var{EnvHandle} is an opaque database handle.
%@  @var{DSNs} is unified with a list of all DSNs (Data Source Names).
%@  The list elements are X-Y where X is the DSN and Y its description.

odbc_list_DSN(EnvHandle, DSNs) :-
    Goal = odbc_list_DSN(EnvHandle, DSNs),
    must_be_environment_handle(EnvHandle, Goal),
    odbc_list_DSN1(EnvHandle, DSNs).

odbc_list_DSN1(EnvHandle, DSNs) :-
    symbol_lookup('SQL_FETCH_FIRST', DirectionFirst),
    odbc_list_DSN1(EnvHandle, [], DSNs, DirectionFirst).


odbc_list_DSN1(EnvHandle, DSNs, DSNs1, Direction) :-
    '$odbc_data_sources'(EnvHandle, Direction, DSN, Desc, Result),
    '$sql_return'(Result, ReturnCode, _),
    ( ReturnCode == 'SQL_SUCCESS' ->
        symbol_lookup('SQL_FETCH_NEXT', DirectionNext),
        odbc_list_DSN1(EnvHandle, [[DSN-Desc]|DSNs], DSNs1, DirectionNext)
    ; otherwise ->
        DSNs = DSNs1
    ).

%%%%%%%%%%%%%%%%%%%%%%%

% call like:
% odbc_list_data_types(StatementHandle, scratch(vehicle, wheels), DataTypes)

:- odbc_list_data_types(+, +, -) is det.
%@  @item odbc_list_data_types(@var{+StatementHandle}, @var{+TableDesc}, @var{-DataTypes})
%@  Makes a list of the datatypes in a table.
%@  @var{StatementHandle} is a handle previously allocated with @code{odbc_query_open/2}.
%@  @var{TableDesc} is a description of the table and its columns of the form
%@  @code{tablename(columnname1, columnname2, ..., columnnameN)}, or of the form
%@  @code{[tablename, columnname1, columnname2, ..., columnnameN]} (the latter
%@  form is useful if the table has more than 255 columns).
%@  @var{DataTypes} is unified with a list of the corresponding datatypes, i.e.
%@  on the form @code{[datatype1, datatype2, ... datatypeN]}.

odbc_list_data_types(StatementHandle, TableDesc, DataTypes) :-
    Goal = odbc_list_data_types(StatementHandle, TableDesc,
                                DataTypes),
    must_be_statement_handle(StatementHandle, Goal),
    must_be(TableDesc, compound, Goal, 2),
    ( is_list(TableDesc) ->     % [PM] 4.2 FIXME: do not use is_list/1
        TableDesc = [TableName | Cols]
    ; otherwise ->
        TableDesc =.. [TableName | Cols]
    ),
    listcolumns(StatementHandle, TableName, TypedCols),
    length(Cols, NCols),
    length(TypedCols, NTCols),
    ( NCols =\= NTCols ->
        E = odbc_error(consistency_error(NCols, NTCols, 'Number of columns not consistent with number of types'), Goal),
        throw(error(E,E))
    ;
        map_data_types(Cols, TypedCols, DataTypes,[])
    ).

map_data_types([], _TypedCols, DataTypes,DataTypes1) :-
   DataTypes = DataTypes1.
map_data_types([Col | Cols], TypedCols, [DT|DataTypes],DataTypes1) :-
    ( atom(Col) ->
        ColAtom = Col
    ;
        atom_codes(ColAtom, Col)
    ),
    select(ColAtom-DT, TypedCols, RestTypedCols), !,
    map_data_types(Cols, RestTypedCols, DataTypes,DataTypes1).



%% return a list of ColumnName-Typecode
listcolumns(StatementHandle, TableName, AllCols1) :-
    %% http://msdn.microsoft.com/en-us/library/ms711683(VS.85).aspx
    %% We need column 4, COLUMN_NAME, and column 5, DATA_TYPE
    %% The maximum columns name length can be very large on some OSes (i.e. OS X).

    Goal = '$odbc_columns'(StatementHandle, TableName, Res),
    % [PM] This is like odbc_exec_direct
    '$odbc_columns'(StatementHandle, TableName, Res),
    odbc_check_return(StatementHandle, Res, Goal),
    resultcolumns(StatementHandle, Cols),
    resultrows(StatementHandle, Rows),
    ( false ->
        describeandbindcolumns(Cols, StatementHandle, Descriptions)
    ; otherwise ->
        COLUMN_NAME_COLUMN = 4, % "COLUMN_NAME"
        DATA_TYPE_COLUMN = 5,   % "DATA_TYPE"
        describeandbindcolumn(StatementHandle, COLUMN_NAME_COLUMN, COLUMN_NAME_Description),
        describeandbindcolumn(StatementHandle, DATA_TYPE_COLUMN, DATA_TYPE_Description),
        Descriptions = [COLUMN_NAME_Description, DATA_TYPE_Description]
    ),
    ParameterBindings = [],
    ResultSet = odbc_result_set(StatementHandle, Cols, Rows, Descriptions, ParameterBindings),
    listcolumns1(ResultSet, [], AllCols),
    odbc_free_result_set(ResultSet), % does not free StatementHandle
    AllCols1 = AllCols.


listcolumns1(ResultSet, Rows, AllCols) :-
    odbc_sql_fetch(ResultSet, Row),
    ( Row == [] ->
        AllCols = Rows
    ;
        memberchk('DATA_TYPE'-DT, Row),
        memberchk('COLUMN_NAME'-CN, Row),
        atom_codes(ColName, CN),
        Rows1 = [ColName-DT | Rows],
        listcolumns1(ResultSet, Rows1, AllCols)
    ).

%% [PM] 4.2 All rows produced by SQLColumns
sqlcolumns_rows(ConnectionHandle, TableName, Rows) :-
   odbc_allocate_statement_handle(ConnectionHandle,StatementHandle),
   odbc_columns(StatementHandle, TableName, Rows1),
   odbc_free_statement_handle(StatementHandle),
   Rows = Rows1.

odbc_columns(StatementHandle, TableName, Rows1) :-
    %% http://msdn.microsoft.com/en-us/library/ms711683(VS.85).aspx
    Goal = '$odbc_columns'(StatementHandle, TableName, Res),
    % [PM] This is like odbc_exec_direct
    '$odbc_columns'(StatementHandle, TableName, Res),
    odbc_check_return(StatementHandle, Res, Goal),
    resultcolumns(StatementHandle, ColumnCount),
    resultrows(StatementHandle, RowCount),
    describeandbindcolumns(ColumnCount, StatementHandle, Descriptions),
    ParameterBindings = [],
    ResultSet = odbc_result_set(StatementHandle, ColumnCount, RowCount, Descriptions, ParameterBindings),
    odbc_sql_fetch_all(ResultSet, Rows),
    odbc_free_result_set(ResultSet), % does not free StatementHandle
    Rows1 = Rows.


:- odbc_current_table(+, ?) is nondet.
%@  @item odbc_current_table(+ConnectionHandle, ?TableName) @since{release 4.2}
%@  Enumerate the @emph{proper} tables in the database, i.e.@: tables
%@  with attribute @code{'TABLE_TYPE'("TABLE")}.
%@  
%@  @var{ConnectionHandle} is a handle previously allocated with @code{odbc_db_open/[3,4,5]}.
%@  @var{TableName} is the name, as an atom, of the table.
%@  
%@  Note that @code{odbc_current_table/2} may exit nondeterminately
%@  even if all arguments are instantiated when it is called.


odbc_current_table(ConnectionHandle, TableName) :-
   odbc_current_table1(ConnectionHandle, TableName, "TABLE", 'TABLE_NAME'(Name)),
   atom_codes(TableName, Name).


:- odbc_current_table(+, ?, ?) is nondet.
%@  @item odbc_current_table(+ConnectionHandle, ?TableName, ?Attribute) @since{release 4.2}
%@  Enumerate database tables and their attributes.
%@
%@  @var{ConnectionHandle} is a handle previously allocated with @code{odbc_db_open/[3,4,5]}.
%@  @var{TableName} is the name, as an atom, of the table.
%@  @var{Attribute} is an attribute of the table.
%@
%@  There are two kinds of attributes, @emph{derived} attributes and
%@  @emph{raw} attributes.
%@
%@  The @emph{derived} attributes are translations of raw attributes
%@  and other information and are in a format that is directly
%@  useful. There is currently only one derived attribute,
%@  @table @code
%@  
%@  @item arity(@var{Value})
%@  The number of columns in the table, as an integer.
%@  
%@  This attribute is always present.
%@  @end table
%@  @noindent
%@  the set of derived attributes may be extended in the future.
%@  
%@  The raw attributes correspond direcly to the (non-null) values
%@  returned from the ODBC function @code{SQLTables()} and are
%@  returned as is, wrapped in a functor with the same name as the
%@  attribute, e.g. @code{'TABLE_CAT'("foo")} would be returned for a
%@  table in the catalog "foo". Note that the names of the raw
%@  attributes are in all uppercase so you need to surround them with
%@  single quotes to prevent their name from being parsed as a
%@  variable. Some of the raw attributes are,
%@  @table @code
%@  
%@  @item 'TABLE_CAT'(@var{Value})
%@  Catalog name, as a code list. This attribute corresponds to the
%@  @code{TABLE_CAT} column, called @code{TABLE_QUALIFIER}
%@  in ODBC 2.0, as returned from the ODBC function @code{SQLTables()}.

%@  @item 'TABLE_TYPE'(@var{Value})
%@  Table type, as a code list. This attribute corresponds to the
%@  @code{TABLE_TYPE} column, as returned from the ODBC function
%@  @code{SQLTables()}. The standard table types are @code{"TABLE"},
%@  @code{"VIEW"}, @code{"SYSTEM TABLE"}, @code{"GLOBAL TEMPORARY"},
%@  @code{"LOCAL TEMPORARY"}, @code{"ALIAS"}, and @code{"SYNONYM"}, but
%@  there can be data-source-specific types as well.
%@  
%@  This attribute is always present.

%@  @item 'REMARKS'(@var{Value})
%@  Table descriptive text, as a code list. This attribute corresponds
%@  to the @code{REMARKS} column, as returned from the ODBC function
%@  @code{SQLTables()}.
%@  
%@  @end table

%@  @noindent
%@  see the ODBC documentation for @code{SQLTables()} for the full
%@  list of raw attributes and their meaning.

%@ 
%@  Note that @code{odbc_current_table/3} may exit nondeterminately
%@  even if one of more arguments are instantiated when it is called.
odbc_current_table(ConnectionHandle, TableName, Attribute) :-
   odbc_current_table1(ConnectionHandle, TableName, _TableTypeCodes, Attribute).


:- odbc_current_table1(+, ?, -, ?) is nondet.
odbc_current_table1(ConnectionHandle, TableName, TableTypeCodes, Attribute) :-
   % We could perhaps avoid sqltables_rows if TableName is input and
   % Attribute is arity(_) but only if we do not need to distinguish
   % empty tables from non-existing tables (table_arity/2 gives zero
   % for both cases).
   sqltables_rows(ConnectionHandle, Rows),
   ( atom(TableName) ->
       atom_codes(TableName, TABLE_NAME) % early filter
   ; true
   ),
   TABLE_TYPE = TableTypeCodes, % filter
   %% Ordered as in http://msdn.microsoft.com/en-us/library/ms711831(v=vs.85).aspx
   Row = row(TABLE_CAT, TABLE_SCHEM, TABLE_NAME, TABLE_TYPE, REMARKS),
   % The column names changed between ODBC 2.0 and 3.x so it seems
   % prudent to go by column number instead. They are all VARCHAR (and
   % the reference page does not rule out NULL for any of them which
   % is strange for some columns like TABLE_NAME).
   RowList = [_-TABLE_CAT,      % Was TABLE_QUALIFIER in ODBC 2.1
              _-TABLE_SCHEM,    % Was TABLE_OWNER in ODBC 2.1
              _-TABLE_NAME,
              _-TABLE_TYPE,
              _-REMARKS|_],
   member(RowList, Rows),             % BT gen
   atom_codes(TableName, TABLE_NAME), % late filter
   ( tables_row_attribute(Attribute, Row)
   ; table_attributes(Attribute, ConnectionHandle, TableName)
   ).

tables_row_attribute('TABLE_CAT'(Value), Row) :-
   arg(1, Row, TABLE_CAT),
   not_null(TABLE_CAT),
   %% TABLE_CAT \== [],
   Value = TABLE_CAT.
tables_row_attribute('TABLE_SCHEM'(Value), Row) :-
   arg(2, Row, TABLE_SCHEM),
   not_null(TABLE_SCHEM),
   %% TABLE_SCHEM \= [],
   Value = TABLE_SCHEM.
tables_row_attribute('TABLE_NAME'(Value), Row) :-
   arg(3, Row, TABLE_NAME),
   % assumed non-empty
   Value = TABLE_NAME.
tables_row_attribute('TABLE_TYPE'(Value), Row) :-
   arg(4, Row, TABLE_TYPE),
   % assumed non-empty
   Value = TABLE_TYPE.
tables_row_attribute('REMARKS'(Value), Row) :-
   arg(5, Row, REMARKS),
   not_null(REMARKS),
   Value = REMARKS.

:- table_attributes(?, +, +) is nondet.
table_attributes(arity(Arity), ConnectionHandle, TableName) :-
   table_arity(ConnectionHandle, TableName, Arity).


:- table_arity(+, +, -)  is det.
table_arity(ConnectionHandle, TableName, Arity) :-
   odbc_allocate_statement_handle(ConnectionHandle, StatementHandle),
   listcolumns(StatementHandle, TableName, AllCols),
   odbc_free_statement_handle(StatementHandle),
   length(AllCols, Arity).

:- odbc_table_column(+, ?, ?) is nondet.
%@  @item odbc_table_column(+ConnectionHandle, ?TableName, ?ColumnName) @since{release 4.2}
%@  Enumerate database table columns.
%@  
%@  @var{ConnectionHandle} is a handle previously allocated with @code{odbc_db_open/[3,4,5]}.
%@  @var{TableName} is the name, as an atom, of the table.
%@  @var{ColumnName} is the name, as an atom, of the table.
%@  
odbc_table_column(ConnectionHandle, TableName, ColumnName) :-
   TableTypeCodes = _Any, % Consider "TABLE" so that only proper tables are returned.
   odbc_table_column1(ConnectionHandle, TableName, ColumnName, TableTypeCodes, 'TABLE_NAME'(_)).


:- odbc_table_column(+, ?, ?, ?) is nondet.
%@  @item odbc_table_column(+ConnectionHandle, ?TableName, ?ColumnName, ?Attribute) @since{release 4.2}
%@  Enumerate database table columns and their attributes.
%@  
%@  @var{ConnectionHandle} is a handle previously allocated with @code{odbc_db_open/[3,4,5]}.
%@  @var{TableName} is the name, as an atom, of the table.
%@  @var{ColumnName} is the name, as an atom, of the table.
%@  @var{Attribute} is an attribute of the table.
%@
%@  There are two kinds of attributes, @emph{derived} attributes and
%@  @emph{raw} attributes.
%@

%@  The @emph{derived} attributes are translations of raw attributes
%@  and other information and are in a format that is directly
%@  useful. There is currently only one derived attribute,
%@  @table @code
%@  
%@  @item nullable(@var{Value})

%@  @code{true} if the column is definitely nullable, or @code{false}
%@  if the column is definitely not nullable. The value is derived
%@  from the raw attributes @code{NULLABLE} and @code{IS_NULLABLE},
%@  see the documentation for @code{SQLColumns()} for details.
%@  
%@  This attribute is not present if it can not be determined whether the column is nullable.
%@  @end table
%@  @noindent
%@  the set of derived attributes may be extended in the future.
%@  
%@  The raw attributes correspond direcly to the (non-null) values
%@  returned from the ODBC function @code{SQLColumns()} and are
%@  returned as is, wrapped in a functor with the same name as the
%@  attribute, e.g. @code{'TABLE_CAT'("foo")} would be returned for a
%@  column in a table in the catalog "foo". Note that the names of the
%@  raw attributes are in all uppercase so you need to surround them
%@  with single quotes to prevent their name from being parsed as a
%@  variable. Some of the raw attributes are,
%@  @table @code
%@  
%@  @item 'REMARKS'(@var{Value})
%@  Column descriptive text, as a code list. This attribute corresponds
%@  to the @code{REMARKS} column, as returned from the ODBC function
%@  @code{SQLColumns()}.
%@ 
%@  @item 'ORDINAL_POSITION'(@var{Value})
%@  The ordinal position of the column in the table, starting at 1. This attribute corresponds to the
%@  @code{ORDINAL_POSITION} column, as returned from the ODBC function @code{SQLColumns()}.
%@  
%@  This attribute is always present.

%@  
%@  @end table
%@  
%@  See the ODBC documentation for @code{SQLColumns()} for the full
%@  list of raw attributes and their meaning.
%@  
%@  Note that @code{odbc_table_column/4} may exit nondeterminately
%@  even if one of more arguments are instantiated when it is called.

odbc_table_column(ConnectionHandle, TableName, ColumnName, Attribute) :-
   TableTypeCodes = _Any,  % Consider "TABLE" so that only proper tables are returned.
   odbc_table_column1(ConnectionHandle, TableName, ColumnName, TableTypeCodes, Attribute).


:- odbc_table_column1(+, ?, ?, ?, ?) is nondet.
odbc_table_column1(ConnectionHandle, TableName, ColumnName, TableTypeCodes, Attribute) :-
   odbc_current_table(ConnectionHandle, TableName, 'TABLE_TYPE'(TableTypeCodes)), % enumerate existing tables
   sqlcolumns_rows(ConnectionHandle, TableName, Rows),
   % Ordered as in http://msdn.microsoft.com/en-us/library/ms711683(VS.85).aspx 
   Row = row(TABLE_CAT, TABLE_SCHEM, TABLE_NAME, COLUMN_NAME, DATA_TYPE,
       TYPE_NAME, COLUMN_SIZE, BUFFER_LENGTH, DECIMAL_DIGITS, NUM_PREC_RADIX,
       NULLABLE, REMARKS, COLUMN_DEF, SQL_DATA_TYPE,  SQL_DATETIME_SUB,
       CHAR_OCTET_LENGTH, ORDINAL_POSITION, IS_NULLABLE),
   ( atom(ColumnName) ->
       atom_codes(ColumnName, COLUMN_NAME) % early filter
   ; true
   ),
   % The column names changed between ODBC 2.0 and 3.x so it seems prudent to go by column number instead.
   RowList = [_-TABLE_CAT,      % VARCHAR. Was TABLE_QUALIFIER in ODBC 2.1
              _-TABLE_SCHEM,    % VARCHAR. Was TABLE_OWNER in ODBC 2.1
              _-TABLE_NAME,     % VARCHAR not NULL
              _-COLUMN_NAME,    % VARCHAR not NULL
              _-DATA_TYPE,      % SMALLINT not NULL
              _-TYPE_NAME,      % VARCHAR not NULL
              _-COLUMN_SIZE,    % INTEGER
              _-BUFFER_LENGTH,  % INTEGER
              _-DECIMAL_DIGITS, % SMALLINT
              _-NUM_PREC_RADIX, % SMALLINT
              _-NULLABLE,       % SMALLINT not NULL. One of SQL_NO_NULLS, SQL_NULLABLE, SQL_NULLABLE_UNKNOWN
              _-REMARKS,        % VARCHAR
              _-COLUMN_DEF,     % VARCHAR
              % The columns below appeared in ODBC 3.0
              _-SQL_DATA_TYPE,  % SMALLINT not NULL
              _-SQL_DATETIME_SUB, % SMALLINT
              _-CHAR_OCTET_LENGTH, % INTEGER
              _-ORDINAL_POSITION,  % INTEGER not NULL
              _-IS_NULLABLE        % VARCHAR "NO", "YES" or, as an extension to the ISO standard "" if nullability is unknown
             |_],
   member(RowList, Rows),             % BT gen
   atom_codes(ColumnName, COLUMN_NAME), % late filter
   ( columns_row_attribute(Attribute, Row)
   ; column_attributes(Attribute, ConnectionHandle, TableName, Row)
   ).

column_attributes(nullable(Value), _ConnectionHandle, _TableName, Row) :-
   columns_row_attribute('NULLABLE'(NULLABLE), Row),
   %% "The NULLABLE column indicates with certainty that a column can
   %% accept NULLs, but cannot indicate with certainty that a column
   %% does not accept NULLs. The IS_NULLABLE column indicates with
   %% certainty that a column cannot accept NULLs, but cannot indicate
   %% with certainty that a column accepts NULLs."
   'SQL_NULLABLE'(SQL_NULLABLE),
   ( NULLABLE == SQL_NULLABLE ->
       Value = true
   ; columns_row_attribute('IS_NULLABLE'(IS_NULLABLE), Row), % [PM] 4.2.2 may fail (if NULL)
     IS_NULLABLE == "NO" ->
       Value = false
   %% ; otherwise -> Value = unknown
   ).

% % sql.h: #define SQL_NULLABLE 1
% 'SQL_NULLABLE'(1).
'SQL_NULLABLE'(Value) :-
   symbol_lookup('SQL_NULLABLE', Value).

not_null(Value) :- Value \== null.

%% [PM] 4.2 FIXME: Add derived attributes with symbolic names etc.
columns_row_attribute('TABLE_CAT'(Value), Row) :-
   arg(1, Row, TABLE_CAT),  % VARCHAR. Was TABLE_QUALIFIER in ODBC 2.1
   not_null(TABLE_CAT),
   Value = TABLE_CAT.
columns_row_attribute('TABLE_SCHEM'(Value), Row) :-
   arg(2, Row, TABLE_SCHEM),    % VARCHAR. Was TABLE_OWNER in ODBC 2.1
   not_null(TABLE_SCHEM),
   Value = TABLE_SCHEM.
columns_row_attribute('TABLE_NAME'(Value), Row) :-
   arg(3, Row, TABLE_NAME),     % VARCHAR not NULL
   % not_null(TABLE_NAME),
   Value = TABLE_NAME.
columns_row_attribute('COLUMN_NAME'(Value), Row) :-
   arg(4, Row, COLUMN_NAME),    % VARCHAR not NULL
   % not_null(COLUMN_NAME),
   Value = COLUMN_NAME.
columns_row_attribute('DATA_TYPE'(Value), Row) :-
   arg(5, Row, DATA_TYPE),      % SMALLINT not NULL
   % not_null(DATA_TYPE),
   Value = DATA_TYPE.           % FIXME: translate
columns_row_attribute('TYPE_NAME'(Value), Row) :-
   arg(6, Row, TYPE_NAME),      % VARCHAR not NULL
   % not_null(TYPE_NAME),
   Value = TYPE_NAME.
columns_row_attribute('COLUMN_SIZE'(Value), Row) :-
   arg(7, Row, COLUMN_SIZE),    % INTEGER
   not_null(COLUMN_SIZE),
   Value = COLUMN_SIZE.
columns_row_attribute('BUFFER_LENGTH'(Value), Row) :-
   arg(8, Row, BUFFER_LENGTH),  % INTEGER
   not_null(BUFFER_LENGTH),
   Value = BUFFER_LENGTH.
columns_row_attribute('DECIMAL_DIGITS'(Value), Row) :-
   arg(9, Row, DECIMAL_DIGITS), % SMALLINT
   not_null(DECIMAL_DIGITS),
   Value= DECIMAL_DIGITS.
columns_row_attribute('NUM_PREC_RADIX'(Value), Row) :-
   arg(10, Row, NUM_PREC_RADIX), % SMALLINT
   not_null(NUM_PREC_RADIX),
   Value = NUM_PREC_RADIX.
columns_row_attribute('NULLABLE'(Value), Row) :-
   arg(11, Row, NULLABLE), % SMALLINT not NULL. One of SQL_NO_NULLS, SQL_NULLABLE, SQL_NULLABLE_UNKNOWN
   not_null(NULLABLE),
   Value = NULLABLE.
columns_row_attribute('REMARKS'(Value), Row) :-
   arg(12, Row, REMARKS),       % VARCHAR
   not_null(REMARKS),
   Value = REMARKS.
columns_row_attribute('COLUMN_DEF'(Value), Row) :-
   arg(13, Row, COLUMN_DEF),    % VARCHAR
   not_null(COLUMN_DEF),
   Value = COLUMN_DEF.
columns_row_attribute('SQL_DATA_TYPE'(Value), Row) :-
   arg(14, Row, SQL_DATA_TYPE), % SMALLINT not NULL
   % not_null(SQL_DATA_TYPE),
   Value = SQL_DATA_TYPE.
columns_row_attribute('SQL_DATETIME_SUB'(Value), Row) :-
   arg(15, Row, SQL_DATETIME_SUB), % SMALLINT
   not_null(SQL_DATETIME_SUB),
   Value = SQL_DATETIME_SUB.
columns_row_attribute('CHAR_OCTET_LENGTH'(Value), Row) :-
   arg(16, Row, CHAR_OCTET_LENGTH), % INTEGER
   not_null(CHAR_OCTET_LENGTH),
   Value = CHAR_OCTET_LENGTH.
columns_row_attribute('ORDINAL_POSITION'(Value), Row) :-
   arg(17, Row, ORDINAL_POSITION), % INTEGER not NULL
   not_null(ORDINAL_POSITION),
   Value = ORDINAL_POSITION.
columns_row_attribute('IS_NULLABLE'(Value), Row) :-
   % IS_NULLABLE == null happens for PostgreSQL.
   arg(18, Row, IS_NULLABLE),
   not_null(IS_NULLABLE),
   Value = IS_NULLABLE.

%% All rows produced by SQLTables
sqltables_rows(ConnectionHandle, Rows) :-
   odbc_allocate_statement_handle(ConnectionHandle,StatementHandle),
   odbc_tables(StatementHandle, Rows1),
   odbc_free_statement_handle(StatementHandle),
   Rows = Rows1.

odbc_tables(StatementHandle, Rows1) :-
    %% See http://msdn.microsoft.com/en-us/library/ms711831(v=vs.85).aspx
    TableName = '',             % no filtering
    Goal = '$odbc_tables'(StatementHandle, TableName, Res),
    % [PM] This is like odbc_exec_direct
    '$odbc_tables'(StatementHandle, TableName, Res),
    odbc_check_return(StatementHandle, Res, Goal),
    % resultcolumns(StatementHandle, ColumnCount),
    ColumnCount = 5, % [PM] 4.2 We only want the five standard columns
    resultrows(StatementHandle, RowCount),
    describeandbindcolumns(ColumnCount, StatementHandle, Descriptions),
    ParameterBindings = [],
    ResultSet = odbc_result_set(StatementHandle, ColumnCount, RowCount, Descriptions, ParameterBindings),
    odbc_sql_fetch_all(ResultSet, Rows),
    odbc_free_result_set(ResultSet), % does not free StatementHandle
    Rows1 = Rows.



%%% This gives a strange error when testing with MySQL. Data seems to sometimes
%%% not be converted from strings when writing to the database. So we won't use%
%% this method.
% %% Bind all parameters to strings (SQL_CHAR).
% bind_parameters(StatementHandle, ParamDataList, ParameterBindings) :-
%     bind_parameters2(StatementHandle, ParamDataList, 1, [], ParameterBindings).

%% Bind parameters according to the DataTypeList
bind_parameters(StatementHandle, ParamDataList, DataTypeList, ParameterBindings) :-
    bind_parameters1(StatementHandle, ParamDataList, DataTypeList, 1, [], ParameterBindings).

bind_parameters1(_StatementHandle, [], [], _ParamNum, ParameterBindings, ParameterBindings) :- !.
bind_parameters1(StatementHandle, [ParamData | PDs], [DataType | DTs], ParamNum, ParamBindAcc, ParameterBindings) :-
    bind_parameter(StatementHandle, ParamNum, 'SQL_PARAM_INPUT', DataType, ParamData, ParamBinding),
    ParamNum1 is ParamNum + 1,
    bind_parameters1(StatementHandle, PDs, DTs, ParamNum1, [ParamBinding | ParamBindAcc], ParameterBindings).


bind_parameter(StatementHandle, ParameterNumber, InputOutputType, SQLDataType,
               ParameterValue, ParameterBinding) :-
    Goal = bind_parameter(StatementHandle, ParameterNumber, InputOutputType, SQLDataType,
                          ParameterValue, ParameterBinding),
    ( integer(SQLDataType) ->
        ParameterType = SQLDataType
    ;
        symbol_lookup(SQLDataType, ParameterType)
    ),
    '$odbc_bind_parameter'(StatementHandle, ParameterNumber, ParameterType, ParameterValue, ParameterValueBufPtr, BindParamRes),
    odbc_check_return(StatementHandle, BindParamRes, Goal),
    ParameterValueBuffer = odbc_buffer(ParameterValueBufPtr),
    ParameterBinding = p(ParameterNumber, ParameterValueBuffer).


%% Parameters are converted according to parameter type list.

%@  @item odbc_query_execute_sql(@var{+StatementHandle}, @var{+SQLString}, @var{+ParamData}, @var{+ParamDataTypes}, @var{-ResultSet})
%@  Executes an SQL query.
%@  @var{StatementHandle} is a handle previously allocated with @code{odbc_query_open/2}.
%@  @var{SQLString} is the SQL statement to be executed. The statement string may
%@  contain parameter markers.
%@  @var{ParamData} is a list of data to be bound to the parameter markers.
%@  @var{ParamDataTypes} is a list of data types corresponding to the
%@  @var{ParamData} list.
%@  @var{ResultSet} is bound to an opaque data structure describing the result
%@  of the query.

%% Undocumented structure of the result set. DO NOT rely on this. It is
%% liable to change without notice.
%%
%% ResultSet = odbc_result_set(StatementHandle, Cols, Rows, Descriptions,
%%                             ParameterBindings)
%%     StatementHandle   = statement_handle(HandleStmt)
%%         HandleStmt = <native data>
%%     Cols              = <number of columns>
%%     Rows              = <number of rows>
%%     Descriptions      = [cd(Cols, ColumnName, DataType, ColumnBinding) | _]
%%         Cols          = <column number>
%%         ColumnName    = <column name>
%%         DataType      = <data type of column>
%%         ColumnBinding = odbc_column_binding(ColumnNumber, Buffer)
%%             ColumnNumber = <column number>
%%             Buffer       = <data buffer for this column>
%%     ParameterBindings = [p(ParameterNumber, ParameterValueBuffer) | _]
%%         ParameterNumber      = <parameter number>
%%         ParameterValueBuffer = <parameter value buffer>
%%
%% All buffers are of the form odbc_buffer(BufPtr),
%%                                 BufPtr  = <native buffer>

odbc_query_execute_sql(StatementHandle, SQLString, ParamData, ParamDataTypes,
                       ResultSet) :-
    Goal = odbc_query_execute_sql(StatementHandle, SQLString,
                                  ParamData, ParamDataTypes,
                                  ResultSet),
    bind_parameters(StatementHandle, ParamData, ParamDataTypes, ParameterBindings),
    odbc_query_execute_sql1(StatementHandle, SQLString, ParameterBindings, Goal,
                           ResultSet).

%% No parameter bindings. For SQL-statements without parameter markers.

%@  @item odbc_query_execute_sql(@var{+StatementHandle}, @var{+SQLString}, @var{-ResultSet})
%@  @var{StatementHandle} is a handle previously allocated with @code{odbc_query_open/2}.
%@  @var{SQLString} is the SQL statement to be executed.
%@  @var{ResultSet} is bound to an opaque data structure describing the result
%@  of the query.

odbc_query_execute_sql(StatementHandle, SQLString, ResultSet) :-
    Goal = odbc_query_execute_sql(StatementHandle, SQLString, ResultSet),
    odbc_query_execute_sql1(StatementHandle, SQLString, [], Goal, ResultSet).

odbc_query_execute_sql1(StatementHandle, SQLString, ParameterBindings, Goal,
                       ResultSet) :-
    must_be_statement_handle(StatementHandle, Goal),
    %% SQLString is accepted as either a code list or an atom.
    ( atom(SQLString) ->
        SQLString = SQLStringAtom
    ;
        atom_codes(SQLStringAtom, SQLString)
    ),
    '$odbc_exec_direct'(StatementHandle, SQLStringAtom, Res),
    odbc_check_return(StatementHandle, Res, Goal),
    resultcolumns(StatementHandle, Cols),
    resultrows(StatementHandle, Rows),
    describeandbindcolumns(Cols, StatementHandle, Descriptions),
    ResultSet = odbc_result_set(StatementHandle, Cols, Rows, Descriptions, ParameterBindings).

% [PM] 4.2.2 not yet documented/exported. Value is one of 'SQL_AUTOCOMMIT_ON' or 'SQL_AUTOCOMMIT_OFF'
odbc_set_autocommit(ConnectionHandle, Value) :-
    Goal = odbc_set_autocommit(ConnectionHandle, Value),
    symbol_lookup('SQL_ATTR_AUTOCOMMIT', SQL_ATTR_AUTOCOMMIT),
    symbol_lookup(Value, OptVal),
    '$odbc_set_connect_integer_attr'(ConnectionHandle, SQL_ATTR_AUTOCOMMIT, OptVal, Result),
    odbc_check_return(ConnectionHandle, Result, Goal).

% [PM] 4.2.2 not yet documented/exported
odbc_commit_transaction(ConnectionHandle) :-
    '$odbc_end_transaction'(ConnectionHandle, 1, Res),
     odbc_check_return(ConnectionHandle, Res, odbc_commit_transaction(ConnectionHandle)).

% [PM] 4.2.2 not yet documented/exported
odbc_rollback_transaction(ConnectionHandle) :-
    '$odbc_end_transaction'(ConnectionHandle, 0, Res),
     odbc_check_return(ConnectionHandle, Res, odbc_rollback_transaction(ConnectionHandle)).

%%%% This gives strange errors when testing with MySQL. Data seems to sometimes
%%%% not be converted from strings when writing to the database. So we won't use
%%%% this method.
% %% All parameters are converted to strings.

% %@  @item odbc_query_execute_sql(@var{+StatementHandle}, @var{+SQLString}, @var{+ParamData}, @var{-ResultSet})
% %@  @var{StatementHandle} is a handle previously allocated with @code{odbc_query_open/2}.
% %@  @var{SQLString} is the SQL statement to be executed. The statement string may
% %@  contain parameter markers.
% %@  @var{ParamData} is a list of data to be bound to the parameter markers.
% %@  The parameter data is converted to strings when bound.
% %@  @var{ResultSet} is bound to an opaque data structure describing the result
% %@  of the query.

% odbc_query_execute_sql(StatementHandle, SQLString, ParamData, ResultSet) :-
%     Goal = odbc_query_execute_sql(StatementHandle, SQLString, ParamData, ResultSet),
%     bind_parameters(StatementHandle, ParamData, ParameterBindings),
%     odbc_query_execute_sql1(StatementHandle, SQLString, ParameterBindings, Goal,
%                          ResultSet).

%@  @item odbc_sql_fetch(@var{+ResultSet}, @var{-Row})
%@  Fetch the next row from the result set.
%@  @var{ResultSet} is the result set from @code{odbc_query_execute_sql/[3,5]}.
%@  @var{Row} is unified with a non-empty list of data constituting a row in the result set,
%@  or with @code{[]} when there are no more rows.
%@  The elements in the @var{Row} are in the same order as in the corresponding query.

odbc_sql_fetch(ResultSet, Row) :-
    Goal = odbc_sql_fetch(ResultSet, Row),
    must_be_result_set(ResultSet, Goal),
    ResultSet = odbc_result_set(StatementHandle, _Cols, _Rows, _Descriptions,
                   _ParameterBindings),
    odbc_fetch(StatementHandle, RC),
    make_row(RC, ResultSet, Row).

% [PM] 4.2 Return Row elements in ResultSet order, i.e. in column order. (changed in 4.2)
make_row('SQL_NO_DATA', _ResultSet, Row) :- !,
   Row = [].
make_row(_, ResultSet, Row) :-
    ResultSet = odbc_result_set(StatementHandle, _Cols, _Rows, Descriptions,
                   _ParameterBindings),
    make_row1(Descriptions, StatementHandle, Row).

make_row1([], _StatementHandle, Row) :-
   Row = [].
make_row1([Desc | Descs], StatementHandle, Row) :-
    Desc = cd(_Cols, ColumnName, _DataType, ColumnBinding),
    ColumnBinding = odbc_column_binding(_ColumnNumber, Buffer),
    buffer_get_data(Buffer, StatementHandle, Data),
    Row = [ColumnName-Data | Row1],
    make_row1(Descs, StatementHandle, Row1).


% [PM] 4.2 FIXME: Make public?
odbc_sql_fetch_all(ResultSet, Rows) :-
    odbc_sql_fetch(ResultSet, Row),
    ( Row = [] ->
        Rows = []
    ; otherwise ->
        Rows = [Row|Rows1],
        odbc_sql_fetch_all(ResultSet, Rows1)
    ).

resultcolumns(StatementHandle, ColumnCount1) :-
    Goal = '$odbc_num_result_cols'(StatementHandle, ColumnCount, Res4),
    '$odbc_num_result_cols'(StatementHandle, ColumnCount, Res4),
    odbc_check_return(StatementHandle, Res4, Goal),
    ColumnCount1 = ColumnCount.

resultrows(StatementHandle, RowCount1) :-
    Goal = '$odbc_row_count'(StatementHandle, RowCount, Res5),
    '$odbc_row_count'(StatementHandle, RowCount, Res5),
    odbc_check_return(StatementHandle, Res5, Goal),
    RowCount1 = RowCount.


describeandbindcolumns(Cols, StatementHandle, Descriptions) :-
    describeandbindcolumns1(Cols, StatementHandle, Descriptions,[]).

describeandbindcolumns1(0, _StatementHandle, Desc,Desc1) :- !,
    Desc = Desc1.
describeandbindcolumns1(Cols, StatementHandle, Desc,Desc1) :-
    describeandbindcolumn(StatementHandle, Cols, Description),
    Desc2 = [Description | Desc1],
    Col is Cols - 1,
    describeandbindcolumns1(Col, StatementHandle, Desc,Desc2).


describeandbindcolumn(StatementHandle, ColumnNumber, Description) :-
    describeandbindcolumn(StatementHandle, ColumnNumber, ColumnName, DataType, ColumnBinding),
    Description = cd(ColumnNumber, ColumnName, DataType, ColumnBinding).


describeandbindcolumn(StatementHandle, ColumnNumber, ColumnName, DataType, ColumnBinding) :-
    Goal = describeandbindcolumn(StatementHandle, ColumnNumber, ColumnName, DataType, ColumnBinding),
    '$odbc_describe_and_bind_column'(StatementHandle, ColumnNumber, ColumnName1, DataType1, BufPtr, Res),
    odbc_check_return(StatementHandle, Res, Goal),
    ColumnName = ColumnName1,
    DataType = DataType1,
    ColumnBinding = odbc_column_binding(ColumnNumber, odbc_buffer(BufPtr)).


%@  @item odbc_query_close(@var{+Query})
%@  Closes the query represented by @var{Query}, which can be either a
%@  result set, e.g. as returned from @code{odbc_query_execute_sql/[3,5]},
%@  or it can be a statement handle, as returned from
%@  @code{odbc_query_open/2}.
%@  

odbc_query_close(Query) :-
    Goal = odbc_query_close(Query),
    ( nonvar(Query), Query = statement_handle(_) ->
        StatementHandle = Query
    ; otherwise ->
        ResultSet = Query,
        must_be_result_set(ResultSet, Goal),
        odbc_free_result_set(ResultSet),
        ResultSet = odbc_result_set(StatementHandle, _Cols, _Rows, _Descriptions, _ParamBindings)
    ),
    odbc_free_statement_handle(StatementHandle).

% Unbind statement handle and free description and parameter buffers. Does not free statememt handle
odbc_free_result_set(ResultSet) :-
    ResultSet = odbc_result_set(StatementHandle, _Cols, _Rows, Descriptions, ParamBindings),
    unbind_statement(StatementHandle),
    % [PM] 4.2 deallocate buffers before statement handle (but after unbind_statement)
    deallocate_column_buffers(StatementHandle, Descriptions),
    deallocate_parameter_buffers(StatementHandle, ParamBindings).


odbc_free_statement_handle(StatementHandle) :-
    Goal = '$odbc_free_statement_handle'(StatementHandle, Res),
    '$odbc_free_statement_handle'(StatementHandle, Res),
    odbc_check_return(StatementHandle, Res, Goal).


deallocate_column_buffers(_StatementHandle, []) :- !.
deallocate_column_buffers(StatementHandle, [Desc | Descs]) :-
    Desc = cd(_Cols, _ColumnName, _DataType, ColumnBinding),
    ColumnBinding = odbc_column_binding(_ColumnNumber, Buffer),
    deallocate_buffer(StatementHandle, Buffer),
    deallocate_column_buffers(StatementHandle, Descs).

deallocate_parameter_buffers(_StatementHandle, []) :- !.
deallocate_parameter_buffers(StatementHandle, [ParamBinding | ParamBindings]) :-
    ParamBinding = p(_ParameterNumber, ParameterValuePtr),
    deallocate_buffer(StatementHandle, ParameterValuePtr),
    deallocate_parameter_buffers(StatementHandle, ParamBindings).

unbind_statement(StatementHandle) :-
    odbc_free_statement(StatementHandle, 'SQL_CLOSE'),
    odbc_free_statement(StatementHandle, 'SQL_UNBIND'),
    odbc_free_statement(StatementHandle, 'SQL_RESET_PARAMS').

odbc_free_statement(StatementHandle, Option) :-
    symbol_lookup(Option, OptVal),
    '$odbc_free_statement'(StatementHandle, OptVal, Result),
    Goal = '$odbc_free_statement'(StatementHandle, OptVal, Result),
    odbc_check_return(StatementHandle, Result, Goal).

%@  @item odbc_db_close(@var{+ConnectionHandle})
%@  Closes the connection to the database.

odbc_db_close(ConnectionHandle) :-
    Goal = odbc_db_close(ConnectionHandle),
    must_be_connection_handle(ConnectionHandle, Goal),
    odbc_disconnect(ConnectionHandle).

odbc_disconnect(ConnectionHandle) :-
    '$odbc_disconnect'(ConnectionHandle, ResDiscConn),
    Goal = '$odbc_disconnect'(ConnectionHandle, ResDiscConn),
    odbc_check_return(ConnectionHandle, ResDiscConn, Goal),
    Goal1 = '$odbc_free_connection_handle'(ConnectionHandle, ResConn),
    '$odbc_free_connection_handle'(ConnectionHandle, ResConn),
    odbc_check_return(ConnectionHandle, ResConn, Goal1).


%@  @item odbc_env_close(@var{+EnvHandle})
%@  Frees the environment handle.

odbc_env_close(EnvHandle) :-
    Goal = odbc_env_close(EnvHandle),
    must_be_environment_handle(EnvHandle, Goal),
    '$odbc_free_environment_handle'(EnvHandle, ResEnv),
    odbc_check_return(EnvHandle, ResEnv, Goal).


odbc_fetch(StatementHandle, RC) :-
    Goal = '$odbc_fetch'(StatementHandle, Res),
    '$odbc_fetch'(StatementHandle, Res),
    odbc_check_return(StatementHandle, Res, Goal),
    '$sql_return'(Res,RC, _).

%% Not used but perhaps useful for debugging.
:- public odbc_getinfo/3.
odbc_getinfo(ConnectionHandle, InfoType, InfoValue1) :-
    symbol_lookup(InfoType, InfoTypeNum),
    Goal = '$odbc_getinfo'(ConnectionHandle, InfoTypeNum, InfoValue, Res),
    '$odbc_getinfo'(ConnectionHandle, InfoTypeNum, InfoValue, Res),
    odbc_check_return(ConnectionHandle, Res, Goal),
    InfoValue1 = InfoValue.


symbol_lookup(Symbol, Value) :-
    Goal = '$symbol_lookup'(Symbol, Value, RetCode),
    '$symbol_lookup'(Symbol, Value, RetCode),
    sp_check_return(RetCode, Goal).% PROLOG_NATIVE_CODE_...

%%% Foreign declarations

foreign(odbc_allocate_environment_handle,
        '$odbc_allocate_environment_handle'(-integer, [-integer])).
foreign(odbc_allocate_connection_handle,
        '$odbc_allocate_connection_handle'(+term, -integer, [-integer])).
foreign(odbc_allocate_statement_handle,
        '$odbc_allocate_statement_handle'(+term, -integer, [-integer])).
foreign(valid_statement_handle,
        '$valid_statement_handle'(+term)).
foreign(odbc_set_environment_attribute,
        '$odbc_set_environment_attribute'(+term, +integer, +integer,
                                          [-integer])).
foreign(odbc_free_statement,
        '$odbc_free_statement'(+term, +integer, [-integer])).
foreign(odbc_free_environment_handle,
        '$odbc_free_environment_handle'(+term, [-integer])).
foreign(odbc_free_connection_handle,
        '$odbc_free_connection_handle'(+term, [-integer])).

foreign(odbc_free_statement_handle,
        '$odbc_free_statement_handle'(+term, [-integer])).
foreign(odbc_data_sources, % Unicode
        '$odbc_data_sources'(+term, +integer, -string,
                             -string, [-integer])).
foreign(odbc_set_connect_integer_attr,
        '$odbc_set_connect_integer_attr'(+term, +integer, +integer,
                                         [-integer])).
foreign(odbc_columns, % Unicode
	'$odbc_columns'(+term, +string, [-integer])).
foreign(odbc_tables, % Unicode
	'$odbc_tables'(+term, +string, [-integer])).

foreign(odbc_connect, % Unicode
	'$odbc_connect'(+term, +string, +string, +string,
                                      [-integer])).
foreign(odbc_driver_connect, % Unicode
        '$odbc_driver_connect'(+term, +codes, -string, [-integer])).
foreign(odbc_get_diag_rec, % Unicode
        '$odbc_get_diag_rec'(+integer, +term, +integer, -string,
                             -integer, -string, -integer, [-integer])).
foreign(odbc_bind_parameter,
        '$odbc_bind_parameter'(+term, +integer, +integer,
                               +term, -integer, [-integer])).
foreign(free_buffer,
        '$free_buffer'(+term, +term, [-integer])).
foreign(odbc_describe_and_bind_column, % Unicode
        '$odbc_describe_and_bind_column'(+term /* StatementHandle */,
                                         +integer /* ColumnNumber */,
                                         -string /* ColumnName */,
                                         -integer /* DataType */,
                                         %% -integer /* ColumnSize */,
                                         -integer, /* BufPtr */
                                        [-integer])).
foreign(odbc_exec_direct, % Unicode
        '$odbc_exec_direct'(+term, +string, [-integer])).
foreign(odbc_disconnect,
        '$odbc_disconnect'(+term, [-integer])).
foreign(odbc_getinfo,
        '$odbc_getinfo'(+term, +integer, -term, [-integer])).
% The argument is 1 for SQL_COMMIT and 0 for SQL_ROLLBACK
foreign(odbc_end_transaction,
        '$odbc_end_transaction'(+term, +integer, [-integer])).
foreign(odbc_num_result_cols,
        '$odbc_num_result_cols'(+term, -integer, [-integer])).
foreign(odbc_row_count,
        '$odbc_row_count'(+term, -integer, [-integer])).
foreign(odbc_fetch,
        '$odbc_fetch'(+term, [-integer])).
foreign(buffer_get_data,
        '$buffer_get_data'(+term, +term, -term, [-integer])).
foreign(symbol_lookup, % Unicode
        '$symbol_lookup'(+string, -integer, [-integer])).
foreign(sql_return, % Unicode
        '$sql_return'(+term, -atom, [-integer])).
foreign_resource(odbc,
                 [odbc_allocate_environment_handle,
                  odbc_allocate_connection_handle,
                  odbc_allocate_statement_handle,
                  valid_statement_handle,
                  odbc_set_environment_attribute,
                  odbc_free_environment_handle,
                  odbc_free_connection_handle,
                  odbc_free_statement_handle,
                  odbc_free_statement,
                  odbc_data_sources,
                  odbc_set_connect_integer_attr,
                  odbc_columns,
                  odbc_tables,
                  odbc_connect,
                  odbc_driver_connect,
                  odbc_get_diag_rec,
                  odbc_bind_parameter,
                  free_buffer,
                  odbc_describe_and_bind_column,
                  odbc_exec_direct,
                  odbc_disconnect,
                  odbc_getinfo,
                  odbc_end_transaction,
                  odbc_num_result_cols,
                  odbc_row_count,
                  odbc_fetch,
                  buffer_get_data,
                  symbol_lookup,
                  sql_return,
                  init(odbc_init),
                  deinit(odbc_deinit)]).
:- load_foreign_resource(library(system(odbc))).
%@  @end table
