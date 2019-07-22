/* Copyright (C) 1999, Swedish Institute of Computer Science. */

%   File       : bdb.pl
%   Author     : Tamas Benko
%   Updated    : 29 August 2000
%   Purpose    : An interface to Berkeley DB

:- module(bdb, [
	db_open_env/2, 
	db_open_env/3,
	db_close_env/1, 
	db_current_env/2,
	db_open/4, 
	db_open/5, 
	db_close/1, 
	db_current/5,
	db_store/3, 
	db_fetch/3, 
	db_erase/2, 
	db_erase/3,
	db_enumerate/3,
	db_findall/5,
	db_compress/2, 
	db_compress/3,
	db_sync/1,
	db_make_iterator/2, 
	db_make_iterator/3,
	db_iterator_next/3, 
	db_iterator_done/1,
	db_current_iterator/3,
	db_export/2,		% [PD] 3.12.1
	db_export/3,		% [PD] 3.12.1
	db_import/2,		% [PD] 3.12.1
	db_import/3		% [PD] 3.12.1
	]).

:- use_module(library(types), [
	must_be/4,
	illarg/3,
	illarg/4
	]).

:- use_module(library(fastrw), [
	fast_buf_read/2,
	fast_buf_write/3
	]).

:- use_module(library(file_systems), [
        directory_exists/1,
        make_directory/1,
	delete_file/1,
	file_exists/1
        ]).

:- meta_predicate
        db_findall(+, +, +, 0, -).

:- meta_predicate
        db_findall_cont(+, +, +, 0, -),
        unify_call(+, +, 0).

:- dynamic
	'$db_env'/2,
	'$db'/5,
	'$db_it'/2,
	'$db_it'/3,
	'$first_error'/2,
	'$db_specs'/4.

:- volatile
        '$db_env'/2,
        '$db'/5,
        '$db_it'/2,
        '$db_it'/3,
        '$first_error'/2,
        '$db_specs'/4.


%@  This library module handles storage and retrieval of terms
%@  on files.  By using indexing, the store/retrieve operations are
%@  efficient also for large data sets.  The package is an interface to the
%@  Berkeley DB toolset.
%@  
%@  @menu
%@  * BDB Basics:: Basics
%@  * Current Limitations:: Current Limitations
%@  * Berkeley DB:: Berkeley DB
%@  * The DB-Spec Informal Description:: The DB-Spec---Informal Description
%@  * Predicates:: Predicates
%@  * An Example Session:: An Example Session
%@  * The DB-Spec:: The DB-Spec
%@  * Exporting and importing a database:: Exporting and importing a database
%@  @end menu
%@  
%@  @node BDB Basics
%@  @subsection Basics
%@  
%@  The idea is to get a behavior similar to @code{assert/1},
%@  @code{retract/1} and @code{clause/2}, but the terms are stored on
%@  files instead of in primary memory.
%@  
%@  The differences compared with the Prolog database are:
%@  
%@  @itemize @bullet
%@  @item
%@  @cindex database
%@  A @dfn{database} must be opened before any access and closed after the
%@  last access. (There are special predicates for this:
%@  @code{db_open/[4,5]} and @code{db_close/1}.)
%@  
%@  @item
%@  The functors and the indexing specifications of the terms to
%@  be stored have to be given when the database is
%@  created. (@pxref{The DB-Spec}).
%@  
%@  @item
%@  The indexing is specified when the database is created.  It is
%@  possible to index on other parts of the term than just the
%@  functor and first argument.
%@  
%@  @item
%@  Changes affect the database immediately.
%@  
%@  @item
%@  The database will store variables with attributes or with blocked
%@  goals as ordinary variables.
%@  @end itemize
%@  
%@  Some commercial databases can't store non-ground terms
%@  or more than one instance of a term.  This library module
%@  can however store terms of either kind.
%@  
%@  @node Current Limitations
%@  @subsection Current Limitations
%@  
%@  @itemize @bullet
%@  @item
%@  The terms are not necessarily fetched in the same order as they
%@  were stored.
%@  
%@  @item
%@  If the process dies during an update operation (@code{db_store/3},
%@  @code{db_erase/[2,3]}), the database can be inconsistent.
%@  
%@  @item
%@  Databases can only be shared between processes running on the
%@  machine where the environment is created (@pxref{Predicates}).  The
%@  database itself can be on a different machine.
%@  
%@  @item
%@  The number of terms ever inserted in a database cannot
%@  exceed 2^32-1.
%@  
%@  @item
%@  Duplicate keys are not handled efficiently by Berkeley DB.  This
%@  limitation is supposed to get lifted in the future.  Duplicate keys can
%@  result from indexing on non-key attribute sets, inserting terms
%@  with variables on indexing positions, or simply from storing the
%@  same term more than once.
%@  @end itemize
%@  
%@  @node Berkeley DB
%@  @subsection Berkeley DB
%@  This library module is an interface to the Berkeley DB toolset to
%@  support persistent storage of Prolog terms.  Some of the notions
%@  of Berkeley DB are directly inherited, e.g.@: the environment.
%@  
%@  The interface uses the Concurrent Access Methods product of Berkeley DB.
%@  This means that multiple processes can open the same database, but
%@  transactions and disaster recovery are not supported.
%@  
%@  
%@  @c [PD] 4.0 We now use the default hash function.
%@  @c @c [PM] 3.10.2 SPRM 4917 (We should use the default hash function)
%@  @c The environment and the database files are ordinary Berkeley DB
%@  @c entities but uses a custom hash function which means that most, but not
%@  @c all, of the standard support utilities will work.
%@  The environment and the database files are ordinary Berkeley DB entities
%@  which means that the standard support utilities (e.g.@: @code{db_stat})
%@  will work.
%@  
%@  
%@  @node The DB-Spec Informal Description
%@  @subsection The DB-Spec---Informal Description
%@  
%@  @cindex db-spec
%@  The @dfn{db-spec} defines which functors are allowed and which
%@  parts of a term are used for indexing in a database.  The
%@  syntax of a db-spec is a skeletal goal with no module.  The
%@  db-spec is a list of atoms and compound terms where
%@  the arguments are either @code{+} or @code{-}.  A term can
%@  be inserted in the database if there is a spec in the spec
%@  list with the same functor.
%@  
%@  Multilevel indexing is not supported, terms have to be
%@  ``flattened''.
%@  
%@  @cindex indexed term
%@  @cindex term, indexed
%@  Every spec with the functor of the @dfn{indexed term} specifies an
%@  indexing.  Every argument where there is a @code{+} in the spec is
%@  indexed on.
%@  
%@  The idea of the db-spec is illustrated with a few examples.  (A section
%@  further down explains the db-spec in a more formal way).
%@  
%@  Given a spec of @code{[f(+,-), .(+,-), g, f(-,+)]} the indexing works
%@  as follows.  (The parts with indexing are underlined.)
%@  
%@  @multitable { mmmmmmmm } { domain error mmmmmmmmmmmmmmmm } { instantiation error }
%@  @item @var{Term}               @tab @var{Store}                      @tab @var{Fetch}
%@  @item 
%@  @item @code{g(x,y)}              @tab domain error              @tab domain error
%@  @item 
%@  @item @code{f(A,B)}       @tab @code{f(A,B)}                    @tab instantiation error
%@  @item                     @tab @code{-}
%@  @item 
%@  @item @code{f(a,b)}       @tab @code{f(a,b)  f(a,b)}            @tab @code{f(a,b)}
%@  @item                     @tab @code{- -     -   -}             @tab @code{- -}
%@  @item 
%@  @item @code{[a,b]}        @tab @code{.(a,.(b,[]))}              @tab @code{.(a,.(b,[]))}
%@  @item                     @tab @code{- -}                       @tab @code{- -}
%@  @item 
%@  @item @code{g}            @tab @code{g}                         @tab @code{g}
%@  @item                     @tab @code{-}                         @tab @code{-}
%@  @end multitable
%@  
%@  The specification @code{[f(+,-), f(-,+)]} is different from
%@  @code{[f(+,+)]}.  The first specifies that two indices are to be made
%@  whereas the second specifies that only one index is to be made on both
%@  arguments of the term.
%@  
%@  @node Predicates
%@  @subsection Predicates
%@  
%@  @menu
%@  * Conventions:: Conventions
%@  * The Environment:: The Environment
%@  * Memory Leaks:: Memory Leaks
%@  * The Predicates:: The Predicates
%@  @end menu
%@  
%@  
%@  @node Conventions
%@  @subsubsection Conventions
%@  The following conventions are used in the predicate descriptions
%@  below.
%@  @itemize @bullet
%@  @item
%@  @var{Mode} is either @code{update} or @code{read} or @code{enumerate}.
%@  In mode @code{read} no updates can be made.  Mode @code{enumerate} is
%@  like mode @code{read}, but indexing cannot be used, i.e.@: you can
%@  only sequentially enumerate the items in the database.  In mode
%@  @code{enumerate} only the file storing the terms along with their
%@  references is used.
%@  
%@  @item
%@  @var{EnvRef} is a reference to an open database environment.  The
%@  environment is returned when it is opened.  The reference becomes
%@  invalid after the environment has been closed.
%@  
%@  @item
%@  @var{DBRef} is a reference to an open database.  The reference is
%@  returned when the database is opened.  The reference becomes
%@  invalid after the database has been closed.
%@  
%@  @item
%@  @var{TermRef} is a reference to a term in a given database.
%@  The reference is returned when a term is stored.  The reference
%@  stays valid even after the database has been closed and hence can
%@  be stored permanently as part of another term.  However, if such
%@  references are stored in the database, automatic compression of
%@  the database (using @code{db_compress/[2,3]}) is not possible, in
%@  that case the user has to write her own compressing predicate.
%@  
%@  @item
%@  @var{SpecList} is a description of the indexing scheme;
%@  @pxref{The DB-Spec}.
%@  
%@  @item
%@  @var{Term} is any Prolog term.
%@  
%@  @item
%@  @var{Iterator} is a non-backtrackable mutable object.  It can be
%@  used to iterate through a set of terms stored in a database.
%@  The iterators are unidirectional.
%@  @end itemize
%@  
%@  @node The Environment
%@  @subsubsection The Environment
%@  @cindex environment
%@  To enable sharing of databases between process, programs
%@  have to create @dfn{environments} and the databases should be
%@  opened in these environments.  A database can be shared between
%@  processes that open it in the same environment.  An environment
%@  physically consists of a directory containing the files needed to enable
%@  sharing databases between processes.  The directory of the
%@  environment has to be located in a local file system.
%@  
%@  Databases can be opened outside any environment (see
%@  @code{db_open/4}), but in that case a process writing the database
%@  must ensure exclusive access or the behavior of the predicates is
%@  undefined.
%@  
%@  @node Memory Leaks
%@  @subsubsection Memory Leaks
%@  In order to avoid memory leaks, environments, databases and
%@  iterators should always be closed explicitly.  Consider using
%@  @code{call_cleanup/2} to automate the closing/deallocation of these
%@  objects.  You can always use @code{db_current_env/1},
%@  @code{db_current/5} and @code{db_current_iterator/3} to enumerate the
%@  currently living objects.
%@  
%@  @quotation
%@  @strong{Please note}: a database must not be closed while there are outstanding
%@  choices for some @code{db_fetch/3} goal that refers to that
%@  database.  Outstanding choices can be removed with a cut
%@  (@code{!}).
%@  @end quotation
%@  
%@  @node The Predicates
%@  @subsubsection The Predicates
%@  @table @code

%@  @item db_open_env(@var{+EnvName}, @var{-EnvRef})
%@  @itemx db_open_env(@var{+EnvName}, @var{+CacheSize}, @var{-EnvRef})
%@  @PLXindex {db_open_env/[2,3] (bdb)}
%@  Opens an environment with the name @var{EnvName}.  A directory with this
%@  name is created for the environment if necessary.
%@  @c [PD] 4.0.5 Now it is subject to @code{absolute_file_name/3} conversion.
%@  @c @var{EnvName} is not
%@  @c subject to @code{absolute_file_name/3} conversion.
%@  
%@  By using @code{db_open_env/3} one can specify the size of the cache:
%@  @var{CacheSize} is the (integer) size of the cache in kilobytes.  The
%@  size of the cache cannot be less than 20 kilobytes.
%@  @code{db_open_env/2} will create a cache of the system's default size.
%@  
%@  The size of the cache is determined when the environment is created and
%@  cannot be changed by future openings.
%@  
%@  A process cannot open the same environment more than once.

db_open_env(EnvName, EnvRef) :-
        db_open_env(EnvName, 0, EnvRef).

db_open_env(EnvName0, CacheSize, EnvRef) :-
	ErrGoal = db_open_env(EnvName, CacheSize, EnvRef),
        % [PM] 4.3 consider avoiding case-normalization here. Beware: case-sensitive sub-path matching is done and can easily go wrong if we change this.
	absolute_file_name(EnvName0, EnvName, []),
	(   '$db_env'(_, EnvName) ->
	    illarg(permission(open,environment,'already open'), ErrGoal, 1, EnvName0)
	;   true
	),
	%% [PD] 4.0.5 Do all file system stuff in Prolog instead of in C code.
	(   directory_exists(EnvName) ->
	    true
	;
	    (   make_directory(EnvName) ->
		true
	    ;
		illarg(permission(open,envname,'cannot create directory'), ErrGoal, 1, EnvName0)
	    )	    
	),
	c_open_env(EnvName, CacheSize, EnvAddr, R),
	db_error_chk(R, open_env),
	EnvRef = '$db_env'(EnvAddr),
	assertz('$db_env'(EnvAddr, EnvName)).

%@  @item db_close_env(@var{+EnvRef})
%@  @PLXindex {db_close_env/1 (bdb)}
%@  Closes an environment.  All databases opened in the environment
%@  will be closed as well.

db_close_env(EnvRef) :-
	ErrGoal = db_close_env(EnvRef),
	must_be_open_env(EnvRef, EnvAddr, CRef, ErrGoal, 1),
	close_all_dbs_in_env(EnvAddr),
	c_close_env(EnvAddr, R),
	db_error_chk(R, close_env),
	erase(CRef).

close_all_dbs_in_env(EnvAddr) :-
	'$db'(DBAddr, EnvAddr, _N, _M, _S),
	on_exception(E, db_close_int(DBAddr),
		     asserta_if_ok(close_all_dbs_in_env, E)),
	fail.
close_all_dbs_in_env(_) :-
	retract('$first_error'(close_all_dbs_in_env, E)), !,
	throw(E).
close_all_dbs_in_env(_).

%@  @item db_current_env(@var{?EnvName}, @var{?EnvRef})
%@  @PLXindex {db_current_env/2 (bdb)}
%@  Unifies the arguments with the open environments.  This
%@  predicate can be used for enumerating all currently open
%@  environments through backtracking.
:- db_current_env/2 is nondet.
db_current_env(EnvName, '$db_env'(EnvAddr)) :-
	'$db_env'(EnvAddr, EnvName).

%@  @item db_open(@var{+DBName}, @var{+Mode}, @var{?SpecList}, @var{-DBRef})
%@  @itemx db_open(@var{+DBName}, @var{+Mode}, @var{?SpecList}, @var{+Options}, @var{-DBRef})
%@  @PLXindex {db_open/[4,5] (bdb)}
%@  Opens a database with the name @var{DBName}.  The database
%@  physically consists of a directory with the same name, containing the
%@  files that make up the database.  If the directory does not exist,
%@  it is created.  In that case @var{Mode} must be @code{update} and the
%@  db-spec @var{SpecList} must be ground.  If an existing
%@  database is opened and @var{Mode} is @code{read} or @code{update},
%@  @var{SpecList} is unified with the db-spec given when the
%@  database was created.  If the unification fails an error is
%@  raised.  @var{DBRef} is unified with a reference to the opened
%@  database.
%@  
%@  If @var{Mode} is @code{enumerate} then the indexing specification is not
%@  read, and @var{SpecList} is left unbound.
%@  
%@  @var{Options} provides a way to specify an environment in which to open
%@  the database, or a cache size.  @var{Options} should be a
%@  list of terms of the following form:
%@  
%@  @table @code
%@  @item environment(@var{EnvRef})
%@  @findex environment/1 (db_open/5 option)
%@  The database will be opened in this environment.
%@  
%@  @item cache_size(@var{CacheSize})
%@  @findex cache_size/1 (db_open/5 option)
%@  This is the (integer) size of the cache in kilobytes.  The size of the
%@  cache cannot be less than 20 kilobytes.  If @var{CacheSize} is given as
%@  the atom @code{default}, a default cache size will be used.  If
%@  @var{CacheSize} is given as the atom @code{off} or the atom
%@  @code{none}, all modified records will be flushed to disk after each
%@  operation.
%@  @end table
%@  
%@  To avoid inconsistency, if multiple processes open the same
%@  database, then all of them should do that with @var{Mode} set to
%@  @code{read} or @code{enumerate}.  (This is not enforced by the system.)

db_open(DBName, Mode, SpecList, DBRef) :-
	ErrGoal = db_open(DBName, Mode, SpecList, DBRef),
	db_open_int(none, DBName, Mode, SpecList, 0, DBRef, ErrGoal).

% SpecList is not relevant if Mode = enumerate.
db_open(DBName, Mode, SpecList, Options, DBRef) :-
	ErrGoal = db_open(DBName, Mode, SpecList, Options, DBRef),
	must_be(Options, proper_list, ErrGoal, 4),
	(   db_open_options(Options, none, EnvRef, 0, CacheSize) -> true
	;   illarg(domain(term,db_open_option), ErrGoal, 4)
	),
	db_open_int(EnvRef, DBName, Mode, SpecList, CacheSize, DBRef, ErrGoal).

db_open_options([], EnvRef, EnvRef, CacheSize, CacheSize).
db_open_options([Opt|Opts], EnvRef0, EnvRef, CacheSize0, CacheSize) :-
	callable(Opt),
	db_open_option(Opt, EnvRef0, EnvRef1, CacheSize0, CacheSize1),
	db_open_options(Opts, EnvRef1, EnvRef, CacheSize1, CacheSize).

db_open_option(environment(EnvRef), _, EnvRef, CacheSize, CacheSize).
db_open_option(cache_size(CacheKey), EnvRef, EnvRef, _, CacheSize) :-
	(   CacheKey==none    -> CacheSize = -1
	;   CacheKey==off     -> CacheSize = -1
	;   CacheKey==default -> CacheSize = 0
	;   CacheSize = CacheKey
	).

db_open_int(EnvRef, DBName0, Mode, SpecList0, CacheSize, DBRef, ErrGoal) :-
	(   EnvRef==none -> EnvAddr = 0 % uses NULL pointer!!!
	;  
            EnvAddrArgNo = 0, % [PM] 4.3 Was 4 but there is no EnvRef-argument in ErrGoal
            must_be_open_env(EnvRef, EnvAddr, _, ErrGoal, EnvAddrArgNo)
	),
	must_be(Mode, oneof([read,update,enumerate]), ErrGoal, 2),
	must_be_valid_spec_list(SpecList0, ErrGoal, 3),
        CacheSizeArgNo = 0, % [PM] 4.3 was 5 but there is no CacheSize-argument in ErrGoal.
	must_be(CacheSize, integer, ErrGoal, CacheSizeArgNo),
	(   var(SpecList0)
	->  SpecList = SpecList0, SpecSize = 0, SpecAddr = 0 % NULL pointer!!!
	;   normalise_spec_list(SpecList0, SpecList),
	    bdb_fast_buf_write(SpecList, SpecSize, SpecAddr)
	),
        DBName = DBName0,
	(   EnvAddr =:= 0 ->
            % [PM] 4.1 Was: DBName = DBName1
            % [PM] 4.3 See above for problems with turning off case-normalization
            absolute_file_name(DBName, DBName1, [])
	;   '$db_env'(EnvAddr, EnvName) -> % [PM[ 4.2.1 did leave choice point if EnvAddr is bignum.
	    absolute_file_name(DBName, DBName1, [relative_to(EnvName)]),
	    %% EnvName is already an absolute file name here.
	    must_be_filename_part(EnvName, DBName1, ErrGoal, 1)
	),
	%% [PD] 4.0.5 Keep these names in sync with library/bdb/bdb.h
	absolute_file_name('terms.db', TermsDBName, [relative_to(DBName1)]),
	absolute_file_name('index.db', IndexDBName, [relative_to(DBName1)]),
	absolute_file_name('admin.db', AdminDBName, [relative_to(DBName1)]),
	mode_open_db(EnvAddr, DBName1, TermsDBName, IndexDBName, AdminDBName, Mode, DBAddr, SpecAddr, SpecSize, CacheSize, ErrGoal, R),
	db_error_chk(R, db_open),
	(   Mode = enumerate -> SpecList = [] % [MC] 4.1.2 bind SpecList
	;   c_read_spec(DBAddr, RealSpecAddr, R1),
	    (	R1 =:= 0 -> true
	    ;	c_close_db(DBAddr, _), db_error('read_spec', R1)
	    ),
	    bdb_fast_buf_read(SpecList1, RealSpecAddr),
	    (	SpecList = SpecList1 -> true
	    ;	c_close_db(DBAddr, _),
		illarg(consistency(SpecList1,SpecList,'inconsistent database spec'), ErrGoal, 0, 0)
	    )
	),
	DBRef = '$db'(DBAddr),
	assert_applicable_keys(SpecList, DBAddr, 1),
	assertz('$db'(DBAddr, EnvAddr, DBName, Mode, SpecList)).

mode_open_db(EnvAddr, _DBName, TermsDBName, IndexDBName, AdminDBName, read, DBAddr, _SpecAddr, _SpecSize, CacheSize, _ErrGoal, R) :- !,
	c_open_db_read(EnvAddr, TermsDBName, IndexDBName, AdminDBName, DBAddr, CacheSize, R).
mode_open_db(EnvAddr, DBName, TermsDBName, IndexDBName, AdminDBName, update, DBAddr, SpecAddr, SpecSize, CacheSize, ErrGoal, R) :- !,
	(   directory_exists(DBName) ->
	    (   file_exists(TermsDBName),
		file_exists(IndexDBName),
		file_exists(AdminDBName) ->
		NFiles = 3
	    ;
		illarg(existence(file, 'db files', 'one or more db files missing'), ErrGoal, 1)
	    )
	;
	    (   ( SpecAddr =:= 0 ; SpecSize =:= 0 ) ->
 		illarg(var, ErrGoal, 3)
 	    ;
 		true
 	    ),
	    catch((make_directory(DBName),
		   NFiles = 0 ),
		  error(_,_),   % [PM] 4.1.0 Only ISO exceptions (SPRM 11617)
		  illarg(permission(open,dbname,'cannot create directory'), ErrGoal, 1, DBName))
	),
	c_open_db_update(EnvAddr, TermsDBName, IndexDBName, AdminDBName, DBAddr, SpecAddr, SpecSize, CacheSize, NFiles, R),
	(   R =\= 0, NFiles =:= 0 ->
	    delete_file(TermsDBName),
	    delete_file(IndexDBName),
	    delete_file(AdminDBName)
	;
	    true
	).
mode_open_db(EnvAddr, _DBName, TermsDBName, _IndexDBName, _AdminDBName, enumerate, DBAddr, _SpecAddr, _SpecSize, CacheSize,  _ErrGoal, R) :- !,
	c_open_db_enumerate(EnvAddr, TermsDBName, DBAddr, CacheSize, R).

valid_spec_list(SL) :- var(SL), !.
valid_spec_list(SL) :-
	SL = [_|_],
	(   foreach(S,SL)
	do  valid_spec(S)
	).

valid_spec(S) :-
	nonvar(S),
	functor(S, N, _),
	atom(N),
	(   foreacharg(A,S)
	do  valid_argspec(A)
	).

valid_argspec(?) :- !, fail.
valid_argspec(+).
valid_argspec(-).

normalise_spec_list(SpecList, NSpecList) :-
	(   foreach(S, SpecList),
	    foreach(K-S, KSpecList)
	do  spec_key(S, K)
	),
	keysort(KSpecList, KSSpecList),
	(   foreach(_-X, KSSpecList),
	    foreach(X, NSpecList)
	do  true
	).

spec_key(S, K) :-
	functor(S, F, A),
	minus_args(S, 1, A, 0, P),
	functor(K, F, 2),
	arg(1, K, A),
	arg(2, K, P).

minus_args(S, I, A, P0, P) :-
	(   I =< A
	->  arg(I, S, X),
	    (	X == (-)
	    ->	P1 is P0+1
	    ;	P1 = P0
	    ),
	    I1 is I+1,
	    minus_args(S, I1, A, P1, P)
	;   P = P0
	).

assert_applicable_keys([], _, _).
assert_applicable_keys([Spec|SpecList], DB, N) :-
	functor(Spec, F, A),
	functor(SpecPattern, F, A),
	same_functor_prefix(SpecList, F, A, SameFs, Rest, N, N1),
	assert('$db_specs'(SpecPattern, DB, N, [Spec|SameFs])),
	assert_applicable_keys(Rest, DB, N1).

same_functor_prefix([], _F, _A, [], [], N, N).
same_functor_prefix(SpecList, F, A, SameFs, Rest, N0, N) :-
	SpecList = [S|Spec],
	functor(S, F1, A1),
	(   F == F1, A == A1
	->  SameFs = [S|SameFs1], N1 is N0+1,
	    same_functor_prefix(Spec, F, A, SameFs1, Rest, N1, N)
	;   SameFs = [], Rest = SpecList, N = N0
	).

%@  @item db_close(@var{+DBRef})
%@  @PLXindex {db_close/1 (bdb)}
%@  Closes the database referenced by @var{DBRef}.  Any iterators
%@  opened in the database will be deallocated.

db_close(DBRef) :-
	ErrGoal = db_close(DBRef),
	must_be_open_db(DBRef, DBAddr, _, _, ErrGoal, 1),
	db_close_int(DBAddr).

db_close_int(DBAddr) :-
	close_all_its_in_db(DBAddr),
	c_close_db(DBAddr, R),
	db_error_chk(R, close_env),
	retractall('$db_specs'(_, DBAddr, _, _)),
	retract('$db'(DBAddr, _, _, _, _)),
        % [MC] 4.1.2 DBAddr is often a bignum
        !.

close_all_its_in_db(DBAddr) :-
	db_current_iterator_int(DBAddr, _T, I),
	on_exception(E, db_iterator_done_int(I),
		     asserta_if_ok(close_all_its_in_db, E)),
	fail.
close_all_its_in_db(_) :-
	retract('$first_error'(close_all_its_in_db, E)), !,
	throw(E).
close_all_its_in_db(_).

%@  @item db_current(@var{?DBName}, @var{?Mode}, @var{?SpecList}, @var{?EnvRef}, @var{?DBRef})
%@  @PLXindex {db_current/5 (bdb)}
%@  Unifies the arguments with the open databases.  This
%@  predicate can be used to enumerate all currently open
%@  databases through backtracking.  If the database was
%@  opened without an environment, then @var{EnvRef} will be unified
%@  with the atom @code{none}.
:- db_current/5 is nondet.
db_current(DBName, Mode, Spec, EnvRef, '$db'(DBAddr)) :-
	'$db'(DBAddr, EnvAddr, DBName, Mode, Spec),
	(   EnvAddr =:= 0 -> EnvRef = none
	;   EnvRef = '$db_env'(EnvAddr)
	).


%@  @item db_store(@var{+DBRef}, @var{+Term}, @var{-TermRef})
%@  @PLXindex {db_store/3 (bdb)}
%@  Stores @var{Term} in the database @var{DBRef}.  @var{TermRef} is
%@  unified with a corresponding term reference.  The
%@  functor of @var{Term} must match the functor of a spec in
%@  the db-spec associated with @var{DBRef}.

db_store(DBRef, Term, TermRef) :-
	ErrGoal = db_store(DBRef, Term, TermRef),
	TermRef = '$db_termref'(ITermRef),
	must_be_open_db(DBRef, DBAddr, update, _SpecList, ErrGoal, 1),
	must_be(Term, callable, ErrGoal, 2),
	index_keys(Term, DBAddr, Keys),
	illarg_on_nil(Keys, domain(term,'valid term'), ErrGoal, 2, Term),
	hash_key_set(Keys, HCs),
	c_next_termref(DBAddr, ITermRef, R1),
	db_error_chk(R1, next_termref),
	(   foreach(H,HCs),
	    param([DBAddr,ITermRef])
	do  c_store_termref(DBAddr, H, ITermRef, R2),
	    db_error_chk(R2, store_termref)
	),
	bdb_fast_buf_write(Term, TermSize, TermAddr),
	c_store_term(DBAddr, ITermRef, TermAddr, TermSize, R3),
	db_error_chk(R3, store_term).

illarg_on_nil([], Ball, ErrGoal, ArgNo, Culprit) :-
	illarg(Ball, ErrGoal, ArgNo, Culprit).
illarg_on_nil([_|_], _, _, _, _).

hash_key_set(Ks, HCs) :-
	(   foreach(K,Ks),
	    foreach(H,HCs0)
	do  c_term_hash(K, H)
	),
	sort(HCs0, HCs).

%@  @item db_fetch(@var{+DBRef}, @var{?Term}, @var{?TermRef})
%@  @PLXindex {db_fetch/3 (bdb)}
%@  Unifies @var{Term} with a term from the database
%@  @var{DBRef}.  At the same time, @var{TermRef} is unified with a
%@  corresponding term reference.  Backtracking over the
%@  predicate unifies with all terms matching @var{Term}.
%@  
%@  If @var{TermRef} is not instantiated then both the functor
%@  and the instantiatedness of @var{Term} must match a spec in the
%@  db-spec associated with @var{DBRef}.
%@  
%@  If @var{TermRef} is instantiated, the referenced term is
%@  read and unified with @var{Term}.
%@  
%@  If you simply want to find all matching terms, it is more
%@  efficient to use @code{db_findall/5} or @code{db_enumerate/3}.
:- db_fetch(+, -, +) is semidet.
:- db_fetch(+, ?, -) is nondet.
db_fetch(DBRef, Term, TermRef) :-
	nonvar(TermRef), !,
	ErrGoal = db_fetch(DBRef, Term, TermRef),
	must_be_db(DBRef, DBAddr, ErrGoal, 1),
	must_be_termref(TermRef, ITermRef, ErrGoal, 3),
	fetch_ref(DBAddr, Term, ITermRef).
db_fetch(DBRef, Term, TermRef) :-
	ErrGoal = db_fetch(DBRef, Term, TermRef),
	must_be_open_db(DBRef, DBAddr, Mode, _SpecList, ErrGoal, 1),
	must_not_be_enumerate(Mode, ErrGoal, 1, DBRef),
	must_be(Term, callable, ErrGoal, 2),
	TermRef = '$db_termref'(ITermRef),
	query_keys(Term, DBAddr, Keys, ErrGoal),
	hash_key_set(Keys, HCs),
	c_term_iterator(DBAddr, HCs, ItAddr, R),
	db_error_chk(R, term_iterator),
	call_cleanup(tfetch(ItAddr, Term, ITermRef), tfetch_cleanup(ItAddr)).

fetch_ref(DBAddr, Term, ITermRef) :-
	c_fetch_term(DBAddr, ITermRef, TermAddr, R),
	db_error_chk(R, fetch_term),
	bdb_fast_buf_read(Term, TermAddr).

:- tfetch/3 is nondet.
tfetch(ItAddr, Term, ITermRef) :-
	repeat,
	  c_term_iterator_next(ItAddr, TermAddr, ITermRef, R),
	  db_error_chk(R, term_iterator_next),
	  (   TermAddr =:= 0 -> !, fail
	  ;   bdb_fast_buf_read(Term, TermAddr)
	  ).

tfetch_cleanup(ItAddr) :-
	c_term_iterator_done(ItAddr, R),
	db_error_chk(R, term_iterator_done).

%@  @item db_erase(@var{+DBRef}, @var{+TermRef})
%@  @itemx db_erase(@var{+DBRef}, @var{+TermRef}, @var{+Term})
%@  @PLXindex {db_erase/[2,3] (bdb)}
%@  Deletes the term from the database @var{DBRef} that is
%@  referenced by @var{TermRef}.
%@  
%@  In the case of @code{db_erase/2} the term associated with
%@  @var{TermRef} has to be looked up.  @code{db_erase/3} assumes that the
%@  term @var{Term} is identical with the term associated with
%@  @var{TermRef} (modulo variable renaming).  If this is not the
%@  case, the behavior is undefined.

db_erase(DBRef, TermRef) :-
	ErrGoal = db_erase(DBRef, TermRef),
	db_erase_chk(DBRef, TermRef, ErrGoal, DBAddr, ITermRef),
	fetch_ref(DBAddr, Term, ITermRef),
	index_keys(Term, DBAddr, Keys),
	db_erase_int(DBAddr, Keys, ITermRef).

db_erase(DBRef, TermRef, Term) :-
	ErrGoal = db_erase(DBRef, TermRef, Term),
	db_erase_chk(DBRef, TermRef, ErrGoal, DBAddr, ITermRef),
	index_keys(Term, DBAddr, Keys),
	illarg_on_nil(Keys, domain(term,'valid term'), ErrGoal, 3, Term),
	db_erase_int(DBAddr, Keys, ITermRef).

db_erase_chk(DBRef, TermRef, ErrGoal, DBAddr, ITermRef) :-
	must_be_open_db(DBRef, DBAddr, update, _SpecList, ErrGoal, 1),
	must_be_termref(TermRef, ITermRef, ErrGoal, 2).

db_erase_int(DBAddr, Keys, ITermRef) :-
	hash_key_set(Keys, HCs),
	c_delete_term(DBAddr, ITermRef, R),
	db_error_chk(R, delete_term),
	(   foreach(H,HCs),
	    param([DBAddr,ITermRef])
	do  c_delete_termref(DBAddr, H, ITermRef, S),
	    db_error_chk(S, delete_termref)
	).

%@  @item db_enumerate(@var{+DBRef}, @var{?Term}, @var{?TermRef})
%@  @PLXindex {db_enumerate/3 (bdb)}
%@  
%@  Unifies @var{Term} with a term from the database
%@  @var{DBRef}.  At the same time, @var{TermRef} is unified with a
%@  corresponding term reference.  Backtracking over the
%@  predicate unifies with all terms matching @var{Term}.
%@  
%@  Implemented by linear search---the db-spec associated with @var{DBRef}
%@  is ignored.  It is not useful to call this predicate with
%@  @var{TermRef} instantiated.

:- db_enumerate/3 is nondet.
db_enumerate(DBRef, Term, TermRef) :-
	ErrGoal = db_enumerate(DBRef, Term, TermRef),
%% [PD] 4.0.5 SPRM 8463
%	must_be_db(DBRef, DBAddr, ErrGoal, 1),
	must_be_open_db(DBRef, DBAddr, _Mode, _SpecList, ErrGoal, 1),
	TermRef = '$db_termref'(ITermRef),
	c_global_iterator(DBAddr, ItAddr, R),
	db_error_chk(R, global_iterator),
	call_cleanup(gfetch(ItAddr, Term, ITermRef), gfetch_cleanup(ItAddr)).

:- gfetch/3 is nondet.
gfetch(ItAddr, Term, ITermRef) :-
	repeat,
	  c_global_iterator_next(ItAddr, TermAddr, ITermRef, R),
	  db_error_chk(R, global_iterator_next),
	  (   TermAddr =:= 0 -> !, fail
	  ;   bdb_fast_buf_read(Term, TermAddr)
	  ).

gfetch_cleanup(ItAddr) :-
	c_global_iterator_done(ItAddr, R),
	db_error_chk(R, global_iterator_done).

%@  @item db_findall(@var{+DBRef}, @var{+Template}, @var{+Term}, @var{:Goal}, @var{-Bag})
%@  @PLXindex {db_findall/3 (bdb)}
%@  Unifies @var{Bag} with the list of instances of
%@  @var{Template} in all proofs of @var{Goal} found when @var{Term} is
%@  unified with a matching term from the database
%@  @var{DBRef}.  Both the functor and the instantiatedness of
%@  @var{Term} must match a spec in the db-spec associated with @var{DBRef}.
%@  Conceptually, this predicate is equivalent to
%@  @code{findall(@var{Template}, (db_fetch(@var{DBRef}, @var{Term}, _),
%@  @var{Goal}), @var{Bag})}.
:- db_findall(+, +, +, 0, -) is det.
db_findall(DBRef, Template, Term, Goal, Bag) :-
	(   var(Term) -> db_make_iterator(DBRef, Iterator)
	;   db_make_iterator(DBRef, Term, Iterator)
	),
	call_cleanup(db_findall_cont(Iterator, Template, Term, Goal, Bag),
		     db_iterator_done_int(Iterator)).

db_findall_cont(Iterator, Template, Term, Goal, Bag) :-
	db_iterator_next_int(Iterator, Term1, _, R),
	(   R =\= 0 -> Bag = []	% no more terms
	;   findall(Template, unify_call(Term, Term1, Goal), Bag, BagT),
	    db_findall_cont(Iterator, Template, Term, Goal, BagT)
	).

:- unify_call/3 is nondet.
unify_call(T, T, G) :-
	call(G).

%@  @item db_compress(@var{+DBRef}, @var{+DBName})
%@  @itemx db_compress(@var{+DBRef}, @var{+DBName}, @var{+SpecList})
%@  @PLXindex {db_compress/[2,3] (bdb)}
%@  Copies the database given by @var{DBRef} to a new database
%@  named by @var{DBName}.  The new database will be a compressed
%@  version of the first one in the sense that it will not have ``holes''
%@  resulting from deletion of terms.  Deleted term references
%@  will also be reused, which implies that references that refer to
%@  terms in the old database will be invalid in the new one.
%@  
%@  @code{db_compress/2} looks for a database with the db-spec of the
%@  original one.  @code{db_compress/3} stores the terms found in the
%@  original database with the indexing specification @var{SpecList}.
%@  @code{db_compress/2} cannot be used if the database @var{DBRef}
%@  was opened in mode @code{enumerate}.
%@  
%@  If the database @var{DBName} already exists then the terms
%@  of @var{DBRef} will be appended to it.  Of course @var{DBName} must have
%@  an indexing specification, which enables the terms in @var{DBRef}
%@  to be inserted into it.
%@  
%@  In the case of @code{db_compress/3} if the database @var{DBName}
%@  does not exist, then @var{SpecList} must be a valid indexing
%@  specification.

db_compress(DBRef, DBName) :-
	ErrGoal = db_compress(DBRef, DBName),
	must_be_open_db(DBRef, DBAddr, Mode, SpecList, ErrGoal, 1),
	must_not_be_enumerate(Mode, ErrGoal, 1, DBRef),
	db_compress_int(DBAddr, DBName, SpecList).

db_compress(DBRef, DBName, Spec) :-
	ErrGoal = db_compress(DBRef, DBName, Spec),
	must_be_db(DBRef, DBAddr, ErrGoal, 1),
	must_be_valid_spec_list(Spec, ErrGoal, 3),
	db_compress_int(DBAddr, DBName, Spec).

db_compress_int(DBAddr, DBName, Spec) :-
	db_open(DBName, update, Spec, DBRefNew),
	db_make_iterator_int(DBAddr, Iterator),
	call_cleanup(copy_db(Iterator, DBRefNew), copy_db_cleanup(Iterator, DBRefNew)).

copy_db(Iterator, DBRef) :-
	db_iterator_next_int(Iterator, Term, _, R),
	(   R =\= 0 -> true	% no more terms
	;   db_store(DBRef, Term, _), copy_db(Iterator, DBRef)
	).

copy_db_cleanup(Iterator, DBRefNew) :-
	db_iterator_done_int(Iterator),
	db_close(DBRefNew).

%@  @item db_sync(@var{+DBRef})
%@  @PLXindex {db_sync/1 (bdb)}
%@  Flushes any cached information from the database referenced by
%@  @var{DBRef} to stable storage.

db_sync(DBRef) :-
	ErrGoal = db_sync(DBRef),
%% [PD] 4.0.5 SPRM 8463
%	must_be_db(DBRef, DBAddr, ErrGoal, 1),
	must_be_open_db(DBRef, DBAddr, _Mode, _SpecList, ErrGoal, 1),
	c_sync_db(DBAddr, R),
	db_error_chk(R, sync_db).

%@  @item db_make_iterator(@var{+DBRef}, @var{-Iterator})
%@  @itemx db_make_iterator(@var{+DBRef}, @var{+Term}, @var{-Iterator})
%@  @PLXindex {db_make_iterator/[2,3] (bdb)}
%@  Creates a new iterator and unifies it with @var{Iterator}.
%@  Iterators created with @code{db_make_iterator/2} iterate through the
%@  whole database.  Iterators created with @code{db_make_iterator/3}
%@  iterate through the terms that would be found by
%@  @code{db_fetch(@var{DBRef}, @var{Term}, _)}.
%@  
%@  Every iterator created by @code{db_make_iterator/[2,3]} must be
%@  destroyed with @code{db_iterator_done/1}.

db_make_iterator(DBRef, Iterator) :-
	ErrGoal = db_make_iterator(DBRef, Iterator),
%% [PD] 4.0.5 SPRM 8463
%	must_be_db(DBRef, DBAddr, ErrGoal, 1),
	must_be_open_db(DBRef, DBAddr, _Mode, _SpecList, ErrGoal, 1),
	db_make_iterator_int(DBAddr, Iterator).

db_make_iterator_int(DBAddr, Iterator) :-
	c_global_iterator(DBAddr, ItAddr, R),
	db_error_chk(R, global_iterator),
	Iterator = '$db_global_it'(ItAddr),
	assertz('$db_it'(ItAddr, DBAddr)).

db_make_iterator(DBRef, Term, Iterator) :-
	ErrGoal = db_make_iterator(DBRef, Term, Iterator),
	must_be_open_db(DBRef, DBAddr, Mode, _SpecList, ErrGoal, 1),
	must_not_be_enumerate(Mode, ErrGoal, 1, DBRef),
	must_be(Term, callable, ErrGoal, 2),
	Iterator = '$db_term_it'(ItAddr),
	query_keys(Term, DBAddr, Keys, ErrGoal),
	hash_key_set(Keys, HCs),
	create_term_iterator(DBAddr, HCs, Term, ItAddr).

%@  @item db_iterator_next(@var{+Iterator}, @var{-Term}, @var{-TermRef})
%@  @PLXindex {db_iterator_next/3 (bdb)}
%@  @var{Iterator} advances to the next term, @var{Term} and
%@  @var{TermRef} is unified with the term and its reference
%@  pointed to by @var{Iterator}.  If there is no next term, the
%@  predicate fails.
:- db_iterator_next(+Iterator, -Term, -TermRef) is semidet.
db_iterator_next(Iterator, Term, TermRef) :-
	ErrGoal = db_iterator_next(Iterator, Term, TermRef),
	iterator_chk(Iterator, ErrGoal),
	db_iterator_next_int(Iterator, Term, TermRef, 0).

%% [PD] SPRM 8463
% iterator_chk(Iterator, ErrGoal) :-
% 	(   Iterator = '$db_global_it'(ItAddr), integer(ItAddr) -> true
% 	;   Iterator = '$db_term_it'(ItAddr), integer(ItAddr) -> true
% 	;   illarg(domain(ground,'iterator'), ErrGoal, 1, Iterator)
% 	).

%% [PD] SPRM 8463
iterator_chk(Iterator, ErrGoal) :-
 	(   Iterator = '$db_global_it'(ItAddr),
	    integer(ItAddr),
	    '$db_it'(ItAddr, _DBAddr) -> true
 	;   Iterator = '$db_term_it'(ItAddr),
	    integer(ItAddr),
	    '$db_it'(ItAddr, _DBAddr, _Term) -> true
 	;   illarg(existence(iterator,Iterator,'nonexisting iterator'), ErrGoal, 1, Iterator)
 	).

db_iterator_next_int('$db_global_it'(ItAddr), Term, '$db_termref'(ITermRef),
		     Code) :-
	c_global_iterator_next(ItAddr, TermAddr, ITermRef, R),
	db_error_chk(R, global_iterator_next),
	(   TermAddr =:= 0 -> Code = 1 % no more terms
	;   Code = 0, bdb_fast_buf_read(Term, TermAddr)
	).
db_iterator_next_int('$db_term_it'(ItAddr), Term, TermRef, Code) :-
	'$db_it'(ItAddr, DBAddr, Copy),
        !, % [PM] 4.2.1 ItAddr may be a bignum.
	term_iterator_next(ItAddr, DBAddr, Copy, Term, TermRef, Code).

term_iterator_next(ItAddr, DBAddr, Copy, Term, TermRef, Code) :-
	c_term_iterator_next(ItAddr, TermAddr, ITermRef, R),
	db_error_chk(R, term_iterator_next),
	(   TermAddr =:= 0 -> Code = 1 % no more terms
	;   bdb_fast_buf_read(TermR, TermAddr),
	    (	Copy = TermR ->
		Term = TermR, TermRef = '$db_termref'(ITermRef), Code = 0
	    ;	term_iterator_next(ItAddr, DBAddr, Copy, Term, TermRef, Code)
	    )
	).

create_term_iterator(DBAddr, HCs, Term, ItAddr) :-
	c_term_iterator(DBAddr, HCs, ItAddr, R),
	db_error_chk(R, term_iterator),
	assertz('$db_it'(ItAddr, DBAddr, Term)).

%@  @item db_iterator_done(@var{+Iterator})
%@  @PLXindex {db_iterator_done/1 (bdb)}
%@  Deallocates @var{Iterator}, which must not be in use anymore.

db_iterator_done(Iterator) :-
	ErrGoal = db_iterator_done(Iterator),
	iterator_chk(Iterator, ErrGoal),
	db_iterator_done_int(Iterator).

db_iterator_done_int('$db_global_it'(ItAddr)) :-
	c_global_iterator_done(ItAddr, R),
	db_error_chk(R, global_iterator_done),
	retract('$db_it'(ItAddr, _DBAddr)),
        % [PM] 4.2.1 ItAddr may be bignum
        !.
db_iterator_done_int('$db_term_it'(ItAddr)) :-
	c_term_iterator_done(ItAddr, R),
	db_error_chk(R, term_iterator_done),
	retract('$db_it'(ItAddr, _DBAddr, _Copy)),
        % [PM] 4.2.1 ItAddr may be bignum
        !.

%@  @item db_current_iterator(@var{?DBRef}, @var{?Term}, @var{?Iterator})
%@  @PLXindex {db_current_iterator/3 (bdb)}
%@  Unifies the variables with the respective properties of
%@  the living iterators.  This predicate can be used to enumerate all
%@  currently alive iterators through backtracking.  If @var{Iterator}
%@  was made with @code{db_make_iterator/2} then @var{Term} will be left
%@  unbound.

:- db_current_iterator/3 is nondet.
db_current_iterator('$db'(DBAddr), Term, Iterator) :-
	db_current_iterator_int(DBAddr, Term, Iterator).

:- db_current_iterator_int/3 is nondet.
db_current_iterator_int(DBAddr, Term, Iterator) :-
	(   '$db_it'(ItAddr, DBAddr), Iterator = '$db_global_it'(ItAddr)
	;   '$db_it'(ItAddr, DBAddr, Term), Iterator = '$db_term_it'(ItAddr)
	).

% Syntax of specs
% ---------------
%  
%   speclist  = [spec1, ..., specM]
%   spec      = FUNCTOR(argspec1, ..., argspecN)
%   argspec   = + | -

query_keys(Term, DB, Keys, ErrGoal) :-
	'$db_specs'(Term, DB, I0, Spec), !,
	keylist(Spec, I0, Term, Key),
	(   Key \== none -> Key = [I|M]
	;   illarg(var, ErrGoal, 2, Term)
	),
	(   M = [] -> Keys = [[I]]
	;   var_code(VAR), Keys = [Key,[I,VAR]]
	).
query_keys(Term, _, _, ErrGoal) :-
	illarg(domain(term,'valid term'), ErrGoal, 2, Term).

keylist([], _, _, none).
keylist([S|Spec], I, T, Key) :-
	c_index_keys(S, T, K, C),
	(   C < 0 -> I1 is I+1, keylist(Spec, I1, T, Key)
	;   Key = [I|K]
	).

index_keys(Term, DB, Keys) :-
	'$db_specs'(Term, DB, I0, Spec), !,
	index_keys(Spec, I0, Term, Keys).
index_keys(_, _, []).

index_keys([], _, _, []).
index_keys([S|Spec], N, T, Keys) :-
	c_index_keys(S, T, K, C),
	(   C =:= -1 -> Keys = Keys1
	;   C =:= -2 -> var_code(VAR), Keys = [[N,VAR]|Keys1]
	;   Keys = [[N|K]|Keys1]
	),
	N1 is N+1,
	index_keys(Spec, N1, T, Keys1).

var_code(-0x1227F5A).

% decode the type of error and print it along with the context of error
:- db_error/2 is throwing.
db_error(Ctxt, ErrorCode) :-
	decode_error(ErrorCode, Err),
        Goal = bdb, % [PM] 4.3 FIXME: proper goal
	illarg(system(berkeley_db(Ctxt,Err)), Goal, 0, 0). % TODO: msgs.pl

db_error_chk(R, _) :- R =:= 0, !.
db_error_chk(R, M) :- 
	db_error(M, R).

asserta_if_ok(Id, _) :-
	'$first_error'(Id, _), !.
asserta_if_ok(Id, E) :-
	asserta('$first_error'(Id, E)).

must_be_open_env(EnvRef, EnvAddr, CRef, ErrGoal, ArgNo) :-
	(   EnvRef = '$db_env'(EnvAddr), integer(EnvAddr) ->
	    (	clause('$db_env'(EnvAddr, _), true, CRef) -> true
	    ;   illarg(permission(access,environment,'not open'), ErrGoal, ArgNo, EnvRef)
	    )
	;   illarg(domain(ground,'environment reference'), ErrGoal, ArgNo, EnvRef)
	).

must_be_valid_spec_list(SpecList, ErrGoal, ArgNo) :-
	(   valid_spec_list(SpecList) -> true
	;   illarg(domain(list,'database spec'),
			  ErrGoal, ArgNo, SpecList)
	).

must_be_open_db(DBRef, DBAddr, Access, SpecList, ErrGoal, ArgNo) :-
	must_be_db(DBRef, DBAddr, ErrGoal, ArgNo),
	(   '$db'(DBAddr, _, _, Access1, SpecList) -> true
	;   illarg(permission(access,database,'not open'), ErrGoal, ArgNo, DBRef)
	),
	(   Access = Access1 -> true
	;   illarg(permission(store,database,read_only), ErrGoal, ArgNo, DBRef)
	).

must_be_db(DBRef, DBAddr, ErrGoal, ArgNo) :-
	(   DBRef = '$db'(DBAddr),
	    integer(DBAddr) -> true
	;   illarg(domain(ground,'database reference'), ErrGoal, ArgNo, DBRef)
	).

must_be_termref(TermRef, ITermRef, ErrGoal, ArgNo) :-
	(   TermRef = '$db_termref'(ITermRef),
	    integer(ITermRef) -> true
	;   illarg(domain(ground,'term reference'), ErrGoal, ArgNo, TermRef)
	).

must_not_be_enumerate(enumerate, ErrGoal, ArgNo, DBRef) :- !,
	illarg(permission(access,database,enumerate_only), ErrGoal, ArgNo, DBRef).
must_not_be_enumerate(_, _, _, _).

must_be_filename_part(BasePath, FilePath, ErrGoal, ArgNo) :-
	(   atom_concat(BasePath, _, FilePath) -> true
	;   illarg(consistency(BasePath,FilePath,'inconsistent database name'), ErrGoal, ArgNo, FilePath)
	).

%%% fastrw backward compatibility wrapper %%%%%%%%%

%% [PM] 3.11.0+ Should change the version byte for backward (pre
%%      3.11.0) fastrw compatibility.

bdb_fast_buf_read(Term, Addr) :-
   c_from_fastrw_old(Addr, Addr1, Res), % [PM] 3.11.0+
   from_fastrw_res(Res, c_from_fastrw_old(Addr, Addr1, Res)),
   fast_buf_read(Term, Addr1).


bdb_fast_buf_write(Term, Size, Addr) :-
   fast_buf_write(Term, Size1, Addr1),
   c_from_fastrw_new(Addr1, Size1, Addr2, Size2, Res), % [PM] 3.11.0+
   from_fastrw_res(Res, c_from_fastrw_new(Addr1, Size1, Addr2, Size2, Res)),
   Size = Size2,
   Addr = Addr2.

from_fastrw_res(0, _Goal) :- !.
from_fastrw_res(Code, Goal) :-
   illarg(system(berkeley_db(Goal,error(Code))), Goal, 0, 0). % TODO: msgs.pl

%@  @item db_export(@var{+DBName}, @var{+ExportFile})
%@  @itemx db_export(@var{+DBName}, @var{+Options}, @var{+ExportFile})
%@  @PLXindex {db_export/[2,3] (bdb)}
%@  Exports the database with the name @var{DBName} to the text file
%@  @var{ExportFile}. @var{ExportFile} can be imported by
%@  @code{db_import/[2,3]}.
%@  
%@  @var{Options} should be an options list of the form acceptable by
%@  @code{db_open/[4,5]}.
%@  
%@  In SICStus 3.12.0 @code{bdb:export/[2,3]} is available instead of
%@  @code{db_export/[2,3]}.

db_export(DBName, Options, ExportFile) :-
    export(DBName, Options, ExportFile).

db_export(DBName, ExportFile) :-
    export(DBName, ExportFile).

export(DBName, Options, ExportFile) :-
    export1(DBName, Options, ExportFile).

export(DBName, ExportFile) :-
    export1(DBName, [], ExportFile).

export1(DBName, Options, ExportFile) :-
	ErrGoal = db_export(DBName, Options, ExportFile),
	must_be(Options, proper_list, ErrGoal, 2),
	export_db_open_read(DBName, SpecList, Options, DBRef),
	(   db_open_options(Options, none, EnvRef, 0, _CacheSize) -> true
	;   illarg(domain(term,db_open_option), ErrGoal, 2)
	),	
	open(ExportFile, write, Stream),
	call_cleanup(once(export2(Stream,DBName,DBRef,EnvRef,SpecList)),
		     export_cleanup(Stream, DBRef)).

export_db_open_read(DBName, SpecList, [], DBRef) :-
    !, % [PM] 4.2.1 SPIDER detcheck's first kill!
    db_open(DBName, read, SpecList, DBRef).
export_db_open_read(DBName, SpecList, Options, DBRef) :-
    db_open(DBName, read, SpecList, Options, DBRef).

export2(Stream, DBName, DBRef, EnvRef, SpecList) :-
    format(Stream, '%% This is an export of the database "~w"', [DBName]),
    ( db_current_env(EnvName, EnvRef) ->
	EnvProp=EnvName,
	format(Stream, ',~n%%   in the environment "~w".~2n', [EnvName])
    ; otherwise ->
	EnvProp=none,
	format(Stream, '.~2n', [])
    ),
    export_version(Version),
    format(Stream, '%% The first term in this file is a property list for the exported database.~2n', []),
    write_canonical(Stream, properties([environment(EnvProp),
					database(DBName),
					speclist(SpecList),
					version(Version)
				       ])),
    write(Stream, '.'),
    nl(Stream),
    nl(Stream),
    format(Stream, '%% The rest of the file contains the exported terms.~n', []),
    format(Stream, '%% Each term is wrapped like this: "term(<term>)"~2n', []),
    export_enumerate(Stream,DBRef).

export_cleanup(Stream, DBRef) :-
    close(Stream),
    db_close(DBRef).

export_enumerate(Stream,DBRef) :-
        db_enumerate(DBRef, Term, _),
	write_canonical(Stream, term(Term)),
	write(Stream, '.'),
	nl(Stream),
        fail.
export_enumerate(_,_).

%%% *** IMPORTANT!
%%% This version number should change if the export format is changed.
export_version(1).

%@  @item db_import(@var{+DBName}, @var{+ImportFile})
%@  @itemx db_import(@var{+DBName}, @var{+Options}, @var{+ImportFile})
%@  @PLXindex {db_import/[2,3] (bdb)}
%@  Imports the text file @var{ImportFile} into the database with
%@  the name @var{DBName}.
%@  
%@  If @var{ImportFile} is imported into an existing database, the
%@  @var{SpecList} found in the @var{ImportFile} will be unified with the
%@  @var{SpecList} in the database.
%@  
%@  @var{Options} should be an options list of the form acceptable by
%@  @code{db_open/[4,5]}.
%@  
%@  In SICStus 3.12.0 @code{bdb:import/[2,3]} is available instead of
%@  @code{db_import/[2,3]}.

db_import(DBName, Options, ImportFile) :-
    import(DBName, Options, ImportFile).

db_import(DBName, ImportFile) :-
    import(DBName, ImportFile).

import(DBName, Options, ImportFile) :-
    import1(DBName, Options, ImportFile).

import(DBName, ImportFile) :-
    import1(DBName, [], ImportFile).

import1(DBName, Options, ImportFile) :-
    open(ImportFile, read, Stream),
    call_cleanup(once(import2(Stream,DBName,Options)),close(Stream)).

import2(Stream, DBName, Options) :-
    read(Stream,PropertyTerm),
    import_properties(PropertyTerm,_EnvProp,_DBName,SpecList,_DaTime,_HostName,Version),
    import_check_version(Version),
    import_db_open_update(DBName, SpecList, Options, DBRef),
    call_cleanup(once(import_read_store(Stream,DBRef)), db_close(DBRef)).

import_read_store(Stream, DBRef) :-
    repeat,
      read(Stream, ImportTerm),
      import_store(DBRef, ImportTerm),
      ImportTerm == end_of_file,
    !.

import_properties(Properties,EnvProp,DBName,SpecList,DaTime,HostName,Version) :- nonvar(Properties),
    Properties = properties(PropertyList),
    !,
    import_get_properties(PropertyList,EnvProp,DBName,SpecList,DaTime,HostName,Version).
import_properties(PropertyTerm,_EnvProp,_DBName,_SpecList,_DaTime,_HostName,_Version) :-
    throw(unexpected_properties_error('First data must be properties', PropertyTerm)).

import_get_properties([],_EnvProp,_DBName,_SpecList,_DaTime,_HostName,_Version).
import_get_properties([Property|PropertyList],EnvProp,DBName,SpecList,DaTime,HostName,Version) :-
    import_property(Property,EnvProp,DBName,SpecList,DaTime,HostName,Version),
    import_get_properties(PropertyList,EnvProp,DBName,SpecList,DaTime,HostName,Version).

import_property(Property,_EnvProp,_DBName,_SpecList,_DaTime,_HostName,_Version) :-
        var(Property), !,
        true.                                                                                
import_property(environment(EnvProp1),EnvProp,_DBName,_SpecList,_DaTime,_HostName,_Version) :-
        !,
        EnvProp = EnvProp1.
import_property(database(DBName1),_EnvProp,DBName,_SpecList,_DaTime,_HostName,_Version) :-
        !,
        DBName = DBName1.
import_property(speclist(SpecList1),_EnvProp,_DBName,SpecList,_DaTime,_HostName,_Version) :-
        !,
        SpecList = SpecList1.
%% This one is (not yet) written by db_export/[2,3]
import_property(timestamp(DaTime1),_EnvProp,_DBName,_SpecList,DaTime,_HostName,_Version) :-
        !,
        DaTime = DaTime1.
%% This one is (not yet) written by db_export/[2,3]
import_property(hostname(HostName1),_EnvProp,_DBName,_SpecList,_DaTime,HostName,_Version) :-
        !,
        HostName = HostName1.
import_property(version(Version1),_EnvProp,_DBName,_SpecList,_DaTime,_HostName,Version) :-
        !,
        Version = Version1.
% Ignore unknown properties
import_property(_Properties, _EnvProp,_DBName,_SpecList,_DaTime,_HostName,_Version).

import_check_version(Version) :-
    ( export_version(Version) ->
	true
    ; otherwise ->
	throw(wrong_export_version_error(Version))
    ).

import_db_open_update(DBName, SpecList, [], DBRef) :-
    !,
    db_open(DBName, update, SpecList, DBRef).
import_db_open_update(DBName, SpecList, Options, DBRef) :-
    db_open(DBName, update, SpecList, Options, DBRef).

import_store(DBRef, Datum) :-
        import_store1(Datum, DBRef).

import_store1(end_of_file, _DBRef) :-
    !.
import_store1(term(Term), DBRef) :-
    !,
    db_store(DBRef, Term, _TermRef).
import_store1(ImportTerm, _DBRef) :-
    throw(unexpected_data_error(ImportTerm)).

foreign(open_env,
	c_open_env(+string, +integer, % cache size in KB (0 selects default)
		   -integer/*address('DB_ENV')*/, [-integer])).
foreign(close_env,
	c_close_env(+integer/*address('DB_ENV')*/, [-integer])).
%% [PD] 4.0.5 open_db did too much file ops. Put file ops in prolog code instead.
%foreign(open_db,
%	c_open_db(+integer/*address('DB_ENV')*/, +string, +string, -address('db_struct'),
%		  +integer/*address(void)*/, +integer, +integer, [-integer])).
foreign(open_db_read,
	c_open_db_read(+integer/*address('DB_ENV')*/, +string, +string, +string, -address('db_struct'),
	+integer, [-integer])).
foreign(open_db_update,
	c_open_db_update(+integer/*address('DB_ENV')*/, +string, +string, +string, -address('db_struct'),
	+integer/*address(void)*/, +integer, +integer, +integer, [-integer])).
foreign(open_db_enumerate,
	c_open_db_enumerate(+integer/*address('DB_ENV')*/, +string, -address('db_struct'),
		  +integer, [-integer])).
foreign(close_db,
	c_close_db(+address('db_struct'), [-integer])).
foreign(read_spec,
	c_read_spec(+address('db_struct'), -integer/*address(void)*/, [-integer])).
foreign(next_termref,
	c_next_termref(+address('db_struct'), -integer, [-integer])).
foreign(store_termref,
	c_store_termref(+address('db_struct'), +integer, +integer, [-integer])).
foreign(store_term,
	c_store_term(+address('db_struct'), +integer, +integer/*address(void)*/, +integer, [-integer])).
foreign(fetch_term,
	c_fetch_term(+address('db_struct'), +integer, -integer/*address(void)*/, [-integer])).
foreign(delete_term,
	c_delete_term(+address('db_struct'), +integer, [-integer])).
foreign(delete_termref,
	c_delete_termref(+address('db_struct'), +integer, +integer, [-integer])).
foreign(global_iterator,
	c_global_iterator(+address('db_struct'), -integer/*address('DBC')*/, [-integer])).
foreign(global_iterator_next,
	c_global_iterator_next(+integer/*address('DBC')*/, -integer/*address(void)*/, -integer, [-integer])).
foreign(global_iterator_done,
	c_global_iterator_done(+integer/*address('DBC')*/, [-integer])).
foreign(term_iterator,
	c_term_iterator(+address('db_struct'), +term, -address('iterator'), [-integer])).
foreign(term_iterator_next,
	c_term_iterator_next(+address('iterator'), -integer/*address(void)*/, -integer, [-integer])).
foreign(term_iterator_done,
	c_term_iterator_done(+address('iterator'), [-integer])).
foreign(db_term_hash,
	c_term_hash(+term, [-integer])).
foreign(ixkeys, c_index_keys(+term, +term, -term, [-integer])).
%foreign(max_it, c_max_it([-integer])).
foreign(decode_error, decode_error(+integer, [-string])).
%% foreign(db_normal_path, db_normal_path(+string, [-string])).
foreign(sync_db, c_sync_db(+address('db_struct'), [-integer])).

foreign(from_fastrw_old, c_from_fastrw_old(+integer/* C-block */, -integer /* D-block */, [-integer /* error-code */])).
foreign(from_fastrw_new, c_from_fastrw_new(+integer/* D-block */, +integer /* sizeof D */, -integer /* C-block */, -integer /* sizeof C-block */, [-integer /* error-code */])).

%% [PM] 4.1.3 not used.
%% foreign(get_fastrw_compatibility, c_get_fastrw_compatibility(-integer)).
%% foreign(set_fastrw_compatibility, c_set_fastrw_compatibility(+integer)).

%% [PM] 4.1.3 SPIDER/xref
:- public zerostat/0, printstat/0.


foreign(printstat, printstat).
foreign(zerostat, zerostat).

foreign_resource(bdb, 
  [
   init(db_init),
   deinit(db_deinit),
   open_env,close_env,
   open_db_read,open_db_update,open_db_enumerate,close_db,
   read_spec,next_termref,
   store_termref,store_term,
   fetch_term,delete_termref,delete_term,
   global_iterator,global_iterator_next,global_iterator_done,
   term_iterator,term_iterator_next,term_iterator_done,
   db_term_hash,
   ixkeys,
   decode_error,
   %% db_normal_path,
   sync_db
   ,from_fastrw_new, from_fastrw_old  % [PM] 3.11.0+
   %% ,get_fastrw_compatibility, set_fastrw_compatibility
   %% [PM] 4.2.2 Keep these for easy benchmarking (SPRM 11744)
  ,printstat,zerostat
  ]).

:- load_foreign_resource(library(system(bdb))).

%@  @end table
%@  
%@  @node An Example Session
%@  @subsection An Example Session
%@  @example
%@  @group
%@  | ?- @kbd{db_open(tempdb, update, [a(+,-)], DBRef), assert(tempdb(DBRef)).}
%@  DBRef = '$db'(1077241400)
%@  
%@  | ?- @kbd{tempdb(DBRef), db_store(DBRef, a(b,1), _).}
%@  DBRef = '$db'(1077241400)
%@  
%@  | ?- @kbd{tempdb(DBRef), db_store(DBRef, a(c,2), _).}
%@  DBRef = '$db'(1077241400)
%@  
%@  | ?- @kbd{tempdb(DBRef), db_fetch(DBRef, a(b,X), _).}
%@  X = 1,
%@  DBRef = '$db'(1077241400) ? @kbd{;}
%@  no
%@  
%@  | ?- @kbd{tempdb(DBRef), db_enumerate(DBRef, X, _).}
%@  X = a(b,1),
%@  DBRef = '$db'(1077241400) ? @kbd{;}
%@  X = a(c,2),
%@  DBRef = '$db'(1077241400) ? @kbd{;}
%@  no
%@  
%@  | ?- @kbd{db_current(DBName, Mode, Spec, EnvRef, DBRef).}
%@  Mode = update,
%@  Spec = [a(+,-)],
%@  DBRef = '$db'(1077241400),
%@  DBName = tempdb,
%@  EnvRef = none ? @kbd{;}
%@  no
%@  
%@  | ?- @kbd{tempdb(DBRef), db_close(DBRef).}
%@  DBRef = '$db'(1077241400)
%@  @end group
%@  @end example
%@  
%@  @node The DB-Spec
%@  @subsection The DB-Spec
%@  
%@  A db-spec has the form of a @var{speclist}:
%@  
%@  @table @var
%@  @item speclist
%@  = @code{[}@var{spec1}, @dots{}, @var{specM}@code{]}
%@  
%@  @item spec
%@  = @var{functor}@code{(}@var{argspec1}, @dots{}, @var{argspecN}@code{)}
%@  
%@  @item argspec
%@  = @code{+} | @code{-}
%@  @end table
%@  where @var{functor} is a Prolog atom.  The case @var{N} = 0 is
%@  allowed.
%@  
%@  A spec @var{F}@code{(}@var{argspec1}, @dots{}, @var{argspecN}@code{)} is
%@  @emph{applicable} to any nonvar term with principal functor
%@  @var{F}/@var{N}.
%@  
%@  When storing a term @var{T} we generate a hash code for every
%@  applicable spec in the db-spec, and a reference to @var{T} is stored
%@  with each of them.  (More precisely with each element of the set of
%@  generated hash codes).  If @var{T} contains nonvar elements on each
%@  @code{+} position in the spec, then the hash code depends on each of
%@  these elements.  If @var{T} does contain some variables on
%@  @code{+} position, then the hash code depends only on the functor
%@  of @var{T}.
%@  
%@  When fetching a term @var{Q} we look for an applicable spec for
%@  which there are no variables in @var{Q} on positions maked
%@  @code{+}.  If no applicable spec can be found a domain error is raised.
%@  If no spec can be found where on each @code{+} position a nonvar
%@  term occurs in @var{Q} an instantiation error is raised.
%@  Otherwise, we choose the spec with the most @code{+} postitions in
%@  it breaking ties by choosing the leftmost one.
%@  
%@  The terms that contain nonvar terms on every @code{+}
%@  postition will be looked up using indexing based on the principal
%@  functor of the term and the principal functor of
%@  terms on @code{+} postitions.  The other (more general)
%@  terms will be looked up using an indexing based on the principal
%@  functor of the term only.
%@  
%@  As can be seen, storing and fetching terms with variables on
%@  @code{+} positions are not vigorously supported operations.
%@  
%@  @node Exporting and importing a database
%@  @subsection Exporting and importing a database
%@  
%@  Since the database format of a Berkeley DB may change from version to
%@  version it may become necessary to migrate a database when upgrading.
%@  To this purpose there are two predicates available:
%@  @code{db_export/[2,3]} and @code{db_import/[2,3]}
%@  (@pxref{The Predicates}).
%@  
%@  The export/import feature was introduced in SICStus 3.12.0, but in
%@  that version you have to use @code{bdb:export/[2,3]} and
%@  @code{bdb:import/[2,3]}. Neither is exported from the bdb
%@  module, but can be used with module prefixing.
%@  
%@  Since the bdb interface prior to SICStus 4 uses a custom hash function,
%@  the standard Berkeley DB migration tools will not
%@  work when migrating a database from SICStus 3 to SICStus 4.

