/* Copyright (C) 1995-2010, Swedish Institute of Computer Science. */

:- module(make_index, [make_library_index/1, make_index/0, make_index/1]).

:- ensure_loaded(mklibs).


:- use_module(library(file_systems), [
	directory_exists/2,
        file_exists/2,
	file_members_of_directory/2,
	current_directory/2
	]).
:- use_module(library(lists), [
	keys_and_values/3,
	delete/3
	]).
%--------------------------------------------------------------------------

make_index :-
        make_index(.).

make_index(D) :-
        % current_directory(_,D),
	prolog_flag(redefine_warnings, _, off),
        asserta(use_error_message_hook([])),
        %% [PM] 4.2.1 Not used
	%% absolute_file_name(D, A),
        %% asserta(my_library_directory(A)),
	%% load_files(mkindex),
	make_library_index(D).


% make_library_index(Dir)
% Make an INDEX.pl in the directory Dir
make_library_index(Dir) :-
	directory_exists(Dir, [read,write,search]),
	current_directory(OldDir, OldDir),
	call_cleanup((current_directory(_, Dir), make_library_index), current_directory(_, OldDir)).

make_library_index :-
	pl_files('.', PlFiles0, S1),
	%% CHR is gone:
        %% pl_files('chr', S1, S2),
        S1=S2,
	pl_files('linda', S2, []),
        delete(PlFiles0, f('INDEX.pl',_,_), PlFiles1), % [PM] Avoid sharing violation if INDEX.pl already exists
        delete(PlFiles1, f('index.pl',_,_), PlFiles), % [PM] 4.0 lower case for Win32
	open('INDEX.pl', write, IndexStream),
	make_library_index2(PlFiles, IndexStream),
	close(IndexStream).

%--------------------------------------------------------------------------
pl_files(Dir) -->
	{directory_exists(Dir, [read,search])}, !,
	{file_members_of_directory(Dir, Files)},
	{keys_and_values(Files, ShortFiles, _)},
	pl_files(ShortFiles, Dir).

% Note! RelFile means the file with path relative to the library directory

%% [MC] 3.8.6: made determinate
pl_files([], _) --> [].
pl_files([File|Files], Dir) -->
	{concat(Dir, File, RelFile)},
	(   {file_exists(RelFile, [read])},
	    {is_pl_file(File)}
	->  [f(RelFile,Dir,File)]
	;   []
	),
	pl_files(Files, Dir).

concat('.', A2, A3) :- !, A2=A3.
concat(A1, A2, A3) :-
	atom_concat(A1, /, A1S),
	atom_concat(A1S, A2, A3).

is_pl_file(File) :-
	% [PM] 4.1.3 genmakefile.pl is gone.
        % goneFile \== 'genmakefile.pl', % a Perl file
	atom_concat(_, '.pl', File).

%--------------------------------------------------------------------------
make_library_index2([], _).
make_library_index2([f(RelFile,Dir,File)|Files], IndexStream) :-
	prolog_flag(typein_module, Module),
 	scan_module_decl(RelFile, File, Dir, Module, IndexStream),
        make_library_index2(Files, IndexStream).

scan_module_decl(RelFile, File, Dir, Module0, IndexStream) :-
	open(RelFile, read, Stream),
	absolute_file_name(Dir, AbsDir),
 	asserta(prolog:'$load context'(Module0,File,Stream,AbsDir), Ref),
        Excp = error(_,_),
 	on_exception(Excp,
	             read(Stream, First),
		     (   format(user_error,
			        'Warning: While reading ~w:~n', [RelFile]),
			 print_message(error, Excp)
		     )
		    ),
	erase(Ref),
	close(Stream),
	(   First = (:- module(Module, Public)) ->
	    add_library_index(Public, RelFile, Module, IndexStream)
	;   true
	).

add_library_index([], _, _, _).
add_library_index([Name/Arity|T], RelFile, Module, IndexStream) :-
	% format(IndexStream, 'index(~q, ~d, ~q, ~q).~n', [Name, Arity, Module, RelFile]), 
        format(IndexStream, '~k.~n', [index(Name, Arity, Module, RelFile)]),
        !,
	add_library_index(T, RelFile, Module, IndexStream).
