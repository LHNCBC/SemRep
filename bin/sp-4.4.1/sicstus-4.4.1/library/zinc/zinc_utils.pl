/* Copyright(C) 2008, Swedish Institute of Computer Science */

%   File       : zinc_utils.pl
%   Author     : agren
%   Purpose    : Common predicates for files in library(zinc).

:- module(zinc_utils, [illarg_syntax/3,
		       safe_process_wait/2,
		       safe_close/1,
		       zinc_open/4,
		       zinc_options/3]).

:- use_module(library(lists)).
:- use_module(library(types)).
:- use_module(library(process)).
:- use_module(library(file_systems)).


:- multifile user:generate_message_hook/3.
user:generate_message_hook(syntax_error(_, _, zinc(ControlArg), _, _)) --> !,
	['Syntax error'-[], nl,
	 ControlArg, nl].

:- illarg_syntax/3 is throwing.
illarg_syntax(ControlArg, Goal, Arg) :-
	illarg(syntax(0, zinc(ControlArg), [], 0), Goal, Arg).

zinc_open(File, Mode, Ext, Stream) :-
	absolute_file_name(File, AbsFile, [extensions([Ext]), access(Mode)]),
	open(AbsFile, Mode, Stream).

safe_process_wait(Process, ExitCode) :-
	(   is_process(Process)
	->  process_wait(Process, ExitCode)
	;   true
	).

safe_close(Stream) :-
	(   var(Stream)
	->  true
	;   close(Stream)
	).

zinc_options([], _, []).
zinc_options([O|Os], Goal-ArgNo, [O1|Os1]) :-
	(   on_exception(error(_,_), zinc_option(O, O1), fail)
	->  zinc_options(Os, Goal-ArgNo, Os1)
	;   illarg(domain(term, zinc_option), Goal, ArgNo, O)
	).

zinc_option(data_file(File), data_file(File)) :-
	must_be(File, ground, nogoal, 0).
zinc_option(fzn_file(File), fzn_file(File)) :-
	must_be(File, ground, nogoal, 0).
zinc_option(ozn_file(File), ozn_file(File)) :-
	must_be(File, ground, nogoal, 0).
zinc_option(parameters(ListOfParDef), parameters(ListOfParDef)) :-
	(   foreach(Id=Value, ListOfParDef)
	do  must_be(Id, atom, nogoal, 0),
	    must_be(Value, ground, nogoal, 0)
	).
zinc_option(variables(ListOfVarDef), variables(ListOfVarDef)) :-
	(   foreach(Id=Variable, ListOfVarDef)
	do  must_be(Id, atom, nogoal, 0),
	    must_be(Variable, var, nogoal, 0)
	).
zinc_option(optimise(B), optimise(B)) :-
	bool_option(B).
zinc_option(optimize(B), optimize(B)) :-
	bool_option(B).
zinc_option(post(B), post(B)) :-
	bool_option(B).
zinc_option(statistics(B), statistics(B)) :-
	bool_option(B).
zinc_option('--search-dir'(Dir), search_dir(Dir)) :- % deprecated
	must_be(Dir, ground, nogoal, 0).
zinc_option(search_dir(Dir), search_dir(Dir)) :-
	must_be(Dir, ground, nogoal, 0).
zinc_option(search(S), search(S)) :-
	must_be(S, oneof([bab,restart]), nogoal, 0).
zinc_option(solutions(N), solutions(N)) :-
	on_exception(error(_,_), must_be(N, oneof([all]), nogoal, 0),
		     must_be(N, integer(>=(1)), nogoal, 0)).
zinc_option(output(File), output(File)) :-
	must_be(File, ground, nogoal, 0).
zinc_option(timeout(T), timeout(T)) :-
	integer(T),
	T > 0.

bool_option(true).
bool_option(false).


