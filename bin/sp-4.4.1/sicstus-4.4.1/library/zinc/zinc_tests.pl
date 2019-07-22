/* Copyright(C) 2009, Swedish Institute of Computer Science */

%   File       : zinc_tests.pl
%   Author     : agren
%   Purpose    : Test file for library(zinc).

:- module(zinc_tests, [run_tests/0,
		       run_tests/1,
		       run_tests/2
		       ]).

:- use_module(library(clpfd)).
:- use_module(library(file_systems)).
:- use_module(library(lists)).
:- use_module(library(zinc)).
:- use_module(library(plunit)).

running_on_32_bit :-
    current_prolog_flag(max_tagged_integer, Max),
    Max < 1<<31.


cannot_run_on_32bits('grocery.fzn'). % [PM 4.2.1 Gets 'CLPFD integer overflow' on 32-bit
cannot_run_on_32bits('grocery.mzn'). % [PM 4.2.1 Gets 'CLPFD integer overflow' on 32-bit

can_run(File) :-
    running_on_32_bit,
    cannot_run_on_32bits(File),
    !,
    fail.
can_run(_File).

:- begin_tests(fzn).

test(conformance, [forall(fzn_conformance_pair(Fzn, ResFile, ExpFiles, FznOptions)),
		   true(check_expected(ResFile, ExpFiles))]) :-
	fzn_run_file(Fzn, FznOptions).

test(interface(1)) :-
	fzn_run_file(library('zinc/examples/queen4')).

test(interface(2)) :-
	fzn_run_file(library('zinc/examples/queen4'),
				[solutions(all), statistics(true)]).

test(interface(3)) :-
	fzn_run_file(library('zinc/examples/queen4'),
				[solutions(2)]).

test(interface(4)) :-
	fzn_load_file(library('zinc/examples/queen4'), _).

test(interface(5)) :-
	open(library('zinc/examples/queen4.fzn'), read, Stream),
	fzn_load_stream(Stream, _),
	close(Stream).

test(interface(6)) :-
	open(library('zinc/examples/queen4.fzn'), read, Stream),
	fzn_run_stream(Stream),
	close(Stream).

test(interface(7)) :-
	open(library('zinc/examples/queen4.fzn'), read, Stream),
	fzn_run_stream(Stream, [solutions(all)]),
	close(Stream).

test(interface(8), nondet) :-
	fzn_load_file(library('zinc/examples/queen4'), Queen4State),
	fzn_post(Queen4State),
	fzn_solve(Queen4State),
	fzn_output(Queen4State).

test(interface(9)) :-
	fzn_load_file(library('zinc/examples/queen4'), Queen4State),
	fzn_post(Queen4State),
	fzn_identifier(Queen4State, q, Q),
	findall(_, (labeling([], Q), fzn_output(Queen4State)), _).

test(interface(10)) :-
	fzn_load_file(library('zinc/examples/queen4'), Queen4State),
	fzn_post(Queen4State),
	fzn_identifier(Queen4State, q, Q),
	Q = [Q1, Q2|_],
	Q1 #< Q2,
	findall(_, (labeling([], Q), fzn_output(Queen4State)), _).

test(interface(11)) :-
	fzn_load_file(library('zinc/examples/queen4'), Queen4State),
	fzn_dump(Queen4State, [variables([q=_])], queen4),
	delete_file('queen4.pl').

:- end_tests(fzn).

:- begin_tests(mzn).

test(g12, [forall(mzn_case(F, Params))]) :-
	mzn_run_file(F, Params).

test(interface(1)) :-
	mzn_load_file(library('zinc/examples/G12/golomb'), [],
		      GolombState),
	fzn_objective(GolombState, O),
	fzn_identifier(GolombState, mark, Mark),
	labeling([minimize(O)], Mark),
	fzn_output(GolombState).

test(interface(2)) :-
	NQueens = ["int: n;", 
		   "array [1..n] of var 1..n: q;", 
		   "constraint forall (i in 1..n, j in i+1..n)", 
		   "(q[i] != q[j] /\\", 
		   "q[i] + i != q[j] + j /\\", 
		   "q[i] - i != q[j] - j);", 
	"solve satisfy;", 
	"output [\"A solution to the \", show(n),", 
		   "\" Queens problem: \", show(q), \"\\n\"];"],
	mzn_run_model(NQueens, [parameters([n=4])]).

test(interface(3)) :-
	NQueens = ["int: n;", 
		   "array [1..n] of var 1..n: q;", 
		   "constraint forall (i in 1..n, j in i+1..n)", 
		   "(q[i] != q[j] /\\", 
		   "q[i] + i != q[j] + j /\\", 
		   "q[i] - i != q[j] - j);", 
	"solve satisfy;", 
	"output [\"A solution to the \", show(n),", 
		   "\" Queens problem: \", show(q), \"\\n\"];"],
	mzn_run_model(NQueens,
		      [data_file(library('zinc/examples/queen4.dzn'))]).

test(interface(4)) :-
	NQueens = ["int: n=4;", 
		   "array [1..n] of var 1..n: q;", 
		   "constraint forall (i in 1..n, j in i+1..n)", 
		   "(q[i] != q[j] /\\", 
		   "q[i] + i != q[j] + j /\\", 
		   "q[i] - i != q[j] - j);", 
	"solve satisfy;", 
	"output [\"A solution to the \", show(n),", 
		   "\" Queens problem: \", show(q), \"\\n\"];"],
	mzn_run_model(NQueens).

test(interface(5)) :-
	NQueens = ["int: n0; int: n;", 
		   "array [1..n] of var 1..n: q;", 
		   "constraint forall (i in 1..n, j in i+1..n)", 
		   "(q[i] != q[j] /\\", 
		   "q[i] + i != q[j] + j /\\", 
		   "q[i] - i != q[j] - j);", 
	"solve satisfy;", 
	"output [\"A solution to the \", show(n),", 
		   "\" Queens problem: \", show(q), \"\\n\"];"],
	mzn_load_model(NQueens, [parameters([n0=4, n=n0])], _).

test(interface(6)) :-
	mzn_run_file(library('zinc/examples/queen'),
		     [data_file(library('zinc/examples/queen4.dzn'))]).

test(interface(7)) :-
	mzn_run_file(library('zinc/examples/queen'),
		     [solutions(all),
		      data_file(library('zinc/examples/queen4.dzn'))]).

test(interface(8)) :-
	mzn_run_file(library('zinc/examples/G12/queen_cp2')).

test(interface(9)) :-
	mzn_load_file(library('zinc/examples/queen'),
		      [parameters([n=4])], _Queen4State).

test(interface(10)) :-
	mzn_load_file(library('zinc/examples/queen'),
		      [parameters([n=4]), variables([q=Q])],
		      Queen4State),
	Q = [Q1, Q2|_],
	Q1 #< Q2,
	findall(_, (labeling([], Q), fzn_output(Queen4State)), _).

test(interface(11)) :-
	mzn_load_file(library('zinc/examples/queen'),
		      [parameters([n=2]), variables([q=Q]),
		       post(false)], Queen4State),
	findall(_, (labeling([], Q), fzn_output(Queen4State)), _).

test(interface(12)) :-
	mzn_to_fzn(library('zinc/examples/queen'),
		   [parameters([n=4])],
		   queen4),
	fzn_run_file(queen4),
	delete_file('queen4.fzn').

:- end_tests(mzn).

:- public fzn_conformance_pair/4. % referenced by test/2 option
fzn_conformance_pair(Fzn, ResFile, ExpFiles, FznOptions) :-
	absolute_file_name(library('zinc/tests/fcts'), FCTS),
	directories(FCTS, Fzn, ResFile, ExpFiles, FznOptions).

:- directories/5 is nondet.
directories(D, Fzn, ResFile, ExpFiles, FznOptions) :-
	files(D, Fzn, ResFile, ExpFiles, FznOptions).
directories(D, Fzn, ResFile, ExpFiles, FznOptions) :-
	directory_member_of_directory(D, _, SubDir),
	directories(SubDir, Fzn, ResFile, ExpFiles, FznOptions).

files(D, Fzn, ResFile, ExpFiles, FznOptions) :-
	file_member_of_directory(D, '*.fzn', FznBase, Fzn),
	atom_concat(FznBase0, '.fzn', FznBase),
	atom_concat(FznBase0, '.exp*', ExpPattern),
	file_members_of_directory(D, ExpPattern, ExpFiles0),
	keys_and_values(ExpFiles0, _, ExpFiles),
	open(temp('sptmp.res'), write, ResStream0,
	     [if_exists(generate_unique_name)]),
	stream_property(ResStream0, file_name(ResFile)),
	close(ResStream0),
	atom_concat(FznBase0, '.opt', OptBase),
	(   file_member_of_directory(D, OptBase, Opt)
	->  options(Opt, FznOptions0)
	;   FznOptions0 = []
	),
	FznOptions = [output(ResFile)|FznOptions0].

options(FznOpt, Options) :-
	open(FznOpt, read, Stream),
	parse_options(Stream, Lines),
	close(Stream),
	(   foreach(Line, Lines),
	    foreach(Option, Options)
	do  option(Line, Option)
	).

parse_options(Stream, Lines) :-
	read_line(Stream, Codes),
	(   Codes = end_of_file
	->  Lines = []
	;   atom_codes(Line, Codes),
	    Lines = [Line|Lines1],
	    parse_options(Stream, Lines1)
	).

option('--all', solutions(all)) :- !.
option(Option, _) :-
	format(user_error, '~n~n...ERROR: unexpected option ~a~n~n', [Option]),
	fail.

:- public check_expected/2.	% referenced by test/2 option
check_expected(ResFile, ExpFiles) :- !,
	member(ExpFile, ExpFiles),
	open(ResFile, read, ResStream),
	open(ExpFile, read, ExpStream),
	call_cleanup(compare_streams(ResStream, ExpStream),
		     (close(ResStream), close(ExpStream))).

compare_streams(S1, S2) :-
	get_code(S1, C1),
	get_code(S2, C2),
	(   C1 = C2
	->  (   C1 = -1
	    ->  true
	    ;   compare_streams(S1, S2)
	    )
	).

:- public mzn_case/2.		% referenced by test/2 option
mzn_case(F, Params) :-
	Dir = library('zinc/examples/G12'),
	file_member_of_directory(Dir, '*.mzn', F0, F),
        (   can_run(F0) -> true
        ;   format(user_error, '! Warning: Skipping test ~w (this is expected)', [F0]),
	    fail
        ),
	atom_concat(FznBase0, '.mzn', F0),
	atom_concat(FznBase0, '.*dzn', DatPattern),
	file_members_of_directory(Dir, DatPattern, DatFiles),
	(   DatFiles==[] ->
	    Params = [statistics(true), timeout(120000)]
	;   member(_-DatFile, DatFiles),
	    Params = [data_file(DatFile), statistics(true), timeout(120000)]
	).
