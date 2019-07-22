% :- set_prolog_flag(redefine_warnings,off).
%

% Pre-compile all the dynamically-generated predicates
% so they will be ready when needed for subsequent compilation
:- load_files(usemrep_lib(module_version), [
        when(compile_time)
    ]).


:- load_files(usemrep_lib(pre_compilation), [
       when(compile_time)
    ]).


% :- pre_compile_data_files(usemrep,['06', '12', '14'],['06', '_2012AA', '_2014AA']).
% :- pre_compile_data_files(usemrep,['06', '12', '14', '15'],['06', '_2012AA', '_2014AA', '_2015AA']).
% :- pre_compile_data_files(usemrep,['06', '12', '14', '15','17'],['06', '_2012AA', '_2014AA', '_2015AA','_2017AA']).
%:- pre_compile_data_files(usemrep,['06', '12', '14', '15','18'],['06', '_2012AA', '_2014AA', '_2015AA','_2018AA']).

:- use_module(skr_lib(nls_signal), [
	establish_signal_handling/0
    ]).

:- use_module(skr_lib(sicstus_utils), [
	ttyflush/0
    ]).

:- use_module(library(file_systems), [
	file_exists/1,
	file_property/3
    ]).

:- use_module(usemrep_main(usemrep), [
        fg/0,
        go/0,
        shutdown_usemrep/0
    ]).

:- use_module(library(random), [
        random/1,
        setrand/1
    ]).

:- use_module(library(system), [
	datime/1
    ]).

%%% Code provided by Mats Carlsson of SICS to FML via e-mail 03/27/2007:
%%% 
%%% There are two issues:
%%% 
%%% 1. The initial seed of the random number generator is always the
%%%    same. This is by design, so that you can reproduce the run with the
%%%    same result, which is sometimes desirable. To get different
%%%    sequences of random numbers each time, the seed has to be set
%%%    explicitly.
%%% 
%%% 2. There's a bug in maybe/[0,1] that makes it always fail the first
%%%    time, no matter what the seed is.
%%% 
%%% The piece of code below addresses both issues: it computes a random
%%% seed, and it calls maybe/0 once to compensate for the bug.
%%% --Mats
%%%

%%% SICStus version updated by Per Mildner.

:- initialization
	datime(Date),
	Date = datime(A,B,C,D,E,F),
	X is 1 + ((A*D) mod 30000),
	Y is 1 + ((B*E) mod 30000),
	Z is 1 + ((C*F) mod 30000),
	%% high bits matters so make W big
	random(R),
	W is 1 + integer(R*(1<<30)),
	setrand(random(X,Y,Z,W)).

runtime_entry(start) :-
	establish_signal_handling,
	go.
    
runtime_entry(abort) :-
	ttyflush,
	shutdown_usemrep,
	format(user_output,'Done.~n',[]).

pl_to_po :-
	source_file(FilePL),
	compile_to_PO_if_necessary(FilePL),
	fail
      ; true.

compile_to_PO_if_necessary(FilePL) :-
	( sub_atom(FilePL, _, _, _, 'SAW') ->
	  true
	; sub_atom(FilePL, _, _, _, 'SKR')
	),
	atom_concat(Prefix, '.pl', FilePL),
	atom_concat(Prefix, '.po', FilePO),
	% UNLESS the PO file exists and is more recent than the PL file,
	% compile the PL file to PO
	( file_exists(FilePO),
	  file_property(FilePL, modify_timestamp, TimeStampPL),
	  file_property(FilePO, modify_timestamp, TimeStampPO),
	  TimeStampPO > TimeStampPL ->
	  true
	; format(user_output, 'Saving ~w to ~w~n', [FilePL,FilePO]),
	  save_files(FilePL, FilePO)
	).
	   
:- pl_to_po.
