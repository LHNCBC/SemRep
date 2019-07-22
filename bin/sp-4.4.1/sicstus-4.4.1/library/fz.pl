/* Copyright(C) 2013, Swedish Institute of Computer Science */

:- use_module(library(lists)).
:- use_module(library(random), [setrand/1]).
:- use_module(library(zinc), [fzn_run_file/2]).

usage :- write(user_error,
 'Usage: spfz [options] <file>\n\
       spfz --help for more information\n\
').

help :- help(user_output).

help(S) :- write(S,
 'SICStus FlatZinc interpreter\n\
 - Supported FlatZinc version: 1.6\n\
\n\
Options for SICStus/FlatZinc:\n\
        -help, --help, -?\n\
                print this help message\n\
        --version\n\
                print version\n\
        -n (unsigned int) default: 1\n\
                number of solutions (0 = all)\n\
        -a\n\
                return all solutions (equal to -n 0)\n\
        -search (bab, restart) default: bab\n\
                search engine variant\n\
        -time (unsigned int) default: no cutoff\n\
                time (in ms) cutoff\n\
        -r (int)\n\
                random seed\n\
        -s\n\
                emit statistics\n\
        -o (string) default: standard output stream\n\
                file to send output to\n\
'), flush_output(S).

version :-  version(user_output).

version(S) :- 
        current_prolog_flag(version_data, sicstus(Major,Minor,Rev,Beta,_)),
        ( Beta > 0 ->
          number_codes(Beta, BetaCodes),
          append("beta", BetaCodes, BetaSuffix)
        ; BetaSuffix = ""
        ),
        Tool = spfz,
        format(S, '~w (SICStus Prolog ~d.~d.~d~s)~n', [Tool, Major,Minor,Rev,BetaSuffix]), flush_output(S).

user:runtime_entry(start) :-
	prolog_flag(argv, Args),
	parse(Args, File, Options), !,
	fzn_run_file(File, Options).
user:runtime_entry(start) :-
	usage,
	halt(1).

parse(Args, File, Options) :-
	(   fromto(Args,Args1,Args2,[]),
	    fromto(Options0,Options1,Options2,[])
	do  parse_option(Options1, Options2, Args1, Args2)
	),
	select(file(File0), Options0, Options3),
        !,
        File = File0,
        Options = Options3.

parse_option(Opt, Opt) --> ['-help'], !,
	{help, halt(0)}.
parse_option(Opt, Opt) --> ['--help'], !,
        {help, halt(0)}.
parse_option(Opt, Opt) --> ['--version'], !,
        {version, halt(0)}.
parse_option(Opt, Opt) --> ['-?'], !,
	{help, halt(0)}.
parse_option([solutions(all)|Opt], Opt) --> ['-n','0'], !.
parse_option([solutions(N)|Opt], Opt) --> ['-n',A], !,
	{atom_codes(A, Codes), number_codes(N, Codes)}.
parse_option([solutions(all)|Opt], Opt) --> ['-a'], !.
parse_option([search(S)|Opt], Opt) --> ['-search',S], !.
parse_option([timeout(N)|Opt], Opt) --> ['-time',A], !,
	{atom_codes(A, Codes), number_codes(N, Codes)}.
parse_option(Opt, Opt) --> ['-r',A], !,
	{atom_codes(A, Codes), number_codes(N, Codes)},
	{setrand(N)}.
parse_option([statistics(true)|Opt], Opt) --> ['-s'], !.
parse_option([output(A)|Opt], Opt) --> ['-o',A], !.
parse_option([file(A)|Opt], Opt) --> [A],
	{\+atom_concat(-, _, A)}.
