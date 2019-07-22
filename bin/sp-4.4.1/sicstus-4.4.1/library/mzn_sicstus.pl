/* Copyright(C) 2013, Swedish Institute of Computer Science */

% A thin wrapper for calling minizinc with our solver.
:- module(mzn_sicstus, []).

:- use_module(library(process), [process_create/3]).
        

user:runtime_entry(start) :-
	prolog_flag(argv, Args),
        mzn_sicstus(Args, ExitCode),
        halt(ExitCode).

mzn_sicstus(Args, ExitCode) :-
        absolute_file_name(library('zinc/globals'), Globals),
        absolute_file_name(application(spfz), FLATZINC_CMD, [file_type(executable)]),
        process_create(path(minizinc), ['-I', file(Globals) | Args],
                       [wait(Result),
                        environment(['FLATZINC_CMD'=file(FLATZINC_CMD)])]),
        ( Result = exit(EC) ->
          ExitCode = EC
        ; Result = killed(Signal) ->
          ExitCode is 128 + Signal
        ; otherwise ->
          % internal error
          ExitCode = 42
        ).
