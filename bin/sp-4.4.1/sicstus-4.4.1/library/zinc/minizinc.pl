/* Copyright(C) 2008, Swedish Institute of Computer Science */

%   File         : minizinc.pl
%   Author       : agren
%   Purpose      : Predicates to handle MiniZinc programs.

:- module(minizinc, [mzn_load_file/2,
		     mzn_load_file/3,
		     mzn_load_model/2,
		     mzn_load_model/3,
		     mzn_run_file/1,
		     mzn_run_file/2,
		     mzn_run_model/1,
		     mzn_run_model/2,
		     mzn_to_fzn/2,
		     mzn_to_fzn/3,
		     mzn_to_fzn_dir/1]).

:- use_module(library(file_systems)).
:- use_module(library(lists)).
:- use_module(library(process)).
:- use_module(library(system)).
:- use_module(library(types)).

:- use_module(library('zinc/zinc_utils')).
:- use_module(library('zinc/flatzinc')).


mzn_run_file(MznFile) :-
	mzn_run_file(MznFile, [], mzn_run_file(MznFile)-0).

mzn_run_file(MznFile, Options) :-
	mzn_run_file(MznFile, Options, mzn_run_file(MznFile, Options)-2).

mzn_run_file(MznFile, Options, GoalArg) :-
	mzn2fzn(MznFile, Options, GoalArg, FznOptions, CleanUp),
	memberchk(fzn_file(FznFile), FznOptions),
	call_cleanup(fzn_run_file(FznFile,FznOptions),
		     cleanup(CleanUp)).

mzn_run_model(MznModel) :-
	mzn_run_model(MznModel, [], mzn_run_model(MznModel)-0).

mzn_run_model(MznModel, Options) :-
	mzn_run_model(MznModel, Options, mzn_run_model(MznModel, Options)-2).

mzn_run_model(MznModel, Options, GoalArg) :-
	model_to_tmp(MznModel, MznFile),
	call_cleanup(mzn_run_file(MznFile, Options, GoalArg),
		     delete_file(MznFile)).


mzn_load_file(MznFile, FznState) :-
	mzn_load_file(MznFile, [], mzn_load_file(MznFile, FznState)-0,
		      FznState).

mzn_load_file(MznFile, Options, FznState) :-
	mzn_load_file(MznFile, Options,
		      mzn_load_file(MznFile, Options, FznState)-2, FznState).

mzn_load_file(MznFile, Options, GoalArg, FznState) :-
	mzn2fzn(MznFile, Options, GoalArg, FznOptions, CleanUp),
	memberchk(fzn_file(FznFile), FznOptions),
	open(FznFile, read, FznStream),
	call_cleanup(fzn_load_stream(FznStream, FznState),
		     (
		       close(FznStream),
		       cleanup(CleanUp)
		     )),
	(   member(post(false), Options) ->  true
	;   fzn_post(FznState)
	),
	mzn_variables(Options, FznState).

mzn_load_model(MznModel, FznState) :-
	GoalArg = mzn_load_model(MznModel, FznState)-0,
	mzn_load_model(MznModel, [], GoalArg, FznState).

mzn_load_model(MznModel, Options, FznState) :-
	GoalArg = mzn_load_model(MznModel, Options, FznState)-2,
	mzn_load_model(MznModel, Options, GoalArg, FznState).

mzn_load_model(MznModel, Options, GoalArg, FznState) :-
	model_to_tmp(MznModel, MznFile),
	call_cleanup(mzn_load_file(MznFile, Options, GoalArg, FznState),
		     delete_file(MznFile)).

mzn_to_fzn(MznFile, FznFile) :-
	mzn_to_fzn(MznFile, [], mzn_to_fzn(MznFile, FznFile)-0, FznFile).

mzn_to_fzn(MznFile, Options, FznFile) :-
	mzn_to_fzn(MznFile, Options, mzn_to_fzn(MznFile, Options, FznFile)-2,
		   FznFile).

mzn_to_fzn_dir(Dir) :-
	file_member_of_directory(Dir, '*.mzn', F0, MznFile),
	atom_concat(FznBase0, '.mzn', F0),
	atom_concat(FznBase0, '.*dzn', DatPattern),
	file_members_of_directory(Dir, DatPattern, DatFiles),
	(   DatFiles==[] ->
	    atom_concat(ButSuffix, '.mzn', MznFile),
	    atom_concat(ButSuffix, '.fzn', FznFile),
	    Params = [statistics(true)]
	;   member(_-DatFile, DatFiles),
	    atom_concat(ButSuffix, '.dzn', DatFile),
	    atom_concat(ButSuffix, '.fzn', FznFile),
	    Params = [data_file(DatFile), statistics(true)]
	),
	mzn_to_fzn(MznFile, Params, FznFile),
	fail.
mzn_to_fzn_dir(_).


mzn_to_fzn(MznFile, Options, GoalArg, FznFile) :-
	zinc_open(FznFile, write, '.fzn', FznOutput),
	stream_property(FznOutput, file_name(FznAbs)),
	close(FznOutput),
	mzn2fzn(MznFile, [fzn_file(FznAbs)|Options], GoalArg, _, CleanUp),
	cleanup(CleanUp).

model_to_tmp(MznModel0, MznFile) :-
	append(MznModel0, MznModel),
	open(temp('sptmp.mzn'), write, MznStream,
	     [if_exists(generate_unique_name)]),
	stream_property(MznStream, file_name(MznFile)),
	format(MznStream, '~s', [MznModel]),
	close(MznStream).

% [PM] 4.2.3+ unreachable code.
:- if(false). 
copy_stream(FznInput, FznOutput) :-
	read_line(FznInput, Line1),
	(   fromto(Line1,Line2,Line3,end_of_file),
	    param(FznInput,FznOutput)
	do  format(FznOutput, '~s\n', [Line2]),
	    read_line(FznInput, Line3)
	).
:- endif.

mzn2fzn(MznFile0, Options, GoalArg, FznOptions, CleanUp0) :-
	zinc_options(Options, GoalArg, Options1),
	include(mzn_option, Options1, MznOptions),
	exclude(mzn_option, Options1, FznOptions0),
	mzn2fzn_path(GoalArg, Mzn2Fzn),
	absolute_file_name(library('zinc/globals'), LibDir,
			   [file_type(directory), access(exist)]),
	absolute_file_name(MznFile0, MznFile, [extensions(['.mzn']),
					       access(read)]),
	absolute_file_name((.), MznFileDir, [relative_to(MznFile),
					     file_type(directory)]),
	(   memberchk(fzn_file(FznBase), FznOptions0)
	->  FznOptions1 = FznOptions0,
	    CleanUp0 = CleanUp1,
	    zinc_open(FznBase, write, '.fzn', FznStream),
	    stream_property(FznStream, file_name(FznFile)),
	    close(FznStream)
	;   FznOptions1 = [fzn_file(FznFile)|FznOptions0],
	    CleanUp0 = [delete_file(FznFile)|CleanUp1],
	    open(temp('sptmp.fzn'), write, FznStream,
		 [if_exists(generate_unique_name)]),
	    stream_property(FznStream, file_name(FznFile)),
	    close(FznStream)
	),
	(   memberchk(ozn_file(OznBase), FznOptions1)
	->  FznOptions = FznOptions1,
	    CleanUp1 = CleanUp,
	    zinc_open(OznBase, write, '.ozn', OznStream),
	    stream_property(OznStream, file_name(OznFile)),
	    close(OznStream)
	;   FznOptions = [ozn_file(OznFile)|FznOptions1],
	    CleanUp1 = [delete_file(OznFile)|CleanUp],
	    open(temp('sptmp.ozn'), write, OznStream,
		 [if_exists(generate_unique_name)]),
	    stream_property(OznStream, file_name(OznFile)),
	    close(OznStream)
	),
	mzn_parameters(MznOptions, Parameters, CleanUp),
	append([ '--output-fzn-to-file', file(FznFile),
		 '--output-ozn-to-file', file(OznFile),
		 '-I', file(LibDir),
		 '-I', file(MznFileDir)
		 | Parameters],
	       [file(MznFile)], Args),
	process_create(Mzn2Fzn, Args, [stdin(null),	
				       stderr(pipe(StdErr)),
				       wait(ExitCode)]),
	call_cleanup(handle_exit(ExitCode, StdErr),
		     close(StdErr)).

mzn2fzn_path(GoalArg, Mzn2Fzn) :-
	( environ('MZN2FZN', Mzn2Fzn0) ->
            true
        ; otherwise ->
            Mzn2Fzn0 = path(mzn2fzn)
        ),
        Goal-_ = GoalArg,
        % [PM] 4.2.3 We could use absolute_file_name/4 but we want mzn2fzn_path as culprit.
	prolog:absolute_file_name(Mzn2Fzn0, Mzn2Fzn,
					[file_type(executable), access(execute)], Goal, 0).

:- multifile user:generate_message_hook/3.
user:generate_message_hook(Error) -->
	{Error = existence_error(_, _, mzn2fzn, _, _)}, !,
	['Existence error'-[], nl,
	 'mzn2fzn was not found or could not be run'-[], nl].

mzn_option(data_file(_)).
mzn_option(parameters(_)).
mzn_option(variables(_)).
mzn_option(post(_)).
mzn_option(optimise(_)).
mzn_option(optimize(_)).
mzn_option(search_dir(_)).

handle_exit(exit(0), _) :- !.
handle_exit(_, StdErr) :-
	read_line(StdErr, Line1),
	(   fromto(Line1,Line2,Line3,end_of_file),
	    param(StdErr)
	do  print_message(error, format('~s',[Line2])),
	    read_line(StdErr, Line3)
	),
	illarg(system(mzn2fzn), mzn2fzn, 0).

mzn_parameters([], [], []).
mzn_parameters([parameters(Pars)|Os], ['--data', DatFile|Ds],
	       [delete_file(DatFile)|Cs]) :- !,
	open(temp('sptmp.dzn'), write, DatStream,
	     [if_exists(generate_unique_name)]),
	call_cleanup((stream_property(DatStream, file_name(DatFile)),
		      write_parameters(Pars, DatStream)),
		     close(DatStream)),
	mzn_parameters(Os, Ds, Cs).
mzn_parameters([optimise(false)|Os], ['--no-optimise'|Ds], Cs) :- !,
	mzn_parameters(Os, Ds, Cs).
mzn_parameters([optimize(false)|Os], ['--no-optimise'|Ds], Cs) :- !,
	mzn_parameters(Os, Ds, Cs).
mzn_parameters([search_dir(D)|Os], ['-I', D|Ds], Cs) :- !,
	mzn_parameters(Os, Ds, Cs).
mzn_parameters([data_file(File0)|Os], ['--data', File|Ds], Cs) :- !,
	absolute_file_name(File0, File),
	mzn_parameters(Os, Ds, Cs).
mzn_parameters([_|Os], Ds, Cs) :-
	mzn_parameters(Os, Ds, Cs).

write_parameters([], _).
write_parameters([P|Ps], Stream) :-
	write(Stream, P),
	write(Stream, ';'),
	nl(Stream),
	write_parameters(Ps, Stream).

mzn_variables([], _).
mzn_variables([variables(Vars)|Os], State) :- !,
	bind_variables(Vars, State),
	mzn_variables(Os, State).
mzn_variables([_|Os], State) :-
	mzn_variables(Os, State).

bind_variables([], _).
bind_variables([Id=Var|Vars], State) :-
	fzn_identifier(State, Id, Var),
	bind_variables(Vars, State).

cleanup([]) :- !.
cleanup([G|Gs]) :-
	call(G),
	cleanup(Gs).
