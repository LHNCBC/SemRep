/*  Purpose:       Unit Test Harness
    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006-2008, University of Amsterdam
    Modified:      2010 by SICS for SICStus Prolog 4

    This file is covered by the `The Artistic License', also in use by
    Perl.  See http://www.perl.com/pub/a/language/misc/Artistic.html
*/

:- module(plunit, [
	  begin_tests/1,	% +Name
	  begin_tests/2,	% +Name, +Options
	  end_tests/1,		% +Name
	  run_tests/0,		% 
	  run_tests/1,		% +Specs
	  run_tests/2		% +Specs, +Options
	  % set_test_options/1	% SWI specific
	  % load_test_files/1	% SWI specific
	  % running_tests/0	% SWI specific
	  % test_report/1	% SWI specific
	  ]).

:- use_module(library(types)).
:- use_module(library(terms), [
	term_variables_set/2,
	subsumeschk/2
]).

:- meta_predicate
        valid_options(+, -, 1, +, +),
        call_det(0, -).

:- multifile
	user:term_expansion/6,
	user:generate_message_hook/3,
	'SU_messages':generate_message/3.

:- dynamic
	enabled/0,		% enabler for hooks
	loading_unit/4,		% Unit, Module, File, OldSource
	current_unit/4,		% Unit, Module, File, Options
	passed/5,		% Unit, Name, Line, Det, Time
	failed/4,		% Unit, Name, Line, Error
	blocked/4,		% Unit, Name, Line, Reason
	fixme/5,		% Unit, Name, Line, Reason, Ok
	seen_unit/1.
:- volatile
        % enabled/0,              % enabler for hooks
        loading_unit/4,         % Unit, Module, File, OldSource
        current_unit/4,         % Unit, Module, File, Options
        passed/5,               % Unit, Name, Line, Det, Time
        failed/4,               % Unit, Name, Line, Error
        blocked/4,              % Unit, Name, Line, Reason
        fixme/5,                % Unit, Name, Line, Reason, Ok
	seen_unit/1.

:- public
        test_option/1,
        test_set_option/1,
        passed/5,
        failed/4,
        run_option/1.


		 /*******************************
		 *	      MODULE		*
		 *******************************/

%%	begin_tests(+UnitName:atom) is det.
%%	begin_tests(+UnitName:atom, Options) is det.
%
%	Start a test-unit. UnitName is the  name   of  the test set. the
%	unit is ended by :- end_tests(UnitName).

begin_tests(Unit) :-
	begin_tests(Unit, []).

begin_tests(Unit, Options0) :-
	prolog_load_context(module, Module),
	prolog_load_context(source, Source),
	Excp = error(_,_),
	on_exception(Excp,
		     valid_options(Options0, Options, test_set_option, begin_tests(Unit,Options0), 2),
		     (   Options = [],
			 print_message(error, Excp)
		     )),
	(   current_unit(Unit, Module, Source, Options) -> true
	;   retractall(current_unit(Unit, Module, _, _)),
	    assertz(current_unit(Unit, Module, Source, Options))
	),
	asserta(loading_unit(Unit, Module, Source, -)).

%%	end_tests(+Name) is det.
%
%	Close a unit-test module.
%
%	@tbd	Run tests/clean module?
%	@tbd	End of file?

end_tests(Unit) :-
	loading_unit(StartUnit, _, _, _), !,
	(   Unit == StartUnit
	->  retractall(loading_unit(StartUnit, _, _, _))
	;   illarg(context(in_plunit(StartUnit),end_tests(Unit)), end_tests(Unit), 1)
	).
end_tests(Unit) :-
	illarg(context(not_in_plunit,end_tests(Unit)), end_tests(Unit), 1).


unit_module(Unit, Module) :-
	current_unit(Unit, Module, _, _), !.

		 /*******************************
		 *	     EXPANSION		*
		 *******************************/

%%	expand_test(+Name, +Options, +Body, -Clause) is det.
%
%	Expand test(Name, Options) :-  Body  into   a  clause  for
%	'unit test'/5 and 'unit body'/4.

expand_test(Name, Options0, Body,
	    [ 'unit test'(Unit, Name, File:Line, Options, Module:'unit body'(Unit, Name, File:Line, Vars)),
	      ('unit body'(Unit, Name, File:Line, Vars) :- !, Body)
	    ]) :-
	loading_unit(Unit, _, _, _), !,
	source_location(File, Line),
	prolog_load_context(module, Module),
	term_variables_set(Body, VarList),
	Vars =.. [vars|VarList],
	valid_options(Options0, Options, test_option, test(Name,Options0), 2).


%%	expand(+Term, -Clauses) is semidet.

expand((test(Name) :- Body), Clauses) :- !,
	expand_test(Name, [], Body, Clauses).
expand((test(Name, Options) :- Body), Clauses) :- !,
	expand_test(Name, Options, Body, Clauses).
expand(test(Name), Clauses) :- !,
	expand((test(Name) :- true), Clauses).
expand(test(Name, Options), Clauses) :- !,
	expand((test(Name, Options) :- true), Clauses).
% [PM] 4.3.2 end_of_file is now handled in caller instead.
%expand(end_of_file, _) :-
%	prolog_load_context(source, Source),
%	retractall(seen_unit(Source)),
%	loading_unit(Unit, _, _, _), !,
%	end_tests(Unit),		% warn?
%	fail.

user:term_expansion((:- begin_tests(Set)), Lay, Ids, Expanded, Lay2, Ids2) :-
	nonmember(plunit, Ids), !,
	user:term_expansion((:- begin_tests(Set,[])), Lay, Ids, Expanded, Lay2, Ids2).
user:term_expansion((:- begin_tests(Set,Opt)), _Lay, Ids, Expanded, [], [plunit|Ids]) :-
	nonmember(plunit, Ids),
	prolog_load_context(source, Source),
	seen_unit(Source), !,
	Expanded =  [ (:- plunit:begin_tests(Set,Opt)) ].
user:term_expansion((:- begin_tests(Set,Opt)), _Lay, Ids, Expanded, [], [plunit|Ids]) :-
	nonmember(plunit, Ids), !,
	prolog_load_context(source, Source),
	assertz(seen_unit(Source)),
	Expanded =  [ (:- plunit:begin_tests(Set,Opt)),
		      (:- discontiguous('unit body'/4)),
		      (:- discontiguous('unit test'/5))
		    ].
user:term_expansion((:- end_tests(Set)), _Lay, Ids, Expanded, [], [plunit|Ids]) :-
	nonmember(plunit, Ids), !,
	Expanded = ((:- plunit:end_tests(Set))).
user:term_expansion(end_of_file, _Lay0, _Ids0, _Expanded, _Lay, _Ids) :-
        prolog_load_context(source, Source),
        % [PM] 4.3.2 This ensures that we emit discontiguous directives when Source is re-loaded.
        retractall(seen_unit(Source)),
        fail.
user:term_expansion(end_of_file, _Lay0, _Ids0, _Expanded, _Lay, _Ids) :-
        % [PM] 4.3.2 the idea seems to be to catch EOF after begin_tests(Unit,...), before the matching
        % end_tests(Unit), but the way it is done here would do the wrong thing if some
        % file is loaded (or included?) between begin_tests and end_tests.
        % In either case, this code does the same as the code it replaces.
        once(loading_unit(Unit, _, _, _)),
        % warn? (premature end of file)
        end_tests(Unit),
        fail.
user:term_expansion(Term, _Lay, Ids, Expanded, [], [plunit|Ids]) :-
	nonmember(plunit, Ids),
	catch(enabled, error(_,_), fail),
	loading_unit(_, _, Source, _),
	prolog_load_context(source, Source),
	Excp = error(_,_),
	on_exception(Excp,
		     expand(Term,Expanded),
		     (   Expanded = [],
			 print_message(error, Excp)
		     )), !.

source_location(File, Line) :-
	prolog_load_context(file, File),
	prolog_load_context(term_position, Pos),
	stream_position_data(line_count, Pos, Line).

		 /*******************************
		 *	       OPTIONS		*
		 *******************************/

%%	valid_options(+Options, :Pred) is det.
%
%	Verify Options to be a list of valid options according to
%	Pred.
%
%	@throws	=type_error= or =instantiation_error=.

valid_options(Options0, Options, Pred, Goal, ArgNo) :-
	(   Options0 = [] ->    Options = []
	;   Options0 = [H|T] -> Options = [H|T]
	;   true ->             Options  = [Options0]
	),
	must_be(Options, proper_list(nonvar), Goal, ArgNo),
	(   foreach(Opt,Options),
	    param(Pred,Goal,ArgNo)
	do  (   call(Pred, Opt) -> true
	    ;   illarg(domain(term,option), Goal, ArgNo, Opt)
	    )
	).

%%	test_option(+Option) is semidet.
%
%	True if Option is a valid option for test(Name, Options).

test_option(Option) :-
	test_set_option(Option), !.
test_option(true).		% for compatibility
test_option(true(_)).
test_option(all(_)).
test_option(set(_)).
test_option(fail).
test_option(exception(_)).	% SICStus specific
test_option(throws(_)).
test_option(error(_)).
test_option(error(_,_)).	% SICStus specific
% test_option(sto(_)).		% SWI specific
test_option(nondet).
test_option(forall(X)) :-
	callable(X).

%%	test_option(+Option) is semidet.
%
%	True if Option is a valid option for :- begin_tests(Name,
%	Options).

test_set_option(blocked(X)) :-
	ground(X).
test_set_option(fixme(X)) :-
	ground(X).
test_set_option(condition(X)) :-
	callable(X).
test_set_option(setup(X)) :-
	callable(X).
test_set_option(cleanup(X)) :-
	callable(X).

		 /*******************************
		 *	  RUNNING TOPLEVEL	*
		 *******************************/

%%	run_tests is semidet.
%%	run_tests(+TestSet) is semidet.

run_tests :-
	findall(U, current_unit(U,_,_,_), Set),
	run_tests(Set, []).

run_tests(Raw) :-
	run_tests(Raw, []).

run_tests(Raw, Options0) :-	% Options are SICStus specific
	Goal = run_tests(Raw,Options0),
	valid_options(Options0, Options, run_option, Goal, 2),
	ensure_list(Raw, Set0),
	must_be(Set0, proper_list(nonvar), Goal, 1),
	(   foreach(Elt0,Set0),
	    foreach(Elt,Set),
	    param(Goal)
	do  (   atom(Elt0)
	    ->  Elt = Elt0
	    ;   Elt0 = (Name:Tests0),
		atom(Name),
		ensure_list(Tests0, Tests),
		must_be(Tests, proper_list(nonvar), Goal, 1)
	    ->  Elt  = (Name:Tests )
	    ;   illarg(domain(callable,test_spec), Elt, Goal, 1)
	    )
	),
	cleanup,
	(   foreach(Spec,Set),
	    param(Options)
	do  unit_from_spec(Spec, Unit, Tests1, Module, UnitOptions),
	    Context = unit(Unit),
	    (   member(blocked(Reason), UnitOptions)
	    ->  print_informational(plunit(blocked(unit(Unit, Reason))), Options)
	    ;   member(fixme(Reason), UnitOptions)
	    ->  print_informational(plunit(fixme(unit(Unit, Reason))), Options)
	    ;   setup(Module, Context, UnitOptions)
	    ->  print_informational(plunit(begin(Spec)), Options),
		call_cleanup(run_unit_tests(Unit, Tests1, Module, Options),
			     cleanup_unit_tests(Spec, Options, Module, Context, UnitOptions))
	    ;   true
	    )
	),
	report(Options).

run_option(quiet).
run_option(verbose).

ensure_list(X, L) :-
	(   nonvar(X), X = [_|_] -> L = X ; L = [X]   ).

run_unit_tests(Unit, Tests, Module, Verbosity) :-
	\+ (   (   Module:'unit test'(Unit, Name, FileLine, Options, Body),
		   memberchk(Name, Tests),
		   append(Options, Verbosity, OptionsV)
	       ),
	       \+run_test(Unit, Name, FileLine, OptionsV, Body)
	   ).

cleanup_unit_tests(Spec, Options, Module, Context, UnitOptions) :-
	print_informational(plunit(end(Spec)), Options),
	cleanup(Module, Context, UnitOptions).

unit_from_spec(Unit, Unit, _, Module, Options) :-
	atom(Unit), !,
	(   current_unit(Unit, Module, _, Options)
	->  true
	;   illarg(existence(unit_test,Unit,0), run_tests(Unit), 1)
	).
unit_from_spec(Unit:Tests, Unit, Tests, Module, Options) :-
	atom(Unit), !,
	(   current_unit(Unit, Module, _, Options)
	->  true
	;   illarg(existence(unit_test,Unit,0), run_tests(Unit:Tests), 1)
	).

cleanup :-
	retractall(passed(_, _, _, _, _)),
	retractall(failed(_, _, _, _)),
	retractall(blocked(_, _, _, _)),
	retractall(fixme(_, _, _, _, _)).

		 /*******************************
		 *	   RUNNING A TEST	*
		 *******************************/

%%	run_test(+Unit, +Name, +FileLine, +Options, +Body) is det.
%
%	Run a single test.

run_test(Unit, Name, FileLine, Options, _Body) :-
	member(blocked(Reason), Options), !,
	assertz(blocked(Unit, Name, FileLine, Reason)).
run_test(Unit, Name, FileLine, Options, Body) :-
	member(forall(Generator), Options), !,
	unit_module(Unit, Module),
	term_variables_set(Generator, Vars),
	\+ (   catch(call(Module:Generator), E, true),
	       \+ (   var(E)
		  ->  run_test_once(Unit, @(Name,Vars), FileLine, Options, Body)
		  ;   print_message(error, plunit(error(forall, test(Unit,Name,FileLine), E)))
		  )
	   ).
run_test(Unit, Name, FileLine, Options, Body) :-
	run_test_once(Unit, Name, FileLine, Options, Body).

run_test_once(Unit, Name, FileLine, Options, Body) :-
	unit_module(Unit, Module),
	Context = test(Unit,Name,FileLine),
	(   setup(Module, Context, Options)
	->  call_cleanup(call_test(Module, Body, Options, Context, Result),
			 cleanup(Module, Context, Options)),
	    report_result(Result, Options)
	;   true
	).

report_result(failure(Unit, Name, FileLine, How), Options) :-
	(   member(fixme(Reason), Options)
	->  assertz(fixme(Unit, Name, FileLine, Reason, failed))
	;   print_message(error, plunit(failed(Unit, Name, FileLine, How))),
	    assertz(failed(Unit, Name, FileLine, How))
	).
report_result(success(Unit, Name, FileLine, Det, Time), Options) :-
	(   (   Det == det
	    ;	memberchk(nondet, Options)
	    )
	->  Ok = passed
	;   Ok = nondet
	),
	(   member(fixme(Reason), Options)
	->  assertz(fixme(Unit, Name, FileLine, Reason, Ok))
	;   assertz(passed(Unit, Name, FileLine, Det, Time)),
	    print_informational(plunit(succeeded(Unit, Name, FileLine, Det, Time)), Options),
	    (   Ok == passed -> true
	    ;   print_message(warning, plunit(nondet(FileLine, Name)))
	    )
	).

call_test(Module, Body0, Options, Context, Result) :-
	get_options(Options, Body0, Body, ExitOpt),
	statistics(runtime, [T0,_]), 
	(   catch(call_det(Module:Body, Det), E, true)
	->  (   var(E)
	    ->  report_result(true(Det), T0, Context, Module, ExitOpt, Result)
	    ;   report_result(excp(E),   T0, Context, Module, ExitOpt, Result)
	    )
	;   report_result(fail, T0, Context, Module, ExitOpt, Result)
	).

%%	call_det(:Goal, -Det) is nondet.
%
%	True if Goal succeeded.  Det is unified to =true= if Goal left
%	no choicepoints and =false= otherwise.

call_det(Goal, Det) :-
	call_cleanup(Goal, Det0=true),
	( var(Det0) -> Det = nondet ; Det = det ).

report_result(GotExit, T0, test(Unit,Name,FileLine), Module, ExpExit, Result) :-
	statistics(runtime, [T1,_]),
	Time is (T1 - T0)/1000.0,
	report_result(GotExit, ExpExit, Time, Module, Unit, Name, FileLine, Result).

report_result(true(Det),  true(Cmp) , Time, Module, Unit, Name, FileLine, Result) :- !,
	(   catch(call(Module:Cmp), E, true)
	->  (   var(E)
	    ->  Result = success(Unit, Name, FileLine, Det, Time)
	    ;   print_message(warning, plunit(error(true, test(Unit,Name,FileLine), E))),
		Result = failure(Unit, Name, FileLine, error)
	    )
	;   Result = failure(Unit, Name, FileLine, wrong_answer(Cmp))
	).
report_result(true(_Det),  fail      , Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, succeeded(Time)).
report_result(true(_Det),  excp(_ExpE), _Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, no_exception).
report_result(fail,       true(_Cmp) , _Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, failed).
report_result(fail,       fail      , Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = success(Unit, Name, FileLine, det, Time).
report_result(fail,       excp(_ExpE), _Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, failed).
report_result(excp(GotE), true(_Cmp),  _Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, GotE).
report_result(excp(GotE), fail     ,  _Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, GotE).
report_result(excp(GotE), excp(ExpE), Time, _Module, Unit, Name, FileLine, Result) :- !,
	(   subsumeschk(ExpE, GotE)
	->  Result = success(Unit, Name, FileLine, det, Time)
	;   Result = failure(Unit, Name, FileLine, wrong_exception(ExpE,GotE))
	).

get_options(Options, Body0, Body, Exit) :-
	(   foreach(Opt,Options),
	    fromto(true(true),Exit1,Exit2,Exit),
	    fromto(Body0,     Body1,Body2,Body)
	do  (   Opt = true
	    ->  Exit2 = true(true),
		Body2 = Body1
	    ;   Opt = true(T)
	    ->  Exit2 = true(T),
		Body2 = Body1
	    ;   Opt = fail
	    ->  Exit2 = fail,
		Body2 = Body1
	    ;   Opt = exception(T)
	    ->  Exit2 = excp(T),
		Body2 = Body1
	    ;   Opt = throws(T)
	    ->  Exit2 = excp(T),
		Body2 = Body1
	    ;   Opt = error(T)
	    ->  Exit2 = excp(error(T,_)),
		Body2 = Body1
	    ;   Opt = error(T,U)
	    ->  Exit2 = excp(error(T,U)),
		Body2 = Body1
	    ;   Opt = all(Cmp1)
	    ->  Cmp1 =.. [F,X|Ys],
		Cmp2 =.. [F,Xs|Ys],
		Exit2 = true(Cmp2),
		Body2 = findall(X,Body1,Xs)
	    ;   Opt = set(Cmp1)
	    ->  Cmp1 =.. [F,X|Ys],
		Cmp2 =.. [F,Xs|Ys],
		Exit2 = true(Cmp2),
		Body2 = (findall(X,Body1,Xs0), sort(Xs0,Xs))
	    ;   Exit2 = Exit1,
		Body2 = Body1
	    )
	).

%%	setup(+Module, +Context, +Options) is semidet.
%
%	Call the setup handler and  fail  if   it  cannot  run  for some
%	reason. The condition handler is  similar,   but  failing is not
%	considered an error.  Context is one of
%
%	    * unit(Unit)
%	    If it is the setup handler for a unit
%	    * test(Unit,Name,FileLine)
%	    If it is the setup handler for a test

setup(Module, Context, Options) :-
	member(condition(Condition), Options),
	member(setup(Setup), Options), !,
	setup(Module, Context, [condition(Condition)]),
	setup(Module, Context, [setup(Setup)]).
setup(Module, Context, Options) :-
	member(setup(Setup), Options), !,
	(   catch(call(Module:Setup), E, true) -> true
	;   E = error(_,failed(Setup))
	),
	(   var(E) -> true
	;   print_message(error, plunit(error(setup, Context, E))),
	    fail
	).
setup(Module, Context, Options) :-
	member(condition(Setup), Options), !,
	(   catch(call(Module:Setup), E, true)
	->  (   var(E)
	    ->	true
	    ;	print_message(error, plunit(error(condition, Context, E))),
		fail
	    )
	;   fail
	).
setup(_, _, _).

%%	cleanup(+Module, +Context, +Options) is det.
%
%	Call the cleanup handler and succeed.	Failure	 or error of the
%	cleanup handler is reported, but tests continue normally.

cleanup(Module, Context, Options) :-
	member(cleanup(Cleanup), Options), !,
	(   catch(call(Module:Cleanup), E, true) -> true
	;   E = error(_,failed(Cleanup))
	),
	(   var(E) -> true
	;   print_message(warning, plunit(error(cleanup, Context, E)))
	).
cleanup(_, _, _).

		 /*******************************
		 *	      REPORTING		*
		 *******************************/

%%	report is semidet.
%
%	True if there are no errors.  If errors were encountered, report
%	them to current output and fail.

report(Options) :-
	number_of_clauses(passed/5, Passed),
	number_of_clauses(failed/4, Failed),
	print_informational(plunit(passed(Passed)), Options),
	print_informational(plunit(failed(Failed)), Options),
	report_blocked(Options),
	report_fixme(TuplesF, TuplesP, TuplesN, Options),
	append(TuplesP, TuplesN, TuplesPN),
	append(TuplesF, TuplesPN, Tuples),
	print_informational(plunit(fixme(Tuples)), Options).

number_of_clauses(F/A, N) :-
	(   current_predicate(F/A)
	->  functor(G, F, A),
	    findall(t, G, Ts),
	    length(Ts, N)
	;   N = 0
	).

report_blocked(Options) :-
	number_of_clauses(blocked/4, N),
	N > 0, !,
	print_informational(plunit(blocked(N)), Options),
	(   blocked(_, Name, FileLine, Reason),
	    print_informational(plunit(blocked(FileLine, Name, Reason)), Options),
	    fail
	;   true
	).
report_blocked(_).

report_fixme(TuplesF, TuplesP, TuplesN, Options) :-
	fixme(failed, TuplesF, Failed),
	fixme(passed, TuplesP, Passed),
	fixme(nondet, TuplesN, Nondet),
	print_informational(plunit(fixme(Failed, Passed, Nondet)), Options).

print_informational(_Term, Options) :-
	member(quiet, Options), !.
print_informational(Term, _) :-
	print_message(informational, Term).


fixme(How, Tuples, Count) :-
	findall(fixme(Unit, Name, FileLine, Reason, How),
		fixme(Unit, Name, FileLine, Reason, How), Tuples),
	length(Tuples, Count).

		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

locationprefix(File:Line) --> !,
	[ '~w:~d:'-[File,Line],nl,'\t'-[]].
locationprefix(test(_,_,FileLine)) --> !,
	locationprefix(FileLine).
locationprefix(unit(Unit)) --> !,
	[ 'PL-Unit: unit ~w: '-[Unit],nl,'\t'-[] ].
locationprefix(FileLine) -->
	{ illarg(domain(term,locationprefix), 0, 0, FileLine) }.

message(error(context_error(plunit_close(Name, -)), _)) --> !,
	[ 'PL-Unit: cannot close unit ~w: no open unit'-[Name],nl ].
message(error(context_error(plunit_close(Name, Start)), _)) -->
	[ 'PL-Unit: cannot close unit ~w: current unit is ~w'-[Name, Start],nl ].

message(plunit(nondet(FileLine, Name))) --> !,
	locationprefix(FileLine),
	test_name(Name),
	[ 'succeeded nondet'-[],nl ].
					% Unit start/end
message(plunit(begin(Unit))) --> !,
	[ 'PL-Unit: ~w '-[Unit],nl ].
message(plunit(end(_Unit))) --> !,
	[ 'done'-[],nl ].
message(plunit(blocked(unit(Unit, Reason)))) --> !,
	[ 'PL-Unit: ~w blocked: ~w'-[Unit,Reason],nl ].
message(plunit(fixme(unit(Unit, Reason)))) --> !,
	[ 'PL-Unit: ~w fixme: ~w'-[Unit,Reason],nl ].
message(plunit(fixme(Tuples))) --> !,
	(   foreach(fixme(_Unit, _Name, FileLine, Reason, How),Tuples)
	do  fixme_message(FileLine, Reason, How)
	).
					% Blocked tests
message(plunit(blocked(1))) --> !,
	[ 'one test is blocked:'-[],nl ].
message(plunit(blocked(N))) --> !,
	[ '~D tests are blocked:'-[N],nl ].
message(plunit(blocked(Pos, Name, Reason))) --> !,
	locationprefix(Pos),
	test_name(Name),
	[ '~w'-[Reason],nl ].
					% fail/success
message(plunit(no_tests)) --> !,
	[ 'No tests to run'-[],nl ].
message(plunit(all_passed(Count))) --> !,
	[ 'All ~D tests passed'-[Count],nl ].
message(plunit(failed(0))) --> !,
	[].
message(plunit(failed(1))) --> !,
	[ '1 test failed'-[],nl ].
message(plunit(failed(N))) --> !,
	[ '~D tests failed'-[N],nl ].
message(plunit(passed(N))) --> !,
	[ '~D tests passed'-[N],nl ].
message(plunit(fixme(0,0,0))) --> !,
	[].
message(plunit(fixme(Failed,0,0))) --> !,
	[ 'all ~D tests flagged FIXME failed'-[Failed],nl ].
message(plunit(fixme(Failed,Passed,0))) --> !,
	[ 'FIXME: ~D failed; ~D passed'-[Failed, Passed],nl ].
message(plunit(fixme(Failed,Passed,Nondet))) --> !,
	{ TotalPassed is Passed+Nondet },
	[ 'FIXME: ~D failed; ~D passed; (~D nondet)'-[Failed, TotalPassed, Nondet],nl ].
message(plunit(failed(_Unit, Name, FileLine, Failure))) --> !,
       locationprefix(FileLine),
       test_name(Name),
       failure(Failure).
message(plunit(succeeded(_Unit, Name, FileLine, Det, Time))) --> !,
       locationprefix(FileLine),
       test_name(Name),
       [ 'succeeded (~w) in ~2f seconds'-[Det,Time],nl ].
					% Setup/condition errors
message(plunit(error(Where, Context, Exception))) --> !,
	locationprefix(Context),
	test_name(Context),
	[ 'error in ~w'-[Where],nl ],
	error(Exception).

test_name(@(Name,Bindings)) --> !,
	[ 'test ~w (forall bindings = ~p): '-[Name, Bindings] ].
test_name(unit(_)) --> !.
test_name(test(_,Name,_)) --> !,
	test_name(Name).
test_name(Name) --> !,
	[ 'test ~w: '-[Name] ].

expected_got_ops_(Ex, E, Goals) -->
	['    Expected: ~q'-[Ex],nl],
	['    Got:      ~q'-[E ],nl],
	( { Goals = true } -> []
	; ['       with: ~q'-[Goals],nl]
	).


failure(Var) -->
	{ var(Var) }, !,
	[ 'Unknown failure?'-[],nl ].
failure(no_exception) --> !,
	[ 'should raise exception but did not'-[],nl ].
failure(succeeded(Time)) --> !,
	[ 'should fail but succeeded in ~2f seconds'-[Time],nl ].
failure(wrong_exception(Expected, Error)) --> !,
	{ copy_term(Expected-Error, Ex-E, Goals),
	  numbervars(Ex-E-Goals, 0, _)
	},
	[ 'wrong exception'-[], nl ],
	expected_got_ops_(Ex, E, Goals).
failure(wrong_answer(Cmp)) -->
	{ Cmp =.. [Op,Answer,Expected], !,
	  copy_term(Expected-Answer, Ex-A, Goals),
	  numbervars(Ex-A-Goals, 0, _)
	},
	[ 'wrong answer (compared using ~w)'-[Op], nl ],
	expected_got_ops_(Ex, A, Goals).
failure(wrong_answer(Test)) --> !,
	[ 'wrong answer: ~q - failed'-[Test], nl ].
failure(error(ISO,ClassicError)) --> !,
	['raised an error exception'-[],nl],
	error(error(ISO,ClassicError)).
failure(Why) -->
	[ '~q'-[Why],nl ].

error(error(_,ClassicError)) --> !,
	'SU_messages':generate_message(ClassicError), !.
error(Why) -->
	[ '~q'-[Why],nl ].

fixme_message(Location, Reason, failed) --> !,
	[ 'FIXME: ~w: ~w'-[Location, Reason],nl ].
fixme_message(Location, Reason, passed) --> !,
	[ 'FIXME: ~w: passed ~w'-[Location, Reason],nl ].
fixme_message(Location, Reason, nondet) --> !,
	[ 'FIXME: ~w: passed (nondet) ~w'-[Location, Reason],nl ].

user:generate_message_hook(Message) -->
	{catch(enabled, _, fail)},
	message(Message), !.

enabled.
