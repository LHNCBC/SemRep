/* ----------------------------------------------------------------------
    File:	determinacy.pl
    Authors:	Dave Bowen and Peter Schachte
    SCCS:	@(#)det.pl	66.1 10/14/92
    Purpose:	Determinacy checking support

    Copyright (C) 1989 Quintus Computer Systems, Inc.  Permission is hereby
    granted for this program to be used or modified for any purpose whatsoever
    provided only that this copyright notice is retained in the source.

    Most functionality provided by this code has been superseded by
    the analysis performed by the SICStus Prolog IDE, SPIDER.

    This file contains support code for the two Quintus determinacy
    checkers, det and detcheck.
   ---------------------------------------------------------------------- */

:- module(determinacy, [
	    initialize/0,
	    initialize_file/1,
	    get_option/2,
	    set_option/2,
	    process/4,
	    forget_discontiguity/0,
	    saved_clauses/3,
	    bottom_up_detcheck/0,
	    print_decls/1,
	    pred_for_clause/3,
	    directive_pred/1,
	    print_error_message/1,
	    read_clause/2,
	    declared/4,
	    undeclare/2,
            %% [PM] 4.1 these are dynamic, asserted by det.pl. Therefore rightfully part of the interface.
            module_exports/2,
            file_processed/2
   ]).

:- meta_predicate set_option(+,:).

:- use_module(library(ordsets), [
	ord_intersection/3
	]).

:- meta_predicate conditional_is_true(0).

:- dynamic seen_pred/4.
:- volatile seen_pred/4.
:- dynamic declared/4.
:- volatile declared/4.
:- dynamic saved_clause/6.
:- volatile saved_clause/6.
:- dynamic reported_problem/4.
:- volatile reported_problem/4.
:- dynamic called/3.
:- volatile called/3.
:- dynamic file_processed/2.
:- volatile file_processed/2.
:- dynamic module_exports/2.
:- volatile module_exports/2.
:- dynamic inferred_nondet/6.
:- volatile inferred_nondet/6.
:- dynamic option_recursive/1.
:- volatile option_recursive/1.
:- dynamic detchecked/3.
:- volatile detchecked/3.



%  Clean up before (and/or after) checking some files

initialize :-
    retractall(seen_pred(_,_,_,_)),
    retractall(declared(_,_,_,_)),
    retractall(reported_problem(_,_,_,_)),
    retractall(inferred_nondet(_,_,_,_,_,_)),
    retractall(saved_clause(_,_,_,_,_,_)),
    retractall(detchecked(_,_,_)),
    retractall(file_processed(_,_)),
    retractall(called(_,_,_)),
    (	init_option,
	fail
    ;	true
    ).

%  Clean up before checking File.  This is subsumed by initialize/0.

initialize_file(File) :-
    retractall(seen_pred(_,_,_,File)),
    retractall(reported_problem(_,_,_,_)),
    retractall(file_processed(File, _)),
    retractall(saved_clause(_,_,_,_,File,_)),
    retractall(called(_,_,_)).


finished_with_pred(Mod:Name/Arity) :-
    assert(detchecked(Name,Arity,Mod)),
    retractall(saved_clause(Name,Arity,Mod,_,_,_)).


detchecked(Pred) :-
    (	ground(Pred) ->
	    Pred = Mod:Name/Arity,
	    detchecked(Name,Arity,Mod),
	    !
    ;	Pred = Mod:Name/Arity,
	detchecked(Name,Arity,Mod)
    ).


note_inferred_nondet(Mod:Name/Arity, Clausenum, File, Reason) :-
    (	inferred_nondet(Name, Arity, Mod, Clausenum, _, _) ->
	    true
    ;	assert(inferred_nondet(Name, Arity, Mod, File, Clausenum, Reason))
    ).

inferred_nondet(Pred, Clausenum, Reason, File) :-
    (	ground(Pred) ->
	    Pred = Mod:Name/Arity,
	    inferred_nondet(Name, Arity, Mod, File, Clausenum, Reason),
	    !
    ;	Pred = Mod:Name/Arity,
	inferred_nondet(Name, Arity, Mod, File, Clausenum, Reason)
    ).


/* ----------------------------------------------------------------------
   Simple data abstraction for handling determinacy checking options.
   get_option(+Option, -Value) gets the value of an option.
   set_option(+Option, +Value) sets the value of an option.
   backtracking over init_option initializes all options to default values.
   ---------------------------------------------------------------------- */

:- discontiguous get_option/2, set_option/2, init_option/0.


:- init_option/0 is nondet.
init_option :-
    retractall(option_recursive(_)),
    assert(option_recursive(false)).
get_option(recursive, Rec) :-
	option_recursive(Rec).
set_option(recursive, _:Rec) :-
	retractall(option_recursive(_)),
	assert(option_recursive(Rec)).


:- dynamic option_print_decls/1.
:- volatile option_print_decls/1.


init_option :-
    retractall(option_print_decls(_)),
    assert(option_print_decls(false)).
get_option(print_decls, Decls) :-
	option_print_decls(Decls).
set_option(print_decls, _:Decls) :-
	retractall(option_print_decls(_)),
	assert(option_print_decls(Decls)).


:- dynamic option_print_all_decls/1.
:- volatile option_print_all_decls/1.

init_option :-
    retractall(option_print_all_decls(_)),
    assert(option_print_all_decls(false)).
get_option(print_all_decls, Decls) :-
	option_print_all_decls(Decls).
set_option(print_all_decls, _:Decls) :-
	retractall(option_print_all_decls(_)),
	assert(option_print_all_decls(Decls)).


:- dynamic process_stream/4.
:- volatile process_stream/4.

:- public  process_stream_directives/4.

init_option :-
    retractall(process_stream(_,_,_,_)),
    assert((process_stream(S, Path, Mod, Filemod) :-
		determinacy:process_stream_directives(S, Path, Mod, Filemod))).
get_option(process_dependency, M:Pred) :-
	clause(process_stream(_,_,_,_), M:Body), % SP/QP
	functor(Pred, 4, Body).
set_option(process_dependency, M:Pred) :-
	retractall(process_stream(_,_,_,_)),
	Body =.. [Pred, S, Path, Mod, Filemod],
	assert((process_stream(S, Path, Mod, Filemod):-M:Body)).


/* ----------------------------------------------------------------------
    Processes op, discontiguous, and multifile declarations.  Handle load_file
    directives with when(compile_time) or when(both) specified, since these
    may specify files that define operators or even term_expansion clauses
    that will be needed to process this file.  module directives are used to
    note the current module.  Other directives are ignored.
   ---------------------------------------------------------------------- */

directive_pred(_:(:-)/1) :- !.
directive_pred(_:(?-)/1).

process_directive(op(P,T,O), _, _Where) :- !,
    op(P,T,O).
process_directive(load_files(Fs, Opts), Mod, Where) :-
    append(Front, [when(When)|Back], Opts),
    !,
    (	When == run_time -> % SP: can't happen
	    process_dependencies(Fs, Mod, Where)
    ;	prolog_load_context(stream, _) ->
	    true		% we're running from term_expansion: no need
				% to load file, the compile will do it for us
    ;	append(Front, Back, Opts1),
	% Special case:  don't load library detcheck, as it will duplicate
	% every warning we issue through term_expansion.
	(   Fs = [_|_] -> Fs1 = Fs
	;   Fs1 = [Fs]
	),
	(   append(FrontFs, [library(detcheck)|BackFs], Fs1) ->
		append(FrontFs, BackFs, Fs2)
	;   Fs2 = Fs
	),
	Mod:load_files(Fs2, Opts1)
    ).
process_directive(load_files(Fs), Mod, Where) :- !,
    process_dependencies(Fs, Mod, Where).
process_directive(compile(Fs), Mod, Where) :- !,
    process_dependencies(Fs, Mod, Where).
process_directive(consult(Fs), Mod, Where) :- !,
    process_dependencies(Fs, Mod, Where).
process_directive(ensure_loaded(Fs), Mod, Where) :- !,
    process_dependencies(Fs, Mod, Where).
process_directive(include(Fs), Mod, Where) :- !, % SP
    process_dependencies(Fs, Mod, Where).
process_directive(use_module(Fs), Mod, Where) :- !,
    process_dependencies(Fs, Mod, Where).
process_directive(use_module(F,Imports), Mod, Where) :- !,
    process_dependency(F, Mod, Imports, Where).
process_directive(use_module(_,F,Imports), Mod, Where) :- !,
    process_dependency(F, Mod, Imports, Where).
process_directive([F|Fs], Mod, Where) :- !,
    process_dependencies([F|Fs], Mod, Where).
process_directive(discontiguous(P), Mod, _Where) :- !,
    declare(P, Mod, discontiguous).
process_directive(multifile(P), Mod, _Where) :- !,
    declare(P, Mod, multifile).
process_directive(dynamic(P), Mod, _Where) :- !,
    declare(P, Mod, dynamic).
process_directive((P,Q), Mod, Where) :- !,
    process_directive(P, Mod, Where),
    process_directive(Q, Mod, Where).
process_directive(nondet(P), Mod, _Where) :- !,
    declare(P, Mod, nondet).
process_directive(det(P), Mod, _Where) :- !, % [MC]
    declare(P, Mod, det).
process_directive((P is D), Mod, _Where) :-
    % [PM] 4.2.1 Accept "f/2 is det" and "f(+,-) is det" as a synonym for det(f/2). Ditto for f//2 and //(foo(+,-) (plDoc inspired.)    
    translate_is_directive(P, D, Mod, MFA, Decl),
    !,
    declare(MFA, Mod, Decl).

process_directive(_, _, _Where).

% Succeeds if P is D can be interpreted as a legacy det/1 or nondet/1
translate_is_directive(P, D, Mod, MFA, Decl) :-
        translate_is_directive_predspec(P, Mod, MFA1),
        translate_is_directive_detspec(D, Decl1),
        !,
        MFA = MFA1,
        Decl = Decl1.

translate_is_directive_predspec(P, _Mod, _MFA) :-
        var(P),
        !,
        fail.
translate_is_directive_predspec(M:P, _Mod, MFA) :-
        !,
        translate_is_directive_predspec(P, M, MFA).
translate_is_directive_predspec(F/A, Mod, MFA) :-
        atom(F),
        integer(A),
        A >= 0,
        !,
        MFA = Mod:F/A.
% plDoc "foo(+)// is det" denotes foo//1, i.e. foo/3. (You would need something like op(500, yf, //) for this to look good.)
translate_is_directive_predspec(//(Template), Mod, MFA) :-
        callable(Template),
        !,
        functor(Template,F,A1),
        A is A1+2,
        MFA = Mod:F/A.
translate_is_directive_predspec(Template, Mod, MFA) :-
        callable(Template),
        !,
        functor(Template,F,A),
        MFA = Mod:F/A.



translate_is_directive_detspec(D, _Decl) :-
        var(D),
        !,
        fail.
translate_is_directive_detspec(det, Decl) :-
        Decl = (det).
translate_is_directive_detspec(nondet, Decl) :-
        Decl = (nondet).
%% semidet, failing, throwing are known by SPIDER detcheck and semidet is used by plDoc
translate_is_directive_detspec(semidet, Decl) :-
        Decl = (det).
translate_is_directive_detspec(failing, Decl) :-
        Decl = (det).
translate_is_directive_detspec(throwing, Decl) :-
        Decl = (det).



/* ----------------------------------------------------------------------
    Read a clause.  Does expand_term, mainly to take care of grammar rules.
   ---------------------------------------------------------------------- */

:- dynamic tmp_clause/1.
:- volatile tmp_clause/1.


read_clause(S, Clause) :-
        read_clause_0(S, Clause0),
        ( Clause0 == end_of_file ->
          conditional_end_of_file_cleanup(S)
        ; true
        ),
        Clause = Clause0.

read_clause_0(_, Clause) :-
	retract(tmp_clause(Clause)), !.
read_clause_0(S, Clause) :-
	on_exception(Err, read_clause_1(S, Clause),
		     recover_error(Err, S, Clause)).

read_clause_1(S, Clause) :-
    read(S, X),
    read_clause_2(X, S, Clause).


read_clause_2(X, S, Clause) :-
    process_conditional(X, S), !,
    read_clause_1(S, Clause).
read_clause_2(_X, S, Clause) :-
    skipping(S), !,
    read_clause_1(S, Clause).
read_clause_2(X, S, Clause) :-
    expand_term(X, Expanded),
    (   Expanded == []		% SP
    ->  read_clause_1(S, Clause)
    ;   nonvar(Expanded),
	Expanded = [Clause|Rest]
    ->  (   foreach(C,Rest)
	do  assertz(tmp_clause(C))
	)
    ;   Expanded = Clause
    ).

   %  Avoid infinite error loop when reading past end of file.
recover_error(error(permission_error(input, past_end_of_stream, _Stream), _), _, end_of_file) :- !. % SP
recover_error(Error, S, Clause) :-
	print_error_message(Error),
	read_clause_0(S, Clause).

%% state(processing (skip/do), seen true branch (true/false), seen else branch (true/false), Stream).
%% Stream is present so we get one state stack per stream (for nested processing of files)
:- dynamic state/4.
:- volatile state/4.


%% True if in false branch of conditional
skipping(S) :-
        state(skip, _, _, S),
        !.

%% Update state if called with conditional, else fail.
process_conditional(X, _S) :- var(X), !, fail.
process_conditional((:- Directive), S) :-
        process_conditional_1(Directive, S).

%% FIXME: Beware of if-stack spilling over between files (can only happen for erroneous files).
process_conditional_1(Directive, _S) :- var(Directive), !, fail.
process_conditional_1(if(Test), S) :-
        % [PM] 4.2.1 fake a sane module context. See SPRM 12251
        Mod = user,     
        ( skipping(S) ->
          %% continue skipping, pretend we have already seen true branch, not seen else branch
          asserta(state(skip, true, false, S))
        ; conditional_is_true(Mod:Test) ->
          %% do, seen true branch, not seen else branch
          asserta(state(do, true, false, S))
        ; otherwise ->
          %% skip, not seen true branch, not seen else branch
          asserta(state(skip, false, false, S))
        ).
process_conditional_1(elif(Test), S) :- 
        % [PM] 4.2.1 fake a sane module context. See SPRM 12251
        Mod = user,    
        ( retract(state(_, SeenTrue, SeenElse, S)) ->
          ( SeenElse == true ->
            %% mismatched elif, bail out
            asserta(state(skip, true, false, S)),
            print_message(error, mismatched_conditional((:- elif(Test))))
          ; SeenTrue == true ->
            %% skip, seen true branch, not seen else branch
            asserta(state(skip, true, false, S))
          ; conditional_is_true(Mod:Test) ->
            %% do, seen true branch, not seen else branch
            asserta(state(do, true, false, S))
          ; otherwise ->
            %% do, seen true branch, not seen else branch
            asserta(state(skip, true, false, S))
          )
        ; otherwise ->
          %% mismatched elif, bail out
          asserta(state(skip, true, false, S)),
          print_message(error, mismatched_conditional((:- elif(Test))))
        ).
process_conditional_1(else, S) :- 
        ( retract(state(_, SeenTrue, SeenElse, S)) ->
          ( SeenElse == true ->
            %% mismatched else, bail out
            asserta(state(skip, true, true, S)),
            print_message(error, mismatched_conditional((:- else)))
          ; SeenTrue == true ->
            %% skip, seen true branch, seen else branch
            asserta(state(skip, true, true, S))
          ; otherwise ->
            %% do, seen true branch, seen else branch
            asserta(state(do, true, true, S))
          )
        ; otherwise ->
          %% mismatched else, bail out
          asserta(state(skip, true, true, S)),
          print_message(error, mismatched_conditional((:- else)))
        ).
process_conditional_1(endif, S) :- 
        ( retract(state(_, _, _, S)) ->
          true
        ; otherwise ->
          %% mismatched endif, ignore
          print_message(error, mismatched_conditional((:- endif)))
        ).

%% Test whether a if/1 or elif/1 condition is true. Prints a message and fails on error.
conditional_is_true(Test) :-
        on_exception(E,
                     \+ \+ call(Test),
                     (print_message(error, E),
                      fail
                     )
                    ).

%% Pop (and warn for) any unterminated conditonal directives for this stream
conditional_end_of_file_cleanup(S) :-
        ( state(_, _, _, S) ->
          findall(state(Do,SeenTrue,SeenElse), retract(state(Do,SeenTrue,SeenElse, S)), Conds),
          print_message(error, unterminated_conditionals(Conds))
        ; true
        ).

/* ----------------------------------------------------------------------
    Warn if we have seen this pred before.
   ---------------------------------------------------------------------- */

check_pred_contiguity(Pred, File) :-
    Pred = Mod:Name/Arity,
    (	seen_pred(Name, Arity, Mod, File),
	\+ declared(Name, Arity, Mod, discontiguous) ->
	    report_missed_some(Pred, discontiguous)
    ;	seen_pred(Name, Arity, Mod, File2),
	File2 \== File,
	\+ declared(Name, Arity, Mod, multifile) ->
	    report_missed_some(Pred, multifile)
    ;   assert(seen_pred(Name, Arity, Mod, File))
    ).


report_missed_some(Pred, Property) :-
    Pred = Mod:Name/Arity,
    (	reported_problem(Name, Arity, Mod, Property) ->
	    true
    ;	print_message(error, late_or_missing_declaration(Pred, Property)),
	assert(reported_problem(Name, Arity, Mod, Property))
    ).



/* ----------------------------------------------------------------------
    Keep track of predicates declared discontiguous or multifile.
   ---------------------------------------------------------------------- */

declared(Mod:Name/Arity, Prop) :-
    declared(Name, Arity, Mod, Prop),
    !.

declare(Var, _, Prop) :-
    var(Var),
    !,
    Decl =.. [Prop,Var],
    (	prolog_load_context(stream, _),
	Prop\==nondet, Prop\==det -> true % [MC] compiler will complain
    ;	print_message(error, instantiation_error(Decl, 1))
    ).
declare((Spec1,Spec2), Mod, Prop) :-
    !,
    declare(Spec1, Mod, Prop),
    declare(Spec2, Mod, Prop).
declare([Spec1,Spec2], Mod, Prop) :- % [PM] 4.1
    !,
    declare(Spec1, Mod, Prop),
    declare(Spec2, Mod, Prop).
declare(Name/Arity, Mod, Prop) :-
    !,
    declare(Name, Arity, Mod, Prop).
declare(Name//DCGArity, Mod, Prop) :-
    % [PM] 4.2.1 F//A denotes a DCG, i.e. a predicate with arity A+2. Currently undocumented.
    !,
    Arity is DCGArity+2,
    declare(Name, Arity, Mod, Prop).
declare(Mod:Spec, _, Prop) :-
    !,
    declare(Spec, Mod, Prop).
declare(Spec, _, Prop) :-
    !,
    Decl =.. [Prop,Spec],
    (	prolog_load_context(stream, _),
	Prop\==nondet, Prop\==det -> true % [MC] compiler will complain
    ;	print_message(error, domain_error(Decl, 0, predicate_specification,
					  Spec))
    ).


declare(Name, Arity, Mod, Prop) :-
    (	atom(Name),
	integer(Arity),
	atom(Mod) ->
	    (	declared(Name, Arity, Mod, Prop) ->
		    true
	    ;	(   (   seen_pred(Name, Arity, Mod, _)
		    ;	Prop == (dynamic),
			called(Name, Arity, Mod)
		    ) ->	     
			report_missed_some(Mod:Name/Arity, Prop)
		;   true
		),
		assert(declared(Name, Arity, Mod, Prop))
	    )
    ;	true
    ).


undeclare(Mod:Name/Arity, Prop) :-
    retractall(declared(Name, Arity, Mod, Prop)).


% Once we've reached the end of the file, don't treat anything as discontiguous
forget_discontiguity :-
    retractall(declared(_,_,_,discontiguous)).

/* ----------------------------------------------------------------------
    Check determinacy of all loaded files bottom-up, one SCC at a time.

    The determinacy of a predicate depends in part on the determinacy of the
    predicates that it calls, so we want to detcheck all callees before
    checking the caller.  Directly recursive predicates are handled quite
    easily, because of the nature of determinacy analysis.  We never conclude
    a predicate is deterministic just because a predicate it calls is
    nondeterministic, though it may be deterministic despite calling
    nondeterministic predicates.  So if we ignore directly recursive calls and
    find the predicate is nondeterministic, then the predicate is
    nondeterministic.  And if, ignoring directly recursive calls, we find the
    predicate is deterministic, then the predicate *is* deterministic.  Thus
    we do not need to consider directly recursive calls at all, or,
    equivalently, we can consider it all directly recursive calls to be
    deterministic.

    Indirectly recursive calls are another matter, however.  If we detcheck a
    predicate assuming some predicates it calls are deterministic, and later
    determine that they are nondeterministic, then we must reanalyze that
    predicate in the light of the new information.  This process must repeat
    until no new information is inferred.  This is guaranteed to terminate,
    because once a predicate is determined to be nondeterministic, we can
    never go back to thinking it deterministic, and there are a finite number
    of predicates to analyze.

    For recursive checking, we initially ignore the declared nondeterminism,
    and only use inferred nondeterminism.  When we're done, though, we only
    warn about undeclared inferred nondeterminism, and also where
    deterministic predicates are declared nondeterminstic.

---------------------------------------------------------------------- */

bottom_up_detcheck :-
    (	saved_clauses(File, Pred, Clauses),
	bottom_up_detcheck(Clauses, Pred, File, [], [], _, [], _, false, _),
	fail
    ;	bagof(ndet(Pred,Clause,Reason),
	      nondet_in_file(false/*MC*/, Pred, Clause, Reason, File),
	      Ndets),
	print_message(warning, in_file(File)),
	member(ndet(Pred,Clause,Reason), Ndets),
	print_message(warning, nondet(Pred,Clause,Reason)),
	fail
    ;	true
    ).


nondet_in_file(All, Pred, Clause, Reason, File) :-
    inferred_nondet(Pred, Clause, Reason, File),
    \+ declared(Pred, det),	% [MC]
    (   declared(Pred, nondet) -> All==true
    ;	true
    ).


%  Detcheck Pred and all the predicates it calls, if is hasn't already been
%  checked.  If we're checking it now, then it's a recursive case to be
%  handled depending on whether it's direct or indirect recursion.

bottom_up_detcheck(Clauses, Pred, File, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :-
    (	detchecked(Pred) ->
	    % Don't analyze it again if we're finished with it
	    ND = ND0,
	    SCC = SCC0,
	    Backto = Backto0
    ;	Callchain = [Pred|_] ->
	    % direct recursion:  ignore recursive call
	    ND = ND0,
	    SCC = SCC0,
	    Backto = Backto0
    ;	Backto = [Pred|_],
	append(NewSCC, Backto, Callchain) ->
	    % indirect recursion:  we've found a nontrivial SCC
	    (	SCC0 = [] -> SCC = [Pred|NewSCC]
	    ;	% this may duplicate elements, but that's harmless.
		append([Pred|NewSCC], SCC0, SCC)
	    ),
	    ND = ND0
    ;	Clauses=[Mod:_|_],
	detcheck_pred_dependencies(Clauses, [Pred|Callchain], Backto0, Backto1,
				   SCC0, SCC1, ND0, ND1),
	(   inferred_nondet(Pred, _, _, _) ->
		% No point analyzing it again if we already know it's nondet
		ND2 = ND1
	;   process_pred(Clauses, 0, Mod, Pred, File, ND1, ND2)
	),
	(   Backto1 = [Pred|_] ->
		(   ND2 \== ND0 ->
			bottom_up_detcheck(Clauses, Pred, File, Callchain,
					   Backto0, _, SCC0, SCC, ND0, ND),
			Backto0 = []
		;   Backto = [],
		    SCC = [],
		    ND = false,
		    (   foreach(SCCp,SCC1)
		    do  finished_with_pred(SCCp)
		    )
		)
	;   Backto1 = [] ->
		Backto = [],
		SCC = SCC1,
		ND = ND2,
		finished_with_pred(Pred)
	;   Backto = Backto1,
	    SCC = SCC1,
	    ND = ND2
	)
    ).


detcheck_pred_dependencies([], _, Backto, Backto, SCC, SCC, ND, ND).
detcheck_pred_dependencies([Defmod:Cl|Cls], Callchain, Backto0, Backto,
		SCC0, SCC, ND0, ND) :-
    detcheck_clause_dependencies(Cl, Defmod, Callchain, Backto0, Backto1,
				 SCC0, SCC1, ND0, ND1),
    detcheck_pred_dependencies(Cls, Callchain, Backto1, Backto, SCC1, SCC,
			       ND1, ND).


detcheck_clause_dependencies((_:-Body), M, Callchain, Backto0, Backto,
		SCC0, SCC, ND0, ND) :-
    !,
    detcheck_body_dependencies(Body, M, Callchain, Backto0, Backto, SCC0, SCC,
			       ND0, ND).
detcheck_clause_dependencies(_, _, _, Backto, Backto, SCC, SCC, ND, ND).

detcheck_body_dependencies(Var, _, _, Backto, Backto, SCC, SCC, ND, ND) :-
    var(Var),
    !.
detcheck_body_dependencies(M:B, _, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :-
    !,
    detcheck_body_dependencies(B, M, Callchain, Backto0, Backto, SCC0, SCC,
			       ND0, ND).
detcheck_body_dependencies((B1,B2), M, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :-
    !,
    detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto1, SCC0, SCC1,
			       ND0, ND1),
    detcheck_body_dependencies(B2, M, Callchain, Backto1, Backto, SCC1, SCC,
			       ND1, ND).
detcheck_body_dependencies((B1->B2), M, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :-
    !,
    detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto1, SCC0, SCC1,
			       ND0, ND1),
    detcheck_body_dependencies(B2, M, Callchain, Backto1, Backto, SCC1, SCC,
			       ND1, ND).
detcheck_body_dependencies(\+(B1), M, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :- !,	% QP/SP
	detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto, SCC0, SCC,
				   ND0, ND).
detcheck_body_dependencies(once(B1), M, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :- !,	% SP
	detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto, SCC0, SCC,
				   ND0, ND).
detcheck_body_dependencies(do(_,B1), M, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :- !,	% SP
	detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto, SCC0, SCC,
				   ND0, ND).
detcheck_body_dependencies(call_cleanup(B1,_), M, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :-	% SP
    !,
    detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto, SCC0, SCC,
			       ND0, ND).
detcheck_body_dependencies(call_residue_vars(B1,_), M, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :-	% SP
    !,
    detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto, SCC0, SCC,
			       ND0, ND).
detcheck_body_dependencies((_^B1), M, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :-	% QP/SP
    !,
    detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto, SCC0, SCC,
			       ND0, ND).
detcheck_body_dependencies((B1;B2), M, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :-
    !,
    detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto1, SCC0, SCC1,
			       ND0, ND1),
    detcheck_body_dependencies(B2, M, Callchain, Backto1, Backto, SCC1, SCC,
			       ND1, ND).
detcheck_body_dependencies(if(B1,B2,B3), M, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :-	% SP
    !,
    detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto1, SCC0, SCC1,
			       ND0, ND1),
    detcheck_body_dependencies(B2, M, Callchain, Backto1, Backto2, SCC1, SCC2,
			       ND1, ND2),
    detcheck_body_dependencies(B3, M, Callchain, Backto2, Backto, SCC2, SCC,
			       ND2, ND).
detcheck_body_dependencies(on_exception(_,B1,B2), M, Callchain,
		Backto0, Backto, SCC0, SCC, ND0, ND) :-
    !,
    detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto1, SCC0, SCC1,
			       ND0, ND1),
    detcheck_body_dependencies(B2, M, Callchain, Backto1, Backto, SCC1, SCC,
			       ND1, ND).
detcheck_body_dependencies(catch(B1,_,B2), M, Callchain,
		Backto0, Backto, SCC0, SCC, ND0, ND) :-	% SP
    !,
    detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto1, SCC0, SCC1,
			       ND0, ND1),
    detcheck_body_dependencies(B2, M, Callchain, Backto1, Backto, SCC1, SCC,
			       ND1, ND).
% % call/N -- treat as opaque
% detcheck_body_dependencies(Call, M, Callchain, Backto0, Backto, SCC0, SCC,
% 		ND0, ND) :-	% QP/SP
%     metacall_goal(Call, B1), !,
%     detcheck_body_dependencies(B1, M, Callchain, Backto0, Backto, SCC0, SCC,
% 			       ND0, ND).
%  Don't need to handle bagof, setof, etc, because their determinism doesn't
%  depend on the determinism of the called goal, as far as we're concerned.
detcheck_body_dependencies(Goal, Mod, Callchain, Backto0, Backto, SCC0, SCC,
		ND0, ND) :-
    goal_pred(Goal, Mod, Pred),
    (	saved_clauses(File, Pred, Clauses) ->
	    bottom_up_detcheck(Clauses, Pred, File, Callchain, Backto0, Backto,
			       SCC0, SCC, ND0, ND)
    ;	Backto = Backto0,
	ND = ND0,
	SCC = SCC0
    ).

% metacall_goal(Call1, Call2) :-
% 	Call1 =.. [call,A1|As],
% 	nonvar(A1),
% 	(   A1 = M:A3 -> Call2 = M:Call3
% 	;   A1 = A3, Call2 = Call3
% 	),
% 	callable(A3), !,
% 	A3 =.. L3,
% 	append(L3, As, L4),
% 	Call3 =.. L4.
% metacall_goal(Call, _) :-
% 	functor(Call, call, A),
% 	A > 0.


/* ----------------------------------------------------------------------
    Handle file dependencies
   ---------------------------------------------------------------------- */

process_dependencies([], _, _Where) :- !.
process_dependencies([F|Fs], Mod, Where) :- !,
    process_dependencies(F, Mod, Where),
    process_dependencies(Fs, Mod, Where).
process_dependencies(Mod:File, _, Where) :- !, % [MC]
	process_dependencies(File, Mod, Where).
process_dependencies(File, Mod, Where) :-
    on_exception(Error, process_dependency(File, Mod, all, Where),
		 print_error_message(Error)).

process_dependency(File, Mod, Imports, Where-Level) :-
    absolute_file_name(File, Path, [access(exist),file_type(source),relative_to(Where)]),
    (	file_processed(Path, Filemod),
	( Filemod \== user ; Mod == user ) ->
	    true
    ;	open(Path, read, S),
	Level1 is Level+1,
	on_exception(Error, process_stream(S, Path-Level1, Mod, Filemod),
		     (	 print_error_message(Error), close(S), Filemod=user))
    ),
    process_imports(Filemod, Mod, Imports).


process_imports(From, Into, Imports) :-
    (	From == Into ->
	    true
    ;	From = user ->
	    true
    ;	module_exports(From, Exports) ->
	    (	Imports == all ->
		    Preds = Exports
	    ;	sort(Exports, Exports1),
		sort(Imports, Imports1),
		ord_intersection(Exports1, Imports1, Preds)
	    ),
	    duplicate_decls(Preds, From, Into)
    ).

duplicate_decls([], _, _).
duplicate_decls([Name/Arity|Preds], From, To) :-
    (	declared(Name, Arity, From, Prop),
	declare(Name, Arity, To, Prop),
	fail
    ;	true
    ),
    duplicate_decls(Preds, From, To).


process_stream_directives(S, Path-Level, Loadingmod, Module) :-
    prolog_flag(syntax_errors, Old, dec10),
    read_clause(S, Firstterm),
    (	ground(Firstterm),
	Firstterm = :-(module(Module,Exports)) ->
	    assert(module_exports(Module, Exports)),
	    assert(file_processed(Path, Module))
    ;	ground(Firstterm),
	Firstterm = :-(module(Module,Exports,_)) ->
	    assert(module_exports(Module, Exports)),
	    assert(file_processed(Path, Module))
    ;	Module = Loadingmod,
	assert(file_processed(Path, Module)),
	process_stream_directives_item(Firstterm, Module, Path-Level)
    ),
    repeat,
	read_clause(S, Term),
	process_stream_directives_item(Term, Module, Path-Level),
	Term == end_of_file,
    !,
    close(S),
    prolog_flag(syntax_errors, _, Old).

process_stream_directives_item(:-(Decl), Module, Where) :-
    !,
    process_directive(Decl, Module, Where).
process_stream_directives_item(?-(Decl), Module, Where) :-
    !,
    process_directive(Decl, Module, Where).
process_stream_directives_item(_, _, _Where). % ignore everything but directives


/* ----------------------------------------------------------------------
    Record a clause for later.
   ---------------------------------------------------------------------- */

save_list_for_later([], _, _, _).
save_list_for_later([Clause|Clauses], Defmod, File, Pred) :-
    save_for_later(Defmod, File, Pred, Clause),
    save_list_for_later(Clauses, Defmod, File, Pred).

save_for_later(Defmod, File, Mod:Name/Arity, Clause) :-
    assertz(saved_clause(Name, Arity, Mod, Defmod, File, Clause)).

saved_clauses(File, Mod:Name/Arity, Clauses) :-
    % backtracks over Mod:Name/Arity specs returning for each a list of
    % clauses for that pred.
    bagof(Defmod:Clause, saved_clause(Name, Arity, Mod, Defmod, File, Clause),
	  Clauses).

/* ----------------------------------------------------------------------
    Process a predicate, one clause at a time.
   ---------------------------------------------------------------------- */

process(Pred, Defmod, File-Level, Clauses) :-
    (	directive_pred(Pred) ->
	    (	Clauses = [Item], arg(1, Item, Directive) -> true
	    ;	throw(internal_error)
	    ),
	    process_directive(Directive, Defmod, File-Level)
    ;	Pred = (-) ->
	    % Special there's-no-pred-there pred
	    true
    ;   declared(Pred, discontiguous) ->
	    save_list_for_later(Clauses, Defmod, File, Pred)
    ;	check_pred_contiguity(Pred, File),
	(   foreach(Cl,Clauses),
	    param(Defmod)
	do  clause_body(Cl, Body),	% QP/SP
	    scan_body(Body, Defmod)
	),
	(   get_option(recursive, true) ->
		save_list_for_later(Clauses, Defmod, File, Pred)
	;   process_pred(Clauses, 0, Defmod, Pred, File, false, _)
	)
    ).

%  process_pred(Clauses, Clausenum, Defmod, Pred, File, Nondet0, Nondet)
%  Check Clauses for nondeterminism.  The head of Clauses is numbered
%  Clausenum+1.  These are clause for Pred.  Nontet is true if we infer that
%  Pred is nondeterministic (not just declared nondet), or if Nondet0 is
%  true.

process_pred([], _, _, _, _, Nondet, Nondet).
process_pred([Clause|Rest], I, Defmod, Pred, File, Nondet0, Nondet) :-
    J is I+1,
    (	get_option(recursive, false),
	get_option(print_all_decls, false),
	(declared(Pred, nondet); declared(Pred, det)) -> % [MC]
	    Nondet = Nondet0
    ;	inter_clause_check(Clause, J, Rest, Pred, File, Nondet0, Nondet1),
	mark_head_args_seen(Clause),
	clause_body(Clause, Defmod, Mod, Body),
	intra_clause_check(Body, Mod, J, Pred, File, Nondet1, Nondet2)
    ),
    process_pred(Rest, J, Defmod, Pred, File, Nondet2, Nondet).


/* ----------------------------------------------------------------------
    Clause is deemed non-determinate if it contains a disjunction or a 
    repeat, or calls a dynamic predicate, subject to some exceptions.
   ---------------------------------------------------------------------- */

mark_head_args_seen(Clause) :-
	clause_head(Clause, Head),
	mark_term_seen(Head).

mark_term_seen('$seen') :- !.
mark_term_seen(Term) :-
	functor(Term, _, Arity),
	mark_args_seen(Arity, Term).

mark_args_seen(N, Term) :-
	(   N =:= 0 ->
		true
	;   arg(N, Term, Subterm),
	    mark_term_seen(Subterm),
	    N1 is N - 1,
	    mark_args_seen(N1, Term)
	).

intra_clause_check(Body,_Mod, I, Pred, File, _, Nondet) :-
    has_disjunction(Body),
    !,
    Nondet = true,
    warn(Pred, I, File, contains_disjunction).
intra_clause_check(Body, Mod, I, Pred, File, _, Nondet) :-
    calls_known_nondet(Body, Mod, Culprit),
    \+ Culprit = Pred,
    !,
    Nondet = true,
    warn(Pred, I, File, calls(Culprit)).
intra_clause_check(_Body,_Mod,_I,_Pred,_File,Nondet, Nondet).


/* ----------------------------------------------------------------------
    Clause is deemed non-determinate unless 
	(1) it contains a cut, or
	(2) its first argument is distinct in its principal functor from all
	    following clauses, or
	(3) it ends with a 'fail'.
   ---------------------------------------------------------------------- */

inter_clause_check(Clause, I, Clauses, Pred, File, Nondet0, Nondet) :-
    (	clause_body(Clause, Body),
	cuts_or_fails(Body) ->
	    Nondet = Nondet0
    ;	declared(Pred, multifile) ->
	    % multifile preds may not be det even
	    warn(Pred, I, File, cutless_multifile),
	    Nondet = true
    ;	N0 is I + 1,
	same_index(Clause, Clauses, N0, N) ->
	    % not indexable
	    Nondet = true,
	    warn(Pred, I, File, multi_clause(N))
    ;	Nondet = Nondet0
    ).


/* ----------------------------------------------------------------------
    cuts_or_fails(Body) succeeds if Body cannot succeed without removing a
    choicepoint.  That is, Body must either fail, raise an exception, or
    execute a cut.
   ---------------------------------------------------------------------- */

cuts_or_fails(X) :- var(X), !, fail.  % catch variables
cuts_or_fails(!).
cuts_or_fails(fail).
cuts_or_fails(false).
cuts_or_fails(raise_exception(_)).
cuts_or_fails(throw(_)).	% SP
cuts_or_fails(( _ : P  )) :- cuts_or_fails(P).
cuts_or_fails(( _ ^ P  )) :- cuts_or_fails(P). % QP/SP
cuts_or_fails(( P  , _Q)) :- cuts_or_fails(P), !.
cuts_or_fails((_P  ,  Q)) :- cuts_or_fails(Q).
cuts_or_fails((_P -> Q  ;  R)) :- !, cuts_or_fails(Q), cuts_or_fails(R).
cuts_or_fails(( P  ;  Q)) :- cuts_or_fails(P), cuts_or_fails(Q).
cuts_or_fails(( P -> _Q)) :- cuts_or_fails(P), !. % SP
cuts_or_fails((_P ->  Q)) :- cuts_or_fails(Q).
cuts_or_fails(if(P,Q,R))  :- cuts_or_fails((P -> Q ; R)). % SP
% cuts_or_fails(\+(P))  :- cuts_or_fails((P -> fail ; true)). % SP
cuts_or_fails(once(P)) :- cuts_or_fails(P). % SP
cuts_or_fails(do(_,P)) :- cuts_or_fails(P). % SP
cuts_or_fails(call_cleanup(P,_)) :- cuts_or_fails(P). % SP
cuts_or_fails(call_residue_vars(P,_)) :- cuts_or_fails(P). % SP
cuts_or_fails(on_exception(_,P,Q)) :- cuts_or_fails(P), cuts_or_fails(Q).
cuts_or_fails(catch(P,E,Q)) :- cuts_or_fails(on_exception(E,P,Q)).
% call/N -- treat as opaque
% cuts_or_fails(Call) :-
% 	metacall_goal(Call, B1),
% 	cuts_or_fails(B1).


/* ----------------------------------------------------------------------
    has_disjunction(Body) succeeds if Body contains a disjunction.  There 
    are four exceptions: (1) the disjunction is followed by a cut, (2) the 
    disjunction is an if-then-else, (3) the left-hand side of the 
    disjunction contains a cut, or (4) the left-hand side of the disjunction
    ends with 'fail'.
   ---------------------------------------------------------------------- */

has_disjunction(X) :- var(X), !, fail.  % catch variables
has_disjunction(( _ : P  )) :- has_disjunction(P).
has_disjunction(( _ ^ P  )) :- has_disjunction(P). % QP/SP
has_disjunction(( P  ,  Q)) :- has_disjunction(P), \+ cuts_or_fails(Q), !.
has_disjunction((_P  ,  Q)) :- has_disjunction(Q).
has_disjunction((_P ->  Q ; _R)) :- has_disjunction(Q), !.
has_disjunction((_P -> _Q ;  R)) :- !, has_disjunction(R).
has_disjunction(( P  ; _Q)) :- \+ cuts_or_fails(P), !.
has_disjunction(( P  ; _Q)) :- has_disjunction(P), !.
has_disjunction((_P  ;  Q)) :- has_disjunction(Q).
has_disjunction((_P ->  Q)) :- has_disjunction(Q).
has_disjunction(if(P,Q,R))  :- has_disjunction((P -> Q ; R)). % SP
has_disjunction(do(_,P))    :- has_disjunction(P). % SP
% has_disjunction(\+(P))  :- has_disjunction((P -> true ; fail)). % SP
% has_disjunction(once(P)) :- has_disjunction((P -> true)).
has_disjunction(call_cleanup(P,_)) :- has_disjunction(P). % SP
has_disjunction(call_residue_vars(P,_)) :- has_disjunction(P). % SP
has_disjunction(on_exception(_, P,_Q)) :- has_disjunction(P), !.
has_disjunction(on_exception(_,_P, Q)) :- !, has_disjunction(Q).
has_disjunction(catch(P,E,Q)) :- has_disjunction(on_exception(E,P,Q)).
% call/N -- treat as opaque
% has_disjunction(Call) :-
% 	metacall_goal(Call, P),
% 	has_disjunction(P).


/* ----------------------------------------------------------------------
    [PM] 4.2.1 bogus docs:
    Succeeds if the key for Clause can not be unified with that of any
    of the Clauses.  Fails if Clause has no key and Clauses is not empty.
   ---------------------------------------------------------------------- */

same_index(Clause, Clauses, N0, N) :-
    (	key(Clause, Key) ->
	    member_key(Key, Clauses, N0, N)
    ;	Clauses = [_|_] ->
	    % 0 arity predicates are not indexable
	    N = N0
    ;	fail
    ).


%% [MC] This must not bind the key of Clause
member_key(Key, [Clause|Clauses], N0, N) :-
    (	\+key(Clause, Key) ->
	    N1 is N0 + 1,
	    member_key(Key, Clauses, N1, N)
    ;   N = N0
    ).


/* ----------------------------------------------------------------------
    Returns the key of a given clause, that is, the principal functor of its
    first argument.  If the first argument is a variable, Key is a variable.
   ---------------------------------------------------------------------- */

key(Clause, Key) :-
    clause_head(Clause, Head),
    functor(Head, PName, Arity),
    Arity >= 1,
    arg(1, Head, Arg1),
    (   var(Arg1),
	functor(General, PName, Arity),
	subsumes_term(Head, General), % i.e. all args are unique variables
        clause_body(Clause, Body),
	body_unifies(Body, Arg1, Val1)
    ->  Key0 = Val1
    ;   Key0 = Arg1
    ),
    (	var(Key0) -> Key = Key0
    %% [PM] 4.4.0 All numbers now uses exact indexing.
    %% ;   number(Key0),		% SP: coarse indexing of bignums and floats
    %%     prolog:'$large_data'(0, Key0, F)
    %% ->  Key = '$large'(F)
    ;	functor(Key0, Name, KeyArity),
	functor(Key, Name, KeyArity)
    ).

body_unifies(X=Val, Arg, Val) :- X==Arg.
body_unifies((Goal,_), Arg, Val) :-
	body_unifies(Goal, Arg, Val).


/* ----------------------------------------------------------------------
   Scan clause bodies noting predicates we *haven't* seen a dynamic decl for,
   and looking for indications that preds are dynamic, such as asserts.
   ---------------------------------------------------------------------- */

scan_body(Var, _) :-
    var(Var),
    !.
scan_body((B1,B2), Mod) :-
    !,
    scan_body(B1, Mod),
    scan_body(B2, Mod).
scan_body((B1;B2), Mod) :-
    !,
    scan_body(B1, Mod),
    scan_body(B2, Mod).
scan_body((B1->B2), Mod) :-	% QP/SP
    !,
    scan_body(B1, Mod),
    scan_body(B2, Mod).
scan_body(\+(B), Mod) :-
    !,
    scan_body(B, Mod).
scan_body(Mod:B, _) :-
    !,
    scan_body(B, Mod).
scan_body(_^B, Mod) :-		% QP/SP
    !,
    scan_body(B, Mod).
scan_body(once(B), Mod) :-	% SP
    !,
    scan_body(B, Mod).
scan_body(do(_,B), Mod) :-	% SP
    !,
    scan_body(B, Mod).
scan_body(call_cleanup(B,_), Mod) :- % SP
    !,
    scan_body(B, Mod).
scan_body(call_residue_vars(B,_), Mod) :- % SP
    !,
    scan_body(B, Mod).
scan_body(on_exception(_, P, Q), Mod) :-
    !,
    scan_body(P, Mod),
    scan_body(Q, Mod).
scan_body(catch(P, _, Q), Mod) :- % SP
    !,
    scan_body(P, Mod),
    scan_body(Q, Mod).
scan_body(bagof(_,B,_), Mod) :-
    !,
    scan_body(B, Mod).
scan_body(setof(_,B,_), Mod) :-
    !,
    scan_body(B, Mod).
scan_body(findall(_,B,_), Mod) :-
    !,
    scan_body(B, Mod).
scan_body(findall(_,B,_,_), Mod) :- % SP
    !,
    scan_body(B, Mod).
scan_body(abolish(Spec), Mod) :-
    !,
    must_be_dynamic_spec(Spec, Mod).
% scan_body(abolish(Name,Arity), Mod) :-
%     !,
%     must_be_dynamic_spec(Name/Arity, Mod).
scan_body(assert(Clause), Mod) :-
    !,
    must_be_dynamic_clause(Clause, Mod).
scan_body(assert(Clause,_), Mod) :-
    !,
    must_be_dynamic_clause(Clause, Mod).
scan_body(asserta(Clause), Mod) :-
    !,
    must_be_dynamic_clause(Clause, Mod).
scan_body(asserta(Clause,_), Mod) :-
    !,
    must_be_dynamic_clause(Clause, Mod).
scan_body(assertz(Clause), Mod) :-
    !,
    must_be_dynamic_clause(Clause, Mod).
scan_body(assertz(Clause,_), Mod) :-
    !,
    must_be_dynamic_clause(Clause, Mod).
scan_body(clause(Head,Body), Mod) :-
    !,
    must_be_dynamic_head(Head, Mod),
    scan_body(Body, Mod).
scan_body(clause(Head,Body,_), Mod) :-
    !,
    must_be_dynamic_head(Head, Mod),
    scan_body(Body, Mod).
scan_body(retract(Clause), Mod) :-
    !,
    must_be_dynamic_clause(Clause, Mod).
scan_body(retractall(Head), Mod) :-
    !,
    must_be_dynamic_head(Head, Mod).
% call/N -- treat as opaque
% scan_body(Call, Mod) :-	% SP
%     metacall_goal(Call, B),
%     !,
%     scan_body(B, Mod).
scan_body(Goal, Mod) :-
    (	predicate_property(Mod:Goal, built_in) ->
	    true
    ;	functor(Goal, Name, Arity),
	note_called(Mod, Name, Arity)
    ).

must_be_dynamic_spec(Var, _) :-
    var(Var),
    !.
must_be_dynamic_spec(Name0/Arity, Mod0) :-
    !,
    name_and_module(Name0, Mod0, Name, Mod),
    must_be_dynamic(Mod, Name, Arity).
must_be_dynamic_spec(Mod:Spec, _) :-
    must_be_dynamic_spec(Spec, Mod).


name_and_module(Name, Mod, Name, Mod) :-
    var(Name),
    !.
name_and_module(Mod0:Name0, _, Name, Mod) :-
    !,
    name_and_module(Name0, Mod0, Name, Mod).
name_and_module(Name, Mod, Name, Mod).


must_be_dynamic_clause(Var, _) :-
    var(Var),
    !.
must_be_dynamic_clause((Head:-Body), Mod) :-
    !,
    must_be_dynamic_head(Head, Mod),
    scan_body(Body, Mod).
must_be_dynamic_clause(Mod:Clause, _) :-
    !,
    must_be_dynamic_clause(Clause, Mod).
must_be_dynamic_clause(Head, Mod) :-
    must_be_dynamic_head(Head, Mod).


must_be_dynamic_head(Var, _) :-
    var(Var),
    !.
must_be_dynamic_head(Mod:Head, _) :-
    !,
    must_be_dynamic_head(Head, Mod).
must_be_dynamic_head(Head, Mod) :-
    functor(Head, Name, Arity),
    must_be_dynamic(Mod, Name, Arity).


must_be_dynamic(Mod, Name, Arity) :-
    (	atom(Mod),
	atom(Name),
	integer(Arity) ->
	    (	declared(Name, Arity, Mod, dynamic) ->
		    true
	    ;	report_missed_some(Mod:Name/Arity, dynamic)
	    )
    ;	true
    ).


note_called(Mod, Name, Arity) :-
    (	called(Name, Arity, Mod) ->
	    true
    ;	assert(called(Name, Arity, Mod))
    ).


/* ----------------------------------------------------------------------
    Obtaining the Head, Body and Predicate of a Clause
   ---------------------------------------------------------------------- */

clause_head(Clause, Head) :-
	clause_head(Clause, _, _, Head).

clause_head(Var, _, _, _) :-
    var(Var),
    !,
    fail.
clause_head(M:Clause, _, Mod, H) :-
    !,
    clause_head(Clause, M, Mod, H).
clause_head((H0 :- _B), Mod, Mod, H) :-
    !,
    H = H0.
clause_head(Fact, Mod, Mod, Fact).


clause_body(Clause, Body) :-
	clause_body(Clause, dontcare, _, Body).

clause_body(Var, _, _, _) :-
    var(Var),
    !,
    fail.
clause_body(M:Clause, _, Mod, B) :-
    !,
    clause_body(Clause, M, Mod, B).
clause_body((_H :- B0), Mod, Mod, B) :-
    !,
    B = B0.
clause_body(_, Mod, Mod, true).


pred_for_clause(Clause, Mod0, Pred) :-
    clause_head(Clause, Mod0, Mod, Head),
    head_pred(Head, Mod, Pred).

head_pred(Mod:Head, _, Pred) :-
    !,
    head_pred(Head, Mod, Pred).
head_pred(Head, Mod, Mod:Name/Arity) :-
    functor(Head, Name, Arity).


/* ----------------------------------------------------------------------
   calls_known_nondet(+Goal, +Module, -Pred) succeeds if Goal invokes
   nondet predicate Pred in a context which might make the body
   nondeterministic.  Binds Pred to only the first nondet predicate it calls.
   ---------------------------------------------------------------------- */

calls_known_nondet(X, _, _) :- var(X), !, fail.  % catch variables goals.
calls_known_nondet(( Mod : P  ), _, Pred) :- !,
    calls_known_nondet(P, Mod, Pred).
calls_known_nondet(( P ,  Q), Mod, Pred) :-
    calls_known_nondet(P, Mod, Pred),
    \+ cuts_or_fails(Q), !.
calls_known_nondet(( P ,  Q), Mod, Pred) :-
	!,
	mark_term_seen(P),
	calls_known_nondet(Q, Mod, Pred).
calls_known_nondet(( P ->  Q ; _R), Mod, Pred) :-
	mark_term_seen(P),
	calls_known_nondet(Q, Mod, Pred),
	!.
calls_known_nondet((_P -> _Q ;  R), Mod, Pred) :-
    !,
    calls_known_nondet(R, Mod, Pred).
calls_known_nondet(if(P,Q,R), Mod, Pred) :- % SP
	!,
	calls_known_nondet((P , Q ; R), Mod, Pred).
calls_known_nondet(do(_,P), Mod, Pred) :- % SP
	!,
	calls_known_nondet(P, Mod, Pred).
calls_known_nondet(( P  ; _Q), Mod, Pred) :-
    calls_known_nondet(P, Mod, Pred),
    !.
calls_known_nondet((_P  ;  Q), Mod, Pred) :-
    !,
    calls_known_nondet(Q, Mod, Pred).
calls_known_nondet(( P ->  Q), Mod, Pred) :-
	!,
	mark_term_seen(P),
	calls_known_nondet(Q, Mod, Pred).
% calls_known_nondet(\+(P), Mod, Pred) :-	% SP
% 	calls_known_nondet((P -> fail ; true), Mod, Pred).
% calls_known_nondet(once(Goal), Mod, Pred) :-  % SP
% 	calls_known_nondet((Goal -> true), Mod, Pred).
calls_known_nondet(call_cleanup(Goal,_), Mod, Pred) :- % SP
    !,
    calls_known_nondet(Goal, Mod, Pred).
calls_known_nondet(call_residue_vars(Goal,_), Mod, Pred) :- % SP
    !,
    calls_known_nondet(Goal, Mod, Pred).
calls_known_nondet((_^Goal), Mod, Pred) :- % SP
    !,
    calls_known_nondet(Goal, Mod, Pred).
calls_known_nondet(on_exception(_, P,_Q), Mod, Pred) :-
    calls_known_nondet(P, Mod, Pred),
    !.
calls_known_nondet(on_exception(_,_P, Q), Mod, Pred) :-
    !,
    calls_known_nondet(Q, Mod, Pred).
calls_known_nondet(catch(P,E,Q), Mod, Pred) :- % SP
	!, calls_known_nondet(on_exception(E,P,Q), Mod, Pred).
% Clauses for builtins, in alphabetical order
calls_known_nondet(absolute_file_name(_,_,Opts), _, absolute_file_name/3) :- !, % SP argument order
    ground(Opts),
    memberchk(solutions(all), Opts).
calls_known_nondet(atom_concat(A1,A2,_), _, atom_concat/3) :- !, % SP
	var(A1), var(A2).
calls_known_nondet(bagof(Template,Gen,_), _, bagof/3) :-
    !,
    mark_term_seen(Template),
    \+ (skolemize(Gen), ground(Gen)).
calls_known_nondet(clause(_,_), _, clause/2) :- !.
calls_known_nondet(clause(_,_,_), _, clause/3) :- !.
calls_known_nondet(current_key(_,Kterm), _, current_key/2) :-
    !,
    \+ ground(Kterm).
calls_known_nondet(current_module(Mod, File), _, current_module/2) :-
    !,
    \+ ground(Mod-File).
calls_known_nondet(current_op(_Prec, Typ,_Name), _, current_op/3) :-
    \+ ground(Typ),
    !.
calls_known_nondet(current_op(_Prec,_Typ, Name), _, current_op/3) :-
    !,
    \+ ground(Name).
calls_known_nondet(current_predicate(_,Term), _, current_predicate/2) :-
    !,
    \+ ground(Term).
calls_known_nondet(length(L,N), _, length/2) :- !, % SP/QP
	var(L), var(N).
calls_known_nondet(predicate_property( Pred,_Prop), _, predicate_property/2) :-
    \+ ground(Pred),
    !.
calls_known_nondet(predicate_property(_Pred, Prop), _, predicate_property/2) :-
    !,
    \+ ground(Prop).
calls_known_nondet(stream_property(Stream, Prop), _, stream_property/2) :- % SP
    !,
    \+ ground(Stream-Prop).
calls_known_nondet(phrase(PhraseType0,List), Mod0, Pred) :-
    !,
    callable(PhraseType0),
    module_and_goal(PhraseType0, Mod0, Mod, PhraseType),
    add_args(PhraseType, [List,_], Goal),
    calls_known_nondet(Goal, Mod, Pred).
calls_known_nondet(phrase(PhraseType0,List,Rest), Mod0, Pred) :-
    !,
    callable(PhraseType0),
    module_and_goal(PhraseType0, Mod0, Mod, PhraseType),
    add_args(PhraseType, [List,Rest], Goal),
    calls_known_nondet(Goal, Mod, Pred).
calls_known_nondet(recorded(_,_,Ref), _, recorded/3) :- !, \+ ground(Ref).
calls_known_nondet(repeat, _, repeat/0) :- !.
calls_known_nondet(retract(_), _, retract/1) :- !.
calls_known_nondet(setof(Template,Gen,_), _, setof/3) :-
    !,
    mark_term_seen(Template),
    \+ (skolemize(Gen), ground(Gen)).
calls_known_nondet(source_file(File), _, source_file/1) :-
    !,
    \+ ground(File).
calls_known_nondet(source_file(_,File), _, source_file/2) :-
    !,
    \+ ground(File).
% calls_known_nondet(source_file(_,_,File), _, source_file/3) :- % QP
%     !,
%     \+ ground(File).
calls_known_nondet(sub_atom(_Atom,Before,_Len,After,Sub), _, sub_atom/5) :- !, % SP
	(   var(Before), var(After) -> true
	;   var(Before), var(Sub) -> true
	;   var(After), var(Sub)
	).
% call/N -- treat as opaque
% calls_known_nondet(Call, Mod, Pred) :-
%     metacall_goal(Call, Goal),
%     !,
%     calls_known_nondet(Goal, Mod, Pred).
% Clause for hooks
calls_known_nondet(Goal, Mod, N/A) :-
	hook(Goal, Mod), !,
	functor(Goal, N, A).
% Clause for non-builtins
calls_known_nondet(Goal, Mod, Pred) :-
    goal_pred(Goal, Mod, Pred),
    (	declared(Pred, det) -> fail % [MC]
    ;	get_option(recursive, true) ->
	    inferred_nondet(Pred, _, _, _)
    ;	declared(Pred, dynamic) -> true	% [MC]
    ;	declared(Pred, multifile) -> true
    ;	declared(Pred, nondet)
    ).

hook(term_expansion(_,_,_,_,_,_), user).
hook(file_search_path(_,_), user).
hook(library_directory(_), user).
hook(unknown_predicate_handler(_,_,_), user).
hook(portray(_), user).
hook(portray_message(_,_), user).
hook(message_hook(_,_,_), user).
hook(generate_message_hook(_,_,_), user).
hook(query_hook(_,_,_,_,_,_), user).
hook(query_class_hook(_,_,_,_,_), user).
hook(query_input_hook(_,_,_), user).
hook(query_map_hook(_,_,_,_), user).
hook(breakpoint_expansion(_,_), user).
hook(debugger_command_hook(_,_), user).
hook(error_exception(_), user).
hook(generate_message(_,_,_), 'SU_messages').
hook(query_abbreviation(_,_,_), 'SU_messages').
hook(query_class(_,_,_,_,_), 'SU_messages').
hook(query_input(_,_,_), 'SU_messages').
hook(query_map(_,_,_,_), 'SU_messages').
hook(goal_expansion(_,_,_), _M).
hook(attribute_goal(_,_), _M).
hook(project_attributes(_,_), _M).
hook(verify_attributes(_,_,_), _M).

add_args(Goal0, Extras, Goal) :-
	Goal0 =.. Front,
	append(Front, Extras, Full),
	Goal =.. Full.

module_and_goal(Mod0:Goal0, _, Mod, Goal) :-
	!,
	module_and_goal(Goal0, Mod0, Mod, Goal).
module_and_goal(Goal, Mod, Mod, Goal).


goal_pred(Mod:Goal, _, Pred) :-
    !,
    goal_pred(Goal, Mod, Pred).
goal_pred(Goal, Mod, Mod:Name/Arity) :-
    functor(Goal, Name, Arity).




%  skolemize(+Quantified)
%  Ground any quantified variables in Quantified.  Actually, for this
%  application all we want is for the quantified variables to be ground;
%  we don't care if the variables are bound to distinct terms.

skolemize(Q^Goal) :-
	!,
	numbervars(Q, 0, _),
	skolemize(Goal).
skolemize(_).


/* ----------------------------------------------------------------------
    Print a warning message.
   ---------------------------------------------------------------------- */

warn(Pred, I, File, Problem) :-
    note_inferred_nondet(Pred, I, File, Problem),
    (	get_option(recursive, false),
	\+ declared(Pred, nondet),
	\+ declared(Pred, det) -> % [MC]
	    print_message(warning, nondet(Pred,I,Problem))
    ;	true
    ).


/* ----------------------------------------------------------------------
    Print nondet declarations
   ---------------------------------------------------------------------- */

print_decls(All) :-
    (	setof(Pred,
	      Clause^Reason^nondet_in_file(All, Pred, Clause, Reason, File),
	      Ndets),
	prolog_flag(informational, Old, on),
	print_message(informational, nondet_header(File,All)),
	prolog_flag(informational, _, Old),
	member(Pred, Ndets),
        % [PM] 4.3 Note that this should go to user_output (whereas
        % the messages goes to user_error).
        format(':- ~q is nondet.~n', [Pred]),
	% format(':- nondet ~q.~n', [Pred]),
	fail
    ;	flush_output            % [PM] 4.0
    ).


/* ----------------------------------------------------------------------
    Print error messages
   ---------------------------------------------------------------------- */

%   print minimal syntax error message

% [PM] 4.3 This is all wrong, the first argument will be an error/2 term. Luckily the last clause will probably suffice anyway.
print_error_message(syntax_error(_,Line,Msg,Tokens,Fl)) :-
    !,
    print_message(error, syntax_error(0,Line,Msg,Tokens,Fl)).
print_error_message(existence_error(_,_,Obj,Culprit,Msg)) :-
    !,
    print_message(error, existence_error(0,0,Obj,Culprit,Msg)).
print_error_message(permission_error(_,Op,Obj,Culprit,Msg)) :-
    !,
    print_message(error, permission_error(0,Op,Obj,Culprit,Msg)).
print_error_message(E) :-
    print_message(error, E).




:- multifile user:generate_message_hook/3.

user:generate_message_hook(late_or_missing_declaration(Pred, Property)) --> !,
    ['warning: predicate ~q is ~w.'-[Pred,Property], nl,
     '         Some nondeterminism may have been missed.'-[], nl,
     '         Add (or move) the directive'-[], nl
    ],

%    ['             :- ~w ~q.'-[Property,Pred], nl],
     generate_prop_line(Pred, Property, '             '),
    ['         near the top of this file.'-[], nl
    ].
user:generate_message_hook(nondet(Pred,I,Reason)) --> !,
    ['Non-determinate: ~q (clause ~d)'-[Pred,I],nl],
    nondet_reason(Reason).
user:generate_message_hook(in_file(File)) --> !,
    ['In file ~w:'-[File],nl].
user:generate_message_hook(nondet_header(File,All)) --> !,
    (   {All==true} -> ['Put these in place of the nondet declarations'-[],nl]
    ;                  ['Insert these nondet declarations'-[],nl]
    ),
    ['in file ~w'-[File],nl].

generate_prop_line(Pred, Property, Prefix) --> {is2_property(Property), !},
    % [PM] 4.3 The plain writeq output is too ugly.
    % We need a pretty printer, and a way to tell write_term to write operators as operator argument without parenthesis.
    % ['~q.'-[(:- (Pred is Prop))], nl],
    ['~w:- ~q is ~q.'-[Prefix, Pred, Property], nl].
generate_prop_line(Pred, Property, Prefix) --> !,
    ['~w:- ~q ~q.'-[Prefix, Property, Pred], nl].

is2_property(det).
is2_property(nondet).


nondet_reason(contains_disjunction) -->
    ['    This clause contains a disjunction not forced to be deterministic.'
	 -[],
     nl].
nondet_reason(calls(Pred)) -->
    ['    Calls nondet predicate ~q.'-[Pred], nl].
nondet_reason(multi_clause(N)) -->
    ['    Indexing cannot distinguish this from clause ~d.'-[N], nl].
nondet_reason(cutless_multifile) -->
    ['    This predicate is multifile, and this clause has no cut.'-[], nl].

/* ----------------------------------------------------------------------
    The following allows det to be built as a saved-state.  Since no
    foreign code is used, starting up the saved-state is reasonably fast,
    so there is little point in building a stand-alone program which will
    take up more disk space.

	% spld [-D] -o spdet det.po SU_messages.po
 	% spdet file1 file2 ...

   ---------------------------------------------------------------------- */

:- initialization initialize.
