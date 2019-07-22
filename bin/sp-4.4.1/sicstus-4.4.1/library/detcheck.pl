/* ----------------------------------------------------------------------
    File:	detcheck.pl
    Authors:	Dave Bowen and Peter Schachte
    SCCS:	@(#)det.pl	66.1 10/14/92
    Purpose:	Determinacy checker

    Copyright (C) 1989 Quintus Computer Systems, Inc.  Permission is hereby
    granted for this program to be used or modified for any purpose whatsoever
    provided only that this copyright notice is retained in the source.

    Most functionality provided by this code has been superseded by
    the analysis performed by the SICStus Prolog IDE, SPIDER.

    This program is intended to be used to look for unintended non-determinacy
    in programs which are intended to be mostly determinate.  Unintended
    non-determinacy should be eradicated because

	(1) it may give you wrong answers on backtracking
	(2) it may cause a lot of memory to be wasted

    The simple way to avoid non-determinacy is by appropriate use of cuts.
    However, it is not good practice to sprinkle cuts all over your program
    "just in case".  Cuts should be used sparingly because they damage the
    declarative reading of your code:  if a procedure contains a cut you cannot
    read each clause in it as an independent logical implication - you have to
    take into account the preceding clauses with cuts.

    The purpose of this program is to help you spot the places where cuts are
    really necessary.  It won't find them all, and it will point to some
    clauses where you don't want a cut because you really intended
    non-determinacy.  However, it is hoped that it will help.

    For advice on the correct placement of cuts, see the chapter "Writing
    Efficient Programs" in the Quintus Prolog Reference Manual.
    An important point to understand is how first-argument indexing can
    often be used to avoid the need for cuts.

    The way to use this program is to put the line

	:- load_files(library(detcheck), [when(compile_time), if(changed)]).

    near the top of your source file.  After that, every time you compile this
    file, either from inside Prolog or using the qpc compiler, your program
    will be checked for unintended nondeterminism.  Messages of the following
    form will be printed to standard output:

	* Non-determinate: foo/3 (clause 2)
	*    This clause contains a disjunction not forced to be deterministic.

    This message indicates that if clause 2 of foo/3 succeeds it will leave
    behind a choicepoint, because the clause contains a disjunction.  You
    should look at it carefully and decide whether you really want the system
    backtrack into that clause and look for another solution.  If not, put in
    a cut as early as possible in the clause.  That is, put the cut
    immediately after the goal which determines that the subsequent clauses
    should not be used on backtracking.

    Clauses are reported as being non-determinate if

	(1) the clause does not contain a cut, and
	(2) the clause does not have a first argument with a different 
	    principal functor than the first argument of any subsequent clause
	    for the same predicate, and
	(3) the clause does not end with 'fail' or 'raise_exception'

    Clauses are also reported as being non-determinate if

	(1) the clause contains a disjunction for which some disjunct other
	    than the last does not contain a cut and does not end in fail or
	    raise_exception; or
	(2) the clause invokes a goal known to be nondeterministic, unless
	    that goal is followed by a cut, fail, or raise_exception, or
	    appears in the condition of an if->then;else.  Goals known to be
	    nondetermistic include calls to predicates declared to be
	    nondeterministic or dynamic, calls to certain builtins, and
	    disjunctions that don't force each disjunct but the last to be
	    deterministic.

    Since not all the clauses of a multifile predicate may be available for
    analysis, det must not assume that a clause is determinate just because
    the principle functor of its first argument differs from those of
    subsequent clauses.

    You may declare a predicate to be [non]deterministic with a
    declaration of the form

        :- predspec is nondet.
        :- predspec is det.

    or the legacy forms:

	:- nondet predspec.
	:- det predspec.

    where predspec is name/arity or module:name/arity.  Such predicates are
    not warned about by the determinacy checker.

    Disclaimer:  This program does not catch all possible sources of
    non-determinacy.  In particular, note that it assumes determinacy when
    any two clauses have first arguments with different principal functors.
    Of course, this only implies determinacy if the predicate is always called
    with its first argument instantiated.  In general, this code assumes if a
    goal would be deterministic with certain arguments ground, then those
    arguments will be ground when the goal is invoked.

   ---------------------------------------------------------------------- */

:- module(detcheck, [nondet/1,det/1]).

:- op(1150, fx, nondet).

nondet _.

:- op(1150, fx, det).

det _.


:- use_module(library(determinacy)).

:- dynamic current_pred/2.
:- volatile current_pred/2.
:- dynamic saved_clause/2.
:- volatile saved_clause/2.


%  Expand a clause, reporting any undeclared nondeterminism detected.
%  There are a number of gotchas to avoid.  Most obviously, we must check
%  an entire predicate at once but we receive it a clause at a time, so
%  we must buffer clauses as they are received.  Also, we must handle
%  declarations and file loading directives.  We can count on the compiler
%  to handle op declarations for us.  Another problem with directives in
%  general is that they must be handled in order.  So when we are asked to
%  expand an item, if it is for the same predicate as previous items, we
%  must save it and finish.  If not, we must collect the clauses we have
%  saved, and process that predicate.  After that, if the current item is a
%  directive, we should handle it, otherwise we should save it away.
%  end_of_file should be treated the same way, except that we don't save it
%  away, and at that point we must handle any saved discontiguous predicates.
%  Yet another tricky issue is that we see clauses in the order in which the
%  compiler sees them.  When a file loads another file, we see the clauses for 
%  the loaded file before finishing the loading file.  To simplify our
%  handling of this, we consider predicates which contain file loading
%  directives among their clauses to be discontiguous, requiring a
%  declaration.

handle([], _, _) :-
    !.
handle([Clause|Clauses], Module, File) :-
    !,
    handle(Clause, Module, File),
    handle(Clauses, Module, File).
handle(beginning_of_file, _, _) :- !, fail.
handle(end_of_file, Module, File) :-
    !,
    detcheck_curr_pred(File, Module),
    detcheck_saved(File, Module),
    retractall(current_pred(File, _)),
    initialize_file(File).
handle(Item, Module, File) :-
    (	current_pred(File, Curpred) ->
	    true
    ;	initialize_file(File),
	asserta(current_pred(File, -)),
	retractall(saved_clause(File, _)),
	Curpred = (-)
    ),
    pred_for_clause(Item, Module, Pred),
    (	Curpred == Pred ->
	    assert(saved_clause(File, Item))
    ;	detcheck_curr_pred(File, Module),
	(   directive_pred(Pred) ->
		asserta(current_pred(File, -)),
		process(Pred, Module, File-0, [Item])
	;
	     asserta(current_pred(File, Pred)),
	     assert(saved_clause(File, Item))
	)
    ).

detcheck_curr_pred(File, Module) :-
    retract(current_pred(File, Pred)),
    !,		% protect from nondeterminacy of retract
    collect_saved(File, Clauses),
    (	process(Pred, Module, File-0, Clauses) ->
	    true
    ;	throw(internal_error)
    ).

collect_saved(File, Clauses) :-
    (	retract(saved_clause(File, Item)) ->
	    Clauses = [Item|Clauses1],
	    collect_saved(File, Clauses1)
    ;	Clauses = []
    ).


detcheck_saved(File, Module) :-
    (	saved_clauses(File, Pred, Clauses),
	undeclare(Pred, discontiguous),
	process(Pred, Module, File-0, Clauses),
	fail
    ;	true
    ).

:- if(false). % [PM] 4.1.3 not used.
% Return the line number of the first line of the term layout
condense_layout([FL0|_], FL) :- !, FL=FL0.
condense_layout(FL, FL).
:- endif.

:- multifile user:term_expansion/6.

user:term_expansion(Term0, Lay0, Ids, Term0, Lay0, [detcheck|Ids]) :-
    nonmember(detcheck, Ids),
    prolog_load_context(module, Module),
    prolog_load_context(file, File),
    Module \== detcheck,			% don't expand myself!
    !,
    prolog:term_expansion_loop_internal(Term0, Lay0, [detcheck|Ids], Terms, _),
    handle(Terms, Module, File).
