% File:	    counters.pl
% Module:   Counters
% Author:   Lan
% Purpose:  Provide named counter facility


:- module(counters,[
    initialize_counters/0,
    declare_counter/1,
    inc_counter/1,
    inc_counter/2
    ]).


:- use_module(library(ctr),[
    ctr_set/2,
    ctr_inc/1,
    ctr_inc/2
    ]).


:- dynamic counter_table/2.



/* initialize_counters
   declare_counter(+Name)
   find_free_counter(-Counter)
   find_free_counter(+N, -Counter)

initialize_counters/0 clears all previous counter declarations, if any.
declare_counter/1 and free_counter/1 reserve and free a counter associated
with Name. declare_counter/1 fails if more than 32 counters are
simultaneously declared or for a duplicate declaration.
find_free_counter/1,2 search for a counter number (0..31) that has not
already been declared.  */

initialize_counters :-
    retractall(counter_table(_,_)).

declare_counter(Name) :-
    \+counter_table(Name,_),
    find_free_counter(Counter),
    !,
    assert(counter_table(Name,Counter)),
    reset_counter(Name).

find_free_counter(Counter) :-
    find_free_counter(0,Counter).

find_free_counter(N,N) :-
    \+counter_table(_,N).
find_free_counter(N,Counter) :-
    N < 31,
    M is N+1,
    find_free_counter(M,Counter).

/* reset_counter(+Name)
   inc_counter(+Name)
   inc_counter(+Name, +Increment)

reset_counter/1 sets counter Name to 0.  inc_counter/1,2 increments counter
Name by Increment (if specified; otherwise, 1). 
Each predicate fails if Name is not a declared counter.  */

reset_counter(Name) :-
    counter_table(Name,Counter),
    !,
    ctr_set(Counter,0).
%reset_counter(Name) :-
%    format('ERROR: reset_counter/1 failed for unknown counter ~a.~n',[Name]),
%    !,
%    fail.

inc_counter(Name) :-
    counter_table(Name,Counter),
    !,
    ctr_inc(Counter).
%inc_counter(Name) :-
%    format('ERROR: inc_counter/1 failed for unknown counter ~a.~n',[Name]),
%    !,
%    fail.

inc_counter(Name,Increment) :-
    counter_table(Name,Counter),
    !,
    ctr_inc(Counter,Increment).
%inc_counter(Name,_Increment) :-
%    format('ERROR: inc_counter/2 failed for unknown counter ~a.~n',[Name]),
%    !,
%    fail.

