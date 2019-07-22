%   Package: varnumbers
%   Author : Richard A. O'Keefe
%   Updated: 31 Mar 1988
%   Purpose: Inverse of numbervars/3

%   The idea was copied from NU Prolog, but the source code is
%   Copyright (C) 1988, Quintus Computer Systems, Inc.  All rights reserved.

:- module(varnumbers, [
	numbervars/1,
	varnumbers/2,
	varnumbers/3
   ]).

:- use_module(library(types), [
	must_be/4
   ]).



%@  @cindex numbervars
%@  The built-in predicate @code{numbervars/3} makes a term
%@  ground by binding the variables in it to subterms of the form @code{'$VAR'(@var{N})}
%@  where @var{N} is an integer.  Most of the calls to @code{numbervars/3} look like
%@  @example
%@      numbervars(Term, 0, _)
%@  @end example
%@  which can be abbreviated to
%@  @example
%@      numbervars(Term)
%@  @end example
%@  if you use this package.
%@  
%@  @code{varnumbers/3} is a partial inverse to @code{numbervars/3}:
%@  @example
%@      varnumbers(Term, N0, Copy)
%@  @end example
%@  unifies @var{Copy} with a copy of @var{Term} in which subterms of the form
%@  @code{'$VAR'(@var{N})} where @var{N} is an integer not less than @var{N0} (that is, subterms
%@  which might have been introduced by @code{numbervars/3} with second argument
%@  @var{N0}) have been consistently replaced by new variables.  Since 0 is the
%@  usual second argument of @code{numbervars/3}, there is also
%@  @example
%@      varnumbers(Term, Copy)
%@  @end example
%@  
%@  This provides a facility whereby a Prolog-like data base can be
%@  kept as a term.  For example, we might represent @code{append/3} thus:
%@  @example
%@      Clauses = [
%@          (append([], '$VAR'(0), '$VAR'(0)) :- true),
%@          (append(['$VAR'(0)|'$VAR'(1), '$VAR'(2), ['$VAR'(0)|'$VAR(3)]) :-
%@              append('$VAR'(1), '$VAR'(2), '$VAR'(3)))
%@      ]
%@  @end example
%@  and we might access clauses from it by doing
%@  @example
%@  @group
%@      prove(Goal, Clauses) :-
%@              member(Clause, Clauses),
%@              varnumbers(Clause, (Goal:-Body)),
%@              prove(Goal).
%@  @end group
%@  @end example
%@  @c I'm afraid this has been thrown together in an hour.  More work needs
%@  @c to be done to make it easy to follow.  It has, however, been tested.
%@  
%@  Exported predicates:
%@  
%@  @table @code

%@  @item numbervars(@var{+Term})
%@  @PLXindex {numbervars/1 (varnumbers)}
%@  makes @var{Term} ground by binding variables to subterms @code{'$VAR'(@var{N})} with
%@  values of @var{N} ranging from 0 up.

numbervars(Term) :-
	numbervars(Term, 0, _).


%@  @item varnumbers(@var{+Term}, @var{-Copy})
%@  @PLXindex {varnumbers/[2,3] (varnumbers)}xo
%@  succeeds when @var{Term} was a term producing by calling @code{numbervars(@var{Term})}
%@  and @var{Copy} is a copy of @var{Term} with such subterms replaced by variables.

varnumbers(Term, Copy) :-
	varnumbers(Term, 0, Copy).

%@  @item varnumbers(@var{+Term}, @var{+N0}, @var{-Copy})
%@  succeeds when @var{Term} was a term produced by calling
%@    @code{numbervars(@var{Term}, @var{N0}, @var{N})}
%@  (so that all subterms @code{'$VAR'(@var{X})} have @code{integer(@var{X})}, @code{@var{X} >= @var{N0}})
%@  and @var{Copy} is a copy of @var{Term} with such subterms replaced by variables.

varnumbers(Term, N0, Copy) :-
	(   integer(N0) ->
	    B0 is N0-1,
	    max_var_number(Term, B0, B1),
	    Nvars is B1-B0,		% number of variables to be created
	    (   Nvars >= 256 ->
		make_map(Nvars, Map),
		apply_map(Term, N0, Map, Copy)
	    ;	Nvars > 0 ->
		functor(Map, '', Nvars),
		apply_small_map(Term, B0, Map, Copy)
	    ;	Nvars =:= 0 ->
		Copy = Term
	    )
	;   must_be(N0, integer, varnumbers(Term,N0,Copy), 2)
	).


%.  max_var_number(+Term, +B0, -B)

max_var_number('$VAR'(N), B0, B) :- integer(N), !,
	B is max(B0,N).
max_var_number(Var, B, B) :- var(Var), !.
max_var_number(Term, B0, B) :-
	(   foreacharg(Arg,Term),
	    fromto(B0,B1,B2,B)
	do  max_var_number(Arg, B1, B2)
	).

%.  apply_small_map(+Term, +B0, +Map, -Copy)

apply_small_map('$VAR'(N), B0, Map, Var) :-
	integer(N),
	I is N-B0,
	I > 0,
	!,
	arg(I, Map, Var).
apply_small_map(Var, _, _, Copy) :- var(Var), !,
	Copy = Var.
apply_small_map(Term, B0, Map, Copy) :-
	functor(Term, F, N),
	functor(Copy, F, N),
	(   for(I,1,N),
	    param([Term,B0,Map,Copy])
	do  arg(I, Term, Term_N),
	    arg(I, Copy, Copy_N),
	    apply_small_map(Term_N, B0, Map, Copy_N)
	).

%.  apply_map(+Term, +N0, +Map, -Copy)

apply_map('$VAR'(N), N0, Map, Var) :-
	integer(N),
	I is N-N0,
	I >= 0,
	!,
	map_get(Map, I, Var).
apply_map(Var, _, _, Copy) :- var(Var), !,
	Copy = Var.
apply_map(Term, N0, Map, Copy) :-
	functor(Term, F, N),
	functor(Copy, F, N),
	(   foreacharg(Term_N,Term),
	    foreacharg(Copy_N,Copy),
	    param([N0,Map])
	do  apply_map(Term_N, N0, Map, Copy_N)
	).

make_map(N, Map) :-
    (	N < 256 ->
	functor(Map, '', N)
    ;	A is (N+7)>>3,
	make_map(A, M1), make_map(A, M2),
	make_map(A, M3), make_map(A, M4),
	make_map(A, M5), make_map(A, M6),
	make_map(A, M7), make_map(A, M8),
	Map = +(M1,M2,M3,M4,M5,M6,M7,M8)
    ).

map_get(Map, N, Var) :-
	functor(Map, F, _),
	map_get(F, Map, N, Var).

map_get(+, Map, N, Var) :-
	I is (N/\7) + 1,
	M is (N>>3),
	arg(I, Map, Sub),
	map_get(Sub, M, Var).
map_get('', Map, N, Var) :-
	I is N+1,
	arg(I, Map, Var).

%@  @end table

