%   Package: between
%   Author : Richard A. O'Keefe
%   Updated: 26 Feb 2009, Mats Carlsson
%   Purpose: Generate integers.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(between, [
	between/3,		%   Lower x Upper x Bounded
	gen_int/1,		%   Integer
	gen_nat/1,		%   Natural
	numlist/2,		%   Upper -> List
	numlist/3,		%   Lower x Upper -> List
	numlist/5,		%   Lower x Step x Upper x Length -> List
	repeat/1		%   Natural
   ]).
:- use_module(library(types), [
	must_be/4,
	illarg/3
   ]).
:- mode
	between(+, +, ?),
	    between1(+, +, -),
	gen_int(?),
	gen_nat(?),
	gen_nat(+, -),
	numlist(?, ?, ?),
	    % anchor(+, +, -, -),
		% anchor(+, +, -),
	    % numlist1(+, +, ?),
		% numlist2(+, +, ?),
	repeat(+).

%@  This library module provides some means of generating integers.
%@  Exported predicates:
%@  
%@  @table @code

%@  @item between(@var{+Lower}, @var{+Upper}, @var{-Number})
%@  @PLXindex {between/3 (between)}
%@  is true when @var{Lower}, @var{Upper}, and @var{Number} are integers,
%@  and @var{Lower =< Number =< Upper}.  If @var{Lower} and @var{Upper} are given,
%@  @var{Number} can be tested or enumerated.  If either @var{Lower} or @var{Upper}
%@  is absent, there is not enough information to find it, and an
%@  error will be reported.

between(Lower, Upper, Point) :-
	integer(Lower),
	integer(Upper),
	(   integer(Point), !,		%  These cuts must be cuts;
	    Lower =< Point, Point =< Upper
	;   var(Point), !,		%  they can't be arrows.
	    Lower =< Upper,
	    between1(Lower, Upper, Point)
	).
between(Lower, Upper, Point) :-
	Goal = between(Lower,Upper,Point),
	must_be(Lower, integer, Goal, 1),
	must_be(Upper, integer, Goal, 2),
	must_be(Point, integer, Goal, 3).



%%  between1(Lower, Upper, Point)
%   enumerates values of Point satisfying Lower =< Point =< Upper,
%   where it is already known that Lower =< Upper and Point was a
%   variable.  A purer version of this is left as a comment.
:- between1/3 is nondet.
between1(L, U, N) :- L=:=U, !, N = L.
between1(L, _, L).		% between1(L, U, L) :- L =< U.
between1(L, U, N) :-		% between1(L, U, N) :- L < U,
	M is L+1,		%	M is L+1,
	between1(M, U, N).	%	between1(M, U, N).



%@  @item gen_nat(@var{?N})
%@  @PLXindex {gen_nat/1 (between)}
%@  is true when @var{N} is a natural number.  If @var{N} is a variable, it will
%@  enumerate the natural numbers 0,1,2,... and of course not terminate.
%@  It is not meant to be applied to anything but integers and variables.

gen_nat(N) :-			% gen-erate nat-ural
	(   integer(N) ->	% if we aren't to generate it
	    N >= 0		% test that it is not negative
	;   var(N) ->		% if we are to generate it,
	    gen_nat(0, N)	% do so counting up from 0.
	;   must_be(N, integer, gen_nat(N), 1)
	).

:- gen_nat/2 is nondet. 
gen_nat(L, L).
gen_nat(L, N) :-		% generate natural > L
	M is L+1,
	gen_nat(M, N).		% generate natural >= M

%@  @item gen_int(@var{?I})
%@  @PLXindex {gen_int/1 (between)}
%@  is true when I is an integer.  If I is a variable, it will
%@  enumerate the integers in the order 0, 1, -1, 2, -2, 3, -3, &c.
%@  Of course this sequence has no end.
%@  It is not meant to be applied to anything but integers and variables.

gen_int(I) :-			% gen-erate int-eger
	(   integer(I) ->	% if we aren't to generate it
	    true		% just succeed.
	;   var(I) ->		% if we are to generate it,
	    gen_int(0, I)	% do so starting from 0.
	;   must_be(I, integer, gen_int(I), 1)
	).

:- gen_int/2 is nondet. 
gen_int(L, L).
gen_int(L, N) :-
	(   L > 0 -> M is -L	% 1-> -1, 2-> -2, &c
	;   M is 1-L		% 0-> 1, -1-> 2, &c
	),
	gen_int(M, N).


%@  @item repeat(@var{+N})
%@  @PLXindex {repeat/1 (between)}
%@  (where @var{N} is a non-negative integer) succeeds exactly @var{N} times.
%@  You can only understand it procedurally, and really it is only
%@  included for compatibility with some other Prologs.

repeat(N) :-
	(   integer(N) ->
	    N >= 1,
	    repeat1(N)
	;   must_be(N, integer, repeat(N), 1)
	).

:- repeat1/1 is nondet.
repeat1(1) :- !.		% the structure of this
repeat1(_).			% predicate is parallel to
repeat1(N) :-			% the structure of
	M is N-1,		% between1/3 above.
	repeat1(M).


%@  @item numlist(@var{?Upper}, @var{?List})
%@  @PLXindex {numlist/[2,3,5] (between)}
%@  is true when @var{List} is the list of integers @var{[1, ..., Upper]}.
%@  For example, @code{numlist(3,L)} binds @code{L = [1,2,3]}.

numlist(Upper, List) :-
	numlist(1, 1, Upper, _, List).

%@  @item numlist(@var{?Lower}, @var{?Upper}, @var{?List})
%@  is true when @var{List} is @var{[Lower, ..., Upper]}, @var{Lower} and @var{Upper} integers.
%@  For example, @code{numlist(1, 3, L)} binds @code{L = [1,2,3]}.

numlist(Lower, Upper, List) :-
	numlist(Lower, 1, Upper, _, List).

%@  @item numlist(@var{?Lower}, @var{?Step}, @var{?Upper}, @var{?Length}, @var{?List})
%@  is true when @var{List} is the list of integers @var{[Lower, Lower+Step, ..., Upper]} and
%@  of length @var{Length}.
%@  For example, @code{numlist(L,2,U,S,[1,X,Y,Z])} binds @code{L=1, S=4, U=7, X=3, U=5, Z=7}.

numlist(Lower, Inc, Upper, Length, List) :-
	List = [Lower|_],
	(   proper_list_last(List, Lower, Last) ->
	    Upper = Last,
	    length(List, Length)
	;   true
	),
	Goal = numlist(Lower,Inc,Upper,Length,List),
	(   var(Lower) -> numlist_l(Lower, Inc, Upper, Length, Goal)
	;   var(Inc)   -> numlist_i(Lower, Inc, Upper, Length, Goal)
	;   var(Length)-> numlist_s(Lower, Inc, Upper, Length, Goal)
	;   true
	),
	Upper is Lower + Inc*(Length-1), % also common postcond
	length(List, Length),
	numlist_fill(List, Lower, Inc).

numlist_l(Lower, Inc, Upper, Length, Goal) :-
	must_be(Inc  , integer, Goal, 2),
	must_be(Upper, integer, Goal, 3),
	must_be(Length,integer, Goal, 4),
	Lower is Upper-Inc*(Length-1).

numlist_i(Lower, Inc, Upper, Length, Goal) :-
	must_be(Upper, integer, Goal, 3),
	must_be(Length,integer, Goal, 4),
	(   Length=:=1 ->
	    Lower = Upper,
	    illarg(var, Goal, 2)
	;   Inc is (Upper-Lower) // (Length-1)
	).	    

numlist_s(Lower, Inc, Upper, Length, Goal) :-
	must_be(Upper, integer, Goal, 3),
	(   Inc=:=0 ->
	    Lower = Upper,
	    illarg(var, Goal, 4)
	;   Length is (Upper-Lower) // Inc + 1
	).

numlist_fill([], _, _).
numlist_fill([X|Xs], X, I) :-
	Y is X+I,
	numlist_fill(Xs, Y, I).

proper_list_last(V, _, _) :- var(V), !, fail.
proper_list_last([], T, T).
proper_list_last([T0|L], _, T) :-
	proper_list_last(L, T0, T).


%@  @end table
