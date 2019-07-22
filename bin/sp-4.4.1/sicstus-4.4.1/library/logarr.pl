%   Package: logarr
%   Authors: Mostly David H.D. Warren, some changes by Fernando C. N.  Pereira
%   Updated: 09 Jan 1990
%   Purpose: Extendable arrays with logarithmic access time.

%   Adapted from shared code written by the same authors; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(logarr, [
	new_array/1,
	is_array/1,
	aref/3,
	arefa/3,
	arefl/3,
	aset/4,
	alist/2
   ]).
:- use_module(library(types), [
	must_be/4
   ]).
:- mode
	aref(+, +, ?),
	arefa(+, +, ?),
	arefl(+, +, ?),
	alist(+, -),
	aset(+, +, +, -),
	array_item(+, +, +, +, ?),
	is_array(+),
	new_array(-),
	not_undef(+),
	subarray_to_list(+, +, +, +, ?, ?),
	update_subarray(+, +, ?, ?, -).



/*  An array extends from 0 to (2^Size)-1, where Size is even.
    Note that 2^Size = 1<<Size.
*/


%@  This libary module provides extendible arrays with logarithmic access time.
%@  @strong{Please note:} the atom @code{$} is used to indicate an unset element, and the
%@  functor @code{$/4} is used to indicate a subtree.  In general, array
%@  elements whose principal function symbol is @code{$} will not work.
%@  
%@  Exported predicates:
%@  
%@  @table @code
%@  new_array(@var{-A})
%@      returns a new empty array @var{A}.

new_array(array($($,$,$,$),2)).


%@  @item is_array(@var{+A})
%@  checks whether @var{A} is an array.

is_array(array(_,_)).


%@  @item alist(@var{+Array}, @var{-List})
%@  returns a list of pairs @var{Index-Element} of all the elements
%@  of @var{Array} that have been set.

alist(array($(A0,A1,A2,A3),Size), L0) :-
	N is Size-2,
	subarray_to_list(0, N, 0, A0, L0, L1),
	subarray_to_list(1, N, 0, A1, L1, L2),
	subarray_to_list(2, N, 0, A2, L2, L3),
	subarray_to_list(3, N, 0, A3, L3, []).

subarray_to_list(K, 0, M, Item, [N-Item|L], L) :-
	not_undef(Item), !,
	N is K+M.
subarray_to_list(K, N, M, $(A0,A1,A2,A3), L0, L) :-
	N > 0, !,
	N1 is N-2,
	M1 is (K+M) << 2,
	subarray_to_list(0, N1, M1, A0, L0, L1),
	subarray_to_list(1, N1, M1, A1, L1, L2),
	subarray_to_list(2, N1, M1, A2, L2, L3),
	subarray_to_list(3, N1, M1, A3, L3, L).
subarray_to_list(_, _, _, _, L, L).



aerr(Index, Array, Goal) :-
	must_be(Index, integer(>=(0)), Goal, 1),
	must_be(Array, array, Goal, 2).



%@  @item aref(@var{+Index}, @var{+Array}, @var{-Element})
%@  unifies @var{Element} with @var{Array[Index]}, or fails if @var{Array[Index]}
%@  has not been set.

aref(Index, array(Array,Size), Item) :-
	integer(Size),
	integer(Index), 0 =< Index,
	!,
	Index < 1<<Size,
	N is Size-2,
	Subindex is Index>>N /\ 3,
	array_item(Subindex, N, Index, Array, Item).
aref(Index, Array, Item) :-
	aerr(Index, Array, aref(Index,Array,Item)).


%@  @item arefa(@var{+Index}, @var{+Array}, @var{-Element})
%@  is as @code{aref/3}, except that it unifies @var{Element} with a new array if
%@  @var{Array[Index]} is undefined.  This is useful for multidimensional
%@  arrays implemented as arrays of arrays.

arefa(Index, array(Array,Size), Item) :-
	integer(Size),
	integer(Index), 0 =< Index,
	!,
	(   Index < 1<<Size,
	    N is Size-2,
	    Subindex is Index>>N /\ 3,
	    array_item(Subindex, N, Index, Array, Value),
	    !,
	    Item = Value
	;   new_array(Item)
	).
arefa(Index, Array, Item) :-
	aerr(Index, Array, arefa(Index,Array,Item)).


%@  @item arefl(@var{+Index}, @var{+Array}, @var{-Element})
%@  is as @code{aref/3}, except that @var{Element} appears as @code{[]} for
%@  undefined cells.

arefl(Index, array(Array,Size), Item) :-
	integer(Size),
	integer(Index), 0 =< Index,
	!,
	(   Index < 1<<Size,
	    N is Size-2,
	    Subindex is Index>>N /\ 3,
	    array_item(Subindex, N, Index, Array, Value),
	    !,
	    Item = Value
	;   Item = []
	).
arefl(Index, Array, Item) :-
	aerr(Index, Array, arefl(Index,Array,Item)).



%@  @item aset(@var{+Index}, @var{+Array}, @var{+Element}, @var{-NewArray})
%@  unifies @var{NewArray} with the result of setting @var{Array[Index]}
%@  to @var{Element}.

aset(Index, array(Array0,Size0), Item, NewArray) :-
	integer(Size0),
	integer(Index), 0 =< Index,
	!,
        NewArray = array(Array,Size),
	enlarge_array(Index, Size0, Array0, Size, Array1),
	update_array_item(Size, Index, Array1, Item, Array).
aset(Index, Array0, Item, Array) :-
	aerr(Index, Array0, aset(Index,Array0,Item,Array)).



% Guts

enlarge_array(I, Size0, Array0, Size, Array) :-
    (	I < 1<<Size0 ->
	Size = Size0, Array = Array0
    ;	Size1 is Size0+2,
	enlarge_array(I, Size1, $(Array0,$,$,$), Size, Array)
    ).


array_item(0, 0,_Index, $(Item,_,_,_), Item) :- !,
	not_undef(Item).
array_item(0, N, Index, $(Array,_,_,_), Item) :-
	N1 is N-2,
	Subindex is Index >> N1 /\ 3,
	array_item(Subindex, N1, Index, Array, Item).
array_item(1, 0,_Index, $(_,Item,_,_), Item) :- !,
	not_undef(Item).
array_item(1, N, Index, $(_,Array,_,_), Item) :-
	N1 is N-2,
	Subindex is Index >> N1 /\ 3,
	array_item(Subindex, N1, Index, Array, Item).
array_item(2, 0,_Index, $(_,_,Item,_), Item) :- !,
	not_undef(Item).
array_item(2, N, Index, $(_,_,Array,_), Item) :-
	N1 is N-2,
	Subindex is Index >> N1 /\ 3,
	array_item(Subindex, N1, Index, Array, Item).
array_item(3, 0,_Index, $(_,_,_,Item), Item) :- !,
	not_undef(Item).
array_item(3, N, Index, $(_,_,_,Array), Item) :-
	N1 is N-2,
	Subindex is Index >> N1 /\ 3,
	array_item(Subindex, N1, Index, Array, Item).


not_undef($) :- !,
	fail.
not_undef(_).


update_array_item(0,_Index,_Item, NewItem, NewItem) :- !.
update_array_item(N, Index, Array, NewItem, NewArray) :-
	N1 is N-2,
	Subindex is Index >> N1 /\ 3,
	update_subarray(Subindex, Array, Array1, NewArray1, NewArray),
	update_array_item(N1, Index, Array1, NewItem, NewArray1).


update_subarray(I, $, X, X1, Array) :- !,
	update_subarray(I, $($,$,$,$), X, X1, Array).
update_subarray(0, $(W,X,Y,Z), W, W1, $(W1,X,Y,Z)).
update_subarray(1, $(W,X,Y,Z), X, X1, $(W,X1,Y,Z)).
update_subarray(2, $(W,X,Y,Z), Y, Y1, $(W,X,Y1,Z)).
update_subarray(3, $(W,X,Y,Z), Z, Z1, $(W,X,Y,Z1)).


%@  @end table
