%   Package: assoc
%   Author : Richard A. O'Keefe
%   Updated: 19 Jul 1991
%   Purpose: Binary tree implementation of "association lists".

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

%   Note   : the keys should be ground, the associated values need not be.
%   CHANGE : 2 December 1987; arguments of gen_assoc/3 re-ordered to be
%	     the same as the get_assoc* (i.e. to follow the arg/3 rule).

:- module(assoc, [
	empty_assoc/1,		% Assoc ->
	assoc_to_list/2,	% Assoc -> List
	gen_assoc/3,		% Key x Assoc x Val
	get_assoc/3,		% Key x Assoc -> Val
	get_next_assoc/4,	% Key x Assoc -> Key x Val
	get_prev_assoc/4,	% Key x Assoc -> Key x Val
	is_assoc/1,		% Assoc ->
	list_to_assoc/2,	% List -> Assoc
	ord_list_to_assoc/2,	% List -> Assoc
	map_assoc/2,		% Pred x Assoc ->
	map_assoc/3,		% Pred x Assoc -> Assoc
	max_assoc/3,		% Assoc -> Key x Val
	min_assoc/3,		% Assoc -> Key x Val
	portray_assoc/1,	% Assoc ->
	put_assoc/4		% Key x Assoc x Val -> Assoc
   ]).

:- meta_predicate
	map_assoc(1, +),
	map_assoc(2, +, -).

:- meta_predicate
           'map assoc'(+, 1),
           'map assoc'(+, -, 2).

:- mode
	empty_assoc(?),
	assoc_to_list(+, -),
	    assoc_to_list(+, -, +),
	gen_assoc(?, +, ?),
	get_assoc(+, +, ?),
	    get_assoc(+, +, +, +, +, ?),
	get_next_assoc(+, +, ?, ?),
	get_prev_assoc(+, +, ?, ?),
	is_assoc(+),
	    is_assoc(+, ?, ?),
	list_to_assoc(+, -),
	    list_to_assoc(+, +, +, -),
	ord_list_to_assoc(+, -),
	map_assoc(+, ?),
	    'map assoc'(?, +),
	map_assoc(+, ?, ?),
	    'map assoc'(?, ?, +),
	portray_assoc(+),
	    portray_assoc(+, +, -),
	put_assoc(+, +, +, -),
	    'put assoc'(+, +, +, -),
		'put assoc'(+, +, +, -, +,+,+,+).

/* type
	assoc(Key,Val) -->
		assoc
	    |	assoc(Key, Val, assoc(Key,Val), assoc(Key,Val)).

:- pred
	assoc_to_list(assoc(K,V), list(pair(K,V))),
	    assoc_to_list(assoc(K,V), list(pair(K,V)), list(pair(K,V))),
	gen_assoc(K, assoc(K,V), V),
	get_assoc(K, assoc(K,V), V),
	    get_assoc(order, K, V, assoc(K,V), assoc(K,V), V),
	get_next_assoc(K, assoc(K,V), K, V),
	get_prev_assoc(K, assoc(K,V), K, V),
	is_assoc(assoc(K,V)),
	    is_assoc(assoc(K,V), K, K),
	list_to_assoc(list(pair(K,V)), assoc(K,V)),
	    list_to_assoc(integer, list(pair(K,V)), list(pair(K,V)), assoc(K,V)),
	ord_list_to_assoc(list(pair(K,V)), assoc(K,V)),
	map_assoc(void(V), assoc(K,V)),
	    'map assoc'(assoc(K,V), void(V)),
	map_assoc(void(V,U), assoc(K,V), assoc(K,U)),
	    'map assoc'(assoc(K,V), assoc(K,U), void(V,U)),
	max_assoc(assoc(K,V), K, V),
	min_assoc(assoc(K,V), K, V),
	portray_assoc(assoc(K,V)),
	    portray_assoc(assoc(K,V), atom, atom),
	put_assoc(K, assoc(K,V), V, assoc(K,V)),
	    'put assoc'(assoc(K,V), K, V, assoc(K,V)),
		'put assoc'(order, K, , V, assoc(K,V)K, V, assoc(K,V), assoc(K,V)).
*/




%@  @cindex association list
%@  @cindex list, association
%@  @cindex binary tree
%@  @cindex tree, binary
%@  This library provides a binary tree implementation of "association
%@  lists". The binary tree is @emph{not} kept balanced, as opposed to
%@  @code{library(avl)}, which provides similar functionality based on balanced
%@  AVL trees.
%@
%@  Exported predicates:
%@  
%@  @table @code

%@  @item empty_assoc(@var{?Assoc})
%@  @PLXindex {empty_assoc/1 (assoc)}
%@  is true when @var{Assoc} is an empty assoc.

empty_assoc(assoc).

%@  @item assoc_to_list(@var{+Assoc}, @var{-List})
%@  @PLXindex {assoc_to_list/2 (assoc)}
%@  assumes that @var{Assoc} is a proper "assoc" tree, and is true when
%@  @var{List} is a list of @var{Key-Value} pairs in ascending order with no
%@  duplicate @var{Keys} specifying the same finite function as @var{Assoc}.
%@  Use this to convert an assoc to a list.

assoc_to_list(Assoc, List) :-
	assoc_to_list(Assoc, List, []).


assoc_to_list(assoc) --> [].
assoc_to_list(assoc(Key,Val,L,R)) -->
	assoc_to_list(L),
	[Key-Val],
	assoc_to_list(R).



%@  @item gen_assoc(@var{?Key}, @var{+Assoc}, @var{?Value})
%@  @PLXindex {gen_assoc/3 (assoc)}
%@  assumes that @var{Assoc} is a proper "assoc" tree, and is true when
%@  @var{Key} is associated with @var{Value} in @var{Assoc}.  Use this to enumerate
%@  @var{Keys} and @var{Values} in the @var{Assoc}, or to find @var{Keys} associated with
%@  a particular @var{Value}.  If you want to look up a particular @var{Key},
%@  you should use @code{get_assoc/3}.  Note that this predicate is not
%@  determinate.  If you want to maintain a finite bijection, it
%@  is better to maintain two assocs than to drive one both ways.
%@  The @var{Keys} and @var{Values} are enumerated in ascending order of @var{Keys}.
:- gen_assoc(?,+,?) is nondet.
gen_assoc(Key, assoc(_,_,L,_), Val) :-
	gen_assoc(Key, L, Val).
gen_assoc(Key, assoc(Key,Val,_,_), Val).
gen_assoc(Key, assoc(_,_,_,R), Val) :-
	gen_assoc(Key, R, Val).



%@  @item get_assoc(@var{+Key}, @var{+Assoc}, @var{-Value})
%@  @PLXindex {get_assoc/3 (assoc)}
%@  assumes that @var{Assoc} is a proper "assoc" tree.  It is true when
%@  @var{Key} is identical to (@code{==}) one of the keys in @var{Assoc}, and Value
%@  unifies with the associated value.  Note that since we use the
%@  term ordering to identify keys, we obtain logarithmic access,
%@  at the price that it is not enough for the @var{Key} to unify with a
%@  key in @var{Assoc}, it must be identical.  This predicate is determinate.
%@  The argument order follows the pattern established by the
%@  built-in predicate @code{arg/3} (called the @code{arg/3}, or selector, rule):
%@  @example
%@      @var{predicate(indices, structure, element)}.
%@  @end example
%@  The analogy with @code{arg(@var{N}, @var{Term}, @var{Element})} is that
%@  @example
%@      @var{Key:N :: Assoc:Term :: Value:Element}.
%@  @end example

get_assoc(Key, assoc(K,V,L,R), Val) :-
	compare(Rel, Key, K),
	get_assoc(Rel, Key, V, L, R, Val).


get_assoc(=, _, Val, _, _, Val).
get_assoc(<, Key, _, Tree, _, Val) :-
	get_assoc(Key, Tree, Val).
get_assoc(>, Key, _, _, Tree, Val) :-
	get_assoc(Key, Tree, Val).




%@  @item get_next_assoc(@var{+Key}, @var{+Assoc}, @var{-Knext}, @var{-Vnext})
%@  @PLXindex {get_next_assoc/4 (assoc)}
%@  is true when @var{Knext} is the smallest key in @var{Assoc} such that @var{Knext@@>Key},
%@  and @var{Vnext} is the value associated with @var{Knext}.  If there is no such
%@  @var{Knext}, @code{get_next_assoc/4} naturally fails.  It assumes that @var{Assoc} is
%@  a proper assoc.  @var{Key} should normally be ground.  Note that there is
%@  no need for @var{Key} to be in the association at all.  You can use this
%@  predicate in combination with @code{min_assoc/3} to traverse an association
%@  tree; but if there are @var{N} pairs in the tree the cost will be @var{O(N lg N)}.
%@  If you want to traverse all the pairs, calling @code{assoc_to_list/2} and
%@  walking down the list will take @var{O(N)} time.

get_next_assoc(Key, assoc(K,V,L,R), Knext, Vnext) :-
	(   K @=< Key ->
	    get_next_assoc(Key, R, Knext, Vnext)
	;   get_next_assoc(Key, L, Knext, Vnext) ->
	    true
	;   Knext = K, Vnext = V
	).




%@  @item get_prev_assoc(@var{+Key}, @var{+Assoc}, @var{-Kprev}, @var{-Vprev})
%@  @PLXindex {get_prev_assoc/4}
%@  is true when @var{Kprev} is the largest key in @var{Assoc} such that @var{Kprev@@<Key},
%@  and @var{Vprev} is the value associated with @var{Kprev}.  You can use this
%@  predicate in combination with @code{max_assoc/3} to traverse an assoc.
%@  See the notes on @code{get_next_assoc/4}.

get_prev_assoc(Key, assoc(K,V,L,R), Kprev, Vprev) :-
	(   K @>= Key ->
	    get_prev_assoc(Key, L, Kprev, Vprev)
	;   get_prev_assoc(Key, R, Kprev, Vprev) ->
	    true
	;   Kprev = K, Vprev = V
	).




%@  @item is_assoc(@var{+Thing})
%@  @PLXindex {is_assoc/1 (assoc)}
%@  is true when @var{Thing} is a (proper) association tree.  If you use the
%@  routines in this file, you have no way of constructing a tree with
%@  an unbound tip, and the heading of this file explicitly warns
%@  against using variables as keys, so such structures are NOT
%@  recognised as being association trees.  Note that the code relies
%@  on variables (to be precise, the first anonymous variable in
%@  @code{is_assoc/1}) being @code{@@<} than any non-variable.

is_assoc(Assoc) :-
	is_assoc(Assoc, _, _).

is_assoc(X, _, _) :- var(X), !, fail.		% catch and reject variables
is_assoc(assoc, Min, Min).
is_assoc(assoc(Key,_,L,R), Min, Max) :-
	is_assoc(L, Min, Mid),
	Mid @< Key,
	is_assoc(R, Key, Max).



%@  @item list_to_assoc(@var{+List}, @var{-Assoc})
%@  @PLXindex {list_to_assoc/2 (assoc)}
%@  is true when @var{List} is a proper list of @var{Key-Val} pairs (in any order)
%@  and @var{Assoc} is an association tree specifying the same finite function
%@  from @var{Keys} to @var{Values}.  Note that the list should not contain any
%@  duplicate keys.  In this release, @code{list_to_assoc/2} doesn't check for
%@  duplicate keys, but the association tree which gets built won't work.

list_to_assoc(List, Assoc) :-
	keysort(List, Pairs),
	length(Pairs, N),
	list_to_assoc(N, Pairs, [], Assoc).


list_to_assoc(0, List, List, assoc) :- !.
list_to_assoc(N, List, Rest, assoc(Key,Val,L,R)) :-
	A is (N-1) >> 1,
	Z is (N-1) - A,
	list_to_assoc(A, List, [Key-Val|More], L),
	list_to_assoc(Z, More, Rest, R).



%@  @item ord_list_to_assoc(@var{+List}, @var{-Assoc})
%@  @PLXindex {ord_list_to_assoc/2 (assoc)}
%@  is a version of @code{list_to_assoc/2} which trusts you to have sorted
%@  the list already.  If you pair up an ordered set with suitable
%@  values, calling this instead will save the sort.

ord_list_to_assoc(List, Assoc) :-
	length(List, N),
	list_to_assoc(N, List, [], Assoc).



%@  @item map_assoc(@var{:Pred}, @var{+Assoc})
%@  @PLXindex {map_assoc/2 (assoc)}
%@  is true when @var{Assoc} is a proper association tree, and for each
%@  @var{Key->Val} pair in @var{Assoc}, the proposition @var{Pred(Val)} is true.
%@  @var{Pred} must be a closure, and @var{Assoc} should be proper.
%@  There should be a version of this predicate which passes @var{Key}
%@  to @var{Pred} as well as @var{Val}, but there isn't.

map_assoc(Pred, Assoc) :-
	/* we should check Pred here */
	'map assoc'(Assoc, Pred).

'map assoc'(assoc, _).
'map assoc'(assoc(_,Val,L,R), Pred) :-
	'map assoc'(L, Pred),
	call(Pred, Val),
	'map assoc'(R, Pred).



%@  @item map_assoc(@var{:Pred}, @var{?OldAssoc}, @var{?NewAssoc})
%@  @PLXindex {map_assoc/3 (assoc)}
%@  is true when @var{OldAssoc} and @var{NewAssoc} are association trees of the
%@  same shape (at least one of them should be provided as a proper
%@  assoc, or @code{map_assoc/3} may not terminate), and for each @var{Key},
%@  if @var{Key} is associated with @var{Old} in @var{OldAssoc} and with @var{New} in
%@  @var{NewAssoc}, the proposition @var{Pred(Old,New)} is true.  Normally we
%@  assume that @var{Pred} is a function from @var{Old} to @var{New}, but the code
%@  does not require that.  There should be a version of this
%@  predicate which passes @var{Key} to @var{Pred} as well as @var{Old} and @var{New},
%@  but there isn't.  If you'd have a use for it, please tell us.

map_assoc(Pred, OldAssoc, NewAssoc) :-
	% we should check Pred here!
	'map assoc'(OldAssoc, NewAssoc, Pred).

'map assoc'(assoc, assoc, _).
'map assoc'(assoc(Key,Old,L0,R0), assoc(Key,New,L1,R1), Pred) :-
	'map assoc'(L0, L1, Pred),
	call(Pred, Old, New),
	'map assoc'(R0, R1, Pred).



%@  @item max_assoc(@var{+Assoc}, @var{-Key}, @var{-Val})
%@  @PLXindex {max_assoc/3 (assoc)}
%@  is true when @var{Key} is the largest @var{Key} in @var{Assoc}, and @var{Val} is the
%@  associated value.  It assumes that @var{Assoc} is a proper assoc.
%@  This predicate is determinate.  If @var{Assoc} is empty, it just
%@  fails quietly; an empty set can have no largest element!

max_assoc(assoc(K,V,_,R), Key, Val) :-
	(   atom(R) ->		% the right son is empty, K is biggest
	    Key = K, Val = V
	;   max_assoc(R, Key, Val)
	).




%@  @item min_assoc(@var{+Assoc}, @var{-Key}, @var{-Val})
%@  @PLXindex {min_assoc/3 (assoc)}
%@  is true when @var{Key} is the smallest @var{Key} in @var{Assoc}, and @var{Val} is the
%@  associated value.  It assumes that @var{Assoc} is a proper assoc.
%@  This predicate is determinate.  If @var{Assoc} is empty, it just
%@  fails quietly; an empty set can have no smallest element!

min_assoc(assoc(K,V,L,_), Key, Val) :-
	(   atom(L) ->		% the left son is empty, K is smallest
	    Key = K, Val = V
	;   min_assoc(L, Key, Val)
	).




%@  @item portray_assoc(@var{+Assoc})
%@  @PLXindex {portray_assoc/1 (assoc)}
%@  writes an association tree to the current output stream in a
%@  pretty form so that you can easily see what it is.  Note that
%@  an association tree written out this way can NOT be read back
%@  in.  For that, use @code{writeq/1}.  The point of this predicate is
%@  @c that you can add a directive
%@  @c     :- add_portray(portray_assoc).
%@  to get association trees displayed nicely by @code{print/1}.

portray_assoc(assoc) :- !,
	write('Assoc{'),
	put_code(0'}).
portray_assoc(Assoc) :-
	is_assoc(Assoc),
	portray_assoc(Assoc, 'Assoc{', _),
	put_code(0'}).


portray_assoc(assoc, M, M).
portray_assoc(assoc(Key,Val,L,R), M0, M) :-
	portray_assoc(L, M0, M1),
	write(M1),
	print(Key), write(->), print(Val),
	portray_assoc(R, ',', M).




%@  @item put_assoc(@var{+Key}, @var{+OldAssoc}, @var{+Val}, @var{-NewAssoc})
%@  @PLXindex {put_assoc/4 (assoc)}
%@  is true when @var{OldAssoc} and @var{NewAssoc} define the same finite function,
%@  except that @var{NewAssoc} associates @var{Val} with @var{Key}.  @var{OldAssoc} need not
%@  have associated any value at all with Key,

put_assoc(Key, OldAssoc, Val, NewAssoc) :-
	'put assoc'(OldAssoc, Key, Val, NewAssoc).

'put assoc'(assoc, Key, Val, assoc(Key,Val,assoc,assoc)).
'put assoc'(assoc(K,V,L,R), Key, Val, New) :-
	compare(Rel, Key, K),
	'put assoc'(Rel, Key, Val, New, K, V, L, R).


'put assoc'(=, Key, Val, assoc(Key,Val,L,R), _, _, L, R).
'put assoc'(<, Key, Val, assoc(K,V,Tree,R), K, V, L, R) :-
	'put assoc'(L, Key, Val, Tree).
'put assoc'(>, Key, Val, assoc(K,V,L,Tree), K, V, L, R) :-
	'put assoc'(R, Key, Val, Tree).



/* ----------------------------------------------------------------------
test :-
	ord_list_to_assoc([a-1,b-2,c-3,d-4,e-5,g-6,h-7], Assoc),
	portray_assoc(Assoc), nl,
	min_assoc(Assoc, Kmin, Vmin),
	write((Kmin->Vmin)), nl,
	max_assoc(Assoc, Kmax, Vmax),
	write((Kmax->Vmax)), nl,
	get_next_assoc(Kmin, Assoc, Knxt, Vnxt),
	write((Knxt->Vnxt)), nl,
	get_prev_assoc(Kmax, Assoc, Kprv, Vprv),
	write((Kprv->Vprv)), nl,
	get_prev_assoc(Kmin, Assoc, _, _).
---------------------------------------------------------------------- */

%@  @end table
