%   Package: bags
%   Author : Richard A. O'Keefe
%   Updated: 14 Jan 1991
%   Purpose: Bag Utilities
%   SeeAlso: ordsets, maps

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

%@  @cindex bag
%@  @cindex multiset
%@  
%@  This library module provides operations on bags.
%@  Bags are also known as multisets.
%@  A bag @var{B} is a function from a set @code{dom(@var{B})} to the non-negative integers.
%@  For the purposes of this module, a bag is constructed from two functions:
%@  
%@  @table @code
%@  @item bag
%@  creates an empty bag
%@  @item bag(@var{E},@var{M},@var{B})
%@  extends the bag @var{B} with a @emph{new} element @var{E} which occurs
%@  with multiplicity @var{M}, and which precedes all elements of @var{B}
%@  in Prolog's order.
%@  @end table
%@  
%@  A bag is represented by a Prolog term mirroring its construction.  There
%@  is one snag with this: what are we to make of
%@  @example
%@      bag(f(a,Y), 1, bag(f(X,b), 1, bag))     ?
%@  @end example
%@  As a term it has two distinct elements, but @code{f(a,b)} will be reported as
%@  occurring in it twice.  But according to the definition above,
%@  @example
%@      bag(f(a,b), 1, bag(f(a,b), 1, bag))
%@  @end example
%@  is not the representation of any bag, that bag is represented by
%@  @example
%@      bag(f(a,b), 2, bag)
%@  @end example
%@  alone.  We are apparently stuck with a scheme which is only guaranteed
%@  to work for "sufficiently instantiated" terms, but then, that's true of 
%@  a lot of Prolog code.
%@  
%@  The reason for insisting on the order is to make union and 
%@  intersection linear in the sizes of their arguments.
%@  @code{library(ordsets)} does the same for ordinary sets.
%@  
%@  @c empty_bag/1, bag_add_element/[3,4], and bag_del_element/[3,4]
%@  @c were added at the suggestion of NCR.

:- module(bags, [
	bag_add_element/3,
	bag_add_element/4,
	bag_del_element/3,
	bag_del_element/4,
	bag_intersect/2,
	bag_intersection/2,
	bag_intersection/3,
	bag_max/2,
	bag_max/3,
	bag_min/2,
	bag_min/3,
	bag_subtract/3,
	bag_to_list/2,
	bag_to_ord_set/2,
	bag_to_ord_set/3,
	bag_to_set/2,
	bag_to_set/3,
	bag_union/2,
	bag_union/3,
	checkbag/2,
	empty_bag/1,
	is_bag/1,
	length/3,
	list_to_bag/2,
	make_sub_bag/2,
	mapbag/2,
	mapbag/3,
	member/3,
	memberchk/3,
	portray_bag/1,
	somebag/2,
	somechkbag/2,
	test_sub_bag/2
   ]).
:- meta_predicate
	checkbag(2, +),
	    check_bag(+, 2),
	mapbag(1, +),
	    map_bag(+, 1),
	mapbag(2, +, -),
	    map_bag_list(+, -, 2),
	somebag(2, +),
	somechkbag(2, +).

:- mode
	bag_add_element(+, +, ?),
	bag_add_element(+, +, +, ?),
	    bag_add_element_1(+, +, +, ?),
		bag_add_element_1(+, +, +, ?, +, +, +),
	bag_del_element(+, +, ?),
	bag_del_element(+, +, +, ?),
	    bag_del_element_1(+, +, +, ?),
		bag_del_element_1(+, +, +, ?, +, +, +),
	bag_intersection(+, ?),
	    bag_intersection_3(+, +, ?),
	bag_intersection(+, +, ?),
	    bag_intersection(+, +, +, +, ?),
		bag_intersection(+, +, +, +, +, +, +, ?),
	bag_intersect(+, +),
	    bag_intersect(+, +, +, +, +, +, +),
	bag_subtract(+, +, ?),
	    bag_subtract_1(+, +, +, +, ?),
	    bag_subtract_2(+, +, +, +, ?),
		bag_subtract(+, +, +, +, +, +, +, ?),
	bag_to_list(+, ?),
	    bag_to_list(+, ?, +, +),
	bag_to_ord_set(+, ?),
	bag_to_ord_set(+, +, ?),
	bag_to_set(+, ?),
	bag_to_set(+, +, ?),
	bag_union(+, ?),
	bag_union(+, +, ?),
	    bag_union(+, +, +, +, ?),
		bag_union(+, +, +, +, +, +, +, ?),
	bagform(+, ?),
	    bagform(+, +, +, -, -),
	bag_max(+, ?),
	bag_min(+, ?),
	    bag_scan(+, +, +, +, ?),
	bag_max(+, ?, ?),
	bag_min(+, ?, ?),
	    bag_scan(+, +, +, +, ?, ?),
	checkbag(2, +),
	    check_bag(+, 2),
	countdown(+, -),
	empty_bag(?),
	is_bag(+),
	    is_bag(+, +),
	length(+, ?, ?),
	    length(+, +, ?, +, ?),
	list_to_bag(+, -),
	make_sub_bag(+, -),
	mapbag(1, +),
	    map_bag(+, 1),
	mapbag(+, +, -),
	    map_bag_list(+, -, 2),
	member(?, ?, +),
	memberchk(+, ?, +),
	portray_bag(+),
	    portray_bag(+, +, +),
		portray_bag(+, +),
	somebag(+, +),
	somechkbag(+, +),
	test_sub_bag(+, +),
	    test_sub_bag(+, +, +, +, +, +, +).


%@  
%@  Exported predicates:
%@  
%@  @table @code

%@  @item is_bag(@var{+Bag})
%@  @PLXindex {is_bag/1 (bags)}
%@  recognises proper well-formed bags.  You can pass variables to @code{is_bag/1},
%@  and it will reject them. @c just like is_list/1, is_ord_set/1, and so on.

is_bag(X) :- var(X), !, fail.		% catch and reject variables.
is_bag(bag).			% can't be a variable, clause 1 catches them.
is_bag(bag(E,M,B)) :-
	integer(M), M > 0,
	is_bag(B, E).


is_bag(X, _) :- var(X), !, fail.	% catch and reject variables.
is_bag(bag, _).			% can't be a variable, clause 1 catches them.
is_bag(bag(E,M,B), P) :-
	E @> P,
	integer(M), M > 0,
	is_bag(B, E).



%@  @item portray_bag(@var{+Bag})
%@  @PLXindex {portray_bag/1 (bags)}
%@  writes a bag to the current output stream in a pretty form so that
%@  you can easily see what it is.  Note that a bag written out this
%@  way can @emph{not} be read back in.  For that, use @code{write_canonical/1}.
%@  The point of this predicate is @c that you can add a directive
%@  @c      :- add_portray(portray_bag).
%@  to have bags displayed nicely by print/1 and the debugger.
%@  This will print things which are not fully instantiated, which is
%@  mainly of interest for debugging this module.

portray_bag(bag) :-
	write('Bag{'), write('}').
portray_bag(bag(E,M,B)) :-
	write('Bag{'), portray_bag(B, E, M), write('}').


portray_bag(B, E, M) :- var(B), !,
	portray_bag(E, M), write(' | '), write(B).
portray_bag(bag(F,N,B), E, M) :- !,
	portray_bag(E, M), write(', '),
	portray_bag(B, F, N).
portray_bag(bag, E, M) :- !,
	portray_bag(E, M).
portray_bag(B, E, M) :-
	portray_bag(E, M), write(' | '), write(B).


portray_bag(E, M) :-
	print(E), write(':'), write(M).



%   If bags are to be as useful as lists, we should provide mapping
%   predicates similar to those for lists.  Hence
%       checkbag(Pred, Bag)             - applies Pred(Element, Count)
%       mapbag(Pred, Bag)               - applies Pred(Element)
%       mapbag(Pred, BagIn, BagOut)     - applies Pred(Element, Answer)
%       somebag(Pred, Bag)              - applies Pred(Element, Count)
%       somechkBag(Pred, Bag)           - applies Pred(Element, Count)
%   Note that mapbag does NOT give the Count to Pred, but preserves it.
%   It wouldn't be hard to apply Pred to four arguments if it wants them.



%@  @item checkbag(@var{:Pred}, @var{+Bag})
%@  @PLXindex {checkbag/2 (bags)}
%@  is true when @var{Bag} is a @var{Bag@{E1:M1, ..., En:Mn@}} with elements @var{Ei}
%@  of multiplicity @var{Mi}, and @var{Pred(Ei, Mi)} is true for each @var{i}.

checkbag(Pred, Bag) :-
	check_bag(Bag, Pred).


check_bag(bag, _).
check_bag(bag(Element,Multiplicity,Bag), Pred) :-
	call(Pred, Element, Multiplicity),
	check_bag(Bag, Pred).



%@  @item mapbag(@var{:Pred}, @var{+Bag})
%@  @PLXindex {mapbag/2 (bags)}
%@  is true when @var{Bag} is a @var{Bag@{E1:M1, ..., En:Mn@}} with elements @var{Ei}
%@  of multiplicity @var{Mi}, and @var{Pred(Ei)} is true for each element @var{Ei}.
%@  The multiplicities are ignored:  if you don't want this, use @code{checkbag/2}.

mapbag(Pred, Bag) :-
	map_bag(Bag, Pred).


map_bag(bag, _).
map_bag(bag(Element,_,Bag), Pred) :-
	call(Pred, Element),
	map_bag(Bag, Pred).



%@  @item mapbag(@var{:Pred}, @var{+OldBag}, @var{-NewBag})
%@  @PLXindex {mapbag/3 (bags)}
%@  is true when @var{OldBag} is a @var{Bag@{E1:M1, ..., En:Mn@}} and @var{NewBag} is a
%@  @var{Bag@{F1:M'1, ..., Fn:M'n@}} and the elements of @var{OldBag} and @var{NewBag}
%@  are related by @var{Pred(Ei, Fj)}.  What happens is that the elements
%@  of @var{OldBag} are mapped, and then the result is converted to a bag,
%@  so there is no positional correspondence between @var{Ei} and @var{Fj}.
%@  Even when @var{Pred} is bidirectional, @code{mapbag/3} is @emph{not}.  @var{OldBag} should
%@  satisfy @code{is_bag/1} before @code{mapbag/3} is called.

mapbag(Pred, BagIn, BagOut) :-
	map_bag_list(BagIn, Listed, Pred),
	keysort(Listed, Sorted),
	bagform(Sorted, BagOut).


map_bag_list(bag, [], _).
map_bag_list(bag(Element,Multiplicity,Bag), [R-Multiplicity|L], Pred) :-
	call(Pred, Element, R),
	map_bag_list(Bag, L, Pred).



%@  @item somebag(@var{:Pred}, @var{+Bag})
%@  @PLXindex {somebag/2 (bags)}
%@  is true when @var{Bag} is a @var{Bag@{E1:M1, ..., En:Mn@}} with elements @var{Ei} of
%@  multiplicity @var{Mi} and @var{Pred(Ei, Mi)} is true of some element @var{Ei} and
%@  its multiplicity.  There is no version which ignores the @var{Mi}.
:- somebag(0,+) is nondet.
somebag(Pred, bag(Element,Multiplicity,_)) :-
	call(Pred, Element, Multiplicity).
somebag(Pred, bag(_,_,Bag)) :-
	somebag(Pred, Bag).



%@  @item somechkbag(@var{:Pred}, @var{+Bag})
%@  @PLXindex {somechkbag/2 (bags)}
%@  is like @code{somebag(@var{Pred}, @var{Bag})}, but commits to the first solution it
%@  finds.  For example, if @code{p(X,X,_)}, @code{somechk(p(X), @var{Bag})} would be an
%@  analogue of @code{memberchk/2} for bags.

somechkbag(Pred, bag(Element,Multiplicity,_)) :-
	call(Pred, Element, Multiplicity),
	!.
somechkbag(Pred, bag(_,_,Bag)) :-
	somechkbag(Pred, Bag).



%@  @item bag_to_list(@var{+Bag}, @var{-List})
%@  @PLXindex {bag_to_list/2 (bags)}
%@  converts a @var{Bag@{E1:M1, ..., En:Mn@}} to a list where each element
%@  appears as many times as its multiplicity requires.  For example,
%@  @code{Bag@{a:1, b:3, c:2@}} would be converted to @code{[a,b,b,b,c,c]}.

bag_to_list(bag, []).
bag_to_list(bag(Element,Multiplicity,Bag), List) :-
	Multiplicity > 0,		% should be redundant
	bag_to_list(Multiplicity, List, Element, Bag).


bag_to_list(N, [Element|List], Element, Bag) :-
	(   N =:= 1 ->
	    bag_to_list(Bag, List)
	;   M is N-1,			% M > 0
	    bag_to_list(M, List, Element, Bag)
	).



%@  @item bag_to_ord_set(@var{+Bag}, @var{-Ordset})
%@  @PLXindex {bag_to_ord_set/2 (bags)}
%@  converts a @var{Bag@{E1:M1, ..., En:Mn@}} to a list where each element
%@  appears once without its multiplicity.  The result is always an
%@  ordered (representation of a) set, suitable for processing by
%@  @code{library(ordsets)}.  See also @code{bag_to_list/2}.

bag_to_ord_set(bag, []).
bag_to_ord_set(bag(Element,_,Bag), [Element|Ordset]) :-
	bag_to_ord_set(Bag, Ordset).



%@  @item bag_to_ord_set(@var{+Bag}, @var{+Threshold}, @var{-Ordset})
%@  @PLXindex {bag_to_ord_set/3 (bags)}
%@  given a @var{Bag@{E1:M1, ..., En:Mn@}} returns a list in standard order of
%@  the set of elements @var{@{Ei | Mi >= Threshold@}}.  The result is an Ordset.

bag_to_ord_set(Bag, Threshold, Ordset) :-
	bag_to_set(Bag, Threshold, Ordset).



%@  @item list_to_bag(@var{+List}, @var{-Bag})
%@  @PLXindex {list_to_bag/2 (bags)}
%@  converts a proper list @var{List} to a @var{Bag} representing the same multi-set.
%@  Each element of the List appears once in the @var{Bag} together
%@  with the number of times it appears in the @var{List}.

list_to_bag(L, B) :-
	(   foreach(H,L),
	    foreach(H-1,K)
	do  true
	),
	keysort(K, S),
	bagform(S, B).


bagform([], bag).
bagform([Element-M|Tail], bag(Element,N,B)) :-
	bagform(Tail, Element, M, N, Rest),
	bagform(Rest, B).


bagform([Element-M|Tail], Element, N0, N, Rest) :- !,
	N1 is N0+M,
	bagform(Tail, Element, N1, N, Rest).
bagform(Rest, _, N, N, Rest).



%@  @item bag_to_set(@var{+Bag}, @var{-Set})
%@  @PLXindex {bag_to_set/2 (bags)}
%@  converts a @var{Bag@{E1:M1, ..., En:Mn@}} to a list which represents the
%@  @var{Set} @var{@{E1, ..., En@}}.  The order of elements in the result is not
%@  defined:  for a version where the order is defined use @code{bag_to_ord_set/2}.

bag_to_set(bag, []).
bag_to_set(bag(Element,_,Bag), [Element|Set]) :-
	bag_to_set(Bag, Set).



%@  @item bag_to_set(@var{+Bag}, @var{+Threshold}, @var{-Set})
%@  @PLXindex {bag_to_set/3 (bags)}
%@  given a @var{Bag@{E1:M1, ..., En:Mn@}} returns a list which represents the
%@  @var{Set} of elements @var{@{Ei | Mi >= Threshold@}}.  Because the @var{Bag} is sorted,
%@  the result is necessarily an ordered set.

bag_to_set(bag, _, []).
bag_to_set(bag(_,Multiplicity,Bag), Threshold, Set) :-
	Multiplicity < Threshold,
	!,
	bag_to_set(Bag, Threshold, Set).
bag_to_set(bag(Element,_,Bag), Threshold, [Element|Set]) :-
	bag_to_set(Bag, Threshold, Set).



%@  @item empty_bag(@var{?Bag})
%@  @PLXindex {empty_bag/1 (bags)}
%@  is true when @var{Bag} is the representation of an empty bag.  It can be
%@  used both to make and to recognise empty bags.

empty_bag(bag).



%@  @item member(@var{?Element}, @var{?Multiplicity}, @var{+Bag})
%@  @PLXindex {member/3 (bags)}
%@  is true when @var{Element} appears in the multi-set represented by @var{Bag}
%@  with the indicated @var{Multiplicity}.  @var{Bag} should be instantiated,
%@  but @var{Element} and @var{Multiplicity} may severally be given or solved for.
:- member(?,?,+) is nondet.
member(Element, Multiplicity, bag(Element,Multiplicity,_)).
member(Element, Multiplicity, bag(_,_,Bag)) :-
	member(Element, Multiplicity, Bag).



%@  @item memberchk(@var{+Element}, @var{?Multiplicity}, @var{+Bag})
%@  @PLXindex {memberchk/3 (bags)}
%@  is true when @var{Element} appears in the multi-set represented by @var{Bag},
%@  with the indicated @var{Multiplicity}.  It should only be used to check
%@  whether a given element occurs in the @var{Bag}, or whether there is an
%@  element with the given @var{Multiplicity}.  Note that guessing the
%@  multiplicity and getting it wrong may force the wrong choice of
%@  clause, but the result will be correct if @code{is_bag(@var{Bag})}.

memberchk(Element, Multiplicity, bag(Element,Multiplicity,_)) :- !.
memberchk(Element, Multiplicity, bag(_,_,Bag)) :-
	memberchk(Element, Multiplicity, Bag).



%@  @item bag_max(@var{+Bag}, @var{-CommonestElement})
%@  @PLXindex {bag_max/2 (bags)}
%@  unifies @var{CommonestElement} with the element of @var{Bag} which occurs
%@  most often, picking the leftmost element if several have this
%@  multiplicity.  To find the multiplicity as well, use @code{bag_max/3}.
%@  @code{bag_max/2} and @code{bag_min/2} break ties the same way.

bag_max(bag(E0,M0,B), E) :-
	bag_scan(B, E0, M0, 1, E).


%@  @item bag_min(@var{+Bag}, @var{-RarestElement})
%@  @PLXindex {bag_min/2 (bags)}
%@  unifies @var{RarestElement} with the element of @var{Bag} which occurs
%@  least often, picking the leftmost element if several have this
%@  multiplicity.  To find the multiplicity as well, use @code{bag_min/3}.
%@  @code{bag_max/2} and @code{bag_min/2} break ties the same way, so
%@  @example
%@      bag_max(Bag, Elt), bag_min(Bag, Elt)
%@  @end example
%@  is true only when all the elements of @var{Bag} have the same multiplicity.

bag_min(bag(E0,M0,B), E) :-
	bag_scan(B, E0, M0, -1, E).


%.  bag_scan(+Bag, +E0, +M0, +Sign, -E)
%   generalises bag_{max,min}/2.  They share a common routine, at a small
%   cost in efficiency, to make it impossible for them to break ties
%   differently.

bag_scan(bag, E, _, _, E).
bag_scan(bag(E1,M1,B), E0, M0, Sign, E) :-
	(   (M1-M0)*Sign > 0 ->
	    bag_scan(B, E1, M1, Sign, E)
	;/* (M1-M0)*Sign =< 0 */
	    bag_scan(B, E0, M0, Sign, E)
	).



%@  @item bag_max(@var{+Bag}, @var{-CommonestElement}, @var{-Multiplicity})
%@  @PLXindex {bag_max/3 (bags)}
%@  unifies @var{CommonestElement} with the element of @var{Bag} which occurs
%@  most often, and @var{Multiplicity} with the multiplicity of that element.
%@  If there are several elements with the same greatest multiplicity,
%@  the left-most is returned.  @code{bag_min/3} breaks ties the same way.

bag_max(bag(E0,M0,B), E, M) :-
	bag_scan(B, E0, M0, 1, E, M).


%@  @item bag_min(@var{+Bag}, @var{-RarestElement})
%@  @PLXindex {bag_min/2 (bags)}
%@  unifies @var{RarestElement} with the element of @var{Bag} which occurs
%@  least often, and @var{Multiplicity} with the multiplicity of that element.
%@  If there are several elements with the same least multiplicity,
%@  the left-most is returned.  @code{bag_max/3} breaks ties the same way, so
%@  @example
%@      bag_max(Bag, Elt, Mult), bag_min(Bag, Elt, Mult)
%@  @end example
%@  is true only when all the elements of @var{Bag} have multiplicity @var{Mult}.

bag_min(bag(E0,M0,B), E, M) :-
	bag_scan(B, E0, M0, -1, E, M).


%.  bag_scan(+Bag, +E0, +M0, +Sign, ?E, ?M)
%   generalises bag_{max,min}/3.  They share a common routine, at a small
%   cost in efficiency, to make it impossible for them to break ties
%   differently.

bag_scan(bag, E, M, _, E, M).
bag_scan(bag(E1,M1,B), E0, M0, Sign, E, M) :-
	(   (M1-M0)*Sign > 0 ->
	    bag_scan(B, E1, M1, Sign, E, M)
	;/* (M1-M0)*Sign =< 0 */
	    bag_scan(B, E0, M0, Sign, E, M)
	).



%@  @item length(@var{+Bag}, @var{-BagCardinality}, @var{-SetCardinality})
%@  @PLXindex {length/3 (bags)}
%@  unifies @var{BagCardinality} with the total cardinality of the multi-set
%@  @var{Bag} (the sum of the multiplicities of its elements) and
%@  @var{SetCardinality} with the number of distinct elements.
%@  @c length(B, BC, SC) & bag_to_list(B, L) & bag_to_set(B, S)
%@  @c --> length(L, BC) & length(S, SC).

length(B, BL, SL) :-
	length(B, 0, BL, 0, SL).

length(bag,	   BL, BL, SL, SL).
length(bag(_,M,B), BA, BL, SA, SL) :-
	BB is BA+M, SB is SA+1,
	length(B, BB, BL, SB, SL).


%  sub_bag, if it existed, could be used two ways: to test whether one bag
%  is a sub_bag of another, or to generate all the sub_bags.  The two uses
%  need different implementations.


%@  @item make_sub_bag(@var{+Bag}, @var{-SubBag})
%@  @PLXindex {make_sub_bag/2 (bags)}
%@  enumerates the sub-bags of @var{Bag}, unifying @var{SubBag} with each of them in
%@  turn.  The order in which the sub-bags are generated is such that if
%@  SB2 is a sub-bag of SB1 which is a sub-bag of Bag, SB1 is generated
%@  before SB2.  In particular, Bag is enumerated first and bag last.
:- make_sub_bag(+,-) is nondet.
make_sub_bag(bag, bag).
make_sub_bag(bag(Element,M,B), bag(Element,N,C)) :-
	countdown(M, N),
	make_sub_bag(B, C).
make_sub_bag(bag(_,_,B), C) :-
	make_sub_bag(B, C).

:- countdown(+,-) is nondet.
countdown(M, M).
countdown(M, N) :-
	M > 1, K is M-1,
	countdown(K, N).



%@  @item test_sub_bag(@var{+Bag}, @var{+SubBag})
%@  @PLXindex {test_sub_bag/2 (bags)}
%@  is true when @var{SubBag} is (already) a sub-bag of @var{Bag}.  That is,
%@  each element of SubBag must occur in @var{Bag} with at least the
%@  same multiplicity.  If you know @var{SubBag}, you should use this
%@  to test, not @code{make_sub_bag/2}.

test_sub_bag(bag, _).
test_sub_bag(bag(E1,M1,B1), bag(E2,M2,B2)) :-
	compare(C, E1, E2),
	test_sub_bag(C, E1, M1, B1, E2, M2, B2).

test_sub_bag(>, E1, M1, B1, _, _, bag(E2,M2,B2)) :-
	compare(C, E1, E2),
	test_sub_bag(C, E1, M1, B1, E2, M2, B2).
test_sub_bag(=, E1, M1, B1, E1, M2, B2) :-
	M1 =< M2,
	test_sub_bag(B1, B2).


%@  @item bag_union(@var{+Bag1}, @var{+Bag2}, @var{-Union})
%@  @PLXindex {bag_union/3 (bags)}
%@  unifies @var{Union} with the multi-set union of bags @var{Bag1} and @var{Bag2}.

bag_union(bag, Bag2, Bag2).
bag_union(bag(E1,M1,B1), Bag2, Union) :-
	bag_union(Bag2, E1, M1, B1, Union).

bag_union(bag, E1, M1, B1, bag(E1,M1,B1)).
bag_union(bag(E2,M2,B2), E1, M1, B1, Union) :-
	compare(Order, E1, E2),
	bag_union(Order, E1, M1, B1, E2, M2, B2, Union).

bag_union(<, E1, M1, B1, E2, M2, B2, bag(E1,M1,Union)) :-
	bag_union(B1, E2, M2, B2, Union).
bag_union(>, E1, M1, B1, E2, M2, B2, bag(E2,M2,Union)) :-
	bag_union(B2, E1, M1, B1, Union).
bag_union(=, E1, M1, B1, _,  M2, B2, bag(E1,M3,Union)) :-
	M3 is M1+M2,
	bag_union(B1, B2, Union).



%@  @item bag_union(@var{+ListOfBags}, @var{-Union})
%@  @PLXindex {bag_union/2 (bags)}
%@  is true when @var{ListOfBags} is given as a proper list of bags and @var{Union}
%@  is their multi-set union.  Letting @var{K} be the length of @var{ListOfBags},
%@  and @var{N} the sum of the sizes of its elements, the cost is
%@  @var{O(N lg K)}. 
%   The auxiliary routine
%   bag_union_3(N, L, U, R)
%   is true when the multi-set union of the first N sets in L is U and
%   R is the remaining elements of L.

bag_union(ListOfBags, Union) :-
	length(ListOfBags, NumberOfBags),
	bag_union_3(NumberOfBags, ListOfBags, Union, []).

bag_union_3(0, R0, bag, R) :- !, R = R0.
bag_union_3(1, [U|R0], U, R) :- !, R = R0.
bag_union_3(2, [A,B|R0], U, R) :- !, R = R0,
	bag_union(A, B, U).
bag_union_3(N, R0, U, R) :-
	P is N>>1,	% |first  half of list|
	Q is N- P,	% |second half of list|
	bag_union_3(P, R0, A, R1),
	bag_union_3(Q, R1, B, R),
	bag_union(A, B, U).



%@  @item bag_intersection(@var{+Bag1}, @var{+Bag2}, @var{-Intersection})
%@  @PLXindex {bag_intersection/2 (bags)}
%@  unifies @var{Intersection} with the multi-set intersection
%@  of bags @var{Bag1} and @var{Bag2}.

bag_intersection(bag, _, bag).
bag_intersection(bag(E1,M1,B1), Bag2, Intersection) :-
	bag_intersection(Bag2, E1, M1, B1, Intersection).

bag_intersection(bag, _, _, _, bag).
bag_intersection(bag(E2,M2,B2), E1, M1, B1, Intersection) :-
	compare(Order, E1, E2),
	bag_intersection(Order, E1, M1, B1, E2, M2, B2, Intersection).

bag_intersection(<, _,  _,  B1, E2, M2, B2, Intersection) :-
	bag_intersection(B1, E2, M2, B2, Intersection).
bag_intersection(>, E1, M1, B1, _,  _,  B2, Intersection) :-
	bag_intersection(B2, E1, M1, B1, Intersection).
bag_intersection(=, E1, M1, B1, _,  M2, B2, bag(E1,M3,Intersection)) :-
	( M1 < M2 -> M3 = M1 ; M3 = M2 ),	%  M3 is min(M1,M2)
	bag_intersection(B1, B2, Intersection).



%@  @item bag_intersection(@var{+ListOfBags}, @var{-Intersection})
%@  @PLXindex {bag_intersection/2 (bags)}
%@  is true when @var{ListOfBags} is given as a non-empty proper list of Bags
%@  and @var{Intersection} is their intersection.  The intersection of an
%@  empty list of Bags would be the universe with infinite multiplicities!

bag_intersection([Bag|Bags], Intersection) :-
	bag_intersection_3(Bags, Bag, Intersection).


bag_intersection_3([], Intersection, Intersection).
bag_intersection_3([Bag|Bags], Intersection0, Intersection) :-
	bag_intersection(Bag, Intersection0, Intersection1),  
	bag_intersection_3(Bags, Intersection1, Intersection).



%@  @item bag_intersect(@var{+Bag1}, @var{+Bag2})
%@  @PLXindex {bag_intersect/2 (bags)}
%@  is true when the multi-sets @var{Bag1} and @var{Bag2} have at least one
%@  element in common.

bag_intersect(bag(E1,M1,B1), bag(E2,M2,B2)) :-
	compare(C, E1, E2),
	bag_intersect(C, E1, M1, B1, E2, M2, B2).

bag_intersect(<, _, _, bag(E1,M1,B1), E2, M2, B2) :-
	compare(C, E1, E2),
	bag_intersect(C, E1, M1, B1, E2, M2, B2).
bag_intersect(>, E1, M1, B1, _, _, bag(E2,M2,B2)) :-
	compare(C, E1, E2),
	bag_intersect(C, E1, M1, B1, E2, M2, B2).
bag_intersect(=, _, M1, _, _, M2, _) :-
	M1+M2 >= 2.	% just a little safety check.



%@  @item bag_add_element(@var{+Bag1}, @var{+Element}, @var{+Multiplicity}, @var{-Bag2})
%@  @PLXindex {bag_add_element/4 (bags)}
%@  computes @var{Bag2 = Bag1 U @{Element:Multiplicity@}}.
%@  @var{Multiplicity} must be an integer.

bag_add_element(Bag1, E, M, Bag2) :-
	integer(M),
	(   M > 0 ->
	    bag_add_element_1(Bag1, E, M, Bag2)
	;   M < 0 -> 
	    N is -M,
	    bag_del_element_1(Bag1, E, N, Bag2)
	;/* M = 0 */
	    Bag2 = Bag1
	).


bag_add_element(Bag1, Element, Bag2) :-
	bag_add_element_1(Bag1, Element, 1, Bag2).


bag_add_element_1(bag, E, M, bag(E,M,bag)).
bag_add_element_1(bag(E1,M1,B1), E, M, Bag) :-
	compare(R, E1, E),
	bag_add_element_1(R, E, M, Bag, E1, M1, B1).


bag_add_element_1(<, E, M, bag(E1,M1,Bag2), E1, M1, B1) :-
	bag_add_element(B1, E, M, Bag2).
bag_add_element_1(=, E, M, bag(E,M2,B1), _, M1, B1) :-
	M2 is M1+M.
bag_add_element_1(>, E, M, bag(E,M,bag(E1,M1,B1)), E1, M1, B1).



%@  @item bag_del_element(@var{+Bag1}, @var{+Element}, @var{+Multiplicity}, @var{-Bag2})
%@  @PLXindex {bag_del_element/4 (bags)}
%@  computes @var{Bag2 = Bag1 \ @{Element:Multiplicity@}}.
%@  @var{Multiplicity} must be an integer.  
%@  @c It might be cleaner to have predicates
%@  @c like *del_element/ which would fail if the thing to be deleted did
%@  @c not occur in the collection.

bag_del_element(Bag1, E, M, Bag2) :-
	integer(M),
	(   M > 0 ->
	    bag_del_element_1(Bag1, E, M, Bag2)
	;   M < 0 -> 
	    N is -M,
	    bag_add_element_1(Bag1, E, N, Bag2)
	;/* M = 0 */
	    Bag2 = Bag1
	).


bag_del_element(Bag1, Element, Bag2) :-
	bag_del_element_1(Bag1, Element, 1, Bag2).


bag_del_element_1(bag, _, _, bag).
bag_del_element_1(bag(E1,M1,B1), E, M, Bag) :-
	compare(R, E1, E),
	bag_del_element_1(R, E, M, Bag, E1, M1, B1).


bag_del_element_1(<, E, M, bag(E1,M1,Bag2), E1, M1, B1) :-
	bag_del_element_1(B1, E, M, Bag2).
bag_del_element_1(=, E, M, Bag2, _, M1, B1) :-
	M2 is M1-M,
	(   M2 > 0 -> Bag2 = bag(E,M2,B1)
	;/* M2=< 0 */ Bag2 = B1
	).
bag_del_element_1(>, _, _, bag(E1,M1,B1), E1, M1, B1).



%@  @item bag_subtract(@var{+Bag1}, @var{+Bag2}, @var{-Difference})
%@  @PLXindex {bag_subtract/3 (bags)}
%@  is true when @var{Difference} is the multiset difference of @var{Bag1} and @var{Bag2}.

bag_subtract(bag, _, bag).
bag_subtract(bag(E1,M1,B1), Bag2, Difference) :-
	bag_subtract_1(Bag2, E1, M1, B1, Difference).

bag_subtract_1(bag, E1, M1, B1, bag(E1,M1,B1)).
bag_subtract_1(bag(E2,M2,B2), E1, M1, B1, Difference) :-
	compare(Order, E1, E2),
	bag_subtract(Order, E1, M1, B1, E2, M2, B2, Difference).

bag_subtract_2(bag, E2, M2, B2, bag(E2,M2,B2)).
bag_subtract_2(bag(E1,M1,B1), E2, M2, B2, Difference) :-
	compare(Order, E1, E2),
	bag_subtract(Order, E1, M1, B1, E2, M2, B2, Difference).

bag_subtract(<, E1, M1, B1, E2, M2, B2, bag(E1,M1,Difference)) :-
	bag_subtract_2(B1, E2, M2, B2, Difference).
bag_subtract(>, E1, M1, B1, _,  _,  B2, Difference) :-
	bag_subtract_1(B2, E1, M1, B1, Difference).
bag_subtract(=, E,  M1, B1, _,  M2, B2, Difference) :-
	(   M1 =< M2 ->		% do not include in result
	    bag_subtract(B1, B2, Difference)
	;   M is M1-M2,		% include with reduced multplicity
	    Difference = bag(E,M,B),
	    bag_subtract(B1, B2, B)
	).

%@  @end table
