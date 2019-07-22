%   Module : ordsets
%   Author : Richard A. O'Keefe
%   Updated: %G%
%   Purpose: Ordered set manipulation utilities

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

%@  This library module provides operations on sets represented as ordered lists with no
%@  duplicates.  Thus @code{@{c,r,a,f,t@}} would be @code{[a,c,f,r,t]}.  The ordering
%@  is defined by the @code{@@<} family of term comparison predicates, which
%@  is the ordering used by @code{sort/2} and @code{setof/3}.
%@  
%@  The benefit of the ordered representation is that the elementary
%@  set operations can be done in time proportional to the sum of the
%@  argument sizes rather than their product.  You should use the
%@  operations defined here in preference to those in @code{library(sets)}
%@  unless there is a compelling reason why you can't.  Some of the
%@  unordered set routines, such as @code{member/2}, @code{length/2} and @code{select/3} can
%@  be used unchanged on ordered sets; feel free so to use them.
%@  
%@  There is no @code{ordset_to_list/2}, as an ordered set is a list already.

:- module(ordsets, [
	is_ordset/1,		%  Set ->
	list_to_ord_set/2,	%  List -> Set
	ord_add_element/3,	%  Set x Elem -> Set
	ord_del_element/3,	%  Set x Elem -> Set
	ord_disjoint/2,		%  Set x Set ->
	ord_disjoint_union/3,	%  Set x Set -> Set
	ord_intersect/2,	%  Set x Set ->
	ord_intersection/3,	%  Set x Set -> Set
	ord_intersection/4,	%  Set x Set -> Set x Set
	ord_intersection/2,	%  list(Set) -> Set
	ord_member/2,		%  Elem x Set ->
	ord_nonmember/2,	%  Elem x Set ->
	ord_seteq/2,		%  Set x Set ->
	ord_setproduct/3,	%  Set x Set -> SetOfPairs
	ord_subset/2,		%  Set x Set ->
	ord_subtract/3,		%  Set x Set -> Set
	ord_symdiff/3,		%  Set x Set -> Set
	ord_union/3,		%  Set x Set -> Set
	ord_union/2,		%  list(Set) -> Set
	ord_union/4,		%  Set x Set -> Set x Set
	ordset_order/3		%  Term x Term -> Relation
   ]).


%@  Exported predicates:
%@  
%@  @table @code


%@  @item is_ordset(@var{+List})
%@  @PLXindex {is_ordset/1 (ordsets)}
%@  is true when @var{List} is a list of terms @var{[T1,T2,...,Tn]} and the
%@  terms are strictly increasing: @var{T1 @@< T2 @@< ... @@< Tn}.  The
%@  output of @code{sort/2} always satisfies this test.  Anything which
%@  satisfies this test can be given to the predicates in this
%@  file, regardless of where you got it.

is_ordset(X) :- var(X), !, fail.	% catch and reject variables.
is_ordset([]).
is_ordset([Head|Tail]) :-
	is_ordset(Tail, Head).

is_ordset(X, _) :- var(X), !, fail.	% catch and reject variables.
is_ordset([], _).
is_ordset([Head|Tail], Left) :-
	Head @> Left,
	is_ordset(Tail, Head).



%@  @item list_to_ord_set(@var{+List}, @var{-Set})
%@  @PLXindex {list_to_ord_set/2 (ordsets)}
%@  is true when @var{Set} is the ordered representation of the set represented
%@  by the unordered representation List.  The only reason for giving it
%@  a name at all is that you may not have realised that @code{sort/2} could be
%@  used this way.

list_to_ord_set(List, Set) :-
	sort(List, Set).



%@  @item ord_add_element(@var{+Set1}, @var{+Element}, @var{-Set2})
%@  @PLXindex {ord_add_element/3 (ordsets)}
%@  Equivalent to @code{ord_union(@var{Set1}, [@var{Element}], @var{Set2})}, but a bit
%@  faster.

ord_add_element([], Element, [Element]).
ord_add_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_add_element(Order, Head, Tail, Element, Set).


ord_add_element(<, Head, Tail, Element, [Head|Set]) :-
	ord_add_element(Tail, Element, Set).
ord_add_element(=, Head, Tail, _, [Head|Tail]).
ord_add_element(>, Head, Tail, Element, [Element,Head|Tail]).



%@  @item ord_del_element(@var{+Set1}, @var{+Element}, @var{-Set2})
%@  @PLXindex {ord_del_element/3 (ordsets)}
%@  Equivalent to @code{ord_subtract(@var{Set1}, [@var{Element}], @var{Set2})}, but a bit
%@  faster.
%  Because it uses
%  ordering, it typically builds less structure, but is slower than
%  del_element.  I am beginning to wonder whether a predicate
%	set_plus(SmallSet, Element, LargeSet)
%  would be a better way of doing this, the idea being that
%  LargeSet = SmallSet U {Element} and Element is not in SmallSet.
%  There is already a predicate with this effect called select/3.

ord_del_element([], _, []).
ord_del_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_del_element(Order, Element, Head, Tail, Set).

ord_del_element(<, Element, Head, Tail, [Head|Set]) :-
	ord_del_element(Tail, Element, Set).
ord_del_element(=, _, _, Set, Set).
ord_del_element(>, _, Head, Tail, [Head|Tail]).



%@  @item ord_disjoint(@var{+Set1}, @var{+Set2})
%@  @PLXindex {ord_disjoint/2 (ordsets)}
%@  is true when the two ordered sets have no element in common.
%   If the arguments are not ordered, I have no idea what happens.

ord_disjoint(Set1, Set2) :-
	\+ ord_intersect(Set1, Set2).



%@  @item ord_intersect(@var{+Set1}, @var{+Set2})
%@  @PLXindex {ord_intersect/2 (ordsets)}
%@  is true when the two ordered sets have at least one element in common.
%   Note that the test is == rather than = .
%   ord_intersect/2 has been unfolded in ord_intersect/5 to avoid
%   constructing anything.

ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).

ord_intersect(=, _, _, _, _).
ord_intersect(<, _, [Head1|Tail1], Head2, Tail2) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).
ord_intersect(>, Head1, Tail1, _, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).



%@  @item ord_intersection(@var{+Set1}, @var{+Set2}, @var{-Intersection})
%@  @PLXindex {ord_intersection/[2,3,4] (ordsets)}
%@  is true when @var{Intersection} is the ordered representation of @var{Set1}
%@  and @var{Set2}, provided that @var{Set1} and @var{Set2} are ordered sets.

ord_intersection([], _, []).
ord_intersection([Head1|Tail1], Set2, Intersection) :-
	ord_intersection_1(Set2, Head1, Tail1, Intersection).

ord_intersection_1([], _, _, []).
ord_intersection_1([Head2|Tail2], Head1, Tail1, Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection_1(Order, Head1, Tail1, Head2, Tail2, Intersection).

ord_intersection_1(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	ord_intersection(Tail1, Tail2, Intersection).
ord_intersection_1(<, _, Tail1, Head2, Tail2, Intersection) :-
	ord_intersection_1(Tail1, Head2, Tail2, Intersection).
ord_intersection_1(>, Head1, Tail1, _, Tail2, Intersection) :-
	ord_intersection_1(Tail2, Head1, Tail1, Intersection).



%@  @item ord_intersection(@var{+Set1}, @var{+Set2}, @var{?Intersection}, @var{?Difference})
%@  is true when @var{Intersection} is the intersection of @var{Set1} and @var{Set2}, 
%@  and @var{Difference} is @var{Set2 \ Set1} (like in ord_union/4),
%@  provided that @var{Set1} and @var{Set2} are ordered sets.

ord_intersection([], Set2, [], Set2).
ord_intersection([Head1|Tail1], Set2, Intersection, Difference) :-
	ord_intersection4(Set2, Head1, Tail1, Intersection, Difference).

ord_intersection4(<, _, Set1, Head2, Tail2, Intersection, Difference) :-
	(   Set1 = []
	->  Intersection = [], Difference = [Head2|Tail2]
	;   Set1 = [Head1|Tail1],
	    compare(Order, Head1, Head2),
	    ord_intersection4(Order, Head1, Tail1, Head2, Tail2, Intersection, Difference)
	).
ord_intersection4(=, Head, Tail1, _, Tail2, [Head|Intersection], Difference) :-
	ord_intersection(Tail1, Tail2, Intersection, Difference).
ord_intersection4(>, Head1, Tail1, Head2, Set2, Intersection, [Head2|Difference]) :-
	ord_intersection4(Set2, Head1, Tail1, Intersection, Difference).

ord_intersection4([], _, _, [], []).
ord_intersection4([Head2|Tail2], Head1, Tail1, Intersection, Difference) :-
	compare(Order, Head1, Head2),
	ord_intersection4(Order, Head1, Tail1, Head2, Tail2, Intersection, Difference).



%@  @item ord_intersection(@var{+ListOfSets}, @var{-Intersection})
%@  is true when @var{ListOfSets} is a nonempty proper list of ordered sets
%@  and @var{Intersection} is their intersection.  
%   Earlier versions of this
%   package used a "logarithmic" method for this, similar to the code
%   in ord_union/2.  However, experiments showed that intersection is
%   better done using a "linear" method (in the case of union, we are
%   faced with _growing_ intermediate results, but in intersection we
%   have a case of _shrinking_ intermediate results).  We cannot take
%   an empty ListOfSets because its intersection is the universe!

ord_intersection([Set|Sets], Intersection) :-
	ord_intersection_3(Sets, Set, Intersection).

ord_intersection_3([], Intersection, Intersection).
ord_intersection_3([Set|Sets], Intersection0, Intersection) :-
	ord_intersection(Set, Intersection0, Intersection1),
	ord_intersection_3(Sets, Intersection1, Intersection).


%@  @item ord_member(@var{+Elt}, @var{+Set})
%@  @PLXindex {ord_member/2 (ordsets)}
%@  is true when @var{Elt} is a member of @var{Set}.  Suggested by Mark Johnson.

ord_member(X, [E|Es]) :-
        compare(C, X, E),
        ord_member(C, X, Es).

ord_member(=, _X, _Es).
ord_member(>, X, [E|Es]) :-
        compare(C, X, E),
        ord_member(C, X, Es).


%@  @item ord_nonmember(@var{+Item}, @var{+Set})
%@  @PLXindex {ord_nonmember/2 (ordsets)}
%@  is true when the given @var{Item} is @emph{not} an element of the given @var{Set}.
%   Whether this is faster or slower than basics:nonmember/2 depends
%   on the version of Prolog you are using.  It expresses intent.

ord_nonmember(Item, Set) :-
	ord_nonmember_(Set, Item).

ord_nonmember_([], _).
ord_nonmember_([X|Xs], Item) :-
	compare(R, X, Item),
	ord_nonmember_(R, Item, Xs).

ord_nonmember_(>, _, _).
ord_nonmember_(<, Item, Xs) :-
	ord_nonmember_(Xs, Item).

%@  @item ord_seteq(@var{+Set1}, @var{+Set2})
%@  @PLXindex {ord_seteq/2 (ordsets)}
%@  is true when the two arguments represent the same set.  Since they
%@  are assumed to be ordered representations, they must be identical.

ord_seteq(Set1, Set2) :-
	Set1 == Set2.

%@  @item ord_setproduct(@var{+Set1}, @var{+Set2}, @var{-Product})
%@  @PLXindex {ord_setproduct/3 (ordsets)}
%   is in fact identical to setproduct(Set1, Set2, Product).
%@  If @var{Set1} and @var{Set2} are ordered sets, @var{Product} will be an ordered
%@  set of @var{x1-x2} pairs.  Note that we cannot solve for @var{Set1} and
%@  @var{Set2}, because there are infinitely many solutions when
%@  @var{Product} is empty, and may be a large number in other cases.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  ord_setproduct(Set1, Set2, Product) :-
%@  	(   foreach(H1,Set1),
%@  	    param(Set2),
%@  	    fromto(Product,P1,P3,[])
%@  	do  (   foreach(H2,Set2),
%@  		param(H1),
%@  		fromto(P1,[H1-H2|P2],P2,P3)
%@  	    do  true
%@  	    )
%@  	).
%@  @end group
%@  @end example

ord_setproduct(Set1, Set2, Product) :-
	(   foreach(H1,Set1),
	    param(Set2),
	    fromto(Product,P1,P3,[])
	do  (   foreach(H2,Set2),
		param(H1),
		fromto(P1,[H1-H2|P2],P2,P3)
	    do  true
	    )
	).



%@  @item ord_subset(@var{+Set1}, @var{+Set2})
%@  @PLXindex {ord_subset/2 (ordsets)}
%@  is true when every element of the ordered set @var{Set1} appears in the
%@  ordered set @var{Set2}.

ord_subset([], _).
ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Tail2).

ord_subset(=, _, Tail1, Tail2) :-
	ord_subset(Tail1, Tail2).
ord_subset(>, Head1, Tail1, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Tail2).



%@  @item ord_subtract(@var{+Set1}, @var{+Set2}, @var{-Difference})
%@  @PLXindex {ord_subtract/3 (ordsets)}
%@  is true when @var{Difference} contains all and only the elements of @var{Set1}
%@  which are not also in @var{Set2}.

ord_subtract([], _, []).
ord_subtract([Head1|Tail1], Set2, Difference) :-
	ord_subtract_1(Set2, Head1, Tail1, Difference).

ord_subtract_1([], Head1, Tail1, [Head1|Tail1]).
ord_subtract_1([Head2|Tail2], Head1, Tail1, Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_subtract(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	ord_subtract_2(Tail1, Head2, Tail2, Difference).
ord_subtract(>, Head1, Tail1, _,     Tail2, Difference) :-
	ord_subtract_1(Tail2, Head1, Tail1, Difference).
ord_subtract(=, _,     Tail1, _,     Tail2, Difference) :-
	ord_subtract(Tail1, Tail2, Difference).

ord_subtract_2([], _, _, []).
ord_subtract_2([Head1|Tail1], Head2, Tail2, Difference) :-
	compare(Order, Head1, Head2),
	ord_subtract(Order, Head1, Tail1, Head2, Tail2, Difference).



%@  @item ord_symdiff(@var{+Set1}, @var{+Set2}, @var{-Difference})
%@  @PLXindex {ord_symdiff/3 (ordsets)}
%@  is true when @var{Difference} is the symmetric difference of @var{Set1} and @var{Set2}.

ord_symdiff([], Set2, Set2).
ord_symdiff([Head1|Tail1], Set2, Difference) :-
	ord_symdiff(Set2, Head1, Tail1, Difference).

ord_symdiff([], Head1, Tail1, [Head1|Tail1]).
ord_symdiff([Head2|Tail2], Head1, Tail1, Difference) :-
	compare(Order, Head1, Head2),
	ord_symdiff(Order, Head1, Tail1, Head2, Tail2, Difference).

ord_symdiff(<, Head1, Tail1, Head2, Tail2, [Head1|Difference]) :-
	ord_symdiff(Tail1, Head2, Tail2, Difference).
ord_symdiff(>, Head1, Tail1, Head2, Tail2, [Head2|Difference]) :-
	ord_symdiff(Tail2, Head1, Tail1, Difference).
ord_symdiff(=, _,     Tail1, _,     Tail2, Difference) :-
	ord_symdiff(Tail1, Tail2, Difference).



%@  @item ord_disjoint_union(@var{+Set1}, @var{+Set2}, @var{-Union})
%@  @PLXindex {ord_disjoint_union/3 (ordsets)}
%@  is true when @var{Set1} and @var{Set2} (given to be ordered sets) have no element
%@  in common, and @var{Union} is their union.  The meaning is the same as
%@  @example
%@      ord_disjoint(Set1, Set2),
%@      ord_union(Set1, Set2, Union)
%@  @end example
%@  but it is more efficient.

ord_disjoint_union([], Set2, Set2).
ord_disjoint_union([Head1|Tail1], Set2, Union) :-
	ord_disjoint_union_1(Set2, Head1, Tail1, Union).

ord_disjoint_union_1([], Head1, Tail1, [Head1|Tail1]).
ord_disjoint_union_1([Head2|Tail2], Head1, Tail1, Union) :-
	compare(Order, Head1, Head2),
	ord_disjoint_union_1(Order, Head1, Tail1, Head2, Tail2, Union).

ord_disjoint_union_1(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	ord_disjoint_union_1(Tail1, Head2, Tail2, Union).
ord_disjoint_union_1(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	ord_disjoint_union_1(Tail2, Head1, Tail1, Union).



%@  @item ord_union(@var{+Set1}, @var{+Set2}, @var{-Union})
%@  @PLXindex {ord_union/[2,3,4] (ordsets)}
%@  is true when @var{Union} is the union of @var{Set1} and @var{Set2}.  Note that when
%@  something occurs in both sets, we want to retain only one copy.
%   The previous version had some cuts, and was 10-20% slower.

ord_union([], Set2, Set2).
ord_union([Head1|Tail1], Set2, Union) :-
	ord_union_1(Set2, Head1, Tail1, Union).

ord_union_1([], Head1, Tail1, [Head1|Tail1]).
ord_union_1([Head2|Tail2], Head1, Tail1, Union) :-
	compare(Order, Head1, Head2),
	ord_union_1(Order, Head1, Tail1, Head2, Tail2, Union).

ord_union_1(<, Head1, Tail1, Head2, Tail2, [Head1|Union]) :-
	ord_union_1(Tail1, Head2, Tail2, Union).
ord_union_1(>, Head1, Tail1, Head2, Tail2, [Head2|Union]) :-
	ord_union_1(Tail2, Head1, Tail1, Union).
ord_union_1(=, Head1, Tail1, _,     Tail2, [Head1|Union]) :-
	ord_union(Tail1, Tail2, Union).



%@  @item ord_union(@var{+OldSet}, @var{+NewSet}, @var{-Union}, @var{-ReallyNew})
%@  is true when @var{Union} is @var{NewSet U OldSet} and @var{ReallyNew} is @var{NewSet \ OldSet}.
%@  This is useful when you have an iterative problem, and you're adding
%@  some possibly new elements (@var{NewSet}) to a set (@var{OldSet}), and as well as
%@  getting the updated set (@var{Union}) you would like to know which if any of
%@  the "new" elements didn't already occur in the set (@var{ReallyNew}).

ord_union([], Set2, Set2, Set2).
ord_union([Head1|Tail1], Set2, Union, New) :-
	ord_union_1(Set2, Head1, Tail1, Union, New).

ord_union_1([], Head1, Tail1, [Head1|Tail1], []).
ord_union_1([Head2|Tail2], Head1, Tail1, Union, New) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union, New).

ord_union(<, Head1, Tail1, Head2, Tail2, [Head1|Union], New) :-
	ord_union_2(Tail1, Head2, Tail2, Union, New).
ord_union(>, Head1, Tail1, Head2, Tail2, [Head2|Union], [Head2|New]) :-
	ord_union_1(Tail2, Head1, Tail1, Union, New).
ord_union(=, Head1, Tail1, _,     Tail2, [Head1|Union], New) :-
	ord_union(Tail1, Tail2, Union, New).

ord_union_2([], Head2, Tail2, [Head2|Tail2], [Head2|Tail2]).
ord_union_2([Head1|Tail1], Head2, Tail2, Union, New) :-
	compare(Order, Head1, Head2),
	ord_union(Order, Head1, Tail1, Head2, Tail2, Union, New).



%@  @item ord_union(@var{+ListOfSets}, @var{-Union})
%@  is true when @var{ListOfSets} is given as a proper list of ordered sets
%@  and @var{Union} is their union.  Letting @var{K} be the length of @var{ListOfSets},
%@  and @var{N} the sum of the sizes of its elements, the cost is
%@  @var{O(N lg K)}.  
%   The auxiliary routine
%   ord_union(N, L, U, R)
%   is true when the union of the first N sets in L is U and
%   R is the remaining elements of L.

ord_union(ListOfSets, Union) :-
	length(ListOfSets, NumberOfSets),
	ord_union_3(NumberOfSets, ListOfSets, Union, []).

ord_union_3(0, R0, [], R) :- !, R = R0.
ord_union_3(1, [U|R0], U, R) :- !, R = R0.
ord_union_3(2, [A,B|R0], U, R) :- !, R = R0,
	ord_union(A, B, U).
ord_union_3(N, R0, U, R) :-
	P is N>>1,	% |first  half of list|
	Q is N- P,	% |second half of list|
	ord_union_3(P, R0, A, R1),
	ord_union_3(Q, R1, B, R),
	ord_union(A, B, U).

/*  You will notice that ord_union/2 and ord_intersection/2 have
    the same structure.  This is no accident.  Suppose we want to
    compute <F>([X1,...,Xn]) = <G>(X1) <H> ... <H> <G>(Xn),
    where <G> is any function, and <H> is associative with left
    identity <E> (it does not need to be commutative or to have
    a right identity) we can perform the calculation in at least
    the following ways:

    <F>_lin(List, Answer) :-
	<F>_lin(List, <E>, Answer).

    <F>_lin([], Answer, Answer).
    <F>_lin([X|Xs], SoFar, Answer) :-
	<G>(X, Xg),
	<H>(SoFar, Xg, Next),
	<F>_lin(Xs, Next, Answer).


    <F>_log(List, Answer) :-
	length(List, Length),
	<F>_log(Length, List, Answer, []

    <F>_log(0, R, <E>, R) :- !.
    <F>_log(1, [X|R], Answer, R) :- !,
	<G>(X Answer).
    <F>_log(2, [X1,X2|R], Answer, R) :- !,
	<G>(X1, G1),
	<G>(X2, G2),
	<H>(G1, G2, Answer).
    <F>_log(N, R0, Answer, R) :-
	P is N>>1,	% |first  half of list|
	Q is N- P,	% |second half of list|
	<F>_log(P, R0, A, R1),
	<F>_log(Q, R1, B, R),
	<H>(A, B, Answer).

    You will note that the third clause of <F>_log/4 is actually
    redundant.  If you partially evaluate the fourth clause with
    N=2 that is the answer you get.  The optimisation is usually
    a good idea.  You will further note that the first clause of
    <F>_log/4 is only used when <F>_log([], X) is called, so it
    could be moved up into that procedure.

    The <F>_lin schema is easy to grasp.  However, the <F>_log
    schema can be considerably more efficient.  Consider sorting.
    Here <E> = [], <G>(X,[X]), and <H>(X,Y,Z) = merge(X,Y,Z).
    Merging a single element into a list of length N costs O(N)
    time, so <sort>_lin would take O(N**2) time to sort a list
    of length N, whereas <sort>_log would take O(N.lgN) time.
    Hence the names.  <F>_log is a real win when the cost of the
    <H> operation is non-trivial and increases with bigger
    operands.  Numerical analysts know that <sum>_log is often
    the best way to sum a list of floating point numbers, that is,
    that it gives a more accurate answer (because there are only
    O(lgN) roundoffs in it, as opposed to O(N) in <sum>_lin), but
    that is a rather marginal case.  sumlist/3 in library(lists)
    is <sum>_lin because it was originally intended for integers.
*/


%@  @item ordset_order(@var{+Xs}, @var{+Ys}, @var{-R})
%@  @PLXindex {ordset_order/3 (ordsets)}
%@  is true when @var{R} is @code{<}, @code{=}, or @code{>} according as @var{Xs} is a subset of @var{Ys},
%@  equal to @var{Ys}, or a superset of @var{Ys}.  @var{Xs} and @var{Ys} are ordered sets.

ordset_order(Xs, Ys, R) :-
	ordset_order(Xs, Ys, 0b111, R).

ordset_order([], Ys, Mask0, Mask) :-
	ordset_order1(Ys, Mask0, Mask).
ordset_order([X|Xs], Ys, Mask0, Mask) :-
	ordset_order1(Ys, X, Mask0, Mask, Xs).

ordset_order1([], Mask, R) :-
	(   Mask /\ 0b010 =\= 0 -> R = (=)
	;   Mask /\ 0b100 =\= 0 -> R = (<)
	;   Mask /\ 0b001 =\= 0 -> R = (>)
	).
ordset_order1([_|_], Mask, <) :-
	Mask /\ 0b100 =\= 0.

ordset_order1([], _, Mask, >, _) :-
	Mask /\ 0b001 =\= 0.
ordset_order1([Y|Ys], X, Mask, RR, Xs) :-
	compare(R, X, Y),
	ordset_order12(R, Ys, Mask, RR, Xs, X, Y).

ordset_order12(=, Ys, Mask, R, Xs, _, _) :-
	ordset_order(Xs, Ys, Mask, R).
ordset_order12(<, Ys, Mask0, R, Xs, _, Y) :-
	Mask1 is Mask0 /\ 0b001,
	ordset_order2(Xs, Y, Mask1, R, Ys).
ordset_order12(>, Ys, Mask0, R, Xs, X, _) :-
	Mask1 is Mask0 /\ 0b100,
	ordset_order1(Ys, X, Mask1, R, Xs).

ordset_order2([], _, Mask, <, _) :-
	Mask /\ 0b100 =\= 0.
ordset_order2([X|Xs], Y, Mask, RR, Ys) :-
	compare(R, X, Y),
	ordset_order12(R, Ys, Mask, RR, Xs, X, Y).

%@  @end table
