%   Module : sets
%   Authors: Lawrence Byrd + Richard A. O'Keefe
%   Updated: 19 Jan 1994
%   Purpose: Set manipulation utilities
%   SeeAlso: ordsets

%   Adapted from shared code written by the same authors; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(sets, [
	add_element/3,		%  Elem x Set -> Set
	del_element/3,		%  Elem x Set -> Set
	is_set/1,		%  List ->
	disjoint/2,		%  Set x Set ->
	disjoint_union/3,	%  Set x Set -> Set
	intersect/2,		%  Set x Set ->
	intersection/2,		%  list(Set) -> Set
	intersection/3,		%  Set x Set -> Set
	list_to_set/2,		%  List -> Set
	pairfrom/4,		%  Set -> Elem x Elem x Set
	power_set/2,		%  Set -> Set of Sets
	set_order/3,		%  Set x Set -> Relation
	seteq/2,		%  Set x Set ->
	setproduct/3,		%  Set x Set -> Pairs
	subset/2,		%  Set x Set ->
	subtract/3,		%  Set x Set -> Set
	symdiff/3,		%  Set x Set -> Set
	union/2,		%  list(Set) -> Set
	union/3,		%  Set x Set -> Set
	union/4			%  Set x Set -> Set x Set
   ]).
:- use_module(library(lists), [
	select/3,		%  Elem <- Set -> Set
	selectchk/3		%  Elem x Set -> Set
	]).


/*
%@  This library module provides operations on sets represented as unordered lists
%@  with no repeated elements.
%@  The ordered representation used in @code{library(ordsets)} is much more
%@  efficient, but these routines were designed before sort/2
%@  entered the language.

    Before the introduction of modules, it used to be the case
    that if you loaded library(sets), you got library(basics) too.
    Now if you want {member,memberchk,nonmember}/2, you have to
    ask for library(basics) yourself.  Eheu.  We could export
    them from sets, but then you'd have trouble if you did load
    library(basics).  Eheu again.

    The predicates in this file are defined only when their arguments
    are of the correct types (as shown in the :- pred declaration) and
    in the right instantiation state.  Typically the last argument will
    be unified with a function result and can be in any state, but all
    the other arguments must be proper lists and "sufficiently"
    instantiated for memberchk/2 to be sound.  Many of the predicates
    do odd things if these requirements are violated:
	disjoint(X, Y) fails
	union([], 2, X) succeeds with X = 2
	add_element(3, X, Y) binds X = Y = [3|_]
    These oddities are not errors:  they are just "undefined".  The
    basic problem is that we have to know when something is NOT in a
    list, and for that to work things have to be ground.

    Using proper_list/1 and must_be_proper_list/3 from library(types),
    we could easily make this module check that arguments have the
    right form, but we currently don't do that.
*/

:- mode
	add_element(+, +, ?),
	del_element(+, +, ?),
	disjoint(+, +),
	disjoint_union(+, +, ?),
	is_set(+),
	pairfrom(?, ?, ?, ?),
	intersect(+, +),
	subset(+, +),
	seteq(+, +),
	list_to_set(+, ?),
	power_set(?, ?),
	    ps(+, ?),
		ps(+, +, +, ?),
	intersection(+, +, ?),
	intersection(+, ?),
	    intersection1(+, +, ?),
	setproduct(?, ?, ?),
	subtract(+, +, ?),
	symdiff(+, +, ?),
	    symdiff(+, +, ?, ?),
	union(+, +, ?),
	union(+, ?),
	union(+, _, ?, ?).

/* pred
	add_element(T, list(T), list(T)),
	del_element(T, list(T), list(T)),
	disjoint(list(T), list(T)),
	disjoint_union(list(T), list(T), list(T)),
	is_set(list(T)),
	select(T, list(T), list(T)),
	selectchk(T, list(T), list(T)),
	pairfrom(list(T), T, T, list(T)),
	intersect(list(T), list(T)),
	subset(list(T), list(T)),
	seteq(list(T), list(T)),
	list_to_set(list(T), list(T)),
	power_set(list(T), list(list(T))),
	    ps(list(T), list(list(T))),
		ps(list(list(T)), T, list(list(T)), list(list(T))),
	intersect(list(T), list(T), list(T)),
	intersection(list(T), list(T), list(T)),
	intersection(list(list(T)), list(T)),
	    intersection1(list(T), list(list(T)), list(T)),
	subtract(list(T), list(T), list(T)),
	symdiff(list(T), list(T), list(T)),
	    symdiff(list(T), list(T), list(T), list(T)),
	setproduct(list(T), list(U), list(pair(T,U))),
	    setproduct(list(U), T, list(pair(T,U))),
	union(list(list(T)), list(T)),
	    union1(list(list(T)), list(T)),
		union2(list(T), list(T)),
	union(list(T), list(T), list(T)),
	union(list(T), list(T), list(T), list(T)).
*/


%@  Exported predicates:
%@  
%@  @table @code

%@  @item add_element(@var{+Element}, @var{+Set1}, @var{-Set2})
%@  @PLXindex {add_element/3 (sets)}
%@  is true when @var{Set1} and @var{Set2} are sets represented as unordered lists,
%@  and @var{Set2 = Set1 U @{Element@}}.  It may only be used to calculate @var{Set2}
%@  given @var{Element} and @var{Set1}.  
%   However, if @var{Set1} is a partial list, there
%   is an unpleasant hack using add_element(Element, Set1, Set1) which
%   adds new Elements at the end of Set1.

add_element(Element, Set1, Set2) :-
	memberchk(Element, Set1),
	!,
	Set2 = Set1.
add_element(Element, Set1, [Element|Set1]).



%@  @item del_element(@var{+Element}, @var{+Set1}, @var{-Set2})
%@  @PLXindex {del_element/3 (sets)}
%@  is true when @var{Set1} and @var{Set2} are sets represented as unordered lists,
%@  and @var{Set2 = Set1 \ @{Element@}}.  It may only be used to calculate @var{Set2}
%@  given @var{Element} and @var{Set1}.  If @var{Set1} does not contain @var{Element}, @var{Set2} will
%@  be identical to @var{Set1} (the old version made a new copy of @var{Set1}).  If
%@  @var{Set1} is not an unordered set, but contains more than one copy of
%@  @var{Element}, only the first will be removed.  If you want to delete all
%@  copies of a given element, use @code{lists:delete/3}.  For a
%@  version which fails if @var{Element} is not in @var{Set1}, use @code{selectchk/3}.

del_element(Element, Set1, Set2) :-
	selectchk(Element, Set1, Result),
	!,
	Set2 = Result.
del_element(_, Set1, Set1).



%@  @item disjoint(@var{+Set1}, @var{+Set2})
%@  @PLXindex {disjoint/2 (sets)}
%@  is true when the two given sets have no elements in common.
%@  It is the opposite of @code{intersect/2}.  If either of the arguments
%@  is improper, @code{disjoint/2} will fail.  
%   Note that the success of
%   @code{disjoint/2} does not entail @var{Set1} and @var{Set2} being lists: the goal
%   disjoint(1,2) succeeds.  disjoint/2 is only defined for lists.

disjoint(Set1, Set2) :-
	member(Element, Set1),
	memberchk(Element, Set2),
	!, fail.
disjoint(_, _).



%@  @item is_set(@var{+List})
%@  @PLXindex {is_set/1 (sets)}
%@  is true when @var{List} is a proper list that contains no repeated elements.
%   That, is, List represents a set in the style used by this package.
%   See the description of nonmember/2 for some restrictions.  The way we
%   test for List being proper is rather curious: if it ends with a
%   variable the call to nonmember/2 must fail, which is the reason for
%   the odd clause order and the cut in the first clause.

is_set([Head|Tail]) :- !,
	nonmember(Head, Tail),
	is_set(Tail).
is_set([]).



%@  @item pairfrom(@var{?Set}, @var{?Element1}, @var{?Element2}, @var{?Residue})
%@  @PLXindex {pairfrom/4 (sets)}
%@  is true when @var{Set} is a list, @var{Element1} occurs in list, @var{Element2}
%@  occurs in list after @var{Element1}, and @var{Residue} is everything in @var{Set}
%@  bar the two @var{Elements}.  The point of this thing is to select
%@  pairs of elements from a set without selecting the same pair
%@  twice in different orders.
%   This can be used to select two elements from a given @var{Set} or to
%   insert two elements into a given @var{Residue}, but if neither @var{Set}
%   nor @var{Residue} is proper you're in trouble.  You could diagonalise
%   by doing Set = [_,_|L], same_length(L, Residue), and then
%   calling pairfrom.  We could do that here, but for the two uses
%   that are intended it is not necessary.
:- pairfrom(?,?,?,?) is nondet.
pairfrom([Element1|Set], Element1, Element2, Residue) :-
	select(Element2, Set, Residue).
pairfrom([Head|Tail], Element1, Element2, [Head|Rest]) :-
	pairfrom(Tail, Element1, Element2, Rest).



%@  @item intersect(@var{+Set1}, @var{+Set2})
%@  @PLXindex {intersect/2 (sets)}
%@  is true when the two sets have a member in common.  It assumes
%@  that both sets are known, and that you don't care which element
%@  it is that they share.  
%   If either argument is partial, intersect/2
%   will succeed: this isn't always right.  You should ensure that the
%   arguments are proper lists.

intersect(Set1, Set2) :-
	member(Element, Set1),		%  generates Elements from Set1
	memberchk(Element, Set2),	%  tests them against Set2
	!.				%  if it succeeds once, is enough.



%@  @item subset(@var{+Set1}, @var{+Set2})
%@  @PLXindex {subset/2 (sets)}
%@  is true when each member of @var{Set1} occurs in @var{Set2}.
%@  It can only be used to test two given sets; it cannot be used
%@  to generate subsets.  
%@  There is no predicate for generating
%@  subsets as such, but the predicates @code{subseq/3}, @code{subseq0/2}, @code{subseq1/2}
%@  in @code{library(lists)} may do what you want (they preserve the order
%@  of elements within a list).
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  subset(Set1, Set2) :-
%@  	(   foreach(X,Set1),
%@  	    param(Set2)
%@  	do  memberchk(X,Set2)
%@  	).
%@  @end group
%@  @end example

subset(Set1, Set2) :-
	(   foreach(X,Set1),
	    param(Set2)
	do  memberchk(X,Set2)
	).


%@  @item set_order(@var{+Xs}, @var{+Ys}, @var{-R})
%@  @PLXindex {set_order/3 (sets)}
%@  is true when @var{R} is @code{<}, @code{=}, or @code{>} according as @var{Xs} is a subset of @var{Ys},
%@  equivalent to @var{Ys}, or a superset of @var{Ys}.

set_order(Xs, Ys, R) :-
	(   member(X, Xs), \+ member(X, Ys) ->
	    /* Xs has a member not in Ys, so "=" and "<" are ruled out */
	    Mask1 = 0b001
	;   /* Xs has no elements not in Ys, so ">" is ruled out */
	    Mask1 = 0b110
	),
	(   member(Y, Ys), \+ member(Y, Xs) ->
	    /* Ys has a member not in Xs, so "=" and ">" are ruled out */
	    Mask2 is Mask1 /\ 0b100
	;   /* Ys has no elements not in Xs, so "<" is ruled out */
	    Mask2 is Mask1 /\ 0b011
	),
	(   Mask2 /\ 0b100 =\= 0 -> R = (<)
	;   Mask2 /\ 0b001 =\= 0 -> R = (>)
	;   Mask2 /\ 0b010 =\= 0 -> R = (=)
	).


%@  @item seteq(@var{+Set1}, @var{+Set2})
%@  @PLXindex {seteq/2 (sets)}
%@  is true when each Set is a subset of the other.  
%   There are two
%   ways of doing this.  The fast one is commented out.

seteq(Set1, Set2) :-
	subset(Set1, Set2),
	subset(Set2, Set1).
%	sort(Set1, Standard),
%	sort(Set2, Standard).



%@  @item list_to_set(@var{+List}, @var{-Set})
%@  @PLXindex {list_to_set/2 (sets)}
%@  is true when @var{List} and @var{Set} are lists, and @var{Set} has the same elements
%@  as @var{List} in the same order, except that it contains no duplicates.
%@  The two are thus equal considered as sets.  
%   If you really want to
%   convert a list to a set, list_to_ord_set is faster, but this way
%   preserves as much of the original ordering as possible.
%   If List contains several copies of an element X, only the LAST
%   copy of X is retained.  If you want to convert a List to a Set,
%   retaining the FIRST copy of repeated elements, call
%	symdiff([], List, Set)

list_to_set([], []).
list_to_set([Head|Tail], Set) :-
	memberchk(Head, Tail),
	!,
	list_to_set(Tail, Set).
list_to_set([Head|Tail], [Head|Set]) :-
	list_to_set(Tail, Set).



%@  @item power_set(@var{+Set}, @var{-PowerSet})
%@  @PLXindex {power_set/2 (sets)}
%@  is true when @var{Set} is a list and @var{PowerSet} is a list of lists which
%@  represents the power set of the set that Set represents.
%   The particular representation of the power set chosen has this defining
%   property: if A subset-of B subset-of Set, then B appears *BEFORE*
%   A in PowerSet.  In particular, the first element of PowerSet must
%   be Set itself, and the last element of PowerSet must be [].  As an
%   example, power_set([a,b], X) binds X=[[a,b],[a],[b],[]].
%   Note that length(PowerSet) = 2**length(Set), so that for Sets with
%   more than about 18 elements, this isn't a very practical operation.

power_set(Set, [Set|Rest]) :-
	ps(Set, [Set|Rest]).


ps([], [[]]).
ps([Head|Tail], ListPow) :-
	ps(Tail, TailPow),
	ps(TailPow, Head, TailPow, ListPow).


ps([], _, ListPow, ListPow).
ps([Subset|Subsets], Element, TailPow, [[Element|Subset]|ListPow]) :-
	ps(Subsets, Element, TailPow, ListPow).



%@  @item intersection(@var{+Set1}, @var{+Set2}, @var{-Intersection})
%@  @PLXindex {intersection/[2,3] (sets)}
%@  is true when all three arguments are lists representing sets,
%@  and @var{Intersection} contains every element of @var{Set1} which is also
%@  an element of @var{Set2}, the order of elements in @var{Intersection}
%@  being the same as in @var{Set1}.  That is, @var{Intersection} represents
%@  the intersection of the sets represented by @var{Set1} and @var{Set2}.
%   If Set2 is a partial list, Intersection will be empty, which
%   is not, of course, correct.  If Set1 is a partial list, this
%   predicate will run away on backtracking.  Set1 and Set2 should
%   both be proper lists, but this is not checked.  Duplicates in
%   Set1 may survive in Intersection.  It is worthy of note that
%   if Set1 is an ordset, Intersection is an ordset, despite Set2.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  intersection(Set1, Set2, Intersection) :-
%@  	(   foreach(X,Set1),
%@  	    fromto(Intersection,S0,S,[]),
%@  	    param(Set2)
%@  	do  (member(X, Set2) -> S0 = [X|S] ; S0 = S)
%@  	).
%@  @end group
%@  @end example

intersection(Set1, Set2, Intersection) :-
	(   foreach(X,Set1),
	    fromto(Intersection,S0,S,[]),
	    param(Set2)
	do  (member(X, Set2) -> S0 = [X|S] ; S0 = S)
	).

%@  @item intersection(@var{+ListOfSets}, @var{-Intersection})
%@  is true when @var{Intersection} is the intersection of all the sets in
%@  @var{ListOfSets}.  The order of elements in @var{Intersection} is taken from
%@  the first set in @var{ListOfSets}.  This has been turned inside out to
%@  minimise the storage turnover.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  intersection([Set1|Sets], Intersection) :-
%@  	(   foreach(X,Set1),
%@  	    fromto(Intersection,S0,S,[]),
%@  	    param(Sets)
%@  	do  (   (   foreach(Set,Sets),
%@  		    param(X)
%@  		do  memberchk(X, Set)
%@  		) -> S0 = [X|S]
%@  	    ;   S0 = S
%@  	    )
%@  	).
%@  @end group
%@  @end example

intersection([Set|Sets], Intersection) :-
	intersection1(Set, Sets, Intersection).

intersection1(Set1, Sets, Intersection) :-
	(   foreach(X,Set1),
	    fromto(Intersection,S0,S,[]),
	    param(Sets)
	do  (   (   foreach(Set,Sets),
		    param(X)
		do  memberchk(X, Set)
		) -> S0 = [X|S]
	    ;   S0 = S
	    )
	).

%@  @item subtract(@var{+Set1}, @var{+Set2}, @var{-Difference})
%@  @PLXindex {subtract/3 (sets)}
%@  is like @code{intersect/3}, but this time it is the elements of @var{Set1} which
%@  @emph{are} in @var{Set2} that are deleted.  Note that duplicated @var{Elements} of
%@  @var{Set1} which are not in @var{Set2} are retained in @var{Difference}.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  subtract(Set1, Set2, Difference) :-
%@  	(   foreach(X,Set1),
%@  	    fromto(Difference,S0,S,[]),
%@  	    param(Set2)
%@  	do  (member(X, Set2) -> S0 = S ; S0 = [X|S])
%@  	).
%@  @end group
%@  @end example

subtract(Set1, Set2, Difference) :-
	(   foreach(X,Set1),
	    fromto(Difference,S0,S,[]),
	    param(Set2)
	do  (member(X, Set2) -> S0 = S ; S0 = [X|S])
	).

%@  @item symdiff(+@var{Set1}, @var{+Set2}, @var{-Difference})
%@  @PLXindex {symdiff/3 (sets)}
%@  is true when @var{Difference} is the symmetric difference of @var{Set1} and @var{Set2},
%@  that is, if each element of @var{Difference} occurs in one of @var{Set1} and @var{Set2} but
%@  not both.  The construction method is such that the answer will have
%@  no duplicates even if the @var{Sets} do.

symdiff(Set1, Set2, Difference) :-
	symdiff(Set1, Set2, Difference, Difference1),
	symdiff(Set2, Set1, Difference1, []).

symdiff([], _, Diff, Diff).
symdiff([Elem|Rest], Avoid, Diff0, Diff) :-
	memberchk(Elem, Avoid), !,
	symdiff(Rest, Avoid, Diff0, Diff).
symdiff([Elem|Rest], Avoid, [Elem|Diff1], Diff) :-
	symdiff(Rest, [Elem|Avoid], Diff1, Diff).



%@  @item setproduct(@var{+Set1}, @var{+Set2}, @var{-CartesianProduct})
%@  @PLXindex {setproduct/3 (sets)}
%@  is true when @var{Set1} is a set (list) and @var{Set2} is a set (list) and
%@  @var{CartesianProduct} is a set of @var{Elt1-Elt2} pairs, with a pair for
%@  for each element @var{Elt1} of @var{Set1} and @var{Elt2} of @var{Set2}.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  setproduct(Set1, Set2, Product) :-
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

setproduct(Set1, Set2, Product) :-
	(   foreach(H1,Set1),
	    param(Set2),
	    fromto(Product,P1,P3,[])
	do  (   foreach(H2,Set2),
		param(H1),
		fromto(P1,[H1-H2|P2],P2,P3)
	    do  true
	    )
	).

%@  @item disjoint_union(@var{+Set1}, @var{+Set2}, @var{-Union})
%@  @PLXindex {disjoint_union/3 (sets)}
%@  is true when @code{disjoint(Set1, Set2)} and @code{union(Set1, Set2, Union)},
%@  that is, @var{Set1} and @var{Set2} have no element in command and @var{Union} is
%@  their union.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  disjoint_union(Set1, Set2, Union) :-
%@  	(   foreach(X,Set1),
%@  	    fromto(Union,[X|S],S,Set2),
%@  	    param(Set2)
%@  	do  nonmember(X, Set2)
%@  	).
%@  @end group
%@  @end example

disjoint_union(Set1, Set2, Union) :-
	(   foreach(X,Set1),
	    fromto(Union,[X|S],S,Set2),
	    param(Set2)
	do  nonmember(X, Set2)
	).

%@  @item union(@var{+Set1}, @var{+Set2}, @var{-Union})
%@  @PLXindex {union/[2,3,4] (sets)}
%@  is true when @code{subtract(Set1,Set2,Diff)} and @code{append(Diff,Set2,Union)},
%@  that is, when @var{Union} is the elements of @var{Set1} that do not occur in
%@  @var{Set2}, followed by all the elements of @var{Set2}.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  union(Set1, Set2, Union) :-
%@  	(   foreach(X,Set1),
%@  	    fromto(Union,S0,S,Set2),
%@  	    param(Set2)
%@  	do  (member(X, Set2) -> S0 = S ; S0 = [X|S])
%@  	).
%@  @end group
%@  @end example

union(Set1, Set2, Union) :-
	(   foreach(X,Set1),
	    fromto(Union,S0,S,Set2),
	    param(Set2)
	do  (member(X, Set2) -> S0 = S ; S0 = [X|S])
	).

%@  @item union(@var{+Set1}, @var{+Set2}, @var{-Union}, @var{-Difference})
%@  is true when @code{union(Set1, Set2, Union)} and @code{subtract(Set1, Set2, Difference)}.
%   This was added to keep sets.pl and ordsets.pl parallel.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  union(Set1, Set2, Union, Difference) :-
%@  	(   foreach(X,Set1),
%@  	    fromto(Union,S0,S,Set2),
%@  	    fromto(Difference,T0,T,[]),
%@  	    param(Set2)
%@  	do  (   member(X, Set2) -> S0 = S, T0 = T
%@  	    ;   S0 = [X|S], T0 = [X|T]
%@  	    )
%@  	).
%@  @end group
%@  @end example

union(Set1, Set2, Union, Difference) :-
	(   foreach(X,Set1),
	    fromto(Union,S0,S,Set2),
	    fromto(Difference,T0,T,[]),
	    param(Set2)
	do  (   member(X, Set2) -> S0 = S, T0 = T
	    ;   S0 = [X|S], T0 = [X|T]
	    )
	).

%@  @item union(@var{+ListOfSets}, @var{-Union})
%@  is true when @var{Union} is the union of all the sets in @var{ListOfSets}.
%@  It has been arranged with storage turnover in mind.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  union(Sets, Union) :-
%@  	(   foreach(Set,Sets),
%@  	    param(Answer)
%@  	do  (   foreach(X,Set),
%@  		param(Answer)
%@  	    do  memberchk(X, Answer)
%@  	    )
%@  	),
%@  	append(Answer, [], Answer),	% cauterise it
%@  	!,
%@  	Union = Answer.
%@  @end group
%@  @end example

union(Sets, Union) :-
	(   foreach(Set,Sets),
	    param(Answer)
	do  (   foreach(X,Set),
		param(Answer)
	    do  memberchk(X, Answer)
	    )
	),
	append(Answer, [], Answer),	% cauterise it
	!,
	Union = Answer.

%@  @end table

