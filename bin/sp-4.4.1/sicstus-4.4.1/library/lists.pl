%   Adapted from shared code written by the same authors; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(lists, [
	% library(sets), should have been library(basic)
	select/3,		%  Elem <- Set -> Set
	selectchk/3,		%  Elem x Set -> Set
	% library(lists)
	append/2,			%   ListOfLists -> List
	append/5,			%   List x List x List x List x List
	correspond/4,			%   Elem <- List x List -> Elem
	delete/3,			%   List x Elem -> List
	delete/4,			%   List x Elem x Count -> List
	is_list/1,			%   List ->
	keys_and_values/3,		%   KeyValList -> KeyList x ValList
	last/2,				%   List -> Elem
	nextto/3,			%   Elem, Elem <- List
	nth0/3,				%   Integer x List -> Elem
	nth0/4,				%   Integer x List -> Elem x List
	nth1/3,				%   Integer x List -> Elem
	nth1/4,				%   Integer x List -> Elem x List
	one_longer/2,			%   List x List ->
	perm/2,				%   List -> List
	perm2/4,			%   Elem x Elem -> Elem x Elem
	permutation/2,			%   List <-> List
	proper_length/2,		%   List -> Length
	remove_dups/2,			%   List -> Set
	rev/2,				%   List -> List
	reverse/2,			%   List -> List
	same_length/2,			%   List x List ->
	same_length/3,			%   List x List x Integer ->
	select/4,			%   Elem x List x Elem -> List
	selectchk/4,			%   Elem x List x Elem -> List
	shorter_list/2,			%   List x List ->
	subseq/3,			%   List -> List x List
	subseq0/2,			%   List -> List
	subseq1/2,			%   List -> List
	sumlist/2,			%   List -> Integer
	transpose/2,			%   ListOfLists <-> ListOfLists
	% library(length)
	append_length/3,			% List x List x Length
	append_length/4,			% List x List x List x Length
	prefix_length/3,			% Whole x Part x Length
	proper_prefix_length/3,			% Whole x Part x Length
	proper_suffix_length/3,			% Whole x Part x Length
	rotate_list/2,				% List x List
	rotate_list/3,				% Integer -> (List x List)
	suffix_length/3,			% Whole x Part x Length
	sublist/3,				% Whole x Part x Length
	sublist/4,				% Whole x Part x Length^2
	sublist/5,				% Whole x Part x Length^3
	% library(list_parts)
	cons/3,			last/3,
	head/2,			tail/2,
	prefix/2,		proper_prefix/2,
	suffix/2,		proper_suffix/2,
	segment/2,		proper_segment/2,
	% library(maplist)
	cumlist/4,
	cumlist/5,
	cumlist/6,
	maplist/2,
	maplist/3,
	maplist/4,
	map_product/4,
	scanlist/4,
	scanlist/5,
	scanlist/6,
	some/2,
	some/3,
	some/4,
	somechk/2,
	somechk/3,
	somechk/4,
	% library(more_maps)
	convlist/3,
	exclude/3,
	exclude/4,
	exclude/5,
	include/3,
	include/4,
	include/5,
	partition/5,
	group/3,
	group/4,
	group/5,
	% library(ordered)
	ordered/1,
	ordered/2,
	max_member/2,
	min_member/2,
	max_member/3,
	min_member/3,
	select_max/3,
	select_max/4,
	select_min/3,
	select_min/4,
	% library(ordprefix)
	decreasing_prefix/3,
	decreasing_prefix/4,
	increasing_prefix/3,
	increasing_prefix/4,
	% library(clump)
	clumped/2,
	clumps/2,
	keyclumped/2,
	keyclumps/2
   ]).
:- meta_predicate
	cumlist(3, +, ?, ?),
	cumlist(4, ?, ?, ?, ?),
	cumlist(5, ?, ?, ?, ?, ?),
	maplist(1, ?),
	maplist(2, ?, ?),
	maplist(3, ?, ?, ?),
	map_product(3, +, +, ?),
	scanlist(3, +, ?, ?),
	scanlist(4, ?, ?, ?, ?),
	scanlist(5, ?, ?, ?, ?, ?),
	some(1, ?),
	some(2, ?, ?),
	some(3, ?, ?, ?),
	somechk(1, +),
	somechk(2, +, +),
	somechk(3, +, +, +),
	convlist(2, +, ?),
	exclude(1, +, ?),
	exclude(2, +, +, ?),
	exclude(3, +, +, +, ?),
	include(1, +, ?),
	include(2, +, +, ?),
	include(3, +, +, +, ?),
	partition(2, +, ?, ?, ?),
	    partition_1(+, 2, ?, ?, ?),
	        partition_1(+, +, ?, ?, ?, +, 2),
	group(2, +, ?),
	    group1(+, ?, 2),
	group(1, +, ?, ?),
	group(2, +, +, ?, ?),
	ordered(2, +),
	    ordered_0(+, 2),
	    ordered_1(+, +, 2),
	max_member(2, +, ?),
	min_member(2, +, ?),
	select_min(2, ?, +, ?),
	    sel_min_gen(+, ?, 2, ?),
		sel_min_gen(+, ?, +, 2, -, ?),
	select_max(2, ?, +, ?),
	    sel_max_gen(+, ?, 2, ?),
		sel_max_gen(+, ?, +, 2, -, ?),
	decreasing_prefix(2, +, ?, ?),
	first_precedes(2, +, +),
	increasing_prefix(2, +, ?, ?),
	precedes_first(2, +, +).

:- use_module(library(types), [
	must_be/4
   ]).

%@  This library module provides operations on lists.
%@  Exported predicates:
%@  
%@  @table @code
%@  @item select(@var{?Element}, @var{?Set}, @var{?Residue})
%@  @PLXindex {select/3 (lists)}
%@  is true when @var{Set} is a list, @var{Element} occurs in @var{Set}, and @var{Residue} is
%@  everything in @var{Set} except @var{Element} (things stay in the same order).
:- select(?,?,?) is nondet.
select(X, [X|R],     R        ).
select(X, [A,X|R],   [A|R]    ).
select(X, [A,B,X|R], [A,B|R]  ).
select(X, [A,B,C|L], [A,B,C|R]) :-
	select(X, L, R).

/*  The original code was
	select(X, [X|R], R).
	select(X, [H|T], [H|R]) :- select(X, T, R).
    This has been unrolled to save 10-20% of the time.
    It would be nice if select/3 were in library(basics), but we're
    stuck with it now.  Ah, hindsight.
*/



%@  @item selectchk(@var{+Element}, @var{+Set}, @var{?Residue})
%@  @PLXindex {selectchk/3 (lists)}
%@  is to @code{select/3} what @code{memberchk/2} is to @code{member/2}.  That is, it locates
%@  the first occurrence of @var{Element} in @var{Set}, and deletes it, giving @var{Residue}.
%@  It is steadfast in @var{Residue}.

selectchk(X, [X|R],     Residue) :- !, Residue = R.
selectchk(X, [A,X|R],   Residue) :- !, Residue = [A|R].
selectchk(X, [A,B,X|R], Residue) :- !, Residue = [A,B|R].
selectchk(X, [A,B,C|L], [A,B,C|R]) :-
	selectchk(X, L, R).

/*  The original code, had it existed, would have been
	selectchk(X, [X|T], R) :- !, R = T.
	selectchk(X, [H|T], [H|R]) :- selectchk(X, T, R).
    This has been unrolled, in order to make del_element/3 faster.
*/



%   Module : lists
%   Authors: Bob Welham, Lawrence Byrd, and Richard A. O'Keefe
%   Updated: %G%
%   Defines: list processing utilities
%   SeeAlso: library(flatten)


/*  Several of the comments below indicate that a particular predicate
    only works when a particular argument "is a proper list".  In
    general, an instance of a recursively defined data type "foo" is
    said to be "a proper foo" when it is a non-variable and all its
    "foo" components are proper foos.  For example, consider
	:- type tree(K,V) = {} | node(K,V,tree(K,V),tree(K,V)).
    X is said to be "a proper tree" if X is {} or if X is node(_,_,L,R)
    where both L and R are proper trees.  Similarly, X is a proper list
    if and only if X is [] or X is [_|L] where L is a proper list.
    The point is that a recursive procedure working its way down a
    proper whatever will not be creating new structure.  The predicate
    is_list/1 recognises proper lists.

    In general, the predicates in this file are only defined when the
    arguments have types compatible with the :- pred declaration below
    instantiated compatibly with the :- mode declaration below.  Their
    effect on other inputs is not defined.  For example,
	append([], 1, X, 2, Y)
    succeeds, binding X = 1 and Y = 2.  This is rather odd, but since
    the query is ill-typed the behavior is not an error.
*/
:- mode
	append(+, ?),
	append(?, ?, ?, ?, ?),
	correspond(?, +, +, ?),
	delete(+, +, -),
	delete(+, +, +, -),
	    delete_1(+, +, +, -),
	% is_list(?),
	is_list(+),
	keys_and_values(?, ?, ?),
	last(?, ?),
	    last_1(?, ?, ?),
	nextto(?, ?, ?),
	nth0(+, +, ?),
	nth0(+, ?, ?, ?),
	nth1(+, +, ?),
	nth1(+, ?, ?, ?),
	one_longer(?, ?),
	perm(+, ?),
	    insert(+, +, ?),
	permutation(?, ?),
	    permutation(?, ?, ?),
	perm2(?,?, ?,?),
	proper_length(+, ?),
	    % proper_length(+, +, -),
	remove_dups(+, ?),
	rev(?, ?),
	reverse(?, ?),
	% reverse(?, +, ?),
	same_length(?, ?),
	same_length(?, ?, ?),
	    'same length'(+, ?, ?),
	    'same length'(?, ?, +, -),
	select(?, ?, ?, ?),
	selectchk(+, +, ?, ?),
	shorter_list(?, +),
	subseq(?, ?, ?),
	subseq0(+, ?),
	subseq1(+, ?),
	sumlist(+, ?).
	% sumlist(+, +, ?).

/* pred
	append(list(list(T)), list(T)),
	append(list(T), list(T), list(T), list(T), list(T)),
	correspond(T, list(T), list(U), U),
	delete(list(T), T, list(T)),
	delete(list(T), T, integer, list(T)),
	    delete_1(list(T), T, integer, list(T)),
	is_list(T),
	keys_and_values(list(pair(T,U)), list(T), list(U)),
	last(list(T), T),
	    last_1(list(T), T, T),
	nextto(T, T, list(T)),
	nth0(integer, list(T), T),
	nth1(integer, list(T), T),
	    nth0v(list(T), T, integer, integer),
	    nth0i(integer, list(T), T),
	nth0(integer, list(T), T, list(T)),
	nth1(integer, list(T), T, list(T)),
	    nth0i(integer, list(T), T, list(T)),
	    nth0v(list(T), T, integer, integer, list(T)),
	one_longer(list(T), list(T)),
	perm(list(T), list(T)),
	    insert(list(T), T, list(T)),
	permutation(list(T), list(T)),
	    permutation(list(T), list(T), list(T)),
	perm2(T, T, T, T),
	proper_length(list(T), integer),
	    proper_length(list(T), integer, integer),
	remove_dups(list(T), list(T)),
	rev(list(T), list(T)),
	    rev(list(T), list(T), list(T)),
	reverse(list(T), list(T)),
	    reverse(list(T), list(T), list(T), list(T)),
	same_length(list(T), list(T)),
	same_length(list(T), list(T), integer),
	    'same length'(integer, list(T), list(T)),
	    'same length'(list(T), list(T), integer, integer),
	select(T, list(T), T, list(T)),
	selectchk(T, list(T), T, list(T)),
	shorter_list(list(T), list(T)),
	subseq(list(T), list(T), list(T)),
	subseq0(list(T), list(T)),
	subseq1(list(T), list(T)),
	sumlist(list(integer), integer),
	    sumlist(list(integer), integer, integer),
	transpose(list(list(T)), list(list(T))),
	    transpose(list(list(T)), list(list(T)), list(list(T))),
		transpose_1(list(U), list(U)),
		transpose_1(list(list(T)), list(T), list(list(T))).
*/


%@  @item append(@var{+ListOfLists}, @var{-List})
%@  @PLXindex {append/[2,5] (lists)}
%@  is true when @var{ListOfLists} is a list @var{[L1,...,Ln]} of lists, @var{List} is
%@  a list, and appending @var{L1}, ..., @var{Ln} together yields @var{List}.
%@  @var{ListOfLists} @emph{must} be a proper list.  
%@  Additionally, either @var{List} should be a proper list, or
%@  each of @var{L1}, ..., @var{Ln} should be a proper list.  The behavior on
%@  non-lists is undefined.  @var{ListOfLists} must be proper because for
%@  any given solution, infinitely many more can be obtained by
%@  inserting nils ([]) into @var{ListOfList}.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  append(Lists, Appended) :-
%@  	(   foreach(List,Lists),
%@  	    fromto(Appended,S0,S,[])
%@  	do  append(List, S, S0)
%@  	).
%@  @end group
%@  @end example

append(Lists, Appended) :-
	(   foreach(List,Lists),
	    fromto(Appended,S0,S,[])
	do  append(List, S, S0)
	).

%@  @item append(@var{?Prefix}, @var{?Tail1}, @var{?List1}, @var{?Tail2}, @var{?List2})
%@  is true when @code{append(@var{Prefix}, @var{Tail1}, @var{List1})} and @code{append(@var{Prefix}, @var{Tail2}, @var{List2})}
%@  are both true.  You could call @code{append/3} twice, but that is order-
%@  dependent.  This will terminate if @var{Prefix} is a proper list or if
%@  either @var{List1} or @var{List2} is a proper list.

append([], List1, List1, List2, List2).
append([H|T], Tail1, [H|List1], Tail2, [H|List2]) :-
	append(T, Tail1, List1, Tail2, List2).



%@  @item correspond(@var{?X}, @var{?Xlist}, @var{?Ylist}, @var{?Y})
%@  @PLXindex {correspond/4 (lists)}
%@  is true when @var{Xlist} and @var{Ylist} are lists, @var{X} is an element of @var{Xlist}, @var{Y} is
%@  an element of @var{Ylist}, and @var{X} and @var{Y} are in similar places in their lists.
%@  No relation is implied between other elements of @var{Xlist} and @var{Ylist}.
%@  For a similar predicate without the cut, see @code{select/4}.

correspond(X, [X|_], [Y|_], Y) :- !.
correspond(X, [_|T], [_|U], Y) :-
	correspond(X, T, U, Y).



%@  @item delete(@var{+List}, @var{+Kill}, @var{-Residue})
%@  @PLXindex {delete/[3,4] (lists)}
%@  is true when @var{List} is a list, in which @var{Kill} may or may not occur, and
%@  @var{Residue} is a copy of @var{List} with all elements equal to @var{Kill} deleted.
%@  To extract a single copy of @var{Kill}, use @code{select(@var{Kill}, @var{List}, @var{Residue})}.
%@  If @var{List} is not proper, @code{delete/3} will @emph{fail}.  @var{Kill} and the elements of
%@  @var{List} should be sufficiently instantiated for @code{\=} to be sound.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  delete(List, Kill, Residue) :-
%@  	(   foreach(X,List),
%@  	    fromto(Residue,S0,S,[]),
%@  	    param(Kill)
%@  	do  (X = Kill -> S0 = S ; S0 = [X|S])
%@  	).
%@  @end group
%@  @end example

delete(List, Kill, Residue) :-
	(   foreach(X,List),
	    fromto(Residue,S0,S,[]),
	    param(Kill)
	do  (X = Kill -> S0 = S ; S0 = [X|S])
	).


%@  @item delete(@var{+List}, @var{+Kill}, @var{+Count}, @var{-Residue})
%@  is true when @var{List} is a list, in which @var{Kill} may or may not occur,
%@  and @var{Count} is a non-negative integer, and @var{Residue} is a copy of
%@  @var{List} with the first @var{Count} elements equal to @var{Kill} deleted.  If
%@  @var{List} has fewer than @var{Count} elements equal to @var{Count}, all of them
%@  are deleted.
%@  If @var{List} is not proper, @code{delete/4} may @emph{fail}.  @var{Kill} and the elements of
%@  @var{List} should be sufficiently instantiated for @code{\=} to be sound.

delete(List, Kill, N, Residue) :-
	(   integer(N), nonvar(Kill) ->
	    (   N > 0 ->
		delete_1(List, Kill, N, Residue)
	    ;   N =:= 0 ->
		Residue = List
	    )
	;   Goal = delete(List,Kill,N,Residue),
	    must_be(N, integer, Goal,   3),
	    must_be(Kill, nonvar, Goal, 2)
	).

delete_1(X, _, _, _) :- var(X), !, fail.	% reject partial lists
delete_1([], _, _, []).
delete_1([Kill|Tail], Kill, N, Residue) :- !,
	M is N-1,
	(   M > 0 -> delete_1(Tail, Kill, M, Residue)
	;/* M = 0 */ Residue = Tail
	).
delete_1([Head|Tail], Kill, N, [Head|Residue]) :-
    %	Head \= Kill,
	delete_1(Tail, Kill, N, Residue).


:- if(true).

%@  @item is_list(@var{+List})
%@  @PLXindex {is_list/1 (lists)}
%@  succeeds when @var{List} is a proper list.  That is, @var{List} is nil ([]) or
%@  a cons cell (@var{[Head|Tail]}) whose @var{Tail} is a proper list.
%@  A variable, or a list whose final tail is a variable, or a cyclic list, will fail this
%@  test.
is_list(X) :-
        % [PM] 4.3 Handles cycles, and likely faster.
        prolog:proper_list(X).

:- else. % pre 4.3
is_list(X) :- var(X), !, fail.		% catch & reject variables
is_list([]).
is_list([_|Tail]) :-
	is_list(Tail).
:- endif. % pre 4.3


%@  @item keys_and_values(@var{?[K1-V1,...,Kn-Vn]}, @var{?[K1,...,Kn]}, @var{?[V1,...,Vn]})
%@  @PLXindex {keys_and_values/3 (lists)}
%@  is true when its arguments look like the picture above.  It is meant
%@  for splitting a list of @var{Key-Value} pairs (such as @code{keysort/2} wants and
%@  produces) into separate lists of @var{Keys} and of @var{Values}.  It may just as
%@  well be used for building a list of pairs from a pair of lists.   In
%@  fact one usually wants just the keys or just the values, but you can
%@  supply @code{_} as the other argument.   For example, suppose you wanted to
%@  sort a list without having duplicates removed.  You could do
%@  @example
%@  @group
%@      keys_and_values(RawPairs, RawKeys, _),
%@      keysort(RawPairs, OrdPairs),
%@      keys_and_values(OrdPairs, OrdKeys, _).
%@  @end group
%@  @end example
%   (In fact this operation is msort/2 and should be available somewhere.)
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  keys_and_values([], [], []).
%@  keys_and_values([Key-Value|Pairs], [Key|Keys], [Value|Values]) :-
%@  	keys_and_values(Pairs, Keys, Values).
%@  @end group
%@  @end example

keys_and_values([], [], []).
keys_and_values([Key-Value|Pairs], [Key|Keys], [Value|Values]) :-
	keys_and_values(Pairs, Keys, Values).



%@  @item last(@var{+List}, @var{-Last})
%@  @PLXindex {last/2 (lists)}
%@  is true when @var{List} is a @var{List} and @var{Last} is its last element.
%@  There is also a @code{last(@var{?Fore}, @var{?Last}, @var{?List})}
%@  whose argument order matches append/3.
%@  This could be defined as 
%@  @example
%@  @group
%@      last(L, X) :- append(_, [X], L).
%@  @end group
%@  @end example

last([Head|Tail], Last) :-
	last_1(Tail, Head, Last).		

last_1([], Last, Last).
last_1([Head|Tail], _, Last) :-
	last_1(Tail, Head, Last).



%@  @item nextto(@var{?X}, @var{?Y}, @var{?List})
%@  @PLXindex {nextto/3 (lists)}
%@  is true when @var{X} and @var{Y} appear side-by-side in @var{List}.
%@  It could be written as
%@  @example
%@  @group
%@      nextto(X, Y, List) :- append(_, [X,Y|_], List).
%@  @end group
%@  @end example
%@  It may be used to enumerate successive pairs from the list.
%@  @var{List} should be proper, otherwise @code{nextto/3} will generate it.
:- nextto(?,?,?) is nondet.
nextto(X,Y, [X,Y|_]).
nextto(X,Y, [_|List]) :-
	nextto(X,Y, List).



%@  @item nth0(@var{?N}, @var{?List}, @var{?Elem})
%@  @PLXindex {nth0/[3,4] (lists)}
%@  is true when @var{Elem} is the @var{N}th member of @var{List}, counting the first as
%@  element 0.  That is, throw away the first @var{N} elements and unify @var{Elem}
%@  with the next.  E.g. @code{nth0(0, [H|T], H)}.
%@  Either @var{N} should be an integer, or @var{List} should be proper.

nth0(Index, List, Element) :-
	(   integer(Index) ->		% are we finding Element?
	    Index >= 0,
	    nth0i(Index, List, Element)
	;   var(Index) ->		% or are we finding Index?
	    nth0v(List, Element, 0, Index)
	;   must_be(Index, integer, nth0(Index,List,Element), 1)
	).


%   nth0v: find the Index of an Element in the given List.
%   The Element might occur more than once, find each place.
:- nth0v/4 is nondet.
nth0v([Element|_], Element, Index, Index).
nth0v([_|Tail], Element, M, Index) :-
	N is M+1,
	nth0v(Tail, Element, N, Index).


%   nth0i: find an Element in the given List at a known Index >= 0.

nth0i(N, [Head|Tail], Elem) :-
	(   N =:= 0 -> Elem = Head
	;   M is N-1,			% should be succ(M, N)
	    nth0i(M, Tail, Elem)
	).


%@  @item nth1(@var{?N}, @var{?List}, @var{?Element})
%@  @PLXindex {nth1/[3,4] (lists)}
%@  is true when @var{Elem} is the @var{Nth} member of @var{List}, counting the first as
%@  element 1.  That is, throw away the first @var{N-1} elements and unify @var{Elem}
%@  with the next element (the @var{Nth}).  E.g. @code{nth1(1, [H|T], H)}.
%@  This is just like @code{nth0/3} except that it counts from 1 instead of 0.
%@  Either @var{N} should be an integer, or @var{List} should be proper.

nth1(Index, List, Element) :-
	(   integer(Index) ->		% are we finding Element?
	    Index >= 1,
	    N is Index-1,
	    nth0i(N, List, Element)
	;   var(Index) ->		% or are we finding Index?
	    nth0v(List, Element, 1, Index)
	;   must_be(Index, integer, nth1(Index,List,Element), 1)
	).



%@  @item nth0(@var{?N}, @var{?List}, @var{?Elem}, @var{?Rest})
%@  unifies @var{Elem} with the @var{Nth} element of @var{List}, counting from 0, and @var{Rest}
%@  with the other elements.  It can be used to select the @var{Nth} element
%@  of @var{List} (yielding @var{Elem} and @var{Rest}), or to insert @var{Elem} @emph{before} the @var{Nth}
%@  (counting from 0) element of @var{Rest}, when it yields @var{List}, e.g.
%@  @code{nth0(2, List, c, [a,b,d,e])} unifies @var{List} with @code{[a,b,c,d,e]}.
%@  This can be seen as inserting @var{Elem} @emph{after} the @var{Nth} element of @var{Rest}
%@  if you count from 1 rather than 0.
%@  Either @var{N} should be an integer, or @var{List} or @var{Rest} should be proper.

nth0(Index, List, Elem, Rest) :-
	(   integer(Index) ->		% are we finding Elem?
	    Index >= 0,
	    nth0i(Index, List, Elem, Rest)
	;   var(Index) ->		% or are we finding Index?
	    one_longer(List, Rest),
	    nth0v(List, Elem, 0, Index, Rest)
	;   must_be(Index, integer, nth0(Index,List,Elem,Rest), 1)
	).


nth0i(N, [Head|Tail], Elem, Rest) :-
	(   N =:= 0 ->
	    Elem = Head, Rest = Tail
	;   M is N-1,			% succ(M, N); should fail if N < 1
	    Rest = [Head|More],
	    nth0i(M, Tail, Elem, More)
	).

:- nth0v/5 is nondet.
nth0v([Head|Tail], Head, Index, Index, Tail).
nth0v([Head|Tail], Elem, M, Index, [Head|Rest]) :-
	N is M+1,
	nth0v(Tail, Elem, N, Index, Rest).


%@  @item nth1(@var{?N}, @var{?List}, @var{?Elem}, @var{?Rest})
%@  unifies @var{Elem} with the @var{Nth} element of @var{List}, counting from 1, and @var{Rest}
%@  with the other elements.  It can be used to select the @var{Nth} element
%@  of @var{List} (yielding @var{Elem} and @var{Rest}), or to insert @var{Elem} @emph{before} the @var{Nth}
%@  (counting from 1) element of @var{Rest}, when it yields @var{List}, e.g.
%@  @code{nth1(2, List, b, [a,c,d,e])} unifies @var{List} with @code{[a,b,c,d,e]}.
%@  Either @var{N} should be an integer, or @var{List} or @var{Rest} should be proper.

nth1(Index, List, Elem, Rest) :-
	(   integer(Index) ->		% are we finding Elem?
	    Index >= 1,
	    N is Index-1,
	    nth0i(N, List, Elem, Rest)
	;   var(Index) ->		% or are we finding Index?
	    one_longer(List, Rest),
	    nth0v(List, Elem, 1, Index, Rest)
	;   must_be(Index, integer, nth1(Index,List,Elem,Rest), 1)
	).



%@  @item one_longer(@var{?Longer}, @var{?Shorter})
%@  @PLXindex {one_longer/2 (lists)}
%@  is true when 
%@  @example
%@  @group
%@      length(Longer,N), length(Shorter,M), succ(M,N)
%@  @end group
%@  @end example
%@  for some integers @var{M}, @var{N}.  It was
%@  written to make @code{@{nth0,nth1@}/4} able to find the index, just as
%@  @code{same_length/2} is useful for making things invertible.

one_longer([_|RestLonger], Shorter) :-
	same_length(RestLonger, Shorter).



%@  @item perm(@var{+List}, @var{?Perm})
%@  @PLXindex {perm/2 (lists)}
%@  is true when @var{List} and @var{Perm} are permutations of each other.  The main
%@  use of @code{perm/2} is to generate permutations.  You should not use this
%@  predicate in new programs; use @code{permutation/2} instead.  @var{List} must be
%@  a proper list.  @var{Perm} may be partly instantiated.

perm([], []).
perm([X|Xs], Ys1) :-
	perm(Xs, Ys),
	insert(Ys, X, Ys1).

:- insert/3 is nondet.
insert(L, X, [X|L]).
insert([H|T], X, [H|L]) :-
	insert(T, X, L).



%@  @item permutation(@var{?List}, @var{?Perm})
%@  @PLXindex {permutation/2 (lists)}
%@  is true when @var{List} and @var{Perm} are permutations of each other.
%@  Unlike @code{perm/2}, it will work even when @var{List} is not a proper list.
%@  Any way, it works by generating permutations of @var{List} and unifying them with @var{Perm}.
%@  Be careful: this is quite efficient, but the number of permutations of an
%@  @var{N}-element list is @var{N!}, and even for a 7-element list that is 5040.

permutation(List, Perm) :-
	permutation(List, Perm, Perm).

permutation([], [], []).
permutation([X|Xs], Ys1, [_|Zs]) :-
	permutation(Xs, Ys, Zs),
	insert(Ys, X, Ys1).



%@  @item perm2(@var{?A},@var{?B}, @var{?C},@var{?D})
%@  @PLXindex {perm2/4 (lists)}
%@  is true when @var{@{A,B@} = @{C,D@}}.  It is very useful for writing pattern
%@  matchers over commutative operators.
:- perm2/4 is nondet.
perm2(X,Y, X,Y).
perm2(X,Y, Y,X).


:- if(true).
%@  @item proper_length(@var{+List}, @var{?Length})
%@  @PLXindex {proper_length/2 (lists)}
%@  succeeds when @var{List} is a proper list, binding @var{Length} to its length.
%@  That is, @code{is_list(List), length(List, Length)}.
%@  Will fail for cyclic lists.
proper_length(List, Length) :-
        % [PM] 4.3 Handles cycles, and likely faster.
        prolog:proper_list_length(List, Length).

:- else. % pre 4.3

proper_length(List, Length) :-
	proper_length(List, 0, Length).

/*  The original code was
	proper_length(X, _, _) :- var(X), !, fail.	% fail for variables too
	proper_length([], N, N).
	proper_length([_|List], N0, N) :-
		N1 is N0+1,
		proper_length(List, N1, N).
    The current version unrolls the loop by a factor of four; I have
    experimented with various degrees of unrolling, and while larger
    degrees do pay off for long lists, they don't do a lot for short
    ones.  This code is about 4.1 times faster than length/2 -- that
    cannot be speeded up because it has to cope with partial lists.
*/
proper_length(X, _, _) :- var(X), !, fail.
proper_length([_,_,_,_|L], N0, N) :- !, N1 is N0+4, proper_length(L, N1, N).
proper_length([_,_,_],     N0, N) :- !, N is N0+3.
proper_length([_,_],       N0, N) :- !, N is N0+2.
proper_length([_],         N0, N) :- !, N is N0+1.
proper_length([],          N,  N).

:- endif. % pre 4.3


%@  @item remove_dups(@var{+List}, @var{?Pruned})
%@  @PLXindex {remove_dups/2 (lists)}
%@  removes duplicated elements from @var{List}, which should be a proper list.
%@  If @var{List} has non-ground elements, @var{Pruned} may contain elements which
%@  unify; two elements will remain separate iff there is a substitution
%@  which makes them different.  E.g. @var{[X,X] -> [X]} but @var{[X,Y] -> [X,Y]}.
%@  The surviving elements, by ascending standard order, is unified with @var{Pruned}.

remove_dups(List, Pruned) :-
	sort(List, Pruned).



%@  @item reverse(@var{?List}, @var{?Reversed})
%@  @PLXindex {reverse/2 (lists)}
%@  is true when @var{List} and @var{Reversed} are lists with the same elements
%@  but in opposite orders.  Either @var{List} or @var{Reversed} should be a
%@  proper list: given either argument the other can be found.  If
%@  both are incomplete @code{reverse/2} can backtrack forever trying ever
%@  longer lists.

reverse(List, Reversed) :-
	reverse(List, Reversed, [], Reversed).

reverse([], [], Reversed, Reversed).
reverse([Head|Tail], [_|Bound], Sofar, Reversed) :-
	reverse(Tail, Bound, [Head|Sofar], Reversed).



%@  @item rev(@var{+List}, @var{?Reversed})
%@  @PLXindex {rev/2 (lists)}
%@  is a version of @code{reverse/2} which only works one way around.
%@  Its @var{List} argument must be a proper list whatever @var{Reversed} is.
%@  You should use @code{reverse/2} in new programs, though @code{rev/2} is
%@  faster when it is safe to use it.

rev(List, Reversed) :-
	rev(List, [], Reversed).

rev([], Reversed, Reversed).
rev([Head|Tail], Sofar, Reversed) :-
	rev(Tail, [Head|Sofar], Reversed).



%@  @item same_length(@var{?List1}, @var{?List2})
%@  @PLXindex {same_length/[2,3] (lists)}
%@  is true when @var{List1} and @var{List2} are both lists and have the same number
%@  of elements.  No relation between the values of their elements is
%@  implied.  It may be used to generate either list given the other,
%@  or indeed to generate two lists of the same length, in which case
%@  the arguments will be bound to lists of length 0, 1, 2, ... 
%@  If either @var{List1} or @var{List2} is bound to a proper
%@  list, same_length is determinate and terminating.
%   The current versions of @code{reverse/2} and @code{permutation/2} were obtained
%   by mixing this in with unidirectional versions.

% Determinate if either argument is a proper list
same_length(L1, L2) :- var(L1), !,
        'same_length_+?'(L2,L1).
same_length([], []).
same_length([_|List1], [_|List2]) :-
        same_length(List1, List2).

% Determinate if the first argument is a proper list
'same_length_+?'([], []).
'same_length_+?'([_|List1], [_|List2]) :-
        'same_length_+?'(List1, List2).





%@  @item same_length(@var{?List1}, @var{?List2}, @var{?Length})
%@  is true when @var{List1} and @var{List2} are both lists, @var{Length} is a non-negative
%@  integer, and both @var{List1} and @var{List2} have exactly @var{Length} elements.  No
%@  relation between the elements of the lists is implied.  If @var{Length}
%@  is instantiated, or if either @var{List1} or @var{List2} is bound to a proper
%@  list, same_length is determinate and terminating.

same_length(List1, List2, Length) :-
	(   integer(Length) ->
	    Length >= 0,
	    'same length'(Length, List1, List2)
	;   nonvar(Length) ->
	    must_be(Length, integer, same_length(List1,List2,Length), 3)
	;
	    'same length'(List1, List2, 0, Length)
	).

'same length'(0, List1, List2) :- !,	% delay unification
	List1 = [],			% to block infinite loops
	List2 = [].
'same length'(N, [_|Rest1], [_|Rest2]) :-
	M is N-1,			% N > 0, M >= 0
	'same length'(M, Rest1, Rest2).

% Determinate if either list argument is a proper list
'same length'(List1, List2, I, N) :- var(List1), !,
        'same length +?'(List2, List1, I, N).
'same length'([], [], N, N).
'same length'([_|Rest1], [_|Rest2], I, N) :-
	J is I+1,
	'same length'(Rest1, Rest2, J, N).

% Determinate if the first argument is a proper list
'same length +?'([], [], N, N).
'same length +?'([_|Rest1], [_|Rest2], I, N) :-
        J is I+1,
        'same length +?'(Rest1, Rest2, J, N).



%@  @item select(@var{?X}, @var{?Xlist}, @var{?Y}, @var{?Ylist})
%@  @PLXindex {select/4 (lists)}
%@  is true when @var{X} is the @var{Kth} member of @var{Xlist} and @var{Y} the @var{Kth} element of @var{Ylist}
%@  for some @var{K}, and apart from that @var{Xlist} and @var{Ylist} are the same.  You can
%@  use it to replace @var{X} by @var{Y} or vice versa.  Either @var{Xlist} or @var{Ylist} should
%@  be a proper list.  
%   This is very like sets:select/3.  Note that the
%   arguments are like the arguments of member/2, twice.
:- select(?,?,?,?) is nondet.
select(X, [X|Tail], Y, [Y|Tail]).
select(X, [Head|Xlist], Y, [Head|Ylist]) :-
	select(X, Xlist, Y, Ylist).



%@  @item selectchk(@var{?X}, @var{+Xlist}, @var{?Y}, @var{+Ylist})
%@  @PLXindex {selectchk/4 (lists)}
%@  is to @code{select/4} as @code{memberhck/2} is to @code{member/2}.  That is, it finds the
%@  first @var{K} such that @var{X} unifies with the @var{Kth} element of @var{Xlist} and @var{Y} with
%@  the @var{Kth} element of @var{Ylist}, and it commits to the bindings thus found.
%@  If you have @var{Keys} and @var{Values} in "parallel" lists, you can use this to
%@  find the @var{Value} associated with a particular @var{Key} (much better methods
%@  exist).  Except for argument order, this is identical to @code{correspond/4},
%@  but @code{selectchk/4} is a member of a coherent family.  Note that the
%@  arguments are like the arguments of @code{memberchk/2}, twice.

selectchk(X, [X|Tail], Y, [Y|Tail]) :- !.
selectchk(X, [Head|Xlist], Y, [Head|Ylist]) :-
	selectchk(X, Xlist, Y, Ylist).



%@  @item shorter_list(@var{?Short}, @var{?Long})
%@  @PLXindex {shorter_list/2 (lists)}
%@  is true when @var{Short} is a list is strictly shorter than @var{Long}.  @var{Long}
%@  doesn't have to be a proper list provided it is long enough.  This
%@  can be used to generate lists shorter than @var{Long}, lengths 0, 1, 2...
%@  will be tried, but backtracking will terminate with a list that is
%@  one element shorter than @var{Long}.  It cannot be used to generate lists
%@  longer than @var{Short}, because it doesn't look at all the elements of the
%@  longer list.

shorter_list([], [_|_]).
shorter_list([_|Short], [_|Long]) :-
	shorter_list(Short, Long).

%@  @item subseq(@var{?Sequence}, @var{?SubSequence}, @var{?Complement})
%@  @PLXindex {subseq/3 (lists)}
%@  is true when @var{SubSequence} and @var{Complement} are both subsequences of the
%@  list @var{Sequence} (the order of corresponding elements being preserved)
%@  and every element of @var{Sequence} which is not in @var{SubSequence} is in the
%@  @var{Complement} and vice versa.  That is,
%@  @code{length(Sequence) = length(SubSequence)+length(Complement)}, 
%@  e.g. @code{subseq([1,2,3,4], [1,3,4], [2])}.  This was written to generate subsets
%@  and their complements together, but can also be used to interleave two
%@  lists in all possible ways.  
%   Note that if @var{S1} is a subset of @var{S2}, it will
%   be generated before @var{S2} as a @var{SubSequence} and after it as a @var{Complement}.
:- subseq(?,?,?) is nondet.
subseq([], [], []).
subseq([Head|Tail], Sbsq, [Head|Cmpl]) :-
	subseq(Tail, Sbsq, Cmpl).
subseq([Head|Tail], [Head|Sbsq], Cmpl) :-
	subseq(Tail, Sbsq, Cmpl).



%@  @item subseq0(@var{+Sequence}, @var{?SubSequence})
%@  @PLXindex {subseq0/2 (lists)}
%@  is true when @var{SubSequence} is a subsequence of @var{Sequence}, but may
%@  be @var{Sequence} itself.   Thus @code{subseq0([a,b], [a,b])} is true as well
%@  as @code{subseq0([a,b], [a])}.  @var{Sequence} must be a proper list, since
%@  there are infinitely many lists with a given @var{SubSequence}.
%@  @example
%@  @group
%@  ?- setof(X, subseq0([a,b,c],X), Xs).
%@  Xs = [[],[a],[a,b],[a,b,c],[a,c],[b],[b,c],[c]] 
%@  ?- bagof(X, subseq0([a,b,c,d],X), Xs).
%@  Xs = [[a,b,c,d],[b,c,d],[c,d],[d],[],[c],[b,d],[b],[b,c],[a,c,d],
%@        [a,d],[a],[a,c],[a,b,d],[a,b],[a,b,c]] 
%@  @end group
%@  @end example
:- subseq0(+,?) is nondet.
subseq0(List, List).
subseq0(List, Rest) :-
	subseq1(List, Rest).


%@  @item subseq1(@var{+Sequence}, @var{?SubSequence})
%@  @PLXindex {subseq1/2 (lists)}
%@  is true when @var{SubSequence} is a proper subsequence of @var{Sequence},
%@  that is it contains at least one element less.  @var{Sequence} must
%@  be a proper list, as @var{SubSequence} does not determine @var{Sequence}.
:- subseq1(+,?) is nondet.
subseq1([_Head|Tail], Rest) :-
	subseq0(Tail, Rest).
subseq1([ Head|Tail], [Head|Rest]) :-
	subseq1(Tail, Rest).



%@  @item sumlist(@var{+Numbers}, @var{?Total})
%@  @PLXindex {sumlist/2 (lists)}
%@  is true when @var{Numbers} is a list of integers, and @var{Total} is their sum.
%@  @var{Numbers} should be a proper list.  
%   Note that in Dec-10 compiled Prolog this will only work as stated;
%   interpreters will almost certainly accept integer expressions.  Also
%   note here as elsewhere in Prolog arithmetic that machine arithmetic
%   wraps round in Quintus Prolog: (2^28 - 1)+1 = -2^28 .
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  sumlist(Numbers, Total) :-
%@  	(   foreach(X,Numbers),
%@  	    fromto(0,S0,S,Total)
%@  	do  S is S0+X
%@  	).
%@  @end group
%@  @end example

sumlist(Numbers, Total) :-
	(   foreach(X,Numbers),
	    fromto(0,S0,S,Total)
	do  S is S0+X
	).


%@  @item transpose(@var{?X}, @var{?Y})
%@  @PLXindex {transpose/2 (lists)}
%@  is true when @var{X} is a list of the form @var{[[X11,...,X1m],...,[Xn1,...,Xnm]]}
%@  and @var{Y} is its transpose, that is, @var{Y = [[X11,...,Xn1],...,[X1m,...,Xnm]]}
%@  We insist that both lists should have this rectangular form, so that
%@  the predicate can be invertible.  For the same reason, we reject empty
%@  arrays with @var{m = 0} or @var{n = 0}.

transpose(Xs, Ys) :-
	Xs = [X|_],	same_length(X, Ys), % length(X) = length([Y|Ys]) = M
	Ys = [Y|_],	same_length(Xs, Y), % length(Y) = length([X|Xs]) = N
	transpose(Ys, Xs, Xs).

transpose([], Zs, Xs) :-
	transpose_1(Zs, Xs).
transpose([Y|Ys], Zs, Xs) :-
	transpose_1(Xs, Y, Xs1),
	transpose(Ys, Zs, Xs1).

transpose_1([], []).
transpose_1([_|Zs], [[]|Xs]) :-
	transpose_1(Zs, Xs).

transpose_1([], [], []).
transpose_1([[H|T]|Xs], [H|Hs], [T|Ts]) :-
	transpose_1(Xs, Hs, Ts).

%   Package: length
%   Author : Richard A. O'Keefe
%   Updated: 25 Oct 1990
%   Purpose: chopping a list into pieces by length.

%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

/*  The predicates in this file were inspired by some written by
    Edouard Lagache for Automata Design Associates Prolog.  The
    file in question was called stdlist.pro.  No aspect of this
    code imitates any aspect of his code, but it grew out of a
    detailed criticism of said code.

    Most of the predicates in this package can be seen as a
    conjunction of append/3 or some predicate in library(listparts)
    with length/2.  The general convention is that the arguments
    are identical to those of the original in library(listparts)
    with the addition of a new last parameter, the Length of interest.
    The names of the predicates are formed by adding the suffix
    '_length' to the name of the original.

    It may help you when reading this code to note that the general
    pattern is to call <foo>_find when Length is a variable, or to
    call <foo>_make when Length is suitably instantiated.  I take
    the attitude that it is better to make life easier for the user
    of a package by making interfaces as logical as possible than
    to make life easier for me by just implementing half of something.

    The first draft of this package didn't have any error reporting,
    but since {proper_,}suffix_length/3 don't really make sense when
    the List is not proper, I decided to check and report that, and
    having imported one thing from library(types), it seemed good to
    me to follow the precedent set in nth{0,1}/[3,4] and report bad
    Length arguments too.  Why not?

    One thing which may seem odd to you is the placement of the
    Length parameter.  Why does it come last, when it is usually
    an input?  This takes some justifying.  I would point out
    that length/2 has the Length parameter last, and the
    "substring" predicates in library(strings) have all their
    length parameters last.  A selector argument should indeed
    come first, but the Length arguments do not indicate the
    location of an *element* of the Whole list, but the length
    of a sublist (or whatever) of the Whole, and we seem to have
    ended up with a convention of putting such arguments last.
    I have gone to some effort to make these predicates act like
    logical relations, so no argument is to be regarded as more
    of an input or an output than the others.  You can call any
    of these predicates with the Length unbound, and it will work.

    Note that "proper" does not mean "non-empty":  X is a proper
    prefix (suffix) of Y if X is a prefix (suffix) of Y,
    *other than Y itself*.  [] is a proper prefix of [a].

    See also the predicates
	nth0(Index, List, Element)
	nth0(Index, List, Element, Residue)
	nth1(Index, List, Element)
	nth1(Index, List, Element, Residue)
    in library(lists).  They may eventually move here.
*/



%   length(?List, ?Length)
%   is true when List is a list and Length is a non-negative integer
%   and List has exactly Length elements.  The normal use of this is
%   to find the Length of a given List, but it can be used any way
%   around provided that
%	Length is instantiated, or
%	List   is a proper list.
%   The other predicates in this file are derived pretty much directly
%   from this one.  length/2 is actually built into Quintus Prolog,
%   but if it weren't here's how we'd write it:
/*
length(List, Length) :-
	(   var(Length) ->
	    length_find(List, 0, Length)
	;   integer(Length) ->
	    Length >= 0,
	    length_make(Length, List)
	;   must_be(Length, integer, length(List,Length), 2)
	).

length_find([], N, N).
length_find([_|List], N0, N) :-
	N1 is N0+1,
	length_find(List, N1, N).

length_make(0, List) :- !,
	List = [].
length_make(N0, [_|List]) :-
	N1 is N0-1,
	length_make(N1, List).
*/



%@  @item append_length(@var{?Prefix}, @var{?Suffix}, @var{?List}, @var{?Length})
%@  @PLXindex {append_length/[3,4] (lists)}
%@  is true when
%@  @example
%@  @group
%@      append(Prefix, Suffix, List), length(Prefix, Length).
%@  @end group
%@  @end example
%@  The normal use of this is to split a @var{List} into a @var{Prefix} of
%@  a given @var{Length} and the corresponding @var{Suffix}, but it can be
%@  used any way around provided that
%@      @var{Length} is instantiated, or
%@      @var{Prefix} is a proper list, or
%@      @var{List}   is a proper list.

append_length(Prefix, Suffix, List, Length) :-
	(   var(Length) ->
	    append_find(Prefix, Suffix, List, 0, Length)
	;   integer(Length) ->
	    Length >= 0,
	    append_make(Length, Prefix, Suffix, List)
	;   must_be(Length, integer,
		append_length(Prefix,Suffix,List,Length), 4)
	).

append_find([], List, List, N, N).
append_find([Head|Prefix], Suffix, [Head|List], N0, N) :-
	N1 is N0+1,
	append_find(Prefix, Suffix, List, N1, N).

append_make(0, Prefix, Suffix, List) :- !,
	Prefix = [], Suffix = List.
append_make(N, [Head|Prefix], Suffix, [Head|List]) :-
	M is N-1,
	append_make(M, Prefix, Suffix, List).



%@  @item append_length(@var{?Suffix}, @var{?List}, @var{?Length})
%@  is true when there exists a list @var{Prefix} such that
%@  @code{append_length(@var{Prefix}, @var{Suffix}, @var{List}, @var{Length})} is true.
%@  When you don't want to know the @var{Prefix}, you should call this
%@  predicate, because it doesn't construct the @var{Prefix} argument,
%@  which @code{append_length/4} would do.

append_length(Suffix, List, Length) :-
	(   var(Length) ->
	    append_find(Suffix, List, 0, Length)
	;   integer(Length) ->
	    Length >= 0,
	    append_make(Length, Suffix, List)
	;   must_be(Length, integer, append_length(Suffix,List,Length), 3)
	).

:- append_find/4 is nondet.
append_find(List, List, N, N).
append_find(Suffix, [_|List], N0, N) :-
	N1 is N0+1,
	append_find(Suffix, List, N1, N).

append_make(0, Suffix, List) :- !,
	Suffix = List.
append_make(N, Suffix, [_|List]) :-
	M is N-1,
	append_make(M, Suffix, List).



%@  @item prefix_length(@var{?List}, @var{?Prefix}, @var{?Length})
%@  @PLXindex {prefix_length/3 (lists)}
%@  is true when
%@  @example
%@      prefix(List, Prefix) &
%@      length(Prefix, Length).
%@  @end example
%@  The normal use of this is to find the first @var{Length} elements of
%@  a given @var{List}, but it can be used any way around provided that
%@      @var{Length} is instantiated, or
%@      @var{Prefix} is a proper list, or
%@      @var{List}   is a proper list.
%@  It is identical in effect to @code{append_length(Prefix, _, List, Length)}.

prefix_length(List, Prefix, Length) :-
	(   var(Length) ->
	    prefix_find(Prefix, List, 0, Length)
	;   integer(Length) ->
	    Length >= 0,
	    prefix_make(Length, Prefix, List)
	;   must_be(Length, integer, prefix_length(List,Prefix,Length), 3)
	).

prefix_find([], _, N, N).
prefix_find([Head|Prefix], [Head|List], N0, N) :-
	N1 is N0+1,
	prefix_find(Prefix, List, N1, N).

prefix_make(0, Prefix, _) :- !,
	Prefix = [].
prefix_make(N, [Head|Prefix], [Head|List]) :-
	M is N-1,
	prefix_make(M, Prefix, List).



%@  @item proper_prefix_length(@var{?List}, @var{?Prefix}, @var{?Length})
%@  @PLXindex {proper_prefix_length/3 (lists)}
%@  is true when
%@  @example
%@      proper_prefix(List, Prefix) &
%@      length(Prefix, Length).
%@  @end example
%@  The normal use of this is to find the first @var{Length} elements of
%@  a given @var{List}, but it can be used any way around provided that
%@      @var{Length} is instantiated, or
%@      @var{Prefix} is a proper list, or
%@      @var{List}   is a proper list.
%@  It is logically equivalent to @code{prefix(Prefix, List, Length), Length > 0}.

proper_prefix_length(List, Prefix, Length) :-
	(   var(Length) ->
	    append_find(Prefix, Suffix, List, 0, Length)
	;   integer(Length) ->
	    Length >= 0,
	    append_make(Length, Prefix, Suffix, List)
	;   must_be(Length, integer,
		proper_prefix_length(List,Prefix,Length), 3)
	),
	Suffix = [_|_].



%@  @item suffix_length(@var{+List}, @var{?Suffix}, @var{?Length})
%@  @PLXindex {suffix_length/3 (lists)}
%@  is true when
%@  @example
%@      suffix(List, Suffix) &
%@      length(Suffix, Length).
%@  @end example
%@  The normal use of this is to return the last @var{Length} elements of
%@  a given @var{List}.  For this to be sure of termination,
%@      @var{List} must be a proper list.
%@  The predicate suffix/2 has the same requirement.
%@  If @var{Length} is instantiated or @var{Suffix} is a proper list, this predicate
%@  is determinate.

suffix_length(List, Suffix, Length) :-
	proper_length(List, Bound),
	!,
	(   var(Length) ->
	    (   proper_length(Suffix, Length) ->
		suffix_make(Length, Suffix, List, Bound)
	    ;   suffix_find(Suffix, List, Bound, Length)
	    )
	;   integer(Length) ->
	    Length >= 0, Length =< Bound,
	    suffix_make(Length, Suffix, List, Bound)
	;   must_be(Length, integer, suffix_length(List,Suffix,Length), 3)
	).
suffix_length(List, Suffix, Length) :-
	must_be(List, proper_list, suffix_length(List,Suffix,Length), 1).

:- suffix_find/4 is nondet.
suffix_find(List, List, N, N).
suffix_find(Suffix, [_|List], N0, N) :-
	N1 is N0-1,
	suffix_find(Suffix, List, N1, N).

suffix_make(Length, Suffix, List, Bound) :-
	Skip is Bound-Length,
	append_make(Skip, Suffix, List).



%@  @item proper_suffix_length(@var{+List}, @var{?Suffix}, @var{?Length})
%@  @PLXindex {proper_suffix_length/3 (lists)}
%@  is true when
%@  @example
%@      proper_suffix(List, Suffix) &
%@      length(Suffix, Length).
%@  @end example
%@  The normal use of this is to return the last @var{Length} elements of
%@  a given @var{List}.  For this to be sure of termination,
%@      @var{List} must be a proper list.
%@  The predicate proper_suffix/2 has the same
%   requirement.  The trouble is that for any given Suffix and Length
%   there are infinitely many compatible Lists.
%@  If @var{Length} is instantiated or @var{Suffix} is a proper list, this predicate
%@  is determinate.

proper_suffix_length([Head|Tail], Suffix, Length) :-
	proper_length(Tail, Bound),
	!,
	Bound > 0,		% otherwise it can't have a proper suffix
	(   var(Length) ->
	    (   proper_length(Suffix, Length) ->
		suffix_make(Length, Suffix, Tail, Bound)
	    ;   suffix_find(Suffix, Tail, Bound, Length)
	    )
	;   integer(Length) ->
	    Length >= 0, Length =< Bound,
	    suffix_make(Length, Suffix, Tail, Bound)
	;   must_be(Length, integer,
		proper_suffix_length([Head|Tail],Suffix,Length), 3)
	).
proper_suffix_length(List, Suffix, Length) :-
	must_be(List, proper_list,
	    proper_suffix_length(List,Suffix,Length), 1).



%@  @item rotate_list(@var{+Amount}, @var{?List}, @var{?Rotated})
%@  @PLXindex {rotate_list/[2,3] (lists)}
%@  is true when @var{List} and @var{Rotated} are lists of the same length, and
%@  @example
%@      append(Prefix, Suffix, List) &
%@      append(Suffix, Prefix, Rotated) &
%@      (   Amount >= 0 & length(Prefix, Amount)
%@      |   Amount =< 0 & length(Suffix, Amount)
%@      ).
%@  @end example
%@  That is to say, @var{List} rotated LEFT by @var{Amount} is @var{Rotated}.
%   I had two choices with this predicate: I could have it solve for
%   Amount, in which case Amount would have to be bounded, or I could
%   let it accept unbounded Amounts.  I wanted it to be the case that
%       rotate_list(A, X, Y) & rotate_list(B, Y, Z) & plus(A, B, C)
%    => rotate_list(C, X, Z),
%    so I chose to accept unbounded Amounts.  This means that the
%    @var{Amount} must already be instantiated.  As it is a strict input,
%    it must come first.
%@  If either @var{List} or @var{Rotated} is bound to a proper
%@  list, rotate_list is determinate.

rotate_list(Amount, List, Rotated) :-
	integer(Amount),
	!,
	same_length(List, Rotated, Length),
	Length > 0,		% [MC] SPRM 14871
	Delta is Amount mod Length,
	append_length(Prefix, Suffix, List, Delta),
	append(Suffix, Prefix, Rotated).
rotate_list(Amount, List, Rotated) :-
	must_be(Amount, integer, rotate_list(Amount,List,Rotated), 1).



%@  @item rotate_list(@var{?List}, @var{?Rotated})
%@  is true when @code{rotate_list(1, List, Rotated)}, but is a bit less
%@  heavy-handed.
%@  @code{rotate_list(X, Y)} rotates @var{X} left  one place yielding @var{Y}.
%@  @code{rotate_list(Y, X)} rotates @var{X} right one place yielding @var{Y}.
%@  Either @var{List} or @var{Rotated} should be a proper list,
%@  in which case rotate_list is determinate and terminating.

rotate_list(List, Rotated) :-
        same_length(List, Rotated), % [PM] 4.3.3 SPRM 14871
        List = [Head|Tail],
	append(Tail, [Head], Rotated).



%@  @item sublist(@var{+Whole}, @var{?Part}, @var{?Before}, @var{?Length}, @var{?After})
%@  @PLXindex {sublist/5 (lists)}
%@  is true when
%@  @itemize @bullet
%@  @item @var{Whole} is a list -- it must be proper already
%@  @item @var{Part}  is a list
%@  @item @var{Whole = Alpha || Part || Omega}
%@  @item @code{length(@var{Alpha}, @var{Before})}
%@  @item @code{length(@var{Part},  @var{Length})}
%@  @item @code{length(@var{Omega}, @var{After})}
%@  @end itemize
%   This is an exact parallel to substring/5 in library(strings),
%   and may be used for similar purposes on lists of character codes.
%   But it can be used to divide a list up many ways:
%       prefix_length(P, W, L) :- sublist(W, P, 0, L, _)
%       suffix_length(S, W, L) :- sublist(W, P, _, L, 0)
%   and so on.  Note that these more specific predicates may be used
%   to solve for Whole, but sublist/5 cannot be.
%  There is more work to be done on these to make them work more ways
%  around.  It would be nice if there were a way to take the logical
%  specification and systematically work out how to code it to work
%  in as many cases as possible.

sublist(Whole, Part, Before) :-
	sublist(Whole, Part, Before, _, _).

sublist(Whole, Part, Before, Length) :-
	sublist(Whole, Part, Before, Length, _).

sublist(Whole, Part, Before, Length, After) :-
	integer(Before),
	(   integer(Length) -> true
	;   var(Length), proper_length(Part, Length)
	),
	!,
	Before >= 0,
	Length >= 0,
	append_make(Before, Suffix, Whole),
	append_make(Length, Part, Rest, Suffix),
	length(Rest, After).
sublist(Whole, Part, Before, Length, After) :-
	proper_length(Whole, LengthOfWhole),	
	(   integer(Before) -> true
	;   var(Before)
	),
	(   integer(After) -> true
	;   var(After)
	),
	(   integer(Length) -> true
	;   nonvar(Length) -> fail
	;   proper_length(Part, Length) -> true
	;   true
	),
	!,
	append_length(Suffix, Whole, Before),	
	(   var(Length) -> true	
	;   Length =< LengthOfWhole-Before
	),
	(   var(After) -> true
	;   After =< LengthOfWhole-Before
	),
	append_length(Part, _, Suffix, Length),
	After is LengthOfWhole-Before-Length.
sublist(Whole, Part, Before, Length, After) :-
	Goal = sublist(Whole,Part,Before,Length,After),
	must_be(Whole, proper_list, Goal, 1),
	must_be(Before, integer, Goal, 3),
	must_be(Length, integer, Goal, 4),
	must_be(After, integer, Goal,  5).


%   Module : list_parts
%   Author : Richard A. O'Keefe
%   Updated: 26 Dec 1989
%   Defines: names for parts of lists.

%   Copyright (C) 1989, Quintus Computer Systems, Inc.  All rights reserved.

/* pred
	cons(T, list(T), list(T)),
	last(list(T), T, list(T)),
	head(list(T), T),
	tail(list(T), list(T)),
	prefix(list(T), list(T)),
	suffix(list(T), list(T)),
	segment(list(T), list(T)),
	proper_prefix(list(T), list(T)),
	proper_suffix(list(T), list(T)),
	proper_segment(list(T), list(T)).
*/



/*  The main purpose of this file is to establish a common vocabulary
    for names of parts of lists among Prolog programmers.  You will
    seldom have occasion to use head/2 or tail/2 in your programs
    -- pattern matching is clearer and faster -- but you will often
    use these words when talking about your programs, and we shall
    all benefit if we use the same words with the same meanings.

    It really has not been clear what to call segments.  I originally
    called them "sublists", but when I came to generalise these predicates
    to other kinds of sequences, I found that I could talk about a prefix
    or a suffix or a segment of a general sequence, but that talking about
    a sublist of a general sequence didn't make sense.  

    Please send in your suggestions for other parts of lists (or of
    other standard data structures) which need agreed names, and for
    what those names should be.
*/

%@  @item cons(@var{?Head}, @var{?Tail}, @var{?List})
%@  @PLXindex {cons/3 (lists)}
%@  is true when @var{Head} is the head of @var{List} and @var{Tail} is its tail.
%@  i.e. @code{append([Head], Tail, List)}.   No restrictions.

cons(Head, Tail, [Head|Tail]).


%@  @item last(@var{?Fore}, @var{?Last}, @var{?List})
%@  @PLXindex {last/3 (lists)}
%@  is true when @var{Last} is the last element of @var{List} and @var{Fore} is the
%@  list of preceding elements, e.g. @code{append(Fore, [Last], List)}.
%@  @var{Fore} or @var{Last} should be proper.  It is expected that @var{List} will
%@  be proper and @var{Fore} unbound, but it will work in reverse too.
%   In QP 2.4.2 on a Sun-3/50M, this is >3x faster than append/3.

last(Fore, Last, [Head|Tail]) :-
	last_1(Tail, Fore, Head, Last).

last_1([], [], Last, Last).
last_1([Head|Tail], [Prev|Fore], Prev, Last) :-
	last_1(Tail, Fore, Head, Last).



/*  All of the remaining predicates in this file are binary, and
    <part>(Whole, Part) is to be read as "Part is the/a <part>
    of Whole".  When both <part>/2 and proper_<part>/2 exist,
    proper <part>s are strictly smaller than Whole, whereas
    Whole may be a <part> of itself.  In the comments, N is the
    length of the Whole argument, assumed to be a proper list.

    Version 9.1 of this file had the Whole and Part arguments
    swapped.  I had allowed the order of the words in the English
    sentence to influence the order of the arguments, which is a
    very poor way to choose an argument order.  The new order is
    strictly in accord with the fundamental principle of
    argument ordering in Prolog: INPUTS BEFORE OUTPUTS.

    The predicates bear their indicated meanings only when their
    arguments are of the right types.  They are undefined for other
    terms.  Thus tail([e|g], g) succeeds despite the fact that 'g'
    isn't any sort of list.  Similarly, prefix(g, []) succeeds.
    This is not a bug!  'g' not being a list, the behavior of
    prefix/2 in that case is not defined.

    The picture to keep in mind is
	Whole = Prefix || SubList || Suffix
	      = [Head] || Tail
	      =                 _ || [Last]
*/


%@  @item head(@var{?List}, @var{?Head})
%@  @PLXindex {head/2 (lists)}
%@  is true when @var{List} is a non-empty list and @var{Head} is its head.
%@  A list has only one head.  No restrictions.

head([Head|_], Head).


%@  @item tail(@var{?List}, @var{?Tail})
%@  @PLXindex {tail/2 (lists)}
%@  is true when @var{List} is a non-empty list and @var{Tail} is its tail.
%@  A list has only one tail.  No restrictions.

tail([_|Tail], Tail).



%@  @item prefix(@var{?List}, @var{?Prefix})
%@  @PLXindex {prefix/2 (lists)}
%@  is true when @var{List} and @var{Prefix} are lists and @var{Prefix} is a prefix of @var{List}.
%@  It terminates if either argument is proper, and has at most @var{N+1} solutions.
%@  Prefixes are enumerated in ascending order of length.

prefix(List, Prefix) :-
	prefix_1(Prefix, List).

prefix_1([], _).
prefix_1([Head|Prefix], [Head|List]) :-
	prefix_1(Prefix, List).


%@  @item proper_prefix(@var{?List}, @var{?Prefix})
%@  @PLXindex {proper_prefix/2 (lists)}
%@  is true when @var{List} and @var{Prefix} are lists and @var{Prefix} is a proper prefix
%@  of @var{List}.  That is, @var{Prefix} is a prefix of @var{List} but is not @var{List} itself.
%@  It terminates if either argument is proper, and has at most @var{N} solutions.
%@  Prefixes are enumerated in ascending order of length.

proper_prefix(List, Prefix) :-
	proper_prefix_1(Prefix, List).

proper_prefix_1([], [_|_]).
proper_prefix_1([Head|Prefix], [Head|List]) :-
	proper_prefix_1(Prefix, List).



%@  @item suffix(@var{?List}, @var{?Suffix})
%@  @PLXindex {suffix/2 (lists)}
%@  is true when @var{List} and @var{Suffix} are lists and @var{Suffix} is a suffix of @var{List}.
%@  It terminates only if @var{List} is proper, and has at most @var{N+1} solutions.
%@  Suffixes are enumerated in descending order of length.
:- suffix(?,?) is nondet.
suffix(List, List).
suffix([_|List], Suffix) :-
	suffix(List, Suffix).


%@  @item proper_suffix(@var{?List}, @var{?Suffix})
%@  @PLXindex {proper_suffix/2 (lists)}
%@  is true when @var{List} and @var{Suffix} are lists and @var{Suffix} is a proper suffix
%@  of @var{List}.  That is, @var{Suffix} is a suffix of @var{List} but is not @var{List} itself.
%@  It terminates only if @var{List} is proper, and has at most @var{N} solutions.
%@  Suffixes are enumerated in descending order of length.

proper_suffix([_|List], Suffix) :-
	suffix(List, Suffix).



%@  @item segment(@var{?List}, @var{?Segment})
%@  @PLXindex {segment/2 (lists)}
%@  is true when @var{List} and @var{Segment} are lists and @var{Segment} is a segment
%@  of @var{List}.  That is, @var{List = _ <> Segment <> _ }.
%@  Terminates only if @var{List} is proper.  If @var{Segment} is proper
%@  it enumerates all solutions.  If neither argument is proper, it
%@  would have to diagonalise to find all solutions, but it doesn't, so
%@  it is then incomplete.  If @var{Segment} is proper, it has at most @var{N+1}
%@  solutions.  Otherwise, it has at most @var{(1/2)(N+1)(N+2)} solutions.
:- segment(?,?) is nondet.
segment([], []).
segment([Head|Tail], [Head|Segment]) :-
        prefix_1(Segment, Tail).
segment([_|Tail], Segment) :-
        segment(Tail, Segment).


%@  @item proper_segment(@var{?List}, @var{?Segment})
%@  @PLXindex {proper_segment/2 (lists)}
%@  is true when @var{List} and @var{Segment} are lists and @var{Segment} is a proper
%@  segment of @var{List}.  It terminates only if @var{List} is proper.  The only
%@  solution of @code{segment/2} which is not a solution of @code{proper_segment/2}
%@  is @code{segment(List,List)}.  So @code{proper_segment/2} has one solution fewer.
:- proper_segment(?,?) is nondet.
proper_segment(List, Segment) :-
	proper_prefix_1(Segment, List).
proper_segment([_|List], Segment) :-
	segment(List, Segment).


%   Package: maplist
%   Author : Lawrence Byrd + Richard A. O'Keefe
%   Updated: 02 Nov 1988
%   Purpose: Various "function" application routines based on library(call).

%   Adapted from shared code written by the same authors; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

/*  There are five general families:
	maplist(P, [X11,...,X1n], ..., [Xm1,...,Xmn]) :-
	    P(X11, ..., Xm1),
	    ...
	    P(Xm1, ..., Xmn).

	scanlist(P, [X11,...,X1n], ..., [Xm1,...,Xmn], V0, Vn) :-
	    P(X11, ..., Xm1, V0, V1),
	    ...
	    P(Xm1, ..., Xmn, V', Vn).

	cumlist(P, [X11,...,X1n], ..., [Xm1,...,Xmn], V1, [V1,...,Vn]) :-
	    P(X11, ..., Xmn, V0, V1),
	    ...
	    P(Xm1, ..., Xmn, V', Vn).


	some(P, [X11,...,X1n], ..., [Xm1,...,Xmn]) :-
	    P(X11, ..., Xm1)
	  ; ...
	  ; P(Xm1, ..., Xmn).

	somechk(P, [X11,...,X1n], ..., [Xm1,...,Xmn]) :-
	    P(X11, ..., Xm1) -> true
	  ; ...
	  ; P(Xm1, ..., Xmn) -> true.

    Ideally, {maplist,some,somechk}/N would exist for all N >= 2
    and {cumlist,scanlist}/N would exist for all N >= 4.
    At the moment, only the ones I have had occasion to use exist.
    The names may be changed before the package becomes supported.
    Note that the argument order of the predicate argument to the
    scanlist/4 predicate was changed in May 1988.  This was so that

    A general characteristic of these families is that the order of
    arguments passed to Pred is the same as the order of the
    arguments following Pred.  This close correspondence helps you
    keep track of which argument goes where.

    Several other predicates (convlist/3, exclude/3, include/3, and
    partition/5) have moved into a new file library(moremaps) where
    some additional new predicates will be found.
*/


/* pred
	cumlist(void(T,U,U), list(T), U, list(U)),
	    cum_list(list(T), U, list(U), void(T,U,U)),
	cumlist(void(S,T,U,U), list(S), list(T), U, list(U)),
	    cum_list(list(S), list(T), U, list(U), void(S,T,U,U)),
	cumlist(void(R,S,T,U,U), list(R), list(S), list(T), U, list(U)),
	    cum_list(list(R), list(S), list(T), U, list(U), void(R,S,T,U,U)),
	maplist(void(T), list(T)),
	    map_list(list(T), void(T)),
	maplist(void(T,U), list(T), list(U)),
	    map_list(list(T), list(U), void(T,U)),
	maplist(void(T,U,V), list(T), list(U), list(V)),
	    map_list(list(T), list(U), list(V), void(T,U,V)),
	map_product(void(U,V,T), list(U), list(V), list(T)),
	    map_product_1(list(U), list(V), list(T), list(T), void(U,V,T)),
		map_product_2(list(V), U, list(T), list(T), void(U,V,T)),
	scanlist(void(T,U,U), list(T), U, U),
	    scan_list(list(T), U, U, void(T,U,U)),
	scanlist(void(S,T,U,U), list(S), list(T), U, U),
	    scan_list(list(S), list(T), U, U, void(S,T,U,U)),
	scanlist(void(R,S,T,U,U), list(R), list(S), list(T), U, U),
	    scan_list(list(R), list(S), list(T), U, U, void(R,S,T,U,U)),
	some(void(T), list(T)),
	some(void(T,U), list(T), list(U)),
	some(void(T,U,V), list(T), list(U), list(V)),
	somechk(void(T), list(T)),
	somechk(void(T,U), list(T), list(U)),
	somechk(void(T,U,V), list(T), list(U), list(V)).


    The <foo>list predicates are retained for backwards compatibility with
    the Dec-10 Prolog library.  They all take a void(...) argument as their
    first argument.  The <foo>_list predicates take the void(...)
    argument *last* so as to exploit indexing on the first argument.
    Putting the Pred argument first is still considered to be the better
    style as far as human reading and comprehension is concerned, and
    putting it last is not recommended for exported predicates.

    BEWARE: the scanlist/4 predicate changed subtly in May 1988 when it
    was generalised to the scanlist/N family.  If you were passing
    commutative functions to it, you won't notice any change, but the
    new version is simpler than the old.
*/


%@  @item cumlist(@var{:Pred}, @var{+[X1,...,Xn]}, @var{?V0}, @var{?[V1,...,Vn]})
%@  @itemx cumlist(@var{:Pred}, @var{+[X1,...,Xn]}, @var{+[Y1,...,Yn]}, @var{?V0}, @var{?[V1,...,Vn]})
%@  @itemx cumlist(@var{:Pred}, @var{+[X1,...,Xn]}, @var{+[Y1,...,Yn]}, @var{+[Z1,...,Zn]}, @var{?V0}, @var{?[V1,...,Vn]})
%@  @PLXindex {cumlist/[4,5,6] (lists)}
%@  @code{cumlist/4} maps a ternary predicate @var{Pred} down the list @var{[X1,...,Xn]} just as
%@  @code{scanlist/4} does, and returns a list of the results.  It terminates
%@  when the lists runs out.  If @var{Pred} is bidirectional, it may be
%@  used to derive @var{[X1...Xn]} from @var{V0} and @var{[V1...Vn]}, e.g.
%@  @code{cumlist(plus, [1,2,3,4], 0, /* -> */ [1,3,6,10])} and
%@  @code{cumlist(plus, [1,1,1,1], /* <- */ 0, [1,2,3,4])}.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  cumlist(Pred, Xs, V0, Cum) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(V,Cum),
%@  	    fromto(V0,V1,V,_),
%@  	    param(Pred)
%@  	do  call(Pred,X,V1,V)
%@  	).
%@  
%@  cumlist(Pred, Xs, Ys, V0, Cum) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(Y,Ys),
%@  	    foreach(V,Cum),
%@  	    fromto(V0,V1,V,_),
%@  	    param(Pred)
%@  	do  call(Pred,X,Y,V1,V)
%@  	).
%@  
%@  cumlist(Pred, Xs, Ys, Zs, V0, Cum) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(Y,Ys),
%@  	    foreach(Z,Zs),
%@  	    foreach(V,Cum),
%@  	    fromto(V0,V1,V,_),
%@  	    param(Pred)
%@  	do  call(Pred,X,Y,Z,V1,V)
%@  	).
%@  @end group
%@  @end example

cumlist(Pred, Xs, V0, Cum) :-
	(   foreach(X,Xs),
	    foreach(V,Cum),
	    fromto(V0,V1,V,_),
	    param(Pred)
	do  call(Pred,X,V1,V)
	).

cumlist(Pred, Xs, Ys, V0, Cum) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(V,Cum),
	    fromto(V0,V1,V,_),
	    param(Pred)
	do  call(Pred,X,Y,V1,V)
	).

cumlist(Pred, Xs, Ys, Zs, V0, Cum) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(Z,Zs),
	    foreach(V,Cum),
	    fromto(V0,V1,V,_),
	    param(Pred)
	do  call(Pred,X,Y,Z,V1,V)
	).

%@  @item maplist(@var{:Pred}, @var{+List})
%@  @PLXindex {maplist/[2,3,4] (lists)}
%@  succeeds when @var{Pred(X)} succeeds for each element @var{X} of @var{List}.
%@  @var{List} should be a proper list.  
%   This is identical to the (now obsolete) predicate checklist/2.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  maplist(Pred, Xs) :-
%@  	(   foreach(X,Xs),
%@  	    param(Pred)
%@  	do  call(Pred, X)
%@  	).
%@  @end group
%@  @end example

maplist(Pred, Xs) :-
	(   foreach(X,Xs),
	    param(Pred)
	do  call(Pred, X)
	).



%@  @item maplist(@var{:Pred}, @var{+OldList}, @var{?NewList})
%@  succeeds when @var{Pred(Old,New)} succeeds for each corresponding
%@  @var{Old} in @var{OldList}, @var{New} in @var{NewList}.  
%   In InterLisp, this is MAPCAR. 
%   It is also MAP2C.  Isn't bidirectionality wonderful?
%@  Either @var{OldList} or @var{NewList} should be a proper list.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  maplist(Pred, Xs, Ys) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(Y,Ys),
%@  	    param(Pred)
%@  	do  call(Pred, X, Y)
%@  	).
%@  @end group
%@  @end example

maplist(Pred, Xs, Ys) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    param(Pred)
	do  call(Pred, X, Y)
	).

%@  @item maplist(@var{:Pred}, @var{+Xs}, @var{?Ys}, @var{?Zs})
%@  is true when @var{Xs}, @var{Ys}, and @var{Zs} are lists of equal length, and
%@  @var{Pred(X, Y, Z)} is true for corresponding elements @var{X} of @var{Xs},
%@  @var{Y} of @var{Ys}, and @var{Z} of @var{Zs}.
%@  At least one of @var{Xs}, @var{Ys}, and @var{Zs} should be a proper list.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  maplist(Pred, Xs, Ys, Zs) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(Y,Ys),
%@  	    foreach(Z,Zs),
%@  	    param(Pred)
%@  	do  call(Pred, X, Y, Z)
%@  	).
%@  @end group
%@  @end example

maplist(Pred, Xs, Ys, Zs) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(Z,Zs),
	    param(Pred)
	do  call(Pred, X, Y, Z)
	).


%@  @item map_product(Pred, Xs, Ys, PredOfProduct)
%@  @PLXindex {map_product/5 (lists)}
%@  Just as @code{maplist(P, Xs, L)} is the analogue of Miranda's
%@  @example
%@      let L = [ P x | x <- Xs ]
%@  @end example
%@  so @code{map_product(P, Xs, Ys, L)} is the analogue of Miranda's
%@  @example
%@      let L = [ P x y | x <- Xs; y <- Ys ]
%@  @end example
%@  That is, if @var{Xs = [X1,...,Xm]}, @var{Ys = [Y1,...,Yn]}, and @var{P(Xi,Yj,Zij)},
%@  @var{L = [Z11,...,Z1n,Z21,...,Z2n,...,Zm1,...,Zmn]}.
%@  It is as if we formed the cartesian product of @var{Xs} and @var{Ys} and
%@  applied @var{P} to the @var{(Xi,Yj)} pairs.
%@  @var{Xs} and @var{Ys} should be proper lists.  
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  map_product(Pred, Xs, Ys, Zs) :-
%@  	(   foreach(X,Xs),
%@  	    fromto(Zs,S0,S,[]),
%@  	    param([Ys,Pred])
%@  	do  (   foreach(Y,Ys),
%@  		fromto(S0,[Z|S1],S1,S),
%@  		param([X,Pred])
%@  	    do  call(Pred, X, Y, Z)
%@  	    )
%@  	).
%@  @end group
%@  @end example

map_product(Pred, Xs, Ys, Zs) :-
	(   foreach(X,Xs),
	    fromto(Zs,S0,S,[]),
	    param([Ys,Pred])
	do  (   foreach(Y,Ys),
		fromto(S0,[Z|S1],S1,S),
		param([X,Pred])
	    do  call(Pred, X, Y, Z)
	    )
	).


%@  @item scanlist(@var{:Pred}, @var{[X1,...,Xn]}, @var{?V1}, @var{?V})
%@  @itemx scanlist(@var{:Pred}, @var{[X1,...,Xn]}, @var{[Y1,...,Yn]}, @var{?V1}, @var{?V})
%@  @itemx scanlist(@var{:Pred}, @var{[X1,...,Xn]}, @var{[Y1,...,Yn]}, @var{[Z1,...,Zn]}, @var{?V1}, @var{?V})
%@  @PLXindex {scanlist/[4,5,6] (lists)}
%@  @code{scanlist/4} maps a ternary relation @var{Pred} down a list.  The computation is
%@  @var{Pred(X1,V1,V2)}, @var{Pred(X2,V2,V3)}, ..., @var{Pred(Xn,Vn,V)}
%@  So if @var{Pred} is @code{plus/3}, @code{scanlist(plus, [X1,...,Xn], 0, V)} puts the
%@  sum of the list elements in @var{V}.
%@  Note that the order of the arguments passed to Pred is the same
%@  as the order of the arguments following Pred.  This also holds
%@  for scanlist/5 and scanlist/6, e.g.
%@  scanlist(Pred, Xs, Ys, Zs, V1, V) calls Pred(X3,Y3,Z3,V3,V4).
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  scanlist(Pred, Xs, V0, V) :-
%@  	(   foreach(X,Xs),
%@  	    fromto(V0,V1,V2,V),
%@  	    param(Pred)
%@  	do  call(Pred, X, V1, V2)
%@  	).
%@  
%@  scanlist(Pred, Xs, Ys, V0, V) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(Y,Ys),
%@  	    fromto(V0,V1,V2,V),
%@  	    param(Pred)
%@  	do  call(Pred, X, Y, V1, V2)
%@  	).
%@  
%@  scanlist(Pred, Xs, Ys, Zs, V0, V) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(Y,Ys),
%@  	    foreach(Z,Zs),
%@  	    fromto(V0,V1,V2,V),
%@  	    param(Pred)
%@  	do  call(Pred, X, Y, Z, V1, V2)
%@  	).
%@  @end group
%@  @end example

scanlist(Pred, Xs, V0, V) :-
	(   foreach(X,Xs),
	    fromto(V0,V1,V2,V),
	    param(Pred)
	do  call(Pred, X, V1, V2)
	).

scanlist(Pred, Xs, Ys, V0, V) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    fromto(V0,V1,V2,V),
	    param(Pred)
	do  call(Pred, X, Y, V1, V2)
	).

scanlist(Pred, Xs, Ys, Zs, V0, V) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(Z,Zs),
	    fromto(V0,V1,V2,V),
	    param(Pred)
	do  call(Pred, X, Y, Z, V1, V2)
	).


%@  @item some(@var{:Pred}, @var{+List})
%@  @PLXindex {some/[2,3,4] (lists)}
%@  succeeds when @var{Pred(Elem)} succeeds for some @var{Elem} in @var{List}.  It will
%@  try all ways of proving @var{Pred} for each @var{Elem}, and will try each @var{Elem}
%@  in the @var{List}.  @code{somechk/2} is to @code{some/2} as @code{memberchk/2} is to @code{member/2}.
%@  @example
%@      member(X,L)     <-> some(=(X), L).
%@      memberchk(X, L) <-> somechk(=(X), L).
%@      some(Pred,L)    <-> member(X, L), call(Pred,X).
%@  @end example
%@  This acts on backtracking like member/2; List should be a proper list.
%   In InterLisp this is SOME.
:- some(1,+) is nondet.
some(Pred, [X|_]) :-
	call(Pred, X).
some(Pred, [_|Xs]) :-
	some(Pred, Xs).


%@  @item some(@var{:Pred}, @var{+[X1,...,Xn]}, @var{?[Y1,...,Yn]})
%@  is true when @var{Pred(Xi, Yi)} is true for some @var{i}.
:- some(2,+,?) is nondet.
some(Pred, [X|_], [Y|_]) :-
	call(Pred, X, Y).
some(Pred, [_|Xs], [_|Ys]) :-
	some(Pred, Xs, Ys).



%@  @item some(@var{:Pred}, @var{+[X1,...,Xn]}, @var{?[Y1,...,Yn]}, @var{?[Z1,...,Zn]})
%@  is true when @var{Pred(Xi, Yi, Zi)} is true for some @var{i}.
:- some(3,+,?,?) is nondet.
some(Pred, [X|_], [Y|_], [Z|_]) :-
	call(Pred, X, Y, Z).
some(Pred, [_|Xs], [_|Ys], [_|Zs]) :-
	some(Pred, Xs, Ys, Zs).



%@  @item somechk(@var{:Pred}, @var{+[X1,...,Xn]})
%@  @PLXindex {somechk/[2,3,4] (lists)}
%@  is true when @var{Pred(Xi)} is true for some @var{i}, and it commits to
%@  the first solution it finds (like @code{memberchk/2}).

somechk(Pred, [X|_]) :-
	call(Pred, X),
	!.
somechk(Pred, [_|Xs]) :-
	somechk(Pred, Xs).


%@  @item somechk(@var{:Pred}, @var{+[X1,...,Xn]}, @var{?[Y1,...,Yn]})
%@  is true when @var{Pred(Xi, Yi)} is true for some @var{i}, and it commits to
%@  the first solution it finds (like @code{memberchk/2}).

somechk(Pred, [X|_], [Y|_]) :-
	call(Pred, X, Y),
	!.
somechk(Pred, [_|Xs], [_|Ys]) :-
	somechk(Pred, Xs, Ys).


%@  @item somechk(@var{:Pred}, @var{+[X1,...,Xn]}, @var{?[Y1,...,Yn]}, @var{?[Z1,...,Zn]})
%@  is true when @var{Pred(Xi, Yi, Zn)} is true for some @var{i}, and it commits to
%@  the first solution it finds (like @code{memberchk/2}).

somechk(Pred, [X|_], [Y|_], [Z|_]) :-
	call(Pred, X, Y, Z),
	!.
somechk(Pred, [_|Xs], [_|Ys], [_|Zs]) :-
	somechk(Pred, Xs, Ys, Zs).



%   There used to be a predicate sublist(Pred, List, SubList) 
%   which was identical to include(Pred, List, SubList) in all but name.
%   The name sublist/3 was needed in library(length), so the redundant
%   sublist/3 has been withdrawn from this file.


/*  Example:
	maplist(include(atom), [[a,1,2,b],[3,c],[4,5,6],[d,e]], L)
  binds L = [[a,b],[c],[],[d,e]].
	maplist(exclude(atom), [[a,1,2,b],[3,c],[4,5,6],[d,e]], L)
  binds L = [[1,2],[3],[4,5,6],[]].
*/

%   Package: more_maps
%   Author : Richard A. O'Keefe
%   Updated: 13 Jul 1989
%   Purpose: More predicates for mapping down lists.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1988, Quintus Computer Systems, Inc.  All rights reserved.

/* pred
	convlist(void(T,U), list(T), list(U)),
	    conv_list(list(T), list(U), void(T,U)),
	exclude(void(T), list(T), list(T)),
	    exclude_list(list(T), list(T), void(T)),
	include(void(T), list(T), list(T)),
	    include_list(list(T), list(T), void(T)),
	partition(void(T,order), list(T), list(T), list(T), list(T)),
	    partition_1(list(T), void(T,order), list(T), list(T), list(T)),
		partition_1(order, T, list(T), list(T), list(T),
			    void(T,order), list(T)),
	group(void(T,T), list(T), list(list(T))),
	    group1(list(T), list(list(T)), void(T,T)),
	group(void(T,T), list(T), list(T), list(T)),
	group(void(T,T), T, list(T), list(T), list(T)).
*/




%@  @item convlist(@var{:Rewrite}, @var{+OldList}, @var{?NewList})
%@  @PLXindex {convlist/3 (lists)}
%@  is a sort of hybrid of @code{maplist/3} and @code{include/3}.
%@  Each element of @var{NewList} is the image under @var{Rewrite} of some
%@  element of @var{OldList}, and order is preserved, but elements of
%@  @var{OldList} on which @var{Rewrite} is undefined (fails) are not represented.
%@  Thus if @code{foo(K,X,Y) :- integer(X), Y is X+K.}
%@  then @code{convlist(foo(1), [1,a,0,joe(99),101], [2,1,102]).}
%@  @var{OldList} should be a proper list.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  convlist(Pred, Xs, News) :-
%@  	(   foreach(X,Xs),
%@  	    fromto(News,S0,S,[]),
%@  	    param(Pred)
%@  	do  (call(Pred,X,N) -> S0 = [N|S] ; S0 = S)
%@  	).
%@  @end group
%@  @end example

convlist(Pred, Xs, News) :-
	(   foreach(X,Xs),
	    fromto(News,S0,S,[]),
	    param(Pred)
	do  (call(Pred,X,N) -> S0 = [N|S] ; S0 = S)
	).

%@  @item exclude(@var{:Pred}, @var{+Xs}, @var{?SubList})
%@  @itemx exclude(@var{:Pred}, @var{+Xs}, @var{+Ys}, @var{?SubList})
%@  @itemx exclude(@var{:Pred}, @var{+Xs}, @var{+Ys}, @var{+Zs}, @var{?SubList})
%@  @PLXindex {exclude/[3,4,5] (lists)}
%@  succeeds when @var{SubList} is the sublist of @var{Xs} containing all the
%@  elements @var{Xi[,Yi[,Zi]]} for which @var{Pred(Xi[,Yi[,Zi]])} is @emph{false}.  That is, it removes
%@  all the elements satisfying @var{Pred}.
%@  @var{Xs}, @var{Ys} or @var{Zs} should be a proper list.  
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  exclude(Pred, Xs, News) :-
%@  	(   foreach(X,Xs),
%@  	    fromto(News,S0,S,[]),
%@  	    param(Pred)
%@  	do  (call(Pred,X) -> S0 = S ; S0 = [X|S])
%@  	).
%@  
%@  exclude(Pred, Xs, Ys, News) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(Y,Ys),
%@  	    fromto(News,S0,S,[]),
%@  	    param(Pred)
%@  	do  (call(Pred,X,Y) -> S0 = S ; S0 = [X|S])
%@  	).
%@  
%@  exclude(Pred, Xs, Ys, Zs, News) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(Y,Ys),
%@  	    foreach(Z,Zs),
%@  	    fromto(News,S0,S,[]),
%@  	    param(Pred)
%@  	do  (call(Pred,X,Y,Z) -> S0 = S ; S0 = [X|S])
%@  	).
%@  @end group
%@  @end example

exclude(Pred, Xs, News) :-
	(   foreach(X,Xs),
	    fromto(News,S0,S,[]),
	    param(Pred)
	do  (call(Pred,X) -> S0 = S ; S0 = [X|S])
	).

exclude(Pred, Xs, Ys, News) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    fromto(News,S0,S,[]),
	    param(Pred)
	do  (call(Pred,X,Y) -> S0 = S ; S0 = [X|S])
	).

exclude(Pred, Xs, Ys, Zs, News) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(Z,Zs),
	    fromto(News,S0,S,[]),
	    param(Pred)
	do  (call(Pred,X,Y,Z) -> S0 = S ; S0 = [X|S])
	).

%@  @item include(@var{:Pred}, @var{+Xs}, @var{?SubList})
%@  @itemx include(@var{:Pred}, @var{+Xs}, @var{+Ys}, @var{?SubList})
%@  @itemx include(@var{:Pred}, @var{+Xs}, @var{+Ys}, @var{+Zs}, @var{?SubList})
%@  @PLXindex {include/[3,4,5] (lists)}
%@  succeeds when @var{SubList} is the sublist of @var{Xs} containing all the
%@  elements @var{Xi[,Yi[,Zi]]} for which @var{Pred(Xi[,Yi[,Zi]])} is @emph{true}.  That is, it retains
%@  all the elements satisfying @var{Pred}.
%@  @var{Xs}, @var{Ys} or @var{Zs} should be a proper list.  
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  include(Pred, Xs, News) :-
%@  	(   foreach(X,Xs),
%@  	    fromto(News,S0,S,[]),
%@  	    param(Pred)
%@  	do  (call(Pred,X) -> S0 = [X|S] ; S0 = S)
%@  	).
%@  
%@  include(Pred, Xs, News) :-
%@  	(   foreach(X,Xs),
%@  	    fromto(News,S0,S,[]),
%@  	    param(Pred)
%@  	do  (call(Pred,X) -> S0 = [X|S] ; S0 = S)
%@  	).
%@  
%@  include(Pred, Xs, Ys, News) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(Y,Ys),
%@  	    fromto(News,S0,S,[]),
%@  	    param(Pred)
%@  	do  (call(Pred,X,Y) -> S0 = [X|S] ; S0 = S)
%@  	).
%@  
%@  include(Pred, Xs, Ys, Zs, News) :-
%@  	(   foreach(X,Xs),
%@  	    foreach(Y,Ys),
%@  	    foreach(Z,Zs),
%@  	    fromto(News,S0,S,[]),
%@  	    param(Pred)
%@  	do  (call(Pred,X,Y,Z) -> S0 = [X|S] ; S0 = S)
%@  	).
%@  @end group
%@  @end example

include(Pred, Xs, News) :-
	(   foreach(X,Xs),
	    fromto(News,S0,S,[]),
	    param(Pred)
	do  (call(Pred,X) -> S0 = [X|S] ; S0 = S)
	).

include(Pred, Xs, Ys, News) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    fromto(News,S0,S,[]),
	    param(Pred)
	do  (call(Pred,X,Y) -> S0 = [X|S] ; S0 = S)
	).

include(Pred, Xs, Ys, Zs, News) :-
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(Z,Zs),
	    fromto(News,S0,S,[]),
	    param(Pred)
	do  (call(Pred,X,Y,Z) -> S0 = [X|S] ; S0 = S)
	).

%@  @item partition(@var{:Pred}, @var{+List}, @var{?Less}, @var{?Equal}, @var{?Greater})
%@  @PLXindex {partition/5 (lists)}
%@  is a relative of @code{include/3} and @code{exclude/3} which has some pretensions
%@  to being logical.  For each @var{X} in @var{List}, we call @var{Pred(X,R)}, and route
%@  @var{X} to @var{Less}, @var{Equal}, or @var{Greater} according as @var{R} is @code{<}, @code{=}, or @code{>} .
%   Note that the argument order of the built-in predicate compare/3 is
%   not what we want here, so you will typically have to write your own
%   interface predicate (see library(order) for some examples).

partition(Pred, List, Less, Equal, Greater) :-
	partition_1(List, Pred, Less, Equal, Greater).

partition_1([], _, [], [], []).
partition_1([X|Xs], Pred, L, E, G) :-
	call(Pred, X, R),
	partition_1(R, X, L, E, G, Xs, Pred).

partition_1(<, X, [X|L], E, G, Xs, Pred) :-
	partition_1(Xs, Pred, L, E, G).
partition_1(=, X, L, [X|E], G, Xs, Pred) :-
	partition_1(Xs, Pred, L, E, G).
partition_1(>, X, L, E, [X|G], Xs, Pred) :-
	partition_1(Xs, Pred, L, E, G).



%@  @item group(@var{:Pred}, @var{+List}, @var{?Front}, @var{?Back})
%@  @PLXindex {group/[3,4,5] (lists)}
%@  is true when @code{append(Front, Back, List), maplist(Pred, Front)},
%@  and @var{Front} is as long as possible.

group(Pred, [Head|Tail], Front, Back) :-
	call(Pred, Head),
	!,
	Front = [Head|Rest],
	group(Pred, Tail, Rest, Back).
group(_, Back, [], Back).


%@  @item group(@var{:Pred}, @var{+Key}, @var{+List}, @var{?Front}, @var{?Back})
%@  is true when @code{append(Front, Back, List), maplist(call(Pred,Key), Front)},
%@  and @var{Front} is as long as possible.  Strictly speaking we don't need it;
%@  @code{group(call(Pred,Key), List, Front, Back)} would do just as well.

group(Pred, Key, [Head|Tail], Front, Back) :-
	call(Pred, Key, Head),
	!,
	Front = [Head|Rest],
	group(Pred, Key, Tail, Rest, Back).
group(_, _, Back, [], Back).


%@  @item group(@var{:Pred}, @var{+List}, @var{?ListOfLists})
%@  is true when @code{append(ListOfLists, List)}, each element of @var{ListOfLists}
%@  has the form @var{[Head|Tail]} such that @code{group(Pred, Head, Tail, Tail, [])},
%@  and each element of @var{ListOfLists} is as long as possible.  For example,
%@  if you have a keysorted list, and define @code{same_key(K-_, K-_)}, then
%@  @code{group(same_key, List, Buckets)} will divide @var{List} up into @var{Buckets} of
%@  pairs having the same key.

group(Pred, List, ListOfLists) :-
	group1(List, ListOfLists, Pred).

group1([], [], _).
group1([Head|Tail], [[Head|Front]|ListOfLists], Pred) :-
	group(Pred, Head, Tail, Front, Back),
	group1(Back, ListOfLists, Pred).

%   Package: ordered
%   Author : Richard A. O'Keefe
%   Updated: 01 Jun 1989
%   Purpose: Define the "ordered" predicates.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

/*  BEWARE!!
    This is still an unsupported package.
    In snapshot 22 (12-Jan-88) the first two arguments of the
    {max,min}_member/[2,3] predicates were swapped so that the
    element now precedes this list.  This makes the argument
    order of these predicates compatible with basics:member/2,
    and more than that, it makes them compatible with the
    select_{max,min}/[3,4] predicates in this very package.
	max_member(Pred, Member, List) :- select_max(Pred, Member, List, _).
    and so on.
*/

/* pred
	ordered(list(T)),
	    ordered_1(list(T), T),
	ordered(void(T,T), list(T)),
	    ordered_1(list(T), T, void(T,T)),
	max_member(S, list(S)),
	    max_member_1(list(S), S, S),
	min_member(S, list(S)),
	    min_member_1(list(S), S, S),
	max_member(void(T,T), T, list(T)),
	    max_member_1(list(T), T, T, void(T,T)),
	min_member(void(T,T), T, list(T)),
	    min_member_1(list(T), T, T, void(T,T)),
	select_min(S, list(S), list(S)),
	    sel_min_trm(list(S), list(S), S),
		sel_min_trm(list(S), list(S), S, list(S), list(S)),
	select_max(S, list(S), list(S)),
	    sel_max_trm(list(S), list(S), S),
		sel_max_trm(list(S), list(S), S, list(S), list(S)),
	select_min(void(T,T), T, list(T), list(T)),
	    sel_min_gen(list(T), list(T), void(T,T), T),
		sel_min_gen(list(T), list(T), T, void(T,T), list(T), list(T)),
	select_max(void(T,T), T, list(T), list(T)),
	    sel_max_gen(list(T), list(T), void(T,T), T),
		sel_max_gen(list(T), list(T), T, void(T,T), list(T), list(T)).
*/



%@  @item ordered(@var{+List})
%@  @PLXindex {ordered/[1,2] (lists)}
%@  is true when @var{List} is a list of terms @var{[T1,T2,...,Tn]} such that
%@  for all @var{k} in @var{2..n} @var{Tk-1} @code{@@=<} @var{Tk}, i.e. @var{T1} @code{@@=<} @var{T2} @code{@@=<} @var{T3} ...
%@  The output of @code{keysort/2} is always ordered, and so is that of
%@  @code{sort/2}.  Beware: just because a list is ordered does not mean
%@  that it is the representation of an ordered set; it might contain
%@  duplicates.  
%   E.g. L = [1,2,2,3] & sort(L,M) => ordered(L) & M\=L.

ordered([]).
ordered([Head|Tail]) :-
	ordered_1(Tail, Head).

ordered_1([], _).
ordered_1([Head|Tail], Left) :-
	Left @=< Head,
	ordered_1(Tail, Head).



%@  @item ordered(@var{+P}, @var{+[T1,T2,...,Tn]})
%@  is true when @var{P(T1,T2) & P(T2,T3) & ...}   That is, if you take
%@  @var{P} as a "comparison" predicate like @code{@@=<}, the list is ordered.
%@  This is good for generating prefixes of sequences,
%@  e.g. @code{L = [1,_,_,_,_], ordered(times(2), L)} yields @code{L = [1,2,4,8,16]}.

ordered(P, List) :- ordered_0(List, P).

ordered_0([], _).
ordered_0([Head|Tail], Relation) :-
	ordered_1(Tail, Head, Relation).

ordered_1([], _, _).
ordered_1([Head|Tail], Left, Relation) :-
	call(Relation, Left, Head),
	ordered_1(Tail, Head, Relation).



/*  Here we define four predicates:

	min_member(Minimum <- Set)
	max_member(Maximum <- Set)

	min_member(Pred, Minimum <- Set)
	max_member(Pred, Maximum <- Set)

    Set is always a list, Minimum (or Maximum) is a member of that
    list, and it is a least (greatest) element under the standard
    ordering on terms (under the '@=<'-like ordering Pred).

    Before 12-Jan-88, the argument order was not the same as the
    argument order of member/2 or the select_{max,min}/[3,4]
    predicates.  It is now.  The old order was more beautiful,
    but consistency is a greater beauty.
*/

%@  @item max_member(@var{?Xmax}, @var{+[X1,...,Xn]})
%@  @PLXindex {max_member/[2,3] (lists)}
%@  unifies @var{Xmax} with the maximum (in the sense of @code{@@=<}) of @var{X1},...,@var{Xn}.
%@  The list should be proper. If it is empty, the predicate fails quietly.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  max_member(Maximum, [Head|Tail]) :-
%@  	(   foreach(X,Tail),
%@  	    fromto(Head,M0,M,Maximum)
%@  	do  (X@@=<M0 -> M = M0 ; M = X)
%@  	).
%@  @end group
%@  @end example

max_member(Maximum, [Head|Tail]) :-
	(   foreach(X,Tail),
	    fromto(Head,M0,M,Maximum)
	do  (X@=<M0 -> M = M0 ; M = X)
	).

%@  @item min_member(@var{?Xmin}, @var{+[X1,...,Xn]})
%@  @PLXindex {min_member/[2,3] (lists)}
%@  unifies @var{Xmin} with the minimum (in the sense of @code{@@=<}) of @var{X1},...,@var{Xn}.
%@  The list should be proper. If it is empty, the predicate fails quietly.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  min_member(Minimum, [Head|Tail]) :-
%@  	(   foreach(X,Tail),
%@  	    fromto(Head,M0,M,Minimum)
%@  	do  (M0@@=<X -> M = M0 ; M = X)
%@  	).
%@  @end group
%@  @end example

min_member(Minimum, [Head|Tail]) :-
	(   foreach(X,Tail),
	    fromto(Head,M0,M,Minimum)
	do  (M0@=<X -> M = M0 ; M = X)
	).


%@  @item max_member(@var{:P}, @var{?Xmax}, @var{+[X1,...,Xn]})
%@  unifies @var{Xmax} with the maximum element of @var{[X1,...,Xn]}, as defined
%@  by the comparison predicate @var{P}, which should act like @code{@@=<} .
%@  The list should be proper. If it is empty, the predicate fails quietly.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  max_member(Pred, Maximum, [Head|Tail]) :-
%@  	(   foreach(X,Tail),
%@  	    fromto(Head,M0,M,Maximum),
%@  	    param(Pred)
%@  	do  (call(Pred,X,M0) -> M = M0 ; M = X)
%@  	).
%@  @end group
%@  @end example

max_member(Pred, Maximum, [Head|Tail]) :-
	(   foreach(X,Tail),
	    fromto(Head,M0,M,Maximum),
	    param(Pred)
	do  (call(Pred,X,M0) -> M = M0 ; M = X)
	).

%@  @item min_member(@var{:P}, @var{?Xmin}, @var{+[X1,...,Xn]})
%@  unifies @var{Xmin} with the minimum element of @var{[X1,...,Xn]}, as defined
%@  by the comparison predicate @var{P}, which should act like @code{@@=<} .
%@  The list should be proper. If it is empty, the predicate fails quietly.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  min_member(Pred, Minimum, [Head|Tail]) :-
%@  	(   foreach(X,Tail),
%@  	    fromto(Head,M0,M,Minimum),
%@  	    param(Pred)
%@  	do  (call(Pred,M0,X) -> M = M0 ; M = X)
%@  	).
%@  @end group
%@  @end example

min_member(Pred, Minimum, [Head|Tail]) :-
	(   foreach(X,Tail),
	    fromto(Head,M0,M,Minimum),
	    param(Pred)
	do  (call(Pred,M0,X) -> M = M0 ; M = X)
	).


/*  Here we define four predicates:

	select_min(Minimum, Set, Residue)
	select_max(Maximim, Set, Residue)

	select_min(Pred, Minimum, Set, Residue)
	select_max(Pred, Maximum, Set, Residue)

    The general idea is that they select an item out of a list in the
    style of select/3 (see library(sets)), and that item is the left-
    most (minimum,maximum) using the (standard term ordering @=<, the
    ordering defined by Pred).  An earlier version of this file used
    arithmetic ordering as the default; this was changed to use term
    ordering for consistency with orderd/2 and library(ordprefix).

    Set is always a list, Minimum (or Maximum) is a member of that
    list, and Residue is the whole of Set except for the one element.
    We could have defined select_min/3 this way:

    select_min(Minimum, Set, Residue) :-
	    append(Front, [Minimum|Back], Set),
	    append(Front, Back, Residue),
	    forall(member(X,Front), \+(X =< Minimum)),
	    forall(member(X,Back),     Minimum =< X ).

    That is, Minimum is less than every element preceding it in Set,
    and less than or equal to every element following it in Set.
    The two predicates with a Pred argument use that instead of =< .
    The advantage of this code is that it uses O(N) comparisons,
    whereas the more obvious code could do O(N^2).

    With these routines, we could define selection sort:

	ssort([], []).
	ssort(List, [Minimum|Sorted]) :-
		select_min(Minimum, List, Rest),
		ssort(Rest, Sorted).

    Considering the requirements for this kind of use led directly
    to the code in this file.
*/


%@  @item select_min(@var{?Element}, @var{+Set}, @var{?Residue})
%@  @PLXindex {select_min/[3,4] (lists)}
%@  unifies @var{Element} with the smallest (in the sense of @code{@@=<}) element
%@  of @var{Set}, and @var{Residue} with a list of all the other elements.

select_min(Element, Set, Residue) :-
	sel_min_trm(Set, Residue, Element).


sel_min_trm([Head|Tail], Residue, Element) :-
	sel_min_trm([Head|Tail], Residue, Head, List1, Residue1),
	!,
	sel_min_trm(List1, Residue1, Element).
sel_min_trm([Head|Tail], Tail, Head).


sel_min_trm([], _, _, _, _) :- !, fail.
sel_min_trm([Head|Tail], [Head|Residue], Current, List1, Residue1) :-
	Current @=< Head,
	!,
	sel_min_trm(Tail, Residue, Current, List1, Residue1).
sel_min_trm(List, Residue, _, List, Residue).  % non-empty list!



%@  @item select_min(@var{:Pred}, @var{?Element}, @var{+Set}, @var{?Residue})
%@  find the least @var{Element} of @var{Set}, i.e. @var{Pred(Element,X)} for all @var{X} in @var{Set}.

select_min(Pred, Element, Set, Residue) :-
	sel_min_gen(Set, Residue, Pred, Element).


sel_min_gen([Head|Tail], Residue, Pred, Element) :-
	sel_min_gen([Head|Tail], Residue, Head, Pred, List1, Residue1),
	!,
	sel_min_gen(List1, Residue1, Pred, Element).
sel_min_gen([Head|Tail], Tail, _, Head).


sel_min_gen([], _, _, _, _, _) :- !, fail.
sel_min_gen([Head|Tail], [Head|Residue], Current, Pred, List1, Residue1) :-
	call(Pred, Current, Head),
	!,
	sel_min_gen(Tail, Residue, Current, Pred, List1, Residue1).
sel_min_gen(List, Residue, _, _, List, Residue).  % non-empty list!



%@  @item select_max(@var{?Element}, @var{+Set}, @var{?Residue})
%@  @PLXindex {select_max/[3,4] (lists)}
%@  unifies @var{Element} with the (leftmost) maximum element of the @var{Set},
%@  and @var{Residue} to the other elements in the same order.


select_max(Element, Set, Residue) :-
	sel_max_trm(Set, Residue, Element).


sel_max_trm([Head|Tail], Residue, Element) :-
	sel_max_trm([Head|Tail], Residue, Head, List1, Residue1),
	!,
	sel_max_trm(List1, Residue1, Element).
sel_max_trm([Head|Tail], Tail, Head).


sel_max_trm([], _, _, _, _) :- !, fail.
sel_max_trm([Head|Tail], [Head|Residue], Current, List1, Residue1) :-
	Head @=< Current,
	!,
	sel_max_trm(Tail, Residue, Current, List1, Residue1).
sel_max_trm(List, Residue, _, List, Residue).  % non-empty list!



%@  @item select_max(@var{:Pred}, @var{?Element}, @var{+Set}, @var{?Residue})
%@  find the greatest @var{Element} of @var{Set}, i.e. @var{Pred(X,Element)} for all @var{X} in @var{Set}.

select_max(Pred, Element, Set, Residue) :-
	sel_max_gen(Set, Residue, Pred, Element).


sel_max_gen([Head|Tail], Residue, Pred, Element) :-
	sel_max_gen([Head|Tail], Residue, Head, Pred, List1, Residue1),
	!,
	sel_max_gen(List1, Residue1, Pred, Element).
sel_max_gen([Head|Tail], Tail, _, Head).


sel_max_gen([], _, _, _, _, _) :- !, fail.
sel_max_gen([Head|Tail], [Head|Residue], Current, Pred, List1, Residue1) :-
	call(Pred, Head, Current),
	!,
	sel_max_gen(Tail, Residue, Current, Pred, List1, Residue1).
sel_max_gen(List, Residue, _, _, List, Residue).  % non-empty list!

%   Package: ordprefix
%   Author : Richard A. O'Keefe
%   Updated: 10 Nov 1987
%   Purpose: Extract ordered prefixes from sequences
%   SeeAlso: ordered.pl; ordsets.pl

%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.


/* mode
	decreasing_prefix(+, ?, ?),
	decreasing_prefix(+, +, ?, ?),
	first_precedes(+, +),
	first_precedes(+, +, +),
	increasing_prefix(+, ?, ?),
	increasing_prefix(+, +, ?, ?),
	precedes_first(+, +),
	precedes_first(+, +, +).

:- pred
	decreasing_prefix(list(T), list(T), list(T)),
	decreasing_prefix(void(T,T), list(T), list(T), list(T)),
	first_precedes(T, list(T)),
	first_precedes(void(T,T), T, list(T)),
	increasing_prefix(list(T), list(T), list(T)),
	increasing_prefix(void(T,T), list(T), list(T), list(T)),
	precedes_first(T, list(T)),
	precedes_first(void(T,T), T, list(T)).
*/


/*  The routines in this package are intended for finding runs
    in sequences.
*/

%@  @item increasing_prefix(@var{?Sequence}, @var{?Prefix}, @var{?Suffix})
%@  is true when @code{append(Prefix, Suffix, Sequence)}
%@  and @var{Prefix}, together with the first element of @var{Suffix},
%@  forms a monotone non-decreasing sequence, and
%@  no longer Prefix will do.  Pictorially,
%@  @example
%@  Sequence = [x1,...,xm,xm+1,...,xn]
%@  Prefix   = [x1,...,xm]
%@  Suffix     = [xm+1,...,xn]
%@  x1 @=< x2 @=< ... @=< xm @=< xm+1
%@  not xm+1 @=< xm+2
%@  @end example
%@  This is perhaps a surprising definition; you might expect
%@  that the first element of @var{Suffix} would be included in @var{Prefix}.
%@  However, this way, it means that if Sequence is a strictly
%@  decreasing sequence, the @var{Prefix} will come out empty.
%   The routine is part of a package for counting runs.

increasing_prefix([Elem|Sequence], [Elem|Prefix], Tail) :-
	precedes_first(Elem, Sequence),
	!,
	increasing_prefix(Sequence, Prefix, Tail).
increasing_prefix(Sequence, [], Sequence).

precedes_first(X, [Y|_]) :-
	X @=< Y.


%@  @item increasing_prefix(@var{:Order}, @var{?Sequence}, @var{?Prefix}, @var{?Suffix})
%@  @PLXindex {increasing_prefix/[3,4] (lists)}
%@  is the same as @code{increasing_prefix/3}, except that it uses the
%@  binary relation @var{Order} in place of @code{@@=<}.

increasing_prefix(Order, [Elem|Sequence], [Elem|Prefix], Tail) :-
	precedes_first(Order, Elem, Sequence),
	!,
	increasing_prefix(Order, Sequence, Prefix, Tail).
increasing_prefix(_, Sequence, [], Sequence).

precedes_first(Order, X, [Y|_]) :-
	call(Order, X, Y).


%@  @item decreasing_prefix(@var{?Sequence}, @var{?Prefix}, @var{?Suffix})
%@  @itemx decreasing_prefix(@var{:Order}, @var{?Sequence}, @var{?Prefix}, @var{?Suffix})
%@  @PLXindex {decreasing_prefix/[3,4] (lists)}
%@  is the same, except it looks for a decreasing prefix.
%@  The order is the converse of the given order.  That
%@  is, where @code{increasing_prefix/[3,4]} check @var{X(R)Y}, these
%@  routines check @var{Y(R)X}.

decreasing_prefix([Elem|Sequence], [Elem|Prefix], Tail) :-
	first_precedes(Elem, Sequence),
	!,
	decreasing_prefix(Sequence, Prefix, Tail).
decreasing_prefix(Sequence, [], Sequence).

first_precedes(X, [Y|_]) :-
	Y @=< X.


decreasing_prefix(Order, [Elem|Sequence], [Elem|Prefix], Tail) :-
	first_precedes(Order, Elem, Sequence),
	!,
	decreasing_prefix(Order, Sequence, Prefix, Tail).
decreasing_prefix(_, Sequence, [], Sequence).

first_precedes(Order, X, [Y|_]) :-
	call(Order, Y, X).


/*  As an example of these predicates, here is how to tell
    whether a sequence is unimodal:

	unimodal(Seq) :-
		increasing_prefix(Seq, _, Mid),
		decreasing_prefix(Mid, _, [_]).
*/
%   Package: clump
%   Author : Richard A. O'Keefe
%   Updated: 28 Aug 1989
%   Purpose: Group adjacent related elements of lists

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1989, Quintus Computer Systems, Inc.  All rights reserved.



%@  @item clumps(@var{+Items}, @var{-Clumps})
%@  @PLXindex {clumps/2 (lists)}
%@  is true when @var{Clumps} is a list of lists such that
%@  @itemize @bullet
%@  @item @code{append(Clumps, Items)}
%@  @item for each @var{Clump} in @var{Clumps}, all the elements of @var{Clump} are identical (@code{==})
%@  @end itemize
%@  @var{Items} must be a proper list of terms for which sorting would have been
%@  sound.  In fact, it usually is the result of sorting.

clumps([], []).
clumps([Item|Items], [Clump|Clumps]) :-
	clumps(Items, Item, Clump, Clumps).


clumps([], Key, [Key], []).
clumps([Item|Items], Key, [Key|Clump], Clumps) :-
	compare(O, Key, Item),
	clumps(O, Item, Clump, Clumps, Items).


clumps(=, Item, Clump, Clumps, Items) :-
	clumps(Items, Item, Clump, Clumps).
clumps(<, Item, [], [Clump|Clumps], Items) :-
	clumps(Items, Item, Clump, Clumps).
clumps(>, Item, [], [Clump|Clumps], Items) :-
	clumps(Items, Item, Clump, Clumps).



%@  @item keyclumps(@var{+Pairs}, @var{?Clumps})
%@  @PLXindex {keyclumps/2 (lists)}
%@  is true when @var{Pairs} is a list of pairs and @var{Clumps} a list of lists such that
%@  @itemize @bullet
%@  @item @code{append(Clumps, Pairs)}
%@  @item for each @var{Clump} in @var{Clumps}, all of the @var{Key-Value} pairs in @var{Clump} have
%@  identical (@code{==}) @var{Keys}.
%@  @end itemize
%@  @var{Pairs} must be a proper list of pairs for which keysorting would have
%@  been sound.  In fact, it usually is the result of keysorting.

keyclumps([], []).
keyclumps([Pair|Pairs], [Clump|Clumps]) :-
	keyclumps(Pairs, Pair, Clump, Clumps).


keyclumps([], Prev, [Prev], []).
keyclumps([Pair|Pairs], Prev, [Prev|Clump], Clumps) :-
	keycompare(O, Prev, Pair),
	keyclumps(O, Pair, Clump, Clumps, Pairs).


keyclumps(=, Pair, Clump, Clumps, Pairs) :-
	keyclumps(Pairs, Pair, Clump, Clumps).
keyclumps(<, Pair, [], [Clump|Clumps], Pairs) :-
	keyclumps(Pairs, Pair, Clump, Clumps).
keyclumps(>, Pair, [], [Clump|Clumps], Pairs) :-
	keyclumps(Pairs, Pair, Clump, Clumps).


keycompare(O, K1-_, K2-_) :-
	compare(O, K1, K2).



%@  @item clumped(@var{+Items}, @var{?Counts})
%@  @PLXindex {clumped/2 (lists)}
%@  is true when @var{Counts} is a list of @var{Item-Count} pairs such that
%@  if @code{clumps(Items, Clumps)}, then each @var{Item-Count} pair in @var{Counts} corresponds
%@  to an element @var{[Item/*1*/,...,Item/*Count*/]} of @var{Clumps}.
%@  @var{Items} must be a proper list of terms for which sorting would have been
%@  sound.  In fact, it usually is the result of sorting.

clumped([], []).
clumped([Item|Items], Clumps) :-
	clumped(Items, Item, 1, Clumps).


clumped([], Key, Count, [Key-Count]).
clumped([Item|Items], Key, Count0, Counts) :-
	compare(O, Key, Item),
	clumped(O, Item, Count0, Counts, Items, Key).


clumped(=, Item, Count0, Counts, Items, _) :-
	Count1 is Count0+1,
	clumped(Items, Item, Count1, Counts).
clumped(<, Item, Count, [Key-Count|Counts], Items, Key) :-
	clumped(Items, Item, 1, Counts).
clumped(>, Item, Count, [Key-Count|Counts], Items, Key) :-
	clumped(Items, Item, 1, Counts).



%@  @item keyclumped(@var{+Pairs}, @var{?Groups})
%@  @PLXindex {keyclumped/2 (lists)}
%@  is true when @var{Pairs} is a list of @var{Key-Item} pairs and
%@  @var{Groups} is a list of @var{Key-Items} pairs such that
%@  if @code{keyclumps(Pairs, Clumps)}, then for each @var{K-[I1,...,In]} pair in @var{Groups}
%@  there is a @var{[K-I1,...,K-In]} clump in @var{Clumps}.
%@  @var{Pairs} must be a proper list of pairs for which keysorting would have
%@  been sound.  In fact, it usually is the result of keysorting.

keyclumped(Pairs, Groups) :-	% [MC] 4.3.2
	prolog:keyclumped(Pairs, Groups).

%HIDE% keyclumped([], []).
%HIDE% keyclumped([Key-Item|Pairs], [Key-[Item|Items]|Groups]) :-
%HIDE% 	keyclumped(Pairs, Key, Items, Groups).
%HIDE% 
%HIDE% 
%HIDE% keyclumped([], _, [], []).
%HIDE% keyclumped([Next-Item|Pairs], Key, Items, Groups) :-
%HIDE% 	compare(O, Key, Next),
%HIDE% 	keyclumped(O, Key, Items, Groups, Next, Item, Pairs).
%HIDE% 
%HIDE% 
%HIDE% keyclumped(=, Key, [Item|Items], Groups, _, Item, Pairs) :-
%HIDE% 	keyclumped(Pairs, Key, Items, Groups).
%HIDE% keyclumped(<, _, [], [Key-[Item|Items]|Groups], Key, Item, Pairs) :-
%HIDE% 	keyclumped(Pairs, Key, Items, Groups).
%HIDE% keyclumped(>, _, [], [Key-[Item|Items]|Groups], Key, Item, Pairs) :-
%HIDE% 	keyclumped(Pairs, Key, Items, Groups).


% [PM] Docs must end before explicit end_of_file
%@  @end table

end_of_file.


/*  TEST CODE  */

test :-
	test([3,1,4,1,5,9]).

test(L) :-
	max_member(Max1, L), write(max1=Max1), nl,
	min_member(Min1, L), write(min1=Min1), nl,
	max_member(=<, Max2, L), write(max2=Max2), nl,
	min_member(=<, Min2, L), write(min2=Min2), nl,
	select_max(Max3, L, Res1), write((max3=Max3,res1=Res1)), nl,
	select_min(Min3, L, Res3), write((min3=Min3,res3=Res3)), nl,
	select_max(=<, Max4, L, Res2), write((max4=Max4,res2=Res2)), nl,
	select_min(=<, Min4, L, Res4), write((min4=Min4,res4=Res4)), nl.

t1 :-
	clumps([m,i,s,s,i,s,s,i,p,p,i], X),
	write(mississippi=X).

t2 :-
	clumped([m,i,s,s,i,s,s,i,p,p,i], X),
	write(mississippi=X).

t3 :-
	keyclumps([m-1,i-2,s-3,s-4,i-5,s-6,s-7,i-8,p-9,p-a,i-b], X),
	write(mississippi=X).

t4 :-
	keyclumped([m-1,i-2,s-3,s-4,i-5,s-6,s-7,i-8,p-9,p-a,i-b], X),
	write(mississippi=X).

