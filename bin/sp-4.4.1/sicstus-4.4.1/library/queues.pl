%   Package: newqueues
%   Author : Richard A. O'Keefe
%   Updated: 23 Aug 1990
%   Defines: queue operations (using Mark Johnson's method)
%   SeeAlso: library(mapqueue), library(listparts).

%   NOTE: This library package is new in release 3.0.  The old
%	  queues package remains available as library(queues).

%   Copyright (C) 1990, Quintus Computer Systems, Inc.  All rights reserved.

:- module(queues, [
	append_queue/3,		% List x Queue -> Queue
	empty_queue/1,		% -> Queue
	singleton_queue/2,	% Item -> Queue
	is_queue/1,		% Queue ->
	list_queue/2,		% List <-> Queue
	portray_queue/1,	% Queue ->
	queue_append/3,		% Queue x List -> Queue
	queue_cons/3,		% Item x Queue -> Queue
	queue_head/2,		% Queue -> Item
	queue_last/2,		% Queue -> Queue
	queue_last/3,		% Queue x Item -> Queue
	queue_length/2,		% Queue <-> Integer
	queue_list/2,		% Queue <-> List
	queue_member/2,		% Item x Queue
	queue_memberchk/2,	% Item x Queue ->
	queue_tail/2,		% Queue -> Item
	map_queue/2,		% Pred x Queue ->
	map_queue/3,		% Pred x Queue x Queue
	map_queue_list/3,	% Pred x Queue x List
	map_list_queue/3,	% Pred x List x Queue
	some_queue/2,		% Pred x Queue
	some_queue/3,		% Pred x Queue x Queue
	somechk_queue/2,	% Pred x Queue ->
	somechk_queue/3		% Pred x Queue x Queue ->
   ]).

%@  This module provides an implementation of queues, where you can
%@  @itemize @bullet
%@  @item create an empty queue
%@  @item add an element at either end of a queue
%@  @item add a list of elements at either end of a queue
%@  @item remove an element from the front of a queue
%@  @item remove a list of elements from the front of a queue
%@  @item determine the length of a queue
%@  @item enumerate the elements of a queue
%@  @item recognise a queue
%@  @item print a queue nicely
%@  @end itemize
%@  The representation was invented by Mark Johnson of the Center for
%@  the Study of Language and Information.  All operations are fast.
%   The names of the predicates in this file are based on the names
%   for parts of sequences defined in library(listparts).

/*  This module is based on library(maplist).  Analogues of all the
    predicates in that file could be defined; I have chosen not to do
    that in this edition of library(mapqueue) but merely to show how
    such analogues are constructed.

    The three families of mapping predicates represente here are

	map_queue(P, Queue[X11,...,X1n], ..., Queue[Xm1,...,Xmn]) :-
	    P(X11, ..., Xm1),
	    ...
	    P(Xm1, ..., Xmn).

	some_queue(P, Queue[X11,...,X1n], Queue[Xm1,...,Xmn]) :-
	    P(X11, ..., Xm1)
	  ; ...
	  ; P(Xm1, ..., Xmn).

	somechk_queue(P, Queue[X11,...,X1n], Queue[Xm1,...,Xmn]) :-
	    P(X11, ..., Xm1) -> true
	  ; ...
	  ; P(Xm1, ..., Xmn) -> true.

    Ideally these predicates would exist for all N >= 2.

    There are also two "mixed" predicates
	map_queue_list(P, Queue[X11,...,X1n], [X21,...,X2n])
	map_list_queue(P, [X11,...,X1n], Queue[X21,...,X2n])
    to show how such mixed predicates might be coded.
*/

:- meta_predicate
	map_queue(1, ?),
	map_queue(2, ?, ?),
	map_queue_list(2, ?, ?),
	map_list_queue(2, ?, ?),
	some_queue(1, ?),
	some_queue(2, ?, ?),
	somechk_queue(1, ?),
	somechk_queue(2, ?, ?).

:- meta_predicate
        map_list_queue_1(?, -, ?, ?, 2),
        map_list_queue_2(?, -, ?, ?, 2),
        map_queue_list_1(?, ?, ?, ?, 2),
        map_queue_list_2(?, ?, ?, ?, 2),
        mapqueue(+, +, +, +, +, 2),
        mapqueue(+, +, +, 1),
        somechkqueue(+, +, 1),
        somechkqueue(+, +, ?, 2),
        somequeue(+, +, 1),
        somequeue(+, +, ?, 2).


:- mode
	empty_queue(?),
	is_queue(+),	
	    is_queue(+, +, +),
	list_queue(+, ?),
	    list_queue(+, ?, ?, ?),
	queue_list(+, ?),
	    queue_list(+, ?, ?, +),
	queue_head(+, ?),
	queue_tail(?, ?),
	queue_cons(?, ?, ?),
	queue_last(?, ?),
	    queue_last_1(?, ?, ?, ?),
	queue_last(+, ?, ?),
	queue_append(+, ?, ?),
	    queue_append(+, ?, ?, ?, ?),
	append_queue(+, ?, ?),
	    append_queue(+, ?, ?, ?, ?),
	queue_length(?, ?),
	    queue_length_1(?, ?, ?, +, -),
	    queue_length_2(+, ?, ?, ?),
	queue_member(?, +),
	    queue_member(+, +, ?),
	queue_memberchk(+, +),
	    queue_memberchk(+, +, +),
	portray_queue(+),
	    portray_queue(+, +, +).


:- mode
	map_queue(+, ?),
	    mapqueue(?, ?, ?, +),
	map_queue(+, ?, ?),
	    mapqueue(?, ?, ?, ?, ?, +),
	some_queue(+, +),
	    somequeue(+, +, +),
	some_queue(+, +, +),
	    somequeue(+, +, +, +),
	somechk_queue(+, +),
	    somechkqueue(+, +, +),
	somechk_queue(+, +, +),
	    somechkqueue(+, +, +, +).

/* type unary --> z | s(unary).
:- type queue(T) --> queue(unary,list(T),list(T)).
:- pred
	empty_queue(queue(T)),
	is_queue(queue(T)),
	    is_queue(unary, list(T), list(T)),
	list_queue(list(T), queue(T)),
	    list_queue(list(T), unary, list(T), list(T)),
	queue_list(queue(T), list(T)),
	    queue_list(unary, list(T), list(T), list(T)),
	queue_head(queue(T), T),
	queue_tail(queue(T), queue(T)),
	queue_cons(T, queue(T), queue(T)),
	queue_last(T, queue(T)),
	    queue_last_1(unary, list(T), list(T), T),
	queue_last(queue(T), T, queue(T)),
	queue_append(queue(T), list(T), queue(T)),
	    queue_append(list(T), list(T), list(T), unary, unary),
	append_queue(list(T), queue(T), queue(T)),
	    append_queue(list(T), list(T), list(T), unary, unary),
	queue_length(queue(T), integer),
	    queue_length_1(unary, list(T), list(T), integer, integer),
	    queue_length_2(integer, unary, list(T), list(T)),
	queue_member(T, queue(T)),
	    queue_member(unary, list(T), T),
	queue_memberchk(T, queue(T)),
	    queue_memberchk(unary, list(T), T),
	portray_queue(queue(T)),
	    portray_queue(unary, list(T), atom).
	map_queue(void(T), queue(T)),
	    mapqueue(unary, list(T), list(T), void(T)),
	map_queue(void(S,T), queue(S), queue(T)),
	    mapqueue(unary, list(S), list(S), list(T), list(T), void(S,T)),
	map_queue_list(void(S,T), queue(S), list(T)),
	    mapqueue(unary, list(S), list(S), list(T), list(T), void(S,T)),
	map_list_queue(void(S,T), list(S), queue(T)),
	    mapqueue(unary, list(S), list(S), list(T), list(T), void(S,T)),
	some_queue(void(T), queue(T)),
	    somequeue(unary, list(T), void(T)),
	some_queue(void(S,T), queue(S), queue(T)),
	    somequeue(unary, list(S), list(T), void(S,T)),
	somechk_queue(void(T), queue(T)),
	    somechkqueue(unary, list(T), void(T)),
	somechk_queue(void(S,T), queue(S), queue(T)),
	    somechkqueue(unary, list(S), list(T), void(S,T)).
*/



/*  In this module, a queue is represented as a triple
	queue(Size, Front, Back)
    where                       n    1 0
	Size is a unary number, s(...s(z)...)
	Front is a list [X1,...,Xn|Back]
	Back is a suffix of Front, and is normally a variable.
    The elements of the queue are the list difference Front\Back, that
    is all the elements starting at Front and stopping at Back.  There
    are Size of them.  Examples:

	queue(s(s(s(s(s(z))))), [a,b,c,d,e|Z], Z)    has elements a,b,c,d,e
	queue(s(s(s(z))),       [a,b,c,d,e], [d,e])  has elements a,b,c
	queue(z, Z, Z)                               has no elements
	queue(z, [1,2,3], [1,2,3])                   has no elements
*/

%@
%@  Exported predicates:
%@  
%@  @table @code

%@  @item empty_queue(@var{?Queue})
%@  @PLXindex {empty_queue/1 (queues)}
%@  is true when @var{Queue} represents an empty queue.  It can be used to
%@  test whether an existing queue is empty or to make a new empty queue.

empty_queue(queue(z,L,L)).


%@  @item singleton_queue(@var{?X}, @var{?Queue})
%@  @PLXindex {singleton_queue/2 (queues)}
%@  is true when @var{Queue} is a queue with just one element @var{X}.

singleton_queue(X, queue(s(0),[X|B],B)).



%@  @item portray_queue(@var{+Queue})
%@  @PLXindex {portray_queue/1 (queues)}
%@  writes a queue out in a pretty form, as @var{Queue[elements]}.  This form
%@  cannot be read back in, it is just supposed to be readable.  While it
%@  is meant to be called only when @code{is_queue(@var{Queue})} has been established,
%@  as by @code{user:portray(Q) :- is_queue(Q), !, portray_queue(Q)}.
%@  it is also meant to work however it is called.

portray_queue(queue(Size,Front,_)) :-
	write('Queue['),
	portray_queue(Size, Front, '').

portray_queue(z, _, _) :-
	put_code(0']).
portray_queue(s(N), [X|Front], Sep) :-
	write(Sep), print(X),
	portray_queue(N, Front, ',').


%@  @item is_queue(@var{+Queue})
%@  @PLXindex {is_queue/1 (queues)}
%@  is true when @var{Queue} is a queue.  The elements of @var{Queue} do not have
%@  to be instantiated, and the @var{Back} of the @var{Queue} may or may not be.
%@  It can only be used to recognise queues, not to generate them.
%@  To generate queues, use @code{queue_length(@var{Queue}, _)}.

is_queue(queue(Size,Front,Back)) :-
	is_queue(Size, Front, Back).

is_queue(X, _, _) :- var(X), !, fail.		% catch and reject variables
is_queue(z, Front, Back) :-
	Front == Back.
is_queue(s(N), [_|Front], Back) :-
	is_queue(N, Front, Back).



/*  The following five operations correspond precisely to the similarly
    named operations on lists.  See library(listparts).
*/

%@  @item queue_head(@var{+Queue}, @var{-Head})
%@  @PLXindex {queue_head/2 (queues)}
%@  is true when @var{Head} is the first element of the given @var{Queue}.  It does
%@  not remove @var{Head} from @var{Queue}; @var{Head} is still there afterwards.  It can
%@  only be used to find @var{Head}, it cannot be used to make a @var{Queue}.

queue_head(queue(s(_),[Head|_],_), Head).



%@  @item queue_tail(@var{?Queue}, @var{?Tail})
%@  @PLXindex {queue_tail/2 (queues)}
%@  is true when @var{Queue} and @var{Tail} are both queues and @var{Tail} contains all the
%@  elements of @var{Queue} except the first.  Note that @var{Queue} and @var{Tail} share
%@  structure, so that you can add elements at the back of only one of them.
%@  It can solve for either argument given the other.

queue_tail(queue(s(N),[_|Front],Back), queue(N,Front,Back)).



%@  @item queue_cons(@var{?Head}, @var{?Tail}, @var{?Queue})
%@  @PLXindex {queue_cons/3 (queues)}
%@  is true when @var{Head} is the head of @var{Queue} and @var{Tail} is the tail of @var{Queue},
%@  that is, when @var{Tail} and @var{Queue} are both queues, and the elements of the
%@  @var{Queue} are @var{Head} followed by the elements of @var{Tail} in order.  It can be
%@  used in either direction, so
%@  @example
%@      queue_cons(+Head, +Q0, -Q)      adds Head to Q0 giving Q
%@      queue_cons(-Head, -Q, +Q0)      removes Head from Q0 giving Q
%@  @end example

queue_cons(Head, queue(N,Front,Back), queue(s(N),[Head|Front],Back)).



%@  @item queue_last(@var{?Last}, @var{?Queue})
%@  @PLXindex {queue_last/[2,3] (queues)}
%@  is true when @var{Last} is the last element currently in @var{Queue}.  It does
%@  not remove @var{Last} from @var{Queue}; it is still there.  This can be used to
%@  generate a non-empty @var{Queue}.  The cost is @var{O(|Queue|)}.

queue_last(Last, queue(s(N),Front,Back)) :-
	queue_last_1(N, Front, Back, Last).

queue_last_1(z, [Last|Back], Back, Last).
queue_last_1(s(N), [_|Front], Back, Last) :-
	queue_last_1(N, Front, Back, Last).



%@  @item queue_last(@var{+Fore}, @var{+Last}, @var{-Queue})
%@  is true when @var{Fore} and @var{Queue} are both lists and the elements of @var{Queue}
%@  are the elements of @var{Fore} in order followed by @var{Last}.  This is the
%@  operation which adds an element at the end of @var{Fore} giving @var{Queue};  it
%@  is not reversible, unlike @code{queue_cons/3}, and it side-effects @var{Fore},
%@  again unlike @code{queue_cons/3}.  
%   The normal use is
%	queue_last(+Q0, +Last, -Q)	add Last to Q0 giving Q

queue_last(queue(N,Front,[Last|Back]), Last, queue(s(N),Front,Back)).



%@  @item append_queue(@var{?List}, @var{?Queue0}, @var{?Queue})
%@  @PLXindex {append_queue/3 (queues)}
%@  is true when @var{Queue} is obtained by appending the elements of @var{List}
%@  in order at the front of @var{Queue0}, e.g.
%@  @code{append_queue([a,b,c], Queue[d,e], Queue[a,b,c,d,e])}.  Use
%@  @example
%@      append_queue([+X1,...,+Xn], +Q0, -Q) to add X1,...,Xn to Q0 giving Q
%@      append_queue([-X1,...,-Xn], -Q, +Q0) to take X1...Xn from Q0 giving Q
%@  @end example
%@  The cost is @var{O(n)} and the operation is pure.

append_queue(List, queue(N0,F0,B), queue(N1,F1,B)) :-
	append_queue(List, F0, F1, N0, N1).

append_queue([], F, F, N, N).
append_queue([H|T], F, [H|R], N0, s(N1)) :-
	append_queue(T, F, R, N0, N1).



%@  @item queue_append(@var{+Queue0}, @var{+List}, @var{-Queue})
%@  @PLXindex {queue_append/3 (queues)}
%@  is true when @var{Queue} is obtained by appending the elements of @var{List}
%@  in order at the rear end of @var{Queue0}, e.g.
%@  @code{append_queue(Queue[a,b,c], [d,e], Queue[a,b,c,d,e])}.
%@  This is like @code{queue_last/3}; it side-effects @var{Queue0}.

queue_append(queue(N0,F,B0), List, queue(N1,F,B1)) :-
	queue_append(List, B1, B0, N0, N1).

queue_append([], B, B, N, N).
queue_append([H|T], B, [H|R], N0, s(N1)) :-
	queue_append(T, B, R, N0, N1).



%@  @item list_queue(@var{?List}, @var{?Queue})
%@  @PLXindex {list_queue/2 (queues)}
%@  is true when @var{Queue} is a queue and @var{List} is a list and both have
%@  the same elements in the same order.  @code{list_queue/2} and @code{queue_list/2}
%@  are the same except for argument order.

list_queue(List, queue(Size,Front,Back)) :-
    (	var(List) ->
	queue_list(Size, List, Back, Front)
    ;	list_queue(List, Size, Back, Front)
    ).

list_queue([], z, Back, Back).
list_queue([Head|Tail], s(N), Back, [Head|Front]) :-
	list_queue(Tail, N, Back, Front).



%@  @item queue_list(@var{?Queue}, @var{?List})
%@  @PLXindex {queue_list/2 (queues)}
%@  is true when @var{Queue} is a queue and @var{List} is a list and both have
%@  the same elements in the same order.  @code{queue_list/2} and @code{list_queue/2}
%@  are the same except for argument order.

queue_list(queue(Size,Front,Back), List) :-
    (	var(Size) ->
	list_queue(List, Size, Back, Front)
    ;	queue_list(Size, List, Back, Front)
    ).

queue_list(z, [], Back, Back).
queue_list(s(N), [Head|Tail], Back, [Head|Front]) :-
	queue_list(N, Tail, Back, Front).



%@  @item queue_length(@var{?Queue}, @var{?Length})
%@  @PLXindex {queue_length/2 (queues)}
%@  is true when @var{Queue} is a queue having @var{Length} elements.  It may be used
%@  to determine the @var{Length} of a @var{Queue} or to make a @var{Queue} of given @var{Length}.

queue_length(queue(N,F,B), Length) :-
    (	var(Length) ->
	queue_length_1(N, F, B, 0, Length)
    ;	integer(Length) ->
	Length >= 0,
	queue_length_2(Length, N, F, B)
 %  ;	must_be(Length, integer, queue_length(queue(N,F,B),Length), 2)
    ).

queue_length_1(z, B, B, Length, Length).
queue_length_1(s(N), [_|F], B, Length0, Length) :-
	Length1 is Length0+1,
	queue_length_1(N, F, B, Length1, Length).

queue_length_2(0, z, B, B) :- !.
queue_length_2(L0, s(N), [_|F], B) :-
	L0 > 0,
	L1 is L0-1,
	queue_length_2(L1, N, F, B).



%@  @item queue_member(@var{?Element}, @var{+Queue})
%@  @PLXindex {queue_member/2 (queues)}
%@  is true when @var{Element} is an element of @var{Queue}.  It could be made to
%@  generate queues, but that would be rather inefficient.  It bears
%@  the name @code{queue_member/2} because it is prepared to enumerate @var{Elements}.

queue_member(Element, queue(s(N),Front,_)) :-
	queue_member(N, Front, Element).

:- queue_member/3 is nondet.
queue_member(z,    [Element|_], Element).
queue_member(s(_), [Element|_], Element).
queue_member(s(N), [_|Front],   Element) :-
	queue_member(N, Front, Element).



%@  @item queue_memberchk(@var{+Element}, @var{+Queue})
%@  @PLXindex {queue_memberchk/2 (queues)}
%@  is true when the given @var{Element} is an element of @var{Queue}.  Once it finds
%@  a member of @var{Queue} which unifies with @var{Element}, it commits to it.  Use
%@  it to check a ground @var{Element}.

queue_memberchk(Element, queue(s(N),Front,_)) :-
	queue_memberchk(N, Front, Element).

queue_memberchk(z,    [Element|_], Element).
queue_memberchk(s(_), [Element|_], Element) :- !.
queue_memberchk(s(N), [_|Front],   Element) :-
	queue_memberchk(N, Front, Element).


%@  @item map_queue(@var{:Pred}, @var{+Queue[X1,...,Xn]})
%@  @PLXindex {map_queue/[2,3] (queues)}
%@  succeeds when @var{Pred(Xi)} succeeds for each element @var{Xi} of the @var{Queue}.
map_queue(Pred, queue(Size,F1,B1)) :-
	mapqueue(Size, F1, B1, Pred).

mapqueue(z, B1, B1, _).
mapqueue(s(N), [X1|F1], B1, Pred) :-
	call(Pred, X1),
	mapqueue(N, F1, B1, Pred).


%@  @item map_queue(@var{:Pred}, @var{+Queue[X1,...,Xn]}, @var{?Queue[Y1,...,Yn]})
%@  succeeds when @var{Pred(Xi,Yi)} succeeds for each corresponding pair
%@  of elements @var{Xi}, @var{Yi} of the two queues.
map_queue(Pred, queue(Size,F1,B1), queue(Size,F2,B2)) :-
	mapqueue(Size, F1, B1, F2, B2, Pred).

mapqueue(z, B1, B1, B2, B2, _).
mapqueue(s(N), [X1|F1], B1, [X2|F2], B2, Pred) :-
	call(Pred, X1, X2),
	mapqueue(N, F1, B1, F2, B2, Pred).



%@  @item map_queue_list(@var{:Pred}, @var{?Queue[X1,...,Xn]}, @var{?[Y1,...,Yn]})
%@  @PLXindex {map_queue_list/3 (queues)}
%@  succeeds when @var{Pred(Xi, Yi)} is true for each corresponding pair @var{Xi,Yi}
%@  of elements of the @var{Queue} and the @var{List}.  It may be used to generate
%@  either of the sequences from the other.
map_queue_list(Pred, queue(N,F,B), L) :-
    (	var(N) ->
	map_list_queue_2(L, N, F, B, Pred)
    ;	map_queue_list_1(N, F, B, L, Pred)
    ).

map_queue_list_1(z, B, B, [], _).
map_queue_list_1(s(N), [X1|F], B, [X2|L], Pred) :-
	call(Pred, X1, X2),
	map_queue_list_1(N, F, B, L, Pred).

map_queue_list_2(z, B, B, [], _).
map_queue_list_2(s(N), [X2|F], B, [X1|L], Pred) :-
	call(Pred, X1, X2),	% note arguments swapped
	map_queue_list_2(N, F, B, L, Pred).


%@  @item map_list_queue(@var{:Pred}, @var{?[X1,...,Xn]}, @var{?Queue[Y1,...,Yn]})
%@  @PLXindex {map_list_queue/3 (queues)}
%@  succeeds when @var{Pred(Xi, Yi)} is true for each corresponding pair @var{Xi,Yi}
%@  of elements of the @var{List} and the @var{Queue}.  It may be used to generate
%@  either of the sequences from the other.

map_list_queue(Pred, L, queue(N,F,B)) :-
    (	var(N) ->
	map_list_queue_1(L, N, F, B, Pred)
    ;	map_queue_list_2(N, F, B, L, Pred)
    ).

map_list_queue_1([], z, B, B, _).
map_list_queue_1([X1|L], s(N), [X2|F], B, Pred) :-
	call(Pred, X1, X2),
	map_list_queue_1(L, N, F, B, Pred).

map_list_queue_2([], z, B, B, _).
map_list_queue_2([X2|L], s(N), [X1|F], B, Pred) :-
	call(Pred, X1, X2),	% note arguments swapped.
	map_list_queue_2(L, N, F, B, Pred).



%@  @item some_queue(@var{:Pred}, @var{+Queue[X1,...,Xn]})
%@  @PLXindex {some_queue/[2,3] (queues)}
%@  succeeds when @var{Pred(Xi)} succeeds for some @var{Xi} in the @var{Queue}.  It will
%@  try all ways of proving @var{Pred(Xi)} for each @var{Xi}, and will try each @var{Xi}
%@  in the @var{Queue}.  @code{somechk_queue/2} is to @code{some_queue/2} as @code{memberchk/2}
%@  is to @code{member/2}; you are more likely to want @code{somechk_queue/2}.
%@  This acts on backtracking like @code{member/2}; @var{Queue} should be proper.
some_queue(Pred, queue(Size,F1,_)) :-
	somequeue(Size, F1, Pred).

:- somequeue/3 is nondet.
somequeue(s(_), [X1|_], Pred) :-
	call(Pred, X1).
somequeue(s(N), [_|F1], Pred) :-
	somequeue(N, F1, Pred).


%@  @item some_queue(@var{:Pred}, @var{+Queue[X1,...,Xn]}, @var{?Queue[Y1,...,Yn]})
%@  is true when @var{Pred(Xi, Yi)} is true for some @var{i}.
some_queue(Pred, queue(Size,F1,_), queue(Size,F2,_)) :-
	somequeue(Size, F1, F2, Pred).

:- somequeue/4 is nondet.
somequeue(s(_), [X1|_], [X2|_], Pred) :-
	call(Pred, X1, X2).
somequeue(s(N), [_|F1], [_|F2], Pred) :-
	somequeue(N, F1, F2, Pred).



%@  @item somechk_queue(@var{:Pred}, @var{+Queue[X1,...,Xn]})
%@  @PLXindex {somechk_queue/[2,3] (queues)}
%@  is true when @var{Pred(Xi)} is true for some @var{i}, and it commits to
%@  the first solution it finds (like @code{memberchk/2}).
somechk_queue(Pred, queue(Size,F1,_)) :-
	somechkqueue(Size, F1, Pred).

somechkqueue(s(_), [X1|_], Pred) :-
	call(Pred, X1),
	!.
somechkqueue(s(N), [_|F1], Pred) :-
	somechkqueue(N, F1, Pred).


%@  @item somechk_queue(@var{:Pred}, @var{+Queue[X1,...,Xn]}, @var{?Queue[Y1,...,Yn]})
%@  is true when @var{Pred(Xi, Yi)} is true for some @var{i}, and it commits to
%@  the first solution it finds (like @code{memberchk/2}).
somechk_queue(Pred, queue(Size,F1,_), queue(Size,F2,_)) :-
	somechkqueue(Size, F1, F2, Pred).

somechkqueue(s(_), [X1|_], [X2|_], Pred) :-
	call(Pred, X1, X2),
	!.
somechkqueue(s(N), [_|F1], [_|F2], Pred) :-
	somechkqueue(N, F1, F2, Pred).


%@  @end table
