%   Package: avl
%   Author : Richard A. O'Keefe
%   Updated: 18 Jan 1994
%   Purpose: AVL trees in Prolog

%   Copyright (C) 1989, Quintus Computer Systems, Inc.  All rights reserved.

:- module(avl, [
	avl_to_list/2,			% AVL -> List
	is_avl/1,			% AVL ->
	avl_change/5,			% Key -> (AVL x Val <-> AVL x Val)
	avl_domain/2,			% AVL -> OrdSet
	avl_fetch/2,			% Key x AVL ->
	avl_fetch/3,			% Key x AVL -> Val
	avl_height/2,			% AVL -> Height
	avl_incr/4,			% Key x AVL x Inc -> AVL
	avl_max/2,			% AVL -> Key
	avl_max/3,			% AVL -> Key x Val
	avl_member/2,			% Key x AVL
	avl_member/3,			% Key x AVL x Val
	avl_min/2,			% AVL -> Key
	avl_min/3,			% AVL -> Key x Val
	avl_next/3,			% Key x AVL -> Key
	avl_next/4,			% Key x AVL -> Key x Val
	avl_prev/3,			% Key x AVL -> Key
	avl_prev/4,			% Key x AVL -> Key x Val
	avl_range/2,			% AVL -> OrdSet
	avl_size/2,			% AVL -> Size
	avl_store/4,			% Key x AVL x Val -> Val
	avl_delete/4,			% Key x AVL x Val -> AVL
	avl_del_max/4,			% AVL -> Key x Val x AVL
	avl_del_min/4,			% AVL -> Key x Val x AVL
	avl_map/2,			% Goal x AVL ->
	avl_map/3,			% Goal x AVL -> AVL
	empty_avl/1,			% -> AVL
	list_to_avl/2,			% List -> AVL
	ord_list_to_avl/2,		% List -> AVL
	portray_avl/1			% AVL ->
   ]).

:- meta_predicate
        %% [PM] 4.2.1 FIXME: The '?' positions are '-' in the documentation.
	avl_map(1, ?),
	avl_map(2, ?, ?).

:- meta_predicate
        avl_map_1(?, 1),
        avl_map_1(?, ?, 2).

:- mode
	avl_to_list(+, ?),
	    avl_to_list(+, ?, ?),
	is_avl(+),
	    is_avl(+, +, -, -),
	avl_change(+, ?, ?, ?, ?),
	    avl_change(+, +, ?, ?, ?, ?, ?, ?, ?, ?),
	avl_domain(+, ?),
	    avl_domain(+, ?, +),
	avl_fetch(+, +),
	    avl_fetch_1(+, +, +, +),
	avl_fetch(+, +, ?),
	    avl_fetch_1(+, +, ?, +, +, +),
	avl_height(+, ?),
	    avl_height(+, +, ?),
	avl_member(?, +),
	avl_member(?, +, ?),
	avl_max(+, ?),
	    avl_max_1(+, ?, +),
	avl_max(+, ?, ?),
	    avl_max_1(+, ?, ?, +, +),
	avl_min(+, ?),
	    avl_min_1(+, ?, +),
	avl_min(+, ?, ?),
	    avl_min_1(+, ?, ?, +, +),
	avl_next(+, +, ?),
	avl_next(+, +, ?, ?),
	avl_prev(+, +, ?),
	avl_prev(+, +, ?, ?),
	avl_range(+, ?),
	    avl_range(+, ?, +),
	avl_size(+, ?),
	avl_store(+, +, +, ?),
	    avl_store(+, +, +, ?, -),
		avl_store(+, +, +, ?, -, +, +, +, +, +),
	empty_avl(?),
	list_to_avl(+, ?),
	ord_list_to_avl(+, ?),
	portray_avl(+),
	    portray_avl(+, +, -),
	    portray_avl(+, +, +, +).



%@  @cindex association list
%@  @cindex list, association
%@  @cindex binary tree
%@  @cindex tree, binary
%@  This library module provides an AVL tree implementation of "association
%@  lists". The binary tree @emph{is} kept balanced, as opposed to
%@  @code{library(assoc)}, which provides similar functionality based on
%@  binary trees that are not kept balanced.
%@  
%@  Exported predicates:
%@  
%@  @table @code

%@  @item empty_avl(@var{?AVL})
%@  @PLXindex {empty_avl/1 (avl)}
%@  is true when @var{AVL} is an empty AVL tree.

empty_avl(empty).



%@  @item avl_to_list(@var{+AVL}, @var{-List})
%@  @PLXindex {avl_to_list/2 (avl)}
%@  assumes that @var{AVL} is a proper AVL tree, and is true when
%@  @var{List} is a list of @var{Key-Value} pairs in ascending order with no
%@  duplicate keys specifying the same finite function as @var{AVL}.
%@  Use this to convert an @var{AVL} to an ordered list.

avl_to_list(AVL, List) :-
	avl_to_list(AVL, List, []).


avl_to_list(empty) --> [].
avl_to_list(node(K,V,_,L,R)) -->
	avl_to_list(L),
	[K-V],
	avl_to_list(R).



%@  @item is_avl(@var{+AVL})
%@  @PLXindex {is_avl/1 (avl)}
%@  is true when @var{AVL} is a (proper) AVL tree.  It checks both the order
%@  condition (that the keys are in ascending order as you go from left
%@  to right) and the height balance condition.  This code relies on
%@  variables (to be precise, the first anonymous variable in is_avl/1)
%@  being @code{@@<} than any non-variable.  in strict point of fact you @emph{can}
%@  construct an AVL tree with variables as keys, but @code{is_avl/1} doesn't
%@  believe it, and it is not good taste to do so.

is_avl(AVL) :-
	is_avl(AVL, _, _, _).

is_avl(X, _, _, _) :- var(X), !, fail.
is_avl(empty, Min, Min, 0).
is_avl(node(Key,_,B,L,R), Min, Max, H) :-
	is_avl(L, Min, Mid, HL),
	Mid @< Key,
	is_avl(R, Key, Max, HR),
	B is HR-HL,
	( HL < HR -> H is HR+1 ; H is HL+1 ).



%@  @item avl_domain(@var{+AVL}, @var{-Domain})
%@  @PLXindex {avl_domain/2 (avl)}
%@  unifies @var{Domain} with the ordered set representation of the domain
%@  of the AVL tree (the keys of it).  As the keys are in ascending
%@  order with no duplicates, we just read them off like @code{avl_to_list/2}.

avl_domain(AVL, Domain) :-
	avl_domain(AVL, Domain, []).

avl_domain(empty, Domain, Domain).
avl_domain(node(Key,_,_,L,R), Domain0, Domain) :-
	avl_domain(L, Domain0, [Key|Domain1]),
	avl_domain(R, Domain1, Domain).



%@  @item avl_range(@var{+AVL}, @var{-Range})
%@  @PLXindex {avl_range/2 (avl)}
%@  unifies @var{Range} with the ordered set representation of the range of the
%@  AVL (the values associated with its keys, not the keys themselves).
%@  Note that the cardinality (length) of the domain and the range are
%@  seldom equal, except of course for trees representing intertible maps.

avl_range(AVL, Range) :-
	avl_range(AVL, Values, []),
	sort(Values, Range).


avl_range(empty, Values, Values).
avl_range(node(_,Val,_,L,R), Values0, Values) :-
	avl_range(L, Values0, [Val|Values1]),
	avl_range(R, Values1, Values).



%@  @item avl_min(@var{+AVL}, @var{-Key})
%@  @PLXindex {avl_min/2 (avl)}
%@  is true when @var{Key} is the smallest key in @var{AVL}.

avl_min(node(K,_,_,L,_), MinKey) :-
	avl_min_1(L, MinKey, K).

avl_min_1(empty, K, K).
avl_min_1(node(K,_,_,L,_), MinKey, _) :-
	avl_min_1(L, MinKey, K).


%@  @item avl_min(@var{+AVL}, @var{-Key}, @var{-Val})
%@  @PLXindex {avl_min/3 (avl)}
%@  is true when @var{Key} is the smallest key in @var{AVL} and @var{Val} is its value.

avl_min(node(K,V,_,L,_), MinKey, MinVal) :-
	avl_min_1(L, MinKey, MinVal, K, V).

avl_min_1(empty, K, V, K, V).
avl_min_1(node(K,V,_,L,_), MinKey, MinVal, _, _) :-
	avl_min_1(L, MinKey, MinVal, K, V).



%@  @item avl_max(@var{+AVL}, @var{-Key})
%@  @PLXindex {avl_max/2 (avl)}
%@  is true when @var{Key} is the greatest key in @var{AVL}.

avl_max(node(K,_,_,_,R), MaxKey) :-
	avl_max_1(R, MaxKey, K).

avl_max_1(empty, K, K).
avl_max_1(node(K,_,_,_,R), MaxKey, _) :-
	avl_max_1(R, MaxKey, K).


%@  @item avl_max(@var{+AVL}, @var{-Key}, @var{-Val})
%@  @PLXindex {avl_max/3 (avl)}
%@  is true when @var{Key} is the greatest key in @var{AVL} and @var{Val} is its value.

avl_max(node(K,V,_,_,R), MaxKey, MaxVal) :-
	avl_max_1(R, MaxKey, MaxVal, K, V).

avl_max_1(empty, K, V, K, V).
avl_max_1(node(K,V,_,_,R), MaxKey, MaxVal, _, _) :-
	avl_max_1(R, MaxKey, MaxVal, K, V).



%@  @item avl_height(@var{+AVL}, @var{-Height})
%@  @PLXindex {avl_height/2 (avl)}
%@  is true when @var{Height} is the height of the given AVL tree, that is,
%@  the longest path in the tree has @var{Height} 'node's on it.

avl_height(AVL, Height) :-
	avl_height(AVL, 0, Height).

avl_height(empty, H, H).
avl_height(node(_,_,B,L,R), H0, H) :-
	H1 is H0+1,
	(   B >= 0 -> avl_height(R, H1, H)
	;	      avl_height(L, H1, H)
	).



%@  @item avl_size(@var{+AVL}, @var{-Size})
%@  @PLXindex {avl_size/2 (avl)}
%@  is true when @var{Size} is the size of the AVL tree, the number of 'node's in it.

avl_size(empty, 0).
avl_size(node(_,_,_,L,R), Size) :-
	avl_size(L, A),
	avl_size(R, B),
	Size is A+B+1.



%@  @item portray_avl(@var{+AVL})
%@  @PLXindex {portray_avl/1 (avl)}
%@  writes an AVL tree to the current output stream in a pretty form so
%@  that you can easily see what it is.  Note that an AVL tree written
%@  out this way can NOT be read back in; for that use @code{writeq/1}.  The
%@  point of this predicate is 
%@  @c that you can add a directive
%   @c     :- add_portray(portray_avl).
%@  to get AVL trees displayed nicely by @code{print/1}.

portray_avl(empty) :-
	write('AVL{'),
	put_code(0'}).
portray_avl(node(K,V,B,L,R)) :-
	write('AVL{'),
	portray_avl(L, 0, X0),
	portray_avl(K, V, B, X0),
	portray_avl(R, 1, _),
	put_code(0'}).


portray_avl(empty, X, X).
portray_avl(node(K,V,B,L,R), X0, X) :-
	portray_avl(L, X0, X1),
	portray_avl(K, V, B, X1),
	portray_avl(R, 1, X).


portray_avl(K, V, B, X0) :-
	( X0 =:= 0 -> true ; put_code(0',) ),
	print(K),
	(   B < 0 -> write('*->')
	;   B > 0 -> write('->*')
	;            write('->')
	),
	print(V).



%@  @item avl_member(@var{?Key}, @var{+AVL})
%@  @PLXindex {avl_member/2 (avl)}
%@  is true when @var{Key} is one of the keys in the given AVL.  This
%@  predicate should be used to enumerate the keys, not to look for
%@  a particular key (use @code{avl_fetch/2} or @code{avl_fetch/3} for that).
%@  The @var{Keys} are enumerated in ascending order.

avl_member(Key, node(K,_,_,L,R)) :-
	(   avl_member(Key, L)
	;   Key = K
	;   avl_member(Key, R)
	).


%@  @item avl_member(@var{?Key}, @var{+AVL}, @var{?Val})
%@  @PLXindex {avl_member/3 (avl)}
%@  is true when @var{Key} is one of the keys in the given AVL and @var{Val} is
%@  the value the AVL associates with that @var{Key}.  This predicate should
%@  be used to enumerate the keys and their values, not to look up the
%@  value of a known key (use @code{avl_fetch/3}) for that.
%@  The @var{Keys} are enumerated in ascending order.

avl_member(Key, node(K,V,_,L,R), Val) :-
	(   avl_member(Key, L, Val)
	;   Key = K, Val = V
	;   avl_member(Key, R, Val)
	).



%@  @item avl_fetch(@var{+Key}, @var{+AVL})
%@  @PLXindex {avl_fetch/2 (avl)}
%@  is true when the (given) @var{Key} is one of the keys in the (given) AVL.
%@  Use this to test whether a known Key occurs in @var{AVL} and you don't
%@  want to know the value associated with it.

avl_fetch(Key, node(K,_,_,L,R)) :-
	compare(O, Key, K),
	avl_fetch_1(O, Key, L, R).


avl_fetch_1(=, _,   _, _).
avl_fetch_1(<, Key, node(K,_,_,L,R), _) :-
	compare(O, Key, K),
	avl_fetch_1(O, Key, L, R).
avl_fetch_1(>, Key, _, node(K,_,_,L,R)) :-
	compare(O, Key, K),
	avl_fetch_1(O, Key, L, R).

%@  @item avl_fetch(@var{+Key}, @var{+AVL}, @var{-Val})
%@  @PLXindex {avl_fetch/3 (avl)}
%@  is true when the (given) @var{Key} is one of the keys in the (given) AVL
%@  and the value associated with it therein is @var{Val}.  It should be
%@  used to look up @emph{known} keys, not to enumerate keys (use either
%@  @code{avl_member/2} or @code{avl_member/3} for that).

avl_fetch(Key, node(K,V,_,L,R), Val) :-
	compare(O, Key, K),
	avl_fetch_1(O, Key, Val, V, L, R).


avl_fetch_1(=, _,   Val, Val, _, _).
avl_fetch_1(<, Key, Val, _, node(K,V,_,L,R), _) :-
	compare(O, Key, K),
	avl_fetch_1(O, Key, Val, V, L, R).
avl_fetch_1(>, Key, Val, _, _, node(K,V,_,L,R)) :-
	compare(O, Key, K),
	avl_fetch_1(O, Key, Val, V, L, R).


%@  @item avl_next(@var{+Key}, @var{+AVL}, @var{-Knext})
%@  @PLXindex {avl_next/3 (avl)}
%@  is true when @var{Knext} is the next key after @var{Key} in @var{AVL};
%@  that is, @var{Knext} is the smallest key in @var{AVL} such that @var{Knext @@> Key}.

avl_next(Key, node(K,_,_,L,R), Knext) :-
	(   K @=< Key ->
	    avl_next(Key, R, Knext)
	;   avl_next(Key, L, K1) ->
	    Knext = K1
	;   Knext = K
	).


%@  @item avl_next(@var{+Key}, @var{+AVL}, @var{-Knext}, @var{-Vnext})
%@  @PLXindex {avl_next/4 (avl)}
%@  is true when @var{Knext} is the next key after @var{Key} in @var{AVL} and @var{Vnext} is the
%@  value associated with @var{Knext} in @var{AVL}.  That is, @var{Knext} is the smallest
%@  key in @var{AVL} such that @var{Knext @@> Key}, and @code{avl_fetch(@var{Knext}, @var{AVL}, @var{Vnext})}.

avl_next(Key, node(K,V,_,L,R), Knext, Vnext) :-
	(   K @=< Key ->
	    avl_next(Key, R, Knext, Vnext)
	;   avl_next(Key, L, K1, V1) ->
	    Knext = K1, Vnext = V1
	;   Knext = K,  Vnext = V
	).


%@  @item avl_prev(@var{+Key}, @var{+AVL}, @var{-Kprev})
%@  @PLXindex {avl_prev/3 (avl)}
%@  is true when @var{Kprev} is the key previous to @var{Key} in @var{AVL};
%@  that is, @var{Kprev} is the greatest key in @var{AVL} such that @var{Kprev @@< Key}.

avl_prev(Key, node(K,_,_,L,R), Kprev) :-
	(   K @>= Key ->
	    avl_prev(Key, L, Kprev)
	;   avl_prev(Key, R, K1) ->
	    Kprev = K1
	;   Kprev = K
	).


%@  @item avl_prev(@var{+Key}, @var{+AVL}, @var{-Kprev}, @var{-Vprev})
%@  @PLXindex {avl_prev/4 (avl)}
%@  is true when @var{Kprev} is the key previous to Key in @var{AVL} and @var{Vprev} is the
%@  value associated with @var{Kprev} in @var{AVL}.  That is, @var{Kprev} is the greatest key
%@  in @var{AVL} such that @var{Kprev @@< Key}, and @code{avl_fetch(@var{Kprev}, @var{AVL}, @var{Vprev})}.

avl_prev(Key, node(K,V,_,L,R), Kprev, Vprev) :-
	(   K @>= Key ->
	    avl_prev(Key, L, Kprev, Vprev)
	;   avl_prev(Key, R, K1, V1) ->
	    Kprev = K1, Vprev = V1
	;   Kprev = K,  Vprev = V
	).


%@  @item avl_change(@var{+Key}, @var{?AVL1}, @var{?Val1}, @var{?AVL2}, @var{?Val2})
%@  @PLXindex {avl_change/5 (avl)}
%@  is true when @var{AVL1} and @var{AVL2} are avl trees of exactly the same shape,
%@  @var{Key} is a key of both of them, @var{Val1} is the value associated with @var{Key}
%@  in @var{AVL1} and @var{Val2} is the value associated with it in @var{AVL2}, and when
%@  @var{AVL1} and @var{AVL2} are identical except perhaps for the value they assign
%@  to @var{Key}.  Use this to change the value associated with a @var{Key} which is
%@  already present, not to insert a new @var{Key} (it won't).

avl_change(Key, node(K,V1,B,L1,R1), Val1, node(K,V2,B,L2,R2), Val2) :-
	compare(O, Key, K),
	avl_change(O, Key, Val1, Val2, V1, V2, L1, R1, L2, R2).

avl_change(=, _,   Val1, Val2, Val1, Val2, L, R, L, R).
avl_change(<, Key, Val1, Val2, V, V,
		node(K,V1,B,L1,R1), R, node(K,V2,B,L2,R2), R) :-
	compare(O, Key, K),
	avl_change(O, Key, Val1, Val2, V1, V2, L1, R1, L2, R2).
avl_change(>, Key, Val1, Val2, V, V,
		L, node(K,V1,B,L1,R1), L, node(K,V2,B,L2,R2)) :-
	compare(O, Key, K),
	avl_change(O, Key, Val1, Val2, V1, V2, L1, R1, L2, R2).



%@  @item ord_list_to_avl(@var{+List}, @var{-AVL})
%@  @PLXindex {ord_list_to_avl/2 (avl)}
%@  is given a list of @var{Key-Val} pairs where the @var{Keys} are already in
%@  standard order with no duplicates (this is not checked) and
%@  returns an AVL representing the same associations.  This takes
%@  @var{O(N)} time, unlike @code{list_to_avl/2} which takes @var{O(N lg N)}.

ord_list_to_avl(List, AVL) :-
	length(List, N),
	ord_list_to_avl(N, List, [], _, AVL).

ord_list_to_avl(0, List, List, 0, empty) :- !.
ord_list_to_avl(N, List0, List, H, node(Key,Val,Bal,L,R)) :-
	A is (N-1) >> 1,
	Z is (N-1) - A,
	ord_list_to_avl(A, List0, [Key-Val|List1], HL, L),
	ord_list_to_avl(Z, List1, List, HR, R),
	Bal is HR - HL,
	( HR > HL -> H is HR + 1 ; H is HL + 1).


%@  @item list_to_avl(@var{+Pairs}, @var{-AVL})
%@  @PLXindex {list_to_avl/2 (avl)}
%@  is given a proper list of @var{Key-Val} pairs where the @var{Keys} are in no particular
%@  order (but are sufficiently instantiated to be told apart) and
%@  returns an AVL representing the same associations.  This works by
%@  starting with an empty tree and inserting the elements of the list
%@  into it.  This takes @var{O(N lg N)} time.  Since it is possible to read
%@  off a sorted list in @var{O(N)} time from the result, @var{O(N lg N)} is as good as
%@  can possibly be done.  If the same @var{Key} appears more than once in the
%@  input, the last value associated with it will be used.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  list_to_avl(Pairs, AVL) :-
%@  	(   foreach(K-V,Pairs),
%@  	    fromto(empty,AVL0,AVL1,AVL)
%@  	do  avl_store(K, AVL0, V, AVL1)
%@  	).
%@  @end group
%@  @end example

list_to_avl(Pairs, AVL) :-
	(   foreach(K-V,Pairs),
	    fromto(empty,AVL0,AVL1,AVL)
	do  avl_store(K, AVL0, V, AVL1)
	).



%@  @item avl_store(@var{+Key}, @var{+OldAVL}, @var{+Val}, @var{+NewAVL})
%@  @PLXindex {avl_store/4 (avl)}
%@  is true when @var{OldAVL} and @var{NewAVL} define the same finite function
%@  except that @var{NewAVL} associates @var{Val} with @var{Key}.  @var{OldAVL} need not have
%@  associated any value at all with @var{Key}.  When it didn't, you can
%@  read this as "insert @var{(Key->Val)} into @var{OldAVL} giving @var{NewAVL}".

avl_store(Key, AVL0, Val, AVL1) :-
	avl_store(AVL0, Key, Val, AVL1, _).


avl_store(empty,           Key, Val, node(Key,Val,0,empty,empty), 1).
avl_store(node(K,V,B,L,R), Key, Val, Result, Delta) :-
	compare(O, Key, K),
	avl_store(O, Key, Val, Result, Delta, K, V, B, L, R).


avl_store(=, Key, Val, node(Key,Val,B,L,R), 0, _, _, B, L, R).
avl_store(<, Key, Val, Result,          Delta, K, V, B, L, R) :-
	avl_store(L, Key, Val, Lavl, Ldel),
	Delta is \(B) /\ Ldel,	% this grew iff left grew and was balanced
	B1 is B-Ldel,
	(   B1 =:= -2 ->	% rotation needed
	    Lavl = node(Y,VY,OY,A,CD),	    
	    (   OY =< 0 ->
		NY is OY+1, NK is -NY,
		Result = node(Y,VY,NY,A,node(K,V,NK,CD,R))
	    ;/* OY = 1, double rotation needed */
		CD = node(X,VX,OX,C,D),
		NY is 0-((1+OX) >> 1),
		NK is (1-OX) >> 1,
		Result = node(X,VX,0,node(Y,VY,NY,A,C),node(K,V,NK,D,R))
	    )
	;   Result = node(K,V,B1,Lavl,R)
	).
avl_store(>, Key, Val, Result,          Delta, K, V, B, L, R) :-
	avl_store(R, Key, Val, Ravl, Rdel),
	Delta is \(B) /\ Rdel,	% this grew iff right grew and was balanced
	B1 is B+Rdel,
	(   B1 =:= 2 ->		% rotation needed
	    Ravl = node(Y,VY,OY,AC,D),
	    (   OY >= 0 ->
		NY is OY-1, NK is -NY,
		Result = node(Y,VY,NY,node(K,V,NK,L,AC),D)
	    ;/* OY = -1, double rotation needed */
		AC = node(X,VX,OX,A,C),
		NY is (1-OX) >> 1,
		NK is 0-((1+OX) >> 1),
		Result = node(X,VX,0,node(K,V,NK,L,A),node(Y,VY,NY,C,D))
	    )
	;   Result = node(K,V,B1,L,Ravl)
	).



%@  @item avl_incr(@var{+Key}, @var{+OldAVL}, @var{+Incr}, @var{+NewAVL})
%@  @PLXindex {avl_incr/4 (avl)}
%@  if @var{Key} is not present in @var{OldAVL}, adds @var{Key->Incr}.
%@  if @var{Key->N} is present in @var{OldAvl}, changes it to @var{Key->N+Incr}.

avl_incr(Key, AVL0, Inc, AVL1) :-
	avl_incr(AVL0, Key, Inc, AVL1, _).


avl_incr(empty,           Key, Inc, node(Key,Inc,0,empty,empty), 1).
avl_incr(node(K,V,B,L,R), Key, Inc, Result, Delta) :-
	compare(O, Key, K),
	avl_incr(O, Key, Inc, Result, Delta, K, V, B, L, R).


avl_incr(=, Key, Inc, node(Key,Val,B,L,R), 0, _, V, B, L, R) :-
	Val is V+Inc.
avl_incr(<, Key, Inc, Result,          Delta, K, V, B, L, R) :-
	avl_incr(L, Key, Inc, Lavl, Ldel),
	Delta is \(B) /\ Ldel,	% this grew iff left grew and was balanced
	B1 is B-Ldel,
	(   B1 =:= -2 ->	% rotation needed
	    Lavl = node(Y,VY,OY,A,CD),	    
	    (   OY =< 0 ->
		NY is OY+1, NK is -NY,
		Result = node(Y,VY,NY,A,node(K,V,NK,CD,R))
	    ;/* OY = 1, double rotation needed */
		CD = node(X,VX,OX,C,D),
		NY is 0-((1+OX) >> 1),
		NK is (1-OX) >> 1,
		Result = node(X,VX,0,node(Y,VY,NY,A,C),node(K,V,NK,D,R))
	    )
	;   Result = node(K,V,B1,Lavl,R)
	).
avl_incr(>, Key, Inc, Result,          Delta, K, V, B, L, R) :-
	avl_incr(R, Key, Inc, Ravl, Rdel),
	Delta is \(B) /\ Rdel,	% this grew iff right grew and was balanced
	B1 is B+Rdel,
	(   B1 =:= 2 ->		% rotation needed
	    Ravl = node(Y,VY,OY,AC,D),
	    (   OY >= 0 ->
		NY is OY-1, NK is -NY,
		Result = node(Y,VY,NY,node(K,V,NK,L,AC),D)
	    ;/* OY = -1, double rotation needed */
		AC = node(X,VX,OX,A,C),
		NY is (1-OX) >> 1,
		NK is 0-((1+OX) >> 1),
		Result = node(X,VX,0,node(K,V,NK,L,A),node(Y,VY,NY,C,D))
	    )
	;   Result = node(K,V,B1,L,Ravl)
	).

%@  @item avl_delete(@var{+Key}, @var{+OldAVL}, @var{-Val}, @var{-NewAVL})
%@  @PLXindex {avl_delete/4 (avl)}
%@  is true when @var{OldAVL} and @var{NewAVL} define the same finite function
%@  except that @var{OldAVL} associates @var{Key} with @var{Val} and @var{NewAVL} doesn't
%@  associate @var{Key} with any value.

avl_delete(Key, AVL0, Val, AVL) :-
	avl_delete(AVL0, Key, Val, AVL, _).

avl_delete(node(K,V,B,L,R), Key, Val, AVL, Delta) :-
	compare(C, Key, K),
	avl_delete(C, Key, Val, AVL, Delta, K, V, B, L, R).

avl_delete(<, Key, Val, AVL, Delta, K, V, B, L, R) :-
	avl_delete(L, Key, Val, L1, D1),
        B1 is B+D1,
	avl(B1, K, V, L1, R, AVL),
	avl_shrinkage(AVL, D1, Delta).
avl_delete(=, _, Val, AVL, Delta, _, Val, B, L, R) :-
	(   L == empty -> AVL = R, Delta = 1
	;   R == empty -> AVL = L, Delta = 1
	;   avl_del_max(L, K, V, L1, D1),
	    B1 is B+D1,
	    avl(B1, K, V, L1, R, AVL),
	    avl_shrinkage(AVL, D1, Delta)
	).
avl_delete(>, Key, Val, AVL, Delta, K, V, B, L, R) :-
	avl_delete(R, Key, Val, R1, D1),
	B1 is B-D1,
	avl(B1, K, V, L, R1, AVL),
	avl_shrinkage(AVL, D1, Delta).


%@  @item avl_del_min(@var{+OldAVL}, @var{-Key}, @var{-Val}, @var{-NewAVL})
%@  @PLXindex {avl_del_min/4 (avl)}
%@  is true when @var{OldAVL} and @var{NewAVL} define the same finite function
%@  except that @var{OldAVL} associates @var{Key} with @var{Val} and @var{NewAVL} doesn't
%@  associate @var{Key} with any value and @var{Key} precedes all other keys in @var{OldAVL}.

avl_del_min(AVL0, Key, Val, AVL) :-
	avl_del_min(AVL0, Key, Val, AVL, _).

avl_del_min(node(K,V,B,L,R), Key, Val, AVL, Delta) :-
	(   L == empty ->
	    AVL = R, Key = K, Val = V, Delta = 1
	;   avl_del_min(L, Key, Val, L1, D1),
	    B1 is B+D1,
	    avl(B1, K, V, L1, R, AVL),
	    avl_shrinkage(AVL, D1, Delta)
	).



%@  @item avl_del_max(@var{+OldAVL}, @var{-Key}, @var{-Val}, @var{-NewAVL})
%@  @PLXindex {avl_del_max/4 (avl)}
%@  is true when @var{OldAVL} and @var{NewAVL} define the same finite function
%@  except that @var{OldAVL} associates @var{Key} with @var{Val} and @var{NewAVL} doesn't
%@  associate @var{Key} with any value and 
%@  @var{Key} is preceded by all other keys in @var{OldAVL}.

avl_del_max(AVL0, Key, Val, AVL) :-
	avl_del_max(AVL0, Key, Val, AVL, _).

avl_del_max(node(K,V,B,L,R), Key, Val, AVL, Delta) :-
	(   R == empty ->
	    AVL = L, Key = K, Val = V, Delta = 1
	;   avl_del_max(R, Key, Val, R1, D1),
	    B1 is B-D1,
	    avl(B1, K, V, L, R1, AVL),
	    avl_shrinkage(AVL, D1, Delta)

	).

avl(-2, K, V, L, R, AVL) :-
	L = node(K1,V1,B1,L1,R1),
	avl_left(B1, K1, V1, L1, R1, K, V, R, AVL).
avl(-1, K, V, L, R, node(K,V,-1,L,R)).
avl( 0, K, V, L, R, node(K,V, 0,L,R)).
avl( 1, K, V, L, R, node(K,V, 1,L,R)).
avl( 2, K, V, L, R, AVL) :-
	R = node(K1,V1,B1,L1,R1),
	avl_right(B1, K1, V1, L1, R1, K, V, L, AVL).

avl_left(-1, K1, V1, L1, R1, K, V, R,		% single LL rotation
	    node(K1,V1, 0,L1,node(K,V, 0,R1,R))).
avl_left( 0, K1, V1, L1, R1, K, V, R,		% single LL rotation
	    node(K1,V1, 1,L1,node(K,V,-1,R1,R))).
avl_left( 1, K1, V1, L1, R1, K, V, R,		% double LR rotation
	    node(K2,V2, 0,node(K1,V1,BK1,L1,L2),node(K,V,BK,R2,R))) :-
        R1 = node(K2,V2,B2,L2,R2),
	avl(B2, BK1, BK).

avl_right( 1, K1, V1, L1, R1, K, V, L,	% single RR rotation
	     node(K1,V1, 0,node(K,V, 0,L,L1),R1)).
avl_right( 0, K1, V1, L1, R1, K, V, L,	% single RR rotation
	     node(K1,V1,-1,node(K,V, 1,L,L1),R1)).
avl_right(-1, K1, V1, L1, R1, K, V, L,	% double RL rotation
	     node(K2,V2, 0,node(K,V,BK,L,L2),node(K1,V1,BK1,R2,R1))) :-
        L1 = node(K2,V2,B2,L2,R2),
	avl(B2, BK, BK1).

avl(-1,  0, 1).
avl( 0,  0, 0).
avl( 1, -1, 0).

avl_shrinkage(node(_,_,B,_,_), D1, Delta) :-
	Delta is \(B) /\ D1.		% this shrank iff L/R shrank and
					% this became balanced


%@  @item avl_map(@var{:Pred}, @var{+AVL})
%@  @PLXindex {avl_map/2 (avl)}
%@  is true when @var{AVL} is an association tree, and for each @var{Key}, 
%@  if @var{Key} is associated with @var{Value} in @var{AVL}, @var{Pred(Value)} is true.

avl_map(MPred, AVL) :-
	avl_map_1(AVL, MPred).

avl_map_1(empty, _).
avl_map_1(node(_,Val,_,L,R), MPred) :-
	avl_map_1(L, MPred),
	call(MPred, Val),
	avl_map_1(R, MPred).



%@  @item avl_map(@var{:Pred}, @var{+OldAVL}, @var{-NewAVL})
%@  @PLXindex {avl_map/3 (avl)}
%@  is true when @var{OldAVL} and @var{NewAVL} are association trees of the
%@  same shape, and for each @var{Key}, if @var{Key} is associated with @var{Old} in
%@  @var{OldAVL} and with @var{New} in @var{NewAVL}, @var{Pred(Old,New)} is true.

avl_map(MPred, OldAVL, NewAVL) :-
	avl_map_1(OldAVL, NewAVL, MPred).

avl_map_1(empty, empty, _).
avl_map_1(node(Key,Old,B,L0,R0), node(Key,New,B,L1,R1), MPred) :-
	avl_map_1(L0, L1, MPred),
	call(MPred, Old, New),
	avl_map_1(R0, R1, MPred).

% [PM] Docs must end before explicit end_of_file
%@  @end table

end_of_file.

:- use_module(library(addportray), [add_portray/1]).

:- add_portray(portray_avl).

test :-
	list_to_avl([f-1,r-2,e-3,d-4,i-5,c-6,k-7,t-8,h-9,
		     g-10,a-11,o-12,u-13,s-14], Result),
	print(* = Result), nl,
	avl_height(Result, Height),
	avl_size(Result, Size),
	write((size(*)=Size, height(*)=Height)), nl.

test2 :-
	list_to_avl([g-1,a-2,r-3,f-4,i-5,e-6,l-7,d-8], T),
	avl_min(T, K0),
	avl_next(K0, T, K1),
	avl_next(K1, T, K2),
	avl_next(K2, T, K3),
	avl_next(K3, T, K4),
	avl_next(K4, T, K5),
	avl_next(K5, T, K6),
	write([K0,K1,K2,K3,K4,K5,K6]).


test(N) :-
	insert_integers(N, empty, AVL),
	avl_size(AVL, Size),
	avl_height(AVL, Height),
	write(('N',size,height)=(N,Size,Height)), nl.

time(N) :-
	statistics(runtime, _),
	insert_integers(N, empty, _),
	statistics(runtime, [_,T1]),
	insert_integers(0, N, empty, _),
	statistics(runtime, [_,T2]),
	write((T1,T2)).

extremes(N) :-
	insert_integers(N, empty, X1),
	insert_integers(0, N, empty, X2),
	avl_min(X1, Min1), avl_max(X1, Max1),
	avl_min(X2, Min2), avl_max(X2, Max2),
	write((N,Min1,Max1,Min2,Max2)).

insert_integers(0, A, A) :- !.
insert_integers(N, A0, A) :-
	avl_store(N, A0, N, A1),
	M is N-1,
	insert_integers(M, A1, A).

insert_integers(N, N, A, A) :- !.
insert_integers(I, N, A0, A) :- 
	J is I+1,
	avl_store(J, A0, N, A1),
	insert_integers(J, N, A1, A).


del_min(node(K,V,B,L,R), AVL, Del, Key, Val) :-
	(   atom(L) -> Key = K, Val = V, AVL = R, Del = 1
	;   del_min(L, L1, D1, Key, Val),
	    <repair>(K,V,B,L1,R, D1, AVL, Del)
	).

del_max(node(K,V,B,L,R), AVL, Del, Key, Val) :-
	(   atom(R) -> Key = K, Val = V, AVL = L, Del = 1
	;   del_max(R, R1, D1, Key, Val).
	    <repair>(K,V,B,L,R1, D1, AVL, Del)
	).


delete(Key, AVL0, AVL)


delete(Key, AVL0, AVL) :-
	delete_1(AVL0, Key, AVL, _).

delete_1(empty, _, empty, 0).
delete_1(node(K,V,B,L,R), Key, AVL, Del) :-
	compare(R, Key, K),
	delete_1(R, Key, AVL, Del, K, V, B, L, R).

delete_1(=, _, AVL, Del, _, _, B, L, R) :-
	(   B =:= 0, atom(L) ->
	    AVL = empty, Del = 1
	;   B >= 0 ->
	    del_min(R, K, V, R1, D1),
	    B1 is B-D1,
	    AVL = node(K, V, B1, L, R1),
	    Del = 0
	;   B < 0 ->
	    del_max(L, K, V, L1, D1),
	    B1 is B+D1,
	    AVL = node(K, V, B1, L1, R)
	).
delete_1(<, Key, AVL, Del, K, V, B, L, R) :-
	delete_1(L, Key, L1, D1),
	B1 is B+D1,
	(   B1 < 2 ->
	    AVL = node(K,V,B1,L1,R),
	    Del = 0
	;/* B1 = 2, rotation needed */
	).
delete_1(>, Key, AVL, Del, K, V, B, L, R) :-
	delete_1(R, Key, R1, D1),
	B1 is B-D1,
	(   B1 > -2 ->
	    AVL = node(K,V,B1,L,R1),
	    Del = 0
	;/* B1 = -2, rotation needed */
	).
