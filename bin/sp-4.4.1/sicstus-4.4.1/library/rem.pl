%   Package: rem
%   Author : Richard A. O'Keefe
%   Updated: 26 Aug 1987
%   Purpose: Maintain equivalence classes using Rem's algorithm.

%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

:- module(rem, [
	rem_create/2,		% NumberOfNodes -> F
	rem_head/3,
	rem_add_link/4,	% Node x Node x F -> F
	rem_equivalent/3	% Node x Node x F ->
   ]).
:- use_module(library(between), [
	between/3
   ]).


%@  This library module maintains equivalence classes using Rem's algorithm.
%@  Exported predicates:
%@  
%@  @table @code


%@  @item rem_create(@var{+Size}, @var{-REM})
%@  @PLXindex {rem_create/2 (rem)}
%@  creates an equivalence representation function @var{REM}
%@  which maps each of the nodes @var{1..Size} to itself.

rem_create(Size, rem(Size,Tree)) :-
	rem_head_1(Size, Tree).


rem_head_1(N, Tree) :-
    (	N < 1 -> true
    ;	put_value(N, Tree, N, Tree),
	M is N-1,
	rem_head_1(M, Tree)
    ).



%@  @item rem_head(@var{?Node}, @var{+REM}, @var{-Head})
%@  @PLXindex {rem_head/3 (random)}
%@  is true when @var{Head} is the representative of the equivalence class
%@  that @var{Node} belongs to in the given @var{REM}.

rem_head(Node, rem(Size,Tree), Head) :-
	between(1, Size, Node),
	rem_head_1(Node, Tree, Head).


rem_head_1(P, F, Head) :-
	get_value(P, F, FofP),
	(   FofP =:= P ->
	    Head = P
	;   rem_head_1(FofP, F, Head)
	).



%@  @item rem_equivalent(@var{?Node1}, @var{?Node2}, @var{+REM})
%@  @PLXindex {rem_equivalent/3 (rem)}
%@  is true when @var{Node1} and @var{Node2} belong to the same equivalence
%@  class in the given @var{REM}.

rem_equivalent(P, Q, F) :-
	rem_head(P, F, Head),
	rem_head(Q, F, Head).



%@  @item rem_add_link(@var{?Node1}, @var{?Node2}, @var{+OldREM}, @var{-NewREM})
%@  @PLXindex {rem_add_link/4 (rem)}
%@  is true when adding the equivalence @var{Node1===Node2} to the partition
%@  represented by @var{OldREM} yields a partition which is represented by
%@  @var{NewREM}.  If @var{Node1} or @var{Node2} is uninstantiated, it will backtrack
%@  over all the nodes.  It's not clear how useful this is.

rem_add_link(P0, Q0, rem(Size,OldF), rem(Size,NewF)) :-
	between(1, Size, P0),
	between(1, Size, Q0),
	get_value(P0, OldF, P1),
	get_value(Q0, OldF, Q1),
	rem_add_link_1(P0, P1, Q0, Q1, OldF, NewF).


rem_add_link_1(P0, P1, Q0, Q1, OldF, NewF) :-
	(   Q1 < P1 ->
	    put_value(P0, OldF, Q1, MidF),
	    get_value(P1, OldF, P2),
	    rem_add_link_1(P1, P2, Q0, Q1, MidF, NewF)
	;   P1 < Q1 ->
	    put_value(Q0, OldF, P1, MidF),
	    get_value(Q1, OldF, Q2),
	    rem_add_link_1(P0, P1, Q1, Q2, MidF, NewF)
	; /*P1 = Q1*/
	    NewF = OldF
	).


%   get_value(+Index, +Tree, ?Value)
%   unifies Value with the Indexth element of the array represented
%   by the four-way tree Tree.  It assumes that Index >= 1, which is
%   true in this file because we keep calling between/3 to check.

get_value(Index, Tree, Value) :-
	(   Index < 4 ->
	    arg(Index, Tree, Value)
	;   K is (Index/\3)+4,
	    J is Index >> 2,
	    arg(K, Tree, Son),
	    get_value(J, Son, Value)
	).



%   put_value(+Index, +OldTree, +Value, -NewTree)
%   replaces the Indexth element of OldTree by value, giving NewTree.
%   In the usual case, OldTree is not changed.  If, however, NewTree
%   and OldTree are the same, this can be used to bind a previously
%   non-existent element of the tree to Value.

put_value(Index, OldTree, Value, NewTree) :-
	(   Index < 4 ->
	    replace_arg(Index, OldTree, _, NewTree, Value)
	;   K is (Index/\3)+4,
	    J is Index >> 2,
	    replace_arg(K, OldTree, OldSon, NewTree, NewSon),
	    put_value(J, OldSon, Value, NewSon)
	).


replace_arg(1, node(A,B,C,D,E,F,G), A, node(X,B,C,D,E,F,G), X).
replace_arg(2, node(A,B,C,D,E,F,G), B, node(A,X,C,D,E,F,G), X).
replace_arg(3, node(A,B,C,D,E,F,G), C, node(A,B,X,D,E,F,G), X).
replace_arg(4, node(A,B,C,D,E,F,G), D, node(A,B,C,X,E,F,G), X).
replace_arg(5, node(A,B,C,D,E,F,G), E, node(A,B,C,D,X,F,G), X).
replace_arg(6, node(A,B,C,D,E,F,G), F, node(A,B,C,D,E,X,G), X).
replace_arg(7, node(A,B,C,D,E,F,G), G, node(A,B,C,D,E,F,X), X).

%@  @end table
