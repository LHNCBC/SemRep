%   Package: trees
%   Author : Richard A. O'Keefe
%   Updated: 29 Aug 1989
%   Purpose: Updatable binary trees.
%   SeeAlso: library(logarr).

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

/*  These are the routines I meant to describe in DAI-WP-150, but the
    wrong version went in.  We have
	list_to_tree : O(N)
	tree_to_list : O(N)
	tree_size    : O(N)
	map_tree     : O(N)
	get_label    : O(lg N)
	put_label    : O(lg N)
    where N is the number of elements in the tree.  The way get_label
    and put_label work is worth noting: they build up a pattern which
    is matched against the whole tree when the position number finally
    reaches 1.  In effect they start out from the desired node and
    build up a path to the root.  They still cost O(lg N) time rather
    than O(N) because the patterns contain O(lg N) distinct variables,
    with no duplications.  put_label simultaneously builds up a pattern
    to match the old tree and a pattern to match the new tree.
*/

:- module(trees, [
	gen_label/3,
	get_label/3,
	list_to_tree/2,
	map_tree/3,
	put_label/4,
	put_label/5,
	tree_size/2,
	tree_to_list/2
   ]).

:- meta_predicate
	map_tree(2, ?, ?),
	    'map tree'(?, ?, 2).

:- mode
	get_label(+, ?, ?),
	    find_node(+, ?, ?),
	list_to_tree(+, -),
	put_label(+, ?, ?, ?),
	    find_node(+, ?, ?, ?, ?),
	tree_size(?, ?),
	    tree_size(+, +, -),
	tree_to_list(+, -),
	    tree_to_list(+, -, -).



/*  The tree starts out looking like this:

				1
		+---------------+---------------+
		2				3
	+-------+-------+		+-------+-------+
	4		5		6		7
    +---+---+	    +---+---+	    +---+---+	    +---+---+
    8	    9	   10	   11	   12	   13	   14	   15
*/

%@  This libary module provides updatable binary trees with logarithmic access time.
%@  Exported predicates:
%@  
%@  @table @code

%@  @item gen_label(@var{?Index}, @var{+Tree}, @var{?Value})
%@  @PLXindex {gen_label/3 (trees)}
%@  assumes that Tree is a proper binary tree, and is true when @var{Value}
%@  is the @var{Index-th} element in @var{Tree}.  Can be used to enumerate
%@  all @var{Values} by ascending @var{Index}.
gen_label(Index, Tree, Value) :-
	gen_label([Tree|Tail], Tail, 1, Index, Value).

:- gen_label/5 is nondet.
gen_label([t(Head,_,_)|_], _, I, I, Head).
gen_label([t(_,Left,Right)|Qhead], [Left,Right|Qtail], I, Index, Value) :-
	J is I+1,
	gen_label(Qhead, Qtail, J, Index, Value).


%@  @item get_label(@var{+Index}, @var{+Tree}, @var{-Label})
%@  @PLXindex {get_label/3 (trees)}
%@  treats the tree as an array of @var{N} elements and returns the @var{Index-th}.
%@  If @var{Index < 1} or @var{> N} it simply fails, there is no such element.  As
%@  Tree need not be fully instantiated, and is potentially unbounded,
%@  we cannot enumerate @var{Indices}.

get_label(N, Tree, Label) :-
	integer(N),
	find_node(N, Tree, t(Label,_,_)).


find_node(N, Tree, Node) :-
    (   N > 1 ->
	M is N >> 1,
	(   N/\1 =:= 0 -> find_node(M, Tree, t(_,Node,_))
	;/* N/\1 =:= 1 */ find_node(M, Tree, t(_,_,Node))		
	)
    ;   N =:= 1 ->
	Tree = Node
    ).



%@  @item list_to_tree(@var{+List}, @var{-Tree})
%@  @PLXindex {list_to_tree/2 (trees)}
%@  takes a given proper @var{List} of @var{N} elements and constructs a binary @var{Tree}
%@  where @code{get_label(@var{K}, @var{Tree}, @var{Lab})} <=> @var{Lab} is the @var{Kth} element of @var{List}.

list_to_tree(List, Tree) :-
	(   foreach(Head,List),
	    fromto([Tree|Tail],[t(Head,Left,Right)|Qhead1],Qhead1,Qhead),
	    fromto(Tail,[Left,Right|Qtail],Qtail,[])
	do  true
	),
	(   foreach(t,Qhead)
	do  true
	).

%@  @item map_tree(@var{:Pred}, @var{+OldTree}, @var{?NewTree})
%@  @PLXindex {map_tree/3 (trees)}
%@  is true when @var{OldTree} and @var{NewTree} are binary trees of the same shape
%@  and @var{Pred(Old,New)} is true for corresponding elements of the two trees.

map_tree(Pred, OldTree, NewTree) :-
	'map tree'(OldTree, NewTree, Pred).

'map tree'(t, t, _).
'map tree'(t(Old,OLeft,ORight), t(New,NLeft,NRight), Pred) :-
	call(Pred, Old, New),
	'map tree'(OLeft, NLeft, Pred),
	'map tree'(ORight, NRight, Pred).



%@  @item put_label(@var{+Index}, @var{+OldTree}, @var{-Label}, @var{-NewTree})
%@  @PLXindex {put_label/[4,5] (trees)}
%@  constructs a new tree the same shape as the old which moreover has the
%@  same elements except that the @var{Index-th} one is @var{Label}.  Unlike the
%@  "arrays" of @code{library(arrays)}, @var{OldTree} is not modified and you can hang on to
%@  it as long as you please.  Note that @var{O(lg N)} new space is needed.

put_label(N, OldTree, Label, NewTree) :-
	integer(N),
	find_node(N, OldTree, t(_,Left,Right),
		     NewTree, t(Label,Left,Right)).

%@  @item put_label(@var{+Index}, @var{+OldTree}, @var{-OldLabel}, @var{-NewTree}, @var{+NewLabel})
%@  is true when @var{OldTree} and @var{NewTree} are trees of the same shape having
%@  the same elements except that the @var{Index-th} element of @var{OldTree} is
%@  @var{OldLabel} and the @var{Index-th} element of @var{NewTree} is @var{NewLabel}.  You can
%@  swap the @var{<Tree,Label>} argument pairs if you like, it makes no difference.

put_label(N, OldTree, OldLabel, NewTree, NewLabel) :-
	integer(N),
	find_node(N, OldTree, t(OldLabel,Left,Right),
		     NewTree, t(NewLabel,Left,Right)).


find_node(N, OldTree, OldNode, NewTree, NewNode) :-
    (	N > 1 ->
	M is N >> 1,
	(   N/\1 =:= 0 ->
	    find_node(M, OldTree, t(Label,OldNode,Right),
			 NewTree, t(Label,NewNode,Right))
	;/* N/\1 =:= 1 */
	    find_node(M, OldTree, t(Label,Left,OldNode),
			 NewTree, t(Label,Left,NewNode))
	)
    ;   N =:= 1 ->
	OldNode = OldTree, NewNode = NewTree
    ).



%@  @item tree_size(@var{+Tree}, @var{-Size})
%@  @PLXindex {tree_size/2 (trees)}
%@  calculates the number of elements in the @var{Tree}.  All trees made by
%@  @code{list_to_tree/2} that are the same size have the same shape.

tree_size(Tree, Size) :-
    (	var(Size) ->
	tree_size(Tree, 0, Size)
    ;	integer(Size) ->
	length(List, Size),
	list_to_tree(List, Tree)
    ).


tree_size(t, Size, Size).
tree_size(t(_,Left,Right), Size0, Size) :-
	tree_size(Right, Size0, Size1),
	Size2 is Size1+1,
	tree_size(Left, Size2, Size).



%@  @item tree_to_list(@var{+Tree}, @var{-List})
%@  @PLXindex {tree_to_list/2 (trees)}
%@  is the converse operation to @code{list_to_tree/2}.  Any mapping or checking
%@  operation can be done by converting the tree to a list, mapping or
%@  checking the list, and converting the result, if any, back to a tree.
%@  It is also easier for a human to read a list than a tree, as the
%@  order in the tree goes all over the place.

tree_to_list(Tree, List) :-
	tree_to_list([Tree|Tail], Tail, List).


tree_to_list([], [], []) :- !.
tree_to_list([t|_], _, []) :- !.
tree_to_list([t(Head,Left,Right)|Qhead], [Left,Right|Qtail], [Head|Tail]) :-
	tree_to_list(Qhead, Qtail, Tail).


%@  @end table
