%   Package: heaps
%   Author : Richard A. O'Keefe
%   Updated: 08/25/98
%   Defines: Priority queues (also known as heaps)

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

%@  A heap is a labelled binary tree where the key of each node is less
%@  than or equal to the keys of its sons.  The point of a heap is that
%@  we can keep on adding new elements to the heap and we can keep on
%@  taking out the minimum element.  If there are @var{N} elements total, the
%@  total time is @var{O(N lg N)}.  If you know all the elements in advance, you
%@  are better off doing a merge-sort, but this file is for when you
%@  want to do say a best-first search, and have no idea when you start
%@  how many elements there will be, let alone what they are.
%@  
%@  A heap is represented as a triple @code{heap(N,Free,Tree)} where @var{N} is the
%@  number of elements in the tree, @var{Free} is a list of integers which
%@  specifies unused positions in the tree, and @var{Tree} is a tree made of:
%@  @table @code
%@  @item heap
%@  terms for empty subtrees and
%@  @item heap(@var{Key},@var{Datum},@var{Lson},@var{Rson}) 
%@  terms for the rest
%@  @end table
%@  
%@  The nodes of the tree are notionally numbered like this:
%@  
%@  @example
%@                                  1
%@                   2				    3
%@           4               6               5               7
%@       8      12      10     14       9       13      11     15
%@    ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..  ..
%@  @end example
%@  The idea is that if the maximum number of elements that have been in
%@  the heap so far is @var{M}, and the tree currently has @var{K} elements, the tree
%@  is some subtreee of the tree of this form having exactly @var{M} elements,
%@  and the @var{Free} list is a list of @var{M-K} integers saying which of the 
%@  positions in the @var{M}-element tree are currently unoccupied.  This free
%@  list is needed to ensure that the cost of passing @var{N} elements through
%@  the heap is @var{O(N lg M)} instead of @var{O(N lg N)}.  For @var{M} say 100 and @var{N} say 10^4
%@  this means a factor of two.  The cost of the free list is slight.
%@  The storage cost of a heap in a copying Prolog is @var{2K+3M} words.

:- module(heaps, [
	add_to_heap/4,		%   Heap x Key x Datum -> Heap
	empty_heap/1,		%   -> Heap
	delete_from_heap/4,	%   Heap -> Key x Datum x Heap
	get_from_heap/4,	%   Heap -> Key x Datum x Heap
	heap_size/2,		%   Heap -> Size
	heap_to_list/2,		%   Heap -> List
	is_heap/1,		%   Heap ->
	list_to_heap/2,		%   List -> Heap
	min_of_heap/3,		%   Heap -> Key x Datum
	min_of_heap/5,		%   Heap -> (Key x Datum) x (Key x Datum)
	portray_heap/1		%   Heap ->
   ]).
:- mode
	add_to_heap(+, +, +, -),
	    add_to_heap(+, +, +, +, -),
		add_to_heap(+, +, +, +, +, +, +, +, -),
	empty_heap(?),
	get_from_heap(+, ?, ?, -),
	    repair_heap(+, +, +, -),
		less_than_key(+, +),
	heap_size(+, ?),
	heap_to_list(+, -),
	    heap_tree_to_list(+, -),
		heap_tree_to_list(+, +, -),
		    less_than_head(+, +),
	is_heap(+),
	    is_heap(+, +, +, ?),
	list_to_heap(+, -),
	min_of_heap(+, ?, ?),
	min_of_heap(+, ?, ?, ?, ?),
	    min_of_heap(+, +, ?, ?),
	portray_heap(+),
	    portray_heap(+, +).


%@  Exported predicates:
%@  
%@  @table @code
%@  @item add_to_heap(@var{+OldHeap}, @var{+Key}, @var{+Datum}, @var{-NewHeap})
%@  @item add_to_heap/4 (heaps)
%@  inserts the new @var{Key-Datum} pair into the heap.  The insertion is
%@  not stable, that is, if you insert several pairs with the same
%@  @var{Key} it is not defined which of them will come out first, and it
%@  is possible for any of them to come out first depending on the 
%@  history of the heap.  
%   If you need a stable heap, you could add
%   a counter to the heap and include the counter at the time of
%   insertion in the key.  If the free list is empty, the tree will
%   be grown, otherwise one of the empty slots will be re-used.  (I
%   use imperative programming language, but the heap code is as 
%   pure as the trees code, you can create any number of variants
%   starting from the same heap, and they will share what common
%   structure they can without interfering with each other.)

add_to_heap(heap(M,[],OldTree), Key, Datum, NewHeap) :- !,
        NewHeap = heap(N,[],NewTree),
	N is M+1,
	add_to_heap(N, Key, Datum, OldTree, NewTree).
add_to_heap(heap(M,[H|T],OldTree), Key, Datum, heap(N,T,NewTree)) :-
	N is M+1,
	add_to_heap(H, Key, Datum, OldTree, NewTree).


add_to_heap(1, Key, Datum, _, heap(Key,Datum,heap,heap)) :- !.
add_to_heap(N, Key, Datum, heap(K1,D1,L1,R1), Heap) :-
	E is N /\ 1,
	M is N >> 1,	% M > 0
	(   Key @< K1 ->
	    add_to_heap(E, K1, D1, M, L1, R1, Key, Datum, Heap)
	;/* Key @>= K1 */
	    add_to_heap(E, Key, Datum, M, L1, R1, K1, D1, Heap)
	).


add_to_heap(0, Key, Datum, N, L1, R1, K2, D2, heap(K2,D2,L2,R1)) :-
	add_to_heap(N, Key, Datum, L1, L2).
add_to_heap(1, Key, Datum, N, L1, R1, K2, D2, heap(K2,D2,L1,R2)) :-
	add_to_heap(N, Key, Datum, R1, R2).



%@  @item delete_from_heap(@var{+OldHeap}, @var{+Key}, @var{-Datum}, @var{-NewHeap})
%@  @item delete_from_heap/4 (heaps)
%@  deletes a single @var{Key-Datum} pair from the @var{OldHeap} producing
%@  a @var{NewHeap}.  This is useful if you want to e.g. change
%@  the priority of Datum.

delete_from_heap(heap(N,Free,Tree0), Key, Datum, Heap) :-
	delete_from_heap(Tree0, Key, Datum, Tree, Hole), !,
	M is N-1,
	(   M =:= 0 -> empty_heap(Heap)
	;   Heap = heap(M,[Hole|Free],Tree)
	).

delete_from_heap(heap(K,D,L,R), Key, Datum, Tree, Hole) :-
	compare(C, Key, K),
	delete_from_heap(C, K, D, L, R, Key, Datum, Tree, Hole).

delete_from_heap(=, _, Datum, L, R, _, Datum, Tree, Hole) :-
	repair_heap(L, R, Tree, Hole).
delete_from_heap(>, K, D, L, R, Key, Datum, Tree, Hole) :-
	delete_from_heap(L, Key, Datum, L1, Hole0), !,
        Tree = heap(K,D,L1,R),
	Hole is (Hole0<<1).	% Jonas Barklund 1999-09-29.
delete_from_heap(>, K, D, L, R, Key, Datum, heap(K,D,L,R1), Hole) :-
	delete_from_heap(R, Key, Datum, R1, Hole0),
	Hole is (Hole0<<1) \/ 1. % Jonas Barklund 1999-09-29.



%@  @item get_from_heap(@var{+OldHeap}, @var{-Key}, @var{-Datum}, @var{-NewHeap})
%@  @item get_from_heap/4 (heaps)
%@  returns the @var{Key-Datum} pair in @var{OldHeap} with the smallest @var{Key}, and
%@  also a @var{NewHeap} which is the @var{OldHeap} with that pair deleted.
%   The easy part is picking off the smallest element.  The hard part
%   is repairing the heap structure.  repair_heap/4 takes a pair of
%   heaps and returns a new heap built from their elements, and the
%   position number of the gap in the new tree.  Note that repair_heap
%   is *not* tail-recursive.

get_from_heap(heap(N,Free,heap(Key,Datum,L,R)), Key, Datum, Heap) :-
	M is N-1,
	(   M=:=0 -> empty_heap(Heap)
	;   Heap = heap(M,[Hole|Free],Tree),
	    repair_heap(L, R, Tree, Hole)
	).

repair_heap(Heap1, heap(K2,D2,L2,R2), heap(K2,D2,Heap1,R3), Hole) :-
	less_than_key(Heap1, K2),
	!,
	repair_heap(L2, R2, R3, Hole0),
	Hole is (Hole0<<1) \/ 1.
repair_heap(heap, heap, heap, 1).
repair_heap(heap(K1,D1,L1,R1), Heap2, heap(K1,D1,L3,Heap2), Hole) :-
	repair_heap(L1, R1, L3, Hole0),
	Hole is (Hole0<<1).

less_than_key(heap, _).
less_than_key(heap(K1,_,_,_), K2) :- K2 @< K1.



%@  @item heap_size(@var{+Heap}, @var{-Size})
%@  @item heap_size/2 (heaps)
%@  reports the number of elements currently in the heap.

heap_size(heap(Size,_,_), Size).



%@  @item heap_to_list(@var{+Heap}, @var{-List})
%@  @item heap_to_list/2 (heaps)
%@  returns the current set of @var{Key-Datum} pairs in the @var{Heap} as a
%@  @var{List}, sorted into ascending order of @var{Keys}.  
%   This is included
%   simply because I think every data structure foo ought to have
%   a foo_to_list and list_to_foo relation (where, of course, it
%   makes sense!) so that conversion between arbitrary data
%   structures is as easy as possible.  This predicate is basically
%   just a merge sort, where we can exploit the fact that the tops
%   of the subtrees are smaller than their descendants.

heap_to_list(heap(_,_,Tree), List) :-
	heap_tree_to_list(Tree, List).


heap_tree_to_list(heap, []).
heap_tree_to_list(heap(Key,Datum,Lson,Rson), [Key-Datum|Merged]) :-
	heap_tree_to_list(Lson, Llist),
	heap_tree_to_list(Rson, Rlist),
	heap_tree_to_list(Llist, Rlist, Merged).


heap_tree_to_list(L1, [H2|T2], [H2|T3]) :-
	less_than_head(H2, L1),
	!,
	heap_tree_to_list(L1, T2, T3).
heap_tree_to_list([H1|T1], L2, [H1|T3]) :-
	heap_tree_to_list(T1, L2, T3).
heap_tree_to_list([], L2, L2).


less_than_head(X, [H|_]) :-
	X @< H.



%@  @item list_to_heap(@var{+List}, @var{-Heap})
%@  @item list_to_heap/2 (heaps)
%@  takes a proper list of @var{Key-Datum} pairs (such as @code{keysort/2} could be used to
%@  sort) and forms them into a heap.  
%   We could do that a wee bit
%   faster by keysorting the list and building the tree directly, but
%   this algorithm makes it obvious that the result is a heap, and
%   could be adapted for use when the ordering predicate is not @<
%   and hence keysort is inapplicable.

list_to_heap(List, heap(N,[],Heap)) :-
	(   foreach(Key-Datum,List),
	    count(I,1,N),
	    fromto(heap,OldTree,MidTree,Heap)
	do  add_to_heap(I, Key, Datum, OldTree, MidTree)
	).

%@  @item empty_heap(@var{?Heap})
%@  @item empty_heap/1 (heaps)
%@  is true when @var{Heap} represents an empty heap.  There is only one
%@  way it can be true.

empty_heap(heap(0,[],heap)).



%@  @item is_heap(@var{+Heap})
%@  @item is_heap/1 (heaps)
%@  is true when @var{Heap} is a well formed heap.  For this to be true, the
%@  size must be right and the tree must satisfy the heap condition.

is_heap(heap(Size, _, Tree)) :-		    % Note: heap/3
	integer(Size),
	(   Size =:= 0 ->
	    Tree = heap
	;   Size > 0,
	    Tree = heap(K,_,L,R),	    % Note: heap/4
	    is_heap(L, K, 1, Size1),
	    is_heap(R, K, Size1, Size)
	).


is_heap(heap, _, Size, Size).
is_heap(heap(K,_,L,R), Min, Size0, Size) :-
	K @>= Min,
	Size1 is Size0+1,
	is_heap(L, K, Size1, Size2),
	is_heap(R, K, Size2, Size).




%@  @item min_of_heap(@var{+Heap}, @var{-Key}, @var{-Datum})
%@  @item min_of_heap/3 (heaps)
%@  returns the @var{Key-Datum} pair at the top of the heap (which is of
%@  course the pair with the smallest @var{Key}), but does not remove it
%@  from the heap.  It fails if the heap is empty.

min_of_heap(heap(_,_,heap(Key,Datum,_,_)), Key, Datum).


%@  @item min_of_heap(@var{+Heap}, @var{-Key1}, @var{-Datum1}, @var{-Key2}, @var{-Datum2})
%@  @item min_of_heap/5 (heaps)
%@  returns the smallest (@var{Key1}) and second smallest (@var{Key2}) pairs in
%@  the heap, without deleting them.  It fails if the heap does not
%@  have at least two elements.

min_of_heap(heap(_,_,heap(Key1,Datum1,Lson,Rson)), Key1, Datum1, Key2, Datum2) :-
	min_of_heap(Lson, Rson, Key2, Datum2).


min_of_heap(heap, heap(Kb,Db,_,_), Kb, Db).
min_of_heap(heap(Ka,_,_,_), heap(Kb,Db,_,_), K, D) :- Kb @< Ka, !,
	K = Kb, D = Db.
min_of_heap(heap(Ka,Da,_,_), _, Ka, Da).



%@  @item portray_heap(@var{+Heap})
%@  @item portray_heap/1 (heaps)
%@  writes a heap to the current output stream in a pretty format so that
%@  you can easily see what it is.  Note that a heap written out this way
%@  can @emph{not} be read back in.  The point of this predicate is that you can
%@  add a clause
%@  @example
%@  @group
%@      portray(X) :- is_heap(X), !, portray_heap(X).
%@  @end group
%@  @end example
%   to get heaps displayed nicely by @code{print/1}.  
%   It isn't much use for
%   watching the predicates of this file at work, as they work with heaps
%   in incomplete states of instantiation.  To do that, use the clause
%	portray(X) :- portray_heap(X).
%   which should be safe enough.

portray_heap(heap(0,[],heap)) :- !,
	write('Heap{'), write('}').
portray_heap(heap(_,_,Heap)) :-
	portray_heap(Heap, 'heap{'),
	write('}').

portray_heap(heap, _).
portray_heap(heap(Key,Datum,Lson,Rson), Prefix) :-
	write(Prefix),
	print(Datum), write('->'), print(Key),
	portray_heap(Lson, ', '),
	portray_heap(Rson, ', ').

%@  @end table
