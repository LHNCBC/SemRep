:- module(spiral, [spiral/1]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

/***
P. Szeredi's Magic Spiral puzzle.

An NxN square board is given.
Place integers 1..M in this board (many cells are not filled) so that
(a) in each row and column, each of 1..M occurs exactly once.
(b) along the spiral starting at the top left corner,
    the integers in the filled cells follow the pattern 1,2..M,1,2..M,...
***/

:- dynamic data/4.
data(a, 7, 4,
     [[_,_,_,_,_,_,_],
      [_,_,_,4,_,_,_],
      [1,_,_,_,_,_,_],
      [_,_,_,_,_,_,_],
      [_,_,_,_,_,_,_],
      [_,_,_,_,_,_,_],
      [_,_,_,_,_,_,_]]). 

spiral(ID) :-
	data(ID, N, M, Rows),
	transpose(Rows, Cols),
	append(Rows, Cells),
	domain(Cells, 0, M),
	Jokers is N-M,
	(   for(I,1,M),
	    fromto(Counts,[I-1|Cs],Cs,[0-Jokers]),
	    fromto(Arcs,[arc(I,0,I),arc(I,J,J)|R],R,[]),
	    param(M)
	do  J is (I mod M) + 1
	),
	(   for(_,1,N),
	    foreach(Row,Rows),
	    foreach(Col,Cols),
	    fromto(Fmts,["~t~w,~6+"|Tail],Tail,["~n~n"]),
	    param(Counts)
	do  global_cardinality(Row, Counts),
	    global_cardinality(Col, Counts)
	),
	append(Fmts, Fmt),
	spiral(N, Rows, Cols, N, Sequence, []),
	automaton(Sequence, [source(M),sink(M)], Arcs),
	labeling([], Cells), % 54 bt
	(   foreach(R2,Rows),
	    param(Fmt)
	do  format(Fmt, R2)
	).

spiral(P, _, _, N) --> {P+P < N+1}, !.
spiral(P, Rows, _, N) --> {P+P =:= N+1}, !, [X],
	{I is P-1},
	{nth0(I, Rows, Row)},
	{nth0(I, Row, X)}.
spiral(P, Rows, Cols, N) -->
	{Low is N-P},
	{High is P-1},
	{nth0(Low, Rows, LowRow)},
	{nth0(High, Rows, HighRow)},
	{nth0(Low, Cols, LowCol)},
	{nth0(High, Cols, HighCol)},
	sublist(Low, 1, High, LowRow),
	sublist(Low, 1, High, HighCol),
	sublist(High, -1, Low, HighRow),
	sublist(High, -1, Low, LowCol),
	{Q is P-1},
	spiral(Q, Rows, Cols, N).

sublist(Low, Inc, High, Row) -->
	(   fromto(Low,L,L1,High),
	    param(Inc,Row)
	do  [Elt],
	    {nth0(L, Row, Elt)},
	    {L1 is L+Inc}
	).

