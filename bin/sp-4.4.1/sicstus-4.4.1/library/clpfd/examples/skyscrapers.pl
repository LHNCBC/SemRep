:- module(skyscrapers, [skyscrapers/1]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

/***

Every row and colum in the diagram contains skyscrapers of different
heights, exactly those heights indicated at the side of the puzzle. No
two skyscrapers of the same height are in the same row or column. The
numbers around the diagram denote how many skyscrapers are visible
from that direction: higher skyscrapers block lower ones.

***/

:- dynamic data/6.
data(a, 4,
     [_,1,2,_],			% top
     [_,2,_,3],			% left
     [_,_,1,_],			% bottom
     [_,1,3,_]).		% right
data(572, 4,
     [_,_,_,4],
     [_,_,3,2],
     [_,_,_,_],
     [_,_,_,_]).
data(537, 4,
     [1,3,3,2],
     [1,4,2,3],
     [3,2,1,3],
     [2,1,2,2]).
data(718, 5,
     [_,_,_,_,_],
     [_,_,_,_,_],
     [_,_,_,_,_],
     [4,3,2,_,5]).
data(463, 4,
     [_,2,_,2],
     [_,_,2,2],
     [2,_,2,_],
     [_,_,2,2]).
data(461, 5,
     [3,1,2,2,2],
     [2,3,2,1,4],
     [2,4,3,1,2],
     [3,3,1,3,2]).
data(688, 5,
     [2,2,1,2,3],
     [3,1,3,2,3],
     [3,2,4,2,1],
     [3,2,2,4,1]).
data(573, 5,
     [2,4,3,1,2],
     [4,1,3,3,2],
     [2,1,3,3,2],
     [2,3,3,1,2]).
data(900, 5,
     [_,_,_,_,_],
     [5,2,3,1,2],
     [_,_,_,_,_],
     [1,4,2,4,2]).
data(1049, 5,
     [_,2,_,2,3],
     [3,2,_,4,_],
     [2,4,3,_,_],
     [_,2,3,_,2]).
data(909, 6,
     [_,_,_,4,2,_],
     [_,_,2,3,_,_],
     [_,_,3,_,4,5],
     [_,3,_,_,_,3]).
data(1174, 6,
     [_,4,_,3,_,3],
     [_,_,_,4,_,3],
     [_,_,_,_,4,2],
     [_,3,5,_,2,_]).
data(1489, 6,
     [3,2,3,2,4,1],
     [3,3,2,1,3,2],
     [2,1,4,3,2,3],
     [1,3,4,3,2,4]).
data(575, 6,
     [1,2,2,4,3,3],
     [1,2,2,4,4,4],
     [5,3,5,3,1,2],
     [4,2,5,3,1,2]).
data(602, 6,
     [_,_,3,_,5,_],
     [_,_,4,1,4,_],
     [_,2,_,3,_,_],
     [_,2,2,3,_,4]).
data(627, 6,
     [1,3,3,3,2,2],
     [1,3,2,2,3,3],
     [5,2,3,1,2,2],
     [3,2,3,4,1,2]).
data(576, 7,
     [1,4,4,3,2,3,4],
     [1,5,2,2,3,4,4],
     [4,4,3,5,4,2,1],
     [4,3,4,2,3,2,1]).
data(582, 7,
     [4,4,2,3,2,1,3],
     [3,3,3,5,2,1,2],
     [2,1,2,3,3,3,2],
     [2,3,2,1,2,4,2]).
data(707, 8,
     [4,_,1,4,6,_,_,_],
     [_,_,2,2,_,4,_,4],
     [4,_,6,5,_,3,4,_],
     [_,_,1,_,_,3,3,_]).

skyscrapers(ID) :-
	data(ID, N, Top, Left, Bottom, Right),
	(   for(I,1,N),
	    foreach(I,Base),
	    foreach(Row,Rows),
	    fromto(Fmts,["~t~w,~6+"|Tail],Tail,["~n~n"]),
	    param(N)
	do  length(Row,N)
	),
	append(Fmts, Fmt),
	transpose(Rows, Cols),
	append(Rows, Cells),
	domain(Cells, 1, N),
	findall(Perm, perm(Base,Perm), Perms),
	(   foreach(P,Perms),
	    foreach([V1,V2|P],Ext1)
	do  visibility(P, V1),
	    reverse(P, P2),
	    visibility(P2, V2)
	),
	sort(Ext1, Ext2),
	(   foreach(T,Top),
	    foreach(B,Bottom),
	    foreach(L,Left),
	    foreach(R,Right),
	    foreach(C1,Cols),
	    foreach(R1,Rows),
	    fromto(Ts,Ts1,Ts3,[])
	do  (   var(T), var(B) ->
		Ts1 = Ts2,
		all_distinct(C1)
	    ;   Ts1 = [[T,B|C1]|Ts2]
	    ),
	    (   var(L), var(R) ->
		Ts2 = Ts3,
		all_distinct(R1)
	    ;   Ts2 = [[L,R|R1]|Ts3]
	    )
	),
	table(Ts, Ext2),
	labeling([ff,enum], Cells),
	(   foreach(R2,Rows),
	    param(Fmt)
	do  format(Fmt, R2)
	).

visibility([P1|Ps], V) :-
	(   foreach(Q,Ps),
	    fromto(P1,P2,P3,_),
	    fromto(1,V1,V2,V)
	do  (Q>P2 -> P3=Q, V2 is V1+1 ; P3=P2, V2=V1)
	).

