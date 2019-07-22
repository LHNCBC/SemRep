:- module(dsizes, [dsizes/1]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(search).

%% this is a program for finding the smallest square in which on can pack n rectangles which all have distinct sizes
%% see question 2 of:  http://www.stetson.edu/~efriedma/mathmagic/0899.html

%% N = 7
dsizes(N) :-
	top(N, L, SumSurf),
	sqr(1, SumSurf, SqSize),
	(   fromto(L,[L1,L2|R],R,[]),
	    fromto(0-[],K1-XY1,Lrg2,_-[Xl,Yl]),
	    foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(S,XSizes),
	    foreach(T,YSizes),
	    foreach(task(X,S,E1,T,0),Tasks1),
	    foreach(task(Y,T,E2,S,0),Tasks2),
	    foreach(object(I,I,[X,Y]),Objects),
	    foreach(sbox(I,[0,0],[S,T]),Sboxes),
	    count(I,1,_),
	    param(SqSize)
	do  E1 in 0..SqSize,
	    E2 in 0..SqSize,
	    X in 0..SqSize,
	    Y in 0..SqSize,
	    S in {L1,L2},
	    T in {L1,L2},
	    S #\= T,
	    X+S #=< SqSize,
	    Y+T #=< SqSize,
	    K2 is L1*L2,
	    (   K1 >= K2 -> Lrg2 = K1-XY1
	    ;   Lrg2 = K2-[X,Y]
	    )
	),
	(   foreach(Pl,[Xl,Yl])
	do  fd_max(Pl, Max1),
	    Max2 is Max1//2,
	    Pl in 0..Max2
	),
	cumulative(Tasks1, [global(true),limit(SqSize)]),
	cumulative(Tasks2, [global(true),limit(SqSize)]),
	labeling([], XSizes),
	geost(Objects, Sboxes),
	search(Xs, Ys, XSizes, YSizes),
	(   foreach(object(_,S1,Orig),Objects),
	    foreach(Size:Orig,Placement),
	    fromto(Sboxes,SS,SS1,_)
	do  selectchk(sbox(S1,_,Size), SS, SS1)
	),	
	format('placement space = ~dx~d\n', [SqSize,SqSize]),
	format('~d squares = ~w\n', [N,Placement]).

search(Xs, Ys, XSizes, YSizes) :-
	by_area(Xs, XSizes, YSizes, Xs1, XSizes1),
	by_area(Ys, YSizes, XSizes, Ys1, YSizes1),
	by_intervals(Xs1, XSizes1, 0.5),
	labeling([bisect], Xs1),
	by_intervals(Ys1, YSizes1, 0.5),
	labeling([bisect], Ys1).

by_area(Xs, XSizes, YSizes, Xs1, XSizes1) :-
	(   foreach(X,Xs),
	    foreach(W,XSizes),
	    foreach(H,YSizes),
	    foreach(Tag-(X-W),L1)
	do  Tag is -W*H
	),
	keysort(L1, L2),
	(   foreach(_-(X1-W1),L2),
	    foreach(X1,Xs1),
	    foreach(W1,XSizes1)
	do  true
	).

sqr(I, S, R) :-
	J is I*I,
	J < S, !,
	I1 is I+1,
	sqr(I1, S, R).
sqr(R, _, R).

				% N: number of rectangles
top(N, L, SumSurf) :-
	L = [1|_],		% Mats
	N2 is N+N,		% number of rectangles sizes
	length(L, N2),		% list of rectangles sizes
	domain(L, 1, N2),	% domain of rectangles sizes
	(   fromto(L,[SX,SY|R],R,[]),
	    fromto(0,SX0,SX,_),
	    fromto(0,Sum1,Sum2,SumSurf),
	    for(I,1,N),
	    fromto(0,S1,S2,SMin),
	    param(N2)
	do  SX #< SY,
	    SX0 #< SX,
	    Sum2 #= Sum1+SX*SY,
	    S2 is S1+I*(N2-I+1)
	),
	SumSurf #>= SMin,
	all_distinct(L),	% distinct rectangles sizes
	indomain(SumSurf),
	reverse(L, RL),		% label first of large rectangles
	labeling([leftmost], RL).

