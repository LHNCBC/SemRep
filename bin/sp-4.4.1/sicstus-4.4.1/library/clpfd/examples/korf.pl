:- module(korf, [korf/1]).

:- use_module(library(clpfd)).
:- use_module(search, [interval_labeling/2]).

/*

main program to run tests for placing squares in minimal area rectangle

*/

korf(N) :-
	(   for(I,1,N),
	    fromto(0,LB0,LB1,LB)
	do  LB1 is LB0+I*I
	),
        UB is LB+200, % dummy value
        findall(param(Size,W,H,N),
                param(Size,W,H,N,LB,UB), L),
	fd_statistics(backtracks, _),
        sort(L, Sorted),
	member(Param, Sorted),
	try_rectangle(Param, Rectangles), !,
	Param = param(Size, W, H, _),
	format('total size = ~d\n', [Size]),
	format('placement space = ~dx~d\n', [W,H]),
	format('rectangles r(X,W,Y,H) = ~w\n', [Rectangles]).

param(Size, W, H, N, LB, UB) :-
        domain([W,H], N, LB),
        W*H #>= LB,
        W*H #=< UB,
        W #=< H,
        indomain(W),
        indomain(H),
        (   W < 2*N-1,
	    K is (W+1)//2,
	    H < (N*N+N-(K-1)*(K-1)-(K-1))//2 -> fail
        ;   true
        ),
        Size is W*H.
        
try_rectangle(param(_,Width,Height,N), Rects) :-
	(   fromto(N,S,S1,1),
	    foreach(S,Sizes),
	    foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(r(X,S,Y,S),Rects),
	    foreach(object(S,S,[X,Y]),Objs),
	    foreach(sbox(S,[0,0],[S,S]),Shapes),
	    param(Width,Height)
	do  S1 is S-1,
	    Wd is Width-S+1,
	    Hd is Height-S+1,
	    X in 1..Wd,
	    Y in 1..Hd,
	    remove_both(X, Y, S, Width, Height)
	),
	lower_quadrant(Xs, Ys, Sizes, Width, Height),
	geost(Objs, Shapes, [cumulative(true),
			     longest_hole(true,-1),
			     parconflict(true),
			     visavis_init(true)]),
        interval_labeling(Rects, 0.75).

lower_quadrant([X|_], [Y|_], [D|_], Width, Height) :-
	UBx is (Width-D+2)>>1,
	X in 1..UBx,
        UBy is (Height+1)>>1,
	Y in 1..UBy.

/*
remove values close to edge which would create a dominated gap
*/

remove_both(_X, _Y, 1, _, _) :- !.
remove_both(X, Y, S, W, H) :-
        forbidden_gap(S, From, To),
	(   for(I,From,To),
	    param(X,Y,S,W,H)
	do  I1 is I+1,
	    X #\= I1,     % domains start from 1, so shift gap by 1
	    Y #\= I1,
	    Xr is W-S+1-I,
	    Yr is H-S+1-I,
	    (   Xr > 1 -> X #\= Xr
	    ;   true
	    ),
	    (   Yr > 1 -> Y #\= Yr
	    ;   true
	    )
	).

/*
check gaps between large rectangles
*/

forbidden_gap(S, From, To) :-
        gap(N, From, To),
        N =< S, !.

/*
gap table calculated by simple rectangle layout 
*/

gap(45, 1, 10).
gap(35, 1, 9).
gap(30, 1, 8).
gap(22, 1, 7).
gap(18, 1, 6).
gap(12, 1, 5).
gap(9, 1, 4).
gap(5, 1, 3).
gap(3, 1, 2).
gap(2, 1, 1).

