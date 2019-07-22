:- module(almost_squares, [almost_squares/1]).

:- use_module(library(clpfd)).
:- use_module(search, [interval_labeling/2]).

/*

main program to run tests for placing squares in minimal area rectangle

*/

almost_squares(N) :- 
	(   for(I,1,N),
	    fromto(0,LB0,LB1,LB)
	do  LB1 is LB0+I*(I+1)
	),
        UB is LB+2000, % dummy value
        findall(param(Size,W,H,N),
                param(Size,W,H,N,LB,UB), L),
	fd_statistics(backtracks, _),
        sort(L, Sorted),
	member(Param, Sorted),
	try_rectangle(Param, Rectangles), !,
	Param = param(Size,W,H,_),
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
        Size is W*H.

try_rectangle(param(_,Width,Height,N), Rect) :-
	(   foreach(S,Sizes),
	    foreach(r(X,W,Y,H),Rect),
	    foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(W,Ws),
	    foreach(H,Hs),
	    foreach(object(OId,SId,[X,Y]),Objects),
	    fromto(N,S,S2,1),
	    fromto(2,S0,S1,Surface),
	    fromto(0,OId,OId1,_),
	    fromto(Shapes,[sbox(SId1,[0,0],[S,OSize]),sbox(SId2,[0,0],[OSize,S])|Shapes1],Shapes1,[]),
	    param(N,Width,Height)
	do  OSize is S+1,
	    W in S..OSize,
	    H in S..OSize,
	    X in 1..Width,
	    Y in 1..Height,
	    X+W-1 #=< Width,
	    Y+H-1 #=< Height,
	    SId1 is OId,
	    SId2 is OId + 1,
	    SId in SId1..SId2,
	    SId #= SId1 #<=> W #= S,
	    SId #= SId2 #<=> H #= S,
	    S2 is S - 1,
	    S1 is S0+S*OSize,
	    OId1 is OId + 2
	),
	lower_quadrant(Rect, Sizes, Width, Height),
        Slack is Width*Height-Surface,
	(   foreach(r(X1,_W,Y1,_H),Rect),
	    foreach(Size,Sizes),
	    param(Slack,Width,Height)
	do  gap_lp(1, 10, Slack, X1, Y1, Size, Width, Height)
	),
	cumulative(Xs, Ws, Hs, Height),
	cumulative(Ys, Hs, Ws, Width),
	geost(Objects, Shapes, [visavis_init(true)]),
        interval_labeling(Rect, 0.30).

lower_quadrant([r(X,_,Y,_)|_], [D|_], Width, Height) :-
	UBx is (Width-D+2)>>1,
	X in 1..UBx,
        UBy is (Height+1)>>1,
	Y in 1..UBy.

cumulative(Os, Ds, Hs, L) :-
	(   foreach(O,Os),
	    foreach(D,Ds),
	    foreach(H,Hs),
	    foreach(task(O,D,_,H,0),Tasks)
	do  true
	),
	cumulative(Tasks, [limit(L),global(true)]).

gap_lp(D, End, _, _, _, _, _, _) :- 
        D > End, !.
gap_lp(D, End, Slack, X, Y, Size, Width, Height) :-
        D1 is D+1,
        (   gap(Size, D, V),
	    V > Slack ->
	    X #\= D1,
            Y #\= D1
        ;   true
        ),
        Size1 is Size+1,
        DD is D-1,
        (   gap(Size1, D, V1),
	    gap(Size, DD, V2),
	    V1 > Slack,
            V2 > Slack ->
            R1 is Width-Size+1-D,
            X #\= R1,
            R2 is Height-Size+1-D,
            Y #\= R2
        ;   true
        ),
        gap_lp(D1, End, Slack, X, Y, Size, Width, Height).

gap(3, 1, 1).
gap(4, 1, 2).
gap(5, 1, 3).
gap(5, 2, 2).
gap(6, 1, 4).
gap(6, 2, 4).
gap(7, 1, 5).
gap(7, 2, 6).
gap(7, 3, 1).
gap(8, 1, 6).
gap(8, 2, 8).
gap(8, 3, 4).
gap(9, 1, 7).
gap(9, 2, 10).
gap(9, 3, 7).
gap(10, 1, 8).
gap(10, 2, 12).
gap(10, 3, 10).
gap(11, 1, 9).
gap(11, 2, 14).
gap(11, 3, 13).
gap(11, 4, 4).
gap(12, 1, 10).
gap(12, 2, 16).
gap(12, 3, 16).
gap(12, 4, 8).
gap(13, 1, 11).
gap(13, 2, 18).
gap(13, 3, 19).
gap(13, 4, 12).
gap(14, 1, 12).
gap(14, 2, 20).
gap(14, 3, 22).
gap(14, 4, 16).
gap(15, 1, 13).
gap(15, 2, 22).
gap(15, 3, 25).
gap(15, 4, 20).
gap(15, 5, 5).
gap(16, 1, 14).
gap(16, 2, 24).
gap(16, 3, 28).
gap(16, 4, 24).
gap(16, 5, 10).
gap(17, 1, 15).
gap(17, 2, 26).
gap(17, 3, 31).
gap(17, 4, 28).
gap(17, 5, 15).
gap(18, 1, 16).
gap(18, 2, 28).
gap(18, 3, 34).
gap(18, 4, 32).
gap(18, 5, 20).
gap(18, 6, 1).
gap(19, 1, 17).
gap(19, 2, 30).
gap(19, 3, 37).
gap(19, 4, 36).
gap(19, 5, 25).
gap(19, 6, 2).
gap(20, 1, 18).
gap(20, 2, 32).
gap(20, 3, 40).
gap(20, 4, 40).
gap(20, 5, 30).
gap(20, 6, 8).
gap(21, 1, 19).
gap(21, 2, 34).
gap(21, 3, 43).
gap(21, 4, 44).
gap(21, 5, 35).
gap(21, 6, 14).
gap(22, 1, 20).
gap(22, 2, 36).
gap(22, 3, 46).
gap(22, 4, 48).
gap(22, 5, 40).
gap(22, 6, 20).
gap(23, 1, 21).
gap(23, 2, 38).
gap(23, 3, 49).
gap(23, 4, 52).
gap(23, 5, 45).
gap(23, 6, 26).
gap(23, 7, 2).
gap(24, 1, 22).
gap(24, 2, 40).
gap(24, 3, 52).
gap(24, 4, 56).
gap(24, 5, 50).
gap(24, 6, 32).
gap(24, 7, 3).
gap(25, 1, 23).
gap(25, 2, 42).
gap(25, 3, 55).
gap(25, 4, 60).
gap(25, 5, 55).
gap(25, 6, 38).
gap(25, 7, 7).
gap(26, 1, 24).
gap(26, 2, 44).
gap(26, 3, 58).
gap(26, 4, 64).
gap(26, 5, 60).
gap(26, 6, 44).
gap(26, 7, 14).
gap(26, 8, 1).
gap(27, 1, 25).
gap(27, 2, 46).
gap(27, 3, 61).
gap(27, 4, 68).
gap(27, 5, 65).
gap(27, 6, 50).
gap(27, 7, 21).
gap(27, 8, 2).
gap(28, 1, 26).
gap(28, 2, 48).
gap(28, 3, 64).
gap(28, 4, 72).
gap(28, 5, 70).
gap(28, 6, 56).
gap(28, 7, 28).
gap(28, 8, 3).
gap(29, 1, 27).
gap(29, 2, 50).
gap(29, 3, 67).
gap(29, 4, 76).
gap(29, 5, 75).
gap(29, 6, 62).
gap(29, 7, 35).
gap(29, 8, 4).

