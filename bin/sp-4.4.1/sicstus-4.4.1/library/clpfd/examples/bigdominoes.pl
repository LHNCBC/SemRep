:- module(bigdominoes, [bigdominoes/1]).

:- use_module(library(clpfd)).
:- use_module(search, [interval_labeling/2]).

/*

main program to run tests for placing bigdominoes in minimal area square

*/

bigdominoes(N) :-
	(   for(I,1,N),
	    fromto(0,LB0,LB1,LB)
	do  LB1 is LB0+2*I*I
	),
        findall(param(Size,WH,N),
                param(Size,WH,N,LB), L),
	fd_statistics(backtracks, _),
        sort(L, Sorted),
	member(Param, Sorted),
	try_rectangle(Param, Rectangles), !,
	Param = param(Size,WH,_),
	format('total size = ~d\n', [Size]),
	format('placement space = ~dx~d\n', [WH,WH]),
	format('rectangles r(X,W,Y,H) = ~w\n', [Rectangles]).

param(Size, WH, N, LB) :-
	WH in N..LB,
        Size #= WH*WH,
        Size #>= LB,
        Size #=< LB+500,
        indomain(WH).

try_rectangle(param(_,WH,N), Rect) :-
	(   fromto(N,S,S1,0),
	    foreach(r(X,W,Y,H),Rect),
	    foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(W,Ws),
	    foreach(H,Hs),
	    fromto(0,OId,OId1,_),
	    foreach(object(OId,SId,[X,Y]),Objects),
	    fromto(Shapes,[sbox(SId1,[0,0],[S,SS]),sbox(SId2,[0,0],[SS,S])|Shapes1],Shapes1,[]),
	    param(WH)
	do  S1 is S-1,
	    SS is 2*S,
	    domain([W,H], S, SS),
	    X in 1..WH,
	    Y in 1..WH,
	    X+W-1 #=< WH,
	    Y+H-1 #=< WH,
	    SId1 is OId,
	    SId2 is OId + 1,
	    SId in SId1..SId2,
	    SId #= SId1 #<=> W #= S,
	    SId #= SId2 #<=> H #= S,
	    OId1 is OId + 2,
	    range_to_fdset((inf..S)\/(SS..sup), Mask),
	    W in_set Mask,
	    H in_set Mask
	),
	geost(Objects, Shapes, [visavis_init(true)]),
	cumulative(Xs, Ws, Hs, WH),
	cumulative(Ys, Hs, Ws, WH),
        interval_labeling(Rect, 0.30).

cumulative(Os, Ds, Hs, L) :-
	(   foreach(O,Os),
	    foreach(D,Ds),
	    foreach(H,Hs),
	    foreach(task(O,D,_,H,0),Tasks)
	do  true
	),
	cumulative(Tasks, [limit(L),global(true)]).

