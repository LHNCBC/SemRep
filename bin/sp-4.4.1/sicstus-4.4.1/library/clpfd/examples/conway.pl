:- module(conway, [conway/0]).

:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(clpfd)).

% The Conway Packing Problem
% Place 6 4x2x1 boxes, 6 3x2x2 boxes and 5 1x1x1 cubes within a 5x5x5 cube
conway :-
	findall(1, conway1, Ones),
	length(Ones, N),
	format('~d solutions\n', [N]).

conway1 :-
	Objects = [object( 1,S1 ,[X1 ,Y1 ,Z1]),
		   object( 2,S2 ,[X2 ,Y2 ,Z2]),
		   object( 3,S3 ,[X3 ,Y3 ,Z3]),
		   object( 4,S4 ,[X4 ,Y4 ,Z4]),
		   object( 5,S5 ,[X5 ,Y5 ,Z5]),
		   object( 6,S6 ,[X6 ,Y6 ,Z6]),
		   object( 7,S7 ,[X7 ,Y7 ,Z7]),
		   object( 8,S8 ,[X8 ,Y8 ,Z8]),
		   object( 9,S9 ,[X9 ,Y9 ,Z9]),
		   object(10,S10,[X10,Y10,Z10]),
		   object(11,S11,[X11,Y11,Z11]),
		   object(12,S12,[X12,Y12,Z12]),
		   object(13,10, [X13,Y13,Z13]),
		   object(14,10, [X14,Y14,Z14]),
		   object(15,10, [X15,Y15,Z15]),
		   object(16,10, [X16,Y16,Z16]),
		   object(17,10, [X17,Y17,Z17])],
	Sboxes = [sbox( 1,[0,0,0],[4,2,1]),
		  sbox( 2,[0,0,0],[4,1,2]),
		  sbox( 3,[0,0,0],[2,4,1]),
		  sbox( 4,[0,0,0],[2,1,4]),
		  sbox( 5,[0,0,0],[1,4,2]),
		  sbox( 6,[0,0,0],[1,2,4]),
		  sbox( 7,[0,0,0],[3,2,2]),
		  sbox( 8,[0,0,0],[2,3,2]),
		  sbox( 9,[0,0,0],[2,2,3]),
		  sbox(10,[0,0,0],[1,1,1])],
	Options = [lex([1,2,3,4,5,6]),
		   lex([7,8,9,10,11,12]),
				% lex([13,14,15,16,17]),
		   parconflict(true),
		   bounding_box([0,0,0],[5,5,5])],
	Objects = [O1,O2,O3,O4,O5,O6,O7,O8,O9,O10,O11,O12,O13,O14,O15,O16,O17],
	Groups  = [[O1,O2,O3,O4,O5,O6],[O7,O8,O9,O10,O11,O12],[O13,O14,O15,O16,O17]],
	domain([S1,S2,S3,S4,S5,S6], 1, 6),
	domain([S7,S8,S9,S10,S11,S12], 7, 9),
	domain([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17], 0, 4),
	domain([Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12,Y13,Y14,Y15,Y16,Y17], 0, 4),
	domain([Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9,Z10,Z11,Z12,Z13,Z14,Z15,Z16,Z17], 0, 4),
	geost(Objects, Sboxes, Options),
	[X13,X14,X15,X16,X17] = [0,1,2,3,4],
	all_distinct([Y13,Y14,Y15,Y16,Y17]),
	all_distinct([Z13,Z14,Z15,Z16,Z17]),
	make_points(125, 5, 5, Points, []),
	(   fromto(Points,Ps0,Ps,[]),
	    fromto(Groups,Todo,Rest,_),
	    param(Sboxes)
	do  Ps0 = [X|Ps1],
	    Piece = object(_,SID,X),
	    select_first(Piece, Todo, Rest),
	    indomain(SID),
	    covered_by(Piece, Sboxes, Del, []),
	    selectchk(X, Del, Del1),
	    sort(Del1, Del2),
	    ord_subtract(Ps1, Del2, Ps)
	).

make_points(NCells, D, H) -->
	(   for(P,0,NCells-1),
	    param(D,H)
	do  [[Px,Py,Pz]],
	    {Px is P//(D*H)},
	    {Py is (P//H) mod D},
	    {Pz is P mod H}
	).

select_first(X, [[X]|R],     R).
select_first(X, [[X|Xs]|R],  [Xs|R]) :- Xs\==[].
select_first(X, [A|L],       [A|R]) :-
	select_first(X, L, R).

covered_by(Piece, Shapes) -->
	{Piece = object(_,SID,[X,Y,Z])},
	{select(sbox(SID,[Ox,Oy,Oz],[Sx,Sy,Sz]), Shapes, Shapes1)}, !,
	{Xmin is X+Ox},
	{Xmax is Xmin+Sx-1},
	{Ymin is Y+Oy},
	{Ymax is Ymin+Sy-1},
	{Zmin is Z+Oz},
	{Zmax is Zmin+Sz-1},
	findall(Point, point_in(Xmin,Xmax,Ymin,Ymax,Zmin,Zmax,Point)),
	covered_by(Piece, Shapes1).
covered_by(_, _) --> [].

point_in(Ox, Oxe, Oy, Oye, Oz, Oze, [X,Y,Z]) :-
	between(Ox, Oxe, X),
	between(Oy, Oye, Y),
	between(Oz, Oze, Z).

