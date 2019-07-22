/*
A solver for Shikaku problems.

Author: Mats Carlsson.
*/
:- module(shikaku, [shikaku/1]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

shikaku(ID) :-
	fd_statistics(backtracks, _),
	statistics(runtime, _),
	data(ID, Data),
	length(Data, Height),
	Data = [Row|_],
	length(Row, Width),
	shape_groups(Data, Width, Height, Groups, []),
	append(Groups, Shapes),
	(   foreach(sbox(SID,_,_),Shapes),
	    count(SID,0,_)
	do  true
	),
	(   foreach(Group,Groups),
	    foreach(object(OID,Var,[0,0]),Objects),
	    foreach(Var,Vars),
	    count(OID,0,_)
	do  (   foreach(sbox(SID1,_,_),Group),
		foreach(SID1,SIDs)
	    do  true
	    ),
	    list_to_fdset(SIDs, Set),
	    Var in_set Set
	),
	geost(Objects, Shapes, [polymorphism(true),
				longest_hole(true,1000),
				parconflict(true),
				visavis_init(true)]),
	labeling([ff,enum], Vars),
	(   foreach(Shape,Shapes),
	    fromto(Vars,Vars1,Vars2,[]),
	    fromto(Shapes1,Sh1,Sh2,[])
	do  (   Vars1 = [Var1|Vars2],
		Shape = sbox(Var1,_,_) -> Sh1 = [Shape|Sh2]
	    ;   Vars1 = Vars2,
		Sh1 = Sh2
	    )
	),
	(   foreach(Row1,Data),
	    foreach(Sep1,RowSeps),
	    foreach(Sep2,HlineSeps),
	    count(Y,0,_),
	    param(Shapes1)
	do  (   foreach(_,Row1),
		foreach(S1,Sep1),
		foreach(S2,Sep2),
		count(X,0,_),
		param(Shapes1,Y)
	    do  horizontal_sep(X, Y, S1, Shapes1),
		vertical_sep(X, Y, S2, Shapes1)
	    )
	),
	length(Zeros, Width),
	domain(Zeros, 0, 0),
	length(Ones, Width),
	domain(Ones, 1, 1),
	append(RowSeps, [Zeros], RowSeps1),
	print_solution_hline(Data, [Zeros|RowSeps1], [Ones|HlineSeps]).

shape_groups(Data, Width, Height) -->
	(   foreach(Row0,Data),
	    count(Y0,0,_),
	    param(Width,Height)
	do  (   foreach(C,Row0),
		count(X0,0,_),
		param(Y0,Width,Height)
	    do  (   {atom(C)} -> []
		;   [Group0],
		    {shape_group(C, X0, Y0, Width, Height, Group0, [])}
		)
	    )
	).

shape_group(C, X0, Y0, Width, Height) -->
	(   for(H,1,C),
	    param(C,X0,Y0,Width,Height)
	do  (   {C mod H =:= 0} ->
		{W is C//H},
		(   for(J,0,C-1),
		    param(X0,Y0,W,H,Width,Height)
		do  (   {X1 is X0 + (J mod W - W + 1)},
			{Y1 is Y0 + (J // W - H + 1)},
			{X1>=0},
			{Y1>=0},
			{X1+W=<Width},
			{Y1+H=<Height} ->
			[sbox(_,[X1,Y1],[W,H])]
		    ;   []
		    )
		)
	    ;   []
	    )
	).		    



/*
shape_groups(Data, Width, Height, Groups, []) :-
	(   foreach(Row0,Data),
	    count(Y0,0,_),
	    fromto(Groups,G1,G4,[]),
	    param(Width,Height)
	do  (   foreach(C,Row0),
		count(X0,0,_),
		fromto(G1,G2,G3,G4),
		param(Y0,Width,Height)
	    do  (   atom(C) -> G2 = G3
		;   G2 = [Group0|G3],
		    (   for(I,0,C-1),
			fromto(Group0,Gr1,Gr4,[]),
			param(C,X0,Y0,Width,Height)
		    do  (   H is I+1,
			    C mod H =:= 0 ->
			    W is C//H,
			    (   for(J,0,C-1),
				fromto(Gr1,Gr2,Gr3,Gr4),
				param(X0,Y0,W,H,Width,Height)
			    do  (   X1 is X0 + (J mod W - W + 1),
				    Y1 is Y0 + (J // W - H + 1),
				    X1>=0,
				    Y1>=0,
				    X1+W=<Width,
				    Y1+H=<Height ->
				    Gr2 = [sbox(_,[X1,Y1],[W,H])|Gr3]
				;   Gr2 = Gr3
				)
			    )
			;   Gr1 = Gr4
			)
		    )		    
		)
	    )
	).
*/

print_solution_hline(Data, [HSep1,HSep2|HSeps], [VSep|VSeps]) :-
	hline_string([1|HSep1], [0|VSep], [1|HSep2], String, []),
	format('~s\n', [String]),
	print_solution_row(Data, [HSep2|HSeps], VSeps).

print_solution_row([], _, _).
print_solution_row([Row|Rows], [HSep|HSeps], VSeps) :-
	row_string(Row, [1|HSep], String, []),
	format('~s\n', [String]),
	print_solution_hline(Rows, [HSep|HSeps], VSeps).

hline_string([H1], [V], [H2]) --> !,
	(   {H1+H2>0} ->
	    ({V>0} -> "+" ; "|")
	;   ({V>0} -> "-" ; " ")
	).
hline_string([H1|Hs1], [V1,V2|Vs], [H2|Hs2]) -->
	(   {H1+H2>0} ->
	    ({V1+V2>0} -> "+" ; "|")
	;   ({V1+V2>0} -> "-" ; " ")
	),
	({V2>0} -> "-" ; " "),
	hline_string(Hs1, [V2|Vs], Hs2).

row_string([], [1]) --> "|".
row_string([_|Row], [S|Seps]) -->
	({S=:=1} -> "| " ; "  "),
	row_string(Row, Seps).

horizontal_sep(X, Y, 0, Shapes) :-
	member(sbox(_,[Xo,Yo],[W,H]), Shapes),
	Xo=<X, Xo+W-1>X,
	Yo=<Y, Yo+H  >Y, !.
horizontal_sep(_, _, 1, _).

vertical_sep(X, Y, 0, Shapes) :-
	member(sbox(_,[Xo,Yo],[W,H]), Shapes),
	Xo=<X, Xo+W  >X,
	Yo=<Y, Yo+H-1>Y, !.
vertical_sep(_, _, 1, _).

:- dynamic data/2.

data(tiny,
     [[9,-,-,-,12,-,-,5,-,-],
      [-,-,-,-,-,-,-,-,-,-],
      [-,-,-,-,-,-,-,-,-,6],
      [8,-,6,-,8,-,-,-,-,-],
      [-,-,-,-,-,-,-,-,-,-],
      [-,-,-,-,-,-,-,-,-,-],
      [-,-,-,-,-,6,-,8,-,12],
      [4,-,-,-,-,-,-,-,-,-],
      [-,-,-,-,-,-,-,-,-,-],
      [-,-,3,-,-,9,-,-,-,4]]).

data(nikoliweb0,
     [[x ,8 ,4 ,x ,x ,x ,x ,4 ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ],
      [x ,x ,x ,3 ,3 ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ],
      [5 ,4 ,x ,x ,x ,x ,2 ,x ,x ,x ],
      [x ,x ,x ,9 ,x ,x ,x ,x ,6 ,7 ],
      [x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,4 ,5 ,x ,x ,x ],
      [x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,5 ,x ,x ,x ,x ,5 ,6 ,x ]]).

data(nikoliweb1,
     [[9 ,x ,x ,x ,12,x ,x ,5 ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ],
      [8 ,x ,6 ,x ,8 ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,6 ,x ,8 ,x ,12],
      [4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,3 ,x ,x ,9 ,x ,x ,x ,4 ]]).

data(nikoliweb2,
     [[7 ,x ,x ,x ,x ,x ,x ,x ,x ,8 ],
      [x ,x ,x ,x ,9 ,5 ,x ,x ,x ,x ],
      [x ,5 ,x ,x ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,x ,6 ,x ,x ,8 ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,10,x ,x ,9 ,x ,x ,x ],
      [x ,3 ,x ,x ,x ,x ,x ,x ,6 ,x ],
      [x ,x ,x ,x ,4 ,2 ,x ,x ,x ,x ],
      [6 ,x ,x ,x ,x ,x ,x ,x ,x ,8 ]]).

data(nikoliweb3,
     [[x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ],
      [x ,x ,2 ,x ,2 ,x ,x ,3 ,x ,x ],
      [x ,4 ,x ,3 ,x ,x ,3 ,x ,4 ,x ],
      [x ,x ,4 ,x ,x ,x ,x ,4 ,x ,10],
      [x ,x ,x ,x ,2 ,5 ,x ,x ,3 ,x ],
      [x ,4 ,x ,x ,2 ,4 ,x ,x ,x ,x ],
      [4 ,x ,3 ,x ,x ,x ,x ,2 ,x ,x ],
      [x ,6 ,x ,3 ,x ,x ,3 ,x ,2 ,x ],
      [x ,x ,4 ,x ,x ,3 ,x ,4 ,x ,x ],
      [x ,x ,x ,x ,x ,x ,3 ,x ,x ,x ]]).

data(nikoliweb4,
     [[x ,x ,x ,x ,x ,x ,x ,x ,8 ,9 ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,3 ,2 ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,4 ,x ,x ,x ],
      [x ,x ,x ,3 ,8 ,x ,2 ,4 ,x ,x ,4 ,2 ,x ,6 ,3 ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,3 ,9 ,x ,x ,2 ,2 ,x ,x ,x ,x ,x ,x ],
      [5 ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,3 ],
      [3 ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,5 ],
      [x ,x ,x ,x ,x ,x ,2 ,3 ,x ,x ,2 ,2 ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,5 ,2 ,x ,2 ,2 ,x ,x ,4 ,2 ,x ,6 ,10,x ,x ,x ],
      [x ,x ,x ,4 ,5 ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,2 ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,9 ,8 ,x ,x ,x ,x ,x ,x ,x ,x ]]).

data(nikoliweb5,
     [[x ,5 ,x ,x ,x ,x ,x ,x ,x ,10,4 ,x ,x ,x ,x ,x ,x ,4 ],
      [x ,3 ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,2 ,x ,x ,x ,x ],
      [x ,x ,x ,4 ,x ,9 ,x ,x ,x ,x ,x ,x ,9 ,6 ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,4 ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,6 ,6 ,x ],
      [x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,2 ,4 ,x ],
      [x ,3 ,8 ,x ,x ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,4 ,4 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,4 ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,4 ,2 ,x ,x ,x ,x ,x ,x ,6 ,x ,6 ,x ,x ,x ],
      [x ,x ,x ,x ,4 ,2 ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,6 ,x ],
      [8 ,x ,x ,x ,x ,x ,x ,6 ,8 ,x ,x ,x ,x ,x ,x ,x ,2 ,x ]]).

data(nikoliweb6,
     [[4 ,x ,x ,x ,x ,x ,9 ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,x ,x ,x ,x ,x ,6 ,x ,9 ,x ,x ,x ,x ,6 ,x ,x ,4 ],
      [x ,6 ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,2 ,x ,x ],
      [x ,x ,4 ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ],
      [x ,3 ,x ,x ,x ,x ,x ,8 ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,4 ,x ,x ,x ,x ,x ,3 ,x ],
      [x ,x ,x ,x ,9 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ],
      [x ,x ,3 ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,6 ,x ],
      [6 ,x ,x ,4 ,x ,x ,x ,x ,4 ,x ,4 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,2 ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,2 ,x ,x ,x ,x ,x ,6 ]]).

data(nikoliweb7,
     [[x ,x ,6 ,6 ,x ,x ,x ,x ,x ,x ,x ,2 ,8 ,x ,x ,x ,x ,x ,x ,x ,2 ,4 ,x ,x ],
      [x ,x ,x ,x ,x ,3 ,3 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,2 ,x ,x ,x ,x ,x ],
      [4 ,4 ,x ,x ,x ,x ,x ,x ,2 ,6 ,x ,x ,x ,x ,4 ,4 ,x ,x ,x ,x ,x ,x ,6 ,3 ],
      [x ,x ,x ,x ,8 ,4 ,x ,x ,x ,x ,x ,3 ,3 ,x ,x ,x ,x ,x ,6 ,6 ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,3 ,12,x ,x ,x ,x ,x ,x ,4 ,2 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,5 ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,9 ,10,x ],
      [x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ],
      [x ,3 ,8 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,9 ,3 ,x ],
      [x ,x ,x ,x ,x ,x ,x ,5 ,12,x ,x ,x ,x ,x ,x ,6 ,4 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,6 ,3 ,x ,x ,x ,x ,x ,4 ,4 ,x ,x ,x ,x ,x ,3 ,6 ,x ,x ,x ,x ],
      [6 ,4 ,x ,x ,x ,x ,x ,x ,2 ,4 ,x ,x ,x ,x ,2 ,3 ,x ,x ,x ,x ,x ,x ,4 ,6 ],
      [x ,x ,x ,x ,x ,6 ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,3 ,x ,x ,x ,x ,x ],
      [x ,x ,3 ,5 ,x ,x ,x ,x ,x ,x ,x ,8 ,5 ,x ,x ,x ,x ,x ,x ,x ,4 ,4 ,x ,x ]]).

data(nikoliweb8,
     [[x ,5 ,x ,x ,x ,3 ,8 ,x ,x ,x ,x ,x ,x ,x ,2 ,2 ,x ,x ,x ,x ,x ,x ,9 ,6 ],
      [x ,x ,2 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ],
      [x ,x ,x ,4 ,x ,x ,x ,x ,4 ,x ,x ,x ,6 ,x ,x ,x ,3 ,x ,x ,3 ,x ,x ,x ,x ],
      [6 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,3 ,x ,x ,4 ,x ,x ,x ,x ,4 ,x ,x ,x ],
      [5 ,x ,x ,x ,5 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ],
      [x ,x ,x ,6 ,x ,x ,3 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ],
      [x ,x ,3 ,x ,x ,x ,x ,6 ,x ,x ,x ,3 ,x ,x ,x ,4 ,x ,x ,8 ,x ,x ,x ,2 ,x ],
      [x ,4 ,x ,x ,x ,5 ,x ,x ,3 ,x ,x ,x ,9 ,x ,x ,x ,5 ,x ,x ,x ,x ,6 ,x ,x ],
      [x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,3 ,x ,x ,x ],
      [x ,x ,9 ,x ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,2 ,x ,x ,x ,4 ],
      [x ,x ,x ,4 ,x ,x ,x ,x ,6 ,x ,x ,6 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,3 ],
      [x ,x ,x ,x ,8 ,x ,x ,9 ,x ,x ,x ,4 ,x ,x ,x ,2 ,x ,x ,x ,x ,5 ,x ,x ,x ],
      [x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,5 ,x ,x ,x ,x ,x ,x ,3 ,x ,x ],
      [5 ,4 ,x ,x ,x ,x ,x ,x ,4 ,6 ,x ,x ,x ,x ,x ,x ,x ,3 ,8 ,x ,x ,x ,6 ,x ]]).

data(nikoliweb9,
     [[6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ],
      [x ,x ,6 ,x ,x ,5 ,x ,x ,9 ,x ,x ,9 ,x ,x ,6 ,x ,x ,x ,x ,x ,2 ,x ,2 ,x ,x ,8 ,x ,x ,6 ,x ,x ,4 ,x ,4 ,x ,x ],
      [x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,4 ,x ],
      [x ,x ,x ,5 ,x ,x ,9 ,x ,x ,x ,6 ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,4 ,x ],
      [x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,8 ,x ,x ,10,x ,x ,x ,3 ,x ,x ,2 ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,4 ,x ],
      [x ,2 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,4 ,x ,6 ,x ,x ,12,x ,x ,6 ,x ,x ,6 ,x ,2 ,x ,x ],
      [x ,x ,4 ,x ,x ,8 ,x ,x ,6 ,x ,x ,9 ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ],
      [6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,9 ,17,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,17,9 ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ],
      [6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,2 ,x ,x ,10,x ,x ,8 ,x ,x ,4 ,x ,x ],
      [x ,x ,6 ,x ,4 ,x ,x ,4 ,x ,x ,4 ,x ,x ,8 ,x ,3 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ],
      [x ,6 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,8 ,x ,x ,8 ,x ,x ,x ,2 ,x ,x ,6 ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ],
      [x ,8 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,3 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,3 ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,3 ,x ,x ,10,x ,x ,x ,12,x ,x ,2 ,x ,x ,x ],
      [x ,4 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,4 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ],
      [x ,x ,6 ,x ,6 ,x ,x ,6 ,x ,x ,2 ,x ,x ,2 ,x ,6 ,x ,x ,x ,x ,x ,4 ,x ,x ,8 ,x ,x ,6 ,x ,x ,6 ,x ,x ,6 ,x ,x ],
      [4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,9 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ]]).

data(nikoliweb10,
     [[x ,x ,4 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,12,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,6 ],
      [x ,4 ,x ,x ,x ,x ,5 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,3 ,5 ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,8 ,x ],
      [4 ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,10,x ,x ,x ,x ,x ,x ,6 ,8 ,x ,x ,x ,x ,x ,x ,9 ,3 ,4 ,x ,x ,x ,x ,x ,8 ,x ,x ],
      [x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,10,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,12,6 ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ],
      [x ,8 ,x ,x ,4 ,2 ,8 ,x ,4 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ],
      [5 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ,8 ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,8 ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ],
      [x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,6 ,6 ,6 ,x ,x ,x ],
      [x ,x ,9 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,9 ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,4 ,x ,x ],
      [x ,x ,x ,6 ,8 ,8 ,x ,x ,x ,x ,9 ,x ,x ,x ,x ,x ,9 ,x ,x ,x ,9 ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ],
      [x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,12,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [6 ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ],
      [x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,4 ,x ,4 ,6 ,6 ,x ,x ,10,x ],
      [x ,x ,x ,x ,x ,7 ,x ,x ,x ,x ,x ,x ,x ,12,6 ,x ,x ,x ,x ,x ,9 ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ],
      [x ,x ,6 ,x ,x ,x ,x ,x ,4 ,6 ,4 ,x ,x ,x ,x ,x ,x ,4 ,4 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,3 ],
      [x ,4 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,6 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,4 ,x ],
      [4 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,7 ,x ,x ,x ,x ,3 ,x ,x ]]).
           
data(giants0,
     [[4 ,4 ,x ,x ,x ,x ,5 ],
      [x ,x ,x ,4 ,x ,x ,x ],
      [x ,x ,3 ,x ,x ,8 ,x ],
      [x ,x ,x ,x ,x ,x ,x ],
      [x ,3 ,x ,x ,3 ,x ,x ],
      [x ,x ,x ,2 ,x ,x ,x ],
      [6 ,x ,x ,x ,x ,4 ,3 ]]).

data(giants1,
     [[9 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,5 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ],
      [x ,x ,x ,x ,2 ,x ,8 ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,6 ,x ,x ,x ,x ],
      [x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,x ],
      [x ,x ,3 ,x ,x ,2 ,x ,x ,3 ,x ,x ,x ,8 ,x ,x ,4 ,x ,x ,6 ,x ,x ],
      [x ,4 ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,10,x ],
      [x ,x ,x ,6 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,10,x ,x ,x ],
      [x ,x ,4 ,x ,x ,x ,x ,x ,5 ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,4 ,x ,x ],
      [8 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,4 ],
      [x ,x ,x ,6 ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,3 ,x ,x ,x ],
      [x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,x ],
      [8 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,8 ],
      [x ,x ,6 ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,4 ,x ,x ],
      [x ,x ,x ,5 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,5 ,x ,x ,x ],
      [x ,6 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,2 ,x ],
      [x ,x ,4 ,x ,x ,3 ,x ,x ,2 ,x ,x ,x ,6 ,x ,x ,7 ,x ,x ,8 ,x ,x ],
      [x ,x ,x ,8 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,x ],
      [x ,x ,x ,x ,3 ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,3 ,x ,6 ,x ,x ,x ,x ],
      [5 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,10,x ,x ,x ,x ,x ,x ,x ,x ,x ,5 ],
      [x ,x ,x ,x ,6 ,x ,7 ,x ,x ,x ,x ,x ,x ,x ,3 ,x ,10,x ,x ,x ,x ],
      [x ,x ,x ,3 ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,3 ,x ,x ,x ],
      [x ,x ,4 ,x ,x ,9 ,x ,x ,8 ,x ,x ,x ,5 ,x ,x ,4 ,x ,x ,6 ,x ,x ],
      [x ,6 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,6 ,x ],
      [x ,x ,x ,3 ,x ,x ,x ,7 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,3 ,x ,x ,x ],
      [x ,x ,8 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,4 ,x ,x ],
      [3 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,8 ],
      [x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,2 ,x ,x ,x ],
      [x ,x ,x ,4 ,x ,x ,x ,5 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,x ],
      [6 ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,7 ],
      [x ,x ,6 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,3 ,x ,x ],
      [x ,x ,x ,4 ,x ,x ,x ,5 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,x ],
      [x ,4 ,x ,x ,x ,x ,x ,x ,x ,9 ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,6 ,x ],
      [x ,x ,3 ,x ,x ,6 ,x ,x ,8 ,x ,x ,x ,2 ,x ,x ,2 ,x ,x ,4 ,x ,x ],
      [x ,x ,x ,4 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,5 ,x ,x ,x ,2 ,x ,x ,x ],
      [x ,x ,x ,x ,4 ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,4 ,x ,x ,x ,x ],
      [6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ]]).

data(giants2,
     [[x ,x ,6 ,x ,x ,x ,x ,4 ,x ,x ,3 ,x ,x ,5 ,x ,x ,x ,x ,3 ,x ,x ],
      [x ,4 ,x ,x ,2 ,3 ,x ,x ,4 ,x ,x ,x ,3 ,x ,x ,4 ,3 ,x ,x ,2 ,x ],
      [2 ,x ,2 ,x ,x ,x ,x ,4 ,x ,2 ,x ,2 ,x ,3 ,x ,x ,x ,x ,5 ,x ,6 ],
      [x ,x ,x ,4 ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,5 ,x ,x ,6 ,x ,x ,x ],
      [x ,2 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,x ,3 ,x ],
      [x ,3 ,x ,x ,6 ,5 ,x ,x ,3 ,x ,x ,x ,4 ,x ,x ,3 ,4 ,x ,x ,2 ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [6 ,x ,x ,8 ,x ,x ,3 ,x ,x ,4 ,x ,8 ,x ,x ,3 ,x ,x ,2 ,x ,x ,3 ],
      [x ,x ,2 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,3 ,x ,x ],
      [3 ,x ,x ,2 ,x ,x ,3 ,x ,x ,3 ,x ,2 ,x ,x ,6 ,x ,x ,4 ,x ,x ,5 ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,6 ,x ,x ,4 ,2 ,x ,x ,10,x ,x ,x ,3 ,x ,x ,2 ,4 ,x ,x ,6 ,x ],
      [x ,2 ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,x ,6 ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,4 ,x ,x ,x ],
      [5 ,x ,2 ,x ,x ,x ,x ,2 ,x ,5 ,x ,3 ,x ,6 ,x ,x ,x ,x ,2 ,x ,3 ],
      [x ,3 ,x ,x ,3 ,6 ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,5 ,3 ,x ,x ,3 ,x ],
      [x ,x ,2 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,2 ,x ,x ],
      [4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,10,x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ],
      [x ,x ,2 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,10,x ,x ,x ,x ,3 ,x ,x ],
      [x ,4 ,x ,x ,3 ,2 ,x ,x ,3 ,x ,x ,x ,8 ,x ,x ,4 ,2 ,x ,x ,4 ,x ],
      [2 ,x ,3 ,x ,x ,x ,x ,2 ,x ,5 ,x ,4 ,x ,4 ,x ,x ,x ,x ,3 ,x ,6 ],
      [x ,x ,x ,2 ,x ,x ,9 ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,4 ,x ,x ,x ],
      [x ,2 ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,5 ,x ],
      [x ,3 ,x ,x ,2 ,6 ,x ,x ,4 ,x ,x ,x ,3 ,x ,x ,3 ,2 ,x ,x ,4 ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [6 ,x ,x ,8 ,x ,x ,4 ,x ,x ,3 ,x ,2 ,x ,x ,4 ,x ,x ,6 ,x ,x ,3 ],
      [x ,x ,2 ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,6 ,x ,x ],
      [3 ,x ,x ,4 ,x ,x ,2 ,x ,x ,8 ,x ,6 ,x ,x ,8 ,x ,x ,4 ,x ,x ,4 ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,4 ,x ,x ,5 ,2 ,x ,x ,6 ,x ,x ,x ,2 ,x ,x ,9 ,2 ,x ,x ,3 ,x ],
      [x ,6 ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,3 ,x ],
      [x ,x ,x ,3 ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,5 ,x ,x ,3 ,x ,x ,x ],
      [5 ,x ,2 ,x ,x ,x ,x ,3 ,x ,4 ,x ,3 ,x ,4 ,x ,x ,x ,x ,2 ,x ,4 ],
      [x ,3 ,x ,x ,4 ,2 ,x ,x ,2 ,x ,x ,x ,4 ,x ,x ,2 ,4 ,x ,x ,3 ,x ],
      [x ,x ,4 ,x ,x ,x ,x ,4 ,x ,x ,3 ,x ,x ,4 ,x ,x ,x ,x ,5 ,x ,x ]]).

data(giants3,
     [[x ,x ,4 ,x ,3 ,x ,x ,4 ,x ,x ,2 ,x ,x ,4 ,x ,4 ,x ,x ,x ,3 ,x ],
      [x ,6 ,x ,x ,x ,2 ,x ,x ,x ,2 ,x ,3 ,x ,x ,6 ,x ,x ,3 ,x ,x ,4 ],
      [x ,x ,2 ,x ,4 ,x ,x ,x ,5 ,x ,x ,x ,2 ,x ,x ,x ,2 ,x ,2 ,x ,x ],
      [3 ,x ,x ,4 ,x ,x ,6 ,x ,x ,2 ,x ,4 ,x ,x ,x ,4 ,x ,x ,x ,3 ,x ],
      [x ,3 ,x ,x ,x ,2 ,x ,2 ,x ,x ,2 ,x ,x ,4 ,x ,x ,2 ,x ,4 ,x ,x ],
      [4 ,x ,x ,x ,4 ,x ,x ,x ,3 ,x ,x ,x ,3 ,x ,3 ,x ,x ,3 ,x ,x ,4 ],
      [x ,x ,3 ,x ,x ,2 ,x ,4 ,x ,x ,x ,2 ,x ,x ,x ,2 ,x ,x ,x ,2 ,x ],
      [x ,2 ,x ,2 ,x ,x ,4 ,x ,x ,3 ,x ,x ,2 ,x ,2 ,x ,x ,x ,3 ,x ,x ],
      [2 ,x ,x ,x ,2 ,x ,x ,x ,2 ,x ,4 ,x ,x ,2 ,x ,x ,3 ,x ,x ,4 ,x ],
      [x ,3 ,x ,2 ,x ,x ,x ,2 ,x ,x ,x ,2 ,x ,x ,x ,2 ,x ,2 ,x ,x ,4 ],
      [x ,x ,9 ,x ,x ,4 ,x ,x ,2 ,x ,3 ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ],
      [2 ,x ,x ,x ,2 ,x ,3 ,x ,x ,6 ,x ,x ,x ,x ,x ,2 ,x ,2 ,x ,x ,x ],
      [x ,x ,x ,8 ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ,x ,3 ,x ,x ,4 ,x ],
      [x ,x ,x ,x ,4 ,x ,3 ,x ,x ,x ,x ,x ,x ,5 ,x ,x ,x ,x ,6 ,x ,3 ],
      [4 ,x ,x ,x ,x ,5 ,x ,x ,x ,2 ,x ,4 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ],
      [x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,6 ,x ,2 ],
      [x ,x ,6 ,x ,x ,x ,x ,x ,12,x ,x ,x ,8 ,x ,x ,2 ,x ,x ,x ,6 ,x ],
      [x ,x ,x ,x ,5 ,x ,x ,x ,x ,x ,9 ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,x ],
      [x ,4 ,x ,x ,x ,2 ,x ,x ,3 ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,2 ,x ,x ],
      [4 ,x ,2 ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,x ,3 ,x ,x ,x ,x ,x ,6 ,x ,6 ,x ,x ,x ,12,x ,x ,x ,x ,3 ],
      [2 ,x ,2 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,4 ,x ,4 ,x ,x ,x ,x ],
      [x ,6 ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,4 ,x ,x ,x ],
      [x ,x ,x ,3 ,x ,2 ,x ,x ,x ,x ,x ,12,x ,x ,2 ,x ,4 ,x ,x ,x ,2 ],
      [x ,x ,4 ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,3 ,x ,x ,3 ,x ,x ,4 ,x ,x ],
      [4 ,x ,x ,2 ,x ,2 ,x ,x ,x ,3 ,x ,x ,x ,2 ,x ,x ,x ,3 ,x ,4 ,x ],
      [x ,2 ,x ,x ,3 ,x ,x ,4 ,x ,x ,2 ,x ,4 ,x ,x ,x ,7 ,x ,x ,x ,4 ],
      [x ,x ,3 ,x ,x ,x ,4 ,x ,2 ,x ,x ,3 ,x ,x ,4 ,x ,x ,2 ,x ,3 ,x ],
      [x ,4 ,x ,x ,x ,2 ,x ,x ,x ,4 ,x ,x ,x ,2 ,x ,2 ,x ,x ,4 ,x ,x ],
      [4 ,x ,x ,6 ,x ,x ,3 ,x ,4 ,x ,x ,x ,2 ,x ,x ,x ,3 ,x ,x ,x ,2 ],
      [x ,x ,2 ,x ,2 ,x ,x ,3 ,x ,x ,3 ,x ,x ,4 ,x ,2 ,x ,x ,x ,4 ,x ],
      [x ,3 ,x ,x ,x ,4 ,x ,x ,x ,4 ,x ,3 ,x ,x ,3 ,x ,x ,3 ,x ,x ,4 ],
      [x ,x ,4 ,x ,2 ,x ,x ,x ,2 ,x ,x ,x ,6 ,x ,x ,x ,2 ,x ,2 ,x ,x ],
      [4 ,x ,x ,5 ,x ,x ,4 ,x ,x ,4 ,x ,2 ,x ,x ,x ,2 ,x ,x ,x ,4 ,x ],
      [x ,5 ,x ,x ,x ,2 ,x ,2 ,x ,x ,3 ,x ,x ,2 ,x ,x ,3 ,x ,3 ,x ,x ]]).

data(giants4,
     [[x ,x ,2 ,3 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,2 ,4 ,x ,x ,x ],
      [x ,4 ,x ,x ,6 ,x ,x ,x ,2 ,x ,x ,x ,4 ,3 ,x ,3 ,x ,x ,4 ,x ,x ],
      [x ,4 ,x ,x ,x ,2 ,x ,x ,4 ,x ,x ,2 ,x ,x ,2 ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,2 ,3 ,x ,x ,4 ,4 ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,2 ,x ,x ,6 ],
      [x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,2 ,4 ,x ,3 ,2 ,x ],
      [x ,x ,x ,x ,x ,2 ,x ,x ,3 ,4 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ],
      [x ,x ,4 ,x ,x ,2 ,x ,3 ,x ,x ,x ,x ,4 ,4 ,x ,x ,2 ,6 ,x ,x ,x ],
      [x ,6 ,x ,2 ,2 ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,8 ,x ,x ],
      [8 ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,x ,3 ,x ],
      [3 ,x ,x ,x ,x ,4 ,4 ,x ,x ,3 ,x ,x ,2 ,x ,x ,x ,4 ,x ,x ,x ,3 ],
      [x ,6 ,x ,x ,2 ,x ,x ,2 ,x ,4 ,x ,x ,x ,4 ,x ,x ,4 ,x ,x ,x ,3 ],
      [x ,x ,4 ,4 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,2 ,2 ,x ,x ,x ,8 ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [4 ,x ,x ,6 ,x ,x ,6 ,6 ,x ,x ,8 ,x ,x ,4 ,6 ,x ,x ,2 ,x ,x ,8 ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ],
      [x ,x ,2 ,x ,4 ,x ,x ,x ,x ,4 ,x ,6 ,x ,x ,x ,x ,2 ,x ,2 ,x ,x ],
      [x ,4 ,x ,x ,x ,6 ,x ,x ,2 ,x ,x ,x ,4 ,x ,x ,12,x ,x ,x ,6 ,x ],
      [x ,x ,4 ,x ,2 ,x ,x ,x ,x ,4 ,x ,6 ,x ,x ,x ,x ,2 ,x ,3 ,x ,x ],
      [x ,x ,x ,10,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [3 ,x ,x ,10,x ,x ,3 ,6 ,x ,x ,12,x ,x ,10,8 ,x ,x ,6 ,x ,x ,4 ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,8 ,x ,x ,x ,4 ,3 ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,4 ,2 ,x ,x ],
      [2 ,x ,x ,x ,4 ,x ,x ,2 ,x ,x ,x ,2 ,x ,2 ,x ,x ,6 ,x ,x ,2 ,x ],
      [2 ,x ,x ,x ,3 ,x ,x ,x ,2 ,x ,x ,2 ,x ,x ,4 ,4 ,x ,x ,x ,x ,2 ],
      [x ,6 ,x ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,x ,6 ],
      [x ,x ,2 ,x ,x ,3 ,x ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,2 ,2 ,x ,2 ,x ],
      [x ,x ,x ,4 ,2 ,x ,x ,8 ,2 ,x ,x ,x ,x ,6 ,x ,3 ,x ,x ,3 ,x ,x ],
      [x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,3 ,2 ,x ,x ,6 ,x ,x ,x ,x ,x ],
      [x ,2 ,4 ,x ,4 ,2 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,5 ,x ,x ,x ,x ],
      [4 ,x ,x ,2 ,x ,x ,x ,x ,x ,9 ,x ,x ,x ,4 ,4 ,x ,x ,4 ,3 ,x ,x ],
      [x ,2 ,x ,x ,x ,x ,2 ,x ,x ,4 ,x ,x ,4 ,x ,x ,4 ,x ,x ,x ,2 ,x ],
      [x ,x ,4 ,x ,x ,2 ,x ,2 ,2 ,x ,x ,x ,3 ,x ,x ,x ,2 ,x ,x ,4 ,x ],
      [x ,x ,x ,4 ,2 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,3 ,3 ,x ,x ]]).

data(giants5,
     [[x ,x ,3 ,x ,x ,x ,8 ,x ,x ,x ,8 ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ],
      [4 ,x ,x ,x ,3 ,x ,x ,x ,6 ,x ,x ,x ,8 ,x ,x ,x ,4 ,x ,x ,x ,4 ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [6 ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,x ,6 ],
      [x ,x ,6 ,x ,x ,x ,2 ,x ,x ,x ,8 ,x ,x ,x ,2 ,x ,x ,x ,8 ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,4 ,x ,x ,x ,x ,6 ,x ,x ,x ,3 ,x ,x ,x ,6 ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,x ,6 ,x ,x ,x ,2 ,x ,x ,x ,x ],
      [x ,x ,8 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,12,x ,x ],
      [2 ,x ,x ,x ,x ,8 ,x ,4 ,x ,x ,x ,x ,x ,4 ,x ,4 ,x ,x ,x ,x ,5 ],
      [x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,5 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ],
      [x ,x ,12,x ,x ,x ,x ,x ,x ,4 ,x ,8 ,x ,x ,x ,x ,x ,x ,2 ,x ,x ],
      [4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ],
      [x ,x ,x ,x ,x ,6 ,x ,x ,4 ,x ,x ,x ,4 ,x ,x ,9 ,x ,x ,x ,x ,x ],
      [6 ,x ,x ,x ,5 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,6 ],
      [x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ],
      [x ,x ,10,x ,x ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,6 ,x ,x ],
      [x ,8 ,x ,4 ,x ,x ,4 ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,4 ,x ,4 ,x ],
      [x ,x ,2 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,2 ,x ,x ],
      [x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ],
      [4 ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,6 ],
      [x ,x ,x ,x ,x ,2 ,x ,x ,4 ,x ,x ,x ,2 ,x ,x ,4 ,x ,x ,x ,x ,x ],
      [4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ],
      [x ,x ,12,x ,x ,x ,x ,x ,x ,4 ,x ,8 ,x ,x ,x ,x ,x ,x ,5 ,x ,x ],
      [x ,x ,x ,x ,x ,x ,12,x ,x ,x ,3 ,x ,x ,x ,9 ,x ,x ,x ,x ,x ,x ],
      [8 ,x ,x ,x ,x ,3 ,x ,2 ,x ,x ,x ,x ,x ,3 ,x ,6 ,x ,x ,x ,x ,4 ],
      [x ,x ,4 ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,8 ,x ,x ],
      [x ,x ,x ,x ,6 ,x ,x ,x ,8 ,x ,x ,x ,3 ,x ,x ,x ,4 ,x ,x ,x ,x ],
      [x ,6 ,x ,x ,x ,x ,7 ,x ,x ,x ,8 ,x ,x ,x ,2 ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,9 ,x ,x ,x ,10,x ,x ,x ,4 ,x ,x ,x ,8 ,x ,x ,x ,4 ,x ,x ],
      [2 ,x ,x ,x ,4 ,x ,x ,x ,2 ,x ,x ,x ,6 ,x ,x ,x ,8 ,x ,x ,x ,6 ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [2 ,x ,x ,x ,8 ,x ,x ,x ,4 ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,x ,6 ],
      [x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,9 ,x ,x ,x ,4 ,x ,x ]]).

data(giants6,
     [[x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ],
      [x ,6 ,x ,x ,x ,x ,4 ,x ,x ,x ,2 ,x ,x ,x ,8 ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,3 ,x ,x ,6 ,4 ,x ,x ,x ,x ,x ,x ,x ,3 ,3 ,x ,x ,3 ,x ,x ],
      [x ,x ,x ,2 ,x ,x ,x ,x ,x ,2 ,x ,3 ,x ,x ,x ,x ,x ,3 ,x ,x ,x ],
      [x ,6 ,x ,x ,12,x ,x ,x ,8 ,x ,x ,x ,8 ,x ,x ,x ,3 ,x ,x ,2 ,x ],
      [x ,x ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ],
      [2 ,x ,x ,x ,x ,6 ,x ,x ,x ,3 ,x ,6 ,x ,x ,x ,12,x ,x ,x ,x ,4 ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,2 ,x ,2 ,x ,9 ,x ,x ,x ,4 ,x ,x ,x ,9 ,x ,4 ,x ,8 ,x ,x ],
      [x ,6 ,x ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,x ,2 ,x ],
      [x ,x ,x ,6 ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,4 ,x ,x ,x ],
      [x ,4 ,x ,x ,x ,x ,x ,x ,x ,5 ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,x ,x ,x ,2 ,6 ,x ,x ,6 ,x ,4 ,x ,x ,4 ,8 ,x ,x ,x ,x ,x ],
      [x ,8 ,x ,x ,x ,6 ,x ,x ,3 ,x ,x ,x ,10,x ,x ,4 ,x ,x ,x ,6 ,x ],
      [x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,3 ,x ,x ,x ],
      [x ,x ,6 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,2 ,x ,x ],
      [2 ,x ,x ,x ,6 ,2 ,x ,x ,x ,12,x ,8 ,x ,x ,x ,2 ,4 ,x ,x ,x ,4 ],
      [x ,3 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ],
      [6 ,x ,x ,x ,3 ,2 ,x ,x ,x ,9 ,x ,9 ,x ,x ,x ,2 ,2 ,x ,x ,x ,6 ],
      [x ,x ,2 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,6 ,x ,x ],
      [x ,x ,x ,3 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ],
      [x ,4 ,x ,x ,x ,3 ,x ,x ,4 ,x ,x ,x ,3 ,x ,x ,2 ,x ,x ,x ,2 ,x ],
      [x ,x ,x ,x ,x ,8 ,3 ,x ,x ,2 ,x ,3 ,x ,x ,3 ,4 ,x ,x ,x ,x ,x ],
      [x ,2 ,x ,x ,x ,x ,x ,x ,x ,3 ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,x ,3 ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,6 ,x ,x ,x ],
      [x ,4 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,5 ,x ,x ,x ,x ,x ,x ,6 ,x ],
      [x ,x ,6 ,x ,4 ,x ,4 ,x ,x ,x ,3 ,x ,x ,x ,3 ,x ,8 ,x ,6 ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,2 ,x ,x ,x ,x ,x ,x ,x ,x ,x ],
      [6 ,x ,x ,x ,x ,6 ,x ,x ,x ,2 ,x ,2 ,x ,x ,x ,12,x ,x ,x ,x ,6 ],
      [x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,6 ,x ,x ,8 ,x ,x ,x ,3 ,x ,x ,x ,4 ,x ,x ,x ,6 ,x ,x ,8 ,x ],
      [x ,x ,x ,6 ,x ,x ,x ,x ,x ,6 ,x ,4 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ],
      [x ,x ,3 ,x ,x ,3 ,6 ,x ,x ,x ,x ,x ,x ,x ,2 ,4 ,x ,x ,2 ,x ,x ],
      [x ,4 ,x ,x ,x ,x ,2 ,x ,x ,x ,12,x ,x ,x ,8 ,x ,x ,x ,x ,3 ,x ],
      [x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ]]).

data(giants7,
     [[4 ,x ,x ,x ,x ,x ,4 ,6 ,x ,x ,x ,x ,x ,4 ,4 ,x ,x ,x ,x ,x ,4 ],
      [x ,x ,x ,x ,6 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ],
      [x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,8 ,x ,x ,x ,x ,x ],
      [x ,x ,x ,12,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,12,x ,x ,x ],
      [x ,x ,x ,x ,x ,12,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ],
      [x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ],
      [4 ,x ,x ,x ,x ,x ,2 ,6 ,x ,x ,x ,x ,x ,6 ,6 ,x ,x ,x ,x ,x ,6 ],
      [2 ,x ,x ,x ,x ,x ,2 ,6 ,x ,x ,x ,x ,x ,6 ,6 ,x ,x ,x ,x ,x ,4 ],
      [x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,4 ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ],
      [x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ],
      [x ,8 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,6 ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,4 ,x ,x ,x ,x ,12,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ],
      [4 ,x ,x ,x ,x ,x ,8 ,4 ,x ,x ,x ,x ,x ,8 ,4 ,x ,x ,x ,x ,x ,6 ],
      [6 ,x ,x ,x ,x ,x ,4 ,2 ,x ,x ,x ,x ,x ,6 ,4 ,x ,x ,x ,x ,x ,4 ],
      [x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ],
      [x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ,x ],
      [x ,x ,x ,4 ,x ,x ,x ,x ,8 ,x ,9 ,x ,6 ,x ,x ,x ,x ,6 ,x ,x ,x ],
      [x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ],
      [x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ],
      [6 ,x ,x ,x ,x ,x ,4 ,6 ,x ,x ,x ,x ,x ,6 ,2 ,x ,x ,x ,x ,x ,4 ],
      [4 ,x ,x ,x ,x ,x ,4 ,6 ,x ,x ,x ,x ,x ,4 ,4 ,x ,x ,x ,x ,x ,4 ],
      [x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ],
      [x ,x ,x ,x ,x ,4 ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ],
      [x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ],
      [x ,8 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,10,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,12,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ],
      [2 ,x ,x ,x ,x ,x ,4 ,4 ,x ,x ,x ,x ,x ,6 ,2 ,x ,x ,x ,x ,x ,6 ],
      [8 ,x ,x ,x ,x ,x ,4 ,6 ,x ,x ,x ,x ,x ,6 ,4 ,x ,x ,x ,x ,x ,4 ],
      [x ,x ,x ,x ,6 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ],
      [x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,8 ,x ,x ,x ,x ,x ],
      [x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,10,x ,x ,x ],
      [x ,x ,x ,x ,x ,4 ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ],
      [x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,8 ,x ,x ,x ,x ],
      [4 ,x ,x ,x ,x ,x ,2 ,6 ,x ,x ,x ,x ,x ,6 ,3 ,x ,x ,x ,x ,x ,3 ]]).

data(giants8,
     [[x ,x ,x ,x ,5 ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ],
      [x ,x ,4 ,x ,x ,x ,x ,x ,x ,4 ,x ,6 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ],
      [12,x ,x ,x ,x ,x ,x ,x ,10,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,6 ],
      [x ,x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,8 ,x ,x ,3 ,x ,x ,x ,3 ,x ,x ,x ,10,x ,x ,6 ,x ,x ,x ],
      [x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,2 ,x ,x ,x ,x ],
      [x ,6 ,x ,x ,x ,8 ,x ,x ,x ,8 ,x ,4 ,x ,x ,x ,6 ,x ,x ,x ,4 ,x ],
      [x ,x ,x ,x ,x ,x ,x ,4 ,4 ,x ,x ,x ,2 ,3 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ],
      [x ,x ,5 ,x ,x ,6 ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,3 ,x ,x ,9 ,x ,x ],
      [x ,8 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,6 ,x ],
      [x ,x ,x ,x ,6 ,x ,x ,x ,2 ,x ,x ,x ,6 ,x ,x ,x ,6 ,x ,x ,x ,x ],
      [x ,x ,2 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,5 ,x ,x ,x ,x ,2 ,x ,x ],
      [x ,x ,x ,3 ,x ,x ,x ,x ,x ,3 ,6 ,2 ,x ,x ,x ,x ,x ,3 ,x ,x ,x ],
      [4 ,x ,x ,x ,x ,8 ,x ,4 ,x ,x ,x ,x ,x ,2 ,x ,8 ,x ,x ,x ,x ,3 ],
      [2 ,x ,x ,x ,6 ,x ,x ,x ,7 ,x ,x ,x ,4 ,x ,x ,x ,3 ,x ,x ,x ,3 ],
      [x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ],
      [x ,x ,4 ,x ,x ,x ,5 ,x ,x ,x ,15,x ,x ,x ,3 ,x ,x ,x ,12,x ,x ],
      [x ,x ,x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ],
      [4 ,x ,x ,x ,6 ,x ,x ,x ,2 ,x ,x ,x ,4 ,x ,x ,x ,9 ,x ,x ,x ,4 ],
      [3 ,x ,x ,x ,x ,3 ,x ,5 ,x ,x ,x ,x ,x ,6 ,x ,2 ,x ,x ,x ,x ,6 ],
      [x ,x ,x ,4 ,x ,x ,x ,x ,x ,3 ,2 ,3 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ],
      [x ,x ,8 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,2 ,x ,x ],
      [x ,x ,x ,x ,8 ,x ,x ,x ,8 ,x ,x ,x ,8 ,x ,x ,x ,5 ,x ,x ,x ,x ],
      [x ,4 ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,8 ,x ],
      [x ,x ,2 ,x ,x ,6 ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,8 ,x ,x ,4 ,x ,x ],
      [x ,x ,x ,6 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,8 ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,4 ,3 ,x ,x ,x ,2 ,5 ,x ,x ,x ,x ,x ,x ,x ],
      [x ,2 ,x ,x ,x ,10,x ,x ,x ,3 ,x ,4 ,x ,x ,x ,3 ,x ,x ,x ,x ,x ],
      [x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ],
      [x ,x ,x ,12,x ,x ,4 ,x ,x ,x ,8 ,x ,x ,x ,6 ,x ,x ,2 ,x ,x ,x ],
      [x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ],
      [12,x ,x ,x ,x ,x ,x ,x ,4 ,x ,x ,x ,8 ,x ,x ,x ,x ,x ,x ,x ,10],
      [x ,x ,12,x ,x ,x ,x ,x ,x ,3 ,x ,4 ,x ,x ,x ,x ,x ,x ,6 ,x ,x ],
      [x ,x ,x ,x ,6 ,x ,x ,x ,x ,x ,3 ,x ,x ,x ,x ,x ,12,x ,x ,x ,x ]]).

end_of_file.

