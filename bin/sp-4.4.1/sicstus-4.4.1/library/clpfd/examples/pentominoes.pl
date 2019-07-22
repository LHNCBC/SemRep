:- module(pentominoes, [pentominoes/1]).

:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(clpfd)).

pentominoes(Gabarit) :-
	pentominoes(Gabarit, Objects, Shapes, Map),
	draw_board(Gabarit, Objects, Shapes, Map).

pentominoes(Gabarit, Objects, Shapes, Map) :-
	Gabarit = gabarit(Length,Depth,Height),
	problem(Gabarit, Objects, Shapes, Map),
	NCells is Length*Depth*Height,
	make_points(NCells, Depth, Height, Points, []),
	(   fromto(Points,Ps0,Ps,[]),
	    fromto(Objects,Todo,Rest,_),
	    param(Shapes)
	do  Ps0 = [X|Ps1],
	    Piece = object(_,SID,X),
	    select(Piece, Todo, Rest),
	    indomain(SID),
	    covered_by(Piece, Shapes, Del, []),
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

draw_board(Gabarit, Objects, Shapes, Map) :-
	Gabarit = gabarit(Length,Depth,Height),
	(   for(Z,0,Height-1),
	    param(Length,Depth,Objects,Shapes,Map)
	do  format('+~*c+\n', [Length,0'-]),
	    (   for(Y,0,Depth-1),
		param(Length,Z,Objects,Shapes,Map)
	    do  write('|'),
		(   for(X,0,Length-1),
		    param(Y,Z,Objects,Shapes,Map)
		do  (   member(object(OID,SID,[Xo,Yo,Zo]), Objects),
			member(sbox(SID,[Xt,Yt,Zt],[Xl,Yl,Zl]), Shapes),
			Xo+Xt =< X,
			X < Xo+Xt+Xl,
			Yo+Yt =< Y,
			Y < Yo+Yt+Yl,
			Zo+Zt =< Z,
			Z < Zo+Zt+Zl ->
			member(OID-Code, Map),
			write(Code)
		    ;   write(' ')
		    )
		),
		write('|'), nl
	    ),
	    format('+~*c+\n\n', [Length,0'-])
	).

problem(Gabarit, Objects, Shapes, Map) :-
	Gabarit = gabarit(Length,Depth,Height),
	findall(ID-Shape, piece_variant(ID,Shape), Pcs0),
	filter_variants(Pcs0, Pcs1, Gabarit),
	keyclumped(Pcs1, Pcs2),
	(   foreach(Tag-Clump,Pcs2),
	    foreach(OID-Tag,Map),
	    foreach(object(OID,SID3,[X,Y,Z]),Objects),
	    foreach(X,Xs),
	    foreach(Y,Ys),
	    foreach(Z,Zs),
	    count(OID,0,_),
	    fromto(1,SID0,SID1,_)
	do  length(Clump, N),
	    SID1 is SID0+N,
	    SID2 is SID1-1,
	    SID3 in SID0..SID2
	),
	(   foreach(_-Parts,Pcs1),
	    count(SID,1,_),
	    fromto(Shapes,Sh1,Sh3,[])
	do  (   foreach(Tran-Len,Parts),
	        fromto(Sh1,[sbox(SID,Tran,Len)|Sh2],Sh2,Sh3),
		param(SID)
	    do  true
	    )
	),
	L1 is Length-1,
	domain(Xs, 0, L1),
	D1 is Depth-1,
	domain(Ys, 0, D1),
	H1 is Height-1,
	domain(Zs, 0, H1),
  	geost(Objects, Shapes, [polymorphism(false),bounding_box([0,0,0],[Length,Depth,Height])]).

filter_variants([], [], _).
filter_variants([T-S1|Ss1], [T-S3|Ss2], Gabarit) :-
	(   foreach([T1,T2,T3]-[L1,L2,L3],S1),
	    param(Gabarit)
	do  Gabarit = gabarit(L,D,H),
	    T1+L1 =< L,
	    T2+L2 =< D,
	    T3+L3 =< H
	), !,
	keysort(S1, S2),
	S2 = [Decr-_|_],
	(   Decr==[0,0,0] -> S3=S2
	;   (   foreach([X1,Y1,Z1]-Len,S2),
		foreach([X2,Y2,Z2]-Len,S3),
		param(Decr)
	    do  Decr = [Dx,Dy,Dz],
		X2 is X1-Dx,
		Y2 is Y1-Dy,
		Z2 is Z1-Dz
	    )
	),
	filter_variants(Ss1, Ss2, Gabarit).
filter_variants([_|Ss1], Ss2, Gabarit) :-
	filter_variants(Ss1, Ss2, Gabarit).

piece_variant(ID, Variant) :-
	piece(ID, Shape1),
	sort(Shape1, Shape2),
	(   fromto([Shape2],New0,New,[]),
	    fromto([Shape2],Sofar0,Sofar,Closure)
	do  New0 = [B1|New1],
	    findall(B2, transform_shape(B1,B2), Shapes2),
	    sort(Shapes2, Shapes3),
	    ord_union(Sofar0, Shapes3, Sofar, New2),
	    append(New1, New2, New)
	),
	member(Member, Closure),
	compress_shape(Member, Variant).

compress_shape(Pairs1, Pairs) :-
	select(P1, Pairs1, Pairs2),
	select(P2, Pairs2, Pairs3),
	join_sboxes(P1, P2, P3), !,
	compress_shape([P3|Pairs3], Pairs).
compress_shape(Pairs, Pairs).

join_sboxes([Px1,Py,Pz]-[Sx1,Sy,Sz], [Px2,Py,Pz]-[Sx2,Sy,Sz], Join) :-
	Px1+Sx1 =:= Px2, !,
	Sx is Sx1+Sx2,
	Join = [Px1,Py,Pz]-[Sx,Sy,Sz].
join_sboxes([Px,Py1,Pz]-[Sx,Sy1,Sz], [Px,Py2,Pz]-[Sx,Sy2,Sz], Join) :-
	Py1+Sy1 =:= Py2, !,
	Sy is Sy1+Sy2,
	Join = [Px,Py1,Pz]-[Sx,Sy,Sz].
join_sboxes([Px,Py,Pz1]-[Sx,Sy,Sz1], [Px,Py,Pz2]-[Sx,Sy,Sz2], Join) :-
	Pz1+Sz1 =:= Pz2, !,
	Sz is Sz1+Sz2,
	Join = [Px,Py,Pz1]-[Sx,Sy,Sz].

transform_shape(S1, S4) :-
	axis(Axis),
	(   foreach(Box1,S1),
	    foreach(Box2,S2),
	    fromto([100,100,100],Min0,Min1,[X2,Y2,Z2]),
	    param(Axis)
	do  rotate_box(Axis, Box1, Box2),
	    Min0 = [Mx,My,Mz],
	    Box2 = [Nx,Ny,Nz]-_,
	    Min1 = [Ox,Oy,Oz],
	    Ox is min(Mx,Nx),
	    Oy is min(My,Ny),
	    Oz is min(Mz,Nz)
	),
	(   foreach([X1,Y1,Z1]-Offset,S2),
	    foreach([X3,Y3,Z3]-Offset,S3),
	    param(X2,Y2,Z2)
	do  X3 is X1-X2,
	    Y3 is Y1-Y2,
	    Z3 is Z1-Z2
	),
	sort(S3, S4).

axis(x). axis(y). axis(z).

rotate_box(x, [T1,T2,T3]-[L1,L2,L3], [T1,T3,NT2]-[L1,L3,L2]) :-
	rot_negate(T2, L2, NT2).
rotate_box(y, [T1,T2,T3]-[L1,L2,L3], [NT3,T2,T1]-[L3,L2,L1]) :-
	rot_negate(T3, L3, NT3).
rotate_box(z, [T1,T2,T3]-[L1,L2,L3], [T2,NT1,T3]-[L2,L1,L3]) :-
	rot_negate(T1, L1, NT1).

rot_negate(T1, L, T2) :-
	T2 is (1-L)-T1.

piece(Key, Pairs) :-
	% member(Key, [i,y,l,n,v,t,w,x,u,z,f,p]), % default
	member(Key, [i,y,t,n,f,l,v,w,x,z,u,p]), % experimentally determined
	spiece(Key, Lines),
	lines_pairs(Lines, Pairs, []).

:- dynamic spiece/2.

spiece(i, ["*****"]).
spiece(y, ["****",
	   " *  "]).
spiece(l, ["****",
	   "*   "]).
spiece(n, ["*** ",
	   "  **"]).
spiece(v, ["***",
	   "*  ",
	   "*  "]).
spiece(t, ["***",
	   " * ",
	   " * "]).
spiece(w, ["** ",
	   " **",
	   "  *"]).
spiece(x, [" * ",
	   "***",
	   " * "]).
spiece(u, ["***",
	   "* *"]).
spiece(z, ["** ",
	   " * ",
	   " **"]).
spiece(f, ["** ",
	   " **",
	   " * "]).
spiece(p, ["***",
	   "** "]).

lines_pairs(Lines) -->
	(   foreach(Line,Lines),
	    count(Y,0,_)
	do  (   foreach(Char,Line),
		count(X,0,_),
		param(Y)
	    do  ({Char=:=0'*} -> [[X,Y,0]-[1,1,1]] ; [])
	    )
	).

