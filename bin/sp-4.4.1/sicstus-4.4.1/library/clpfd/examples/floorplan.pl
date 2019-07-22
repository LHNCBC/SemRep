:- module(floorplan, [floorplan/2]).

/***

Floorplanning benchmarks from:

@PhdThesis{medjdoub96,
  author = 	 {B. Medjdoub},
  title = 	 {M\'ethode de conception fonctionnelle en architecture: une approche {CAO} bas\'ee sur les contraintes: {ARCHiPLAN}},
  school = 	 {\'Ecole Centrale de Paris},
  year = 	 {1996}
}

***/

:- use_module(library(lists)).
:- use_module(library(avl)).
:- use_module(library(clpfd)).

:- discontiguous
	space/12,
	symmetry/2,
	contour/3,
	adj/4,
	hint/2.

:- dynamic
	space/12,
	symmetry/2,
	contour/3,
	adj/4,
	hint/2.
 
% used by floorplan/2.
:- public space/12.

%% DATA TYPES
%% chamber(KIND, NAME, X, L, Y, W, Z, H, A)
%%   KIND ::= floor | room | corridor
%%   NAME ::= home | living | room1 | room2 | room3 | room4 | kitchen | shower | toilet | corridor1 | corridor2

%% PROBLEM INSTANCE (from Robert Maculet PhD thesis)

%% DECLARATION OF THE DIFFERENT SPACE WITH THEIR RESPECTIVE DIMENSIONAL CONSTRAINTS
%% space(kind, name, storey, min_len, max_len, min_width, max_width, min_height, max_height, min_surf, max_surf).
%% (first a floor fact for which all sizes are fixed)
space(maculet1, floor   , home     , 1, 12, 12, 10, 10, 1, 1,  120,  120).
space(maculet1, room    , living   , 1,  4,  _,  4,  _, 1, 1,   33,   42).
space(maculet1, corridor, corridor1, 1,  1,  _,  1,  _, 1, 1,    1,   12).
space(maculet1, corridor, corridor2, 1,  1,  _,  1,  _, 1, 1,    1,   12).
space(maculet1, room    , room1    , 1,  3,  _,  3,  _, 1, 1,   11,   15).
space(maculet1, room    , room2    , 1,  3,  _,  3,  _, 1, 1,   11,   15).
space(maculet1, room    , room3    , 1,  3,  _,  3,  _, 1, 1,   11,   15).
space(maculet1, room    , room4    , 1,  3,  _,  3,  _, 1, 1,   15,   20).
space(maculet1, room    , kitchen  , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(maculet1, room    , shower   , 1,  2,  _,  2,  _, 1, 1,    6,    9).
space(maculet1, room    , toilet   , 1,  1,  _,  1,  _, 1, 1,    1,    2).

%% DECLARATION OF THE ORIENTATIONS CONSTRAINTS
%% contour(room or corridor, list of directions: n, s, w, e, nw, sw, ne, se).
contour(maculet1, living , [sw]).
contour(maculet1, kitchen, [s,n]).
contour(maculet1, room1  , [s,n]).
contour(maculet1, room2  , [s,n]).
contour(maculet1, room3  , [s,n]).
contour(maculet1, room4  , [s]).

%% DECLARATION OF THE ADJACENCY CONSTRAINTS
%% adj(room or corridor, list of rooms or corridors).
adj(maculet1, living   , [corridor1,corridor2], 1).
adj(maculet1, shower   , [corridor1,corridor2], 1).
adj(maculet1, toilet   , [corridor1,corridor2], 1).
adj(maculet1, room1    , [corridor1,corridor2], 1).
adj(maculet1, room2    , [corridor1,corridor2], 1).
adj(maculet1, room3    , [corridor1,corridor2], 1).
adj(maculet1, room4    , [corridor1,corridor2], 1).
adj(maculet1, kitchen  , [living]             , 1).
adj(maculet1, kitchen  , [shower]             , 1).
adj(maculet1, toilet   , [kitchen,shower]     , 1).
adj(maculet1, corridor1, [corridor2]          , 1).

% convenience, and also search order
symmetry(maculet1, [living]).
symmetry(maculet1, [corridor1,corridor2]).
symmetry(maculet1, [room1,room2,room3]).
symmetry(maculet1, [room4]).
symmetry(maculet1, [kitchen]).
symmetry(maculet1, [shower]).
symmetry(maculet1, [toilet]).

hint(maculet1, 2*(corridor1^length) + 2*(corridor1^height) + 2*(corridor2^length) + 2*(corridor2^height) - 2 #>= 7).
hint(maculet1, minimize(corridor1^area + corridor2^area)).
/***
hint(maculet1, visavis(2*(corridor1^length) + 2*(corridor1^height) + 2*(corridor2^length) + 2*(corridor2^height) #>=
		       visavis(corridor1,corridor2) +
		       visavis(corridor1,living) +
		       visavis(corridor1,shower) +
		       visavis(corridor1,toilet) +
		       visavis(corridor1,room1) +
		       visavis(corridor1,room2) +
		       visavis(corridor1,room3) +
		       visavis(corridor1,room4) +
		       visavis(corridor2,living) +
		       visavis(corridor2,shower) +
		       visavis(corridor2,toilet) +
		       visavis(corridor2,room1) +
		       visavis(corridor2,room2) +
		       visavis(corridor2,room3) +
		       visavis(corridor2,room4))).
***/



%% PROBLEM INSTANCE, modification of the above

%% DECLARATION OF THE DIFFERENT SPACE WITH THEIR RESPECTIVE DIMENSIONAL CONSTRAINTS
%% space(kind, name, storey, min_len, max_len, min_width, max_width, min_height, max_height, min_surf, max_surf).
%% (first a floor fact for which all sizes are fixed)
space(maculet2, floor   , home     , 1, 12, 12, 10, 10, 1, 1,  120,  120).
space(maculet2, room    , living   , 1,  4,  _,  4,  _, 1, 1,   33,   42).
space(maculet2, corridor, corridor1, 1,  1,  _,  1,  _, 1, 1,    1,   12).
space(maculet2, corridor, corridor2, 1,  1,  _,  1,  _, 1, 1,    1,   12).
space(maculet2, room    , room1    , 1,  3,  _,  3,  _, 1, 1,   11,   15).
space(maculet2, room    , room2    , 1,  3,  _,  3,  _, 1, 1,   11,   15).
space(maculet2, room    , room3    , 1,  3,  _,  3,  _, 1, 1,   11,   15).
space(maculet2, room    , room4    , 1,  3,  _,  3,  _, 1, 1,   15,   20).
space(maculet2, room    , kitchen  , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(maculet2, room    , shower   , 1,  2,  _,  2,  _, 1, 1,    4,    9).	% NB
space(maculet2, room    , toilet   , 1,  1,  _,  1,  _, 1, 1,    1,    2).

%% DECLARATION OF THE ORIENTATIONS CONSTRAINTS
%% contour(room or corridor, list of directions: n, s, w, e, nw, sw, ne, se).
contour(maculet2, living , [sw]).
contour(maculet2, corridor1, [s,n,w,e]). % NB
contour(maculet2, kitchen, [s,n]).
contour(maculet2, room1  , [s,n]).
contour(maculet2, room2  , [s,n]).
contour(maculet2, room3  , [s,n]).
contour(maculet2, room4  , [s]).

%% DECLARATION OF THE ADJACENCY CONSTRAINTS
%% adj(room or corridor, list of rooms or corridors, 1).
adj(maculet2, living   , [corridor1,corridor2], 1).
adj(maculet2, shower   , [corridor1,corridor2], 1).
adj(maculet2, toilet   , [corridor1,corridor2], 1).
adj(maculet2, room1    , [corridor1,corridor2], 1).
adj(maculet2, room2    , [corridor1,corridor2], 1).
adj(maculet2, room3    , [corridor1,corridor2], 1).
adj(maculet2, room4    , [corridor1,corridor2], 1).
adj(maculet2, kitchen  , [corridor1,corridor2], 1). % NB
adj(maculet2, kitchen  , [living]             , 1).
adj(maculet2, kitchen  , [shower]             , 1).
adj(maculet2, toilet   , [kitchen,shower]     , 1).
adj(maculet2, corridor1, [corridor2]          , 1).

% convenience, and also search order
symmetry(maculet2, [living]).
symmetry(maculet2, [corridor1]). % NB
symmetry(maculet2, [corridor2]). % NB
symmetry(maculet2, [room1,room2,room3]).
symmetry(maculet2, [room4]).
symmetry(maculet2, [kitchen]).
symmetry(maculet2, [shower]).
symmetry(maculet2, [toilet]).

hint(maculet2, 2*(corridor1^length) + 2*(corridor1^height) + 2*(corridor2^length) + 2*(corridor2^height) - 2 #>= 8).
hint(maculet2, minimize(corridor1^area + corridor2^area)).



%% PROBLEM INSTANCE: from Medjdoub PhD: Offices

%% DECLARATION OF THE DIFFERENT SPACE WITH THEIR RESPECTIVE DIMENSIONAL CONSTRAINTS
%% space(kind, name, storey, min_len, max_len, min_width, max_width, min_height, max_height, min_surf, max_surf).
%% (first a floor fact for which all sizes are fixed)
space(bureaux, floor   , home     , 1, 15, 15, 15, 15, 1, 1,  225,  225).
space(bureaux, room    , patio    , 1,  7,  7,  7,  7, 1, 1,   49,   49).
space(bureaux, corridor, corridor1, 1,  1,  _,  1,  _, 1, 1,    1,   30).
space(bureaux, corridor, corridor2, 1,  1,  _,  1,  _, 1, 1,    1,   30).
space(bureaux, room    , room1    , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(bureaux, room    , room2    , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(bureaux, room    , room3    , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(bureaux, room    , room4    , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(bureaux, room    , room5    , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(bureaux, room    , room6    , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(bureaux, room    , room7    , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(bureaux, room    , room8    , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(bureaux, room    , room9    , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(bureaux, room    , room10   , 1,  3,  _,  3,  _, 1, 1,    9,   15).
space(bureaux, room    , toilet1  , 1,  2,  _,  2,  _, 1, 1,    6,    9).
space(bureaux, room    , toilet2  , 1,  2,  _,  2,  _, 1, 1,    6,    9).
space(bureaux, room    , reception, 1,  3,  _,  3,  _, 1, 1,    9,    15).

%% DECLARATION OF THE ORIENTATIONS CONSTRAINTS
%% contour(room or corridor, list of directions: n, s, w, e, nw, sw, ne, se).
contour(bureaux, reception , [s,n,w,e]). % hard
contour(bureaux, patio     , [se]).      % hard

% STRONG HINTS
% contour(bureaux, room1     , [sw]).
% contour(bureaux, room2     , [w]).
% contour(bureaux, room3     , [w]).
contour(bureaux, room4     , [nw]). % don't relax
% contour(bureaux, room5     , [s]).
% contour(bureaux, room8     , [n]).
% contour(bureaux, room9     , [ne]).
% contour(bureaux, room10    , [e]).
% contour(bureaux, reception , [n]).

%% DECLARATION OF THE ADJACENCY CONSTRAINTS
%% adj(room or corridor, list of rooms or corridors).
adj(bureaux, reception, [corridor1,corridor2], 1).
adj(bureaux, room1    , [corridor1,corridor2], 1).
adj(bureaux, room2    , [corridor1,corridor2], 1).
adj(bureaux, room3    , [corridor1,corridor2], 1).
adj(bureaux, room4    , [corridor1,corridor2], 1).
adj(bureaux, room5    , [corridor1,corridor2], 1).
adj(bureaux, room6    , [corridor1,corridor2], 1).
adj(bureaux, room7    , [corridor1,corridor2], 1).
adj(bureaux, room8    , [corridor1,corridor2], 1).
adj(bureaux, room9    , [corridor1,corridor2], 1).
adj(bureaux, room10   , [corridor1,corridor2], 1).
adj(bureaux, corridor1, [corridor2]          , 1).

% convenience, and also search order
symmetry(bureaux, [patio]).
symmetry(bureaux, [room1,room2,room3,room4,room5,room6,room7,room8,room9,room10]).
symmetry(bureaux, [toilet1,toilet2]).
symmetry(bureaux, [reception]).
symmetry(bureaux, [corridor1,corridor2]).

hint(bureaux, 2*(corridor1^length) + 2*(corridor1^height) + 2*(corridor2^length) + 2*(corridor2^height) - 2 #>= 11).



%% PROBLEM INSTANCE: from Medjdoub PhD: 2-storey house

%% DECLARATION OF THE DIFFERENT SPACE WITH THEIR RESPECTIVE DIMENSIONAL CONSTRAINTS
%% space(kind, name, storey, min_len, max_len, min_width, max_width, min_height, max_height, min_surf, max_surf).
%% (first a floor fact for which all sizes are fixed)
space(house, floor   , home     , 1, 20, 20, 16, 16, 2, 2,  640,  640).	% N Volume of the two storeys
space(house, room    , living   , 1,  6,  _,  6,  _, 1, 1,   72,  128).
space(house, room    , kitchen  , 1,  5,  _,  5,  _, 1, 1,   36,   60).
space(house, room    , bathroom1, 1,  4,  _,  4,  _, 1, 1,   16,   36).
space(house, room    , office   , 1,  6,  _,  6,  _, 1, 1,   36,   60).
space(house, corridor, corridor1, 1,  3,  _,  3,  _, 1, 1,    4,   64).
space(house, stairs  , stairs   , 1,  4,  _,  6,  _, 2, 2,   48,   56).	% NB Volume of stairs
space(house, room    , room1    , 2,  6,  _,  6,  _, 1, 1,   36,   60). % RELAX!
space(house, room    , room2    , 2,  6,  _,  6,  _, 1, 1,   48,   60).
space(house, room    , room3    , 2,  6,  _,  6,  _, 1, 1,   48,   60).
space(house, room    , room4    , 2,  6,  _,  6,  _, 1, 1,   48,   72).
space(house, room    , bathroom2, 2,  4,  _,  4,  _, 1, 1,   16,   36).
space(house, room    , bathroom3, 2,  4,  _,  4,  _, 1, 1,   16,   36).
space(house, room    , balcony  , 2,  3,  _,  3,  _, 1, 1,   12,   24).
space(house, corridor, corridor2, 2,  3,  _,  3,  _, 1, 1,    4,   64).

%% DECLARATION OF THE ORIENTATIONS CONSTRAINTS
%% contour(room or corridor, list of directions: n, s, w, e, nw, sw, ne, se).
contour(house, living    , [s]). % hard
contour(house, kitchen   , [s,n]). % hard
contour(house, balcony   , [s]). % hard
% contour(house, living    , [se]).
contour(house, office    , [nw]). % hint -- don't relax
% contour(house, stairs    , [n ]). % hint
% contour(house, bathroom1 , [sw]). % hint
% contour(house, kitchen   , [s ]). % hint
contour(house, room1     , [nw]). % hint -- 5 minutes if relaxed
contour(house, room2     , [se]). % hint -- don't relax
% contour(house, room3     , [ne]). % hint
% contour(house, room4     , [s ]). % hint
% contour(house, balcony   , [sw]). % hint
% contour(house, bathroom2 , [w ]). % hint
% contour(house, bathroom3 , [n ]). % hint
% contour(house, corridor1 , [w ]). % hint

contour(house, office    , [s,n,w,e]). % weak hint
contour(house, bathroom1 , [s,n,w,e]). % weak hint
contour(house, room1     , [s,n,w,e]). % weak hint
contour(house, room2     , [s,n,w,e]). % weak hint
contour(house, room3     , [s,n,w,e]). % weak hint
contour(house, room4     , [s,n,w,e]). % weak hint
contour(house, bathroom2 , [s,n,w,e]). % weak hint
contour(house, bathroom3 , [s,n,w,e]). % weak hint


%% DECLARATION OF THE ADJACENCY CONSTRAINTS
%% adj(room or corridor, list of rooms or corridors).
adj(house, living   , [corridor1], 2).
adj(house, kitchen  , [corridor1], 2).
adj(house, bathroom1, [corridor1], 2).
adj(house, office   , [corridor1], 2).
adj(house, kitchen  , [living   ], 2).
adj(house, kitchen  , [bathroom1], 2).
adj(house, room1    , [corridor2], 2).
adj(house, room2    , [corridor2], 2).
adj(house, room3    , [corridor2], 2).
adj(house, room4    , [corridor2], 2).
adj(house, bathroom2, [corridor2], 2).
adj(house, bathroom3, [corridor2], 2).
adj(house, room4    , [bathroom2], 2).
adj(house, room4    , [balcony  ], 2).

% convenience, and also search order
symmetry(house, [corridor1]).
symmetry(house, [corridor2]).
symmetry(house, [bathroom1]).
symmetry(house, [bathroom2]).
symmetry(house, [bathroom3]).
symmetry(house, [room4]).
symmetry(house, [room1,room2,room3]).

hint(house, 2*(corridor1^length) + 2*(corridor1^height) #>= 8).
hint(house, 2*(corridor2^length) + 2*(corridor2^height) #>= 14).



%% PROBLEM INSTANCE: from Medjdoub PhD: Kindergarten

%% DECLARATION OF THE DIFFERENT SPACE WITH THEIR RESPECTIVE DIMENSIONAL CONSTRAINTS
%% space(kind, name, storey, min_len, max_len, min_width, max_width, min_height, max_height, min_surf, max_surf).
%% (first a floor fact for which all sizes are fixed)
space(kindergarten, floor   , home     , 1, 28, 28, 50, 50, 1, 1, 1400, 1400).
space(kindergarten, corridor, patio1   , 1, 24, 24,  9,  9, 1, 1,  160,  240).
space(kindergarten, corridor, patio2   , 1,  8,  8, 20, 20, 1, 1,  160,  240).
space(kindergarten, root    , restarea , 1, 10,  _, 10,  _, 1, 1,  140,  160).
space(kindergarten, room    , rhythm   , 1, 28, 28,  9,  9, 1, 1,  140,  280).
space(kindergarten, room    , class1   , 1,  8,  _,  8,  _, 1, 1,   90,  120).
space(kindergarten, room    , class2   , 1,  8,  _,  8,  _, 1, 1,   90,  120).
space(kindergarten, room    , class3   , 1,  8,  _,  8,  _, 1, 1,   90,  120).
space(kindergarten, room    , class4   , 1,  8,  _,  8,  _, 1, 1,   90,  120).
space(kindergarten, room    , class5   , 1,  8,  _,  8,  _, 1, 1,   90,  120).
space(kindergarten, room    , kitchen  , 1,  4,  _,  4,  _, 1, 1,  25,    35).
space(kindergarten, room    , bathroom , 1,  4,  _,  4,  _, 1, 1,  25,    35).
space(kindergarten, room    , reception, 1,  4,  _,  4,  _, 1, 1,  20,    35).
space(kindergarten, room    , directors, 1,  3,  _,  3,  _, 1, 1,  10,    15).
space(kindergarten, room    , teachers , 1,  3,  _,  3,  _, 1, 1,  10,    30).
space(kindergarten, room    , waitarea , 1,  3,  _,  3,  _, 1, 1,  12,    30).
space(kindergarten, room    , toilet   , 1,  3,  _,  3,  _, 1, 1,  10,    15).
space(kindergarten, room    , rangement, 1,  3,  _,  3,  _, 1, 1,  10,    15).
space(kindergarten, room    , sickbay  , 1,  3,  _,  3,  _, 1, 1,  10,    15).

%% DECLARATION OF THE ORIENTATIONS CONSTRAINTS
%% contour(room or corridor, list of directions: n, s, w, e, nw, sw, ne, se).
contour(kindergarten, class1    , [s,n,w,e]). % hard
contour(kindergarten, class2    , [s,n,w,e]). % hard
contour(kindergarten, class3    , [s,n,w,e]). % hard
contour(kindergarten, class4    , [s,n,w,e]). % hard
contour(kindergarten, class5    , [s,n,w,e]). % hard
contour(kindergarten, reception , [s,n,w,e]). % hard

%% DECLARATION OF THE ADJACENCY CONSTRAINTS
%% adj(room or corridor, list of rooms or corridors).
adj(kindergarten, patio1   , [patio2], 1).
adj(kindergarten, class1   , [patio2], 1).
adj(kindergarten, class2   , [patio2], 1).
adj(kindergarten, class3   , [patio2], 1).
adj(kindergarten, class4   , [patio2], 1).
adj(kindergarten, class5   , [patio2], 1).
adj(kindergarten, restarea , [patio2], 1).
adj(kindergarten, bathroom , [patio2], 1).
adj(kindergarten, waitarea , [patio2], 1).
adj(kindergarten, rhythm   , [patio1  ], 1).
adj(kindergarten, rangement, [patio1  ], 1).
adj(kindergarten, sickbay  , [patio1  ], 1).
adj(kindergarten, toilet   , [patio1  ], 1).
adj(kindergarten, waitarea , [reception], 1).

% convenience, and also search order
symmetry(kindergarten, [rhythm]).
symmetry(kindergarten, [patio1]).
symmetry(kindergarten, [patio2]).
symmetry(kindergarten, [class1,class2,class3,class4,class5]).
symmetry(kindergarten, [rangement,sickbay,toilet]).

hint(kindergarten, class1^area #= class2^area).
hint(kindergarten, class1^area #= class3^area).
hint(kindergarten, class1^area #= class4^area).
hint(kindergarten, class1^area #= class5^area).


%% SOLVER (main goal to call)

floorplan(ID, Model) :-
	Space = space(ID,_,_,_,_,_,_,_,_,_,_,_),
	findall(Space, Space, Spaces),
	Floor = space(ID, floor, _, _, Length, Length, Width, Width, Height, Height, Area, Area),
	selectchk(Floor, Spaces, RoomsCorridors),
	(   foreach(RC,RoomsCorridors),
	    foreach(Ch,Chambers),
	    foreach(Shs,Shapes),
	    foreach(A,Areas),
	    param(Length,Width,Height,Area)
	do  RC = space(_,KIND,NAME,Z,MINL,MAXL,MINW,MAXW,MINH,MAXH,MINS,MAXS),
	    Ch = chamber(KIND,NAME,X,L,Y,W,Z,H,A),
	    L in 1..Length,
	    L #>= MINL,
	    L #=< MAXL,
	    W in 1..Width,
	    W #>= MINW,
	    W #=< MAXW,
	    H in 1..Height,
	    H #>= MINH,
	    H #=< MAXH,
	    A in 1..Area,
	    A #>= MINS,
	    A #=< MAXS,
	    A #= L*W*H,
	    X in 1..Length,
	    Y in 1..Width,
	    findall([L,W,H], labeling([],[L,W,H]), Shs)
	),
	sum(Areas, #=, Area),	% does help
	constraints(Model,ID, Chambers, Shapes, Objects, Classes, Length, Width, Height),
	(   foreach(Class,Classes),
	    foreach(Pairs,PairClasses),
	    param(Chambers,Objects)
	do  (   foreach(Name,Class),
		foreach(C-O,Pairs),
		param(Chambers,Objects)
	    do  C = chamber(_,Name,_,_,_,_,_,_,_),
		nth0(I, Chambers, C),
		nth0(I, Objects, O) -> true	    
	    )
	),
	findall(Hint1, hint(ID,Hint1), Hints),
	(   foreach(Hint,Hints),
	    param(Chambers)
	do  do_hint(Hint, Chambers)
	),
	points(Length, Width, Height, Points0, []),
	(   fromto(Points0,[P|Points1],Points,[]),
	    fromto(PairClasses,PairClasses1,PairClasses2,[])
	do  Ch1 = chamber(_,_,_,L1,_,W1,_,H1,A1),
	    select_first(Ch1-Obj, PairClasses1, PairClasses2),
	    Obj = object(_,Sid,P),
	    labeling([bisect], [A1,Sid]),
	    P = [X1,Y1,Z1], 
	    (   foreach(P1,Points1),
		fromto(Points,Ps0,Ps,[]),
		param(L1,W1,H1,X1,Y1,Z1)
	    do  (   P1 = [Px,Py,Pz],
		    Px >= X1,
		    Px < X1+L1,
		    Py >= Y1,
		    Py < Y1+W1,
		    Pz >= Z1,
		    Pz < Z1+H1 -> Ps0 = Ps
		;   Ps0 = [P1|Ps]
		)
	    )
	), !,
	format('solution = ~q\n', [Chambers]).

points(Length, Width, Height) -->
	(   for(X,1,Length),
	    param(Width,Height)
	do  (   for(Y,1,Width),
		param(X,Height)
	    do  (   for(Z,1,Height),
		    param(X,Y)
		do  [[X,Y,Z]]
		)
	    )
	).

select_first(X, [[X]|R],     R).
select_first(X, [[X|Xs]|R],  [Xs|R]) :- Xs\==[].
select_first(X, [A|L],       [A|R]) :-
	select_first(X, L, R).

do_hint(minimize(SExpr), Chambers) :- !,
	parse(SExpr, Expr, Chambers),
	call(Var #= Expr),
	indomain(Var).
do_hint(SExpr1 #= SExpr2, Chambers) :- !,
	parse(SExpr1, Expr1, Chambers),
	parse(SExpr2, Expr2, Chambers),
	call(Expr1 #= Expr2).
do_hint(SExpr1 #>= SExpr2, Chambers) :- !,
	parse(SExpr1, Expr1, Chambers),
	parse(SExpr2, Expr2, Chambers),
	call(Expr1 #>= Expr2).
do_hint(_, _).

parse(X, X, _) :-
	simple(X), !.
parse(X1+X2, Y1+Y2, Chambers) :-
	parse(X1, Y1, Chambers),
	parse(X2, Y2, Chambers).
parse(X1-X2, Y1-Y2, Chambers) :-
	parse(X1, Y1, Chambers),
	parse(X2, Y2, Chambers).
parse(X1*X2, Y1*Y2, Chambers) :-
	parse(X1, Y1, Chambers),
	parse(X2, Y2, Chambers).
parse(Name^Attr, Var, Chambers) :-
	C = chamber(_,Name,_,_,_,_,_,_,_),
	memberchk(C, Chambers),
	parse_attr(Attr, C, Var).

parse_attr(length, chamber(_,_,_,L,_,_,_,_,_), L).
parse_attr(width,  chamber(_,_,_,_,_,W,_,_,_), W).
parse_attr(height, chamber(_,_,_,_,_,_,_,H,_), H).
parse_attr(area,   chamber(_,_,_,_,_,_,_,_,A), A).

constraints(InOut, ID, Chambers, Shapes, Objects, Classes, Length, Width, Height) :-
	Length1 is Length+1,
	Width1 is Width+1,
	Height1 is Height+1,
	append(Shapes, List1),
	sort(List1, List2),
	(   foreach(LWH,List2),
	    foreach(sbox(I,[0,0,0],LWH),Sboxes),
	    foreach([I|LWH],Extension),
	    count(I,1,_)
	do  true
	),
	(   foreach(chamber(_,Name,X,L,Y,W,Z,H,_),Chambers),
	    foreach(Shs,Shapes),
	    foreach(object(J,Sid,[X,Y,Z]),Objects),
	    foreach([Sid,L,W,H],Tuples),
	    foreach(Name-J,N2Olist),
	    count(J,1,_),
	    param(Sboxes)
	do  (   foreach(Sh,Shs),
		foreach(S,Sids),
		param(Sboxes)
	    do  memberchk(sbox(S,_,Sh), Sboxes)
	    ),
	    list_to_fdset(Sids, Fdset),
	    Sid in_set Fdset
	),
	table(Tuples, Extension),
	list_to_avl(N2Olist, N2O),
	symmetries(ID, Chambers, Classes, N2O, Lexes),
	Volume is Length*Width*Height,
	(   InOut==(in) ->
	    macros(Rules, Rules0),
	    contour_rules(N2Olist, ID, Length1, Width1, Rules0, Rules1),
	    adjacency_rules(N2Olist, ID, N2O, Rules1, Rules2),
	    (   hint(ID, visavis(Cond)) ->
		visavis_lib(Rules2, [Cond1]),
		visavis_convert(Cond, Cond1, N2O)
	    ;   Rules2 = []
	    )
	;   Rules = [],
	    findall(Name1-Cont, contour(ID,Name1,Cont), List3),
	    (   foreach(Name2-Contour,List3),
		param(Chambers,Length1,Width1)
	    do  Chamber = chamber(_,Name2,_,_,_,_,_,_,_),
		memberchk(Chamber, Chambers),
		(   foreach(C,Contour),
		    fromto(DISJ,Dis0,Dis,0),
		    param(Chamber,Length1,Width1)
		do  gen_contour3(C, Chamber, Length1, Width1, Dis0, Dis)
		),
		call(DISJ)
	    ),
	    findall(adj(Name3,Adjacencies,K), adj(ID,Name3,Adjacencies,K), List4),
	    (   foreach(adj(Name4,Adjacencies4,K4),List4),
		param(Chambers)
	    do  Chamber4 = chamber(_,Name4,_,_,_,_,_,_,_),
		memberchk(Chamber4, Chambers),
		(   foreach(Name5,Adjacencies4),
		    fromto(DISJ4,Disj0,Disj,[]),
		    param(K4,Chamber4,Chambers)
		do  X1+L1#=X2 #<=> B1,
		    X2+L2#=X1 #<=> B2,
		    Y1+W1#=Y2 #<=> B3,
		    Y2+W2#=Y1 #<=> B4,
		    Y1+W1#>=Y2+K4 #<=> B5,
		    X1+L1#>=X2+K4 #<=> B6,
		    Y2+W2#>=Y1+K4 #<=> B7,
		    X2+L2#>=X1+K4 #<=> B8,
		    (B1 #/\ B5 #/\ B7) #<=> B9,
		    (B2 #/\ B5 #/\ B7) #<=> B10,
		    (B3 #/\ B6 #/\ B8) #<=> B11,
		    (B4 #/\ B6 #/\ B8) #<=> B12,
		    Disj0 = [B9 , B10 , B11 , B12 | Disj],
		    Chamber5 = chamber(_,Name5,X1,L1,Y1,W1,_,_,_),
		    Chamber4 = chamber(_,    _,X2,L2,Y2,W2,_,_,_),
		    memberchk(Chamber5, Chambers)		
		),
		bool_or(DISJ4, 1)
	    )
	),
	geost(Objects, Sboxes, [volume(Volume),cumulative(true),bounding_box([1,1,1],[Length1,Width1,Height1])|Lexes], Rules).

gen_contour3(n, OBJ, _Length, Width, Y+W#=Width #\/ R, R) :-
	OBJ = chamber(_,_,_,_,Y,W,_,_,_).
gen_contour3(s, OBJ, _Length, _Width, Y#=1 #\/ R, R) :-
	OBJ = chamber(_,_,_,_,Y,_,_,_,_).
gen_contour3(e, OBJ, Length, _Width, X+L#=Length #\/ R, R) :-
	OBJ = chamber(_,_,X,L,_,_,_,_,_).
gen_contour3(w, OBJ, _Length, _Width, X#=1 #\/ R, R) :-
	OBJ = chamber(_,_,X,_,_,_,_,_,_).
gen_contour3(nw, OBJ, _Length, Width, (X#=1 #/\ Y+W#=Width) #\/ R, R) :-
	OBJ = chamber(_,_,X,_,Y,W,_,_,_).
gen_contour3(ne, OBJ, Length, Width, (X+L#=Length #/\ Y+W#=Width) #\/ R, R) :-
	OBJ = chamber(_,_,X,L,Y,W,_,_,_).
gen_contour3(se, OBJ, Length, _Width, (X+L#=Length #/\ Y#=1) #\/ R, R) :-
	OBJ = chamber(_,_,X,L,Y,_,_,_,_).
gen_contour3(sw, OBJ, _Length, _Width, (X#=1 #/\ Y#=1) #\/ R, R) :-
	OBJ = chamber(_,_,X,_,Y,_,_,_,_).

macros -->
	[(origin(O1,S1,D) ---> O1^x(D)+S1^t(D)),
	 (end(O1,S1,D) ---> O1^x(D)+S1^t(D)+S1^l(D)),
	 (overlap(O1,S1,O2,S2,I,D) --->
	     end(O1,S1,D) #>= origin(O2,S2,D)+I #/\
	     end(O2,S2,D) #>= origin(O1,S1,D)+I),
	 (qadjacent(O1,S1,O2,S2,I) --->
	     ((origin(O1,S1,1) #= end(O2,S2,1) #\/ origin(O2,S2,1) #= end(O1,S1,1)) #/\ overlap(O1,S1,O2,S2,I,2)) #\/
	     ((origin(O1,S1,2) #= end(O2,S2,2) #\/ origin(O2,S2,2) #= end(O1,S1,2)) #/\ overlap(O1,S1,O2,S2,I,1))),
	 (adjacent(OID1,OID2,I) --->
	     forall(O1,objects([OID1]), forall(S1,sboxes([O1^sid]), 
	         forall(O2,objects([OID2]), forall(S2,sboxes([O2^sid]),
	             qadjacent(O1,S1,O2,S2,I))))))].
	  
contour_rules([], _, _, _) --> [].
contour_rules([Name-OID|N2Olist], ID, L, H) -->
	{findall(Cont1, contour(ID,Name,Cont1), Conts)},
	(   foreach(Cont,Conts),
	    param(OID,L,H)
	do  [Rule],
	    {   foreach(Ori,Cont),
		fromto(Rule,R#\/Rs,Rs,false),
		param(OID,L,H)
	    do  contour_rule(Ori, OID, R, L, H)
	    }
	),
	contour_rules(N2Olist, ID, L, H).

contour_rule(n, OID, (forall(O,objects([OID]), forall(S,sboxes([O^sid]), end(O,S,2)#=H))), _, H).
contour_rule(s, OID, (forall(O,objects([OID]), forall(S,sboxes([O^sid]), origin(O,S,2)#=1))), _, _).
contour_rule(e, OID, (forall(O,objects([OID]), forall(S,sboxes([O^sid]), end(O,S,1)#=L))), L, _).
contour_rule(w, OID, (forall(O,objects([OID]), forall(S,sboxes([O^sid]), origin(O,S,1)#=1))), _, _).
contour_rule(nw, OID, R1 #/\ R2, L, H) :-
	contour_rule(n, OID, R1, L, H),
	contour_rule(w, OID, R2, L, H).
contour_rule(ne, OID, R1 #/\ R2, L, H) :-
	contour_rule(n, OID, R1, L, H),
	contour_rule(e, OID, R2, L, H).
contour_rule(sw, OID, R1 #/\ R2, L, H) :-
	contour_rule(s, OID, R1, L, H),
	contour_rule(w, OID, R2, L, H).
contour_rule(se, OID, R1 #/\ R2, L, H) :-
	contour_rule(s, OID, R1, L, H),
	contour_rule(e, OID, R2, L, H).

adjacency_rules([], _, _) --> [].
adjacency_rules([Name-OID|N2Olist], ID, N2O) -->
	{findall(Nbs1-Atleast1, adj(ID,Name,Nbs1,Atleast1), Nbss)},
	{Nbss \== []}, !,
	{avl_fetch(Name, N2O, OID)},
	(   foreach(Nbs-Atleast,Nbss),
	    param(OID,N2O)
	do  [Rule],
	    {   foreach(Nb,Nbs),
		fromto(Rule,adjacent(OID,OID2,Atleast) #\/ Rule0,Rule0,false),
		param(Atleast,OID,N2O)
	    do  avl_fetch(Nb, N2O, OID2)
	    }
	),
	adjacency_rules(N2Olist, ID, N2O).
adjacency_rules([_|N2Olist], ID, N2O) --> 
	adjacency_rules(N2Olist, ID, N2O).

visavis_lib -->
	[(common(O1,S1,O2,S2,D) --->
	  min(end(O2,S2,D)-origin(O1,S1,D),end(O1,S1,D)-origin(O2,S2,D))),
	 (diff(O1,S1,O2,S2,D) --->
	  max(end(O1,S1,D)-origin(O2,S2,D),origin(O2,S2,D)-end(O1,S1,D))),
	 (visavis(O1,S1,O2,S2) --->
	  max(0,common(O1,S1,O2,X2,1) - 1000*diff(O1,S1,O2,S2,2)) +
	  max(0,common(O1,S1,O2,X2,1) - 1000*diff(O2,S2,O1,S1,2)) +
	  max(0,common(O1,S1,O2,X2,2) - 1000*diff(O1,S1,O2,S2,1)) +
	  max(0,common(O1,S1,O2,X2,2) - 1000*diff(O2,S2,O1,S1,1)))].

visavis_convert(Rule1, Rule2, N2Oid) :-
	avl_to_list(N2Oid, N2Oidlist),
	(   foreach(Name-Oid,N2Oidlist),
	    foreach(Name-O,N2Olist),
	    foreach(Name-S,N2Slist),
	    fromto(Rule2,exists(O,objects([Oid]),exists(S,sboxes([O^sid]),Rule2b)),Rule2b,Rule3)
	do  true
	),
	ord_list_to_avl(N2Olist, N2O),
	ord_list_to_avl(N2Slist, N2S),
	visavis_parse(Rule1, Rule3, N2O, N2S).

visavis_parse(X#>=Y, X1#>=Y1, N2O, N2S) :- !,
	visavis_parse(X, X1, N2O, N2S),
	visavis_parse(Y, Y1, N2O, N2S).
visavis_parse(X+Y, X1+Y1, N2O, N2S) :- !,
	visavis_parse(X, X1, N2O, N2S),
	visavis_parse(Y, Y1, N2O, N2S).
visavis_parse(X*Y, X1*Y1, N2O, N2S) :- !,
	visavis_parse(X, X1, N2O, N2S),
	visavis_parse(Y, Y1, N2O, N2S).
visavis_parse(X^length, S^l(1), _, N2S) :- !,
	avl_fetch(X, N2S, S).
visavis_parse(X^height, S^l(2), _, N2S) :- !,
	avl_fetch(X, N2S, S).
visavis_parse(visavis(N1,N2), visavis(O1,S1,O2,S2), N2O, N2S) :- !,
	avl_fetch(N1, N2O, O1),
	avl_fetch(N2, N2O, O2),
	avl_fetch(N1, N2S, S1),
	avl_fetch(N2, N2S, S2).
visavis_parse(X, X, _, _).

symmetries(ID, Chambers, AllClasses, N2O, Lexes) :-
	findall(Class, symmetry(ID,Class), Classes),
	(   foreach(Ch,Chambers),
	    foreach(Name1,Names1)
	do  arg(2, Ch, Name1)
	),
	symmetry_classes(Classes, Names1, AllClasses, []),
	(   foreach(Names,Classes),
	    fromto(Lexes,S0,S,[]),
	    param(N2O)
	do  (   Names = [_,_|_] ->
		S0 = [lex(OIDs)|S],
		(   foreach(Name,Names),
		    foreach(OID,OIDs),
		    param(N2O)
		do  avl_fetch(Name, N2O, OID)
		)
	    ;   S0 = S
	    )	    
	).

symmetry_classes([], Names) -->
	(   foreach(Name,Names)
	do  [[Name]]
	).
symmetry_classes([Class|Classes], Names0) --> [Class],
	{   foreach(X,Class),
	    fromto(Names0,Names1,Names2,Names)
	do  selectchk(X, Names1, Names2)
	},
	symmetry_classes(Classes, Names).

