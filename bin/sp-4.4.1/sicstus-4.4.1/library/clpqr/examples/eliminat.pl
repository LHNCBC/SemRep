%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   eliminat.pl                                            %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*

  If you want to learn more about the subject of quantifier elimination
  you should read:

  Huynh T., Lassez J.L.: Practical Issues on the Projection of Polyhedral Sets,
  IBM Research Division, RC 15872 (#70560), 1990.


  The predicate example/1 below relates the four variables X1,X2,X3,X4
  in some way. The query 'example( [X1,X2,X3,X4])' just returns the
  normalized set of constraints (there is no redundancy).
  Now suppose you want to know: what are the implied relations
  between X3 and X4? The following query expresses this projection:

  [Clp(Q)] ?- example( 1, [_,_,X3,X4]).

  X4=<1+3*X3,
  X4=<4/13+18/13*X3,
  X4>=-1/8+9/8*X3,
  X4>= -2+6*X3,
  X4>=-1/11+9/11*X3

*/

%
% taken from the reference above
%
example( 1, [X1,X2,X3,X4]) :-
  {
    12*X1 +   X2 -  3*X3 +    X4 =< 1,
   -36*X1 - 2*X2 + 18*X3 - 11*X4 =< -2,
   -18*X1 -   X2 +  9*X3 -  7*X4 =< -1,
    45*X1 + 4*X2 - 18*X3 + 13*X4 =< 4,
       X1			 >= 0,
	      X2		 >= 0
  }.

example( 2, [X0,X1,X2,X3,X4]) :-
  {								% supremum for the row
       +87*X0  +52*X1  +27*X2  -54*X3  +56*X4 =<  -93,		% r	-1108
       +33*X0  -10*X1  +61*X2  -28*X3  -29*X4 =<   63,		% r	- 595
       -68*X0	+8*X1  +35*X2  +68*X3  +35*X4 =<  -85,		%	-  85
       +90*X0  +60*X1  -76*X2  -53*X3  +24*X4 =<  -68,		%	-  68
       -95*X0  -10*X1  +64*X2  +76*X3  -24*X4 =<   33,		%	   33
       +43*X0  -22*X1  +67*X2  -68*X3  -92*X4 =<  -97,		% r	- 709
       +39*X0	+7*X1  +62*X2  +54*X3  -26*X4 =<  -27,		%	-  27
       +48*X0  -13*X1	+7*X2  -61*X3  -59*X4 =<   -2,		% r	- 211
       +49*X0  -23*X1  -31*X2  -76*X3  +27*X4 =<    3,		%	    3
       -50*X0  +58*X1	-1*X2  +57*X3  +20*X4 =<    6,		%	    6
       -13*X0  -63*X1  +81*X2	-3*X3  +70*X4 =<   64,		% r	- 385
       +20*X0  +67*X1  -23*X2  -41*X3  -66*X4 =<   52,		% r	- 489
       -81*X0  -44*X1  +19*X2  -22*X3  -73*X4 =<  -17,		%	-  17
       -43*X0	-9*X1  +14*X2  +27*X3  +40*X4 =<   39,		%	   39
       +16*X0  +83*X1  +89*X2  +25*X3  +55*X4 =<   36,		% r	-1474
	+2*X0  +40*X1  +65*X2  +59*X3  -32*X4 =<   13,		% r	- 378
       -65*X0  -11*X1  +10*X2  -13*X3  +91*X4 =<   49,		% r	- 366
       +93*X0  -73*X1  +91*X2	-1*X3  +23*X4 =<  -87		%	-  87
  }.

example( 3, [X1,X2,X3,X4]) :-
  {
    12*X1 +   X2 -  3*X3 +    X4 =< 1,
   -36*X1 - 2*X2 + 18*X3 - 11*X4 =< -2,
   -18*X1 -   X2 +  9*X3 -  7*X4 =< -1,
    45*X1 + 4*X2 - 18*X3 + 13*X4 =< 4,
       X1			  > 0,			% strict !!!
	      X2		 >= 0
  }.




/*

 The next example deals with the computation of the convex hull of
 a given set of points. The problem can be formulated as a quantifier
 elimination problem (for details please refer to the reference).

 Example:

   The set of three points in a 3 dimensional space is represented as

   [ [1,0,0], [0,1,0], [0,0,1] ]

   The following query computes the convex hull.

   [Clp(Q)] ?- conv_hull( [ [1,0,0],[0,1,0],[0,0,1] ], [X1,X2,X3] ).

   X3 = 1-X1-X2,
   X1=<1-X2,
   X2>=0,
   X1>=0

   In two dimensions it is easier to visualize and verify the
   result:

       |
     2 -    *	 *
       |
       |
     1 -    *
       |
       |
     0 -----|----*----*----
	    1	 2    3

  [Clp(Q)] ?- conv_hull([ [1,1], [2,0], [3,0], [1,2], [2,2] ], [X,Y]).

  X=<3-1/2*Y,
  Y=<2,
  Y>=0,
  X>=1,
  X>=2-Y

*/

hull( [X,Y]) :- conv_hull([ [1,1], [2,0], [3,0], [1,2], [2,2] ], [X,Y]).

conv_hull( Points, Xs) :-
  lin_comb( Points, Lambdas, Zero, Xs),
  zero( Zero),
  polytope( Lambdas).

polytope( Xs) :-
  positive_sum( Xs, 1).

  positive_sum( [],	Z) :- {Z=0}.
  positive_sum( [X|Xs], SumX) :-
    { X >= 0, SumX = X+Sum },
    positive_sum( Xs, Sum).

zero( []).
zero( [Z|Zs]) :- {Z=0}, zero( Zs).

lin_comb( [],	     [],     S1, S1).
lin_comb( [Ps|Rest], [K|Ks], S1, S3) :-
  lin_comb_r( Ps, K, S1, S2),
  lin_comb( Rest, Ks, S2, S3).

  lin_comb_r( [],     _, [],	 []).
  lin_comb_r( [P|Ps], K, [S|Ss], [Kps|Ss1]) :-
    { Kps = K*P+S },
    lin_comb_r( Ps, K, Ss, Ss1).

% -------------------------------- junkyard --------------------------------

/*
subseq0(List, List).
subseq0(List, Rest) :-
	subseq1(List, Rest).

subseq1([_Head|Tail], Rest) :-
	subseq0(Tail, Rest).
subseq1([ Head|Tail], [Head|Rest]) :-
	subseq1(Tail, Rest).

subseq0([A,B,C,D,E],S),portray_clause((proj(2,S):-example(2,[A,B,C,D,E]))),fail.
*/

proj( N) :-
  time( call_residue( proj(N,P), R)),
  numbervars(P,0,_),
  length( R, Rl),
  print(P:Rl),nl,presi(R),nl,
  fail.

presi( []).
presi( [_-R|Rs]) :-
  print( R), nl,
  presi( Rs).

proj(1, [A,B,C,D]) :-
	example(1, [A,B,C,D]).
proj(1, [A,B,C]) :-
	example(1, [_,A,B,C]).
proj(1, [A,B]) :-
	example(1, [_,_,A,B]).
proj(1, [A]) :-
	example(1, [_,_,_,A]).
proj(1, []) :-
	example(1, [_,_,_,_]).
proj(1, [A]) :-
	example(1, [_,_,A,_]).
proj(1, [A,B]) :-
	example(1, [_,A,_,B]).
proj(1, [A]) :-
	example(1, [_,A,_,_]).
proj(1, [A,B]) :-
	example(1, [_,A,B,_]).
proj(1, [A,B,C]) :-
	example(1, [A,_,B,C]).
proj(1, [A,B]) :-
	example(1, [A,_,_,B]).
proj(1, [A]) :-
	example(1, [A,_,_,_]).
proj(1, [A,B]) :-
	example(1, [A,_,B,_]).
proj(1, [A,B,C]) :-
	example(1, [A,B,_,C]).
proj(1, [A,B]) :-
	example(1, [A,B,_,_]).
proj(1, [A,B,C]) :-
	example(1, [A,B,C,_]).

proj(2, [A,B,C,D,E]) :-
	example(2, [A,B,C,D,E]).
proj(2, [A,B,C,D]) :-
	example(2, [_,A,B,C,D]).
proj(2, [A,B,C]) :-
	example(2, [_,_,A,B,C]).
proj(2, [A,B]) :-
	example(2, [_,_,_,A,B]).
proj(2, [A]) :-
	example(2, [_,_,_,_,A]).
proj(2, []) :-
	example(2, [_,_,_,_,_]).
proj(2, [A]) :-
	example(2, [_,_,_,A,_]).
proj(2, [A,B]) :-
	example(2, [_,_,A,_,B]).
proj(2, [A]) :-
	example(2, [_,_,A,_,_]).
proj(2, [A,B]) :-
	example(2, [_,_,A,B,_]).
proj(2, [A,B,C]) :-
	example(2, [_,A,_,B,C]).
proj(2, [A,B]) :-
	example(2, [_,A,_,_,B]).
proj(2, [A]) :-
	example(2, [_,A,_,_,_]).
proj(2, [A,B]) :-
	example(2, [_,A,_,B,_]).
proj(2, [A,B,C]) :-
	example(2, [_,A,B,_,C]).
proj(2, [A,B]) :-
	example(2, [_,A,B,_,_]).
proj(2, [A,B,C]) :-
	example(2, [_,A,B,C,_]).
proj(2, [A,B,C,D]) :-
	example(2, [A,_,B,C,D]).
proj(2, [A,B,C]) :-
	example(2, [A,_,_,B,C]).
proj(2, [A,B]) :-
	example(2, [A,_,_,_,B]).
proj(2, [A]) :-
	example(2, [A,_,_,_,_]).
proj(2, [A,B]) :-
	example(2, [A,_,_,B,_]).
proj(2, [A,B,C]) :-
	example(2, [A,_,B,_,C]).
proj(2, [A,B]) :-
	example(2, [A,_,B,_,_]).
proj(2, [A,B,C]) :-
	example(2, [A,_,B,C,_]).
proj(2, [A,B,C,D]) :-
	example(2, [A,B,_,C,D]).
proj(2, [A,B,C]) :-
	example(2, [A,B,_,_,C]).
proj(2, [A,B]) :-
	example(2, [A,B,_,_,_]).
proj(2, [A,B,C]) :-
	example(2, [A,B,_,C,_]).
proj(2, [A,B,C,D]) :-
	example(2, [A,B,C,_,D]).
proj(2, [A,B,C]) :-
	example(2, [A,B,C,_,_]).
proj(2, [A,B,C,D]) :-
	example(2, [A,B,C,D,_]).

proj(3, [A,B,C,D]) :-
	example(3, [A,B,C,D]).
proj(3, [A,B,C]) :-
	example(3, [_,A,B,C]).
proj(3, [A,B]) :-
	example(3, [_,_,A,B]).
proj(3, [A]) :-
	example(3, [_,_,_,A]).
proj(3, []) :-
	example(3, [_,_,_,_]).
proj(3, [A]) :-
	example(3, [_,_,A,_]).
proj(3, [A,B]) :-
	example(3, [_,A,_,B]).
proj(3, [A]) :-
	example(3, [_,A,_,_]).
proj(3, [A,B]) :-
	example(3, [_,A,B,_]).
proj(3, [A,B,C]) :-
	example(3, [A,_,B,C]).
proj(3, [A,B]) :-
	example(3, [A,_,_,B]).
proj(3, [A]) :-
	example(3, [A,_,_,_]).
proj(3, [A,B]) :-
	example(3, [A,_,B,_]).
proj(3, [A,B,C]) :-
	example(3, [A,B,_,C]).
proj(3, [A,B]) :-
	example(3, [A,B,_,_]).
proj(3, [A,B,C]) :-
	example(3, [A,B,C,_]).
