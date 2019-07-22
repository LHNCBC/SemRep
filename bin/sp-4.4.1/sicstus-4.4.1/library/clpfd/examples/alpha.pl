/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            INRIA Rocquencourt - ChLoE Project */
/*                                                                         */
/* Name           : alpha.pl                                               */
/* Title          : alphacipher                                            */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     :                                                        */
/* Date           : January 1993                                           */
/*                                                                         */
/* This problem comes from the news group rec.puzzle.                      */
/* The numbers 1 - 26 have been randomly assigned to the letters of the    */
/* alphabet. The numbers beside each word are the total of the values      */
/* assigned to the letters in the word. e.g for LYRE L,Y,R,E might equal   */
/* 5,9,20 and 13 respectively or any other combination that add up to 47.  */
/* Find the value of each letter under the equations:                      */
/*                                                                         */
/*    BALLET  45     GLEE  66     POLKA      59     SONG     61            */
/*    CELLO   43     JAZZ  58     QUARTET    50     SOPRANO  82            */
/*    CONCERT 74     LYRE  47     SAXOPHONE 134     THEME    72            */
/*    FLUTE   30     OBOE  53     SCALE      51     VIOLIN  100            */
/*    FUGUE   50     OPERA 65     SOLO       37     WALTZ    34            */
/*                                                                         */
/* Solution:                                                               */
/*  [A, B,C, D, E,F, G, H, I, J, K,L,M, N, O, P,Q, R, S,T,U, V,W, X, Y, Z] */
/*  [5,13,9,16,20,4,24,21,25,17,23,2,8,12,10,19,7,11,15,3,1,26,6,22,14,18] */
/*-------------------------------------------------------------------------*/

:- module(alpha,[alpha/2,alpha_ix/2]).
:- use_module(library(clpfd)).

alpha(Lab, Consistency) :-
	LD=[A,B,C,_D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z],
	Opt = [consistency(Consistency)],
	domain(LD,1,26),
	all_different(LD, Opt),

	scalar_product([1,1,1,1,1,1], [B,A,L,L,E,T], #=, 45, Opt),
	scalar_product([1,1,1,1,1], [C,E,L,L,O], #=, 43, Opt),
	scalar_product([1,1,1,1,1,1,1], [C,O,N,C,E,R,T], #=, 74, Opt),
	scalar_product([1,1,1,1,1], [F,L,U,T,E], #=, 30, Opt),
	scalar_product([1,1,1,1,1], [F,U,G,U,E], #=, 50, Opt),
	scalar_product([1,1,1,1], [G,L,E,E], #=, 66, Opt),
	scalar_product([1,1,1,1], [J,A,Z,Z], #=, 58, Opt),
	scalar_product([1,1,1,1], [L,Y,R,E], #=, 47, Opt),
	scalar_product([1,1,1,1], [O,B,O,E], #=, 53, Opt),
	scalar_product([1,1,1,1,1], [O,P,E,R,A], #=, 65, Opt),
	scalar_product([1,1,1,1,1], [P,O,L,K,A], #=, 59, Opt),
	scalar_product([1,1,1,1,1,1,1], [Q,U,A,R,T,E,T], #=, 50, Opt),
	scalar_product([1,1,1,1,1,1,1,1,1], [S,A,X,O,P,H,O,N,E], #=, 134, Opt),
	scalar_product([1,1,1,1,1], [S,C,A,L,E], #=, 51, Opt),
	scalar_product([1,1,1,1], [S,O,L,O], #=, 37, Opt),
	scalar_product([1,1,1,1], [S,O,N,G], #=, 61, Opt),
	scalar_product([1,1,1,1,1,1,1], [S,O,P,R,A,N,O], #=, 82, Opt),
	scalar_product([1,1,1,1,1], [T,H,E,M,E], #=, 72, Opt),
	scalar_product([1,1,1,1,1,1], [V,I,O,L,I,N], #=, 100, Opt),
	scalar_product([1,1,1,1,1], [W,A,L,T,Z], #=, 34, Opt),
	labeling(Lab,LD),
	writeq(LD),
	nl.

alpha_ix(Lab, Consistency):-
	LD=[A,B,C,_D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z],

	domain(LD,1,26),
	all_different(LD, [consistency(Consistency)]),

	eq1(B,A,L,E,T),
	eq2(C,E,L,O),
	eq3(C,O,N,E,R,T),
	eq4(F,L,U,T,E),
	eq5(F,U,G,E),
	eq6(G,L,E),
	eq7(J,A,Z),
	eq8(L,Y,R,E),
	eq9(O,B,E),
	eq10(O,P,E,R,A),
	eq11(P,O,L,K,A),
	eq12(Q,U,A,R,T,E),
	eq13(S,A,X,O,P,H,N,E),
	eq14(S,C,A,L,E),
	eq15(S,O,L),
	eq16(S,O,N,G),
	eq17(S,O,P,R,A,N),
	eq18(T,H,E,M),
	eq19(V,I,O,L,N),
	eq20(W,A,L,T,Z),
	labeling(Lab,LD),
	writeq(LD),
	nl.

eq1(B,A,L,E,T) +:
	B+A+L+L+E+T       #= 45.

eq2(C,E,L,O) +:
	C+E+L+L+O         #= 43.

eq3(C,O,N,E,R,T) +:
	C+O+N+C+E+R+T     #= 74.

eq4(F,L,U,T,E) +:
	F+L+U+T+E         #= 30.

eq5(F,U,G,E) +:
	F+U+G+U+E         #= 50.

eq6(G,L,E) +:
	G+L+E+E           #= 66.

eq7(J,A,Z) +:
	J+A+Z+Z           #= 58.

eq8(L,Y,R,E) +:
	L+Y+R+E           #= 47.

eq9(O,B,E) +:
	O+B+O+E           #= 53.

eq10(O,P,E,R,A) +:
	O+P+E+R+A         #= 65.

eq11(P,O,L,K,A) +:
	P+O+L+K+A         #= 59.

eq12(Q,U,A,R,T,E) +:
	Q+U+A+R+T+E+T     #= 50.

eq13(S,A,X,O,P,H,N,E) +:
	S+A+X+O+P+H+O+N+E #= 134.

eq14(S,C,A,L,E) +:
	S+C+A+L+E         #= 51.

eq15(S,O,L) +:
	S+O+L+O           #= 37.

eq16(S,O,N,G) +:
	S+O+N+G           #= 61.

eq17(S,O,P,R,A,N) +:
	S+O+P+R+A+N+O     #= 82.

eq18(T,H,E,M) +:
	T+H+E+M+E         #= 72.

eq19(V,I,O,L,N) +:
	V+I+O+L+I+N       #= 100.

eq20(W,A,L,T,Z) +:
	W+A+L+T+Z         #= 34.
