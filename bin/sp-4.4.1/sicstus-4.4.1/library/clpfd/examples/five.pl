/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            INRIA Rocquencourt - ChLoE Project */
/*                                                                         */
/* Name           : five.pl                                                */
/* Title          : five house puzzle                                      */
/* Original Source: P. Van Hentenryck's book                               */
/* Adapted by     : Daniel Diaz - INRIA France                             */
/* Date           : September 1992                                         */
/*                                                                         */
/* A logic puzzle                                                          */
/*                                                                         */
/* Solution:                                                               */
/*  [N1,N2,N3,N4,N5,     [3,4,5,2,1,                                       */
/*   C1,C2,C3,C4,C5,      5,3,1,2,4,                                       */
/*   P1,P2,P3,P4,P5,      5,1,4,2,3,                                       */
/*   A1,A2,A3,A4,A5,      4,5,1,3,2,                                       */
/*   D1,D2,D3,D4,D5]      4,1,2,5,3]                                       */
/*-------------------------------------------------------------------------*/
:- module(five,[five_house/2]).
:- use_module(library(clpfd)).


five_house(Lab, Consistency):-
	L=[N1,N2,N3,N4,N5,
	   C1,C2,C3,C4,C5,
	   P1,P2,P3,P4,P5,
	   A1,A2,A3,A4,A5,
	   D1,D2,D3,D4,D5],
	domain(L, 1, 5),
	N5 #= 1,
	D5 #= 3,
	N1 #= C2,
	N2 #= A1,
	N3 #= P1,
	N4 #= D3,
	P3 #= D1,
	C1 #= D4,
	P5 #= A4,
	P2 #= C3,
	C1 #= C5+1,
	all_different([C1,C2,C3,C4,C5], [consistency(Consistency)]),
	all_different([P1,P2,P3,P4,P5], [consistency(Consistency)]),
	all_different([N1,N2,N3,N4,N5], [consistency(Consistency)]),
	all_different([A1,A2,A3,A4,A5], [consistency(Consistency)]),
	all_different([D1,D2,D3,D4,D5], [consistency(Consistency)]),
	abs(A3-P4) #= 1,
	abs(A5-P2) #= 1,
	abs(N5-C4) #= 1,
	labeling(Lab, L),
	writeq(L),
	nl.

