/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            INRIA Rocquencourt - ChLoE Project */
/*                                                                         */
/* Name           : donald.pl                                              */
/* Title          : crypt-arithmetic                                       */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     :                                                        */
/* Date           : September 1992                                         */
/*                                                                         */
/* Solve the operation:                                                    */
/*                                                                         */
/*    D O N A L D                                                          */
/*  + G E R A L D                                                          */
/*  --------------                                                         */
/*  = R O B E R T                                                          */
/*                                                                         */
/* (resolution by line)                                                    */
/*                                                                         */
/* Solution:                                                               */
/*  [D,O,N,A,L,G,E,R,B,T]                                                  */
/*  [5,2,6,4,8,1,9,7,3,0]                                                  */
/*-------------------------------------------------------------------------*/

:- module(donald,[donald/2,donald_ix/2]).
:- use_module(library(clpfd)).

donald(Lab, Consistency):-
	LD=[D,O,N,A,L,G,E,R,B,T],
	Opt = [consistency(Consistency)],
	domain(LD,0,9),
	domain([D,G],1,9),
	all_different(LD, Opt),
	scalar_product([ 100000, 10000, 1000, 100, 10, 1,
			 100000, 10000, 1000, 100, 10, 1,
			-100000,-10000,-1000,-100,-10,-1],
		       [D,O,N,A,L,D,
			G,E,R,A,L,D,
			R,O,B,E,R,T], #=, 0, []/*Opt*/),
% 	   100000*D+10000*O+1000*N+100*A+10*L+D +
% 	   100000*G+10000*E+1000*R+100*A+10*L+D
% 	#= 100000*R+10000*O+1000*B+100*E+10*R+T,
	labeling(Lab,LD),
	writeq(LD),
	nl.

donald_ix(Lab, Consistency):-
	LD=[D,O,N,A,L,G,E,R,B,T],
	domain(LD,0,9),
	domain([D,G],1,9),
	all_different(LD, [consistency(Consistency)]),
	eq(D,O,N,A,L,G,E,R,B,T),
	labeling(Lab,LD),
	writeq(LD),
	nl.

eq(D,O,N,A,L,G,E,R,B,T) +:
	   100000*D+10000*O+1000*N+100*A+10*L+D +
	   100000*G+10000*E+1000*R+100*A+10*L+D
	#= 100000*R+10000*O+1000*B+100*E+10*R+T.

