/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)            INRIA Rocquencourt - ChLoE Project */
/*                                                                         */
/* Name           : crypta.pl                                              */
/* Title          : crypt-arithmetic                                       */
/* Original Source: P. Van Hentenryck's book                               */
/* Adapted by     : Daniel Diaz - INRIA France                             */
/* Date           : September 1992                                         */
/*                                                                         */
/* Solve the operation:                                                    */
/*                                                                         */
/*    B A I J J A J I I A H F C F E B B J E A                              */
/*  + D H F G A B C D I D B I F F A G F E J E                              */
/*  -----------------------------------------                              */
/*  = G J E G A C D D H F A F J B F I H E E F                              */
/*                                                                         */
/* Solution:                                                               */
/*  [A,B,C,D,E,F,G,H,I,J]                                                  */
/*  [1,2,3,4,5,6,7,8,9,0]                                                  */
/*-------------------------------------------------------------------------*/

:- module(crypta,[crypta/2,crypta_ix/2]).
:- use_module(library(clpfd)).

crypta(Lab, Consistency):-
	Opt = [consistency(Consistency)],
	LD=[A,B,C,D,E,F,G,H,I,J],
	domain(LD,0,9),
	domain([Sr1,Sr2],0,1),
	domain([B,D,G],1,9),
	all_different(LD, Opt),
	scalar_product([ 1, 10, 100, 1000, 10000, 100000, 1000000,
			 1, 10, 100, 1000, 10000, 100000, 1000000,
			-1,-10,-100,-1000,-10000,-100000,-1000000,-10000000],
		       [A,E,J,B,B,E,F,
			E,J,E,F,G,A,F,
			F,E,E,H,I,F,B,Sr1], #=, 0, []/*Opt*/),
	scalar_product([ 1, 10, 100, 1000, 10000, 100000, 1000000,
			 1, 10, 100, 1000, 10000, 100000, 1000000, 1,
			-1,-10,-100,-1000,-10000,-100000,-1000000,-10000000],
		       [C,F,H,A,I,I,J,
			F,I,B,D,I,D,C,Sr1,
			J,F,A,F,H,D,D,Sr2], #=, 0, []/*Opt*/),
	scalar_product([ 1, 10, 100, 1000, 10000, 100000,
			 1, 10, 100, 1000, 10000, 100000, 1,
			-1,-10,-100,-1000,-10000,-100000],
		       [A,J,J,I,A,B,  
			B,A,G,F,H,D,Sr2,
			C,A,G,E,J,G      ], #=, 0, []/*Opt*/),

% 	   A+10*E+100*J+1000*B+10000*B+100000*E+1000000*F+
% 	   E+10*J+100*E+1000*F+10000*G+100000*A+1000000*F
% 	#= F+10*E+100*E+1000*H+10000*I+100000*F+1000000*B+10000000*Sr1,
      
      
% 	   C+10*F+100*H+1000*A+10000*I+100000*I+1000000*J+
% 	   F+10*I+100*B+1000*D+10000*I+100000*D+1000000*C+Sr1
% 	#= J+10*F+100*A+1000*F+10000*H+100000*D+1000000*D+10000000*Sr2,
      
% 	   A+10*J+100*J+1000*I+10000*A+100000*B+
% 	   B+10*A+100*G+1000*F+10000*H+100000*D+Sr2
% 	#= C+10*A+100*G+1000*E+10000*J+100000*G,
	labeling(Lab,LD),
	writeq(LD),
	nl.

crypta_ix(Lab, Consistency):-
	Opt = [consistency(Consistency)],
	LD=[A,B,C,D,E,F,G,H,I,J],
	domain(LD,0,9),
	domain([Sr1,Sr2],0,1),
	domain([B,D,G],1,9),
	all_different(LD, Opt),

	eq1(A,B,E,F,G,H,I,J,Sr1),
	eq2(A,B,C,D,F,H,I,J,Sr1,Sr2),
	eq3(A,B,C,D,E,F,G,H,I,J,Sr2),
	labeling(Lab,LD),
	writeq(LD),
	nl.

eq1(A,B,E,F,G,H,I,J,Sr1) +:
	   A+10*E+100*J+1000*B+10000*B+100000*E+1000000*F+
	   E+10*J+100*E+1000*F+10000*G+100000*A+1000000*F
	#= F+10*E+100*E+1000*H+10000*I+100000*F+1000000*B+10000000*Sr1.
      
      
eq2(A,B,C,D,F,H,I,J,Sr1,Sr2) +:
	   C+10*F+100*H+1000*A+10000*I+100000*I+1000000*J+
	   F+10*I+100*B+1000*D+10000*I+100000*D+1000000*C+Sr1
	#= J+10*F+100*A+1000*F+10000*H+100000*D+1000000*D+10000000*Sr2.
      
eq3(A,B,C,D,E,F,G,H,I,J,Sr2) +:
	   A+10*J+100*J+1000*I+10000*A+100000*B+
	   B+10*A+100*G+1000*F+10000*H+100000*D+Sr2
	#= C+10*A+100*G+1000*E+10000*J+100000*G.


