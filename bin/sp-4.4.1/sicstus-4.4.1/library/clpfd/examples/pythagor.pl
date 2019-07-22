/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Pythagoras
 * Author    : Mats Carlsson
 */

:- module(pythagoras, [pythagoras/1]).
:- use_module(library(clpfd)).

pythagoras(Lab) :-
	L = [A,B,C],
	domain(L, 1, 1000),
	AA #= A*A,
	BB #= B*B,
	CC #= C*C,
	AA + BB #= CC,
	A #=< B,
	B #=< C,
	2*BB #>= CC,				% redundant
	labeling(Lab, L),
	writeq(L),
	nl.
