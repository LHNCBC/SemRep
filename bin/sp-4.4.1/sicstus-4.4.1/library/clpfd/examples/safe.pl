/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Professor Smart's safe combination
 * Author    : Mats Carlsson
 */

:- module(safe, [safe/2]).
:- use_module(library(clpfd)).

safe(Lab, Consistency) :-
	L = [A,B,C,D,_,F,G,H,I],
	domain(L, 1, 9),
	all_distinct(L, [consistency(Consistency)]),
	(   foreach(X,L),
	    count(J,1,_)
	do  X #\= J
	),
	D-F #= G,
	A*B*C #= H+I,
	B+C+F #< H,
	I #< H,
	labeling(Lab, L),
	writeq(L),
	nl.

