/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : SEND+MORE=MONEY
 * Author    : Mats Carlsson
 */

%%   SEND
%%   MORE
%%  +____
%%  MONEY

:- module(smm,[smm/2,smm_ix/2]).
:- use_module(library(clpfd)).

smm(Lab, Consistency):-
	L = [S,E,N,D,M,O,R,Y],
	domain(L, 0, 9),
	S #> 0,
	M #> 0,
	all_different(L, [consistency(Consistency)]),
	           1000*S+100*E+10*N+D
        +          1000*M+100*O+10*R+E
        #= 10000*M+1000*O+100*N+10*E+Y,
	labeling(Lab, L),
	writeq(L),
	nl.


smm_ix(Lab, Consistency):-
	L = [S,E,N,D,M,O,R,Y],
	domain(L, 0, 9),
	S #> 0,
	M #> 0,
	all_different(L, [consistency(Consistency)]),
	sendmory(S,E,N,D,M,O,R,Y),
	labeling(Lab, L),
	writeq(L),
	nl.

sendmory(S,E,N,D,M,O,R,Y) +:
	           1000*S+100*E+10*N+D
        +          1000*M+100*O+10*R+E
        #= 10000*M+1000*O+100*N+10*E+Y.
