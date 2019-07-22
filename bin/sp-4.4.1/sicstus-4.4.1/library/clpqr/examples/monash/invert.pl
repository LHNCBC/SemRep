:- use_module(library('clpqr/monash')).

/*
 **********************************************************************
 *
 *      CLP(R) Version 2.0	(Example Programs Release)
 *	(C) Copyright, March 1986, Monash University
 *
 **********************************************************************
 */

%
% Matrix inversion package. The goal          ?- go
% invers a matrix, prints the inverse, then multiplies the two,
% getting the identity matrix, and prints that.
%


sizemat(M,R,C):-
	length(M,R),
	allvec(M,C).

allvec([],C).
allvec([H|T],C):-
	length(H,C),
	allvec(T,C).
	
matmul([H|T],B,[H1|T1]):-
	rowmul(H,B,H1),
	matmul(T,B,T1).
matmul([],_,[]).

rowmul(AV,[H|T],[H1|T1]):-
	vecmul(AV,H,0,H1),
	rowmul(AV,T,T1).
rowmul(_,[],[]).

vecmul([H1|T1],[H2|T2],IN,OUT):-
	{TMP = IN + H1 * H2},
	vecmul(T1,T2,TMP,OUT).
vecmul([],[],S0,S) :-
	{S0 = S}.
	
rowtocol(A,[H|T]):-
	stripfirst(A,H,REST),
	rowtocol(REST,T).
rowtocol(A,[]).

stripfirst([H|T],[H1|T1],[H2|T2]):-
	stripvec(H,H1,H2),
	stripfirst(T,T1,T2).
stripfirst([],[],[]).

stripvec([H|T],H,T).

putmat([H|T]):-
	putvec(H),
	putmat(T).
putmat([]).

putvec([H|T]):-
	printf(" %9.4f ",[H]),
	putvec(T).
putvec([]):-
	nl.

go:-
	M = [ [4,5,6], [1,5,3], [1,8,9] ],
	I = [ [1,0,0], [0,1,0], [0,0,1] ],
	sizemat(N,3,3),
	sizemat(NR,3,3),
	sizemat(T,3,3),
	matmul(M,N,I),
	rowtocol(NR,N),
	matmul(M,N,T),
	printf(" NR = \n",[]),
	putmat(NR),
	printf(" T = \n",[]),
	putmat(T).
     
% Output:
%   NR = 
%      0.2917     0.0417    -0.2083 
%     -0.0833     0.4167    -0.0833 
%      0.0417    -0.3750     0.2083 
%   T = 
%      1.0000     0.0000     0.0000 
%      0.0000     1.0000     0.0000 
%      0.0000     0.0000     1.0000 

?- printf("\n>>> Sample goal: go/0\n", []).
