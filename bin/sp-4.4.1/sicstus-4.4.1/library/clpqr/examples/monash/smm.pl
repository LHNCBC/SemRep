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
% Crypto-arithmetic puzzle: 
% Need to assign integers from 0 to 9 to the letters S, E, N, D, M, O, R, Y
% without duplicates such that the equation
%
%        S E N D
%      + M O R E
%      ---------
%      M O N E Y
%
% is satisfied.
%

p(S, E, N, D, M, O, R, Y) :-
        {S >  0, E >= 0, N >= 0, D >= 0, M > 0, O >= 0, R >= 0, Y >= 0},
        {S <= 9, E <= 9, N <= 9, D <= 9, M <= 9, O <= 9, R <= 9, Y <= 9},
        {D + E = Y + 10*C1},
        {C1 + N + R = E + 10*C2},
        {C2 + E + O = N + 10*C3},
        {C3 + S + M = O + 10*M},
        carry(C1),
        carry(C2),
        carry(C3),
        dig(S), dig(E), dig(N), dig(D), dig(M), dig(O), dig(R), dig(Y),
        printf("S = %d, E = %d, N = %d, D = %d, M = %d, O = %d, R = %d, Y = %d \n",[S, E, N, D, M, O, R, Y]),
        difflist([S, E, N, D, M, O, R, Y]).

carry(C) :-
	{C = 1}.
carry(C) :-
	{C = 0}.

dig(D) :-
	digit(E),
	{D = E}.

digit(9).
digit(8).
digit(7).
digit(6).
digit(5).
digit(4).
digit(3).
digit(2).
digit(1).
digit(0).

difflist([X | T]) :- notmem(X, T), difflist(T).
difflist([]).

notmem(X, [Y | Z]) :-
	{X < Y},
	notmem(X, Z).
notmem(X, [Y | Z]) :-
	{X > Y},
	notmem(X, Z).
notmem(X, []).

/**************************************************************/
/*** S = 9, E = 5, N = 6, D = 7, M = 1, O = 0, R = 8, Y = 2 ***/
/**************************************************************/

go:-
        p(S, E, N, D, M, O, R, Y),
	printf("\nAns:\n",[]),
        printf("S = %d, E = %d, N = %d, D = %d, M = %d, O = %d, R = %d, Y = %d \n",[S, E, N, D, M, O, R, Y]).

% Output:
%  S = 9, E = 8, N = 9, D = 9, M = 1, O = 0, R = 8, Y = 7 
%  S = 9, E = 8, N = 9, D = 8, M = 1, O = 0, R = 8, Y = 6 
%  S = 9, E = 8, N = 9, D = 7, M = 1, O = 0, R = 8, Y = 5 
%  S = 9, E = 8, N = 9, D = 6, M = 1, O = 0, R = 8, Y = 4 
%  S = 9, E = 8, N = 9, D = 5, M = 1, O = 0, R = 8, Y = 3 
%  S = 9, E = 8, N = 9, D = 4, M = 1, O = 0, R = 8, Y = 2 
%  S = 9, E = 8, N = 9, D = 3, M = 1, O = 0, R = 8, Y = 1 
%  S = 9, E = 8, N = 9, D = 2, M = 1, O = 0, R = 8, Y = 0 
%  S = 9, E = 7, N = 8, D = 9, M = 1, O = 0, R = 8, Y = 6 
%  S = 9, E = 7, N = 8, D = 8, M = 1, O = 0, R = 8, Y = 5 
%  S = 9, E = 7, N = 8, D = 7, M = 1, O = 0, R = 8, Y = 4 
%  S = 9, E = 7, N = 8, D = 6, M = 1, O = 0, R = 8, Y = 3 
%  S = 9, E = 7, N = 8, D = 5, M = 1, O = 0, R = 8, Y = 2 
%  S = 9, E = 7, N = 8, D = 4, M = 1, O = 0, R = 8, Y = 1 
%  S = 9, E = 7, N = 8, D = 3, M = 1, O = 0, R = 8, Y = 0 
%  S = 9, E = 6, N = 7, D = 9, M = 1, O = 0, R = 8, Y = 5 
%  S = 9, E = 6, N = 7, D = 8, M = 1, O = 0, R = 8, Y = 4 
%  S = 9, E = 6, N = 7, D = 7, M = 1, O = 0, R = 8, Y = 3 
%  S = 9, E = 6, N = 7, D = 6, M = 1, O = 0, R = 8, Y = 2 
%  S = 9, E = 6, N = 7, D = 5, M = 1, O = 0, R = 8, Y = 1 
%  S = 9, E = 6, N = 7, D = 4, M = 1, O = 0, R = 8, Y = 0 
%  S = 9, E = 5, N = 6, D = 9, M = 1, O = 0, R = 8, Y = 4 
%  S = 9, E = 5, N = 6, D = 8, M = 1, O = 0, R = 8, Y = 3 
%  S = 9, E = 5, N = 6, D = 7, M = 1, O = 0, R = 8, Y = 2 
%  
%  Ans:
%  S = 9, E = 5, N = 6, D = 7, M = 1, O = 0, R = 8, Y = 2 

?- printf("\n>>> Sample goal: go/0\n", []).
