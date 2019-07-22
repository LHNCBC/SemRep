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
% Simple recursive formulation of fibonacci numbers.
% The goal      ?- go       shows how the definition can be used for 
% both finding a a fibonacci number given its index, and finding the index 
% of a given fibonacci number.
%

fib(N, F) :-
	{N = 0},
	{F = 1}.
fib(N, F) :-
	{N = 1},
	{F = 1}.
fib(N, F) :-
	{N > 1},
	{F = X1 + X2},
	{N1 = N-1},
	{N2 = N-2},
	fib(N1, X1), 
	fib(N2, X2).

go:- 	fib(10, Z), 
	printf("forward: fib(10) = %d\n",[Z]),
	fib(Y, Z),
	printf("backward: fib(%d) = %d\n",[Y,Z]).

% Answer:
%  forward: fib(10) = 89
%  backward: fib(10) = 89

?- printf("\n>>> Sample goal: go/0\n", []).
