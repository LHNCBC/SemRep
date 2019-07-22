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
% This program finds a root of a transcendental function given an initial
% guess. It uses the Steffensen algorithm.
% The goal         ?- go          finds a root of
%
%      2
% 1 - X
%
% given X = 1.7 as an initial guess, requiring that the solution give a value
% no more than 0.0000005 from zero.

getnext(X,NX):-
	{NX = X - (F / G)},
	eval(X,F),
	getg(X,G).

getg(X,G):-
	{G = (FF - F)/F},
	eval(X,F),
	eval(X+F,FF).
	
eval(X, Y) :-
	{Y = 1 - X * X}.

small(X,E):-
	{X < 0},
	{(0-X) < E }.
small(X,E):-
	{X >= 0},
	{X < E }.

solve(I,E):-
	eval(I,F),
	small(F,E),
	printf("%10.8f\n",[I]).
solve(I,E):-
	printf("%10.8f\n",[I]),
	getnext(I,X),
	solve(X,E).

go:-  solve(1.7,0.0000005).

% Output:
%  1.70000000
%  0.44834437
%  0.91953538
%  0.99701354
%  0.99999555
%  1.00000000

?- printf("\n>>> Sample goal: go/0\n", []).
