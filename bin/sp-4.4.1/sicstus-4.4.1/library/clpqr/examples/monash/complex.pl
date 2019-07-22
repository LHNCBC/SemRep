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
% The rule below describes the relationship between two complex numbers 
% and their product. The goal        ?- go         uses the rule
% first to multiply two complex numbers, and then to divide one by another,
% in two different ways.
%

zmul(c(R1,I1),c(R2,I2),c(R3,I3)) :-
	{R3 = R1 * R2 - I1 * I2 },
	{I3 = R1 * I2 + R2 * I1 }.

go :- 	zmul(c(1,1),c(2,2),Z),
	zmul(c(1,1),Y,c(0,4)),
	zmul(X,c(2,2),c(0,4)),
	nl,
	write(' X = '),
	write(X),
	nl,
	write(' Y = '),
	write(Y),
	nl,
	write(' Z = '),
	write(Z),
	nl.

% Answer:
%   X = c(1, 1)
%   Y = c(2, 2)
%   Z = c(0, 4)

?- printf("\n>>> Sample goal: go/0\n", []).
