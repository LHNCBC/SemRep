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
% Calculate compound interest.
% Arguments are Principal, Time (months), Interest (fraction), Balance,
%  and Monthly Payment.
%
% The goals     ?- go1    demonstrates
% obtaining ground answers for two different combinations of inputs. 
% The goals     ?- go2      and     ?- go3      deal with getting linear 
% equations as answers, while       ?- go4      produces a 
% polynomial equation.
%

mg(P,T,I,B,MP):-
	{T > 0},
	{T <= 1},
	{B + MP = P * (1 + I)}.
mg(P,T,I,B,MP):-
	{T > 1},
	mg(P * (1 + I) - MP, T - 1, I, B, MP).

go1:- mg(120000,120,0.01,0,MP), printf("Forward: MP = %g\n",[MP]),
      mg(P,120,0.01,0,MP),      printf("Backward: P = %g\n",[P]).

% Answer:
%  Forward: MP = 1721.651381
%  Backward: P = 120000

go2(P,MP) :- mg(P,120,0.01,0,MP), ordering([P,MP]).

% Answer:
%  2 ?- go2.
%  P = 69.700522*MP

go3(P,B,MP) :- mg(P,120,0.01,B,MP), ordering([P,B,MP]).

% Answer:
%  3 ?- go3.
%  P = 0.302995*B + 69.700522*MP

go4(Int) :- mg(999, 3, Int, 0, 400).

% Answer:
%  400 = (-400 + (599 + 999*Int) * (1 + Int)) * (1 + Int)

?- printf("\n>>> Sample goals: go1, go2(P,MP), go3(P,B,MP), go4(Int)\n", []).
