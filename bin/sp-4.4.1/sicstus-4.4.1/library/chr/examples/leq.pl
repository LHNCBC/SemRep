% simple constraint solver for inequalities between variables
% thom fruehwirth ECRC 950519, LMU 980207, 980311

:- use_module( library(chr)).

% handler leq.

:- chr_constraint leq/2.
% X leq Y means variable X is less-or-equal to variable Y

:- op(500, xfx, leq).

reflexivity  @ X leq X <=> true.
antisymmetry @ X leq Y , Y leq X <=> X=Y.
idempotence  @ X leq Y \ X leq Y <=> true.
transitivity @ X leq Y , Y leq Z ==> X leq Z.

/*
% more efficient, less propagating version using pragma passive
reflexivity  @ X leq X <=> true.
antisymmetry @ X leq Y , Y leq X # Id <=> X=Y pragma passive(Id).
idempotence  @ X leq Y # Id \ X leq Y <=> true pragma passive(Id).
transitivity @ X leq Y # Id , Y leq Z ==> X leq Z pragma passive(Id).
*/

% this generates a circular leq-relation chain with N variables

test(N, Time):-
	cputime(X),
	length(L,N),
	genleq(L,Last),
	L=[First|_],
	Last leq First,
	cputime( Now),
	Time is Now-X.

  genleq([Last],Last).
  genleq([X,Y|Xs],Last):-
	X leq Y,
	genleq([Y|Xs],Last).

cputime( Ts) :- 
	statistics( runtime, [Tm,_]),
	Ts is Tm/1000.


% eof handler leq -----------------------------------------------
