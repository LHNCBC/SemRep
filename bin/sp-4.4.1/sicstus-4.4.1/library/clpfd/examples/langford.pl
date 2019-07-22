% % Langford's Problem  (CSPlib problem 24)
% %
% % June 2006; Sebastian Brand
% %
% % Instance L(k,n):
% % Arrange k sets of numbers 1 to n so that each appearance of the number m is m
% % numbers on from the last.  For example, the L(3,9) problem is to arrange 3
% % sets of the numbers 1 to 9 so that the first two 1's and the second two 1's
% % appear one number apart, the first two 2's and the second two 2's appear two
% % numbers apart, etc.
% %-----------------------------------------------------------------------------%
% % MiniZinc version
% % Peter Stuckey September 30

% include "globals.mzn";

% %-----------------------------------------------------------------------------%
% % Instance
% %-----------------------------------------------------------------------------%

% int: n = 9;
% int: k = 3;

% %-----------------------------------------------------------------------------%
% % Input
% %-----------------------------------------------------------------------------%

% set of int: numbers = 1..n;             % numbers
% set of int: sets    = 1..k;             % sets of numbers
% set of int: num_set = 1..n*k;

% set of int: positions = 1..n*k;         % positions of (number, set) pairs

% %-----------------------------------------------------------------------------%
% % Primal model
% %-----------------------------------------------------------------------------%

% array[num_set] of var positions: Pos;
% 					% Pos[ns]: position of (number, set)
%                                         % pair in the sought sequence
% constraint
%         forall(i in 1..n, j in 1..k-1) (
%             Pos[k*(i-1) + j+1] - Pos[k*(i-1) + j] = i+1
%         );

% constraint
%         alldifferent(Pos);

% %-----------------------------------------------------------------------------%
% % Dual model (partial)
% %-----------------------------------------------------------------------------%

% array[positions] of var num_set: Num;   % Num[p]: (number, set) pair at
%                                         % position p in the sought sequence
% constraint
%         alldifferent(Num);

% %-----------------------------------------------------------------------------%
% % Channelling between primal model and dual model
% %-----------------------------------------------------------------------------%

% constraint
%         forall(i in numbers, j in sets, p in positions) (
%                 (Pos[k*(i-1) + j] = p) <-> (Num[p] = k*(i-1) + j)
%         );

% %-----------------------------------------------------------------------------%

% 	% Without specifying a sensible search order this problem takes
% 	% forever to solve.
% 	%
% solve	:: int_search(Pos, first_fail, indomain_split, complete)
% 	satisfy;

% output
% 	[ if j = 1 then "\n" ++ show(i) ++ "s at " else ", " endif ++
% 	  show(Pos[k*(i-1) + j])
% 	| i in 1..n, j in 1..k
% 	] ++
% 	[ "\n" ];

:- module(langford, [langford/2]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

langford(K, N) :-
	NK is N*K,
	length(Pos, NK),
	domain(Pos, 1, NK),	% Pos[ns]: position of (number, set)
				% pair in the sought sequence
	(   for(I,1,N),
	    param(K,Pos)
	do  (   for(J,1,K-1),
		param(I,K,Pos)
	    do  Ix1 is K*(I-1) + J+1,
		Ix2 is Ix1-1,
		I1 is I+1,
		nth1(Ix1, Pos, Pos1),
		nth1(Ix2, Pos, Pos2),
		Pos1 - Pos2 #= I1
	    )
	),
	length(Num, NK),
	domain(Num, 1, NK),	% Num[p]: (number, set) pair at
				% position p in the sought sequence
	assignment(Pos, Num),
	/* totally redundant:
	(   for(_,1,NK),
	    foreach(NumjEq,NumEq),
	    param(NK)
	do  length(NumjEq, NK)
	),
	transpose(NumEq, NumEqT),
	(   foreach(Numi,Num),
	    foreach(NumiEq,NumEq),
	    foreach(Posi,Pos),
	    foreach(PosiEq,NumEqT)
	do  bool_channel(NumiEq, Numi, #=, 1),
	    bool_channel(PosiEq, Posi, #=, 1)
	),
	*/
	labeling([min,bisect], Num), % by trial and error
	format('~w\n~w\n', [Pos,Num]),
	(   for(I3,1,N),
	    fromto(Pos,Posa,Posb,[]),
	    param(K)
	do  length(Prefix, K),
	    append(Prefix, Posb, Posa),
	    format('Position of "~d": ~w\n', [I3,Prefix])
	).
