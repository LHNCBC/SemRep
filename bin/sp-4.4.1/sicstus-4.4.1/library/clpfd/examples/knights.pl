/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Knights Tour
 * Author    : Mats Carlsson
 *
 * Find a Hamiltonian walk of a knight on an NxN board.
 */

:- module(knights, [knights/1]).

:- use_module(library(lists), [nth1/3]).
:- use_module(library(clpfd)).

knights(N) :-
	NN is N*N,
	problem(N, Succ, Jump),
	labeling([], Succ), !,
	nth1(1, Jump, 1),
	fill_jump(1, NN, Succ, 1, Jump),
	fmt(N, Fmt, []),
	(   fromto(Jump,S0,S,[]),
	    param([N,Fmt])
	do  (   for(_,1,N),
		fromto(S0,[X|S1],S1,S),
		fromto(Row,[X|R],R,[])
	    do  true
	    ),
	    format(Fmt, Row)
	).

problem(N, Succ, Jump) :-
	NN is N*N,
	length(Succ, NN),
	length(Jump, NN),
	domain(Succ, 1, NN),
	N3 is N+3,
	nth1(1, Succ, N3),	% seed
	circuit(Succ),
	neighbors(0, NN, Succ, N).

neighbors(NN, NN, _, _) :- !.
neighbors(F, NN, Succ, N) :-
	F1 is F+1,
	neighbor_pos(F1, N, Gs, []),
	list_to_fdset(Gs, Gset),	
	nth1(F1, Succ, Sf),
	Sf in_set Gset,
	neighbors(F1, NN, Succ, N).

neighbor_pos(N0, N) -->
	{I is (N0-1) //  N,
	 J is (N0-1) mod N,
	 N1 is N0 - 2*N - 1,
	 N2 is N0 - 2*N + 1,
	 N3 is N0 -   N - 2,
	 N4 is N0 -   N + 2,
	 N5 is N0 +   N - 2,
	 N6 is N0 +   N + 2,
	 N7 is N0 + 2*N - 1,
	 N8 is N0 + 2*N + 1},
	({I>=2,   J>=1}   -> [N1] ; []),
	({I>=2,   J=<N-2} -> [N2] ; []),
	({I>=1,   J>=2}   -> [N3] ; []),
	({I>=1,   J=<N-3} -> [N4] ; []),
	({I=<N-2, J>=2}   -> [N5] ; []),
	({I=<N-2, J=<N-3} -> [N6] ; []),
	({I=<N-3, J>=1}   -> [N7] ; []),
	({I=<N-3, J=<N-2} -> [N8] ; []).

fill_jump(NN, NN, _, _, _) :- !.
fill_jump(N1, NN, Succ, Cur, Jump) :-
	N2 is N1+1,
	nth1(Cur, Succ, Next),
	nth1(Next, Jump, N2),
	fill_jump(N2, NN, Succ, Next, Jump).

fmt(0) --> !, "\n\n".
fmt(I) --> "~t~d~+",
	{J is I-1},
	fmt(J).

