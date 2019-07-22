% Solve the ternary Steiner problem of order n=3, 7, 9, 13, 15, ...
% Find n(n-1)/6 triples of integers in 1..n such that every two triples
% share at most one element.

% ?- steiner(dual ,_    ,33). needs   5396 backtracks

% ?- steiner(byrow,[min],37). needs 305622 backtracks

% ?- steiner(byrow,[min],39). needs  87204 backtracks
% ?- steiner(dual ,_    ,39). needs 270336 backtracks

:- module(steiner, [steiner/4]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

steiner(dual, _Opt, N, Consistency) :-
	problem(N, Triples, Consistency),
	append(Triples, Vars),
	dual_labeling(0, N, Vars),
	format_steiner(Triples, N).
steiner(byrow, Opt, N, Consistency) :-
	problem(N, Triples, Consistency),
	append(Triples, Vars),
	labeling(Opt, Vars),
	format_steiner(Triples, N).
steiner(bycol, Opt, N, Consistency) :-
	problem(N, Triples, Consistency),
	transpose(Triples, Transpose),
	append(Transpose, Vars),
	labeling(Opt, Vars),
	format_steiner(Triples, N).


format_steiner(Triples, N) :-
	format('Steiner instance of order ~d:\n', [N]),
	(   foreach(T,Triples)
	do  format('~t~d~+~t~d~+~t~d~+\n', T)
	).

dual_labeling(N, N, _) :- !.
dual_labeling(N1, N3, Vars) :-
	N2 is N1+1,
	split_by_min(Vars, N2, Cands, Rest, Rest2),
	M is N3>>1,
	dual_choose(0, M, N2, Cands, Rest2),
	dual_labeling(N2, N3, Rest).

split_by_min([], _, []) --> [].
split_by_min([X|L1], N, L2) --> [X],
	{fd_min(X, Xmin)},
	{Xmin=\=N}, !,
	split_by_min(L1, N, L2).
split_by_min([X|L1], N, [X|L2]) -->
	split_by_min(L1, N, L2).
	
dual_choose(M, M, _, Cands, Cands) :- !.
dual_choose(I, M, Val, [Val|Cands], Rest) :-
	J is I+1,
	dual_choose(J, M, Val, Cands, Rest).
dual_choose(I, M, Val, [X|Cands], [X|Rest]) :-
	X #\= Val,
	dual_choose(I, M, Val, Cands, Rest).



problem(N, Triples, Consistency) :-
	M is N mod 6,
	(M=:=1 ; M=:=3), !,
	NTrip is N*(N-1)//6,
	length(Triples, NTrip),
	(   foreach([A,B,C],Triples),
	    foreach([A,B],Tuples),
	    param(N)
	do  domain([A,B,C], 1, N),
	    A #< B, B #< C
	),
	lex_chain(Tuples,[increasing]),
	% binary_constraints(Triples),
	pair_constraints(Triples, N, Consistency),
	card_constraint(Triples, N).

% binary_constraints([]) :- !.
% binary_constraints([T|Ts]) :-
% 	binary_constraints(Ts, T),
% 	binary_constraints(Ts).

% binary_constraints([], _).
% binary_constraints([T2|Ts], T1) :-
% 	binary_constraint(T1, T2),
% 	binary_constraints(Ts, T1).

% % N.B. The first triple is lex_lt the second one.
% binary_constraint([A,B,C], [D,E,F]) :-
% 	A #= D #<=> AD,
% 	B #= E #<=> BE,
% 	C #= F #<=> CF,
% 	B #= D #<=> BD,
% 	C #= E #<=> CE,
% 	at_most_one(AD,BE,CF,BD,CE).

% at_most_one(A,B,C,D,E) +: A+B+C+D+E #=< 1.

% pair_constraints(Triples, N) :-
% 	binomial_table(0, 0, N, Table, []),
% 	triples_pairs(Triples, Tuples, Codes),
% 	table(Tuples, Table, [consistency(bound)]),
% 	all_distinct(Codes, [consistency(bound),on(minmax)]).

% binomial_table(N, _, N) --> !.
% binomial_table(A, N, N) --> !,
% 	{A1 is A+1},
% 	binomial_table(A1, 0, N).
% binomial_table(A, B, N) --> [[A1,B1,Prod]],
% 	{A1 is A+1},
% 	{B1 is B+1},
% 	{Prod is N*A+B},
% 	binomial_table(A, B1, N).

% triples_pairs([], [], []).
% triples_pairs([[A,B,C]|Ts], [[A,B,AB],[A,C,AC],[B,C,BC]|Tuples], [AB,AC,BC|Pairs]) :-
% 	triples_pairs(Ts, Tuples, Pairs).
	
pair_constraints(Triples, N, Consistency) :-
	(   foreach([A,B,C],Triples),
	    fromto(Codes,[AB,AC,BC|S],S,[]),
	    param([N,Consistency])
	do  scalar_product([N,1], [A,B], #=, AB, [consistency(Consistency)]),
	    scalar_product([N,1], [A,C], #=, AC, [consistency(Consistency)]),
	    scalar_product([N,1], [B,C], #=, BC, [consistency(Consistency)])
	),
	all_distinct(Codes, [consistency(Consistency)]).

card_constraint(Triples, N) :-
	M is N>>1,
	% seed_triples(M, Triples, Aux, []),
	% all_distinct(Aux),
	% M1 is M-1,
	% Triples = [_|Tail],
	% leap_triples(Tail, M1),
	(   for(J,1,N),
	    foreach(J-M,Cs),
	    param(M)
	do  true
	),
	append(Triples, Vars),
	global_cardinality(Vars, Cs).

:- if(false).

% doesn't seem to help
seed_triples(0, _) --> !.
seed_triples(N, [[1,B,C]|Triples]) --> [B,C],
	{M is N-1},
	seed_triples(M, Triples).

% doesn't seem to help
leap_triples([[X|_]|L2], N) :-
	nth1(N, L2, [Y|_]), !,
	X #< Y,
	leap_triples(L2, N).
leap_triples(_, _).

:- endif.

