:- module(hamming, [hamming/3]).

% ?- bench([hamming(10,3,64),hamming(10,5,9)]).

:- use_module(library(clpfd)).
:- use_module(library(lists), [append/2,transpose/2]).

%%% Find a binary Hamming code (N,D) with K elements.
%%% K = #words
%%% N = word size
%%% D: sum of pairwise difference >= D

hamming(N, D, K) :-
	length(Words, K),
	(   foreach(Word1,Words),
	    param(N)
	do  length(Word1, N),
	    domain(Word1, 0, 1)
	),
	Words = [Head|_],
	domain(Head, 0, 0),
	lex_chain(Words, [op(#<)]),
	transpose(Words, Columns),
	lex_chain(Columns),
	(   fromto(Words,[Word2|Rest],Rest,[]),
	    param(D)
	do  (   foreach(Word3,Rest),
		param([Word2,D])
	    do  (   foreach(A,Word3),
		    foreach(B,Word2),
		    foreach(C,Cs)
		do  (A #\ B) #<=> C
		),
		sum(Cs, #>=, D)
	    )
	),
	append(Words, Vars),
	labeling([], Vars), !,
	(   foreach(Row,Words)
	do  (   foreach(R,Row),
		foreach(S,String)
	    do  S is R+"0"
	    ),
	    format('~s\n', [String])
	).

