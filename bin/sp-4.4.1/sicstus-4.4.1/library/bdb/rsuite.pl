%% [PM] 4.1.3 obsolete code
end_of_file.

:- use_module(newdb), use_module(library(lists)).

p :-
	do('/tmp/env', '/tmp/db').

pwin :-
	do('c:\\tmp\\env', 'c:\\tmp\\db').

do(FE, FDB) :-
	newdb:zerostat,
	db_open_env(FE, EnvRef),
	db_open(EnvRef, FDB, read, _, DBRef),
	N = 26,
	time(generate_term, generate_term(N)),
	time(count, count(N, DBRef)),
	time(enumerate, enumerate(DBRef)),
	time(fetch, fetch(DBRef)),
	time(fetch_all, fetch_all(N, DBRef)),
	time(findall, findall(DBRef)),
	db_close(DBRef),
	db_close_env(EnvRef), nl,
	newdb:printstat.

time(Name, Goal) :-
	statistics(runtime, [T0,_]),
	call(Goal),
	statistics(runtime, [T1,_]),
	T is (T1-T0)/1000,
	format('~w: ~w sec.~n', [Name,T]).

generate_term(N) :-
	generate_term(N, _),
	fail.
generate_term(_).

count(N, DBRef) :-
	db_make_iterator(DBRef, Iterator),
	count_loop(Iterator, 0, Count),
	db_iterator_done(Iterator),
	format('# of elements: ~d: ~w~n', [Count]).

count_loop(Iterator, N0, N) :-
	db_iterator_next(Iterator, _, _), !,
	N1 is N0+1,
	count_loop(Iterator, N1, N).
count_loop(_, N, N).

enumerate(DBRef) :-
	db_enumerate(DBRef, _, _),
	fail.
enumerate(_).

fetch(DBRef) :-
	db_fetch(DBRef, g(7,19,20,_), _),
	db_fetch(DBRef, f(17,_,x), _),
	fail.
fetch(_).

fetch_all(N, DBRef) :-
	generate_term(N, Term),
	db_fetch(DBRef, Term, _),
	fail.
fetch_all(_, _).

findall(DBRef) :-
	db_findall(DBRef, X, f(11,X,3), atom(X), Bag),
	length(Bag, L),
	format('baglength: ~d~n', [L]).

generate_term(N, f(A,X,B)) :-
	term(X),
	between(1,N,A),
	between(1,N,B).
generate_term(N, g(A,B,C,X)) :-
	term(X),
	between(1,N,A),
	between(1,N,B),
	between(1,N,C).

between(I, J, I) :- I =< J.
between(I, J, K) :- I < J, I1 is I+1, between(I1, J, K).

term(_).
term(x).
term(y).
