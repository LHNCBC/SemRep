%% [PM] 4.1.3 obsolete code
end_of_file.

/*
| ?- p.
generate_term: 0.087 sec.
fill: 39.865 sec.
enumerate: 2.016 sec.
fetch: 0.002 sec.
fetch_all: 47.096 sec.
baglength: 3
findall: 0.001 sec.
delete+enumerate: 2.527 sec.

yes
| ?- p.
generate_term: 0.087 sec.
fill: 37.178 sec.
enumerate: 3.914 sec.
fetch: 0.004 sec.
fetch_all: 56.361 sec.
baglength: 3
findall: 0.0 sec.
delete+enumerate: 20.946 sec.

yes
| ?- 
*/

:- use_module(library(db)), use_module(library(lists)).

p :-
	Spec = on(on,on,on),
	db_open('/tmp/odb', update, Spec, DBRef),
	N = 26,
	time(generate_term, generate_term(N)),
	time(fill, fill(N, DBRef)),
	time(enumerate, enumerate(DBRef)),
	time(fetch, fetch(DBRef)),
	time(fetch_all, fetch_all(N, DBRef)),
	time(findall, findall(DBRef)),
	time('delete+enumerate', delete(DBRef)),
	db_close(DBRef).

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

fill(N, DBRef) :-
	generate_term(N, Term),
	db_store(DBRef, Term, _),
	fail.
fill(_, _).

enumerate(DBRef) :-
	db_fetch(DBRef, _, _),
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
	db_findall(DBRef, f(10,_,3), Bag),
	length(Bag, L),
	format('baglength: ~d~n', [L]).

delete(DBRef) :-
	db_fetch(DBRef, X, R),
	X = f(A,_,_),		% to correspond to suite1.pl
	(   A mod 2 =:= 0 -> db_erase(DBRef, R)
	;   true
	),
	fail.
delete(_).

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
