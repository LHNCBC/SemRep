%% [PM] 4.1.3 obsolete code
end_of_file.

:- use_module(newdb), use_module(library(lists)).
:- use_module(library(gauge)).

p :-
	newdb:zerostat,
	db_open_env('/tmp/env', EnvRef),
	Spec = [x,y,f(+,-),f(-,+),g(-,-,-),g(+,-,+)],
	db_open('/tmp/db', update, Spec, EnvRef, DBRef),
	time(fill, fill(DBRef)),
	time(enumerate, enumerate(DBRef)),
	time(fetch_ind, fetch_ind(DBRef)),
	time(fetch_noind, fetch_noind(DBRef)),
	time(findall, findall(DBRef)),
	time(delete, delete(DBRef)),
%	time(copy, copy(DBRef, '/tmp/db1')),
	db_close(DBRef),
	db_close_env(EnvRef), nl,
	newdb:printstat.

time(Name, Goal) :-
	statistics(runtime, [T0,_]),
	call(Goal),
	statistics(runtime, [T1,_]),
	T is (T1-T0)/1000,
	format('~w: ~w sec.~n', [Name,T]).

fill(DBRef) :-
	generate_term(Term),
	db_store(DBRef, Term, _),
	fail.
fill(_).

enumerate(DBRef) :-
	db_enumerate(DBRef, _, _),
	fail.
enumerate(_).

fetch_ind(DBRef) :-
	db_fetch(DBRef, g(y,_,x), _),
	fail.
fetch_ind(_).

% enumerate: 22.53 sec.
% fetch_noind: 38.21 sec.
fetch_noind(DBRef) :-
	db_fetch(DBRef, g(f(y,x),_,_), _),
	fail.
fetch_noind(_).

findall(DBRef) :-
	db_findall(DBRef, X, f(X,y), var(X), Bag),
	length(Bag, L),
	format('baglength: ~d~n', [L]).

delete(DBRef) :-
	(   db_fetch(DBRef, f(_,x), R) -> true
	;   write(failure), nl, fail % local iterator next: Invalid argument
	),
	db_erase(DBRef, R),
	fail.
delete(_).

% the resulting database is bigger!
% fill: 530.61 sec.
% copy: 474.16 sec.
copy(DBRef, DB1) :-
	db_compress(DBRef, DB1).

generate_term(T) :-
	generate_term(3, T), nonvar(T).

generate_term(N, T) :-
	N > 0,
	N1 is N-1,
	(   term(T)
	;   T = f(X,Y),
	    generate_term(N1, X),
	    generate_term(N1, Y)
	;   T = g(X,Y,Z),
	    generate_term(N1, X),
	    generate_term(N1, Y),
	    generate_term(N1, Z)
	).

term(_).
term(x).
term(y).
