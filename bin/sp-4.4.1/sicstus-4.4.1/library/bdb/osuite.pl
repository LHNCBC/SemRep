%% [PM] 4.1.3 obsolete code
end_of_file.

:- use_module(library(db)), use_module(library(lists)).

op :-
	Spec = on(on,on,on),
	db_open('/tmp/odb', update, Spec, DBRef),
	time(fill, fill(DBRef)),
	time(enumerate, enumerate(DBRef)),
	time(fetch_ind, fetch_ind(DBRef)),
	time(fetch_noind, fetch_noind(DBRef)),
	time(findall, findall(DBRef)),
	time(delete, delete(DBRef)),
	db_close(DBRef).

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
	db_fetch(DBRef, _, _),
	fail.
enumerate(_).

fetch_ind(DBRef) :-
	db_fetch(DBRef, g(y,_,x), _),
	fail.
fetch_ind(_).

fetch_noind(DBRef) :-
	db_fetch(DBRef, g(f(y,x),_,_), _),
	fail.
fetch_noind(_).

findall(DBRef) :-
	db_findall(DBRef, f(_,y), Bag),
	length(Bag, L),
	format('baglength: ~d~n', [L]).

delete(DBRef) :-
	db_fetch(DBRef, f(_,x), R),
	db_erase(DBRef, R),
	fail.
delete(_).

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
