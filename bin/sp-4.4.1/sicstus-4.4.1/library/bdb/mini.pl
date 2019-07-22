%% [PM] 4.1.3 obsolete code
end_of_file.

:- use_module(newdb).

s(T, R) :-
	p(store, '/tmp/env', '/home/benko/benko/tmp/db', T, R).

f(T, R) :-
	p(fetch, '/tmp/env', '/home/benko/benko/tmp/db', T, R).

e(T, R) :-
	p(enum, '/tmp/env', '/home/benko/benko/tmp/db', T, R).

p(O, FE, FDB, T, R) :-
	db_open_env(FE, EnvRef),
	Spec = [g(+,+,+,-),f(+,-,+)],
	db_open(FDB, update, Spec, EnvRef, DBRef),
	call_cleanup(operate(O, DBRef, T, R), db_close_env(EnvRef)).

operate(store, DBRef, T, R) :-
	db_store(DBRef, T, R).
operate(fetch, DBRef, T, R) :-
	db_fetch(DBRef, T, R).
operate(enum, DBRef, T, R) :-
	db_enumerate(DBRef, T, R).
