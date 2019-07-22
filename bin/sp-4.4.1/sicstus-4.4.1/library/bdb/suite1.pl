%% [PM] 4.1.3 obsolete code
end_of_file.

/*
| ?- p.
generate_term: 0.09 sec.
fill: 28.193 sec.
# of elements: 54756: ok
count: 3.469 sec.
enumerate: 3.037 sec.
fetch: 0.002 sec.
fetch_all: 55.72 sec.
baglength: 2
findall: 0.002 sec.
delete+enumerate: 3.572 sec.
press <ret>
|: 

name                  calls     user     system
-------------------------------------------------
open_env                      1    0.000    0.033
close_env                     1    0.000    0.000
open_db                       1    0.000    0.017
close_db                      1    0.000    0.050
read_spec                     1    0.000    0.000
next_termref              54756    4.483    0.283
store_termref             54756    6.617    1.367
delete_termref             1014    0.033    0.000
store_term                54756    9.183    2.917
delete_term                1014    0.050    0.000
fetch_term                 1014    0.050    0.000
global_iterator               3    0.000    0.000
global_iterator_next     164271    2.983    1.100
global_iterator_done          3    0.000    0.000
term_iterator             54761    1.083    0.450
term_iterator_next       253865   32.367    3.683
term_iterator_done        54761    0.533    0.417

yes
| ?- p.
generate_term: 0.09 sec.
fill: 30.214 sec.
# of elements: 108498: wrong
count: 7.07 sec.
enumerate: 6.409 sec.
fetch: 0.005 sec.
fetch_all: 98.123 sec.
baglength: 2
findall: 0.001 sec.
delete+enumerate: 6.975 sec.
press <ret>
|: 

name                  calls     user     system
-------------------------------------------------
open_env                      1    0.000    0.000
close_env                     1    0.000    0.000
open_db                       1    0.000    0.000
close_db                      1    0.000    0.050
read_spec                     1    0.017    0.000
next_termref              54756    5.200    0.483
store_termref             54756    4.567    3.917
delete_termref             1014    0.117    0.017
store_term                54756   12.017   13.517
delete_term                1014    0.117    0.000
fetch_term                 1014    0.067    0.017
global_iterator               3    0.000    0.000
global_iterator_next     325497    5.617    2.267
global_iterator_done          3    0.000    0.000
term_iterator             54764    1.167    0.483
term_iterator_next       449684   61.950   29.933
term_iterator_done        54764    0.717    0.333

yes
| ?- 
*/

:- use_module(newdb), use_module(library(lists)).
% :- use_module(library(gauge)).

p :-
	do('/tmp/env', '/tmp/db', '/tmp/db1').

pwin :-
	do('c:/tmp/env', 'c:\\tmp\\db', 'c:/tmp/db1').

do(FE, FDB, FDB1) :-
%	newdb:zerostat,
	db_open_env(FE, 2048, EnvRef),
	Spec = [g(+,+,+,-),f(+,-,+)],
	db_open(FDB, update, Spec, EnvRef, DBRef),
	N = 26,
	time(generate_term, generate_term(N)),
	time(fill, fill(N, DBRef)),
	time(count, count(N, DBRef)),
	time(enumerate, enumerate(DBRef)),
	time(fetch, fetch(DBRef)),
	time(fetch_all, fetch_all(N, DBRef)),
	time(findall, findall(DBRef)),
	time('delete+enumerate', delete(DBRef)),
%	time(copy, copy(DBRef, FDB1)),
%	write('press <ret>\n'),
%	get0(_),
	db_close(DBRef),
	db_close_env(EnvRef), nl.
%	newdb:printstat.

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

count(N, DBRef) :-
	db_make_iterator(DBRef, Iterator),
	count_loop(Iterator, 0, Count),
	db_iterator_done(Iterator),
	(   Count =:= 3*N*N+3*N*N*N -> R = ok
	;   R = wrong
	),
	format('# of elements: ~d: ~w~n', [Count, R]).

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
	db_findall(DBRef, X, f(10,X,3), atom(X), Bag),
	length(Bag, L),
	format('baglength: ~d~n', [L]).

delete(DBRef) :-
	db_enumerate(DBRef, X, R),
	X = f(A,_,_),
	(   A mod 2 =:= 0 -> db_erase(DBRef, R)
	),
	fail.
delete(_).

copy(DBRef, DB1) :-
	db_compress(DBRef, DB1).

generate_term(N, f(A,X,B)) :-
	between(1,N,A),
	between(1,N,B),
	term(X).
generate_term(N, g(A,B,C,X)) :-
	between(1,N,A),
	between(1,N,B),
	between(1,N,C),
	term(X).

between(I, J, I) :- I =< J.
between(I, J, K) :- I < J, I1 is I+1, between(I1, J, K).

term(_).
term(x).
term(y).
