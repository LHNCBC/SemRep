%% [PM] 4.1.3 obsolete code
end_of_file.

:- use_module(library(bdb)).

o(Term, TermRef) :-
	db_open('/tmp/db', read, _Spec, DBRef),
	call_cleanup(db_fetch(DBRef, Term, TermRef),
		     db_close(DBRef)).

p(Term) :-
        db_open_env('/tmp/env', EnvRef),
        db_open(EnvRef, '/tmp/db', update, [x, f(+,-,+)], DBRef),
        db_store(DBRef, Term, _TermRef),
        db_close(DBRef),        % db_close_env closes the database
        db_close_env(EnvRef).

q :-
        db_open('/tmp/db', enumerate, _, DBRef),
        db_make_iterator(DBRef, Iterator),
        print_terms(Iterator),
        db_iterator_done(Iterator), % db_close deletes the iterator
        db_close(DBRef).

r :-
        db_open('/tmp/db', read, _Spec, DBRef),
        db_make_iterator(DBRef, f(a, _, c), Iterator),
        print_terms(Iterator),
        db_iterator_done(Iterator), % db_close deletes the iterator
        db_close(DBRef).

s :-
	db_open('/tmp/db', enumerate, Spec, DBRef),
	write(Spec), nl,
	(   db_enumerate(DBRef, Term, TermRef),
	    write(Term-TermRef), nl,
	    fail
	;   db_close(DBRef)
	).

t :-
	db_open('/tmp/db', update, _Spec, DBRef),
	db_findall(DBRef, Y, f(a, Y, b), atom(Y), Bag),
	write_list(Bag),
	db_close(DBRef).

v :-
	db_open('/tmp/db', read, Spec, DBRef),
	write(Spec), nl,
	(   db_fetch(DBRef, f(a,Y,b), TR),
	    write(Y-TR), nl,
	    fail
	;   db_close(DBRef)
	).

w :-
	db_open_env('/tmp/env', EnvRef),
	db_open(EnvRef, '/tmp/db', update, Spec, DBRef),
	write(Spec), nl,
	(   db_fetch(DBRef, f(a,Y,c), TR),
	    write(Y-TR), nl,
	    db_erase(DBRef, TR),
	    fail
	;   db_close(DBRef), db_close_env(EnvRef)
	).

write_list([]).
write_list([H|T]) :-
	write(H), nl,
	write_list(T).

print_terms(Iterator) :-
        db_iterator_next(Iterator, Term, TermRef), !,
        write(Term-TermRef), nl,
        print_terms(Iterator).
print_terms(_).
