write_store(Mod) :-
	collect_store(Mod,Store),
	write_list(Store).

collect_store(Mod,Store) :-
	findall(C,get_constraints(C,Mod),Store).

get_constraints(C,Mod) :-
	Mod:'$enumerate_suspensions'(Susp),
	arg(6,Susp,C),
	numbervars(C,0,_).

write_list([]).
write_list([X|R]) :-
	write(X), nl,
	write_list(R).
