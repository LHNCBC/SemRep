:- module(primesums, [primesums/1]).

:- use_module(library(lists)).
:- use_module(library(clpfd)).

/* Fill an NxN square with integers 1..n such that the sum of every
   row and column is a prime number. */

primesums(Side) :-
        Side > 1,
	(   for(_,1,Side),
	    foreach(Row1,Rows),
	    fromto(Fmts,["~t~w,~6+"|Tail],Tail,["~n~n"]),
	    param(Side)
	do  length(Row1,Side)	    
	),
	append(Fmts, Fmt),
	append(Rows, List),
        Square is Side*Side,
        domain(List, 1, Square),
        all_distinct(List),
	(   for(J,2,Side*Side*Side),
	    foreach(J,S)
	do  true
	),
	(   fromto(S,[P|S1],S2,[]),
	    foreach(P,Primes)
	do  (   foreach(Y,S1),
		fromto(S2,T1,T2,[]),
		param(P)
	    do  (Y mod P > 0 -> T1 = [Y|T2] ; T1 = T2)
	    )
	),
	list_to_fdset(Primes, Fdset),
        transpose(Rows, Cols),
	Rows = [[R11,R12|R1s]|_],
	Cols = [[C11,C12|C1s]|_],
	R12 #< C12,
	(   foreach(Ry,[R12|R1s]),
	    fromto(R11,Rx,Ry,_),
	    foreach(Cy,[C12|C1s]),
	    fromto(C11,Cx,Cy,_)
	do  Rx #< Ry,
	    Cx #< Cy
	),
	(   foreach(Row2,Rows),
	    foreach(Col,Cols),
	    param(Fdset)
	do  Sum1 in_set Fdset,
	    sum(Row2, #=, Sum1),
	    Sum2 in_set Fdset,
	    sum(Col, #=, Sum2)
	),
        labeling([ff,bisect], List),
	(   foreach(Row3,Rows),
	    param(Fmt)
	do  format(Fmt, Row3)
	).

