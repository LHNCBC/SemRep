:- use_module(library('clpqr/monash')).

/*
 **********************************************************************
 *
 *      CLP(R) Version 2.0	(Example Programs Release)
 *	(C) Copyright, March 1986, Monash University
 *
 **********************************************************************
 */

%
% Program to translate declarative machine tool path descriptions into
% code for a programmable controller.
%

move_p(p(_,X,Y)):- 
	printf("G00 X%-.3f Y%-.3f\n",[X,Y]).
draw_p(p(_,X,Y)):- 
	printf("G01 X%-.3f Y%-.3f\n",[X,Y]).

path([p(N)|T],DB):- 
	member(p(N,X,Y),DB),
	move_p(p(N,X,Y)),
	rest_path(T,DB).
	
rest_path([p(N)|T],DB):- 
	member(p(N,X,Y),DB),
	draw_p(p(N,X,Y)),
	rest_path(T,DB).
rest_path([],_).

make_vars([H|T],V,[NV|EV]):- 
	get_vars(H,NV),
	make_vars(T,V,EV).
make_vars([],V,V).

get_vars(p(N,xy(X,Y)),p(N,X,Y)).
get_vars(p(N,l(A),l(B)),p(N,X,Y)).
get_vars(l(N,mc(M,C)),l(N,M,C)).
get_vars(l(N,p(A),p(B)),l(N,M,C)).

make_constraints([H|T],V):- 
	get_constraint(H,V),
	make_constraints(T,V).
make_constraints([],_).

get_constraint(p(N,l(A),l(B)),V):-
	{Y = M1 * X + C1},
	{Y = M2 * X + C2},
	member(p(N,X,Y),V),
	member(l(A,M1,C1),V),
	member(l(B,M2,C2),V).
get_constraint(l(N,p(A),p(B)),V):- 
	{X1 = X2},
	{M = 10000000},
	{Y1 = M * X1 + C},
	member(l(N,M,C),V),
	member(p(A,X1,Y1),V),
	member(p(B,X2,Y2),V).
get_constraint(l(N,p(A),p(B)),V):- 
	{Y1 = M * X1 + C},
	{Y2 = M * X2 + C},
	neq_r(X1,X2),
	member(l(N,M,C),V),
	member(p(A,X1,Y1),V),
	member(p(B,X2,Y2),V).
get_constraint(p(_,xy(_,_)),_).
get_constraint(l(_,mc(_,_)),_).

neq_r(X,Y):-
	{Y > X}.
neq_r(X,Y):-
	{X > Y}.

make_db([p(N,X,Y)|T],DB,[p(N,X,Y)|EDB]):- 
	make_db(T,DB,EDB).
make_db([l(N,M,C)|T],DB,EDB):- 
	make_db(T,DB,EDB).
make_db([],DB,DB).

geometry(X,DB):- 
	make_vars(X,[],V),
	make_db(V,[],DB),
	make_constraints(X,V).

go:-
	{Ten = 10},
	{Twenty = 20},
	geometry(
		[
			p(1,xy(Ten,Ten)),
			p(2,xy(Ten,Twenty)),
			p(3,xy(Twenty,Ten)),
			p(4,xy(Twenty,Twenty)),
			l(1,p(1),p(4)),
			l(2,p(2),p(3)),
			p(5,l(1),l(2))
		],
		DB
	),
	path(
		[
			p(1),
			p(2),
			p(5),
			p(1)
		],
		DB
	).

% Output:
%  G00 X10.000 Y10.000
%  G01 X10.000 Y20.000
%  G01 X15.000 Y15.000
%  G01 X10.000 Y10.000

?- printf("\n>>> Sample goal: go/0\n", []).
