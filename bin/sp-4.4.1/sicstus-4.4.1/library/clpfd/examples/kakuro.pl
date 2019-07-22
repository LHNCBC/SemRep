:- module(kakuro, [kakuro/1]).

:- use_module(library(lists)).
:- use_module(library(terms), [term_variables_set/2]).
:- use_module(library(clpfd)).
	
% A matrix element is one of:
% !\! - black square
% A\! - black square, sum of cells below is A
% !\B - black square, sum of cells to the right is B
% A\B - black square, sum of cells below\to the right is A\B
kakuro(ID) :-
	matrix(ID, Matrix),
	term_variables_set(Matrix, Vars),
	domain(Vars, 1, 9),
	(   foreach(Row,Matrix)
	do  constrain(row, Row)
	),
	transpose(Matrix, Transpose),
	(   foreach(Col,Transpose)
	do  constrain(col, Col)
	),
	length(Transpose, N),
	fmt(N, Fmt, []),
	labeling([ff,enum], Vars),
	(   foreach(R,Matrix),
	    param(Fmt)
	do  format(user, Fmt, R)
	).

constrain(_, []) :- !.
constrain(row, [_\Sum|Row]) :-
	integer(Sum), !,
	stretch(Stretch, Row, Row1),
	all_diff_sum(global, Stretch, Sum),
	constrain(row, Row1).
constrain(col, [Sum\_|Row]) :-
	integer(Sum), !,
	stretch(Stretch, Row, Row1), 
	all_diff_sum(global, Stretch, Sum),
	constrain(col, Row1).
constrain(Mode, [_|Row]) :-
	constrain(Mode, Row).

stretch([X|Xs]) --> [X], {simple(X)}, !,
	stretch(Xs).
stretch([]) --> [].

fmt(0) --> !, "\n\n".
fmt(I) --> "~t~w~6+",
	{J is I-1},
	fmt(J).

all_diff_sum(_, L, Sum) :-
	length(L, N),
	Series is N*(N+1)//2,
	(   Sum=:=Series ->
	    domain(L, 1, N)
	;   Sum=:=10*N-Series ->
	    Min is 10-N,
	    domain(L, Min, 9)
	), !,
	all_distinct(L).
all_diff_sum(global, L, Sum) :-
	plusify(L, Expr),
	all_distinct(L, [Expr #= Sum]).
all_diff_sum(gcc, L, Sum) :-
	same_length(L, Matrix),
	(   foreach([1,2,3,4,5,6,7,8,9],Matrix)
	do  true
	),
	domain([B1,B2,B3,B4,B5,B6,B7,B8,B9], 0, 1),
	global_cardinality(L, [1-B1,2-B2,3-B3,4-B4,5-B5,6-B6,7-B7,8-B8,9-B9], [cost(Sum,Matrix)]).
all_diff_sum(table, L, Sum) :-
	findall(T, all_diff_sum_tuple(L,Sum,T), Ts),
	table([L], Ts).

all_diff_sum_tuple(L, Sum, T) :-
	same_length(L, T),
	domain(T, 1, 9),
	sum(T, #=, Sum),
	all_different(T),
	labeling([], T).

plusify([], 0).
plusify([P|Ps], Conj) :-
	plusify(Ps, P, Conj).

plusify([], P, P).
plusify([P|Ps], Q, Conj) :-
	plusify(Ps, Q+P, Conj).


matrix(tiny, [[ !\! , !\! ,11\! , 4\! , !\! , !\! ],
	      [ !\! ,14\5 ,  _  ,  _  ,10\! , !\! ],
	      [ !\17,  _  ,  _  ,  _  ,  _  , 3\!],
	      [ !\6 ,  5  ,  1  , 3\4 ,  3  ,  1],
	      [ !\! , !\10,  3  ,  1  ,  4  ,  2],
	      [ !\! , !\! , !\3 ,  2  ,  1  , !\! ]]).

matrix(kakuro17, [[ !\! ,16\! ,17\! , !\! , !\! ,16\! ,22\! , !\! ,13\! ,14\! ],
		  [ !\17,  _  ,  _  ,17\! , !\14,  _  ,  _  ,30\16,  _  ,  _  ],
		  [ !\24,  _  ,  _  ,  _  ,15\35,  _  ,  _  ,  _  ,  _  ,  _  ],
		  [ !\! ,26\! ,28\16,  _  ,  _  ,35\16,  _  ,  _  ,24\! ,22\! ],
		  [ !\4 ,  _  ,  _  , !\17,  _  ,  _  ,16\24,  _  ,  _  ,  _  ],
		  [ !\14,  _  ,  _  , !\! , 6\35,  _  ,  _  ,  _  ,  _  ,  _  ],
		  [ !\3 ,  _  ,  _  ,30\21,  _  ,  _  ,  _  , !\3 ,  _  ,  _  ],
		  [ !\32,  _  ,  _  ,  _  ,  _  ,  _  ,17\! , !\6 ,  _  ,  _  ],
		  [ !\23,  _  ,  _  ,  _  ,24\16,  _  ,  _  ,16\4 ,  _  ,  _  ],
		  [ !\! ,14\! ,13\17,  _  ,  _  ,17\17,  _  ,  _  ,16\! ,17\! ],
		  [ !\35,  _  ,  _  ,  _  ,  _  ,  _  , !\24,  _  ,  _  ,  _  ],
		  [ !\16,  _  ,  _  , !\16,  _  ,  _  , !\! , !\16,  _  ,  _  ]]).

% Author: Takei Daisuke
matrix(hard, [[ !\! , 7\! ,11\! ,28\! ,12\! , !\! ,24\! ,23\! ,15\! , !\! , 4\! ,41\! ,14\! , 7\! ],
	      [ !\10,  _  ,  _  ,  _  ,  _  , !\12,  _  ,  _  ,  _  , !\10,  _  ,  _  ,  _  ,  _  ],
	      [ !\24,  _  ,  _  ,  _  ,  _  ,18\20,  _  ,  _  ,  _  ,19\15,  _  ,  _  ,  _  ,  _  ],
	      [ !\7 ,  _  ,  _  ,  _  , 7\27,  _  ,  _  ,  _  ,  _  ,  _  ,10\20,  _  ,  _  ,  _  ],
	      [ !\! , !\16,  _  ,  _  ,  _  ,  _  ,  _  , !\31,  _  ,  _  ,  _  ,  _  ,  _  , !\! ],
	      [ !\! , !\! ,19\9 ,  _  ,  _  ,  _  ,35\! , !\! ,19\8 ,  _  ,  _  ,  _  ,17\! , !\! ],
	      [ !\! ,12\17,  _  ,  _  ,  _  ,  _  ,  _  , 9\16,  _  ,  _  ,  _  ,  _  ,  _  , 7\! ],
	      [ !\8 ,  _  ,  _  ,  _  , !\16,  _  ,  _  ,  _  ,  _  ,  _  , !\6 ,  _  ,  _  ,  _  ],
	      [ !\17,  _  ,  _  ,34\! , !\! ,20\22,  _  ,  _  ,  _  ,35\! , !\! ,23\8 ,  _  ,  _  ],
	      [ !\13,  _  ,  _  ,  _  ,16\20,  _  ,  _  ,  _  ,  _  ,  _  ,18\9 ,  _  ,  _  ,  _  ],
	      [ !\! , !\32,  _  ,  _  ,  _  ,  _  ,  _  , !\17,  _  ,  _  ,  _  ,  _  ,  _  , !\! ],
	      [ !\! , !\! ,33\7 ,  _  ,  _  ,  _  ,23\! , !\! ,33\24,  _  ,  _  ,  _  ,15\! , !\! ],
	      [ !\! ,23\34,  _  ,  _  ,  _  ,  _  ,  _  ,10\32,  _  ,  _  ,  _  ,  _  ,  _  , 6\! ],
	      [ !\22,  _  ,  _  ,  _  , !\30,  _  ,  _  ,  _  ,  _  ,  _  , !\6 ,  _  ,  _  ,  _  ],
	      [ !\12,  _  ,  _  ,29\! , !\! , 29\9,  _  ,  _  ,  _  ,31\! , !\! ,36\7 ,  _  ,  _  ],
	      [ !\24,  _  ,  _  ,  _  ,10\30,  _  ,  _  ,  _  ,  _  ,  _  ,24\7 ,  _  ,  _  ,  _  ],
	      [ !\! , !\35,  _  ,  _  ,  _  ,  _  ,  _  , !\19,  _  ,  _  ,  _  ,  _  ,  _  , !\! ],
	      [ !\! , !\! ,28\7 ,  _  ,  _  ,  _  ,26\! , !\! ,10\24,  _  ,  _  ,  _  ,26\! , !\! ],
	      [ !\! ,23\31,  _  ,  _  ,  _  ,  _  ,  _  ,23\34,  _  ,  _  ,  _  ,  _  ,  _  , 23\!],
	      [ !\19,  _  ,  _  ,  _  ,10\26,  _  ,  _  ,  _  ,  _  ,  _  ,12\24,  _  ,  _  ,  _  ],
	      [ !\28,  _  ,  _  ,  _  ,  _  , !\10,  _  ,  _  ,  _  , !\30,  _  ,  _  ,  _  ,  _  ],
	      [ !\14,  _  ,  _  ,  _  ,  _  , !\20,  _  ,  _  ,  _  , !\12,  _  ,  _  ,  _  ,  _  ]]). 

