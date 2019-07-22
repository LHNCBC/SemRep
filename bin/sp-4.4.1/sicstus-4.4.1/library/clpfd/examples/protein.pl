:- module(protein, [protein/1]).

%% ?- bench([protein(15),protein(20)]).

:- use_module(library(clpfd)).

protein(N) :-
	length(Primary,N),
	(   foreach(h,Primary)
	do  true
	),
	constrain(Primary,Tertiary,NegErg),
	labeling([maximize(NegErg)],Tertiary),
	format('Energy = ~d\n', [-NegErg]),
	plot(Tertiary, N).
protein(_) :-
	write('no solutions:'),nl.

constrain(Primary,Tertiary,NegErg) :-
	length(Primary,N),
	M is 2*N,
	Min is N - integer(sqrt(N)), 
	Max is N + integer(sqrt(N)),
	length(Tertiary,M),
	domain(Tertiary,Min,Max),
	starting_point(Tertiary,N),
	avoid_self_loops(Tertiary),
	next_constraints(Tertiary),
	energy_terms(Primary, Tertiary, Terms, []),
	sum(Terms, #=, NegErg).

starting_point(Tert, N) :-
	N1 is N + 1,
	Tert = [N,N,N,N1|_],
	all_up(Tert, Up, N, N),
	lex_chain([Tert,Up]).	% Mats - break symmetry

all_up([], [], _, _).
all_up([_,_|Tert], [X,Y|Up], X, Y) :-
	Y1 is Y+1,
	all_up(Tert, Up, X, Y1).

avoid_self_loops(Tertiary):- !,
	positions_to_rectangles(Tertiary, Rectanges),
	disjoint2(Rectanges).

positions_to_rectangles([],[]) :- !.
positions_to_rectangles([X,Y|R], [r(X,1,Y,1)|S]):-
	positions_to_rectangles(R,S).

next_constraints([_,_]).
next_constraints([X1,Y1,X2,Y2|C]) :-
	next(X1,Y1,X2,Y2),
	next_constraints([X2,Y2|C]).

next(X1,Y1,X2,Y2):-
	domain([Dx,Dy],-1,1),
	Dx #= X1-X2,
	Dy #= Y1-Y2,
	moveone(Dx, Dy).

moveone(_Dx, _Dy) +:
	table([[{-1,1},0],[0,{-1,1}]]).

energy_terms([_], _) --> !.
energy_terms([A,B|Primary], [XA,YA,XB,YB|Tertiary]) -->
	energy_contribution_of_A(0,A,XA,YA,Primary,Tertiary),
	energy_terms([B|Primary],[XB,YB|Tertiary]).

energy_contribution_of_A(_,_,_,_,[],[]) --> !.
energy_contribution_of_A(0,A,XA,YA,[_|Primary],[_,_|Tertiary]) -->
	energy_contribution_of_A(1,A,XA,YA,Primary,Tertiary).
energy_contribution_of_A(1,A,XA,YA,[B|Primary],[XB,YB|Tertiary]) -->
	energy(A,XA,YA,B,XB,YB),
	energy_contribution_of_A(0,A,XA,YA,Primary,Tertiary).

energy(h,XA,YA,h,XB,YB) --> !, [C],
	{Delta #= abs(XA - XB) + abs(YA - YB)},
	{Delta #= 1 #<=> C}.
energy(_,_,_,_,_,_) --> [].

plot(Tertiary, N) :-
	Min is N - integer(sqrt(N)), 
	Max is N + integer(sqrt(N)),
	y_major(Tertiary, 0, Pairs1),
	sort(Pairs1, Pairs2),
	Min1 is Min-1,
	plot(Min1, Max, Min1, Pairs2, []).

plot(Max, Max, _) --> !.
plot(Y0, Max, Min1) -->
	{Y is Y0+1},
	plot_row(Min1, Max, Y),
	plot(Y, Max, Min1).

plot_row(Max, Max, _) --> !, {nl,nl}.
plot_row(X0, Max, Y) -->
	{X is X0+1},
	plot_element(X, Y),
	plot_row(X, Max, Y).

plot_element(X, Y) --> [f(Y,X,J)], !,
	{write4(J)}.
plot_element(_, _) -->
	{write('    ')}.

write4(J) :- J<10, !, write('   '), write(J).
write4(J) :- write('  '), write(J).

y_major([], _, []).
y_major([X,Y|XYs], I, [f(Y,X,J)|YXs]) :-
	J is I+1,
	y_major(XYs, J, YXs).
