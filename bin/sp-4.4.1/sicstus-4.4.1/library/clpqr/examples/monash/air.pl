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
% Calculate excess air needed for a chemical reaction to proceed in some way
% The goal
%            ?- go.
% is a sample reaction.
%

mol_comp(n2, [f(n, 2)]).
mol_comp(o2, [f(o, 2)]).
mol_comp(co2, [f(c, 1),f(o, 2)]).
mol_comp(co, [f(c, 1),f(o, 1)]).
mol_comp(c, [f(c, 1)]).
mol_comp(h2o, [f(h, 2),f(o, 1)]).

comp(air, vol, [f(n2, 0.79),f(o2, 0.21)]).
comp(flue, vol, [f(co2, 0.125),f(co, 0.025),f(o2, 0.03),f(n2, 0.82)]).
comp(full_comb, vol, [f(co2, 1),f(n2, T)]).
comp(fuel, vol, [f(c, 1)]).

basis(1.0).

bal(LEFT, RIGHT, VARS) :-  
	bal_side(LEFT, VARS, NEW_VARS),
	bal_side(RIGHT, NEW_VARS, N_VARS),
	zero(N_VARS).

zero([]).
zero([f(X, Z)|L]) :-
	{Z = 0},
	zero(L).

bal_side([], [], []).
bal_side([], [f(A, X)| L1], [f(A, MX)| L2]) :- 
	{MX = -X},
	bal_side([], L1, L2).
bal_side([f(S,X)| L], VARS, NEW_VARS) :-  
	comp(S, T, COMP_LIST),
	printf("species   %s \n",[S]),
	add_species(X, COMP_LIST, VARS, N_VARS),
	bal_side(L,N_VARS, NEW_VARS).

add_species(X, [f(SPEC, PROP)| L], VARS, NEW_VARS) :- 
	printf("molecule   %s \n",[SPEC]),
	mol_comp(SPEC, ELEMENTS),
	add_elements(X, PROP, ELEMENTS, VARS, N_VARS),
	add_species(X, L, N_VARS, NEW_VARS).
add_species(X, [], VARS, VARS).

add_elements(X, PROP, [f(n, NUM)| L], VARS, NEW_VARS) :- 
	add_elements(X, PROP, L, VARS, NEW_VARS).

add_elements(X, PROP, [f(E, NUM)| L], [f(E, Z)| VARS], [f(E, Z1)| NEW_VARS]):-
	{Z = X*PROP*NUM + Z1},
	printf("atom   %s \n",[E]),
	add_elements(X, PROP, L, VARS, NEW_VARS).

add_elements(X, PROP, [f(F, NUM)| L], [f(E, Z)| VARS], [f(E, Z)| NEW_VARS]):- 
	add_elements(X, PROP, [f(F, NUM)| L], VARS, NEW_VARS).

add_elements(X, PROP, [], VARS, VARS).

excess(A, A1) :- 
	{B = A - A1},
	{B = ANS * A1},
	{PERC = ANS * 100},
	printf("Excess = %f%% \n",[PERC]).
	
go:- 	basis(BASE),
	bal([f(air, A),f(fuel, BASE)], [f(flue, F)], [f(c,0),f(o,0)]),
	printf("With base of %f mol of fuel\n",[BASE]),
	printf("air (just to burn the carbon) = %f mol\n",[A]),
	printf("flue (mol) :%f\n",[F]),
	nl,
	bal([f(air, A1),f(fuel, BASE)], [f(full_comb, F1)], [f(c,0),f(o,0)]),
	printf("Compared with theoretical air (for complete combustion):\n",[]),
	printf("     %f mol\n",[A1]),
	excess(A, A1).


% Answer:
%  species   air 
%  molecule   n2 
%  molecule   o2 
%  atom   o 
%  species   fuel 
%  molecule   c 
%  atom   c 
%  species   flue 
%  molecule   co2 
%  atom   c 
%  atom   o 
%  molecule   co 
%  atom   c 
%  atom   o 
%  molecule   o2 
%  atom   o 
%  molecule   n2 
%  With base of 1.000000 mol of fuel
%  air (just to burn the carbon) = 5.317460 mol
%  flue (mol) :6.666667
%  
%  species   air 
%  molecule   n2 
%  molecule   o2 
%  atom   o 
%  species   fuel 
%  molecule   c 
%  atom   c 
%  species   full_comb 
%  molecule   co2 
%  atom   c 
%  atom   o 
%  molecule   n2 
%  Compared with theoretical air (for complete combustion):
%       4.761905 mol
%  Excess = 11.666667% 


?- printf("\n>>> Sample goal: go/0\n", []).

