%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   dump.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
dump( +Target, ?NewVars, ?CodedAnswer)

where Target and NewVars are lists of variables of equal length and
CodedAnswer is the term representation of the projection of constraints
onto the target variables where the target variables are replaced by   
the corresponding variables from NewVars.
*/

:- use_module( library(types), [must_be/4]).

:- use_module( library(terms), [term_variables_set/2]).

:- use_module( library(assoc3), 
	[
	    empty_assoc/1,
	    put_assoc/4,
	    assoc_to_list/2
	]).

dump( Target, NewVars, Constraints) :-
	must_be(Target, proper_list(var), dump( Target, NewVars, Constraints), 1),
	findall(Target-Gs, vars_constraints(Target,Gs), [NewVars-Constraints]).

:- meta_predicate projecting_assert(:).

projecting_assert( Module:Clause) :-
	term_variables_set( Clause, Target),
	findall(Clause-Gs, vars_constraints(Target,Gs), [Copy-Constraints]),
	l2c( Constraints, Conj),		  % fails for []
	this_linear_solver( Sm),		  % proper module for {}/1
	!,
	( Copy = (H:-B) ->			  % former rule
	    Module:assert((H:-Sm:{Conj},B))
	;					  % former fact
	    Module:assert((Copy:-Sm:{Conj}))
	).
projecting_assert( Clause) :-			  % not our business
	assert( Clause).

vars_constraints(Target, Gs) :-
	ordering(Target),
	related_linear_vars( Target, All),
	nonlin_crux( All, Nonlin),
	project_attributes( Target, All),
	related_linear_vars( Target, Again), % project drops/adds vars
	all_attribute_goals( Again, Gs, Nonlin).

l2c( [X|Xs], Conj) :-
	( Xs = [] ->
	    Conj = X
	;
	    Conj = (X,Xc),
	    l2c( Xs, Xc)
	).

related_linear_vars( Vs, All) :-
	empty_assoc( S0),
	related_linear_sys( Vs, S0,Sys),
	related_linear_vars( Sys, All, []).

related_linear_sys( [],     S0,L0) :-
	assoc_to_list( S0, L0).
related_linear_sys( [V|Vs], S0,S2) :-
	( get_atts( V, class(C)) ->
	    put_assoc( C, S0, C, S1)
	;
	    S1 = S0
	),
	related_linear_sys( Vs, S1,S2).

related_linear_vars( []) --> [].
related_linear_vars( [S-_|Ss]) -->
	{
	    class_allvars( S, Otl)
	},
	cpvars( Otl),
	related_linear_vars( Ss).

cpvars( Xs) --> {var(Xs)}, !.
cpvars( [X|Xs]) -->
	( {var(X)} -> [X] ; [] ),
	cpvars( Xs).

nonlin_crux( All, Gss) :-
	collect_nonlin( All, Gs, []),		% destructive
	this_linear_solver( Solver),
	nonlin_strip( Gs, Solver, Gss).

nonlin_strip( [],          _,      []).
nonlin_strip( [M:What|Gs], Solver, Res) :-
	( M == Solver ->
	    ( What = {G} ->
	        Res = [G|Gss]
	    ;
		Res = [What|Gss]
	    )
	;
	    Res = Gss
	),
	nonlin_strip( Gs, Solver, Gss).

all_attribute_goals( []) --> [].
all_attribute_goals( [V|Vs]) -->
	dump_linear( V, toplevel),
	dump_nonzero( V, toplevel),
	all_attribute_goals( Vs).

end_of_file.
