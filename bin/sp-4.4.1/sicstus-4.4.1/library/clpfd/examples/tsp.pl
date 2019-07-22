/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Traveling Salesman Problem
 * Author    : Mats Carlsson
 */

:- module(tsp, [tsp/1]).
:- use_module(library(lists), [transpose/2]).
:- use_module(library(atts)).
:- use_module(library(clpfd)).
:- attribute costs/1.

verify_attributes(_, _, []).

tsp(chip) :-
	problem([[0,205,677,581,461,878,345],
	         [205,0,882,427,390,1105,540],
		 [677,882,0,619,316,201,470],
		 [581,427,619,0,412,592,570],
		 [461,390,316,412,0,517,190],
		 [878,1105,201,592,517,0,691],
		 [345,540,470,570,190,691,0]],
		Succ, Pred, Cost),
	append(Succ, Pred, All),
	labeling([minimize(Cost),variable(selector),value(enumerator)], All),
	writeq(Succ-Cost),
	nl.
tsp(ilog) :-
	problem([[2,4,4,1,9,2,4,4,1,9],
	         [2,9,5,5,5,2,9,5,5,5],
		 [1,5,2,3,3,1,5,2,3,3],
		 [2,6,8,9,5,2,6,8,9,5],
		 [3,7,1,6,4,3,7,1,6,4],
		 [1,2,4,1,7,1,2,4,1,7],
		 [3,5,2,7,6,3,5,2,7,6],
		 [2,7,9,5,5,2,7,9,5,5],
		 [3,9,7,3,4,3,9,7,3,4],
		 [4,1,5,9,2,4,1,5,9,2]],
		Succ, Pred, Cost),
	append(Succ, Pred, All),
	labeling([minimize(Cost),variable(selector),value(enumerator)], All),
	writeq(Succ-Cost),
	nl.

problem(Matrix, Succ, Pred, Cost) :-
	length(Matrix, N),
	length(Succ, N),
	length(Pred, N),
	domain(Succ, 1, N),
	domain(Pred, 1, N),
	assignment(Succ, Pred, [circuit(true),cost(Cost,Matrix)]),
	cost_sum(Matrix, 0, Succ),
	transpose(Matrix, Transpose),
	cost_sum(Transpose, 0, Pred).

cost_sum([], _, []).
cost_sum([Row|Mat], I, [X|Xs]) :-
	costs_and_values(Row, 0, I, List),
	keysort(List, Costs),
	put_atts(X, costs(Costs)),
	J is I+1,
	cost_sum(Mat, J, Xs).

costs_and_values([], _, _, []).
costs_and_values([_|Row], I, I, List) :- !,
	J is I+1,
	costs_and_values(Row, J, I, List).
costs_and_values([Cost|Row], I, K, [Cost-J|List]) :-
	J is I+1,
	costs_and_values(Row, J, K, List).

% [PM] 4.3.1 used as labeling variables/1 option.
:- public selector/3.

%% ff,max_regret variable choice
selector([V|Vars], X, Rest) :-
	fd_size(V, S),
	var_regret(V, R),
	selector(Vars, V, S, R, X, Rest).

selector([], X, _, _, X, []).
selector([V|Vars], V0, S0, R0, X, Rest) :-
	integer(V), !,
	selector(Vars, V0, S0, R0, X, Rest).
selector([V|Vars], V0, S0, R0, X, [Y|Rest]) :-
	fd_size(V, S),
	var_regret(V, R),
	(   S<S0 -> Y=V0, selector(Vars, V, S, R, X, Rest)
	;   S=S0, R>=R0 -> Y=V0, selector(Vars, V, S, R, X, Rest)
	;   Y=V, selector(Vars, V0, S0, R0, X, Rest)
        ).

% [PM] 4.3.1 used as labeling value/1 option.
:- public enumerator/4.

%% min cost value choice
enumerator(Var, _Rest, BB, BB1) :-
	fd_set(Var, Set),
	get_atts(Var, costs(CostMap0)),
	var_value_cost(CostMap0, Set, Value, _, CostMap),
	(   put_atts(Var, -costs(_)),
	    first_bound(BB, BB1),
	    Var #= Value
        ;   later_bound(BB, BB1),
	    Var #\= Value,
	    (integer(Var) -> true; put_atts(Var, costs(CostMap)))
        ).

var_regret(Var, Regret) :-
	fd_set(Var, Set),
	get_atts(Var, costs(CostMap0)),
	var_value_cost(CostMap0, Set, _, Cost1, CostMap1),
	var_value_cost(CostMap1, Set, _, Cost2, _),
	Regret is Cost2-Cost1.

var_value_cost([Cost-Value|Map], Set, Value, Cost, Map) :-
	fdset_member(Value, Set), !.
var_value_cost([_|Map0], Set, Value, Cost, Map) :-
	var_value_cost(Map0, Set, Value, Cost, Map).

