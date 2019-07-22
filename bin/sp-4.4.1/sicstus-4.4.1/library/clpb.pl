/* Copyright (C) 1991, Swedish Institute of Computer Science */

%   File       : clpb.pl
%   Author     : Mats Carlsson
%   Updated    : 21 October 1999
%   Purpose    : Unification solver for the Boolean ring a la CHIP 

:- module(clpb, [
	sat/1,
	taut/2,
	labeling/1
		]).

:- use_module(library(atts)).

:- use_module(library(lists), [
	reverse/2
			      ]).

:- attribute value/2.


/*
Available predicates:

sat(+EXPRESSION) :-
	EXPRESSION is a Boolean expression, the syntax of which is
	defined below.  The predicate checks the consistency of the
	expression w.r.t. the accumulated constraints, and, if the
	check succeeds, tells the constraint that the expression be
	true.

	If a variable X, occurring in the expression, is subsequently
	unified with some term T, this is treated as a shorthand for
	the constraint
		sat(X=:=T).

taut(+EXPRESSION, -TRUTH) :- 
	EXPRESSION is as above.  The predicate asks whether the
	expression is now entailed by the accumulated constraints
	(TRUTH=1), or whether its negation is entailed by the
	accumulated constraints (TRUTH=0).  Otherwise, it fails.

labeling(+VARIABLES) :-
	VARIABLES should be a list of variables.  The variables are
	instantiated to a list of 0s and 1s, in a way that satisfies
	any accumulated constraints.  Creates choicepoints only if
	necessary.

Boolean expressions are composed from the following operands: the
constants 0 and 1 (FALSE and TRUE), logical variables, and symbolic
constants, and from the following connectives:

	~ P	% negation (not)
	P * Q	% conjunction (and)
	P + Q	% disjunction (ior)
	P # Q	% antivalence (eor)
	P ^ Q	% there exists a P s.t. Q
	P =:= Q	% same as ~P # Q
	P =\= Q	% same as P # Q
	P =< Q	% same as ~P + Q
	P >= Q	% same as P + ~Q
	P < Q	% same as ~P * Q
	P > Q	% same as P * ~Q

	card(C,E) % where C is a list of integers or integer ranges,
                  % denoting a set of integers, and
		  % E is a list of Boolean expressions:
		  % the number of true expressions in E is
		  % a member of the set C

Symbolic constants (Prolog atoms) denote parametric values and can be
viewed as all-quantified variables whose quantifiers are placed
outside the entire expression.  They are useful for forcing certain
variables of an equation to be treated as input parameters.
*/

:- op(300, fy, [~]).
:- op(500, yfx, [#]).


%%% ATTRIBUTE HOOKS %%%

% the unification hook
verify_attributes(X, V, Goals) :-
	get_atts(X, value(G,M)),
	nonvar(G), !,				% "bound" variable
	truth_value(V),
	Goals = [sat(Y=:=V)],
	put_atts(Y, value(G,M)).
verify_attributes(X, V, Goals) :-
	get_atts(X, value(G,M)),
	var(G), !,				% "free" variable
	truth_value(V),
	(   var(V),
	    get_atts(V, value(G1,_))
	->  (   var(G1) -> Goals = []
	    ;   Goals = [sat(Y=:=V)], put_atts(Y, value(G,M))
	    )
	;   var(V)
	->  Goals = [], put_atts(V, value(G,M))
	;   Goals = []
	).
verify_attributes(_, _, []).


% the variable projection hook
project_attributes(QV, AV) :-
	vars_graphs(QV, 1, GraphMaps, Dic),
	vars_eqs(Dic, GraphMaps, -0x1ffffff, _, Dic, _, Eqs),
	(   foreach(V,AV)
	do  (var(V) -> put_atts(V, -value(_,_)); true)
	),
	bool1(Dic, Eqs).


% the attribute-as-goal hook
attribute_goal(X, Goal) :-
	get_atts(X, value(G,M)),
	nonvar(G),
	bdd_to_formula(G, F, M),
        (   F = [] -> Goal = (X=0)
        ;   F = [[]] -> Goal = (X=1)
        ;   F = [[]|F1] ->
	    Goal = sat(X=\=Ext),
	    poly_to_ext(F1, Ext)
        ;   Goal = sat(X=:=Ext),
	    poly_to_ext(F, Ext)
	).


%%% LABELING PRIMITIVE %%%

% see documentation above
labeling(Set) :-
	constrained_free(Set, ConstrainedSet, FreeSet),
	labeling_01(FreeSet),
	labeling1(ConstrainedSet).

labeling1([]).
labeling1([Var|Rest]) :-
	formula_to_bdd(Var, X, 1, [], _, Dic),
	(   integer(X) -> apply_unifier(Dic, [])
	;   bool1(Dic, [X])
	;   prolog:'$bdd_negate'(X, Y), bool1(Dic, [Y])
	),
	labeling(Rest).

constrained_free([], [], []).
constrained_free([V|Vs], Constrained, Free) :-
	var(V),
	get_atts(V, value(_,Graph)),
	nonvar(Graph), !,
        Constrained = [V|Constrained1],
	constrained_free(Vs, Constrained1, Free).
constrained_free([V|Vs], Constrained, [V|Free]) :-
	constrained_free(Vs, Constrained, Free).

% :- parallel labeling_01/1.
:- labeling_01/1 is nondet.
labeling_01([]).
labeling_01([X|Dic]) :- integer(X), !, labeling_01(Dic).
labeling_01([0|Dic]) :- labeling_01(Dic).
labeling_01([1|Dic]) :- labeling_01(Dic).


vars_graphs([], _, [], []).
vars_graphs([V|Vs], I, [value(G,M)|GMs], [V-Node|Dic]) :-
	(   get_atts(V, value(G,M)) -> put_atts(V, -value(_,_))
	;   true
	),
	prolog:'$bdd_build'(I, 1, 0, Node),
	J is I+1,
	vars_graphs(Vs, J, GMs, Dic).

vars_eqs([], _, Free, Free, Dic, Dic, []).
vars_eqs([_-PNode|Ns], [value(G,M)|GMs], Free0, Free, Dic0, Dic, Eqs0) :-
	(   var(G) ->
	    vars_eqs(Ns, GMs, Free0, Free, Dic0, Dic, Eqs0)
	;   Eqs0 = [Eq|Eqs],
	    prolog:'$bdd_negate'(PNode, NNode),
	    bdd_to_bdd(G, M, Node, Free0, Free1, Dic0, Dic1),
	    bdd_univ(Node, NNode, PNode, Eq),
	    vars_eqs(Ns, GMs, Free1, Free, Dic1, Dic, Eqs)
	).

% Translating from BDD to internal polynomial format.
% P*Q + ~P*R =:= P*Q # P*R # R.
bdd_to_formula(0, [], _) :- !.
bdd_to_formula(1, [[]], _) :- !.
bdd_to_formula(Graph, F, Dic) :-
	bdd_parts(Graph, V, Q, R, Dic),
	prolog:'$bdd_type'(Q, R, Type),
	bdd_to_formula(Type, V, Q, R, Dic, F).

bdd_to_formula(0, V, _, _, _,   [[V]]).
bdd_to_formula(1, V, _, _, _,   [[],[V]]).
bdd_to_formula(2, V, _, R, Dic, Formula) :-
	bdd_to_formula(R, R1, Dic),
	poly_mul_add(R1, V, R1, R2),
	poly_mul_add([[]], V, R2, Formula).
bdd_to_formula(3, V, _, R, Dic, Formula) :-
	bdd_to_formula(R, R1, Dic),
	poly_mul_add(R1, V, R1, Formula).
bdd_to_formula(4, V, Q, _, Dic, Formula) :-
	bdd_to_formula(Q, Q1, Dic),
	poly_mul_add(Q1, V, [], Formula).
bdd_to_formula(5, V, Q, _, Dic, Formula) :-
	bdd_to_formula(Q, Q1, Dic),
	poly_mul_add(Q1, V, [[],[V]], Formula).
bdd_to_formula(6, V, _, R, Dic, Formula) :-
	bdd_to_formula(R, R1, Dic),
	poly_mul_add([[]], V, R1, Formula).
bdd_to_formula(7, V, Q, R, Dic, Formula) :-
	bdd_to_formula(Q, Q1, Dic),
	bdd_to_formula(R, R1, Dic),
	poly_mul_add(R1, V, R1, VRxorR),
	poly_mul_add(Q1, V, VRxorR, Formula).

poly_mul_add([], _, Poly, Poly).
poly_mul_add([O|Os], V, [], [[V|O]|Poly]) :- !,
	poly_mul_add(Os, V, [], Poly).
poly_mul_add([O|Os], V, [N|Ns], Poly) :-
	VO = [V|O],
	compare(C, VO, N), 
	poly_mul_add(C, VO, Os, V, N, Ns, Poly).

poly_mul_add(<, O, [], _, N, Ns, [O,N|Ns]) :- !.
poly_mul_add(<, O1, [O|Os], V, N, Ns, [O1|Poly]) :-
	VO = [V|O],
	compare(C, VO, N), 
	poly_mul_add(C, VO, Os, V, N, Ns, Poly).
poly_mul_add(=, _, Os, V, _, Ns, Poly) :-
	poly_mul_add(Os, V, Ns, Poly).
poly_mul_add(>, O, Os, V, N, [], [N,O|Poly]) :- !,
	poly_mul_add(Os, V, [], Poly).
poly_mul_add(>, O, Os, V, N1, [N|Ns], [N1|Poly]) :-
	compare(C, O, N), 
	poly_mul_add(C, O, Os, V, N, Ns, Poly).


% Translating from internal to external polynomial format.
poly_to_ext(Poly, Ext) :-
	poly_tag_length(Poly, Poly1),
	keysort(Poly1, [_-[L|M]|Poly2]),
	mon_to_ext(M, L, Ext0),
	poly_to_ext(Poly2, Ext0, Ext).

poly_to_ext([], Ext, Ext).
poly_to_ext([_-[L|M]|Poly], Ext0, Ext) :-
	mon_to_ext(M, L, Ext1),
	poly_to_ext(Poly, Ext0#Ext1, Ext).

mon_to_ext([], Ext, Ext).
mon_to_ext([L|Mon], Ext0, Ext) :-
	mon_to_ext(Mon, Ext0*L, Ext).

poly_tag_length([], []).
poly_tag_length([M|Poly0], [J-M|Poly]) :-
	length(M, I),
	J is -I,
	poly_tag_length(Poly0, Poly).


%%% BACK SUBSTITUTION PHASE %%%

% Compose a set of variable substitutions into a total substitution.
substs_unifier([], Unifier, Unifier).
substs_unifier([V-Sub0|Substs], Unifier0, Unifier) :-
	bdd_subst_set(Unifier0, Sub0, Sub),
	(   prolog:'$bdd_parts'(Sub, V, 1, 0) -> Unifier1=Unifier0
	;   Unifier1=[V-Sub|Unifier0]
	),
	substs_unifier(Substs, Unifier1, Unifier).

% Apply a total substitution to a Var-Graph mapping.
apply_unifier(Dic0, Unifier) :-
	dic_keylist(Dic0, Dic1, Unifier),
	keysort(Dic1, Dic),
	tell_values(Dic, _).

dic_keylist([], [], _).
dic_keylist([V-G|Dic0], Dic1, Unifier) :-
	bdd_subst_set(Unifier, G, G1),
	(   integer(G1)
	->  Dic1=Dic,
	    /* put_atts(V, -value(_,_)), prevent verifying attributes */
	    V=G1
	;   arg(1, G1, I), integer(I)
	->  Dic1=[I-(V-G1)|Dic]
	;   Dic1=Dic,
	    put_atts(V, value(G1,[]))
	),
	dic_keylist(Dic0, Dic, Unifier).

tell_values([], []).
tell_values(Dic, Map0) :-
	Dic = [I-_|_],
	Map0 = [I-MapVar|_],
	tell_values(I, Dic, MapVar, Map0).

tell_values(I, [I-(V-G)|Dic], MapVar, Map) :- !,
	(   prolog:'$bdd_parts'(G, I, 1, 0)
	->  /* put_atts(MapVar, -value(_,_)), prevent verifying attributes */
	    MapVar = V
	    /* display('made it here'), ttynl */
	;   put_atts(V, value(G,Map))
	),
	tell_values(I, Dic, MapVar, Map).
tell_values(_, Dic, _, [_|Map]) :-
	tell_values(Dic, Map).


%%% TAUT PRIMITIVE %%%

taut(Expr, Value) :-
	on_exception(taut(Val), taut(Expr), true),
	Value = Val.

:- taut/1 is throwing.
taut(Expr) :-
	formula_expansion(Expr, Expr1),
	split_expr(Expr1, 1, Parts, []),
	formulas_to_bdds(Parts, Graphs, 1, [], _, _),
	merge_eqs(Graphs, [], Eqs),
	solve_eqs(Eqs, [], _), !,
	Eqs = [],
	raise_exception(taut(1)).
taut(_) :-
	raise_exception(taut(0)).


%%% SAT PRIMITIVE %%%

sat(Expr) :-
	formula_expansion(Expr, Expr1),
	split_expr(Expr1, 1, Parts, []),
	formulas_to_bdds(Parts, Graphs, 1, [], _, Dic),
	bool1(Dic, Graphs).


bool1(Dic, Graphs) :-
	merge_eqs(Graphs, [], Eqs),
	solve_eqs(Eqs, [], Substs),
	prolog:'$bdd_subst_reset',
	substs_unifier(Substs, [], Unifier),
	apply_unifier(Dic, Unifier).

% Translate formula to a set of graphs, each of which is to be
% bool-unified with 0.
split_expr(P,     B) --> {var(P)}, !,
	[P1],
	{signed(B, P, P1)}.
split_expr(P*Q,   1) --> !,
	split_expr(P, 1),
	split_expr(Q, 1).
split_expr(P+Q,   0) --> !,
	split_expr(P, 0),
	split_expr(Q, 0).
split_expr(P#Q,   B) --> {Q==1}, !,
	{B1 is 1-B},
	split_expr(P, B1).
split_expr(P,     B) -->
	[P1],
	{signed(B, P, P1)}.

signed(0, P, P).
signed(1, P, P#1).

formulas_to_bdds([], [], F, Dic, F, Dic).
formulas_to_bdds([E|Es], [G|Gs], F0, Dic0, F, Dic) :-
	formula_to_bdd(E, G, F0, Dic0, F1, Dic1),
	formulas_to_bdds(Es, Gs, F1, Dic1, F, Dic).


% Translate Graph-Map to a new Graph in current context.
bdd_to_bdd(Graph0, Map, Graph, Free0, Free, Dic0, Dic) :-
	bdd_map_to_unifier(Map, Unifier, Free0, Free, Dic0, Dic),
	prolog:'$bdd_subst_reset',
	bdd_subst_set(Unifier, Graph0, Graph).

bdd_map_to_unifier([], [], Free, Free, Dic, Dic).
bdd_map_to_unifier([V-Formula|Map], Unifier0, Free0, Free, Dic0, Dic) :-
	formula_expansion(Formula, Formula1),
	formula_to_bdd(Formula1, Graph, Free0, Dic0, Free1, Dic1),
	(   prolog:'$bdd_parts'(Graph, V, 1, 0) -> Unifier0=Unifier
	;   Unifier0=[V-Graph|Unifier]
	),
	bdd_map_to_unifier(Map, Unifier, Free1, Free, Dic1, Dic).

% Guts of Formula to Graph.  Variables are dereferenced eagerly.
formula_to_bdd(F, Graph, Free0, Dic0, Free, Dic) :-
	var(F), !,
	(   get_atts(F, value(G1,M1)) -> true
	;   put_atts(F, value(G1,M1))
	),
	(   nonvar(G1)
	->  put_atts(F, value(_,_)),
	    bdd_to_bdd(G1, M1, Graph, Free0, Free, Dic0, Dic1),
	    (   integer(Graph)
	    ->  Dic=Dic1,
	        /* put_atts(F, -value(_,_)), prevent verifying attributes */
		F=Graph
	    ;   dic_insert(Dic1, F, Graph, Dic)
	    )
	;   dic_lookup(Dic0, F, Item) ->
	    Item = Graph,
	    Dic = Dic0,
	    Free = Free0
	;   dic_insert(Dic0, F, Graph, Dic),
	    Free is Free0+1,
	    prolog:'$bdd_build'(Free0, 1, 0, Graph)
	).
formula_to_bdd(F, Graph, Free0, Dic0, Free, Dic) :-
	atom(F), !,
        Free = Free0,
        Dic = Dic0,
	prolog:'$bdd_build'(F, 1, 0, Graph).
formula_to_bdd(0, 0, Free, Dic, Free, Dic).
formula_to_bdd(1, 1, Free, Dic, Free, Dic).
formula_to_bdd(F1*F2, P, Free0, Dic0, Free, Dic) :-
	formula_to_bdd(F1, P1, Free0, Dic0, Free1, Dic1),
	formula_to_bdd(F2, P2, Free1, Dic1, Free, Dic),
	bdd_univ(P1, P2, 0, P).
formula_to_bdd(F1+F2, P, Free0, Dic0, Free, Dic) :-
	formula_to_bdd(F1, P1, Free0, Dic0, Free1, Dic1),
	formula_to_bdd(F2, P2, Free1, Dic1, Free, Dic),
	bdd_univ(P1, 1, P2, P).
formula_to_bdd(F1#F2, P, Free0, Dic0, Free, Dic) :- F2==1, !,
	formula_to_bdd(F1, P1, Free0, Dic0, Free, Dic),
	prolog:'$bdd_negate'(P1, P).
formula_to_bdd(F1#F2, P, Free0, Dic0, Free, Dic) :-
	formula_to_bdd(F1, P1, Free0, Dic0, Free1, Dic1),
	formula_to_bdd(F2, P2, Free1, Dic1, Free, Dic),
	prolog:'$bdd_negate'(P2, N2),
	bdd_univ(P1, N2, P2, P).
formula_to_bdd(Var^F2, P, Free0, Dic0, Free, Dic) :-
	var(Var),
	formula_to_bdd(F2, P1, Free0, Dic0, Free, Dic),
	(   dic_lookup(Dic, Var, Graph) ->
	    prolog:'$bdd_parts'(Graph, V, 1, 0)
	;   true
	),
	(   integer(V),
	    prolog:'$bdd_build'(-0x2000000, 1, 0, Node),
	    prolog:'$bdd_subst_reset',
	    bdd_subst_set([V-Node], P1, P2),
	    prolog:'$bdd_parts'(P2, -0x2000000, Q, R) ->
	    bdd_univ(Q, 1, R, P)
	;   P = P1
	).
formula_to_bdd(card(Cset,List), P, Free0, Dic0, Free, Dic) :-
	card_cset(Cset, 0, Cset0),
	formulas_to_keylist(List, Cset0, Cset1, Free0, Dic0, Free, Dic,
	                    List1, []),
	keysort(List1, List2),
	length(List2, N),
	reverse(List2, List3),
	Cset2 is Cset1/\((2<<N)-1),
	card_to_bdd(Cset2, N, List3, P).

formula_expansion(P,     P) :- truth_value(P), !.
formula_expansion([P|Q], [P1|Q1]) :-
	formula_expansion(P, P1),
	formula_expansion(Q, Q1).
formula_expansion(~P,    P1#1) :-
	formula_expansion(P, P1).
formula_expansion(P=:=Q, (P1#Q1)#1) :-
	formula_expansion(P, P1),
	formula_expansion(Q, Q1).
formula_expansion(P*Q,   P1*Q1) :-
	formula_expansion(P, P1),
	formula_expansion(Q, Q1).
formula_expansion(P+Q,   P1+Q1) :-
	formula_expansion(P, P1),
	formula_expansion(Q, Q1).
formula_expansion(P#Q,   P1#Q1) :-
	formula_expansion(P, P1),
	formula_expansion(Q, Q1).
formula_expansion(P=\=Q, (P1#Q1)) :-
	formula_expansion(P, P1),
	formula_expansion(Q, Q1).
formula_expansion(P=<Q,  (P1#1)+Q1) :-
	formula_expansion(P, P1),
	formula_expansion(Q, Q1).
formula_expansion(P>=Q,  P1+(Q1#1)) :-
	formula_expansion(P, P1),
	formula_expansion(Q, Q1).
formula_expansion(P<Q,   (P1#1)*Q1) :-
	formula_expansion(P, P1),
	formula_expansion(Q, Q1).
formula_expansion(P>Q,   P1*(Q1#1)) :-
	formula_expansion(P, P1),
	formula_expansion(Q, Q1).
formula_expansion(P^Q,   P^Q1) :-
	formula_expansion(Q, Q1).
formula_expansion(card(C,E), card(C,E1)) :-
	formula_expansion(E, E1).

truth_value(A) :- var(A), !.
truth_value(0) :- !.
truth_value(1) :- !.
truth_value(A) :- atom(A).


%%% SUPPORT FOR CARD CONSTRAINTS %%%

card_cset([], C, C).
card_cset([I|Cset], C0, C) :-
	(   integer(I) ->
	    C1 is C0\/(1<<I)
	;   I = J-K,
	    integer(J),
	    integer(K),
	    card_cset(J, K, C0, C1)
	),
	card_cset(Cset, C1, C).

card_cset(J, K, C, C) :- J>K, !.
card_cset(I, K, C0, C) :-
	C1 is C0\/(1<<I),
	J is I+1,
	card_cset(J, K, C1, C).

formulas_to_keylist([], Cset, Cset, Free, Dic, Free, Dic) --> [].
formulas_to_keylist([X|Xs], Cset0, Cset, Free0, Dic0, Free, Dic) -->
	{formula_to_bdd(X, Y, Free0, Dic0, Free1, Dic1)},
	(   {Y=1} -> {Cset1 is Cset0>>1}
	;   {Y=0} -> {Cset1=Cset0}
	;   [A-Y], {arg(1, Y, A), Cset1=Cset0}
	),
	formulas_to_keylist(Xs, Cset1, Cset, Free1, Dic1, Free, Dic).

card_to_bdd(0, _, _, 0) :- !.
card_to_bdd(Cset, N, _, 1) :- Cset is (2<<N)-1, !.
card_to_bdd(1, _, List, P) :- !,
	card_disj(List, 0, P1),
	prolog:'$bdd_negate'(P1, P).
card_to_bdd(Cset, N, List, P) :- Cset is (2<<N)-2, !,
	card_disj(List, 0, P).
card_to_bdd(Cset, N, List, P) :- Cset is 1<<N, !,
	card_conj(List, 1, P).
card_to_bdd(Cset, N, List, P) :- Cset is (1<<N)-1, !,
	card_conj(List, 1, P1),
	prolog:'$bdd_negate'(P1, P).
card_to_bdd(Cset, N, List, P) :-
	card_values(N, Cset, L),
	card_to_bdd(List, L, P).

card_disj([], P, P).
card_disj([_-X|Xs], P0, P) :- bdd_univ(X, 1, P0, P1), card_disj(Xs, P1, P).

card_conj([], P, P).
card_conj([_-X|Xs], P0, P) :- bdd_univ(X, P0, 0, P1), card_conj(Xs, P1, P).

card_values(-1, _, []) :- !.
card_values(I, Cset0, [X|Xs]) :-
	X is Cset0/\1,
	Cset is Cset0>>1,
	J is I-1,
	card_values(J, Cset, Xs).

card_to_bdd([], [P], P).
card_to_bdd([_-X|Xs], [N|Ns], P) :-
	card_values(Ns, N, X, Ms),
	card_to_bdd(Xs, Ms, P).

card_values([], _, _, []).
card_values([N1|Ns], N0, X, [M|Ms]) :-
	bdd_univ(X, N1, N0, M),
	card_values(Ns, N1, X, Ms).

%%% VARIABLE ELIMINATION PHASE %%%

solve_eqs([], Substs, Substs).
solve_eqs([V-Eq|OldEqs], Substs0, Substs) :-
	(   V<0 -> Substs1=Substs0
	;   Substs1=[V-Sub|Substs0]
	),
	prolog:'$bdd_parts'(Eq, V, Q, R),
	prolog:'$bdd_type'(Q, R, Type),
	solve(Type, V, Q, R, Sub, Residue),
	merge_eq(Residue, OldEqs, NewEqs),
	solve_eqs(NewEqs, Substs1, Substs).


solve(0, _, _, _, 0, 0).			% x=0
solve(1, _, _, _, 1, 0).			% ~x=0
solve(2, _, _, R, 0, R).			% x+R=0
solve(3, V, _, R, Sub, 0) :-		% ~x*R=0
	prolog:'$bdd_build'(V, 1, R, Sub).
solve(4, V, Q, _, Sub, 0) :-		% x*Q=0
	prolog:'$bdd_negate'(Q, IQ),
	prolog:'$bdd_build'(V, IQ, 0, Sub).
solve(5, _, Q, _, 1, Q).			% ~x+Q=0
solve(6, _, _, R, R, 0).			% x#R=0
solve(7, V, Q, R, Sub, Residue) :-		% if(x,Q,R)=0
	prolog:'$bdd_negate'(Q, IQ),
	prolog:'$bdd_build'(V, IQ, R, Sub),
	bdd_univ(Q, R, 0, Residue).

merge_eqs([], Eqs, Eqs).
merge_eqs([X|Xs], Eqs0, Eqs) :-
	merge_eq(X, Eqs0, Eqs1),
	merge_eqs(Xs, Eqs1, Eqs).

merge_eq(0, Eqs, Eqs) :- !.
merge_eq(Graph, Eqs0, Eqs) :-
	compound(Graph),	% [MC] 4.1.0
	arg(1, Graph, Var),
	integer(Var),
	merge_eq(Eqs0, Var, Graph, Eqs).

merge_eq([], Var, Graph, [Var-Graph]).
merge_eq([Var0-Graph0|Eqs0], Var, Graph, Eqs) :-
	compare(C, Var0, Var),
	merge_eq(C, Var0, Graph0, Eqs0, Var, Graph, Eqs).

merge_eq(<, Var0, Graph0, Eqs0, Var, Graph, [Var0-Graph0|Eqs]) :-
	merge_eq(Eqs0, Var, Graph, Eqs).
merge_eq(=, Var0, Graph0, Eqs0, _, Graph1, Eqs) :-
	prolog:'$bdd_parts'(Graph0, Var0, Q0, R0),
	prolog:'$bdd_parts'(Graph1, Var0, Q1, R1),
	bdd_univ(Q0, 1, Q1, Q),
	bdd_univ(R0, 1, R1, R),
	prolog:'$bdd_build'(Var0, Q, R, Graph),
	merge_eq(Graph, Eqs0, Eqs).
merge_eq(>, Var0, Graph0, Eqs0, Var, Graph, [Var-Graph,Var0-Graph0|Eqs0]).


%%% GRAPH MANIPULATION PRIMITIVES %%%

bdd_parts(Graph, V1, Q, R, Dic) :-
	prolog:'$bdd_parts'(Graph, V, Q, R),
	(   atom(V) -> V1=V
	;   dic_lookup(Dic, V, V1)
	).

% The "if-then-else" function.
bdd_univ(F, G, H, Val) :-
	prolog:'$bdd_univ_case'(F, G, H, C, X, Y, Z),
	bdd_univ(C, X, Y, Z, Val).

bdd_univ(1, F, _, _, F).			% F
bdd_univ(11, F, G, H, Val) :-			% F*G + ~F*H, F on top
	prolog:'$bdd_parts'(F, V, FQ, FR),
	bdd_univ(FQ, FR, G, G, H, H, V, Val),
	prolog:'$bdd_store'(F, G, H, Val).
bdd_univ(12, F, G, H, Val) :-			% F*G + ~F*H, G on top
	prolog:'$bdd_parts'(G, V, GQ, GR),
	bdd_univ(F, F, GQ, GR, H, H, V, Val),
	prolog:'$bdd_store'(F, G, H, Val).
bdd_univ(13, F, G, H, Val) :-			% F*G + ~F*H, H on top
	prolog:'$bdd_parts'(H, V, HQ, HR),
	bdd_univ(F, F, G, G, HQ, HR, V, Val),
	prolog:'$bdd_store'(F, G, H, Val).
bdd_univ(14, F, G, H, Val) :-			% F*G + ~F*H, FG on top
	prolog:'$bdd_parts'(F, V, FQ, FR),
	prolog:'$bdd_parts'(G, V, GQ, GR),
	bdd_univ(FQ, FR, GQ, GR, H, H, V, Val),
	prolog:'$bdd_store'(F, G, H, Val).
bdd_univ(15, F, G, H, Val) :-			% F*G + ~F*H, FH on top
	prolog:'$bdd_parts'(F, V, FQ, FR),
	prolog:'$bdd_parts'(H, V, HQ, HR),
	bdd_univ(FQ, FR, G, G, HQ, HR, V, Val),
	prolog:'$bdd_store'(F, G, H, Val).
bdd_univ(16, F, G, H, Val) :-			% F*G + ~F*H, GH on top
	prolog:'$bdd_parts'(G, V, GQ, GR),
	prolog:'$bdd_parts'(H, V, HQ, HR),
	bdd_univ(F, F, GQ, GR, HQ, HR, V, Val),
	prolog:'$bdd_store'(F, G, H, Val).
bdd_univ(17, F, G, H, Val) :-			% F*G + ~F*H, FGH on top
	prolog:'$bdd_parts'(F, V, FQ, FR),
	prolog:'$bdd_parts'(G, V, GQ, GR),
	prolog:'$bdd_parts'(H, V, HQ, HR),
	bdd_univ(FQ, FR, GQ, GR, HQ, HR, V, Val),
	prolog:'$bdd_store'(F, G, H, Val).

bdd_univ(FQ, FR, GQ, GR, HQ, HR, V, Val) :-
	bdd_univ(FQ, GQ, HQ, Q),
	bdd_univ(FR, GR, HR, R),
	prolog:'$bdd_build'(V, Q, R, Val).


% bdd_subst_set(VarsGs, F, Result) :- Result is F with Vars replaced by Gs
bdd_subst_set(Unifier, In, Out) :-
	prolog:'$bdd_subst_case'(Unifier, In, C, Unifier1, AbsIn, T, Q, R),
	bdd_subst_case(C, Unifier1, AbsIn, T, Q, R, Out).

bdd_subst_case(0, _, Out, _, _, _, Out).
bdd_subst_case(1, _, Out1, _, _, _, Out) :-
	prolog:'$bdd_negate'(Out1, Out).
bdd_subst_case(10, Unifier, In, T, Q, R, Out) :- % normal recursion
	bdd_subst_set(Unifier, Q, Q1),
	bdd_subst_set(Unifier, R, R1),
	bdd_univ(T, Q1, R1, Out),
	prolog:'$bdd_subst_store'(In, Out).
bdd_subst_case(11, Unifier, In, T, Q, R, Out) :- % recurse and negate
	bdd_subst_set(Unifier, Q, Q1),
	bdd_subst_set(Unifier, R, R1),
	bdd_univ(T, Q1, R1, Out1),
	prolog:'$bdd_subst_store'(In, Out1),
	prolog:'$bdd_negate'(Out1, Out).
bdd_subst_case(20, Unifier, In, T, Q, _, Out) :- % Q=\=R, recurse
	bdd_subst_set(Unifier, Q, Q1),
	prolog:'$bdd_negate'(Q1, R1),
	bdd_univ(T, Q1, R1, Out),
	prolog:'$bdd_subst_store'(In, Out).
bdd_subst_case(21, Unifier, In, T, Q, _, Out) :- % Q=\=R, recurse and negate
	bdd_subst_set(Unifier, Q, Q1),
	prolog:'$bdd_negate'(Q1, R1),
	bdd_univ(T, Q1, R1, Out1),
	prolog:'$bdd_subst_store'(In, Out1),
	prolog:'$bdd_negate'(Out1, Out).

%%% SYMBOL TABLE UTILITIES %%%

% attribute juggling destroys order

% add a Key-Value pair to an unordered list
% Duplicates are allowed (oldest first).
dic_insert([], Key, Value, [Key-Value]).
dic_insert([Item|Dic0], Key, Value, Dic) :-
	Item = Key0-_,
	(   Key0\==Key 
	->  Dic = [Item|Dic1], dic_insert(Dic0, Key, Value, Dic1)
	;   Dic = [Item,Key-Value|Dic0]
	).

% given a key, find its value in an unordered list list.
dic_lookup([Item|Dic], Key, Value) :-
	Item = Key0-Value0,
	(   Key0\==Key -> dic_lookup(Dic, Key, Value)
	;   Value = Value0
	).

end_of_file.

% add a Key-Value pair to a keysorted list.
% Duplicates are allowed (oldest first).
dic_insert([], Key, Value, [Key-Value]).
dic_insert([Item|Dic0], Key, Value, Dic) :-
	Item = Key0-_,
	compare(C, Key0, Key),
	dic_insert(C, Item, Dic0, Key, Value, Dic).

dic_insert(<, Item, Dic0, Key, Value, [Item|Dic]) :-
	dic_insert(Dic0, Key, Value, Dic).
dic_insert(=, Item, Dic, Key, Value, [Item,Key-Value|Dic]).
dic_insert(>, Item, Dic, Key, Value, [Key-Value,Item|Dic]).

% given a key, find its value in a keysorted list.
dic_lookup([Key0-Value0|Dic], Key, Value) :-
	compare(C, Key0, Key),
	dic_lookup(C, Key0, Value0, Dic, Key, Value).

dic_lookup(<, _, _, [Key0-Value0|Dic], Key, Value) :-
	compare(C, Key0, Key),
	dic_lookup(C, Key0, Value0, Dic, Key, Value).
dic_lookup(=, Key, Value, _, Key, Value).
