/* Copyright(C) 1999, Swedish Institute of Computer Science */

% Consistency rules:
% VARS(DAG) = VARS(Template)
% VARS(Tuples) \disjoint VARS(Template)
% every ID integer and unique
% all paths complete

case(Template, Tuples, Dag) :-
	case(Template, Tuples, Dag, [], case(Template,Tuples,Dag)).

case(Template, Tuples, Dag, Options) :-
	case(Template, Tuples, Dag, Options, case(Template,Tuples,Dag,Options)).

% A "source" is in fact an interval B..E
case([], _, [], _, _) :- !.	% nullary case :-)
case(Template, Tuples, Dag1, Options, Posted) :-
	(   foreach(node(ID,Var,Edges1),Dag1),
	    foreach(node(ID,Var,Edges2),Dag2)
	do  (   foreach(Edge1,Edges1),
		foreach(edge(Source,Linles,Target),Edges2)
	    do  (   Edge1 = Source-Linles-Target
		->  true
		;   Edge1 = Source-Target,
		    simple(Target)
		->  Linles = []
		;   Edge1 = Source-Linles
		->  Target = []
		;   Edge1 = Source, Source = (_.._)
		->  Linles = [],
		    Target = []
		;   Edge1 = Linles
		->  Source = [],
		    Target = []
		)
	    )
	),
	(   foreach(O,Options),
	    fromto(SCPs,SCPs1,SCPs2,[])
	do  (   O = scalar_product(_,_,_,_)
	    ->  SCPs1 = [O|SCPs2]
	    ;   SCPs1 = SCPs2
	    )
	),
	mddi(Template, Tuples, Dag2, SCPs, Posted).

mddi(Template, Tuples, Dag, SCPs, Posted) :-
	Goal = mddi(Template,Tuples,Dag,Posted),
	prolog:term_variables_dfs(Template, Vars), % prolog:term_variables_set would NOT work
	sort(Vars, TemplateVars),
	prolog:term_variables_set(Dag, DagVars),
	prolog:term_variables_set(Tuples, TuplesVars),
	(   DagVars==TemplateVars -> true
	;   illarg(consistency(Template,Dag,''), Posted, 3)
	),
	(   ord_disjoint(TuplesVars, TemplateVars) -> true
	;   illarg(consistency(Template,Tuples,''), Posted, 2)
	),
	(   mddi_compile(Dag, Vars, Nodes, Edges, VarVals, Linles2, Sets, Posted) -> true
	;   illarg(consistency(Template,Dag,inconsistent_paths), Posted, 3)
	),
	mddi_scps(SCPs, Vars, -1, Linles1, Linles2),
	sort(Linles1, Linles),	% by increasing edge index
	mddi_post(Tuples, Template, Vars, Nodes, Edges, VarVals, Linles, Sets, Goal, Posted).
	
mddi_post(Tuples1, Template, Vars, Nodes, Edges, VarVals, Linles, Sets, Goal, Posted) :-
	length(Vars, NVars),
	(Linles = [] -> Flag = 0 ; Flag = 4), % idempotent or not
	length(Tuples1, NT),
	'$fd_mddi_common'(NVars, Nodes, Edges, VarVals, Linles, NT, Common), % Common = state([_ | '$free'(Ptr)], 0)
	(   foreach(Row1,Tuples1),
	    param(Template,Vars,Sets,Goal,Common,Posted,Flag)
	do  copy_term(Template-Vars, Row1-Row2),
	    (   foreach(X,Row2),
		foreach(X-XA,Row3),
		foreach(none(X),Susp),
		foreach(Set,Sets),
		param(Goal)
	    do  arg_attribute(X, XA, Goal, 0),
		prune_and_propagate(X, Set)
	    ),
	    fd_global_internal(Goal,
			       state(Row3,Common, 0/*trail_top*/,_Handle,0),
			       Susp, _, clpfd:Posted, Flag)
	).

mddi_compile(Dag, Vars, Nodes, Edges, VarVals, Linles, Sets, Goal) :-
	empty_avl(ID2Index0),
	mddi_map_nodes(Dag, Nodes, Pivots0, 0, ID2Index0, ID2Index, Vars, Goal),
	sort(Pivots0, Pivots1),
	keyclumped(Pivots1, Pivots2),
	compensate_for_inf(Pivots2, Pivots3),
	ord_list_to_avl(Pivots3, Node2P),
	mddi_map_rest(Dag, Nodes, Edges1, VarVals1, 0, _, ID2Index, Node2P, Goal),
	keysort(VarVals1, VarVals2),
	keyclumped(VarVals2, VarVals3),
	(   foreach(varval(Var,_,B,E)-Cl,VarVals3),
	    foreach(varval(Var,B,E),VarVals),
	    foreach(Var-(B..E),KL1),
	    count(Ix,0,_)
	do  (   foreach(Ix,Cl),
		param(Ix)
	    do  true
	    )
	),
	keysort(Edges1, Edges2), % sort by inc. varval
	(   foreach(_-Edge2,Edges2),
	    foreach(Edge,Edges),
	    fromto(Linles,Linles1,Linles2,[]),
	    count(EdgeIx,0,_),
	    param(Vars)
	do  Edge2 = edge(Source,SCPs,Dest,U),
	    Edge = edge(Source,Dest,U),
	    mddi_scps(SCPs, Vars, EdgeIx, Linles1, Linles2)
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(_-Vals,KL3),
	    foreach(Set,Sets)
	do  (   foreach(R1,Vals),
		foreach(S1,SetParts)
	    do  range_to_fdset(R1, S1)
	    ),
	    fdset_union(SetParts, Set)
	).

mddi_scps(SCPs, Vars, EdgeIx) -->
	(   foreach(scalar_product(Cs,Xs,#=<,RHS),SCPs),
	    param(Vars,EdgeIx)
	do  [linle(EdgeIx,LHS,RHS)],
	    (   foreach(T1,Vars),
		foreach(C,LHS),
		param(Cs,Xs)
	    do  {   nth0(J, Xs, T2),
		    T1==T2
		->  nth0(J, Cs, C)
		;   C = 0
		}
	    )
	).

compensate_for_inf(KL1, KL2) :-
	(   foreach(N-Cl1,KL1),
	    foreach(N-Cl3,KL2)
	do  (   select(inf, Cl1, Cl2) -> Cl3 = slow([inf|Cl2])
	    ;   member(sup, Cl1) -> Cl3 = slow(Cl1)
	    ;   Cl3 = fast(Cl1)
	    )
	).

mddi_map_nodes([], [], [], _, A, A, _, _).
mddi_map_nodes([node(ID,Var,SymEdges)|Nodes], [N|Ns], Pivots0, I, A0, A, Vars, Goal) :-
	must_be(ID, integer, Goal, 0),
	var_nth(Var, Vars, 0, N),
	avl_store(ID, A0, I-N, A1),
	J is I+1,
	(   foreach(edge(B..E,_,_),SymEdges),
	    fromto(Pivots0,[N-B,N-E1|Pivots1],Pivots1,Pivots),
	    param(N)
	do  fdinc(E, E1, 1)
	),
	mddi_map_nodes(Nodes, Ns, Pivots, J, A1, A, Vars, Goal).

mddi_map_rest([], [], [], [], Bot, Bot, _, _, _).
mddi_map_rest([node(_,_,SymEdges)|Nodes], [N|Ns], Edges0, VarVals0, Source, Bot, ID2Index, Node2P, Goal) :-
	(   foreach(SymEdge,SymEdges),
	    fromto(VarVals0,VarVals1,VarVals3,VarVals4),
	    fromto(Edges0,Edges1,Edges3,Edges4),
	    param(N,Source,Bot,ID2Index,Node2P)
	do  (   SymEdge = edge(Key,Linles,[]) -> Dest = Bot
	    ;   SymEdge = edge(Key,Linles,Y) ->  avl_fetch(Y, ID2Index, Dest-_)
	    ),
	    avl_fetch(N, Node2P, Pivots),
	    mddi_fragment(Pivots, Key, Vals),
	    (   foreach(B..E,Vals),
		fromto(VarVals1,[varval(N,Aux,B,E)-U|VarVals2],VarVals2,VarVals3),
		fromto(Edges1,[U-edge(Source,Linles,Dest,U)|Edges2],Edges2,Edges3),
		param(N,Source,Linles,Dest)
	    do  (integer(B) -> Aux = B ; Aux = 0.0)
	    )
	),
	Source1 is Source+1,
	mddi_map_rest(Nodes, Ns, Edges4, VarVals4, Source1, Bot, ID2Index, Node2P, Goal).

mddi_fragment(_, Min..Min, [Min..Min]) :- !.
mddi_fragment(fast(Pivots), Min..Max, Vals) :-
	mddi_fragment_fast(Pivots, Min, Max, Vals, []).
mddi_fragment(slow(Pivots), Min..Max, Vals) :-
	mddi_fragment(Pivots, Min, Max, Vals, []).

mddi_fragment([P|Ps], Min, Max) --> {\+le(Min,P)}, !,
	mddi_fragment(Ps, Min, Max).
mddi_fragment([P,Q|Ps], Min, Max) --> {le(P,Max)}, !, [P..R],
	{fdinc(Q, R, -1)},
	mddi_fragment([Q|Ps], Min, Max).
mddi_fragment(_, _, _) --> [].

mddi_fragment_fast([P|Ps], Min, Max) --> {Min>P}, !,
	mddi_fragment_fast(Ps, Min, Max).
mddi_fragment_fast([P,Q|Ps], Min, Max) --> {P=<Max}, !, [P..R],
	{R is Q-1},
	mddi_fragment_fast([Q|Ps], Min, Max).
mddi_fragment_fast(_, _, _) --> [].

fdinc(inf, inf, _) :- !.
fdinc(sup, sup, _) :- !.
fdinc(X, Y, C) :-
	Y is X+C.

table(Tuples, Extension1) :-
	table(Tuples, Extension1, [], table(Tuples, Extension1)).

table(Tuples, Extension1, Options) :-
	table(Tuples, Extension1, Options, table(Tuples, Extension1, Options)).

table(Tuples, Extension1, Options, Goal) :-
	must_be(Options, proper_list(callable), Goal, 2),
	(   foreach(Opt,Options),
	    fromto(opt(leftmost,default,_),Opt0,Opt1,opt(Order,Method,NbNodes)),
	    param(Goal)
	do  (   table_option(Opt, Opt0, Opt1) -> true
	    ;   illarg(domain(term,table_option), Goal, 2, Opt)
	    )
	),
	table_order(Order, Tuples, Tuplesa, Extension1, Extension1a),
	table_augment(Method, Tuplesa, Tuplesb, Extension1a, Extension1b),
	Tuplesb = [Tuple|_],
	length(Tuple, N),
	(   foreach(Row1,Extension1b),
	    fromto(Extension2,Extension3,Extension4,[]),
	    param(Goal)
	do  (   foreach(X,Row1),
		foreach(Y,Row2),
		param(Goal)
	    do  (   integer(X) ->
		    set_expression_check({X}, Y, Goal, 2)
		;   set_expression_check(X, Y, Goal, 2)
		)
	    ),
	    (   member([], Row2) ->
		Extension3 = Extension4
	    ;   Extension3 = [Row2|Extension4]
	    )
	),
	(   Method\==default
	->  table_case(Tuplesb, Extension2, N, NbNodes, Goal)
	;   N=:=0 -> Extension2\==[]
	;   N=:=1
	->  append(Extension2, List),
	    fdset_union(List, FD),
	    append(Tuplesb, Vars),
	    domain(Vars, FD)
	;   N=:=2
	->  table_binary(Tuplesb, Extension2, Goal)
	;   table_compact(Tuplesb, Extension2, N, NbNodes, Goal)
	).

table_option(order(Order), opt(_,Method,NbNodes), opt(Order,Method,NbNodes)) :-
	memberchk(Order, [leftmost,id3]).
table_option(method(Method), opt(Order,_,NbNodes), opt(Order,Method,NbNodes)) :-
	memberchk(Method, [default,noaux,aux]).

table_order(leftmost, Ts, Ts, Rs, Rs).
table_order(id3, Ts0, Ts, Rs0, Rs) :-
	transpose(Rs0, Cols0),
	(   foreach(Col,Cols0),
	    foreach(Rank-Var,Rank1),
	    foreach(Var,Perm1)
	do  sort(Col, Set),
	    length(Set, Len),
	    Rank is -Len
	),
	keysort(Rank1, Rank2),
	(   foreach(_-Var2,Rank2),
	    foreach(Var2,Perm2)
	do  true
	),
	(   foreach(X1,[Cols0|Ts0]),
	    foreach(X2,[Cols|Ts]),
	    param(Perm1,Perm2)
	do  copy_term(Perm1-Perm2, X1-X2)
	),
	transpose(Cols, Rs).

table_augment(default, Ts, Ts, Rs, Rs).
table_augment(noaux, Ts, Ts, Rs, Rs).
table_augment(aux, Ts0, Ts, Rs0, Rs) :-
	(   foreach(T,Ts0),
	    foreach([_|T],Ts)
	do  true
	),
	(   foreach(R,Rs0),
	    foreach([J|R],Rs),
	    count(J,1,_)
	do  true
	).

% use mddi propagator
table_case(Tuples, Extension, N, NbNodes, Goal) :- !,
	length(Template, N),
	table_general(Extension, Dag, Template),
	length(Dag, NbNodes),
	case(Template, Tuples, Dag, [], Goal).

table_general(Extension, Dag, Template) :-
	Extension\==[],
	empty_avl(Empty),
	length(Template, N),
	table_tree(N, Extension, Tree, Empty, _),
	tree_to_dag(Tree, _ID, 0, Empty, A2),
	avl_to_list(A2, L1),
	dag_nodes(L1, Dag, Template, 0).

dag_nodes([], [], _, _).
dag_nodes([(D-_)-(J-Children)|L1], [node(J,Var,Children)|L2], Template, J) :-
	nth0(D, Template, Var),
	K is J+1,
	dag_nodes(L1, L2, Template, K).

tree_to_dag([], [], _, A, A) :- !.
tree_to_dag(Children, ID, Depth, A, A) :-
	avl_fetch(Depth-Children, A, ID-_), !.
tree_to_dag(Children, ID, Depth, A0, A) :-
	avl_store(Depth-Children, A0, ID-Dags, A1),
	Depth1 is Depth+1,
	children_to_dags(Children, Dags, Depth1, A1, A).

children_to_dags([], [], _, A, A).
children_to_dags([Key-Tree|L1], [Key-ID|L2], D, A1, A3) :-
	tree_to_dag(Tree, ID, D, A1, A2),
	children_to_dags(L1, L2, D, A2, A3).

table_tree(0, _, [], M, M) :- !.
table_tree(N, Rows, Tree, M0, M) :-
	Key = N-Rows,
	(   avl_fetch(Key, M0, Tree) -> M0 = M
	;   avl_store(Key, M0, Tree, M1),
	    N1 is N-1,
	    head_events(Rows, 1, Ev1, []),
	    fdsort(Ev1, Ev2),
	    table_split(Rows, Ev2, KL1, []),
	    samsort(keyrange_le_table, KL1, KL2),
	    keyclumped(KL2, KL3),
	    table_tree_parts(KL3, N1, Raw, M1, M),
	    merge_subtrees(Raw, Tree)
	).

merge_subtrees([], []).
merge_subtrees([(A..B)-T,(C..D)-T|L1], L2) :-
	integer(B),
	integer(C),
	B+1 =:= C, !,
	merge_subtrees([(A..D)-T|L1], L2).
merge_subtrees([X|L1], [X|L2]) :-
	merge_subtrees(L1, L2).

table_tree_parts([], _, [], M, M).
table_tree_parts([Head-Rows|KL1], N, [Head-Tree|KL2], M0, M) :-
	table_tree(N, Rows, Tree, M0, M1),
	table_tree_parts(KL1, N, KL2, M1, M).

head_events([], _) --> [].
head_events([[Set|_]|Rows], A) -->
	set_events(Set),
	head_events(Rows, A).

set_events([]) --> [].
set_events([[X|Y]|Set]) --> [X,Z],
	{fdinc(Y, Z, 1)},
	set_events(Set).

table_parts([], _) --> [].
table_parts([H|Hs], Tail) --> [H-Tail],
	table_parts(Hs, Tail).

table_split([], _) --> [].
table_split([Row|Tab], Ev) -->
	{Row = [Head | Tail]},
	{split_intervals(Head, Ev, Parts, [])},
	table_parts(Parts, Tail),
	table_split(Tab, Ev).

split_intervals([], _) --> [].
split_intervals([[Min|Max]|Rest], Ev0) -->
	split_interval(Ev0, Min, Max, Ev),
	split_intervals(Rest, Ev).

split_interval([X|Xs], Min, Max, Ev) -->
	{X\==Min}, !,
	split_interval(Xs, Min, Max, Ev).
split_interval([_,Y|Ys], Min, Max, Ev) -->
	{integer(Y)},
	{Max==sup -> true ; Y =< Max}, !,
	{Y1 is Y-1},
	[Min..Y1],
	split_interval([Y|Ys], Y, Max, Ev).
split_interval([_,Y|Ys], Min, Max, [Y|Ys]) -->
	[Min..Max].

fdsort(L0, L) :-
	sort(L0, L1),
	(   select(inf, L1, L2) -> L = [inf|L2]
	;   L = L1
	).

keyrange_le_table((A.._)-_, (C.._)-_) :-
        le(A, C).

table_binary(VarTuples, Extension, Goal) :-
	table_is_dc_int_element(Extension, Values), !,
	list_to_fdset(Values, Set),
	(   foreach([X,Y],VarTuples),
	    param(Goal,Values,Set)
	do  dc_int_element(X, Values, Set, Y, Goal)
	).
table_binary(VarTuples, Extension, Goal) :-
	extension_and_literals(binary, Extension, ExtensionXY, AllLits, [XU,YU]),
	(   foreach([A,B],ExtensionXY),
	    foreach([B,A],ExtensionInv)
	do  true
	),
	sort(ExtensionInv, ExtensionYX),
	length(VarTuples, NT),
	'$fd_ac3intervals_common'(ExtensionXY, ExtensionYX, AllLits, NT, PSet), % PSet = state([_ | '$free'(Ptr)], 0)
	Template = ac3intervals(VarTuples,Extension),
	(   foreach([X,Y],VarTuples),
	    param(Goal,Template,PSet,XU,YU)
	do  X in_set XU,
	    Y in_set YU,
	    arg_attribute(X, XA, Goal, 1),
	    arg_attribute(Y, YA, Goal, 1),
	    fd_global_internal(Template,
			       state([X-XA,Y-YA],PSet,_Handle,0),
			       [none(X),none(Y)], _, clpfd:Goal, 0)
	).

table_is_dc_int_element(Extension, Values) :-
	(   foreach([Iset,Vset],Extension),
	    foreach(V,Values),
	    count(I,1,_)
	do  Iset = [[I|I]],
	    Vset = [[V|V]]
	).

% use compact-table propagator
% called by automaton/9
table_compact(VarTuples, Extension1, _, _, Goal) :-
	extension_and_literals(compact, Extension1, Extension2, AllLits, XDs),
	length(VarTuples, NT),
	'$fd_compact_table_common'(Extension2, AllLits, NT, TSet), % TSet = state([_ | '$free'(Ptr)], 0)
	Constraint = compact_table(VarTuples,Extension1),
	(   foreach(VarTuple,VarTuples),
	    param(XDs,Goal,TSet,Constraint)
	do  (   foreach(V,VarTuple),
		foreach(V-VA,VATuple),
		foreach(VD,XDs),
		foreach(none(V),Susp),
		param(Goal)
	    do  V in_set VD,
		arg_attribute(V, VA, Goal, 1)
	    ),
	    fd_global_internal(Constraint,
			       state(VATuple,TSet,0/*trail*/,_Handle,0),
			       Susp, _, Goal, 0)
	).

extension_and_literals(Encoding, Extension1, Extension2, AllLits, Domains) :-
	transpose(Extension1, Extension1T),
	(   foreach(Column,Extension1T),
	    foreach(Lits,Litss),
	    foreach(Min2Lit,Min2Lits),
	    foreach(Base,Bases),
	    foreach(XU,Domains),
	    fromto(0,Base,Base1,_),
	    count(I,0,_)
	do  sets2atoms(Column, Atoms),
	    (   foreach(Atom,Atoms),
		foreach(lit(I,A,B),Lits),
		foreach(A-lit2(J,Atom),MinLits),
		fromto(Base,J,K,_),
		param(I)
	    do  K is J+1,
		Atom = [[A|B]]
	    ),
	    fdset_union(Column, XU),
	    length(Lits, N),
	    Base1 is Base+N,
	    list_to_avl(MinLits, Min2Lit)
	),
	append(Litss, AllLits),
	encode_extension(Encoding, Extension1, Min2Lits, Bases, Extension2).

encode_extension(binary, Raw, Min2Lits, Bases, Sorted) :-
	(   foreach(Tuple,Raw),
	    foreach(Encoded,Encodeds),
	    param(Min2Lits,Bases)
	do  tuple_set(Tuple, Min2Lits, Bases, Encoded)
	),
	append(Encodeds, AllEncoded),
	sort(AllEncoded, Sorted).
encode_extension(compact, Raw, Min2Lits, Bases, Sorted) :-
	(   foreach(Tuple,Raw),
	    foreach(Encoded,AllEncoded),
	    param(Min2Lits,Bases)
	do  (   foreach(X,Tuple),
		foreach(Min2Lit,Min2Lits),
		foreach(Base,Bases),
		foreach(Is,Encoded)
	    do  entry_indices(X, Min2Lit, Base, Is)
	    )
	),
	sort(AllEncoded, Sorted).

tuple_set([], [], [], [[]]).
tuple_set([X|Xs], [Min2Lit|Min2Lits], [Base|Bases], Encoded1) :-
	tuple_set(Xs, Min2Lits, Bases, Tails),
	entry_indices(X, Min2Lit, Base, Is),
	(   foreach(Tail,Tails),
	    fromto(Encoded1,Encoded2,Encoded5,[]),
	    param(Is)
	do  (   foreach(J,Is),
		fromto(Encoded2,Encoded3,Encoded4,Encoded5),
		param(Tail)
	    do  Encoded3 = [[J|Tail]|Encoded4]
	    )
	).

entry_indices([], _, _, []) :- !.
entry_indices(FDS1, Min2Lit, Base, [I|Is]) :-
	FDS1 = [[A|_]|_],
	avl_fetch(A, Min2Lit, lit2(I,Atom)),
	fdset_subtract(FDS1, Atom, FDS3),
	entry_indices(FDS3, Min2Lit, Base, Is).

%% 1. Get all intervals.
%% 2. Make them all prime.
sets2atoms(Sets, Atoms) :-
	append(Sets, RawIntervals),
	(   foreach(Raw,RawIntervals),
	    foreach([Raw],Intervals)
	do  true
	),
	sort_intervals(Intervals, Intervals1),
	prime_intervals(Intervals1, Atoms).

prime_intervals(Intervals1, Atoms) :-
	prime_intervals_iter(Intervals1, Intervals2, yes, no), !,
	sort_intervals(Intervals2, Intervals3),
	prime_intervals(Intervals3, Atoms).
prime_intervals(Atoms, Atoms).

prime_intervals_iter([], [], Fix, Fix).
prime_intervals_iter([X1,X2|L1], L5, _, Fix) :-
	fdset_intersection(X1, X2, Inter),
	Inter = [_|_], !,
	fdset_subtract(X1, X2, Part1),
	fdset_subtract(X2, X1, Part2),
	prime_prepend(Part2, L1, L2),
	prime_prepend(Inter, L2, L3),
	prime_prepend(Part1, L3, L4),
	prime_intervals_iter(L4, L5, no, Fix).
prime_intervals_iter([X|L1], [X|L2], Fix0, Fix) :-
	prime_intervals_iter(L1, L2, Fix0, Fix).

prime_prepend([], L, L).
prime_prepend([X], L, [[X]|L]) :- !.
prime_prepend([X,Y], L, [[X],[Y]|L]).

sort_intervals(Ints1, Sorted) :-
	(   foreach(Int,Ints1),
	    fromto(Inf1,Inf2,Inf3,[]),
	    fromto(Normal1,Normal2,Normal3,[])
	do  (   Int = [[inf|_]] -> Inf2 = [Int|Inf3], Normal2 = Normal3
	    ;   Inf2 = Inf3, Normal2 = [Int|Normal3]
	    )
	),
	sort(Inf1, InfS),
	sort(Normal1, NormalS),
	append(InfS, NormalS, Sorted).
