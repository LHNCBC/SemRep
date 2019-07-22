/* Copyright(C) 1997, Swedish Institute of Computer Science */

%%% Target library for arithmetic expressions:
%%%	'x+y=t'/3
%%%	'ax=t'/3
%%%	'ax+y=t'/4
%%%	'ax+by=t'/5
%%%	't=c'/2
%%%	't=<c'/2
%%%	't\\=c'/2
%%%	't>=c'/2
%%%	't=u+c'/3
%%%	't=<u+c'/3
%%%	't\\=u+c'/3
%%%	't>=u+c'/3
%%%	't+u=c'/3
%%%	't+u=<c'/3
%%%	't+u\\=c'/3
%%%	't+u>=c'/3
%%%	't=x+y+c'/4
%%%	'x+y=u+c'/4
%%%	'x+y+z=c'/4

:- public
        't=<c'/2,
        't>=c'/2,
        'x*x=y'/2,
        'x*y=z'/3,
        'x/y=z'/3,
        'x div y=z'/3,
        'x mod y=z'/3,
        'x rem y=z'/3,
        'min(x,y)=z'/3,
        'max(x,y)=z'/3,
        'oneof(x,y)=z'/3,
        '|x|=y'/2,
        'x=y'/2,
        'x\\=y'/2,
        'x=<y'/2,
        in_aux_rt/3,
        in_set_aux_rt/3,
        oneof/3,
        abs/2,
        'p/\\q'/3,
        'p\\q'/3,
        'p\\/q'/3,
        'p=>q'/3,
        'p<=>q'/3.

/***
't=c'(T,C) +:
	T in {C}.

't=<c'(T,C) +:
	T in inf..C.

't>=c'(T,C) +:
	T in C..sup.

't\\=c'(T,C) +:
	T in \{C}.

Type checking done at compile time.
***/

:- meta_predicate
	smt(:).

:- meta_predicate smt_to_case(:, -).

% [PM] 4.3.1 ensure that meta declaration is seen by first use.
:- meta_predicate #<=>(:,:).

't=c'(T,C) :-
	check_arguments_error(2, T#=C),
	propagate_interval(T, C, C).

't=<c'(T,C) :-
	check_arguments_error(2, T#=<C),
	propagate_interval(T, inf, C).

't>=c'(T,C) :-
	check_arguments_error(2, T#>=C),
	propagate_interval(T, C, sup).

't\\=c'(T,C) :-
	check_arguments_error(2, T#\=C),
	'$fd_range'(C, C, R, 1),
	'$fd_dom_complement'(R, R1),
	prune_and_propagate(T, R1).


%%% indexicals for arithmetic

'ax=t'(A,X,T) +:
	X in   min(T) /> A..max(T) /< A,
	T in !(min(X) *  A..max(X) *  A).

'x+y=t'(X,Y,T) +:
	X in !(min(T) - max(Y)..max(T) - min(Y)),
	Y in !(min(T) - max(X)..max(T) - min(X)),
	T in !(min(X) + min(Y)..max(X) + max(Y)).

% [4.4] domain consistency can make scalar_product/4 non-idempotent
't+u=c'(T,U,C) +:
	T in !(C - max(U) .. C - min(U)),
	U in !(C - max(T) .. C - min(T)).

% [4.4] domain consistency can make scalar_product/4 non-idempotent - obsolete
'x+c=y'(X,C,Y) +:
	X in !(min(Y) - C .. max(Y) - C),
	Y in !(min(X) + C .. max(X) + C).

% [4.4] domain consistency can make scalar_product/4 non-idempotent
't=u+c'(T,U,C) +:
	T in !(min(U) + C .. max(U) + C),
	U in !(min(T) - C .. max(T) - C).

't=<u+c'(T,U,C) +:
	T in inf..max(U)+C,
	U in min(T) - C..sup.

't\\=u+c'(T,U,C) +:
	T in \{U + C},
	U in \{T - C}.

't>=u+c'(T,U,C) +:
	T in min(U) + C..sup,
	U in inf..max(T) - C.

'ax+c=t'(A,X,C,Y) +:		% obsolete
	X in  (min(Y) - C) /> A..(max(Y) - C) /< A,
	Y in !(min(X)*A + C    .. max(X)*A + C).

'ax+y=t'(A,X,Y,Z) +:
	X in  (min(Z) - max(Y)) /> A..(max(Z) - min(Y)) /< A,
	Y in !(min(Z) - max(X)*A    .. max(Z) - min(X)*A),
	Z in !(min(X)*A + min(Y)    .. max(X)*A + max(Y)).

't+u=<c'(T,U,C) +:
	T in inf..C - min(U),
	U in inf..C - min(T).

't+u\\=c'(T,U,C) +:
	T in \{C - U},
	U in \{C - T}.

't+u>=c'(T,U,C) +:
	T in C - max(U)..sup,
	U in C - max(T)..sup.

'ax+by=t'(A,X,B,Y,Z) +:		% obsolete, backward compatibility?
	X in  (min(Z) - max(Y)*B) /> A.. (max(Z) - min(Y)*B) /< A,
	Y in  (min(Z) - max(X)*A) /> B.. (max(Z) - min(X)*A) /< B,
	Z in !(min(X)*A + min(Y)*B    ..  max(X)*A + max(Y)*B).

'x+y=u+c'(X,Y,U,C) +:
	X in !(min(U) - max(Y) + C..max(U) - min(Y) + C),
	Y in !(min(U) - max(X) + C..max(U) - min(X) + C),
	U in !(min(X) + min(Y) - C..max(X) + max(Y) - C).

't=x+y+c'(T,X,Y,C) :-
	'x+y+c=z'(X,Y,C,T).

'x+y+c=z'(X,Y,C,Z) +:
	X in !(min(Z) - max(Y) - C..max(Z) - min(Y) - C),
	Y in !(min(Z) - max(X) - C..max(Z) - min(X) - C),
	Z in !(min(X) + min(Y) + C..max(X) + max(Y) + C).

'ax+y+c=z'(A,X,Y,C,Z) +:	% obsolete
	X in  (min(Z) -	  max(Y) - C)/>A.. (max(Z) -   min(Y) - C)/<A,
	Y in !(min(Z) -   max(X)*A - C	..  max(Z) -   min(X)*A - C),
	Z in !(min(X)*A + min(Y) + C	..  max(X)*A + max(Y) + C).

'x+y+z=t'(X,Y,Z,T) +:		% obsolete
	X in !(min(T) - max(Y) - max(Z)..max(T) - min(Y) - min(Z)),
	Y in !(min(T) - max(X) - max(Z)..max(T) - min(X) - min(Z)),
	Z in !(min(T) - max(X) - max(Y)..max(T) - min(X) - min(Y)),
	T in !(min(X) + min(Y) + min(Z)..max(X) + max(Y) + max(Z)).

'x+y+z=c'(X,Y,Z,C) +:
	X in !(C - max(Y) - max(Z)..C - min(Y) - min(Z)),
	Y in !(C - max(X) - max(Z)..C - min(X) - min(Z)),
	Z in !(C - max(X) - max(Y)..C - min(X) - min(Y)).

'ax+y+z=t'(A,X,Y,Z,T) +:	% obsolete
	X in  (min(T) -	  max(Y) - max(Z)) /> A..
	      (max(T) -	  min(Y) - min(Z)) /< A,

	Y in !(min(T) - max(X)*A - max(Z)..
	       max(T) - min(X)*A - min(Z)),

	Z in !(min(T) - max(X)*A - max(Y)..
	       max(T) - min(X)*A - min(Y)),

	T in !(min(X)*A + min(Y) + min(Z)..
	       max(X)*A + max(Y) + max(Z)).


%%% Utilities for globals.

arg_attribute(X, Attr, _, _) :-
	'$fd_arg_attribute'(X, 0, Attr), !.
arg_attribute(X, _, Goal, ArgNo) :-
	fd_argument_error(Goal, ArgNo, X).

finite_arg_attribute(X, Attr, _, _) :-
	'$fd_arg_attribute'(X, 1, Attr), !.
finite_arg_attribute(X, _, Goal, ArgNo) :-
	var(X), !,
	illarg(var, Goal, ArgNo, X).
finite_arg_attribute(X, _, Goal, ArgNo) :-
	fd_argument_error(Goal, ArgNo, X).

must_be_dvar_list(List, _, _) :-
	'$fd_dvar_list'(List, 0), !.
must_be_dvar_list(List, Goal, ArgNo) :-
	not_dvar_list(List, Goal, ArgNo).

not_dvar_list(List, Goal, ArgNo) :- var(List), !,
	illarg(var, Goal, ArgNo, List).
not_dvar_list([Arg|List], Goal, ArgNo) :- !,
	arg_attribute(Arg, _, Goal, ArgNo),
	not_dvar_list(List, Goal, ArgNo).
not_dvar_list(List, Goal, ArgNo) :-
	illarg(domain(term,list), Goal, ArgNo, List).

must_be_list_of_finite_dvar(List, _, _) :-
	'$fd_dvar_list'(List, 1), !.
must_be_list_of_finite_dvar(List, Goal, ArgNo) :-
	not_list_of_finite_dvar(List, Goal, ArgNo).

not_list_of_finite_dvar(L, Goal, ArgNo) :- var(L), !,
	illarg(var, Goal, ArgNo, L).
not_list_of_finite_dvar([X|L], Goal, ArgNo) :- !,
	finite_arg_attribute(X, _, Goal, ArgNo),
	not_list_of_finite_dvar(L, Goal, ArgNo).
not_list_of_finite_dvar(L, Goal, ArgNo) :-
	illarg(domain(term,list), Goal, ArgNo, L).

% support for cumulative/2, cumulatives/3
atleast2_finite_arg_attributes(Org, OA, Dur, DA, End, EA, Goal, 1) :-
	'$fd_arg_attribute'(Org, 0, OA),
	'$fd_arg_attribute'(Dur, 0, DA),
	'$fd_arg_attribute'(End, 0, EA),
	arg(3, OA, ODomM),
	get_mutable(dom(_,OMin,OMax,_), ODomM),
	arg(3, DA, DDomM),
	get_mutable(dom(_,DMin,DMax,_), DDomM),
	arg(3, EA, EDomM),
	get_mutable(dom(_,EMin,EMax,_), EDomM),
	(   integer(OMin),
	    integer(OMax),
	    integer(DMin),
	    integer(DMax) ->
	    Min is OMin+DMin,
	    Max is OMax+DMax,
	    propagate_interval(End, Min, Max)
	;   integer(EMin),
	    integer(EMax),
	    integer(DMin),
	    integer(DMax) ->
	    Min is EMin-DMax,
	    Max is EMax-DMin,
	    propagate_interval(Org, Min, Max)
	;   integer(OMin),
	    integer(OMax),
	    integer(EMin),
	    integer(EMax) ->
	    Min is EMin-OMax,
	    Max is EMax-OMin,
	    propagate_interval(Dur, Min, Max)
	;   finite_arg_attribute(Org, _, Goal, 1),
	    finite_arg_attribute(Dur, _, Goal, 1),
	    finite_arg_attribute(End, _, Goal, 1)
	).

/****************************************************************/
/* new all_different/[1,2], all_distinct/[1,2]                  */
/****************************************************************/

all_different(Xs) :-
        OptionsArgNo = 0,
	all_different(Xs, [], opt(local,val,[],[],0,false), all_different(Xs), OptionsArgNo).

all_different(Xs, Opt) :-
        OptionsArgNo = 2,
	all_different(Xs, Opt, opt(local,val,[],[],0,false), all_different(Xs,Opt), OptionsArgNo).

all_distinct(Xs) :-
        OptionsArgNo = 0,
	all_different(Xs, [], opt(global,dom,[],[],0,false), all_distinct(Xs), OptionsArgNo).

all_distinct(Xs, Opt) :-
        OptionsArgNo = 2,
	all_different(Xs, Opt, opt(global,dom,[],[],0,false), all_distinct(Xs,Opt), OptionsArgNo).

circuit(Xs) :-
        OptionsArgNo = 0,
	all_different(Xs, [], opt(global,dom,[],[],0,true), circuit(Xs), OptionsArgNo).

all_different(Xs, Options, Opt0, Goal, OptionsArgNo) :-
	must_be(Options, proper_list, Goal, OptionsArgNo),
	all_diff_options(Options, Opt0, Opt, Goal, OptionsArgNo),
	must_be_dvar_list(Xs, Goal, 1),
	length(Xs, N),
	Opt = opt(Cons,On,_,Eqs,Hack,Circuit),
	dvar_list_susp(Xs, Vec, On, Goal, 1, Susp, Susp1),
	(   nonvar(Susp), Susp = [F|_] -> functor(F, SuspF, 1)
	;   SuspF = none
	),
	(   SuspF==val -> SuspVal = Susp
	;   (   foreach(X,Xs),
		fromto(SuspVal,[val(X)|S1],S1,[])
	    do  true
	    )
	),
	(   Circuit==true
	->  Flag=1,
	    '$fd_range'(1, N, Set, 1),
	    domain(Xs, Set),
	    not_self(Xs, 0),
	    fd_global_internal(circuit(Xs), state(Vec,0,_Handle3,0), SuspVal,
			       _, clpfd:Goal, 4) % non-idempotent
	;   Flag=0
	),
	(   Eqs\==[]
	->  filter_eqs(Eqs, Eqs1),
	    encode_eqs(Eqs1, Eqs2, RHS, RHS2, Xs),
	    must_be_list_of_finite_dvar(Xs, Goal, 1),
	    dvar_list_susp(RHS, RHS2, On, Goal, 1, Susp1, []),
	    sort(Susp, Susp2),
            fd_global_internal(bc_alldiff_lia(Xs), f(Vec,0,Hack,Eqs2,_Handle2,0), Susp2,
			       _, clpfd:Goal, 4) % non-idempotent
	;   N < 2 -> true
	;   N=:=2 ->
	    Xs = [X0,X1],
	    'x\\=y'(X0,X1)
	;   Cons==bound		% [MC] 4.3 does not require finite dvars
	->  Susp1 = [],
            fd_global_internal(bc_alldiff(Xs), f(Vec,0,SuspF,_Handle2,0), Susp,
			       _, clpfd:Goal, 4) % non-idempotent
	;   Cons\==local
	->  Susp1 = [],
	    fd_global_internal(all_different(Xs), state(Vec,0,_Handle1,0), SuspVal,
 			       _, clpfd:Goal, 4), % non-idempotent
	    fd_global_internal(all_distinct(Xs), f(Vec,0,0,Flag,SuspF,_Handle2,0), []/*Susp*/,
			       _, true, 0)
	; /*Cons==local ->*/
	    Susp1 = [],
	    fd_global_internal(all_different(Xs), state(Vec,0,_Handle2,0), SuspVal,
			       _, clpfd:Goal, 4) % non-idempotent
	).

filter_eqs(Eqs, Eqs2) :-
	(   foreach(L #= R,Eqs),
	    fromto(Eqs2,Eqs3,Eqs4,[])
	do  (   L==0 -> R = 0, Eqs3 = Eqs4
	    ;   Eqs3 = [L #= R|Eqs4]
	    )
	).

encode_eqs(Eqs, Eqs2, RHS, RHS2, Xs) :-
	(   foreach(L #= R,Eqs),
	    foreach(eqn(Type,Ixs,Pair),Eqs2),
	    foreach(R,RHS),
	    foreach(Pair,RHS2),
	    param(Xs)
	do  addends(L, Type, Ys, []),
	    encode_eqs_more(Type, Ys, Ixs, R, Xs)
	).

encode_eqs_more(1, Ys, Ixs, R, Xs) :- !,
	(   foreach(Var,Ys),
	    foreach(Ix,Ixs),
	    fromto(0,Min1,Min3,Min),
	    fromto(0,Max1,Max3,Max),
	    param(Xs)
	do  var_nth(Var, Xs, 0, Ix),
	    fd_min_max(Var, Min2, Max2),
	    Min3 is Min1+Min2,
	    Max3 is Max1+Max2
	),
	R in Min..Max.
encode_eqs_more(2, Ys, Ixs, R, Xs) :-
	(   foreach(Var^2,Ys),
	    foreach(Ix,Ixs),
	    fromto(0,Min1,Min3,Min),
	    fromto(0,Max1,Max3,Max),
	    param(Xs)
	do  var_nth(Var, Xs, 0, Ix),
	    fd_min_max(Var, Min2, Max2),
	    Min3 is Min1+Min2*Min2,
	    Max3 is Max1+Max2*Max2
	),
	R in Min..Max.
encode_eqs_more(3, Ys, Ixs, R, Xs) :-
	(   foreach(Var,Ys),
	    foreach(Ix,Ixs),
	    fromto(1,Min1,Min3,Min),
	    fromto(1,Max1,Max3,Max),
	    param(Xs)
	do  var_nth(Var, Xs, 0, Ix),
	    fd_min_max(Var, Min2, Max2),
	    Min3 is Min1*Min2,
	    Max3 is Max1*Max2
	),
	R in Min..Max.

addends(X, T) --> {simple(X), T in {1,3}}, !, [X].
addends(X + Y, T) -->
	addends(X, T),
	addends(Y, T).
addends(X * Y, 3) --> {X \== Y}, !,
	addends(X, 3),
	addends(Y, 3).
addends(X * X, 2) --> [X^2].

dvar_list_susp([], [], _On, _Goal, _ArgNo) --> [].
dvar_list_susp([X|Xs], [X-M|Vec], On, Goal, ArgNo) -->
	on(On, X),
	{arg_attribute(X, M, Goal, ArgNo)},
	dvar_list_susp(Xs, Vec, On, Goal, ArgNo).

finite_dvar_list_susp([], [], _On, _Goal, _ArgNo) --> [].
finite_dvar_list_susp([X|Xs], [X-M|Vec], On, Goal, ArgNo) -->
	on(On, X),
	{finite_arg_attribute(X, M, Goal, ArgNo)},
	finite_dvar_list_susp(Xs, Vec, On, Goal, ArgNo).

all_diff_options([], Opt, Opt, _, _).
all_diff_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    all_diff_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,all_diff_option), Goal, ArgNo, X)
        ),
	all_diff_options(L, Opt1, Opt, Goal, ArgNo).

all_diff_option(on(On), opt(Cons,_,Cost,Eqs,Hack,Circ), opt(Cons,On,Cost,Eqs,Hack,Circ)) :-
	on(On, _, _, _).
all_diff_option(consistency(Arg), opt(_,_,Cost,Eqs,Hack,Circ), opt(Cons,On,Cost,Eqs,Hack,Circ)) :-
	consistency_option(Arg, Cons, On).
all_diff_option(cost(V,Mat), opt(Cons,On,_,Eqs,Hack,Circ), opt(Cons,On,cost(V,VM,Mat),Eqs,Hack,Circ)) :-
	arg_attribute(V, VM, 0, 3).
all_diff_option(L #= R, opt(_,_,Cost,Eqs,Hack,Circ), opt(Cons,On,Cost,[L #= R|Eqs],Hack,Circ)) :-
	consistency_option(bound, Cons, On).
all_diff_option(circuit(Circ), opt(Cons,On,Cost,Eqs,Hack,_), opt(Cons,On,Cost,Eqs,Hack,Circ)) :-
	bool_option(Circ, _).
% forbidden values optimization, undocumented so far
all_diff_option(hack(Hack), opt(Cons,On,Cost,Eqs,_,Circ), opt(Cons,On,Cost,Eqs,F,Circ)) :-
	bool_option(Hack, F).

/****************************************************************/
/* element/3							*/
/****************************************************************/

element(Index, Xs, Value) :-
	Goal = element(Index, Xs, Value),
	must_be_dvar_list(Xs, Goal, 2),
	length(Xs, N),
	propagate_interval(Index, 1, N),
	(   integer(Index) ->
	    nth1(Index, Xs, X),
	    'x=y'(X, Value)
	;   is_bool_list(Xs, Indexes) ->
	    list_to_fdset(Indexes, FDSet),
	    in_set_iff_rt(Index, FDSet, Value)
	;   ground(Xs) ->
	    min_member(Min, Xs),
	    max_member(Max, Xs),
	    Value in Min..Max,
	    arg_attribute(Index, IndexM, Goal, 1),
	    arg_attribute(Value, ValueM, Goal, 3),
	    fd_global(Goal, f(Index-IndexM,Value-ValueM,Xs,1,_Handle,0), [dom(Index),dom(Value)])
	;   arg_attribute(Index, IndexM, Goal, 1),
	    arg_attribute(Value, ValueM, Goal, 3),
	    dvar_list_susp(Xs, Xs2, none, Goal, 2, Susp, [dom(Index),dom(Value)]),
	    fd_global(Goal, f(Index-IndexM,Value-ValueM,Xs2,0,_Handle,0), Susp)
	).

dc_int_element(Index, Xs, Set, Value, Source) :-
	Goal = element(Index, Xs, Value),
	length(Xs, N),
	propagate_interval(Index, 1, N),
	(   integer(Index) ->
	    nth1(Index, Xs, X),
	    'x=y'(X, Value)
	;   is_bool_list(Xs, Indexes) ->
	    list_to_fdset(Indexes, FDSet),
	    in_set_iff_rt(Index, FDSet, Value)
	;   Value in_set Set,
	    arg_attribute(Index, IndexM, Goal, 1),
	    arg_attribute(Value, ValueM, Goal, 3),
	    fd_global_internal(Goal, f(Index-IndexM,Value-ValueM,Xs,3,_Handle,0), [dom(Index),dom(Value)], _, Source, 0)
	).

is_bool_list(Xs, Indexes) :-
	(   foreach(X,Xs),
	    fromto(Indexes,I2,I3,[]),
	    count(I,1,_)
	do  integer(X),
	    (   X = 0 -> I2 = I3
	    ;   X = 1 -> I2 = [I|I3]
	    )
	).

% precond: Ys has been checked
% derived from table_binary/3, currently unused, but keep it as an option
ac3element(VarTuples, Ys, Goal) :-
	Ys = [Y1|_],
	(   foreach(V,Ys),
	    count(I,1,Xsize),
	    foreach(V-I,KL1),
	    fromto(Y1,Min1,Min2,Min),
	    fromto(Y1,Max1,Max2,Max)
	do  Min2 is min(Min1,V),
	    Max2 is max(Max1,V)
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(K-Is,KL3),
	    fromto(Y2X,Y2X2,Y2X4,[])
	do  list_to_fdset(Is, Set),
	    (   foreach([A|B],Set),
		fromto(Y2X2,[(K..K)-(A..B)|Y2X3],Y2X3,Y2X4),
		param(K)
	    do  true
	    )
	),
	length(VarTuples, NT),
	'$fd_ac3element_common'(Ys, Y2X, NT, Common), % Common = state([_ | '$free'(Ptr)], 0)
	Template = ac3element(VarTuples,Ys),
	(   foreach([X,Y],VarTuples),
	    fromto(Susp1,[none(X),none(Y)|Susp2],Susp2,[]),
	    param(Template,Common,Goal,Xsize,Min,Max)
	do  X in 1..Xsize,
	    Y in Min..Max,
	    arg_attribute(X, XA, Goal, 1),
	    arg_attribute(Y, YA, Goal, 1),
	    fd_global_internal(Template,
			       state([X-XA,Y-YA],Common/*Ys,Y2X*/,_Handle,0),
			       Susp1, _, clpfd:Goal, 0)
	).

/****************************************************************/
/* relation/3 (deprecated)					*/
/****************************************************************/

relation(X, L1, Y) :-
	(   foreach(U-V,L1),
	    foreach([U,V],L2)
	do  true
	),
	table([[X,Y]], L2).

/****************************************************************/
/* minimum/2, maximum/2						*/
/****************************************************************/

minimum(Y, Xs) :-
	minmax(Y, Xs, minimum(Y,Xs), 0).

maximum(Y, Xs) :-
	minmax(Y, Xs, maximum(Y,Xs), 1).

minmax(Y, Xs, Goal, IsMax) :-
	arg_attribute(Y, YM, Goal, 1),
	must_be_dvar_list(Xs, Goal, 2),
	length(Xs, N),
	(   N=:=1 -> Xs = [Y]
	;   N=:=2, IsMax=:=1 -> Xs = [X1,X2], 'max(x,y)=z'(X1, X2, Y)
	;   N=:=2 -> Xs = [X1,X2], 'min(x,y)=z'(X1, X2, Y)
	;   N>=3 ->	    
	    dvar_list_susp(Xs, Xs2, none, Goal, 2, Susp, [minmax(Y)]),
	    fd_global(Goal, f(Y-YM,Xs2,N,IsMax,_Handle,0), Susp)
	).

/****************************************************************/
/* bool_and/2    						*/
/****************************************************************/

% since 4.3.4
bool_and(Xs, Y) :-
	Goal = bool_and(Xs,Y),
	must_be(Xs, proper_list, Goal, 1),
	'$fd_debug'(off, off, 1), !,
	% bool_new_style(Xs, Y), !,
	(   foreach(X,Xs),
	    foreach(#\ X, NXs),
	    param(Y,Goal)
	do  bool_or_raw([X,#\ Y], Goal)
	),
	bool_or_raw([Y|NXs], Goal).
bool_and(Xs, Y) :-
	Goal = bool_and(Xs,Y),
	% must_be(Xs, proper_list, Goal, 1),
	must_be_lit_list(Xs, XAs, XSs, XNs, none, Goal, 1),
	must_be_lit_list([Y], [YA], [YS], [YN], none, Goal, 2),
	(   foreach(S1,[YS|XSs]), % invert all senses, piggy-back on bool_or
	    foreach(S2,Ss2)
	do  S2 is 1-S1
	),
	fd_global(Goal, f([YA|XAs],Ss2,0/*trailed counter*/,_Handle,0), [YN|XNs]).

/****************************************************************/
/* bool_or/2    						*/
/****************************************************************/

% since 4.3.4
bool_or(Xs, Y) :-
	Goal = bool_or(Xs,Y),
	must_be(Xs, proper_list, Goal, 1),
	'$fd_debug'(off, off, 1), !,
	% bool_new_style(Xs, Y), !,
	(   Y==1
	->  bool_or_raw(Xs, Goal)
	;   (   foreach(X,Xs),
		param(Y,Goal)
	    do  bool_or_raw([#\ X,Y], Goal)
	    ),
	    bool_or_raw([#\ Y|Xs], Goal)
	).
bool_or(Xs, Y) :-
	Goal = bool_or(Xs,Y),
	% must_be(Xs, proper_list, Goal, 1),
	must_be_lit_list(Xs, XAs, XSs, XNs, none, Goal, 1),
	must_be_lit_list([Y], [YA], [YS], [YN], none, Goal, 2),
	fd_global(Goal, f([YA|XAs],[YS|XSs],0/*trailed counter*/,_Handle,0), [YN|XNs]).
	
/****************************************************************/
/* bool_xor/2    						*/
/****************************************************************/

% since 4.3.4
bool_xor(Xs, Y) :-
	Goal = bool_xor(Xs,Y),
	must_be(Xs, proper_list, Goal, 1),
	must_be_lit_list(Xs, XAs, XSs, XNs, none, Goal, 1),
	must_be_lit_list([Y], [YA], [YS], [YN], none, Goal, 2),
	'$fd_debug'(off, off, 1), !,
	bool_xor_new([YA|XAs], [YS|XSs], [YN|XNs]).
% bool_xor(Xs, Y) :-
% 	Goal = bool_xor(Xs,Y),
% 	must_be(Xs, proper_list, Goal, 1),
% 	bool_new_style(Xs, Y), !,
% 	bool_xor_raw([Y|Xs], Goal).
bool_xor(Xs, Y) :-
	Goal = bool_xor(Xs,Y),
% 	must_be(Xs, proper_list, Goal, 1),
	must_be_lit_list(Xs, XAs, XSs, XNs, none, Goal, 1),
	must_be_lit_list([Y], [YA], [YS], [YN], none, Goal, 2),
	fd_global(Goal, f([YA|XAs],[YS|XSs],0,0,_Handle,0), [YN|XNs]).

bool_xor_new(XAs, Ss, _Susp) :-
	(   foreach(X-A,XAs),
	    foreach(S,Ss),
	    fromto(0,Sense1,Sense2,Sense),
	    fromto(Ys1,Ys2,Ys3,[]),
	    fromto(Bs1,Bs2,Bs3,[])
	do  (   nonvar(X) -> Ys2 = Ys3, Bs2 = Bs3, Sense2 is Sense1 \ S \ X
	    ;   Ys2 = [X|Ys3], Bs2 = [A|Bs3], Sense2 is Sense1 \ S
	    )
	),
	length(Ys1, N),
	(   N = 0 -> Sense = 0
	;   N = 1 -> Ys1 = [Sense]
	;   N = 2, Sense = 0
	->  post_disequation([1,1], Ys1, Bs1, 1) % alternatively, equate them with an indexical
	;   N = 2, Sense = 1
	->  post_disequation([-1,1], Ys1, Bs1, 0)
	;   M is (N+2-Sense)//2,
	    (   for(I,0,M),
		foreach(Valid,Valids),
		param(Sense)
	    do  Valid is 2*I+Sense
	    ),
	    list_to_fdset(Valids, FDSet),
	    Sum in_set FDSet,
	    sum(Ys1, #=, Sum)
	).

bool_new_style([], _).
bool_new_style([_], _).
bool_new_style([_,_], _).
bool_new_style(_, Y) :-
	integer(Y).

bool_or_raw(Disj, Goal) :-
	(   foreach(L,Disj),
	    foreach(S,Ss),
	    foreach(Pair,Pairs),
	    param(Goal)
	do  deref_lit(L, 0, S, Pair, Goal, 1)
	),
	(   foreach(S2,Ss),
	    foreach(X-Attr,Pairs),
	    fromto(Pos1,Pos2,Pos3,[]),
	    fromto(Neg1,Neg2,Neg3,[]),
	    fromto(0,Ent2,Ent3,Ent)
	do  (   var(X), S2=0
	    ->  Pos2 = [X-Attr|Pos3], Neg2 = Neg3, Ent2 = Ent3
	    ;   var(X), S2=1
	    ->  Pos2 = Pos3, Neg2 = [X-Attr|Neg3], Ent2 = Ent3
	    ;   Pos2 = Pos3, Neg2 = Neg3, Ent3 is Ent2 \/ (X \ S2)
	    )
	),
	sort(Pos1, Pos),
	sort(Neg1, Neg),
	ord_intersection(Pos, Neg, Both),
	(   Ent = 1 -> true
	;   Both = [_|_] -> true
	;   Pos = [], Neg = [Y-_] -> Y = 0
	;   Pos = [Y-_], Neg = [] -> Y = 1
	;   Pos-Neg \== []-[],
	    '$fd_post_clause'(Pos, Neg),
	    % print_message(warning, post_clause(Disj)), % MC trace
	    true
	).

deref_lit(Arg, S0, S, Pair, Goal, ArgNo) :-
	compound(Arg),
	Arg = (#\ X), !,
	S1 is 1-S0,
	deref_lit(X, S1, S, Pair, Goal, ArgNo).
deref_lit(X, S, S, X-Attr, Goal, ArgNo) :-
	arg_attribute(X, Attr, Goal, ArgNo),
	propagate_interval(X, 0, 1).

post_disequation(Coeffs, Xs, As, RHS) :-
	(   foreach(X,Xs),
	    foreach(A,As),
	    foreach(X-A,XAs)
	do  true
	),
	'$fd_post_disequation'(Coeffs, XAs, RHS),
	% print_message(warning, post_disequation(Coeffs,Xs,RHS)), % MC trace
	true.

/****************************************************************/
/* bool_channel/4						*/
/****************************************************************/

% X[I]=1 <-> Y Rel I+O
bool_channel(Xs, Y, Rel, O) :-
	Goal = bool_channel(Xs,Y,Rel,O),
	scalar_op(Rel, Code0, 0, Strict),
	must_be(Xs, proper_list, Goal, 1),
	must_be_lit_list(Xs, XAs, XSs, XNs, none, Goal, 1),
	arg_attribute(Y, YA, Goal, 2),
	must_be(O, integer, Goal, 4),
	length(Xs, N),
	ON1 is O+N-1,
	Code is Code0+((Strict/\1)<<2)-1,
	(   Code0 =< 2 ->	% #=<, #>=, #<, #>
	    AC1 = 1, AC2 is N+1
	;   true ->			    % #=, #\=
	    '$fd_range'(O, ON1, AC1, 1) % initial "prev domain" is [O,O+len(X))
	),
	fd_global(Goal, f(XAs,XSs,Y-YA,Code,O,AC1,AC2,_Handle,0), [none(Y)|XNs]).

must_be_lit_list([], [], [], [], _, _, _).
must_be_lit_list([Lit|List], [Arg-Attr|Args], [S|Ss], [Susp|Susps], F, Goal, ArgNo) :-
	deref_lit(Lit, 0, S, Arg-Attr, Goal, ArgNo),
	Susp =.. [F,Arg],
	must_be_lit_list(List, Args, Ss, Susps, F, Goal, ArgNo).

/****************************************************************/
/* nvalue/2        						*/
/****************************************************************/

nvalue(N, Xs) :-
	Goal = nvalue(N, Xs),
	must_be_dvar_list(Xs, Goal, 2),
	arg_attribute(N, NM, Goal, 1),
	finite_dvar_list_susp(Xs, Xs2, dom, Goal, 2, Susp, [minmax(N)]),
	fd_global(Goal, f(N-NM,Xs2,0,0,_Handle,0), Susp).

/****************************************************************/
/* assignment/[2,3]						*/
/****************************************************************/

% assignment(X1...Xn, Y1...Yn) is true if 
% all Xi,Yi in 1..n and Xi=j iff Yj=i

assignment(Xs, Ys) :-
        OptionsArgNo = 0,
	assignment(Xs, Ys, [], opt(global,dom,[],[],0,false), assignment(Xs,Ys,[]), OptionsArgNo).

assignment(Xs, Ys, Options) :-
        OptionsArgNo = 0,
	assignment(Xs, Ys, Options, opt(global,dom,[],[],0,false), assignment(Xs,Ys,Options), OptionsArgNo).

circuit(Xs, Ys) :-
        OptionsArgNo = 0,
	assignment(Xs, Ys, [], opt(global,dom,[],[],0,true), circuit(Xs,Ys), OptionsArgNo).

% [MC] dead
% circuit(Xs, Ys, Options) :-
% 	assignment(Xs, Ys, Options, opt(global,dom,[],[],0,true), circuit(Xs,Ys,Options)).

assignment(Xs, Ys, Options, Opt0, Goal, OptionsArgNo) :-
	must_be(Options, proper_list, Goal, OptionsArgNo),
	all_diff_options(Options, Opt0, Opt, Goal, OptionsArgNo),
	must_be_dvar_list(Xs, Goal, 1),
	length(Xs, N),
	length(Ys, N),
	must_be_dvar_list(Ys, Goal, 2),
	'$fd_range'(1, N, Set, 1),
	domain(Xs, Set),
	domain(Ys, Set),
	Opt = opt(Cons,On,Cost,_,_,Circuit),
	assignment_state(Xs, Ys, 0, XVec, YVec, On, Goal, Susp, Susp1),
	(   Cost==[] -> Susp1 = [], Flag0 = 0
	;   Cost = cost(C,_,_), Susp1 = [dom(C)], Flag0 = 2
	),
	(   foreach(X,Xs),
	    fromto(SuspVal,[val(X)|S1],S1,S2)
	do  true
	),
	(   foreach(Y,Ys),
	    fromto(S2,[val(Y)|S3],S3,[])
	do  true
	),
	fd_global_internal(pairing(Xs,Ys), state(XVec,YVec,0,_Handle1,0), SuspVal,
			   _, clpfd:Goal, 4), % non-idempotent
	(   Circuit==true
	->  Flag is Flag0+1,
	    not_self(Xs, 0),
	    not_self(Ys, 0),
	    fd_global_internal(circuit(Xs), state(XVec,0,_Handle2,0), SuspVal,
			       _, true, 4), % non-idempotent
	    fd_global_internal(circuit(Ys), state(YVec,0,_Handle3,0), SuspVal,
			       _, true, 4) % non-idempotent
	;   Flag = Flag0
	),
	(   Flag>=2
	->  fd_global_internal(assignment(_,_,_),
			       f(XVec,YVec,0,Flag,Cost,0/*cost so far*/,_Handle4,0), Susp,
			       Global, true, 0),
	    Global = global(StateM,_,_,_,_),
	    (   foreach(X2,Xs),
		count(I,0,_),
		param(C,StateM,Goal)
	    do  fd_global_internal(assignment_helper(X2,C), f(I,X2,StateM), [val(X2)], _, true, 0)
	    )
	;   Cons==local -> true
	;   fd_global_internal(assignment(_,_,_), f(XVec,YVec,0,Flag,Cost,0/*cost so far*/,_Handle4,0), Susp,
			       _, true, 0)
	).

assignment_state([], [], _, [], [], _On, _Goal) --> [].
assignment_state([X|Xs], [Y|Ys], I, [X-XM|XVec], [Y-YM|YVec], On, Goal) -->
	on(On, X),
	on(On, Y),
	{arg_attribute(X, XM, Goal, 1),
	 arg_attribute(Y, YM, Goal, 2),
	 J is I+1},
	assignment_state(Xs, Ys, J, XVec, YVec, On, Goal).

not_self([], _).
not_self([Y|Ys], I) :-
	J is I+1,
	't\\=c'(Y, J),
	not_self(Ys, J).

/****************************************************************/
/* multi_cumulative/[2,3]                        		*/
/****************************************************************/

multi_cumulative(Tasks, Capas) :-
	multi_cumulative(Tasks, Capas, []).

multi_cumulative(Tasks, Capas, Options) :-
	Goal = multi_cumulative(Tasks,Capas,Options),
	must_be(Options, proper_list, Goal, 2),
	multi_cumulative_options(Options, opt(0,[]), opt(Greedy1,Precedences1), Goal, 3),
	propagate_interval(Greedy1, 0, 1),
	arg_attribute(Greedy1, Greedy2, Goal, 2),
	(   foreach(task(Org,Dur,End,Uses,ID),Tasks),
	    foreach(task(Org-OA,Dur,End-EA,Uses,ID),Tasks2),
	    foreach(Uses,UseMat),
	    foreach(ID-I,Map1),
	    foreach(Org,Orgs),
	    foreach(End,Ends),
	    fromto(Susp,[none(Org),none(End)|S],S,[]),
	    count(I,1,N),
	    param(KDim,Goal)
	do  length(Uses, KDim),
	    atleast2_finite_arg_attributes(Org, OA, Dur, _, End, EA, Goal, 1),
	    fd_hiding('t=u+c'(End, Org, Dur)),
	    must_be(ID, integer, Goal, 1),
	    must_be(Uses, proper_list(integer), Goal, 1)
	),
	transpose(UseMat,UseMatT),
	(   foreach(Capa,Capas),
	    foreach(Colors,UseMatT),
	    fromto(1,KC1,KC2,KC),
	    count(_,1,KR),
	    param(Goal)
	do  (   Capa = cumulative(Capi),
		integer(Capi)
	    ->  KC1 = KC2
	    ;   Capa = colored(Capi),
		integer(Capi)
	    ->  max_member(MaxC, Colors),
		KC2 is max(KC1,MaxC+1)
	    ;   illarg(domain(term,cumulative_capacity), Goal, 2, Capa)
	    )
	),
	keysort(Map1, Map2),
	keyclumped(Map2, Map3),
	ord_list_to_avl(Map3, Map4),
	must_be(Precedences1, proper_list(pair), Goal, 3),
	(   foreach(P1-S1,Precedences1),
	    foreach(P2-S2,Precedences2),
	    param(Map4,Orgs,Ends)
	do  avl_fetch(P1, Map4, [P2|_]), % if IDs are not unique, take first hit
	    avl_fetch(S1, Map4, [S2|_]),
	    nth1(P2, Ends, PrecDate),
	    nth1(S2, Orgs, SuccDate),
	    fd_hiding(PrecDate #=< SuccDate)
	),
	fd_global(Goal, f(N,KR,KC,Tasks2,Capas,Greedy1-Greedy2,
			  N/*ntargets*/,0/*nsources*/,Precedences2,_Handle,0), Susp).

multi_cumulative_options([], Opt, Opt, _, _).
multi_cumulative_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    multi_cumulative_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,multi_cumulative_option), Goal, ArgNo, X)
        ),
	multi_cumulative_options(L, Opt1, Opt, Goal, ArgNo).

% opt(Greedy,Precedences)
multi_cumulative_option(greedy(G),         opt(_,P), opt(G,P)).
multi_cumulative_option(precedences(P),    opt(G,_), opt(G,P)).

/****************************************************************/
/* cumulative/[1,2]                        			*/
/****************************************************************/

ef_cumulative(Tasks) :-
	cumulative(Tasks, []).

ef_cumulative(Tasks, Options) :-
	Goal = cumulative(Tasks,Options),
	must_be(Options, proper_list, Goal, 2),
	cumulative_options(Options,
			   opt(1,    _,                    [], bounding_box(_,_),0),
			   opt(Limit,resource(Tasks2,Map2),Ps1,bounding_box(L,U),Flags),
			   Goal, 2),
	finite_arg_attribute(Limit, LimitA, Goal, 2),
	arg_attribute(L, LA, Goal, 2),
	arg_attribute(U, UA, Goal, 2),
	(   foreach(task(Org,Dur,End,Height,Machine),Tasks),
	    foreach(task(Org-OA,Dur-DA,End-EA,Height-HA,Machine),Tasks2),
	    param(Goal)
	do  atleast2_finite_arg_attributes(Org, OA, Dur, DA, End, EA, Goal, 1),
	    finite_arg_attribute(Height, HA, Goal, 1),
	    must_be(Machine, integer, Goal, 1)
	),
	(   foreach(task(Org1,Dur1,End1,Height1,_),Tasks),
	    fromto(Susp,[dom(Org1),minmax(Dur1),dom(End1),minmax(Height1)|S],S,Susp1) % [MC] 4.4: some methods reason on whole domain
	do  true
	),
	(   Flags/\1 =:= 0 -> Ps2 = Ps1
	;   (   foreach(K#=V,Ps1),
		foreach(K-V,KV1)
	    do  true
	    ),
	    list_to_avl(KV1, Map1),
	    task_diffs_to_avl(Tasks2, Map1, Map2, Ps2, Ps1)
	),
	(   foreach(J-I #= D,Ps2),
	    foreach(f(J,I,D-DA1), Ps3),
	    fromto(Susp1,[minmax(D)|T],T,[minmax(Limit)]),
	    param(Goal)
	do  arg_attribute(D, DA1, Goal, 2)
	),
	length(Tasks2, N),
	fd_global_internal(cumulative(Tasks,Options),
			   f(N,Tasks2,Ps3,Limit-LimitA,L-LA,U-UA,Flags,N,0,_Handle,0),
			   Susp, _, clpfd:Goal, 4). % non-idempotent

task_diffs_to_avl([], Map, Map) --> [].
task_diffs_to_avl([Task|Tasks], Map1, Map3) -->
	{Task = task(_,_,_,_,ID)},
	task_diffs_to_avl(Tasks, ID, Map1, Map2),
	task_diffs_to_avl(Tasks, Map2, Map3).

task_diffs_to_avl([], _, Map, Map) --> [].
task_diffs_to_avl([Task|Tasks], ID1, Map1, Map3) -->
	{Task = task(_,_,_,_,ID2)},
	{K12 = ID1-ID2},
	(   {avl_fetch(K12, Map1, _) -> Map1 = Map2}
	;   {avl_fetch(ID2-ID1, Map1, _) -> Map1 = Map2}
	;   {avl_store(K12, Map1, D12, Map2)}, [K12 #= D12]
	),
	task_diffs_to_avl(Tasks, ID1, Map2, Map3).

cumulative_options([], Opt, Opt, _, _).
cumulative_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    cumulative_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,cumulative_option), Goal, ArgNo, X)
        ),
	cumulative_options(L, Opt1, Opt, Goal, ArgNo).

% opt(Limit,Resource,Precedences,BoundingBox,Flags) where
% 0x1 = precedences
% 0x2 = stronger than O(N log N) algos
cumulative_option(limit(Lim),        opt(_  ,R,Ps,BB,Flags), opt(Lim,R,Ps,BB               ,Flags)).
cumulative_option(bounding_box(L,U), opt(Lim,R,Ps,_ ,Flags), opt(Lim,R,Ps,bounding_box(L,U),Flags)).
cumulative_option(precedences(Ps),   opt(Lim,R,_, BB,Flags), opt(Lim,R,Ps,BB               ,Flags)).
% cumulative_option(resource(R),     opt(Lim,_,Ps,BB,Flags0),opt(Lim,R,Ps,BB               ,Flags)) :-
% 	Flags is (Flags0 /\ -2) \/ 1.
cumulative_option(global(B),         opt(Lim,R,Ps,BB,Flags0),opt(Lim,R,Ps,BB               ,Flags)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -3) \/ (Value<<1).

ttef_cumulative(Tasks) :-
	ttef_cumulative(Tasks, []).

ttef_cumulative(Tasks, Options) :-
	Goal = cumulative(Tasks,Options),
	must_be(Options, proper_list, Goal, 2),
	cumulative_options(Options, opt(1,_,[],bb,0), opt(Limit,_,Pr,BB,_), Goal, 2),
	finite_arg_attribute(Limit, LimitA, Goal, 2),
	(   foreach(task(Org,Dur,End,Height,Machine),Tasks),
	    foreach(task(Org-OA,Dur-DA,End-EA,Height-HA,Machine),Tasks2),
	    foreach(Org,Orgs),
	    foreach(End,Ends),
	    fromto(Susp,[minmax(Org),minmax(Dur),minmax(End),minmax(Height)|S],S,[minmax(Limit)]),
	    param(Goal)
	do  atleast2_finite_arg_attributes(Org, OA, Dur, DA, End, EA, Goal, 1),
	    fd_hiding(plus(Org, Dur, End)),
	    finite_arg_attribute(Height, HA, Goal, 1)
	),
	(   BB = bb -> true
	;   BB = bounding_box(L,U),
	    fd_hiding(minimum(L, Orgs)),
	    fd_hiding(maximum(U, Ends))
	),
	fd_hiding(ttef_precedences(Pr, Tasks)),
	length(Tasks, N),
	fd_global_internal(ttef_cumulative(Tasks,Options),
			   f(Tasks2,Limit-LimitA,N,_Handle,0),
			   Susp, _, clpfd:Goal, 4). % non-idempotent

ttef_precedences([], _) :- !.
ttef_precedences(Pr, Tasks) :-
	(   foreach(task(O,_,_,_,Id),Tasks),
	    foreach(Id-O,KL)
	do  true
	),
	list_to_avl(KL, Avl),
	(   foreach(Ti-Tj #= Dij,Pr),
	    param(Avl)
	do  avl_fetch(Ti, Avl, Oi),
	    avl_fetch(Tj, Avl, Oj),
	    Oi - Oj #= Dij
	).	

cumulative(Tasks) :-
	ttef_cumulative(Tasks).

cumulative(Tasks, Options) :-
	ttef_cumulative(Tasks, Options).


/****************************************************************/
/* count/4 (deprecated)						*/
/****************************************************************/

count(Value, Xs, Rel, Card) :-
	'$fd_range'(Value, Value, Set, 1),
	(   foreach(X,Xs),
	    foreach(B,Bs),
	    param(Set)
	do  in_set_iff_rt(X, Set, B)
	),
	sum(Bs, Rel, Card).

/****************************************************************/
/* sum/3, scalar_product/[4,5]					*/
/****************************************************************/

sum(Xs, Rel, S) :-
	Goal = sum(Xs, Rel, S),
	must_be(Xs, proper_list, Goal, 1),
	(   foreach(_,Xs),
	    foreach(1,Cs)
	do  true
	),
	scalar_product(Cs, Xs, Rel, S).

scalar_product(Cs, Xs, Rel, S) :-
	scalar_product(Cs, Xs, Rel, S, []).

scalar_product(Cs, Xs, Rel, S, Options) :-
	Goal = scalar_product(Cs,Xs,Rel,S,Options),
	Goal4 = scalar_product(Cs,Xs,Rel,S),
	must_be(Cs, proper_list, Goal, 1),
	must_be(Xs, proper_list, Goal, 2),
	length(Cs, Len),
	must_be(Options, proper_list, Goal, 5),
	scalar_options(Options, opt(bound,[],Len), opt(Cons,Among,_), Goal, 5),
	must_be_dvar_list(Xs, Goal, 2),
	must_be_dvar_list([S], Goal, 4),
	(   foreach(X,Xs),
	    foreach(C,Cs),
	    foreach(X-C,L1)
	do  true
	),
	keysort(L1, L2),
	keyfuse(L2, L3),
	(   foreach(K-V,L3),
	    foreach(K,Xs1),
	    foreach(V,Cs1)
	do  true
	),
	scalar_op(Rel, Op0, 0, RHS),
	scalar_bound_sum(Cs1, Xs1, Op0, 0, 0, S), % try to bound S if unbounded
	scalar_state([-1|Cs1], [S|Xs1], Op0, RHS, Vec, Sum, Susp, Goal),
	length(Vec, N),
	(   \+'$fd_coref'(Susp),
	    sp_strength_reduce(N, Vec, Op0, Sum, [Kern], []) -> call(Kern)
	;   scalar_op_fast(Vec, Sum, Op0, Op),
	    (   Options\==[] -> Pretty = Goal
	    ;   pretty_scalar_product(Goal4, Pretty)
	    ),
	    fd_global_internal(Goal4, state(Vec,Op,Sum,0/*Nground*/,_,0), Susp,
			       _, clpfd:Pretty, 0)
	),
	% (   var(Op) -> true
	% ;   (Op/\0x2f) =\= 0xb -> true	% 8+3 means #= and "fast" and not "unit"
	% ;   gcd_aux(Cs, Xs, S, Vec, Sum, Susp)
	% ),
	(   Cons\==global -> true
	;   Rel\==(#=) -> true
	;   ground(Xs1) -> true	% happens in factory_planning_instance.fzn
	;   nonvar(Kern),
	    domain_consistent_indexical(Kern) -> true
	;   dc_linear(Cs1, Xs1, S, Vec, Sum, Goal)
	),
	(   Rel \== (#\=),
	    ground([S|Cs])
	->  do_atleast_le(Among, Cs, Xs, Rel, S, Goal)
	;   do_atleast(Among, Xs)
	).

do_atleast_le(Among, As, Xs, Rel, C, Goal) :-
	dvar_list_susp(Xs, XVec, none, Goal, 3, Susp, []),
	length(Xs, Len),
	Prop = atleast_le(_,_,_,_,_),
	(   foreach(among(A,B,S),Among),
	    param(Rel,As,XVec,C,Goal,Susp,Prop,Len)
	do  do_atleast_le(Rel, As, XVec, C, Goal, Susp, Prop, A, B, Len, S)
	).

do_atleast_le(#=<, As, XVec, C, _Goal, Susp, Prop, A, B, Len, S) :-
	A =< B,
	(   A =< 0 -> true
	;   fd_global_internal(Prop, state(A,S,XVec,As,C,0/*#ground*/,_Handle1,0), Susp,
			       _, true, 0)
	),
	(   B >= Len -> true
	;   BC is Len-B,
	    fdset_complement(S, SC),
	    fd_global_internal(Prop, state(BC,SC,XVec,As,C,0/*#ground*/,_Handle2,0), Susp,
			       _, true, 0)
	).
do_atleast_le(#<, As, XVec, C, Goal, Susp, Prop, A, B, Len, S) :-
	C1 is C-1,
	do_atleast_le(#=<, As, XVec, C1, Goal, Susp, Prop, A, B, Len, S).
do_atleast_le(#>=, As, XVec, C, Goal, Susp, Prop, A, B, Len, S) :-
	NC is -C,
	(   foreach(Ac,As),
	    foreach(NA,NAs)
	do  NA is -Ac
	),
	do_atleast_le(#=<, NAs, XVec, NC, Goal, Susp, Prop, A, B, Len, S).
do_atleast_le(#>, As, XVec, C, Goal, Susp, Prop, A, B, Len, S) :-
	C1 is C-1,
	do_atleast_le(#>=, As, XVec, C1, Goal, Susp, Prop, A, B, Len, S).
do_atleast_le(#=, As, XVec, C, Goal, Susp, Prop, A, B, Len, S) :-
	do_atleast_le(#=<, As, XVec, C, Goal, Susp, Prop, A, B, Len, S),
	do_atleast_le(#>=, As, XVec, C, Goal, Susp, Prop, A, B, Len, S).

do_atleast(Among, Xs) :-
	(   foreach(among(A,B,S),Among),
	    param(Xs)
	do  (   foreach(X,Xs),
		foreach(Q,Qs),
		param(S)
	    do  X in_set S #<=> Q
	    ),
	    sum(Qs, #>=, A),
	    sum(Qs, #=<, B)
	).

% dc_linear(Cs, Xs, S) :-
% 	Goal = scalar_product(Cs,Xs,#=,S,[consistency(domain)]), % for error handling
% 	scalar_state([-1|Cs], [S|Xs], 3, 0, Vec, Sum, _, Goal),
% 	dc_linear(Cs, Xs, S, Vec, Sum, Goal).

% dc_linear(Cs, Xs, S, Vec, Sum, Goal) :-
% 	finite_dvar_list_susp(Xs, _, dom, Goal, 2, _, []),	 % check finite bounds!
% 	(   foreach(f(_,B,_),Vec),
% 	    foreach(none(B),Susp),
% 	    count(_,1,N)
% 	do  true
% 	),
% 	fd_global_internal(dc_linear(Cs,Xs,S), state(Vec,N,Sum,N/*Ntargets*/,_,0), Susp,
% 			   _, clpfd:Goal, 0).

dc_linear(_Cs, Xs, _S, Vec, Sum, Goal) :-
	finite_dvar_list_susp(Xs, _, dom, Goal, 2, _, []), % check finite bounds!
	(   foreach(f(C,X,_),Vec),
	    foreach(Unit-Y,UYs1)
	do  dc_linear_mult(C, X, Unit, Y)
	),
	keysort(UYs1, UYs2),
	keyclumped(UYs2, UYs3),
	NSum is -Sum,
	dc_linear_cases(UYs3, Sum, NSum).

dc_linear_cases([], 0, 0).
dc_linear_cases([-1-MYs], _, NSum) :- !,
	dc_sum(MYs, NSum).
dc_linear_cases([1-PYs], Sum, _) :- !,
	dc_sum(PYs, Sum).
dc_linear_cases([-1-MYs,1-PYs], 0, 0) :- !,
	dc_sum(PYs, XY),
	dc_sum(MYs, XY).
dc_linear_cases([-1-MYs,1-PYs], _, NSum) :- !,
	dc_sum(PYs, X),
	dc_sum(MYs, Y),
	plus(X, NSum, Y).

dc_sum([X], X) :- !.
dc_sum([X,Y|Zs], Sum) :-
	dc_sum([Y|Zs], Rest),
	plus(X, Rest, Sum).

dc_linear_mult(1, X, 1, X) :- !.
dc_linear_mult(-1, X, -1, X) :- !.
dc_linear_mult(C, X, D, Y) :-
	AC is abs(C),
	D is C//AC,
	fd_set(X, XSet),
	fdset_to_list(XSet, XList),
	(   foreach(I,XList),
	    foreach(ACI,YList),
	    param(AC)
	do  ACI is AC*I
	),
	list_to_fdset(YList, YSet),
	Y in_set YSet,
	arg_attribute(X, XM, true, 0),
	arg_attribute(Y, YM, true, 0),
	fd_global_internal('ax=y'(AC,X,Y), state(AC,X,XM,Y,YM), [dom(X),dom(Y)], _, true, 0).
	
pretty_scalar_product(scalar_product(As,Xs,Rel,S0), Pretty) :-
	pretty_scalar_product([-1|As], [S0|Xs], 0, 0, 0, LHS, RHS, S),
	(   S =:= 0 -> LHS=LHS1, RHS=RHS1
	;   S > 0   -> LHS=LHS1, scalar_cons(RHS, S, RHS1)
	;   S1 is -S,  RHS=RHS1, scalar_cons(LHS, S1, LHS1)
	),
	Pretty =.. [Rel,LHS1,RHS1].

pretty_scalar_product([], [], LHS, RHS, S, LHS, RHS, S).
pretty_scalar_product([A|As], [X|Xs], LHS, RHS, S, LHS2, RHS2, S2) :-
	integer(X), !,
	S1 is S - A*X,
	pretty_scalar_product(As, Xs, LHS, RHS, S1, LHS2, RHS2, S2).
pretty_scalar_product([A|As], [X|Xs], LHS, RHS, S, LHS2, RHS2, S2) :-
	var(X),
	(   A =:= 0
	->  LHS = LHS1,
	    RHS = RHS1
	;   A =:= 1
	->  scalar_cons(LHS, X, LHS1),
	    RHS = RHS1
	;   A > 0
	->  scalar_cons(LHS, A*X, LHS1),
	    RHS = RHS1
	;   A =:= -1
	->  LHS = LHS1,
	    scalar_cons(RHS, X, RHS1)
	;   NA is -A,
	    LHS = LHS1,
	    scalar_cons(RHS, NA*X, RHS1)
	),
	pretty_scalar_product(As, Xs, LHS1, RHS1, S, LHS2, RHS2, S2).

scalar_cons(S, T, T1) :- (S==0 -> T1=T; T1=S+T).


% helper for GCD reasoning, where at least one nonunit coefficient
% currently disabled, but consider as an option for the future
% gcd_aux(As, Xs, S, Vec, Sum, Susp) :-
% 	(   foreach(f(C,_,_),Vec),
% 	    fromto(0,NU1,NU2,NonUnit)
% 	do  (abs(C) =:= 1 -> NU2 = NU1 ; NU2 is NU1+1)
% 	),
% 	NonUnit > 1, !,		% claim: no extra pruning unless >1 non-unit coeffs
% 	fd_global(gcd_aux(As,Xs,S), state(Vec,Sum,0/*NGround*/,0/*NEdges*/,_,0), Susp).
% gcd_aux(_, _, _, _, _, _).

scalar_options([], Opt, Opt, _, _).
scalar_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    scalar_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,scalar_option), Goal, ArgNo, X)
        ),
	scalar_options(L, Opt1, Opt, Goal, ArgNo).

scalar_option(consistency(Arg), opt(_,Among,Len), opt(Cons,Among,Len)) :-
	consistency_option(Arg, Cons, _).
scalar_option(among(A,B,R), opt(Cons,Among,Len), opt(Cons,[among(A,B,S)|Among],Len)) :-
	integer(A),
	integer(B),
	set_expression(R, S).

scalar_op(#=<, 1, Sum, Sum).
scalar_op(#<,  1, Sum, Sum1) :- Sum1 is Sum-1.
scalar_op(#>=, 2, Sum, Sum).
scalar_op(#>,  2, Sum, Sum1) :- Sum1 is Sum+1.
scalar_op(#=,  3, Sum, Sum).
scalar_op(#\=, 4, Sum, Sum).

%% Use shortcuts if we can guarantee that (Sum - sum(lb)) and (-Sum + sum(ub))
%% can be represented by a signed long, in which case all arithmetic can use
%% inline C.
%% 0x20 = all coeffs +-1
%% 0x10 = all vars binary
%% 0x8 = safe arithmetic
scalar_op_fast(Vec, Sum, Op0, Op) :-
	NSum is -Sum,
	(   foreach(f(C,X,_),Vec),
	    fromto(Sum,F0,F1,F),
	    fromto(NSum,E0,E1,E),
	    fromto(0x20,Unit1,Unit2,UnitFlag),
	    fromto(0x10,Binary1,Binary2,BinaryFlag)
	do  get_fd_domain(X, Dom),
	    Dom = dom(_,Min,Max,_),
	    integer(Min),
	    integer(Max),
	    (   Min=:=0, Max=:=1
	    ->  Binary2 = Binary1,
		CMin is min(C,0),
		CMax is max(C,0)
	    ;   Binary2 = 0,
		CMin is min(C*Min,C*Max),
		CMax is max(C*Min,C*Max)
	    ),
	    (abs(C)=:=1 -> Unit2 = Unit1 ; Unit2 = 0),
	    F1 is F0 - CMin,
	    E1 is E0 + CMax
	),
	Fscaled is F>>3,
	\+prolog:'$large_data'(0, Fscaled, _), % not too large?
	Escaled is E>>3,
	\+prolog:'$large_data'(0, Escaled, _), !,% not too large?
	Op is Op0+UnitFlag+BinaryFlag+0x8.
scalar_op_fast(_, _, Op, Op).

%%% This is not an exhaustive list.

% assert: Under > 0
floordiv(Over, Under, Div) :-
	(Over>=0 -> Div is Over//Under ; Div is -((-Over-1)//Under+1)).

% assert: Under > 0
ceildiv(Over, Under, Div) :-
	(Over=<0 -> Div is Over//Under ; Div is (Over-1)//Under+1).

sp_strength_reduce(0, [], Op, S) --> !,
	(   {Op =:= 1} -> [S >= 0]
	;   {Op =:= 2} -> [S =< 0]
	;   {Op =:= 3} -> [S =:= 0]
	;   {Op =:= 4} -> [S =\= 0]
	).
sp_strength_reduce(1, [f(C1,X1,_M1)], Op, S) --> !,
	(   {Op =:= 1, C1 > 0} ->
	    {floordiv(S, C1, S1)},
	    ['t=<c'(X1, S1)]
	;   {Op =:= 1, C1 < 0} ->
	    {NS is -S},
	    {NC is -C1},
	    {ceildiv(NS, NC, S1)},
	    ['t>=c'(X1, S1)]
	;   {Op =:= 2, C1 > 0} ->
	    {ceildiv(S, C1, S1)},
	    ['t>=c'(X1, S1)]
	;   {Op =:= 2, C1 < 0} ->
	    {NS is -S},
	    {NC is -C1},
	    {floordiv(NS, NC, S1)},
	    ['t=<c'(X1, S1)]
	;   {Op =:= 3} -> 
	    {S1 is S//C1},
	    ({S mod C1 =\= 0} -> [false] ; ['t=c'(X1, S1)])
	;   {Op =:= 4} ->
	    {S1 is S//C1},
	    ({S mod C1 =\= 0} -> [true] ; ['t\\=c'(X1, S1)])
	).   
sp_strength_reduce(_, Fs, 4, S) --> % general disequation
	{'$fd_debug'(off, off, 1)},
	(   foreach(f(C,X,M),Fs),
	    foreach(C,Cs),
	    foreach(X,Xs),
	    foreach(M,Ms),
	    fromto(abs(S),Sum1,Sum2,Sum3)
	do  {fd_min_max(X, 0, 1)},
	    {Sum2 is Sum1+abs(C)}
	),
	{\+prolog:'$large_data'(0, Sum3, _)}, !,
	[post_disequation(Cs, Xs, Ms, S)].
sp_strength_reduce(2, [f(C1,X1,_M1),f(C2,X2,_M2)], Op, S) -->
	(   {Op =:= 1} -> sp_strength_reduce_2_le(C1, X1, C2, X2, S)
	;   {Op =:= 2} -> sp_strength_reduce_2_ge(C1, X1, C2, X2, S)
	;   {Op =:= 3} -> sp_strength_reduce_2_eq(C1, X1, C2, X2, S)
	;   {Op =:= 4} -> sp_strength_reduce_2_ne(C1, X1, C2, X2, S)
	).	
sp_strength_reduce(3, [f(C1,X1,_M1),f(C2,X2,_M2),f(C3,X3,_M3)], 3, S) -->
	sp_strength_reduce_3_eq(C1, X1, C2, X2, C3, X3, S).
sp_strength_reduce(N, Fs, Op, S) -->
	(   foreach(f(1,X,_),Fs),
	    foreach(X,Xs)
	do  {fd_min_max(X, 0, 1)}
	), !,
	(   {Op=:=2, S=:=1} -> [bool_or(Xs,1)] % scp(1s, Xs, #>=, 1)
	;   {Op=\=1, Op=\=4, S=:=N} -> [bool_and(Xs,1)] % scp(1s, Xs, #>= | #=, N)
	).
sp_strength_reduce(N, Fs, Op, S) -->
	(   foreach(f(-1,X,_),Fs),
	    foreach(X,Xs)
	do  {fd_min_max(X, 0, 1)}
	), !,
	(   {Op=:=1, S=:= -1} -> [bool_or(Xs,1)] % scp(-1s, Xs, #=<, -1)
	;   {Op=\=2, Op=\=4, S=:= -N} -> [bool_and(Xs,1)] % scp(-1s, Xs, #=< | #=, -N)
	).

sp_strength_reduce_2_le(1, X1, 1, X2, S) --> !,
	['t+u=<c'(X1, X2, S)].
sp_strength_reduce_2_le(-1, X1, 1, X2, S) --> !,
	['t=<u+c'(X2, X1, S)].
sp_strength_reduce_2_le(1, X1, -1, X2, S) --> !,
	['t=<u+c'(X1, X2, S)].
sp_strength_reduce_2_le(-1, X1, -1, X2, S) --> !,
	{S1 is -S},
	['t+u>=c'(X1, X2, S1)].

sp_strength_reduce_2_ge(1, X1, 1, X2, S) --> !,
	['t+u>=c'(X1, X2, S)].
sp_strength_reduce_2_ge(-1, X1, 1, X2, S) --> !,
	['t>=u+c'(X2, X1, S)].
sp_strength_reduce_2_ge(1, X1, -1, X2, S) --> !,
	['t>=u+c'(X1, X2, S)].
sp_strength_reduce_2_ge(-1, X1, -1, X2, S) --> !,
	{S1 is -S},
	['t+u=<c'(X1, X2, S1)].

% unused
sp_strength_reduce_2_ne(1, X1, 1, X2, S) --> !,
	['t+u\\=c'(X1, X2, S)].
sp_strength_reduce_2_ne(-1, X1, 1, X2, S) --> !,
	['t\\=u+c'(X2, X1, S)].
sp_strength_reduce_2_ne(1, X1, -1, X2, S) --> !,
	['t\\=u+c'(X1, X2, S)].
sp_strength_reduce_2_ne(-1, X1, -1, X2, S) --> !,
	{S1 is -S},
	['t+u\\=c'(X1, X2, S1)].

sp_strength_reduce_2_eq(1, X1, 1, X2, S) --> !,
	['t+u=c'(X1, X2, S)].
sp_strength_reduce_2_eq(-1, X1, 1, X2, S) --> !,
	['t=u+c'(X2, X1, S)].
sp_strength_reduce_2_eq(1, X1, -1, X2, S) --> !,
	['t=u+c'(X1, X2, S)].
sp_strength_reduce_2_eq(-1, X1, -1, X2, S) --> !,
	{S1 is -S},
	['t+u=c'(X1, X2, S1)].
sp_strength_reduce_2_eq(1, X1, C2, X2, S) --> {C2>0}, !,
	['ax+y=t'(C2, X2, X1, S)].
sp_strength_reduce_2_eq(1, X1, C2, X2, 0) --> {C2<0}, !,
	{C3 is -C2},
	['ax=t'(C3, X2, X1)].
sp_strength_reduce_2_eq(1, X1, C2, X2, S) --> {C2<0}, !,
	{C3 is -C2},
	['ax+y=t'(C3, X2, S, X1)].
sp_strength_reduce_2_eq(-1, X1, C2, X2, 0) --> {C2>0}, !,
	['ax=t'(C2, X2, X1)].
sp_strength_reduce_2_eq(-1, X1, C2, X2, S) --> {C2>0}, !,
	{NS is -S},
	['ax+y=t'(C2, X2, NS, X1)].
sp_strength_reduce_2_eq(-1, X1, C2, X2, S) --> {C2<0}, !,
	{C3 is -C2},
	{NS is -S},
	['ax+y=t'(C3, X2, X1, NS)].
sp_strength_reduce_2_eq(C1, X1, 1, X2, S) --> {C1>0}, !,
	['ax+y=t'(C1, X1, X2, S)].
sp_strength_reduce_2_eq(C1, X1, 1, X2, 0) --> {C1<0}, !,
	{C3 is -C1},
	['ax=t'(C3, X1, X2)].
sp_strength_reduce_2_eq(C1, X1, 1, X2, S) --> {C1<0}, !,
	{C3 is -C1},
	['ax+y=t'(C3, X1, S, X2)].
sp_strength_reduce_2_eq(C1, X1, -1, X2, 0) --> {C1>0}, !,
	['ax=t'(C1, X1, X2)].
sp_strength_reduce_2_eq(C1, X1, -1, X2, S) --> {C1>0}, !,
	{NS is -S},
	['ax+y=t'(C1, X1, NS, X2)].
sp_strength_reduce_2_eq(C1, X1, -1, X2, S) --> {C1<0}, !,
	{C3 is -C1},
	{NS is -S},
	['ax+y=t'(C3, X1, X2, NS)].

sp_strength_reduce_3_eq(1, X1, 1, X2, 1, X3, S) --> !,
	['x+y+z=c'(X1, X2, X3, S)].
sp_strength_reduce_3_eq(-1, X1, 1, X2, 1, X3, 0) --> !,
	['x+y=t'(X2, X3, X1)].
sp_strength_reduce_3_eq(-1, X1, 1, X2, 1, X3, S) --> !,
	['x+y=u+c'(X2, X3, X1, S)].
sp_strength_reduce_3_eq(1, X1, -1, X2, 1, X3, 0) --> !,
	['x+y=t'(X1, X3, X2)].
sp_strength_reduce_3_eq(1, X1, -1, X2, 1, X3, S) --> !,
	['x+y=u+c'(X1, X3, X2, S)].
sp_strength_reduce_3_eq(-1, X1, -1, X2, 1, X3, 0) --> !,
	['x+y=t'(X1, X2, X3)].
sp_strength_reduce_3_eq(-1, X1, -1, X2, 1, X3, S) --> !,
	['t=x+y+c'(X3, X1, X2, S)].
sp_strength_reduce_3_eq(1, X1, 1, X2, -1, X3, 0) --> !,
	['x+y=t'(X1, X2, X3)].
sp_strength_reduce_3_eq(1, X1, 1, X2, -1, X3, S) --> !,
	['x+y=u+c'(X1, X2, X3, S)].
sp_strength_reduce_3_eq(-1, X1, 1, X2, -1, X3, 0) --> !,
	['x+y=t'(X3, X1, X2)].
sp_strength_reduce_3_eq(-1, X1, 1, X2, -1, X3, S) --> !,
	['t=x+y+c'(X2, X3, X1, S)].
sp_strength_reduce_3_eq(1, X1, -1, X2, -1, X3, 0) --> !,
	['x+y=t'(X3, X2, X1)].
sp_strength_reduce_3_eq(1, X1, -1, X2, -1, X3, S) --> !,
	['t=x+y+c'(X1, X3, X2, S)].
sp_strength_reduce_3_eq(-1, X1, -1, X2, -1, X3, S) --> !,
	{S1 is -S},
	['x+y+z=c'(X1, X2, X3, S1)].

scalar_bound_sum([], [], 3, Min, Max, S) :-
	\+prolog:'$large_data'(0, Min, _),
	\+prolog:'$large_data'(0, Max, _), !,
	propagate_interval(S, Min, Max).
scalar_bound_sum([C|Cs], [X|Xs], 3, Min0, Max0, S) :-
	fd_min_max(X, Min1, Max1),
	integer(Min1),
	integer(Max1), !,
	(   C>0 ->
	    Min2 is Min0 + C*Min1,
	    Max2 is Max0 + C*Max1
	;   Min2 is Min0 + C*Max1,
	    Max2 is Max0 + C*Min1
	),
	scalar_bound_sum(Cs, Xs, 3, Min2, Max2, S).
scalar_bound_sum(_, _, _, _, _, _).

scalar_state(Cs, Xs, Op, S, Vec, Sum, Susp, Goal) :-
	scalar_vector(Cs, Xs, S, Vec0, Sum0, Susp, Goal),
	(   Vec0=[] -> Vec = Vec0, Sum = Sum0
	;   Vec0=[f(GCD0,_,_)|_],
	    GCD1 is abs(GCD0),
	    (   foreach(f(C0,_,_),Vec0),
		fromto(GCD1,GCD2,GCD3,GCD)
	    do  GCD3 is gcd(GCD2,C0)
	    ),
	    (   foreach(f(C1,X,M),Vec0),
		foreach(f(C,X,M),Vec1),
		param(GCD)
	    do  C is C1//GCD
	    ),
	    scalar_state_adjust(Op, Sum0, GCD, Sum, Vec1, Vec)
	).

scalar_state_adjust(1, Sum0, GCD, Sum, Vec, Vec) :- % #=<
	floordiv(Sum0, GCD, Sum).
scalar_state_adjust(2, Sum0, GCD, Sum, Vec, Vec) :- % #>=
	ceildiv(Sum0, GCD, Sum).
scalar_state_adjust(3, Sum0, GCD, Sum, Vec, Vec) :- % #=, disentailed if =\= (modulo GCD)
	Sum0 mod GCD =:= 0, !,
	Sum is Sum0//GCD.
scalar_state_adjust(3, _, _, 1, _, []). % #=, disentailed if =\= (modulo GCD)
scalar_state_adjust(4, Sum0, GCD, Sum, Vec, Vec) :- % #\=, entailed if =\= (modulo GCD)
	Sum0 mod GCD =:= 0, !,
	Sum is Sum0//GCD.
scalar_state_adjust(4, _, _, 1, _, []). % #\=, entailed if =\= (modulo GCD)

scalar_vector([], [], Sum, [], Sum, [], Goal) :-
	must_be_fd_integer(Sum, Goal, 4).
scalar_vector([C|Cs], [X|Xs], S0, Vec, S, Susp, Goal) :-
	integer(X), !,
	S1 is S0-C*X,
	scalar_vector(Cs, Xs, S1, Vec, S, Susp, Goal).
scalar_vector([C|Cs], [X|Xs], S0, [f(C,X,M)|Vec], S, [none(X)|Susp], Goal) :-
	C=\=0, !,
	must_be_fd_integer(C, Goal, 1),
	arg_attribute(X, M, Goal, 2),
	scalar_vector(Cs, Xs, S0, Vec, S, Susp, Goal).
scalar_vector([_|Cs], [_|Xs], S0, Vec, S, Susp, Goal) :-
	scalar_vector(Cs, Xs, S0, Vec, S, Susp, Goal).

/****************************************************************/
/* 'x*x=y'/2							*/
/****************************************************************/

'x*x=y'(X, Y) :-
	Goal = 'x*x=y'(X,Y),
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 2),
	Y in 0..sup,
	(   integer(X) -> Y is X*X
	;   integer(Y)
	->  Y >= 0,
	    X1 is integer(sqrt(Y)),
	    X1*X1 =:= Y,
	    X0 is -X1,
	    X in {X0,X1}
	;   Susp = [minmax(X),minmax(Y)],
	    fd_global_internal(Goal, state(X,XM,Y,YM,_Handle,0), Susp, _, clpfd:Goal, 4/*non-fixp*/)
	).

/****************************************************************/
/* 'x*y=z'/3							*/
/****************************************************************/

'x*y=z'(X, Y, Z) :-
	Goal = 'x*y=z'(X,Y,Z),
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 2),
	arg_attribute(Z, ZM, Goal, 3),
	Susp = [minmax(X),minmax(Y),minmax(Z)],
	fd_global_internal(Goal, state(X,XM,Y,YM,Z,ZM,_Handle,0), Susp, _, clpfd:Goal, 4/*non-fixp*/).

% for compatibility with formulas saved prior to 3.11.2:
'-ax=t'(A,X,T) +:
	X in   -max(T) /< A .. -min(T) /> A,
	T in !(-max(X) *  A .. -min(X) *  A).

/****************************************************************/
/* 'x/y=z'/3							*/
/****************************************************************/

'x/y=z'(X, Y, Z) :-
	Goal = 'x/y=z'(X,Y,Z),
	quo_div_mod_rem(X, Y, Z, Goal).

quo_div_mod_rem(X, Y, Z, Goal) :-
	arg_attribute(X, XM, Goal, 1),
	arg_attribute(Y, YM, Goal, 2),
	arg_attribute(Z, ZM, Goal, 3),
	Susp = [minmax(X),minmax(Y),minmax(Z)],
	fd_global_internal(Goal, state(X-XM,Y-YM,Z-ZM,_Handle,0), Susp, _, clpfd:Goal, 4/*non-fixp*/).

/****************************************************************/
/* 'x div y=z'/3						*/
/****************************************************************/

'x div y=z'(X, Y, Z) :-
	Goal = 'x div y=z'(X,Y,Z),
	quo_div_mod_rem(X, Y, Z, Goal).

/****************************************************************/
/* 'x mod y=z'/3						*/
/****************************************************************/

'x mod y=z'(X, Y, Z) :-
	Goal = 'x mod y=z'(X,Y,Z),
	quo_div_mod_rem(X, Y, Z, Goal).

/****************************************************************/
/* 'x rem y=z'/3						*/
/****************************************************************/

'x rem y=z'(X, Y, Z) :-
	Goal = 'x rem y=z'(X,Y,Z),
	quo_div_mod_rem(X, Y, Z, Goal).

/****************************************************************/
/* min/3, max/3 etc						*/
/****************************************************************/

% 'min(x,y)=z'(X, Y, Z) :-
% 	X #>= Z,
% 	Y #>= Z,
% 	'oneof(x,y)=z'(X, Y, Z).

% 'max(x,y)=z'(X, Y, Z) :-
% 	X #=< Z,
% 	Y #=< Z,
% 	'oneof(x,y)=z'(X, Y, Z).

'min(x,y)=z'(X, Y, Z) +:
	X in !(((dom(Y)/\dom(Z)) ? (min(Z)..sup)) \/ 
	       ((min(Z)..max(Y)) ? dom(Z))),
	Y in !(((dom(X)/\dom(Z)) ? (min(Z)..sup)) \/
	       ((min(Z)..max(X)) ? dom(Z))),
	Z in !((dom(X)\/dom(Y)) /\ (inf..max(X)) /\ (inf..max(Y))).

'max(x,y)=z'(X, Y, Z) +:
	X in !(((dom(Y)/\dom(Z)) ? (inf..max(Z))) \/ 
	       ((min(Y)..max(Z)) ? dom(Z))),
	Y in !(((dom(X)/\dom(Z)) ? (inf..max(Z))) \/
	       ((min(X)..max(Z)) ? dom(Z))),
	Z in !((dom(X)\/dom(Y)) /\ (min(X)..sup) /\ (min(Y)..sup)).

'oneof(x,y)=z'(X, Y, Z) :-
	'oneof(x,y)=z IND'(X, Y, Z).

'oneof(x,y)=z IND'(X, Y, Z) +:
	X in !(((dom(Y)/\dom(Z)) ? (inf..sup)) \/ dom(Z)),
	Y in !(((dom(X)/\dom(Z)) ? (inf..sup)) \/ dom(Z)),
	Z in !((dom(X)\/dom(Y))).

'|x|=y'(X,Y) :-
	't>=c'(Y, 0),				% do this outside the loop
	'|x|=y 1'(X,Y).

'|x|=y 1'(X,Y) +:
	X in !(dom(Y) \/ (0-dom(Y))),
	Y in !(dom(X) \/ (0-dom(X))).

%%% domain consistent addition
plus(X, Y, Z) +:
	Z in !(dom(X)+dom(Y)),
	X in !(dom(Z)-dom(Y)),
	Y in !(dom(Z)-dom(X)).

%%% Support for reified constraints (domain reasoning for now)

% Now domain consistent!
% optimized for common cases
'x=y'(X, Y) :- integer(Y), !,
	't=c'(X, Y).
'x=y'(X, Y) :- integer(X), !,
	't=c'(Y, X).
'x=y'(X, Y) :-
	't=u IND'(X, Y).	% [MC] SPRM 13711

% 't=u'(U, U).			% [MC] 3.11.3 -- unused as of SP 4.3

't=u IND'(X,Y) +:
	X in !(dom(Y)),
	Y in !(dom(X)).
't=u IND'(X,Y) -:
	X in \{Y},
	Y in \{X}.
% 't=u IND'(X,Y) +?				% covered by next rule
%	X in {Y}.
't=u IND'(X,Y) -?
	X in \dom(Y).

% Now domain consistent!
% just the negation of the above
'x\\=y'(T, U) :- integer(T), !,
	't\\=c'(U, T).
'x\\=y'(T, U) :- integer(U), !,
	't\\=c'(T, U).
'x\\=y'(T, U) :-
	'$fd_debug'(off, off, 1),
	get_atts(T, fd_attribute(_,TDomM,_)),
	get_mutable(TDom, TDomM),
	get_atts(U, fd_attribute(_,UDomM,_)),
	get_mutable(UDom, UDomM),
	TDom = dom(_,0,1,_),
	UDom = dom(_,0,1,_), !,
	'$fd_arg_attribute'(T, 0, TA),
	'$fd_arg_attribute'(U, 0, UA),
	post_disequation([-1,1], [T,U], [TA,UA], 0).
'x\\=y'(T, U) :-
	'x\\=y IND'(T, U).

'x\\=y IND'(X,Y) -:
	X in !(dom(Y)),
	Y in !(dom(X)).
'x\\=y IND'(X,Y) +:
	X in \{Y},
	Y in \{X}.
% 'x\\=y IND'(X,Y) -?			% covered by next rule
%	X in {Y}.
'x\\=y IND'(X,Y) +?
	X in \dom(Y).

'x=<y'(T, U) :- integer(T), !,
	't>=c'(U, T).
'x=<y'(T, U) :- integer(U), !,
	't=<c'(T, U).
'x=<y'(T, U) :-
	'x=<y IND'(T, U).

'x=<y IND'(X,Y) +:
	X in inf..max(Y),
	Y in min(X)..sup.
'x=<y IND'(X,Y) -:
	X in (min(Y)+1)..sup,
	Y in inf..(max(X)-1).
'x=<y IND'(X,Y) +?
	X in inf..min(Y).
'x=<y IND'(X,Y) -?				% NOT covered by prev rule
	X in (max(Y)+1)..sup.

% relevant for #= only
domain_consistent_indexical('t=c'(_,_)).
domain_consistent_indexical('t+u=c'(_,_,_)).
domain_consistent_indexical('t=u+c'(_,_,_)).

in_aux_rt(X, Expr) :-
	set_expression_check(Expr, Set, X in Expr, 2),
	prune_and_propagate_chk(X, Set).

in_aux_rt(X, Expr, B) :-
	set_expression_check(Expr, Set, X in Expr #<=> B, 1),
	in_set_iff_rt(X, Set, B).


in_set_aux_rt(X, Set) :-
	'$fd_size'(Set, _, 1), !,
	'$fd_dom_union'(Set, [], Copy),	% Set could occur multiple times
	prune_and_propagate_chk(X, Copy).
in_set_aux_rt(X, Set) :-
	ill_formed_constraint(X in_set Set, Goal), call(Goal).

in_set_aux_rt(X, Set, Bool) :-
	'$fd_size'(Set, _, 1), !,
	in_set_iff_rt(X, Set, Bool).
in_set_aux_rt(X, Set, B) :-
	ill_formed_constraint(X in_set Set #<=> B, Goal), call(Goal).

in_set_iff_rt(X, Set, B) :-
	'$fd_dom_union'(Set, [], Copy),	% [MC] 3.12.10 protect against destructs AND ensure deref
	Constraint = in_set_ix(X,Set),
	'$fd_find_definition'(Constraint, clpfd, DefPtr),
	arg_attribute(X, A, Constraint, 1),
	Attv =  in_set_ix(A,Copy),
	iff_aux(DefPtr, Constraint, Attv, B).

in_set_ix(X, Set) +:
	X in set(Set).
in_set_ix(X, Set) -:
	X in \set(Set).
in_set_ix(X, Set) +?
	X in set(Set).

/****************************************************************/
/* bool/4 - for binary compatibility				*/
/****************************************************************/

bool(0, X, Y, Z) :-		% X #/\ Y   #<=> Z i.e. [X | \Z], [Y | \Z], [\X | \Y | Z]
	'$fd_debug'(off, off, 1), !,
	bool_or_raw([X, #\ Z], []),
	bool_or_raw([Y, #\ Z], []),
	bool_or_raw([#\ X, #\ Y, Z], []).
bool(1, X, Y, Z) :-		% X #/\ #\Y #<=> Z i.e. [X | \Z], [\Y | \Z], [\X | Y | Z]
	'$fd_debug'(off, off, 1), !,
	bool_or_raw([X, #\ Z], []),
	bool_or_raw([#\ Y, #\ Z], []),
	bool_or_raw([#\ X, Y, Z], []).
bool(2, X, Y, Z) :-		% X #/\ Y   #<=> #\Z i.e. [X | Z], [Y | Z], [\X | \Y | \Z]
	'$fd_debug'(off, off, 1), !,
	bool_or_raw([X, Z], []),
	bool_or_raw([Y, Z], []),
	bool_or_raw([#\ X, #\ Y, #\ Z], []).
bool(3, X, Y, Z) :-		% X #\/ Y   #<=> Z i.e. [\X | Z], [\Y | Z], [X | Y | \Z]
	'$fd_debug'(off, off, 1), !,
	bool_or_raw([#\ X, Z], []),
	bool_or_raw([#\ Y, Z], []),
	bool_or_raw([X, Y, #\ Z], []).
bool(4, X, Y, Z) :-		% X #\/ #\Y #<=> Z i.e. [\X | Z], [Y | Z], [X | \Y | \Z]
	'$fd_debug'(off, off, 1), !,
	bool_or_raw([#\ X, Z], []),
	bool_or_raw([Y, Z], []),
	bool_or_raw([X, #\ Y, #\ Z], []).
bool(5, X, Y, Z) :-		% X #\/ Y   #<=> #\Z i.e. [\X | \Z], [\Y | \Z], [X | Y | Z]
	'$fd_debug'(off, off, 1), !,
	bool_or_raw([#\ X, #\ Z], []),
	bool_or_raw([#\ Y, #\ Z], []),
	bool_or_raw([X, Y, Z], []).
bool(6, X, Y, Z) :-		% X #\ Y    #<=> Z i.e. [\X | Y | Z], [X | \Y | Z], [X | Y | \Z], [\X | \Y | \Z],
	'$fd_debug'(off, off, 1), !,
	bool_or_raw([#\ X, Y, Z], []),
	bool_or_raw([X, #\ Y, Z], []),
	bool_or_raw([X, Y, #\ Z], []),
	bool_or_raw([#\ X, #\ Y, #\ Z], []).
bool(7, X, Y, Z) :-		% X #\ #\Y  #<=> Z i.e. [X | Y | Z], [X | \Y | \Z], [\X | Y | \Z], [\X | \Y | Z],
	'$fd_debug'(off, off, 1), !,
	bool_or_raw([X, Y, Z], []),
	bool_or_raw([X, #\ Y, #\ Z], []),
	bool_or_raw([#\ X, Y, #\ Z], []),
	bool_or_raw([#\ X, #\ Y, Z], []).
bool(0, X, Y, Z) :-		% X #/\ Y   #<=> Z i.e. [X | \Z], [Y | \Z], [\X | \Y | Z]
	bool_and([X,Y], Z).
bool(1, X, Y, Z) :-		% X #/\ #\Y #<=> Z i.e. [X | \Z], [\Y | \Z], [\X | Y | Z]
	bool_and([X,#\Y], Z).
bool(2, X, Y, Z) :-		% X #/\ Y   #<=> #\Z i.e. [X | Z], [Y | Z], [\X | \Y | \Z]
	bool_and([X,Y], #\Z).
bool(3, X, Y, Z) :-		% X #\/ Y   #<=> Z i.e. [\X | Z], [\Y | Z], [X | Y | \Z]
	bool_or([X,Y], Z).
bool(4, X, Y, Z) :-		% X #\/ #\Y #<=> Z i.e. [\X | Z], [Y | Z], [X | \Y | \Z]
	bool_or([X,#\Y], Z).
bool(5, X, Y, Z) :-		% X #\/ Y   #<=> #\Z i.e. [\X | \Z], [\Y | \Z], [X | Y | Z]
	bool_or([X,Y], #\Z).
bool(6, X, Y, Z) :-		% X #\ Y    #<=> Z i.e. [\X | Y | Z], [X | \Y | Z], [X | Y | \Z], [\X | \Y | \Z],
	bool_xor([X,Y], Z).
bool(7, X, Y, Z) :-		% X #\ #\Y  #<=> Z i.e. [X | Y | Z], [X | \Y | \Z], [\X | Y | \Z], [\X | \Y | Z],
	bool_xor([X,#\Y], Z).

% FOR BACKWARD COMPATIBILITY!

'\\p'(P, B) :- bool(6, P, B, 1).

'p/\\q'(P, Q, B) :- bool(0, P, Q, B).

'p\\q'(P, Q, B) :- bool(6, P, Q, B).

'p\\/q'(P, Q, B) :- bool(3, P, Q, B).

'p=>q'(P, Q, B) :- bool(4, Q, P, B).

'p<=>q'(P, Q, B) :- bool(7, P, Q, B).

/****************************************************************/
/* domain/3							*/
/****************************************************************/

domain(Vars, Min, Max) :-
	Goal = domain(Vars,Min,Max),
	must_be_dvar_list(Vars, Goal, 1),
	set_expression_check(Min..Max, Set, Goal, 0),
	domain(Vars, Set).

% FDBG puts advice on this!
domain([], _Set) :- !.
domain(Vars, Set) :-
	Set = [[A|B]], !,
	domain1(Vars, A, B, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
domain(Vars, Set) :-
	domain2(Vars, Set, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

% interval
domain1([], _, _, _).
domain1([X|Xs], A, B, Init) :-
	'$fd_in_interval'(X, A, B, Init),
	domain1(Xs, A, B, 0).

% set --- must maintain copies if multiple occs
domain2([], _, _).
domain2([X|Xs], Set, 1) :- !,
	'$fd_in_set'(X, Set, 1),
	domain2(Xs, Set, 0).
domain2([X|Xs], Set, 0) :-
	'$fd_dom_union'(Set, [], Copy),	% Set could occur multiple times
	'$fd_in_set'(X, Copy, 0),
	domain2(Xs, Set, 0).

%%% predicates corresponding to macro-expanded constraints

X in Expr :-
	in_aux_rt(X, Expr).

X in_set Set :-
	in_set_aux_rt(X, Set).

X #= Y :-
	fd_goal_expansion(X #= Y, clpfd, Goal),
	Goal.

X #\= Y :-
	fd_goal_expansion(X #\= Y, clpfd, Goal),
	Goal.

X #< Y :-
	fd_goal_expansion(X #< Y, clpfd, Goal),
	Goal.

X #=< Y :-
	fd_goal_expansion(X #=< Y, clpfd, Goal),
	Goal.

X #> Y :-
	fd_goal_expansion(X #> Y, clpfd, Goal),
	Goal.

X #>= Y :-
	fd_goal_expansion(X #>= Y, clpfd, Goal),
	Goal.

:- meta_predicate #\(:).
#\ Q :-
	fd_goal_expansion(#\ Q, clpfd, Goal),
	Goal.

:- meta_predicate #/\(:,:).
P #/\ Q :-
	fd_goal_expansion(P #/\ Q, clpfd, Goal),
	Goal.

:- meta_predicate #\(:,:).
P #\ Q :-
	fd_goal_expansion(P #\ Q, clpfd, Goal),
	Goal.

:- meta_predicate #\/(:,:).
P #\/ Q :-
	fd_goal_expansion(P #\/ Q, clpfd, Goal),
	Goal.

:- meta_predicate #=>(:,:).
P #=> Q :-
	fd_goal_expansion(P #=> Q, clpfd, Goal),
	Goal.

:- meta_predicate #<=(:,:).
P #<= Q :-
	fd_goal_expansion(P #<= Q, clpfd, Goal),
	Goal.

% [PM] 4.3.1 Meta declaration moved to top of file, before first use: :- meta_predicate #<=>(:,:).
P #<=> Q :-
	fd_goal_expansion(P #<=> Q, clpfd, Goal),
	Goal.

/****************************************************************/
/* disjoint1/[1,2]                          			*/
/****************************************************************/

disjoint1(Items) :-
	disjoint1(Items, []).

disjoint1(Items, Options) :-
	Goal = disjoint1(Items,Options),
	must_be(Options, proper_list, Goal, 2),
	disjoint1_options(Options, opt(0,inf,sup,[]), Opt, Goal, 2),
	(   Opt = opt(Flags,Min,B,_),
	    Flags /\ 2 =:= 2
	->  Max is B-1
	;   Min = inf, Max = sup
	),
	mkitems(Items, Items2, Min, Max, Goal, Susp, []),
	length(Items2, N),
	fd_global_internal(Goal, f(N,Opt,Items2,N,0,_Handle,0), Susp, _, clpfd:Goal, 4). % non-idempotent


disjoint1_options([], Opt, Opt, _, _).
disjoint1_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    disjoint1_option(X, Goal, Opt0, Opt1) -> true
        ;   illarg(domain(term,disjoint1_option), Goal, ArgNo, X)
        ),
	disjoint1_options(L, Opt1, Opt, Goal, ArgNo).

% opt(0bAMXD,Min,Max,Margins) where
% A = global
% M = Margins \== []
% X = wrap-around
% D = decomposition
% Min..Max is the interval subject to wrap-around
% Margins = list of margin(Type1,Type2,Diff) = list of extra margins
disjoint1_option(global(B), _, opt(Flags0,Min,Max,Ms), opt(Flags,Min,Max,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -9) \/ (Value<<3).
disjoint1_option(wrap(Min,Max), _, opt(Flags0,_,_,Ms), opt(Flags,Min,Max,Ms)) :-
	(   Min==inf, Max==sup -> Flags is Flags0 /\ -3
	;   integer(Min),
	    integer(Max),
	    Min < Max,
	    Flags is (Flags0 /\ -3) \/ 2
	).
disjoint1_option(margin(T1,T2,D), _, opt(Flags0,Min,Max,Tail), opt(Flags,Min,Max,[margin(T1,T2,D)|Tail])) :-
	Flags is (Flags0 /\ -5) \/ 4.


mkitems([], [], _, _, _) --> [].
mkitems([X|Xs], [item(S,SM,D,DM,Type)|Items], Min, Max, Goal) -->
	[minmax(S),min(D)],
	{arg(1,X,S), arg(2,X,D)},
	{arg(3,X,Type) -> true; Type=0},
	{finite_arg_attribute(S, SM, Goal, 1)},
	{finite_arg_attribute(D, DM, Goal, 1)},
	{propagate_interval(S, Min, Max)},
	mkitems(Xs, Items, Min, Max, Goal).

/****************************************************************/
/* disjoint2/[1,2]                          			*/
/****************************************************************/

disjoint2(Items) :-
	disjoint2(Items, []).

disjoint2(Items, Options) :-
	Goal = disjoint2(Items,Options),
	must_be(Options, proper_list, Goal, 2),
	disjoint2_options(Options, opt(0,inf,sup,inf,sup,[]), Opt, Goal, 2),
	Opt = opt(Flags,Min1,B1,Min2,B2,_),
	(   Flags /\ 2 =:= 2
	->  Max1 is B1-1
	;   Min1 = inf, Max1 = sup
	),
	(   Flags /\ 4 =:= 4
	->  Max2 is B2-1
	;   Min2 = inf, Max2 = sup
	),
	mkitems(Items, Items2, Min1, Max1, Min2, Max2, Goal, Susp, []),
	length(Items2, N),
	fd_global_internal(Goal, f(N,Opt,Items2,N,0,_Handle,0), Susp, _, clpfd:Goal, 4). % non-idempotent


disjoint2_options([], Opt, Opt, _, _).
disjoint2_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    disjoint2_option(X, Goal, Opt0, Opt1) -> true
        ;   illarg(domain(term,disjoint2_option), Goal, ArgNo, X)
        ),
	disjoint2_options(L, Opt1, Opt, Goal, ArgNo).

% opt(0bAMYXD,Min1,Max1,Min2,Max2,Margins) where
% A = global
% M = Margins \== []
% Y = wrap-around in Y dim
% X = wrap-around in X dim
% D = decomposition
% Min..Max is the interval subject to wrap-around
% Margins = list of margin(Type1,Type2,Diff1,Diff2) = list of extra margins
disjoint2_option(global(B), _,
		 opt(Flags0,Min1,Max1,Min2,Max2,Ms),
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -17) \/ (Value<<4).
disjoint2_option(wrap(Min1,Max1,Min2,Max2), _,
		 opt(Flags0,_,_,_,_,Ms),
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	(   Min1==inf, Max1==sup -> Flags1 is (Flags0 /\ -3)
	;   integer(Min1),
	    integer(Max1),
	    Min1 < Max1,
	    Flags1 is (Flags0 /\ -3) \/ 2
	),
	(   Min2==inf, Max2==sup -> Flags is (Flags1 /\ -5)
	;   integer(Min2),
	    integer(Max2),
	    Min2 < Max2,
	    Flags is (Flags1 /\ -5) \/ 4
	).
disjoint2_option(margin(T1,T2,D1,D2), _,
		 opt(Flags0,Min1,Max1,Min2,Max2,Tail),
		 opt(Flags,Min1,Max1,Min2,Max2,[margin(T1,T2,D1,D2)|Tail])) :-
	Flags is (Flags0 /\ -9) \/ 8.
disjoint2_option(synchronization(B), _,
		 opt(Flags0,Min1,Max1,Min2,Max2,Ms),
		 opt(Flags,Min1,Max1,Min2,Max2,Ms)) :-
	bool_option(B, Value),
	Flags is (Flags0 /\ -65) \/ (Value<<6).


% S1,S2 - start variables
% SM1,SM2 - start domain mutables
% D1,D2 - durations
% Type - type of object (optional)
mkitems([], [], _, _, _, _, _) --> [].
mkitems([X|Xs], [item(S1,SM1,D1,DM1,S2,SM2,D2,DM2,Type)|Items],
	Min1, Max1, Min2, Max2, Goal) -->
	[minmax(S1),min(D1),minmax(S2),min(D2)],
	{arg(1,X,S1), arg(2,X,D1), arg(3,X,S2), arg(4,X,D2)},
	{arg(5,X,Type) -> true; Type=0},
	{finite_arg_attribute(S1, SM1, Goal, 1)},
	{finite_arg_attribute(D1, DM1, Goal, 1)},
	{finite_arg_attribute(S2, SM2, Goal, 1)},
	{finite_arg_attribute(D2, DM2, Goal, 1)},
	{propagate_interval(S1, Min1, Max1)},
	{propagate_interval(S2, Min2, Max2)},
	mkitems(Xs, Items, Min1, Max1, Min2, Max2, Goal).

/****************************************************************/
/* cumulatives/[2,3]  						*/
/****************************************************************/

cumulatives(Tasks0, Machines) :-
	cumulatives(Tasks0, Machines, []).

cumulatives(Tasks, Machines1, Options) :-
	Goal = cumulatives(Tasks, Machines1, Options),
	must_be(Tasks, proper_list(callable), Goal, 1),
	must_be(Machines1, proper_list(callable), Goal, 2),
	must_be(Options, proper_list, Goal, 3),
	sort(Machines1, Machines2),
	(   foreach(machine(M,C),Machines2),
	    foreach(machine(M,C-CA),Machines3),
	    foreach(M,Mids1),
	    foreach(minmax(C),S1),
	    param(Goal)
	do  must_be(M, integer, Goal, 2),
	    finite_arg_attribute(C, CA, Goal, 2)
	),
	sort(Mids1, Mids2),
	list_to_fdset(Mids2, MidSet),
	cumulatives_options(Options, 0, Opt, Goal, 3),
	(   foreach(task(Org,Dur,End,Height,Machine),Tasks),
	    foreach(task(Org-OA,Dur-DA,End-EA,Height-HA,Machine-MA),Tasks2),
	    fromto(Susp,S0,S,S1),
	    param(MidSet,Goal)
	do  Machine in_set MidSet,
	    atleast2_finite_arg_attributes(Org, OA, Dur, DA, End, EA, Goal, 1),
	    finite_arg_attribute(Height, HA, Goal, 1),
	    finite_arg_attribute(Machine, MA, Goal, 1),
	    S0 = [minmax(Org),minmax(Dur),minmax(End),minmax(Height),dom(Machine)|S]
	),
	length(Tasks2, NT),
	length(Machines3, NM),
	fd_global_internal(Goal, f(NT,NM,Opt,Tasks2,Machines3,NT,0,_Handle,0), Susp, _, clpfd:Goal, 4). % non-idempotent

cumulatives_options([], Opt, Opt, _, _).
cumulatives_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    cumulatives_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,cumulatives_option), Goal, ArgNo, X)
        ),
	cumulatives_options(L, Opt1, Opt, Goal, ArgNo).

%%% Valid options:
%%% bound(lower|upper)   ==> 0x0|0x1
%%% generalization(Bool) ==> 0x0|0x2
%%% task_intervals(Bool) ==> 0x0|0x4
%%% prune(all|next)      ==> 0x0|0x8

cumulatives_option(bound(B), Opt0, Opt) :-
	aux_option(bound, B, Value),
	Opt is (Opt0 /\ -2) \/ Value.
cumulatives_option(generalization(B), Opt0, Opt) :-
	bool_option(B, Value),
	Opt is (Opt0 /\ -3) \/ (Value<<1).
cumulatives_option(task_intervals(B), Opt0, Opt) :-
	bool_option(B, Value),
	Opt is (Opt0 /\ -9) \/ (Value<<2).
cumulatives_option(prune(B), Opt0, Opt) :-
	aux_option(prune, B, Value),
	Opt is (Opt0 /\ -17) \/ (Value<<3).

/****************************************************************/
/* global_cardinality/[2,3]					*/
/****************************************************************/

global_cardinality(Xs, Ys) :-
	global_cardinality(Xs, Ys, []).

global_cardinality(Xs, Ys, Opt) :-
	global_cardinality(Xs, Ys, Opt, opt(global,dom,0,[]), global_cardinality(Xs,Ys,Opt)).

global_cardinality(Vars, Vals1, Options, Opt0, Goal) :-
	must_be(Vals1, proper_list(pair), Goal, 2),
	must_be(Options, proper_list, Goal, 3),
	gcc_options(Options, Opt0, Opt, Goal, 3),
	Opt = opt(Cons,On,Flag,Cost),	% Cost is [] or cost(UB,Matrix)
	must_be_dvar_list(Vars, Goal, 1),
	length(Vars, N),
	(   foreach(K-Count,Vals1),
	    foreach(K-(Count-CA),Vals2),
	    foreach(K,KeyList),
	    foreach(Count,Counts),
	    param(Goal)
	do  arg_attribute(Count, CA, Goal, 2)
	),
	list_to_fdset(KeyList, KeySet),
	length(KeyList, M),
	'$fd_size'(KeySet, M, 1), !, % no duplicates
	keysort(Vals2, SVals),
	domain(Vars, KeySet),
	domain(Counts, 0, N),
	global_cardinality_implied(Vars, KeyList, Counts, N),
	(   foreach(X,Vars),
	    fromto(Susp,Susp1,Susp2,Susp3),
	    param(On)
	do  on(On, X, Susp1, Susp2)
	),
	(   foreach(C1,Counts),
	    fromto(Susp3,[minmax(C1)|T],T,Susp4)
	do  true
	),
	(   Flag=\=0 ->
	    Cost=cost(C,_,_),
	    Susp4 = [dom(C)],
	    fd_global_internal(Goal,
			       f(N,M,Vars,SVals,Flag,Cost,0/*cost so far*/,_Handle,0),
			       Susp, Global, clpfd:Goal, 0),
	    Global = global(StateM,_,_,_,_),
	    global_cardinality_helpers(Vars, C, 0, StateM, Goal)
	;   Cons==local ->
	    Susp4=[],
	    fd_global_internal(local_cardinality,
			       f(N,M,Vars,SVals,_Handle,0),
			       Susp, _Global, clpfd:Goal, 0)
	;   Susp4=[],
	    fd_global(Goal, f(N,M,Vars,SVals,Flag,Cost,0/*cost so far*/,_Handle,0), Susp)
	).

global_cardinality_implied(Vars, Keys, Counts, N) :-
	(   foreach(V,Vars),
	    fromto(Terms,[V|Terms1],Terms1,Counts),
	    fromto(Coeffs,[-1|Coeffs1],Coeffs1,Keys)
	do  true
	),
	fd_hiding(sum(Counts, #=, N)),
	fd_hiding(scalar_product(Coeffs, Terms, #=, 0)).

global_cardinality_helpers([], _, _, _, _).
global_cardinality_helpers([X|Xs], C, I, StateM, Goal) :-
	fd_global_internal(global_cardinality_helper(X,C), f(I,X,StateM), [val(X)],
			   _, true, 0),
	J is I+1,
	global_cardinality_helpers(Xs, C, J, StateM, Goal).

gcc_options([], Opt, Opt, _, _).
gcc_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    gcc_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,global_cardinality_option), Goal, ArgNo, X)
        ),
	gcc_options(L, Opt1, Opt, Goal, ArgNo).

gcc_option(consistency(Arg), opt(_,_,VM,Mat), opt(Cons,On,VM,Mat)) :-
	consistency_option(Arg, Cons, On).
gcc_option(on(On), opt(Cons,_,VM,Mat), opt(Cons,On,VM,Mat)) :-
	on(On, _, _, _).
gcc_option(cost(V,Mat), opt(Cons,On,_,_), opt(Cons,On,1,cost(V,VM,Mat))) :-
	arg_attribute(V, VM, 0, 3).

/****************************************************************/
/* sorting/3							*/
/****************************************************************/

sorting(Xs, Ps, Ys) :-
	Goal = sorting(Xs,Ps,Ys),
	must_be_dvar_list(Xs, Goal, 1),
	must_be_dvar_list(Ps, Goal, 2),
	must_be_dvar_list(Ys, Goal, 3),
	length(Xs, N),
	length(Ps, N),
	length(Ys, N),
	'$fd_range'(1, N, Set, 1),
	domain(Ps, Set),
	dvar_list_susp(Xs, XVec, minmax, Goal, 1, Susp , Susp1),
	dvar_list_susp(Ps, PVec, minmax, Goal, 2, Susp1, Susp2),
	dvar_list_susp(Ys, YVec, minmax, Goal, 3, Susp2, []),
	fd_global(Goal, f(XVec,PVec,YVec,N,
			  0/*NShaved*/,0/*YOffset*/,_Handle,0), Susp).

/****************************************************************/
/* keysorting/[2,3]						*/
/****************************************************************/

keysorting(Xs, Ys) :-
	keysorting(Xs, Ys, []).

keysorting(Xs, Ys, Options) :-
	Goal = keysorting(Xs,Ys,Options),
	must_be(Xs, proper_list(proper_list), Goal, 1),
	must_be(Ys, proper_list(proper_list), Goal, 2),
	must_be(Options, proper_list, Goal, 3),
	keysorting_options(Options, opt(1,[]), opt(NK,Ps0), Goal, 3),
	length(Xs, NT),
	(Ps0==[] -> length(Ps, NT) ; Ps = Ps0),
	domain(Ps, 1, NT),
	(   foreach(X,Xs),
	    foreach(P,Ps),
	    foreach(Y,Ys),
	    foreach(X1,Xs1),
	    foreach(Y1,Ys1),
	    for(I,1,NT),
	    param(NK,NKV)
	do  length(X, NKV),
	    length(Y, NKV),
	    prefix_length(X, XP, NK),
	    append(XP, XS, X),
	    append(XP, [I|XS], X1),
	    prefix_length(Y, YP, NK),
	    append(YP, YS, Y),
	    append(YP, [P|YS], Y1)
	),
	NV is NKV-NK,
	append(Xs1, XFlat),
	append(Ys1, YFlat),
	must_be_dvar_list(XFlat, Goal, 1),
	must_be_dvar_list(YFlat, Goal, 2),
	dvar_list_susp(XFlat, XVec, minmax, Goal, 1, Susp , Susp1),
	dvar_list_susp(YFlat, YVec, minmax, Goal, 2, Susp1, []),
	fd_global(Goal, f(XVec,YVec,NT,NK,NV,
			  0/*NShaved*/,0/*YOffset*/,_Handle,0), Susp).

keysorting_options([], Opt, Opt, Goal, ArgNo) :-
	Opt = opt(Keys,Perm),
	must_be(Keys, integer, Goal, ArgNo),
	must_be_dvar_list(Perm, Goal, ArgNo).
keysorting_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    keysorting_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,keysorting_option), Goal, ArgNo, X)
        ),
	keysorting_options(L, Opt1, Opt, Goal, ArgNo).

keysorting_option(keys(Keys), opt(_,Perm), opt(Keys,Perm)) :-
	integer(Keys).
keysorting_option(permutation(Perm), opt(Keys,_), opt(Keys,Perm)).

/****************************************************************/
/* lex_chain/[2,3]						*/
/****************************************************************/

lex_chain(Tuples) :-
	lex_chain(Tuples, []).

lex_chain(Tuples, Options) :-
	Goal = lex_chain(Tuples, Options),
	must_be(Tuples, proper_list(proper_list), Goal, 1),
	must_be(Options, proper_list, Goal, 2),
	lex_chain_options(Options, opt(0,[]), Opt, Goal, 2),
	length(Tuples, NT),
	(   foreach(T,Tuples),
	    foreach(R,Matrix),
	    foreach(Susp,Susps),
	    param(NV,Goal)
	do  length(T, NV),
	    must_be_list_of_finite_dvar(T, Goal, 1),
	    dvar_list_susp(T, R, none, Goal, 1, Susp, [])
	),
	Opt = opt(Flag,Among),
	(   Flag/\0x2 =:= 0 -> true
	;   Among = among(L,U,Vals),
	    Vals\==[], L=<U -> true
	;   illarg(consistency(Options,Options,''), Goal, 2)
	),
	(NT<3 -> Flag1 is Flag\/0x8 ; Flag1 = Flag),
	(   Flag1/\0x8 =:= 0x8 /* at most 2 or global(true) */ ->
	    append(Susps, SuspG),
	    length(EntFlags, NT), % keep track of entailed pairs
	    fd_global(Goal, state(Matrix,NT,NV,Flag,Among,EntFlags,_/*handle*/,0/*stamp*/),
		      SuspG)
	;   (   fromto(Tuples,[Tuple1,Tuple2|Ts],[Tuple2|Ts],[_]),
		fromto(Matrix,[Row1,Row2|Rs],    [Row2|Rs],  [_]),
		fromto(Susps, [Susp1,Susp2|Ss],  [Susp2|Ss], [_]),
		param(Options,NV,Flag,Among)
	    do  append(Susp1, Susp2, Susp12),
	        fd_global(lex_chain([Tuple1,Tuple2],Options),
			  state([Row1,Row2],2,NV,Flag,Among,[_,_],_/*handle*/,0/*stamp*/),
			  Susp12)
	    )
	).


lex_chain_options([], Opt, Opt, _, _).
lex_chain_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    lex_chain_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,lex_chain_option), Goal, ArgNo, X)
        ),
	lex_chain_options(L, Opt1, Opt, Goal, ArgNo).

lex_chain_option(op(#=<), opt(F0,A), opt(F,A)) :- !,
	F is (F0/\0xe).
lex_chain_option(op(#<), opt(F0,A), opt(F,A)) :- !,
	F is (F0/\0xe)\/0x1.
lex_chain_option(increasing, opt(F0,A), opt(F,A)) :- !,
	F is (F0/\0xb)\/0x4.
lex_chain_option(among(L,U,Vals0), opt(F0,_), opt(F,among(L,U,Vals))) :- !,
	F is (F0/\0xd)\/0x2,
	list_to_fdset(Vals0, Vals).
lex_chain_option(global(B), opt(F0,A), opt(F,A)) :- !,
	bool_option(B, Value),
	F is (F0/\0x7)\/(Value<<3).


/****************************************************************/
/* experimental smt/1       					*/
/****************************************************************/

smt(Formula) :-
	smt_to_case(Formula, info(CaseGoal,_,_,_)),
	call(CaseGoal).

smt_to_case(Formula0, info(CaseGoal,_,_,_)) :-
	prolog:get_module_meta_arg(Formula0, Formula, Module),
	formula_to_case(Formula, Module, CaseGoal).

formula_to_case(Formula1, Module, Expanded) :-
	implicit_booleans(Formula1, Module),
	reify_lias(Formula1, Module, Formula2, L2B),
	empty_avl(Empty),
	(   foreach(L-B,L2B),
	    foreach(B,Bs),
	    fromto(Empty,B2L1,B2L2,B2L3)
	do  B in 0..1,
	    avl_store(B, B2L1, L, B2L2)
	),
	by_descending_occs(Formula2, VarOrder1, B2L3), % by descending #occs
	list_to_avl(VarOrder1, Var2Depth1),
	formula_to_tree(Formula2, PMDD1, Var2Depth1),
	(   simple(PMDD1) -> Expanded = PMDD1
	;   Expanded = case(Template,Tuples,Dag,[],smt(Formula1)),
	    relax_lias(PMDD1, PMDD1a, Var2Depth1, B2L3),
	    insert_lias(PMDD1a, PMDD2, B2L3),
	    bag_of_varoccs(PMDD2, L1, []),
	    (   foreach(V-1,L1),
		foreach(V,L2)
	    do  true
	    ),
	    sort(L2, Vars2),
				% remove dead variables
	    (   foreach(V1-_,VarOrder1),
		fromto(VarOrder2,VO3,VO4,VO5),
		param(Vars2)
	    do  (   ord_member(V1, Vars2) -> VO3 = [V1|VO4] ; VO3 = VO4   )
	    ),
				% add LIA variables
	    (   foreach(V2,Vars2),
		fromto(VO5,VO6,VO7,[]),
		param(Var2Depth1)
	    do  (   avl_fetch(V2, Var2Depth1, _) -> VO6 = VO7 ; VO6 = [V2|VO7]   )
	    ),
	    (   Bs==[] ->
		VarOrder2 = VarOrder3,
		PMDD2 = PMDD3
	    ;   sort(Bs, BSet),
		scan_for_bad_links(PMDD2, Bad0, []),
		sort(Bad0, Bad),
		compress_varorder(VarOrder2, VarOrder3, BSet, Bad, keep),
		sort(VarOrder3, Precious),
		compress_tree(PMDD2, PMDD3, Precious)
	    ),
	    complete_tree(VarOrder3, PMDD3, PMDD4),
	    tree_to_dag(VarOrder3, L2B, PMDD4, Template, Tuples, Dag)
	).

% Detect variable pairs that would lead to aggregated LIAs if coalesced
scan_for_bad_links(true) --> [].
scan_for_bad_links(or(L1)) -->
	(   foreach(and(in(X,_),LIAs,T),L1)
	do  (   {LIAs = [_|_]},
		{T = or(L2)},
		{member(and(in(Y,_),[_|_],_), L2)} -> [X-Y]
	    ;   []
	    ),
	    scan_for_bad_links(T)
	).

reify_lias(F1, M, F2, L2B) :-
	reify_lias_loop(F1, M, F2, KL1, []),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(LIA-Bs,KL3),
	    foreach(LIA-B ,L2B)
	do  (   foreach(B,Bs),
		param(B)
	    do  true
	    )
	).

reify_lias_loop(F1, M, F4) -->
	reify_lias(F1, M, F2, R0, R1),
	(   foreach(reified(M1,Ctr,B),R0),
	    fromto(F2,F3,F3#/\G,F4),
	    fromto(R1,R2,R3,[])
	do  reify_lias(Ctr#<=>B, M1, G, R2, R3)
	).

reify_lias(X, _, Exp, Reif, Reif) --> {var(X)}, !,
	{Exp = (X in 1..1)}.
reify_lias(X, _, X, Reif, Reif) --> {simple(X)}, !.
reify_lias(X in R, _, Exp, Reif, Reif) --> !,
	{   var(X) -> Exp = (X in R)
	;   X in R -> Exp = true
	;   Exp = false
	}.
reify_lias(X in_set S, _, Exp, Reif, Reif) --> !,
	{   var(X) -> Exp = (X in_set S)
	;   X in_set S -> Exp = true
	;   Exp = false
	}.
reify_lias(#\ F2, M, #\ G2, Reif0, Reif) --> !,
	reify_lias(F2, M, G2, Reif0, Reif).
reify_lias(F1 #/\ F2, M, G1 #/\ G2, Reif0, Reif) --> !,
	reify_lias(F1, M, G1, Reif0, Reif1),
	reify_lias(F2, M, G2, Reif1, Reif).
reify_lias(F1 #\/ F2, M, G1 #\/ G2, Reif0, Reif) --> !,
	reify_lias(F1, M, G1, Reif0, Reif1),
	reify_lias(F2, M, G2, Reif1, Reif).
reify_lias(F1 #\ F2, M, G1 #\ G2, Reif0, Reif) --> !,
	reify_lias(F1, M, G1, Reif0, Reif1),
	reify_lias(F2, M, G2, Reif1, Reif).
reify_lias(F1 #=> F2, M, G1 #=> G2, Reif0, Reif) --> !,
	reify_lias(F1, M, G1, Reif0, Reif1),
	reify_lias(F2, M, G2, Reif1, Reif).
reify_lias(F1 #<= F2, M, G1 #<= G2, Reif0, Reif) --> !,
	reify_lias(F1, M, G1, Reif0, Reif1),
	reify_lias(F2, M, G2, Reif1, Reif).
reify_lias(F1 #<=> F2, M, G1 #<=> G2, Reif0, Reif) --> !,
	reify_lias(F1, M, G1, Reif0, Reif1),
	reify_lias(F2, M, G2, Reif1, Reif).
reify_lias(element(I,L,Y), _, Expanded, Reif0, Reif) --> !,
	(   foreach(X,L),
	    foreach(Disjunct,Disjuncts),
	    count(J,1,_),
	    fromto(Reif0,Reif1,Reif2,Reif),
	    param(I,Y,M)
	do  reify_lias(I#=J #/\ Y#=X, M, Disjunct, Reif1, Reif2)
	),
	{orify(Disjuncts, Expanded)}.
reify_lias(table(Tuples,Ext), M, Expanded, Reif0, Reif) --> !,
	(   foreach(Tuple,Tuples),
	    foreach(Conjunct,Conjuncts),
	    fromto(Reif0,Reif1,Reif6,Reif),
	    param(Ext,M)
	do  (   foreach(Row,Ext),
		foreach(Case,Disjuncts),
		fromto(Reif1,Reif2,Reif5,Reif6),
		param(Tuple,M)
	    do  (   foreach(X,Tuple),
		    foreach(Y,Row),
		    foreach(C,Cs),
		    fromto(Reif2,Reif3,Reif4,Reif5),
		    param(M)
		do  {simple(Y) -> C0 = (X#=Y) ; C0 = (X in Y)},
		    reify_lias(C0, M, C, Reif3, Reif4)
		),
		{andify(Cs, Case)}
	    ),
	    {orify(Disjuncts,Conjunct)}
	),
	{andify(Conjuncts, Expanded)}.
reify_lias(Expr, M, Expanded, Reif0, Reif) -->
	{arithop2(Expr, Rel, X, Y)}, !,
	{linearize_rel(X, Rel, Y, M, Subs, [])},
	(   fromto(Subs,[R|Subs1],Subs1,[linear(E1,Rel,E2)]),
	    fromto(Reif0,[R|Reif1],Reif1,Reif)
	do  []
	),
	{normalize(E1-E2, 1, Poly0, [])},
	{keysort(Poly0, Poly1)},
	{keyfuse(Poly1, Poly2)},
	{isolate(Poly2, A, Z, As1, Xs1, Sum)},
	{   A =< 0 -> As2 = As1, Rel1 = Rel, Sum1 = Sum
	;   Sum1 is -Sum,
	    rel_inverse(Rel, Rel1),
	    (   foreach(P,As1),
		foreach(N,As2)
	    do  N is -P
	    )
	},
	reify_scalar_product(As2, Xs1, Sum1, Rel1, Z, Expanded).

arithop2(A #= B, #=, A, B).
arithop2(A #\= B, #\=, A, B).
arithop2(A #< B, #=<, A, B-1).
arithop2(A #=< B, #=<, A, B).
arithop2(A #> B, #>=, A, B+1).
arithop2(A #>= B, #>=, A, B).

reify_scalar_product([], [], S, Rel, X, Expanded) --> {integer(X)}, !,
	{evalop(Rel, S, X, Expanded)}.
reify_scalar_product([], [], S, Rel, X, Expanded) --> !,
	{rel_inverse(Rel, Inv)},
	{Expanded=..[Inv,X,S]}.
reify_scalar_product([A], [Y], S, Rel, X, Expanded) --> {integer(X)}, !,
	{HL is X-S},
	{unary_scp(Rel, A, Y, HL, Expanded)}.
reify_scalar_product(As, Xs, S, Rel, X, F) -->
	(   {integer(X)}
	->  {S1 is X-S},
	    canonical_scp(Rel, As, Xs, S1, F)
	;   {S1 is -S},
	    canonical_scp(Rel, [-1|As], [X|Xs], S1, F)
	).

unary_scp(#=<, A, X, S, F) :- A < 0, !,
	B is -A,
	T is -S,
	unary_scp(#>=, B, X, T, F).
unary_scp(#>=, A, X, S, F) :- A < 0, !,
	B is -A,
	T is -S,
	unary_scp(#=<, B, X, T, F).
unary_scp(#=, A, X, S, F) :- A < 0, !,
	B is -A,
	T is -S,
	unary_scp(#=, B, X, T, F).
unary_scp(#\=, A, X, S, F) :- A < 0, !,
	B is -A,
	T is -S,
	unary_scp(#\=, B, X, T, F).
unary_scp(#=<, A, X, S, X#=<T) :-
	(S>=0 -> T is S//A ; T is (S+1)//A-1).
unary_scp(#>=, A, X, S, X#>=T) :-
	(S=<0 -> T is S//A ; T is (S-1)//A+1).
unary_scp(#\=, A, X, S, F) :-
	T is S//A,
	(A*T =:= S -> F = (X#\=T) ; F = true).
unary_scp(#=, A, X, S, F) :-
	T is S//A,
	(A*T =:= S -> F = (X#=T) ; F = false).

canonical_scp(#=<, As, Xs, S, B1) --> [scalar_product(As,Xs,#=<,S)-B1].
canonical_scp(#>=, As, Xs, S, B1) --> [scalar_product(Bs,Xs,#=<,T)-B1],
	{T is -S},
	{   foreach(A,As),
	    foreach(B,Bs)
	do  B is -A
	}.
canonical_scp(#=, As, Xs, S, B1#/\B2) -->
	    [scalar_product(As,Xs,#=<,S)-B1],
	    [scalar_product(Bs,Xs,#=<,T)-B2],
	{T is -S},
	{   foreach(A,As),
	    foreach(B,Bs)
	do  B is -A
	}.
canonical_scp(#\=, As, Xs, S, B1#\/B2) -->
	    [scalar_product(As,Xs,#=<,S1)-B1],
	    [scalar_product(Bs,Xs,#=<,T1)-B2],
	{S1 is S-1},
	{T1 is -S-1},
	{   foreach(A,As),
	    foreach(B,Bs)
	do  B is -A
	}.

by_descending_occs(Formula, VarOrder, _B2L) :-
	bag_of_varoccs(Formula, L1, []),
	keysort(L1, L2),
	keyclumped(L2, L3),
	length(L3, I),
	(   foreach(Var-Ones,L3),
	    foreach(Weight-Var,L4),
	    foreach(_-V1,L5),
	    foreach(V1-J,VarOrder),
	    count(J,-I,_)
	do  length(Ones, Len),
	    Weight is -Len
	),
	keysort(L4, L5).

bag_of_varoccs(X) --> {var(X)}, !, [X-1].
bag_of_varoccs(X) --> {atomic(X)}, !.
bag_of_varoccs(X) --> {ground(X)}, !.
bag_of_varoccs([X|Xs]) --> !,
	(   foreach(Y,[X|Xs])
	do  bag_of_varoccs(Y)
	).
bag_of_varoccs(X) -->
	(   foreacharg(Y,X)
	do  bag_of_varoccs(Y)
	).

relax_lias(true, true, _, _).
relax_lias(false, false, _, _).
relax_lias(or(L1), or(L2), VarOrder, B2L) :-
	L1 = [and(in(X,[[0|0]]),Sub1),and(in(X,[[1|1]]),Sub2)],
	avl_fetch(X, B2L, _), !,
	L2 = [and(in(X,Set1),Sub3),and(in(X,Set2),Sub4)],
	pmdd_and(Sub1, Sub2, Sub12, VarOrder),
	(   Sub1==Sub12 ->	% Sub1 implies Sub2
	    Set1 = [[0|1]], Set2 = [[1|1]]
	;   Sub2==Sub12 ->	% Sub2 implies Sub1
	    Set1 = [[0|0]], Set2 = [[0|1]]
	;   Set1 = [[0|0]], Set2 = [[1|1]]
	),
	relax_lias(Sub1, Sub3, VarOrder, B2L),
	relax_lias(Sub2, Sub4, VarOrder, B2L).
relax_lias(or(L1), or(L2), VarOrder, B2L) :-
	(   foreach(and(in(X,Set),Sub1),L1),
	    foreach(and(in(X,Set),Sub2),L2),
	    param(VarOrder,B2L)
	do  relax_lias(Sub1, Sub2, VarOrder, B2L)
	).

insert_lias(P1, P2, B2L) :-
	insert_lias(P1, P2, B2L, top, [], []).

insert_lias(true, true, _, _) --> [].
insert_lias(false, false, _, _) --> [].
insert_lias(or(L1), P, B2L, rec) -->
	{L1 = [and(in(X,Set),Sub)]},
	{avl_fetch(X, B2L, LIA)}, !,
	(   {Set==[[1|1]]} -> [LIA]
	;   {Set==[[0|0]]} -> [LIAi],
	    {invert_lia(LIA, LIAi)}
	;   {Set==[[0|1]]} -> []
	),
	insert_lias(Sub, P, B2L, rec).
insert_lias(or(L1), or(L2), B2L, _) -->
	{   foreach(and(in(X,Set),Sub1),L1),
	    foreach(and(in(X,Set2),LIAs,Sub2),L2),
	    param(B2L)
	do  (   avl_fetch(X, B2L, LIA) -> % X is a dummy
		Set2 = [[0|0]],
		(   Set==[[1|1]] -> LIAs = [LIA|LIAt]
		;   Set==[[0|0]] -> LIAs = [LIAi|LIAt],
		    invert_lia(LIA, LIAi)
		;   LIAs = LIAt
		)
	    ;   LIAs = LIAt,	% X is not a dummy
		Set2 = Set
	    ),
	    insert_lias(Sub1, Sub2, B2L, rec, LIAt, [])
	}.

invert_lia(scalar_product(As,Xs,Rel,S),
	   scalar_product(Bs,Xs,Rel,T)) :-
	T is -S-1,
	(   foreach(A,As),
	    foreach(B,Bs)
	do  B is -A
	).

complete_tree([], T, T).
complete_tree([V|Vas], or(L1), or(L2)) :-
	L1 = [and(in(X,_),_,_)|_],
	X==V, !,
	(   foreach(and(XinS,LIAs,Sub1),L1),
	    foreach(and(XinS,LIAs,Sub2),L2),
	    param(Vas)
	do  complete_tree(Vas, Sub1, Sub2)
	).
complete_tree([V|Vas], P1, or([and(in(V,MM),[],P2)])) :-
	fd_min(V, Min),
	fd_max(V, Max),
	'$fd_range'(Min, Max, MM, 1),
	complete_tree(Vas, P1, P2).

compress_varorder([], [], _, _, _).
compress_varorder([X|Xs], Ys, BSet, Bad, skip(X0)) :-
	ord_member(X, BSet),
	ord_nonmember(X0-X, Bad), !,
	compress_varorder(Xs, Ys, BSet, Bad, skip(X)).
compress_varorder([X|Xs], [X|Ys], BSet, Bad, keep) :-
	ord_member(X, BSet), !,
	compress_varorder(Xs, Ys, BSet, Bad, skip(X)).
compress_varorder([X|Xs], [X|Ys], BSet, Bad, _) :-
	compress_varorder(Xs, Ys, BSet, Bad, keep).

compress_tree(true, true, _).
compress_tree(or(L1), or(L2), Precious) :-
	(   foreach(and(XinS,LIAs,Sub),L1),
	    fromto(L2,L3,L4,[]),
	    param(Precious)
	do  distribute_disjunct(Sub, LIAs, XinS, Precious, L3, L4)
	).

distribute_disjunct(or(L1), LIAs1, XinS, Precious) -->
	{L1 = [and(in(X,_),_,_)|_]},
	{ord_nonmember(X, Precious)}, !,
	(   foreach(and(_,LIAs2,Sub),L1),
	    param(LIAs1,XinS,Precious)
	do  {append(LIAs1, LIAs2, LIAs3)},
	    distribute_disjunct(Sub, LIAs3, XinS, Precious)
	).
distribute_disjunct(Tree0, LIAs, XinS, Precious) -->
	[and(XinS,LIAs,Tree)],
	{compress_tree(Tree0, Tree, Precious)}.

%% N.B. PMDD contains real vas, not template vas
tree_to_dag(VarOrder1, L2B, PMDD1, VarOrder2, [VarOrder1], Dag) :-
	copy_term(f(VarOrder1,PMDD1), f(VarOrder2,PMDD2)),
	(   foreach(_-0,L2B)
	do  true
	),
	empty_avl(Chs2ID0),
	tree_to_id(PMDD2, _, Chs2ID0, Chs2ID, Nodes, []),
	reverse(Nodes, Dag),
	avl_to_list(Chs2ID, L1),
	(   foreach(_-ID,L1),
	    count(ID,1,_)
	do  true
	).

tree_to_id(true, [], Chs2ID, Chs2ID) --> [].
tree_to_id(or(L1), ID, Chs2ID1, Chs2ID) -->
	(   foreach(and(in(X,Set),LIAs,Sub1),L1),
	    fromto(Children,Children1,Children3,[]),
	    fromto(Chs2ID1,Chs2ID2,Chs2ID3,Chs2ID4),
	    param(X)
	do  tree_to_id(Sub1, Sub2, Chs2ID2, Chs2ID3),
	    (   foreach([A|B],Set),
		fromto(Children1,[(A..B)-LIAs-Sub2|Children2],Children2,Children3),
		param(LIAs,Sub2)
	    do  []
	    )
	),
	(   {avl_fetch(X-Children, Chs2ID4, ID)} -> {Chs2ID = Chs2ID4}
	;   {avl_store(X-Children, Chs2ID4, ID, Chs2ID)},
	    [node(ID,X,Children)]
	).


formula_to_tree(X, P, _VarOrder) :- var(X), !,	% assumed 0/1
	pmdd_cons([and(in(X,[[1|1]]),true)], P).
formula_to_tree(1, true, _VarOrder) :- !.
formula_to_tree(0, false, _VarOrder) :- !.
formula_to_tree(true, true, _VarOrder) :- !.
formula_to_tree(false, false, _VarOrder) :- !.
formula_to_tree(X in_set Set, P, _VarOrder) :- integer(X), !,
	(   '$fd_dom_contains'(Set, X) -> P = true ; P = false   ).
formula_to_tree(X in_set Set, P, _VarOrder) :- !,
	pmdd_cons([and(in(X,Set),true)], P).
formula_to_tree(X in R, P, _VarOrder) :- integer(X), !,
	range_to_fdset(R, Set),
	(   '$fd_dom_contains'(Set, X) -> P = true ; P = false   ).
formula_to_tree(X in R, P, _VarOrder) :- !,
	range_to_fdset(R, Set),
	pmdd_cons([and(in(X,Set),true)], P).
formula_to_tree(X #=< Con, P, _VarOrder) :- integer(X), !,
	(X =< Con -> P = true ; P = false).
formula_to_tree(X #=< Con, P, _VarOrder) :- !,
	fd_min(X, Min),
	'$fd_range'(Min, Con, MM, 1),
	pmdd_cons([and(in(X,MM),true)], P).
formula_to_tree(X #>= Con, P, _VarOrder) :- integer(X), !,
	(X >= Con -> P = true ; P = false).
formula_to_tree(X #>= Con, P, _VarOrder) :- !,
	fd_max(X, Max),
	'$fd_range'(Con, Max, MM, 1),
	pmdd_cons([and(in(X,MM),true)], P).
formula_to_tree(X #= Con, P, _VarOrder) :- integer(X), !,
	(X =:= Con -> P = true ; P = false).
formula_to_tree(X #= Con, P, _VarOrder) :- !,
	'$fd_range'(Con, Con, CC, 1),
	pmdd_cons([and(in(X,CC),true)], P).
formula_to_tree(X #\= Con, P, _VarOrder) :- integer(X), !,
	(X =\= Con -> P = true ; P = false).
formula_to_tree(X #\= Con, P, _VarOrder) :- !,
	fd_min(X, Min),
	fd_max(X, Max),
	'$fd_range'(Min, Max, MM, 1),
	'$fd_range'(Con, Con, CC, 1),
	'$fd_dom_subtract'(MM, CC, Inters),
	pmdd_cons([and(in(X,Inters),true)], P).
formula_to_tree(#\ F2, P, VarOrder) :- !,
	formula_to_tree(F2, P2, VarOrder),
	pmdd_not(P2, P).
formula_to_tree(F1 #/\ F2, P, VarOrder) :- !,
	formula_to_tree(F1, P1, VarOrder),
	formula_to_tree(F2, P2, VarOrder),
	pmdd_and(P1, P2, P, VarOrder).
formula_to_tree(F1 #\/ F2, P, VarOrder) :- !,
	formula_to_tree(F1, P1, VarOrder),
	formula_to_tree(F2, P2, VarOrder),
	pmdd_or(P1, P2, P, VarOrder).
formula_to_tree(F1 #\ F2, P, VarOrder) :- !,
	formula_to_tree((F1 #/\ #\F2) #\/ (#\F1 #/\ F2), P, VarOrder).
formula_to_tree(F1 #=> F2, P, VarOrder) :- !,
	formula_to_tree(#\F1 #\/ F2, P, VarOrder).
formula_to_tree(F1 #<= F2, P, VarOrder) :- !,
	formula_to_tree(F1 #\/ #\F2, P, VarOrder).
formula_to_tree(F1 #<=> F2, P, VarOrder) :- !,
	formula_to_tree((F1 #/\ F2) #\/ (#\F1 #/\ #\F2), P, VarOrder).

pmdd_cmp(or(L1), or(L2), K, VarOrder) :-
	L1 = [and(in(X1,_),_)|_],
	L2 = [and(in(X2,_),_)|_],
	avl_fetch(X1, VarOrder, D1),
	avl_fetch(X2, VarOrder, D2),
	compare(K, D1, D2).

pmdd_and(true, F, F, _VarOrder) :- !.
pmdd_and(F, true, F, _VarOrder) :- !.
pmdd_and(false, _, false, _VarOrder) :- !.
pmdd_and(_, false, false, _VarOrder) :- !.
pmdd_and(F1, F2, F3, VarOrder) :-
	pmdd_cmp(F1, F2, K, VarOrder),
	pmdd_and(K, F1, F2, F3, VarOrder).

pmdd_and(>, F1, F2, F3, VarOrder) :-
	pmdd_and(<, F2, F1, F3, VarOrder).
pmdd_and(<, or(Disjuncts1), F2, F3, VarOrder) :-
	(   foreach(and(InX,G1),Disjuncts1),
	    foreach(and(InX,G2),Disjuncts3),
	    param(F2,VarOrder)
	do  pmdd_and(G1, F2, G2, VarOrder)
	),
	pmdd_cons(Disjuncts3, F3).
pmdd_and(=, or(Disjuncts1), or(Disjuncts2), F3, VarOrder) :-
	(   fromto(Disjuncts3,D4,D6,[]),
	    foreach(D1,Disjuncts1),
	    param(Disjuncts2,VarOrder)
	do  (   fromto(D4,[and(in(X,Set12),F12)|D5],D5,D6),
		foreach(D2,Disjuncts2),
		param(D1,VarOrder)
	    do  D1 = and(in(X,Set1),F1),
		D2 = and(in(X,Set2),F2),
		'$fd_dom_intersection'(Set1, Set2, Set12),
		pmdd_and(F1, F2, F12, VarOrder)
	    )
	),
	pmdd_cons(Disjuncts3, F3).

pmdd_or(true, _, true, _VarOrder) :- !.
pmdd_or(_, true, true, _VarOrder) :- !.
pmdd_or(false, F, F, _VarOrder) :- !.
pmdd_or(F, false, F, _VarOrder) :- !.
pmdd_or(F1, F2, F3, VarOrder) :- !,
	pmdd_not(F1, F1n),
	pmdd_not(F2, F2n),
	pmdd_and(F1n, F2n, F3n, VarOrder),
	pmdd_not(F3n, F3).

pmdd_not(true, false).
pmdd_not(false, true).
pmdd_not(or([and(in(X,Set),G1)]), F2) :- !, % accelerator
	fd_min(X, Min),
	fd_max(X, Max),
	'$fd_range'(Min, Max, MM, 1),
	'$fd_dom_subtract'(MM, Set, Out),
	pmdd_not(G1, G2),
	pmdd_cons([and(in(X,Out),true),and(in(X,Set),G2)], F2).
pmdd_not(or(Disjuncts1), F2) :-
	(   foreach(and(in(X,Set),_),Disjuncts1),
	    fromto([],U1,U2,Union),
	    param(X)
	do  '$fd_dom_union'(U1, Set, U2)
	),
	fd_min(X, Min),
	fd_max(X, Max),
	'$fd_range'(Min, Max, MM, 1),
	'$fd_dom_subtract'(MM, Union, Out),
	(   foreach(and(InX,G1),Disjuncts1),
	    foreach(and(InX,G2),Disjuncts2)
	do  pmdd_not(G1, G2)
	),
	pmdd_cons([and(in(X,Out),true)|Disjuncts2], F2).

pmdd_cons(Disjuncts1, F) :-
	(   foreach(and(in(X,Set),G1),Disjuncts1),
	    fromto(D2,D3,D4,[]),
	    param(X)
	do  (   G1==false -> D3 = D4
	    ;   Set==[] -> D3 = D4
	    ;   D3 = [G1-Set|D4]
	    )
	),
	(   D2 = [] -> F = false
	;   D2 = [G3-Set3] -> F = or([and(in(X,Set3),G3)])
	;   keysort(D2, D5),
	    keyclumped(D5, D6),
	    (   foreach(G2-Sets,D6),
		foreach(and(in(X,Union),G2),D7),
		param(X)
	    do  fdset_union(Sets, Union)
	    ),
	    sort(D7, D8),
	    F = or(D8)
	).

implicit_booleans(X, _) :- var(X), !,
	X in 0..1.
implicit_booleans(X, _) :- simple(X), !.
implicit_booleans(Expr, M) :-
	arithop2(Expr, Rel, X, Y), !,
	linearize_rel(X, Rel, Y, M, Subs, []),
	(   foreach(Sub,Subs)
	do  (   Sub = reified(M2,Ctr,_)
	    ->  implicit_booleans(Ctr, M2)
	    ;   true
	    )
	).
implicit_booleans(_ in _, _).
implicit_booleans(_ in_set _, _).
implicit_booleans(element(_,_,_), _).
implicit_booleans(table(_,_), _).
implicit_booleans(#\ F2, M) :-
	implicit_booleans(F2, M).
implicit_booleans(F1 #/\ F2, M) :-
	implicit_booleans(F1, M),
	implicit_booleans(F2, M).
implicit_booleans(F1 #\/ F2, M) :-
	implicit_booleans(F1, M),
	implicit_booleans(F2, M).
implicit_booleans(F1 #\ F2, M) :-
	implicit_booleans(F1, M),
	implicit_booleans(F2, M).
implicit_booleans(F1 #=> F2, M) :-
	implicit_booleans(F1, M),
	implicit_booleans(F2, M).
implicit_booleans(F1 #<= F2, M) :-
	implicit_booleans(F1, M),
	implicit_booleans(F2, M).
implicit_booleans(F1 #<=> F2, M) :-
	implicit_booleans(F1, M),
	implicit_booleans(F2, M).

/****************************************************************/
/* bin-packing                                                  */
/****************************************************************/

% bin_packing(Items, Bins) :-
% 	Goal = binpacking(Items, Bins),
% 	must_be(Items, proper_list, Goal, 1),
% 	must_be(Bins, proper_list, Goal, 2),
% 	(   foreach(bin(IDj,_),Bins),
% 	    foreach(IDj,IDs)
% 	do  true
% 	),
% 	list_to_fdset(IDs, IDset),
% 	(   foreach(item(Bi,Si),Items),
% 	    foreach(Bi,Bs),
% 	    foreach(Si,Ss),
% 	    fromto(0,T1,T2,T3),
% 	    param(IDset)
% 	do  Bi in_set IDset,
% 	    T2 is T1+Si
% 	),
% 	(   foreach(bin(IDk,Lk),Bins),
% 	    param(T3,Bs,Ss)
% 	do  (   foreach(B,Bs),
% 		foreach(X,Xs),
% 		param(IDk)
% 	    do  B #= IDk #<=> X
% 	    ),
% 	    Lk in 0..T3,
% 	    scalar_product(Ss, Xs, #=, Lk)
% 	).
bin_packing(Items, Bins) :-
	Goal = bin_packing(Items, Bins),
	must_be(Items, proper_list, Goal, 1),
	must_be(Bins, proper_list, Goal, 2),
	(   foreach(bin(Jj1,Lj1),Bins),
	    foreach(Jj1-Lj1,JLs1),
	    foreach(Jj2-Lj2,JLs2),
	    foreach(Jj2,Js2),
	    foreach(Lj2,Ls2)
	do  true
	),
	keysort(JLs1, JLs2),	% ensure increasing size
	must_be(Js2, proper_list(integer), Goal, 2),
	list_to_fdset(Js2, Jset),
	length(Js2, N),
	fdset_size(Jset, N),	% Js2 all different
	(   foreach(item(Bi,Si),Items),
	    fromto(KL1,KL2,KL3,[]),
	    fromto(0,T1,T2,T3),
	    param(Jset,Goal)
	do  Bi in_set Jset,
	    must_be(Si, integer, Goal, 1),
	    T2 is T1+Si,
	    (   Si=0 -> KL2 = KL3
	    ;   Si>0,
		Ni is -Si,
		KL2 = [Ni-Bi|KL3]
	    )
	),
	keysort(KL1, KL4),
	(   foreach(Nk-Bk,KL4),
	    foreach(Sk,Ss),
	    foreach(Bk,Bs)
	do  Sk is -Nk
	),
	domain(Ls2, 0, T3),
	dvar_list_susp(Bs, BsPairs, dom, Goal, 1, Susp1, Susp2),
	dvar_list_susp(Ls2, LsPairs, minmax, Goal, 2, Susp2, []),
	fd_global_internal(Goal, state(BsPairs,Ss,Js2,LsPairs,_/*handle*/,0/*stamp*/), Susp1, _, clpfd:Goal, 4). % non-idempotent
	
		    
/****************************************************************/
/* miscellaneous shared code					*/
/****************************************************************/

bool_option(X, _) :- var(X), !, fail.
bool_option(false, 0).
bool_option(true, 1).

boolall_option(X, _) :- var(X), !, fail.
boolall_option(false, 0).
boolall_option(true, 1).
boolall_option(all, 3).

aux_option(_, X, _) :- var(X), !, fail.
aux_option(bound, lower, 0) :- !.
aux_option(bound, upper, 1).
aux_option(prune, all, 0) :- !.
aux_option(prune, next, 1).

consistency_option(local, local, val).
consistency_option(value, local, val).
consistency_option(bound, bound, minmax).
consistency_option(bounds, bound, minmax).
consistency_option(global, global, dom).
consistency_option(domain, global, dom).

on(_, X) --> {nonvar(X)}, !.
on(dom, X) --> [dom(X)].
on(min, X) --> [min(X)].
on(max, X) --> [max(X)].
on(minmax, X) --> [minmax(X)].
on(val, X) --> [val(X)].
on(none, X) --> [none(X)].
% the following are deprecated
on(domain, X) --> [dom(X)].
on(range, X) --> [minmax(X)].
on(value, X) --> [val(X)].
on(global, X) --> [dom(X)].
on(bound, X) --> [minmax(X)].
on(bounds, X) --> [minmax(X)].
on(local, X) --> [val(X)].

end_of_file.

/****************************************************************/
/* atleast + linear le                                          */
/****************************************************************/

% atleast_le(B, Vmin..Vmax, Xs, As, C) :-
%   at least B elements of Xs in Vmin..Vmax, and
%   As*Xs =< C
%   only Xs are domain variables

atleast_le(B, Vrange, Xs, As, C) :-
	Goal = atleast_le(B, Vrange, Xs, As, C),
	arg_attribute(B, BA, Goal, 1),
	must_be_dvar_list(Xs, Goal, 3),
	dvar_list_susp(Xs, XVec, none, Goal, 3, Susp, []),
	set_expression_check(Vrange, Vset, Goal, 2),
	fd_global(Goal, state(B-BA,Vset,XVec,As,C,0/*#ground*/,_Handle,0), Susp).

scalar_product_count(As, Xs, C, Vrange, B) :-
	Goal = atleast_le(B, Vrange, Xs, As, C),
	NGoal = atleast_le(B, Vrange, Xs, NAs, NC),
	NC is -C,
	set_expression_check(Vrange, Vset, Goal, 2),
	(   foreach(A,As),
	    foreach(NA,NAs),
	    foreach(X,Xs),
	    foreach(Q,Qs),
	    param(Vset)
	do  NA is -A,
	    X in_set Vset #<=> Q
	),
	sum(Qs, #=, B),
	scalar_product(As, Xs, #=, C), % for dcg reasoning in particular
	arg_attribute(B, BA, Goal, 1),
	must_be_dvar_list(Xs, Goal, 3),
	dvar_list_susp(Xs, XVec, none, Goal, 3, Susp, [min(B)]),
	fd_global(Goal, state(B-BA,Vset,XVec,As,C,0/*#ground*/,_Handle,0), Susp),
	fd_global(NGoal, state(B-BA,Vset,XVec,NAs,NC,0/*#ground*/,_NHandle,0), Susp).

