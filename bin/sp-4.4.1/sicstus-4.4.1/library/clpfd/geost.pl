/* Copyright(C) 1999, Swedish Institute of Computer Science */
/****************************************************************/
/* geost/[2,3,4]                            			*/
/****************************************************************/

geost(Objects1, Sboxes1) :-
	geost(Objects1, Sboxes1, []).

geost(Objects1, Sboxes1, Options) :-
	geost(Objects1, Sboxes1, Options, []).

geost(Objects1, Sboxes1, Options, Rules) :-
	Goal = geost(Objects1,Sboxes1,Options,Rules),
	must_be(Options, proper_list, Goal, 3),
	must_be(Rules  , proper_list(callable), Goal, 4),
	geost_sboxes(Sboxes1, Sboxes1r, Goal, K),
	sort(Sboxes1r, Sboxes2), % ascending SID
	length(Objects1, NO),
	length(Sboxes2, NS),
	(   foreach(Sbox,Sboxes2),
	    foreach(K1-Sbox,Sboxes3)
	do  Sbox = sbox(K1,_,_,_)
	),
	keyclumped(Sboxes3, Sboxes4),
	(   foreach(Sid-Group,Sboxes4),
	    foreach(Sid,Sids),
	    fromto(0,N1,N3,MNS),
	    param(Goal)
	do  geost_noverlap_group(Group, Goal),
	    length(Group, N2),
	    N3 is max(N1,N2)
	),
	list_to_fdset(Sids, SidSet),
	geost_objects(Objects1, Objects1r, Objects2, SidSet, Goal, K, _Susp1, []),
	(   geosr_compile(Objects1r, Sboxes2, Rules, Procs2, Oid2Oix, Goal, K) -> true
	;   illarg(consistency(geost(Objects1,Sboxes1),Rules,'bad rules'), Goal, 4)
	),
	geost_distances(Options, Options2, Oid2Oix, Procs1, Procs2),
	(   Procs2==[false] -> fail
	;   Procs2==[] ->
	    Mask0=0,
	    keysort(Procs1, Procs3),
	    keyclumped(Procs3, Procs)
	;   Mask0=0x10000,
	    Procs = Procs2
	),
	(   for(I,1,K),
	    fromto(Fixorder1,[J|S],S,[])
	do  J is -I
	),
	geost_options(Options2,
		      opt(    0,  Mask0,Fixorder1,_                    ,K,    [],      -1,           _),
		      opt(Flag1,OptMask,Fixorder2,bounding_box(LB2,UB2),K,Chains,Maxbacks,volume(Vol1)),
		      Goal, 3),
	propagate_interval(Flag1, 0, 1),
	arg_attribute(Flag1, Flag2, Goal, 3),
	length(LB2, K),
	length(UB2, K),
	dvar_list_susp(LB2, LB3, min, Goal, 3, _Susp2, []),
	dvar_list_susp(UB2, UB3, max, Goal, 3, _Susp3, []),
	(   OptMask /\ 0x40000 =:= 0 -> true
	;   propagate_interval(Vol1, 0, sup),
	    arg_attribute(Vol1, Vol2, Goal, 3)
	),
	fd_global(Goal, f(K/*#dims*/,
			  NO/*#objects*/,
			  NS/*#sboxes*/,
			  MNS/*max #nsboxes*/,
			  NO/*#targets*/,
			  Objects2,
			  Sboxes2,
			  Flag1-Flag2,
			  LB3-UB3,
			  Vol1-Vol2,
			  OptMask,
			  Fixorder2,
			  Chains,
			  Maxbacks,
			  Procs,
			  0,/*arg(16): #holes*/
			  0,/*arg(17): total volume of non-target objects*/
			  _Handle,0), []).

geost_distances([], [], _) --> [].
geost_distances([dist(Oid1,Oid2,FN)|L1], L2, Map) --> !, [Oix1-dist(Oix2,OpN),Oix2-dist(Oix1,OpN)],
	{avl_fetch(Oid1, Map, Oix1)},
	{avl_fetch(Oid2, Map, Oix2)},
	{geost_encode_dis(FN, OpN)},
	geost_distances(L1, L2, Map).
geost_distances([X|L1], [X|L2], Map) -->
	geost_distances(L1, L2, Map).

geost_encode_dis(#>=(N), N).
geost_encode_dis(#=<(M), N) :- N is -1-M.

geost_options([], Opt, Opt, _, _).
geost_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    on_exception(error(_,_), geost_option(X, Opt0, Opt1), fail) -> true
        ;   illarg(domain(term,geost_option), Goal, ArgNo, X)
        ),
	geost_options(L, Opt1, Opt, Goal, ArgNo).

geost_option(fixall(F,Patterns), opt(_,D,_  ,BB,K,Chains,MB,Vol), opt(F,D,Pat,BB,K,Chains,MB,Vol)) :-
	geost_fixall(Patterns, K, Pat, []),
	Pat \== [].
geost_option(lex(Chain),        opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,[Chain|Chains],MB,Vol)) :-
	must_be(Chain, proper_list(integer), nogoal, 0), % TODO: tighten
	C is (A \/ 1).
geost_option(disjunctive(B),    opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xffffd) \/ (Value<<1).
geost_option(cumulative(B),     opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xfffef) \/ (Value<<4).
geost_option(polymorphism(B), opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xfffdf) \/ (Value<<5).
geost_option(parconflict(B),    opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xfffbf) \/ (Value<<6).
geost_option(visavis_init(B),        opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xfff7f) \/ (Value<<7).
geost_option(visavis(B),        opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xffeff) \/ (Value<<8).
geost_option(corners(B),        opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xffdff) \/ (Value<<9).
geost_option(task_intervals(B), opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xffbff) \/ (Value<<10).
geost_option(dynamic_programming(B), opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xff7ff) \/ (Value<<11).
geost_option(longest_hole(B,MB),   opt(F,A,Pat,BB,K,Chains,_,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	boolall_option(B, Value),
	must_be(MB, integer, nogoal, 0),
	C is (A /\ 0xfcfff) \/ (Value<<12).
geost_option(visavis_floating(B), opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xfbfff) \/ (Value<<14).
geost_option(pallet_loading(B), opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xf7fff) \/ (Value<<15).
geost_option(overlap(B)       , opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
	bool_option(B, Value),
	C is (A /\ 0xdffff) \/ (Value<<17).
% geost_option(spheres(B)       , opt(F,A,Pat,BB,K,Chains,MB,Vol), opt(F,C,Pat,BB,K,Chains,MB,Vol)) :-
% 	bool_option(B, Value),
% 	C is (A /\ 0x7ffff) \/ (Value<<19).
geost_option(volume(Vol1)     , opt(F,A,Pat,BB,K,Chains,MB,_), opt(F,C,Pat,BB,K,Chains,MB,volume(Vol1))) :-
	C is ((A /\ 0xbffff) \/ 0x40000).
geost_option(bounding_box(LB,UB), opt(F,C,Pat,_,K,Chains,MB,Vol), opt(F,C,Pat,bounding_box(LB,UB),K,Chains,MB,Vol)).

geost_fixall([], _) --> [].
geost_fixall([object(Oid,Sid,Xs)|Objects], K) --> !,
	geost_fixall([object(Oid,Sid,Xs,[])|Objects], K).
geost_fixall([object(_,Sid,Xs,_)|Objects], K) -->
	{length(Xs, K)},
	{geost_fixall_pairs([Sid|Xs], 1, L1)},
	{L1 = [1-_|_]},		% Sid must be min(1) or max(1)
	{keysort(L1, L2)},
	(   foreach(J-V,L2),
	    count(J,1,_)
	do  [V]
	),
	geost_fixall(Objects, K).

geost_fixall_pairs([], _, []).
geost_fixall_pairs([min(K)|L1], I, [K-MI|L2]) :- !,
	MI is -I,
	J is I+1,
	geost_fixall_pairs(L1, J, L2).
geost_fixall_pairs([max(K)|L1], I, [K-I|L2]) :-
	J is I+1,
	geost_fixall_pairs(L1, J, L2).

geost_objects([],            [],              [], _, _, _) --> [].
geost_objects([Object1|Os1], [Object1r|Os1r], [object(Oid,Sid1-Sid2,Origin2,MinWit,MaxWit)|Os2], SidSet, Goal, K) -->
	{geost_object(Object1, Object1r, Oid, Sid1, Origin1, More)},
	{must_be(Oid, integer, Goal, 1)},
	{Sid1 in_set SidSet},
	{must_be(More, proper_list(callable), Goal, 1)},
	{arg_attribute(Sid1, Sid2, Goal, 1)},
	{length(Origin1, K)},
	finite_dvar_list_susp(Origin1, Origin2, minmax, Goal, 1),
	{create_mutable([], MinWit)},
	{create_mutable([], MaxWit)},
	geost_objects(Os1, Os1r,Os2, SidSet, Goal, K).

geost_object(object(Oid,Sid,Origin),      object(Oid,Sid,Origin,[]),   Oid, Sid, Origin, []).
geost_object(object(Oid,Sid,Origin,More), object(Oid,Sid,Origin,More), Oid, Sid, Origin, More).

geost_sboxes([], [], _, _).
geost_sboxes([Sbox1|Ss1], [Sbox2|Ss2], Goal, K) :-
	geost_sbox(Sbox1, Sbox2, Sid, Offset, Size, More),
	must_be(Sid, integer, Goal, 2),
	must_be(Offset, list(integer), Goal, 2),
	length(Offset, K),
	must_be(Size, list(integer(>=(0))), Goal, 2),
	length(Size, K),
	must_be(More, proper_list(callable), Goal, 2),
	geost_sboxes(Ss1, Ss2, Goal, K).

geost_sbox(sbox(Sid,Offset,Size),      sbox(Sid,Offset,Size,[]),   Sid, Offset, Size, []).
geost_sbox(sbox(Sid,Offset,Size,More), sbox(Sid,Offset,Size,More), Sid, Offset, Size, More).

geost_noverlap_group([], _).
geost_noverlap_group([S|Ss], Goal) :-
	(   foreach(S1,Ss),
	    param(S,Goal)
	do  (   geost_noverlap(S1, S) -> true
	    ;   illarg(consistency(S,S1,'overlapping sboxes'), Goal, 2)
	    )
	),
	geost_noverlap_group(Ss, Goal).

geost_noverlap(sbox(_,Offset1,Size1,_), sbox(_,Offset2,Size2,_)) :-
	geost_noverlap(Offset1, Size1, Offset2, Size2).

geost_noverlap([T1|Ts1], [S1|Ss1], [T2|Ts2], [S2|Ss2]) :-
	(   T1+S1 =< T2 -> true
	;   T2+S2 =< T1 -> true
	;   geost_noverlap(Ts1, Ss1, Ts2, Ss2)
	).

/*** rule compilation

% Important datatypes:

%%%%%%%%%%%%%%

% Outset (set of (k+1)-dimensional points)

Outset --> list of OBox

%%%%%%%%%%%%%%

% O-box (outbox).

OBox --> list of Integer..Integer

%%%%%%%%%%%%%%

% S-expression (set expression), evaluates to an Outset
% S-expressions are compiled; see below.

SExpr --> {} // empty set 
        | {OExpr} // singleton outbox if nonempty, otherwise empty set
        | ltz(AExpr) // if (AExpr>=0) then empty set, else infinite set
        | lez(AExpr) // if (AExpr>0) then empty set, else infinite set
        | sids(Oid,J,A,=<,AExpr) // {p in Z^{k+1} | O^sid=p[k+1] implies O^pbox(J)^A =< AExpr}
        | sids(Oid,J,A,>=,AExpr) // {p in Z^{k+1} | O^sid=p[k+1] implies O^pbox(J)^A >= AExpr}
	| SExpr/\SExpr // intersection 
	| SExpr\/SExpr // union 

% O-expression (outbox expression), evaluates to an outbox.

OExpr --> list of AExpr..AExpr

%%%%%%%%%%%%%%

% A-expression (arithmetic expression), evaluates to an integer.

AExpr --> inf | sup | int(Integer)
	| xmin(OID,J)   // min(object(I,_,_,_)^x(J))
	| xmax(OID,J)   // max(object(I,_,_,_)^x(J))
	| pmin(OID,J,A) // min(object(I,_,_,_)^pbox(J)^A)
	| pmax(OID,J,A) // max(object(I,_,_,_)^pbox(J)^A)
	| AExpr + AExpr
	| AExpr - AExpr
	| mul(AExpr,Integer)
	| floordiv(AExpr,Integer)
	| ceildiv(AExpr,Integer)

%%%%%%%%%%%%%%

% Instruction set, i.e. compiled representation of S-expression.
% Register-based virtual machine, for common subexpression elimination.
% With respect to a current object O.
% A register holds an integer or an Outset.
% The end result is the Outset used for sweeping O in the various dimensions.

Procedure --> procedure(NReg,NStack,Dest,Code,Dep) // stores an Outset in register Dest
	                                     // using up to NTemp registers
	                                     // Dep are the objects on which Code depends

Bytecode  --> proc(Oix,Dest,NReg,NStack,NCode,Code) // bytecode for procedure: everything 0-based, ref. object index
	
Code --> list of InsnPair

InsnPair -->  Insn-Dest           // executes Insn, stores value in register Dest

Insn --> inf | sup | int(I)       // store the relevant integer
       | xmin(OID,J)              // stores min(object(I,_,_,_)^x(J))
       | xmax(OID,J)              // stores max(object(I,_,_,_)^x(J))
       | pmin(OID,J,A)            // stores min(object(I,_,_,_)^pbox(J)^A)
       | pmax(OID,J,A)            // stores max(object(I,_,_,_)^pbox(J)^A)
       | plus(R1,R2)              // stores R1 + R2
       | minus(R1,R2)             // stores R1 - R2
       | mul(R1,I)                // stores R1 * I
       | floordiv(R1,I)           // stores floor(R1 / I)
       | ceiliv(R1,I)             // stores ceiling(R1 / I)
       | obox(list of RegPair)    // stores an Outset of one OBox if nonempty, otherwise the empty set
       | ltz(R1)                  // if R1>=0, stores the empty set, otherwise stores dom(O.x)
       | lez(R1)                  // if R1>0,  stores the empty set, otherwise stores dom(O.x)
       | sids(_,J,A,Op,R1)        // stores outset corresponding to the values of O.sid
				  //  that satisfy O^pbox(J)^A Op R1
       | intersection(R1,R2,Code) // if R1 is the empty set, stores the empty set
				  //  otherwise, executes Code with value in R2,
				  //  and stores the intersection of R1 and R2
       | union(R1,R2,Code)        // if R1 includes dom(O.x), stores R1,
				  //  otherwise, executes Code with value in R2,
				  //  and stores the union of R1 and R2

RegPair --> Reg1..Reg2            // builds an interval, part of an OBox

*/

geosr_compile(Objects0, Sboxes, Rules, Procs, Oid2Oix, Goal, K) :-
	(   foreach(object(OID,_,_,_),Objects0),
	    count(I,0,_),
	    fromto(Oids1,[OID-I|S],S,[])
	do  true
	),
	keysort(Oids1, Oids2),
	ord_list_to_avl(Oids2, Oid2Oix),	
	sort(Objects0, Objects), % by ascending OID
	rewrite_rules(Rules, Tree0, Objects, Sboxes, Goal),
	rewrite_reified(Tree0, Tree1),
	rewrite_away_not(Tree1, Tree2),
	rewrite_relations(Tree2, Tree3),
	rewrite_away_ops(Tree3, Tree4),
	rewrite_away_min_max(Tree4, Tree5),
	rewrite_linearize(Tree5, Tree6),
	rewrite_simplify(Tree6, Tree7, context(Objects,Sboxes)),
	compile_linearized(Tree7, K, context(Objects,Sboxes), ObjProcs1, []),
	assemble_procs(ObjProcs1, Procs, Oid2Oix).

% [MC] dead
% geosr_objects([], [], _, _, _) --> [].
% geosr_objects([object(Oid,Sid,Origin)|Os1], Os2, Goal, I, K) --> !,
% 	geosr_objects([object(Oid,Sid,Origin,[])|Os1], Os2, Goal, I, K).
% geosr_objects([Object|Os1], [Object|Os2], Goal, I, K) --> [Oid-I],
% 	{Object = object(Oid,_,Origin1,Atts)},
% 	{must_be(Atts, proper_list, Goal, 1)},
% 	{length(Origin1, K)},
% 	{J is I+1},
% 	geosr_objects(Os1, Os2, Goal, J, K).

% geosr_sboxes([], [], _, _, []).
% geosr_sboxes([sbox(Sid,Trans,Size)|Ss1], Ss2, Goal, K, Sids) :- !,
% 	geosr_sboxes([sbox(Sid,Trans,Size,[])|Ss1], Ss2, Goal, K, Sids).
% geosr_sboxes([Sbox|Ss1], [Sbox|Ss2], Goal, K, [Sid|Sids]) :-
% 	Sbox = sbox(Sid,Offset,Size,Atts),
% 	must_be(Sid, integer, Goal, 2),
% 	must_be(Offset, list(integer), Goal, 2),
% 	length(Offset, K),
% 	must_be(Size, list(integer(>(0))), Goal, 2),
% 	must_be(Atts, proper_list, Goal, 2),
% 	length(Size, K),
% 	geosr_sboxes(Ss1, Ss2, Goal, K, Sids).

simple_aexpr(inf, inf, -0x10000000).
simple_aexpr(sup, sup,  0x10000000).
simple_aexpr(int(I), I, I).

% rewrite_rules(Rules, Tree, Objects, Sboxes, Goal)
%   Expand away all Head--->Body, forall, exists, let, card, fold, #=>, #<=>, objects(_), sboxes(_)
%   Fold ground _integer_ expressions.
%   Simplify as far as possible.

rewrite_rules(Forms, Expanded, Objects, Sboxes, Goal) :-
	partition_rules(Forms, Rules, Defs),
	length(Rules, N),
	cardify(N, N, N, Rules, Rule),
	rewrite_expr(Rule, Expanded, Defs, Objects, Sboxes, Goal).

partition_rules([], [], []).
partition_rules([X|Xs], Rules, [X|Defs]) :-
	X = (_--->_), !,
	partition_rules(Xs, Rules, Defs).
partition_rules([X|Xs], [X|Rules], Defs) :-
	partition_rules(Xs, Rules, Defs).

rewrite_expr(X, Y, _, _, _, _) :-
	var(X), !, Y = X.
rewrite_expr(X, Y, _, _, _, _) :-
	integer(X), !, Y = X.
rewrite_expr(true, Y, _, _, _, _) :- !,
	Y = true.
rewrite_expr(false, Y, _, _, _, _) :- !,
	Y = false.
rewrite_expr(X, Y, _, _, _, _) :-
	X = object(_,_,_,_), !, Y = X.
rewrite_expr(X, Y, _, _, _, _) :-
	X = sbox(_,_,_,_), !, Y = X.
rewrite_expr(Main^Sel, Part, _, Objects, Sboxes, _) :-
	get_path(Main, Sel, Part, Objects, Sboxes),
	integer(Part), !.
rewrite_expr(Main^Sel, Part, Defs, Objects, Sboxes, Goal) :-
	on_exception(error(_,_), rewrite_expr(Sel, Sel1, Defs, Objects, Sboxes, Goal), fail),
	Sel\==Sel1, !,
	rewrite_expr(Main^Sel1, Part, Defs, Objects, Sboxes, Goal).
rewrite_expr(Main^Sel, Main^Sel, _, _, _, _) :- !.
rewrite_expr(#\ P, Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(P, P1, Defs, Objects, Sboxes, Goal),
	(   P1==false -> Expanded=true
	;   P1==true -> Expanded=false
	;   Expanded=(#\P1)
	).
rewrite_expr(P #/\ Q, Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(P, P1, Defs, Objects, Sboxes, Goal),
	(   P1==false -> Expanded=false
	;   rewrite_expr(Q, Q1, Defs, Objects, Sboxes, Goal),
	    (   P1==true -> Expanded=Q1
	    ;   Q1==true -> Expanded=P1
	    ;   Q1==false -> Expanded=false
	    ;   Expanded=(P1#/\Q1)
	    )
	).
rewrite_expr(P #\/ Q, Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(P, P1, Defs, Objects, Sboxes, Goal),
	(   P1==true -> Expanded=true
	;   rewrite_expr(Q, Q1, Defs, Objects, Sboxes, Goal),
	    (   P1==false -> Expanded=Q1
	    ;   Q1==true -> Expanded=true
	    ;   Q1==false -> Expanded=P1
	    ;   Expanded=(P1#\/Q1)
	    )
	).
rewrite_expr(P #=> Q, Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(#\ P #\/ Q, Expanded, Defs, Objects, Sboxes, Goal).
rewrite_expr(P #<=> Q, Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(P, P1, Defs, Objects, Sboxes, Goal),
	rewrite_expr(Q, Q1, Defs, Objects, Sboxes, Goal),
	(   P1==true  -> Expanded = Q1
	;   Q1==true  -> Expanded = P1
	;   P1==Q1    -> Expanded = true
	;   P1==false -> Expanded = (#\Q1)
	;   Q1==false -> Expanded = (#\P1)
	;   Expanded = ((P1#/\Q1)#\/((#\P1)#/\(#\Q1)))
	).
rewrite_expr(forall(Quant,List,Expr), Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_each(List, Quant, Expr, ExpandedList, Defs, Objects, Sboxes, Goal),
	length(ExpandedList, N),
	cardify(N, N, N, ExpandedList, Expanded).
rewrite_expr(let(Quant,Element,Expr), Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(exists(Quant,[Element],Expr), Expanded, Defs, Objects, Sboxes, Goal).
rewrite_expr(exists(Quant,List,Expr), Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_each(List, Quant, Expr, ExpandedList, Defs, Objects, Sboxes, Goal),
	length(ExpandedList, N),
	cardify(1, N, N, ExpandedList, Expanded).
rewrite_expr(card(Quant,List,L,U,Expr), Expanded, Defs, Objects, Sboxes, Goal) :- !,
	must_be(L, integer, Goal, 4),
	must_be(U, integer, Goal, 4),
	rewrite_each(List, Quant, Expr, ExpandedList, Defs, Objects, Sboxes, Goal),
	length(ExpandedList, N),
	cardify(L, U, N, ExpandedList, Expanded).
rewrite_expr(fold(Quant,List,Op,Ident,Expr), Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_each(List, Quant, Expr, ExpandedList, Defs, Objects, Sboxes, Goal),
	(   foreach(X,ExpandedList),
	    fromto(Ident,E0,E,Expanded),
	    param(Op)
	do  E =.. [Op,X,E0]
	).
rewrite_expr(min(X,Y), Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(X, X1, Defs, Objects, Sboxes, Goal),
	rewrite_expr(Y, Y1, Defs, Objects, Sboxes, Goal),
	(   integer(X1), integer(Y1) -> Expanded is min(X1,Y1)
	;   Expanded = min(X1,Y1)
	).
rewrite_expr(max(X,Y), Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(X, X1, Defs, Objects, Sboxes, Goal),
	rewrite_expr(Y, Y1, Defs, Objects, Sboxes, Goal),
	(   integer(X1), integer(Y1) -> Expanded is max(X1,Y1)
	;   Expanded = max(X1,Y1)
	).
rewrite_expr(X+Y, Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(X, X1, Defs, Objects, Sboxes, Goal),
	rewrite_expr(Y, Y1, Defs, Objects, Sboxes, Goal),
	(   X1==0 -> Expanded = Y1
	;   Y1==0 -> Expanded = X1
	;   integer(X1), integer(Y1) -> Expanded is X1+Y1
	;   Expanded = (X1+Y1)
	).
rewrite_expr(-Y, Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(0-Y, Expanded, Defs, Objects, Sboxes, Goal).
rewrite_expr(X-Y, Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(X, X1, Defs, Objects, Sboxes, Goal),
	rewrite_expr(Y, Y1, Defs, Objects, Sboxes, Goal),
	(   Y1==0 -> Expanded = X1
	;   integer(X1), integer(Y1) -> Expanded is X1-Y1
	;   Expanded = (X1-Y1)
	).
rewrite_expr(X*Y, Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(X, X1, Defs, Objects, Sboxes, Goal),
	rewrite_expr(Y, Y1, Defs, Objects, Sboxes, Goal),
	(   X1==0 -> Expanded = 0
	;   Y1==0 -> Expanded = 0
	;   X1==1 -> Expanded = Y1
	;   Y1==1 -> Expanded = X1
	;   integer(X1), integer(Y1) -> Expanded is X1*Y1
	;   Expanded = (X1*Y1)
	).
rewrite_expr(X//Y, Expanded, Defs, Objects, Sboxes, Goal) :- !, % SPRM 13803
	rewrite_expr(X/Y, Expanded, Defs, Objects, Sboxes, Goal).
rewrite_expr(X/Y, Expanded, Defs, Objects, Sboxes, Goal) :- !,
	rewrite_expr(X, X1, Defs, Objects, Sboxes, Goal),
	rewrite_expr(Y, Y1, Defs, Objects, Sboxes, Goal),
	(   Y1==0 -> Expanded = (X1/Y1)
	;   X1==0 -> Expanded = 0
	;   Y1==1 -> Expanded = X1
	;   integer(X1), integer(Y1), X1 mod Y1 =:= 0 ->
	    Expanded is X1//Y1
	;   Expanded = (X1/Y1)
	).
rewrite_expr(Expr, Expanded, Defs, Objects, Sboxes, Goal) :-
	arithop(Expr, Op, X, Y), !,
	rewrite_expr(X, X1, Defs, Objects, Sboxes, Goal),
	rewrite_expr(Y, Y1, Defs, Objects, Sboxes, Goal),
	(   integer(X1), integer(Y1) -> evalop(Op, X1, Y1, Expanded)
	;   Expanded =.. [Op,X1,Y1]
	).
rewrite_expr(Expr, Expanded, Defs, Objects, Sboxes, Goal) :-
	functor(Expr, F, A),
	functor(Head, F, A),
	member((Head--->Body), Defs), !,
	copy_term((Head--->Body), (Expr--->Copy)),
	rewrite_expr(Copy, Expanded, Defs, Objects, Sboxes, Goal).
rewrite_expr(Expr, _Expanded, _Defs, _Objects, _Sboxes, Goal) :-
	illarg(existence(macro,Expr,0), Goal, 4).

rewrite_each(objects(Oids), Quant, Expr, ExpandedList, Defs, Objects, Sboxes, Goal) :- !,
	select_objects(Oids, Objects, List),
	rewrite_each1(List, Quant, Expr, ExpandedList, Defs, Objects, Sboxes, Goal).
rewrite_each(sboxes(Sids), Quant, Expr, ExpandedList, Defs, Objects, Sboxes, Goal) :- !,
	select_sboxes(Sids, Sboxes, List, Objects),
	rewrite_each1(List, Quant, Expr, ExpandedList, Defs, Objects, Sboxes, Goal).	
rewrite_each(List, Quant, Expr, ExpandedList, Defs, Objects, Sboxes, Goal) :-
	must_be(List, proper_list, Goal, 4),
	rewrite_each1(List, Quant, Expr, ExpandedList, Defs, Objects, Sboxes, Goal).

rewrite_each1([], _, _, [], _, _, _, _).
rewrite_each1([X|Xs], Q, E, [Y2|Ys], Defs, Objects, Sboxes, Goal) :-
	copy_term(Q-E, X-Y1),
	rewrite_expr(Y1, Y2, Defs, Objects, Sboxes, Goal),
	rewrite_each1(Xs, Q, E, Ys, Defs, Objects, Sboxes, Goal).

% quick & dirty
select_objects([], _, []).
select_objects([Oid|Oids], L1, [X|L3]) :-
	X = object(Oid,_,_,_),
	select(X, L1, L2), !,
	select_objects(Oids, L2, L3).

% quick & dirty
select_sboxes([], _, [], _).
select_sboxes([Expr|Sids], L1, L3, Objects) :-
	Expr = Obj^sid,
	Obj = object(_,Sid,_,_),
	(   integer(Sid) ->
	    select_sboxes(L1, Sid, L2, L3, L4)
	;   memberchk(Obj, Objects),
	    fd_min(Sid, Min),
	    select_pboxes(L1, Min, L2, Obj, 0, L3, L4)
	),
	select_sboxes(Sids, L2, L4, Objects).

select_sboxes([], _, []) --> [].
select_sboxes([X|L1], Y, [X|L2]) -->
	{\+(X=sbox(Y,_,_,_))}, !,
	select_sboxes(L1, Y, L2).
select_sboxes([X|L1], Y, L2) --> [X],
	select_sboxes(L1, Y, L2).

select_pboxes([], _, [], _, _) --> [].
select_pboxes([X|L1], Y, [X|L2], Obj, I) -->
	{\+(X=sbox(Y,_,_,_))}, !,
	select_pboxes(L1, Y, L2, Obj, I).
select_pboxes([_|L1], Y, L2, Obj, I) --> [Obj^pbox(J)],
	{J is I+1},
	select_pboxes(L1, Y, L2, Obj, J).

select_jth_sbox_atts([], _, _, _, []).
select_jth_sbox_atts([Sid|Sids], J, A, Sboxes, [V|Values]) :-
	select_jth_sbox(Sid, J, Sbox, Sboxes),
	get_path(Sbox, A, V, [], []),
	integer(V),
	select_jth_sbox_atts(Sids, J, A, Sboxes, Values).

select_jth_sbox(Sid, 1, Sbox, [Sbox0|_]) :-
	Sbox0 = sbox(Sid,_,_,_), !,
	Sbox = Sbox0.
select_jth_sbox(Sid, J, Sbox, [Sbox0|S]) :-
	Sbox0 = sbox(Sid,_,_,_), !,
	I is J-1,
	select_jth_sbox(Sid, I, Sbox, S).
select_jth_sbox(Sid, J, Sbox, [_|S]) :-
	select_jth_sbox(Sid, J, Sbox, S).

conjuncts(true) --> !.
conjuncts(P #/\ Q) --> !,
	conjuncts(P),
	conjuncts(Q).
conjuncts(P) --> [P].

% compiler.pl
% andify([], true).
% andify([P|Ps], Conj) :-
% 	andify(Ps, P, Conj).

% andify([], P, P).
% andify([P|Ps], Q, Conj) :-
% 	andify(Ps, Q#/\P, Conj).

% orify([], false).
% orify([P|Ps], Disj) :-
% 	orify(Ps, P, Disj).

% orify([], P, P).
% orify([P|Ps], Q, Disj) :-
% 	orify(Ps, Q#\/P, Disj).

cardify(L1, U1, N, List, Expanded) :-
	cardify_filter(List, List1, L1, L2, U1, U2, N, N2),
	L3 is max(L2,0),
	U3 is min(U2,N2),
	cardify2(L3, U3, N2, List1, Expanded).

cardify_filter([], [], L, L, U, U, N, N).
cardify_filter([true|List], List1, L, L2, U, U2, N, N2) :- !,
	L1 is L-1,
	U1 is U-1,
	N1 is N-1,
	cardify_filter(List, List1, L1, L2, U1, U2, N1, N2).
cardify_filter([false|List], List1, L, L2, U, U2, N, N2) :- !,
	U1 is U-1,
	N1 is N-1,
	cardify_filter(List, List1, L, L2, U1, U2, N1, N2).
cardify_filter([X|List], [X|List1], L, L2, U, U2, N, N2) :-
	cardify_filter(List, List1, L, L2, U, U2, N, N2).

cardify2(L, U, _N, _Ps, false) :- L>U, !.
cardify2(0, N, N, _Ps, true) :- !.
cardify2(1, N, N, Ps, Exp) :- !,
	orify(Ps, Exp).
cardify2(N, N, N, Ps, Exp) :- !,
	andify(Ps, Exp).
cardify2(0, 0, _N, Ps, #\ Exp) :- !,
	orify(Ps, Exp).
cardify2(0, N1, N, Ps, #\ Exp) :- N1=:=N-1, !,
	andify(Ps, Exp).
cardify2(L, U, N, [P|Ps], (P #/\ Exp1 #\/ #\P #/\ Exp2)) :-
	L1 is L-1,
	U1 is U-1,
	N1 is N-1,
	cardify2(L1, U1, N1, Ps, Exp1),
	cardify2(L , U , N1, Ps, Exp2).

% compiler.pl
% arithop(A #= B, #=, A, B).
% arithop(A #\= B, #\=, A, B).
% arithop(A #< B, #<, A, B).
% arithop(A #=< B, #=<, A, B).
% arithop(A #> B, #>, A, B).
% arithop(A #>= B, #>=, A, B).

% evalop(#=, A, B, true)  :- A =:= B, !.
% evalop(#\=, A, B, true) :- A =\= B, !.
% evalop(#<, A, B, true)  :- A < B, !.
% evalop(#=<, A, B, true) :- A =< B, !.
% evalop(#>, A, B, true)  :- A > B, !.
% evalop(#>=, A, B, true) :- A >= B, !.
% evalop(_, _, _, false).

get_path(object(OID,_,_,_), oid, OID, _Objects, _Sboxes) :- !.
get_path(object(_,SID,_,_), sid, SID, _Objects, _Sboxes) :- !.
get_path(object(_,_,X,_),   x(I),V,   _Objects, _Sboxes) :- !,
	nth1(I, X, V).
get_path(object(_,_,_,Atts),Key, V,   _Objects, _Sboxes) :- !,
	memberchk(Key-V, Atts).
get_path(Obj^pbox(J),      Attr, Val, Objects, Sboxes) :- !,
	get_pbox_path(Obj, J, Attr, Val, Objects, Sboxes).
get_path(sbox(SID,_,_,_),  sid, SID, _Objects, _Sboxes) :- !.
get_path(sbox(_,T,_,_),    t(I),V,   _Objects, _Sboxes) :- !,
	nth1(I, T, V).
get_path(sbox(_,_,L,_),    l(I),V,   _Objects, _Sboxes) :- !,
	nth1(I, L, V).
get_path(sbox(_,_,_,Atts), Key, V,   _Objects, _Sboxes) :- !,
	memberchk(Key-V, Atts).

get_pbox_path(Obj, J, Attr, Val, Objects, Sboxes) :-
	memberchk(Obj, Objects),
	Obj = object(_,Sid,_,_),
	fd_set(Sid, Set),
	fdset_to_list(Set, L),
	select_jth_sbox_atts(L, J, Attr, Sboxes, Values),
	same_values(Values, Val).

same_values([], _).
same_values([X|Xs], X) :-
	same_values(Xs, X).
	
% rewrite_reified(Tree1, Tree2)
%   Eliminate reified exprs
rewrite_reified(true, true) :- !.
rewrite_reified(false, false) :- !.
rewrite_reified(#\ X, #\ Y) :- !,
	rewrite_reified(X, Y).
rewrite_reified(X1 #/\ X2, Y1 #/\ Y2) :- !,
	rewrite_reified(X1, Y1),
	rewrite_reified(X2, Y2).
rewrite_reified(X1 #\/ X2, Y1 #\/ Y2) :- !,
	rewrite_reified(X1, Y1),
	rewrite_reified(X2, Y2).
rewrite_reified(Expr, ExprR) :-
	functor(Expr, Op, 2),
	arg(1, Expr, Left),
	arg(2, Expr, Right),
	rewrite_reified(Op, Left, Right, ExprM), !,
	rewrite_reified(ExprM, ExprR).
rewrite_reified(Expr, Expr).

rewrite_reified(Op, Left, Right, ExprM) :-
	replace_reified(Left, Fol, LeftX, X), !,
	copy_term(X-LeftX, 0-LeftF),
	X = 1,
	functor(CmpT, Op, 2),
	arg(1, CmpT, LeftX),
	arg(2, CmpT, Right),
	functor(CmpF, Op, 2),
	arg(1, CmpF, LeftF),
	arg(2, CmpF, Right),
	ExprM = ((Fol #/\ CmpT) #\/ (#\ Fol #/\ CmpF)).
rewrite_reified(Op, Left, Right, ExprM) :-
	replace_reified(Right, Fol, RightX, X), !,
	copy_term(X-RightX, 0-RightF),
	X = 1,
	functor(CmpT, Op, 2),
	arg(1, CmpT, Left),
	arg(2, CmpT, RightX),
	functor(CmpF, Op, 2),
	arg(1, CmpF, Left),
	arg(2, CmpF, RightF),
	ExprM = ((Fol #/\ CmpT) #\/ (#\ Fol #/\ CmpF)).

replace_reified(_^_, _, _, _) :- !, fail.
replace_reified(Expr, _, _, _) :-
	simple(Expr), !, fail.
replace_reified(min(X1,Y1), Fol, min(X2,Y2), Var) :- !,
	(   replace_reified(X1, Fol, X2, Var) -> Y2 = Y1
	;   replace_reified(Y1, Fol, Y2, Var) -> X2 = X1
	).
replace_reified(max(X1,Y1), Fol, max(X2,Y2), Var) :- !,
	(   replace_reified(X1, Fol, X2, Var) -> Y2 = Y1
	;   replace_reified(Y1, Fol, Y2, Var) -> X2 = X1
	).
replace_reified(+(X1,Y1), Fol, +(X2,Y2), Var) :- !,
	(   replace_reified(X1, Fol, X2, Var) -> Y2 = Y1
	;   replace_reified(Y1, Fol, Y2, Var) -> X2 = X1
	).
replace_reified(-(X1,Y1), Fol, -(X2,Y2), Var) :- !,
	(   replace_reified(X1, Fol, X2, Var) -> Y2 = Y1
	;   replace_reified(Y1, Fol, Y2, Var) -> X2 = X1
	).
replace_reified(*(X1,Y1), Fol, *(X2,Y2), Var) :- !,
	(   replace_reified(X1, Fol, X2, Var) -> Y2 = Y1
	;   replace_reified(Y1, Fol, Y2, Var) -> X2 = X1
	).
replace_reified(//(X1,Y1), Fol, R, Var) :- !, % SPRM 13803
	replace_reified(/(X1,Y1), Fol, R, Var).
replace_reified(/(X1,Y1), Fol, /(X2,Y2), Var) :- !,
	(   replace_reified(X1, Fol, X2, Var) -> Y2 = Y1
	;   replace_reified(Y1, Fol, Y2, Var) -> X2 = X1
	).
replace_reified(Fol, Fol, Var, Var).

% rewrite_away_not(Tree1, Tree2)
%   Eliminate #\
rewrite_away_not(true, true) :- !.
rewrite_away_not(false, false) :- !.
rewrite_away_not(#\ X, Y) :- !,
	rewrite_away_not_neg(X, Y).
rewrite_away_not(X1 #/\ X2, Y1 #/\ Y2) :- !,
	rewrite_away_not(X1, Y1),
	rewrite_away_not(X2, Y2).
rewrite_away_not(X1 #\/ X2, Y1 #\/ Y2) :- !,
	rewrite_away_not(X1, Y1),
	rewrite_away_not(X2, Y2).
rewrite_away_not(R, R).

rewrite_away_not_neg(true, false) :- !.
rewrite_away_not_neg(false, true) :- !.
rewrite_away_not_neg(#\ X, Y) :- !,
	rewrite_away_not(X, Y).
rewrite_away_not_neg(X1 #/\ X2, Y1 #\/ Y2) :- !,
	rewrite_away_not_neg(X1, Y1),
	rewrite_away_not_neg(X2, Y2).
rewrite_away_not_neg(X1 #\/ X2, Y1 #/\ Y2) :- !,
	rewrite_away_not_neg(X1, Y1),
	rewrite_away_not_neg(X2, Y2).
rewrite_away_not_neg(A #= B, A #\= B).
rewrite_away_not_neg(A #\= B, A #= B).
rewrite_away_not_neg(A #< B, A #>= B).
rewrite_away_not_neg(A #=< B, A #> B).
rewrite_away_not_neg(A #> B, A #=< B).
rewrite_away_not_neg(A #>= B, A #< B).

% rewrite_relations(Tree1, Tree2)
%   Normalize all expressions into one of [Expr #>= 0, Expr #> 0]
rewrite_relations(true, true).
rewrite_relations(false, false).
rewrite_relations(X1 #/\ X2, Y1 #/\ Y2) :-
	rewrite_relations(X1, Y1),
	rewrite_relations(X2, Y2).
rewrite_relations(X1 #\/ X2, Y1 #\/ Y2) :-
	rewrite_relations(X1, Y1),
	rewrite_relations(X2, Y2).
rewrite_relations(X #= Y,  X-Y #>= 0 #/\ Y-X #>= 0).
rewrite_relations(X #\= Y, X-Y #>  0 #\/ Y-X #>  0).
rewrite_relations(X #< Y,  Y-X #>  0).
rewrite_relations(X #> Y,  X-Y #>  0).
rewrite_relations(X #=< Y, Y-X #>= 0).
rewrite_relations(X #>= Y, X-Y #>= 0).

% rewrite_away_ops(Tree1, Tree2)
%   Eliminate -, *, /
rewrite_away_ops(true, true) :- !.
rewrite_away_ops(false, false) :- !.
rewrite_away_ops(X1 #/\ X2, Y1 #/\ Y2) :- !,
	rewrite_away_ops(X1, Y1),
	rewrite_away_ops(X2, Y2).
rewrite_away_ops(X1 #\/ X2, Y1 #\/ Y2) :- !,
	rewrite_away_ops(X1, Y1),
	rewrite_away_ops(X2, Y2).
rewrite_away_ops(X #> 0,  Y #>  0) :- !,
	rewrite_away_ops(X, Y, rat(1,1)).
rewrite_away_ops(X #>= 0, Y #>= 0) :- !,
	rewrite_away_ops(X, Y, rat(1,1)).

rewrite_away_ops(min(X1,X2), Expr, M) :- !,
	rewrite_away_ops(X1, Y1, M),
	rewrite_away_ops(X2, Y2, M),
	(   rat_gez(M) -> Expr = min(Y1,Y2)
	;   Expr = max(Y1,Y2)
	).
rewrite_away_ops(max(X1,X2), Expr, M) :- !,
	rewrite_away_ops(X1, Y1, M),
	rewrite_away_ops(X2, Y2, M),
	(   rat_gez(M) -> Expr = max(Y1,Y2)
	;   Expr = min(Y1,Y2)
	).
rewrite_away_ops(X1 + X2, Y1 + Y2, M) :- !,
	rewrite_away_ops(X1, Y1, M),
	rewrite_away_ops(X2, Y2, M).
rewrite_away_ops(X1 - X2, Y1 + Y2, M) :- !,
	rewrite_away_ops(X1, Y1, M),
	rat_multiply(M, -1, M1),
	rewrite_away_ops(X2, Y2, M1).
rewrite_away_ops(X1 * X2, Y2, M) :-
	eval_ground(X1, V), !,
	rat_mul(M, V, M1),
	rewrite_away_ops(X2, Y2, M1).
rewrite_away_ops(X1 * X2, Y1, M) :- !,
	eval_ground(X2, V),
	rat_mul(M, V, M1),
	rewrite_away_ops(X1, Y1, M1).
rewrite_away_ops(X1 // X2, Y1, M) :- !, % SPRM 13803
	rewrite_away_ops(X1 / X2, Y1, M).
rewrite_away_ops(X1 / X2, Y1, M) :- !,
	eval_ground(X2, V),
	rat_div(M, V, M1),
	rewrite_away_ops(X1, Y1, M1).
rewrite_away_ops(X, scaled(R,1), M) :-
	integer(X), !,
	rat_multiply(M, X, R).
rewrite_away_ops(X, scaled(M,X), M).

%   eval_ground(+Expr, -Rat)
eval_ground(X+Y, R) :- !,
	eval_ground(X, R1),
	eval_ground(Y, R2),
	rat_add(R1, R2, R).
eval_ground(X-Y, R) :- !,
	eval_ground(X, R1),
	eval_ground(Y, R2),
	rat_sub(R1, R2, R).
eval_ground(X*Y, R) :- !,
	eval_ground(X, R1),
	eval_ground(Y, R2),
	rat_mul(R1, R2, R).
eval_ground(X//Y, R) :- !,	% SPRM 13803
	eval_ground(X/Y, R).
eval_ground(X/Y, R) :- !,
	eval_ground(X, R1),
	eval_ground(Y, R2),
	rat_div(R1, R2, R).
eval_ground(min(X,Y), R) :- !,
	eval_ground(X, R1),
	eval_ground(Y, R2),
	rat_min_max(R1, R2, R, _).
eval_ground(max(X,Y), R) :- !,
	eval_ground(X, R1),
	eval_ground(Y, R2),
	rat_min_max(R1, R2, _, R).
eval_ground(X, rat(X,1)) :-
	integer(X).

% rewrite_away_min_max(Tree1, Tree2)
%   Eliminate min, max
rewrite_away_min_max(true, true).
rewrite_away_min_max(false, false).
rewrite_away_min_max(X1 #/\ X2, Y1 #/\ Y2) :-
	rewrite_away_min_max(X1, Y1),
	rewrite_away_min_max(X2, Y2).
rewrite_away_min_max(X1 #\/ X2, Y1 #\/ Y2) :-
	rewrite_away_min_max(X1, Y1),
	rewrite_away_min_max(X2, Y2).
rewrite_away_min_max(X #> 0,  Comb) :-
	rewrite_distribute_plus(X, Y),
	rewrite_min_max(Y, Comb, #>).
rewrite_away_min_max(X #>= 0, Comb) :-
	rewrite_distribute_plus(X, Y),
	rewrite_min_max(Y, Comb, #>=).

rewrite_min_max(min(X,Y), P1 #/\ P2, Rel) :- !,
	rewrite_min_max(X, P1, Rel),
	rewrite_min_max(Y, P2, Rel).
rewrite_min_max(max(X,Y), P1 #\/ P2, Rel) :- !,
	rewrite_min_max(X, P1, Rel),
	rewrite_min_max(Y, P2, Rel).
rewrite_min_max(X, P, Rel) :-
	P =.. [Rel,X,0].

rewrite_distribute_plus(min(X1,X2), min(Y1,Y2)) :-
	rewrite_distribute_plus(X1, Y1),
	rewrite_distribute_plus(X2, Y2).
rewrite_distribute_plus(max(X1,X2), max(Y1,Y2)) :-
	rewrite_distribute_plus(X1, Y1),
	rewrite_distribute_plus(X2, Y2).
rewrite_distribute_plus(X1+X2, Y) :-
	rewrite_distribute_plus(X1, Y1),
	rewrite_distribute_plus(X2, Y2),
	rewrite_distribute_plus(Y1, Y2, Y).
rewrite_distribute_plus(scaled(R,C), scaled(R,C)).

rewrite_distribute_plus(min(X1,X2), Y, min(Z1,Z2)) :- !,
	rewrite_distribute_plus(X1+Y, Z1),
	rewrite_distribute_plus(X2+Y, Z2).
rewrite_distribute_plus(Y, min(X1,X2), min(Z1,Z2)) :- !,
	rewrite_distribute_plus(Y+X1, Z1),
	rewrite_distribute_plus(Y+X2, Z2).
rewrite_distribute_plus(max(X1,X2), Y, max(Z1,Z2)) :- !,
	rewrite_distribute_plus(X1+Y, Z1),
	rewrite_distribute_plus(X2+Y, Z2).
rewrite_distribute_plus(Y, max(X1,X2), max(Z1,Z2)) :- !,
	rewrite_distribute_plus(Y+X1, Z1),
	rewrite_distribute_plus(Y+X2, Z2).
rewrite_distribute_plus(X, Y, X+Y).

% rewrite_linearize(Tree1, Tree2)
%   Normalize all expressions into one of [SP#>=C,true,false] where
%   SP is a list of Var-Coeff pairs, where Coeffs and C are integers.
rewrite_linearize(true, true).
rewrite_linearize(false, false).
rewrite_linearize(X1 #/\ X2, Y1 #/\ Y2) :-
	rewrite_linearize(X1, Y1),
	rewrite_linearize(X2, Y2).
rewrite_linearize(X1 #\/ X2, Y1 #\/ Y2) :-
	rewrite_linearize(X1, Y1),
	rewrite_linearize(X2, Y2).
rewrite_linearize(Expr #>= 0, Rel) :-
	rewrite_linearize(Expr, 0, Rel).
rewrite_linearize(Expr #> 0, Rel) :-
	rewrite_linearize(Expr, 1, Rel).

rewrite_linearize(Expr, Inc, Rel) :-
	linearize(Expr, Vector0, []),
	keysort(Vector0, Vector1),
	key_add_rat_values(Vector1, Vector2),
	(   foreach(Var-Rat,Vector2),
	    foreach(Var,Vars),
	    foreach(Rat,Rats),
	    fromto(1,LCM0,LCM1,LCM)
	do  Rat = rat(_,Div),
	    LCM1 is Div*LCM0//gcd(Div,LCM0)
	),
	(   foreach(rat(A,B),Rats),
	    foreach(V-Int,Vector3),
	    foreach(V,Vars),
	    param(LCM)
	do  Int is A*LCM//B
	),
	(   Vector3=[1-C|Vector4] -> RHS is Inc-C
	;   Vector4 = Vector3,
	    RHS = Inc
	),
	(   Vector4==[] -> evalop(#>=, 0, RHS, Rel)
	;   Rel = (Vector4 #>= RHS)
	).

% compiler.pl
% linearize(X+Y) -->
% 	linearize(X),
% 	linearize(Y).
% linearize(scaled(R,X)) --> [X-R].

% key_add_rat_values([], []).
% key_add_rat_values([K1-C1,K2-C2|V1], V3) :-
% 	K1==K2, !,
% 	rat_add(C1, C2, C3),
% 	(C3==rat(0,1) -> V2=V1 ; V2=[K1-C3|V1]),
% 	key_add_rat_values(V2, V3).
% key_add_rat_values([KC|V1], [KC|V2]) :-
% 	key_add_rat_values(V1, V2).

% rat_gez(rat(A,_)) :- A>=0.

% rat_normalize(rat(A,B), rat(C,D)) :-
% 	G is gcd(A,B)*sign(B),
% 	C is A//G,
% 	D is B//G.

% rat_multiply(rat(A,B), M, R) :-
% 	C is M*A,
% 	rat_normalize(rat(C,B), R).

% rat_add(rat(A,B), rat(C,D), R) :-
% 	E is A*D+B*C,
% 	F is B*D,
% 	rat_normalize(rat(E,F), R).

% rat_sub(rat(A,B), rat(C,D), R) :-
% 	E is A*D-B*C,
% 	F is B*D,
% 	rat_normalize(rat(E,F), R).

% rat_mul(rat(A,B), rat(C,D), R) :-
% 	E is A*C,
% 	F is B*D,
% 	rat_normalize(rat(E,F), R).

% rat_div(rat(A,B), rat(C,D), R) :-
% 	E is A*D,
% 	F is B*C,
% 	rat_normalize(rat(E,F), R).

% rat_min_max(rat(A,B), rat(C,D), Min, Max) :-
% 	(   A*D =< B*C ->
% 	    Min=rat(A,B), Max=rat(C,D)
% 	;   Max=rat(A,B), Min=rat(C,D)
% 	).

% rewrite_simplify(Tree1, Tree2, Context)
%   Simplify away all occurrences of 'true' and 'false' except at the root.
rewrite_simplify(true, true, _).
rewrite_simplify(false, false, _).
rewrite_simplify(P #/\ Q, Rel, Context) :-
	rewrite_simplify(P, P1, Context),
	(   P1==false -> Rel = false
	;   rewrite_simplify(Q, Q1, Context),
	    (   P1==true -> Rel = Q1
	    ;   Q1==true -> Rel = P1
	    ;   Q1==false -> Rel = false
	    ;   Rel = (P1#/\Q1)
	    )
	).
rewrite_simplify(P #\/ Q, Rel, Context) :-
	rewrite_simplify(P, P1, Context),
	(   P1==true -> Rel = true
	;   rewrite_simplify(Q, Q1, Context),
	    (   P1==false -> Rel = Q1
	    ;   Q1==true -> Rel = true
	    ;   Q1==false -> Rel = P1
	    ;   Rel = (P1#\/Q1)
	    )
	).
rewrite_simplify(L#>=R, Rel, Context) :-
	linear_min_max(L, 0, Min, 0, Max, Context),
	(   Min>=R -> Rel = true
	;   Max<R  -> Rel = false
	;   Rel = (L#>=R)
	).

linear_min_max([], Min, Min, Max, Max, _).
linear_min_max([Ref-C|Linear], Min0, Min, Max0, Max, Context) :-
	ref_min_max(Ref, Rmin, Rmax, Context),
	(   C>0 ->
	    Min1 is Min0+C*Rmin, Max1 is Max0+C*Rmax
	;   Min1 is Min0+C*Rmax, Max1 is Max0+C*Rmin
	),
	linear_min_max(Linear, Min1, Min, Max1, Max, Context).

ref_min_max(object(ID,_,_,_)^x(J), Min, Max, Context) :- !,
	Context = context(Objects,_),
	memberchk(object(ID,_,Xs,_), Objects),
	nth1(J, Xs, X),
	fd_min(X, Min),
	fd_max(X, Max).
ref_min_max((object(ID,_,_,_)^pbox(J))^A, Min, Max, Context) :- !,
	Context = context(Objects,Sboxes),
	memberchk(object(ID,SID,_,_), Objects),
	fd_set(SID, S),
	fdset_to_list(S, L),
	select_jth_sbox_atts(L, J, A, Sboxes, Values),
	min_member(Min, Values),
	max_member(Max, Values).

compile_linearized(false, _, _) --> !,
	[false].
compile_linearized(Tree, _, Context) -->
	{conjuncts(Tree, Parts1, [])},
	{   foreach(Part,Parts1),
	    foreach(Objs-Part,Parts2)
	do  objects_of_tree(Part, IDs, []),
	    sort(IDs, Objs)
	},
	{keysort(Parts2, Parts3)},
	{keyclumped(Parts3, Parts4)},
	(   foreach(Objs2-List,Parts4),
	    param(Context)
	do  {andify(List, Tree1)},
	    {commute_tree(Tree1, Tree2, _)},
	    (   foreach(ID,Objs2),
		param(Tree2,Context)
	    do  {forbidden_set_for_object(Tree2, ID, RHS)},
		compile_forbidden_for_object(RHS, o(ID), Context)
	    )
	).

objects_of_tree(R1 #/\ R2) -->
	objects_of_tree(R1),
	objects_of_tree(R2).
objects_of_tree(R1 #\/ R2) -->
	objects_of_tree(R1),
	objects_of_tree(R2).
objects_of_tree(L #>= _) -->
	objects_of_vector(L).

objects_of_vector([]) --> [].
objects_of_vector([object(ID,_,_,_)^_-_|L]) --> !, [ID],
	objects_of_vector(L).	
objects_of_vector([(object(ID,_,_,_)^pbox(_))^_-_|L]) --> !, [ID],
	objects_of_vector(L).	

commute_tree(P #\/ Q, Tree, Depth) :- !,
	commute_tree(P, P1, D1),
	commute_tree(Q, Q1, D2),
	Depth is D1+D2+1,
	(D1=<D2 -> Tree = (P1#\/Q1) ; Tree = (Q1#\/P1)).
commute_tree(P #/\ Q, Tree, Depth) :- !,
	commute_tree(P, P1, D1),
	commute_tree(Q, Q1, D2),
	Depth is D1+D2+1,
	(D1=<D2 -> Tree = (P1#/\Q1) ; Tree = (Q1#/\P1)).
commute_tree(Tree, Tree, 0).

forbidden_set_for_object(P #\/ Q, ID, SExpr) :-
	forbidden_set_for_object(P, ID, SExpr1),
	forbidden_set_for_object(Q, ID, SExpr2),
	sexpr_intersection(SExpr1, SExpr2, SExpr).
forbidden_set_for_object(P #/\ Q, ID, SExpr) :-
	forbidden_set_for_object(P, ID, SExpr1),
	forbidden_set_for_object(Q, ID, SExpr2),
	sexpr_union(SExpr1, SExpr2, SExpr).
forbidden_set_for_object(L#>=R, ID, SExpr) :-
	findall(SExpr1, build_an_ix(ID,L,R,SExpr1), SExprs),
	SExprs\==[], !,
	sexprs_union(SExprs, SExpr).
forbidden_set_for_object(L#>=R, _, ltz(Sum)) :-
	R1 is -R,
	build_ix_sum(L, 1, max, int(R1), Sum).

build_an_ix(ID, L, R, SExpr) :- 
	select(Ref-C, L, L1),
	build_ix(Ref, ID, C, L1, R, SExpr).

build_ix(Ref, Oid, C, L1, R, {OExpr}) :- 
	Ref = object(Oid,_,Orig,_)^x(J),
	C>0, !,
	R1 is R-C,
	build_ix_sum(L1, -1, min, int(R1), Sum),
	(   C =:= 1 -> Quot = Sum
	;   Quot = ceildiv(Sum,C)
	),
	length(Orig, K),
	build_oexpr(0, J, K, inf..Quot, OExpr).
% DISABLED and fishy for sweeping in context of fixed shape
% build_ix(Ref, Oid, C, L1, R, SExpr) :-
% 	Ref = (object(Oid,_,_,_)^pbox(J))^A,
% 	C>0, !,
% 	R1 is R-C,
% 	build_ix_sum(L1, -1, min, int(R1), Sum),
% 	(   C =:= 1 -> Quot = Sum
% 	;   Quot = ceildiv(Sum,C)
% 	),
% 	SExpr = sids(Oid,J,A,=<,Quot).
build_ix(Ref, Oid, C, L1, R, {OExpr}) :-
	Ref = object(Oid,_,Orig,_)^x(J), !,
	R1 is -R-C,
	build_ix_sum(L1, 1, max, int(R1), Sum),
	(   C =:= -1 -> Quot = Sum
	;   D is -C, Quot = floordiv(Sum,D)
	),
	length(Orig, K),
	build_oexpr(0, J, K, Quot..sup, OExpr).
% DISABLED and fishy for sweeping in context of fixed shape
% build_ix(Ref, Oid, C, L1, R, SExpr) :-
% 	Ref = (object(Oid,_,_,_)^pbox(J))^A,
% 	R1 is -R-C,
% 	build_ix_sum(L1, 1, max, int(R1), Sum),
% 	(   C =:= -1 -> Quot = Sum
% 	;   D is -C, Quot = floordiv(Sum,D)
% 	),
% 	SExpr = sids(Oid,J,A,>=,Quot).

build_ix_sum([], _, _, Sum, Sum).
build_ix_sum([Ref-Con|V], Sign, Tag, Sum0, Sum) :-
	SCon is Sign*Con,
	SCon>0, !,
	build_term(Ref, Tag, Term),
	(   SCon =:= 1 -> Sum1 = Sum0+Term
	;   Sum1 = Sum0+mul(Term,SCon)
	),
	build_ix_sum(V, Sign, Tag, Sum1, Sum).
build_ix_sum([Ref-Con|V], Sign, Tag, Sum0, Sum) :-
	ACon is -Sign*Con,
	flip_tag(Tag, Fag),
	build_term(Ref, Fag, Term),
	(   ACon =:= 1 -> Sum1 = Sum0-Term
	;   Sum1 = Sum0-mul(Term,ACon)
	),
	build_ix_sum(V, Sign, Tag, Sum1, Sum).

build_term(object(ID,_,_,_)^x(J),        min, xmin(ID,J)) :- !.
build_term(object(ID,_,_,_)^x(J),        max, xmax(ID,J)) :- !.
build_term((object(ID,_,_,_)^pbox(N))^A, min, pmin(ID,N,A)) :- !.
build_term((object(ID,_,_,_)^pbox(N))^A, max, pmax(ID,N,A)) :- !.
build_term(X, _, X).

flip_tag(min, max).
flip_tag(max, min).

build_oexpr(I, _, K, _, []) :-
	I > K, !.
build_oexpr(I, Key, K, Range, [Range|OExpr]) :-
	J is I+1,
	J=:=Key, !,
	build_oexpr(J, Key, K, Range, OExpr).
build_oexpr(I, Key, K, Range, [inf..sup|OExpr]) :- !,
	J is I+1,
	build_oexpr(J, Key, K, Range, OExpr).

%% union and intersection

sexprs_union([SE], SE) :- !.
sexprs_union([SE0|SEs], SE) :-
	sexprs_union(SEs, SE0, SE).

sexprs_union([], SE, SE).
sexprs_union([SE1|SEs], SE0, SE) :-
	sexpr_union(SE0, SE1, SE2),
	sexprs_union(SEs, SE2, SE).

sexpr_union(SE1, SE2, SE1) :-
	sexpr_subset(SE2, SE1), !.
sexpr_union(SE1, SE2, SE2) :-
	sexpr_subset(SE1, SE2), !.
sexpr_union({OE1}, {OE2}, {OE3}) :-
	oexpr_union(OE1, OE2, OE3, 0), !.
sexpr_union(SE1, SE2, SE1\/SE2).

% true if at most one pair of ranges is not the same,
% and that one must overlap
oexpr_union([], [], [], _).
oexpr_union([R|OE1], [R|OE2], [R|OE3], K) :- !,
	oexpr_union(OE1, OE2, OE3, K).
oexpr_union([A1..B1|OE1], [A2..B2|OE2], [A3..B3|OE3], 0) :-
	intervals_overlap(A1, B1, A2, B2),
	bound_min(A1, A2, A3),
	bound_max(B1, B2, B3),
	oexpr_union(OE1, OE2, OE3, 1).

intervals_overlap(A1, B1, A2, B2) :-
	simple_aexpr(A1, _, A1v),
	simple_aexpr(A2, _, A2v),
	simple_aexpr(B1, _, B1v),
	simple_aexpr(B2, _, B2v),
	A1v=<B2v,
	A2v=<B1v.

sexpr_intersection(SE1, SE2, SE1) :-
	sexpr_subset(SE1, SE2), !.
sexpr_intersection(SE1, SE2, SE2) :-
	sexpr_subset(SE2, SE1), !.
sexpr_intersection({OE1}, {OE2}, SE3) :- !,
	(oexpr_intersection(OE1, OE2, OE3) -> SE3 = {OE3} ; SE3 = {}).
% sexpr_intersection(SE1\/SE2, SE3, SE6) :- !, % maintain DNF
% 	sexpr_intersection(SE1, SE3, SE4),
% 	sexpr_intersection(SE2, SE3, SE5),
% 	sexpr_union(SE4, SE5, SE6).
% sexpr_intersection(SE1, SE2\/SE3, SE6) :- !, % maintain DNF
% 	sexpr_intersection(SE1, SE2, SE4),
% 	sexpr_intersection(SE1, SE3, SE5),
% 	sexpr_union(SE4, SE5, SE6).
sexpr_intersection(SE1, SE2, SE1/\SE2).

oexpr_intersection([], [], []).
oexpr_intersection([A1..B1|OE1], [A2..B2|OE2], [A3..B3|OE3]) :-
	bound_max(A1, A2, A3),
	bound_min(B1, B2, B3),
	\+aexpr_greater_than(A3, B3),
	oexpr_intersection(OE1, OE2, OE3).

sexpr_subset({}, _) :- !.
sexpr_subset(_, {}) :- !, fail.
sexpr_subset(ltz(_), _) :- !, fail.
sexpr_subset(_, ltz(_)) :- !, fail.
sexpr_subset(sids(_,_,_,_,_), _) :- !, fail.
sexpr_subset(_, sids(_,_,_,_,_)) :- !, fail.
sexpr_subset(SE1/\SE2, SE3) :- !,
	(   sexpr_subset(SE1, SE3)
	;   sexpr_subset(SE2, SE3)
	).
sexpr_subset(SE1\/SE2, SE3) :- !,
	sexpr_subset(SE1, SE3),
	sexpr_subset(SE2, SE3).
sexpr_subset(SE1, SE2/\SE3) :- !,
	sexpr_subset(SE1, SE2),
	sexpr_subset(SE1, SE3).
sexpr_subset(SE1, SE2\/SE3) :- !,
	(   sexpr_subset(SE1, SE2)
	;   sexpr_subset(SE1, SE3)
	).
sexpr_subset({OE1}, {OE2}) :-
	oexpr_subset(OE1, OE2).

oexpr_subset([], []).
oexpr_subset([A1..B1|OE1], [A2..B2|OE2]) :-
	bound_min(A1, A2, A2),
	bound_max(B1, B2, B2),
	oexpr_subset(OE1, OE2).

aexpr_greater_than(sup, _) :- !.
aexpr_greater_than(inf, _) :- !, fail.
aexpr_greater_than(_, sup) :- !, fail.
aexpr_greater_than(_, inf) :- !.
aexpr_greater_than(int(X), int(Y)) :- !,
	X>Y.
aexpr_greater_than(Min, Max) :-
	decompose_sum(Min, Rmax, C),
	decompose_sum(Max, Rmin, D),
	C>D,
	ixref(Rmax, R, max),
	ixref(Rmin, R, min).

bound_min(inf, _, inf) :- !.
bound_min(_, inf, inf) :- !.
bound_min(sup, X, X) :- !.
bound_min(X, sup, X) :- !.
bound_min(int(X), int(Y), int(Z)) :- !,
	Z is min(X,Y).
bound_min(X, Y, Expr) :-
	decompose_sum(X, Var, Con1),
	decompose_sum(Y, Var, Con2), !,
	(Con1=<Con2 -> Expr=X ; Expr=Y).
bound_min(X, Y, min(X,Y)).

bound_max(sup, _, sup) :- !.
bound_max(_, sup, sup) :- !.
bound_max(inf, X, X) :- !.
bound_max(X, inf, X) :- !.
bound_max(int(X), int(Y), int(Z)) :- !,
	Z is max(X,Y).
bound_max(X, Y, Expr) :-
	decompose_sum(X, Var, Con1),
	decompose_sum(Y, Var, Con2), !,
	(Con1>=Con2 -> Expr=X ; Expr=Y).
bound_max(X, Y, max(X,Y)).

decompose_sum(int(C) + X, X, C) :- !,
	ixref(X, _, _).
decompose_sum(X + int(C), X, C) :- !,
	ixref(X, _, _).
decompose_sum(X - int(C), X, D) :- !,
	D is -C,
	ixref(X, _, _).
decompose_sum(X, X, 0) :-
	ixref(X, _, _).

ixref(xmin(A,B), x(A,B), min).
ixref(xmax(A,B), x(A,B), max).
ixref(pmin(A,B,C), p(A,B,C), min).
ixref(pmax(A,B,C), p(A,B,C), max).

%%% S-expression to procedure

compile_forbidden_for_object({}, _, _) --> !.
compile_forbidden_for_object(SE1\/SE2, X, Context) --> !,
	compile_forbidden_for_object(SE1, X, Context),
	compile_forbidden_for_object(SE2, X, Context).
compile_forbidden_for_object({OE}, o(I), context(Objects,_)) -->
	{ground_oexpr(OE, J, Min, Max)}, !,
	{memberchk(object(I,_,Xs,_), Objects)},
	{nth1(J, Xs, X)},
	{fdset_parts(Unsafe, Min, Max, [])},
	{fdset_complement(Unsafe, Safe)},
	{X in_set Safe}.
compile_forbidden_for_object(SE, OX, _) --> [OX-Proc],
	{sexpr_to_procedure(SE, OX, Proc)}.

ground_oexpr(OE, J, Min, Max) :-
	nth1(J, OE, Pair, Rest),
	Pair \== inf..sup,
	Pair = A..B,
	simple_aexpr(A, Min, _),
	simple_aexpr(B, Max, _),
	(   foreach(inf..sup,Rest)
	do  true
	).

sexpr_to_procedure(SE, OX, procedure(NTemp,NStack,Dest,Insns,Dep2)) :-
	empty_avl(Map1),
	sexpr_to_procedure(SE, Dest, OX, Map1, _, Insns, []),
	ascending_values(Insns, 2, Counts, []),
	max_member(NTemp, Counts),
	length(Counts, NC),
	NStack is NC//2,
	dependencies(Insns, Dep1, []),
	sort(Dep1, Dep2).

sexpr_to_procedure(SExpr, Dest, _, Map, Map) -->
	{avl_fetch(SExpr, Map, Dest)}, !.
sexpr_to_procedure(SExpr, Dest, OX, Map1, Map3) -->
	sexpr_to_procedure1(SExpr, Dest, OX, Map1, Map2),
	{avl_store(SExpr, Map2, Dest, Map3)}.

% N.B. Any obox that depends e.g. on object(3)^x(2) where object(3) is being filtered
%      is only valid for the value of x(2) given in the current sweep point.
sexpr_to_procedure1({OExpr}, Dest, OX, Map1, Map) -->
	{oexpr_self_use(OExpr, OX, Dims1, [])},
	{Dims1 = [_|_]}, !,
	{sort(Dims1, Dims2)},
	oexpr_to_procedure(OExpr, OX, Map1, Map2, Args1),
	self_oexpr(OExpr, Dims2, 0, OX, Map2, Map, Args2),
	[obox(Args1)-Dest1,
	 obox(Args2)-Dest2,
	 intersection(Dest1,Dest2)-Dest].
sexpr_to_procedure1({OExpr}, Dest, OX, Map1, Map2) -->
	oexpr_to_procedure(OExpr, OX, Map1, Map2, Args),
	[obox(Args)-Dest].
sexpr_to_procedure1(ltz(A), Dest, OX, Map1, Map2) -->
	aexpr_to_procedure(A, Ad, OX, Map1, Map2),
	[ltz(Ad)-Dest].
sexpr_to_procedure1(sids(_I,J,A,Op,Rhs), Dest, OX, Map1, Map2) -->
	aexpr_to_procedure(Rhs, Ed, OX, Map1, Map2),
	% for sweeping polymorphic objects:
	% [sids(_I,J,A,Op,Ed)-Dest].
	% for sweeping monomorphic objects:
	[pget(J,A)-R1],
	({Op == (=<)} -> [minus(R1,Ed)-R2] ; [minus(Ed,R1)-R2]),
	[lez(R2)-Dest].
sexpr_to_procedure1(SE1/\SE2, Dest3, OX, Map1, Map3) -->
	sexpr_to_procedure(SE1, Dest1, OX, Map1, Map2),
	sexpr_to_procedure(SE2, Dest2, OX, Map2, Map3),
	[intersection(Dest1,Dest2)-Dest3].
sexpr_to_procedure1(SE1\/SE2, Dest, OX, Map, Map) -->
	{sexpr_to_procedure(SE1, Dest1, OX, Map, _, Sub1, [])},
	{sexpr_to_procedure(SE2, Dest2, OX, Map, _, Sub2, [])},
	[union(Dest1,Dest2,Sub1,Sub2)-Dest].

oexpr_self_use([], _) --> [].
oexpr_self_use([A..B|OExpr], OX) -->
	aexpr_self_use(A, OX),
	aexpr_self_use(B, OX),
	oexpr_self_use(OExpr, OX).

aexpr_self_use(xmin(I,J), o(I)) --> !, [J].
aexpr_self_use(xmax(I,J), o(I)) --> !, [J].
aexpr_self_use(pmin(I,J,_), o(I)) --> !, [J].
aexpr_self_use(pmax(I,J,_), o(I)) --> !, [J].
aexpr_self_use(A+B, OX) --> !,
	aexpr_self_use(A, OX),
	aexpr_self_use(B, OX).
aexpr_self_use(A-B, OX) --> !,
	aexpr_self_use(A, OX),
	aexpr_self_use(B, OX).
aexpr_self_use(mul(A,_), OX) --> !,
	aexpr_self_use(A, OX).
aexpr_self_use(floordiv(A,_), OX) --> !,
	aexpr_self_use(A, OX).
aexpr_self_use(ceildiv(A,_), OX) --> !,
	aexpr_self_use(A, OX).
aexpr_self_use(_, _) --> [].

self_oexpr([], [], _, _, Map, Map, []) --> [].
self_oexpr([_|OExpr], [J|Dims], I, OX, Map1, Map, [Ad..Bd|Args]) -->
	{J is I+1}, !,
	{OX = o(OID)},
	aexpr_to_procedure(xmin(OID,J), Ad, OX, Map1, Map2),
	aexpr_to_procedure(xmax(OID,J), Bd, OX, Map2, Map3),
	self_oexpr(OExpr, Dims, J, OX, Map3, Map, Args).
self_oexpr([_|OExpr], Dims, I, OX, Map1, Map, [0..1|Args]) -->
	{J is I+1},
	self_oexpr(OExpr, Dims, J, OX, Map1, Map, Args).
	
oexpr_to_procedure([], _, Map, Map, []) --> [].
oexpr_to_procedure([A..B|OExpr], OX, Map1, Map4, [Ad..Bd|Dest]) -->
	aexpr_to_procedure(A, Ad, OX, Map1, Map2),
	aexpr_to_procedure(B, Bd, OX, Map2, Map3),
	oexpr_to_procedure(OExpr, OX, Map3, Map4, Dest).

aexpr_to_procedure(Aexpr, Dest, _, Map, Map) -->
	{avl_fetch(Aexpr, Map, Dest)}, !.
aexpr_to_procedure(Aexpr, Dest, OX, Map1, Map3) -->
	aexpr_to_procedure1(Aexpr, Dest, OX, Map1, Map2),
	{avl_store(Aexpr, Map2, Dest, Map3)}.

aexpr_to_procedure1(inf, 0, _, Map, Map) --> [].
aexpr_to_procedure1(sup, 1, _, Map, Map) --> [].
aexpr_to_procedure1(int(I), Dest, _, Map, Map) --> [int(I)-Dest].
aexpr_to_procedure1(xmin(I,J), Dest, o(I), Map, Map) --> !, [xget(J)-Dest].
aexpr_to_procedure1(xmin(I,J), Dest, _, Map, Map) --> [xmin(I,J)-Dest].
aexpr_to_procedure1(xmax(I,J), Dest, o(I), Map, Map) --> !, [xget(J)-Dest].
aexpr_to_procedure1(xmax(I,J), Dest, _, Map, Map) --> [xmax(I,J)-Dest].
aexpr_to_procedure1(pmin(I,J,A), Dest, o(I), Map, Map) --> !, [pget(J,A)-Dest].
aexpr_to_procedure1(pmin(I,J,A), Dest, _, Map, Map) --> [pmin(I,J,A)-Dest].
aexpr_to_procedure1(pmax(I,J,A), Dest, o(I), Map, Map) --> !, [pget(J,A)-Dest].
aexpr_to_procedure1(pmax(I,J,A), Dest, _, Map, Map) --> [pmax(I,J,A)-Dest].
aexpr_to_procedure1(+(AE1,AE2), Dest3, OX, Map1, Map3) -->
	aexpr_to_procedure(AE1, Dest1, OX, Map1, Map2),
	aexpr_to_procedure(AE2, Dest2, OX, Map2, Map3),
	[plus(Dest1,Dest2)-Dest3].
aexpr_to_procedure1(-(AE1,AE2), Dest3, OX, Map1, Map3) -->
	aexpr_to_procedure(AE1, Dest1, OX, Map1, Map2),
	aexpr_to_procedure(AE2, Dest2, OX, Map2, Map3),
	[minus(Dest1,Dest2)-Dest3].
aexpr_to_procedure1(mul(AE,Int), Dest2, OX, Map1, Map2) -->
	aexpr_to_procedure(AE, Dest1, OX, Map1, Map2),
	[mul(Dest1,Int)-Dest2].
aexpr_to_procedure1(floordiv(AE,Int), Dest2, OX, Map1, Map2) -->
	aexpr_to_procedure(AE, Dest1, OX, Map1, Map2),
	[floordiv(Dest1,Int)-Dest2].
aexpr_to_procedure1(ceildiv(AE,Int), Dest2, OX, Map1, Map2) -->
	aexpr_to_procedure(AE, Dest1, OX, Map1, Map2),
	[ceildiv(Dest1,Int)-Dest2].

ascending_values([], NTemp) --> [NTemp].
ascending_values([union(_,_,Sub1,Sub2)-I|Insns], I) --> !,
	ascending_values(Sub1, I),
	ascending_values(Sub2, I),
	{J is I+1},
	ascending_values(Insns, J).
ascending_values([_-I|Insns], I) -->
	{J is I+1},
	ascending_values(Insns, J).

dependencies([]) --> [].
dependencies([Insn-_|Insns]) -->
	dependency(Insn),
	dependencies(Insns).

dependency(xmin(ID,_)) --> !, [ID].
dependency(xmax(ID,_)) --> !, [ID].
dependency(pmin(ID,_,_)) --> !, [ID].
dependency(pmax(ID,_,_)) --> !, [ID].
% dependency(sids(ID,_,_,_,_)) --> !, [ID].
dependency(union(_,_,Sub1,Sub2)) --> !,
	dependencies(Sub1),
	dependencies(Sub2).
dependency(_) --> [].

% assemble_procs(ObjProcs1, ObjProcs2)
assemble_procs([], [], _).
assemble_procs([false], [false], _) :- !.
assemble_procs([o(Oid)-procedure(NReg,NStack,Dest,Code,DepsOid)|Procs], [proc(Oix,Dest,NReg,NStack,NCode,NDep,Bytecode,DepsOix)|As], Oid2Oix) :-
	length(DepsOid, NDep),
	avl_fetch(Oid, Oid2Oix, Oix),
	(   foreach(D1,DepsOid),
	    foreach(D2,DepsOix),
	    param(Oid2Oix)
	do  avl_fetch(D1, Oid2Oix, D2)
	),
	assemble(Code, 0, NCode, Oid2Oix, Bytecode, []),
	assemble_procs(Procs, As, Oid2Oix).

assemble([], Off, Off, _) --> [].
assemble([Insn-Dest|Insns], Off0, Off, Oid2Oix) -->
	assemble1(Insn, Dest, Off0, Off1, Oid2Oix),
	assemble(Insns, Off1, Off, Oid2Oix).

assemble1(int(I), Dest, Off0, Off, _) --> [0,Dest,I],
	{Off is Off0+3}.
assemble1(xmin(Oid,J), Dest, Off0, Off, Map) --> [1,Dest,Oix,I],
	{avl_fetch(Oid, Map, Oix)},
	{I is J-1},
	{Off is Off0+4}.
assemble1(xmax(Oid,J), Dest, Off0, Off, Map) --> [2,Dest,Oix,I],
	{avl_fetch(Oid, Map, Oix)},
	{I is J-1},
	{Off is Off0+4}.
assemble1(xget(J), Dest, Off0, Off, _) --> [3,Dest,I],
	{I is J-1},
	{Off is Off0+3}.
assemble1(pmin(Oid,J,l(L)), Dest, Off0, Off, Map) --> !, [4,Dest,Oix,I,K],
	{avl_fetch(Oid, Map, Oix)},
	{I is J-1, K is L-1},
	{Off is Off0+5}.
assemble1(pmax(Oid,J,l(L)), Dest, Off0, Off, Map) --> !, [5,Dest,Oix,I,K],
	{avl_fetch(Oid, Map, Oix)},
	{I is J-1, K is L-1},
	{Off is Off0+5}.
assemble1(pget(J,l(L)), Dest, Off0, Off, _) --> !, [6,Dest,I,K],
	{I is J-1, K is L-1},
	{Off is Off0+4}.
assemble1(pmin(Oid,J,t(L)), Dest, Off0, Off, Map) --> [7,Dest,Oix,I,K],
	{avl_fetch(Oid, Map, Oix)},
	{I is J-1, K is L-1},
	{Off is Off0+5}.
assemble1(pmax(Oid,J,t(L)), Dest, Off0, Off, Map) --> [8,Dest,Oix,I,K],
	{avl_fetch(Oid, Map, Oix)},
	{I is J-1, K is L-1},
	{Off is Off0+5}.
assemble1(pget(J,t(L)), Dest, Off0, Off, _) --> [9,Dest,I,K],
	{I is J-1, K is L-1},
	{Off is Off0+4}.
assemble1(plus(R1,R2), Dest, Off0, Off, _) --> [10,Dest,R1,R2],
	{Off is Off0+4}.
assemble1(minus(R1,R2), Dest, Off0, Off, _) --> [11,Dest,R1,R2],
	{Off is Off0+4}.
assemble1(mul(R1,R2), Dest, Off0, Off, _) --> [12,Dest,R1,R2],
	{Off is Off0+4}.
assemble1(floordiv(R1,R2), Dest, Off0, Off, _) --> [13,Dest,R1,R2],
	{Off is Off0+4}.
assemble1(ceildiv(R1,R2), Dest, Off0, Off, _) --> [14,Dest,R1,R2],
	{Off is Off0+4}.
assemble1(obox(L), Dest, Off0, Off, _) --> [15,Dest],
	assemble_obox(L),
	{length(L, N)},
	{Off is Off0+2*N}.
assemble1(ltz(R1), Dest, Off0, Off, _) --> [16,Dest,R1],
	{Off is Off0+3}.
assemble1(lez(R1), Dest, Off0, Off, _) --> [17,Dest,R1],
	{Off is Off0+3}.
assemble1(intersection(R1,R2), Dest, Off0, Off, _) --> [18,Dest,R1,R2],
	{Off is Off0+4}.
assemble1(union(R1,R2,Code1,Code2), Dest, Off0, Off, Map) --> [19,Off3/*to Code2*/],
	{Off1 is Off0+2},
	assemble(Code1, Off1, Off2, Map),
	[20,Off4/*to join*/],
	{Off3 is Off2+2},
	assemble(Code2, Off3, Off4, Map),
	[21,Dest,R1,R2],
	{Off is Off4+4}.

assemble_obox([_]) --> !.
assemble_obox([R1..R2|L]) --> [R1,R2],
	assemble_obox(L).

% opcode(INT, 0).
% opcode(XMIN, 1).
% opcode(XMAX, 2).
% opcode(XGET, 3).
% opcode(LMIN, 4).
% opcode(LMAX, 5).
% opcode(LGET, 6).
% opcode(TMIN, 7).
% opcode(TMAX, 8).
% opcode(TGET, 9).
% opcode(PLUS, 10).
% opcode(MINUS, 11).
% opcode(MUL, 12).
% opcode(FLOORDIV, 13).
% opcode(CEILDIV, 14).
% opcode(OBOX, 15).
% opcode(LTZ, 16).
% opcode(LEZ, 17).
% opcode(INTERSECTION, 18).
% opcode(TRY, 19).
% opcode(TRUST, 20).
% opcode(JOIN, 21).

end_of_file.

% Pretty experimental stuff, currently disabled

% exported
geost_domination_data(Sizes, Gabarit, data(DataX,DataY)) :-
	(   foreach([W,H],Sizes),
	    foreach([H,W],SwapSizes)
	do  true
	),
	domination_data(Sizes, SwapSizes, Gabarit, DataX, DataY).

% exported
geost_domination_post(Objs, Sizes, Gabarit, data(DataX,DataY)) :-
	Gabarit = [MaxX,MaxY],
	SwapGabarit = [MaxY,MaxX],
	transpose(Objs, [Xs,Ys]),
	(   foreach([W,H],Sizes),
	    foreach([H,W],SwapSizes)
	do  true
	),
	geost_gen_relations(DataX, Sizes,     Gabarit,     Xs, CtrX, []),
	(   foreach(X,CtrX)
	do  geost_eval_relation(X)
	),
	geost_gen_relations(DataY, SwapSizes, SwapGabarit, Ys, CtrY, []),
	(   foreach(Y,CtrY)
	do  geost_eval_relation(Y)
	).

% Reified constraint version
geost_eval_relation(c(notin(Xi,Expr))) :-
	range_to_fdset(Expr, Set),
	fdset_complement(Set, Compl),
	Xi in_set Compl.
geost_eval_relation(c(neq(Xi,A) \/ Conj)) :-
	geost_merge_intervals(Conj, Xj, Set),
	Xi #= A #=> #\in_set(Xj,Set).

geost_merge_intervals(X/\Y, Xj, BC) :-
	geost_merge_intervals(X, Xj, B),
	geost_merge_intervals(Y, Xj, C),
	fdset_union(B, C, BC).
geost_merge_intervals(nin(Xj,B,C), Xj, BC) :-
	fdset_interval(BC, B, C).

geost_gen_relations(Tuples, Sizes,     Gabarit, Os) -->
	{geost_gen_dynamic(Tuples, Sizes,     Gabarit, Pairs1, [])},
	{keysort(Pairs1, Pairs2)},
	{keyclumped(Pairs2, Groups)},
	geost_groups_relation(Groups, Os).

geost_groups_relation([], _) --> [].
geost_groups_relation([k(I)-Values|Groups], Os) --> !, [c(notin(Xi,Expr))],
	{nth1(I, Os, Xi)},
	{list_to_fdset(Values, Set)},
	{fdset_to_range(Set, Expr)},
	geost_groups_relation(Groups, Os).
geost_groups_relation([k(I,J,A)-Disjuncts|Groups], Os) --> [c(neq(Xi,A)\/Expr)],
	{nth1(I, Os, Xi)},
	{nth1(J, Os, Xj)},
	{geost_gen_disjuncts(Disjuncts, Xj, Ds, [])},
	{geost_and(Ds, Expr)},
	geost_groups_relation(Groups, Os).


geost_gen_disjuncts([], _) --> [].
geost_gen_disjuncts([(B-C)|Fs], Xj) --> [nin(Xj,B,C)],
	geost_gen_disjuncts(Fs, Xj).

geost_gen_dynamic([], _, _) --> [].
geost_gen_dynamic([T|Tuples], Sizes, Gabarit) -->
	{T = t(I,V)}, !,
	{Gabarit = [L,_]},
	{nth1(I, Sizes, [Li,_])},
	{V2 is L-V-Li},
	geost_gen_neq(I, V, L, Li),
	geost_gen_neq(I, V2, L, Li),
	geost_gen_dynamic(Tuples, Sizes, Gabarit).
geost_gen_dynamic([T|Tuples], Sizes, Gabarit) -->
	{T = t(I,_,_,H1)},
	{nth1(I, Sizes, [Li,Hi])},
	geost_gen_dynamic_for_i(Sizes, 1, H1, Li, Hi, T, Gabarit),
	geost_gen_dynamic(Tuples, Sizes, Gabarit).

geost_gen_neq(I, V, L, Li) --> 
	({V>0, V<L-Li} -> [k(I)-V] ; []).

geost_gen_dynamic_for_i([], _, _, _, _, _, _) --> [].
geost_gen_dynamic_for_i([[Lj,Hj]|Sizes], J, H1, Li, Hi, T, Gabarit) -->
	(   {Hj>=H1, Hj<Hi} ->
	    geost_gen_dynamic(1, T, J, Li, Lj, Gabarit),
	    geost_gen_dynamic(2, T, J, Li, Lj, Gabarit),
	    geost_gen_dynamic(3, T, J, Li, Lj, Gabarit),
	    geost_gen_dynamic(4, T, J, Li, Lj, Gabarit)
	;   []
	),
	{J1 is J+1},
	geost_gen_dynamic_for_i(Sizes, J1, H1, Li, Hi, T, Gabarit).

geost_gen_dynamic(1, t(I,V,V1,_), J, Li, Lj, [L,_]) --> [k(I,J,V)-(Min-Max)],
	{V>0, V<L-Li},
	{geost_clip_interval(V1, V, L, Lj, Min, Max)}, !.
geost_gen_dynamic(2, t(I,V,V1,_), J, Li, Lj, [L,_]) --> [k(I,J,V)-(Min-Max)],
	{V>0, V<L-Li},
	{B is L-V-Lj},
	{C is L-V1-Lj},
	{geost_clip_interval(B, C, L, Lj, Min, Max)}, !.
geost_gen_dynamic(3, t(I,V,V1,_), J, Li, Lj, [L,_]) --> [k(I,J,V2)-(Min-Max)],
	{V2 is L-V-Li},
	{V2>0, V2<L-Li},
	{geost_clip_interval(V1, V, L, Lj, Min, Max)}, !.
geost_gen_dynamic(4, t(I,V,V1,_), J, Li, Lj, [L,_]) --> [k(I,J,V2)-(Min-Max)],
	{V2 is L-V-Li},
	{V2>0, V2<L-Li},
	{B is L-V-Lj},
	{C is L-V1-Lj},
	{geost_clip_interval(B, C, L, Lj, Min, Max)}, !.
geost_gen_dynamic(_, _, _, _, _, _) --> [].

geost_clip_interval(A1, B1, L, Li, A3, B3) :-
	A2 is max(A1,1),
	A3 is min(A2,L-Li-1),
	B2 is max(B1,1),
	B3 is min(B2,L-Li-1),
	A3 =< B3.

geost_and([X|Xs], Expr) :-
	geost_and(Xs, X, Expr).

geost_and([], Expr, Expr).
geost_and([X|Xs], E0, E) :-
	geost_and(Xs, E0/\X, E).

% MaxSizeX: maximum possible size of the placement space on the x axis
% MaxSizeY: maximum possible size of the placement space on the y axis
% Sizes       : list of rectangles sizes where each rectangle is a term of the form [W,H]
domination_data(Sizes, SwapSizes, [MaxSizeX,MaxSizeY], ResultX, ResultY) :-
	length(Sizes, N),
	MaxX is MaxSizeX // 2,
	geost_gen_dom_ctrs(1, N, MaxX, Sizes, [], ResultX),
	MaxY is MaxSizeY // 2,
	geost_gen_dom_ctrs(1, N, MaxY, SwapSizes, [], ResultY).

geost_gen_dom_ctrs(I, N, _, _, Result, Result) :-
	I > N, !.
geost_gen_dom_ctrs(I, N, Max, Sizes, Previous, Result) :-
	nth1(I, Sizes, [_,H], OtherSizes),
	geost_gen_dom_ctr_rect(1, Max, I, H, OtherSizes, [], Current),
	append(Previous, Current, Next),
	I1 is I+1,
	geost_gen_dom_ctrs(I1, N, Max, Sizes, Next, Result).

geost_gen_dom_ctr_rect(W, Max, _, _, _, Result, Result) :-
	W > Max, !.
geost_gen_dom_ctr_rect(W, Max, I, H, OtherSizes, Previous, Result) :-
	geost_gen(OtherSizes, I, W, H, Current),
	(   Current==0 -> Result = Previous
	;   Current==1 ->		
	    append(Previous, [t(I,W)], Next),
	    W1 is W+1,
	    geost_gen_dom_ctr_rect(W1, Max, I, H, OtherSizes, Next, Result)
	;   append(Previous, Current, Next),
	    W1 is W+1,
	    geost_gen_dom_ctr_rect(W1, Max, I, H, OtherSizes, Next, Result)
	).

geost_gen(Sizes, I, W, H, Result) :-
	geost_try_to_pack(Sizes, W, H, Wres, Hres),
	(   Wres=:=0, Hres=:=0 -> Result=1
	;   (   foreach([Wi,Hi],Sizes),
		foreach(Wi-Hi,WH)
	    do  true
	    ),
	    sort(WH, SWH),
	    reverse(SWH, RSWH),
	    append([W-H], RSWH, ARSWH),
	    geost_disj(ARSWH, W, DARSWH, BVars, BTerm),
	    SumB in 0..H,
	    call(SumB #= BTerm),
	    labeling([leftmost,maximize(SumB),time_out(2000,_)], BVars),
	    geost_reorder(DARSWH, RSizes),
	    geost_try_to_pack(RSizes, W, H, RWres, RHres),
	    geost_try_pack_max_surf(DARSWH, W, H, RRWres, RRHres),
	    geost_merge_results([Wres-Hres, RWres-RHres, RRWres-RRHres], DomCtrs),
	    (   DomCtrs=[0-0] -> Resul=[]
	    ;   Resul=DomCtrs
	    ),
	    geost_complete_pairs(Resul, I, W, Result)
	), !.
geost_gen(_, _, _, _, 0). % 0 indicates that we fail and should stop

geost_complete_pairs([], _, _, []).
geost_complete_pairs([VP-HP|R], I, W, [t(I,W,VP,HP)|S]) :-
	geost_complete_pairs(R, I, W, S).

geost_merge_results(L, R) :-
	geost_merge_results1(L, R1),
	reverse(R1, R2),
	geost_merge_results1(R2, R).

geost_merge_results1([], []).
geost_merge_results1([X|Y], [X|Z]) :-
	geost_not_dominated(Y, X), !,
	geost_merge_results1(Y, Z).
geost_merge_results1([_|Y], Z) :-
	geost_merge_results1(Y, Z).

geost_not_dominated([], _).
geost_not_dominated([Wy-Hy|R],Wx-Hx) :-
	geost_not_dom(Wx,Hx,Wy,Hy),
	geost_not_dominated(R,Wx-Hx).	

geost_not_dom(Wx,_,Wy,_) :- Wx < Wy.
geost_not_dom(_,Hx,_,Hy) :- Hx < Hy.

geost_try_to_pack(Sizes, W, H, Wres, Hres) :-
	geost_try_to_pack_rectangles(Sizes, W, H, [], Rout),
	(   Rout==[] -> Wres=0, Hres=0
	;   geost_try_to_pack_rest(Rout, W, H, Wres, Hres)
	).

geost_reorder(B, R) :-
	geost_reorder(B, 0, R0),
	geost_reorder(B, 1, R1),
	append(R1, R0, R).

geost_reorder([], _, []).
geost_reorder([r(W,H,B)|R], B, [[W,H]|S]) :- !,
	geost_reorder(R, B, S).
geost_reorder([_|R], B, S) :-
	geost_reorder(R, B, S).

geost_disj([], _, [], [], 0) :- !.
geost_disj([_], _, [], [], 0) :- !.
geost_disj([W1-_,W2-H2|R], W, [r(W2,H2,B2)|S], [B2|RB], H2*B2+BR) :-
	W12 is W1+W2,
	W12 > W, !,
	B2 in 0..1,
	geost_disj([W2-H2|R], W, S, RB, BR).
geost_disj([W1-_,W2-H2|R], W, [r(W2,H2,0)|S], [], 0) :-
	W12 is W1+W2,
	W12 =< W,
	geost_disj([W2-H2|R], W, S, [], 0).

geost_try_to_pack_rest(Rects, W, H, W2, H2) :-
	(   foreach([Wi,Hi],Rects),
	    fromto(0,N0,N,Surf)
	do  N is N0 + Wi*Hi
	),
	Hmax is min(Surf // W + 10,H-1),
	W2 in 1..W,
	H2 in 1..Hmax,
	W2*H2 #>= Surf,
	Cost #= 10*H2+W2,
	geost_rects(Rects, W2, H2, Rectangles, Origins),
	disjoint2(Rectangles),
	append([Cost,H2,W2], Origins, Vars),
	labeling([leftmost,minimize(Cost),time_out(2000,_)], Vars).

% R: list of rectangle sizes
% W: width of the gap
% H: height of the gap
% Rin: rectangles that can be placed in the gap
% Rout: rectangles that cannot be placed in the gap
geost_try_to_pack_rectangles(R, W, H, Rin, Rout) :-
				% do not consider rectangles of R that have a width greater than W
				% check if all rectangles of FR have a height that is smaller or equal than H
	(   foreach([Wr,Hr],R),
	    fromto(FR,S0,S,[]),
	    param(W,H)
	do  (   Wr > W -> S0 = S
	    ;   Hr =< H -> S0 = [[Wr,Hr]|S]
	    )
	),
	geost_try_to_pack_rects(FR, W, H, Rin, Rout). % try to pack as many rectangles as possible

geost_try_to_pack_rects([], _, _, _, []).
geost_try_to_pack_rects([R|S], W, H, Rin, Rout) :-
	append([R], Rin, Rects),
	geost_pack(Rects, W, H), !,
	geost_try_to_pack_rects(S, W, H, Rects, Rout).
geost_try_to_pack_rects([R|S], W, H, Rin, [R|Rout]) :-
	geost_try_to_pack_rects(S, W, H, Rin, Rout).

geost_pack(Rects, W, H) :-
	reverse(Rects, RevRects),
	geost_rects(RevRects, W, H, Rectangles, Origins),
	disjoint2(Rectangles),
	labeling([leftmost,time_out(2000,_)], Origins).
%	label(Origins).

geost_rects([], _, _, [], []).
geost_rects([[Wr,Hr]|S], W, H, [r(Ox,Wr,Oy,Hr)|R], [Ox,Oy|T]) :-
	fd_max(W, Wmax),
	fd_max(H, Hmax),
	MaxX is Wmax-Wr+1,
	MaxY is Hmax-Hr+1,	
	Ox in 1..MaxX,
	Oy in 1..MaxY,
	Ox + Wr - 1 #=< W,
	Oy + Hr - 1 #=< H,
	geost_rects(S, W, H, R, T).

geost_try_pack_max_surf(Rectangles, W, H, Wres, Hres) :-
	geost_extract_in_maybe(Rectangles, Rin, Rmaybe),
	keysort(Rin, SRin),
	reverse(SRin, RSRin),
	geost_merge_sorted_rect(RSRin, MRSRin),
	geost_available(MRSRin, W, H, 0, ARin),
	reverse(ARin, RARin),
	keysort(Rmaybe, SRmaybe),
	reverse(SRmaybe, RSRmaybe),
	(   foreach(W1-H1,RSRmaybe),
	    foreach(r(W1,H1,B1),VRSRmaybe),
	    foreach(B1,BVars)
	do  B1 in 0..1
	),
	(   foreach(W2-H2,RARin),
	    param(VRSRmaybe)
	do  geost_extract_rect_width_geq(VRSRmaybe, W2, BTerm),
	    call(BTerm #=< H2)
	),
	(   foreach(r(W3,H3,B3),VRSRmaybe),
	    fromto(0,Expr0,WH*B3+Expr0,CostSurf),
	    fromto(0,Surf0,Surf,SurfMax)
	do  WH is W3*H3,
	    Surf is Surf0+WH
	),
	Cost in 0..SurfMax,
	call(CostSurf #= Cost),
	labeling([maximize(Cost),time_out(2000,_)], BVars),
	(   foreach([W4,H4],Rin),
	    foreach(W4-H4,CRin)
	do  true
	),
	geost_reorder(VRSRmaybe, OVRSRmaybe),
	append(CRin, OVRSRmaybe, Ordered),
	geost_try_to_pack(Ordered, W, H, Wres, Hres).

geost_extract_in_maybe([], [], []).
geost_extract_in_maybe([r(W,H,1)|R], [W-H|S], T) :-
	geost_extract_in_maybe(R, S, T).
geost_extract_in_maybe([r(W,H,0)|R], S, [W-H|T]) :-
	geost_extract_in_maybe(R, S, T).

geost_extract_rect_width_geq([r(W,H,B)|S], Wi, H*B+R) :-
	W >= Wi, !,
	geost_extract_rect_width_geq(S, Wi, R).
geost_extract_rect_width_geq(_, _, 0).

geost_merge_sorted_rect([], []) :- !.
geost_merge_sorted_rect([R], [R]) :- !.
geost_merge_sorted_rect([W-H1,W-H2|R], Res) :- !,
	H12 is H1+H2,
	geost_merge_sorted_rect([W-H12|R], Res).
geost_merge_sorted_rect([W1-H1,W2-H2|R], [W1-H1|S]) :-
	geost_merge_sorted_rect([W2-H2|R], S).

geost_available([], W, H, Hp, [W-H2]) :-
	H2 is H-Hp.
geost_available([W1-H1|R], W, H, Hp, [W2-H2|S]) :-
	W2 is W-W1,
	H2 is H-Hp,
	Hpp is Hp+H1,
	geost_available(R, W, H, Hpp, S).
