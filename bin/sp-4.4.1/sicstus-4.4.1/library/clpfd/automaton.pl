/* Copyright(C) 1997, Swedish Institute of Computer Science */

% automaton/[3,8,9]
% decomposes a nondeterministic finite automaton into a system of constraints.

% Any transition not mentioned is assumed to go to an implicit failure node.

% An abstract grammar of the Prolog terms that we will need:

% Args ::= list of Arg /* they must all have the same shape */

% Arg  ::= term        /* most general shape of the Args */
                       /* its variables should be local to the constraint */

% Signature ::= list of dvar

% SourcesSinks ::= list of SourceSink

% SourceSink ::= source(Node) /* Node is an initial state */
%              | sink(Node)   /* Node is an accept state */

% Node ::= term

% Arcs  ::= list of Arc

% Arc      ::= arc(Node,integer,Node)
%            | arc(Node,integer,Node,Exprs)
%            | arc(Node,integer,Node,Conditional)

% Conditional ::= (Cond -> Exprs)
%               | (Conditional ; Conditional)

% Exprs := list of FD arith expr /* over Counters, Arg and constants */
                                 /* same length as Counters */

% Cond := any constraint /* on Counters, Arg and constants */

% Counters ::= list of var    /* should be local to the constraint */

% Initial ::= list of integer /* same length as Counters */

% Final ::= list of dvar      /* same length as Counters */

% AutoConstraint ::= automaton(Args, Arg, Signature,
%                              SourcesSinks, Arcs,
% 			       Counters, Initial, Final, list of Option)
%                  | automaton(Args, Arg, Signature,
%                              SourcesSinks, Arcs,
% 			       Counters, Initial, Final)
%                  | automaton(Signature, SourcesSinks, Arcs)

% Option ::= state(Map,StateSequence)
%          | counterseq(CounterSequence)
%          | valueprec(First,Later,N)
%          | anystretchocc(N)
%          | stretchocc(ValuePat,N)
%          | stretchoccmod(ValuePat,Mod,N)
%          | stretchmaxlen(ValuePat,N)
%          | stretchminlen(ValuePat,N)
%          | wordocc(WordPat,N)
%          | wordoccmod(WordPat,Mod,N)
%          | wordprefix(WordPat,ZO)
%          | wordsuffix(WordPat,ZO)

% ValuePat ::= int
%            | [int,int,...]
%            | int/int/...

% WordPat ::= [ValuePat,ValuePat,...]

% First ::= int

% Later ::= int

% Mod ::= int


automaton(Signature, SourcesSinks, Arcs) :-
	automaton(Signature, _, Signature, SourcesSinks, Arcs, [], [], [], []).

automaton(CtrArgs, CtrTuple, Signature, SourcesSinks, Arcs, Counters, Initial, Final) :-
	automaton(CtrArgs, CtrTuple, Signature, SourcesSinks, Arcs, Counters, Initial, Final, []).

automaton(CtrArgs, CtrTuple, Signature, SourcesSinks, Arcs0, Counters1, Initial1, Final1, Options) :-
	Goal = automaton(CtrArgs,CtrTuple,Signature,SourcesSinks,Arcs0,Counters1,Initial1,Final1,Options),
	must_be(CtrArgs, proper_list, Goal, 1),
	must_be(Signature, proper_list, Goal, 3),
	must_be(SourcesSinks, proper_list(callable), Goal, 4),
	must_be(Arcs0, proper_list(callable), Goal, 5),
	must_be(Counters1, proper_list, Goal, 6),
	must_be(Initial1, proper_list, Goal, 7),
	must_be(Final1, proper_list, Goal, 8),
	must_be(Options, proper_list, Goal, 9),
	(   foreach(Opt,Options),
	    fromto(f(_,_,_,[]),Opt1,Opt2,f(Nodes3,[S1|StateVars],[Initial1|CtrSeq],Ann)),
	    param(Goal)
	do  automaton_option(Opt, Opt1, Opt2, Goal, 9)
	),
	(   foreach(Arc0,Arcs0),
	    foreach(Arc,Arcs1),
	    param(Counters1)
	do  automaton_normalize(Arc0, Counters1, Arc)
	),
	kernel_annotate(Ann, kernel(SourcesSinks,Arcs1 ,Counters1 ,Initial1 ,Final1 ),
			     kernel(SourcesSinks,Arcs1a,Counters1a,Initial1a,Final1a)),
	(   foreach(arc(N3,_,N4,_),Arcs1a),
	    fromto(Nodes1,[N3,N4|S],S,[])
	do  true
	),
	sort(Nodes1, Nodes2),
	(   foreach(N,Nodes2),
	    foreach(N-J,Nodes3),
	    count(J,1,_)
	do  true
	),
	list_to_avl(Nodes3, Node2Int),
	(   foreach(arc(From1,Via,To1,Expr),Arcs1a),
	    foreach(arc(From2,Via,To2,Expr),Arcs2),
	    param(Node2Int)
	do  avl_fetch(From1, Node2Int, From2),
	    avl_fetch(To1, Node2Int, To2)
	),
	sort(Arcs2, Arcs3),
	automaton_map_arcs_case(CtrTuple, Counters1, Counters1a, Arcs3, Extension, Clause),
	automaton_state(source, SourcesSinks, S1, Node2Int),
	automaton_state(sink, SourcesSinks, Sn, Node2Int),
	(   foreach(Sig,Signature),
	    foreach(S3,StateVars),
	    foreach(C3tail,CtrSeq),
	    fromto(S1,S2,S3,Sn),
	    foreach([S2,Sig,S3,Leaf1],ArgsLs),
	    foreach(CtrArg,CtrArgs),
	    fromto(Initial1a,C2,C3,Final1a),
	    param(Clause)
	do  copy_term(Clause, (Head2:-Body2)),
	    Head2 = aux(CtrArg, C2, C3, C3tail, Leaf1),
	    call(Body2)
	),
	table_compact(ArgsLs, Extension, 4, _, Goal).

automaton_option(Ann, _, _, Goal, Argno) :-
	var(Ann), !,
	illarg(domain(term,automaton_option), Goal, Argno, Ann).
automaton_option(state(Map,StateVars), f(_,_,CtrSeq,Ann), f(Map,StateVars,CtrSeq,Ann), _, _) :- !.
automaton_option(counterseq(CtrSeq), f(M,SV,_,Anns), f(M,SV,CtrSeq,Anns), _, _) :- !.
automaton_option(Ann, f(M,SV,CtrSeq,Anns), f(M,SV,CtrSeq,[Spec|Anns]), _, _) :-
	normalize_spec(Ann, Spec), !.
automaton_option(Ann, _, _, Goal, Argno) :-
	illarg(domain(term,automaton_option), Goal, Argno, Ann).

automaton_state(F, SourcesSinks, S1, Avl) :-
	automaton_state_list(SourcesSinks, F, List, Avl),
	list_to_fdset(List, Set),
	S1 in_set Set.

automaton_state_list([], _, [], _).
automaton_state_list([Node|Nodes], F, [Y|List], Avl) :-
	Node =.. [F,X],
	avl_fetch(X, Avl, Y), !,
	automaton_state_list(Nodes, F, List, Avl).
automaton_state_list([_|Nodes], F, List, Avl) :-
	automaton_state_list(Nodes, F, List, Avl).

automaton_normalize(arc(From,Via,To), Counters, arc(From,Via,To,(true->Counters))) :- !.
automaton_normalize(arc(From,Via,To,Counters), _, arc(From,Via,To,(true->Counters))) :-
	\+functor(Counters, ->, 2),
	\+functor(Counters,  ;, 2), !.
automaton_normalize(Arc, _, Arc).

automaton_map_arcs_case(CtrTuple, Counters0, Counters1, Arcs, Extension, Clause) :-
	(   foreach(arc(_,_,At,Ae),Arcs),
	    foreach(At-Ae,Leaves1)
	do  true
	),
	sort(Leaves1, Leaves2),
	(   foreach(Lt-Le,Leaves2),
	    foreach((Lt-Le)-I,Leaves3),
	    count(I,1,_)
	do  true
	),
	ord_list_to_avl(Leaves3, Leaf2ID),
	(   foreach(arc(Bf,Bv,Bt,Be),Arcs),
	    foreach([BfSet,BvSet,BtSet,IDSet],Extension),
	    param(Leaf2ID)
	do  avl_fetch(Bt-Be, Leaf2ID, ID),
	    '$fd_range'(Bf, Bf, BfSet, 1),
	    '$fd_range'(Bv, Bv, BvSet, 1),
	    '$fd_range'(Bt, Bt, BtSet, 1),
	    '$fd_range'(ID, ID, IDSet, 1)
	),
	automaton_aux_code(Leaf, Leaves2, Counters2, AuxGoals, []),
	commafy(AuxGoals, AuxCode),
	length(Counters0, N),
	suffix_length(Counters2, Ctr2tail, N),
	Clause = (aux(CtrTuple,Counters1,Counters2,Ctr2tail,Leaf) :- AuxCode).

automaton_aux_code(Leaf, Transitions, Counters2) -->
	automaton_expression_code(Transitions, Expr2Var),
	automaton_cond_code(Transitions, Indices, CounterCases, 1, 1, Leaf, Expr2Var),
	automaton_emit(Leaf,Indices,Index),
	(   {transpose(CounterCases, CounterCasesT)} ->
	    (   foreach(Row,CounterCasesT),
		foreach(C,Counters2),
		param(Index)
	    do  automaton_emit(Index, Row, C)
	    )
	;   {Counters2 = []}
	).

automaton_expression_code(Transitions, Expr2Var) -->
	{   foreach(_-Dest,Transitions),
	    fromto(L1,S0,S,[])
	do  automaton_args(Dest, ;, Cases, []),
	    (   foreach((_->E),Cases),
		fromto(S0,S1,S2,S)
	    do  append(E, S2, S1)
	    )
	},
	{sort(L1, L2)},
	(   foreach(X,L2),
	    foreach(X-Y,L3)
	do  ({simple(X)} -> {Y = X} ; [Y #= X])
	),
	{ord_list_to_avl(L3, Expr2Var)}.

automaton_cond_code([], [], [], _, _, _, _) --> [].
automaton_cond_code([_-Dest|Transitions], [Ix|Ixs], CtrCases1, NextL, NextI, Leaf, Expr2Var) -->
	{automaton_args(Dest, ;, Cases, [])},
	{length(Cases, NC)},
	(   {Cases=[(true->_)]}
	->  {Ix = NextI}
	;   {Cases=[(P->_)]}
	->  {Ix = NextI},
	    [Leaf#=NextL #=> P]
	;   {MaxI is NextI+NC-1},
	    [Leaf#=NextL #=> Ix in NextI..MaxI],
	    (   foreach((P->_),Cases),
		count(I,NextI,_),
		param(Ix)
	    do  [P #<=> Ix#=I]
	    )
	),
	{   foreach((_->Tuple),Cases),
	    fromto(CtrCases1,[Col|Cols],Cols,CtrCases2),
	    param(Expr2Var)
	do  (   foreach(X,Tuple),
		foreach(Y,Col),
		param(Expr2Var)
	    do  avl_fetch(X, Expr2Var, Y)
	    )
	},
	{NextL1 is NextL+1},
	{NextI1 is NextI+NC},
	automaton_cond_code(Transitions, Ixs, CtrCases2, NextL1, NextI1, Leaf, Expr2Var).

automaton_args(Term, F) -->
	{Term =.. [F,X,Y]}, !,
	automaton_args(X, F),
	automaton_args(Y, F).
automaton_args(X, _) --> [X].

automaton_emit(1, [X], X) --> !.
automaton_emit(N, List, Nth) -->
	{   foreach(X,List),
	    count(I,1,_)
	do  X==I
	}, !,
	{Nth = N}.
automaton_emit(_, [X|List], Nth) -->
	{   foreach(Y,List),
	    param(X)
	do  X==Y
	}, !,
	{Nth = X}.
automaton_emit(N, List, Nth) -->
	[element(N, List, Nth)].

kernel_annotate(Specs, Kernel0, Kernel) :-
	Kernel0 = kernel(SourcesSinks,Arcs0,Counters0,Initial0,Final0),
	Kernel  = kernel(SourcesSinks,Arcs,Counters,Initial,Final),
	(   foreach(Spec0,Specs),
	    fromto(Arcs0,Arcs1,Arcs2,Arcs),
	    fromto(Counters0,Counters1,Counters2,Counters),
	    fromto(Initial0,Initial1,Initial2,Initial),
	    fromto(Final0,Final1,Final2,Final)
	do  normalize_spec(Spec0, Spec),
	    boundary_annotation(Spec, Initial3, Final3),
	    length(Initial3, Len),
	    length(Ctrs, Len),
	    append(Ctrs, Counters1, Counters2),
	    append(Initial3, Initial1, Initial2),
	    append(Final3, Final1, Final2),
	    (   foreach(arc(S1,U,S2,Cond1),Arcs1),
		foreach(arc(S1,U,S2,Cond2),Arcs2),
		param(Spec,Ctrs)
	    do  arc_annotation(Spec, U, Ctrs, CtrsA),
		conjoin((true->CtrsA), Cond1, Cond2)
	    )
	).

normalize_spec(valueprec(U,V,N), valueprec(U,V,N)).
normalize_spec(anystretchocc(N), anystretchocc(N)).
normalize_spec(stretchocc(V1,N), stretchocc(V2,N)) :-
	value_pattern(V1, V2).
normalize_spec(stretchoccmod(V1,M,N), stretchoccmod(V2,M,N)) :-
	value_pattern(V1, V2).
normalize_spec(stretchmaxlen(V1,N), stretchmaxlen(V2,N)) :-
	value_pattern(V1, V2).
normalize_spec(stretchminlen(V1,N), stretchminlen(V2,N)) :-
	value_pattern(V1, V2).
normalize_spec(wordocc(V1,N), wordocc(V2,N)) :-
	word_pattern(V1, V2).
normalize_spec(wordoccmod(V1,M,N), wordoccmod(V2,M,N)) :-
	word_pattern(V1, V2).
normalize_spec(wordprefix(V1,N), wordprefix(V2,N)) :-
	word_pattern(V1, V2).
normalize_spec(wordsuffix(V1,N), wordsuffix(V2,N)) :-
	word_pattern(V1, V2).

word_pattern(L1, L2) :-
	(   foreach(V1,L1),
	    foreach(V2,L2)
	do  value_pattern(V1, V2)
	).

value_pattern(V1, V2) :-
	(   atomic(V1) -> V2 = [V1]
	;   V1 = _/_   -> slash_pattern(V1, V2, [])	    
	;   V1 = [_|_], V2 = V1
	).

slash_pattern(H/T) --> !, [H],
	slash_pattern(T).
slash_pattern(X) --> [X].

boundary_annotation(valueprec(_,_,N), [0,0], [N,_]).
boundary_annotation(anystretchocc(N), [0,-0xffffff], [N,_]).
boundary_annotation(stretchocc(_,N), [0,0], [N,_]).
boundary_annotation(stretchoccmod(_,_,N), [0,0], [N,_]).
boundary_annotation(stretchmaxlen(_,N), [0,0], [N,_]).
boundary_annotation(stretchminlen(_,N), [F,F,0], [N,_,_]) :-
	F = 0xffffff.
boundary_annotation(wordocc(Word,N), Initial, Final) :-
	(   foreach(_,Word),
	    foreach(0,Initial),
	    foreach(_,Final)
	do  true
	),
	last(Final, N).
boundary_annotation(wordoccmod(Word,_,N), Initial, Final) :-
	boundary_annotation(wordocc(Word,N), Initial, Final).
boundary_annotation(wordprefix(Word,ZO), [1|Initial], [_|Final]) :-
	boundary_annotation(wordocc(Word,ZO), Initial, Final).
boundary_annotation(wordsuffix(Word,ZO), Initial, Final) :-
	boundary_annotation(wordocc(Word,ZO), Initial, Final).

arc_annotation(valueprec(X,Y,_), U, [C,D], [Cnew,Dnew]) :-
	(U=:=Y -> Cnew = max(C,D) ; Cnew = C),
	(U=:=X -> Dnew = D+1 ; U=:=Y -> Dnew = -0xffffff ; Dnew = D).
arc_annotation(anystretchocc(_), U, [C,D], [C+min(1,abs(D-U)),U]).
arc_annotation(stretchocc(V,_), U, [C,D], [Cnew,Dnew]) :-
	(memberchk(U,V) -> Cnew = C-D+1 ; Cnew = C),
	(memberchk(U,V) -> Dnew = 1     ; Dnew = 0).
arc_annotation(stretchoccmod(V,Mod,_), U, [C,D], [Cnew,Dnew]) :-
	(memberchk(U,V) -> Cnew = (C-D+1) mod Mod ; Cnew = C),
	(memberchk(U,V) -> Dnew = 1               ; Dnew = 0).
arc_annotation(stretchmaxlen(V,_), U, [C,D], [Cnew,Dnew]) :-
	(memberchk(U,V) -> Cnew = max(C,D+1) ; Cnew = C),
	(memberchk(U,V) -> Dnew = D+1        ; Dnew = 0).
arc_annotation(stretchminlen(V,_), U, [C,D,E], [Cnew,Dnew,Enew]) :-
	(memberchk(U,V) -> Cnew = min(D,E+1) ; Cnew = C),
	(memberchk(U,V) -> Dnew = D          ; Dnew = C),
	(memberchk(U,V) -> Enew = E+1        ; Enew = 0).
arc_annotation(wordocc(Word,_), U, Ctrs0, Ctrs) :-
	wordocc_map(Word, Ctrs0, Ctrs, U, 1).
arc_annotation(wordoccmod(Word,Mod,_), U, Ctrs0, Ctrs) :-
	wordoccmod_map(Word, Ctrs0, Ctrs, Mod, U, 1).
arc_annotation(wordprefix(Word,_), U, [C0|Ctrs0], [0|Ctrs]) :-
	wordprefix_map(Word, Ctrs0, Ctrs, U, C0).
arc_annotation(wordsuffix(Word,_), U, Ctrs0, Ctrs) :-
	wordsuffix_map(Word, Ctrs0, Ctrs, U, 1).

wordocc_map([], [], [], _, _).
wordocc_map([W], [C0], [C], U, B0) :- !,
	(memberchk(U,W) -> C=C0+B0 ; C=C0).
wordocc_map([W|Word], [C0|Cs0], [C|Cs], U, B0) :-
	(memberchk(U,W) -> C=B0 ; C=0),
	wordocc_map(Word, Cs0, Cs, U, C0).

wordoccmod_map([], [], [], _, _, _).
wordoccmod_map([W], [C0], [C], Mod, U, B0) :- !,
	(memberchk(U,W) -> C=(C0+B0) mod Mod ; C=C0).
wordoccmod_map([W|Word], [C0|Cs0], [C|Cs], Mod, U, B0) :-
	(memberchk(U,W) -> C=B0 ; C=0),
	wordoccmod_map(Word, Cs0, Cs, Mod, U, C0).

wordprefix_map([], [], [], _, _).
wordprefix_map([W], [C0], [C], U, B0) :- !,
	(memberchk(U,W) -> C=max(B0,C0) ; C=C0).
wordprefix_map([W|Word], [C0|Cs0], [C|Cs], U, B0) :-
	(memberchk(U,W) -> C=B0 ; C=0),
	wordprefix_map(Word, Cs0, Cs, U, C0).

wordsuffix_map([], [], [], _, _).
wordsuffix_map([W], [_], [C], U, B0) :- !,
	(memberchk(U,W) -> C=B0 ; C=0).
wordsuffix_map([W|Word], [C0|Cs0], [C|Cs], U, B0) :-
	(memberchk(U,W) -> C=B0 ; C=0),
	wordsuffix_map(Word, Cs0, Cs, U, C0).

conjoin((true->C1), (P2->C2), (P2->C12)) :- !,
	append(C1, C2, C12).
conjoin((P1->C1), (true->C2), (P1->C12)) :- !,
	append(C1, C2, C12).
conjoin((P1->C1), (P2->C2), ((P1,P2)->C12)) :- !,
	append(C1, C2, C12).
conjoin((P1;P2), Q, (R1;R2)) :- !,
	conjoin(P1, Q, R1),
	conjoin(P2, Q, R2).
conjoin(P, (Q1;Q2), (R1;R2)) :-
	conjoin(P, Q1, R1),
	conjoin(P, Q2, R2).
