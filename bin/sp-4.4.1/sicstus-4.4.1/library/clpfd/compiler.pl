/* Copyright(C) 1997, Swedish Institute of Computer Science */

:- public rt_compile/4. % term expansions and Suite.

% Return the line number of the first line of the term layout
condense_layout([FL0|_], FL) :- !, FL=FL0.
condense_layout(FL, FL).

:- multifile
	user:term_expansion/6.

user:term_expansion((Head-:Body), Lay0, Ids, Expansion, Lay, [clpfd|Ids]) :-
	nonmember(clpfd, Ids), !,
        fd_expansion(Head, Body, 0, Expansion),
        condense_layout(Lay0, Lay).
user:term_expansion((Head+:Body), Lay0, Ids, Expansion, Lay, [clpfd|Ids]) :-
	nonmember(clpfd, Ids), !,
        fd_expansion(Head, Body, 1, Expansion),
        condense_layout(Lay0, Lay).
user:term_expansion((Head-?Body), Lay0, Ids, Expansion, Lay, [clpfd|Ids]) :-
	nonmember(clpfd, Ids), !,
        fd_expansion(Head, Body, 2, Expansion),
        condense_layout(Lay0, Lay).
user:term_expansion((Head+?Body), Lay0, Ids, Expansion, Lay, [clpfd|Ids]) :-
	nonmember(clpfd, Ids), !,
        fd_expansion(Head, Body, 3, Expansion),
        condense_layout(Lay0, Lay).

type_neck(0, -:).
type_neck(1, +:).
type_neck(2, -?).
type_neck(3, +?).

fd_expansion(Head1, Body, Type, [(:-initialization clpfd:rt_compile(Module, Head, Body, Type))]) :-
	(   Head1=Module:Head -> true
	;   Head1=Head,
	    prolog_load_context(module, Module)
	).

rt_compile(Module, Head, Body, Type) :-
	functor(Head, Functor, Arity),
	Spec = Functor/Arity,
	Head =..[Functor|Args],
	numbervars(Head, 0, _),
	type_neck(Type, Neck),
	(   on_exception(Error,
	                 compile(Body, Type, Info, Args),
		         handle_compile_exception(Error, Spec, Neck))
	->  true
	;   print_message(warning, format('compilation of FD predicate clause failed for ~q (clause ~a)',[Spec,Neck])),
	    fail
	),
	length(Info, InfoLength),
	check_info(Type, Info, Spec, Neck),
	'$fd_install'(Spec, Module, Type, InfoLength, Info).

check_info(0, Info, Spec, Neck) :-
	check_info(1, Info, Spec, Neck).
check_info(1, Info, Spec, Neck) :-
	(   foreach(info(_,_,P,_,_,_,_),Info),
	    foreach(P,Lhs)
	do  true
	),
	sort(Lhs, Set),
	length(Lhs, N1),
	length(Set, N2),
	(   N1=:=N2 -> true
	;   print_message(warning, format('multiple propagating indexicals for an argument of ~q (clause ~a)',[Spec,Neck]))
	).
check_info(2, Info, Spec, Neck) :-
	check_info(3, Info, Spec, Neck).
check_info(3, Info, Spec, Neck) :-
	length(Info, N),
	(   N=:=1 -> true
	;   print_message(warning, format('multiple checking indexicals for ~q (clause ~a)',[Spec,Neck]))
	).

handle_compile_exception(domain(D), Spec,Neck) :- !,
	print_message(warning, format('~q - bad domain in finite domain range in ~q (clause ~a)',[D,Spec,Neck])),
	fail.
handle_compile_exception(term(T), Spec,Neck) :- !,
	print_message(warning, format('~q - bad term in finite domain range in ~q (clause ~a)',[T,Spec,Neck])),
	fail.
handle_compile_exception(idmember(X), Spec,Neck) :- !,
	print_message(warning, format('~q - unknown variable in finite domain range in ~q (clause ~a)',[X,Spec,Neck])),
	fail.
handle_compile_exception(Excp, _, _) :-
	throw(Excp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FD COMPILER
%% Generating DCG's
%%

compile((Expr1,Expr2), Type, Codes, Args) :- !,
	compile(Expr1, Type, Codes1, Args),
	compile(Expr2, Type, Codes2, Args),
	append(Codes1, Codes2, Codes).
compile(X in R, Type, Codes, _) :- !,
	Codes = [info(Linkage,LinkageLength,Pruned,Code,CodeLength,Lit,LitLength)],
	indexical(X, R, Type, Linkage, LinkageLength,
	          Pruned, CodeLength1, [], Lit, Code1, []),
	peep_code(Code1, CodeLength1, Code, CodeLength),
	length(Lit, LitLength).
compile(Body, 1, Codes, Args) :-
	ixcompile_ground(Args, Body, Pairs),
	(   foreach(V-Ix1,Pairs),
	    foreach(Part,Parts),
	    param(Args)
	do  ixadapt(Ix1, Ix2, Args),
	    compile(V in Ix2, 1, Part, Args)
	),
	append(Parts, Codes).

indexical('$VAR'(Pruned), R, Type, Linkage, LinkageLength, Pruned, Length, L0, L) -->
	{   integer(Pruned) -> true
        ;
            % xref handle_compile_exception/3    
            throw(idmember('$VAR'(Pruned)))
        },
						% ground(Mon) if used for tell
						% ground(Amon) if used for ask
	{decode_pragma(R, R1, P)},
	domain_top_level(R1, Op, [], state(Linkage1,0,Length0,Mon,Amon), L0, L),
	emit(ret(Type,Op,P), Unit),
	{   foreach(X,Mon),
	    foreach(X-8,Mon1)
	do  true
	},
	{   foreach(Y,Amon),
	    foreach(Y-16,Amon1)
	do  true
	},
	{merge(Mon1, Linkage1, Linkage2),
	 merge(Amon1, Linkage2, Linkage3),
	 merge_for_ask(Type, R1, Pruned, Linkage3, Linkage),
	 length(Linkage, LinkageLength),
	 Length is Length0+Unit}.

decode_pragma(!(R), R, 1) :- !.
decode_pragma(R, R, 0).

domain_top_level(R, 7, Qs, State, L0, L) -->
	{simple(R)}, !,
	domain(R, Qs, State, L0, L).
domain_top_level('$VAR'(X), 7, Qs, State, L0, L) --> !,
	domain('$VAR'(X), Qs, State, L0, L).
domain_top_level(R0, 6, Qs, State, L0, L) -->
	{unit_term(R0, R)}, !,
	{state_copy(State, State1, MonAmon, MonAmon, S, G)},
	term(R, Qs, State1, L0, L),
	{ord_union(S, G, MonAmon)}.
domain_top_level(Expr, 7, Qs, State, L0, L) -->
	{set_expression(Expr, _)}, !,
	domain(Expr, Qs, State, L0, L).
domain_top_level({R}, 7, Qs, State, L0, L) --> !,
	domain({R}, Qs, State, L0, L).
domain_top_level(Min..Max, 0, Qs, State, L0, L) -->
	{Min==inf, Max==sup}, !,
	void(Qs, State, L0, L).
domain_top_level(Min..Max, 1, Qs, State, L0, L) -->
	{Min==inf}, !,
	term(Max, Qs, State, L0, L).
domain_top_level(Min..Max, 2, Qs, State, L0, L) -->
	{Max==sup}, !,
	{state_copy(State, State1, G, S, S, G)},
	term(Min, Qs, State1, L0, L).
domain_top_level(Min..Max, 3, Qs, State, L0, L) --> !,
	{State = state(Linkage,CodeOffset,Length,Mon,Amon)},
	term(Min, Qs, state(Linkage1,CodeOffset,Length1,S1,G1), L0, L1),
	{CodeOffset1 is CodeOffset + Length1},
	term(Max, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	{merge(Linkage1, Linkage2, Linkage), 
	 ord_union(G1, S2, Mon),
	 ord_union(S1, G2, Amon),
	 Length is Length1 + Length2}.
domain_top_level(\R0, 4, Qs, State, L0, L) -->
	{unit_term(R0, R)}, !,
	{state_copy(State, State1, MonAmon, MonAmon, S, G)},
	term(R, Qs, State1, L0, L),
	{ord_union(S, G, MonAmon)}.
domain_top_level(\R, 5, Qs, State, L0, L) --> !,
	{state_copy(State, State1, G, S, S, G)},
	domain(R, Qs, State1, L0, L).
domain_top_level(R, 7, Qs, State, L0, L) -->
	domain(R, Qs, State, L0, L).

unit_term({R}, R) :- 
	(   var(R) -> true
	;   R='$VAR'(_) -> true
        ;   R=(_,_) -> fail
        ;   true
        ).

void(_, state([],_,0,[],[]), L, L) --> [].

state_copy(state(Linkage,CodeOffset,Length,A,B),
	   state(Linkage,CodeOffset,Length,C,D), A, B, C, D).
	

term_or_domain(R0, Qs, State, L0, L, term) -->
	{unit_term(R0, R)}, !,
	{State = state(Linkage,CodeOffset,Length,MonAmon,MonAmon)},
	term(R, Qs, state(Linkage,CodeOffset,Length,S,G), L0, L),
	{ord_union(S, G, MonAmon)}.
term_or_domain(R, Qs, State, L0, L, domain) -->
	domain(R, Qs, State, L0, L).

is_domain(R) :- var(R), !, fail.
is_domain(dom(_)).
is_domain({_}).
is_domain(_.._).
is_domain(\_).
is_domain(_/\_).
is_domain(_\/_).
is_domain(_?_).
is_domain(R1+R2) :- is_domain(R1); is_domain(R2).
is_domain(R1-R2) :- is_domain(R1); is_domain(R2).
is_domain(-R2) :- is_domain(R2).
is_domain(R1 mod R2) :- is_domain(R1); is_domain(R2).
is_domain(R1 rem R2) :- is_domain(R1); is_domain(R2).
is_domain(unionof(_,_,_)).
is_domain(switch(_,_)).

domain(Dom, _, _, _, _) -->
	{Dom \== {}},
	{simple(Dom)}, !,
        % xref handle_compile_exception/3    
	{throw(domain(Dom))}.
domain('$VAR'(X), _, _, _, _) --> !,
        % xref handle_compile_exception/3    
	{throw(domain('$VAR'(X)))}.
domain(D, Qs, State, L0, L) -->
	{unit_term(D, T)}, !,
	{State = state(Linkage,CodeOffset,Length,MonAmon,MonAmon)},
	term(T, Qs, state(Linkage,CodeOffset,Length1,S,G), L0, L),
	emit(dup_range, Unit),
	{ord_union(S, G, MonAmon),
	 Length is Length1 + Unit}.
domain(D, _, State, L0, L) -->
	{set_expression(D, Set)}, !,
	{State = state([],_,Unit,[],[])},
	{lookup_literal(L0, d(Set), 0, X, L)},
	emit(const_dom(X), Unit).
domain({D1,D2}, Qs, State, L0, L) --> !,
	domain({D1}\/{D2}, Qs, State, L0, L).
domain(set(X), _, State, L, L) --> !,
	{X='$VAR'(No)},
        {State = state([No-1],_CodeOffset,Unit,[],[])},
	emit(set(No), Unit).
domain(dom(X), Qs, State, L0, L) --> !,
	(   {nonvar(X), X='$VAR'(No)}
        ->  {State = state([No-1],_CodeOffset,Unit,[],[No])},
            {L = L0},
	    emit(dom(No), Unit)
	;   {var(X)}
	->  {State = state([],_,Unit,[],[])},
            {L = L0},
	    {var_nth(X, Qs, 0, No)},
            emit(qval(No), Unit1),
	    emit(dup_range, Unit2),
	    {Unit is Unit1+Unit2}
	;   domain({X}, Qs, State, L0, L)
	).
domain(Min .. Max, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	range(Min, Max, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L).
domain(D1 /\ D2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{nonvar(D2), D2 = \ND2}, !,
	term_or_domain(D1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1, Op1), 
	{CodeOffset1 is CodeOffset + Length1},
	term_or_domain(ND2, Qs, state(Linkage2,CodeOffset1,Length2,Amon2,Mon2), L1, L, Op2),
	emit(subtract(Op1,Op2), Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(Mon1, Mon2, Mon),
	  ord_union(Amon1, Amon2, Amon),
	  Length is Length1 + Length2 + Unit}.
domain(D1 /\ D2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	term_or_domain(D1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1, Op1), 
	{CodeOffset1 is CodeOffset + Length1},
	term_or_domain(D2, Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L, Op2),
	emit(inter(Op1,Op2), Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(Mon1, Mon2, Mon),
	  ord_union(Amon1, Amon2, Amon),
	  Length is Length1 + Length2 + Unit}.
domain(D1 ? Univ \/ D2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{Univ == (inf..sup)}, !,
	domain(D1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1),
	emit(check_union(Offset2), Unit),
	{CodeOffset1 is CodeOffset + Length1 + Unit},
	domain(D2, Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L),
	{Offset2 is CodeOffset1+Length2},
	{merge(Linkage1,Linkage2,Linkage),
	 ord_union(Mon1,Mon2,Mon),
	 ord_union(Amon1,Amon2,Amon),
	 Length is Length1+Length2+Unit}.
domain(D1 \/ D2,Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	term_or_domain(D1,Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1, Op1), 
	{CodeOffset1 is CodeOffset + Length1},
	term_or_domain(D2,Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L, Op2),
	emit(union(Op1,Op2), Unit),
	{merge(Linkage1, Linkage2, Linkage), 
	 ord_union(Mon1, Mon2, Mon),
	 ord_union(Amon1, Amon2, Amon),
	 Length is Length1 + Length2 + Unit}.
domain(\D, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	term_or_domain(D, Qs, state(Linkage,CodeOffset,Length1,Amon,Mon), L0, L, Op),
	emit(compl(Op), Unit),
	{Length is Length1 + Unit}.
/*
domain(R1 + R2, Qs, State, L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(unionof(B,R2,R1+B), Qs, State, L0, L).
*/
domain(R1 + R2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(R1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	domain(R2, Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L),
	emit(setplus, Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(Mon1,Mon2,Mon),
	  ord_union(Amon1,Amon2,Amon),
	Length is Length1 + Length2 + Unit}.
domain(R + T, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	domain(R, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(setadd, Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(S2,G2,Tmp),
	  ord_union(Mon1,Tmp,Mon),
	  ord_union(Amon1,Tmp,Amon),
	Length is Length1 + Length2 + Unit}.
domain(- R, Qs, State, L0, L) --> !,
	domain(0 - R, Qs, State, L0, L).
domain(T - R, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{is_term(T), is_domain(R)}, !,
	domain(R, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(setneg, Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(S2,G2,Tmp),
	  ord_union(Mon1,Tmp,Mon),
	  ord_union(Amon1,Tmp,Amon),
	Length is Length1 + Length2 + Unit}.
/*
domain(R1 - R2, Qs, State, L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(unionof(B,R2,R1-B), Qs, State, L0, L).
*/
domain(R1 - R2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(R1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	domain(R2, Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L),
	emit(setminus, Unit),
	 {merge(Linkage1, Linkage2, Linkage),
	  ord_union(Mon1,Mon2,Mon),
	  ord_union(Amon1,Amon2,Amon),
	Length is Length1 + Length2 + Unit}.
domain(R - T, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	domain(R, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(setsub, Unit),
	{merge(Linkage1, Linkage2, Linkage), 
	 ord_union(S2,G2,Tmp),
	 ord_union(Mon1,Tmp,Mon),
	 ord_union(Amon1,Tmp,Amon),
	 Length is Length1 + Length2 + Unit}.
domain(R1 mod R2, Qs, State, L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(unionof(B,R2,R1 mod B), Qs, State, L0, L).
domain(R1 rem R2, Qs, State, L0, L) -->
	{is_domain(R1), is_domain(R2)}, !,
	domain(unionof(B,R2,R1 rem B), Qs, State, L0, L).
domain(D mod T, Qs, state(Linkage,CodeOffset,Length,S,G), L0, L) --> !,
	domain(D, Qs, state(Linkage1,CodeOffset,Length1,S1,G1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(setmod, Unit),
	{merge(Linkage1, Linkage2, Linkage), 
	ord_union(S2,G2,Tmp),
	ord_union(S1,Tmp,S),
	ord_union(G1,Tmp,G),
	Length is Length1 + Length2 + Unit}.
domain(D rem T, Qs, state(Linkage,CodeOffset,Length,S,G), L0, L) --> !,
	domain(D, Qs, state(Linkage1,CodeOffset,Length1,S1,G1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(setrem, Unit),
	{merge(Linkage1, Linkage2, Linkage), 
	ord_union(S2,G2,Tmp),
	ord_union(S1,Tmp,S),
	ord_union(G1,Tmp,G),
	Length is Length1 + Length2 + Unit}.
domain(D1 ? D2, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	domain(D1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1),
	emit(check(Offset2), Unit),
	{CodeOffset1 is CodeOffset + Length1 + Unit},
	domain(D2, Qs, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L),
	{Offset2 is CodeOffset1+Length2},
	{merge(Linkage1,Linkage2,Linkage),
	 ord_union(Mon1,Mon2,Mon),
	 ord_union(Amon1,Amon2,Amon),
	 Length is Length1+Length2+Unit}.
domain(unionof(B,D1,D2), Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	domain(D1, Qs, state(Linkage1,CodeOffset,Length1,Mon1,Amon1), L0, L1),
	emit(unionof(CodeOffset2), Unit1),
	{CodeOffset1 is CodeOffset + Length1 + Unit1},
	{length(Qs, BIndex), append(Qs, [B], Qs1)},
	domain(D2, Qs1, state(Linkage2,CodeOffset1,Length2,Mon2,Amon2), L1, L),
	emit(unionof_next(BIndex,CodeOffset1), Unit2),
	{CodeOffset2 is CodeOffset1 + Length2 + Unit2},
	{merge(Linkage1,Linkage2,Linkage),
	 ord_union(Mon1,Mon2,Mon3),
	 ord_union(Amon1,Amon2,Amon3),
	 ord_del_element(Mon3, B, Mon),
	 ord_del_element(Amon3, B, Amon),
	 Length is Length1+Length2+Unit1+Unit2}.
domain(switch(T,Keylist), Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) --> !,
	term(T, Qs, state(Linkage0,CodeOffset,Length0,S0,G0), L0, L1),
	emit(switch(Index,CodeOffset2), Unit),
	{CodeOffset1 is CodeOffset+Length0+Unit},
	switch_table(Keylist, Qs, state(Linkage1,CodeOffset1,Length1,S1,G1), Table, L1, L2),
	{Length is Length0+Unit+Length1, CodeOffset2 is CodeOffset+Length},
	{merge(Linkage0,Linkage1,Linkage),
	 ord_union(S0,S1,Mon),
	 ord_union(G0,G1,Amon),
	 lookup_literal(L2, h(Table), 0, Index, L)}.
domain(Dom, _, _, _, _) -->
        % xref handle_compile_exception/3    
	{throw(domain(Dom))}.

range(Min, Max, _, state([],_,Unit,[],[]), L, L) -->
	{Min==inf, Max==sup}, !,
	emit(range(open,open), Unit).
range(Min, Max, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{Min==inf}, !,
	term(Max, Qs, state(Linkage,CodeOffset,Length2,Mon,Amon), L0, L),
	emit(range(open,closed), Unit),
	{Length is Length2 + Unit}.
range(Min, Max, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	{Max==sup}, !,
	term(Min, Qs, state(Linkage,CodeOffset,Length1,Amon,Mon), L0, L),
	emit(range(closed,open), Unit),
	{Length is Length1 + Unit}.
range(Min, Max, Qs, state(Linkage,CodeOffset,Length,Mon,Amon), L0, L) -->
	term(Min, Qs, state(Linkage1,CodeOffset,Length1,S1,G1), L0, L1),
	{CodeOffset1 is CodeOffset + Length1},
	term(Max, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(range(closed,closed), Unit),
	{merge(Linkage1, Linkage2, Linkage), 
	 ord_union(G1, S2, Mon),
	 ord_union(S1, G2, Amon),
	 Length is Length1 + Length2 + Unit}.


is_term(N) :- simple(N), !.
is_term('$VAR'(_)).
is_term(min(_)).
is_term(max(_)).
is_term(card(_)).
is_term(T1+T2) :- is_term(T1), is_term(T2).
is_term(T1-T2) :- is_term(T1), is_term(T2).
is_term(-T) :- is_term(T).
is_term(_*_).
is_term(_/>_).
is_term(_/<_).
is_term(T1 mod T2) :- is_term(T1), is_term(T2).
is_term(T1 rem T2) :- is_term(T1), is_term(T2).

term(N, Qs, state([],_,Unit,[],[]), L, L) --> 
	{var(N)}, !,
	{var_nth(N, Qs, 0, No)},
	emit(qval(No), Unit).
term(N, _, state([],_,Unit,[],[]), L0, L) -->
	{simple(N)}, !,
	{lookup_literal(L0, N, 0, X, L)},
	emit(const(X), Unit).
term('$VAR'(No), _, state([],_,Unit,[No],[No]), L, L) --> !,
	emit(val(No), Unit).
term(min(X), Qs, State, L0, L) --> !,
	(   {nonvar(X), X='$VAR'(No)}
        ->  {State = state([No-2],_,Unit,[No],[])},
            {L = L0},
            emit(min(No), Unit)
	;   {simple(X)},
	    term(X, Qs, State, L0, L)
        ).
term(max(X), Qs, State, L0, L) --> !,
	(   {nonvar(X), X='$VAR'(No)}
        ->  {State = state([No-4], _,Unit,[],[No])},
            {L = L0},
            emit(max(No), Unit)
	;   {simple(X)},
	    term(X, Qs, State, L0, L)
        ).
term(card(X), Qs, State, L0, L) --> !,
	(   {nonvar(X), X='$VAR'(No)}
        ->  {State = state([No-1], _,Unit,[],[No])},
            {L = L0},
	    emit(card(No), Unit)
	;   {simple(X)},
	    term(1, Qs, State, L0, L)
	).
term(T1+T2, Qs, Tuple, L0, L) --> !,
	binary_term(add, sym, T1, T2, Qs, Tuple, L0, L).
term(T1-T2, Qs, Tuple, L0, L) --> !,
	binary_term(sub, asym, T1, T2, Qs, Tuple, L0, L).
term(-T2, Qs, Tuple, L0, L) --> !,
	binary_term(sub, asym, 0, T2, Qs, Tuple, L0, L).
term(T1*T2, Qs, Tuple, L0, L) --> !,
	binary_term_val(mult, sym, T1, T2, Qs, Tuple, L0, L).
term(T1/>T2, Qs, Tuple, L0, L) --> !,
	binary_term_val(divu, asym, T1, T2, Qs, Tuple, L0, L).
term(T1/<T2, Qs, Tuple, L0, L) --> !,
	binary_term_val(divd, asym, T1, T2, Qs, Tuple, L0, L).
term(T1 mod T2, Qs, Tuple, L0, L) --> !,
	binary_term(mod, amon, T1, T2, Qs, Tuple, L0, L).
term(T1 rem T2, Qs, Tuple, L0, L) --> !,
	binary_term(rem, amon, T1, T2, Qs, Tuple, L0, L).
term(Term, _, _, _, _) -->
        % xref handle_compile_exception/3    
	{throw(term(Term))}.

binary_term(Op, Mon, T1, T2, Qs, state(Linkage,CodeOffset,Length,S,G), L0, L) --> !,
	term(T1, Qs, state(Linkage1,CodeOffset,Length1,S1,G1), L0, L1), 
	{CodeOffset1 is CodeOffset + Length1},
	term(T2, Qs, state(Linkage2,CodeOffset1,Length2,S2,G2), L1, L),
	emit(Op, Unit),
	{merge(Linkage1, Linkage2, Linkage),
	 binary_monotonicity(Mon, S1, G1, S2, G2, S, G),
	 Length is Length1 + Length2 + Unit}.

binary_term_val(Op, Mon, T1, T2, Qs, state(Linkage,CodeOffset,Length,S,G), L0, L) --> !,
	term(T1, Qs, state(Linkage,CodeOffset,Length1,S1,G1), L0, L1), 
	(   {integer(T2), T2>=0}
	->  {SG2 = []},
	    {lookup_literal(L1, T2, 0, X, L)},
	    emit_imm(Op, X, Unit)
	;   {nonvar(T2), T2 = '$VAR'(No)}
	->  {SG2 = [No], L = L1},
	    emit_val(Op, No, Unit)
	;   {var(T2)}
	->  {SG2 = [], L = L1},
	    {var_nth(T2, Qs, 0, No)},
	    emit_qval(Op, No, Unit)
	),
	{binary_monotonicity(Mon, S1, G1, SG2, SG2, S, G),
	 Length is Length1 + Unit}.

binary_monotonicity(sym, S1, G1, S2, G2, S, G) :-
	ord_union(S1, S2, S),
	ord_union(G1, G2, G).
binary_monotonicity(asym, S1, G1, S2, G2, S, G) :-
	ord_union(S1, G2, S),
	ord_union(G1, S2, G).
binary_monotonicity(amon, S1, G1, S2, G2, SG, SG) :-
	ord_union([S1,G1,S2,G2], SG).

switch_table([], Qs, State, [], L0, L) -->
	void(Qs, State, L0, L).
switch_table([Con-D|Keylist], Qs, State, [Con-CodeOffset|Table], L0, L) -->
	{State = state(Linkage,CodeOffset,Length,S,G),
	 State1 = state(Linkage1,CodeOffset,Length1,S1,G1)},
	domain(D, Qs, State1, L0, L1),
	emit(popj, Unit),
	{CodeOffset1 is CodeOffset+Length1+Unit},
	{State2 = state(Linkage2,CodeOffset1,Length2,S2,G2)},
	switch_table(Keylist, Qs, State2, Table, L1, L),
	{merge(Linkage1, Linkage2, Linkage),
	 ord_union(S1, S2, S),
	 ord_union(G1, G2, G),
	 Length is Length1+Length2+Unit}.



% xref fd_insn.h for now!  Operands:
% a(I) - argreg offset
% l(I) - code offset, becomes immediate label
% t(I) - literal offset, becomes immediate tagged
% h(I) - literal offset, becomes immediate hash table
%% [MC] 3.8.6: made determinate
emit(dup_range, 1) --> [0].
emit(range(open,open), 1) --> !, [1].
emit(range(open,closed), 1) --> !, [2].
emit(range(closed,open), 1) --> !, [3].
emit(range(closed,closed), 1) --> [4].
emit(setadd, 1) --> [5].
emit(setsub, 1) --> [6].
emit(setmod, 1) --> [7].
emit(setrem, 1) --> [86].	% 4.0.3
emit(setneg, 1) --> [73].
emit(setplus, 1) --> [74].
emit(setminus, 1) --> [75].
emit(compl(term), 1) --> !, [8].
emit(compl(domain), 1) --> [9].
emit(union(term,term), 1) --> !, [10].
emit(union(term,domain), 1) --> !, [11].
emit(union(domain,term), 1) --> !, [12].
emit(union(domain,domain), 1) --> [13].
emit(inter(term,term), 1) --> !, [14].
emit(inter(term,domain), 1) --> !, [15].
emit(inter(domain,term), 1) --> !, [16].
emit(inter(domain,domain), 1) --> [17].
emit(subtract(term,term), 1) --> !, [76].
emit(subtract(term,domain), 1) --> !, [77].
emit(subtract(domain,term), 1) --> !, [78].
emit(subtract(domain,domain), 1) --> [79].
emit(qval(X), 2) --> [18,a(X)].
emit(add, 1) --> [19].
emit(sub, 1) --> [20].
% 21, 22, 23 -- see below
emit(mod, 1) --> [24].
emit(rem, 1) --> [85].		% 4.0.3
emit(val(0), 1) --> !, [26].
emit(val(1), 1) --> !, [27].
emit(val(2), 1) --> !, [28].
emit(val(X), 2) --> [25,a(X)].
emit(dom(0), 1) --> !, [30].
emit(dom(1), 1) --> !, [31].
emit(dom(2), 1) --> !, [32].
emit(dom(X), 2) --> [29,a(X)].
emit(set(1), 1) --> [71].	% reused
emit(min(0), 1) --> !, [34].
emit(min(1), 1) --> !, [35].
emit(min(2), 1) --> !, [36].
emit(min(X), 2) --> [33,a(X)].
emit(max(0), 1) --> !, [38].
emit(max(1), 1) --> !, [39].
emit(max(2), 1) --> !, [40].
emit(max(X), 2) --> [37,a(X)].
emit(const(C), 2) --> [41,t(C)].
emit(const_dom(C), 2) --> [41,d(C)].
% 42, 43, 44 -- see below
emit(ret(Type,Op,P), 2) --> {Type<2}, !, [B,a(P)], {B is 45+Op+8*(Type>>1)}.
emit(ret(Type,Op,_), 1) --> [B], {B is 45+Op+8*(Type>>1)}.
emit(check_union(Op), 2) --> [61,l(Op)].
emit(check(Op), 2) --> [62,l(Op)].
emit(unionof(Op), 2) --> [63,l(Op)].
emit(unionof_next(Bi,Op), 3) --> [64,a(Bi),l(Op)].
emit(card(0), 1) --> !, [66].
emit(card(1), 1) --> !, [67].
emit(card(2), 1) --> !, [68].
emit(card(X), 2) --> [65,a(X)].
emit(switch(Index,Join), 3) --> [69,h(Index),l(Join)].
emit(popj, 1) --> [70].
emit(error, 1) --> [72].

emit_imm(mult, I, 2) --> [21,t(I)].
emit_imm(divd, I, 2) --> [22,t(I)].
emit_imm(divu, I, 2) --> [23,t(I)].

emit_val(mult, X, 2) --> [42,a(X)].
emit_val(divd, X, 2) --> [43,a(X)].
emit_val(divu, X, 2) --> [44,a(X)].

emit_qval(mult, X, 2) --> [80,a(X)].
emit_qval(divd, X, 2) --> [81,a(X)].
emit_qval(divu, X, 2) --> [82,a(X)].

peep_code(Stretch, _, Jumbo, Len) :-
	term_hash(Stretch, Hash),
	jumbo_instruction_hash(Hash, Stretch, Jumbo), !,
	length(Jumbo, Len).
peep_code(Code1, CodeLength1, Code, CodeLength) :-
	append(Pre, [74,52,Arg], Code1), !, % SETPLUS,PRUNE -> PRUNE_PLUS
	append(Pre, [83,Arg], Code),
	CodeLength is CodeLength1-1.
peep_code(Code1, CodeLength1, Code, CodeLength) :-
	append(Pre, [75,52,Arg], Code1), !, % SETMINUS,PRUNE -> PRUNE_MINUS
	append(Pre, [84,Arg], Code),
	CodeLength is CodeLength1-1.
peep_code(Code, CodeLength, Code, CodeLength).

% 'ax=t'(A,X,T) +:
% 	X in   min(T) /> A..max(T) /< A,
% 	T in !(min(X) *  A..max(X) *  A).
jumbo_instruction([36,44,a(0),40,43,a(0),48,a(0)], [88]).
jumbo_instruction([35,42,a(0),39,42,a(0),48,a(1)], [89]).
% 'x+y=t'(X,Y,T) +:
% 	X in !(min(T) - max(Y)..max(T) - min(Y)),
% 	Y in !(min(T) - max(X)..max(T) - min(X)),
% 	T in !(min(X) + min(Y)..max(X) + max(Y)).
jumbo_instruction([36,39,20,40,35,20,48,a(1)], [90,a(2),a(1),a(0)]).
jumbo_instruction([36,38,20,40,34,20,48,a(1)], [90,a(2),a(0),a(0)]).
jumbo_instruction([34,35,19,38,39,19,48,a(1)], [90,a(0),a(1),a(1)]).
% 't+u=c'(T,U,C) +:
% 	T in !(C - dom(U)),
% 	U in !(C - dom(T)).
jumbo_instruction([31,28,73,52,a(1)], [91,a(1)]).
jumbo_instruction([30,28,73,52,a(1)], [91,a(0)]).
% 't=u+c'(T,U,C) +:
% 	T in !(dom(U) + C),
% 	U in !(dom(T) - C).
jumbo_instruction([31,28,5,52,a(1)], [92,a(1),a(1)]).
jumbo_instruction([30,28,6,52,a(1)], [92,a(0),a(0)]).
% 't=<u+c'(T,U,C) +:
% 	T in inf..max(U)+C,
% 	U in min(T) - C..sup.
jumbo_instruction([39,28,19,46,a(0)], [93,a(1),a(1)]).
jumbo_instruction([34,28,20,47,a(0)], [93,a(0),a(0)]).
% 't\\=u+c'(T,U,C) +:
% 	T in \{U + C},
% 	U in \{T - C}.
jumbo_instruction([27,28,19,49,a(0)], [94,a(1),a(1)]).
jumbo_instruction([26,28,20,49,a(0)], [94,a(0),a(0)]).
% 't>=u+c'(T,U,C) +:
% 	T in min(U) + C..sup,
% 	U in inf..max(T) - C.
jumbo_instruction([35,28,19,47,a(0)], [95,a(1),a(1)]).
jumbo_instruction([38,28,20,46,a(0)], [95,a(0),a(0)]).
% 'ax+y=t'(A,X,Y,Z) +:
% 	X in  (min(Z) - max(Y)) /> A..(max(Z) - min(Y)) /< A,
% 	Y in !(min(Z) - max(X)*A    .. max(Z) - min(X)*A),
% 	Z in !(min(X)*A + min(Y)    .. max(X)*A + max(Y)).
jumbo_instruction([33,a(3),40,20,44,a(0),37,a(3),36,20,43,a(0),48,a(0)], [96]).
jumbo_instruction([33,a(3),39,42,a(0),20,37,a(3),35,42,a(0),20,48,a(1)], [97]).
jumbo_instruction([35,42,a(0),36,19,39,42,a(0),40,19,48,a(1)], [98]).
% 't+u=<c'(T,U,C) +:
% 	T in inf..C - min(U),
% 	U in inf..C - min(T).
jumbo_instruction([28,35,20,46,a(0)], [99,a(1)]).
jumbo_instruction([28,34,20,46,a(0)], [99,a(0)]).
% 't+u\\=c'(T,U,C) +:
% 	T in \{C - U},
% 	U in \{C - T}.
jumbo_instruction([28,27,20,49,a(0)], [100,a(1)]).
jumbo_instruction([28,26,20,49,a(0)], [100,a(0)]).
% 't+u>=c'(T,U,C) +:
% 	T in C - max(U)..sup,
% 	U in C - max(T)..sup.
jumbo_instruction([28,39,20,47,a(0)], [101,a(1)]).
jumbo_instruction([28,38,20,47,a(0)], [101,a(0)]).
% 'x+y=u+c'(X,Y,U,C) +:
% 	X in !(min(U) - max(Y) + C..max(U) - min(Y) + C),
% 	Y in !(min(U) - max(X) + C..max(U) - min(X) + C),
% 	U in !(min(X) + min(Y) - C..max(X) + max(Y) - C).
jumbo_instruction([36,39,20,25,a(3),19,40,35,20,25,a(3),19,48,a(1)], [102,a(2),a(1),a(0),a(3),a(1)]).
jumbo_instruction([36,38,20,25,a(3),19,40,34,20,25,a(3),19,48,a(1)], [102,a(2),a(0),a(0),a(3),a(1)]).
jumbo_instruction([34,35,19,25,a(3),20,38,39,19,25,a(3),20,48,a(1)], [102,a(0),a(1),a(1),a(3),a(0)]).
% 'x+y+c=z'(X,Y,C,Z) +:
% 	X in !(min(Z) - max(Y) - C..max(Z) - min(Y) - C),
% 	Y in !(min(Z) - max(X) - C..max(Z) - min(X) - C),
% 	Z in !(min(X) + min(Y) + C..max(X) + max(Y) + C).
jumbo_instruction([33,a(3),39,20,28,20,37,a(3),35,20,28,20,48,a(1)], [102,a(3),a(1),a(0),a(2),a(0)]).
jumbo_instruction([33,a(3),38,20,28,20,37,a(3),34,20,28,20,48,a(1)], [102,a(3),a(0),a(0),a(2),a(0)]).
jumbo_instruction([34,35,19,28,19,38,39,19,28,19,48,a(1)], [102,a(0),a(1),a(1),a(2),a(1)]).
% 'x+y+z=c'(X,Y,Z,C) +:
% 	X in !(C - max(Y) - max(Z)..C - min(Y) - min(Z)),
% 	Y in !(C - max(X) - max(Z)..C - min(X) - min(Z)),
% 	Z in !(C - max(X) - max(Y)..C - min(X) - min(Y)).
jumbo_instruction([25,a(3),39,20,40,20,25,a(3),35,20,36,20,48,a(1)], [103,a(1),a(2)]).
jumbo_instruction([25,a(3),38,20,40,20,25,a(3),34,20,36,20,48,a(1)], [103,a(0),a(2)]).
jumbo_instruction([25,a(3),38,20,39,20,25,a(3),34,20,35,20,48,a(1)], [103,a(0),a(1)]).
% 'oneof(x,y)=z IND'(X, Y, Z) +:
% 	X in !(((dom(Y)/\dom(Z)) ? (inf..sup)) \/ dom(Z)),
% 	Y in !(((dom(X)/\dom(Z)) ? (inf..sup)) \/ dom(Z)),
% 	Z in !((dom(X)\/dom(Y))).
jumbo_instruction([31,32,17,61,l(6),32,52,a(1)], [104,a(1)]).
jumbo_instruction([30,32,17,61,l(6),32,52,a(1)], [104,a(0)]).
jumbo_instruction([30,31,13,52,a(1)], [105]).
% '|x|=y 1'(X,Y) +:
% 	X in !(dom(Y) \/ (0-dom(Y))),
% 	Y in !(dom(X) \/ (0-dom(X))).
jumbo_instruction([31,31,41,t(0),73,13,52,a(1)], [106,a(1)]).
jumbo_instruction([30,30,41,t(0),73,13,52,a(1)], [106,a(0)]).

:- dynamic jumbo_instruction_hash/3.

hash_jumbo_instructions :-
	jumbo_instruction(Stretch, Jumbo),
	term_hash(Stretch, Hash),
	assertz(jumbo_instruction_hash(Hash,Stretch,Jumbo)),
	fail.
hash_jumbo_instructions.

lookup_literal([], Lit, I, I, [Lit]).
lookup_literal([X|L1], Lit, I, K, [X|L2]) :-
	(   X==Lit
	->  K = I, L2 = L1
	;   J is I+1,
	    lookup_literal(L1, Lit, J, K, L2)
	).

var_nth(X, [Y|_], M, N) :- X == Y, !, N = M.
var_nth(X, [_|Ys], I, N) :-
	J is I+1,
	var_nth(X, Ys, J, N).

merge_for_ask(Type, R, Pruned, S, [Pruned-Ix|S]) :-
	Type /\ 2 =:= 2, !,			% for ask
	merge_for_ask(R, Ix).
merge_for_ask(_, _, _, S, S).

merge_for_ask(R, Ix) :-
	simple(R), !, Ix = 16.
merge_for_ask('$VAR'(_), Ix) :- !,
	Ix = 16.
merge_for_ask(A..B, Ix) :- !,
	(   A==inf -> Ix = 4
	;   B==sup -> Ix = 2
	;   Ix = 6
	).
merge_for_ask(_, 1).


merge([], L, L).
merge([X|Xs], X2, [E|X3]) :-
	merge2(X, X2, E, LeftOvers),
	merge(Xs, LeftOvers, X3).

%% [MC] 3.8.6: made determinate
merge2(V, [], V, []) :- !.
merge2(V1-N1, [V2-N2|R], V1-N3, R) :-
	V1 == V2, !,
	N3 is N1 \/ N2.
merge2(V1, [X|Xs], X2s, [X|LeftOvers]) :-
	merge2(V1, Xs, X2s, LeftOvers).


in_set_goal(X, Set, Goal) :-
	'$fd_size'(Set, _, 1), !,
	in_set_goal_1(X, Set, Goal).
in_set_goal(X, Set, clpfd:in_set_aux_rt(X,Set)).

in_set_goal_1(X, Set, Goal) :-
	fd_integer(X), !,
	(   '$fd_dom_contains'(Set, X) -> Goal = true
	;   Goal = false
	).
in_set_goal_1(X, [[Min|Max]], Goal) :- !,
	Goal = clpfd:propagate_interval_chk(X,Min,Max).
in_set_goal_1(X, Set, clpfd:prune_and_propagate_chk(X,Set)).


expand_arith(Rel, X, Y, M, Goal) :-
	trans_goal(X, Rel, Y, M, List, List1), !,
	declare_dvars(X, Y, List, List1, []),
	(   List = [Elt] -> single_goal(Elt, Goal)
	;   unfoldeq(List, List2),
	    commafy(List2, Goal)
	).
expand_arith(Rel, X, Y, _, Goal) :-
	Goal0 =.. [Rel,X,Y],
	ill_formed_constraint(Goal0, Goal).

% SPRM 13713: for X #= 0*Z, ensure that Z is "typed" as dvar
declare_dvars(X, Y, Exp) -->
	{term_variables_set(X-Y, Set1)},
	{term_variables_set(Exp, Set2)},
	{ord_subtract(Set1, Set2, Diff)},
	(   foreach(V,Diff)
	do  [Goal],
	    {fd_goal_expand_in(V, inf..sup, Goal)}
	).

ill_formed_constraint(Constraint, clpfd:illarg(domain(term,constraint), Constraint, 0)).

single_goal(T=C, Goal) :- fd_dvar(T), fd_integer(C), !,
	Goal = 't=c'(T,C).
single_goal(X=Y, Goal) :- !, Goal = 'x=y'(X,Y).
single_goal(Goal, Goal).

fd_dvar(Arg) :- var(Arg), !.
fd_dvar(Arg) :-
	integer(Arg),
	\+prolog:'$large_data'(0, Arg, _).

fd_integer(Arg) :-
	integer(Arg),
	\+prolog:'$large_data'(0, Arg, _).

set_expression(S, _) :- var(S), !, fail.
set_expression({}, []).
set_expression({S}, Mask) :-
	union_expression(S, Mask).
set_expression(A..B, Set) :-
	atomic(A),
	atomic(B),
	'$fd_range'(A, B, Set, 1).
set_expression(\D, Mask) :-
	set_expression(D, Mask1),
	'$fd_dom_complement'(Mask1, Mask).
set_expression(D1/\D2, Mask) :-
	set_expression(D1, Mask1),
	set_expression(D2, Mask2),
	'$fd_dom_intersection'(Mask1, Mask2, Mask).
set_expression(D1\/D2, Mask) :-
	set_expression(D1, Mask1),
	set_expression(D2, Mask2),
	'$fd_dom_union'(Mask1, Mask2, Mask).
  
% same as above, but raises an exception instead of failing
set_expression_check(X, _, Goal, ArgNo) :- var(X), !,
	illarg(var, Goal, ArgNo, X).
set_expression_check({}, [], _Goal, _ArgNo) :- !.
set_expression_check({S}, Set, _, _) :-
	union_expression(S, Set), !.
set_expression_check(A..B, Set, _, _) :-
	'$fd_range'(A, B, Set, 1), !.
set_expression_check(\D, Set, Goal, ArgNo) :- !,
	set_expression_check(D, Set1, Goal, ArgNo),
	'$fd_dom_complement'(Set1, Set).
set_expression_check(D1/\D2, Set, Goal, ArgNo) :- !,
	set_expression_check(D1, Set1, Goal, ArgNo),
	set_expression_check(D2, Set2, Goal, ArgNo),
	'$fd_dom_intersection'(Set1, Set2, Set).
set_expression_check(D1\/D2, Set, Goal, ArgNo) :- !,
	set_expression_check(D1, Set1, Goal, ArgNo),
	set_expression_check(D2, Set2, Goal, ArgNo),
	'$fd_dom_union'(Set1, Set2, Set).
set_expression_check(X, _Set, Goal, ArgNo) :-
	illarg(domain(term,set_expression), Goal, ArgNo, X).

must_be_fd_integer(Arg, _Constraint, _ArgNo) :-
	fd_integer(Arg), !.
must_be_fd_integer(Arg, Constraint, Argno) :-
	fd_argument_error(Constraint, Argno, Arg).


union_expression(S, _) :- var(S), !, fail.
union_expression(I, Set) :-
	integer(I), !,
	'$fd_range'(I, I, Set, 1).
union_expression((S1,S2), Set) :-
	union_expression(S1, Set1),
	union_expression(S2, Set2),
	'$fd_dom_union'(Set1, Set2, Set).

commafy([], true).
commafy([X|L], Conj) :- commafy(L, X, Conj).

commafy([], X, X).
commafy([Y|L], X, (X,Conj)) :- commafy(L, Y, Conj).

fd_expandable(E1 #= E2, E1, E2, #=).
fd_expandable(E1 #< E2, E1, E2-1, #=<).
fd_expandable(E1 #=< E2, E1, E2, #=<).
fd_expandable(E1 #\= E2, E1, E2, #\=).
fd_expandable(E1 #> E2, E2+1, E1, #=<).
fd_expandable(E1 #>= E2, E2, E1, #=<).

unfoldeq([], []).
unfoldeq([X=X|L1], L2) :- !,
	unfoldeq(L1, L2).
unfoldeq([X|L1], [X|L2]) :-
	unfoldeq(L1, L2).

trans_goal(E1, Rel, E2, M) -->
	{linearize_rel(E1, Rel, E2, M, Pieces, [])},
	(   foreach(Piece,Pieces)
	do  trans_piece(Piece)
	).

trans_piece(reified(M,Ctr,B)) -->
	trans_bool(Ctr #<=> B, M).
trans_piece(nonlinear(Ctr)) --> [Ctr].
trans_piece(linear(E1,Rel,E2)) -->
	{normalize(E1-E2, 1, Poly0, [])},
	{keysort(Poly0, Poly1)},
	{keyfuse(Poly1, Poly2)},
	{isolate(Poly2, A, X, As1, Xs1, Sum)},
	{   A =< 0 -> As2 = As1, Rel1 = Rel, Sum1 = Sum
	;   Sum1 is -Sum,
	    rel_inverse(Rel, Rel1),
	    (   foreach(P,As1),
		foreach(N,As2)
	    do  N is -P
	    )
	},
	trans_linear(As2, Xs1, Sum1, Rel1, X).

trans_linear([], [], S, Rel, X) --> !,
	{rel_inverse(Rel, Inv)},
	t_rel_c(Inv, X, S).
trans_linear([1], [Y], S, Rel, X) --> {integer(X)}, !,
	{HL is X-S},
	t_rel_c(Rel, Y, HL).
trans_linear([1], [Y], 0, Rel, X) --> !,
	t_rel_u(Rel, Y, X).
trans_linear([1], [Y], S, Rel, X) --> !,
	{S1 is -S},
	t_rel_u_c(Rel, Y, X, S1).
trans_linear([-1], [Y], S, Rel, X) --> !,
	{rel_inverse(Rel, Rel1)},
	t_u_rel_c(Rel1, X, Y, S).
trans_linear(As, Xs, S, Rel, X) -->
	(   {integer(X)}
	->  {S1 is X-S},
	    peep_linear(As, Xs, Rel, S1)
	;   {S1 is -S},
	    peep_linear([-1|As], [X|Xs], Rel, S1)
	).

peep_linear([-1|As], Xs, Rel, S) --> !,
	{   foreach(P,As),
	    foreach(N,As1)
	do  N is -P
	},
	{S1 is -S},
	{rel_inverse(Rel, Inv)},
	peep_linear([1|As1], Xs, Inv, S1).
peep_linear([A], [X], #=, S) --> !,
	(   {A =:= 0, S =:= 0} -> []
	;   {A =:= 0} -> [fail]
	;   {S mod A =:= 0} -> {Q is S//A}, [X = Q]
	;   [fail]
	).
peep_linear([1,1], [X,Y], Rel, S) --> !, t_u_rel_c(Rel,X,Y,S).
peep_linear([1,-1], [X,Y], Rel, 0) --> !, t_rel_u(Rel,X,Y).
peep_linear([1,-1], [X,Y], Rel, S) --> !, t_rel_u_c(Rel,X,Y,S).
peep_linear([1,1,1], [X,Y,Z], #=, S) --> !, ['x+y+z=c'(X,Y,Z,S)].
peep_linear([1,1,-1], [X,Y,Z], #=, 0) --> !, ['x+y=t'(X,Y,Z)].
peep_linear([1,1,-1], [X,Y,Z], #=, S) --> !, ['x+y=u+c'(X,Y,Z,S)].
peep_linear([1,-1,1], [X,Y,Z], #=, 0) --> !, ['x+y=t'(X,Z,Y)].
peep_linear([1,-1,1], [X,Y,Z], #=, S) --> !, ['x+y=u+c'(X,Z,Y,S)].
peep_linear([1,-1,-1], [X,Y,Z], #=, 0) --> !, ['x+y=t'(Y,Z,X)].
peep_linear([1,-1,-1], [X,Y,Z], #=, S) --> !, ['t=x+y+c'(X,Y,Z,S)].
peep_linear(As, Xs, Rel, S) -->
	peep_scalar_product(As, Xs, Rel, S).

peep_scalar_product(As, Xs, Rel, S) -->
	[scalar_product(As,Xs,Rel,S)].


isolate([], 0, 0, [], [], 0).
isolate([X- -1|Poly], -1, X, As, Xs, S) :- var(X), !,
	isolate(Poly, As, Xs, S).
isolate([X-1|Poly], 1, X, As, Xs, S) :- var(X), !,
	isolate(Poly, As, Xs, S).
isolate([N-S], 0, 0, [], [], S) :- N==1, !.
isolate([X-0|Poly], A1, X1, As1, Xs1, S) :- 
	fd_dvar(X), !,
	isolate(Poly, A1, X1, As1, Xs1, S).
isolate([X-A|Poly], A1, X1, [A|As1], [X|Xs1], S) :-
	isolate(Poly, A1, X1, As1, Xs1, S).


isolate([], [], [], 0).
isolate([N-S], [], [], S) :- N==1, !.
isolate([X-0|Poly], As1, Xs1, S) :-
	fd_dvar(X), !,
	isolate(Poly, As1, Xs1, S).
isolate([X-A|Poly], [A|As1], [X|Xs1], S) :-
	isolate(Poly, As1, Xs1, S).

linearize_eq(E1, E2, _) -->
	{fd_dvar(E1)}, !, {E2 = E1}.
linearize_eq(E1, E2, M) -->
	linearize_rel(E1, #=, E2, M).

linearize_rel(E1, Rel, E2, M) -->
	linearize(E1, M, 1, L1),
	linearize(E2, M, 1, L2),
	[linear(L1, Rel, L2)].

linearize(E,   _, I, J) --> {integer(E)}, !, {J is I*E}.
linearize(E,   _, I, I*E) --> {simple(E)}, !.
linearize(K*E, M, I, L) --> {integer(K)}, !,
	{J is I*K},
	linearize(E, M, J, L).
linearize(E*K, M, I, L) --> {integer(K)}, !,
	{J is I*K},
	linearize(E, M, J, L).
linearize(E1*E2, M, I, I*Y) -->
	{E1 == E2}, !,
	linearize_eq(E1, X, M),
	[nonlinear('x*x=y'(X,Y))].
linearize(E1*E2, M, I, I*Z) --> !,
	linearize_eq(E1, X, M),
	linearize_eq(E2, Y, M),
	[nonlinear('x*y=z'(X,Y,Z))].
linearize(E1//E2, M, I, L) --> !, % SPRM 13803
	linearize(E1/E2, M, I, L).
linearize(E1/E2, M, I, I*Z) --> !,
	linearize_eq(E1, X, M),
	linearize_eq(E2, Y, M),
	[nonlinear('x/y=z'(X,Y,Z))].
linearize(E1 div E2, M, I, I*Z) --> !,
	linearize_eq(E1, X, M),
	linearize_eq(E2, Y, M),
	[nonlinear('x div y=z'(X,Y,Z))].
linearize(E1 mod E2, M, I, I*Z) --> !,
	linearize_eq(E1, X, M),
	linearize_eq(E2, Y, M),
	[nonlinear('x mod y=z'(X,Y,Z))].
linearize(E1 rem E2, M, I, I*Z) --> !,
	linearize_eq(E1, X, M),
	linearize_eq(E2, Y, M),
	[nonlinear('x rem y=z'(X,Y,Z))].
linearize(min(E1,E2), M, I, I*Z) --> !,
	linearize_eq(E1, X, M),
	linearize_eq(E2, Y, M),
	[nonlinear('min(x,y)=z'(X,Y,Z))].
linearize(max(E1,E2), M, I, I*Z) --> !,
	linearize_eq(E1, X, M),
	linearize_eq(E2, Y, M),
	[nonlinear('max(x,y)=z'(X,Y,Z))].
linearize(abs(E1), M, I, I*Y) --> !,
	linearize_eq(E1, X, M),
	[nonlinear('|x|=y'(X,Y))].
linearize(E1+E2, M, I, L1+L2) --> !,
	linearize(E1, M, I, L1),
	linearize(E2, M, I, L2).
linearize(-E2, M, I, E3) --> !,
	linearize(0-E2, M, I, E3).
linearize(E1-E2, M, I, L1-L2) --> !,
	linearize(E1, M, I, L1),
	linearize(E2, M, I, L2).
linearize(M:Ctr, _, I, L) --> !,
	linearize(Ctr, M, I, L).
linearize(Ctr, M, I, I*B) -->
	[reified(M,Ctr,B)].

normalize(I, Sign) --> {I = '$VAR'(_)}, !, [I-Sign].
normalize(I, Sign) --> {atomic(I)}, !,
	[Item],
	{   integer(I) -> J is I*Sign, Item = 1-J
	;   Item = I-Sign
	}.
normalize(K*I, Sign) --> !, {integer(K), J is K*Sign}, [I-J].
normalize(E1+E2, Sign) --> !,
	normalize(E1, Sign),
	normalize(E2, Sign).
normalize(-E2, Sign) --> !,
	normalize(0-E2, Sign).
normalize(E1-E2, Sign) --> !,
	{Neg is -Sign},
	normalize(E1, Sign),
	normalize(E2, Neg).

keyfuse(KL1, KL3) :-
	keyclumped(KL1, KL2),
	(   foreach(K-L,KL2),
	    foreach(K-S,KL3)
	do  sumlist(L, S)
	).

t_rel_c(#=, T, Con) --> {integer(T)}, !,
	({T=:=Con} -> [] ; [fail]).
t_rel_c(#=, T, Con) -->
	[T=Con /*'t=c'(T,Con)*/].
t_rel_c(#=<, T, Con) -->
	({fd_dvar(T), fd_integer(Con)} -> ['t=<c'(T,Con)]; ['x=<y'(T,Con)]).
t_rel_c(#\=, T, Con) -->
	({fd_dvar(T), fd_integer(Con)} -> ['t\\=c'(T,Con)]; ['x\\=y'(T,Con)]).
t_rel_c(#>=, T, Con) -->
	({fd_dvar(T), fd_integer(Con)} -> ['t>=c'(T,Con)]; ['x=<y'(Con,T)]).

t_rel_u_c(#=, T, U, Con) --> ['t=u+c'(T,U,Con)].
t_rel_u_c(#=<, T, U, Con) --> ['t=<u+c'(T,U,Con)].
t_rel_u_c(#\=, T, U, Con) --> ['t\\=u+c'(T,U,Con)].
t_rel_u_c(#>=, T, U, Con) --> ['t>=u+c'(T,U,Con)].

t_rel_u(#=, T, U) --> [T=U /*'x=y'(T,U) bad for linear eq.*/].
t_rel_u(#=<, T, U) --> ['x=<y'(T,U)].
t_rel_u(#\=, T, U) --> ['x\\=y'(T,U)].
t_rel_u(#>=, T, U) --> ['x=<y'(U,T)].

t_u_rel_c(#=, T, U, Con) --> ['t+u=c'(T,U,Con)].
t_u_rel_c(#=<, T, U, Con) --> ['t+u=<c'(T,U,Con)].
t_u_rel_c(#\=, T, U, Con) --> ['t+u\\=c'(T,U,Con)].
t_u_rel_c(#>=, T, U, Con) --> ['t+u>=c'(T,U,Con)].

rel_inverse(#=,  #=).
rel_inverse(#=<, #>=).
rel_inverse(#\=, #\=).
rel_inverse(#>=, #=<).

fd_goal_expansion(X in Expr, _, Goal) :-
	fd_goal_expand_in(X, Expr, Goal).
fd_goal_expansion(X in_set Set, _, Goal) :-
	in_set_goal(X, Set, Goal).
fd_goal_expansion(X #= Y, M, clpfd:Goal) :-
	expand_arith(#=, X, Y, M, Goal).
fd_goal_expansion(X #< Y, M, clpfd:Goal) :-
	expand_arith(#=<, X, Y-1, M, Goal).
fd_goal_expansion(X #=< Y, M, clpfd:Goal) :-
	expand_arith(#=<, X, Y, M, Goal).
fd_goal_expansion(X #\= Y, M, clpfd:Goal) :-
	expand_arith(#\=, X, Y, M, Goal).
fd_goal_expansion(X #> Y, M, clpfd:Goal) :-
	expand_arith(#=<, Y+1, X, M, Goal).
fd_goal_expansion(X #>= Y, M, clpfd:Goal) :-
	expand_arith(#=<, Y, X, M, Goal).
fd_goal_expansion(Prop iff B, M, Expanded) :- % compatibility
	fd_goal_expansion(Prop #<=> B, M, Expanded).
fd_goal_expansion(#\ Y, M, Expanded) :-
	fd_goal_expand_prop(#\ Y, M, Expanded).
fd_goal_expansion(X #/\ Y, M, Expanded) :-
	fd_goal_expand_prop(X #/\ Y, M, Expanded).
fd_goal_expansion(X #\ Y, M, Expanded) :-
	fd_goal_expand_prop(X #\ Y, M, Expanded).
fd_goal_expansion(X #\/ Y, M, Expanded) :-
	fd_goal_expand_prop(X #\/ Y, M, Expanded).
fd_goal_expansion(X #=> Y, M, Expanded) :-
	fd_goal_expand_prop(X #=> Y, M, Expanded).
fd_goal_expansion(X #<= Y, M, Expanded) :-
	fd_goal_expand_prop(X #<= Y, M, Expanded).
fd_goal_expansion(X #<=> Y, M, Expanded) :-
	fd_goal_expand_prop(X #<=> Y, M, Expanded).

%% [PM] 4.1.3 inline goal expansion for SPIDER
:- if(current_prolog_flag(dialect, spider)).

%% [PM] 4.1.3 This is only seen by SPIDER (but would work in SICStus too)
goal_expansion(X in Expr, Lay, M, Goal, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X in Expr, M, Goal).
goal_expansion(X in_set Set, Lay, M, Goal, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X in_set Set, M, Goal).
goal_expansion(X #= Y, Lay, M, Goal, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #= Y, M, Goal).
goal_expansion(X #< Y, Lay, M, Goal, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #< Y, M, Goal).
goal_expansion(X #=< Y, Lay, M, Goal, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #=< Y, M, Goal).
goal_expansion(X #\= Y, Lay, M, Goal, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #\= Y, M, Goal).
goal_expansion(X #> Y, Lay, M, Goal, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #> Y, M, Goal).
goal_expansion(X #>= Y, Lay, M, Goal, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #>= Y, M, Goal).
goal_expansion(Prop iff B, Lay, M, Expanded, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(Prop iff B, M, Expanded).
goal_expansion(#\ Y, Lay, M, Expanded, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(#\ Y, M, Expanded).
goal_expansion(X #/\ Y, Lay, M, Expanded, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #/\ Y, M, Expanded).
goal_expansion(X #\ Y, Lay, M, Expanded, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #\ Y, M, Expanded).
goal_expansion(X #\/ Y, Lay, M, Expanded, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #\/ Y, M, Expanded).
goal_expansion(X #=> Y, Lay, M, Expanded, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #=> Y, M, Expanded).
goal_expansion(X #<= Y, Lay, M, Expanded, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #<= Y, M, Expanded).
goal_expansion(X #<=> Y, Lay, M, Expanded, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
        fd_goal_expansion(X #<=> Y, M, Expanded).

:- else. % !SPIDER

goal_expansion(Goal, Lay, Module, Expanded, LayExpanded) :-
	condense_layout(Lay, LayExpanded), % keep only line number
	fd_goal_expansion(Goal, Module, Expanded).

:- endif. % !SPIDER

fd_goal_expand_in(X, Expr, Goal) :-
	(   on_exception(error(_,_), set_expression(Expr, Set), fail)
	->  in_set_goal_1(X, Set, Goal)
	;   Goal = clpfd:in_aux_rt(X,Expr)
	).

%%% propositional constraints

fd_goal_expand_prop(Prop, M, Expanded) :-
	trans_bool(Prop, M, S0, []), !,
	fd_commafy(S0, Expanded).
fd_goal_expand_prop(Prop, M, Expanded) :-
	ill_formed_constraint(M:Prop, Expanded).

fd_commafy([], true).
fd_commafy([true|L], Conj) :- !, fd_commafy(L, Conj).
fd_commafy([X|L], Conj) :- fd_ify(X, X1), fd_commafy(L, X1, Conj).

fd_commafy([], X, X).
fd_commafy([true|L], X, Conj) :- !, fd_commafy(L, X, Conj).
fd_commafy([Y|L], X, (X,Conj)) :- fd_ify(Y, Y1), fd_commafy(L, Y1, Conj).

fd_ify(M:P, M:P) :- !.
fd_ify(X=Y, X=Y) :- !.
fd_ify(G, clpfd:G).

trans_bool(Prop, M) -->
	bool_decompose(Prop, M, Expr, 0),
	{bool_leaves(Expr, Simples, [])},
	{bool_normalize(Expr, Expr1)},
	{sort(Simples, Set1)},
	{term_variables_set(Expr1, Set2)},
	{ord_subtract(Set1, Set2, Set3)},
	{ord_subtract(Set3, [0,1,false,true], Diff)},
	% SPRM 13801: ensure that eliminated variables get 0..1 domain
	(   {Diff = []} -> []
	;   [domain(Diff, 0, 1)]
	),
	bool_connect_top(Expr1).

bool_leaves(X, [X|Simples], Simples) :- simple(X), !.
bool_leaves(not(X), Simples0, Simples) :-
	bool_leaves(X, Simples0, Simples).
bool_leaves(and(X,Y), Simples0, Simples) :-
	bool_leaves(X, Simples0, Simples1),
	bool_leaves(Y, Simples1, Simples).
bool_leaves(or(X,Y), Simples0, Simples) :-
	bool_leaves(X, Simples0, Simples1),
	bool_leaves(Y, Simples1, Simples).
bool_leaves(xor(X,Y), Simples0, Simples) :-
	bool_leaves(X, Simples0, Simples1),
	bool_leaves(Y, Simples1, Simples).

% bool_decompose(P, _, B, _) --> {atom(P)}, !, {B=P}. % SPRM 14178
bool_decompose(P, _, B, _) --> {simple(P)}, !, {B=P}.
bool_decompose(M:Constraint, _, B, Depth) --> !,
	bool_decompose(Constraint, M, B, Depth).
bool_decompose( #\  P, M, not(P1), _) --> !,
	bool_decompose(P, M, P1, 1).
bool_decompose(P #/\ Q, M, and(P1,Q1), _) --> !,
	bool_decompose(P, M, P1, 1),
	bool_decompose(Q, M, Q1, 1).
bool_decompose(P #\  Q, M, xor(P1,Q1), _) --> !,
	bool_decompose(P, M, P1, 1),
	bool_decompose(Q, M, Q1, 1).
bool_decompose(P #\/ Q, M, or(P1,Q1), _) --> !,
	bool_decompose(P, M, P1, 1),
	bool_decompose(Q, M, Q1, 1).
bool_decompose(P #=> Q, M, or(Q1,not(P1)), _) --> !,
	bool_decompose(P, M, P1, 1),
	bool_decompose(Q, M, Q1, 1).
bool_decompose(Q #<= P, M, B, Depth) --> !,
	bool_decompose(P #=> Q, M, B, Depth).
bool_decompose(P #<=> Q, M, Deco, Depth) --> !,
	{bool_decompose(P, M, P1, 1, S0, S1)},
	{bool_decompose(Q, M, Q1, 1, S1, [])},
	{   Depth=0, var(P1), var(Q1), % top-level context
	    S0 \== []		       % at least one new var
	->  P1 = Q1, Deco = true
	;   Deco = xor(P1,not(Q1))
	},
	(foreach(Goal,S0) do [Goal]).
bool_decompose(X in Expr, _, B, _) --> !,
	(   {set_expression(Expr, Set)}
	->  [in_set_iff_rt(X,Set,B)]
	;   [in_aux_rt(X,Expr,B)]
	).
bool_decompose(X in_set Set, _, B, _) --> !,
	(   {'$fd_size'(Set, _, 1)}
	->  [in_set_iff_rt(X,Set,B)]
	;   [in_set_aux_rt(X,Set,B)]
	).
bool_decompose(element(I,L,Y), M, B, Depth) --> !,
	(   foreach(X,L),
	    foreach(I#=J #/\ Y#=X,Disjuncts),
	    count(J,1,_),
	    param(I,Y)
	do  []
	),
	{orify(Disjuncts, Expanded)},
	bool_decompose(Expanded, M, B, Depth).
bool_decompose(table(Tuples,Ext), M, B, Depth) --> !,
	(   foreach(Tuple,Tuples),
	    foreach(Conjunct,Conjuncts),
	    param(Ext)
	do  (   foreach(Row,Ext),
		foreach(Case,Disjuncts),
		param(Tuple)
	    do  (   foreach(X,Tuple),
		    foreach(Y,Row),
		    foreach(C,Cs)
		do  {simple(Y) -> C = (X#=Y) ; C = (X in Y)}
		),
		{andify(Cs, Case)}
	    ),
	    {orify(Disjuncts,Conjunct)}
	),
	{andify(Conjuncts, Expanded)},
	bool_decompose(Expanded, M, B, Depth).
bool_decompose(Goal0, M, B, _) -->
	{fd_expandable(Goal0, X, Y, Rel)}, !,
 	(   {trans_goal(X, Rel, Y, M, List, [])}
	->  (   {List==[]} -> {B=1}
            ;   {member(fail, List)} -> {B=0}
	    ;   unfoldeq_reify(List, B)
	    )
	%;  {ill_formed_constraint(M:Goal0 #<=> B)}
        ).
bool_decompose(Goal, M, B, _) --> [iff_aux(M:Goal,B)].

% unfoldeq_reify(+Goals, -ToReify) --> ToPost.
unfoldeq_reify([], 1) --> [].
unfoldeq_reify([Goal|Goals], B) -->
	unfoldeq_reify_quodivmodrem(Goal, B, B1), !,
	unfoldeq_reify(Goals, B1).
unfoldeq_reify([Last], B) --> !,
	reify(Last, B).
unfoldeq_reify([X=Y|L], B) --> !, {X=Y},
	unfoldeq_reify(L, B).
unfoldeq_reify([Goal|Goals], B) --> [Goal],
	unfoldeq_reify(Goals, B).

unfoldeq_reify_quodivmodrem('x/y=z'(X,Y,Z), 0, _) --> {Y==0}, !, [domain([X,Z], inf, sup)].
unfoldeq_reify_quodivmodrem('x div y=z'(X,Y,Z), 0, _) --> {Y==0}, !, [domain([X,Z], inf, sup)].
unfoldeq_reify_quodivmodrem('x mod y=z'(X,Y,Z), 0, _) --> {Y==0}, !, [domain([X,Z], inf, sup)].
unfoldeq_reify_quodivmodrem('x rem y=z'(X,Y,Z), 0, _) --> {Y==0}, !, [domain([X,Z], inf, sup)].
unfoldeq_reify_quodivmodrem('x/y=z'(X,Y,Z), B, B) --> {integer(Y)}, !,
	['x/y=z'(X,Y,Z)].
unfoldeq_reify_quodivmodrem('x div y=z'(X,Y,Z), B, B) --> {integer(Y)}, !,
	['x div y=z'(X,Y,Z)].
unfoldeq_reify_quodivmodrem('x mod y=z'(X,Y,Z), B, B) --> {integer(Y)}, !,
	['x mod y=z'(X,Y,Z)].
unfoldeq_reify_quodivmodrem('x rem y=z'(X,Y,Z), B, B) --> {integer(Y)}, !,
	['x rem y=z'(X,Y,Z)].
unfoldeq_reify_quodivmodrem('x/y=z'(X,Y,Z), B, B3) -->
	['x/y=z'(X,Y1,Z), 'x\\=y'(Y,0,B1), 'x=y'(Y,Y1,B2), bool_or([B2,#\B1], 1), bool_and([B1,B3], B)].
unfoldeq_reify_quodivmodrem('x div y=z'(X,Y,Z), B, B3) -->
	['x div y=z'(X,Y1,Z), 'x\\=y'(Y,0,B1), 'x=y'(Y,Y1,B2), bool_or([B2,#\B1], 1), bool_and([B1,B3], B)].
unfoldeq_reify_quodivmodrem('x mod y=z'(X,Y,Z), B, B3) -->
	['x mod y=z'(X,Y1,Z), 'x\\=y'(Y,0,B1), 'x=y'(Y,Y1,B2), bool_or([B2,#\B1], 1), bool_and([B1,B3], B)].
unfoldeq_reify_quodivmodrem('x rem y=z'(X,Y,Z), B, B3) -->
	['x rem y=z'(X,Y1,Z), 'x\\=y'(Y,0,B1), 'x=y'(Y,Y1,B2), bool_or([B2,#\B1], 1), bool_and([B1,B3], B)].

reify(T = U, B) -->
	['x=y'(T,U,B)].
reify('x=y'(T,U), B) -->
	['x=y'(T,U,B)].
reify('x\\=y'(T,U), B) -->
	['x\\=y'(T,U,B)].
reify('x=<y'(T,U), B) -->
	['x=<y'(T,U,B)].
reify('x+y=t'(X,Y,T), B) -->
	['x+y=t'(X,Y,Z), 'x=y'(T,Z,B)].
reify('ax=t'(A,X,T), B) -->
	['ax=t'(A,X,Z), 'x=y'(T,Z,B)].
reify('ax+y=t'(A,X,Y,T), B) -->
	['ax+y=t'(A,X,Y,Z), 'x=y'(T,Z,B)].
reify('ax+by=t'(A,X,B,Y,T), B) -->
	['ax+by=t'(A,X,B,Y,Z), 'x=y'(T,Z,B)].
reify('t=c'(T,C), B) -->
	['x=y'(T,C,B)].
reify('t=<c'(T,C), B) -->
	['x=<y'(T,C,B)].
reify('t\\=c'(T,C), B) -->
	['x\\=y'(T,C,B)].
reify('t>=c'(T,C), B) -->
	['x=<y'(C,T,B)].
reify('t=u+c'(T,U,C), B) -->
	['t=u+c'(Z,U,C), 'x=y'(T,Z,B)].
reify('t=<u+c'(T,U,C), B) -->
	['t=u+c'(Z,U,C), 'x=<y'(T,Z,B)].
reify('t\\=u+c'(T,U,C), B) -->
	['t=u+c'(Z,U,C), 'x\\=y'(T,Z,B)].
reify('t>=u+c'(T,U,C), B) -->
	['t=u+c'(Z,U,C), 'x=<y'(Z,T,B)].
reify('t+u=c'(T,U,C), B) -->
	['x+y=t'(T,U,Z), 'x=y'(Z,C,B)].
reify('t+u=<c'(T,U,C), B) -->
	['x+y=t'(T,U,Z), 'x=<y'(Z,C,B)].
reify('t+u\\=c'(T,U,C), B) -->
	['x+y=t'(T,U,Z), 'x\\=y'(Z,C,B)].
reify('t+u>=c'(T,U,C), B) -->
	['x+y=t'(T,U,Z), 'x=<y'(C,Z,B)].
reify('t=x+y+c'(T,X,Y,C), B) -->
	['t=x+y+c'(U,X,Y,C), 'x=y'(T,U,B)].
reify('x+y=u+c'(X,Y,U,C), B) -->
	[('x+y=t'(X,Y,T), 't=u+c'(Z,U,C)), 'x=y'(T,Z,B)].
reify('x+y+z=c'(X,Y,Z,C), B) -->
	[('x+y=t'(X,Y,T), 'x+y=t'(T,Z,U)), 'x=y'(U,C,B)].
reify(scalar_product(As,Xs,#=,S1), B) -->
	[scalar_product([-1|As],[T|Xs],#=,0),
	 'x=y'(T,S1,B)].
reify(scalar_product(As,Xs,Rel,S1), B) -->
	[scalar_product([-1|As],[T|Xs],#=,0),
	 iff_aux(clpfd:Goal, B)],
	{t_rel_u(Rel, T, S1, [Goal], [])}.

% push negations down
% remove 0s and 1s as operands
bool_normalize(P, P) :- var(P), !.
bool_normalize(false, 0) :- !.
bool_normalize(true, 1) :- !.
bool_normalize(P, P) :- simple(P), !.
bool_normalize(not(P), R) :-
	bool_norm_neg(P, R).
bool_normalize(and(P,Q), Expr) :-
	bool_normalize(P, P1),
	bool_normalize(Q, Q1),
	bool_norm(and(P1,Q1), Expr).
bool_normalize(or(P,Q), Expr) :-
	bool_normalize(P, P1),
	bool_normalize(Q, Q1),
	bool_norm(or(P1,Q1), Expr).
bool_normalize(xor(P,Q), Expr) :-
	bool_normalize(P, P1),
	bool_normalize(Q, Q1),
	bool_norm(xor(P1,Q1), Expr).

bool_norm(and(P1,Q1), Expr) :-
	(   P1==0 -> Expr = 0
        ;   P1==1 -> Expr = Q1
        ;   Q1==0 -> Expr = 0
        ;   Q1==1 -> Expr = P1
	;   Expr = and(P1,Q1)
        ).
bool_norm(or(P1,Q1), Expr) :-
	(   P1==1 -> Expr = 1
        ;   P1==0 -> Expr = Q1
        ;   Q1==1 -> Expr = 1
        ;   Q1==0 -> Expr = P1
	;   Expr = or(P1,Q1)
        ).
bool_norm(xor(P1,Q1), Expr) :-
	(   P1==0 -> Expr = Q1
        ;   P1==1 -> bool_norm_neg(Q1, Expr)
        ;   Q1==0 -> Expr = P1
        ;   Q1==1 -> bool_norm_neg(P1, Expr)
	;   Expr = xor(P1,Q1)
        ).


bool_norm_neg(P, not(P)) :- var(P), !.
bool_norm_neg(0, 1) :- !.
bool_norm_neg(1, 0) :- !.
bool_norm_neg(false, 1) :- !.
bool_norm_neg(true, 0) :- !.
bool_norm_neg(P, not(P)) :- simple(P), !.
bool_norm_neg(not(P), R) :-
	bool_normalize(P, R).
bool_norm_neg(and(P,Q), Expr) :-
	bool_norm_neg(P, P1),
	bool_norm_neg(Q, Q1),
	bool_norm(or(P1,Q1), Expr).
bool_norm_neg(or(P,Q), Expr) :-
	bool_norm_neg(P, P1),
	bool_norm_neg(Q, Q1),
	bool_norm(and(P1,Q1), Expr).
bool_norm_neg(xor(P,Q), Expr) :-
	bool_normalize(P, P1),
	bool_norm_neg(Q, Q1),
	bool_norm(xor(P1,Q1), Expr).

bool_connect_top(V) --> {V==1}, !.
bool_connect_top(V) --> {simple(V)}, !, [V#=1]. % type checks
bool_connect_top(not(V)) --> !, [V#=0].		% type checks
bool_connect_top(xor(P,Q)) -->
	{bool_literal(P, P1)},
	{bool_literal(Q, Q1)}, !,
	[bool_xor([P1,Q1], 1)].
bool_connect_top(xor(P,Q)) -->
	{nonvar(Q), Q = not(Q1), var(Q1)}, !,
	bool_connect(P, Q1, 0).
bool_connect_top(xor(P,Q)) -->
	{nonvar(P), P = not(P1), var(P1)}, !,
	bool_connect(Q, P1, 0).
bool_connect_top(xor(P,Q)) -->
	{var(Q)}, !,
	{bool_norm_neg(P, NP)},
	bool_connect(NP, Q, 0).
bool_connect_top(xor(P,Q)) -->
	{var(P)}, !,
	{bool_norm_neg(Q, NQ)},
	bool_connect(NQ, P, 0).
bool_connect_top(Expr) -->
	bool_connect(Expr, 1, 0).

bool_connect(V, V, 0) --> {var(V)}, !.
bool_connect(not(V), V, 1) --> [].
bool_connect(and(P,Q), Z, 0) --> 
	and_flatten(P, S0, S),
	and_flatten(Q, S, []),
	[bool_and(S0,Z)].
bool_connect(or(P,Q), Z, 0) --> 
	or_flatten(P, S0, S),
	or_flatten(Q, S, []),
	[bool_or(S0,Z)].
bool_connect(xor(P,Q), Z, 0) --> 
	xor_flatten(P, S0, S),
	xor_flatten(Q, S, []),
	[bool_xor(S0,Z)].

and_flatten(V, [V|S], S) --> {var(V)}, !.
and_flatten(and(P,Q), S0, S) --> !,
	and_flatten(P, S0, S1),
	and_flatten(Q, S1, S).
and_flatten(P, [Lit|S], S) -->
	bool_connect(P, Z, Sense),
	{Sense=:=1 -> Lit = (#\ Z) ; Lit = Z}.

or_flatten(V, [V|S], S) --> {var(V)}, !.
or_flatten(or(P,Q), S0, S) --> !,
	or_flatten(P, S0, S1),
	or_flatten(Q, S1, S).
or_flatten(P, [Lit|S], S) -->
	bool_connect(P, Z, Sense),
	{Sense=:=1 -> Lit = (#\ Z) ; Lit = Z}.

xor_flatten(V, [V|S], S) --> {var(V)}, !.
xor_flatten(xor(P,Q), S0, S) --> !,
	xor_flatten(P, S0, S1),
	xor_flatten(Q, S1, S).
xor_flatten(P, [Lit|S], S) -->
	bool_connect(P, Z, Sense),
	{Sense=:=1 -> Lit = (#\ Z) ; Lit = Z}.

bool_literal(X, L) :- simple(X), !, L = X.
bool_literal(not(X), L) :- simple(X), !, L = #\(X).

/****************************************************************

Compiling QFPA constraints to indexicals.

Some examples:

elt(E,G) +: element(E, [0,75,85,80,75], G).

xor3(A,B,C) +: A#\B#\C.

foo(T,N3,M3) +: (T#=1#=>400#=<N3#/\64#=<M3) #/\ (T#=2#=>450#=<N3#/\128#=<M3).

| ?- elt(E,G), findall(E-G, indomain(E), L).
L = [1-0,2-75,3-85,4-80,5-75],
E in 1..5,
G in{0}\/{75}\/{80}\/{85} ? 
yes

| ?- xor3(A,B,C), findall([A,B,C],labeling([],[A,B,C]),L).
L = [[0,0,1],[0,1,0],[1,0,0],[1,1,1]],
A in 0..1,
B in 0..1,
C in 0..1 ? 
yes

| ?- T in 1..100, N3 in 375..475, M3 in {1,2,4,8,16,32,64,128,256,512,1024,2048,4096},
     foo(T,N3,M3),
     findall(1,labeling([],[T,N3,M3]),L),
     length(L,N).
L = [1,1,1,1,1,1,1,1,1,1|...],
N = 129362,
T in 1..100,
N3 in 375..475,
M3 in(1..2)\/{4}\/{8}\/{16}\/{32}\/{64}\/{128}\/{256}\/{512}\/{1024}\/{2048}\/{4096} ? 
yes

% Important datatypes:

%%%%%%%%%%%%%%

% Outset (set of intervals)

Outset --> list of OBox

%%%%%%%%%%%%%%

% O-box (outbox).

OBox --> Integer..Integer

%%%%%%%%%%%%%%

% S-expression (set expression), evaluates to an Outset
% S-expressions are compiled; see below.

ISExpr --> {} // empty set 
	| dom(OID)
        | AExpr..AExpr // singleton outbox if nonempty, otherwise empty set
        | nonempty(ISExpr) // if ISExpr is empty set then empty set, else infinite set
	| ISExpr/\ISExpr // intersection 
	| ISExpr\/ISExpr // union 

% A-expression (arithmetic expression), evaluates to an integer.

AExpr --> inf | sup | Integer
	| min(OID)
	| max(OID)
	| AExpr + AExpr
	| AExpr - AExpr
	| mul(AExpr,Integer)
	| floordiv(AExpr,Integer)
	| ceildiv(AExpr,Integer)

*/

ixadapt(X, X, _) :-
	simple(X), !.
ixadapt('$VAR'(I), X, Vars) :-
	nth0(I, Vars, X).
ixadapt(min(X), min(X1), Vars) :-
	ixadapt(X, X1, Vars).
ixadapt(max(X), max(X1), Vars) :-
	ixadapt(X, X1, Vars).
ixadapt(dom(X), dom(X1), Vars) :-
	ixadapt(X, X1, Vars).
ixadapt(nonempty(X), X1 ? (inf..sup), Vars) :-
	ixadapt(X, X1, Vars).
ixadapt(X..Y, X1..Y1, Vars) :-
	ixadapt(X, X1, Vars),
	ixadapt(Y, Y1, Vars).
ixadapt(P/\Q, X1 ? Y1, Vars) :-
	remove_from_conjunction(P/\Q, nonempty(X), Y), !,
	ixadapt(X, X1, Vars),
	ixadapt(Y, Y1, Vars).
ixadapt(X/\Y, X1/\Y1, Vars) :-
	ixadapt(X, X1, Vars),
	ixadapt(Y, Y1, Vars).
ixadapt(X\/Y, X1\/Y1, Vars) :-
	ixadapt(X, X1, Vars),
	ixadapt(Y, Y1, Vars).	
ixadapt(X+Y, X1+Y1, Vars) :-
	ixadapt(X, X1, Vars),
	ixadapt(Y, Y1, Vars).
ixadapt(X-Y, X1-Y1, Vars) :-
	ixadapt(X, X1, Vars),
	ixadapt(Y, Y1, Vars).
ixadapt(-Y, -Y1, Vars) :-
	ixadapt(Y, Y1, Vars).
ixadapt(mul(Y,I), Y1*I, Vars) :-
	ixadapt(Y, Y1, Vars).
ixadapt(floordiv(Y,I), Y1/<I, Vars) :-
	ixadapt(Y, Y1, Vars).
ixadapt(ceildiv(Y,I), Y1/>I, Vars) :-
	ixadapt(Y, Y1, Vars).

remove_from_conjunction(Key/\Q, Key, Q).
remove_from_conjunction(Q/\Key, Key, Q).
remove_from_conjunction(P/\Q, Key, P1/\Q) :-
	remove_from_conjunction(P, Key, P1).
remove_from_conjunction(P/\Q, Key, P/\Q1) :-
	remove_from_conjunction(Q, Key, Q1).

ixcompile_ground(Vars, Bool, Indexicals) :-
	% numbervars(Vars, 0, _),
	length(Vars, N),
	(   foreach(_,Vars),
	    foreach(inf,Infs),
	    foreach(sup,Sups)
	do  true
	),
	list_to_tree(Infs, LB0),
	list_to_tree(Sups, UB0),
	ixrewrite_bool(Bool, Tree1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_away_not(Tree1, Tree2),
	ixrewrite_relations(Tree2, Tree3),
	ixrewrite_away_ops(Tree3, Tree4),
	ixrewrite_away_min_max(Tree4, Tree5),
	ixrewrite_linearize(Tree5, Tree6),
	capture_bounds(Tree6, LB1, LB, UB1, UB, N),
	ixrewrite_simplify(Tree6, Tree7, LB, UB),
	(   for(J,1,N),
	    fromto(Ix0,['$VAR'(I)-(L1..U1)|Ix1],Ix1,Ix2),
	    param(LB,UB)
	do  I is J-1,
	    get_label(J, LB, L1),
	    get_label(J, UB, U1)
	),
	ixcompile_conjuncts(Tree7, Vars, Ix2, []),
	keysort(Ix0, Ix3),
	keyclumped(Ix3, Ix4),
	(   foreach(V-Clump,Ix4),
	    foreach(V-SE5,Indexicals),
	    param(Vars)
	do  partition_conjuncts(Clump, Dom, Res),
	    (   Res==[] -> SE4 = Dom
	    ;   (   foreach(R,Res),
		    foreach(S,[SE1|SEs]),
		    param(Dom)
		do  distribute_domains(R, Dom, S)
		),
		(   fromto(SE1,SE2,SE3,SE4),
		    foreach(SE,SEs)
		do  ixsexpr_intersection(SE2, SE, SE3)
		)
	    ),
	    ensure_dependencies(SE4, SE5, V, Vars)
	).

ensure_dependencies(SE0, SE, V, Vars) :-
	ixdependencies(SE0, Dep0, [V]),
	sort(Dep0, Dep),
	ord_subtract(Vars, Dep, Residue),
	(   foreach(R,Residue),
	    fromto(SE0,SE1,SE2,SE)
	do  ixsexpr_intersection(SE1, nonempty(dom(R)), SE2)
	).

ixdependencies(X) -->
	{simple(X)}, !.
ixdependencies(min(X)) --> [X].
ixdependencies(max(X)) --> [X].
ixdependencies(dom(X)) --> [X].
ixdependencies(nonempty(X)) -->
	ixdependencies(X).
ixdependencies(X..Y) -->
	ixdependencies(X),
	ixdependencies(Y).
ixdependencies(X/\Y) -->
	ixdependencies(X),
	ixdependencies(Y).
ixdependencies(X\/Y) -->
	ixdependencies(X),
	ixdependencies(Y).	
ixdependencies(X+Y) -->
	ixdependencies(X),
	ixdependencies(Y).
ixdependencies(X-Y) -->
	ixdependencies(X),
	ixdependencies(Y).
ixdependencies(-Y) -->
	ixdependencies(Y).
ixdependencies(mul(Y,_)) -->
	ixdependencies(Y).
ixdependencies(floordiv(Y,_)) -->
	ixdependencies(Y).
ixdependencies(ceildiv(Y,_)) -->
	ixdependencies(Y).

partition_conjuncts(Clump, Dom, Res) :-
	(   foreach(C,Clump),
	    fromto(inf..sup,Dom1,Dom2,Dom),
	    fromto(Res,Res1,Res2,[])
	do  (   ground_interval(C)
	    ->  ixsexpr_intersection(Dom1, C, Dom2),
		Res1 = Res2
	    ;   Dom1 = Dom2,
		Res1 = [C|Res2]
	    )
	).

distribute_domains(X/\Y, Dom, XY) :- !,
	distribute_domains(X, Dom, X1),
	distribute_domains(Y, Dom, Y1),
	ixsexpr_intersection(X1, Y1, XY).
distribute_domains(X\/Y, Dom, XY) :- !,
	distribute_domains(X, Dom, X1),
	distribute_domains(Y, Dom, Y1),
	ixsexpr_union(X1, Y1, XY).
distribute_domains(X, Dom, X1) :-
	ixsexpr_intersection(X, Dom, X1).

ixcompile_conjuncts(false, Vars) --> !,
	(   foreach(V,Vars) do [V-{}]   ).
ixcompile_conjuncts(true, _) --> !.
ixcompile_conjuncts(T #/\ U, Vars) --> !,
	ixcompile_conjuncts(T, Vars),
	ixcompile_conjuncts(U, Vars).
ixcompile_conjuncts(T, _) -->
	{vars_of_tree(T, Bag, [])},
	{sort(Bag, Vars)},
	(   foreach(V,Vars),
	    param(T)
	do  [V-Ix],
	    {feasible_set_for_object(T, V, Ix)}
	).
	
vars_of_tree(R1 #/\ R2) -->
	vars_of_tree(R1),
	vars_of_tree(R2).
vars_of_tree(R1 #\/ R2) -->
	vars_of_tree(R1),
	vars_of_tree(R2).
vars_of_tree(L #>= _) -->
	(foreach(V-_,L) do [V]).

% ixrewrite_bool(Rules, Tree)
%   Expand away all in, element, table, #\, #=>, #<=>
%   Simplify as far as possible.
ixrewrite_bool(X, X#=1, LB0, LB, UB0, UB, _) :-
	X = '$VAR'(I), !,
	J is I+1,
	put_label(J, LB0, 0, LB),
	put_label(J, UB0, 1, UB).
ixrewrite_bool(true, true, LB, LB, UB, UB, _) :- !.
ixrewrite_bool(false, false, LB, LB, UB, UB, _) :- !.
ixrewrite_bool(1, true, LB, LB, UB, UB, _) :- !.
ixrewrite_bool(0, false, LB, LB, UB, UB, _) :- !.
ixrewrite_bool(X in R, Expanded, LB, LB, UB, UB, Vars) :- !,
	range_to_fdset(R, S),
	ixrewrite_bool(X in_set S, Expanded, LB, LB, UB, UB, Vars).
ixrewrite_bool(X in_set S, Expanded, LB, LB, UB, UB, _) :- !,
	(   foreach([A|B],S),
	    foreach(D,Disjuncts),
	    param(X)
	do  (   [A|B]==[inf|sup] -> D = true
	    ;   A==inf -> D = (X #=< B)
	    ;   B==sup -> D = (X #>= A)
	    ;   D = (X #>= A #/\ X #=< B)
	    )
	),
	orify(Disjuncts, Expanded).
ixrewrite_bool(element(I,L,Y), Expanded, LB, LB, UB, UB, _) :- !,
	(   foreach(X,L),
	    foreach(I#=J #/\ Y#=X,Disjuncts),
	    count(J,1,_),
	    param(I,Y)
	do  true
	),
	orify(Disjuncts, Expanded).
ixrewrite_bool(table(Ext), Expanded, LB, LB, UB, UB, Vars) :- !,
	ixrewrite_bool(table([Vars],Ext), Expanded, LB, LB, UB, UB, Vars).
ixrewrite_bool(table(Tuples,Ext), Expanded, LB, LB, UB, UB, Vars) :- !,
	(   foreach(Tuple,Tuples),
	    foreach(Conjunct,Conjuncts),
	    param(Ext,LB,UB,Vars)
	do  (   foreach(Row,Ext),
		foreach(Case,Disjuncts),
		param(Tuple,LB,UB,Vars)
	    do  (   foreach(X,Tuple),
		    foreach(Y,Row),
		    foreach(C,Cs),
		    param(LB,UB,Vars)
		do  (   simple(Y) -> C = (X#=Y)
		    ;   ixrewrite_bool(X in Y, C, LB, LB, UB, UB, Vars)
		    )
		),
		andify(Cs, Case)
	    ),
	    orify(Disjuncts,Conjunct)
	),
	andify(Conjuncts, Expanded).
ixrewrite_bool(#\ P, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_bool(P, P1, LB0, LB, UB0, UB, Vars),
	(   P1==false -> Expanded=true
	;   P1==true -> Expanded=false
	;   Expanded=(#\P1)
	).
ixrewrite_bool(P #/\ Q, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_bool(P, P1, LB0, LB1, UB0, UB1, Vars),
	(   P1==false -> Expanded=false, LB1=LB, UB1=UB
	;   ixrewrite_bool(Q, Q1, LB1, LB, UB1, UB, Vars),
	    (   P1==true -> Expanded=Q1
	    ;   Q1==true -> Expanded=P1
	    ;   Q1==false -> Expanded=false
	    ;   Expanded=(P1#/\Q1)
	    )
	).
ixrewrite_bool(P #\/ Q, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_bool(P, P1, LB0, LB1, UB0, UB1, Vars),
	(   P1==true -> Expanded=true, LB1=LB, UB1=UB
	;   ixrewrite_bool(Q, Q1, LB1, LB, UB1, UB, Vars),
	    (   P1==false -> Expanded=Q1
	    ;   Q1==true -> Expanded=true
	    ;   Q1==false -> Expanded=P1
	    ;   Expanded=(P1#\/Q1)
	    )
	).
ixrewrite_bool(P #=> Q, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_bool(#\ P #\/ Q, Expanded, LB0, LB, UB0, UB, Vars).
ixrewrite_bool(P #\ Q, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_bool(P, P1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_bool(Q, Q1, LB1, LB,  UB1, UB, Vars),
	(   P1==true  -> Expanded = (#\Q1)
	;   Q1==true  -> Expanded = (#\P1)
	;   P1==Q1    -> Expanded = false
	;   P1==false -> Expanded = Q1
	;   Q1==false -> Expanded = P1
	;   Expanded = ((P1#/\(#\Q1))#\/((#\P1)#/\Q1))
	).
ixrewrite_bool(P #<=> Q, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_bool(P, P1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_bool(Q, Q1, LB1, LB,  UB1, UB, Vars),
	(   P1==true  -> Expanded = Q1
	;   Q1==true  -> Expanded = P1
	;   P1==Q1    -> Expanded = true
	;   P1==false -> Expanded = (#\Q1)
	;   Q1==false -> Expanded = (#\P1)
	;   Expanded = ((P1#/\Q1)#\/((#\P1)#/\(#\Q1)))
	).
ixrewrite_bool(Expr, Expanded, LB0, LB, UB0, UB, Vars) :-
	arithop(Expr, Op, X, Y), !,
	ixrewrite_expr(X, X1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_expr(Y, Y1, LB1, LB,  UB1, UB, Vars),
	(   integer(X1), integer(Y1) ->
	    evalop(Op, X1, Y1, Expanded)
	;   Expanded =.. [Op,X1,Y1]
	).

ixrewrite_expr(X, X, LB, LB, UB, UB, _) :-
	X = '$VAR'(_), !.
ixrewrite_expr(X, X, LB, LB, UB, UB, _) :-
	simple(X), !.
ixrewrite_expr(min(X,Y), Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_expr(X, X1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_expr(Y, Y1, LB1, LB,  UB1, UB, Vars),
	(   integer(X1), integer(Y1) ->
	    Expanded is min(X1,Y1)
	;   Expanded = min(X1,Y1)
	).
ixrewrite_expr(max(X,Y), Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_expr(X, X1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_expr(Y, Y1, LB1, LB,  UB1, UB, Vars),
	(   integer(X1), integer(Y1) ->
	    Expanded is max(X1,Y1)
	;   Expanded = max(X1,Y1)
	).
ixrewrite_expr(X+Y, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_expr(X, X1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_expr(Y, Y1, LB1, LB,  UB1, UB, Vars),
	(   X1==0 -> Expanded = Y1
	;   Y1==0 -> Expanded = X1
	;   integer(X1), integer(Y1) ->
	    Expanded is X1+Y1
	;   Expanded = (X1+Y1)
	).
ixrewrite_expr(-Y, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_expr(0-Y, Expanded, LB0, LB, UB0, UB, Vars).
ixrewrite_expr(X-Y, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_expr(X, X1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_expr(Y, Y1, LB1, LB,  UB1, UB, Vars),
	(   Y1==0 -> Expanded = X1
	;   integer(X1), integer(Y1) ->
	    Expanded is X1-Y1
	;   Expanded = (X1-Y1)
	).
ixrewrite_expr(X*Y, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_expr(X, X1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_expr(Y, Y1, LB1, LB,  UB1, UB, Vars),
	(   X1==0 -> Expanded = 0
	;   Y1==0 -> Expanded = 0
	;   X1==1 -> Expanded = Y1
	;   Y1==1 -> Expanded = X1
	;   integer(X1), integer(Y1) ->
	    Expanded is X1*Y1
	;   Expanded = (X1*Y1)
	).
ixrewrite_expr(X//Y, Expanded, LB0, LB, UB0, UB, Vars) :- !, % SPRM 13803
	ixrewrite_expr(X/Y, Expanded, LB0, LB, UB0, UB, Vars).
ixrewrite_expr(X/Y, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_expr(X, X1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_expr(Y, Y1, LB1, LB,  UB1, UB, Vars),
	(   Y1==0 -> Expanded = (X1/Y1)
	;   X1==0 -> Expanded = 0
	;   Y1==1 -> Expanded = X1
	;   integer(X1), integer(Y1), X1 rem Y1 =:= 0 ->
	    Expanded is X1//Y1
	;   Expanded = (X1/Y1)
	).
ixrewrite_expr(X div Y, Expanded, LB0, LB, UB0, UB, Vars) :- !,
	ixrewrite_expr(X, X1, LB0, LB1, UB0, UB1, Vars),
	ixrewrite_expr(Y, Y1, LB1, LB,  UB1, UB, Vars),
	(   Y1==0 -> Expanded = (X1 div Y1)
	;   X1==0 -> Expanded = 0
	;   Y1==1 -> Expanded = X1
	;   integer(X1), integer(Y1), X1 mod Y1 =:= 0 ->
	    Expanded is X1 div Y1
	;   Expanded = (X1 div Y1)
	).
ixrewrite_expr(Expr, reified(Expanded,#\Expanded), LB0, LB, UB0, UB, Vars) :-
	ixrewrite_bool(Expr, Expanded,  LB0, LB, UB0, UB, Vars).

arithop(A #= B, #=, A, B).
arithop(A #\= B, #\=, A, B).
arithop(A #< B, #<, A, B).
arithop(A #=< B, #=<, A, B).
arithop(A #> B, #>, A, B).
arithop(A #>= B, #>=, A, B).

evalop(#=, A, B, true)  :- A =:= B, !.
evalop(#\=, A, B, true) :- A =\= B, !.
evalop(#<, A, B, true)  :- A < B, !.
evalop(#=<, A, B, true) :- A =< B, !.
evalop(#>, A, B, true)  :- A > B, !.
evalop(#>=, A, B, true) :- A >= B, !.
evalop(_, _, _, false).

andify([], true).
andify([P|Ps], Conj) :-
	andify(Ps, P, Conj).

andify([], P, P).
andify([P|Ps], Q, Conj) :-
	andify(Ps, Q#/\P, Conj).

orify([], false).
orify([P|Ps], Disj) :-
	orify(Ps, P, Disj).

orify([], P, P).
orify([P|Ps], Q, Disj) :-
	orify(Ps, Q#\/P, Disj).

% ixrewrite_away_not(Tree1, Tree2)
%   Eliminate #\
ixrewrite_away_not(X, X) :- simple(X), !.
ixrewrite_away_not('$VAR'(X), '$VAR'(X)) :- !.
ixrewrite_away_not(Tree1, Tree2) :-
	arithop(Tree1, Op, X1, Y1),
	arithop(Tree2, Op, X2, Y2), !,
	ixrewrite_away_not(X1, X2),
	ixrewrite_away_not(Y1, Y2).
ixrewrite_away_not(#\ X, Y) :-
	ixrewrite_away_not_neg(X, Y).
ixrewrite_away_not(X1 #/\ X2, Y1 #/\ Y2) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not(X1 #\/ X2, Y1 #\/ Y2) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not(min(X1,X2), min(Y1,Y2)) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not(max(X1,X2), max(Y1,Y2)) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not(+(X1,X2), +(Y1,Y2)) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not(-(X1,X2), -(Y1,Y2)) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not(*(X1,X2), *(Y1,Y2)) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not(//(X1,X2), R) :- !, % SPRM 13803
	ixrewrite_away_not(/(X1,X2), R).
ixrewrite_away_not(/(X1,X2), /(Y1,Y2)) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not(div(X1,X2), div(Y1,Y2)) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not(reified(X1,X2), reified(Y1,Y2)) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).

ixrewrite_away_not_neg(true, false).
ixrewrite_away_not_neg(false, true).
ixrewrite_away_not_neg(#\ X, Y) :-
	ixrewrite_away_not(X, Y).
ixrewrite_away_not_neg(X1 #/\ X2, Y1 #\/ Y2) :-
	ixrewrite_away_not_neg(X1, Y1),
	ixrewrite_away_not_neg(X2, Y2).
ixrewrite_away_not_neg(X1 #\/ X2, Y1 #/\ Y2) :-
	ixrewrite_away_not_neg(X1, Y1),
	ixrewrite_away_not_neg(X2, Y2).
ixrewrite_away_not_neg(X1 #= X2, Y1 #\= Y2) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not_neg(X1 #\= X2, Y1 #= Y2) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not_neg(X1 #< X2, Y1 #>= Y2) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not_neg(X1 #=< X2, Y1 #> Y2) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not_neg(X1 #> X2, Y1 #=< Y2) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).
ixrewrite_away_not_neg(X1 #>= X2, Y1 #< Y2) :-
	ixrewrite_away_not(X1, Y1),
	ixrewrite_away_not(X2, Y2).

% ixrewrite_relations(Tree1, Tree2)
%   Normalize all expressions into one of [Expr #>= 0, Expr #> 0]
ixrewrite_relations(X, X) :- simple(X), !.
ixrewrite_relations('$VAR'(X), '$VAR'(X)) :- !.
ixrewrite_relations(X1 #/\ X2, Y1 #/\ Y2) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(X1 #\/ X2, Y1 #\/ Y2) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(X1 #= X2,  Y1-Y2 #>= 0 #/\ Y2-Y1 #>= 0) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(X1 #\= X2, Y1-Y2 #>  0 #\/ Y2-Y1 #>  0) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(X1 #< X2,  Y2-Y1 #>  0) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(X1 #> X2,  Y1-Y2 #>  0) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(X1 #=< X2, Y2-Y1 #>= 0) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(X1 #>= X2, Y1-Y2 #>= 0) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(min(X1,X2), min(Y1,Y2)) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(max(X1,X2), max(Y1,Y2)) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(+(X1,X2), +(Y1,Y2)) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(-(X1,X2), -(Y1,Y2)) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(*(X1,X2), *(Y1,Y2)) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(//(X1,X2), R) :- % SPRM 13803
	ixrewrite_relations(/(X1,X2), R).
ixrewrite_relations(/(X1,X2), /(Y1,Y2)) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(div(X1,X2), div(Y1,Y2)) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).
ixrewrite_relations(reified(X1,X2), reified(Y1,Y2)) :-
	ixrewrite_relations(X1, Y1),
	ixrewrite_relations(X2, Y2).

% ixrewrite_away_ops(Tree1, Tree2)
%   Eliminate -, *, /, div
ixrewrite_away_ops(true, true).
ixrewrite_away_ops(false, false).
ixrewrite_away_ops(X1 #/\ X2, Y1 #/\ Y2) :-
	ixrewrite_away_ops(X1, Y1),
	ixrewrite_away_ops(X2, Y2).
ixrewrite_away_ops(X1 #\/ X2, Y1 #\/ Y2) :-
	ixrewrite_away_ops(X1, Y1),
	ixrewrite_away_ops(X2, Y2).
ixrewrite_away_ops(X #> 0,  Y #>  0) :-
	ixrewrite_away_ops(X, Y, rat(1,1)).
ixrewrite_away_ops(X #>= 0, Y #>= 0) :-
	ixrewrite_away_ops(X, Y, rat(1,1)).

ixrewrite_away_ops(min(X1,X2), Expr, M) :- !,
	ixrewrite_away_ops(X1, Y1, M),
	ixrewrite_away_ops(X2, Y2, M),
	(   rat_gez(M) -> Expr = min(Y1,Y2)
	;   Expr = max(Y1,Y2)
	).
ixrewrite_away_ops(max(X1,X2), Expr, M) :- !,
	ixrewrite_away_ops(X1, Y1, M),
	ixrewrite_away_ops(X2, Y2, M),
	(   rat_gez(M) -> Expr = max(Y1,Y2)
	;   Expr = min(Y1,Y2)
	).
ixrewrite_away_ops(reified(Pos1,Neg1),
		   reified(Pos2,Neg2,scaled(M,1),scaled(rat(0,1),1)),
		   M) :- !,
	ixrewrite_away_ops(Pos1, Pos2),
	ixrewrite_away_ops(Neg1, Neg2).
ixrewrite_away_ops(X1 + X2, Y1 + Y2, M) :- !,
	ixrewrite_away_ops(X1, Y1, M),
	ixrewrite_away_ops(X2, Y2, M).
ixrewrite_away_ops(X1 - X2, Y1 + Y2, M) :- !,
	ixrewrite_away_ops(X1, Y1, M),
	rat_multiply(M, -1, M1),
	ixrewrite_away_ops(X2, Y2, M1).
ixrewrite_away_ops(X1 * X2, Y2, M) :-
	integer(X1), !,
	rat_mul(M, rat(X1,1), M1),
	ixrewrite_away_ops(X2, Y2, M1).
ixrewrite_away_ops(X1 * X2, Y1, M) :- !,
	integer(X2),
	rat_mul(M, rat(X2,1), M1),
	ixrewrite_away_ops(X1, Y1, M1).
ixrewrite_away_ops(X1 // X2, Y1, M) :- !, % SPRM 13803
	ixrewrite_away_ops(X1 / X2, Y1, M).
ixrewrite_away_ops(X1 / X2, Y1, M) :- !,
	integer(X2),
	rat_div(M, rat(X2,1), M1),
	ixrewrite_away_ops(X1, Y1, M1).
ixrewrite_away_ops(X1 div X2, Y1, M) :- !,
	integer(X2),
	rat_div(M, rat(X2,1), M1),
	ixrewrite_away_ops(X1, Y1, M1).
ixrewrite_away_ops(X, scaled(R,1), M) :-
	integer(X), !,
	rat_multiply(M, X, R).
ixrewrite_away_ops(X, scaled(M,X), M).

% ixrewrite_away_min_max(Tree1, Tree2)
%   Eliminate min, max
ixrewrite_away_min_max(true, true).
ixrewrite_away_min_max(false, false).
ixrewrite_away_min_max(X1 #/\ X2, Y1 #/\ Y2) :-
	ixrewrite_away_min_max(X1, Y1),
	ixrewrite_away_min_max(X2, Y2).
ixrewrite_away_min_max(X1 #\/ X2, Y1 #\/ Y2) :-
	ixrewrite_away_min_max(X1, Y1),
	ixrewrite_away_min_max(X2, Y2).
ixrewrite_away_min_max(X #> 0,  Comb) :-
	ixrewrite_distribute_plus(X, Y),
	ixrewrite_min_max(Y, Comb, #>).
ixrewrite_away_min_max(X #>= 0, Comb) :-
	ixrewrite_distribute_plus(X, Y),
	ixrewrite_min_max(Y, Comb, #>=).

ixrewrite_min_max(min(X,Y), P1 #/\ P2, Rel) :- !,
	ixrewrite_min_max(X, P1, Rel),
	ixrewrite_min_max(Y, P2, Rel).
ixrewrite_min_max(max(X,Y), P1 #\/ P2, Rel) :- !,
	ixrewrite_min_max(X, P1, Rel),
	ixrewrite_min_max(Y, P2, Rel).
ixrewrite_min_max(reified(Pos,Neg,PVal,NVal), (Pos #/\ V1) #\/ (Neg #/\ V2), Rel) :- !,
	ixrewrite_min_max(PVal, V1, Rel),
	ixrewrite_min_max(NVal, V2, Rel).
ixrewrite_min_max(X, P, Rel) :-
	P =.. [Rel,X,0].

ixrewrite_distribute_plus(min(X1,X2), min(Y1,Y2)) :-
	ixrewrite_distribute_plus(X1, Y1),
	ixrewrite_distribute_plus(X2, Y2).
ixrewrite_distribute_plus(max(X1,X2), max(Y1,Y2)) :-
	ixrewrite_distribute_plus(X1, Y1),
	ixrewrite_distribute_plus(X2, Y2).
ixrewrite_distribute_plus(reified(Pos1,Neg1,PVal,NVal), reified(Pos2,Neg2,PVal,NVal)) :-
	ixrewrite_away_min_max(Pos1, Pos2),
	ixrewrite_away_min_max(Neg1, Neg2).
ixrewrite_distribute_plus(X1+X2, Y) :-
	ixrewrite_distribute_plus(X1, Y1),
	ixrewrite_distribute_plus(X2, Y2),
	ixrewrite_distribute_plus(Y1, Y2, Y).
ixrewrite_distribute_plus(scaled(R,C), scaled(R,C)).

ixrewrite_distribute_plus(min(X1,X2), Y, min(Z1,Z2)) :- !,
	ixrewrite_distribute_plus(X1+Y, Z1),
	ixrewrite_distribute_plus(X2+Y, Z2).
ixrewrite_distribute_plus(Y, min(X1,X2), min(Z1,Z2)) :- !,
	ixrewrite_distribute_plus(Y+X1, Z1),
	ixrewrite_distribute_plus(Y+X2, Z2).
ixrewrite_distribute_plus(max(X1,X2), Y, max(Z1,Z2)) :- !,
	ixrewrite_distribute_plus(X1+Y, Z1),
	ixrewrite_distribute_plus(X2+Y, Z2).
ixrewrite_distribute_plus(Y, max(X1,X2), max(Z1,Z2)) :- !,
	ixrewrite_distribute_plus(Y+X1, Z1),
	ixrewrite_distribute_plus(Y+X2, Z2).
ixrewrite_distribute_plus(reified(Pos,Neg,PVal1,NVal1), Y, reified(Pos,Neg,PVal2,NVal2)) :- !,
	ixrewrite_distribute_plus(PVal1+Y, PVal2),
	ixrewrite_distribute_plus(NVal1+Y, NVal2).
ixrewrite_distribute_plus(Y, reified(Pos,Neg,PVal1,NVal1), reified(Pos,Neg,PVal2,NVal2)) :- !,
	ixrewrite_distribute_plus(Y+PVal1, PVal2),
	ixrewrite_distribute_plus(Y+NVal1, NVal2).
ixrewrite_distribute_plus(X, Y, X+Y).

% ixrewrite_linearize(Tree1, Tree2)
%   Normalize all expressions into one of [SP#>=C,true,false] where
%   SP is a list of Var-Coeff pairs, where Coeffs and C are integers.
ixrewrite_linearize(true, true).
ixrewrite_linearize(false, false).
ixrewrite_linearize(X1 #/\ X2, Y1 #/\ Y2) :-
	ixrewrite_linearize(X1, Y1),
	ixrewrite_linearize(X2, Y2).
ixrewrite_linearize(X1 #\/ X2, Y1 #\/ Y2) :-
	ixrewrite_linearize(X1, Y1),
	ixrewrite_linearize(X2, Y2).
ixrewrite_linearize(Expr #>= 0, Rel) :-
	ixrewrite_linearize(Expr, 0, Rel).
ixrewrite_linearize(Expr #> 0, Rel) :-
	ixrewrite_linearize(Expr, 1, Rel).

ixrewrite_linearize(Expr, Inc, Rel) :-
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

linearize(X+Y) -->
	linearize(X),
	linearize(Y).
linearize(scaled(R,X)) --> [X-R].

key_add_rat_values([], []).
key_add_rat_values([K1-C1,K2-C2|V1], V3) :-
	K1==K2, !,
	rat_add(C1, C2, C3),
	(C3==rat(0,1) -> V2=V1 ; V2=[K1-C3|V1]),
	key_add_rat_values(V2, V3).
key_add_rat_values([KC|V1], [KC|V2]) :-
	key_add_rat_values(V1, V2).

rat_gez(rat(A,_)) :- A>=0.

rat_normalize(rat(A,B), rat(C,D)) :-
	G is gcd(A,B)*sign(B),
	C is A//G,
	D is B//G.

rat_multiply(rat(A,B), M, R) :-
	C is M*A,
	rat_normalize(rat(C,B), R).

rat_add(rat(A,B), rat(C,D), R) :-
	E is A*D+B*C,
	F is B*D,
	rat_normalize(rat(E,F), R).

rat_sub(rat(A,B), rat(C,D), R) :-
	E is A*D-B*C,
	F is B*D,
	rat_normalize(rat(E,F), R).

rat_mul(rat(A,B), rat(C,D), R) :-
	E is A*C,
	F is B*D,
	rat_normalize(rat(E,F), R).

rat_div(rat(A,B), rat(C,D), R) :-
	E is A*D,
	F is B*C,
	rat_normalize(rat(E,F), R).

rat_min_max(rat(A,B), rat(C,D), Min, Max) :-
	(   A*D =< B*C ->
	    Min=rat(A,B), Max=rat(C,D)
	;   Max=rat(A,B), Min=rat(C,D)
	).

capture_bounds(P #/\ Q, LB0, LB, UB0, UB, N) :- !,
	capture_bounds(P, LB0, LB1, UB0, UB1, N),
	capture_bounds(Q, LB1, LB, UB1, UB, N).
capture_bounds(P #\/ Q, LB0, LB, UB0, UB, N) :- !,
	capture_bounds(P, LB0, LB1, UB0, UB1, N),
	capture_bounds(Q, LB0, LB2, UB0, UB2, N),
	(   for(I,1,N),
	    fromto(LB2,LB3,LB4,LB),
	    fromto(UB2,UB3,UB4,UB),
	    param(LB1,LB2,UB1,UB2)
	do  get_label(I, LB1, L1),
	    get_label(I, LB2, L2),
	    get_label(I, UB1, U1),
	    get_label(I, UB2, U2),
	    (le(L1, L2) -> put_label(I, LB3, L1, LB4) ; LB3 = LB4),
	    (le(U2, U1) -> put_label(I, UB3, U1, UB4) ; UB3 = UB4)
	).
capture_bounds(['$VAR'(I) - 1] #>= RHS, LB0, LB, UB, UB, _) :- !,
	J is I+1,
	get_label(J, LB0, L0),
	(le(L0, RHS) -> put_label(J, LB0, RHS, LB) ; LB0 = LB).
capture_bounds(['$VAR'(I) - -1] #>= RHS, LB, LB, UB0, UB, _) :- !,
	J is I+1,
	NHS is -RHS,
	get_label(J, UB0, U0),
	(le(NHS, U0) -> put_label(J, UB0, NHS, UB) ; UB0 = UB).
capture_bounds(_, LB, LB, UB, UB, _).

% ixrewrite_simplify(Tree1, Tree2)
%   Simplify away all occurrences of 'true' and 'false' except at the root.
ixrewrite_simplify(P #/\ Q, Rel, LB, UB) :- !,
	ixrewrite_simplify(P, P1, LB, UB),
	(   P1==false -> Rel = false
	;   ixrewrite_simplify(Q, Q1, LB, UB),
	    (   P1==true -> Rel = Q1
	    ;   Q1==true -> Rel = P1
	    ;   Q1==false -> Rel = false
	    ;   Rel = (P1#/\Q1)
	    )
	).
ixrewrite_simplify(P #\/ Q, Rel, LB, UB) :- !,
	ixrewrite_simplify(P, P1, LB, UB),
	(   P1==true -> Rel = true
	;   ixrewrite_simplify(Q, Q1, LB, UB),
	    (   P1==false -> Rel = Q1
	    ;   Q1==true -> Rel = true
	    ;   Q1==false -> Rel = P1
	    ;   Rel = (P1#\/Q1)
	    )
	).
ixrewrite_simplify(LHS#>=R, true, LB, UB) :-
	lower_bound(LHS, B, LB, UB),
	B >= R, !.
ixrewrite_simplify(LHS#>=R, false, LB, UB) :-
	(   foreach(V-P,LHS),
	    foreach(V-N,NHS)
	do  N is -P
	),
	lower_bound(NHS, B, LB, UB),
	-B < R, !.
ixrewrite_simplify(Expr, Expr, _LB, _UB).

lower_bound(LHS, B, LB, UB) :-
	(   foreach('$VAR'(I)-A,LHS),
	    fromto(0,B1,B2,B),
	    param(LB,UB)
	do  J is I+1,
	    (   A>0
	    ->  get_label(J, LB, Lj),
		(Lj==inf -> B2 is B1-0x10000000*A ; B2 is B1+A*Lj)
	    ;   get_label(J, UB, Uj),
		(Uj==sup -> B2 is B1+0x10000000*A ; B2 is B1+A*Uj)
	    )
	).

feasible_set_for_object(P #\/ Q, ID, Ixsexpr) :-
	feasible_set_for_object(P, ID, Ixsexpr1),
	feasible_set_for_object(Q, ID, Ixsexpr2),
	ixsexpr_union(Ixsexpr1, Ixsexpr2, Ixsexpr).
feasible_set_for_object(P #/\ Q, ID, Ixsexpr) :-
	feasible_set_for_object(P, ID, Ixsexpr1),
	feasible_set_for_object(Q, ID, Ixsexpr2),
	ixsexpr_intersection(Ixsexpr1, Ixsexpr2, Ixsexpr).
feasible_set_for_object(L#>=R, ID, Ixsexpr) :-
	select(ID-C, L, L1), !,
	build_ix(C, L1, R, Ixsexpr).
feasible_set_for_object(L#>=R, _, Ixsexpr) :-
	ix_build_sum(L, 1, max, 0, Sum),
	(   Sum = -NSum
	->  NR is -R,
	    Ixsexpr = nonempty(NSum..NR)
	;   Ixsexpr = nonempty(R..Sum)
	).

build_ix(C, L1, R, Quot..sup) :- C>0, !,
	ix_build_sum(L1, -1, min, R, Sum),
	(   C =:= 1 -> Quot = Sum
	;   Quot = ceildiv(Sum,C)
	).
build_ix(C, L1, R, inf..Quot) :-
	R1 is -R,
	ix_build_sum(L1, 1, max, R1, Sum),
	(   C =:= -1 -> Quot = Sum
	;   D is -C, Quot = floordiv(Sum,D)
	).

ix_build_sum([], _, _, Sum, Sum).
ix_build_sum([Ref-Con|V], Sign, Tag, Sum0, Sum) :-
	SCon is Sign*Con,
	SCon>0, !,
	tag_ix(Tag, Ref, Term),
	(   SCon =:= 1 -> Term1 = Term
	;   Term1 = mul(Term,SCon)
	),
	(   Sum0==0 -> Sum1 = Term1
	;   Sum1 = Sum0+Term1
	),
	ix_build_sum(V, Sign, Tag, Sum1, Sum).
ix_build_sum([Ref-Con|V], Sign, Tag, Sum0, Sum) :-
	ACon is -Sign*Con,
	fag_ix(Tag, Ref, Term),
	(   ACon =:= 1 -> Term1 = Term
	;   Term1 = mul(Term,ACon)
	),
	(   Sum0==0 -> Sum1 = -Term1
	;   Sum1 = Sum0-Term1
	),
	ix_build_sum(V, Sign, Tag, Sum1, Sum).

tag_ix(min, R, min(R)).
tag_ix(max, R, max(R)).

fag_ix(min, R, max(R)).
fag_ix(max, R, min(R)).

%% union and intersection

ixsexpr_union(SE1, SE2, SE) :-
	SE1 @> SE2, !,
	ixsexpr_union(SE2, SE1, SE).
ixsexpr_union(SE1, SE2, SE1) :-
	ixsexpr_subset(SE2, SE1), !.
ixsexpr_union(SE1, SE2, SE2) :-
	ixsexpr_subset(SE1, SE2), !.
ixsexpr_union(nonempty(SE1), nonempty(SE2), nonempty(SE3)) :- !,
	ixsexpr_union(SE1, SE2, SE3).
ixsexpr_union(I..max(A), min(A)..J, dom(A)/\((inf..J)\/(I..sup))) :- !.
ixsexpr_union(I..max(X), dom(X)/\S, dom(X)/\U) :- !,
	ixsexpr_union(I..sup, S, U).
ixsexpr_union(A1..B1, A2..B2, A3..B3) :-
	simple_aexpr(A1, A1v),
	simple_aexpr(A2, A2v),
	simple_aexpr(B1, B1v),
	simple_aexpr(B2, B2v),
	A1v=<B2v+1,
	A2v=<B1v+1,
	ixbound_min(A1, A2, A3),
	ixbound_max(B1, B2, B3), !.
ixsexpr_union(P/\R, Disj, Union) :-
	replace_disjunct(P/\Q, Disj, P/\QR, Union), !,
	ixsexpr_union(Q, R, QR).
ixsexpr_union(P/\R, Disj, Union) :-
	replace_disjunct(Q/\R, Disj, PQ/\R, Union), !,
	ixsexpr_union(P, Q, PQ).
ixsexpr_union(SE1, SE2, SE1\/SE2).

replace_disjunct(X, P0\/Q, Y, P\/Q) :-
	replace_disjunct(X, P0, Y, P).
replace_disjunct(X, P\/Q0, Y, P\/Q) :- !,
	replace_disjunct(X, Q0, Y, Q).
replace_disjunct(X, X, Y, Y).

simple_aexpr(inf, X) :- !, X = -0x10000000.
simple_aexpr(sup, X) :- !, X =  0x10000000.
simple_aexpr(I, I) :-
	integer(I).

ground_interval(X..Y) :-
	simple(X),
	simple(Y).

empty_interval(_..inf) :- !.
empty_interval(sup.._) :- !.
empty_interval(I..J) :-
	integer(I),
	integer(J),
	I > J.

ixsexpr_intersection(SE1, SE2, SE) :-
	SE1 @> SE2, !,
	ixsexpr_intersection(SE2, SE1, SE).
ixsexpr_intersection(SE1, SE2, SE1) :-
	ixsexpr_subset(SE1, SE2), !.
ixsexpr_intersection(SE1, SE2, SE2) :-
	ixsexpr_subset(SE2, SE1), !.
ixsexpr_intersection(nonempty(dom(X)/\SE1), nonempty(dom(X)/\SE2), nonempty(dom(X)/\SE3)) :- !,
	ixsexpr_intersection(SE1, SE2, SE3).
ixsexpr_intersection(nonempty(A1..max(X)), nonempty(min(X)..B2), nonempty(dom(X)/\(A1..B2))) :- !.
ixsexpr_intersection(nonempty(0.. -(min(B))+max(C)), nonempty(0..max(B)-min(C)), nonempty(dom(B)/\dom(C))) :- !.
ixsexpr_intersection(0.. -(min(B))+max(C), 0..max(B)-min(C), dom(B)/\dom(C)) :- !.
ixsexpr_intersection(A1..max(X), min(X)..B2, dom(X)/\(A1..B2)) :- !.
ixsexpr_intersection(A1..B1, A2..B2, SE3) :-
	ixbound_max(A1, A2, A3),
	ixbound_min(B1, B2, B3), !,
	(   \+empty_interval(A3..B3)
	->  SE3 = A3..B3
	;   SE3 = {}
	).
ixsexpr_intersection(SE1/\SE2, SE3, SE5) :-
	ixsexpr_intersection(SE1, SE3, SE4),
	SE4 \== SE1/\SE3,
	SE4 \== SE3/\SE1, !,
	ixsexpr_intersection(SE4, SE2, SE5).
ixsexpr_intersection(SE1/\SE2, SE3, SE5) :-
	ixsexpr_intersection(SE2, SE3, SE4),
	SE4 \== SE2/\SE3,
	SE4 \== SE3/\SE2, !,
	ixsexpr_intersection(SE1, SE4, SE5).
ixsexpr_intersection(SE3, SE1/\SE2, SE5) :-
	ixsexpr_intersection(SE1, SE3, SE4),
	SE4 \== SE1/\SE3,
	SE4 \== SE3/\SE1, !,
	ixsexpr_intersection(SE4, SE2, SE5).
ixsexpr_intersection(SE3, SE1/\SE2, SE5) :-
	ixsexpr_intersection(SE2, SE3, SE4),
	SE4 \== SE2/\SE3,
	SE4 \== SE3/\SE2, !,
	ixsexpr_intersection(SE1, SE4, SE5).
% distribute intervals into disjunctions (good for nonempty, but no effect)
% ixsexpr_intersection(A..B, SE1\/SE2, SE3\/SE4) :- !, 
% 	ixsexpr_intersection(A..B, SE1, SE3),
% 	ixsexpr_intersection(A..B, SE2, SE4).
ixsexpr_intersection(SE1, SE2, SE1/\SE2).

ixsexpr_subset({}, _) :- !.
ixsexpr_subset(_, {}) :- !, fail.
ixsexpr_subset(inf..sup, _) :- !, fail.
ixsexpr_subset(_, inf..sup) :- !.
ixsexpr_subset(nonempty(SE1), nonempty(SE2)) :- !,
	ixsexpr_subset(SE1, SE2).
ixsexpr_subset(SE1/\SE2, SE3) :- !,
	(   ixsexpr_subset(SE1, SE3) -> true
	;   ixsexpr_subset(SE2, SE3)
	).
ixsexpr_subset(SE1\/SE2, SE3) :- !,
	ixsexpr_subset(SE1, SE3),
	ixsexpr_subset(SE2, SE3).
ixsexpr_subset(SE1, SE2/\SE3) :- !,
	ixsexpr_subset(SE1, SE2),
	ixsexpr_subset(SE1, SE3).
ixsexpr_subset(SE1, SE2\/SE3) :- !,
	(   ixsexpr_subset(SE1, SE2) -> true
	;   ixsexpr_subset(SE1, SE3)
	).
ixsexpr_subset(A1..B1, A2..B2) :-
	ixbound_min(A1, A2, A2),
	ixbound_max(B1, B2, B2).

ixbound_min(inf, _, inf) :- !.
ixbound_min(_, inf, inf) :- !.
ixbound_min(sup, X, X) :- !.
ixbound_min(X, sup, X) :- !.
ixbound_min(X, Y, Z) :-
	integer(X),
	integer(Y), !,
	Z is min(X,Y).

ixbound_max(sup, _, sup) :- !.
ixbound_max(_, sup, sup) :- !.
ixbound_max(inf, X, X) :- !.
ixbound_max(X, inf, X) :- !.
ixbound_max(X, Y, Z) :-
	integer(X),
	integer(Y), !,
	Z is max(X,Y).

le(A, B) :-
	(   integer(A), integer(B) -> A=<B
	;   A==inf -> true
	;   B==sup -> true
	).

:- initialization hash_jumbo_instructions.

