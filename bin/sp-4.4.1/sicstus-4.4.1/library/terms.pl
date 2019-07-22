/* Copyright (C) 1995, Swedish Institute of Computer Science. */

%   File       : terms.pl
%   Author     : Mats Carlsson
%   Updated    : 25 July 2000
%   Purpose    : Miscellaneous operations on terms

:- module(terms, [
	subsumeschk/2, 
	subsumes/2, 
	variant/2, 
	term_subsumer/3, 
	term_hash/2,
	term_hash/3,		% [PM] 4.0.5
	term_hash/4,
        % [PM] 4.3 term_variables/2,
        term_variables_set/2,   % [PM] 4.3
	term_variables_bag/2,
	% [PM] 4.3 Now built-in (ISO Cor.2): acyclic_term/1,
	cyclic_term/1,
	% library(order)
	term_order/3,
	% library(occurs)
	contains_term/2,	%   T2 contains term T1
	contains_var/2,		%   T2 contains variable V1
	free_of_term/2,		%   T2 is free of term T1
	free_of_var/2,		%   T2 is free of variable V1
	occurrences_of_term/3,	%   T2 contains N3 instances of term T1
	occurrences_of_var/3,	%   T2 contains N3 instances of var V1
	sub_term/2,		%   T1 is a sub-term of T2 (enumerate T1)
	% library(term_depth)
	depth_bound/2,
	length_bound/2,
	size_bound/2,
	term_depth/2,
	term_size/2,
	% library(same_functor)
	same_functor/2,
	same_functor/3,
	same_functor/4
	]).

:- use_module(library(types), [
	must_be/4,
	illarg/3,
	illarg/4
	]).
:- use_module(library(avl), [
	empty_avl/1,
	avl_fetch/3,
	avl_store/4
	]).


%@  This library module provides miscellaneous operations on terms.
%@  Exported predicates:
%@  
%@  @table @code


%@  @item subsumeschk(@var{+General}, @var{+Specific})
%@  @PLXindex {subsumeschk/2 (terms)}
%@  is true when @var{Specific} is an instance of @var{General}.  It
%@  does not bind any variables.
%@  
%@  This predicate is identical to the built-in @code{subsumes_term/2}
%@  and it is only present for backwards compatibility.
%@
subsumeschk(General, Specific) :-
	subsumes_term(General, Specific).

%@  @item subsumes(@var{+General}, @var{+Specific})
%@  @PLXindex {subsumes/2 (terms)}
%@  is true when @var{Specific} is an instance of @var{General}.  It will bind
%@  variables in @var{General} (but not those in @var{Specific},
%@  except when @var{+General} and @var{+Specific} share variables) so that @var{General}
%@  becomes identical to @var{Specific}.
%@  
%@  @c [PM] 4.3 ISO Prologue and SWI warns that binding the variables may be error prone and rarely needed.
%@  In many cases, binding variable is not really desirable, in which case
%@  @code{subsumes_term/2} should be used instead.
%@  If unification is in fact wanted, it may be better to make this explicit in your code by
%@  using @code{subsumes_term/2} followed by an explicit unification, e.g.@: @code{subsumes_term(G,S), G=S}.

subsumes(General, Specific) :-
	subsumes_term(General, Specific),
	General = Specific.


%@  @item variant(@var{+Term}, @var{+Variant})
%@  @PLXindex {variant/2 (terms)}
%@  is true when @var{Term} and @var{Variant} are identical modulo renaming of variables,
%@  provided @var{Term} and @var{Variant} have no variables in common.

variant(Term, Variant) :-
	subsumes_term(Term, Variant),
	subsumes_term(Variant, Term).



%@  @item term_subsumer(@var{+Term1}, @var{+Term2}, @var{-Term})
%@  @PLXindex {term_subsumer/3 (terms)}
%@  binds @var{Term} to a most specific generalisation of @var{Term1} and @var{Term2}.
%@  Using Plotkin's algorithm [Machine Intelligence 5, 1970], extended
%@  by Dan Sahlin to handle cyclic structures.

term_subsumer(Term1, Term2, Subsumer) :-
	cyclic_term(Term1),
	cyclic_term(Term2), !,
	empty_avl(S),
	cyclic_subsumer(Term1, Term2, S, _, S, Subsumer).
term_subsumer(Term1, Term2, Subsumer) :-
	empty_avl(S),
	subsumer(Term1, Term2, S, _, Subsumer).

subsumer(Term1, Term2, S0, S, Term) :-
	(   compound(Term1), compound(Term2),
	    functor(Term1, F, N), functor(Term2, F, N)
	->  functor(Term, F, N),
	    subsumer(N, Term1, Term2, S0, S, Term)
	;   Term1 == Term2 -> S = S0, Term = Term1
	;   avl_fetch(Term1-Term2, S0, V) -> S = S0, Term = V
	;   avl_store(Term1-Term2, S0, Term, S)
	).

subsumer(0, _, _, S, S, _) :- !.
subsumer(N, T1, T2, S0, S, T3) :-
	arg(N, T1, T1x),
	arg(N, T2, T2x),
	arg(N, T3, T3x),
	subsumer(T1x, T2x, S0, S1, T3x),
	M is N-1,
	subsumer(M, T1, T2, S1, S, T3).


cyclic_subsumer(Term1, Term2, S0, S, U, Term) :-
	(   compound(Term1), compound(Term2),
	    functor(Term1, F, N), functor(Term2, F, N) ->
	    (	avl_fetch(Term1-Term2, U, V) -> S = S0, Term = V
	    ;	functor(Term, F, N),
		avl_store(Term1-Term2, U, Term, U1),
		cyclic_subsumer(N, Term1, Term2, S0, S, U1, Term)
	    )
	;   Term1 == Term2 -> S = S0, Term = Term1
	;   avl_fetch(Term1-Term2, S0, V) -> S = S0, Term = V
	;   avl_store(Term1-Term2, S0, Term, S)
	).

cyclic_subsumer(0, _, _, S, S, _, _) :- !.
cyclic_subsumer(N, T1, T2, S0, S, U, T3) :-
	arg(N, T1, T1x),
	arg(N, T2, T2x),
	arg(N, T3, T3x),
	cyclic_subsumer(T1x, T2x, S0, S1, U, T3x),
	M is N-1,
	cyclic_subsumer(M, T1, T2, S1, S, U, T3).





%@  @item term_hash(@var{+Term}, @var{-Hash})
%@  @PLXindex {term_hash/[2,3,4] (terms)}
%@  Equivalent to @code{term_hash(Term, [], Hash)}.
%% %@  @PLXindex {term_hash/[2,3,4] (terms)}
%% %@  If @var{Term} is ground, an integer hash value corresponding to @var{Term} 
%% %@  is unified with @var{Hash}.  Otherwise, the goal just succeeds.

term_hash(Term, Value) :-
	Modulus = 0x10000000,		% 1<<28 for 32-bit arch small integer, also used for 64-bit
	%% prolog:'$term_hash'(Term, -1, Modulus, Value).
	Depth = -1,
	which_code('default', Which),
	Options = Which,
	prolog:'$term_hash_4_0_5'(Term, Options, Depth, Modulus, Value).


%@  @item term_hash(@var{+Term}, @var{+Options}, @var{-Hash})
%@  @var{Options} is a list of options,
%@  
%@  @table @code
%@  @item algorithm(@var{Algorithm})
%@  
%@  @var{Algorithm} specifies which hash function to use. An atom, one
%@  of,
%@  
%@  @table @code
%@  
%@  @item default
%@  
%@  This is currently the same as @code{jenkins}. This is the
%@  default. If we ever see a need to change the default hash
%@  algorithm again then the algorithm denoted by @code{default} may
%@  change but the algorithm denoted by the other names, like
%@  @code{'sicstus-4.0.5'}, will not change.
%@  
%@  @item jenkins
%@  
%@  Based on the algorithm ``lookup3'' by Bob Jenkins, see
%@  @uref{http://burtleburtle.net/bob/hash/doobs.html}.
%@  
%@  @item hsieh
%@  
%@  Based on the algorithm ``SuperFastHash'' by Paul Hsieh, see
%@  @uref{http://www.azillionmonkeys.com/qed/hash.html}. Despite the
%@  name neither this nor any other choice of algorithm significantly
%@  affects the speed of @code{term_hash/3}.
%@  
%@  @item sdbm
%@  
%@  Based on the well known algorithm ``sdbm''.
%@  
%@  @item 'sicstus-4.0.4'
%@  
%@  This is the algorithm used up to SICStus Prolog 4.0.4
%@  (inclusive). It is only present to provide backwards
%@  compatibility. It is not as good as any of the above
%@  algorithms. Note that this atom needs to be quoted.
%@  
%@  This algorithm produces hash values that may differ
%@  between platforms.
%@  
%@  @item 'sicstus-4.0.5'
%@  
%@  This is the same as @code{jenkins}. I.e.@: the default since
%@  SICStus Prolog 4.0.5. Note that this atom needs to be quoted.
%@  
%@  @end table
%@  
%@  @noindent there are some other (not as good) algorithms available
%@  for the curious, see the source for detail.
%@  
%@  Unless otherwise noted, the hash value will be identical across
%@  runs and platforms.
%@  
%@  @item range(@var{Range})
%@  
%@  The resulting hash value will be non-negative and less than the
%@  upper bound specified by @var{Range}. @var{Range} should be either
%@  a positive integer, or an atom, one of,
%@  
%@  @table @code
%@  
%@  @item infinite
%@  
%@  Do not constrain the hash value. Currently all hash algorithms
%@  produce an unsigned 32-bit integer.
%@  
%@  @item smallint
%@  
%@  Ensure the resulting hash value is a @use{small integer}.
%@  This is the same as
%@  specifying a range of @code{2^28} on 32-bit platforms and
%@  @code{2^60} on 64-bit platforms.
%@  
%@  @item smallint32
%@  
%@  Ensure the resulting hash value is in the 32-bit platform range
%@  of small integers, i.e.@: the same as a range of @code{2^28}.
%@  
%% not documented
%% %@  @item smallint64
%@  
%@  @item default
%@  
%@  The same as @code{smallint32}. This is the default. This ensures
%@  that, by default, the same hash value is computed for the same
%@  term on both 32-bit and 64-bit platforms.
%@  
%@  @end table
%@  
%@  @item depth(@var{Depth})

%@  Specifies how deep to descend into the term when calculating the
%@  hash value.

%@  If @code{Depth} is a non-negative integer the subterms up to depth
%@  @var{Depth} of @var{Term} are used in the
%@  computation. Alternatively, if @code{Depth} is the atom
%@  @code{infinite}, all subterms of @var{Term} are relevant in
%@  computing @var{Hash}. In the latter case @var{Term} must be
%@  acyclic.

%@  In this context the @emph{depth} of a @use{term} is defined as
%@  follows: the (principal @use{functor} of) the @use{term} itself
%@  has depth 1, and an @use{argument} of a @use{term} with depth
%@  @var{i} has depth @var{i+1}. Note that this is similar to, but not
%@  the same as, the value computed by @code{term_depth/2}.


%@  For legacy reasons a @var{Depth} of -1 is treated the same a
%@  @code{infinite}.
%@  
%@  @item if_var(@var{IfVar})
%@  
%@  Specifies what to do if a variable is encountered in the term
%@  (i.e.@: to the specified depth). @var{IfVar} should be an atom, one of,
%@  
%@  @table @code
%@  
%@  @item error
%@  
%@  An instantiation error is thrown.
%@  
%@  @item ignore
%@  
%@  The variable is ignored and the hash algorithm continues with the
%@  other parts of the term.
%@  
%@  @item value(Value)
%@

%@  The hash algorithm stops, the intermediate hash result is
%@  discarded and @code{Hash} is bound to @code{Value}. There is no
%@  restrictions on @code{Value}, it need not be an integer or even be
%@  ground.

%@  @item default

%@  This is the same as @code{value(_)}, i.e.@: @code{term_hash/3}
%@  just succeeds without binding @code{Hash}. This is the default.

%@  This is useful when the hash value us used for first-argument
%@  indexing. This ensures that if the (possibly variable-valued) hash
%@  values for @var{Term1} and @var{Term2} are @var{Hash1} and
%@  @var{Hash2}, respectively, then if @var{Term1} and @var{Term2} are
%@  unifiable (to the specified depth) then so are @var{Hash1} and
%@  @var{Hash2}.

%@  For other use cases it is probably more appropriate to specify
%@  @code{if_var(error)}.

%@  @end table

%@  @end table

term_hash(Term, Options, HashValue) :-
	Goal = term_hash(Term, Options, HashValue),
	OptionsArgNo = 2,
	term_hash_options(Options, Which, Range, Depth, VarHandling, Goal, OptionsArgNo),
	TermArgNo = 1,
	term_hash1(Term, HashValue, Which, Range, Depth, VarHandling, Goal, TermArgNo).

%@  @item term_hash(@var{+Term}, @var{+Depth}, @var{+Range}, @var{-Hash})
%@  Equivalent to @code{term_hash(Term, [depth(@var{Depth}), range(@var{Range})], Hash)}.
%% %@  If @var{Term} is instantiated to the given @var{Depth}, an integer hash value in
%% %@  the range @var{[0,Range)} corresponding to @var{Term} is unified with @var{Hash}.
%% %@  Otherwise, the goal just succeeds.

term_hash(Term, Depth, Range, Value) :-
	term_hash(Term, [depth(Depth), range(Range)], Value).


%@  @code{term_hash/[2,3,4]} is provided primarily as a tool for the
%@  construction of sophisticated Prolog clause access schemes.	 Its
%@  intended use is to generate hash values for terms that will be
%@  used with first argument clause indexing, yielding compact and
%@  efficient multi-argument or deep argument indexing.



term_hash1(Term, HashValue, WhichCode, Range, Depth, VarHandling, Goal, TermArgNo) :-
	Options0 = WhichCode,
	set_var_handling_option(VarHandling, Options0, Options),
	prolog:'$term_hash_4_0_5'(Term, Options, Depth, Range, Value0),
	term_hash_value_fixup(Value0, VarHandling, Range, HashValue, Goal, TermArgNo).


set_var_handling_option(value(_), Options0, Options) :-
	Options = Options0.
set_var_handling_option('error', Options0, Options) :-
	Options = Options0.
set_var_handling_option('ignore', Options0, Options) :-
	PROLOG_TERM_HASH_OPTION_VAR_HANDLING_CONTINUE = 0x10,
	Options is Options0 \/ PROLOG_TERM_HASH_OPTION_VAR_HANDLING_CONTINUE.


term_hash_value_fixup(Value0, VarHandling, _Range, Value, Goal, TermArgNo) :-
	var(Value0), !,
	( VarHandling = value(Value1) ->
	    Value = Value1
	; %% VarHandling == 'error' ->
	    illarg(var, Goal, TermArgNo)
	).
term_hash_value_fixup(Value0, _VarHandling, Range, Value, _Goal, _TermArgNo) :-
	integer(Range), !,
	Value = Value0.
term_hash_value_fixup(Value0, _VarHandling, Range, Value, _Goal, _TermArgNo) :-
	Range = modulo(Modulus),
	%% Modulus may be too large to be handled in C
	Value is Value0 mod Modulus.

term_hash_options(Options, Which, Range, Depth, VarHandling, Goal, OptionsArgNo) :-
	which_code(default, Which0),
	Range0 = 0x10000000,
	Depth0 = -1,		% meaning 'infinite',
	VarHandling0 = value(_FreshVariable),
	must_be(Options, proper_list, Goal, OptionsArgNo),
	(   foreach(Option,Options),
	    fromto(Which0,Which1,Which2,Which),
	    fromto(Range0,Range1,Range2,Range),
	    fromto(Depth0,Depth1,Depth2,Depth),
	    fromto(VarHandling0,VarHandling1,VarHandling2,VarHandling),
	    param(Goal,OptionsArgNo)
	do  term_hash_option(Option,
			     Which1,Which2,
			     Range1,Range2,
			     Depth1,Depth2,
			     VarHandling1,VarHandling2,
			     Goal, OptionsArgNo)
	).

term_hash_option(Option, _Which0,_Which, _Range0,_Range, _Depth0,_Depth, _VarHandling0,_VarHandling, Goal, ArgNo) :-
	\+callable(Option), !,
	illarg(domain(nonvar, term_hash_option), Goal, ArgNo, Option).
term_hash_option(Option, _Which0,Which, Range0,Range, Depth0,Depth, VarHandling0,VarHandling, Goal, ArgNo) :-
	Option = algorithm(Alg), !,
	( nonvar(Alg), which_code(Alg, Which1) ->
	    true
	; otherwise ->
	    findall(A, which_code(A,_), As),
	    sort(As, Algorithms),
	    illarg(domain(nonvar, one_of(Algorithms)), Goal, ArgNo, Option)
	),
	Which = Which1,
	Range = Range0,
	Depth = Depth0,
	VarHandling = VarHandling0.
term_hash_option(Option, Which0,Which, _Range0,Range, Depth0,Depth, VarHandling0,VarHandling, Goal, ArgNo) :-
	Option = range(Range0), !,
	( Range0 == 'default' ->
	    Range = -2		% special flag
	; Range0 == 'smallint32' ->
	    Range = -2		% special flag
	%% All current algorithms give 32-bit results so smallint64 is an empty promise
	; Range0 == 'smallint64' ->
	    Range = -3		% special flag
	; Range0 == 'smallint' ->
	    Range = -4		% special flag
	; Range0 == 'infinite' ->
	    Range = -5		% special flag
	%% [PM] 4.0.5 Range = -1 was never documented (it caused hash = (x % (int)-1))
	%% ; Range0 == -1 ->		   % legacy
	%%	Range = -1		   % special flag
	; integer(Range0), Range0 =< (1<<31)-1 -> %	fits in 32-bit signed, C can do it
	    Range = Range0
	; integer(Range0) ->	% general case
	    %% [PM] 4.0.5 There is no reason to constrain the range
	    Domain = >=(1),
	    %% UB is (1<<31)-1,	% also on 64-bit machines
	    %% Domain = between(1, UB)
	    must_be(Range0, integer(Domain), Goal, ArgNo),
	    Range = modulo(Range0) % post process
	; illarg(domain(term, term_hash_range_option), Goal, ArgNo, Option)
	),
	Which = Which0,
	%% Range = Range0,
	Depth = Depth0,
	VarHandling = VarHandling0.
term_hash_option(Option, Which0,Which, Range0,Range, _Depth0,Depth, VarHandling0,VarHandling, Goal, ArgNo) :-
	Option = depth(Depth0), !,
	( Depth0 == 'infinite' ->
	    Depth = -1		% special flag
	; Depth0 == -1 ->	% legacy
	    Depth = -1		% special flag
	; integer(Depth0) ->
	    UB is (1<<31)-1,	% also on 64-bit machines
	    must_be(Depth0, integer(between(0, UB)), Goal, ArgNo),
	    Depth = Depth0
	; illarg(domain(term, term_hash_depth_option), Goal, ArgNo, Option)
	),
	Which = Which0,
	Range = Range0,
	%% Depth = Depth0,
	VarHandling = VarHandling0.
term_hash_option(Option, Which0,Which, Range0,Range, Depth0,Depth, _VarHandling0,VarHandling, Goal, ArgNo) :-
	Option = if_var(Handling), !,
	%% xref term_hash_var_handling/5
	( var(Handling) ->
	    illarg(domain(term, term_hash_variable_option), Goal, ArgNo, Option)
	; Handling = value(_) ->
	    VarHandling = Handling
	; Handling = 'error' ->
	    VarHandling = Handling
	; Handling = 'ignore' ->
	    VarHandling = Handling
	; Handling = 'default' ->
	    VarHandling = value(_FreshVariable)
	; illarg(domain(term, term_hash_variables_option), Goal, ArgNo, Option)
	),
	Which = Which0,
	Range = Range0,
	Depth = Depth0,
	%% VarHandling = VarHandling0,
	true.
term_hash_option(Option, _Which0,_Which, _Range0,_Range, _Depth0,_Depth, _VarHandling0,_VarHandling, Goal, ArgNo) :-
	illarg(domain(nonvar, term_hash_option), Goal, ArgNo, Option).

%% xref misc.c
which_code('sicstus-4.0.4', 1). % PROLOG_TERM_HASH_OPTION_WHICH_4_0_4. Legacy, bad.
which_code('hsieh',	     2). % PROLOG_TERM_HASH_OPTION_WHICH_HSIEH
which_code('sdbm',	    3). % PROLOG_TERM_HASH_OPTION_WHICH_SDBM
which_code('jenkins',	    4). % PROLOG_TERM_HASH_OPTION_WHICH_JENKINS
which_code('sicstus-4.0.5', 4).
which_code('default',	    4).
which_code('djb2',	    5). % PROLOG_TERM_HASH_OPTION_WHICH_DJB2
which_code('djb2_xor',	    6). % PROLOG_TERM_HASH_OPTION_WHICH_DJB2_XOR
which_code('k_and_r1',	    7). % PROLOG_TERM_HASH_OPTION_WHICH_K_AND_R_1. Very bad!
which_code('k_and_r2',	    8). % PROLOG_TERM_HASH_OPTION_WHICH_K_AND_R_2. Bad.
which_code('dummy',	    9). % PROLOG_TERM_HASH_OPTION_WHICH_DUMMY. Very bad!



%@  @item term_variables_set(@var{+Term}, @var{-Variables}) @since{release 4.3}
%@  @PLXindex {term_variables_set/2 (terms)}
%@  True if @var{Variables} is the (ordered) set of variables occurring in @var{Term}.
%@

%@  This was called @code{term_variables/2} prior to SICStus Prolog
%@  4.3 but now @code{term_variables/2} is a built-in with different
%@  meaning, due to alignment with the ISO Prolog standard.

term_variables_set(Term, Variables) :-
	term_variables(Term, Vs),
        sort(Vs, Variables).

%@  @item term_variables_bag(@var{+Term}, @var{-Variables})
%@  @PLXindex {term_variables_bag/2 (terms)}
%@  True if @var{Variables} is the list of variables occurring in @var{Term},
%@  in first occurrence order.
%@
%@  This predicate has been superseeded by the built-in
%@  @code{term_variables/2} and it is only present for backwards compatibility.
%@  
%@  The name is an historical accident, the result is not really a bag (i.e.@: multiset).
term_variables_bag(Term, Variables) :-
	term_variables(Term, Variables).

%@  @item cyclic_term(@var{+X})
%@  @PLXindex {cyclic_term/1 (terms)}
%@  True if @var{X} is infinite (cyclic).  Runs in linear time.

cyclic_term(X) :-
	\+acyclic_term(X).

%@  @item term_order(@var{+X}, @var{+Y}, @var{-R})
%@  @PLXindex {term_order/3 (terms)}
%@  is true when @var{X} and @var{Y} are arbitrary terms, and @var{R} is @code{<}, @code{=}, or @code{>} according
%@  as @var{X @@< Y}, @var{X == Y}, or @var{X @@> Y}.  This is the same as @code{compare/3}, except
%@  for the argument order.

term_order(X, Y, R) :-
	compare(R, X, Y).



%   Module : occurs
%   Author : Richard A. O'Keefe
%   Updated: 10 Apr 1990
%   Purpose: checking whether a term does/does not contain a term/variable

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.	All rights reserved.

:- mode
	contains_term(+, +),		%   Kernel x Term ->
	contains_var(+, +),		%   Kernel x Term ->
	free_of_term(+, +),		%   Kernel x Term ->
	free_of_var(+, +),		%   Kernel x Term ->
	occurrences_of_term(+, +, ?),	%   Kernel x Term -> Tally
	    occurrences_of_term(+, +, +, -),
	occurrences_of_var(+, +, ?),	%   Kernel x Term -> Tally
	    occurrences_of_var(+, +, +, -),
	sub_term(?, +),			%   Kernel x Term
	    sub_term(+, +, ?).		%   Tally x Term x Kernel




%   These relations were available in the public domain library with
%   different names and inconsistent argument orders.  The names are
%   different so that "contains", "freeof" and so forth can be used
%   by your own code.  free_of_var is used by the "unify" package.
%   The _term predicates check for a sub-term that unifies with the
%   Kernel argument, the _var predicates for one identical to it.
%   In release 2.0, the *_var predicates were modified to continue
%   using the identity (==) rather than unifiability (=) test, but
%   to cease insisting that the Kernel should be a variable.  When
%   the Kernel is a variable, they continue to work as before.



%@  @item contains_term(@var{+Kernel}, @var{+Expression})
%@  @PLXindex {contains_term/2 (terms)}
%@  is true when the given @var{Kernel} occurs somewhere in the @var{Expression}.
%@  It can only be used as a test; to generate sub-terms use @code{sub_term/2}.

contains_term(Kernel, Expression) :-
	\+ free_of_term(Kernel, Expression).


%@  @item free_of_term(@var{+Kernel}, @var{+Expression})
%@  @PLXindex {free_of_term/2 (terms)}
%@  is true when the given @var{Kernel} does not occur anywhere in the
%@  @var{Expression}.  NB: if the @var{Expression} contains an unbound variable,
%@  this must fail, as the @var{Kernel} might occur there.  Since there are
%@  infinitely many @var{Kernels} not contained in any @var{Expression}, and also
%@  infinitely many @var{Expressions} not containing any @var{Kernel}, it doesn't
%@  make sense to use this except as a test.

free_of_term(Kernel, Kernel) :- !,
	fail.
free_of_term(Kernel, Term) :-
	nonvar(Term),
	(   foreacharg(Arg,Term),
	    param(Kernel)
	do  free_of_term(Kernel, Arg)
	).

%@  @item occurrences_of_term(@var{+Kernel}, @var{+Expression}, @var{-Tally})
%@  @PLXindex {occurrences_of_term/3 (terms)}
%@  is true when the given @var{Kernel} occurs exactly @var{Tally} times in
%@  @var{Expression}.  It can only be used to calculate or test @var{Tally};
%@  to enumerate @var{Kernels} you'll have to use @code{sub_term/2} and then
%@  test them with this routine.  If you just want to find out
%@  whether @var{Kernel} occurs in @var{Expression} or not, use @code{contains_term/2}
%@  or @code{free_of_term/2}.

occurrences_of_term(Kernel, Expression, Occurrences) :-
	occurrences_of_term(Expression, Kernel, 0, Tally),
	Occurrences = Tally.

occurrences_of_term(Kernel, Kernel, Tally0, Tally) :- !,
	Tally is Tally0+1.
occurrences_of_term(Term, Kernel, Tally0, Tally) :-
	nonvar(Term),
	(   foreacharg(Arg,Term),
	    fromto(Tally0,Tally1,Tally2,Tally),
	    param(Kernel)
	do  occurrences_of_term(Arg, Kernel, Tally1, Tally2)
	).

%@  @item contains_var(@var{+Variable}, @var{+Term})
%@  @PLXindex {contains_var/2 (terms)}
%@  is true when the given @var{Term} contains at least one sub-term which
%@  is identical to the given @var{Variable}.  We use @code{==} to check for
%@  the variable (@code{contains_term/2} uses @code{=}) so it can be used to check
%@  for arbitrary terms, not just variables.

contains_var(Variable, Term) :-
	\+ free_of_var(Variable, Term).


%@  @item free_of_var(@var{+Variable}, @var{+Term})
%@  @PLXindex {free_of_var/2 (terms)}
%@  is true when the given @var{Term} contains no sub-term identical to the
%@  given @var{Variable} (which may actually be any term, not just a var).
%@  For variables, this is precisely the "occurs check" which is
%@  needed for sound unification.

free_of_var(Variable, Term) :-
	Variable==Term, !,
	fail.
free_of_var(_, Term) :-
	var(Term), !.
free_of_var(Variable, Term) :-
	(   foreacharg(Arg,Term),
	    param(Variable)
	do  free_of_var(Variable, Arg)
	).

%@  @item occurrences_of_var(@var{+Term}, @var{+Variable}, @var{-Tally})
%@  @PLXindex {occurrences_of_var/3 (terms)}
%@  is true when the given @var{Variable} occurs exactly @var{Tally} times in
%@  @var{Term}.	 It can only be used to calculate or test @var{Tally};
%@  to enumerate Variables you'll have to use @code{sub_term/2} and then
%@  test them with this routine.  If you just want to find out
%@  whether @var{Variable} occurs in @var{Term} or not, use @code{contains_var/2}
%@  or @code{free_of_var/2}.

occurrences_of_var(Variable, Term, Occurrences) :-
	occurrences_of_var(Term, Variable, 0, Tally),
	Occurrences = Tally.

occurrences_of_var(Term, Variable, Tally0, Tally) :-
	Term == Variable, !,
	Tally is Tally0+1.
occurrences_of_var(Term, _, Tally, Tally) :-
	var(Term), !.
occurrences_of_var(Term, Variable, Tally0, Tally) :-
	(   foreacharg(Arg,Term),
	    fromto(Tally0,Tally1,Tally2,Tally),
	    param(Variable)
	do  occurrences_of_var(Arg, Variable, Tally1, Tally2)
	).


:- sub_term(?, +) is nondet.
:- sub_term(+, ?, +) is nondet.
%@  @item sub_term(@var{?Kernel}, @var{+Term})
%@  @PLXindex {sub_term/2 (terms)}
%@  is true when @var{Kernel} is a sub-term of @var{Term}.  It enumerates the
%@  sub-terms of @var{Term} in an arbitrary order.  Well, it is defined
%@  that a sub-term of @var{Term} will be enumerated before its own
%@  sub-terms are (but of course some of those sub-terms might be
%@  elsewhere in @var{Term} as well).

sub_term(Term, Term).
sub_term(SubTerm, Term) :-
	nonvar(Term),
	functor(Term, _, N),
	N > 0,
	sub_term(N, Term, SubTerm).

sub_term(N, Term, SubTerm) :-
	arg(N, Term, Arg),
	sub_term(SubTerm, Arg).
sub_term(N, Term, SubTerm) :-
	N > 1,
	M is N-1,
	sub_term(M, Term, SubTerm).



%   Package: term_depth
%   Author : Richard A. O'Keefe
%   Updated: 04/15/99
%   Purpose: Find or check the depth of a term.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.	All rights reserved.

/*  Many resolution-based theorem provers impose a Depth Bound on the
    terms they create.	Not the least of the reasons for this is to
    stop infinite loops.  This module exports five predicates:

	term_depth(Term, Depth)
	depth_bound(Term, Bound)

	term_size(Term, Size)
	size_bound(Term, Bound)

	length_bound(List, Bound)

    term_depth calculates the depth of the term, using the definition
	term_depth(Var) = 0
	term_depth(Const) = 0
	term_depth(F(T1,...,Tn)) = 1+max(term_depth(T1),...,term_depth(Tn))

    Mostly, we couldn't care less what the depth of a term is, provided
    it is below some fixed bound.  depth_bound checks that the depth of
    the given term is below the bound (which is assumed to be an integer
    >= 1), without ever finding out what the depth actually is.

    term_size calculates the size of the term, defined to be the number
    of constant and function symbols in it.  (Note that this is a lower
    bound on the size of any term instantiated from it, and that
    instantiating any variable to a non-variable must increase the size.
    This latter property is why we don't count variables as 1.)

	term_size(Var) = 0
	term_size(Const) = 1
	term_size(F(T1,...,Tn)) = 1+term_size(T1)+...+term_size(Tn).

    size_bound(Term, Bound) is true if the size of Term is less than
    or equal to the Bound (assumed to be an integer >= 0).  Note that
    size_bound/2 and depth_bound/2 will always terminate.

    length_bound(List, Bound) is true when List is a list having at
    most Bound elements.  Bound must be instantiated.  If List ends
    with a variable, it will be instantiated to successively longer
    proper lists, up to the length permitted by the Bound.  This was
    added when I noticed that the depth of a list of constants is
    its length, and we already have a length/2, but did not have a
    length_bound.

    In the DEC-10 Prolog library, this was depth.pl, defining
    depth_of_term/2 and depth_bound/2.
*/

:- mode
	depth_bound(+, +),
	    depth_bound(+, +, +),
	size_bound(+, +),
	    size_bound(+, +, -),
		size_bound(+, +, +, -),
	term_depth(+, ?),
	term_size(+, ?).



%@  @item depth_bound(@var{+Term}, @var{+Bound})
%@  @PLXindex {depth_bound/2 (terms)}
%@  is true when the term depth of @var{Term} is no greater than @var{Bound},
%@  that is, when constructor functions are nested no more than @var{Bound} deep.
%@  Later variable bindings may invalidate this bound.	To find the
%@  (current) depth, use @code{term_depth/2}.

depth_bound(Compound, Bound) :-
	nonvar(Compound),
	functor(Compound, _, Arity),
	Arity > 0,
	!,
	Bound > 0,		% this is the test!
	Limit is Bound-1,
	depth_bound(Arity, Compound, Limit).
depth_bound(_, _).


depth_bound(N, Compound, Limit) :-
	(	N =:= 0 -> true
	;	arg(N, Compound, Arg),
	    depth_bound(Arg, Limit),
	    M is N-1,
	    depth_bound(M, Compound, Limit)
	).



%@  @item length_bound(@var{?List}, @var{+Bound})
%@  @PLXindex {length_bound/2 (terms)}
%@  is true when the length of @var{List} is no greater than @var{Bound}.  It can be
%@  used to enumerate Lists up to the bound.  
%   See also bnd/[2,3,4] in
%   library(more_lists).  Its companion is length/2, which is built in.

length_bound([], Bound) :-
	Bound >= 0.
length_bound([_|List], Bound) :-
	Bound > 0,
	Limit is Bound-1,
	length_bound(List, Limit).



%@  @item size_bound(@var{+Term}, @var{+Bound})
%@  @PLXindex {size_bound/2 (terms)}
%@  is true when the number of constant and function symbols in @var{Term} is
%@  (currently) at most @var{Bound}.  If @var{Term} is non-ground, later variable
%@  bindings may invalidate this bound.	 To find the (current) size, use
%@  @code{term_size/2}.

size_bound(Term, Bound) :-
	size_bound(Term, Bound, _).


size_bound(Term, Bound, Left) :-
	(	var(Term) -> Left = Bound
	;/* nonvar(Term) */
	functor(Term, _, Arity),
	    Bound > 0,		% this is the test!
	    Limit is Bound-1,
	    size_bound(Arity, Term, Limit, Left)
	).


size_bound(N, Term, Limit, Left) :-
	(	N =:= 0 ->
	    Left is Limit
	;	arg(N, Term, Arg),
	    size_bound(Arg, Limit, Limit1),
	    M is N-1,
	    size_bound(M, Term, Limit1, Left)
	).



%@  @item term_depth(@var{+Term}, @var{-Depth})
%@  @PLXindex {term_depth/2 (terms)}
%@  calculates the Depth of a Term, using the definition
%@  @example
%@	term_depth(Var) = 0
%@	term_depth(Const) = 0
%@	term_depth(F(T1,...,Tn)) = 1+max(term_depth(T1),...,term_depth(Tn))
%@  @end example
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  term_depth(X, Depth) :-
%@	simple(X), !, Depth = 0.
%@  term_depth(X, Depth) :-
%@	(   foreacharg(A,X),
%@	    fromto(0,D0,D,Depth0)
%@	do  term_depth(A, D1),
%@	    D is max(D0,D1)
%@	),
%@	Depth is Depth0+1.
%@  @end group
%@  @end example

term_depth(X, Depth) :-
	simple(X), !, Depth = 0.
term_depth(X, Depth) :-
	(   foreacharg(A,X),
	    fromto(0,D0,D,Depth0)
	do  term_depth(A, D1),
	    D is max(D0,D1)
	),
	Depth is Depth0+1.



%@  @item term_size(@var{+Term}, @var{-Size})
%@  @PLXindex {term_size/2 (terms)}
%@  calculates the @var{Size} of a @var{Term}, defined to be the number of constant and
%@  function symbol occurrences in it.
%@  Could be defined as:
%@  
%@  @example
%@  @group
%@  term_size(X, Size) :-
%@	var(X), !, Size = 0.
%@  term_size(X, Size) :-
%@	simple(X), !, Size = 1.
%@  term_size(X, Size) :-
%@	(   foreacharg(A,X),
%@	    fromto(1,S0,S,Size)
%@	do  term_size(A, S1),
%@	    S is S0+S1
%@	).
%@  @end group
%@  @end example

term_size(X, Size) :-
	var(X), !, Size = 0.
term_size(X, Size) :-
	simple(X), !, Size = 1.
term_size(X, Size) :-
	(   foreacharg(A,X),
	    fromto(1,S0,S,Size)
	do  term_size(A, S1),
	    S is S0+S1
	).

%   Module : same_functor
%   Author : Richard A. O'Keefe
%   Updated: 29 Aug 1989
%   Defines: same_functor/[2,3,4]

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.	All rights reserved.

:- mode
	same_functor(?, ?),
	same_functor(?, ?, ?),
	same_functor(?, ?, ?, ?).



%@  @item same_functor(@var{?T1}, @var{?T2})
%@  @PLXindex {same_functor/[2,3,4] (terms)}
%@  is true when @var{T1} and @var{T2} have the same principal functor.	 If one of
%@  the terms is a variable, it will be instantiated to a new term
%@  with the same principal functor as the other term (which should be
%@  instantiated) and with arguments being new distinct variables.  If
%@  both terms are variables, an error is reported.

same_functor(T1, T2) :-
	(   nonvar(T1) ->
	    functor(T1, F, N),
	    functor(T2, F, N)
	;   nonvar(T2) ->
	    functor(T2, F, N),
	    functor(T1, F, N)
	;   % var(T1), var(T2)
	    illarg(var, same_functor(T1,T2), 0)
	).


%@  @item same_functor(@var{?T1}, @var{?T2}, @var{?N})
%@  is true when @var{T1} and @var{T2} have the same principal functor, and their
%@  common arity is @var{N}. Like @code{same_functor/3}, at least one of @var{T1} and @var{T2}
%@  must be bound, or an error will be reported.  

same_functor(T1, T2, N) :-
	(   nonvar(T1) ->
	    functor(T1, F, N),
	    functor(T2, F, N)
	;   nonvar(T2) ->
	    functor(T2, F, N),
	    functor(T1, F, N)
	;   % var(T1), var(T2)
	    illarg(var, same_functor(T1,T2,N), 0)
	).



%@  @item same_functor(@var{?T1}, @var{?T2}, @var{?F}, @var{?N})
%@  is true when @var{T1} and @var{T2} have the same principal functor, and their
%@  common functor is @var{F/N}. Given @var{T1} (or @var{T2}) the remaining arguments
%@  can be computed.  Given @var{F} and @var{N}, the remaining arguments can be
%@  computed.  If too many arguments are unbound, an error is reported.

same_functor(T1, T2, F, N) :-
	(   nonvar(T1) ->
	    functor(T1, F, N),
	    functor(T2, F, N)
	;   nonvar(T2) ->
	    functor(T2, F, N),
	    functor(T1, F, N)
	;   number(F) ->
	    N = 0, T1 = F, T2 = F
	;   nonvar(F), nonvar(N) ->
	    functor(T1, F, N),
	    functor(T2, F, N)
	;   %  var(T1), var(T2), (var(F) ; var(N)), \+number(F).
	    illarg(var, same_functor(T1,T2,F,N), 0)
	).

%@  @end table
