%   Package: random
%   Author : Richard A. O'Keefe
%   Updated: 04/20/99
%   Purpose: Provide a portable random number generator.


%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

/*  This package uses algorithm AS 183 from the journal Applied Statistics
    as the basic random number generator.  random.c contains a "C"
    version of the code.  Lisp and Prolog versions exist too.  Exactly the
    same underlying sequence will be produced in all versions of Quintus
    Prolog; the actual results may not be identical because of differences
    in floating-point arithmetic.

    The operations are
	getrand(OldState)
	setrand(NewState)
	    -- allow saving and restoring the generator state
	maybe
	    -- succeed/fail with equal probability (variant of maybe/1)
	maybe(P) {Peter Schachte's brain-child}
	    -- succeed with probability P, fail with probability 1-P
	maybe(P, N)
	    -- succeed with probability P/N (variant of maybe/1)
	random(X)
	    -- bind X to a random float in [0.0,1.0)
	random(L, U, X)
	    -- bind X to a random element of [L,U)
	    -- if L, U are integers, X will be an integer.
	random_member(Elem, List)
	    -- unifies Elem with a random element of List.
	    -- Reports an error if List is not a proper list.
	random_perm2(A, B, X, Y)
	    -- does X=A,Y=B or X=B,Y=A with equal probability.
	    -- random_perm2 : perm2 :: random_permutation : permutation
	random_select(Elem, List, Rest)
	    -- is like select(Elem, List, Rest), but picks a random
	    -- element of the list.  You can use it to insert Elem in
	    -- a random place in Rest, yielding List, or to delete a
	    -- random Elem of List, yielding Rest.
	    -- Either List or Rest should be proper.
	    -- This used to be called rand_elem/3.
	random_subseq(List, Sbsq, Cmpl)
	    -- is like subseq(List, Sbsq, Cmpl), but picks a random
	    -- subsequence of List rather than enumerating all of them.
	    -- Either List, or both Sbsq and Cmpl, should be proper.
	random_permutation(L1, L2)
	    -- unify L2 with a random permutation of L1.
	    -- like permutation(L1, L2), either of the arguments may
	    -- be proper and the other will be solved for.
	    -- This used to be rand_perm/2.
*/

:- module(random, [
	getrand/1,			% save the state
	setrand/1,			% reset the state
	maybe/0,			% random success/failure
	maybe/1,			% random success/failure
	maybe/2,			% random success/failure
	random/1,			% uniform [0,1)
	random/3,			% uniform [L,U)
	random_numlist/4,		% Prob x Low x High -> Set
	random_member/2,		% Elem <- List
	random_select/3,		% Elem <- List -> Rest
	random_subseq/3,		% List -> Sbsq x Cmpl
	random_permutation/2,		% List -> Perm
	random_perm2/4			% Item x Item -> Item x Item
   ]).

:- use_module(library(types), [
	illarg/3,
	must_be/4
   ]).

:- mode
	setrand(+),
	maybe,
	maybe(+),
	maybe(+, +),
	random(?),
	random(+, -, -),
	random_member(?, +),
	    random_member(+, +, ?),
	random_select(?, ?, ?),
	    random_select(+, +, ?, +),
	    'one longer'(?, ?, -),
	    'common length'(?, ?, +, -),
	random_subseq(?, ?, ?),
	random_permutation(?, ?),
	    random_permutation(+, +, -, +),
		random_partition(+, +, +, -, -),
	random_perm2(?, ?, ?, ?).



foreign_resource(random, [
    init(rand_init),
    deinit(rand_deinit),
    'QPRget',		% read random-state variables
    'QPRput',		% write random-state variables
    'QPRnxt',		% return the next random number
    'QPRmyb',		% return 0/1 for maybe/2
    'QPRbit'		% return 0/1 for maybe/0
]).
foreign('QPRget', 'QPRget'(-integer,-integer,-integer,-integer)).
foreign('QPRput', 'QPRput'(+integer,+integer,+integer,+integer,[-integer])).
foreign('QPRnxt', 'QPRnxt'([-float])).
foreign('QPRbit', 'QPRbit'([-integer])).
foreign('QPRmyb', 'QPRmyb'(+integer,+integer,[-integer])).

:- load_foreign_resource(library(system(random))).


%@  This library module provides a random number generator using
%@  algorithm AS 183 from the Journal of Applied Statistics
%@  as the basic algorithm.
%@  
%@  The state of the random number generator corresponds to a term
%@  @code{random(@var{X},@var{Y},@var{Z},@var{B})} where 
%@  @var{X} is an integer in the range [1,30268],
%@  @var{Y} is an integer in the range [1,30306],
%@  @var{Z} is an integer in the range [1,30322], and
%@  @var{B} is a nonzero integer.
%@  
%@  Exported predicates:
%@  
%@  @table @code

%@  @item getrand(@var{-RandomState})
%@  @PLXindex {getrand/1 (random)}
%@  returns the random number generator's current state

getrand(random(X,Y,Z,B)) :-
	'QPRget'(X, Y, Z, B).


%@  @item setrand(@var{+RandomState})
%@  @PLXindex {setrand/1 (random)}
%@  sets the random number generator's state to @var{RandomState}.

%@  @var{RandomState} can either be a random state previously obtained
%@  with @code{getrand/1}, or an arbitrary integer. The latter is
%@  useful when you want to initialize the random state to a fresh
%@  value.

%@  If @var{RandomState} is not an integer or a valid random state, it
%@  raises an error.

setrand(Integer) :- integer(Integer), !,
	%% xref library(terms) which we do not want to depend on
	WhichCode = 4,		% jenkins
	%% xref random.c
	ALimit = 30269, ARange is ALimit - 1,
	BLimit = 30307, BRange is BLimit - 1,
	CLimit = 30323, CRange is CLimit - 1,
	SRange is 1<<30,
	prolog:'$term_hash_4_0_5'(Integer-a, WhichCode, -1, ARange, A1),
	prolog:'$term_hash_4_0_5'(Integer-b, WhichCode, -1, BRange, B1),
	prolog:'$term_hash_4_0_5'(Integer-c, WhichCode, -1, CRange, C1),
	prolog:'$term_hash_4_0_5'(Integer, WhichCode, -1, SRange, S),
	A is 1 + A1,
	B is 1 + B1,
	C is 1 + C1,
	setrand(random(A,B,C,S)).
setrand(random(X,Y,Z,B)) :-
	integer(X), integer(Y),
	integer(Z), integer(B),
	'QPRput'(X, Y, Z, B, 1),
	!.
setrand(RandomState) :-
	illarg(domain(term,random_state), setrand(RandomState), 1).


%@  @item maybe
%@  @PLXindex {maybe/[0,1,2] (random)}
%@  succeeds determinately with probability 1/2,
%@  fails with probability 1/2.  We use a separate "random bit"
%@  generator for this test to avoid doing much arithmetic.

maybe :-
	'QPRbit'(1).


%@  @item maybe(@var{+Probability})
%@  succeeds determinately with probability Probability,
%@  fails with probability @var{1-Probability}.
%@  Arguments =< 0 always fail, >= 1 always succeed.

maybe(P) :-
	random(X),
	X < P.


%@  @item maybe(@var{+P}, @var{+N})
%@  succeeds determinately with probability @var{P/N},
%@  where @var{0 =< P =< N} and @var{P} and @var{N} are integers.
%@  If this condition is not met, it fails.
%@  It is equivalent to @code{random(0, N, X), X < P}, but is somewhat faster.

maybe(P, N) :-
	'QPRmyb'(P, N, 1).

%   Sun-3/50, floating point in software:	0.83 -vs- 1.06 msec
%   Sun-3/160, floating point mc68881 chip:	0.21 -vs- 0.38 msec
%   These are the times for a single "maybe"    this -vs- random<P.



%@  @item random(@var{-Uniform})
%@  @PLXindex {random/[1,3] (random)}
%@  unifies @var{Uniform} with a new random number in [0.0,1.0)

random(Uniform) :-
	'QPRnxt'(Uniform).



%@  @item random(@var{+L}, @var{+U}, @var{-R})
%@  unifies @var{R} with a random integer in @var{[L,U)}
%@  when @var{L} and @var{U} are integers (note that @var{U} will @emph{never} be generated),
%@  or to a random floating number in @var{[L,U)} otherwise.
%   If either L or U is not a number, it quietly fails.

random(L, U, R) :- integer(L), integer(U), !,
	L < U,
	'QPRnxt'(X),
	R is integer((U-L)*X) + L.
random(L, U, R) :- number(L), number(U), !,
	L < U,
	'QPRnxt'(X),
	R is (U-L)*X + L.



/*----------------------------------------------------------------------+
|		    Randomised list-processing				|
+----------------------------------------------------------------------*/


%@  @item random_member(@var{-Elem}, @var{+List})
%@  @PLXindex {random_member/2 (random)}
%@  unifies @var{Elem} with a random element of @var{List}, which must be proper.
%@  Takes @var{O(N)} time (average and best case).

random_member(Elem, List) :-
	must_be(List, proper_list, random_member(Elem,List), 2),
	length(List, N),
	random(0, N, I),
	random_member(I, List, Elem).


%%  random_member/3 has the same arguments and meaning as nth0/3
%   in library(lists), but only works one way around.  (Not exported.)

random_member(0, [Elem|_], Elem) :- !.
random_member(N, [_|List], Elem) :-
	M is N-1,
	random_member(M, List, Elem).


%@  @item random_select(@var{?Elem}, @var{?List}, @var{?Rest})
%@  @PLXindex {random_select/3 (random)}
%@  unifies @var{Elem} with a random element of @var{List} and @var{Rest} with all the
%@  other elements of @var{List} (in order).  Either @var{List} or @var{Rest} should
%@  be proper, and @var{List} should/will have one more element than @var{Rest}.
%@  Takes @var{O(N}) time (average and best case).

random_select(Elem, List, Rest) :-
	'one longer'(List, Rest, N),
	random(0, N, I),
	random_select(I, List, Elem, Rest).


%   random_select/4 has the same arguments and meaning as nth0/4
%   in library(lists), but only works one way around.  (Not exported.)

random_select(0, List, Elem, Rest) :- !,
	List = [Elem|Rest].
random_select(N, [Head|Tail], Elem, [Head|Rest]) :-
	M is N-1,
	random_select(M, Tail, Elem, Rest).


%  'one longer'(+List, +Rest, -N) is true when List and Rest are lists,
%   List has N elements, and Rest has N-1 elements.  It is assumed
%   that either List or Rest is proper.

'one longer'([_|List], Rest, N) :-
	'common length'(List, Rest, 1, N).


%   'common length'(L1, L2, N0, N) is true when L1 and L2 are
%   lists of the same length, and N-N0 is that common length.
%   same_length/3 in library(lists) was derived from this.

'common length'([], [], N, N).
'common length'([_|X], [_|Y], K, N) :-
	L is K+1,
	'common length'(X, Y, L, N).



%@  @item random_subseq(@var{+List}, @var{-Sbsq}, @var{-Cmpl})
%@  @PLXindex {random_subseq/3 (random)}
%@  unifies @var{Sbsq} with a random sub-sequence of @var{List}, and @var{Cmpl} with its
%@  complement.  After this, @code{subseq(List, Sbsq, Cmpl)} will be true.
%@  Each of the @var{2**|List|} solutions is equally likely.  Like its
%@  name-sake @code{subseq/3}, if you supply @var{Sbsq} and @var{Cmpl} it will interleave
%@  them to find @var{List}.  Takes @var{O(N)} time.  @var{List} should be proper.

random_subseq([], [], []).
random_subseq([Elem|List], Sbsq, Cmpl) :-
	'QPRbit'(B),
	(   B =:= 0 ->		% B = 0 with probability 1/2
	    Sbsq = [Elem|Sbs1],
	    random_subseq(List, Sbs1, Cmpl)
	;   			% B = 1 with probability 1/2
	    Cmpl = [Elem|Cmp1],
	    random_subseq(List, Sbsq, Cmp1)
	).



%@  @item random_permutation(@var{?List}, @var{?Perm})
%@  @PLXindex {random_permutation/2 (random)}
%@  unifies @var{Perm} with a random permutation of @var{List}.  Either @var{List} or @var{Perm}
%@  should be proper, and they should/will have the same length. Each of
%@  the @var{N!} permutations is equally likely, where @code{length(List, N)}.
%   The old rand_perm/2 predicate took O(N**2) time and only worked one
%   way around; 
%@  This takes @var{O(N lg N)} time and is bidirectional.

random_permutation(List, Perm) :-
	'common length'(List, Perm, 0, N),
	random_permutation(N, List, Ans, []),
	Perm = Ans.

random_permutation(0, _, X, X) :- !.
random_permutation(1, [X], [X|L], L) :- !.
random_permutation(2, [X,Y], [U,V|L], L) :- !,
	'QPRbit'(B),
	( B =:= 0 -> U = X, V = Y ; U = Y, V = X ).
random_permutation(N, List, Front, Back) :-
	Z is N>>1,
	A is N-Z,
	random_partition(List, A, N, Alist, Zlist),
	random_permutation(Z, Zlist, Middle, Back),
	random_permutation(A, Alist, Front, Middle).


random_partition(L, 0, _, [], L) :- !.
random_partition([], _, _, [], []).
random_partition([X|Xs], P0, N0, A0, B0) :-
	'QPRmyb'(P0, N0, D),
	P1 is P0-D,
	N1 is N0-1,
	(   D =:= 1 -> A0 = [X|A1], random_partition(Xs, P1, N1, A1, B0)
	;   D =:= 0 -> B0 = [X|B1], random_partition(Xs, P1, N1, A0, B1)
	).


%@  @item random_perm2(@var{A},@var{B}, @var{X},@var{Y})
%@  @PLXindex {random_perm2/4 (random)}
%@  unifies @var{X,Y = A,B} or @var{X,Y = B,A}, making the choice at random,
%@  each choice being equally likely.  It is equivalent to
%@  @code{random_permutation([A,B], [X,Y])}.

random_perm2(U, V, X, Y) :-
	'QPRbit'(B),
	( B =:= 0 -> X = U, Y = V ; X = V, Y = U  ).


%@  @item random_numlist(@var{+P}, @var{+L}, @var{+U}, @var{-List})
%@  @PLXindex {random_numlist/4 (random)}
%@  where @var{P} is a probability (0..1) and @var{L=<U} are integers
%@  unifies @var{List} with a random subsequence of the integers @var{L..U},
%@  each integer being included with probability @var{P}.

random_numlist(P, L, U, List) :-
	Goal = random_numlist(P,L,U,List),
	must_be(P, number(between(0,1)), Goal, 1),
	must_be(L, integer, Goal, 2),
	must_be(U, integer, Goal, 3),
	random_numlist_1(L, U, P, List).

random_numlist_1(L, U, P, List) :-
	(   L > U -> List = []
	;   M is L+1,
	    random(X),
	    (   X > P -> random_numlist_1(M, U, P, List)
	    ;   List = [L|Rest],  random_numlist_1(M, U, P, Rest)
	    )
	).

%@  @end table
