/* Copyright(C) 1997, Swedish Institute of Computer Science */

/*
:- module(fdsets, [
	is_fdset/1,
	empty_fdset/1,
	fdset_parts/4,
	empty_interval/2,
	fdset_interval/3,
	fdset_singleton/2,
	fdset_min/2,
	fdset_max/2,
	fdset_size/2,
	list_to_fdset/2,
	fdset_to_list/2,
	range_to_fdset/2,
	fdset_to_range/2,
	fdset_add_element/3,
	fdset_del_element/3,
	fdset_disjoint/2,
	fdset_intersect/2,
	fdset_intersection/3,
	fdset_intersection/2,
	fdset_member/2,
	fdset_eq/2,
	fdset_subset/2,
	fdset_subtract/3,
	fdset_union/3,
	fdset_union/2,
	fdset_complement/2
		   ]).
*/

%   In this package, sets are represented by ordered lists of non-adjacent
%   intervals [Min|Max], where Min is an integer or the atom 'inf', and Max
%   is an integer or the atom 'sup'.  The guts of library(clpfd) assume
%   that fdsets are always fully dereferenced.



%   error detection

must_be_fdset(Set, Goal, ArgNo) :-
	var(Set), !,
	illarg(var, Goal, ArgNo).
must_be_fdset(Set, _Goal, _ArgNo) :-
	'$fd_size'(Set, _, 1), !.
must_be_fdset(_Set, Goal, ArgNo) :-
	illarg(domain(term,fdset), Goal, ArgNo).

must_be_finite_fdset(Set, Goal, ArgNo) :-
	var(Set), !,
	illarg(var, Goal, ArgNo).
must_be_finite_fdset(Set, _Goal, _ArgNo) :-
	'$fd_size'(Set, Size, 1),
	integer(Size), !.
must_be_finite_fdset(_Set, Goal, ArgNo) :-
	illarg(domain(term,finite_fdset), Goal, ArgNo).


%   is_fdset(+Set)
%   is true when Set is a valid fdset.

is_fdset(Set) :-
	'$fd_size'(Set, _, 1).


%   empty_fdset(?Set)
%   is true when Set is the empty fdset.

empty_fdset([]).


%   fdset_parts(?Set, ?Min, ?Max, ?RestSet)
%   is true when Set is an fdset which is a union of the interval Min..Max
%   and the fdset RestSet and all elements of RestSet are greater than Max+1.

fdset_parts(Set, Min, Max, RestSet) :- nonvar(Set), !,
	Set = [[Min|Max]|RestSet].
fdset_parts(Set, Min, Max, RestSet) :-
	'$fd_cons'(Min, Max, RestSet, Set0, 1), !,
	Set = Set0.
fdset_parts(Set, Min, Max, RestSet) :-
	illarg(var, fdset_parts(Set,Min,Max,RestSet), 0).


%   empty_interval(+Min, +Max)
%   is true when Min..Max is an empty interval.

empty_interval(Min, Max) :-
	nonvar(Min), nonvar(Max),
	'$fd_range'(Min, Max, [], 1).


%   fdset_interval(?Set, ?Min, ?Max)
%   is true when Set is an fdset which is the interval Min..Max.

fdset_interval(Set, Min, Max) :- nonvar(Set), !,
	Set = [[Min|Max]].
fdset_interval(Set, Min, Max) :-
	nonvar(Min), nonvar(Max), !,
	'$fd_range'(Min, Max, Set, 1),
	Set \== [].
fdset_interval(Set, Min, Max) :-
	illarg(var, fdset_interval(Set,Min,Max), 0).


%   fdset_singleton(?Set, ?Element)
%   is true when Set is an fdset containing Element only.

fdset_singleton(Set, E) :-
	nonvar(Set), !, Set = [[E|E]].
fdset_singleton(Set, E) :-
	integer(E), !, '$fd_range'(E, E, Set, 1).
fdset_singleton(Set, E) :-
	var(Set), var(E),
	illarg(var, fdset_singleton(Set,E), 0).


%   fdset_min(+Set, -Min)
%   is true when Min is the lower bound of Set.

fdset_min([[Min|_]|_], Min).

%   fdset_max(+Set, -Min)
%   is true when Max is the upper bound of Set.

fdset_max(Set, Max) :-
	last(Set, [_|Max]).

%   fdset_size(+Set, -Size)
%   is true when Size is the cardinality of Set.

fdset_size(Set, Size) :-
	var(Set), !,
	illarg(var, fdset_size(Set,Size), 1).
fdset_size(Set, Size) :-
	'$fd_size'(Set, Siz, 1), !,
	Size = Siz.
fdset_size(Set, Size) :-
	illarg(domain(term,fdset), fdset_size(Set,Size), 1).


%   list_to_fdset(+List, -Set)
%   is true when Set contains the elements of List.
%   Slightly more efficient if List is ordered.

list_to_fdset(List, Set) :-
	list_to_fdset(List, Intervals, []), !,
	fdset_union(Intervals, Set).
list_to_fdset(List, Set) :-
	illarg(domain(term,integer_list), list_to_fdset(List, Set), 1).

list_to_fdset(L) --> {var(L)}, !, {fail}.
list_to_fdset([]) --> [].
list_to_fdset([I|L]) --> {integer(I)},
	list_to_fdset(L, I, I).

list_to_fdset([I|L], Min, Max) -->
	{integer(I), I =:= Max+1}, !,
	list_to_fdset(L, Min, I).
list_to_fdset(L, Min, Max) --> [R],
	{'$fd_range'(Min, Max, R, 1)},
	list_to_fdset(L).


%   fdset_to_list(+Set, -List)
%   is true when List contains the elements of Set.
%   List will be ordered.

fdset_to_list(Set, List) :-
	fdset_to_list(Set, List, []), !.
fdset_to_list(Set, List) :-
	illarg(domain(term,finite_fdset), fdset_to_list(Set, List), 1).

fdset_to_list([]) --> [].
fdset_to_list([[B|E]|S]) -->
	{integer(B), integer(E)},
	fdset_to_list(B, E, S).

fdset_to_list(B, E, S) --> {B=<E}, !, [B],
	{B1 is B+1},
	fdset_to_list(B1, E, S).
fdset_to_list(_, _, S) -->
	fdset_to_list(S).


%   range_to_fdset(+Range, -Set)
%   is true when Set contains the elements of Range.

range_to_fdset(Range, Set) :-
	set_expression_check(Range, Set, range_to_fdset(Range,Set), 1).


%   fdset_to_range(+Set, -Range)
%   is true when Range contains the elements of Set.

fdset_to_range([], 1..0).
fdset_to_range([R|Set], Range) :-
	fd_range(R, R1),
	fd_range(Set, R1, Range).

fd_range([], Set, Set).
fd_range([R|Set0], Set1, Set) :-
	fd_range(R, R1),
	fd_range(Set0, Set1\/R1, Set).

fd_range([A|A], R) :- !, R = {A}.
fd_range([A|B], A..B).


%   fdset_add_element(+Set1, +Element -Set2)
%   is true when Set2 is Set1 with Element inserted in it.

fdset_add_element(Set1, Element, Set2) :-
	Goal = fdset_add_element(Set1, Element, Set2),
	must_be(Element, integer, Goal, 2),
	must_be_fdset(Set1, Goal, 1),
	'$fd_dom_insert'(Set1, Element, Set2).


%   fdset_del_element(+Set1, +Element -Set2)
%   is true when Set2 is Set1 with Element deleted from it.

fdset_del_element(Set1, Element, Set2) :-
	Goal = fdset_del_element(Set1, Element, Set2),
	must_be(Element, integer, Goal, 2),
	must_be_fdset(Set1, Goal, 1),
	'$fd_dom_delete'(Set1, Element, Set2).


%   fdset_disjoint(+Set1, +Set2)
%   is true when the two fdsets have no element in common.  

fdset_disjoint(Set1, Set2) :-
	Goal = fdset_disjoint(Set1, Set2),
	must_be_fdset(Set1, Goal, 1),
	must_be_fdset(Set2, Goal, 2),
	(   Set1==[] -> true
	;   Set2==[] -> true
	;   '$fd_dom_intersect'(Set1, Set2, 1)
	).



%   fdset_intersect(+Set1, +Set2)
%   is true when the two fdsets have at least one element in common.

fdset_intersect(Set1, Set2) :-
	Goal = fdset_intersect(Set1, Set2),
	must_be_fdset(Set1, Goal, 1),
	must_be_fdset(Set2, Goal, 2),
	(   Set1==[] -> fail
	;   Set2==[] -> fail
	;   '$fd_dom_intersect'(Set1, Set2, I),
	    I > 1
	).


%   fdset_intersection(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the intersecton of Set1
%   and Set2, provided that Set1 and Set2 are fdsets.

fdset_intersection(Set1, Set2, Intersection) :-
	Goal = fdset_intersection(Set1, Set2, Intersection),
	must_be_fdset(Set1, Goal, 1),
	must_be_fdset(Set2, Goal, 2),
	'$fd_dom_intersection'(Set1, Set2, Intersection).


%   fdset_intersection(+Sets, ?Intersection) 
%   is true when Intersection is the intersection of all the fdsets in Sets. 

fdset_intersection([], []) :- !.
fdset_intersection(L, D) :- length(L, N), fdset_intersection(N, D, L, []).

fdset_intersection(1, X) --> !, [X].
fdset_intersection(N, D) -->
	{A is N>>1, B is N-A},
	fdset_intersection(A, D1),
	fdset_intersection(B, D2),
	{'$fd_dom_intersection'(D1, D2, D)}.


%   fdset_member(?Elt, +Set)
%   is true when Elt is a member of Set.

fdset_member(Elt, Set) :-
	integer(Elt), !,
	must_be_fdset(Set, fdset_member(Elt, Set), 2),
	'$fd_dom_contains'(Set, Elt).
fdset_member(Elt, Set) :-
	var(Elt),
	must_be_finite_fdset(Set, fdset_member(Elt, Set), 2),
	member([A|B], Set),
	indomain_fast([], A, B, Elt).


%   fdset_eq(+Set1, +Set2)
%   is true when the two arguments represent the same set.  Since they
%   are assumed to be fdset representations, they must be identical.


fdset_eq(Set1, Set2) :-
	Set1 == Set2.


%   fdset_subset(+Set1, +Set2)
%   is true when every element of the fdset Set1 appears in the
%   fdset Set2.

fdset_subset(Set1, Set2) :-
	Goal = fdset_subset(Set1, Set2),
	must_be_fdset(Set1, Goal, 1),
	must_be_fdset(Set2, Goal, 2),
	'$fd_dom_intersect'(Set1, Set2, 2).


%   fdset_subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2, i.e. Set1 \ Set2.

fdset_subtract(Set1, Set2, Difference) :-
	Goal = fdset_subtract(Set1, Set2, Difference),
	must_be_fdset(Set1, Goal, 1),
	must_be_fdset(Set2, Goal, 2),
	'$fd_dom_subtract'(Set1, Set2, Difference).


%   fdset_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

fdset_union(Set1, Set2, Union) :-
	Goal = fdset_union(Set1, Set2, Union),
	must_be_fdset(Set1, Goal, 1),
	must_be_fdset(Set2, Goal, 2),
	'$fd_dom_union'(Set1, Set2, Union).


%   fdset_union(+Sets, ?Union) 
%   is true when Union is the union of all the fdsets in Sets. 

fdset_union([], []) :- !.
fdset_union(L, D) :- length(L, N), fdset_union(N, D, L, []).

fdset_union(1, X) --> !, [X].
fdset_union(N, D) -->
	{A is N>>1, B is N-A},
	fdset_union(A, D1),
	fdset_union(B, D2),
	{'$fd_dom_union'(D1, D2, D)}.


%   fdset_complement(+Set1, ?Complement)
%   is true when Complement is the complement of Set1.

fdset_complement(Set1, Complement) :-
	Goal = fdset_complement(Set1, Complement),
	must_be_fdset(Set1, Goal, 1),
	'$fd_dom_complement'(Set1, Complement).


end_of_file.

/*
Domain = list of intervals
Domain structure = dom(Domain, Min, Max, Size)
*/

interval_vs_interval([B1|E1], R2, RR) :-
	point_vs_interval(B1, R2, PR),
	interval_vs_interval(PR, B1, E1, R2, RR).

interval_vs_interval(before, _, E1, R2, RR) :-
	point_vs_interval(E1, R2, PR),
	interval_vs_interval_before(PR, E1, R2, RR).
interval_vs_interval(in, B1, E1, R2, RR) :-
	R2 = [B1|_], !,
	point_vs_interval(E1, R2, PR),
	interval_vs_interval_starts(PR, E1, R2, RR).
interval_vs_interval(in, _, E1, R2, RR) :-
	point_vs_interval(E1, R2, PR),
	interval_vs_interval_in(PR, E1, R2, RR).
interval_vs_interval(after, _, _, _, after).		% or met_by

interval_vs_interval_before(before, _, _, before).	% or meets
interval_vs_interval_before(in, E, [_|E], finished_by) :- !.
interval_vs_interval_before(in, _, _, overlaps).
interval_vs_interval_before(after, _, _, contains).

interval_vs_interval_starts(in, E, [_|E], equals) :- !.
interval_vs_interval_starts(in, _, _, starts).
interval_vs_interval_starts(after, _, _, started_by).

interval_vs_interval_in(in, E, [_|E], finishes) :- !.
interval_vs_interval_in(in, _, _, during).
interval_vs_interval_in(after, _, _, overlapped_by).

point_vs_interval(X, [B|E], Rel) :-
        (   lt(X, B) -> Rel = before
        ;   lt(E, X) -> Rel = after
        ;   Rel = in
        ).

lt(inf, B) :- B \== inf.
lt(A, sup) :- A \== sup.
lt(A, B) :- integer(A), integer(B), A<B.

dom_union([], D, D).
dom_union([R1|D1], D2, D) :- dom_union(D2, R1, D1, D).

dom_union([], R, D, [R|D]).
dom_union([R1|D1], R2, D2, D) :-
        R1 = [B|_],
        point_vs_interval(B, R2, C),
        dom_union(C, R1, D1, R2, D2, D).

dom_union(after, R2, D2, [B|E], D1, [[B|E1]|D]) :-
        R2 = [B2|_],
        B2 =:= E+1, !,
        point_vs_interval(E, R2, C),
        dom_union_extend(C, E, D1, R2, D2, E1, D).
dom_union(after, R1, D1, R2, D2, [R2|D]) :-
        dom_union(D2, R1, D1, D).
dom_union(before, [B|E], D1, R2, D2, [[B|E1]|D]) :-
        point_vs_interval(E, R2, C),
        dom_union_extend(C, E, D1, R2, D2, E1, D).
dom_union(in, R2, D2, [B|E], D1, [[B|E1]|D]) :-
        point_vs_interval(E, R2, C),
        dom_union_extend(C, E, D1, R2, D2, E1, D).

dom_union_extend(before, B, D1, [C|E], D2, E1, D) :-
        C =:= B+1, !,
        dom_union_extend(D1, E, D2, E1, D).
dom_union_extend(before, E, D1, R2, D2, E, D) :-
        dom_union(D1, R2, D2, D).
dom_union_extend(in, _, D1, [_|E], D2, E1, D) :-
        dom_union_extend(D1, E, D2, E1, D).
dom_union_extend(after, E, D2, _, D1, E1, D) :-
        dom_union_extend(D1, E, D2, E1, D).

dom_union_extend([], E, D, E, D).
dom_union_extend([R2|D2], E, D1, E1, D) :-
        point_vs_interval(E, R2, C),
        dom_union_extend(C, E, D1, R2, D2, E1, D).

/********/


dom_intersection([], _, []).
dom_intersection([R1|D1], D2, D) :- dom_intersection(D2, R1, D1, D).

dom_intersection([], _, _, []).
dom_intersection([R1|D1], R2, D2, D) :-
        R1 = [B|_],
        point_vs_interval(B, R2, C),
        dom_intersection(C, R1, D1, R2, D2, D).

dom_intersection(after, R1, D1, _, D2, D) :-
        dom_intersection(D2, R1, D1, D).
dom_intersection(before, R1, D1, R2, D2, D) :-
        R1 = [_|E],
        point_vs_interval(E, R2, C),
        dom_intersection_extend(C, R1, D1, R2, D2, D).
dom_intersection(in, R2, D2, R1, D1, D) :-
        R1 = [_|E],
        point_vs_interval(E, R2, C),
        dom_intersection_extend(C, R1, D1, R2, D2, D).

dom_intersection_extend(before, _, D1, R2, D2, D) :-
        dom_intersection(D1, R2, D2, D).
dom_intersection_extend(in, [_|E], D1, R2, D2, [[B|E]|D]) :-
        R2 = [B|_],
        dom_intersection(D1, R2, D2, D).
dom_intersection_extend(after, R1, D1, R2, D2, [R2|D]) :-
        dom_intersection(D2, R1, D1, D).

/********/

dom_subtract([], _, []).
dom_subtract([R1|D1], D2, D) :- dom_subtract(D2, R1, D1, D).

dom_subtract([], R1, D1, [R1|D1]).
dom_subtract([R2|D2], R1, D1, D) :-
	interval_vs_interval(R2, R1, C),
	dom_subtract(C, R2, D2, R1, D1, D).

dom_subtract_cont([], _, _, []).
dom_subtract_cont([R1|D1], R2, D2, D) :-
	interval_vs_interval(R2, R1, C),
	dom_subtract(C, R2, D2, R1, D1, D).

dom_subtract(before, _, D2, R1, D1, D) :-	% or meets
	dom_subtract(D2, R1, D1, D).
dom_subtract(overlaps, [_|E2], D2, [_|E1], D1, D) :-
	B1 is E2+1,
	dom_subtract(D2, [B1|E1], D1, D).
dom_subtract(finished_by, _, D2, _, D1, D) :-
	dom_subtract(D1, D2, D).
dom_subtract(contains, R2, D2, _, D1, D) :-
	dom_subtract_cont(D1, R2, D2, D).
dom_subtract(starts, [_|E2], D2, [_|E1], D1, D) :-
	B1 is E2+1,
	dom_subtract(D2, [B1|E1], D1, D).
dom_subtract(equals, _, D2, _, D1, D) :-
	dom_subtract(D1, D2, D).
dom_subtract(started_by, R2, D2, _, D1, D) :-
	dom_subtract_cont(D1, R2, D2, D).
dom_subtract(during, [B2|E2], D2, [B1|E1], D1, [[B1|E3]|D]) :-
	E3 is B2-1,
	B3 is E2+1,
	dom_subtract(D2, [B3|E1], D1, D).
dom_subtract(finishes, [B2|_], D2, [B1|_], D1, [[B1|E3]|D]) :-
	E3 is B2-1,
	dom_subtract(D1, D2, D).
dom_subtract(overlapped_by, [B2|E2], D2, [B1|_], D1, [[B1|E3]|D]) :-
	E3 is B2-1,
	dom_subtract_cont(D1, [B2|E2], D2, D).
dom_subtract(after, R2, D2, R1, D1, [R1|D]) :-
	dom_subtract_cont(D1, R2, D2, D).

/********/

dom_intersect(D1, D2, X) :-
        dom_intersect(D1, D2, none, X).

dom_intersect([[B|E]|D1], [R2|D2], X0, X) :- !,
        point_vs_interval(B, R2, C),
        dom_intersect(C, B, E, D1, R2, D2, X0, X).
dom_intersect(_, _, X, X).

dom_intersect(before, _, E, D1, R2, D2, X0, X) :-
        X0\==subset,
        point_vs_interval(E, R2, before), !,
        dom_intersect(D1, [R2|D2], disjoint, X).
dom_intersect(in, _, E, D1, R2, D2, X0, X) :-
        X0\==disjoint,
        point_vs_interval(E, R2, in), !,
        dom_intersect(D1, [R2|D2], subset, X).
dom_intersect(after, _, _, _, _, [], X0, X) :-
        X0\==subset, !, X = disjoint.
dom_intersect(after, B, E, D1, _, [R2|D2], X0, X) :- !,
        dom_intersect([[B|E]|D1], [R2|D2], X0, X).
dom_intersect(_, _, _, _, _, _, _, intersect).

/********/

% Set vs. Subset
dom_compare(_, [[B|B]], B, B, 1, 31) :- !.
dom_compare([R1|D1], [R2|D2], Min, Max, Card, Why) :-
        R1 = [B|_],
        R2 = [Min|E2],
        ne(Min, B, 3, 0, Why0),
        point_vs_interval(E2, R1, C),
        dom_compare(C, R1, D1, R2, D2, Max, 0, Card, Why0, Why).

dom_compare(after, _, [R1|D1], R2, D2, Max, Card0, Card, Why0, Why):-
        Why1 is Why0\/1,
        R2 = [_|E2],
        point_vs_interval(E2, R1, C),
        dom_compare(C, R1, D1, R2, D2, Max, Card0, Card, Why1, Why).
dom_compare(in, [B1|E], [R1|D1], [B|E], [R2|D2], Max, Card0, Card, Why0, Why) :- !,
        card(B, E, Card0, Card1),
        ne(B1, B, 1, Why0, Why1),
        R2 = [_|E2],
        point_vs_interval(E2, R1, C),
        dom_compare(C, R1, D1, R2, D2, Max, Card1, Card, Why1, Why).
dom_compare(in, R1, D1, [B|E], [R2|D2], Max, Card0, Card, Why0, Why) :- !,
        card(B, E, Card0, Card1),
        ne(R1, [B|E], 1, Why0, Why1),
        R2 = [_|E2],
        point_vs_interval(E2, R1, C),
        dom_compare(C, R1, D1, R2, D2, Max, Card1, Card, Why1, Why).
dom_compare(in, [B1|E1], D1, [B2|Max], [], Max, Card0, Card, Why0, Why) :-
        card(B2, Max, Card0, Card),
        ne(B1, B2, 1, Why0, Why1),
        ne(E1, Max, 5, Why1, Why2),
        ne(D1, [], 5, Why2, Why).

ne(X, Y, N, Why0, Why) :- X\==Y, !, Why is Why0\/N.
ne(_, _, _, Why, Why).

card(inf, _, _, sup) :- !.
card(_, sup, _, sup) :- !.
card(_, _, sup, sup) :- !.
card(B, E, C0, C) :- C is C0+E-B+1.


/********/



dom_complement([], [[inf|sup]]).
dom_complement([R|D], D0) :-
        R = [A|Z],
        dom_compl_header(A, D0, D1),
        dom_compl(D, Z, Last, D1, D2),
        dom_compl_trailer(Last, D2, []).

dom_compl_header(inf) --> !.
dom_compl_header(B) --> {A is B-1}, [[inf|A]].

dom_compl([], Last, Last) --> [].
dom_compl([[F|G]|D], A, Last) --> [[B|C]],
        {B is A+1, C is F-1},
        dom_compl(D, G, Last).

dom_compl_trailer(sup) --> !.
dom_compl_trailer(A) --> {B is A+1}, [[B|sup]].


/********/


dom_member(X, [R|D]) :-
        point_vs_interval(X, R, C),
        dom_member(C, X, D).

dom_member(in, _, _).
dom_member(after, X, D) :- dom_member(X, D).

/********/


data(0, [.(inf,1), .(5,11), .(15,21), .(25,28), .(35,sup)]).
data(1, [.(1,5), .(11,15), .(21,25), .(28,35)]).
data(2, [.(3,3), .(5,6), .(10,11), .(16,17), .(20,21), .(25,31), .(35,sup)]).

end_of_file.


/* Program scheme for function on domains. */

static TAGGED fd_generic(d1,d2)
     TAGGED d1, d2;
{
  TAGGED r1, r2, b, e, *h;
  TAGGED value;
  TAGGED *valuep = &value;

  goto bump21;
  
bump2:
  if (d2==EmptySet)
    {
      EMIT_RANGE(h,r1);
      *valuep = d1;
      return value;
    }
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
  goto bump;
bump21:
  if (d2==EmptySet)
    {
      *valuep = d1;
      return value;
    }
  r2 = CTagToCar(d2);  d2 = CTagToCdr(d2);
bump1:
  if (d1==EmptySet)
    {
      EMIT_RANGE(h,r2);
      *valuep = d2;
      return value;
    }
  r1 = CTagToCar(d1);  d1 = CTagToCdr(d1);
  b = RangeMin(r1);
  e = RangeMax(r1);
bump:
  if (b==RangeMin(r2))
    {			/* r1 starts|equals|started_by r2 */
      if (e==RangeMax(r2))	/* equals */
	;
      else if (point_vs_range(e,r2)==CMP_INSIDE) /* starts */
	;
      else			/* started_by */
	;
    }
  else switch (point_vs_range(b,r2))
    {
    case CMP_BEFORE:	/* r1 before|meets|overlaps|finished_by|contains r2 */
      if (e==RangeMax(r2))	/* finished_by */
	;
      else switch (point_vs_range(e,r2))
	{
	case CMP_BEFORE:	/* before|meets */
	  ;
	case CMP_INSIDE:	/* overlaps */
	  ;
	case CMP_AFTER:		/* contains */
	  ;
	}
      break;
    case CMP_INSIDE:	/* r1 during|finishes|overlapped_by r2 */
      if (e==RangeMax(r2))	/* finishes */
	;
      else if (point_vs_range(e,r2)==CMP_INSIDE) /* during */
	;
      else			/* overlapped_by */
	;
      break;
    default:			/* r1 met_by|after r2 */
    }
end1:
  *valuep = d2;
  return value;
}
