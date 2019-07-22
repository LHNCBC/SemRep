/*
The name of this file is a historical accident.
It contains a predicates that at some point were in hProlog, but not in SWI,
plus predicates that have been factored out here.

The SWI distribution contains a file with the same name, but its
contents is different.

Bart Demoen
K.U.Leuven
Wed Jan 18 13:07:00 CET 2006
*/

:- module(hprolog,
	  [ memberchk_eq/2,		% +Val, +List
	    intersect_eq/3,		% +List1, +List2, -Intersection
	    list_difference_eq/3,	% +List, -Subtract, -Rest
	    take/3,			% +N, +List, -FirstElements
	    or_list/2,			% +ListOfInts, -BitwiseOr
	    flatten/2,
	    chr_delete/3,
	    nb_setval/2,
	    nb_getval/2,
	    nb_current/2,
	    nb_delete/1,
            init_store/2,
	    get_store/2,
	    update_store/2,
            make_get_store_goal/3,
	    make_update_store_goal/3,
	    make_init_store_goal/3,
	    chr_term_variables/2,
	    chr_term_variables/3,
	    predsort/3,
	    sublist/2,
	    substitute/4,
	    ord_empty/1,
	    ord_memberchk/2,
	    ord_add_element/3,
	    ord_del_element/3,
	    ord_intersection/3,
	    ord_subset/2,
	    ord_subtract/3,
	    list_to_ord_set/2,
	    empty_ds/1,
	    ds_to_list/2,
	    get_ds/3,
	    put_ds/4,
	    max_go_list/2,
	    min_list/2,
	    nth1/3,
	    maplist/2
	    append/2,
	    reverse/2,
	    permutation/2,
	    delete/3,
	    is_list/1,
	    last/2,
	    append/2,
	    append/3,
	    member/2,
	    memberchk/2,
	    illarg/4
	  ]).


:- meta_predicate predsort(:,?,?).

:- use_module(library(lists),[append/3,nth/3,reverse/2,permutation/2,is_list/1,last/2,append/3,member/2,memberchk/2]).
:- use_module(library(terms),[term_variables/2]).
:- use_module(b_globval).
:- use_module(library(assoc)).
:- use_module(library(ordsets)).

illarg(A,B,C,D) :- prolog:illarg(A,B,C,D).

empty_ds(DS) :- empty_assoc(DS).
ds_to_list(DS,LIST) :- assoc_to_list(DS,LIST).
get_ds(A,B,C) :- get_assoc(A,B,C).
put_ds(A,B,C,D) :- put_assoc(A,B,C,D).


:- dynamic init_store_/2.

init_store(Name,Value) :- % to be postponed in SICStus
	(
	  retract(init_store_(Name,_)), fail
	;
	  asserta(init_store_(Name,Value))
	).

get_store(Name,Value) :-
	b_getval(Name,Value), !.
get_store(Name,Value) :-
	init_store_(Name,Value), !,
	b_setval(Name,Value).
get_store(Name,_) :-
	error(attempt2get_from_non_initted_store(Name)).

update_store(Name,Value) :-
	b_setval(Name,Value).


make_init_store_goal(Name,Value,Goal) :- Goal = (hprolog:init_store(Name,Value)).

make_get_store_goal(Name,Value,Goal) :- Goal = (hprolog:get_store(Name,Value)).

make_update_store_goal(Name,Value,Goal) :- Goal = (hprolog:update_store(Name,Value)).

                 /*********************************
                 *    max and min in lists        *
		 *********************************/

%       max_go_list(+List, -Max)
%
%       Return the maximum of List in the standard order of terms.

max_go_list([H|T], Max) :-
	max_go_list(T, H, Max).

max_go_list([], Max, Max).
max_go_list([H|T], X, Max) :-
	(   H @=< X
	->  max_go_list(T, X, Max)
	;   max_go_list(T, H, Max)
	).


min_list([H|T], Min) :-
	'$min_list1'(T, H, Min).

'$min_list1'([], Min, Min).
'$min_list1'([H|T], X, Min) :-
	(   H>=X ->
	    '$min_list1'(T, X, Min)
	    ;   '$min_list1'(T, H, Min)
	).


		 /*********************************
		 *      non-backtrackable globals *
		 *********************************/

nb_setval(A,B) :- bb_put(A,B).
nb_getval(A,B) :- bb_get(A,B).
nb_current(A,B) :- bb_get(A,B).
nb_delete(A) :- bb_put(A,0).       % not quite with the same meaning as in SWI 



		 /*******************************
		 *      MORE LIST OPERATIONS	*
		 *******************************/

%	memberchk_eq(+Val, +List)
%	
%	Deterministic check of membership using == rather than
%	unification.

memberchk_eq(X, [Y|Ys]) :-
   (   X == Y
   ->  true
   ;   memberchk_eq(X, Ys)
   ).


%	list_difference_eq(+List, -Subtract, -Rest)
%	
%	Delete all elements of Subtract from List and unify the result
%	with Rest.  Element comparision is done using ==/2.

list_difference_eq([],_,[]).
list_difference_eq([X|Xs],Ys,L) :-
	(   memberchk_eq(X,Ys)
	->  list_difference_eq(Xs,Ys,L)
	;   L = [X|T],
	    list_difference_eq(Xs,Ys,T)
	).

%	intersect_eq(+List1, +List2, -Intersection)
%	
%	Determine the intersection of two lists without unifying values.

intersect_eq([], _, []).
intersect_eq([X|Xs], Ys, L) :-
	(   memberchk_eq(X, Ys)
	->  L = [X|T],
	    intersect_eq(Xs, Ys, T)
	;   intersect_eq(Xs, Ys, L)
	).


%	take(+N, +List, -FirstElements)
%	
%	Take the first  N  elements  from   List  and  unify  this  with
%	FirstElements. The definition is based   on the GNU-Prolog lists
%	library. Implementation by Jan Wielemaker.

take(0, _, []) :- !.
take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).


%	or_list(+ListOfInts, -BitwiseOr)
%	
%	Do a bitwise disjuction over all integer members of ListOfInts.

or_list(L, Or) :-
	or_list(L, 0, Or).

or_list([], Or, Or).
or_list([H|T], Or0, Or) :-
	Or1 is H \/ Or0,
	or_list(T, Or1, Or).


% taken literally from SWI Prolog
%       flatten(+List1, ?List2)
%
%       Is true when Lis2 is a non nested version of List1.

flatten(List, FlatList) :-
	flatten(List, [], FlatList0), !,
	FlatList = FlatList0.

flatten(Var, Tl, [Var|Tl]) :-
	var(Var), !.
flatten([], Tl, Tl) :- !.
flatten([Hd|Tl], Tail, List) :-
	flatten(Hd, FlatHeadTail, List),
	flatten(Tl, Tail, FlatHeadTail).
flatten(Atom, Tl, [Atom|Tl]).

		 
		 
chr_delete([], _, []).
chr_delete([H|T], X, L) :-
        (   H==X ->
            chr_delete(T, X, L)
        ;   L=[H|RT],
            chr_delete(T, X, RT)
        ).


%%%%%%%%                                                            |maplist(Pred,List) :- maplist2(List,Pred).

:- meta_predicate maplist(:,?).

maplist(Pred,List) :- maplist2(List,Pred).

maplist2([],_).
maplist2([X|R],Pred) :-
	call1(Pred,X),
	maplist2(R,Pred).

call1(Mod:Term,X) :-
	Term =.. [Name,A],
	Goal =.. [Name,A,X],
	call(Mod:Goal).


    
%%%%%%%%

predsort(Pred,In,Out) :-
	psort(In,Pred,Out,[]).

psort([],_,O,O).
psort([X|R],P,Out,Tail) :-
	split(R,X,P,Smaller,Larger),
	psort(Smaller,P,Out,[X|SmallerTail]),
	psort(Larger,P,SmallerTail,Tail).

split([],_,_,[],[]).
split([Y|R],X,P,Smaller,Larger) :-
	atomcall(P,Result,Y,X),
	(
	  Result == (<) ->
	  Smaller = [Y|TSmaller],
	  split(R,X,P,TSmaller,Larger)
	;
	  Result == (>) ->
	  Larger = [Y|TLarger],
	  split(R,X,P,Smaller,TLarger)
	;
	  split(R,X,P,Smaller,Larger)
	).

atomcall(Mod:Name,A,B,C) :-
	G =.. [Name,A,B,C],
	call(Mod:G).

%%%%%%%%%%%%%%%%%%%%%%%

chr_term_variables(Term,Vars) :-
	term_variables(Term,Vars).

chr_term_variables(Term,Vars,Tail) :-
	chr_term_variables(Term,TVars),
	append(TVars,Tail,Vars).

%   sublist(?Sub, +List)
%   is true when all members of Sub are members of List

sublist(List, List).
sublist(Sub, [Head|Tail]) :- sublist_(Tail, Head, Sub).

sublist_(Sub, _, Sub).
sublist_([Head|Tail], _, Sub) :- sublist_(Tail, Head, Sub).
sublist_([Head|Tail], X, [X|Sub]) :- sublist_(Tail, Head, Sub).


%   substitute(?X, ?Xlist, ?Y, ?Ylist)
%   is true when Xlist and Ylist are identical lists except for the 
%   corresponding elements X and Y.


substitute(_, [], _, []) :- !.
substitute(OldElem, [OldHead|OldRest], NewElem, [NewElem|NewRest]) :-
	OldElem==OldHead, !,
	substitute(OldElem, OldRest, NewElem, NewRest).
substitute(OldElem, [NotElem|OldRest], NewElem, [NotElem|NewRest]) :-
	substitute(OldElem, OldRest, NewElem, NewRest).


ord_empty([]).

ord_memberchk(X,Y) :- ord_member(X,Y).

%%%%%%%%%%%%%%%%%%%%%%%

nth1(N,L,O) :- nth(N,L,O).

append([],[]).
append([X|Xs],L) :-
	append(X,T,L),
	append(Xs,T).

delete([], _, []).
delete([Kill|Tail], Kill, Residue) :- !,
	delete(Tail, Kill, Residue).
delete([Head|Tail], Kill, [Head|Residue]) :-
    %	Head \= Kill,
	delete(Tail, Kill, Residue).
