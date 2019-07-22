/* Copyright(C) 1988, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Name: lists.pl                                                             %
%  Maintainer: Lena Flood                                                     %
%  Date: 4 November 1988                                                      %
%  Purpose: List manipulating routines                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%   Adapted from shared code written by Lawrence Byrd & Richard A O'Keefe      

:- module(lists3, [
        delete/3,
        is_list/1,
        last/2,
        nextto/3,
        no_doubles/1,
        non_member/2,
        nth/3,
        nth/4,
        nth0/3,
        nth0/4,
        permutation/2,
        prefix/2,
        remove_duplicates/2,
        reverse/2,
        same_length/2,
        same_length/3,
        select/3,
        sublist/2,
        substitute/4,
        suffix/2,
        max_list/2,
        min_list/2,
        sum_list/2
	]).

:- use_module(library(lists), [
        is_list/1,
        last/2,
        nextto/3,
        nth1/3,
        nth1/4,
        nth0/3,
        nth0/4,
        reverse/2,
        same_length/2,
        same_length/3,
        select/3,
        max_member/2,
        min_member/2,
        sumlist/2
	]).


%   delete(+List, +Element, ?Residue)
%   is true when all *identical* occurences of Element in List are removed 
%   and the result is Residue.  

delete([], _, []).
delete([Head|Tail], Element, Rest) :-
	Head==Element, !,
	delete(Tail, Element, Rest).
delete([Head|Tail], Element, [Head|Rest]) :-
	delete(Tail, Element, Rest).


%   no_doubles(+List) 
%   is true when the List contains no duplicate elements.

no_doubles([]).
no_doubles([Head|Tail]) :-
	non_member_(Tail, Head),
	no_doubles(Tail).


%   non_member(+Element, +List)
%   non_member is true when Element does not exist in List.

non_member(Element, List) :-
	non_member_(List, Element).


non_member_([], _).
non_member_([Head|Tail], Element) :-
	dif(Head, Element),
	non_member_(Tail, Element).

nth(N, List, Element) :-
	nth1(N, List, Element).


nth(N, List, Element, Rest) :-
        nth1(N, List, Element, Rest).


%   permutation(?List, ?Perm)
%   is true when Perm is a permutation of List.
:- permutation(?,?) is nondet.
permutation([], []).
permutation(List, [First|Perm]) :- 
	select(First, List, Rest),
	permutation(Rest, Perm).


%   prefix(?Prefix, +List)
%   is true when Prefix is a prefix of List.

prefix([], _).
prefix([X|PreTail], [X|Tail]) :-
	prefix(PreTail, Tail).


%   remove_duplicates(+List, ?Pruned)
%   is true when Pruned is like List but with all *identical* duplicate 
%   elements removed.

remove_duplicates([], []).
remove_duplicates([Head|Tail1], [Head|Tail2]) :- 
	delete(Tail1, Head, Residue),
        remove_duplicates(Residue, Tail2).


%   sublist(?Sub, +List)
%   is true when all members of Sub are members of List
:- sublist(?,+) is nondet.
sublist(List, List).
sublist(Sub, [Head|Tail]) :- sublist_(Tail, Head, Sub).

:- sublist_/3 is nondet.
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


%   suffix(?Suffix, +List)
%   is true Suffix is an ending part of List.
:- suffix(?,+) is nondet.
suffix(Suffix, Suffix).
suffix(X, [_|Tail]) :-
	suffix(X, Tail).

max_list(ListOfNumbers, Max) :-
	max_member(Max, ListOfNumbers).

min_list(ListOfNumbers, Min) :-
	min_member(Min, ListOfNumbers).

sum_list(ListOfNumbers, Sum) :-
	sumlist(ListOfNumbers, Sum).

