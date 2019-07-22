%   Copyright(C) 1994, Swedish Institute of Computer Science

%   File       : ASSOC.PL
%   Maintainer : Mats Carlsson
%   Updated    : 15 December 1994
%   Purpose    : AVL tree implementation of "association lists".

:- module(assoc3, [
	assoc_to_list/2,		% Assoc -> List
	empty_assoc/1,			% -> Assoc
	del_assoc/4,			% Key x Assoc x Val -> Assoc
	del_max_assoc/4,		% Assoc -> Key x Val x Assoc
	del_min_assoc/4,		% Assoc -> Key x Val x Assoc
	gen_assoc/3,			% Key x Assoc x Val
	get_assoc/3,			% Key x Assoc -> Val
	get_assoc/5,			% Key x Assoc x Val -> Assoc x Val
	get_next_assoc/4,		% Key x Assoc -> Key x Val
	get_prev_assoc/4,		% Key x Assoc -> Key x Val
	is_assoc/1,			% Assoc ->
	list_to_assoc/2,		% List -> Assoc
	map_assoc/2,			% Goal x Assoc ->
	map_assoc/3,			% Goal x Assoc -> Assoc
	max_assoc/3,			% Assoc -> Key x Val
	min_assoc/3,			% Assoc -> Key x Val
	ord_list_to_assoc/2,		% List -> Assoc
	put_assoc/4			% Key x Assoc x Val -> Assoc
   ]).

:- meta_predicate
	map_assoc(1, ?),
	map_assoc(2, ?, ?).

:- use_module(library(avl), [
	avl_to_list/2,
	empty_avl/1,
	avl_delete/4,
	avl_del_max/4,
	avl_del_min/4,
	avl_member/3,
	avl_fetch/3,
	avl_change/5,
	avl_next/4,
	avl_prev/4,
	is_avl/1,
	list_to_avl/2,
	avl_map/2,
	avl_map/3,
	avl_max/3,
	avl_min/3,
	ord_list_to_avl/2,
	avl_store/4
   ]).

assoc_to_list(Assoc, List) :-
	avl_to_list(Assoc, List).
empty_assoc(Assoc) :-
	empty_avl(Assoc).
del_assoc(Key, Assoc1, Val, Assoc2) :-
	avl_delete(Key, Assoc1, Val, Assoc2).
del_max_assoc(Assoc1, Key, Val, Assoc2) :-
	avl_del_max(Assoc1, Key, Val, Assoc2).
del_min_assoc(Assoc1, Key, Val, Assoc2) :-
	avl_del_min(Assoc1, Key, Val, Assoc2).
gen_assoc(Key, Assoc, Val) :-
	avl_member(Key, Assoc, Val).
get_assoc(Key, Assoc, Val) :-
	avl_fetch(Key, Assoc, Val).
get_assoc(Key, Assoc1, Val1, Assoc2, Val2) :-
	avl_change(Key, Assoc1, Val1, Assoc2, Val2).
get_next_assoc(Key1, Assoc, Key2, Val) :-
	avl_next(Key1, Assoc, Key2, Val).
get_prev_assoc(Key1, Assoc, Key2, Val) :-
	avl_prev(Key1, Assoc, Key2, Val).
is_assoc(Assoc) :-
	is_avl(Assoc).
list_to_assoc(List, Assoc) :-
	list_to_avl(List, Assoc).
map_assoc(Goal, Assoc) :-
	avl_map(Goal, Assoc).
map_assoc(Goal, Assoc1, Assoc2) :-
	avl_map(Goal, Assoc1, Assoc2).
max_assoc(Assoc, Key, Val) :-
	avl_max(Assoc, Key, Val).
min_assoc(Assoc, Key, Val) :-
	avl_min(Assoc, Key, Val).
ord_list_to_assoc(List, Assoc) :-
	ord_list_to_avl(List, Assoc).
put_assoc(Key, Assoc1, Val, Assoc2) :-
	avl_store(Key, Assoc1, Val, Assoc2).
