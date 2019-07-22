
% File:	    metamap_semantics.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  MetaMap semantics


:- module(metamap_semantics,[
    is_semantically_consistent/1
    ]).


:- multifile sccs_id/1.
:- dynamic sccs_id/1.

sccs_id('@(#)metamap_semantics.pl	1.1 09/14/06').


:- use_module(skr_lib(semnet_access06),[
    type_relation_type/3
    ]).

:- use_module(skr_lib(semtype_translation06),[
    expand_semtypes/2
    ]).

:- use_module(library(sets),[
    setproduct/3
    ]).



/* ************************************************************************
   ************************************************************************
   ************************************************************************
                       MetaMap Semantics X Predicates
   ************************************************************************
   ************************************************************************
   ************************************************************************ */


/* is_semantically_consistent(+Mapping)
   are_semantically_consistent(+Evaluation1, +Evaluation2)
   are_semantically_consistent(+Concept1, +SemTypes1, +Concept2, +SemTypes2)
   contains_semantically_consistent_pair(+SemTypeProduct)

is_semantically_consistent/1 checks to make sure that the adjacent elements
of Mapping have a potential relationship according to the semantic network.
are_semantically_consistent/2,3 are auxiliaries.  */

is_semantically_consistent([]) :- !.
is_semantically_consistent([_]) :- !.
is_semantically_consistent([FirstEv,SecondEv|Rest]) :-
    are_semantically_consistent(FirstEv,SecondEv),
    is_semantically_consistent([SecondEv|Rest]).

are_semantically_consistent(ev(_,_,_,Concept1,_,SemTypes1,_,_,_),
                            ev(_,_,_,Concept2,_,SemTypes2,_,_,_)) :-
    are_semantically_consistent(Concept1,SemTypes1,Concept2,SemTypes2).

are_semantically_consistent(Concept1,SemTypes1,Concept2,SemTypes2) :-
    setproduct(SemTypes1,SemTypes2,SemTypeProduct),
    (contains_semantically_consistent_pair(SemTypeProduct) ->
        true
    ;   expand_semtypes(SemTypes1,Exp1),
        expand_semtypes(SemTypes2,Exp2),
        format('Semantically unrelated:~n  ~p ~p~n  ~p ~p~n',
               [Concept1,Exp1,Concept2,Exp2]),
        !,
        fail
    ).

contains_semantically_consistent_pair([]) :-
    !,
    fail.
contains_semantically_consistent_pair([SemType1-SemType2|_Rest]) :-
    type_relation_type(SemType1,_Relation,SemType2),
    !.
contains_semantically_consistent_pair([_First|Rest]) :-
    contains_semantically_consistent_pair(Rest).

