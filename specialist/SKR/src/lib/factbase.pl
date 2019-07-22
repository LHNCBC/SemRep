% File:     factbase.pl
% Module:   Factbase
% Author:   Lan
% Purpose:  Provides for the storage, retrieval and management of facts
%           characterized by having a Key, Property and Value each of which
%           is an arbitrary term (atomic Keys are preferred for efficiency).


/* Factbase Module
*/

:- module(factbase,[
    put_fact/3,
    get_fact/3,
    erase_fact/3,
    erase_all_facts/2
    ]).


:- dynamic 'FactBase'/3.


/* put_fact(+Key, +Property, +Value)
*/

put_fact(Key,Property,Value) :-
    assertz('FactBase'(Key,Property,Value)).

/* get_fact(+Key, ?Property, ?Value)
   get_set_of_all_values(+Key, +Property, -ValueSet)
*/

get_fact(Key,Property,Value) :-
    'FactBase'(Key,Property,Value).

/* 
   erase_all_facts(+Key, +Property)
*/

erase_all_facts(Key,Property) :-
    retractall('FactBase'(Key,Property,_)),
    !.

/* erase_fact(+Key, +Property, ?Value)
*/

erase_fact(Key,Property,Value) :-
    retract(('FactBase'(Key,Property,Value) :- true)),
    !.

