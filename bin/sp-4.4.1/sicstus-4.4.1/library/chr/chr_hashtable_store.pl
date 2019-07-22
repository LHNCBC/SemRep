/*
This module offers the predicates for a hashtable datatructure for the
constraint store.
The predicates needed are the exported ones.

In a Prolog system with functors of arbitrary arity like SWI, this is
done relying in such functors.

Otherwise, one needs to provide an alternative ...

Below is a simple implementation with the correct functionality, but
for sure very inefficient: replace this module at will ...

A hashtable is an association between keys and values: one key can
have many values associated to it. Keys are ground "enough" (if real
hashing is used, one uses hash_term (or term_hash) on them).
Values and keys are compared for equality (==/2).


new_ht(-hashtable)
	creates an empty hashtable

lookup_ht(+hashtable,+key,?valuelist)
	finds in hashtable the values associated to key;
	assume no order on the values in the list

value_ht(+hashtable,value)
	retrieves by backtracking all values in the hashtable

insert_ht(+hashtable,+key,?value)
	adds the association key-value in the hashtable;
	if the key was there already there with the value associated to it,
	   nothing changes to the hashtable (no duplicates)

delete_ht(+hashtable,+key,?value)
	deletes the association key-value from the hashtable, if it was there;
	otherwise nothing changes

Documentation and list-based implementation by
Bart Demoen
K.U.Leuven
Wed Jan 18 12:38:01 CET 2006

New implementation based on assoc by Tom Schrijvers
*/

:- module(chr_hashtable_store,
	  [ new_ht/1,
	    lookup_ht/3,
	    insert_ht/3,
	    delete_ht/3,
	    value_ht/2
	  ]).

:- use_module( library(assoc3)).

new_ht(HT) :- 
	empty_assoc(Assoc),
 	create_mutable(Assoc,HT).

lookup_ht(HT,B,C) :- 
	get_mutable(Assoc,HT),
	get_assoc(B,Assoc,C).

insert_ht(HT,B,C) :-
	get_mutable(Assoc,HT),
	( get_assoc(B,Assoc,V) ->
	    put_assoc(B,Assoc,[C|V],NewAssoc)
	  ; put_assoc(B,Assoc,[C],NewAssoc)
	),
	update_mutable(NewAssoc,HT).	

delete_ht(HT,B,C) :-
	get_mutable(Assoc,HT),
	( get_assoc(B,Assoc,V) ->
	    delete_first_fail(V,C,NewV),
	    put_assoc(B,Assoc,NewV,NewAssoc),
            update_mutable(NewAssoc,HT)
	  ; true
	).

value_ht(HT,B) :-
	get_mutable(Assoc,HT),
	gen_assoc(_,Assoc,V),
	member(B,V).

delete_first_fail([X | Xs], Y, Zs) :-
	( X == Y ->
	    Zs = Xs
	;
	    Zs = [X | Zs1],
	    delete_first_fail(Xs, Y, Zs1)
	).


% writeht(HT) :-
% 	get_mutable(KeyVals,HT),
% 	writeks(KeyVals).

% writeks([]).
% writeks([K-_|R]) :-
% 	write(key(K)), write(' '),
% 	writeks(R).
