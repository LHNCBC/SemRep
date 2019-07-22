
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  https://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

%   Package: flatten
%   Author : Richard A. O'Keefe
%   Updated: 01 Jun 1989
%   Purpose: Flatten various binary trees to lists and convert back.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1987, Quintus Computer Systems, Inc.  All rights reserved.

%   Copied verbatim from QP library.

/*  This file was originally for PRESS, where you often want to take
    a tree such as 1+x+0+(u*v+9)+(x^2+2) and flatten it to a list
    such as [1,x,u*v,9,x^2,2] so that you can easily pick out all the
    constants or all the terms involving x or something, without having
    to write N different sets of predicates to handle N different
    binary operators.  It can be useful for other things as well.

    The <operator>_to_list predicates take a binary tree (where leaf
    nodes are anything not labelled by the operator) and flatten it
    to a list.  They also omit "units" of that operator, that is, if
    the operator is & {| + *} the constant true {false 0 1} will not
    appear in the list.  The predicate
	binary_to_list(Tree, Operator, Unit, Before, After)
    enables you to make your own versions.  Note that the answer is
    accumulated in the difference list Before-After.
	binary_to_list(Tree, Operator, Before, After)
    lets you convert trees where the operator has no unit.

    The well known and not often useful predicate "flatten" is a not
    very interesting special case of binary_to_list/5.

    The list_to_<operator> predicates take a list and turn it back
    into a tree.  Now there is an interesting question here: is
    [a,b,c] to be turned into f(a,f(b,c)) or into f(f(a,b),c)?  The
    former is a good idea for & | and '.', while the latter is a
    good idea for + and *.  My solution was to have the top-level
    predicate check whether the Operator is a yfx operator (such as
    + and * are) and if so to generate f(f(a,b),c).  In all other
    cases (xfy,xfx, or no operator declaration) f(a,f(b,c)) is
    generated.
	list_to_binary(List, Operator, Unit, Tree)
    lets you make your own versions.  If the list is [] the Unit will
    be returned, that is the only use of the Unit.
	list_to_binary(List, Operator, Tree)
    should be used when the Operator has no Unit, if given an empty
    list it will fail.  You may find it more useful to call the
    auxiliary routines list_to_binaryL and list_to_binaryR directly.

    Note: a good Prolog programmer should be rather puzzled by the
    way some of these predicates are coded.  Why haven't I use =.. ?
    The calls to functor & arg would be much clearer as univs.  The
    superb Prolog programmer will take the trouble to measure how
    fast X =.. [F,A,B] is, compared with explicit calls to functor &
    arg, and will find that the explicit calls are quite a bit faster.
*/

:- module(flatten, [
% 	and_to_list/2,		%  conjunction -> list of conjuncts
% 	list_to_and/2,		%  list of conjuncts -> conjunction
% 
% 	or_to_list/2,		%  disjunction -> list of disjuncts
% 	list_to_or/2,		%  list of disjuncts -> disjunction
% 
% 	plus_to_list/2,		%  sum -> list of terms
% 	list_to_plus/2,		%  list of terms -> sum
% 
% 	times_to_list/2,	%  product -> list of factors
% 	list_to_times/2,	%  list of factors -> product
% 
 	flatten/2		%  list of lists -> list
% 
% 	binary_to_list/4,	%  Term,Operator -> DifferenceList
% 	binary_to_list/5,	%  Term,Operator,Unit -> DifferenceList
% 
% 	list_to_binary/3,	%  List,Operator -> Term
% 	list_to_binary/4	%  List,Operator,Unit -> Term
]).
% 
% :- mode
% 	and_to_list(+, -),
% 	binary_to_list(+, +, -, ?),
% 	binary_to_list(+, +, +, -, ?),
% 	flatten(+, ?),
% 	    flatten(+, ?, ?),
% 	list_to_and(+, -),
% 	list_to_binary(+, +, -),
% 	list_to_binary(+, +, +, -),
% 	list_to_binaryL(+, +, +, -),
% 	list_to_binaryR(+, +, +, -),
% 	list_to_or(+, -),
% 	list_to_plus(+, -),
% 	list_to_times(+, -),
% 	or_to_list(+, -),
% 	plus_to_list(+, -),
% 	times_to_list(+, -).

% and_to_list(Conjunction, List) :-
% 	binary_to_list(Conjunction, &, true, List, []).
% 
% 
% list_to_and(List, Conjunction) :-
% 	list_to_binary(List, &, true, Conjunction).
% 
% 
% 
% or_to_list(Disjunction, List) :-
% 	binary_to_list(Disjunction, ;, false, List, []).
% 
% 
% list_to_or(List, Disjunction) :-
% 	list_to_binary(List, ;, false, Disjunction).
% 
% 
% 
% plus_to_list(Sum, List) :-
% 	binary_to_list(Sum, +, 0, List, []).
% 
% 
% list_to_plus(List, Sum) :-
% 	list_to_binary(List, +, 0, Sum).
% 
% 
% 
% times_to_list(Product, List) :-
% 	binary_to_list(Product, *, 1, List, []).
% 
% 
% list_to_times(List, Product) :-
% 	list_to_binary(List, *, 1, Product).



%   flatten(Tree, List)
%   flattens a Tree of cons cells, yielding a List.  This is just
%	binary_to_list(Tree, ., [], List, []).
%   but it is more efficient.  You may find it helpful to see the
%   special case as a guide to the general case.
	
flatten(Tree, List) :-
	flatten(Tree, List, []).

flatten([]) --> !.
flatten([Head|Tail]) --> !,
	flatten(Head),
	flatten(Tail).
flatten(Other) -->
    %	{ Other ~= [], Other ~= [_|_] },
	[Other].



% binary_to_list(Unit, _, Unit) --> !.
% binary_to_list(Node, Operator, Unit) -->
% 	{ Node =.. [Operator,Lhs,Rhs] }, % Node can't be a variable
% 	!,
% 	binary_to_list(Lhs, Operator, Unit),
% 	binary_to_list(Rhs, Operator, Unit).
% binary_to_list(Other, _, _) -->
%     %	{ Other ~= Unit, Other ~=.. [Operator,_,_] },
% 	[Other].
% 
% 
% binary_to_list(Node, Operator) -->
% 	{ nonvar(Node) },
% 	{ Node =.. [Operator,Lhs,Rhs] },
% 	!,
% 	binary_to_list(Lhs, Operator),
% 	binary_to_list(Rhs, Operator).
% binary_to_list(Other, _) -->
%     %	{ Other ~=.. [Operator,_,_] },
% 	[Other].
% 
% 
% 
% list_to_binary([], _, Unit, Unit).
% list_to_binary([Head|Tail], Operator, _, Answer) :-
% 	current_op(_, yfx, Operator),
% 	!,
% 	list_to_binaryL(Tail, Operator, Head, Answer).
% list_to_binary([Head|Tail], Operator, _, Answer) :-
% 	list_to_binaryR(Tail, Head, Operator, Answer).
% 
% 
% list_to_binary([Head|Tail], Operator, Answer) :-
% 	current_op(_, yfx, Operator),
% 	!,
% 	list_to_binaryL(Tail, Operator, Head, Answer).
% list_to_binary([Head|Tail], Operator, Answer) :-
% 	list_to_binaryR(Tail, Head, Operator, Answer).
% 
% 
% list_to_binaryL([], _, Term, Term).
% list_to_binaryL([Head|Tail], Operator, Term0, Term) :-
% 	Term1 =.. [Operator,Term0,Head],
% 	list_to_binaryL(Tail, Operator, Term1, Term).
% 
% 
% list_to_binaryR([], Term, _, Term).
% list_to_binaryR([Head|Tail], Term0, Operator, Term) :-
% 	Term =.. [Operator,Term0,Term1],
% 	list_to_binaryR(Tail, Head, Operator, Term1).



