
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

% File:	    nls_lists.pl
% Module:   NLS Lists
% Author:   Lan
% Purpose:  Provide various list-processing predicates.


:- module(nls_lists,[
	get_from_list/3,
	% must be exported for SemRep
	get_from_list_nd/3,
	first_n_or_less/3
    ]).


/* first_n_or_less(+List, +N, -PrefixList)

first_n_or_less/3 computes PrefixList consisting of the first N elements
of List.  If List has fewer than N elements, PrefixList is simply List,
itself.  */

first_n_or_less(List,N,PrefixList) :-
    (N=<0 ->
        PrefixList=[]
    ;   first_n_or_less_aux(List,N,PrefixList)
    ).

first_n_or_less_aux([],_,[]) :-
    !.
first_n_or_less_aux(_,0,[]) :-
    !.
first_n_or_less_aux([First|Rest],N,[First|ModifiedRest]) :-
    M is N-1,
    first_n_or_less_aux(Rest,M,ModifiedRest).


% ---------- GET_FROM_LIST ----------
% Get the next item which matches Target or get the argument from a structure
% which has Target as its functor;
% the cuts mean you only get the first match (in a flat list).

% get_from_list_nd is the NonDeterministic version of the original get_from_list;
% The semantics of the original get_from_list have not changed, because this predicate
% simply calls get_from_list_nd, and then immediately cuts.

% :- nondet get_from_list_nd/3.

get_from_list_nd(Target, [Target|_More], Target).
get_from_list_nd(Target, [Target:TargetList|_More], TargetList).

get_from_list_nd(Target, [Structure|_More], TargetArg) :-
        functor(Structure, Target, 1),
        arg(1, Structure, TargetArg).
get_from_list_nd(Target, [_Other|More], TargetOut) :-
        get_from_list_nd(Target, More, TargetOut).
get_from_list_nd(Target, [[EmbeddedList]|_More], TargetOut) :-
        get_from_list_nd(Target, [EmbeddedList], TargetOut).

% Deterministic version of get_from_list; the same as the get_from_list
% before the introduction of get_from_list_nd
get_from_list(Target, List, Structure) :-
	get_from_list_nd(Target, List, Structure),
	!.
