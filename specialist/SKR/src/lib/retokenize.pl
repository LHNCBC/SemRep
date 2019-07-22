
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

% File:     retokenize.pl
% Module:   retokenize
% Author:   FML, indirectly
% Purpose:  predicates to retokenize before lexical lookup to solve the "in patients" problem

:- module(retokenize,[
	remove_null_atom_defns/2,
	retokenize/2,
	retokenize_for_apostrophe/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	lower/2
   ]).


/*
% ---- Retokenize
This is an ad hoc fix ultimately needed because of multi-word lexical entries.
The particular problem this addresses is the fact that "after-treatment" is a lexical entry.
Thus the prepositional phrase "after treatment" is lexicalized as the lexical entry (a noun).
Ultimately, what is needed is a switch that allows only single-word lexical entries
to be retrieved from the lexicon.
Until that is available, "after treatment" is retokenized as [after, '', treatment].

The list of "in ..." expressions for which this hack is done is below. Gross.
The lexical entry for '' is then removed via remove_null_atom_defn/2.

Yes, this is a hack. Any questions?

*/

retokenize([], []).
retokenize([ThisList|MoreLists], [NewList|Gap]) :-
	retokenize_list(ThisList, NewList),
	retokenize(MoreLists, Gap).

retokenize_list([], []).
retokenize_list([H|T], NewList) :-
	retokenize_list_1(T, H, NewList).

retokenize_list_1([], Last, [Last]).
retokenize_list_1([Next|Rest], First, [First|ReTokenizedRest]) :-
	lower(First, LowerFirst),
	lower(Next, LowerNext),
	retokenize_word(LowerFirst, LowerNext, ReTokenizedRest, ReTokenizedRestTail),
	retokenize_list_1(Rest, Next, ReTokenizedRestTail).

retokenize_word(after, treatment, [''|Tail], Tail) :- !.
retokenize_word(has,   been,      [''|Tail], Tail) :- !.
retokenize_word(in,    InWord,    [''|Tail], Tail) :- in_patients_in_word(InWord), !.
retokenize_word(over,  long,      [''|Tail], Tail) :- !.
retokenize_word(_First, _Next,     Tail,     Tail).

% retokenize_for_apostrophe([], []).
% retokenize_for_apostrophe([FirstList|RestLists], [RetokenizedFirstList|RetokenizedRestLists]) :-
% 	retokenize_list_for_apostrophe(FirstList, RetokenizedFirstList),
% 	retokenize_for_apostrophe(RestLists, RetokenizedRestLists).
% 
% 
% % keep separate any apostrophe that appears at the beginning or end of a token
% retokenize_list_for_apostrophe([], []).
% retokenize_list_for_apostrophe([First|Rest], RetokenizedWords) :-
% 	( First == 39 ->
% 	  RetokenizedWords = [First|RetokenizedRestWords],
% 	  NewRest = Rest
% 	; apostrophe_token(First, X, Y) ->
% 	  RetokenizedWords = RetokenizedRestWords,
% 	  NewRest = [X,Y|Rest]
% 	; RetokenizedWords = [First|RetokenizedRestWords],
% 	  NewRest = Rest
% 	),
% 	retokenize_list_for_apostrophe(NewRest, RetokenizedRestWords).
% 
% apostrophe_token(First, X, Y) :-
% 	atom_codes(First, Codes),
% 	( Codes = [39|RestCodes],
% 	  RestCodes \== [] ->
% 	  X = '\'',
% 	  atom_codes(NewFirst, RestCodes),
% 	  Y = NewFirst
% 	; append(PrefixString, [39], Codes) ->
% 	  PrefixString \== [],
% 	  atom_codes(PrefixAtom, PrefixString),
% 	  X = PrefixAtom,
% 	  Y = '\''
% 	).



% in_patients_in_word('-').
in_patients_in_word(and).
in_patients_in_word(articulo).
in_patients_in_word(as).
% in_patients_in_word(between).
in_patients_in_word(born).
% in_patients_in_word(bred).
in_patients_in_word(built).
in_patients_in_word(cellulo).
in_patients_in_word(center).
in_patients_in_word(ceram).
in_patients_in_word(class).
in_patients_in_word(d).
in_patients_in_word(depth).
in_patients_in_word(dies).
in_patients_in_word(door).
in_patients_in_word(dwell).
in_patients_in_word(dwelled).
in_patients_in_word(dwelling).
in_patients_in_word(dwells).
in_patients_in_word(extenso).
% in_patients_in_word(extremis).
in_patients_in_word(field).
in_patients_in_word(fields).
in_patients_in_word(fighting).
in_patients_in_word(folding).
in_patients_in_word(foldings).
in_patients_in_word(fundo).
in_patients_in_word(group).
in_patients_in_word(groups).
in_patients_in_word(growing).
in_patients_in_word(hospital).
% in_patients_in_word(law).
in_patients_in_word(laws).
% in_patients_in_word(lieu).
% in_patients_in_word(memoriam).
in_patients_in_word(migrate).
in_patients_in_word(migrated).
in_patients_in_word(migrates).
in_patients_in_word(migrating).
in_patients_in_word(migration).
in_patients_in_word(migrations).
in_patients_in_word(ovo).
in_patients_in_word(part).
in_patients_in_word(patient).
in_patients_in_word(patients).
in_patients_in_word(phase).
in_patients_in_word(phaser).
in_patients_in_word(phasest).
in_patients_in_word(service).
in_patients_in_word(shoe).
% in_patients_in_word(situ).
in_patients_in_word(tela).
in_patients_in_word(toed).
in_patients_in_word(toeing).
in_patients_in_word(toto).
in_patients_in_word(training).
in_patients_in_word(turned).
% in_patients_in_word(utero).
% in_patients_in_word(vacuo).
% in_patients_in_word(vitro).
% in_patients_in_word(vivo).

remove_null_atom_defns([], []).
remove_null_atom_defns([Tag:Defn|RestDefns], NewDefns) :-
	( null_atom_defn(Tag, Defn) ->
	  RestNewDefns = NewDefns
	; NewDefns = [Tag:Defn|RestNewDefns]
	),
	remove_null_atom_defns(RestDefns, RestNewDefns).

null_atom_defn(shapes,  [inputmatch:[''],features:[integer]]).
null_atom_defn(unknown, [inputmatch:['']]).
