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

% File:     dysonyms.pl
% Module:   Dysonym processing
% Author:   Halil
% Purpose:  Implement dysonym processing; formerly in skr/skr.pl

:- module(dysonyms, [
	exclude_dysonyms/2
    ]).

:- use_module(skr(skr_utilities), [
	get_all_candidate_features/3
   ]).

:- use_module(skr_lib(nls_strings), [
	split_string_completely/3
   ]).

:- use_module(skr_lib(semgroup_member), [
	semrep_semgroup_member/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	lower/2,
	midstring/6
    ]).

:- use_module(library(lists), [
	last/2,
	nth0/3
    ]).

:- use_module(library(sets), [
	intersection/3
    ]).


% dysonym processing
exclude_dysonyms([], []).
exclude_dysonyms([FirstCandidate|RestMapEval], RestMapEvalOut) :-
	get_all_candidate_features([metaterm,metaconcept,semtypes],
				   FirstCandidate,
				   [Syn,PrefName,SemTypes]),
	is_a_dysonym(Syn, PrefName,SemTypes),
	!,
	exclude_dysonyms(RestMapEval, RestMapEvalOut).
exclude_dysonyms([MapEval|RestMapEval], [MapEval|RestMapEvalOut]) :-
	exclude_dysonyms(RestMapEval, RestMapEvalOut).

is_a_dysonym(Syn,PrefName,SemTypes) :-
	remove_useless_suffixes(Syn, Syn0),
	remove_useless_suffixes(PrefName, PrefName0),
	atom_to_list(Syn0, SynList, SynLen),
	atom_to_list(PrefName0, PrefList, PrefLen),
	SynLen >= 1,
	PrefLen > SynLen,
	intersection(SynList, PrefList, Intersection),
	length(Intersection,SynLen),
	\+ dysonym_to_allow(Syn, PrefName),
	\+ head_exception(PrefList), 
	\+ gapped_dysonym(SynList, PrefList),
	\+ anatomy_term_with_redundant_mod(SynList, PrefList, SemTypes).

head_exception(PrefList) :-
	last(PrefList, PrefListHead),
	dysonym_exception(PrefListHead).

remove_useless_suffixes(Str,StrOut) :-
	useless_suffixes(Suffixes),
	remove_suffixes(Suffixes, Str, StrOut).

remove_suffixes([], Str, Str).
remove_suffixes([Suffix|_RestSuffixes], Str, StrOut) :-
	atom_codes(Suffix, SuffixStr),
	length(SuffixStr, SuffixLen),
	midstring(Str, StrOut, Suffix, 0, _, SuffixLen),
	!.
remove_suffixes([_Suffix|RestSuffixes], Str, StrOut) :-
	remove_suffixes(RestSuffixes, Str, StrOut).

useless_suffixes([', NOS',', NEC', ' NOS', ' NEC']).

% coronary disease -> coronary artery disease
gapped_dysonym(SynList, PrefList) :-
	length(SynList, 2),
	length(PrefList, 3),
	nth0(0,SynList, Syn0),
	nth0(0,PrefList, Syn0),
	nth0(1,SynList, Syn1),
	nth0(2,PrefList, Syn1).

% hippocampus->Entire hippocampus	
anatomy_term_with_redundant_mod(SynList, PrefList, SemTypes) :-
	length(SynList, SynLen),
	length(PrefList, PrefLen),
	PrefLen =:= SynLen + 1,
	nth0(0,PrefList, 'entire'),
	\+ nth0(0,SynList, 'entire'),
	check_anat(SemTypes).


% check_anat([]) :- fail.  
check_anat([SemType|_Rest]) :-
	semrep_semgroup_member(SemType, anat),
	!.
check_anat([_SemType|Rest]) :-
	check_anat(Rest).

% exception_list([gene, genes, proteins, 'protein,', procedure, procedures, regime, regimes, disease, diseases, disorder, disorders]).

dysonym_exception(gene).
dysonym_exception(genes).
dysonym_exception(proteins).
dysonym_exception('protein,').
dysonym_exception(procedure).
dysonym_exception(procedures).
dysonym_exception(regime).
dysonym_exception(regimes).
dysonym_exception(disease).
dysonym_exception(diseases).
dysonym_exception(disorder).
dysonym_exception(disorders).
dysonym_exception(group).
dysonym_exception(groups).

% These are dysonym exceptions that cannot be dealt with more general
% dysonym exception rules. (They seem similar to "anatomy_term_with_redundant_mod" case,
% but semantically are different.)
dysonym_to_allow('Fistula',                      'pathologic fistula').
dysonym_to_allow('computed tomography',          'X-Ray Computed Tomography').
dysonym_to_allow('tomography',                   'X-Ray Computed Tomography').
dysonym_to_allow('Food and Drug Administration', 'United States Food and Drug Administration').
dysonym_to_allow('Nucleus',                      'Cell Nucleus').

% copied from ssuppserv.pl - halil
strings_to_atoms([], []).
strings_to_atoms([String|More], [Atom|Gap]) :-
	atom_codes(Atom, String),
	strings_to_atoms(More, Gap).

atom_to_list(Atom, List, Len) :-
	lower(Atom, LCAtom),
	atom_codes(LCAtom, LCChars),
	split_string_completely(LCChars, " ", LCCharList),
	strings_to_atoms(LCCharList, List),
	length(List, Len).

