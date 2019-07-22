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
***************************************************************************/

% File:	    abgenemod.pl
% Module:   abgenemod
% Author:   Halil,tcr
% Purpose:  call abgene, Tanabe & Wilbur's program to identify genes
%           and proteins in text


% ----- Module declaration and exported predicates

:- module( abgenemod, [
	call_abgene/3,
	get_abgene_atom/2
   ]).

:- use_module( library( lists), [
	rev/2
   ]).

:- use_module( library( sets ), [
	list_to_set/2
  ]).

:- use_module(library(system), [
	environ/2
   ]).

:- use_module( skr_lib( nls_strings ), [
	split_string_completely/3
   ]).

:- use_module(skr_lib(sicstus_utils), [
	atoms_to_strings/2,
	concat_atom/2,
	lower/2
   ]).

:- use_module( skr_lib( efficiency ),  [
	maybe_atom_gc/3
   ]).

%:- use_module( usemrep_lib(inputformat), [
%		convert_to_medline/2
%   ]).

:- use_module( usemrep_lib(ssuppserv), [
		look_for_pmid/1
   ]).


foreign_resource(abgenemod,[
	c_abgene
      ]).

foreign(c_abgene, c, c_abgene(+string, [-string])).

%:- load_foreign_resource('../abgenemod').
:- environ('DYNAMIC_LIB_DIR',DynamicLibDir),
   atom_concat(DynamicLibDir,'/abgenemod',ABGeneSo),
   load_foreign_resource(ABGeneSo).

%:- abolish(foreign_file,2).
%:- abolish(foreign,3).

% :- prolog_flag(character_escapes,_,on).

% ---------- call_abgene

call_abgene(Domain, ABGeneAtom, ABGenes) :-
 	( Domain \== genetics,
	  Domain \== generic ->
  	  ABGenes = []
	; abgene(ABGeneAtom, ABGenes1),
 	  maybe_atom_gc(_,_,_),
 	  list_to_set(ABGenes1, ABGenes),
	  format(user_output,'ABGenes-~q~n', [ABGenes])
 	).


% ---------- ABGene

abgene(ABGeneAtomIn, GeneList) :-
	c_abgene(ABGeneAtomIn, ABGeneAtomOut),
	atom_codes(ABGeneAtomOut, ABGeneStringOut),
	list_of_strings(ABGeneStringOut, ABGeneList),
	ABGeneList = [_FirstLine|AbstractLines],
	split_abstract(AbstractLines, SplitAbstract),
	extract_all_genes(SplitAbstract, GeneList).

% ----- List of Strings
% ASCII 10 is the newline char that was added in prep_for_abgene

list_of_strings([], []).
list_of_strings([H|T], [ThisLine|Gap]) :-
	get_a_line([H|T], ThisLine, Remainder),
	list_of_strings(Remainder, Gap).

% ---

get_a_line([H|T], ThisLine, Remainder) :-
	( H == 10 ->
	  ThisLine = [],
	  Remainder = T
	; ThisLine = [H|Rest],
	  get_a_line(T, Rest, Remainder)
	).

% ---------- Split Abstract

split_abstract([], []).
split_abstract([ThisLine|More], [StrList|Gap]) :-
	split_string_completely(ThisLine, " ", StrList),
	split_abstract(More, Gap).

% ----- Extract All Genes

extract_all_genes(SplitAbstract, GeneList) :-
	extract_all_genes_1(SplitAbstract, GeneList, []).

extract_all_genes_1([], GL, GL).
extract_all_genes_1([StrList|More], GeneListIn, GeneListOut) :-
	convert_to_struct(StrList, LabeledInpList),
	get_genes(LabeledInpList, GeneListIn, GeneListNext),
	extract_all_genes_1(More, GeneListNext, GeneListOut).

/*
% --- Convert to Struct

The slash is ambiguous, because there are structures like "PEK/EIF2AK3/NEWGENE".
Therefore we have to find the *last* slash in the string, which does *not* require reversing!!
convert_one_string/2, which implements the logic of convert_to_struct/2, has become *far*
simpler and more efficient.

*/

convert_to_struct([], []).
convert_to_struct([String|More], Converted) :-
	( String == [] ->
	  convert_to_struct(More, Converted)
	; convert_one_string(String, ConvertedString),
	  Converted = [ConvertedString|RestConvertedStrings],
	  convert_to_struct(More, RestConvertedStrings)
	).

convert_one_string(ThisStr, LabeledWord) :-
	split_string(ThisStr, 47, BeforeLastSlashString, AfterLastSlashString), % 47 is ascii('/')
	atom_codes(BeforeLastSlashAtom, BeforeLastSlashString),
	atom_codes(AfterLastSlashAtom, AfterLastSlashString),
	lower(AfterLastSlashAtom, LowerCaseTag),
	functor(LabeledWord, LowerCaseTag, 1),
	arg(1, LabeledWord, BeforeLastSlashAtom).

split_string(String, Char, Prefix, Suffix) :-
	append(Prefix, [Char|Suffix], String),
	\+ memberchk(Char, Suffix),
	!.

% ----- Get Genes

get_genes([], Genes, Genes).
get_genes([multigene(Name)|More], [FullMGeneList|RestGenes], Rest) :-
	!,
	get_multi_gene(More, MGeneList, [], Remainder),
	FullMGeneList = [Name|MGeneList],
	get_genes(Remainder, RestGenes, Rest).
get_genes([ThisStruc|More], [GeneNameList|RestGenes], Rest) :-
	functor(ThisStruc, Label, _),
	memberchk(Label, [gene,newgene,contextgene,multigene]),
	!,
	arg(1,ThisStruc,GeneName),
	GeneNameList = [GeneName],
	% remove_hyphens(GeneName,GeneNameList),
	get_genes(More, RestGenes, Rest ).
get_genes([_Other|More], RestGenes, Rest) :-
	get_genes(More, RestGenes, Rest).

% ---
get_multi_gene([multigene(Name)|More], MultiGeneListIn, MultiGeneListOut, Remainder) :-
	!,
	MultiGeneListIn = [Name|MultiGeneListNext],
	get_multi_gene(More, MultiGeneListNext,  MultiGeneListOut, Remainder).
get_multi_gene([Other|More], MultiGeneList, MultiGeneList, [Other|More]).

% ---
% From jdimod.pl
get_abgene_atom(Text, ABGeneAtom) :-
	prep(Text, TextAtom),
	( look_for_pmid(Text) ->
	  ABGeneAtom = TextAtom
	; convert_to_medline(TextAtom, AtomConv),
	  atom_codes(AtomConv, StringConv),
	  split_string_completely(StringConv, [10], SplitStringConv),
	  prep(SplitStringConv, ABGeneAtom)
	),
        maybe_atom_gc(_,_,_).

prep(ListOfStrings, Atom) :-
	add_newline_for_each_string(ListOfStrings, ListOfStringsWithNewline),
	atoms_to_strings(ListOfAtoms,ListOfStringsWithNewline),
	concat_atom(ListOfAtoms, Atom).

add_newline_for_each_string([], []).
add_newline_for_each_string([H|T], [H,"\n"|Rest]) :-
	add_newline_for_each_string(T, Rest).

convert_to_medline(TextAtom,TextAtomOut) :-
	format(user_output,'Converting to Medline format .~n',[]),
	add_newline_for_each_string(%["UI  - 00000000",
				    ["PMID- 00000000",
				     "DA  - 00000000",
				     "TI  - "],PreambleWithNewLine),
	atoms_to_strings(PreambleAtoms,PreambleWithNewLine),
	concat_atom(PreambleAtoms,PreambleAtom),
	concat_atom([PreambleAtom,TextAtom],TextAtomOut).

	