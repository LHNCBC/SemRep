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

% File:	    generate_varinfo.pl
% Module:   generate_varinfo
% Author:   tcr
% Purpose:  generate variant information from a lexical entry



% ----- Module declaration and exported predicates

:- module(generate_semrep_varinfo, [
	generate_variant_info/2
]).


% ----- Imported predicates

:- use_module( lexicon(lex_access), [
	get_im_varlist/2
  ]).

:- use_module( skr_lib(sicstus_utils), [
	lower/2,
	midstring/5,
	midstring/6,
	string_char/3,
	string_size/2
  ]).


% ******************************* GENERATE_VARIANT_INFO *******************************

generate_variant_info([], []).
generate_variant_info([LexiconOrOther:List|RestDefinitions], Variants) :-
	generate_variant_info_1(LexiconOrOther, List, RestDefinitions, Variants).

generate_variant_info_1(unknown, UnkList, RestDefinitions, [ThisItem|NewGap]) :-
	!,
	get_lex_item(UnkList, inputmatch, InpMatch),
	InpMatch = [ThisItem],
	generate_variant_info(RestDefinitions, NewGap).
generate_variant_info_1(lexicon, [lexmatch:[LexMatch], InputMatch|_], RestDefinitions,
		      [LexMatch:ThisVarInfo|NewGap]) :-
	!,
	get_im_varlist(LexMatch, VarInfo),
	% lex_form_ci_vars(LexMatch, VarInfo),
	get_this_variant(VarInfo, LexMatch, ThisVarInfo, VariantTail),
	% append(ThisVarInfo, [InputMatch], ThisVarInfoAndInputMatch),   % Lan needs InputMatch
	VariantTail = [InputMatch], 
	generate_variant_info(RestDefinitions, NewGap).
% This is for shapes, punctuation, and perhaps other stuff
generate_variant_info_1(Other, OtherList, RestDefinitions, [Other:OtherList|NewGap]) :-
	generate_variant_info(RestDefinitions, NewGap).

% ----- GET_LEX_ITEM

% get_lex_item([], _Item, _ItemInfo) :- !, fail.

get_lex_item([Item:ItemInfo|_More], Item, ItemInfo) :-
	!.
get_lex_item([_Other|More], Item, ItemInfo) :-
	get_lex_item(More, Item, ItemInfo).

% ----- GET_THIS_VARIANT
% LexKeys other than forms of *be* and *have* have the format: Entry:VarList
% LexKeys for forms of *be* have the format: 'VarForm;Agr':VarList
% get_actual_lex_key/3 commented out below, due to a problem in runtime in SICStus

get_this_variant([], _ThisWord, ThisVarInfo, ThisVarInfo).
get_this_variant([LexKey:[ThisList]|MoreVariants], ThisWord, [ThisList|Rest], Tail) :-
%	get_actual_lex_key(LexKey, ActualLexKey, _AgrInfo),
        ActualLexKey = LexKey,
	lower(ActualLexKey, LowerLexKey),
	lower(ThisWord, LowerLexKey),
	!,
	get_this_variant(MoreVariants, ThisWord, Rest, Tail).
get_this_variant([_Other|MoreVariants], ThisWord, ThisVarInfo, Rest) :-
	get_this_variant(MoreVariants, ThisWord, ThisVarInfo, Rest).


%--- GET_ACTUAL_LEX_KEY

get_actual_lex_key(LexKey, ActualLexKey, AgrInfo) :-
	string_char(Index, LexKey, 0';),
	!,
	string_size(LexKey, Length),
	LLen is Index - 1,
	RLen is Length - Index,
	midstring(LexKey, _, LexKeyAndAgrInfo , LLen, 1, RLen),
	midstring(LexKeyAndAgrInfo, ActualLexKey, AgrInfo, 0, LLen).
get_actual_lex_key(ActualLexKey, ActualLexKey, none).
