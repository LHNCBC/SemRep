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

% File:	    metamap_parsing.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  MetaMap parsing routines


:- module(metamap_parsing, [
	demote_heads/2,
	generate_syntactic_analysis_plus/4,
	collapse_syntactic_analysis/2,
	re_attach_apostrophe_s_to_prev_word/3
    ]).

:- use_module(lexicon(qp_token), [
	tokenize_string/2
    ]).

:- use_module(lexicon(qp_lookup), [
	assemble_definitions/2
    ]).

:- use_module(metamap(metamap_tokenization), [
	tokenize_text_utterly/2
    ]).

:- use_module(skr_lib(consulttt), [
	consult_tagged_text/5
   ]).

:- use_module(skr_lib(generate_varinfo), [
	generate_variant_info/2
    ]).

:- use_module(skr_lib(mincoman), [
	minimal_commitment_analysis/4
    ]).

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1
   ]).

:- use_module(skr_lib(retokenize), [
	remove_null_atom_defns/2,
	retokenize/2
	% retokenize_for_apostrophe/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2
    ]).

:- use_module(library(lists), [
	append/2
    ]).


/* ************************************************************************
   ************************************************************************
   ************************************************************************
                          MetaMap Parsing Predicates
   ************************************************************************
   ************************************************************************
   ************************************************************************ */

%%% get_tagger_atoms([], []).
%%% get_tagger_atoms([[H,_]|RestTagList], [H|RestAtoms]) :-
%%% 	get_tagger_atoms(RestTagList, RestAtoms).

generate_syntactic_analysis_plus(ListOfAscii, TagList, SyntAnalysis, Definitions) :-
 	tokenize_string(ListOfAscii, Words0),
	re_attach_apostrophe_s_syntax(Words0, TagList, Words1),
	retokenize(Words1, Words),
	% retokenize_for_apostrophe(Words1, Words),
	% modify_tag_list_for_apostrophe(TagList0, TokenLists, TagList),
	% assemble_definitions(TokenLists, TagList0, Definitions0),
	assemble_definitions(Words, Definitions0),
	remove_null_atom_defns(Definitions0, Definitions),
	generate_variant_info(Definitions, VarInfoList),
	% ChromosomeFound is 0,
	% LeftOverWords = [],
	% PrevTagWord = '',
	% update_taglist(TagList, Definitions, PrevTagWord,
	% 	       ChromosomeFound, LeftOverWords, TagList),
	maybe_consult_tagged_text(TagList, Definitions, VarInfoList, LabeledText, 1),
	!,
	minimal_commitment_analysis(TagList, VarInfoList, LabeledText, SyntAnalysis).

% This predicate is intentionally nondeterminate; must be backtrackable.
% The choice points are removed by the cut after maybe_consult_tagged_text/5
% in the definition of generate_syntactic_analysis_plus/4.
%%% modify_tag_list_for_apostrophe([], _TokenLists, []).
%%% modify_tag_list_for_apostrophe(TagListIn, TokenLists, TagListOut) :-
%%% 	TagListIn =  [[_AtomBefore,_LexCatBefore],['\'',ap],[_AtomAfter,_LexCatAfter] | _RestTagListIn],
%%% 	concat_all_apostrophe_atoms(TagListIn, [[ConcatAtom,ConcatLexCat] | RestTagListNext]),
%%% 	member(ThisTokenList, TokenLists),
%%% 	memberchk(ConcatAtom, ThisTokenList),
%%%  	TagListOut = [[ConcatAtom,ConcatLexCat] | RestTagListOut],
%%% 	modify_tag_list_for_apostrophe(RestTagListNext, TokenLists, RestTagListOut).
%%% modify_tag_list_for_apostrophe([H|RestTagListIn], TokenLists, [H|RestTagListOut]) :-
%%% 	modify_tag_list_for_apostrophe(RestTagListIn, TokenLists, RestTagListOut).
%%% 
%%% 
%%% % This predicate is intentionally nondeterminate; must be backtrackable.
%%% concat_all_apostrophe_atoms(TagListIn, TagListNext) :-
%%% 	TagListIn =  [[AtomBefore,LexCatBefore],['\'',ap],[AtomAfter,LexCatAfter] | RestTagListIn],
%%% 	concat_atom([AtomBefore, '\'', AtomAfter], ConcatAtom0),
%%% 	choose_lexcat(AtomBefore, LexCatBefore, AtomAfter, LexCatAfter, ConcatLexCat),
%%% 	concat_all_apostrophe_atoms([[ConcatAtom0,ConcatLexCat]|RestTagListIn], TagListNext).
%%% concat_all_apostrophe_atoms(TagListIn, TagListNext) :-
%%% 	TagListNext = TagListIn.


% Assign to the combined atom the lexical category of the longer of the two atoms
% choose_lexcat(AtomBefore, LexCatBefore, AtomAfter, LexCatAfter, CombinedLexCat) :-
% 	atom_length(AtomBefore, AtomBeforeLength),
% 	atom_length(AtomAfter,  AtomAfterLength),
% 	( AtomBeforeLength > AtomAfterLength ->
% 	  CombinedLexCat = LexCatBefore
% 	; CombinedLexCat = LexCatAfter
% 	).

% First arg is TagList: Call consult_tagged_text iff TagList is not []
maybe_consult_tagged_text([], _Definitions, _VarInfoList, [], _1).
maybe_consult_tagged_text([H|T], Definitions, VarInfoList, LabeledText, 1) :-
	consult_tagged_text(Definitions, VarInfoList, [H|T], LabeledText, 1).

% The call to tokenize_string(ListOfAscii, Words0)
% will parse words ending in "'s" (apostrophe + s) into three tokens, e.g.,
% tokenize_string("finkelstein's test positive", Words0)
% will instantiate Words0 to [[finkelstein,'\'',s,test,positive]].
% MetaMap's (new!) default behavior is to reattach the "'s" to the previous token.

% WordListsIn is a list of lists of tokens, e.g.,
% [[finkelstein, '\'', s, test, positive]]
re_attach_apostrophe_s_syntax(WordListsIn, TagList, WordListsOut) :-
	(  foreach(ListIn,  WordListsIn),
	     foreach(ListOut, WordListsOut),
	     param(TagList)
	  do re_attach_apostrophe_s_to_prev_word(ListIn, TagList, ListOut)
	).

% Given a list of tokens, transform the sequence of atoms <any token>, <apostrophe>, <s>
% to the atom <any token apostrophe s>, e.g.,
% [finkelstein, '\'', s, test, positive] --> [finkelstein's, test, positive]
% We do not have to worry about glomming the apostrophe onto a previous token
% ending in a punctuation mark here...I think...

% The tagger output for "alzheimer's disease" is
% [[alzheimer,noun],['\'',ap],[s,noun],[disease,noun]]
% but the tokenization is
% [['alzheimer\'s',disease]]
% We must glue together the tagger components for compatibility with the tokenization.
% This process must be recursive because of tokens like
% D'ALEMBERT'S PRINCIPLE
% PRESIDENT O'Shea's report.
% use of D'Herelle's technique
% C.D. O'Malley's Vesalius
% D'Arnaud's barbet
% O'Sullivan's oral glucose tolerance procedure
% O'Brien's actinic granuloma
% using O'Farrell's system

re_attach_apostrophe_s_to_prev_word([], _TagList, []).
re_attach_apostrophe_s_to_prev_word([OrigWord, '''', s | RestWordsIn], TagList, WordListOut) :-	
	concat_atom([OrigWord, '''', s], WordWithApostropheS),
	% verify that the synthetically created Word+apostrophe+s
	% is indeed in the TagList!
	memberchk([WordWithApostropheS,_LexCat], TagList),
	!,
	WordListOut = [WordWithApostropheS|RestWordsOut],
	re_attach_apostrophe_s_to_prev_word(RestWordsIn, TagList, RestWordsOut).
re_attach_apostrophe_s_to_prev_word([H|RestIn], TagList, [H|RestOut]) :-
	re_attach_apostrophe_s_to_prev_word(RestIn, TagList, RestOut).

/* collapse_syntactic_analysis(+SyntAnalysis, -CollapsedSyntAnalysis)

collapse_syntactic_analysis/2 collapses all phrases of SyntAnalysis into
one, the head being the head of the first phrase.  */

collapse_syntactic_analysis(minimal_syntax(Syntax), minimal_syntax([CollapsedSyntax])) :-
        collapse_syntactic_analysis(Syntax, CollapsedSyntax).
collapse_syntactic_analysis([], []).
collapse_syntactic_analysis([First|Rest], CollapsedSyntAnalysis) :-
	demote_all_heads(Rest, DemotedRest),
	append([First|DemotedRest], CollapsedSyntAnalysis).

demote_all_heads([], []).
demote_all_heads([First|Rest], [DemotedFirst|DemotedRest]) :-
	demote_heads(First, DemotedFirst),
	demote_all_heads(Rest, DemotedRest).

demote_heads([], []).
demote_heads([First|Rest], [NewFirst|DemotedRest]) :-
	( functor(First, head, 1) ->
	  arg(1, First, Arg),
	  functor(NewFirst, mod, 1),
  	  arg(1, NewFirst, Arg)
	; NewFirst = First
	),
	demote_heads(Rest, DemotedRest).

% This is less efficient!
% demote_heads([head(X)|Rest], [mod(X)|DemotedRest]) :-
% 	!,
% 	demote_heads(Rest, DemotedRest).
% demote_heads([First|Rest], [First|DemotedRest]) :-
% 	demote_heads(Rest, DemotedRest).
