
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

:- module(wsdmod, [
	do_WSD/9,
	% called by MetaMap API -- do not change signature!
	extract_SemRep_phrases_1/3
    ]).

:- use_module(metamap(metamap_tokenization), [
	listify/2
    ]).

:- use_module(metamap(metamap_utilities), [
	candidate_term/16
    ]).

:- use_module(skr(skr_utilities), [
	fatal_error/2,
        get_all_candidate_features/3,
        get_candidate_feature/3
    ]).

:- use_module(skr_lib(nls_lists), [
         get_from_list/3
    ]).

:- use_module(skr_lib(nls_strings), [
	trim_whitespace_right/2
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
   ]).

:- use_module(skr_lib(skr_tcp), [
	establish_tcp_connection/4
    ]).

:- use_module(skr(skr), [
	compute_confidence_value/5,
	extract_phrases_from_aps/2,
	get_inputmatch_atoms_from_phrase/2,
	get_phrase_tokens/4
   ]).

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	split_string_completely/3,
	trim_and_compress_whitespace/2
   ]).

:- use_module(skr_lib(sicstus_utils), [
	ttyflush/0
   ]).

:- use_module(skr(skr_utilities), [
	debug_message/3,
	ensure_number/2,
	fatal_error/2,
	generate_AAs_MMO_term/2,
	token_template/6

   ]).

:- use_module(skr_lib(xml), [
	xml_parse/3
   ]).

:- use_module(text(text_object_util),[
	field_or_label_type/1
    ]).

:- use_module(wsd(mmoxml), [
	mmo_terms_to_xml_chars/3
   ]).

:- use_module(library(codesio),[
	read_from_codes/2
   ]).

:- use_module(library(lists),[
	append/2,
	keys_and_values/3,
	last/2,
	max_member/2,
	rev/2,
	selectchk/3
   ]).

:- use_module(library(lists),[
	last/3
    ]).

:- use_module(library(random),[
	random_member/2
    ]).

:- use_module(library(sets),[
	subtract/3
    ]).

:- use_module(library(system), [
	environ/2
   ]).

% :- prolog_flag(character_escapes,_,on).

do_WSD(UtteranceText, InputLabel, CitationTextAtom, AAs, Tokens,
       WSDServerStream, MMOPhrases, DisambMMOPhrases, SemRepPhrasesWithWSD) :-
	maybe_do_SemRep_WSD(MMOPhrases, MMOPhrases1),
	( control_option(word_sense_disambiguation) ->
	  generate_AAs_MMO_term([mm_output(utterance, citation, modifiedtext, tagging,
					   AAs, syntax, MMOPhrases1, extractedphrases)],
				AATerm),
	  MMOutput = mm_output(utterance(InputLabel, UtteranceText, _PosInfo, _ReplPos),
			       citation(CitationTextAtom), modifiedtext, tagging,
			       AATerm, syntax,
			       MMOPhrases1, extractedphrases),
          % format(user_output, 'BEFORE disambiguate_mmoutput~n', []), ttyflush,
	  maybe_disambiguate_mmoutput([MMOutput], WSDServerStream, [DisambMMOutput]),
	  % format(user_output, 'AFTER disambiguate_mmoutput~n', []), ttyflush,
	  DisambMMOutput = mm_output(_Utterance, _Citation, _Modifiedtext, _Tagging,
				     _AATerm, _Syntax, DisambMMOPhrases0, _ExtractedPhrases),
	  % format('DisambMMOPhrases0: ~q~n~n', [DisambMMOPhrases0]),
	  maybe_create_supermapping_for_conj(DisambMMOPhrases0, DisambMMOPhrases),  
	  get_utterance_token_list(Tokens, TokensThisUtterance, _CharOffset, _TokensOut),
	  % skr_utilities:write_raw_token_lists(Tokens, []),
	  % skr_utilities:write_raw_token_lists(TokensThisUtterance, []),
	  extract_SemRep_phrases(DisambMMOPhrases, TokensThisUtterance, SemRepPhrasesWithWSD)
	; DisambMMOPhrases0 = MMOPhrases,
	  get_utterance_token_list(Tokens, TokensThisUtterance, _CharOffset, _TokensOut),
	  extract_SemRep_phrases(DisambMMOPhrases0, TokensThisUtterance, SemRepPhrasesWithWSD)
	),
	  ( control_option(conj) ->
	    create_supermapping_for_conj(DisambMMOPhrases0, DisambMMOPhrases)
	  ; DisambMMOPhrases = DisambMMOPhrases0
	).	


maybe_create_supermapping_for_conj(PhrasesIn, PhrasesOut) :-
	( control_option(conj) ->
	  create_supermapping_for_conj(PhrasesIn, PhrasesOut)
	; PhrasesOut = PhrasesIn
	).

create_supermapping_for_conj([], []).
create_supermapping_for_conj([FirstPhraseIn|RestPhrasesIn], [FirstPhraseOut|RestPhrasesOut]) :-
	FirstPhraseIn = phrase(phrase(PhraseTextAtom0,Phrase,StartPos/Length,ReplacementPos),
			       candidates(TotalCandidateCount,ExcludedCandidateCount,
					  PrunedCandidateCount,RemainingCandidateCount,Evaluations),
			       mappings(Mappings),
			       pwi(PhraseWordInfo),
			       gvcs(GVCs),
			       ev0(Evaluations3),
			       aphrases(APhrases)),
	merge_mappings(Mappings, SuperMapping),
	FirstPhraseOut = phrase(phrase(PhraseTextAtom0,Phrase,StartPos/Length,ReplacementPos),
				candidates(TotalCandidateCount,ExcludedCandidateCount,
					   PrunedCandidateCount,RemainingCandidateCount,Evaluations),
				mappings(SuperMapping),
				pwi(PhraseWordInfo),
				gvcs(GVCs),
				ev0(Evaluations3),
				aphrases(APhrases)),
	create_supermapping_for_conj(RestPhrasesIn, RestPhrasesOut).
	
merge_mappings([], []).
merge_mappings([H|T], [SuperMapping]) :-
	Mappings = [H|T],
	get_all_candidates(Mappings, Candidates0),
	append(Candidates0, Candidates1),
	sort(Candidates1, Candidates2),
	Candidates2 = [FirstCandidate|_],
	get_candidate_feature(negvalue, FirstCandidate, NegValue),
	normalize_scores(Candidates2, NegValue, Candidates),
	Conj = 1,
	compute_confidence_value(Candidates, Conj, _NPhraseWords, _Variants, ModScore),
	SuperMapping = map(ModScore, Candidates).

% Canonical conjunction phrase:
% hereditary recurrent and metastatic ovarian or renal or pancreatic carcinoma

% Ensure that all candidates have the same score,
% specifically the score of the first candidate.
normalize_scores([], _NegValue, []).
normalize_scores([FirstCandidateIn|RestCandidatesIn], CommonNegValue,
		 [FirstCandidateOut|RestCandidatesOut]) :-
	candidate_term(_NegValue, CUI, MetaTerm, PreferredName, MetaWords, SemTypes,
		       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
		       IsOvermatch, UniqueSources, PosInfo, Status, Negated,
		       FirstCandidateIn),
	candidate_term(CommonNegValue, CUI, MetaTerm, PreferredName, MetaWords, SemTypes,
		       MatchMap, LSComponents, TargetLSComponent, InvolvesHead,
		       IsOvermatch, UniqueSources, PosInfo, Status, Negated,
		       FirstCandidateOut),
	normalize_scores(RestCandidatesIn, CommonNegValue, RestCandidatesOut).
	
get_all_candidates([], []).
get_all_candidates([map(_NegScore, Candidates)|RestMappings],
		   [Candidates|RestCandidates]) :-
	get_all_candidates(RestMappings, RestCandidates).

maybe_do_SemRep_WSD(MMOPhrases, MMOPhrases1) :-
	( % control_option(word_sense_disambiguation),
	  control_option(usemrep_processing) ->
	  do_SemRep_WSD(MMOPhrases, MMOPhrases1)
	; MMOPhrases1 = MMOPhrases
	).

do_SemRep_WSD(MMOPhrasesIn,MMOPhrasesOut) :-
	preferred_name_synonym_equality(MMOPhrasesIn,MMOPhrases0),
	remove_ftcn_inpr_semtypes(MMOPhrases0,MMOPhrasesOut).
%	format('MMOPhrase: ~q~n', [MMOPhrasesOut]).

% extract_SemRep_phrases(mm_output(_Utt,_CitText,_MT,_TG,_AA,_Sx,MMOPhrases,_Ex),
% 		       Tokens, SemRepPhrases) :-
% 	extract_SemRep_phrases_aux(MMOPhrases, Tokens, SemRepPhrases).

extract_SemRep_phrases(DisambMMOPhrases, TokensThisUtterance, SemRepPhrasesWithWSD) :-
	( control_option(usemrep_processing) ->
	  extract_SemRep_phrases_aux(DisambMMOPhrases,
				     TokensThisUtterance,
				     SemRepPhrasesWithWSD)
	  % reformat_SemRepPhrases(SemRepPhrasesWithWSD0, SemRepPhrasesWithWSD)
	; SemRepPhrasesWithWSD = []
	).

/*
* reformat_SemRepPhrases(SemRepPhrasesIn, SemRepPhrasesOut) :-
* 	append(SemRepPhrasesIn, SemRepPhrasesAppended),
* 	listify(SemRepPhrasesAppended, SemRepPhrasesOut).
* 	
* create_super_APhrase([NextAPhrase|RestAPhrases], FirstAPhrase, SuperAPhrase) :-
* 	FirstAPhrase = ap(NegValue,LPhrase,PhraseMap,Mapping),
* 	get_all_components([NextAPhrase|RestAPhrases], RestLPhrases, RestMappings),
* 	append([LPhrase|RestLPhrases], AllLPhrases0),
* 	append([Mapping|RestMappings], AllMappings0),
* 	sort(AllLPhrases0, AllLPhrases),
* 	sort(AllMappings0, AllMappings),
* 	SuperAPhrase = ap(NegValue,AllLPhrases,PhraseMap,AllMappings).
* 
* 
* get_all_components([], [], []).
* get_all_components([NextAPhrase|RestAPhrases],
* 		   [NextLPhrase|RestLPhrases],
* 		   [NextMapping|RestMappings]) :-
* 	NextAPhrase = ap(_NegValue,NextLPhrase,_PhraseMap,NextMapping),
* 	get_all_components(RestAPhrases, RestLPhrases, RestMappings).
* 
* sort_by_position(SemRepPhrases0, [confid(C)|SemRepPhrases]) :-
* 	selectchk(confid(C), SemRepPhrases0, RestPhrases),
* 	get_position_terms(RestPhrases, PositionTerms),
* 	keys_and_values(RestPhrasesWithPositionPrefix, PositionTerms, RestPhrases),
* 	keysort(RestPhrasesWithPositionPrefix, Sorted),
* 	keys_and_values(Sorted, _PositionTerms, SemRepPhrases).
* 
* get_position_terms([], []).
* get_position_terms([First|Rest], [FirstPosTerm|RestPosTerms]) :-
* 	arg(1, First, List),
* 	last(List, FirstPosTerm),
* 	get_position_terms(Rest, RestPosTerms).
*/

extract_SemRep_phrases_aux([], _Tokens, []).
extract_SemRep_phrases_aux([phrase(Phrase,_,_,_,_,_,aphrases(APhrases))|RestPhraseTerms],
			   Tokens, [SemRepPhrases|RestSemRepPhrases]) :-
	Phrase = phrase(_,PhraseList,_,_),
	get_inputmatch_atoms_from_phrase(PhraseList, InputMatchPhraseWords),
	get_phrase_tokens(InputMatchPhraseWords, Tokens, PhraseTokens, TokensRest),
 	( APhrases == [] ->
 	  extract_SemRep_phrases_1([ap(_,PhraseList,_,[])], PhraseTokens, SemRepPhrases)
 	; APhrases = [APhrases0|_],
	  extract_SemRep_phrases_1([APhrases0], PhraseTokens, SemRepPhrases)
 	),
	!,
	extract_SemRep_phrases_aux(RestPhraseTerms, TokensRest, RestSemRepPhrases).
extract_SemRep_phrases_aux([phrase(_,_,_,_,_,_,aphrases(APhrases))|RestPhraseTerms],
	                   Tokens, [SemRepPhrases|RestSemRepPhrases]) :-
	APhrases = [APhrases0|_],
	extract_phrases_from_aps([APhrases0], SemRepPhrases),
	extract_SemRep_phrases_aux(RestPhraseTerms, Tokens, RestSemRepPhrases).

extract_SemRep_phrases_1([], _Tokens, []).
extract_SemRep_phrases_1([ap(_NegValue,Phrase,_PhraseMap,Mapping)|Rest],
	                 Tokens, [PhraseWithPos|ExtractedRest]) :-
	Tokens = [tok(_,_,_,pos(StartTok,_EndTok),_)|_],
	add_pos_info_to_phrases(Phrase, Mapping, Tokens, StartTok,PhraseWithPos),
	!,
	extract_SemRep_phrases_1(Rest, Tokens, ExtractedRest).
extract_SemRep_phrases_1([ap(_NegValue,_Phrase,_PhraseMap,[])|Rest],
	                 Tokens, ExtractedRest) :-
	extract_SemRep_phrases_1(Rest, Tokens, ExtractedRest).

add_pos_info_to_phrases([], _Eval, _Tokens, _FromPos, []).
add_pos_info_to_phrases([Item|RestItems], Evaluations, Tokens, FromPos, [ItemPos|RestItemsPos]) :-
	functor(Item, Type, 1),
	arg(1, Item, Elements),
	get_from_list(metaconc, Elements, _Conc:CUI:_SemType),
	get_metaconc_position_and_neg(Evaluations, CUI, Tokens, FromPos,
				      StartPos, EndPos, Negated),
	!,
	functor(ItemPos, Type, 1),
	append(Elements, [negated(Negated),position(StartPos,EndPos)], ElementsWithPos),
	arg(1,ItemPos, ElementsWithPos),
	get_item_position(Item, Tokens, TokensRest, _ItemPos),
	add_pos_info_to_phrases(RestItems, Evaluations, TokensRest, StartPos, RestItemsPos).
add_pos_info_to_phrases([Item|RestItems], Evals, Tokens, FromPos, [ItemPos|RestItemsPos]):-
	get_item_position(Item, Tokens, TokensRest, ItemPos),
	!,
	arg(1, ItemPos, ArgList),
	memberchk(position(_StartPos,EndPos),ArgList),
	( functor(Item,punc,_) ->
	    add_pos_info_to_phrases(RestItems, Evals, TokensRest, EndPos, RestItemsPos)
	; add_pos_info_to_phrases(RestItems, Evals, TokensRest, FromPos, RestItemsPos)
	).
add_pos_info_to_phrases([Item|RestItems], Evals, Tokens, FromPos, [Item|RestItemsPos]) :-
	add_pos_info_to_phrases(RestItems, Evals, Tokens, FromPos, RestItemsPos).

get_metaconc_position_and_neg([Eval|_RestEvals], CUI, Tokens, FromPos, StartPos, EndPos, Negated) :-
	get_all_candidate_features([cui,posinfo,negated], Eval, [CUI,PosInfoList,Negated]),
%	format(user_output,'CUI-PosInfo-Negated: ~q ~q ~q~n', [CUI,PosInfoList,Negated]),
	PosInfoList = [Start/_Len|_],
	( FromPos > Start ->
	   FromPos0 is Start
	; FromPos0 is FromPos
	),
	match_pos_token(PosInfoList, Tokens, FromPos0, MatchingTokens),
	\+ MatchingTokens == [],
	!,
	compute_positions(MatchingTokens, StartPos, EndPos).
get_metaconc_position_and_neg([_Eval|RestEvals], CUI, Tokens, FromPos, StartPos, EndPos, Negated) :-
	get_metaconc_position_and_neg(RestEvals, CUI, Tokens, FromPos, StartPos, EndPos, Negated).



% get_metaconc_position(CUI,[_Eval|RestEval], Tokens, StartPos, EndPos) :-
% 	get_metaconc_position(CUI,RestEval, Tokens, StartPos, EndPos).

match_pos_token([], _Tokens, _FromPos, []) :- !.
match_pos_token(_PosInfoList, [], _FromPos, []) :- !.
match_pos_token([Start/Len|RestPosInfoList], [Token|TokenRest], FromPos, [Token|TokenRestMatch]) :-
	Token = tok(_,_,_,_,pos(StartTok,LenTok)),
	StartTok >= Start,
	StartTok >= FromPos, 
	EndTok is StartTok + LenTok,
	EndPos is Start + Len,
	EndPos =:=  EndTok,
	!,
	match_pos_token(RestPosInfoList,TokenRest,FromPos,TokenRestMatch).
match_pos_token([Start/Len|RestPosInfoList], [Token|TokenRest], FromPos, [Token|TokenRestMatch]) :-
	Token = tok(_,_,_,_,pos(StartTok,LenTok)),
	StartTok >= Start,
	StartTok >= FromPos,
	EndTok is StartTok + LenTok,
	EndPos is Start + Len,
	EndPos >=  EndTok,
	!,
	match_pos_token([Start/Len|RestPosInfoList], TokenRest, FromPos, TokenRestMatch).
match_pos_token(PosInfoList,[_Token|RestTokens], FromPos, MatchTokens) :-
	!,
	match_pos_token(PosInfoList, RestTokens, FromPos, MatchTokens).
match_pos_token([_PosInfo|RestPosInfoList], [_Token|RestTokens], FromPos, MatchTokens) :-
	match_pos_token(RestPosInfoList, RestTokens, FromPos, MatchTokens).

get_item_position(Item, TokensIn, TokensOut, ItemPos) :-
	functor(Item, Type, 1),
	arg(1, Item, ArgList),
	get_from_list(inputmatch, ArgList, InputMatch),
	match_token(TokensIn, InputMatch, MatchingTokens, TokensOut),
	compute_positions(MatchingTokens, Start, End),
	functor(ItemPos, Type, 1),
	append(ArgList, [position(Start,End)], ItemArgList),
	arg(1, ItemPos, ItemArgList),
	!.

match_token(Tokens, [], [], Tokens) :- !.
match_token([Token|RestTokens], [WordAtom|RestInput], [Token|RestMatch], TokensOut) :-
	arg(2, Token, Word),
	atom_codes(WordAtom, Word),
	!,
	match_token(RestTokens, RestInput, RestMatch, TokensOut).
match_token(Tokens, [WordAtom|RestInput], [MatchingToken|RestMatch], TokensOut) :-
	find_word_with_gap(WordAtom,Tokens, TokensBefore, MatchingToken, TokensAfter),
	!,
	append(TokensBefore, TokensAfter, TokensOut0),
	match_token(TokensOut0, RestInput, RestMatch, TokensOut).
match_token([_Token|RestTokens],InputMatch,MatchTokens,TokensOut) :-
	match_token(RestTokens, InputMatch, MatchTokens, TokensOut).

compute_positions(Tokens, StartPos, EndPos) :-
	Tokens = [FirstToken|_],
	arg(5, FirstToken, FirstPos),
	FirstPos = pos(StartPos,_),
%	rev(Tokens, RevTokens),
%	RevTokens = [LastToken|_],
	last(Tokens,LastToken),
	arg(5, LastToken, LastPos),
	LastPos = pos(LastStartPos,Length),
	EndPos is LastStartPos + Length,
%	StartPos is StartPos0 + 1,
	!.

find_word_with_gap(_WordAtom, [], [], [], []) :- !.
find_word_with_gap(WordAtom, [Token|Rest], _TokensBefore, Token, Rest) :-
	arg(2, Token, Word),
	atom_codes(WordAtom, Word),
	!.
find_word_with_gap(WordAtom, [Token|Rest], [Token|BeforeRest], Match, After) :-
	find_word_with_gap(WordAtom, Rest, BeforeRest, Match, After).
	
	
/* disambiguate_mmoutput(MMOutput, -DisambMMOutput)
   extract_mmo_from_utterance(+MMOutput, +UttNum, -MMO, -MarkedMappings, -MarkedMMOutput)
   extract_mmo_phrases(+MMOutput, +UttNum, +PhraseNum, -MMO, -MarkedMappings,
                       -MarkedMMOutput)
   disambiguate_mmo(+MMOIn, -MMOOut)
   reinsert_mmo(+DisambMMO, +MarkedMappings, +MarkedMMOutput, -ModifiedMMOutput)

disambiguate_mmoutput/3 performs word sense disambiguation (WSD) on the
MetaMap output embedded in MMOutput forming DisambMMOutput.
extract_mmo/5 extracts the actual MetaMap output (MMO) from MMOutput
creating MarkedMappings and MarkedMMOutput (with marked mappings (phrases,
respectively) with their zero-based utterance and phrase number) in the
process. It uses extract_mmo_phrases/6.
disambiguate_mmo/2 calls disambiguation on MMOIn and produces MMOOut.
And reinsert_mmo/4 strips MarkedMappings of unwanted senses according to
DisambMMO and inserts the stripped mappings into MarkedMMOutput producing
DisambMMOutput. */

/*

MMOutput and DisambMMOutput are lists of MMO_Terms [MMOTerm1, ..., MMOTermN]

where each MMOTerm corresponds to an utterance in the citation and is of the form

mm_output(ExpSentence, Citation, ModifiedText, Tagging, AA, Syntax, MMOPhrases, ExtractedPhrases)

*/

maybe_disambiguate_mmoutput(MMOutput, WSDServerStream, DisambMMOutput) :-
	( control_option(word_sense_disambiguation) ->
	  disambiguate_mmoutput(MMOutput, WSDServerStream, DisambMMOutput)
	; DisambMMOutput = MMOutput
	).

disambiguate_mmoutput(MMOutput, _WSDServerStream, DisambMMOutput) :-
	unambiguous_output(MMOutput, _UtteranceText),
	!,
	% format('~n~n### Unambiguous utterance: ~w~n~n', [UtteranceText]),
	DisambMMOutput = MMOutput.
disambiguate_mmoutput(MMOutput, WSDServerStream, DisambMMOutput) :-
	InitialUtteranceNumber is 1,
	extract_mmo_from_utterance(MMOutput, Citation,
				   InitialUtteranceNumber,
				   MMO, [],
				   MarkedMappings, [],
				   MarkedMMOutput),
	MMOutput = [mm_output(_Utt,_Cit,_ModTxt,_Tag,AAs,_Stx,_MMOPhrases,_ExtrPhrases)|_],
	% MarkedMappings is now built up using difference lists; no need to append!
	disambiguate_mmo([Citation,AAs|MMO], WSDServerStream, DisambMMO),
	reinsert_mmo(DisambMMO, MarkedMappings, MarkedMMOutput, DisambMMOutput),
	!.
disambiguate_mmoutput(MMOutput, _WSDServerStream, MMOutput) :-
	format('disambiguate_mmoutput/2 failed for:~n~p~nIgnoring any disambiguations.~n',
	       [MMOutput]).

unambiguous_output([MMOutput], UtteranceText) :-
	MMOutput = mm_output(utterance(_Label, UtteranceTextString, _PosInfo, _ReplPos),
			     _Cit,_ModTxt,_Tag,_AAs,_Stx,MMOPhrases,_ExtrPhrases),
	atom_codes(UtteranceText, UtteranceTextString),
	all_unambiguous_phrases(MMOPhrases).

all_unambiguous_phrases([]).
all_unambiguous_phrases([FirstPhrase|RestPhrases]) :-
	unambiguous_phrase(FirstPhrase),
	all_unambiguous_phrases(RestPhrases).

unambiguous_phrase(PhraseTerm) :-
	PhraseTerm = phrase(_Phrase,_Candidates,Mappings,_PWI,_GVCs,_EV0,_APhrases),
	Mappings = mappings(MapTermList),
	length(MapTermList, Length),
	Length =< 1.

% Avoid WSD machinery if one of the candidate mappings
% has as its synonym the same string as the preferred name
% First implemented for "Infective endocarditis", 
% which was ambiguous between "Infective endocarditis" and "Bacterial endocarditis"
preferred_name_synonym_equality([],[]):- !.
preferred_name_synonym_equality([MMOPhrase|RestMMOPhrases],[MMOPhraseOut|RestMMOPhrasesOut]) :-
	MMOPhrase = phrase(Phrase,Candidates,Mappings,PWI,GVCs,EV0,APhrases),
	Mappings = mappings(MapTermList),
	preferred_name_synonym_equal(MapTermList,MapTerm),
	update_aps(APhrases,MapTerm,APhrasesOut),
	!,
	MMOPhraseOut = phrase(Phrase,Candidates,mappings([MapTerm]),PWI,GVCs,EV0,APhrasesOut),
	preferred_name_synonym_equality(RestMMOPhrases,RestMMOPhrasesOut).
preferred_name_synonym_equality([MMOPhrase|RestMMOPhrases],[MMOPhrase|RestMMOPhrasesOut]) :-
	preferred_name_synonym_equality(RestMMOPhrases,RestMMOPhrasesOut).


preferred_name_synonym_equal([MapTerm|_MapTermRest],MapTerm) :-
	MapTerm = map(_NegScore,[ev(_,_,Syn,Syn,_,_,_,_,_,_,_,_,_,_,_)]),
	!.
% I implemented this thinking there was a problem with equality when the synonym was X and the preferred name 'entire X'
% but it turned out to be a problem with the arity of MapTerm. But I am keeping this here, just in case
% Halil -- 12/15/2017
%preferred_name_synonym_equal([MapTerm|_MapTermRest],MapTerm) :-
%       format(user_output,'MapTerm: ~q~n', [MapTerm]),
%        MapTerm = map(_NegScore,[ev(_,_,PrefName,Syn,_,_,_,_,_,_,_,_,_,_,_)]),
%	format(user_output,'Pref Name: ~q~n~q~n', [PrefName,Syn]),
%	atom_to_list(Syn,SynList,SynLen),
%	atom_to_list(PrefName,PrefList,PrefLen),
%	SynLen >= 1,
%	intersection(SynList,PrefList,Intersection),
%	length(Intersection,SynLen),
%	PrefLen =:= SynLen + 1,
%	nth0(0,PrefList,'entire'),
%	\+ nth0(0,SynList,'entire'),
%	!.
preferred_name_synonym_equal([_MapTerm|MapTermRest],MapTermOut) :-
	preferred_name_synonym_equal(MapTermRest,MapTermOut).

% Apply the update to APhrases, since it is later used by extract_SemRep_phrases

%%% Halil and FML reviewed this code on 08/23/2016 and concluded that it's broken
%%% because it does no matching between the APhrases and Mappings.
%%% The intent (I hope!) is simply to discard and aphrase(...) term
%%% whose mapping(...) component is not in MapTermList.
%%% update_aps_list(aphrases(APhrases), MapTermList, ModMapTermList,
%%% 		aphrases(APhrasesOut)) :-
%%% 	update_aps_list_aux(APhrases,MapTermList,ModMapTermList,APhrasesOut),
%%% 	!.
%%% 
%%% update_aps_list_aux(_,[],[],[]) :- !.
%%% update_aps_list_aux([APhrase|RestAPhrases],[MapTerm|RestMapTerm],
%%% 		    [MapTerm|RestModMapTerm],[APhrase|RestAPhrasesOut]) :-
%%% 	!,
%%% 	update_aps_list_aux(RestAPhrases,RestMapTerm,RestModMapTerm,RestAPhrasesOut).
%%% update_aps_list_aux(APhrases,[_MapTerm|RestMapTerm],RestModMapTerm,APhrasesOut) :-
%%% 	update_aps_list_aux(APhrases,RestMapTerm,RestModMapTerm,APhrasesOut).

% If there are no more APhrases, terminate with [].
update_aphrase_list([], _MapTermList, []).
% If there are no more Mappings, discard all remaining APhrases.
update_aphrase_list(_APhrases, [], []) :- !.
update_aphrase_list([APhrase|RestAPhrases],[MapTerm|RestMapTerms], [APhrase|RestAPhrasesOut]) :-
	APhrase = ap(_NegValue,_Phrase,_PhraseMap,Mapping),
	MapTerm = map(_NegMapScore,Mapping),
	!,
	update_aphrase_list(RestAPhrases, RestMapTerms, RestAPhrasesOut).
update_aphrase_list([_APhrase|RestAPhrases], MapTerms, APhrasesOut) :-
	update_aphrase_list(RestAPhrases, MapTerms, APhrasesOut).

%%% different predicate

update_aps(aphrases(APhrases),MapTerm,aphrases(APhrasesOut)) :-
	update_aps_aux(APhrases,MapTerm,APhrasesOut),
	!.

update_aps_aux([APhrase|_RestAPhrases],MapTerm, [APhrase]) :-
	APhrase = ap(_NegValue,_Phrase,_PhraseMap,Mapping),
	MapTerm = map(_NegMapScore,Mapping),
	!.
update_aps_aux([_APhrase|RestAPhrases],MapTerm,APhrasesOut) :-
	update_aps_aux(RestAPhrases,MapTerm,APhrasesOut).

% If one of the candidates has the semantic type ftcn (Functional Concept) or inpr (Intellectual Product)
% ignore it,
% This is based on Antonio's work, so it his WSD method (AEC) is used, this should already be
% done, but we still want to do it in general.
remove_ftcn_inpr_semtypes([],[]):- !.
remove_ftcn_inpr_semtypes([MMOPhrase|RestMMOPhrases],[MMOPhraseOut|RestMMOPhrasesOut]) :-
	MMOPhrase = phrase(Phrase,Candidates,Mappings,PWI,GVCs,EV0,APhrases),
	Mappings = mappings(MapTermList),
	length(MapTermList,Len),
	Len > 1,
	% Mappings in Mappings term and APhrases are ordered differently
	% Since update_aps_list looks at term differences later to reconcile mappings
	% we first sort the mappings in Mappings term
	APhrases = aphrases(APhraseListIn),
	sort_by_aphrases(APhraseListIn, MapTermList, MapTermList, MapTermListSorted),
	remove_ftcn_inpr_evaluations(MapTermListSorted, MapTermListSorted, MapTermListSortedOut),
	%%% See comment above immediately before definition of update_aphrase_list/3
	%%% update_aps_list(APhrases,MapTermListSorted,MapTermListSortedOut,APhrasesOut),
	update_aphrase_list(APhraseListIn, MapTermListSortedOut, APhraseListOut),
	APhrasesOut = aphrases(APhraseListOut),
	% And now, mappings list is out of whack, so correct that, as well.
	% The reason is the order of mappings matter for the rest of WSD process 
	% If we do not correct the order here, we will get different WSD results
	% this got too convoluted, investigate a simpler way
	update_mapping_list(MapTermList,MapTermListSortedOut,MapTermListOut),
	!,
	MMOPhraseOut = phrase(Phrase,Candidates,mappings(MapTermListOut),PWI,GVCs,EV0,APhrasesOut),
	remove_ftcn_inpr_semtypes(RestMMOPhrases,RestMMOPhrasesOut).
remove_ftcn_inpr_semtypes([MMOPhrase|RestMMOPhrases],[MMOPhrase|RestMMOPhrasesOut]) :-
	remove_ftcn_inpr_semtypes(RestMMOPhrases,RestMMOPhrasesOut).

sort_by_aphrases([],_RestMapTermList,_MapTermList,[]) :- !.
sort_by_aphrases([APhrase|RestAPhrases],[MapTerm|_RestMapTermList],MapTermList,[MapTerm|RestMapTermListOut]) :-
	APhrase = ap(_NegValue,_Phrase,_PhraseMap,Mapping),
	MapTerm = map(_NegMapScore,Mapping),
	!,
	sort_by_aphrases(RestAPhrases,MapTermList,MapTermList,RestMapTermListOut).
sort_by_aphrases(APhrases,[_MapTerm|RestMapTermList],MapTermList,MapTermListOutSorted) :-
	sort_by_aphrases(APhrases,RestMapTermList,MapTermList,MapTermListOutSorted).

update_mapping_list([],_SortedList,[]) :- !.
update_mapping_list([MapTerm|RestMapTerms],SortedList,[MapTerm|RestMapTermsOut]) :-
	memberchk(MapTerm,SortedList),
	!,
	update_mapping_list(RestMapTerms,SortedList,RestMapTermsOut).
update_mapping_list([_MapTerm|RestMapTerms],SortedList,RestMapTermsOut) :-
	update_mapping_list(RestMapTerms,SortedList,RestMapTermsOut).

% remove map(_) terms if they have ftcn/inpr ambiguity
% need to keep the integrity of the map(_) term, so we need to look at other map(_) terms.
remove_ftcn_inpr_evaluations([],_MapTerms,[]) :- !.
remove_ftcn_inpr_evaluations([MapTerm|MapTermRest],MapTermList,MapTermRestOut) :-
	MapTerm = map(_NegScore,Evaluations),
	append(Preceding,[MapTerm|MapTermRest],MapTermList),
	append(Preceding,MapTermRest,OtherMapTerms),
	adjust_evaluations(Evaluations,OtherMapTerms,EvaluationsOut),
%	EvaluationsOut == [],
	\+ Evaluations == EvaluationsOut,
	!,
	remove_ftcn_inpr_evaluations(MapTermRest,MapTermList,MapTermRestOut).
%remove_ftcn_inpr_evaluations([MapTerm|MapTermRest],MapTermList,[MapTermOut|MapTermRestOut]) :-
%	MapTerm = map(NegScore,Evaluations),
%	adjust_evaluations(Evaluations,MapTermList,EvaluationsOut),
%	MapTermOut = map(NegScore,EvaluationsOut),
%	!,
%	remove_ftcn_inpr_evaluations(MapTermRest,MapTermList,MapTermRestOut).
remove_ftcn_inpr_evaluations([MapTerm|MapTermRest],MapTermList,[MapTerm|MapTermRestOut]) :-
	remove_ftcn_inpr_evaluations(MapTermRest,MapTermList,MapTermRestOut).

% MapTermList consists of other map(_) terms and is used for comparison.
adjust_evaluations([],_MapTermList,[]) :- !.
adjust_evaluations([Evaluation|EvaluationRest],MapTermList,EvaluationRestOut) :-
	Evaluation = ev(_,_,_,_,_,SemTypeList,_,_,_,_,_,_,PosList,_,_),
	subtract(SemTypeList,[inpr,ftcn],Diff),
	length(Diff,0),
	ambiguous_with_non_ftcn_inpr(PosList,Evaluation,MapTermList),
	!,
	adjust_evaluations(EvaluationRest,MapTermList,EvaluationRestOut).
adjust_evaluations([Evaluation|EvaluationRest],MapTermList,[Evaluation|EvaluationRestOut]) :-
	adjust_evaluations(EvaluationRest,MapTermList,EvaluationRestOut).

% Is the given Evaluation with the PosList ambiguous with non-{ftcn/inpr} concepts?
ambiguous_with_non_ftcn_inpr(PosList,Evaluation,[MapTerm|_Rest]) :-
	MapTerm = map(_NegScore,Evaluations),
	ambiguous_with_non_ftcn_inpr_aux(PosList,Evaluation,Evaluations),
	!.
ambiguous_with_non_ftcn_inpr(PosList,Evaluation,[_MapTerm|Rest]) :-
	ambiguous_with_non_ftcn_inpr(PosList,Evaluation,Rest).

ambiguous_with_non_ftcn_inpr_aux(PosList,Evaluation,[Eval|_RestEval]) :-
	Evaluation = ev(_,_,_,_,_,_,_,_,_,_,_,_,PosList,_,_),
	Eval = ev(_,_,_,_,_,SemTypeList,_,_,_,_,_,_,PosList,_,_),
	\+ Evaluation == Eval,
	subtract(SemTypeList,[inpr,ftcn],Diff),
	length(Diff,DiffLen), DiffLen > 0.
ambiguous_with_non_ftcn_inpr_aux(PosList,Evaluation,[_Eval|RestEval]) :-
	ambiguous_with_non_ftcn_inpr_aux(PosList,Evaluation,RestEval).

/*
extract_mmo/8 takes MMOutput, a list of MMO_Terms [MMOTerm1, MMOTerm2, ..., MMOTermN]

where each MMOTerm corresponds to an utterance in the citation and is of the form

mm_output(ExpSentence, Citation, ModifiedText, Tagging, AAs, Syntax, MMOPhrases, ExtractedPhrases)

The only component of the mm_output structure we care about is MMOPhrases,

which is a list [PhraseTerm1, PhraseTerm2, ..., PhraseTermN]

where each PhraseTerm is of the form

phrase(phrase(_), candidates(_), mappings(_), pwi(_), gvcs(_), ev0(_), aphrases(_))

extract_mmo/8 produces three data structures:
(1) MMO, which gets sent to the WSD client.
MMO is a list of the following form:

[utterance(_), phrase(_), candidates(_), mappings(_), 
	       phrase(_), candidates(_), mappings(_), 
	       ...
 'EOU',
 utterance(_), phrase(_), candidates(_), mappings(_), 
	       phrase(_), candidates(_), mappings(_), 
	       ...
 'EOU',
 ...
 ]

(2) MarkedMappings, a list of terms of the form
    markedmappings(UttNum,PhraseNum,Mappings,APhrases), and

(3) MarkedMMOutput, a list like MMOutput of terms of the form
    mm_output(Utterance,Citation,ModifiedText,Tagging,AAs,Syntax,MarkedMMOPhrases,ExtractedPhrases)
    MarkedMMOPhrases is like MMOPhrases, except that instead of
    phrase(phrase(_), candidates(_), mappings(_), pwi(_), gvcs(_), ev0(_), aphrases(_))
    the terms are of the form
    markedphrase(UttNum,PhraseNum,Phrase,Candidates,Mappings,PWI,GVCs,EV0,APhrases)

*/

% extract_mmo_from_utterance is called once per utterance to create 
% (1) the MMO to be sent to the WSD client,
% (2) the MarkedMappings, and
% (3) the MarkedMMOutput

extract_mmo_from_utterance([], _Citation, _UtteranceNum,
			   MMO, MMO, MarkedMappings, MarkedMappings, []).
extract_mmo_from_utterance([MMOutput|RestMMOutput], Citation, UtteranceNum,
			   MMOIn, MMOOut,
			   MarkedMappingsIn, MarkedMappingsOut,
			   [MarkedMMOutput|RestMarkedMMOutput]) :-
	MMOutput =
           mm_output(_Utterance,Citation,_ModifiedText,_Tagging,
		     _AAs,_Syntax,_MMOPhrases,_ExtractedPhrases),
	extract_mmo_aux(MMOutput, UtteranceNum,
			MMOIn, MMONext,
			MarkedMappingsIn, MarkedMappingsNext,
			MarkedMMOutput),
	NextUtteranceNum is UtteranceNum + 1,
	extract_mmo_from_utterance(RestMMOutput, Citation, NextUtteranceNum,
				   MMONext, MMOOut,
				   MarkedMappingsNext, MarkedMappingsOut,
				   RestMarkedMMOutput).
	   
extract_mmo_aux(MMOutput, UttNum,
		[Utterance|MMOIn], MMOOut,
		MarkedMappingsIn, MarkedMappingsOut,
		MarkedMMOutput) :-
	% Identify the MMOPhrases component of the MMOutput term
	MMOutput =
           mm_output(Utterance,Citation,ModifiedText,Tagging,AAs,Syntax,MMOPhrases,ExtractedPhrases),
	InitialPhraseNumber is 1,
	extract_mmo_phrases(MMOPhrases, UttNum, InitialPhraseNumber, MMOIn, MMOOut,
			    MarkedMappingsIn, MarkedMappingsOut, MarkedMMOPhrases),
	% MarkedMMOPhrases is a list just like MMOPhrases, except that each term
	% phrase(Phrase,Candidates,Mappings,PWI,GVCs,EV0,APhrases)
	% is replaced by a term with the UtteranceNumber and the PhraseNumber
	% phrase(UttNum,PhraseNum,Phrase,Candidates,Mappings,PWI,GVCs,EV0,APhrases)
	% MarkedMMOutput is a copy of MMOutput, but with MarkedMMOPhrases instead of MMOPhrases
	MarkedMMOutput =
           mm_output(Utterance,Citation,ModifiedText,Tagging,
		     AAs,Syntax,MarkedMMOPhrases,ExtractedPhrases).
	% temp
	%format('~nextract_mmo--~nUtterance: ~p~nModifiedText: ...~nTagging: ...~nSyntax: ...~nMMOPhrases: ~p~nMarkedMMOPhrases: ~p~n',[Utterance,MMOPhrases,MarkedMMOPhrases]),
	% Add the 'EOU' token in the base case of extract_mmo_phrases
	% append(MMO0, ['EOU'], MMO1),
	% MMOIn = [Utterance|MMOTemp],
	% temp
	%format('~nMMO: ~p~n',[MMO]),

% MarkedMMOPhrases adds UttNum and PhraseNum to phrases
extract_mmo_phrases([], _UttNum, _PhraseNum, ['EOU'|MMO], MMO, MarkedMappings, MarkedMappings, []).
extract_mmo_phrases([Phrase|Rest],
		    UttNum, PhraseNum, MMOIn, MMOOut,
		    MarkedMappingsIn, MarkedMappingsOut,
		    [MarkedPhrase|MarkedPhraseRest]) :-
	Phrase = phrase(Phrase1,Candidates,Mappings,PWI,GVCs,EV0,APhrases),
	MMOIn = [Phrase1,Candidates,Mappings|MMONext],
	MarkedPhrase =
            markedphrase(UttNum,PhraseNum,Phrase1,Candidates,Mappings,PWI,GVCs,EV0,APhrases),
	MarkedMappingsIn = [markedmappings(UttNum,PhraseNum,Mappings,APhrases)|MarkedMappingsNext],
	NextPhraseNum is PhraseNum + 1,
	extract_mmo_phrases(Rest, UttNum, NextPhraseNum, MMONext, MMOOut,
			    MarkedMappingsNext, MarkedMappingsOut, MarkedPhraseRest).

disambiguate_mmo(MMOTermList, WSDServerStream, DisambMMO) :-
	( call_WSD(MMOTermList, WSDServerStream, RawDisambMMOString) ->
	  test_parse_disamb_mmo(RawDisambMMOString, DisambMMO)
	; fatal_error('call_WSD/2 could not process:~n~p~n', [MMOTermList])
	).

test_parse_disamb_mmo(RawDisambMMOString, DisambMMO) :-
	( parse_disamb_mmo(RawDisambMMOString, DisambMMO) ->
	  true
	; atom_codes(RawDisambMMOAtom, RawDisambMMOString),
	  fatal_error('disambiguate_mmo/2 could not parse:~n~p~n', [RawDisambMMOAtom])
	  ).

/*
RawDisambMMO is an atom of the form
00000000.tx.1|0|0|[Acute - Triage Code$Admission Level of Care Code - Acute$acute]|[acute]|
00000000.tx.1|0|5|[Changed status$Changing]|[Changing]|
00000000.tx.2|1|1|[Clinical Research$Room of building - Study$Scientific Study]|[Scientific Study]|

parse_disamb_mmo/2 parses RawDisambMMO into a list of terms

[disamb('00000000.tx.1',0,0,
	['Acute - Triage Code','Admission Level of Care Code - Acute',acute],
	[acute]),
 disamb('00000000.tx.1',0,5,
	['Changed status','Changing'],
	['Changing']),
 disamb('00000000.tx.2',1,1,
	['Clinical Research','Room of building - Study','Scientific Study'],
	['Scientific Study'])
 ]

*/

parse_disamb_mmo(RawDisambMMO, DisambMMO) :-
	% first split into lines using "^J" as separator character
	split_string_completely(RawDisambMMO, [10], RawDisambMMOLines),
	% temp
	%    length(RawDisambMMOLines,NRDML),
	%    format('parse_disamb_mmo/2: ~d RawDisambMMOLines = ~p~n',[NRDML,RawDisambMMOLines]),
	parse_disamb_mmo_aux(RawDisambMMOLines, DisambMMO).

parse_disamb_mmo_aux([], []).
parse_disamb_mmo_aux([[]|Rest], DisambRest) :-
	!,
	parse_disamb_mmo_aux(Rest, DisambRest).
parse_disamb_mmo_aux([First|Rest], Result) :-
	% First, split each line into fields using "|" as separator character
	split_string_completely(First, "|", Fields),
	% temp
	%    length(Fields,NFields),
	%    format('parse_disamb_mmo_aux fields: (~d) ~p~n',[NFields,Fields]),
	( Fields==["None."] ->  % There is nothing to disambiguate
	  Result=DisambRest
        ; Fields=[Label0,I0,N0,AllSenses0,DisambSenses0,_],
          ( append("[Error JDI",_,DisambSenses0) ->
	    fatal_error('parse_disamb_mmo_aux/2 failed ~p on ~p~n', [DisambSenses0,First])
	  ; true
	  ),
          ( ( DisambSenses0 == "[No match found.]"
            ; DisambSenses0 == "[None of the Above]"
            ; DisambSenses0 == "[JDI unable to disambiguate input]"
            ) ->
	    Result = DisambRest,
	    debug_message('WSD', '~n### WSD response: ~s~n', [DisambSenses0])
	  ; atom_codes(Label, Label0),
	    number_codes(I, I0),
	    number_codes(N, N0),
	    parse_senses(AllSenses0, AllSenses),
	    parse_senses(DisambSenses0, DisambSenses),
	    % temp
	    % format('  AllSenses: ~p~n',[AllSenses]),
	    % format('  DisambSenses: ~p~n',[DisambSenses]),
	    Result = [disamb(Label,I,N,AllSenses,DisambSenses)|DisambRest],
	    length(AllSenses, NumWSDInputs),
	    length(DisambSenses, NumWSDOutputs),
	    debug_message('WSD', '~n### WSD inputs (~d): ~w~n', [NumWSDInputs,AllSenses]),
	    debug_message('WSD', '### WSD output (~d): ~w~n~n',  [NumWSDOutputs,DisambSenses])

	  )
      ),
      !,
      parse_disamb_mmo_aux(Rest, DisambRest).
parse_disamb_mmo_aux([_First|Rest], DisambRest) :-
	% temp
	parse_disamb_mmo_aux(Rest, DisambRest).

% parse_senses("[]", []) :- !.
parse_senses(Senses0, Senses) :-
	Senses0 = [_|RestSensesChars],
	last(Senses1, _LastSensesChar, RestSensesChars),
	phrase(term_list(Senses), Senses1).

% ---------------------------------------------------------
% -------------- $-separated term list grammar --------------
% ---------------------------------------------------------

term_list(Ts) -->
          term(T), "$", !, term_list(Us), {Ts=[T|Us]}
	| term(T), {Ts=[T]}.

term(T) --> term_chars(Cs), !, {atom_codes(T,Cs)}.

term_chars(Cs) -->
	  [C], {is_term_char(C)}, !, term_chars(Ds), {Cs=[C|Ds]}
	| {Cs=[]}.

is_term_char(C) :- \+is_non_term_char(C).

is_non_term_char(0'$).
%is_non_term_char(0'[). % not really necessary
%is_non_term_char(0']).

% DisambMMO is a list of terms of the form
% disamb('00000000.tx.1',0,5,['Changed status','Changing'],['Changing'])

% MarkedMappings is a list of terms of the form
% markedmappings(UttNum,PhraseNum,Mappings,APhrases)
% We need the UttNum/PhraseNum to know from what Utt/Phrase mappings
% to remove senses ruled out by WSD

% MarkedMMOutput a list like MMOutput of terms of the form
% mm_output(Utterance,Citation,ModifiedText,Tagging,AAs,Syntax,MarkedMMOPhrases,ExtractedPhrases)

% in which MarkedMMOPhrases is a list of terms of the form
% phrase(UttNum,PhraseNum,Phrase,Candidates,Mappings,PWI,GVCs,EV0,APhrases)

reinsert_mmo(DisambMMO, MarkedMappings, MarkedMMOutput, DisambMMOutput) :-
	% temp
	% format('MarkedMappings: ~p~n~n',[MarkedMappings]),
	strip_mappings(DisambMMO, MarkedMappings, StrippedMarkedMappings),
	% temp
	% format('StrippedMarkedMappings: ~p~n~n',[StrippedMarkedMappings]),
	reinsert_mmoutput(MarkedMMOutput, StrippedMarkedMappings, DisambMMOutput),
	% temp
	%format('MarkedMMOutput:~p~n~nDisambMMOutput: ~p~n~n',
	%       [MarkedMMOutput,DisambMMOutput]),
	!.
% temp
reinsert_mmo(DisambMMO, _MarkedMappings, MarkedMMOutput, MarkedMMOutput) :-
	%    fatal_error('reinsert_mmo/4 failed for~nDisambMMO: ~p~nMarkedMappings: ~p~nMarkedMMOutput: ~p~nContinuing...~n',
	%	   [DisambMMO,MarkedMappings,MarkedMMOutput]).
	fatal_error('reinsert_mmo/4 failed for~nDisambMMO: ~p~n', [DisambMMO]).


% The cut in the base clause is necessary, because the third clause has a var first arg.
strip_mappings([], MarkedMappings, MarkedMappings) :- !.
% This clause fires when
% the disamb term's UttNum/PhraseNum match
% the markedmappings term's UttNum/PhraseNum
strip_mappings([disamb(_Label,UttNum,PhraseNum,AllSenses,SensesToKeep)|RestDisambs],
	       [markedmappings(UttNum,PhraseNum,Mappings,APhrases)|RestMarkedMappings],
	       StrippedMarkedMappings) :-
	!,
	subtract(AllSenses, SensesToKeep, SensesToStrip),
	% temp
	%format('  SensesToStrip: ~p~n',[SensesToStrip]),
	% temp; the number of disambiguated senses is no longer restricted to one
	%    length(Senses,NS),
	%    length(SensesToStrip,NSTS),
	%    NDiff is NS-NSTS,
	%    (NDiff=:=1 ->
	%	true
	%    ;   fatal_error('strip_mappings/3 found bad senses (~p) with sense ~p~n',
	%	       [Senses,Sense])
	%    ),
	strip_mappings_aux(SensesToStrip, SensesToKeep, Mappings, StrippedMappings),
	% Presumably, the aphrases that need to be stripped are those
	% in the same positions as the mappings that were stripped?
	strip_aphrases(SensesToStrip, SensesToKeep, APhrases, StrippedAPhrases),
	% temp
	%Mappings=mappings(M),
	%length(M,LM),
	%StrippedMappings=mappings(SM),
	%length(SM,LSM),
	%StrippedAPhrases=aphrases(SAP),
	%length(SAP,LSAP),
	%format('~nstrip_mappings/3: Sense = ~p~n',[Sense]),
	%format('Numbers: ~d ~d ~d~n',[LM,LSM,LSAP]),
	%format('StrippedMappings: ~p~n',[StrippedMappings]),
	%format('StrippedAPhrases: ~p~n',[StrippedAPhrases]),

	% If there is another disambiguation for this (utterance,phrase),
	% update Mappings and APhrases to StrippedMappings and StrippedAPhrases
	% in the current markedmappings term, and recurse with the next disamb term
	% and the updated current markedmappings plus the rest of the markedmappings;
	% otherwise, recurse with the next disamb term and the rest of the markedmappings.
	( RestDisambs = [disamb(_,UttNum,PhraseNum,_,_)|_] ->
	  strip_mappings(RestDisambs,
	  		 [markedmappings(UttNum,PhraseNum,StrippedMappings,StrippedAPhrases)
		           |RestMarkedMappings],
		         StrippedMarkedMappings)
    	; StrippedMarkedMappings = 
		[markedmappings(UttNum,PhraseNum,StrippedMappings,StrippedAPhrases)
	          | RestStrippedMarkedMappings],
	  strip_mappings(RestDisambs,RestMarkedMappings,RestStrippedMarkedMappings)
        ).
strip_mappings(DisambMMO, [FirstMarkedMapping|RestMarkedMappings],
	       [FirstMarkedMapping|RestStrippedMarkedMappings]) :-
	strip_mappings(DisambMMO, RestMarkedMappings, RestStrippedMarkedMappings).

/*
  Suppose WSD returns [Age$Elderly]|[Age],
  meaning that where an Age/Elderly ambiguity exists,
  strip out the Elderly sense, and keep the Age sense.

  Let's call
  ['Elderly'] SensesToStrip
  and
  ['Age'] SensesToKeep.

  Suppose further that Maps is a list of terms like this:

[ map(-888,[
    ev(-694, C0001792, Elderly, Elderly, [elderly], [aggp], [[[1,1],[1,1],0]], no,  no, Srcs, PI),
    ev(-861, C0001779, Age,     Age,     [age],     [orga], [[[2,2],[1,1],0]], yes, no, Srcs, PI)]),
  map(-888,[
    ev(-694, C0001792, Elderly, Elderly, [elderly], [aggp], [[[1,1],[1,1],0]], no,  no, Srcs, PI),
    ev(-861, C0001792, age,     Elderly, [age],     [aggp], [[[2,2],[1,1],0]], yes, no, Srcs, PI)])]) ]

What has historically been done here is that a map(NegValue, EVList) term was stripped
iff one of the ev terms in its EVList had one of the SensesToStrip as its fourth argument.
In the example above, BOTH map terms would be stripped, because both map terms' EVList
contains an ev term with 'Elderly' as a fourth argument.

This is too strict a test, because in this (real) case, ALL senses would be stripped.

We need instead to strip out a map term MT1 iff
(1) Its EVList contains an ev term whose fourth argument
    is one of the SensesToStrip, AND
(2) Another map term (MT2)'s EVList contains an ev term whose fourth argument
    is one of the SensesToKeep, AND
(3) Both the ev terms are in the same relative position in the MT1's and MT2's EVLists.

*/

strip_mappings_aux([], _SensesToKeep, Mappings, Mappings).
strip_mappings_aux([SenseToStrip|RestSensesToStrip], SensesToKeep,
		   mappings(Maps), mappings(StrippedMaps)) :-
	strip_maps(Maps, [], [SenseToStrip|RestSensesToStrip], SensesToKeep, StrippedMaps).

strip_maps([], _MapsToKeep, _SensesToStrip, _SensesToKeep, []).
% Strip this map
strip_maps([FirstMap|RestMaps], MapsToKeep, SensesToStrip, SensesToKeep, RestStrippedMaps) :-
	map_contains_sense_to_strip(FirstMap, RestMaps, MapsToKeep, SensesToStrip, SensesToKeep),
	!,
	strip_maps(RestMaps, MapsToKeep, SensesToStrip, SensesToKeep, RestStrippedMaps).
% Keep this map by putting it on the MapsToKeep List.
% We need to do that to allow it to match against a later map.
strip_maps([FirstMap|RestMaps], MapsToKeep,
	   SensesToStrip, SensesToKeep, [FirstMap|RestStrippedMaps]) :-
	strip_maps(RestMaps, [FirstMap|MapsToKeep],
		   SensesToStrip, SensesToKeep, RestStrippedMaps).

% ThisMap contains one of SensesToStrip; in addition,
% the Nth EVTerm in ThisMap's EV list had one of the SensesToStrip as its 4th arg
% AND
% The Nth EVTerm in the EV list of one of the other maps,
%  * either one we've already seen and decided to keep (MapsToKeep), or
%  * one we haven't seen yet (RestMaps)
% had one of the SensesToKeep as its 4th arg.

map_contains_sense_to_strip(ThisMap, RestMaps, MapsToKeep,
			    SensesToStrip, SensesToKeep) :-
	ThisMap = map(_ThisMapNegScore, ThisMapEVs),
	evs_contain_sense(ThisMapEVs, 1, MatchingPosition, SensesToStrip),
	% do NOT change these calls to member/2 to calls to memberchk/2!!
	( member(OtherMap, RestMaps)
        ; member(OtherMap, MapsToKeep)
        ),
	OtherMap = map(_OtherMapNegScore, OtherMapEVs),
	evs_contain_sense(OtherMapEVs, 1, MatchingPosition, SensesToKeep),
	!.

% :- nondet evs_contain_sense/4.

% At least one of Evs contains at least one of SensesToStrip
evs_contain_sense([Candidate|_RestEVs], Position, Position, SensesToStrip) :-
	get_candidate_feature(metaconcept, Candidate, Concept),
	% ev(_,_,_,Concept,_,_,_,_,_,_Srcs,_PosInfo)|_RestEVs
	memberchk(Concept, SensesToStrip).
	% Must NOT have a cut here because ev_contains_sense has to be backtrackable
evs_contain_sense([_First|Rest], CurrentPosition, MatchingPosition, SensesToStrip) :-
	NextPosition is CurrentPosition + 1,
	evs_contain_sense(Rest, NextPosition, MatchingPosition,SensesToStrip).

% strip_aphrases is identical to strip_mappings_aux,
% other than the form of the data structures used.
% The two predicates could be combined via paramaterization,
% but I have chosen not to in order to maintain declaratory clarity.

strip_aphrases([], _SensesToKeep, APhrases, APhrases).
strip_aphrases([Sense|RestSenses], SensesToKeep,
		aphrases(APs), aphrases(StrippedAPs)) :-
	strip_aps(APs, [], [Sense|RestSenses], SensesToKeep, StrippedAPs).

strip_aps([], _APsToKeep, _SensesToStrip, _SensesToKeep, []).
% Strip this APhrase
strip_aps([FirstAP|RestAPs], APsToKeep, SensesToStrip, SensesToKeep, RestStrippedAPs) :-
	ap_contains_sense_to_strip(FirstAP, RestAPs, APsToKeep, SensesToStrip, SensesToKeep),
	!,
	strip_aps(RestAPs, APsToKeep, SensesToStrip, SensesToKeep, RestStrippedAPs).
strip_aps([FirstAP|RestAPs], APsToKeep,
	  SensesToStrip, SensesToKeep, [FirstAP|RestStrippedAPs]) :-
	strip_aps(RestAPs, [FirstAP|APsToKeep],
		  SensesToStrip, SensesToKeep, RestStrippedAPs).

% The ap contains at least one of Senses, to be more accurate
ap_contains_sense_to_strip(ThisAP, RestAPs, APsToKeep,
			   SensesToStrip, SensesToKeep) :-
	ThisAP = ap(_ThisAPNegScore,_ThisAPLinearPhrase,_ThisAPLinearPhraseMap,ThisAPEVs),
	evs_contain_sense(ThisAPEVs, 1, MatchingPosition, SensesToStrip),
	% do NOT change these calls to member/2 to calls to memberchk/2!!
	( member(OtherAP, RestAPs)
	; member(OtherAP, APsToKeep)
	),
	OtherAP = ap(_OtherAPNegScore,_OtherAPLinearPhrase,_OtherAPLinearPhraseMap,OtherAPEVs),
	evs_contain_sense(OtherAPEVs, 1, MatchingPosition, SensesToKeep).

reinsert_mmoutput([], [], []) :- !.
reinsert_mmoutput([mm_output(Utterance,Citation,ModifiedText,Tagging,
			     AAs,Syntax,MMOPhrases,_ExtractedPhrases)
		   |RestMMOutput],
		  StrippedMarkedMappingsIn,
		  [mm_output(Utterance,Citation,ModifiedText,Tagging,
			     AAs,Syntax,ReinsertedMMOPhrases,ReinsertedExtractedPhrases)
		   |RestReinserted]) :-
	% temp
	% format('~nreinsert_mmoutput/3:~nExtractedPhrases: ~p~n',[ExtractedPhrases]),
	reinsert_mmoutput_aux(MMOPhrases,
			      StrippedMarkedMappingsIn, StrippedMarkedMappingsNext,
			      ReinsertedMMOPhrases, ReinsertedExtractedPhrases),
	!,
	% temp
	%format('reinsert_mmoutput/3:~nReinsertedExtractedPhrases: ~p~n',
	%[ReinsertedExtractedPhrases]),
	%(ExtractedPhrases==ReinsertedExtractedPhrases ->
	%    format('No change.~n',[])
	%;   format('They differ.~n',[])
	%),
	reinsert_mmoutput(RestMMOutput, StrippedMarkedMappingsNext, RestReinserted).
reinsert_mmoutput(MarkedMMOutput, StrippedMarkedMappings, MarkedMMOutput) :-
	fatal_error('reinsert_mmoutput/3 failed for~nMarkedMMOutput: ~p~nStrippedMarkedMappings: ~p~~n',
	       [MarkedMMOutput,StrippedMarkedMappings]).

reinsert_mmoutput_aux([], StrippedMarkedMappingsIn, StrippedMarkedMappingsIn, [], []) :-
	!.
reinsert_mmoutput_aux([markedphrase(UttNum,PhraseNum,Phrase1,Candidates,
				    _Mappings,PWI,GVCs,EV0,_APhrases)
		        |RestMarkedPhrases],
		      [markedmappings(UttNum,PhraseNum,ReinsertedMappings,ReinsertedAPhrases)
		        |RestStrippedMarkedMappings],
		      StrippedMarkedMappingsOut,
		      [phrase(Phrase1,Candidates,ReinsertedMappings,
		      	      PWI,GVCs,EV0,ReinsertedAPhrases)
			|RestReinsertedMMOPhrases],
		      [ReinsertedExtractedPhrases
		  	|RestReinsertedExtractedPhrases]) :-
	ReinsertedAPhrases = aphrases(ReinsertedAPs),
	extract_phrases_from_aps(ReinsertedAPs, ReinsertedExtractedPhrases),
	!,
	% temp
	%format('reinsert_mmoutput/5:~nReinsertedExtractedPhrases: ~p~n',
	%[ReinsertedExtractedPhrases]),
	reinsert_mmoutput_aux(RestMarkedPhrases,
			      RestStrippedMarkedMappings, StrippedMarkedMappingsOut,
			      RestReinsertedMMOPhrases, RestReinsertedExtractedPhrases).

reinsert_mmoutput_aux(MMOPhrases, StrippedMarkedMappings, _, MMOPhrases, _) :-
	fatal_error('reinsert_mmoutput/4 failed for~nMMOPhrases: ~p~nStrippedMarkedMappings: ~p~n~n',
	       [MMOPhrases,StrippedMarkedMappings]).

call_WSD(MMOTermList, WSDServerStream, WSDString) :-
	get_WSD_parameters(MethodList, BeginDelimiterChars, EndDelimiterChars),
	mmo_terms_to_xml_chars(MMOTermList, MethodList, XMLRequest),
	% format(user_output, 'BEFORE call_WSD_client~n', []),
	call_WSD_server(XMLRequest, WSDServerStream, Response),
	append([BeginDelimiterChars, WSDString, EndDelimiterChars, [10]], Response),
	!.
	% atom_codes(WSDAtomOut, WSDString).
	% format("wsdmod.pl:wsd/1: ~a~n",[WSDAtomOut]), ttyflush.

get_WSD_parameters(MethodList, BeginDelimiterChars, EndDelimiterChars) :-
	environ('WSD_METHODS', MethodsAtom),
	atom_codes(MethodsAtom, MethodChars),
	append(MethodChars, ".", MethodCharsWithPeriod),
	read_from_codes(MethodCharsWithPeriod, Methods),
	environ('WSD_WEIGHTS', WeightsAtom),
	atom_codes(WeightsAtom, WeightsChars),	
	append(WeightsChars, ".", WeightsCharsWithPeriod),
	read_from_codes(WeightsCharsWithPeriod, Weights),
	environ('WSD_SERVER_BEGIN_DELIMITER', BEGIN_DELIMITER),
	atom_codes(BEGIN_DELIMITER, BeginDelimiterChars),
	environ('WSD_SERVER_END_DELIMITER',   END_DELIMITER),
	atom_codes(END_DELIMITER, EndDelimiterChars),
	make_method_list(Methods, Weights, MethodList).

call_WSD_server(XMLRequest, WSDServerStream, Response) :-
	test_post_WSD_query(WSDServerStream, XMLRequest),
	% format(user_output, 'BEFORE get_WSD_result~n', []),
	test_get_WSD_result(WSDServerStream, XMLRequest, Response),
	% format(user_output, 'RESULT: ~q~n~n', [ResponseAtom]), ttyflush,
	!.

% post_WSD_query/2
% first argument is an XML request for Knowledge Source server.
test_post_WSD_query(SocketStream, Request) :-
	( format(SocketStream, "~s~n~n", [Request]),
	  flush_output(SocketStream) ->
	  true
        ; fatal_error('Unable to post WSD query~n~w~n', [Request])
        ).

% get_WSD_result/2
% first argument is an XML response for Knowledge Source server to be filled in.
test_get_WSD_result(SocketStream, Request, StreamTerm) :-
	( get_codes_WSD(SocketStream, StreamTerm) ->
	  true
        ; fatal_error('Unable to get WSD result for ~n~w~n', [Request])
        ).

get_codes_WSD(Stream, Input) :-
	get_code(Stream, Code),
	( Code is 0 ->
	  Input = []
	; Input = [Code|RestCodes],
	  get_codes_WSD(Stream, RestCodes)
	).

/* 
Pass in    [FirstToken|RestTokensIn]
the list of all tokens for all current input text.
Return in  [FirstToken|RestTokensThisUtterance]
the list of tokens corresponding to the current utterance.
Return in TokenListOut
the list of tokens corresponding to the remaining utterances.

How does this work?
The first token for each utterance is of the form
tok(sn,[],0,pos(_,_)),
so the entire token list will look like this:

tok(sn,[],0,pos(Utt1StartPos,Utt1EndPos)) %%% token signalling beginning of first utterance
tok(...)                                  %%% first  "real" token  of first utterance
tok(...)                                  %%% second "real" token  of first utterance
tok(...)                                  %%% third  "real" token  of first utterance
...                                       %%% intervening   tokens of first utterance
tok(...)                                  %%% last   "real" token  of first utterance
tok(sn,[],0,pos(Utt2StartPos,Utt2EndPos)) %%% token signalling beginning of second utterance
...                                       %%% remaining tokens for rest of input text

FirstToken is the first token of the current utterance.
To get the remaining tokens corresponding to the first utterance,
we need to recognize that
 *  the list of tokens for this utterance appended to
    the list of tokens for the rest of the utterances is equal to
    the entire list of tokens.
If we then specify that the first token of the rest of the utterances must be of the form
        tok(sn,[],0,pos(_,_))
we can then get the list of tokens for the first utterance. I.e.,

        append([FirstToken|RestTokensThisUtterance], RestTokensRestUtterances, AllTokens),
        RestTokensRestUtterances = [tok(sn,[],0,pos(_,_))|_]

*/
get_utterance_token_list(TokensIn, TokensThisUtterance, CharOffset, TokenListOut) :-
        remove_field_and_label_tokens(TokensIn, TokensOut),
        get_utterance_token_list_aux(TokensOut, TokensThisUtterance, CharOffset, TokenListOut).

get_utterance_token_list_aux([FirstToken|RestTokensIn],
			     TokensThisUtterance, CharOffset, TokenListOut) :-
        first_token_char_offset(FirstToken, CharOffset),
        get_utterance_token_list_1([FirstToken|RestTokensIn],
				   TokensThisUtterance,TokenListOut).
	% [FirstToken|RestTokensThisUtterance], TokenListOut).


get_utterance_token_list_1([FirstToken|RestTokensIn],
			   TokensThisUtterance,  TokenListOut) :-
	token_template(FirstToken, sn, [], 0, _PosInfo1, _PosInfo2),
	% FirstToken = tok(sn,[],0,_,_),
	token_template(NewFirstToken, sn, [], 0, _NewPosInfo1, _NewPosInfo2),
        append(RestTokensThisUtterance,
	       % [tok(sn,[],0,pos(StartPos,EndPos),pos(StartPos1,EndPos1))|RestTokenListOut],
	       [NewFirstToken|RestTokenListOut],
               RestTokensIn),
        !,
        append([FirstToken], RestTokensThisUtterance, TokensThisUtterance),
	% TokenListOut = [tok(sn,[],0,pos(StartPos,EndPos),pos(StartPos1,EndPos1))|RestTokenListOut].
	TokenListOut = [NewFirstToken|RestTokenListOut].
get_utterance_token_list_1([FirstToken|RestTokensIn], TokensThisUtterance, []) :-
	% FirstToken = tok(sn,[],0,_,_),
	token_template(FirstToken, sn, [], 0, _PosInfo1, _PosInfo2),
	!,
	TokensThisUtterance = [FirstToken|RestTokensIn].
get_utterance_token_list_1([_FirstToken|RestTokensIn], TokensThisUtterance, TokenListOut) :-
        get_utterance_token_list_1(RestTokensIn, TokensThisUtterance, TokenListOut).

% first_token_char_offset(tok(_,_,_,pos(StartPos,_EndPos),_), StartPos).
first_token_char_offset(FirstToken, StartPos) :-
	token_template(FirstToken, _TokenType, _TokenString, _LCString,
		       pos(StartPos,_EndPos), _PosInfo2).


remove_field_and_label_tokens([], []).
remove_field_and_label_tokens([Token|RestTokens], TokensOut) :-
	token_template(Token, TokenType, _TokenString, _LCTokenString, _PosInfo1, _PosInfo2),
	( field_or_label_type(TokenType) ->
	  TokensOut = RestTokensOut
	; TokensOut = [Token|RestTokensOut]
	),
	remove_field_and_label_tokens(RestTokens, RestTokensOut).



make_method_list([], [], []).
make_method_list([Method0|Methods], [Weight|Weights], [MethodElement|MethodList]) :-
	atom_codes(Method0, Method),
	MethodElement = method(Method, Weight),
	make_method_list(Methods, Weights, MethodList).

%%% display_WSD_methods :-
%%% 	get_WSD_methods(WSDMethods),
%%% 	sort(WSDMethods, SortedWSDMethods),
%%% 	(  foreach(ThisShortName-_ThisLongName, SortedWSDMethods),
%%% 	   foreach(ThisShortNameLength, ShortNameLengths)
%%% 	do
%%% 	   length(ThisShortName, ThisShortNameLength)
%%% 	),
%%% 	max_member(LongestShortNameLength, ShortNameLengths),
%%% 	member(ShortName-LongName, SortedWSDMethods),
%%% 	trim_whitespace_right(LongName, TrimmedLongName),
%%% 	length(ShortName, ShortNameLength),
%%% 	Padding is LongestShortNameLength - ShortNameLength,
%%% 	format('~s~*c : ~s~n', [ShortName,Padding,32,TrimmedLongName]),
%%% 	fail
%%%       ; true.

%%% not yet implemented, because call_WSD_server/5 doesn't yet exist
%%% get_WSD_methods(Methods) :-
%%% 	call_WSD_server("<?xml version=""1.0""?><rdf:RDF xmlns:rdf=""https://www.w3.org/1999/02/22-rdf-syntax-ns#""           xmlns:wsd=""http://wsd.nlm.nih.gov/wsdserver#"">      <rdf:description about=""methodslistrequest"">      </rdf:description></rdf:RDF>",'ii-server2',0,5554, ResponseString),
%%% 	xml_parse(ResponseString, ResponseTerm, [format(false)]),
%%% 	arg(2, ResponseTerm, [_,NS,_]),
%%% 	arg(3, NS, RDFElement),
%%% 	arg(3, RDFElement, List1),
%%% 	memberchk(element(description,_,List2), List1),
%%% 	memberchk(element('Bag',_,List3), List2),
%%% 	(  foreach(Member, List3),
%%% 	   fromto([], S0, S, Methods)
%%% 	do ( Member = element(li, [], _) ->
%%% 	     arg(3, Member, [NameSpaceTerm]),
%%% 	     arg(3, NameSpaceTerm, MethodElement),
%%% 	     arg(2, MethodElement, [id=MethodShortName]),
%%% 	     arg(3, MethodElement, [pcdata(MethodLongName)]),
%%% 	     S = [MethodShortName-MethodLongName|S0]
%%% 	   ; S = S0
%%% 	   )
%%% 	).	

