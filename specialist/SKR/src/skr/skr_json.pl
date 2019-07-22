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

% File:     skr_json.pl
% Module:   Generate JSON output
% Author:   Francois
% Purpose:  Generate JSON output analogous to MetaMap Machine Output

:- module(skr_json,[
	generate_and_print_json/2,
	conditionally_print_json_bracket/3,
	conditionally_print_json_document_separator/3,
	json_output_format/1,
	json_output_params/6
    ]).

:- use_module(metamap(metamap_utilities), [
	candidate_term/16
   ]).

:- use_module(skr_lib(negex), [
	final_negation_template/6
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
   ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	form_one_string/3,
	is_print_string/1,
	split_string_completely/3
   ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atom/2,
	interleave_string/3,
	ttyflush/0
   ]).


:- use_module(skr(skr_utilities), [
	ensure_atom/2,
	fatal_error/2,
	replace_blanks_with_crs/4
   ]).

:- use_module(library(lists),[
	append/2,
	delete/3,
	is_list/1
   ]).

:- use_module(library(system),[
	environ/2
   ]).

generate_and_print_json(AllMMO, OutputStream) :-
	( json_output_params(JSONFormat, StartIndent, IndentInc, Padding, Space, NewLine),
	  JSONFormat \== '' ->	  
	  AllMMO = [ArgsMMO,AAsMMO,NegExMMO|UtteranceMMO],
	  generate_json_cmd_line_pair(ArgsMMO, JCmdLinePair),
	  generate_json_AA_pair(AAsMMO, JAAPair),
	  generate_json_NegEx_pair(NegExMMO, JNegExPair),
	  generate_json_utterances_pair(UtteranceMMO, JUtterancesPair),
	  generate_json_MMO_pair(JCmdLinePair, JAAPair, JNegExPair, JUtterancesPair, JMMOPair),
	  create_json_object([JMMOPair], JMMOObj),
	  json_print(JMMOObj, 0, StartIndent, IndentInc, Padding, Space, NewLine, OutputStream)
	; true
	),
	!.
generate_and_print_json(AllMMO, _OutputStream) :-
	AllMMO = [_ArgsMMO,_AAsMMO,_NegExMMO|UtteranceMMO],
	UtteranceMMO = [utterance(UttID,_UttString,_UttPosInfo,_UttReplPos)|_],
	fatal_error('Could not generate JSON for MMO containing utterance labeled ~q~n.', [UttID]).
			
generate_json_MMO_pair(JCmdLinePair, JAAPair, JNegExPair, JUtterancesPair, JMMOPair) :-
	  create_json_object([JCmdLinePair,JAAPair,JNegExPair,JUtterancesPair], JMMOObject),
	  create_json_pair('Document', JMMOObject, JMMOPair).

% print_json_output(OutputStream, JChars) :-
% 	% 0 means inner header/footer
% 	flush_output(OutputStream),
% 	format(OutputStream, "~s", [JChars]),
% 	flush_output(OutputStream),
% 	conditionally_print_json_footer(PrintSetting, JSetting, OutputStream),
% 	flush_output(OutputStream).
% 
% conditionally_print_CR(Format, OutputStream) :-
% 	( Format == 'Jf1' ->
% 	  format(OutputStream, "~n", []),
% 	  flush_output(OutputStream)
% 	; Format == 'Jf' ->
% 	  format(OutputStream, "~n", []),
% 	  flush_output(OutputStream)
% 	; true
% 	).

generate_json_utterances_pair(UtterancesMMO, JUtterancesPair) :-
	% Count the number of terms in UtteranceMMO that are of type 'utterance'
	generate_json_utterances_list(UtterancesMMO, JUtterancesList),
	create_json_pair('Utterances', JUtterancesList, JUtterancesPair).

generate_json_utterances_list([], []).
generate_json_utterances_list([UtteranceMMO|RestUtteranceMMO],
			      [JUtteranceObj|JRestUtteranceObjs]) :-
	generate_one_json_utterance_obj(UtteranceMMO, RestUtteranceMMO, JUtteranceObj,
					RemainingUtteranceMMO),
	generate_json_utterances_list(RemainingUtteranceMMO, JRestUtteranceObjs).

generate_one_json_utterance_obj(UtteranceMMO, RestUtteranceMMO,
				JUtteranceObj, RemainingUtteranceMMO) :-
	UtteranceMMO = utterance(Label, TempMMOUtteranceString, UttStartPos/UttLength, ReplPos),
	replace_blanks_with_crs(ReplPos, TempMMOUtteranceString, UttStartPos, MMOUtteranceString),
	generate_json_utterance_data(Label, UttStartPos, UttLength,
				     MMOUtteranceString,
				     JPMIDPair, JUtteranceTypePair, JUtteranceNumberPair,
				     JUtteranceTextPair, JUtteranceStartPosPair,
				     JUtteranceLengthPair),
	% "PMID": "00000000",
	% "UttSection": "tx",
	% "UttNum": "1",
	% "UttText": "no gall bladder cancer (GBCA)",
	% "UttStartPos": "0",
	% "UttLength": "29"
	generate_json_phrases_pair(RestUtteranceMMO, JPhrasesPair, RemainingUtteranceMMO),
	create_json_object([JPMIDPair,JUtteranceTypePair,JUtteranceNumberPair,JUtteranceTextPair,
			    JUtteranceStartPosPair,JUtteranceLengthPair,JPhrasesPair],
			   JUtteranceObj).
			   
% Accumulate all phrase(_), candidates(_), and mappings(_) terms
% that occur before the next utterance(_) term.
generate_phrases_before_next_utterance([], [], []).
generate_phrases_before_next_utterance([H|T], PhrasesMMO, MMOOut) :-
	( H = utterance(_,_,_,_) ->
	  PhrasesMMO = [],
	  MMOOut = [H|T]	  
	; H = phrase(_,_,_,_) ->
	  PhrasesMMO = [H|RestPhrases],
	  generate_phrases_before_next_utterance(T, RestPhrases, MMOOut)
	  % catch-all for candidates and mappings terms
	; PhrasesMMO = [H|RestPhrases],
	  generate_phrases_before_next_utterance(T, RestPhrases, MMOOut)
	).
			       
generate_json_phrases_pair(MMOIn, JPhrasesPair, MMOOut) :-
	generate_phrases_before_next_utterance(MMOIn, PhrasesMMO, MMOOut),
	generate_json_phrase_list(PhrasesMMO, JPhrasesList),
	create_json_pair('Phrases', JPhrasesList, JPhrasesPair).

generate_json_phrase_list([], []).
generate_json_phrase_list([MMOPhraseTerm,MMOCandidatesTerm,MMOMappingsTerm|RestMMOPhrases],
			  [JPhraseObj|JRestPhraseObjs]) :-
	generate_one_json_phrase_obj(MMOPhraseTerm, MMOCandidatesTerm,
				     MMOMappingsTerm, JPhraseObj), 
	generate_json_phrase_list(RestMMOPhrases, JRestPhraseObjs).
				
generate_one_json_phrase_obj(MMOPhraseTerm, MMOCandidatesTerm, MMOMappingsTerm, JPhraseObj) :-
	MMOPhraseTerm = phrase(TempMMOPhraseAtom,PhraseSyntax,PhraseStartPos/PhraseLength,ReplPos),
	MMOCandidatesTerm = candidates(_TotalCandidateCount,
				       _ExcludedCandidateCount,_PrunedCandidateCount,
				       _RemainingCandidateCount,MMOCandidatesList),
	MMOMappingsTerm   = mappings(MMOMappingsList),
	atom_codes(TempMMOPhraseAtom, TempMMOPhraseString),
	replace_blanks_with_crs(ReplPos, TempMMOPhraseString, PhraseStartPos, MMOPhraseString),
	create_json_pair('PhraseText', MMOPhraseString, JPhraseTextPair),
	% "PhraseText": "no gall bladder cancer"
	generate_json_phrase_syntax_pair(PhraseSyntax, JPhraseSyntaxPair),
	create_json_pair('PhraseStartPos', PhraseStartPos, JPhraseStartPosPair),
	% "PhraseStartPos": "0"
	create_json_pair('PhraseLength', PhraseLength, JPhraseLengthPair),
	% "PhraseLength": "22"
	% generate_json_replacement_pos(ReplPos, JReplPos),
	CandidatesTag = 'Candidates',
	GenerateCandidates is 0,
	generate_json_candidates_pair(GenerateCandidates, CandidatesTag,
				      MMOCandidatesList, JCandidatesPair),
	generate_json_mappings_pair(MMOMappingsList, JMappingsPair),
	create_json_object([JPhraseTextPair,JPhraseSyntaxPair,JPhraseStartPosPair,
			    JPhraseLengthPair,JCandidatesPair,JMappingsPair],
			   JPhraseObj).

% "Total": "15"
% OR
% "Excluded": "1"
% OR
% "Pruned": "0"
% OR
% "Remaining": "14"
% generate_candidate_count_pair(CandidateCount, CountType, CandidateCountAttribute) :-
% 	( CandidateCount =:= -1 ->
% 	  CandidateCountAttribute = []
% 	  % The pair/2 term needs to be in a list for the subsequent call to append/2
% 	; CandidateCountAttribute = [pair(CountType,CandidateCount)]
% 	).

% We want the Candidates tag to contain only the Count attribute inside Mappings,
% e.g., <Candidates Count="1">,
% but the ExcludedCount and PrunedCount tags otherwise,
% e.g., <Candidates Count="2" ExcludedCount="0" PrunedCount="0">.
generate_json_candidates_pair(GenerateCandidates, CandidatesTag,
			      MMOCandidatesList, JCandidatesPair) :-
	generate_json_candidates_list(GenerateCandidates, MMOCandidatesList, JCandidatesList),
	create_json_pair(CandidatesTag, JCandidatesList, JCandidatesPair).

generate_json_candidates_list(GenerateCandidates, MMOCandidatesList, JCandidatesList) :-
	( control_option(show_candidates) ->
	  generate_json_candidates_list_aux(MMOCandidatesList, JCandidatesList)
	; GenerateCandidates =:= 1 ->
	  generate_json_candidates_list_aux(MMOCandidatesList, JCandidatesList)
	; JCandidatesList = []
	).


generate_json_candidates_list_aux([], []).
generate_json_candidates_list_aux([FirstMMOCandidate|RestMMOCandidates],
				  [JFirstCandidateObj|JRestCandidatesObjs]) :-
	generate_one_json_candidate_obj(FirstMMOCandidate, JFirstCandidateObj),
	generate_json_candidates_list_aux(RestMMOCandidates, JRestCandidatesObjs).

generate_one_json_candidate_obj(MMOCandidate, JCandidateObj) :-
	candidate_term(NegValue, CUI, CandidateMatched, PreferredName, MatchedWords,
		       SemTypesList, MatchMap, _LSComponents, _TargetLSComponent,
		       IsHead, IsOverMatch, SourcesList, PosInfo, Status, Negated, MMOCandidate),
	create_json_pair('CandidateScore', NegValue, JNegValuePair),
	% "CandidateScore": "-1000",
	create_json_pair('CandidateCUI', CUI, JCUIPair),
	% "CandidateCUI": "C0153452",
	create_json_pair('CandidateMatched', CandidateMatched, JCandidateMatchedPair),
	% "CandidateMatched": "Cancer, Gallbladder",
	create_json_pair('CandidatePreferred', PreferredName, JPreferredNamePair),
	% "CandidatePreferred": "Malignant neoplasm of gallbladder",
	create_json_pair('MatchedWords', MatchedWords, JMatchedWordsPair),
	% "MatchedWords": ["gall","bladder","cancer"]
	create_json_pair('SemTypes', SemTypesList, JSemTypesPair),
	% "SemTypes": [ "neop", "bpoc"]
	generate_json_matchmaps_pair(MatchMap, JMatchMapPair),
	% "MatchMaps": [
	%    {
	%         "TextMatchStart": "1",
	%         "TextMatchEnd": "2",
	%         "ConcMatchStart": "2",
	%         "ConcMatchStart": "2",
	%         "LexVariation": "0"
	%    },
	%    {
	%         "TextMatchStart": "1",
	%         "TextMatchEnd": "2",
	%         "ConcMatchStart": "2",
	%         "ConcMatchStart": "2",
	%         "LexVariation": "0"
	%     }
	% ]
	create_json_pair('IsHead', IsHead, JIsHeadPair),
	% "IsHead": "yes"
	create_json_pair('IsOverMatch', IsOverMatch, JIsOverMatchPair),	
	% "IsOverMatch": "no"
	create_json_pair('Sources', SourcesList, JSourcesPair),
	% "Sources": ["MSH", "SNOMEDCT_US"]
	generate_json_pos_info_pair(PosInfo, 'ConceptPIs', JConceptPosInfoPair),
	% "ConceptPIs": {
	%     "Count": "1",
	%     "ConceptPI": {
	%         "StartPos": "0",
	%         "Length": "4"
	%     }
	% }
	create_json_pair('Status', Status, JStatusPair),
	% "Status": "0"
	create_json_pair('Negated', Negated, JNegatedPair),
	% "Negated": "0"
        create_json_object([JNegValuePair,JCUIPair,JCandidateMatchedPair,
			    JPreferredNamePair,JMatchedWordsPair,JSemTypesPair,
			    JMatchMapPair,JIsHeadPair, JIsOverMatchPair,
			    JSourcesPair,JConceptPosInfoPair,JStatusPair,JNegatedPair],
			   JCandidateObj).

% "MatchMaps": {
%     "Count": "2",
%     "MatchMap": {
%         "TextMatchStart": "1",
%         "TextMatchEnd": "2",
%         "ConcMatchStart": "2",
%         "ConcMatchStart": "2",
%         "LexVariation": "0"
%      },
%     "MatchMap": {
%         "TextMatchStart": "1",
%         "TextMatchEnd": "2",
%         "ConcMatchStart": "2",
%         "ConcMatchStart": "2",
%         "LexVariation": "0"
%      }
% }
generate_json_matchmaps_pair(MatchMaps, JMatchMapsPair) :-
	generate_json_matchmaps_list(MatchMaps, JMatchMapsList),
	% [ {
	%       "TextMatchStart": "1",
	%       "TextMatchEnd": "2",
	%       "ConcMatchStart": "2",
	%       "ConcMatchStart": "2",
	%       "LexVariation": "0"
	%    },
	%    {
	%       "TextMatchStart": "1",
	%       "TextMatchEnd": "2",
	%       "ConcMatchStart": "2",
	%       "ConcMatchStart": "2",
	%       "LexVariation": "0"
	%    }
	% ]
	create_json_pair('MatchMaps', JMatchMapsList, JMatchMapsPair).
	% "MatchMaps": [
	%    {
	%         "TextMatchStart": "1",
	%         "TextMatchEnd": "2",
	%         "ConcMatchStart": "2",
	%         "ConcMatchStart": "2",
	%         "LexVariation": "0"
	%    },
	%    {
	%         "TextMatchStart": "1",
	%         "TextMatchEnd": "2",
	%         "ConcMatchStart": "2",
	%         "ConcMatchStart": "2",
	%         "LexVariation": "0"
	%    }
	% ]


% [ {
%       "TextMatchStart": "1",
%       "TextMatchEnd": "2",
%       "ConcMatchStart": "2",
%       "ConcMatchStart": "2",
%       "LexVariation": "0"
%    },
%   {
%       "TextMatchStart": "1",
%       "TextMatchEnd": "2",
%       "ConcMatchStart": "2",
%       "ConcMatchStart": "2",
%       "LexVariation": "0"
%    }
% ]
generate_json_matchmaps_list([], []).
generate_json_matchmaps_list([FirstMatchMap|RestMatchMaps],
			    [JFirstMatchMapObj|JRestMatchMapObjs]) :-
	generate_one_json_matchmap_obj(FirstMatchMap, JFirstMatchMapObj),
	% {
	%     "TextMatchStart": "1",
	%     "TextMatchEnd": "2",
	%     "ConcMatchStart": "2",
	%     "ConcMatchStart": "2",
	%     "LexVariation": "0"
	%  }
	generate_json_matchmaps_list(RestMatchMaps, JRestMatchMapObjs).
	% [ {
	%       "TextMatchStart": "1",
	%       "TextMatchEnd": "2",
	%       "ConcMatchStart": "2",
	%       "ConcMatchStart": "2",
	%       "LexVariation": "0"
	%   },
	%   {
	%       "TextMatchStart": "1",
	%       "TextMatchEnd": "2",
	%       "ConcMatchStart": "2",
	%       "ConcMatchStart": "2",
	%       "LexVariation": "0"
	%   }
	% ]



% {
%     "TextMatchStart": "1",
%     "TextMatchEnd": "2",
%     "ConcMatchStart": "2",
%     "ConcMatchStart": "2",
%     "LexVariation": "0"
%  }
generate_one_json_matchmap_obj(MatchMap, JMatchMapObj) :-
	MatchMap = [[TextMatchStart,TextMatchEnd],
		    [ConcMatchStart, ConcMatchEnd],
		    LexVariation],
	create_json_pair('TextMatchStart',TextMatchStart, JTextMatchStartPair),
	% "TextMatchStart": "1",
	create_json_pair('TextMatchEnd', TextMatchEnd, JTextMatchEndPair),
	% "TextMatchEnd": "2",
	create_json_pair('ConcMatchStart', ConcMatchStart, JConcMatchStartPair),
	% "ConcMatchStart": "2",
	create_json_pair('ConcMatchEnd', ConcMatchEnd, JConcMatchEndPair),
	% "ConcMatchStart": "2",
	create_json_pair('LexVariation', LexVariation, JLexVariationPair),
	% "LexVariation": "0",
	create_json_object([JTextMatchStartPair,JTextMatchEndPair,JConcMatchStartPair,
			    JConcMatchEndPair,JLexVariationPair], JMatchMapObj).
	% { "TextMatchStart": "1",
	%   "TextMatchEnd": "2",
	%   "ConcMatchStart": "2",
	%   "ConcMatchStart": "2",
	%   "LexVariation": "0"
	% }

% "NegConcepts": [
%     {
%         "NegConcCUI": "C0235782",
%         "NegConcMatched": "Gall Bladder Cancer"
%     },
%     {
%         "NegConcCUI": "C0153452",
%         "NegConcMatched": "Cancer, GallBladder"
%     }
% ]
generate_json_NegEx_concepts_pair(NegExConceptList, JNegExConceptsPair) :-
	generate_json_NegEx_concept_list(NegExConceptList, JNegExConceptsList),
	% [ {
	%       "NegConcCUI": "C0235782",
	%       "NegConcMatched": "Gall Bladder Cancer"
	%    },
	%   {
	%       "NegConcCUI": "C0153452",
	%       "NegConcMatched": "Cancer, GallBladder"
	%    }
	% ]
	create_json_pair('NegConcepts', JNegExConceptsList, JNegExConceptsPair).
	% "NegConcepts": [
	%     {
	%         "NegConcCUI": "C0235782",
	%         "NegConcMatched": "Gall Bladder Cancer"
	%     },
	%     {
	%         "NegConcCUI": "C0153452",
	%         "NegConcMatched": "Cancer, GallBladder"
	%     }
	% ]

generate_json_NegEx_concept_list([], []).
generate_json_NegEx_concept_list([FirstNegExConcept|RestNegExConcepts],
				 [JFirstNegExConceptObj|JRestNegExConceptObjs]) :-
	generate_one_json_NegEx_concept_obj(FirstNegExConcept, JFirstNegExConceptObj),
	% {
	%     "NegConcCUI": "C0235782",
	%     "NegConcMatched": "Gall Bladder Cancer"
	%  }
	generate_json_NegEx_concept_list(RestNegExConcepts, JRestNegExConceptObjs).
	% [ {
	%       "NegConcCUI": "C0235782",
	%       "NegConcMatched": "Gall Bladder Cancer"
	%   },
	%   {
	%       "NegConcCUI": "C0153452",
	%       "NegConcMatched": "Cancer, GallBladder"
	%   }
	% ]

% "NegConcept": {
%     "NegConcCUI": "C0235782",
%     "NegConcMatched": "Gall Bladder Cancer"
% }
% generate_one_json_NegEx_concept_pair(CUI:Concept, JNegExConceptPair) :-
generate_one_json_NegEx_concept_obj(CUI:Concept, JNegExConceptObj) :-
	create_json_pair('NegConcCUI', CUI, JCUIPair),
	% "NegConcCUI": "C0235782"
	create_json_pair('NegConcMatched', Concept, JConceptPair),
	% "NegConcMatched": "Gall Bladded Cancer"
	create_json_object([JCUIPair,JConceptPair], JNegExConceptObj).
	% { "NegConcCUI": "C0235782",
	%   "NegConcMatched": "Gall Bladder Cancer"
	% }

% "NegConcPIs": {
%     "Count": "1",
%     "NegTriggerPI": {
%         "StartPos": "0",
%         "Length": "4"
%     }
% }
generate_json_pos_info_pair(PosInfoList, PluralElementName, JPosInfoPair) :-
	generate_json_pos_info_list(PosInfoList, JPosInfoList),
	% [ {
	%       "StartPos": "0",
	%       "Length": "4"
	%   },
	%   {
	%       "StartPos": "0",
	%       "Length": "4"
	%   }
	% ]
	create_json_pair(PluralElementName, JPosInfoList, JPosInfoPair).
	% "NegConcPIs": [
	%     {
	%         "StartPos": "0",
	%         "Length": "4"
	%     },
	%     {
	%         "StartPos": "0",
	%        "Length": "4"
	%     }
	%  ]
	% OR
	% "NegConcPIs": [
	%     {
	%         "StartPos": "0",
	%         "Length": "4"
	%     },
	%     {
	%         "StartPos": "0",
	%        "Length": "4"
	%     }
	%  ]

% [ {
%       "StartPos": "0",
%       "Length": "4"
%   },
%   {
%       "StartPos": "0",
%       "Length": "4"
%   }
% ]
generate_json_pos_info_list([], []).
generate_json_pos_info_list([FirstPosInfo|RestPosInfo], [JFirstPosInfo|JRestPosInfo]) :-
	generate_one_json_pos_info_obj(FirstPosInfo, JFirstPosInfo),
	% {
	%     "StartPos": "0",
	%     "Length": "4"
	% }
	generate_json_pos_info_list(RestPosInfo, JRestPosInfo).
	% [ {
	%       "StartPos": "0",
	%       "Length": "4"
	%   },
	%   {
	%       "StartPos": "0",
	%       "Length": "4"
	% ]

generate_one_json_pos_info_obj(StartPos/Length, JPosInfoObj) :-
	create_json_pair('StartPos', StartPos, JStartPosPair),
	% "StartPos": "0"
	create_json_pair('Length', Length, JLengthPair),
	% "Length": "4"
	create_json_object([JStartPosPair,JLengthPair], JPosInfoObj).

generate_json_mappings_pair(MappingsList, JMappingsPair) :-
	generate_json_mappings_list(MappingsList, JMappingsList),
	create_json_pair('Mappings', JMappingsList, JMappingsPair).

generate_json_mappings_list([], []).
generate_json_mappings_list([FirstMMOMapping|RestMMOMappings],
			    [JFirstMappingObj|JRestMappingObjs]) :-
	generate_one_json_mapping_obj(FirstMMOMapping, JFirstMappingObj),
	generate_json_mappings_list(RestMMOMappings, JRestMappingObjs).
			       
generate_one_json_mapping_obj(MMOMapping, JMappingObj) :-
	MMOMapping = map(MappingScore, CandidatesList),
	create_json_pair('MappingScore', MappingScore, JMappingScorePair),
	% "MappingScore": "-1000"
	% We want the Candidates tag to contain only the Count attribute inside Mappings,
	% e.g., <Candidates Count="1">,
	% but the ExcludedCount and PrunedCount tags otherwise,
	% e.g., <Candidates Count="2" ExcludedCount="0" PrunedCount="0">

	CandidatesTag = 'MappingCandidates',
	GenerateCandidates is 1,
	generate_json_candidates_pair(GenerateCandidates, CandidatesTag,
				      CandidatesList, JCandidatesPair),
	create_json_object([JMappingScorePair,JCandidatesPair], JMappingObj).

generate_json_phrase_syntax_pair(PhraseSyntax, JSyntaxPair) :-
	generate_json_syntax_list(PhraseSyntax, JSyntaxList),
	create_json_pair('SyntaxUnits', JSyntaxList, JSyntaxPair).

generate_json_syntax_list([], []).
generate_json_syntax_list([FirstMMOSyntaxElement|RestMMOSyntaxElements],
			  [JFirstSyntaxObj|JRestJSyntaxObjs]) :-
	generate_one_json_syntax_obj(FirstMMOSyntaxElement, JFirstSyntaxObj),
	% {   "SyntaxType": "head",
	%     "LexMatch": "gall bladder cancer",
	%     "InputMatch": "gall bladder cancer",
	%     "LexCat": "noun",
	%     "Tokens": ["gall","bladder","cancer"]
	% }
	generate_json_syntax_list(RestMMOSyntaxElements, JRestJSyntaxObjs).
	% [ {
	%      "SyntaxType": "head",
	%      "LexMatch": "gall bladder cancer",
	%      "InputMatch": "gall bladder cancer",
	%      "LexCat": "noun",
	%      "Tokens": ["gall","bladder","cancer"]
	%   },
	%  {
	%      "SyntaxType": "head",
	%      "LexMatch": "gall bladder cancer",
	%      "InputMatch": "gall bladder cancer",
	%      "LexCat": "noun",
	%      "Tokens": ["gall","bladder","cancer"]
	%  }
	% ]


generate_one_json_syntax_obj(MMOSyntaxElement, JSyntaxObj) :-
	functor(MMOSyntaxElement, SyntaxTypeAtom, _Arity),
	arg(1, MMOSyntaxElement, FeatureList),
	create_json_pair('SyntaxType', SyntaxTypeAtom, JSyntaxTypePair),
	% "SyntaxType": "head"
	generate_json_lexmatch_pair(FeatureList, JLexMatchPair),
	% "LexMatch": "gall bladder cancer"
	% OR
	% []
	generate_json_inputmatch_pair(FeatureList, JInputMatchPair),
	% "InputMatch": "gall bladder cancer"
	% OR
	% []
	generate_json_LexCat_pair(FeatureList, JLexCatPair),
	% "LexCat": "noun"
	% OR
	% []
	generate_json_token_pair(FeatureList, JTokenPair),
	% "Tokens": ["gall","bladder","cancer"]
	% JLexMatchTerm and JLexCatTerm are optional
	% and can therefore be [], so we delete them if they are in fact []!
	delete([JSyntaxTypePair,JLexMatchPair,
		JInputMatchPair,JLexCatPair,JTokenPair], [], JFeatureList),
	create_json_object(JFeatureList, JSyntaxObj).
	% { "SyntaxType": "head",
	%   "LexMatch": "gall bladder cancer",
	%   "InputMatch": "gall bladder cancer",
	%   "LexCat": "noun",
	%   "Tokens": ["gall","bladder","cancer"]
	% }

generate_json_lexmatch_pair(FeatureList, JLexMatchPair) :-
	( memberchk(lexmatch(LexMatchAtomList), FeatureList) ->
	  atom_codes_list(LexMatchAtomList, LexMatchStringList),
	  form_one_string(LexMatchStringList, " ", LexMatchString),
	  create_json_pair('LexMatch', LexMatchString, JLexMatchPair)
	; JLexMatchPair = []
	).

generate_json_inputmatch_pair(FeatureList, JInputMatchPair) :-
	memberchk(inputmatch(InputMatchAtomList), FeatureList) ->
	atom_codes_list(InputMatchAtomList, InputMatchStringList),
	form_one_string(InputMatchStringList, " ", InputMatchString),
	create_json_pair('InputMatch', InputMatchString, JInputMatchPair).

generate_json_LexCat_pair(FeatureList, JLexCatPair) :-
	( memberchk(tag(LexicalCategoryAtom), FeatureList) ->
	  create_json_pair('LexCat', LexicalCategoryAtom, JLexCatPair)
	; JLexCatPair = []
	).


% "Tokens": ["gall","bladder","cancer"]
generate_json_token_pair(FeatureList, JTokenPair) :-
	memberchk(tokens(MMOTokenList), FeatureList),
	create_json_pair('Tokens', MMOTokenList, JTokenPair).
	% "Tokens": ["gall","bladder","cancer"]

% generate_json_replacement_pos(ReplPos, JReplPos) :-
% 	generate_json_replacement_pos_list(ReplPos, JReplPosList),
% 	length(ReplPos, Length),
% 	number_codes(Length, Count),
% 	create_json_object('ReplList',
% 			   ['Count'=Count],
% 			   JReplPosList,
%    			   JReplPos).


% generate_json_replacement_pos_list([], []).
% generate_json_replacement_pos_list([FirstReplPos|RestReplPos],
% 				  [FirstReplPosJ|RestReplPosJs]) :-
% 	generate_one_json_replacement_pos(FirstReplPos, FirstReplPosJ),
% 	generate_json_replacement_pos_list(RestReplPos, RestReplPosJs).
% 
% generate_one_json_replacement_pos(ReplPos, ReplPosJ) :-
% 	number_codes(ReplPos, ReplPosString),
% 	create_json_object('Repl',
% 			   [],
% 			   [pcdata(ReplPosString)],
% 			   ReplPosJ).
% 

generate_json_utterance_data(UtteranceLabel,
			     UtteranceStartPos, UtteranceLength,
			     MMOUtteranceString,
			     JPMIDPair, JUtteranceTypePair, JUtteranceNumberPair,
			     JUtteranceTextPair, JUtteranceStartPosPair, JUtteranceLengthPair) :-
	atom_codes(UtteranceLabel, LabelString),
	split_string_completely(LabelString, ".", LabelStringComponents),
	append(PMIDComponents, [UtteranceTypeString,UtteranceNumberString], LabelStringComponents),
	!,
	interleave_string(PMIDComponents, ".", PMIDStringList),
	append(PMIDStringList, PMIDString),
	% RealUtteranceLength is UtteranceLength - 1,
	create_json_pair('PMID', PMIDString, JPMIDPair),
	% "PMID": "00000000"
	create_json_pair('UttSection', UtteranceTypeString, JUtteranceTypePair),
	% "UttSection": "tx"
  	create_json_pair('UttNum', UtteranceNumberString, JUtteranceNumberPair),
	% "UttNum": "1"
	create_json_pair('UttText', MMOUtteranceString, JUtteranceTextPair),
	% "UttText": "no gall bladder cancer (GBCA)"
	create_json_pair('UttStartPos', UtteranceStartPos, JUtteranceStartPosPair),
	% "UttStartPos": "0"
	create_json_pair('UttLength', UtteranceLength, JUtteranceLengthPair).
	% "UttLength": "29"
	% generate_json_replacement_pos(ReplPos, JReplPos).


generate_json_AA_pair(AAsMMO, JAAPair) :-
	% aas(['GBCA'*'gall bladder cancer'*(1,4,5,19,24:4)*[]])
	AAsMMO = aas(AAListMMO),
	generate_json_AA_list(AAListMMO, JAAList),
	% See below
	create_json_pair('AAs', JAAList, JAAPair).
	%  "AAs": [
	%    {
	%       "AAText": "GBCA",
	%       "AAExp": "gall bladder cancer",
	%       "AATokenNum": "1",
	%       "AALen": "4",
	%       "AAExpTokenNum": "5",
	%       "AAExpLen": "19",
	%       "AAStartPos": "24",
	%       "AACUIs": ["C0027051"]
	%     }
	%  ]


% [ "AA": {
%       "AAText": "GBCA",
%       "AAExp": "gall bladder cancer",
%       "AATokenNum": "1",
%       "AALen": "4",
%       "AAExpTokenNum": "5",
%       "AAExpLen": "19",
%       "AAStartPos": "24",
%       "AACUIs": {
%           "Count": "1",
%           "AACUI": "C0027051"
%	}
%   }
% ]
generate_json_AA_list([], []).
generate_json_AA_list([AcronymAtom*ExpansionAtom*CountTerm*CUIList|RestAATerms],
		      [JAAObj|JRestJAAObjs]) :-
	generate_one_json_AA_obj(AcronymAtom, ExpansionAtom, CountTerm, CUIList, JAAObj),
	% { "AAText": "GBCA",
	%   "AAExp": "gall bladder cancer",
	%   "AATokenNum": "1",
	%   "AALen": "4",
	%   "AAExpTokenNum": "5",
	%   "AAExpLen": "19",
	%   "AAStartPos": "24"
	%   "AACUIs": ["C0027051","C0027052"]
	% }
	generate_json_AA_list(RestAATerms, JRestJAAObjs).
	% [ "AA": {
	%       "AAText": "GBCA",
	%       "AAExp": "gall bladder cancer",
	%       "AATokenNum": "1",
	%       "AALen": "4",
	%       "AAExpTokenNum": "5",
	%       "AAExpLen": "19",
	%       "AAStartPos": "24",
	%       "AACUIs": {
	%           "Count": "1",
	%	    "AACUI": "C0027051"
	%   }
	% ]

%%% % Beginning with MM2014, the CountTerm is of the form (A,B,C,D), and not [A,B,C,D]
%%% % in order to prevent skr_utilities:write_AAs_MMO_term from possibly printing the list
%%% % as control characters: [9,10,11,12,13] will print as "^I^J^K^L^M".
%%% 
%%% % This clause will handle the AA count term of the form (A,B,C,D),
%%% % which will be created by MetaMap14.
%%% convert_AA_count_term((AATokenNum,AALen,AAExpTokenNum,AAExpLen), A, B, C, D) :-
%%% 	convert_AA_count_term([AATokenNum,AALen,AAExpTokenNum,AAExpLen], A, B, C, D).
%%% % This clause handles MetaMap13 AA count terms that are well behaved,
%%% % i.e., that do not contain "^I", "^J", "^K", "^L", or "^M".
%%% % This clause should be commented out for MM14.
%%% convert_AA_count_term([AATokenNum,AALen,AAExpTokenNum,AAExpLen], A, B, C, D) :-
%%% 	!,
%%% 	integer(AATokenNum),
%%% 	integer(AALen),
%%% 	integer(AAExpTokenNum),
%%% 	integer(AAExpLen),
%%% 	A = AATokenNum,
%%% 	B = AALen,
%%% 	C = AAExpTokenNum,
%%% 	D = AAExpLen.
%%% % This is the case that handles count terms like "^I^J^K^L";
%%% % it succeeds only if [H|T] is converted to a 4-element list.
%%% % This clause should be commented out for MM14.
%%% convert_AA_count_term([H|T], AATokenNum, AALen, AAExpTokenNum, AAExpLen) :-
%%% 	convert_AA_count_term_1([H|T], [AATokenNum,AALen,AAExpTokenNum,AAExpLen]).
%%% 
%%% convert_AA_count_term_1([], []).
%%% convert_AA_count_term_1([H|T], [H1|T1]) :-
%%% 	% The printed representation of "^I", "^J", "^K", "^L", or "^M" is in the CountTerm!
%%% 	( H == 0'^ ->
%%% 	  T = [NextCode|RestCodes],
%%% 	  whitespace_control_code(NextCode),
%%% 	  H1 is NextCode - 64,
%%% 	  RemainingCodes = RestCodes
%%% 	; H1 = H,
%%% 	  RemainingCodes = T
%%% 	),
%%% 	convert_AA_count_term_1(RemainingCodes, T1).
%%% 
%%% whitespace_control_code(73). % I
%%% whitespace_control_code(74). % J
%%% whitespace_control_code(75). % K
%%% whitespace_control_code(76). % L
%%% whitespace_control_code(77). % M
%%% whitespace_control_code(95). % _

% { "AAText": "GBCA",
%   "AAExp": "gall bladder cancer",
%   "AATokenNum": "1",
%   "AALen": "4",
%   "AAExpTokenNum": "5",
%   "AAExpLen": "19",
%   "AAStartPos": "24"
%   "AACUIs": ["C0027051","C0027052"]
% }
generate_one_json_AA_obj(Acronym, Expansion, CountTerm, MMOCUIList, JAAObj) :-
	% With MM2014, delete convert_AA_count_term and use the next line instead:
	CountTerm = (AATokenNum,AALen,AAExpTokenNum,AAExpLen,AAPosInfo),
	AAPosInfo = AAStartPos:AALen,
	% convert_AA_count_term(CountTerm, AATokenNum, AALen, AAExpTokenNum, AAExpLen),
	create_json_pair('AAText', Acronym, JAATextPair),
	% "AAText": "GBCA"
	create_json_pair('AAExp', Expansion, JAAExpPair),
	% "AAExp": "gall bladder cancer"
	create_json_pair('AATokenNum', AATokenNum, JAATokenNumPair),
	% "AATokenNum": "1"
	create_json_pair('AALen', AALen, JAALenPair),
	% "AALen": "4"
	create_json_pair('AAExpTokenNum', AAExpTokenNum, JAAExpTokenNumPair),
	% "AAExpTokenNum": "5"
	create_json_pair('AAExpLen', AAExpLen, JAAExpLenPair),
	% "AAExpLen": "19"
	create_json_pair('AAStartPos', AAStartPos, JAAStartPosPair),
	% "AAStartPos": "24"	
	create_json_pair('AACUIs', MMOCUIList, JCUIPair),
	% "AACUIs": ["C0027051","C0027052"]
	create_json_object([JAATextPair,JAAExpPair, JAATokenNumPair,JAALenPair,
			    JAAExpTokenNumPair,JAAExpLenPair,JAAStartPosPair,JCUIPair],
			   JAAObj).
	% { "AAText": "GBCA",
	%   "AAExp": "gall bladder cancer",
	%   "AATokenNum": "1",
	%   "AALen": "4",
	%   "AAExpTokenNum": "5",
	%   "AAExpLen": "19",
	%   "AAStartPos": "24"
	%   "AALen": "4",
	%   "AACUIs": ["C0027051","C0027052"]
	% }



generate_json_cmd_line_pair(ArgsMMO, JCommandLinePair) :-
	% args('a.out.Linux -L 2015 -Z 2015AB -q',
	%      [lexicon_year-'2015',
	%       mm_data_year-'2015AB',
	%       machine_output-[],
	%       infile-user_input,
	%       outfile-user_output])
	ArgsMMO = args(CommandAtom, Options),
	create_json_pair('Command', CommandAtom, JCommandPair),
	% "Command": "a.out.Linux -L 2015 -Z 2015AB --XMLf"
	generate_json_option_pair_list(Options, JOptionList),
	% [ {
	%       "OptName": "lexicon_year",
	%       "OptValue": "2015"
	%    },
	%   {
	%       "OptName": "mm_data_year",
	%       "OptValue": "
	%   },
	%   {
	%       "OptName": "machine_output"
	%   },
	%   {
	%       "OptName": "infile",
	%       "OptValue": "user_input"
	%   },
	%   {
	%       "OptName": "outfile",
	%       "OptValue": "user_output"
	%   }
	% ]
	create_json_pair('Options', JOptionList, JOptionsPair),
	% "Options": [
	%     {
	%         "OptName": "lexicon_year",
	%         "OptValue": "2015"
	%     },
	%     {
	%         "OptName": "mm_data_year",
	%         "OptValue": "2015AA"
	%     },
	%     {
	%         "OptName": "machine_output"
	%     },
	%     {
	%         "OptName": "infile",
	%         "OptValue": "user_input"
	%     },
	%     {
	%         "OptName": "outfile",
	%         "OptValue": "user_output"
	%     }
	% ]
	create_json_object([JCommandPair,JOptionsPair], JCommandLineObj),
	create_json_pair('CmdLine', JCommandLineObj, JCommandLinePair).
	% "CmdLline": {
	%     "Options": {
	%         "Count": 5,
	%         "Option": {
	%             "OptName": "lexicon_year",
	%             "OptValue": "2015"
	%         },
	%         "Option": {
	%             "OptName": "mm_data_year",
	%             "OptValue": "2015AA"
	%         },
	%         "Option": {
	%             "OptName": "machine_output"
	%         },
	%         "Option": {
	%             "OptName": "infile",
	%             "OptValue": "user_input"
	%         },
	%         "Option": {
	%             "OptName": "outfile",
	%             "OptValue": "user_output"
	%         }
	%     }
	% }

% [ {
%       "OptName": "lexicon_year",
%       "OptValue": "2015"
%    },
%   {
%       "OptName": "mm_data_year",
%       "OptValue": "2015AB"
%    }
% ]
generate_json_option_pair_list([], []).
generate_json_option_pair_list([Option-Value|RestOptions], [JOption|RestJOptions]) :-

	generate_one_json_option_pair(Option, Value, JOption),
	% {
	%     "OptName": "lexicon_year",
	%     "OptValue": "2015"
	% }
	generate_json_option_pair_list(RestOptions, RestJOptions).

% {
%     "OptName": "lexicon_year",
%     "OptValue": "2015"
% }
generate_one_json_option_pair(OptionName, OptionValue, JOptionObj) :-
	generate_json_option_value_term(OptionValue, OptionValuePair),
	% either [] or "OptValue": "user_input"
	create_json_pair('OptName', OptionName, OptionNamePair),
	% "OptName": mm_data_year
	create_json_object([OptionNamePair|OptionValuePair], JOptionObj).


% OptionValueTerm is optional (!), and it's omitted if the value is []
% either [] or "OptValue": "user_input"
generate_json_option_value_term(OptionValue, OptionValueTermList) :-
	( OptionValue == [] ->
	  OptionValueTermList = []
	  % "OptValue": "2015AB"
	; create_json_pair('OptValue', OptionValue, OptionValueTerm),
	  OptionValueTermList = [OptionValueTerm]
	).

% convert_to_string(OptionValue, OptionValueString) :-
% 	( atom(OptionValue) ->
% 	  atom_codes(OptionValue, OptionValueString)
% 	; number(OptionValue) ->
% 	  number_codes(OptionValue, OptionValueString)
% 	; OptionValue == [] ->
% 	  OptionValueString = "[]"
% 	; OptionValue = [_|_] ->
% 	  OptionValueAtomList = OptionValue,
% 	  atom_codes_list(OptionValueAtomList, OptionValueStringList),
% 	  form_one_string(OptionValueStringList, ",", OptionValueString)
% 	).
			   
generate_json_NegEx_pair(NegExTerm, JNegExPair) :-
	NegExTerm = neg_list(NegExList),
	generate_json_NegEx_list(NegExList, JNegExList),
	create_json_pair('Negations', JNegExList, JNegExPair).

generate_json_NegEx_list([], []).
generate_json_NegEx_list([NegEx|RestNegExes], [JNegExObj|RestJNegExeObjs]) :-
	generate_one_json_NegEx_object(NegEx, JNegExObj),
	% see immediately below
	generate_json_NegEx_list(RestNegExes, RestJNegExeObjs).

% {
%     "NegType": "nega",
%     "NegTrigger": "no",
%     "NegTriggerPIs": {
%         "Count": "1",
%         "NegTriggerPI": {
%             "StartPos": "0",
%             "Length": "2"
%         }
%     },
%     "NegConcepts": {
%         {
%             "NegConcCUI": "C0235782",
%             "NegConcMatched": "Gall Bladder Cancer"
%         },
%         {
%	      "NegConcCUI": "C0153452",
%             "NegConcMatched": "Cancer, GallBladder"
%         }
%     },
%     "NegConcPIs": [
%         {
%             "StartPos": "0",
%             "Length": "2"
%         }
%     ]
%  }
generate_one_json_NegEx_object(NegEx, JNegationObj) :-
	final_negation_template(NegEx,
				NegationType,
				NegationTrigger,  TriggerPosInfo,
				NegExConceptList, ConceptPosInfo),
	create_json_pair('NegType', NegationType, JNegTypePair),
	% "NegType": "nega"
	create_json_pair('NegTrigger', NegationTrigger, JNegTriggerPair),
	% "NegTrigger": "no"
	generate_json_pos_info_pair(TriggerPosInfo, 'NegTriggerPIs', JTriggerPosInfoPair),
	% "NegTriggerPIs": [
	%   {
	%     "StartPos": "0",
	%     "Length": "2"
	%   }
	%  ]

	generate_json_NegEx_concepts_pair(NegExConceptList, JNegExConceptPair),
	% "NegConcepts": [
	%     {
	%        "NegConcCUI": "C0235782",
	%        "NegConcMatched": "Gall Bladder Cancer"
	%     },
	%     {
	%        "NegConcCUI": "C0153452",
	%        "NegConcMatched": "Cancer, GallBladder"
	%     }
	% ]
	generate_json_pos_info_pair(ConceptPosInfo, 'NegConcPIs', JConceptPosInfoPair),
	% "NegConcPIs": [
	%     {
	%         "StartPos": "0",
	%         "Length": "2"
	%     }
	% ]
	create_json_object([JNegTypePair,JNegTriggerPair,JTriggerPosInfoPair,
			    JNegExConceptPair, JConceptPosInfoPair],
			   JNegationObj).
	% { "NegType": "nega",
	%   "NegTrigger": "no",
	%   "NegTriggerPIs": [
	%       {
	%           "StartPos": "0",
	%           "Length": "2"
	%       }
	%   },
	%   "NegConcepts": {
	%       {
	%          "NegConcCUI": "C0235782",
	%          "NegConcMatched": "Gall Bladder Cancer"
	%       },
	%       {
	%          "NegConcCUI": "C0153452",
	%           "NegConcMatched": "Cancer, GallBladder"
	%       }
	%   },
	%   "NegConcPIs": [
	%       {
	%           "StartPos": "0",
	%           "Length": "2"
	%       }
	%   ]
	% }

% We must
% (1) remove all backslash chars, and
% (2) escape all double quote chars.

reformat_for_json([], []).
reformat_for_json([0'\\|Rest], [0'\\, 0'\\ | ModifiedRest]) :-
	!,
	reformat_for_json(Rest, ModifiedRest).
reformat_for_json([0'"|Rest], [0'\\, 0'" | ModifiedRest]) :-     %:" To keep Emacs happy!
	!,
	reformat_for_json(Rest, ModifiedRest).
reformat_for_json([First|Rest], [First|ModifiedRest]) :-
	reformat_for_json(Rest, ModifiedRest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_json_object([H|T], JStructure) :-
	JStructure = object([H|T]).

create_json_pair(String, ValueIn, JsonPair) :-
	( atom(ValueIn) ->
	  atom_codes(ValueIn, ValueString),
	  reformat_for_json(ValueString, ValueStringEscaped),
	  atom_codes(ValueOut, ValueStringEscaped)
	; is_print_string(ValueIn) ->
	  reformat_for_json(ValueIn, ValueEscaped),
	  atom_codes(ValueOut, ValueEscaped)
	; ValueOut = ValueIn
	),
	JsonPair = pair(String,ValueOut).


% create_json_members_1([], []).
% create_json_members_1([H|T], [JH|JT]) :-
% 	create_json_pair(H, JH),
% 	create_json_members_1(T, JT).
	
% create_json_array_1([], []).
% create_json_array_1([H|T], [HValue|TValue]) :-
% 	create_json_object(H, HValue),
% 	create_json_object(T, TValue).

% create_json_value_1(object(O), object(O)) :- !.
% create_json_value_1(PrologAtom, PrologAtom) :-
% 	atomic(PrologAtom),
% 	!.
% create_json_value_1(List, List) :-
% 	is_list(List).
% create_json_value_1([H|T], [HValue|TValue]) :-
% 	create_json_value_1(H, HValue),
% 	create_json_value_1(T, TValue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% JSON printing utilities

% Originally implemented by Will Rogers;
% updated by Francois Lang to take advantage of first-argument indexing
% and allow both unformatted and formatted output,
% the latter with user-specified incremental indenting.

% StartIndent: How many spaces to initially indent
% IndentInc:   How many spaces to incrementally indent
% Padding:     How many spaces to print after the colon in a String:Value pair
% Space:       Either 32 or '', depending on whether the JSON is formatted or not
% NewLine:     Either '\n' or '', depending on whether the JSON is formatted or not
json_output_params(JSONFormat, StartIndent, IndentInc, Padding, Space, NewLine) :-
	( control_value('JSONf', IndentInc)  ->
	  JSONFormat = 'JSONf',
	  StartIndent is 1,
	  Padding is 1,
	  Space is 32,
	  NewLine = '\n'
	; control_option('JSONn')  ->
	  JSONFormat = 'JSONn',
	  StartIndent is 0,
	  Padding is 0,
	  IndentInc is 0,
	  Space = '',
	  NewLine = ''
	; JSONFormat = '',
	  StartIndent = '',
	  IndentInc = '',
	  Padding = '',
	  Space = '',
	  NewLine = ''	    
	).

json_begin('{"AllDocuments":[').
json_end(']}').

% Print the JSON opening bracket--if MetaMap is generating JSON
conditionally_print_json_bracket('{', JSONFormat, OutputStream) :-
	json_begin(JBEGIN),
	( JSONFormat == '' ->
	  true
	; JSONFormat == 'JSONn' ->
	  format(OutputStream, '~w', [JBEGIN])
	; JSONFormat == 'JSONf' ->
	  format(OutputStream, '~w~n', [JBEGIN])
	).
% Print the JSON closing bracket--if MetaMap is generating JSON
conditionally_print_json_bracket('}', JSONFormat, OutputStream) :-
	json_end(JEND),
	( JSONFormat == '' ->
	  true
	; JSONFormat == 'JSONn' ->
	  format(OutputStream, '~w~n', [JEND])
	; JSONFormat == 'JSONf' ->
	  format(OutputStream, '~n~w~n', [JEND])
	).

conditionally_print_json_document_separator(DocNum, JSONFormat, OutputStream) :-
	( JSONFormat == '' ->
	  true
	; DocNum is 1 ->
	  true
	; JSONFormat == 'JSONn' ->
	  format(OutputStream, ',', [])
	; JSONFormat == 'JSONf' ->
	  format(OutputStream, ',~n', [])
	).

get_next_indent(Indent, IndentInc, NextIndent) :-
	( Indent < 1 ->
	  NextIndent is Indent
	; NextIndent is Indent + IndentInc
	).


conditionally_indent(Env, Indent, Space, OutputStream) :-
	( Env == 0 ->
	  true
	; Indent < 1 ->
	  true
	; format(OutputStream, '~*c', [Indent,Space])
	).

jp(J) :-
	json_print(J, 0, 0, 0, 0, '', '', user_output).

json_print(object(ElementList), Env, Indent, IndentInc, Padding, Space, NewLine, OutputStream) :-
	!,
	conditionally_indent(Env, Indent, Space, OutputStream),
	format(OutputStream, '{~w',[NewLine]),
	get_next_indent(Indent, IndentInc, NextIndent),
	json_print_aux(ElementList, NextIndent, IndentInc, Padding, Space, NewLine, OutputStream),
	format(OutputStream, '~w', [NewLine]),
	conditionally_indent(1, Indent, Space, OutputStream),
	format(OutputStream, '}', []).

json_print(pair(Key, Value), _Env, Indent, IndentInc, Padding, Space, NewLine, OutputStream) :-
	!,
	( atomic(Value) ->
	  ensure_atom(Value, ValueAtom),
	  conditionally_indent(1, Indent, Space, OutputStream),
	  json_print_key_value_atom(OutputStream, IndentInc, Padding, Space, Key, ValueAtom)
	; conditionally_indent(1, Indent, Space, OutputStream),	 
	  format(OutputStream, '"~s":', [Key]),
	  conditionally_indent(1, Padding, Space, OutputStream),	 
	  json_print(Value, 0, Indent, IndentInc, Padding, Space, NewLine, OutputStream)
	).

json_print([H|T], _Env, Indent, IndentInc, Padding, Space, NewLine, OutputStream) :-
	!,
	format(OutputStream, '[', []),
	( atomic(H) ->
	  true
	; format(OutputStream, '~w',[NewLine])
	),
	get_next_indent(Indent, IndentInc, NextIndent),
	json_print_aux([H|T], NextIndent, IndentInc, Padding, Space, NewLine, OutputStream),
	format(OutputStream, ']', []).
	
json_print(Atomic, _Env, Indent, _IndentInc, _Padding, Space, _NewLine, OutputStream) :-
	atomic(Atomic),
	!,
	conditionally_indent(0, Indent, Space, OutputStream),
	format(OutputStream, '"~a"', [Atomic]).

json_print_aux([], _Indent, _IndentInc, _Padding, _Space, _NewLine, _Stream).
json_print_aux([Element|ElementList], Indent, IndentInc, Padding, Space, NewLine, OutputStream) :-
	( atomic(Element) ->
	  format(OutputStream, '"~a"', [Element])
	; json_print(Element, 1, Indent, IndentInc, Padding, Space, NewLine, OutputStream)
	),
	( ElementList == [] ->
	  format(OutputStream, '', [])
	; atomic(Element) ->
	  format(OutputStream, ',', [])
	; format(OutputStream, ',~w', [NewLine])
	),
	json_print_aux(ElementList, Indent, IndentInc, Padding, Space, NewLine, OutputStream).

json_print_key_value_atom(OutputStream, _IndentInc, Padding, Space, Key, ValueAtom) :-
	format(OutputStream, '"~s":', [Key]),
	conditionally_indent(1, Padding, Space, OutputStream),
	( ValueAtom == [] ->
	  format(OutputStream, '~w', [[]])
	;  format(OutputStream, '"~s"', [ValueAtom])
	).

% json_print_element(Element, OutputStream) :-
% 	( atom(Element) ->
% 	    format(OutputStream, '"~s"', [Element])
% 	;    format(OutputStream, '[',[]),
% 	    json_print_array_aux(Element, OutputStream),
% 	    format(OutputStream, ']',[])
% 	).

% fin

json_output_format(JSONFormat) :-
	( control_option('JSONf')  ->
	  JSONFormat = 'JSONf'
	; control_option('JSONn')  ->
	  JSONFormat = 'JSONn'
	).

