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

% File:     skr_xml.pl
% Module:   Generate XML output
% Author:   Francois
% Purpose:  Generate XML output analogous to MetaMap Machine Output

:- module(skr_xml,[
	conditionally_print_xml_header/2,
	conditionally_print_xml_footer/3,
	generate_and_print_xml/2,
	xml_header_footer_print_setting/3,
	xml_output_format/1
    ]).

:- use_module(skr_lib(negex), [
	final_negation_template/6
   ]).

:- use_module(metamap(metamap_utilities), [
	candidate_term/16
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
   ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	form_one_string/3,
	split_string_completely/3
   ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atom/2,
	interleave_string/3,
	ttyflush/0
   ]).

:- use_module(skr_lib(xml),[
	xml_parse/3
   ]).

:- use_module(skr(skr_utilities), [
	fatal_error/2,
	replace_blanks_with_crs/4
   ]).

:- use_module(library(lists),[
	append/2,
	delete/3
   ]).

:- use_module(library(system),[
	environ/2
   ]).

% :- use_module(library(xml),[
% 	xml_parse/3
%    ]).


% This code creates Prolog structures that are fed to xml:xml_parse,
% which is defined in the Quintus Prolog library.

generate_and_print_xml(AllMMO, OutputStream) :-
	( xml_output_format(XMLFormat) ->
	  get_xml_format_mode(XMLFormat, _OneOrZero, TrueOrFalse),
	  XMLTerm = xml([], MMOXML),
	  AllMMO = [ArgsMMO,AAsMMO,NegExMMO|UtteranceMMO],
	  generate_xml_arg_term(ArgsMMO,    XMLArgTerm),
	  generate_xml_AA_term(AAsMMO,      XMLAATerm),
	  generate_xml_negex_term(NegExMMO, XMLNegExTerm),
	  generate_xml_utterances_term(UtteranceMMO, XMLUtterancesTerm),
	  generate_xml_MMO_term(XMLArgTerm, XMLAATerm, XMLNegExTerm, XMLUtterancesTerm, MMOXML),
	  xml_parse(XMLChars, XMLTerm, [format(TrueOrFalse)]),
	  % format(user_output, 'Increasing indent~n', []), ttyflush,
	  XMLChars1 = XMLChars,
	  print_xml_output(OutputStream, XMLChars1)
	; true
	),
	!.
generate_and_print_xml(AllMMO, _OutputStream) :-
	AllMMO = [_ArgsMMO,_AAsMMO,_NegExMMO|UtteranceMMO],
	UtteranceMMO = [utterance(UttID,_UttString,_UttPosInfo,_UttReplPos)|_],
	fatal_error('Could not generate XML for MMO containing utterance labeled ~q~n.', [UttID]).
			
generate_xml_MMO_term(XMLArgTerm, XMLAATerm, XMLNegExTerm, XMLUtterancesTerm, MMOXML) :-
	  create_xml_element('MMO',
			     [],
			     [XMLArgTerm,XMLAATerm,XMLNegExTerm,XMLUtterancesTerm],
			     MMOXML).

print_xml_output(OutputStream, XMLChars) :-
	% 0 means inner header/footer
	xml_header_footer_print_setting(0, XMLSetting, PrintSetting),
	conditionally_print_xml_header(PrintSetting, OutputStream),
	flush_output(OutputStream),
	format(OutputStream, "~s", [XMLChars]),
	flush_output(OutputStream),
	conditionally_print_xml_footer(PrintSetting, XMLSetting, OutputStream),
	flush_output(OutputStream).

conditionally_print_CR(Format, OutputStream) :-
	( Format == 'XMLf1' ->
	  format(OutputStream, "~n", []),
	  flush_output(OutputStream)
	; Format == 'XMLf' ->
	  format(OutputStream, "~n", []),
	  flush_output(OutputStream)
	; true
	).

count_utterance_terms([], UtteranceCount, UtteranceCount).
count_utterance_terms([H|T], UtteranceCountIn, UtteranceCountOut) :-
	( functor(H, utterance, _Arity) ->
	  UtteranceCountNext is UtteranceCountIn + 1
	; UtteranceCountNext is UtteranceCountIn
	),
	count_utterance_terms(T, UtteranceCountNext, UtteranceCountOut).

generate_xml_utterances_term(UtteranceMMO, XMLUtterancesTerm) :-
	% Count the number of terms in UtteranceMMO that are of type 'utterance'
	count_utterance_terms(UtteranceMMO, 0, UtteranceCountNumber),
	number_codes(UtteranceCountNumber, UtteranceMMOLength),
	generate_xml_utterance_list(UtteranceMMO, UtteranceList),
	create_xml_element('Utterances',
			   ['Count'=UtteranceMMOLength],
			   UtteranceList,
			   XMLUtterancesTerm).

generate_xml_utterance_list([], []).
generate_xml_utterance_list([UtteranceMMO|RestUtteranceMMO], [XMLUtterance|RestXMLUtterances]) :-
	generate_one_xml_utterance_term(UtteranceMMO, RestUtteranceMMO, XMLUtterance,
					RemainingUtteranceMMO),
	generate_xml_utterance_list(RemainingUtteranceMMO, RestXMLUtterances).

generate_one_xml_utterance_term(UtteranceMMO, RestUtteranceMMO,
				XMLUtteranceTerm, RemainingUtteranceMMO) :-
	UtteranceMMO = utterance(Label, TempMMOUtteranceString, UttStartPos/UttLength, ReplPos),
	replace_blanks_with_crs(ReplPos, TempMMOUtteranceString, UttStartPos, MMOUtteranceString),
	generate_xml_utterance_data(Label, UttStartPos, UttLength,
				    MMOUtteranceString,
				    XMLPMID, XMLUtteranceType, XMLUtteranceNumber,
				    XMLUtteranceText,
				    XMLUtteranceStartPos, XMLUtteranceLength),
	generate_xml_phrases_term(RestUtteranceMMO, XMLPhraseListTerm, RemainingUtteranceMMO),
	create_xml_element('Utterance',
			   [],
			   [XMLPMID,XMLUtteranceType,XMLUtteranceNumber,XMLUtteranceText,
			    XMLUtteranceStartPos,XMLUtteranceLength,XMLPhraseListTerm],
			   XMLUtteranceTerm).
			   
% accumulate all phrase(_), candidates(_), and mappings(_) terms
% that occur before the next utterance(_) term,
% and while we're at it, count the number of phrase(_) terms
generate_phrases_before_next_utterance([],    PhraseCount,   [],         PhraseCount,    []).
generate_phrases_before_next_utterance([H|T], PhraseCountIn, PhrasesMMO, PhraseCountOut, MMOOut) :-
	( H = utterance(_,_,_,_) ->
	  PhraseCountOut is PhraseCountIn,
	  PhrasesMMO = [],
	  MMOOut = [H|T]	  
	; H = phrase(_,_,_,_) ->
	  PhraseCountNext is PhraseCountIn + 1,
	  PhrasesMMO = [H|RestPhrases],
	  generate_phrases_before_next_utterance(T, PhraseCountNext,
						 RestPhrases, PhraseCountOut, MMOOut)
	  % catch-all for candidates and mappings terms
	; PhraseCountNext is PhraseCountIn,
	  PhrasesMMO = [H|RestPhrases],
	  generate_phrases_before_next_utterance(T, PhraseCountNext,
						 RestPhrases, PhraseCountOut, MMOOut)
	).
			       

generate_xml_phrases_term(MMOIn, XMLPhraseTerm, MMOOut) :-
	generate_phrases_before_next_utterance(MMOIn, 0, PhrasesMMO, PhrasesLength, MMOOut),
	number_codes(PhrasesLength, PhraseCount),
	generate_xml_phrase_list(PhrasesMMO, XMLPhraseList),
	create_xml_element('Phrases',
			   ['Count'=PhraseCount],
			   XMLPhraseList,
			   XMLPhraseTerm).

generate_xml_phrase_list([], []).
generate_xml_phrase_list([MMOPhraseTerm,MMOCandidatesTerm,MMOMappingsTerm|RestMMOPhrases],
			 [XMLPhrase|RestXMLPhrases]) :-
	generate_one_xml_phrase(MMOPhraseTerm, MMOCandidatesTerm, MMOMappingsTerm, XMLPhrase), 
	generate_xml_phrase_list(RestMMOPhrases, RestXMLPhrases).
				
generate_one_xml_phrase(MMOPhraseTerm, MMOCandidatesTerm, MMOMappingsTerm, XMLPhrase) :-
	MMOPhraseTerm     = phrase(TempMMOPhraseAtom,PhraseSyntax,PhraseStartPos/PhraseLength,ReplPos),
	MMOCandidatesTerm = candidates(TotalCandidateCount,
				       ExcludedCandidateCount,PrunedCandidateCount,
				       RemainingCandidateCount,MMOCandidatesList),
	MMOMappingsTerm   = mappings(MMOMappingsList),
	atom_codes(TempMMOPhraseAtom, TempMMOPhraseString),
	replace_blanks_with_crs(ReplPos, TempMMOPhraseString, PhraseStartPos, MMOPhraseString),
	generate_xml_phrase_text(MMOPhraseString, XMLText),
	generate_xml_phrase_syntax(PhraseSyntax, XMLSyntax),
	generate_xml_phrase_start_pos(PhraseStartPos, XMLPhraseStartPos),
	generate_xml_phrase_length(PhraseLength, XMLPhraseLength),
	% generate_xml_replacement_pos(ReplPos, XMLReplPos),
	CandidatesTag = 'Candidates',
	GenerateCandidates is 0,
	generate_xml_phrase_candidates_term(GenerateCandidates, TotalCandidateCount, CandidatesTag,
					    ExcludedCandidateCount, PrunedCandidateCount,
					    RemainingCandidateCount, MMOCandidatesList,
					    XMLCandidatesTerm),
	generate_xml_phrase_mappings_term(MMOMappingsList, XMLMappingsTerm),
	create_xml_element('Phrase',
			   [],
			   [XMLText,XMLSyntax,XMLPhraseStartPos,XMLPhraseLength,
			    XMLCandidatesTerm,XMLMappingsTerm],
			   XMLPhrase).

generate_candidate_count_attribute(CandidateCount, CountType, CandidateCountAttribute) :-
	( CandidateCount =:= -1 ->
	  CandidateCountAttribute = []
	; number_codes(CandidateCount, CandidateCountCodes),
	  CandidateCountAttribute = [CountType=CandidateCountCodes]
	).

% We want the Candidates tag to contain only the Count attribute inside Mappings,
% e.g., <Candidates Count="1">,
% but the ExcludedCount and PrunedCount tags otherwise,
% e.g., <Candidates Count="2" ExcludedCount="0" PrunedCount="0">.
generate_xml_phrase_candidates_term(GenerateCandidates, TotalCandidateCount, CandidatesTag,
				    ExcludedCandidateCount, PrunedCandidateCount,
				    RemainingCandidateCount, MMOCandidatesList,
				    XMLCandidatesTerm) :-
	generate_candidate_count_attribute(TotalCandidateCount, 'Total',
					   TotalCandidateCountAttribute),
	generate_candidate_count_attribute(ExcludedCandidateCount, 'Excluded',
					   ExcludedCandidateCountAttribute),
	generate_candidate_count_attribute(PrunedCandidateCount, 'Pruned',
					   PrunedCandidateCountAttribute),
	generate_candidate_count_attribute(RemainingCandidateCount, 'Remaining',
					   RemainingCandidateCountAttribute),
	generate_xml_candidates_list(GenerateCandidates, MMOCandidatesList, XMLCandidatesList),
	append([TotalCandidateCountAttribute,ExcludedCandidateCountAttribute,
		PrunedCandidateCountAttribute,RemainingCandidateCountAttribute],
	       CountAttributeList),
	create_xml_element(CandidatesTag,
			   CountAttributeList,
			   XMLCandidatesList,
			   XMLCandidatesTerm).

generate_xml_candidates_list(GenerateCandidates, MMOCandidatesList, XMLCandidatesList) :-
	( control_option(show_candidates) ->
	  generate_xml_candidates_list_aux(MMOCandidatesList, XMLCandidatesList)
	; GenerateCandidates =:= 1 ->
	  generate_xml_candidates_list_aux(MMOCandidatesList, XMLCandidatesList)
	; XMLCandidatesList = []
	).


generate_xml_candidates_list_aux([], []).
generate_xml_candidates_list_aux([FirstMMOCandidate|RestMMOCandidates],
			     [FirstXMLCandidate|RestXMLCandidates]) :-
	generate_one_xml_candidate(FirstMMOCandidate, FirstXMLCandidate),
	generate_xml_candidates_list_aux(RestMMOCandidates, RestXMLCandidates).

generate_one_xml_candidate(MMOCandidate, XMLCandidate) :-
	candidate_term(NegValue, CUI, CandidateMatched, PreferredName, MatchedWords,
		       SemTypes, MatchMap, _LSComponents, _TargetLSComponent,
		       IsHead, IsOverMatch, Sources, PosInfo, Status, Negated, MMOCandidate),
	generate_xml_candidate_score(NegValue, XMLNegValue),
	generate_xml_CUI(CUI, XMLCUI),
	generate_xml_concept_matched(CandidateMatched, XMLCandidateMatched),
	generate_xml_preferred_name(PreferredName, XMLPreferredName),
	generate_xml_matched_words(MatchedWords, XMLMatchedWords),
	generate_xml_semtypes(SemTypes, XMLSemTypes),
	generate_xml_matchmap(MatchMap, XMLMatchMap),
	generate_xml_is_head(IsHead, XMLIsHead),	
	generate_xml_is_overmatch(IsOverMatch, XMLIsOverMatch),	
	generate_xml_sources(Sources, XMLSources),
	generate_xml_pos_info(PosInfo, 'ConceptPIs', 'ConceptPI', XMLPosInfo),
	generate_xml_status(Status, XMLStatus),
	generate_xml_negated(Negated, XMLNegated),
        create_xml_element('Candidate',
                           [],
                           [XMLNegValue,XMLCUI,XMLCandidateMatched,XMLPreferredName,
                            XMLMatchedWords,XMLSemTypes,XMLMatchMap,XMLIsHead,
                            XMLIsOverMatch,XMLSources,XMLPosInfo,XMLStatus,XMLNegated],
                            XMLCandidate).

generate_xml_candidate_score(NegValue, XMLNegValue) :-
	number_codes(NegValue, NegValueString),
	create_xml_element('CandidateScore',
			   [],
			   [pcdata(NegValueString)],
			   XMLNegValue).

generate_xml_CUI(CUI, XMLCUI) :-
	atom_codes(CUI, CUIString),
	create_xml_element('CandidateCUI',
			   [],
			   [pcdata(CUIString)],
			   XMLCUI).

generate_xml_concept_matched(CandidateMatched, XMLCandidateMatched) :-
	atom_codes(CandidateMatched, CandidateMatchedString),
	create_xml_element('CandidateMatched',
			   [],
			   [pcdata(CandidateMatchedString)],
			   XMLCandidateMatched).

generate_xml_preferred_name(PreferredName, XMLPreferredName) :-
	atom_codes(PreferredName, PreferredNameString),
	create_xml_element('CandidatePreferred',
			   [],
			   [pcdata(PreferredNameString)],
			   XMLPreferredName).

generate_xml_matched_words(MatchedWords, XMLMatchedWords) :-
	generate_xml_matched_word_list(MatchedWords, XMLMatchedWordList),
	length(MatchedWords, Length),
	number_codes(Length, Count),
	create_xml_element('MatchedWords',
			   ['Count'=Count],
			   XMLMatchedWordList,
			   XMLMatchedWords).

generate_xml_matched_word_list([], []).
generate_xml_matched_word_list([FirstMatchedWordAtom|RestMatchedWordAtoms],
			       [FirstMatchedWordXML|RestMatchedWordXMLs]) :-
	generate_one_xml_matched_word(FirstMatchedWordAtom, FirstMatchedWordXML),
	generate_xml_matched_word_list(RestMatchedWordAtoms, RestMatchedWordXMLs).

generate_one_xml_matched_word(MatchedWordAtom, MatchedWordXML) :-
	atom_codes(MatchedWordAtom, MatchedWordString),
	create_xml_element('MatchedWord',
			   [],
			   [pcdata(MatchedWordString)],
			   MatchedWordXML).

generate_xml_semtypes(SemTypes, XMLSemTypes) :-
	generate_xml_semtype_list(SemTypes, XMLSemTypeList),
	length(SemTypes, Length),
	number_codes(Length, Count),
	create_xml_element('SemTypes',
			   ['Count'=Count],
			   XMLSemTypeList,
   			   XMLSemTypes).

generate_xml_semtype_list([], []).
generate_xml_semtype_list([FirstSemTypeAtom|RestSemTypeAtoms],
			  [FirstSemTypeXML|RestSemTypeXMLs]) :-
	generate_one_xml_semtype(FirstSemTypeAtom, FirstSemTypeXML),
	generate_xml_semtype_list(RestSemTypeAtoms, RestSemTypeXMLs).

generate_one_xml_semtype(SemTypeAtom, SemTypeXML) :-
	atom_codes(SemTypeAtom, SemTypeString),
	create_xml_element('SemType',
			   [],
			   [pcdata(SemTypeString)],
			   SemTypeXML).			   

generate_xml_matchmap(MatchMap, XMLMatchMap) :-
	generate_xml_matchmap_list(MatchMap, XMLMatchMapList),
	length(MatchMap, Length),
	number_codes(Length, Count),
	create_xml_element('MatchMaps',
			   ['Count'=Count],
			   XMLMatchMapList,
   			   XMLMatchMap).


generate_xml_matchmap_list([], []).
generate_xml_matchmap_list([FirstMatchMap|RestMatchMap],
			   [FirstMatchMapXML|RestMatchMapXMLs]) :-
	generate_one_xml_matchmap(FirstMatchMap, FirstMatchMapXML),
	generate_xml_matchmap_list(RestMatchMap, RestMatchMapXMLs).

generate_one_xml_matchmap(MatchMap, MatchMapXML) :-
	MatchMap = [[TextMatchStart,TextMatchEnd],
		    [ConcMatchStart, ConcMatchEnd],
		    LexVariation],
	number_codes(TextMatchStart, TextMatchStartString),
	number_codes(TextMatchEnd, TextMatchEndString),
	number_codes(ConcMatchStart, ConcMatchStartString),
	number_codes(ConcMatchEnd, ConcMatchEndString),
	number_codes(LexVariation,   LexVariationString),

	create_xml_element('TextMatchStart',
			   [],
			   [pcdata(TextMatchStartString)],
			   XMLTextMatchStart),
	create_xml_element('TextMatchEnd',
			   [],
			   [pcdata(TextMatchEndString)],
			   XMLTextMatchEnd),
	create_xml_element('ConcMatchStart',
			   [],
			   [pcdata(ConcMatchStartString)],
			   XMLConcMatchStart),
	create_xml_element('ConcMatchEnd',
			   [],
			   [pcdata(ConcMatchEndString)],
			   XMLConcMatchEnd),
	create_xml_element('LexVariation',
			   [],
			   [pcdata(LexVariationString)],
			   XMLLexVariation),

	create_xml_element('MatchMap',
			   [],
			   [XMLTextMatchStart,XMLTextMatchEnd,
			    XMLConcMatchStart,XMLConcMatchEnd,
			    XMLLexVariation],
			   MatchMapXML).

generate_xml_is_head(IsHead, XMLIsHead) :-
	atom_codes(IsHead, IsHeadString),
	create_xml_element('IsHead',
			   [],
			   [pcdata(IsHeadString)],
			   XMLIsHead).

generate_xml_status(Status, XMLStatus) :-
	number_codes(Status, StatusString),
	create_xml_element('Status',
			   [],
			   [pcdata(StatusString)],
			   XMLStatus).

generate_xml_negated(Negated, XMLNegated) :-
	number_codes(Negated, NegatedString),
	create_xml_element('Negated',
			   [],
			   [pcdata(NegatedString)],
			   XMLNegated).

generate_xml_is_overmatch(IsOverMatch, XMLIsOverMatch) :-
	atom_codes(IsOverMatch, IsOverMatchString),
	create_xml_element('IsOverMatch',
			   [],
			   [pcdata(IsOverMatchString)],
			   XMLIsOverMatch).


generate_xml_sources(Sources, XMLSources) :-
	generate_xml_sources_list(Sources, XMLSourceList),
	length(Sources, Length),
	number_codes(Length, Count),
	create_xml_element('Sources',
			   ['Count'=Count],
			   XMLSourceList,
   			   XMLSources).

generate_xml_sources_list([], []).
generate_xml_sources_list([FirstSourceAtom|RestSourceAtoms],
			  [FirstSourceXML|RestSourceXMLs]) :-
	generate_one_xml_sources(FirstSourceAtom, FirstSourceXML),
	generate_xml_sources_list(RestSourceAtoms, RestSourceXMLs).

generate_one_xml_sources(SourceAtom, SourceXML) :-
	atom_codes(SourceAtom, SourceString),
	create_xml_element('Source',
			   [],
			   [pcdata(SourceString)],
			   SourceXML).			   

generate_xml_NegEx_concepts(NegExConceptList, ElementName, XMLNegExConcept) :-
	generate_xml_NegEx_concept_list(NegExConceptList, XMLNegExConceptList),
	length(NegExConceptList, Length),
	number_codes(Length, Count),
	create_xml_element(ElementName,
			   ['Count'=Count],
			   XMLNegExConceptList,
   			   XMLNegExConcept).

generate_xml_NegEx_concept_list([], []).
generate_xml_NegEx_concept_list([FirstNegExConcept|RestNegExConcepts],
			      [FirstNegExConceptXML|RestNegExConceptsXML]) :-
	generate_one_xml_NegEx_concept(FirstNegExConcept, FirstNegExConceptXML),
	generate_xml_NegEx_concept_list(RestNegExConcepts, RestNegExConceptsXML).

generate_one_xml_NegEx_concept(CUI:Concept, NegExConceptXML) :-
	atom_codes(CUI, CUIString),
	atom_codes(Concept,  ConceptString),
	create_xml_element('NegConcCUI',
			   [],
			   [pcdata(CUIString)],
			   CUIXML),
	create_xml_element('NegConcMatched',
			   [],
			   [pcdata(ConceptString)],
			   ConceptXML),
	create_xml_element('NegConcept',
			   [],
			   [CUIXML,ConceptXML],
			   NegExConceptXML).

generate_xml_pos_info(PosInfo, PluralElementName, SingularElementName, XMLPosInfo) :-
	generate_xml_pos_info_list(PosInfo, SingularElementName, XMLPosInfoList),
	length(PosInfo, Length),
	number_codes(Length, Count),
	create_xml_element(PluralElementName,
			   ['Count'=Count],
			   XMLPosInfoList,
   			   XMLPosInfo).

generate_xml_pos_info_list([], _SingularElementName, []).
generate_xml_pos_info_list([FirstPosInfo|RestPosInfos], SingularElementName,
			   [FirstPosInfoXML|RestPosInfoXMLs]) :-
	generate_one_xml_pos_info(FirstPosInfo, SingularElementName, FirstPosInfoXML),
	generate_xml_pos_info_list(RestPosInfos, SingularElementName, RestPosInfoXMLs).


generate_one_xml_pos_info(StartPos/Length, SingularElementName, PosInfoXML) :-
	number_codes(StartPos, StartPosString),
	number_codes(Length,   LengthString),
	create_xml_element('StartPos',
			   [],
			   [pcdata(StartPosString)],
			   StartPosXML),
	create_xml_element('Length',
			   [],
			   [pcdata(LengthString)],
			   LengthXML),
	create_xml_element(SingularElementName,
			   [],
			   [StartPosXML,LengthXML],
			   PosInfoXML).

generate_xml_phrase_mappings_term(MappingsList, XMLMappingsTerm) :-
	length(MappingsList, MappingsLength),
	number_codes(MappingsLength, Count),
	generate_xml_mappings_list(MappingsList, XMLMappingsList),
	create_xml_element('Mappings',
			   ['Count'=Count],
			   XMLMappingsList,
			   XMLMappingsTerm).


generate_xml_mappings_list([], []).
generate_xml_mappings_list([FirstMMOMapping|RestMMOMappings],
			   [FirstXMLMapping|RestXMLMappings]) :-
	generate_one_xml_mapping(FirstMMOMapping, FirstXMLMapping),
	generate_xml_mappings_list(RestMMOMappings, RestXMLMappings).
			       
generate_one_xml_mapping(MMOMapping, XMLMapping) :-
	MMOMapping = map(MappingScore, CandidatesList),
	generate_xml_mapping_score(MappingScore, XMLMappingScore),
	length(CandidatesList, TotalCandidateCount),
	ExcludedCandidateCount is -1,
	PrunedCandidateCount is -1,
	RemainingCandidateCount is -1,
	% We want the Candidates tag to contain only the Count attribute inside Mappings,
	% e.g., <Candidates Count="1">,
	% but the ExcludedCount and PrunedCount tags otherwise,
	% e.g., <Candidates Count="2" ExcludedCount="0" PrunedCount="0">

	CandidatesTag = 'MappingCandidates',
	GenerateCandidates is 1,
	generate_xml_phrase_candidates_term(GenerateCandidates, TotalCandidateCount, CandidatesTag,
					    ExcludedCandidateCount, PrunedCandidateCount,
					    RemainingCandidateCount, CandidatesList,
					    XMLCandidatesTerm),
	create_xml_element('Mapping',
			   [],
			   [XMLMappingScore,XMLCandidatesTerm],
			   XMLMapping).

generate_xml_mapping_score(MappingScore, XMLMappingScore) :-
	number_codes(MappingScore, MappingScoreString),
	create_xml_element('MappingScore',
			   [],
			   [pcdata(MappingScoreString)],
			   XMLMappingScore).

generate_xml_phrase_text(PhraseString, XMLText) :-
	create_xml_element('PhraseText',
			  [],
			  [pcdata(PhraseString)],
			  XMLText).

generate_xml_phrase_syntax(PhraseSyntax, XMLSyntax) :-
	length(PhraseSyntax, PhraseSyntaxLength),
	number_codes(PhraseSyntaxLength, Count),
	generate_xml_syntax_list(PhraseSyntax, SyntaxUnitList),
	create_xml_element('SyntaxUnits',
			    ['Count'=Count],
			    SyntaxUnitList,
			    XMLSyntax).

generate_xml_syntax_list([], []).
generate_xml_syntax_list([FirstMMOSyntaxElement|RestMMOSyntaxElements],
			 [FirstXMLSyntaxElement|RestXMLSyntaxElements]) :-
	generate_one_xml_syntax_element(FirstMMOSyntaxElement, FirstXMLSyntaxElement),
	generate_xml_syntax_list(RestMMOSyntaxElements, RestXMLSyntaxElements).

generate_one_xml_syntax_element(MMOSyntaxElement, XMLSyntaxElement) :-
	functor(MMOSyntaxElement, TypeAtom, _Arity),
	atom_codes(TypeAtom, TypeString),
	arg(1, MMOSyntaxElement, FeatureList),
	generate_xml_type_term(TypeString, XMLTypeTerm),
	generate_xml_lexmatch_term(FeatureList, XMLLexMatchTerm),
	generate_xml_inputmatch_term(FeatureList, XMLInputMatchTerm),
	generate_xml_LexCat_term(FeatureList, XMLLexCatTerm),
	generate_xml_token_term(FeatureList, XMLTokenTerm),
	% XMLLexMatchTerm and XMLLexCatTerm are optional
	% and can therefore be [], so we delete them.
	delete([XMLTypeTerm,XMLLexMatchTerm,
		XMLInputMatchTerm,XMLLexCatTerm,XMLTokenTerm], [], XMLFeatureList),
	create_xml_element('SyntaxUnit',
			   [],
			   XMLFeatureList,
			   XMLSyntaxElement).

generate_xml_type_term(TypeString, XMLType) :-
	create_xml_element('SyntaxType',
			   [],
			   [pcdata(TypeString)],
			   XMLType).

generate_xml_lexmatch_term(FeatureList, XMLLexMatch) :-
	( memberchk(lexmatch(LexMatchAtomList), FeatureList) ->
	  atom_codes_list(LexMatchAtomList, LexMatchStringList),
	  form_one_string(LexMatchStringList, " ", LexMatchString),
	  create_xml_element('LexMatch',
			     [],
			     [pcdata(LexMatchString)],
			     XMLLexMatch)
	; XMLLexMatch = []
	).

generate_xml_inputmatch_term(FeatureList, XMLInputMatch) :-
	memberchk(inputmatch(InputMatchAtomList), FeatureList) ->
	atom_codes_list(InputMatchAtomList, InputMatchStringList),
	form_one_string(InputMatchStringList, " ", InputMatchString),
	create_xml_element('InputMatch',
			   [],
			   [pcdata(InputMatchString)],
			   XMLInputMatch).

generate_xml_LexCat_term(FeatureList, XMLLexCat) :-
	( memberchk(tag(LexicalCategoryAtom), FeatureList) ->
	  atom_codes(LexicalCategoryAtom, LexicalCategoryString),
	  create_xml_element('LexCat',
			     [],
			     [pcdata(LexicalCategoryString)],
			     XMLLexCat)
	; XMLLexCat = []
	).


generate_xml_token_term(FeatureList, XMLTokenTerm) :-
	memberchk(tokens(MMOTokenList), FeatureList),
	length(MMOTokenList, TokenListLength),
	number_codes(TokenListLength, TokenListLengthString),
	generate_xml_token_list(MMOTokenList, XMLTokenList),
	create_xml_element('Tokens',
			   ['Count'=TokenListLengthString],
			   XMLTokenList,
			   XMLTokenTerm).

generate_xml_token_list([], []).
generate_xml_token_list([FirstMMOTokenAtom|RestMMOTokenAtoms], [FirstXMLToken|RestXMLTokens]) :-
	atom_codes(FirstMMOTokenAtom, TokenString),
	create_xml_element('Token',
			   [],
			   [pcdata(TokenString)],
			   FirstXMLToken),
	generate_xml_token_list(RestMMOTokenAtoms, RestXMLTokens).
			     

generate_xml_phrase_start_pos(PhraseStartPos, XMLPhraseStartPos) :-
	number_codes(PhraseStartPos, PhraseStartPosString),
	create_xml_element('PhraseStartPos',
			   [],
			   [pcdata(PhraseStartPosString)],
			   XMLPhraseStartPos).

generate_xml_phrase_length(PhraseLength, XMLPhraseLength) :-
	number_codes(PhraseLength, PhraseLengthString),
	create_xml_element('PhraseLength',
			   [],
			   [pcdata(PhraseLengthString)],
			   XMLPhraseLength).

% generate_xml_replacement_pos(ReplPos, XMLReplPos) :-
% 	generate_xml_replacement_pos_list(ReplPos, XMLReplPosList),
% 	length(ReplPos, Length),
% 	number_codes(Length, Count),
% 	create_xml_element('ReplList',
% 			   ['Count'=Count],
% 			   XMLReplPosList,
%    			   XMLReplPos).


% generate_xml_replacement_pos_list([], []).
% generate_xml_replacement_pos_list([FirstReplPos|RestReplPos],
% 				  [FirstReplPosXML|RestReplPosXMLs]) :-
% 	generate_one_xml_replacement_pos(FirstReplPos, FirstReplPosXML),
% 	generate_xml_replacement_pos_list(RestReplPos, RestReplPosXMLs).
% 
% generate_one_xml_replacement_pos(ReplPos, ReplPosXML) :-
% 	number_codes(ReplPos, ReplPosString),
% 	create_xml_element('Repl',
% 			   [],
% 			   [pcdata(ReplPosString)],
% 			   ReplPosXML).
% 

generate_xml_utterance_data(UtteranceLabel,
			    UtteranceStartPos, UtteranceLength,
			    MMOUtteranceText,
			    XMLPMID, XMLUtteranceType, XMLUtteranceNumber,
			    XMLUtteranceText,
			    XMLUtteranceStartPos, XMLUtteranceLength) :-
	atom_codes(UtteranceLabel, LabelString),
	split_string_completely(LabelString, ".", LabelStringComponents),
	append(PMIDComponents, [UtteranceTypeString,UtteranceNumberString], LabelStringComponents),
	!,
	interleave_string(PMIDComponents, ".", PMIDStringList),
	append(PMIDStringList, PMIDString),
	number_codes(UtteranceStartPos, StartPosString),
	number_codes(UtteranceLength,   LengthString),
	% RealUtteranceLength is UtteranceLength - 1,
	create_xml_element('PMID',
			   [],
			   [pcdata(PMIDString)],
			   XMLPMID),
	create_xml_element('UttSection',
			   [],
			   [pcdata(UtteranceTypeString)],
			   XMLUtteranceType),
  	create_xml_element('UttNum',
			   [],
			   [pcdata(UtteranceNumberString)],
			   XMLUtteranceNumber),
	create_xml_element('UttText',
			   [],
			   [pcdata(MMOUtteranceText)],
			   XMLUtteranceText),
	create_xml_element('UttStartPos',
			   [],
			   [pcdata(StartPosString)],
			   XMLUtteranceStartPos),
	create_xml_element('UttLength',
			   [],
			   [pcdata(LengthString)],
			   XMLUtteranceLength).
	% generate_xml_replacement_pos(ReplPos, XMLReplPos).

generate_xml_AA_term(AAsMMO, XMLAATerm) :-
	AAsMMO = aas(AAListMMO),
	length(AAListMMO, AAListLengthAtom),
	number_codes(AAListLengthAtom, AACount),
	generate_xml_AA_list(AAListMMO, AAList),
	create_xml_element('AAs',
			   ['Count'=AACount],
			   AAList,
			   XMLAATerm).


generate_xml_AA_list([], []).
generate_xml_AA_list([AcronymAtom*ExpansionAtom*CountTerm*CUIList|RestAATerms],
		     [XMLAA|RestXMLAAs]) :-
	atom_codes(AcronymAtom, AcronymString),
	atom_codes(ExpansionAtom, ExpansionString),
	generate_one_xml_AA_term(AcronymString, ExpansionString, CountTerm, CUIList, XMLAA),
	generate_xml_AA_list(RestAATerms, RestXMLAAs).


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

generate_one_xml_AA_term(Acronym, Expansion, CountTerm, MMOCUIList, XMLAA) :-
	% With MM2014, delete convert_AA_count_term and use the next line instead:
	CountTerm = (AATokenNum,AALen,AAExpTokenNum,AAExpLen,AAPosInfo),
	AAPosInfo = AAStartPos:AALen,
	% convert_AA_count_term(CountTerm, AATokenNum, AALen, AAExpTokenNum, AAExpLen),
	length(MMOCUIList, CUILengthAtom),
	number_codes(CUILengthAtom, AACUICount),
	number_codes(AATokenNum, AATokenNumString),
	number_codes(AALen, AALenString),
	number_codes(AAExpTokenNum, AAExpTokenNumString),
	number_codes(AAExpLen, AAExpLenString),
	number_codes(AAStartPos, AAStartPosString),
	generate_xml_AA_CUI_list(MMOCUIList, XMLCUIList),
	create_xml_element('AAText',
			   [],
			   [pcdata(Acronym)],
			   AATextTerm),
	create_xml_element('AAExp',
			   [],
			   [pcdata(Expansion)],
			   AAExpTerm),
	create_xml_element('AATokenNum',
			   [],
			   [pcdata(AATokenNumString)],
			   AATokenNumTerm),
	create_xml_element('AALen',
			   [],
			   [pcdata(AALenString)],
			   AALenTerm),
	create_xml_element('AAExpTokenNum',
			   [],
			   [pcdata(AAExpTokenNumString)],
			   AAExpTokenNumTerm),
	create_xml_element('AAExpLen',
			   [],
			   [pcdata(AAExpLenString)],
			   AAExpLenTerm),
	create_xml_element('AAStartPos',
			   [],
			   [pcdata(AAStartPosString)],
			   AAStartPosTerm),
	create_xml_element('AACUIs',
			   ['Count'=AACUICount],
			   XMLCUIList,
			   CUIListTerm),	
	create_xml_element('AA',
			   [],
			   [AATextTerm,AAExpTerm,
			    AATokenNumTerm,AALenTerm,
			    AAExpTokenNumTerm,AAExpLenTerm,AAStartPosTerm,CUIListTerm],
			   XMLAA).

generate_xml_AA_CUI_list([], []).
generate_xml_AA_CUI_list([MMOCUI|RestMMOCUIs], [XMLCUI|RestXMLCUIs]) :-
	atom_codes(MMOCUI, CUIString),
	create_xml_element('AACUI',
			   [],
			   [pcdata(CUIString)],
			   XMLCUI),
	generate_xml_AA_CUI_list(RestMMOCUIs, RestXMLCUIs).


generate_xml_arg_term(ArgsMMO, XMLArgTerm) :-
	ArgsMMO = args(CommandLineAtom, Options),
	atom_codes(CommandLineAtom, CommandLineString),
	length(Options, OptionsLength),
	number_codes(OptionsLength, CountString),
	generate_xml_option_list(Options, OptionList),
	create_xml_element('Command',
			   [],
			   [pcdata(CommandLineString)],
			   XMLCommandTerm),
	create_xml_element('Options',
			   ['Count'=CountString],
			   OptionList,
			   XMLOptionListTerm),	
	create_xml_element('CmdLine',
			   [],
			   [XMLCommandTerm,XMLOptionListTerm],
			   XMLArgTerm).

				   
generate_xml_option_list([], []).
generate_xml_option_list([Option-Value|RestOptions], [XMLOption|RestXMLOptions]) :-
	generate_one_xml_option(Option, Value, XMLOption),
	generate_xml_option_list(RestOptions, RestXMLOptions).

generate_one_xml_option(OptionNameAtom, OptionValue, XMLOption) :-
	atom_codes(OptionNameAtom, OptionNameString),
	generate_xml_option_value_term(OptionValue, OptionValueTerm),
	create_xml_element('OptName',
			   [],
			   [pcdata(OptionNameString)],
			   OptionNameTerm),
	create_xml_element('Option',
			   [],
			   % OptionValueTerm can be empty
			   [OptionNameTerm|OptionValueTerm],
			   XMLOption).

% OptionValueTerm is optional (!), and it's omitted if the value is []
generate_xml_option_value_term(OptionValue, OptionValueTermList) :-
	( OptionValue == [] ->
	  OptionValueTermList = []
	; convert_to_string(OptionValue, OptionValueString),
	% ; atom_codes(OptionValue, OptionValueString),
	  create_xml_element('OptValue',
			     [],
			     [pcdata(OptionValueString)],
			     OptionValueTerm),
	  OptionValueTermList = [OptionValueTerm]
	).

convert_to_string(OptionValue, OptionValueString) :-
	( atom(OptionValue) ->
	  atom_codes(OptionValue, OptionValueString)
	; number(OptionValue) ->
	  number_codes(OptionValue, OptionValueString)
	; OptionValue == [] ->
	  OptionValueString = "[]"
	; OptionValue = [_|_] ->
	  OptionValueAtomList = OptionValue,
	  atom_codes_list(OptionValueAtomList, OptionValueStringList),
	  form_one_string(OptionValueStringList, ",", OptionValueString)
	).
			   
generate_xml_negex_term(NegExTerm, XMLNegExTerm) :-
	NegExTerm = neg_list(NegExList),
	generate_xml_NegEx_list(NegExList, XMLNegExList),
	length(NegExList, Length),
	number_codes(Length, Count),
	create_xml_element('Negations',
			   ['Count'=Count],
			   XMLNegExList,
			   XMLNegExTerm).

generate_xml_NegEx_list([], []).
generate_xml_NegEx_list([NegEx|RestNegExes], [XMLNegEx|RestXMLNegExes]) :-
	generate_one_xml_NegEx_term(NegEx, XMLNegEx),
	generate_xml_NegEx_list(RestNegExes, RestXMLNegExes).

generate_one_xml_NegEx_term(NegEx, XMLNegEx) :-
	final_negation_template(NegEx,
				NegationType,
				NegationTrigger,  TriggerPosInfo,
				NegExConceptList, ConceptPosInfo),
	atom_codes(NegationType, NegationTypeString),
	create_xml_element('NegType',
			   [],
			   [pcdata(NegationTypeString)],
			   XMLNegType),

	atom_codes(NegationTrigger, NegationTriggerString),
	create_xml_element('NegTrigger',
			   [],
			   [pcdata(NegationTriggerString)],
			   XMLNegTrigger),
	generate_xml_pos_info(TriggerPosInfo, 'NegTriggerPIs', 'NegTriggerPI', XMLTriggerPosInfo),
	generate_xml_NegEx_concepts(NegExConceptList, 'NegConcepts', XMLNegExConceptList),
	% atom_codes(NegatedConcept, NegationConceptString),
	% create_xml_element('NegConcept',
	% 		   [],
	% 		   [pcdata(NegationConceptString)],
	% 		   XMLNegConcept),
	generate_xml_pos_info(ConceptPosInfo, 'NegConcPIs', 'NegConcPI', XMLConceptPosInfo),
	create_xml_element('Negation',
			   [],
			   [XMLNegType,
			    XMLNegTrigger,       XMLTriggerPosInfo,
			    XMLNegExConceptList, XMLConceptPosInfo],
			   XMLNegEx).

create_xml_element(ElementName, AttributeList, Components, XMLStructure) :-
	XMLStructure = element(ElementName, AttributeList, Components).

% xml_disclaimer('<!-- NOTE: The XML below was generated from MMO and not the original text. -->').
% 
% print_xml_disclaimer(0, _).
% print_xml_disclaimer(1, OutputStream) :-
% 	xml_disclaimer(XMLDisclaimer),
% 	format(OutputStream, '~n~w~n~n', [XMLDisclaimer]),
% 	format(OutputStream, '~n~w~n~n', [XMLDisclaimer]).
% 

conditionally_print_xml_header(PrintSetting, OutputStream) :-
	( PrintSetting =:= 1 ->
	  environ('XML_VERSION',XMLVersion),
	  environ('XML_DOCTYPE', XMLDocType),
	  environ('XML_DOCNAME', XMLDocName),
	  environ('XML_DTD', XMLDTD),
	  concat_atom(['<!', XMLDocType, ' "', XMLDocName, '" "', XMLDTD, '">'], DocType),
	  format(OutputStream, '~w~n~w', [XMLVersion,DocType]),
	  format(OutputStream, '~n<MMOs>', []),
	  flush_output(OutputStream)
	; true
	).

conditionally_print_xml_footer(PrintSetting, XMLSetting, OutputStream) :-
	( PrintSetting =:= 1 ->
	  conditionally_print_CR(XMLSetting, OutputStream),
	  format(OutputStream, '</MMOs>~n', []),
	  flush_output(OutputStream)
	; true
	).

xml_header_footer_print_setting(InnerOrOuter, XMLFormat, PrintSetting) :-
	( xml_output_format(XMLFormat),
	  get_xml_format_mode(XMLFormat, FormatMode, _TrueOrFalse) ->
	  % This is bitwise XOR
	  PrintSetting is \(InnerOrOuter, FormatMode)
	; PrintSetting is 0
	).

% Why can we use XOR here?
% Let's assume XML is on -- otherwise this is all irrelevant anyway.
% If we're at the outer header/footer (InnerOrOuter =:= 1)
% we want to print the header iff the format mode is 0 (XMLf1/XMLn1).
% If we're at the inner header/footer (InnerOrOuter =:= 0)
% we want to print the header iff the format mode is 1 (XMLf/XMLn).

% The overall structure of MetaMap XML output is the following.
% Note that there will be exactly one of inner and outer headers/footers.
% If the user has requested one XML document for the entire input file
% (XMLf1/XMLn1), only the outer header and footer will be printed.
% If the user has requested multiple XML documents, i.e., one per citation
% (format/noformat), only the inner headers and footers will be printed.
% 
% outer XML header                     (for XMLf1/XMLn1 only)
% 
%     inner XML header                 (for XMLf/XMLn only)
%     inner XML footer                 (for XMLf/XMLn only)
% 
%     inner XML header                 (for XMLf/XMLn only)
%     inner XML footer                 (for XMLf/XMLn only)
% 
%     inner XML header                 (for XMLf/XMLn only)
%     inner XML footer                 (for XMLf/XMLn only)
% 
% outer XML footer                     (for XMLf1/XMLn1 only)
% 


% get_xml_format_mode(XMLFormatMode, OneOrZero, TrueOrFalse)
% OneOrZero controls the printing of the inner header/footer
% TrueOrFalse controls whether XML output is formatted (true) or unformatted (false)
get_xml_format_mode('XMLf1', 0, true).
get_xml_format_mode('XMLn1', 0, false).
get_xml_format_mode('XMLf',  1, true).
get_xml_format_mode('XMLn',  1, false).

xml_output_format(XMLFormat) :-
	( control_option('XMLf')  ->
	  XMLFormat = 'XMLf'
	; control_option('XMLf1') ->
	  XMLFormat = 'XMLf1'
	; control_option('XMLn')  ->
	  XMLFormat = 'XMLn'
	; control_option('XMLn1') ->
	  XMLFormat = 'XMLn1'
	).

% :- use_module(library(addportray)).
% portray_mm_output(candidates(_)) :- write('CANDIDATES').
% portray_mm_output(mappings(_)) :- write('MAPPINGS').
% portray_mm_output(phrase(_,_,_,_)) :- write('PHRASE').
% :- add_portray(portray_mm_output).
  
% doit :-
% 	open(xml_call, read, Stream),
% 	read(Stream, Term),
% 	close(Stream),
% 	call(Term).
