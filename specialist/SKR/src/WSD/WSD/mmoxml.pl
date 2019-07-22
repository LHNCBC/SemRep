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

% File:     mmoxml.pl
% Module:   MMOXML
% Author:   Willie Rogers
% Purpose:  MetaMap Machine output to XML converter
% Source:   

:- module(mmoxml, [
	mmo_terms_to_xml_chars/3
    ]).

:- use_module(metamap(metamap_utilities), [
	candidate_term/16
    ]).

:- use_module(metamap(metamap_tokenization), [
	get_phrase_item_name/2,
	get_phrase_item_subitems/2,
	get_subitem_value/2                                    
    ]).

:- use_module(skr(skr_utilities), [
	get_all_candidate_features/3
   ]).

:- use_module(skr_lib(xml), [
	xml_parse/3
   ]).

:- use_module(library(codesio), [
	with_output_to_codes/2
    ]).

:- use_module(library(lists), [
	append/2,
	last/3
    ]).

:- use_module(library(sets), [
	list_to_set/2
    ]).

% :- use_module(library(xml), [
% 	xml_parse/3
%    ]).

%% generate serialized XML representation of MetaMap machine output
%% including WSD Method List
mmo_terms_to_xml_chars(MMOTermList, MethodList, XMLChars) :-
	mmo_terms_to_xml_doc(MMOTermList, MethodList, XMLDoc),
	% close(InputStream),
	xml_parse(XMLChars, XMLDoc, [format(false)]). 

mmo_terms_to_xml_doc(MMOTermList, MethodList, XMLDoc) :-
	% start by pulling off the citation term, e.g., citation('Text of entire citation')
	MMOTermList = [CitationTerm,AATerm|RestMMOTerms],
	CitationTerm = citation(CitationAtom),
	AATerm = aas(AAList0),
	% XML expects both the AA and the expansion to be strings
	make_AA_and_expansion_strings(AAList0, AAList),
	atom_codes(CitationAtom, CitationString),
        get_utterance_list(RestMMOTerms, UtteranceTerms),
	CitationOutputTerm = element(citation,[],[pcdata(CitationString)]),
	get_aa_list(AAList, AAOutputList),
	AAOutputTerm = element(aas,[],AAOutputList),
        handle_methods(MethodList, MethodsTerm),
        % append([CitationOutputTerm|UtteranceTerms],[MethodsTerm],TermList),
        % append(UtteranceTerms,[MethodsTerm],TermList),
	handle_server_options([serveroption(keepalive,"true")], ServerOptionsTerm),
        TermList = [CitationOutputTerm,AAOutputTerm,UtteranceTerms,MethodsTerm,ServerOptionsTerm],
	MachineOutputTerm = element(machine_output,[],TermList),
        XMLDoc = xml([version="1.0"],[MachineOutputTerm]).

make_AA_and_expansion_strings(AAListAtoms, AAListStrings) :-
	(  foreach(AAAtom*ExpansionAtom*AAData*CUIs, AAListAtoms),
	   foreach(AAString*ExpansionString*AAData*CUIs, AAListStrings)
	do atom_codes(AAAtom, AAString),
	   atom_codes(ExpansionAtom, ExpansionString)
	).


handle_server_options(ServerOptionList, ServerOptionsTerm) :-
       handle_server_optionlist(ServerOptionList, OptionTermList),
       ServerOptionsTerm = element(serveroptionlist,[],OptionTermList).

handle_server_optionlist([], []).
handle_server_optionlist([serveroption(Name,Value)|Rest], [Term|Terms]) :-
       Term = element(serveroption,[Name=Value],[]),
       handle_server_optionlist(Rest,Terms).

get_utterance_list([], []).
get_utterance_list([X|RestMMOTermListIn], [UtteranceTerm|UtteranceTerms]) :-
        ( X \== end_of_file,
	  X \== 'EOT' ->
	  X = utterance(LabelAtom, UtteranceText, _PosInfo, _ReplPos),
	  atom_codes(LabelAtom,  LabelString),
	  get_phrase_list(RestMMOTermListIn, 1, PhraseTerms, RestMMOTermListOut),
	  UtteranceTerm = element(utterance,
				  [ui=LabelString,pos="1",sentence=UtteranceText],
				  PhraseTerms),
	  % format(user_output, 'UtteranceTerm = ~q~n~n', [UtteranceTerm]),
	  get_utterance_list(RestMMOTermListOut, UtteranceTerms)
        ; UtteranceTerm = comment("end_of_file"),
	  UtteranceTerms=[]
	).

get_aa_list([], []).
get_aa_list([AA*Expansion*CountDataList*CUIList|RestAAs],
	    [element(aa_data,
		     [aa=AA,
		      expansion=Expansion,
		      countdata=CountDataString,
		      cui=CUIString],[])|RestAAElements]) :-
	with_output_to_codes(write_term(CUIList,[]), CUIString),
	with_output_to_codes(write_term(CountDataList,[]), CountDataString),
	get_aa_list(RestAAs, RestAAElements).


get_phrase_list(MMOTermListIn, PhraseNum, [PhraseTerm|PhraseTerms], MMOTermListOut) :-
	MMOTermListIn = [X|RestMMOTermListIn1],
        ( X \== 'EOU' ->
          X = phrase(PhraseString0, Syntax, _PosInfo,_ReplacementPos),
	  atom_codes(PhraseString0, PhraseString),
	  RestMMOTermListIn1 = [MMOCandidateTerm|RestMMOTermListIn2],
	  RestMMOTermListIn2 = [MMOMappingsTerm|RestMMOTermListIn3],
	  MMOCandidateTerm = candidates(_TotalCandidateCount,
					_ExcludedCandidateCount,_PrunedCandidateCount,
					_RemainingCandidateCount, Candidates),
	  MMOMappingsTerm = mappings(Mappings),
	  handle_syntax(Syntax, PhraseElementsTerm),
	  handle_candidates(Candidates, CandidatesTerm),
	  handle_mappings(Mappings, MappingsTerm),
	  handle_ambiguities(Mappings, AmbiguitiesTerm),
	  number_codes(PhraseNum, PhraseNumString),
	  PhraseTerm = element(phrase,
			       [pos=PhraseNumString,noun_phrase=PhraseString],
			       [PhraseElementsTerm,CandidatesTerm,MappingsTerm,AmbiguitiesTerm]),
	  % format(user_output, 'PhraseTerm = ~q~n~n', [PhraseTerm]),
	  NextPhraseNum is PhraseNum + 1,
	  get_phrase_list(RestMMOTermListIn3, NextPhraseNum, PhraseTerms, MMOTermListOut)
        ; PhraseTerm = comment("end_of_utterances"),
          PhraseTerms = [],
	  MMOTermListOut = []
        ).

handle_syntax(Syntax, PhraseElementsTerm) :-
	handle_syntax_list(Syntax, PhraseElementTerms),
	PhraseElementsTerm = element(phrase_elements,[],PhraseElementTerms).

handle_syntax_list([], []).
handle_syntax_list([First|Rest], [Term|Terms]) :-
	get_phrase_item_name(First, Category0),
	atom_codes(Category0, Category),
	get_phrase_item_subitems(First, Fields),
	handle_fields(Fields, FieldTerms),
	Term = element(phrase_element,[type=Category],FieldTerms),
	handle_syntax_list(Rest, Terms).

% The first argument of handle_fields [First|Rest] is a list like
% [lexmatch([cold]),inputmatch([cold]),tag(noun),tokens([cold])]
% First = lexmatch([cold])
handle_fields([], []).
%ignore negated that I added to cui-less items in negex-- Halil 09/04/18
handle_fields([First|Rest],Terms) :-
	get_phrase_item_name(First,negated),
	handle_fields(Rest,Terms).
handle_fields([First|Rest], [Term|Terms]) :-
	% First = lexmatch([cold])
	% Name0 = lexmatch
	get_phrase_item_name(First, Name0),
	% Name = "lexmatch"
	atom_codes(Name0, Name),
	% Value = [cold]
	get_subitem_value(First, Value),
	( atom(Value) ->
          ( Value == [] ->
            ValueTerm=""
	  ; atom_codes(Value, ValueTerm)
          )
	  % ValueTermWithBrackets = "[cold]"
	; with_output_to_codes(write_term(Value,[]), ValueTermWithBrackets),
	  % ValueTerm = "cold"
	  % Ignore the initial "["
	  ValueTermWithBrackets = [_|RestValueTermWithBrackets],
	  % Get rid of final "]"
	  last(ValueTerm, _Last, RestValueTermWithBrackets)
	),
	Term = element(field,[name=Name,value=ValueTerm],[]),
	handle_fields(Rest, Terms).

handle_candidates(Evaluations,CandidatesTerm) :-
        handle_evaluations(Evaluations, EvTerms),
        CandidatesTerm = element(candidates,[],EvTerms).

handle_evaluations([], []).
handle_evaluations([FirstCandidate|RestCandidates], [EvTerm|EvTerms]) :-
	% WSD doesn't need to know about LSComponents, TargetLSComponent, or Status
	candidate_term(Score0, Cui0, ConceptName0, PrefName0, MatchedWords0, SemTypes0,
		       MatchMap0, _LSComponents, _TargetLSComponent,
		       HeadFlag0, OverMatchFlag0, Sources0, PosInfo0, _Status, _Negated,
		       FirstCandidate),
	% First = ev(Score0,Cui0,ConceptName0,PrefName0,MatchedWords0,SemTypes0,
	% 	   MatchMap0,HeadFlag0,OverMatchFlag0,Sources0,PosInfo0,_Status0),
	number_codes(Score0, Score),
	atom_codes(Cui0, Cui),
	atom_codes(ConceptName0, ConceptName),
	atom_codes(PrefName0, PrefName),
	with_output_to_codes(write_term(MatchedWords0,[]), MatchedWordsWithBrackets),
	MatchedWordsWithBrackets = [_|RestMatchedWord],
	last(MatchedWords, _LastMatchedWordChar, RestMatchedWord),
	with_output_to_codes(write_term(SemTypes0,[]), SemTypesWithBrackets),
	SemTypesWithBrackets = [_|RestSemTypes],
	last(SemTypes, _LastSemTypesChar, RestSemTypes),
	with_output_to_codes(write_term(MatchMap0,[]), MatchMap),
	with_output_to_codes(write_term(Sources0, []), Sources),
	with_output_to_codes(write_term(PosInfo0, []), PosInfo),
	atom_codes(HeadFlag0, HeadFlag),
	atom_codes(OverMatchFlag0, OverMatchFlag),
	EvTerm = element(candidate,
			 [score=Score,
			  cui=Cui,
			  umls_concept=ConceptName,
			  preferred_name=PrefName,
			  matched_words=MatchedWords,
			  semtypes=SemTypes,
			  matchmap=MatchMap,
			  head_flag=HeadFlag,
			  overmatch_flag=OverMatchFlag,
			  sources=Sources,
			  pos_info=PosInfo],[]),
	handle_evaluations(RestCandidates, EvTerms).

handle_mappings(Mappings, MappingsTerm) :-
	handle_mapping_list(Mappings, MapTerms),
	MappingsTerm = element(mappings, [], MapTerms).
        
handle_mapping_list([], []).
handle_mapping_list([map(NegValue0,Mapping)|Rest], [Term|Terms]) :-
        number_codes(NegValue0, NegValue),
        handle_evaluations(Mapping, CandidateTerms),
        Term = element(mapping,[score=NegValue],CandidateTerms),
        handle_mapping_list(Rest, Terms).

handle_ambiguities([], AmbiguitiesTerm) :-
	AmbiguitiesTerm = element(ambiguities,[],[]).
handle_ambiguities([H|T], AmbiguitiesTerm) :-
	extract_ev_lists_from_map_terms([H|T], ListOfEvLists),
	append(ListOfEvLists, EvList),
	list_to_set(EvList, EvSet),
	group_ev_set(EvSet, GroupedListOfEvLists, []),
	handle_ambiguity_list(GroupedListOfEvLists, AmbiguityTerms),
	AmbiguitiesTerm = element(ambiguities,[],AmbiguityTerms).

% Given a list of terms of the form
% map(NegScore, EvList),
% return a list of the EvLists; e.g., transform this list

%    [map(-840,[
%         ev(-637,'C0441833','Group','Groups',[group],[inpr],[[[1,1],[1,1],0]],no,no,Srcs,PI),
% 	ev(-637,'C0444504','Mean','Mean average',[mean],[qnco],[[[2,2],[1,1],0]],no,no,Srcs,PI),
% 	ev(-853,'C0683149','serum concentration','serum drug concentration',[serum,concentration],[qnco],[[[3,3],[1,1],0],[[5,5],[2,2],0]],yes,no,Srcs,PI),
% 	ev(-637,'C0010294','Creatinine','Creatinine',[creatinine],[bacs,orch],[[[4,4],[1,1],0]],no,no,Srcs,PI)]),
%     map(-840,[
%         ev(-637,'C0441833','Group','Groups',[group],[inpr],[[[1,1],[1,1],0]],no,no,Srcs,PI),
% 	ev(-637,'C0444504','Mean','Mean average',[mean],[qnco],[[[2,2],[1,1],0]],no,no,Srcs,PI),
% 	ev(-853,'C0683149','serum concentration','serum drug concentration',[serum,concentration],[qnco],[[[3,3],[1,1],0],[[5,5],[2,2],0]],yes,no,Srcs,PI),
% 	ev(-637,'C1561535','Creatinine','Creatinine finding',[creatinine],[fndg],[[[4,4],[1,1],0]],no,no,Srcs,PI)])]
% 

% into

% [ [ev(-637,'C0441833','Group','Groups',[group],[inpr],[[[1,1],[1,1],0]],no,no,Srcs,PI),
%    ev(-637,'C0444504','Mean','Mean average',[mean],[qnco],[[[2,2],[1,1],0]],no,no,Srcs,PI),
%    ev(-853,'C0683149','serum concentration','serum drug concentration',[serum,concentration],[qnco],[[[3,3],[1,1],0],[[5,5],[2,2],0]],yes,no,Srcs,PI),
%    ev(-637,'C0010294','Creatinine','Creatinine',[creatinine],[bacs,orch],[[[4,4],[1,1],0]],no,no,Srcs,PI)],
%   [ev(-637,'C0441833','Group','Groups',[group],[inpr],[[[1,1],[1,1],0]],no,no,Srcs,PI),
%    ev(-637,'C0444504','Mean','Mean average',[mean],[qnco],[[[2,2],[1,1],0]],no,no,Srcs,PI),
%    ev(-853,'C0683149','serum concentration','serum drug concentration',[serum,concentration],[qnco],[[[3,3],[1,1],0],[[5,5],[2,2],0]],yes,no,Srcs,PI),
%    ev(-637,'C1561535','Creatinine','Creatinine finding',[creatinine],[fndg],[[[4,4],[1,1],0]],no,no,Srcs,PI)]]

extract_ev_lists_from_map_terms([], []).
extract_ev_lists_from_map_terms([map(_NegValue0,EvList)|RestMapTerms], [EvList|RestEvLists]) :-
        extract_ev_lists_from_map_terms(RestMapTerms, RestEvLists).

% Regroup one EvList, e.g., [a1, a2, b1, b2] into [[a1, a2], [b1, b2]]
group_ev_set([], GroupedEvLists, GroupedEvLists).
group_ev_set([FirstEvTerm|RestEvTerms],
			     GroupedEvListsIn, GroupedEvListsOut) :-
	matching_ev_terms(RestEvTerms, FirstEvTerm,
			  MatchingEvTerms, LeftoverEvTerms),
	( MatchingEvTerms == [] ->
	  GroupedEvListsIn = GroupedEvListsNext
	; GroupedEvListsIn = [[FirstEvTerm|MatchingEvTerms]|GroupedEvListsNext]
	),
	% GroupedEvListsIn = [[FirstEvTerm|MatchingEvTerms]|GroupedEvListsNext],
	group_ev_set(LeftoverEvTerms, GroupedEvListsNext, GroupedEvListsOut).

/*

[ev(-901,'C0312431','Hormone secretion','Hormone secretion',[hormone,secretion],[ortf],[[[2,2],[1,1],0],[[3,3],[2,2],0]],yes,no,Srcs,PI),
 ev(-827,'C0036537','Secretion','Bodily secretions',[secretion],[bdsu],[[[3,3],[1,1],0]],yes,no,Srcs,PI),
 ev(-827,'C1327616','Secretion','Cell secretion',[secretion],[celf],[[[3,3],[1,1],0]],yes,no,Srcs,PI),
 ev(-827,'C0015283',secretion,'Exocytosis',[secretion],[celf],[[[3,3],[1,1],0]],yes,no,Srcs,PI),
 ev(-734,'C0023607','Luteinising hormone','Luteinizing Hormone',[luteinising,hormone],[aapp,horm,phsu],[[[1,1],[1,1],0],[[2,2],[2,2],0]],no,no,Srcs,PI),
 ev(-827,'C0036536',secretion,'Process of secretion',[secretion],[biof],[[[3,3],[1,1],0]],yes,no,Srcs,PI)]

*/


matching_ev_terms([],    _EvTerm, [], []).
matching_ev_terms([H|T], EvTerm,  MatchingEvTerms, LeftoverEvTerms) :-
	% If H and EvTerm share the same ConceptName,
	% add H to the list of MatchingEvTerms,
	% and do not add H to the list of LeftoverEvTerms
	( ev_terms_match(EvTerm, H) ->
	  MatchingEvTerms = [H|RestMatchingEvTerms],
	  LeftoverEvTerms = RestLeftoverEvTerms
	% Otherwise, (i.e., if H and EvTerm do NOT share the same ConceptName),
	% do NOT add H to the list of MatchingEvTerms,
	% but add H to the list of LeftoverEvTerms, to be possibly matched
	% by a later EvTerm
        ; MatchingEvTerms = RestMatchingEvTerms,
	  LeftoverEvTerms = [H|RestLeftoverEvTerms]
        ),
	matching_ev_terms(T, EvTerm, RestMatchingEvTerms, RestLeftoverEvTerms).

% ev(-1000,'C0221192','Authors','Author',[authors],[idcn,prog],[[[1,1],[1,1],0]],yes,no,Srcs,PI)
%  -1000             = NegScore
%  'C0221192'        = CUI
%  'Authors'         = Concept Name
%  'Author'          = Preferred Name
%  [authors]         = Matching Words
%  [idcn,prog]       = Semantic Types 
%  [[[1,1],[1,1],0]] = MatchMap List
%  yes               = Head Flag
%  no                = OverMatch
%  Srcs              = Sources
%  PI                = PosInfo

% Two EV terms match, and need disambiguating iff:
% (1) The have the same NegScore, MatchMap List, HeadFlag, and OverMatch fields, and
% (2) They differ in at least one of Concept Name, Preferred Name, Semantic Types.

ev_terms_match(EvTerm1, EvTerm2) :-
	get_all_candidate_features([negvalue,metaterm,metaconcept,semtypes,
				    matchmap,involveshead,isovermatch],
				   EvTerm1,
				   [NegScore1,ConceptName1,PreferredName1,SemTypes1,
				    MatchMap1,HeadFlag1,OverMatch1]),
	get_all_candidate_features([negvalue,metaterm,metaconcept,semtypes,
				    matchmap,involveshead,isovermatch],
				   EvTerm2,
				   [NegScore2,ConceptName2,PreferredName2,SemTypes2,
				    MatchMap2,HeadFlag2,OverMatch2]),
	% EvTerm1 = ev(NegScore1, _Cui1, ConceptName1, PreferredName1, _WordMatch1,
	% 	       SemTypes1, MatchMap1, HeadFlag1, OverMatch1, _Sources1, _PosInfo1),
	% EvTerm2 = ev(NegScore2, _Cui2, ConceptName2, PreferredName2, _WordMatch2,
	% 	       SemTypes2, MatchMap2, HeadFlag2, OverMatch2, _Sources2, _PosInfo2),
	NegScore1 =:= NegScore2,
	MatchMap1 == MatchMap2,
	HeadFlag1 == HeadFlag2,
	OverMatch1 == OverMatch2,
	( ConceptName1 \== ConceptName2 ->
	  true
	; PreferredName1 \== PreferredName2 ->
	  true
	; SemTypes1 \== SemTypes2
	).

handle_ambiguity_list([], []).
handle_ambiguity_list([EvList|ListOfEvLists], [Term|Terms]) :-
        handle_evaluations(EvList, CandidateTerms),
        Term = element(ambiguity,[process="yes"],CandidateTerms),
        handle_ambiguity_list(ListOfEvLists, Terms).

% [method("JDI_METHOD",0.5),method("FREQ_METHOD",0.5)]
handle_methods(MethodList,MethodsTerm) :-
	handle_methodlist(MethodList,MethodTermList),
	MethodsTerm = element(methods,[],MethodTermList).

handle_methodlist([], []).
handle_methodlist([method(Name,Weight0)|Rest], [Term|Terms]) :-
	with_output_to_codes(format('~1f',Weight0), Weight),
	Term = element(method,[method_name=Name,weight=Weight],[]),
	handle_methodlist(Rest,Terms).

