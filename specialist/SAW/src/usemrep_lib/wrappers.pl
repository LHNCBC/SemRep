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

% File:	    wrappers.pl
% Module:   wrappers
% Author:   tcr, NLS
% Purpose:  wrapper predicates for calling high-level linguistic functions

:- module( wrappers, [
        anaphora_resolution_wrapper/12,
%	assembledefns_wrapper/5,
% 	consult_tagged_text_wrapper/8,
%	convert_to_usemrep_structure_wrapper/6,
	draw_inferences_wrapper/8,
	extract_sentences_wrapper/12,
% 	format_one_gendis_term_wrapper/12,
% 	format_one_entity_wrapper/11,
% 	format_one_predication_wrapper/11,
%	format_one_predication_full_fielded_wrapper/13,
	generate_variant_info_wrapper/5,
%	get_abstract_id_wrapper/5,
	get_all_bases_wrapper/6,
	get_semtypes_wrapper/6,
	identify_genetic_concepts_wrapper/7,
%	insert_msu_token_lists_wrapper/6,
	intermediate_analysis_wrapper/11,
	metamap_phrases_wrapper/17,
%	minimal_commitment_analysis_wrapper/8,
	perform_domain_processing_wrapper/9,
	semantic_interpretation_wrapper/16,
	semantic_specification_wrapper/11
%	semantics_tree_charpos_wrapper/10,
%	syntax_tree_charpos_wrapper/14,
%	tokenize_string_wrapper/4,
%	write_full_fielded_output_wrapper/12
   ]).


:- use_module( library(system), [
		datime/1
   ]).

:- use_module( library(system), [
		environ/2
   ]).

:- use_module( skr_lib(sicstus_utils), [
		concat_atom/2,
		ttyflush/0
   ]).

:- use_module( skr_lib( nls_system ), [
		control_option/1
   ]).

:- use_module( lexicon( qp_token ), [
		tokenize_string/2
   ]).

%:- use_module( lexicon( qp_lookup ), [
%		assembledefns/2
%   ]).

:- use_module( usemrep_lib(inference), [
		draw_inferences/4
   ]).

% :- use_module( usemrep_lib(consulttt), [
% 		consult_tagged_text/5 % for Semrep, but not SemGen
%    ]).
:- use_module( skr_lib(generate_varinfo), [
		generate_variant_info/2
   ]).

%:- use_module( skr_lib(mincoman), [
%		minimal_commitment_analysis/5
%   ]).

:- use_module( usemrep_lib(get_semtypes), [
		get_semtypes/3
   ]).

:- use_module( usemrep_main(usemrep), [ errorFlag/1 ]).

:- use_module( skr(skr_text_processing), [
	extract_sentences/13
   ]).

:- use_module( skr( skr ), [
	skr_phrases/22
   ]).

%- use_module( usemrep_domain(convert), [
%convert_to_usemrep_structure/3
%  ]).

:- use_module( usemrep_domain(domain_processing), [
		perform_domain_processing/8
   ]).

:- use_module( usemrep_domain(identify), [
                identify_genetic_concepts/4
   ]).

:- use_module( usemrep_lib(interanalysis), [
		intermediate_analysis/8
   ]).

:- use_module( usemrep_lib(ssuppserv), [
		fix_metamap_analysis/2,
%		get_abstract_id/2,
		get_all_bases/3
%		insert_clinical_trials_semtypes/3,
%		insert_msu_token_lists/3
   ]).

% :- use_module( usemrep_lib(full_fielded_output), [
% 		format_one_gendis_term/9,
%                 format_one_predication_full_fielded/10,
% 		semantics_tree_charpos/7,
% 		syntax_tree_charpos/13,
% 		write_full_fielded_output/12,
% 		write_token_list/3
%    ]).

:- use_module( usemrep_lib(seminterp), [
		semantic_interpretation/12
   ]).

:- use_module( usemrep_lib(semspec), [
		semantic_specification/8
   ]).

:- use_module( usemrep_lib(anaphora), [
		anaphora_resolution/9
   ]).

:- use_module( usemrep_lib(write_results), [
                format_one_entity/9,
		format_one_predication/9
   ]).


%:- use_module( usemrep_domain(semgeninterp), [
%		semgen_interpretation/8
%   ]).

%assembledefns_wrapper( [], _InputLabel, _InputText, _OutputStream, [] ).
%assembledefns_wrapper( [FirstWord|RestWords], InputLabel, InputText, OutputStream, Definitions ) :-
%	( assembledefns( [FirstWord|RestWords], Definitions ) ->
%	  true
%        ; flag_wrapper_failure(0, OutputStream, assembledefns, 2, [InputLabel,InputText])
%        ).

% consult_tagged_text_wrapper([], [], _InputLabel, _InputText,
% 			     _OutputStream, _TaggedTextList, [], _IndexIn ).
% consult_tagged_text_wrapper([H|T], VarInfoList, InputLabel, InputText,
% 			    OutputStream, TaggedTextList, LabeledText, IndexIn ) :-
% 	Definitions = [H|T],
% 	( consult_tagged_text(Definitions, VarInfoList, TaggedTextList, LabeledText, IndexIn ) ->
% 	  true
%         ; flag_wrapper_failure(0, OutputStream, consult_tagged_text, 5, [InputLabel,InputText])
%         ).

%convert_to_usemrep_structure_wrapper(InputLabel, InputText, OutputStream,
%				     SemGenAnalysis, Domain, ConvertedSemGenAnalysis) :-
%	( convert_to_usemrep_structure(SemGenAnalysis, Domain, ConvertedSemGenAnalysis) ->
%	  true
%	; flag_wrapper_failure(0, OutputStream, convert_to_usemrep_structure, 3,
%			       [InputLabel,InputText])5
%	).

draw_inferences_wrapper(generic, DebugBitVector, InputLabel, InputText, OutputStream,
			SemInterpHead, InferenceHead, InferenceTail) :-
	!,
	( draw_inferences(SemInterpHead, DebugBitVector, InferenceHead, InferenceTail) ->
	  true
        ; flag_wrapper_failure(0, OutputStream, draw_inferences, 3, [InputLabel,InputText])
        ).
draw_inferences_wrapper(_OtherDomain, _DebugBitVector, _InputLabel, _InputText, _OutputStream,
			_SemInterpHead, InferenceHead, InferenceTail) :-
	InferenceHead = InferenceTail.

extract_sentences_wrapper(InputText,InputLabel,OutputStream,
	                  InputType,TextFields,NonTextFields,
			  Sentences,CoordSentences,UniqueID,AAs,UDAs,Lines) :-
	( extract_sentences(InputText,InputLabel,InputType,
			    TextFields,NonTextFields,
			    Sentences,CoordSentences,
			    UniqueID,AAs,_UDAListIn,_UDAListOut,UDAs,Lines) ->
	  true
        ; InputText = [InputText|_Rest],
	  flag_wrapper_failure(0, OutputStream, extract_sentences, 12,
			       [InputLabel,InputText])
	).


% format_one_gendis_term_wrapper(GenDisTerm, OutputStream, InputLabel,
% 			       ExpandedInputText,
% 			       WordCharPositionList, IndexCharPositionList,
% 			       UtteranceType, TiOrAb, SentenceID, MetaConcList,
% 			       [FmtdGenDisTerm|Rest], Rest) :-
% 	( format_one_gendis_term(GenDisTerm, TiOrAb, SentenceID,
% 				 ExpandedInputText,
% 				 WordCharPositionList, IndexCharPositionList,
% 				 UtteranceType, MetaConcList, FmtdGenDisTerm) ->
% 	  true
% 	; flag_wrapper_failure(0, OutputStream, format_one_gendis_term, 7,
% 			       [InputLabel,ExpandedInputText,
% 				'GenDisTerm'-GenDisTerm,
% 				'WordCharPositionList'-WordCharPositionList,
% 				'IndexCharPositionList'-IndexCharPositionList]),
% 	  FmtdGenDisTerm = ''-''
% 
% 	).

% format_one_entity_wrapper(FullFielded, ChosenInputText, UNExpandedInputText, OutputStream,
% 	                  FirstConcept, MetaConcList, GenPhenomList, IndexCharPositionList, 
% 			  InputLabel, UtteranceType, FormattedFirstConcept) :-
% 	( format_one_entity(FullFielded, UNExpandedInputText, FirstConcept,
% 	                    MetaConcList, GenPhenomList, IndexCharPositionList,
% 			    InputLabel, UtteranceType, FormattedFirstConcept) ->
% 	  true
% 	; flag_wrapper_failure(0, OutputStream,
% 			       format_one_entity, 9,
% 			       [InputLabel,ChosenInputText,
% 				'FirstConcept'-FirstConcept])
% 	).


% format_one_predication_wrapper(FullFielded, FirstPredication, ChosenInputText,
% 	                       UNExpandedInputText, OutputStream, MetaConcList,
% 			       GenPhenomList, IndexCharPositionList, InputLabel,
% 			       UtteranceType, FormattedFirstPredication) :-
% 	( format_one_predication(FullFielded, FirstPredication, UNExpandedInputText,
% 	                         MetaConcList, GenPhenomList, IndexCharPositionList,
% 				 InputLabel, UtteranceType, FormattedFirstPredication) ->
% 	  true
% 	; flag_wrapper_failure(0, OutputStream,
% 			       format_one_predication, 9,
% 			       [InputLabel,ChosenInputText,
% 				'FirstPredication'-FirstPredication])
% 	).


% format_one_predication_full_fielded_wrapper(FirstPredication, Domain,
% 					    ChosenInputText, OutputStream,
% 					    MetaConcList, GenPhenomList, 
% 					    IndexCharPositionList,
% 					    UNExpandedInputText,
% 					    PMID, TiOrAb, SentenceID, UtteranceType,
% 					    FormattedFirstPredication) :-
% 	( format_one_predication_full_fielded(FirstPredication, Domain, 
% 					      MetaConcList, GenPhenomList, 
% 					      IndexCharPositionList,
% 					      UNExpandedInputText,
% 					      TiOrAb, SentenceID, UtteranceType,
% 					      FormattedFirstPredication) ->
% 	  true
% 	; concat_atom([PMID, '.', TiOrAb, '.', SentenceID], InputLabel),
% 	  flag_wrapper_failure(0, OutputStream,
% 			       format_one_predication_full_fielded, 8,
% 			       [InputLabel,ChosenInputText,
% 				'FirstPredication'-FirstPredication])
% 	).
	  
generate_variant_info_wrapper([], _InputLabel, _InputText, _OutputStream, [] ).
generate_variant_info_wrapper( [H|T], InputLabel, InputText, OutputStream, VarInfoList ) :-
	Definitions = [H|T],
	( generate_variant_info( Definitions, VarInfoList ) ->
	  true
        ; flag_wrapper_failure(0, OutputStream, generate_variant_info, 2, [InputLabel,InputText])
	).

get_all_bases_wrapper(InputText, InputLabel, OutputStream, AnalysisWithSemtypes1,
		      Definitions, AnalysisWithBases) :-
	( get_all_bases(AnalysisWithSemtypes1, Definitions, AnalysisWithBases) ->
	  true
	; flag_wrapper_failure(0, OutputStream, get_all_bases, 3, [InputLabel,InputText])
	).

%get_abstract_id_wrapper( ExpandedUtterances, InputText, InputLabel, OutputStream, AbstractID ) :-
%	( get_abstract_id( ExpandedUtterances, AbstractID ) ->
%	  true
%       ; InputText = [InputText|_Rest],
%	  flag_wrapper_failure(0, OutputStream, get_abstract_id, 2, [InputLabel,InputText]),
%	  AbstractID = []
%	).

get_semtypes_wrapper(AnalysisWithMetaConcs, InputText, InputLabel,
		     OutputStream, AnalysisWithSemtypes, Domain) :-
	( get_semtypes( AnalysisWithMetaConcs, AnalysisWithSemtypes, Domain ) ->
	  true
        ; flag_wrapper_failure(1, OutputStream, get_semtypes, 3, [InputLabel,InputText])
	).

identify_genetic_concepts_wrapper(InputLabel, InputText, OutputStream, Domain,
	                          AnalysisWithSemTypes, ABGenes, AnalysisWithGenes) :-
	( identify_genetic_concepts(Domain, AnalysisWithSemTypes,ABGenes, AnalysisWithGenes) ->
	  true
	; AnalysisWithGenes = [],
	  flag_wrapper_failure(0, OutputStream,
			       identify_genetic_concepts, 4,
			       [InputLabel,InputText])
	).

% insert_msu_token_lists_wrapper(InputLabel, InputText, OutputStream,
% 			       minimal_syntax(SyntacticAnalysis),
% 			       AnalysisWithSemTypes,
% 			       SyntacticAnalysisWithTokens) :-
% 	( insert_msu_token_lists(minimal_syntax(SyntacticAnalysis),
% 				 AnalysisWithSemTypes,
% 				 SyntacticAnalysisWithTokens) ->
% 	  true
% 	; flag_wrapper_failure(0, OutputStream, insert_msu_token_lists_wrapper,
% 			       3, [InputLabel,InputText])
% 	).


intermediate_analysis_wrapper( [], _InputText, _InputLabel, _OutputStream,
			       _Definitions, _PrincipalDomain,
			       CompPreds, CompPreds, [], [], []).
intermediate_analysis_wrapper( [H|T], InputText, InputLabel, OutputStream,
			       Definitions, PrincipalDomain,
			       ComparativePredications, ComparativePredicationsTail,
			       AdjustedAnalysis, ConjunctList, SubordinatorPredicateList ) :-
	AnalysisWithSemtypes = [H|T],
	( intermediate_analysis( minimal_syntax(AnalysisWithSemtypes),
				 Definitions, PrincipalDomain,
				 ComparativePredications, ComparativePredicationsTail,
			         AdjustedAnalysis, ConjunctList , SubordinatorPredicateList) ->
	  true
        ; flag_wrapper_failure(0, OutputStream, intermediate_analysis, 8, [InputLabel,InputText])
        ).

metamap_phrases_wrapper(_ServerStreams, [],
			_InputText, _InputLabel,
			_OrigCitationTextAtomWithBlanks, _CitationTextAtomWithCRs, _TagList,
                        _OutputStream, _AAs, _UDAs, WordDataCache, USCCache, 
			RawTokens, RawTokens, WordDataCache, USCCache,
			minimal_syntax([]) ).
metamap_phrases_wrapper( ServerStreams, [H|T],
			 InputText, InputLabel,
			 OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,TagList,
			 OutputStream, AAs, UDAs, WordDataCacheIn, USCCacheIn,
			 RawTokensIn,RawTokensOut, WordDataCacheOut,USCCacheOut,
			 AnalysisWithMetaConcs ) :-
	SyntacticAnalysis0 = [H|T],
	( skr_phrases(InputLabel, InputText,
		      OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
		      AAs, UDAs, _NoMapPairs, _NoVarPairs,
		      minimal_syntax(SyntacticAnalysis0),
		      minimal_syntax(_SyntacticAnalysis),
		      TagList,
	              WordDataCacheIn,USCCacheIn,RawTokensIn,
		      ServerStreams,RawTokensOut,WordDataCacheOut,USCCacheOut,
                      _MMOPhrases,_NegationTerms,
		      _ExtractedPhrases,AnalysisWithMetaConcs0) ->
	  fix_metamap_analysis(AnalysisWithMetaConcs0, AnalysisWithMetaConcs)
        ; flag_wrapper_failure(0, OutputStream, skr_phrases, 16, [InputLabel,InputText])
        ).

%minimal_commitment_analysis_wrapper(generic, Definitions, VarInfoList,
%				    LabeledText, InputLabel, InputText,
%				    OutputStream, SyntacticAnalysis) :-
%	generic_minimal_commitment_analysis_wrapper(Definitions, VarInfoList,
%						    LabeledText, InputLabel, InputText,
%						    OutputStream, SyntacticAnalysis).


%minimal_commitment_analysis_wrapper(genetics, Definitions, VarInfoList,
%				    LabeledText, InputLabel, InputText,
%				    OutputStream, SyntacticAnalysis) :-
%	genetics_minimal_commitment_analysis_wrapper(Definitions, VarInfoList,
%						    LabeledText, InputLabel, InputText,
%						    OutputStream, SyntacticAnalysis).


%generic_minimal_commitment_analysis_wrapper( [], [], _LabeledText, _InputLabel, 
%				     _InputText, _OutputStream, minimal_syntax([])).
%generic_minimal_commitment_analysis_wrapper( [H|T], VarInfoList, LabeledText, InputLabel,
%				     InputText, OutputStream, SyntAnalysis ) :-
%	Definitions = [H|T],
%	( minimal_commitment_analysis( tag, Definitions, VarInfoList,
%				       LabeledText, SyntAnalysis ) ->
%	  true
 %       ; flag_wrapper_failure(0, OutputStream,
%			       minimal_commitment_analysis, 5,
%			       [InputLabel,InputText])
%        ).

%genetics_minimal_commitment_analysis_wrapper( [], [], _LabeledText, _InputLabel, 
%				     _InputText, _OutputStream, minimal_syntax([])).
%genetics_minimal_commitment_analysis_wrapper( [H|T], VarInfoList, LabeledText, InputLabel,
%				     InputText, OutputStream, SyntAnalysis ) :-
%	Definitions = [H|T],
%	( bind_minimal_commitment_analysis( tag, Definitions, VarInfoList,
%					    LabeledText, SyntAnalysis ) ->
%	  true
%       ; flag_wrapper_failure(0, OutputStream,
%			       bind_minimal_commitment_analysis, 5,
%			       [InputLabel,InputText])
%        ).

perform_domain_processing_wrapper(InputLabel, InputText, CitationTextAtom, OutputStream,
	                          PrincipalDomain, AnatomyDomain, SyntacticAnalysis,
				  AnalysisWithSemTypes, AnalysisWithSemTypes1) :-
	( perform_domain_processing( InputLabel, InputText, CitationTextAtom, 
	                             PrincipalDomain, AnatomyDomain, SyntacticAnalysis,
	                             AnalysisWithSemTypes, AnalysisWithSemTypes1 ) ->
	  true
        ; flag_wrapper_failure(0, OutputStream, perform_domain_processing, 8, [InputLabel,InputText])
	).

semantic_interpretation_wrapper(DebugBitVector,_Domain, Definitions,
				InputText, InputLabel,
				OutputStream, VarInfoList, IndexedAnalysis,
				PrecedingVarInfoList, PrecedingAnalysis,PrecedingConjList,
				ConjunctList, SubordinatorPredicateList,PredicationsTail,
				SemInterpHead, SemInterpTail ) :-
%	( Domain == genetics ->
%	  semgen_interpretation(IndexedAnalysis, Definitions, VarInfoList, 
%	                        ConjunctList, SubordinatorPredicateList, 
%			        SemInterpHead, SemInterpTail, Domain )
%	;
	semantic_interpretation_wrapper_1(Definitions, DebugBitVector,
					  InputText, InputLabel, OutputStream,
					  VarInfoList, IndexedAnalysis,
					  PrecedingVarInfoList,PrecedingAnalysis, PrecedingConjList,
					  ConjunctList, SubordinatorPredicateList,PredicationsTail,
					  SemInterpHead, SemInterpTail ).
%	).


semantic_interpretation_wrapper_1([], _DebugBitVector, _InputText,
				  _InputLabel, _OutputStream, _VarInfoList,
 				   minimal_syntax([]),_,_,_,[],_, _,_, _).
semantic_interpretation_wrapper_1([H|T], DebugBitVector,
				  InputText, InputLabel, OutputStream,
				  VarInfoList, IndexedAnalysis,
				  PrecedingVarInfoList,PrecedingAnalysis,PrecedingConjList,
				  ConjunctList, SubordinatorPredicateList,PredicationsTail,
				  SemInterpHead, SemInterpTail) :-
	Definitions = [H|T],
	( semantic_interpretation(DebugBitVector, IndexedAnalysis, Definitions, VarInfoList,
				  PrecedingVarInfoList, PrecedingAnalysis,PrecedingConjList,
				  ConjunctList, SubordinatorPredicateList,PredicationsTail,
				  SemInterpHead, SemInterpTail) ->
	  true
        ; flag_wrapper_failure(0, OutputStream, semantic_interpretation, 12, [InputLabel,InputText])
        ).


semantic_specification_wrapper(InputText, InputLabel,OutputStream,
			       PrincipalDomain, AnalysisWithSemtypes,
			       Definitions, VarInfoList, ConjunctList,
			       SemSpecAnalysis, SemSpecAnalysisTail, PrincipalDomain) :-
	( semantic_specification(PrincipalDomain, AnalysisWithSemtypes,
				 Definitions, VarInfoList, ConjunctList,
				 SemSpecAnalysis, SemSpecAnalysisTail, PrincipalDomain) ->
	  true
	; flag_wrapper_failure(0, OutputStream, semantic_specification, 8, [InputLabel,InputText])
	).

anaphora_resolution_wrapper(InputText, InputLabel,OutputStream,
			    PrecedingAnalysis, PrecedingVarInfoList, PrecedingConjList,
			    Analysis, VarInfoList, ConjunctList, SemSpecAnalysis,
			    CoreferenceIn, CoreferenceOut) :-
	( anaphora_resolution(Analysis, PrecedingAnalysis, PrecedingVarInfoList,
			      PrecedingConjList, VarInfoList, ConjunctList,
			      SemSpecAnalysis, CoreferenceIn, CoreferenceOut) ->
	  true
	; flag_wrapper_failure(0, OutputStream, anaphora_resolution, 9, [InputLabel,InputText])
	).


% semantics_tree_charpos_wrapper(SemanticAnalysis,
% 			       TokenListThisUtterance,
% 			       ChosenInputText, UNExpandedInputText,
% 			       OutputStream, InputLabel,
% 			       WordCharPositionList,
% 			       _WordCharPositionListOut,
% 			       IndexCharPositionList,
% 			       IndexCharPositionList) :-
% 	( /*
% 	  writeq('CALL':
% 		 semantics_tree_charpos(SemanticAnalysis,
% 					WordCharPositionList,
% 					_WordCharPositionListOut,
% 					0, _UtteranceStartPos,
% 					IndexCharPositionList,
% 					IndexCharPositionList)),
% 	  */
% 	  semantics_tree_charpos(SemanticAnalysis,
% 				 WordCharPositionList,
% 				 _WordCharPositionListOut,
% 				 0, _UtteranceStartPos,
% 				 IndexCharPositionList,
% 				 IndexCharPositionList) ->
% 	  true
% 	; IndexCharPositionList = [],
% 	  atom_codes(UNExpandedInputTextAtom, UNExpandedInputText),
% 	  flag_wrapper_failure(1, OutputStream,
% 			       semantics_tree_charpos, 5,
% 			       [InputLabel,ChosenInputText,
% 				'UNExpandedInputText'-UNExpandedInputTextAtom,
% 				'TokenListThisUtterance'
% 			       -write_token_list(TokenListThisUtterance,0)])
% 	).


% syntax_tree_charpos_wrapper(SyntacticAnalysis, SemanticAnalysis,
% 			    CharOffset,
% 			    OutputStream, InputLabel,
% 			    AADefFoundIn, _AADefFoundOut,
% 			    ChosenInputText, UNExpandedInputText, AAs,
% 			    TokenListThisUtterance, _TokenListOut,
% 			    WordCharPositionList, Rest) :-
% 	( /*
% 	  writeq('CALL':
% 		 syntax_tree_charpos(SyntacticAnalysis,
% 				     SemanticAnalysis,
% 				     CharOffset,
% 				     AADefFoundIn, _AADefFoundOut,
% 				     ChosenInputText, AAs,
% 				     TokenListThisUtterance, _TokenListOut,
% 				     0, _UtteranceStartPos,
% 				     WordCharPositionList, Rest)),
% 	   */
% 	  syntax_tree_charpos(SyntacticAnalysis, SemanticAnalysis,
% 			      CharOffset,
% 			      AADefFoundIn, _AADefFoundOut,
% 			      ChosenInputText, AAs,
% 			      TokenListThisUtterance, _TokenListOut,
% 			      0, _UtteranceStartPos,
% 			      WordCharPositionList, Rest) ->
% 	  true
% 	; WordCharPositionList = [],
% 	  Rest = [],
% 	  atom_codes(UNExpandedInputTextAtom, UNExpandedInputText),
% 	  flag_wrapper_failure(1, OutputStream,
% 			       syntax_tree_charpos, 12,
% 			       [InputLabel,ChosenInputText,
% 				'UNExpandedInputText'-UNExpandedInputTextAtom,
% 				'TokenListThisUtterance'
% 			       -write_token_list(TokenListThisUtterance,0)])
% 	).


% tokenize_string_wrapper(InputText, InputLabel, OutputStream, Words) :-
% 	( tokenize_string(InputText, Words) ->
% 	  true
%         ; flag_wrapper_failure(0, OutputStream, tokenize_string, 2, [InputLabel,InputText])
%         ).
% 
% write_full_fielded_output_wrapper(OutputStream, CharOffset, Domain,
% 			     InputLabel, AAs, TokenListThisUtterance,
% 			     ChosenInputText, UNExpandedInputText,
% 			     UtteranceType,
% 			     SyntacticAnalysis, SemanticAnalysis,
% 			     Predications) :-
% 	( write_full_fielded_output(OutputStream, CharOffset, Domain,
% 			       InputLabel, AAs, TokenListThisUtterance,
% 			       ChosenInputText, UNExpandedInputText,
% 			       UtteranceType,
% 			       SyntacticAnalysis, SemanticAnalysis,
% 			       Predications) ->
%           true
% 	; flag_wrapper_failure(0, OutputStream, write_full_fielded_output, 10,
% 			       [InputLabel,ChosenInputText,
% 				'TokenListThisUtterance'
% 			       -write_token_list(TokenListThisUtterance,0)])
% 	).


% Every element in RestArgs is expected to be a term of the form
% 'VariableName'-VariableValue
flag_wrapper_failure(AbortFlag, OutputStream, Predicate, Arity, [InputLabel,InputText|RestArgs]) :-
%	time_stamp('%y.%02n.%02d-%02c:%02i:%02s', Date),
        datime(Datime),
        Datime = datime(A,B,C,D,E,F),
	atom_codes(InputTextAtom, InputText),
 	format_to_all_streams([OutputStream,user_output],
			      '~nSemrep wrapper ERROR: ~q/~q failed at ~d/~d/~d-~d:~d:~d on ~w:~n~w~n',
			      [Predicate,Arity,B,C,A,D,E,F,InputLabel,InputTextAtom]),
	flag_wrapper_failure_rest_args(RestArgs, OutputStream),
	assert(errorFlag(false)), % tell go/4 there was an error
	flush_output(OutputStream),
	ttyflush,
	abort_if_necessary(AbortFlag).


flag_wrapper_failure_rest_args([], OutputStream) :-
 	format_to_all_streams([OutputStream,user_output], '~n~n', []).
flag_wrapper_failure_rest_args([ArgName-Arg|RestArgs], OutputStream) :-
	( functor(Arg, PredicateName, _Arity),
	  current_predicate(PredicateName, _Module:_Predicate) ->
 	  format_to_all_streams([OutputStream,user_output],
			      '~n~w:~n', [ArgName]),
 	  call_in_all_streams([OutputStream,user_output], Arg)
	; format_to_all_streams([OutputStream,user_output],
			      '~n~w:~n~w~n', [ArgName,Arg])
	),
	flag_wrapper_failure_rest_args(RestArgs, OutputStream).


format_to_all_streams([], _FormatString, _Args).
format_to_all_streams([FirstStream|RestStreams], FormatString, Args) :-
	format(FirstStream, FormatString, Args),
	flush_output(FirstStream),
	format_to_all_streams(RestStreams, FormatString, Args).

% The predicate passed here must take a stream as its first argument
call_in_all_streams([], _Predicate).
call_in_all_streams([FirstStream|RestStreams], Predicate) :-
	Predicate =.. [PredicateName|GivenArgs],
	PredicateWithStream =.. [PredicateName,FirstStream|GivenArgs],
	call(PredicateWithStream),
	call_in_all_streams(RestStreams, Predicate).

% If AbortFlag is 0, keep on going.
abort_if_necessary(0).
% If AbortFlag is 1, simply fail.
% abort_if_necessary(1) :- fail.
% If AbortFlag is 2, abort processing.
abort_if_necessary(2) :- abort.

